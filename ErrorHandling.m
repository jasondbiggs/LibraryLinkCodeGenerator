

(* ::Subsection::Closed:: *)
(*Error handling code, adapted from LibraryLinkUtilities.wl*)



If[
	StringQ[$LibraryName = FindLibrary["LibraryName"]] && TrueQ[Check[LibraryLoad[$LibraryName];True, False]],
	libraryFunctionLoad[args__] := Replace[
		Quiet @ Check[LibraryFunctionLoad @ args, $Failed],
		Except[_LibraryFunction] :> Throw[$Failed[args], LibraryFailureTag]
	];
	libraryFunctionLoad[___] := $Failed
]

With[
	{fun = libraryFunctionLoad[$LibraryName, "setExceptionDetailsContext", {String}, String]},
	fun[$Context]
]



If[!AssociationQ[$CorePacletFailureLUT],
	$CorePacletFailureLUT = <|
		"GeneralFailure" -> {31, "`1`"},
		"UnknownEnumValue" -> {27, "`1` is not a known value for type `2`, available types include `3`."}
	|>
];

With[
	{fun = libraryFunctionLoad[$LibraryName, "sendRegisteredErrors", LinkObject, LinkObject]},
	AssociateTo[$CorePacletFailureLUT, fun[]]
]


With[{rdtag = LibraryFailureTag},
	ThrowPacletFailure[type_String, params_List] := ThrowPacletFailure[type, "MessageParameters" -> params];
	ThrowPacletFailure[type_ ? StringQ, opts:OptionsPattern[createPacletFailure]] := ThrowPacletFailure[createPacletFailure[type, opts]];
	ThrowPacletFailure[failure_Failure] := Throw[failure, rdtag, cleanFailure]
]


Attributes[CatchPacletFailure] = {HoldAll}
CatchPacletFailure[arg_] := Catch[arg, LibraryFailureTag]
	

Options[createPacletFailure] = {
	"CallingFunction" -> None,
	"MessageParameters" -> <||>,
	"Parameters" -> {}
};

createPacletFailure[type_?StringQ, opts:OptionsPattern[]] :=
Block[{msgParam, param, errorCode, msgTemplate, errorType, assoc},
	msgParam = Replace[OptionValue["MessageParameters"], Except[_?AssociationQ | _List] -> <||>];
	param = Replace[OptionValue["Parameters"], {p_?StringQ :> {p}, Except[{_?StringQ.. }] -> {}}];
	{errorCode, msgTemplate} =
		Lookup[
			$CorePacletFailureLUT
			,
			errorType = type
			,
			(
				AppendTo[msgParam, "ErrorName" -> type];
				$CorePacletFailureLUT[errorType = "UnknownFailure"]
			)
		];
	If[errorCode < 0, (* if failure comes from the C++ code, extract message template parameters *)
		{msgParam, param} = GetCCodeFailureParams[msgTemplate];
	];
	assoc = <|
		"MessageTemplate" -> msgTemplate,
		"MessageParameters" -> msgParam,
		"ErrorCode" -> errorCode
	|>;
	If[OptionValue["CallingFunction"] =!= None, 
		assoc["CallingFunction"] = OptionValue["CallingFunction"]
	];
	If[Length[param] > 0, 
		assoc["Parameters"] = param
	];
	Failure[errorType,
		assoc
	]
];



GetCCodeFailureParams[msgTemplate_String ? StringQ] := Block[
	{slotNames, slotValues, msgParams, selectedSlotValues, params = {}},
	slotNames = DeleteDuplicates @ Cases[First @ StringTemplate @ msgTemplate, TemplateSlot[s_] :> s];
	slotValues = If[ListQ[$LastFailureParameters], $LastFailureParameters, {}];
	$LastFailureParameters = {};
	msgParams = If[MatchQ[slotNames, {Repeated[_Integer]}],
		slotValues,
		{selectedSlotValues, params} = TakeList[slotValues, {UpTo @ Length @ slotNames, All}];
		selectedSlotValues = PadRight[slotValues, Length @ slotNames, ""];
		If[VectorQ[slotNames, StringQ],
			AssociationThread[slotNames, selectedSlotValues],
			MapThread[Function[If[StringQ[#], <|# -> #2|>, #2]],
				{slotNames, selectedSlotValues}
			]
		]
	];
	{msgParams, params}
];


catchThrowErrors[HoldPattern[LibraryFunctionError[_, b_]], caller_] := ThrowPacletFailure[errorCodeToName[b], "CallingFunction" -> caller]
catchThrowErrors[ulf : HoldPattern[LibraryFunction[__][__]], _] := ThrowPacletFailure[Failure["UnevaluatedLibraryFunction", <|"Input" -> HoldForm[ulf]|>]]
catchThrowErrors[a_, _] := a

catchReleaseErrors[HoldPattern[LibraryFunctionError[_, b_]], caller_] := createPacletFailure[errorCodeToName[b], "CallingFunction" -> caller]
catchThrowErrors[ulf : HoldPattern[LibraryFunction[__][__]], _] := Failure["UnevaluatedLibraryFunction", <|"Input" -> HoldForm[ulf]|>]
catchReleaseErrors[a_, _] := a


errorCodeToName[errorCode_Integer]:=
Block[{name = Select[$CorePacletFailureLUT, MatchQ[#, {errorCode, _}] &]},
	If[Length[name] > 0 && Depth[name] > 2,
		First @ Keys @ name
		,
		""
	]
];


cleanFailure[f:Failure[type_,ass:KeyValuePattern[Rule["MessageParameters", {str_}]]],___] := Module[
	{failureArg = ass, res},
	If[ass["MessageTemplate"] === "`1`",
		failureArg["RawMessage"] = First[ass["MessageParameters"], ""];
		failureArg["MessageParameters"] = fixLibraryMessage[ass["MessageParameters"]];
	];
	res = Failure[type, failureArg]
]

cleanFailure[f_,__] := f

fixLibraryMessage[{str_String}] /; StringContainsQ[str, "\n"] := Module[
	{lines = Map[StringTrim, ImportString[str, "Lines"]]},
	{StringJoin[StringTake[StringRiffle[lines, ", "], UpTo[50]], "\[Ellipsis]"]}
]

fixLibraryMessage[{str_String}] /; StringLength[str] > 50 := {
	StringJoin[StringTake[str, UpTo[50]], "\[Ellipsis]"]
}

fixLibraryMessage[arg__] := arg





