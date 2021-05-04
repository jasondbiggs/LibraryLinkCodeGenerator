(* ::Subsection::Closed:: *)
(*ErrorHandling*)

(*
	Code adapted from LibraryLinkUtilities error handling functions.	
*)



With[
	{fun = LibraryFunctionLoad[$LibraryName, "setExceptionDetailsContext", {String}, String]},
	fun[$Context]
]

With[{rdtag = $FailureTag},
	ThrowPacletFailure[type_String, params_List] := ThrowPacletFailure[type, "MessageParameters" -> params];
	ThrowPacletFailure[type_ ? StringQ, opts:OptionsPattern[CreatePacletFailure]] := With[
		{failure = CreatePacletFailure[type, opts]},
		Throw[failure, rdtag, cleanFailure];
	];
	ThrowInvalidArgumentFailure[HoldPattern[LibraryFunction[_,lfun_,largs_,_][args___]], caller_] := Throw[
		Failure["InvalidLibraryArguments", 
			<|
				"MessageTemplate" -> "Invalid library arguments.", 
				"Expected" -> largs, "Actual" -> {args}, "ThrowingFunction" -> caller
			|>
		], rdtag, cleanFailure
	]
]
Attributes[CatchRDExceptions] = {HoldAll}
CatchRDExceptions[arg_] := Catch[arg, $FailureTag]
	

Options[CreatePacletFailure] = {
	"ThrowingFunction" -> None,
	"MessageParameters" -> <||>,
	"Parameters" -> {}
};

CreatePacletFailure[type_?StringQ, opts:OptionsPattern[]] :=
Block[{msgParam, param, errorCode, msgTemplate, errorType, fun, assoc},
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
	If[OptionValue["ThrowingFunction"] =!= None, 
		assoc["ThrowingFunction"] = OptionValue["ThrowingFunction"]
	];
	If[Length[param] > 0, 
		assoc["Parameters"] = param
	];
	Failure[errorType,
		assoc
	]
];


Clear @ $LastFailureParameters;
GetCCodeFailureParams[msgTemplate_String ? StringQ] := Block[
	{slotNames, slotValues, msgParams, selectedSlotValues, params = {}},
	slotNames = DeleteDuplicates @ Cases[First @ StringTemplate @ msgTemplate, TemplateSlot[s_] -> s];
	slotValues = If[ListQ[Echo @ $LastFailureParameters], $LastFailureParameters, {}];
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


$CorePacletFailureLUT = <|
	"LibraryLoadFailure" -> {20, "Failed to load library `LibraryName`. Details: `Details`."},
	"FunctionLoadFailure" -> {21, "Failed to load the function `FunctionName` from `LibraryName`. Details: `Details`."},
	"RegisterFailure" -> {22, "Incorrect arguments to RegisterPacletErrors."},
	"UnknownFailure" -> {23, "The error `ErrorName` has not been registered."},
	"ProgressMonInvalidValue" -> {24, "Expecting None or a Symbol for the option \"ProgressMonitor\"."},
	"InvalidManagedExpressionID" -> {25, "`Expr` is not a valid ManagedExpression." },
	"UnexpectedManagedExpression" -> {26, "Expected managed `Expected`, got `Actual`." },
	"UnknownEnumValue" -> {27, "`1` is not a known value for type `2`."},
	"NoDataStore" -> {28, "`1` cannot be converted to a DataStore node."},
	"InvalidParameter" -> {29, "Invalid value `2` for parameter `1`."},
	"InvalidResult" -> {30, "Invalid result `1`."},
	"GeneralFailure" -> {31, "`1`"}
|>;



With[
	{fun = LibraryFunctionLoad[$LibraryName, "sendRegisteredErrors", LinkObject, LinkObject]},
	AssociateTo[$CorePacletFailureLUT, fun[]]
]


libraryFunctionLoad[args__] := Quiet @ Check[LibraryFunctionLoad @ args, Throw[$Failed, "liberr"]]

catchThrowErrors = Function[{arg, caller}, 
	Replace[Quiet @ arg,
		{
			LibraryFunctionError[_, b_] :> ThrowPacletFailure[ErrorCodeToName[b], "ThrowingFunction" -> caller](*,
			res:(_LibraryFunction)[___] :> ThrowInvalidArgumentFailure[res, caller]*) (* costs an extra 8 microseconds, maybe remove *)
		}
	],
	HoldFirst
];


ErrorCodeToName[errorCode_Integer]:=
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
	lines = Select[lines, StringFreeQ["on line"|"RDKIT:"|"BOOST:"]];
	{StringJoin[StringTake[StringRiffle[lines, ", "], UpTo[50]], "\[Ellipsis]"]}
]

fixLibraryMessage[{str_String}] /; StringLength[str] > 50 := {
	StringJoin[StringTake[str, UpTo[50]], "\[Ellipsis]"]
}

fixLibraryMessage[arg__] := arg

