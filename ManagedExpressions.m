(* ::Subsection::Closed:: *)
(*Managed library expression helper functions*)

getManagedID[type_][(type_)[id_]] := id
getManagedID[type_][l_List] := getManagedID[type] /@ l

normalizeFormattingGrid[ass_?AssociationQ] := KeyValueMap[{StringJoin[#1, ": "], #2}&, ass]
normalizeFormattingGrid[x_] := x

getMLEBox[obj_, fmt_, Optional[interpretable_, True]] := Module[
	{icon, grid, sym, validQ},
	sym = Head @ obj;
	icon = getIcon @ obj;
	grid = getObjectInformation[obj];
	If[AssociationQ[grid] && ManagedLibraryExpressionQ[obj],
		grid["ObjectType"] =.
	];
	grid = normalizeFormattingGrid @ grid;
	validQ = MatchQ[grid, Alternatives[{_}, {Repeated[{_String, _}]}]] || ManagedLibraryExpressionQ[obj];
	If[MatchQ[grid, Alternatives[{_}, {Repeated[{_String, _}]}]],
		BoxForm`ArrangeSummaryBox[sym,
			obj, icon, Map[BoxForm`SummaryItem, Part[grid, Span[1, UpTo @ 2]]],
			Map[BoxForm`SummaryItem, grid],
			fmt, "CompleteReplacement" -> True, "Interpretable" -> interpretable
		],
		BoxForm`ArrangeSummaryBox[sym,
			obj, icon, {BoxForm`SummaryItem @ {"ID: ", ManagedLibraryExpressionID @ obj}},
			{}, fmt, "Interpretable" -> interpretable
		]
	] /; validQ
];
getIcon[___] := None;

getObjectInformation[obj_?ManagedLibraryExpressionQ] := Module[
	{info = obj["information"], methods = methodData[Head[obj]]},
	If[!AssociationQ[info], Return[$Failed, Module]];
	info["ObjectType"] = Head @ obj;
	If[AssociationQ[methods], info["MethodNames"] = Keys @ methods];
	info
]

getObjectInformation[___] := $Failed

getObjectInformationSubset[obj_, props_] := Module[
	{info = getObjectInformation[obj], methods = methodData[Head[obj]]},
	If[!AssociationQ[info], Return[$Failed, Module]];
	If[!AssociationQ[methods], methods = <||>];
	Replace[props,
		{
			p : (Alternatives @@ Keys[info]) :> info[p],
			p : (Alternatives @@ Keys[methods]) :> methods[p],
			p_ :> Missing["KeyAbsent", p]
		},
		{1}
	]
]

$clearManagedInstances := $clearManagedInstances = libraryFunctionLoad[$LibraryName, "clearManagedInstances", {"UTF8String"}, "Void"]
clearManagedInstances[type_ : "All"] := $clearManagedInstances[type]

$managedInstanceIDList := $managedInstanceIDList = libraryFunctionLoad[$LibraryName, "managedInstanceIDList", {"UTF8String"}, {Integer, 1}]
managedInstanceIDList[type_] := $managedInstanceIDList[type]

$deleteInstance := $deleteInstance = libraryFunctionLoad[$LibraryName, "deleteInstance", {"UTF8String", Integer}, "Void"]

methodData = <||>

Do[
	With[
		{t = type, tstring = ToString[type]},
		t /: t[n_Integer]["Delete"] /; ManagedLibraryExpressionQ[t[n]] := $deleteInstance[SymbolName[t], n];
		t /: MakeBoxes[obj$:t[_Integer], fmt$_] /; ManagedLibraryExpressionQ[Unevaluated[obj$]] := getMLEBox[obj$, fmt$, True];
		methodData[type] = <|"Delete" -> StringJoin[tstring, "[..][\"Delete\"] deletes the ", tstring, " object."]|>;
		Information`AddRegistry[t, getObjectInformation];
		t /: Information`GetInformationSubset[obj : t[_Integer], props_List] := getObjectInformationSubset[obj, props];
	],
	{type, $MLEList}
]