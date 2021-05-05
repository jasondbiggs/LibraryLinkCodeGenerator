(* ::Subsection::Closed:: *)
(*Managed library expression helper functions*)

getManagedID[type_][(type_)[id_]] := id
getManagedID[type_][l_List] := getManagedID[type] /@ l


getMLEBox[obj_, fmt_, Optional[interpretable_, True]] := Module[
	{icon, grid, sym, validQ},
	sym = Head @ obj;
	icon = getIcon @ obj;
	grid = Catch[obj @ "formattingGrid", _];
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



$clearManagedInstances := $clearManagedInstances = libraryFunctionLoad[$LibraryName, "clearManagedInstances", {"UTF8String"}, "Void"]
clearManagedInstances[type_ : "All"] := $clearManagedInstances[type]

$managedInstanceIDList := $managedInstanceIDList = libraryFunctionLoad[$LibraryName, "managedInstanceIDList", {"UTF8String"}, {Integer, 1}]
managedInstanceIDList[type_] := $managedInstanceIDList[type]

$deleteInstance := $deleteInstance = libraryFunctionLoad[$LibraryName, "deleteInstance", {"UTF8String", Integer}, "Void"]

Do[With[{t = type},
	t /: t[n_Integer]["delete"] /; ManagedLibraryExpressionQ[t[n]] := $deleteInstance[SymbolName[t], n];
	t /: MakeBoxes[obj$:t[_Integer], fmt$_] /; ManagedLibraryExpressionQ[Unevaluated[obj$]] := getMLEBox[obj$, fmt$, True]
	],
	{type, $MLEList}
]