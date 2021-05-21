(* ::Subsection::Closed:: *)
(*Managed library expression helper functions*)

getManagedID[type_][(type_)[id_]] := id
getManagedID[type_][l_List] := getManagedID[type] /@ l

normalizeFormattingGrid[ass_?AssociationQ] := KeyValueMap[BoxForm`SummaryItem @ {StringJoin[#1, ": "], ElisionsDump`expandablePane @ #2}&, ass]
normalizeFormattingGrid[ass:{__?AssociationQ}] := normalizeFormattingGrid /@ ass
normalizeFormattingGrid[x_] := x

$gridPattern = {__BoxForm`SummaryItem} | {{_BoxForm`SummaryItem,___}..}

addMethods[dat_ : $gridPattern, head_] := Module[
	{methods = methodData[head]},
	If[AssociationQ[methods],
		methods = BoxForm`SummaryItem[{"Operations: ", getMethodButton[head, #]& /@ Keys[methods]}];
		If[ListQ[dat[[1]]], methods = {methods, SpanFromLeft}];
		Append[dat, methods]
		,
		dat
	]
]
addMethods[dat_,___] := dat

getMLEBox[obj_, fmt_, Optional[interpretable_, True]] := Module[
	{icon, grid, sym},
	sym = Head @ obj;
	icon = getIcon @ obj;
	grid = addMethods[normalizeFormattingGrid @obj["information"], Head @ obj];
	If[!MatchQ[grid, $gridPattern] && ManagedLibraryExpressionQ[obj],
		grid = {BoxForm`SummaryItem @ {"ID: ", ManagedLibraryExpressionID @ obj}}
	];
	BoxForm`ArrangeSummaryBox[sym,
		obj, icon, Part[grid, Span[1, UpTo @ 2]],
		grid,
		fmt, "CompleteReplacement" -> True, "Interpretable" -> interpretable
	] /; MatchQ[grid, $gridPattern]
];
getIcon[___] := None;

getObjectInformation[obj_?ManagedLibraryExpressionQ] := Module[
	{info = obj["information"], methods = methodData[Head[obj]]},
	If[!AssociationQ[info], Return[$Failed, Module]];
	info["ObjectType"] = Head @ obj;
	If[AssociationQ[methods], info["Operations"] = getMethodButton[Head[obj], #]& /@ Keys[methods]];
	info
]


getMethodButton[obj_, name_] := name

getMethodButton[obj_, name_] /; $Notebooks && StringQ[methodData[obj, name]] := Button[
	Style[name, "InformationGridButton"],
	Print @ methodData[obj, name],
	Appearance -> None
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
	If[ListQ[type]
		,
		With[
			{t = Last[type], owner = First[type]},
			t /: t[{owner[n_Integer],___}]["GetOwner"] := owner[n];
			t /: MakeBoxes[obj$:t[{owner[n_Integer],___}], fmt$_] /; ManagedLibraryExpressionQ[Unevaluated[owner[n]]] := getMLEBox[obj$, fmt$, True];
			methodData[t] = <|"GetOwner" -> StringJoin["returns the owning ", ToString[owner], " object."]|>;
			Information`AddRegistry[t, getObjectInformation];
			t /: Information`GetInformationSubset[obj : t[{owner[n_Integer],___}], props_List] := getObjectInformationSubset[obj, props];
			t /: Information`OpenerViewQ[t, "Operations"] := True;
			Replace[methodData[t],
				ass_?AssociationQ :> (
				t /: (_t)["Operations"] := getMethodButton[Head[t], #]& /@ Keys[methodData[t]]
					
				)
			]
		]
		,
		With[
			{t = type, tstring = ToString[type]},
			
			t /: t[n_Integer]["Delete"] /; ManagedLibraryExpressionQ[t[n]] := $deleteInstance[SymbolName[t], n];
			t /: MakeBoxes[obj$:t[_Integer], fmt$_] /; ManagedLibraryExpressionQ[Unevaluated[obj$]] := getMLEBox[obj$, fmt$, True];
			methodData[type] = <|"Delete" -> StringJoin[tstring, "[..][\"Delete\"] deletes the ", tstring, " object."]|>;
			Information`AddRegistry[t, getObjectInformation];
			t /: Information`GetInformationSubset[obj : t[_Integer], props_List] := getObjectInformationSubset[obj, props];
			t /: Information`OpenerViewQ[t, "Operations"] := True;
			Replace[methodData[t],
				ass_?AssociationQ :> (
				t /: (_t)["Operations"] := getMethodButton[Head[t], #]& /@ Keys[methodData[t]]
					
				)
			]
		]
	],
	{type, $MLEList}
]