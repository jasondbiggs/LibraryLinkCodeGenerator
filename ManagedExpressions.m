(* ::Subsection::Closed:: *)
(*Managed library expression helper functions*)

getManagedID[type_][(type_)[id_]] := id
getManagedID[type_][l_List] := getManagedID[type] /@ l

normalizeFormattingGrid[ass_?AssociationQ] := KeyValueMap[BoxForm`SummaryItem @ {StringJoin[#1, ": "], ElisionsDump`expandablePane @ #2}&, ass]
normalizeFormattingGrid[ass:{__?AssociationQ}] := normalizeFormattingGrid /@ ass
normalizeFormattingGrid[x_] := x

$gridPattern = {__BoxForm`SummaryItem} | {{_BoxForm`SummaryItem,___}..}

addMethods[dat_ : $gridPattern, obj_] := Module[
	{methods = methodData[Head @ obj]},
	If[AssociationQ[methods],
		methods = BoxForm`SummaryItem[{"Operations: ", getMethodButton[obj, #]& /@ Keys[methods]}];
		If[ListQ[dat[[1]]], methods = {methods, SpanFromLeft}];
		Append[dat, methods]
		,
		dat
	]
]
addMethods[dat_,___] := dat

(*getMLEBox[obj_, fmt_, Optional[interpretable_, True]] := Module[
	{icon, grid, sym},
	sym = Head @ obj;
	icon = getIcon @ obj;
	grid = addMethods[normalizeFormattingGrid @ obj["information"], obj];
	
	If[!MatchQ[grid, $gridPattern] && ManagedLibraryExpressionQ[obj],
		grid = {BoxForm`SummaryItem @ {"ID: ", ManagedLibraryExpressionID @ obj}}
	];
	BoxForm`ArrangeSummaryBox[sym,
		obj, icon, Part[grid, Span[1, UpTo @ 2]],
		grid,
		fmt, "CompleteReplacement" -> True, "Interpretable" -> interpretable
	] /; MatchQ[grid, $gridPattern]
];*)
getIcon[___] := None;

getObjectInformation[obj_?ManagedLibraryExpressionQ] := Module[
	{info = obj["information"], methods = methodData[Head[obj]]},
	If[!AssociationQ[info], Return[$Failed, Module]];
	info["ObjectType"] = Head @ obj;
	If[AssociationQ[methods], info["Operations"] = getMethodButton[Head[obj], #]& /@ Keys[methods]];
	info
]


getMethodButton[obj_, name_] := name

getMethodButton[obj_, name_] := Module[
	{data = methodData[Head @ obj, name]},
	With[
		{params = If[MatchQ[data["Parameters"], {__String}], Placeholder /@ data["Parameters"], Sequence @@ {}]},
		ClickToCopy[name, Defer[obj[name, params]]]
	]
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
				t /: (_t)["Operations"] := getMethodButton[t, #]& /@ Keys[methodData[t]]
					
				)
			]
		]
		,
		With[
			{t = type, tstring = ToString[type]},
			
			t /: t[n_Integer]["Delete"] /; ManagedLibraryExpressionQ[t[n]] := $deleteInstance[SymbolName[t], n];
			t /: MakeBoxes[obj$:t[_Integer], fmt$_] /; validObject[Unevaluated[obj$]] := getMLEBox[obj$, fmt$, True];
			Information`AddRegistry[t, getObjectInformation];
			methodData[t] = <||>;
			t /: Information`GetInformationSubset[obj : t[_Integer], props_List] := getObjectInformationSubset[obj, props];
			t /: Information`OpenerViewQ[t, "Operations"] := True;
			Replace[methodData[t],
				ass_?AssociationQ :> (
				t /: (_t)["Operations"] := getMethodButton[t, #]& /@ Keys[methodData[t]]
					
				)
			]
		]
	],
	{type, $MLEList}
]

validObject[x_] := ManagedLibraryExpressionQ[Unevaluated[x]]



ClearAll @ dynamicEvaluate;
SetAttributes[dynamicEvaluate, HoldAllComplete];
dynamicEvaluate[expr_, cachedValue_] := Dynamic[
	expr, SynchronousUpdating -> False, TrackedSymbols :> {},
	CachedValue :> cachedValue
];

ClearAll @ addDeclarations;
SetAttributes[addDeclarations, HoldAll];
addDeclarations[DynamicModuleBox[{a__}, b__], expr__] := DynamicModuleBox[{a, expr}, b];
addDeclarations[a_, ___] := a;


getMLEBox[obj_, fmt_, Optional[interpretable_, True]] := Module[
	{icon, grid, sym, methods = methodData[Head @ obj], box, sometimesGrid},
	sym = Head @ obj;
	icon = getIcon @ obj;
	grid = normalizeFormattingGrid @ obj["information"];
	If[!MatchQ[grid, $gridPattern] && ManagedLibraryExpressionQ[obj],
		grid = {BoxForm`SummaryItem @ {"ID: ", ManagedLibraryExpressionID @ obj}}
	];
	sometimesGrid = Part[grid, Span[1, UpTo @ 2]];
	If[Length[methods] > 0,
		grid = addMethodsToGrid[methods, grid]
	];
	(
		box = BoxForm`ArrangeSummaryBox[sym,
			obj, icon, sometimesGrid,
			grid,
			fmt, "CompleteReplacement" -> True, "Interpretable" -> interpretable
		];
		If[Length[methods] > 0,
			addMethodsToBox[methods, box, obj],
			box
		]
	) /; MatchQ[grid, $gridPattern]
];
getIcon[___] := None;

addMethodsToGrid[methodsData_, grid_] := Module[
	{item},
	item = BoxForm`SummaryItem[
		{
			"Operations: ", 
			dynamicEvaluate[
				ElisionsDump`expandablePane @ Replace[
					getMethodButtons[Typeset`sobj$$, Typeset`sops$$],
					Except[_List] :> Typeset`sops$$
				],
				Typeset`sops$$
			]
		}
	];
	If[MatchQ[grid, {__BoxForm`SummaryItem}],
		Append[grid, item],
		Append[grid, {item, SpanFromLeft}]
	]
	
]

addMethodsToBox[methods_, box_, obj_] := Module[
	{dbox},
	dbox = Cases[box, a_DynamicModuleBox :> a, Infinity, 1];
	dbox = With[
		{a = First[dbox], ops = Keys @ methods}, 
		addDeclarations[a, Typeset`sobj$$ = obj, Typeset`sops$$ = ops]
	];
	box /. (_DynamicModuleBox -> dbox)
		
]
		
		
getMethodButtons[obj_, operations_] := Map[
	Function[
		Button[
			Tooltip[#,methodData[Head[obj], #, "Usage"]],
			CopyToClipboard[
				Replace[getInput[obj, #], _getInput :> StringJoin["\"", #, "\""]]
			],
			Appearance -> None, BaseStyle -> Automatic
		]
	],
	operations
]


getInput[obj: HoldPattern[(head_)[__]], method_] := Module[
	{md = methodData[head, method], vars},
	(
		vars = Replace[
			md["Parameters"],
			{
				x:{__String} :> Apply[Sequence, Placeholder /@ x],
				_ :> Sequence[]
			}
		];
		With[{expr = Defer[obj][method, vars]},
			{box = MakeBoxes[expr, StandardForm]},
			Cell[BoxData @ box, "Input"]
		]
	) /; AssociationQ[md]
]


		
		
		
		
		
		
		
		
		
		
