(* ::Subsection::Closed:: *)
(*Managed library expression helper functions*)

getManagedID[type_][(type_)[id_]] := id
getManagedID[type_][l_List] := getManagedID[type] /@ l

normalizeFormattingGrid[ass_?AssociationQ] := Module[
	{res},
	res = KeyValueMap[BoxForm`SummaryItem @ {StringJoin[#1, ": "], ElisionsDump`expandablePane @ #2}&, ass];
	If[Length[res] > 10,
		Partition[res, UpTo[2]],
		res
	]
]
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

getIcon[___] := None;


getMethodButton[obj_, name_] := name

getMethodButton[obj_, name_] := Module[
	{data = methodData[Head @ obj, name]},
	With[
		{params = If[MatchQ[data["Parameters"], {__String}], Placeholder /@ data["Parameters"], Sequence @@ {}]},
		ClickToCopy[name, Defer[obj[name, params]]]
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
			t /: MakeBoxes[obj$:t[{owner[n_Integer],___}], fmt$_] /; ManagedLibraryExpressionQ[Unevaluated[owner[n]]] := Module[{res}, res /; !FailureQ[res = getMLEBox[obj$, fmt$, True]]];
			methodData[t] = <|"GetOwner" -> <|"Usage" -> StringJoin["returns the owning ", ToString[owner], " object."]|>|>;
			t /: (_t)["Operations"] := Keys[methodData[t]]
		]
		,
		With[
			{t = type},
			
			t /: t[n_Integer]["Delete"] /; ManagedLibraryExpressionQ[t[n]] := $deleteInstance[SymbolName[t], n];
			t /: MakeBoxes[obj$:t[_Integer], fmt$_] /; validObject[Unevaluated[obj$]] := Module[{res}, res /; !FailureQ[res = getMLEBox[obj$, fmt$, True]]];
			methodData[t] = <||>;
			t /: (_t)["Operations"] := Keys[methodData[t]]
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
	If[!MatchQ[grid, $gridPattern],
		grid = Which[
			ManagedLibraryExpressionQ[obj],
				{BoxForm`SummaryItem @ {"ID: ", ManagedLibraryExpressionID @ obj}},
			MatchQ[obj, _[{_?ManagedLibraryExpressionQ, _Integer}]],
				{
					BoxForm`SummaryItem @ {"Parent: ", Head @ obj[[1,1]]},
					BoxForm`SummaryItem @ {"ID: ", obj[[1,2]]}
				},
			True,
				Return[$Failed, Module]
		]
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
		{a = First[dbox], ops = Sort @ Keys @ methods}, 
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


		
		
		
		
		
		
		
		
		
		
