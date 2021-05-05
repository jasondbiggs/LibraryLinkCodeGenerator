(* Wolfram Language package *)

BeginPackage["GetFunctions`"];

Needs["CodeParser`"]
Needs["CodeFormatter`"]
Needs["GeneralUtilities`"]

WriteLibrarySignatures
getFunctionString
nodeToString
throw

Begin["`Private`"];

ClearAll["GetFunctions`Private`*"]
(* ::Subsection::Closed:: *)
(*Utilities*)

$inputDir = DirectoryName @ $InputFileName;

$PacletDirectory = FileNameDrop[$InputFileName, -2]

echo = If[$Notebooks, Echo, (Print[##2];Print[#1])&];

throw[args__] := (
	echo @ args;
	Throw[$Failed, _, #&]
);


closeOff[function_] := SetDelayed[
	function[in_],
	throw[{in},SymbolName[function]]
]

Attributes[failOnMessage] = {HoldAll}
failOnMessage[eval_] := Internal`HandlerBlock[
	{"Message", throw[HoldForm[eval], "message issued"]&},
	eval
]


$ElidedContexts = {"System`","Global`", "var`", "inert`", $Context}
GeneralUtilities`Formatting`PackagePrivate`sn[e_] := If[MemberQ[$ElidedContexts,Context[e]], 
	SymbolName[Unevaluated[e]], 
	Context[e] <> SymbolName[Unevaluated[e]]
];


balancedBracesQ[str_String] := And[
	StringCount[str, "{"] === StringCount[str, "}"],
	StringCount[str, "["] === StringCount[str, "]"],
	StringCount[str, "("] === StringCount[str, ")"]
];

StringExpression["Sequence[", Shortest[v__] /; balancedBracesQ[v], "]"] :> v

parenthize[s_String] := If[StringContainsQ[s, " "], StringJoin["(", s, ")"], s]
SetAttributes[prettyInputString, HoldAll]
prettyInputString[expr_] := (
	Needs["GeneralUtilities`"];
	StringReplace[
		GeneralUtilities`ToPrettyString[expr(*Unevaluated[expr]*)],
		{
			"\r\n" -> "\n",
			"$LibrarySymbol" -> "$LibrarySymbol",
			(* this one is questionable *)
			"fun$" -> "fun",
			"res$" -> "res",
			"Apply[Sequence, options]" -> "Sequence @@ options",
			"\\[LeftDoubleBracket]" -> "[[",
			"\\[RightDoubleBracket]" -> "]]",
			"\\[RuleDelayed]" -> ":>",
			"writeJSON" -> "Developer`WriteRawJSONString",
			"readJSON" -> "Developer`ReadRawJSONString",
			"managedLibraryExpressionID" -> "ManagedLibraryExpressionID",
			"managedLibraryExpressionQ" -> "ManagedLibraryExpressionQ",
			"map" -> "Map",
			(*"libraryFunctionLoad" -> "libraryFunctionLoad",*)
			"\\[Rule]" -> "->",
			RuleDelayed[
				StringExpression["Rule[",
					Shortest[a__] /; balancedBracesQ[a],
					", ",
					Shortest[b__] /; balancedBracesQ[b],
					"]"
				],
				StringJoin[parenthize @ a, " -> ", parenthize @ b]
			],
			"inert" -> "",
			"\\[LeftDoubleBracket]" -> "[[", 
			"\\[RightDoubleBracket]" -> "]]",
			"Hold" -> "catchErrors"
		}
	]
)

prettyfyCodeString[codeString_] := ToExpression[codeString, 
	StandardForm,
	Function[
		arg, 
		GeneralUtilities`ToPrettyString[Unevaluated[arg]],
		HoldAll
	]
]


(* ::Subsection::Closed:: *)
(*Scan source files*)


scanForSignatures[file_?FileExistsQ] := Module[
	{res, text = Import[file, "Text"]},
	{res, cppFunctionDeclarations} = Reap[Join[
		scanForFunctions[text],
		scanForEnumTypes[text]
	], "cpp", cppDeclaration];
	If[Length[res] > 0,
		{file, res},
		Nothing
	]
]
scanForSignatures[file___] := throw[{file},"file doesn't exist"]

functionTypes = <|
	"LLU_WSTP_FUNCTION" -> defineWSTPFunction,
	"BEGIN_WSTP_FUNCTION" -> defineWSTPFunction,
	"CONSTRUCTOR_FUNCTION" -> defineConstructor,
	"MEMBER_FUNCTION" -> defineMemberFunction, 
	"EXPORTED_FUNCTION" -> defineExportedFunction, 
	"BEGIN_LIBRARY_FUNCTION" -> defineExportedFunction
|>;


$types = Apply[Alternatives, Keys @ functionTypes];
scanForFunctions[string_] := StringCases[
	string,
	RuleDelayed[
		StringExpression[functiontype:$types,
			"(", Shortest[name__], ")", Shortest[body__], "END_LIBRARY_FUNCTION"
		],
		scanForFunction[functiontype, name, body]
	]
]

scanForFunction[functiontype_, name_, body_] := Module[
	{
		ftype = Lookup[functionTypes, functiontype, throw[functiontype,"bad type"]], 
		arguments = scanForArguments @ body
	},
	checkValidArguments[arguments, {functiontype, name, body}];
	
	ftype[StringSplit[StringReplace[name, bef__ ~~ "," ~~ __ :> bef], "_"], Sequence @@ arguments]
]

scanForArguments[body_] := Block[{comments, MArgument = Identity, MReturn = Identity, usage},
	comments = scanForComments[body];
	usage = Replace[
		Select[comments, StringContainsQ["MUsage"]],
		{{x_,___} :> x, _ :> Nothing}
	];
	{
		usage,
		Map[toExpression, Select[comments, StringContainsQ["MArgument"]]],
		toExpression @ SelectFirst[comments, StringContainsQ["MReturn"], throw[body, "no return"]]
	}
];

scanForComments[body_] := StringCases[
	body,
	{
		StringExpression["//", Shortest[c__], EndOfLine] :> StringTrim[c],
		StringExpression["/*", Shortest[c__], "*/"] :> StringTrim[c]
	}
]


(*scanForEnumTypes[body_ /; StringContainsQ[body, "Enumerate["]] := With[
	{comments = scanForComments[body]},
	toExpression /@ Select[comments, StringContainsQ["Enumerate["]]
]
scanForEnumTypes[___] := {}*)


hasEnum[string_] := StringContainsQ[string, "Enumerate[" | "LIBRARYLINK_ENUM"]

scanForEnumTypes[body_ /; hasEnum[body]] := StringCases[body,
	{
		match:("Enumerate[" ~~ Shortest[__] ~~ "]") :> toExpression[match],
		("LIBRARYLINK_ENUM("~~ Shortest[type__] ~~")" ~~ Shortest[___] ~~ "{" ~~ Shortest[list__] ~~ "}") :> getLibraryEnums[type, list]
	}
]

scanForEnumTypes[___] := {}

getLibraryEnums[type_, list_] := Enumerate[type,
	Map[StringTrim,StringSplit[list, ","]]
]

With[{context = $Context},
	toExpression[arg_] := Block[{$Context = context}, Replace[ToExpression @ arg, Null :> throw[arg, "null expression"]]]
]


checkValidArguments[arguments_, {functiontype_, name_, body_}] := If[
	Last[arguments] === "Void" && StringContainsQ[body, "mngr.set"]
	,
	throw[name, "returning a value from a void function"]
]

(* ::Subsection::Closed:: *)
(*CodeParser stuff*)

$flag = False;

nodeToString[node: Except[_List]] := nodeToString[{node}];
nodeToString[nodes_List] := Block[{$flag = True},
	stringPostProcess @ failOnMessage @ CodeFormatCST @ ToExpression[ToString[containerNode[nodes],InputForm]]
]


stringNode[sym_Symbol] /; $flag := stringNode @ SymbolName @ sym;
stringNode[sym_String] /; $flag := LeafNode[String, ToString[sym, InputForm], <||>];

symbolNode[sym_String] /; $flag := LeafNode[Symbol, sym, <||>];
symbolNode[sym_Symbol] /; $flag := symbolNode @ SymbolName @ sym;

numberNode[num_Integer] /; $flag := LeafNode[Head @ num, ToString[num,InputForm], <||>]

commaInfixNode[nodes_List] /; $flag := InfixNode[Comma, Riffle[nodes, LeafNode[Comma, ",", <||>]], <||>];

composeNode[head_, node:Except[_List]] /; $flag := composeNode[head, {node}];
composeNode[head:_Symbol | _String, nodes_List] /; $flag := composeNode[symbolNode @ head, nodes];
composeNode[head_, nodes_List] /; $flag := CallNode[
	{head},
	{
		GroupNode[GroupSquare,
			{
				LeafNode[Token`OpenSquare, "[", <||>],
				If[Length[nodes] > 0, commaInfixNode @ nodes, Nothing],
				LeafNode[Token`CloseSquare, "]", <||>]
			},
			<||>
		]
	},
	<||>
];

partNode[expr_, part_Integer] /; $flag := composeNode[symbolNode @ "Part", {expr, getNode @ part}]


listNode[nodes_List] /; $flag := GroupNode[
	List,
	{
		LeafNode[Token`OpenCurly, "{", <||>],
		If[Length @ nodes > 0, commaInfixNode @ nodes, Nothing],
		LeafNode[Token`CloseCurly, "}", <||>]
	},
	<||>
];


setNode[x_, y_] /; $flag := BinaryNode[Set, {x, LeafNode[Equal, "=", <||>], y}, <||>];
setDelayedNode[x_, y_] /; $flag := BinaryNode[SetDelayed, {x, LeafNode[Token`ColonEqual, ":=", <||>], y}, <||>];
ruleDelayedNode[x_, y_] /; $flag := BinaryNode[
	RuleDelayed,
	{x, LeafNode[Token`ColonGreater, ":>", <||>], y},
	<||>
];

ruleNode[x_, y_] /; $flag := BinaryNode[
	Rule,
	{x, LeafNode[Token`MinusGreater, ":", <||>], y},
	<||>
];

applyNode[x_ : _Symbol | _String, y_] /; $flag := applyNode[symbolNode @ x, y]
applyNode[x_, y_] /; $flag := BinaryNode[
	Apply,
	{x, LeafNode[Token`AtAt, "@@", <||>], y},
	<||>
]

blankNode[head_] /; $flag := CompoundNode[
	Blank,
	{LeafNode[Token`Under, "_", <||>], getNode @ head},
	<||>
]

namedBlankNode[name_] /; $flag := CompoundNode[
	PatternBlank,
	{LeafNode[Symbol, name, <||>], LeafNode[Token`Under, "_", <||>]},
	<||>
];

namedPatternNode[name_, patternNode_] /; $flag := BinaryNode[Pattern,
	{
		symbolNode[name],
		LeafNode[Token`Colon, ":", <||>],
		patternNode
	},
	<||>
];

optionalPatternNode[name_, val_] /; $flag := composeNode[Optional, {namedBlankNode[name], getNode @ val}]


withNode[assignmentNodes_, exprNode_] /; $flag := composeNode[symbolNode @ "With", {listNode @ assignmentNodes, exprNode}];
blockNode[assignmentNodes_, exprNode_] /; $flag := composeNode[symbolNode @ "Block", {listNode @ assignmentNodes, compoundExpression @ exprNode}];


compoundExpression[{node_}] /; $flag := node
compoundExpression[nodes_List] /; $flag := InfixNode[
	CompoundExpression,
	Riffle[nodes,
		Splice @ {LeafNode[Token`Semi, ";", <||>], LeafNode[Token`Newline, "\n", <||>]}
	],
	<||>
];
compoundExpression[node_] /; $flag := node;

containerNode[nodes_List] /; $flag := ContainerNode[String, nodes, <||>]
containerNode[cn_ContainerNode] := cn
containerNode[node_] /; $flag := ContainerNode[String, {node}, <||>]


getNode[x_String] := stringNode  @ x;
getNode[x_Symbol] := symbolNode  @ x;
getNode[Rule[x_, y_]] := ruleNode[getNode @ x, getNode @ y]

getNode[x_] /; StringEndsQ[SymbolName[Head[x]], "Node"] := x
getNode[x_] := Replace[
	ReplaceAll[KeyValuePattern["Source" -> _] -> <||>] @ CodeConcreteParse @ ToString[x, InputForm],
	{
		ContainerNode[_, {node_}, <||>] :> node,
		c_ContainerNode :> c,
		other_ :> throw[x, "getNode"]
	}
]

parenNode[x_] /; $flag := GroupNode[
	GroupParen, 
	{
		LeafNode[Token`OpenParen, "(", <||>], 
		x, 
		LeafNode[Token`CloseParen, ")", <||>]
	}, 
	<||>
]


(* ::Subsection::Closed:: *)
(*Write WL source files*)



WriteLibrarySignatures[sourceFiles_, destinationFile_, params_] := Module[
	{
		signatures = scanForSignatures /@ sourceFiles, outstring, catchingFunction,
		symbols, filestring, functionsString, libraryName, enumerateDefinitionString,
		librarySymbolContext, failureTag, throwingFunction, errorHandlingString,
		datastoreString, mleString, mles, pretty
	},
	libraryName = Lookup[params, "LibraryName", throw[$Failed, "no library name"]];
	librarySymbolContext = Lookup[params, "LibrarySymbolContext", ""];
	failureTag = Lookup[params, "FailureTag", libraryName <> "FailureTag"];
	throwingFunction = Lookup[params, "ThrowingFunction", "Throw" <> libraryName <> "Failure"];
	catchingFunction = Lookup[params, "CatchingFunction", "Catch" <> libraryName <> "Failure"];
	pretty = If[Lookup[params, "Pretty", False], prettyfyCodeString, Identity];
	
	
	If[!MatchQ[signatures, {{_,_}..}], throw[signatures,"no signatures found"]];
	symbols = scanForSymbols[
		Flatten[DeleteCases[signatures[[All, 2]],_Enumerate, Infinity]][[All, 1]],
		librarySymbolContext
	];
	functionsString = UsingFrontEnd @ Block[{stringPostProcess = pretty},
		StringRiffle[
			fileDefinitionString @@@ signatures,
			"\n"
		]
	];
	filestring = addSymbolDeclarations[Import[destinationFile, "Text"], symbols];
	
	
	mles = scanForManagedTypes[Flatten[DeleteCases[signatures[[All, 2]],_Enumerate, Infinity]][[All, 1]]];
	mleString = If[Length[mles] < 1,
		Nothing,
		StringReplace[
			Import[FileNameJoin[{$inputDir, "ManagedExpressions.m"}], "Text"],
			"$MLEList" :> ToString[mles]
		]
	];
	
	errorHandlingString = Import[FileNameJoin[{$inputDir, "ErrorHandling.m"}], "Text"];
	
	enumerateDefinitionString = If[FreeQ[signatures, Enumerate],
		Nothing,
		Import[FileNameJoin[{$inputDir, "Enumerate.m"}], "Text"]
	];
	
	datastoreString = If[FreeQ[signatures, "DataStore"],
		Nothing,
		Import[FileNameJoin[{$inputDir, "ToFromDataStore.m"}], "Text"]
	];
	
	If[!StringQ[functionsString],throw[$Failed,"bad signature conversion"]];
	outstring = StringRiffle[
		{
			$beginCodeBlock,
			errorHandlingString,
			enumerateDefinitionString,
			datastoreString,
			functionsString,
			mleString,
			$endCodeBlock
		},
		"\n\n"
	];
	
	filestring = StringReplace[filestring,
		$beginCodeBlock ~~ __ ~~ $endCodeBlock :> outstring
	];
	filestring = StringReplace[filestring,
		{
			"LibraryName" -> libraryName,
			"FailureTag" -> failureTag,
			"ThrowingFunction" -> throwingFunction,
			"CatchingFunction" -> catchingFunction
		}
	];

	Export[destinationFile, filestring, "Text"]
]

addSymbolDeclarations[fileString_, symbolsList_] := Module[
	{begin, middle, end},
	begin = "(* ::Subsection::Closed:: *)
(*Symbols list*)

(* This list is auto-generated, changes will not be preserved *)";
	end = "(* End autogenerated symbols block *)";
	middle = StringRiffle[
		symbolsList,
		"\n"
	];
	StringReplace[fileString, begin ~~ __ ~~ end :> StringRiffle[{begin, middle, end}, "\n\n"]]
]

exportedSymbolsBlock[symbols_] := StringRiffle[
	{
		commentTemplate["Autogenerated Exported Symbols"],
		StringRiffle[
			symbols,
			"\n"
		],
		commentTemplate["Begin Private Context"],
		$beginPrivateBlock
	},
	"\n\n"
]

$beginCodeBlock = "(* 
	WARNING: The following code is automatically generated, 
	manual changes will not be preserved.
*)"

$beginPrivateBlock = "
Begin[\"`Private`\"]

"

$endCodeBlock = "(*
	End autogenerated code block.
*)"


scanForSymbols[input_, librarySymbolContext_] := DeleteDuplicates @ Replace[input,
	{
		{sym_} :> librarySymbolContext <> sym,
		{obj_, method_} :> librarySymbolContext <> obj,
		{obj_, sub_, method_} :> librarySymbolContext <> sub
	},
	{1}
]

scanForManagedTypes[input_] := DeleteDuplicates @ Replace[input,
	{
		{sym_} :> Nothing,
		{obj_, method_} :> obj,
		{obj_, sub_, method_} :> {obj, sub}
	},
	{1}
]

fileDefinitionString[file_, functionSignatures_] := StringJoin[
	commentTemplate @ FileBaseName @ file,
	getFunctionStrings[functionSignatures]
]

commentTemplate = StringTemplate["\n\n(* ::Subsection::Closed:: *)\n(*`1`*)\n\n"];


getFunctionStrings[exprs_] := Check[
	StringRiffle[
		getFunctionAndComment /@ exprs,
		"\n\n"
	],
	throw[exprs, "bad exprs"]
]


getFunctionAndComment[(head_)[obj_, MUsage[usage_], args___]] := StringJoin[
	getUsageString[head[obj, args], usage],
	"\n\n",
	getFunctionString[head[obj, args]]
]

getUsageString[_, usage_] := StringJoin["(*\n", usage, "\n*)"];


(* ::Subsection::Closed:: *)
(*getFunctionStrings*)

getFunctionString[defineConstructor[{object_, fname_}, rest__]] := Module[
	{node},
	node = getFunctionNode[defineMemberFunction[{object}, rest]];
	nodeToString[node /. stringNode[object] :> stringNode[object<>"_"<>fname]]
	
]

getFunctionString[fun:(defineMemberFunction | defineExportedFunction)[__]] := Module[
	{node = getFunctionNode @ fun},
	nodeToString @ node
]

getFunctionString[defineWSTPFunction[args__]] := Module[
	{node},
	node = getFunctionNode[defineMemberFunction[args]];
	
	node = node /. composeNode[libraryFunctionLoad, {n1_, n2_, __}] :> composeNode[libraryFunctionLoad, {n1, n2, symbolNode@LinkObject, symbolNode@LinkObject}];
	nodeToString[node]
]


getFunctionString[Enumerate[type_String, vals:{__String} | _?AssociationQ]] := GeneralUtilities`ToPrettyString[
	enumerate[type, vals]
]

getFunctionNode[function:(head_)[fspec_List, arguments:{___List}, return_]] := Module[
	{
		argumentPatterns, libraryArguments, variables, funOptions = {}, 
		libReturn, libdef, optionsNode, lhs, rhs
	},
	argumentPatterns = Replace[
		getArgPatterns @ function,
		HoldPattern[{ap___, Verbatim[OptionsPattern][op_List]}] :> (
			funOptions = op;
			{ap, If[Length[fspec] > 1, namedPatternNode["opts", composeNode[OptionsPattern,{}]], composeNode[OptionsPattern,{}]]}
		)
	];
		
	libraryArguments = getLibraryArguments @ function;
	variables = getVariables @ function;
	{argumentPatterns, libraryArguments, variables};
	libReturn = getNode @ transformReturn @ return;
	libdef = makeLibDef[fspec, libraryArguments, libReturn];
	(*If[Length[funOptions] > 0 && MatchQ[fspec, {_, __}],
		throw[function, "options are not supported for object subvalues"]
	];*)
	optionsNode = makeOptionsNode[fspec, funOptions[[All, 2]]];
	lhs = makeLHSNode[fspec, argumentPatterns];
	rhs = makeRHSNode[fspec, variables, funOptions, return];
	compoundExpression @ {
		optionsNode,
		withNode[
			{libdef},
			setDelayedNode[lhs, rhs]
		]
	}
	
]

closeOff[getFunctionString]

$VoidReturnPattern = _Managed
ClearAll @ getArgPatterns;
getArgPatterns[(head_)[{class_, ___, member_}, arguments:{___List}, return_]] := Prepend[getArgPatterns @ arguments, stringNode @ member]


getArgPatterns[_[_, arguments:{___List}, _]] := getArgPatterns @ arguments
getArgPatterns[{arguments:({_, _String}...), parameters: ({_, _Rule} ..)}] := Append[
	getArgPatterns[{arguments}], OptionsPattern[{parameters}]
]

getArgPatterns[{arguments:({_, _String}...), optionalArguments: ({_, _String, Except[_argumentPattern]} ..)}] := Join[
	getArgPatterns[{arguments}], 
	optionalPatternNode[#2, getNode @ #3]& @@@ {optionalArguments}
]
getArgPatterns[args:{{_, _String}...}] := namedBlankNode[#2]& @@@ args;

getArgPatterns[args:{{_, _String, _argumentPattern}...}] := namedPatternNode[#2, getNode[#3[[1]]]]& @@@ args;
closeOff[getArgPatterns]


ClearAll @ getLibraryArguments;
getLibraryArguments[(head_)[{class_, subtype_, member_}, arguments:{___List}, return_]] := getLibraryArguments[
	head[{member}, Join[{{Integer}, subTypeArgument[subtype]}, arguments], return]
]

getLibraryArguments[(head_)[{class_,member_}, arguments:{___List}, return_]] := getLibraryArguments[
	head[{member}, Prepend[arguments , {Integer}], return]
]

getLibraryArguments[(head_)[{function_}, arguments:{___List}, $VoidReturnPattern]] := getLibraryArguments[
	head[{function}, Append[arguments , {Integer}], "Void"]
]

getLibraryArguments[(head_)[{function_}, arguments:{___List}, ret : Except[$VoidReturnPattern]]] := 
	getNode @* transformLibraryArguments /@ arguments[[All, 1]]
closeOff[getLibraryArguments]

subTypeArgument["RDRingInfo"] := Nothing
subTypeArgument[_] := {Integer}

ClearAll @ getVariables;
getVariables[(head_)[functionType_, arguments:{___List}, return_]] := Join[
	Replace[functionType,
		{
			{_} :> {},
			{class_, member_} :> {mleID["expr"]},
			{class_, subtype_, member_} :> {mleID["expr"], subTypeVariable[subtype]},
			_ :> throw[functionType, "getVariables"]
		}
	],
	getVariables @ arguments,
	Replace[return,
		{
			vr: $VoidReturnPattern :> {mleID["res"]}, 
			_ :> {}
		}
	]
]

getVariables[{arguments:({_, _String, ___}...), parameters: ({_, _Rule}..)}] := Append[
	getVariables[{arguments}], applyNode[symbolNode @ Sequence, symbolNode @ "options"]
]

getVariables[arguments : {{_, _String,___}...}] := Replace[arguments,
	{type_, name_, ___} :> getVariable[type, symbolNode @ name],
	{1}
]

closeOff[getVariables]

ClearAll[getVariable]
preprocessFunctionNode[enum[type_]] := composeNode[symbolNode @ "enum", stringNode @ type]
preprocessFunctionNode["RawJSON"] := symbolNode @ "Developer`WriteRawJSONString"
preprocessFunctionNode[type_Managed] := composeNode["getManagedID", getNode @ First @ type]
preprocessFunctionNode[{type_Managed, 1}] := composeNode["getManagedID",getNode @ First @ type]
preprocessFunctionNode["DataStore"] := symbolNode @ "toDataStore"
preprocessFunctionNode["StringList"] := composeNode[symbolNode @ "Apply", symbolNode @ "Developer`DataStore"]
preprocessFunctionNode[_] := Identity

getVariable[type_, name_] := Replace[preprocessFunctionNode[type],
	{
		Identity :> name,
		other_ :> composeNode[other, name]
	}
]
closeOff[getVariable]


mleID[sym_String] := composeNode["ManagedLibraryExpressionID", symbolNode @ sym]

ClearAll[subTypeVariable]
subTypeVariable["RDRingInfo"] := Nothing
subTypeVariable[_] := symbolNode @ "idx"



postProcess[enum[type_String]] := Function[composeNode[composeNode[symbolNode @ "enum", stringNode @ type], #]]
postProcess["RawJSON"] := Function[composeNode[symbolNode @ "Developer`ReadRawJSONString", #]]
postProcess["DataStore"] := Function[composeNode[symbolNode @ "fromDataStore", #]]
postProcess[PostProcessed[type_, fun_(* : (_Function | _Symbol)*)]] := Function[
	composeNode[getNode @ fun, postProcess[type][#]]
]
(*postProcess[PostProcessed[type_, fun_]] := throw[fun, "should be a function"]*)

postProcess[$VoidReturnPattern] := Function[
	composeNode[
		Replace,
		{
			#,
			ruleDelayedNode[symbolNode["Null"], symbolNode["res"]]
		}
	]
]
postProcess[_] := Identity
			


transformReturn = ReplaceAll[
	{
		"RawJSON" -> String,
		_Managed -> "Void",
		_enum -> Integer,
		PostProcessed[a_,_] :> transformReturn[a]
	}
]

transformLibraryArguments = ReplaceAll[
	{
		{type:(Integer | Real), _?IntegerQ} :> {type, _},
		"RawJSON" -> String,
		"StringList" -> "DataStore",
		_Managed -> Integer,
		_enum -> Integer,
		{"NumericArray", _} -> "NumericArray"
	}
];


makeLibDef[fspec_, libargs_List, libreturn_] := setNode[
	symbolNode @ "fun",
	composeNode[
		libraryFunctionLoad,
		{
			symbolNode["$LibraryName"],
			stringNode @ StringRiffle[fspec,"_"],
			listNode @ libargs,
			libreturn
		}
	]
]

optionHead[{object_, method_}] := StringJoin[object, method]
makeOptionsNode[_, {}] := Nothing;
makeOptionsNode[{functionName_}, options:{__Rule}] := setNode[
	composeNode[Options, symbolNode @ functionName],
	getNode[System`Private`SortOptions[options]]
]
makeOptionsNode[{object_, method_}, options:{__Rule}] := setNode[
	composeNode[Options, symbolNode @ optionHead[{object, method}]],
	getNode[System`Private`SortOptions[options]]
]

closeOff[makeOptionsNode]

getHeadNode[{sym_, _}] := Pattern[expr, Blank[getHeadNode[{sym}]]]
getHeadNode[{s_String}] := getHeadNode[{s}] = Symbol[$LibraryContext<>s]
$NumberedSubtypes = "RDAtom" | "RDBond" | "RDConformer"
getHeadNode[{owningType_, type:$NumberedSubtypes, x_}] := Pattern[
	sub,
	getHeadNode[{type}][{getHeadNode[{owningType, x}], Pattern[idx, Blank[]]}]
]

getHeadNode[{s_String}] := symbolNode @ s;
getHeadNode[{sym_, _}] := parenNode @ namedPatternNode["expr", blankNode[getHeadNode @ {sym}]]
getHeadNode[{owningType_, type:$NumberedSubtypes, _}] := composeNode[
	type,
	listNode @ {
		getHeadNode[{owningType, type}],
		namedBlankNode["idx"]
	}
]

getThrowerNode[{s_String}] := symbolNode @ s;
getThrowerNode[s:{__}] := listNode[stringNode /@ s];

makeLHSNode[fspec_, argPatternNodes_] := composeNode[
	getHeadNode @ fspec,
	argPatternNodes
]


makeRHSNode[fun_, variableNodes_List, funOptions_, return_] := Module[
	{res, definitions = {}, opts},
	res = postProcess[return] @ composeNode[
		"catchThrowErrors",
		{
			composeNode[symbolNode @ "fun", variableNodes],
			getThrowerNode[fun]
		}
	];

	If[Length @ funOptions > 0,
		AppendTo[definitions,
			setNode[
				symbolNode @ "options",
				composeNode[
					symbolNode @ OptionValue,
					If[Length[fun] > 1,
						{
							symbolNode @ optionHead[fun], 
							listNode @ {symbolNode @ "opts"}, 
							listNode @ Map[getNode, funOptions[[All, 2, 1]]]
						},
						listNode @ Map[getNode, funOptions[[All, 2, 1]]]
					]
					
				]
			]	
		];
		opts = MapAt[preprocessFunctionNode, funOptions, {All, 1}];
			
		If[!MatchQ[opts[[All, 1]] , {Identity ..}],
			res = {res};
			Replace[Thread[{opts, Range @ Length @ opts}],
				{
					{{Identity, _}, _} :> Null,
					{{function_, _}, n_} :> PrependTo[res,
						setNode[
							partNode[symbolNode @ "options", n], 
							composeNode[function, partNode[symbolNode @ "options", n]]
						]
						
						
						
					] 	
				},
				{1}
			]
			
		];
	];
	If[MatchQ[return, $VoidReturnPattern]
		,
		AppendTo[definitions,
			setNode[
				symbolNode @ "res",
				getVoidNode @ return
			]
		]
	];
	If[Length[definitions] > 0,
		res = blockNode[definitions, res]
	];
	res
];

getVoidNode[Managed[x_Symbol]] := composeNode[symbolNode @ CreateManagedLibraryExpression, {stringNode @ x, symbolNode @ x}]

End[]
EndPackage[]

