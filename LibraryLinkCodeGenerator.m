(* Wolfram Language package *)

BeginPackage["GetFunctions`"];



WriteLibrarySignatures


Begin["`Private`"];

Needs["CodeParser`"]
(*Needs["CodeFormatter`"]*)
Needs["GeneralUtilities`"]


ClearAll["GetFunctions`Private`*"]
(* ::Subsection::Closed:: *)
(*Utilities*)

tps := GeneralUtilities`ToPrettyString

$inputDir = DirectoryName @ $InputFileName;

$PacletDirectory = FileNameDrop[$InputFileName, -2]

echo = If[$Notebooks, Echo, (Print[""];Print[#1];Print[##2];Print[""];)&];

If[!$Notebooks,
	print[args___] := Print["\033[1;33m", args, "\033[1;0m"];
]


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



With[{context = $Context},
	toExpression[arg_] := Block[{$Context = context}, Replace[ToExpression @ arg, Null :> throw[arg, "null expression"]]]
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
		tps[expr(*Unevaluated[expr]*)],
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
		tps[Unevaluated[arg]],
		HoldAll
	]
]


(* ::Subsection::Closed:: *)
(*Scan source files*)

Get @ FileNameJoin[{$inputDir, "CommentParser.m"}]


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
$doxyPattern = StringExpression["/**", Shortest @ __, "*/"];
scanForFunctions[fileString_] := StringCases[
	fileString,
	{
		RuleDelayed[
			StringExpression[doc:$doxyPattern,
				WhitespaceCharacter...,
				str:StringExpression[functiontype:$types,
					"(", Shortest[name__], ")", Shortest[body__], "END_LIBRARY_FUNCTION"
				]
			],
			scanForFunction[functiontype, name, body, doc]
		],
		RuleDelayed[
			str:StringExpression[functiontype:$types,
				"(", Shortest[name__], ")", Shortest[body__], "END_LIBRARY_FUNCTION"
			],
			scanForFunction[functiontype, name, body]
		]
	}
]


scanForFunction[functiontype_, name_, body_, doc_ : ""] := Module[
	{
		ftype = Lookup[functionTypes, functiontype, throw[functiontype,"bad type"]], 
		arguments = scanForArguments[body, doc]
	},
	checkValidArguments[arguments, {functiontype, name, body}];
	
	ftype[StringSplit[StringReplace[name, bef__ ~~ "," ~~ __ :> bef], "_"], arguments]
]




scanForArguments[body_, ""] := Block[
	{comments, MArgument = Identity, usage},
	comments = Select[ImportString[body, "Lines"], StringContainsQ["///"]];
	(*If[Length[comments] === 0,
		throw[body, "no arguments found"]
	];*)
	(* convert the inline comments to the block-style doxygen comment and parse that *)
	
	comments = StringRiffle[
		{
			"/**",
			Sequence @@ Map[StringReplace["///" -> "*"], comments], 
			"*/"
		},
		"\n"
	];
	scanForArguments[body, comments]
	
];

scanForArguments[body_, docString_] := scanDoxyString[docString, body]




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


checkValidArguments[arguments_, {functiontype_, name_, body_}] := Module[
	{returnType, params, argCount},
	params = Lookup[arguments, "Parameters", throw[name, "no parameters"]];
	argCount = Length[params];
	
	If[MatchQ[functiontype, "CONSTRUCTOR_FUNCTION" | "MEMBER_FUNCTION"],
		argCount++
	];
	
	
	returnType = Lookup[arguments, "Return", throw[name, "no return"]];
	returnType = Lookup[returnType, "ReturnType", throw[name, "no return type"]];
	If[Head[returnType] === PostProcessed, 
		If[!MatchQ[Length[returnType], 2 | 3], throw[name, "bad post process"]];
		returnType = First[returnType]
	];
	Switch[functiontype,
		"CONSTRUCTOR_FUNCTION",
			If[!MatchQ[returnType, _Managed], throw[name, "must return managed object"]],
		"MEMBER_FUNCTION",
			argCount++
	];
	Switch[returnType,
		_Managed,
			If[StringFreeQ[body, "createInstance"], throw[name, "must call createInstance to return managed object"]];
			If[!StringFreeQ[body, "mngr.set"], throw[name, "cannot set return value in function returning managed object"]];
			argCount++,
		"Boolean",
			If[StringFreeQ[body, "mngr.setBoolean"], throw[name, "no setBoolean call"]],
		"Void",
			If[StringContainsQ[body, "mngr.set"], throw[name, "returns a value when declared void"]]
	]
]




(* ::Subsection::Closed:: *)
(*CodeParser stuff*)

$flag = False;

nodeToString[node: Except[_List]] := nodeToString[{node}];
nodeToString[nodes_List] := stringPostProcess[
	failOnMessage @ ToSourceCharacterString @ withFlag @ withFlag[containerNode[nodes]]
]



withFlag[expr_] := Block[{$flag = True},
	Update /@ $flagConditionedSymbols;
	expr
]


stringNode[sym_Symbol] /; $flag := stringNode @ SymbolName @ sym;
stringNode[sym_String] /; $flag := LeafNode[String, ToString[sym, InputForm], <||>];

symbolNode[sym_String] /; $flag := LeafNode[Symbol, sym, <||>];
symbolNode[sym_Symbol] /; $flag := symbolNode @ If[MemberQ[$ElidedContexts,Context[sym]], 
	SymbolName[Unevaluated[sym]], 
	Context[sym] <> SymbolName[Unevaluated[sym]]
]



symbolNode @ SymbolName @ sym;

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
]

namedBlankSequenceNode[name_] /; $flag := CompoundNode[
	PatternBlankSequence,
	{LeafNode[Symbol, name, <||>], LeafNode[Token`Under, "__", <||>]},
	<||>
]

namedBlankNode[withHead[name_, head_]] /; $flag := CompoundNode[
	PatternBlank,
	{
		symbolNode @ name, 
		CompoundNode[Blank,
			{
				LeafNode[Token`Under, "_", <||>],
				symbolNode @ head
			},
			<||>
		]
	},
	<||>
]

namedPatternNode[name_, patternNode_] /; $flag := BinaryNode[Pattern,
	{
		symbolNode[name],
		LeafNode[Token`Colon, ":", <||>],
		patternNode
	},
	<||>
];

optionalPatternNode[name_, Hold[val_]] /; $flag := composeNode[HoldPattern, {composeNode[Optional, {namedBlankNode[name], getNode @ val}]}]
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

If[
	Not[
		And[UnsameQ[Part[GetEnvironment @ "pbwidth", 2], None],
			IntegerQ[pbwidth = ToExpression[Part[GetEnvironment @ "pbwidth", 2]]]
		]
	],
	pbwidth = 50
]

WriteLibrarySignatures[sourceFiles_, destinationFile_, params_] := Module[
	{
		signatures, outstring, catchingFunction,
		symbols, filestring, functionsString, libraryName, enumerateDefinitionString,
		librarySymbolContext, failureTag, throwingFunction, errorHandlingString,
		datastoreString, mleString, mles, pretty
	},
	getCustomTypes[sourceFiles];
	pb = progbar[pbwidth, "scanning source files"];
	nn = 0;
	
	Block[{throwAlways = TrueQ @ Lookup[params, "AlwaysThrow", True]},
		signatures = (pb[Floor[++nn * pbwidth/Length[sourceFiles]]];scanForSignatures[#])& /@ sourceFiles;
	];
	
	pb[pbwidth, True];
	
	
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
	pb = progbar[pbwidth, "generating WL function definitions"];
	nn = 0;
	functionsString = UsingFrontEnd @ Block[{stringPostProcess = pretty},
		StringRiffle[
			(pb[Floor[++nn * pbwidth/Length[signatures]]];fileDefinitionString[##])& @@@ signatures,
			"\n"
		]
	];
	pb[pbwidth, True];
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
			mleString,
			functionsString,
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
			"LibraryFailureTag" -> failureTag,
			"ThrowPacletFailure" -> throwingFunction,
			"CatchPacletFailure" -> catchingFunction,
			"\\\\\\n" -> "\\\n",
			"\\n" -> "\n",
			"("~~Shortest[def__]~~"::usage) =" /; StringFreeQ[def, ")"] :> (def <> "::usage =")
		}
	];
	print["writing output to ", FileNameTake @ destinationFile];
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
		{obj__, sub_, method_} :> librarySymbolContext <> sub
	},
	{1}
]

scanForManagedTypes[input_] := DeleteDuplicates @ Replace[input,
	{
		{sym_} :> Nothing,
		{obj_, method_} :> obj,
		{obj__, sub_, method_} :> {obj, sub}
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

getFunctionAndComment[(head_)[obj_, args_]] := StringJoin[
	getUsageString[head, obj, args],
	"\n\n",
	getFunctionString[head[obj, args]]
]

getUsageString[defineMemberFunction, obj:{___, object_, member_}, args_] := Module[
	{rhs = <|"Usage" -> getFunctionusage[defineMemberFunction, obj, args]|>, vars},
	vars = Select[args["Parameters"], #["ParameterType"] === "Required" &];
	If[Length[vars] > 0, rhs["Parameters"] = Lookup[vars, "Name"]];
	With[
	{sym = Symbol[object], md = rhs},
		tps[
			Unevaluated @ Set[methodData[sym, member], md]
		]
	]
]


getUsageString[defineExportedFunction, obj:{member_}, args_] := With[
	{sym = Symbol[member], usage = getFunctionusage[defineExportedFunction, obj, args]},
	tps[
		Unevaluated @ Set[sym::usage, usage]
	]
	
]

getUsageString[defineConstructor, obj:{___, object_, member_}, args_] := With[
	{sym = Symbol[object], usage = getFunctionusage[defineConstructor, obj, args]},
	tps[
		Unevaluated @ Set[sym::usage, usage]
	]
	
]	

getUsageString[Enumerate, ___] := ""

getUsageString[head_, obj_, args_] := StringJoin["(*\n", 
	StringReplace[getFunctionusage[head, obj, args], "\n" -> "\n\t"], 
	"\n*)"
];


getFunctionusage[head_, obj_, args_] := Block[{hasOptions = False},Module[
	{main, usage = args["Usage"], params, return, note = Lookup[args, "Note", Nothing]},
	If[StringQ[note], note = "Note: " <> note];
	If[!StringQ[usage] || StringLength[usage] === 0, usage = "", usage = " \\\n" <> usage];
	main = getInputString[head, obj, args] <> usage;
	params = getParameterUsage[#["ParameterType"], #["Name"], #["ArgumentDescription"], #["Comment"], #["DefaultValue"]]& /@ args["Parameters"];
	return = getReturnUsage[args["Return"]];
	If[Length[Select[args["Parameters"], #["ParameterType"] =!= "Option"&]] > 0, PrependTo[params, "Variables:"]];
	If[Length[params] > 0, params = StringRiffle[params, "\n"], params = Nothing];
	StringRiffle[{main, params, return}, "\n\n"]
]]

getReturnUsage[return_] := Module[
	{res},
	If[StringQ[res = return["ReturnDescription"]]
		,
		res = "Returns: "<>res;
		If[StringQ[return["Comment"]],
			res = res <> " - " <> return["Comment"]
		]
		,
		res = Nothing
	];
	res
]

getInputString[head_, type_, args_] := Module[
	{fhead, argstring, params = args["Parameters"], opts = Nothing},
	If[MemberQ[params[[All, "ParameterType"]], "Option"], 
		opts = "options";
		params = Select[params, #["ParameterType"] =!= "Option"&];
	];
	params = Append[params[[All, "Name"]], opts];
	fhead = getHead[head, type];
	If[Length @ type > 1 && head =!= defineConstructor,
		PrependTo[params, ToString[Last[type], InputForm]];
	];
	argstring = StringRiffle[params, ", "];
	StringJoin[{fhead, "[", argstring, "]"}]
]

getHead[defineConstructor, {object_, _}] := object;
getHead[defineMemberFunction, {object_, member_}] := ToLowerCase[object];
getHead[defineMemberFunction, {_, object_, member_}] := ToLowerCase[object];
getHead[_, {fun_}] := fun;
getHead[___] := ""

getParameterUsage["Option", name_, description_, comment_, default_] := Module[
	{string = ToString[name, InputForm]},
	If[StringQ[description] && StringLength[description] > 0,
		string = string <> " - " <> wrapWithDefault[description, default]
	];
	If[StringQ[comment] && StringLength[comment] > 0,
		string = string <> " - " <> comment
	]; 
	If[TrueQ[hasOptions],
		string,
		hasOptions = True;
		Sequence @@ {"Options:", string}
	]
	
]

wrapWithDefault[description_, _Missing] := description
wrapWithDefault[description_, default_] := "(" <> description <> " : " <> ToString[default, InputForm] <>")"

getParameterUsage[type_, name_, description_, comment_, default_] := Module[
	{string = ToString[name]},
	If[StringQ[description] && StringLength[description] > 0,
		string = string <> " - " <> wrapWithDefault[description, default]
	];
	If[StringQ[comment] && StringLength[comment] > 0,
		string = string <> " - " <> comment
	]; 
	string
	
]

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


getFunctionString[Enumerate[type_String, vals:{__String} | _?AssociationQ]] := tps[
	enumerate[type, vals]
]

getLibSym[fspec_] := composeNode[symbolNode @ "libfun", stringNode @ StringRiffle[fspec, "_"]]

getFunctionNode[function:(head_)[fspec_List, arguments_Association]] := Module[
	{
		argumentPatterns, libraryArguments, variables, funOptions = {}, 
		libReturn, libdef, optionsNode, lhs, rhs, libsym = getLibSym[fspec]
	},
	argumentPatterns = (*Echo[#, "argPatterns"]&@ *)Replace[
		getArgPatterns @ function,
		HoldPattern[{ap___, Verbatim[OptionsPattern][op_List]}] :> (
			funOptions = op;
			{ap, If[Length[fspec] > 1, namedPatternNode["opts", composeNode[OptionsPattern,{}]], composeNode[OptionsPattern,{}]]}
		)
	];
		
	libraryArguments = (*Echo[#, "libargs"]&@*)getLibraryArguments @ function;
	variables = getVariables @ ReplaceAll[function, withHead[x_,___] :> x](* // Echo[#, "variables"]&*);
	{argumentPatterns, libraryArguments, variables};
	libReturn = getNode @ transformReturn @ arguments["Return", "ReturnType"](*  // Echo[#, "libreturn"]&*);
	libdef = makeLibDef[fspec, libraryArguments, libReturn, libsym];
	If[Head[libdef]=== makeLibDef, Throw[{function, libraryArguments, libReturn},"foo",#1&]];
	(*If[Length[funOptions] > 0 && MatchQ[fspec, {_, __}],
		throw[function, "options are not supported for object subvalues"]
	];*)
	optionsNode = makeOptionsNode[fspec, Rule[#Name, #DefaultValue]& /@ funOptions];
	lhs = makeLHSNode[fspec, argumentPatterns];
	rhs = makeRHSNode[fspec, variables, funOptions, arguments["Return", "ReturnType"], TrueQ @ arguments["ThrowFailure"], libsym];
	compoundExpression @ {
		optionsNode,
		libdef,
		setDelayedNode[lhs, rhs]
	}
	
]

closeOff[getFunctionString]

$VoidReturnPattern = _Managed | "AsynchronousTaskObject"
ClearAll @ getArgPatterns;
getArgPatterns[(head_)[{class_, ___, member_}, arguments_]] := Prepend[
	getArgPatterns @ arguments["Parameters"], 
	stringNode @ member
]


getArgPatterns[_[_List, arguments_Association]] := getArgPatterns @ arguments["Parameters"]

getArgPatterns[params:{___, KeyValuePattern["ParameterType" -> "Option"]}] := Append[
	getArgPatterns[Select[params, #ParameterType =!= "Option"&]],
	OptionsPattern[Select[params, #ParameterType === "Option"&]]
]

getArgPatterns[params:{___, KeyValuePattern["ParameterType" -> "Optional"]}] := Join[
	getArgPatterns[Select[params, #ParameterType === "Required"&]],
	optionalPatternNode[#Name, #DefaultValue]& /@ Select[params, #ParameterType === "Optional"&]
]

getArgPatterns[params:{___, KeyValuePattern["ParameterType" -> "PatternTest"]}] := namedPatternNode[#Name, getNode[#Pattern]]& /@ params;

getArgPatterns[args:{___}] := namedBlankNode[#Name]& /@ args;

closeOff[getArgPatterns]


ClearAll @ getLibraryArguments;

getLibraryArguments[(head_)[func_, arguments_]] := Module[
	{params = arguments[["Parameters", All, "ArgumentType"]]},
	params = Replace[params, PreProcessed[x_, _] :> x, {1}];
	Switch[func,
		{_, _},
			PrependTo[params, Integer],
		{_, __, _},
			PrependTo[params, {Integer, 1}]
	];
	If[MatchQ[arguments["Return", "ReturnType"], $VoidReturnPattern | PostProcessed[_Managed,___]],
		AppendTo[params, Integer]
	];
	getNode @* transformLibraryArguments /@ params

]

closeOff[getLibraryArguments]

subTypeArgument["RDRingInfo"] := Nothing
subTypeArgument[_] := {Integer}

ClearAll @ getVariables;
getVariables[(_)[functionType_List, arguments_Association]] := Join[
	Replace[functionType,
		{
			{_} :> {},
			{class_, member_} :> {mleID[class, "expr"]},
			{class_, subtype__, member_} :> {listNode[{mleID[class, "expr"], symbolNode @ "idx"}]},
			_ :> throw[functionType, "getVariables"]
		}
	],
	getVariables @ arguments["Parameters"],
	Replace[arguments["Return", "ReturnType"],
		{
			vr: (Managed[x_] | PostProcessed[Managed[x_],___]) :> {mleID[x, "res"]},
			"AsynchronousTaskObject" -> {composeNode["getTaskID", {symbolNode @ "res"}]},
			_ :> {}
		}
	]
]

getVariables[arguments:{___Association}] := Append[
	getVariable[#ArgumentType, symbolNode @ #Name]& /@ Select[arguments, #ParameterType =!= "Option" &],
	If[MemberQ[Lookup[arguments, "ParameterType"], "Option"],
		applyNode[symbolNode @ Sequence, symbolNode @ "options"],
		Nothing
	]
]


closeOff[getVariables]

ClearAll[getVariable]
preprocessFunctionNode[enum[type_]] := composeNode[symbolNode @ "enum", stringNode @ type]
preprocessFunctionNode["AsynchronousTaskObject"] := symbolNode @ "getTaskID"
preprocessFunctionNode[type_Managed] := composeNode["getManagedID", getNode @ First @ type]
preprocessFunctionNode[{type_Managed, 1}] := composeNode["getManagedID",getNode @ First @ type]
preprocessFunctionNode["DataStore"] := symbolNode @ "toDataStore"
preprocessFunctionNode["StringList"] := composeNode[symbolNode @ "Apply", symbolNode @ "Developer`DataStore"]
preprocessFunctionNode[PreProcessed[type_, fun_]] := getNode[fun]
preprocessFunctionNode[_] := Identity

getVariable[type_, name_] := Replace[preprocessFunctionNode[type],
	{
		Identity :> name,
		other_ :> composeNode[other, name]
	}
]
closeOff[getVariable]


mleID[type_, sym_String] := composeNode[
	composeNode["getManagedID", symbolNode @ type], 
	symbolNode @ sym
]

ClearAll[subTypeVariable]
subTypeVariable["RDRingInfo"] := Nothing
subTypeVariable[_] := symbolNode @ "idx"



postProcess[enum[type_String]] := Function[composeNode[composeNode[symbolNode @ "enum", stringNode @ type], #]]
postProcess["DataStore"] := Function[composeNode[symbolNode @ "fromDataStore", #]]
postProcess[PostProcessed[type_, fun_, ___]] := Function[
	composeNode[getNode @ fun, postProcess[type][#]]
]


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
		"AsynchronousTaskObject" -> "Void",
		_enum -> Integer,
		PostProcessed[a_,___] :> transformReturn[a]
	}
]

transformLibraryArguments = ReplaceAll[
	{
		{type:(Integer | Real), _?IntegerQ} :> {type, _},
		"RawJSON" -> String,
		"StringList" -> "DataStore",
		_Managed -> Integer,
		"AsynchronousTaskObject" -> Integer,
		_enum -> Integer,
		{"NumericArray", _} -> "NumericArray"
	}
];


makeLibDef[fspec_, libargs_List, libreturn_, libsym_] := setDelayedNode[
	libsym,
	setNode[
		libsym,
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
]

(*makeLibDef[fspec_, libargs_List, libreturn_] := setDelayedNode[
	setNode[
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
]*)


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
getHeadNode[{s_String}] := symbolNode @ s;
getHeadNode[{sym_, _}] := parenNode @ namedPatternNode["expr", blankNode[getHeadNode @ {sym}]]
getHeadNode[{owningType_, type_, _}] := composeNode[
	type,
	listNode @ {
		getHeadNode[{owningType, type}],
		namedBlankNode["idx"]
	}
]

getHeadNode[{owningType_, __, type_, _}] := composeNode[
	type,
	listNode @ {
		getHeadNode[{owningType, type}],
		namedBlankSequenceNode["idx"]
	}
]

getThrowerNode[{s_String}] := symbolNode @ s;
getThrowerNode[s:{__}] := listNode[stringNode /@ s];

makeLHSNode[fspec_, argPatternNodes_] := composeNode[
	getHeadNode @ fspec,
	argPatternNodes
]


makeRHSNode[fun_, variableNodes_List, funOptions_, return_, throws_, libsym_] := Module[
	{res, definitions = {}, opts},
	res = postProcess[return] @ composeNode["Quiet", composeNode[
		If[throws, "catchThrowErrors", "catchReleaseErrors"],
		{
			composeNode[libsym, variableNodes],
			getThrowerNode[fun]
		}
	]];

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
							listNode @ Map[getNode, funOptions[[All, "Name"]]]
						},
						listNode @ Map[getNode, funOptions[[All, "Name"]]]
					]
					
				]
			]	
		];
		opts = preprocessFunctionNode /@ funOptions[[All, "ArgumentType"]];
			
		If[!MatchQ[opts , {Identity ..}],
			res = {res};
			Replace[Reverse @ Thread[{opts, Range @ Length @ opts}],
				{
					{Identity, _} :> Null,
					{function_, n_} :> PrependTo[res,
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
	If[MatchQ[return, $VoidReturnPattern | PostProcessed[_Managed,__]]
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

getVoidNode[Managed[x_Symbol] | PostProcessed[Managed[x_Symbol],__]] := composeNode[symbolNode @ CreateManagedLibraryExpression, {stringNode @ x, symbolNode @ x}]
getVoidNode["AsynchronousTaskObject"] := composeNode[
	symbolNode @ Internal`CreateAsynchronousTask, 
	{
		symbolNode @ "createAsyncTaskID",
		composeNode[symbolNode @ "List", {}],
		symbolNode @ "MonitorAsyncTask"
	}
]

flagConditionedQ[string_] := With[
	{sym = ToExpression[string, StandardForm, Unevaluated]},
	Not[
		FreeQ[DownValues @ sym, HoldPattern[Verbatim[Condition][_, $flag]]]
	]
]

$flagConditionedSymbols = Map[Symbol] @ Select[
	Names[$Context <> "*"],
	flagConditionedQ
]


End[]
EndPackage[]

