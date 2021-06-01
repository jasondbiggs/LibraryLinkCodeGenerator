
inputAliases = <|
	"RawJSON" -> PreProcessed[String, Developer`WriteRawJSONString]
|>

typeDescriptions = <|
	Integer -> "Integer",
	Real -> "Real",
	String | "UTF8String" -> "String",
	"Void" -> "Null",
	"DataStore" -> "datastore",
	Managed[x_] :> StringJoin[Replace[x, {s_String :> s, s_Symbol :> SymbolName[s]}], " object"],
	{type_, 1} :> StringJoin[Replace[getTypeDescription[type], Except[_String] :> ""], " vector"],
	{type_, 2} :> StringJoin[Replace[getTypeDescription[type], Except[_String] :> ""], " matrix"],
	{type_, n_} :> StringJoin["rank-", IntegerString[n], " tensor of ", Replace[getTypeDescription[type], Except[_String] :> ""], "s"],
	PostProcessed[_, _, desc_] :> desc,
	PostProcessed[x_, _] :> getTypeDescription[x],
	enum[x_] :> x,
	x_ :> ToString[x]

|>

getTypeDescription[type_] := Replace[
	Replace[type, Normal @ typeDescriptions],
	Except[_String] :> ""
]

outputAliases = <|
	"RawJSON" -> PostProcessed[String, Developer`ReadRawJSONString]
|>


scanDoxyString[body_] := Module[
	{parsed = parseDoxygen[body], usage, params, return, res},
	usage = FirstCase[parsed, {"@brief", brief:{__String}} :> brief, {}];
	res = <|"Usage" -> StringRiffle[usage, "\n"]|>;
	res["Parameters"] = Cases[parsed, {"@param", param_} :> parseDoxyParam[param]];
	res["Return"] = FirstCase[parsed, {"@return", ret:{__String}} :> parseReturn[ret], throw[body, "no return"]];
	res["ThrowFailure"] = TrueQ[alwaysThrow || MemberQ[parsed, {"@throws", ___}]];
	FirstCase[
		parsed,
		{"@note", {note__String}} :> (res["Note"] = StringRiffle[note, "\n"])
	];
	res
]


parseDoxygen[body_] := Module[{res},
	res = StringReplace[body, "\t" -> "    "];
	res = First[
		StringCases[res, StringExpression["/**", Shortest[x__], "*/"] :> trim[x]],
		throw[body, "no doxy string"]
	];
	res = StringSplit[res, x:delimiter :> x];
	res = SequenceCases[res,
		{x_String /; StringMatchQ[x, delimiter], y_} :> {x, getLines[y]}
	];
	res
]

trim[st_] := StringTrim[
	StringReplace[StringTrim @ st,
		StringExpression[StartOfLine, Whitespace..., "*"] :> ""
	]
]


getLines[string_] := Map[StringTrim, ImportString[StringTrim @ string, "Lines"]]

delimiter = "@" ~~ Shortest[__] ~~ WordBoundary


parseDoxyLine["@brief", brief:{__String}] := MUsage[StringRiffle[brief, "\n"]]

getparam[var_]:= Replace[
	toExpression[var],
	Except[_List] :> throw[var, "is not a list"]
]

parseDoxyParam[{var_, comment___}] := Module[
	{res = <||>, typeData = Replace[toExpression[var], Except[_List] :> throw[var, "is not a list"]]},
	Switch[typeData,
		{_, _, _argumentPattern},
			res["ParameterType"] = "PatternTest";
			res["ArgumentType"] = First[typeData];
			res["Name"] = typeData[[2]];
			res["Pattern"] = typeData[[3, 1]],
		{_, _, _},
			res["ParameterType"] = "Optional";
			res["ArgumentType"] = First[typeData];
			res["Name"] = typeData[[2]];
			res["DefaultValue"] = typeData[[3]],
		{_, _Rule},
			res["ParameterType"] = "Option";
			res["ArgumentType"] = First[typeData];
			res["Name"] = typeData[[2,1]];
			res["DefaultValue"] = typeData[[2,2]],
		{_, _String},
			res["ParameterType"] = "Required";
			res["ArgumentType"] = First[typeData];
			res["Name"] = typeData[[2]],
		_,
			throw[typeData, "invalid parameter"]
	];
	res["ArgumentDescription"] = getTypeDescription[res["ArgumentType"]];
	res["ArgumentType"] = Replace[res["ArgumentType"], inputAliases];
	If[Length[{comment}] > 0, res["Comment"] = StringRiffle[{comment}, "\n"]];
	res
]


parseReturn[{return_, comment___}] := Module[
	{res = <|"ReturnType" -> toExpression[return]|>},
	res["ReturnDescription"] = getTypeDescription[res["ReturnType"]];
	res["ReturnType"] = Replace[res["ReturnType"], outputAliases];
	If[Length[{comment}] > 0, res["Comment"] = StringRiffle[{comment}, "\n"]];
	res
]


addCustomType[type_, None, output_, description_ : ""] := (
	inputAliases[type] := throw[type, " type is not set up for input"];
	outputAliases[type] = output;
	getTypeDescription[type] = description;
)
addCustomType[type_, input_, None, description_ : ""] := (
	inputAliases[type] = input;
	outputAliases[type] := throw[type, " type is not set up for output"];
	getTypeDescription[type] = description;
)

addCustomType[type_, input_, output_, description_ : ""] := (
	inputAliases[type] = input;
	outputAliases[type] = output;
	getTypeDescription[type] = description;
)

getCustomTypes[files_] := Module[
	{directories,customTypeFiles, comments},


	directories = DeleteDuplicates[DirectoryName /@ files];

	customTypeFiles = Import[#, "Text"]& /@ FileNames["CustomTypes.h", directories];
	comments = Select[Flatten[scanForComments /@ customTypeFiles], StringContainsQ["CustomType["]];
	
	toExpression[StringReplace[comments, "CustomType" -> "addCustomType"]]
]


scanForComments[body_] := StringCases[
	body,
	{
		StringExpression["///", Shortest[c__], EndOfLine] :> trim[c],
		StringExpression["//", Shortest[c__], EndOfLine] :> trim[c],
		StringExpression["/**", Shortest[c__], "*/"] :> trim[c],
		StringExpression["/*", Shortest[c__], "*/"] :> trim[c]
	}
]


progbar[width_, title_] := ({##} /. {ndx_, last_ : False} :>
    print[ "  " <> title<>"\n  [" <>
            StringJoin@ConstantArray["*", ndx] <>
            StringJoin@ConstantArray[" ", width-ndx] <>
            "]" <> ToString[Round[100*ndx/width,1]] <>
            If[last, "", "%   \n\033[3A"] ])&



