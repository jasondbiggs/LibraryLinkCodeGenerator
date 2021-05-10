




scanDoxyString[body_] := Module[
	{parsed = parseDoxygen[body], usage, params, return, res},
	usage = FirstCase[parsed, {"@brief", brief:{__String}} :> brief, {}];
	res = <|"Usage" -> StringRiffle[usage, "\n"]|>;
	res["Parameters"] = Cases[parsed, {"@param", param_} :> parseDoxyParam[param]];
	res["Return"] = FirstCase[parsed, {"@return", ret:{__String}} :> parseReturn[ret], throw[body, "no return"]];
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
	If[Length[{comment}] > 0, res["Comment"] = StringRiffle[{comment}, "\n"]];
	res
]


parseReturn[{return_, comment___}] := Module[{res = <|"ReturnType" -> toExpression[return]|>},
	If[Length[{comment}] > 0, res["Comment"] = StringRiffle[{comment}, "\n"]];
	res
]







