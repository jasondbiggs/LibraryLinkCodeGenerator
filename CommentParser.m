




scanDoxyString[body_] := Module[
	{parsed = parseDoxygen[body], usage, params, return},
	usage = FirstCase[parsed, {"@brief", brief:{__String}} :> MUsage[StringRiffle[brief, "\n"]], Nothing];
	params = Cases[parsed, {"@param", param_} :> parseDoxyParam[param]];
	return = FirstCase[parsed, {"@brief", brief:{__String}} :> MUsage[StringRiffle[brief, "\n"]], throw[body, "no return"]];
	{usage, params, return}
	
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

parseDoxyParam[{var_}] := Replace[
	toExpression[var],
	Except[_List] :> throw[var, "is not a list"]
];

parseDoxyParam[{var_, comment__}] := AppendTo[
	parseDoxyLine["@param", {var}],
	PUsage @ StringRiffle[{comment}, "\n"]
]


parseDoxyLine["@return", {return_, comment___}] := Apply[
	MReturn,
	{toExpression @ return, StringRiffle[{comment}, "\n"]}
]







