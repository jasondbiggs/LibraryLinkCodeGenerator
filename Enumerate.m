
(* ::Subsection::Closed:: *)
(*enumerate*)


(*
	For converting between String values and enums
*)

enumerate[name_, enums:{__String}] := enumerate[name, AssociationThread[enums -> Range[0, Length@enums - 1]]]
enumerate[name_, enums_?AssociationQ] := With[
	{
		data = Join[
			enums,
			Association @ Map[Reverse, Normal[enums]]
		]
	},
	enum[name][num_] := Lookup[
		data, 
		num, 
		ThrowPacletFailure["UnknownEnumValue", "MessageParameters" -> {num, name, Keys[enums]}]
	];
]

