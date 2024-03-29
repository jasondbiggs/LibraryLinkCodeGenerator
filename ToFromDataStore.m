
(* ::Subsection::Closed:: *)
(*DataStore helpers*)


toDataStore[ds_Developer`DataStore] := ds;
toDataStore[Rule[s_String, v_]] := Developer`DataStore[Rule[s, dsPrimitive @ v]]
toDataStore[rules:{__Rule}] := Developer`DataStore @@ MapAt[dsPrimitive, rules, {All, 2}]
toDataStore[obj_List] := Developer`DataStore @@ (dsPrimitive /@ obj)
toDataStore[ass_?AssociationQ /; AllTrue[Keys[ass], StringQ]] := Developer`DataStore @@ Normal[dsPrimitive /@ ass]

dsPrimitive[obj : (_String | _Integer | _Real | {__Integer} | {__Real} | True | False | _Rule)] := obj
dsPrimitive[{s__String}] := Developer`DataStore[s]
dsPrimitive[in : _Association | {__Rule}] := toDataStore[in]
dsPrimitive[in_List] := Block[{pa},Which[
	Developer`PackedArrayQ[in],
		in,
	Developer`PackedArrayQ[pa = Developer`ToPackedArray @ in],
		pa,
	True,
		toDataStore[in]
]]
dsPrimitive[ds_Developer`DataStore] := ds;
dsPrimitive[obj_] := ThrowPacletFailure["NoDataStore", "MessageParameters" -> {obj}]


fromDataStore[arg_] := Block[
	{Developer`DataStore},
	Developer`DataStore[r__Rule] := Association[r];
	Developer`DataStore[r___] := List[r];
	arg
];
