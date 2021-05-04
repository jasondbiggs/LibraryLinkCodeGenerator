
(* ::Subsection::Closed:: *)
(*DataStore helpers*)


toDataStore[ds_DataStore] := ds;
toDataStore[Rule[s_String, v_]] := DataStore[Rule[s, dsPrimitive @ v]]
toDataStore[rules:{__Rule}] := DataStore @@ MapAt[dsPrimitive, rules, {All, 2}]
toDataStore[obj_List] := DataStore @@ (dsPrimitive /@ obj)
toDataStore[ass_?AssociationQ /; AllTrue[Keys[ass], StringQ]] := DataStore @@ Normal[dsPrimitive /@ ass]

dsPrimitive[obj : (_String | _Integer | _Real | {__Integer} | {__Real} | True | False)] := obj
dsPrimitive[{s__String}] := DataStore[s]
dsPrimitive[obj_] := $PacletFailureFunction["NoDataStore", "MessageParameters" -> {obj}]


fromDataStore[arg_] := Block[
	{Developer`DataStore},
	Developer`DataStore[r__Rule] := Association[r];
	Developer`DataStore[r___] := List[r];
	arg
];
