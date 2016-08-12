(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: P001 *)
(* :Context: P001` *)
(* :Author: junix *)
(* :Date: 2016-08-12 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2016 junix *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["P001`"]
(* Exported symbols added here with SymbolName::usage *)

Begin["`Private`"]

End[] (* `Private` *)

Plus@@Select[Range[999],(Mod[#,3]==0 || Mod[#,5]==0)&]

EndPackage[]