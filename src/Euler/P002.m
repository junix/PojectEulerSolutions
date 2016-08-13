(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: P002 *)
(* :Context: P002` *)
(* :Author: junix *)
(* :Date: 2016-08-13 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2016 junix *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["P002`"]
(* Exported symbols added here with SymbolName::usage *)


Fib[x]::usage="fib func"

Begin["`Private`"]

Fib[x_] := x/;x<2
Fib[x_] := Fib[x-1] + Fib[x-2]/;x>=2

End[] (* `Private` *)

EndPackage[]