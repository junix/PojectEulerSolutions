(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA *)

(* :Title: P002 *)
(* :Context: P002` *)
(* :Author: junix *)
(* :Date: 2016-08-14 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: *)
(* :Copyright: (c) 2016 junix *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["P002`"]
(* Exported symbols added here with SymbolName::usage *)

P002[]::usage="sum of fib seq"

Begin["`Private`"]
sumOfFib[n_] := 0 /; Fibonacci[n] > 4000000
sumOfFib[n_] := Fibonacci[n] + sumOfFib[n + 1] /; EvenQ@Fibonacci[n]
sumOfFib[n_] := sumOfFib[n + 1] /; OddQ@Fibonacci[n]

P002[] = sumOfFib[0]

End[] (* `Private` *)

EndPackage[]