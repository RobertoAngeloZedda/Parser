open Rule;;
open Item;;
open Hashset;;
open Grammar;;
open State;;

Format.printf "\n\t\tSTATE TESTS\n";;

let r1 = (new rule)#newRule "A" ["B"; "C"];;
let r2 = (new rule)#newRule "B" ["y"; "B"];;
let r3 = (new rule)#newRule "B" ["C"];;
let r4 = (new rule)#newRule "C" ["x"];;

let g = (new grammar)#newGrammar [r1; r2; r3; r4];;

let i1 = (new item)#newItem r1;;
let i2 = (new item)#newItem r2;;
let i3 = (new item)#newItem r3;;
let i4 = (new item)#newItem r4;;

let hs = new hashset (fun (x:item) (y:item) -> x#equals y) (fun (x:item) -> x#toString ());;
hs#add i1;;

let s1 = (new state)#newState hs g;;

let rec stringListToString (l:string list) = match l with
	| []      -> ""
	| h :: [] -> h
	| h :: t  -> h ^ "; " ^ stringListToString t;;

Format.printf "g:\n%s\n" (g#toString ());;
Format.printf "s1:\n%s\n" (s1#toString ());;
Format.printf "s1#getCurrentSymbols () = %s\n" (stringListToString (s1#getCurrentSymbols ()));;

(*hs#clear ();;
hs#addAll ((new state)#createItems (g#getRulesByVariable "B"));;
let s2 = (new state)#newState hs g;;
Format.printf "s2 (from s1 with \"B\"):\n%s\n" (s2#toString ());;

hs#clear ();;
hs#addAll ((new state)#createItems (g#getRulesByVariable "C"));;
let s3 = (new state)#newState hs g;;
Format.printf "s3 (from s1 with \"C\"):\n%s\n" (s3#toString ());;

hs#clear ();;
hs#addAll ((new state)#createItems (g#getRulesByVariable "y"));;
let s4 = (new state)#newState hs g;;
Format.printf "s4 (from s1 with \"y\"):\n%s\n" (s4#toString ());;

hs#clear ();;
hs#addAll ((new state)#createItems (g#getRulesByVariable "x"));;
let s5 = (new state)#newState hs g;;
Format.printf "s5 (from s1 with \"x\"):\n%s\n" (s5#toString ());;*)