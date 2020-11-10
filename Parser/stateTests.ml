open Rule;;
open Item;;
open Hashset;;
open Grammar;;
open State;;

Format.printf "\n\t\tSTATE TESTS\n";;

let g = (new grammar)#newGrammar [
	(new rule)#newRule "AND" ["EXP"; "and"; "EXP"], 1;
	(new rule)#newRule "OR" ["EXP"; "or"; "EXP"], 1;
	(new rule)#newRule "NOT" ["not"; "EXP"], 2;
	(new rule)#newRule "TERM" ["true"], 3;
	(new rule)#newRule "TERM" ["false"], 3
] "EXP" [];;

let hs = new hashset (fun (x:item) (y:item) -> x#equals y) (fun (x:item) -> x#toString ());;
hs#add ((new item)#newItem ((new rule)#newRule "START" ["PRIO1"]));;

let s1 = (new state)#newState hs g;;

let rec stringListToString (l:string list) = match l with
	| []      -> ""
	| h :: [] -> h
	| h :: t  -> h ^ "; " ^ stringListToString t;;

Format.printf "g:\n%s\n" (g#toString ());;
Format.printf "s1:\n%s\n" (s1#toString ());;
Format.printf "s1#getCurrentSymbols () = %s\n" (stringListToString (s1#getCurrentSymbols ()));;