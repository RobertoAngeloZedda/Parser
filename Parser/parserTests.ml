open Rule;;
open Item;;
open Hashset;;
open Grammar;;
open State;;
open Stack;;
open Parser;;

let g = (new grammar)#newGrammar [
	(new rule)#newRule "AND" ["EXP"; "and"; "EXP"], 1;
	(new rule)#newRule "OR" ["EXP"; "or"; "EXP"], 1;
	(new rule)#newRule "NOT" ["not"; "EXP"], 2;
	(new rule)#newRule "BOOL" ["true"], 3;
	(new rule)#newRule "BOOL" ["false"], 3
] "EXP" [];;

let p = (new parser)#newParser g;;

Format.printf "\np: \n%s\n" (p#toString ());

Format.printf "p#isEnd 0 = %b\n" (p#isEnd 0);
Format.printf "p#isEnd 1 = %b\n" (p#isEnd 1);
Format.printf "p#nextState 7 NOT = %i\n" (p#nextState 7 "NOT");
Format.printf "p#nextState 7 AND = %i\n" (p#nextState 7 "AND");
Format.printf "p#nextState 15 and = %i\n" (p#nextState 15 "and");
Format.printf "p#nextState 15 not = %i\n\n" (p#nextState 15 "not");

(*Format.printf "p#parse [not not true] = %s\n\n" (p#parse ["not"; "not"; "true"]);;*)

(*Format.printf "p#parse [true and true] = %s\n\n" (p#parse ["true"; "and"; "true"]);;*)

Format.printf "p#parse [true and true or not false] = %s\n\n" 
	(p#parse [
		"true"; "and"; "true"; "or"; "not"; "false"
	]);;