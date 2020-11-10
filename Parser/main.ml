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

Format.printf "\n%s\n" 
	(p#parse [
		"true"; "and"; "true"; "or"; "not"; "false"
	]);;