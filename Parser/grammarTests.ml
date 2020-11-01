open Rule;;
open Hashset;;
open Grammar;;

Format.printf "\n\t\tGRAMMAR TESTS\n";;

let g = (new grammar)#newGrammar [
	(new rule)#newRule "A" ["B"; "C"];
	(new rule)#newRule "A" ["B"];
	(new rule)#newRule "B" ["x"];
	(new rule)#newRule "C" ["y"; "z"]
];;

let rec printRules (l:rule list) = match l with
	| []      -> ""
	| h :: [] -> h#toString ()
	| h :: t  -> h#toString () ^ "; " ^ printRules t;;

Format.printf "g: \n%s\n" (g#toString ());;

Format.printf "g#isVariable \"A\" = %b\n" (g#isVariable "A");;
Format.printf "g#isVariable \"D\" = %b\n" (g#isVariable "D");;
Format.printf "g#isVariable \"x\" = %b\n" (g#isTerminal "x");;
Format.printf "g#isVariable \"w\" = %b\n" (g#isTerminal "w");;
Format.printf "g#getRulesByVariable \"A\" = [%s]\n" (printRules (g#getRulesByVariable "A"));;
Format.printf "g#getRulesByVariable \"B\" = [%s]\n" (printRules (g#getRulesByVariable "B"));;
Format.printf "g#getRulesByVariable \"C\" = [%s]\n" (printRules (g#getRulesByVariable "C"));;
Format.printf "g#getRulesByVariable \"D\" = [%s]\n" (printRules (g#getRulesByVariable "D"));;