open Rule;;
open Hashset;;
open Grammar;;

Format.printf "\n\t\tGRAMMAR TESTS\n";;

let g = (new grammar)#newGrammar [
	(new rule)#newRule "EQUAL" ["EXP"; "="; "EXP"], 1;
	(new rule)#newRule "LESS"  ["EXP"; "<"; "EXP"], 1;
	(new rule)#newRule "TIMES" ["EXP"; "*"; "EXP"], 3;
	(new rule)#newRule "PLUS"  ["EXP"; "+"; "EXP"], 2;
	(new rule)#newRule "MINUS" ["EXP"; "-"; "EXP"], 2;
] "EXP" [
	"TERM", "id"
];;

let rec printRules (l:rule list) = match l with
	| []      -> ""
	| h :: [] -> h#toString ()
	| h :: t  -> h#toString () ^ "; " ^ printRules t;;

Format.printf "g: \n%s\n" (g#toString ());;

Format.printf "g#isVariable \"LESS\" = %b\n" (g#isVariable "LESS");;
Format.printf "g#isVariable \"IF\" = %b\n" (g#isVariable "IF");;
Format.printf "g#isVariable \"PRIO1\" = %b\n" (g#isTerminal "PRIO1");;
Format.printf "g#isVariable \"AND\" = %b\n" (g#isTerminal "AND");;
Format.printf "g#getRulesByVariable \"PRIO2\" = [%s]\n" (printRules (g#getRulesByVariable "PRIO2"));;
Format.printf "g#getRulesByVariable \"TIMES\" = [%s]\n" (printRules (g#getRulesByVariable "TIMES"));;
Format.printf "g#getRulesByVariable \"AND\" = [%s]\n" (printRules (g#getRulesByVariable "AND"));;
Format.printf "g#isPrioritySymbol \"PRIO2\" = %b\n" (g#isPrioritySymbol "PRIO2");;
Format.printf "g#isPrioritySymbol \"NOT\" = %b\n" (g#isPrioritySymbol "NOT");;
Format.printf "g#getEndingSymbols \"START\" = [ %s ]\n" ((g#getEndingSymbols "START")#toString ());;
Format.printf "g#getEndingSymbols \"EQUAL\" = [ %s ]\n" ((g#getEndingSymbols "EQUAL")#toString ());;
Format.printf "g#getEndingSymbols \"PRIO3\" = [ %s ]\n" ((g#getEndingSymbols "PRIO3")#toString ());;
Format.printf "g#getEndingSymbols \"PRIO1\" \"=\" = %b\n" (g#isEndingSymbol "PRIO1" "=");;
Format.printf "g#getEndingSymbols \"PRIO1\" \"*\" = %b\n" (g#isEndingSymbol "PRIO1" "*");;