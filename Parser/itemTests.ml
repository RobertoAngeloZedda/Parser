open Rule;;
open Item;;

Format.printf "\n\t\tITEM TESTS\n";;

let i = (new item)#newItem ((new rule)#newRule "A" ["B"; "C"; "D"]);;

Format.printf "i: %s\n" (i#toString ());;
Format.printf "i#getCurrentSymbol () = %s\n" (i#getCurrentSymbol ());;
Format.printf "i#isExplored () = %b\n" (i#isExplored ());;
i#nextSymbol ();;
Format.printf "i#nextSymbol ()\n\n";;

Format.printf "i: %s\n" (i#toString ());;
Format.printf "i#getCurrentSymbol () = %s\n" (i#getCurrentSymbol ());;
Format.printf "i#isExplored () = %b\n" (i#isExplored ());;
i#nextSymbol ();;
Format.printf "i#nextSymbol ()\n\n";;

Format.printf "i: %s\n" (i#toString ());;
i#nextSymbol ();;
Format.printf "i#nextSymbol ()\n\n";;

Format.printf "i: %s\n" (i#toString ());;
Format.printf "i#getCurrentSymbol () = %s\n" (i#getCurrentSymbol ());;
Format.printf "i#isExplored () = %b\n" (i#isExplored ());;