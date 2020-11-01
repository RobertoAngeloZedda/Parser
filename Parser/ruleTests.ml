open Rule;;

Format.printf "\t\tRULE TESTS\n";;

let r1 = (new rule)#newRule "A" ["B"; "C"];;

let rec stringListToString x = match x with
		| []      -> ""
		| h :: [] -> h
		| h :: t  -> h ^ "; " ^ stringListToString t;;

Format.printf "r1: %s\n" (r1#toString ());;

Format.printf "r1#getLeft () = %s\n" (r1#getLeft ());;
Format.printf "r1#getRight () = [%s]\n" (stringListToString (r1#getRight ()));;
Format.printf "r1#getRightWithIndex 0 = %s\n" (r1#getRightWithIndex 0);;
Format.printf "r1#getRightWithIndex 1 = %s\n" (r1#getRightWithIndex 1);;
Format.printf "r1#getRightLength () = %i\n" (r1#getRightLength ());;
Format.printf "r1#equals D -> E = %b\n" (r1#equals ((new rule)#newRule "D" ["E"]));;
Format.printf "r1#equals A -> B C = %b\n" (r1#equals ((new rule)#newRule "A" ["B"; "C"]));;