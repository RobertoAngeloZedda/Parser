open Stack;;

let s = new stack (fun (x:string) -> x);;

Format.printf "\t\tSTACK TESTS\n";;

Format.printf "s: %s\n" (s#toString ());;
s#push "S";;
Format.printf "s#push \"S\"\n";;

Format.printf "\ns: %s\n" (s#toString ());;
s#push "D";;
Format.printf "s#push \"S\"\n";;

Format.printf "\ns: %s\n" (s#toString ());;
Format.printf "s#size () = %i\n" (s#size ());;
s#push "F";;
Format.printf "s#push \"S\"\n";;

Format.printf "\ns: %s\n" (s#toString ());;
Format.printf "s#pop () = %s\n" (s#pop ());;

Format.printf "\ns: %s\n" (s#toString ());;
Format.printf "s#pop () = %s\n" (s#pop ());;

Format.printf "\ns: %s\n" (s#toString ());;
s#clear ();;
Format.printf "s#clear ()\n";;

Format.printf "\ns: %s\n" (s#toString ());;