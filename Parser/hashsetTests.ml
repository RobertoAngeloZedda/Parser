open Rule;;
open Hashset;;

Format.printf "\n\t\tHASHSET TESTS\n";;

let hs1 = new hashset (fun x y -> x = y) (fun x -> x);;
Format.printf "hs1: %s\n" (hs1#toString ());;
hs1#add "A";;
Format.printf "hs1#add \"A\"\n\n";;

Format.printf "hs1: %s\n" (hs1#toString ());;
hs1#addAll ["B"; "C"; "C"];;
Format.printf "hs1#addAll [\"B\"; \"C\"; \"C\"]\n\n";;

Format.printf "hs1: %s\n" (hs1#toString ());;
Format.printf "hs1#size () = %i\n" (hs1#size ());;
Format.printf "hs1#getIndex \"C\" = %i\n" (hs1#getIndex "C");;
Format.printf "hs1#getItem 1 = %s\n" (hs1#getItem 1);;
hs1#remove "A";;
Format.printf "hs1#remove \"A\"\n\n";;

Format.printf "hs1: %s\n" (hs1#toString ());;
Format.printf "hs1#size () = %i\n" (hs1#size ());;
Format.printf "hs1#contains \"A\" = %b\n" (hs1#contains "A");;
Format.printf "hs1#contains \"B\" = %b\n" (hs1#contains "B");;
Format.printf "hs1#containsAll [\"A\"; \"B\"] = %b\n" (hs1#containsAll ["A"; "B"]);;
Format.printf "hs1#containsAll [\"B\"; \"C\"] = %b\n" (hs1#containsAll ["B"; "C"]);;
Format.printf "hs1#isEmpty () = %b\n" (hs1#isEmpty ());;
hs1#clear ();;
Format.printf "hs1#clear ()\n\n";;

Format.printf "hs1: %s\n" (hs1#toString ());;
Format.printf "hs1#isEmpty () = %b\n\n" (hs1#isEmpty ());;

let hs2 = new hashset (fun (x:rule) (y:rule) -> x#equals y) (fun (x:rule) -> x#toString ());;

let r1 = (new rule)#newRule "A" ["B"; "C"];;
let r2 = (new rule)#newRule "D" ["E"];;
let r3 = (new rule)#newRule "F" ["G"; "H"; "I"];;

Format.printf "hs2: %s\n" (hs2#toString ());;
hs2#add r1;;
Format.printf "hs2#add A -> B C\n\n";;

Format.printf "hs2: %s\n" (hs2#toString ());;
hs2#addAll [r2; r2; r3];;
Format.printf "hs2#addAll [D -> E; D -> E; F -> G H I]\n\n";;

Format.printf "hs2: %s\n" (hs2#toString ());;
Format.printf "hs2#size () = %i\n" (hs2#size ());;
Format.printf "hs2#getIndex F -> G H I = %i\n" (hs2#getIndex r3);;
Format.printf "hs2#getItem 1 = %s\n" ((hs2#getItem 1)#toString ());;
hs2#remove (r1);;
Format.printf "hs2#remove A -> B C\n\n";;

Format.printf "hs2: %s\n" (hs2#toString ());;
Format.printf "hs2#contains A -> B C = %b\n" (hs2#contains r1);;
Format.printf "hs2#contains D -> E = %b\n" (hs2#contains r2);;
Format.printf "hs2#containsAll [A -> B C; D -> E] = %b\n" (hs2#containsAll [r1; r2]);;
Format.printf "hs2#containsAll [D -> E; F -> G H I] = %b\n" (hs2#containsAll [r2; r3]);;
Format.printf "hs2#isEmpty () = %b\n" (hs2#isEmpty ());;
hs2#clear ();;
Format.printf "hs2#clear ()\n\n";;

Format.printf "hs2: %s\n" (hs2#toString ());;
Format.printf "hs2#isEmpty () = %b\n" (hs2#isEmpty ());;