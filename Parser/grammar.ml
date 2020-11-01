open Rule;;
open Hashset;;

class grammar = object(self)
	val mutable rules = new hashset
		(fun (x:rule) (y:rule) -> x#equals y)
		(fun (x:rule) -> x#toString ())
	
	val mutable variables = new hashset
		(fun x y -> x = y) (fun x -> x)
	
	val mutable terminals = new hashset
		(fun x y -> x = y) (fun x -> x)
	
	method newGrammar (l:rule list) =
		let rec removeTerminals (l:string list) = match l with
			| []     -> ()
			| h :: t -> 
				if terminals#contains h then terminals#remove h else ();
				removeTerminals t
		in let rec loopStrings (l:string list) = match l with 
			| []     -> ()
			| h :: t -> 
				terminals#add h;
				loopStrings t
		in let rec loopRules (l:rule list) = match l with
			| []     -> ()
			| h :: t ->
				variables#add (h#getLeft ());
				loopStrings (h#getRight ());
				rules#add h;
				loopRules t
		in loopRules l;
		removeTerminals (variables#toList ());
		self
	
	method getRules () =
		rules
	
	method getRulesAsList () =
		rules#toList ()
	
	method isVariable (s:string) =
		variables#contains s
	
	method isTerminal (s:string) =
		terminals#contains s	
	
	method getRulesByVariable (s:string) =
		let rec loop (l:rule list) = match l with
			| []     -> []
			| h :: t ->
				if h#getLeft () = s then
					h :: loop t
				else
					loop t
		in loop (rules#toList ())
	
	method toString () =
		"Rules:\n"     ^ rules#toString () ^
		"\nVariables:\n" ^ variables#toString () ^
		"\nTerminals:\n" ^ terminals#toString () ^ "\n"
end;;