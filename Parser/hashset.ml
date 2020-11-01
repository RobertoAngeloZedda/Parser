class ['a] hashset ( eq : 'a -> 'a -> bool ) ( str : 'a -> string ) = object(self)
	val mutable items = ([]:'a list)
	
	method size () =
		let rec loop (l:'a list) = match l with
			| []     -> 0
			| h :: t -> 1 + loop t
		in loop items
	
	method add (item:'a) =
		let rec loop (l:'a list) = match l with
			| []     -> [item]
			| h :: t -> 
				if eq h item then
					h :: t
				else
					h :: loop t
		in items <- loop items
	
	method addAll (l:'a list) =
		let rec loop (l:'a list) = match l with
			| []     -> ()
			| h :: t -> 
				self#add h;
				loop t
		in loop l
	
	method clear () =
		items <- []
	
	method isEmpty () =
		self#size () = 0
	
	method remove (item:'a) =
		let rec loop (l:'a list) = match l with
			| []     -> []
			| h :: t -> 
				if eq h item then
					t
				else
					h :: loop t
		in items <- loop items
	
	method contains (item:'a) =
		let rec loop (l:'a list) = match l with
			| []     -> false
			| h :: t -> 
				if eq h item then
					true
				else
					loop t
		in loop items
	
	method containsAll (l:'a list) =
		let rec loop (l:'a list) = match l with
			| []     -> true
			| h :: t ->
				if self#contains h then
					loop t
				else
					false
		in loop l
	
	method toList () =
		items
	
	method equals (hs:'a hashset) =
		self#containsAll (hs#toList ()) && 
		hs#containsAll (self#toList ())
	
	method toString () =
		let rec loop (l:'a list) = match l with
			| []      -> ""
			| h :: [] -> str h
			| h :: t  -> str h ^ "; " ^ (loop t)
		in "{ " ^ loop items ^ " }"
end;;