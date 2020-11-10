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
		
	method getIndex (i:'a) = 
		let rec loop (l:'a list) (c:int) = match l with
			| []     -> -1
			| h :: t ->
				if eq h i then
					c
				else
					loop t (c+1)
		in loop items 0
	
	method getItem (i:int) =
		let rec loop (l:'a list) (i:int) = match l with
			| []     -> failwith "hashset - out of bound"
			| h :: t -> if i = 0 then h else loop t (i-1)
		in loop items i
	
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