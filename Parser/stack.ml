class ['a] stack ( str : 'a -> string ) = object(self)
	val mutable items = ([]:'a list)
	
	method size () =
		let rec loop (l:'a list) = match l with
			| []     -> 0
			| h :: t -> 1 + loop t
		in loop items
	
	method push (item:'a) =
		items <- item :: items
	
	method clear () =
		items <- []
	
	method isEmpty () =
		self#size () = 0
	
	method popGet () =
		match items with
			| []     ->
				failwith "stack - Empty stack"
			| h :: t -> 
				h
	
	method pop () =
		match items with
			| []     ->
				failwith "stack - Empty stack"
			| h :: t -> 
				items <- t;
				h
	
	method toList () =
		items
	
	method toString () =
		let rec loop (l:'a list) = match l with
			| []      -> ""
			| h :: [] -> str h
			| h :: t  -> str h ^ "; " ^ (loop t)
		in "{ " ^ loop items ^ " }"
end;;