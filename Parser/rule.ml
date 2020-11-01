class rule = object(self)
	val mutable left  = ( "" : string )
	val mutable right = ( [] : string list )
	
	method newRule (l:string) (r:string list) =
		if l != "" && l != "S'" then
			left  <- l
		else
			failwith "The left side of a rule can't be empty nor S'.";
		if List.length r > 0 then
			right <- r
		else
			failwith "Trying to create a rule without a right side.";
		self
	
	method getLeft () =
		left
	
	method getRight () =
		right
	
	method clone (r:rule) =
		left  <- r#getLeft  ();
		right <- r#getRight ();
		self
	
	method getRightWithIndex (i:int) =
		let rec loop (l:string list) (i:int) = match l with
			| []     -> failwith "rule - out of bound"
			| h :: t -> if i = 0 then h else loop t (i-1)
		in loop right i

	method getRightLength () =
		let rec loop (l:string list) = match l with
			| []     -> 0
			| h :: t -> 1 + loop t
		in loop right
	
	method equals (r:rule) =
		if self#getLeft () <> r#getLeft () then
			false
		else
			let rec loop (l1:string list) (l2:string list) = 
			match l1, l2 with
				| [], []         -> true
				| h1::t1, h2::t2 -> 
					if h1 <> h2 then false else loop t1 t2
				| _, _           -> false
			in loop (self#getRight ()) (r#getRight ())
	
	method toString () =
		let rec loop (l:string list) = match l with
			| []     -> ""
			| h :: t -> " " ^ h ^ (loop t)
		in left ^ " ->" ^ (loop right)
end;;