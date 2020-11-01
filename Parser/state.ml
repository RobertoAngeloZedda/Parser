open Rule;;
open Item;;
open Hashset;;
open Grammar;;

class state = object(self)
	val mutable sourceItems = new hashset
		(fun (x:item) (y:item) -> x#equals y)
		(fun (x:item) -> x#toString ())
		
	val mutable reachableItems = new hashset
		(fun (x:item) (y:item) -> x#equals y)
		(fun (x:item) -> x#toString ())
	
	method createItems (l:rule list) = 
		let rec loop (l:rule list) = match l with
			| []     -> []
			| h :: t ->
				let i = (new item)#newItem h
				in i :: loop t
		in loop l
	
	method newState (s:item hashset) (g:grammar) =
		let rec goDeep (l:item list) = match l with
			| []     -> []
			| h :: t ->
				if h#isExplored () = false && 
				   g#isVariable (h#getCurrentSymbol ()) then
						g#getRulesByVariable (h#getCurrentSymbol ()) @
						goDeep t
				else
					goDeep t
		in let rec loop () = 
			let temp = self#createItems (goDeep (reachableItems#toList ()))
			in if reachableItems#containsAll (temp) then
				()
			else (
				reachableItems#addAll (temp);
				loop()
			)
		in sourceItems <- s;
		reachableItems#addAll (sourceItems#toList ());
		loop ();
		self
	
	method getSourceItems () =
		sourceItems
	
	method getReachableItems () =
		reachableItems
	
	method getCurrentSymbols () =
		let rec loop (l:item list) (hs:string hashset) = match l with
			| []     -> hs#toList ()
			| h :: t ->
				if h#isExplored () = false then (
					hs#add (h#getCurrentSymbol ());
					loop t hs
				)
				else
					loop t hs
		in let symbols = new hashset (fun x y -> x = y) (fun x -> x)
		in loop (reachableItems#toList ()) symbols
	
	method equals (s:state) =
		reachableItems#equals (s#getReachableItems ())
	
	method toString () =
		"sourceItems:\n" ^ sourceItems#toString () ^ "\n" ^
		"reachableItems:\n" ^ reachableItems#toString () ^ "\n"
end;;