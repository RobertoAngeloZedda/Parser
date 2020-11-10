open Rule;;
open Hashset;;
open Grammar;;
open Item;;
open State;;
open Stack;;

class parser = object(self)
	val mutable grammar = new grammar

	val mutable states = new hashset
		(fun (x:state) (y:state) -> x#equals y)
		(fun (x:state) -> x#toString ())
	
	val mutable automa = ( [] : (int * string * int) list )
	
	val mutable stateStack = new stack
		(fun (x:int) -> string_of_int x)

	val mutable symbolStack = new stack
		(fun (x:string) -> x)

	method newParser (g:grammar) =
		(* INIZIALIZZO IL PRIMO STATO *)
		let startRule = List.hd (g#getRulesAsList ()) in
		let startItem = (new item)#newItem (startRule) in
		let hs = new hashset
			(fun (x:item) (y:item) -> x#equals y)
			(fun (x:item) -> x#toString ()) in
		hs#add startItem;
		let startState = (new state)#newState hs g in
		states#add startState;
		
		(* PER TUTTI GLI STATI *)
		let rec loopStates (stateList:state list) = 
		match stateList with
			| [] -> ()
			| stateH :: stateT ->
				(* PER TUTTI I SIMBOLI ATTUALI *)
				let rec loopSymbols (symbolList:string list) = 
				match symbolList with
					| [] -> []
					| symbolH :: symbolT ->
						(* CREO UN HASHSET CHE CONTERRA' 
						   GLI ITEM DI PARTENZA DEL NUOVO STATO *)
						let nextStateItems = new hashset
							(fun (x:item) (y:item) -> x#equals y)
							(fun (x:item) -> x#toString ()) in
						
						(* PER TUTTI GLI ITEM RAGGIUNGIBILI DELLO STATO ATTUALE *)
						let rec loopItem (itemList:item list) = 
						match itemList with
							| [] -> ()
							| itemH :: itemT ->
								(* SE L'ITEM NON E' ESPLORATO 
								   E IL SIMBOLO ATTUALE CORRISPONDE 
								   A QUELLO DEL LOOP *)
								if itemH#isExplored () = false &&
								   itemH#getCurrentSymbol () = symbolH then
										(* CLONO L'ITEM, PASSO AL SIMBOLO SUCCESSIVO
										   E LO AGGIUNGO ALL'HASHSET *)
										let newItem = (new item)#clone itemH
										in newItem#nextSymbol ();
										nextStateItems#add newItem;
										loopItem itemT
								else
										loopItem itemT
							
						in loopItem (stateH#getReachableItemsAsList ());
						
						(* CREO IL NUOVO STATO A PARTIRE
						   DALL'HASHSET APPENA POPOLATO *)
						let newState = (new state)#newState nextStateItems g in

						(* AGGIUNGO UNA NUOVA TRANSIZIONE ALL'AUTOMA
						   E SE LO STATO NON ESISTE ANCORA LO AGGIUNGO *)
						if states#contains newState then (
							automa <- automa @ [(states#getIndex (stateH)), 
							                    (symbolH), 
												(states#getIndex (newState))];
							loopSymbols symbolT
						) else (
							states#add newState;
							automa <- automa @ [(states#getIndex (stateH)), 
							                    (symbolH), 
												(states#getIndex (newState))];
							newState :: loopSymbols symbolT
						)
						
				in loopStates (stateT @ loopSymbols (stateH#getCurrentSymbols ()))
				
		in loopStates [startState];
		grammar <- g;
		self
	
	method isEnd (state:int) =
		let rec loop (l:item list) = match l with
			| [] -> false
			| h :: t ->
				if h#isExplored () &&
				   (h#getRule ())#getLeft () = grammar#getStartSymbol ()
				   then
					true
				else
					loop t
		in loop (((states#getItem state)#getReachableItems ())#toList ())
	
	method nextState (state:int) (symbol:string) =
		let rec loop (a:(int * string * int) list) = match a with
			| [] -> -1
			| (f, s, d) :: t  ->
				if state = f && symbol = s then
					d
				else
					loop t
		in loop automa
	
	method create (state:int) (symbol:string) =
		let rec loop (l:item list) = match l with
			| [] -> ""
			| h :: t ->
				if (h#isExplored ()) &&  (symbol = "" ||
				    grammar#isEndingSymbol ((h#getRule ())#getLeft ()) symbol)
				   then (
					if grammar#isPrioritySymbol ((h#getRule ())#getLeft ()) then (
						Format.printf "\t\t..di priorita'\n";
						let _ = stateStack#pop () in ()
					)
					else if grammar#isVariable ((h#getRule ())#getLeft ()) then (
						Format.printf "\t\t..di un operazione\n";
						let rec loopRight (l:string list) = match l with
							| []      -> ")"
							| h :: [] ->
								let _ = stateStack#pop () in
								if grammar#isVariable h then
									symbolStack#pop () ^ ")"
								else
									")"
							| h :: t  -> 
								let _ = stateStack#pop () in
								if grammar#isVariable h then
									symbolStack#pop () ^ ", " ^ loopRight t
								else
									loopRight t
									
						in symbolStack#push (
							(h#getRule ())#getLeft () ^ "( " ^
							loopRight ((h#getRule ())#getRight ())
						)
					)
					else (
						Format.printf "\t\t..di un terminale\n";
						let _ = stateStack#pop () in ();
						symbolStack#push symbol
					);
					Format.printf "\t\t(restituisco %s)\n" ((h#getRule ())#getLeft ());
					(h#getRule ())#getLeft ()
				)
				else
					loop t
		in loop (((states#getItem state)#getReachableItems ())#toList ())
	
	method parse (l:string list) =
		let rec loop (input:string list) = 
			Format.printf "stateStack: %s\n" (stateStack#toString ());
			Format.printf "symbolStack: %s\n\n" (symbolStack#toString ());
			match input with
			| [] ->
				Format.printf "symbol \"$\"\n";
				if self#isEnd (stateStack#popGet ()) then (
					Format.printf "Concludo\n\n";
					let result = symbolStack#pop () in
					stateStack#clear ();
					symbolStack#clear ();
					result
				)
				else (
					let s = self#create (stateStack#popGet ()) "" in
					if s = "" then
						failwith "Cannot be parsed"
					else (
						Format.printf "Creo una regola..\n";
						loop [s]
					)
				)
			| h :: t ->
				Format.printf "symbol \"%s\"\n" h;
				let s = self#create (stateStack#popGet ()) h in
				if s = "" then
					let i = self#nextState (stateStack#popGet ()) h
					in if i = -1 then
						failwith "Cannot be parsed"
					else (
						Format.printf "Mi sposto da %i a %i\n" ((stateStack#popGet ())) (i);
						stateStack#push i;
						loop t
					)
				else (
					Format.printf "Creo una regola..\n";
					loop (s :: h :: t)
				)
		in stateStack#push 0;
		loop l
	
	method toString () =
		let rec loopAutoma (l:(int * string * int) list) = match l with
			| []      -> ""
			| (from, symbol, destination) :: t  -> 
				string_of_int from ^ ", " ^ symbol ^ ", " ^
				string_of_int destination ^ "\n" ^ loopAutoma t
		in let rec loopStates (l:state list) (c:int) = match l with
			| []      -> ""
			| h :: t  -> 
				string_of_int c ^ ": " ^ (h#getReachableItems ())#toString () ^ 
				"\n" ^ loopStates t (c+1)
		in loopStates (states#toList ()) 0 ^ "\n\n" ^ loopAutoma automa
end;;