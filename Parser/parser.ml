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
	
	val mutable program = ( [] : string list )

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
		let rec loopBasicTypes 
			(l:(string*string) list)
			(s:string) =
		match l with
			| [] -> ""
			| (_, r) :: t ->
				if (Str.string_match (Str.regexp r)) s 0 
				then
					r
				else
					loopBasicTypes t s
		in let rec loop (a:(int * string * int) list) = match a with
			| [] -> -1
			| (f, s, d) :: t  ->
				if state = f && 
				   s = loopBasicTypes 
					(grammar#getTerminalRegexp ()) symbol 
				then (
					symbolStack#push symbol;
					d
				)
				else if state = f && symbol = s then
					d
				else
					loop t
		in loop automa
	
	method create (state:int) (symbol:string) =
		let rec loop (l:item list) = match l with
			| [] -> ""
			| h :: t ->
				if (h#isExplored ()) then (
					if symbol = "" || grammar#isEndingSymbol ((h#getRule ())#getLeft ()) symbol then (
						if grammar#isPrioritySymbol ((h#getRule ())#getLeft ()) then (
							Format.printf "\t\t..di priorita'\n";
							let _ = stateStack#pop () in ();
							Format.printf "\t\t(restituisco %s)\n" ((h#getRule ())#getLeft ());
							(h#getRule ())#getLeft ()
						)
						else if grammar#isTerminal ((h#getRule ())#getLeft ()) then (
							let term = symbolStack#popGet () in
							Format.printf "\t\t..di un terminale (%s)\n" term;
							let rec loopBasicTypes (l:(string*string) list) = match l with
								| (n, r) :: t ->
									if (h#getRule ())#getLeft () = n && Str.string_match (Str.regexp r) term 0 then (
										Format.printf "\t\t(restituisco %s)\n" ((h#getRule ())#getLeft ());
										let _ = symbolStack#pop () in
										let _ = stateStack#pop () in
										symbolStack#push (n ^ "(" ^ term ^ ")");
										n
									)
									else
										loopBasicTypes t
								| _ -> ""
							in loopBasicTypes (grammar#getTerminalRegexp ())
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
								(h#getRule ())#getLeft () ^ "(" ^
								loopRight ((h#getRule ())#getRight ())
							);
							Format.printf "\t\t(restituisco %s)\n" ((h#getRule ())#getLeft ());
							(h#getRule ())#getLeft ()
						)
						else (
							failwith "parser - Error1"
						)
					)
					else (
						loop t
					)
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
	
	method programFromFile (fileName:string) =
		(* Ripulisco la lista *)
		program <- [];
		
		(* Apro il file e lo leggo riga per riga *)
		let chan = (open_in fileName) in
		let rec readLines (l:string list) = 
		match input_line chan with
			| line -> 
				if (String.get line ((String.length line) - 1) = '\r') then
					(String.sub line 0 ((String.length line) - 1)) :: readLines l
				else
					line :: readLines l
			| exception End_of_file -> 
				close_in chan;
				List.rev l in

		(* Per ogni riga *)
		let rec loopLines (ll:string list) = match ll with
			| [] -> ()
			| lh :: lt ->
				(* Rimuovo eventuali spazi bianchi all'inizio *)
				let removeWhiteSpaces (s:string) =
					let regex = Str.regexp "[ |\t]+" in
					if Str.string_match regex lh 0 then
						String.sub lh (Str.match_end ()) ((String.length lh) - (Str.match_end ()))
					else
						s
				in let str = removeWhiteSpaces lh in
				
				(* Provo il matching per ogni terminale *)
				let rec loopTerminals (tl:(string*string) list) = match tl with
					| [] ->
						let regex1 = Str.regexp ".+[ |\t]+" in
						let regex2 = Str.regexp "[ |\t]+" in
						if Str.string_match regex1 str 0 then (
							let _ = Str.search_forward  regex2 str 0 in
							let s1 = String.sub str 0 (Str.match_beginning  ()) in
							let s2 = String.sub str (Str.match_end ()) ((String.length str) - (Str.match_end ())) in
							program <- program @ [s1];
							loopLines (s2 :: lt)
						)
						else (
							program <- program @ [str];
							loopLines lt
						)
					| (_, re) :: tt ->
						if Str.string_match (Str.regexp re) str 0 then (
							let s1 = String.sub str 0 (Str.match_end ()) in
							let s2 = String.sub str (Str.match_end ()) ((String.length str) - (Str.match_end ())) in
							program <- program @ [s1];
							loopLines (s2 :: lt)
						)
						else
							loopTerminals tt 
						in
				if str = "" then
					loopLines lt
				else
					loopTerminals (grammar#getTerminalRegexp ()) in
		loopLines (readLines []);
		program
	
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