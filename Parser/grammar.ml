open Rule;;
open Hashset;;

class grammar = object(self)
	val mutable rulesWithPrio = new hashset
		(fun (x:(rule*int)) (y:(rule*int)) ->
		 match x, y with
			| (xr, _), (yr, _) ->
				xr#getLeft () = yr#getLeft ()
		)
		(fun (x:(rule*int)) -> "")

	val mutable rules = new hashset
		(fun (x:rule) (y:rule) -> x#equals y)
		(fun (x:rule) -> x#toString ())
	
	val startSymbol = ( "START" : string )
	
	val mutable priorities = ( [] : int list )
	
	val mutable variables = new hashset
		(fun x y -> x = y) (fun x -> x)
	
	val mutable terminals = new hashset
		(fun x y -> x = y) (fun x -> x)
		
	val mutable terminalRegexp = 
		( [] : (string * string) list )
	
	method getRulesWithPrio () =
		rulesWithPrio
	
	method getRulesWithPrioAsList () =
		rulesWithPrio#toList ()
	
	method getRules () =
		rules
		
	method getRulesAsList () =
		rules#toList ()
	
	method getStartSymbol () =
		startSymbol
	
	method getPriorities () =
		priorities
	
	method prioritiesAsHashsetSymbols () =
		let prioritySymbols = new hashset
			(fun x y -> x = y) (fun x -> x)
		
		in let rec loop (l:int list) = match l with
			| []      -> ()
			| h :: t  -> 
				prioritySymbols#add 
					("PRIO" ^ string_of_int h);
				loop t
		in loop priorities;
		prioritySymbols
	
	method isPrioritySymbol (s:string) =
		(self#prioritiesAsHashsetSymbols ())#contains s
	
	method getVariables () =
		variables
	
	method getVariablesAsList () =
		variables#toList ()
	
	method isVariable (s:string) =
		variables#contains s
	
	method getTerminals () =
		terminals
	
	method getTerminalsAsList () =
		variables#toList ()
	
	method isTerminal (s:string) =
		terminals#contains s
		
	method getTerminalRegexp () =
		terminalRegexp
		
	method getRegexp (name:string) =
		let rec loop (l:(string*string) list) = match l with
			| []     ->
				failwith "grammar - No regexp with this name"
			| (n, r) :: t ->
				if n = name then
					r
				else
					loop t
		in loop terminalRegexp
		
	method newGrammar (l:(rule * int) list) 
		              (genericSymbol:string)
					  (basicTypes:(string*string) list) =
		rulesWithPrio#addAll l;
		terminalRegexp <- basicTypes;

		(* Creo una lista che contiene tutte le priorita' 
		   senza ripetizioni e ordinate in ordine crescente *)
		let rec findPriorities (l:(rule * int) list) = match l with
			| []     -> ()
			| (hr, hp) :: t ->
				let rec addPriorities (l:int list) (p:int) = match l with
					| []     -> [p]
					| h :: t ->
						if p < h then
							p :: h :: t
						else if p = h then
							h :: t
						else
							h :: addPriorities t p
				in priorities <- addPriorities priorities hp;
				findPriorities t
		in findPriorities l;
		
		(* Crelo la regola di partenza che deve essere unica *)
		let createStartRule (l:int list) = match l with
			| h :: t -> 
				rules#add ((new rule)#newRule 
					startSymbol
					["PRIO" ^ string_of_int h]
				)
			| _      -> failwith "grammar - no rules"
		in createStartRule priorities;
		
		(* Creo le regole che esprimono la relazione
		   tra regola di priorita' e regola generiche *)
		let rec createPrioRules (l:(rule * int) list) = match l with
			| []     -> ()
			| (hr, hp) :: t ->
				rules#add ((new rule)#newRule 
					("PRIO" ^ string_of_int hp)
					[hr#getLeft ()]
				);
				createPrioRules t
		in createPrioRules l;
		
		(* Creo le regole che definiscono la gerarchia 
		   tra le regole di priorita' *)
		let rec createPrioLinkRules (l:int list) = match l with
			| [] -> ()
			| ph :: [] ->
				(* Lego l'ultimo livello di priorita
				   con i tipi base (int, char, bool)
				   che hanno la priorita piu alta di tutti *)
					let rec loopBasicTypes(l:(string*string) list) =
					match l with
						| [] -> ()
						| (name, regex) :: t ->
							rules#add ((new rule)#newRule
								("PRIO" ^ string_of_int ph)
								[name]
							);
							terminals#add name;
							loopBasicTypes t
					in loopBasicTypes basicTypes
			| h1 :: h2 :: t ->
				rules#add ((new rule)#newRule
					("PRIO" ^ string_of_int h1)
					["PRIO" ^ string_of_int h2]
				);
				createPrioLinkRules (h2 :: t)
		in createPrioLinkRules priorities;
		
		(* Aggiungo la regola di partenza
		   all'hashset che contiene tutte le variabili *)
		variables#add startSymbol;
		
		(* Aggiungo i nomi delle regole di priorita 
		   all'hashset che contiene tutte le variabili *)
		variables#addAll ((self#prioritiesAsHashsetSymbols ())#toList ());
		
		(* Creo le regole generiche
		   e aggiungo i nomi delle regole
		   all'hashset che contiene tutte le variabili *)
		let rec createGenericRules (l:(rule * int) list) = match l with
			| []     -> ()
			| (hr, hp) :: t ->
				let rec newRight (l:string list) (oldS:string)
				                 (newS:string) = match l with
					| []     -> []
					| h :: t ->
						if h = oldS then
							newS :: newRight t oldS newS
						else
							h :: newRight t oldS newS
				in rules#add ((new rule)#newRule
					(hr#getLeft ())
					(newRight (hr#getRight ()) genericSymbol
						("PRIO" ^ string_of_int hp)
					)
				);
				variables#add (hr#getLeft ());
				createGenericRules t
		in createGenericRules l;
		
		(* Creo le regole per i tipi terminali *)
		let rec createBasicTypeRules (l:(string*string) list) = match l with
			| []          -> ()
			| (n, r) :: t ->
				rules#add ((new rule)#newRule n [r]);
				variables#add n;
				createBasicTypeRules t
		in createBasicTypeRules basicTypes;
		
		self
	
	method getRulesByVariable (s:string) =
		let rec loop (l:rule list) = match l with
			| []     -> []
			| h :: t ->
				if h#getLeft () = s then
					h :: loop t
				else
					loop t
		in loop (rules#toList ())
		
	method getEndingSymbols (s:string) =
		let symbols = new hashset
			(fun (x:string) (y:string) -> x = y)
			(fun (x:string) -> x) in
		let rec loopPrioSymbols (l:string list) (s:string) =
		match l with
			| [] -> symbols
			| h :: t ->
				let rec loopRules (l:rule list) (s) = match l with
					| [] -> ()
					| rh :: rt ->
						let rec loopSymbols (l:string list) = 
						match l with
							| sh1 :: sh2 :: st ->
								if sh1 = s && 
								   self#isVariable sh2 = false then (
									symbols#add sh2;
									loopSymbols (sh2 :: st)
								)
								else
									loopSymbols (sh2 :: st)
							| _ -> ()
						in loopSymbols (rh#getRight ());
						loopRules rt s
				in loopRules (rules#toList ()) h;
				if h = s then
					symbols
				else
					loopPrioSymbols t s
				
		in if s = startSymbol then
			symbols
		else if self#isPrioritySymbol s then
			loopPrioSymbols
				((self#prioritiesAsHashsetSymbols ())#toList ()) s
		else if self#isTerminal s then
			let rec getLastPrio (l:int list) = match l with
				| []      -> 
					failwith "grammr - no priorities"
				| h :: [] ->
					h
				| h :: t  ->
					getLastPrio t
			in loopPrioSymbols
				((self#prioritiesAsHashsetSymbols ())#toList ())
				("PRIO" ^ string_of_int (getLastPrio priorities))
		else if self#isVariable s then ( 
			loopPrioSymbols 
				((self#prioritiesAsHashsetSymbols ())#toList ()) 
				("PRIO" ^ 
					string_of_int(
						snd (
							rulesWithPrio#getItem (
								rulesWithPrio#getIndex (
									(new rule)#newRule s [""], 0
								)
							)
						)
					)
				)
		)
		else
			failwith "grammar - Error"
			
	method isEndingSymbol (variable:string) (terminal:string) =
		(self#getEndingSymbols variable)#contains terminal
	
	method toString () =
		"Rules:\n"     ^ rules#toString () ^
		"\nStart Symbol:\n{ " ^ startSymbol ^ " }" ^
		"\nPriorities:\n" ^ (self#prioritiesAsHashsetSymbols ())#toString () ^
		"\nVariables:\n"  ^ variables#toString () ^
		"\nTerminals:\n"  ^ terminals#toString () ^ "\n"
end;;