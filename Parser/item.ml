open Rule;;

class item = object(self)
	val mutable rule = new rule
	val mutable index = ( 0 : int )
	val mutable explored = ( true : bool )
	
	method newItem (r:rule) =
		rule <- r;
		index <- 0;
		explored <- false;
		self
	
	method getRule () =
		rule
	
	method getIndex () =
		index
	
	method getCurrentSymbol () = 
		if explored then
			""
		else
			rule#getRightWithIndex index
	
	method nextSymbol () =
		if index + 1 >= rule#getRightLength () then
			explored <- true
		else
			index <- index + 1
	
	method isExplored () =
		explored
	
	method clone (i:item) =
		rule <- i#getRule ();
		index <- i#getIndex ();
		explored <- i#isExplored ();
		self
	
	method equals (i:item) =
		if index != i#getIndex () then
			false
		else if explored != i#isExplored () then
			false
		else
			rule#equals (i#getRule ())
	
	method toString () =
		rule#toString () ^ 
		if explored then 
			" (Already explored)"
		else
			" (Index: " ^ string_of_int index ^ 
			" { " ^ rule#getRightWithIndex index ^ " } )"
end;;