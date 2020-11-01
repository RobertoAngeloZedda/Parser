open Rule;;
open Grammar;;
open Parser;;

class parser = object(self)
	val mutable aaa = new hashsetObj

	method create (g:grammar) =
		(* INIZIALIZZO IL PRIMO STATO *)
		let startRule = List.hd (g#getRulesAsList ()) in
		
		let startItem = new item in
		item#setRule (startRule);
		
		start#add startItem
end;;

