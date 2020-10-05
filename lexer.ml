#load "str.cma";;

type keyword = 
    Keyword of Str.regexp;;

type token =
  | IDEx          | INTx       | CHARx
  | IDE of string | INT of int | CHAR of char
  | TRUE          | FALSE      | EMPTY

  | PLUS | MINUS | TIMES
  | AND  | OR    | NOT
  | EQ   | LESS
  | CONS | HEAD  | TAIL
  | PAIR | FST   | SND

  | IF   | THEN | ELSE
  | LET  | BE   | IN
  | FUN  | DEF
  | CALL
  | REC 

  | RIGHT_BRACK | LEFT_BRACK

  | END;;

let keyword_token_list =
  (Keyword(Str.regexp "true"),  TRUE)  ::
  (Keyword(Str.regexp "false"), FALSE) ::
  (Keyword(Str.regexp "empty"), EMPTY) ::

  (Keyword(Str.regexp "\\+"), PLUS)  ::
  (Keyword(Str.regexp "-"),   MINUS) ::
  (Keyword(Str.regexp "\\*"), TIMES) ::

  (Keyword(Str.regexp "&&"),  AND) ::
  (Keyword(Str.regexp "||"),  OR)  ::
  (Keyword(Str.regexp "not"), NOT) ::

  (Keyword(Str.regexp "=="), EQ)   ::
  (Keyword(Str.regexp "<"),  LESS) ::

  (Keyword(Str.regexp "::"), CONS) ::
  (Keyword(Str.regexp "hd"), HEAD) ::
  (Keyword(Str.regexp "tl"), TAIL) ::

  (Keyword(Str.regexp ","),   PAIR) ::
  (Keyword(Str.regexp "fst"), FST)  ::
  (Keyword(Str.regexp "snd"), SND)  ::

  (Keyword(Str.regexp "if"),   IF)   ::
  (Keyword(Str.regexp "then"), THEN) ::
  (Keyword(Str.regexp "else"), ELSE) ::

  (Keyword(Str.regexp "let"), LET) ::
  (Keyword(Str.regexp "="),   BE)  ::
  (Keyword(Str.regexp "in"),  IN)  ::

  (Keyword(Str.regexp "fun"), FUN) ::
  (Keyword(Str.regexp "->"),  DEF) ::

  (Keyword(Str.regexp "call"), CALL) ::

  (Keyword(Str.regexp "rec"), REC) ::

  (Keyword(Str.regexp "("), LEFT_BRACK)  ::
  (Keyword(Str.regexp ")"), RIGHT_BRACK) ::

  (Keyword(Str.regexp "\\."), END) ::

  (Keyword(Str.regexp "[a-zA-Z][a-zA-Z0-9_]?"),  IDEx)  ::
  (Keyword(Str.regexp "-?[0-9]+"),               INTx)  ::
  (Keyword(Str.regexp "'[a-zA-Z]'"),             CHARx) ::

  [];;

let read_file (file:string) =
  let chan = (open_in file) in
  let try_read () =
    try Some (input_line chan)
    with eof -> None in
  let rec create_list () = match try_read () with
    | Some s -> s :: create_list ()
    | None   -> close_in chan; [] in
  create_list ();;

let rec split_lines (strL:string list) = match strL with
  | []     -> []
  | h :: t -> (Str.split (Str.regexp "[ |\t|\n|\r]+") h) @ split_lines t;;

let rec matches (s:string) (ktl:(keyword * token) list) = match ktl with
  | (Keyword(regex), t) :: tail ->
      if (Str.string_match regex s 0) then
        (match t with
           | IDEx  -> IDE(s)
           | INTx  -> INT(int_of_string s)
           | CHARx -> CHAR(s.[0])
           | _     -> t)
        else matches s tail
  |  _ -> failwith "Unknown symbol";;

let rec translate (kwlist:string list) = match kwlist with
  | []     -> []
  | h :: t -> matches h keyword_token_list :: translate t;;

let lexer (file:string) = translate (split_lines (read_file file));;
