type exp =
  | Ide  of string | Int   of int | Char  of char
  | True           | False        | Empty

  | Not  of exp
  | Head of exp | Tail of exp
  | Fst  of exp | Snd  of exp

  | Sum  of exp * exp | Diff of exp * exp | Times of exp * exp
  | And  of exp * exp | Or   of exp * exp
  | Eq   of exp * exp | Less of exp * exp
  | Cons of exp * exp
  | Pair of exp * exp

  | Ifthenelse of exp * exp * exp
  | Appl       of exp * exp
  
  | Let of exp * exp * exp
  | Fun of exp * exp
  | Rec of exp * exp;;

type token =
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

let token_prio_list =
  (TRUE, 7) :: (FALSE, 7) :: (EMPTY, 7) ::
  (PLUS, 5) :: (MINUS, 5) :: (TIMES, 6) ::
  (AND,  4) :: (OR,    4) :: (NOT,   4) ::
  (EQ,   3) :: (LESS,  3) ::
  (CONS, 2) :: (HEAD,  2) :: (TAIL,  2) ::
  (PAIR, 2) :: (FST,   2) :: (SND,   2) ::
  (IF,   1) :: (THEN,  1) :: (ELSE,  1) ::
  (LET,  1) :: (BE,    1) :: (IN,    1) ::
  (FUN,  1) :: (DEF,   1) ::
  (CALL, 1) ::
  (REC,  1) ::

  (LEFT_BRACK,  0) :: (RIGHT_BRACK, 0) ::

  (END, -1) ::

  [];;

let rec pop_del (i:int) (es:exp list) = match es with
  | []     -> if i <= 0 then [] else 
              failwith "pop_del - Trying to pop from an empty stack."
  | h :: t -> if i <= 0 then es else pop_del (i-1) t;;

let rec pop_get (i:int) (es:exp list) = match es with
  | []      -> failwith "pop_get - Trying to pop from an empty stack."      
  | h :: t  -> if i <= 1 then h else pop_get (i-1) t;;

let create (token:token) (es:exp list) = match token with
  | IDE(s)  -> Ide  (s)
  | INT(i)  -> Int  (i)
  | CHAR(c) -> Char (c)
  | TRUE    -> True
  | FALSE   -> False
  | EMPTY   -> Empty

  | NOT  -> Not  (pop_get 1 es)
  | HEAD -> Head (pop_get 1 es)
  | TAIL -> Tail (pop_get 1 es)
  | FST  -> Fst  (pop_get 1 es)
  | SND  -> Snd  (pop_get 1 es)

  | PLUS  -> Sum  (pop_get 2 es, pop_get 1 es)
  | MINUS -> Diff (pop_get 2 es, pop_get 1 es)
  | TIMES -> Times(pop_get 2 es, pop_get 1 es)
  | AND   -> And  (pop_get 2 es, pop_get 1 es)
  | OR    -> Or   (pop_get 2 es, pop_get 1 es)
  | EQ    -> Eq   (pop_get 2 es, pop_get 1 es)
  | LESS  -> Less (pop_get 2 es, pop_get 1 es)
  | CONS  -> Cons (pop_get 2 es, pop_get 1 es)
  | PAIR  -> Pair (pop_get 2 es, pop_get 1 es)
  | DEF   -> Fun  (pop_get 2 es, pop_get 1 es)
  | REC   -> Rec  (pop_get 2 es, pop_get 1 es)
  | CALL  -> Appl (pop_get 2 es, pop_get 1 es)

  | ELSE -> Ifthenelse (pop_get 3 es, pop_get 2 es, pop_get 1 es)
  | IN   -> Let        (pop_get 3 es, pop_get 2 es, pop_get 1 es)

  | _ -> failwith "Create error";;

let rec priority (token:token) (tpl:(token*int) list) = match tpl with
  | (t, p) :: tail -> if t = token then p else priority token tail
  |  _             -> failwith ("Priority error");;

let prio (token:token) = priority token token_prio_list;;

let resolve (t:token) (es:exp list) = match t with
  | NOT 
  | HEAD | TAIL 
  | FST  | SND ->
      (create t es) :: (pop_del 1 es)

  | PLUS | MINUS | TIMES
  | AND  | OR
  | EQ   | LESS
  | CONS
  | PAIR
  | DEF
  | REC
  | CALL ->
      (create t es) :: (pop_del 2 es)

  | ELSE 
  | IN ->
      (create t es) :: (pop_del 3 es)

  | _ -> failwith "Resolve error";;

let rec solve (token:token) (ts:token list) (es:exp list) = 
  match token, ts with
    | RIGHT_BRACK, []     ->
        failwith "Missing LEFT_BRACK"
    | RIGHT_BRACK, h :: t ->
        if h != LEFT_BRACK (*(prio h) > (prio token)*) then
           solve token t (resolve h es)
        else
           t, es

    | THEN, []     ->
        failwith "Missing IF"
    | THEN, h :: t -> 
        if h != IF && (prio h) >= (prio token) then
           solve token t (resolve h es)
        else
           t, es

    | ELSE, []     ->
        failwith "Missing THEN"
    | ELSE, h :: t -> 
        if h != THEN && (prio h) >= (prio token) then
           solve token t (resolve h es)
        else
           t, es

    | DEF, []     ->
        failwith "Missing FUN"
    | DEF, h :: t ->
        if h != FUN then
          failwith "Missing FUN"
        else
          (match es with
             | Ide(_) :: _ ->
                 t, es
             | []
             | _ :: _      ->
                 failwith "Missing IDE()")

    | BE, []     ->
        failwith "Missing LET"
    | BE, h :: t -> 
        if h != LET && (prio h) >= (prio token) then
           solve token t (resolve h es)
        else
           t, es

    | IN, []     ->
        failwith "Missing BE"
    | IN, h :: t -> 
        if h != BE && (prio h) >= (prio token) then
           solve token t (resolve h es)
        else
           t, es

    | END, []     ->
        ts, es
    | END, h :: t ->
        (match h with
           | LEFT_BRACK -> failwith "Missing RIGHT_BRACK"
           | IF         -> failwith "Missing THEN and ELSE"
           | THEN       -> failwith "Missing ELSE"
           | FUN        -> failwith "Missing DEF"
           | LET        -> failwith "Missing BE"
           | BE         -> failwith "Missing IN"
           | _          -> solve token t (resolve h es))

    | _, [] ->
        ts, es
    | _, h :: t ->
        if (prio h) >= (prio token) then
           solve token t (resolve h es)
        else
          ts, es;;

let rec parse (tl:token list) (ts:token list) (es:exp list) = match tl with
  | h :: t -> (match h with
      | IDE(_) | INT(_) | CHAR(_)
      | TRUE   | FALSE  | EMPTY -> 
          parse t ts ((create h es) :: es) 

      | PLUS | MINUS | TIMES
      | AND  | OR    | NOT
      | EQ   | LESS
      | CONS | HEAD  | TAIL
      | PAIR | FST   | SND
      | THEN | ELSE
      | DEF
      | BE | IN ->
          let stacks = solve h ts es in
          parse t (h :: (fst stacks)) (snd stacks)

      | IF
      | FUN
      | REC
      | CALL
      | LET
      | LEFT_BRACK ->
          parse t (h :: ts) es

      | RIGHT_BRACK ->
          let stacks = solve h ts es in
          parse t (fst stacks) (snd stacks)

      | END -> (solve END ts es)(*, ts, es*))
  | [] -> (solve END ts es)(*, ts, es*);;
