
type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

type ('a, 'b) matchable =
  | RHS of ('a, 'b) symbol list
  | Alt of ('a, 'b) symbol list list
  | Symb of ('a, 'b) symbol

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

(** Converts from a hw1 style grammar to a hw2 style one *)
let rec convert_grammar (start, rules) =
  start, match rules with
    | [] -> fun x -> []
    | (nt, rhs)::other -> fun x ->
      let other_rhs = snd (convert_grammar (start, other)) x in
      if x = nt then
        rhs::other_rhs
      else
        other_rhs

(** Returns a list of leaves in a tree in right-to-left order *)
let rec parse_tree_leaves = function
  | Leaf l -> [l]
  | Node (nt, children) -> List.flatten (List.map parse_tree_leaves children)

let match_empty accept frag = accept frag
let match_nothing accept frag = None

(** This part needed some lazy execution to stop having 
    stupid stack overflows. Took me forever to figure out
    how to fix this. Without it, the matcher tries to expand
    all the way to terminal symbols, which isn't possible
    for a grammar with any recursion. If this is not allowed
    then so be it, I will eat the point loss *)
let append_matchers matcher1 lz_matcher2 accept =
  match lz_matcher2 with lazy matcher2 ->
  matcher1 (matcher2 accept)
let make_appended_matchers make_a_matcher ls =
  let rec mams = function
    | [] -> match_empty
    | head::tail -> append_matchers (make_a_matcher head) (lazy (mams tail))
  in mams ls

let rec make_or_matcher make_a_matcher = function
  | [] -> match_nothing
  | head::tail ->
    let head_matcher = make_a_matcher head
    and tail_matcher = make_or_matcher make_a_matcher tail
    in fun accept frag ->
      let ormatch = head_matcher accept frag
      in match ormatch with
        | None -> tail_matcher accept frag
        | _ -> ormatch

let match_t t accept = function
  | [] -> None
  | hd::tl -> if t = hd then accept tl else None

let make_match_rhs match_any rhs =
  let symbol_matcher = fun s -> match_any (Symb s) in
  make_appended_matchers symbol_matcher rhs

let make_match_alt match_any = function
  | [] -> match_empty (** Special case for empty alt list *)
  | alt -> 
    let match_rhs = fun rhs -> match_any (RHS rhs) in
    make_or_matcher match_rhs alt

let make_match_nt prod match_any nt =
  match_any (Alt (prod nt))

let make_match_any prod = 
  let rec m_any = function
    | RHS r -> make_match_rhs m_any r
    | Alt a -> make_match_alt m_any a
    | Symb (N nt) -> make_match_nt prod m_any nt
    | Symb (T t) -> match_t t
  in m_any

let make_matcher (start, prod) =
  make_match_any prod (Symb (N start))



let parse_t t accept (frag, tree) =
  match match_t t accept frag with
    | Some suffix -> Some (Leaf t)
    | _ -> None

let make_parse_any prod = 
  let rec m_any = function
    | RHS r -> make_match_rhs m_any r
    | Alt a -> make_match_alt m_any a
    | Symb (N nt) -> make_match_nt prod m_any nt
    | Symb (T t) -> parse_t t
  in m_any

(* let append_parsers parser1 parser2 accept frag =
  parser1 (parser2 accept) frag

let rec parse_rhs parse_symbol rhs =
  match rhs with
    | [] -> match_empty
    | s::tl ->
      let head_parser = parse_symbol s
      and tail_parser = parse_rhs parse_symbol tl
      in fun accept frag ->
        let andparse = head_parser accept frag in
        match andparse with
          | None -> None
          | Some suffix, subtree -> 
            subtree::(tail_parser accept suffix)

and parse_alt alt accept frag =
  match alt with
    | [] -> match_nothing accept frag
    | rhs::tl ->
      match parse_rhs 

and parse_nt prod nt accept frag =
  let match_nt = make_match_nt prod in
  match match_nt nt accept frag with
    | None -> None
    | Some suffix -> fun nnext -> 
      match next accept frag nnext with
        | None -> None
        | Some ans  *)


let make_parser (start, prod) frag = None
  (* let accept_empty = function
    | [] -> Some []
    | _ -> None
  in parse_nt prod start accept_empty *)