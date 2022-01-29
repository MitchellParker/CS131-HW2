
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
    | Symb (N nt) -> make_match_nt prod m_any nt
    | Symb (T t) -> match_t t
    | RHS r -> make_match_rhs m_any r
    | Alt a -> make_match_alt m_any a
  in m_any

let make_matcher (start, prod) =
  make_match_any prod (Symb (N start))



let parse_t t = 
  Leaf t

let parse_nt prod matcher parser nt accept frag =
  let alt_list = prod nt in
  let rec fst_valid_rhs = function
    | [] -> []
    | hd::tl ->
      match matcher (RHS hd) accept frag with
        | None -> fst_valid_rhs tl
        | Some success -> hd
  in
  let rec branches rhs fr =
    match rhs with
    | [] -> []
    | hd::tl -> 
      let tail_matcher = matcher (RHS tl) accept in
      let matched = matcher (Symb hd) tail_matcher fr in
      let parsed = parser hd tail_matcher fr in
      match matched, parsed with
        | None, _ -> assert false
        | _, None -> assert false
        | Some suffix, Some tree -> 
          tree::(branches tl suffix)
  in
  Node (nt, branches (fst_valid_rhs alt_list) frag)

let make_parse_symbol prod = 
  let matcher = make_match_any prod in
  let rec p_symb = fun symbol accept frag ->
    match matcher (Symb symbol) accept frag with
      | None -> None
      | Some suffix -> Some (
        match symbol with
          | N nt -> parse_nt prod matcher p_symb nt accept suffix
          | T t -> parse_t t
      )
  in p_symb

let make_parser (start, prod) =
  let accept_empty_suffix = function _::_ -> None | [] -> Some [] in
  make_parse_symbol prod (N start) accept_empty_suffix