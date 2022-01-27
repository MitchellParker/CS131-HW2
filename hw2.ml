
type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

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

let append_matchers matcher1 matcher2 accept =
  matcher1 (matcher2 accept)

let make_appended_matchers make_a_matcher ls =
  let rec mams = function
    | [] -> match_empty
    | head::tail -> append_matchers (make_a_matcher head) (mams tail)
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

type awksub_nonterminals =
  | Expr | Term | Lvalue | Incrop | Binop | Num

let rec match_nt prod nt =
  let alt = prod nt in
  if alt = [] then match_empty else
  let match_alt =
    let match_symbol = function
      | T t -> match_t t
      | N n -> match_nt prod n
    in make_or_matcher (make_appended_matchers match_symbol)
  in match_alt alt

let make_matcher (start, prod) =
  match_nt prod start

let make_parser gram frag = None