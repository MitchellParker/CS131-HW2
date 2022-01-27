
type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal


type awksub_nonterminals =
  | Expr | Term | Lvalue | Incrop | Binop | Num

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

let match_empty lz_accept frag = Lazy.force lz_accept frag
let match_nothing lz_accept frag = None

let append_matchers matcher1 lz_matcher2 lz_accept =
  matcher1  (lazy ((Lazy.force lz_matcher2)  lz_accept))

let make_appended_matchers make_a_matcher (ls: (awksub_nonterminals, string) symbol list ) =
  let rec mams = function
    | [] -> match_empty
    | head::tail -> append_matchers (make_a_matcher head) (lazy (mams tail))
  in mams ls

let rec make_or_matcher make_a_matcher = function
  | [] -> match_nothing
  | (head : (awksub_nonterminals, string) symbol list)::tail ->
    let head_matcher = make_a_matcher head
    and tail_matcher = make_or_matcher make_a_matcher tail
    in fun lz_accept frag ->
      let ormatch = head_matcher lz_accept frag
      in match ormatch with
        | None -> tail_matcher lz_accept frag
        | _ -> ormatch

let match_t (t : string) lz_accept = function
  | [] -> None
  | hd::tl -> if t = hd then Lazy.force lz_accept tl else None


let make_match_nt prod =
  let rec mnt (nt : awksub_nonterminals) =
    let alt = prod nt in
    if alt = [] then match_empty else
    let match_alt =
      let match_symbol = function
        | T t -> match_t t
        | N n -> mnt n
      in make_or_matcher (make_appended_matchers match_symbol)
    in match_alt alt
  in mnt

let make_matcher (start, prod) accept =
  make_match_nt prod start (lazy accept)

let make_parser gram frag = None