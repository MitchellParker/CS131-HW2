
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

let match_t t accept = function
  | [] -> None
  | hd::tl -> if t = hd then accept tl else None

let make_matcher (start, prod) =
  let rec match_helper prefix accept frag =
    match prefix with
      | [] -> accept frag
      | (T x)::tl -> match_t x (match_helper tl accept) frag
      | (N x)::tl ->
        let rec expand = function
          | [] -> None
          | alt::alt_tl ->
            let res = match_helper (alt@tl) accept frag in
            if res = None then
              expand alt_tl
            else
              res
        in 
        let alt_list = prod x in
        match alt_list with
          | [] -> match_helper tl accept frag
          | _ -> expand alt_list
  in match_helper [N start]


let make_parser gram frag = None