#use "hw2.ml";;
#use "hw2sample.ml";;
#use "hw2test.ml";;

let rec fst_valid_rhs_temp matcher accept frag = function
    | [] -> []
    | hd::tl ->
      match matcher (RHS hd) accept frag with
        | None -> fst_valid_rhs_temp matcher accept frag tl
        | Some success -> hd

let rec branches_temp matcher parser accept rhs fr =
  match rhs with
  | [] -> []
  | hd::tl -> 
    let tail_matcher = matcher (RHS tl) accept in
    let matched = matcher (Symb hd) tail_matcher fr in
    let parsed = parser hd tail_matcher fr in
    match matched, parsed with
      | None, _ -> print_string "no match\n"; assert false
      | _, None -> print_string "no parse\n"; assert false
      | Some suffix, Some tree -> 
    tree::(branches_temp matcher parser accept tl suffix)

let accept = accept_empty_suffix
let matcher_tmp = make_match_any (snd awkish_grammar)
let parser_tmp = make_parse_symbol (snd awkish_grammar)
let parser = fun x -> parser_tmp x accept
let matcher = make_matcher awkish_grammar accept
let fst_valid_rhs = fst_valid_rhs_temp matcher_tmp accept
let branches = branches_temp matcher_tmp parser_tmp accept


let frag = ["$"; "7"]

let tmp_test = parser (N Expr) small_awk_frag
let tmp_test_2 = matcher small_awk_frag
let tmp_test_3 = parser (N Lvalue) frag
let tmp_test_4 = fst_valid_rhs frag [ [N Num];
          [N Lvalue];
          [N Incrop; N Lvalue];
          [N Lvalue; N Incrop];
          [T"("; N Expr; T")"] ]
let tmp_test_5 = branches [ T "$"; T "7" ] frag