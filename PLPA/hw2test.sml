use "hw2.sml";
(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

(* val test_allexc_1 = all_except_option ("string", ["string"]) = SOME []
val test_allexc_2 = all_except_option ("string", []) = NONE
val test_allexc_3 = all_except_option ("string", ["otherString"]) = NONE
val test_allexc_4 = all_except_option ("string", ["str1","string","str3"]) = SOME ["str1", "str3"]


val test_getsubst1_1 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test_getsubst1_2 = get_substitutions1 ([], "foo") = []
val test_getsubst1_3 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test_getsubst1_4 = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff") =  ["Jeffrey","Geoff","Jeffrey"]
val test_getsubst1_5 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") =  ["Fredrick","Freddie","F"]
val test_getsubst1_6 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Jeff") =  []


val test_getsubst2_1 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val test_getsubst2_2 = get_substitutions1 ([], "foo") = []
val test_getsubst2_3 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test_getsubst2_4 = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff") =  ["Jeffrey","Geoff","Jeffrey"]
val test_getsubst2_5 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") =  ["Fredrick","Freddie","F"]
val test_getsubst2_6 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Jeff") =  []

val test_similarnames_1 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]
val test_similarnames_2 = similar_names ([], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}]
val test_similarnames_3 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Elizabeth", middle="The", last="Queen"}) =
	    [{first="Elizabeth", last="Queen", middle="The"}, {first="Betty", last="Queen", middle="The"}]
val test_similarnames_4 = similar_names ([["Frad","Fredrick"],["Elizabeth","Betty"],["Freddie","Ford","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}]

val test_cardcolor_1 = card_color (Clubs, Num 2) = Black
val test_cardcolor_2 = card_color (Diamonds, Num 2) = Red
val test_cardcolor_3 = card_color (Clubs, Ace) = Black

val test_cardval_1 = card_value (Clubs, Num 2) = 2
val test_cardval_2 = card_value (Diamonds, Num 9) = 9
val test_cardval_3 = card_value (Clubs, Ace) = 11
val test_cardval_4 = card_value (Clubs, Queen) = 10

val test_remcard_1 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val test_remcard_2 = remove_card ([], (Hearts, Ace), IllegalMove)
    handle IllegalMove => []
val test_remcard_3 = remove_card ([(Hearts, Ace), (Clubs, Num 7)], (Diamonds, Num 8), IllegalMove)
    handle IllegalMove => []
val test_remcard_4 = remove_card ([(Clubs, Num 7), (Hearts, Ace), (Diamonds, Num 8), (Hearts, Ace)], (Hearts, Ace), IllegalMove) = 
        [(Clubs, Num 7), (Diamonds, Num 8), (Hearts, Ace)]
val test_remcard_5 = remove_card ([(Clubs, Num 7), (Hearts, Ace), (Diamonds, Num 8), (Hearts, Ace)], (Diamonds, Num 8), IllegalMove) = 
        [(Hearts, Ace), (Clubs, Num 7), (Hearts, Ace)]


val test_all_same_col_1 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val test_all_same_col_2 = all_same_color [(Hearts, Ace), (Clubs, Ace)] = false
val test_all_same_col_3 = all_same_color [(Hearts, Ace), (Diamonds, Ace), (Hearts, Num 5), (Hearts, Num 8)] = true
val test_all_same_col_4 = all_same_color [(Hearts, Ace), (Hearts, Ace), (Clubs, Num 4), (Hearts, Num 5)] = false
val test_all_same_col_5 = all_same_color [(Diamonds, Jack)] = true
val test_all_same_col_6 = all_same_color [] = true


val test_sumcard_1 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4
val test_sumcard_2 = sum_cards [] = 0
val test_sumcard_3 = sum_cards [(Spades, Num 9),(Hearts, Ace)] = 20
val test_sumcard_4 = sum_cards [(Clubs, Num 1),(Clubs, Num 8)] = 9
val test_sumcard_5 = sum_cards [(Clubs, King),(Clubs, King), (Clubs, Ace)] = 31


val test_score1 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val test_score2 = score ([(Hearts, Num 9),(Clubs, Num 8)],10) = 21
val test_score3 = score ([(Hearts, Num 2),(Hearts, Num 4)],10) = 2
val test_score4 = score ([(Spades, Num 9),(Clubs, Num 8)],10) = 10


val test_officiate1 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6
val test_officiate2 = (officiate ([(Hearts, Num 2),(Clubs, Num 4), (Spades, King)],[Draw, Draw, Discard (Hearts, Num 2), Draw], 15) = 0)
val test_officiate3 = officiate ([(Hearts, Num 2),(Clubs, Num 4), (Diamonds, King), (Hearts, Num 9), (Diamonds, Num 6)],
    [Draw, Draw, Discard(Hearts,Num 2), Draw], 15) = 1
val test_officiate4 = officiate ([(Hearts, Ace),(Clubs, Num 3), (Spades, King), (Hearts, Num 9)],[Draw, Draw, Draw], 15) = 27
val test_officiate5 = officiate ([(Hearts, Ace),(Clubs, Num 4), (Spades, King), (Hearts, Num 9)],[Draw, Draw], 15) = 0
val test_officiate6 = ((officiate ([(Hearts, Num 2),(Clubs, Num 4)], [Draw,Draw,Discard(Hearts, Num 2),Draw], 15); false)
    handle IllegalMove => true)
val test_officiate7 = ((officiate ([(Hearts, Num 2),(Clubs, Num 4), (Spades, King), (Hearts, Num 9), (Diamonds, Num 6)],
    [Draw, Draw, Discard (Hearts, Num 3), Draw], 15); false)
    handle IllegalMove => true)
val test_officiate8 = (officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)], [Draw,Draw,Draw,Draw,Draw], 42) = 3)
val test_officiate9 = ((officiate([(Clubs,Jack),(Spades,Num(8))], [Draw,Discard(Hearts,Jack)], 42); false)
              handle IllegalMove => true) *)


(* val test_score_chall1 = score_challenge ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val test_score_chall2 = score_challenge ([(Hearts, Num 9),(Clubs, Num 8)],10) = 21
val test_score_chall3 = score_challenge ([(Hearts, Num 2),(Hearts, Num 4)],10) = 2
val test_score_chall4 = score_challenge ([(Spades, Num 9),(Clubs, Num 8)],10) = 10
val test_score_chall5 = score_challenge ([(Hearts, Ace),(Clubs, Num 3), (Spades, King)],15) = 1
val test_score_chall6 = score_challenge ([(Hearts, Ace),(Clubs, Ace), (Diamonds, King)],16) = 4
val test_score_chall7 = score_challenge ([(Hearts, Ace),(Hearts, Ace), (Diamonds, King)],16) = 2 *)

(*
val test_officiate_chall1 = officiate_challenge([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6
val test_officiate_chall2 = (officiate_challenge ([(Hearts, Num 2),(Clubs, Num 4), (Spades, King)],[Draw, Draw, Discard (Hearts, Num 2), Draw], 15) = 0)
val test_officiate_chall3 = officiate_challenge ([(Hearts, Num 2),(Clubs, Num 4), (Diamonds, King), (Hearts, Num 9), (Diamonds, Num 6)],
    [Draw, Draw, Discard(Hearts,Num 2), Draw], 15) = 1
val test_officiate_chall4 = officiate_challenge ([(Hearts, Ace),(Clubs, Num 3), (Spades, King), (Hearts, Num 9)],[Draw, Draw, Draw], 15) = 1
val test_officiate_chall5 = officiate_challenge ([(Hearts, Ace),(Clubs, Num 4), (Spades, King), (Hearts, Num 9)],[Draw, Draw], 15) = 0
val test_officiate_chall6 = ((officiate_challenge ([(Hearts, Num 2),(Clubs, Num 4)], [Draw,Draw,Discard(Hearts, Num 2),Draw], 15); false)
    handle IllegalMove => true)
val test_officiate_chall7 = (officiate_challenge ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)], [Draw,Draw,Draw,Draw,Draw], 42) = 3)
val test_officiate_chall8 = ((officiate_challenge ([(Hearts, Num 2),(Clubs, Num 4), (Spades, King), (Hearts, Num 9), (Diamonds, Num 6)],
    [Draw, Draw, Discard (Hearts, Num 3), Draw], 15); false)
    handle IllegalMove => true)
val test_officiate_chall9 = ((officiate_challenge([(Clubs,Jack),(Spades,Num(8))], [Draw,Discard(Hearts,Jack)], 42); false)
              handle IllegalMove => true)
val test_officiate_chall10 = officiate_challenge([(Hearts, Ace),(Clubs, Ace), (Diamonds, King), (Hearts, Num 9)],[Draw, Draw, Draw], 16) = 4
val test_officiate_chall11 = officiate_challenge([(Clubs, Ace),(Clubs, Ace), (Spades, King), (Hearts, Num 9)],[Draw, Draw, Draw], 16) = 2


val test_careful_1 = careful_player([(Spades, Num 10),(Hearts, Ace)], 8) = []
val test_careful_2 = careful_player([(Spades, Num 10),(Hearts, Ace)], 13) = [Draw]
val test_careful_3 = careful_player([(Spades, Num 10),(Hearts, Num 9), (Clubs, Num 6)], 19) = [Draw, Draw]
val test_careful_4 = careful_player([(Spades, Num 6),(Hearts, Num 9), (Clubs, Num 10)], 16) = [Draw, Draw, Discard (Hearts, Num 9), Draw]
val test_careful_5 = careful_player([(Hearts, Num 2),(Clubs, Num 4), (Spades, King)], 15) = [Draw, Draw]
*)
