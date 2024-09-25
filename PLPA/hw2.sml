(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(*******************************************************************************************************************************)
(********************************************************** PROBLEM 1 **********************************************************)
(*******************************************************************************************************************************)

(* string, string list -> option string list *)
fun all_except_option (queryStr, xs) = 
   case xs of
      [] => NONE
   |  head::neck => if same_string(head, queryStr)
                    then SOME neck
                    else
                        case all_except_option(queryStr, neck) of
                           NONE => NONE
                        |  SOME returned_neck => SOME (head::returned_neck)


(* string list list, string -> string list *)
fun get_substitutions1(substLst, queryStr) =
   case substLst of
      [] => []
   |  head::neck =>  
         case all_except_option(queryStr, head) of
         NONE => get_substitutions1(neck, queryStr)
      |  SOME found_matches => found_matches @ get_substitutions1(neck, queryStr)


(* string list list, string -> string list *)
fun get_substitutions2(substLst, queryStr) =
   let fun aux(substLst, acc) = 
      case substLst of
         [] => acc
      |  head::neck => (case all_except_option(queryStr, head) of
            NONE => aux(neck, acc)
         |  SOME found_matches => aux(neck, acc@found_matches))
   in aux(substLst, [])
   end


(* full_name is type {string, string, string} *)
(* string list list, full_name -> full_name list *)
fun similar_names(substLst, {first=x, middle=y, last=z}) =
   let fun add_first_name(firstNameList, acc) =
      case firstNameList of
      [] => acc
   |  firstNickname::restNicknames => 
         add_first_name(restNicknames, acc @ [{first=firstNickname, middle=y, last=z}])
   in add_first_name(get_substitutions2(substLst, x), [{first=x, middle=y, last=z}])
   end

(*******************************************************************************************************************************)
(********************************************************** PROBLEM 2 **********************************************************)
(*******************************************************************************************************************************)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(* card -> color *)
fun card_color(suit, rank) =
   case suit of
      Clubs => Black
   |  Diamonds => Red
   |  Hearts => Red
   |  Spades => Black


(* card -> int *)
fun card_value(suit, rank) =
   case rank of
      Ace => 11
   |  King => 10
   |  Queen => 10
   |  Jack => 10
   |  Num n => n


(* card list, card, excep -> card list *)
fun remove_card(cardList, card, excep) =
   let fun read_cards(innerCardList, acc) = 
      case innerCardList of
         [] => raise excep
   |     firstCard::restCards => 
            if firstCard = card
            then acc @ restCards
            else read_cards(restCards, firstCard :: acc)
   in read_cards(cardList, [])
   end


(* card list -> bool *)
fun all_same_color(cardList) =
   case cardList of
      [] => true
   |  x :: [] => true
   |  x :: y :: z => 
         if card_color(x) = card_color (y)
         then all_same_color(y :: z)
         else false


(* card list -> int *)
fun sum_cards(cardList) = 
   let fun count_card(cards, acc) =
      case cards of
         [] => acc
      |  firstCard :: restCards => 
            count_card(restCards, (acc + card_value(firstCard)))
   in count_card(cardList, 0)
   end


(* card list, int -> int *)
fun score(heldCards, goal) = 
   let val sum = sum_cards(heldCards)
       val prelim = 
         if sum > goal
         then 3 * (sum - goal)
         else goal - sum
   in
      if all_same_color(heldCards)
      then prelim div 2
      else prelim
   end


(* card list, move list, int -> int *)
fun officiate(origCardList, origMoveList, goal) = 
   let fun play_turn(turn) =
      let val (cardList, heldCards, moveList) = turn
      in 
         if sum_cards(heldCards) > goal
         then score(heldCards, goal)
         else
            case turn of
               (_, heldCards, []) => score(heldCards, goal)
            |  ([], heldCards, Draw::_) => score(heldCards, goal)
            |  ([], heldCards, Discard discardedCard::restMoves) => play_turn([], remove_card([], discardedCard, IllegalMove), restMoves)
            |  (topCard::restDeck, heldCards, currMove::restMoves) => (case currMove of
                  Discard discardedCard => play_turn(topCard::restDeck, remove_card(heldCards, discardedCard, IllegalMove), restMoves)
               |  Draw => play_turn(restDeck, topCard::heldCards, restMoves))
      end
   in play_turn(origCardList, [], origMoveList)
   end


(*******************************************************************************************************************************)
(********************************************************** CHALLENGE **********************************************************)
(*******************************************************************************************************************************)


(* helper for score_challenge and officiate_challenge *)
(* card list -> int *)
fun countAces(heldCards) = 
            case heldCards of
               [] => 0
            |  (suit, rank)::restCards => 
                  if rank = Ace
                  then 1 + countAces(restCards)
                  else countAces(restCards)


(* There is probably a better/shorter solution using two accumulators *)
(* card list, int -> int *)
fun score_challenge(heldCards, goal) =
   let   fun scoreProvidingSum(providedSum) =
            let val prelim =
               if providedSum > goal
               then 3 * (providedSum - goal)
               else goal - providedSum
            in if all_same_color(heldCards)
               then prelim div 2
               else prelim
            end
         fun findPossibleScores(sum, numAces, scoresSoFar) =
            if ((numAces >= 1) andalso (sum >= 10))
            then findPossibleScores(sum - 10, numAces - 1, scoreProvidingSum(sum - 10) :: scoresSoFar)
            else scoresSoFar
         fun find_lowest(possibleScores, bestSoFar) =
               case possibleScores of
                  [] => bestSoFar
               |  nextScore::restScores => if nextScore < bestSoFar
                  then find_lowest(restScores, nextScore)
                  else find_lowest(restScores, bestSoFar)
   in find_lowest(findPossibleScores(sum_cards(heldCards), countAces(heldCards), []), score(heldCards, goal))
   end


(* card list, move list, int -> int *)
fun officiate_challenge(origCardList, origMoveList, goal) = 
   let
      fun sumExceeded(heldCards, goal) =
         let val exceededAmt = sum_cards(heldCards) - goal
         in if exceededAmt <= 0
            then false
            else if (sum_cards(heldCards) - countAces(heldCards) * 10) <= goal
               then false
               else true
         end
      fun play_turn(turn) =
      let val (cardList, heldCards, moveList) = turn
      in 
         if sumExceeded(heldCards, goal)
         then score_challenge(heldCards, goal)
         else
            case turn of
               (_, heldCards, []) => score_challenge(heldCards, goal)
            |  ([], heldCards, Draw::_) => score_challenge(heldCards, goal)
            |  ([], heldCards, Discard discardedCard::restMoves) => play_turn([], remove_card([], discardedCard, IllegalMove), restMoves)
            |  (topCard::restDeck, heldCards, currMove::restMoves) => (case currMove of
                  Discard discardedCard => play_turn(topCard::restDeck, remove_card(heldCards, discardedCard, IllegalMove), restMoves)
               |  Draw => play_turn(restDeck, topCard::heldCards, restMoves))
      end
   in play_turn(origCardList, [], origMoveList)
   end


(* move list -> move list *)
fun rev_list_helper(preList) =
   case preList of
      [] => []
   |  head::neck => (rev_list_helper(neck) @ [head])


(* card list, card, int -> card option*)
fun check_winnable(heldCards, topCard, goal, scoreDiff) =
   case heldCards of
      [] => NONE
   |  firstHeld::restHeld =>
         if (scoreDiff + card_value(firstHeld) - card_value(topCard)) = 0
         then SOME firstHeld
         else check_winnable(restHeld, topCard, goal, scoreDiff)


(* card list, int -> move list *)
fun careful_player(cardList, gameGoal) =
   let fun makeMoves(deckList, goal, heldCards, movesSoFar) = 
      let val scoreDiff = goal - sum_cards(heldCards)
      in
         if scoreDiff = 0
         then rev_list_helper(movesSoFar)
         else case deckList of
            [] => rev_list_helper(movesSoFar)
         |  topCard::restCards => 
               if (scoreDiff > 10) orelse ((scoreDiff - card_value(topCard)) >= 0)
               then makeMoves(restCards, goal, topCard::heldCards, Draw::movesSoFar)
               else
                  let val checkedWin = check_winnable(heldCards, topCard, goal, scoreDiff)
                  in case checkedWin of
                     NONE => rev_list_helper(movesSoFar)
                  |  SOME discardableCard => rev_list_helper(Draw :: Discard discardableCard :: movesSoFar)
                  end
      end
   in makeMoves(cardList, gameGoal, [], [])
   end
