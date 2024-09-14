(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* string list -> string list *)
fun only_capitals slist =	List.filter (fn x => Char.isUpper(String.sub(x, 0))) slist


(* string list -> string *)
fun longest_string1 slist = foldl(fn (x,y) => if (String.size(x) > String.size(y)) then x else y) "" slist


(* string list -> string *)
fun longest_string2 slist = foldl(fn (x,y) => if (String.size(x) >= String.size(y)) then x else y) "" slist


(* (int, int -> bool) -> string list -> string *)
fun longest_string_helper passedFun slist = foldl(fn (x,y) => if passedFun(x,y) then x else y) "" slist


(* string list -> string *)
val longest_string3 = longest_string_helper (fn (x, y) => String.size(x) > String.size(y))


(* string list -> string *)
val longest_string4 = longest_string_helper (fn (x, y) => String.size(x) >= String.size(y))


(* string list -> string *)
val longest_capitalized = longest_string_helper 
	(fn (x, y) => (Char.isUpper o (fn z => String.sub (z, 0))) x andalso String.size(x) > String.size(y))


(* string -> string *)
val rev_string = String.implode o List.rev o String.explode


(* ('a -> option) -> 'a list -> 'b *)
fun first_answer passedFun aList = 
	case aList of
		[] => raise NoAnswer
	|	head::neck => case passedFun head of
			SOME x => x
		|	NONE => first_answer passedFun neck


(* (’a -> ’b list option) -> ’a list -> ’b list option *)
fun all_answers passedFun aList =
	let fun tryAnswer elems acc =
		case elems of
			[] => SOME acc
		|	head::neck => case passedFun head of
				NONE => NONE
			|	SOME x => tryAnswer neck (acc @ x)
	in tryAnswer aList []
	end


(* pattern -> int *)
fun count_wildcards p = g (fn() => 1) (fn x => 0) p


(* pattern -> int *)
fun count_wild_and_variable_lengths p = g (fn() => 1) (fn x => String.size(x)) p


(* (string, pattern) -> int *)
fun count_some_var (s,p) = g (fn() => 0) (fn z => if z = s then 1 else 0) p


(* pattern -> bool *)
val check_pat = 
	let fun no_repeats slist =
			case slist of
				[] => true
			|	head::neck => not(List.exists (fn x => x = head) neck) andalso no_repeats neck
		fun check_pat_allstrings p =
			let fun acc_strings p acc =
				case p of		
					Variable x => [x]
				|	TupleP ps  => List.foldl (fn (p, a) => a @ (acc_strings p a)) [] ps
				|	ConstructorP (_, k) => acc_strings k acc
				|	_ => []
			in acc_strings p []
			end
	in no_repeats o check_pat_allstrings
	end


(* valu, pattern -> (string * value) list option *)
fun match (va, pat) =
	case (va, pat) of
		(_, Wildcard) => SOME []
	|	(x, Variable s) => SOME [(s, x)]
	|	(Unit, UnitP) => SOME []
	|	(Const y, ConstP x) => if x = y then SOME [] else NONE
	|	(Constructor (s2, v), ConstructorP (s1, p)) => if s1 = s2 then match (v, p) else NONE
	|	(Tuple vlist ,TupleP ps) => if List.length(ps) = List.length(vlist) 
					then all_answers match (ListPair.zip (vlist, ps)) else NONE
	|	_ => NONE


(* valu, pattern list -> (string * valu) list option *)
fun first_match va plist = 
	SOME (first_answer (fn pat => match(va, pat)) plist)
	handle NoAnswer => NONE
