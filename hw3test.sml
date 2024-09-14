use "hw3.sml";

(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

(* val test_filtercaps1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test_filtercaps2 = only_capitals ["a","b","c"] = []
val test_filtercaps3 = only_capitals [] = []
val test_filtercaps4 = only_capitals ["A","b","C"] = ["A", "C"]
val test_filtercaps5 = only_capitals ["Ape","Badger","c"] = ["Ape","Badger"] *)


(* val test_longstring1 = longest_string1 ["A","bc","C"] = "bc"
val test_longstring2 = longest_string1 [] = ""
val test_longstring3 = longest_string1 ["A","bc","wz"] = "bc"
val test_longstring4 = longest_string1 ["Algae","bota","Cor"] = "Algae"
val test_longstring5 = longest_string1 ["bota","Algae","Cor"] = "Algae"
val test_longstring6 = longest_string1 ["bota","Cor","Algae"] = "Algae" *)

(* val test_longstring2_1 = longest_string2 ["A","bc","C"] = "bc"
val test_longstring2_2 = longest_string2 [] = ""
val test_longstring2_3 = longest_string2 ["A","bc","wz"] = "wz"
val test_longstring2_4 = longest_string2 ["Algae","bota","Cor"] = "Algae"
val test_longstring2_5 = longest_string2 ["bota","Algae","Cor"] = "Algae"
val test_longstring2_6 = longest_string2 ["bota","Cor","Algae"] = "Algae" *)

(* val test_longstring3_1 = longest_string3 ["A","bc","C"] = "bc"
val test_longstring3_2 = longest_string3 [] = ""
val test_longstring3_3 = longest_string3 ["A","bc","wz"] = "bc"
val test_longstring3_4 = longest_string3 ["Algae","bota","Cor"] = "Algae"
val test_longstring3_5 = longest_string3 ["bota","Algae","Cor"] = "Algae"
val test_longstring3_6 = longest_string3 ["bota","Cor","Algae"] = "Algae"

val test_longstring4_1 = longest_string4 ["A","bc","C"] = "bc"
val test_longstring4_2 = longest_string4 [] = ""
val test_longstring4_3 = longest_string4 ["A","bc","wz"] = "wz"
val test_longstring4_4 = longest_string4 ["Algae","bota","Cor"] = "Algae"
val test_longstring4_5 = longest_string4 ["bota","Algae","Cor"] = "Algae"
val test_longstring4_6 = longest_string4 ["bota","Cor","Algae"] = "Algae" *)


(* val test_longcap1 = longest_capitalized ["A","bc","C"] = "C"
val test_longcap2 = longest_capitalized [] = ""
val test_longcap3 = longest_capitalized ["A","Cbc","wZ"] = "Cbc"
val test_longcap4 = longest_capitalized ["Algae","bota","Cor"] = "Algae"
val test_longcap5 = longest_capitalized ["bota","Algae","Cor"] = "Algae"
val test_longcap6 = longest_capitalized ["bota","Cor","Algae"] = "Algae"
val test_longcap7 = longest_capitalized ["Aba","Cbc","wZ"] = "Cbc" *)

(* val test_revstring1 = rev_string "abc" = "cba" *)

(* val test_firstans1 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val test_firstans3 = first_answer (fn x => if x > 3 then SOME x else NONE) [6,2,3,4,5] = 6
val test_firstans4 = first_answer (fn x => if x > 3 then SOME x else NONE) [3,2,3,4,6] = 4 *)
(* val test_firstans2 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,2,~19] *)

(* val test_allans1 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test_allans2 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [1,3,4,5,6,7] = NONE
val test_allans3 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,1,5,6,7] = NONE
val test_allans4 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [1,1,1,1,1,1] = SOME [1,1,1,1,1,1]
val test_allans5 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [] = SOME [] *)

(* val test_countwild1 = count_wildcards Wildcard = 1
val test_countwild2 = count_wildcards (TupleP [Wildcard,Wildcard,Wildcard]) = 3
val test_countwild3 = count_wildcards (TupleP [Wildcard,ConstP 17,Wildcard]) = 2
val test_countwild4 = count_wildcards (ConstP 5) = 0 *)


(* val test_countwildvar1 = count_wild_and_variable_lengths (Variable("a")) = 1
val test_countwildvar2 = count_wild_and_variable_lengths Wildcard = 1
val test_countwildvar3 = count_wild_and_variable_lengths (TupleP [Variable("a"), Wildcard, Variable("abc")]) = 5 *)


(* val test_coutsomevar1 = count_some_var ("x", Variable("x")) = 1
val test_coutsomevar2 = count_some_var ("x", Variable("y")) = 0
val test_coutsomevar3 = count_some_var ("x", TupleP [Variable("x"), Variable("x"), Wildcard, Variable("x")]) = 3 *)

(* val test_checkpat_allstrings1 = check_pat_allstrings (Variable("x")) = ["x"]
val test_checkpat_allstrings2 = check_pat_allstrings (TupleP [Variable("x"), Variable("y"), Variable("xy")]) = ["x", "y", "xy"]
val test_checkpat_allstrings3 = check_pat_allstrings (TupleP [Variable("x"), Variable("y"), Variable("x")]) = ["x", "y", "x"]
val test_checkpat_allstrings4 = check_pat_allstrings (TupleP [Variable("x"), Wildcard, Variable("xy")]) = ["x", "xy"]
val test_checkpat_allstrings5 = check_pat_allstrings (TupleP [Variable("x"), Variable("y"), TupleP [Variable("z"), 
    Variable("w"), Wildcard, Variable("g")], Variable("x")]) = ["x", "y", "z", "w", "g", "x"] *)

(* val test_norepeats1 = no_repeats(["x"]) = true
val test_norepeats2 = no_repeats ["x", "y", "xy"] = true
val test_norepeats3 = no_repeats ["x", "y", "x"] = false
val test_norepeats4 = no_repeats ["x", "xy"] = true
val test_norepeats5 = no_repeats ["x", "y", "z", "w", "g", "x"] = false *)


(* val test_checkpat1 = check_pat (Variable("x")) = true
val test_checkpat2 = check_pat (TupleP [Variable("x"), Variable("y"), Variable("xy")]) = true
val test_checkpat3 = check_pat (TupleP [Variable("x"), Variable("y"), Variable("x")]) = false *)

(* 
val test11 = match (Const(1), UnitP) = NONE

val test12 = first_match Unit [UnitP] = SOME [] *)


val failedTest1 = check_pat (ConstructorP ("hi",TupleP[Variable "x",Variable "x"])) = false
val failedTest2 = check_pat (ConstructorP ("hi",TupleP[Variable "x",ConstructorP ("yo",TupleP[Variable "x",UnitP])])) = false
