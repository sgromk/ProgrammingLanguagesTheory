fun is_older (date1 : int*int*int, date2 : int*int*int) =
	(#1 date1) < (#1 date2) orelse
	((#1 date1) = (#1 date2) andalso (#2 date1) < (#2 date2)) orelse
	((#1 date1) = (#1 date2) andalso (#2 date1) = (#2 date2) 
							 andalso (#3 date1) < (#3 date2))


fun number_in_month (yearlist: (int*int*int) list, mnth : int)  = 
	if null yearlist orelse mnth < 0
	then 0
	else 
		if (#2 (hd yearlist)) = mnth
		then 1 + number_in_month((tl yearlist), mnth)
		else number_in_month((tl yearlist), mnth)

(* (int int int) list, int list -> int *)
fun number_in_months(datelist: (int*int*int) list, mnth_list: int list) = 
	if null mnth_list
	then 0
	else
		number_in_month(datelist, (hd mnth_list)) + 
		number_in_months(datelist, (tl mnth_list))


(* (int int int) list, int  -> int list *)
fun dates_in_month(datelist: (int*int*int) list, mnth: int) =
	if null datelist
	then []
	else
		if (#2 (hd datelist)) = mnth
		then (hd datelist) :: dates_in_month((tl datelist), mnth)
		else dates_in_month((tl datelist), mnth)


(* (int int int) list, int list  -> (int*int*int) list *)
fun dates_in_months(datelist: (int*int*int) list, mnth_list : int list) = 
	if null mnth_list
	then []
	else
		dates_in_month(datelist, (hd mnth_list))@dates_in_months(datelist,(tl mnth_list))


(* string list, int -> string *)
fun get_nth(stringlist: string list, nelement: int) =
	if nelement < 2
	then (hd stringlist)
	else get_nth((tl stringlist), (nelement - 1))


(* int*int*int -> string *)
fun date_to_string(dateInt: (int*int*int)) = 
	let val mnth_list = ["January", "February", "March", "April", "May", "June", 
				 "July", "August", "September", "November", "October", "December"]
	in
		get_nth(mnth_list, (#2 dateInt)) ^ " " ^ Int.toString(#3 dateInt) ^ ", " ^ Int.toString(#1 dateInt)
	end


(* int, int list -> int *)
fun number_before_reaching_sum(sum: int, intList: int list) = 
	if (sum <= (hd intList))
	then 0
	else
		1 + number_before_reaching_sum((sum - (hd intList)), (tl intList))


(* int -> int *)
fun what_month(yearDay: int) = 
	let val mnth_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	in 
		1 + number_before_reaching_sum(yearDay, mnth_days)
	end


(* int, int -> int list *)
fun month_range(day1: int, day2: int) = 
	if day1 > day2
	then []
	else
		what_month(day1) :: month_range(day1 + 1, day2)


(* (int*int*int) list -> (int*int*int) option *)
fun oldest(dateList: (int*int*int) list) = 
	if null dateList
	then NONE
	else
		let 
			fun oldest_nonempty (dateList: (int*int*int) list) =
				if null (tl dateList)
				then hd dateList
				else
					let val tl_ans = oldest_nonempty(tl dateList)
					in
						if is_older(hd dateList, tl_ans)
						then hd dateList
						else tl_ans
					end
		in
			SOME (oldest_nonempty(dateList))
		end


(* int, int list -> bool *)
fun not_in_list(entry: int, intList: int list) = 
	if null intList
	then true
	else
		if entry = (hd intList)
		then false
		else
			not_in_list(entry, tl intList)


(* int list -> int list *)
fun remove_duplicates_helper(entryList: int list) = 
	if null entryList
	then []
	else
		if not_in_list(hd entryList, tl entryList)
		then (hd entryList) :: remove_duplicates_helper(tl entryList)
		else remove_duplicates_helper(tl entryList)


fun number_in_months_challenge (dateList: (int*int*int) list, mnth_list: int list) = 
	let val clean_mnths = remove_duplicates_helper(mnth_list)
	in number_in_months(dateList, clean_mnths)
	end


fun dates_in_months_challenge(dateList: (int*int*int) list, mnth_list: int list) = 
	let val clean_mnths = remove_duplicates_helper(mnth_list)
	in dates_in_months(dateList, clean_mnths)
	end


(* int -> bool *)
fun is_leap_year(year: int) = 
	if (year mod 100) = 0
	then (year mod 400) = 0
	else (year mod 4) = 0


(* int -> int *)
fun get_nth_month(mnth_index: int, mnth_days) =
	if mnth_index = 1
	then (hd mnth_days)
	else get_nth_month(mnth_index - 1, tl mnth_days)


(* int*int*int -> bool *)
fun reasonable_date(date: int*int*int) = 
	let val mnth_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	in
		if (#1 date) > 0 andalso (((#2 date) >= 1) andalso ((#2 date) <= 12))
		then
			if (is_leap_year(#1 date) andalso ((#2 date) = 2))
			then (#3 date) <= 29
			else ((#3 date) <= get_nth_month((#2 date), mnth_days))
		else false
	end
