datatype date = date of int * int * int

exception InvalidArg

(*task 1*)
fun is_older (date(year1,month1,day1) : date, date(year2,month2,day2) : date) =
    if year1 < year2 then true
    else if year1 = year2 andalso day1 < day2 then true
    else 
        if year1 = year2 andalso month1 = month2 andalso day1 < day2 then true
        else false

(*task 2*)
fun number_in_month (dates: (int*int*int) list, month: int) =
    if null dates
	    then 0
	    else
            if #2 (hd dates) = month then 1 + number_in_month(tl dates, month)
            else number_in_month(tl dates, month)

	

(*task 3*)
fun number_in_months (dates: (int*int*int) list, months: int list) =
	if null months orelse null dates
	then 0
	else number_in_months(dates, tl months) + number_in_month(dates, hd months)

(*task 4*)
fun dates_in_month([], _) = []
  | dates_in_month(date(a, b, c)::dates, m) : date list = 
    if b = m then date(a, b, c)::dates_in_month(dates, m)
    else dates_in_month(dates, m)

(*task 5*)
fun dates_in_months(dates, []) = []
  | dates_in_months(dates, m::ms) : date list = 
    dates_in_month(dates, m) @ dates_in_months(dates, ms)

(*task 6*)
fun get_nth (strings: string list, n: int) =
	if n = 1
	then if null strings
		then ""
		else hd strings
	else get_nth(tl strings, n - 1)


(*task 7*)
fun date_to_string(date(year, month, day) : date) : string =
    let val months = ["January", "February",
    "March", "April", "May", 
    "June", "July", "August", 
    "September", "October", "November", 
    "December"]
    in
        get_nth(months, month) ^ ", "^ 
        Int.toString(day) ^ ", " ^ 
        Int.toString(year)
    end

(*task 8*)
fun number_before_reaching_sum (sum, numbers) =
	let
		fun get_sum (nums, elements) = 
			if elements <= 0 orelse null nums
			then 0
			else hd nums + get_sum(tl nums, elements - 1)

		fun count (counter) =
			if get_sum(numbers, counter + 1) < sum
			then count(counter+1)
			else counter
	in
		count(0)
	end

(*task 9*)
fun what_month(day : int) =
    1 + number_before_reaching_sum(day, [31, 28, 31, 
    30, 31, 30,
    31, 31, 30,
    31, 30, 31])

(*task 10*)
fun month_range_util(day1 : int, day2 : int) : int list =
    let
      val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
      val m1 = what_month(day1)
      val m2 = what_month(day2)
    in
      if m1 > m2 then []
      else if m1 = m2 then [m1]
      else [m1, m2]
    end


fun for_loop_util(m1 : int, m2 : int) : int list =
    if m1 > m2 then []
    else m1::for_loop_util(m1+1, m2)


fun month_range(day1 : int, day2 : int) = 
    let
      val range = month_range_util(day1, day2)
    in
      if range = [] then []
      else for_loop_util(List.nth(range, 0), List.nth(range, 1))
    end


(*task 11*)
fun oldest_date([]) = date(0, 0, 0)
  | oldest_date(date(year, month, day)::dates : date list) =
    if is_older(oldest_date(dates), date(year, month, day)) then date(year, month, day)
    else oldest_date(dates)

(*tests for functions*)

(* test1 *)
val test1 = is_older (date(2001,3,3), date(2001,5,5));
(* result = true *)
(* test2 *)
val test2 = number_in_month([(2010,2,28),(2010,1,1),(2012,3,1),(2010,2,1)],2)
(* retult = 2 *)
(* test3 *)
val test3 = number_in_months([(2010,1,10),(2011,2,20),(2012,3,30),(2013,1,2)],[2,1])
(* retult = 3 *)
(* test4 *)
val test4 = dates_in_month ([date(2000, 11, 1), date(2000, 12, 1), date(2000, 11, 2)], 11)
(* result = [date (2000,11,1),date (2000,11,2)] : date list *)
(* test5 *)
val test5 = dates_in_months ([date(2002, 2, 1), date(2003, 12, 1), date(2004, 11, 2)], [2, 1])
(* result = [date (2002,2,1)] : date list *)
(* test6 *)
val test6 = get_nth(["Hello", "Everybody", "My", "Name", "is", "Kostya"], 6)
(* result = "Kostya" : string *)
(* test7 *)
val test7 = date_to_string(date(2022, 2, 28))
(* reuslt = "February, 28, 2022" : string *)
(* test8 *)
val test8 = number_before_reaching_sum(70, [10, 12, 14, 16, 18, 20])
(* result = 4 *)
(* test9 *)
val test9 = what_month(203)
(* result = 7 *)
(* test10 *)
val test10 = month_range(15, 79)
(* result = [1,2,3] : int list *)
(* test11 *)
val test11= oldest_date([date(2010, 4, 1), date(2011, 4, 4), date(2013, 5, 6), date(2012, 1, 2)])
(* result =  date (2013,5,6) : date *)
