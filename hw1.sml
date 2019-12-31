
(*written by Yusra Haider, September 2019
Solution for Homework 1, for the course Programming Languages Part A by Dan Grossman*)


(*Code for Question 1*)
(*tells if date1 < date2. returns false if date1 = date2*)
fun is_older(date1: int*int*int, date2: int*int*int) =
    (*compare years of date1 and date2*)
    if #1 date1 > #1 date2 then false
    else if #1 date1 < #1 date2 then true
    else 
        (*if years are equal, compare months*)
        if #2 date1 > #2 date2 then false
        else if #2 date1 < #2 date2 then true
        else 
	    (*if years and months are equal, compare days. If dates are equal, return false*)
	    if #3 date1 >= #3 date2 then false
	    else true
		     
		     
(*Code for Question 2*)
(*returns the number of dates that lie in given month*)
fun number_in_month(dates: (int*int*int) list, month: int) =
    if null dates then 0
    else
	let val count = number_in_month(tl(dates), month)
	in
	    if #2(hd(dates)) = month then 1 + count
	    else 0 + count
	end

	    
(*Code for Question 3*)
(*return the number of dates that lie in the given months*)
fun number_in_months(dates: (int*int*int) list, months: int list) =
    if null months then 0
    else if null dates then 0
    else
	number_in_month(dates, hd(months)) + number_in_months(dates, tl(months))

							     
(*Code for Question 4*)
(*returns list of dates that lie in given month*)
fun dates_in_month(dates: (int*int*int) list, month: int) =
    if null dates then []
    else
	let val dates_in_month = dates_in_month(tl(dates), month)
	in
	    if #2(hd(dates)) = month then hd(dates)::dates_in_month
	    else dates_in_month
	end

	    
(*Code for question 5*)
(*returns list of dates that lie in given months*)
fun dates_in_months(dates: (int*int*int) list, months: int list) =
    if null months then []
    else if null dates then []
    else
	dates_in_month(dates, hd(months))@dates_in_months(dates, tl(months))
	
	
(*Code for Question 6*)
(*gets the nth elem from given list. Throws Empty exception for invalid n / empty lists*)
fun get_nth(strings: string list, n) =
    if n = 1 then hd(strings)
    else get_nth(tl(strings),n-1)

		
(*Code for Question 7*)
(*converts date of the form (year, month, day) to formatted string of form month day, year
an example would be: (2013,6,1) --> June 1, 2013*)
fun date_to_string(date: (int*int*int)) =
    let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "O    ctober", "November", "November"]
    in get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end


(*Code for Question 8*)
fun number_before_reaching_sum(sum: int, elems: int list) =
    (*helper function. partial_sum is the sum of elems list from 1 till nth index.
    partial_elems is the list from n+1 till end of elems*)
    let fun helper(partial_sum: int, n: int, partial_elems: int list) =
	    if partial_sum + hd(partial_elems) >= sum then n
	    else helper(partial_sum + hd(partial_elems), n+1, tl(partial_elems))
    in
	(*if list is empty return 0*)
	if null elems then 0
	else helper(0,0,elems)
    end


(*Code for Question 9*)
(*Returns the month number to which this day belongs. The code doesn't handle leap years*)
fun what_month(day: int) =
    let val number_of_days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	number_before_reaching_sum(day,number_of_days_in_months) + 1
    end

	
(*Code for Question 10*)
fun month_range(day1: int, day2: int) =
    if day1 > day2 then []
    else
	what_month(day1) :: month_range(day1+1, day2)

				       
(*Code for Question 11*)
(*return oldest date from list of given dates*)
fun oldest(dates:  (int*int*int) list) =
    if null dates then NONE
    else
	let val oldest = oldest(tl(dates))
	in
	    if isSome(oldest) andalso is_older(valOf(oldest), hd(dates)) then oldest
	    else SOME(hd(dates))		
	end    
