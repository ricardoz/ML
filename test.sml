(* problem 1: is older
take two dates y/m/d
return true if the first comes before the second*)

val dates = [(1,1,1),(1,2,2),(3,6,12),(5,6,9)]
val ms = [6,1]
val monthNames = ["January","February","March","April","May","June",
"July","August","September","October", "November", "December"]
val numbers = [1,2,3,4,5,6,7,8,9,10,11,12]
val dim =[31,28,31,30,31,30,31,31,30,31,30,31]

fun is_older(date1: int*int*int, date2: int*int*int) =
    if (#1 date1) < (#1  date2)
    then true
    else if (#1 date1) = (#1 date2) andalso (#2 date1) < (#2 date2)
    then true
    else if (#1 date1) = (#1 date2) andalso (#2 date1) = (#2 date2) andalso (#3 date1) < (#3 date2)
    then true
    else false

fun number_in_month(xs : (int*int*int) list, month : int) =
	if null xs
	then 0
	else if (#2 (hd(xs))) = month
	then 1 + number_in_month(tl xs, month)
	else 0 + number_in_month(tl xs, month)


fun number_in_months(xs : (int*int*int) list, months : int list) =
	if null months
	then 0
	else number_in_month(xs, hd(months)) + number_in_months(xs, tl(months))

fun dates_in_month(xs : (int*int*int) list, month : int) =
	if null xs
	then []
	else if (#2 (hd(xs))) = month
	then (hd xs) :: dates_in_month(tl xs, month)
	else dates_in_month(tl xs, month)

fun dates_in_months(xs : (int*int*int) list, months : int list) =
	if null months
	then []
	else dates_in_month(xs, hd(months)) @ dates_in_months(xs, tl(months))

fun get_nth(xs : string  list, n : int) =
	if n <= 1
	then hd xs
	else get_nth(tl xs, n - 1)

fun date_to_string(date : int*int*int) =
	get_nth(monthNames, (#2 date)) ^ " " ^ Int.toString((#3 date))^  ", " ^ 
Int.toString((#1 date))

fun number_before_reaching_sum(sum : int, numbers: int list) =
	if (hd numbers) >=  sum
	then 0
	else 1 + number_before_reaching_sum(sum - (hd numbers), tl(numbers)) 


fun what_month(day_of_year : int) =
	number_before_reaching_sum(day_of_year, dim) + 1


fun month_range(date1: int, date2: int) =
	if (date2) > (date1)
	then []
	else (date1) :: month_range((date1), date2 - 1)


fun oldest(dates : (int*int*int) list) =
	if null dates
	then NONE
	else let
		fun ne_oldest(dates : (int*int*int) list) =
			if null (tl dates)
		then hd dates
		else  let val ans = ne_oldest(tl dates)
			in
				if is_older(hd dates, ans)
				then hd dates
				else ans
			end

		in
			SOME (ne_oldest dates)
		end

fun contains(xs  : int list, obj : int) =
	if null xs
	then false
	else if hd xs = obj 
	then true 
	else contains(tl xs, obj)


fun remove_duplicates(xs  : int list) = 
	if null xs
	then []
	else if contains(tl xs, hd xs)
	then remove_duplicates(tl xs)
	else hd xs :: remove_duplicates(tl xs)



fun number_in_months_challenge(xs : (int*int*int) list, months : int list) =
    number_in_months(xs, remove_duplicates(months))

fun dates_in_months_challenge(xs : (int*int*int) list, months : int list) =
    dates_in_months(xs, remove_duplicates(months))



(*fun reasonable_date(date : int*int*int) =*) 
    



							 

				
