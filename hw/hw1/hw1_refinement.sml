type date = int * int * int;
fun is_older (date1:date, date2:date):bool = 
    if ((#1 date1)*366 + (#2 date1)*31 + (#3 date1) < (#1 date2)*366 + (#2
    date2)*31 + (#3 date2)) then true else false;  

fun number_in_month (listOfdates:date list, month:int):int =
  (case (listOfdates)
    of nil => 0
     | _ => if (#2 (hd listOfdates) = month) then 1 + number_in_month(tl
     listOfdates, month) 
            else number_in_month(tl listOfdates,month));

fun number_in_months (listOfdates:date list, listOfMonths:int list):int = 
  (case (listOfMonths)
    of nil => 0
     | _ => number_in_month(listOfdates, hd listOfMonths) +
     number_in_months(listOfdates, tl listOfMonths));
     
fun dates_in_month (listOfdates:date list, month:int):date list =
  (case (listOfdates)
    of nil => nil
      | _ => if (#2 (hd listOfdates) = month) then (hd listOfdates) :: dates_in_month(tl
      listOfdates, month) 
             else dates_in_month(tl listOfdates, month));

fun dates_in_months (listOfdates:date list, listOfMonths:int list):date list =
  (case (listOfMonths)
    of nil => nil
     | _ => dates_in_month(listOfdates, hd listOfMonths) @
     dates_in_months(listOfdates, tl listOfMonths));

fun get_nth (listOfStrings:string list, n:int):string =
  (case n
    of 1 => hd listOfStrings
     | _ => get_nth(tl listOfStrings, n-1));

fun date_to_string (date0:date):string = 
  get_nth(["January","February","March","April","May","June","July","August","September","October","November","December"], 
  #2 date0) ^ " " ^ Int.toString(#3 date0) ^ "," ^ " " ^ Int.toString(#1 date0);

fun number_before_reaching_sum (sum:int, listOfInts:int list):int =
  if sum - (hd listOfInts) <= 0 then 0 
    else number_before_reaching_sum(sum - (hd listOfInts), tl listOfInts) + 1;

fun what_month (day:int):int = 
    let
      val daysOfMonth:int list = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
      number_before_reaching_sum(day, daysOfMonth) + 1
    end;

fun month_range (day1:int, day2:int):int list = 
    if day1 > day2 then nil else what_month(day1) :: month_range(day1+1, day2);

fun oldest (listOfdates:date list):(int*int*int) option = 
    (case listOfdates
        of nil => NONE
         | _ => if (isSome(oldest(tl listOfdates))) andalso
         is_older(getOpt(oldest(tl listOfdates), hd listOfdates), (hd listOfdates)) 
                    then oldest(tl listOfdates)
                else SOME (hd listOfdates));

fun number_in_months_challenge (listOfdates:date list, listOfMonths:int
  list):int =
    let
     fun is_men x [] = false | is_men x (y::ys) = if (x=y) then true else (is_men x ys)
     fun rm_dupl [] = []
        | rm_dupl (x::xs) = 
        let
          val xs' = rm_dupl xs 
        in
          if (is_men x xs') then xs' else (x::xs')
        end
    in
      number_in_months(listOfdates, rm_dupl listOfMonths)
    end;

fun dates_in_months_challenge (listOfdates:date list, listOfMonths:int
  list):date list =
    let
     fun is_men x [] = false | is_men x (y::ys) = if (x=y) then true else (is_men x ys)
     fun rm_dupl [] = []
        | rm_dupl (x::xs) = 
        let
          val xs' = rm_dupl xs 
        in
          if (is_men x xs') then xs' else (x::xs')
        end
    in
      dates_in_months(listOfdates, rm_dupl listOfMonths)
    end;

fun reasonable_date (date:date):bool = 
    if #1 date <= 0 then false
    else if (#2 date < 1 orelse #2 date > 12) then false
         else 
           let 
             val daysOfMonthCom:int list = [31,28,31,30,31,30,31,31,30,31,30,31]
             val daysOfMonthLeap:int list = [31,29,31,30,31,30,31,31,30,31,30,31]
             fun get_nth (listOfInts:int list, n:int):int =
               (case n
                 of 1 => hd listOfInts
                  | _ => get_nth(tl listOfInts, n-1))
           in
             if ((#1 date) mod 400 = 0) orelse ((#1 date) mod 4 = 0 andalso (#1
             date) mod 100 <> 0) 
                then if (#3 date <= get_nth(daysOfMonthLeap, #2 date) andalso
                #3 date > 0) then true else false
             else if (#3 date <= get_nth(daysOfMonthCom, #2 date) andalso
                #3 date > 0) then true else false
           end;
 
    


