is_older((1991,10,11), (1991,10,10));
is_older((1991,10,11), (1991,10,12));
is_older((1991,10,11), (1991,10,11));
is_older((1991,10,11), (1991,11,11));
is_older((1991,10,11), (1991,9,12));
is_older((1991,10,11), (1992,10,11));
is_older((1991,10,11), (1993,10,11));
is_older((1991,10,11), (1992,9,11));
is_older((1991,10,11), (1992,12,11));

val listOfdate:date list = [(1991,10,11),(1992,9,11),(1992,10,11),(1991,11,11)];
number_in_month(listOfdate, 10);
number_in_months(listOfdate, [10,9]);
dates_in_month(listOfdate, 10);
dates_in_months(listOfdate, [10,9]);

val listOfStrings:string list =
  ["January","February","March","April","May","June","July","August","September","October","November","December"];

get_nth(listOfStrings, 1);
get_nth(listOfStrings, 2);
get_nth(listOfStrings, 10);
date_to_string((2013,1,20));
date_to_string((2014,3,23));
date_to_string((2012,4,1));

val listOfInts:int list = [1,2,3,4,5,6,7,8,9];
number_before_reaching_sum(1,listOfInts);
number_before_reaching_sum(9,listOfInts);
number_before_reaching_sum(11,listOfInts);
number_before_reaching_sum(14,listOfInts);
number_before_reaching_sum(16,listOfInts);
what_month(30);
what_month(31);
what_month(59);
what_month(60);
what_month(365);

month_range(28,28);
month_range(28,31);
month_range(59,60);

oldest(listOfdate);
oldest([]);

number_in_months_challenge(listOfdate, [10,10,9]);
dates_in_months_challenge(listOfdate, [10,10,9]);
reasonable_date((~1,1,1));
reasonable_date((0,1,1));
reasonable_date((1991,0,1));
reasonable_date((1991,13,1));
reasonable_date((1991,2,29));
reasonable_date((2004,2,29));

