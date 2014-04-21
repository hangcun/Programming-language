(* Dan Grossman, CSE341 Spring 2013, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(*fun all_except_option(str:string, str_list:string list):string list option = 
    case str_list of
         nil => NONE | x::xs =>
            if same_string(str,x) then SOME xs 
            else let
                    val temp_str_list = all_except_option(str,xs)
                 in
                    if isSome temp_str_list then SOME (x::(valOf temp_str_list))
                    else NONE
                 end 
                 *)
fun all_except_option(str:string, str_list:string list):string list option = 
    case str_list of
         nil => NONE | x::xs =>
            if same_string(str,x) then SOME xs 
            else case all_except_option(str,xs) of
                      NONE => NONE
                    | SOME slist => SOME (x::slist)
 

fun get_substitutions1(substitutions:(string list) list, s:string):string list = 
    case substitutions of
         nil => []
       | x::xs => let
                    val temp_str_list = all_except_option(s,x)
                  in
                    if isSome temp_str_list then (valOf temp_str_list)@get_substitutions1(xs,s)
                    else get_substitutions1(xs,s)
                  end

fun get_substitutions2(substitutions:(string list) list, s:string):string list = 
    let fun get_sub(sub:(string list) list, ret_str_list:string list) =
           case sub of
                nil => ret_str_list
              | x::xs => let 
                            val temp_str_list = all_except_option(s,x)
                         in
                            if isSome temp_str_list then get_sub(xs, ret_str_list@(valOf temp_str_list))
                            else get_sub(xs, ret_str_list)  
                         end
    in get_sub(substitutions, []) end

fun similar_names(substitutions:(string list) list,
  full_name:{first:string,middle:string,last:string}):{first:string,middle:string,last:string} list = 
    let
      type full_name_type = {first:string,middle:string,last:string}
      fun similar_names_helper(sub:string list):full_name_type list =
        case sub of
             nil => []
           | x::xs => {first = x, last = #last full_name, middle = #middle
           full_name} :: similar_names_helper(xs)
    in
      full_name::similar_names_helper(get_substitutions2(substitutions, #first full_name))
    end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color(card_sam:card):color = 
    case card_sam of
         (Clubs,_) => Black
       | (Spades,_) => Black
       | _ => Red

fun card_value(card_sam:card):int = 
    case card_sam of 
         (_,Num(x)) => x
       | (_,Ace) => 11
       | _ => 10

fun remove_card(cs:card list, c:card, e):card list = 
    let
      fun men(cs_temp:card list):bool =
        case cs_temp of
             nil => false
           | x::xs => if x=c then true else men(xs)
      fun delete_first(cs_temp:card list):card list = 
        case cs of
             [] => []
           | y::ys => if y=c then ys else y::delete_first(ys)
    in
      if men(cs) then delete_first(cs) else raise e
    end

fun all_same_color(cs:card list):bool =
    case cs of
         nil => true
       | _::[] => true 
       | x::(y::rs) => if card_color(x) = card_color(y) andalso
              all_same_color(rs) then true else false

fun sum_cards(cs:card list):int = 
    let
      fun sum_cards_helper(cs:card list, sum:int):int = 
        case cs of 
             nil => sum
           | x::xs => sum_cards_helper(xs, sum+card_value(x))
    in
      sum_cards_helper(cs, 0)
    end

fun score(cs:card list, goal:int):int = 
    let
      val sum = sum_cards(cs)
      val pre_score = if sum > goal then 3*(sum-goal) else goal-sum
    in
      if all_same_color(cs) then pre_score div 2 else pre_score
    end

fun officiate(cs:card list, move_list:move list, goal:int):int = 
    let
      fun officiate_helper(held_cards:card list, moves:move list, card_list:card list) = 
        (case moves of
              nil => score(held_cards, goal)
            | x::xs => (case x of
                             Discard(m) => officiate_helper(remove_card(held_cards,m,IllegalMove),xs,card_list)
                           | Draw => (case card_list of
                                            nil => score(held_cards,goal)
                                          | y::ys =>
                                              if(sum_cards(y::held_cards) > goal) then
                                                score(y::held_cards,goal)
                                              else
                                                officiate_helper(y::held_cards,xs,ys))))
    in
      officiate_helper([],move_list,cs)
    end
      
    

fun provided_test1 () = (* correct behavior: raise IllegalMove *)
    let val cards = [(Clubs,Jack),(Spades,Num(8))]
	val moves = [Draw,Discard(Hearts,Jack)]
    in
	officiate(cards,moves,42)
    end

fun provided_test2 () = (* correct behavior: return 3 *)
    let val cards = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)]
	val moves = [Draw,Draw,Draw,Draw,Draw]
    in
 	officiate(cards,moves,42)
    end




val str_list_list1 =
  [["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]];
val str_list_list2 =
  [["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]];

val str1 = "Fred";
val str2 = "Jeff";
val full_name1 = {first="Fred",middle="W",last="Smith"};

get_substitutions1(str_list_list1,str1);
get_substitutions2(str_list_list1,str1);

get_substitutions1(str_list_list2,str2);
get_substitutions2(str_list_list2,str2);

similar_names(str_list_list1,{first="Fred",middle="W",last="Smith"});


provided_test2();

val cards = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)]
val moves = [Draw,Draw,Draw,Draw,Draw](*insufficient cards *)
val score1 = officiate(cards,moves,42)(*should be 3*)

val cards = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace),(Clubs,Num 1)]
val moves = [Draw,Draw,Draw,Draw](*there are cards left + same color*)
val score2 = officiate(cards,moves,42)(*should be 4*)

val cards = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace),(Diamonds,Num 1)]
val moves = [Draw,Draw,Draw,Draw](*there are cards left + different colors*)
val score3 = officiate(cards,moves,42)(*should be 8*)
