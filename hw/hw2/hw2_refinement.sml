(* Dan Grossman, CSE341 Spring 2013, HW2 Provided Code *)

fun same_string(s1 : string, s2 : string) =
    s1 = s2

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
       | x::xs => case all_except_option(s,x) of
                       NONE => get_substitutions1(xs,s)
                     | SOME slist => slist@get_substitutions1(xs,s)

fun get_substitutions2(substitutions:(string list) list, s:string):string list = 
    let fun get_sub(sub:(string list) list, ret_str_list:string list) =
           case sub of
                nil => ret_str_list
              | x::xs => case all_except_option(s,x) of
                              NONE => get_sub(xs, ret_str_list)
                            | SOME slist => get_sub(xs, ret_str_list@slist)
    in 
      get_sub(substitutions, []) 
    end

fun similar_names(substitutions:(string list) list,
  full_name:{first:string,middle:string,last:string}):{first:string,middle:string,last:string} list = 
    let
      type full_name_type = {first:string,middle:string,last:string}
      val {first = f, middle = m, last = l} = full_name 
      fun similar_names_helper(sub:string list):full_name_type list =
        case sub of
             nil => []
           | x::xs => {first = x, last = l, middle = m} :: similar_names_helper(xs)
    in
      full_name::similar_names_helper(get_substitutions2(substitutions, f))
    end

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
        case cs_temp of
             [] => []
           | y::ys => if y=c then ys else y::delete_first(ys)
    in
      if men(cs) then delete_first(cs) else raise e
    end

fun all_same_color(cs:card list):bool =
    case cs of
         x::(y::rs) => if card_color(x) = card_color(y) andalso 
       all_same_color(y::rs) then true else false
       | _ => true

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
        case (moves, card_list) of
             (Discard(m)::xs,_) => officiate_helper(remove_card(held_cards,m,IllegalMove),xs,card_list)
           | (Draw::xs,y::ys) => if(sum_cards(y::held_cards) > goal) then score(y::held_cards,goal)
                             else officiate_helper(y::held_cards,xs,ys)
           | (_,_) => score(held_cards, goal)
    in
      officiate_helper([],move_list,cs)
    end
      
fun score_challenge(cs:card list, goal:int):int = 
    let
      fun least_score(back_cards:card list, pre_cards:card list, least:int) = 
        case back_cards of 
             nil => least
           | (su,Ace)::rest => 
               let
                 val ace_one_cards = pre_cards@((su,Num(1))::rest)
                 val cur_score = score(ace_one_cards, goal)
               in
                 if cur_score < least then
                   least_score(rest,pre_cards@[(su,Num(1))],cur_score)
                 else least_score(rest,pre_cards@[(su,Ace)],least)
               end
           | c::rest => least_score(rest,pre_cards@[c],least)
      val ini_score = score(cs,goal)
    in
      least_score(cs,[],ini_score)
    end

fun officiate_challenge(cs:card list, move_list:move list, goal:int):int = 
    let
      fun officiate_helper(held_cards:card list, moves:move list, card_list:card list) = 
        case (moves, card_list) of
             (Discard(m)::xs,_) => officiate_helper(remove_card(held_cards,m,IllegalMove),xs,card_list)
           | (Draw::xs,y::ys) => if(sum_cards(y::held_cards) > goal) then score(y::held_cards,goal)
                             else officiate_helper(y::held_cards,xs,ys)
           | (_,_) => score_challenge(held_cards, goal)
    in
      officiate_helper([],move_list,cs)
    end

(*for this challenge problem, 3(b), I assume that the careful_player can just
* look ahead to the next card, and when the condition is none of these four ones
* described in problem description, I set a Draw action to move list by
* default*)
fun careful_player(cs:card list, goal:int):move list = 
    let 
      fun discard_zero(pre_hcd:card list,res_hcd:card list):card option = 
            case res_hcd of
                 nil => NONE 
               | x::xs => if score(pre_hcd@res_hcd,goal) = 0 then NONE 
                            else if score(pre_hcd@xs,goal) = 0 then SOME x
                           (* else if sum_cards(pre_hcd@xs) < goal andalso card_value(x) < least
                                    then
                                      discard_zero(x::pre_hcd,xs,card_value(x))&*)
                          else discard_zero(x::pre_hcd,xs) 
      fun get_move_cards(cs_rest:card list,move_list:move list,held_cards:card list):move list = 
            case cs_rest of
                 nil => move_list
               | x::xs => if sum_cards(held_cards) + 10 <= goal then
                 get_move_cards(xs,move_list@[Draw],held_cards@[x])
                            else if score(held_cards,goal) = 0 then move_list
                            else if sum_cards(held_cards@[x]) > goal then
                              (case discard_zero(held_cards@[x],[]) of
                                    NONE => move_list 
                                  | SOME a =>
                                      get_move_cards(xs,move_list@[Discard(a)],remove_card(held_cards@[x],a,IllegalMove)))
                                 else
                                   get_move_cards(xs,move_list@[Draw],held_cards@[x])
    in
     case cs of
          nil => []
        | _ => get_move_cards(cs,[],[])
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
 	officiate_challenge(cards,moves,42)
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


val raise_exn = provided_test1() handle IllegalMove => 1;
provided_test2();

val cards = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)];
val moves = [Draw,Draw,Draw,Draw,Draw](*insufficient cards *);
val score1 = officiate_challenge(cards,moves,46)(*should be 3*);

val cards = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace),(Clubs,Num 1)];
val moves = [Draw,Draw,Draw,Draw](*there are cards left + same color*);
val score2 = officiate_challenge(cards,moves,42)(*should be 3*);

val cards = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace),(Diamonds,Num
1)];
val moves = [Draw,Draw,Draw,Draw](*there are cards left + different colors*);
val score3 = officiate_challenge(cards,moves,42)(*should be 3*);


val color1 = card_color (Clubs,Jack) (*Black*);
val color2 = card_color (Diamonds, Queen) (*Red*);
val card_val1 = card_value (Clubs, Num 2) (*2*);
val card_val2 = card_value (Clubs, Ace) (*11*);
val card_val3 = card_value (Clubs, King) (*10*);

exception CardNotInList 
val cards1 = [(Clubs,Num 2), (Clubs, Ace), (Clubs, King)];
val cards2 = [(Clubs,Num 2), (Spades, King), (Clubs, Ace), (Clubs, Ace)];
val cards3 = [(Clubs, Ace)];
val cards4 = [(Clubs, Jack)];
val card0 = (Clubs, Ace);

(*anwser:[(Clubs,Num 2), (Clubs, King)]*)
val removed_cardlist1 = remove_card(cards1, card0, CardNotInList);

(*answer: [(Clubs,Num 2), (Spades, King), (Clubs, Ace)]*)
val removed_cardlist2 = remove_card(cards2, card0, CardNotInList);

(*answer: []*)
val removed_cardlist3 = remove_card(cards3, card0, CardNotInList);

(*answer: []*)
(*val removed4 = remove_card(cards4, card0, CardNotInList) handle CardNotInList
* c => []*)


val cards1 = [(Clubs,Num 2)];
val cards2 = [(Clubs,Num 2), (Clubs, Ace), (Clubs, King)];
val cards3 = [(Clubs,Num 2), (Spades, King), (Clubs, Ace)];
val cards4 = [(Clubs,Num 2), (Spades, King), (Diamonds, Ace)];

val same1 = all_same_color(cards1)(*true*);
val same2 = all_same_color(cards2)(*true*);
val same3 = all_same_color(cards3)(*true*);
val same4 = all_same_color(cards4)(*false*);

val cards1 = [(Clubs,Num 2)];
val cards2 = [(Clubs,Num 2), (Clubs, Num 3), (Clubs, King)];
val cards3 = [(Clubs,Num 2), (Spades,Num 3), (Clubs, Ace)];

val sum1 = sum_cards(cards1)(*2*);
val sum2 = sum_cards(cards2)(*15*);
val sum3 = sum_cards(cards3)(*16*);

val goal = 16;
val cards1 = [(Clubs,Num 2), (Spades,Num 6), (Clubs, Ace)](*same color, sum
19*);
val cards2 = [(Clubs,Num 2), (Spades,Num 3), (Clubs, Jack)](*same color, sum
15*);
val cards3 = [(Clubs,Num 2), (Spades,Num 6), (Diamonds, Ace)](*different color,
sum 19*);
val cards4 = [(Clubs,Num 2), (Spades,Num 3), (Diamonds, Jack)](*different color,
sum 15*);

val score1 = score(cards1, goal)(*4*);
val score2 = score(cards2, goal)(*0*);
val score3 = score(cards3, goal)(*9*);
val score4 = score(cards4, goal)(*1*);

val cards = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)];
val moves = [Draw,Draw,Draw,Draw,Draw](*insufficient cards *);
val score1 = officiate(cards,moves,42)(*should be 3*);

val cards = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace),(Clubs,Num 1)];
val moves = [Draw,Draw,Draw,Draw](*there are cards left + same color*);
val score2 = officiate(cards,moves,42)(*should be 3*);

val cards = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace),(Diamonds,Num
1)];
val moves = [Draw,Draw,Draw,Draw](*there are cards left + different colors*);
val score3 = officiate(cards,moves,42)(*should be 3*);

val care_p = careful_player(cards, 44);
val score3 = officiate(cards,care_p,44);
