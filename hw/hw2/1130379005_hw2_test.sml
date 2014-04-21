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
