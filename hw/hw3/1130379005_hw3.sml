(* Dan Grossman, CSE341 Spring 2013, HW3 Provided Code *)
exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
fun only_capitals(str_list:string list):string list =
    List.filter (fn x=>Char.isUpper(String.sub(x,0))) str_list;

fun longest_string1(str_list:string list):string =
    foldl (fn (x,y) => if String.size(x) <= String.size(y) then y else x) "" str_list;

fun longest_string2(str_list:string list):string =
    foldl (fn (x,y) => if String.size(x) < String.size(y) then y else x) "" str_list;
    
fun longest_string_helper f str_list =
    foldl (fn (x,y) => if f(String.size(x),String.size(y)) then y else x) "" str_list; 
    
val longest_string3 = longest_string_helper (fn (x,y) => x <= y);

val longest_string4 = longest_string_helper (fn (x,y) => x < y);  

val longest_capitalized = longest_string1 o only_capitals;

fun rev_string(str:string):string = (String.implode o rev o String.explode) str;

fun first_answer f a_list =
    case a_list of
         [] => raise NoAnswer
       | x::xs => case f x of
                       SOME v => v
                     | NONE => first_answer f xs;

fun all_answers f a_list =
    let 
      fun accumulator f a_list = 
        case a_list of
             [] => SOME []
           | x::xs => case (f x,accumulator f xs) of
                           (SOME v,SOME (v_list)) => SOME (v@v_list)
                         | (_,_) => NONE
    in
      accumulator f a_list
    end;

val count_wildcards = g (fn () => 1) (fn (x:string) => 0);

val count_wild_and_variable_lengths = g (fn () => 1) (fn x => String.size(x));

fun count_some_var(str:string, p:pattern) = g (fn () =>0) (fn x => if x = str then 1 else 0) p 

fun check_pat p =
    let
      fun p_to_str p = 
        case p of 
             Variable s => [s]
           | TupleP ps => List.foldl (fn (r, i) => p_to_str(r)@i) [] ps
           | _ => []
      fun isRep str_list = 
        case str_list of
             [] => true
           | x::xs => if List.exists (fn y => x=y) xs then false else isRep xs
    in
      (isRep o p_to_str) p
    end;

fun match (value:valu, p:pattern):(string*valu) list option =
    case (value,p) of
         (_,Wildcard) => SOME []
       | (_,Variable s) => SOME [(s,value)]
       | (Unit,UnitP) => SOME []
       | (Const i,ConstP n) => SOME []
       | (Tuple vs,TupleP ps) => if List.length(vs)=List.length(ps) then all_answers match (ListPair.zip(vs,ps)) else NONE
       | (Constructor (s2,v),ConstructorP(s1,p)) => if s1 = s2 then match (v,p) else NONE
       | (_,_) => NONE;

fun first_match value p_list =
    SOME (first_answer (fn x => match (value,x)) p_list) handle NoAnswer => NONE

fun typecheck_patterns(typ_list:(string*string*typ) list, pat_list:pattern list):typ option = 
    let 
      fun pat_to_typ(pat:pattern, typ_list:(string*string*typ) list):typ =
        let
          fun tuple_helper(ptn:pattern list):typ list =
            case ptn of
                 nil => []
               | x::xs => pat_to_typ(x, typ_list)::tuple_helper xs
          fun constr_helper(typ_list, datatype_str, con_type) =
            case typ_list of
                 nil => raise NoAnswer
               | (x,y,z)::xs => if x=datatype_str andalso con_type=z then Datatype y
                                else constr_helper(xs, datatype_str, con_type)
        in
          case pat of
               Wildcard => Anything
             | Variable s => Anything
             | UnitP => UnitT
             | ConstP v => IntT
             | TupleP ps => TupleT(tuple_helper ps)
             | ConstructorP (s,p) => constr_helper(typ_list, s, pat_to_typ(p,typ_list))
        end
      fun most_lenient (typ_list1, typ_list2) =
        let
          fun tup_helper type_list =
            case type_list of 
                 (nil, nil) => []
               | (x::xs, y::ys) => most_lenient(x,y)::tup_helper(xs,ys)
               | _ => raise NoAnswer
        in
          case (typ_list1, typ_list2) of
               (Anything,_) => typ_list2
             | (Datatype s1,Datatype s2) => if s1=s2 then typ_list1 else raise NoAnswer
             | (IntT,IntT) => IntT
             | (UnitT,UnitT) => UnitT
             | (TupleT ps1,TupleT ps2) => TupleT(tup_helper(ps1,ps2))
             | (_,Anything) => typ_list1
             | (_,_) => raise NoAnswer
        end
    in
      SOME (foldl (fn (x,acc) => most_lenient(pat_to_typ(x,typ_list),acc)) Anything pat_list) handle NoAnswer => NONE
    end

