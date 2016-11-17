(*categorizing each restaurant
SMK = ??, ??
BF = ??, fancy

C = breakfast, restaurant - 4.0
WC = Italian, "Contemporary American", fancy - ****, $$$$

ES = Mexican, restaurant
O = Italian, sandwiches, Mexican, cafe
TMR = ??, restaurant

WWBC = ?? - 4.5

WB = ??, restaurant - 4.5


@GK = ethnic, Mexican, inthemiddle, Pescatarian, Vegetarian 4.5, $
@SB = American, Italian, pizza, lowkey, 4.5, $, Vegetarian, Pescatarian, GlutenFree
@TM = Italian, American, fancy, 4.0, $$$, Pescatarian, Vegetarian
@G = American, sandwiches, soups, lowkey, Vegetarian, Pescatarian,$, 4.5
@MCB = American, burgers, sandwiches, inthemiddle, Vegetarian,Pescatarian, 3.0, $$
@SHS = Japanese, sushi, restaurant, 4.5, Vegatarian, Pescatarian, $$
@OaC = "Contemporary American", Italian, restaurant, 4.0, $$$, Vegetarian, Pescatarian
*)


datatype restaurantName = Saffron_Mediterranean_Kitchen | Brasserie_Four 
| T_Maccarones | Clarettes | Whitehouse_Crawford | Graze | El_Sombrero 
| Olive | The_Marc_Restaurant | Grandmas_Kitchen 
| Walla_Walla_Bread_Company | Sweet_Basil | Wingman_Birdz 
| Mill_Creek_Brewpub | Shiki_Hibachi_Sushi | Ox_and_Cart;

datatype casual = lowkey | inthemiddle | restaurant | fancy;
datatype foodType = Burgers | Sushi | Pizza | Pasta | Sandwiches | Breakfast | Meat | Fish | MexicanFood | Soups | Salads;
datatype options = Vegetarian | Pescatarian | GlutenFree | None;
datatype cuisine = Japanese | Italian | American | Mexican;
datatype price = $ | $$ | $$$ | $$$$;

datatype categorizePlace = Place of restaurantName * casual * foodType list * options list * cuisine list * string * real;

val GK = Place(Grandmas_Kitchen, lowkey, [MexicanFood], [Pescatarian, Vegetarian], [Mexican], "$", 4.5);
val SB = Place(Sweet_Basil, lowkey, [Pizza, Salads], [Pescatarian, Vegetarian, GlutenFree], [Italian, American], "$", 4.5);
val TM = Place(T_Maccarones, fancy, [Pizza, Pasta, Meat, Fish, Salads], [Pescatarian, Vegetarian], [Italian, American], "$$$", 4.0);
val G = Place(Graze, lowkey, [Sandwiches, Soups, Salads], [Pescatarian, Vegetarian], [American], "$", 4.5);
val MCB = Place(Mill_Creek_Brewpub, inthemiddle, [Burgers, Sandwiches, Salads, Meat, Fish], [Pescatarian, Vegetarian], [American], "$$", 3.0);
val SHS = Place(Shiki_Hibachi_Sushi, restaurant, [Sushi, Salads, Soups], [Pescatarian, Vegetarian], [Japanese], "$$", 4.5);
val OaC = Place(Ox_and_Cart, restaurant, [Pasta, Meat, Fish, Salads], [Pescatarian, Vegetarian], [American], "$$$", 4.0);

val allplaces = [GK,SB,TM,G,MCB,SHS,OaC];

fun findName(Place(thename,_,_,_,_,_,_)) = thename;
fun findCasual(Place(_,thecas,_,_,_,_,_)) = thecas;
fun findFoodList(Place(_,_,thefoodlist,_,_,_,_)) = thefoodlist;
fun findOptionsList(Place(_,_,_,theoptionslist,_,_,_)) = theoptionslist;
fun findCuisineList(Place(_,_,_,_,thecuisinelist,_,_)) = thecuisinelist;
fun findPrice(Place(_,_,_,_,_,theprice,_)) = theprice;
fun findRating(Place(_,_,_,_,_,_,therate)) = therate;

fun makeTuple(x,y) = (x,y);

val testlist = [GK,SB,TM,G,MCB,SHS,OaC];


fun sortByRating([]) = [] (*second item is real*)
  | sortByRating(alist) =
    let fun removeFirstT((x,y),[]) = []
	  | removeFirstT((x,y),(a,b)::rest) =
		if ((abs(y-b)<0.001) andalso x=a)
		then rest
		else (a,b)::removeFirstT((x,y),rest);
	fun listMinT([(a,b:real)]) = (a,b)
	  | listMinT((a,b:real)::rest) = 
		let val n = #1(listMinT(rest));
		    val m:real = #2(listMinT(rest));
		in if b<m then (a,b) else (n,m)
		end;
	fun sortT([]) = []
	  | sortT(xx) = 
		let val m = listMinT(xx);
		    val l = removeFirstT(m,xx);
		in if not (null l) then m::sortT(l) else [m]
		end;
	fun tupleList([]) = []
	  | tupleList(x::rest) = makeTuple(findName(x),findRating(x))::tupleList(rest);
     in sortT(tupleList(alist))
     end;

fun sortByPrice([]) = [] (*second item is string*)
  | sortByPrice(alist) =
    let fun removeFirstT((x,y),[]) = []
	  | removeFirstT((x,y),(a,b)::rest) =
		if (y=b andalso x=a)
		then rest
		else (a,b)::removeFirstT((x,y),rest);
	fun listMinT([(a,b:string)]) = (a,b)
	  | listMinT((a,b:string)::rest) = 
		let val n = #1(listMinT(rest));
		    val m:string = #2(listMinT(rest));
		in if b<m then (a,b) else (n,m)
		end;
	fun sortT([]) = []
	  | sortT(xx) = 
		let val m = listMinT(xx);
		    val l = removeFirstT(m,xx);
		in if not (l=[]) then m::sortT(l) else [m]
		end;
	fun tupleList([]) = []
	  | tupleList(x::rest) = makeTuple(findName(x),findPrice(x))::tupleList(rest);
     in sortT(tupleList(alist))
     end;

(*Returns all Places with specified foodtype.*)
fun sortByFoodType(foodType, []) = []
   |sortByFoodType(foodType, placelist) = let 
   	fun contains(x,[]) = false |
  		contains(x,y::rest) = x=y orelse contains(x,rest);
   	fun tupleList([]) = []
	  | tupleList(x::rest) = makeTuple(findName(x),findFoodList(x))::tupleList(rest);
	fun returnSameType([]) = []
	   |returnSameType((x,y)::rest) = if contains(foodType,y) then (x,y)::returnSameType(rest)
	   else returnSameType(rest);
	  in returnSameType(tupleList(placelist))
	  end;	

fun sortByFanciness([]) = []
  | sortByFanciness(placelist) = let
  	fun tupleList([]) = []
	  | tupleList(x::rest) = makeTuple(findName(x),findCasual(x))::tupleList(rest);
	fun sneakySort(casual,[]) = []
	  | sneakySort(casual,(x,y)::rest) = if (y = casual) then (x,y)::sneakySort(casual,rest) else sneakySort(casual,rest);
	val tlist = tupleList(placelist); 
	in sneakySort(fancy,tlist)@sneakySort(restaurant,tlist)@sneakySort(inthemiddle,tlist)@sneakySort(lowkey,tlist)
	end;


fun sortByCuisine(cuisinetype,[]) = []
   |sortByCuisine(cuisinetype,placelist) = let 
   	fun tupleList([]) = []
	  | tupleList(x::rest) = makeTuple(findName(x),findCuisineList(x))::tupleList(rest);
   	val tlist = tupleList(placelist);
   	fun contains(x,[]) = false |
  		contains(x,y::rest) = x=y orelse contains(x,rest);

  	fun returnSameType([]) = []
	   |returnSameType((x,y)::rest) = if contains(cuisinetype,y) then (x,y)::returnSameType(rest)
	   else returnSameType(rest);
	in 
		returnSameType(tlist)
	end;



(*)
fun returnPlace

fun anyRestrictions

*)















(*removes first occurence of x tuple in list of tuples*)
fun removeFirst(x,[]) = []
  | removeFirst(x,y::rest) = if (x=y) then rest else y::removeFirst(x,rest);

fun removeFirstT((x,y),[]) = [] (*works if y is real*)
  | removeFirstT((x,y),(a,b)::rest) =
	if ((abs(y-b)<0.001) andalso x=a)
	then rest
	else (a,b)::removeFirstT((x,y),rest);

(*finds minimum of list of tuples (compares second item in tuple*)
fun listMinT([(a,b:real)]) = (a,b) (*returns tuple*) (*works if b is real*) (*remove real to make int*)
  | listMinT((a,b:real)::rest) = 
	let val n = #1(listMinT(rest));
	val m:real = #2(listMinT(rest));
	in if b<m then (a,b) else (n,m)
	end;

(*sorts list of tuples by second item; from lowest to highest*)
fun sortT([]) = []
  | sortT(xx) = 
	let val m = listMinT(xx);
	    val l = removeFirstT(m,xx);
	in if not (null l) then m::sortT(l)
 	else [m]
	end;






(*original listMin and sort functions*)
fun listMin([x]) = x
  | listMin(x::rest) = 
	let val m = listMin(rest);
	in if x<m then x else m
	end;
fun sort([]) = []
  | sort(xx) = 
	let val m = listMin(xx);
	in m::sort(removeFirst(m,xx))
	end;
