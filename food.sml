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


datatype restaurantName = Saffron_Mediterranean_Kitchen | Brasserie_Four | T_Maccarones | Clarettes | Whitehouse_Crawford | Graze | El_Sombrero | Olive | The_Marc_Restaurant | Grandmas_Kitchen | Walla_Walla_Bread_Company | Sweet_Basil | Wingman_Birdz | Mill_Creek_Brewpub | Shiki_Hibachi_Sushi | Ox_and_Cart;

datatype casual = lowkey | inthemiddle | restaurant | fancy;
datatype foodType = Burgers | Sushi | Pizza | Pasta | Sandwiches | Breakfast | Meat | Fish | MexicanFood | Soups | Salads;
datatype options = Vegetarian | Pescatarian | GlutenFree | None;
datatype cuisine = Japanese | Italian | American | Mexican;
datatype price = $ | $$ | $$$ | $$$$;

datatype categorizePlace = Place of restaurantName * casual * foodType list * options list * cuisine list * price * real;

val GK = Place(Grandmas_Kitchen, lowkey, [MexicanFood], [Pescatarian, Vegetarian], [Mexican], $, 4.5);
val SB = Place(Sweet_Basil, lowkey, [Pizza, Salads], [Pescatarian, Vegetarian, GlutenFree], [Italian, American], $, 4.5);
val TM = Place(T_Maccarones, fancy, [Pizza, Pasta, Meat, Fish, Salads], [Pescatarian, Vegetarian], [Italian, American], $$$, 4.0);
val G = Place(Graze, lowkey, [Sandwiches, Soups, Salads], [Pescatarian, Vegetarian], [American], $, 4.5);
val MCB = Place(Mill_Creek_Brewpub, inthemiddle, [Burgers, Sandwiches, Salads, Meat, Fish], [Pescatarian, Vegetarian], [American], $$, 3.0);
val SHS = Place(Shiki_Hibachi_Sushi, restaurant, [Sushi, Salads, Soups], [Pescatarian, Vegetarian], [Japanese], $$, 4.5);
val OaC = Place(Ox_and_Cart, restaurant, [Pasta, Meat, Fish, Salads], [Pescatarian, Vegetarian], [American], $$$, 4.0);

fun sortByFanciness(placeList)

fun sortByFoodType

fun anyRestrictions

fun sortByCuisine

fun sortByPrice

fun findName(Place(thename,_,_,_,_,_,_)) = thename;
fun findCasual(Place(_,thecas,_,_,_,_,_)) = thecas;
fun findFoodList(Place(_,_,thefoodlist,_,_,_,_)) = thefoodlist;
fun findOptionsList(Place(_,_,_,theoptionslist,_,_,_)) = theoptionslist;
fun findCuisineList(Place(_,_,_,_,thecuisinelist,_,_)) = thecuisinelist;
fun findPrice(Place(_,_,_,_,_,theprice,_)) = theprice;
fun findRating(Place(_,_,_,_,_,_,therate)) = therate;

fun makeTuple(x,y) = (x,y);

fun sortByRating([]) = []
  | sortByRating(alist) =
	let fun tupleList([]) = []
	      | tupleList(x::rest) = makeTuple(findName(x),findRating(x))::tupleList(rest);
	in sort(#2(tupleList(alist)));

fun returnPlace

fun removeFirst(x,[]) = []
  | removeFirst(x,y::rest) = if (x=y) then rest else y::removeFirst(x,rest);

fun listMinT([(a,b)]) = (a,b) (*returns tuple*)
  | listMinT((a,b)::rest) = 
	let val n = #1(listMinT(rest));
	val m = #2(listMinT(rest));
	in if b<m then (a,b) else (n,m)
	end;
fun sortT([]) = []
  | sortT(xx) = 
	let val m = listMinT(xx);
	    val l = removeFirst(m,xx);
	in if not (l = []) then m::sortT(l)
 	else [m]
	end;



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
