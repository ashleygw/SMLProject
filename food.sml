(*George Ashley and Brooke Taylor*)
(*CS220 Project - SML Walla Walla Restaurants Sorter*)

Control.Print.printDepth:=100;
Control.Print.printLength:=100;

(*DATATYPES*)
datatype restaurantName = Saffron_Mediterranean_Kitchen | Brasserie_Four 
| T_Maccarones | Clarettes | Whitehouse_Crawford | Graze | El_Sombrero 
| Olive | The_Marc_Restaurant | Grandmas_Kitchen 
| Walla_Walla_Bread_Company | Sweet_Basil | Wingman_Birdz 
| Mill_Creek_Brewpub | Shiki_Hibachi_Sushi | Ox_and_Cart;

datatype casual = Lowkey | Inthemiddle | Restuarant | Fancy;
datatype foodType = Burgers | Sushi | Pizza | Pasta | Sandwiches | Breakfast | Meat | Fish | MexicanFood | Soups | Salads | Tapas;
datatype options = Vegetarian | Pescatarian | GlutenFree | None;
datatype cuisine = Japanese | Italian | American | Mexican | Spanish | French;

datatype aPlace = Place of restaurantName * casual * foodType list * options list * cuisine list * string * real * string;

(*all data courtesy of Google, restaurants' websites, and general guesses by Brooke and George based on experience*)
val SMK = Place(Saffron_Mediterranean_Kitchen, Fancy, [Pizza, Pasta, Meat, Tapas], [Pescatarian], [Italian, Spanish], "$$", 4.5, "A romantic restaurant serving imaginative takes on Spanish and Italian tapas, pizza and pasta.");
val BF = Place(Brasserie_Four, Inthemiddle, [Meat, Soups, Salads], [Pescatarian, Vegetarian], [French], "$$", 4.5, "An unassuming cafe serving classic French fare and pizza, plus Washington wines to drink in or take home.");
val TM = Place(T_Maccarones, Fancy, [Pizza, Pasta, Meat, Fish, Salads], [Pescatarian, Vegetarian], [Italian, American], "$$$", 4.0, "Locally sourced produce and meats round out the upmarket Italian menu at this elegant bistro.");
val C = Place(Clarettes, Lowkey, [Breakfast], [Pescatarian, Vegetarian], [American], "$$", 4.0, "A cozy, old-school eatery with booths offering homestyle American eats for breakfast, lunch, and dinner.");
val WHC = Place(Whitehouse_Crawford, Fancy, [Pasta, Meat, Salads, Soups], [Pescatarian, Vegetarian], [American], "$$$", 4.0, "A light-filled restaurant in a remodeled 1904 sawmill serving inventive American food and local wines.");
val G = Place(Graze, Lowkey, [Sandwiches, Soups, Salads], [Pescatarian, Vegetarian], [American], "$", 4.5, "A hip hangout for gourmet sandwiches.");
val ES = Place(El_Sombrero, Lowkey, [MexicanFood], [Pescatarian, Vegetarian], [Mexican], "$$", 4.5, "The finest authentic Mexican food found north of the border, with a warm, inviting atmosphere to welcome you to a most pleasurable dining experience.");
val O = Place(Olive, Lowkey, [Pizza, Pasta, Sandwiches, Meat, Soups, Salads], [Pescatarian, Vegetarian], [American, Italian], "$$", 4.0, "An upscale cafe with a locally-sourced American menu, plus an artisanal market and wine shop.");
val TMR = Place(The_Marc_Restaurant, Restuarant, [Pasta, Meat, Fish, Soups, Salads], [Pescatarian, Vegetarian], [American], "$$$", 4.0, "An upscale American restaurant at the Marcus Whitman Hotel offering seasonal cooking and regional wines.");
val GK = Place(Grandmas_Kitchen, Lowkey, [MexicanFood], [Pescatarian, Vegetarian], [Mexican], "$", 4.5, "A down-to-earth, contemporary eatery featuring homestyle regional Mexican fare, plus beer and wine.");
val WWBC = Place(Walla_Walla_Bread_Company, Lowkey, [Pizza, Meat, Sandwiches, Breakfast], [Pescatarian, Vegetarian], [French, American], "$$", 4.5, "An industrial-chic cafe & bakery offering wood-fired pizza & steaks, plus artisan bread and pastries.");
val SB = Place(Sweet_Basil, Lowkey, [Pizza, Salads], [Pescatarian, Vegetarian, GlutenFree], [Italian, American], "$", 4.5, "A relaxed eatery with TVs tuned to sports serving up NYC-style pizza, beer, and wine.");
val WB = Place(Wingman_Birdz, Inthemiddle, [Meat, Soups, Salads], [Pescatarian, Vegetarian], [American], "$$", 4.5, "A family-run, kid-friendly restuarant specializing in designer chicken wings, craft beer, and local wine.");
val MCB = Place(Mill_Creek_Brewpub, Inthemiddle, [Burgers, Sandwiches, Salads, Meat, Fish], [Pescatarian, Vegetarian], [American], "$$", 3.0, "A longtime locale with a chill vibe serving fish tacos and other bar staples along with housemade brews.");
val SHS = Place(Shiki_Hibachi_Sushi, Restuarant, [Sushi, Salads, Soups], [Pescatarian, Vegetarian], [Japanese], "$$", 4.5, "A great place for sushi-lovers, with lots of fun for family and friends.");
val OaC = Place(Ox_and_Cart, Restuarant, [Pasta, Meat, Fish, Salads], [Pescatarian, Vegetarian], [American], "$$$", 4.0, "A local ranch and farm focused restaurant in the heart of downtown Walla Walla.");

val allplaces = [SMK, BF, TM, C, WHC, G, ES, O, TMR, GK, WWBC, SB, WB, MCB, SHS, OaC];


(*---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------*)

(*FUNCTIONS*)

(* Helper Functions *)

(*functions return item in the aPlace datatype--used as helper functions*)
fun findName(Place(thename,_,_,_,_,_,_,_)) = thename;
fun findCasual(Place(_,thecas,_,_,_,_,_,_)) = thecas;
fun findFoodList(Place(_,_,thefoodlist,_,_,_,_,_)) = thefoodlist;
fun findOptionsList(Place(_,_,_,theoptionslist,_,_,_,_)) = theoptionslist;
fun findCuisineList(Place(_,_,_,_,thecuisinelist,_,_,_)) = thecuisinelist;
fun findPrice(Place(_,_,_,_,_,theprice,_,_)) = theprice;
fun findRating(Place(_,_,_,_,_,_,therate,_)) = therate;
fun findDescription(Place(_,_,_,_,_,_,_,thedescr)) = thedescr;

(*makes a tuple--used as a helper function*)
fun makeTuple(x,y) = (x,y);


(* Sort Functions *)

(*Takes a list of places and returns a list sorted by ratings (real)*)
fun sortByRating([]) = []
  | sortByRating(alist) =
    let fun partition(pivot, [], bef, aft, mayPrecede) = (bef,aft)
	  | partition(pivot, x::rest, bef, aft, mayPrecede) = 
		    if mayPrecede(x,pivot) 
		    then partition(pivot, rest, x::bef, aft, mayPrecede)
		    else partition(pivot, rest, bef, x::aft, mayPrecede);
	fun quickSortT([], mayPrecede) = []
	  | quickSortT((a,b)::rest, mayPrecede) = 
		    let val (bef,aft) = partition((a,b), rest, [], [], mayPrecede);
		    in quickSortT(bef,mayPrecede) @ [(a,b)] @ quickSortT(aft,mayPrecede)
		    end;
	fun tupleList([]) = []
	  | tupleList(x::rest) = makeTuple(findName(x),findRating(x))::tupleList(rest);
     in quickSortT(tupleList(alist), fn((a,b),(c,d)) => b<=d)
     end;

(*Takes a list of places and returns a list sorted by price (string)*)
fun sortByPrice([]) = []
  | sortByPrice(alist) =
    let fun partition(pivot, [], bef, aft, mayPrecede) = (bef,aft)
	  | partition(pivot, x::rest, bef, aft, mayPrecede) = 
		    if mayPrecede(x,pivot) 
		    then partition(pivot, rest, x::bef, aft, mayPrecede)
		    else partition(pivot, rest, bef, x::aft, mayPrecede);
	fun quickSortT([], mayPrecede) = []
	  | quickSortT((a,b)::rest, mayPrecede) = 
		    let val (bef,aft) = partition((a,b), rest, [], [], mayPrecede);
		    in quickSortT(bef,mayPrecede) @ [(a,b)] @ quickSortT(aft,mayPrecede)
		    end;
	fun tupleList([]) = []
	  | tupleList(x::rest) = makeTuple(findName(x),findPrice(x))::tupleList(rest);
     in quickSortT(tupleList(alist),fn((a,b),(c,d)) => b<=d)
     end;

(*Returns all places with specified foodtype*)
fun sortByFoodType(foodType, []) = []
  | sortByFoodType(foodType, placelist) = let 
   	fun contains(x,[]) = false |
  		contains(x,y::rest) = x=y orelse contains(x,rest);
   	fun tupleList([]) = []
	  | tupleList(x::rest) = makeTuple(findName(x),findFoodList(x))::tupleList(rest);
	fun returnSameType([]) = []
	   |returnSameType((x,y)::rest) = if contains(foodType,y) then (x,y)::returnSameType(rest)
	   else returnSameType(rest);
	  in returnSameType(tupleList(placelist))
	  end;	

(*Takes a list of places and returns how fancy they are, sorted from most to least*)
fun sortByFanciness([]) = []
  | sortByFanciness(placelist) = let
  	fun tupleList([]) = []
	  | tupleList(x::rest) = makeTuple(findName(x),findCasual(x))::tupleList(rest);
	fun customSort(casual,[]) = []
	  | customSort(casual,(x,y)::rest) = if (y = casual) then (x,y)::customSort(casual,rest) else customSort(casual,rest);
	val tlist = tupleList(placelist); 
	in customSort(Fancy,tlist)@
		customSort(Restuarant,tlist)@
		customSort(Inthemiddle,tlist)@
		customSort(Lowkey,tlist)
	end;

(*Takes a choice of cuisine and list of places and returns a list of restaurants and what cuisine they serve*)
fun sortByCuisine(cuisinetype,[]) = []
  | sortByCuisine(cuisinetype,placelist) = let 
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

(*Takes a diet restriction and list of places and returns a list of restaurant names and their accommodations*)
fun dietaryRestrictions(restriction, []) = []
  | dietaryRestrictions(restriction, placelist) = let 
   	fun tupleList([]) = []
	  | tupleList(x::rest) = makeTuple(findName(x),findOptionsList(x))::tupleList(rest);
	val tlist = tupleList(placelist);
   	fun contains(x,[]) = false |
  		contains(x,y::rest) = x=y orelse contains(x,rest);
  	fun returnSameType([]) = []
	   |returnSameType((x,y)::rest) = if contains(restriction,y) then (x,y)::returnSameType(rest)
	   else returnSameType(rest);
	in 
		returnSameType(tlist)
	end;

(* Print Functions*)

(*Converts datatypes to strings*)
fun stringName(Saffron_Mediterranean_Kitchen) = "Saffron Mediterranean Kitchen"
  | stringName(Brasserie_Four) = "Brasserie Four"
  | stringName(T_Maccarones) = "T Maccarone's"
  | stringName(Clarettes) = "Clarette's"
  | stringName(Whitehouse_Crawford) = "Whitehouse Crawford"
  | stringName(Graze) = "Graze"
  | stringName(El_Sombrero) = "El Sombrero Mexican Restaurant"
  | stringName(Olive) = "Olive Marketplace and Cafe"
  | stringName(The_Marc_Restaurant) = "The Marc Restaurant"
  | stringName(Grandmas_Kitchen) = "Grandma's Kitchen"
  | stringName(Walla_Walla_Bread_Company) = "The Walla Walla Bread Company"
  | stringName(Sweet_Basil) = "Sweet Basil Pizzeria"
  | stringName(Wingman_Birdz) = "Wingman Birdz & Brewz"
  | stringName(Mill_Creek_Brewpub) = "Mill Creek Brewpub"
  | stringName(Shiki_Hibachi_Sushi) = "Shiki Hibachi Sushi"
  | stringName(Ox_and_Cart) = "Ox and Cart";

fun stringcasual(Lowkey) = "Lowkey"
  | stringcasual(Inthemiddle) = "Inthemiddle"
  | stringcasual(Restuarant) = "Restuarant"
  | stringcasual(Fancy) = "Fancy";

fun stringOptions(Vegetarian) = "Vegetarian"
  | stringOptions(Pescatarian) = "Pescatarian"
  | stringOptions(GlutenFree) = "GlutenFree"
  | stringOptions(None) = "None";

fun stringOptLst([]) = ""
  | stringOptLst([a]) = stringOptions(a)
  | stringOptLst(a::rest) = stringOptions(a) ^ ", " ^ stringOptLst(rest);

(*Takes a place and returns description*)
fun printDescription(place) = print(stringName(findName(place)) ^ ":\n" ^ findDescription(place) ^ "\n");

(*Breaks up string between words if too long in terminal for formatting -- help from Derek Slone*)
fun checkIfWord(string1:string) = 
    let
	val len = size(string1)
	val i = ref 0;
	val x = ref 1;
    in (
	while substring(string1, 78 - !i, 1) <> " " do (
	  i := !i + 1;
	  x := !x + 1
	);
      substring(string1, 0, 79 - !i) ^ " \n " ^ substring(string1, 79 - !i, (len) - (79 - !i))
    )
    end;

fun stringdescription(string1) = if size(string1) > 79 then checkIfWord(string1) else string1;

(*Takes a single place and returns name, description, rating, price, dietary restrictions, and casual atmosphere*)
fun printPlace(Place(a, b, c, d, e, f, g, h)) = print(" \n " ^ stringName(a) ^ ": \n " ^ stringdescription(h) ^ " \n Rating: " ^ Real.toString(g) ^ " stars \n " ^ "Price: " ^ f ^ " \n Options available: " ^ stringOptLst(d) ^ " \n Atmosphere: " ^ stringcasual(b) ^ " \n \n ");

(*Takes a list of places and returns name, description, rating, price, dietary restrictions, and casual atmosphere of each (using printPlace as a helper)*)
fun printLstPlace([]) = [print("")]
  | printLstPlace(a::rest) = printPlace(a)::printLstPlace(rest);

(*---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------*)

(* INSTRUCTIONS *)

print(
"\n  Welcome to our Walla Walla Restaurant Sorter! Our model can be used to sort \nthrough a number of popular restaurants in downtown Walla Walla based on a few\nthings: \n    - sortByRating --> Each restaurant is rated on a scale of 0.0 to 5.0 \n\tstars based on online reviews. \n    - sortByPrice --> The expense expected from visiting the restaurant is \n\trated, from lowest to highest, by $, $$, $$$, or $$$$. \n    - sortByFoodType --> An assortment of food types included in the menu, \n\twhich are Burgers, Sushi, Pizza, Pasta, Sandwiches, Breakfast, Meat, \n\tFish, MexicanFood, Soups, Salads, and Tapas. Note that this is only a \n\tbroad selection of the food offered for the restaurant. \n    - sortByCuisine --> A list of the cuisine of the restaurant, including \n\tJapanese, Italian, American, Mexican, Spanish, and French. \n    - DietaryRestrictions --> If the restaurant has options for those who are \n\tVegetarian, Pescatarian, GlutenFree, or have None. \n    - sortByFanciness --> Each restaurant is given a rating, from lowest to \n\thighest, of Lowkey, Inthemiddle, Restaurant, or Fancy. \n\n  To use these functions you will need to input a list of restaurants (and \npossibly another parameter based on the function). If you would like the list \nof all possible restaurants, use the keyword \"allplaces\". Otherwise, each \nrestaurant uses the following abbreviations:\n - SMK = Saffron Mediterranean Kitchen\n - BF = Brasserie Four\n - TM = T Maccarone's\n - C = Clarette's\n - WHC = Whitehouse Crawford\n - G = Graze\n - ES = El Sombrero Restaurant\n - O = Olive Marketplace and Cafe\n - TMR = The Marc Restaurant\n - GK = Grandma's Kitchen\n - WWBC = The Walla Walla Bread Company\n - SB = Sweet Basil Pizzeria\n - WB = Wingman Birdz & Brewz\n - MCB = Mill Creek Brewpub\n - SHS = Shiki Hibachi Sushi\n - OaC = Ox and Cart\n\n  Enjoy your browsing! \n\n");
