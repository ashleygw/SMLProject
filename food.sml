(*categorizing each restaurant
SMK = ??, ??
BF = ??, fancy
TM = Italian, American, fancy - ****, $$$
C = breakfast, restaurant
WC = Italian, "Contemporary American", fancy - ****, $$$$
G = sandwiches, soups, cafe - ****
ES = Mexican, restaurant
O = Italian, sandwiches, Mexican, cafe
TMR = ??, restaurant
GK = Mexican, inthemiddle - ****, $
WWBC = ?? - ****
SB = pizza, inthemiddle - ****
WB = ??, restaurant
MCB = burgers, Mexican, sandwiches, inthemiddle - ***, $$
SHS = Japanese, sushi, restaurant - ****
OaC = "Contemporary American", Italian, restaurant - ****, $$
*)


datatype restaurantName = Saffron_Mediterranean_Kitchen | Brasserie_Four | T_Maccarones | Clarettes | Whitehouse_Crawford | Graze | El_Sombrero | Olive | The_Marc_Restaurant | Grandmas_Kitchen | Walla_Walla_Bread_Company | Sweet_Basil | Wingman_Birdz | Mill_Creek_Brewpub | Shiki_Hibachi_Sushi | Ox_and_Cart;

datatype casual = cafe | inthemiddle | restaurant | fancy;
datatype foodType = Burgers | Sushi | Pizza | Pasta | Sandwiches | Breakfast | Meat | Fish | Vegetarian;
datatype restrictions = Vegetarian | Pescatarian | GlutenFree | None;
datatype cuisine = Japanese | Italian | American | Mexican;
datatype price = $ | $$ | $$$ | $$$$;
datatype rating = * | ** | *** | **** | *****;

datatype categorizePlace = Place of restaurantName * casual * foodType list * restrictions list * cuisine list * price * rating;

val TM = Place(T_Maccarones, fancy, [Pizza, Pasta, Meat, Fish], [Pescatarian, None], [Italian, American], $$$, ****);

fun sortByFanciness
fun sortByFoodType
fun anyRestrictions
fun sortByCuisine
fun sortByPrice
fun sortByRating
fun returnPlace
