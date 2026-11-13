#!/usr/bin/env python3
"""Generate menus/*.pl, one file per venue, clauses sorted by (price, sugar).

Venues: 20 Raleigh spots on/near Hillsborough St where NCSU students eat.
Prices (USD), sugar (grams), calories: ESTIMATES for demo purposes.
Calories derived from course/price/sugar bands (crude but plausible).
Realistic menu sizes: ~20-45 items per venue.
"""
import os

def shakes(flavors, price, base_sugar):
    return [(f"{f}_shake", "dessert", price, base_sugar + 4*(i % 4))
            for i, f in enumerate(flavors)]

def subs(names, price, sugar=4):
    return [(n, "main", price + 0.5*(i % 3), sugar + (i % 4))
            for i, n in enumerate(names)]

V = {}

V["cookout"] = ("Cook Out", "3625 Hillsborough St",
  [("hushpuppies","starter",1.99,4),("corn_dog","starter",1.49,6),
   ("quesadilla_chicken","starter",1.99,2),("quesadilla_beef","starter",1.99,2),
   ("chicken_nuggets_5","starter",1.99,0),("white_cheddar_bites","starter",2.49,2),
   ("fries_reg","starter",1.99,0),("fries_cajun","starter",1.99,1),
   ("onion_rings","starter",2.29,5),("slaw","starter",1.29,9),
   ("big_double_burger","main",5.49,9),("cheddar_style_burger","main",5.99,8),
   ("out_west_burger","main",5.99,11),("bbq_plate","main",6.99,12),
   ("bbq_sandwich","main",4.49,10),("chicken_strips_3","main",5.99,5),
   ("spicy_chicken_sandwich","main",4.99,6),("grilled_chicken_sandwich","main",5.29,6),
   ("hot_dog","main",2.49,6),("bacon_cheddar_quesadilla","main",5.49,3),
   ("cheerwine_float","dessert",2.99,55)]
  + shakes(["chocolate","vanilla","strawberry","oreo","reeses","banana",
            "caramel","cheesecake","mint","peach","mocha","blueberry",
            "pineapple","walnut","snickers","mm","heath","fudge"], 3.99, 62))

V["chargrill"] = ("Char-Grill", "618 Hillsborough St",
  [("fries","starter",2.49,0),("slaw","starter",1.99,8),
   ("onion_rings","starter",2.99,5),("chili_cup","starter",3.49,4),
   ("side_salad","starter",3.99,3),("pimento_cheese_fries","starter",4.49,2),
   ("hamburger_steak","main",7.49,6),("charburger","main",5.29,8),
   ("charburger_double","main",7.29,9),("grilled_chicken_sandwich","main",6.49,7),
   ("hot_dog_all_the_way","main",3.29,7),("bbq_sandwich","main",5.49,11),
   ("fish_sandwich","main",6.29,5),("kids_burger","main",3.99,6),
   ("apple_turnover","dessert",2.99,26)]
  + shakes(["chocolate","vanilla","strawberry","pineapple","cherry",
            "oreo","banana","peanut_butter"], 4.29, 58))

V["davids_dumpling"] = ("David's Dumpling & Noodle Bar", "1900 Hillsborough St",
  [("pork_dumplings","starter",6.50,3),("veggie_dumplings","starter",6.00,3),
   ("shrimp_dumplings","starter",7.00,2),("spring_rolls","starter",5.50,4),
   ("summer_rolls","starter",5.95,5),("wonton_soup","starter",4.95,2),
   ("hot_sour_soup","starter",4.95,3),("miso_soup","starter",3.95,2),
   ("edamame","starter",4.50,1),("scallion_pancake","starter",5.95,2),
   ("cucumber_salad","starter",4.95,6),("crab_rangoon","starter",6.50,7),
   ("dan_dan_noodles","main",11.95,8),("pad_thai_chicken","main",12.50,14),
   ("pad_thai_tofu","main",11.50,14),("pad_thai_shrimp","main",13.95,14),
   ("curry_chicken","main",12.95,9),("curry_veggie","main",11.50,9),
   ("lo_mein_pork","main",11.95,7),("lo_mein_veggie","main",10.95,7),
   ("singapore_noodles","main",12.95,5),("drunken_noodles","main",12.95,10),
   ("ramen_pork_belly","main",13.95,6),("fried_rice_chicken","main",10.95,4),
   ("fried_rice_shrimp","main",12.95,4),("mapo_tofu","main",11.95,5),
   ("general_tso_chicken","main",12.95,22),("sesame_chicken","main",12.95,24),
   ("fried_banana","dessert",5.50,24),("mango_sticky_rice","dessert",6.50,28),
   ("green_tea_ice_cream","dessert",4.50,19)])

V["gonza"] = ("Gonza Tacos y Tequila", "2100 Hillsborough St",
  [("chips_salsa","starter",4.50,3),("guacamole","starter",7.95,2),
   ("queso_fundido","starter",7.50,3),("elote","starter",5.50,6),
   ("sopa_tortilla","starter",6.50,4),("ceviche","starter",9.95,3),
   ("ensalada_casa","starter",6.95,5),("empanadas_2","starter",7.50,3),
   ("tacos_al_pastor","main",12.95,7),("tacos_carnitas","main",12.95,5),
   ("tacos_pollo","main",11.95,4),("tacos_pescado","main",13.95,5),
   ("tacos_veggie","main",10.95,6),("enchiladas_verdes","main",13.95,6),
   ("enchiladas_mole","main",14.50,11),("burrito_gonza","main",12.50,5),
   ("burrito_veggie","main",11.50,5),("quesadilla_grande","main",11.95,4),
   ("fajitas_chicken","main",15.95,8),("fajitas_steak","main",17.95,8),
   ("torta_cubana","main",12.95,7),("pozole","main",12.50,4),
   ("churros","dessert",6.95,32),("flan","dessert",6.50,29),
   ("tres_leches","dessert",7.50,38),("sopapillas","dessert",5.95,26)])

V["jasmin"] = ("Jasmin Mediterranean Bistro", "2430 Hillsborough St",
  [("hummus","starter",5.49,1),("baba_ghanoush","starter",5.99,2),
   ("falafel_4pc","starter",4.99,2),("lentil_soup","starter",4.49,3),
   ("tabouli","starter",4.99,2),("grape_leaves_4","starter",4.99,3),
   ("greek_salad_sm","starter",5.49,4),("feta_olives","starter",4.49,1),
   ("spanakopita","starter",5.99,2),("fried_cauliflower","starter",5.49,3),
   ("chicken_shawarma_plate","main",11.99,4),("beef_shawarma_plate","main",12.99,4),
   ("gyro_wrap","main",8.99,5),("chicken_shawarma_wrap","main",8.49,4),
   ("falafel_wrap","main",7.99,4),("kofta_kabob","main",12.99,3),
   ("chicken_kabob_plate","main",12.49,3),("lamb_gyro_plate","main",13.49,5),
   ("veggie_plate","main",10.99,5),("moussaka","main",12.99,8),
   ("baklava","dessert",3.49,21),("rice_pudding","dessert",3.99,24),
   ("kunafa","dessert",5.49,31)])

V["mitchs"] = ("Mitch's Tavern", "2426 Hillsborough St",
  [("chili_cup","starter",4.50,4),("chili_bowl","starter",6.50,6),
   ("side_salad","starter",4.00,5),("potato_skins","starter",7.50,2),
   ("nachos","starter",8.50,4),("fried_pickles","starter",6.00,3),
   ("pretzel_bites","starter",6.50,5),("soup_of_day","starter",4.50,3),
   ("reuben","main",10.50,6),("tavern_burger","main",9.50,7),
   ("mushroom_swiss_burger","main",10.50,7),("chicken_philly","main",10.00,6),
   ("beef_philly","main",10.50,6),("grilled_cheese_tomato_soup","main",8.50,9),
   ("french_dip","main",10.95,4),("blt","main",8.00,5),
   ("turkey_club","main",9.50,7),("veggie_wrap","main",8.50,6),
   ("fish_sandwich","main",9.95,5),("caesar_chicken_salad","main",10.00,4),
   ("brownie","dessert",4.00,38),("apple_pie","dessert",4.50,32),
   ("ice_cream_scoop","dessert",2.50,17)])

V["players_retreat"] = ("Player's Retreat", "105 Oberlin Rd",
  [("pr_wings_6","starter",8.50,2),("pr_wings_12","starter",14.50,4),
   ("pimento_cheese","starter",6.50,3),("fried_green_tomatoes","starter",7.50,5),
   ("deviled_eggs","starter",5.50,1),("hushpuppy_basket","starter",4.50,8),
   ("she_crab_soup","starter",7.00,3),("house_salad","starter",5.50,4),
   ("oysters_half_dozen","starter",12.00,1),
   ("pr_burger","main",11.50,7),("mushroom_burger","main",12.50,7),
   ("fish_and_chips","main",13.50,4),("shrimp_and_grits","main",14.50,3),
   ("club_sandwich","main",10.50,8),("hot_roast_beef","main",12.00,5),
   ("chicken_sandwich","main",10.95,6),("ribeye_sandwich","main",13.95,4),
   ("crab_cake_sandwich","main",13.50,5),("veggie_burger","main",10.50,8),
   ("caesar_salmon_salad","main",14.00,3),
   ("pecan_pie","dessert",6.00,41),("key_lime_pie","dessert",6.00,33),
   ("chocolate_cake","dessert",6.50,46),("bread_pudding","dessert",6.00,37)])

V["snoopys"] = ("Snoopy's Hot Dogs", "2431 Hillsborough St",
  [("fries","starter",2.29,0),("cheese_fries","starter",3.29,1),
   ("chili_cheese_fries","starter",3.79,2),("onion_rings","starter",3.29,4),
   ("slaw_cup","starter",1.79,7),("chips","starter",1.29,1),
   ("hot_dog_all_the_way","main",2.99,7),("hot_dog_plain","main",2.49,5),
   ("double_dog_combo","main",6.99,10),("burger","main",4.99,8),
   ("double_burger","main",6.49,9),("cheeseburger_combo","main",7.99,11),
   ("chicken_tenders_combo","main",7.49,6),("bbq_sandwich","main",4.49,10),
   ("grilled_cheese","main",2.99,4),("corn_dog","main",2.29,8)])

V["two_guys"] = ("Two Guys Grill", "2504 Hillsborough St",
  [("greek_salad_sm","starter",4.95,4),("mozzarella_sticks","starter",5.95,3),
   ("garlic_bread","starter",3.50,2),("fried_mushrooms","starter",5.50,3),
   ("wings_6","starter",7.95,2),("chef_salad_sm","starter",5.50,4),
   ("french_fries","starter",2.95,0),("feta_fries","starter",4.50,1),
   ("gyro_platter","main",10.95,6),("souvlaki_platter","main",11.95,5),
   ("spaghetti","main",9.95,12),("lasagna","main",11.50,13),
   ("pizza_slice_combo","main",7.95,8),("pizza_sm_cheese","main",9.95,10),
   ("pizza_sm_special","main",12.95,11),("stromboli","main",9.95,8),
   ("italian_sub","main",8.95,5),("philly_cheese_steak","main",9.50,6),
   ("hamburger_deluxe","main",8.50,9),("chicken_parm","main",11.95,14),
   ("eggplant_parm","main",10.95,13),("fish_plate","main",10.50,4),
   ("baklava","dessert",3.95,22),("cheesecake","dessert",4.50,27),
   ("chocolate_cake","dessert",4.50,42)])

V["jimmy_johns"] = ("Jimmy John's", "2316 Hillsborough St",
  [("chips_reg","starter",1.89,1),("chips_bbq","starter",1.89,3),
   ("chips_jalapeno","starter",1.89,1),("chips_salt_vinegar","starter",1.89,1),
   ("pickle_whole","starter",1.59,1),("pickle_half","starter",0.99,1)]
  + subs(["turkey_tom","big_john","totally_tuna","vito","pepe",
          "veggie_sub","club_lulu","italian_night_club","beach_club",
          "bootlegger_club","hunters_club","country_club","gargantuan",
          "jj_blt","billy_club","ultimate_porker"], 7.49)
  + [("chocolate_chip_cookie","dessert",2.09,24),
     ("oatmeal_raisin_cookie","dessert",2.09,20)])

V["chipotle"] = ("Chipotle", "2604 Hillsborough St",
  [("chips_guac","starter",4.95,1),("chips_queso","starter",4.45,2),
   ("chips_salsa","starter",2.95,2),("side_tortilla","starter",0.75,0),
   ("side_guac","starter",2.95,0),("side_queso","starter",1.95,1),
   ("chicken_bowl","main",9.95,4),("steak_bowl","main",11.45,4),
   ("carnitas_bowl","main",10.45,3),("barbacoa_bowl","main",11.45,4),
   ("sofritas_bowl","main",9.95,6),("veggie_bowl","main",9.45,6),
   ("chicken_burrito","main",9.95,5),("steak_burrito","main",11.45,5),
   ("carnitas_burrito","main",10.45,4),("sofritas_burrito","main",9.95,7),
   ("chicken_tacos_3","main",9.95,3),("steak_tacos_3","main",11.45,3),
   ("chicken_salad","main",9.95,9),("sofritas_salad","main",9.95,9),
   ("chicken_quesadilla","main",10.95,2),("steak_quesadilla","main",12.45,2)])

V["bojangles"] = ("Bojangles", "2524 Hillsborough St",
  [("bo_rounds","starter",2.49,1),("green_beans","starter",2.79,3),
   ("mashed_potatoes","starter",2.79,1),("dirty_rice","starter",2.79,1),
   ("mac_n_cheese","starter",2.99,2),("coleslaw","starter",2.49,10),
   ("grits","starter",1.99,0),("picnic_fries","starter",2.99,0),
   ("plain_biscuit","starter",1.29,2),
   ("cajun_filet_biscuit","main",5.49,4),("sausage_biscuit","main",3.29,2),
   ("bacon_egg_cheese_biscuit","main",4.49,3),("steak_biscuit","main",3.99,3),
   ("two_pc_dinner_leg_thigh","main",7.99,2),("three_pc_dinner","main",9.49,3),
   ("four_pc_supremes_combo","main",8.99,4),("eight_pc_tailgate","main",21.99,6),
   ("chicken_rice_bowl","main",6.99,3),("cajun_filet_sandwich","main",5.99,6),
   ("grilled_chicken_sandwich","main",5.99,5),
   ("bo_berry_biscuit","dessert",1.99,14),("sweet_potato_pie","dessert",2.29,20),
   ("cinnamon_biscuit","dessert",1.99,17)])

V["insomnia"] = ("Insomnia Cookies", "2302 Hillsborough St",
  [("cookie_dough_bites","starter",4.95,35),("warm_pretzel","starter",3.95,4),
   ("cookiewich","main",5.95,42),("six_pack_classic","main",14.95,108),
   ("classic_chocolate_chunk","dessert",2.45,18),("classic_sugar","dessert",2.45,17),
   ("classic_snickerdoodle","dessert",2.45,18),("classic_double_chocolate","dessert",2.45,20),
   ("classic_oatmeal_raisin","dessert",2.45,16),("classic_peanut_butter","dessert",2.45,15),
   ("deluxe_smores","dessert",3.95,29),("deluxe_chocolate_peanut_butter","dessert",3.95,28),
   ("brownie","dessert",3.45,33),("blondie","dessert",3.45,30),
   ("scoop_vanilla","dessert",2.95,16),("scoop_chocolate","dessert",2.95,18)])

V["jubala"] = ("Jubala Coffee", "2100 Hillsborough St",
  [("fruit_cup","starter",3.50,11),("granola_yogurt","starter",5.50,16),
   ("grits_bowl_side","starter",3.50,1),("toast_butter","starter",2.50,2),
   ("seasonal_fruit_toast","starter",4.50,9),
   ("biscuit_egg_cheese","main",6.50,3),("biscuit_sausage_egg","main",7.50,3),
   ("biscuit_fried_chicken","main",8.00,5),("avocado_toast","main",8.00,2),
   ("almond_butter_toast","main",7.00,8),("pancake","main",7.00,18),
   ("breakfast_bowl","main",8.50,3),("blt_sandwich","main",8.00,4),
   ("turkey_pesto_sandwich","main",9.00,4),
   ("hand_pie","dessert",4.50,23),("cinnamon_roll","dessert",4.75,27),
   ("muffin","dessert",3.75,22),("scone","dessert",3.75,15)])

V["benny_capitales"] = ("Benny Capitale's Pizza", "2325 Hillsborough St",
  [("garlic_knots_4","starter",3.50,2),("caesar_side","starter",4.50,2),
   ("house_salad","starter",4.95,3),("mozzarella_sticks","starter",5.95,3),
   ("fried_calamari","starter",8.95,2),("garlic_bread","starter",3.25,2),
   ("jumbo_cheese_slice","main",4.25,5),("jumbo_pepperoni_slice","main",4.75,5),
   ("jumbo_sausage_slice","main",4.75,5),("jumbo_veggie_slice","main",4.95,6),
   ("jumbo_buffalo_chicken_slice","main",5.25,6),("sicilian_slice","main",4.50,5),
   ("calzone_cheese","main",9.50,7),("calzone_meat","main",10.95,7),
   ("stromboli","main",10.50,8),("chicken_parm_sub","main",9.95,9),
   ("meatball_sub","main",9.50,10),("baked_ziti","main",10.50,12),
   ("spaghetti_meatballs","main",11.50,13),("lasagna","main",11.95,13),
   ("cannoli","dessert",4.50,19),("zeppole","dessert",4.95,24),
   ("ny_cheesecake","dessert",5.50,28)])

V["lime_lemon"] = ("Lime & Lemon Indian Grill", "2422 Hillsborough St",
  [("samosa_2","starter",5.99,3),("papdi_chaat","starter",6.99,8),
   ("samosa_chaat","starter",7.99,9),("gobi_manchurian","starter",9.99,10),
   ("chicken_65","starter",10.99,4),("onion_pakora","starter",6.99,3),
   ("paneer_pakora","starter",8.99,3),("garlic_naan","starter",3.99,1),
   ("plain_naan","starter",2.99,1),("mango_lassi","starter",4.99,28),
   ("butter_chicken","main",14.99,12),("chicken_tikka_masala","main",14.99,11),
   ("chana_masala","main",12.99,7),("palak_paneer","main",13.99,6),
   ("paneer_butter_masala","main",13.99,12),("veg_biryani","main",13.49,6),
   ("chicken_biryani","main",14.99,6),("goat_biryani","main",16.99,5),
   ("lamb_rogan_josh","main",16.99,7),("dal_tadka","main",11.99,4),
   ("veg_korma","main",12.99,10),("tandoori_chicken_half","main",14.99,3),
   ("gulab_jamun","dessert",4.99,36),("rasmalai","dessert",5.99,32),
   ("kheer","dessert",4.99,27)])

V["jersey_mikes"] = ("Jersey Mike's", "3100 Hillsborough St",
  [("chips_reg","starter",1.99,1),("chips_bbq","starter",1.99,3),
   ("drink_zero","starter",2.19,0),("pickle_spear","starter",0.99,1)]
  + subs(["turkey_provolone","club_sub","club_supreme","original_italian",
          "blt","veggie","ham_provolone","roast_beef_provolone","tuna",
          "philly_cheese_steak","chipotle_cheese_steak","buffalo_chicken",
          "chicken_philly","portabella_cheese_steak","big_kahuna"], 9.45)
  + [("brownie","dessert",2.99,30),("chocolate_chip_cookie","dessert",1.59,19)])

V["neomonde"] = ("Neomonde Mediterranean", "3817 Beryl Rd",
  [("taboule","starter",4.75,2),("grape_leaves_4","starter",5.25,3),
   ("kibbeh_1","starter",5.50,1),("hummus_sm","starter",4.50,1),
   ("baba_ghanouj_sm","starter",4.95,2),("lentil_soup","starter",4.50,3),
   ("fattoush_sm","starter",5.25,4),("falafel_4","starter",4.95,2),
   ("spinach_pie","starter",3.50,2),("meat_pie","starter",3.75,2),
   ("zaatar_bread","starter",2.95,1),("cheese_bread","starter",3.50,1),
   ("chicken_kabob_plate","main",12.25,4),("beef_kabob_plate","main",13.75,4),
   ("kafta_plate","main",12.50,3),("shawarma_plate","main",12.95,4),
   ("falafel_plate","main",10.25,5),("veggie_combo_plate","main",10.95,6),
   ("shawarma_wrap","main",8.75,3),("chicken_tawook_wrap","main",8.50,3),
   ("falafel_wrap","main",7.50,4),("kafta_wrap","main",8.50,3),
   ("namoura","dessert",3.25,18),("baklava_2","dessert",4.25,22),
   ("maamoul_2","dessert",3.95,16),("rice_pudding","dessert",3.75,23)])

V["wendys"] = ("Wendy's", "3920 Western Blvd",
  [("small_chili","starter",2.89,5),("large_chili","starter",3.89,7),
   ("side_salad","starter",2.49,3),("fries_sm","starter",2.19,0),
   ("fries_med","starter",2.79,0),("baked_potato_plain","starter",2.99,3),
   ("baked_potato_cheese","starter",3.99,4),("nuggets_6","starter",3.49,0),
   ("daves_single","main",6.79,9),("daves_double","main",8.49,10),
   ("baconator","main",8.99,9),("jr_bacon_cheeseburger","main",3.49,6),
   ("spicy_chicken_sandwich","main",6.99,6),("classic_chicken_sandwich","main",6.49,6),
   ("grilled_chicken_wrap","main",5.99,4),("asiago_club","main",7.49,7),
   ("apple_pecan_chicken_salad","main",8.19,27),("taco_salad","main",8.19,13),
   ("frosty_sm_chocolate","dessert",2.69,46),("frosty_sm_vanilla","dessert",2.69,43),
   ("frosty_med","dessert",3.29,65)])

V["taco_bell"] = ("Taco Bell", "3401 Hillsborough St",
  [("chips_cheese","starter",2.49,1),("chips_guac","starter",2.99,1),
   ("black_beans_rice","starter",2.29,1),("cheesy_fiesta_potatoes","starter",2.99,2),
   ("pintos_n_cheese","starter",2.29,1),("mexican_rice","starter",1.99,0),
   ("crunchy_taco","main",1.89,1),("soft_taco","main",1.99,2),
   ("doritos_locos_taco","main",2.59,1),("crunchy_taco_supreme","main",2.79,2),
   ("bean_burrito","main",2.79,3),("burrito_supreme","main",5.49,5),
   ("five_layer_burrito","main",4.29,4),("crunchwrap_supreme","main",5.79,6),
   ("chicken_quesadilla","main",6.29,4),("steak_quesadilla","main",6.79,4),
   ("mexican_pizza","main",5.99,4),("chalupa_supreme","main",4.79,4),
   ("gordita_crunch","main",4.59,5),("chicken_power_bowl","main",7.49,3),
   ("nachos_bellgrande","main",6.49,4),("quesarito","main",4.99,5),
   ("cinnamon_twists","dessert",1.99,13),("cinnabon_delights_4","dessert",2.99,27),
   ("cinnabon_delights_12","dessert",7.49,81)])

os.makedirs("menus", exist_ok=True)
assert len(V) == 20, len(V)
total = 0
for venue,(pretty,addr,items) in V.items():
    def cal(course, price, sugar):
        if course == "starter": c = 40*price + 150
        elif course == "main":  c = 80*price + 200
        else:                   c = 25*price + 9*sugar
        return int(round(min(max(c, 80), 1200), -1))
    items = sorted(items, key=lambda it: (it[2], it[3]))  # price, then sugar
    total += len(items)
    with open(f"menus/{venue}.pl","w") as f:
        f.write(f"% {pretty} -- {addr}\n")
        f.write("% Prices USD, sugar grams: ESTIMATES (demo data).\n")
        f.write("% item(Venue, Item, Course, PriceUSD, SugarG, Calories).\n")
        f.write("% Clauses sorted least to most (price, then sugar).\n")
        f.write(f"venue({venue}, \"{pretty}\").\n")
        for item,course,price,sugar in items:
            f.write(f"item({venue}, {item}, {course}, {price:.2f}, {sugar}, {cal(course,price,sugar)}).\n")
    print(f"{venue:18} {len(items):3} items")
print("total items:", total)
