#####Neural Network
set <- readRDS("clean_set")
set$meterprice <- NULL
set$sq_dif <- NULL
library(neuralnet)
n <- names(new)
f <- as.formula(paste("price_doc ~", paste(n[!n %in% "price_doc"], collapse = " + ")))
install.packages("dummies")
library(dummies)
div <- sample(1:dim(set)[1],25000)
train <- set[div,]
tst <- set[-div,]
new <- dummy.data.frame(trainp1, sep = ".")
set$
nn <- neuralnet(price_doc ~full_sq + life_sq + 
                  floor + max_floor + num_room + kitch_sq + 
                  product_type + raion_popul + green_zone_part + children_preschool + 
                  school_quota + school_education_centers_top_20_raion + culture_objects_top_25_raion + 
                  shopping_centers_raion + railroad_terminal_raion + big_market_raion + 
                  full_all + male_f + female_f + young_all + young_male + young_female + 
                  work_all + work_male + work_female + X0_6_male + X0_6_female + 
                  X0_17_all + X0_17_male + X0_17_female + X16_29_all + X16_29_male + 
                  X16_29_female + X0_13_all + X0_13_male + X0_13_female + raion_build_count_with_material_info + 
                  build_count_block + build_count_wood + build_count_frame + 
                  build_count_brick + build_count_monolith + build_count_panel + 
                  build_count_foam + build_count_slag + ID_metro + kindergarten_km + 
                  school_km + industrial_km + railroad_station_avto_min + ID_railroad_station_avto + 
                  public_transport_station_km + water_1line + ttk_km + sadovoe_km + 
                  ID_big_road1 + ID_big_road2 + power_transmission_line_km + 
                  big_market_km + fitness_km + hospice_morgue_km + detention_facility_km + 
                  public_healthcare_km + office_km + church_synagogue_km + 
                  mosque_km + museum_km + catering_km + green_part_500 + prom_part_500 + 
                  cafe_count_500 + cafe_count_500_na_price + cafe_count_500_price_500 + 
                  cafe_count_500_price_1000 + cafe_count_500_price_1500 + cafe_count_500_price_2500 + 
                  leisure_count_500 + green_part_1000 + prom_part_1000 + office_count_1000 + 
                  trc_count_1000 + trc_sqm_1000 + cafe_count_1000 + cafe_count_1000_na_price + 
                  cafe_count_1000_price_500 + cafe_count_1000_price_1000 + 
                  cafe_count_1000_price_1500 + cafe_count_1000_price_2500 + 
                  cafe_count_1000_price_4000 + church_count_1000 + leisure_count_1000 + 
                  market_count_1000 + prom_part_1500 + cafe_count_1500 + cafe_sum_1500_max_price_avg + 
                  cafe_avg_price_1500 + cafe_count_1500_price_1000 + cafe_count_1500_price_4000 + 
                  big_church_count_1500 + green_part_2000 + prom_part_2000 + 
                  office_count_2000 + office_sqm_2000 + trc_count_2000 + trc_sqm_2000 + 
                  cafe_count_2000 + cafe_sum_2000_max_price_avg + cafe_avg_price_2000 + 
                  cafe_count_2000_na_price + cafe_count_2000_price_500 + cafe_count_2000_price_1000 + 
                  cafe_count_2000_price_1500 + cafe_count_2000_price_2500 + 
                  cafe_count_2000_price_4000 + big_church_count_2000 + green_part_3000 + 
                  prom_part_3000 + office_sqm_3000 + trc_sqm_3000 + cafe_count_3000 + 
                  cafe_sum_3000_min_price_avg + cafe_sum_3000_max_price_avg + 
                  cafe_avg_price_3000 + cafe_count_3000_na_price + cafe_count_3000_price_500 + 
                  cafe_count_3000_price_1000 + cafe_count_3000_price_1500 + 
                  cafe_count_3000_price_2500 + cafe_count_3000_price_4000 + 
                  church_count_3000 + market_count_3000 + green_part_5000 + 
                  prom_part_5000 + trc_count_5000 + cafe_count_5000 + cafe_sum_5000_min_price_avg + 
                  cafe_sum_5000_max_price_avg + cafe_avg_price_5000 + cafe_count_5000_price_1000 + 
                  cafe_count_5000_price_4000 + leisure_count_5000 + market_count_5000 + 
                  buildage+ healthcare_ratio + sport_objects_ratio + 
                  shopping_centers_ratio + office_ratio + months, data=train, hidden=c(5,3),linear.output=T)
nn_yhat <- predict(nn, tst)
RMSLE_nn <- rmsle(nn_yhat, tst$price_doc)