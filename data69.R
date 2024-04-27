library(haven)

# read data from blocks
b4 <- read_dta("data69/Block - 4 Particulars of living facilities - drinking water, bathroom, sanitation etc - level 4.dta",
               col_select = c('b4_q2',
                              'b4_q16',
                              'b4_q17',
                              'b4_q4',
                              'b4_q5',
                              'b4_q21',
                              'b4_q1',
                              'b4_q23',
                              'b4_q24',
                              'b4_q12',
                              'b4_q10',
                              'b4_q6',
                              'Sector',
                              'Key_hhold',
                              'State_code'))

b3_l2 <- read_dta('data69/Block - 3 Household characteristics - level 2.dta',
               col_select = c('b3_q6',
                              'b3_q7',
                              'b3_q3',
                              'b3_q4',
                              'b3_q13',
                              'b3_q11',
                              'b3_q12',
                              'Sector',
                              'Key_hhold',
                              'State_code'))
b3_l3 <- read_dta('data69/Block - 3  Household characteristics - level 3.dta',
               col_select = c('b3_q17',
                              'Sector',
                              'Key_hhold',
                              'State_code'))

b5 <- read_dta('data69/Block - 5 housing characteristics and micro environment - level 5.dta',
               col_select = c('b5_q9',
                              'b5_q10',
                              'Sector',
                              'Key_hhold',
                              'State_code'))
 # merge data
com_data_69 <- b3_l2 |>
  left_join(b3_l3, join_by('Key_hhold')) |> 
  left_join(b4, join_by('Key_hhold')) |>
  left_join(b5, join_by('Key_hhold'))

# variables names added
com_data_69.1 <- com_data_69 |> 
  transmute(HHID = Key_hhold,
        State_code = as.numeric(State_code.x),
        Sector = as.factor(Sector.x),
        gender_head = as.factor(as.numeric(b3_q4)),
        hh_size = as.numeric(b3_q3),
        highest_edu_m = as.factor(as.numeric(b3_q6)),
        highest_edu_f = as.factor(as.numeric(b3_q7)),
        religion = as.factor(as.numeric(b3_q11)),
        land_possesed = as.factor(as.numeric(b3_q13)),
        total_month_exp = as.numeric(b3_q17),
        source_drinking_water = as.factor(as.numeric(b4_q1)),
        water_sufficient_drink = as.factor(as.numeric(b4_q2)),
        water_sufficient = as.factor(as.numeric(b4_q16)),
        access_to_source = as.factor(as.numeric(b4_q4)),
        distance_source_water = as.factor(as.numeric(b4_q5)),
        water_collector = as.factor(as.numeric(b4_q6)),
        access_bathroom = as.factor(as.numeric(b4_q21)),
        is_stagnant_water = as.factor(as.numeric(b4_q10)),
        method_treat = as.factor(as.numeric(b4_q12)),
        freq_water_supply = as.factor(as.numeric(b4_q17)),
        access_to_latrine = as.factor(as.numeric(b4_q23)),
        type_of_latrine = as.factor(as.numeric(b4_q24)),
        disposal_waste_water = as.factor(as.numeric(b5_q9)),
        garbage_collection = as.factor(as.numeric(b5_q10))) 
write.csv(com_data_69.1, "com_data_69.1.csv" )
# -------------------------------------------------


# ------------------------------------------------
# add state names using codes for mapping
com_data_69.2 <- 
com_data_69.1 %>% 
  mutate(State = factor(case_when(
    State_code == "35" ~ 'Andaman & Nicobar Islands',
    State_code == '28' ~ 'Andhra Pradesh',
    State_code == '12' ~ 'Arunachal Pradesh',
    State_code == '18' ~ 'Assam',
    State_code == '10' ~ 'Bihar',
    State_code == '04' ~ 'Chandigarh',
    State_code == '22' ~ 'Chhattisgarh',
    State_code == '26' ~ 'Dadra & Nagar Haveli',
    State_code == '25' ~ 'Daman & Diu',
    State_code == '07' ~ 'Delhi',
    State_code == '30' ~ 'Goa',
    State_code == '24' ~ 'Gujarat',
    State_code == '06' ~ 'Haryana',
    State_code == '02' ~ 'Himachal Pradesh',
    State_code == '01' ~ 'Jammu & Kashmir',
    State_code == '20' ~ 'Jharkhand',
    State_code == '29' ~ 'Karnataka',
    State_code == '32' ~ 'Kerala',
    State_code == '31' ~ 'Lakshadweep',
    State_code == '23' ~ 'Madhya Pradesh',
    State_code == '27' ~ 'Maharashtra',
    State_code == '14' ~ 'Manipur',
    State_code == '17' ~ 'Meghalaya',
    State_code == '15' ~ 'Mizoram',
    State_code == '13' ~ 'Nagaland',
    State_code == '21' ~ 'Odisha',
    State_code == '34' ~ 'Puducherry',
    State_code == '03' ~ 'Punjab',
    State_code == '08' ~ 'Rajasthan',
    State_code == '11' ~ 'Sikkim',
    State_code == '33' ~ 'Tamil Nadu',
    State_code == '16' ~ 'Tripura',
    State_code == '05' ~ 'Uttarakhand',
    State_code == '09' ~ 'Uttarpradesh',
    State_code == '19' ~ 'West Bengal'
  )),
  high_edu = as.factor(if_else(
    as.numeric(highest_edu_m) > as.numeric(highest_edu_f), 
    highest_edu_m, highest_edu_f)))


# -------------------------------------

# scores and levels

score_data <- com_data_69.2 |> 
  transmute(r1 = as.factor(
    case_when(
      water_sufficient_drink == 1 ~ 1,
      water_sufficient_drink == 2 ~ 0)),
    r2 = as.factor(
      case_when(
        water_sufficient == 1 ~ 1,
        water_sufficient == 2 ~ 0)),
    r3 = case_when(freq_water_supply %in% c(1,2) ~ 1,
                   freq_water_supply == 3 ~ 0.66,
                   freq_water_supply == 4 ~ 0.33,
                   freq_water_supply == 9 ~ 0),
    a1 = case_when(access_to_source == 1 ~ 1,
                   access_to_source %in% c(2,3,4,5) ~ 0.66,
                   access_to_source %in% c(6,7) ~ 0.33,
                   access_to_source == 9 ~ 0),
    a2 = case_when(distance_source_water == 1 ~ 1,
                   distance_source_water == 2 ~ 0.66,
                   distance_source_water == 3 ~ 0.33,
                   distance_source_water %in% c(4:7) ~ 0),
    a3 = case_when(access_bathroom == 1 ~ 1,
                   access_bathroom == 2 ~ 0.66,
                   access_bathroom == 3 ~ 0.33,
                   access_bathroom == 9 ~ 0),
    a4 = case_when(source_drinking_water %in% c(1:4) ~ 1,
                   source_drinking_water %in% c(5) ~ 0.66,
                   source_drinking_water %in% c(6:12) ~ 0.33,
                   source_drinking_water %in% c(19) ~ 0),
    u1 = case_when(access_to_latrine %in% c(1) ~ 1,
                   access_to_latrine %in% c(2:4) ~ 0.66,
                   access_to_latrine %in% c(5) ~ 0.33,
                   access_to_latrine %in% c(9) ~ 0),
    u2 = case_when(type_of_latrine %in% c(1) ~ 1,
                   type_of_latrine %in% c(2) ~ 0.66,
                   type_of_latrine %in% c(3:6) ~ 0.33,
                   type_of_latrine %in% c(7:10) ~ 0),
    u3 = case_when(method_treat %in% c(1:2) ~ 1,
                   method_treat %in% c(3,4) ~ 0.66,
                   method_treat %in% c(5,6) ~ 0.33,
                   method_treat %in% c(7,9) ~ 0),
    c1 = total_month_exp,
    c2 = high_edu,
    e1 = if_else(is_stagnant_water == 1, 1, 0),
    e2 = disposal_waste_water,
    e3 = garbage_collection
  )
