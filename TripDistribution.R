closeAllConnections()
rm(list=ls())
setwd("C:\\Users\\benny\\OneDrive\\Desktop\\Academic Work\\20202021\\ORF478\\ProjectWork\\csv")
set.seed(2020)

TAZ <- data.frame(read.csv(file = 'TAZ Data - Sheet1.csv'))
num_rows = nrow(TAZ)
zone_names <- data.frame(read.csv(file = 'TAZ Data - Sheet2.csv', header = FALSE))

# population info/demographics
tp = 500000 #total population
under_five = 4.5/100
under_18 = 13.4/100
college_students = 20000/500000
over_65 = 15.1/100
demographics = c(under_five, under_18-under_five,college_students, 
                 1 - (under_18+over_65+college_students), over_65 )
unemployment = 0.082
working_pop = demographics[4]*(1-unemployment)*tp
# inverse of ^
unemployed = demographics[4]*(unemployment)*tp

pixel_marketcap = c()
for (i in seq(12)){
  pixel_marketcap[i] = sum(TAZ[which(TAZ[,2]==i) ,3])
}

equalizer = function(production, attraction){
  # returns attraction that is equal to the size of the production
  psize = sum(production)
  asize = sum(attraction)
  if(psize == asize) {return(attraction)}
  else {
    disparity = psize - asize
    while(TRUE){
    i = sample(1:length(attraction), 1)
    if (attraction[i] != 0){
      attraction[i] = attraction[i]+disparity
      return(attraction)
    }
    }
  }
}
#-------------------------------------------------------------------------------
# row 1
#-------------------------------------------------------------------------------
P_home_to_work = vector("double", num_rows)
A_home_to_work = vector("double", num_rows)
j = 0
for (i in TAZ[,2]){
  j = j + 1
  if(i == 6) P_home_to_work[j] = round(TAZ[j,3]*(working_pop/pixel_marketcap[6]))
  if(i == 10 | i == 11) A_home_to_work[j]   = round(TAZ[j,3]*working_pop
                                                    /(pixel_marketcap[10]+pixel_marketcap[11]))
}
A_home_to_work = equalizer(P_home_to_work,A_home_to_work)
row1 = data.frame(P_home_to_work,A_home_to_work)
write.csv(row1, file = "row1.csv")
#-------------------------------------------------------------------------------
# row 2
#-------------------------------------------------------------------------------
P_home_to_school = vector("double", num_rows)
A_home_to_school = vector("double", num_rows)
j = 0
for (i in TAZ[,2]){
  j = j + 1
  if(i == 6) P_home_to_school[j] = round(TAZ[j,3]*
                                           ((demographics[2]+demographics[3])*tp
                                                   /pixel_marketcap[6]))
  if(i == 5) A_home_to_school[j] =round(TAZ[j,3]*demographics[2]*tp
                                                 /pixel_marketcap[5])
  if(i == 4) A_home_to_school[j] =round(TAZ[j,3]*demographics[3]*tp
                                        /pixel_marketcap[4])
}
A_home_to_school = equalizer(P_home_to_school, A_home_to_school)
row2 = data.frame(P_home_to_school,A_home_to_school)
write.csv(row2, file = "row2.csv")
#-------------------------------------------------------------------------------
# row 3
#-------------------------------------------------------------------------------
p_3_ue = vector("double", num_rows)
a_3_ue = vector("double", num_rows)
p_3_e = vector("double", num_rows)
a_3_e = vector("double", num_rows)
shoppers_ue = 1*(unemployed+demographics[5]*tp)
      
shoppers_e = 0.3*(demographics[2]+demographics[3])*tp
            +0.5*working_pop
j = 0
for (i in TAZ[,2]){
  j = j + 1
  if(i == 6) {
    p_3_ue[j] = round(shoppers_ue
                         *(TAZ[j,3]/pixel_marketcap[6]))
    p_3_e[j] = round(shoppers_e*TAZ[j,3]/pixel_marketcap[6])
  
  }
}
# recreational distribution of adults 
# aka "proportioned to the attractiveness of the zone"
rec_dist_3 = c(0,0.07,0.10,0,0,0,0.13,0.06,0.56,0,0.06,0.02)
j=0
for (i in TAZ[,2]){
  j = j + 1
  a_3_ue[j] = round(rec_dist_3[i]*shoppers_ue*TAZ[j,3]/pixel_marketcap[i])
  a_3_e[j] = round(rec_dist_3[i]*shoppers_e*TAZ[j,3]/pixel_marketcap[i])
}
a_3_e = equalizer(p_3_e, a_3_e)
a_3_ue = equalizer(p_3_ue, a_3_ue)
row3_unemployed = data.frame(p_3_ue, a_3_ue)
row3_employed = data.frame(p_3_e, a_3_e)

write.csv(row3_unemployed, file = "row3_unemployed_homebased_shopping.csv")
write.csv(row3_employed, file = "row3_employed_homebased_shopping.csv")
#-------------------------------------------------------------------------------
# row 4
#-------------------------------------------------------------------------------
p_4 = round(A_home_to_work * 0.60)
a_4 = vector("double", num_rows)
j = 0
for (i in TAZ[,2]){
  j = j + 1
  if(i == 6) {a_4[j] = round(TAZ[j,3]*0.6*working_pop/pixel_marketcap[6])
  }
}
a_4 = equalizer(p_4, a_4)
row4 = data.frame(p_4,a_4)
write.csv(row4, file ="row4.csv")
#-------------------------------------------------------------------------------
# row 5
#-------------------------------------------------------------------------------
p_5 = round(A_home_to_school * 0.75)
a_5 = vector("double", num_rows)
j = 0
for (i in TAZ[,2]){
  j = j + 1
  if(i == 6) {a_5[j] = 
    round(TAZ[j,3]*0.75*(demographics[2]+demographics[3])*tp/pixel_marketcap[6])
  }
}
a_5 = equalizer(p_5, a_5)
row5 = data.frame(p_5,a_5)
write.csv(row5, file ="row5.csv")
#-------------------------------------------------------------------------------
# row 6
#-------------------------------------------------------------------------------
p_6_e = round(0.50 * a_3_e)
p_6_ue = round(0.50 * a_3_ue)
a_6_e = vector("double", num_rows)
a_6_ue = vector("double", num_rows)
j = 0
for (i in TAZ[,2]){
  j = j + 1
  if(i == 6) {
    a_6_e[j] = 
    round(TAZ[j,3]*0.50*shoppers_e/pixel_marketcap[6])
    a_6_ue[j] = 
      round(TAZ[j,3]*0.50*shoppers_ue/pixel_marketcap[6])
  }
}
a_6_e = equalizer(p_6_e, a_6_e)
a_6_ue = equalizer(p_6_ue, a_6_ue)
row6_e = data.frame(p_6_e,a_6_e)
row6_ue = data.frame(p_6_ue,a_6_ue)

write.csv(row6_e, file ="row6_homebound_employed_shoppers.csv")
write.csv(row6_ue, file ="row6_homebound_unemployed_shoppers.csv")
#-------------------------------------------------------------------------------
# row 7
#-------------------------------------------------------------------------------
#lunch
p_7_0_lunch = round(A_home_to_work * 0.10)
a_7_0_lunch = vector("double", num_rows)
j = 0
for (i in TAZ[,2]){
  j = j + 1
  if(i == 9) {a_7_0_lunch[j] = 
    round(TAZ[j,3]*0.10*working_pop/pixel_marketcap[9])
  }
}
a_7_0_lunch = equalizer(p_7_0_lunch, a_7_0_lunch)
going_to_lunch = data.frame(p_7_0_lunch, a_7_0_lunch)
write.csv(going_to_lunch, file ="row7_goingtolunch.csv")

p_7_1_lunch = a_7_0_lunch
a_7_1_lunch = p_7_0_lunch #everyone goes back to work
coming_from_lunch = data.frame(p_7_1_lunch, a_7_1_lunch)
write.csv(coming_from_lunch, file ="row7_comingfromlunch.csv")

p_7_afterwork = round(0.40*A_home_to_work)
#MAKE Attraction and send them home
a_7_afterwork = vector("double", num_rows)
afterwork_pop = sum(p_7_afterwork)
rec_dist_7 = c(0,0.10,0.13,0,0,0,0.04,0.15,0.56,0,0.00,0.02)
j=0
for (i in TAZ[,2]){
  j = j + 1
  a_7_afterwork[j] = round(rec_dist_7[i]*afterwork_pop*TAZ[j,3]/pixel_marketcap[i])
}
a_7_afterwork = equalizer(p_7_afterwork, a_7_afterwork)
afterwork_recreate = data.frame(p_7_afterwork, a_7_afterwork)
write.csv(afterwork_recreate, file = "row7_afterwork_recreate.csv")

#go home
p_7_backhome = a_7_afterwork
a_7_backhome = vector("double", num_rows)
j=0
for (i in TAZ[,2]){
  j = j + 1
  if(i==6){a_7_backhome[j] = round(afterwork_pop*TAZ[j,3]/pixel_marketcap[i])}
}
a_7_backhome = equalizer(p_7_backhome, a_7_backhome)
cominghome = data.frame(p_7_backhome, a_7_backhome)
write.csv(cominghome, file = "row7_coming_home_after_recreate.csv")
#-------------------------------------------------------------------------------
# row 8
#-------------------------------------------------------------------------------
p_8 = round(0.25*A_home_to_school)
a_8 = vector("double", num_rows)
#a percentage weighted portfolio of recreation demand...size 12
rec_dist_8 = c(0,0.04,0.13,0,0,0,0.23,0.01,0.52,0,0.02,0.05) 
aep = sum(p_8)# the number of kids doing extracurricular
j=0
for (i in TAZ[,2]){
  j = j + 1
  a_8[j] = round(rec_dist_8[i]*aep*TAZ[j,3]/pixel_marketcap[i])
}
a_8 = equalizer(p_8,a_8)
afterschool_extracurricular = data.frame(p_8,a_8)
write.csv(afterschool_extracurricular, file = "row8_afterschool_extracurricular.csv")

#send everyone home
p_8_backhome = a_8
a_8_backhome = vector("double", num_rows)
j=0
for (i in TAZ[,2]){
  j = j + 1
  if(i==6){a_8_backhome[j] = round(aep*TAZ[j,3]/pixel_marketcap[i])}
}
a_8_backhome = equalizer(p_8_backhome, a_8_backhome)
goinghome_after_extracurricular = data.frame(p_8_backhome, a_8_backhome)
write.csv(goinghome_after_extracurricular, file = "row8_goinghome_extracurricular.csv")
#-------------------------------------------------------------------------------
# row 9 more shopping for shoppers
#-------------------------------------------------------------------------------
p_9_e = a_6_e
p_9_ue = a_6_ue
a_9_e = vector("double", num_rows)
a_9_ue = vector("double", num_rows)

rec_dist_9 = c(0,0.10,0.15,0,0,0,0.15,0.10,0.42,0,0.07,0.01)
j=0
for (i in TAZ[,2]){
  j = j + 1
  a_9_e[j] = round(rec_dist_9[i]*shoppers_e*TAZ[j,3]/pixel_marketcap[i])
  a_9_ue[j] = round(rec_dist_9[i]*shoppers_ue*TAZ[j,3]/pixel_marketcap[i])
}
a_9_e = equalizer(p_9_e, a_9_e)
a_9_ue = equalizer(p_9_ue, a_9_ue)

row9_e = data.frame(p_9_e,a_9_e)
row9_ue = data.frame(p_9_ue,a_9_ue)

write.csv(row9_e, file = "row9_more_shopping_employed.csv")
write.csv(row9_ue, file = "row9_more_shopping_unemployed.csv")

#send home
p_9_home_e = a_9_e
p_9_home_ue = a_9_ue
a_9_home_e = vector("double", num_rows)
a_9_home_ue = vector("double", num_rows)
j=0
for (i in TAZ[,2]){
  j = j + 1
  if(i==6){
    a_9_home_e[j] = round(shoppers_e*TAZ[j,3]/pixel_marketcap[i])
    a_9_home_ue[j] = round(shoppers_ue*TAZ[j,3]/pixel_marketcap[i])
  }
}
a_9_home_e = equalizer(p_9_home_e, a_9_home_e)
a_9_home_ue = equalizer(p_9_home_ue, a_9_home_ue)

cominghome_e = data.frame(p_9_home_e, a_9_home_e)
cominghome_ue = data.frame(p_9_home_ue, a_9_home_ue)
write.csv(cominghome_e, file = "row9_employed_homebound_shopping.csv")
write.csv(cominghome_ue, file = "row9_unemployed_homebound_shopping.csv")