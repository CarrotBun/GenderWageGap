
load("modified_data.RData")

# find the quartiles of va previous, the value of log productivity
# for the employers in period -1 (the year before the reference period).

quartiles_va_prv = as.data.frame(quantile(wage_df$va_previous))
colnames(quartiles_va_prv) = "Quartiles"
pQ1 = quartiles_va_prv["25%", ] 
pQ2 = quartiles_va_prv["50%", ]
pQ3 = quartiles_va_prv["75%", ]

wage_df[ ,c("va_prv_Q1", "va_prv_Q2", "va_prv_Q3", "va_prv_Q4")] = rep(NA, nrow(wage_df))

for (i in 1:nrow(wage_df)){
  if (wage_df$va_previous[i] <= pQ1){
    wage_df[i, "va_prv_Q1"] = 1
    wage_df[i, "va_prv_Q2"] = 0
    wage_df[i, "va_prv_Q3"] = 0
    wage_df[i, "va_prv_Q4"] = 0
  }
  else if(wage_df$va_previous[i] <= pQ2 & wage_df$va_previous[i] > pQ1){
    wage_df[i, "va_prv_Q1"] = 0
    wage_df[i, "va_prv_Q2"] = 1
    wage_df[i, "va_prv_Q3"] = 0
    wage_df[i, "va_prv_Q4"] = 0
  }
  else if(wage_df$va_previous[i] <= pQ3 & wage_df$va_previous[i] > pQ2){
    wage_df[i, "va_prv_Q1"] = 0
    wage_df[i, "va_prv_Q2"] = 0
    wage_df[i, "va_prv_Q3"] = 1
    wage_df[i, "va_prv_Q4"] = 0
  }
  else if(wage_df$va_previous[i] > pQ3){
    wage_df[i, "va_prv_Q1"] = 0
    wage_df[i, "va_prv_Q2"] = 0
    wage_df[i, "va_prv_Q3"] = 0
    wage_df[i, "va_prv_Q4"] = 1
  }
}

N_f = nrow(subset(wage_df, female == 1, female))
N_m = nrow(subset(wage_df, female == 0, female))

quartiles_table = as.data.frame(cbind( 
                        "females" = round(c(nrow(subset(wage_df, female == 1 & va_prv_Q1 == 1,))/N_f,
                                            nrow(subset(wage_df, female == 1 & va_prv_Q2 == 1,))/N_f,
                                            nrow(subset(wage_df, female == 1 & va_prv_Q3 == 1,))/N_f,
                                            nrow(subset(wage_df, female == 1 & va_prv_Q4 == 1,))/N_f), 4),
                        "males" = round(c(nrow(subset(wage_df, female == 0 & va_prv_Q1 == 1,))/N_m,
                                          nrow(subset(wage_df, female == 0 & va_prv_Q2 == 1,))/N_m,
                                          nrow(subset(wage_df, female == 0 & va_prv_Q3 == 1,))/N_m,
                                          nrow(subset(wage_df, female == 0 & va_prv_Q4 == 1,))/N_m), 4)))
pander(quartiles_table)




quartiles_va = as.data.frame(quantile(wage_df$va))
colnames(quartiles_va) = "Quartiles"
Q1 = quartiles_va["25%", ] 
Q2 = quartiles_va["50%", ]
Q3 = quartiles_va["75%", ]



wage_df[ ,c("va_Q1", "va_Q2", "va_Q3", "va_Q4")] = rep(NA, nrow(wage_df))

for (i in 1:nrow(wage_df)){
  if (wage_df$va[i] <= Q1){
    wage_df[i, "va_Q1"] = 1
    wage_df[i, "va_Q2"] = 0
    wage_df[i, "va_Q3"] = 0
    wage_df[i, "va_Q4"] = 0
  }
  else if(wage_df$va[i] <= Q2 & wage_df$va[i] > Q1){
    wage_df[i, "va_Q1"] = 0
    wage_df[i, "va_Q2"] = 1
    wage_df[i, "va_Q3"] = 0
    wage_df[i, "va_Q4"] = 0
  }
  else if(wage_df$va[i] <= Q3 & wage_df$va[i] > Q2){
    wage_df[i, "va_Q1"] = 0
    wage_df[i, "va_Q2"] = 0
    wage_df[i, "va_Q3"] = 1
    wage_df[i, "va_Q4"] = 0
  }
  else if(wage_df$va[i] > Q3){
    wage_df[i, "va_Q1"] = 0
    wage_df[i, "va_Q2"] = 0
    wage_df[i, "va_Q3"] = 0
    wage_df[i, "va_Q4"] = 1
  }
}
wage_df2 = wage_df
###############
save(wage_df2, file = "modified_data2.RData")
###############

quartiles_table2 = as.data.frame(cbind( "females" = round(c(nrow(subset(wage_df, female == 1 & va_Q1 == 1, female))/N_f,
                                        nrow(subset(wage_df, female == 1 & va_Q2 == 1, female))/N_f,
                                        nrow(subset(wage_df, female == 1 & va_Q3 == 1, female))/N_f,
                                        nrow(subset(wage_df, female == 1 & va_Q4 == 1, female))/N_f), 4),
                          "males" = round(c(nrow(subset(wage_df, female == 0 & va_Q1 == 1, female))/N_m,
                                      nrow(subset(wage_df, female == 0 & va_Q2 == 1, female))/N_m,
                                      nrow(subset(wage_df, female == 0 & va_Q3 == 1, female))/N_m,
                                      nrow(subset(wage_df, female == 0 & va_Q4 == 1, female))/N_m), 4)))

# Compare the changes in how males and females
# are distributed between period -1 and period 0 and report the results in Table
# 5 (columns 5 and 6).

quartiles_table3 = cbind("females" = quartiles_table2$females - quartiles_table$females,
                         "males" = quartiles_table2$males - quartiles_table$males)

combined_quartile_table = as.data.frame(cbind("quartiles" = c("Q1", "Q2", "Q3", "Q4"), 
                                              "period -1" = quartiles_table, 
                                              "period 0" = quartiles_table2,
                                              "changes" = quartiles_table3))

save(quartiles_va_prv, quartiles_va, combined_quartile_table, file = "quartiles.Rdata")


# estimate mean wages for workers who start in a given origin quartile 
# and end up in a given destination quartile in each of the 4 periods 
# from two years before the job change to one year after (i.e., get 
# the means of yl2, yl1, y, and yp1)

############# female ##############################
f_from_Q1 = cbind(c("Q1", "Q2", "Q3", "Q4"),
                      # female Q1 to Q1
                rbind(colMeans(subset(wage_df, female == 1 & va_prv_Q1 == 1 & va_Q1 == 1, c("yl2", "yl1", "y", "yp1"))),
                      # female Q1 to Q2
                      colMeans(subset(wage_df, female == 1 & va_prv_Q1 == 1 & va_Q2 == 1, c("yl2", "yl1", "y", "yp1"))),
                      # female Q1 to Q3
                      colMeans(subset(wage_df, female == 1 & va_prv_Q1 == 1 & va_Q3 == 1, c("yl2", "yl1", "y", "yp1"))),
                      # female Q1 to Q4
                      colMeans(subset(wage_df, female == 1 & va_prv_Q1 == 1 & va_Q4 == 1, c("yl2", "yl1", "y", "yp1")))))


f_from_Q2 = cbind(c("Q1", "Q2", "Q3", "Q4"),    
                # female Q2 to Q1
                rbind(colMeans(subset(wage_df, female == 1 & va_prv_Q2 == 1 & va_Q1 == 1, c("yl2", "yl1", "y", "yp1"))),
                      # female Q2 to Q2
                      colMeans(subset(wage_df, female == 1 & va_prv_Q2 == 1 & va_Q2 == 1, c("yl2", "yl1", "y", "yp1"))),
                      # female Q2 to Q3
                      colMeans(subset(wage_df, female == 1 & va_prv_Q2 == 1 & va_Q3 == 1, c("yl2", "yl1", "y", "yp1"))),
                      # female Q2 to Q4
                      colMeans(subset(wage_df, female == 1 & va_prv_Q2 == 1 & va_Q4 == 1, c("yl2", "yl1", "y", "yp1")))))
                
f_from_Q3 = cbind(c("Q1", "Q2", "Q3", "Q4"),    
                # female Q3 to Q1
                rbind(colMeans(subset(wage_df, female == 1 & va_prv_Q3 == 1 & va_Q1 == 1, c("yl2", "yl1", "y", "yp1"))),
                      # female Q3 to Q2
                      colMeans(subset(wage_df, female == 1 & va_prv_Q3 == 1 & va_Q2 == 1, c("yl2", "yl1", "y", "yp1"))),
                      # female Q3 to Q3
                      colMeans(subset(wage_df, female == 1 & va_prv_Q3 == 1 & va_Q3 == 1, c("yl2", "yl1", "y", "yp1"))),
                      # female Q3 to Q4
                      colMeans(subset(wage_df, female == 1 & va_prv_Q3 == 1 & va_Q4 == 1, c("yl2", "yl1", "y", "yp1")))))

f_from_Q4 = cbind(c("Q1", "Q2", "Q3", "Q4"),    
                # female Q4 to Q1
                rbind(colMeans(subset(wage_df, female == 1 & va_prv_Q4 == 1 & va_Q1 == 1, c("yl2", "yl1", "y", "yp1"))),
                      # female Q4 to Q2
                      colMeans(subset(wage_df, female == 1 & va_prv_Q4 == 1 & va_Q2 == 1, c("yl2", "yl1", "y", "yp1"))),
                      # female Q4 to Q3
                      colMeans(subset(wage_df, female == 1 & va_prv_Q4 == 1 & va_Q3 == 1, c("yl2", "yl1", "y", "yp1"))),
                      # female Q4 to Q4
                      colMeans(subset(wage_df, female == 1 & va_prv_Q4 == 1 & va_Q4 == 1, c("yl2", "yl1", "y", "yp1")))))

############# male ################################
m_from_Q1 = cbind(c("Q1", "Q2", "Q3", "Q4"),
                  # female Q1 to Q1
                  rbind(colMeans(subset(wage_df, female == 0 & va_prv_Q1 == 1 & va_Q1 == 1, c("yl2", "yl1", "y", "yp1"))),
                        # female Q1 to Q2
                        colMeans(subset(wage_df, female == 0 & va_prv_Q1 == 1 & va_Q2 == 1, c("yl2", "yl1", "y", "yp1"))),
                        # female Q1 to Q3
                        colMeans(subset(wage_df, female == 0 & va_prv_Q1 == 1 & va_Q3 == 1, c("yl2", "yl1", "y", "yp1"))),
                        # female Q1 to Q4
                        colMeans(subset(wage_df, female == 0 & va_prv_Q1 == 1 & va_Q4 == 1, c("yl2", "yl1", "y", "yp1")))))


m_from_Q2 = cbind(c("Q1", "Q2", "Q3", "Q4"),    
                  # female Q2 to Q1
                  rbind(colMeans(subset(wage_df, female == 0 & va_prv_Q2 == 1 & va_Q1 == 1, c("yl2", "yl1", "y", "yp1"))),
                        # female Q2 to Q2
                        colMeans(subset(wage_df, female == 0 & va_prv_Q2 == 1 & va_Q2 == 1, c("yl2", "yl1", "y", "yp1"))),
                        # female Q2 to Q3
                        colMeans(subset(wage_df, female == 0 & va_prv_Q2 == 1 & va_Q3 == 1, c("yl2", "yl1", "y", "yp1"))),
                        # female Q2 to Q4
                        colMeans(subset(wage_df, female == 0 & va_prv_Q2 == 1 & va_Q4 == 1, c("yl2", "yl1", "y", "yp1")))))

m_from_Q3 = cbind(c("Q1", "Q2", "Q3", "Q4"),    
                  # female Q3 to Q1
                  rbind(colMeans(subset(wage_df, female == 0 & va_prv_Q3 == 1 & va_Q1 == 1, c("yl2", "yl1", "y", "yp1"))),
                        # female Q3 to Q2
                        colMeans(subset(wage_df, female == 0 & va_prv_Q3 == 1 & va_Q2 == 1, c("yl2", "yl1", "y", "yp1"))),
                        # female Q3 to Q3
                        colMeans(subset(wage_df, female == 0 & va_prv_Q3 == 1 & va_Q3 == 1, c("yl2", "yl1", "y", "yp1"))),
                        # female Q3 to Q4
                        colMeans(subset(wage_df, female == 0 & va_prv_Q3 == 1 & va_Q4 == 1, c("yl2", "yl1", "y", "yp1")))))

m_from_Q4 = cbind(c("Q1", "Q2", "Q3", "Q4"),    
                  # female Q4 to Q1
                  rbind(colMeans(subset(wage_df, female == 0 & va_prv_Q4 == 1 & va_Q1 == 1, c("yl2", "yl1", "y", "yp1"))),
                        # female Q4 to Q2
                        colMeans(subset(wage_df, female == 0 & va_prv_Q4 == 1 & va_Q2 == 1, c("yl2", "yl1", "y", "yp1"))),
                        # female Q4 to Q3
                        colMeans(subset(wage_df, female == 0 & va_prv_Q4 == 1 & va_Q3 == 1, c("yl2", "yl1", "y", "yp1"))),
                        # female Q4 to Q4
                        colMeans(subset(wage_df, female == 0 & va_prv_Q4 == 1 & va_Q4 == 1, c("yl2", "yl1", "y", "yp1")))))



# plot the means of yl2, yl1, y, and yp1 in "event time"
# (i.e., periods -2, -1, 0, 1) for male workers who started in a quartile 1 firm and
# ended up in a quartile 1, origin quartile 2, origin quartile 3, and origin quartile
# 4 firm.

#########
# Males # ##############################################################################
#########

m_to1_tab = cbind(c("Q1", "Q2", "Q3", "Q4"), rbind(m_from_Q1[1, ], m_from_Q2[1, ], m_from_Q3[1, ], m_from_Q4[1, ])[ ,-1])
m_to1_table = as.data.frame(cbind(rbind(m_to1_tab[ ,1:2],
           m_to1_tab[ ,c(1,3)], 
           m_to1_tab[ ,c(1, 4)],
           m_to1_tab[ ,c(1, 5)]), 
     as.vector(c(rep(-2, 4), rep(-1, 4), rep(0, 4), rep(1, 4)))))
colnames(m_to1_table) = c("origin", "wage", "period")
m_to1_table$wage = as.numeric(levels(m_to1_table$wage))[m_to1_table$wage]
m_to1_table$period = as.numeric(levels(m_to1_table$period))[m_to1_table$period]

m_to1_graph = ggplot(m_to1_table, aes(x = period, y = wage, group = origin, colour = origin)) + 
  geom_line() + 
  ylim(1, 2.2) + 
  labs(title = "Male moving to Q1")

###############

m_to2_tab = cbind(c("Q1", "Q2", "Q3", "Q4"), rbind(m_from_Q1[2, ], m_from_Q2[2, ], m_from_Q3[2, ], m_from_Q4[2, ])[ ,-1])
m_to2_table = as.data.frame(cbind(rbind(m_to2_tab[ ,1:2],
                                        m_to2_tab[ ,c(1,3)], 
                                        m_to2_tab[ ,c(1,4)],
                                        m_to2_tab[ ,c(1,5)]), 
                                  as.vector(c(rep(-2, 4), rep(-1, 4), rep(0, 4), rep(1, 4)))))
colnames(m_to2_table) = c("origin", "wage", "period")
m_to2_table$wage = as.numeric(levels(m_to2_table$wage))[m_to2_table$wage]
m_to2_table$period = as.numeric(levels(m_to2_table$period))[m_to2_table$period]

m_to2_graph = ggplot(m_to2_table, aes(x = period, y = wage, group = origin, colour = origin)) + 
  geom_line() + 
  ylim(1, 2.2) +
  labs(title = "Male moving to Q2")

#######################

m_to3_tab = cbind(c("Q1", "Q2", "Q3", "Q4"), rbind(m_from_Q1[3, ], m_from_Q2[3, ], m_from_Q3[3, ], m_from_Q4[3, ])[ ,-1])
m_to3_table = as.data.frame(cbind(rbind(m_to3_tab[ ,1:2],
                                        m_to3_tab[ ,c(1,3)], 
                                        m_to3_tab[ ,c(1,4)],
                                        m_to3_tab[ ,c(1,5)]), 
                                  as.vector(c(rep(-2, 4), rep(-1, 4), rep(0, 4), rep(1, 4)))))
colnames(m_to3_table) = c("origin", "wage", "period")
m_to3_table$wage = as.numeric(levels(m_to3_table$wage))[m_to3_table$wage]
m_to3_table$period = as.numeric(levels(m_to3_table$period))[m_to3_table$period]

m_to3_graph = ggplot(m_to3_table, aes(x = period, y = wage, group = origin, colour = origin)) + 
  geom_line() + 
  ylim(1, 2.2) +
  labs(title = "Male moving to Q3")

########################

m_to4_tab = cbind(c("Q1", "Q2", "Q3", "Q4"), rbind(m_from_Q1[4, ], m_from_Q2[4, ], m_from_Q3[4, ], m_from_Q4[4, ])[ ,-1])
m_to4_table = as.data.frame(cbind(rbind(m_to4_tab[ ,1:2],
                                        m_to4_tab[ ,c(1,3)], 
                                        m_to4_tab[ ,c(1,4)],
                                        m_to4_tab[ ,c(1,5)]), 
                                  as.vector(c(rep(-2, 4), rep(-1, 4), rep(0, 4), rep(1, 4)))))
colnames(m_to4_table) = c("origin", "wage", "period")
m_to4_table$wage = as.numeric(levels(m_to4_table$wage))[m_to4_table$wage]
m_to4_table$period = as.numeric(levels(m_to4_table$period))[m_to4_table$period]


m_to4_graph = ggplot(m_to4_table, aes(x = period, y = wage, group = origin, colour = origin)) + 
  geom_line() + 
  ylim(1, 2.2) +
  labs(title = "Male moving to Q4")


###########
# Females # ##############################################################################
###########

f_to1_tab = cbind(c("Q1", "Q2", "Q3", "Q4"), 
                  rbind(f_from_Q1[1, ], f_from_Q2[1, ], f_from_Q3[1, ], f_from_Q4[1, ])[ ,-1])
f_to1_table = as.data.frame(cbind(rbind(f_to1_tab[ ,1:2],
                                        f_to1_tab[ ,c(1,3)], 
                                        f_to1_tab[ ,c(1, 4)],
                                        f_to1_tab[ ,c(1, 5)]), 
                                  as.vector(c(rep(-2, 4), rep(-1, 4), rep(0, 4), rep(1, 4)))))
colnames(f_to1_table) = c("origin", "wage", "period")
f_to1_table$wage = as.numeric(levels(f_to1_table$wage))[f_to1_table$wage]
f_to1_table$period = as.numeric(levels(f_to1_table$period))[f_to1_table$period]

f_to1_graph = ggplot(f_to1_table, aes(x = period, y = wage, group = origin, colour = origin)) + 
  geom_line() + 
  ylim(1, 2.2) +
  labs(title = "Female moving to Q1")

###############

f_to2_tab = cbind(c("Q1", "Q2", "Q3", "Q4"), rbind(f_from_Q1[2, ], f_from_Q2[2, ], f_from_Q3[2, ], f_from_Q4[2, ])[ ,-1])
f_to2_table = as.data.frame(cbind(rbind(f_to2_tab[ ,1:2],
                                        f_to2_tab[ ,c(1,3)], 
                                        f_to2_tab[ ,c(1,4)],
                                        f_to2_tab[ ,c(1,5)]), 
                                  as.vector(c(rep(-2, 4), rep(-1, 4), rep(0, 4), rep(1, 4)))))
colnames(f_to2_table) = c("origin", "wage", "period")
f_to2_table$wage = as.numeric(levels(f_to2_table$wage))[f_to2_table$wage]
f_to2_table$period = as.numeric(levels(f_to2_table$period))[f_to2_table$period]


f_to2_graph = ggplot(f_to2_table, aes(x = period, y = wage, group = origin, colour = origin)) + 
  geom_line() + 
  ylim(1, 2.2) +
  labs(title = "Female moving to Q2")

#######################

f_to3_tab = cbind(c("Q1", "Q2", "Q3", "Q4"), rbind(f_from_Q1[3, ], f_from_Q2[3, ], f_from_Q3[3, ], f_from_Q4[3, ])[ ,-1])
f_to3_table = as.data.frame(cbind(rbind(f_to3_tab[ ,1:2],
                                        f_to3_tab[ ,c(1,3)], 
                                        f_to3_tab[ ,c(1,4)],
                                        f_to3_tab[ ,c(1,5)]), 
                                  as.vector(c(rep(-2, 4), rep(-1, 4), rep(0, 4), rep(1, 4)))))
colnames(f_to3_table) = c("origin", "wage", "period")
f_to3_table$wage = as.numeric(levels(f_to3_table$wage))[f_to3_table$wage]
f_to3_table$period = as.numeric(levels(f_to3_table$period))[f_to3_table$period]


f_to3_graph = ggplot(f_to3_table, aes(x = period, y = wage, group = origin, colour = origin)) + 
  geom_line() + 
  ylim(1, 2.2) +
  labs(title = "Female moving to Q3")

########################

f_to4_tab = cbind(c("Q1", "Q2", "Q3", "Q4"), rbind(f_from_Q1[4, ], f_from_Q2[4, ], f_from_Q3[4, ], f_from_Q4[4, ])[ ,-1])
f_to4_table = as.data.frame(cbind(rbind(f_to4_tab[ ,1:2],
                                        f_to4_tab[ ,c(1,3)], 
                                        f_to4_tab[ ,c(1,4)],
                                        f_to4_tab[ ,c(1,5)]), 
                                  as.vector(c(rep(-2, 4), rep(-1, 4), rep(0, 4), rep(1, 4)))))
colnames(f_to4_table) = c("origin", "wage", "period")
f_to4_table$wage = as.numeric(levels(f_to4_table$wage))[f_to4_table$wage]
f_to4_table$period = as.numeric(levels(f_to4_table$period))[f_to4_table$period]


f_to4_graph = ggplot(f_to4_table, aes(x = period, y = wage, group = origin, colour = origin)) + 
  geom_line() + 
  ylim(1, 2.2) +
  labs(title = "Female moving to Q4")

#####################
save(m_to1_graph, m_to2_graph, m_to3_graph, m_to4_graph,
  f_to1_graph, f_to2_graph, f_to3_graph, f_to4_graph, file = "quartiles-graphs.RData")
