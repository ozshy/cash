# cash_2021_1_5.R Cash survey for JEL
## libraries
library(xtable)
library(ggplot2); theme_set(theme_bw())
library(tidyr)# for gather (changing wide to long)
library(dplyr)
library(scales)# for currency and % symbols
library(rpart)
library(rpart.plot)
library(partykit) # modifies rpart tree plot

setwd("~/Papers/cash/cash_coding")
dir()

## Function definitions: Cummulative Annual Growth Rate
CAGR_formula <- function(FV, PV, yrs) {
  values <- ((FV/PV)^(1/yrs)-1)
  return(values)
}
# testing the formula
CAGR_formula(110, 100, 1)
CAGR_formula(110, 100, 2)
CAGR_formula(121, 100, 2)

### Table holding.t displaying CIC using CPMI BIS data: cic_bis
#holding1.df = read.csv("cic_bis_2012_2019.csv")
#saveRDS(holding1.df, "cic_bis_2012_2019.rds")
holding1.df = readRDS("cic_bis_2012_2019.rds")
holding1.df
(holding2.df = holding1.df[-c(22,23),])
dim(holding2.df)
names(holding2.df)
holding2.df$Country
holding3.df = holding2.df

# new column: % change in value/GDP (2019 vs 2012)
(holding3.df$cagrgdp = 100*CAGR_formula(as.vector(holding3.df[,2])/100, holding3.df[,4]/100, 7))

names(holding3.df)
# new column: % change in value/narrow money (2019 vs 2012)
(holding3.df$cagrnarrow = 100*CAGR_formula(holding3.df[,3], holding3.df[,5], 7))

holding4.df = holding3.df
names(holding4.df)
holding5.df =  subset(holding4.df, select = -c(4,5))# removing 2012
names(holding5.df)
holding5.df
dim(holding5.df)

names(holding5.df)[names(holding5.df)=="X2019.value.as.a.percentage.of.GDP"] = "CIC/GDP"
names(holding5.df)[names(holding5.df)=="X2019.value.as.a.percentage.of.narrow.money"] = "CIC/Narrow"
names(holding5.df)[4] = "CAGR GDP"
names(holding5.df)[5] = "CAGR Narrow"
#
names(holding5.df)
dim(holding5.df)
holding5.df
#
holding6.df = holding5.df[, c(1,2,4,3,5)]
names(holding6.df)
holding6.df
#
(holding7.df = holding6.df[order(-holding6.df[,2]), ])

### Preparing LaTeX table
# below, create matrix with 1 extra column to indicate number of digits for each row
#(digitm = matrix(c(rep(0,8), 0, rep(1,7), rep(0,8), 0, rep(1,7), rep(0,8), 0, rep(1,7), rep(0,8), 0, rep(1,7), 0, rep(2,7), 0, rep(2,7), rep(0,8), 0, rep(2,7), rep(0,8), 0, rep(2,7), rep(0,8), rep(0,8), rep(0,8), rep(0,8), rep(0,8), rep(0,8)), nrow = 20, ncol = 8, byrow = T))
#
print(xtable(holding7.df, digits = 1), include.rownames = F, hline.after = c(0)) 
# End of table holding.t

### Figure/Table hoard.t hoard.f using FRB SF data on bills in circulation
dir()
#hoard1.df = read.csv("hoard.csv")
#saveRDS(hoard1.df, "hoard.rds")         
hoard2.df = readRDS("hoard.rds")
names(hoard2.df)
hoard3.df = hoard2.df
head(hoard3.df)
dim(hoard3.df)
(names(hoard3.df) = c("Year", "$1", "$2", "$5", "$10", "$20", "$50", "$100", ">$100", "Total"))

# Now, delete >$100 and recompute Total
dim(hoard3.df)
(hoard4.df = subset(hoard3.df, select = -c(9,10)))
hoard5.df = hoard4.df
(hoard5.df$Total = rowSums(hoard4.df[2:8]))
hoard5.df
dim(hoard5.df)
hoard6.df = hoard5.df[-21,]# del 1999
hoard6.df
(hoard7.df = hoard6.df[order(hoard6.df$Year),])
#
# Changing values to % of total
hoard8.df = hoard7.df
dim(hoard8.df)
hoard8.df$`$1` = 100*hoard7.df$`$1`/hoard7.df$Total
hoard8.df$`$2` = 100*hoard7.df$`$2`/hoard7.df$Total
hoard8.df$`$5` = 100*hoard7.df$`$5`/hoard7.df$Total
hoard8.df$`$10` = 100*hoard7.df$`$10`/hoard7.df$Total
hoard8.df$`$20` = 100*hoard7.df$`$20`/hoard7.df$Total
hoard8.df$`$50` = 100*hoard7.df$`$50`/hoard7.df$Total
hoard8.df$`$100` = 100*hoard7.df$`$100`/hoard7.df$Total
round(hoard8.df, 3)
# renaming colum Total to $Total
names(hoard8.df)[names(hoard8.df)=="Total"] = "$Total"
names(hoard8.df)
# Adding a 100% column (sum of shares)
dim(hoard8.df)
(hoard8.df$Total = rowSums(hoard8.df[,c(2:8)]))
(hoard9.df = hoard8.df)
dim(hoard9.df)
(hoard9.df = hoard9.df[,c(1,2,3,4,5,6,7,8,10,9)])
#
# computing CAGR for each denomination and total
CAGR_formula <- function(FV, PV, yrs) {
  values <- ((FV/PV)^(1/yrs)-1)
  return(values)
}
#
(cagr_1_perc = 100*CAGR_formula(hoard9.df[20,2], hoard9.df[1,2], 20))
(cagr_2_perc = 100*CAGR_formula(hoard9.df[20,3], hoard9.df[1,3], 20))
(cagr_5_perc = 100*CAGR_formula(hoard9.df[20,4], hoard9.df[1,4], 20))
(cagr_10_perc = 100*CAGR_formula(hoard9.df[20,5], hoard9.df[1,5], 20))
(cagr_20_perc = 100*CAGR_formula(hoard9.df[20,6], hoard9.df[1,6], 20))
(cagr_50_perc = 100*CAGR_formula(hoard9.df[20,7], hoard9.df[1,7], 20))
(cagr_100_perc = 100*CAGR_formula(hoard9.df[20,8], hoard9.df[1,8], 20))
(cagr_total_perc = 100*CAGR_formula(hoard9.df[20,9], hoard9.df[1,9], 20))
(cagr_total2_perc = 100*CAGR_formula(hoard9.df[20,10], hoard9.df[1,10], 20))

hoard9.df
dim(hoard9.df)
### Preparing LaTeX table hoard.t
# below, create matrix with 1 extra column to indicate number of digits for each row (note: only need to specify 1st row, then it gets duplicated)
# (digitm = matrix(c(0,rep(2,8),0,1), nrow = 20, ncol = 11, byrow = T))
# #
# print(xtable(hoard9.df, digits = digitm), include.rownames = F, hline.after = c(0))
# End of table hoard.t

# Switching from Table to Figure: hoard.f using hoard7.df
hoard10.df = hoard7.df

hoard10.df = gather(hoard10.df, key = "Denomination", value = "Value", -Year)

hoard10.df$Denomination = factor(hoard10.df$Denomination,
                                 levels = c("Total", "$100", "$50", "$20", "$10", "$5", "$2", "$1"))

# below, plotting with Total [deleted later on, see below]
ggplot(hoard10.df, aes(x = Year, y = Value, col = Denomination)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 2000:2019) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18), 
        axis.text.y = element_text(size = 18),
        legend.position = c(0.15, 0.7),
        text = element_text(size = 20)) +
  scale_y_continuous(breaks = seq(0, 1800, 200)) +
  ylab("U.S. dollars (billions)") +
  scale_color_manual(values = c("black", "red", "blue", "violetred4", "deeppink","cyan", "darkgreen", "gold"))

## Removing Total from Figure hoarding.f
#
hoard11.df = hoard7.df
names(hoard11.df)
dim(hoard11.df)
hoard11.df = subset(hoard11.df, select = -9)
head(hoard11.df, 3)


hoard11.df = gather(hoard11.df, key = "Denomination", value = "Value", -Year)

hoard11.df$Denomination = factor(hoard11.df$Denomination,
                                 levels = c("$100", "$50", "$20", "$10", "$5", "$2", "$1"))

# blow, plotting without Total
ggplot(hoard11.df, aes(x = Year, y = Value, col = Denomination)) +
  geom_line(size = 1.2) +
  geom_point(size = 3, aes(shape = Denomination)) +
  scale_x_continuous(breaks = 2000:2019) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18, color = "black"), 
        axis.text.y = element_text(size = 18, color = "black"),
        legend.position = c(0.15, 0.7),
        text = element_text(size = 20)) +
  scale_y_continuous(breaks = seq(0, 1500, 100)) +
  ylab("U.S. dollars (billions)") +
  scale_color_manual(values = c("red", "black", "blue", "violetred4", "deeppink","cyan", "darkgreen"))+ 
  scale_shape_manual(values=c(0:6))
# End of figure hoard.f

### Figure/Table hoard_eu.t hoard_eu.f using ECB data on bills in circulation
dir()
#hoard_eu1.df = read.csv("hoard_eu.csv")
#saveRDS(hoard_eu1.df, "hoard_eu.rds")         
hoard_eu2.df = readRDS("hoard_eu.rds")
names(hoard_eu2.df)
hoard_eu3.df = hoard_eu2.df
head(hoard_eu3.df)
dim(hoard_eu3.df)
(names(hoard_eu3.df) = c("Year", "5eur", "10eur", "20eur", "50eur", "100eur", "200eur", "500eur"))

(hoard_eu4.df = hoard_eu3.df[order(hoard_eu3.df$Year),])
#

# Figure: hoard_eu.f using hoard_eu4.df
hoard_eu10.df = hoard_eu3.df

hoard_eu10.df = gather(hoard_eu10.df, key = "Denomination", value = "Value", -Year)

hoard_eu10.df$Denomination = factor(hoard_eu10.df$Denomination,
                                 levels = c("Year", "5eur", "10eur", "20eur", "50eur", "100eur", "200eur", "500eur"))


ggplot(hoard_eu10.df, aes(x = Year, y = Value, col = Denomination)) +
  geom_line(size = 1.2) +
  geom_point(size = 3, aes(shape = Denomination)) +
  scale_x_continuous(breaks = 2002:2020) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18, color = "black"), 
        axis.text.y = element_text(size = 18, color = "black"),
        legend.position = c(0.15, 0.7),
        text = element_text(size = 20)) +
  scale_y_continuous(breaks = seq(0, 650, 50)) +
  ylab("Euro (billions)") +
  scale_color_manual(values = c("darkgreen", "cyan", "blue", "red", "deeppink","violetred4", "black")) +
  scale_shape_manual(values=c(0:6))
# End of figure hoard_eu.f

### Figure ATM starts here
#dir()
#atm1=read.csv("ATM_48_countries.csv")
#saveRDS(atm1, "ATM_48_countries.rds")    
atm2 = readRDS("ATM_48_countries.rds")
dim(atm2)
names(atm2)
names(atm2) = c("country", "code", "num_atm", "GDP_PP")
atm2
(atm3 = atm2)
str(atm3)
# Constucting vector gdp for labeling dots
(gdp.vec = dollar(round(atm3$GDP_PP, 0)))
str(gdp.vec)
gdp_no_cad.vec = gdp.vec
atm3[atm3$country=="Canada",]# which row has Canada
gdp_no_cad.vec[7]=NA # remove value for Canada
gdp_no_cad.vec
(gdp_cad.vec = gdp.vec)
gdp_cad.vec[1:6] = NA# remove all but Canada
gdp_cad.vec[8:48] = NA# remove all but Canada
gdp_cad.vec
# adding to data frame
atm3$gdp_no_cad = gdp_no_cad.vec
atm3$gdp_cad = gdp_cad.vec
atm3
#
ggplot(atm3, aes(x=num_atm, y=reorder(country, num_atm)))+
  geom_point(size=3)+
  scale_x_continuous(breaks = seq(0,220,20)) +
  theme(axis.text.x = element_text(size = 18, color = "black"), 
        axis.text.y = element_text(size = 12, color = "black"),
        text = element_text(size = 20)) +
  xlab("Number of ATMs per 100,000 adults") + ylab("Country")+
  geom_text(aes(label= gdp_no_cad), hjust= -0.2, colour="black", size=5) +
  geom_text(aes(label= gdp_cad), vjust= 1.5, colour="black", size=5)

cor(atm3$num_atm, atm3$GDP_PP)# correlation between num ATM and GDP PP

### Machine learing decision tree of payment method begins
# Prepare the data and save it as a new RDS for the public
# dir()
# tree1.df = readRDS("diary171819_210206.rds")
# names(tree1.df)
# 
# # # we need only 10 variables [filter for in-person only]
# table(tree1.df$in_person)
# tree2.df = subset(tree1.df, in_person == "yes")
# dim(tree2.df)
# names(tree2.df)
# #
# tree3.df = subset(tree2.df, select = c(pi, amnt, merch, work, gender, age, marital, education, hh_income, hh_size))
# names(tree3.df)
# #select only 5 pi
# table(tree3.df$pi)
# tree4.df = subset(tree3.df, pi  %in% c("cash", "debit_card", "credit_card", "check", "prepaid/gift/EBT_card"))
# table(tree4.df$pi)
# dim(tree4.df)
# # drop unused payment method factors
# tree5.df = tree4.df
# tree5.df$pi = droplevels(tree4.df$pi)
# table(tree5.df$pi)
# levels(tree5.df$pi)
# dim(tree5.df)
# #
# # check for NA
# names(tree5.df)
# sum(is.na(tree5.df$pi))
# sum(is.na(tree5.df$amnt))
# tree6.df = tree5.df[!is.na(tree5.df$amnt), ]
# dim(tree6.df)
# dim(tree6.df)-dim(tree5.df)
# sum(is.na(tree6.df$merch))
# tree6.df = tree6.df[!is.na(tree6.df$merch), ]
# dim(tree6.df)
# sum(is.na(tree6.df$work))
# tree6.df = tree6.df[!is.na(tree6.df$work), ]
# dim(tree6.df)
# sum(is.na(tree6.df$gender))
# sum(is.na(tree6.df$age))
# tree6.df = tree6.df[!is.na(tree6.df$age), ]
# dim(tree6.df)
# sum(is.na(tree6.df$marital))
# tree6.df = tree6.df[!is.na(tree6.df$marital), ]
# dim(tree6.df)
# sum(is.na(tree6.df$education))
# sum(is.na(tree6.df$work))
# sum(is.na(tree6.df$hh_income))
# tree6.df = tree6.df[!is.na(tree6.df$hh_income), ]
# dim(tree6.df)
# sum(is.na(tree6.df$hh_size))
# tree6.df = tree6.df[!is.na(tree6.df$hh_size), ]
# #
# #save for RDS to be posted
# dim(tree6.df)
# saveRDS(tree6.df, "tree_210209.rds")
# # 
## tree starts here with loading the tree data (public)
tree10.df = readRDS("tree_210209.rds")
names(tree10.df)
# setting up the tree model
pi_model1 = Method~ Amount +In_person + merch +Age +Gender +Marital +Education + Work +HH_income +HH_size # 
# Use model below (in-person removed because data are restricted to in-person payments only)
pi_model2 = Method~ Amount + merch +Age +Gender +Marital +Education + Work +HH_income +HH_size # 
#


### Figure: Share of payment methods $1 to $50 
# Prepare the data and save it as a new RDS for the public
# dir()
# pi1.df = readRDS("diary171819_210206.rds")
# # we need only 2 variables: payment method (pi) and dollar amount (amnt) [filter for in-person only]
# table(pi1.df$in_person)
# pi2.df = subset(pi1.df, in_person == "yes")
# dim(pi2.df)
# #
# pi3.df = subset(pi2.df, select = c(pi, amnt, id))
# names(pi3.df)
# dim(pi3.df)
# #
# #select only 5 pi
# table(pi3.df$pi)
# pi4.df = subset(pi3.df, pi  %in% c("cash", "debit_card", "credit_card", "check", "prepaid/gift/EBT_card"))
# table(pi4.df$pi)
# dim(pi4.df)
# # drop unused payment method factors
# pi5.df = pi4.df
# pi5.df$pi = droplevels(pi4.df$pi)
# dim(pi5.df)
# table(pi5.df$pi)
# levels(pi5.df$pi)
# #
# # check for NA
# sum(is.na(pi5.df$pi))
# sum(is.na(pi5.df$amnt))
# pi6.df = pi5.df[!is.na(pi5.df$amnt), ]
# dim(pi6.df)
# dim(pi6.df)-dim(pi5.df)
# #
# #save for RDS to be posted
# saveRDS(pi6.df, "pi_share_210207.rds")
# 

## # Figure payment method share begins
# #
 pi10.df = readRDS("pi_share_210207.rds")
 names(pi10.df)
 str(pi10.df)
 levels(pi10.df$pi)
 pi10.df$pi <- factor(pi10.df$pi, sort(unique(pi10.df$pi), decreasing = T))# changing the order of pi
 #
ggplot(subset(pi10.df, pi10.df$amnt<=50), aes(x=amnt, fill=pi)) + 
  geom_histogram(position="fill", stat="bin", binwidth = 1, color = "black") +
  scale_y_continuous(labels = scales::percent, breaks = round(seq(0,1, 0.1), 2)) +
  xlab("Payment amount (U.S.\ dollars)") +
  ylab("") +
  theme(legend.position = c(0.15, 0.15), legend.title = element_text(color = "black", size = 18), legend.text = element_text(color = "black")) +
  scale_fill_manual(name = "Payment method", labels = c("Prepaid card", "Debit card", "Credit card", "Check", "Cash"), values = c("orange", "blue", "green", "black", "red")) +
  scale_x_continuous(labels = scales::dollar, breaks = seq(0,50,5)) + 
  theme(axis.text.x = element_text(size = 18, color = "black"), axis.text.y = element_text(size = 18, color = "black"), text = element_text(size = 20), panel.grid.major.y = element_line(size=.1, color="black"), panel.grid.major.x = element_line(size=.1, color="black") )
#
# How many trans and resp in the $0-$50 range? (excl $0)
nrow(subset(pi10.df, amnt <= 50 & amnt>0 ))# num trans
length(unique(subset(pi10.df, amnt <= 50 & amnt>0)$id))# num of resp
#
## percentage change before and after currency denominations (discussion in the paper about spikes in cash use)
pi11.df = subset(pi10.df, amnt > 0 & amnt <= 51)
dim(pi11.df)
names(pi11.df)
#
# share of cash payments $3 to $4 (by volume)
(s34 = 100*nrow(subset(pi11.df, pi=="cash" & amnt>3 & amnt<=4))/nrow(subset(pi11.df, amnt>3 & amnt<=4)))
# share of cash payments $4 to $5 (by volume)
(s45 = 100*nrow(subset(pi11.df, pi=="cash" & amnt>4 & amnt<=5))/nrow(subset(pi11.df, amnt>4 & amnt<=5)))
# share of cash payments $5 to $6 (by volume)
(s56 = 100*nrow(subset(pi11.df, pi=="cash" & amnt>5 & amnt<=6))/nrow(subset(pi11.df, amnt>5 & amnt<=6)))
# share of cash payments $8 to $9 (by volume)
(s89 = 100*nrow(subset(pi11.df, pi=="cash" & amnt>8 & amnt<=9))/nrow(subset(pi11.df, amnt>8 & amnt<=9)))
# share of cash payments $9 to $10 (by volume)
(s910 = 100*nrow(subset(pi11.df, pi=="cash" & amnt>9 & amnt<=10))/nrow(subset(pi11.df, amnt>9 & amnt<=10)))
# share of cash payments $10 to $11 (by volume)
(s1011 = 100*nrow(subset(pi11.df, pi=="cash" & amnt>10 & amnt<=11))/nrow(subset(pi11.df, amnt>10 & amnt<=11)))
# share of cash payments $13 to $14 (by volume)
(s1314 = 100*nrow(subset(pi11.df, pi=="cash" & amnt>13 & amnt<=14))/nrow(subset(pi11.df, amnt>13 & amnt<=14)))
# share of cash payments $14 to $15 (by volume)
(s1415 = 100*nrow(subset(pi11.df, pi=="cash" & amnt>14 & amnt<=15))/nrow(subset(pi11.df, amnt>14 & amnt<=15)))
# share of cash payments $15 to $16 (by volume)
(s1516 = 100*nrow(subset(pi11.df, pi=="cash" & amnt>15 & amnt<=16))/nrow(subset(pi11.df, amnt>15 & amnt<=16)))
# share of cash payments $18 to $19 (by volume)
(s1819 = 100*nrow(subset(pi11.df, pi=="cash" & amnt>18 & amnt<=19))/nrow(subset(pi11.df, amnt>18 & amnt<=19)))
# share of cash payments $18 to $19 (by volume)
(s1920 = 100*nrow(subset(pi11.df, pi=="cash" & amnt>19 & amnt<=20))/nrow(subset(pi11.df, amnt>19 & amnt<=20)))
# share of cash payments $20 to $21 (by volume)
(s2021 = 100*nrow(subset(pi11.df, pi=="cash" & amnt>20 & amnt<=21))/nrow(subset(pi11.df, amnt>20 & amnt<=21)))
# share of cash payments $23 to $24 (by volume)
(s2324 = 100*nrow(subset(pi11.df, pi=="cash" & amnt>23 & amnt<=24))/nrow(subset(pi11.df, amnt>23 & amnt<=24)))
# share of cash payments $24 to $25 (by volume)
(s2425 = 100*nrow(subset(pi11.df, pi=="cash" & amnt>24 & amnt<=25))/nrow(subset(pi11.df, amnt>24 & amnt<=25)))
(s2526 = 100*nrow(subset(pi11.df, pi=="cash" & amnt>25 & amnt<=26))/nrow(subset(pi11.df, amnt>25 & amnt<=26)))
# share of cash payments $28 to $29 (by volume)
(s2829 = 100*nrow(subset(pi11.df, pi=="cash" & amnt>28 & amnt<=29))/nrow(subset(pi11.df, amnt>28 & amnt<=29)))
# share of cash payments $29 to $30 (by volume)
(s2930 = 100*nrow(subset(pi11.df, pi=="cash" & amnt>29 & amnt<=30))/nrow(subset(pi11.df, amnt>29 & amnt<=30)))
# share of cash payments $30 to $31 (by volume)
(s3031 = 100*nrow(subset(pi11.df, pi=="cash" & amnt>30 & amnt<=31))/nrow(subset(pi11.df, amnt>30 & amnt<=31)))
# share of cash payments $33 to $34 (by volume)
(s3334 = 100*nrow(subset(pi11.df, pi=="cash" & amnt>33 & amnt<=34))/nrow(subset(pi11.df, amnt>33 & amnt<=34)))
# share of cash payments $34 to $35 (by volume)
(s3435 = 100*nrow(subset(pi11.df, pi=="cash" & amnt>34 & amnt<=35))/nrow(subset(pi11.df, amnt>34 & amnt<=35)))
# share of cash payments $35 to $36 (by volume)
(s3536 = 100*nrow(subset(pi11.df, pi=="cash" & amnt>35 & amnt<=36))/nrow(subset(pi11.df, amnt>35 & amnt<=36)))
# share of cash payments $38 to $39 (by volume)
(s3839 = 100*nrow(subset(pi11.df, pi=="cash" & amnt>38 & amnt<=39))/nrow(subset(pi11.df, amnt>38 & amnt<=39)))
# share of cash payments $39 to $40 (by volume)
(s3940 = 100*nrow(subset(pi11.df, pi=="cash" & amnt>39 & amnt<=40))/nrow(subset(pi11.df, amnt>39 & amnt<=40)))
# share of cash payments $40 to $41 (by volume)
(s4041 = 100*nrow(subset(pi11.df, pi=="cash" & amnt>40 & amnt<=41))/nrow(subset(pi11.df, amnt>38 & amnt<=39)))
# share of cash payments $43 to $44 (by volume)
(s4344 = 100*nrow(subset(pi11.df, pi=="cash" & amnt>43 & amnt<=44))/nrow(subset(pi11.df, amnt>43 & amnt<=44)))
# share of cash payments $43 to $44 (by volume)
(s4445 = 100*nrow(subset(pi11.df, pi=="cash" & amnt>44 & amnt<=45))/nrow(subset(pi11.df, amnt>44 & amnt<=45)))
# share of cash payments $45 to $46 (by volume)
(s4546 = 100*nrow(subset(pi11.df, pi=="cash" & amnt>45 & amnt<=46))/nrow(subset(pi11.df, amnt>45 & amnt<=46)))
# share of cash payments $48 to $49 (by volume)
(s4849 = 100*nrow(subset(pi11.df, pi=="cash" & amnt>48 & amnt<=49))/nrow(subset(pi11.df, amnt>48 & amnt<=49)))
# share of cash payments $49 to $50 (by volume)
(s4950 = 100*nrow(subset(pi11.df, pi=="cash" & amnt>49 & amnt<=50))/nrow(subset(pi11.df, amnt>49 & amnt<=50)))
# share of cash payments $50 to $51 (by volume)
(s5051 = 100*nrow(subset(pi11.df, pi=="cash" & amnt>50 & amnt<=51))/nrow(subset(pi11.df, amnt>50 & amnt<=51)))
#
# start constructing table share_denom.t
(Amount = dollar(seq(5,50,5)))
# vector of denomination -2 to -1 range
(denom_minus2 = c(s34, s89, s1314, s1819, s2324, s2829, s3334, s3839, s4344, s4849))
# vector of denomination -1 to 0 range
(denom_minus1 = c(s45, s910, s1415, s1920, s2425, s2930, s3435, s3940, s4445, s4950))
# vector of denomination 0 to +1 range
(denom_plus1 = c(s56, s1011, s1516, s2021, s2526, s3031, s3536, s4041, s4546, s5051))
#
(share_denom.df = data.frame(Amount, denom_minus2, denom_minus1, denom_plus1))
(digitm = matrix(c(0,1,1,1,1), nrow = 10, ncol = 4+1, byrow = T))
#
print(xtable(share_denom.df, digits = digitm), include.rownames = F, hline.after = c(0)) 


### End figure share of payments by PI