# cash_2022_4_6.R Cash survey for JEL. 
# Table of contents: 
# Line 41 Table 1. 
# Line 82 Figure 3. 
# Line 148 Figure 4. 
# Line 187 Figure 7. 
# Line 232 Figure 5. 
# Line 272 Figure 6. 
# Line 301 Table 4.  
# Line 382 Table 5. 
# Line 387 Table 6. 
# Alternatively, search for "Start" to find the next item. 

## Measuring run time from start to the end of code
start_time = Sys.time()

## Loading all the libraries (packages) used in this code:
library(xtable)# for LaTeX tables
library(ggplot2); theme_set(theme_bw())
library(tidyr)# for gather (changing wide to long)
library(dplyr)
library(scales)# for currency and % symbols
library(spatstat) # for weighted.median
library(rpart)# machine learning tree
library(rpart.plot)# machine learning tree
library(partykit) # modifies rpart tree plot

setwd("~/Papers/Papers_accepted/cash_survey/cash_coding")
dir()

## Function definitions: Cumulative Annual Growth Rate
CAGR_formula <- function(FV, PV, yrs) {
  values <- ((FV/PV)^(1/yrs)-1)
  return(values)
}
# testing the formula
CAGR_formula(110, 100, 1)
CAGR_formula(110, 100, 2)
CAGR_formula(121, 100, 2)

### Start Table 1: holding.t (in paper) displaying CIC using CPMI BIS data: cic_bis
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

print(xtable(holding7.df, digits = 1), include.rownames = F, hline.after = c(0)) 
# End of table holding.t

### Start Figure 3 hoard.f using FRB SF data on bills in circulation
dir()
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
hoard6.df = hoard5.df[-22,]# del 1999
hoard6.df
(hoard7.df = hoard6.df[order(hoard6.df$Year),])
#
## Changing values to % of total [This is for the discussion on the share of $100 in total CIC value]
hoard8.df = hoard7.df
#
100*hoard7.df$`$100`/hoard7.df$Total# share of $100 in total CIC (value)

hoard10.df = hoard7.df
#
hoard10.df = gather(hoard10.df, key = "Denomination", value = "Value", -Year)

hoard10.df$Denomination = factor(hoard10.df$Denomination,
                                 levels = c("Total", "$100", "$50", "$20", "$10", "$5", "$2", "$1"))

## Removing Total from Figure 3 hoarding.f
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
  scale_x_continuous(breaks = 2000:2020) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18, color = "black"), 
        axis.text.y = element_text(size = 18, color = "black"),
        legend.position = c(0.15, 0.7),
        text = element_text(size = 20)) +
  scale_y_continuous(breaks = seq(0, 1600, 100)) +
  ylab("U.S. dollars (billions)") +
  scale_color_manual(values = c("red", "black", "blue", "violetred4", "deeppink","cyan", "darkgreen"))+ 
  scale_shape_manual(values=c(0:6))

# Saving the above plot on your hard drive
ggsave("Figure_3.jpg", plot = last_plot(), device = "jpg", width = 11, height = 8.5, units = "in")
# Note that 11x8.5 is the size of a US Legal paper in landscape mode. 

# End of figure 3 hoard.f

### Start Figure 4 hoard_eu.f using ECB data on bills in circulation
dir()
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

# Saving the above plot on your hard drive
ggsave("Figure_4.jpg", plot = last_plot(), device = "jpg", width = 11, height = 8.5, units = "in")
# Note that 11x8.5 is the size of a US Legal paper in landscape mode. 

# End of Figure 4 (hoard_eu.f)

### Start Figure 7 ATM per 100k adults with PP-GDP
atm2 = readRDS("ATM_48_countries.rds")
dim(atm2)
names(atm2)
names(atm2) = c("country", "code", "num_atm", "GDP_PP")
atm2
(atm3 = atm2)
str(atm3)
# Constructing vector gdp for labeling dots
(gdp.vec = dollar(round(atm3$GDP_PP, 0)))
str(gdp.vec)
# Because Canada's GPD sticks out on the right I separate it and put Canada's PP GDP below the dot
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
names(atm3)
#
ggplot(atm3, aes(x=num_atm, y=reorder(country, GDP_PP)))+
  geom_point(size=3)+
  scale_x_continuous(breaks = seq(0,220,20)) +
  theme(axis.text.x = element_text(size = 18, color = "black"), 
        axis.text.y = element_text(size = 12, color = "black"),
        text = element_text(size = 20)) +
  xlab("Number of ATMs per 100,000 adults") + ylab("Country")+ 
  geom_text(aes(label= gdp_no_cad), hjust= -0.2, colour="black", size=5) +
  geom_text(aes(label= gdp_cad), vjust= 1.5, colour="black", size=5)

# Saving the above plot on your hard drive
ggsave("Figure_7.jpg", plot = last_plot(), device = "jpg", width = 11, height = 8.5, units = "in")
# Note that 11x8.5 is the size of a US Legal paper in landscape mode. 

cor(atm3$num_atm, atm3$GDP_PP)# correlation between num ATM and GDP PP

## End of Figure 7 (number of ATMs, num_atm.f)

### Start Figure 5 Machine learning decision tree of payment method begins
## loading the tree data (public)
tree10.df = readRDS("tree_210210.rds")
colnames(tree10.df)
# Rename variables (columns) to read nicely on a tree
tree11.df = tree10.df
(colnames(tree11.df) = c("id", "Method", "Amount", "Merchant", "Work", "Gender", "Age", "Marital", "Education", "HH_income", "HH_size"))

# setting up the tree model
pi_model1 = Method~ Amount +In_person + Merchant +Age +Gender +Marital +Education + Work +HH_income +HH_size # 
# Use model below (in-person removed because data are restricted to in-person payments only. Also, Merchant removed (makes tree figure messy with 21 merchant types))
pi_model2 = Method~ Amount +Age +Gender +Marital +Education + Work +HH_income +HH_size # 
#
set.seed(1955)# to be able to reproduce the rpart CV below
pi_tree2 = rpart(pi_model2, data = tree11.df, method = "class", control = rpart.control(cp = 0.001))# Extremely-long tree first, then prune it
#Below, plot a tree (Note: Longer than optimal, but needed for later prunning and redrawing). 
prp(pi_tree2, type = 3, box.palette = "auto", extra = 100, under = T, tweak = 1.0, varlen = 0, faclen = 0)#faclet=0 avoids abbreviations, tweak for char size
#now search for optimal cp, rpart has cp table built in
plotcp(pi_tree2)# plot cp: Not used for this demo plot. See training data below
names(pi_tree2)
pi_tree2$cptable # List cp, number of splits and errors
# Below, I choose cp to use for pruning (highest rel error below the dashed line)
(cp.choice = pi_tree2$cptable[8, "CP"]) # Corresponds to 6 splits (just for demonstration)
pi_prune2 = prune.rpart(pi_tree2, cp=cp.choice)
#
#rpart.plot(pi_prune2, cex = 1)

rpart.plot(pi_prune2, type = 5, extra = 100, legend.x=NA, legend.y=NA, tweak = 1.9, fallen.leaves = FALSE, gap = 0, space = 1, digits = 4, compress = T, ycompress = F)

# Saving the above plot on your hard drive 
png(filename='Figure_5.png', type='cairo-png');
rpart.plot(pi_prune2, type = 5, extra = 100, legend.x=NA, legend.y=NA, tweak = 1.9, fallen.leaves = FALSE, gap = 0, space = 1, digits = 4, compress = T, ycompress = F)
dev.off();

# caption Fig 5 (tree)
nrow(tree11.df)
length(unique(tree11.df$id))

## End of Figure 5 (Decision tree, tree.f)

###  Start Figure 6: Share of payment methods $1 to $50 
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

# Saving the above plot on your hard drive
ggsave("Figure_6.jpg", plot = last_plot(), device = "jpg", width = 11, height = 8.5, units = "in")
# Note that 11x8.5 is the size of a US Legal paper in landscape mode. 

# How many trans and resp in the $0-$50 range? (excl $0)
nrow(subset(pi10.df, amnt <= 50 & amnt>0 ))# num trans
length(unique(subset(pi10.df, amnt <= 50 & amnt>0)$id))# num of resp

## End of Figure 6 (histogram of share of payment methods, hist.f)

### Start constructing table 4 share_denom.t percentage change before and after currency denominations (discussion in the paper about spikes in cash use)
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
#
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

## End Table 4 (histogram of payment methods used, hist.f) 

### Start Table 5 Assessments 
dir()
(assess2.df = readRDS("assessments_210310.rds"))
print(xtable(assess2.df), include.rownames = F, hline.after = c(0)) 

### Start Table 6 (Card adoption by household income)
### Reading RDS datasets (merged data: merged by a separate R-file)
d1 = readRDS("diary171819_210206.rds")
objects()
names(d1)
length(unique(d1$id)) # num of unique respondents
dim(d1) # num trans times num variables

##
h1 = d1
table(h1$pi)
nrow(h1[is.na(h1$pi), ])# num missing pi
h2 = h1[!is.na(h1$pi), ]# removing pi missing
table(h2$pi)
#
h3 = h2
table(h3$type)# The removal of the above NA pi also removed type==income, so there no need to remove type==income. All remaining obs are payments

# Shorten PI names
str(h3$pi)
levels(h3$pi)
#
#levels(h3$pi)[levels(h3$pi)=="1"] = "cash"
#levels(h3$pi)[levels(h3$pi)=="2"] = "check"
levels(h3$pi)[levels(h3$pi)=="credit_card"] = "credit"
levels(h3$pi)[levels(h3$pi)=="debit_card"] = "debit"
levels(h3$pi)[levels(h3$pi)=="prepaid/gift/EBT_card"] = "prepaid"
table(h3$pi)

## adding var with respondent's adoption profile: Both_cards, DC_only, CC_only, None, both_UB. 
h4= h3
h4$adopt = NA
h4$adopt[h4$dc_adopt==1 & h4$cc_adopt==1] = "Both_cards"
h4$adopt[h4$cc_adopt==0 & h4$pi != "credit"] = "No_cc"
h4$adopt[h4$dc_adopt==0 & h4$pi != "debit"] = "No_dc"
h4$adopt[h4$dc_adopt==0 & h4$cc_adopt==0 & h4$pi != "credit" & h4$pi != "debit"] = "None"
table(h4$adopt)
str(h4$adopt)
h4$adopt = as.factor(h4$adopt)
sum(is.na(h4$adopt))# num trans with missing adopt =0

## create some duplicates demographics (upper and lower case)
names(dplyr::select(h4, contains("age")))
h4$Age = h4$age # have it both ways
summary(h4$Age)
names(dplyr::select(h4, contains("income")))
h4$HH_income = h4$hh_income
summary(h4$HH_income)
names(dplyr::select(h4, contains("size")))
h4$HH_size = h4$hh_size
summary(h4$HH_size)
h4$Work = h4$work
table(h4$Work)
h4$Marital = h4$marital
table(h4$Marital)
names(dplyr::select(h4, contains("gender")))
table(h4$gender)
h4$Gender = h4$gender
table(h4$Gender)
#
names(dplyr::select(h4, contains("educ")))
table(h4$education)
h4$Education = h4$education
table(h4$Education)

# removing payments not made in October (otherwise, the per-respondent monthly averages become complicated)
dim(h4)
h5 = h4 %>% filter((date > "2017-09-30" & date < "2017-11-01") | (date > "2018-09-30" & date < "2018-11-01") | (date > "2019-09-30" & date < "2019-11-01"))
table(h5[h5$year==2017, ]$date)
table(h5[h5$year==2018, ]$date)
table(h5[h5$year==2019, ]$date)
dim(h5)

# remove NA from HH_income
h6 = h5
summary(h6$HH_income)
dim(h6)
h6 = h6[!is.na(h5$HH_income),]
dim(h6)
summary(h6$HH_income)

h6_unique = h6[!duplicated(h6$id), ] # data set containing each resp only once (not to be used for trans stats, only adoption stats)
dim(h6_unique)
# check missing weights for h6_unique$weight_171819_1
sum(is.na(h6_unique$weight_171819_1))
dim(h6_unique)
h6_unique = h6_unique[!is.na(h6_unique$weight_171819_1), ]
dim(h6_unique)

#
#Percent of None by income
(none_perc_10k = nrow(h6_unique[h6_unique$HH_income<=10000 & h6_unique$adopt=="None", ])/nrow(h6_unique[h6_unique$HH_income<=10000, ]))
#
(none_perc_20k = nrow(h6_unique[h6_unique$HH_income > 10000 & h6_unique$HH_income <= 20000 & h6_unique$adopt=="None", ])/nrow(h6_unique[h6_unique$HH_income > 10000 & h6_unique$HH_income <= 20000, ]))
#
(none_perc_30k = nrow(h6_unique[h6_unique$HH_income > 20000 & h6_unique$HH_income <= 30000 & h6_unique$adopt=="None", ])/nrow(h6_unique[h6_unique$HH_income > 20000 & h6_unique$HH_income <= 30000, ]))
#
(none_perc_40k = nrow(h6_unique[h6_unique$HH_income > 30000 & h6_unique$HH_income <= 40000 & h6_unique$adopt=="None", ])/nrow(h6_unique[h6_unique$HH_income > 30000 & h6_unique$HH_income <= 40000, ]))
#
(none_perc_50k = nrow(h6_unique[h6_unique$HH_income > 40000 & h6_unique$HH_income <= 50000 & h6_unique$adopt=="None", ])/nrow(h6_unique[h6_unique$HH_income > 40000 & h6_unique$HH_income <= 50000, ]))
#
(none_perc_60k = nrow(h6_unique[h6_unique$HH_income > 50000 & h6_unique$HH_income <= 60000 & h6_unique$adopt=="None", ])/nrow(h6_unique[h6_unique$HH_income > 50000 & h6_unique$HH_income <= 60000, ]))
#
(none_perc_70k = nrow(h6_unique[h6_unique$HH_income > 60000 & h6_unique$HH_income <= 70000 & h6_unique$adopt=="None", ])/nrow(h6_unique[h6_unique$HH_income > 60000 & h6_unique$HH_income <= 70000, ]))

# define adopt2 which separates banked from unbaned
h7 = h6
# below, split (may be used for table)
h7$adopt2=NA
h7$adopt2[h7$dc_adopt==1 & h7$cc_adopt==1] = "Both_cards"
h7$adopt2[h7$cc_adopt==0 & h7$pi != "credit"] = "No_cc"
h7$adopt2[h7$dc_adopt==0 & h7$pi != "debit"] = "No_dc"
h7$adopt2[h7$dc_adopt==0 & h7$cc_adopt==0 & h7$bnk_acnt_adopt==1 & h7$pi != "credit" & h7$pi != "debit"] = "None_banked"
h7$adopt2[h7$dc_adopt==0 & h7$cc_adopt==0 & h7$bnk_acnt_adopt==0 & h7$pi != "credit" & h7$pi != "debit"] = "None_unbanked"
table(h7$adopt)
table(h7$adopt2)# 

# for the table, construct avg monthly exp and avg. monthly bill spending
table(h7$type)# verify payments only
h8 = h7 
dim(h8)
table(h8$bill)
h8_unique = h8[!duplicated(h8$id), ] # all 

# rescaling weight constructing weight_2_unique
names(dplyr::select(h8_unique, contains("weight")))
h8_unique$weight_2_unique = nrow(h8_unique)*h8_unique$weight_171819_1 / sum(h8_unique$weight_171819_1, na.rm = T)
dim(h8_unique)
sum(h8_unique$weight_2_unique, na.rm = T)# = nrow = num resp

## subseting h8 (trans obs) into income groups
h8_0_10k_unique = subset(h8_unique, HH_income >= 0     & HH_income < 10000)
h8_10_20k_unique = subset(h8_unique, HH_income >= 10000 & HH_income < 20000)
h8_20_30k_unique = subset(h8_unique, HH_income >= 20000 & HH_income < 30000)
h8_30_40k_unique = subset(h8_unique, HH_income >= 30000 & HH_income < 40000)
h8_40_60k_unique = subset(h8_unique, HH_income >= 40000 & HH_income < 60000)
h8_60_80k_unique = subset(h8_unique, HH_income >= 60000 & HH_income < 80000)
h8_80_120k_unique = subset(h8_unique, HH_income >= 80000 & HH_income < 120000)
h8_120_180k_unique = subset(h8_unique, HH_income >= 120000 & HH_income < 180000)
h8_180_inf_unique  = subset(h8_unique, HH_income >= 180000)

table(h8$adopt)
table(h8$adopt2)
# frac resp having both CC and DC by income group
(h8_0_10k_cc_dc = nrow(subset(h8_0_10k_unique, adopt=="Both_cards"))/nrow(h8_0_10k_unique))
(h8_10_20k_cc_dc = nrow(subset(h8_10_20k_unique, adopt=="Both_cards"))/nrow(h8_10_20k_unique))
(h8_20_30k_cc_dc = nrow(subset(h8_20_30k_unique, adopt=="Both_cards"))/nrow(h8_20_30k_unique))
(h8_30_40k_cc_dc = nrow(subset(h8_30_40k_unique, adopt=="Both_cards"))/nrow(h8_30_40k_unique))
(h8_40_60k_cc_dc = nrow(subset(h8_40_60k_unique, adopt=="Both_cards"))/nrow(h8_40_60k_unique))
(h8_60_80k_cc_dc = nrow(subset(h8_60_80k_unique, adopt=="Both_cards"))/nrow(h8_60_80k_unique))
(h8_80_120k_cc_dc = nrow(subset(h8_80_120k_unique, adopt=="Both_cards"))/nrow(h8_80_120k_unique))
(h8_120_180k_cc_dc = nrow(subset(h8_120_180k_unique, adopt=="Both_cards"))/nrow(h8_120_180k_unique))
(h8_180_inf_cc_dc = nrow(subset(h8_180_inf_unique, adopt=="Both_cards"))/nrow(h8_180_inf_unique))
(h8_all_cc_dc = nrow(subset(h8_unique, adopt=="Both_cards"))/nrow(h8_unique))
#
# frac resp No CC by income group
(h8_0_10k_no_cc = nrow(subset(h8_0_10k_unique, adopt=="No_cc"))/nrow(h8_0_10k_unique))
(h8_10_20k_no_cc = nrow(subset(h8_10_20k_unique, adopt=="No_cc"))/nrow(h8_10_20k_unique))
(h8_20_30k_no_cc = nrow(subset(h8_20_30k_unique, adopt=="No_cc"))/nrow(h8_20_30k_unique))
(h8_30_40k_no_cc = nrow(subset(h8_30_40k_unique, adopt=="No_cc"))/nrow(h8_30_40k_unique))
(h8_40_60k_no_cc = nrow(subset(h8_40_60k_unique, adopt=="No_cc"))/nrow(h8_40_60k_unique))
(h8_60_80k_no_cc = nrow(subset(h8_60_80k_unique, adopt=="No_cc"))/nrow(h8_60_80k_unique))
(h8_80_120k_no_cc = nrow(subset(h8_80_120k_unique, adopt=="No_cc"))/nrow(h8_80_120k_unique))
(h8_120_180k_no_cc = nrow(subset(h8_120_180k_unique, adopt=="No_cc"))/nrow(h8_120_180k_unique))
(h8_180_inf_no_cc = nrow(subset(h8_180_inf_unique, adopt=="No_cc"))/nrow(h8_180_inf_unique))
(h8_all_no_cc = nrow(subset(h8_unique, adopt=="No_cc"))/nrow(h8_unique))
#
# frac resp No DC by income group
(h8_0_10k_no_dc = nrow(subset(h8_0_10k_unique, adopt=="No_dc"))/nrow(h8_0_10k_unique))
(h8_10_20k_no_dc = nrow(subset(h8_10_20k_unique, adopt=="No_dc"))/nrow(h8_10_20k_unique))
(h8_20_30k_no_dc = nrow(subset(h8_20_30k_unique, adopt=="No_dc"))/nrow(h8_20_30k_unique))
(h8_30_40k_no_dc = nrow(subset(h8_30_40k_unique, adopt=="No_dc"))/nrow(h8_30_40k_unique))
(h8_40_60k_no_dc = nrow(subset(h8_40_60k_unique, adopt=="No_dc"))/nrow(h8_40_60k_unique))
(h8_60_80k_no_dc = nrow(subset(h8_60_80k_unique, adopt=="No_dc"))/nrow(h8_60_80k_unique))
(h8_80_120k_no_dc = nrow(subset(h8_80_120k_unique, adopt=="No_dc"))/nrow(h8_80_120k_unique))
(h8_120_180k_no_dc = nrow(subset(h8_120_180k_unique, adopt=="No_dc"))/nrow(h8_120_180k_unique))
(h8_180_inf_no_dc = nrow(subset(h8_180_inf_unique, adopt=="No_dc"))/nrow(h8_180_inf_unique))
(h8_all_no_dc = nrow(subset(h8_unique, adopt=="No_dc"))/nrow(h8_unique))
#
# frac resp None_banked by income group
(h8_0_10k_none_banked = nrow(subset(h8_0_10k_unique, adopt2=="None_banked"))/nrow(h8_0_10k_unique))
(h8_10_20k_none_banked = nrow(subset(h8_10_20k_unique, adopt2=="None_banked"))/nrow(h8_10_20k_unique))
(h8_20_30k_none_banked = nrow(subset(h8_20_30k_unique, adopt2=="None_banked"))/nrow(h8_20_30k_unique))
(h8_30_40k_none_banked = nrow(subset(h8_30_40k_unique, adopt2=="None_banked"))/nrow(h8_30_40k_unique))
(h8_40_60k_none_banked = nrow(subset(h8_40_60k_unique, adopt2=="None_banked"))/nrow(h8_40_60k_unique))
(h8_60_80k_none_banked = nrow(subset(h8_60_80k_unique, adopt2=="None_banked"))/nrow(h8_60_80k_unique))
(h8_80_120k_none_banked = nrow(subset(h8_80_120k_unique, adopt2=="None_banked"))/nrow(h8_80_120k_unique))
(h8_120_180k_none_banked = nrow(subset(h8_120_180k_unique, adopt2=="None_banked"))/nrow(h8_120_180k_unique))
(h8_180_inf_none_banked = nrow(subset(h8_180_inf_unique, adopt2=="None_banked"))/nrow(h8_180_inf_unique))
(h8_all_none_banked = nrow(subset(h8_unique, adopt2=="None_banked"))/nrow(h8_unique))
#
# frac resp None_unbanked by income group
(h8_0_10k_none_unbanked = nrow(subset(h8_0_10k_unique, adopt2=="None_unbanked"))/nrow(h8_0_10k_unique))
(h8_10_20k_none_unbanked = nrow(subset(h8_10_20k_unique, adopt2=="None_unbanked"))/nrow(h8_10_20k_unique))
(h8_20_30k_none_unbanked = nrow(subset(h8_20_30k_unique, adopt2=="None_unbanked"))/nrow(h8_20_30k_unique))
(h8_30_40k_none_unbanked = nrow(subset(h8_30_40k_unique, adopt2=="None_unbanked"))/nrow(h8_30_40k_unique))
(h8_40_60k_none_unbanked = nrow(subset(h8_40_60k_unique, adopt2=="None_unbanked"))/nrow(h8_40_60k_unique))
(h8_60_80k_none_unbanked = nrow(subset(h8_60_80k_unique, adopt2=="None_unbanked"))/nrow(h8_60_80k_unique))
(h8_80_120k_none_unbanked = nrow(subset(h8_80_120k_unique, adopt2=="None_unbanked"))/nrow(h8_80_120k_unique))
(h8_120_180k_none_unbanked = nrow(subset(h8_120_180k_unique, adopt2=="None_unbanked"))/nrow(h8_120_180k_unique))
(h8_180_inf_none_unbanked = nrow(subset(h8_180_inf_unique, adopt2=="None_unbanked"))/nrow(h8_180_inf_unique))
(h8_all_none_unbanked = nrow(subset(h8_unique, adopt2=="None_unbanked"))/nrow(h8_unique))

## weighted percentage respondents in each income group (row in Table 1)
# payments
nrow(h8)# num payments
nrow(h8_unique)# num resp
sum(h8_unique$weight_2_unique, na.rm = T)# verify nrow = num resp
#
(frac_h8_0_10k_w = sum(h8_0_10k_unique$weight_2_unique, na.rm = T)/sum(h8_unique$weight_2_unique, na.rm = T))
(frac_h8_10_20k_w = sum(h8_10_20k_unique$weight_2_unique, na.rm = T)/sum(h8_unique$weight_2_unique, na.rm = T))
(frac_h8_20_30k_w = sum(h8_20_30k_unique$weight_2_unique, na.rm = T)/sum(h8_unique$weight_2_unique, na.rm = T))
(frac_h8_30_40k_w = sum(h8_30_40k_unique$weight_2_unique, na.rm = T)/sum(h8_unique$weight_2_unique, na.rm = T))
(frac_h8_40_60k_w = sum(h8_40_60k_unique$weight_2_unique, na.rm = T)/sum(h8_unique$weight_2_unique, na.rm = T))
(frac_h8_60_80k_w = sum(h8_60_80k_unique$weight_2_unique, na.rm = T)/sum(h8_unique$weight_2_unique, na.rm = T))
(frac_h8_80_120k_w = sum(h8_80_120k_unique$weight_2_unique, na.rm = T)/sum(h8_unique$weight_2_unique, na.rm = T))
(frac_h8_120_180k_w = sum(h8_120_180k_unique$weight_2_unique, na.rm = T)/sum(h8_unique$weight_2_unique, na.rm = T))
(frac_h8_180_inf_w = sum(h8_180_inf_unique$weight_2_unique, na.rm = T)/sum(h8_unique$weight_2_unique, na.rm = T))
(frac_h8_all_w = sum(h8_unique$weight_2_unique, na.rm = T)/sum(h8_unique$weight_2_unique, na.rm = T))

## UNweighted percentage respondents in each income group (row in Table 1) 
(frac_h8_0_10k = nrow(h8_0_10k_unique)/nrow(h8_unique))
(frac_h8_10_20k = nrow(h8_10_20k_unique)/nrow(h8_unique))
(frac_h8_20_30k = nrow(h8_20_30k_unique)/nrow(h8_unique))
(frac_h8_30_40k = nrow(h8_30_40k_unique)/nrow(h8_unique))
(frac_h8_40_60k = nrow(h8_40_60k_unique)/nrow(h8_unique))
(frac_h8_60_80k = nrow(h8_60_80k_unique)/nrow(h8_unique))
(frac_h8_80_120k = nrow(h8_80_120k_unique)/nrow(h8_unique))
(frac_h8_120_180k = nrow(h8_120_180k_unique)/nrow(h8_unique))
(frac_h8_180_inf = nrow(h8_180_inf_unique)/nrow(h8_unique))
(frac_h8_all = nrow(h8_unique)/nrow(h8_unique))
#
## Sample number respondents in each income group (row in Table 1) 
(num_h8_0_10k = nrow(h8_0_10k_unique))
(num_h8_10_20k = nrow(h8_10_20k_unique))
(num_h8_20_30k = nrow(h8_20_30k_unique))
(num_h8_30_40k = nrow(h8_30_40k_unique))
(num_h8_40_60k = nrow(h8_40_60k_unique))
(num_h8_60_80k = nrow(h8_60_80k_unique))
(num_h8_80_120k = nrow(h8_80_120k_unique))
(num_h8_120_180k = nrow(h8_120_180k_unique))
(num_h8_180_inf = nrow(h8_180_inf_unique))
(num_h8_all = nrow(h8_unique))
#

## Monthly avg dollar non-bill expenditure per respondent
table(h8$bill)
table(h8$year)
length(unique(h8[h8$year==2017,]$id))# num resp in 2017
length(unique(h8[h8$year==2018,]$id))# num resp in 2018
length(unique(h8[h8$year==2019,]$id))# num resp in 2018
h8_2017 = subset(h8, year==2017)# 2017 payments
h8_2018 = subset(h8, year==2018)# 2018 payments
h8_2019 = subset(h8, year==2019)# 2019 payments
#
# 2017 payments within this income group
h8_2017_0_10k = subset(h8_2017, id %in% h8_0_10k_unique$id)
h8_2017_10_20k = subset(h8_2017, id %in% h8_10_20k_unique$id)
h8_2017_20_30k = subset(h8_2017, id %in% h8_20_30k_unique$id)
h8_2017_30_40k = subset(h8_2017, id %in% h8_30_40k_unique$id)
h8_2017_40_60k = subset(h8_2017, id %in% h8_40_60k_unique$id)
h8_2017_60_80k = subset(h8_2017, id %in% h8_60_80k_unique$id)
h8_2017_80_120k = subset(h8_2017, id %in% h8_80_120k_unique$id)
h8_2017_120_180k = subset(h8_2017, id %in% h8_120_180k_unique$id)
h8_2017_180_inf = subset(h8_2017, id %in% h8_180_inf_unique$id)
h8_2017_all = subset(h8_2017, id %in% h8_unique$id)
# 2018 payments within this income group
h8_2018_0_10k = subset(h8_2018, id %in% h8_0_10k_unique$id)
h8_2018_10_20k = subset(h8_2018, id %in% h8_10_20k_unique$id)
h8_2018_20_30k = subset(h8_2018, id %in% h8_20_30k_unique$id)
h8_2018_30_40k = subset(h8_2018, id %in% h8_30_40k_unique$id)
h8_2018_40_60k = subset(h8_2018, id %in% h8_40_60k_unique$id)
h8_2018_60_80k = subset(h8_2018, id %in% h8_60_80k_unique$id)
h8_2018_80_120k = subset(h8_2018, id %in% h8_80_120k_unique$id)
h8_2018_120_180k = subset(h8_2018, id %in% h8_120_180k_unique$id)
h8_2018_180_inf = subset(h8_2018, id %in% h8_180_inf_unique$id)
h8_2018_all = subset(h8_2018, id %in% h8_unique$id)
# 2019 payments within this income group
h8_2019_0_10k = subset(h8_2019, id %in% h8_0_10k_unique$id)
h8_2019_10_20k = subset(h8_2019, id %in% h8_10_20k_unique$id)
h8_2019_20_30k = subset(h8_2019, id %in% h8_20_30k_unique$id)
h8_2019_30_40k = subset(h8_2019, id %in% h8_30_40k_unique$id)
h8_2019_40_60k = subset(h8_2019, id %in% h8_40_60k_unique$id)
h8_2019_60_80k = subset(h8_2019, id %in% h8_60_80k_unique$id)
h8_2019_80_120k = subset(h8_2019, id %in% h8_80_120k_unique$id)
h8_2019_120_180k = subset(h8_2019, id %in% h8_120_180k_unique$id)
h8_2019_180_inf = subset(h8_2019, id %in% h8_180_inf_unique$id)
h8_2019_all = subset(h8_2019, id %in% h8_unique$id)

# 2017 monthly avg num payments per responding by income group
(h8_2017_0_10k_num = (31/3)*nrow(h8_2017_0_10k)/length(unique(h8_2017_0_10k$id)))
(h8_2017_10_20k_num = (31/3)*nrow(h8_2017_10_20k)/length(unique(h8_2017_10_20k$id)))
(h8_2017_20_30k_num = (31/3)*nrow(h8_2017_20_30k)/length(unique(h8_2017_20_30k$id)))
(h8_2017_30_40k_num = (31/3)*nrow(h8_2017_30_40k)/length(unique(h8_2017_30_40k$id)))
(h8_2017_40_60k_num = (31/3)*nrow(h8_2017_40_60k)/length(unique(h8_2017_40_60k$id)))
(h8_2017_60_80k_num = (31/3)*nrow(h8_2017_60_80k)/length(unique(h8_2017_60_80k$id)))
(h8_2017_80_120k_num = (31/3)*nrow(h8_2017_80_120k)/length(unique(h8_2017_80_120k$id)))
(h8_2017_120_180k_num = (31/3)*nrow(h8_2017_120_180k)/length(unique(h8_2017_120_180k$id)))
(h8_2017_180_inf_num = (31/3)*nrow(h8_2017_180_inf)/length(unique(h8_2017_180_inf$id)))
(h8_2017_all_num = (31/3)*nrow(h8_2017)/length(unique(h8_2017$id)))
#
# 2018 monthly avg num payments per responding by income group
(h8_2018_0_10k_num = (31/3)*nrow(h8_2018_0_10k)/length(unique(h8_2018_0_10k$id)))
(h8_2018_10_20k_num = (31/3)*nrow(h8_2018_10_20k)/length(unique(h8_2018_10_20k$id)))
(h8_2018_20_30k_num = (31/3)*nrow(h8_2018_20_30k)/length(unique(h8_2018_20_30k$id)))
(h8_2018_30_40k_num = (31/3)*nrow(h8_2018_30_40k)/length(unique(h8_2018_30_40k$id)))
(h8_2018_40_60k_num = (31/3)*nrow(h8_2018_40_60k)/length(unique(h8_2018_40_60k$id)))
(h8_2018_60_80k_num = (31/3)*nrow(h8_2018_60_80k)/length(unique(h8_2018_60_80k$id)))
(h8_2018_80_120k_num = (31/3)*nrow(h8_2018_80_120k)/length(unique(h8_2018_80_120k$id)))
(h8_2018_120_180k_num = (31/3)*nrow(h8_2018_120_180k)/length(unique(h8_2018_120_180k$id)))
(h8_2018_180_inf_num = (31/3)*nrow(h8_2018_180_inf)/length(unique(h8_2018_180_inf$id)))
(h8_2018_all_num = (31/3)*nrow(h8_2018)/length(unique(h8_2018$id)))
#
# 2019 monthly avg num payments per responding by income group
(h8_2019_0_10k_num = (31/3)*nrow(h8_2019_0_10k)/length(unique(h8_2019_0_10k$id)))
(h8_2019_10_20k_num = (31/3)*nrow(h8_2019_10_20k)/length(unique(h8_2019_10_20k$id)))
(h8_2019_20_30k_num = (31/3)*nrow(h8_2019_20_30k)/length(unique(h8_2019_20_30k$id)))
(h8_2019_30_40k_num = (31/3)*nrow(h8_2019_30_40k)/length(unique(h8_2019_30_40k$id)))
(h8_2019_40_60k_num = (31/3)*nrow(h8_2019_40_60k)/length(unique(h8_2019_40_60k$id)))
(h8_2019_60_80k_num = (31/3)*nrow(h8_2019_60_80k)/length(unique(h8_2019_60_80k$id)))
(h8_2019_80_120k_num = (31/3)*nrow(h8_2019_80_120k)/length(unique(h8_2019_80_120k$id)))
(h8_2019_120_180k_num = (31/3)*nrow(h8_2019_120_180k)/length(unique(h8_2019_120_180k$id)))
(h8_2019_180_inf_num = (31/3)*nrow(h8_2019_180_inf)/length(unique(h8_2019_180_inf$id)))
(h8_2019_all_num = (31/3)*nrow(h8_2019)/length(unique(h8_2018$id)))

# 2017-2018-2019 avg monthly avg num payments per resp (averaging 2017-18)
(h8_0_10k_num = (h8_2017_0_10k_num + h8_2018_0_10k_num + h8_2019_0_10k_num)/3)
(h8_10_20k_num = (h8_2017_10_20k_num + h8_2018_10_20k_num + h8_2019_10_20k_num)/3)
(h8_20_30k_num = (h8_2017_20_30k_num + h8_2018_20_30k_num + h8_2019_20_30k_num)/3)
(h8_30_40k_num = (h8_2017_30_40k_num + h8_2018_30_40k_num + h8_2019_30_40k_num)/3)
(h8_40_60k_num = (h8_2017_40_60k_num + h8_2018_40_60k_num + h8_2019_40_60k_num)/3)
(h8_60_80k_num = (h8_2017_60_80k_num + h8_2018_60_80k_num + h8_2019_60_80k_num)/3)
(h8_80_120k_num = (h8_2017_80_120k_num + h8_2018_80_120k_num + h8_2019_80_120k_num)/3)
(h8_120_180k_num = (h8_2017_120_180k_num + h8_2018_120_180k_num + h8_2019_120_180k_num)/3)
(h8_180_inf_num = (h8_2017_180_inf_num + h8_2018_180_inf_num + h8_2019_180_inf_num)/3)
(h8_all_num = (h8_2017_all_num + h8_2018_all_num + h8_2019_all_num)/3)

# 2017 monthly avg spending per responding by income group
(h8_2017_0_10k_amnt = (31/3)*sum(h8_2017_0_10k$amnt, na.rm = T)/length(unique(h8_2017_0_10k$id)))
(h8_2017_10_20k_amnt = (31/3)*sum(h8_2017_10_20k$amnt, na.rm = T)/length(unique(h8_2017_10_20k$id)))
(h8_2017_20_30k_amnt = (31/3)*sum(h8_2017_20_30k$amnt, na.rm = T)/length(unique(h8_2017_20_30k$id)))
(h8_2017_30_40k_amnt = (31/3)*sum(h8_2017_30_40k$amnt, na.rm = T)/length(unique(h8_2017_30_40k$id)))
(h8_2017_40_60k_amnt = (31/3)*sum(h8_2017_40_60k$amnt, na.rm = T)/length(unique(h8_2017_40_60k$id)))
(h8_2017_60_80k_amnt = (31/3)*sum(h8_2017_60_80k$amnt, na.rm = T)/length(unique(h8_2017_60_80k$id)))
(h8_2017_80_120k_amnt = (31/3)*sum(h8_2017_80_120k$amnt, na.rm = T)/length(unique(h8_2017_80_120k$id)))
(h8_2017_120_180k_amnt = (31/3)*sum(h8_2017_120_180k$amnt, na.rm = T)/length(unique(h8_2017_120_180k$id)))
(h8_2017_180_inf_amnt = (31/3)*sum(h8_2017_180_inf$amnt, na.rm = T)/length(unique(h8_2017_180_inf$id)))
(h8_2017_all_amnt = (31/3)*sum(h8_2017$amnt, na.rm = T)/length(unique(h8_2017$id)))
# 2018 monthly avg spending per responding by income group
(h8_2018_0_10k_amnt = (31/3)*sum(h8_2018_0_10k$amnt, na.rm = T)/length(unique(h8_2018_0_10k$id)))
(h8_2018_10_20k_amnt = (31/3)*sum(h8_2018_10_20k$amnt, na.rm = T)/length(unique(h8_2018_10_20k$id)))
(h8_2018_20_30k_amnt = (31/3)*sum(h8_2018_20_30k$amnt, na.rm = T)/length(unique(h8_2018_20_30k$id)))
(h8_2018_30_40k_amnt = (31/3)*sum(h8_2018_30_40k$amnt, na.rm = T)/length(unique(h8_2018_30_40k$id)))
(h8_2018_40_60k_amnt = (31/3)*sum(h8_2018_40_60k$amnt, na.rm = T)/length(unique(h8_2018_40_60k$id)))
(h8_2018_60_80k_amnt = (31/3)*sum(h8_2018_60_80k$amnt, na.rm = T)/length(unique(h8_2018_60_80k$id)))
(h8_2018_80_120k_amnt = (31/3)*sum(h8_2018_80_120k$amnt, na.rm = T)/length(unique(h8_2018_80_120k$id)))
(h8_2018_120_180k_amnt = (31/3)*sum(h8_2018_120_180k$amnt, na.rm = T)/length(unique(h8_2018_120_180k$id)))
(h8_2018_180_inf_amnt = (31/3)*sum(h8_2018_180_inf$amnt, na.rm = T)/length(unique(h8_2018_180_inf$id)))
(h8_2018_all_amnt = (31/3)*sum(h8_2018$amnt, na.rm = T)/length(unique(h8_2018$id)))
# 2019 monthly avg spending per responding by income group
(h8_2019_0_10k_amnt = (31/3)*sum(h8_2019_0_10k$amnt, na.rm = T)/length(unique(h8_2019_0_10k$id)))
(h8_2019_10_20k_amnt = (31/3)*sum(h8_2019_10_20k$amnt, na.rm = T)/length(unique(h8_2019_10_20k$id)))
(h8_2019_20_30k_amnt = (31/3)*sum(h8_2019_20_30k$amnt, na.rm = T)/length(unique(h8_2019_20_30k$id)))
(h8_2019_30_40k_amnt = (31/3)*sum(h8_2019_30_40k$amnt, na.rm = T)/length(unique(h8_2019_30_40k$id)))
(h8_2019_40_60k_amnt = (31/3)*sum(h8_2019_40_60k$amnt, na.rm = T)/length(unique(h8_2019_40_60k$id)))
(h8_2019_60_80k_amnt = (31/3)*sum(h8_2019_60_80k$amnt, na.rm = T)/length(unique(h8_2019_60_80k$id)))
(h8_2019_80_120k_amnt = (31/3)*sum(h8_2019_80_120k$amnt, na.rm = T)/length(unique(h8_2019_80_120k$id)))
(h8_2019_120_180k_amnt = (31/3)*sum(h8_2019_120_180k$amnt, na.rm = T)/length(unique(h8_2019_120_180k$id)))
(h8_2019_180_inf_amnt = (31/3)*sum(h8_2019_180_inf$amnt, na.rm = T)/length(unique(h8_2019_180_inf$id)))
(h8_2019_all_amnt = (31/3)*sum(h8_2019$amnt, na.rm = T)/length(unique(h8_2019$id)))
#
# 2017-2018-2019 avg monthly avg num payments per resp (averaging 2017-18)
(h8_0_10k_amnt = (h8_2017_0_10k_amnt + h8_2018_0_10k_amnt + h8_2019_0_10k_amnt)/3)
(h8_10_20k_amnt = (h8_2017_10_20k_amnt + h8_2018_10_20k_amnt + h8_2019_10_20k_amnt)/3)
(h8_20_30k_amnt = (h8_2017_20_30k_amnt + h8_2018_20_30k_amnt + h8_2019_20_30k_amnt)/3)
(h8_30_40k_amnt = (h8_2017_30_40k_amnt + h8_2018_30_40k_amnt + h8_2019_30_40k_amnt)/3)
(h8_40_60k_amnt = (h8_2017_40_60k_amnt + h8_2018_40_60k_amnt + h8_2019_40_60k_amnt)/3)
(h8_60_80k_amnt = (h8_2017_60_80k_amnt + h8_2018_60_80k_amnt + h8_2019_60_80k_amnt)/3)
(h8_80_120k_amnt = (h8_2017_80_120k_amnt + h8_2018_80_120k_amnt + h8_2019_80_120k_amnt)/3)
(h8_120_180k_amnt = (h8_2017_120_180k_amnt + h8_2018_120_180k_amnt + h8_2019_120_180k_amnt)/3)
(h8_180_inf_amnt = (h8_2017_180_inf_amnt + h8_2018_180_inf_amnt + h8_2019_180_inf_amnt)/3)
(h8_all_amnt = (h8_2017_all_amnt + h8_2018_all_amnt + h8_2019_all_amnt)/3)

## Finalizing Table 6: Variable column
(variable = c("Both cards (%)", "No credit (%)", "No debit (%)", "None banked (%)", "None unbanked (%)", "Num respondents", "Percentage (%)", "Percentage (%)(w)", "Payments/month", "Spending/month ($)"))
length(variable)
# 0-10k column
(income_0_10k = c(100*h8_0_10k_cc_dc, 100*h8_0_10k_no_cc, 100*h8_0_10k_no_dc, 100*h8_0_10k_none_banked, 100*h8_0_10k_none_unbanked, num_h8_0_10k,100*frac_h8_0_10k,100*frac_h8_0_10k_w, h8_0_10k_num, h8_0_10k_amnt))
(income_10_20k = c(100*h8_10_20k_cc_dc, 100*h8_10_20k_no_cc, 100*h8_10_20k_no_dc, 100*h8_10_20k_none_banked, 100*h8_10_20k_none_unbanked, num_h8_10_20k, 100*frac_h8_10_20k, 100*frac_h8_10_20k_w, h8_10_20k_num, h8_10_20k_amnt))
(income_20_30k = c(100*h8_20_30k_cc_dc, 100*h8_20_30k_no_cc, 100*h8_20_30k_no_dc, 100*h8_20_30k_none_banked, 100*h8_20_30k_none_unbanked, num_h8_20_30k, 100*frac_h8_20_30k, 100*frac_h8_20_30k_w, h8_20_30k_num, h8_20_30k_amnt))
(income_30_40k = c(100*h8_30_40k_cc_dc, 100*h8_30_40k_no_cc, 100*h8_30_40k_no_dc, 100*h8_30_40k_none_banked, 100*h8_30_40k_none_unbanked, num_h8_30_40k, 100*frac_h8_30_40k, 100*frac_h8_30_40k_w, h8_30_40k_num, h8_30_40k_amnt))
(income_40_60k = c(100*h8_40_60k_cc_dc, 100*h8_40_60k_no_cc, 100*h8_40_60k_no_dc, 100*h8_40_60k_none_banked, 100*h8_40_60k_none_unbanked, num_h8_40_60k, 100*frac_h8_40_60k, 100*frac_h8_40_60k_w, h8_40_60k_num, h8_40_60k_amnt))
(income_60_80k = c(100*h8_60_80k_cc_dc, 100*h8_60_80k_no_cc, 100*h8_60_80k_no_dc, 100*h8_60_80k_none_banked, 100*h8_60_80k_none_unbanked, num_h8_60_80k, 100*frac_h8_60_80k, 100*frac_h8_60_80k_w, h8_60_80k_num, h8_60_80k_amnt))
(income_80_120k = c(100*h8_80_120k_cc_dc, 100*h8_80_120k_no_cc, 100*h8_80_120k_no_dc, 100*h8_80_120k_none_banked, 100*h8_80_120k_none_unbanked, num_h8_80_120k, 100*frac_h8_80_120k, 100*frac_h8_80_120k_w, h8_80_120k_num, h8_80_120k_amnt))
(income_120_180k = c(100*h8_120_180k_cc_dc, 100*h8_120_180k_no_cc, 100*h8_120_180k_no_dc, 100*h8_120_180k_none_banked, 100*h8_120_180k_none_unbanked, num_h8_120_180k, 100*frac_h8_120_180k, 100*frac_h8_120_180k_w, h8_120_180k_num, h8_120_180k_amnt))
(income_180_inf = c(100*h8_180_inf_cc_dc, 100*h8_180_inf_no_cc, 100*h8_180_inf_no_dc, 100*h8_180_inf_none_banked, 100*h8_180_inf_none_unbanked, num_h8_180_inf, 100*frac_h8_180_inf, 100*frac_h8_180_inf_w, h8_180_inf_num, h8_180_inf_amnt))
(income_all = c(100*h8_all_cc_dc, 100*h8_all_no_cc, 100*h8_all_no_dc, 100*h8_all_none_banked, 100*h8_all_none_unbanked, num_h8_all, 100*frac_h8_all, 100*frac_h8_all_w, h8_all_num, h8_all_amnt))

## Constructing Table 6 data frame

#displaying only rows 1:8 (not reporting monthly vol and val)
(income.df = data.frame(variable[1:8], income_0_10k[1:8], income_10_20k[1:8], income_20_30k[1:8], income_30_40k[1:8], income_40_60k[1:8], income_60_80k[1:8], income_80_120k[1:8], income_120_180k[1:8], income_180_inf[1:8], income_all[1:8]))
(colnames(income.df) =  c("Card adoption", "0--10k", "10k--20k", "20k--30k", "30k--40k", "40k--60k", "60k--80k", "80k--120k", "120k--180k", "180k+", "All"))
income.df
dim(income.df)

(digitm = matrix(c(rep(1,11+1), rep(1,11+1), rep(1,11+1), rep(1,11+1), rep(1,11+1) , rep(0,11+1) , rep(1,11+1) , rep(1,11+1)), nrow = 8, ncol = 11+1, byrow = T))

print(xtable(income.df, digits = digitm), include.rownames = F, hline.after = c(0,5,8))# Table 6

# testing whether frac rows sum up to 100%
sum(c(frac_h8_0_10k_w, frac_h8_10_20k_w, frac_h8_20_30k_w, frac_h8_30_40k_w, frac_h8_40_60k_w, frac_h8_60_80k_w, frac_h8_80_120k_w, frac_h8_120_180k_w, frac_h8_180_inf_w))# 
sum(c(frac_h8_0_10k, frac_h8_10_20k, frac_h8_20_30k, frac_h8_30_40k, frac_h8_40_60k, frac_h8_60_80k, frac_h8_80_120k, frac_h8_120_180k, frac_h8_180_inf))

# caption of Table 6
length(unique(h8$id))

## End measuring time to run the entire code
end_time = Sys.time()
(run_time = end_time - start_time)

