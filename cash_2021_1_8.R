# cash_2021_1_5.R Cash survey for JEL
## libraries
library(xtable)
library(ggplot2); theme_set(theme_bw())
library(tidyr)# for gather (changing wide to long)

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
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18), 
        axis.text.y = element_text(size = 18),
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
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 18), 
        axis.text.y = element_text(size = 18),
        legend.position = c(0.15, 0.7),
        text = element_text(size = 20)) +
  scale_y_continuous(breaks = seq(0, 650, 50)) +
  ylab("Euro (billions)") +
  scale_color_manual(values = c("darkgreen", "cyan", "blue", "red", "deeppink","violetred4", "black")) +
  scale_shape_manual(values=c(0:6))
# End of figure hoard_eu.f



    
