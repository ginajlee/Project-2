library(dplyr)

# A)
cortax <- read.csv("P02_Corporate tax.csv")

#Equation 1
eq01 <- lm(ypcg~ctax,data=cortax)
summary(eq01)

#Equation 2
eq02 <- lm(ypcg~ctax + ypc2000,data=cortax)
summary(eq02)

#Equation 3
eq03 <- lm(ypcg~ctax + ypc2000 + dty + ctax*dty,data=cortax)
summary(eq03)

#Figure 4 plot avg corporate tax rate '00-'08 and avg GDP per capita growth '00-'15
plot(cortax[,"ctax"],cortax[,"ypcg"], 
     xlab="Average Corporate Tax Rate '00-'08", ylab="Home Price Growth") 
abline(lm(ypcg ~ ctax, data=cortax), col="red")

eq04 <- lm(ypcg~ctax + ypc2000 + dty + trade + ihc + ctax*dty,data=cortax)
summary(eq04)
# To find the best model, x has to be more exogenous. In the case that x = average corporate tax
# rate from 2000-2008, it is unlikely that GDP per capita growth rate from 2000-2015 will impact x. 

# B)
data09 <- read.csv("ACS_09_5YR_DP02_with_ann.csv")
data09a <- data09[-c(1),c(2,3,232,238,242,246,250,254,258,262)]
colnames(data09a) <- c("ID", "Geography", "Adult population", "% lower than 9th grade",
                       "% 9th to 12th grade", "% high school grad", "% some college",
                       "% Associates degree", "% Bachelors degree", "% graduate and professional degree")

data10 <- read.csv("ACS_10_5YR_DP02_with_ann.csv")
data10a <- data10[-1,c(2,3,232,238,242,246,250,254,258,262)]
colnames(data10a) <- c("ID", "Geography", "Adult population", "% lower than 9th grade",
                       "% 9th to 12th grade", "% high school grad", "% some college",
                       "% Associates degree", "% Bachelors degree", "% graduate and professional degree")

data11 <- read.csv("ACS_11_5YR_DP02_with_ann.csv")
data11a <- data11[-1,c(2,3,232,238,242,246,250,254,258,262)]
colnames(data11a) <- c("ID", "Geography", "Adult population", "% lower than 9th grade",
                       "% 9th to 12th grade", "% high school grad", "% some college",
                       "% Associates degree", "% Bachelors degree", "% graduate and professional degree")

data12 <- read.csv("ACS_12_5YR_DP02_with_ann.csv")
data12a <- data12[-1,c(2,3,232,238,242,246,250,254,258,262)]
colnames(data12a) <- c("ID", "Geography", "Adult population", "% lower than 9th grade",
                       "% 9th to 12th grade", "% high school grad", "% some college",
                       "% Associates degree", "% Bachelors degree", "% graduate and professional degree")

data13 <- read.csv("ACS_13_5YR_DP02_with_ann.csv")
data13a <- data13[-1,c(2,3,232,238,242,246,250,254,258,262)]
colnames(data13a) <- c("ID", "Geography", "Adult population", "% lower than 9th grade",
                       "% 9th to 12th grade", "% high school grad", "% some college",
                       "% Associates degree", "% Bachelors degree", "% graduate and professional degree")

data14 <- read.csv("ACS_14_5YR_DP02_with_ann.csv")
data14a <- data14[-1,c(2,3,232,238,242,246,250,254,258,262)]
colnames(data14a) <- c("ID", "Geography", "Adult population", "% lower than 9th grade",
                       "% 9th to 12th grade", "% high school grad", "% some college",
                       "% Associates degree", "% Bachelors degree", "% graduate and professional degree")

data15 <- read.csv("ACS_15_5YR_DP02_with_ann.csv")
data15a <- data15[-1,c(2,3,232,238,242,246,250,254,258,262)]
colnames(data15a) <- c("ID", "Geography", "Adult population", "% lower than 9th grade",
                       "% 9th to 12th grade", "% high school grad", "% some college",
                       "% Associates degree", "% Bachelors degree", "% graduate and professional degree")

data16 <- read.csv("ACS_16_5YR_DP02_with_ann.csv")
data16a <- data16[-1,c(2,3,232,238,242,246,250,254,258,262)]
colnames(data16a) <- c("ID", "Geography", "Adult population", "% lower than 9th grade",
                       "% 9th to 12th grade", "% high school grad", "% some college",
                       "% Associates degree", "% Bachelors degree", "% graduate and professional degree")

a <- as.numeric(as.character(data09a[,4]))*50
b <- as.numeric(as.character(data09a[,5]))*100
c <- as.numeric(as.character(data09a[,6]))*120
d <- as.numeric(as.character(data09a[,7]))*130
e <- as.numeric(as.character(data09a[,8]))*140
f <- as.numeric(as.character(data09a[,9]))*190
g <- as.numeric(as.character(data09a[,10]))*230

data09a$chci09 = (1/100)*(a+b+c+d+e+f+g)
data09a <- data09a[c(1,2,3,11)]
colnames(data09a) <- c("ID", "County", "pop09", "chci09")

a <- as.numeric(as.character(data10a[,4]))*50
b <- as.numeric(as.character(data10a[,5]))*100
c <- as.numeric(as.character(data10a[,6]))*120
d <- as.numeric(as.character(data10a[,7]))*130
e <- as.numeric(as.character(data10a[,8]))*140
f <- as.numeric(as.character(data10a[,9]))*190
g <- as.numeric(as.character(data10a[,10]))*230

data10a$chci10 = (1/100)*(a+b+c+d+e+f+g)
data10a <- data10a[c(1,3,11)]
colnames(data10a) <- c("ID", "pop10", "chci10")

a <- as.numeric(as.character(data11a[,4]))*50
b <- as.numeric(as.character(data11a[,5]))*100
c <- as.numeric(as.character(data11a[,6]))*120
d <- as.numeric(as.character(data11a[,7]))*130
e <- as.numeric(as.character(data11a[,8]))*140
f <- as.numeric(as.character(data11a[,9]))*190
g <- as.numeric(as.character(data11a[,10]))*230

data11a$chci11 = (1/100)*(a+b+c+d+e+f+g)
data11a <- data11a[c(1,3,11)]
colnames(data11a) <- c("ID", "pop11", "chci11")

a <- as.numeric(as.character(data12a[,4]))*50
b <- as.numeric(as.character(data12a[,5]))*100
c <- as.numeric(as.character(data12a[,6]))*120
d <- as.numeric(as.character(data12a[,7]))*130
e <- as.numeric(as.character(data12a[,8]))*140
f <- as.numeric(as.character(data12a[,9]))*190
g <- as.numeric(as.character(data12a[,10]))*230

data12a$chci12 = (1/100)*(a+b+c+d+e+f+g)
data12a <- data12a[c(1,3,11)]
colnames(data12a) <- c("ID", "pop12", "chci12")

a <- as.numeric(as.character(data13a[,4]))*50
b <- as.numeric(as.character(data13a[,5]))*100
c <- as.numeric(as.character(data13a[,6]))*120
d <- as.numeric(as.character(data13a[,7]))*130
e <- as.numeric(as.character(data13a[,8]))*140
f <- as.numeric(as.character(data13a[,9]))*190
g <- as.numeric(as.character(data13a[,10]))*230

data13a$chci13 = (1/100)*(a+b+c+d+e+f+g)
data13a <- data13a[c(1,3,11)]
colnames(data13a) <- c("ID", "pop13", "chci13")

a <- as.numeric(as.character(data14a[,4]))*50
b <- as.numeric(as.character(data14a[,5]))*100
c <- as.numeric(as.character(data14a[,6]))*120
d <- as.numeric(as.character(data14a[,7]))*130
e <- as.numeric(as.character(data14a[,8]))*140
f <- as.numeric(as.character(data14a[,9]))*190
g <- as.numeric(as.character(data14a[,10]))*230

data14a$chci14 = (1/100)*(a+b+c+d+e+f+g)
data14a <- data14a[c(1,3,11)]
colnames(data14a) <- c("ID", "pop14", "chci14")

a <- as.numeric(as.character(data15a[,4]))*50
b <- as.numeric(as.character(data15a[,5]))*100
c <- as.numeric(as.character(data15a[,6]))*120
d <- as.numeric(as.character(data15a[,7]))*130
e <- as.numeric(as.character(data15a[,8]))*140
f <- as.numeric(as.character(data15a[,9]))*190
g <- as.numeric(as.character(data15a[,10]))*230

data15a$chci15 = (1/100)*(a+b+c+d+e+f+g)
data15a <- data15a[c(1,3,11)]
colnames(data15a) <- c("ID", "pop15", "chci15")

a <- as.numeric(as.character(data16a[,4]))*50
b <- as.numeric(as.character(data16a[,5]))*100
c <- as.numeric(as.character(data16a[,6]))*120
d <- as.numeric(as.character(data16a[,7]))*130
e <- as.numeric(as.character(data16a[,8]))*140
f <- as.numeric(as.character(data16a[,9]))*190
g <- as.numeric(as.character(data16a[,10]))*230

data16a$chci16 = (1/100)*(a+b+c+d+e+f+g)
data16a <- data16a[c(1,3,11)]
colnames(data16a) <- c("ID", "pop16", "chci16")

final=merge(data09a, data10a, by="ID", all=TRUE)
final=merge(final, data11a, by="ID", all=TRUE)
final=merge(final, data12a, by="ID", all=TRUE)
final=merge(final, data13a, by="ID", all=TRUE)
final=merge(final, data14a, by="ID", all=TRUE)
final=merge(final, data15a, by="ID", all=TRUE)
final=merge(final, data16a, by="ID", all=TRUE)
final=final[c(1,2,4,6,8,10,12,14,16,18,3,5,7,9,11,13,15,17)]

final$chcig = final$chci16-final$chci09
final$chcigr = final$chcig/final$chci09
final$popg = as.numeric(as.character(final$pop16))-as.numeric(as.character(final$pop09))
final$popgr = final$popg/as.numeric(as.character(final$pop09))

write.csv(final, file = "Project2.csv")

final$pop16 <- as.numeric(as.character(final$pop16))

chciprob01=head(arrange(final,desc(chci16)), n=20)
chciprob02=head(arrange(final,desc(chcig)), n=20)
chciprob03=head(arrange(final,desc(pop16)), n=20)
chciprob04=head(arrange(final,desc(popg)), n=20)
chciprob05=head(arrange(final,desc(popgr)), n=20)                
chciprob06=head(arrange(final,chci16), n=20)
chciprob07=head(arrange(final,chcig), n=20)
chciprob08=head(arrange(final,popg), n=20)
chciprob09=head(arrange(final,popgr), n=20)

chcitop20=rbind(chciprob01, NA,chciprob02,NA,chciprob06,NA,chciprob07,NA,chciprob03,
               NA,chciprob04,NA,chciprob05,NA,chciprob08,NA,chciprob09)

write.csv(chcitop20, file = "Project2a.csv")
