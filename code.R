# ---------------------------------------------------------------------------- #
### Load packages, import, clean, and view data, estimate baseline model 
# ---------------------------------------------------------------------------- #
# install.packages("mice")
# install.packages("naniar")
library("mice")
library("naniar")

# import file from web, keep only select variables, name them 
d <- read.csv('https://stats.idre.ucla.edu/wp-content/uploads/2016/02/hsbdemo.dat', header = F)
d <- d[,6:8]
names(d) <- c("read", "write", "math")

# view first 6 rows of dataset 
head(d)

# descriptive statistics 
summary(d)

# scatterplot of reading scores and math scores 
plot(d$read, d$math, col = "darkgreen")
abline(lm(d$math ~ d$read))


# estimate a linear regression using full data
full_model <- lm(math ~ 0 + read + write, data = d)
summary(full_model) 
# ---------------------------------------------------------------------------- #






# ---------------------------------------------------------------------------- #
### Missing Completely at Random (MCAR) 
# ---------------------------------------------------------------------------- #
# create missing values as MCAR   
set.seed(681)
d$na_math <- rbinom(n = nrow(d), size = 1, prob = 0.7) # 70% chance for each obs
head(d)


# two-sample t-test ? 
t.test(math ~ na_math, data = d)


d$math <- ifelse(d$na_math == 1, NA, d$math) # make value NA if indicator == 1
head(d)


# Little's (1988) MCAR test
naniar::mcar_test(d[,c("math", "read", "write")])



# proportion of missing? 
prop.table(table(d$na_math))["1"]



# estimate a linear regression modeling using missing data (listwise deletion)
missing_model <- lm(math ~ 0 + read + write, data = d)
summary(missing_model) # observations deleted due to missingness


# compare estimates from full model vs. listwise deletion model 
coef(full_model)
coef(missing_model)




# start from beginning to create only 20% missing 
d <- read.csv('https://stats.idre.ucla.edu/wp-content/uploads/2016/02/hsbdemo.dat', header = F)
d <- d[,6:8]
names(d) <- c("read", "write", "math")

set.seed(681)
d$na_math <- rbinom(n = nrow(d), size = 1, prob = 0.2)
head(d)
d$math <- ifelse(d$na_math == 1, NA, d$math)
head(d)

# porportion missing 
prop.table(table(d$na_math))["1"]


# estimate missing model 
missing_model <- lm(math ~ 0 + read + write, data = d)


# again compare estimates from full model vs. listwise deletion model 
coef(full_model)
coef(missing_model)
# ---------------------------------------------------------------------------- #









# ---------------------------------------------------------------------------- #
### Missing at Random (MAR) 
# ---------------------------------------------------------------------------- #
d <- read.csv('https://stats.idre.ucla.edu/wp-content/uploads/2016/02/hsbdemo.dat', header = F)
d <- d[,6:8]
names(d) <- c("read", "write", "math")

set.seed(681)

d$na_math <- rbinom(n = nrow(d), size = 1, 
                    prob = ifelse(d$read > quantile(d$read, probs = .7), 
                                  1, 0))


# two-sample t-test ? 
t.test(math ~ na_math, data = d)

# overwrite the income value as missing if the missing indicator == 1 
d$math <- ifelse(d$na_math == 1, NA, d$math)

# view our data
d[100:110,]

# proportion of missing?
prop.table(table(d$na_math))["1"]



# Little's (1988) MCAR test
naniar::mcar_test(d[,c("math", "read", "write")])



# estimate missing model using listwise deletion 
missing_model <- lm(math ~ 0 + read + write, data = d)
summary(missing_model) # observations deleted due to missingness

# compare estimates from both models 
coef(full_model)
coef(missing_model)




# how does multiple imputation perform? 
imps <- mice(d, m = 50, method = "pmm")

res <- with(imps, lm(math ~ 0 + read + write))


# estimates from (1) full model, (2) missing model, and (3) imputation model
coef(full_model)
coef(missing_model)
summary(pool(res))[,2]
# ---------------------------------------------------------------------------- #








# ---------------------------------------------------------------------------- #
### Missing Not at Random (MNAR)  
# ---------------------------------------------------------------------------- #
d <- read.csv('https://stats.idre.ucla.edu/wp-content/uploads/2016/02/hsbdemo.dat', header = F)
d <- d[,6:8]
names(d) <- c("read", "write", "math")


set.seed(681)

d$na_math <- rbinom(n = nrow(d), size = 1, 
                    prob = ifelse(d$math > quantile(d$math, probs = 0.75) & 
                                  d$math < quantile(d$math, probs = 0.95) | 
                                  d$math < quantile(d$math, probs = 0.1),  
                                 1, 0))


# two-sample t-test ? 
t.test(math ~ na_math, data = d)


# overwrite the income value as missing if the missing indicator == 1 
d$math <- ifelse(d$na_math == 1, NA, d$math)

# view our data
d[100:110,]

# proportion of missing?
prop.table(table(d$na_math))["1"]


# Little's (1988) MCAR test
naniar::mcar_test(d[,c("math", "read", "write")])



# estimate missing model using listwise deletion 
missing_model <- lm(math ~ 0 + read + write, data = d)
summary(missing_model) # observations deleted due to missingness


# compare estimates from full model vs. missing model 
coef(full_model)
coef(missing_model)




# how does multiple imputation perform? 
imps <- mice(d, m = 50, method = "pmm")

res <- with(imps, lm(math ~ 0 + read + write))


# estimates from (1) full model, (2) missing model, and (3) imputation model
coef(full_model)
coef(missing_model)
summary(pool(res))[,2]
# ---------------------------------------------------------------------------- #




# # # # 
# END # 
# # # #
