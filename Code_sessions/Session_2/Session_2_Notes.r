
#Notes can be found at the link below
#https://nbviewer.jupyter.org/github/corybaird/PLCY_Code_2020_winter/blob/main/Code_sessions/Session_2/Session_2_Notes.ipynb

# A.1 Import libraries¶

## Step 1
#install.packages('dplyr')
#install.packages('ggplot2')

#A.2 Import data¶

# Step 2
library(ggplot2)
library(dplyr)

mtcars = as_tibble(mtcars)
mtcars %>% head(2)


# 1. OLS regression
# 1.1 OLS: Built-in package lm
# 1.1.1 Regression summary¶

linear_reg = lm(mpg ~ wt, data=mtcars)

print(summary(linear_reg))


# 1.1.1.1 Accessing coefficients from summary

linear_reg_summary = summary(linear_reg)

linear_reg_summary$coefficients

# F-stat
print('F-stat')
linear_reg_summary$fstatistic

# R-sq
print(paste('R-sq:', linear_reg_summary$r.squared, sep=' '))


# 1.1.1.2 Accessing specific coefficients¶

#linear_reg_summary$coefficients["wt","Std. Error"] #Std. errors

#linear_reg_summary$coefficients["wt","t value"] #T-value

beta1_lm = linear_reg_summary$coefficients["wt","Estimate"] #Beta 

beta1_lm


# 1.1.2 Residuals¶

#linear_reg$fitted.values #Yhat
#linear_reg$coefficients #coefficients
#linear_reg$residuals #Residuals

df_model = mtcars  %>% select(wt,mpg)

df_model = df_model  %>% 
rename('y' = mpg) %>% 
mutate(residu = linear_reg$residuals,
       predicted_y = linear_reg$fitted.values) 

df_model %>% head(5)


# 1.1.2.1 Plot regression residuals¶

#predicted_y == yhat
df_model  %>% ggplot(aes(y, predicted_y))+ geom_point()


# 1.2 OLS: Using matrix algebra¶
# 1.2.1 Create matrices for X and Y values¶

X = as.matrix(mtcars %>% select(wt)) 
X = cbind(X,rep(1,length(X))) #Add constant/intercept
colnames(X)[2] <- "Intercept" #Name intercept
Y = as.matrix(mtcars %>% select(mpg))

# Method 1: SOLVE OLS USING Linear algebra
A = solve(t(X)%*%X)
bhat_hand = A  %*% t(X) %*% Y 
print(bhat_hand)

# Method 2: SOlVE OLS using cov--Note only works for Single linear regression
cov(Y, X)['mpg','wt']/var(X)['wt','wt'] 


# 2. Regression diagnostics¶

#Step 1
#install.packages('lmtest')

#Step 2
library(lmtest)


# 2.1 Breusch pagan: test for heteroskedasticity¶

linear_reg = lm(mpg ~ wt, data=mtcars)
bptest(linear_reg)

### 2.1.1 Create function to check the above test
diagnostics <- function(regression) {
    bp_res = bptest(linear_reg)$p.value
    if (bp_res<.1){
        print(paste("P-value", round(bp_res,3)))
        print("Reject null. Thus there is evidence of heteroskedasticity")
    }
    else{
        print(paste("P-value", round(bp_res,3)))
        print("Cannot reject null. No evidence of heteroskedasticity")
    } 
}

diagnostics(linear_reg)


# 2.2 DW test: test for serial correlation¶

dwtest(linear_reg)



# 2.3 Functional misspecification¶
resettest(linear_reg)


# 2.4 Introduction to functions¶
# 2.4.1 Function: square numbers¶

square = function(a){
  solution = a^2
  return (solution)
}
square(4)


# 2.4.2 Function: add two numbers¶

add_numbers = function(a,b){
    solution = (a^2)+b
    return (solution)
}

add_numbers(2,3)


# 2.4.3 Function: using if and else¶

number_test = function(a,b,c) { 
    print('You can print anything you want inside functions')
    solution1 = a+b
    solution = solution1-c
    print(paste("The solution is:", solution))
    if (solution>100){
        print('Big numbers are lame')
    }
    else{
        print('Small numbers are cool')
    }
}

number_test(1,2,5)


# 2.4.4 Function: BP, DW and RESET Test¶

diagnostics = function(regression) { 
    #BP-test
    print('-------')
    print('BP-Test')
    bp_res = bptest(linear_reg)$p.value
    if (bp_res<.1){
        print(paste("P-value", round(bp_res,3)))
        print("Reject null. Thus there is evidence of heteroskedasticity")
    }
    else{
        print(paste("P-value", round(bp_res,3)))
        print("Cannot reject null. No evidence of heteroskedasticity")
    }
    #DW-test
    print('-------')
    print('DW-Test')
    dw_res = dwtest(linear_reg)$p.value
    if (dw_res<.1){
        print(paste("P-value", round(dw_res,3)))
        print("Reject null. Thus there is evidence of serial correlation")
    }
    else{
        print(paste("P-value", round(bp_res,3)))
        print("Cannot reject null. No evidence of serial correlation")
    } 
    #Ramsey Reset test
    print('-------')
    print('Rest-Test')
    ram_res = resettest(linear_reg)$p.value
    if (ram_res<.1){
        print(paste("P-value", round(ram_res,3)))
        print("Reject null. Thus there is evidence of misspecification")
    }
    else{
        print(paste("P-value", round(ram_res,3)))
        print("Cannot reject null. No evidence of  misspecification")
    } 
}

diagnostics(linear_reg)



# 3. Other regression models
# 3.1 Robust S.E. regression

library(MASS)

summary(rlm(mpg ~ wt, mtcars))


# 3.2 Fixed effects¶

library(plm)

data("Grunfeld", package="plm")
Grunfeld  %>% head(3)


# 3.2.1 Fixed effects: Individual fixed effects¶

grun.fe <- plm(inv~value+capital, index=c('firm','year'), data = Grunfeld, model = "within")
summary(grun.fe)$coefficients


# 3.2.2 Fixed effects: Individual AND time fixed effects¶

#Method 1: factor(year)
grun.fe <- plm(inv~value+capital+factor(year), index=c('firm','year'), data = Grunfeld, model = "within")

#Method 2: effect='twoways'
grun.fe <- plm(inv~value+capital, index=c('firm','year'), data = Grunfeld, model = "within", effect='twoways')
summary(grun.fe)$coefficients


# 3.3 Standard errors
# 3.3.1 Baseline standard error¶

summary(grun.fe)$coefficients


# 3.3.2 Clustered standard error¶

coeftest(grun.fe, vcovHC(grun.fe, type = 'HC0', cluster = c('group','time')))


# 3.4 Random effects¶

grun.re <- plm(inv~value+capital,index=c('firm','year'), data = Grunfeld, model = "random")
summary(grun.re)$coefficients


# 3.4.1 Robust standard errors¶

coeftest(grun.re, vcovHC(grun.re, type = 'HC0', cluster = c('group','time')))


# 3.5 Logistic regression¶

mtcars %>% head(2)

summary(glm(am~ cyl+disp+drat+mpg, family='binomial', mtcars))


# 4. Machine Learning: Supervised learning¶

# Step 1
#install.packages('caret')

# Step 2
library(caret)

data(Sacramento)
house_df = Sacramento %>% as_tibble()
house_df  = house_df %>% mutate(ID = row_number())
house_df %>% head(3)


# 4.1.1. Baseline regression¶

model = lm(price ~ beds+baths+sqft, data=house_df)


# 4.1.1.1 Evaluate model by calculating RMSE¶

rmse = function(acutal, predicted){
    error = predicted - acutal
    rmse = sqrt(mean(error^2))
    return(rmse)
}


# 4.1.1.2 Actual data=price , Predicted data=fitted.values¶

rmse(house_df$price, model$fitted.values)


# 4.1.2 Train-test regression¶

set.seed(42)

train = house_df  %>% sample_frac(.8)
train %>% head(2)

test = house_df[-train$ID,]
test %>% head(2)

model = lm(price ~ beds+baths+sqft, data=train)

p = predict(model, test)

rmse(p, test$price)


# 4.2 Cross validation¶

model <- train(
  price ~ beds+baths+sqft, 
    data=house_df,
  method = "lm",
  trControl = trainControl(
    method = "cv", 
    number = 10,
    verboseIter = FALSE
  )
)
model
