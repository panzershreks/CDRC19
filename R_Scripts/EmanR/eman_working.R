library('dplyr')
library('broom')

data <- read.csv("./../../Combined DataFrame Work/fully_merged_df.csv")
lm(data)
grep("mortality", names(data), value = TRUE)
grep("death", names(data), value = TRUE)
lm(formula = Total.confirmed.deaths.due.to.COVID.19.per.million.people~., data=data)

data %>%
  summarise_each(funs(list(levels(.))))

test_df <- airquality
lm_all <- lm(Wind~.,data=test_df)
tm <- tidy(lm_all)
x <- tm$term[which.max(tm$p.value)] # term w largest p val
foo <- tm$term[tm$p.value < 0.05]
foo[foo != "(Intercept)"]
coef(lm_all)
summary(lm_all)
summary(lm_all)$coefficients[,4]
summary(lm(Wind~.-Day,data=test_df))
summary(lm(Wind~1+Temp,data=test_df))
lt <- colnames(test_df)
lt <- lt[lt != str(Wind)]
lt[names(lt) != "Wind"]
lt[1]
lt[-1]
f <- "Wind ~ Day + Temp"
lm(as.formula(f), test_df)
as.formula(f)
summary(lm(f,test_df))

all_vars <- colnames(data)
explan_vars <- all_vars[all_vars != response_var]
lm_vars <- c()

test_var <- explan_vars[1]
explan_vars <- explan_vars[-1]

backwards_step <- function(data, response_var, p_val){
    formula <- paste0(response_var, " ~ .")
    model <- lm(as.formula(formula), data)
    tm <- tidy(model)
    tm <- subset(tm, term != "(Intercept)")
    print(tm)
    non_sig_vars <- tm$term[tm$p.value > p_val]
    iter <- 0
    print(paste0("----------Iteration: ", iter,"----------"))
    print(summary(model))
    while (length(non_sig_vars) > 0){
        remove_var <- tm$term[which.max(tm$p.value)]
        print(remove_var)
        formula <- paste0(formula, " - ", remove_var)
        model <- lm(as.formula(formula), data)
        tm <- tidy(model)
        tm <- subset(tm, term != "(Intercept)")
        non_sig_vars <- tm$term[tm$p.value > p_val]
        iter <- iter + 1
        print(paste0("----------Iteration: ", iter,"----------"))
        print(summary(model))
    }
}
backwards_step(data = test_df, response_var = 'Solar.R', p_val=0.05)



