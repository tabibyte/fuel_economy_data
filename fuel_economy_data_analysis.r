library(tidyverse)
library(caret)
library(h2o)
library(plotly)
library(ggplot2)
library(dplyr)
library(glue)
data(mpg)
h2o.init()

mpg <- mpg %>%
  select(cty, year, cyl, displ)

mpg %>% glimpse()

mpg[,-1] <- mpg[,-1] %>% scale() %>% as.data.frame()

h2o_data <- mpg %>% as.h2o()

h2o_data <- h2o_data %>% h2o.splitFrame(ratios = 0.8, seed = 123)
train <- h2o_data[[1]]
test <- h2o_data[[2]]

target <- 'cty'
features <- mpg %>% select(-cty) %>% names()

model <- h2o.glm(
  x = features, y = target,
  training_frame = train,
  validation_frame = test,
  nfolds = 10, seed = 123,
  lambda = 0, compute_p_values = T)

model@model$coefficients_table %>%
  as.data.frame() %>%
  dplyr::select(names,p_value) %>%
  mutate(p_value = round(p_value,3)) %>%
  .[-1,] %>%
  arrange(desc(p_value))

# year - 0.095(.),cyl - 0.003,displ(**) - 0.000(***);

while(model@model$coefficients_table %>%
      as.data.frame() %>%
      dplyr::select(names,p_value) %>%
      mutate(p_value = round(p_value,3)) %>%
      .[-1,] %>%
      arrange(desc(p_value)) %>%
      .[1,2] > 0.05) {
  model@model$coefficients_table %>%
    as.data.frame() %>%
    dplyr::select(names,p_value) %>%
    mutate(p_value = round(p_value,3)) %>%
    filter(!is.nan(p_value)) %>%
    .[-1,] %>%
    arrange(desc(p_value)) %>%
    .[1,1] -> v
  features <- features[features!=v]
  
  train_h2o <- train %>% as.data.frame() %>% select(target,features) %>% as.h2o()
  test_h2o <- test %>% as.data.frame() %>% select(target,features) %>% as.h2o()
  
  model <- h2o.glm(
    x = features, y = target,
    training_frame = train,
    validation_frame = test,
    nfolds = 10, seed = 123,
    lambda = 0, compute_p_values = T)
}

model@model$coefficients_table %>%
  as.data.frame() %>%
  dplyr::select(names,p_value) %>%
  mutate(p_value = round(p_value,3)) 

y_pred <- model %>% h2o.predict(newdata = test) %>% as.data.frame()
y_pred$predict

test_set <- test %>% as.data.frame()
residuals = test_set$cty - y_pred$predict

RMSE = sqrt(mean(residuals^2))

y_test_mean = mean(test_set$cty)

tss = sum((test_set$cty - y_test_mean)^2)
rss = sum(residuals^2)

R2 = 1 - (rss/tss); R2

n <- test_set %>% nrow()
k <- features %>% length()
Adjusted_R2 = 1-(1-R2)*((n-1)/(n-k-1))

tibble(RMSE = round(RMSE,1),
       R2, Adjusted_R2)


my_data <- cbind(predicted = y_pred$predict,
                 observed = test_set$cty) %>% 
  as.data.frame()

g <- my_data %>% 
  ggplot(aes(predicted, observed)) + 
  geom_point(color = "darkred") + 
  geom_smooth(method=lm) + 
  labs(x="Predicted", 
       y="Observed",
       title=glue('Test: Adjusted R2 = {round(enexpr(Adjusted_R2),2)}')) +
  theme(plot.title = element_text(color="darkgreen",size=16,hjust=0.5),
        axis.text.y = element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14))

g %>% ggplotly()