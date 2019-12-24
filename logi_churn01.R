library(tidyverse)
library(readxl)
library(janitor)
library(skimr)

## == Data 정제 == ##

churn_dt <- read.csv("dataset.csv")

churn_df <- churn_dt %>%
  clean_names()

str(churn_dt)

churn_list <- skim_to_list(churn_df)


churn_df <- churn_df %>% 
  mutate(churn = factor(churn, levels = c("No", "Yes"))) %>% 
  mutate_at(vars(churn_list$character$variable), as.factor) %>% 
  mutate(senior_citizen = factor(senior_citizen)) %>% 
  select(-customer_id) %>% 
  mutate(multiple_lines    = ifelse(str_detect(multiple_lines, "No"), "No", "Yes"),
         internet_service  = ifelse(str_detect(internet_service, "No"), "No", "Yes"),
         online_security   = ifelse(str_detect(online_security, "No"), "No", "Yes"),
         online_backup     = ifelse(str_detect(online_backup, "No"), "No", "Yes"),
         device_protection = ifelse(str_detect(device_protection, "No"), "No", "Yes"),
         tech_support      = ifelse(str_detect(tech_support, "No"), "No", "Yes"),
         streaming_tv      = ifelse(str_detect(streaming_tv, "No"), "No", "Yes"),
         streaming_movies  = ifelse(str_detect(streaming_movies, "No"), "No", "Yes")) 


## == Data 통계 == ##

### 이탈률(Churn rate) ###

churn_df %>%
  summarise(churn_rate = mean(as.integer(churn)-1))

## == Data 시각화 == ##

library(ggridges)
library(cowplot)

## 연속형 변수 ------

y_p <- churn_df %>%
  ggplot(aes(x = churn, fill = churn)) +
  geom_bar(alpha = 0.8) +
  scale_fill_manual(values = c("gray", "Light Coral")) +
  guides(fill = FALSE)

x_cont_p <- churn_df %>%
  select(churn, tenure, churn_list$numeric$variable) %>% 
  gather(variable, value, -churn) %>% 
  mutate(value = as.integer(value)) %>%
  ggplot(aes(x = value, y = churn, fill = churn)) +
  facet_wrap( ~ variable, scale = "free", ncol = 2) +
  scale_fill_manual(values = c("gray", "Light Coral")) +
  geom_density_ridges(alpha = 0.8) +
  guides(fill = FALSE, color = FALSE)

plot_grid(y_p, x_cont_p, rel_widths = c(1,3))

## 범주형 변수

x_cat_p <- churn_df %>%
  select_if(is.factor) %>% 
  gather(variable, value, -churn) %>% 
  group_by(churn) %>% 
  count(variable, value) %>% 
  ungroup() %>% 
  ggplot(data = ., aes(x=value, y=n, fill=churn)) + 
  geom_col(position="dodge", width=0.7) +
  facet_wrap(~variable, scale = "free", ncol = 4) +
  scale_fill_manual(values = c("gray", "Light Coral")) +
  guides(fill = FALSE, color = FALSE)

plot_grid(y_p, x_cat_p, rel_widths = c(1,3))

## IV (Information Value) ##

library(Information) 

churn_iv_df <- churn_df %>% 
  mutate(churn = as.integer(churn) -1 )

churn_iv <- create_infotables(data=churn_iv_df, y="churn", bins=10, parallel=TRUE)

churn_iv$Summary %>% 
  mutate(Variable = fct_reorder(Variable, IV)) %>% 
  ggplot(aes(x=Variable, y=IV)) +
  geom_col() +
  coord_flip()

churn_iv$Summary %>% 
  top_n(6, wt=IV)

## 예측모형 생성

library(caret)

index_train <- createDataPartition(churn_df$churn, p = 0.7, list = FALSE)

train_df <- churn_df[index_train, ]
test_df  <- churn_df[-index_train, ]

## VIF 과적합 방지

churn_glm <- glm(churn ~ ., 
                 family = "binomial", data = train_df)

car::vif(churn_glm)

best_glm <- glm(churn ~ . -total_charges, 
                family = "binomial", data = train_df)

car::vif(best_glm)

## 예측모형 성능

library(tidypredict)
library(stringr)

test_df <- test_df %>%
  tidypredict_to_column(best_glm) %>% 
  mutate(pred_churn = ifelse(fit > 0.5, "Yes", "No") %>% as.factor)

confusionMatrix(table(test_df$churn, test_df$pred_churn))

## ROC 곡선

library(ROCR)
p <- predict(best_glm, newdata = test_df, type = "response")
pr <- prediction(p, test_df$churn)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

## 예측모형 활용

cbind(customerID = churn_dt[-index_train, "customerID"], test_df) %>%
  tidypredict_to_column(best_glm) %>% 
  select(customerID, churn, fit) %>% 
  top_n(10, wt=fit) %>% 
  DT::datatable() %>% 
  DT::formatPercentage("fit", digits=1)