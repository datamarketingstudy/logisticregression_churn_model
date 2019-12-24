library(tidyverse)
library(readxl)
library(janitor)
library(skimr)

## == Data 정제 == ##

churn_dt <- read.csv("telecom_churn.csv")

churn_df <- churn_dt %>%
  clean_names()

str(churn_dt)
head(churn_dt)

churn_list <- skim_to_list(churn_df)


churn_df <- churn_df %>% 
  mutate(churn = factor(churn, levels = c("False", "True"))) %>%
  mutate(area_code = factor(area_code)) %>%
  mutate_at(vars(churn_list$character$variable), as.factor) %>% 
  select(-phone_number) %>% 
  mutate(international_plan = ifelse(str_detect(international_plan, "no"), "No", "Yes"),
         voice_mail_plan    = ifelse(str_detect(voice_mail_plan, "no"), "No", "Yes"))

str(churn_df)

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
  select(churn, account_length, churn_list$numeric$variable) %>% 
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

best_glm <- glm(churn ~ . -total_day_minutes -total_eve_minutes -total_night_minutes -total_intl_minutes
                -number_vmail_messages, 
                family = "binomial", data = train_df)

car::vif(best_glm)


## 예측모형 성능

library(tidypredict)
library(stringr)

test_df <- test_df %>%
  tidypredict_to_column(best_glm) %>% 
  mutate(pred_churn = ifelse(fit > 0.5, "True", "False") %>% as.factor)

confusionMatrix(table(test_df$churn, test_df$pred_churn)) # -- Accuracy 85.6% 우수한 성능의 모델일까?

## ROC 곡선

library(ROCR)
p <- predict(best_glm, newdata = test_df, type = "response")
pr <- prediction(p, test_df$churn)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

## 예측모형 활용

cbind(phone_number = churn_dt[-index_train, "phone.number"], test_df) %>%
  tidypredict_to_column(best_glm) %>% 
  select(phone_number, churn, fit) %>% 
  top_n(10, wt=fit) %>% 
  DT::datatable() %>% 
  DT::formatPercentage("fit", digits=1)

# 예측모형 커뮤니케이션
library(texreg)

screenreg(list(churn_glm, best_glm), custom.note = "")
htmlreg(list(churn_glm, best_glm), custom.note = "")