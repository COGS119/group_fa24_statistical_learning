library(here)
library(tidyverse)

processed_data_directory <- here("..","data","processed_data")
file_name <- "statistical_learning"

#read experiment data
processed_data <- read_csv(here(processed_data_directory,paste0(file_name,"-processed-data.csv")))


#average accuracy
avg_subj_acc <- processed_data %>%
  group_by(participant_id,random_id) %>%
  summarize(
    avg_acc = mean(is_right),
    avg_rt = mean(rt[is_right==1])
  ) %>%
  mutate(
    group="statistical learning"
  )

overall_acc <- avg_subj_acc %>%
  group_by(group) %>%
  summarize(
    N=n(),
    average_accuracy = mean(avg_acc),
    sd = sd(avg_acc),
    sem = sd / sqrt(N)
  )

ggplot(avg_subj_acc,aes(x=group,avg_acc))+
  geom_violin(width=0.5)+
  geom_dotplot(binaxis="y",stackdir="center",alpha=0.2, fill="#097969")+
  geom_point(data=overall_acc,aes(y=average_accuracy),size=5)+
  geom_errorbar(data=overall_acc,aes(y=average_accuracy,ymin=average_accuracy-sem,ymax=average_accuracy+sem),width=0)+
  geom_hline(yintercept=0.5,linetype="dashed")+
  ylab("Accuracy")+
  xlab("")+
  theme_bw(base_size=16)
  