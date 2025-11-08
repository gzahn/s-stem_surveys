library(tidyverse)
library(easystats)
library(lmerTest)
library(MASS)
library(broom)
library(textdata)
library(tidytext)
afinn <- get_sentiments()


# themes
theme_set(theme_minimal() +
            theme(strip.text = element_text(face='bold',size=12),
                  legend.title = element_text(face='bold',size=14),
                  legend.text = element_text(face='bold',size=12),
                  axis.text = element_text(face='bold',size=10),
                  axis.title = element_text(face='bold',size=12)))


# Load cleaned data ####
mslq <- readRDS("./data/mslq.RDS")
mcat <- readRDS("./data/mcat.RDS")
impact <- readRDS("./data/impact.RDS")

# Summary plots ####

mslq %>% 
  ggplot(aes(x=date,y=Response_numeric,color=factor(cohort))) +
  geom_jitter(alpha=.25,size=.25,color="gray30",width = 10) +
  geom_smooth() +
  facet_wrap(~factor(cohort)*factor_group) +
  labs(color="Cohort") +
  labs(x="Date",y="Self evaluation") +
  scale_color_viridis_d(option = 'turbo') +
  theme(axis.text.x = element_text(angle = 90,hjust=1,vjust=.5))
ggsave("./output/mslq_grouped_trends_by_cohort.png",dpi=300,height = 7,width = 12)  

mslq %>% 
  ggplot(aes(x=date,y=Response_numeric,color=factor(uvid))) +
  geom_smooth(se=FALSE) +
  facet_wrap(~factor(cohort)*factor_group) +
  theme(legend.position = 'none') +
  scale_color_viridis_d(option='turbo') +
  labs(x="Date",y="Self evaluation")
ggsave("./output/mslq_individual_trends.png",dpi=300,height = 7,width = 10)  

mslq %>% 
  ggplot(aes(x=factor_group,y=Response_numeric,fill=gender)) +
  geom_boxplot() +
  # theme(axis.text.x = element_text(angle=60,hjust=1)) +
  labs(x="MSLQ category",y="Self evaluation",fill="Sex") +
  scale_fill_viridis_d(begin = .2)
ggsave("./output/mslq_grouped_by_gender.png",dpi=300,height = 4,width = 8)

mslq %>% 
  ggplot(aes(x=factor_group,y=Response_numeric,fill=maritalStatus)) +
  geom_boxplot() +
  # theme(axis.text.x = element_text(angle=60,hjust=1)) +
  labs(x="MSLQ category",y="Self evaluation",fill="Sex") +
  scale_fill_viridis_d(begin = .2)
ggsave("./output/mslq_grouped_by_marital.png",dpi=300,height = 4,width = 8)


mcat %>% glimpse
mcat %>% 
  ggplot(aes(x=date,y=Response,color=responder_type)) +
  geom_smooth() +
  facet_wrap(~Question,labeller= label_wrap_gen()) +
  scale_color_viridis_d(option='turbo',begin=.3) +
  theme(strip.text = element_text(size=8)) +
  labs(color="Responder",x="Date")
ggsave("./output/mcat_students_vs_faculty.png",dpi=300,width = 14,height = 8)

# Grouped faculty
mcat %>% 
  dplyr::filter(responder_type == "Faculty") %>% 
  ggplot(aes(x=date,y=Response)) +
  geom_jitter(alpha=.25,size=.25,color="gray30",width = 10) +
  geom_smooth(method="lm",se=TRUE,color="black") +
  facet_wrap(~Question,labeller= label_wrap_gen()) +
  theme(legend.position = "none") +
  theme(strip.text = element_text(size=8)) +
  scale_color_viridis_d(option='turbo') +
  labs(color="Responder",x="Date")
ggsave("./output/mcat_grouped_faculty_over_time.png",dpi=300,height = 12,width = 14)

# summarized MCAT: faculty and students over time
mcat %>% 
  ggplot(aes(x=date,y=Response,color=responder_type)) +
  geom_jitter(alpha=.25,size=.25,color="gray30",width = 10) +
  geom_smooth(method="lm",se=TRUE) +
  labs(color="Responder",x="Date",y="Overall MCAT response mean") +
  scale_color_viridis_d(option='turbo',begin=.2)
ggsave("./output/mcat_summarized_over_time.png",dpi=300,height = 6,width = 6)




# Individual faculty
mcat %>% 
  dplyr::filter(responder_type == "Faculty") %>% 
  ggplot(aes(x=date,y=Response)) +
  geom_jitter(alpha=.25,size=.25,color="gray30",width = 10) +
  geom_smooth(method="lm",se=FALSE,aes(color=factor(uvid))) +
  facet_wrap(~Question,labeller= label_wrap_gen()) +
  theme(legend.position = "none") +
  theme(strip.text = element_text(size=8)) +
  scale_color_viridis_d(option='turbo')
ggsave("./output/mcat_individual_faculty_over_time.png",dpi=300,height = 12,width = 14)
  
# combine mslq with students' mcat
combined <- 
mcat %>% 
  mutate(uvid=as.character(uvid)) %>% 
  dplyr::filter(responder_type == "Student") %>% 
  dplyr::select(uvid, cohort, gender, Question, ID, Response, date) %>% 
  rename("MCAT_ID"="ID",
         "MCAT_Question"="Question",
         "MCAT_Response"="Response") %>% 
  full_join(
  mslq %>% dplyr::select(uvid, ID, Question, factor_group, Response_numeric, date) %>% 
    mutate(uvid=as.character(uvid)) %>% 
    rename("MSLQ_Response"="Response_numeric",
           "MSLQ_Question"="Question",
           "MSLQ_ID"="ID")
  ) %>% 
  unique.data.frame() 



combined_list <- combined %>% split.data.frame(~uvid)
combined <- combined_list %>% map(unique.data.frame) %>% reduce(bind_rows)
impact <- 
  impact %>% 
  mutate(uvid=as.character(uvid)) %>% 
  dplyr::select(uvid, Response, ID) %>% # no date for impact so it can join
  rename("Impact_ID"="ID",
         #"Impact_Question"="Question",
         "Impact_Response"="Response")  %>% 
  split.data.frame(~uvid) %>% 
  map(unique.data.frame) %>% reduce(bind_rows)


combined <- 
  combined %>% 
  full_join(impact)


# STATS ####
# do mslq categories correlate with other factors?



# do faculty get better over time?
mod_faculty_mcat_over_time <- 
  mcat %>% 
  dplyr::filter(responder_type == "Faculty") %>% 
  lmer(data=.,
       formula = Response ~ date + (1|uvid))

sink("./output/mcat_faculty_responses_over_time.txt")
report(mod_faculty_mcat_over_time)
sink(NULL)

# do faculty and students have different opinions of mentoring skills?
mod_student_vs_faculty_mcat <- 
  glm(data=mcat, formula = Response ~ responder_type * date)

sink("./output/mcat_faculty_vs_student_assessment.txt")
report(mod_student_vs_faculty_mcat)
summary(mod_student_vs_faculty_mcat)
sink(NULL)


# do mlsq scores correlate with students' rating of their mentors? 
mod_impact_full <- 
  lmer(data=combined,
       formula = Impact_Response ~ MCAT_Response + MSLQ_Response + (1|uvid))

sink("./output/mcat_and_mslq_effect_on_mentorship.txt")
cat("There is no significant effect of MCAT or MSLQ answers on student perception of mentor impact.")
report(mod_impact_full)
summary(mod_impact_full)
sink(NULL)
rm(mod_impact_full)

# do mlsq scores correlate with students' sentiment scores? 

read_csv("./data/edited_impact_of_relationship.csv") %>% 
  dplyr::select(firstName,lastName,`Q5.1`) %>% 
  group_by(firstName,lastName) %>% 
  summarize(free_response = `Q5.1` %>% paste(collapse = " "))
sentiment <- readRDS("./data/impact.RDS") %>% dplyr::select(uvid,sentiment) %>% 
  group_by(uvid) %>% 
  summarize(mean_sentiment = mean(sentiment,na.rm=TRUE))

mslq %>% 
  left_join(sentiment) %>% 
  ggplot(aes(x=Response_numeric,y=mean_sentiment,color=factor_group)) +
  geom_point() +
  geom_smooth(method='lm') +
  labs(subtitle = "Student free response sentiment about mentor\nnot correlated with MSLQ responses")
ggsave("./output/free_response_vs_mslq.png")


# do mcat scores correlate with students' sentiment scores? 

mcat %>% 
  dplyr::filter(responder_type == "Student") %>% 
  left_join(sentiment) %>%
  ggplot(aes(x=Response,y=mean_sentiment,color=)) +
  geom_point() +
  geom_smooth(method='lm') +
  labs(subtitle = "Student free response sentiment about mentor\nnot correlated with MCAT responses")
ggsave("./output/free_response_vs_mcat.png")

# do mentorship ratings correlate with students' sentiment scores?
