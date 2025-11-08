library(tidyverse)
library(easystats)

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
# facet by Question, color by responder, y=Response, 
# STATS ####
# do mslq categories correlate with other factors?
