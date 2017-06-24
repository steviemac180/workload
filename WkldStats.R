### descriptive and comparative statistics for the workload trends

#average the means over years and quarters
# average per year

year.means <- df.quarters %>% filter(Year > 2013) %>% droplevels %>%
  group_by(Year, Section) %>%
  summarise(Wkld = mean(Workload)) %>%
  arrange(Section)
year.means

ggplot(data = year.means, aes(x= Year, y= Wkld)) + 
  geom_col(aes(fill=Section)) +
  ylab("Mean Annual Workload") # deliverable

# by quarter
quarter.means <- df.quarters %>% filter(Year > 2013) %>% droplevels %>%
  group_by(Year, vec.quarters, Section) %>%
  summarise(Qwkld = mean(Workload)) %>%
  arrange(Section)
quarter.means

ggplot(data = quarter.means, aes(x= vec.quarters, y= Qwkld)) + 
  geom_col(aes(fill=Year)) +
  ylab("Total Quarterly Workload") # deliverable

# compare over time
# filter to use for anova - Haemophilia
df.haem <- df.quarters %>% filter(Section == "Haemophilia", Year > 2013) %>% droplevels
# run anova
aov1 <- aov(Workload~Year+vec.quarters+Month,data=df.haem)
summary(aov1)

# filter to use for anova - Thrombophilia
df.thrombo <- df.quarters %>% filter(Section == "Thrombophilia", Year > 2013) %>% droplevels
# run anova
aov2 <- aov(Workload~Year+vec.quarters+Month,data=df.thrombo)
summary(aov2)

# filter to use for anova - AcuStar
df.acustar <- df.quarters %>% filter(Section == "AcuStar", Year > 2013) %>% droplevels
# run anova
aov3 <- aov(Workload~Year+vec.quarters+Month,data=df.acustar)
summary(aov3)

# filter to use for anova - Miscellaneous
df.misc <- df.quarters %>% filter(Section == "Miscellaneous", Year > 2013) %>% droplevels
# run anova
aov4 <- aov(Workload~Year+vec.quarters+Month,data=df.misc)
summary(aov4)

# percent increase year upon year
year.wkld <- df.quarters %>% filter(Year > 2013) %>%
  group_by(Year, Section) %>%
  summarise(year.average = mean(Workload)) %>%
  arrange(Section)
  
pct.increase <- year.wkld %>% group_by(Section) %>%
  mutate_each(funs(pct), year.average) %>%
  mutate(Percent.increase = round((year.average-1.0)*100),2)

ggplot(data=pct.increase, aes(x = Section, y = Percent.increase, color=Year)) +
  ylab("Monthly average - Percent of previous year's workload") +
  ggtitle("Percent increase of mean monthly workload by year from 2015") +
  geom_text(aes(label=Percent.increase), size=5) # deliverable

## same comparing quarterly stats

quarter.wkld <- df.quarters %>% filter(Year > 2010, Year < 2017) %>%
  group_by(Year, vec.quarters, Section) %>%
  summarise(quarter.average = mean(Workload)) %>%
  arrange(Section, vec.quarters)

pct.increase.q <- quarter.wkld %>% group_by(Section, vec.quarters) %>%
  mutate_each(funs(pct), quarter.average) %>%
  mutate(Percent.increase.q = round((quarter.average-1.0)*100,2))

pct.increase.q <- pct.increase.q %>% filter(Percent.increase.q < 600)

ggplot(data=pct.increase.q, aes(x = Year, y = Percent.increase.q)) +
  ylab("% increase compared to corresponding quarter in the previous year") +
  ggtitle("Comparison by quarter (Q1 - Apr-Jun, Q2 - Jul-Sep, Q3 - Oct-Dec, Q4 - Jan-Mar") +
  geom_point() +
  facet_grid(Section~vec.quarters)# deliverable


View(pct.increase.q)
