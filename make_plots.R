###

library(dplyr)
library(readr)
library(ggplot2)

# load the salaries, spine points and years
scales <- read_csv("national_pay_scale_processed.csv")

# load St Andrews grades
grades <- read_csv("grades.csv")
colnames(grades) <- c("point", "grade")

# CPIH
inflation <- read_csv("series-070322.csv", skip=8, col_names=FALSE)
names(inflation) <- c("year", "inflation")

# merge them
dat <- merge(scales, grades)
dat <- merge(dat, inflation)

# inflation as proportion
dat$inflation <- dat$inflation/100
dat$salary_inflated <- dat$salary * dat$inflation

dat <- dat %>%
  group_by(grade) %>%
  # make index for plotting
  mutate(ind = point - min(point) + year) %>%
  # only print some years
  filter(year %in% seq(2008, 2021, by=3)) %>%
  # only print reasonable years
  filter(ind %in% 2008:2021)


# check
ggplot(dat) +
  geom_line(aes(x=ind, y=salary_inflated,
                group=as.factor(year), colour=as.factor(year))) +
  facet_wrap(~grade)



