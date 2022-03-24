### inflation-adjusted salaries for UK universities grades

# you can use this for your own institution by running the mk_plot function
# and feeding it your own grade boundaries

library(dplyr)
library(readr)
library(ggplot2)

# load the salaries, spine points and years
scales <- read_csv("national_pay_scale_processed.csv")

# load St Andrews grades
grades <- read_csv("grades.csv")
colnames(grades) <- c("point", "grade")

# CPIH index values from ONS
inflation <- read_csv("series-070322.csv", skip=8, col_names=FALSE)
# don't need all the monthly data tagged onto the end
inflation <- inflation[1:34, ]
names(inflation) <- c("year", "inflation")
inflation$year <- as.numeric(inflation$year)
# inflation from each year to 2021
inflation$inflation <- inflation$inflation[inflation$year==2021]/
                        inflation$inflation

# merge them
dat <- merge(scales, grades)
plot_years <- seq(2008, 2022, by=4)#unique(dat$year)


# make a plot for a given grade
mk_plot <- function(dat, grade, top_point, bottom_point){

  # get the points in this grade
  dat_grade <- dat[dat$point %in% bottom_point:top_point, ]

  # function to create the timeseries per plot year
  dat_plot <- lapply(plot_years, function(plot_year, dat_grade, bottom_point,
                                          top_point){
    ipoint <- bottom_point
    pdat <- c()
    for(iyear in plot_year:2022){
      pdat <- rbind(pdat,
                    dat_grade[dat_grade$year==iyear & dat_grade$point==ipoint, ])
      if(ipoint<top_point) ipoint <- ipoint + 1
    }
    pdat$start_year <- plot_year
    return(pdat)
  }, dat_grade=dat_grade, bottom_point=bottom_point, top_point=top_point)

  # stick all that together
  dat_grade <- do.call(rbind, dat_plot)
  # reorder
  dat_grade <- arrange(dat_grade, start_year, point)
  # bind on the inflation data and do the correction
  dat_grade <- merge(dat_grade, inflation, by="year")
  dat_grade$salary_inflated <- dat_grade$salary * (dat_grade$inflation)
  # get years <= 2022
  dat_grade <- dat_grade[dat_grade$year <= 2022, ]

  # make a plot
  g <- ggplot(dat_grade) +
    geom_line(aes(x=year, y=salary_inflated,
                  group=as.factor(start_year), colour=as.factor(start_year))) +
    geom_point(aes(x=year, y=salary_inflated,
                  group=as.factor(start_year), colour=as.factor(start_year))) +
    geom_text(aes(x=year, y=salary_inflated+200, label=point)) +
    scale_x_continuous(breaks=2008:2022) +
    labs(x="Year", y="Salary (in 2021 pounds, CPIH-adjusted)",
         colour="Starting\nyear") +
    ggtitle(paste0("University of St Andrews, grade ", grade)) +
    theme_minimal()

  return(g)
}


# make plots for grades 5 through 7
mk_plot(dat, 5, 29, 23)
mk_plot(dat, 6, 36, 30)
mk_plot(dat, 7, 44, 37)


# make the plots, but as PNG files
ggsave(file="grade5.png", mk_plot(dat, 5, 29, 23), bg="white")
ggsave(file="grade6.png", mk_plot(dat, 6, 36, 30), bg="white")
ggsave(file="grade7.png", mk_plot(dat, 7, 44, 37), bg="white")

