# I manually copy-pasted the data from the PDFs (boooo) but it's not
# in a uniform format, so I'll fiddle here to get it there


library(dplyr)
library(readr)
library(stringr)


# read in the data
dat <- read_csv("national_pay_scale_pasted.csv", col_names=FALSE)

# give the columns the right names
colnames(dat) <- c("point", "salary", "year")

dat <- dat %>%
  mutate(
  # remove leading/trailing whitespace, make a number
         point = as.numeric(str_trim(point)),
# some rows have salary with pound sign and comma, remove that
      salary = str_replace(salary, ",", "")) %>%
  mutate(salary = str_replace(salary, "£", ""))


# save our great work
write_csv(dat, file="national_pay_scale_processed.csv")
