#convert 3 runners table to tipple format 
#not yet complete : has to split age_yr and change age accordingly
# better to add unique check to make sure no duplicate line
#reference : wickham R for data science 2/e
#ch7 input
# Load packages
library(tidyverse)
runner1 <- read_csv("/media/user/1907USB/2511wx/BR4hierarchical/tidyr/cherry-blossom-sample(extract3).csv")

runner1 <- read_csv("cherry-blossom-sample(extract3).csv")

#runner1 <- read_csv("/home/user/Downloads/2.csv")


runner2 <- as_tibble(runner1)
head(runner2)
# round off column 3 to 10 (the net runner finish time to 1 digit after decimal)
#deepseek? R tidyverse code example to round column 3 to 10 of a tibble to 2 digits after decimal pt
runner2_rd <- runner2 |>
  mutate(across(3:10, ~ round(.,1)))
#view result
head(runner2_rd)
# learn fr big-r-book 17.3.2 convert headers to data
##set the column names:
colnames(runner2_rd) <- c("name", "age_yr" , "Year1999","Year2000","Year2001","Year2002","Year2003","Year2004","Year2005","Year2006","Year2007","Year2008")
print(runner2_rd)
#merge to only one column NOT WORK
#runner_merg <- gather(runner2_rd,"Year","Net",2:4)
#print(runner_merg)

#deepseek? use gather() in tidyr to combine multiple columns to one column DO NOT WORK OUT 
# replace old combine with new pivot_longer , much clearer and easier to understand
runner_long<-runner2_rd |>
  pivot_longer(
    cols = c(Year1999:Year2008),
    names_to = "Year",
    values_to = "Net"
  )
head(runner_long)
runner_long$Year <- str_sub(runner_long$Year,5,8) #delete the word Year
print(runner_long,n=30)

#deepseek? in tidyr use group_by(name_in_alphabet) to assign index number to each unique name r code example
#as.numeric return NA! (deepseek? convert string to integer index number in ascending order )
# assign index within each name group
runner_w_index <- runner_long |>
  group_by(name) |>
  mutate(
    #    row_index = row_number(),
    name_index = cur_group_id()
  ) |>
  ungroup()

head(runner_w_index)
print(runner_w_index,n=30)

runner_realage <- runner_w_index |>
  mutate(
    age = parse_double(str_sub(age_yr,1,2)) - parse_double(str_sub(age_yr,4,7)) + parse_double(Year) #
    
  )
print(runner_realage, n=30)
#
# remove rows with NA in Net

runner_realage <- runner_realage |>
  filter(!is.na(Net))

print(runner_realage)


#remove unused columns , and rows with Net is NA
runner_realage <- runner_realage |>
  select(name_index,age,Year,Net)
#head(running)

head(runner_realage)

# 保存为CSV文件
write.csv(runner_realage, "runner_realage.csv", row.names = FALSE)