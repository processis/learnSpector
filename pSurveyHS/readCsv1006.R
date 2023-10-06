#modify mastering shiny ch4 p51 code 
#since cannot get data from the directory in gitHub
# 2023.10.6
#
# following dir create useless , hence replaced by fixed path  
# dir.create("neiss")
#warning 'neiss' already exists
irisdata <- read.csv('/media/user/1907USB/2020data/landerData/IRIS_fisher.csv',header = TRUE)
summary(irisdata)
write.csv(irisdata,file = "/media/user/1907USB/2020data/landerData//irisdata.csv")
idata <-vroom::vroom("/media/user/1907USB/2020data/landerData/irisdata.csv")
idata