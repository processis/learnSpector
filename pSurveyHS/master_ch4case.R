# try read csv iris from local dir
# try Master Ch4 Case Study techniques , use shiny to display
#10.6 try read csv and summary first
# mastering shiny p52
dir.create("neiss")
# may warn already exists
download <-function(name){
  url <- "https://github.com/hadley/astering-shiny/raw/master/neiss"
  download.file(paste0(url,name),paste0("neiss/",name),quiet = TRUE)
}
download("injuries.tsv.gz")
injuries <- vroom:vroom ("neiss/injuries.tsv.gz")
injuries