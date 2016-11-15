#time Rscript 089.R 
#[1] 743
#
#real	0m0.340s
#user	0m0.298s
#sys	0m0.039s

fix_roman <- function(x) {
  # well, super lame hack to make sure we do not get problems with numerals above 4k
  ifelse(substr(x,1,3)=="MMM",
         y<-paste("MM",as.character(as.roman(as.numeric(as.roman(substr(x,3,nchar(x)))))),sep=""),
         y<-as.character(as.roman(as.numeric(as.roman(x))))
  )
  return(y)
}
list_in <- read.table("p089_roman.txt")
sum(apply(list_in,1,nchar)-nchar(apply(list_in,1,fix_roman)))
