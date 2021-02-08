train <- read.csv("beer_training_labelled.csv")

entropy <- function(x,y){
  (-x*log2(x) -y*log2(y))
}
# Wind
Ent <- entropy(9/14,5/14)
# Wind = FALSE
WF <- entropy(6/8,2/8)
# Wind = TRUE
WF <- entropy(3/6,3/6)

information_gain <- function(x,y){
  Ent - (y/x)*WF - ((x-y)/x)*WT
}
information_gain(14,6)







