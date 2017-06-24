# add a quarter column to the data - redo classify function

quarters <- function(x){
  if (x == "05" | x == "06" | x == "07"){
    return("Q1")
  }else{
    if(x == "08"| x == "09"| x == "10"){
      return("Q2")
    }else
      if(x == "11" | x == "12"| x == "01"){
        return("Q3")
      }else{
        return("Q4")
      }
  }
}

vec.quarters <- sapply(s.sections$Month, quarters)
vec.quarters

df.quarters <- cbind(s.sections, vec.quarters)

head(df.quarters)
as.factor(vec.quarters)

