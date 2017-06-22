
classify <- function(x){
  if (x == "AT" | x == "Protein C" | x == "APTT Lupus Screen Ratio"| x == "Liq AT"){
    return("Thrombotic")
  }else{
    if(x == "HIT SCREEN"| x == "ADAMST13"| x == "CAT_ETP" | x == "Rivaroxaban"){
      return("Miscellaneous")
    }else
      if(x == "FVIII" | x == "FVII"| x == "FXIII Antigen"| x == "FVIII Inhibitor Assay"){
        return("Factors")
      }else{
        return("Platelets")
      }
  }
}


vec.new <- sapply(df.neqas.IQ$Assay, classify)
vec.new

df.neqas.classified <- cbind(df.neqas.IQ, vec.new)

head(df.neqas.classified)

df.neqas.classified %>% arrange(desc(Median_Overall))
as.factor(vec.new)
class(df.neqas.classified$Median_Overall)
