get_file <- function(Media,Candi,Month) {
  target_file = read.csv(paste(Media," _ ",Candi," _ ",Month," .csv"),sep = ",")
  return(target_file)
}