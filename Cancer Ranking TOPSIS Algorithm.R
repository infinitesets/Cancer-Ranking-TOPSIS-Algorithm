library(tidyverse)
library(reshape2)

#data
#user should update directory path
BYAREA = read.table('C:/Users/ykadeoua/Desktop/data/BYAREA.TXT',sep = "|", header = T)

#Collecting list of cancer 
my_list = c("Female Breast","Colon and Rectum","Kidney and Renal Pelvis","Leukemias","Liver and Intrahepatic Bile Duct","Lung and Bronchus","Melanomas of the Skin","Ovary","Prostate","Oral Cavity and Pharynx")

#selecting records with given list of cancer
cancer_data = BYAREA[which(with(BYAREA, EVENT_TYPE == "Mortality" & SITE %in% my_list
                                & RACE == "All Races" & AREA != "Atlanta" & AREA != "Detroit" & AREA != "District of Columbia" 
                                & AREA != "East North Central" & AREA != "East South Central" & AREA != "Los Angeles" 
                                & AREA != "Middle Atlantic" & AREA != "Midwest" & AREA != "Mountain" & AREA != "New England" 
                                & AREA != "Northeast" & AREA != "Pacific" & AREA != "San Francisco-Oakland" & AREA != "San Jose-Monterey" 
                                & AREA != "Seattle-Puget Sound" & AREA != "South" & AREA != "South Atlantic" & AREA != "West" 
                                & AREA != "West North Central" & AREA != "West South Central")),]

#Checking for missing records and removing missing values
cancer_data$AGE_ADJUSTED_RATE[cancer_data$AGE_ADJUSTED_RATE == "~"] = NA
table(is.na(cancer_data))
cancer_data = na.omit(cancer_data)
table(is.na(cancer_data))


#collecting data area, cancer and  Age-Adjusted Rates means
descision_matrix = data.frame("state" = cancer_data$AREA,"site" = cancer_data$SITE,"AGE_ADJUSTED_RATE" = as.numeric(as.character(cancer_data$AGE_ADJUSTED_RATE)))
descision_matrix = acast(descision_matrix,descision_matrix$state ~ descision_matrix$site,mean)

#(1) Calculate the normalized decision matrix.
SquareRoot_of_measurement = function(matrix){
  x = 0 
  for(row in 1:nrow(matrix)) {
    for(col in 1:ncol(matrix)) {
      y = matrix[row,col]^2
      x = x + y
    }
  }
  return(sqrt(x))
}


for (row in 1:nrow(descision_matrix)) {
  for(col in 1:ncol(descision_matrix)){
    descision_matrix[row,col] = descision_matrix[row,col]/SquareRoot_of_measurement(descision_matrix)
  }
}

#(2) Calculate the weighted normalized decision matrix.
weights = 0
for (col in 1:nrow(descision_matrix)){
  weights = weights + descision_matrix[row,]
}
weights = (weights / sum(weights))
vij = sweep(descision_matrix,MARGIN = 2,weights,"*")

#(3) Determine ideal best and ideal worst
ideal_best = apply(vij,2,min)
ideal_worst = apply(vij,2,max)

#(4) Calculate the separation measures, using the n-dimensional Euclidean distance
best_distance = sweep(vij,MARGIN = 2,ideal_best,"-")
best_distance = best_distance^2
best_distance = apply(best_distance,1,sum)
best_distance = sqrt(best_distance)

worst_distance = sweep(vij,MARGIN = 2,ideal_worst,"-")
worst_distance = worst_distance^2
worst_distance = apply(worst_distance,1,sum)
worst_distance = sqrt(worst_distance)

test = sqrt(worst_distance)

#(5) Calculate the relative closeness to the ideal solution
ranking = c()
for(i in 1:length(worst_distance)){
  ranking[i] = worst_distance[i]/(best_distance[i] + worst_distance[i])
}

for(i in 1:length(worst_distance)){
  name = names(worst_distance[i])
  ranking[i] = worst_distance[i]/(best_distance[i] + worst_distance[i])
  names(ranking)[i] = name
}

#ranking the states 
sort(rank(ranking))








