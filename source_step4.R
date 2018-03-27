#Process clinical data to extract some fields #################################################

#Load library
library("XML")

#Get all files from Data dir
files = list.files(path="./clinical",pattern=".xml",full.names=TRUE)

#Create dataframe to store lines
df = data.frame()

#Process each file
for(i in files){

  #Parse data
  xml = xmlParse(i)

  #Transform xml to list
  data = xmlToList(xml)

  #Create empty vector
  values = character()

  #Get case id
  values = c(values,data[['patient']]$bcr_patient_uuid$text)

  #Get vital status
  if(data[['patient']]$vital_status$text == "Dead"){
    values = c(values,data[['patient']]$days_to_death$text)
  }
  else{
    values = c(values,data[['patient']]$days_to_last_followup$text)
  }

  #Get vital status and convert it to integer code
  if(data[['patient']]$vital_status$text=="Dead"){   
    values = c(values,2) #Insert 2 in case the patient is dead
  }
  else{
    values = c(values,1) #Insert 1 in case the patient is censored
  }

  #Get age
  values = c(values,data[['patient']]$age_at_initial_pathologic_diagnosis$text)

  #Get gender and convert it to integer code
  if (data[['patient']]$gender$text=="MALE"){
    values = c(values, 1) #Insert 1 in case patient is Male
  }
  else{
    values = c(values, 2) #Insert 2 in case patient is Female
  }
		
  #Add line to daframe
  df = rbind(df,t(values))
}

#Rename columns of dataframe
colnames(df) = c("id", "survival_time", "status", "age", "sex")

#Write result to file
write.table(df,"clinical_data.txt",sep="\t",quote=FALSE,row.names=FALSE,col.names=T)
