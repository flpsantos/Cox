#Find clinical and FPKM files corresponding to the same cases/patients and "randomly" select only one FPKM file per case_id ###############

#Load library
library("rjson")

#Parse json files and put them into a list
json_clinical = fromJSON(file="downloads/clinical_metadata.json")
json_fpkm = fromJSON(file="downloads/fpkm_metadata.json")

#Create function to read each entry of json, extract the tuple "case_id,filename" of each entry, and put the tuple in a new line of a dataframe
processData<-function(json_data){
 processed_data = data.frame(case_id=character(),filename=character())
 for(i in (json_data)){
    case_id = i$cases[[1]]$case_id
    filename = i$file_name
    processed_data = rbind(processed_data,data.frame(case_id,filename))
 }
 return(processed_data)
}

#Call function to process clinical and fpkm data
json_clinical = processData(json_clinical)
json_fpkm = processData(json_fpkm)

#Merge dataframes to find only case_ids in common
concat = merge(json_clinical,json_fpkm,by="case_id")
		
#Remove duplicated case_ids
concat = concat[!duplicated(concat$case_id),]

#Store mappings between case_ids and clinical files into dataframe
clinical_map = concat[,c(1,2)]
colnames(clinical_map)[2] = "filename"

#Store mappings between case_ids and FPKM files into dataframe
fpkm_map = concat[,c(1,3)]
colnames(fpkm_map)[2] = "filename"

#Save mappings between case_ids and filenames
write.table(clinical_map,file = "mapping_clinical_case.txt",row.names = FALSE, col.names = TRUE, sep = "\t",quote = FALSE)
write.table(fpkm_map,file = "mapping_fpkm_case.txt",row.names = FALSE, col.names = TRUE, sep = "\t",quote = FALSE)

#Read metadata files into dataframes
manifest_clinical = read.delim(file="downloads/clinical_manifest.txt")
manifest_fpkm = read.delim(file="downloads/fpkm_manifest.txt")

#Filter metadata files based on previous selected filenames
manifest_clinical = manifest_clinical[manifest_clinical$filename %in% clinical_map$filename,]
manifest_fpkm = manifest_fpkm[manifest_fpkm$filename %in% fpkm_map$filename,]

#Save filtered metadata files
write.table(manifest_clinical,file = "filtered_clinical_manifest.txt",row.names = FALSE, col.names = TRUE, sep = "\t",quote = FALSE)
write.table(manifest_fpkm,file = "filtered_fpkm_manifest.txt",row.names = FALSE, col.names = TRUE, sep = "\t",quote = FALSE)

