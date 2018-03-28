#Process FPKM data #######################################################################
#Genconde has 15787 known lncRNAs

#Read known gencode lncRNAs
rna_list = read.table(file="lncRNAs_list.txt")
colnames(rna_list) = c("id")

#Read mapping file
map = read.table(file="mapping_fpkm_case.txt",header=TRUE)

#Get all files from dir
dir = "./fpkm/"
files = list.files(path=dir,pattern=".FPKM.txt",full.names=FALSE)

#Create new dataframe
df = data.frame(id= character(0))

#Blom transformation #################################################################################################
  #y <- qnorm((r-c)/(N-2c+1)) #where qnorm is the standard normal quantile function 
  #r is the rank of your variable 
  #c = 3/8, the Blom constant 
  #N is the sample size 

#For genes with very low expression, i.e. with many zero counts, a small noise term might be added to handle the ties

#Create function to perform blom transformation on data (see https://doi.org/10.1371/journal.pone.0085150 for detailed information)
blom_transform <- function(var){

  #Create (random) noise vector based on normal distribution with mean 0 and SD 0.0001
  noise = rnorm(length(var), mean = 0, sd = 0.0001)

  #Add noise to values
  var2 = var + noise

  #Apply Blom transformation
  qnorm((rank(var2)-(3/8))/(length(var2)-2*(3/8)+1))

}


#For each file
first = TRUE
count = 1
for(i in files){

  print(paste("Processing file:",count,sep=" "))
  count = count + 1

  #Read file
  fullname = paste(dir,i,sep="")
  data = read.table(file=fullname)

  #Transform data (inverse normal transformation)
  #data$V2 = blom_transform(data$V2)  ## This step is repressed for main brach project

  #Transform expression values into categorial variables (based on median values)
  #median_value = median(data$V2)
  #data$V2 = ifelse(data$V2 > median_value,"1", "0")
  #data$V2 = as.factor(data$V2)  
  	
  #Rename columns of data based on mapping (case_id)
  case_id = as.character(map[grep(i,map$filename),]$case_id)
  colnames(data) = c("id",case_id)

  if(!first){
	df = cbind(df,data[,2])
	colnames(df)[ncol(df)] = case_id
  }else{
	df = data
	first = FALSE
  }

}

#Merge dataframes to filter only lncRNAs expression
df$id = gsub("\\..*","",df$id)
df = merge(df,rna_list,by="id")

#Transpose dataframe
df = t(df)

#Save result to file
write.table(df,file="fpkm_data.txt",sep="\t",quote=F,row.names=T, col.names=F)
