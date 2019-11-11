#45155291 Ankit Sharma
#Data pre-processing 

#--Task 1.1--#
#Extract Data
raw_data <- read.table("./data/breast-cancer-wisconsin.data",sep=',')

#--Task 1.2--#
#Assign names to columns in the data set
names(raw_data) <- c("Sample.code.number","Clump.Thickness","Uniformity.of.Cell.Size","Uniformity.of.Cell.Shape","Marginal.Adhesion","Single.Epithelial.Cell.Size","Bare.Nuclei","Bland.Chromatin","Normal.Nucleoli","Mitoses","Class")

#--Task 1.3--#
#Check data type of columns
str(raw_data)
#We observe that Bare,Nuclei has the data type "factor".This is because the missing values are read as "?"

#Remove rows with missing values
#To remove rows with missing values we can subset from the dataset the rows where Bare,Nuclei has value "?"
raw_data <- raw_data[!(raw_data$Bare.Nuclei=="?"),]
#Check rows have been deleted
str(raw_data)
#We observe that 683 rows remain

#Change data type of column:Bare.Nuclei to integer
#We observe that directly converting the column Bare.Nuceli to integer rounds some values to 11
#To prevent this it is first converted to character and than to integer
raw_data[, c(7)] <- sapply(raw_data[, c(7)], as.character)
raw_data[, c(7)] <- sapply(raw_data[, c(7)], as.integer)

#Check data type has changed and summary statistics
str(raw_data)
summary(raw_data)
raw_data

#--Task 1,4--#
#Remove first column
raw_data <- subset( raw_data, select = -Sample.code.number)
#Check colimn has been removed
str(raw_data)

#--Task 1,5--#
#Change data type of column:Class
raw_data[, c(10)] <- sapply(raw_data[, c(10)], as.factor)
#Check data type has changed
str(raw_data)

#--Task 1,6--#
#Save dataframe into a file
saveRDS(raw_data, file = "./Data/bcw_processed.Rda")