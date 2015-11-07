temp_file <- tempfile()
download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv', temp_file, 'curl')
q1data <- read.csv(temp_file)
unlink(temp_file)
print(paste('Q1 answer:', length(q1data$VAL[!is.na(q1data$VAL) & q1data$VAL == 24])))


temp_file <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx", temp_file, 'curl')
library(xlsx)
dat <- read.xlsx(temp_file, sheetIndex=1, header=TRUE, rowIndex = 18:23, colIndex = 7:15)
unlink(temp_file)
print(paste('Q3 answer:', sum(dat$Zip * dat$Ext, na.rm=T)))

temp_file <- tempfile()
library(XML)
q4data <- xmlTreeParse('http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml', useInternal=TRUE)
root_node <- xmlRoot(q4data)
unlink(temp_file)
print(paste('Q4 answer', sum(xpathSApply(root_node, "//zipcode", xmlValue) == 21231)))

temp_file <- tempfile()
download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv', temp_file, 'curl')
library(data.table)
DT <- fread(temp_file)

mean(DT$pwgtp15,by=DT$SEX)
rowMeans(DT)[DT$SEX==1]; rowMeans(DT)[DT$SEX==2]
tapply(DT$pwgtp15,DT$SEX,mean)
mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15)
sapply(split(DT$pwgtp15,DT$SEX),mean)
DT[,mean(pwgtp15),by=SEX]
