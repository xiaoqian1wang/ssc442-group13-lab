#### LAB 01, EXERCISE 1 ####

#register the data to be read and manipulated further on 
ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                       header = TRUE,
                       sep = ",")

#1
str(ameslist)

#2
typeof(ameslist)

#3
unique(ameslist$GarageType)

# creating outdoor Garage varible
OutdoorGarage <- ameslist$GarageType == 'CarPort' | ameslist$GarageType == 'Detchd'
GarageTemp = model.matrix( ~ ameslist$GarageType - 1, data=ameslist$GarageType )

ameslist_GarageType <- na.omit(ameslist$GarageType) 
ameslist_Garagecom <- cbind(ameslist_GarageType, GarageTemp)

# try it #4
ameslist_Garagecom <- as.data.frame(ameslist_Garagecom, row.names = NULL, optional = FALSE,
                                        make.names = TRUE,
                                        stringsAsFactors = default.stringsAsFactors())
is.atomic(ameslist_Garagecom)


ameslist_GarageType$GarageOutside <- ifelse(
  ameslist_Garagecom$`ameslist$GarageTypeDetchd` == 1 | 
    ameslist_Garagecom$`ameslist$GarageTypeCarPort` == 1, 1, 0)


#Actual Exercise 1

Ames <- data.frame(ameslist$SalePrice, ameslist$YearBuilt,
                   ameslist$YearRemodAdd,ameslist$BsmtFinSF1,
                   ameslist$BsmtUnfSF,ameslist$BsmtFinSF2,
                   ameslist$TotalBsmtSF,ameslist$X1stFlrSF,
                   ameslist$X2ndFlrSF,ameslist$LowQualFinSF,
                   ameslist$GrLivArea,ameslist$FullBath)

# transform in .txt file
write.table(Ames, file = 'Ames.txt')

# create matrix
pairs(Ames)

# find correlation
cor(Ames)
# Honestly it is really hard to interpret this data from this perspective. 
# I'm not too surprised by the variation in correlation. The values and the \n
#   associated variables are dependent on each other and some might be more \n
#   popular than the other, depending on the area (in this case, Ames). 
# The correlation between SalePrice and the variables is sporatic and isn't \n
#   helpful when put into a matrix as this. 

#plot the data!
linez = lm(ameslist$SalePrice ~ ameslist$GrLivArea, data = Ames)
plot(ameslist$SalePrice~ameslist$GrLivArea, data = Ames,
     main = "Correlation Between SalePrice and GrLiveArea Variables",
     xlab = "SalePrice", 
     ylab = "GrLivArea",
     pch = 19, 
     cex = 0.69,
     col = "black")
abline(linez, lwd = 3, col = "grey")

# The largest outlier above the regression line has a SalePrice value of 611657\n
#   and a GrLivArea value of 2364. This house also has 3 full baths, was built \n
#   in 1994 and has a basement with 2330sqft. 


