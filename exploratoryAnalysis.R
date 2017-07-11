## Load R libraries
## ================
library(easypackages) # For easy mgmt of R packages
# Group packages
matpackages <- c("R.matlab","caret")
tabpackages <- c("data.table","dplyr","stringr","magrittr","tidyr")
plotpackages <- c("ggplot2")
libraries(matpackages); libraries(tabpackages); libraries(plotpackages)
rm(matpackages,tabpackages,plotpackages)
# Set working directory
setwd("C:/Users/lopezfe/Documents/Eigenvector data")

## Read Eigenvector etching data
## =============================
# Read machine data sets as large arrays
machineArray <- readMat("MACHINE_Data.mat")[[1]]
# Read description (OPTIONAL)
# machineDescription <- machineArray[[1]]$readme
machineData <- data.table()
# Normal wafers
for(i in 1:108){
  tempData <- as.data.table(machineArray[[2]][[i]][[1]])
  setnames(tempData, trimws(machineArray[[7]]))
  tempWafer <- data.table(lot=rep(substr(machineArray[[3]][i],2,3),nrow(tempData)),
                          wafer=rep(substr(machineArray[[3]][i],4,5),nrow(tempData)))
  tempFault <- data.table(fault = rep(FALSE,nrow(tempData)),
                          faultName = rep(NA,nrow(tempData)))
  waferData <- cbind(tempData,tempWafer,tempFault)
  machineData <- rbind(machineData,waferData)
}
# Faulty wafers
for(i in 1:21){
  tempData <- as.data.table(machineArray[[4]][[i]][[1]])
  setnames(tempData, trimws(machineArray[[7]]))
  tempWafer <- data.table(lot=rep(substr(machineArray[[5]][i],2,3),nrow(tempData)),
                          wafer=rep(substr(machineArray[[5]][i],4,5),nrow(tempData)))
  tempFault <- data.table(fault = rep(TRUE,nrow(tempData)),
                          faultName = rep(machineArray[[6]][i],nrow(tempData)) )
  waferData <- cbind(tempData,tempWafer,tempFault)
  machineData <- rbind(machineData,waferData)
}
rm(waferData,tempData,tempWafer,tempFault,i,machineArray)


# Read optical emission spectroscopy data sets as large arrays
OESArray <- readMat("OES_Data.mat")[[1]]
# Read description (OPTIONAL)
# OESDescription <- OESArray[[1]]$readme
OESData <- data.table()
# Normal wafers
for(i in 1:106){
  tempData <- as.data.table(OESArray[[2]][[i]][[1]])
  # Manually add the three locations
  freq <- as.character( OESArray[[7]] )[1:ncol(OESArray[[7]])]
  locations <- c( rep("Loc1",43), rep("Loc2",43), rep("Loc3",43) )
  paste(freq,locations)
  namesVar <- paste(freq,locations,sep="_")
  setnames(tempData, namesVar)
  tempWafer <- data.table(lot=rep(substr(OESArray[[3]][i],2,3),nrow(tempData)),
                          wafer=rep(substr(OESArray[[3]][i],4,5),nrow(tempData)))
  tempFault <- data.table(fault = rep(FALSE,nrow(tempData)),
                          faultName = rep(NA,nrow(tempData)))
  waferData <- cbind(tempData,tempWafer,tempFault)
  OESData <- rbind(OESData,waferData)
}
# Faulty wafers
for(i in 1:20){
  tempData <- as.data.table(OESArray[[4]][[i]][[1]])
  # Manually add the three locations
  freq <- as.character( OESArray[[7]] )[1:ncol(OESArray[[7]])]
  locations <- c( rep("Loc1",43), rep("Loc2",43), rep("Loc3",43) )
  paste(freq,locations)
  namesVar <- paste(freq,locations,sep="_")
  setnames(tempData, namesVar)
  tempWafer <- data.table(lot=rep(substr(OESArray[[5]][i],2,3),nrow(tempData)),
                          wafer=rep(substr(OESArray[[5]][i],4,5),nrow(tempData)))
  tempFault <- data.table(fault = rep(TRUE,nrow(tempData)),
                          faultName = rep(OESArray[[6]][i],nrow(tempData)) )
  waferData <- cbind(tempData,tempWafer,tempFault)
  OESData <- rbind(OESData,waferData)
}
rm(waferData,tempData,tempWafer,tempFault,i,OESArray,namesVar,freq,locations)


# Read radio frequency monitors data sets as large arrays
RFMArray <- readMat("RFM_Data.mat")[[1]]
# Read description (OPTIONAL)
# RFMDescription <- RFMArray[[1]]$readme
RFMData <- data.table()
# Normal wafers
for(i in 1:106){
  tempData <- as.data.table(RFMArray[[2]][[i]][[1]])
  setnames(tempData, as.character( trimws(RFMArray[[7]]) ))
  tempWafer <- data.table(lot=rep(substr(RFMArray[[3]][i],2,3),nrow(tempData)),
                          wafer=rep(substr(RFMArray[[3]][i],4,5),nrow(tempData)))
  tempFault <- data.table(fault = rep(FALSE,nrow(tempData)),
                          faultName = rep(NA,nrow(tempData)))
  waferData <- cbind(tempData,tempWafer,tempFault)
  RFMData <- rbind(RFMData,waferData)
}
# Faulty wafers
for(i in 1:20){
  tempData <- as.data.table(RFMArray[[4]][[i]][[1]])
  setnames(tempData, as.character( trimws(RFMArray[[7]]) ))
  tempWafer <- data.table(lot=rep(substr(RFMArray[[5]][i],2,3),nrow(tempData)),
                          wafer=rep(substr(RFMArray[[5]][i],4,5),nrow(tempData)))
  tempFault <- data.table(fault = rep(TRUE,nrow(tempData)),
                          faultName = rep(RFMArray[[6]][i],nrow(tempData)) )
  waferData <- cbind(tempData,tempWafer,tempFault)
  RFMData <- rbind(RFMData,waferData)
}
rm(waferData,tempData,tempWafer,tempFault,i,RFMArray)

# Define factors in machine data
cols <- c("Step Number","lot","wafer","fault","faultName")
machineData %<>% mutate_at(cols, funs(factor(.)))
# Determine factors in OES data
cols <- cols[-1]
OESData %<>% mutate_at(cols, funs(factor(.)))
# Determine factors in RFM data
RFMData %<>% mutate_at(cols, funs(factor(.)))
rm(cols)

## Consolidate different data sources
## ==================================
## Convert to data frame tbl to use dplyr
machineData <- tbl_df(machineData); OESData <- tbl_df(OESData)
RFMData <- tbl_df(RFMData); aggregatedData <- data.table()
aggregatedData %<>% tbl_df()
# Split by lot and wafer - Unify by time
mergedWafer <- data.table( Time = seq(from=10, to=120, by=1) ) # From 10 to 110 s
mergedWafer %<>% tbl_df()
for ( numLot in levels(machineData$lot) ){
  for ( numWafer in levels(machineData$wafer) ){
    # Machine data for wafer
    machineDataWafer <- filter(machineData,lot == numLot,wafer == numWafer ) 
    # RFM data for wafer
    RFMDataWafer <- filter(RFMData,lot == numLot,wafer == numWafer) 
    # OES data for wafer
    OESDataWafer <- filter(OESData,lot == numLot,wafer == numWafer)
    # Approximate machine data
    mergedWafer$stepNo <- rep( machineDataWafer$`Step Number`[1] , nrow(mergedWafer) )
    mergedWafer$lot <- rep( machineDataWafer$lot[1], nrow(mergedWafer) )
    mergedWafer$wafer <- rep( machineDataWafer$wafer[1], nrow(mergedWafer) )
    mergedWafer$fault <- rep( machineDataWafer$fault[1], nrow(mergedWafer) )
    mergedWafer$faultName <- rep( machineDataWafer$faultName[1], nrow(mergedWafer) )
    if (nrow(machineDataWafer) != 0){
      for ( i in names(select_if(machineDataWafer[-1],is.numeric)) ){
        mergedWafer[,i] <- approx( as.numeric(unlist(machineDataWafer$Time)),
                                        as.numeric(unlist(machineDataWafer[,i])),
                                        mergedWafer$Time)$y
        # Mean-center the data to remove bias
        mergedWafer[,i] <- mergedWafer[,i]-mean(unlist(mergedWafer[,i]), na.rm=TRUE)
      }
    }
    # Approximate RFM data
    if (nrow(RFMDataWafer) != 0){
      for ( i in names( select(RFMDataWafer, S1V1:S34I5)) ){
        mergedWafer[,i] <- approx( as.numeric(unlist(RFMDataWafer$TIME)),
                                  as.numeric(unlist(RFMDataWafer[,i])),
                                  mergedWafer$Time )$y
        # Mean-center the data to remove bias
        mergedWafer[,i] <- mergedWafer[,i]-mean(unlist(mergedWafer[,i]), na.rm=TRUE)
      }
    }
    # Add statistics from OES
    if (nrow(OESDataWafer) != 0){
      # Measure only spread to prevent capturing drift in the bias
      OESDataWafer %<>% summarize_if(is.numeric, c("sd","mad"), na.rm = TRUE)
      mergedWafer %<>% merge(OESDataWafer,all=TRUE)
    }
    gc()
    # Append to aggregatedData
    aggregatedData %<>% bind_rows(mergedWafer)
  }
}
rm(mergedWafer,machineDataWafer,OESDataWafer,RFMDataWafer,numLot,numWafer,i)
gc(reset=TRUE) # Garbage collection
aggregatedData %<>% drop_na(-faultName) %>%
  filter(Time %in% seq(from=16, to=90, by=1) )

## Construct timed and untimed arrays
## ===================================
# Initialize big array
batches <- unique(aggregatedData[c("wafer","lot")])
faults <- factor(levels = levels(aggregatedData$fault))
faultNames <- factor(levels = levels(aggregatedData$faultName))
for(i in 1:nrow(batches)){
  faults[i] <- filter(aggregatedData, wafer == batches$wafer[i], lot ==batches$lot[i])$fault[1]
  faultNames[i] <- filter(aggregatedData, wafer == batches$wafer[i], lot ==batches$lot[i])$faultName[1]
}
batches$fault <- faults; batches$faultName <- faultNames
ntimes <- 90+1-16
timedNames <- union( names(select_if(machineData[-1],is.numeric)),
                     names(select_if(RFMData[-1],is.numeric)) )
timedVar <- length(timedNames)
untimedNames <- names(select(aggregatedData, contains("mad"), contains("sd")))
untimedVar <- length(untimedNames)
# Construct 2-D array
Xtimed <- matrix( nrow = nrow(batches), ncol= ntimes*timedVar )
Xuntimed <- matrix( nrow = nrow(batches), ncol= untimedVar ) 
for ( i in 1:nrow(batches) ){
  batchData <- filter(aggregatedData,lot==batches$lot[i],wafer==batches$wafer[i] )
  timedbatchData <- select(batchData,timedNames)
  Xtimed[i,] <- as.vector(t(as.matrix(timedbatchData)))
  untimedbatchData <- select(batchData,untimedNames)
  Xuntimed[i,] <- as.vector(t(as.matrix(untimedbatchData[1,])))
}
Xmatrix <- cbind(Xtimed,Xuntimed)
rm(Xtimed,Xuntimed,i,ntimes,timedNames,untimedNames,timedVar,untimedVar)
rm(timedbatchData,untimedbatchData,batchData)
# Remove zero-variance columns
Xmatrix <- Xmatrix[,apply(Xmatrix, 2, var, na.rm=TRUE) != 0]
nvars <- ncol(Xmatrix)
Xt <- tbl_df(Xmatrix)
#Xt <- tbl_df( scale(Xmatrix) ) # Scale every column, now performed with the PCA
Xt$lot <- batches$lot; Xt$wafer <- batches$wafer
Xt$fault <- batches$fault; Xt$faultName <- batches$faultName
rm(Xmatrix,faults,faultNames,batches)
# Xmatrix is free of NA, and is not labeled
#reportNA <- summary( sapply(Xt[,1:nvars], function(x) sum(is.na(x))) )

## Principal Component Analysis (PCA)
## ==================================
# Sample training data
Xnormal <- filter(Xt, fault == FALSE)
Xfault <- filter(Xt, fault == TRUE)
inTrain <- createDataPartition( seq(from=1,to=nrow(Xnormal),by=1) ,p=0.8,list=FALSE)
training <- Xnormal[inTrain,]
testing <- rbind( Xnormal[-inTrain,], Xfault )
normalTesting <- nrow( Xnormal[-inTrain,] )

# Compute principal components
PCA <- prcomp(training[,1:nvars], retx=TRUE, center = TRUE, scale = TRUE)
expl.var <- round(PCA$sdev^2/sum(PCA$sdev^2)*100) # percent explained variance
summary(PCA)
# Plot of variance of each PCA
screeplot(PCA, type="lines",col=3)
# Prediction of PCs for validation set
pred <- predict(PCA, newdata=testing[,1:nvars] )
rm(nvars)

# Plot results
COLOR <- c(2:4); PCH <- c(1,16); pc <- c(1,2) # First two PCs
# Merge all normal data
normalPC <- rbind( PCA$x[,pc], pred[1:normalTesting,pc] )
levelsLots <- levels(Xnormal$lot)
lotNormal <- c(Xnormal$lot[inTrain], Xnormal$lot[-inTrain])
lotNormal <- factor(lotNormal, labels = levelsLots)
faultyPC <- pred[(normalTesting+1):nrow(testing),pc]
lotFaulty <- Xfault$lot

plot(normalPC, col=COLOR[lotNormal], cex=PCH[1], 
     xlab=paste0("PC ", pc[1], " (", expl.var[pc[1]], "%)"), 
     ylab=paste0("PC ", pc[2], " (", expl.var[pc[2]], "%)")
)
points(faultyPC, col=COLOR[lotFaulty], pch=PCH[2])
legend("topright", legend=levels(training$lot), fill = COLOR, border=COLOR)
legend("topleft", legend=c("normal", "faulty"), col=1, pch=PCH)
rm(COLOR,PCH,pc,expl.var); rm(inTrain,pred,testing,training,Xnormal,Xfault,levelsLots)

## Unsupervised anomaly detection
## ==============================
