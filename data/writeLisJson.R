writeNewDataJson <- function()
  {
    d <- read.table(file="pre-post-LIS.csv",header=T,sep="\t")
    countryLevels <- levels(d[['coun']])
    countryString <- levels(d$coun)[d$coun]
    wave <- d$ser
    year <- d$year

    gini_mi <- d$gini_mi
    gini_dhi <- d$gini_dhi
    pov_mi <- d$pov_mi
    pov_dhi <- d$pov_dhi
    
##     set.seed <- 11
##     rT <- 2*log((100+Tough)/100)/(periods-1)
##     rN <- 2*log((100+Nothing)/100)/(periods-1)
##     rS <- 2*log((100+Smart)/100)/(periods-1)
##     periodData <- 0:(periods-1)
##     rTdata <- 100* exp(rT*periodData[1:(.5*(periods-1)+1)])
##     rNdata <- 100* exp(rN*periodData[1:(.5*(periods-1)+1)])
##     rSdata <- 100* exp(rS*periodData[1:(.5*(periods-1)+1)])
##     rTdata <- c(rTdata,rep(rTdata[.5*(periods-1)+1],.5*(periods-1)))
##     rNdata <- c(rNdata,rep(rNdata[.5*(periods-1)+1],.5*(periods-1)))
##     rSdata <- c(rSdata,rep(rSdata[.5*(periods-1)+1],.5*(periods-1)))

##     chgPeriods <- (periods-1)/2
##     flatPeriods <- (periods-1)/2
##     flatData <- rep(0,flatPeriods)
##     noise <- c(0,rnorm(chgPeriods),flatData)
##     rTdata <- rTdata*(1+(0.05*noise))
##     noise <- c(0,rnorm(chgPeriods),flatData)
##     rNdata <- rNdata*(1+(0.05*noise))
##     noise <- c(0,rnorm(chgPeriods),flatData)
##     rSdata <- rSdata*(1+(0.05*noise))

##     rTdata <- round(rTdata,1)
##     rNdata <- round(rNdata,1)
##     rSdata <- round(rSdata,1)    

    
    
## #    time <- read.table(file="rTime.csv",header=TRUE,sep=",")
##     timeTuple <- getTimeTuple(stDate,periods)
##     posixString<- paste(paste(timeTuple[,1],timeTuple[,2],timeTuple[,3],sep="-"),"00:00:00",sep=" ")
##     time <- 1000*as.numeric(as.POSIXct(posixString))

    countryNames <- c("Austria",
                      "Canada",
                      "Switzerland",
                      "Czech Republic",
                      "Germany",
                      "Denmark",
                      "Finland",
                      "Israel",
                      "Netherlands",
                      "Norway",
                      "Romania",
                      "Sweeden",
                      "Taiwan",
                      "United Kingdom",
                      "United States")
    tsKeys <- paste("Line",countryLevels,sep="_")
                      
                      
#    grParam <- matrix(c(tsKeys,countryNames),nrow=15,ncol=2)
    #grData <- data.frame(time,rTdata,rNdata,rSdata)
    tsKeysF <- factor(as.numeric(d[['coun']]),levels=1:15,labels=tsKeys)
    countryNamesF <- factor(as.numeric(d[['coun']]),levels=1:15,labels=countryNames)
    d$tsKeysF <- tsKeysF
    d$countryNameF <- countryNamesF
    recentYears <- c()
    cat("[",sep=",",file="lis-gini.json",append=FALSE)
    for(i in 1:nrow(d))
      {
       
            datum <- list("line_id"=levels(tsKeysF)[as.numeric(d[['coun']][i])],
                          "line_name"=levels(countryNamesF)[as.numeric(d[['coun']][i])],
                          "wave"=d$ser[i],
                          "year"=d$year[i],
                          "gini_mi"=d$gini_mi[i],
                          "gini_dhi"=d$gini_dhi[i],
                          "pov_mi"=d$pov_mi[i],
                          "pov_dhi"=d$pov_dhi[i])
                          
            datumJason <- toJSON(datum)
            if(i==1){
              j=1
              recentYears <- c(recentYears,1)
            } else {
               if(as.numeric(d$coun[i-1])==as.numeric(d$coun[i])) {
                recentYears[j] <- i
              } else {
                recentYears <- c(recentYears,i)
                j <- j+1
              }
             }
            cat(datumJason,sep=",",file="lis-gini.json",append=TRUE)
            if(!(i==nrow(d)))
              {
                cat(",",sep="",file="lis-gini.json",append=TRUE)
              } else {
                cat("]",sep="",file="lis-gini.json",append=TRUE)
              }
          }
  
    cat("[",sep=",",file="lis_gini_recent.json",append=FALSE)
    for(i in 1:length(countryNamesF))
      {
        recentDatum <- list("line_id"=levels(tsKeysF)[as.numeric(d[['coun']][recentYears[i]])],
                          "line_name"=levels(countryNamesF)[as.numeric(d[['coun']][recentYears[i]])],
                          "wave"=d$ser[recentYears[i]],
                          "year"=d$year[recentYears[i]],
                          "gini_mi"=d$gini_mi[recentYears[i]],
                          "gini_dhi"=d$gini_dhi[recentYears[i]],
                          "pov_mi"=d$pov_mi[recentYears[i]],
                          "pov_dhi"=d$pov_dhi[recentYears[i]])

        recentDatumJason <- toJSON(recentDatum)
        cat(recentDatumJason,sep=",",file="lis_gini_recent.json",append=TRUE)
        if(!(i==length(countryNamesF)))
          {
            cat(",",sep="",file="lis_gini_recent.json",append=TRUE)
          } else {
            cat("]",sep="",file="lis_gini_recent.json",append=TRUE)
          }
      }
  }
