########################################################################################################
# Cleaning up Tucson Bird Count (TBC) data 
# Merging data entered into Access database and data submitted via eBird
########################################################################################################

#- Working directory, packages ---------------------------------------------#

# setwd()

library(plyr)
library(reshape2)
library(sf)

# rm(list=ls())

#- Import data -------------------------------------------------------------#

  #TBC data exported from Access DB 
    dfa <- read.csv('OriginalData/TucsonBirdCount-Access.csv',header=TRUE,
                    col.names=c('siteID','alpha','common.name','date','count','count.opc','easting','northing',
                                'firstname','lastname','visitID'))
  
  #TBC data entered into eBird
    dfe <- read.csv('OriginalData/TucsonBirdCount-eBird.csv',header=TRUE,
                    col.names=c('submission','common.name','taxo.order','count','state','county','locationID',
                                'location','lat','long','date','time','protocol','duration','all.obs.reported','distance',
                                'area','n.observers','breeding','details','comments','MLCatalogNo'))
    
    
    #In both datasets, each row contains count data for one bird species observed by a primary observer 
    #at one site (within a particular route) at a particular date/time. 
    
    #In the future, I'll extract the data from Access and eBird in a different way (and include all fields)
    #For now, we'll go with the datasets created by JM in November 2021

#- Format data entered into Access -------------------------------------------------------------#

  #Create separate routeID
    dfa$routeID <- colsplit(dfa$siteID,'_',c('routeID','Site'))[,1]
  
  #Date -- need to clean up (some formatted with /, some with -)
    #Creating new field, formatted as a date (obsdate)
    dfa$obsdate <- as.Date('1900-01-01')
    dfa$obsdate[grepl('/',dfa$date)] <- as.Date(dfa$date[grepl('/',dfa$date)],format='%m/%d/%Y')
    dfa$obsdate[grepl('-',dfa$date)] <- as.Date(dfa$date[grepl('-',dfa$date)],format='%d-%b-%y')
    summary(dfa$obsdate) #years included: 2001-2021
    dfa$yr <- as.numeric(format(dfa$obsdate,'%Y'))
    dfa$mon <- as.numeric(format(dfa$obsdate,'%m'))
    dfa$doy <- as.numeric(format(dfa$obsdate,'%j'))
    
  #Counts (count = official count on survey; count.opc = supplemental count [off site, outside of 5-min, or not by primary observer])
    #For this project, we can probably focus on official counts and ignore supplemental counts
      summary(dfa$count)
      # dfa[dfa$count>300,]  #Most high counts seem legit (counts of NSHO: 340-1900; YEBL: 625; MALL: 400; ROPI: 350)
      
      #Removing counts that are clearly incorrect
      dfa <- dfa[dfa$count<2000,]
      #Remove counts = 0
      dfa <- dfa[dfa$count!=0,]
      
  #Observer
    #Merge first and last names
    dfa$observer <- paste(dfa$lastname,dfa$firstname,sep='_')
  
  #Get rid of unnecessary columns and sort:
    dfa <- dfa[,c('routeID','siteID','easting','northing','visitID','obsdate','yr','mon','doy','common.name','count','observer')]
    dfa <- dfa[with(dfa,order(obsdate,siteID,observer,common.name)),]
  
  #VisitIDs (visit = independent 5-min stationary count of all birds observed at that site by the primary observer)
    #Sites were sometimes surveyed multiple times on the same day (by the same or different observers) = legitimate, independent visits
    #However, there are instances where the data seems to be entered into Access more than once = duplicate records
    
    #Investigating this a bit:
      #Dataframe with each row = unique combination of siteID, obsdate, and observer
      surveys.a <- ddply(dfa,.(siteID,obsdate,observer),summarize,n.visits=length(unique(visitID)))
      #Dataframe with instances where siteID-obsdate-observer combo associated with >1 visitID
      multvis <- surveys.a[surveys.a$n.visits>1,] 
      #Identify when >1 visit by the same observer at same location on same day had an identical number of birds counted
      #indicating this is probably a duplicate record
      multvis$combo <- paste(multvis$siteID,multvis$obsdate,multvis$observer,sep='.')
      dfa$combo <- paste(dfa$siteID,dfa$obsdate,dfa$observer,sep='.')
      visits <- ddply(dfa[dfa$combo %in% multvis$combo,],.(visitID,combo),summarize,
                        n.species=length(unique(common.name)),total.abund=sum(count))
      visitdups <- ddply(visits,.(combo),summarize,n.vis=length(total.abund),n.totals=length(unique(total.abund)))
      #List of potential duplicate visits:
      visitdups2 <- visitdups[visitdups$n.vis!=visitdups$n.totals,]
        # ex1 <- dfa[dfa$combo==visitdups2$combo[1],]
        # ex1[with(ex1,order(visitID,common.name)),]  

    #To avoid potential duplicates, we'll only keep one set of count data (one visit) for each siteID-obsdate-observer combination
    #We'll keep data from the visit with the highest species total (and if same number of species, the highest total abundance)
    
      #Create an indicator for visits that we'll want to exclude
      visits <- visits[with(visits,order(combo,-n.species,-total.abund)),]
      visits$exclude <- c(0,rep(NA,nrow(visits)-1))
      for(i in 2:nrow(visits)){
        visits$exclude[i] <- ifelse(visits$combo[i]==visits$combo[i-1],1,0)
      }
  
      #Attach the exclusion indicator to the count dataframe (dfa), and remove those observations
      dfa$exclude <- visits$exclude[match(dfa$visitID,visits$visitID)]
      dfa2 <- dfa[-which(dfa$exclude==1),]
      dfa2$exclude <- NULL
      dfa2$combo <- NULL

#- Format data entered into eBird -------------------------------------------------------------#
  
  #Remove columns that have no information (entries all the same or all NA)
    dfe$state <- NULL
    dfe$county <- NULL
    dfe$area <- NULL
    dfe$MLCatalogNo <- NULL
  
  #Formatting date/time
    #Creating new field, formatted as a date (obsdate)
    dfe$obsdate <- as.Date(dfe$date,format='%m/%d/%Y')
    summary(dfe$obsdate) #years included: 2015-2021
    dfe$yr <- as.numeric(format(dfe$obsdate,'%Y'))
    dfe$mon <- as.numeric(format(dfe$obsdate,'%m'))
    dfe$doy <- as.numeric(format(dfe$obsdate,'%j'))
    dfe$hr <- colsplit(dfe$time,pattern=':',c('hr','min'))[,1]
    dfe$hr <- ifelse(grepl('PM',dfe$time),dfe$hr+12,dfe$hr)
    minp <- colsplit(dfe$time,pattern=':',c('hr','min'))[,2]  
    dfe$min <- colsplit(minp,pattern=' ',c('min','p'))[,1]  
    dfe$date <- NULL
    dfe$time <- NULL
  
  #submission column appears to be the sampling event identifier from eBird.  
  #Important note: if two people did a count together and shared a checklist, they would have the different submission numbers
  #So, submission ID can't be used to identify independent "visits".
  #This is a little tricky, since there's no observer ID in the eBird dataset provided in November
  
  #Identify submissions where TBC protocols weren't followed: 
    #Dataframe with unique submission IDs 
    submissions <- ddply(dfe,.(submission,locationID,location,protocol,duration,n.observers,comments,obsdate,yr,mon,doy,hr,min),
                         summarize,n.species=length(unique(common.name)),total.abund=sum(count))
    
    #Most sites surveyed using a 5-min stationary count, EXCEPT for a few transect surveys in parks
    #Identify those sites that use transects (have "T" in site name), and keep submissions that used Traveling Count
    submissions$transectsOK <- ifelse(grepl("_T",submissions$location) & submissions$protocol=='eBird - Traveling Count',1,0)
    
    #Identify submissions that are 5-min stationary counts
    submissions$pointsOK <- ifelse(submissions$protocol=='eBird - Stationary Count' & submissions$duration==5,1,0)  
    
    #Identifying counts that occurred between 5AM and noon (excluding those done too early or too late)
    submissions$timeOK <- ifelse(submissions$hr %in% 5:11,1,0)
    
    #Note: there are occasionally up to 8 observers listed. Assuming only one person was the primary observer and retaining all these submissions
  
    #Create indicator for those submissions that violated protocols so we can exclude counts from dfe 
    submissions$exclude <- ifelse(rowSums(submissions[,grepl('OK',names(submissions))])==2,0,1)
    # #check:
    # count(submissions[,grepl('OK|exclude',names(submissions))])

  #Remove all counts when visit wasn't a stationary, 5-min count (or transect where appropriate) between 5 and noon:
    dfe$exclude <- submissions$exclude[match(dfe$submission,submissions$submission)]
    dfe2 <- dfe[dfe$exclude==0,]
    dfe2$exclude <- NULL
    
  #Extract a list of submission #s (sampling event identifiers) in this dataset 
    #that meet the survey criteria
    seis <- sort(unique(dfe2$submission))
    # write.table(seis, "clipboard", row.names = FALSE)
  
  #Dataframe with each row = unique combination of locationID, obsdate, and time (by using time, should be unique observers)
    surveys.e <- ddply(dfe2,.(locationID,location,obsdate,hr,min),summarize,n.submissions=length(unique(submission)))
    #Dataframe with instances where locationID-obsdate-time combo associated with >1 submissionID
    multsubm <- surveys.e[surveys.e$n.submissions>1,] 
    #Identify when >1 submission at the same location, same day, and same time had an identical number of birds counted
    #indicating this is probably a duplicate record
    multsubm$combo <- paste(multsubm$locationID,multsubm$obsdate,multsubm$hr,multsubm$min,sep='.')
    dfe2$combo <- paste(dfe2$locationID,dfe2$obsdate,dfe2$hr,dfe2$min,sep='.')
    submissions2 <- ddply(dfe2[dfe2$combo %in% multsubm$combo,],.(submission,combo),summarize,
                          n.species=length(unique(common.name)),total.abund=sum(count))  
    #Create an indicator for submissions we'll want to exclude
    submissions2 <- submissions2[with(submissions2,order(combo,-n.species,-total.abund)),]
    submissions2$exclude <- c(0,rep(NA,nrow(submissions2)-1))
    for(i in 2:nrow(submissions2)){
      submissions2$exclude[i] <- ifelse(submissions2$combo[i]==submissions2$combo[i-1],1,0)
    }
    
    #Attach the exclusion indicator to the count dataframe (dfe2), and remove those observations
    dfe2$exclude <- submissions2$exclude[match(dfe2$submission,submissions2$submission)]
    dfe3 <- dfe2[-which(dfe2$exclude==1),]
    dfe3$exclude <- NULL
    dfe3$combo <- NULL
    
    #Are locations in this eBird dataset surveyed more than once in a day?
    locdate.e <- ddply(dfe3,.(locationID,obsdate),summarize,n.submissions=length(unique(submission)))
    locdate.e[locdate.e$n.submissions>1,]  #One time
    # submissions[submissions$locationID==locdate.e$locationID[locdate.e$n.submissions>1] & 
    #                submissions$obsdate==locdate.e$obsdate[locdate.e$n.submissions>1],]
    #Exclude the submission with fewer species in case it's the same observer (treating it the same as the Access data)
    dfe3 <- dfe3[dfe3$submission!='S68982199',]
  
  #Check if there are notes in the comments or details fields that observations are supplemental:
    # dfe3[grepl('supp|Supp',dfe3$details),] #Nothing in details field
    # dfe3[grepl('supp|Supp',dfe3$comments),] 
      #Looks like submissions S56532940 and S88838859 should be supplemental, so we'll remove from dfe3
    dfe3 <- dfe3[!dfe3$submission %in% c('S56532940','S88838859'),]
    
  #Counts (official only)
    summary(dfe3$count) #0-150
    #Removing counts = 0
    dfe3 <- dfe3[dfe3$count!=0,]
  
  #Create site and route IDs from the location column (to match that in Access DB)
    locnames <- unique(dfe3[,c('locationID','location','lat','long')])
    
    #One location name associated with two locationIDs:
      # locnames[duplicated(locnames$location),]
      # locnames[locnames$location=='TBC 512_01',] #L8599294, L8359576
      # sites.a[sites.a$routeID==512,]  #there are 05 points and 7 transects associated with route 512 in Access DB
      # sort(unique(dfe2$location[grep('TBC 512',dfe2$location)])) #only the points are in eBird data
      # unique(dfe2[dfe2$location=='TBC 512_01',c('location','locationID','obsdate','lat','long')])
      # #Looks like maybe this point got moved between 2019 and 2020 -- rename new location 512_06?
    
    #Most locations formatted as: TBC XXX_XX (nchar=10) but we do have:
      #TBC XXX Supp. 
      #TBC 501_01 (new)
      #TBC 131 12 521 E Delano St.... (there is a TBC 131_12)
      #153_13, 153_14, 153_15 (TBC 153_13, 14, 15 don't exist)
      #162-08 Alt (there is a TBC 162_08)
      #Random location names... (eg, Lincoln Regional Park, Tucson....)
  
    #Exploring locations that aren't formatted as TBC XXX_XX
      # head(dfe3[dfe3$location=='153_13',])
      # dfe3[grepl('153',dfe3$location) & dfe3$obsdate=='2021-05-13',] #sites were surveyed once (2021)
      # count(dfa2[dfa2$routeID==153,c('routeID','siteID')]) #sites 13-15 not in Access data
      # 
      # dfe3[grepl('162-08|162_08',dfe3$location),c('location','obsdate','comments')]
      # #Original location used in 2019 and 2020 (32.26742 -110.7263), Alt location also used in 2020 (32.26926 -110.7295)
      # #However, comments (in both years) suggest point has been done in multiple locations.
      # 
      # dfe3[grepl('131_12|131 12',dfe3$location),c('location','lat','long','obsdate','common.name','count','details','comments')]
      # #Original location name surveyed in 2019, new name surveyed in 2020 and 2021; lat/longs slightly different
      # count(dfe3$location[grepl('131',dfe3$location)]) #01-12 + (new 12)
      # count(dfa2$siteID[dfa2$routeID==131]) #Access database has 12 points for route 131: 01-12
      # 
      # dfe3[grepl('501_01',dfe3$location),c('location','lat','long','obsdate','common.name','count','details','comments')]
      # count(dfe3$location[grepl('501',dfe3$location)]) #03-06, 08-09, 12-18, + (new 01)
      # count(dfa2$siteID[dfa$routeID==501]) #Access database has 12 points for route 501: 01-12
      
    #For now, we can:
      #Ignore locations that have no TBC route/site number included
      #Ignore 153_13, 153_14, 153_15
      #Ignore supplemental observation for routes (since we don't know where exactly birds were seen)
      #Ignore all observations at 162_08 for 2019 and 2020 -- not sure where surveys were done and whether they were too close to other points
      #Assume that the 131_12 site was moved slightly in 2020 -- RENAME this point 131_13
      #Assume that the 501_01 site was moved in 2021 (note that stationary, 5-min count was in 2019) -- RENAME this point 501_19
      #Assume that the 512_01 location was moved in 2020 -- RENAME this point 512_06
      #Fix site name that had dash instead of underscore (145-12)
      
      dfe4 <- dfe3[grepl('TBC',dfe3$location),]
      dfe4 <- dfe4[!grepl('Supp',dfe4$location),]
      dfe4$location[grepl('131 12',dfe4$location)] <- 'TBC 131_13'
      dfe4$location[grepl('501_01',dfe4$location)] <- 'TBC 501_19'
      dfe4$location[dfe4$location=='TBC 512_01' & dfe4$yr>2019] <- 'TBC 512_06'
      dfe4$location[dfe4$location=='TBC 145-12'] <- 'TBC 145_12'
  
  #New site and route names
    dfe4$routeID <- as.numeric(substr(dfe4$location,5,7))
    dfe4$siteID <- substr(dfe4$location,5,10)
    dfe4$location <- NULL
  
  #Remove unnecessary columns and reorder:  
    dfe5 <- dfe4[c('routeID','siteID','lat','long','submission','n.observers','obsdate','yr','mon','doy',
                   'common.name','count')]

#- Bird species (appearing in either dataset) -------------------------------------------------------------#    

  spp.a <- data.frame(common.name=sort(unique(dfa2$common.name)))
  spp.e <- data.frame(common.name=sort(unique(dfe5$common.name))) #Based on Clements 2019 list
  
  #Indicator for unknown, hybrid or domestic species
    spp.a$unknown <- 1*grepl('sp[.]|spp[.]|Sp[.]|Unknown|Hybrid|hybrid|[/]|Domestic',spp.a$common.name)
    spp.e$unknown <- 1*grepl('sp[.]|spp[.]|Sp[.]|Unknown|Hybrid|hybrid|[/]|Domestic',spp.e$common.name)
  
  #Identify subspecies in eBird dataset
    spp.e$subspecies <- 1*grepl('[(]',spp.e$common.name)
    
  #Create new species labels (that exclude subspecies information) with unknown/hybrids/domestics = NA 
    spp.a$name.new <- ifelse(spp.a$unknown==1,NA,spp.a$common.name)
    spp.e$name.new <- sapply(strsplit(spp.e$common.name,' [(]'),'[',1)
    spp.e$name.new <- ifelse(spp.e$unknown==1,NA,spp.e$name.new)
    
  #Change names/capitalizations of species labels in Access dataset to match eBird
    #Function to capitalize names correctly:
      simpleCap <- function(x) {
        s <- strsplit(x, " ")[[1]]
        paste(toupper(substring(s, 1,1)), substring(s, 2),
              sep="", collapse=" ")
      }
      
    for(i in 1:nrow(spp.a)){
      spp.a$name.new[i] <- ifelse(is.na(spp.a$name.new[i]),NA,simpleCap(spp.a$name.new[i]))
    }
      
    spp.a$name.new[spp.a$common.name=='Mexican Mallard'] <- 'Mexican Duck'
    spp.a$name.new[spp.a$common.name=='Common Black-Hawk'] <- 'Common Black Hawk' 
    spp.a$name.new[spp.a$common.name=='Common Moorhen'] <- 'Common Gallinule'
    spp.a$name.new[spp.a$common.name=='Northern Beardless-tyrannulet'] <- 'Northern Beardless-Tyrannulet'
  
  #Add new species names to count dataframes
    dfa2$species <- spp.a$name.new[match(dfa2$common.name,spp.a$common.name)]
    dfe5$species <- spp.e$name.new[match(dfe5$common.name,spp.e$common.name)]
    
  #Remove rows with species=NA
    dfa2 <- dfa2[!is.na(dfa2$species),]
    dfe5 <- dfe5[!is.na(dfe5$species),]
  
  #Does each species have only one count per visit in Access DB? 
    dim(unique(dfa2[,c('visitID','species')])); nrow(dfa2) #No. Species has >1 count per visitID on 52 occasions.
    # head(dfa2[duplicated(dfa2[,c('visitID','species')]),])
    
    #This shouldn't happen -- probably due to a data entry problem (accidentally entering the count for a species twice)
    #For now, keeping the higher count:
    vissp <- ddply(dfa2,.(visitID,species),summarize,count.max=max(count))
    dfa3 <- unique(dfa2[,c('routeID','siteID','easting','northing','visitID','obsdate','yr','mon','doy','observer','species')])
    dfa3 <- join(dfa3,vissp,by=c('visitID','species'),type='left')

#- Merge Access and eBird datasets -------------------------------------------------------------#
  
  dfa.merge <- data.frame(routeID=dfa3$routeID,siteID=dfa3$siteID,
                          easting=dfa3$easting,northing=dfa3$northing,lat=NA,long=NA,
                          obsdate=dfa3$obsdate,yr=dfa3$yr,mon=dfa3$mon,doy=dfa3$doy,observer=dfa3$observer,n.observers=NA,
                          visitID=dfa3$visitID,submission=NA,species=dfa3$species,count=dfa3$count.max,source='Access')

  dfe.merge <- data.frame(routeID=dfe5$routeID,siteID=dfe5$siteID,
                          easting=NA,northing=NA,lat=dfe5$lat,long=dfe5$long,
                          obsdate=dfe5$obsdate,yr=dfe5$yr,mon=dfe5$mon,doy=dfe5$doy,observer=NA,n.observers=dfe5$n.observers,
                          visitID=NA,submission=dfe5$submission,species=dfe5$species,count=dfe5$count,source='eBird')
    #In dfe.merge, each site surveyed only once per day
    # nrow(dfe.merge); nrow(unique(dfe.merge[,c('siteID','obsdate','species')])) 
  
  #Determine if there are duplicate records between the two datasets
  #(could occur if observer entered data into Access and eBird or if the data from eBird was already imported into Access)
    dfa.visits <- ddply(dfa.merge,.(siteID,obsdate,observer,visitID),summarize,
                        n.species=length(species),total.abund=sum(count))
    dfe.visits <- ddply(dfe.merge,.(siteID,obsdate,submission),summarize,
                        n.species.e=length(species),total.abund.e=sum(count))
    dfa.visits.e <- join(dfa.visits,dfe.visits,by= c('siteID','obsdate'),type='left')
    overlap <- dfa.visits.e[!is.na(dfa.visits.e$submission),]
    overlap$same.sp <- 1*(overlap$n.species==overlap$n.species.e)
    overlap$same.abund <- 1*(overlap$total.abund==overlap$total.abund.e)
    
    #If same.sp and same.abund == 1, then it's a duplicate record -- can remove these submissions from eBird dataset
    
    #What if the data aren't exactly the same?

      #Number of species the same, but abundance different? (2 occurrences)
      # overlap[rowSums(overlap[,c('same.sp','same.abund')])==1,]
      # dfa[dfa$visitID==24311,]; dfe[dfe$submission=='S55975941',]
      # dfa[dfa$visitID==24756,]; dfe[dfe$submission=='S56229513',]
      # dfa[dfa$visitID==25050,]; dfe[dfe$submission=='S67433687',] 
      #In all cases, one count off. Probably entered wrong
      
      #Number of species different? (16 occurrences)
      # overlap[rowSums(overlap[,c('same.sp','same.abund')])==0,]
      #In all cases, species counts differ by 1 and abundance is very similar.  Probably forgot to enter one species
  
    #We can remove either eBird submissions or Access visit records that are duplicates of data in the other dataset.
    #We'll keep the record that has more species or higher counts
      overlap$exclude.source <- ifelse(overlap$n.species>=overlap$n.species.e,'eBird','Access')
      overlap$exclude.visit <- ifelse(overlap$exclude.source=='Access',overlap$visitID,NA)
      overlap$exclude.subm <- ifelse(overlap$exclude.source=='eBird',overlap$submission,NA) 
      visit.ex <- overlap$exclude.visit[!is.na(overlap$exclude.visit)]
      subm.ex <- overlap$exclude.subm[!is.na(overlap$exclude.subm)]
      dfa.merge <- dfa.merge[!dfa.merge$visitID %in% visit.ex,]
      dfe.merge <- dfe.merge[!dfe.merge$submission %in% subm.ex,]
  
  #Merge the two datasets:
    df <- rbind(dfa.merge,dfe.merge)

  #Create a unique "surveyID" that can be used to group counts of birds observed at a site on a 
  #particular date (and time) by one observer
    df$combo <- paste(df$visitID,df$submission,sep='.')
    matchdf <- data.frame(combo=unique(df$combo),surveyID=1:length(unique(df$combo)))
    df$surveyID <- matchdf$surveyID[match(df$combo,matchdf$combo)]
    df$combo <- NULL
    
#- Create unique lists of sites (with geographic locations) --------------------------------#
  
  sites <- ddply(df,.(siteID,routeID),summarize,firstyr=min(yr),lastyr=max(yr),
                 n.years=length(unique(yr)),n.visits=length(unique(surveyID)))
  sites <- join(sites,dfa.merge[,c('siteID','easting','northing')],by='siteID',type='left',match='first')
  sites <- sites[with(sites,order(routeID,siteID)),]

  #Indicator for sites that were retired (or moved) -- have lowercase letter added to siteID
    sites$retired <- 1*grepl('[a-z]',sites$siteID)
  #Indicator for transects (in parks) -- have uppercase letter in siteID
    sites$transect <- 1*grepl('[A-Z]',sites$siteID)  
  
  #Need to fill in UTMs for sites that only appear in eBird dataset (and have lat/longs) 
    #A few of them are new because they were moved in recent years and we just renamed them in code above: 131_13, 501_19, 512_06
    #Others look like new points that were added to a couple routes: 501_13 through 501_19 and 537_09, 537_10.  
    sites.e <- unique(dfe.merge[,c('siteID','routeID','lat','long')])
    sites.e1 <- sites.e[!sites.e$siteID %in% dfa.merge$siteID,]  

    #Converting lat/longs to UTMs, NAD83, Zone 12 (like Access Data)
    ll.e <- st_as_sf(sites.e1,crs=4269,coords=c('long','lat'))
    locs.e <- st_transform(ll.e,crs=26912)
    sites.e1 <- cbind(sites.e1,st_coordinates(locs.e))

    #Add UTMs to sites dataframe
    sites <- join(sites,sites.e1[,c('siteID','X','Y')],by='siteID',type='left')
    sites$easting <- ifelse(is.na(sites$easting),round(sites$X),sites$easting)
    sites$northing <- ifelse(is.na(sites$northing),round(sites$Y),sites$northing)
    sites$X <- NULL
    sites$Y <- NULL
    
    #Add UTMs to count dataframe
    df$easting <- NULL
    df$northing <- NULL
    df <- join(df,sites[,c('siteID','easting','northing')],by='siteID',type='left')
    
  #Note: Will need to look more at retired sites - distances between original and relocated sites aren't always small (eg, 96_09)
    # sites[grepl('96_09',sites$siteID),]
    #Moved a short distance between original (96_09a) and next (96_09b), but then moved 920 m to
    #location surveyed in 2017 (96_09).    
  
#- Create unique lists of routes -----------------------------------------------------------------#
    
  routes <- ddply(sites,.(routeID),summarize,firstyr=min(firstyr),lastyr=max(lastyr),
                  n.sites=length(unique(siteID)),n.sites.ret=sum(retired))
  
    #Calculate number of current sites in route
    routes$n.sites.current <- routes$n.sites - routes$n.sites.ret
    #Calculate number of years at least one site in a route was surveyed
    routes.yrs <- ddply(df,.(routeID),summarize,n.years=length(unique(yr)))
    routes$n.years <- routes.yrs$n.years[match(routes$routeID,routes.yrs$routeID)]

  #Create indicators for different types of routes:
    routes$r.urban <- ifelse(routes$routeID %in% 100:199,1,0)                 #Urban routes
    routes$r.parks <- ifelse(routes$routeID < 100 | routes$routeID >199,1,0)  #Park routes

#- Export counts, sites, routes dataframes -------------------------------------------------------#
  
  df.export <- df[,c('routeID','siteID','easting','northing','obsdate','yr','mon','doy','observer',
                     'n.observers','surveyID','species','count','source')]
  
  # write.csv(df.export,'MergedData/TBC_counts.csv',row.names=FALSE)
  # write.csv(sites,'MergedData/TBC_sites.csv',row.names=FALSE)
  # write.csv(routes,'MergedData/TBC_routes.csv',row.names=FALSE)
  
#- Creating bird species list for TBC and appending traits -----------------------------------------------------#   
   
  tbcbirds <- data.frame(common.name=sort(unique(c(spp.e$name.new,spp.a$name.new))))
  
  #A list of birds in southern Arizona with *preliminary* listing of characteristics
    #native.US = native/non-native in the US
    #status.SEAZ = migratory status in southeastern Arizona 
      #Year-Round, Migrant, Breeding [summer], Winter, Rarity [not supposed to be here], Released
    #diet (many missing values)
    #group = taxonomic group (can subsequently be grouped into higher levels)
  allbirds <- read.csv('OriginalData/FullBirdList_Preliminary.csv',header=TRUE,na.strings=c('NA',''))
  
  #Attach information to list of birds from TBC:
  tbcbirds <- join(tbcbirds,allbirds,by='common.name',type='left')
  
  #Export TBC bird list:
  # write.csv(tbcbirds,'MergedData/TBC_birds.csv',row.names=FALSE)
  
#- Plot site locations -------------------------------------------------------------#    
  
  streets <- st_read(dsn='PimaCo_StreetsMajor',layer='Street_Network_-_Major')  
  streets.83 <- st_transform(streets,crs=26912) #crs=26912 = NAD83, zone 12N
  
  locations <- sites[,c('routeID','siteID','easting','northing')]
  locs <- st_as_sf(locations,crs=26912,coords=c('easting','northing'))
  # locs[1:3,]
  
  #Plot all sites -- current and retired
  plot(st_geometry(streets.83),axes=TRUE,xlim=c(470000,540000),ylim=c(3540000,3620000))
  plot(st_geometry(locs),pch=19,cex=0.7,col='steelblue3',add=TRUE)
  #There is one route that's way further north and one pretty far SE, 
  #which I'll eliminate for now so I can view others in close
  
  #Focusing in on all but a couple routes   
  plot(st_geometry(streets.83),axes=TRUE,xlim=c(475000,540000),ylim=c(3550000,3596000))
  plot(st_geometry(locs),pch=19,cex=0.7,col='steelblue3',add=TRUE) 
  