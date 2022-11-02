# Exploring Tucson Bird Count data

First stage of the project: cleaning up and merging two sets of Tucson Bird Count data.  
1. TBC data entered directly into Access database (currently managed by Tucson Audubon): [TucsonBirdCount-Access.csv](OriginalData/TucsonBirdCount-Access.csv)
2. TBC data entered into eBird: [TucsonBirdCount-eBird.csv](OriginalData/TucsonBirdCount-eBird.csv)

For more on the TBC, see the [Tucson Audubon website](https://tucsonaudubon.org/our-work/conserving-birds/citizen-science/tucson-bird-count/).  
Briefly, there are more than 100 routes in the Tucson Valley. Most routes contain 8-12 survey locations (sites). Urban routes are surveyed one morning each spring, between 15 April and 15 May. Sites in parks are surveyed quarterly. At most sites, observers conduct a 5-min stationary count, recording the number of birds of each species observed.

Brief description of each file (csv) with original data:
1. [TucsonBirdCount-Access.csv](OriginalData/TucsonBirdCount-Access.csv): Data from Tucson Bird Count surveys that were entered directly into an Access database.
    - SiteID: unique ID for each site, with the associated route number before the underscore 
	- Alpha: 4-letter species code
	- Common Name: species common name
	- Date: date of survey
	- Number: species count (number of birds seen and/or heard during 5-min count)
	- NumberOPC: number of "supplemental" observations (birds seen away from survey location, outside 5-min window, or by someone other than the primary observer)
	- Longitude
	- Latitude
	- First and Last: observer name
	- VisitID: unique ID for independent survey of a site
	
2. [TucsonBirdCount-eBird.csv](OriginalData/TucsonBirdCount-eBird.csv): Data from Tucson Bird Count surveys that were entered directly into eBird.
    - Submission ID: sampling event identifier from eBird
	- Common Name: species common name
	- Taxonomic order: ID number for bird species/subspecies
	- Count: number of birds seen and/or heard during 5-min count
	- State/Province
	- County
	- Location ID: unique ID number for each location (from eBird)
	- Location: location name in eBird
	- Latitude
	- Longitude
	- Date: date of survey
	- Time: time of survey
	- Protocol: type of eBird survey (Stationary, Traveling, or Casual/Incidental)
	- Duration (min): duration of survey, in minutes
	- All Obs Reported: 1 if all bird observations were reported, 0 if not
	- Distance traveled (km): only relevant for traveling surveys
	- Area Covered: not relevant
	- Number of Observers: for the TBC, we assume that one person was the primary observer (even if more observers were listed)
	- Breeding Code: eBird breeding code
	- Observation Details: details about observation added by observer
	- Checklist Comments: details about the survey added by observer
	- ML Catalog Numbers
	
After cleaning up and merging data from Access and eBird (in [TBC_DataCleanUp.R](TBC_DataCleanUp.R)), four files were exported and put in the [MergedData](MergedData) folder:
1. [TBC_counts.csv](MergedData/TBC_counts.csv): Count data, where each row is associated with a single bird species observed at one site, on one day/time, by one observer. This dataset does not include observations of birds outside the survey area or outside the 5-min survey window, and does not include observations made by someone other than the primary observer. This dataset excludes surveys that violated TBC protocols (eg, traveling or incidental counts in eBird, stationary counts that were not 5-min in duration, counts that occurred before 5am or after noon). One exception, however, is a few sites in parks that are surveyed by walking transects rather than surveyed with stationary points (sites with "T" in the siteID). 
	- routeID: unique route ID (ID numbers between 100 and 200 are urban routes; other routes are in parks or protected areas)
	- siteID: unique site ID (includes route ID before the underscore)
	- easting and northing: geographic location, UTMs in NAD83, Zone 12
	- obsdate: date of survey
	- yr: year of survey (ranges from 2001 through 2021)
	- mon: month of survey
	- doy: numeric day of the year
	- observer: name of primary observer (currently only available for data entered into Access)
	- n.observers: number of people in group (this information not available for data entered in Access). Even if there are multiple people in a group, only one person is designated as the primary observer. 
	- surveyID: unique ID for each independent survey of a site at a particular day/time by a single primary observer
	- species: common name of species (data set does not include unknowns or hybrids; subspecies information was removed)
	- count: number of birds seen and/or heard (only on official count, does not include supplemental observations)
	- source: where was data entered? Access or eBird
	
2. [TBC_routes.csv](MergedData/TBC_routes.csv): Information about each TBC route.
	- routeID: unique route ID (ID numbers between 100 and 200 are urban routes; other routes are in parks or protected areas)
	- firstyr: first year surveyed
	- lastyr: most recent year surveyed (note: all 2021 data may not be included)
	- n.sites: total number of unique sites ever surveyed as part of this route
	- n.sites.ret: number of sites in route that were retired (no longer surveyed, or moved because of development or access issues)
	- n.sites.current: number of sites currently included in route
	- n.years: number of years at least one site in the route was surveyed
	- r.urban: 1 if route is in an urban area, 0 otherwise (park or protected area)
	- r.parks: 1 if route is a park or protected area, 0 otherwise

3. [TBC_sites.csv](MergedData/TBC_sites.csv): Information about each TBC site.
	- siteID: unique site ID (includes associated route ID before the underscore)
	- routeID
	- firstyr: first year surveyed
	- lastyr: most recent year surveyed (note: all 2021 data may not be included)
	- n.years: number of years surveyed
	- n.visits: number of unique surveys at each site
	- easting and northing: geographic location, UTMs in NAD83, Zone 12
	- retired: 1 if the site has been retired (no longer surveyed, or moved because of development or access issues), 0 if not
	- transect: 1 if the site is surveyed with a walking transect, 0 if surveyed with a stationary count

4. [TBC_birds.csv](MergedData/TBC_birds.csv): Preliminary table with information about each species included in the TBC data.
 	- common.name: species common name
	- native.US: native or non-native in the US
	- status.SEAZ: migratory status in southeastern Arizona: Year-Round, Migrant, Breeding (summer), Winter, Rarity (not supposed to be in SE Arizona), Released. (Note: these are preliminary classifications and will need to be confirmed)
	- diet: preliminary classification of most-common diet items
	- group: taxonomic group (can subsequently be grouped into higher levels)
