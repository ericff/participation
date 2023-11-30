## particip.R
## Exploration of Labor Force Participation Rate graphs

## blsAPI function is by Mike Silva, obtained from https://www.bls.gov/developers/api_r.htm

################################################
### INSTRUCTIONS
################################################


## NOTE that there are several parameters in these instructions you may
##  want to set.

## SET THESE to get different start and end years on
## the graphs
start.year = 1973
end.year = 2023

## Restrict to first 100 of 1457 series with "Participation" in the name:
## If one does all of them, it's probably better to get credentials from BLS
## to avoid the daily limit set by the BLS API that does not demand credentials.
first.n.series = 100


## We will later split up the series codes into the maximum number (25) you can
## download at 
## a time with v1 of the BLS API. (We use v1 so we don't have to deal with credentials
## here.)
## See https://www.bls.gov/developers/api_faqs.htm
## It's probably good to keep this next parameter at 25:
max.series.per.request <- 25

##
## YOU SHOULD SET THIS yourself or create this directory:
##   e.g. your directory might be "~/jobs/lfpr"
homedir = "~/jobs/lfpr"

## The end result will be a file called something like graphs.lfpr.20010101.pdf,
## which will sit inside the directory homedir that you chose above.
## Note that it will write over any graph file created that same day.
## and it will also save an R data file there, because
##  if one loses it, you may have to wait a day to download it again (or register
##  for the API).
## It will save a new file for each day, and will write over one saved on the same day.


################################################
### END INSTRUCTIONS
################################################





## You need to install these, and rjson and RCurl, if you have not already
library(data.table)
library(jsonlite)
library(dplyr)
library(purrr)



###############################################################
## blsAPI function, by Mike Silva
###############################################################

## blsAPI.R is by Mike Silva, obtained from https://www.bls.gov/developers/api_r.htm
## with slight modification to suppress warning that isn't needed

## blsAPI.R 
## by Mike Silva, obtained from https://www.bls.gov/developers/api_r.htm
##
##' This function allows users to request data from the BLS API.   
##' See https://www.bls.gov/developers/ and https://www.bls.gov/developers/api_signature.htm for more details. 
##' @param data Information to send to the API. 
##' @keywords bls api economics 
##' @export 
##' @examples 
##' ## These examples are taken from https://www.bls.gov/developers/api_signature.htm 
##'  
##' ## Single Series request 
##' response <- blsAPI('LAUCN040010000000005') 
##' json <- fromJSON(response) 
##'  
##' #### Multiple Series  
##' payload <- list('seriesid'=c('LAUCN040010000000005','LAUCN040010000000006')) 
##' response <- blsAPI(payload) 
##' json <- fromJSON(response) 
##'  
##' ## One or More Series, Specifying Years 
##' payload <- list('seriesid'=c('LAUCN040010000000005','LAUCN040010000000006'), 'startyear'='2010', 'endyear'='2012') 
##' response <- blsAPI(payload) 
##' json <- fromJSON(response) 
 
blsAPI <- function(data=NA){ 
  require(rjson) 
  require(RCurl) 
  h = basicTextGatherer() 
  h$reset()
  ## flag added by EF to suppress warning which isn't needed in
  ##    the case that data has length >= 2:
  flag.to.continue.download <- TRUE
  if (length(data) <=1){
      if (is.na(data)){ 
          message('blsAPI: No parameters specified.')
          flag.to.continue.download <- FALSE
      } 
  }
  if (flag.to.continue.download) { 
    ## Parameters specified so make the request 
    if(is.list(data)){ 
      ## Multiple Series or One or More Series, Specifying Years request 
      curlPerform(url='https://api.bls.gov/publicAPI/v1/timeseries/data/', 
                httpheader=c('Content-Type' = "application/json;"), 
                postfields=rjson::toJSON(data), 
                verbose = FALSE,  
                writefunction = h$update 
      ) 
    }else{ 
      ## Single Series request 
      curlPerform(url=paste0('https://api.bls.gov/publicAPI/v1/timeseries/data/',data), 
                verbose = FALSE,  
                writefunction = h$update 
      ) 
    } 
  } 
  h$value() 
} 
###############################################################
## end blsAPI, by Mike Silva
###############################################################




###############################################################
## Helper functions for converting data
###############################################################

# BLS often stores month as, e.g. M02, convert year (say, 2003), and this, to 200302, output is numeric
# THIS IS FOR SCALARS
# ASSUMES blsperiod.t is CHARACTER ARGUMENT

# blsperiod.t="M02"
# convert.bls.period.to.decimal.scalar(blsperiod.t="M02")

convert.bls.period.to.decimal.scalar<-function(blsperiod.t){
	month.as.numeric.t<- as.numeric(substr(x=blsperiod.t, start=2, stop=3))
	## month.as.two.digit.character.t<- paste(strsplit(blsperiod.t,split="")[[1]][2:3],collapse="")
	(1/12)*month.as.numeric.t - (1/24)
}

## this is for scalars
## convert.year.and.bls.period.to.decimal.scalar(year = 2010, blsperiod="M02")
## year = 2010; blsperiod.t="M02"
convert.year.and.bls.period.to.decimal.scalar<-function(year, blsperiod){
    as.numeric(year) + convert.bls.period.to.decimal.scalar(blsperiod)
}



## convert.year.and.bls.period.to.decimal(year = c(2009,2010), blsperiod=c("M02","M12"))
## this is for vectors
## 
convert.year.and.bls.period.to.decimal<-function(year, blsperiod){
    map2_dbl(.x=year, .y=blsperiod, .f=convert.year.and.bls.period.to.decimal.scalar)
}

###############################################################
## End of helper functions for converting data
###############################################################







## Download the files

## Split into 10 year increments, which is the maximum according to one http response I got, although it appears to say 20 years here: https://www.bls.gov/developers/api_signature_v2.htm#multiple

download.start.years <- seq.int(from=start.year, to=end.year, by =10)
download.end.years <- c(tail(download.start.years,n=-1)-1,end.year)


## #####################################################################
## ## Which series to download. This will be hard-coded for now, rather
## ## than asking a user to manually download the file with the series names.
## ## See here where it discusses what to me suggests a list cannot be
## ## obtained with the API, although it can be downloaded manually.
## ## 
## ## "I don't know the series ID(s) for the data I want to access. How do I find the
## ##  ID(s)?
## ##  The BLS Public Data API requires users to know the series ID to request
## ##  data. We do not currently have a catalogue of series IDs, but all BLS
## ##  series IDs follow a similar format."
## ##  https://www.bls.gov/developers/api_faqs.htm#signatures3
## #####################################################################
## ## Source: https://download.bls.gov/pub/time.series/ln/ln.series, downloaded and cleaned manually
## ## One can do it this way after saving it to file ln.seriesnames.txt
## ln.series.raw <- fread(file.path(homedir,"ln.seriesnames.txt"))
## ## 

## ## Remove quarterly series
## ln.series.raw <- ln.series.raw[tail(series_id,n=1)!="Q",]
## ## Find the last character of each series_id:
## library(stringr)
## ln.series.raw[,last.char.series.name:= str_sub(series_id, start=-1L, end=-1L)]
## ln.series.raw <- subset(ln.series.raw, last.char.series.name!="Q")

## ## Restrict to series having "Participation" in the title
## ln.series.raw <- subset(ln.series.raw,grepl(pattern="Participation", x= ln.series.raw$series_title, ignore.case=T))
## ## There are 1457 of these, restrict to first "first.n.series"
##     so you don't bump up against
##     API v1 daily limit, so as not to have to get credentials to run this, rather than
##     making a user have to get credentials for v2. But one can repeat this
##     commented part with a higher value of first.n.series to get more.
## seriescodes.temp <- ln.series.raw$series_id[1:first.n.series]
## seriestitles.temp <- ln.series.raw$series_title[1:first.n.series]
## ## print them out to hard code them:
## cat(paste('series.codes <- c("', paste(seriescodes.temp, collapse='","'), '")', sep=""))
## cat(paste('series.titles <- c("', paste(seriestitles.temp, collapse='","'), '")', sep=""))
## Then cut and paste manually to get the below. One might need to split it up by doing
## something like the following because of line length limitations in R
## ## series.codes <- c(series.codes, "",""), where one puts a large list of series
## ##            codes in place of "",""  

## #####################################################################
## End portion of code to use if one wants to get IDs and titles for more series 
## #####################################################################



series.codes <- c("LNS11300000","LNS11300001","LNS11300002","LNS11300003","LNS11300004","LNS11300005","LNS11300006","LNS11300007","LNS11300008","LNS11300009","LNS11300012","LNS11300013","LNS11300014","LNS11300015","LNS11300016","LNS11300017","LNS11300018","LNS11300019","LNS11300020","LNS11300021","LNS11300022","LNS11300023","LNS11300024","LNS11300025","LNS11300026","LNS11300027","LNS11300028","LNS11300029","LNS11300030","LNS11300031","LNS11300032","LNS11300033","LNS11300034","LNS11300035","LNS11300036","LNS11300037","LNS11300038","LNS11300048","LNS11300049","LNS11300050","LNS11300060","LNS11300061","LNS11300062","LNS11300086","LNS11300088","LNS11300089","LNS11300091","LNS11300093","LNS11300150","LNS11300152","LNS11300154","LNS11300164","LNS11300173","LNS11300182","LNS11300315","LNS11300317","LNS11300319","LNS11300327","LNS11300334","LNS11300341","LNS11324230","LNS11324231","LNS11324232","LNS11324885","LNS11324886","LNS11324887","LNS11325669","LNS11327659","LNS11327660","LNS11327662","LNS11327689","LNS11332183","LNS11349526","LNS11349527","LNS11349601","LNS11349602","LNU01300000","LNU01300001","LNU01300002","LNU01300003","LNU01300004","LNU01300005","LNU01300006","LNU01300007","LNU01300008","LNU01300009","LNU01300010","LNU01300011","LNU01300012","LNU01300013","LNU01300014","LNU01300015","LNU01300016","LNU01300017","LNU01300018","LNU01300019","LNU01300020","LNU01300021","LNU01300022","LNU01300023")


series.titles <- c("(Seas) Labor Force Participation Rate","(Seas) Labor Force Participation Rate - Men","(Seas) Labor Force Participation Rate - Women","(Seas) Labor Force Participation Rate - White","(Seas) Labor Force Participation Rate - White Men","(Seas) Labor Force Participation Rate - White Women","(Seas) Labor Force Participation Rate - Black or African American","(Seas) Labor Force Participation Rate - Black or African American Men","(Seas) Labor Force Participation Rate - Black or African American Women","(Seas) Labor Force Participation Rate - Hispanic or Latino","(Seas) Labor Force Participation Rate - 16-19 yrs.","(Seas) Labor Force Participation Rate - 16-19 yrs., Men","(Seas) Labor Force Participation Rate - 16-19 yrs., Women","(Seas) Labor Force Participation Rate - 16-19 yrs., White","(Seas) Labor Force Participation Rate - 16-19 yrs., White Men","(Seas) Labor Force Participation Rate - 16-19 yrs., White Women","(Seas) Labor Force Participation Rate - 16-19 yrs., Black or African American","(Seas) Labor Force Participation Rate - 16-19 yrs., Black or African American Men","(Seas) Labor Force Participation Rate - 16-19 yrs., Black or African American Women","(Seas) Labor Force Participation Rate - 16-19 yrs., Hispanic or Latino","(Seas) Labor Force Participation Rate - 16-19 yrs., Hispanic or Latino, Men","(Seas) Labor Force Participation Rate - 16-19 yrs., Hispanic or Latino, Women","(Seas) Labor Force Participation Rate - 20 yrs. & over","(Seas) Labor Force Participation Rate - 20 yrs. & over, Men","(Seas) Labor Force Participation Rate - 20 yrs. & over, Women","(Seas) Labor Force Participation Rate - 20 yrs. & over, White","(Seas) Labor Force Participation Rate - 20 yrs. & over, White Men","(Seas) Labor Force Participation Rate - 20 yrs. & over, White Women","(Seas) Labor Force Participation Rate - 20 yrs. & over, Black or African American","(Seas) Labor Force Participation Rate - 20 yrs. & over, Black or African American Men","(Seas) Labor Force Participation Rate - 20 yrs. & over, Black or African American Women","(Seas) Labor Force Participation Rate - 20 yrs. & over, Hispanic or Latino","(Seas) Labor Force Participation Rate - 20 yrs. & over, Hispanic or Latino, Men","(Seas) Labor Force Participation Rate - 20 yrs. & over, Hispanic or Latino, Women","(Seas) Labor Force Participation Rate - 20-24 yrs.","(Seas) Labor Force Participation Rate - 20-24 yrs., Men","(Seas) Labor Force Participation Rate - 20-24 yrs., Women","(Seas) Labor Force Participation Rate - 25 yrs. & over","(Seas) Labor Force Participation Rate - 25 yrs. & over, Men","(Seas) Labor Force Participation Rate - 25 yrs. & over, Women","(Seas) Labor Force Participation Rate - 25-54 yrs.","(Seas) Labor Force Participation Rate - 25-54 yrs., Men","(Seas) Labor Force Participation Rate - 25-54 yrs., Women","(Seas) Labor Force Participation Rate - 16-17 yrs.","(Seas) Labor Force Participation Rate - 18-19 yrs.","(Seas) Labor Force Participation Rate - 25-34 yrs.","(Seas) Labor Force Participation Rate - 35-44 yrs.","(Seas) Labor Force Participation Rate - 45-54 yrs.","(seas) Labor force participation rate - Married, spouse present, men","(Seas) Labor Force Participation Rate - 16-17 yrs., Men")
series.titles <- c(series.titles,"(Seas) Labor Force Participation Rate - 18-19 yrs., Men","(Seas) Labor Force Participation Rate - 25-34 yrs., Men","(Seas) Labor Force Participation Rate - 35-44 yrs., Men","(Seas) Labor Force Participation Rate - 45-54 yrs., Men","(seas) Labor force participation rate - Married, spouse present, women","(Seas) Labor Force Participation Rate - 16-17 yrs., Women","(Seas) Labor Force Participation Rate - 18-19 yrs., Women","(Seas) Labor Force Participation Rate - 25-34 yrs., Women","(Seas) Labor Force Participation Rate - 35-44 yrs., Women","(Seas) Labor Force Participation Rate - 45-54 yrs., Women","(Seas) Labor Force Participation Rate - 55 yrs. & over","(Seas) Labor Force Participation Rate - 55 yrs. & over, Men","(Seas) Labor Force Participation Rate - 55 yrs. & over, Women","(Seas) Labor Force Participation Rate - 16-24 yrs., Men","(Seas) Labor Force Participation Rate - 16-24 yrs., Women","(Seas) Labor Force Participation Rate - 16-24 yrs.","(seas) Labor force participation rate - Married, spouse present","(Seas) Labor Force Participation Rate - Less than a High School Diploma, 25 yrs. & over","(Seas) Labor Force Participation Rate - High School Graduates, No College, 25 yrs. & over","(Seas) Labor Force Participation Rate - Bachelor's degree and higher, 25 yrs. & over","(Seas) Labor Force Participation Rate - Some College or Associate Degree, 25 yrs. & over","(Seas) Labor Force Participation Rate - Asian","(Seas) Labor Force Participation Rate - Veterans, 18 years and over","(Seas) Labor Force Participation Rate - Veterans, 18 years and over, Men","(Seas) Labor Force Participation Rate - Nonveterans, 18 years and over","(Seas) Labor Force Participation Rate - Nonveterans, 18 years and over, Men","(Unadj) Labor Force Participation Rate","(Unadj) Labor Force Participation Rate - Men","(Unadj) Labor Force Participation Rate - Women","(Unadj) Labor Force Participation Rate - White","(Unadj) Labor Force Participation Rate - White Men","(Unadj) Labor Force Participation Rate - White Women","(Unadj) Labor Force Participation Rate - Black or African American","(Unadj) Labor Force Participation Rate - Black or African American Men","(Unadj) Labor Force Participation Rate - Black or African American Women","(Unadj) Labor Force Participation Rate - Hispanic or Latino","(Unadj) Labor Force Participation Rate -  Hispanic or Latino Men","(Unadj) Labor Force Participation Rate -  Hispanic or Latino Women","(Unadj) Labor Force Participation Rate - 16-19 yrs.","(Unadj) Labor Force Participation Rate - 16-19 yrs., Men","(Unadj) Labor Force Participation Rate - 16-19 yrs., Women","(Unadj) Labor Force Participation Rate - 16-19 yrs., White","(Unadj) Labor Force Participation Rate - 16-19 yrs., White Men","(Unadj) Labor Force Participation Rate - 16-19 yrs., White Women","(Unadj) Labor Force Participation Rate - 16-19 yrs., Black or African American","(Unadj) Labor Force Participation Rate - 16-19 yrs., Black or African American Men","(Unadj) Labor Force Participation Rate - 16-19 yrs., Black or African American Women","(Unadj) Labor Force Participation Rate - 16-19 yrs., Hispanic or Latino","(Unadj) Labor Force Participation Rate - 16-19 yrs., Hispanic or Latino Men","(Unadj) Labor Force Participation Rate - 16-19 yrs., Hispanic or Latino Women")


## Function to download one group of series for one specific 10 year time period
## Example:
## test1 <- download.one.group.of.series.for.one.time.period(series.ids.to.download=c("LNS11300000","LNS11300001"), start.time.period='2014', end.time.period='2023')
download.one.group.of.series.for.one.time.period <- function(series.ids.to.download, start.time.period, end.time.period){
    blsAPI(list('seriesid'=series.ids.to.download, 'startyear'=as.character(start.time.period), 'endyear'=as.character(end.time.period)))
}

## Function to download one group of series for the set of all 10 year time periods
## Example:
## test2 <- download.one.group.of.series.for.multiple.time.periods(series.ids.to.download=c("LNS11300000","LNS11300001"), start.years.for.download=c(1973,1983,1993,2003,2013,2023), end.years.for.download=c(1982,1992,2002,2012,2022,2023))
download.one.group.of.series.for.multiple.time.periods <- function(series.ids.to.download, start.years.for.download, end.years.for.download){
    n.time.periods.to.download <- length(start.years.for.download)
    stopifnot(n.time.periods.to.download >=1 & (n.time.periods.to.download ==length(end.years.for.download)))
    raw.output <- list()
    for (i.period in 1:n.time.periods.to.download){
        raw.output[[i.period]] <- jsonlite::fromJSON(download.one.group.of.series.for.one.time.period(series.ids.to.download=series.ids.to.download, start.time.period = start.years.for.download[i.period], end.time.period=end.years.for.download[i.period]))
        ## Check that download succeeded:
        if (raw.output[[i.period]]$status != "REQUEST_SUCCEEDED"){
            stop(paste("Request failed for period", i.period, " for these series ids: ", paste(series.ids.to.download, collapse=",")))
        }
    }
    ## Rearrange so that there is one data frame of the list for every series; i.e.
    ##   merge the years together
    ## Do a loop over each series:
    list.of.data.frames.for.each.series <- list()
    n.series.ids <- length(series.ids.to.download)
    for (i.series in 1:n.series.ids) {
        ## Get a list of data frames for just the series
        list.of.data.for.this.series <- rev(lapply(X= raw.output, FUN=function(x){ x$Results$series$data[[i.series]]}))
        ## Drop the "latest" column
        list.of.data.for.this.series <- lapply(list.of.data.for.this.series, FUN=function(x){x$latest <- NULL; x})
        
        ## Now bind them together into one dataframe, and after binding, reverse the row order so that oldest observations are first
        list.of.data.frames.for.each.series[[i.series]] <- bind_rows(list.of.data.for.this.series) %>%
            arrange(desc(row_number()))
    }
    ## output the raw results as well in case you want to check for download issues
    list(list.of.data.frames.for.each.series=list.of.data.frames.for.each.series, raw.output=raw.output)
}

## Do all the downloads
## output.from.downloads <- do.all.downloads(series.codes, first.n.series, max.series.per.request)
do.all.downloads <- function(series.codes, first.n.series, max.series.per.request){
    ## Now split up the series codes into the maximum number (25) you can download at
    ## a time with v1 of the BLS API. (We use v1 so we don't have to deal with credentials
    ## here.)
    ## See https://www.bls.gov/developers/api_faqs.htm

    download.start.indices.series <- seq.int(from=1, to=first.n.series, by = max.series.per.request)
    download.end.indices.series <- c(tail(download.start.indices.series,n=-1)-1,first.n.series)

    n.requests.of.groups.of.series <- length(download.start.indices.series)

    ## Now do the downloads; put each one in an element of the list
    ##   list.of.downloaded.data
    ## But also put together the (n.requests.of.groups.of.series)
    ##    lists called list.of.data.frames.for.each.series into one large
    ##    list, e.g. of length 100, with one data frame for each series


    list.of.series.for.requests <- list()
    list.of.downloaded.data <- list()

    ## this next list will be one big list of (e.g.) 100 df's, rather than just 25 each:
    list.of.data.frames.for.series.over.all.requests <- list()

    for (i.request in 1:n.requests.of.groups.of.series){
        cat("Working on request ", i.request," of ", n.requests.of.groups.of.series, ".\n", sep="")
        list.of.series.for.requests[[i.request]] <- series.codes[download.start.indices.series[i.request]:download.end.indices.series[i.request]]
        list.of.downloaded.data[[i.request]] <- download.one.group.of.series.for.multiple.time.periods(series.ids.to.download=list.of.series.for.requests[[i.request]], start.years.for.download=download.start.years, end.years.for.download=download.end.years)
        list.of.data.frames.for.series.over.all.requests <- c(list.of.data.frames.for.series.over.all.requests,list.of.downloaded.data[[i.request]]$list.of.data.frames.for.each.series)
    }
    list(list.of.data.frames.for.series.over.all.requests=list.of.data.frames.for.series.over.all.requests, list.of.downloaded.data=list.of.downloaded.data)
}

## test5 <- clean.one.df.series(df.one.series=list.of.data.frames.for.series.over.all.requests[[1]])
## Make the decimalperiod variable, which is useful for plotting.
clean.one.df.series <- function(df.one.series){
    df.one.series %>%
        mutate(.data = df.one.series, "decimalperiod"=convert.year.and.bls.period.to.decimal(year = year, blsperiod=period))    
}


## Plot the trailing 12-month average, for just one data series
## plot.one.data.series(series.id = series.codes[77], series.name = series.titles[77], nicedf=list.of.clean.dfs[[77]], start.year = start.year, end.year = end.year)
#
plot.one.data.series <- function(series.id, series.name, nicedf, start.year, end.year){
    ## Smooth, and then remove NAs that will occur because of the smoothing:
    xy.for.graphing <- nicedf %>%
        mutate(smoothed.percentage= stats::filter(value, rep(1/12,12), sides=1)) %>%
        select(decimalperiod, smoothed.percentage) %>%
        filter(!is.na(decimalperiod) & !is.na(smoothed.percentage))
    plot(xy.for.graphing, type="l", ann=FALSE, col="lightblue", lwd=3, xlim = c(start.year, end.year+1))
    ttmain = paste0(series.name, "\nTrailing 12 Month Average, Series ID ", series.id, ", ", start.year, "-", end.year)
    ## ttsub = paste0(series.id, "\nhttp://data.bls.gov/timeseries/", series.id, "?years_option=specific_years&include_graphs=true&to_month=1&from_month=2")
    title(main=ttmain, cex.main= 0.9)
} 


## ################################################################################
## Now run everything
## ################################################################################


## Do all the downloads
output.from.downloads <- do.all.downloads(series.codes, first.n.series, max.series.per.request)

## Write the data to an Rdata file, to save the data, because
##  if one loses it, you may have to wait a day to download it again (or register
##  for the API).
## It will save new file for each day, and will write over one saved on the same day
rdatafilename <- file.path(homedir,paste("outputfromdownloads",tolower(format(Sys.time(),"%Y%m%d")),"Rdata",sep="."))
save(output.from.downloads, file=rdatafilename)


## Clean all the data series
list.of.clean.dfs <- map(.x= output.from.downloads$list.of.data.frames.for.series.over.all.requests, .f = clean.one.df.series)

graph.file.name <- file.path(homedir, paste("graphs.lfpr.",tolower(format(Sys.time(),"%Y%m%d")),".pdf",sep=""))
pdf(file = graph.file.name)

## Make the graphs
## Just do a loop for these:
for (i.series in seq_along(series.codes)){
    if (i.series %% 10== 0){
        cat("Working on series ", i.series, " out of ",length(series.codes),".\n", sep="")
    }
    plot.one.data.series(series.id = series.codes[i.series], series.name = series.titles[i.series], nicedf=list.of.clean.dfs[[i.series]], start.year = start.year, end.year = end.year)    
}
dev.off()

cat("Produced graph file called ", graph.file.name, ".\n", sep="")





