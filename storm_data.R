# USAGE
# source("storm_data.R")
# read_raw()
# data = read_tmp("data.RData")

require(data.table)

read_raw <- function () {
    file_path = "data/repdata-data-StormData.csv";
    data = read.csv(file_path,sep = ",")   
}

read_tmp <- function (var) {
    cat ("Read from tmp:", var ,"\n")
    
    return (readRDS( paste('data','tmp',var,sep = "/")));
}

cleanup <- function(data) {
    # lowercase EVTYPE
    ev = tolower(data$EVTYPE)
    
    # remove special sign (non word)
    ev = gsub("\\W", " ", ev, perl = TRUE)
    
    # single space, use "sub" for single, "gsub" for global
    ev = gsub("\\s+", " ", ev, perl = TRUE)
    
    # trim
    ev = sub("^\\s+", "", ev, perl = TRUE)
    ev = sub("\\s+$", "", ev, perl = TRUE)
    
    # plural
    ev = sub("s$", "", ev, perl = TRUE)
    
    data$EVTYPE = ev
    
    # lowercase damage exp
    data$PROPDMGEXP = cleanup_exp(data$PROPDMGEXP)
    data$CROPDMGEXP = cleanup_exp(data$CROPDMGEXP)
    
    # calculate property damage PROPDMG * 10 ^ PROPDMGEXP
    data$PROPDMG = data$PROPDMG * 10^(data$PROPDMGEXP)
    
    # calculate crop damage CROPDMG * 10 ^ CROPDMGEXP
    data$CROPDMG = data$CROPDMG * 10^(data$CROPDMGEXP)
    
    return(data)
}

cleanup_exp <- function(ex) {
    
    ex = gsub("K|k", 3, ex, perl = TRUE)
    ex = gsub("M|m", 6, ex, perl = TRUE)
    ex = gsub("B|b", 9, ex, perl = TRUE)
    ex = gsub("\\D", 0, ex, perl = TRUE)
    
    ex = as.numeric(ex)
    ex[is.na(ex)] = 0 # set NA to 0
    
    ex
}

filter_result <- function(agg, limit=10) {
    idx = agg[2] > 0
    agg = agg[idx,]
    agg = agg[order(-agg[[2]]), ]
    
    total_damage = sum(agg[[2]])
    agg['damage'] = agg[2] / total_damage * 100
    
    head(agg, limit)
}

