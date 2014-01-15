require(RCurl)
require(XML)
require(rjson)
require(rCharts)

rblog <- htmlParse("http://www.r-bloggers.com")

#used fancy SelectorGadget recommended by Hadley Wickham
#http://selectorgadget.com/
#to get xpath
#not beautiful but works

blogroll <- getNodeSet(
  rblog,
  '//*[contains(concat( " ", @class, " " ), concat( " ", "blogroll", " " ))]//a'
)

#get href from blogroll to search with feedly
links <- sapply(blogroll,function(blog){
    xmlGetAttr(blog, "href")
  }
)

#use feedly api to get data on blogs in r-bloggers
#feedly api url
#http://developer.feedly.com/v3/search/
feedly = "http://cloud.feedly.com/v3/search/feeds/?q="

feedlyresults = lapply(
  links,
  function(x){
    searchurl = strsplit(x,"/")[[1]][3]
    feedlyresult = fromJSON(readLines(paste0(feedly,searchurl)))
    #$results[[1]]$subscribers
  }
)

#start to work on data.frame containing data we would like to analyze
feedlydata <- lapply(
  feedlyresults,
  function(x){
    tempdata <- if(length(x$results) > 0) {
      t(unlist(x[[1]][[1]])[c("title","subscribers","lastUpdated","velocity","score")])
    } else {
      NULL
    }
    return(tempdata)
  } 
)

#remove nulls
feedlydata <- feedlydata[!sapply(feedlydata,is.null)]

feedlydata.df <- do.call(
  rbind,
  lapply(
    feedlydata,
    function(x){
      tempdf <- as.data.frame(x,stringsAsFactors=F)
      colnames(tempdf) <- c("title","subscribers","lastUpdated","velocity","score")
      return(tempdf)
    }
  )
)

#make numeric data numeric
feedlydata.df[,2:5] <- sapply(feedlydata.df[,2:5],as.numeric)

d1 <- dPlot(
  y="title",
  x="subscribers",
  groups="title",
  data=feedlydata.df[order(feedlydata.df$subscribers,decreasing=TRUE)[1:50],],
  type="bar",
  height=600,
  width=800,
  bounds = list(x=200, y=30, width=600, height=500)
)
d1$xAxis(type="addMeasureAxis", outputFormat="#,")
d1$yAxis(type="addCategoryAxis")
d1
