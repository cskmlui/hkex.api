#' An API to Download Short Selling Turnover on Main Board
#'
#' The function is to retrieve short selling turnover on main board
#' @param no parameter
#' @keywords hkex.api
#' @export
#' @references https://www.hkex.com.hk
#' @examples data <- getLastShortSelling()
#' getLastShortSelling()
getLastShortSelling=function()
{ 
  library(XML)
  library(httr)
  library(RCurl)
  domain="https://www.hkex.com.hk/"
  webpage="eng/stat/smstat/ssturnover/ncms/ASHTMAIN.HTM"
  argument=""
  urlLink=paste(domain,webpage,argument,sep="")
  page= getURL(urlLink)
  hp=htmlParse(page)
  x <- xpathApply(hp,"//pre")
  s=x[[1]][[1]][[1]]
  xs=xmlValue(s)
  
 
  sDate=substr(xs, regexpr("TRADING DATE", xs)+15,regexpr("TRADING DATE", xs)+15+10) 
  sDate=as.Date(sDate,"%d %b %Y")
  skip=function(xs,n,  pattern)
  {
    for ( i in 1:n)
    {
      xs=substr(xs, regexpr(pattern, xs)+nchar(pattern), nchar(xs))
    }
    return (xs)
  }
  xs=skip(xs,7, "\r\n")
  
  f=function(x) !is.na(as.numeric(x))
  df=data.frame()
  for ( i in 1:2000)  
  {
    code=substr(xs, 1, 7)
    
    code=gsub(" ","", code)
    if (f(code)==FALSE) break
    cname=substr(xs, 10, 30)
    to.sh=substr(xs, 31, 41)
    to.val=substr(xs, 42, 56)
    df=rbind(df, data.frame(date=sDate, code,cname,to.sh, to.val))
    xs=skip(xs,1, "\r\n")
  }
  df$to.sh=gsub(",","", df$to.sh)
  df$to.val=gsub(",","", df$to.val)
  return(df)
}  
