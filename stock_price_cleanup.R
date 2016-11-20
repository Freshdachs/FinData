#

df <- read.csv2("~/Downloads/wiki_prices.csv", sep = ',')
df_red_cols <- df[c("ticker", "date" ,"adj_close")]
write.csv2(df_red_cols,"~/Downloads/prices_red.csv")


df_red_cols$date <- as.Date(df_red_cols$date);

min_date <- aggregate( date ~ ticker, df_red_cols, min)
max_date <- aggregate(date ~ ticker, df_red_cols, max)

get_rng<- function(hi="2016-11-11",lo="2011-11-11"){
  tickers <- max_date$ticker[ max_date$date >= as.Date(hi) & min_date$date <= as.Date(lo)]
  df_del_dates <- df_red_cols[df_red_cols$date <= as.Date(hi) & df_red_cols$date >= as.Date(lo),]
  df_del_dates$adj_close <- as.numeric(as.character(df_del_dates$adj_close))
  df_agg <- aggregate(adj_close ~ ticker,df_del_dates,c)
  row.names(df_agg)=df_agg$ticker
  df_agg[tickers,]
}
sandp <- c("MMM","ABT","ABBV","ACN","ATVI","AYI","ADBE","AAP","AES","AET","AFL","AMG","A","APD","AKAM","ALK","ALB","AGN","LNT","ALXN","ALLE","ADS","ALL","GOOGL","GOOG","MO","AMZN","AEE","AAL","AEP","AXP","AIG","AMT","AWK","AMP","ABC","AME","AMGN","APH","APC","ADI","ANTM","AON","APA","AIV","AAPL","AMAT","ADM","ARNC","AJG","AIZ","T","ADSK","ADP","AN","AZO","AVB","AVY","BHI","BLL","BAC","BK","BCR","BAX","BBT","BDX","BBBY","BRK","BBY","BIIB","BLK","HRB","BA","BWA","BXP","BSX","BMY","AVGO","BF","CHRW","CA","COG","CPB","COF","CAH","HSIC","KMX","CCL","CAT","CBG","CBS","CELG","CNC","CNP","CTL","CERN","CF","SCHW","CHTR","CHK","CVX","CMG","CB","CHD","CI","XEC","CINF","CTAS","CSCO","C","CFG","CTXS","CLX","CME","CMS","COH","KO","CTSH","CL","CMCSA","CMA","CAG","CXO","COP","ED","STZ","GLW","COST","COTY","CCI","CSRA","CSX","CMI","CVS","DHI","DHR","DRI","DVA","DE","DLPH","DAL","XRAY","DVN","DLR","DFS","DISCA","DISCK","DG","DLTR","D","DOV","DOW","DPS","DTE","DD","DUK","DNB","ETFC","EMN","ETN","EBAY","ECL","EIX","EW","EA","EMR","ENDP","ETR","EOG","EQT","EFX","EQIX","EQR","ESS","EL","ES","EXC","EXPE","EXPD","ESRX","EXR","XOM","FFIV","FB","FAST","FRT","FDX","FIS","FITB","FSLR","FE","FISV","FLIR","FLS","FLR","FMC","FTI","FL","F","FTV","FBHS","BEN","FCX","FTR","GPS","GRMN","GD","GE","GGP","GIS","GM","GPC","GILD","GPN","GS","GT","GWW","HAL","HBI","HOG","HAR","HRS","HIG","HAS","HCA","HCP","HP","HES","HPE","HOLX","HD","HON","HRL","HST","HPQ","HUM","HBAN","ITW","ILMN","IR","INTC","ICE","IBM","IP","IPG","IFF","INTU","ISRG","IVZ","IRM","JEC","JBHT","SJM","JNJ","JCI","JPM","JNPR","KSU","K","KEY","KMB","KIM","KMI","KLAC","KSS","KHC","KR","LB","LLL","LH","LRCX","LM","LEG","LEN","LVLT","LUK","LLY","LNC","LLTC","LKQ","LMT","L","LOW","LYB","MTB","MAC","M","MNK","MRO","MPC","MAR","MMC","MLM","MAS","MA","MAT","MKC","MCD","MCK","MJN","MDT","MRK","MET","MTD","KORS","MCHP","MU","MSFT","MHK","TAP","MDLZ","MON","MNST","MCO","MS","MOS","MSI","MUR","MYL","NDAQ","NOV","NAVI","NTAP","NFLX","NWL","NFX","NEM","NWSA","NWS","NEE","NLSN","NKE","NI","NBL","JWN","NSC","NTRS","NOC","NRG","NUE","NVDA","ORLY","OXY","OMC","OKE","ORCL","OI","PCAR","PH","PDCO","PAYX","PYPL","PNR","PBCT","PEP","PKI","PRGO","PFE","PCG","PM","PSX","PNW","PXD","PBI","PNC","RL","PPG","PPL","PX","PCLN","PFG","PG","PGR","PLD","PRU","PEG","PSA","PHM","PVH","QRVO","PWR","QCOM","DGX","RRC","RTN","O","RHT","REGN","RF","RSG","RAI","RHI","ROK","COL","ROP","ROST","RCL","R","CRM","SCG","SLB","SNI","STX","SEE","SRE","SHW","SIG","SPG","SWKS","SLG","SNA","SO","LUV","SWN","SE","SPGI","STJ","SWK","SPLS","SBUX","STT","SRCL","SYK","STI","SYMC","SYF","SYY","TROW","TGT","TEL","TGNA","TDC","TSO","TXN","TXT","COO","HSY","TRV","TMO","TIF","TWX","TJX","TMK","TSS","TSCO","TDG","RIG","TRIP","FOXA","FOX","TSN","UDR","ULTA","USB","UA","UA","UNP","UAL","UNH","UPS","URI","UTX","UHS","UNM","URBN","VFC","VLO","VAR","VTR","VRSN","VRSK","VZ","VRTX","VIAB","V","VNO","VMC","WMT","WBA","DIS","WM","WAT","WFC","HCN","WDC","WU","WRK","WY","WHR","WFM","WMB","WLTW","WEC","WYN","WYNN","XEL","XRX","XLNX","XL","XYL","YHOO","YUM","ZBH","ZION","ZTS")
#only keep available sandp data
sup <- sandp[sapply(sandp,function(i) i %in% tickers)]
srtd <- sort(min_date[sup,]$date)
q <- function(v,p) sort(v)[p*length(v)]
retrieve <- function(t,o) o[o$ticker==t,]
gen_ts_dates<- function(sup,del_dates = df_del_dates) lapply(sup,function(t) {cat(t);c(retrieve(t,aggregate(date ~ ticker,del_dates,c)), retrieve(t,aggregate(adj_close ~ticker,del_dates,c)))})
ts_dates <- gen_ts_dates(sup)
tst <- retrieve("MMM",tmp)

truncate <- function(d=df_del_dates,s=sup)d[d$ticker %in% s,]
trunc = truncate(df_del_dates,sup)

tname <- function(o,name){r=aggregate(o[,name] ~ ticker,o,c)[,name];names(r)<-o$ticker;r}
dates <- tname(trunc,"date")
adjs <- tname(trunc,"adj_close")
#selection
counts <- sort(sapply(dates,length))
sup_cnt <- names(counts[counts>2500]) #2500 days

trunc_cnt <- truncate(trunc,sup_cnt)
tpd <- aggregate(ticker ~ date,trunc_cnt,c)
tcnt <- sapply(tpd$ticker,length)
date_filter <- tpd[tcnt==max(tcnt),]$date

trunc_cnt_dt <- trunc_cnt[trunc_cnt$date %in% date_filter,]
timeseries <- aggregate(adj_close ~ ticker,trunc_cnt_dt,c)
dateseries <- aggregate(date ~ ticker,trunc_cnt_dt,c)
str(timeseries$adj_close)
tss <- as.data.frame(t(timeseries$adj_close),names=timeseries$ticker)
colnames(tss)<- timeseries$ticker
rownames(tss)<- dateseries$date[1,]
str(tss)
write.csv2(tss,"~/Documents/FinancialData/tss2.csv")
tss2<-read.csv2("~/Documents/FinancialData/tss2.csv",header=TRUE)
as.Date(tss2$X)
