# install.packages("quantmod")
library("quantmod")
spyTick <- c('A',	'AAL',	'AAP',	'AAPL',	'ABBV',	'ABC',	'ABT',	'ACN',	'ADBE',	'ADI',	'ADM',
             'ADP',	'ADS',	'ADSK',	'AEE',	'AEP',	'AES',	'AET',	'AFL',	'AGN',	'AIG',	'AIV',
             'AIZ',	'AJG',	'AKAM',	'ALB',	'ALGN',	'ALK',	'ALL',	'ALLE',	'ALXN',	'AMAT',	'AMD',
             'AME',	'AMG',	'AMGN',	'AMP',	'AMT',	'AMZN',	'ANDV',	'ANSS',	'ANTM',	'AON',	'AOS',
             'APA',	'APC',	'APD',	'APH',	'ARE',	'ARNC',	'ATVI',	'AVB',	'AVGO',	'AVY',	'AWK',	'AXP',
             'AYI',	'AZO',	'BA',	'BAC',	'BAX',	'BBT',	'BBY',	'BCR',	'BDX',	'BEN',	'BF-B',	'BHF',
             'BHGE',	'BIIB',	'BK',	'BLK',	'BLL',	'BMY',	'BRK-B',	'BSX',	'BWA',	'BXP',	'C',
             'CA',	'CAG',	'CAH',	'CAT',	'CB',	'CBG',	'CBOE',	'CBS',	'CCI',	'CCL',	'CELG',	'CERN',
             'CF',	'CFG',	'CHD',	'CHK',	'CHRW',	'CHTR',	'CI',	'CINF',	'CL',	'CLX',	'CMA',	'CMCSA',	'CME',
             'CMG',	'CMI',	'CMS',	'CNC',	'CNP',	'COF',	'COG',	'COH',	'COL',	'COO',	'COP',	'COST',
             'COTY',	'CPB',	'CRM',	'CSCO',	'CSRA',	'CSX',	'CTAS',	'CTL',	'CTSH',	'CTXS',	'CVS',	'CVX',
             'CXO',	'D',	'DAL',	'DE',	'DFS',	'DG',	'DGX',	'DHI',	'DHR',	'DIS',	'DISCA',	'DISCK',
             'DISH',	'DLPH',	'DLR',	'DLTR',	'DOV',	'DPS',	'DRE',	'DRI',	'DTE',	'DUK',	'DVA',	'DVN',
             'DWDP',	'DXC',	'EA',	'EBAY',	'ECL',	'ED',	'EFX',	'EIX',	'EL',	'EMN',	'EMR',	'EOG',	'EQIX',	
             'EQR',	'EQT',	'ES',	'ESRX',	'ESS',	'ETFC',	'ETN',	'ETR',	'EVHC',	'EW',	'EXC',	'EXPD',	'EXPE',
             'EXR',	'F',	'FAST',	'FB',	'FBHS',	'FCX',	'FDX',	'FE',	'FFIV',	'FIS',	'FISV',	'FITB',	'FL',	'FLIR',
             'FLR',	'FLS',	'FMC',	'FOX',	'FOXA',	'FRT',	'FTI',	'FTV',	'GD',	'GE',	'GGP',	'GILD',	'GIS',
             'GLW',	'GM',	'GOOG',	'GOOGL',	'GPC',	'GPN',	'GPS',	'GRMN',	'GS',	'GT',	'GWW',	'HAL',	'HAS',
             'HBAN',	'HBI',	'HCA',	'HCN',	'HCP',	'HD',	'HES',	'HIG',	'HLT',	'HOG',	'HOLX',	'HON',	'HP',
             'HPE',	'HPQ',	'HRB',	'HRL',	'HRS',	'HSIC',	'HST',	'HSY',	'HUM',	'IBM',	'ICE',	'IDXX',
             'IFF',	'ILMN',	'INCY',	'INFO',	'INTC',	'INTU',	'IP',	'IPG',	'IR',	'IRM',	'ISRG',	'IT',	'ITW',
             'IVZ',	'JBHT',	'JCI',	'JEC',	'JNJ',	'JNPR',	'JPM',	'JWN',	'K',	'KEY',	'KHC',	'KIM',	'KLAC',
             'KMB',	'KMI',	'KMX',	'KO',	'KORS',	'KR',	'KSS',	'KSU',	'L',	'LB',	'LEG',	'LEN',	'LH',	'LKQ',
             'LLL',	'LLY',	'LMT',	'LNC',	'LNT',	'LOW',	'LRCX',	'LUK',	'LUV',	'LVLT',	'LYB',	'M',	'MA',
             'MAA',	'MAC',	'MAR',	'MAS',	'MAT',	'MCD',	'MCHP',	'MCK',	'MCO',	'MDLZ',	'MDT',	'MET',	'MGM',
             'MHK',	'MKC',	'MLM',	'MMC',	'MMM',	'MNST',	'MO',	'MON',	'MOS',	'MPC',	'MRK',	'MRO',	'MS',	'MSFT',
             'MSI',	'MTB',	'MTD',	'MU',	'MYL',	'NAVI',	'NBL',	'NDAQ',	'NEE',	'NEM',	'NFLX',	'NFX',	'NI',	'NKE',
             'NLSN',	'NOC',	'NOV',	'NRG',	'NSC',	'NTAP',	'NTRS',	'NUE',	'NVDA',	'NWL',	'NWS',	'NWSA',	'O',
             'OKE',	'OMC',	'ORCL',	'ORLY',	'OXY',	'PAYX',	'PBCT',	'PCAR',	'PCG',	'PCLN',	'PDCO',	'PEG',	'PEP',
             'PFE',	'PFG',	'PG',	'PGR',	'PH',	'PHM',	'PKG',	'PKI',	'PLD',	'PM',	'PNC',	'PNR',	'PNW',	'PPG',
             'PPL',	'PRGO',	'PRU',	'PSA',	'PSX',	'PVH',	'PWR',	'PX',	'PXD',	'PYPL',	'Q',	'QCOM',	'QRVO',	'RCL',
             'RE',	'REG',	'REGN',	'RF',	'RHI',	'RHT',	'RJF',	'RL',	'RMD',	'ROK',	'ROP',	'ROST',	'RRC',	'RSG',
             'RTN',	'SBAC',	'SBUX',	'SCG',	'SCHW',	'SEE',	'SHW',	'SIG',	'SJM',	'SLB',	'SLG',	'SNA',	'SNI',	'SNPS',
             'SO',	'SPG',	'SPGI',	'SPLS',	'SRCL',	'SRE',	'STI',	'STT',	'STX',	'STZ',	'SWK',	'SWKS',	'SYF',	'SYK',
             'SYMC',	'SYY',	'T',	'TAP',	'TDG',	'TEL',	'TGT',	'TIF',	'TJX',	'TMK',	'TMO',	'TRIP',	'TROW',	'TRV',
             'TSCO',	'TSN',	'TSS',	'TWX',	'TXN',	'TXT',	'UA',	'UAA',	'UAL',	'UDR',	'UHS',	'ULTA',	'UNH',	'UNM',
             'UNP',	'UPS',	'URI',	'USB',	'UTX',	'V',	'VAR',	'VFC',	'VIAB',	'VLO',	'VMC',	'VNO',	'VRSK',	'VRSN',
             'VRTX',	'VTR',	'VZ',	'WAT',	'WBA',	'WDC',	'WEC',	'WFC',	'WHR',	'WLTW',	'WM',	'WMB',	'WMT',	'WRK',
             'WU',	'WY',	'WYN',	'WYNN',	'XEC',	'XEL',	'XL',	'XLNX',	'XOM',	'XRAY',	'XRX',	'XYL',	'YUM',	'ZBH',
             'ZION',	'ZTS')



out <- list()
for(i in 1:length(spyTick)){
  print(i)
  out[[i]] <- getSymbols(Symbols = spyTick[i], auto.assign = F, from = "2007-01-01")
  if(dim(out[[i]])[1] < min.dim){
    min.dim = dim(out[[i]])[1]
  }
}

lenDat <- sapply(out, function(x){dim(x)[1]})

returns <- lapply(out[-c(which(lenDat < 2691))], function(z){
  x <- as.matrix(z)
  x[2:2691, 4] / x[1:2690, 4] -1 })

ret <- do.call("cbind", returns)
colnames(ret) <- spyTick[-c(which(lenDat < 2691))]
na.cols <- unique(which(is.na(ret), arr.ind = T)[, 2])
ret.cleaned <- ret[, -na.cols]
ret.final <- scale(ret.cleaned, scale = F)

write.table(ret.final, "returnsSPY.csv", sep = ",")
write.table(ret.final, "returnsSPY_no_names.csv", sep = ",", row.names = F, col.names = F)

cs <- .8
ncores <- 6
cl <- makeCluster(ncores)
registerDoParallel(cl)
md <- 4
output3 <- highDLingam::findGraphMulti(ret.final, maxInDegree = md, cutOffScaling = cs, degree = 4,
                                       verbose = T)

sim.size <- 10
rec <- rep(0, sim.size)
for(i in 1:sim.size){
  ret.subs <- ret.clean[sample(1:dim(ret.clean)[1], 250), output3$topOrder]
  output4 <- highDLingam::findGraphMulti(ret.subs, maxInDegree = md, cutOffScaling = cs, degree = 4,
                                         verbose = F)
  rec[i] <- cor(output4$topOrder, 1:dim(ret.clean)[2], method = "kendall")
}
rec



#########################################################################################
DJIA <- c('AAPL',	'AXP',	'BA',	'CAT',	'CSCO',	'CVX',	'DIS',	'GE',	'GS',
          'HD',	'IBM',	'INTC',	'JNJ',	'JPM',	'KO',	'MCD',	'MMM',	'MRK',	'MSFT',
          'NKE',	'PFE',	'PG',	'TRV',	'UNH',	'UTX',	'V',	'VZ',	'WMT',	'XOM')


out <- list()
for(i in 1:length(DJIA)){
  print(i)
  out[[i]] <- getSymbols(Symbols = DJIA[i], auto.assign = F, from = "2009-01-01")
  if(dim(out[[i]])[1] < min.dim){
    min.dim = dim(out[[i]])[1]
  }
}

lenDat <- sapply(out, function(x){dim(x)[1]})
min(lenDat)
max(lenDat)




returns <- lapply(out, function(z){
  x <- as.matrix(z)
  x[2:lenDat[1], 4] / x[1:(lenDat[1] - 1), 4] -1 })

ret <- do.call("cbind", returns)
colnames(ret) <- DJIA
ret.clean <- scale(ret, scale = F)
# write.table(ret.clean, "djia_no_names.csv", sep = ",", row.names = F, col.names = F)


cs <- .8
cl <- makeCluster(ncores)
registerDoParallel(cl)
md <- 6
output3 <- highDLingam::findGraphMulti(ret.clean, maxInDegree = md, cutOffScaling = cs, degree = 4,
                                       verbose = T)

output3$topOrder

sim.size <- 100
rec <- rep(0, sim.size)
for(i in 1:sim.size){
  ret.subs <- ret.clean[sample(1:dim(ret.clean)[1], 250), output3$topOrder]
  output4 <- highDLingam::findGraphMulti(ret.subs, maxInDegree = md, cutOffScaling = cs, degree = 4,
                                         verbose = F)
  rec[i] <- cor(output4$topOrder, 1:dim(ret.clean)[2], method = "kendall")
}
rec


output.ling <- pcalg::lingam(ret.clean)




output.ling$Bpruned

dling <- unlist(read.csv("../data/dling_res.csv", header = F))
pwling <- unlist(read.csv("../data/pwling_res.csv", header = F))

cor(dling, pwling, method = "kendall")


