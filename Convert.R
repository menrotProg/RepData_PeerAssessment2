decode.units <- function(d) {
  switch(d, H = 100, K = 1000, M = 1e+06, B = 1e+09, `0` = 1, `1` = 10, `2` = 100, 
         `3` = 1000, `4` = 10000, `5` = 1e+05, `6` = 1e+06, `7` = 1e+07, `8` = 1e+08, 
         `9` = 1e+09, 0)
}

stormdata$DAMAGE <- stormdata$PROPDMG * sapply(stormdata$PROPDMGEXP, decode.units) + 
  stormdata$CROPDMG * sapply(stormdata$CROPDMGEXP, decode.units)
data.damage <- aggregate(DAMAGE ~ EVTYPE, stormdata, sum, na.rm = T)

convExp = function(expo) {
  switch(as.character(expo),
         "-" = 1,
         "?" = 1,
         "+" = 1,
         "0" = 1,
         "1" = 10,
         "2" = 100,
         "3" = 1000,
         "4" = 10000,
         "5" = 100000,
         "6" = 1000000,
         "7" = 10000000,
         "8" = 100000000,
         "9" = 1000000000,
         "h" = 100,
         "H" = 100,
         "k" = 1000,
         "K" = 1000,
         "m" = 1000000,
         "M" = 1000000,
         "b" = 1000000000,
         "B" = 1000000000,
         1)
}

