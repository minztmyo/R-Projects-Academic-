---
title: "Lab5"
author: "Astros"
date: "12/15/2020"
output: 
  html_document:
    keep_md: TRUE
---






```r
library(dplyr)
```


###Data import and aggregation



```r
inv.dat <- read.csv("data.investors16.csv", header = F, row.names = 1)
dim(inv.dat) #check dimensions
```

```
## [1] 20 16
```

```r
xbarA <- apply(inv.dat, MARGIN = 1, mean) #sample mean of A for each day
ssdA <- apply(inv.dat, MARGIN = 1, sd) #std dev of A for each day
nobsA <- apply(inv.dat, MARGIN = 1, length) #sample size of A for each day

mystatsA <- data.frame(xbarA, ssdA, nobsA)


inv.dat.tall <- read.csv("data.investors.tall.csv", header = T)
dim(inv.dat.tall) #check dimensions
```

```
## [1] 320   2
```

```r
xbarB <- inv.dat.tall %>% group_by(day) %>% summarise(mean = mean(x))
ssdB <- inv.dat.tall %>% group_by(day) %>% summarise(sd = sd(x))
nobsB <- inv.dat.tall %>% group_by(day) %>% summarise(count = length(x))

mystatsB <- data.frame(xbarB = xbarB$mean, ssdB = ssdB$sd,nobsB = nobsB$count, row.names = xbarB$day)
row.names(mystatsB) <- row.names(mystatsA)


#Confirm that mystatsA matches mystatsB

cbind(mystatsA,mystatsB)
```

```
##          xbarA      ssdA nobsA    xbarB      ssdB nobsB
## day1  4.985000 0.9485709    16 4.985000 0.9485709    16
## day2  4.549375 0.5177962    16 4.549375 0.5177962    16
## day3  4.996250 0.9250721    16 4.996250 0.9250721    16
## day4  4.988750 1.1546421    16 4.988750 1.1546421    16
## day5  5.625000 1.1636265    16 5.625000 1.1636265    16
## day6  5.295625 1.1175865    16 5.295625 1.1175865    16
## day7  4.964375 0.8995700    16 4.964375 0.8995700    16
## day8  4.698750 1.2368340    16 4.698750 1.2368340    16
## day9  4.700000 0.8996814    16 4.700000 0.8996814    16
## day10 5.201875 1.1718260    16 5.201875 1.1718260    16
## day11 4.738750 0.7650523    16 4.738750 0.7650523    16
## day12 4.495625 0.9407583    16 4.495625 0.9407583    16
## day13 4.721250 1.0841517    16 4.721250 1.0841517    16
## day14 5.164375 0.9385448    16 5.164375 0.9385448    16
## day15 4.736250 0.9035255    16 4.736250 0.9035255    16
## day16 5.034375 1.0375225    16 5.034375 1.0375225    16
## day17 5.281250 1.0005324    16 5.281250 1.0005324    16
## day18 4.662500 0.9161914    16 4.662500 0.9161914    16
## day19 4.678125 1.0637307    16 4.678125 1.0637307    16
## day20 4.900000 0.9023082    16 4.900000 0.9023082    16
```

```r
TF <- mystatsA == mystatsB 
TF
```

```
##       xbarA ssdA nobsA
## day1   TRUE TRUE  TRUE
## day2   TRUE TRUE  TRUE
## day3   TRUE TRUE  TRUE
## day4   TRUE TRUE  TRUE
## day5   TRUE TRUE  TRUE
## day6   TRUE TRUE  TRUE
## day7   TRUE TRUE  TRUE
## day8   TRUE TRUE  TRUE
## day9   TRUE TRUE  TRUE
## day10  TRUE TRUE  TRUE
## day11  TRUE TRUE  TRUE
## day12  TRUE TRUE  TRUE
## day13  TRUE TRUE  TRUE
## day14  TRUE TRUE  TRUE
## day15  TRUE TRUE  TRUE
## day16  TRUE TRUE  TRUE
## day17  TRUE TRUE  TRUE
## day18  TRUE TRUE  TRUE
## day19  TRUE TRUE  TRUE
## day20  TRUE TRUE  TRUE
```

```r
#create temporary data frames to make the dimnames equal to one another without affecting code further down; this is just to test that the values are the same using identical() function
tmp_mystatsA <- mystatsA
tmp_mystatsB <- mystatsB
dimnames(tmp_mystatsB) <- dimnames(tmp_mystatsA)
identical(tmp_mystatsA, tmp_mystatsB) 
```

```
## [1] TRUE
```

```r
# another method to show both the data frames are equal

identical(mystatsA$xbarA,mystatsB$xbarB)
```

```
## [1] TRUE
```

```r
identical(mystatsA$ssdA,mystatsB$ssdB)
```

```
## [1] TRUE
```

```r
identical(mystatsA$nobsA,mystatsB$nobsB)
```

```
## [1] TRUE
```


###Investor A
Run mystats = mystatsA to make a new data.frame. (Would it matter if you used mystatsB instead?)
We will use mystats for everything that follows. Each step below asks you to compute a 20-vector and
add it to mystats. The column dimension of mystats will go from 3 to 10.




```r
mystats = mystatsA
mystats
```

```
##          xbarA      ssdA nobsA
## day1  4.985000 0.9485709    16
## day2  4.549375 0.5177962    16
## day3  4.996250 0.9250721    16
## day4  4.988750 1.1546421    16
## day5  5.625000 1.1636265    16
## day6  5.295625 1.1175865    16
## day7  4.964375 0.8995700    16
## day8  4.698750 1.2368340    16
## day9  4.700000 0.8996814    16
## day10 5.201875 1.1718260    16
## day11 4.738750 0.7650523    16
## day12 4.495625 0.9407583    16
## day13 4.721250 1.0841517    16
## day14 5.164375 0.9385448    16
## day15 4.736250 0.9035255    16
## day16 5.034375 1.0375225    16
## day17 5.281250 1.0005324    16
## day18 4.662500 0.9161914    16
## day19 4.678125 1.0637307    16
## day20 4.900000 0.9023082    16
```


Compute the value of the test statistic for each of the 20 days.



```r
mystats <- mystats %>% mutate(tstat=(xbarA-5)/(ssdA/sqrt(nobsA)))
row.names(mystats) <- row.names(mystatsA)
mystats
```

```
##          xbarA      ssdA nobsA       tstat
## day1  4.985000 0.9485709    16 -0.06325305
## day2  4.549375 0.5177962    16 -3.48109924
## day3  4.996250 0.9250721    16 -0.01621495
## day4  4.988750 1.1546421    16 -0.03897312
## day5  5.625000 1.1636265    16  2.14845568
## day6  5.295625 1.1175865    16  1.05808365
## day7  4.964375 0.8995700    16 -0.15840901
## day8  4.698750 1.2368340    16 -0.97426171
## day9  4.700000 0.8996814    16 -1.33380546
## day10 5.201875 1.1718260    16  0.68909546
## day11 4.738750 0.7650523    16 -1.36591971
## day12 4.495625 0.9407583    16 -2.14454650
## day13 4.721250 1.0841517    16 -1.02845382
## day14 5.164375 0.9385448    16  0.70055264
## day15 4.736250 0.9035255    16 -1.16764828
## day16 5.034375 1.0375225    16  0.13252725
## day17 5.281250 1.0005324    16  1.12440142
## day18 4.662500 0.9161914    16 -1.47349125
## day19 4.678125 1.0637307    16 -1.21036278
## day20 4.900000 0.9023082    16 -0.44330753
```


Let tcrit1 be a 20-vector containing
the negative critical value, and let tcrit2 contain the positive critical value. Add these two vectors to
mystats.



```r
alpha <- 0.10
df <- nobsA-1
mystats <- mystats %>% mutate(tcrit1=qt(alpha/2,df))
mystats <- mystats %>% mutate(tcrit2=qt(alpha/2,df,lower.tail=F))
row.names(mystats) <- row.names(mystatsA)
mystats
```

```
##          xbarA      ssdA nobsA       tstat   tcrit1  tcrit2
## day1  4.985000 0.9485709    16 -0.06325305 -1.75305 1.75305
## day2  4.549375 0.5177962    16 -3.48109924 -1.75305 1.75305
## day3  4.996250 0.9250721    16 -0.01621495 -1.75305 1.75305
## day4  4.988750 1.1546421    16 -0.03897312 -1.75305 1.75305
## day5  5.625000 1.1636265    16  2.14845568 -1.75305 1.75305
## day6  5.295625 1.1175865    16  1.05808365 -1.75305 1.75305
## day7  4.964375 0.8995700    16 -0.15840901 -1.75305 1.75305
## day8  4.698750 1.2368340    16 -0.97426171 -1.75305 1.75305
## day9  4.700000 0.8996814    16 -1.33380546 -1.75305 1.75305
## day10 5.201875 1.1718260    16  0.68909546 -1.75305 1.75305
## day11 4.738750 0.7650523    16 -1.36591971 -1.75305 1.75305
## day12 4.495625 0.9407583    16 -2.14454650 -1.75305 1.75305
## day13 4.721250 1.0841517    16 -1.02845382 -1.75305 1.75305
## day14 5.164375 0.9385448    16  0.70055264 -1.75305 1.75305
## day15 4.736250 0.9035255    16 -1.16764828 -1.75305 1.75305
## day16 5.034375 1.0375225    16  0.13252725 -1.75305 1.75305
## day17 5.281250 1.0005324    16  1.12440142 -1.75305 1.75305
## day18 4.662500 0.9161914    16 -1.47349125 -1.75305 1.75305
## day19 4.678125 1.0637307    16 -1.21036278 -1.75305 1.75305
## day20 4.900000 0.9023082    16 -0.44330753 -1.75305 1.75305
```


Perform the hypothesis test. If tstat is outside the critical values, Investor A rejects H0. If tstat is
between the critical values, she accepts. Use this rule with ifelse() to create vector test1 populated
with the values ACCEPT or REJECT.



```r
mystats$test1 <- ifelse((mystats$tstat > mystats$tcrit1) & (mystats$tstat < mystats$tcrit2), "ACCEPT", "REJECT") 
mystats
```

```
##          xbarA      ssdA nobsA       tstat   tcrit1  tcrit2  test1
## day1  4.985000 0.9485709    16 -0.06325305 -1.75305 1.75305 ACCEPT
## day2  4.549375 0.5177962    16 -3.48109924 -1.75305 1.75305 REJECT
## day3  4.996250 0.9250721    16 -0.01621495 -1.75305 1.75305 ACCEPT
## day4  4.988750 1.1546421    16 -0.03897312 -1.75305 1.75305 ACCEPT
## day5  5.625000 1.1636265    16  2.14845568 -1.75305 1.75305 REJECT
## day6  5.295625 1.1175865    16  1.05808365 -1.75305 1.75305 ACCEPT
## day7  4.964375 0.8995700    16 -0.15840901 -1.75305 1.75305 ACCEPT
## day8  4.698750 1.2368340    16 -0.97426171 -1.75305 1.75305 ACCEPT
## day9  4.700000 0.8996814    16 -1.33380546 -1.75305 1.75305 ACCEPT
## day10 5.201875 1.1718260    16  0.68909546 -1.75305 1.75305 ACCEPT
## day11 4.738750 0.7650523    16 -1.36591971 -1.75305 1.75305 ACCEPT
## day12 4.495625 0.9407583    16 -2.14454650 -1.75305 1.75305 REJECT
## day13 4.721250 1.0841517    16 -1.02845382 -1.75305 1.75305 ACCEPT
## day14 5.164375 0.9385448    16  0.70055264 -1.75305 1.75305 ACCEPT
## day15 4.736250 0.9035255    16 -1.16764828 -1.75305 1.75305 ACCEPT
## day16 5.034375 1.0375225    16  0.13252725 -1.75305 1.75305 ACCEPT
## day17 5.281250 1.0005324    16  1.12440142 -1.75305 1.75305 ACCEPT
## day18 4.662500 0.9161914    16 -1.47349125 -1.75305 1.75305 ACCEPT
## day19 4.678125 1.0637307    16 -1.21036278 -1.75305 1.75305 ACCEPT
## day20 4.900000 0.9023082    16 -0.44330753 -1.75305 1.75305 ACCEPT
```


The p-value represents the probability of obtaining getting a value of the test statistic more contradictory
to H0 than tstat. Compute the vector of p-values like this:



```r
mystats <- mystats %>% mutate(pval=2*(1-pt(abs(mystats$tstat), nobsA - 1)))
row.names(mystats) <- row.names(mystatsA)
mystats
```

```
##          xbarA      ssdA nobsA       tstat   tcrit1  tcrit2  test1        pval
## day1  4.985000 0.9485709    16 -0.06325305 -1.75305 1.75305 ACCEPT 0.950400210
## day2  4.549375 0.5177962    16 -3.48109924 -1.75305 1.75305 REJECT 0.003351012
## day3  4.996250 0.9250721    16 -0.01621495 -1.75305 1.75305 ACCEPT 0.987276619
## day4  4.988750 1.1546421    16 -0.03897312 -1.75305 1.75305 ACCEPT 0.969425805
## day5  5.625000 1.1636265    16  2.14845568 -1.75305 1.75305 REJECT 0.048416339
## day6  5.295625 1.1175865    16  1.05808365 -1.75305 1.75305 ACCEPT 0.306760023
## day7  4.964375 0.8995700    16 -0.15840901 -1.75305 1.75305 ACCEPT 0.876247481
## day8  4.698750 1.2368340    16 -0.97426171 -1.75305 1.75305 ACCEPT 0.345377328
## day9  4.700000 0.8996814    16 -1.33380546 -1.75305 1.75305 ACCEPT 0.202167404
## day10 5.201875 1.1718260    16  0.68909546 -1.75305 1.75305 ACCEPT 0.501284182
## day11 4.738750 0.7650523    16 -1.36591971 -1.75305 1.75305 ACCEPT 0.192095671
## day12 4.495625 0.9407583    16 -2.14454650 -1.75305 1.75305 REJECT 0.048776215
## day13 4.721250 1.0841517    16 -1.02845382 -1.75305 1.75305 ACCEPT 0.320035950
## day14 5.164375 0.9385448    16  0.70055264 -1.75305 1.75305 ACCEPT 0.494306325
## day15 4.736250 0.9035255    16 -1.16764828 -1.75305 1.75305 ACCEPT 0.261174569
## day16 5.034375 1.0375225    16  0.13252725 -1.75305 1.75305 ACCEPT 0.896328720
## day17 5.281250 1.0005324    16  1.12440142 -1.75305 1.75305 ACCEPT 0.278514871
## day18 4.662500 0.9161914    16 -1.47349125 -1.75305 1.75305 ACCEPT 0.161293127
## day19 4.678125 1.0637307    16 -1.21036278 -1.75305 1.75305 ACCEPT 0.244867681
## day20 4.900000 0.9023082    16 -0.44330753 -1.75305 1.75305 ACCEPT 0.663870365
```


Use the p-value to perform the hypothesis test a different way. Compare the p-value to the significance
level α. If pval ≤ α, reject H0. Otherwise, accept H0. Use this rule with ifelse() to create vector
test2 populated with the values ACCEPT or REJECT. It should be identical to test1.



```r
mystats$test2 <- ifelse(mystats$pval <= 0.1, "REJECT", "ACCEPT")   #perform the hypothethis test using p-value
mystats 
```

```
##          xbarA      ssdA nobsA       tstat   tcrit1  tcrit2  test1        pval
## day1  4.985000 0.9485709    16 -0.06325305 -1.75305 1.75305 ACCEPT 0.950400210
## day2  4.549375 0.5177962    16 -3.48109924 -1.75305 1.75305 REJECT 0.003351012
## day3  4.996250 0.9250721    16 -0.01621495 -1.75305 1.75305 ACCEPT 0.987276619
## day4  4.988750 1.1546421    16 -0.03897312 -1.75305 1.75305 ACCEPT 0.969425805
## day5  5.625000 1.1636265    16  2.14845568 -1.75305 1.75305 REJECT 0.048416339
## day6  5.295625 1.1175865    16  1.05808365 -1.75305 1.75305 ACCEPT 0.306760023
## day7  4.964375 0.8995700    16 -0.15840901 -1.75305 1.75305 ACCEPT 0.876247481
## day8  4.698750 1.2368340    16 -0.97426171 -1.75305 1.75305 ACCEPT 0.345377328
## day9  4.700000 0.8996814    16 -1.33380546 -1.75305 1.75305 ACCEPT 0.202167404
## day10 5.201875 1.1718260    16  0.68909546 -1.75305 1.75305 ACCEPT 0.501284182
## day11 4.738750 0.7650523    16 -1.36591971 -1.75305 1.75305 ACCEPT 0.192095671
## day12 4.495625 0.9407583    16 -2.14454650 -1.75305 1.75305 REJECT 0.048776215
## day13 4.721250 1.0841517    16 -1.02845382 -1.75305 1.75305 ACCEPT 0.320035950
## day14 5.164375 0.9385448    16  0.70055264 -1.75305 1.75305 ACCEPT 0.494306325
## day15 4.736250 0.9035255    16 -1.16764828 -1.75305 1.75305 ACCEPT 0.261174569
## day16 5.034375 1.0375225    16  0.13252725 -1.75305 1.75305 ACCEPT 0.896328720
## day17 5.281250 1.0005324    16  1.12440142 -1.75305 1.75305 ACCEPT 0.278514871
## day18 4.662500 0.9161914    16 -1.47349125 -1.75305 1.75305 ACCEPT 0.161293127
## day19 4.678125 1.0637307    16 -1.21036278 -1.75305 1.75305 ACCEPT 0.244867681
## day20 4.900000 0.9023082    16 -0.44330753 -1.75305 1.75305 ACCEPT 0.663870365
##        test2
## day1  ACCEPT
## day2  REJECT
## day3  ACCEPT
## day4  ACCEPT
## day5  REJECT
## day6  ACCEPT
## day7  ACCEPT
## day8  ACCEPT
## day9  ACCEPT
## day10 ACCEPT
## day11 ACCEPT
## day12 REJECT
## day13 ACCEPT
## day14 ACCEPT
## day15 ACCEPT
## day16 ACCEPT
## day17 ACCEPT
## day18 ACCEPT
## day19 ACCEPT
## day20 ACCEPT
```


 Finally, create vector dec.A and populate it with the values GOLF or STARBUCKS. The data.frame
mystats should now be 20x10.



```r
mystats$dec.A <- ifelse(mystats$test1 == "ACCEPT", "GOLF", "STARBUCKS")
mystats
```

```
##          xbarA      ssdA nobsA       tstat   tcrit1  tcrit2  test1        pval
## day1  4.985000 0.9485709    16 -0.06325305 -1.75305 1.75305 ACCEPT 0.950400210
## day2  4.549375 0.5177962    16 -3.48109924 -1.75305 1.75305 REJECT 0.003351012
## day3  4.996250 0.9250721    16 -0.01621495 -1.75305 1.75305 ACCEPT 0.987276619
## day4  4.988750 1.1546421    16 -0.03897312 -1.75305 1.75305 ACCEPT 0.969425805
## day5  5.625000 1.1636265    16  2.14845568 -1.75305 1.75305 REJECT 0.048416339
## day6  5.295625 1.1175865    16  1.05808365 -1.75305 1.75305 ACCEPT 0.306760023
## day7  4.964375 0.8995700    16 -0.15840901 -1.75305 1.75305 ACCEPT 0.876247481
## day8  4.698750 1.2368340    16 -0.97426171 -1.75305 1.75305 ACCEPT 0.345377328
## day9  4.700000 0.8996814    16 -1.33380546 -1.75305 1.75305 ACCEPT 0.202167404
## day10 5.201875 1.1718260    16  0.68909546 -1.75305 1.75305 ACCEPT 0.501284182
## day11 4.738750 0.7650523    16 -1.36591971 -1.75305 1.75305 ACCEPT 0.192095671
## day12 4.495625 0.9407583    16 -2.14454650 -1.75305 1.75305 REJECT 0.048776215
## day13 4.721250 1.0841517    16 -1.02845382 -1.75305 1.75305 ACCEPT 0.320035950
## day14 5.164375 0.9385448    16  0.70055264 -1.75305 1.75305 ACCEPT 0.494306325
## day15 4.736250 0.9035255    16 -1.16764828 -1.75305 1.75305 ACCEPT 0.261174569
## day16 5.034375 1.0375225    16  0.13252725 -1.75305 1.75305 ACCEPT 0.896328720
## day17 5.281250 1.0005324    16  1.12440142 -1.75305 1.75305 ACCEPT 0.278514871
## day18 4.662500 0.9161914    16 -1.47349125 -1.75305 1.75305 ACCEPT 0.161293127
## day19 4.678125 1.0637307    16 -1.21036278 -1.75305 1.75305 ACCEPT 0.244867681
## day20 4.900000 0.9023082    16 -0.44330753 -1.75305 1.75305 ACCEPT 0.663870365
##        test2     dec.A
## day1  ACCEPT      GOLF
## day2  REJECT STARBUCKS
## day3  ACCEPT      GOLF
## day4  ACCEPT      GOLF
## day5  REJECT STARBUCKS
## day6  ACCEPT      GOLF
## day7  ACCEPT      GOLF
## day8  ACCEPT      GOLF
## day9  ACCEPT      GOLF
## day10 ACCEPT      GOLF
## day11 ACCEPT      GOLF
## day12 REJECT STARBUCKS
## day13 ACCEPT      GOLF
## day14 ACCEPT      GOLF
## day15 ACCEPT      GOLF
## day16 ACCEPT      GOLF
## day17 ACCEPT      GOLF
## day18 ACCEPT      GOLF
## day19 ACCEPT      GOLF
## day20 ACCEPT      GOLF
```



Investor B



```r
df <- nobsA-1

(t <- round(qt((1-0.10/2),df),2))
```

```
##  day1  day2 day3   day4  day5  day6  day7  day8  day9 day10 day11 day12 day13 
##  1.75  1.75  1.75  1.75  1.75  1.75  1.75  1.75  1.75  1.75  1.75  1.75  1.75 
## day14 day15 day16 day17 day18 day19 day20 
##  1.75  1.75  1.75  1.75  1.75  1.75  1.75
```

```r
(mystats$lim1 <- mystatsB$xbarB - t*(mystatsB$ssdB/(sqrt(mystatsB$nobsB))))
```

```
##  [1] 4.570000 4.322839 4.591531 4.483594 5.115913 4.806681 4.570813 4.157635
##  [9] 4.306389 4.689201 4.404040 4.084043 4.246934 4.753762 4.340958 4.580459
## [17] 4.843517 4.261666 4.212743 4.505240
```

```r
(mystats$lim2 <- mystatsB$xbarB + t*(mystatsB$ssdB/(sqrt(mystatsB$nobsB))))
```

```
##  [1] 5.400000 4.775911 5.400969 5.493906 6.134087 5.784569 5.357937 5.239865
##  [9] 5.093611 5.714549 5.073460 4.907207 5.195566 5.574988 5.131542 5.488291
## [17] 5.718983 5.063334 5.143507 5.294760
```

```r
mystats$test3 <- ifelse(mystats$lim2 < 5, "REJECT", "ACCEPT")

mystats$dec.B <- ifelse(mystats$test3 == "ACCEPT", "GOLF", "STARBUCKS")

mystats
```

```
##          xbarA      ssdA nobsA       tstat   tcrit1  tcrit2  test1        pval
## day1  4.985000 0.9485709    16 -0.06325305 -1.75305 1.75305 ACCEPT 0.950400210
## day2  4.549375 0.5177962    16 -3.48109924 -1.75305 1.75305 REJECT 0.003351012
## day3  4.996250 0.9250721    16 -0.01621495 -1.75305 1.75305 ACCEPT 0.987276619
## day4  4.988750 1.1546421    16 -0.03897312 -1.75305 1.75305 ACCEPT 0.969425805
## day5  5.625000 1.1636265    16  2.14845568 -1.75305 1.75305 REJECT 0.048416339
## day6  5.295625 1.1175865    16  1.05808365 -1.75305 1.75305 ACCEPT 0.306760023
## day7  4.964375 0.8995700    16 -0.15840901 -1.75305 1.75305 ACCEPT 0.876247481
## day8  4.698750 1.2368340    16 -0.97426171 -1.75305 1.75305 ACCEPT 0.345377328
## day9  4.700000 0.8996814    16 -1.33380546 -1.75305 1.75305 ACCEPT 0.202167404
## day10 5.201875 1.1718260    16  0.68909546 -1.75305 1.75305 ACCEPT 0.501284182
## day11 4.738750 0.7650523    16 -1.36591971 -1.75305 1.75305 ACCEPT 0.192095671
## day12 4.495625 0.9407583    16 -2.14454650 -1.75305 1.75305 REJECT 0.048776215
## day13 4.721250 1.0841517    16 -1.02845382 -1.75305 1.75305 ACCEPT 0.320035950
## day14 5.164375 0.9385448    16  0.70055264 -1.75305 1.75305 ACCEPT 0.494306325
## day15 4.736250 0.9035255    16 -1.16764828 -1.75305 1.75305 ACCEPT 0.261174569
## day16 5.034375 1.0375225    16  0.13252725 -1.75305 1.75305 ACCEPT 0.896328720
## day17 5.281250 1.0005324    16  1.12440142 -1.75305 1.75305 ACCEPT 0.278514871
## day18 4.662500 0.9161914    16 -1.47349125 -1.75305 1.75305 ACCEPT 0.161293127
## day19 4.678125 1.0637307    16 -1.21036278 -1.75305 1.75305 ACCEPT 0.244867681
## day20 4.900000 0.9023082    16 -0.44330753 -1.75305 1.75305 ACCEPT 0.663870365
##        test2     dec.A     lim1     lim2  test3     dec.B
## day1  ACCEPT      GOLF 4.570000 5.400000 ACCEPT      GOLF
## day2  REJECT STARBUCKS 4.322839 4.775911 REJECT STARBUCKS
## day3  ACCEPT      GOLF 4.591531 5.400969 ACCEPT      GOLF
## day4  ACCEPT      GOLF 4.483594 5.493906 ACCEPT      GOLF
## day5  REJECT STARBUCKS 5.115913 6.134087 ACCEPT      GOLF
## day6  ACCEPT      GOLF 4.806681 5.784569 ACCEPT      GOLF
## day7  ACCEPT      GOLF 4.570813 5.357937 ACCEPT      GOLF
## day8  ACCEPT      GOLF 4.157635 5.239865 ACCEPT      GOLF
## day9  ACCEPT      GOLF 4.306389 5.093611 ACCEPT      GOLF
## day10 ACCEPT      GOLF 4.689201 5.714549 ACCEPT      GOLF
## day11 ACCEPT      GOLF 4.404040 5.073460 ACCEPT      GOLF
## day12 REJECT STARBUCKS 4.084043 4.907207 REJECT STARBUCKS
## day13 ACCEPT      GOLF 4.246934 5.195566 ACCEPT      GOLF
## day14 ACCEPT      GOLF 4.753762 5.574988 ACCEPT      GOLF
## day15 ACCEPT      GOLF 4.340958 5.131542 ACCEPT      GOLF
## day16 ACCEPT      GOLF 4.580459 5.488291 ACCEPT      GOLF
## day17 ACCEPT      GOLF 4.843517 5.718983 ACCEPT      GOLF
## day18 ACCEPT      GOLF 4.261666 5.063334 ACCEPT      GOLF
## day19 ACCEPT      GOLF 4.212743 5.143507 ACCEPT      GOLF
## day20 ACCEPT      GOLF 4.505240 5.294760 ACCEPT      GOLF
```



####A level questions


Do you think A and B will ever get to know each other? Explain why or why not


```r
c(paste("The number of days that investigator A and B are in the same place out of 20 days is :" ))
```

```
## [1] "The number of days that investigator A and B are in the same place out of 20 days is :"
```

```r
sum(mystats$dec.A == mystats$dec.B)
```

```
## [1] 19
```


Assume the true value of µ is 5 on each of the 20 trading days. Use that information to add columns
grade.A and grade.B to mystats. Grade each investor’s decisions as CORRECT or INCORRECT.


```r
mystats$grade.A <- ifelse(mystats$test1 == "REJECT", "INCORRECT", "CORRECT") 

mystats$grade.B <- ifelse(5 > mystats$lim2 | 5 < mystats$lim1, "INCORRECT", "CORRECT")

mystats
```

```
##          xbarA      ssdA nobsA       tstat   tcrit1  tcrit2  test1        pval
## day1  4.985000 0.9485709    16 -0.06325305 -1.75305 1.75305 ACCEPT 0.950400210
## day2  4.549375 0.5177962    16 -3.48109924 -1.75305 1.75305 REJECT 0.003351012
## day3  4.996250 0.9250721    16 -0.01621495 -1.75305 1.75305 ACCEPT 0.987276619
## day4  4.988750 1.1546421    16 -0.03897312 -1.75305 1.75305 ACCEPT 0.969425805
## day5  5.625000 1.1636265    16  2.14845568 -1.75305 1.75305 REJECT 0.048416339
## day6  5.295625 1.1175865    16  1.05808365 -1.75305 1.75305 ACCEPT 0.306760023
## day7  4.964375 0.8995700    16 -0.15840901 -1.75305 1.75305 ACCEPT 0.876247481
## day8  4.698750 1.2368340    16 -0.97426171 -1.75305 1.75305 ACCEPT 0.345377328
## day9  4.700000 0.8996814    16 -1.33380546 -1.75305 1.75305 ACCEPT 0.202167404
## day10 5.201875 1.1718260    16  0.68909546 -1.75305 1.75305 ACCEPT 0.501284182
## day11 4.738750 0.7650523    16 -1.36591971 -1.75305 1.75305 ACCEPT 0.192095671
## day12 4.495625 0.9407583    16 -2.14454650 -1.75305 1.75305 REJECT 0.048776215
## day13 4.721250 1.0841517    16 -1.02845382 -1.75305 1.75305 ACCEPT 0.320035950
## day14 5.164375 0.9385448    16  0.70055264 -1.75305 1.75305 ACCEPT 0.494306325
## day15 4.736250 0.9035255    16 -1.16764828 -1.75305 1.75305 ACCEPT 0.261174569
## day16 5.034375 1.0375225    16  0.13252725 -1.75305 1.75305 ACCEPT 0.896328720
## day17 5.281250 1.0005324    16  1.12440142 -1.75305 1.75305 ACCEPT 0.278514871
## day18 4.662500 0.9161914    16 -1.47349125 -1.75305 1.75305 ACCEPT 0.161293127
## day19 4.678125 1.0637307    16 -1.21036278 -1.75305 1.75305 ACCEPT 0.244867681
## day20 4.900000 0.9023082    16 -0.44330753 -1.75305 1.75305 ACCEPT 0.663870365
##        test2     dec.A     lim1     lim2  test3     dec.B   grade.A   grade.B
## day1  ACCEPT      GOLF 4.570000 5.400000 ACCEPT      GOLF   CORRECT   CORRECT
## day2  REJECT STARBUCKS 4.322839 4.775911 REJECT STARBUCKS INCORRECT INCORRECT
## day3  ACCEPT      GOLF 4.591531 5.400969 ACCEPT      GOLF   CORRECT   CORRECT
## day4  ACCEPT      GOLF 4.483594 5.493906 ACCEPT      GOLF   CORRECT   CORRECT
## day5  REJECT STARBUCKS 5.115913 6.134087 ACCEPT      GOLF INCORRECT INCORRECT
## day6  ACCEPT      GOLF 4.806681 5.784569 ACCEPT      GOLF   CORRECT   CORRECT
## day7  ACCEPT      GOLF 4.570813 5.357937 ACCEPT      GOLF   CORRECT   CORRECT
## day8  ACCEPT      GOLF 4.157635 5.239865 ACCEPT      GOLF   CORRECT   CORRECT
## day9  ACCEPT      GOLF 4.306389 5.093611 ACCEPT      GOLF   CORRECT   CORRECT
## day10 ACCEPT      GOLF 4.689201 5.714549 ACCEPT      GOLF   CORRECT   CORRECT
## day11 ACCEPT      GOLF 4.404040 5.073460 ACCEPT      GOLF   CORRECT   CORRECT
## day12 REJECT STARBUCKS 4.084043 4.907207 REJECT STARBUCKS INCORRECT INCORRECT
## day13 ACCEPT      GOLF 4.246934 5.195566 ACCEPT      GOLF   CORRECT   CORRECT
## day14 ACCEPT      GOLF 4.753762 5.574988 ACCEPT      GOLF   CORRECT   CORRECT
## day15 ACCEPT      GOLF 4.340958 5.131542 ACCEPT      GOLF   CORRECT   CORRECT
## day16 ACCEPT      GOLF 4.580459 5.488291 ACCEPT      GOLF   CORRECT   CORRECT
## day17 ACCEPT      GOLF 4.843517 5.718983 ACCEPT      GOLF   CORRECT   CORRECT
## day18 ACCEPT      GOLF 4.261666 5.063334 ACCEPT      GOLF   CORRECT   CORRECT
## day19 ACCEPT      GOLF 4.212743 5.143507 ACCEPT      GOLF   CORRECT   CORRECT
## day20 ACCEPT      GOLF 4.505240 5.294760 ACCEPT      GOLF   CORRECT   CORRECT
```


Suppose Investor B uses α = .05 on each of the 20 days. Run his analyses again with this new α value
and make a new version of mystats to incorporate the new results



```r
#alpha = 0.05
df <- nobsA-1

(t1 <- round(qt((1-0.05/2),df),2))
```

```
##  day1  day2 day3   day4  day5  day6  day7  day8  day9 day10 day11 day12 day13 
##  2.13  2.13  2.13  2.13  2.13  2.13  2.13  2.13  2.13  2.13  2.13  2.13  2.13 
## day14 day15 day16 day17 day18 day19 day20 
##  2.13  2.13  2.13  2.13  2.13  2.13  2.13
```

```r
mystats$newlim1 <- mystatsB$xbarB - t1*(mystatsB$ssdB/(sqrt(mystatsB$nobsB)))

mystats$newlim2 <- mystatsB$xbarB + t1*(mystatsB$ssdB/(sqrt(mystatsB$nobsB)))

mystats$newtest <- ifelse(mystats$newlim2 >= 5, "ACCEPT", "REJECT")
mystats$dec.BNew <- ifelse(mystats$newtest == "ACCEPT", "GOLF", "STARBUCKS")
mystats$grade.BNew <- ifelse(5 > mystats$newlim2 | 5 < mystats$newlim1, "INCORRECT", "CORRECT")

c(paste("The chances of A and B getting to know each other is the same as before." ))
```

```
## [1] "The chances of A and B getting to know each other is the same as before."
```

```r
sum(mystats$dec.A == mystats$dec.BNew)
```

```
## [1] 19
```

```r
mystats
```

```
##          xbarA      ssdA nobsA       tstat   tcrit1  tcrit2  test1        pval
## day1  4.985000 0.9485709    16 -0.06325305 -1.75305 1.75305 ACCEPT 0.950400210
## day2  4.549375 0.5177962    16 -3.48109924 -1.75305 1.75305 REJECT 0.003351012
## day3  4.996250 0.9250721    16 -0.01621495 -1.75305 1.75305 ACCEPT 0.987276619
## day4  4.988750 1.1546421    16 -0.03897312 -1.75305 1.75305 ACCEPT 0.969425805
## day5  5.625000 1.1636265    16  2.14845568 -1.75305 1.75305 REJECT 0.048416339
## day6  5.295625 1.1175865    16  1.05808365 -1.75305 1.75305 ACCEPT 0.306760023
## day7  4.964375 0.8995700    16 -0.15840901 -1.75305 1.75305 ACCEPT 0.876247481
## day8  4.698750 1.2368340    16 -0.97426171 -1.75305 1.75305 ACCEPT 0.345377328
## day9  4.700000 0.8996814    16 -1.33380546 -1.75305 1.75305 ACCEPT 0.202167404
## day10 5.201875 1.1718260    16  0.68909546 -1.75305 1.75305 ACCEPT 0.501284182
## day11 4.738750 0.7650523    16 -1.36591971 -1.75305 1.75305 ACCEPT 0.192095671
## day12 4.495625 0.9407583    16 -2.14454650 -1.75305 1.75305 REJECT 0.048776215
## day13 4.721250 1.0841517    16 -1.02845382 -1.75305 1.75305 ACCEPT 0.320035950
## day14 5.164375 0.9385448    16  0.70055264 -1.75305 1.75305 ACCEPT 0.494306325
## day15 4.736250 0.9035255    16 -1.16764828 -1.75305 1.75305 ACCEPT 0.261174569
## day16 5.034375 1.0375225    16  0.13252725 -1.75305 1.75305 ACCEPT 0.896328720
## day17 5.281250 1.0005324    16  1.12440142 -1.75305 1.75305 ACCEPT 0.278514871
## day18 4.662500 0.9161914    16 -1.47349125 -1.75305 1.75305 ACCEPT 0.161293127
## day19 4.678125 1.0637307    16 -1.21036278 -1.75305 1.75305 ACCEPT 0.244867681
## day20 4.900000 0.9023082    16 -0.44330753 -1.75305 1.75305 ACCEPT 0.663870365
##        test2     dec.A     lim1     lim2  test3     dec.B   grade.A   grade.B
## day1  ACCEPT      GOLF 4.570000 5.400000 ACCEPT      GOLF   CORRECT   CORRECT
## day2  REJECT STARBUCKS 4.322839 4.775911 REJECT STARBUCKS INCORRECT INCORRECT
## day3  ACCEPT      GOLF 4.591531 5.400969 ACCEPT      GOLF   CORRECT   CORRECT
## day4  ACCEPT      GOLF 4.483594 5.493906 ACCEPT      GOLF   CORRECT   CORRECT
## day5  REJECT STARBUCKS 5.115913 6.134087 ACCEPT      GOLF INCORRECT INCORRECT
## day6  ACCEPT      GOLF 4.806681 5.784569 ACCEPT      GOLF   CORRECT   CORRECT
## day7  ACCEPT      GOLF 4.570813 5.357937 ACCEPT      GOLF   CORRECT   CORRECT
## day8  ACCEPT      GOLF 4.157635 5.239865 ACCEPT      GOLF   CORRECT   CORRECT
## day9  ACCEPT      GOLF 4.306389 5.093611 ACCEPT      GOLF   CORRECT   CORRECT
## day10 ACCEPT      GOLF 4.689201 5.714549 ACCEPT      GOLF   CORRECT   CORRECT
## day11 ACCEPT      GOLF 4.404040 5.073460 ACCEPT      GOLF   CORRECT   CORRECT
## day12 REJECT STARBUCKS 4.084043 4.907207 REJECT STARBUCKS INCORRECT INCORRECT
## day13 ACCEPT      GOLF 4.246934 5.195566 ACCEPT      GOLF   CORRECT   CORRECT
## day14 ACCEPT      GOLF 4.753762 5.574988 ACCEPT      GOLF   CORRECT   CORRECT
## day15 ACCEPT      GOLF 4.340958 5.131542 ACCEPT      GOLF   CORRECT   CORRECT
## day16 ACCEPT      GOLF 4.580459 5.488291 ACCEPT      GOLF   CORRECT   CORRECT
## day17 ACCEPT      GOLF 4.843517 5.718983 ACCEPT      GOLF   CORRECT   CORRECT
## day18 ACCEPT      GOLF 4.261666 5.063334 ACCEPT      GOLF   CORRECT   CORRECT
## day19 ACCEPT      GOLF 4.212743 5.143507 ACCEPT      GOLF   CORRECT   CORRECT
## day20 ACCEPT      GOLF 4.505240 5.294760 ACCEPT      GOLF   CORRECT   CORRECT
##        newlim1  newlim2 newtest  dec.BNew grade.BNew
## day1  4.479886 5.490114  ACCEPT      GOLF    CORRECT
## day2  4.273649 4.825101  REJECT STARBUCKS  INCORRECT
## day3  4.503649 5.488851  ACCEPT      GOLF    CORRECT
## day4  4.373903 5.603597  ACCEPT      GOLF    CORRECT
## day5  5.005369 6.244631  ACCEPT      GOLF  INCORRECT
## day6  4.700510 5.890740  ACCEPT      GOLF    CORRECT
## day7  4.485354 5.443396  ACCEPT      GOLF    CORRECT
## day8  4.040136 5.357364  ACCEPT      GOLF    CORRECT
## day9  4.220920 5.179080  ACCEPT      GOLF    CORRECT
## day10 4.577878 5.825872  ACCEPT      GOLF    CORRECT
## day11 4.331360 5.146140  ACCEPT      GOLF    CORRECT
## day12 3.994671 4.996579  REJECT STARBUCKS  INCORRECT
## day13 4.143939 5.298561  ACCEPT      GOLF    CORRECT
## day14 4.664600 5.664150  ACCEPT      GOLF    CORRECT
## day15 4.255123 5.217377  ACCEPT      GOLF    CORRECT
## day16 4.481894 5.586856  ACCEPT      GOLF    CORRECT
## day17 4.748467 5.814033  ACCEPT      GOLF    CORRECT
## day18 4.174628 5.150372  ACCEPT      GOLF    CORRECT
## day19 4.111688 5.244562  ACCEPT      GOLF    CORRECT
## day20 4.419521 5.380479  ACCEPT      GOLF    CORRECT
```





