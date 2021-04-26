---
title: "Lab4"
author: "Astros"
date: "11/15/2020"
output: 
  html_document:
    keep_md: TRUE
---






```r
# Question 1_4
color <- c(rep("red",13),rep("black",13),rep("red",13),rep("black",13))
suit <- c(rep("diamonds",13),rep("clubs",13),rep("hearts",13),rep("spades",13))
name <- c(rep(c("ace","2","3","4","5","6","7","8","9","10","Jack","Queen","King"),4))
rank <- c(rep(1:13,4))
value <- c(rep(c(1:10,10,10,10),4))
deck52 <- data.frame(cbind(name,suit,color,rank,value))
dim(deck52)
```

```
## [1] 52  5
```

```r
names(deck52) <- toupper(names(deck52))
deck52
```

```
##     NAME     SUIT COLOR RANK VALUE
## 1    ace diamonds   red    1     1
## 2      2 diamonds   red    2     2
## 3      3 diamonds   red    3     3
## 4      4 diamonds   red    4     4
## 5      5 diamonds   red    5     5
## 6      6 diamonds   red    6     6
## 7      7 diamonds   red    7     7
## 8      8 diamonds   red    8     8
## 9      9 diamonds   red    9     9
## 10    10 diamonds   red   10    10
## 11  Jack diamonds   red   11    10
## 12 Queen diamonds   red   12    10
## 13  King diamonds   red   13    10
## 14   ace    clubs black    1     1
## 15     2    clubs black    2     2
## 16     3    clubs black    3     3
## 17     4    clubs black    4     4
## 18     5    clubs black    5     5
## 19     6    clubs black    6     6
## 20     7    clubs black    7     7
## 21     8    clubs black    8     8
## 22     9    clubs black    9     9
## 23    10    clubs black   10    10
## 24  Jack    clubs black   11    10
## 25 Queen    clubs black   12    10
## 26  King    clubs black   13    10
## 27   ace   hearts   red    1     1
## 28     2   hearts   red    2     2
## 29     3   hearts   red    3     3
## 30     4   hearts   red    4     4
## 31     5   hearts   red    5     5
## 32     6   hearts   red    6     6
## 33     7   hearts   red    7     7
## 34     8   hearts   red    8     8
## 35     9   hearts   red    9     9
## 36    10   hearts   red   10    10
## 37  Jack   hearts   red   11    10
## 38 Queen   hearts   red   12    10
## 39  King   hearts   red   13    10
## 40   ace   spades black    1     1
## 41     2   spades black    2     2
## 42     3   spades black    3     3
## 43     4   spades black    4     4
## 44     5   spades black    5     5
## 45     6   spades black    6     6
## 46     7   spades black    7     7
## 47     8   spades black    8     8
## 48     9   spades black    9     9
## 49    10   spades black   10    10
## 50  Jack   spades black   11    10
## 51 Queen   spades black   12    10
## 52  King   spades black   13    10
```



```r
# Question 5
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
B <- filter(deck52,color == "black")
B
```

```
##     NAME   SUIT COLOR RANK VALUE
## 1    ace  clubs black    1     1
## 2      2  clubs black    2     2
## 3      3  clubs black    3     3
## 4      4  clubs black    4     4
## 5      5  clubs black    5     5
## 6      6  clubs black    6     6
## 7      7  clubs black    7     7
## 8      8  clubs black    8     8
## 9      9  clubs black    9     9
## 10    10  clubs black   10    10
## 11  Jack  clubs black   11    10
## 12 Queen  clubs black   12    10
## 13  King  clubs black   13    10
## 14   ace spades black    1     1
## 15     2 spades black    2     2
## 16     3 spades black    3     3
## 17     4 spades black    4     4
## 18     5 spades black    5     5
## 19     6 spades black    6     6
## 20     7 spades black    7     7
## 21     8 spades black    8     8
## 22     9 spades black    9     9
## 23    10 spades black   10    10
## 24  Jack spades black   11    10
## 25 Queen spades black   12    10
## 26  King spades black   13    10
```

```r
R <- filter(deck52,color == "red")
R
```

```
##     NAME     SUIT COLOR RANK VALUE
## 1    ace diamonds   red    1     1
## 2      2 diamonds   red    2     2
## 3      3 diamonds   red    3     3
## 4      4 diamonds   red    4     4
## 5      5 diamonds   red    5     5
## 6      6 diamonds   red    6     6
## 7      7 diamonds   red    7     7
## 8      8 diamonds   red    8     8
## 9      9 diamonds   red    9     9
## 10    10 diamonds   red   10    10
## 11  Jack diamonds   red   11    10
## 12 Queen diamonds   red   12    10
## 13  King diamonds   red   13    10
## 14   ace   hearts   red    1     1
## 15     2   hearts   red    2     2
## 16     3   hearts   red    3     3
## 17     4   hearts   red    4     4
## 18     5   hearts   red    5     5
## 19     6   hearts   red    6     6
## 20     7   hearts   red    7     7
## 21     8   hearts   red    8     8
## 22     9   hearts   red    9     9
## 23    10   hearts   red   10    10
## 24  Jack   hearts   red   11    10
## 25 Queen   hearts   red   12    10
## 26  King   hearts   red   13    10
```

```r
D <- filter(deck52,suit == "diamonds")
D
```

```
##     NAME     SUIT COLOR RANK VALUE
## 1    ace diamonds   red    1     1
## 2      2 diamonds   red    2     2
## 3      3 diamonds   red    3     3
## 4      4 diamonds   red    4     4
## 5      5 diamonds   red    5     5
## 6      6 diamonds   red    6     6
## 7      7 diamonds   red    7     7
## 8      8 diamonds   red    8     8
## 9      9 diamonds   red    9     9
## 10    10 diamonds   red   10    10
## 11  Jack diamonds   red   11    10
## 12 Queen diamonds   red   12    10
## 13  King diamonds   red   13    10
```

```r
H <- filter(deck52,suit == "hearts")
H
```

```
##     NAME   SUIT COLOR RANK VALUE
## 1    ace hearts   red    1     1
## 2      2 hearts   red    2     2
## 3      3 hearts   red    3     3
## 4      4 hearts   red    4     4
## 5      5 hearts   red    5     5
## 6      6 hearts   red    6     6
## 7      7 hearts   red    7     7
## 8      8 hearts   red    8     8
## 9      9 hearts   red    9     9
## 10    10 hearts   red   10    10
## 11  Jack hearts   red   11    10
## 12 Queen hearts   red   12    10
## 13  King hearts   red   13    10
```

```r
S <- filter(deck52,suit == "spades")
S
```

```
##     NAME   SUIT COLOR RANK VALUE
## 1    ace spades black    1     1
## 2      2 spades black    2     2
## 3      3 spades black    3     3
## 4      4 spades black    4     4
## 5      5 spades black    5     5
## 6      6 spades black    6     6
## 7      7 spades black    7     7
## 8      8 spades black    8     8
## 9      9 spades black    9     9
## 10    10 spades black   10    10
## 11  Jack spades black   11    10
## 12 Queen spades black   12    10
## 13  King spades black   13    10
```

```r
C <- filter(deck52,suit == "clubs")
C
```

```
##     NAME  SUIT COLOR RANK VALUE
## 1    ace clubs black    1     1
## 2      2 clubs black    2     2
## 3      3 clubs black    3     3
## 4      4 clubs black    4     4
## 5      5 clubs black    5     5
## 6      6 clubs black    6     6
## 7      7 clubs black    7     7
## 8      8 clubs black    8     8
## 9      9 clubs black    9     9
## 10    10 clubs black   10    10
## 11  Jack clubs black   11    10
## 12 Queen clubs black   12    10
## 13  King clubs black   13    10
```

```r
FACE <- filter(deck52,name == "Jack"|name == "Queen"|name == "King")
FACE
```

```
##     NAME     SUIT COLOR RANK VALUE
## 1   Jack diamonds   red   11    10
## 2  Queen diamonds   red   12    10
## 3   King diamonds   red   13    10
## 4   Jack    clubs black   11    10
## 5  Queen    clubs black   12    10
## 6   King    clubs black   13    10
## 7   Jack   hearts   red   11    10
## 8  Queen   hearts   red   12    10
## 9   King   hearts   red   13    10
## 10  Jack   spades black   11    10
## 11 Queen   spades black   12    10
## 12  King   spades black   13    10
```

```r
A <- filter(deck52,name == "ace")
A
```

```
##   NAME     SUIT COLOR RANK VALUE
## 1  ace diamonds   red    1     1
## 2  ace    clubs black    1     1
## 3  ace   hearts   red    1     1
## 4  ace   spades black    1     1
```

```r
ODD <- filter(deck52,value %% 2 != 0)
ODD
```

```
##    NAME     SUIT COLOR RANK VALUE
## 1   ace diamonds   red    1     1
## 2     3 diamonds   red    3     3
## 3     5 diamonds   red    5     5
## 4     7 diamonds   red    7     7
## 5     9 diamonds   red    9     9
## 6   ace    clubs black    1     1
## 7     3    clubs black    3     3
## 8     5    clubs black    5     5
## 9     7    clubs black    7     7
## 10    9    clubs black    9     9
## 11  ace   hearts   red    1     1
## 12    3   hearts   red    3     3
## 13    5   hearts   red    5     5
## 14    7   hearts   red    7     7
## 15    9   hearts   red    9     9
## 16  ace   spades black    1     1
## 17    3   spades black    3     3
## 18    5   spades black    5     5
## 19    7   spades black    7     7
## 20    9   spades black    9     9
```

```r
LOW <- filter(deck52,rank < 3)
LOW
```

```
##   NAME     SUIT COLOR RANK VALUE
## 1  ace diamonds   red    1     1
## 2    2 diamonds   red    2     2
## 3  ace    clubs black    1     1
## 4    2    clubs black    2     2
## 5  ace   hearts   red    1     1
## 6    2   hearts   red    2     2
## 7  ace   spades black    1     1
## 8    2   spades black    2     2
```





```r
# Question 7
p_H <- nrow(H)/nrow(deck52)
p_H
```

```
## [1] 0.25
```

```r
# Question 7
P_FACE <- nrow(FACE)/nrow(deck52)
P_FACE
```

```
## [1] 0.2307692
```

```r
# Question 8
P_ODD <- nrow(ODD)/nrow(deck52)
P_ODD
```

```
## [1] 0.3846154
```

```r
# Question 9
EVEN <- filter(deck52,value %% 2 == 0)
p_EVEN <- nrow(EVEN)/nrow(deck52)
p_EVEN
```

```
## [1] 0.6153846
```

```r
# Question 10
ODD_C <- filter(deck52,value %% 2 == 0)
p_ODD_C <- nrow(ODD_C)/nrow(deck52)
p_ODD_C
```

```
## [1] 0.6153846
```

```r
# Question 11
p_D <- nrow(D)/nrow(deck52)
p_D
```

```
## [1] 0.25
```

```r
# Question 12
p_DandR <- nrow(filter(deck52,suit == "diamonds" & color == "red"))/nrow(deck52)
p_DandR
```

```
## [1] 0.25
```

```r
# Question 13
p_R <- nrow(R)/nrow(deck52)
p_DgivenR <- p_DandR/p_R
p_DgivenR
```

```
## [1] 0.5
```

```r
# Question 14
p_DorR <- p_D + p_R - p_DandR
p_DorR
```

```
## [1] 0.5
```

```r
# Question 15 
FACEandEVEN <- filter(deck52,name == "Jack"|name == "Queen"|name == "King" & value %% 2 == 0)
p_FgivenEVEN <- (nrow(FACEandEVEN)/nrow(deck52))/p_EVEN
p_FgivenEVEN
```

```
## [1] 0.375
```

```r
# Question 16
question <- c(6:15)
probability <- c("P(H)","P(FACE)","P(ODD)","P(EVEN)","P(ODD^C)","P(D)","P(D & R)","P(D|R)","P(D U R)","P(F|EVEN)")
answer <- c(p_H,P_FACE,P_ODD,p_EVEN,p_ODD_C,p_D,p_DandR,p_DgivenR,p_DorR,p_FgivenEVEN)
myprobs <- data.frame(question = question,probability = probability,answer = answer)
myprobs
```

```
##    question probability    answer
## 1         6        P(H) 0.2500000
## 2         7     P(FACE) 0.2307692
## 3         8      P(ODD) 0.3846154
## 4         9     P(EVEN) 0.6153846
## 5        10    P(ODD^C) 0.6153846
## 6        11        P(D) 0.2500000
## 7        12    P(D & R) 0.2500000
## 8        13      P(D|R) 0.5000000
## 9        14    P(D U R) 0.5000000
## 10       15   P(F|EVEN) 0.3750000
```

You can also embed plots, for example:


```r
# Question 17
color_new <- c(rep(c("black","red"),26))
suit_new <- c(rep(c("clubs","diamonds","spades","hearts"),13))
name_new <- c(rep("ace",4),rep("2",4),rep("3",4),rep("4",4),rep("5",4),rep("6",4),rep("7",4),rep("8",4),rep("9",4),rep("10",4),rep("Jack",4),rep("Queen",4),rep("King",4))
rank_new <- c(rep(1,4),rep(2,4),rep(3,4),rep(4,4),rep(5,4),rep(6,4),rep(7,4),rep(8,4),rep(9,4),rep(10,4),rep(11,4),rep(12,4),rep(13,4))
value_new <- c(rep(1,4),rep(2,4),rep(3,4),rep(4,4),rep(5,4),rep(6,4),rep(7,4),rep(8,4),rep(9,4),rep(10,16))
deck52_new <- data.frame(cbind(name_new,suit_new,color_new,rank_new,value_new))
deck52_new
```

```
##    name_new suit_new color_new rank_new value_new
## 1       ace    clubs     black        1         1
## 2       ace diamonds       red        1         1
## 3       ace   spades     black        1         1
## 4       ace   hearts       red        1         1
## 5         2    clubs     black        2         2
## 6         2 diamonds       red        2         2
## 7         2   spades     black        2         2
## 8         2   hearts       red        2         2
## 9         3    clubs     black        3         3
## 10        3 diamonds       red        3         3
## 11        3   spades     black        3         3
## 12        3   hearts       red        3         3
## 13        4    clubs     black        4         4
## 14        4 diamonds       red        4         4
## 15        4   spades     black        4         4
## 16        4   hearts       red        4         4
## 17        5    clubs     black        5         5
## 18        5 diamonds       red        5         5
## 19        5   spades     black        5         5
## 20        5   hearts       red        5         5
## 21        6    clubs     black        6         6
## 22        6 diamonds       red        6         6
## 23        6   spades     black        6         6
## 24        6   hearts       red        6         6
## 25        7    clubs     black        7         7
## 26        7 diamonds       red        7         7
## 27        7   spades     black        7         7
## 28        7   hearts       red        7         7
## 29        8    clubs     black        8         8
## 30        8 diamonds       red        8         8
## 31        8   spades     black        8         8
## 32        8   hearts       red        8         8
## 33        9    clubs     black        9         9
## 34        9 diamonds       red        9         9
## 35        9   spades     black        9         9
## 36        9   hearts       red        9         9
## 37       10    clubs     black       10        10
## 38       10 diamonds       red       10        10
## 39       10   spades     black       10        10
## 40       10   hearts       red       10        10
## 41     Jack    clubs     black       11        10
## 42     Jack diamonds       red       11        10
## 43     Jack   spades     black       11        10
## 44     Jack   hearts       red       11        10
## 45    Queen    clubs     black       12        10
## 46    Queen diamonds       red       12        10
## 47    Queen   spades     black       12        10
## 48    Queen   hearts       red       12        10
## 49     King    clubs     black       13        10
## 50     King diamonds       red       13        10
## 51     King   spades     black       13        10
## 52     King   hearts       red       13        10
```

```r
names(deck52_new) <- toupper(names(deck52_new))
deck52_new
```

```
##    NAME_NEW SUIT_NEW COLOR_NEW RANK_NEW VALUE_NEW
## 1       ace    clubs     black        1         1
## 2       ace diamonds       red        1         1
## 3       ace   spades     black        1         1
## 4       ace   hearts       red        1         1
## 5         2    clubs     black        2         2
## 6         2 diamonds       red        2         2
## 7         2   spades     black        2         2
## 8         2   hearts       red        2         2
## 9         3    clubs     black        3         3
## 10        3 diamonds       red        3         3
## 11        3   spades     black        3         3
## 12        3   hearts       red        3         3
## 13        4    clubs     black        4         4
## 14        4 diamonds       red        4         4
## 15        4   spades     black        4         4
## 16        4   hearts       red        4         4
## 17        5    clubs     black        5         5
## 18        5 diamonds       red        5         5
## 19        5   spades     black        5         5
## 20        5   hearts       red        5         5
## 21        6    clubs     black        6         6
## 22        6 diamonds       red        6         6
## 23        6   spades     black        6         6
## 24        6   hearts       red        6         6
## 25        7    clubs     black        7         7
## 26        7 diamonds       red        7         7
## 27        7   spades     black        7         7
## 28        7   hearts       red        7         7
## 29        8    clubs     black        8         8
## 30        8 diamonds       red        8         8
## 31        8   spades     black        8         8
## 32        8   hearts       red        8         8
## 33        9    clubs     black        9         9
## 34        9 diamonds       red        9         9
## 35        9   spades     black        9         9
## 36        9   hearts       red        9         9
## 37       10    clubs     black       10        10
## 38       10 diamonds       red       10        10
## 39       10   spades     black       10        10
## 40       10   hearts       red       10        10
## 41     Jack    clubs     black       11        10
## 42     Jack diamonds       red       11        10
## 43     Jack   spades     black       11        10
## 44     Jack   hearts       red       11        10
## 45    Queen    clubs     black       12        10
## 46    Queen diamonds       red       12        10
## 47    Queen   spades     black       12        10
## 48    Queen   hearts       red       12        10
## 49     King    clubs     black       13        10
## 50     King diamonds       red       13        10
## 51     King   spades     black       13        10
## 52     King   hearts       red       13        10
```

```r
# Question 18
deck52_new %>% mutate(LONGNAME = paste(name_new,"of",suit_new))
```

```
##    NAME_NEW SUIT_NEW COLOR_NEW RANK_NEW VALUE_NEW          LONGNAME
## 1       ace    clubs     black        1         1      ace of clubs
## 2       ace diamonds       red        1         1   ace of diamonds
## 3       ace   spades     black        1         1     ace of spades
## 4       ace   hearts       red        1         1     ace of hearts
## 5         2    clubs     black        2         2        2 of clubs
## 6         2 diamonds       red        2         2     2 of diamonds
## 7         2   spades     black        2         2       2 of spades
## 8         2   hearts       red        2         2       2 of hearts
## 9         3    clubs     black        3         3        3 of clubs
## 10        3 diamonds       red        3         3     3 of diamonds
## 11        3   spades     black        3         3       3 of spades
## 12        3   hearts       red        3         3       3 of hearts
## 13        4    clubs     black        4         4        4 of clubs
## 14        4 diamonds       red        4         4     4 of diamonds
## 15        4   spades     black        4         4       4 of spades
## 16        4   hearts       red        4         4       4 of hearts
## 17        5    clubs     black        5         5        5 of clubs
## 18        5 diamonds       red        5         5     5 of diamonds
## 19        5   spades     black        5         5       5 of spades
## 20        5   hearts       red        5         5       5 of hearts
## 21        6    clubs     black        6         6        6 of clubs
## 22        6 diamonds       red        6         6     6 of diamonds
## 23        6   spades     black        6         6       6 of spades
## 24        6   hearts       red        6         6       6 of hearts
## 25        7    clubs     black        7         7        7 of clubs
## 26        7 diamonds       red        7         7     7 of diamonds
## 27        7   spades     black        7         7       7 of spades
## 28        7   hearts       red        7         7       7 of hearts
## 29        8    clubs     black        8         8        8 of clubs
## 30        8 diamonds       red        8         8     8 of diamonds
## 31        8   spades     black        8         8       8 of spades
## 32        8   hearts       red        8         8       8 of hearts
## 33        9    clubs     black        9         9        9 of clubs
## 34        9 diamonds       red        9         9     9 of diamonds
## 35        9   spades     black        9         9       9 of spades
## 36        9   hearts       red        9         9       9 of hearts
## 37       10    clubs     black       10        10       10 of clubs
## 38       10 diamonds       red       10        10    10 of diamonds
## 39       10   spades     black       10        10      10 of spades
## 40       10   hearts       red       10        10      10 of hearts
## 41     Jack    clubs     black       11        10     Jack of clubs
## 42     Jack diamonds       red       11        10  Jack of diamonds
## 43     Jack   spades     black       11        10    Jack of spades
## 44     Jack   hearts       red       11        10    Jack of hearts
## 45    Queen    clubs     black       12        10    Queen of clubs
## 46    Queen diamonds       red       12        10 Queen of diamonds
## 47    Queen   spades     black       12        10   Queen of spades
## 48    Queen   hearts       red       12        10   Queen of hearts
## 49     King    clubs     black       13        10     King of clubs
## 50     King diamonds       red       13        10  King of diamonds
## 51     King   spades     black       13        10    King of spades
## 52     King   hearts       red       13        10    King of hearts
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
