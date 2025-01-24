Data_Science_in_Psychology_FinalProject
================
Bhuvanesh Wadhwani
2022-10-27

## Calling packages

``` r
packages_to_use<- c("ROCR", "tidyverse", "caret", "glmnet", "rpart", "rpart.plot", "vip", "pdp", "randomForest", "gbm", "GGally", "cowplot", "dplyr", "DALEX", "DALEXtra", "lime", "localModel", "gridExtra")

for(i in packages_to_use){
  if( ! i %in% rownames(installed.packages())  ) {
    print(paste(i, "not installed; installing now:\n") )
    install.packages(i)
  }
  
  require(i, character.only = TRUE)
}
```

    ## Loading required package: gridExtra

    ## 
    ## Attaching package: 'gridExtra'

    ## The following object is masked from 'package:randomForest':
    ## 
    ##     combine

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

# Part 1: Data Exploration

## Taking in data

``` r
sact.dat <- read.csv("sac.csv")

#combining variables
sact.dat <- sact.dat %>%
  mutate(paredu = (Medu + Fedu) ) %>% 
  mutate(grades = (G1 + G2 + G3)/3)

head(sact.dat)
```

    ##   school sex age address famsize Pstatus Medu Fedu     Mjob     Fjob     reason
    ## 1     GP   F  18       U     GT3       A    4    4  at_home  teacher     course
    ## 2     GP   F  17       U     GT3       T    1    1  at_home    other     course
    ## 3     GP   F  15       U     LE3       T    1    1  at_home    other      other
    ## 4     GP   F  15       U     GT3       T    4    2   health services       home
    ## 5     GP   F  16       U     GT3       T    3    3    other    other       home
    ## 6     GP   M  16       U     LE3       T    4    3 services    other reputation
    ##   guardian traveltime studytime failures schoolsup famsup paid activities
    ## 1   mother          2         2        0       yes     no   no         no
    ## 2   father          1         2        0        no    yes   no         no
    ## 3   mother          1         2        0       yes     no   no         no
    ## 4   mother          1         3        0        no    yes   no        yes
    ## 5   father          1         2        0        no    yes   no         no
    ## 6   mother          1         2        0        no    yes   no        yes
    ##   nursery higher internet romantic famrel freetime goout Dalc Walc health
    ## 1     yes    yes       no       no      4        3     4    1    1      3
    ## 2      no    yes      yes       no      5        3     3    1    1      3
    ## 3     yes    yes      yes       no      4        3     2    2    3      3
    ## 4     yes    yes      yes      yes      3        2     2    1    1      5
    ## 5     yes    yes       no       no      4        3     2    1    2      5
    ## 6     yes    yes      yes       no      5        4     2    1    2      5
    ##   absences G1 G2 G3 paredu    grades
    ## 1        4  0 11 11      8  7.333333
    ## 2        2  9 11 11      2 10.333333
    ## 3        6 12 13 12      2 12.333333
    ## 4        0 14 14 14      6 14.000000
    ## 5        0 11 13 13      6 12.333333
    ## 6        6 12 12 13      7 12.333333

``` r
psych::describe(sact.dat)
```

    ##             vars   n  mean   sd median trimmed  mad   min   max range  skew
    ## school*        1 649  1.35 0.48   1.00    1.31 0.00  1.00  2.00  1.00  0.64
    ## sex*           2 649  1.41 0.49   1.00    1.39 0.00  1.00  2.00  1.00  0.37
    ## age            3 649 16.74 1.22  17.00   16.70 1.48 15.00 22.00  7.00  0.41
    ## address*       4 649  1.70 0.46   2.00    1.74 0.00  1.00  2.00  1.00 -0.85
    ## famsize*       5 649  1.30 0.46   1.00    1.25 0.00  1.00  2.00  1.00  0.89
    ## Pstatus*       6 649  1.88 0.33   2.00    1.97 0.00  1.00  2.00  1.00 -2.29
    ## Medu           7 649  2.51 1.13   2.00    2.53 1.48  0.00  4.00  4.00 -0.03
    ## Fedu           8 649  2.31 1.10   2.00    2.27 1.48  0.00  4.00  4.00  0.21
    ## Mjob*          9 649  2.94 1.25   3.00    2.93 1.48  1.00  5.00  4.00 -0.19
    ## Fjob*         10 649  3.22 0.86   3.00    3.29 0.00  1.00  5.00  4.00 -0.53
    ## reason*       11 649  2.11 1.19   2.00    2.02 1.48  1.00  4.00  3.00  0.56
    ## guardian*     12 649  1.83 0.52   2.00    1.83 0.00  1.00  3.00  2.00 -0.20
    ## traveltime    13 649  1.57 0.75   1.00    1.43 0.00  1.00  4.00  3.00  1.24
    ## studytime     14 649  1.93 0.83   2.00    1.85 1.48  1.00  4.00  3.00  0.70
    ## failures      15 649  0.22 0.59   0.00    0.07 0.00  0.00  3.00  3.00  3.08
    ## schoolsup*    16 649  1.10 0.31   1.00    1.01 0.00  1.00  2.00  1.00  2.57
    ## famsup*       17 649  1.61 0.49   2.00    1.64 0.00  1.00  2.00  1.00 -0.46
    ## paid*         18 649  1.06 0.24   1.00    1.00 0.00  1.00  2.00  1.00  3.69
    ## activities*   19 649  1.49 0.50   1.00    1.48 0.00  1.00  2.00  1.00  0.06
    ## nursery*      20 649  1.80 0.40   2.00    1.88 0.00  1.00  2.00  1.00 -1.52
    ## higher*       21 649  1.89 0.31   2.00    1.99 0.00  1.00  2.00  1.00 -2.55
    ## internet*     22 649  1.77 0.42   2.00    1.83 0.00  1.00  2.00  1.00 -1.26
    ## romantic*     23 649  1.37 0.48   1.00    1.34 0.00  1.00  2.00  1.00  0.55
    ## famrel        24 649  3.93 0.96   4.00    4.05 1.48  1.00  5.00  4.00 -1.10
    ## freetime      25 649  3.18 1.05   3.00    3.19 1.48  1.00  5.00  4.00 -0.18
    ## goout         26 649  3.18 1.18   3.00    3.20 1.48  1.00  5.00  4.00 -0.01
    ## Dalc          27 649  1.50 0.92   1.00    1.28 0.00  1.00  5.00  4.00  2.13
    ## Walc          28 649  2.28 1.28   2.00    2.14 1.48  1.00  5.00  4.00  0.63
    ## health        29 649  3.54 1.45   4.00    3.67 1.48  1.00  5.00  4.00 -0.50
    ## absences      30 649  3.66 4.64   2.00    2.80 2.97  0.00 32.00 32.00  2.01
    ## G1            31 649 11.40 2.75  11.00   11.38 2.97  0.00 19.00 19.00  0.00
    ## G2            32 649 11.57 2.91  11.00   11.56 2.97  0.00 19.00 19.00 -0.36
    ## G3            33 649 11.91 3.23  12.00   12.04 2.97  0.00 19.00 19.00 -0.91
    ## paredu        34 649  4.82 2.03   5.00    4.79 2.97  0.00  8.00  8.00  0.12
    ## grades        35 649 11.63 2.83  11.67   11.64 2.47  1.33 18.67 17.33 -0.23
    ##             kurtosis   se
    ## school*        -1.60 0.02
    ## sex*           -1.87 0.02
    ## age             0.05 0.05
    ## address*       -1.28 0.02
    ## famsize*       -1.21 0.02
    ## Pstatus*        3.23 0.01
    ## Medu           -1.27 0.04
    ## Fedu           -1.12 0.04
    ## Mjob*          -0.83 0.05
    ## Fjob*           1.17 0.03
    ## reason*        -1.25 0.05
    ## guardian*       0.17 0.02
    ## traveltime      1.08 0.03
    ## studytime       0.02 0.03
    ## failures        9.70 0.02
    ## schoolsup*      4.64 0.01
    ## famsup*        -1.79 0.02
    ## paid*          11.66 0.01
    ## activities*    -2.00 0.02
    ## nursery*        0.31 0.02
    ## higher*         4.50 0.01
    ## internet*      -0.41 0.02
    ## romantic*      -1.71 0.02
    ## famrel          1.32 0.04
    ## freetime       -0.41 0.04
    ## goout          -0.87 0.05
    ## Dalc            4.28 0.04
    ## Walc           -0.78 0.05
    ## health         -1.13 0.06
    ## absences        5.70 0.18
    ## G1              0.02 0.11
    ## G2              1.63 0.11
    ## G3              2.66 0.13
    ## paredu         -1.14 0.08
    ## grades          0.58 0.11

``` r
#selecting columns of interest based on low skewness
sapply(sact.dat, var)
```

    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion
    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion
    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion
    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion
    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion
    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion
    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion
    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion
    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion
    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion
    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion
    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion
    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion
    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion
    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion
    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion
    ## Warning in FUN(X[[i]], ...): NAs introduced by coercion

    ##     school        sex        age    address    famsize    Pstatus       Medu 
    ##         NA         NA  1.4838593         NA         NA         NA  1.2872082 
    ##       Fedu       Mjob       Fjob     reason   guardian traveltime  studytime 
    ##  1.2098480         NA         NA         NA         NA  0.5604919  0.6880861 
    ##   failures  schoolsup     famsup       paid activities    nursery     higher 
    ##  0.3519279         NA         NA         NA         NA         NA         NA 
    ##   internet   romantic     famrel   freetime      goout       Dalc       Walc 
    ##         NA         NA  0.9133948  1.1047956  1.3824260  0.8553187  1.6496319 
    ##     health   absences         G1         G2         G3     paredu     grades 
    ##  2.0916652 21.5366423  7.5364806  8.4892903 10.4371398  4.1130657  8.0279305

``` r
sacf.dat <- select(sact.dat, Mjob, Fjob, guardian, famsup, activities, romantic, freetime, health, goout, paredu, sex, age, reason, grades)

print(head(sacf.dat))
```

    ##       Mjob     Fjob guardian famsup activities romantic freetime health goout
    ## 1  at_home  teacher   mother     no         no       no        3      3     4
    ## 2  at_home    other   father    yes         no       no        3      3     3
    ## 3  at_home    other   mother     no         no       no        3      3     2
    ## 4   health services   mother    yes        yes      yes        2      5     2
    ## 5    other    other   father    yes         no       no        3      5     2
    ## 6 services    other   mother    yes        yes       no        4      5     2
    ##   paredu sex age     reason    grades
    ## 1      8   F  18     course  7.333333
    ## 2      2   F  17     course 10.333333
    ## 3      2   F  15      other 12.333333
    ## 4      6   F  15       home 14.000000
    ## 5      6   F  16       home 12.333333
    ## 6      7   M  16 reputation 12.333333

``` r
print(str(sacf.dat))
```

    ## 'data.frame':    649 obs. of  14 variables:
    ##  $ Mjob      : chr  "at_home" "at_home" "at_home" "health" ...
    ##  $ Fjob      : chr  "teacher" "other" "other" "services" ...
    ##  $ guardian  : chr  "mother" "father" "mother" "mother" ...
    ##  $ famsup    : chr  "no" "yes" "no" "yes" ...
    ##  $ activities: chr  "no" "no" "no" "yes" ...
    ##  $ romantic  : chr  "no" "no" "no" "yes" ...
    ##  $ freetime  : int  3 3 3 2 3 4 4 1 2 5 ...
    ##  $ health    : int  3 3 3 5 5 5 3 1 1 5 ...
    ##  $ goout     : int  4 3 2 2 2 2 4 4 2 1 ...
    ##  $ paredu    : int  8 2 2 6 6 7 4 8 5 7 ...
    ##  $ sex       : chr  "F" "F" "F" "F" ...
    ##  $ age       : int  18 17 15 15 16 16 16 17 15 15 ...
    ##  $ reason    : chr  "course" "course" "other" "home" ...
    ##  $ grades    : num  7.33 10.33 12.33 14 12.33 ...
    ## NULL

\##–Details about the dataset: Student Alcohol Consumption–##

\#school - student’s school (binary: ‘GP’ - Gabriel Pereira or ‘MS’ -
Mousinho da Silveira)

\#sex - student’s sex (binary: ‘F’ - female or ‘M’ - male)

\#age - student’s age (numeric: from 15 to 22)

\#address - student’s home address type (binary: ‘U’ - urban or ‘R’ -
rural)

\#famsize - family size (binary: ‘LE3’ - less or equal to 3 or ‘GT3’ -
greater than 3)

\#Pstatus - parent’s cohabitation status (binary: ‘T’ - living together
or ‘A’ - apart)

\#Medu - mother’s education (numeric: 0 - none, 1 - primary education
(4th grade), 2 – 5th to 9th grade, 3 – secondary education or 4 – higher
education)

\#Fedu - father’s education (numeric: 0 - none, 1 - primary education
(4th grade), 2 – 5th to 9th grade, 3 – secondary education or 4 – higher
education)

\#Mjob - mother’s job (nominal: ‘teacher’, ‘health’ care related, civil
‘services’ (e.g. administrative or police), ‘at_home’ or ‘other’)

\#Fjob - father’s job (nominal: ‘teacher’, ‘health’ care related, civil
‘services’ (e.g. administrative or police), ‘at_home’ or ‘other’)

\#reason - reason to choose this school (nominal: close to ‘home’,
school ‘reputation’, ‘course’ preference or ‘other’)

\#guardian - student’s guardian (nominal: ‘mother’, ‘father’ or ‘other’)

\#traveltime - home to school travel time (numeric: 1 - \<15 min., 2 -
15 to 30 min., 3 - 30 min. to 1 hour, or 4 - \>1 hour)

\#studytime - weekly study time (numeric: 1 - \<2 hours, 2 - 2 to 5
hours, 3 - 5 to 10 hours, or 4 - \>10 hours)

\#failures - number of past class failures (numeric: n if 1\<=n\<3, else
4)

\#schoolsup - extra educational support (binary: yes or no)

\#famsup - family educational support (binary: yes or no)

\#paid - extra paid classes within the course subject (Math or
Portuguese) (binary: yes or no)

\#activities - extra-curricular activities (binary: yes or no)

\#nursery - attended nursery school (binary: yes or no)

\#higher - wants to take higher education (binary: yes or no)

\#internet - Internet access at home (binary: yes or no)

\#romantic - with a romantic relationship (binary: yes or no)

\#famrel - quality of family relationships (numeric: from 1 - very bad
to 5 - excellent)

\#freetime - free time after school (numeric: from 1 - very low to 5 -
very high)

\#goout - going out with friends (numeric: from 1 - very low to 5 - very
high)

\#Dalc - workday alcohol consumption (numeric: from 1 - very low to 5 -
very high)

\#Walc - weekend alcohol consumption (numeric: from 1 - very low to 5 -
very high)

\#health - current health status (numeric: from 1 - very bad to 5 - very
good)

\#absences - number of school absences (numeric: from 0 to 93)

\#G1 - first period grade (numeric: from 0 to 20)

\#G2 - second period grade (numeric: from 0 to 20)

\#G3 - final grade (numeric: from 0 to 20, output target)

\##added variables:

\#paredu - parent’s education out of 8 (higher = higher combined
education levels of both parents)

\#grade - mean grade (numeric: from 0 to 20)

## Factoring variables in the dataset

``` r
#sex
sacf.dat$sex <- factor(sacf.dat$sex,
                       levels = c("F", "M"),
                       labels = c("Female", "Male"))

#Mjob
sacf.dat$Mjob <- factor(sacf.dat$Mjob,
                        levels = c("teacher", "health","services","at_home","other"),
                        labels = c("teacher", "health","services","at_home","other"))

#Fjob
sacf.dat$Fjob <- factor(sacf.dat$Fjob,
                        levels = c("teacher", "health","services","at_home","other"),
                        labels = c("teacher", "health","services","at_home","other"))

#guardian
sacf.dat$guardian <- factor(sacf.dat$guardian,
                            levels = c("mother", "father","other"),
                            labels = c("mother", "father","other"))


#famsup
sacf.dat$famsup <- factor(sacf.dat$famsup,
                          levels = c("no", "yes"),
                          labels = c("no", "yes"))


#activities
sacf.dat$activities <- factor(sacf.dat$activities,
                              levels = c("no", "yes"),
                              labels = c("no", "yes"))

#romantic
sacf.dat$romantic <- factor(sacf.dat$romantic,
                            levels = c("no", "yes"),
                            labels = c("no", "yes"))

#freetime
sacf.dat$freetime <- factor(sacf.dat$freetime,
                            levels = c("1","2","3","4","5"), 
                            labels = c("1","2","3","4","5"))

#goout
sacf.dat$goout <- factor(sacf.dat$goout,
                         levels = c("1","2","3","4", "5"), 
                         labels = c("1","2","3","4", "5"))

#reason
sacf.dat$reason <- factor(sacf.dat$reason,
                          levels = c("home", "reputation","course","other"),
                          labels = c("home", "reputation","course","other"))

#health
sacf.dat$health <- factor(sacf.dat$health,
                       levels = c("1","2","3","4","5"), 
                       labels = c("1","2","3","4","5"))
```

``` r
head(sacf.dat)
```

    ##       Mjob     Fjob guardian famsup activities romantic freetime health goout
    ## 1  at_home  teacher   mother     no         no       no        3      3     4
    ## 2  at_home    other   father    yes         no       no        3      3     3
    ## 3  at_home    other   mother     no         no       no        3      3     2
    ## 4   health services   mother    yes        yes      yes        2      5     2
    ## 5    other    other   father    yes         no       no        3      5     2
    ## 6 services    other   mother    yes        yes       no        4      5     2
    ##   paredu    sex age     reason    grades
    ## 1      8 Female  18     course  7.333333
    ## 2      2 Female  17     course 10.333333
    ## 3      2 Female  15      other 12.333333
    ## 4      6 Female  15       home 14.000000
    ## 5      6 Female  16       home 12.333333
    ## 6      7   Male  16 reputation 12.333333

``` r
str(sacf.dat)
```

    ## 'data.frame':    649 obs. of  14 variables:
    ##  $ Mjob      : Factor w/ 5 levels "teacher","health",..: 4 4 4 2 5 3 5 5 3 5 ...
    ##  $ Fjob      : Factor w/ 5 levels "teacher","health",..: 1 5 5 3 5 5 5 1 5 5 ...
    ##  $ guardian  : Factor w/ 3 levels "mother","father",..: 1 2 1 1 2 1 1 1 1 1 ...
    ##  $ famsup    : Factor w/ 2 levels "no","yes": 1 2 1 2 2 2 1 2 2 2 ...
    ##  $ activities: Factor w/ 2 levels "no","yes": 1 1 1 2 1 2 1 1 1 2 ...
    ##  $ romantic  : Factor w/ 2 levels "no","yes": 1 1 1 2 1 1 1 1 1 1 ...
    ##  $ freetime  : Factor w/ 5 levels "1","2","3","4",..: 3 3 3 2 3 4 4 1 2 5 ...
    ##  $ health    : Factor w/ 5 levels "1","2","3","4",..: 3 3 3 5 5 5 3 1 1 5 ...
    ##  $ goout     : Factor w/ 5 levels "1","2","3","4",..: 4 3 2 2 2 2 4 4 2 1 ...
    ##  $ paredu    : int  8 2 2 6 6 7 4 8 5 7 ...
    ##  $ sex       : Factor w/ 2 levels "Female","Male": 1 1 1 1 1 2 2 1 2 2 ...
    ##  $ age       : int  18 17 15 15 16 16 16 17 15 15 ...
    ##  $ reason    : Factor w/ 4 levels "home","reputation",..: 3 3 4 1 1 2 1 1 1 1 ...
    ##  $ grades    : num  7.33 10.33 12.33 14 12.33 ...

``` r
summary(sacf.dat)
```

    ##        Mjob           Fjob       guardian   famsup    activities romantic 
    ##  teacher : 72   teacher : 36   mother:455   no :251   no :334    no :410  
    ##  health  : 48   health  : 23   father:153   yes:398   yes:315    yes:239  
    ##  services:136   services:181   other : 41                                 
    ##  at_home :135   at_home : 42                                              
    ##  other   :258   other   :367                                              
    ##                                                                           
    ##  freetime health  goout       paredu          sex           age       
    ##  1: 45    1: 90   1: 48   Min.   :0.000   Female:383   Min.   :15.00  
    ##  2:107    2: 78   2:145   1st Qu.:3.000   Male  :266   1st Qu.:16.00  
    ##  3:251    3:124   3:205   Median :5.000                Median :17.00  
    ##  4:178    4:108   4:141   Mean   :4.821                Mean   :16.74  
    ##  5: 68    5:249   5:110   3rd Qu.:6.000                3rd Qu.:18.00  
    ##                           Max.   :8.000                Max.   :22.00  
    ##         reason        grades      
    ##  home      :149   Min.   : 1.333  
    ##  reputation:143   1st Qu.:10.000  
    ##  course    :285   Median :11.667  
    ##  other     : 72   Mean   :11.625  
    ##                   3rd Qu.:13.333  
    ##                   Max.   :18.667

``` r
psych::describe(sacf.dat) #skew is high for guardian, Mjob, Fjob. 
```

    ##             vars   n  mean   sd median trimmed  mad   min   max range  skew
    ## Mjob*          1 649  3.71 1.35   4.00    3.88 1.48  1.00  5.00  4.00 -0.73
    ## Fjob*          2 649  4.05 1.22   5.00    4.24 0.00  1.00  5.00  4.00 -0.95
    ## guardian*      3 649  1.36 0.60   1.00    1.25 0.00  1.00  3.00  2.00  1.43
    ## famsup*        4 649  1.61 0.49   2.00    1.64 0.00  1.00  2.00  1.00 -0.46
    ## activities*    5 649  1.49 0.50   1.00    1.48 0.00  1.00  2.00  1.00  0.06
    ## romantic*      6 649  1.37 0.48   1.00    1.34 0.00  1.00  2.00  1.00  0.55
    ## freetime*      7 649  3.18 1.05   3.00    3.19 1.48  1.00  5.00  4.00 -0.18
    ## health*        8 649  3.54 1.45   4.00    3.67 1.48  1.00  5.00  4.00 -0.50
    ## goout*         9 649  3.18 1.18   3.00    3.20 1.48  1.00  5.00  4.00 -0.01
    ## paredu        10 649  4.82 2.03   5.00    4.79 2.97  0.00  8.00  8.00  0.12
    ## sex*          11 649  1.41 0.49   1.00    1.39 0.00  1.00  2.00  1.00  0.37
    ## age           12 649 16.74 1.22  17.00   16.70 1.48 15.00 22.00  7.00  0.41
    ## reason*       13 649  2.43 0.96   3.00    2.41 1.48  1.00  4.00  3.00 -0.20
    ## grades        14 649 11.63 2.83  11.67   11.64 2.47  1.33 18.67 17.33 -0.23
    ##             kurtosis   se
    ## Mjob*          -0.66 0.05
    ## Fjob*          -0.18 0.05
    ## guardian*       0.95 0.02
    ## famsup*        -1.79 0.02
    ## activities*    -2.00 0.02
    ## romantic*      -1.71 0.02
    ## freetime*      -0.41 0.04
    ## health*        -1.13 0.06
    ## goout*         -0.87 0.05
    ## paredu         -1.14 0.08
    ## sex*           -1.87 0.02
    ## age             0.05 0.05
    ## reason*        -1.04 0.04
    ## grades          0.58 0.11

``` r
sum(is.na(sacf.dat)) #there are no missing values in the dataframe
```

    ## [1] 0

``` r
#ggpairs to look at overall patterns
plotgg1 <- GGally::ggpairs(sacf.dat, 
                          progress = FALSE, 
                          alpha = 0.2)
```

    ## Warning in warn_if_args_exist(list(...)): Extra arguments: "alpha" are being
    ## ignored.  If these are meant to be aesthetics, submit them using the 'mapping'
    ## variable within ggpairs with ggplot2::aes or ggplot2::aes_string.

``` r
suppressWarnings(print(plotgg1))
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/GGpairs-1.png)<!-- -->

``` r
age.count <- ggplot(sacf.dat, aes(x = age, fill = age, color = age  ) ) +
  geom_histogram(alpha = 0.5, bins = 10) +
  theme(legend.position = "right")

paredu.count <- ggplot(sacf.dat, aes(x = paredu, fill = paredu, color = paredu  ) ) +
  geom_histogram(alpha = 0.5, bins = 10) +
  theme(legend.position = "right")


plot_grid(age.count, paredu.count, labels=c( "age", "paredu"), ncol = 2, nrow = 1)
```

    ## Warning: The following aesthetics were dropped during statistical transformation: fill
    ## and colour.
    ## ℹ This can happen when ggplot fails to infer the correct grouping structure in
    ##   the data.
    ## ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
    ##   variable into a factor?
    ## The following aesthetics were dropped during statistical transformation: fill
    ## and colour.
    ## ℹ This can happen when ggplot fails to infer the correct grouping structure in
    ##   the data.
    ## ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
    ##   variable into a factor?

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/count-1.png)<!-- -->

## Scatterplots

``` r
num.dat <- select_if(sacf.dat, is.numeric)
head(num.dat)
```

    ##   paredu age    grades
    ## 1      8  18  7.333333
    ## 2      2  17 10.333333
    ## 3      2  15 12.333333
    ## 4      6  15 14.000000
    ## 5      6  16 12.333333
    ## 6      7  16 12.333333

``` r
age<- ggplot(sacf.dat, aes(x = grades, y = age)) +
  geom_point(position = position_jitter(0.33), col = "red", alpha=0.3) + 
  geom_smooth(method = lm, se = FALSE,linetype ="dashed") +
  #facet_wrap(~sex) +
  theme(legend.position = "right")

paredu<- ggplot(sacf.dat, aes(x = grades, y = paredu )) +
  geom_point(position = position_jitter(0.33), col = "green", alpha=0.3) + 
  geom_smooth(method = lm, se = FALSE,linetype ="dashed") +
  #facet_wrap(~sex) +
  theme(legend.position = "right")

plot_grid(age, paredu,labels=c("age", "paredu"), ncol = 2, nrow = 1)
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## `geom_smooth()` using formula = 'y ~ x'

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/Scatterplots-1.png)<!-- -->

``` r
#the spread seems to be quite even, with a few outliers
```

## Histogram

``` r
cat.dat <- select_if(sacf.dat, is.factor)
head(cat.dat)
```

    ##       Mjob     Fjob guardian famsup activities romantic freetime health goout
    ## 1  at_home  teacher   mother     no         no       no        3      3     4
    ## 2  at_home    other   father    yes         no       no        3      3     3
    ## 3  at_home    other   mother     no         no       no        3      3     2
    ## 4   health services   mother    yes        yes      yes        2      5     2
    ## 5    other    other   father    yes         no       no        3      5     2
    ## 6 services    other   mother    yes        yes       no        4      5     2
    ##      sex     reason
    ## 1 Female     course
    ## 2 Female     course
    ## 3 Female      other
    ## 4 Female       home
    ## 5 Female       home
    ## 6   Male reputation

``` r
#Histograms for each variables against grades
sex.plot <- ggplot(sacf.dat, aes(x = grades, fill = sex, color = sex  ) ) +
  geom_histogram(alpha = 0.5, bins = 30) +
  theme(legend.position = "right")

Mjob.plot <- ggplot(sacf.dat, aes(x = grades, fill = Mjob, color = Mjob  ) ) +
  geom_histogram(alpha = 0.5, bins = 30) +
  theme(legend.position = "right")

Fjob.plot <- ggplot(sacf.dat, aes(x = grades, fill = Fjob, color = Fjob  ) ) +
  geom_histogram(alpha = 0.5, bins = 30) +
  theme(legend.position = "right")

guardian.plot <- ggplot(sacf.dat, aes(x = grades, fill = guardian, color = guardian  ) ) +
  geom_histogram(alpha = 0.5, bins = 30) +
  theme(legend.position = "right")

famsup.plot <- ggplot(sacf.dat, aes(x = grades, fill = famsup, color = famsup  ) ) +
  geom_histogram(alpha = 0.5, bins = 30) +
  theme(legend.position = "right")

activities.plot <- ggplot(sacf.dat, aes(x = grades, fill = activities, color = activities  ) ) +
  geom_histogram(alpha = 0.5, bins = 30) +
  theme(legend.position = "right")

romantic.plot <- ggplot(sacf.dat, aes(x = grades, fill = romantic, color = romantic  ) ) +
  geom_histogram(alpha = 0.5, bins = 30) +
  theme(legend.position = "right")

freetime.plot <- ggplot(sacf.dat, aes(x = grades, fill = freetime, color = freetime  ) ) +
  geom_histogram(alpha = 0.5, bins = 30) +
  theme(legend.position = "right")

health.plot <- ggplot(sacf.dat, aes(x = grades, fill = health, color = health  ) ) +
  geom_histogram(alpha = 0.5, bins = 30) +
  theme(legend.position = "right")

goout.plot <- ggplot(sacf.dat, aes(x = grades, fill = goout, color = goout  ) ) +
  geom_histogram(alpha = 0.5, bins = 30) +
  theme(legend.position = "right")

reason.plot <- ggplot(sacf.dat, aes(x = grades, fill = reason, color = reason  ) ) +
  geom_histogram(alpha = 0.5, bins = 30) +
  theme(legend.position = "right")

age.plot <- ggplot(sacf.dat, aes(x = grades, fill = age, color = age  ) ) +
  geom_histogram(alpha = 0.5, bins = 30) +
  theme(legend.position = "right")

paredu.plot <- ggplot(sacf.dat, aes(x = grades, fill = paredu, color = paredu  ) ) +
  geom_histogram(alpha = 0.5, bins = 30) +
  theme(legend.position = "right")


plot_grid(sex.plot, Mjob.plot, Fjob.plot, guardian.plot, famsup.plot, activities.plot, romantic.plot, freetime.plot, health.plot, goout.plot, reason.plot, age.plot, paredu.plot, 
          labels=c("sex", "Mjob","Fjob", "guardian", "famsup", "activities", "romantic", "freetime", "health", "goout",  "reason", "age",  "paredu"), 
          ncol = 3, nrow = 5)
```

    ## Warning: The following aesthetics were dropped during statistical transformation: fill
    ## and colour.
    ## ℹ This can happen when ggplot fails to infer the correct grouping structure in
    ##   the data.
    ## ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
    ##   variable into a factor?
    ## The following aesthetics were dropped during statistical transformation: fill
    ## and colour.
    ## ℹ This can happen when ggplot fails to infer the correct grouping structure in
    ##   the data.
    ## ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
    ##   variable into a factor?

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/Histogram-1.png)<!-- -->

## Bar graphs

``` r
sex.check <- ggplot(sacf.dat, aes(x = sex, fill = sex, color = sex  ) ) +
  geom_bar(alpha = 0.5, bins = 30) +
  theme(legend.position = "right")
```

    ## Warning in geom_bar(alpha = 0.5, bins = 30): Ignoring unknown parameters:
    ## `bins`

``` r
Mjob.check <- ggplot(sacf.dat, aes(x = Mjob, fill = Mjob, color = Mjob  ) ) +
  geom_bar(alpha = 0.5, bins = 30) +
  theme(legend.position = "right")
```

    ## Warning in geom_bar(alpha = 0.5, bins = 30): Ignoring unknown parameters:
    ## `bins`

``` r
Fjob.check <- ggplot(sacf.dat, aes(x = Fjob, fill = Fjob, color = Fjob  ) ) +
  geom_bar(alpha = 0.5, bins = 30) +
  theme(legend.position = "right")
```

    ## Warning in geom_bar(alpha = 0.5, bins = 30): Ignoring unknown parameters:
    ## `bins`

``` r
guardian.check <- ggplot(sacf.dat, aes(x = guardian, fill = guardian, color = guardian  ) ) +
  geom_bar(alpha = 0.5, bins = 30) +
  theme(legend.position = "right")
```

    ## Warning in geom_bar(alpha = 0.5, bins = 30): Ignoring unknown parameters:
    ## `bins`

``` r
famsup.check <- ggplot(sacf.dat, aes(x = famsup, fill = famsup, color = famsup  ) ) +
  geom_bar(alpha = 0.5, bins = 30) +
  theme(legend.position = "right")
```

    ## Warning in geom_bar(alpha = 0.5, bins = 30): Ignoring unknown parameters:
    ## `bins`

``` r
activities.check <- ggplot(sacf.dat, aes(x = activities, fill = activities, color = activities  ) ) +
  geom_bar(alpha = 0.5, bins = 30) +
  theme(legend.position = "right")
```

    ## Warning in geom_bar(alpha = 0.5, bins = 30): Ignoring unknown parameters:
    ## `bins`

``` r
romantic.check <- ggplot(sacf.dat, aes(x = romantic, fill = romantic, color = romantic  ) ) +
  geom_bar(alpha = 0.5, bins = 30) +
  theme(legend.position = "right")
```

    ## Warning in geom_bar(alpha = 0.5, bins = 30): Ignoring unknown parameters:
    ## `bins`

``` r
freetime.check <- ggplot(sacf.dat, aes(x = freetime, fill = freetime, color = freetime  ) ) +
  geom_bar(alpha = 0.5, bins = 30) +
  theme(legend.position = "right")
```

    ## Warning in geom_bar(alpha = 0.5, bins = 30): Ignoring unknown parameters:
    ## `bins`

``` r
health.check <- ggplot(sacf.dat, aes(x = health, fill = health, color = health  ) ) +
  geom_bar(alpha = 0.5, bins = 30) +
  theme(legend.position = "right")
```

    ## Warning in geom_bar(alpha = 0.5, bins = 30): Ignoring unknown parameters:
    ## `bins`

``` r
goout.check <- ggplot(sacf.dat, aes(x = goout, fill = goout, color = goout  ) ) +
  geom_bar(alpha = 0.5, bins = 30) +
  theme(legend.position = "right")
```

    ## Warning in geom_bar(alpha = 0.5, bins = 30): Ignoring unknown parameters:
    ## `bins`

``` r
reason.check <- ggplot(sacf.dat, aes(x = reason, fill = reason, color = reason  ) ) +
  geom_bar(alpha = 0.5, bins = 30) +
  theme(legend.position = "right")
```

    ## Warning in geom_bar(alpha = 0.5, bins = 30): Ignoring unknown parameters:
    ## `bins`

``` r
plot_grid(sex.check, Mjob.check, Fjob.check, guardian.check,  famsup.check, activities.check, romantic.check, freetime.check, health.check, goout.check, reason.check, 
          labels=c("sex", "Mjob","Fjob", "guardian", "famsup", "activities", "romantic", "freetime", "health", "goout", "reason"), 
          ncol = 3, nrow = 4)
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/Bar%20Check-1.png)<!-- -->

``` r
#again we see how Mjob, Fjob, and guardian are skewed.
#health does look a little skewed, but the psych::description showed that it is fine, so we will trust that.
```

## Boxplots

``` r
#boxplots 
sex.plot1 <- ggplot(sacf.dat, aes(y = grades, x = sex, fill = sex, color = sex  ) ) +
  geom_boxplot(alpha = 0.5, bins = 30) +
  theme(legend.position = "right")
```

    ## Warning in geom_boxplot(alpha = 0.5, bins = 30): Ignoring unknown parameters:
    ## `bins`

``` r
Mjob.plot1 <- ggplot(sacf.dat, aes(y = grades, x = Mjob, fill = Mjob, color = Mjob  ) ) +
  geom_boxplot(alpha = 0.5, bins = 30) +
  theme(legend.position = "right")
```

    ## Warning in geom_boxplot(alpha = 0.5, bins = 30): Ignoring unknown parameters:
    ## `bins`

``` r
Fjob.plot1 <- ggplot(sacf.dat, aes(y = grades, x = Fjob, fill = Fjob, color = Fjob  ) ) +
  geom_boxplot(alpha = 0.5, bins = 30) +
  theme(legend.position = "right")
```

    ## Warning in geom_boxplot(alpha = 0.5, bins = 30): Ignoring unknown parameters:
    ## `bins`

``` r
guardian.plot1 <- ggplot(sacf.dat, aes(y = grades, x = guardian, fill = guardian, color = guardian  ) ) +
  geom_boxplot(alpha = 0.5, bins = 30) +
  theme(legend.position = "right")
```

    ## Warning in geom_boxplot(alpha = 0.5, bins = 30): Ignoring unknown parameters:
    ## `bins`

``` r
famsup.plot1 <- ggplot(sacf.dat, aes(y = grades, x = famsup, fill = famsup, color = famsup  ) ) +
  geom_boxplot(alpha = 0.5, bins = 30) +
  theme(legend.position = "right")
```

    ## Warning in geom_boxplot(alpha = 0.5, bins = 30): Ignoring unknown parameters:
    ## `bins`

``` r
activities.plot1 <- ggplot(sacf.dat, aes(y = grades, x = activities, fill = activities, color = activities  ) ) +
  geom_boxplot(alpha = 0.5, bins = 30) +
  theme(legend.position = "right")
```

    ## Warning in geom_boxplot(alpha = 0.5, bins = 30): Ignoring unknown parameters:
    ## `bins`

``` r
romantic.plot1 <- ggplot(sacf.dat, aes(y = grades, x = romantic, fill = romantic, color = romantic  ) ) +
  geom_boxplot(alpha = 0.5, bins = 30) +
  theme(legend.position = "right")
```

    ## Warning in geom_boxplot(alpha = 0.5, bins = 30): Ignoring unknown parameters:
    ## `bins`

``` r
freetime.plot1 <- ggplot(sacf.dat, aes(y = grades, x = freetime, fill = freetime, color = freetime  ) ) +
  geom_boxplot(alpha = 0.5, bins = 30) +
  theme(legend.position = "right")
```

    ## Warning in geom_boxplot(alpha = 0.5, bins = 30): Ignoring unknown parameters:
    ## `bins`

``` r
health.plot1 <- ggplot(sacf.dat, aes(y = grades, x = health, fill = health, color = health  ) ) +
  geom_boxplot(alpha = 0.5, bins = 30) +
  theme(legend.position = "right")
```

    ## Warning in geom_boxplot(alpha = 0.5, bins = 30): Ignoring unknown parameters:
    ## `bins`

``` r
goout.plot1 <- ggplot(sacf.dat, aes(y = grades, x = goout, fill = goout, color = goout  ) ) +
  geom_boxplot(alpha = 0.5, bins = 30) +
  theme(legend.position = "right")
```

    ## Warning in geom_boxplot(alpha = 0.5, bins = 30): Ignoring unknown parameters:
    ## `bins`

``` r
reason.plot1 <- ggplot(sacf.dat, aes(y = grades, x = reason, fill = reason, color = reason  ) ) +
  geom_boxplot(alpha = 0.5, bins = 30) +
  theme(legend.position = "right")
```

    ## Warning in geom_boxplot(alpha = 0.5, bins = 30): Ignoring unknown parameters:
    ## `bins`

``` r
plot_grid(sex.plot1, Mjob.plot1, Fjob.plot1, guardian.plot1, famsup.plot1, activities.plot1, romantic.plot1, freetime.plot1, health.plot1, goout.plot1, reason.plot1, 
          labels=c("sex", "Mjob","Fjob", "guardian", "famsup", "activities", "romantic", "freetime", "health", "goout", "reason"), 
          ncol = 3, nrow = 4)
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/Boxplot-1.png)<!-- -->

``` r
sacf.dat <- sacf.dat %>% 
  mutate(paredu.cat = case_when(paredu > 4 ~ "High",
                                       paredu <= 4 ~ "Low",
                                       ))

head(sacf.dat)
```

    ##       Mjob     Fjob guardian famsup activities romantic freetime health goout
    ## 1  at_home  teacher   mother     no         no       no        3      3     4
    ## 2  at_home    other   father    yes         no       no        3      3     3
    ## 3  at_home    other   mother     no         no       no        3      3     2
    ## 4   health services   mother    yes        yes      yes        2      5     2
    ## 5    other    other   father    yes         no       no        3      5     2
    ## 6 services    other   mother    yes        yes       no        4      5     2
    ##   paredu    sex age     reason    grades paredu.cat
    ## 1      8 Female  18     course  7.333333       High
    ## 2      2 Female  17     course 10.333333        Low
    ## 3      2 Female  15      other 12.333333        Low
    ## 4      6 Female  15       home 14.000000       High
    ## 5      6 Female  16       home 12.333333       High
    ## 6      7   Male  16 reputation 12.333333       High

``` r
ggplot(sacf.dat, aes(x = grades, y = paredu, fill = paredu.cat)) +
  geom_point(size = 2, shape = 16, position = position_jitter(0.5)) +
  facet_wrap(~ paredu.cat)
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/paredu%20plot-1.png)<!-- -->

## Check for Near-Zero Variance

``` r
nearZeroVar(
  cat.dat,
  freqCut = 80/20,
  uniqueCut = 10,
  saveMetrics = TRUE,
  names = FALSE,
  foreach = FALSE,
  allowParallel = TRUE
)
```

    ##            freqRatio percentUnique zeroVar   nzv
    ## Mjob        1.897059     0.7704160   FALSE FALSE
    ## Fjob        2.027624     0.7704160   FALSE FALSE
    ## guardian    2.973856     0.4622496   FALSE FALSE
    ## famsup      1.585657     0.3081664   FALSE FALSE
    ## activities  1.060317     0.3081664   FALSE FALSE
    ## romantic    1.715481     0.3081664   FALSE FALSE
    ## freetime    1.410112     0.7704160   FALSE FALSE
    ## health      2.008065     0.7704160   FALSE FALSE
    ## goout       1.413793     0.7704160   FALSE FALSE
    ## sex         1.439850     0.3081664   FALSE FALSE
    ## reason      1.912752     0.6163328   FALSE FALSE

``` r
#None of the variables failed nzv test. Although this is to check for dichotomized variables 
```

## Selecting final variables to use

``` r
#we remove Mjob, Fjob, and guardian due to skewness issue. 

sac.dat <- select(sacf.dat, sex, age, paredu, famsup, activities, romantic, freetime, health, goout, reason, grades)


head(sac.dat)
```

    ##      sex age paredu famsup activities romantic freetime health goout     reason
    ## 1 Female  18      8     no         no       no        3      3     4     course
    ## 2 Female  17      2    yes         no       no        3      3     3     course
    ## 3 Female  15      2     no         no       no        3      3     2      other
    ## 4 Female  15      6    yes        yes      yes        2      5     2       home
    ## 5 Female  16      6    yes         no       no        3      5     2       home
    ## 6   Male  16      7    yes        yes       no        4      5     2 reputation
    ##      grades
    ## 1  7.333333
    ## 2 10.333333
    ## 3 12.333333
    ## 4 14.000000
    ## 5 12.333333
    ## 6 12.333333

``` r
str(sac.dat)
```

    ## 'data.frame':    649 obs. of  11 variables:
    ##  $ sex       : Factor w/ 2 levels "Female","Male": 1 1 1 1 1 2 2 1 2 2 ...
    ##  $ age       : int  18 17 15 15 16 16 16 17 15 15 ...
    ##  $ paredu    : int  8 2 2 6 6 7 4 8 5 7 ...
    ##  $ famsup    : Factor w/ 2 levels "no","yes": 1 2 1 2 2 2 1 2 2 2 ...
    ##  $ activities: Factor w/ 2 levels "no","yes": 1 1 1 2 1 2 1 1 1 2 ...
    ##  $ romantic  : Factor w/ 2 levels "no","yes": 1 1 1 2 1 1 1 1 1 1 ...
    ##  $ freetime  : Factor w/ 5 levels "1","2","3","4",..: 3 3 3 2 3 4 4 1 2 5 ...
    ##  $ health    : Factor w/ 5 levels "1","2","3","4",..: 3 3 3 5 5 5 3 1 1 5 ...
    ##  $ goout     : Factor w/ 5 levels "1","2","3","4",..: 4 3 2 2 2 2 4 4 2 1 ...
    ##  $ reason    : Factor w/ 4 levels "home","reputation",..: 3 3 4 1 1 2 1 1 1 1 ...
    ##  $ grades    : num  7.33 10.33 12.33 14 12.33 ...

``` r
summary(sac.dat)
```

    ##      sex           age            paredu      famsup    activities romantic 
    ##  Female:383   Min.   :15.00   Min.   :0.000   no :251   no :334    no :410  
    ##  Male  :266   1st Qu.:16.00   1st Qu.:3.000   yes:398   yes:315    yes:239  
    ##               Median :17.00   Median :5.000                                 
    ##               Mean   :16.74   Mean   :4.821                                 
    ##               3rd Qu.:18.00   3rd Qu.:6.000                                 
    ##               Max.   :22.00   Max.   :8.000                                 
    ##  freetime health  goout          reason        grades      
    ##  1: 45    1: 90   1: 48   home      :149   Min.   : 1.333  
    ##  2:107    2: 78   2:145   reputation:143   1st Qu.:10.000  
    ##  3:251    3:124   3:205   course    :285   Median :11.667  
    ##  4:178    4:108   4:141   other     : 72   Mean   :11.625  
    ##  5: 68    5:249   5:110                    3rd Qu.:13.333  
    ##                                            Max.   :18.667

``` r
psych::describe(sac.dat)
```

    ##             vars   n  mean   sd median trimmed  mad   min   max range  skew
    ## sex*           1 649  1.41 0.49   1.00    1.39 0.00  1.00  2.00  1.00  0.37
    ## age            2 649 16.74 1.22  17.00   16.70 1.48 15.00 22.00  7.00  0.41
    ## paredu         3 649  4.82 2.03   5.00    4.79 2.97  0.00  8.00  8.00  0.12
    ## famsup*        4 649  1.61 0.49   2.00    1.64 0.00  1.00  2.00  1.00 -0.46
    ## activities*    5 649  1.49 0.50   1.00    1.48 0.00  1.00  2.00  1.00  0.06
    ## romantic*      6 649  1.37 0.48   1.00    1.34 0.00  1.00  2.00  1.00  0.55
    ## freetime*      7 649  3.18 1.05   3.00    3.19 1.48  1.00  5.00  4.00 -0.18
    ## health*        8 649  3.54 1.45   4.00    3.67 1.48  1.00  5.00  4.00 -0.50
    ## goout*         9 649  3.18 1.18   3.00    3.20 1.48  1.00  5.00  4.00 -0.01
    ## reason*       10 649  2.43 0.96   3.00    2.41 1.48  1.00  4.00  3.00 -0.20
    ## grades        11 649 11.63 2.83  11.67   11.64 2.47  1.33 18.67 17.33 -0.23
    ##             kurtosis   se
    ## sex*           -1.87 0.02
    ## age             0.05 0.05
    ## paredu         -1.14 0.08
    ## famsup*        -1.79 0.02
    ## activities*    -2.00 0.02
    ## romantic*      -1.71 0.02
    ## freetime*      -0.41 0.04
    ## health*        -1.13 0.06
    ## goout*         -0.87 0.05
    ## reason*        -1.04 0.04
    ## grades          0.58 0.11

``` r
sum(is.na(sac.dat))
```

    ## [1] 0

``` r
#we kept variables with low skewness but also kept some variables with intermediate skew levels because we are interested in those variables (think they might help predict grades). 
```

## Checking correlations between variables

``` r
car::vif(lm(grades ~ ., 
            data = sac.dat,
            ))
```

    ##                GVIF Df GVIF^(1/(2*Df))
    ## sex        1.129465  1        1.062763
    ## age        1.101425  1        1.049488
    ## paredu     1.095749  1        1.046780
    ## famsup     1.078598  1        1.038556
    ## activities 1.102624  1        1.050059
    ## romantic   1.080818  1        1.039624
    ## freetime   1.433898  4        1.046080
    ## health     1.142574  4        1.016800
    ## goout      1.397374  4        1.042711
    ## reason     1.141863  3        1.022356

``` r
#correlations between variables and grades are extremely low here. Where 1 means no correlation and larger number means more correlations
```

# Part 2: Modeling

## Create Train and Test Set

``` r
set.seed(1234) 

train.index <- caret::createDataPartition(sac.dat$grades, p = .7, list = FALSE)

train.dat <- sac.dat[ train.index, ]
test.dat <- sac.dat[ - train.index, ]

dim(train.dat)
```

    ## [1] 457  11

``` r
dim(test.dat)
```

    ## [1] 192  11

``` r
my.seed <- 111

set.seed(my.seed)
tr.Control <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5
                           )
```

## Multiple Linear Regression

``` r
set.seed(my.seed)
lm.fit <- train(grades ~ ., 
             method = "lm",
             trControl = tr.Control,
             data = train.dat,
             metric = "RMSE",
             preProcess = c("center", "scale")
             )

lm.fit$results
```

    ##   intercept     RMSE  Rsquared      MAE    RMSESD RsquaredSD     MAESD
    ## 1      TRUE 2.576139 0.1642073 2.015726 0.2196639  0.0861487 0.1693247

## Regularized Regression

``` r
#random search
set.seed(my.seed)
elastic.fit1 <- train(grades ~ ., 
                  data = train.dat, 
                  method = 'glmnet', 
                  trControl = tr.Control,
                  verbose = FALSE,
                  tuneLength = 20,
                  metric = "RMSE",
                  preProcess = c("center", "scale")
                  )
```

    ## Warning in nominalTrainWorkflow(x = x, y = y, wts = weights, info = trainInfo,
    ## : There were missing values in resampled performance measures.

``` r
plot(elastic.fit1)
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/elastic%20net-1.png)<!-- -->

``` r
elastic.fit1$results[ rownames(elastic.fit1$bestTune), ]
```

    ##        alpha    lambda     RMSE  Rsquared     MAE    RMSESD RsquaredSD
    ## 31 0.1473684 0.2750149 2.560243 0.1659912 1.98834 0.2127696 0.08922157
    ##        MAESD
    ## 31 0.1535565

``` r
unique(elastic.fit1$results$alpha) 
```

    ##  [1] 0.1000000 0.1473684 0.1947368 0.2421053 0.2894737 0.3368421 0.3842105
    ##  [8] 0.4315789 0.4789474 0.5263158 0.5736842 0.6210526 0.6684211 0.7157895
    ## [15] 0.7631579 0.8105263 0.8578947 0.9052632 0.9526316 1.0000000

``` r
#alpha is close to one end of the tuning range.
unique(elastic.fit1$results$lambda)
```

    ##  [1] 0.0009186832 0.0014244328 0.0022086055 0.0034244777 0.0053097068
    ##  [6] 0.0082327842 0.0127650617 0.0197924297 0.0306884745 0.0475829640
    ## [11] 0.0737781365 0.1143941649 0.1773699578 0.2750149185 0.4264149708
    ## [16] 0.6611631410 1.0251438831

``` r
#grid search
set.seed(my.seed) 
elastic.grid <- expand.grid(alpha = seq(0, 0.3, length.out = 20),
                            lambda = seq(0.1, 0.6, length.out = 20))

set.seed(my.seed)
elastic.fit2 <- train(grades ~ ., 
                      data = train.dat, 
                      method = 'glmnet', 
                      trControl = tr.Control,
                      verbose = FALSE,
                      tuneGrid = elastic.grid,
                      metric = "RMSE",
                      preProcess = c("center", "scale")
                      )

plot(elastic.fit2)
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/elastic%20net-2.png)<!-- -->

``` r
elastic.fit2$results[ rownames(elastic.fit2$bestTune), ]
```

    ##          alpha    lambda    RMSE  Rsquared      MAE    RMSESD RsquaredSD
    ## 130 0.09473684 0.3368421 2.56004 0.1658442 1.988752 0.2142611 0.08915918
    ##         MAESD
    ## 130 0.1543687

``` r
unique(elastic.fit2$results$alpha) 
```

    ##  [1] 0.00000000 0.01578947 0.03157895 0.04736842 0.06315789 0.07894737
    ##  [7] 0.09473684 0.11052632 0.12631579 0.14210526 0.15789474 0.17368421
    ## [13] 0.18947368 0.20526316 0.22105263 0.23684211 0.25263158 0.26842105
    ## [19] 0.28421053 0.30000000

``` r
unique(elastic.fit2$results$lambda)
```

    ##  [1] 0.1000000 0.1263158 0.1526316 0.1789474 0.2052632 0.2315789 0.2578947
    ##  [8] 0.2842105 0.3105263 0.3368421 0.3631579 0.3894737 0.4157895 0.4421053
    ## [15] 0.4684211 0.4947368 0.5210526 0.5473684 0.5736842 0.6000000

``` r
#alpha and lambda are not at the end of the tuning range now.


# get the upper and lower RMSE bordering the optimal RMSE
opt.RMSE.index <- which( unique(elastic.fit2$results$RMSE) %in% 
                          elastic.fit2$results[rownames(elastic.fit2$bestTune),])


RMSE.new.range <- unique(elastic.fit2$results$RMSE)[c(opt.RMSE.index -1, 
                                    opt.RMSE.index,
                                    opt.RMSE.index + 1) ]


elastic.fit2$results[elastic.fit2$results$RMSE %in% RMSE.new.range, ]
```

    ##          alpha    lambda     RMSE  Rsquared      MAE    RMSESD RsquaredSD
    ## 129 0.09473684 0.3105263 2.560071 0.1657937 1.988831 0.2144791 0.08898615
    ## 130 0.09473684 0.3368421 2.560040 0.1658442 1.988752 0.2142611 0.08915918
    ## 131 0.09473684 0.3631579 2.560179 0.1658411 1.988876 0.2140479 0.08930964
    ##         MAESD
    ## 129 0.1549954
    ## 130 0.1543687
    ## 131 0.1538091

``` r
#how much RMSE varies from one sample to the next = RMSESD/sqrt(50)
cv.sd = elastic.fit2$results [ row.names(elastic.fit2$bestTune),]$RMSESD
cv.n = 50
avCV.SE = cv.sd / sqrt(cv.n)
avCV.SE
```

    ## [1] 0.03030109

``` r
#We see that the RMSE has not changed much. The variation in RMSE seems much smaller than the CV RMSE SE = 0.03. So we stop.
#RMSE should vary about 0.03 across different samples. However, the RMSE here vary less than 0.01, so we can stop fine tuning here.
```

## Decision Tree

``` r
#random search
set.seed(my.seed)
tree.fit1 <- train(grades ~ .,
                   data = train.dat,
                   trControl = tr.Control,
                   method = "rpart",
                   metric = "RMSE",
                   #preProcess = c("center", "scale"),
                   tuneLength = 20
                   )
```

    ## Warning in nominalTrainWorkflow(x = x, y = y, wts = weights, info = trainInfo,
    ## : There were missing values in resampled performance measures.

``` r
tree.fit1$results [ row.names(tree.fit1$bestTune),]
```

    ##            cp     RMSE Rsquared      MAE    RMSESD RsquaredSD     MAESD
    ## 18 0.02313252 2.703663 0.073586 2.130176 0.2343079 0.06035126 0.1599496

``` r
plot(tree.fit1)
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/decision%20tree-1.png)<!-- -->

``` r
rpart.plot(tree.fit1$finalModel)
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/decision%20tree-2.png)<!-- -->

``` r
unique(tree.fit1$results$cp)
```

    ##  [1] 0.003810606 0.004585483 0.005286322 0.005745640 0.006240667 0.006589220
    ##  [7] 0.006784420 0.008405313 0.008519346 0.008640897 0.008660516 0.009572490
    ## [13] 0.010597562 0.011084748 0.011817110 0.012459137 0.014042632 0.023132523
    ## [19] 0.031686707 0.070890298

``` r
#cp is quite close to the extreme end of selected range

#grid search
set.seed(my.seed)
tree.fit2 <- train(grades ~ .,
                   data = train.dat,
                   trControl = tr.Control,
                   method = "rpart",
                   metric = "RMSE",
                   #preProcess = c("center", "scale"),
                   tuneGrid = expand.grid(cp = seq(0.014, 0.032, length.out = 20 ) )
                   )

tree.fit2$results [ row.names(tree.fit2$bestTune),]
```

    ##            cp    RMSE   Rsquared      MAE    RMSESD RsquaredSD    MAESD
    ## 10 0.02252632 2.70047 0.07550309 2.128543 0.2361885 0.05958051 0.161072

``` r
plot(tree.fit2)
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/decision%20tree-3.png)<!-- -->

``` r
rpart.plot(tree.fit2$finalModel)
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/decision%20tree-4.png)<!-- -->

``` r
unique(tree.fit2$results$cp)
```

    ##  [1] 0.01400000 0.01494737 0.01589474 0.01684211 0.01778947 0.01873684
    ##  [7] 0.01968421 0.02063158 0.02157895 0.02252632 0.02347368 0.02442105
    ## [13] 0.02536842 0.02631579 0.02726316 0.02821053 0.02915789 0.03010526
    ## [19] 0.03105263 0.03200000

``` r
#cp is not close to the end of the tuning range, but the number of buckets is too little and we will fine tune that.


split.vec <- c(20, 40, 50, 60, 70, 80)

# create empty dataframe to store results of all minsplit
tree.result.dat <- data.frame()

for(i in split.vec){
set.seed(my.seed)
tree.fit3 <- train(grades ~ .,
                   data = train.dat,
                   trControl = tr.Control, 
                   method = "rpart",
                   metric = "RMSE",
                   #preProcess = c("center", "scale"),
                   tuneGrid = expand.grid(cp = seq(0.014, 0.032, length.out = 20 ) ),
                   control = rpart.control(minsplit = i)
                 )

temp.result <- tree.fit3$results
temp.result$minsplit = i

tree.result.dat <- rbind(tree.result.dat, temp.result)

}

ggplot(tree.result.dat, aes( x = cp, y = RMSE, color = as.factor(minsplit)) ) +
  geom_point() +
  geom_line()
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/decision%20tree-5.png)<!-- -->

``` r
tree.result.dat %>% 
  filter(RMSE == min(RMSE, na.rm = TRUE))
```

    ##           cp    RMSE   Rsquared      MAE    RMSESD RsquaredSD    MAESD minsplit
    ## 1 0.02252632 2.70047 0.07550309 2.128543 0.2361885 0.05958051 0.161072       20
    ## 2 0.02252632 2.70047 0.07550309 2.128543 0.2361885 0.05958051 0.161072       40
    ## 3 0.02252632 2.70047 0.07550309 2.128543 0.2361885 0.05958051 0.161072       50

``` r
#we will keep minsplit = 40 and cp = 0.02252632
set.seed(my.seed)

tree.fit4 <- train(grades ~ .,
                   data = train.dat,
                   trControl = tr.Control, 
                   method = "rpart",
                   metric = "RMSE",
                   #preProcess = c("center", "scale"),
                   tuneGrid = expand.grid(cp = 0.02252632    ),
                   control = rpart.control(minsplit = 40)
                 )

tree.fit4$results [ row.names(tree.fit4$bestTune),]
```

    ##           cp    RMSE   Rsquared      MAE    RMSESD RsquaredSD    MAESD
    ## 1 0.02252632 2.70047 0.07550309 2.128543 0.2361885 0.05958051 0.161072

``` r
#plot(tree.fit4)
rpart.plot(tree.fit4$finalModel)
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/decision%20tree-6.png)<!-- -->

## Random Forest

``` r
set.seed(my.seed)
rf.fit1 <- train(grades ~ .,
                 data = train.dat,
                 method = "rf",
                 trControl = tr.Control ,
                 #preProc = c("center", "scale"),  
                 ntree = 1000,
                 metric = "RMSE",
                 tuneGrid = expand.grid(mtry = seq(1, ncol(train.dat)-1))
                 )

#node size here = 10
```

``` r
# bag.fit

rf.fit1$results
```

    ##    mtry     RMSE  Rsquared      MAE    RMSESD RsquaredSD     MAESD
    ## 1     1 2.670301 0.1251016 2.121746 0.2255992 0.07271636 0.1450470
    ## 2     2 2.633737 0.1138724 2.080641 0.2117456 0.06639839 0.1332613
    ## 3     3 2.630691 0.1115196 2.071496 0.2019564 0.06008428 0.1289168
    ## 4     4 2.639827 0.1081964 2.073501 0.1990731 0.05932240 0.1281654
    ## 5     5 2.646452 0.1068010 2.077764 0.1950594 0.05793634 0.1267241
    ## 6     6 2.653286 0.1052388 2.079790 0.1924161 0.05631559 0.1279332
    ## 7     7 2.664393 0.1018892 2.088239 0.1942837 0.05558167 0.1306198
    ## 8     8 2.668567 0.1018751 2.089174 0.1911831 0.05496624 0.1288483
    ## 9     9 2.671777 0.1018588 2.092629 0.1915110 0.05566215 0.1304676
    ## 10   10 2.677769 0.1003134 2.098943 0.1951272 0.05527494 0.1310200

``` r
rf.fit1$results[ rownames(rf.fit1$bestTune),]
```

    ##   mtry     RMSE  Rsquared      MAE    RMSESD RsquaredSD     MAESD
    ## 3    3 2.630691 0.1115196 2.071496 0.2019564 0.06008428 0.1289168

``` r
plot(rf.fit1)
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/summary%20rf-1.png)<!-- -->

``` r
plot.dat <- rf.fit1$results
ggplot(plot.dat, aes(x = mtry, y = Rsquared )) +
  geom_point() +
  geom_line() +
  theme_bw()
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/summary%20rf-2.png)<!-- -->

``` r
#fine tune rf since it looks quite similar to the linear model 
```

``` r
node.size = c(10, 20, 30, 40, 50, 60)
rf2.results.dat <- NULL 
rf.models <- vector("list", length = length(node.size)) 



for(i in 1: length(node.size) ){
  
  cat("\n Iteration: ",i,  " ; Node size =  ",
      node.size[i], "\n")
  
  set.seed(my.seed)
  
  rf.fit2 <- train(grades ~ ., 
                   data = train.dat, 
                   trControl = tr.Control, 
                   method = "rf",
                 # preProc = c("center", "scale"), 
                   ntree = 1000, 
                   tuneGrid = expand.grid(mtry = seq(1, ncol(train.dat)-1) ), 
                   nodesize = node.size[i]
                   )
  
  rf.models[[i]] <- rf.fit2
  
  results.temp <- rf.fit2$results
  results.temp$node.size = node.size[i]
  
  # print(results.temp)
  
  rf2.results.dat <- rbind(rf2.results.dat, results.temp)
}  
```

    ## 
    ##  Iteration:  1  ; Node size =   10 
    ## 
    ##  Iteration:  2  ; Node size =   20 
    ## 
    ##  Iteration:  3  ; Node size =   30 
    ## 
    ##  Iteration:  4  ; Node size =   40 
    ## 
    ##  Iteration:  5  ; Node size =   50 
    ## 
    ##  Iteration:  6  ; Node size =   60

``` r
rf2plot.dat <- rf2.results.dat
rf2plot.dat$node.size <- factor(rf2plot.dat$node.size)
ggplot(rf2plot.dat, aes(x = mtry, y = RMSE, color = node.size )) +
  geom_point() +
  geom_line() +
  theme_bw()
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/summary%20rf2-1.png)<!-- -->

``` r
rf2.results.dat[rf2.results.dat$RMSE == min(rf2.results.dat$RMSE), ]
```

    ##    mtry     RMSE  Rsquared     MAE    RMSESD RsquaredSD     MAESD node.size
    ## 57    7 2.587995 0.1456905 2.02236 0.2097109   0.073771 0.1339882        60

``` r
#from the graph we can see that both node size 10 and 20 does well, almost equally. We will go with node size 10 for our chosen model.
#since there is not a big change from rf.fit1, we will stop fine tuning here.
rf.fitfinal <- rf.models[[ which(node.size == 60)  ]]
```

``` r
tree.N = c(500, 1000, 2000, 3000)

rf3.results.dat <- NULL 
rf.models2 <- vector("list", length = length(tree.N)) 

for(i in 1:length( tree.N)){
set.seed(30825920)
rf.fit3 <- train(grades ~ .,
                 data = train.dat,
                 method = "rf",
                 trControl = tr.Control ,
                 preProc = c("center", "scale"),  
                 ntree = tree.N[i],
                 tuneGrid = expand.grid(mtry = seq(1, ncol(train.dat)-1)
                         )
)

rf.models2[[i]] <- rf.fit3

results.temp <- rf.fit3$results
results.temp$tree.size = tree.N[i]

rf3.results.dat <- rbind(rf3.results.dat, results.temp)
}
```

``` r
rf3plot.dat <- rf3.results.dat
rf3plot.dat$tree.size <- factor(rf3plot.dat$tree.size)
ggplot(rf3plot.dat, aes(x = mtry, y = RMSE, color = tree.size )) +
  geom_point() +
  geom_line() +
  theme_bw()
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/summary%20rf3-1.png)<!-- -->

``` r
rf3.results.dat[rf3.results.dat$RMSE == min(rf3.results.dat$RMSE), ]
```

    ##    mtry    RMSE  Rsquared      MAE    RMSESD RsquaredSD    MAESD tree.size
    ## 12    2 2.63206 0.1155033 2.081163 0.2654382  0.0878663 0.181373      1000

``` r
#this did slightly worse than rf.fit2 (otherwise it's the same), so we will use rf.fit2 instead.
```

## Boosting

``` r
set.seed(my.seed)
gbm.fit1 <- train(grades ~ ., 
                  data = train.dat, 
                  method = "gbm", 
                  trControl = tr.Control, 
                  preProc = c("center", "scale"),
                  tuneLength = 20,
                  verbose = FALSE,
                  metric = "RMSE"
                  )
```

``` r
plot(gbm.fit1)
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/summary%20boosting-1.png)<!-- -->

``` r
gbm.fit1$results[ rownames(gbm.fit1$bestTune),]
```

    ##   shrinkage interaction.depth n.minobsinnode n.trees     RMSE Rsquared     MAE
    ## 2       0.1                 1             10     100 2.568025 0.161945 1.99818
    ##      RMSESD RsquaredSD     MAESD
    ## 2 0.2098142 0.08911544 0.1500962

``` r
tree.size <- c(50, 100, 500, 1000,  1500, 2000)

gbm.grid <- expand.grid(interaction.depth = c(1, 2, 3, 4),
                        n.trees = tree.size, 
                        shrinkage = c(10^-(1:3)),
                        n.minobsinnode = 10
                        )

set.seed(my.seed)
gbm.fit2 <- train(grades ~ ., 
                 data = train.dat, 
                 method = "gbm", 
                 trControl = tr.Control,  
                 preProc = c("center", "scale"), 
                 tuneGrid = gbm.grid, 
                 verbose = FALSE,
                 metric = "RMSE"
                 )
```

``` r
plot(gbm.fit2)
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/summary%20gbm2-1.png)<!-- -->

``` r
gbm.fit2$results[ rownames(gbm.fit2$bestTune),]
```

    ##    shrinkage interaction.depth n.minobsinnode n.trees     RMSE  Rsquared
    ## 28      0.01                 1             10    1000 2.563566 0.1640269
    ##         MAE    RMSESD RsquaredSD     MAESD
    ## 28 1.993294 0.2114444  0.0891012 0.1492431

# Part 3: Comparing Models

``` r
model.resamples <- resamples( list(Regression = lm.fit, 
               Elastic = elastic.fit2, 
               Tree = tree.fit4,
               RF = rf.fitfinal,
               Boosting = gbm.fit2))

summary(model.resamples)
```

    ## 
    ## Call:
    ## summary.resamples(object = model.resamples)
    ## 
    ## Models: Regression, Elastic, Tree, RF, Boosting 
    ## Number of resamples: 50 
    ## 
    ## MAE 
    ##                Min.  1st Qu.   Median     Mean  3rd Qu.     Max. NA's
    ## Regression 1.676993 1.888787 2.033141 2.015726 2.119890 2.364281    0
    ## Elastic    1.707368 1.869172 1.971440 1.988752 2.089666 2.350665    0
    ## Tree       1.887932 2.002949 2.094138 2.128543 2.219750 2.589130    0
    ## RF         1.792896 1.930809 2.012714 2.022360 2.114549 2.363991    0
    ## Boosting   1.671437 1.883625 1.990164 1.993294 2.066425 2.344192    0
    ## 
    ## RMSE 
    ##                Min.  1st Qu.   Median     Mean  3rd Qu.     Max. NA's
    ## Regression 2.138816 2.453151 2.560630 2.576139 2.704602 3.163428    0
    ## Elastic    2.170629 2.431644 2.537092 2.560040 2.686954 3.188372    0
    ## Tree       2.337741 2.524175 2.634610 2.700470 2.851354 3.305859    0
    ## RF         2.264037 2.461165 2.567661 2.587995 2.722219 3.191767    0
    ## Boosting   2.188182 2.437061 2.529624 2.563566 2.688840 3.183981    0
    ## 
    ## Rsquared 
    ##                    Min.    1st Qu.     Median       Mean   3rd Qu.      Max.
    ## Regression 1.604298e-03 0.09878338 0.15554560 0.16420725 0.2294914 0.3419734
    ## Elastic    1.862394e-02 0.09878126 0.15069644 0.16584422 0.2180598 0.3544555
    ## Tree       3.409078e-05 0.02727492 0.06552866 0.07550309 0.1122245 0.2273834
    ## RF         2.582424e-02 0.08248527 0.13694023 0.14569050 0.1864163 0.3210509
    ## Boosting   1.360126e-02 0.09399700 0.14955917 0.16402686 0.2181890 0.3773888
    ##            NA's
    ## Regression    0
    ## Elastic       0
    ## Tree          0
    ## RF            0
    ## Boosting      0

``` r
dotplot(model.resamples, metric = "RMSE")
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/combine%20models-1.png)<!-- -->

``` r
splom(model.resamples, metric = "RMSE")
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/combine%20models-2.png)<!-- -->

``` r
summary(diff(model.resamples))
```

    ## 
    ## Call:
    ## summary.diff.resamples(object = diff(model.resamples))
    ## 
    ## p-value adjustment: bonferroni 
    ## Upper diagonal: estimates of the difference
    ## Lower diagonal: p-value for H0: difference = 0
    ## 
    ## MAE 
    ##            Regression Elastic   Tree      RF        Boosting 
    ## Regression             0.026974 -0.112817 -0.006633  0.022432
    ## Elastic    0.00413              -0.139792 -0.033608 -0.004542
    ## Tree       3.053e-06  4.916e-11            0.106184  0.135249
    ## RF         1.00000    0.03498   2.258e-10            0.029065
    ## Boosting   0.30780    1.00000   2.249e-10 0.06324            
    ## 
    ## RMSE 
    ##            Regression Elastic   Tree      RF        Boosting 
    ## Regression             0.016100 -0.124330 -0.011855  0.012574
    ## Elastic    0.47657              -0.140430 -0.027955 -0.003526
    ## Tree       6.772e-06  1.079e-09            0.112475  0.136904
    ## RF         1.00000    0.05636   3.946e-10            0.024429
    ## Boosting   1.00000    1.00000   5.221e-09 0.28004            
    ## 
    ## Rsquared 
    ##            Regression Elastic    Tree       RF         Boosting  
    ## Regression            -0.0016370  0.0887042  0.0185168  0.0001804
    ## Elastic    1.000000               0.0903411  0.0201537  0.0018174
    ## Tree       6.710e-10  2.591e-10             -0.0701874 -0.0885238
    ## RF         0.108280   0.013471   4.844e-10             -0.0183364
    ## Boosting   1.000000   1.000000   8.889e-11  0.008829

``` r
RMSE.dat <- model.resamples$values %>% 
  select(Resample, ends_with("~RMSE"))

names(RMSE.dat) <- c("folds", "Logistic", "Elastic", "Tree", "RandomForest","GBM")

RMSE.dat %>% 
  pivot_longer(cols = !folds,
               names_to = "Method",
               values_to = "RMSE") %>% 
  
  ggplot(aes(x = Method, y = RMSE, color = Method)) +
  geom_point(position = position_jitter(width = 0.1))
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/performance%20of%20RMSE%20across%20train%20models-1.png)<!-- -->

``` r
RMSE.long <- RMSE.dat %>% 
  pivot_longer(cols = !folds,
               names_to = "Method",
               values_to = "RMSE") %>%  
  separate(folds, into =  c("Fold", "Rep"))


RMSE.long %>% 
   ggplot(aes(x = Fold, y = RMSE, color = Method)) +
  geom_point(position = position_dodge(width = 0.3), alpha = 0.3) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "pointrange", position = position_dodge(width = 0.5)) +
  theme_bw()
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/performance%20across%20cv%20folds-1.png)<!-- -->

``` r
RMSE.long %>% 
   ggplot(aes(x = Method, y = RMSE, color = Rep)) +
  geom_point(position = position_dodge(width = 0.3), alpha = 0.3) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "pointrange", position = position_dodge(width = 0.5)) +
  theme_bw()
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/performance%20across%20repetitions-1.png)<!-- -->

``` r
# getting cv measure for best tune :
cv.dat <- rbind(lm.fit$results[rownames(lm.fit$bestTune), c("RMSE", "RMSESD") ],
      elastic.fit2$results[rownames(elastic.fit2$bestTune), c("RMSE", "RMSESD") ],
      tree.fit4$results[rownames(tree.fit4$bestTune), c("RMSE", "RMSESD") ],
      rf.fitfinal$results[rownames(rf.fitfinal$bestTune), c("RMSE", "RMSESD")],
      gbm.fit2$results[rownames(gbm.fit2$bestTune), c("RMSE", "RMSESD")]
      )

# getting model names from resamples()
cv.dat$model <- model.resamples$models

#the number of repeated cv folds = 10 x 5 = 50
cv.N = 50

#SE = sd / sqrt(length(x))

#table:
cv.dat %>% 
  mutate(SE = RMSESD / sqrt(cv.N))
```

    ##         RMSE    RMSESD      model         SE
    ## 1   2.576139 0.2196639 Regression 0.03106517
    ## 130 2.560040 0.2142611    Elastic 0.03030109
    ## 11  2.700470 0.2361885       Tree 0.03340210
    ## 7   2.587995 0.2097109         RF 0.02965759
    ## 28  2.563566 0.2114444   Boosting 0.02990275

``` r
# plot: 
cv.dat %>% 
  mutate(SE = RMSESD / sqrt(cv.N)) %>% 

ggplot(aes(x = model, y = RMSE, color = model)) +
  # 95%CI for CV average RMSE
  geom_pointrange(aes(ymin = RMSE - 1.96*SE, ymax = RMSE + 1.96*SE) )
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/plot%20RMSE%20over%20model-1.png)<!-- -->

## Check Variable Importance

``` r
p1 <- vip(lm.fit) + ggtitle("Regression")
p2 <- vip(elastic.fit2) + ggtitle("Elastic")
p3 <- vip(tree.fit4) + ggtitle("Tree")
p4 <- vip(rf.fitfinal) + ggtitle("RF")
p5 <- vip(gbm.fit2) + ggtitle("Boosting")

grid.arrange(p1, p2, p3, p4, p5, ncol = 5)
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/vip%20train-1.png)<!-- -->

``` r
#combined top 3 from all:
  ##paredu
  ##sex
  #goout
  ##reason
  ##age
```

# Part 4: Model Performance

``` r
#Getting RMSE and Rsquared for each model on test.dat
#linear regression
lm.pred <- predict(lm.fit, newdata = test.dat, type = "raw")
lm.RMSE.pred <- RMSE(lm.pred, test.dat$grades)
lm.R2.pred <- R2(lm.pred, test.dat$grades)

#elastic net
elastic.pred <- predict(elastic.fit2, newdata = test.dat, type = "raw")
elastic.RMSE.pred <- RMSE(elastic.pred, test.dat$grades)
elastic.R2.pred <- R2(elastic.pred, test.dat$grades)

#decision tree
tree.pred <- predict(tree.fit4, newdata = test.dat, type = "raw")
tree.RMSE.pred <- RMSE(tree.pred, test.dat$grades)
tree.R2.pred <- R2(tree.pred, test.dat$grades)

#random forest
rf.pred <- predict(rf.fitfinal, newdata = test.dat, type = "raw")
rf.RMSE.pred <- RMSE(rf.pred, test.dat$grades)
rf.R2.pred <- R2(rf.pred, test.dat$grades)

#boosting
gbm.pred <- predict(gbm.fit2, newdata = test.dat, type = "raw")
gbm.RMSE.pred <- RMSE(gbm.pred, test.dat$grades)
gbm.R2.pred <- R2(gbm.pred, test.dat$grades)



test.results<- data.frame(models=c("lm","elastic","tree","rf","gbm"),
                          train.RMSE=c(lm.fit$results$RMSE,
                                       elastic.fit2$results[ rownames(elastic.fit2$bestTune), ]$RMSE,
                                       tree.fit4$results [ row.names(tree.fit4$bestTune),]$RMSE,
                                       rf2.results.dat[rf2.results.dat$RMSE == min(rf2.results.dat$RMSE), ]$RMSE,
                                       gbm.fit2$results[ rownames(gbm.fit2$bestTune),]$RMSE),
                          train.R2=c(lm.fit$results$Rsquared,
                                     elastic.fit2$results[ rownames(elastic.fit2$bestTune), ]$Rsquared,
                                     tree.fit4$results [ row.names(tree.fit4$bestTune),]$Rsquared,
                                     rf2.results.dat[rf2.results.dat$RMSE == min(rf2.results.dat$RMSE), ]$Rsquared,
                                     gbm.fit2$results[ rownames(gbm.fit2$bestTune),]$Rsquared
),
                          test.RMSE=c(lm.RMSE.pred,elastic.RMSE.pred,tree.RMSE.pred,rf.RMSE.pred,gbm.RMSE.pred),
                         test.R2=c(lm.R2.pred,elastic.R2.pred,tree.R2.pred,rf.R2.pred,gbm.R2.pred))
print(test.results)
```

    ##    models train.RMSE   train.R2 test.RMSE    test.R2
    ## 1      lm   2.576139 0.16420725  2.782950 0.12181139
    ## 2 elastic   2.560040 0.16584422  2.758887 0.12428297
    ## 3    tree   2.700470 0.07550309  2.831194 0.08049928
    ## 4      rf   2.587995 0.14569050  2.758322 0.13174238
    ## 5     gbm   2.563566 0.16402686  2.757155 0.12735992

``` r
#minimum RMSE for train set
test.results[which.min(test.results$train.RMSE),]
```

    ##    models train.RMSE  train.R2 test.RMSE  test.R2
    ## 2 elastic    2.56004 0.1658442  2.758887 0.124283

``` r
#minimum RMSE for test set
test.results[which.min(test.results$test.RMSE),]
```

    ##   models train.RMSE  train.R2 test.RMSE   test.R2
    ## 5    gbm   2.563566 0.1640269  2.757155 0.1273599

``` r
#based on the results, we can see that elastic had the lowest RMSE and highest Rsquared in train data. However, gbm had the lowest RMSE and higher Rsquared in test data.  
```

## Bootstrapping

``` r
boot.iter = 1000
set.seed(my.seed)

boot.dat <- data.frame(LM= vector(length = boot.iter),
                       Elastic = vector(length = boot.iter),
                       Tree = vector(length = boot.iter),
                       RF =  vector(length = boot.iter),
                       GBM = vector(length = boot.iter)
           )

for ( i in 1: boot.iter){
  
  # get a bootstrap sample:
  boot.index <- sample(   nrow(test.dat), replace = TRUE   )
  boot.sample <- test.dat[ boot.index, ]
  
  pred.dat <- data.frame( y.lm = predict(lm.fit, newdata = boot.sample),
                          y.elastic = predict(elastic.fit2, newdata = boot.sample),
                          y.tree= predict(tree.fit4, newdata = boot.sample),
                          y.rf = predict(rf.fitfinal, newdata = boot.sample),
                          y.gbm = predict(gbm.fit2, newdata = boot.sample))
  
  
  sq.err.dat <- (boot.sample$grades - pred.dat)^2 
  
  boot.MSE = colMeans(sq.err.dat)
  
  boot.RMSE = sqrt(boot.MSE)
  boot.dat[i, ] <-    boot.RMSE
  
}

head(boot.dat)
```

    ##         LM  Elastic     Tree       RF      GBM
    ## 1 2.665443 2.606571 2.611300 2.574718 2.602244
    ## 2 2.848095 2.782658 2.879957 2.739207 2.793967
    ## 3 2.798956 2.758910 2.728206 2.739180 2.697285
    ## 4 2.704563 2.656495 2.707461 2.598371 2.652230
    ## 5 2.452856 2.426535 2.568996 2.464149 2.463650
    ## 6 2.626322 2.587371 2.629527 2.550545 2.591210

``` r
mean.dat <- boot.dat %>% 
  pivot_longer(cols = everything(),
               names_to = "Model",
               values_to = "RMSE") %>% 
  group_by(Model) %>% 
  summarize(mean = mean(RMSE))

mean.dat
```

    ## # A tibble: 5 × 2
    ##   Model    mean
    ##   <chr>   <dbl>
    ## 1 Elastic  2.74
    ## 2 GBM      2.74
    ## 3 LM       2.76
    ## 4 RF       2.74
    ## 5 Tree     2.82

``` r
boot.dat %>% 
  pivot_longer(cols = everything(),
               names_to = "Model",
               values_to = "boot.RMSE") %>% 
  
  ggplot(aes(x = boot.RMSE, fill = Model)) +
  geom_histogram(col = "grey") +
  geom_vline(data = mean.dat, aes(xintercept = mean), col = "black", lty = 2 ) +
  facet_wrap( ~ Model, ncol = 1)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/bootstrap%20histogram-1.png)<!-- -->

``` r
# parametric bootstrap 95%CI: assumes normal distribution:
par.ci.dat <- data.frame(RMSE =apply(boot.dat, 2, mean, na.rm = TRUE),
                       SD = apply(boot.dat, 2, sd, na.rm = TRUE),
                error.type = "Test-bootstrap",
                model.type = c("LM","Elastic", "Tree","RF","GBM")
                      ) %>%
  group_by(model.type) %>%
  summarize( RMSE = RMSE, 
             lower = RMSE - 1.96*SD, 
             upper = RMSE + 1.96*SD,
            error.type = "Test.boot.param" )


# empirical 95%CI: percentile bootstrap CI
plyr::ldply(boot.dat, quantile, c(.025, .975))
```

    ##       .id     2.5%    97.5%
    ## 1      LM 2.422336 3.105772
    ## 2 Elastic 2.379332 3.096791
    ## 3    Tree 2.473934 3.170690
    ## 4      RF 2.378803 3.109788
    ## 5     GBM 2.385917 3.098516

``` r
par.ci.dat %>% 
  ggplot(aes(x = model.type, y = RMSE, color = model.type)) +
  geom_pointrange(aes(ymin = lower, 
                      ymax = upper),
                  position = position_dodge(width = 0.2))
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/mean%20RMSE%20across%20models%20for%20test.dat-1.png)<!-- -->

``` r
test.ci.dat <- data.frame(RMSE = test.results$test.RMSE,
                       SD = apply(boot.dat, 2, sd, na.rm = TRUE),
                error.type = "Test-bootstrap",
                model.type = c("LM","Elastic", "Tree","RF","GBM")
                      ) %>%
  group_by(model.type) %>%
  summarize( RMSE = RMSE, 
             lower = RMSE - 1.96*SD, 
             upper = RMSE + 1.96*SD,
            error.type = "Test.boot.param" )

test.ci.dat %>% 
  ggplot(aes(x = model.type, y = RMSE, color = model.type)) +
  geom_pointrange(aes(ymin = lower, 
                      ymax = upper),
                  position = position_dodge(width = 0.2))
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/plot%20test-set%20error%20and%20error%20bars%20from%20parametric%20bootstrap%20SEs-1.png)<!-- -->

``` r
print(par.ci.dat)
```

    ## # A tibble: 5 × 5
    ##   model.type  RMSE lower upper error.type     
    ##   <chr>      <dbl> <dbl> <dbl> <chr>          
    ## 1 Elastic     2.74  2.38  3.10 Test.boot.param
    ## 2 GBM         2.74  2.38  3.10 Test.boot.param
    ## 3 LM          2.76  2.43  3.10 Test.boot.param
    ## 4 RF          2.74  2.38  3.11 Test.boot.param
    ## 5 Tree        2.82  2.46  3.17 Test.boot.param

``` r
print(test.ci.dat)
```

    ## # A tibble: 5 × 5
    ##   model.type  RMSE lower upper error.type     
    ##   <chr>      <dbl> <dbl> <dbl> <chr>          
    ## 1 Elastic     2.76  2.40  3.12 Test.boot.param
    ## 2 GBM         2.76  2.40  3.12 Test.boot.param
    ## 3 LM          2.78  2.44  3.12 Test.boot.param
    ## 4 RF          2.76  2.39  3.12 Test.boot.param
    ## 5 Tree        2.83  2.48  3.18 Test.boot.param

## Partial-Dependence Plots

``` r
#combined top 3 from all:
  ##paredu
  ##sex
  #goout
  ##reason
  ##age

#first we try paredu, age, sex 
pdp.lm1 <- partial(lm.fit, pred.var = c("paredu", "age", "sex"),
              plot.engine = "ggplot",
              train = train.dat,
              chull = FALSE)

pdp.elastic1 <- partial(elastic.fit2, pred.var = c("paredu", "age", "sex"),
              plot.engine = "ggplot",
              train = train.dat,
              chull = FALSE)

pdp.tree1 <- partial(tree.fit4, pred.var = c("paredu", "age", "sex"),
              plot.engine = "ggplot",
              train = train.dat,
              chull = FALSE)

pdp.rf1 <- partial(rf.fitfinal, pred.var = c("paredu", "age", "sex"),
              plot.engine = "ggplot",
              train = train.dat,
              chull = FALSE)



pdp.gbm1 <- partial(gbm.fit2, pred.var = c("paredu", "age", "sex"),
              plot.engine = "ggplot",
              train = train.dat,
              chull = FALSE)




lm.plot1 <- plotPartial(pdp.lm1, levelplot = FALSE, zlab = "grades", drape = TRUE,
            colorkey = TRUE, 
            # screen = list(z = -20, x = -80),
             main = "Linear Regression"
            )

elastic.plot1 <- plotPartial(pdp.elastic1, levelplot = FALSE, zlab = "grades", drape = TRUE,
            colorkey = TRUE, 
            # screen = list(z = -20, x = -80),
             main = "Regularised Regression"
            )

tree.plot1 <- plotPartial(pdp.tree1, levelplot = FALSE, zlab = "grades", drape = TRUE,
            colorkey = TRUE, 
            # screen = list(z = -20, x = -80),
             main = "Decision Tree"
            )

rf.plot1 <- plotPartial(pdp.rf1, levelplot = FALSE, zlab = "grades", drape = TRUE,
            colorkey = TRUE, 
            # screen = list(z = -20, x = -80),
            main = "Random Forest"
            )

gbm.plot1 <- plotPartial(pdp.gbm1, levelplot = FALSE, zlab = "grades", drape = TRUE,
            colorkey = TRUE, 
            # screen = list(z = -20, x = -80),
             main = "Gradient Boosting"
            )





grid.arrange(lm.plot1, elastic.plot1, tree.plot1, rf.plot1, gbm.plot1, ncol = 2, nrow = 3)
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/PDP1-1.png)<!-- -->

``` r
#now we try paredu, age, reason
pdp.lm2 <- partial(lm.fit, pred.var = c("paredu", "age", "reason"),
              plot.engine = "ggplot",
              train = train.dat,
              chull = FALSE)

pdp.elastic2 <- partial(elastic.fit2, pred.var = c("paredu", "age", "reason"),
              plot.engine = "ggplot",
              train = train.dat,
              chull = FALSE)

pdp.tree2 <- partial(tree.fit4, pred.var = c("paredu", "age", "reason"),
              plot.engine = "ggplot",
              train = train.dat,
              chull = FALSE)

pdp.rf2 <- partial(rf.fitfinal, pred.var = c("paredu", "age", "reason"),
              plot.engine = "ggplot",
              train = train.dat,
              chull = FALSE)

pdp.gbm2 <- partial(gbm.fit2, pred.var = c("paredu", "age", "reason"),
              plot.engine = "ggplot",
              train = train.dat,
              chull = FALSE)




lm.plot2 <- plotPartial(pdp.lm2, levelplot = FALSE, zlab = "grades", drape = TRUE,
            colorkey = TRUE, 
            # screen = list(z = -20, x = -80),
             main = "Linear Regression"
            )

elastic.plot2 <- plotPartial(pdp.elastic2, levelplot = FALSE, zlab = "grades", drape = TRUE,
            colorkey = TRUE, 
            # screen = list(z = -20, x = -80),
             main = "Regularised Regression"
            )

tree.plot2 <- plotPartial(pdp.tree2, levelplot = FALSE, zlab = "grades", drape = TRUE,
            colorkey = TRUE, 
            # screen = list(z = -20, x = -80),
             main = "Decision Tree"
            )

rf.plot2 <- plotPartial(pdp.rf2, levelplot = FALSE, zlab = "grades", drape = TRUE,
            colorkey = TRUE, 
            # screen = list(z = -20, x = -80),
            main = "Random Forest"
            )

gbm.plot2 <- plotPartial(pdp.gbm2, levelplot = FALSE, zlab = "grades", drape = TRUE,
            colorkey = TRUE, 
            # screen = list(z = -20, x = -80),
             main = "Gradient Boosting"
            )


grid.arrange(lm.plot2, elastic.plot2, tree.plot2, rf.plot2, gbm.plot2, ncol = 2, nrow = 3)
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/PDP2-1.png)<!-- -->

``` r
#there are hints of interaction across reason in decision trees
```

``` r
#lastly we try paredu, age, goout
pdp.lm3 <- partial(lm.fit, pred.var = c("paredu", "age", "goout"),
              plot.engine = "ggplot",
              train = train.dat,
              chull = FALSE)

pdp.elastic3 <- partial(elastic.fit2, pred.var = c("paredu", "age", "goout"),
              plot.engine = "ggplot",
              train = train.dat,
              chull = FALSE)

pdp.tree3 <- partial(tree.fit4, pred.var = c("paredu", "age", "goout"),
              plot.engine = "ggplot",
              train = train.dat,
              chull = FALSE)

pdp.rf3 <- partial(rf.fitfinal, pred.var = c("paredu", "age", "goout"),
              plot.engine = "ggplot",
              train = train.dat,
              chull = FALSE)



pdp.gbm3 <- partial(gbm.fit2, pred.var = c("paredu", "age", "goout"),
              plot.engine = "ggplot",
              train = train.dat,
              chull = FALSE)




lm.plot3 <- plotPartial(pdp.lm3, levelplot = FALSE, zlab = "grades", drape = TRUE,
            colorkey = TRUE, 
            # screen = list(z = -20, x = -80),
             main = "Linear Regression"
            )

elastic.plot3 <- plotPartial(pdp.elastic3, levelplot = FALSE, zlab = "grades", drape = TRUE,
            colorkey = TRUE, 
            # screen = list(z = -20, x = -80),
             main = "Regularised Regression"
            )

tree.plot3 <- plotPartial(pdp.tree3, levelplot = FALSE, zlab = "grades", drape = TRUE,
            colorkey = TRUE, 
            # screen = list(z = -20, x = -80),
             main = "Decision Tree"
            )

rf.plot3 <- plotPartial(pdp.rf3, levelplot = FALSE, zlab = "grades", drape = TRUE,
            colorkey = TRUE, 
            # screen = list(z = -20, x = -80),
            main = "Random Forest"
            )

gbm.plot3 <- plotPartial(pdp.gbm3, levelplot = FALSE, zlab = "grades", drape = TRUE,
            colorkey = TRUE, 
            # screen = list(z = -20, x = -80),
             main = "Gradient Boosting"
            )





grid.arrange(lm.plot3, elastic.plot3, tree.plot3, rf.plot3, gbm.plot3, ncol = 2, nrow = 3)
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/PDP3-1.png)<!-- -->

``` r
ggplot(pdp.tree2, aes(x = paredu, y = yhat, color = age)) +
  geom_line(aes(group = as.factor(age) ) ) +
  facet_wrap(~reason)
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/PDP4-1.png)<!-- -->

``` r
ggplot(pdp.gbm1, aes(x = paredu, y = yhat, color = age)) +
  geom_line(aes(group = as.factor(age) ) ) +
  facet_wrap(~sex)
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/PDP5-1.png)<!-- -->

## Additive Effects

``` r
set.seed(my.seed)
lm.add1 <- train(grades ~ . + I(paredu^2), 
             method = "lm",
             trControl = tr.Control,
             data = train.dat,
             metric = "RMSE",
             preProcess = c("center", "scale")
             )



set.seed(my.seed)
lm.add2 <- train(grades ~ . + I(paredu^2) + I(paredu^3), 
             method = "lm",
             trControl = tr.Control,
             data = train.dat,
             metric = "RMSE",
             preProcess = c("center", "scale")
             )



set.seed(my.seed)
lm.add3 <- train(grades ~ . + I(paredu^2) + I(paredu^3) + I(paredu^4), 
             method = "lm",
             trControl = tr.Control,
             data = train.dat,
             metric = "RMSE",
             preProcess = c("center", "scale")
             )



set.seed(my.seed)
lm.add4 <- train(grades ~ . + I(age^2), 
             method = "lm",
             trControl = tr.Control,
             data = train.dat,
             metric = "RMSE",
             preProcess = c("center", "scale")
             )


set.seed(my.seed)
lm.add5 <- train(grades ~ . + I(age^2) + I(age^3), 
             method = "lm",
             trControl = tr.Control,
             data = train.dat,
             metric = "RMSE",
             preProcess = c("center", "scale")
             )


set.seed(my.seed)
lm.add6 <- train(grades ~ . + I(age^2) + I(age^3) +  I(age^4), 
             method = "lm",
             trControl = tr.Control,
             data = train.dat,
             metric = "RMSE",
             preProcess = c("center", "scale")
             )

lm.addresults <- rbind(lm.fit$results, lm.add1$results, lm.add2$results, lm.add3$results, lm.add4$results, lm.add5$results, lm.add6$results)
print(lm.addresults)
```

    ##   intercept     RMSE  Rsquared      MAE    RMSESD RsquaredSD     MAESD
    ## 1      TRUE 2.576139 0.1642073 2.015726 0.2196639 0.08614870 0.1693247
    ## 2      TRUE 2.578394 0.1632450 2.015453 0.2195351 0.08587161 0.1702570
    ## 3      TRUE 2.580669 0.1619890 2.017806 0.2196297 0.08588693 0.1706715
    ## 4      TRUE 2.584801 0.1598574 2.022239 0.2189800 0.08527003 0.1700173
    ## 5      TRUE 2.569060 0.1696336 2.009235 0.2213509 0.08806187 0.1689818
    ## 6      TRUE 2.576036 0.1662082 2.017741 0.2200569 0.08735137 0.1697163
    ## 7      TRUE 2.656088 0.1442942 2.049506 0.2957550 0.09242126 0.1792705

``` r
lm.addresults[which.min(lm.addresults$RMSE),]
```

    ##   intercept    RMSE  Rsquared      MAE    RMSESD RsquaredSD     MAESD
    ## 5      TRUE 2.56906 0.1696336 2.009235 0.2213509 0.08806187 0.1689818

``` r
#we see that lm.add4, with addititive effect of age helped reduce RMSE by a little and increase Rsquared by a little as well. Overall, not a lot of effect. 
```

## Interaction effects

``` r
#no interactions were shown in PDPs but we will try anyway.
set.seed(my.seed)
lm.int1 <- train(grades ~ . + age:reason, 
             method = "lm",
             trControl = tr.Control,
             data = train.dat,
             metric = "RMSE",
             preProcess = c("center", "scale")
             )

set.seed(my.seed)
lm.int2 <- train(grades ~ . + I(age^2) + age:reason, 
             method = "lm",
             trControl = tr.Control,
             data = train.dat,
             metric = "RMSE",
             preProcess = c("center", "scale")
             )



set.seed(my.seed)
lm.int3 <- train(grades ~ . + I(age^2) + I(age^3) + age:reason, 
             method = "lm",
             trControl = tr.Control,
             data = train.dat,
             metric = "RMSE",
             preProcess = c("center", "scale")
             )

set.seed(my.seed)
lm.int4 <- train(grades ~ . + age:sex, 
             method = "lm",
             trControl = tr.Control,
             data = train.dat,
             metric = "RMSE",
             preProcess = c("center", "scale")
             )

set.seed(my.seed)
lm.int5 <- train(grades ~ . + I(age^2) + age:sex, 
             method = "lm",
             trControl = tr.Control,
             data = train.dat,
             metric = "RMSE",
             preProcess = c("center", "scale")
             )

set.seed(my.seed)
lm.int6 <- train(grades ~ . + I(age^2) + I(age^3) + age:sex, 
             method = "lm",
             trControl = tr.Control,
             data = train.dat,
             metric = "RMSE",
             preProcess = c("center", "scale")
             )
set.seed(my.seed)
lm.int7 <- train(grades ~ . + age:paredu, 
             method = "lm",
             trControl = tr.Control,
             data = train.dat,
             metric = "RMSE",
             preProcess = c("center", "scale")
             )

set.seed(my.seed)
lm.int8 <- train(grades ~ . + I(age^2) + age:paredu, 
             method = "lm",
             trControl = tr.Control,
             data = train.dat,
             metric = "RMSE",
             preProcess = c("center", "scale")
             )

set.seed(my.seed)
lm.int9 <- train(grades ~ . + I(age^2) + I(age^3) + age:paredu, 
             method = "lm",
             trControl = tr.Control,
             data = train.dat,
             metric = "RMSE",
             preProcess = c("center", "scale")
             )
set.seed(my.seed)
lm.int10 <- train(grades ~ . + age:goout, 
             method = "lm",
             trControl = tr.Control,
             data = train.dat,
             metric = "RMSE",
             preProcess = c("center", "scale")
             )

set.seed(my.seed)
lm.int11 <- train(grades ~ . + I(age^2) + age:goout, 
             method = "lm",
             trControl = tr.Control,
             data = train.dat,
             metric = "RMSE",
             preProcess = c("center", "scale")
             )

set.seed(my.seed)
lm.int12 <- train(grades ~ . + I(age^2) + I(age^3) + age:goout, 
             method = "lm",
             trControl = tr.Control,
             data = train.dat,
             metric = "RMSE",
             preProcess = c("center", "scale")
             )




lm.intresults <- rbind(lm.fit$results, lm.int1$results, lm.int2$results, lm.int3$results, lm.int4$results, lm.int5$results, lm.int6$results, lm.int7$results, lm.int8$results, lm.int9$results, lm.int10$results, lm.int11$results, lm.int12$results )
print(lm.intresults)
```

    ##    intercept     RMSE  Rsquared      MAE    RMSESD RsquaredSD     MAESD
    ## 1       TRUE 2.576139 0.1642073 2.015726 0.2196639 0.08614870 0.1693247
    ## 2       TRUE 2.578179 0.1642603 2.025673 0.2102657 0.08328293 0.1573871
    ## 3       TRUE 2.574482 0.1685998 2.015499 0.2140838 0.08557726 0.1625302
    ## 4       TRUE 2.586206 0.1620217 2.025086 0.2117536 0.08447715 0.1632040
    ## 5       TRUE 2.576998 0.1645093 2.023278 0.2229984 0.08769018 0.1743435
    ## 6       TRUE 2.571421 0.1690507 2.014922 0.2244917 0.08889643 0.1739584
    ## 7       TRUE 2.577460 0.1662290 2.022782 0.2222795 0.08856331 0.1741426
    ## 8       TRUE 2.577878 0.1637266 2.017023 0.2206085 0.08699967 0.1676820
    ## 9       TRUE 2.574063 0.1672966 2.012224 0.2216906 0.08824220 0.1681112
    ## 10      TRUE 2.582554 0.1630163 2.021803 0.2197465 0.08695132 0.1692731
    ## 11      TRUE 2.586052 0.1605505 2.023655 0.2165140 0.08405205 0.1644683
    ## 12      TRUE 2.580254 0.1652532 2.017811 0.2186990 0.08456856 0.1608782
    ## 13      TRUE 2.593660 0.1583121 2.030608 0.2169911 0.08325163 0.1635244

``` r
lm.intresults[which.min(lm.intresults$RMSE),]
```

    ##   intercept     RMSE  Rsquared      MAE    RMSESD RsquaredSD     MAESD
    ## 6      TRUE 2.571421 0.1690507 2.014922 0.2244917 0.08889643 0.1739584

``` r
#we see that lm.int5 with additive age and interaction between age and sex helped decrease RMSE and increase Rsquared by a little, but still not so much. This is also higher than the RMSE of the additive model from before.  
#since the improvement is minimal, we will not use it for further analyses. 
```

``` r
# get predictions
res.dat <- data.frame(test.y = test.dat$grades,
                      lm.pred, 
                      elastic.pred,
                      tree.pred,
                      rf.pred, 
                      gbm.pred)


# get residuals
res.dat <- res.dat %>% 
            mutate(
            resid.lm = test.y - lm.pred,
            resid.elastic = test.y - elastic.pred,
            resid.tree= test.y - tree.pred,
            resid.rf = test.y - rf.pred,
            resid.gbm = test.y - gbm.pred
            )
```

``` r
#transform to long format 
#first get a residuals only dataset, then merge with a predictions only dataset:

# 1. get residuals only data:
residuals.long <- res.dat %>% 
  select(resid.lm, resid.elastic, resid.tree, resid.rf, resid.gbm, test.y) %>% 
  pivot_longer(cols = !test.y, 
               names_to = "method", 
               values_to = "residuals")

#clean up the labels in "method"
residuals.long <- 
  residuals.long %>% 
  mutate(method = case_when(method == "resid.lm" ~ "Linear",
                            method == "resid.elastic" ~ "Elastic",
                            method == "resid.tree" ~ "Tree",
                            method == "resid.rf" ~ "RF",
                            method == "resid.gbm" ~ "GBM")
         )%>% 
  mutate(ID = 1:nrow(residuals.long))

# 2.get predictions-only data:
predictions.long <- res.dat %>% 
  select(lm.pred, elastic.pred, tree.pred, rf.pred,gbm.pred,test.y) %>% 
  pivot_longer(cols = !test.y, 
               names_to = "method", 
               values_to = "predictions")

#clean up the labels in "method"
predictions.long <- 
  predictions.long %>% 
  mutate(method = case_when(method == "lm.pred" ~ "Linear",
                            method == "elastic.pred" ~ "Elastic",
                            method == "tree.pred" ~ "Tree",
                            method == "rf.pred" ~ "RF",
                            method == "gbm.pred" ~ "GBM")
         ) %>% 
  mutate(ID = 1:nrow(predictions.long))

# 3. merge both to get the diagnostics dataset:
diag.dat <- merge(predictions.long, residuals.long)
head(diag.dat)
```

    ##     test.y  method  ID predictions  residuals
    ## 1 1.666667 Elastic 867   12.631518 -10.964852
    ## 2 1.666667 Elastic 877    9.590935  -7.924269
    ## 3 1.666667     GBM 870   13.007687 -11.341020
    ## 4 1.666667     GBM 880    8.566205  -6.899539
    ## 5 1.666667  Linear 866   12.043743 -10.377076
    ## 6 1.666667  Linear 876    9.493757  -7.827090

``` r
ggplot(diag.dat, aes(x = predictions, y = residuals)) +
  geom_point(alpha = 0.2) +
  geom_hline(yintercept = 0, col = "red", lty = 2) +
  geom_smooth(se = FALSE, lty = 1) +
  facet_wrap(~method) +
  theme_minimal()
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : pseudoinverse used at 12.622

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : neighborhood radius 1.6218

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : reciprocal condition number 1.6447e-16

    ## Warning in simpleLoess(y, x, w, span, degree = degree, parametric = parametric,
    ## : There are other near singularities as well. 2.5722

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/plot%20residuals%20vs%20predictions-1.png)<!-- -->

``` r
ggplot(diag.dat, aes(x = test.y, y = predictions)) +
  geom_point(alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1, col = "red", lty = 2) +
  geom_smooth(se = FALSE, lty = 1) +
  facet_wrap(~method) +
  theme_minimal()
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/plotpredictions%20vs%20test%20data-1.png)<!-- -->

``` r
dv = "grades"
pred.vars <- names(sac.dat)[!names(sac.dat)%in% dv]
explainer.dat <- test.dat

# build explainer for linear regression:
lm.exp <- DALEX::explain(model = lm.fit, 
                  data = explainer.dat[ , pred.vars],
                  y = explainer.dat[ , dv], 
                  label = "Linear Regression",
                  type = "regression")
```

    ## Preparation of a new explainer is initiated
    ##   -> model label       :  Linear Regression 
    ##   -> data              :  192  rows  10  cols 
    ##   -> target variable   :  192  values 
    ##   -> predict function  :  yhat.train  will be used (  default  )
    ##   -> predicted values  :  No value for predict function target column. (  default  )
    ##   -> model_info        :  package caret , ver. 7.0.1 , task regression (  default  ) 
    ##   -> model_info        :  type set to  regression 
    ##   -> predicted values  :  numerical, min =  8.445585 , mean =  11.64295 , max =  15.1418  
    ##   -> residual function :  difference between y and yhat (  default  )
    ##   -> residuals         :  numerical, min =  -10.37708 , mean =  -0.1099685 , max =  7.464469  
    ##   A new explainer has been created!

``` r
# build explainer for elastic net:
elastic.exp <- DALEX::explain(model = elastic.fit2, 
                       data = explainer.dat[ , pred.vars], 
                       y = explainer.dat[, dv], 
                       label = "Elastic",
                       type = "regression")
```

    ## Preparation of a new explainer is initiated
    ##   -> model label       :  Elastic 
    ##   -> data              :  192  rows  10  cols 
    ##   -> target variable   :  192  values 
    ##   -> predict function  :  yhat.train  will be used (  default  )
    ##   -> predicted values  :  No value for predict function target column. (  default  )
    ##   -> model_info        :  package caret , ver. 7.0.1 , task regression (  default  ) 
    ##   -> model_info        :  type set to  regression 
    ##   -> predicted values  :  numerical, min =  9.225666 , mean =  11.65599 , max =  14.47265  
    ##   -> residual function :  difference between y and yhat (  default  )
    ##   -> residuals         :  numerical, min =  -10.96485 , mean =  -0.1230071 , max =  7.016845  
    ##   A new explainer has been created!

``` r
# build explainer for tree:
tree.exp <- DALEX::explain(model = tree.fit4, 
                    data = explainer.dat[ , pred.vars], 
                    y = explainer.dat[, dv], 
                    label = "Tree",
                    type = "regression")
```

    ## Preparation of a new explainer is initiated
    ##   -> model label       :  Tree 
    ##   -> data              :  192  rows  10  cols 
    ##   -> target variable   :  192  values 
    ##   -> predict function  :  yhat.train  will be used (  default  )
    ##   -> predicted values  :  No value for predict function target column. (  default  )
    ##   -> model_info        :  package caret , ver. 7.0.1 , task regression (  default  ) 
    ##   -> model_info        :  type set to  regression 
    ##   -> predicted values  :  numerical, min =  9.013889 , mean =  11.70908 , max =  12.60381  
    ##   -> residual function :  difference between y and yhat (  default  )
    ##   -> residuals         :  numerical, min =  -10.93714 , mean =  -0.1760946 , max =  6  
    ##   A new explainer has been created!

``` r
# build explainer for RF:
rf.exp <- DALEX::explain(model = rf.fitfinal, 
                  data = explainer.dat[ , pred.vars], 
                  y = explainer.dat[, dv], 
                  label = "Random Forest",
                  type = "regression")
```

    ## Preparation of a new explainer is initiated
    ##   -> model label       :  Random Forest 
    ##   -> data              :  192  rows  10  cols 
    ##   -> target variable   :  192  values 
    ##   -> predict function  :  yhat.train  will be used (  default  )
    ##   -> predicted values  :  No value for predict function target column. (  default  )
    ##   -> model_info        :  package caret , ver. 7.0.1 , task regression (  default  ) 
    ##   -> model_info        :  type set to  regression 
    ##   -> predicted values  :  numerical, min =  9.453839 , mean =  11.67044 , max =  13.24907  
    ##   -> residual function :  difference between y and yhat (  default  )
    ##   -> residuals         :  numerical, min =  -11.15196 , mean =  -0.1374501 , max =  5.935747  
    ##   A new explainer has been created!

``` r
# build explainer for GBM:
gbm.exp <- DALEX::explain(model = gbm.fit2, 
                   data = explainer.dat[ , pred.vars], 
                   y= explainer.dat[, dv], 
                   label = "GBM",
                   type = "regression")
```

    ## Preparation of a new explainer is initiated
    ##   -> model label       :  GBM 
    ##   -> data              :  192  rows  10  cols 
    ##   -> target variable   :  192  values 
    ##   -> predict function  :  yhat.train  will be used (  default  )
    ##   -> predicted values  :  No value for predict function target column. (  default  )
    ##   -> model_info        :  package caret , ver. 7.0.1 , task regression (  default  ) 
    ##   -> model_info        :  type set to  regression 
    ##   -> predicted values  :  numerical, min =  8.256733 , mean =  11.62835 , max =  14.57703  
    ##   -> residual function :  difference between y and yhat (  default  )
    ##   -> residuals         :  numerical, min =  -11.34102 , mean =  -0.09536118 , max =  7.176075  
    ##   A new explainer has been created!

``` r
set.seed(my.seed)
vip_lm  <- model_parts(explainer = lm.exp,  B = 50, N = NULL)
set.seed(my.seed)
vip_elastic  <- model_parts(explainer = elastic.exp,  B = 50, N = NULL)
set.seed(my.seed)
vip_tree  <- model_parts(explainer = tree.exp,  B = 50, N = NULL)
set.seed(my.seed)
vip_rf  <- model_parts(explainer = rf.exp,  B = 50, N = NULL)
set.seed(my.seed)
vip_gbm <- model_parts(explainer = gbm.exp, B = 50, N = NULL)
```

``` r
plot(vip_lm, vip_elastic,vip_tree, vip_rf, vip_gbm) +
  ggtitle("Mean variable-importance over 50 permutations", "") 
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

## Create Partial-Dependence Profiles

``` r
pdp_lm.age <- model_profile(explainer = lm.exp, variables = "age")
pdp_elastic.age <- model_profile(explainer = elastic.exp, variables = "age")
pdp_tree.age <- model_profile(explainer = tree.exp, variables = "age")
pdp_rf.age <- model_profile(explainer = rf.exp, variables = "age")
pdp_gbm.age <- model_profile(explainer = gbm.exp, variables = "age")


plot(pdp_lm.age, pdp_elastic.age, pdp_tree.age, pdp_rf.age, pdp_gbm.age) +  
  ggtitle("Partial-dependence profile for age") 
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/PDP%20for%20age-1.png)<!-- -->

``` r
pdp_lm.paredu <- model_profile(explainer = lm.exp, variables = "paredu")
pdp_elastic.paredu <- model_profile(explainer = elastic.exp, variables = "paredu")
pdp_tree.paredu <- model_profile(explainer = tree.exp, variables = "paredu")
pdp_rf.paredu <- model_profile(explainer = rf.exp, variables = "paredu")
pdp_gbm.paredu <- model_profile(explainer = gbm.exp, variables = "paredu")


plot(pdp_lm.paredu, pdp_elastic.paredu, pdp_tree.paredu, pdp_rf.paredu, pdp_gbm.paredu) +  
  ggtitle("Partial-dependence profile for paredu") 
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/PDP%20for%20paredu-1.png)<!-- -->

## Create Ceteris-Paribus Profiles

``` r
plot(pdp_lm.paredu, geom = "profiles") + 
    ggtitle("Ceteris-paribus and partial-dependence profiles for paredu (lm)")
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/cp%20for%20paredu-1.png)<!-- -->

``` r
plot(pdp_elastic.paredu, geom = "profiles") + 
    ggtitle("Ceteris-paribus and partial-dependence profiles for paredu (elastic)")
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/cp%20for%20paredu-2.png)<!-- -->

``` r
plot(pdp_tree.paredu, geom = "profiles") + 
    ggtitle("Ceteris-paribus and partial-dependence profiles for paredu (tree)")
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/cp%20for%20paredu-3.png)<!-- -->

``` r
plot(pdp_rf.paredu, geom = "profiles") + 
    ggtitle("Ceteris-paribus and partial-dependence profiles for paredu (rf)") 
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/cp%20for%20paredu-4.png)<!-- -->

``` r
plot(pdp_gbm.paredu, geom = "profiles") + 
    ggtitle("Ceteris-paribus and partial-dependence profiles for paredu (gbm)") 
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/cp%20for%20paredu-5.png)<!-- -->

``` r
plot(pdp_lm.age, geom = "profiles") + 
    ggtitle("Ceteris-paribus and partial-dependence profiles for age (lm)") 
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/cp%20for%20age-1.png)<!-- -->

``` r
plot(pdp_elastic.age, geom = "profiles") + 
    ggtitle("Ceteris-paribus and partial-dependence profiles for age (elastic)")
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/cp%20for%20age-2.png)<!-- -->

``` r
plot(pdp_tree.age, geom = "profiles") + 
    ggtitle("Ceteris-paribus and partial-dependence profiles for age (tree)") 
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/cp%20for%20age-3.png)<!-- -->

``` r
plot(pdp_rf.age, geom = "profiles") + 
    ggtitle("Ceteris-paribus and partial-dependence profiles for age (rf)") 
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/cp%20for%20age-4.png)<!-- -->

``` r
plot(pdp_gbm.age, geom = "profiles") + 
    ggtitle("Ceteris-paribus and partial-dependence profiles for age (gbm)")
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/cp%20for%20age-5.png)<!-- -->

# Part 5: Model Prediction Test

``` r
nesh.dat <- data.frame(
  sex = "Male",
  age = 17,
  paredu = 7,
  famsup = "yes",
  activities = "no",
  romantic = "yes",
  freetime = "1",
  health = "4",
  goout = "1",
  reason = "home"
  )


predict(lm.fit, newdata = nesh.dat)
```

    ##        1 
    ## 10.19554

``` r
predict(elastic.fit2, newdata = nesh.dat)
```

    ## [1] 11.24229

``` r
predict(tree.fit4, newdata = nesh.dat)
```

    ##        1 
    ## 12.60381

``` r
predict(rf.fitfinal, newdata = nesh.dat)
```

    ##       1 
    ## 12.0188

``` r
predict(gbm.fit2, newdata = nesh.dat)
```

    ## [1] 11.74516

``` r
cp.lm <- predict_profile(explainer = lm.exp, 
                           new_observation = nesh.dat)


cp.elastic <- predict_profile(explainer = elastic.exp, 
                           new_observation = nesh.dat)

cp.tree <- predict_profile(explainer = tree.exp, 
                           new_observation = nesh.dat)

cp.rf <- predict_profile(explainer = rf.exp, 
                           new_observation = nesh.dat)

cp.gbm <- predict_profile(explainer = gbm.exp, 
                           new_observation = nesh.dat)
cp.lm
```

    ## Top profiles    : 
    ##         sex age paredu famsup activities romantic freetime health goout reason
    ## 1    Female  17      7    yes         no      yes        1      4     1   home
    ## 1.1    Male  17      7    yes         no      yes        1      4     1   home
    ## 11     Male  15      7    yes         no      yes        1      4     1   home
    ## 1.11   Male  16      7    yes         no      yes        1      4     1   home
    ## 1.2    Male  17      7    yes         no      yes        1      4     1   home
    ## 1.3    Male  18      7    yes         no      yes        1      4     1   home
    ##         _yhat_ _vname_ _ids_           _label_
    ## 1    11.386205     sex     1 Linear Regression
    ## 1.1  10.195544     sex     1 Linear Regression
    ## 11   10.624303     age     1 Linear Regression
    ## 1.11 10.409924     age     1 Linear Regression
    ## 1.2  10.195544     age     1 Linear Regression
    ## 1.3   9.981165     age     1 Linear Regression
    ## 
    ## 
    ## Top observations:
    ##    sex age paredu famsup activities romantic freetime health goout reason
    ## 1 Male  17      7    yes         no      yes        1      4     1   home
    ##     _yhat_           _label_ _ids_
    ## 1 10.19554 Linear Regression     1

``` r
cp.elastic
```

    ## Top profiles    : 
    ##         sex age paredu famsup activities romantic freetime health goout reason
    ## 1    Female  17      7    yes         no      yes        1      4     1   home
    ## 1.1    Male  17      7    yes         no      yes        1      4     1   home
    ## 11     Male  15      7    yes         no      yes        1      4     1   home
    ## 1.11   Male  16      7    yes         no      yes        1      4     1   home
    ## 1.2    Male  17      7    yes         no      yes        1      4     1   home
    ## 1.3    Male  18      7    yes         no      yes        1      4     1   home
    ##        _yhat_ _vname_ _ids_ _label_
    ## 1    12.14735     sex     1 Elastic
    ## 1.1  11.24229     sex     1 Elastic
    ## 11   11.58860     age     1 Elastic
    ## 1.11 11.41544     age     1 Elastic
    ## 1.2  11.24229     age     1 Elastic
    ## 1.3  11.06914     age     1 Elastic
    ## 
    ## 
    ## Top observations:
    ##    sex age paredu famsup activities romantic freetime health goout reason
    ## 1 Male  17      7    yes         no      yes        1      4     1   home
    ##     _yhat_ _label_ _ids_
    ## 1 11.24229 Elastic     1

``` r
cp.tree
```

    ## Top profiles    : 
    ##         sex age paredu famsup activities romantic freetime health goout reason
    ## 1    Female  17      7    yes         no      yes        1      4     1   home
    ## 1.1    Male  17      7    yes         no      yes        1      4     1   home
    ## 11     Male  15      7    yes         no      yes        1      4     1   home
    ## 1.11   Male  16      7    yes         no      yes        1      4     1   home
    ## 1.2    Male  17      7    yes         no      yes        1      4     1   home
    ## 1.3    Male  18      7    yes         no      yes        1      4     1   home
    ##        _yhat_ _vname_ _ids_ _label_
    ## 1    12.60381     sex     1    Tree
    ## 1.1  12.60381     sex     1    Tree
    ## 11   12.60381     age     1    Tree
    ## 1.11 12.60381     age     1    Tree
    ## 1.2  12.60381     age     1    Tree
    ## 1.3  12.60381     age     1    Tree
    ## 
    ## 
    ## Top observations:
    ##    sex age paredu famsup activities romantic freetime health goout reason
    ## 1 Male  17      7    yes         no      yes        1      4     1   home
    ##     _yhat_ _label_ _ids_
    ## 1 12.60381    Tree     1

``` r
cp.rf
```

    ## Top profiles    : 
    ##         sex age paredu famsup activities romantic freetime health goout reason
    ## 1    Female  17      7    yes         no      yes        1      4     1   home
    ## 1.1    Male  17      7    yes         no      yes        1      4     1   home
    ## 11     Male  15      7    yes         no      yes        1      4     1   home
    ## 1.11   Male  16      7    yes         no      yes        1      4     1   home
    ## 1.2    Male  17      7    yes         no      yes        1      4     1   home
    ## 1.3    Male  18      7    yes         no      yes        1      4     1   home
    ##        _yhat_ _vname_ _ids_       _label_
    ## 1    12.74237     sex     1 Random Forest
    ## 1.1  12.01880     sex     1 Random Forest
    ## 11   12.03692     age     1 Random Forest
    ## 1.11 12.01396     age     1 Random Forest
    ## 1.2  12.01880     age     1 Random Forest
    ## 1.3  11.93738     age     1 Random Forest
    ## 
    ## 
    ## Top observations:
    ##    sex age paredu famsup activities romantic freetime health goout reason
    ## 1 Male  17      7    yes         no      yes        1      4     1   home
    ##    _yhat_       _label_ _ids_
    ## 1 12.0188 Random Forest     1

``` r
cp.gbm
```

    ## Top profiles    : 
    ##         sex age paredu famsup activities romantic freetime health goout reason
    ## 1    Female  17      7    yes         no      yes        1      4     1   home
    ## 1.1    Male  17      7    yes         no      yes        1      4     1   home
    ## 11     Male  15      7    yes         no      yes        1      4     1   home
    ## 1.11   Male  16      7    yes         no      yes        1      4     1   home
    ## 1.2    Male  17      7    yes         no      yes        1      4     1   home
    ## 1.3    Male  18      7    yes         no      yes        1      4     1   home
    ##        _yhat_ _vname_ _ids_ _label_
    ## 1    12.63389     sex     1     GBM
    ## 1.1  11.74516     sex     1     GBM
    ## 11   11.73456     age     1     GBM
    ## 1.11 11.74402     age     1     GBM
    ## 1.2  11.74516     age     1     GBM
    ## 1.3  11.40870     age     1     GBM
    ## 
    ## 
    ## Top observations:
    ##    sex age paredu famsup activities romantic freetime health goout reason
    ## 1 Male  17      7    yes         no      yes        1      4     1   home
    ##     _yhat_ _label_ _ids_
    ## 1 11.74516     GBM     1

``` r
plot(cp.lm, variables = c("age", "paredu")) +
  ggtitle("Ceteris-paribus profile", "")
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/cp%20profiles%20plot-1.png)<!-- -->

``` r
plot(cp.elastic, variables = c("age", "paredu")) +
  ggtitle("Ceteris-paribus profile", "")
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/cp%20profiles%20plot-2.png)<!-- -->

``` r
plot(cp.tree, variables = c("age", "paredu")) +
  ggtitle("Ceteris-paribus profile", "")
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/cp%20profiles%20plot-3.png)<!-- -->

``` r
plot(cp.rf, variables = c("age", "paredu")) +
  ggtitle("Ceteris-paribus profile", "")
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/cp%20profiles%20plot-4.png)<!-- -->

``` r
plot(cp.gbm, variables = c("age", "paredu")) +
  ggtitle("Ceteris-paribus profile", "")
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/cp%20profiles%20plot-5.png)<!-- -->

``` r
plot(cp.lm, cp.elastic, cp.tree, cp.rf, cp.gbm,color = "_label_",  
     variables = c("age", "paredu")) +
     ggtitle("Ceteris-paribus profiles for Nesh", "") 
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/combined%20cp%20profiles%20plot-1.png)<!-- -->

``` r
nesh.dat.1 <- data.frame(
  sex = "Male",
  age = 17,
  paredu = 7,
  famsup = "yes",
  activities = "no",
  romantic = "yes",
  freetime = "1",
  health = "4",
  goout = "1",
  reason = "home"
  )

nesh.dat.2 <- data.frame(
  sex = "Male",
  age = 17,
  paredu = 7,
  famsup = "yes",
  activities = "no",
  romantic = "yes",
  freetime = "1",
  health = "4",
  goout = "1",
  reason = "reputation"
  )




cp.elastic1 <- predict_profile(explainer = elastic.exp, 
                         new_observation = rbind(nesh.dat.1,
                                                 nesh.dat.2))
plot(cp.elastic1 , color = "_ids_",
     variables = c("age", "paredu")) +
    scale_color_manual(name = "Nesh:", breaks = 1:2, 
            values = c("red", "blue"), 
            labels = c("home" , "reputation")) +
  ggtitle("Ceteris-paribus profile elastic", "")
```

    ## Scale for colour is already present.
    ## Adding another scale for colour, which will replace the existing scale.

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/compare%20individual-1.png)<!-- -->

``` r
cp.gbm1 <- predict_profile(explainer = gbm.exp, 
                         new_observation = rbind(nesh.dat.1,
                                                 nesh.dat.2))
plot(cp.gbm1 , color = "_ids_",
     variables = c("age", "paredu")) +
    scale_color_manual(name = "Nesh:", breaks = 1:2, 
            values = c("red", "blue"), 
            labels = c("home" , "reputation")) +
  ggtitle("Ceteris-paribus profile gbm", "")
```

    ## Scale for colour is already present.
    ## Adding another scale for colour, which will replace the existing scale.

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/compare%20individual-2.png)<!-- -->

``` r
bd.elastic <- predict_parts(explainer = elastic.exp, 
                       new_observation = nesh.dat,
                       type = "break_down")

bd.elastic
```

    ##                          contribution
    ## Elastic: intercept             11.656
    ## Elastic: paredu = 7             0.730
    ## Elastic: sex = Male            -0.547
    ## Elastic: goout = 1             -0.314
    ## Elastic: romantic = yes        -0.167
    ## Elastic: health = 4             0.156
    ## Elastic: famsup = yes          -0.148
    ## Elastic: activities = no       -0.073
    ## Elastic: age = 17              -0.041
    ## Elastic: reason = home         -0.039
    ## Elastic: freetime = 1           0.029
    ## Elastic: prediction            11.242

``` r
plot(bd.elastic)
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/breakdown%20plots%20elastic-1.png)<!-- -->

``` r
bd.gbm<- predict_parts(explainer = gbm.exp, 
                       new_observation = nesh.dat,
                       type = "break_down")

bd.gbm
```

    ##                      contribution
    ## GBM: intercept             11.628
    ## GBM: paredu = 7             0.852
    ## GBM: sex = Male            -0.537
    ## GBM: goout = 1             -0.215
    ## GBM: health = 4             0.196
    ## GBM: age = 17               0.191
    ## GBM: romantic = yes        -0.142
    ## GBM: famsup = yes          -0.139
    ## GBM: reason = home         -0.104
    ## GBM: freetime = 1           0.073
    ## GBM: activities = no       -0.059
    ## GBM: prediction            11.745

``` r
plot(bd.gbm)
```

![](BhuvaneshWadhwani_A0199148M_FinalProject_files/figure-gfm/breakdown%20plots%20gbm-1.png)<!-- -->
