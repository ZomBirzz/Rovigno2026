# R 4.6.1 started.
# 
# R version 4.6.1 (2026-06-24) -- "Happy Hop"
# Copyright (C) 2026 The R Foundation for Statistical Computing
# Platform: x86_64-pc-linux-gnu
# 
# R is free software and comes with ABSOLUTELY NO WARRANTY.
# You are welcome to redistribute it under certain conditions.
# Type 'license()' or 'licence()' for distribution details.
# 
#   Natural language support but running in an English locale
# 
# R is a collaborative project with many contributors.
# Type 'contributors()' for more information and
# 'citation()' on how to cite R or R packages in publications.
# 
# Type 'demo()' for some demos, 'help()' for on-line help, or
# 'help.start()' for an HTML browser interface to help.
# Type 'q()' to quit R.
# 
rm(list=ls(all=TRUE))
library(GAD)
dataset <- read.csv(file.choose(),header=T,sep=",", stringsAsFactors = T)
attach(dataset)
summary(dataset)
# Enter file name: 
#       Anno             Sperimentatore    GRUPPO.1 SITE      REPLICA           S         
#  Min.   :2022   Riccardo Brusa: 4     Min.   :1   A:30   Min.   : 1.0   Min.   : 4.000  
#  1st Qu.:2022   Vincenzo      : 4     1st Qu.:1   B:30   1st Qu.: 8.0   1st Qu.: 6.000  
#  Median :2022   Alberto       : 3     Median :2   C:30   Median :15.5   Median : 7.000  
#  Mean   :2022   Althea        : 3     Mean   :2          Mean   :15.5   Mean   : 7.289  
#  3rd Qu.:2022   Beatrice      : 3     3rd Qu.:3          3rd Qu.:23.0   3rd Qu.: 8.000  
#  Max.   :2022   Enrico        : 3     Max.   :3          Max.   :30.0   Max.   :25.000  
#                 (Other)       :70                                                       
boxplot(S~Anno+Sito+Anno*Sito) #why dafuq non ci sono i quadrati?
# Error:
# object 'Sito' not found
# 1. boxplot(S ~ Anno + Sito + Anno * Sito) at Rovigno/ANOVA/anno%202022.R:7:1
# 2. boxplot.formula(S ~ Anno + Sito + Anno * Sito)
# 3. eval(m, parent.frame())
# 4. eval(m, parent.frame())
# 5. stats::model.frame.default(formula = S ~ Anno + Sito + Anno * 
#        Sito)
# 6. eval(predvars, data, env)
# 7. eval(predvars, data, env)
boxplot(S~Anno+Site+Anno*Site) #why dafuq non ci sono i quadrati?
# Error:
# object 'Site' not found
# 1. boxplot(S ~ Anno + Site + Anno * Site) at Rovigno/ANOVA/anno%202022.R:7:1
# 2. boxplot.formula(S ~ Anno + Site + Anno * Site)
# 3. eval(m, parent.frame())
# 4. eval(m, parent.frame())
# 5. stats::model.frame.default(formula = S ~ Anno + Site + Anno * 
#        Site)
# 6. eval(predvars, data, env)
# 7. eval(predvars, data, env)
boxplot(S~Anno+SITE+Anno*SITE) #why dafuq non ci sono i quadrati?

Anno <- as.fixed(Anno)
Sito <- as.random(SITE)
linear.model=lm(S~Anno+Sito+Anno*Sito)
# Error:
# contrasts can be applied only to factors with 2 or more levels
# 1. lm(S ~ Anno + Sito + Anno * Sito) at Rovigno/ANOVA/anno%202022.R:12:1
# 2. model.matrix(mt, mf, contrasts)
# 3. model.matrix.default(mt, mf, contrasts)
# 4. `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]])
# 5. stop("contrasts can be applied only to factors with 2 or more levels")
detach(dataset)
rm(list=ls(all=TRUE))
rm(list=ls(all=TRUE))
library(GAD)
dataset <- read.csv(file.choose(),header=T,sep=",", stringsAsFactors = T)
attach(dataset)
summary(dataset)
# Enter file name: 
#       ANNO                 Sperimentatore     GRUPPO  SITE      REPLICA           S         
#  Min.   :2022   Chiara Frigieri   :  4    Min.   :1   A:60   Min.   : 1.0   Min.   : 3.000  
#  1st Qu.:2022   Riccardo Brusa    :  4    1st Qu.:1   B:60   1st Qu.: 8.0   1st Qu.: 5.000  
#  Median :2022   Vincenzo          :  4    Median :2   C:60   Median :16.5   Median : 6.000  
#  Mean   :2022   Alberto           :  3    Mean   :2          Mean   :16.8   Mean   : 6.489  
#  3rd Qu.:2023   Alessandro Nicolai:  3    3rd Qu.:3          3rd Qu.:25.0   3rd Qu.: 8.000  
#  Max.   :2023   Alex Baggio Morano:  3    Max.   :3          Max.   :36.0   Max.   :25.000  
#                 (Other)           :159                                                      
boxplot(S~Anno+SITE+Anno*SITE) #why dafuq non ci sono i quadrati?
# Error:
# object 'Anno' not found
# 1. boxplot(S ~ Anno + SITE + Anno * SITE) at Rovigno/ANOVA/bruttobrutto.R:7:1
# 2. boxplot.formula(S ~ Anno + SITE + Anno * SITE)
# 3. eval(m, parent.frame())
# 4. eval(m, parent.frame())
# 5. stats::model.frame.default(formula = S ~ Anno + SITE + Anno * 
#        SITE)
# 6. eval(predvars, data, env)
# 7. eval(predvars, data, env)
boxplot(S~ANNO+SITE) #why dafuq non ci sono i quadrati?
Anno <- as.fixed(ANNO)
Sito <- as.random(SITE)
linear.model=lm(S~Anno+Sito+Anno*Sito)
C.test(linear.model) # C = NA ???????
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.50428, n = 30, k = 6, p-value = 1.139e-10
# alternative hypothesis: Group 2022.B has outlying variance
# sample estimates:
#  2022.A  2022.B  2022.C  2023.A  2023.B  2023.C 
#  2.4379 13.1264  2.4379  2.6023  3.0299  2.3954 
S = radq(S)
linear.model=lm(S~Anno+Sito+Anno*Sito)
C.test(linear.model)
# Error:
# could not find function "radq"
S = sqrt(S)
linear.model=lm(S~Anno+Sito+Anno*Sito)
C.test(linear.model)
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.34221, n = 30, k = 6, p-value = 0.0006092
# alternative hypothesis: Group 2022.B has outlying variance
# sample estimates:
# 2022.A 2022.B 2022.C 2023.A 2023.B 2023.C 
# 0.0909 0.2751 0.0909 0.1265 0.1214 0.0991 
S = sqrt(S)
linear.model=lm(S~Anno+Sito+Anno*Sito)
C.test(linear.model)
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.2809, n = 30, k = 6, p-value = 0.03211
# alternative hypothesis: Group 2022.B has outlying variance
# sample estimates:
# 2022.A 2022.B 2022.C 2023.A 2023.B 2023.C 
# 0.0089 0.0213 0.0089 0.0142 0.0123 0.0103 
S = sqrt(S)
linear.model=lm(S~Anno+Sito+Anno*Sito)
C.test(linear.model)
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.25528, n = 30, k = 6, p-value = 0.1239
# alternative hypothesis: Group 2022.B has outlying variance
# sample estimates:
# 2022.A 2022.B 2022.C 2023.A 2023.B 2023.C 
# 0.0014 0.0030 0.0014 0.0024 0.0020 0.0017 
S = sqrt(S)
linear.model=lm(S~Anno+Sito+Anno*Sito)
C.test(linear.model)
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.24364, n = 30, k = 6, p-value = 0.215
# alternative hypothesis: Group 2022.B has outlying variance
# sample estimates:
# 2022.A 2022.B 2022.C 2023.A 2023.B 2023.C 
#  3e-04  6e-04  3e-04  5e-04  4e-04  3e-04 
S = sqrt(S)
linear.model=lm(S~Anno+Sito+Anno*Sito)
C.test(linear.model)
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.23809, n = 30, k = 6, p-value = 0.2757
# alternative hypothesis: Group 2022.B has outlying variance
# sample estimates:
# 2022.A 2022.B 2022.C 2023.A 2023.B 2023.C 
#  1e-04  1e-04  1e-04  1e-04  1e-04  1e-04 
S = sqrt(S)
linear.model=lm(S~Anno+Sito+Anno*Sito)
C.test(linear.model)
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.23538, n = 30, k = 6, p-value = 0.3102
# alternative hypothesis: Group 2022.B has outlying variance
# sample estimates:
# 2022.A 2022.B 2022.C 2023.A 2023.B 2023.C 
#      0      0      0      0      0      0 
S = sqrt(S)
linear.model=lm(S~Anno+Sito+Anno*Sito)
C.test(linear.model)
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.23404, n = 30, k = 6, p-value = 0.3286
# alternative hypothesis: Group 2022.B has outlying variance
# sample estimates:
# 2022.A 2022.B 2022.C 2023.A 2023.B 2023.C 
#      0      0      0      0      0      0 
S = sqrt(S)
linear.model=lm(S~Anno+Sito+Anno*Sito)
C.test(linear.model)
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.23337, n = 30, k = 6, p-value = 0.338
# alternative hypothesis: Group 2022.B has outlying variance
# sample estimates:
# 2022.A 2022.B 2022.C 2023.A 2023.B 2023.C 
#      0      0      0      0      0      0 
S = sqrt(S)
linear.model=lm(S~Anno+Sito+Anno*Sito)
C.test(linear.model)
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.23304, n = 30, k = 6, p-value = 0.3428
# alternative hypothesis: Group 2022.B has outlying variance
# sample estimates:
# 2022.A 2022.B 2022.C 2023.A 2023.B 2023.C 
#      0      0      0      0      0      0 
S = sqrt(S)
linear.model=lm(S~Anno+Sito+Anno*Sito)
C.test(linear.model)
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.23288, n = 30, k = 6, p-value = 0.3452
# alternative hypothesis: Group 2022.B has outlying variance
# sample estimates:
# 2022.A 2022.B 2022.C 2023.A 2023.B 2023.C 
#      0      0      0      0      0      0 
S = sqrt(S)
linear.model=lm(S~Anno+Sito+Anno*Sito)
C.test(linear.model)
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.2328, n = 30, k = 6, p-value = 0.3465
# alternative hypothesis: Group 2022.B has outlying variance
# sample estimates:
# 2022.A 2022.B 2022.C 2023.A 2023.B 2023.C 
#      0      0      0      0      0      0 
S = sqrt(S)
linear.model=lm(S~Anno+Sito+Anno*Sito)
C.test(linear.model)
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.23275, n = 30, k = 6, p-value = 0.3471
# alternative hypothesis: Group 2022.B has outlying variance
# sample estimates:
# 2022.A 2022.B 2022.C 2023.A 2023.B 2023.C 
#      0      0      0      0      0      0 
S = sqrt(S)
linear.model=lm(S~Anno+Sito+Anno*Sito)
C.test(linear.model)
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.23273, n = 30, k = 6, p-value = 0.3474
# alternative hypothesis: Group 2022.B has outlying variance
# sample estimates:
# 2022.A 2022.B 2022.C 2023.A 2023.B 2023.C 
#      0      0      0      0      0      0 
S = sqrt(S)
linear.model=lm(S~Anno+Sito+Anno*Sito)
C.test(linear.model)
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.23272, n = 30, k = 6, p-value = 0.3475
# alternative hypothesis: Group 2022.B has outlying variance
# sample estimates:
# 2022.A 2022.B 2022.C 2023.A 2023.B 2023.C 
#      0      0      0      0      0      0 
S = sqrt(S)
linear.model=lm(S~Anno+Sito+Anno*Sito)
C.test(linear.model)
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.23272, n = 30, k = 6, p-value = 0.3476
# alternative hypothesis: Group 2022.B has outlying variance
# sample estimates:
# 2022.A 2022.B 2022.C 2023.A 2023.B 2023.C 
#      0      0      0      0      0      0 
S = sqrt(S)
linear.model=lm(S~Anno+Sito+Anno*Sito)
C.test(linear.model)
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.23272, n = 30, k = 6, p-value = 0.3476
# alternative hypothesis: Group 2022.B has outlying variance
# sample estimates:
# 2022.A 2022.B 2022.C 2023.A 2023.B 2023.C 
#      0      0      0      0      0      0 
S = sqrt(S)
linear.model=lm(S~Anno+Sito+Anno*Sito)
C.test(linear.model)
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.23271, n = 30, k = 6, p-value = 0.3477
# alternative hypothesis: Group 2022.B has outlying variance
# sample estimates:
# 2022.A 2022.B 2022.C 2023.A 2023.B 2023.C 
#      0      0      0      0      0      0 
S = sqrt(S)
linear.model=lm(S~Anno+Sito+Anno*Sito)
C.test(linear.model)
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.23271, n = 30, k = 6, p-value = 0.3477
# alternative hypothesis: Group 2022.B has outlying variance
# sample estimates:
# 2022.A 2022.B 2022.C 2023.A 2023.B 2023.C 
#      0      0      0      0      0      0 
S = sqrt(S)
linear.model=lm(S~Anno+Sito+Anno*Sito)
C.test(linear.model)
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.23271, n = 30, k = 6, p-value = 0.3477
# alternative hypothesis: Group 2022.B has outlying variance
# sample estimates:
# 2022.A 2022.B 2022.C 2023.A 2023.B 2023.C 
#      0      0      0      0      0      0 
S = sqrt(S)
linear.model=lm(S~Anno+Sito+Anno*Sito)
C.test(linear.model)
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.23271, n = 30, k = 6, p-value = 0.3477
# alternative hypothesis: Group 2022.B has outlying variance
# sample estimates:
# 2022.A 2022.B 2022.C 2023.A 2023.B 2023.C 
#      0      0      0      0      0      0 
S = sqrt(S)
linear.model=lm(S~Anno+Sito+Anno*Sito)
C.test(linear.model)
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.23271, n = 30, k = 6, p-value = 0.3477
# alternative hypothesis: Group 2022.B has outlying variance
# sample estimates:
# 2022.A 2022.B 2022.C 2023.A 2023.B 2023.C 
#      0      0      0      0      0      0 
S = sqrt(S)
linear.model=lm(S~Anno+Sito+Anno*Sito)
C.test(linear.model)
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.23271, n = 30, k = 6, p-value = 0.3477
# alternative hypothesis: Group 2022.B has outlying variance
# sample estimates:
# 2022.A 2022.B 2022.C 2023.A 2023.B 2023.C 
#      0      0      0      0      0      0 
S = sqrt(S)
linear.model=lm(S~Anno+Sito+Anno*Sito)
C.test(linear.model)
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.23271, n = 30, k = 6, p-value = 0.3477
# alternative hypothesis: Group 2022.B has outlying variance
# sample estimates:
# 2022.A 2022.B 2022.C 2023.A 2023.B 2023.C 
#      0      0      0      0      0      0 
S = sqrt(S)
linear.model=lm(S~Anno+Sito+Anno*Sito)
C.test(linear.model)
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.23271, n = 30, k = 6, p-value = 0.3477
# alternative hypothesis: Group 2022.B has outlying variance
# sample estimates:
# 2022.A 2022.B 2022.C 2023.A 2023.B 2023.C 
#      0      0      0      0      0      0 
S = sqrt(S)
linear.model=lm(S~Anno+Sito+Anno*Sito)
C.test(linear.model)
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.23271, n = 30, k = 6, p-value = 0.3477
# alternative hypothesis: Group 2022.B has outlying variance
# sample estimates:
# 2022.A 2022.B 2022.C 2023.A 2023.B 2023.C 
#      0      0      0      0      0      0 
S = sqrt(S)
linear.model=lm(S~Anno+Sito+Anno*Sito)
C.test(linear.model)
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.23271, n = 30, k = 6, p-value = 0.3477
# alternative hypothesis: Group 2022.B has outlying variance
# sample estimates:
# 2022.A 2022.B 2022.C 2023.A 2023.B 2023.C 
#      0      0      0      0      0      0 
detach(dataset)
rm(list=ls(all=TRUE))
library(GAD)
dataset <- read.csv(file.choose(),header=T,sep=",", stringsAsFactors = T)
attach(dataset)
summary(dataset)
# Enter file name: 
#       ANNO                 Sperimentatore     GRUPPO  SITE      REPLICA           S         
#  Min.   :2022   Chiara Frigieri   :  4    Min.   :1   A:60   Min.   : 1.0   Min.   : 3.000  
#  1st Qu.:2022   Riccardo Brusa    :  4    1st Qu.:1   B:60   1st Qu.: 8.0   1st Qu.: 5.000  
#  Median :2022   Vincenzo          :  4    Median :2   C:60   Median :16.5   Median : 6.000  
#  Mean   :2022   Alberto           :  3    Mean   :2          Mean   :16.8   Mean   : 6.489  
#  3rd Qu.:2023   Alessandro Nicolai:  3    3rd Qu.:3          3rd Qu.:25.0   3rd Qu.: 8.000  
#  Max.   :2023   Alex Baggio Morano:  3    Max.   :3          Max.   :36.0   Max.   :25.000  
#                 (Other)           :159                                                      
Anno <- as.fixed(ANNO)
Sito <- as.random(SITE)

linear.model=lm(S~Anno+Sito+Anno*Sito)
C.test(linear.model)
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.50428, n = 30, k = 6, p-value = 1.139e-10
# alternative hypothesis: Group 2022.B has outlying variance
# sample estimates:
#  2022.A  2022.B  2022.C  2023.A  2023.B  2023.C 
#  2.4379 13.1264  2.4379  2.6023  3.0299  2.3954 
S = log(S, base = 2)
linear.model=lm(S~Anno+Sito+Anno*Sito)
C.test(linear.model)
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.23271, n = 30, k = 6, p-value = 0.3477
# alternative hypothesis: Group 2022.B has outlying variance
# sample estimates:
# 2022.A 2022.B 2022.C 2023.A 2023.B 2023.C 
# 0.1167 0.2305 0.1167 0.2148 0.1684 0.1434 
S = log(S, base = 2)
linear.model=lm(S~Anno+Sito+Anno*Sito)
C.test(linear.model)
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.28781, n = 30, k = 6, p-value = 0.02162
# alternative hypothesis: Group 2023.A has outlying variance
# sample estimates:
# 2022.A 2022.B 2022.C 2023.A 2023.B 2023.C 
# 0.0353 0.0535 0.0353 0.0938 0.0568 0.0513 
S = log(S, base = 2)
linear.model=lm(S~Anno+Sito+Anno*Sito)
C.test(linear.model)
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.38709, n = 30, k = 6, p-value = 1.763e-05
# alternative hypothesis: Group 2023.A has outlying variance
# sample estimates:
# 2022.A 2022.B 2022.C 2023.A 2023.B 2023.C 
# 0.0405 0.0523 0.0405 0.1838 0.0803 0.0775 
S = log(S, base = 2)
linear.model=lm(S~Anno+Sito+Anno*Sito)
C.test(linear.model)
# Warning message:
# NaNs produced 
# Error:
# NA/NaN/Inf in 'y'
# 1. lm(S ~ Anno + Sito + Anno * Sito) at Rovigno/ANOVA/bruttobrutto.R:13:1
# 2. lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...)
detach(dataset)
rm(list=ls(all=TRUE))
library(GAD)
dataset <- read.csv(file.choose(),header=T,sep=",", stringsAsFactors = T)
attach(dataset)
summary(dataset)
# Enter file name: 
#       ANNO                 Sperimentatore     GRUPPO  SITE      REPLICA           S         
#  Min.   :2022   Chiara Frigieri   :  4    Min.   :1   A:60   Min.   : 1.0   Min.   : 3.000  
#  1st Qu.:2022   Riccardo Brusa    :  4    1st Qu.:1   B:60   1st Qu.: 8.0   1st Qu.: 5.000  
#  Median :2022   Vincenzo          :  4    Median :2   C:60   Median :16.5   Median : 6.000  
#  Mean   :2022   Alberto           :  3    Mean   :2          Mean   :16.8   Mean   : 6.489  
#  3rd Qu.:2023   Alessandro Nicolai:  3    3rd Qu.:3          3rd Qu.:25.0   3rd Qu.: 8.000  
#  Max.   :2023   Alex Baggio Morano:  3    Max.   :3          Max.   :36.0   Max.   :25.000  
#                 (Other)           :159                                                      
Anno <- as.fixed(ANNO)
Sito <- as.random(SITE)
S = log(S, base = 2)
linear.model=lm(S~Anno+Sito+Anno*Sito)
C.test(linear.model)
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.23271, n = 30, k = 6, p-value = 0.3477
# alternative hypothesis: Group 2022.B has outlying variance
# sample estimates:
# 2022.A 2022.B 2022.C 2023.A 2023.B 2023.C 
# 0.1167 0.2305 0.1167 0.2148 0.1684 0.1434 
gad(linear.model) # ???????????
# $anova
# Analysis of Variance Table
# 
# Response: S
#            Df  Sum Sq Mean Sq F value Pr(>F)  
# Anno        1  5.9247  5.9247 27.3264 0.0347 *
# Sito        2  0.7565  0.3782  2.2914 0.1042  
# Anno:Sito   2  0.4336  0.2168  1.3135 0.2715  
# Residuals 174 28.7211  0.1651                 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
detach(dataset)
rm(list=ls(all=TRUE))
library(GAD)
dataset <- read.csv(file.choose(),header=T,sep=",", stringsAsFactors = T)
attach(dataset)
summary(dataset)

boxplot(S~ANNO+SITE) #why dafuq non ci sono i quadrati?

Anno <- as.fixed(ANNO)
Sito <- as.random(SITE)
# Enter file name: 
#       ANNO                 Sperimentatore     GRUPPO  SITE      REPLICA           S         
#  Min.   :2022   Chiara Frigieri   :  4    Min.   :1   A:60   Min.   : 1.0   Min.   : 3.000  
#  1st Qu.:2022   Riccardo Brusa    :  4    1st Qu.:1   B:60   1st Qu.: 8.0   1st Qu.: 5.000  
#  Median :2022   Vincenzo          :  4    Median :2   C:60   Median :16.5   Median : 6.000  
#  Mean   :2022   Alberto           :  3    Mean   :2          Mean   :16.8   Mean   : 6.489  
#  3rd Qu.:2023   Alessandro Nicolai:  3    3rd Qu.:3          3rd Qu.:25.0   3rd Qu.: 8.000  
#  Max.   :2023   Alex Baggio Morano:  3    Max.   :3          Max.   :36.0   Max.   :25.000  
#                 (Other)           :159                                                      
linear.model=lm(S~Anno+Sito+Anno*Sito)
C.test(linear.model)
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.50428, n = 30, k = 6, p-value = 1.139e-10
# alternative hypothesis: Group 2022.B has outlying variance
# sample estimates:
#  2022.A  2022.B  2022.C  2023.A  2023.B  2023.C 
#  2.4379 13.1264  2.4379  2.6023  3.0299  2.3954 
gad(linear.model) # ???????????
# $anova
# Analysis of Variance Table
# 
# Response: S
#            Df Sum Sq Mean Sq F value  Pr(>F)  
# Anno        1 115.20 115.200 57.1240 0.01706 *
# Sito        2  16.88   8.439  1.9452 0.14605  
# Anno:Sito   2   4.03   2.017  0.4649 0.62901  
# Residuals 174 754.87   4.338                  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
detach(dataset)
rm(list=ls(all=TRUE))
rm(list=ls(all=TRUE))
library(GAD)
dataset <- read.csv(file.choose(),header=T,sep=",", stringsAsFactors = T)
attach(dataset)
summary(dataset)
# Enter file name: 
#       ANNO                 Sperimentatore GRUPPO      SITE        REPLICA            S         
#  Min.   :2022   Chiara Frigieri   :  4    1:60   A      : 60   Min.   : 1.00   Min.   : 3.000  
#  1st Qu.:2023   Riccardo Brusa    :  4    2:60   B      : 60   1st Qu.: 5.00   1st Qu.: 5.000  
#  Median :2024   Vincenzo          :  4    3:60   C      : 60   Median : 7.00   Median : 6.000  
#  Mean   :2024   Adrian Villani    :  3    A:90   12     :  9   Mean   :10.32   Mean   : 6.489  
#  3rd Qu.:2025   Alberto           :  3    B:90   24     :  9   3rd Qu.:12.00   3rd Qu.: 8.000  
#  Max.   :2026   Alessandro Nicolai:  3    C:90   33     :  9   Max.   :36.00   Max.   :25.000  
#                 (Other)           :429           (Other):243                   NAs    :270     
boxplot(S~ANNO+SITE)
Anno <- as.fixed(ANNO)
Sito <- as.random(SITE)
linear.model=lm(S~Anno+Sito+Anno*Sito)
C.test(linear.model)
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.50428, n = 30, k = 6, p-value = 1.139e-10
# alternative hypothesis: Group 2022.B has outlying variance
# sample estimates:
#  2022.A  2022.B  2022.C  2023.A  2023.B  2023.C 
#  2.4379 13.1264  2.4379  2.6023  3.0299  2.3954 
detach(dataset)
rm(list=ls(all=TRUE))
rm(list=ls(all=TRUE))
library(GAD)
dataset <- read.csv(file.choose(),header=T,sep=",", stringsAsFactors = T)
attach(dataset)
summary(dataset)
# Enter file name: 
#       ANNO                 Sperimentatore GRUPPO      SITE        REPLICA            S        
#  Min.   :2022   Chiara Frigieri   :  4    1:60   A      : 60   Min.   : 1.00   Min.   : 3.00  
#  1st Qu.:2023   Riccardo Brusa    :  4    2:60   B      : 60   1st Qu.: 5.00   1st Qu.: 5.00  
#  Median :2024   Vincenzo          :  4    3:60   C      : 60   Median : 7.00   Median : 6.00  
#  Mean   :2024   Adrian Villani    :  3    A:90   12     :  9   Mean   :10.32   Mean   : 6.05  
#  3rd Qu.:2025   Alberto           :  3    B:90   24     :  9   3rd Qu.:12.00   3rd Qu.: 7.00  
#  Max.   :2026   Alessandro Nicolai:  3    C:90   33     :  9   Max.   :36.00   Max.   :11.00  
#                 (Other)           :429           (Other):243                   NAs    :270    
boxplot(S~ANNO+SITE)
Anno <- as.fixed(ANNO)
Sito <- as.random(SITE)
linear.model=lm(S~Anno+Sito+Anno*Sito)
C.test(linear.model)
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.28943, n = 30, k = 6, p-value = 0.01967
# alternative hypothesis: Group 2022.C has outlying variance
# sample estimates:
# 2022.A 2022.B 2022.C 2023.A 2023.B 2023.C 
# 2.4379 1.6379 4.9299 2.6023 3.0299 2.3954 
P = sqrt(S)
#P = log(S, base = 2)
linear.model=lm(P~Anno+Sito+Anno*Sito)
C.test(linear.model)
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.27969, n = 30, k = 6, p-value = 0.03438
# alternative hypothesis: Group 2022.C has outlying variance
# sample estimates:
# 2022.A 2022.B 2022.C 2023.A 2023.B 2023.C 
# 0.0909 0.0786 0.2005 0.1265 0.1214 0.0991 
P = log(S, base = 2)
linear.model=lm(P~Anno+Sito+Anno*Sito)
C.test(linear.model)
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.2726, n = 30, k = 6, p-value = 0.05075
# alternative hypothesis: Group 2022.C has outlying variance
# sample estimates:
# 2022.A 2022.B 2022.C 2023.A 2023.B 2023.C 
# 0.1167 0.1305 0.2900 0.2148 0.1684 0.1434 
linear.model=lm(S~Anno+Sito+Anno*Sito)
gad(linear.model)
# Error:
# missing value where TRUE/FALSE needed
# 1. gad(linear.model) at Rovigno/ANOVA/bruttobrutto.R:19:1
# 2. estimates(object, quasi.f = quasi.f)
C.test(linear.model)
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.28943, n = 30, k = 6, p-value = 0.01967
# alternative hypothesis: Group 2022.C has outlying variance
# sample estimates:
# 2022.A 2022.B 2022.C 2023.A 2023.B 2023.C 
# 2.4379 1.6379 4.9299 2.6023 3.0299 2.3954 
gad(linear.model)
# Error:
# missing value where TRUE/FALSE needed
# 1. gad(linear.model) at Rovigno/ANOVA/bruttobrutto.R:19:1
# 2. estimates(object, quasi.f = quasi.f)
detach(dataset)
rm(list=ls(all=TRUE))
rm(list=ls(all=TRUE))
library(GAD)
dataset <- read.csv(file.choose(),header=T,sep=",", stringsAsFactors = T)
attach(dataset)
summary(dataset)
# Enter file name: 
#       ANNO                 Sperimentatore GRUPPO      SITE        REPLICA            S        
#  Min.   :2022   Chiara Frigieri   :  4    1:60   A      : 60   Min.   : 1.00   Min.   : 3.00  
#  1st Qu.:2023   Riccardo Brusa    :  4    2:60   B      : 60   1st Qu.: 5.00   1st Qu.: 5.00  
#  Median :2024   Vincenzo          :  4    3:60   C      : 60   Median : 7.00   Median : 6.00  
#  Mean   :2024   Adrian Villani    :  3    A:90   12     :  9   Mean   :10.32   Mean   : 6.05  
#  3rd Qu.:2025   Alberto           :  3    B:90   24     :  9   3rd Qu.:12.00   3rd Qu.: 7.00  
#  Max.   :2026   Alessandro Nicolai:  3    C:90   33     :  9   Max.   :36.00   Max.   :11.00  
#                 (Other)           :429           (Other):243                   NAs    :270    
Anno <- as.fixed(ANNO)
Sito <- as.random(SITE)
linear.model=lm(S~Anno+Sito+Anno*Sito)
C.test(linear.model)
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.28943, n = 30, k = 6, p-value = 0.01967
# alternative hypothesis: Group 2022.C has outlying variance
# sample estimates:
# 2022.A 2022.B 2022.C 2023.A 2023.B 2023.C 
# 2.4379 1.6379 4.9299 2.6023 3.0299 2.3954 
gad(linear.model)
# Error:
# missing value where TRUE/FALSE needed
# 1. gad(linear.model) at Rovigno/ANOVA/bruttobrutto.R:19:1
# 2. estimates(object, quasi.f = quasi.f)
P = log(S, base = 2)
linear.model=lm(P~Anno+Sito+Anno*Sito)
gad(linear.model)
# Error:
# missing value where TRUE/FALSE needed
# 1. gad(linear.model) at Rovigno/ANOVA/bruttobrutto.R:19:1
# 2. estimates(object, quasi.f = quasi.f)
detach(dataset)
rm(list=ls(all=TRUE))
rm(list=ls(all=TRUE))
library(GAD)
dataset <- read.csv(file.choose(),header=T,sep=",", stringsAsFactors = T)
attach(dataset)
summary(dataset)

boxplot(S~ANNO+SITE)

Anno <- as.fixed(ANNO)
Sito <- as.random(SITE)
# Enter file name: 
#       ANNO                 Sperimentatore SITE       REPLICA            S        
#  Min.   :2022   Chiara Frigieri   :  4    A:150   Min.   : 1.00   Min.   : 2.00  
#  1st Qu.:2023   Riccardo Brusa    :  4    B:150   1st Qu.: 9.00   1st Qu.: 5.00  
#  Median :2024   Vincenzo          :  4    C:150   Median :18.00   Median : 7.00  
#  Mean   :2024   Adrian Villani    :  3            Mean   :17.72   Mean   :10.91  
#  3rd Qu.:2025   Alberto           :  3            3rd Qu.:26.00   3rd Qu.: 8.00  
#  Max.   :2026   Alessandro Nicolai:  3            Max.   :39.00   Max.   :70.00  
#                 (Other)           :429                                           
near.model=lm(S~Anno+Sito+Anno*Sito)
linear.model=lm(S~Anno+Sito+Anno*Sito)
C.test(linear.model)
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.49754, n = 30, k = 15, p-value < 2.2e-16
# alternative hypothesis: Group 2025.C has outlying variance
# sample estimates:
#   2022.A   2022.B   2022.C   2023.A   2023.B   2023.C   2024.A   2024.B   2024.C   2025.A   2025.B   2025.C   2026.A   2026.B   2026.C 
#   2.4379   1.6379   4.9299   2.6023   3.0299   2.3954   1.9586   1.9828   2.7644   1.8264  89.6333 261.4437   3.4069   3.9126 141.5126 
detach(dataset)
rm(list=ls(all=TRUE))
rm(list=ls(all=TRUE))
library(GAD)
dataset <- read.csv(file.choose(),header=T,sep=",", stringsAsFactors = T)
attach(dataset)
summary(dataset)
# Enter file name: 
#       ANNO                 Sperimentatore SITE       REPLICA            S         
#  Min.   :2022   Chiara Frigieri   :  4    A:150   Min.   : 1.00   Min.   : 2.000  
#  1st Qu.:2023   Riccardo Brusa    :  4    B:150   1st Qu.: 9.00   1st Qu.: 5.000  
#  Median :2024   Vincenzo          :  4    C:150   Median :18.00   Median : 6.000  
#  Mean   :2024   Adrian Villani    :  3            Mean   :17.72   Mean   : 5.976  
#  3rd Qu.:2025   Alberto           :  3            3rd Qu.:26.00   3rd Qu.: 7.000  
#  Max.   :2026   Alessandro Nicolai:  3            Max.   :39.00   Max.   :11.000  
#                 (Other)           :429                                            
boxplot(S~ANNO+SITE)
Anno <- as.fixed(ANNO)
Sito <- as.random(SITE)
linear.model=lm(S~Anno+Sito+Anno*Sito)
C.test(linear.model)
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.12643, n = 30, k = 15, p-value = 0.02359
# alternative hypothesis: Group 2022.C has outlying variance
# sample estimates:
# 2022.A 2022.B 2022.C 2023.A 2023.B 2023.C 2024.A 2024.B 2024.C 2025.A 2025.B 2025.C 2026.A 2026.B 2026.C 
# 2.4379 1.6379 4.9299 2.6023 3.0299 2.3954 1.9586 1.9828 2.7644 1.8264 2.1195 2.2402 3.4069 3.9126 1.7471 
P = sqrt(S)
# P = log(S, base = 2)
linear.model=lm(P~Anno+Sito+Anno*Sito)
C.test(linear.model)
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.11853, n = 30, k = 15, p-value = 0.06481
# alternative hypothesis: Group 2022.C has outlying variance
# sample estimates:
# 2022.A 2022.B 2022.C 2023.A 2023.B 2023.C 2024.A 2024.B 2024.C 2025.A 2025.B 2025.C 2026.A 2026.B 2026.C 
# 0.0909 0.0786 0.2005 0.1265 0.1214 0.0991 0.0957 0.0879 0.1369 0.0726 0.0776 0.0948 0.1692 0.1629 0.0773 
P = log(S, base = 2)
linear.model=lm(P~Anno+Sito+Anno*Sito)

# linear.model=lm(S~Anno+Sito+Anno*Sito)


C.test(linear.model)
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.11691, n = 30, k = 15, p-value = 0.07914
# alternative hypothesis: Group 2026.A has outlying variance
# sample estimates:
# 2022.A 2022.B 2022.C 2023.A 2023.B 2023.C 2024.A 2024.B 2024.C 2025.A 2025.B 2025.C 2026.A 2026.B 2026.C 
# 0.1167 0.1305 0.2900 0.2148 0.1684 0.1434 0.1613 0.1349 0.2423 0.0984 0.0975 0.1392 0.3054 0.2516 0.1183 
linear.model=lm(S~Anno+Sito+Anno*Sito)
gad(linear.model)
# $anova
# Analysis of Variance Table
# 
# Response: S
#            Df  Sum Sq Mean Sq F value    Pr(>F)    
# Anno        4   45.61 11.4022  1.2295 0.3710812    
# Sito        2    2.16  1.0822  0.4163 0.6597283    
# Anno:Sito   8   74.19  9.2739  3.5676 0.0005071 ***
# Residuals 435 1130.77  2.5995                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
snk.test(linear.model, term = "Anno:Sito", among = "Sito", within = "Anno", anova.tab = gad(linear.model))
# Student-Newman-Keuls test for: Anno:Sito 
# 
# Standard error = 0.2944
# Df = 435 
# 
# Pairwise comparisons among levels of: Sito 
# within each level of: Anno 
# 
# Level: 2022 
#               B       C      A  
# Rank order:   1       2      3  
# Ranked means: 5.5     6.6333 7.1
# Comparisons:                    
# 1             3-1 ***           
# 2             2-1 **  3-2 ns    
# Summary: B < C = A 
# 
# Level: 2023 
#               A      C      B     
# Rank order:   1      2      3     
# Ranked means: 5.1333 5.8667 6.0667
# Comparisons:                      
# 1             3-1 ns              
# 2             2-1 x  3-2 x        
# Summary: A = C = B 
# 
# Level: 2024 
#               A      B     C     
# Rank order:   1      2     3     
# Ranked means: 5.2    5.5   6.1667
# Comparisons:                     
# 1             3-1 ns             
# 2             2-1 x  3-2 x       
# Summary: A = B = C 
# 
# Level: 2025 
#               C      A      B     
# Rank order:   1      2      3     
# Ranked means: 6.0333 6.3667 6.4667
# Comparisons:                      
# 1             3-1 ns              
# 2             2-1 x  3-2 x        
# Summary: C = A = B 
# 
# Level: 2026 
#               C      A     B     
# Rank order:   1      2     3     
# Ranked means: 5.6667 5.8   6.1333
# Comparisons:                     
# 1             3-1 ns             
# 2             2-1 x  3-2 x       
# Summary: C = A = B 
# ---
# Signif. codes: ***p < 0.001; **p < 0.01; *p < 0.05; ns, p > 0.05
