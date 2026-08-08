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
?snk.test()
# Error:
# no methods for ‘snk.test’ and no documentation for it as a function
# 1. `?`(snk.test()) at Rovigno/ANOVA/bruttobrutto.R:23:1
# 2. .helpForCall(topicExpr, parent.frame())
# 3. stop(gettextf("no methods for %s and no documentation for it as a function", 
#        sQuote(f)), domain = NA)
library(GAD)
?snk.test()
rm(list=ls(all=TRUE))
library(GAD)
dataset <- read.csv(file.choose(),header=T,sep=",", stringsAsFactors = T)
attach(dataset)
summary(dataset)

boxplot(S~ANNO+SITE)

Anno <- as.fixed(ANNO)
Sito <- as.random(SITE)

# P = sqrt(S)
# P = log(S, base = 2)
# linear.model=lm(P~Anno+Sito+Anno*Sito)

linear.model=lm(S~Anno+Sito+Anno*Sito)
# Enter file name: 
#       ANNO                 Sperimentatore SITE       REPLICA            S         
#  Min.   :2022   Chiara Frigieri   :  4    A:150   Min.   : 1.00   Min.   : 2.000  
#  1st Qu.:2023   Riccardo Brusa    :  4    B:150   1st Qu.: 9.00   1st Qu.: 5.000  
#  Median :2024   Vincenzo          :  4    C:150   Median :18.00   Median : 6.000  
#  Mean   :2024   Adrian Villani    :  3            Mean   :17.72   Mean   : 5.976  
#  3rd Qu.:2025   Alberto           :  3            3rd Qu.:26.00   3rd Qu.: 7.000  
#  Max.   :2026   Alessandro Nicolai:  3            Max.   :39.00   Max.   :11.000  
#                 (Other)           :429                                            
gad(linear.model) # effetto significativo dell'interazione annoXsito, singoli fattori non significativi
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
snk.test(linear.model, term = "Anno:Sito", among = "Anno:Sito", within = "Anno", anova.tab = gad(linear.model))
# Error:
# undefined columns selected
# 1. snk.test(linear.model, term = "Anno:Sito", among = "Anno:Sito", 
#        within = "Anno", anova.tab = gad(linear.model)) at Rovigno/ANOVA/bruttobrutto.R:24:1
# 2. tapply(set[[i]][, 1], subset(set[[i]], select = among), mean)
# 3. subset(set[[i]], select = among)
# 4. subset.data.frame(set[[i]], select = among)
# 5. x[r, vars, drop = drop]
# 6. `[.data.frame`(x, r, vars, drop = drop)
# 7. stop("undefined columns selected")
snk.test(linear.model, term = "Anno:Sito", among = "Sito", within = "Anno:Sito", anova.tab = gad(linear.model))
# Error:
# missing value where TRUE/FALSE needed
# 1. snk.test(linear.model, term = "Anno:Sito", among = "Sito", within = "Anno:Sito", 
#        anova.tab = gad(linear.model)) at Rovigno/ANOVA/bruttobrutto.R:24:1
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
snk.test(linear.model, term = "Anno:Sito", among = "Anno", within = "Sito", anova.tab = gad(linear.model))
# Student-Newman-Keuls test for: Anno:Sito 
# 
# Standard error = 0.2944
# Df = 435 
# 
# Pairwise comparisons among levels of: Anno 
# within each level of: Sito 
# 
# Level: A 
#               2023    2024    2026   2025   2022
# Rank order:   1       2       3      4      5   
# Ranked means: 5.1333  5.2     5.8    6.3667 7.1 
# Comparisons:                                    
# 1             5-1 ***                           
# 2             4-1 *   5-2 ***                   
# 3             3-1 ns  4-2 *   5-3 **            
# 4             2-1 x   3-2 x   4-3 ns 5-4 ns     
# Summary: 2023 = 2024 = 2026 = 2025 = 2022 
# 
# Level: B 
#               2022   2024  2023   2026   2025  
# Rank order:   1      2     3      4      5     
# Ranked means: 5.5    5.5   6.0667 6.1333 6.4667
# Comparisons:                                   
# 1             5-1 ns                           
# 2             4-1 x  5-2 x                     
# 3             3-1 x  4-2 x 5-3 x               
# 4             2-1 x  3-2 x 4-3 x  5-4 x        
# Summary: 2022 = 2024 = 2023 = 2026 = 2025 
# 
# Level: C 
#               2026   2023   2025   2024   2022  
# Rank order:   1      2      3      4      5     
# Ranked means: 5.6667 5.8667 6.0333 6.1667 6.6333
# Comparisons:                                    
# 1             5-1 ns                            
# 2             4-1 x  5-2 x                      
# 3             3-1 x  4-2 x  5-3 x               
# 4             2-1 x  3-2 x  4-3 x  5-4 x        
# Summary: 2026 = 2023 = 2025 = 2024 = 2022 
# ---
# Signif. codes: ***p < 0.001; **p < 0.01; *p < 0.05; ns, p > 0.05