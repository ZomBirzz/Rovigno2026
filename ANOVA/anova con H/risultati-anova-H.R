C.test(linear.model) 
gad(linear.model) 
# 
# 	Cochran test of homogeneity of variances
# 
# data:  linear.model
# C = 0.12954, n = 30, k = 15, p-value = 0.01558
# alternative hypothesis: Group 2025.A has outlying variance
# sample estimates:
# 2022.A 2022.B 2022.C 2023.A 2023.B 2023.C 2024.A 2024.B 2024.C 2025.A 2025.B 2025.C 2026.A 2026.B 2026.C 
# 0.0108 0.0117 0.0201 0.0209 0.0133 0.0299 0.0099 0.0072 0.0073 0.0309 0.0192 0.0075 0.0168 0.0229 0.0103 
# 
# $anova
# Analysis of Variance Table
# 
# Response: H1
#            Df Sum Sq Mean Sq F value    Pr(>F)    
# Anno        4 1.7584 0.43961  2.8302   0.09829 .  
# Sito        2 1.0223 0.51115 32.1306 9.663e-14 ***
# Anno:Sito   8 1.2426 0.15533  9.7640 1.689e-12 ***
# Residuals 435 6.9202 0.01591                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
snk.test(linear.model, term = "Anno:Sito", among = "Sito", within = "Anno", anova.tab = gad(linear.model))
# Student-Newman-Keuls test for: Anno:Sito 
# 
# Standard error = 0.023
# Df = 435 
# 
# Pairwise comparisons among levels of: Sito 
# within each level of: Anno 
# 
# Level: 2022 
#               A       B       C     
# Rank order:   1       2       3     
# Ranked means: 0.6472  0.7981  0.9477
# Comparisons:                        
# 1             3-1 ***               
# 2             2-1 *** 3-2 ***       
# Summary: A < B < C 
# 
# Level: 2023 
#               B      C      A     
# Rank order:   1      2      3     
# Ranked means: 0.7909 0.8048 0.8254
# Comparisons:                      
# 1             3-1 ns              
# 2             2-1 x  3-2 x        
# Summary: B = C = A 
# 
# Level: 2024 
#               A       B      C     
# Rank order:   1       2      3     
# Ranked means: 0.6018  0.6259 0.7205
# Comparisons:                       
# 1             3-1 ***              
# 2             2-1 ns  3-2 **       
# Summary: A = B < C 
# 
# Level: 2025 
#               A       C       B     
# Rank order:   1       2       3     
# Ranked means: 0.6396  0.687   0.7998
# Comparisons:                        
# 1             3-1 ***               
# 2             2-1 ns  3-2 ***       
# Summary: A = C < B 
# 
# Level: 2026 
#               A       B      C     
# Rank order:   1       2      3     
# Ranked means: 0.6154  0.6943 0.7434
# Comparisons:                       
# 1             3-1 ***              
# 2             2-1 *   3-2 ns       
# Summary: A < B = C 
# ---
# Signif. codes: ***p < 0.001; **p < 0.01; *p < 0.05; ns, p > 0.05
snk.test(linear.model, term = "Anno:Sito", among = "Anno", within = "Sito", anova.tab = gad(linear.model))
# Student-Newman-Keuls test for: Anno:Sito 
# 
# Standard error = 0.023
# Df = 435 
# 
# Pairwise comparisons among levels of: Anno 
# within each level of: Sito 
# 
# Level: A 
#               2024    2026    2025    2022    2023  
# Rank order:   1       2       3       4       5     
# Ranked means: 0.6018  0.6154  0.6396  0.6472  0.8254
# Comparisons:                                        
# 1             5-1 ***                               
# 2             4-1 ns  5-2 ***                       
# 3             3-1 x   4-2 x   5-3 ***               
# 4             2-1 x   3-2 x   4-3 x   5-4 ***       
# Summary: 2024 = 2026 = 2025 = 2022 < 2023 
# 
# Level: B 
#               2024    2026   2023   2022   2025  
# Rank order:   1       2      3      4      5     
# Ranked means: 0.6259  0.6943 0.7909 0.7981 0.7998
# Comparisons:                                     
# 1             5-1 ***                            
# 2             4-1 *** 5-2 **                     
# 3             3-1 *** 4-2 ** 5-3 ns              
# 4             2-1 *   3-2 ** 4-3 x  5-4 x        
# Summary: 2024 < 2026 < 2023 = 2022 = 2025 
# 
# Level: C 
#               2025    2024    2026    2023    2022  
# Rank order:   1       2       3       4       5     
# Ranked means: 0.687   0.7205  0.7434  0.8048  0.9477
# Comparisons:                                        
# 1             5-1 ***                               
# 2             4-1 **  5-2 ***                       
# 3             3-1 ns  4-2 *   5-3 ***               
# 4             2-1 x   3-2 x   4-3 ns  5-4 ***       
# Summary: 2025 = 2024 = 2026 = 2023 < 2022 
# ---
# Signif. codes: ***p < 0.001; **p < 0.01; *p < 0.05; ns, p > 0.05