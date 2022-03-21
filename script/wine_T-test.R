# T-test
Ttest <- c()
for (i in 1:dim(wine_g)[2]) {
  Ttest[i] <- t.test(wine_g[,i], wine_b[,i])$p.value
}

# individual test
which(Ttest<0.05)

# family-wise test (패키지 사용)
# bonferroni : p-values are multiplied by the number of comparisons
family.wise.test=p.adjust(Ttest, method = "bonferroni")
which(family.wise.test<0.05)

# family-wise test (자작)
which(Ttest < 0.05/length(Ttest))

library(fdrtool)
fdr <-  fdrtool(Ttest, statistic='pvalue', cutoff.method = 'fndr')
fdr$param

# FDR (자작)
fdr.level <- 0.05
var_table <- matrix(0, nrow=length(Ttest),ncol=4)
colnames(var_table) <- c('id', 'rank', 'P-value', 'i/m*FDR')
var_table[,1] <- c(1:length(Ttest))
var_table[,3] <- Ttest
var_table <- var_table[order(var_table[,3]),]
var_table[,2] <- c(1:length(Ttest))
var_table[,4] <- var_table[,2] / length(Ttest) * fdr.level
max(which(var_table[,3] < var_table[,4])) # 중요 변수 개수
var_table[1:max(which(var_table[,3] < var_table[,4])),1] # 중요 변수 추출
