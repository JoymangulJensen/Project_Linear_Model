###################################################################
#                     Import the dataset                          #
###################################################################
# Clear global environment
rm(list = ls())
# Import datasets
df <- read.table("FW_groupe8.txt", header = TRUE)
df_obs <- read.table("FW_groupe8_obs.txt", header = TRUE)

###################################################################
#             Linear regression variable selection 
#                 regresion by 1 varia
###################################################################
# Get number of descriptor
n_descriptor <- ncol(df)
#Matrix to store index of correlate descriptor
corr_desciptors <- matrix(nrow=n_descriptor)
# Counter for number of correlates descriptor
corr_desciptors_counter <- 0
for (i in 1:n_descriptor) {
  for (j in i:n_descriptor) {
    if(i != j) {
      reg0 = lm(df[,i] ~ df[,j], data=df)
      r2 <- summary(reg0)$r.squared
      if (r2 > 0.95) {
        corr_desciptors_counter <- corr_desciptors_counter + 1
        corr_desciptors[corr_desciptors_counter, 1]= colnames(df)[j]
      }
    }
  }
}
# Truncate the corr_descriptors matrix
corr_desciptors <- corr_desciptors[1:(corr_desciptors_counter),]

# Remove on descriptor which are too correlated
corr_desciptors <- unique(c(corr_desciptors))
new <- subset(df, select = c(corr_desciptors)) 

for (i in 1:corr_desciptors_counter) {
  print(corr_desciptors[i,2])
}