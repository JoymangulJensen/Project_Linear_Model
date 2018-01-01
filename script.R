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
#                 regresion by 1 variable
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
        corr_desciptors[corr_desciptors_counter, 1]= colnames(df)[i]
      }
    }
  }
}
# Truncate the corr_descriptors matrix
corr_desciptors <- corr_desciptors[1:(corr_desciptors_counter),]

# Remove on descriptor which are too correlated
corr_desciptors <- unique(c(corr_desciptors))
new_df <- df[ , -which(names(df) %in% c(corr_desciptors))]

###################################################################
#             Linear regression variable selection 
#                 regresion by 2 variables
###################################################################
# Get number of descriptor
n_descriptor <- ncol(new_df)
#Matrix to store index of correlate descriptor
corr_desciptors_1 <- matrix(nrow=1500)
corr_desciptors_2 <- matrix(nrow=1500)
# Counter for number of correlates descriptor
corr_desciptors_counter <- 0
for (i in 1:n_descriptor) {
  for (j in i:n_descriptor) {
    if(i != j) {
      for (k in j:n_descriptor) {
        if (j != k) {
          reg0 = lm(df[,i] ~ df[,j] + df[,k], data=new_df)
          r2 <- summary(reg0)$r.squared
          if (r2 > 0.95) {
            corr_desciptors_counter <- corr_desciptors_counter + 1
            corr_desciptors_1[corr_desciptors_counter, 1]= colnames(new_df)[i]
            corr_desciptors_2[corr_desciptors_counter, 1]= colnames(new_df)[k]
          }
        }
      }
    }
  }
}

# Truncate the corr_descriptors matrix
corr_desciptors_1 <- corr_desciptors_1[1:(corr_desciptors_counter),]

# Remove on descriptor which are too correlated
corr_desciptors_1 <- unique(c(corr_desciptors_1))
new_df <- new_df[ , -which(names(new_df) %in% c(corr_desciptors_1))]

###################################################################
#             Linear regression variable selection 
#                 regresion by 3 variables
###################################################################
# Get number of descriptor
n_descriptor <- ncol(new_df)
#Matrix to store index of correlate descriptor
corr_desciptors_1 <- matrix(nrow=4000)
corr_desciptors_2 <- matrix(nrow=4000)
# Counter for number of correlates descriptor
corr_desciptors_counter <- 0
for (i in 1:n_descriptor) {
  for (j in i:n_descriptor) {
    if(i != j) {
      for (k in j:n_descriptor) {
        if (j != k) {
          for (l in j:n_descriptor) {
            if (k != l ) {
              reg0 = lm(df[,i] ~ df[,j] + df[,k] + df[,l], data=new_df)
              r2 <- summary(reg0)$r.squared
              if (r2 > 0.95) {
                corr_desciptors_counter <- corr_desciptors_counter + 1
                corr_desciptors_1[corr_desciptors_counter, 1]= colnames(new_df)[i]
                corr_desciptors_2[corr_desciptors_counter, 1]= colnames(new_df)[k]
              }
            }
          }
        }
      }
    }
  }
}

# Truncate the corr_descriptors matrix
corr_desciptors_1 <- corr_desciptors_1[1:(corr_desciptors_counter),]

# Remove on descriptor which are too correlated
corr_desciptors_1 <- unique(c(corr_desciptors_1))
new_df <- new_df[ , -which(names(new_df) %in% c(corr_desciptors_1))]