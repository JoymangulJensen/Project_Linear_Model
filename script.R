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
new_df_obs <- df_obs[ , -which(names(df_obs) %in% c(corr_desciptors))]

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
          reg0 = lm(new_df[,i] ~ new_df[,j] + new_df[,k], data=new_df)
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
new_df_obs <- new_df_obs[ , -which(names(new_df_obs) %in% c(corr_desciptors_1))]

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
              reg0 = lm(new_df[,i] ~ new_df[,j] + new_df[,k] + new_df[,l], data=new_df)
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
new_df_obs <- new_df_obs[ , -which(names(new_df_obs) %in% c(corr_desciptors_1))]

###################################################################
#                     Regression  Stepwise
###################################################################
# Get number of descriptor
n_descriptor_obs <- ncol(new_df_obs)

# Init the R2
best_r2 <- 0

for (i in 2:n_descriptor_obs) {
  for (j in i:n_descriptor_obs) {
    if(i != j) {
      for (k in j:n_descriptor_obs) {
        if (j != k) {
          for (l in j:n_descriptor_obs) {
            if (k != l ) {
              reg0 = lm(new_df_obs[,1] ~ new_df_obs[,i] + new_df_obs[,j] + new_df_obs[,k] + new_df_obs[,l], data=new_df_obs)
              r2 <- summary(reg0)$r.squared
              if (r2 > best_r2) {PRESS(mod)

                best_r2 <- r2
                best_variables_1 <- colnames(new_df_obs)[i]
                best_variables_2 <- colnames(new_df_obs)[j]
                best_variables_3 <- colnames(new_df_obs)[k]
                best_reg <- reg0
              }
            }
          }
        }
      }
    }
  }
}

###################################################################
#                     Calculate the PRESS
###################################################################
sum(best_reg$residuals^2)

###################################################################
#           Regression  ‘Least Absolute Deviation'
###################################################################
install.packages("Blossom")
library(Blossom)
obs <- new_df_obs[,1]
descriptor_only <- new_df_obs[-c(1)]
reponse_str <- "reponse"
names <-  colnames(descriptor_only)
formula <-paste(reponse_str, paste(names, collapse=" + "), sep=" ~ ")

Out <- lad(formula,data = new_df_obs,test = TRUE)

summary(Out)
residuals(Out)
predict(Out)
coefficients(Out)
