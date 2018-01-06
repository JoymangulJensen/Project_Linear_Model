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
  boxplot(df[,i])
  for (j in i:n_descriptor) {
    if(i != j) {
      reg0 = lm(df[,i] ~ df[,j], data=df)
      print(colnames(df)[i])
      r2 <- summary(reg0)$r.squared
      if (r2 >= 0.95) {
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
#                     Regression  Stepwise v1
###################################################################
lm.tot<-lm(reponse~., data=new_df_obs)
lm.1=lm(reponse~1, data=new_df_obs)

lm.stepwise = step(lm.1,scope=formula(lm.tot),direction='forward', steps = 3)
PRESS.stepwise <- sum(abs(lm.stepwise$residuals))
print(PRESS.stepwise)

plot(lm.stepwise)
###################################################################
#                     Regression  Stepwise
###################################################################
# Get number of descriptor
n_descriptor_obs <- ncol(new_df_obs)

# Init the R2
best_r2 <- 0
best_press <- 9999999
for (i in 2:n_descriptor_obs) {
  for (j in i:n_descriptor_obs) {
    if(i != j) {
      for (k in j:n_descriptor_obs) {
        if (j != k) {
          for (l in j:n_descriptor_obs) {
            if (k != l ) {
              reg0 = lm(new_df_obs[,1] ~ new_df_obs[,i] + new_df_obs[,j] + new_df_obs[,k] + new_df_obs[,l], data=new_df_obs)
              current_press <- sum(reg0$residuals^2)
              r2 <- summary(reg0)$r.squared
              if (r2 > best_r2) {

                best_r2 <- r2
                best_variables_1_r2 <- colnames(new_df_obs)[i]
                best_variables_2_r2 <- colnames(new_df_obs)[j]
                best_variables_3_r2 <- colnames(new_df_obs)[k]
                best_reg_r2 <- reg0
              }
              
              if(current_press < best_press) {
                best_press <- current_press
                best_variables_1_press <- colnames(new_df_obs)[i]
                best_variables_2_press <- colnames(new_df_obs)[j]
                best_variables_3_press <- colnames(new_df_obs)[k]
                best_reg_press <- reg0
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
sum(best_reg_r2$residuals^2)
sum(best_reg_press$residuals^2)
sum(abs(best_reg_r2$residuals))
summary(best_reg_r2)$r.squared
summary(best_reg_press)$r.squared
AIC(best_reg_r2)
AIC(best_reg_press)

plot(best_reg_r2)
plot(best_reg_press)

###################################################################
#           Regression  ‘Least Absolute Deviation' v1
###################################################################
install.packages("L1pack")
library(L1pack)
# Get number of descriptor
n_descriptor_obs <- ncol(new_df_obs)

lad_AIC <- matrix(nrow=n_descriptor_obs, ncol = 2)
for (i in 2:n_descriptor_obs) {
  reponse_str <- "reponse" 
  formula <-paste(reponse_str, paste(colnames(new_df_obs)[i], collapse=" + "), sep=" ~ ") 
  intermediate_lad = lad(formula, data=new_df_obs) 
  lad_AIC[i, 1] =  AIC(intermediate_lad)
  lad_AIC[i, 2] =  colnames(new_df_obs)[i]
  colnames(new_df_obs)[i]
}
which.min(lad_AIC)
selected_descriptor.ind1 <- which.min(lad_AIC)
reponse_str <- "reponse" 
formula <-paste(reponse_str, paste(colnames(new_df_obs)[selected_descriptor.ind1], collapse=" + "), sep=" ~ ") 

lad_AIC <- matrix(nrow=n_descriptor_obs, ncol = 2)
for (i in 2:n_descriptor_obs) {
  if (i != selected_descriptor.ind1) {
    intermediate_formula <- paste(formula, paste(colnames(new_df_obs)[i], collapse=" + "), sep=" + ")
    intermediate_lad <- lad(intermediate_formula, data=new_df_obs) 
    lad_AIC[i, 1] <-  AIC(intermediate_lad)
    lad_AIC[i, 2] <-  colnames(new_df_obs)[i]
  }
}

selected_descriptor.ind2 <- which.min(lad_AIC)
formula <-paste(formula, paste(colnames(new_df_obs)[selected_descriptor.ind2], collapse=" + "), sep=" + ") 
lad_AIC <- matrix(nrow=n_descriptor_obs, ncol = 2)
for (i in 2:n_descriptor_obs) {
  if (i != selected_descriptor.ind1 && i != selected_descriptor.ind2) {
    intermediate_formula <- paste(formula, paste(colnames(new_df_obs)[i], collapse=" + "), sep=" + ")
    intermediate_lad <- lad(intermediate_formula, data=new_df_obs) 
    lad_AIC[i, 1] <-  AIC(intermediate_lad)
    lad_AIC[i, 2] <-  colnames(new_df_obs)[i]
  }
}
selected_descriptor.ind3 <- which.min(lad_AIC)
formula <-paste(formula, paste(colnames(new_df_obs)[selected_descriptor.ind3], collapse=" + "), sep=" + ") 
lad.stepwise <-  lad(formula, data=new_df_obs) 
plot(lad.stepwise)
test <- lad.stepwise$residuals * lad.stepwise$residuals 
sum(test)


###################################################################
#           Regression  ‘Least Absolute Deviation'
###################################################################
install.packages("L1pack")
library(L1pack)
# Get number of descriptor
n_descriptor_obs <- ncol(new_df_obs)

# Init the R2
best_r2 <- 0
best_press <- 9999999
for (i in 2:n_descriptor_obs) {
  for (j in i:n_descriptor_obs) {
    if(i != j) {
      for (k in j:n_descriptor_obs) {
        if (j != k) {
          for (l in j:n_descriptor_obs) {
            if (k != l ) {
              reg0 = lad(new_df_obs[,1] ~ new_df_obs[,i] + new_df_obs[,j] + new_df_obs[,k] + new_df_obs[,l], data=new_df_obs, method = "BR")
              current_press <- sum(reg0$residuals^2)
              
              if(current_press < best_press) {
                best_press <- current_press
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
sum(best_reg$residuals^2)
sum(abs(best_reg$residuals))
summary(best_reg)
plot(best_reg_r2$fitted.values, best_reg_r2$residuals,col = "red")
lines(best_reg$fitted.values, best_reg$residuals)

sum(best_reg$residuals^2)

###################################################################
#             Draw box plots
###################################################################
# Draw bxplot for descriptor1
boxplot(new_df_obs[,2],
        main=colnames(new_df_obs)[2])

# Draw bxplot for descriptor10
boxplot(new_df_obs[,5],
        main=colnames(new_df_obs)[5])

# Draw bxplot for descriptor17
boxplot(new_df_obs[,8],
        main=colnames(new_df_obs)[8])

# Draw bxplot for descriptor23
boxplot(new_df_obs[,11],
        main=colnames(new_df_obs)[11])

# Draw bxplot for descriptor61
boxplot(new_df_obs[,36],
        main=colnames(new_df_obs)[36])

# Draw bxplot for descriptor73
boxplot(new_df_obs[,43],
        main=colnames(new_df_obs)[43])

###################################################################
#             Draw graph to compare models
###################################################################
install.packages("ggplot2")
library(ggplot2)
fitted.values  <- best_reg_r2$fitted.values
residuals_m1 <- lm.stepwise$residuals
residuals_m2 <- lad.stepwise$residuals
dfplot <- data.frame(x,y1,y2)

p <-ggplot(dfplot, aes(fitted.values)) +                    # basic graphical object
  geom_point(aes(y=residuals_m1), colour="red", show.legend = TRUE, size=3) +  # first layer
  geom_point(aes(y=residuals_m2), colour="blue", size=3) +  # second layer
  geom_hline(yintercept=0, size=1, linetype="dashed")

p + labs(title = "Residuals vs valeurs prédites", subtitle = "Comparaison des deux models", x ="Valeurs prédites", y = "Residuals")
