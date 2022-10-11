

# About -------------------------------------------------------------------

# Function Name: simulateExposure

# Author: Justin Pretorius

# GitHub: https://github.com/jdpretorius123

# LinkedIn: https://www.linkedin.com/in/justin-pretorius 

# -------------------------------------------------------------------------


# Background ------------------------------------------------------------

# Prospective cohort studies construct the primary groups of interest from 

# the exposure status of their participants. This function simulates 

# non-differential misclassification resulting from the inaccurate screening 

# of exposure status in the study's participants. The population prevalence of 

# the exposure is used as the success probability for many binomial distributions 

# with the same number of trials. Each binomial distribution represents one 

# simulated study, where the number of successes denotes the number of true 

# positive participants for the exposure, and 1 - number of successes denotes 

# the number of true negative participants for the exposure. Each simulated study 

# is processed to determine the number of study participants that are misclassified 

# as a false positive or a false negative. Each study is also processed for each 

# level of specificity of interest.

# -------------------------------------------------------------------------


# Parameters --------------------------------------------------------------

# All parameters are named using the following notation:

# 1. If the parameter is one word, then the entire parameter is lowercase.
# 2. If the parameter is two words, then the entire parameter is lowercase,
#    and the first word is separated from the second word by a period.

# -------------------------------------------------------------------------


# Parameter Definitions ---------------------------------------------------

# prevalence: the population prevalence of the exposure.

# sample.size: the study-wide sample size.

# num.studies: the number of studies to simulate.

# sensitivity: the constant sensitivity applied to each simulated study; it must 
#              be passed as one floating-point value.

# specificity: the levels of specificity to be applied to each simulated study; 
#              it must be passed as a vector with any number of floating-point 
#              values as entries.

# -------------------------------------------------------------------------


# Function Space Objects that are Not Returned -----------------------------

# All function space objects that are not returned are named using the following 
# notation:

# 1. If the object name is one word, then the entire name is lowercase.
# 2. If the object name is two or more words, then the first word is lowercase,
#    and the second word is capitalized, and etc.

# -------------------------------------------------------------------------


# Function Space Objects that are Returned --------------------------------

# All function space objects that are returned are named using the following 
# notation:

# 1. All object names adhere to CamelCase notation
# 2. If the object name is one word, then the name is capitalized.
# 3. If the object name is two words, then each word is capitalized.

# -------------------------------------------------------------------------


# set.seed(1)
# 
# prevalence = 0.3
# 
# sample.size = 2000
# 
# num.studies = 1000
# 
# sensitivity = 1
# 
# specificity = c( 0.5,0.65,0.8,0.95 )


simulateExposure <- function( prevalence,sample.size,num.studies,sensitivity,specificity ) {
  
  # simulates true positive and true negative participants for many studies
  results = rbinom( n = num.studies, size = sample.size, prob = prevalence ) 
  
  # determines number of rows for output data frame
  numRows = num.studies * length( specificity )
  
  # builds output matrix
  Data = matrix( nrow = numRows, ncol = 7 )
  
  # index used for populating output matrix
  row = 1
  
  # iterates through each simulated study
  for ( i in 1:length( results ) ) {
    
    # stores the number of true positives for one study
    truePos = results[i]
    
    # stores the number of true negatives for one study
    trueNeg = sample.size - truePos
    
    # iterates through each level of specificity
    for ( j in 1:length( specificity ) ) {
      
      # simulates misclassification of true negative participants for one study 
      #   using one level of specificity
      falsePos = trueNeg - sum( rbinom( n = 1, size = trueNeg, prob = specificity[j] ) )
      
      # simulates miscalssification of true positive participants for one study using 
      #   constant sensitivity
      falseNeg = truePos - sum( rbinom( n = 1, size = truePos, prob = sensitivity ) )
      
      # stores the observed number of positive participants resulting from 
      #   misclassification for one study
      obsPos = truePos + falsePos - falseNeg
      
      # stores the observed number of negative participants resulting from 
      #   miscalssification for one study
      obsNeg = sample.size - obsPos
      
      # populates output matrix with results from one simulated study
      Data[ row,1:7 ] = c( specificity[j],truePos,obsPos,trueNeg,obsNeg,falsePos,falseNeg )
      
      # increments index declared outside outer for loop
      row = row + 1

    }
    
  }
  
  return( Data )
  
}
