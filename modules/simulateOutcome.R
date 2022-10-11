

# About -------------------------------------------------------------------

# Function Name: simulateOutcome

# Author: Justin Pretorius

# GitHub: https://github.com/jdpretorius123

# LinkedIn: https://www.linkedin.com/in/justin-pretorius 

# -------------------------------------------------------------------------


# Background ------------------------------------------------------------

# This function simulates the outcomes for many prospective cohort studies 

# where the study participants have been misclassified with regard to their 

# exposure status. The impact of this misclassification is calculated as a 

# difference relative to the outcomes for these studies had the misclassification 

# never occurred. The following measures of association are used to capture the 

# impact of non-differential misclassification on the results of each simulated 

# study: Risk Difference, Risk Ratio, and Odds Ratio.

# -------------------------------------------------------------------------


# Parameters --------------------------------------------------------------

# All parameters are named using the following notation:

# 1. If the parameter is one word, then the entire parameter is lowercase.
# 2. If the parameter is two words, then the entire parameter is lowercase,
#    and the first word is separated from the second word by a period.

# -------------------------------------------------------------------------


# Parameter Definitions ---------------------------------------------------

# exposure.data: input matrix containing the true positives and negatives, the 
#                observed positives and negatives, the false positives and false 
#                negatives resulting from non-differential misclassification, 
#                and the level of specificity for each simulated study generated 
#                by simulateExposure

# risk.exposed: the risk associated with developing the outcome of interest in 
#               the exposed group

# risk.unexposed: the risk associated with developing the outcome of interest 
#                 in the unexposed group

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


# source( "simulateExposure.r" )
# 
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
# 
# exposure.data = simulateExposure( prevalence,sample.size,num.studies,sensitivity,specificity )
# 
# risk.exposed = 0.7
# 
# risk.unexposed = 0.3


simulateOutcome <- function( exposure.data,risk.exposed,risk.unexposed ){
  
  # determines number of rows for output matrix
  numRows = dim( exposure.data )[1]
  
  # builds output matrix
  Data = matrix( 
    
    nrow = numRows, 
    
    ncol = 4, 
    
    dimnames = list( c(), c( "Specificity","RDdiff","RRdiff","ORdiff" ) ) 
                 
                 )
  
  # iterates through each simulated study
  for ( i in 1:numRows ) {
    
    # stores one simulated study from exposure.data matrix 
    study = exposure.data[i,]
    
    # stores level of specificity for one study
    specificity = study[1]
    

    # True Measures of Association --------------------------------------------

    # stores number of true positive participants for one study
    truePosCount = study[2]
    
    # simulates the number of true positive participants that develop 
    #   outcome of interest for one study
    truePosOutcome = sum( rbinom( n = 1, size = truePosCount, prob = risk.exposed ) )
    
    # stores the number of true positive participants that do not develop 
    #   outcome of interest for one study
    truePosNoOutcome = truePosCount - truePosOutcome
    
    # stores the true risk associated with the developing the outcome of interest 
    #   in the exposed group for one study
    trueRiskExposed = truePosOutcome / truePosCount
    
    # stores the true odds associated with developing the outcome of interest in 
    #   the exposed group for one study
    trueOddsExposed = truePosOutcome / truePosNoOutcome
    
    # stores the number of true negative participants for one study
    trueNegCount = study[4]
    
    # simulates the number of true negative participants that develop outcome of 
    #   interest for one study
    trueNegOutcome = sum( rbinom( n = 1, size = trueNegCount, prob = risk.unexposed ) )
    
    # stores the number of true negative participants that do not develop outcome of 
    #   interest for one study
    trueNegNoOutcome = trueNegCount - trueNegOutcome
    
    # stores the true risk associated with the developing the outcome of interest in 
    #   the unexposed group for one study
    trueRiskUnexposed = trueNegOutcome / trueNegCount
    
    # stores the true odds associated with the developing the outcome of interest in 
    #   the unexposed group for one study
    trueOddsUnexposed = trueNegOutcome / trueNegNoOutcome
    
    # stores the true risk difference for one study
    trueRD = trueRiskExposed - trueRiskUnexposed
    
    # stores the true risk ratio for one study
    trueRR = trueRiskExposed / trueRiskUnexposed
    
    # stores the true odds ratio for one study
    trueOR = trueOddsExposed / trueOddsUnexposed

    # -------------------------------------------------------------------------

    
    # Observed Measures of Association ----------------------------------------
    
    # stores the observed number of negative participants for one study
    obsNegCount = study[5]
    
    # simulates the number of observed negative participants that develop outcome of 
    #   interest for one study
    obsNegOutcome = sum( rbinom( n = 1, size = obsNegCount, prob = risk.unexposed ) )
    
    # stores the number of observed negative participants that do not develop outcome 
    #   of interest for one study
    obsNegNoOutcome = obsNegCount - obsNegOutcome
    
    # stores the number of false positive participants for one study
    falsePosCount = study[6]
    
    # simulates the number of false positive participants that develop outcome of 
    #   interest for one study
    falsePosOutcome = sum( rbinom( n = 1, size = falsePosCount, prob = risk.unexposed ) )
    
    # stores the number of false positive participants that do not develop outcome of 
    #   interest for one study
    falsePosNoOutcome = falsePosCount - falsePosOutcome
    
    # stores the number of false negative participants for one study
    falseNegCount = study[7]
    
    # simulates the number of false negative participants that develop outcome of interest 
    #   for one study
    falseNegOutcome = sum( rbinom( n = 1, size = falseNegCount, prob = risk.exposed ) )
    
    # stores the number of false negative participants that do not develop outcome of 
    #   interest for one study
    falseNegNoOutcome = falseNegCount - falseNegOutcome
    
    # stores the number of observed positive participants for one study
    obsPosCount = study[3]
    
    # stores the observed risk associated with the developing the outcome of interest in 
    #   the exposed group for one study
    obsRiskExposed = ( truePosOutcome + falsePosOutcome ) / obsPosCount
    
    # stores the observed odds associated with the developing the outcome of interest in 
    #   the exposed group for one study
    obsOddsExposed = ( truePosOutcome + falsePosOutcome ) / ( truePosNoOutcome + falsePosNoOutcome )
    
    # stores the observed risk associated with the developing the outcome of interest in 
    #   the unexposed group for one study
    obsRiskUnexposed = ( obsNegOutcome + falseNegOutcome ) / obsNegCount
    
    # stores the observed odds associated with the developing the outcome of interest in 
    #   the unexposed group for one study
    obsOddsUnexposed = ( obsNegOutcome + falseNegOutcome ) / ( obsNegNoOutcome + falseNegNoOutcome )
    
    # stores the observed risk difference for one study
    obsRD = obsRiskExposed - obsRiskUnexposed
    
    # stores the observed risk ratio for one study
    obsRR = obsRiskExposed / obsRiskUnexposed
    
    # stores the observed odds ratio for one study
    obsOR = obsOddsExposed / obsOddsUnexposed
    
    # -------------------------------------------------------------------------
    
    
    # stores the difference between the true risk difference and observed risk difference 
    #   for one study
    differenceRD = trueRD - obsRD
    
    # stores the difference between the true risk ratio and the observed risk ratio for 
    #   one study
    differenceRR = trueRR - obsRR
    
    # stores the difference between the true odds ratio and the observed odds ratio for 
    #   one study
    differenceOR = trueOR - obsOR
    
    # populates output matrix with results from one simulated study
    Data[ i,1:4 ] = c( specificity,differenceRD,differenceRR,differenceOR )
    
  }
  
  return( Data )
  
}
