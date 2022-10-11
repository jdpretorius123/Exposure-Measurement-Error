



# About -------------------------------------------------------------------

# Function Name: characterToNumeric

# Author: Justin Pretorius

# GitHub: https://github.com/jdpretorius123

# LinkedIn: https://www.linkedin.com/in/justin-pretorius 

# -------------------------------------------------------------------------


# Background ------------------------------------------------------------

# This function converts a character string of comma separated 'numeric' 

# values into a numeric vector containing independent numeric values.

# -------------------------------------------------------------------------


# Parameters --------------------------------------------------------------

# All parameters are named using the following notation:

# 1. If the parameter is one word, then the entire parameter is lowercase.
# 2. If the parameter is two words, then the entire parameter is lowercase,
#    and the first word is separated from the second word by a period.

# -------------------------------------------------------------------------


# Parameter Definitions ---------------------------------------------------

# character.string: a character string of comma separated 'numeric' values

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


#library( tidyverse )

characterToNumeric  = function( character.string ) {
  
  # converts character string of comma separated 'numeric' values into 
  # character matrix
  characterMatrix = str_split( character.string, 
                               
                               pattern = ",",
                               
                               simplify = TRUE )
  
  # coerces character matrix with one row and X columns into numeric
  # vector
  numericVector = as.numeric( characterMatrix )
  
  return( numericVector )
  
}
