# akin 0.1.0

* Initial CRAN submission.

# akin 0.1.1

* Resubmission.

# akin 0.1.2

* Resubmission.

# akin 0.1.3

* Resubmission

# akin 0.1.4

* Resubmission

Added function 'common' which identifies common substrings inside a pair of chains

# akin 0.2.0

* Bugfix: Removed delayed assignment which would prevent spanning the entire from-to window in function "common"
* Clarified some statements in the "Manual"

# akin 0.2.1

* Removed package 'erer' from dependencies used by "tileData" function. The tiles list is now saved as "rds" file
  which can be conveniently read as list subsets by readRDS.
  
# akin 0.3.0

* Added the experimental function "fcommon", a fast version of "common".

# akin 0.3.1

* Expanded function "fcommon" capability to parallel processing for substrings longer than 20 characters.
  A local parallel plan is set when formal argument "strategy" receives a value. Pending on this, formal
  argment "workers" may also receive a value. Without a parallel plan, the function will output the long
  substrings as they are, along with a message. The output is no longer a closure but a character vector.
  
* Function "common" remains available.
 
* Modified the Manual accordingly.

* Added package "future" to Suggests and package "future.apply" to Depends in DESCRIPTION.

