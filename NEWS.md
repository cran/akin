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
  argments "workers" and "maxSize" may also receive values. Without a parallel plan, the function returns
  long substrings as they are with a message. 
  
* Function "fcommon" output is no longer a closure but a character vector.
  
* Function "common" remains available.
 
* Modified the Manual accordingly.

* Added package "future" to Suggests and package "future.apply" to Depends in DESCRIPTION.

# akin 0.3.2

* Minor changes to "fcommon" code.

* Added function "cover" which calculates the number of combinations for a common substring covering.
  Returns the "total", "min", "max" as a prettified, named vector and a plot on request.

* Modified the Manual accordingly.

* Function "common" is now deprecated.

# akin 0.3.3

* Bugfix: Re-coded the sequence isolation in matrix indices routine which were missing few short substrings.

* Redesigned function "fcommon" which is now fully sequential.

* Expanded function "cover" capability to show a list of valid substrings out of string covering using
  combinatorial method (recommended for short strings).

* Removed packages 'future.apply' from Depends and 'future' from Suggests in DESCRIPTION.

* Function "common" is now defunct.

# akin 0.3.4

* Minor code optimization for functions "cover" and "fcommon". "cover" now flags when only empty or
  1-character substrings are identified.
  
* Added new example to "fcommon" which proves the completeness of common substring identification.

* Added new example for function "cover" which shows the difference between string covering and valid
  substrings set.

* Added byte compilation option to DESCRIPTION.

# akin 0.3.5

* Resubmission
 
* Edited "ByteCompile" in the DESCRIPTION file.

# akin 0.3.6

* Resubmission

* Moved package 'RcppAlgos' from Depends to Imports
