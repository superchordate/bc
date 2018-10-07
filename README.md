# bc

My personal R package. Includes functions below. Use ?function to get more detailed documentation.

Each of these is a work in progress. Pull requests are welcome.

* **api** Get data from APIs.
* **cat2bool** Convert categorical columns in a dataset to boolean indicators, useful in modeling.
* **cc** Concatenate, shorthand for paste0 and paste with some functionality I prefer.
* **clean** Generic data-cleaning function. Right now, it just fixes column names ( remove NA, duplicate, etc. ).
* **nastrings** Vector of strings that bc considers NA. Excel errors, NULL, NA, etc.
* **notin** val %ni% vector performs equivalent to ! val %in% vector but is a bit easier to type.
* **r** Generic function for reading in data. A work in progress - doesn't really work currently.
* **runfolder** Runs R scripts in a folder, provides runtime of the script, and opens the script if an error is found.
* **sch** Search a dataset or vector.
* **spl** Sample a dataset or vector.
* **tonum** Convert a vector to numeric in a flexible, intelligent way.
* **vsel** Select best variables to be used in a linear regression model based on my defined criteria. Best called after normalization and splitting categorical to boolean.

*Many of these functions share names and functionality with functions I have built for Oliver Wyman. This is because these are useful names and commonly-used operations, not because I have copied them from my work there. As you can see in the commit history, each of these functions has been built from scratch on my personal time and with my personal computer.*
