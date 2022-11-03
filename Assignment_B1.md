Assignment B1
================
Jerry
2022-11-03

# Assignment B1: Write a Function

If you’ve ever run an ANOVA in R, you’ll know that you often need to run
a “post-hoc” test on the resulting output to actually extract
statistical significance information from the test. One useful R
function to do this is `TukeyHSD()` which is part of the `stats` package
in R. As its name suggests, `TukeyHSD` runs a “Tukey’s Honestly
Significant Difference” test to more or less generate a p-value between
comparisons to assess statistical significance. Unfortunately, the
output of `TukeyHSD()` is kind of ugly. It provides a list output with a
plethora of information, but is not “*reader-friendly*” in my *personal*
opinion. Specifically, it can be difficult to identify the comparison
that you want to look at, especially if you have 100s of comparisons. To
make matters worse, `broom` doesn’t seem to have any methods to extract
info from a `TukeyHSD` object (at least that I know yet … ).

In light of this, let’s try to make a function that takes the output of
`TukeyHSD()` and converts it into a matrix or dataframe where p-values
are easier to read by rearranging the output to look like a matrix array
.

# Exercise 1: Make a Function (25 points)

## Load packages

``` r
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(testthat)) # to test our function
library(datateachr) # we'll take a sample dataframe from datateachr for examples
library(palmerpenguins) # we'll use penguins dataset for examples and testing
```

## Making our function

Quick rundown of what we’re going to do. This function should take any
output of `TukeyHSD()`, extract the desired element, make a matrix array
(tbh idk if I’m using this word right, I think that in real math this
means something meaningful, please let me know), and fill in the cells
of the matrix with the corresponding p-values. Additionally, we will add
an option where we automatically annotate statistically significant
comparisons with asterisks.

``` r
#' Convert TukeyHSD() Output to a Matrix Array
#' Create a user-readable matrix array from the output of TukeyHSD(). Takes the output of TukeyHSD() and converts the output into a matrix array dataframe. 
#' 
#' 
#' @param tukey_object The input data. Should be a TukeyHSD() object. Named tukey_object since input should be a TukeyHSD() object. 
#' @param element The element of the TukeyHSD() object to subset and convert. Does not need to be a string. Named element since this should be the element from the TukeyHSD() object that wants to be extracted.
#' @param significance Defaults to TRUE. Should statistically significant p-values be replaces with asterisks? Named significance since this controls if significant p-values are replaced with asterisks. 
#'
#' @return A dataframe object with rows and columns of comparisons.
#' @export
#'
#' @examples
#' AOV_Out <- aov(Petal.Length ~ Species, iris)
#' TukeyHSD_Out <- TukeyHSD(AOV_Out)
#' tukey2matrix(input = TukeyHSD_Out, element = Species)
#' tukey2matrix(TukeyHSD_Out, Species, significance = FALSE)
tukey2matrix <- function(tukey_object, element, significance = TRUE) {
  
  # Turn TukeyHSD output into a dataframe and turn the rownames into a column 
  data <- as.data.frame(tukey_object[[deparse(substitute(element))]]) %>% 
    rownames_to_column("Comparison")

  # Select the Comparison and `p adj` columns and separate comparison into the two conditions that are being compared
  data <- data %>% 
    select(Comparison, `p adj`) %>% 
    separate(Comparison, into = c("X1", "X2"), sep = "-")

  # Generate a vector containing all of the unique conditions you are comparing
  comparisons <- unique(c(data$X1, data$X2))
  
  # Generate an empty dataframe with the comparison names as row and colnames
  out_dat <- data.frame(row.names = comparisons)
  for (i in comparisons) {
    out_dat <- out_dat %>% 
      mutate("{i}" := NA) # dynamically name cols by comparison name
  }
  
  # Fill the correct cells of the empty dataframe with the `p adj` values using names as indices
  for (i in 1:nrow(data)) {
  
    # Assign x and y as the condition names and then assign its respective value to the empty dataframe
    x <- data[i, "X1"]
    y <- data[i, "X2"]
    out_dat[x, y] <- data[i, "p adj"]
    out_dat[y, x] <- data[i, "p adj"]
    
    row <- as.numeric(out_dat[x, y])
    
    # If significance is TRUE, substitute statistically significant p-values with asterisks
    if (significance == TRUE) {
      if (row <= 0.05 && row > 0.005) {
        out_dat[x,y] <- "*"
        out_dat[y,x] <- "*"
      } else if (row <= 0.005 && row > 0.0005) {
        out_dat[x,y] <- "**"
        out_dat[y,x] <- "**" 
      } else if (row <= 0.0005 && row >= 0) {
        out_dat[x,y] <- "***"
        out_dat[y,x] <- "***" 
      } else if (is.na(row) == TRUE) {
        print("NA")
    }}}
  
  return(out_dat)
}
```

# Exercise 2: Document your Function (20 points)

Included above.

# Exercise 3: Include examples (15 points)

Example with `vancouver_trees` from `datateachr`

``` r
# Examples with the vancouver_trees dataset from the datateachr package
trees <- vancouver_trees

# First, perform an ANOVA: Is diameter statistically different between genera
trees_aov <- aov(diameter ~ genus_name, trees) 

# Now let's TukeyHSD() to get p-values between comparisons
trees_tukey <- TukeyHSD(trees_aov) 

# Finally, let's use `tukey2matrix()` now to convert the output into something readable.
converted_trees <- tukey2matrix(trees_tukey, genus_name) 
head(converted_trees %>% select(1:5))
```

    ##             ACER          AESCULUS AILANTHUS  ALBIZIA ALNUS
    ## ACER        <NA>               *** 1.0000000 1.000000   ***
    ## AESCULUS     ***              <NA> 0.9999999 0.999984   ***
    ## AILANTHUS      1 0.999999923568325        NA 1.000000     1
    ## ALBIZIA        1 0.999984044730028 1.0000000       NA     1
    ## ALNUS        ***               *** 1.0000000 1.000000  <NA>
    ## AMELANCHIER  ***               *** 0.8535143 1.000000   ***

Let’s check to see if `bill_length_mm` is significantly different as a
function of species and island its from:

``` r
# First perform an ANOVA with the outlined variables:
penguins_aov <- aov(bill_length_mm ~ species + island, penguins)

# Now, perform a Tukey HSD test:
penguins_tukey <- TukeyHSD(penguins_aov)

# And finally, convert the TukeyHSD() output to a readable dataframe, specifically let's look at p-values of species first
converted_penguins_species <- tukey2matrix(penguins_tukey, species)
head(converted_penguins_species)
```

    ##           Chinstrap Gentoo Adelie
    ## Chinstrap      <NA>      *    ***
    ## Gentoo            *   <NA>    ***
    ## Adelie          ***    ***   <NA>

``` r
# Also, we can also look at p-values of island comparisons, but let's turn significance off for this one:
converted_penguins_island <- tukey2matrix(penguins_tukey, island)
head(converted_penguins_island)
```

    ##               Dream Torgersen    Biscoe
    ## Dream            NA 0.8262480 0.8666428
    ## Torgersen 0.8262480        NA 0.9701538
    ## Biscoe    0.8666428 0.9701538        NA

# Exercise 4: Test the Function (25 points)

Test the basic functionality of `tukey2matrix`. Specifically, test for
the following:  
1. That the column of the `tukey2matrix()` output matches what we would
expect using the `iris` dataset.  
2. That the values in the column of the `tukey2matrix()` output with
`significance = FALSE` matches what we would expect using the `iris`
dataset.  
3. That the values in the `TukeyHSD()` output are mapped correctly in
`tukey2matrix()`.  
4. That calling a specific element from the `TukeyHSD()` output into
`tukey2matrix()` actually results in an output dataframe from that
element.

``` r
test_that("Basic Functionality Testing", {
  expect_equal(as.list(tukey2matrix(TukeyHSD(aov(Sepal.Length ~ Species, iris)), Species)["versicolor"])[[1]],
               c(as.character(NA), "***", "***")) # Does `versicolor` column match expected  
  
  expect_equal(as.list(tukey2matrix(TukeyHSD(aov(Sepal.Length ~ Species, iris)), Species, significance = FALSE)["virginica"])[[1]],
               c(8.287558e-09   , NA, 2.997602e-15)) # Does `virginica` col match expected values
  
  expect_equal(tukey2matrix(TukeyHSD(aov(Sepal.Length ~ Species, iris)), Species, FALSE)["versicolor", "virginica"],
               TukeyHSD(aov(Sepal.Length ~ Species, iris))$Species['virginica-versicolor', 'p adj']) # Are p-values mapped correctly
  
  expect_equal(colnames(tukey2matrix(TukeyHSD(aov(bill_length_mm ~ species + island, penguins)), island)),
               c("Dream", "Torgersen", "Biscoe")) # Does calling `island` element of TukeyHSD() output actually result in output df of island
  
})
```

    ## Test passed 😸

Additionally, test for common errors. Note that while `tukey2matrix()`
exclusively takes `TukeyHSD()` outputs, it should be able to take any
variation of `TukeyHSD()` output. Specifically:  
1. Test if passing an invalid TukeyHSD element results in an error.  
2. Test if passing a non-TukeyHSD() object results in an error.

``` r
test_that("Basic Error Testing", {
  expect_error(tukey2matrix(TukeyHSD(aov(Sepal.Length ~ Species, iris)), Fake_Col), 
  "Can't subset columns that don't exist.")
  
  expect_error(tukey2matrix(aov(bill_length_mm ~ species + island, penguins)), 
               "Can't subset columns that don't exist")

})
```

    ## Test passed 😸
