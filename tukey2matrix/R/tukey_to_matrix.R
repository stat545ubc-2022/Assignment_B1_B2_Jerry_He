#' Convert TukeyHSD() Output to a Matrix Array
#'
#' Create a user-readable matrix array from the output of TukeyHSD(). Takes the output of TukeyHSD() and converts the output object into a matrix array dataframe.
#'
#'
#' @param tukey_object The input data. Should be a TukeyHSD() object. Named tukey_object since input should be a TukeyHSD() object.
#' @param element The element of the TukeyHSD() object to subset and convert. Does not need to be a string. Named element since this should be the element from the TukeyHSD() object that wants to be extracted.
#' @param significance Defaults to TRUE. Should statistically significant p-values be replaces with asterisks? Named significance since this controls if significant p-values are replaced with asterisks.
#'
#' @return A dataframe object with rows and columns of comparisons.
#' @export
#' @importFrom data.table :=
#' @examples
#' AOV_Out <- aov(Petal.Length ~ Species, iris)
#' TukeyHSD_Out <- TukeyHSD(AOV_Out)
#' tukey_to_matrix(tukey_object = TukeyHSD_Out, element = Species)
#' tukey_to_matrix(TukeyHSD_Out, Species, significance = FALSE)
tukey_to_matrix <- function(tukey_object, element, significance = TRUE) {

  # Turn TukeyHSD output into a dataframe and turn the rownames into a column
  data <- as.data.frame(tukey_object[[deparse(substitute(element))]]) %>%
    tibble::rownames_to_column("Comparison")

  # Select the Comparison and `p adj` columns and separate comparison into the two conditions that are being compared
  data <- data %>%
    dplyr::select("Comparison", "p adj") %>%
    tidyr::separate("Comparison", into = c("X1", "X2"), sep = "-")

  # Generate a vector containing all of the unique conditions you are comparing
  comparisons <- unique(c(data$X1, data$X2))

  # Generate an empty dataframe with the comparison names as row and colnames
  out_dat <- data.frame(row.names = comparisons)
  for (i in comparisons) {
    out_dat <- out_dat %>%
      dplyr::mutate("{i}" := NA) # dynamically name cols by comparison name
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
