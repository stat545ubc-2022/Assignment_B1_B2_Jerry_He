test_that("Basic Functionality Testing", {
  expect_equal(as.list(tukey_to_matrix(TukeyHSD(aov(Sepal.Length ~ Species, iris)), Species)["versicolor"])[[1]],
               c(as.character(NA), "***", "***")) # Does `versicolor` column match expected

  expect_equal(as.list(tukey_to_matrix(TukeyHSD(aov(Sepal.Length ~ Species, iris)), Species, significance = FALSE)["virginica"])[[1]],
               c(8.287558e-09	, NA, 2.997602e-15)) # Does `virginica` col match expected values

  expect_equal(tukey_to_matrix(TukeyHSD(aov(Sepal.Length ~ Species, iris)), Species, FALSE)["versicolor", "virginica"],
               TukeyHSD(aov(Sepal.Length ~ Species, iris))$Species['virginica-versicolor', 'p adj']) # Are p-values mapped correctly

})

test_that("Basic Error Testing", {
  expect_error(tukey_to_matrix(TukeyHSD(aov(Sepal.Length ~ Species, iris)), Fake_Col),
               "Can't subset columns that don't exist.")


})
