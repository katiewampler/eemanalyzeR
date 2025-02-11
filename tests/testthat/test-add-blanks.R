#note: due to the user input, tests are quite challenging to write as it requires the relatively new
  #with_mocked_bindings function from withr. It took a while to figure out how to use the function, but finally
  #the solution from snaut here worked: https://stackoverflow.com/questions/51294489/how-to-test-behavior-that-depends-on-a-package-being-installed-or-not


#test that blank list is pulled correctly

#blanklist <- blanklist[order(c(3,1,2))] #check if blanks are in different order are they still matched correctly?

test_that("blanks are added when a list of blanks is supplied", {
  #gives error if names don't match
})

test_that("blanks are added when a single blank is supplied", {

})

#readline <- NULL
#with_mocked_bindings(
#  readline = function(prompt) "Y",
#  test_that("blanks are added when only a list of samples and blanks are supplied", {
#    eemlist <- add_metadata(metadata, example_eems)
#    augment_eemlist <- add_blanks(eemlist)
#    expect_s3_class(augment_eemlist, "eemlist")
    #how to check blanks were added correctly, snapshot??


#  })
#)

test_that("a yes returns an eemlist", {

})

test_that("a no, returns an error", {

})
