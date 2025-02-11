#test yes or no function

  with_mocked_bindings(
    .yesorno = function(question,
                        y_response,
                        n_response) TRUE,
    test_that("a y returns a positive response", {
      expect_true(.yesorno("does this work", "yes", "no"))
    })
  )
