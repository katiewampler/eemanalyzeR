#used to check if tests sufficiently check all of the code in the function
  #see covr for more details

#test all package coverage (takes a minute because it runs all tests)
    output <- package_coverage(path=getwd())

    report(output) #used to visualize the spots where there's gaps in testing
