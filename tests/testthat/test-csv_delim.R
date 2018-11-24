context("test-csv_delim.R")

encoding <- 'UTF-8'

out1 <- csv_delim("a,b\n1,2,3\n")

test_that("csv_delim can comma separated", {
  # Two rows correctly formatted
  expect_equal(csv_delim("1,2,3\n4,5,6", encoding), ",")

  # Can read with empty row
  expect_equal(csv_delim("a,b\n\n\n1,2", encoding), ",")

  # Can read many empty rows
  expect_equal(csv_delim("a,b\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n1,2", encoding), ",")

  # Can read empty rows at beginnin
  expect_equal(csv_delim("\n\na,b\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n1,2", encoding), ",")

  # Can read empty rows at end
  expect_equal(csv_delim("a,b\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n1,2\n", encoding), ",")

  # Can read empty rows at begin and end
  expect_equal(csv_delim("\na,b\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n1,2\n", encoding), ",")
})

# csv_delim("v1,v2
#             \n
#             1,2
#             \n
#             3,4
#             \n
#             5,6
#             ")
#
# csv_delim("a,b\n1,2\n\n\n2,3\n")
#
# csv_delim("a,b\n1,2\n\n\n")
#
# # Fails because the , is not repeated in over 90% of rows
# csv_delim("v1,v2\n#foo\n1,2\n#bar\n3,4")
#
# csv_delim("x1,x2,x3``
#           \n
#           A2,B2,C2
#           \n
#           A3#,B2,C2
#           \n
#           A4,A5,A6")
