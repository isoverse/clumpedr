context("test-defaults")

# probably, I should just check if they're doubles/numerics?
test_that("defaults are set correctly", {
  expect_equal(default(R13_PDB), 0.01118)
  expect_equal(default(R18_PDB), 1.008751)
  expect_equal(default(R17_PDBCO2), 0.0003931)
  expect_equal(default(R18_PDBCO2), 0.00208839)
  expect_equal(default(lambda), 0.528)
  expect_equal(default(D47), 0)
  expect_equal(default(D48), 0)
  expect_equal(default(D49), 0)
  expect_equal(default(D17O), 0)
})
