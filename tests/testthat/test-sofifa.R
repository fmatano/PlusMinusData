context("sofifa.com scraper")

test_that("sofifa scraper works", {
  example_url <- "https://sofifa.com/players/?v=07&offset=0"

  sotab1 <- scrape_sofifa_table(url=example_url)
  expect_true(nrow(sotab1$sofifa_table) == 81)
  expect_true(class(sotab1$link_title_df) == "data.frame")

  sotab1 <- clean_sofifa_table(tab=sotab1)
  expect_true(class(sotab1) == "data.frame")
  expect_true(nrow(sotab1) == 80)

  example_url2 <- "https://sofifa.com/players/?v=07&offset=80"
  sotab2 <- scrape_sofifa_table(url=example_url2)
  sotab2 <- clean_sofifa_table(tab=sotab2)

  expect_true( all(as.numeric(sotab1[80, "overall"]) >= as.numeric(sotab2$overall)) )
})
