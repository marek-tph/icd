context("data sanity")

pkg_ns <- asNamespace("icd")
lazy_env <- pkg_ns$.__NAMESPACE__.$lazydata

test_that("row numbers and factors are sequential for data frames", {
  skip_slow()
  # TODO: also for cached data in ~/.icd.data
  for (data_name in ls(envir = lazy_env)) {
    d <- get(data_name, envir = lazy_env)
    if (!is.data.frame(d) || grepl("map", data_name)) next
    expect_gt(nrow(d), 1)
    expect_gt(ncol(d), 1)
    expect_identical(
      row.names(d),
      as.character(
        seq_along(d[[1]])
      ),
      info = paste("Data = ", data_name)
    )
    for (col in names(d)) {
      info <- paste0(data_name, "[[\"", col, "\"]]")
      if (col %in% c("three_digit")) {
        expect_true(inherits(d[[col]], "icd9") || inherits(d[[col]], "icd10"),
          info = info
        )
      }
      if (inherits(d[[col]], "icd9") || inherits(d[[col]], "icd10")) {
        if (data_name != "vermont_dx" && col != "code") {
          expect_is(d[[col]], "factor", info = info)
        }
      }
      if (is.factor(d[[col]])) {
        if (data_name %nin% c("uranium_pathology", "vermont_dx") &&
          col %nin% c("code", "chapter", "sub_chapter", "major")) {
          expect_false(
            is.unsorted(d[[col]]),
            info = info
          )
          expect_false(
            is.unsorted(levels(d[[col]])),
            info = info
          )
        }
      }
      if (col == "three_digit") {
        expect_true(is.factor(d[[col]]), info = info)
        next
      }
      expect_classes_ordered(d[[col]])
      if (inherits(d[[col]], "icd9cm")) {
        expect_true(inherits(d[[col]], "icd9"))
      }
      if (inherits(d[[col]], "icd9cm") || inherits(d[[col]], "icd9")) {
        expect_is(d[[col]], "character", info = info)
      } # ICD-9
    } # for each col
  } # for each data.frame
})
