context("data sanity")

test_that("row numbers and factors are sequential for data frames", {
  skip_slow()
  for (data_name in c(.ls_lazy(), .ls())) {
    d <- .get_anywhere(data_name)
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
    # check that three digits are the three-digit codes of the code:
    if (all(c("code", "three_digit") %in% names(d))) {
      four_digit_majors <- nchar(as_char_no_warn(d$three_digit)) == 4
      d_three <- d[!four_digit_majors, ]
      d_four <- d[four_digit_majors, ]
      expect_equivalent(
        unclass(substr(as_char_no_warn(d_three$code), 1, 3)),
        as_char_no_warn(d_three$three_digit),
        info = info
      )
      expect_equivalent(
        unclass(substr(as_char_no_warn(d_four$code), 1, 4)),
        as_char_no_warn(d_four$three_digit),
        info = info
      )
    }
    # now check each column
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
      if (data_name %nin% c("uranium_pathology",
                            "vermont_dx") &&
          col %nin% c(#"code",
                      "chapter",
                      "sub_chapter",
                      "major",
                      "billable",
                      "short_desc",
                      "long_desc")) {
        if (is.factor(d[[col]])) {
          levs <- levels(d[[col]])
          class(levs) <- sub("factor", "character", class(d[[col]]))
          expect_false(
            is_unsorted(levs),
            info = paste0("levels(", info, "), class(levs) = ",
                          paste(class(levs), collapse = ", "))
          )
        }
        expect_false(
          is_unsorted(d[[col]]),
          info = info
        )
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
