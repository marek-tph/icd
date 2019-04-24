context("data sanity")

test_that("row numbers and factors are sequential for data frames", {
  skip_slow()
  # print("may have to increase ulimit -s to e.g. 64000 for this to run")
  for (data_name in c(.ls_lazy(), .ls())) {
    if (.verbose()) print("data frame: ", data_name)
    if (.verbose() > 5) {
      print(Cstack_info())
      print(sys.calls())
      #gcinfo(TRUE)
      }
    gc()
   # if (.verbose() > 5) gcinfo(FALSE)
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
    if (all(c("code", "three_digit") %in% names(d))) {
      test_that("three-digits match the codes", {
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
      })
    }
    test_that("checking each column", {
      #if (.verbose() > 2) print("now check each column: ", col)
      for (col in names(d)) {
        if (.verbose() > 5) { print(Cstack_info()); print(sys.calls()); gcinfo(TRUE); }
        gc()
        if (.verbose() > 5) gcinfo(FALSE)
        info <- paste0(data_name, "[[\"", col, "\"]]")
        if (.verbose() > 2) print("checking three_digit is right class")
        if (col %in% c("three_digit")) {
          expect_true(inherits(d[[col]], "icd9") || inherits(d[[col]], "icd10"),
                      info = info
          )
        }
        if (inherits(d[[col]], "icd9") || inherits(d[[col]], "icd10")) {
          if (data_name != "vermont_dx" && col != "code") {
            test_that("ICD data columns are factors", {
              expect_is(d[[col]], "factor", info = info)
            })
          }
        }
        if (.verbose()) print("checking sorted")
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
          test_that("three_digit is a factor", {
            expect_true(is.factor(d[[col]]), info = info)

          })
          # next
        }

        test_that("classes are ordered", {
          expect_classes_ordered(d[[col]])
        })
        if (.verbose()) print("icd9cm is also icd9")
        if (inherits(d[[col]], "icd9cm")) {
          expect_true(inherits(d[[col]], "icd9"))
        }
        if (inherits(d[[col]], "icd9cm") || inherits(d[[col]], "icd9")) {
          expect_is(d[[col]], "character", info = info)
        } # ICD-9
      } # for each col
    }) # test_that
  } # for each data.frame
})

context("done with sanity")
test_that("done", {
  expect_true(TRUE)
})

