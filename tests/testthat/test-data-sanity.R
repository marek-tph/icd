context("data sanity")

test_that("row numbers and factors are sequential for data frames", {
  skip_slow()
  data_names <- c("icd10be2014",
                  "icd10who2016",
                  "icd10who2008fr",
                  "icd9cm2008",
                  "icd10fr2019"
  )
  # data_names <- c(rev(.ls()), .ls_lazy())
  for (data_name in data_names) {
    dinfo <- paste0("get_", data_name, "()")
    if (.verbose()) print(paste0("data frame: ", data_name))
    d <- .get_anywhere(data_name)
    if (!is.data.frame(d) || grepl("map", data_name)) next
    expect_gt(nrow(d), 1)
    expect_gt(ncol(d), 1)
    expect_identical(
      row.names(d),
      as.character(
        seq_along(d[[1]])
      ),
      info = dinfo
    )
    for (col_name in names(d)) {
      col_dat <- d[[col_name]]
      if (.verbose() > 2) print(paste0("now checking column: ", col_name))
      info <- paste0("get_", data_name, "()[[\"", col_name, "\"]]")
      if (.verbose() > 2) print("checking three_digit is right class")
      if (col_name %in% c("three_digit")) {
        expect_true(inherits(col_dat, "icd9") || inherits(col_dat, "icd10"),
                    info = info
        )
      }
      if (col_name %in% c("code", "three_digit")) {
        test_that("codes are valid", {
          j <- d[[col_name]]
          expect_valid(j, whitespace_ok = FALSE)
        })
        test_that("three-digits match the codes", {
          four_digit_majors <- nchar(as_char_no_warn(d$three_digit)) == 4
          d_three <- d[!four_digit_majors, ]
          d_four <- d[four_digit_majors, ]
          expect_equivalent(
            unclass(substr(as_char_no_warn(d_three$code), 1, 3)),
            as_char_no_warn(d_three$three_digit),
            info = dinfo
          )
          expect_equivalent(
            unclass(substr(as_char_no_warn(d_four$code), 1, 4)),
            as_char_no_warn(d_four$three_digit),
            info = dinfo
          )
        })
        if (col_name == "code") {
          expect_is(d[[col_name]], "character", info = info)
        }
        if (.verbose()) print("checking code columns are sorted")
        if (data_name %nin% c(
          "uranium_pathology",
          "vermont_dx")
        ) {
          if (is.factor(col_dat)) {
            levs <- levels(col_dat)
            class(levs) <- sub("factor", "character", class(col_dat))
            expect_false(
              is_unsorted(levs),
              info = paste0(
                "levels(", info, "), class(levs) = ",
                paste(class(levs), collapse = ", ")
              )
            )
          }
          expect_true(col_name %in% names(d))
          expect_true(!is.null(col_dat))
          expect_false(
            is_unsorted(col_dat),
            info = info
          )
        }
        if (.verbose()) print("checking three_digit is a factor")
        if (col_name == "three_digit") {
          test_that("three_digit is a factor", {
            expect_true(is.factor(col_dat), info = info)
          })
          # next
        }
        test_that("classes are ordered", {
          if (inherits(col_dat, c("icd9", "icd10"))) {
            expect_classes_ordered(col_dat)
          }
        })
        if (.verbose()) print("icd9cm is also icd9")
        if (inherits(col_dat, "icd9cm")) {
          expect_true(inherits(d[[col_name]], "icd9"))
        }
        if (inherits(col_dat, icd9_classes)) {
          expect_is(col_dat, "icd9")
        }
        if (inherits(col_dat, icd10_classes)) {
          expect_is(col_dat, "icd10")
        }
        if (inherits(col_dat, "icd9") || inherits(col_dat, "icd10")) {
          if (data_name != "vermont_dx" && col_name != "code") {
            test_that("ICD data columns are factors", {
              expect_is(col_dat, "factor", info = info)
            })
          }
        }
        # end code or three digit only block
      } else {
        # columns not named code or three_digit should not inherit an ICD code type?
        expect_false(inherits(col_dat, icd_data_classes))
      }
    } # for each col_name
  } # for each data.frame
})
