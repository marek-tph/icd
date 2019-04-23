# nocov start
.make_icd9cm_leaf_parsers()
.make_icd9cm_rtf_parsers()
.make_icd10cm_parsers()
.make_getters_and_fetchers()
# Set up an environment to cache chars_in_icd10cm
.lookup_chars_in_icd10cm <- new.env(parent = emptyenv())
# env just to hold a function we create on the fly, but don't want to do multiple times
.memoised <- new.env(parent = emptyenv())

.make_httr_retry <- function() {
  if (.have_memoise()) {
    memoise::memoise(
      httr::RETRY,
      cache = memoise::cache_filesystem(
        file.path(
          get_icd_data_dir(),
          "memoise")
      )
    )
  } else {
    httr::RETRY
  }
}

.make_make_memoised <- function() {
  assign("httr_retry", .make_httr_retry(), .memoised)
  assign("erm9", .make_erm9(), .memoised)
  assign("erm10", .make_erm10(), .memoised)
}

# if I generate memoise function each time, it crashes Rstudio, but not R, when
# called! Maybe something to do with memory size creating a huge number of new
# memoised functions, which is not what I intended, anyway.
.make_erm9 <- function() {
  if (requireNamespace("memoise", quietly = TRUE)) {
    memoise::memoise(
      expand_range_major.icd9,
      cache = memoise::cache_filesystem(
        file.path(get_icd_data_dir(), "memoise")
      )
    )
  } else {
    expand_range_major.icd9
  }
}

.make_erm10 <- function() {
  if (requireNamespace("memoise", quietly = TRUE)) {
    memoise::memoise(
      expand_range_major.icd10cm
      # cache = memoise::cache_filesystem(
      #   file.path(get_icd_data_dir(), "memoise")
      # )
    )
  } else {
    expand_range_major.icd10cm
  }
}

.onLoad <- function(libname, pkgname) {
  if (.icd_data_dir_okay()) {
    .set_opt(offline = FALSE, overwrite = FALSE)
  }
  if (is.null(getOption("icd.data.who_url"))) {
    options("icd.data.who_url" = "https://icd.who.int/browse10")
  }
  # need to make these functions at time of package loading: if memoise is
  # present, we use it. We cannot keep recreating the same memoised function
  # multiple times, as it seems to cause Rstudio (but no obviously plain R) to
  # crash.
  .make_make_memoised()
}

.onAttach <- function(libname, pkgname) {
  if (system.file(package = "icd9") != "") {
    packageStartupMessage(paste(
      "The", sQuote("icd9"), "package is now deprecated, and should be removed to avoid",
      "conflicts with ", sQuote("icd"), ". The", sQuote("icd"),
      "package up to version 2.1 contains",
      "tested versions of all the deprecated function names which overlap with",
      "those in the old", sQuote("icd9"), "package, e.g., 'icd9ComorbidAhrq'. It is",
      "strongly recommended to run the command: remove.packages(\"icd9\")"
    ))
  }
  if (interactive()) {
    if (!.exists_icd_data_dir()) {
      packageStartupMessage(
        sQuote("icd"), " downloads data when needed. ",
        "set_icd_data_dir() creates a data directory. "
      )
      packageStartupMessage(
        "Default location is: ", sQuote(.default_icd_data_dir())
      )
    }
    if (system.file(package = "icd.data") != "") {
      packageStartupMessage(
        "N.b. the ", sQuote("icd.data"),
        " package is deprecated from ",
        sQuote("icd"), " version 4.0. ",
        "The content from ", sQuote("icd.data"),
        " is now available via ", sQuote("icd"), "."
      )
    }
  }
}

.onUnload <- function(libpath) {
  library.dynam.unload("icd", libpath)
}

release_questions <- function() {
  c( # vignette
    "manual rebuild of efficiency and country-lang-vers vignettes",
    # commands:
    "update_everything() on linux and mac - should be no diff",
    "aspell_package_Rd_files('.')",
    # documentation:
    "Check all TODO comments, make into github issues",
    "Do all examples look ok (not just run without errors)?",
    "Have all the fixed github issues been closed",
    "pkgdown::build_site()",
    # code quality:
    "codetools::checkUsagePackage('icd', all = TRUE, suppressLocal = TRUE)",
    "styler::style_pkg()",
    "devtools::missing_s3()", # http://r-pkgs.had.co.nz/namespace.html
    "jwutil::jw_scan_build() or use .R/Makevars.clang.scan-build",
    # testing and compilation and different platforms:
    "Are there no skipped tests which should be run?",
    "Travis and appveyor?",
    "rhub::check_with_sanitizers()",
    "rhub::check_for_cran()",
    "Have you checked on Windows, win_builder (if possible with configure script), Mac, Ubuntu, rhub::check_with_sanitizers() etc", # nolint
    "Did you check with verbose, offline, interact, with undefined, TRUE and FALSE", # nolint
    # final manual check:
    "Have all unnecessary files been ignored in built source package?"
  )
}

utils::globalVariables(c(
  "icd9_sub_chapters",
  "icd9_chapters",
  "icd9_majors",
  "icd10_sub_chapters",
  "icd10_chapters",
  "icd10cm2016",
  "icd10cm2019",
  "icd9cm_hierarchy"
))

# nocov end
