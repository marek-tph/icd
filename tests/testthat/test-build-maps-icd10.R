context("build icd10 maps")

skip_multi <- function() {
  skip_slow()
  skip_on_appveyor()
  skip_on_travis()
  skip_on_cran()
  skip_if_offline() # also skips if not interactive
  if (!.icd_data_dir_okay()) {
    skip("Don't have icd data cache directory.")
  }
  # could let the functions download automatically, which may be useful for automated testing on new platforms, but probably not suitable for CI, and certainly not CRAN.
  if (!.all_cached()) {
    skip("Don't have all data cached, yet.")
  }
}

test_that("the icd-10 quan elix comorbidity map is reproduced", {
  skip_multi()
  expect_equivalent(icd10_map_quan_elix, icd10_generate_map_quan_elix(save_pkg_data = FALSE))
})

test_that("the icd-10 quan deyo comorbidity map is reproduced", {
  skip_multi()
  expect_equivalent(icd10_map_quan_deyo, icd10_generate_map_quan_deyo(save_pkg_data = FALSE))
})

test_that("the icd-10 elix comorbidity map is reproduced", {
  skip_multi()
  expect_equivalent(icd10_map_elix, icd10_generate_map_elix(save_pkg_data = FALSE))
})

test_that("icd-10 ahrq map is reproduced", {
  skip_if_offline()
  if (is.null(icd10_fetch_ahrq_sas())) {
    skip("AHRQ ICD-10 SAS must be downloaded with icd10_fetch_ahrq_sas")
  }
  expect_equivalent(icd10_map_ahrq, icd10_parse_ahrq_sas(save_data = FALSE))
})
