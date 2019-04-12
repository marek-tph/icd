icd_data_icd9cm_leaf_v32 <- function() {
  if (exists("icd9cm2014_leaf")) return(get("icd9cm2014_leaf"))
  get_icd9cm2014_leaf()
}

# alt is alternative data, not a variable name.
.idget <- function(var_name, alt = NULL, must_work = is.null(alt), ...) {
  stop("obsolete")
  # this should only happen in weird R CMD check internals since we Import
  # icd.data...
  if (!.exists_anywhere(var_name)) {
    msg <- paste(
      "This data is not available with icd.data version < 1.1",
      "Upgrade icd.data using install.packages(\"icd.data\"."
    )
    if (must_work) stop(msg, " Cannot proceed.")
    return(alt)
  }
  .get_anywhere(var_name)
}

.get_lazy <- function(var_name) {
  ns <- asNamespace("icd")
  lz <- ns$.__NAMESPACE__.$lazydata
  get(var_name, lz)
}

.exists_in_lazy <- function(var_name) {
  ns <- asNamespace("icd")
  lz <- ns$.__NAMESPACE__.$lazydata
  exists(var_name, lz)
}

.get_anywhere <- function(var_name, fetch = FALSE) {
  ns <- asNamespace("icd")
  if (.exists_in_lazy(var_name)) return(.get_lazy(var_name))
  if (.exists_in_cache(var_name)) return(.get_from_cache(var_name))
  if (exists(var_name, ns)) return(get(var_name, ns))
  if (fetch && exists(.get_fetcher_name(var_name), ns, mode = "function")) {
    return(.get_fetcher_fun(var_name)())
  }
  .absent_action_switch(
    paste(var_name, "not available in icd.data regular or lazy data"))
}

.exists_anywhere <- function(var_name) {
  ns <- asNamespace("icd")
  if (.exists_in_lazy(var_name) || exists(var_name, ns)) return(TRUE)
  if (exists(.get_fetcher_name(var_name), ns, mode = "function")) {
    dat <- with_absent_action("silent", .get_fetcher_fun(var_name))
    if (!is.null(dat) && is.data.frame(dat)) return(TRUE)
  }
  FALSE
}
