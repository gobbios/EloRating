# this is a set of instructions to check whether the package conforms to CRAN rules
# look at test coverage
# cov <- covr::package_coverage()
# devtools::test_coverage()
# devtools::test_coverage_active_file()

devtools::document()
devtools::clean_dll()
# need checkbashisms
# brew install checkbashisms
# need GS
stopifnot(file.exists("/opt/homebrew/bin/gs"))
Sys.setenv(R_GSCMD = "/opt/homebrew/bin/gs")
# build:
tarball <- withr::with_envvar(
  c(NOT_CRAN = "true"),
  devtools::build(args = "--compact-vignettes=both")
)

# check tarball
devtools::check_built(tarball, args = "--as-cran", env_vars = c(NOT_CRAN = "true"))

# remote checks instead of manual uploads
devtools::check_win_devel(args = "--compact-vignettes=both") # instead of manual browser-based winbuilder upload

# foghorn::winbuilder_queue()

# optional: upload zip/tarball/tgz as release on github
# download zip from winbuilder for github release
devtools::build(binary = TRUE, args = "--compact-vignettes=both")

# finally: submit to CRAN
# check cran-comments.mds
devtools::submit_cran(args = "--compact-vignettes=both")

# foghorn::cran_incoming()

