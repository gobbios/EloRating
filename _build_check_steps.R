# this is a set of instructions to check whether the package conforms to CRAN rules
# these steps here are taken from https://github.com/paul-buerkner/brms/issues/158

devtools::clean_vignettes()
devtools::document()
devtools::clean_dll()
withr::with_envvar(c("NOT_CRAN" = "true"), devtools::build_vignettes())

# size reductions of PDF vignettes
vignettepath <- "doc/EloRating_tutorial.pdf"
if (!file.exists(vignettepath)) stop()
tools::compactPDF(paths = vignettepath, verbose = TRUE, gs_quality = "ebook")
# for whatever reason: compacting currently only works on my windows pc...

# build source package
withr::with_envvar(c("NOT_CRAN" = "true"), devtools::build(args = c('--compact-vignettes=both')))

# check source package (update file name if applicable)
withr::with_envvar(c("NOT_CRAN" = "true"), devtools::check_built("../EloRating_0.46.17.tar.gz", args = "--as-cran"))

# then upload .tar.gz to wincheck
# https://win-builder.r-project.org/upload.aspx
# download zip for github release
# create mac binary for release too
devtools::build(binary = TRUE)


# foghorn::cran_incoming()
