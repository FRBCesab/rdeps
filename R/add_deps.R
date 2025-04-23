#' Add required dependencies to the description file
#'
#' @description
#' This function detects external packages used in an R project (package,
#' compendium, website, etc.) and updates the sections **Depends**,
#' **Imports**, and **Suggests** of the `DESCRIPTION` file.
#'
#' A `DESCRIPTION` file can be created and added to an existing project with
#' the function `use_description()` of the package `usethis`.
#'
#' All `.R`, `.Rmd`, and `.qmd` files are screened to identify packages
#' called by `library(foo)`, `library("foo")`, `library('foo')`,
#' `require(foo)`, `require("foo")`, `require('foo')`, `foo::bar()` or
#' `use("foo", "bar")`.
#'
#' Different types of dependencies are handled with the following rules:
#' - if a package is called with `library(foo)` or `require(foo)`,
#' it will be added to the section **Depends** of the `DESCRIPTION` file
#' (except for vignettes and tests);
#' - if the package is called with `foo::bar()` or `use("foo", "bar")`,
#' it will be added to the section **Imports** of the `DESCRIPTION` file
#' (except for vignettes and tests);
#' - if the package is only used in vignettes or tests,
#' it will be added to the section **Suggests** of the `DESCRIPTION` file.
#'
#' This function also screens the `NAMESPACE` file (it detects packages
#' mentioned as `import(foo)` and `importFrom(foo,bar)`) and `@examples`
#' sections of `roxygen2` headers. The detected packages are added in the
#' **Imports** section of the `DESCRIPTION` file.
#'
#' If the project is not an R package, non-standard folders are also screened
#' (i.e. `analyses/`, `paper/`, etc.). The detected packages are added in the
#' **Imports** section of the `DESCRIPTION` file.
#'
#' @return No return value.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' add_deps()
#' }

add_deps <- function() {
  check_for_descr_file()
  path <- path_proj()

  ## Identify dependencies ----

  deps_in_namespace <- get_deps_in_namespace()
  deps_in_functions_r <- get_deps_in_functions(directory = "R")
  deps_in_examples_r <- get_deps_in_examples(directory = "R")

  deps_in_vignettes <- get_deps_in_markdown(directory = "vignettes")
  deps_in_functions_tests <- get_deps_in_functions(directory = "tests")
  deps_in_examples_tests <- get_deps_in_examples(directory = "tests")
  deps_in_markdown_tests <- get_deps_in_markdown(directory = "tests")

  deps_in_extra <- get_deps_in_extra()

  ## Order SUGGESTS dependencies ----

  deps_suggests <- c(
    unlist(deps_in_vignettes),
    unlist(deps_in_functions_tests),
    unlist(deps_in_examples_tests),
    unlist(deps_in_markdown_tests)
  )

  deps_suggests <- sort(unique(deps_suggests))

  ## Order IMPORTS dependencies ----

  deps_imports <- c(
    deps_in_namespace,
    deps_in_functions_r$"imports",
    deps_in_examples_r$"imports",
    deps_in_extra$"imports"
  )

  deps_imports <- sort(unique(deps_imports))

  ## Order DEPENDS dependencies ----

  deps_depends <- c(
    deps_in_functions_r$"depends",
    deps_in_examples_r$"depends",
    deps_in_extra$"depends"
  )

  deps_depends <- sort(unique(deps_depends))

  ## Remove duplicates ----

  pos <- which(deps_suggests %in% deps_imports)
  if (length(pos) > 0) deps_suggests <- deps_suggests[-pos]

  pos <- which(deps_imports %in% deps_depends)
  if (length(pos) > 0) deps_imports <- deps_imports[-pos]

  ## Clean objects ----

  if (length(deps_depends) == 0) deps_depends <- NULL
  if (length(deps_imports) == 0) deps_imports <- NULL
  if (length(deps_suggests) == 0) deps_suggests <- NULL

  ## Read DESCRIPTION file and extract dependencies ----

  descr_file <- read_descr_file()

  descr_depends <- get_deps_in_depends()
  descr_imports <- get_deps_in_imports()
  descr_suggests <- get_deps_in_suggests()

  ## Detect version ----

  all_deps <- c(descr_depends, descr_imports, descr_suggests)

  if (!is.null(all_deps)) {
    versions <- strsplit(all_deps, "\\s{0,}\\(|\\)")

    is_version <- unlist(lapply(versions, function(x) {
      if (length(x) == 2) TRUE else FALSE
    }))

    versions <- versions[is_version]
    pkg_names <- unlist(lapply(versions, function(x) x[1]))
    pkg_versions <- unlist(lapply(versions, function(x) x[2]))
  } else {
    pkg_names <- NULL
    pkg_versions <- NULL
  }

  ## Add version to packages ----

  if (!is.null(pkg_names)) {
    for (i in 1:length(pkg_names)) {
      pos <- which(deps_depends == pkg_names[i])

      if (length(pos) == 1) {
        deps_depends[pos] <- paste0(pkg_names[i], " (", pkg_versions[i], ")")
      }

      pos <- which(deps_imports == pkg_names[i])

      if (length(pos) == 1) {
        deps_imports[pos] <- paste0(pkg_names[i], " (", pkg_versions[i], ")")
      }

      pos <- which(deps_suggests == pkg_names[i])

      if (length(pos) == 1) {
        deps_suggests[pos] <- paste0(pkg_names[i], " (", pkg_versions[i], ")")
      }
    }
  }

  ## Add R minimum version ----

  deps_depends <- c(r_min_version(), deps_depends)

  ## Add packages in DESCRIPTION file ----

  msg_info("Searching for", msg_value('Depends'), "dependencies")

  if (is.null(deps_depends)) {
    pos <- which(colnames(descr_file) == "Depends")
    if (length(pos)) descr_file <- descr_file[, -pos]

    msg_oops("No package found", indent = "    ")
  } else {
    deps_depends_txt <- paste0(deps_depends, collapse = ",\n    ")
    descr_file$"Depends" <- paste0("\n    ", deps_depends_txt)

    msg_done(
      "Found",
      msg_value(length(deps_depends)),
      "package(s)",
      indent = "    "
    )

    msg <- paste0("Depends: ", paste0(deps_depends, collapse = ", "))
    msg_done(
      "Adding the following line in",
      msg_value('DESCRIPTION'),
      indent = "    "
    )
    msg_line(msg_code(msg), indent = "      ")
  }

  msg_info("Searching for", msg_value('Imports'), "dependencies")

  if (is.null(deps_imports)) {
    pos <- which(colnames(descr_file) == "Imports")
    if (length(pos)) descr_file <- descr_file[, -pos]

    msg_oops("No package found", indent = "    ")
  } else {
    deps_imports_txt <- paste0(deps_imports, collapse = ",\n    ")
    descr_file$"Imports" <- paste0("\n    ", deps_imports_txt)

    msg_done(
      "Found",
      msg_value(length(deps_imports)),
      "package(s)",
      indent = "    "
    )

    msg <- paste0("Imports: ", paste0(deps_imports, collapse = ", "))
    msg_done(
      "Adding the following line in",
      msg_value('DESCRIPTION'),
      indent = "    "
    )
    msg_line(msg_code(msg), indent = "      ")
  }

  msg_info("Searching for", msg_value('Suggests'), "dependencies")

  if (is.null(deps_suggests)) {
    pos <- which(colnames(descr_file) == "Suggests")
    if (length(pos)) descr_file <- descr_file[, -pos]

    msg_oops("No package found", indent = "    ")
  } else {
    deps_suggests_txt <- paste0(deps_suggests, collapse = ",\n    ")
    descr_file$"Suggests" <- paste0("\n    ", deps_suggests_txt)

    msg_done(
      "Found",
      msg_value(length(deps_suggests)),
      "package(s)",
      indent = "    "
    )

    msg <- paste0("Suggests: ", paste0(deps_suggests, collapse = ", "))
    msg_done(
      "Adding the following line in",
      msg_value('DESCRIPTION'),
      indent = "    "
    )
    msg_line(msg_code(msg), indent = "      ")
  }

  ## Rewrite DESCRIPTION ----

  write_descr_file(descr_file)

  invisible(NULL)
}
