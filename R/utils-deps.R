#' **Detect dependencies in Depends field of DESCRIPTION**
#'
#' @noRd

get_deps_in_depends <- function() {
  descr <- read_descr_file()

  if (!is.null(descr$"Depends")) {
    deps <- get_deps_in_field(descr$"Depends")
  } else {
    deps <- NULL
  }

  deps
}


#' **Detect dependencies in Imports field of DESCRIPTION**
#'
#' @noRd

get_deps_in_imports <- function() {
  descr <- read_descr_file()

  if (!is.null(descr$"Imports")) {
    deps <- get_deps_in_field(descr$"Imports")
  } else {
    deps <- NULL
  }

  deps
}


#' **Detect dependencies in Suggests field of DESCRIPTION**
#'
#' @noRd

get_deps_in_suggests <- function() {
  descr <- read_descr_file()

  if (!is.null(descr$"Suggests")) {
    deps <- get_deps_in_field(descr$"Suggests")
  } else {
    deps <- NULL
  }

  deps
}


#' **Get R minimum version**
#'
#' @noRd

r_min_version <- function() {
  check_for_descr_file()

  descr_file <- read_descr_file()

  if ("Depends" %in% colnames(descr_file)) {
    deps <- unlist(strsplit(descr_file$"Depends", ","))

    deps <- gsub("\\(\\s{0,}", " (", deps)
    deps <- gsub("\\s{0,}\\)", ")", deps)
    deps <- gsub("=", "= ", deps)
    deps <- gsub(">", "> ", deps)
    deps <- gsub("<", "< ", deps)

    deps <- gsub("\\s{0,}\\)", ")", deps)
    deps <- gsub("\\s+", " ", deps)
    deps <- trimws(deps)

    deps <- gsub("< =", "<=", deps)
    deps <- gsub("> =", ">=", deps)
    deps <- gsub("= =", "==", deps)

    r_version <- deps[grep("^R\\s{0,}\\(", deps)]

    if (length(r_version) == 0) {
      r_version <- NULL
    }
  } else {
    r_version <- NULL
  }

  r_version
}


#' **Remove R minimum version from list of packages**
#'
#' @noRd

remove_r_min_version <- function(dependencies) {
  r_version <- grep("^R\\s{0,}\\(", dependencies)

  if (length(r_version)) {
    dependencies <- dependencies[-r_version]
  }

  dependencies
}


#' **Extract and clean list of packages in a particular DESCRIPTION field**
#'
#' @noRd

get_deps_in_field <- function(field) {
  deps <- unlist(strsplit(field, "\n\\s{0,}|,|,\\s{0,}"))

  deps <- gsub("\\(\\s{0,}", " (", deps)
  deps <- gsub("\\s{0,}\\)", ")", deps)
  deps <- gsub("=", "= ", deps)
  deps <- gsub(">", "> ", deps)
  deps <- gsub("<", "< ", deps)

  deps <- gsub("\\s{0,}\\)", ")", deps)
  deps <- gsub("\\s+", " ", deps)
  deps <- trimws(deps)

  deps <- gsub("< =", "<=", deps)
  deps <- gsub("> =", ">=", deps)
  deps <- gsub("= =", "==", deps)

  deps <- deps[!(deps == "")]

  remove_r_min_version(deps)
}


#' **Extract and clean list of packages in NAMESPACE**
#'
#' Detect dependencies as `import(pkg)` and `importFrom(pkg,fun)`.
#'
#' @noRd

get_deps_in_namespace <- function() {
  check_for_descr_file()

  path <- path_proj()

  if (file.exists(file.path(path, "NAMESPACE"))) {
    namespace <- readLines(con = file.path(path, "NAMESPACE"), warn = FALSE)
    namespace <- namespace[grep("^\\s{0,}import", namespace)]

    deps <- gsub("importFrom\\s{0,}\\(|import\\s{0,}\\(|\\)", "", namespace)
    deps <- gsub("\\s+", " ", deps)
    deps <- trimws(deps)

    if (length(deps) == 0) {
      deps <- NULL
    } else {
      deps <- strsplit(deps, "\\s{0,},\\s{0,}")
      deps <- lapply(deps, function(x) x[1])
      deps <- sort(unique(unlist(deps)))
    }
  } else {
    deps <- NULL
  }

  deps
}


#' **Extract and clean list of packages in the R folder**
#'
#' Detect dependencies in R functions written as `foo::bar()`, `library(foo)`,
#' `library("foo")`, `library('foo')`, `require(foo)`, `require("foo")`,
#' `require('foo')`.
#'
#' @return A named `list` of two `character` vectors:
#'   - `depends`, packages called with `library(foo)` and `require(foo)`;
#'   - `imports`, packages called with `foo:bar()`.
#'
#' If a package is called with `library(foo)` and `foo:bar()`, it will be added
#' only to `depends`.
#'
#' @noRd

get_deps_in_functions <- function(directory = "R") {
  check_for_descr_file()

  path <- path_proj()

  ## No `directory` folder ----

  if (!dir.exists(file.path(path, directory))) {
    return(list("depends" = NULL, "imports" = NULL))
  }

  r_files <- list.files(
    path = file.path(path, directory),
    pattern = "\\.R$",
    full.names = TRUE,
    ignore.case = TRUE,
    recursive = TRUE
  )

  r_files <- c(
    r_files,
    list.files(
      path = file.path(path),
      pattern = "\\.R$",
      full.names = TRUE,
      ignore.case = TRUE,
      recursive = FALSE
    )
  )

  ## No .R files in `directory` ----

  if (!length(r_files)) {
    return(list("depends" = NULL, "imports" = NULL))
  }

  ## Read R files ----

  content <- lapply(r_files, function(x) readLines(con = x, warn = FALSE))

  ## Remove comments ----

  content <- remove_comment_lines(content)

  ## Remove messages ----

  content <- remove_messages(content)

  ## Functions called as pkg::fun() ----

  deps_imports <- c(
    get_colon_syntax_deps(content),
    get_use_deps(content)
  )

  ## Attached Packages (library & require) ----

  deps_depends <- get_attached_deps(content)

  ## Remove duplicates ----

  pos <- which(deps_imports %in% deps_depends)

  if (length(pos) > 0) deps_imports <- deps_imports[-pos]

  ## Remove project name ----

  pos <- which(deps_depends == basename(path))
  if (length(pos) > 0) deps_depends <- deps_depends[-pos]

  pos <- which(deps_imports == basename(path))
  if (length(pos) > 0) deps_imports <- deps_imports[-pos]

  ## Clean objects ----

  if (length(deps_depends) == 0) deps_depends <- NULL
  if (length(deps_imports) == 0) deps_imports <- NULL

  list("depends" = deps_depends, "imports" = deps_imports)
}


#' **Detect dependencies in @examples**
#'
#' Detect dependencies in **@examples** written as `foo::bar()`, `library(foo)`,
#' `library("foo")`, `library('foo')`, `require(foo)`, `require("foo")`,
#' `require('foo')`.
#'
#' @return A named `list` of two `character` vectors:
#'   - `depends`, packages called with `library(foo)` and `require(foo)`;
#'   - `imports`, packages called with `foo:bar()`.
#'
#' If a package is called with `library(foo)` and `foo:bar()`, it will be added
#' only to `depends`.
#'
#' @noRd

get_deps_in_examples <- function(directory = "R") {
  check_for_descr_file()

  path <- path_proj()

  ## No `directory` folder ----

  if (!dir.exists(file.path(path, directory))) {
    return(list("depends" = NULL, "imports" = NULL))
  }

  r_files <- list.files(
    path = file.path(path, directory),
    pattern = "\\.R$",
    full.names = TRUE,
    ignore.case = TRUE,
    recursive = TRUE
  )

  r_files <- c(
    r_files,
    list.files(
      path = file.path(path),
      pattern = "\\.R$",
      full.names = TRUE,
      ignore.case = TRUE,
      recursive = FALSE
    )
  )

  ## No .R files in `directory` ----

  if (!length(r_files)) {
    return(list("depends" = NULL, "imports" = NULL))
  }

  ## Read R files ----

  content <- lapply(r_files, function(x) readLines(con = x, warn = FALSE))

  ## Select roxygen2 headers ----

  content <- lapply(content, function(x) x[grep("^\\s{0,}#'", x)])

  ## Select @examples sections (dirty code, but working) ----

  ex_start <- lapply(content, function(x) grep("^\\s{0,}#'\\s{0,}@examples", x))
  ex_end <- lapply(content, function(x) grep("#'\\s{0,}@", x))

  examples <- list()

  for (i in seq_len(length(ex_start))) {
    examples[[i]] <- character(0)

    if (length(ex_start[[i]])) {
      for (j in seq_len(length(ex_start[[i]]))) {
        pos <- ex_end[[i]][ex_end[[i]] > ex_start[[i]][j]]

        if (length(pos)) {
          examples[[i]] <- c(
            examples[[i]],
            content[[i]][(ex_start[[i]][j] + 1):(pos[1] - 1)]
          )
        } else {
          examples[[i]] <- c(
            examples[[i]],
            content[[i]][(ex_start[[i]][j] + 1):length(content[[i]])]
          )
        }
      }
    }
  }

  content <- examples

  ## Remove comments ----

  content <- remove_rd_comment_lines(content)

  ## Remove messages ----

  content <- remove_messages(content)

  ## Functions called as pkg::fun() ----

  deps_imports <- c(
    get_colon_syntax_deps(content),
    get_use_deps(content)
  )

  ## Attached Packages (library & require) ----

  deps_depends <- get_attached_deps(content)

  ## Remove duplicates ----

  pos <- which(deps_imports %in% deps_depends)

  if (length(pos) > 0) deps_imports <- deps_imports[-pos]

  ## Remove project name ----

  pos <- which(deps_depends == basename(path))
  if (length(pos) > 0) deps_depends <- deps_depends[-pos]

  pos <- which(deps_imports == basename(path))
  if (length(pos) > 0) deps_imports <- deps_imports[-pos]

  ## Clean objects ----

  if (length(deps_depends) == 0) deps_depends <- NULL
  if (length(deps_imports) == 0) deps_imports <- NULL

  list("depends" = deps_depends, "imports" = deps_imports)
}


#' **Detect dependencies in Markdown files**
#'
#' Detect dependencies in `.Rmd` and `.qmd` as `pkg::fun()`, `library(pkg)`,
#' and `require(pkg)`.
#'
#' If `.Rmd` files are detected, the packages `knitr` and `rmarkdown` are added
#' to the list of packages.
#'
#' If `.qmd` files are detected, the package `quarto` is added to the list of
#' packages.
#'
#' @noRd

get_deps_in_markdown <- function(directory = "vignettes") {
  check_for_descr_file()

  path <- path_proj()

  ## No `directory` folder ----

  if (!dir.exists(file.path(path, directory))) {
    return(list("depends" = NULL, "imports" = NULL))
  }

  packages_to_add <- NULL

  ## Search for Rmd ----

  rmd_files <- list.files(
    path = file.path(path, directory),
    pattern = "\\.Rmd$",
    full.names = TRUE,
    ignore.case = TRUE,
    recursive = TRUE
  )

  rmd_files <- c(
    rmd_files,
    list.files(
      path = file.path(path),
      pattern = "\\.Rmd$",
      full.names = TRUE,
      ignore.case = TRUE,
      recursive = FALSE
    )
  )

  read_me <- which(tolower(basename(rmd_files)) == "readme.rmd")

  if (length(read_me) > 0) {
    rmd_files <- rmd_files[-read_me]
  }

  if (length(rmd_files) > 0) {
    packages_to_add <- c(packages_to_add, "knitr", "rmarkdown")
  }

  ## Search for qmd ----

  qmd_files <- list.files(
    path = file.path(path, directory),
    pattern = "\\.qmd$",
    full.names = TRUE,
    ignore.case = TRUE,
    recursive = TRUE
  )

  qmd_files <- c(
    qmd_files,
    list.files(
      path = file.path(path),
      pattern = "\\.qmd$",
      full.names = TRUE,
      ignore.case = TRUE,
      recursive = FALSE
    )
  )

  read_me <- which(tolower(basename(qmd_files)) == "readme.qmd")

  if (length(read_me) > 0) {
    qmd_files <- qmd_files[-read_me]
  }

  if (length(qmd_files) > 0) {
    packages_to_add <- c(packages_to_add, "knitr", "rmarkdown")
  }

  ## Append files ----

  md_files <- c(rmd_files, qmd_files)

  ## No md files in `directory` ----

  if (!length(md_files)) {
    return(list("depends" = NULL, "imports" = NULL))
  }

  ## Read md files ----

  content <- lapply(md_files, function(x) readLines(con = x, warn = FALSE))

  ## Extract code chunks ----

  content <- get_code_chunk(content)

  ## Remove comments ----

  content <- remove_comment_lines(content)

  ## Remove messages ----

  content <- remove_messages(content)

  ## Functions called as pkg::fun() ----

  deps_imports <- c(
    get_colon_syntax_deps(content),
    get_use_deps(content)
  )

  ## Add additional packages ----

  deps_imports <- c(deps_imports, packages_to_add)
  deps_imports <- sort(unique(deps_imports))

  ## Attached Packages (library & require) ----

  deps_depends <- get_attached_deps(content)

  ## Remove duplicates ----

  pos <- which(deps_imports %in% deps_depends)

  if (length(pos) > 0) deps_imports <- deps_imports[-pos]

  ## Remove project name ----

  pos <- which(deps_depends == basename(path))
  if (length(pos) > 0) deps_depends <- deps_depends[-pos]

  pos <- which(deps_imports == basename(path))
  if (length(pos) > 0) deps_imports <- deps_imports[-pos]

  ## Clean objects ----

  if (length(deps_depends) == 0) deps_depends <- NULL
  if (length(deps_imports) == 0) deps_imports <- NULL

  list("depends" = deps_depends, "imports" = deps_imports)
}


#' **Remove comment lines**
#'
#' Remove comment lines in .R files
#'
#' @noRd

remove_comment_lines <- function(x) {
  lapply(x, function(x) {
    gsub("#.*", "", x)
  })
}


#' **Remove comment lines in @examples**
#'
#' Remove comment lines in .R files in `@examples`
#'
#' @noRd

remove_rd_comment_lines <- function(x) {
  lapply(x, function(x) {
    gsub("(#'.*)(#.*)", "\\1", x)
  })
}


#' **Remove messages**
#'
#' Remove messages in `stop()`, `messages()`, `cat()`, `print()`, etc.
#'
#' @noRd

remove_messages <- function(x) {
  x <- lapply(x, function(x) {
    pos <- grep(
      paste0(
        "^\\s{0,}use\\s{0,}\\(|",
        "^\\s{0,}library\\s{0,}\\(|",
        "^\\s{0,}require\\s{0,}\\("
      ),
      x
    )
    if (length(pos) > 0) {
      x[-pos] <- gsub("\".{0,}\'.{0,}\'.{0,}\"", "", x[-pos])
    } else {
      x <- gsub("\".{0,}\'.{0,}\'.{0,}\"", "", x)
    }
    x
  })

  x <- lapply(x, function(x) {
    pos <- grep(
      paste0(
        "^\\s{0,}use\\s{0,}\\(|",
        "^\\s{0,}library\\s{0,}\\(|",
        "^\\s{0,}require\\s{0,}\\("
      ),
      x
    )
    if (length(pos) > 0) {
      x[-pos] <- gsub("\'.{0,}\".{0,}\".{0,}\'", "", x[-pos])
    } else {
      x <- gsub("\'.{0,}\".{0,}\".{0,}\'", "", x)
    }
    x
  })

  x <- lapply(x, function(x) {
    pos <- grep(
      paste0(
        "^\\s{0,}use\\s{0,}\\(|",
        "^\\s{0,}library\\s{0,}\\(|",
        "^\\s{0,}require\\s{0,}\\("
      ),
      x
    )
    if (length(pos) > 0) {
      x[-pos] <- gsub("\".{0,}\\\".{0,}\\\".{0,}\"", "", x[-pos])
    } else {
      x <- gsub("\".{0,}\\\".{0,}\\\".{0,}\"", "", x)
    }
    x
  })

  x <- lapply(x, function(x) {
    pos <- grep(
      paste0(
        "^\\s{0,}use\\s{0,}\\(|",
        "^\\s{0,}library\\s{0,}\\(|",
        "^\\s{0,}require\\s{0,}\\("
      ),
      x
    )
    if (length(pos) > 0) {
      x[-pos] <- gsub("\'.{0,}\\\'.{0,}\\\'.{0,}\'", "", x[-pos])
    } else {
      x <- gsub("\'.{0,}\\\'.{0,}\\\'.{0,}\'", "", x)
    }
    x
  })

  x
}


#' **Detect packages called with the double colon syntax**
#'
#' Detect packages called with the double colon syntax (i.e. `foo::bar()`).
#'
#' @noRd

get_colon_syntax_deps <- function(x) {
  pattern <- paste0(
    "[A-Z|a-z|0-9|\\.]{1,}\\s{0,}",
    "::",
    "\\s{0,}[A-Z|a-z|0-9|\\.|_]{1,}"
  )

  funs <- unlist(lapply(x, function(x) {
    unlist(regmatches(x, gregexpr(pattern, x)))
  }))

  funs <- gsub("\\s", "", funs)

  deps <- strsplit(funs, "::")
  deps <- lapply(deps, function(x) x[1])
  deps <- sort(unique(unlist(deps)))

  if (length(deps) == 0) deps <- NULL

  deps
}


#' **Detect packages called by use()**
#'
#' Detect packages called by the use function (i.e. `use("dplyr", "filter")`).
#'
#' @noRd

get_use_deps <- function(x) {
  pattern <- "^\\s{0,}use\\s{0,}\\((\"|\').+(\"|\')(\\s{0,}\\)?\\s{0,}),"

  funs <- unlist(lapply(x, function(x) {
    unlist(regmatches(x, gregexpr(pattern, x)))
  }))

  deps <- gsub("\\s|use|\\(|,|\"|\'", "", funs)

  deps <- sort(unique(deps))

  if (length(deps) == 0) deps <- NULL

  deps
}


#' **Detect attached packages**
#'
#' Detect attached packages, i.e. called by `library()` and `require()`.
#'
#' @noRd

get_attached_deps <- function(x) {
  pattern <- c(
    paste0(
      "library\\s{0,}\\(\\s{0,}([A-Z|a-z|0-9|\\.]{1,}|",
      "\"\\s{0,}[A-Z|a-z|0-9|\\.]{1,}\\s{0,}\"|",
      "\'\\s{0,}[A-Z|a-z|0-9|\\.]{1,}\\s{0,}\')"
    ),
    paste0(
      "require\\s{0,}\\(\\s{0,}([A-Z|a-z|0-9|\\.]{1,}|",
      "\"\\s{0,}[A-Z|a-z|0-9|\\.]{1,}\\s{0,}\"|",
      "\'\\s{0,}[A-Z|a-z|0-9|\\.]{1,}\\s{0,}\')"
    )
  )
  pattern <- paste0(pattern, collapse = "|")

  deps <- unlist(lapply(x, function(x) {
    unlist(regmatches(x, gregexpr(pattern, x)))
  }))

  deps <- gsub("\\s", "", deps)
  deps <- gsub("library\\(|require\\(|\"|\'", "", deps)

  if (length(deps) == 0) deps <- NULL

  deps
}


#' **Extract Rmd and qmd code chunks**
#'
#' Extract `.Rmd` and `.qmd` code chunks and inline chunks.
#'
#' @noRd

get_code_chunk <- function(x) {
  lapply(x, function(x) {
    chunks <- NULL

    back_ticks <- grep("```", x)

    if (length(back_ticks) > 0) {
      back_ticks <- data.frame(
        "start" = back_ticks[seq(1, length(back_ticks), by = 2)],
        "end" = back_ticks[seq(2, length(back_ticks), by = 2)]
      )

      for (i in 1:nrow(back_ticks)) {
        if (
          length(grep("```\\s{0,}\\{\\s{0,}r", x[back_ticks[i, "start"]])) > 0
        ) {
          chunks <- c(
            chunks,
            x[(back_ticks[i, "start"] + 1):(back_ticks[i, "end"] - 1)]
          )
        }
      }
    }

    inline_chunks <- x[grep("`r\\s{1,}.*`", x)]

    c(chunks, inline_chunks)
  })
}


#' **List non standard folders**
#'
#' List non standard folders (i.e. `analyses/`, `paper/`, `data/`, etc.).
#'
#' @noRd

list_extra_folders <- function() {
  check_for_descr_file()

  path <- path_proj()

  folders <- list.dirs(path, full.names = FALSE, recursive = FALSE)

  ## Remove hidden folders ----

  hidden_folders <- grep("^\\.", folders)

  if (length(hidden_folders) > 0) {
    folders <- folders[-hidden_folders]
  }

  ## Remove standard folders ----

  folders <- folders[
    !(folders %in%
      c("inst", "man", "R", "tests", "vignettes", "renv"))
  ]

  if (length(folders) == 0) folders <- NULL

  folders
}


#' **Detect dependencies in non standard folders**
#'
#' Detect dependencies in `.R` (code and examples), `.Rmd` and `.qmd` written
#' as `pkg::fun()`, `library(pkg)`, and `require(pkg)`.
#'
#' @noRd

get_deps_in_extra <- function() {
  check_for_descr_file()

  path <- path_proj()

  folders <- list_extra_folders()

  if (is.null(folders)) {
    return(list("depends" = NULL, "imports" = NULL))
  }

  ## Get dependencies ----

  deps_depends <- NULL
  deps_imports <- NULL

  for (folder in folders) {
    deps <- get_deps_in_functions(folder)
    deps_depends <- c(deps_depends, deps$"depends")
    deps_imports <- c(deps_imports, deps$"imports")

    deps <- get_deps_in_examples(folder)
    deps_depends <- c(deps_depends, deps$"depends")
    deps_imports <- c(deps_imports, deps$"imports")

    deps <- get_deps_in_markdown(folder)
    deps_depends <- c(deps_depends, deps$"depends")
    deps_imports <- c(deps_imports, deps$"imports")
  }

  ## Remove duplicates ----

  pos <- which(deps_imports %in% deps_depends)

  if (length(pos) > 0) deps_imports <- deps_imports[-pos]

  ## Remove project name ----

  pos <- which(deps_depends == basename(path))
  if (length(pos) > 0) deps_depends <- deps_depends[-pos]

  pos <- which(deps_imports == basename(path))
  if (length(pos) > 0) deps_imports <- deps_imports[-pos]

  ## Clean objects ----

  if (length(deps_depends) == 0) deps_depends <- NULL
  if (length(deps_imports) == 0) deps_imports <- NULL

  list("depends" = deps_depends, "imports" = deps_imports)
}
