#' Read a GTFS Feed
#'
#' Imports a GTFS zip archive and converts it to `wizardgtfs`.
#'
#' @param file_path Path to a GTFS `.zip` archive.
#' @param files Optional character vector of table names without `.txt`.
#' @param quiet Logical. Suppress importer messages and UTF-16 conversion warnings.
#' @param ... Additional arguments passed to [gtfsio::import_gtfs()], including
#'   the legacy argument `file.path`.
#'
#' @return A validated `wizardgtfs` object.
#'
#' @details UTF-16LE and UTF-16BE GTFS text files are converted to UTF-8 in a
#' temporary copy before import. The original ZIP is not modified. When
#' `quiet = FALSE`, a warning identifies the converted files. Other import
#' arguments, including `fields`, `extra_spec`, and `skip`, are preserved.
#'
#' @examples
#' path <- tempfile(fileext = ".zip")
#' write_gtfs(for_rail_gtfs, path)
#' gtfs <- read_gtfs(path)
#'
#' @seealso [GTFSwizard::write_gtfs()], [GTFSwizard::as_wizardgtfs()]
#' @md
#' @export
read_gtfs <- function(file_path = NULL, files = NULL, quiet = TRUE, ...){
  resolved <- resolve_legacy_argument(
    file_path, missing(file_path), list(...), "file.path", "file_path"
  )
  file_path <- resolved$value
  dots <- resolved$dots
  if(!is.character(file_path) || length(file_path) != 1L ||
     !file.exists(file_path)){
    gw_stop("`file_path` must point to an existing GTFS zip archive.")
  }
  gw_assert_flag(quiet, "quiet")
  object <- if(gtfs_zip_uses_utf16(file_path, files = files, skip = dots$skip)){
    import_utf16_gtfs(file_path, files = files, quiet = quiet, dots = dots)
  } else tryCatch(
    do.call(
      gtfsio::import_gtfs,
      c(list(path = file_path, files = files, quiet = quiet), dots)
    ),
    error = function(error){
      import_error <- conditionMessage(error)
      unsupported_utf16 <- grepl("UTF-16", import_error, fixed = TRUE) ||
        grepl("embedded nul", import_error, fixed = TRUE)
      if(!unsupported_utf16){
        stop(error)
      }
      import_utf16_gtfs(file_path, files = files, quiet = quiet, dots = dots)
    }
  )
  as_wizardgtfs(object)
}

gtfs_zip_uses_utf16 <- function(file_path, files = NULL, skip = NULL){
  listed <- tryCatch(utils::unzip(file_path, list = TRUE), error = function(error) NULL)
  if(is.null(listed)){
    return(FALSE)
  }
  filenames <- listed$Name
  filenames <- filenames[grepl("\\.(txt|geojson)$", filenames, ignore.case = TRUE)]
  if(!is.null(files)){
    filenames <- intersect(append_file_ext_local(files), filenames)
  } else if(!is.null(skip)){
    filenames <- setdiff(filenames, append_file_ext_local(skip))
  }
  any(vapply(filenames, function(filename){
    tryCatch({
      con <- unz(file_path, filename, open = "rb")
      on.exit(close(con))
      nzchar(detect_utf16_bytes(readBin(con, "raw", n = 1024L)))
    }, error = function(error) FALSE)
  }, logical(1)))
}

import_utf16_gtfs <- function(file_path, files = NULL, quiet = TRUE, dots = list()){
  skip <- dots$skip
  if(!is.null(files) && !is.null(skip)){
    gw_stop("supply only one of `files` or `skip`.")
  }
  listed <- utils::unzip(file_path, list = TRUE)
  filenames <- listed$Name
  filenames <- filenames[!grepl("/$", filenames)]
  if(any(grepl("(^/|^[A-Za-z]:|(^|/)\\.\\.(/|$)|\\\\)", filenames))){
    gw_stop("the GTFS archive contains unsafe file paths.")
  }
  importable <- filenames[grepl("\\.(txt|geojson)$", filenames, ignore.case = TRUE)]
  if(!is.null(files)){
    requested <- append_file_ext_local(files)
  } else if(!is.null(skip)){
    requested <- setdiff(importable, append_file_ext_local(skip))
  } else {
    requested <- importable
  }
  missing <- setdiff(requested, importable)
  if(length(missing)){
    gw_stop("missing GTFS file(s): ", paste(missing, collapse = ", "), ".")
  }

  tmpdir <- tempfile("gtfswizard-read-")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  utils::unzip(file_path, files = filenames, exdir = tmpdir)
  converted <- character()
  for(filename in requested){
    path <- file.path(tmpdir, filename)
    encoding <- detect_text_encoding(path)
    if(nzchar(encoding)){
      convert_gtfs_utf16(path, encoding)
      converted <- c(converted, filename)
    }
  }
  if(!length(converted)){
    gw_stop("the importer reported UTF-16, but no UTF-16 GTFS files were found.")
  }
  if(!quiet){
    gw_warn("GTFS text files are encoded in UTF-16. Recoding to UTF-8 before import: ",
            paste(converted, collapse = ", "), ".")
  }
  archive <- tempfile("gtfswizard-utf8-", fileext = ".zip")
  on.exit(unlink(archive), add = TRUE)
  zip::zipr(archive, files = filenames, root = tmpdir, mode = "mirror",
            include_directories = FALSE)
  do.call(gtfsio::import_gtfs,
          c(list(path = archive, files = files, quiet = quiet), dots))
}

convert_gtfs_utf16 <- function(path, encoding){
  converted <- tempfile("gtfswizard-utf8-", tmpdir = dirname(path))
  on.exit(unlink(converted), add = TRUE)
  input <- file(path, open = "rb")
  output <- file(converted, open = "wb")
  tryCatch({
    pending <- raw()
    first <- TRUE
    repeat {
      bytes <- readBin(input, "raw", n = 65536L)
      if(!length(bytes)){
        if(length(pending)) gw_stop("incomplete UTF-16 sequence in `", basename(path), "`.")
        break
      }
      bytes <- c(pending, bytes)
      pending <- raw()
      if(first){
        first <- FALSE
        bom <- if(encoding == "UTF-16LE") as.raw(c(255, 254)) else as.raw(c(254, 255))
        if(length(bytes) >= 2L && identical(bytes[1:2], bom)) bytes <- bytes[-(1:2)]
      }
      if(length(bytes) %% 2L){
        pending <- tail(bytes, 1L)
        bytes <- head(bytes, -1L)
      }
      if(length(bytes) >= 2L){
        last <- as.integer(tail(bytes, 2L))
        code <- if(encoding == "UTF-16LE") last[1] + 256L * last[2] else
          256L * last[1] + last[2]
        if(code >= 0xd800 && code <= 0xdbff){
          pending <- c(tail(bytes, 2L), pending)
          bytes <- head(bytes, -2L)
        }
      }
      if(length(bytes)){
        text <- iconv(list(bytes), from = encoding, to = "UTF-8")
        if(is.na(text)) gw_stop("invalid UTF-16 text in `", basename(path), "`.")
        writeBin(charToRaw(text), output)
      }
    }
  }, finally = { close(input); close(output) })
  if(!file.copy(converted, path, overwrite = TRUE)){
    gw_stop("could not write the converted GTFS file.")
  }
}

append_file_ext_local <- function(x){
  x <- as.character(x)
  ifelse(grepl("\\.(txt|geojson)$", x, ignore.case = TRUE), x, paste0(x, ".txt"))
}

detect_text_encoding <- function(path){
  bytes <- readBin(path, "raw", n = min(1024L, file.info(path)$size))
  detect_utf16_bytes(bytes)
}

detect_utf16_bytes <- function(bytes){
  if(length(bytes) >= 2L){
    if(identical(bytes[1:2], as.raw(c(0xff, 0xfe)))){
      return("UTF-16LE")
    }
    if(identical(bytes[1:2], as.raw(c(0xfe, 0xff)))){
      return("UTF-16BE")
    }
  }
  if(length(bytes) >= 4L){
    even_zero <- sum(bytes[seq(2L, length(bytes), by = 2L)] == as.raw(0))
    odd_zero <- sum(bytes[seq(1L, length(bytes), by = 2L)] == as.raw(0))
    if(even_zero > length(bytes) / 4){
      return("UTF-16LE")
    }
    if(odd_zero > length(bytes) / 4){
      return("UTF-16BE")
    }
  }
  ""
}
