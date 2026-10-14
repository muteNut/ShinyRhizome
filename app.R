# RhizoVision Explorer (rv) - interactive front end
#
# Put app.R next to the portable rv build (rv-dist/rv.exe) and start with
#   shiny::runApp()
# Packages: shiny, bslib, readr, dplyr
# tiff + png, to show TIFF scans in the Original preview view
#
# Workflow: tune the settings on one sample image (Preview tab), run them on
# all images (Batch tab), optionally add dry mass for SRL/SRA/RTD (Dry mass tab).
# Default settings match the earlier RhizoVision Analyzer 2.0.1 runs
# (threshold 220, keep largest [whole-root mode only], speck filter 1 mm2, prune 5 px, 600 dpi,
# class boundary 0.3 mm).

if (!requireNamespace("pacman", quietly = TRUE))
  install.packages("pacman", repos = "https://cloud.r-project.org")
pacman::p_load(
  shiny, bslib, readr, dplyr, tiff, png)

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- defaults ---------------------------------------------------------------
DEFAULT_RV  <- "rv-dist/rv.exe"
DEFAULT_IMG <- "samples"
DEFAULT_OUT <- "output"

IMG_RE <- "\\.(png|jpe?g|bmp|tiff?)$"       # formats rv reads
QC_RE  <- "_(seg|features)\\.png$"          # rv's own QC output, never analysed
WEB_RE <- "\\.(png|jpe?g|bmp|gif)$"         # formats a browser can display

# ---- argument help: used for the tooltips and the Help tab -------------------
arg <- function(id, label, flag, text) list(id = id, label = label, flag = flag, text = text)
ARGS <- list(
  arg("rv_path", "rv.exe", "",
      "The portable rv.exe. Relative paths start from the folder the app runs in (where app.R is)."),
  arg("img_path", "Scans", "",
      "A folder of scans or a single image (png, jpg, bmp, tif). Files ending in _seg.png or _features.png (QC output) and anything inside the output folder are ignored."),
  arg("recursive", "Include subfolders", "",
      "Also analyse images in subfolders, e.g. one folder per treatment. The relative path is kept in the Image.Path column."),
  arg("out_dir", "Output folder", "-op",
      "Where the batch run writes features.csv and the QC images. Created if it doesn't exist."),
  arg("roottype", "Root type", "-rt",
      "Broken roots: washed roots from cores or pots, spread out on the scanner. Gives length, diameter, area, volume and diameter classes. Whole root system: intact root systems or crowns. Adds depth, width, convex hull, solidity and root angles."),
  arg("threshold", "Threshold", "-t",
      "Grey value (0-255) that separates root from background: pixels darker than this count as root. Raise it if thin or pale roots are missing in the Segmented view; lower it if background, shadows or the tray edge turn into root. rv default: 200."),
  arg("invert", "Invert image", "-i",
      "rv expects dark roots on a light background (backlit scan). Turn on if your roots are lighter than the background, e.g. scanned on a dark tray."),
  arg("crop", "Crop borders", "--roipath",
      "Pixels cut off each side of the scan before analysis, e.g. a black scanner border. The cut-off area is shaded red in the preview. rv analyses only the remaining rectangle (as a region of interest); the Region.of.Interest column records the crop, e.g. crop_t0_r1_b120_l80. rv needs at least 1 px margin on the right and at the bottom, so 0 there counts as 1. Values are pixels of the scan: scans at a different DPI need different values."),
  arg("convert", "Convert pixels to mm", "--convert",
      "Report lengths, areas and volumes in mm, mm² and mm³ using the DPI. Off: everything stays in pixels."),
  arg("dpi_mode", "DPI source", "",
      "From image metadata: each image uses the resolution stored in its file (TIFF, JPEG, PNG, BMP); the number below is the fallback for files without one. Fixed value: the number below is used for every image and overrides the metadata."),
  arg("dpi", "DPI", "--factordpi",
      "Scan resolution in dots per inch. Every mm value scales with it, so it has to match the scanner setting. Metadata saying 72 or 96 dpi is usually a software default, not the real scan resolution."),
  arg("bgnoise", "Remove specks", "--bgnoise --bgsize",
      "Removes dark objects smaller than the given area (soil particles, dust) so they are not measured as roots. Raise the area if debris survives; lower it if short root fragments disappear."),
  arg("fgnoise", "Fill holes", "--fgnoise --fgsize",
      "Fills light gaps inside roots smaller than the given area (reflections, air bubbles). Unfilled, they split a root and add false tips. rv quirk: if Remove specks is also on, rv only applies Fill holes and the speck filter is lost."),
  arg("keeplargest", "Keep largest object only", "-kl",
      "Whole root system only, as in RhizoVision 2.0.x: keeps only the biggest connected object and skips Remove specks and Fill holes. In broken-roots mode rv ignores it, so the switch is greyed out there."),
  arg("smooth", "Smooth edges", "--smooth --smooththreshold",
      "Smooths jagged root outlines before the skeleton is computed, which avoids false side branches. Threshold in pixels (rv default 2)."),
  arg("prune", "Prune spurs", "--prune --prunethreshold",
      "Removes tiny side branches the skeleton creates at bumpy edges; they would inflate tip counts and length. Threshold in whole pixels beyond the radius of the parent root (rv default 1)."),
  arg("dranges", "Diameter classes", "--dranges",
      "Class boundaries in mm (pixels if not converting), ascending, separated by commas, dot as decimal mark. k boundaries give k+1 classes: 0.5, 1, 2 gives <0.5, 0.5-1, 1-2 and >2 mm. Length, projected area, surface area and volume are reported per class. Empty: rv default (2, 5)."),
  arg("save_qc", "Save QC images", "--segment --feature",
      "Writes <name>_seg.png (the binary root mask) and <name>_features.png (the scan with skeleton and diameter classes drawn on it) to the output folder. Worth keeping to check odd results later.")
)
ARG <- setNames(ARGS, vapply(ARGS, `[[`, "", "id"))

lab <- function(id) {
  a <- ARG[[id]]
  tags$span(a$label, " ",
            tooltip(icon("circle-info", class = "text-body-secondary"), a$text, placement = "right"))
}

# ---- helpers ----------------------------------------------------------------
win_to_slash <- function(p) gsub("\\\\", "/", gsub('^\\s*"|"\\s*$', "", trimws(p)))
clean_path <- function(p) {
  p <- normalizePath(win_to_slash(p), winslash = "/", mustWork = FALSE)
  if (!grepl("^([A-Za-z]:)?/$", p)) p <- sub("/+$", "", p)   # no trailing slash (breaks quoting on Windows)
  p
}

list_images <- function(path, recursive = FALSE, exclude_dir = "") {
  if (!nzchar(trimws(path))) return(character())
  path <- clean_path(path)
  if (file.exists(path) && !dir.exists(path))
    return(if (grepl(IMG_RE, path, ignore.case = TRUE)) path else character())
  if (!dir.exists(path)) return(character())
  f <- list.files(path, IMG_RE, ignore.case = TRUE, full.names = TRUE, recursive = recursive)
  f <- normalizePath(f[!grepl(QC_RE, f, ignore.case = TRUE)], winslash = "/")
  if (nzchar(trimws(exclude_dir)) && dir.exists(exclude_dir))
    f <- f[!startsWith(f, paste0(clean_path(exclude_dir), "/"))]
  sort(f)
}

rel_path <- function(f, base) {
  base <- clean_path(base)
  if (dir.exists(base)) substring(f, nchar(base) + 2) else basename(f)
}

parse_dranges <- function(x) {          # numeric(0) = rv default, NULL = invalid
  x <- trimws(x)
  if (!nzchar(x)) return(numeric())
  v <- suppressWarnings(as.numeric(trimws(strsplit(x, "[,;[:space:]]+")[[1]])))
  if (anyNA(v) || any(v <= 0) || is.unsorted(v, strictly = TRUE)) return(NULL)
  v
}

check_params <- function(p) {
  num_ok <- function(x, min = 0) length(x) == 1 && is.finite(x) && x >= min
  if (p$convert && !num_ok(p$dpi, 1)) return("DPI must be a positive number.")
  if (is.null(p$dranges)) return("Diameter classes: use ascending numbers separated by commas, e.g. 0.5, 1, 2")
  if (p$bgnoise && !num_ok(p$bgsize)) return("Speck area must be a number >= 0.")
  if (p$fgnoise && !num_ok(p$fgsize)) return("Hole area must be a number >= 0.")
  if (p$smooth && !num_ok(p$smooththr)) return("Smoothing threshold must be a number >= 0.")
  if (p$prune && !num_ok(p$prunethr)) return("Pruning threshold must be a whole number >= 0.")
  if (!crop_ok(p$crop)) return("Crop: use whole numbers of pixels (0 or more).")
  NULL
}

build_args <- function(p, dpi, img, out_dir, csv, qc, roi = NULL) {
  q <- function(x) shQuote(x, type = "cmd")
  n <- function(x) vapply(x, function(v) format(v, scientific = FALSE, digits = 15), "")
  c("-rt", p$roottype, "-t", n(p$threshold),
    if (p$invert)      "-i",
    if (p$keeplargest) "-kl",
    if (p$bgnoise)     c("--bgnoise", "--bgsize", n(p$bgsize)),
    if (p$fgnoise)     c("--fgnoise", "--fgsize", n(p$fgsize)),
    if (p$smooth)      c("--smooth", "--smooththreshold", n(p$smooththr)),
    if (p$prune)       c("--prune", "--prunethreshold", n(round(p$prunethr))),
    if (p$convert)     c("--convert", "--factordpi", n(dpi)),
    if (length(p$dranges)) c("--dranges", paste(n(p$dranges), collapse = ",")),
    if (qc)            c("--segment", "--feature"),
    if (!is.null(roi)) c("--roipath", q(roi)),
    "-na", "-op", q(out_dir), "-o", q(csv), q(img))
}

fmt_cmd <- function(rv, args) paste(c(shQuote(rv, type = "cmd"), args), collapse = " ")

run_rv <- function(rv, args) {
  out <- tryCatch(
    suppressWarnings(system2(rv, args, stdout = TRUE, stderr = TRUE)),   # system2 quotes rv itself
    error = function(e) structure(conditionMessage(e), status = 127L))
  status <- attr(out, "status") %||% 0L
  out <- gsub("\033\\[[0-9;?]*[A-Za-z]", "", out)                     # colour/cursor codes
  out <- vapply(strsplit(out, "\r", fixed = TRUE),                     # progress bars: last state
                function(x) if (length(x)) x[length(x)] else "", "")
  list(status = status, log = paste(out, collapse = "\n"))
}

read_rv_csv <- function(path, as_text = FALSE)
  read_csv(path, show_col_types = FALSE, progress = FALSE,
           col_types = if (as_text) cols(.default = col_character()) else NULL)

resolve_dpi <- function(img, p) {
  if (!p$convert) return(list(value = NA_real_, source = "none (pixels)"))
  if (p$dpi_mode == "meta") {
    d <- read_dpi(img)
    if (!is.na(d)) return(list(value = d, source = if (d %in% c(72, 96))
      "metadata, likely a software default" else "metadata"))
    return(list(value = p$dpi, source = "fallback (no metadata)"))
  }
  list(value = p$dpi, source = "fixed")
}

# ---- DPI from file metadata: TIFF, JPEG (JFIF/EXIF), PNG, BMP; NA if absent --
u16 <- function(r, i, le) { b <- as.integer(r[i + 0:1]); if (le) b[1] + 256 * b[2] else 256 * b[1] + b[2] }
u32 <- function(r, i, le) { b <- as.numeric(as.integer(r[i + 0:3])); if (le) sum(b * 256^(0:3)) else sum(b * 256^(3:0)) }

dpi_tiff <- function(rd) {                 # rd(offset, n): n raw bytes at 0-based offset
  hdr <- rd(0, 8)
  le  <- hdr[1] == as.raw(0x49)
  ifd <- u32(hdr, 5, le)
  n   <- u16(rd(ifd, 2), 1, le)
  if (n < 1 || n > 500) return(NA_real_)
  ent <- rd(ifd + 2, 12 * n)
  xres <- NA_real_; unit <- 2               # TIFF default unit: inch
  for (k in seq_len(n) - 1) {
    e <- 12 * k + 1
    tag <- u16(ent, e, le)
    if (tag == 282) { r <- rd(u32(ent, e + 8, le), 8); xres <- u32(r, 1, le) / u32(r, 5, le) }
    if (tag == 296) unit <- u16(ent, e + 8, le)
  }
  switch(as.character(unit), "2" = xres, "3" = xres * 2.54, NA_real_)
}

dpi_png <- function(h) {
  pos <- 9
  while (pos + 16 <= length(h)) {
    len  <- u32(h, pos, FALSE)
    type <- rawToChar(h[pos + 4:7])
    if (type == "pHYs") return(if (as.integer(h[pos + 16]) == 1) u32(h, pos + 8, FALSE) * 0.0254 else NA_real_)
    if (type %in% c("IDAT", "IEND")) break
    pos <- pos + 12 + len
  }
  NA_real_
}

dpi_jpeg <- function(h) {
  JFIF <- as.raw(c(0x4A, 0x46, 0x49, 0x46, 0x00))
  EXIF <- as.raw(c(0x45, 0x78, 0x69, 0x66, 0x00, 0x00))
  pos <- 3; exif <- NA_real_
  while (pos + 3 <= length(h) && h[pos] == as.raw(0xFF)) {
    marker <- as.integer(h[pos + 1])
    if (marker %in% c(0xD9, 0xDA)) break               # end of image / start of scan data
    len <- u16(h, pos + 2, FALSE); dat <- pos + 4
    end <- min(length(h), pos + 1 + len)
    if (marker == 0xE0 && end - dat >= 11 && all(h[dat + 0:4] == JFIF)) {
      units <- as.integer(h[dat + 7]); x <- u16(h, dat + 8, FALSE)
      if (units == 1) return(x)
      if (units == 2) return(x * 2.54)
    }
    if (marker == 0xE1 && is.na(exif) && end - dat >= 14 && all(h[dat + 0:5] == EXIF)) {
      tiff <- h[(dat + 6):end]
      exif <- tryCatch(dpi_tiff(function(off, n) {
        if (off + n > length(tiff)) stop("EXIF out of range")
        tiff[off + seq_len(n)]
      }), error = function(e) NA_real_)
    }
    pos <- pos + 2 + len
  }
  exif
}

dpi_bmp <- function(h) {
  if (length(h) < 46) return(NA_real_)
  v <- u32(h, 39, TRUE)                     # biXPelsPerMeter
  if (v > 0) v * 0.0254 else NA_real_
}

read_dpi <- function(path) {
  if (!isTRUE(file.exists(path))) return(NA_real_)
  con <- file(path, "rb"); on.exit(close(con))
  d <- tryCatch({
    h <- readBin(con, "raw", 65536)
    sig <- function(...) { s <- as.raw(c(...)); length(h) >= length(s) && all(h[seq_along(s)] == s) }
    if (sig(0x89, 0x50, 0x4E, 0x47)) dpi_png(h)
    else if (sig(0xFF, 0xD8)) dpi_jpeg(h)
    else if (sig(0x49, 0x49, 0x2A, 0x00) || sig(0x4D, 0x4D, 0x00, 0x2A))
      dpi_tiff(function(off, n) {
        seek(con, off)
        r <- readBin(con, "raw", n)
        if (length(r) < n) stop("TIFF out of range")
        r
      })
    else if (sig(0x42, 0x4D)) dpi_bmp(h)
    else NA_real_
  }, error = function(e) NA_real_)
  if (length(d) == 1 && is.finite(d) && d > 0) round(d, 1) else NA_real_
}

# ---- image size from the file header (needed to turn per-side crops into a rectangle) --
dims_tiff <- function(rd) {
  hdr <- rd(0, 8)
  le  <- hdr[1] == as.raw(0x49)
  ifd <- u32(hdr, 5, le)
  n   <- u16(rd(ifd, 2), 1, le)
  if (n < 1 || n > 500) return(NULL)
  ent <- rd(ifd + 2, 12 * n)
  w <- h <- NA_real_
  for (k in seq_len(n) - 1) {
    e <- 12 * k + 1
    tag <- u16(ent, e, le)
    val <- if (u16(ent, e + 2, le) == 3) u16(ent, e + 8, le) else u32(ent, e + 8, le)
    if (tag == 256) w <- val
    if (tag == 257) h <- val
  }
  c(w, h)
}

dims_jpeg <- function(rd) {
  pos <- 2                                    # 0-based, after the SOI marker
  repeat {
    m <- rd(pos, 4)
    if (m[1] != as.raw(0xFF)) return(NULL)
    marker <- as.integer(m[2])
    if (marker == 0xFF) { pos <- pos + 1; next }                          # fill byte
    if (marker == 0x01 || (marker >= 0xD0 && marker <= 0xD7)) { pos <- pos + 2; next }
    if (marker %in% c(0xD9, 0xDA)) return(NULL)                           # no frame header found
    if (marker >= 0xC0 && marker <= 0xCF && !(marker %in% c(0xC4, 0xC8, 0xCC))) {
      s <- rd(pos + 5, 4)                     # SOFn: height, width
      return(c(u16(s, 3, FALSE), u16(s, 1, FALSE)))
    }
    pos <- pos + 2 + u16(m, 3, FALSE)
  }
}

read_dims <- function(path) {                 # c(width, height) in px, NULL if unknown
  if (!isTRUE(file.exists(path))) return(NULL)
  con <- file(path, "rb"); on.exit(close(con))
  rd <- function(off, n) {
    seek(con, off)
    r <- readBin(con, "raw", n)
    if (length(r) < n) stop("header out of range")
    r
  }
  d <- tryCatch({
    h <- rd(0, 30)
    sig <- function(...) { s <- as.raw(c(...)); all(h[seq_along(s)] == s) }
    if (sig(0x89, 0x50, 0x4E, 0x47)) c(u32(h, 17, FALSE), u32(h, 21, FALSE))
    else if (sig(0xFF, 0xD8)) dims_jpeg(rd)
    else if (sig(0x49, 0x49, 0x2A, 0x00) || sig(0x4D, 0x4D, 0x00, 0x2A)) dims_tiff(rd)
    else if (sig(0x42, 0x4D)) {
      hh <- u32(h, 23, TRUE); if (hh >= 2^31) hh <- 2^32 - hh   # negative = top-down BMP
      c(u32(h, 19, TRUE), hh)
    }
    else NULL
  }, error = function(e) NULL)
  if (length(d) == 2 && all(is.finite(d) & d > 0)) d else NULL
}

# ---- crop: per-side pixels -> rv region of interest -------------------------------
crop_ok     <- function(cr) all(vapply(cr, function(x) length(x) == 1 && is.finite(x) && x >= 0 && x == round(x), TRUE))
crop_active <- function(cr) crop_ok(cr) && any(unlist(cr) > 0)

crop_box <- function(dims, cr) {              # the rectangle rv analyses, or list(error = ...)
  if (is.null(dims)) return(list(error = "could not read the image size from the file"))
  t <- cr$t; l <- cr$l
  r <- max(cr$r, 1); b <- max(cr$b, 1)        # rv drops an ROI that touches the right/bottom edge
  w <- dims[1] - l - r; h <- dims[2] - t - b
  if (w < 1 || h < 1)
    return(list(error = sprintf("crop is larger than the image (%d \u00d7 %d px)", dims[1], dims[2])))
  list(x = l, y = t, w = w, h = h, t = t, r = r, b = b, l = l, img_w = dims[1], img_h = dims[2],
       name = sprintf("crop_t%d_r%d_b%d_l%d", t, r, b, l))
}

write_roi <- function(box, dir, file = "crop_roi.csv") {   # rv format: name, class, x, y, width, height
  f <- file.path(dir, file)
  writeLines(c("# ROI name, class, x, y, width, height",
               sprintf("%s, crop, %d, %d, %d, %d", box$name, box$x, box$y, box$w, box$h)), f)
  f
}

roi_ignored <- function(log, data)            # rv ran, but on the whole image
  grepl("Ignoring the region-of-interest", log, fixed = TRUE) ||
  (!is.null(data) && nrow(data) > 0 && identical(as.character(data$Region.of.Interest[1]), "Full"))

# ---- TIFF -> PNG for the Original view (browsers can't display TIFF) -----------
# Needs the small packages tiff + png. Conversions are cached for the session.
TIFF_CACHE    <- new.env()
TIFF_MAX_SIDE <- 8000                     # larger scans are thinned for display only

tiff_as_png <- function(path) {
  if (!requireNamespace("tiff", quietly = TRUE) || !requireNamespace("png", quietly = TRUE))
    stop('Showing TIFF scans needs two small packages: install.packages(c("tiff", "png"))')
  fi  <- file.info(path)
  key <- paste(path, fi$size, as.numeric(fi$mtime))
  hit <- TIFF_CACHE[[key]]
  if (!is.null(hit) && file.exists(hit)) return(hit)
  img <- tryCatch(tiff::readTIFF(path, native = TRUE),   # first page; 16-bit becomes 8-bit
                  error = function(e) stop("Could not read this TIFF for display (", conditionMessage(e),
                                           "). The Overlay view still shows the scan."))
  h <- nrow(img); w <- ncol(img); k <- ceiling(max(h, w) / TIFF_MAX_SIDE)
  if (k > 1) {                            # nativeRaster is stored row-major: index it as (w, h)
    m <- unclass(img); dim(m) <- c(w, h)
    m <- m[seq(1, w, by = k), seq(1, h, by = k), drop = FALSE]
    img <- structure(m, dim = rev(dim(m)), class = "nativeRaster")
  }
  attr(img, "channels") <- NULL           # png::writePNG rejects channels = 1 or 3
  dir.create(file.path(tempdir(), "rv_tiff"), showWarnings = FALSE)
  out <- file.path(tempdir(), "rv_tiff", sprintf("tif%05d.png", length(ls(TIFF_CACHE)) + 1))
  png::writePNG(img, out)
  assign(key, out, envir = TIFF_CACHE)
  out
}

# ---- result formatting --------------------------------------------------------
KEY_PREFIX <- c("Total.Root.Length", "Number.of.Root.Tips", "Number.of.Branch.Points",
                "Average.Diameter", "Median.Diameter", "Network.Area", "Surface.Area",
                "Volume", "Depth", "Maximum.Width")
key_cols <- function(nms) unique(unlist(lapply(KEY_PREFIX, function(p)
  nms[startsWith(nms, p) & !grepl("Diameter.Range", nms, fixed = TRUE)])))

pretty_col <- function(x) {
  has_unit <- grepl("\\.(mm|px)[23]?$", x)
  unit <- sub(".*\\.((mm|px)[23]?)$", "\\1", x)
  unit <- sub("2$", "²", sub("3$", "³", unit))
  base <- gsub(".", " ", sub("\\.(mm|px)[23]?$", "", x), fixed = TRUE)
  ifelse(has_unit, sprintf("%s (%s)", base, unit), base)
}

class_labels <- function(dr, k, unit) {
  if (!length(dr)) dr <- c(2, 5)             # rv default
  if (length(dr) + 1 != k) return(paste("class", seq_len(k)))
  paste(c(paste("<", dr[1]),
          if (length(dr) > 1) paste0(head(dr, -1), "-", dr[-1]),
          paste(">", dr[length(dr)])), unit)
}

fmt <- function(x) if (is.numeric(x)) formatC(x, digits = 4, format = "fg", big.mark = " ") else as.character(x)

measure_table <- function(d, dr) {
  nm <- names(d)
  key <- key_cols(nm)
  rows <- data.frame(Measure = pretty_col(key),
                     Value = vapply(key, function(k) fmt(d[[k]][1]), ""))
  lc <- nm[startsWith(nm, "Root.Length.Diameter.Range.")]
  if (length(lc)) {
    unit <- sub(".*\\.", "", lc[1])
    cls <- class_labels(dr, length(lc), unit)
    rows <- rbind(rows, data.frame(
      Measure = sprintf("Root length, diameter %s (%s)", cls, unit),
      Value = vapply(lc, function(k) fmt(d[[k]][1]), "")))
  }
  rownames(rows) <- NULL
  rows
}

# ---- dry mass helpers -----------------------------------------------------------
stem_key <- function(x) tolower(tools::file_path_sans_ext(basename(as.character(x))))
safe_div <- function(a, b) ifelse(is.finite(b) & b > 0, a / b, NA_real_)

read_table_auto <- function(path) {          # comma or semicolon (decimal comma) CSV
  first <- readLines(path, n = 1, warn = FALSE)
  n_semi  <- lengths(regmatches(first, gregexpr(";", first, fixed = TRUE)))
  n_comma <- lengths(regmatches(first, gregexpr(",", first, fixed = TRUE)))
  suppressMessages(
    if (n_semi > n_comma) read_csv2(path, show_col_types = FALSE)
    else read_csv(path, show_col_types = FALSE))
}

guess_file_col <- function(m, feat) {
  cols <- names(m)
  if (!is.null(feat) && nrow(feat)) {
    keys <- stem_key(feat$File.Name)
    hits <- vapply(cols, function(cl) sum(stem_key(m[[cl]]) %in% keys), 0)
    if (max(hits) > 0) return(cols[which.max(hits)])
  }
  hit <- grep("file|image|name|sample|probe|bild|id", cols, ignore.case = TRUE, value = TRUE)
  if (length(hit)) hit[1] else cols[1]
}

guess_mass_col <- function(num_cols) {
  hit <- grep("mass|weight|dry|dw|gewicht|masse", num_cols, ignore.case = TRUE, value = TRUE)
  if (length(hit)) hit[1] else num_cols[1]
}

# grey out inputs that rv ignores in the current mode (mirrors the 2.0.x GUI)
JS_DISABLE <- "
Shiny.addCustomMessageHandler('set-disabled', function(m) {
  m.ids.forEach(function(id) {
    var el = document.getElementById(id);
    if (!el) return;
    el.disabled = m.disabled;
    var box = el.closest('.shiny-input-container') || el.parentElement;
    if (box) box.style.opacity = m.disabled ? 0.45 : 1;
  });
});"

# zoom and crop frame on the preview are applied in the browser, without re-sending the image
JS_FRAME <- "
Shiny.addCustomMessageHandler('preview-frame', function(m) {
  var wrap = document.getElementById('prev_wrap'), box = document.getElementById('crop_box');
  if (!wrap || !box) return;
  wrap.style.width = m.zoom + '%';
  if (!m.crop) { box.style.display = 'none'; return; }
  box.style.top = m.crop.t + '%'; box.style.right = m.crop.r + '%';
  box.style.bottom = m.crop.b + '%'; box.style.left = m.crop.l + '%';
  box.style.display = 'block';
});"

# ---- UI ------------------------------------------------------------------------
ui <- page_sidebar(
  title = "ShinyRhizome",
  theme = bs_theme(version = 5, primary = "#2f6f4e"),
  sidebar = sidebar(
    width = 360,
    tags$script(HTML(JS_DISABLE)),
    tags$script(HTML(JS_FRAME)),
    accordion(
      open = c("Files", "Segmentation"),
      accordion_panel(
        "Files", icon = icon("folder-open"),
        textInput("rv_path", lab("rv_path"), DEFAULT_RV),
        uiOutput("rv_status"),
        textInput("img_path", lab("img_path"), DEFAULT_IMG),
        input_switch("recursive", lab("recursive"), FALSE),
        div(class = "d-flex gap-2 small mb-3",
            uiOutput("img_status"),
            actionLink("refresh_imgs", NULL, icon = icon("rotate"), title = "Re-scan folder")),
        textInput("out_dir", lab("out_dir"), DEFAULT_OUT)
      ),
      accordion_panel(
        "Segmentation", icon = icon("sliders"),
        radioButtons("roottype", lab("roottype"),
                     c("Broken roots" = "1", "Whole root system" = "0"), inline = TRUE),
        sliderInput("threshold", lab("threshold"), 0, 255, 220, step = 1),
        input_switch("invert", lab("invert"), FALSE)
      ),
      accordion_panel(
        "Crop", icon = icon("crop"),
        div(class = "mb-2", lab("crop")),
        layout_columns(
          col_widths = c(6, 6),
          numericInput("crop_t", "Top (px)", 0, min = 0, step = 10),
          numericInput("crop_r", "Right (px)", 0, min = 0, step = 10),
          numericInput("crop_b", "Bottom (px)", 0, min = 0, step = 10),
          numericInput("crop_l", "Left (px)", 0, min = 0, step = 10)
        ),
        uiOutput("crop_status")
      ),
      accordion_panel(
        "Scale (DPI)", icon = icon("ruler"),
        input_switch("convert", lab("convert"), TRUE),
        conditionalPanel(
          "input.convert",
          radioButtons("dpi_mode", lab("dpi_mode"),
                       c("From image metadata" = "meta", "Fixed value" = "fixed")),
          numericInput("dpi", lab("dpi"), 600, min = 1, step = 50),
          uiOutput("dpi_status")
        )
      ),
      accordion_panel(
        "Clean-up", icon = icon("broom"),
        input_switch("keeplargest", lab("keeplargest"), TRUE),
        conditionalPanel("input.roottype == '1'",
                         div(class = "small text-body-secondary mb-3", "No effect for broken roots (as in 2.0.x).")),
        conditionalPanel("input.roottype == '0' && input.keeplargest",
                         div(class = "small text-body-secondary mb-3", "Speck and hole filters are skipped while this is on.")),
        input_switch("bgnoise", lab("bgnoise"), TRUE),
        conditionalPanel("input.bgnoise",
                         numericInput("bgsize", "Max. speck area (mm²)", 1, min = 0, step = 0.1)),
        input_switch("fgnoise", lab("fgnoise"), FALSE),
        conditionalPanel("input.fgnoise",
                         numericInput("fgsize", "Max. hole area (mm²)", 1, min = 0, step = 0.1)),
        conditionalPanel("input.bgnoise && input.fgnoise && !(input.roottype == '0' && input.keeplargest)",
                         div(class = "small text-warning mb-3",
                             "With both filters on, rv only applies Fill holes (Remove specks is lost).")),
        input_switch("smooth", lab("smooth"), FALSE),
        conditionalPanel("input.smooth",
                         numericInput("smooththr", "Smoothing threshold (px)", 2, min = 0, step = 0.5)),
        input_switch("prune", lab("prune"), TRUE),
        conditionalPanel("input.prune",
                         numericInput("prunethr", "Pruning threshold (px)", 5, min = 0, step = 1))
      ),
      accordion_panel(
        "Diameter classes", icon = icon("layer-group"),
        textInput("dranges", lab("dranges"), "0.3"),
        uiOutput("dranges_status")
      )
    )
  ),
  navset_card_tab(
    id = "tabs",
    nav_panel(
      "Preview", icon = icon("eye"),
      layout_columns(
        col_widths = c(8, 4),
        div(
          div(class = "d-flex gap-2 align-items-end flex-wrap",
              actionButton("prev_prev", NULL, icon = icon("chevron-left"), title = "Previous image"),
              div(style = "flex: 1; min-width: 14rem;",
                  selectInput("preview_img", "Sample image (first in folder by default)",
                              choices = NULL, width = "100%")),
              actionButton("prev_next", NULL, icon = icon("chevron-right"), title = "Next image"),
              actionButton("preview_btn", "Update", icon = icon("rotate"), class = "btn-primary")),
          div(class = "d-flex gap-4 align-items-center flex-wrap",
              input_switch("auto_preview", "Auto-update when settings change", TRUE),
              radioButtons("prev_view", NULL, inline = TRUE,
                           c("Overlay" = "features", "Segmented" = "seg", "Original" = "orig")),
              sliderInput("zoom", NULL, 100, 400, 100, step = 25, post = " %", width = "200px")),
          div(style = "max-height: 70vh; overflow: auto; border: 1px solid var(--bs-border-color);",
              div(id = "prev_wrap", style = "position: relative; overflow: hidden; width: 100%;",
                  imageOutput("prev_img", height = "auto"),
                  div(id = "crop_box",       # area outside the box is cut off (shaded)
                      style = paste("display: none; position: absolute; pointer-events: none;",
                                    "outline: 2px dashed #d9534f; outline-offset: -1px;",
                                    "box-shadow: 0 0 0 10000px rgba(217, 83, 79, 0.35);"))))
        ),
        div(
          h5("This image"),
          tableOutput("prev_table"),
          tags$details(tags$summary("rv command & log"), verbatimTextOutput("prev_log"))
        )
      )
    ),
    nav_panel(
      "Batch", icon = icon("play"),
      div(class = "d-flex gap-3 align-items-center flex-wrap",
          actionButton("run_batch", "Run all images", icon = icon("play"), class = "btn-primary"),
          input_switch("save_qc", lab("save_qc"), FALSE),
          actionButton("load_prev", "Load existing features.csv", icon = icon("file-import")),
          downloadButton("dl_features", "Download features.csv")),
      uiOutput("batch_status"),
      input_switch("all_cols", "Show all columns", FALSE),
      div(style = "overflow-x: auto;", tableOutput("batch_table")),
      tags$details(tags$summary("rv commands & log"), verbatimTextOutput("batch_log"))
    ),
    nav_panel(
      "Dry mass", icon = icon("scale-balanced"),
      p("Load a CSV with one row per scan: a column with the image file name (extension optional)",
        "and a column with root dry mass. Comma- and semicolon-separated files both work."),
      tags$pre("file,dry_mass_g\nsample01,0.123\nsample02.tif,0.098"),
      fileInput("mass_file", NULL, accept = c(".csv", ".txt"), buttonLabel = "Load dry mass…"),
      uiOutput("mass_ui")
    ),
    nav_panel(
      "Help", icon = icon("circle-question"),
      div(style = "max-width: 62rem;",
          h4("Workflow"),
          tags$ol(
            tags$li(strong("Files: "), "point the app to rv.exe, your scans (a folder or a single image) and an output folder."),
            tags$li(strong("Preview: "), "the first image in the folder is analysed with the current settings. ",
                    "Change threshold, invert and clean-up until the ", em("Segmented"), " view shows every root and nothing else. ",
                    "Step through a few more images with the arrow buttons to make sure the settings hold for all of them."),
            tags$li(strong("Crop: "), "if the scans have a border (e.g. a black edge from the scanner lid), enter how many ",
                    "pixels to cut from each side. The cut-off area is shaded red in the preview; the ", em("Original"),
                    " view is easiest for lining it up."),
            tags$li(strong("Scale: "), "check the DPI. The value used for the sample image is listed under ", em("This image"), ". ",
                    "If the metadata is missing or looks wrong, switch to a fixed value."),
            tags$li(strong("Batch: "), "runs every image with the same settings. Results are written to features.csv in the ",
                    "output folder, together with the QC images."),
            tags$li(strong("Dry mass: "), "load dry mass per scan to get specific root length (SRL), specific root area (SRA), ",
                    "root tissue density (RTD) and the fine-root length fraction.")
          ),
          h4("Settings"),
          tags$table(class = "table table-sm",
                     tags$thead(tags$tr(tags$th("Setting"), tags$th("rv flag"), tags$th("What it does / when to change it"))),
                     tags$tbody(lapply(ARGS, function(a) tags$tr(
                       tags$td(a$label), tags$td(if (nzchar(a$flag)) code(a$flag)), tags$td(a$text))))),
          h4("Main output columns (broken roots)"),
          tags$ul(
            tags$li(code("Total.Root.Length"), ": summed length of the root skeleton."),
            tags$li(code("Number.of.Root.Tips"), ", ", code("Number.of.Branch.Points"), ": skeleton end and fork points."),
            tags$li(code("Average/Median/Maximum.Diameter"), ": diameter along the skeleton."),
            tags$li(code("Network.Area"), " (projected area), ", code("Surface.Area"), ", ", code("Volume"),
                    ": computed from the diameter along each root segment."),
            tags$li(code("...Diameter.Range.k"), ": the same quantities split by diameter class (see Diameter classes).")
          ),
          h4("Tips"),
          tags$ul(
            tags$li("Spread roots so they overlap as little as possible; overlaps are counted once."),
            tags$li("Use the same settings for every image in a study and report them. ",
                    "The exact rv command is under ", em("rv command & log"), " on the Preview tab."),
            tags$li("Preview and batch results are identical for the same image and settings: both call the same rv.exe."),
            tags$li("TIFF scans are analysed directly by rv. For the ", em("Original"), " view the app converts them to PNG ",
                    "(needs the packages tiff and png); very large scans are thinned for display only.")
          )
      )
    )
  )
)

# ---- server -----------------------------------------------------------------------
server <- function(input, output, session) {
  session$onSessionEnded(stopApp)              # closing the browser tab stops the app
  # pasted Windows paths: drop quotes, switch \ to / in the fields themselves
  lapply(c("rv_path", "img_path", "out_dir"), function(id) {
    observeEvent(input[[id]], {
      v <- win_to_slash(input[[id]])
      if (!identical(v, input[[id]])) updateTextInput(session, id, value = v)
    }, ignoreInit = TRUE)
  })
  rv_bin <- reactive(clean_path(input$rv_path))
  
  # rv found?
  rv_path_d <- debounce(reactive(input$rv_path), 600)
  output$rv_status <- renderUI({
    p <- clean_path(rv_path_d())
    if (!file.exists(p)) return(div(class = "text-danger small mb-3", "rv.exe not found"))
    r <- run_rv(p, "--version")
    v <- trimws(tail(strsplit(r$log, "\n")[[1]], 1))
    if (r$status != 0 || !length(v)) return(div(class = "text-danger small mb-3", "rv.exe found but does not run"))
    div(class = "text-success small mb-3", icon("check"), v)
  })
  
  # images
  img_path_d <- debounce(reactive(input$img_path), 600)
  images <- reactive({
    input$refresh_imgs
    list_images(img_path_d(), isTRUE(input$recursive), input$out_dir)
  })
  
  output$img_status <- renderUI({
    n <- length(images())
    span(class = if (n) "text-body-secondary" else "text-danger",
         if (n) sprintf("%d image%s found", n, if (n == 1) "" else "s") else "No images found")
  })
  
  observeEvent(images(), {
    imgs <- images()
    sel <- if (!length(imgs)) character() else if (isTRUE(input$preview_img %in% imgs)) input$preview_img else imgs[1]
    updateSelectInput(session, "preview_img",
                      choices = setNames(imgs, rel_path(imgs, input$img_path)), selected = sel)
    updateActionButton(session, "run_batch",
                       label = sprintf("Run all %d image%s", length(imgs), if (length(imgs) == 1) "" else "s"))
  })
  
  step_img <- function(d) {
    imgs <- images(); req(length(imgs))
    i <- match(input$preview_img, imgs); if (is.na(i)) i <- 1
    updateSelectInput(session, "preview_img", selected = imgs[(i - 1 + d) %% length(imgs) + 1])
  }
  observeEvent(input$prev_prev, step_img(-1))
  observeEvent(input$prev_next, step_img(1))
  
  # units follow the convert switch
  observeEvent(input$convert, {
    a <- if (isTRUE(input$convert)) "mm²" else "px²"
    updateNumericInput(session, "bgsize", label = sprintf("Max. speck area (%s)", a))
    updateNumericInput(session, "fgsize", label = sprintf("Max. hole area (%s)", a))
  })
  observeEvent(input$dpi_mode, {
    updateNumericInput(session, "dpi", label = if (input$dpi_mode == "meta")
      "Fallback DPI (images without metadata)" else "DPI (all images)")
  })
  
  output$dpi_status <- renderUI({
    img <- input$preview_img
    if (!isTruthy(img)) return(NULL)
    d <- read_dpi(img)
    if (is.na(d)) return(div(class = "small text-body-secondary mb-3", "Sample image: no DPI in metadata."))
    div(class = paste("small mb-3", if (d %in% c(72, 96)) "text-warning" else "text-body-secondary"),
        sprintf("Sample image metadata: %g dpi", d),
        if (d %in% c(72, 96)) " (probably a software default, check!)")
  })
  
  dranges <- reactive(parse_dranges(input$dranges))
  output$dranges_status <- renderUI({
    dr <- dranges()
    unit <- if (isTRUE(input$convert)) "mm" else "px"
    if (is.null(dr)) return(div(class = "small text-danger", "Use ascending positive numbers separated by commas, e.g. 0.5, 1, 2"))
    used <- if (length(dr)) dr else c(2, 5)
    div(class = "small text-body-secondary",
        if (!length(dr)) "rv default. ",
        "Classes: ", paste(class_labels(used, length(used) + 1, unit), collapse = " | "))
  })
  
  # rv (like the 2.0.x GUI) uses keep-largest only for whole roots, and then skips both noise filters
  keep_largest <- reactive(identical(input$roottype, "0") && isTRUE(input$keeplargest))

  observe({
    broken <- !identical(input$roottype, "0")
    session$sendCustomMessage("set-disabled", list(ids = list("keeplargest"), disabled = broken))
    session$sendCustomMessage("set-disabled", list(ids = list("bgnoise", "bgsize", "fgnoise", "fgsize"),
                                                   disabled = keep_largest()))
  })

  crop_vals <- reactive(list(t = input$crop_t %||% 0, r = input$crop_r %||% 0,
                             b = input$crop_b %||% 0, l = input$crop_l %||% 0))

  output$crop_status <- renderUI({
    cr <- crop_vals()
    if (!crop_ok(cr)) return(div(class = "small text-danger", "Use whole numbers of pixels (0 or more)."))
    if (!crop_active(cr)) return(div(class = "small text-body-secondary", "No crop: the whole scan is analysed."))
    img <- input$preview_img
    if (!isTruthy(img)) return(NULL)
    b <- crop_box(read_dims(img), cr)
    if (!is.null(b$error)) return(div(class = "small text-danger", "Sample image: ", b$error, "."))
    div(class = "small text-body-secondary",
        sprintf("Sample image %d \u00d7 %d px; analysed area %d \u00d7 %d px.", b$img_w, b$img_h, b$w, b$h))
  })

  params <- reactive(list(
    roottype = input$roottype, threshold = input$threshold, invert = isTRUE(input$invert),
    convert = isTRUE(input$convert), dpi_mode = input$dpi_mode %||% "meta", dpi = input$dpi,
    keeplargest = keep_largest(),
    bgnoise = isTRUE(input$bgnoise) && !keep_largest(), bgsize = input$bgsize,
    fgnoise = isTRUE(input$fgnoise) && !keep_largest(), fgsize = input$fgsize,
    smooth = isTRUE(input$smooth), smooththr = input$smooththr,
    prune = isTRUE(input$prune), prunethr = input$prunethr,
    crop = crop_vals(),
    dranges = dranges()))
  params_d <- debounce(params, 800)
  
  # ---- preview: one image, temp folder ----------------------------------------
  preview <- reactiveVal(NULL)
  last_key <- reactiveVal("")
  
  run_preview <- function(p, force = FALSE) {
    img <- input$preview_img
    if (!isTruthy(img) || !file.exists(img)) return(invisible())
    rv <- rv_bin()
    key <- paste(c(img, rv, deparse(p)), collapse = "|")
    if (!force && identical(key, last_key())) return(invisible())
    last_key(key)
    err <- check_params(p) %||% (if (!file.exists(rv)) "rv.exe not found: check the path under Files.")
    if (!is.null(err)) { preview(list(error = err)); return(invisible()) }
    
    dpi  <- resolve_dpi(img, p)
    pdir <- file.path(tempdir(), "rv_preview")
    unlink(pdir, recursive = TRUE); dir.create(pdir, showWarnings = FALSE)
    csv  <- file.path(pdir, "preview.csv")
    box  <- NULL; roi <- NULL
    if (crop_active(p$crop)) {
      box <- crop_box(read_dims(img), p$crop)
      if (!is.null(box$error)) { preview(list(error = paste0("Crop: ", box$error, "."))); return(invisible()) }
      roi <- write_roi(box, pdir)
    }
    args <- build_args(p, dpi$value, img, pdir, csv, qc = TRUE, roi = roi)
    res  <- withProgress(message = paste("Analysing", basename(img)), value = 0.5, run_rv(rv, args))
    stem <- tools::file_path_sans_ext(basename(img))
    data <- if (file.exists(csv)) read_rv_csv(csv) else NULL
    preview(list(
      img = img, dpi = dpi, dranges = p$dranges, status = res$status,
      cmd = fmt_cmd(rv, args), log = res$log,
      seg  = file.path(pdir, paste0(stem, "_seg.png")),
      feat = file.path(pdir, paste0(stem, "_features.png")),
      crop = box, crop_ignored = !is.null(roi) && roi_ignored(res$log, data),
      data = data))
  }
  
  observeEvent(input$preview_btn, run_preview(params(), force = TRUE))
  observeEvent(list(params_d(), input$preview_img, input$auto_preview), {
    req(isTRUE(input$auto_preview), isTruthy(input$preview_img))
    run_preview(params_d())
  })
  
  output$prev_img <- renderImage({
    pv <- preview()
    validate(need(!is.null(pv), "Pick a sample image to see the preview."))
    validate(need(is.null(pv$error), pv$error))
    validate(need(pv$status == 0, "rv failed on this image: see 'rv command & log'."))
    src <- switch(input$prev_view, features = pv$feat, seg = pv$seg, orig = pv$img)
    if (input$prev_view == "orig" && grepl("\\.tiff?$", src, ignore.case = TRUE)) {
      conv <- tryCatch(
        list(path = withProgress(message = "Converting TIFF for display", value = 0.5, tiff_as_png(src))),
        error = function(e) list(err = conditionMessage(e)))
      validate(need(is.null(conv$err), conv$err))
      src <- conv$path
    }
    if (input$prev_view == "orig")
      validate(need(grepl(WEB_RE, src, ignore.case = TRUE),
                    "Browsers can't display this format. Use the Overlay view: it shows the scan with the detected roots."))
    validate(need(file.exists(src), "No image produced: see 'rv command & log'."))
    ext <- tolower(tools::file_ext(src))
    list(src = src, alt = basename(src),
         contentType = switch(ext, jpg = , jpeg = "image/jpeg", bmp = "image/bmp", gif = "image/gif", "image/png"),
         style = "width: 100%; height: auto; display: block;")
  }, deleteFile = FALSE)

  # zoom + crop frame: the frame follows the crop inputs right away, rv re-runs after the debounce
  observe({
    pv <- preview(); cr <- crop_vals(); img <- input$preview_img
    frame <- FALSE
    if (!is.null(pv) && is.null(pv$error) && isTRUE(pv$status == 0) && isTruthy(img) && crop_active(cr)) {
      b <- crop_box(read_dims(img), cr)
      if (is.null(b$error))
        frame <- list(t = 100 * b$t / b$img_h, r = 100 * b$r / b$img_w,
                      b = 100 * b$b / b$img_h, l = 100 * b$l / b$img_w)
    }
    session$sendCustomMessage("preview-frame", list(zoom = input$zoom %||% 100, crop = frame))
  })
  
  output$prev_table <- renderTable({
    pv <- preview()
    req(pv, is.null(pv$error), !is.null(pv$data), nrow(pv$data) > 0)
    rbind(measure_table(pv$data, pv$dranges),
          data.frame(Measure = "DPI used",
                     Value = if (is.na(pv$dpi$value)) pv$dpi$source
                     else sprintf("%g (%s)", pv$dpi$value, pv$dpi$source)),
          if (!is.null(pv$crop))
            data.frame(Measure = "Analysed area (crop)",
                       Value = if (pv$crop_ignored) "CROP IGNORED by rv, whole image analysed (see log)"
                       else sprintf("%d \u00d7 %d of %d \u00d7 %d px", pv$crop$w, pv$crop$h, pv$crop$img_w, pv$crop$img_h)))
  }, striped = TRUE, spacing = "s", width = "100%")
  
  output$prev_log <- renderText({
    pv <- preview(); req(pv, is.null(pv$error))
    paste0("> ", pv$cmd, "\n\n", pv$log)
  })
  
  # ---- batch: every image, one rv call each ----------------------------------------
  batch <- reactiveVal(NULL)
  
  observeEvent(input$run_batch, {
    imgs <- images(); p <- params(); rv <- rv_bin(); out <- clean_path(input$out_dir)
    err <- check_params(p) %||%
      (if (!file.exists(rv)) "rv.exe not found: check the path under Files.") %||%
      (if (!length(imgs)) "No images found.")
    if (!is.null(err)) { showNotification(err, type = "error"); return() }
    dir.create(out, recursive = TRUE, showWarnings = FALSE)
    tmp <- file.path(tempdir(), "rv_batch")
    unlink(tmp, recursive = TRUE); dir.create(tmp, showWarnings = FALSE)
    
    rows <- vector("list", length(imgs)); logs <- character(); failed <- character(); ignored <- character()
    withProgress(message = "Running rv", value = 0, {
      for (i in seq_along(imgs)) {
        img <- imgs[i]
        setProgress((i - 1) / length(imgs), detail = sprintf("%d / %d: %s", i, length(imgs), basename(img)))
        d    <- resolve_dpi(img, p)
        csv  <- file.path(tmp, sprintf("img%05d.csv", i))
        roi  <- NULL
        if (crop_active(p$crop)) {
          box <- crop_box(read_dims(img), p$crop)
          if (!is.null(box$error)) {
            logs <- c(logs, sprintf("## %s\nSkipped. Crop: %s", basename(img), box$error))
            failed <- c(failed, basename(img))
            next
          }
          roi <- write_roi(box, tmp, sprintf("roi%05d.csv", i))
        }
        args <- build_args(p, d$value, img, out, csv, qc = isTRUE(input$save_qc), roi = roi)
        r    <- run_rv(rv, args)
        logs <- c(logs, sprintf("## %s\n> %s\n%s", basename(img), fmt_cmd(rv, args), r$log))
        if (r$status == 0 && file.exists(csv)) {
          df <- read_rv_csv(csv, as_text = TRUE)
          if (!is.null(roi) && roi_ignored(r$log, df)) ignored <- c(ignored, basename(img))
          df$Image.Path <- rel_path(img, input$img_path)
          df$DPI        <- if (is.na(d$value)) NA_character_ else format(d$value)
          df$DPI.Source <- d$source
          rows[[i]] <- df
        } else failed <- c(failed, basename(img))
      }
      setProgress(1)
    })
    
    data <- bind_rows(rows)
    if (nrow(data)) {
      data <- suppressMessages(type_convert(data))
      data <- relocate(data, File.Name, Image.Path, DPI, DPI.Source)
      write_csv(data, file.path(out, "features.csv"))
    }
    batch(list(data = data, dranges = p$dranges, out = out, failed = failed, ignored = ignored,
               n = length(imgs), log = paste(logs, collapse = "\n\n")))
  })
  
  observeEvent(input$load_prev, {
    f <- file.path(clean_path(input$out_dir), "features.csv")
    if (!file.exists(f)) { showNotification("No features.csv in the output folder yet.", type = "warning"); return() }
    d <- read_rv_csv(f)
    batch(list(data = d, dranges = dranges(), out = clean_path(input$out_dir),
               failed = character(), n = nrow(d), log = paste("Loaded", f), loaded = TRUE))
  })
  
  output$batch_status <- renderUI({
    b <- batch()
    if (is.null(b)) return(p(class = "text-body-secondary mt-3",
                             "Every image is analysed with the settings from the sidebar. Tune them on the Preview tab first."))
    tagList(
      p(class = "mt-3",
        if (isTRUE(b$loaded)) sprintf("Loaded %d rows from ", nrow(b$data))
        else sprintf("%d of %d images analysed. Results: ", nrow(b$data), b$n),
        code(file.path(b$out, "features.csv"))),
      if (length(b$failed)) p(class = "text-danger",
                              "Failed: ", paste(b$failed, collapse = ", "), " (see the log below)."),
      if (length(b$ignored)) p(class = "text-danger",
                               "rv ignored the crop (whole image analysed) for: ", paste(b$ignored, collapse = ", "), "."),
      if ("DPI.Source" %in% names(b$data) && any(grepl("default", b$data$DPI.Source))) p(class = "text-warning",
                                                                                         "72/96 dpi taken from metadata for: ",
                                                                                         paste(b$data$File.Name[grepl("default", b$data$DPI.Source)], collapse = ", "),
                                                                                         ". That is usually a software default; if so, set a fixed DPI and run again.")
    )
  })
  
  output$batch_table <- renderTable({
    b <- batch(); req(b, nrow(b$data) > 0)
    d <- b$data
    if (!isTRUE(input$all_cols)) {
      show_path <- "Image.Path" %in% names(d) && any(grepl("/", d$Image.Path))
      d <- d[, unique(c("File.Name", if (show_path) "Image.Path", key_cols(names(d)),
                        intersect("DPI", names(d)))), drop = FALSE]
    }
    d
  }, striped = TRUE, spacing = "s", digits = 3, na = "")
  
  output$batch_log <- renderText({ b <- batch(); req(b); b$log })
  
  output$dl_features <- downloadHandler(
    filename = "features.csv",
    content = function(file) { b <- batch(); req(b); write_csv(b$data, file) })
  
  # ---- dry mass -------------------------------------------------------------------
  mass <- reactive({
    req(input$mass_file)
    tryCatch(list(data = read_table_auto(input$mass_file$datapath)),
             error = function(e) list(error = conditionMessage(e)))
  })
  
  output$mass_ui <- renderUI({
    m <- mass()
    if (!is.null(m$error)) return(p(class = "text-danger", "Could not read the file: ", m$error))
    d <- m$data
    num_cols <- names(d)[vapply(d, is.numeric, TRUE)]
    if (!length(num_cols)) return(p(class = "text-danger",
                                    "No numeric column found. If the file uses a decimal comma, save it with ';' as separator."))
    tagList(
      layout_columns(
        col_widths = c(3, 3, 2, 4),
        selectInput("mass_file_col", "Column with image name", names(d),
                    guess_file_col(d, isolate(batch())$data)),
        selectInput("mass_col", "Column with dry mass", num_cols, guess_mass_col(num_cols)),
        radioButtons("mass_unit", "Mass unit", c("g", "mg"), inline = TRUE),
        selectInput("soil_col", "Soil/core volume in cm³ (optional, for RLD)",
                    c("none", num_cols), "none")
      ),
      uiOutput("traits_status"),
      div(style = "overflow-x: auto;", tableOutput("traits_table")),
      downloadButton("dl_traits", "Download traits.csv")
    )
  })
  
  traits <- reactive({
    m <- mass()$data
    req(m, input$mass_file_col %in% names(m), input$mass_col %in% names(m))
    b <- batch()
    validate(need(!is.null(b) && nrow(b$data) > 0,
                  "Run the batch (or load an existing features.csv) first: traits combine rv results with dry mass."))
    f <- b$data
    validate(need(all(c("Total.Root.Length.mm", "Surface.Area.mm2", "Volume.mm3") %in% names(f)),
                  "Traits need results in mm: turn on 'Convert pixels to mm' and run the batch again."))
    
    mm <- tibble(key      = stem_key(m[[input$mass_file_col]]),
                 mass_g   = m[[input$mass_col]] * if (identical(input$mass_unit, "mg")) 1e-3 else 1,
                 soil_cm3 = if (!identical(input$soil_col, "none")) m[[input$soil_col]] else NA_real_)
    dups <- unique(mm$key[duplicated(mm$key)])
    mm <- mm[!duplicated(mm$key), ]
    
    j <- f |> mutate(key = stem_key(File.Name)) |> inner_join(mm, by = "key")
    fine_col <- grep("^Root\\.Length\\.Diameter\\.Range\\.1\\.mm$", names(f), value = TRUE)
    dr <- b$dranges; if (!length(dr)) dr <- c(2, 5)
    
    out <- tibble(
      File          = j$File.Name,
      Dry_mass_g    = j$mass_g,
      Root_length_m = j$Total.Root.Length.mm / 1000,
      SRL_m_g       = safe_div(j$Total.Root.Length.mm / 1000, j$mass_g),   # specific root length
      SRA_cm2_g     = safe_div(j$Surface.Area.mm2 / 100, j$mass_g),        # specific root area
      RTD_g_cm3     = safe_div(j$mass_g, j$Volume.mm3 / 1000))              # root tissue density
    len_cols <- grep("^Root\\.Length\\.Diameter\\.Range\\.[0-9]+\\.mm$", names(j), value = TRUE)
    if (length(fine_col))   # share of the class sum: rv's Total.Root.Length uses a different estimator
      out[[sprintf("Length_frac_diam_lt_%gmm", dr[1])]] <- safe_div(j[[fine_col]], rowSums(j[len_cols]))
    if (!identical(input$soil_col, "none"))
      out$RLD_cm_cm3 <- safe_div(j$Total.Root.Length.mm / 10, j$soil_cm3)   # root length density
    
    list(data = out, dups = dups,
         no_mass = setdiff(stem_key(f$File.Name), mm$key),
         no_scan = setdiff(mm$key, stem_key(f$File.Name)))
  })
  
  output$traits_status <- renderUI({
    t <- traits()
    msg <- function(label, x) if (length(x)) p(class = "small text-warning mb-1", label,
                                               paste(head(x, 15), collapse = ", "), if (length(x) > 15) sprintf(" ... (+%d)", length(x) - 15))
    tagList(
      p(class = "small text-body-secondary mb-1", sprintf("%d scan(s) matched.", nrow(t$data))),
      msg("Scans without dry mass: ", t$no_mass),
      msg("Dry mass rows without scan: ", t$no_scan),
      msg("Duplicate names in dry mass file (first row used): ", t$dups))
  })
  
  output$traits_table <- renderTable(traits()$data, striped = TRUE, spacing = "s", digits = 4, na = "")
  
  output$dl_traits <- downloadHandler(
    filename = "traits.csv",
    content = function(file) write_csv(traits()$data, file))
}

shinyApp(ui, server)