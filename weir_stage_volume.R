# Load libraries
library(magick)
library(httr2)
library(lubridate)

# Base URL for the Konza Prairie LTER weir data
base_url <- "https://www.konza.ksu.edu/ramps/genomics/weir"

# ── Check Modified Time ─────────────────────────────────────────────────────

#' Get the last modified timestamp for a weir image
#'
#' @param name Character string. The image name without extension ("stage" or "volume")
#' @return A POSIXct datetime object in Etc/GMT+6 timezone
#'
#' @details This function:
#'   1. Sends a HEAD request (gets metadata without downloading the file)
#'   2. Extracts the "last-modified" HTTP header
#'   3. Parses the date string (removing weekday prefix like "Tue, ")
#'   4. Converts to GMT then shifts to Etc/GMT+6 (Central Standard Time, UTC-6)
get_last_modified <- function(name) {
  # Construct the full URL (e.g., https://.../weir/stage.gif)
  url <- paste0(base_url, "/", name, ".gif")

  # Send HEAD request to get only response headers, not the file body
  res <- httr2::request(url) |> # Create HTTP request object
    httr2::req_method("HEAD") |> # Change from default GET to HEAD
    httr2::req_perform() # Execute the request

  # Extract and parse the Last-Modified header
  httr2::resp_header(res, "last-modified") |> # Get header value (e.g., "Tue, 25 May 2026 14:30:00 GMT")
    sub("^[A-Za-z]{3}, ", "", x = _) |> # Remove weekday prefix (e.g., "Tue, " → "")
    lubridate::dmy_hms(tz = "GMT") |> # Parse as "day month year hour:min:sec" in GMT
    lubridate::with_tz("Etc/GMT+6") # Convert to Central Standard Time (UTC-6, no DST)
}

# ── Download ────────────────────────────────────────────────────────────────

#' Download a weir image from the server
#'
#' @param name Character string. The image name without extension ("stage" or "volume")
#' @return A magick image object ready for processing/saving
#'
#' @details Uses magick::image_read() which handles GIF files directly via HTTP
fetch_weir_image <- function(name) {
  url <- paste0(base_url, "/", name, ".gif")
  magick::image_read(url) # Downloads and parses the GIF image
}

# Download both images from the server
stage_img <- fetch_weir_image("stage") # Stage height/water level image
discharge_img <- fetch_weir_image("volume") # Volume/discharge rate image

# ── Create Shared Date Prefix ───────────────────────────────────────────────

# Get the last modified timestamp for the stage image (both images have same date)
# Format it as YYYYMMDD for use in filenames (e.g., "20260525")
file_date <- get_last_modified("stage") |>
  format("%Y%m%d")

# Create output filenames with date prefix and descriptive names
stage_file <- paste0("pics/", file_date, "_weir_stage.png") # e.g., "pics/20260525_weir_stage.png"
discharge_file <- paste0("pics/", file_date, "_weir_discharge.png") # e.g., "pics/20260525_weir_discharge.png"

# ── Save ────────────────────────────────────────────────────────────────────

# Save the stage image as PNG format (converting from source GIF)
magick::image_write(
  stage_img, # Magick image object to save
  path = stage_file, # Output file path
  format = "png" # Convert to PNG format
)

# Save the discharge image as PNG format
magick::image_write(
  discharge_img, # Magick image object to save
  path = discharge_file, # Output file path
  format = "png" # Convert to PNG format
)
