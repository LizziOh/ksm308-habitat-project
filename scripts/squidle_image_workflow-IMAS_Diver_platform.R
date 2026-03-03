# based off Jac's script at https://github.com/jacquomo/squidle/blob/master/getting_bruv_images_ready_SQUIDLE%2B

### Load libraries
library(tidyverse)
library(magick)
library(lubridate)
library(leaflet)
library(fs)
library(geosphere)

## 1.0 Do some setup
### Set the path to the folder containing the images
folder_path <- choose.dir() #navigate to folder
setwd(folder_path)
getwd()


### Set up a naming conventions for folders etc. This is important to be the same/consistent between uploads
institute = "IMAS" #set this for your institute. should remain the same between uploads
platform = "IMAS_Diver" #set this for each platform format should remain the same between uploads 
campaign = "202602_MariaIsland" #set this for each campaign. format should remain the same between uploads 
jpgname = "photoquadrat" #dont change this unless you are working with a different platform
full_res = "full_res"#dont change this
thumbnails = "thumbnails" #dont change this



## 2.0 Read in metadata and sort out attribute names

metadata <- read.csv(list.files(pattern="\\.csv$")[1]) |> 
  mutate(
    date = as.Date(date, format = "%d/%m/%Y"),
    site = str_replace_all(site," ","_"),
    deployment_name = paste0(format(date, "%Y%m%d"),"_",site,"_","T",transect_number))

meta <- metadata %>%
  mutate(
    ts_local = dmy_hm(str_replace(date_time, "T", " "), tz = "Australia/Hobart"),# parse "22/03/2024T13:00" as a local datetime
    ts_utc = with_tz(ts_local, "UTC"), # convert that instant to UTC
    timestamp_start = format(ts_utc, "%Y-%m-%d %H:%M:%SZ"),# ISO 8601 string with Z
    pose.lat = latitude_dd,
    pose.lon = longitude_dd,
    pose.dep = depth_m,
    pose.data.bearing = bearing
  ) %>%
  select(site, pose.lat,pose.lon,pose.dep,pose.data.bearing, timestamp_start)
glimpse(meta)

### Set up folder structure
#### Define the base directory (you can adjust this as needed)
deployment_name <- metadata$deployment_name[1]
base_dir <- file.path(platform, campaign,deployment_name)

#### Define the folders to create
folders <- c("full_res", "thumbnails")

#### Create the folder structure
for (folder in folders) {
  dir_path <- file.path(base_dir, folder)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    message("Created folder: ", dir_path)
  } else {
    message("Folder already exists: ", dir_path)
  }
}

### 2.1 Lets do some basic checks to make sure everything looks right in metadata contents
#### Check for missing values
missing_values <- meta %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Column", values_to = "Missing_Count")

print("Missing values per column:")
print(missing_values)

#### Check for duplicate rows
duplicates <- meta %>%
  filter(duplicated(.))

if (nrow(duplicates) > 0) {
  print("Duplicate rows found:")
  print(duplicates)
} else {
  print("No duplicate rows found.")
}

#### Check data types and ranges
column_summary <- meta %>%
  reframe(across(everything(),
                 list(class = ~ class(.),
                      min = ~ if (is.numeric(.)) min(., na.rm = TRUE) else NA,
                      max = ~ if (is.numeric(.)) max(., na.rm = TRUE) else NA)
                 ))

print("Column data types and ranges:")
print(column_summary)

#### Check for any empty or zero-length strings in character columns
empty_strings <- meta %>%
  summarise(across(where(is.character), ~ sum(. == "")))

print("Empty strings in character columns:")
print(empty_strings)

#### Check for any empty or negative depth values
if ("pose.dep" %in% colnames(meta)) {
  depth_issues <- meta %>%
    filter(is.na(pose.dep) | pose.dep < 0)
  
  if (nrow(depth_issues) > 0) {
    print("Rows with empty or negative depth values in 'Depth':")
    print(depth_issues)
  } else {
    print("No empty or negative depth values in 'Depth'.")
  }
} else {
  print("Column 'Depth' not found in metadata.")
}

## 3.0 Now lets work on checking and moving images into folders
### 3.1 Extra optional - Sometimes more cleaning is needed. In this example we are removing unnecessary part of a string at end of image file name
#### List all .jpg files in the folder
image_names <- list.files(file.path(getwd()), pattern = "\\(jpg|jpeg|png|gif|bmp|tiff|JPG|JPEG|PNG|GIF|BMP|TIFF)$", full.names = TRUE)

#### Remove any spaces and dots from the middle of the image names (eg. in a decimal depth "MIR-S5_HF5.3m220324ReturnPoint (10).JPG")

cleaned_names <- image_names |>
  basename() |>
  gsub(" ", "_", x = _) |>
  sub("^(.*)\\.([^.]+)$", "\\1_EXT_\\2", x = _) |>
  gsub("\\.", "-", x = _) |>
  gsub("_EXT_", "\\.", x = _)


# #### Create new file paths
new_file_paths <- file.path(file.path(getwd()), cleaned_names)

#### Rename files
file.rename(image_names, new_file_paths)

#### Confirm renamed files
list.files(file.path(getwd()))

### 3.2 

##### set output folder
full_res_folder <- file.path(getwd(), base_dir, "full_res")

#### List all images files in the folder
image_files <- list.files(file.path(getwd()),pattern = "\\.(jpg|jpeg|png|gif|bmp|tiff|JPG|JPEG|PNG|GIF|BMP|TIFF)$", full.names = TRUE)
image_files

##### Loop through and rename/move files
for (file in image_files) {
  tryCatch({
    ## Check if the file exists
    if (!file.exists(file)) {
      message(paste("File not found:", file))
      next
    }
    
    ## Construct new file name with .jpg extension
    new_file <- file.path(full_res_folder, gsub(".JPG", ".jpg", basename(file)))
    
    ## Check if moving across drives, use file.copy + unlink instead
    if (!file.rename(file, new_file)) {
      if (file.copy(file, new_file)) {
        unlink(file) # Only remove original if copy succeeds
        message(paste("Copied and removed:", file, "to", new_file))
      } else {
        message(paste("Failed to copy:", file))
      }
    } else {
      message(paste("Moved:", file, "to", new_file))
    }
  }, error = function(e) {
    message(paste("Error with file:", file, "Error:", e$message))
  })
}


### 3.3 Now lets join jpg images to metadata for SQ+ and adjust the coordinates and timestamp per image along the transect
#### Sample data frame
#generate image coordinates from transect start and bearing of transect
interval = 2.5 #change as needed. in metres

interim.metadata <- list.files(path = full_res_folder, pattern = "\\.jpg$", ignore.case = TRUE, full.names = TRUE, recursive = FALSE) %>%
  as.data.frame() %>%
  rename(image.path = ".") %>%
  mutate(
    image = str_extract(image.path, "[^/]+$"),
    key = str_remove(image, "\\.jpg$"),
    site = meta$site[1]) |> 
  left_join(meta, by = 'site') %>%
  mutate(
    order_id = str_extract(key, "\\((\\d+)\\)") |>
      str_remove_all("[()]") |>
      as.integer()
  ) |>
  arrange(order_id) |> 
  dplyr::select("key","pose.lat","pose.lon","pose.dep","pose.data.bearing","timestamp_start")%>%
  mutate(
    distance_m = (row_number() - 1) * interval
  ) |> #note you may need to change selection if you dont have a grant number
  glimpse()

final.metadata <- interim.metadata %>%
  mutate(
    new_coords = pmap(
      list(pose.lon, pose.lat, pose.data.bearing, distance_m),
      ~ destPoint(
        p = c(..1, ..2),
        b = ..3,
        d = ..4
      )
    ),
    pose.lon = round(map_dbl(new_coords, 1), 7),
    pose.lat = round(map_dbl(new_coords, 2), 7)
  ) %>%
  mutate(
    timestamp_start = ymd_hms(timestamp_start, tz = "UTC"),
    timestamp_start = timestamp_start + minutes(row_number() - 1)
  ) |> 
  select(-new_coords, -distance_m) 





## 4.0 Plot metadata on map to make sure it looks ok
### Create a Leaflet map

# leaflet(final.metadata) %>%
#   addTiles() %>%
#   addMarkers(lng = ~pose.lon, lat = ~pose.lat, label = ~key)

leaflet(final.metadata) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~pose.lon,
    lat = ~pose.lat,
    radius = 3,
    stroke = FALSE,
    fillOpacity = 0.8,
    label = ~key
  )

# ### Write out final metadata for later
write.csv(final.metadata, file.path(file.path(getwd(), base_dir), paste0(campaign,"_",metadata$deployment_name[1],"_Metadata",".csv")), row.names = FALSE, na = "") ##change name as needed leaving Metadata_formatted. Optional save if you need it

## 5.0 Now create thumbnails
### Create list of full res images
jpg_files <- list.files(full_res_folder, pattern = ".jpg", full.names = TRUE, ignore.case = TRUE)

### Set thumbnail folder
thumbnails_folder <- file.path(getwd(),base_dir, "thumbnails")

### Loop through the jpg files, resize, and save as thumbnails
for (jpg_file in jpg_files) {
  ## Read the image
  img <- image_read(jpg_file)
  
  ## Calculate the thumbnail dimensions to ensure a maximum height of 350px
  original_width <- image_info(img)$width
  original_height <- image_info(img)$height
  
  thumbnail_height <- min(350, original_height)  # Ensure thumbnail_height is a maximum of 350px
  thumbnail_width <- (thumbnail_height / original_height) * original_width  # Maintain the original aspect ratio
  
  ## Resize the image
  img_thumbnail <- image_scale(img, geometry = paste0(round(thumbnail_width), "x", round(thumbnail_height)))
  
  ## Extract the file name without extension
  file_name <- tools::file_path_sans_ext(basename(jpg_file))
  
  ## Save the thumbnail in the output folder
  thumbnail_output_path <- file.path(thumbnails_folder, paste0(file_name, ".jpg"))
  image_write(img_thumbnail, path = thumbnail_output_path, format = "jpeg")
}


# bind metadata and map transects

library(readr)
library(dplyr)
library(purrr)

# Set your folder path
folder_path2 <- file.path(folder_path, platform,campaign)

# Find all CSV files recursively
csv_files <- list.files(
  path = folder_path2,
  pattern = "\\.csv$",
  full.names = TRUE,
  recursive = TRUE
)

# Read and bind all rows
combined_df <- csv_files %>%
  map_dfr(read_csv, .id = "source_file")


leaflet(combined_df) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~pose.lon,
    lat = ~pose.lat,
    radius = 3,
    stroke = FALSE,
    fillOpacity = 0.8,
    label = ~key
  )
