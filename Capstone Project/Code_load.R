#Packages
library(downloader)
library(readr)

#URL Link
url_zip <- c("https://github.com/UNSW-ZZSC9020/project/tree/main/data/Australia/a.zip",
             "https://github.com/UNSW-ZZSC9020/project/tree/main/data/Australia/b.zip",
             "https://github.com/UNSW-ZZSC9020/project/tree/main/data/Australia/c.zip",
             "https://github.com/UNSW-ZZSC9020/project/tree/main/data/Australia/d.zip")


#Store
all_data <- list()

# Loop through each ZIP file URL
for (zip_url in url_zip) {
  
  zip_path <- tempfile()
  
  
  download(zip_url, zip_path)
  
  
  temp_dir <- getwd()
  
  
  unzip(zip_path, exdir = temp_dir)
  
  
  csv_files <- list.files(temp_dir, pattern = "\\.csv$", full.names = TRUE)
  
  
  for (csv_file in csv_files) {
    data <- read_csv(csv_file)
    all_data[[length(all_data) + 1]] <- data
  }
  
  
  unlink(zip_path)  # Remove the ZIP file
  unlink(temp_dir, recursive = TRUE)  # Remove the unzipped files
}


df <- do.call(rbind, all_data)


