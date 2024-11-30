# Load the dfMaker function into the R session
load("dfMaker.rda")

# Example 1: Basic use with auto-saving, no extra configuration variables provided
# This will process OpenPose JSON files from the specified input folder and automatically
# save the output in the default "./df_outputs/" directory with a name based on the unique ID found in the data.
example1 <- dfMaker(input.folder = "./dfMakerExample/exampleVideos/2006-01-14_0600_US_KTTV-FOX_Ten_OClock_News_273-275_ID202_back_then/")

# Example 2: Specifying an output path, but with no_save set to TRUE
# This will process the data and not save it to a file, useful for in-memory analysis or when saving is not required.
example2 <- dfMaker(input.folder = "./dfMakerExample/exampleVideos/2006-01-14_0600_US_KTTV-FOX_Ten_OClock_News_273-275_ID202_back_then/",
                    output.path = "./dfMakerExample/outputs",
                    no_save = TRUE,
                    #transformation_coords = c(NA,4,7,NA),
                    fast_scaling = TRUE)

# Example 3: Specifying an output file and path to save as a Parquet file
# This will process the data and save it to the specified Parquet file within the given output path.
example3 <- dfMaker(input.folder = "./dfMakerExample/exampleVideos/2006-01-14_0600_US_KTTV-FOX_Ten_OClock_News_273-275_ID202_back_then/",
                    output.file = "./dfMakerExample/outputs/processed_data.parquet")

# Example 3 (B): Specifying an output file and path to save as a CSV file
# This will process the data and save it to the specified CSV file within the given output path.
example3_bi <- dfMaker(input.folder = "./dfMakerExample/exampleVideos/2006-01-14_0600_US_KTTV-FOX_Ten_OClock_News_273-275_ID202_back_then/",
                       output.file = "./dfMakerExample/outputs/processed_data.csv")


# Example 4: Processing with additional configuration and no_save set to FALSE (default)
# Assuming a config file is available that specifies additional metadata to be extracted.
# This will process the data according to the configuration and save it automatically.
example4 <- dfMaker(input.folder = "./dfMakerExample/exampleVideos/2006-01-14_0600_US_KTTV-FOX_Ten_OClock_News_273-275_ID202_back_then/",
                    config.path = "./configuration_files/config_dfMaker.json",
                    output.path = "./dfMakerExample/outputs")
