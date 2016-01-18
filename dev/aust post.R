## Processing for bulk Teltra Aust Post stock

library(dplyr)
library(magrittr)
library(knitr)

# script.dir <- "C:/Users/Rob/Desktop/Stores/dev/"
# script.dir <- dirname(sys.frame(1)$ofile)
# setwd(script.dir)

rm(list = ls())

# PC: '0' - Violet's PC, '1' - Receiving PC, '2' - Home PC
t.pc <- '2'

t.wd <- switch(t.pc,
    '0' = ,
    '1' = ,
    '2' = "C:/Users/Rob/Desktop/Stores/dev/"
    )

setwd(t.wd)
source("ap_processTrayLabelFile.R")
source("ap_processPdfFile.R")

## Declarations

# pdftk is an appplication with the ability to merge pdf files. It is accessed through system commands.
t.pdftkApp <- switch (t.pc,
    '0' = "T:\\Warehouse\\PDFTKBuilderPortable\\App\\pdftkbuilder\\pdftk.exe",
    '1' = "T:\\Warehouse\\PDFTKBuilderPortable\\App\\pdftkbuilder\\pdftk.exe",
    '2' = "C:\\Users\\Rob\\Desktop\\Stores\\PDFTKBuilderPortable\\App\\pdftkbuilder\\pdftk.exe"
)

# wkhtmltopdf is an application used to convert html to pdf files. It is accessed through system commands.
# ULD files are used to generate ULD tags.
t.uldRmdFile <- switch(t.pc,
    '0' = ,
    '1' = ,
    '2' = "C:\\Users\\Rob\\Desktop\\Stores\\dev\\uld files\\uld.Rmd"
)

t.uldCssFile <- switch(t.pc,
   '0' = ,
   '1' = ,
   '2' = "C:\\Users\\Rob\\Desktop\\Stores\\dev\\uld files\\uld.css"
)

t.tray <- data.frame(type = c("Large", "Small"), maxweight = c(16 * 1000, 8 * 1000))
t.brickStack <- FALSE

## User prompts
cat("\014")
cat("This application processes bulk Telstra Aust Post bookings.\n\n")

cat("Select input folder\n\n")

t.inputDir <- switch (t.pc,
    '0' = choose.dir(default = "C:/Users/saadv/Desktop/", caption = "Select Aust Post folder"),
    '1' = choose.dir(default = "T:/Warehouse/Store Copies/Rob/dev/", caption = "Select Aust Post folder"),
    '2' = choose.dir(default = "C:/Users/Rob/Desktop/Stores/dev/data/Telstra Aust Post/", caption = "Select Aust Post folder")
)

t.dirs <- list.dirs(path=t.inputDir, recursive = FALSE, full.names = FALSE)
t.bookings <- data.frame(Region = t.dirs, Booking = "", Items = 0, Bundles = 0, Trays = 0, Brick = 0, stringsAsFactors = FALSE)

cat("Select output folder\n\n")

t.outputDir <- switch(t.pc,
    '0' = choose.dir(default = "C:/Users/saadv/Desktop/", caption = "Select output folder"),
    '1' = choose.dir(default = "T:/Warehouse/Store Copies/Rob/dev/", caption = "Select output folder"),
    '2' = choose.dir(default = "C:/Users/Desktop/Stores/dev/data/Telstra Output Files/", caption = "Select output folder")
)

t.check <- FALSE
while(t.check == FALSE) {
    t.weight <- as.numeric(readline("Enter weight per catalogue (g): "))

    if (!is.na(t.weight) && t.weight > 0) {
        t.check <- TRUE
    }
}

t.check <- FALSE
while(t.check == FALSE) {
    t.bundleSize <- as.integer(t.bundleSize <- readline("Enter bundle size: "))

    if (!is.na(t.bundleSize) && t.bundleSize > 0) {
        t.check <- TRUE
    }
}

t.trayType <- "Large"
cat(paste0("\nDefaulting to ", tolower(t.trayType) ," tray size (max ", t.tray$maxweight[t.tray$type == t.trayType] / 1000,"kg)\n\n"))

t.maxBundlesPerTray <- floor(t.tray$maxweight[t.tray$type == t.trayType] / (t.weight * t.bundleSize))
t.maxWeightPerTray <- t.maxBundlesPerTray * t.bundleSize * t.weight

t.check <- FALSE
while(t.check == FALSE) {
    maxBundles <- as.integer(readline(paste0("Enter maximum bundles per tray (max ", t.maxBundlesPerTray, "): ")))

    if (!is.na(maxBundles) && maxBundles > 0 && maxBundles <= t.maxBundlesPerTray) {
        t.check <- TRUE
    }
}

t.maxBundlesPerTray <- maxBundles
t.maxTraySize <- t.maxBundlesPerTray * t.bundleSize

## Process tray files
cat("Processing tray label files...\n")

sapply(t.dirs, function(t.dir) {
    t.dirPath <- file.path(t.inputDir, t.dir)
    trayLabelFiles <- list.files(path = t.dirPath, recursive = FALSE, pattern = "*TrayLabelLPF.lpf", full.names = FALSE)

    if (length(trayLabelFiles) == 0) {
        cat(paste0("\n** WARNING: There is no tray label file in directory ", t.dir))
        readline("Press <Enter> to Continue...")
        return()
    }

    if (length(trayLabelFiles) > 1) {
        cat(paste0("\n** WARNING: Multiple tray label files exist in directory ", t.dir))
        cat("No tray labels for this directory will be processed.")
        readline("Press <Enter> to Continue...")
        return()
    }

    t.processTrayLabelFile(t.dir, t.dirPath, t.outputDir, trayLabelFiles[1])
})

## Process PDF files
cat("\nValidating pdf files...\n\n")
t.processPdfFile(t.bookings) %>% na.omit()

## Booking list file
cat("\nCreating booking list file...\n\n")

t.bookingFile <- paste0(t.outputDir, "\\", Sys.Date(), " Booking File.csv")

if (file.exists(t.bookingFile)) {
    cat("** WARNING: Existing file will be overwritten.\n\n")
    file.remove(t.bookingFile)
}

write.table(t.bookings, sep = ",",row.names = FALSE, file = t.bookingFile)

## VISA
# Executable found at C:\Program Files\Australia Post\VisaTLMS\VisaCommand.exe - Environment variable added on Violet's computer.

t.visaCommand <- function(sw, arg) {
    t.command <- paste0("VisaCommand ", sw, " \"", arg, "\"")
    t.result <- system(t.command, intern = TRUE)
    t.result

    ## ***TODO: May need to check log file for errors - see VISA user guide
}

t.visaCommand("/i", t.labelFile)
t.visaCommand("/p", t.labelHeader)
t.visaCommand("/d", t.labelHeader)

