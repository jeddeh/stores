## Processing for bulk Teltra Aust Post stock

library(dplyr)
library(magrittr)
library(knitr)

## Dependencies
# pdftk is an appplication with the ability to merge pdf files. It is accessed through system commands.
# wkhtmltopdf is an application used to convert html to pdf files. It is accessed through system commands.
# Visa is an Aust Post application for processing tray labels. It is accessed through system commands.

## Setting up
rm(list = ls())

# setwd(dirname(sys.frame(1)$ofile))
setwd("C:/Users/Rob/Desktop/Stores/dev")

# PC: '0' - Violet's PC, '1' - Receiving PC, '2' - Home PC
t.pc <- '2'

source("ap_processTrayLabelFile.R")
source("ap_processPdfFile.R")

t.baseDir <- "~/AustPost"
closeAllConnections()
unlink(t.baseDir, recursive = TRUE)
dir.create(t.baseDir)

## Declarations
t.bookings <- data.frame(dir = "",
                         region = "",
                         booking = "",
                         items = 0,
                         bundles = 0,
                         trays = 0,
                         brick = 0,
                         bookingConfirmationFile = "",
                         mailingStatementFile = "",
                         uldFile = "",
                         trayLabelFile = "",
                         stringsAsFactors = FALSE)

# ULD files are used to generate ULD tags.
t.uldRmdFile <- switch(t.pc,
    '0' = ,
    '1' = ,
    '2' = "C:/Users/Rob/Desktop/Stores/dev/uld files/uld.Rmd"
)

t.uldCssFile <- switch(t.pc,
   '0' = ,
   '1' = ,
   '2' = "C:/Users/Rob/Desktop/Stores/dev/uld files/uld.css"
)

t.tray <- data.frame(type = c("Large", "Small"), maxweight = c(16 * 1000, 8 * 1000))
t.brickStack <- FALSE

## User prompts
cat("\014")
cat("This application processes bulk Telstra Aust Post bookings.\n\n")

cat("Select input folder\n")

t.check = FALSE
while (t.check == FALSE) {
    defaultDir <- switch (t.pc,
        '0' = "C:/Users/saadv/Desktop",
        '1' = "T:/Warehouse/Store Copies/Rob/dev",
        '2' = "C:/Users/Rob/Desktop/Stores/dev/data")

    t.inputDir <- choose.files(default = defaultDir, caption = "Select Aust Post folder",
                               multi = FALSE, filters = Filters[c("zip", "All"),])

    if (grepl(t.inputDir, pattern = ".*zip$") == FALSE) {
        cat("Please select a Telstra .zip folder\n")
    } else {
        t.check <- TRUE
        cat("\n")
    }
}

unzip(zipfile = t.inputDir, exdir = file.path(t.baseDir, "/input"))
t.bookings$dir <- list.dirs(path=t.inputDir, recursive = FALSE, full.names = TRUE)
t.bookings$region <- list.dirs(path=t.inputDir, recursive = FALSE, full.names = FALSE)

t.check <- FALSE
cat("Select output folder\n")

while(t.check == FALSE) {
    defaultDir <- switch(t.pc,
        '0' = "C:/Users/saadv/Desktop/",
        '1' = "T:/Warehouse/Store Copies/Rob/dev/",
        '2' = "C:/Users/Desktop/Stores/dev/data/Telstra Output Files/"
    )

    t.outputDir <- choose.dir(default = "C:/Users/Desktop/Stores/dev/data/Telstra Output Files/", caption = "Select output folder")

    if (length(dir(all.files=TRUE)) != 0) {
        cat("Please select an empty folder\n")
    } else {
        t.check <- TRUE
        cat("\n")
    }
}

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
t.processTrayLabelFiles()

## Process PDF files
t.processPdfFiles()

## Booking list file
cat("\nCreating booking list file...\n\n")
t.bookingFile <- paste0(t.outputDir, "/", Sys.Date(), " Booking File.csv")

write.table(select(t.bookings, region, booking, items, bundles, trays, brick),
            sep = ",",
            row.names = FALSE,
            col.names = c("Region", "Booking", "Items", "Bundles", "Trays", "Brick"),
            colfile = t.bookingFile)

## VISA
# Executable found at C:\Program Files\Australia Post\VisaTLMS\VisaCommand.exe - Environment variable added on Violet's computer.

# t.visaCommand <- function(sw, arg) {
#     t.command <- paste0("VisaCommand ", sw, " \"", arg, "\"")
#     t.result <- system(t.command, intern = TRUE)
#     t.result
#
#     # Log file created - see VISA user guide for details
# }

## Debug
t.visaCommand <- function(sw, arg) {
    cat(arg)
}
## End debug

sapply(t.bookings$trayLabelFile, function(trayLabelFile) {
    readline(paste("\nPress <ENTER> to print label file for", basename(file.trayLabelFile$file)))

    t.visaCommand("/i", labelFile$file)
    t.visaCommand("/p", labelFile$booking)
    t.visaCommand("/d", labelFile$booking)
})

cat("\n\nFinished printing tray labels.")


