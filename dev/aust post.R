## Processing for bulk Teltra Aust Post stock

library(dplyr)
library(magrittr)
library(knitr)
library(lubridate)

## Dependencies
# pdftk is an appplication with the ability to merge pdf files. It is accessed through system commands.
# wkhtmltopdf is an application used to convert html to pdf files. It is accessed through system commands.
# Visa is an Aust Post application for processing tray labels. It is accessed through system commands.

## Setting up
rm(list = ls())

source("ap_configuration.R")
source("ap_validateBookingFiles.R")
source("ap_processTrayLabelFile.R")
source("ap_processPdfFile.R")

## Declarations
t.bookings <- data.frame(dir = character(),
                         region = character(),
                         booking = character(),
                         items = numeric(),
                         bundles = numeric(),
                         trays = numeric(),
                         brick = numeric(),
                         bookingConfirmationFile = character(),
                         mailingStatementFile = character(),
                         uldFile = character(),
                         trayLabelFile = character(),
                         trayLabelOutputFile = character(),
                         stringsAsFactors = FALSE)

t.bookingsColNames <- c("dir", "region", "booking", "items", "bundles", "trays", "brick", "bookingConfirmationFile",
                        "mailingStatementFile", "uldFile", "trayLabelFile", "trayLabelOutputFile")

t.tray <- data.frame(type = c("Large", "Small"), maxweight = c(16 * 1000, 8 * 1000))
t.brickStack <- FALSE

## User prompts
cat("\014")
cat("This application processes bulk Telstra Aust Post bookings.\n\n")

cat("Select the input folder. This is the zip folder downloaded from the Aust Post booking email.\n")

t.check = FALSE
while (t.check == FALSE) {
    t.inputZip <- choose.files(default = t.defaultInputDir, caption = "Select Aust Post folder",
                               multi = FALSE, filters = Filters[c("zip", "All"),])

    if (length(t.inputZip) == 0) {
        stop()
    }

    if (grepl(t.inputZip, pattern = ".*zip$") == FALSE) {
        cat("Please select a Telstra .zip folder\n")
    } else {
        t.check <- TRUE
        cat("\n")
    }
}

unzip(zipfile = t.inputZip, exdir = t.inputDir)

t.dirs <- list.dirs(path=t.inputDir, recursive = FALSE, full.names = TRUE)

sapply(1:length(t.dirs), function(i) {
    t.bookings <<- rbind(t.bookings, rep(NA, ncol(t.bookings)))
})

names(t.bookings) <- t.bookingsColNames

t.bookings$dir <- list.dirs(path=t.inputDir, recursive = FALSE, full.names = TRUE)
t.bookings$region <- list.dirs(path=t.inputDir, recursive = FALSE, full.names = FALSE)

t.check <- FALSE
cat("Select the output folder. This is an empty folder you need to create where the output pdf and tray label files are to be saved.\n")

while(t.check == FALSE) {
    t.outputDir <- choose.dir(default = t.defaultOutputDir, caption = "Select output folder")

    if (is.na(t.outputDir)) {
        stop()
    }

    if (length(list.files(t.outputDir, all.files = TRUE, include.dirs = TRUE, no.. = TRUE)) != 0) {
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

cat("\nThe following dates can be found in the Booking Confirmation Advice PDFs.\n\n")

t.check <- FALSE
while(t.check == FALSE) {
    t.lodgementDate <- readline("Enter the lodgement date (dd/mm/yy): ")
        if (grepl("^[0-3][0-9]/[0-1][0-9]/[0-2][0-9]", t.lodgementDate) && !is.na(as.Date(dmy(t.lodgementDate)))) {
            t.check <- TRUE
            t.lodgementDate <- format(dmy(t.lodgementDate), "%d/%m/%Y")
        }
}

t.check <- FALSE
while(t.check == FALSE) {
    t.startDate <- readline("Enter the delivery start date (dd/mm/yy): ")
    if (grepl("^[0-3][0-9]/[0-1][0-9]/[0-2][0-9]", t.startDate) && !is.na(as.Date(dmy(t.startDate)))) {
        t.check <- TRUE
        t.startDate <- format(dmy(t.startDate), "%d/%m/%Y")
    }
}

t.check <- FALSE
while(t.check == FALSE) {
    t.endDate <- readline("Enter the delivery end date (dd/mm/yy): ")
    if (grepl("^[0-3][0-9]/[0-1][0-9]/[0-2][0-9]", t.endDate) && !is.na(as.Date(dmy(t.endDate)))) {
        t.check <- TRUE
        t.endDate <- format(dmy(t.endDate), "%d/%m/%Y")
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

## Validate booking files
t.bookings <- t.validateBookingFiles(t.bookings)

## Process tray files
t.bookings <- t.processTrayLabelFiles(t.bookings)

## Process PDF files
t.bookings <- t.processPdfFiles(t.bookings)

## Booking list file
cat("Creating booking list file...\n")
t.bookingFile <- paste0(t.outputDir, "/", Sys.Date(), " Booking File.csv")

write.table(dplyr::select(t.bookings, region, booking, items, bundles, trays, brick),
            sep = ",",
            row.names = FALSE,
            col.names = c("Region", "Booking", "Items", "Bundles", "Trays", "Brick"),
            file = t.bookingFile)

## VISA
# Executable found at C:\Program Files\Australia Post\VisaTLMS\VisaCommand.exe - Environment variable added on Violet's computer.

cat("\nPrinting labels...")

t.visaCommand <- function(sw, arg) {
    t.command <- paste0("VisaCommand ", sw, " \"", arg, "\"")
    t.result <- system(t.command, intern = TRUE)
    t.result

    # Log file created - see VISA user guide for details
}

## Debug
# t.visaCommand <- function(sw, arg) {
#     cat(arg)
# }
## End debug

sapply(1:nrow(t.bookings), function(n) {
    if (is.na(t.bookings$trayLabelOutputFile[n])) {
        return()
    }

    readline(paste("Press <ENTER> to print label file for", t.bookings$region[n]))

    t.visaCommand("/i", t.bookings$dir[n])
    t.visaCommand("/p", t.bookings$booking[n])
    t.visaCommand("/d", t.bookings$booking[n])
})

cat("\n\nFinished printing tray labels.")


