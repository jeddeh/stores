## Processing for bulk Teltra Aust Post stock

library(dplyr)
library(magrittr)

rm(list = ls())

## Declarations
# pdftk is an appplication with the ability to merge pdf files. It is accessed through system commands.
t.pdftkApp <- "C:\\Users\\Rob\\Desktop\\Stores\\PDFTKBuilderPortable\\App\\pdftkbuilder\\pdftk.exe"

t.tray <- data.frame(type = c("Large", "Small"), maxweight = c(16 * 1000, NULL))

## User variables
cat("\014")
cat("This application processes bulk Telstra Aust Post bookings.\n\n")

cat("Select input folder\n\n")

t.path <- choose.dir(default = "C:/Users/Rob/Desktop/Stores/dev/Telstra Aust Post/",
                     caption = "Select Aust Post folder")

t.dirs <- list.dirs(path=t.path, recursive = FALSE)

cat("Select output folder\n\n")

t.outputDir <-  choose.dir(default = "C:/Users/Rob/Desktop/Stores/dev/Telstra Output Files/",
                           caption = "Select output folder")

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

## Process tray label file
t.processTrayLabelFile <- function(t.inputDir, outputDir, t.file) {
    # Read file
    t.data <- readLines(paste0(t.inputDir, "\\", t.file), warn = FALSE)
    t.start <- match("#Label Details", t.data)
    t.end <- match("#End Of File", t.data)
    t.headers <- strsplit(gsub("#", "", t.data[t.start + 1]), ",")[[1]]

    t.frame <- read.table(text = t.data,
                          header = FALSE,
                          sep = ",",
                          skip = t.start,
                          stringsAsFactors = FALSE,
                          col.names = t.headers)

    # Add columns to t.frame splitting Additional_Text column data

    # Regex explanation
    # ^ - starts with
    # . - any character except new line
    # *A - any one of more of previous expression ending in letter A
    # ... then delete this part of the string
    t.frame$Tray_Qty <- as.numeric(gsub("^.*A","", t.frame$Additional_Text))

    t.combined <- aggregate(Tray_Qty ~ Service + Sort_Plan_Type + Sort_Plan + Destination_Ind + Mail_Size + Label_Qty + Date,
                            data = t.frame, FUN = sum)

    # Make t.newframe which will hold the final csv data
    t.newframe <- data.frame(row.names = names(t.combined))

    invisible(sapply(1:nrow(t.combined), function(i) {
        quantity <- t.combined$Tray_Qty[i]
        fullTrays <- floor(quantity / t.maxTraySize)
        partialTrayQuantity <- quantity %% t.maxTraySize

        fullTrayRow <- t.combined[i, ]
        fullTrayRow$Tray_Qty <- t.maxTraySize

        if (fullTrays > 0) {
            sapply(1:fullTrays, function(n) {
                t.newframe <<- rbind(t.newframe, fullTrayRow)
            })
        }

        if (partialTrayQuantity > 0) {
            partialRow <- t.combined[i, ]
            partialRow$Tray_Qty <- partialTrayQuantity
            t.newframe <<- rbind(t.newframe, partialRow)
        }
    }))

    t.newframe <- arrange(t.newframe, desc(Tray_Qty), Sort_Plan)

    invisible(sapply(1:nrow(t.newframe), FUN = function(n) {
        t.newframe$Additional_Text[n] <<- paste0("T", n, "/", nrow(t.newframe), " A", t.newframe$Tray_Qty[n])
    }))

    t.newframe <- select(t.newframe, -Tray_Qty)

    con <- textConnection("t.newcsv", "w")
    write.table(t.newframe, con, quote = FALSE, row.names = FALSE, col.names = FALSE, sep = ",")
    close(con)

    t.newdata <- c(t.data[1:t.start + 1], t.newcsv, t.data[t.end])
    writeLines(t.newdata, con = paste0(t.outputDir, "\\", t.file))
}

## Process PDF files
t.processPdfFile <- function() {
    pdfOutputFiles <- NULL

    sapply(t.dirs, function(t.dir) {
        bookingConfirmationFiles <- list.files(path = t.dir,
                                               recursive = FALSE,
                                               pattern = "*BookingConfirmationAdvice.pdf",
                                               full.names = TRUE)

        mailingStatementFiles <- list.files(path = t.dir,
                                            recursive = FALSE,
                                            pattern = "*MailingStatement.pdf",
                                            full.names = TRUE)

        ## File validation
        validBookingConfirmation <- TRUE
        validMailingStatement <- TRUE

        if (length(bookingConfirmationFiles) == 0) {
            validBookingConfirmation <- FALSE
            cat(paste0("** WARNING: There is no booking confirmation advice file in directory ", t.dir))
            readline("Press <Enter> to Continue...")
        }

        if (length(bookingConfirmationFiles) > 1) {
            validBookingConfirmation <- FALSE
            cat(paste0("** WARNING: Multiple booking confirmation advice files exist in directory ", t.dir))
            cat("No booking confirmation advice for this directory will be processed.")
            readline("Press <Enter> to Continue...")
        }

        if (length(mailingStatementFiles) == 0) {
            validMailingStatement = FALSE
            cat(paste0("** WARNING: There is no mailing statement file in directory ", t.dir))
            readline("Press <Enter> to Continue...")
        }

        if (length(mailingStatementFiles) > 1) {
            validMailingStatement = FALSE
            cat(paste0("** WARNING: Multiple mailing statement advice files exist in directory ", t.dir))
            cat("No mailing statement for this directory will be printed.")
            readline("Press <Enter> to Continue...")
        }

        if (validBookingConfirmation) {
            pdfOutputFiles <<- c(pdfOutputFiles, bookingConfirmationFiles[1])
        }

        if (validMailingStatement) {
            pdfOutputFiles <<- c(pdfOutputFiles, mailingStatementFiles[1])
        }
    })
    pdfOutputFiles
}

cat("\nValidating pdf files...\n\n")
t.pdfOutputFiles <- NULL
t.pdfOutputFiles <- t.processPdfFile() %>% na.omit()

cat("\nMerging pdf files...\n\n")
t.pdfOutputFiles <- paste0("\"", t.pdfOutputFiles, "\"")
t.pdfOutputFile <- paste0(t.outputDir, "\\", Sys.Date(), " Telstra Aust Post.pdf")

if (file.exists(t.pdfOutputFile)) {
    cat("** WARNING: Existing file will be overwritten.\n\n")
    file.remove(t.pdfOutputFile)
}

t.pdfOutputFile <- paste0("\"", t.pdfOutputFile, "\"")

t.mergeCommand <- paste(t.pdftkApp,
             paste(t.pdfOutputFiles, collapse = " "),
             "cat output",
             t.pdfOutputFile)

t.result <- system(t.mergeCommand, intern = TRUE)

if (length(t.result) != 0) {
    cat(paste(t.result, collapse = "\n"))
    readline("Press <Enter> to Continue...")
}

cat("\n")

## File and directory exploration
cat("Processing tray label files...\n")

sapply(t.dirs, function(t.dir) {
    trayLabelFiles <- list.files(path = t.dir, recursive = FALSE, pattern = "*TrayLabelLPF.lpf", full.names = FALSE)

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

    t.processTrayLabelFile(t.dir, outputDir, trayLabelFiles[1])
})

## Booking list file
cat("\nCreating booking list file...\n\n")

t.bookingFile <- paste0(t.outputDir, "\\", Sys.Date(), " Booking File.txt")

if (file.exists(t.bookingFile)) {
    cat("** WARNING: Existing file will be overwritten.\n\n")
    file.remove(t.bookingFile)
}

writeLines(t.dirs, con = t.bookingFile)
