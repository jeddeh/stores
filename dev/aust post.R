## Aust Post - Process single .lpf file

library(dplyr)

## Debug only
t.dir <- "C:/Users/Rob/Desktop/Stores/dev/Telstra Aust Post/Telstra Business Centre Wagga Wagga (Riverina)"
t.inputDir <- "C:/Users/Rob/Desktop/Stores/dev/Telstra Aust Post/Telstra Business Centre Wagga Wagga (Riverina)"
t.outputDir <- "C/Users/Rob/Desktop/Stores/dev/Aust Post Output/"
t.file <- "U0496219_TrayLabelLPF.lpf"
## End debug

cat("\014")

## Declarations
# pdftk is an appplication with the ability to merge pdf files. It is accessed through system commands.
t.pdftkApp <- "C:\\Users\\Rob\\Desktop\\Stores\\PDFTKBuilderPortable\\App\\pdftkbuilder\\pdftk.exe"

t.maxLargeTrayWeight <- 16 * 1000
t.maxSmallTrayWeight <- 8 * 1000

# t.dir <- choose.dir(default = "C:/Users/Rob/Desktop/",
#                                   caption = "Select Aust Post folder")

t.path <- "C:/Users/Rob/Desktop/Stores/dev/Telstra Aust Post"

t.weight <- readline("Enter weight per catalogue (g): ")
t.bundleSize <- readline("Enter bundle size: ")

t.maxTraySize <- 400

t.dirs <- list.dirs(path=t.path, recursive = FALSE)
t.outputDir <- "C:\\Users\\Rob\\Desktop\\Stores\\dev\\Telstra Output Files"

## Process file
t.processfile <- function(t.inputDir, outputDir, t.file) {
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
    # t.frame$Tray_Number <- as.numeric(gsub("T|/.*$", "", t.frame$Additional_Text))
    # t.frame$Tray_Total <- as.numeric(gsub("^.*/| A.*$", "", t.frame$Additional_Text))

    # Regex explanation
    # ^ - starts with
    # . - any character except new line
    # *A - any one of more of previous expression ending in letter A
    # ... then delete this part of the string
    t.frame$Tray_Qty <- as.numeric(gsub("^.*A","", t.frame$Additional_Text))

    # Add a column to t.frame indexing the tray groups
    # t.ngroup <- 0
    #
    # for (i in 1:nrow(t.frame)) {
    #     if (t.frame$Tray_Number[i] == 1) {
    #         t.ngroup <- t.ngroup + 1
    #     }
    #
    #     t.frame$Tray_Group <- t.ngroup
    # }

    t.frame

    t.combined <- aggregate(Tray_Qty ~ Service + Sort_Plan_Type + Sort_Plan + Destination_Ind + Mail_Size + Label_Qty + Date,
                            data = t.frame, FUN = sum)


    t.combined

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

    # # Add a Tray_Group index - not required
    # invisible(sapply(1:nrow(t.newframe), function(i) {
    #     t.newframe$Tray_Total <<- nrow(t.newframe[t.newframe$Tray_Group == t.newframe$Tray_Group[i] ,])
    # }))

    t.newframe <- arrange(t.newframe, desc(Tray_Qty), Sort_Plan)

    invisible(sapply(1:nrow(t.newframe), FUN = function(n) {
        t.newframe$Additional_Text[n] <<- paste0("T", n, "/", nrow(t.newframe), " A", t.newframe$Tray_Qty[n])
    }))

    t.newframe <- select(t.newframe, -Tray_Qty)
    t.newframe

    con <- textConnection("t.newcsv", "w")
    write.table(t.newframe, con, quote = FALSE, row.names = FALSE, col.names = FALSE)
    close(con)

    t.newcsv

    t.newdata <- c(t.data[1:t.start + 1], t.newcsv, t.data[t.end])
    print(t.newdata, sep = "\n")

    # if (interactive()) {
    #     t.dir <- choose.dir(default = "C:/Users/Rob/Desktop/", caption = "Select output folder")
    # }

    # cat("\014")

    writeLines(t.newdata, con = paste0(t.outputDir, "\\", t.file))
}

## Process PDF files
t.processPdfFile <- function() {
    pdfOutputFiles <- NULL

    sapply(t.dirs, function(t.dir) {
        bookingConfirmationFiles <- list.files(path = t.dir, recursive = FALSE, pattern = "*BookingConfirmationAdvice.pdf", full.names = TRUE)
        mailingStatementFiles <- list.files(path = t.dir, recursive = FALSE, pattern = "*MailingStatement.pdf", full.names = TRUE)

        ## File validation
        if (length(bookingConfirmationFiles) == 0) {
            cat(paste0("\n** WARNING: There is no booking confirmation advice file in directory ", t.dir))
            readline("Press <Enter> to Continue...")
        }

        if (length(bookingConfirmationFiles) > 1) {
            cat(paste0("\n** WARNING: Multiple booking confirmation advice files exist in directory ", t.dir))
            cat("No booking confirmation advice for this directory will be processed.")
            readline("Press <Enter> to Continue...")
        }

        if (length(mailingStatementFiles) == 0) {
            cat(paste0("\n** WARNING: There is no mailing statement file in directory ", t.dir))
            readline("Press <Enter> to Continue...")
        }

        if (length(mailingStatementFiles) > 1) {
            cat(paste0("\n** WARNING: Multiple mailing statement advice files exist in directory ", t.dir))
            cat("No mailing statement for this directory will be printed.")
            readline("Press <Enter> to Continue...")
        }

        pdfOutputFiles <<- c(pdfOutputFiles, bookingConfirmationFiles[1], mailingStatementFiles[1])
    })
    pdfOutputFiles
}

cat("\nGenerating pdf file...\n")
t.pdfOutputFiles <- NULL
t.pdfOutputFiles <- t.processPdfFile() %>% na.omit()
t.pdfOutputFiles <- paste0("\"", t.pdfOutputFiles, "\"")
t.pdfOutputFile <- paste0("\"", t.outputDir, "\\", Sys.Date(), " Telstra Aust Post.pdf", "\"")

t.mergeCommand <- paste(t.pdftkApp,
             paste(t.pdfOutputFiles, collapse = " "),
             "cat output",
             t.pdfOutputFile)

t.result <- system(t.mergeCommand, intern = TRUE)
cat(paste0("\nResult:\n", t.result))

## File and directory exploration
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

    t.processfile(t.dir, outputDir, trayLabelFiles[1])
})
