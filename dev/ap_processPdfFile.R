## Generate ULD pdf
t.generateUldPdf <- function(uldRmdFile, uldCssFile, lodgementDate, startDate, endDate, nUld, totalUld, booking, trays, output) {
    uldHtmlFile <- tempfile("uld", fileext = ".html")

    output <- paste0("\"", output, "\"")

    options(markdown.HTML.stylesheet = uldCssFile)
    knit2html(input = uldRmdFile, output = uldHtmlFile)

    t.result <- system(paste0("\"C:\\Program Files (x86)\\wkhtmltopdf\\bin\\wkhtmltopdf\" ", uldHtmlFile, " ", output))
    unlink(uldHtmlFile)
}

## Process PDF files
t.processPdfFile <- function(bookings) {
    pdfOutputFiles <- NULL

    tempDir <- tempdir()

    sapply(1:nrow(bookings), function(n) {
        t.generateUldPdf(uldRmdFile = t.uldRmdFile,
                         uldCssFile = t.uldCssFile,
                         lodgementDate = as.Date("02/12/2015"),
                         startDate = as.Date("02/12/2015"),
                         endDate = as.Date("02/12/2015"),
                         nUld = 1,
                         totalUld = 1,
                         booking = bookings$Booking[n],
                         trays = 44,
                         output = file.path(tempDir, paste0(bookings$Booking[n], ".pdf")))
    })

    sapply(t.dirs, function(t.dir) {
        t.dirPath <- file.path(t.inputDir, t.dir)

        uldFiles <- list.files(path = tempDir,
                               recursive = FALSE,
                               pattern = "*.pdf",
                               full.names = TRUE)

        bookingConfirmationFiles <- list.files(path = t.dirPath,
                                               recursive = FALSE,
                                               pattern = "*BookingConfirmationAdvice.pdf",
                                               full.names = TRUE)

        mailingStatementFiles <- list.files(path = t.dirPath,
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


    t.mergePdfFiles(pdfOutputFiles)

    unlink(tempDir)
}

## Merge PDF files
t.mergePdfFiles <- function(pdfOutputFiles) {
    cat("\nMerging pdf files...\n\n")
    pdfOutputFiles <- paste0("\"", t.pdfOutputFiles, "\"")
    pdfOutputFile <- paste0(t.outputDir, "\\", Sys.Date(), " Telstra Aust Post.pdf")

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

    # if (length(t.result) != 0) {
    #     cat(paste(t.result, collapse = "\n"))
    #     readline("Press <Enter> to Continue...")
    # }

    cat("\n")
}
