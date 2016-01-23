## Generate ULD pdf
t.generateUldPdf <- function(uldRmdFile, uldCssFile, lodgementDate, startDate, endDate, nUld, totalUld, booking, trays, output) {
    uldHtmlFile <- paste0(t.tempDir, "/", booking, ".html")
    output <- paste0("\"", output, "\"")

    options(markdown.HTML.stylesheet = uldCssFile)
    result <- knit2html(input = uldRmdFile, output = uldHtmlFile, quiet = TRUE)

    convertToPdfCommand <- paste0("\"C:\\Program Files (x86)\\wkhtmltopdf\\bin\\wkhtmltopdf\" -q ", uldHtmlFile, " ", output)
    result <- system(convertToPdfCommand, invisible = TRUE, ignore.stdout = TRUE)

    # unlink(uldHtmlFile)
    result
}

## Process PDF files
t.processPdfFiles <- function(bookings) {
    cat("\nProcessing pdf files...\n")

    sapply(1:nrow(bookings), function(n) {
        if (is.na(bookings$booking[n])) {
            return()
        }

        uldFile <- file.path(t.tempDir, paste0(bookings$booking[n], ".pdf"))

        result <- t.generateUldPdf(uldRmdFile = t.uldRmdFile,
                         uldCssFile = t.uldCssFile,
                         lodgementDate = as.Date("02/12/2015"),
                         startDate = as.Date("02/12/2015"),
                         endDate = as.Date("02/12/2015"),
                         nUld = 1,
                         totalUld = 1,
                         booking = bookings$booking[n],
                         trays = 44,
                         output = uldFile)

        bookings$uldFile[n] <<- uldFile
    })

    pdfOutputFiles <- NULL

    sapply(1:nrow(bookings), function(n) {
        if (!is.na(bookings$uldFile[n])) {
            pdfOutputFiles <<- c(pdfOutputFiles, bookings$uldFile[n])
        }

        if (!is.na(bookings$bookingConfirmationFile[n])) {
            pdfOutputFiles <<- c(pdfOutputFiles, bookings$bookingConfirmationFile[n])
        }

        if (!is.na(bookings$mailingStatementFile[n])) {
            pdfOutputFiles <<- c(pdfOutputFiles, bookings$mailingStatementFile[n])
        }
    })

    t.mergePdfFiles(pdfOutputFiles)

    bookings
}

## Merge PDF files
t.mergePdfFiles <- function(pdfOutputFiles) {
    cat("\nMerging pdf files...\n\n")
    pdfOutputFiles <- paste0("\"", pdfOutputFiles, "\"")

    pdfOutputFile <- paste0(t.outputDir, "/", Sys.Date(), " Telstra Aust Post.pdf")
    pdfOutputFile <- paste0("\"", pdfOutputFile, "\"")

    mergeCommand <- paste("pdftk ",
                            paste(pdfOutputFiles, collapse = " "),
                            "cat output",
                            pdfOutputFile)

    result <- system(mergeCommand, intern = TRUE)
}
