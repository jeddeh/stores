## Generate ULD pdf
t.generateUldPdf <- function(uldRmdFile, uldCssFile, lodgementDate, startDate, endDate, nUld, totalUld, booking, trays, output) {
    uldHtmlFile <- paste0(t.processDir, "/", booking, ".html")
    output <- paste0("\"", output, "\"")

    options(markdown.HTML.stylesheet = uldCssFile)
    result <- rmarkdown::render(input = uldRmdFile, output_file = uldHtmlFile, clean = TRUE, quiet = TRUE)

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

        uldFile <- file.path(t.processDir, paste0(bookings$booking[n], ".pdf"))

        result <- t.generateUldPdf(uldRmdFile = t.uldRmdFile,
                         uldCssFile = t.uldCssFile,
                         lodgementDate = t.lodgementDate,
                         startDate = t.startDate,
                         endDate = t.endDate,
                         nUld = 1,
                         totalUld = 1,
                         booking = bookings$booking[n],
                         trays = bookings$trays[n],
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

## Debug only
testPdf <- function() {
    uldRmdFile <- "C:/Users/Rob/Desktop/Stores/dev/uld files/uld.Rmd"
    uldCssFile <- "C:/Users/Rob/Desktop/Stores/dev/uld files/uld.css"
    lodgementDate <- 4
    startDate <- 3
    endDate <- 2
    nUld <- 1
    totalUld <- 9
    booking <- 8
    trays <- 7
    output <- 6
    outputhtml <- "C:/Users/Rob/Desktop/THISFILE.html"
    outputpdf <- "C:/Users/Rob/Desktop/THISFILE.pdf"

    unlink(outputhtml)
    unlink(outputpdf)

    result <- rmarkdown::render(input = uldRmdFile, output_file = outputhtml, output_format = "html_document", clean = TRUE, quiet = TRUE)

    convertToPdfCommand <- paste0("\"C:\\Program Files (x86)\\wkhtmltopdf\\bin\\wkhtmltopdf\" -q ", outputhtml, " ", outputpdf)
    result <- system(convertToPdfCommand, invisible = TRUE, ignore.stdout = TRUE)

    system(paste("open", outputpdf))

    }
