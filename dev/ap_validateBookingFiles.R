## Validate booking files

t.validateBookingFiles <- function(bookings) {
    cat("\nValidating booking files...\n")

    sapply(1:nrow(bookings), function(n) {
        bookingDir <- bookings$dir[n]

        trayLabelFiles <- list.files(path = bookingDir,
                                     recursive = FALSE,
                                     pattern = "*TrayLabelLPF.lpf",
                                     full.names = TRUE)

        if (length(trayLabelFiles) == 0) {
            cat(paste0("\n** WARNING: There is no tray label file in directory ", bookingDir))
        }
        else if (length(trayLabelFiles) > 1) {
            cat(paste0("\n\n** WARNING: Multiple tray label files exist in directory ", bookingDir))
            cat("\nNo tray labels for this directory will be processed.\n")
        }
        else {
            bookings$trayLabelFile[n] <<- trayLabelFiles[1]
        }

        bookingConfirmationFiles <- list.files(path = bookingDir,
                                               recursive = FALSE,
                                               pattern = "*BookingConfirmationAdvice.pdf",
                                               full.names = TRUE)

        if (length(bookingConfirmationFiles) == 0) {
            validBookingConfirmation <- FALSE
            cat(paste0("\n** WARNING: There is no booking confirmation advice file in directory ", bookingDir))
        }
        else if (length(bookingConfirmationFiles) > 1) {
            validBookingConfirmation <- FALSE
            cat(paste0("\n\n** WARNING: Multiple booking confirmation advice files exist in directory ", bookingDir))
            cat("\nNo booking confirmation advice for this directory will be processed.\n")
        }
        else {
            bookings$bookingConfirmationFile[n] <<- bookingConfirmationFiles[1]
        }

        mailingStatementFiles <- list.files(path = bookingDir,
                                            recursive = FALSE,
                                            pattern = "*MailingStatement.pdf",
                                            full.names = TRUE)

        if (length(mailingStatementFiles) == 0) {
            validMailingStatement = FALSE
            cat(paste0("\n** WARNING: There is no mailing statement file in directory ", bookingDir))
        }
        else if (length(mailingStatementFiles) > 1) {
            validMailingStatement = FALSE
            cat(paste0("\n\n** WARNING: Multiple mailing statement advice files exist in directory ", bookingDir))
            cat("\nNo mailing statement for this directory will be printed.\n")
        }
        else {
            bookings$mailingStatementFile[n] <<- mailingStatementFiles[1]
        }
    })

    cat("\n\n")
    readline("Press <Enter> to Continue...")

    bookings
}
