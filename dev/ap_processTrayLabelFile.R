## Process tray label files

t.processTrayLabelFiles <- function(bookings) {
    cat("\nProcessing tray label files...\n")

    sapply(1:nrow(bookings), function(n) {

        if (is.na(bookings$trayLabelFile[n])) {
            return()
        }

        # Read file
        pt.data <- readLines(bookings$trayLabelFile[n], warn = FALSE)

        pt.labelHeader <- (pt.data[match("#Label Plan Header", pt.data) + 1] %>% strsplit(","))
        bookings$booking[n] <<- pt.labelHeader[[1]][5]

        pt.start <- match("#Label Details", pt.data)
        pt.end <- match("#End Of File", pt.data)
        pt.headers <- strsplit(gsub("#", "", pt.data[pt.start + 1]), ",")[[1]]

        pt.frame <- read.table(text = pt.data,
                              header = FALSE,
                              sep = ",",
                              skip = pt.start,
                              stringsAsFactors = FALSE,
                              col.names = pt.headers)

        # Add columns to pt.frame splitting Additional_Text column data

        # Regex explanation
        # ^ - starts with
        # . - any character except new line
        # *A - any one of more of previous expression ending in letter A
        # ... then delete this part of the string
        pt.frame$Tray_Qty <- as.numeric(gsub("^.*A","", pt.frame$Additional_Text))

        pt.combined <- aggregate(Tray_Qty ~ Service + Sort_Plan_Type + Sort_Plan + Destination_Ind + Mail_Size + Label_Qty + Date,
                                data = pt.frame, FUN = sum)

        # Make pt.newframe which will hold the final csv data
        pt.newframe <- data.frame(row.names = names(pt.combined))
        pt.ntrays <- 0

        invisible(sapply(1:nrow(pt.combined), function(i) {
            bookings$items[n] <<- sum(pt.combined$Tray_Qty)
            bookings$bundles[n] <<- ceiling(sum(pt.combined$Tray_Qty) / t.bundleSize)

            quantity <- pt.combined$Tray_Qty[i]

#             if (pt.brickStack && quantity >= 2000) {
#                 # Brick stack
#                 cat(paste0("** BRICK STACK required for ",
#                            pt.dir, " - Sort Plan ",
#                            pt.combined$Sort_Plan[i]), "- Quantity", quantity, "\n")
#
#                 bookings$brick[bookings$region == pt.dir] <<- bookings$brick[bookings$region == pt.dir] + 1
#                 return()
#             }

            fullTrays <- floor(quantity / t.maxTraySize)
            partialTrayQuantity <- quantity %% t.maxTraySize
            pt.ntrays <<- pt.ntrays + fullTrays + (partialTrayQuantity / partialTrayQuantity)

            fullTrayRow <- pt.combined[i, ]
            fullTrayRow$Tray_Qty <- t.maxTraySize

            if (fullTrays > 0) {
                sapply(1:fullTrays, function(n) {
                    pt.newframe <<- rbind(pt.newframe, fullTrayRow)
                })
            }

            if (partialTrayQuantity > 0) {
                partialRow <- pt.combined[i, ]
                partialRow$Tray_Qty <- partialTrayQuantity
                pt.newframe <<- rbind(pt.newframe, partialRow)
            }
        }))

        bookings$trays[n] <<- pt.ntrays
        pt.newframe <- arrange(pt.newframe, desc(Tray_Qty), Sort_Plan)

        invisible(sapply(1:nrow(pt.newframe), FUN = function(n) {
            pt.newframe$Additional_Text[n] <<- paste0("T", n, "/", nrow(pt.newframe), " A", pt.newframe$Tray_Qty[n])
        }))

        pt.newframe <- select(pt.newframe, -Tray_Qty)

        con <- textConnection("pt.newcsv", "w")
        write.table(pt.newframe, con, quote = FALSE, row.names = FALSE, col.names = FALSE, sep = ",")
        close(con)

        pt.newdata <- c(pt.data[1:(pt.start + 1)], pt.newcsv, pt.data[pt.end])
        trayLabelFile <- paste0(t.outputDir, "/", bookings$booking[n], "_", bookings$region[n], ".lpf")
        writeLines(pt.newdata, con = trayLabelFile)

        bookings$trayLabelOutputFile[n] <<- trayLabelFile
    })

    bookings
}

