
## Process tray label file
t.processTrayLabelFile <- function(t.dir, t.inputDir, t.outputDir, t.file) {
    # Read file
    t.data <- readLines(paste0(t.inputDir, "\\", t.file), warn = FALSE)

    t.labelHeader <- (t.data[match("#Label Plan Header", t.data) + 1] %>% strsplit(","))
    t.bookings$Booking[t.bookings$Region == t.dir] <<- t.labelHeader[[1]][5]

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
    t.ntrays <- 0

    invisible(sapply(1:nrow(t.combined), function(i) {
        t.bookings$Items[t.bookings$Region == t.dir] <<- sum(t.combined$Tray_Qty)
        t.bookings$Bundles[t.bookings$Region == t.dir] <<- ceiling(sum(t.combined$Tray_Qty) / t.bundleSize)

        quantity <- t.combined$Tray_Qty[i]

        if (t.brickStack && quantity >= 2000) {
            # Brick stack
            cat(paste0("** BRICK STACK required for ",
                       t.dir, " - Sort Plan ",
                       t.combined$Sort_Plan[i]), "- Quantity", quantity, "\n")

            t.bookings$Brick[t.bookings$Region == t.dir] <<- t.bookings$Brick[t.bookings$Region == t.dir] + 1
            return()
        }

        fullTrays <- floor(quantity / t.maxTraySize)
        partialTrayQuantity <- quantity %% t.maxTraySize
        t.ntrays <<- t.ntrays + fullTrays + (partialTrayQuantity / partialTrayQuantity)

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
    t.bookings$Trays[t.bookings$Region == t.dir] <<- t.ntrays
    t.newframe <- arrange(t.newframe, desc(Tray_Qty), Sort_Plan)

    invisible(sapply(1:nrow(t.newframe), FUN = function(n) {
        t.newframe$Additional_Text[n] <<- paste0("T", n, "/", nrow(t.newframe), " A", t.newframe$Tray_Qty[n])
    }))

    t.newframe <- select(t.newframe, -Tray_Qty)

    con <- textConnection("t.newcsv", "w")
    write.table(t.newframe, con, quote = FALSE, row.names = FALSE, col.names = FALSE, sep = ",")
    close(con)

    t.newdata <- c(t.data[1:(t.start + 1)], t.newcsv, t.data[t.end])
    writeLines(t.newdata, con = paste0(t.outputDir, "\\", gsub("TrayLabelLPF.lpf$", "", t.file), gsub(" ", "_", t.dir), ".lpf"))
}
