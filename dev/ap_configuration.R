## Configuration script for Telstra Aust Post bookings

# setwd(dirname(sys.frame(1)$ofile)) # setwd to directory of current R script

# PC: '0' - Violet's PC, '1' - Receiving PC, '2' - Home PC
t.pc <- '2'

t.baseDir <- switch(t.pc,
                    '0' = "C:/Users/saadv/Desktop/Stores",
                    '1' = "C:/Users/grant/Documents/Stores",
                    '2' = "C:/Users/Rob/Desktop/Stores"
)

# ULD files are used to generate ULD tags.
t.uldRmdFile <- file.path(t.baseDir, "dev/uld files/uld.Rmd")
t.uldCssFile <- file.path(t.baseDir, "dev/uld files/uld.css")

# Pack Sheet files are used to generate Packing Sheets.
t.packSheetRmdFile <- file.path(t.baseDir, "dev/packing sheet files/uld.Rmd")
t.packSheetCssFile <- file.path(t.baseDir, "dev/packing sheet files/uld.css")

t.defaultInputDir <- switch(t.pc,
                        '0' = "C:/Users/saadv/Desktop/Telstra Aust Post.zip",
                        '1' = "C:/Users/grant/Documents/Stores/dev/data/Telstra Aust Post.zip",
                        '2' = "C:/Users/Rob/Desktop/Stores/dev/data/Telstra Aust Post.zip"
)

t.defaultOutputDir <- switch(t.pc,
                            '0' = "C:/Users/saadv/Desktop/Telstra Aust Post",
                            '1' = "C:/Users/grant/Documents/Stores/dev/data/Telstra Output Files",
                            '2' = "C:/Users/Rob/Desktop/Stores/dev/data/Telstra Output Files"
)

# Create temp dir for processsing
t.tempDir <- file.path(t.baseDir, "temp")

closeAllConnections()
unlink(t.tempDir, recursive = TRUE, force = TRUE)
dir.create(t.tempDir)

t.processDir <- file.path(t.tempDir, "process")
dir.create(t.processDir)
t.inputDir <- file.path(t.tempDir, "input")
dir.create(t.inputDir)

