###########################################################################
## Author: Youtao Lu <luyoutao@sas.upenn.edu>
## 
## Copyright (c) 2021-2022, Youtao Lu and Junhyong Kim, Department of Biology, University of Pennsylvania
## Copyright (c) 2021-2022, Jai-Yoon Sul, Department of Systems Pharmacology and Translational Therapeutics, University of Pennsylvania
## 
## This program is free software; you can redistribute it and/or modify it under the terms of the the Artistic License (2.0). You may obtain a copy of the full license at: 
## https://opensource.org/license/artistic-2-0/
###########################################################################
if (!exists("Tools") || is.environment(Tools)) Tools <- new.env(parent = emptyenv())
local({
    .VERSION = "0.6"

    list_to_df <- function(list, fill = "") {
        n <- max(sapply(list, length))
        as.data.frame(sapply(list, function(l) c(l, rep(fill, n - length(l))), simplify = TRUE), 
            stringsAsFactors = FALSE,
            check.names = FALSE
        )
    }

    omit_na <- function(x) {
        x[!is.na(x)]
    }

    omit_space <- function(x) {
        x[x != ""] 
    }

    dfrapply <- function (object, f) {
        if (inherits(object, "data.frame")) { f(object) }
        else { lapply(object, dfrapply, f) }
    }

    write_xlsx <- function(dflist, file, sheetnames = NULL, keepNA = FALSE, na.string = NULL, row.names = TRUE, col.names = TRUE, overwrite = TRUE) {
        suppressPackageStartupMessages(
            if (!requireNamespace("openxlsx")) {
                stop("Package 'openxlsx' not found! Please install it first!")
        })
        wb <- openxlsx::createWorkbook()
        openxlsx::modifyBaseFont(wb, fontSize = 11, fontColour = "#000000", fontName = "Arial")
        k <- 0
        if (is.null(sheetnames)) {
            if (is.null(names(dflist))) { stop("No names in data.frame list nor sheetnames provided!") }
            sheetnames <- names(dflist)
        }
        for (x in sheetnames) {
            k <- k + 1
            openxlsx::addWorksheet(wb, sheetName = substr(x, 1, 31))
            df <- dflist[[k]]
            if (!is.data.frame(df)) { df <- as.data.frame(df) }
            openxlsx::writeDataTable(wb, sheet = k, x = df, 
                                     keepNA = keepNA, na.string = na.string,
                                     withFilter = FALSE, 
                                     headerStyle = NULL, 
                                     tableStyle = "none", 
                                     bandedRows = FALSE, bandedCols = FALSE, 
                                     rowNames = row.names, colNames = col.names)
        }
        openxlsx::saveWorkbook(wb, file = file, overwrite = overwrite)
    }


    read_xlsx <- function(file, sheetnames, ...) {
        suppressPackageStartupMessages(
            if (!requireNamespace("openxlsx")) {
                stop("Package 'openxlsx' not found! Please install it first!")
        })
        dflists <- lapply(sheetnames, function(sheet) {
            openxlsx::read.xlsx(xlsxFile = file, sheet = sheet, ...)
        })
        names(dflists) <- sheetnames
        dflists
    }

    for (.obj in ls()) {
        assign(.obj, get(.obj), envir = Tools)
    }
})
