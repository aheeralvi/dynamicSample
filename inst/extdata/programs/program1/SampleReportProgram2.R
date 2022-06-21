library(reporter)
library(magrittr)

dat <- subset(iris, Species == "{InputSelect}")

tbl <- create_table(dat, borders = c("top", "bottom")) %>%
  titles("Listing 1.0", "Iris Data Listing", bold = TRUE) # %>%
   # footnotes("* Motor Trend, 1974")

# Create the report
rpt <- create_report("./report/example2.html",
                     orientation = "portrait",
                     font = "Arial",
                     output_type = "HTML") %>%
       add_content(tbl)


# Write the report
res <- write_report(rpt)


pth <- tempfile(fileext = ".html")
dir <- dirname(pth)

if (!dir.exists(dir))
  dir.create(dir)

file.copy(res$modified_path, pth)

viewer <- getOption("viewer")

if (!is.null(viewer))
  viewer(pth)




