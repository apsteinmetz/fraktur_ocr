library(paws)
library(httpuv)

extract <- paws::textract()


myPdf <- file("ohio war dead ww1_6.png", "rb")
myBinVec <- readBin(myPdf, raw(), n=100000000) #N needs to be larger than your file.
myBase64 <- rawToBase64(myBinVec)

myresult <- extract$analyze_document(Document = list(S3Object = list(Bucket = "outsiderdata",
                                                                   Name = "ohio war dead WW1.pdf")),
                                   FeatureTypes = list("TABLES"))


# Get all children for a given block.
get_children <- function(block, data) {
   if (length(block$Relationships) == 0) {
      return(list())
   }
   idx <- which(sapply(block$Relationships, function(x) x$Type) == "CHILD")
   if (!idx) {
      return(list())
   }
   child_ids <- block$Relationships[[idx]]$Ids
   result <- data[child_ids]
   return(result)
}

get_tables <- function(analysis) {
   tables <- list()
   blocks <- analysis$Blocks
   names(blocks) <- sapply(blocks, function(x) x$Id)

   for (block in blocks) {

      if (block$BlockType == "TABLE") {
         cells <- get_children(block, blocks)
         rows <- max(sapply(cells, function(x) x$RowIndex))
         cols <- max(sapply(cells, function(x) x$ColumnIndex))
         table <- matrix(nrow = rows, ncol = cols)

         # 1. Go through a table's cells one-by-one
         for (cell in cells) {

            # 2. Get the cell's contents
            words <- get_children(cell, blocks)
            text <- paste(sapply(words, function(x) x$Text), collapse = " ")

            # 3. Insert the cell contents into the matrix
            row <- cell$RowIndex
            col <- cell$ColumnIndex
            table[row, col] <- text
         }
         tables <- c(tables, list(table))
      }
   }
   return(tables)
}

#ASYNCH Operation
analyze_document <- function(bucket, .file) {

   # Start analyzing the PDF.
   resp <- extract$start_document_analysis(
      DocumentLocation = list(
         S3Object = list(Bucket = bucket, Name = .file)
      ),
      FeatureTypes = "TABLES"
   )

   # Check that the analysis is done and get the result.
   count <- 0
   while (count < 1000 && (!exists("result") || result$JobStatus == "IN_PROGRESS")) {
      Sys.sleep(1)
      result <- extract$get_document_analysis(
         JobId = resp$JobId
      )
      # If the result has multiple parts, get the remaining parts.
      next_token <- result$NextToken
      while (length(next_token) > 0) {
         next_result <- extract$get_document_analysis(
            JobId = resp$JobId,
            NextToken = next_token
         )
         result$Blocks <- c(result$Blocks, next_result$Blocks)
         next_token <- next_result$NextToken
      }
      count <- count + 1
   }

   return(result)
}

myresult <- analyze_document("outsiderdata","ohio war dead WW1.pdf")

save(tables,file="tables.rdata")

mytable <-  map(tables,as.tibble) %>% bind_rows()

# entries without serial number
t1 <- mytable %>%
   filter(is.na(V5)) %>%
   select(V1,V5,V2,V3,V4)
names(t1) <-c("name","serial_num","rank","unit","death_date")

t2 <- mytable %>%
   filter(!is.na(V5)) %>%
   select(-V6)
names(t2) <-c("name","serial_num","rank","unit","death_date")

ohio_ww1_deaths <- bind_rows(t1,t2) %>%
   filter(name != "NAME") %>%
   filter(name != "") %>%
   mutate(serial_num = ifelse(serial_num=="",NA,serial_num)) %>%
   arrange(name)

ohio_ww1_deaths
