####----Install and load packages----####

packages <- c("shiny","shinythemes","shinyjqui","shinycssloaders","dplyr","DT","readr","ggplot2")

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
invisible(lapply(packages, library, character.only = TRUE))
#bioconductor packages
bioCpacks <- c("clusterProfiler","enrichplot")
installed_packages_BIOC <- bioCpacks %in% rownames(installed.packages())
if (any(installed_packages_BIOC == FALSE)) {
  BiocManager::install(bioCpacks[!installed_packages_BIOC], ask = F)
}
invisible(lapply(bioCpacks, library, character.only = TRUE))

####----File Input----####

GeneSet_File <- "GeneSets.zip"


####----Read In and Sort File----####

GeneSet <- as.data.frame(read_delim(GeneSet_File, delim = '\t', col_names = T))
msigdb <- GeneSet[which(GeneSet$GeneSet == "msigdb"),]
hallmark <- msigdb[which(grepl("HALLMARK",msigdb$term)),]
lincsu <- GeneSet[which(GeneSet$GeneSet == "lincsu"),]
lincsd <- GeneSet[which(GeneSet$GeneSet == "lincsd"),]
cellm <- GeneSet[which(GeneSet$GeneSet == "cellm"),]

comp <- rbind(msigdb,lincsu,lincsd,cellm,hallmark)



#increase file upload size
options(shiny.maxRequestSize=50*1024^2)


ui <- 
  navbarPage("{ User Ranked GSEA }",
             
             ####----Enrichment Table----####
             
             tabPanel("Enrichment Table",
                      fluidPage(
                        title = "Enrichment Table",
                        sidebarPanel(
                          width = 3,
                          fluidRow(
                            column(9,
                                   fileInput("UserGeneList","Upload Ranked Gene List")
                            ),
                            column(3,
                                   checkboxInput("UserGeneListheaderCheck","Header",value = T)
                            )
                          ),
                          numericInput("gseaPval","GSEA Pvalue Cutoff",value = 1),
                          radioButtons("GeneSetChosen","Choose Gene Set:",
                                       choices = c("MSigDB - Hallmark","MSigDB - All","LINCS L1000 Up-Regulated","LINCS L1000 Down-Regulated","Cell Marker","All Gene Sets","User Upload"),
                                       selected = "MSigDB - Hallmark", inline = F),
                          fluidRow(
                            column(9,
                                   uiOutput("rendUserGSupload")
                                   ),
                            column(3,
                                   uiOutput("rendUserGSheaderCheck")
                                   )
                            )
                          ),
                        mainPanel(
                          p("Please note this may take excess time depending on the number of gene sets being analyzed."),
                          uiOutput("rendEnrichTable"),
                          uiOutput("renddnldEnrichTable"),
                          #withSpinner(DT::dataTableOutput("EnrichTable", width = "100%"), type = 6),
                          #downloadButton("dnldEnrichTable","Download Enriched Signatures Table")
                          )
                        )
                      ),
             tabPanel("Enrichment Plot",
                      fluidPage(
                        title = "Enrichment Plot",
                        sidebarPanel(
                          h4("Select Gene Set:"),
                          div(DT::dataTableOutput("GeneSetTable"), style = "font-size:10px; height:700px; overflow-X: scroll")
                        ),
                        mainPanel(
                          h3("GSEA Enrichment Plot"),
                          verbatimTextOutput("NESandPval"),
                          withSpinner(plotOutput("enrichplot", width = "500px", height = "450px"), type = 6),
                          fluidRow(
                            downloadButton("dnldPlotSVG_gsea","Download as SVG"),
                            downloadButton("dnldPlotPDF_gsea","Download as PDF")
                          ),
                          h3("Leading Edge Genes"),
                          downloadButton("LEGdownload", "Download Leading Edge Gene Table"),
                          div(DT::dataTableOutput("LeadingEdgeGenes"), style = "font-size:12px; height:500px; width:400px")
                        )
                      ))
             )

server <- function(input, output, session) {
  
  output$rendEnrichTable <- renderUI({
    
    req(input$UserGeneList)
    withSpinner(DT::dataTableOutput("EnrichTable", width = "100%"), type = 6)
    
  })
  
  output$renddnldEnrichTable <- renderUI({
    
    req(input$UserGeneList)
    downloadButton("dnldEnrichTable","Download Enriched Signatures Table")
    
  })
  
  output$rendUserGSupload <- renderUI({
    
    if (is.null(input$GeneSetChosen) == FALSE) {
      
      if (input$GeneSetChosen == "User Upload") {
        
        fileInput("UserGeneSet","Upload Gene Set File")
        
      }
      
    }
    
  })
  
  output$rendUserGSheaderCheck <- renderUI({
    
    if (is.null(input$GeneSetChosen) == FALSE) {
      
      if (input$GeneSetChosen == "User Upload") {
        
        checkboxInput("UserGSheaderCheck","Header",value = T)
        
      }
      
    }
    
  })
  
  ## User Upload gene set reactive
  user_gs <- reactive({
    
    header_check <- input$UserGSheaderCheck
    gs.u <- input$UserGeneSet
    ext <- tools::file_ext(gs.u$datapath)
    req(gs.u)
    validate(need(ext == c("gmt","tsv","txt"), "Please upload .gmt, .tsv, or .txt file"))
    
    # If user provides GMT file
    if (ext == "gmt") {
      gmt <- read.gmt(gs.u$datapath)
      colnames(gmt) <- c("term","gene")
      gs_u <- gmt
    }
    
    # If user provides tab-delim file
    else if (ext == "txt" | ext == "tsv") {
      
      gmt <- as.data.frame(read_delim(gs.u$datapath, delim = '\t', col_names = header_check))
      
      if (ncol(gmt) == 1) {
        paths <- as.vector(gmt[,1])
        paths <- gsub("[[:punct:]]",".",paths)
        gs_u <- comp[which(comp$term %in% paths),]
      }
      else if (ncol(gmt) == 2) {
        colnames(gmt) <- c("term","gene")
        gs_u <- gmt
      }
      else if (ncol(gmt) > 2) {
        paths <- as.vector(gmt[,1])
        paths <- gsub("[[:punct:]]",".",paths)
        gs_u <- comp[which(comp$term %in% paths),]
      }
      
    }
    
    gs_u
    
  })
  
  #gmt reactive based on user choice for GSEA
  gmt_react <- reactive({
    
    if (input$GeneSetChosen == "MSigDB - All") {
      
      gmt <- msigdb
      
    }
    else if (input$GeneSetChosen == "LINCS L1000 Up-Regulated") {
      
      gmt <- lincsu
      
    }
    else if (input$GeneSetChosen == "LINCS L1000 Down-Regulated") {
      
      gmt <- lincsd
      
    }
    else if (input$GeneSetChosen == "Cell Marker") {
      
      gmt <- cellm
      
    }
    else if (input$GeneSetChosen == "All Gene Sets") {
      
      gmt <- comp
      
    }
    else if (input$GeneSetChosen == "MSigDB - Hallmark") {
      
      gmt <- hallmark
      
    }
    else if (input$GeneSetChosen == "User Upload") {
      
      req(input$UserGeneList)
      gmt <- user_gs()
      
    }
    
  })
  
  #generate ranked list of genes
  user_gl <- reactive({
    
    header_check <- input$UserGeneListheaderCheck
    gs.u <- input$UserGeneList
    ext <- tools::file_ext(gs.u$datapath)
    req(gs.u)
    validate(need(ext == c("tsv","txt"), "Please upload .tsv or .txt file"))
    
    df <- as.data.frame(read_delim(gs.u$datapath, delim = '\t', col_names = header_check))
    
    # assume just list of gene symbols
    if (ncol(df) == 1) {
      
      # generate rank from high to low
      ranking <- seq(from = nrow(df),to = 1)
      names(ranking) <- as.character(df[,1])
      
    }
    # assume gene symbols and rank
    else if (ncol(df) == 2) {
      
      ranking <- df[,2]
      names(ranking) <- as.character(df[,1])
      ranking <- sort(ranking, decreasing = T)
      
    }
    # assume gene symbols and general annotation columns
    else if (ncol(df) > 2) {
      
      # generate rank from high to low
      ranking <- seq(from = nrow(df),to = 1)
      names(ranking) <- as.character(df[,1])
      
    }
    
    ranking
    
  })
  
  EnrichTabReact <- reactive({
    
    gmt <- gmt_react()
    ranked_genes <- user_gl()
    pval <- input$gseaPval
    gsea.res <- GSEA(ranked_genes, TERM2GENE = gmt, pvalueCutoff = pval)
    gsea.res
    
  })
  
  output$EnrichTable <- DT::renderDataTable({
    
    res <- EnrichTabReact()
    res_df <- res@result
    
    if (nrow(res@result) == 0) {
      
      res_df <- data.frame("No_term_enriched_under_specific_Pvalue_Cutoff")
      
    }
    
    DT::datatable(res_df,
                  extensions = c("KeyTable", "FixedHeader","FixedColumns"),
                  options = list(keys = T,
                                 searchHighlight = T,
                                 pageLength = 25,
                                 lengthMenu = c("10", "25", "50", "100"),
                                 scrollX = T,
                                 autoWidth = TRUE,
                                 fixedColumns = list(leftColumns = 1)),
                  rownames = F) %>%
      formatRound(columns = c(2:10), digits = 4)
    
  })
  
  output$dnldEnrichTable <- downloadHandler(
    filename = function() {
      paste("EnrichedSignaturesTable.txt",sep = "")
    },
    content = function(file) {
      res <- EnrichTabReact()
      res_df <- res@result
      write_delim(res_df,file,delim = '\t')
    }
  )
  
  output$GeneSetTable <- DT::renderDataTable({
    
    res <- EnrichTabReact()
    res_df <- res@result
    GeneSets <- res_df[,c(1,5,6)]
    
    DT::datatable(GeneSets,
                  selection = list(mode = 'single', selected = 1),
                  options = list(keys = TRUE,
                                 searchHighlight = TRUE,
                                 pageLength = 20,
                                 lengthMenu = c("10", "20", "50", "100")),
                  rownames = F) %>%
      formatRound(columns = c(2,3), digits = 4)
    
  })
  
  output$enrichplot <- renderPlot({
    
    if (length(input$GeneSetTable_rows_selected) > 0) {
      
      res <- EnrichTabReact()
      res_df <- res@result
      geneset_name <- as.character(res_df[input$GeneSetTable_rows_selected,1])
      gseaplot2(res,
                geneset_name,
                geneset_name,
                pvalue_table = F)
      
    }
    
  })
  
  output$NESandPval <- renderText({
    
    if (length(input$GeneSetTable_rows_selected) > 0){
      res <- EnrichTabReact()
      gsea.df <- as.data.frame(res@result)
      GS = as.character(gsea.df[input$GeneSetTable_rows_selected,1])
      NES = gsea.df$NES[which(gsea.df[,'ID']==GS)]
      Pval = gsea.df$pvalue[which(gsea.df[,'ID']==GS)]
      NES.o <- paste0("NES: ", NES)
      Pval.o <- paste0("Pvalue: ", Pval)
      #if (NES > 0){
      #  UpOrDown <- paste("Based on the normalized enrichment score above, the", GS, "gene set is upregulated in", input$comparisonA , "group.")
      #}
      #else {
      #  UpOrDown <- paste("Based on the normalized enrichment score above, the", GS, "gene set is downregulated in", input$comparisonA , "group.")
      #}
      #paste(NES.o, Pval.o, UpOrDown, sep = '\n')
      paste(NES.o, Pval.o, sep = '\n')
    }
    else if (length(input$msigdbTable_rows_selected) == 0){
      paste("Please select gene set from side panel table to begin.", sep = '')
    }
    
  })
  
  output$dnldPlotSVG_gsea <- downloadHandler(
    filename = function() {
      
      res <- EnrichTabReact()
      gsea.df <- as.data.frame(res@result)
      GS = as.character(gsea.df[input$GeneSetTable_rows_selected,1])
      paste(GS,"_EnrichmentPlot.svg",sep = "")
      
    },
    content = function(file) {
      
      res <- EnrichTabReact()
      res_df <- res@result
      geneset_name <- as.character(res_df[input$GeneSetTable_rows_selected,1])
      p <- gseaplot2(res,
                geneset_name,
                geneset_name,
                pvalue_table = F)
      ggsave(file,p, width = 10, height = 8)
      
    }
  )
  output$dnldPlotPDF_gsea <- downloadHandler(
    filename = function() {
      
      res <- EnrichTabReact()
      gsea.df <- as.data.frame(res@result)
      GS = as.character(gsea.df[input$GeneSetTable_rows_selected,1])
      paste(GS,"_EnrichmentPlot.pdf",sep = "")
      
    },
    content = function(file) {
      
      res <- EnrichTabReact()
      res_df <- res@result
      geneset_name <- as.character(res_df[input$GeneSetTable_rows_selected,1])
      p <- gseaplot2(res,
                     geneset_name,
                     geneset_name,
                     pvalue_table = F)
      ggsave(file,p, width = 10, height = 8)
      
    }
  )
  
  LeadingEdgeReact <- reactive({
    
    res <- EnrichTabReact()
    res_df <- res@result
    geneset_name <- as.character(res_df[input$GeneSetTable_rows_selected,1])
    NES = res_df$NES[which(res_df[,'ID']==geneset_name)]
    geneList <- user_gl()
    ## Subset core enriched genes
    genes1 <- as.matrix(res_df[which(res_df$ID==geneset_name),"core_enrichment"])
    genes2 <- strsplit(genes1,"/")
    GeneSymbol <- as.data.frame(genes2)
    colnames(GeneSymbol)[1] <- "Gene_Symbol"
    GeneSymbol$Leading_Edge_Rank <- as.numeric(rownames(GeneSymbol))
    GeneSymbol <- GeneSymbol[,c("Leading_Edge_Rank","Gene_Symbol")]
    
    ranking_df <- stack(geneList)
    colnames(ranking_df) <- c("Hazard_Ratio","Gene_Symbol")
    
    Leading_Merge <- merge(GeneSymbol,ranking_df,by = "Gene_Symbol",all.x = T)
    if (NES < 0) {
      Leading_Merge <- Leading_Merge[order(Leading_Merge$Hazard_Ratio),]
    }
    else if (NES > 0) {
      Leading_Merge <- Leading_Merge[order(Leading_Merge$Hazard_Ratio, decreasing = F),]
    }
    Leading_Merge <- Leading_Merge %>%
      select(Leading_Edge_Rank,Gene_Symbol,Hazard_Ratio)
    Leading_Merge
    
  })
  
  output$LeadingEdgeGenes <- DT::renderDataTable({
    
    LEGs <- LeadingEdgeReact()
    DT::datatable(LEGs,
                  options = list(paging = F,
                                 autoWidth = TRUE,
                                 columnDefs = list(list(className = 'dt-center', targets = 0:2))),
                  rownames = F)
    
  })
  
  output$LEGdownload <- downloadHandler(
    
    filename = function() {
      
      res <- EnrichTabReact()
      res_df <- res@result
      geneset_name <- as.character(res_df[input$GeneSetTable_rows_selected,1])
      paste(geneset_name,"_LeadingEdgeGenes.txt",sep ="")
      
    },
    content = function(file) {
      
      LEGs <- LeadingEdgeReact()
      write_delim(LEGs,file,delim = '\t')
      
    }
    
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
