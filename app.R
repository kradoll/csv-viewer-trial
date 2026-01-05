library(shiny)
library(DT)
library(shinyjs)
library(readr)

# ===============================
# 定数（体験版制限）
# ===============================
TRIAL_ROW_LIMIT <- 1000

# -------------------------------
# モジュール定義（CSV対応・体験版）
# -------------------------------
csvViewerModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # ---- 体験版メッセージ ----
    div(
      style = "background-color:#fff3cd; border:1px solid #ffeeba; padding:10px; margin-bottom:10px;",
      strong("Trial version: "),
      "表示は先頭 ", TRIAL_ROW_LIMIT, " 行までに制限されています。また、列の選択機能も制限されています。"
    ),
    
    # ドラッグ＆ドロップ
    tags$div(
      id = ns("dropZone"),
      style = "border: 2px dashed #aaa; border-radius: 5px; padding: 10px; margin-bottom: 20px;",
      tags$p("CSVファイルが含まれるフォルダをここにドラッグ＆ドロップしてください"),
      tags$input(
        id = ns("folderInput"),
        type = "text",
        placeholder = "フォルダパスを入力するか、フォルダをドラッグしてください",
        style = "width: 100%;"
      )
    ),
    
    sidebarLayout(
      sidebarPanel(
        uiOutput(ns("fileSelector")),
        hr(),
        
        h4("エンコーディング設定"),
        selectInput(
          ns("encoding"),
          "エンコーディング:",
          choices = c(
            "UTF-8" = "UTF-8",
            "Shift-JIS" = "SHIFT-JIS",
            "EUC-JP" = "EUC-JP",
            "CP932" = "CP932",
            "ISO-2022-JP" = "ISO-2022-JP"
          ),
          selected = "UTF-8"
        ),
        actionButton(ns("reloadFile"), "ファイルを再読み込み", class = "btn-primary"),
        
        hr(),
        h4("表示する列を選択"),
        uiOutput(ns("columnSelector")),
        width = 3
      ),
      
      mainPanel(
        fluidRow(
          column(6, h4("データセット情報"), verbatimTextOutput(ns("datasetInfo"))),
          column(6, h4("選択データ情報"), verbatimTextOutput(ns("selectedDataInfo")))
        ),
        h4("データ表示"),
        DTOutput(ns("dataTable")),
        h4("列情報"),
        uiOutput(ns("columnInfoTable")),
        width = 9
      )
    )
  )
}

csvViewerModule <- function(input, output, session) {
  ns <- session$ns
  
  moduleState <- reactiveValues(
    folderPath = "",
    selectedFile = NULL,
    selectedColumns = NULL,
    currentEncoding = "UTF-8"
  )
  
  # --- Drag & Drop JS ---
  session$onFlushed(function() {
    shinyjs::runjs(sprintf("
      var dz = document.getElementById('%s');
      dz.addEventListener('dragover', e => { e.preventDefault(); });
      dz.addEventListener('drop', e => {
        e.preventDefault();
        if (e.dataTransfer.files.length > 0) {
          Shiny.setInputValue('%s', e.dataTransfer.files[0].path);
        }
      });
    ", ns("dropZone"), ns("folderPath")))
  })
  
  observeEvent(input$folderPath, {
    moduleState$folderPath <- input$folderPath
    updateTextInput(session, "folderInput", value = input$folderPath)
  })
  
  observeEvent(input$folderInput, {
    moduleState$folderPath <- input$folderInput
  })
  
  csvFiles <- reactive({
    req(moduleState$folderPath)
    if (dir.exists(moduleState$folderPath)) {
      list.files(moduleState$folderPath, "\\.(csv|CSV)$")
    } else character(0)
  })
  
  output$fileSelector <- renderUI({
    files <- csvFiles()
    if (length(files)) {
      selectInput(ns("selectedFile"), "CSVファイル", files)
    } else {
      em("CSVファイルが見つかりません")
    }
  })
  
  observeEvent(input$selectedFile, {
    moduleState$selectedFile <- input$selectedFile
  })
  
  observeEvent(input$encoding, {
    moduleState$currentEncoding <- input$encoding
  })
  
  observeEvent(input$reloadFile, {
    invalidateLater(0)
  })
  
  selectedData <- reactive({
    req(moduleState$selectedFile, moduleState$folderPath)
    fp <- file.path(moduleState$folderPath, moduleState$selectedFile)
    
    raw <- read.csv(
      fp,
      fileEncoding = moduleState$currentEncoding,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    
    list(
      full_n = nrow(raw),
      data = head(raw, TRIAL_ROW_LIMIT),
      col_info = data.frame(
        列名 = names(raw),
        データ型 = sapply(raw, function(x) class(x)[1]),
        stringsAsFactors = FALSE
      )
    )
  })
  
  output$datasetInfo <- renderPrint({
    d <- selectedData()
    cat("ファイル名:", moduleState$selectedFile, "\n")
    cat("エンコーディング:", moduleState$currentEncoding, "\n")
    cat("総行数:", d$full_n, "\n")
    cat("表示行数（Trial）:", nrow(d$data), "\n")
    cat("列数:", ncol(d$data), "\n")
  })
  
  output$selectedDataInfo <- renderPrint({
    cat("選択列数:", length(selectedColumns()), "\n")
  })
  
  selectedColumns <- reactive({
    names(selectedData()$data)
  })
  
  output$dataTable <- renderDT({
    datatable(
      selectedData()$data,
      filter = "top",
      options = list(pageLength = 100, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  output$columnInfoTable <- renderUI({
    ci <- selectedData()$col_info
    tags$table(
      class = "table table-sm",
      tags$thead(tags$tr(tags$th("列名"), tags$th("型"))),
      tags$tbody(
        lapply(seq_len(nrow(ci)), function(i) {
          tags$tr(tags$td(ci$列名[i]), tags$td(ci$データ型[i]))
        })
      )
    )
  })
  
  return(reactive(sub("\\.csv$", "", moduleState$selectedFile)))
}

# -------------------------------
# メインアプリ
# -------------------------------
ui <- fluidPage(
  useShinyjs(),
  titlePanel("CSV ファイルビューア（Trial）"),
  div(id = "tabContainer")
)

server <- function(input, output, session) {
  insertUI(
    selector = "#tabContainer",
    where = "beforeEnd",
    ui = csvViewerModuleUI("tab1")
  )
  callModule(csvViewerModule, "tab1")
}

shinyApp(ui, server)
