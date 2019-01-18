source("global.R")
options(shiny.maxRequestSize=30*1024^2)
library(shiny)
library(shinyjs)

shinyServer(function(input, output, session) {
  
  # TabPanel Daftar Hadir
  
  data_anggota_hadir <- reactive({
    anggota = getTable("ANGGOTA", dataloc)
    anggota = subset(anggota, MEMBER_ID == input$no_anggota_hadir)
    if(nrow(anggota)>0){
      anggota$PHONE = ifelse(!is.na(anggota$PHONE) | anggota$PHONE != "", paste0("0",anggota$PHONE), anggota$PHONE)
    }
    anggota
  })
  
  output$data_anggota_hadir = renderUI({
    anggota = data_anggota_hadir()
    
    list(
      h4("Member Data :", style = "color:darkblue;font-weight:bold;"),
      div(style = "display:inline-block;vertical-align:top;width:100%;",
          column(2,
                 p("Member ID", style = "color:darkblue;")
          ),
          column(4,
                 p(paste(":", anggota$MEMBER_ID), style = "color:darkblue;")
          ),
          column(2,
                 p("Phone Number", style = "color:darkblue;")
          ),
          column(4,
                 p(paste0(": ", anggota$PHONE), style = "color:darkblue;")
          )
      ),
      div(style = "display:inline-block;vertical-align:top;width:100%;",
          column(2,
                 p("Name", style = "color:darkblue;")
          ),
          column(4,
                 p(paste(":",anggota$NAME), style = "color:darkblue;")
          ),
          column(2,
                 p("Status", style = "color:darkblue;")
          ),
          column(4,
                 p(paste(":",anggota$STATUS), style = "color:darkblue;")
          )
      ),
      div(style = "display:inline-block;vertical-align:top;width:100%;",
          column(2,
                 p("Class/Department", style = "color:darkblue;")
          ),
          column(4,
                 p(paste(":",anggota$CLASS), style = "color:darkblue;")
          )
      )
    )
  })
  
  observeEvent(input$btn_hadir, {
    
    anggota = data_anggota_hadir()
    if(input$no_anggota_hadir == "" | nrow(anggota) == 0)
    {
      showModal(modalDialog(
        title = "System Information",
        "Please provide your Member ID.", size = "s", footer = modalButton("Close", icon = icon("times-circle"))
      ))
    } else if(input$keperluan_hadir == "none"){
      showModal(modalDialog(
        title = "System Information",
        "Please provide your Member ID and select your needs.", footer = modalButton("Close", icon = icon("times-circle"))
      ))
    } else {
      no_anggota = anggota$MEMBER_ID
      tgl_hadir = Sys.time()
      keperluan = input$keperluan_hadir
      
      upd_query = paste0("INSERT INTO HADIR VALUES('", no_anggota, "', '", tgl_hadir, "', '", keperluan, "');")
      dbcon = dbConnect(RSQLite::SQLite(), dataloc)
      rs = dbSendStatement(dbcon, upd_query)
      dbClearResult(rs)
      dbDisconnect(dbcon)
      
      updateTextInput(session, "no_anggota_hadir", NULL, value = "", placeholder = "e.g 161710001")
      updateSelectInput(session, "keperluan_hadir", NULL, choices = list("-- Select Needs --" = "none", "Borrowing" = "Pinjam", "Reading" = "Baca", "Return" = "Kembali", "Others" = "Lain"), selected = "none")
      
      showModal(modalDialog(
        title = "System Information", footer = modalButton("Close", icon = icon("times-circle")),
        paste0("Welcome to Library, ", anggota$NAME, "!"), size = "m"
      ))
    }
  })
  
  # TabPanel Daftar Pinjam
  
  data_anggota_pinjam <- reactive({
    anggota = getTable("ANGGOTA", dataloc)
    anggota = subset(anggota, MEMBER_ID == input$no_anggota_pinjam)
    if(nrow(anggota)>0){
      anggota$PHONE = ifelse(!is.na(anggota$PHONE) | anggota$PHONE != "", paste0("0", anggota$PHONE), anggota$PHONE)
    }
    anggota
  })
  
  output$data_anggota_pinjam = renderUI({
    anggota = data_anggota_pinjam()
    
    list(
      h4("Member Data :", style = "color:darkblue;font-weight:bold;"),
      div(style = "display:inline-block;vertical-align:top;width:100%;",
          column(2,
                 p("Member ID", style = "color:darkblue;")
          ),
          column(4,
                 p(paste(":", anggota$MEMBER_ID), style = "color:darkblue;")
          ),
          column(2,
                 p("Phone Number", style = "color:darkblue;")
          ),
          column(4,
                 p(paste0(": ", anggota$PHONE), style = "color:darkblue;")
          )
      ),
      div(style = "display:inline-block;vertical-align:top;width:100%;",
          column(2,
                 p("Name", style = "color:darkblue;")
          ),
          column(4,
                 p(paste(":", anggota$NAME), style = "color:darkblue;")
          ),
          column(2,
                 p("Status", style = "color:darkblue;")
          ),
          column(4,
                 p(paste(":", anggota$STATUS), style = "color:darkblue;")
          )
      ),
      div(style = "display:inline-block;vertical-align:top;width:100%;",
          column(2,
                 p("Class/Department", style = "color:darkblue;")
          ),
          column(4,
                 p(paste(":", anggota$CLASS), style = "color:darkblue;")
          )
      )
    )
  })
  
  data_buku_pinjam <- eventReactive(input$collect_buku_pinjam, {
    buku = getTable("BUKU", dataloc)
    buku = buku[grep(toupper(input$judul_buku_pinjam), buku$TITLE),]
    buku
  })
  
  output$data_buku_pinjam = renderUI({
    buku = data_buku_pinjam()
    
    list(
      h4("Data Buku :", style = "color:darkblue;font-weight:bold;"),
      output$data_tabel_buku_pinjam <- DT::renderDataTable(rownames = FALSE, {
        if(is.null(buku))
        {
          return(NULL)
        } else {
          buku
        }
      })
    )
  })
  
  observeEvent(input$pinjam, {
    anggota = data_anggota_pinjam()
    buku = data_buku_pinjam()
    if(input$no_anggota_pinjam == "" | nrow(anggota) == 0)
    {
      showModal(modalDialog(
        title = "System Information", footer = modalButton("Close", icon = icon("times-circle")),
        "Please provide your correct Member ID.", easyClose = FALSE 
      ))
    } else if(nrow(buku) == 0 | is.null(buku) | nrow(buku) > 1)
    {
      showModal(modalDialog(
        title = "System Information", footer = modalButton("Close", icon = icon("times-circle")),
        "Please select a book, filter by the title.", easyClose = FALSE 
      ))
    } else if(as.numeric(buku$STOCK) == 0)
    {
      showModal(modalDialog(
        title = "Informasi Sistem", footer = modalButton("Tutup", icon = icon("times-circle")),
        "This book can not be borrowed. Please Check the STOCK.", easyClose = FALSE 
      ))
    } else {
      no_pinjam = format(Sys.time(), "%Y%m%d%H%M%S")
      no_anggota = anggota$MEMBER_ID
      nama = anggota$NAME
      no_buku = buku$BOOK_NO
      judul = buku$TITLE
      tgl_pinjam = Sys.time()
      tgl_terakhir = as.Date(tgl_pinjam) + as.numeric(buku$MAX_BORROW_DURATION)
      tgl_terakhir = format(tgl_terakhir, "%Y-%m-%d 16:00:00")
      
      insrt_query = paste0("INSERT INTO PINJAM (BORROW_NO, MEMBER_ID, NAME, BOOK_NO, TITLE, BORROW_DATE, LAST_RETURN_DATE) VALUES('", no_pinjam, "', '", no_anggota, "', '", nama, "', '", no_buku, "', '", judul, "', '", tgl_pinjam, "', '", tgl_terakhir, "');")
      upd_query = paste0("UPDATE BUKU SET STOCK = ", as.numeric(buku$STOCK)-1, " where BOOK_NO = '", no_buku, "' and TITLE = '", judul, "';")
      dbcon = dbConnect(RSQLite::SQLite(), dataloc)
      rs = dbSendStatement(dbcon, insrt_query)
      dbClearResult(rs)
      rs = dbSendStatement(dbcon, upd_query)
      dbClearResult(rs)
      dbDisconnect(dbcon)
      
      updateTextInput(session, "no_anggota_pinjam", NULL, value = "", placeholder = "e.g 161710001")
      updateTextInput(session, "judul_buku_pinjam", NULL, value = "", placeholder = "e.g. PELANGI SENJA")
      
      showModal(modalDialog(
        title = "System Information", footer = modalButton("Close", icon = icon("check-circle")),
        HTML(paste0("Borrowing number <strong>", no_pinjam,"</strong> saved succesfully.<br/>The last return is <strong>", tgl_terakhir, "</strong><br/>Please note the information above.")), size = "m"
      ))
    }
  })
  
  # TabPanel Daftar Kembali
  data_pinjam_kembali <- eventReactive(input$collect_data_kembali, {
    pinjam = getTable("PINJAM", dataloc)
    pinjam = subset(pinjam, BORROW_NO == input$no_pinjaman)
    pinjam
  })
  
  data_anggota_kembali <- reactive({
    pinjam = data_pinjam_kembali()
    # pinjam = getTable("PINJAM", dataloc)
    # pinjam = subset(pinjam, BORROW_NO == input$no_pinjaman)
    
    anggota = getTable("ANGGOTA", dataloc)
    anggota = subset(anggota, MEMBER_ID == pinjam$MEMBER_ID)
    if(nrow(anggota)>0){
      anggota$PHONE = ifelse(!is.na(anggota$PHONE) | anggota$PHONE != "", paste0("0",anggota$PHONE), anggota$PHONE)
    }
    anggota
  })
  
  output$data_anggota_kembali = renderUI({
    anggota = data_anggota_kembali()
    
    list(
      wellPanel(style = paste0("background:", panel_colr_val, ";"),
                h4("Member Data :", style = "color:darkblue;font-weight:bold;"),
                div(style = "display:inline-block;vertical-align:top;width:100%;",
                    column(2,
                           p("Member ID", style = "color:darkblue;")
                    ),
                    column(4,
                           p(paste(":",anggota$MEMBER_ID), style = "color:darkblue;")
                    ),
                    column(2,
                           p("Phone Number", style = "color:darkblue;")
                    ),
                    column(4,
                           p(paste0(": ", anggota$PHONE), style = "color:darkblue;")
                    )
                ),
                div(style = "display:inline-block;vertical-align:top;width:100%;",
                    column(2,
                           p("Name", style = "color:darkblue;")
                    ),
                    column(4,
                           p(paste(":", anggota$NAME), style = "color:darkblue;")
                    ),
                    column(2,
                           p("Status", style = "color:darkblue;")
                    ),
                    column(4,
                           p(paste(":", anggota$STATUS), style = "color:darkblue;")
                    )
                ),
                div(style = "display:inline-block;vertical-align:top;width:100%;",
                    column(2,
                           p("Class/Department", style = "color:darkblue;")
                    ),
                    column(4,
                           p(paste(":", anggota$CLASS), style = "color:darkblue;")
                    )
                )
      )
    )
  })
  
  data_buku_kembali <- reactive({
    pinjam = data_pinjam_kembali()
    # pinjam = getTable("PINJAM", dataloc)
    # pinjam = subset(pinjam, BORROW_NO == input$no_pinjaman)
    
    buku = getTable("BUKU", dataloc)
    buku = subset(buku, BOOK_NO == pinjam$BOOK_NO & TITLE == toupper(pinjam$TITLE))
    buku
  })
  
  output$data_buku_kembali = renderUI({
    pinjam = data_pinjam_kembali()
    if(input$no_pinjaman == "")
    {
      return(NULL)
    } else {
      list(
        h4("Book's Data:", style = "color:darkblue;font-weight:bold;"),
        output$data_tabel_buku_kembali <- DT::renderDataTable(rownames = FALSE, {
          if(is.null(pinjam))
          {
            return(NULL)
          } else {
            pinjam
          }
        })
      )
    }
  })
  
  observeEvent(input$kembali, {
    if(input$no_pinjaman == "")
    {
      showModal(modalDialog(
        title = "System Information", footer = modalButton("Close", icon = icon("times-circle")),
        "Please provide the Borrowing Number.", easyClose = FALSE
      ))
    } else {
      pinjam = data_pinjam_kembali()
      anggota = data_anggota_kembali()
      buku = data_buku_kembali()
      
      tgl_kembali = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      no_anggota = anggota$MEMBER_ID
      nama = anggota$NAME
      no_buku = pinjam$BOOK_NO
      judul = pinjam$TITLE
      tgl_kembali = Sys.time()
      telat = round(as.numeric(difftime(pinjam$LAST_RETURN_DATE, tgl_kembali, units = "days")))
      telat = ifelse(telat >= 0, 0, abs(telat))
      denda = telat*buku$PAY_LATE_FEE
      
      insrt_query = paste0("UPDATE PINJAM SET RETURN_DATE = '", tgl_kembali, "', DAY_LATE = ", telat, ", PAY_LATE_FEE = ", denda, " where BORROW_NO = '", input$no_pinjaman, "';")
      upd_query = paste0("UPDATE BUKU SET STOCK = ", as.numeric(buku$STOCK)+1, " where BOOK_NO = '", no_buku, "' and TITLE = '", judul, "';")
      dbcon = dbConnect(RSQLite::SQLite(), dataloc)
      rs = dbSendStatement(dbcon, insrt_query)
      dbClearResult(rs)
      rs = dbSendStatement(dbcon, upd_query)
      dbClearResult(rs)
      dbDisconnect(dbcon)
      
      updateTextInput(session, "no_pinjaman", NULL, value = "", placeholder = "e.g. 20180806191045")
      
      showModal(modalDialog(
        title = "System Information", footer = modalButton("Close", icon = icon("check-circle")),
        HTML(paste0("Data saved succesfully.<br/>Returning late <strong>", telat, "</strong> day(s) and pay late fee is <strong>", format.money(denda))), size = "m"
      ))
    }
  })
  
  observeEvent(input$btn_set_apply, {
    if(input$set_panel_colr == "Merah Muda")
    {
      set_panel_colr_val = "pink"
    } else if(input$set_panel_colr == "Biru Muda")
    {
      set_panel_colr_val = "lightblue"
    } else if(input$set_panel_colr == "Biru")
    {
      set_panel_colr_val = "blue"
    } else if(input$set_panel_colr == "Merah")
    {
      set_panel_colr_val = "red"
    } else if(input$set_panel_colr == "Kuning")
    {
      set_panel_colr_val = "yellow"
    } else if(input$set_panel_colr == "Hijau")
    {
      set_panel_colr_val = "green"
    } else if(input$set_panel_colr == "Hijau Muda")
    {
      set_panel_colr_val = "lightgreen"
    } 
    upd_query = paste0("UPDATE PENGATURAN SET PANEL_COLR_DISP = '", input$set_panel_colr, "', PANEL_COLR_VAL = '", set_panel_colr_val, "', THEME = '", input$set_theme_colr, "';")
    # upd_query = paste0("UPDATE BUKU SET STOCK = ", as.numeric(buku$STOCK)+1, " where BOOK_NO = '", no_buku, "' and TITLE = '", judul, "';")
    dbcon = dbConnect(RSQLite::SQLite(), dataloc)
    rs = dbSendStatement(dbcon, upd_query)
    dbClearResult(rs)
    dbDisconnect(dbcon)
    tbl_setting = getTable("PENGATURAN", dataloc)
    # panel_colr_disp <<- tbl_setting$PANEL_COLR_DISP
    # panel_colr_val <<- tbl_setting$PANEL_COLR_VAL
    
    showModal(modalDialog(
      title = "System Information", footer = modalButton("Close", icon = icon("circle-times")),
      "Please close this application. Your setting will be applied when you open this application next time."
    ))
  })
  
  output$tbl_cari_pinjam <- DT::renderDataTable(rownames = FALSE, {
    pinjam = getTable("PINJAM", dataloc)
    if(input$cari_pinjam_filter == "Late")
    {
      pinjam = subset(pinjam, LAST_RETURN_DATE < RETURN_DATE)
    } else if(input$cari_pinjam_filter == "Not Returned Yet") 
    {
      pinjam = subset(pinjam, RETURN_DATE == "" | is.na(RETURN_DATE) | is.null(RETURN_DATE))
    } else if(input$cari_pinjam_filter == "Returned") 
    {
      pinjam = subset(pinjam, RETURN_DATE != "" & !is.na(RETURN_DATE) & !is.null(RETURN_DATE))
    } else if(input$cari_pinjam_filter == "Late & Not Returned Yet") 
    {
      pinjam = subset(pinjam, (RETURN_DATE == "" | is.na(RETURN_DATE) | is.null(RETURN_DATE)) & LAST_RETURN_DATE < Sys.time())
    } else {
      pinjam = pinjam
    }
    pinjam = pinjam[order(pinjam$BORROW_DATE, decreasing = TRUE),] 
    pinjam
  })
  
  data_buku_cari_judul <- reactive({
    if(input$cari_buku_judul == "") 
    {
      return(NULL)
    } else {
      buku = getTable("BUKU", dataloc)
      buku = buku[grep(toupper(input$cari_buku_judul), buku$TITLE),]
      buku
    }
  })
  
  output$tbl_cari_buku_judul = renderUI({
    buku = data_buku_cari_judul()
    
    if(is.null(buku))
    {
      return(NULL)
    } else {
      list(
        h4("Book's Data :", style = "color:darkblue;font-weight:bold;"),
        output$data_buku_judul <- DT::renderDataTable(rownames = FALSE, {
          buku
        })
      )
    }
  })
  
  data_buku_cari_no_buku <- reactive({
    if(input$cari_buku_no_buku == "") {return(NULL)}
    buku = getTable("BUKU", dataloc)
    buku = buku[grep(toupper(input$cari_buku_no_buku), buku$BOOK_NO),]
    buku
  })
  
  output$tbl_cari_buku_no_buku = renderUI({
    buku = data_buku_cari_no_buku()
    
    if(is.null(buku))
    {
      return(NULL)
    } else {
      list(
        h4("Book's Data :", style = "color:darkblue;font-weight:bold;"),
        output$data_buku_no_buku <- DT::renderDataTable(rownames = FALSE, {
          buku
        })
      )
    }
  })
  
  data_buku_cari_thn_masuk <- reactive({
    if(input$cari_buku_thn_masuk == "") {return(NULL)}
    buku = getTable("BUKU", dataloc)
    buku = buku[grep(toupper(input$cari_buku_thn_masuk), buku$REGISTER_DATE),]
    buku
  })
  
  output$tbl_cari_buku_thn_masuk = renderUI({
    buku = data_buku_cari_thn_masuk()
    
    if(is.null(buku))
    {
      return(NULL)
    } else {
      list(
        h4("Book's Data :", style = "color:darkblue;font-weight:bold;"),
        output$data_buku_thn_masuk <- DT::renderDataTable(rownames = FALSE, {
          buku
        })
      )
    }
  })
  
  data_buku_cari_pengarang <- reactive({
    if(input$cari_buku_pengarang == "") {return(NULL)}
    buku = getTable("BUKU", dataloc)
    buku = buku[grep(toupper(input$cari_buku_pengarang), buku$AUTHOR),]
    buku
  })
  
  output$tbl_cari_buku_pengarang = renderUI({
    buku = data_buku_cari_pengarang()
    
    if(is.null(buku))
    {
      return(NULL)
    } else {
      list(
        h4("Book's Data :", style = "color:darkblue;font-weight:bold;"),
        output$data_buku_pengarang <- DT::renderDataTable(rownames = FALSE, {
          buku
        })
      )
    }
  })
  
  data_buku_cari_tambah_stok <- reactive({
    if(input$buku_cari_tambah_stok_judul == "" 
       & input$buku_cari_tambah_stok_pengarang == "" 
       & input$buku_cari_tambah_stok_penerbit == "") 
    {
      return(NULL)
    } else {
      
      if(input$buku_cari_tambah_stok_judul != "" & 
         input$buku_cari_tambah_stok_pengarang == "" & 
         input$buku_cari_tambah_stok_penerbit == "")
      {
        buku = getTable("BUKU", dataloc)
        buku = buku[grep(toupper(input$buku_cari_tambah_stok_judul), buku$TITLE),]
      } else if(input$buku_cari_tambah_stok_judul == "" & 
                input$buku_cari_tambah_stok_pengarang != "" & 
                input$buku_cari_tambah_stok_penerbit == "")
      {
        buku = getTable("BUKU", dataloc)
        buku = buku[grep(toupper(input$buku_cari_tambah_stok_pengarang), buku$AUTHOR),]
      } else if(input$buku_cari_tambah_stok_judul == "" & 
                input$buku_cari_tambah_stok_pengarang == "" & 
                input$buku_cari_tambah_stok_penerbit != "")
      {
        buku = getTable("BUKU", dataloc)
        buku = buku[grep(toupper(input$buku_cari_tambah_stok_penerbit), buku$PUBLISHER),]
      } else if(input$buku_cari_tambah_stok_judul != "" & 
                input$buku_cari_tambah_stok_pengarang != "" & 
                input$buku_cari_tambah_stok_penerbit == "")
      {
        buku = getTable("BUKU", dataloc)
        buku = buku[grep(toupper(input$buku_cari_tambah_stok_judul), buku$TITLE),] 
        buku = buku[grep(toupper(input$buku_cari_tambah_stok_pengarang), buku$AUTHOR),]
      } else if(input$buku_cari_tambah_stok_judul != "" & 
                input$buku_cari_tambah_stok_pengarang == "" & 
                input$buku_cari_tambah_stok_penerbit != "")
      {
        buku = getTable("BUKU", dataloc)
        buku = buku[grep(toupper(input$buku_cari_tambah_stok_judul), buku$TITLE),] 
        buku = buku[grep(toupper(input$buku_cari_tambah_stok_penerbit), buku$PUBLISHER),]
      } else if(input$buku_cari_tambah_stok_judul == "" & 
                input$buku_cari_tambah_stok_pengarang != "" & 
                input$buku_cari_tambah_stok_penerbit != "")
      {
        buku = getTable("BUKU", dataloc)
        buku = buku[grep(toupper(input$buku_cari_tambah_stok_penerbit), buku$PUBLISHER),]
        buku = buku[grep(toupper(input$buku_cari_tambah_stok_pengarang), buku$AUTHOR),]
      } else if(input$buku_cari_tambah_stok_judul != "" & 
                input$buku_cari_tambah_stok_pengarang != "" & 
                input$buku_cari_tambah_stok_penerbit != "")
      {
        buku = getTable("BUKU", dataloc)
        buku = buku[grep(toupper(input$buku_cari_tambah_stok_judul), buku$TITLE),]
        buku = buku[grep(toupper(input$buku_cari_tambah_stok_pengarang), buku$AUTHOR),]
        buku = buku[grep(toupper(input$buku_cari_tambah_stok_penerbit), buku$PUBLISHER),]
      }
      # buku = buku[grep(toupper(input$buku_cari_tambah_stok_judul), buku$TITLE) |
      #               grep(toupper(input$buku_cari_tambah_stok_pengarang), buku$AUTHOR) |
      #               grep(toupper(input$buku_cari_tambah_stok_penerbit), buku$PUBLISHER),]
      buku
    }
  })
  
  output$tbl_buku_cari_tambah_stok = renderUI({
    buku = data_buku_cari_tambah_stok()
    
    if(is.null(buku))
    {
      return(NULL)
    } else {
      list(
        h4("Book's Data :", style = "color:darkblue;font-weight:bold;"),
        output$data_cari_tambah_stok <- DT::renderDataTable(rownames = FALSE, {
          buku
        })
      )
    }
  })
  
  observeEvent(input$btn_tambah_stok, {
    buku = data_buku_cari_tambah_stok()
    if(is.null(buku))
    {
      return(NULL)
    } else if(nrow(buku) > 0)
    {
      upd_query = paste0("UPDATE BUKU SET STOCK = ", buku$STOCK + as.numeric(input$buku_cari_tambah_stok_tambah), " WHERE BOOK_NO = '", buku$BOOK_NO, "' and TITLE = '", buku$TITLE, "' and AUTHOR = '", buku$AUTHOR, "' and PUBLISHER = '", buku$PUBLISHER, "';")
      # upd_query = paste0("UPDATE BUKU SET STOCK = ", as.numeric(buku$STOCK)+1, " where BOOK_NO = '", no_buku, "' and TITLE = '", judul, "';")
      dbcon = dbConnect(RSQLite::SQLite(), dataloc)
      rs = dbSendStatement(dbcon, upd_query)
      dbClearResult(rs)
      dbDisconnect(dbcon)
      # buku = getTable("BUKU", dataloc)
      # panel_colr_val <<- tbl_setting$PANEL_COLR_VAL
      updateTextInput(session, "buku_cari_tambah_stok_judul", NULL, value = "", placeholder = "Book's title or part of it")
      updateTextInput(session, "buku_cari_tambah_stok_penerbit", NULL, value = "", placeholder = "Publisher's name")
      updateTextInput(session, "buku_cari_tambah_stok_pengarang", NULL, value = "", placeholder = "Author's name")
      updateNumericInput(session, "buku_cari_tambah_stok_tambah", NULL, value = 1, min = 0, max = 100)
      
      showModal(modalDialog(
        title = "System Information", footer = modalButton("Close", icon = icon("circle-times")),
        paste0("Data saved succesfully."), easyClose = FALSE
      ))
    }
  })
  
  output$tbl_buku_baru_upload <- DT::renderDataTable(rownames = FALSE, {
    infile = input$file_buku_baru
    if(is.null(infile)){return(NULL)}
    new_data = read.xlsx(infile$datapath, sheetIndex = 1, header = T)
    new_data
  })
  
  output$btn_dwnTemplateTambahBuku <- downloadHandler(
    filename = function(){
      paste0("Template Data Add New Book.xlsx")
    },
    content = function(con){
      file.copy("data/Template Tambah Buku.xlsx", con)
    }
  )
  
  # session$onSessionEnded(function() {
  #       stopApp()
  # 		q("no")
  #     })
})