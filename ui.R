# source("global.R")
library(shiny)
library(shinythemes)
library(shinyjs)

jscode <- '
$(function() {
  var $els = $("[data-proxy-click]");
  $.each(
    $els,
    function(idx, el) {
      var $el = $(el);
      var $proxy = $("#" + $el.data("proxyClick"));
      $el.keydown(function (e) {
        if (e.keyCode == 13) {
          $proxy.click();
        }
      });
    }
  );
});
'
pg_title = "High School Administration"
subpg_title = "Library Administration System"

shinyUI(
  fluidPage(
    shinyjs::useShinyjs(),
    tags$head(tags$script(HTML(jscode)),
              tags$link(rel = "shortcut icon", href = "images/Book.png")),
     theme = shinytheme("cerulean"),
    fluidRow(titlePanel(windowTitle = subpg_title,
                        title =
                          div(style = "display:inline-block;",
                              column(1,
                                     img( # image for instance
                                       src = "images/TutWuriH.png",
                                       # height = 70,
                                       width = "130px",
                                       style = "margin:0 40px 0 40px;"
                                     )
                              ),
                              column(10,
                                     h3(pg_title, style = paste0("margin:15px 60px -15px 120px; font-size: 200%; font-weight: bold; color:darkblue;")),
                                     h3(subpg_title, style = paste0("margin:15px 60px -15px 120px; font-weight: bold; color:darkblue;"))#,
                                     # h5("Jl. Citarum, Simpangan, Cikarang Utara, Kab. Bekasi, Jawa Barat 17530", style = "color: black; text-align: center;")
                                     
                              )#,
                              # column(1,
                              #        img(
                              #          src = "images/Book.png",
                              #          # height = 70,
                              #          width = "130px",
                              #          style = "margin:0 40px 0 -30px;align:center;"
                              #        )
                              # )
                          )
    )
    ),
    hr(),
    navlistPanel(widths = c(2, 10), well = FALSE,
                 # tabsetPanel(
                 tabPanel("Attendance", icon = icon("address-card"),
                          fluidRow(
                            column(12,
                                   wellPanel(style = paste0("background:",panel_colr_val,";width: 100%;"), 
                                             div(style = "display:inline-block;vertical-align:top;width:20%;", 
                                                 h4("Member ID", style = "color: darkblue;font-weight: bold;")
                                                 ),
                                             div(style = "display:inline-block;vertical-align:top;width:1%;", 
                                                 h5(":", style = "color:darkblue;font-weight:bold;")
                                                 ),
                                             div(style = "display:inline-block;vertical-align:top;width:20%;height:50%;",
                                                 tagAppendAttributes(
                                                   textInput("no_anggota_hadir", label = NULL, placeholder = "e.g. 161710001", width = "150%"),
                                                   `data-proxy-click` = "btn_hadir"
                                                 )),
                                             div(style = "display:inline-block;vertical-align:top;", 
                                                 actionButton("btn_hadir", "Attend", width = "100%", icon = icon("list-alt", lib = "glyphicon"), class = "btn-primary")),
                                             br(),
                                             div(style = "display:inline-block;vertical-align:top;width:20%;", 
                                                 h4("Needs", style = "color:darkblue;font-weight:bold;")
                                                 ),
                                             div(style = "display:inline-block;vertical-align:top;width:1%;", 
                                                 h5(":", style = "color:darkblue;font-weight:bold;")
                                                 ),
                                             div(style = "display:inline-block;vertical-align:top;width:20%;", 
                                                 tagAppendAttributes(
                                                   selectInput("keperluan_hadir", NULL, 
                                                               choices = list("-- Select Need --" = "none", "Borrowing Book" = "Pinjam", "Reading" = "Baca", "Return Book" = "Kembali", "Others" = "Lain")),
                                                   `data-proxy-click` = "btn_hadir"
                                                 ))
                                   ),
                                   wellPanel(style = paste0("background:",panel_colr_val,";"),
                                             uiOutput("data_anggota_hadir")
                                   )
                            )
                          )
                 ),
                 tabPanel("Borrowing", icon = icon("paste", lib = "glyphicon"),
                          fluidRow(
                            column(12,
                                   wellPanel(style = paste0("background:", panel_colr_val, ";"),
                                             div(style = "display:inline-block;vertical-align:top;width:20%;", 
                                                 h4("Member ID", style = "color: darkblue;font-weight: bold;")
                                                 ),
                                             div(style = "display:inline-block;vertical-align:top;width:1%;", 
                                                 h5(":", style = "color:darkblue;font-weight:bold;")
                                                 ),
                                             div(style = "display:inline-block;vertical-align:top;width:20%;",
                                                 tagAppendAttributes(
                                                   textInput("no_anggota_pinjam", label = NULL, placeholder = "e.g. 161710001", width = "150%"),
                                                   `data-proxy-click` = "collect_pinjam"
                                                 )),
                                             div(style = "display:inline-block;vertical-align:top;", actionButton("collect_pinjam", "Search", width = "100%", icon = icon("search", lib = "glyphicon"), class = "btn-primary"))
                                   ),
                                   wellPanel(style = paste0("background:", panel_colr_val, ";"),
                                             uiOutput("data_anggota_pinjam")
                                   )
                            )
                          ),
                          fluidRow(
                            column(12,
                                   wellPanel(style = paste0("background:", panel_colr_val, ";"),
                                             div(style = "display:inline-block;vertical-align:top;width:20%;", 
                                                 h4("Book Title", style = "color: darkblue;font-weight: bold;")
                                                 ),
                                             div(style = "display:inline-block;vertical-align:top;width:1%;", 
                                                 h5(":", style = "color:darkblue;font-weight:bold;")
                                                 ),
                                             div(style = "display:inline-block;vertical-align:top;width:40%;",
                                                 tagAppendAttributes(
                                                   textInput("judul_buku_pinjam", label = NULL, placeholder = "e.g. PELANGI SENJA"),
                                                   `data-proxy-click` = "collect_buku_pinjam"
                                                 )),
                                             div(style = "display:inline-block;vertical-align:top;", 
                                                 actionButton("collect_buku_pinjam", "Search", width = "100%", icon = icon("search"), class = "btn-primary")
                                                 ),
                                             div(style = "display:inline-block;vertical-align:top;", 
                                                 actionButton("pinjam", "Borrow", width = "100%", icon = icon("book"), class = "btn-primary")
                                                 ),
                                             helpText("Provide book's title until only one row on the bellow table.")
                                   ),
                                   uiOutput("data_buku_pinjam")
                            )
                          )
                 ),
                 tabPanel("Return", icon = icon("copy", lib = "glyphicon"),
                          fluidRow(
                            column(12,
                                   wellPanel(style = paste0("background:", panel_colr_val, ";"),
                                             div(style = "display:inline-block;vertical-align:top;width:20%;", 
                                                 h4("Borrowing ID", style = "color: darkblue;font-weight: bold;")
                                                 ),
                                             div(style = "display:inline-block;vertical-align:top;width:1%;", 
                                                 h5(":", style = "color:darkblue;font-weight:bold;")
                                                 ),
                                             div(style = "display:inline-block;vertical-align:top;width:20%;",
                                                 tagAppendAttributes(
                                                   textInput("no_pinjaman", label = NULL, placeholder = "e.g. 20180806191045", width = "150%"),
                                                   `data-proxy-click` = "collect_data_kembali"
                                                 )),
                                             div(style = "display:inline-block;vertical-align:top;", 
                                                 actionButton("collect_data_kembali", "Search", width = "100%", icon = icon("search", lib = "glyphicon"), class = "btn-primary")
                                                 ),
                                             div(style = "display:inline-block;vertical-align:top;", 
                                                 actionButton("kembali", "Return", width = "100%", icon = icon("book"), class = "btn-primary")
                                                 )
                                   ),
                                   uiOutput("data_anggota_kembali"),
                                   uiOutput("data_buku_kembali")
                            )
                          )
                 ),
                 navbarMenu("Search", icon = icon("search"),
                            # tabsetPanel(type = "pills",
                            tabPanel("Book", icon = icon("book"),
                                     tabsetPanel(#type = "pills",
                                       tabPanel("Title", icon = icon("font"),
                                                br(),
                                                wellPanel(style = paste0("background:", panel_colr_val, ";"),
                                                          fluidRow(
                                                            column(12,
                                                                   div(style = "display:inline-block;vertical-align:top;width:25%;", 
                                                                       h4("Title", style = "color:darkblue;font-weight:bold;")
                                                                       ),
                                                                   div(style = "display:inline-block;vertical-align:top;width:1%;", 
                                                                       h5(":", style = "color:darkblue;font-weight:bold;")
                                                                       ),
                                                                   tagAppendAttributes(
                                                                     div(style = "display:inline-block;vertical-align:top;width:50%;", 
                                                                         textInput("cari_buku_judul", label = NULL, placeholder = "Book's title or part of it", width = "100%")
                                                                         ),
                                                                     `data-proxy-click` = "btn_cari_buku_judul"
                                                                   ),
                                                                   div(style = "display:inline-block;vertical-align:top;", 
                                                                       actionButton("btn_cari_buku_judul", "Search", width = "100%", icon = icon("search"), class = "btn-primary")
                                                                       )
                                                            )
                                                          )
                                                ),
                                                column(12,
                                                       br(),
                                                       uiOutput("tbl_cari_buku_judul")
                                                )
                                       ),
                                       tabPanel("Number of Book", icon = icon("hashtag"),
                                                br(),
                                                wellPanel(style = paste0("background:", panel_colr_val, ";"),
                                                          fluidRow(
                                                            column(12,
                                                                   div(style = "display:inline-block;vertical-align:top;width:25%;", 
                                                                       h4("Number of Book", style = "color:darkblue;font-weight:bold;")
                                                                       ),
                                                                   div(style = "display:inline-block;vertical-align:top;width:1%;", 
                                                                       h5(":", style = "color:darkblue;font-weight:bold;")
                                                                       ),
                                                                   tagAppendAttributes(
                                                                     div(style = "display:inline-block;vertical-align:top;width:30%;", 
                                                                         textInput("cari_buku_no_buku", label = NULL, placeholder = "e.g. ", width = "100%")
                                                                         ),
                                                                     `data-proxy-click` = "btn_cari_buku_no_buku"
                                                                   ),
                                                                   div(style = "display:inline-block;vertical-align:top;", 
                                                                       actionButton("btn_cari_buku_no_buku", "Search", width = "100%", icon = icon("search"), class = "btn-primary")
                                                                       )
                                                            )
                                                          )
                                                ),
                                                column(12,
                                                       br(),
                                                       uiOutput("tbl_cari_buku_no_buku")
                                                )
                                       ),
                                       # tabPanel("Year of Registered", icon = icon("calendar"),
                                       #          br(),
                                       #          wellPanel(style = paste0("background:", panel_colr_val, ";"),
                                       #                    fluidRow(
                                       #                      column(12,
                                       #                             div(style = "display:inline-block;vertical-align:top;width:25%;", 
                                       #                                 h4("Tahun Masuk", style = "color:darkblue;font-weight:bold;")
                                       #                                 ),
                                       #                             div(style = "display:inline-block;vertical-align:top;width:1%;", 
                                       #                                 h5(":", style = "color:darkblue;font-weight:bold;")
                                       #                                 ),
                                       #                             tagAppendAttributes(
                                       #                               div(style = "display:inline-block;vertical-align:top;width:30%;", 
                                       #                                   textInput("cari_buku_thn_masuk", label = NULL, placeholder = "Tahun Masuk", width = "100%")
                                       #                                   ),
                                       #                               `data-proxy-click` = "btn_cari_buku_thn_masuk"
                                       #                             ),
                                       #                             div(style = "display:inline-block;vertical-align:top;", 
                                       #                                 actionButton("btn_cari_buku_thn_masuk", "Search", width = "100%", icon = icon("search"), class = "btn-primary")
                                       #                                 )
                                       #                      )
                                       #                    )
                                       #          ),
                                       #          column(12,
                                       #                 br(),
                                       #                 uiOutput("tbl_cari_buku_thn_masuk")
                                       #          )
                                       # ),
                                       tabPanel("Author", icon = icon("users"),
                                                br(),
                                                wellPanel(style = paste0("background:", panel_colr_val, ";"),
                                                          fluidRow(
                                                            column(12,
                                                                   div(style = "display:inline-block;vertical-align:top;width:25%;", 
                                                                       h4("Author", style = "color:darkblue;font-weight:bold;")
                                                                       ),
                                                                   div(style = "display:inline-block;vertical-align:top;width:1%;", 
                                                                       h5(":", style = "color:darkblue;font-weight:bold;")
                                                                       ),
                                                                   tagAppendAttributes(
                                                                     div(style = "display:inline-block;vertical-align:top;width:30%;", 
                                                                         textInput("cari_buku_pengarang", label = NULL, placeholder = "Author's name", width = "100%")
                                                                         ),
                                                                     `data-proxy-click` = "btn_cari_buku_pengarang"
                                                                   ),
                                                                   div(style = "display:inline-block;vertical-align:top;", 
                                                                       actionButton("btn_cari_buku_pengarang", "Search", width = "100%", icon = icon("search"), class = "btn-primary")
                                                                       )
                                                            )
                                                          )
                                                ),
                                                column(12,
                                                       br(),
                                                       uiOutput("tbl_cari_buku_pengarang")
                                                )
                                       )
                                     )
                            ),
                            tabPanel("Borrowed", icon = icon("folder-open"),
                                     wellPanel(style = paste0("background:", panel_colr_val, ";"),
                                               div(style = "display:inline-block;vertical-align:top;width:20%;", 
                                                   h5("Filter By", style = "color:darkblue;font-weight:bold;")
                                                   ),
                                               div(style = "display:inline-block;vertical-align:top;width:1%;", 
                                                   h5(":", style = "color:darkblue;font-weight:bold;")
                                                   ),
                                               div(style = "display:inline-block;vertical-align:top;width:30%;", 
                                                   selectInput("cari_pinjam_filter", NULL, 
                                                               choices = c("All", "Late", "Not Returned Yet", "Returned", "Late & Not Returned Yet")
                                                               )
                                                   )
                                     ),
                                     dataTableOutput("tbl_cari_pinjam")
                            )
                            # )
                            
                 ),
                 navbarMenu("Books", icon = icon("book"),
                            tabPanel("Add Stock", icon = icon("plus-circle"),
                                     wellPanel(style = paste0("background:", panel_colr_val, ";"),
                                               # div(style = "display:inline-block;vertical-align:top;width:20%;", h5("Nomor Buku", style = "color:darkblue;font-weight:bold;")),
                                               # div(style = "display:inline-block;vertical-align:top;width:1%;", h5(":", style = "color:darkblue;font-weight:bold;")),
                                               # div(style = "display:inline-block;vertical-align:top;width:20%;", textInput("buku_cari_tambah_stok_no_buku", NULL, placeholder = "contoh: 815")),
                                               # br(),
                                               div(style = "display:inline-block;vertical-align:top;width:20%;", 
                                                   h5("Title", style = "color:darkblue;font-weight:bold;")
                                                   ),
                                               div(style = "display:inline-block;vertical-align:top;width:1%;", 
                                                   h5(":", style = "color:darkblue;font-weight:bold;")
                                                   ),
                                               div(style = "display:inline-block;vertical-align:top;width:32%;", 
                                                   textInput("buku_cari_tambah_stok_judul", NULL, placeholder = "Book's title or part of it")
                                                   ),
                                               br(),
                                               div(style = "display:inline-block;vertical-align:top;width:20%;", 
                                                   h5("Author", style = "color:darkblue;font-weight:bold;")
                                                   ),
                                               div(style = "display:inline-block;vertical-align:top;width:1%;", 
                                                   h5(":", style = "color:darkblue;font-weight:bold;")
                                                   ),
                                               div(style = "display:inline-block;vertical-align:top;width:32%;", 
                                                   textInput("buku_cari_tambah_stok_pengarang", NULL, placeholder = "Author's name")
                                                   ),
                                               br(),
                                               div(style = "display:inline-block;vertical-align:top;width:20%;", 
                                                   h5("Publisher", style = "color:darkblue;font-weight:bold;")
                                                   ),
                                               div(style = "display:inline-block;vertical-align:top;width:1%;", 
                                                   h5(":", style = "color:darkblue;font-weight:bold;")
                                                   ),
                                               div(style = "display:inline-block;vertical-align:top;width:32%;", 
                                                   textInput("buku_cari_tambah_stok_penerbit", NULL, placeholder = "Publisher's name")
                                                   ),
                                               br(),
                                               div(style = "display:inline-block;vertical-align:top;width:20%;", 
                                                   h5("Additional Stock", style = "color:darkblue;font-weight:bold;")
                                                   ),
                                               div(style = "display:inline-block;vertical-align:top;width:1%;", 
                                                   h5(":", style = "color:darkblue;font-weight:bold;")
                                                   ),
                                               div(style = "display:inline-block;vertical-align:top;width:20%;", 
                                                   numericInput("buku_cari_tambah_stok_tambah", NULL, value = 1, min = 0, max = 100)
                                                   ),
                                               div(style = "display:inline-block;vertical-align:top;width:20%;", 
                                                   actionButton("btn_tambah_stok", "Add Stock", icon = icon("plus-circle"), class = "btn-primary")
                                                   )
                                     ),
                                     uiOutput("tbl_buku_cari_tambah_stok")
                            ),
                            tabPanel("New Book", icon = icon("signal"),
                                     wellPanel(style = paste0("background:", panel_colr_val, ";"),
                                               fluidRow(
                                                 column(12, 
                                                        radioButtons(inputId = "new_method", label = "Enter New Book Data", choices = list("Upoad File" = "file", "Manual" = "manual"), selected = "file", inline = T)
                                                 )
                                               ),
                                               fluidRow(
                                                 column(8, 
                                                        conditionalPanel(condition = "input.new_method == 'file'",
                                                                         div(style = "display:inline-block;vertical-align:top;width:40%;", 
                                                                             fileInput(inputId = "file_buku_baru", label = NULL, 
                                                                                       accept = c(
                                                                                         "Excel",
                                                                                         "text/comma-separated-values,text/plain",
                                                                                         ".xlsx"), placeholder = "Pilih file Excel...")),
                                                                         div(style = "display:inline-block;vertical-align:top;width:30%;", 
                                                                             actionButton("simpan_buku_baru_upload", "Save Data", icon = icon("save"), class = "btn-primary")))
                                                 ),
                                                 downloadButton("btn_dwnTemplateTambahBuku", "Download Template Add New Book", class = "btn-success", icon = icon("table", lib = "glyphicon"))
                                               )
                                     ),
                                     DT::dataTableOutput("tbl_buku_baru_upload"))
                 )#,
                 # navbarMenu("Anggota", icon = icon("users"),
                 #            tabPanel("Tambah Anggota", icon = icon("plus-circle")),
                 #            tabPanel("Edit Data Anggota", icon = icon("pencil"),
                 #                     tabsetPanel(
                 #                       tabPanel("Update Data Anggota", icon = icon("user")),
                 #                       tabPanel("a"),
                 #                       tabPanel("b")
                 #                     ))
                 # ),
                 # tabPanel("Laporan", icon = icon("copy"),
                 #          downloadButton("laporan", label = "Unduh Laporan", class = "btn-success")
                 # ),
                 # tabPanel("Pengaturan", icon = icon("cogs"),
                 #          wellPanel(style = paste0("background:", panel_colr_val,";"),
                 #                    div(style = "display:inline-block;vertical-align:top;width:30%;", h5("Warna Panel :", style = "font-weight:bold;color:darkblue;")),
                 #                    div(style = "display:inline-block;vertical-align:top;width:30%;", selectInput("set_panel_colr", NULL, 
                 #                                                                                                  choices = c("Merah Muda", "Biru Muda", "Biru", "Merah", "Kuning", "Hijau", "Hijau Muda"), selected = panel_colr_disp)),
                 #                    br(),
                 #                    div(style = "display:inline-block;vertical-align:top;width:30%;", h5("Tema :", style = "font-weight:bold;color:darkblue;")),
                 #                    div(style = "display:inline-block;vertical-align:top;width:30%;", selectInput("set_theme_colr", NULL, 
                 #                                                                                                  choices = thm, selected = theme_colr_disp)),
                 #                    div(style = "display:inline-block;vertical-align:top;width:30%;", actionButton("btn_set_apply", "Terapkan", icon = icon("check-circle"), class = "btn-primary"))
                 #          )
                 # )
    ),
    hr(),
    h5(HTML("Copyright (c) 2018 <strong>Aep Hidayatuloh</strong>"), style = "text-align:right;  color: green;"),
    br()
  )
)
