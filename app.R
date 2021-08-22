options(scipen = 999)
source("global.R")

library(shiny)
library(shinydashboard)

ui <- dashboardPage(
    skin = "yellow",
    dashboardHeader(titleWidth = 300,
                    title = "Geometrik Brownian Hareket"),
    dashboardSidebar(
        width = 300,
        sidebarMenu(
            menuItem(text = "Kaynak (T\u00fcrk\u00e7e)", href = "https://tez.yok.gov.tr/UlusalTezMerkezi/tezDetay.jsp?id=VV-UQ-24tnvxGfzYC_2xnA&no=7WtpiyYPjnk_PYQRWl1zGw"),
            menuItem(text = "Kaynak (\u0130ngilizce)", href = "https://ro.uow.edu.au/cgi/viewcontent.cgi?article=1705&context=aabfj"),
            div(
                style = "text-align:center",
                "Geometrik Brownian Hareket ile ilgili",
                br(),
                "yukar\u0131daki kayna\u011f\u0131:"
            ),
            radioButtons(
                inputId = "about",
                label = "",
                choices = c("Okumad\u0131m" = "1", "Okudum" = "2"),
                inline = T
            ),
            conditionalPanel(
                condition = "input.about == '2'",
                radioButtons(
                    inputId = "choose",
                    label = "",
                    choices = c(
                        "Girece\u011fim De\u011ferleri Biliyorum" = "1",
                        "Girece\u011fim De\u011ferleri Bilmiyorum" = "2"
                    ),
                    selected = "1"
                ),
                conditionalPanel(
                    condition = "input.choose == '2'",
                    div(
                        style = "text-align:center",
                        "Hisse ve tarih aral\u0131\u011f\u0131 bilgisini girerek",
                        br(),
                        "'Se\u00e7ilen Hisse Bilgileri' b\u00f6l\u00fcm\u00fcnden",
                        br(),
                        "bilgilere ula\u015fabilirsiniz."
                    ),
                    selectInput(
                        inputId = "stock",
                        label = "Hisse",
                        choices = stockList
                    ),
                    dateRangeInput(
                        inputId = "dates",
                        label = "Tarih Aral\u0131\u011f\u0131 (Beklenen Getiri ve Standart Sapma i\u00e7in)",
                        separator = " - "
                    ),
                    dateRangeInput(
                        inputId = "dates2",
                        label = "Sim\u00fclasyon Ba\u015flang\u0131\u00e7 Tarihi",
                        separator = " - "
                    ),
                ),
                numericInput(
                    inputId = "nsim",
                    label = "Sim\u00fclasyon Say\u0131s\u0131",
                    value = 100
                ),
                numericInput(
                    inputId = "t",
                    label = "Zaman Periyodu (Uzunluk)",
                    value = 252
                ),
                numericInput(
                    inputId = "s0",
                    label = "Ba\u015flang\u0131\u00e7 Fiyat\u0131",
                    value = 10
                ),
                numericInput(
                    inputId = "mu",
                    label = "Beklenen Getiri",
                    value = 0
                ),
                numericInput(
                    inputId = "sigma",
                    label = "Standart Sapma",
                    value = 0.1
                )
            )
        )
    ),
    dashboardBody(tabBox(
        width = 900,
        tabPanel(title = "Sim\u00fcle Edilmi\u015f De\u011ferler", plotOutput(outputId = "plt", height = 800)),
        tabPanel(title = "Se\u00e7ilen Hisse Bilgileri", plotOutput(outputId = "splt", height = 800)),
        tabPanel(title = "Ger\u00e7ekle\u015fen - Sim\u00fclasyon", plotOutput(outputId = "gsplt", height = 800)),
        tabPanel(title = "Kaydet", dataTableOutput(outputId = "save", height = 800))
    ))
)

server <- function(input, output) {
    gbmdf <- reactive({
        gbm_loop(
            nsim = input$nsim,
            t = input$t,
            mu = input$mu,
            sigma = input$sigma,
            S0 = input$s0
        ) %>%
            as.data.frame() %>%
            mutate(ix = 1:nrow(.)) %>%
            pivot_longer(-ix, names_to = 'sim', values_to = 'price')
        
    })
    
    stockdf <- reactive({
        master <- data.frame()
        for (i in 1:length(input$stock)) {
            tbl <- stockprice(
                ticker = input$stock[i],
                startDate = format(input$dates[1]),
                endDate = format(input$dates[2])
            )
            
            master <- master %>% bind_rows(tbl)
            
            Sys.sleep(time = 1)
            
        }
        
        master
    })
    
    stockdf2 <- reactive({
        master2 <- data.frame()
        for (i in 1:length(input$stock)) {
            tbl2 <- stockprice(
                ticker = input$stock[i],
                startDate = format(input$dates2[1]),
                endDate = format(input$dates2[2])
            )
            
            master2 <- master2 %>% bind_rows(tbl2)
            
            Sys.sleep(time = 1)
            
        }
        
        master2
    })
    
    output$plt <- renderPlot({
        pltdf <- gbmdf() %>%
            pivot_wider(names_from = sim, values_from = price) %>%
            mutate("Ortalama" = rowMeans(.[, -1])) %>%
            pivot_longer(-ix, names_to = 'sim', values_to = 'price')
        
        ggplot() +
            geom_line(
                data = pltdf %>% filter(sim == "Ortalama"),
                aes(
                    x = ix,
                    y = price,
                    group = 1
                ),
                color = "blue",
                size = 2
            ) +
            geom_line(
                data = pltdf %>% filter(sim != "Ortalama"),
                aes(
                    x = ix,
                    y = price,
                    group = sim
                ),
                color = "gray",
                size = 1,
                alpha = .3
            ) +
            theme_minimal() +
            theme(legend.position = 'none') +
            theme(axis.title = element_blank()) +
            labs(
                title = paste0("Sim\u00fcle Edilmi\u015f De\u011ferler ve Ortalama*"),
                subtitle = "*mavi"
            )
    })
    
    output$splt <- renderPlot({
        spltdf <- stockdf() %>%
            select(HGDG_TARIH, HGDG_KAPANIS) %>%
            mutate(
                HGDG_TARIH = dmy(HGDG_TARIH),
                HGDG_KAPANIS = as.numeric(HGDG_KAPANIS)
            ) %>%
            mutate("Pct" = lag(log(
                lead(HGDG_KAPANIS) / HGDG_KAPANIS
            )))
        
        ggplot(spltdf, aes(x = Pct)) +
            geom_histogram(fill = "red") +
            theme_minimal() +
            theme(axis.title = element_blank()) +
            labs(
                title = paste0(
                    input$stock,
                    " hissesine ait ",
                    nrow(spltdf) - 1,
                    " i\u015f g\u00fcn\u00fc i\u00e7in ",
                    "beklenen getiri ",
                    round(mean(spltdf$Pct, na.rm = T) / input$t, digits = 4),
                    " ve standart sapma ",
                    round(sd(spltdf$Pct, na.rm = T) / sqrt(1 / input$t), digits = 4),
                    " hesaplanm\u0131\u015ft\u0131r."
                )
            )
        
    })
    
    output$gsplt <- renderPlot({
        gbmdf2 <- gbmdf() %>%
            pivot_wider(names_from = sim, values_from = price) %>%
            mutate("Ortalama" = rowMeans(.[, -1]))
        
        plistdf <- stockdf2() %>%
            select(HGDG_TARIH, HGDG_KAPANIS) %>%
            mutate(
                HGDG_TARIH = dmy(HGDG_TARIH),
                HGDG_KAPANIS = as.numeric(HGDG_KAPANIS)
            ) %>%
            mutate("Pct" = lag(log(
                lead(HGDG_KAPANIS) / HGDG_KAPANIS
            ))) %>%
            na.omit() %>%
            mutate("ix" = seq(1, nrow(.), 1))
        
        mainplistdf <- plistdf %>%
            full_join(gbmdf2, by = "ix") %>%
            select(-Pct, -HGDG_TARIH) %>%
            pivot_longer(-ix, names_to = 'sim', values_to = 'price') %>%
            mutate("Gr" = ifelse(
                sim == "HGDG_KAPANIS",
                "real",
                ifelse(sim == "Ortalama", "avg", "sim")
            ))
        
        initialPrice <- mainplistdf[1, 3]
        
        ggplot() +
            geom_line(
                data = mainplistdf %>% filter(Gr == "real"),
                aes(
                    x = ix,
                    y = price,
                    group = 1
                ),
                color = "red",
                size = 2
            ) +
            geom_line(
                data = mainplistdf %>% filter(Gr == "avg"),
                aes(
                    x = ix,
                    y = price,
                    group = 1
                ),
                color = "blue",
                size = 2
            ) +
            geom_line(
                data = mainplistdf %>% filter(Gr == "sim"),
                aes(
                    x = ix,
                    y = price,
                    group = sim
                ),
                color = "gray",
                size = 1,
                alpha = .3
            ) +
            theme_minimal() +
            theme(axis.title = element_blank()) +
            labs(
                title = paste0(
                    input$stock,
                    " Hissesine Ait Ger\u00e7ekle\u015fen*, Sim\u00fcle Edilmi\u015f ve Sim\u00fclasyona Ait Ortalama** De\u011ferler\n",
                    "Hissenin ba\u015flang\u0131\u00e7 fiyat\u0131 ",
                    initialPrice,
                    " olmal\u0131d\u0131r. Do\u011frulu\u011funu teyit ediniz."
                ),
                subtitle = "*k\u0131rm\u0131z\u0131\n**mavi"
            )
        
    })
    
    output$save <- renderDataTable({
        savetbl <- gbmdf() %>%
            pivot_wider(names_from = sim, values_from = price) %>%
            mutate("Ortalama" = rowMeans(.[, -1]))
        
        psavedf <- stockdf2() %>%
            select(HGDG_TARIH, HGDG_KAPANIS) %>%
            mutate(
                HGDG_TARIH = dmy(HGDG_TARIH),
                HGDG_KAPANIS = as.numeric(HGDG_KAPANIS)
            ) %>%
            mutate("Pct" = lag(log(
                lead(HGDG_KAPANIS) / HGDG_KAPANIS
            ))) %>%
            na.omit() %>%
            mutate("ix" = seq(1, nrow(.), 1)) %>%
            select(-Pct)
        
        mastersavedf <- psavedf %>%
            full_join(savetbl, by = "ix")
        
        datatable(
            mastersavedf,
            extensions = "Buttons",
            options = list(
                dom = "Bfrtip",
                buttons = c("copy", "csv", "excel", "pdf", "print"),
                paging = T,
                pageLength = 20
            )
        )
        
    })
    
}

shinyApp(ui, server)