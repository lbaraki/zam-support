# PROJECT: Zam-support
# PURPOSE: Munge and Analysis of VLC indicators
# AUTHOR: Lemlem Baraki | SI
# REF ID:   69a0f235
# LICENSE: MIT
# DATE: 2023-10-10
# NOTES: Lemlem Baraki | SI

# LOCALS & SETUP ============================================================================

  # Libraries
  library(gagglr)
  library(tidyverse)
  library(scales)
  library(sf)
  library(extrafont)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(gagglr)
  library(readxl)
  library(glue)
  library(gt)
  library(gtExtras)

    
  # SI specific paths/functions  
  load_secrets()
  merdata <- file.path(glamr::si_path("path_msd"))
  msd_path <- return_latest(merdata, "_PSNU_IM_FY21-24.*Zambia") #grabs district level data
  site_path <- return_latest(merdata, "_Site_IM_FY21-24.*Zambia") #grabs site level data

  # Grab metadata
    msd_source <- source_info(merdata)
    msd_source_path <- source_info(msd_path)
    get_metadata(msd_path)
  
  # REF ID for plots
    ref_id <- "69a0f235"
    
  # Functions
    #Creates a wide viral load coverage data frame 
    create_vl_df <- function(df, ...) {
      df <- df %>%
        filter(
          indicator %in% c("TX_CURR", "TX_PVLS"),
          standardizeddisaggregate %in% c(
            "Age/Sex/HIVStatus",
            "Age/Sex/Indication/HIVStatus"
          )
        ) %>%
        gophr::clean_indicator() %>%
        group_by(indicator, fiscal_year, ...) %>% #change grouping as needed (psnu, sitename) -- use the function ... for this action
        summarise(across(starts_with("qtr"), sum, na.rm = TRUE), 
                  .groups = "drop") %>%
        reshape_msd(include_type = FALSE) %>%
        pivot_wider(
          names_from = indicator,
          names_glue = "{tolower(indicator)}"
        ) %>% 
        #arrange(desc(period))%>% #FY23Q3 at top 
        group_by(...) %>% 
        mutate( #add in calculations 
          tx_curr_lag2 = lag(tx_curr, n = 2),
          vlc = tx_pvls_d / tx_curr_lag2,
          vls = tx_pvls / tx_pvls_d,
          vls_adj = tx_pvls / tx_curr_lag2
        ) %>% 
        ungroup()
      return(df)
    }  
  
  # Limit labels - use this on the x-axis to create some space
  # returns a tick on every nth space given (2 - will skip 1 tick)
    every_nth = function(n) {
      return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
    }  
    
# LOAD DATA ============================================================================  

    #district level 
    df_msd <- read_psd(msd_path) %>% 
      filter(funding_agency =="USAID", 
             trendscoarse == "<15")
    
    names(df_msd) 
    glimpse(df_msd)
    
    
    #site level     
    df_site <- read_psd(site_path) %>% 
      filter(funding_agency == "USAID",
             trendscoarse == "<15")
    
    names(df_site) #add sitename/facility (?) to function
    glimpse(df_site)

# MUNGE ============================================================================
  
    #district level 
    
    df_vl_peds <- df_msd %>%
      create_vl_df(snu1, psnu) %>% #include grouping here
      filter(str_detect(period, "23|22")) %>% 
      arrange(psnu, period)
    
    #site level 
    df_vl_peds_sites <- df_site %>% 
      create_vl_df(psnu, sitename) %>% 
      filter(str_detect(period, "23|22"))
    
# VIZ ============================================================================

  #Summary Tables
      #Peds VLC goal is 85% 
      #Turned into function (substitute period)
    
  vlc_table <- function(sel_period) { 
     vl<- df_vl_peds %>% 
      #df_vl_peds_sites %>% 
      filter(period ==  sel_period,
             vlc <= 0.845)%>% #substitute quarter 
      #filter(snu1 %in% c("Northern Province",
      #                "Copperbelt Province",
      #                 "Central Province")) %>%
      arrange(desc(tx_curr_lag2)) %>% 
      slice_max(tx_curr_lag2, n = 25) %>%
      gt(
        rowname_col = "snu1",
        groupname_col = "period"
      ) %>% 
      cols_hide(c(tx_curr, tx_pvls, vls, vls_adj)) %>% 
      #fmt_number(columns = c(tx_pvls_d, tx_curr_lag2),
      #          decimals = 0) %>% 
      fmt_percent(columns = vlc, 
                  decimals = 0) %>% 
      cols_label(tx_curr_lag2 = "tx_curr",
                 tx_pvls_d = "tx_pvls") %>% 
      #grand_summary_rows(
      # columns = c(tx_pvls_d, tx_curr_lag2),
      # fns = list(Zambia = ~sum(., na.rm = TRUE)),
      #fmt = fmt_number, 
      #decimal = 0)%>% 
      tab_options(data_row.padding = px(1),
                  table.font.size = px(10),
                  row_group.padding = px(1),
                  row_group.font.weight = "bold") %>% 
      gtExtras::gt_hulk_col_numeric(tx_curr_lag2,
                                     #domain = c(0, .845),#range of values 
                                    trim = TRUE) %>% #less intense colors 
      gtExtras::gt_theme_nytimes()  %>% 
    tab_header(title = "DISTRICTS WITH LARGEST TX_CURR & PEDS VLC < 85%") #%>%
      #gt_split(row_every_n = 25) %>% 
      #gtsave_extra(filename = glue("Images/peds_vlc_{sel_period}.png"))
    
  }
    
    vlc_table(sel_period = "FY23Q1") #sub period of interest
    
  
    vlc_table_site <- function(sel_period) { 
      df_vl_peds_sites %>% 
        #df_vl_peds_sites %>% 
        filter(period ==  sel_period,
               vlc <= 0.845)%>% #substitute quarter 
        #filter(snu1 %in% c("Northern Province",
        #                "Copperbelt Province",
        #                 "Central Province")) %>%
        arrange(desc(tx_curr_lag2)) %>% 
        #slice_max(tx_curr_lag2, n = 25) %>%
        gt(
          rowname_col = "psnu",
          groupname_col = "period"
        ) %>% 
        cols_hide(c(tx_curr, tx_pvls, vls, vls_adj)) %>% 
        #fmt_number(columns = c(tx_pvls_d, tx_curr_lag2),
        #          decimals = 0) %>% 
        fmt_percent(columns = vlc, 
                    decimals = 0) %>% 
        cols_label(tx_curr_lag2 = "tx_curr",
                   tx_pvls_d = "tx_pvls") %>% 
        #grand_summary_rows(
        # columns = c(tx_pvls_d, tx_curr_lag2),
        # fns = list(Zambia = ~sum(., na.rm = TRUE)),
        #fmt = fmt_number, 
        #decimal = 0)%>% 
        tab_options(data_row.padding = px(1),
                    table.font.size = px(10),
                    row_group.padding = px(1),
                    row_group.font.weight = "bold") %>% 
        gtExtras::gt_hulk_col_numeric(tx_curr_lag2,
                                      #domain = c(0, .845),#range of values 
                                      trim = TRUE) %>% #less intense colors 
        gtExtras::gt_theme_nytimes()  %>% 
        tab_header(title = "SITES WITH LARGEST TX_CURR & PEDS VLC < 85%") #%>%
      #gt_split(row_every_n = 25) %>% 
      #gtsave_extra(filename = glue("Images/peds_vlc_{sel_period}.png"))
      
    } 
    
    vlc_table_site(sel_period = "FY23Q1") %>%
      gtsave_extra(glue("Images/peds_sites_Q1.png"))
    
    vlc_table_site(sel_period = "FY23Q2") %>% 
      gtsave_extra(glue("Images/peds_sites_Q2.png"))
    
    vlc_table_site(sel_period = "FY23Q3") %>% 
      gtsave_extra(glue("Images/peds_sites_Q3.png"))
    
  #Ranking: PEDS VLC% across FY23 quarters
    #filter to provinces (snu1) of interest (Northern, Copperbelt, Central)
      #alt: facet_wrap by snu1
    #group vlc by districts (psnu)
    #add district labels 
    
    
  # Recommend limiting these to the Provincial level, then facet on PSNUS  
  # Can functionalize this and then loop over a list of snu1s
    
    #prov <- unique(df_vl_peds$snu1) 
    
   peds_vlc_plots <- function(sel_prov) {
     
     #vlc_plot <- 
       df_vl_peds %>%
      filter(snu1 == sel_prov,
             str_detect(period, "23")) %>% 
      mutate(value_label = case_when(period == max(period) ~ paste(percent(vlc), psnu), 
                                     TRUE ~ percent(vlc))) %>% 
      ggplot(aes(x = period, y = vlc, group = psnu)) +
      geom_line(linewidth = 0.5, color = grey70k) +
      geom_point(aes(color = ifelse(vlc<.85, "#287c6f", "#c43d4d")), show.legend = FALSE) +
      scale_color_manual(labels = c("< 85%","> 85%"), values = c('#c43d4d', '#287c6f'))+
      #geom_hline(yintercept = 85, linetype = "dotted", size = 0.5, color= 'red')+
      geom_text(aes(label = percent(vlc, 1)),
                size = 7/.pt,
                vjust = -0.25,
                hjust = -.25) + 
       facet_wrap(~psnu)+
      labs(#color = "VLC",
        title = glue("{toupper(sel_prov)} PEDS VLC IN FY23"), 
        subtitle = "Districts in red with VLC < 85%",
        #subtitle = "<span style='color:#c43d4d'>Districts</span> with VLC",
           caption = glue("{metadata$caption}\n VLC = TX_PVLS/TX_CURR [2 periods prior]")
      )+
       theme(plot.subtitle = element_markdown(),
             strip.text = element_markdown())+ #from ggtext package
      scale_y_continuous( 
        labels = scales::percent,
        limits = c(0, 1.1),
        breaks = seq(0, 1, 0.25))+
      scale_x_discrete(breaks = every_nth(n = 2)) + #add a few spaces between breaks to get cleaner x axis
      si_style_xline(facet_space = 0.99) #%>% 
      #si_save(glue("Graphics/VLC_{sel_prov}.png")) 
    
     #return(vlc_plot)
   }
    
  #Loop over provinces 
    #list of snu1s
   prov_list <- df_vl_peds %>% distinct(snu1) %>% 
     filter(snu1 %ni% c("Southern Province", "Eastern Province")) %>% pull
   
   #use purrr's map function to create list of plots
    #.x = list/atomic vector
   # .f = function
   map(prov_list, ~peds_vlc_plots(.x))


# SPINDOWN ============================================================================

