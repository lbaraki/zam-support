# PROJECT: Zam-support
# PURPOSE: Munge and Analysis of VLC indicators
# AUTHOR: Lemlem Baraki | SI
# REF ID:   69a0f235
# LICENSE: MIT
# DATE: 2023-10-10
# NOTES: Lemlem Baraki | SI

# LOCALS & SETUP ============================================================================

  # Libraries
  library(glitr)
  library(glamr)
  library(gisr)
  library(gophr)
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
        group_by(indicator, fiscal_year, snu1, psnu, ...) %>% #change grouping as needed (psnu, sitename)
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
      create_vl_df() %>% 
      filter(str_detect(period, "23"))
    
    #site level 
    df_vl_peds_sites <- df_site %>% 
      create_vl_df() %>% 
      filter(str_detect(period, "23"))
    
# VIZ ============================================================================

  #Summary Tables
      #Peds VLC goal is 85% 
    
    df_vl_peds %>% 
      #df_vl_peds_sites %>% 
      filter(period == "FY23Q2") %>% #substitute quarter 
      #filter(snu1 %in% c("Northern Province",
      #                "Copperbelt Province",
      #                 "Central Province")) %>%
      arrange(period,vlc) %>% 
      #slice_min(vlc, n = 26) %>%
      gt(
        rowname_col = "snu1",
        groupname_col = "period"
      ) %>% 
      cols_hide(c(tx_curr, tx_pvls, vls, vls_adj)) %>% 
      #fmt_number(columns = c(tx_pvls_d, tx_curr_lag2),
      #          decimals = 0) %>% 
      fmt_percent(columns = vlc, 
                  decimals = 0) %>% 
      #grand_summary_rows(
      # columns = c(tx_pvls_d, tx_curr_lag2),
      # fns = list(Zambia = ~sum(., na.rm = TRUE)),
      #fmt = fmt_number, 
      #decimal = 0)%>% 
      tab_options(data_row.padding = px(1),
                  table.font.size = px(10),
                  row_group.padding = px(1),
                  row_group.font.weight = "bold") %>% 
      gtExtras::gt_hulk_col_numeric(vlc,
                                    domain = c(0,.85),
                                    trim = TRUE) %>% 
      gtExtras::gt_theme_nytimes()  
    #tab_header(title = "22 DISTRICTS WITH PEDS VLC < 50% IN FY23Q2") %>% 
    #gtsave("Images/PEDS_VLC_by_q2.png")
    
    
    
  #Ranking: PEDS VLC% across FY23 quarters
    #filter to provinces (snu1) of interest (Northern, Copperbelt, Central)
      #alt: facet_wrap by snu1
    #group vlc by districts (psnu)
    #add district labels 
    
    df_vl_peds %>%
      #filter(str_detect(period, "Q3", negate = T)) %>% 
      mutate(value_label = case_when(period == max(period) ~ paste(percent(vlc), psnu), 
                                     TRUE ~ percent(vlc))) %>% 
      # filter(snu1 %in% c("Northern Province",
      #                   "Copperbelt Province",
      #                  "Central Province")) %>% 
      ggplot(aes(x = period, y = vlc, group = psnu))+
      geom_line()+
      geom_point()+
      geom_text(aes(label = percent(vlc, 1)),
                size = 7/.pt,
                vjust = -0.25,
                hjust = -.25
      ) +
      #ggrepel::geom_text_repel(aes(label = value_label),
      #                        size = 7/.pt,
      #                       hjust = -.25, 
      #force = 4
      #                      ) +
      #geom_label(aes(label = psnu))+
      facet_wrap(~snu1, scales = "free")+
      scale_x_discrete()+ 
      labs(title = "CHANGE IN PEDS VLC ACROSS FY23", 
           caption = glue("{msd_source_path}\n VLC = TX_PVLS/TX_CURR [2 periods prior]")
      )+
      #coord_cartesian(ylim=c(0, 0.75))+
      scale_y_continuous( 
        labels = scales::percent,
        limits = c(0,.85),
        breaks = seq(0, 1, 0.25)
      )+
      si_style_xline()
    #gtsave("Images/PEDS_VLC_FY23.png")  

# SPINDOWN ============================================================================
