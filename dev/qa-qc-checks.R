## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=FALSE, include=TRUE-------------------------------------------------
abs_index <- data.frame('Index Name'=c("SUVA 254", "SUVA 280", "SVA 412",
                               "Spectral Slope (275-295)",
                               "Spectral Slope (350-400)",
                               "Spectral Slope Ratio", 
                               "E~2~/E~3~", "E~4~/E~6~"
                               ), 
                        'Index Abbreviation' =c("SUVA254", "SUVA280", "SVA412", "S275_295",
                                    "S350_400", "SR", "E2_E3", "E4_E6"),
                        'Interpretation'=c("Proxy for aromaticity", 
                                    "Proxy for aromaticity",
                                    "Proxy for aromaticity",
                                    "Related to molecular weight and aromaticity",
                                    "Related to molecular weight and aromaticity", 
                                    "Related to molecular weight", 
                                    "Related to molecular weight and aromaticity",
                                    "Related to humic-like organic matter"
                                    ))
colnames(abs_index) <- c("Index Name", "Index Abbreviation", "Interpretation")
knitr::kable(abs_index)

## ----echo=FALSE, include=TRUE-------------------------------------------------
eem_index <- data.frame('Index Name'=c("Peak B", "Peak T", "Peak A", "Peak M", "Peak C",
                                       "Peak D", "Peak E", "Peak N", 
                                       "Ratio of Peak A to Peak T",
                                       "Ratio of Peak C to Peak A",
                                       "Ratio of Peak C to Peak M",
                                       "Ratio of Peak C to Peak T",
                                       "Fluorescence Index", "Humification Index-Zsolnay",
                                       "Humification Index-Ohno",
                                       "Freshness Index", "Biological Index"), 
                        'Index Abbreviation' =c("pB", "pT", "pA", "pM","pC","pD",
                                                "pE", "pN","rAT", "rCA", "rCM", "rCT", 
                                                "FI", "HIX", "HIX_ohno", "fresh", "BIX"),
                        'Interpretation'=c("tyrosine-like, protein-like organic matter",
                                           "tryptophan-like, protein-like organic matter",
                                           "UV humic-like organic matter",
                                           "marine humic-like organic matter", 
                                           "visible humic-like organic matter", 
                                           "soil fulvic acid-like organic matter",
                                          "soil fulvic acid-like organic matter",
                                          "related to phytoplankton productivity",
                                          "ratio of humic-like to fresh organic matter",
                                          "ratio of humic-like to fulvic-like organic matter",
                                          "amount of blueshifted organic matter",
                                          "related to biochemical oxygen demand",
                                          "terrestrial versus microbial sources", 
                                          "indication of humic substances",
                                          "indication of humic substances", 
                                          "indication of recently produced organic matter",
                                          "indicator of autotrophic activity"

                                    ))
colnames(eem_index) <- c("Index Name", "Index Abbreviation", "Interpretation")
knitr::kable(eem_index)

