###################################################################################
#' @title bundeslanddata data
#' 
#' @description  
#' Data: Bundeslaender (German Federal State Information)
#' Umlaute converted via 
#' \code{bd[] <- lapply(bd, function(x) gsub("\xfc", "ue", x))}
#' 
#' @details 
#' Interne Datensaetze.
#' 
#'  Datensatz `landkreis`:
#'  Daten aus GENESIS-ONLINE Tabelle 12411-0018 (Bevoelkerung: 
#'  Kreise, Stichtag, Geschlecht, Altersgruppen).
#'  
#'  Datensatz `bundesland`:
#'  Daten aus GENESIS-ONLINE Tabelle 12411-0013 (Bevoelkerung: 
#'  Bundeslaender, Stichtag, Geschlecht, Altersjahre)
#'  
#'      
#' @format 'data.frame':	obs. of  34 variables:
#' \describe{
#'   \item{IdBundesland}{int}
#'   \item{Bundesland}{Factor w/ 16 levels }
#'   \item{Geschlecht}{Factor w/ 2 levels }
#'   \item{Alter}{int}
#'   \item{N}{int population size}
#' }
#'
###################################################################################
"bundeslanddata"
