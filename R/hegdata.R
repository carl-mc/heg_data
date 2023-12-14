######################
# HEGDATA Class
######################

#' @title Data on Historical Ethnic Geography
#'
#' @description 
#'    Provides an interface to retrieve the Historical Ethnic Geography dataset 
#'    published in Müller-Crepon, Schvitz and Cederman (Journal of Conflict Resolution, forthcoming).
#'    
#' @section Usage:
#' \preformatted{
#' # Initialize
#' heg.obj <- hegdata$new()
#' 
#' # Apply any LEDA method
#' # heg.obj$method() ## not run.
#' }
#'
#'
#' @details 
#'    
#'     
#'     For full information on the LEDA project and methodology,
#'     read the \href{https://github.com/carl-mc/hegdata/raw/master/docs/hegdata_paper.pdf}{paper}. 
#'     
#'    When using the \code{hegdata} package, please cite:
#'    Müller-Crepon, Carl, Guy Schvitz, and Lars-Erik Cederman (2023). 
#'    "Right-Peopling" the State: Nationalism, Historical Legacies, and Ethnic Cleansing in Europe, 1886-2020. 
#'    \emph{Journal of Conflict Resolution}, forthcoming.
#'      
#'
#' @name hegdata
#' @import R6
#'    raster
#' @docType class
#' 
#' @examples
#' 
#' # Initialize HEG object
#' heg.obj <- hegdata$new()
#' 
#' # Load raster for Polish people
#' 
#' @export
hegdata <- R6Class("hegdata",
                   public = list(
                     ## Public data
                     grp.meta.df = NULL, ## Group metadata
                     cow.meta.df = NULL, ## Group metadata
                     path = NULL,
                     tv.grp.meta.df = NULL, ## Group metadata
                     tv.cow.meta.df = NULL, ## Group metadata
                     tv.path = NULL,
                     main.ras = NULL,
                     groups = NULL,
                     
                     
                     ## Public functions
                     
                     ### Initialize object
                     #' @description
                     #' Initializes a new heegdata object
                     #' @examples
                     #' library(hegdata)
                     #' heg.obj <- hegdata$new()
                     #' 
                     initialize = function(){
                       # Load data
                       
                       ## Non-smoothed
                       self$grp.meta.df <- read.csv(system.file(file.path("extdata",  "meta"), 
                                                                "country_group_periods.csv", 
                                                                package = "hegdata"))
                       self$cow.meta.df <- read.csv(system.file(file.path("extdata",  "meta"), 
                                                                "country_periods.csv", 
                                                                package = "hegdata"))
                       self$path <- file.path("extdata",  "raster")
                       
                       ## Smoothed
                       self$tv.grp.meta.df <- read.csv(system.file(file.path("extdata",  "meta"),
                                                                   "country_group_periods_tv.csv", 
                                                                   package = "hegdata"))
                       self$tv.cow.meta.df <- read.csv(system.file(file.path("extdata",  "meta"), 
                                                                   "country_periods_tv.csv", 
                                                                   package = "hegdata"))
                       self$tv.path <- file.path("extdata",  "raster_tv")
                       
                       ## Main raster
                       self$main.ras <- raster(system.file(file.path("extdata",  "meta"),
                                                           "main_raster.tif", 
                                                           package = "hegdata"))
                       
                       ## Make language level its own column
                       
                       ### Non-smoothed
                       self$grp.meta.df$level <- sapply(strsplit(self$grp.meta.df$heg_main_match, "[", fixed = T),
                                                        function(x){
                                                          if(length(x) == 1){
                                                            NA
                                                          } else {
                                                            gsub("]", "", x[2], fixed = T)
                                                          }
                                                        })
                       self$grp.meta.df$group <- sapply(strsplit(self$grp.meta.df$heg_main_match, "[", fixed = T),
                                                        function(x){
                                                          trimws(x[1])
                                                        })
                       
                       ### Smoothed
                       self$tv.grp.meta.df$level <- sapply(strsplit(self$tv.grp.meta.df$heg_main_match, "[", fixed = T),
                                                           function(x){
                                                             if(length(x) == 1){
                                                               NA
                                                             } else {
                                                               gsub("]", "", x[2], fixed = T)
                                                             }
                                                           })
                       self$tv.grp.meta.df$group <- sapply(strsplit(self$tv.grp.meta.df$heg_main_match, "[", fixed = T),
                                                           function(x){
                                                             trimws(x[1])
                                                           })
                       
                       
                       ## Groups
                       self$groups <- sort(unique(self$grp.meta.df$group))
                       
                       
                     },
                     
                     #' @description Queries all group names
                     #'
                     #' @return Vector with all group names, alphabetically sorted
                     #' @export
                     #'
                     #' @examples
                     #' library(hegdata)
                     #' heg.obj <- hegdata$new()
                     #' heg.obj$all_groups()
                     #' 
                     all_groups = function(){
                       self$groups
                     },
                     
                     #' @description Queries all group meta data
                     #'
                     #' @param smoothed 
                     #'
                     #' @return A dataset with one row per group-country-period combination. 
                     #'    The dataset contains the following variables:
                     #'    \code{gwcode} Gelditsch & Ward country code
                     #'    \code{fid} Unique ID per country-shape
                     #'    \code{group} Group name
                     #'    \code{start} Start year of period
                     #'    \code{end} End year of period
                     #'    \code{rowmin} Min row of this sub-raster in main raster frame (hegdata$main.ras)
                     #'    \code{rowmax} Max row of this sub-raster in main raster frame (hegdata$main.ras)
                     #'    \code{colmin} Min column of this sub-raster in main raster frame (hegdata$main.ras)
                     #'    \code{colmax} Max column of this sub-raster in main raster frame (hegdata$main.ras)
                     #'    \code{filename} File name of subraster
                     #'    \code{level} Level of group on language tree (org refers to original language node)
                     #'    \code{heg_main_match} Group name and language tree level concatenated
                     #'        
                     #' @export
                     #'
                     #' @examples
                     #' library(hegdata)
                     #' heg.obj <- hegdata$new()
                     #' heg.obj$group_meta_data()
                     #' 
                     group_meta_data = function(smoothed = F){
                       if(smoothed){
                         self$tv.grp.meta.df
                       } else {
                         self$grp.meta.df
                       }
                     },
                     
                     
                     #' @description Returns the main raster frame
                     #'
                     #' @return A spatial raster with value 0 for cells covered by HEG and NA for cells outside HEG's coverage. 
                     #' @export
                     #'
                     #' @examples
                     #' library(hegdata)
                     #' heg.obj <- hegdata$new()
                     #' heg.obj$main_raster()
                     #' 
                     main_raster = function(){
                       self$main.ras
                     },
                     
                     
                     #' @description Loads raster for a group year using the baseline, non-smoothed version of the HEG data.  
                     #'
                     #' @param group Specify the group name (lower or upper case, see $all_groups for all group names).
                     #' @param year Specify a year between 1886 and 2020
                     #' @param verbose Flag for level of verbosity. Defaults to FALSE.
                     #'
                     #' @return Returns a raster of the extent of $main.ras (the full HEG frame) 
                     #' with values between 0 and 1 indicating the local population share 
                     #' belonging to the specified group as estimated in the HEG data. 
                     #' 
                     #' @export
                     #' 
                     #' @examples
                     #' library(hegdata)
                     #' heg.ob <- hegdata$new()
                     #' polish <- heg.obj$loadHEGGroup(group = "polish", year = 1918)
                     #' plot(polish)
                     #' 
                     loadHEGGroup = function(group, year, verbose = F){
                       
                       
                       ## Group
                       group <- self$groups[grepl(group, self$groups, ignore.case = T)]
                       
                       ## Check
                       if(length(group) > 1){
                         stop(paste("More than 1 group identified:", paste(group, collapse = "; ")))
                       } else if(length(group) == 0){
                         stop(paste("No group identified"))
                       } else if(verbose){
                         message(paste("Loading data for", group))
                       }
                       
                       ## Path
                       
                       
                       ## Get all files 
                       these.files <- self$grp.meta.df[self$grp.meta.df$start <= year &
                                                         self$grp.meta.df$end >= year & 
                                                         self$grp.meta.df$group == group,]
                       
                       ## Load and add to main.ras
                       raster <- as.matrix(self$main.ras)
                       for(f in seq_len(nrow(these.files))){
                         this.path <- system.file(self$path, 
                                                  these.files$filename[f], 
                                                  package = "hegdata")
                         if(!file.exists(this.path)){
                           warning("missing file")
                           next
                         }
                         sub.r <- as.matrix(raster(this.path))
                         raster[these.files$rowmin[f]:these.files$rowmax[f],
                                these.files$colmin[f]:these.files$colmax[f]][!is.na(sub.r)] <- 
                           sub.r[!is.na(sub.r)]
                       }
                       raster <- raster(raster)
                       extent(raster) <- extent(self$main.ras)
                       
                       ## Return
                       return(raster)
                     },
                     
                     #' @description Loads raster for a group year using the interpolated version of the HEG data.  
                     #'
                     #' @param group Specify the group name (lower or upper case, see $all_groups for all group names).
                     #' @param year Specify a year between 1886 and 2020
                     #' @param verbose Flag for level of verbosity. Defaults to FALSE.
                     #'
                     #' @return Returns a raster of the extent of $main.ras (the full HEG frame) 
                     #' with values between 0 and 1 indicating the local population share 
                     #' belonging to the specified group as estimated in the HEG data. 
                     #'
                     #' @export
                     #' 
                     #' @examples
                     #' library(hegdata)
                     #' heg.ob <- hegdata$new()
                     #' polish <- heg.obj$loadHEGGroup_interpol(group = "polish", year = 1918)
                     #' plot(polish)
                     #' 
                     loadHEGGroup_interpol = function(group, year, verbose = F){
                       ## Group
                       group <- self$groups[grepl(group, self$groups, ignore.case = T)]
                       
                       ## Check
                       if(length(group) > 1){
                         stop(paste("More than 1 group identified:", paste(group, collapse = "; ")))
                       } else if(length(group) == 0){
                         stop(paste("No group identified"))
                       } else if(verbose){
                         message(paste("Loading data for", group))
                       }
                       
                       ## Path
                       
                       
                       ## Get all files 
                       these.files <- self$tv.grp.meta.df[self$tv.grp.meta.df$start <= year &
                                                            self$tv.grp.meta.df$end >= year & 
                                                            self$tv.grp.meta.df$group == group,]
                       
                       ## Load and add to main.ras
                       raster <- as.matrix(self$main.ras)
                       for(f in seq_len(nrow(these.files))){
                         this.path <- system.file(self$tv.path, 
                                                  these.files$filename[f], 
                                                  package = "hegdata")
                         if(!file.exists(this.path)){
                           warning("missing file")
                           next
                         }
                         layer.id <- year - these.files$start[f] + 1
                         sub.r <- as.matrix(raster(this.path,
                                                   band = layer.id))
                         raster[these.files$rowmin[f]:these.files$rowmax[f],
                                these.files$colmin[f]:these.files$colmax[f]][!is.na(sub.r)] <- 
                           sub.r[!is.na(sub.r)]
                       }
                       raster <- raster(raster)
                       extent(raster) <- extent(self$main.ras)
                       
                       ## Return
                       return(raster)
                     }
                   )
)
