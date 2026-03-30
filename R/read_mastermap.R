#' Read pavement geometries from OSMasterMap
#'
#' This function reads pavement (sidewalk) geometries from OSMasterMap Topography layers.
#' It uses the descriptive group 'Roadside', make 'Manmade', and feature code 10183.
#'
#' @param gpkg_path Path to the topographic_area.gpkg file
#' @param wkt_filter Optional WKT string to filter by area
#' @param query Optional custom SQL query. If NULL, a default query for pavements is used.
#'
#' @export
#' @examples
#' \dontrun{
#' gpkg_path = "path/to/OSMasterMapTopography_topographic_area.gpkg"
#' pavements = read_pavement(gpkg_path)
#' }
read_pavement = function(gpkg_path, wkt_filter = NULL, query = NULL) {
  if (is.null(query)) {
    query = "SELECT * FROM topographic_area WHERE descriptive_group = 'Roadside' AND make = 'Manmade'"
  }
  sf::st_read(gpkg_path, query = query, wkt_filter = wkt_filter)
}

#' Read mastermap data
#'
#' This function can read-in multiple files stored in datasets downloaded from the Ordnance Survey.
#' It was originally designed to read-in
#' [MasterMap Highway data](https://www.ordnancesurvey.co.uk/documents/os-mastermap-highways-network-product-guide.pdf).
#'
#' @param dir Directory where MasterMap files live
#' @param type_match Type of feature to return, matched on directory
#' @param n How many files to return (all by default, can be resource-consuming)
#' @param ext Extension, e.g. .gz
#'
#' @export
#' @examples
#' \donttest{
#' dir = "~/hd/data/os/Download_mastermap-roads-2019_1483661/MasterMap Highways Network_rami_3480319/"
#' roads_uk = read_mastermap(dir, n = 2)
#' roads_uk = read_mastermap(dir)
#' }
read_mastermap = function(dir, type_match = "roadlink", n = 10000, ext = "gz") {
  f = list.files(path = dir, pattern = type_match, ignore.case = TRUE, full.names = TRUE)
  f = f[!grepl(pattern = "properties", x = f)]
  f = f[grepl(pattern = ext, x = f)]
  f_name = basename(f)
  if(length(f) > n) {
    f = f[1:n]
  }
  message("Found these files (first 3 of ", length(f), " to read): ", f_name[1:3])
  res = lapply(f, sf::read_sf)
  res_dt = data.table::rbindlist(res, fill = TRUE)
  res_df = sf::st_as_sf(res_dt)
  # n_cols_res = sapply(res, ncol)
  # median_n_cols_res = stats::median(n_cols_res)
  # n_cols_equal = n_cols_res == median_n_cols_res
  # if(!all(n_cols_equal)) {
  #   warning("Column numbers differ")
  #   message("Names of first offending object ", which(!n_cols_equal)[1], ":")
  #   message(names(res[[which(!n_cols_equal)[1]]]))
  #   message("Usual names:")
  #   message(names(res[[which(n_cols_equal)[1]]]))
  #   message("Returning the list result")
  #   return(res)
  # }
  # res_df = do.call(rbind, res)
  res_df
}

#' Return all list elements with consistent column names
#'
#' @param x A list of data frames
#'
#' @export
rbind_list_fill = function(x) {
  all_colnames = unique(unlist(lapply(x, names)))
  all = lapply(x, function(x) {
    x[setdiff(all_colnames, names(x))] <- NA
    x
  })
  do.call(rbind, all)
}
