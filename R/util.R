#' Build a data.frame of neighbor points
#'
#' @param lon      A length-one numeric. A longitude.
#' @param lat      A length-one numeric. A latitude.
#' @param win_size A length-one integer. A window size.
#' @param pix_res  A length-two list of numeric. A pixel's spatial resolution
#' @return         A data.frame
#' @export
get_neighbors <- function(lon, lat, win_size, pix_res){
    lon_vec <- seq(from = lon - floor(abs(win_size/2)) * pix_res$x,
                   by = pix_res$x, length.out = win_size)
    lat_vec <- seq(from = lat - floor(abs(win_size/2)) * pix_res$y,
                   by = pix_res$y, length.out = win_size)
    points <- expand.grid(lon_vec, lat_vec, stringsAsFactors = FALSE) %>%
        dplyr::bind_cols(expand.grid(1:win_size, 1:win_size,
                                     stringsAsFactors = FALSE)) %>%
        within(id_rel <- paste(Var11, Var21, sep = "_"))
    names(points) <- c("lon", "lat", "x_pix", "y_pix", "id_pix")
    return(points)
}

#' Format the time series comming from WTSS
#'
#' @param ts_ls    A list of time series (from wtss::listTimeSeries)
#' @patam s_points A data.frame of sample points (from get_neighbors)
#' @return         A data.frame.
#' @export
process_ts <- function(ts_ls, s_points){
    names(ts_ls) <- s_points$id_pix
    lapply(names(ts_ls), function(x){
        ts_ls[[x]] %>% .[[1]] %>% .[["attributes"]] %>%
            as.data.frame(stringsAsFactors = FALSE) %>%
            dplyr::mutate(Date = as.Date(rownames(.)), id_pix = x) %>%
            return()
    }) %>%
        dplyr::bind_rows() %>%
        dplyr::left_join(s_points, by = "id_pix") %>%
        dplyr::select(id_pix, lon, lat, x_pix, y_pix, Date,
                      tidyselect::everything()) %>%
        return()
}


#' Convert time series to RasterBrick
#'
#' @param dataInput A data.frame of time series (from process_ts)
#' @return           A list of RasterBrick
#' @export
ts2raster <- function(dataInput){
    rb_dates <- unique(dataInput$Date)
    raster_ts <- lapply(rb_dates, function(x){
        dataInput %>%
            dplyr::filter(Date == x) %>%
            dplyr::select(-c(id_pix, lon, lat, Date)) %>%
            raster::rasterFromXYZ() %>%
            return()
    })
    names(raster_ts) <- rb_dates
    return(raster_ts)
}
