#' Write CSV Shorthand
#' 
#' Easier write function for quickly outputting data. Uses my preferred defaults.
#'
#' @param x Data to write.
#' @param file Fielname, w will add .csv for you so you don't need to include it.
#'
#' @export
#'
#' @examples
#' w( cars, 'output/cars' )
w = function( x, file = 'out', row.names = FALSE, na = "", ... ){
    
    file = paste0( gsub( '[.]csv$', '', file ), '.csv' )
    
    write.csv( x = x, file = file, row.names = row.names, na = "" )

}