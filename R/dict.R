#' Data dictionary.
#' 
#' Get information about the columns in a dataset.
#'
#' @param x Data frame to summarize.
#' @param nastrings Strings to consider NA. Uses this package's defaults.
#' @param run.on.sample Optionally, sample data to make this run faster. Enter the % of rows to sample.
#' @param do.droplevels Unused factorlevels can cause problems with this. Choose to re-leveling all factors. This is slower but may resolve errors.
#'
#' @return Data frame with information for each column. 
#' Outliers use 1.5 IQR method. 
#' pareto80 is a measure of categorical concentration. It is the minimum % of categories that represent 80% of the rows. 80/20 rule applies if this is 0.2 for example. If it is 0.1, then 10% of categories make up 80% of rows. If columns are evenly distributed then it will be 0.8.
#' 
#' @export
#'
#' @examples
#' 
#' dict(cars)
#' 
dict = function( x, nastrings = bc::nastrings, run.on.sample = NULL, do.droplevels = FALSE ){

    dict = data.frame( 
      name = as.character( glue::glue( '{nrow(x)} obs. of {ncol(x)} variables' ) ),
      stringsAsFactors = FALSE
    )

    # sample rows first so values are consistent across.
    splrows = sample( 1:nrow(x), size = min( 5, nrow(x) ), replace = FALSE )
    
    if( !is.null(run.on.sample) ) x = x[ sample( 1:nrow(x), size = ceiling( nrow(x) * run.on.sample ), replace = FALSE ), ]

    for( col in colnames(x) ){

        col.dict = data.frame( 
            name = col, 
            class = gsub( 'POSIXct', 'datetime(POSIXct)', class( x[[col]] )[1] ),
            stringsAsFactors = FALSE
        )

        # factors are faster to analyze.
        if( is.character( x[[col]] ) ) x[[col]] = as.factor( x[[col]] )

        # replace na strings and relevel.
        if( is.factor( x[[col]] ) && !is.null(nastrings) ){
          
            newna = which( levels(x[[col]])[ x[[col]] ] %in% nastrings )
            
            if( length(newna) > 0 ){
                x[[col]][ newna ] <- NA
                x[[col]] = droplevels( x[[col]] )
            }
            
            rm(newna)
            
        }
        
        navals = which( is.na( x[[col]] ) )

        col.dict$unique = ifelse( !any( duplicated( x[[col]] ) ), 'unique', NA )
        col.dict$na.ct = ifelse( length(navals) == 0, as.numeric(NA), length(navals) )

        if( is.factor( x[[col]] ) ){
          
          if( do.droplevels ) x[[col]] = droplevels( x[[col]] )

          if( length(navals) > 0 ) {
            vals = x[[col]][ -navals ]
          } else {
            vals = x[[col]]
          }
          
          valcnt = rev( sort( table( vals ) ) )
          col.dict$mode = as.character( names( valcnt )[1] )
          col.dict$pareto80 = round( ( sum( cumsum(valcnt) < (length(vals) * 0.8) ) + 1 ) / length(valcnt), 2 )

          col.dict$n.vals = length( levels( x[[col]] ) )
            
          rm( valcnt )

        } else if( is.numeric( x[[col]] ) || lubridate::is.Date(x[[col]]) || is.POSIXct(x[[col]]) ){

            if( length(navals) > 0 ) {
              vals = sort( x[[col]][ -navals ] )
            } else {
              vals = sort( x[[col]] )
            }

            pct25 = vals[ ceiling( length(vals) * .25 ) ]
            pct50 = vals[ ceiling( length(vals) * .5 ) ]
            pct75 = vals[ ceiling( length(vals) * .75 ) ]

            col.dict$n.vals = length( unique( vals ) )
            col.dict$min = as.character( vals[1] )
            col.dict$pct25 = as.character( pct25 )
            col.dict$pct50 = as.character( pct50 )
            col.dict$pct75 = as.character( pct75 )
            col.dict$max = as.character( vals[ length(vals) ] )
            
            if( is.numeric( x[[col]] ) ){
              iqr = pct75 - pct25
              col.dict$num.outliers = sum( vals > pct75 + iqr )
              col.dict$mean = as.character( signif( mean(vals), 3 ) )
              col.dict$sd = as.character( signif( sd(vals), 3 ) )
            }

            rm( vals, pct25, pct50, pct75 )

        } else if( is.logical( x[[col]] ) ) {
          
          if( length(navals) > 0 ) {
            vals = x[[col]][ -navals ]
          } else {
            vals = x[[col]]
          }
          
          col.dict$mean = as.character( signif( mean(vals), 3 ) )
          col.dict$sd = as.character( signif( sd(vals), 3 ) )
          
        }
        
        col.dict$sample = paste0( as.character( x[[col]][splrows] ), collapse = '; ' )

        dict = dplyr::bind_rows( dict, col.dict )

        rm( col.dict, col, navals  )

    }
    
    return(dict)

}