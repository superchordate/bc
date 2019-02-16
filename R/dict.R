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

    # add row.
    x$bc.row = 1:nrow(x)

    # sample rows first so values are consistent across.
    splrows = sample( x$bc.row, size = min( 5, nrow(x) ), replace = FALSE )
    
    if( !is.null(run.on.sample) ) x = x[ sample( 1:nrow(x), size = ceiling( nrow(x) * run.on.sample ), replace = FALSE ), ]

    for( col in setdiff( colnames(x), 'bc.row' ) ){

        col.dict = data.frame( 
            name = col, 
            class = gsub( 'POSIXct', 'datetime(POSIXct)', class( x[[col]] )[1] ),
            rows = nrow(x),
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

        if( is.factor( x[[col]] ) ){
          
          if( do.droplevels ) x[[col]] = droplevels( x[[col]] )

          if( length(navals) > 0 ) {
            vals = x[ -navals, c( 'bc.row', col ) ]
          } else {
            vals = x[ , c( 'bc.row', col ) ]
          }
          
          valcnt = rev( sort( table( vals[[col]] ) ) )
          col.dict$mode = as.character( names( valcnt )[1] )
          col.dict$pareto80 = round( ( sum( cumsum(valcnt) < (nrow(vals) * 0.8) ) + 1 ) / length(valcnt), 2 )

          col.dict$n.vals = length( levels( x[[col]] ) )
            
          rm( valcnt )

        } else if( is.numeric( x[[col]] ) || lubridate::is.Date(x[[col]]) || is.POSIXct(x[[col]]) ){

            if( length(navals) > 0 ) {
              vals = x[ -navals, c( 'bc.row', col ) ]
            } else {
              vals = x[ , c( 'bc.row', col ) ]
            }
          
            vals = dplyr::arrange_at( vals, col )

            col.dict$n.vals = length( unique( vals[[col]] ) )
            col.dict$min = vals[[col]][1]
            col.dict$pct25 = vals[[col]][ ceiling( nrow(vals) * .25 ) ]
            col.dict$pct50 = vals[[col]][ ceiling( nrow(vals) * .5 ) ]
            col.dict$pct75 = vals[[col]][ ceiling( nrow(vals) * .75 ) ]
            col.dict$max = vals[[col]][ nrow(vals) ]
            
            if( is.numeric( x[[col]] ) ){
              
              iqr = col.dict$pct75 - col.dict$pct25
      
              outlier.rows = vals$bc.row[ which( vals[[col]] > col.dict$pct75 + iqr  ) ]
              col.dict$outlier.ct = length(outlier.rows)
              col.dict$outlier.pct = col.dict$outlier.ct / nrow(vals)
              if( length(outlier.rows) > 0 ) col.dict$outlier.rows = list(outlier.rows)

              rm(outlier.rows)

              col.dict$mean = mean(vals[[col]])
              col.dict$sd = sd(vals[[col]])
            }

        } else if( is.logical( x[[col]] ) ) {

          if( length(navals) > 0 ) {
            vals = x[ -navals, c( 'bc.row', col ) ]
          } else {
            vals = x[ , c( 'bc.row', col ) ]
          }
          
          col.dict$mean = mean(vals[[col]])
          col.dict$sd = sd(vals[[col]])
          
        }

        col.dict$na.ct = length(navals)
        col.dict$na.pct = length(navals) / nrow(vals)
        if( length(navals) > 0 ) col.dict$na.rows = list( navals )
        
        col.dict$sample = list( x[[col]][splrows] )

        for( i in colnames(col.dict) ) if( is.numeric( col.dict[[i]] ) ) col.dict[[i]] = signif( col.dict[[i]], 3 )
        rm(i)

        for( i in colnames(col.dict) ) col.dict[[i]] = as.character(col.dict[[i]])
        rm(i)

        dict = dplyr::bind_rows( dict, col.dict )

        rm( col.dict, col, navals  )

    }
    
    return(dict)

}