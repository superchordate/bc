#' Variable selector.
#'
#' Select variables to be used in a model. Should be called after normalization and splitting categorical to boolean.
#'
#' @param x full data frame including y/outcome/response column.
#' @param ycol Character value, name of the y/outcome/response column in the data.
#' @param max.acceptable.pval Maximum acceptable p-value for your model.
#' @param verbose Print information about status.
#'
#' @return Data with only selected variables.
#' @export
#'
#' @examples
#' # TODO
vsel = function( x, ycol, max.acceptable.pval = .10, verbose = FALSE ){
  
  x = x[ , sapply( x, function(icol) length(unique(icol)) > 1 ) ]
  
  xcols = setdiff( colnames(x), ycol )
  colnames(x)[ colnames(x) == ycol ] <- 'ycol.vsel'
  picked.cols = data.frame( col = character(), adj.r.squared = numeric(), max.pval = numeric(), stringsAsFactors = FALSE )
  
  repeat{ 
    
    # Get info for all available columns.
    for( icol in xcols ){
      
        ilr = summary(
          lm( ycol.vsel ~ ., data = x[ , c( picked.cols$col, icol, 'ycol.vsel' ) ] )
        )
        
        icf = data.frame( ilr$coefficients )
        icf = icf[ rownames(icf) != '(Intercept)', ]
        
        # somtimes the coefficients don't actually include the column, which is a problem so don't add this to the options.
        if( length(icf) > 0 && icol %in% gsub( 'TRUE', '', rownames(icf) ) ){
          
          max.pval = max( icf[[ "Pr...t.." ]] )
          
          icol.info = data.frame( col = icol, adj.r.squared = ilr$adj.r.squared, max.pval = max.pval, stringsAsFactors = FALSE )
          
          if( exists( 'icols.info') ){ 
            icols.info = bind_rows( icols.info, icol.info ) 
          } else { 
            icols.info = icol.info
          }
          
          rm( max.pval, icol.info )
          
        }
        
        rm( ilr, icf )
        
    }
    
    # Select the best acceptable column.
    
      icols.info %<>% arrange( desc( adj.r.squared ) ) %>% filter( max.pval < max.acceptable.pval )
      
      if( nrow( icols.info ) == 0 ) break
      
      picked.cols = bind_rows( picked.cols, icols.info[ 1, ] )
      
      xcols = setdiff( xcols, icols.info$col[1] )
      
      if( length( xcols ) == 0 ) break
      
      rm( icols.info  )
      
      if( verbose ) cat( '\t [', length(xcols), '] columns left. Max p-value [', max( picked.cols$max.pval ), '] \n' )
    
  }
  
  # Remove with NA coeficients.
  acceptable.coeff = gsub( 'TRUE', '', 
    rownames( summary( lm( ycol.vsel ~ ., data = x[ , c( picked.cols$col, 'ycol.vsel' ) ] ) )$coefficients )
  )
  picked.cols = dplyr::filter( picked.cols, col %in% acceptable.coeff )
  
  return( picked.cols )
  
}
