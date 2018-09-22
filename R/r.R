r = function( 
  file, type = NULL,
  sheet = 1,
  na.strings = na.strings,
  skip.rows = 0
){
  
  # Determine type of file.
  if( is.null( type ) ) type = trimws( tolower( substring( stringr::str_extract( file, '[.].+$' ), 2 ) ) )
  
  if( type == 'xlsx' ) idt = read.xlsx( file, sheet = sheet )
  
  print(type)
  
}
