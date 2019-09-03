#' Output row-collapsed tables for printing
#'
#' escape takes a vector of chracter values, and puts "\\" in front of them to make them
#' allowable in the latex table output
#'
#' @param .data a data.frame
#' @param ... Variables to collapse rows by
#'
#' @return a data.frame where duplicated rows identified by ... are replaced with ""
#
#' @examples
#' sample_df<-data.frame(x=c(1,1,1,2,2,2,2),
#'                       y=c("test1","test1","test2","test1","test2","test2","test1"),
#'                       z=c(1,2,3,4,5,6,7),
#'                       outputVal=runif(7))
#'
#' group_row_collapse(sample_df,x,y,z)
#'
#' @export
group_row_collapse <- function(.data,...){

  stopifnot(is.data.frame(.data))

  fields<-as.character(as.list(substitute(substitute(...)))[-1])

  alt_fields<-setdiff(colnames(.data),fields)

  # make sure all column names passed exist
  stopifnot(all(fields%in%names(.data)))

  #sort row order of fields
  .data<-.data[eval(parse(text=paste0("with(.data,order(",paste(fields,collapse=","),"))"))),]

  #create "new" fields containing the result of collapsing rows
  for(i in 0:(length(fields)-1)){
    field_of_interest<-fields[length(fields)-i]
    new_field<-paste0(fields[length(fields)-i],"_disp")

    .data[[new_field]]<-as.character(.data[[field_of_interest]])
    .data[duplicated(.data[,fields[1:(length(fields)-i)]]),paste0(fields[length(fields)-i],"_disp")] <- ""
  }

  #drop "old" fields in favor of the "new" fields, and rename them
  .data<-.data[c(paste0(fields,"_disp"),alt_fields)]
  colnames(.data)<-c(fields,alt_fields)


  .data
}
