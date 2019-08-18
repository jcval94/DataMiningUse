run_all <- function(dir=getwd()) {
  file<-list.files(dir)
  file<-file[purrr::map_lgl(file,~substr(.x,nchar(.x)-1,nchar(.x))==".R")]
  purrr::map(file,~source(.x))
}
# run_all()
