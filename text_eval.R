#Codigo como string

text_eval<-function(a){
  if(class(a)!="character"){return()}
  eval(parse(text = a))
}
