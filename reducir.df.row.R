reducir.df.row<-function(df,abs=25000,rel){
  if(max_na>1 | max_na<0){return(NULL)}
  if(nrow(df)>abs & missing(rel)){
    return(df[sample(seq_len(nrow(df)), size = abs),])
  }
  if(!missing(rel)){
    return(df[sample(seq_len(nrow(df)), size = floor(nrow(df)*rel)),])
  }
}
