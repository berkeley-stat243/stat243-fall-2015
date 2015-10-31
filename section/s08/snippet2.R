CountPattern=function(x){
  counts=numeric(13)
  names(counts)=c("I","we","America","democra","republic","Democrat",
                  "Republican","free","war","God","God bless","Jesus Christ","job")
  counts[1]=str_count(x,"I")
  counts[2]=str_count(x,"we|We")
  counts[3]=str_count(x,"America")
  counts[4]=str_count(x,"democra")
  counts[5]=str_count(x,"republic")
  counts[6]=str_count(x,"Democrat")
  counts[7]=str_count(x,"Republican")
  counts[8]=str_count(x,"free")
  counts[9]=str_count(x,"war")
  counts[10]=str_count(x,"God[^ bless]")
  counts[11]=str_count(x,"God Bless|God bless|god bless")
  counts[12]=str_count(x,"Jesus|Christ|Christian")
  counts[13]=str_count(x,"job|Job|jobs")   ##my additional pattern of interest
  return(counts)
}

