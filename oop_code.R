# Note: In this updated version, all the 
# calls to dplyr and tidyr functions use their
# standard evaluation versions


{
  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)
  }
  if(!require("tidyr")){
    install.packages("tidyr")
    library(tidyr)
  }
  if(!require("readr")){
    install.packages("readr")
    library(readr)
  }
  if(!require("magrittr")){
    install.packages("magrittr")
    library(magrittr)
  }
}

#constructor for a LongitudinalData instance
#
#function returns a LongitudinalData instance
#that holds both its data (in a data frame)
#and the number of subjects
make_LD<-function(x){
  if(!is.data.frame(x)){
    stop("x must be a data.frame")
  }
  numSubj<-x$id%>%unique%>%length
  out<-structure(list(data=x,numSubjects=numSubj))
  class(out)<-"LongitudinalData"
  return(invisible(out))
}

#print function for LongitudinalData instance
print.LongitudinalData<-function(x){
  cat("Longitudinal dataset with ",x$numSubjects," subjects","\n",sep="")
  return (invisible(x))
}

#check if parameter is of type LongitudinalData
is.longitudinal_data<-function(x){
  class(x)=="LongitudinalData"
}

#check if parameter is of type subject
is.subject<-function(x){
  class(x)=="subject"
}

#check if parameter is of type visit
is.visit<-function(x){
  class(x)=="visit"
}

#Summary function for subject
summary.subject<-function(x){
  data<-x$data%>%group_by_(~visit,~room)%>%summarize_(val= ~ mean(value))%>%spread_(key_col = 'room',value_col = 'val')%>%as.data.frame
  out<-structure(list(data=data,id=x$idNo))
  class(out)<-"summary_subject"
  return(invisible(out))
}

#print function for summary_subject
print.summary_subject<-function(x){
  cat("ID: ",x$id,"\n",sep="")
  print(x$data)
  return(invisible(x))
}

#Summary function for room
#
#captures data for a subject during a particular visit in a particular
#room
summary.room<-function(x){
  data<-x$data$value
  out<-structure(list(id=x$idNo,summaryData=summary(data)))
  class(out)<-"summary_room"
  return(invisible(out))
}

#print function for summary_room
print.summary_room<-function(x){
  cat("ID: ",x$id,"\n",sep="")
  print(x$summaryData)
  return(invisible(x))
}


#constructor for subject class taking params
#x (a LongitudinalData instance) and 
#n (an id number)
#
#the function returns a subject instance
#The subject instance holds both its
#underlying data in a dataframe
#and its id
subject<-function(x,n){
  if(!is.longitudinal_data(x)){
    stop("x must be of type LongitudinalData")
  }
  frame<-x$data
  subj<-frame%>%filter_(~ id %in% n)
  if(nrow(subj)==0){
    return (NULL)
  }
  id<-subj$id[1]
  out<-structure(list(data=subj,idNo=id))
  class(out)<-"subject"
  return (invisible(out))
}

#print funciton for subject
print.subject<-function(x){
  cat("Subject ID: ",x$idNo,"\n",sep="")
  return(invisible(x))
}

#print function for room
print.room<-function(x){
  cat("ID: ",x$idNo,"\n")
  cat("Visit: ",x$visitNo,"\n")
  cat("Room:",x$room,"\n")
}

#constructor for a visit instance
#
#The constructor takes as params subj (a subject instance)
#and visitNo (an integer in the range 0-2)
#and constructs a visit instance
visit<-function(subj,visitNo){
  if(!is.subject(subj)){
    stop("subj must be of type subject")
  }
  if(!(visitNo %in% 0:2)){
    stop("visitNo must be either 0, 1 or 2")
  }
  subjData<-subj$data
  data<-subjData%>%filter_(~ visit %in% visitNo)
  out<-structure(list(data=data,idNo=subj$idNo,visitNo=visitNo))
  class(out)<-"visit"
  return(invisible(out))
}

validRooms<-c("bedroom","living room","family room","study room","den",
              "tv room","office","hall","kitchen","dining room")

#constructor for room
#constructs a room instance from parameters vst (a visit instance)
#and whichRoom (a string representing a room)
room<-function(vst,whichRoom){
  if(!is.visit(vst)){
    stop("vst must be of type visit")
  }  
  if(!(length(whichRoom)==1&&is.character(whichRoom))){
    stop("second parameter is of invalid type")
  }
  if(!(whichRoom %in% validRooms)){
    stop("second parameter has invalid value")
  }
  vstData<-vst$data
  data<-vstData%>%filter_(~ room %in% whichRoom)
  out<-structure(list(data=data,idNo=vst$idNo,visitNo=vst$visitNo,room=whichRoom))
  class(out)<-"room"
  return(invisible(out))
}