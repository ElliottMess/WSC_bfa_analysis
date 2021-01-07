library(XLConnect)
library(stringr)
library(readr)
library(googlesheets4)
#library(RecordLinkage)
		
## aggregation / maths
# create a string to name files with date and time
humanTime <- function() {
	format(Sys.time(), "%Y%m%d-%H%M%OS")
}

# directory
check_create_directory<-function(subDir){
if (!file.exists(subDir)){
  dir.create(file.path(getwd(), subDir))
  }
}

# create uuid for the looped tab in kobo replace the parent indexby the uuid
## function to create parent uuid in child table
make_parent_uuid<-function(parent_table,parent_column_uuid="_uuid", child_table,child_parent_index="_parent_index",child_table_index="_index"){
  a <- parent_table[[parent_column_uuid]][match(child_table[[child_parent_index]],parent_table[[child_table_index]])]
  return(a)
}
# example 
# child_table$parent_uuid<-make_parent_uuid(hh,ind)

# aggregate indicator from loop 

## function create interval from numeric
age_groups<-function(x,age) {
  cut(coerc(x), age,include.lowest = TRUE, right = FALSE, ordered_result = FALSE)
}

# create parent level aggregated table
aggregate_to_parent_cat<-function(child,parent_uuid,variable){
	dcast(child, child[[parent_uuid]]~child[[variable]])
}

# aggregate number to the parent level
aggregate_numeric_to_parent<-function(parent, uuid="_uuid", child, parent_uuid="parent_uuid",variable,FUN){
# recalculate the quantities
	if(is.numeric(child[[variable]])){
		water_quant<-aggregate(child[[variable]], list(child[[parent_uuid]]), FUN,na.rm=T)
		names(water_quant)<-c("parent_uuid","water_quantitty")
		agg_table<- merge(parent,water_quant, by.x=uuid,by.y=parent_uuid,all.x=T)
		return(agg_table)
	} else {
		print("input variable should be numeric")
	}
}



# recode to binary
to_binary<-function(data,variable){
  with(data, model.matrix(formula(paste0("~",variable,"+0")))) %>% as.data.frame
}



	
# mode function
Mode <- function(x) {
	ux <- unique(x)
	ux[which.max(tabulate(match(x, ux)))]
}

############################################################################################
############################################################################################

# character operation
ch<-as.character
chr<-as.character

coerc<-function(x){as.numeric(chr(x))}

## create function to lower 
tlw<-function(x){ x  %>% apply(.,2,tolower) %>% as.data.frame}


proper<-function(s) sub("(.)", ("\\U\\1"), gsub("_"," ",s), pe=TRUE)

# add line break 
wrapit <- function(text,n=40) {
  wtext <- paste(strwrap(text,width=n),collapse="\n")
  return(wtext)
}


# recode missing values
rec_missing<-function(x,missings=c('N/A','n/a',999,888,' ','(vide)','d/m','','NA','na')) {
	x[x %in% missings] <- NA
	return(x)
}


# recode values 
recode_everywhere<-function(data,y,z){
	data <- data.frame(sapply(data, function(x) {x[x %in% y] <- z}),check.names=F)
	return(data)
}




r3c<-function(vec,name,label){
  name <-  name %>% ch
  vec <-  vec %>% ch
  label <-  label %>% ch
  
  if(length(name)==length(label)){
    for (i in 1:length(name)){
      cond<-which(vec%in%name[i])
      if(length(cond)>0){
        vec[cond]<-label[i]
      }
      if(length(grep(paste0(name[i], "\\."), vec)) > 0){
        vec[grep(paste0(name[i], "\\."), vec)] <- gsub(paste0(name[i], "\\."), paste0(label[i],"\\."), vec[grep(paste0(name[i], "\\."), vec)])
      }
    }
    return(vec)
  } else {
    print("y and z must have the length")
  }
}




# return fuzzy match
likely_match<-function(x,y){
		d<-levenshteinSim(x,y)
		matchs<-x[which(d==max(d))]
		return(matchs)
}


# Round values
rd<-function(x,digit) {round(x,digit)}

# sample size calculation
sample_size<-function (Population,c_lev,proport,error_marg) {
	(qchisq(c_lev,1)*Population*proport*(1-proport)) / (error_marg^2*(Population-1)+qchisq(c_lev,df=1)*proport*(1-proport))
  }



############################################################################################
############################################################################################
#############################################################################3
# format data 


# assess the data class
class_assess<-function(df){
  
  lapply(df,function(x){
		check_num<-length(na.omit(as.numeric(as.character(x))))
		check_log<-length(na.omit(as.logical(as.character(x))))
		check_str<-length(na.omit(as.character(x)))
		x<-rec_missing(x)
		
		index<-which(c(check_num,check_log,check_str)%in% max(check_num,check_log,check_str))
		 
		if(all(index==3)){
			 x<-as.factor(as.character(x))
			 class(x)<-"factor"
		} else if(all(c(2,3)%in%index)) {
			 x<-as.logical(as.character(x))
			 class(x)<-"logical"
		} else if(all(c(1,3)%in%index)) {
			 x<-as.numeric(as.character(x))
			 class(x)<-"numeric"
		} else {
			 x<-as.character(x)
			 class(x)<-"character"
		}
		return(x)
	}) %>% do.call(cbind,.)
}

# extract the vectors element ased on multiple pattern
grep_mutl<-function(x,to_match){
  unique (grep(paste(to_match,collapse="|"), 
             x, value=TRUE))
}




sanit_dt<-function(x){
	for (j in 1:ncol(x)){
		if(class(x[,j])!="numeric"){
			x[,j]<-iconv(x[,j], to="ASCII//TRANSLIT")
			x[,j]<-gsub("_"," ",x[,j])
			x[,j]<-proper(x[,j])
		}
	}
	return(x)
}

sanit_name<-function(x){
	names(x)<-iconv(names(x), to="ASCII//TRANSLIT")
	names(x)<-gsub("[()'?{}:&!@*$+%]","",names(x))
	names(x)<-gsub("[,:]","_",names(x))
	names(x)<-gsub("[/-]","_._",names(x))
	names(x)<-gsub("\\.*\\.", "_._", names(x))
	names(x)<-gsub("  ","_",names(x))
	names(x)<-gsub(" ","_",names(x))
	
	return(x)
}



sanit_data<-function(x){
	x%>% sanit_name %>% sapply(sanit_vector) %>% as.data.frame(check.names=F)  %>% class_assess
}


sanit_vector<-function(x){
	#x<-iconv(x, to="ASCII//TRANSLIT")
	x<-gsub("[()'?{}:&!@*+%]","",x)
	x<-gsub("[,:]","_",x)
	x<-gsub("[/-]","_._",x)
	x<-gsub(" ","_",x)
	x<-gsub("  ","_",x)
	
	return(x)
}



sanit_vec_all<-function(x){
	#x<-iconv(x, to="ASCII//TRANSLIT")
	x<-gsub("[()'?{}:&!@*+%]","",x)
	x<-gsub("[,:]","_",x)
	x<-gsub("[/-]","_",x)
	x<-gsub(" ","_",x)
	x<-gsub("__","_",x)
	return(x)
}




### cleaning of similar names
clean_similar_names<-function(hh,in_var,out_var,threshold=0.8){
  
  hh$enum<-hh[[in_var]]%>% as.character %>% sanit_vector %>% gsub("_$","",.) %>% tolower 
  enum<-hh$enum %>% unique
  
  # fuzzy match on enumertors names
  mat_enum<-lapply(enum,jarowinkler, str2=enum) %>%  lapply(as.numeric) %>% do.call(rbind,.) %>% as.data.frame
  names(mat_enum)<-enum
  
  # look at higher match
  mat_enum$id<-enum
  mat_enum<-mat_enum%>% melt(id="id")
  mat_enum<-mat_enum[
    mat_enum$value>threshold
                     &mat_enum$variable!="NA",]
  
  # use graph for similar names
  library(igraph)
  g<-graph_from_data_frame(mat_enum, directed = FALSE, vertices = NULL)
  dg <- decompose.graph(g)
  
  for ( i in 1:length(dg)){
    hh$enum<-r3c(hh$enum,V(dg[[i]])$name,V(dg[[i]])$name[1])
  }
  
  names(hh)[names(hh)=="enum"]<-out_var
  
  return(hh)
}




#####################################################
#####################################################
#####################################################
# color ramp

color_ramp2<-function(x,nb,col_ramp_typ,col_revert,def_col,smult,var_col){
	
	if(is.null(var_col)){
		if((!smult & nb=="yes")|is.na(col_ramp_typ)|col_ramp_typ=="NA"){
			nb_col<-1
		} else if((smult & nb=="no")|any(names(x)%in%"facet")){
			k<-1
			nb_col<-length(unique(x[,k]))
		}else{
			k<-2
			nb_col<-length(unique(x[,k]))
		}
	} else {
		nb_col<-length(unique(x[[var_col]]))
	}
	
	if(nb_col==1){
		color<-def_col
	} else {
			
			if(col_ramp_typ == "REACH red"){
				if(nb_col==2){color<-c("#A7A9AC","#D3CAB7")
				} else if (nb_col==1){color<-c("#F15B55")
				} else if (nb_col==3){color<-c("#F15B55","#D3CAB7","#A7A9AC")
				} else if (nb_col==4){color<-c("#F15B55","#D3CAB7","#C7C8CA","#A7A9AC")
				} else if (nb_col==5){color<-c("#F15B55","#F2A0A1","#D3CAB7","#C7C8CA","#A7A9AC")
				} else if (nb_col==6){color<-c("#F15B55","#F2A0A1","#D3CAB7","#E3E4E5","#C7C8CA","#A7A9AC","#505758")
				} else {color<-rep(c("#F15B55","#F2A0A1","#D3CAB7","#E3E4E5","#C7C8CA","#A7A9AC"),50)[1:nb_col]}
			
			
			} else if(col_ramp_typ == "REACH blue"){
				if(nb_col==2){color<-c("#667A95","#D3CAB7")
				} else if (nb_col==1){color<-c("#667A95")
				} else if (nb_col==3){color<-c("#667A95","#D3CAB7","#A7A9AC")
				} else if (nb_col==4){color<-c("#667A95","#D3CAB7","#C7C8CA","#A7A9AC")
				} else if (nb_col==5){color<-c("#667A95","#89A5C9","#D3CAB7","#C7C8CA","#A7A9AC")
				} else if (nb_col==6){color<-c("#667A95","#89A5C9","#D3CAB7","#E3E4E5","#C7C8CA","#A7A9AC","#505758")
				} else {color<-rep(c("#667A95","#89A5C9","#D3CAB7","#E3E4E5","#C7C8CA","#A7A9AC"),50)[1:nb_col]}
			
			} else {
				if(nb_col<=8){
					color<-brewer.pal(nb_col,col_ramp_typ)
				}else{
					color<-rep(brewer.pal(8,col_ramp_typ),50)[1:nb_col]
				}
			}
		
			if(col_revert==TRUE){
				color<-rev(color)
			}
	}
	color
}





color_ramp<-function(x,col_ramp_typ,col_revert=FALSE){
	nb_col<-length(unique(x))			
	
			if(col_ramp_typ == "REACH red"){
				if(nb_col==1){color<-c("#A7A9AC")
				} else if(nb_col==2){color<-c("#A7A9AC","#D3CAB7")
				} else if (nb_col==1){color<-c("#F15B55")
				} else if (nb_col==3){color<-c("#F15B55","#D3CAB7","#A7A9AC")
				} else if (nb_col==4){color<-c("#F15B55","#D3CAB7","#C7C8CA","#A7A9AC")
				} else if (nb_col==5){color<-c("#F15B55","#F2A0A1","#D3CAB7","#C7C8CA","#A7A9AC")
				} else if (nb_col==6){color<-c("#F15B55","#F2A0A1","#D3CAB7","#E3E4E5","#C7C8CA","#A7A9AC")
				} else {color<-rep(c("#F15B55","#F2A0A1","#D3CAB7","#E3E4E5","#C7C8CA","#A7A9AC"),50)[1:nb_col]}
			
			
			} else if(col_ramp_typ == "REACH blue"){
				if(nb_col==1){color<-c("#A7A9AC")
				} else if(nb_col==2){color<-c("#667A95","#D3CAB7")
				} else if (nb_col==1){color<-c("#667A95")
				} else if (nb_col==3){color<-c("#667A95","#D3CAB7","#A7A9AC")
				} else if (nb_col==4){color<-c("#667A95","#D3CAB7","#C7C8CA","#A7A9AC")
				} else if (nb_col==5){color<-c("#667A95","#89A5C9","#D3CAB7","#C7C8CA","#A7A9AC")
				} else if (nb_col==6){color<-c("#667A95","#89A5C9","#D3CAB7","#E3E4E5","#C7C8CA","#A7A9AC")
				} else {color<-rep(c("#667A95","#89A5C9","#D3CAB7","#E3E4E5","#C7C8CA","#A7A9AC"),50)[1:nb_col]}
			
			} else {
			  require(RColorBrewer)
				if(nb_col<=8){
					color<-brewer.pal(nb_col,"Blues")
				}else{
					color<-rep(brewer.pal(8,"Blues"),50)[1:nb_col]
				}
			}
		
			if(col_revert==TRUE){
				color<-rev(color)
			}

	return(color)
}





##############################################################################
##############################################################################
## graph


# theme "reach"
th<-function(font_family="Arial Narrow",fsize=8){
 theme_pander()+
		theme(

			legend.key.width = unit(2, "cm"),
			legend.key.height = unit(1, "cm"),
			legend.title=element_text(family=font_family,size=round(fsize*3.4,0),face="plain",color="#42423E"),
            legend.text=element_text(family=font_family,size=round(fsize*3.4,0),color="#42423E"),
			axis.text.x=element_blank(),
            axis.text.y=element_text(family=font_family,size=round(fsize*3.4,0),color="#42423E"),
			axis.title.x = element_blank(),axis.title.y = element_blank()
      )
}	  

	  

bar<-function(data,x,y=NULL){
	
	if(is.null(y)){
		g<-ggplot(data, aes(x = data[[x]]))
		g<-g+geom_bar()
	}else{
		g<-ggplot(data, aes(x = data[[x]],fill=data[[y]]))
		g<-g+geom_bar (position = "dodge")
		
	}
}



bar_value<-function(data,x,y=NULL,fill=NULL,order=FALSE,perc=FALSE,fsize=8){
  data[[x]]<-proper(data[[x]])
  data$value<-data[[y]]
  
  if(is.null(y) & is.null(fill)){
    g<-ggplot(data, aes(x = data[[x]]))
    g<-g+geom_bar()
	
	if(order==TRUE){
		ordeR<-data[with(data, order(value, data[[x]])), ][,1]
		g<-g+scale_x_discrete(limits=ordeR)
	}
  } else if (is.null(fill)) {	
	if(perc==TRUE){
		data[[y]]<-round(coerc(data[[y]])*100,1)
	}
	
	g<-ggplot(data, aes(x = data[[x]],y=data[[y]]))
	g<-g+geom_bar (stat = "identity")
	
	if(perc==TRUE){	
		g<-g+geom_text(aes(label=paste0(round(data[[y]],1),"%")),size=fsize,nudge_y = 4)
	}else{
		g<-g+geom_text(aes(label=round(data[[y]],2)),size=fsize,nudge_y = mean(data[[y]])*4/100)
	}
	
	if(order==TRUE & nrow(data)>1){
		ordeR<-data[with(data, order(value, data[[y]])), ][,1]
		g<-g+scale_x_discrete(limits=ordeR)
	}
	
  }else{	
	
	if(perc==TRUE){
		data[[y]]<-round(coerc(data[[y]])*100,1)
	}
	g<-ggplot(data, aes(x = data[[x]],y=data[[y]],fill=data[[fill]]))
	g<-g+geom_bar (stat = "identity",position = position_dodge(width = 0.9), alpha=.9)
	if(perc==TRUE){	
		g<-g+geom_text(aes(label=paste0(round(data[[y]],1),"%")),size=fsize, position = position_dodge(width = 0.75))
	}else{
		g<-g+geom_text(aes(label=round(data[[y]],2)),size=fsize, position = position_dodge(width = 0.75))
	}
	
	if(order==TRUE & nrow(data)>1){
		ordeR<-data[with(data, order(value, data[[y]])), ][,1]
		g<-g+scale_x_discrete(limits=ordeR)
	}
  }
  g<-g+ labs(x = x, y = y, colour = fill)
  g
}


## for ODK

# recode the daset with label values

rec_beta<-function(dataset,form,choices,language){
	
	 fb<-tstrsplit(names(dataset),"/",fill=NA)
	 fb<-data.frame(fb,stringsAsFactors=F)
	 namest<-ifelse(is.na(fb[,2]),fb[,1],fb[,2])
	 fb<-cbind(namest,fb)
	 names(fb)<-c("name","1","2","3")
	 m<-join(x=fb, y=form, by="name",type="left", match="all") 
	 nam_choix<-fb[,4]
	 nam_lab<-ifelse(is.na(m[[paste0('label::',language)]]),as.character(m$name),m[[paste0('label::',language)]])
	 
	 form<-sapply(form, as.character)
	 form<-as.data.frame(form,stringsAsFactors=F)
	 
	 s1<-form[form$q_type=="select_one",]

	for (kl in 1:nrow(s1)){
		choi<-s1$q_group[kl]
		choix<-choices[choices$list_name==choi&!is.na(choices$list_name),]
		ind<-which(str_detect(names(dataset),paste0(s1$name[kl],"$"))&!str_detect(names(dataset),"_other$"))
		
		if(length(ind)>1){print(names(dataset)[ind])}
		rec_var<-dataset[,ind]	
		for (reco in 1:length(choix[,1])){
			rec_var<-ifelse(dataset[,ind]==choix[reco,2],choix[reco,3],rec_var)
		}
		dataset[,ind]<-rec_var
	}


	smult<-form[form$q_type=="select_multiple",]
	m<-merge(smult,choices,by.x="q_group",by.y="list_name",all.x=TRUE)
	for (li in 1:nrow(m)){
			nam_choix<-ifelse(nam_choix==m[['name_choices']][li],m[[paste0('label::',language,'.y')]][li],nam_choix)
	}

	namesdb<-gsub("/NA","",paste0(nam_lab,"/",nam_choix))
	# matchcol<-cbind(names(expor),namesdb)
	# matchcol<-as.data.frame(matchcol)
	names(dataset)<-as.character(namesdb)
	dataset
	
}	

read_sheets_all <- function(fileurl,...){
  
  sheets <- googlesheets4::sheet_names(fileurl)
  
  df <- lapply(sheets, function(x) googlesheets4::read_sheet(fileurl, sheet = x))
  names(df) <- sheets
  df
  
}

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

aok_mode <- function(x) {
  ux <- unique(x[!is.na(x)])
  # This checks to see if we have more than one mode (a tie), return blank if so.
  if (length(which(tabulate(match(x, ux)) == max(tabulate(match(x, ux))))) > 1) {
    if (class(x) == "logical") {
      return(as.logical("")) # Blanks are coerced to values in logical vectors, so we specifically identify columns with TRUE/FALSE (KoBo's select multiples) and output a "logical" blank.
    }
    else {
      return("NC")
    }
  }
  
  else {
    ux[which.max(tabulate(match(x, ux)))] ## This occurs if no tie, so we return the value which has max value! Wambam.
  }
  
}

