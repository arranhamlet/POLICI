'~~~Finding Functions~~~'
#Select array functions
selectarray<-function(x){
  arraylist[[paste(gsub("[[:punct:]]", "", gsub(" ", "", ((x)), fixed = TRUE)),"array",sep="")]]
}

#Select year
metaselectarray<-function(x){
  df<-data.frame("year"=1940:2050,"column"=1:111)
  df$column[which(df$year %in% x)]
}

#Select age proportion
selectageprop<-function(x){
  ageproplist[[paste(gsub("[[:punct:]]", "", gsub(" ", "", ((x)), fixed = TRUE)),"ageprop",sep="")]]
}

#Select age population total
selectagepoptot<-function(x){
  agepoptotlist[[paste(gsub("[[:punct:]]", "", gsub("[[:punct:]]", "", gsub(" ", "", ((x)), fixed = TRUE))),"agepoptot",sep="")]]
}

#Select dataframe
selectdf<-function(x){
  words<-paste(gsub(" ", "", capitalize(tolower(x)), fixed = TRUE),"df",sep="")
  dflist[[words]]
}

#Select province
provinceselector<-function(x){
  var[x,]
}

'~~~Plotting Functions~~~'
vaccov<-function(country, year, min_age = 0, max_age = 100){
  
  #Data
  if(length(country) != length(endemiczone$NAME_0)){
    
    country<-endemiczone[endemiczone$NAME_0 %in% country,]
    # country<-country[order(country$ID_1), ]
    name<-as.character(country$NAME_0[1])
    dataframe<-selectdf(name)
    year1<-metaselectarray(year)
    year<-paste("X",year,sep="")
    var<- dataframe[,year]
    var<-var*100
    var<-round(var,1)
    array1<-selectagepoptot(name)
    var1<- array1[, year1, ]
    
    prop_vac_age<-selectarray(name)[ ,year1, (min_age+1):(max_age+1)]
    prop_vac_age[is.na(prop_vac_age)]<-0
    pop_age_age<-selectagepoptot(name)[ ,year1, (min_age+1):(max_age+1)]
    pop_age_age[is.na(pop_age_age)]<-0
    
    if(min_age != max_age){
      prop4_age<-rowSums(prop_vac_age*pop_age_age)/rowSums(pop_age_age)
    } else prop4_age<-(prop_vac_age*pop_age_age)/(pop_age_age)

    var1<- array1[,year1, (min_age+1):(max_age+1)]
    
    prop4_age<-round(prop4_age*100, 1)
    prop4_age[is.na(prop4_age)]<-0

    poptotal<-if(is.null(nrow(var1))) var1 else rowSums(var1,na.rm=TRUE)
    poptotal<-round_any(x = poptotal,1)
    popformat<-formatC(poptotal,format="d",big.mark=',')
    numpal<-colorNumeric("YlGn", 0:100, na.color = "#808080", alpha = FALSE)
    popup<-paste(paste(country$NAME_1,paste(prop4_age,"%",sep=""),sep=" - ")," - ",paste("Population",popformat,sep=" - "))
    countryID<-country$SP_ID_1

    #countryoutline<-countryoutlines[which(countryoutlines$adm0 %in% name),]

    leaflet(country) %>%
      addProviderTiles("Esri.WorldGrayCanvas", options = tileOptions(maxZoom=9)) %>%
      addPolygons(
        stroke=TRUE,fillOpacity = 1,smoothFactor=1,
        fillColor = ~numpal(prop4_age),col="black",
        weight=2,label=(popup)) %>%
      addLegend("bottomright", pal = numpal, values = 0:100,
                title = "Coverage (%)",
                opacity = 1,bins=10,layerId="map")

  } else {
    name<-"Endemic Zone"
    dataframe<-countryvacdf
    year1<-metaselectarray(year)
    year<-paste("X",year,sep="")
    var<- dataframe[,year]
    var<-var*100
    var<-round(var,1)
    array1<-pop3d
    var1<- array1[,year1,]
    var1[is.na(var1)]<-0
    
    prop_vac_age<-vc3d[, year1, (min_age+1):(max_age+1)]
    prop_vac_age[is.na(prop_vac_age)]<-0
    pop_age_age<-array1[ , year1, (min_age+1):(max_age+1)]
    pop_age_age[is.na(pop_age_age)]<-0
    
    if(min_age != max_age){
      prop4_age<-(rowSums(prop_vac_age*pop_age_age))/rowSums(pop_age_age)
    } else prop4_age<-prop_vac_age*pop_age_age/(pop_age_age)
    
    var1<- array1[,year1, (min_age+1):(max_age+1)]
    
    prop4_age<-round(prop4_age*100, 1)
    prop4_age[is.na(prop4_age)]<-0
    
    
    poptotal<-if(is.null(nrow(var1))) sum(var1, na.rm = TRUE) else rowSums(var1,na.rm=TRUE)
    poptotal<-round_any(x = poptotal,1)
    popformat<-formatC(poptotal,format="d",big.mark=',')
    numpal<-colorNumeric("YlGn", 0:100, na.color = "#808080", alpha = FALSE)
    popup<-paste(paste(country$NAME_0,country$NAME_1,paste(prop4_age,"%",sep=""),sep=" - ")," - ",paste("Population",popformat,sep=" - "))
    countryID<-country$SP_ID_1
    countryoutline<-countryoutlines
    leaflet(country) %>%
      addProviderTiles("Esri.WorldGrayCanvas", options = tileOptions(maxZoom = 5)) %>%
      addPolygons(
        stroke=TRUE,fillOpacity = 1,smoothFactor=1,
        fillColor = ~numpal(prop4_age),col="black",
        weight=2,label=popup) %>%
      addLegend("bottomright", pal = numpal, values = 0:100,
                title = "Coverage (%)",
                opacity = 1,bins=10)  %>%
      addPolygons(data = countryoutline,fill = F, weight = 4, color = "black", group = "Outline",opacity=1)

  }
  
}



vaccovprint<-function(country, year, min_age = 0, max_age = 100){
  
  if(length(country) != length(endemiczone$NAME_0)){
    #Data
    country<-endemiczone[endemiczone$NAME_0 %in% country,]
    name<-as.character(country$NAME_0[1])
    dataframe<-selectdf(name)
    main<-c(name,year)
    main<-paste0(main ,collapse=" - ")
    year1<-metaselectarray(year)
    year<-paste("X",year,sep="")
    var<- dataframe[,year] 
    var<-var*100
    var<-round(var,1)
    
    prop_vac_age<-selectarray(name)[ ,year1, (min_age+1):(max_age+1)]
    prop_vac_age[is.na(prop_vac_age)]<-0
    pop_age_age<-selectagepoptot(name)[ ,year1, (min_age+1):(max_age+1)]
    pop_age_age[is.na(pop_age_age)]<-0
    
    if(min_age != max_age){
      prop4_age<-rowSums(prop_vac_age*pop_age_age)/rowSums(pop_age_age)
    } else prop4_age<-(prop_vac_age*pop_age_age)/(pop_age_age)
    
    var1<- dataframe[,year1, (min_age+1):(max_age+1)]
    
    prop4_age<-round(prop4_age*100, 1)
    prop4_age[is.na(prop4_age)]<-0
    
    
    #Colors
    mycols<- (colorRampPalette(brewer.pal(9,"YlGn"))(100))
    mybreaks<- seq(0,100,length=100)
    vcols<- findInterval(prop4_age,mybreaks)
    # countryoutline<-countryoutlines[which(countryoutlines$adm0 %in% name),]
    #Plot
    # par(mar = c(0,0,4,10),oma = c(0,0,0,0),bg="white")
    plot(country,col=mycols[vcols],lty=1,asp=1.1,border="gray53",cex.main=2.5)
    # plot(countryoutline,border="black",add=TRUE,lwd=2)
    mtext(main,side=3,line=-2.5,outer=TRUE,cex=2.5)
    text(coordinates(country), label=c(1:length(country$ID))+2, cex=1.7)
    # shadowtext( coordinates(country)[,1],coordinates(country)[,2], label=c(1:length(country$SP_ID_1))+2, cex= 1, col="white" ,theta=seq(pi/4, 2 * pi, length.out = 50), r=0.3)
    
    
    par(xpd=NA)
    
    # getridofwater(name)
    mtext("Vaccination coverage (%)",side=4,cex=2,line=0.5,outer=TRUE,col="black")
    image.plot(col.sub="black",legend.only=T,breaks=mybreaks,col=mycols[0:99],zlim=c(0,100),cex=1.5,axis.args = list(cex.axis = 1.5),legend.width = 1.5,legend.shrink	=0.7)
    
  } else {
    
    name<-"Endemic Zone"
    dataframe<-countryvacdf
    main<-c(name,year)
    main<-paste0(main ,collapse=" - ")
    year1<-year-1940
    year<-paste("X",year,sep="")
    var<- dataframe[,year]
    var<-var*100
    var<-round(var,1)
    
    prop_vac_age<-vc3d[, year1, (min_age+1):(max_age+1)]
    prop_vac_age[is.na(prop_vac_age)]<-0
    pop_age_age<-pop3d[ , year1, (min_age+1):(max_age+1)]
    pop_age_age[is.na(pop_age_age)]<-0
    
    if(min_age != max_age){
      prop4_age<-(rowSums(prop_vac_age*pop_age_age))/rowSums(pop_age_age)
    } else prop4_age<-prop_vac_age*pop_age_age/(pop_age_age)
    
    prop4_age<-round(prop4_age*100, 1)
    prop4_age[is.na(prop4_age)]<-0
    
    mycols<- (colorRampPalette(brewer.pal(9,"YlGn"))(100))
    mybreaks<- seq(0,100,length=100)
    vcols<- findInterval(prop4_age,mybreaks)
    
    # par(mar = c(0,0,0,5),oma = c(0,0,0,5),bg="white")
    plot(country,col=mycols[vcols],lty=1,asp=1.1,border="gray53",cex.main=2.5)
    plot(countryoutlines,border="black",add=TRUE,lwd=2)
    mtext(main,side=3,line=-2.5,outer=TRUE,cex=2.5)
    text(coordinates(countryoutlines),label=3:36,cex=0.7,col="dodgerblue4")
    # shadowtext( coordinates(countryoutlines)[,1],coordinates(countryoutlines)[,2], label=3:36, cex= 0.5, col="white" ,theta=seq(pi/4, 2 * pi, length.out = 20), r=0.3)
    par(xpd=NA)
    
    mtext("Vaccination coverage (%)",side=4,cex=2,line=0.5,outer=TRUE,col="black")
    image.plot(col.sub="black",legend.only=T,breaks=mybreaks,col=mycols[0:99],zlim=c(0,100),cex=1.5,axis.args = list(cex.axis = 1.5),legend.width = 1.5,legend.shrink	=0.7)
  }
  
}


tabler<-function(country=da, year=ya, min_age = 0, max_age = 100){
  
  if(length(country) != length(endemiczonelowres$SP_ID)){
    
    name<-country
    
    country<-endemiczone[endemiczone$NAME_0 %in% country,]
    dataframe<-selectdf(name)
    array1<-selectagepoptot(name)
    
    year1<-metaselectarray(year)
    year<-paste("X",year,sep="")
    
    var1<- array1[,year1,]
    poptotal<-rowSums(var1,na.rm=TRUE)
    var<- dataframe[,year]
    var<-var*100
    var<-round(var,1)
    
    #Throw it in
    prop_vac_age<-selectarray(name)[ ,year1, (min_age+1):(max_age+1)]
    prop_vac_age[is.na(prop_vac_age)]<-0
    pop_age_age<-selectagepoptot(name)[ ,year1, (min_age+1):(max_age+1)]
    pop_age_age[is.na(pop_age_age)]<-0
    
    if(min_age != max_age){
      prop4_age<-rowSums(prop_vac_age*pop_age_age)/rowSums(pop_age_age)
    } else prop4_age<-(prop_vac_age*pop_age_age)/(pop_age_age)
    
    var1<- array1[,year1, (min_age+1):(max_age+1)]
    
    prop4_age<-round(prop4_age*100, 1)
    prop4_age[is.na(prop4_age)]<-0
    
    var<-prop4_age
    poptotal<-if(is.null(nrow(pop_age_age))) (pop_age_age) else rowSums(pop_age_age)
    
    length<-length(var)
    df2<-data.frame("admin_unit"=as.character(unlist(dataframe$provinces.NAME_1)),"Vaccination coverage"=as.factor(round(var,2)),
                    "Total population"=format(round(poptotal,0),big.mark=",",scientific=FALSE),row.names=NULL)
    row.names(df2)<-3:(length(df2$admin_unit)+2)
    df<-data.frame("admin_unit"=as.character(unlist(c("All","Average"))),"Vaccination coverage"=as.factor(c("-",round(mean(var),1))),
                   "Total population"=c(format(round(sum(poptotal),0),big.mark=",",scientific=FALSE),"-"),row.names = NULL)
    df<-rbind(df,df2)
    df$admin_unit<-as.character(unlist(df$admin_unit))
    if(df[3,1]%in%"Bandundu") df[5,1] = "Equateur" else df[5,1] = df[5,1]
    df<-data.frame(df,row.names = NULL)
    df
    
  } else {
    
    country<-endemiczonelowres
    name<-"Endemic Zone"
    dataframe<-countryvacdf
    year1<-metaselectarray(year)
    year<-paste("X",year,sep="")
    var<- dataframe[,year]
    # var<-var*100
    # var<-round(var,1)
    
    array1<-pop3d
    var1<- array1[,year1,]
    var1[is.na(var1)]<-0
    length<-length(var)
    
    
    #Throw it in
    prop_vac_age<-vc3d[, year1, (min_age+1):(max_age+1)]
    prop_vac_age[is.na(prop_vac_age)]<-0
    pop_age_age<-array1[ , year1, (min_age+1):(max_age+1)]
    pop_age_age[is.na(pop_age_age)]<-0
    
    # if(min_age != max_age){
    #   prop4_age<-(rowSums(prop_vac_age*pop_age_age))/rowSums(pop_age_age)
    # } else prop4_age<-sum(prop_vac_age*pop_age_age)/sum(pop_age_age)
    # 
    # var<- array1[,year1, (min_age+1):(max_age+1)]
    # 
    # prop4_age<-round(prop4_age*100, 3)
    
    vac_pop<-if(is.null(nrow(prop_vac_age))) prop_vac_age*pop_age_age else rowSums(prop_vac_age*pop_age_age)
    allISO<-unique(substr(names(vac_pop), 1, 3))
    
    countrypops<-if(is.null(nrow(prop_vac_age))) sapply(allISO, function(x) sum(pop_age_age[which(grepl(x, names(pop_age_age)))])) else sapply(allISO, function(x) sum(pop_age_age[which(grepl(x, row.names(pop_age_age))), ])) 
    
    vac_cov<-sapply(allISO, function(z) sum(vac_pop[grepl(z, names(vac_pop))])/sum(countrypops[grepl(z, names(countrypops))]))
    vac_cov<-round(vac_cov*100, 1)
    
    countrypropvacced100<-vac_cov
    countrypropvacced100[is.na(countrypropvacced100)]<-0
    countrypopulations<-round_any(countrypops, 1)
    # poptotal<-round_any(x = poptotal,100)
    
    # 
    # 
    # poptotal<-rowSums(var1,na.rm=TRUE)
    # poptotal<-round_any(x = poptotal,100)
    # 
    # popvacced<-var*poptotal
    
    
    # names(var)<-names(poptotal)
    # countryiso<-sub("_","",gsub('[[:digit:]]+', '',names(poptotal)))
    # names(poptotal)<-countryiso
    # names(popvacced)<-countryiso
    # 
    # countrypopulations<-tapply(poptotal, INDEX=countryiso,FUN=sum)
    # countryvaccination<-tapply(popvacced, INDEX=countryiso,FUN=sum)
    # countrypropvacced<-countryvaccination/countrypopulations
    # countrypropvaccedround<-round(countrypropvacced,3)
    # countrypropvacced100<-countrypropvaccedround*100
    # 
    
    df2<-data.frame("admin_unit"=as.character(unique(endemiczonelowres$NAME_0)),"Vaccination coverage"=as.numeric(countrypropvacced100),
                    "Total population"=as.character(format(round(countrypopulations,0),big.mark=",",scientific=FALSE),row.names=NULL),stringsAsFactors=FALSE)
    row.names(df2)<-3:(length(df2$admin_unit)+2)
    df<-data.frame("admin_unit"=as.character(unlist(c("All","Average"))),"Vaccination coverage"=(c("-",round(mean(countrypropvacced100),1))),
                   "Total population"=c(format(round(sum(countrypopulations),0),big.mark=",",scientific=FALSE),"-"),row.names = NULL,stringsAsFactors=FALSE)
    df3<-rbind(df,df2)
    df3
  }
}


comb_plot<-function(country,year,province,print){
  # pdf(NULL)
  options(scipen=999)
  
  country<-endemiczone[endemiczone$NAME_0 %in% country,]
  
  name<-as.character(country$NAME_0[1])
  main<-c(name,year)
  main<-paste0(main ,collapse=" - ")
  dataframe<-selectdf(name)
  array1<-selectagepoptot(name)
  year<-metaselectarray(year)
  mycols1<- (colorRampPalette(brewer.pal(3,"YlGn"))(3))
  palette(mycols1)
  array<-selectarray(name)
  
  provinceifier<-function(x){
    if(x%in%'All'){
      country$ID
    } else {
      if(x%in%'Average'){
        100
      } else {
        which(country$NAME_1 %in% x)
      }
    }
  }
  
  provinceselector<-function(x){
    if(x %in% 100){
      colMeans(array[(country$ID)-1,year,])
      # NA
    }
    else array[x,year,]
  }
  
  provinceselector2<-function(x){
    if(x %in% 100){
      colMeans(array1[(country$ID)-1,year,])
      # NA
    }
    else array1[x,year,]
  }
  
  province<-if(length(province)>1 && any(province%in%"All")) "All" else province
  part1<-na.omit(as.numeric(unlist(lapply(province,provinceifier))))
  part1 <- part1[!is.na(part1)]
  part1<-part1
  provincelist<-unlist(lapply(if(country$NAME_0[1] == "Equatorial Guinea") part1-1 else part1,provinceselector))
  provincelist<-if(length(provincelist)<1) rep(0,101) else provincelist
  provincemat<-matrix(provincelist,ncol=101,byrow=TRUE)
  provincemat[is.na(provincemat)]<-0
  var<- colMeans(provincemat)
  
  part2<-na.omit((as.numeric(unlist(lapply(province,provinceifier)))))
  part2<-part2[!is.na(part2)]
  part2<-part2
  
  provincelist1<-unlist(lapply(if(country$NAME_0[1] == "Equatorial Guinea") part2-1 else part2,provinceselector2))
  
  # provincelist1<-unlist(lapply(part2,provinceselector2))
  provincelist1<-if(length(provincelist1)<1) rep(0,101) else provincelist1
  provincemat1<-matrix(provincelist1,ncol=101,byrow=TRUE)
  provincemat1[is.na(provincemat1)]<-0
  var1<- colSums(provincemat1)
  
  xlab<-c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84",
          "85-89","90-94","95-99","100")
  varadd<-c(var1,NA,NA,NA,NA)
  vacadd<-c(var,NA,NA,NA,NA)
  
  varmax<-array1[1:length(country$SP_ID_1),101,]
  varaddmax<-c(sum(varmax))
  var5max<-matrix(c(varaddmax,NA,NA,NA,NA),ncol=5,byrow=TRUE)
  var5max[is.na(var5max)]<-0
  var5maxvec<-rowSums(var5max)
  agematrixmax<-matrix(var5max,nrow=21,ncol=5,byrow=TRUE)
  agematrix<-matrix(varadd,nrow=21,ncol=5,byrow=TRUE)
  vacmatrix<-matrix(vacadd,nrow=21,ncol=5,byrow=TRUE)
  fiveyearintervals<-rowSums(agematrix)
  fiveryearvac<-rowMeans(vacmatrix)
  vacced<-fiveyearintervals*(fiveryearvac)
  unvacced<-fiveyearintervals-vacced
  
  unvaccedround<-round_any(unvacced,1000)
  vaccedround<-round_any(vacced,1000)
  
  
  if(print=="no"){
  
  xlab<-base::factor(xlab,levels=xlab)
  p<-plot_ly(x=xlab,y=vaccedround,name="Vaccinated",type="bar",marker = list(color = toRGB("dodgerblue"))) 
  p2<-add_trace(p,x=xlab,y=unvaccedround,type="bar",name="Unvaccinated",marker = list(color = toRGB("tomato1")))
  p3 <- layout(p2, barmode = "stack")
  x <- list(
    title = "Age group"
  )
  y <- list(
    title = "Population"
  )
  dev.off()
  
  p3 %>%  layout(xaxis = x, yaxis = y)
  
  } else {
    
  # par(mar = c(2,2,0,2),oma = c(7,5,4,2),xpd=TRUE,bg="white")
  graphics::layout(matrix(c(1,1,1,1,2,2,
                  1,1,1,1,2,2,
                  1,1,1,1,1,1),nrow=3,byrow=TRUE))
  yaxlim<-round_any(colSums(matrix(c(unvacced,vacced),ncol=21,byrow = TRUE))[1],10000)
  x<-barplot(matrix(c(vacced, unvacced),ncol=21,byrow = TRUE),yaxs="i",cex.lab=2,yaxt="n",xlab="",col=c("dodgerblue", "tomato1"),bty="l",cex.main=2,cex.axis =1.5,cex.lab=1.5)
  # x<-barplot(matrix(c(unvacced,vacced),ncol=21,byrow = TRUE),ylim=c(0,yaxlim),yaxs="i",cex.lab=2,xlab="",col=c(mycols1[1],mycols1[3]),bty="l",cex.main=2,cex.axis =1.5,cex.lab=1.5)
  axis(1, at=x, labels=xlab,cex.axis =1.5,cex.lab=1.5,las=2)
  axis(2, at=seq(0,yaxlim,length.out=5),labels=seq(0,yaxlim,length.out=5),cex.axis =1.5,cex.lab=1.5)
  mtext("Population",side=2,line=1.5,outer=TRUE,cex=2)
  mtext("Age in 5 year groupings",side=1,line=4.5,outer=TRUE,cex=2)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n",xlab="",ylab="")
  legend("left",c("Vaccinated","Unvaccinated"),border=c("black","black"),pch=c(22),pt.bg=c("dodgerblue", "tomato1"),bty="n",cex=3.25,horiz = FALSE)
  mtext(main,side=3,line=0.5,outer=TRUE,cex=2.5)
  }

}




multiplelines<-function(country,year,province="Average",print){
  # pdf(NULL)
  
  country<-endemiczone[endemiczone$NAME_0 %in% country,]
  country<-country[order(country$SPID),]
  mycols<- c("#80B6D6","#E31E20","#97D176","#F78887","#F58724","#399F2F","#FDBA67","#569FA4","#A7D78C","#FE972A","#A89C6B","#CAB2D6","#DF9A89","#E94130","#F06F46","#3686BC","#74C05C","#FDA848","#A6CEE3","#F79C5D","#FE850B","#51AF42","#5B9EC9","#2E82AF","#7FBB98","#F16565","#719E4D","#E7955F","#EA4142","#D8A39A")
  
  provinceifier<-function(x){
    if(x%in%'All'){
      1:length(country$ID)
    } else {
      if(x%in%'Average'){
        100
      } else {
        which(country$NAME_1 %in% x)
      }
    }
  }
  
  provinceifier.vec<-function(x){
    sapply(1:length(x), function(i) provinceifier(x[i]))
  }
  
  provincenames<-function(x){
    if(x %in% "All"){
      as.character(country$NAME_1)
    } else x
  }
  
  provincenames.vec<-function(x){
    sapply(1:length(x),function(i) provincenames(x[i]))
  }
  
  provinceifier.vec<-function(x){
    sapply(1:length(x),function(i) provinceifier(x[i]))
  }
  
  provinceselector<-function(x){
    if(x %in% 100){
      colMeans(var[(country$ID)-1,])
    }
    else var[x,]
  }
  
  provinceselector.vec<-function(x){
    sapply(1:length(x),function(i) provinceselector)
  }
  
  name<-as.character(country$NAME_0[1])
  
  main<-c(name,year)
  main<-paste0(main ,collapse=" - ")
  array<-selectarray(name)
  yearchanged<-metaselectarray(year)
  var<- array[,yearchanged,]
  var<-var*100
  var[is.na(var)]<-0
  
  namesofprovinces<-as.character(unlist(((provincenames.vec(province)))))
  dfcol<-data.frame("Province"=iconv(namesofprovinces,to='ASCII//TRANSLIT'),"colors"=(mycols[1:(length(namesofprovinces))]))
  dfcol$Province<-as.character(dfcol$Province)
  dfcol$colors[1]<-as.character(dfcol$colors[1])
  dfcol$Province<-as.character(unlist(dfcol$Province))
  dfcol$colors<-as.character(unlist(dfcol$colors))
  dfcol<-dfcol[!duplicated(dfcol[,1]),]
  dfcol$lty<-rep(c(1),length(dfcol$Province))
  dfcol$colors[dfcol$Province %in% "Average"] <- "black"
  # dfcol$colors[dfcol$colors %in% NA] <- mycols[20-which(is.na(dfcol$colors))]
  dfcol$lty[dfcol$Province %in% "Average"] <- 1
  
  provincenumbers<-(province)
  step1<-(as.numeric(unlist(lapply(provincenumbers,provinceifier.vec))))
  
  provincelist<-unlist(lapply(step1,provinceselector))
  
  y<-data.frame("Province"=namesofprovinces,matrix(provincelist,ncol=101,byrow=TRUE))
  y$Province<-as.character(y$Province)
  y[is.na(y)]<-0
  
  # ploty(y)
  
  if(print=="no"){
  
    
  df2<-data.frame(
    x=rep(0:100,(length(country$ID_1)+1)),
    y=rep(NA,101*(length(unique(country$ID_1))+1)),
    z=as.factor(rep(c(as.character(country$NAME_1),"Average"),each=101)),
    a=1:((length(country$NAME_1)+1)*101)
  )  
  
  df2$b<-paste(df2$x,df2$z,sep="")
  
  
  df<-data.frame(
    x=rep(0:100,dim(y)[1]),
    y=round(c(t(y[,2:length(y)])),1),
    z=as.factor(rep(y$Province,each=101)),
    a=1:(length(y$Province)*101)
  )
  
  df$b<-paste(df$x,df$z,sep="")
  
  df3<-merge(df,df2,by="a",all=FALSE)  
  
  df4<-data.frame("x"=df3$x.x,"y"=df3$y.x,"z"=df3$z.x,"a"=df3$a,"b"=df3$b.x)
  df4[order(df4$a),]
  
  # df<-df[1:length(country$ID_0)*101,]
  
  
  df4<-df[!duplicated(df$b),]
  
  
  
  m = list(
    l = 50,
    r = 50,
    b = 50,
    t = 50,
    pad = 4
  )
 
    plot_ly(df4, x = df4$x, y = df4$y,color=df4$z) %>% add_lines(y=df4$y) %>%
    layout(xaxis = list(title="Age",range = c(0, 100)),yaxis = list(title="Vaccinated (%)",
                                                                    range = c(0, 100)),showlegend=TRUE,margin=m) 
  
  } else {
    
    ploty<-function(y){
      matplot(t(y[,2:length(y[1,2:length(y)])]),lty=dfcol$lty,type="l",col=dfcol$colors,lwd=3,ylim=c(0,100),
              bty="l",xlab="Age",ylab="Vaccine coverage (%)",xaxs="i",yaxs="i",add=TRUE)
      par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0), new=TRUE)
      plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
      legend(xpd=TRUE,"bottom",inset=c(0.0,+0.01),seg.len=1,legend=as.character(dfcol$Province),ncol=7,col=as.character(dfcol$colors),lwd=2,bty="n",horiz = FALSE)
    }
    
    ploty.vec<-function(y){
      sapply(1:length(y), function(i) ploty(y[i]))
    }
    
    par(mar = c(2,2,2,2),oma = c(10,4,1,1),xpd=TRUE,bg="white")
    plot(1,1,col="white",ylim=c(0,100),xlim=c(0,101),xaxs="i",yaxs="i",bty="n",bg="white",xaxt="n")
    axis(side=1,at = seq(0,100,5))
    mtext("Vaccination coverage",side=2,line=1,outer=TRUE,cex=2)
    mtext("Age",side=1,line=0.5,outer=TRUE,cex=2)  
    ploty(y)
    
    
    
  }
  
  
  
  
}


multiplelinesprint.plot<-function(country,year,province){
  if(is.na(province)){
    country<-endemiczone[endemiczone$NAME_0 %in% country,]
    
    name<-as.character(country$NAME_0[1])
    main<-c(name,year)
    main<-paste0(main ,collapse=" - ")
    
    par(mar = c(2,2,2,2),oma = c(10,4,1,1),xpd=TRUE,bg="white")
    plot(1,1,col="white",ylim=c(0,100),xlim=c(0,101),xaxs="i",yaxs="i",bty="n",bg="white")
    mtext("Vaccination coverage",side=2,line=1,outer=TRUE,cex=2)
    mtext("Age",side=1,line=-0.5,outer=TRUE,cex=2)
    mtext(main,side=3,line=-2.5,outer=TRUE,cex=2.5)
    
    
    
  } else {
    name<-as.character(country$NAME_0[1])
    main<-c(name,year)
    main<-paste0(main ,collapse=" - ")
    
    par(mar = c(2,2,2,2),oma = c(10,4,1,1),xpd=TRUE,bg="white")
    plot(1,1,col="white",ylim=c(0,100),xlim=c(0,101),xaxs="i",yaxs="i",bty="n",bg="white")
    mtext("Vaccination coverage",side=2,line=1,outer=TRUE,cex=2)
    mtext("Age",side=1,line=-0.5,outer=TRUE,cex=2)   
    multiplelinesprint(country,year,province)
    mtext(main,side=3,line=-2.5,outer=TRUE,cex=2.5)
    
  }}


nametofile<-function(country){
  endemiczone[endemiczone$NAME_0 %in% country,]
}

