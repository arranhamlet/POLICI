loc <- "/home/DIDE/ah1114/R/x86_64-pc-linux-gnu-library/3.2/"
.libPaths(c(.libPaths(), loc))

library(shiny)
library(htmltools)
library(shinyBS)
library(DT)
library(htmlwidgets)

# setwd("//fi--didenas1.dide.local/yf/Arran/Yellow fever shiny/Map generating code and data/Vaccination")
# source("data.R")
# source("functions.R")

Sys.setlocale('LC_ALL','C')

#Server
shinyServer(
  function(input, output, session){
    
    '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~OUTPUT MAPS~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'

    '~~~Country Specific Maps~~~'
    output$map<-renderLeaflet({res=300
    
    input$update_range
    age_vals<-isolate(input$age)
    
    country<-input$country
    shpfile<-endemiczone[endemiczone$NAME_0 %in% country,]
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Plotting",detail="Max 3 seconds", value = 1000)
    
    countryies<-endemiczone

    location<-input$country

    shpfile<-endemiczone[endemiczone$NAME_0 %in% location,]

    s<-as.numeric(input$table_rows_selected)

    z<-1

    rowselectvariable<-if(length(z)>length(s)) z else s
    choosefunct<-function(x){
      if(x %in% 1){
        "All"
      } else {
        if(x%in%2){
          "Average"
        } else {
          as.character(shpfile$NAME_1[x-2])
        }
      }
    }

    choosefunctendemic<-function(x){
      if(x %in% 1){
        "All"
      } else {
        if(x%in%2){
          "Average"
        } else {
          as.character(unique(shpfile$NAME_1)[x-2])
        }
      }
    }

    choosefunct.vec<-function(s){
      sapply(1:length(s),function(i) choosefunct(s[i]))
    }

    choosefunctendemic.vec<-function(s){
      sapply(1:length(s),function(i) choosefunctendemic(s[i]))
    }

    province<-if(length(shpfile$NAME_1) != length(endemiczonelowres$NAME_1)) choosefunct.vec(rowselectvariable) else choosefunctendemic.vec(rowselectvariable)

    polygonadd<-shpfile[shpfile$NAME_1 %in% province,]
    
    year<-input$year

    # if(length(polygonadd)!=0){
    # vaccov(country=country,year=year, min_age = input$age[1], max_age = input$age[2]) %>% addPolygons(data = polygonadd,fill = F, weight = 5, color = "#FF6347", group = "Outline",opacity=1)
    # } else vaccov(country = country, year = year, min_age = input$age[1], max_age = input$age[2]) 
    #   
    
    age_vals<-isolate(input$age)
    
    if(length(polygonadd)!=0){
      vaccov(country=country,year=year, min_age = input$age[1], max_age = input$age[2]) %>% addPolygons(data = polygonadd,fill = F, weight = 5, color = "#FF6347", group = "Outline",opacity=1)
    } else vaccov(country = country, year = year, min_age = age_vals[1], max_age = age_vals[2]) 
    
    
    
    })
    
    
    '~~~Endemic map~~~'
    output$endemicmap<-renderLeaflet({res=300
    
    input$update_range
    age_vals<-isolate(input$age2)
    
    country<-endemiczonelowres
    year<-input$year2
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Plotting",detail="Max 10 seconds", value = 1000)
  
    vaccov(country = country, year = year, min_age = age_vals[1], max_age = age_vals[2])
    })
    
    
    '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~OUTPUT GRAPHS~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'

    '~~~Plot barplot of vaccinated individuals~~~'
    output$plot<-renderPlotly({

    country<-input$country 
    year<-input$year
    s<-as.numeric(input$table_rows_selected)
    shpfile<-endemiczone[endemiczone$NAME_0 %in% country,]
    
    y<-if(length(1)>length(s)) NA else s
    choosefunct<-function(x){
      if(x %in% 1){
        "All"
      } else {
        if(x%in%2){
          "All"
        } else {
          as.character(shpfile$NAME_1[x-2])
        }
      }
    }
    
    choosefunct.vec<-function(s){
      sapply(1:length(s),function(i) choosefunct(s[i]))
    }
    
    province<-ifelse(is.na(y),"All",choosefunct.vec(y))

    comb_plot(country=country,year=year,province=province,print="no")
    
    })
    
    
    '~~~Output multiple lines graph~~~'
    output$legend<-renderPlotly({res=300
    country<-input$country  
    year<-input$year
    shpfile<-endemiczone[endemiczone$NAME_0 %in% country,]

    s<-1
    s<-as.numeric(input$table2_rows_selected)

    y<-if(length(1)>length(s)) NA else s

    choosefunct<-function(x){
      if(x %in% 1){
        "All"
      } else {
        if(x%in%2){
          "Average"
        } else {
          as.character(shpfile$NAME_1[x-2])
        }
      }
    }

    choosefunct.vec<-function(s){
      sapply(1:length(s),function(i) choosefunct(s[i]))
    }

    province<-ifelse(is.na(y), "Average" , choosefunct.vec(y))

    multiplelines(country=country,year=year,province=province, print="no")

    })
    

    

    '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~OUTPUT TABLES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'

    '~~~Output country specific table~~~'
    output$table<-DT::renderDataTable({
      
      input$update_range
      age_vals<-isolate(input$age)
      
      country<-input$country
      year<-input$year
      DT::datatable(rownames=TRUE,tabler(country, year, min_age = age_vals[1], max_age = age_vals[2]),options=list(pageLength=14,searching =FALSE,processing = FALSE),
                    colnames=c("ID","Province","Coverage (%)","Population"),#style='bootstrap',
                    selection=list(target='row'))
      
    })
    
    '~~~Output country specific table no age influence~~~'
    output$table2<-DT::renderDataTable({
      country<-input$country
      year<-input$year
      DT::datatable(rownames=TRUE,tabler(country, year),options=list(pageLength=14,searching =FALSE,processing = FALSE),
                    colnames=c("ID","Province","Coverage (%)","Population"),#style='bootstrap',
                    selection=list(target='row'))
      
    })


    '~~~Output endemic table~~~'
    output$endemictable<-DT::renderDataTable({
      input$update_range
      age_vals<-isolate(input$age2)
      
      country<-endemiczonelowres
      year<-input$year2
      DT::datatable(rownames=TRUE,tabler(country, year, min_age = age_vals[1], max_age = age_vals[2]),options=list(pageLength=14,searching =FALSE,processing = FALSE),
                    colnames=c("ID","Country","Coverage (%)","Population"),#style='bootstrap',
                    selection=list(target='row'))
      
    })

    
    '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~PROXY STUFF~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
    
    
    '~~~Observe events and make proxies country specific table~~~'
    proxy=dataTableProxy('table')

    observeEvent(input$resetSelection,{
      selectRows(proxy,NULL)
    })

    observeEvent(input$year,{
      selectRows(proxy,input$table_rows_selected)
    })
    
    observeEvent(input$age,{
      selectRows(proxy,input$table_rows_selected)
    })
    
    observeEvent(input$country,{
      output$table<-DT::renderDataTable({
        input$update_range
        age_vals<-isolate(input$age)
        country<-input$country
        year<-input$year
        DT::datatable(rownames=TRUE,tabler(country, year, min_age = age_vals[1], max_age = age_vals[2]),options=list(pageLength=14,searching =FALSE,processing = FALSE),
                      colnames=c("ID","Province","Coverage (%)","Population"),#style='bootstrap',
                      selection=list(target='row'))
        
      })})
    
    '~~~Table 2~~~'
    proxy3=dataTableProxy('table2')
    
    observeEvent(input$resetSelection,{
      selectRows(proxy3,NULL)
    })
    
    observeEvent(input$year,{
      selectRows(proxy3,input$table2_rows_selected)
    })
    
    observeEvent(input$country,{
      output$table3<-DT::renderDataTable({
        input$update_range
        age_vals<-isolate(input$age)
        country<-input$country
        year<-input$year
        DT::datatable(rownames=TRUE,tabler(country, year, min_age = age_vals[1], max_age = age_vals[2]),options=list(pageLength=14,searching =FALSE,processing = FALSE),
                      colnames=c("ID","Province","Coverage (%)","Population"),#style='bootstrap',
                      selection=list(target='row'))
        
      })})
    
    
    '~~~Observe events and make proxies endemic zone table~~~'
    proxy2=dataTableProxy('endemictable')

    observeEvent(input$resetSelection,{
      selectRows(proxy2,NULL)
    })

    observeEvent(input$year2,{
      selectRows(proxy2,input$endemictable_rows_selected)
    })
    
    observeEvent(input$age2,{
      selectRows(proxy2,input$endemictable_rows_selected)
    })
    

    '~~~Reset selection endemic map~~~'
    observeEvent(input$resetSelection,{
      selectRows(proxy2,NULL)
      output$endemicmap <- renderLeaflet({res=300
      input$update_range
      age_vals<-isolate(input$age2)
      
      country<-endemiczonelowres
      year<-input$year2
      vaccov(country=country, year=year, age_vals[1], age_vals[2])
      })
    })
    
    
    '~~~Reset selection country specific map~~~'
    # observeEvent(input$resetSelection,{
    #   selectRows(proxy,NULL)
    #   output$map <- renderLeaflet({res=300
    #   country<-input$country
    #   year<-input$year
    #   vaccov(country=country,year=year)
    #   })
    # })   
    
    
    '~~~Plot endemic map country outlines~~~'
    observeEvent(input$endemictable_rows_selected,{
      country<-countryoutlines
      shpfile<-endemiczonelowres
      
      s<-as.numeric(input$endemictable_rows_selected)
      
      z<-1
      
      rowselectvariable<-if(length(z)>length(s)) z else s
      choosefunct<-function(x){
        if(x %in% 1){
          "All"
        } else {
          if(x%in%2){
            "Average"
          } else {
            as.character(shpfile$NAME_1[x-2])
          }
        }
      }
      
      choosefunctendemic<-function(x){
        if(x %in% 1){
          "All"
        } else {
          if(x%in%2){
            "Average"
          } else {
            as.character(unique(shpfile$NAME_0)[x-2])
          }
        }
      }
      
      choosefunct.vec<-function(s){
        sapply(1:length(s),function(i) choosefunct(s[i]))
      }
      
      choosefunctendemic.vec<-function(s){
        sapply(1:length(s),function(i) choosefunctendemic(s[i]))
      }
      
      province<-if(length(shpfile$NAME_1) != length(endemiczonelowres$NAME_1)) choosefunct.vec(rowselectvariable) else choosefunctendemic.vec(rowselectvariable)
      
      polygonadd<-countryoutlines[countryoutlines$adm0 %in% province,]
      
      leafletProxy("endemicmap") %>% addPolygons(data = polygonadd,fill = F, weight = 5, color = "#FF6347", group = "Outline",opacity=1)
      
    })
    
    
    '~~~Plot  map country outlines~~~'
    observeEvent(input$resetSelection,{

      
      country<-input$country
      shpfile<-endemiczone[endemiczone$NAME_0 %in% country,]
      
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Plotting",detail="Max 3 seconds", value = 1000)
      
      countryies<-endemiczone
      
      location<-input$country
      
      shpfile<-endemiczone[endemiczone$NAME_0 %in% location,]
      
      s<-as.numeric(input$table_rows_selected)
      
      z<-1
      
      rowselectvariable<-if(length(z)>length(s)) z else s
      choosefunct<-function(x){
        if(x %in% 1){
          "All"
        } else {
          if(x%in%2){
            "Average"
          } else {
            as.character(shpfile$NAME_1[x-2])
          }
        }
      }
      
      choosefunctendemic<-function(x){
        if(x %in% 1){
          "All"
        } else {
          if(x%in%2){
            "Average"
          } else {
            as.character(unique(shpfile$NAME_1)[x-2])
          }
        }
      }
      
      choosefunct.vec<-function(s){
        sapply(1:length(s),function(i) choosefunct(s[i]))
      }
      
      choosefunctendemic.vec<-function(s){
        sapply(1:length(s),function(i) choosefunctendemic(s[i]))
      }
      
      province<-if(length(shpfile$NAME_1) != length(endemiczonelowres$NAME_1)) choosefunct.vec(rowselectvariable) else choosefunctendemic.vec(rowselectvariable)
      
      polygonadd<-shpfile[shpfile$NAME_1 %in% province,]
      
      year<-input$year
      
      if(length(polygonadd)!=0){
        vaccov(country=country,year=year) %>% addPolygons(data = polygonadd,fill = F, weight = 5, color = "#FF6347", group = "Outline",opacity=1)
      } else vaccov(country=country,year=year) 
      

    })

    
    
    '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~DOWNLOAD STUFF~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
    

    '~~~Download endemic map~~~'
    output$downloadmapendemic <- downloadHandler(
      filename = function() {
        age_vals<-isolate(input$age)
        paste("Endemic zone"," - ", "ages ", age_vals[1], " to ", age_vals[2], " (", input$year2, ').png', sep='')
      },
      content = function(file) {
        png(file,width=12,height=8.5,units='in',res=300,bg='white')
        age_vals<-isolate(input$age)
        par(mar=c(0,0,2,4),oma=c(0,0,2,4))
        vaccovprint(endemiczonelowres, input$year2, age_vals[1], age_vals[2])
        dev.off()
      }
    )
    
    

    '~~~Download country specific map~~~'
    output$downloadMap <- downloadHandler(
      filename = function() {
        age_vals<-isolate(input$age)
        paste(input$country," - ", "ages ", age_vals[1], " to ", age_vals[2], " (", input$year, ').png', sep='')
      },
      content = function(file) {
        png(file,width=8,height=8.5,units='in',res=300,bg='white')
        age_vals<-isolate(input$age)
        par(mar=c(0,0,2,4),oma=c(0,0,2,4))
        vaccovprint(input$country, input$year, age_vals[1], age_vals[2])
        dev.off()
      }
    )
    
    
    
    '~~~Download endemic zone country tables~~~'
    output$downloadendemictable<-downloadHandler(
      filename=function(){paste("Endemic zone",input$year2,'.csv',sep=' ')},
      content=function(file){
        country<-endemiczonelowres  
        year<-input$year2
        
        frameprint<-tabler(country, year, min_age = input$age2[1], max_age = input$age2[2])[-c(1,2),]
        frameprint$Map_ID<-(1:length(frameprint[,1]))+2
        
        write.csv(frameprint,file,row.names=FALSE)
      }
    )
    
    
    '~~~Download specific country tables~~~'
    output$downloadtable<-downloadHandler(
      filename=function(){paste(input$country," - ",input$year,'.csv',sep='')},
      content=function(file){
        country<-input$country
        year<-input$year

        frameprint<-tabler(country, year, min_age = input$age[1], max_age = input$age[2])[-c(1,2),]
        frameprint$Map_ID<-(1:length(frameprint[,1]))+2

        write.csv(frameprint,file,row.names=FALSE)
      }
    )
    
    output$downloadtable2<-downloadHandler(
      filename=function(){paste(input$country," - ",input$year,'.csv',sep='')},
      content=function(file){
        country<-input$country
        year<-input$year
        
        frameprint<-tabler(country, year)[-c(1,2),]
        frameprint$Map_ID<-(1:length(frameprint[,1]))+2
        
        write.csv(frameprint,file,row.names=FALSE)
      }
    )
    

    '~~~Download multiplelines~~~'
    output$downloadPlot<-downloadHandler(
      filename=function(){paste(input$country,input$year,'vaccination coverage.png',sep=' ')},
      content=function(file){
        country<-input$country     
        year<-input$year
        shpfile<-endemiczonelowres[endemiczonelowres$NAME_0 %in% country,]

        s<-1
        s<-input$table2_rows_selected
        s<-as.numeric(s)
        y<-if(length(1)>length(s)) NA else s
        choosefunct<-function(x){
          if(x %in% 1){
            "All"
          } else {
            if(x%in%2){
              "Average"
            } else {
              as.character(shpfile$NAME_1[x-2])
            }
          }
        }
        
        choosefunct.vec<-function(s){
          sapply(1:length(s),function(i) choosefunct(s[i]))
        }
        province<-ifelse(is.na(y),"Average",choosefunct.vec(y))
        
        png(file,width=14,height=8,units='in',res=300,bg="white")
        
        if(country %in% "Uganda") par(mar = c(2,2,2,2),oma = c(11,5,4,2),xpd=TRUE,bg="white") else par(mar = c(2,2,2,2),oma = c(8,5,4,2),xpd=TRUE,bg="white")
        plot(1,1,col="white",ylim=c(0,100),xlim=c(0,101),xaxs="i",yaxs="i",bty="n",bg="white",col.axis="black",
             col.lab="black",col.sub="black",yaxt="n",xaxt="n")
        axis(1, labels = FALSE, col = "black")
        axis(side=1,at=seq(0,101,10),col="black",col.axis="black")
        axis(side=1,at=seq(5,95,10),col="black",col.axis="black",cex.axis=0.75,cex.lab=0.75,tck=-0.005,mgp=c(1,0,0))
        axis(side=2,at=seq(0,100,10),col="black",col.axis="black")
        multiplelines(country,year,province,print="yes")
        mtext(paste(shpfile$NAME_0[1],"-",year),side=3,line=-2.5,outer=TRUE,cex=2.5)
        dev.off()
        
      }
    )
    

    
    '~~~Download barplot of vaccination~~~'
    output$downloadComb<-downloadHandler(
      filename=function(){paste(input$country,input$year,'vaccination proportion.png',sep=' ')},
      content=function(file){
        country<-input$country       
        year<-input$year
        shpfile<-endemiczonelowres[endemiczonelowres$NAME_0 %in% country,]
        
        s<-1
        s<-input$table_rows_selected
        s<-as.numeric(s)
        y<-if(length(1)>length(s)) NA else s
        choosefunct<-function(x){
          if(x %in% 1){
            "All"
          } else {
            if(x%in%2){
              "Average"
            } else {
              as.character(shpfile$NAME_1[x-2])
            }
          }
        }
        
        choosefunct.vec<-function(s){
          sapply(1:length(s),function(i) choosefunct(s[i]))
        }
        
        province<-ifelse(is.na(y),"All",choosefunct.vec(y))

        png(file,width=14,height=8,units='in',res=300,bg="white")
        comb_plot(country,year,province,"yes")        
        dev.off()

      }
    )
    
    # outputOptions(output, "downloadmapendemic", suspendWhenHidden=FALSE)
    # outputOptions(output, "downloadMap", suspendWhenHidden=FALSE)
    # outputOptions(output, "downloadendemictable", suspendWhenHidden=FALSE)
    # outputOptions(output, "downloadtable", suspendWhenHidden=FALSE)
    # outputOptions(output, "downloadPlot", suspendWhenHidden=FALSE)
    # outputOptions(output, "downloadComb", suspendWhenHidden=FALSE)

    
    '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~TOOLTIPS~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'

    #Tooltips
    #Year
    addPopover(session,"year","",content=paste0("Select year."),
               placement = "right",trigger="hover")
    
    addPopover(session,"year2","",content=paste0("Select year."),
               placement = "right",trigger="hover")
    
    #Age rane
    addPopover(session,"age","",content=paste0("Select age range and click update."),
               placement = "right",trigger="hover")
    
    addPopover(session,"age2","",content=paste0("Select age range and click update."),
               placement = "right",trigger="hover")
    

    #Country table
    addPopover(session,"table","",content=paste0("Select provinces to display"," on the map and in graphs."),
               placement = "bottom",trigger="hover",options=list(container = 'body',width=500))
    
    addPopover(session,"table2","",content=paste0("Select provinces to display"," on the map and in graphs."),
               placement = "bottom",trigger="hover",options=list(container = 'body',width=500))
    
    #Endemic zone table
    addPopover(session,"endemictable","",content=paste0("Select provinces to display"," on the map and in graphs.","</p><p> Due to memory limitations, the countries will not deselect unless the reset button is pressed."),
               placement = "bottom",trigger="hover",options=list(container = 'body',width=500))
    #Map
    addPopover(session, "map", "", content=paste0("This map shows vaccination coverage"," at the first administrative level."),
               placement = "right",trigger = "hover", options = NULL)
    #Barchart
    addPopover(session,"plot","",content=paste0("This graph shows the total population of each 5 year"," age band by vaccination status.","</p><p> Select provinces from table to display values."," By default the whole countries values are shown"),
               placement = "right",trigger="hover",options=NULL)
    #Multiple lines
    addPopover(session,"legend","",content=paste0("This graph shows the vaccination coverage across age.","</p><p> Select provinces from table to display values."," By default the country average is shown."),
               placement = "right",trigger="hover",options=NULL)
    
    addPopover(session, "endemicmap", "", content=paste0("This map shows vaccination coverage"," at the first administrative level across the endemic zone.",
                                                         "</p><p> Due to the size it may take around 10 seconds to load."),
               placement = "right",trigger = "hover", options = NULL)
    
    

    
  })





