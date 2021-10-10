#####   FILTRO Y ACOMODO DE LOS DATOS    ####

library(lubridate)
library(readxl)
#Buscar los datos 
#file.choose()

#Cambiar el path, de acuedo en donde este, esto lo obtienes
#con la file.choose()

path<-"C:\\Users\\KATY\\Desktop\\Leohdezj\\Servicio Social\\Datos"


#Leer los datos y guardarlos como un Dataframe
group_season<-function(data){
  data<-as.Date(data)
  n<-length(data)
  temp<-data.frame(Temporada=NULL)
  for(i in 1:n){
    temp1<-NA
    if((as.numeric(format(data[i],'%m'))==4)|
       (as.numeric(format(data[i],'%m'))==5)|
       (as.numeric(format(data[i],'%m'))==6)){
      temp1<-"Primavera"
    }
    if((as.numeric(format(data[i],'%m'))==7)|
       (as.numeric(format(data[i],'%m'))==8)|
       (as.numeric(format(data[i],'%m'))==9)){
      temp1<-"Verano"
    }
    if((as.numeric(format(data[i],'%m'))==10)|
       (as.numeric(format(data[i],'%m'))==11)|
       (as.numeric(format(data[i],'%m'))==12)){
      temp1<-"Otoño"
    }
    if((as.numeric(format(data[i],'%m'))==1)|
       (as.numeric(format(data[i],'%m'))==2)|
       (as.numeric(format(data[i],'%m'))==3)){
      temp1<-"Invierno"
    }
    temp<-rbind(temp,temp1)
  }
  return(temp)
}
p.value_tase<-function(tase,base,years){
  n<-length(years)*12
  temp<-numeric(n)
  for(i in 1:n){
    temp[i]<-base*(1+tase)^(-n+i)
  }
  return(temp)
}
group_data<-function(path,folder,file,type,start,end){
  data<-data.frame()
  for(i in start:end){
    temp<-read_excel(paste0(path,"\\",folder,"\\",i,file,".",type),col_types= "text",range='A2:N35')
    temp1<-temp[temp$ENTIDAD=='CIUDAD DE MÉXICO',2:13]
    temp1<-as.numeric(temp1)
    temp2<-temp[temp$ENTIDAD=='NUEVO LEÓN',2:13]
    temp2<-as.numeric(temp2)
    time<-c()
    for(j in 1:12){
      time[j]=paste0(i,"-",j,"-","1") 
    }
    temp<-data.frame(temp1,temp2,as.Date(time))
    data<-rbind(data,temp)
  }
  names(data)=c("CDMX","N.L", "Fecha")
  return(data)
}
filter_data<-function(path,folder,file,type,state){
  data<-data.frame()
  temp<-read_excel(paste0(path,"\\",folder,"\\",file,".",type))
  if(state=="Edo. México"){
    temp<-temp[temp$nombreoficial=='Madín, Méx.',c("almacenaactual","Datee")]
    data<-temp
    names(data)=c("Madín, Méx.","Fecha")
    
  }
  if(state=="Nuevo León"){
    temp0<-temp[temp$nombreoficial=='Cuchillo Solidaridad, N.L.',"almacenaactual"]
    temp1<-temp[temp$nombreoficial=='José López Portillo, N.L.',"almacenaactual"]
    temp2<-temp[temp$nombreoficial=='Rodrigo Gómez, N.L.',"almacenaactual"]
    time<-temp[temp$nombreoficial=='Rodrigo Gómez, N.L.',"Datee"]
    data<-data.frame(temp0,temp1,temp2,time)
    names(data)=c("Cuchillo Solidaridad, N.L.","José López Portillo, N.L.","Rodrigo Gómez, N.L.","Fecha")
  }
  return(data)
}
group_filter<-function(data,datum,years){
  temp<-data.frame(datum=NULL,Fecha=NULL)
  data<-data.frame(data,Year=as.numeric(format(data$Fecha,'%Y')),Month=as.numeric(format(data$Fecha,'%m')))
  for( i in years){
    for(j in 1:12){
      temp<-rbind(temp,c(mean(data[data$Year==i&data$Month==j,datum]),paste0(i,"-",j,"-",01)))
    }
  }
  
  temp<-data.frame(ifelse(temp[,1]=="NaN", NA, temp[,1]),Fecha=as.Date(temp[,2]))
  names(temp)<-c(datum, "Fecha")
  temp<-na.omit(temp)
  return(temp)
}
group_data.frame<-function(state){
  data<-data.frame()
  presipitation<-group_data(path,"Precipitacion","Precip","xls",1985,2019)
  presipitation<-rbind(presipitation,group_data(path,"Precipitacion","Precip","xlsx",2020,2021))
  
  max_temp<-group_data(path,"Temperatura\\Temperatura_Maxima_Excel","Tmax","xlsx",1985,2021)
  
  
  min_temp<-group_data(path,"Temperatura\\Temperatura_Minima_Excel","Tmin","xlsx",1985,2021)
  
  
  if(state=="CDMX"){
    data<-data.frame(min_temp$CDMX,
                     max_temp$CDMX,
                     presipitation$CDMX,
                     p.value_tase(0.001077326,24.40802227,1985:2021),
                     group_season(presipitation$Fecha),
                     presipitation$Fecha)
  }
  if(state=="N.L"){
    data<-data.frame(min_temp$N.L,
                     max_temp$N.L,
                     presipitation$N.L,
                     p.value_tase(0.001077326,30.37488973,1985:2021),
                     group_season(presipitation$Fecha),
                     presipitation$Fecha)
  }
  names(data)=c("Temp min","Temp max","Presipitación","Tarifa","Estación","Fecha")
  data<-na.omit(data)
  return(data)
}
group_dams<-function(state){
  temp<-group_data.frame(state)
  data<-data.frame()
  if(state=="CDMX"){
    data<-filter_data(path,"Presas","PresasEdoMex2011-2021","xlsx","Edo. México")
  }
  if(state=="N.L"){
    data<-filter_data(path,"Presas","PresasNuevoLeon2011-2021","xlsx","Nuevo León")
    data<-data.frame(rowSums(data[,1:3]),data[,4])
  }
  names(data)=c("Presa","Fecha")
  data<-group_filter(data,"Presa",2011:2021)
  data<-merge(x = temp, y = data, all = TRUE)
  data<-na.omit(data)
  data<-data.frame(data[,2:4],as.numeric(data[,7]),data[,5],data[,1])
  names(data)=c("Temp min","Temp max","Presipitación","Presa","Tarifa","Fecha")
  
  return(data)
  
}

CDMX<-group_data.frame("CDMX")
N.L<-group_data.frame("N.L")

CDMX.Dams<-group_dams("CDMX")
N.L.Dams<-group_dams("N.L")

#####      DISTRIBUCIÓN DE LOS DATOS  #####

library(stats) 
library(survival)
library(nortest)
library(univariateML)
library(tidyverse)
library(actuar)
library(vcd)
library(MASS)
#install.packages("grid")

# Se comparan ?nicamente las distribuciones con un dominio [0, +inf)
substring_data.frame<-function(data,column,row,string,start,correction){
  temp<-data
  for(i in 1:column){
    end<-str_locate(data[1:row,i], string)
    end<-end[1:row,1]-correction
    temp[1:row,i]<-substring(data[1:row,i],rep(start,row),end)
  }
  
  return(temp) 
}

distribution_comparison<-function(data,datum){
  temp<-data.frame()
  aic_comparison.tmin<- AIC(
    mlcauchy(data$`Temp min`),
    mlgumbel(data$`Temp min`),
    mllaplace(data$`Temp min`),
    #mllogis(data$`Temp min`),
    mlllogis(data$`Temp min`),
    #mllomax(data$`Temp min`),
    mlpareto(data$`Temp min`),
    #mlbeta(data$`Temp min`),
    #mlkumar(data$`Temp min`),
    #mllogitnorm(data$`Temp min`),
    mlunif(data$`Temp min`),
    mlpower(data$`Temp min`),
    #fitdistr(data$`Temp min`,"normal")$estimate,
    mlbetapr(data$`Temp min`),
    mlexp(data$`Temp min`),
    mlinvgamma(data$`Temp min`),
    #mlgamma(data$`Temp min`),
    mllnorm(data$`Temp min`),
    mlrayleigh(data$`Temp min`),
    mlinvgauss(data$`Temp min`),
    mlweibull(data$`Temp min`),
    mlinvweibull(data$`Temp min`),
    mllgamma(data$`Temp min`)
  )
  aic_comparison.tmin<-aic_comparison.tmin %>% rownames_to_column(var = "Distribución") %>% arrange(AIC)
  
  # Se comparan ?nicamente las distribuciones con un dominio [0, +inf)
  bic_comparison.tmin<- BIC(
    mlcauchy(data$`Temp min`),
    mlgumbel(data$`Temp min`),
    mllaplace(data$`Temp min`),
    #mllogis(data$`Temp min`),
    mlllogis(data$`Temp min`),
    #mllomax(data$`Temp min`),
    mlpareto(data$`Temp min`),
    #mlbeta(data$`Temp min`),
    #mlkumar(data$`Temp min`),
    #mllogitnorm(data$`Temp min`),
    mlunif(data$`Temp min`),
    mlpower(data$`Temp min`),
    #mlnorm(data$`Temp min`),
    mlbetapr(data$`Temp min`),
    mlexp(data$`Temp min`),
    mlinvgamma(data$`Temp min`),
    #mlgamma(data$`Temp min`),
    mllnorm(data$`Temp min`),
    mlrayleigh(data$`Temp min`),
    mlinvgauss(data$`Temp min`),
    mlweibull(data$`Temp min`),
    mlinvweibull(data$`Temp min`),
    mllgamma(data$`Temp min`)
  )
  bic_comparison.tmin<-bic_comparison.tmin %>% rownames_to_column(var = "Distribución") %>% arrange(BIC)
  
  aic_comparison.tmax<- AIC(
    mlcauchy(data$`Temp max`),
    mlgumbel(data$`Temp max`),
    mllaplace(data$`Temp max`),
    #mllogis(data$`Temp max`),
    mlllogis(data$`Temp max`),
    #mllomax(data$`Temp max`),
    mlpareto(data$`Temp max`),
    #mlbeta(data$`Temp max`),
    #mlkumar(data$`Temp max`),
    #mllogitnorm(data$`Temp max`),
    mlunif(data$`Temp max`),
    mlpower(data$`Temp max`),
    #mlnorm(data$`Temp max`),
    mlbetapr(data$`Temp max`),
    mlexp(data$`Temp max`),
    mlinvgamma(data$`Temp max`),
    #mlgamma(data$`Temp max`),
    mllnorm(data$`Temp max`),
    mlrayleigh(data$`Temp max`),
    mlinvgauss(data$`Temp max`),
    mlweibull(data$`Temp max`),
    mlinvweibull(data$`Temp max`),
    mllgamma(data$`Temp max`)
  )
  aic_comparison.tmax<-aic_comparison.tmax %>% rownames_to_column(var = "Distribución") %>% arrange(AIC)
  
  # Se comparan ?nicamente las distribuciones con un dominio [0, +inf)
  bic_comparison.tmax<- BIC(
    mlcauchy(data$`Temp max`),
    mlgumbel(data$`Temp max`),
    mllaplace(data$`Temp max`),
    #mllogis(data$`Temp max`),
    mlllogis(data$`Temp max`),
    #mllomax(data$`Temp max`),
    mlpareto(data$`Temp max`),
    #mlbeta(data$`Temp max`),
    #mlkumar(data$`Temp max`),
    #mllogitnorm(data$`Temp max`),
    mlunif(data$`Temp max`),
    mlpower(data$`Temp max`),
    #mlnorm(data$`Temp max`),
    mlbetapr(data$`Temp max`),
    mlexp(data$`Temp max`),
    mlinvgamma(data$`Temp max`),
    #mlgamma(data$`Temp max`),
    mllnorm(data$`Temp max`),
    mlrayleigh(data$`Temp max`),
    mlinvgauss(data$`Temp max`),
    mlweibull(data$`Temp max`),
    mlinvweibull(data$`Temp max`),
    mllgamma(data$`Temp max`)
  )
  bic_comparison.tmax<-bic_comparison.tmax %>% rownames_to_column(var = "Distribución") %>% arrange(BIC)
  
  aic_comparison.tarifas<- AIC(
    mlcauchy(data$Tarifa),
    mlgumbel(data$Tarifa),
    mllaplace(data$Tarifa),
    #mllogis(data$`Temp min`),
    mlllogis(data$Tarifa),
    #mllomax(data$`Temp min`),
    mlpareto(data$Tarifa),
    #mlbeta(data$`Temp min`),
    #mlkumar(data$`Temp min`),
    #mllogitnorm(data$`Temp min`),
    mlunif(data$Tarifa),
    mlpower(data$Tarifa),
    #fitdistr(data$`Temp min`,"normal")$estimate,
    mlbetapr(data$Tarifa),
    mlexp(data$Tarifa),
    mlinvgamma(data$Tarifa),
    #mlgamma(data$`Temp min`),
    mllnorm(data$Tarifa),
    mlrayleigh(data$Tarifa),
    mlinvgauss(data$Tarifa),
    mlweibull(data$Tarifa),
    mlinvweibull(data$Tarifa),
    mllgamma(data$Tarifa)
  )
  aic_comparison.tarifas<-aic_comparison.tarifas %>% rownames_to_column(var = "Distribución") %>% arrange(AIC)
  
  # Se comparan ?nicamente las distribuciones con un dominio [0, +inf)
  bic_comparison.tarifas<- BIC(
    mlcauchy(data$Tarifa),
    mlgumbel(data$Tarifa),
    mllaplace(data$Tarifa),
    #mllogis(data$`Temp min`),
    mlllogis(data$Tarifa),
    #mllomax(data$`Temp min`),
    mlpareto(data$Tarifa),
    #mlbeta(data$`Temp min`),
    #mlkumar(data$`Temp min`),
    #mllogitnorm(data$`Temp min`),
    mlunif(data$Tarifa),
    mlpower(data$Tarifa),
    #mlnorm(data$`Temp min`),
    mlbetapr(data$Tarifa),
    mlexp(data$Tarifa),
    mlinvgamma(data$Tarifa),
    #mlgamma(data$`Temp min`),
    mllnorm(data$Tarifa),
    mlrayleigh(data$Tarifa),
    mlinvgauss(data$Tarifa),
    mlweibull(data$Tarifa),
    mlinvweibull(data$Tarifa),
    mllgamma(data$Tarifa)
  )
  bic_comparison.tarifas<-bic_comparison.tarifas %>% rownames_to_column(var = "Distribución") %>% arrange(BIC)
  
  
  aic_comparison.p<- AIC(
    mlcauchy(data$`Presipitación`),
    mlgumbel(data$`Presipitación`),
    mllaplace(data$`Presipitación`),
    #mllogis(data$`Presipitaci?n`),
    #mlllogis(data$`Presipitaci?n`),
    #mllomax(data$`Presipitaci?n`),
    #mlpareto(data$`Presipitaci?n`),
    #mlbeta(data$`Presipitaci?n`),
    #mlkumar(data$`Presipitaci?n`),
    #mllogitnorm(data$`Presipitaci?n`),
    mlunif(data$`Presipitación`),
    mlpower(data$`Presipitación`),
    #mlnorm(data$`Presipitaci?n`),
    #mlbetapr(data$`Presipitaci?n`),
    mlexp(data$`Presipitación`)
    #mlinvgamma(data$`Presipitaci?n`),
    #mlgamma(data$`Presipitaci?n`),
    #mllnorm(data$`Presipitaci?n`),
    #mlrayleigh(data$`Presipitaci?n`),
    #mlinvgauss(data$`Presipitaci?n`),
    #mlweibull(data$`Presipitaci?n`),
    #mlinvweibull(data$`Presipitaci?n`),
    #mllgamma(data$`Presipitaci?n`)
  )
  aic_comparison.p<-aic_comparison.p %>% rownames_to_column(var = "Distribución") %>% arrange(AIC)
  
  # Se comparan ?nicamente las distribuciones con un dominio [0, +inf)
  bic_comparison.p<- BIC(
    mlcauchy(data$`Presipitación`),
    mlgumbel(data$`Presipitación`),
    mllaplace(data$`Presipitación`),
    #mllogis(data$`Presipitaci?n`),
    #mlllogis(data$`Presipitaci?n`),
    #mllomax(data$`Presipitaci?n`),
    #mlpareto(data$`Presipitaci?n`),
    #mlbeta(data$`Presipitaci?n`),
    #mlkumar(data$`Presipitaci?n`),
    #mllogitnorm(data$`Presipitaci?n`),
    mlunif(data$`Presipitación`),
    mlpower(data$`Presipitación`),
    #mlnorm(data$`Presipitaci?n`),
    #mlbetapr(data$`Presipitaci?n`),
    mlexp(data$`Presipitación`)
    #mlinvgamma(data$`Presipitaci?n`),
    #mlgamma(data$`Presipitaci?n`),
    #mllnorm(data$`Presipitaci?n`),
    #mlrayleigh(data$`Presipitaci?n`),
    #mlinvgauss(data$`Presipitaci?n`),
    #mlweibull(data$`Presipitaci?n`),
    #mlinvweibull(data$`Presipitaci?n`),
    #mllgamma(data$`Presipitaci?n`)
  )
  bic_comparison.p<-bic_comparison.p %>% rownames_to_column(var = "Distribución") %>% arrange(BIC)
  
  if(datum=="Temp min"){
    temp<-data.frame(aic_comparison.tmin[1:5,1],bic_comparison.tmin[1:5,1])
    names(temp)=c("Temp min AIC","Temp min BIC")
    
  }
  
  if(datum=="Temp max"){
    temp<-data.frame(aic_comparison.tmax[1:5,1],bic_comparison.tmax[1:5,1])
    names(temp)=c("Temp max AIC","Temp max BIC")
    
  }
  
  if(datum=="Presipitación"){
    temp<-data.frame(aic_comparison.p[1:5,1],bic_comparison.p[1:5,1])
    names(temp)=c("Presipitación AIC","Presipitación BIC")
    
  }
  if(datum=="All"){
    temp
    temp<-data.frame(merge(aic_comparison.tmin,bic_comparison.tmin,by="Distribución",sort=F)[1:6,1],
                     merge(aic_comparison.tmax,bic_comparison.tmax,by="Distribución",sort=F)[1:6,1],
                     merge(aic_comparison.tarifas,bic_comparison.tarifas,by="Distribución",sort=F)[1:6,1],
                     merge(aic_comparison.p,bic_comparison.p,by="Distribución",sort=F)[1:6,1])
    names(temp)=c("Temp min",
                  "Temp max",
                  "Tarifa",
                  "Presipitación")
    
  }
  if(datum=="Presa"){
    aic_comparison.presa<- AIC(
      mlcauchy(data$Presa),
      mlgumbel(data$Presa),
      mllaplace(data$Presa),
      #mllogis(data$`Temp min`),
      mlllogis(data$Presa),
      #mllomax(data$`Temp min`),
      mlpareto(data$Presa),
      #mlbeta(data$`Temp min`),
      #mlkumar(data$`Temp min`),
      #mllogitnorm(data$`Temp min`),
      mlunif(data$Presa),
      mlpower(data$Presa),
      #fitdistr(data$`Temp min`,"normal")$estimate,
      mlbetapr(data$Presa),
      mlexp(data$Presa),
      mlinvgamma(data$Presa),
      #mlgamma(data$`Temp min`),
      mllnorm(data$Presa),
      mlrayleigh(data$Presa),
      mlinvgauss(data$Presa),
      mlweibull(data$Presa),
      mlinvweibull(data$Presa),
      mllgamma(data$Presa)
    )
    aic_comparison.presa<-aic_comparison.presa %>% rownames_to_column(var = "Distribución") %>% arrange(AIC)
    
    # Se comparan ?nicamente las distribuciones con un dominio [0, +inf)
    bic_comparison.presa<- BIC(
      mlcauchy(data$Presa),
      mlgumbel(data$Presa),
      mllaplace(data$Presa),
      #mllogis(data$`Temp min`),
      mlllogis(data$Presa),
      #mllomax(data$`Temp min`),
      mlpareto(data$Presa),
      #mlbeta(data$`Temp min`),
      #mlkumar(data$`Temp min`),
      #mllogitnorm(data$`Temp min`),
      mlunif(data$Presa),
      mlpower(data$Presa),
      #mlnorm(data$`Temp min`),
      mlbetapr(data$Presa),
      mlexp(data$Presa),
      mlinvgamma(data$Presa),
      #mlgamma(data$`Temp min`),
      mllnorm(data$Presa),
      mlrayleigh(data$Presa),
      mlinvgauss(data$Presa),
      mlweibull(data$Presa),
      mlinvweibull(data$Presa),
      mllgamma(data$Presa)
    )
    bic_comparison.presa<-bic_comparison.presa %>% rownames_to_column(var = "Distribución") %>% arrange(BIC)
    
    
    temp
    temp<-data.frame(merge(aic_comparison.tmin,bic_comparison.tmin,by="Distribución",sort=F)[1:6,1],
                     merge(aic_comparison.tmax,bic_comparison.tmax,by="Distribución",sort=F)[1:6,1],
                     merge(aic_comparison.tarifas,bic_comparison.tarifas,by="Distribución",sort=F)[1:6,1],
                     merge(aic_comparison.presa,bic_comparison.presa,by="Distribución",sort=F)[1:6,1],
                     merge(aic_comparison.p,bic_comparison.p,by="Distribución",sort=F)[1:6,1])
    names(temp)=c("Temp min",
                  "Temp max",
                  "Tarifa",
                  "Presa",
                  "Presipitación")
    
  }
  temp<-substring_data.frame(temp,dim(temp)[2],dim(temp)[1],"(data)",3,2)
  return(temp)
}

distr_test<-function(data,datum,distr){
  data_end<-data.frame(Indice=1:6) 
  for(j in datum){
    temp_data<-data.frame(Data=numeric(0),Distribución=numeric(0),
                          KS=numeric(0),Test.KS=numeric(0))
    
    for(i in 1:dim(distr)[1]){
      
      Ks<-0
      
      p.v_Ks<-0
      
      test_Ks<-FALSE
      
      
      if(distr[i,j]=="exp"){
        temp<- fitdistr(data[,j], "exponential")
        Ks<- ks.test(data[,j], "pexp", rate=temp$estimate[1])
        p.v_Ks<-Ks$p.value
 
      }
      if(distr[i,j]=="weibull"){
        Ks<- ks.test(data[,j], mlweibull(data[,j]))
        p.v_Ks<-Ks$p.value
        
      }
      if(distr[i,j]=="normal"){
        temp<-fitdistr(data[,j], "normal")
        Ks<- ks.test(data[,j], "pnorm", mean=temp$estimate[1], sd=temp$estimate[2])
        p.v_Ks<-Ks$p.value
        
        
      }
      if(distr[i,j]=="cauchy"){
        Ks<- ks.test(data[,j],mlcauchy(data[,j]))
        p.v_Ks<-Ks$p.value
        
      }
      if(distr[i,j]=="gumbel"){
        Ks<- ks.test(data[,j],mlgumbel(data[,j])) 
        p.v_Ks<-Ks$p.value
      }
      if(distr[i,j]=="laplace"){
        Ks<- ks.test(data[,j],mllaplace(data[,j])) 
        p.v_Ks<-Ks$p.value
      }
      if(distr[i,j]=="llogis"){
        Ks<- ks.test(data[,j],mlllogis(data[,j])) 
        p.v_Ks<-Ks$p.value
      }
      if(distr[i,j]=="pareto"){
        Ks<- ks.test(data[,j],mlpareto(data[,j])) 
        p.v_Ks<-Ks$p.value
      }
      if(distr[i,j]=="unif"){
        Ks<- ks.test(data[,j],mlunif(data[,j])) 
        p.v_Ks<-Ks$p.value
      }
      if(distr[i,j]=="power"){
        Ks<- ks.test(data[,j],mlpower(data[,j])) 
        p.v_Ks<-Ks$p.value
      }
      if(distr[i,j]=="betapr"){
        Ks<- ks.test(data[,j],mlbetapr(data[,j])) 
        p.v_Ks<-Ks$p.value
      }
      if(distr[i,j]=="invgamma"){
        Ks<- ks.test(data[,j],mlinvgamma(data[,j])) 
        p.v_Ks<-Ks$p.value
      }
      if(distr[i,j]=="lnorm"){
        Ks<- ks.test(data[,j],mllnorm(data[,j])) 
        p.v_Ks<-Ks$p.value
      }
      if(distr[i,j]=="rayleigh"){
        Ks<- ks.test(data[,j],mlrayleigh(data[,j])) 
        p.v_Ks<-Ks$p.value
      }
      if(distr[i,j]=="invgauss"){
        Ks<- ks.test(data[,j],mlinvgauss(data[,j])) 
        p.v_Ks<-Ks$p.value
      }
      if(distr[i,j]=="invweibull"){
        Ks<- ks.test(data[,j],mlinvweibull(data[,j])) 
        p.v_Ks<-Ks$p.value
      }
      if(distr[i,j]=="lgamma"){
        Ks<- ks.test(data[,j],mllgamma(data[,j])) 
        p.v_Ks<-Ks$p.value
      }
      if(distr[i,j]=="mllnorm"){
        Ks<- ks.test(data[,j],mllnorm(data[,j])) 
        p.v_Ks<-Ks$p.value
      }
      
      if(p.v_Ks>0.05){
        test_Ks<-TRUE
      }
      
      temp_data<-rbind(temp_data,c(j,distr[i,j],p.v_Ks,test_Ks))
    }
    
    data_end<-cbind(data_end,temp_data)
    
  }
  data_end<-data_end[,2:dim(data_end)[2]]
  name<-c("Data","Distribución", "p.value K.S", "Test")
  temp<-c()
  for(i in 1:length(datum)){
    temp<-c(temp,name)
  }
  name<-temp
  
  names(data_end)=name
  return(data_end) 
}

distr_season<-function(data){
  temp<-data.frame(Estación=rep("Invierno",6),distr_test(data[data$Estación=="Invierno",],c("Temp min","Temp max","Tarifa","Presipitación"),distribution_comparison(data[data$Estación=="Invierno",],"All")))
  season<-c("Otoño","Primavera","Verano")
  for(i in season){
    temp<-rbind(temp,
                data.frame(Estación=rep(i,6),
                           distr_test(data[data$Estación==i,],c("Temp min","Temp max","Tarifa","Presipitación"),distribution_comparison(data[data$Estación==i,],"All")
                                      )))
  }
  names<-c()
  for( i in 1:4){
    names<-c(names,c("Data","Distribución", "p.value K.S", "Test"))
    
  }
  names(temp)<-c("Estación", names)
  return(temp)
  
  
}

d.CDMX<-distr_test(CDMX,c("Temp min","Temp max","Tarifa","Presipitación"),distribution_comparison(CDMX,"All"))

d.N.L<-distr_test(N.L,c("Temp min","Temp max","Tarifa","Presipitación"),distribution_comparison(N.L,"All"))

d.CDMX.Dams<-distr_test(CDMX.Dams,c("Temp min","Temp max","Tarifa","Presa","Presipitación"),distribution_comparison(CDMX.Dams,"Presa"))

d.N.L.Dams<-distr_test(N.L.Dams,c("Temp min","Temp max","Tarifa","Presa","Presipitación"),distribution_comparison(N.L.Dams,"Presa"))

d.S.CDMX<-distr_season(CDMX)
d.S.N.L<-distr_season(N.L)

#####      GRAFICAS DE LOS DATOS      ####
library(ggplot2)
library(univariateML)

## CDMX ##

p.CDMX.Tm<-ggplot(data = CDMX) +
  geom_histogram(aes(x =`Temp min`, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = `Temp min`)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mllaplace(CDMX$`Temp min`))},
                aes(color = "Laplace"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlinvgauss(CDMX$`Temp min`))},
                aes(color = "Inv Gauss"),
                size = 1.1) +
  labs(title = "Distribución Temp Min CDMX",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")

p.CDMX.TM<-ggplot(data = CDMX) +
  geom_histogram(aes(x =`Temp max`, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = `Temp max`)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlinvgauss(CDMX$`Temp max`))},
                aes(color = "Inv Gauss"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlgumbel(CDMX$`Temp max`))},
                aes(color = "Gumbel"),
                size = 1.1) +
  labs(title = "Distribución Temp Max CDMX",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")

p.CDMX.T<-ggplot(data = CDMX) +
  geom_histogram(aes(x =Tarifa, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = `Temp max`)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlinvgauss(CDMX$Tarifa))},
                aes(color = "Inv Gauss"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlunif(CDMX$Tarifa))},
                aes(color = "Uniforme"),
                size = 1.1) +
  labs(title = "Distribución Tarifa CDMX",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")


p.CDMX.P<-ggplot(data = CDMX) +
  geom_histogram(aes(x =Presipitación, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = Presipitación)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlgumbel(CDMX$Presipitación))},
                aes(color = "Gumbel"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mllaplace(CDMX$Presipitación))},
                aes(color = "Laplace"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlcauchy(CDMX$Presipitación))},
                aes(color = "Cauchy"),
                size = 1.1) +
  labs(title = "Distribución Presipitación CDMX",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")


## N.L ##
p.N.L.Tm<-ggplot(data = N.L) +
  geom_histogram(aes(x =`Temp min`, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = `Temp min`)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlweibull(N.L$`Temp min`))},
                aes(color = "Weibull"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlgumbel(N.L$`Temp min`))},
                aes(color = "Gumbel"),
                size = 1.1) +
  labs(title = "Distribución Temp Min N.L",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")

p.N.L.TM<-ggplot(data = N.L) +
  geom_histogram(aes(x =`Temp max`, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = `Temp max`)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlweibull(N.L$`Temp max`))},
                aes(color = "Weibull"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlinvgauss(N.L$`Temp max`))},
                aes(color = "Inv Gauss"),
                size = 1.1) +
  labs(title = "Distribución Temp Max N.L",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")


p.N.L.T<-ggplot(data = N.L) +
  geom_histogram(aes(x =Tarifa, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = Presipitación))  +
  stat_function(fun = function(.x){dml(x = .x, obj = mlinvgauss(N.L$Tarifa))},
                aes(color = "Gauss Inv"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlgumbel(N.L$Tarifa))},
                aes(color = "Gumbel"),
                size = 1.1) +
  labs(title = "Distribución Tarifa N.L",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")


p.N.L.P<-ggplot(data = N.L) +
  geom_histogram(aes(x =Presipitación, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = Presipitación)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlexp(N.L$Presipitación))},
                aes(color = "Exponencial"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlgumbel(N.L$Presipitación))},
                aes(color = "Gumbel"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mllaplace(N.L$Presipitación))},
              aes(color = "Laplace"),
              size = 1.1)+
  labs(title = "Distribución Presipitación N.L",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")




# CDMX DAMS

p.CDMX.D.Tm<-ggplot(data = CDMX.Dams) +
  geom_histogram(aes(x =`Temp min`, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = `Temp min`)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlweibull(CDMX.Dams$`Temp min`))},
                aes(color = "Weibull"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlpower(CDMX.Dams$`Temp min`))},
                aes(color = "Power"),
                size = 1.1) +
  labs(title = "Distribución Temp Min CDMX Dams",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")



p.CDMX.D.TM<-ggplot(data = CDMX.Dams) +
  geom_histogram(aes(x =`Temp max`, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = `Temp min`)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlbetapr(CDMX.Dams$`Temp max`))},
                aes(color = "Beta Pr"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlinvgauss(CDMX.Dams$`Temp max`))},
                aes(color = "Gauss Inv"),
                size = 1.1) +
  labs(title = "Distribución Temp Max CDMX Dams",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")


p.CDMX.D.T<-ggplot(data = CDMX.Dams) +
  geom_histogram(aes(x =Tarifa, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = Presipitación)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlinvgauss(CDMX.Dams$Tarifa))},
                aes(color = "Gauss Inv"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlweibull(CDMX.Dams$Tarifa))},
                aes(color = "Weibull"),
                size = 1.1) +
  labs(title = "Distribución Tarifa CDMX Dams",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")


p.CDMX.D.P<-ggplot(data = CDMX.Dams) +
  geom_histogram(aes(x =Presipitación, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = Presipitación)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mllaplace(CDMX.Dams$Presipitación))},
                aes(color = "Laplace"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlgumbel(CDMX.Dams$Presipitación))},
                aes(color = "Gumbel"),
                size = 1.1) +
  labs(title = "Distribución Presipitación CDMX Dams",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")


# NL DAMS

p.N.L.D.Tm<-ggplot(data = N.L.Dams) +
  geom_histogram(aes(x =`Temp min`, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = `Temp min`)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlweibull(N.L.Dams$`Temp min`))},
                aes(color = "Weibull"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlpower(N.L.Dams$`Temp min`))},
                aes(color = "Power"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlgumbel(N.L.Dams$`Temp min`))},
                aes(color = "Gumbel"),
                size = 1.1) +
  labs(title = "Distribución Temp Min N.L Dams",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")



p.N.L.D.TM<-ggplot(data = N.L.Dams) +
  geom_histogram(aes(x =`Temp max`, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = `Temp min`)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlpower(N.L.Dams$`Temp max`))},
                aes(color = "Power"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlweibull(N.L.Dams$`Temp max`))},
                aes(color = "Weibull"),
                size = 1.1) +
  labs(title = "Distribución Temp Max N.L Dams",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")

p.N.L.D.T<-ggplot(data = N.L.Dams) +
  geom_histogram(aes(x =Tarifa, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = Presipitación)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlinvgauss(N.L.Dams$Tarifa))},
                aes(color = "Gauss Inv"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlweibull(N.L.Dams$Tarifa))},
                aes(color = "Weibull"),
                size = 1.1) +
  labs(title = "Distribución Tarifa N.L Dams",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")


p.N.L.D.P<-ggplot(data = N.L.Dams) +
  geom_histogram(aes(x =Presipitación, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = Presipitación)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mllaplace(N.L.Dams$Presipitación))},
                aes(color = "Laplace"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlexp(N.L.Dams$Presipitación))},
                aes(color = "Exponencial"),
                size = 1.1) +
  labs(title = "Distribución Presipitación N.L Dams",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")

# CDMX Temporadas
#Primavera 
season<-"Primavera"
p.CDMX.P.Tm<-ggplot(data = CDMX[CDMX$Estación==season,]) +
  geom_histogram(aes(x =`Temp min`, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = `Temp min`)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlweibull(CDMX[CDMX$Estación==season,"Temp min"]))},
                aes(color = "Weibull"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlinvgauss(CDMX[CDMX$Estación==season,"Temp min"]))},
                aes(color = "Inv Gauss"),
                size = 1.1) +
  labs(title = "Distribución Temp Min CDMX",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")

p.CDMX.P.TM<-ggplot(data = CDMX[CDMX$Estación==season,]) +
  geom_histogram(aes(x =`Temp max`, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = `Temp max`)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlinvgauss(CDMX[CDMX$Estación==season,"Temp max"]))},
                aes(color = "Inv Gauss"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlweibull(CDMX[CDMX$Estación==season,"Temp max"]))},
                aes(color = "Weibull"),
                size = 1.1) +
  labs(title = "Distribución Temp Max CDMX",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")

p.CDMX.P.T<-ggplot(data = CDMX[CDMX$Estación==season,]) +
  geom_histogram(aes(x =Tarifa, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = `Temp max`)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlinvgauss(CDMX[CDMX$Estación==season,"Tarifa"]))},
                aes(color = "Inv Gauss"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlunif(CDMX[CDMX$Estación==season,"Tarifa"]))},
                aes(color = "Uniforme"),
                size = 1.1) +
  labs(title = "Distribución Tarifa CDMX",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")


p.CDMX.P.P<-ggplot(data = CDMX[CDMX$Estación==season,]) +
  geom_histogram(aes(x =Presipitación, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = Presipitación)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mllaplace(CDMX[CDMX$Estación==season,"Presipitación"]))},
                aes(color = "Laplace"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlpower(CDMX[CDMX$Estación==season,"Presipitación"]))},
                aes(color = "Power"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlgumbel(CDMX[CDMX$Estación==season,"Presipitación"]))},
                aes(color = "Gumbel"),
                size = 1.1) +
  labs(title = "Distribución Presipitación CDMX",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")

#Verano
season<-"Verano"
p.CDMX.V.Tm<-ggplot(data = CDMX[CDMX$Estación==season,]) +
  geom_histogram(aes(x =`Temp min`, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = `Temp min`)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlgumbel(CDMX[CDMX$Estación==season,"Temp min"]))},
                aes(color = "Gumbel"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlinvgauss(CDMX[CDMX$Estación==season,"Temp min"]))},
                aes(color = "Inv Gauss"),
                size = 1.1) +
  labs(title = "Distribución Temp Min CDMX",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")

p.CDMX.V.TM<-ggplot(data = CDMX[CDMX$Estación==season,]) +
  geom_histogram(aes(x =`Temp max`, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = `Temp max`)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlinvweibull(CDMX[CDMX$Estación==season,"Temp max"]))},
                aes(color = "Inv Weibull"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlinvgauss(CDMX[CDMX$Estación==season,"Temp max"]))},
                aes(color = "Inv Gauss"),
                size = 1.1) +
  labs(title = "Distribución Temp Max CDMX",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")

p.CDMX.V.T<-ggplot(data = CDMX[CDMX$Estación==season,]) +
  geom_histogram(aes(x =Tarifa, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = `Temp max`)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlinvgauss(CDMX[CDMX$Estación==season,"Tarifa"]))},
                aes(color = "Inv Gauss"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlgumbel(CDMX[CDMX$Estación==season,"Tarifa"]))},
                aes(color = "Gumbel"),
                size = 1.1) +
  labs(title = "Distribución Tarifa CDMX",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")


p.CDMX.V.P<-ggplot(data = CDMX[CDMX$Estación==season,]) +
  geom_histogram(aes(x =Presipitación, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = Presipitación)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mllaplace(CDMX[CDMX$Estación==season,"Presipitación"]))},
                aes(color = "Laplace"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlpower(CDMX[CDMX$Estación==season,"Presipitación"]))},
                aes(color = "Power"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlcauchy(CDMX[CDMX$Estación==season,"Presipitación"]))},
                aes(color = "Cauchy"),
                size = 1.1) +
  labs(title = "Distribución Presipitación CDMX",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")

#Otoño
season<-"Otoño"
p.CDMX.O.Tm<-ggplot(data = CDMX[CDMX$Estación==season,]) +
  geom_histogram(aes(x =`Temp min`, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = `Temp min`)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlgumbel(CDMX[CDMX$Estación==season,"Temp min"]))},
                aes(color = "Gumbel"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlinvgauss(CDMX[CDMX$Estación==season,"Temp min"]))},
                aes(color = "Inv Gauss"),
                size = 1.1) +
  labs(title = "Distribución Temp Min CDMX",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")

p.CDMX.O.TM<-ggplot(data = CDMX[CDMX$Estación==season,]) +
  geom_histogram(aes(x =`Temp max`, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = `Temp max`)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mllaplace(CDMX[CDMX$Estación==season,"Temp max"]))},
                aes(color = "Laplace"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlweibull(CDMX[CDMX$Estación==season,"Temp max"]))},
                aes(color = "Weibull"),
                size = 1.1) +
  labs(title = "Distribución Temp Max CDMX",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")

p.CDMX.O.T<-ggplot(data = CDMX[CDMX$Estación==season,]) +
  geom_histogram(aes(x =Tarifa, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = `Temp max`)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlinvgauss(CDMX[CDMX$Estación==season,"Tarifa"]))},
                aes(color = "Inv Gauss"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlgumbel(CDMX[CDMX$Estación==season,"Tarifa"]))},
                aes(color = "Gumbel"),
                size = 1.1) +
  labs(title = "Distribución Tarifa CDMX",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")


p.CDMX.O.P<-ggplot(data = CDMX[CDMX$Estación==season,]) +
  geom_histogram(aes(x =Presipitación, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = Presipitación)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mllaplace(CDMX[CDMX$Estación==season,"Presipitación"]))},
                aes(color = "Laplace"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlpower(CDMX[CDMX$Estación==season,"Presipitación"]))},
                aes(color = "Power"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlcauchy(CDMX[CDMX$Estación==season,"Presipitación"]))},
                aes(color = "Cauchy"),
                size = 1.1) +
  labs(title = "Distribución Presipitación CDMX",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")

season<-"Invierno"
p.CDMX.I.Tm<-ggplot(data = CDMX[CDMX$Estación==season,]) +
  geom_histogram(aes(x =`Temp min`, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = `Temp min`)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlweibull(CDMX[CDMX$Estación==season,"Temp min"]))},
                aes(color = "Weibull"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mllaplace(CDMX[CDMX$Estación==season,"Temp min"]))},
                aes(color = "Laplace"),
                size = 1.1) +
  labs(title = "Distribución Temp Min CDMX",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")

p.CDMX.I.TM<-ggplot(data = CDMX[CDMX$Estación==season,]) +
  geom_histogram(aes(x =`Temp max`, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = `Temp max`)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlinvgauss(CDMX[CDMX$Estación==season,"Temp max"]))},
                aes(color = "Inv Gauss"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlgumbel(CDMX[CDMX$Estación==season,"Temp max"]))},
                aes(color = "Gumbel"),
                size = 1.1) +
  labs(title = "Distribución Temp Max CDMX",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")

p.CDMX.I.T<-ggplot(data = CDMX[CDMX$Estación==season,]) +
  geom_histogram(aes(x =Tarifa, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = `Temp max`)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlinvgauss(CDMX[CDMX$Estación==season,"Tarifa"]))},
                aes(color = "Inv Gauss"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlunif(CDMX[CDMX$Estación==season,"Tarifa"]))},
                aes(color = "Uniforme"),
                size = 1.1) +
  labs(title = "Distribución Tarifa CDMX",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")


p.CDMX.I.P<-ggplot(data = CDMX[CDMX$Estación==season,]) +
  geom_histogram(aes(x =Presipitación, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = Presipitación)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mllaplace(CDMX[CDMX$Estación==season,"Presipitación"]))},
                aes(color = "Laplace"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlpower(CDMX[CDMX$Estación==season,"Presipitación"]))},
                aes(color = "Power"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlgumbel(CDMX[CDMX$Estación==season,"Presipitación"]))},
                aes(color = "Gumbel"),
                size = 1.1) +
  labs(title = "Distribución Presipitación CDMX",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")


# N.L Temporadas
#Primavera
season<-"Primavera"
p.N.L.P.Tm<-ggplot(data = N.L[N.L$Estación==season,]) +
  geom_histogram(aes(x =`Temp min`, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = `Temp min`)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlweibull(N.L[N.L$Estación==season,"Temp min"]))},
                aes(color = "Weibull"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlpower(N.L[N.L$Estación==season,"Temp min"]))},
                aes(color = "power"),
                size = 1.1) +
  labs(title = "Distribución Temp Min N.L",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")

p.N.L.P.TM<-ggplot(data = N.L[N.L$Estación==season,]) +
  geom_histogram(aes(x =`Temp max`, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = `Temp max`)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlweibull(N.L[N.L$Estación==season,"Temp max"]))},
                aes(color = "Weibull"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlinvgauss(N.L[N.L$Estación==season,"Temp max"]))},
                aes(color = "Inv Gauss"),
                size = 1.1) +
  labs(title = "Distribución Temp Max N.L",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")


p.N.L.P.T<-ggplot(data = N.L[N.L$Estación==season,]) +
  geom_histogram(aes(x =Tarifa, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = Presipitación))  +
  stat_function(fun = function(.x){dml(x = .x, obj = mlinvgauss(N.L[N.L$Estación==season,"Tarifa"]))},
                aes(color = "Gauss Inv"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlgumbel(N.L[N.L$Estación==season,"Tarifa"]))},
                aes(color = "Gumbel"),
                size = 1.1) +
  labs(title = "Distribución Tarifa N.L",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")


p.N.L.P.P<-ggplot(data = N.L[N.L$Estación==season,]) +
  geom_histogram(aes(x =Presipitación, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = Presipitación)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlcauchy(N.L[N.L$Estación==season,"Presipitación"]))},
                aes(color = "Cauchy"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlpower(N.L[N.L$Estación==season,"Presipitación"]))},
                aes(color = "Power"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mllaplace(N.L[N.L$Estación==season,"Presipitación"]))},
                aes(color = "Laplace"),
                size = 1.1)+
  labs(title = "Distribución Presipitación N.L",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")

#Verano
season<-"Verano"
p.N.L.V.Tm<-ggplot(data = N.L[N.L$Estación==season,]) +
  geom_histogram(aes(x =`Temp min`, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = `Temp min`)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlcauchy(N.L[N.L$Estación==season,"Temp min"]))},
                aes(color = "Cauchy"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mllaplace(N.L[N.L$Estación==season,"Temp min"]))},
                aes(color = "Laplace"),
                size = 1.1) +
  labs(title = "Distribución Temp Min N.L",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")

p.N.L.V.TM<-ggplot(data = N.L[N.L$Estación==season,]) +
  geom_histogram(aes(x =`Temp max`, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = `Temp max`)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlweibull(N.L[N.L$Estación==season,"Temp max"]))},
                aes(color = "Weibull"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mllaplace(N.L[N.L$Estación==season,"Temp max"]))},
                aes(color = "Laplace"),
                size = 1.1) +
  labs(title = "Distribución Temp Max N.L",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")


p.N.L.V.T<-ggplot(data = N.L[N.L$Estación==season,]) +
  geom_histogram(aes(x =Tarifa, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = Presipitación))  +
  stat_function(fun = function(.x){dml(x = .x, obj = mlinvgauss(N.L[N.L$Estación==season,"Tarifa"]))},
                aes(color = "Gauss Inv"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlgumbel(N.L[N.L$Estación==season,"Tarifa"]))},
                aes(color = "Gumbel"),
                size = 1.1) +
  labs(title = "Distribución Tarifa N.L",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")


p.N.L.V.P<-ggplot(data = N.L[N.L$Estación==season,]) +
  geom_histogram(aes(x =Presipitación, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = Presipitación)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlpower(N.L[N.L$Estación==season,"Presipitación"]))},
                aes(color = "Power"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mllaplace(N.L[N.L$Estación==season,"Presipitación"]))},
                aes(color = "Laplace"),
                size = 1.1)+
  labs(title = "Distribución Presipitación N.L",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")

#Otoño
season<-"Otoño"
p.N.L.O.Tm<-ggplot(data = N.L[N.L$Estación==season,]) +
  geom_histogram(aes(x =`Temp min`, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = `Temp min`)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlweibull(N.L[N.L$Estación==season,"Temp min"]))},
                aes(color = "Weibull"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlpower(N.L[N.L$Estación==season,"Temp min"]))},
                aes(color = "Power"),
                size = 1.1) +
  labs(title = "Distribución Temp Min N.L",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")

p.N.L.O.TM<-ggplot(data = N.L[N.L$Estación==season,]) +
  geom_histogram(aes(x =`Temp max`, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = `Temp max`)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlweibull(N.L[N.L$Estación==season,"Temp max"]))},
                aes(color = "Weibull"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlinvgauss(N.L[N.L$Estación==season,"Temp max"]))},
                aes(color = "Inv Gauss"),
                size = 1.1) +
  labs(title = "Distribución Temp Max N.L",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")


p.N.L.O.T<-ggplot(data = N.L[N.L$Estación==season,]) +
  geom_histogram(aes(x =Tarifa, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = Presipitación))  +
  stat_function(fun = function(.x){dml(x = .x, obj = mlinvgauss(N.L[N.L$Estación==season,"Tarifa"]))},
                aes(color = "Gauss Inv"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlgumbel(N.L[N.L$Estación==season,"Tarifa"]))},
                aes(color = "Gumbel"),
                size = 1.1) +
  labs(title = "Distribución Tarifa N.L",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")


p.N.L.O.P<-ggplot(data = N.L[N.L$Estación==season,]) +
  geom_histogram(aes(x =Presipitación, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = Presipitación)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlexp(N.L[N.L$Estación==season,"Presipitación"]))},
                aes(color = "Exponencial"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlgumbel(N.L[N.L$Estación==season,"Presipitación"]))},
                aes(color = "Gumbel"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mllaplace(N.L[N.L$Estación==season,"Presipitación"]))},
                aes(color = "Laplace"),
                size = 1.1)+
  labs(title = "Distribución Presipitación N.L",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")


#Invierno
season<-"Invierno"
p.N.L.I.Tm<-ggplot(data = N.L[N.L$Estación==season,]) +
  geom_histogram(aes(x =`Temp min`, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = `Temp min`)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlinvgauss(N.L[N.L$Estación==season,"Temp min"]))},
                aes(color = "Gauss Inv"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlgumbel(N.L[N.L$Estación==season,"Temp min"]))},
                aes(color = "Gumbel"),
                size = 1.1) +
  labs(title = "Distribución Temp Min N.L",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")

p.N.L.I.TM<-ggplot(data = N.L[N.L$Estación==season,]) +
  geom_histogram(aes(x =`Temp max`, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = `Temp max`)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlweibull(N.L[N.L$Estación==season,"Temp max"]))},
                aes(color = "Weibull"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlinvgauss(N.L[N.L$Estación==season,"Temp max"]))},
                aes(color = "Inv Gauss"),
                size = 1.1) +
  labs(title = "Distribución Temp Max N.L",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")


p.N.L.I.T<-ggplot(data = N.L[N.L$Estación==season,]) +
  geom_histogram(aes(x =Tarifa, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = Presipitación))  +
  stat_function(fun = function(.x){dml(x = .x, obj = mlinvgauss(N.L[N.L$Estación==season,"Tarifa"]))},
                aes(color = "Gauss Inv"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlgumbel(N.L[N.L$Estación==season,"Tarifa"]))},
                aes(color = "Gumbel"),
                size = 1.1) +
  labs(title = "Distribución Tarifa N.L",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")


p.N.L.I.P<-ggplot(data = N.L[N.L$Estación==season,]) +
  geom_histogram(aes(x =Presipitación, y =  after_stat(density)),
                 bins = 40,
                 alpha = 0.3, color = "black") +
  geom_rug(aes(x = Presipitación)) +
  stat_function(fun = function(.x){dml(x = .x, obj = mlpower(N.L[N.L$Estación==season,"Presipitación"]))},
                aes(color = "Power"),
                size = 1.1) +
  stat_function(fun = function(.x){dml(x = .x, obj = mllaplace(N.L[N.L$Estación==season,"Presipitación"]))},
                aes(color = "Laplace"),
                size = 1.1)+
  labs(title = "Distribución Presipitación N.L",
       color = "Distribución") +
  theme_bw() +
  theme(legend.position = "bottom")


#####      COPULAS  ARQUIMEDIANAS     #####
library(copula)
library(LaplacesDemon)
cor_s_k<-function(data){
  spearman<- data.frame(Metodo=rep("spearman",4),cor(data,y=NULL, use = "everything",  method = "spearman"))
  kendall<-data.frame(Metodo=rep("kendall",4),cor(data, method="kendall"))
  return(cbind(spearman,kendall))
}

cor.CDMX<-cor_s_k(CDMX[,1:4])
cor.N.L<-cor_s_k(N.L[,1:4])

#Copulas 
norm.cop.CDMX <- normalCopula(param = cor.CDMX[2,7], dim =2)
norm.cop.N.L<-normalCopula( param = cor.N.L[2,7], dim =2)
gumbel.cop <- archmCopula(family="gumbel",dim = 2,  param = 2)
clayton.cop = archmCopula(family="clayton", dim=2, param=1)


# Copula de CDMX
# Distribuci?n Tem min~Weibull y Tem max~Inv Gauss #
#Copula normal 
norm.CDF.weibull.invgauss<- mvdc(norm.cop.CDMX, margins=c("weibull","invgauss"), 
                                 paramMargins=list( list(shape= as.numeric(mlweibull(CDMX$`Temp min`)[1]), 
                                                         scale =as.numeric(mlweibull(CDMX$`Temp min`)[2])),  
                                                    list(mean=as.numeric(mlinvgauss(CDMX$`Temp max`)[1]),
                                                         shape=as.numeric(mlinvgauss(CDMX$`Temp max`)[2]))))

start.vals.weibull.invgauss<- c(c(as.numeric(mlweibull(CDMX$`Temp min`)[1]), as.numeric(mlweibull(CDMX$`Temp min`)[2])), 
                                c(as.numeric(mlinvgauss(CDMX$`Temp max`)[1]), as.numeric(mlinvgauss(CDMX$`Temp max`)[2])),
                                cor.CDMX[2,7])
#El orden de los valores iniciales afecta dr?sticamente la viabilidad del ajuste
calibration.norm.weibull.invgauss<- fitMvdc( cbind(CDMX$`Temp min`,CDMX$`Temp max`),
                                             norm.CDF.weibull.invgauss,
                                             start= start.vals.weibull.invgauss)

parameters.CDMX<-data.frame(Modelo="norm.weibull.invgauss",
                            mean.Tmin = coef(calibration.norm.weibull.invgauss)[1], 
                            shape.Tmin = coef(calibration.norm.weibull.invgauss)[2], 
                            mean.Tmax= coef(calibration.norm.weibull.invgauss)[3], 
                            shape.Tmax= coef(calibration.norm.weibull.invgauss)[4], 
                            tau = coef(calibration.norm.weibull.invgauss)[5], 
                            stringsAsFactors = FALSE)

sim.calibration.norm.weibull.invgauss<- rMvdc(dim(CDMX)[1], 
                                              norm.CDF.weibull.invgauss)
prediction.CDMX<-data.frame(Modelo=rep("norm.weibull.invgauss",428),as.data.frame(sim.calibration.norm.weibull.invgauss) ) 


#Copula Gumbel

gumbel.CDF.weibull.invgauss<- mvdc(gumbel.cop, margins=c("weibull","invgauss"), 
                                   paramMargins=list( list(shape= as.numeric(mlweibull(CDMX$`Temp min`)[1]), 
                                                           scale =as.numeric(mlweibull(CDMX$`Temp min`)[2])),  
                                                      list(mean=as.numeric(mlinvgauss(CDMX$`Temp max`)[1]),
                                                           shape=as.numeric(mlinvgauss(CDMX$`Temp max`)[2]))))

start.vals.weibull.invgauss<- c(c(as.numeric(mlweibull(CDMX$`Temp min`)[1]), as.numeric(mlweibull(CDMX$`Temp min`)[2])), 
                                c(as.numeric(mlinvgauss(CDMX$`Temp max`)[1]), as.numeric(mlinvgauss(CDMX$`Temp max`)[2])),
                                cor.CDMX[2,7])
#El orden de los valores iniciales afecta dr?sticamente la viabilidad del ajuste
calibration.gumbel.weibull.invgauss<- fitMvdc( cbind(CDMX$`Temp min`,CDMX$`Temp max`),
                                               gumbel.CDF.weibull.invgauss,
                                               start= start.vals.weibull.invgauss)

parameters.CDMX<-rbind(parameters.CDMX,data.frame("gumbel.weibull.invgauss",
                                                  mean.Tmin = coef(calibration.gumbel.weibull.invgauss)[1], 
                                                  shape.Tmin = coef(calibration.gumbel.weibull.invgauss)[2], 
                                                  mean.Tmax= coef(calibration.gumbel.weibull.invgauss)[3], 
                                                  shape.Tmax= coef(calibration.gumbel.weibull.invgauss)[4], 
                                                  tau = coef(calibration.gumbel.weibull.invgauss)[5], 
                                                  stringsAsFactors = FALSE))

sim.calibration.norm.W.IG<- rMvdc(dim(CDMX)[1], 
                                  norm.CDF.weibull.invgauss)
prediction.CDMX<-data.frame(Modelo=rep("norm.weibull.invgauss",428),as.data.frame(sim.calibration.norm.weibull.invgauss) ) 


# Distribuci?n Tem min~Weibull y Tem max~Gumbel

norm.CDF.weibull.gumbel<- mvdc(norm.cop.CDMX, margins=c("weibull","gumbel"), 
                               paramMargins=list( list(shape= as.numeric(mlweibull(CDMX$`Temp min`)[1]), 
                                                       scale =as.numeric(mlweibull(CDMX$`Temp min`)[2])),  
                                                  list(alpha=as.numeric(mlgumbel(CDMX$`Temp max`)[1]),
                                                       scale=as.numeric(mlgumbel(CDMX$`Temp max`)[2]))))

start.vals.norm.weibull.gumbel<- c(c(as.numeric(mlweibull(CDMX$`Temp min`)[1]), as.numeric(mlweibull(CDMX$`Temp min`)[2])), 
                                   c(as.numeric(mlgumbel(CDMX$`Temp max`)[1]), as.numeric(mlgumbel(CDMX$`Temp max`)[2])),
                                   cor.CDMX[2,7])
#El orden de los valores iniciales afecta dr?sticamente la viabilidad del ajuste
calibration.norm.weibull.gumbel<- fitMvdc( cbind(CDMX$`Temp min`,CDMX$`Temp max`),
                                           norm.CDF.weibull.gumbel,
                                           start= start.vals.norm.weibull.gumbel)

parameters.CDMX<-rbind(parameters.CDMX,data.frame(Modelo="norm.weibull.gumbel",
                                                  mean.Tmin = coef(calibration.norm.weibull.gumbel)[1], 
                                                  shape.Tmin = coef(calibration.norm.weibull.gumbel)[2], 
                                                  mean.Tmax= coef(calibration.norm.weibull.gumbel)[3], 
                                                  shape.Tmax= coef(calibration.norm.weibull.gumbel)[4], 
                                                  tau = coef(calibration.norm.weibull.gumbel)[5], 
                                                  stringsAsFactors = FALSE))

sim.calibration.norm.weibull.gumbel<- rMvdc(dim(CDMX)[1], 
                                            norm.CDF.weibull.gumbel)
prediction.CDMX<-cbind(prediction.CDMX,data.frame(Modelo=rep("norm.weibull.gumbel",428),as.data.frame(sim.calibration.norm.weibull.gumbel) ) )

# Distribuci?n Tem min~Laplace y Tem min~Inv Gauss
norm.CDF.laplace.invgauss<- mvdc(norm.cop.CDMX, margins=c("laplace","invgauss"), 
                                 paramMargins=list( list(location= as.numeric(mllaplace(CDMX$`Temp min`)[1]), 
                                                         scale=as.numeric(mllaplace(CDMX$`Temp min`)[2])),  
                                                    list(mean=as.numeric(mlinvgauss(CDMX$`Temp max`)[1]),
                                                         shape=as.numeric(mlinvgauss(CDMX$`Temp max`)[2]))))

start.vals.laplace.invgauss<- c(c(as.numeric(mllaplace(CDMX$`Temp min`)[1]), as.numeric(mllaplace(CDMX$`Temp min`)[2])), 
                                c(as.numeric(mlinvgauss(CDMX$`Temp max`)[1]), as.numeric(mlinvgauss(CDMX$`Temp max`)[2])),
                                cor.CDMX[2,7])
#El orden de los valores iniciales afecta dr?sticamente la viabilidad del ajuste
calibration.norm.laplace.invgauss<- fitMvdc( cbind(CDMX$`Temp min`,CDMX$`Temp max`),
                                             norm.CDF.laplace.invgauss,
                                             start= start.vals.laplace.invgauss)

parameters.CDMX<-rbind(parameters.CDMX,data.frame(Modelo="norm.laplace.invgauss",
                                                  mean.Tmin = coef(calibration.norm.laplace.invgauss)[1], 
                                                  shape.Tmin = coef(calibration.norm.laplace.invgauss)[2], 
                                                  mean.Tmax= coef(calibration.norm.laplace.invgauss)[3], 
                                                  shape.Tmax= coef(calibration.norm.laplace.invgauss)[4], 
                                                  tau = coef(calibration.norm.laplace.invgauss)[5], 
                                                  stringsAsFactors = FALSE))

sim.calibration.norm.laplace.invgauss<- rMvdc(dim(CDMX)[1], 
                                              norm.CDF.laplace.invgauss)
prediction.CDMX<-cbind(prediction.CDMX,data.frame(Modelo=rep("norm.laplace.invgauss",428),as.data.frame(sim.calibration.norm.laplace.invgauss) ) )

# Distribuci?n Tem min~Laplace y Tem max~Gumbel
norm.CDF.laplace.gumbel<- mvdc(norm.cop.CDMX, margins=c("laplace","gumbel"), 
                               paramMargins=list( list(location= as.numeric(mllaplace(CDMX$`Temp min`)[1]), 
                                                       scale= as.numeric(mllaplace(CDMX$`Temp min`)[2])),  
                                                  list(alpha=as.numeric(mlgumbel(CDMX$`Temp max`)[1]),
                                                       scale=as.numeric(mlgumbel(CDMX$`Temp max`)[2]))))

start.vals.norm.laplace.gumbel<- c(c(as.numeric(mllaplace(CDMX$`Temp min`)[1]), as.numeric(mllaplace(CDMX$`Temp min`)[2])), 
                                   c(as.numeric(mlgumbel(CDMX$`Temp max`)[1]), as.numeric(mlgumbel(CDMX$`Temp max`)[2])),
                                   cor.CDMX[2,7])
#El orden de los valores iniciales afecta dr?sticamente la viabilidad del ajuste
calibration.norm.laplace.gumbel<- fitMvdc( cbind(CDMX$`Temp min`,CDMX$`Temp max`),
                                           norm.CDF.laplace.gumbel,
                                           start= start.vals.norm.laplace.gumbel)

parameters.CDMX<-rbind(parameters.CDMX,data.frame(Modelo="norm.laplace.gumbel",
                                                  mean.Tmin = coef(calibration.norm.laplace.gumbel)[1], 
                                                  shape.Tmin = coef(calibration.norm.laplace.gumbel)[2], 
                                                  mean.Tmax= coef(calibration.norm.laplace.gumbel)[3], 
                                                  shape.Tmax= coef(calibration.norm.laplace.gumbel)[4], 
                                                  tau = coef(calibration.norm.laplace.gumbel)[5], 
                                                  stringsAsFactors = FALSE))

sim.calibration.norm.laplace.gumbel<- rMvdc(dim(CDMX)[1], 
                                            norm.CDF.laplace.gumbel)
prediction.CDMX<-cbind(prediction.CDMX,data.frame(Modelo=rep("norm.laplace.gumbel",428),as.data.frame(sim.calibration.norm.laplace.gumbel) ) )

mean(CDMX$`Temp max`)
# Distribución de N.L
# Distribución Tem min~Uniforme y Tem max~Weibull #
norm.cop <- normalCopula( param = cor.N.L[2,7], dim =2)
norm.CDF.U.W<- mvdc(norm.cop, margins=c("unif","weibull"), 
                    paramMargins=list( list(min= as.numeric(mlunif(N.L$`Temp min`)[1]), 
                                            max =as.numeric(mlunif(N.L$`Temp min`)[2])),  
                                       list(shape=as.numeric(mlweibull(N.L$`Temp max`)[1]),
                                            scale=as.numeric(mlweibull(N.L$`Temp max`)[2]))))

start.vals.nor.U.W<- c(c(as.numeric(mlunif(N.L$`Temp min`)[1]), 
                         as.numeric(mlunif(N.L$`Temp min`)[2])), 
                       c(as.numeric(mlweibull(N.L$`Temp max`)[1]), 
                         as.numeric(mlweibull(N.L$`Temp max`)[2])),
                       cor.N.L[2,7])
#El orden de los valores iniciales afecta dr?sticamente la viabilidad del ajuste
calibration.norm.U.W<- fitMvdc( cbind(N.L$`Temp min`,N.L$`Temp max`),
                                norm.CDF.U.W,
                                start= start.vals.nor.U.W)

parameters.N.L<-data.frame("norm.U.W",
                           mean.Tmin = coef(calibration.norm.U.W)[1], 
                           shape.Tmin = coef(calibration.norm.U.W)[2], 
                           mean.Tmax= coef(calibration.norm.U.W)[3], 
                           shape.Tmax= coef(calibration.norm.U.W)[4], 
                           tau = coef(calibration.norm.U.W)[5], 
                           stringsAsFactors = FALSE)

sim.calibration.norm.U.W<- rMvdc(dim(N.L)[1], 
                                 norm.CDF.U.W)
prediction.N.L<-data.frame(Modelo=rep("norm.U.W",428),as.data.frame(sim.calibration.norm.U.W) ) 





# Distribución Tem min~Uniforme y Tem max~Inv Gauss

# Distribución Tem min~Weibull y Tem min~Weibull
norm.cop <- normalCopula( param = cor.N.L[2,7], dim =2)
norm.CDF.W.W<- mvdc(norm.cop, margins=c("weibull","weibull"), 
                    paramMargins=list( list(shape= as.numeric(mlweibull(N.L$`Temp min`)[1]), 
                                            scale =as.numeric(mlweibull(N.L$`Temp min`)[2])),  
                                       list(shape=as.numeric(mlweibull(N.L$`Temp max`)[1]),
                                            scale=as.numeric(mlweibull(N.L$`Temp max`)[2]))))

start.vals.nor.W.W<- c(c(as.numeric(mlweibull(N.L$`Temp min`)[1]), 
                         as.numeric(mlweibull(N.L$`Temp min`)[2])), 
                       c(as.numeric(mlweibull(N.L$`Temp max`)[1]), 
                         as.numeric(mlweibull(N.L$`Temp max`)[2])),
                       cor.N.L[2,7])
#El orden de los valores iniciales afecta drasticamente la viabilidad del ajuste
calibration.norm.W.W<- fitMvdc( cbind(N.L$`Temp min`,N.L$`Temp max`),
                                norm.CDF.W.W,
                                start= start.vals.nor.W.W)

parameters.N.L<-data.frame("norm.W.W",
                           mean.Tmin = coef(calibration.norm.W.W)[1], 
                           shape.Tmin = coef(calibration.norm.W.W)[2], 
                           mean.Tmax= coef(calibration.norm.W.W)[3], 
                           shape.Tmax= coef(calibration.norm.W.W)[4], 
                           tau = coef(calibration.norm.W.W)[5], 
                           stringsAsFactors = FALSE)

sim.calibration.norm.W.W<- rMvdc(dim(N.L)[1], 
                                 norm.CDF.W.W)
prediction.N.L<-cbind(prediction.N.L,data.frame(Modelo=rep("norm.W.W",428),as.data.frame(sim.calibration.norm.W.W) )) 




# Distribución Tem min~Weibull y Tem max~Inv Gauss



##### EXPORTAR DATOS ####
library(xlsx)

write.xlsx(CDMX,file="ServicioSocial.xlsx",sheetName="DATA CDMX", 
           append=FALSE)

write.xlsx(N.L,file="ServicioSocial.xlsx",sheetName="DATA N.L", 
           append=TRUE)

write.xlsx(dams.CDMX,file="ServicioSocial.xlsx",sheetName="DAMS CDMX", 
           append=T)

write.xlsx(dams.N.L,file="ServicioSocial.xlsx",sheetName="DAMS N.L", 
           append=T)

write.xlsx(distr.CDMX,file="ServicioSocial.xlsx",sheetName="DISTR CDMX", 
           append=T)

write.xlsx(distr.N.L,file="ServicioSocial.xlsx",sheetName="DISTR N.L", 
           append=T)

write.xlsx(cor.CDMX,file="ServicioSocial.xlsx",sheetName="COR CDMX", 
           append=T)

write.xlsx(cor.N.L,file="ServicioSocial.xlsx",sheetName="COR N.L", 
           append=T)

write.xlsx(parameters.CDMX,file="ServicioSocial.xlsx",sheetName="PARAM CDMX", 
           append=T)

write.xlsx(parameters.N.L,file="ServicioSocial.xlsx",sheetName="PARAM N.L", 
           append=T)
