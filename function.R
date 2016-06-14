resumer <- function (x) {
  name = deparse(substitute(x))
  N=length(x)
  Moyenne=round(mean(x,na.rm=TRUE),2)
  ecart_type=round(sd(x,na.rm=TRUE),2)
  Variance=round(var(x,na.rm=TRUE),2)
  min=min(x,na.rm=TRUE)
  q25=quantile(x, probs=0.25, na.rm=TRUE)
  q50=quantile(x, probs=0.5, na.rm=TRUE)
  q75=quantile(x, probs=0.75, na.rm=TRUE)
  max=max(x,na.rm=TRUE)
  Na=table(is.na(x))[2]
  h=as.data.frame(cbind(N,Moyenne,ecart_type,Variance, min, q25, q50,q75, max,  Na))
  rownames(h) <- name
  return(as.data.frame(h))
}


resumer_borne <- function (x, vect=NULL) {
  if(length(vect) == 2) {
    l <- x[!((x < vect[1]) | (x > vect[2]))]
    df <- resumer(l)
    df$hb <- length(x) - length(l)
    return(df)
  }

  if(length(vect) > 2) {
    l <- x[!(!(x %in% vect))]
    df <- resumer(l)
    df$hb <- length(x) - length(l)
    return(df)
  }
}









#tapply(para$PARACL_VDP_OeDrSaCorr, factor(para$PARACL_SOC_CES_Antenne), resumer_borne, vect=vecte))

resumer_bis<-function (x, inf=NULL, sup=NULL, vect=NULL) {
  name = deparse(substitute(x))
  N=length(x)
  Moyenne=round(mean(x,na.rm=TRUE),2)
  ecart_type=round(sd(x,na.rm=TRUE),2)
  Variance=round(var(x,na.rm=TRUE),2)
  min=min(x,na.rm=TRUE)
  q25=quantile(x, probs=0.25, na.rm=TRUE)
  q50=quantile(x, probs=0.5, na.rm=TRUE)
  q75=quantile(x, probs=0.75, na.rm=TRUE)
  max=max(x,na.rm=TRUE)
  Na=table(is.na(x))[2]
  if (is.null(inf) & is.null(sup) & is.null(vect))
    stop("You must precise vect OR inf-sup")
  if(!(is.null(inf) & is.null(sup)) & is.null(vect))
  {x=ifelse( (x >= inf) & (x <= sup), x, 9999)
  hb=table(x>sup)[2] }
  if((is.null(inf) & is.null(sup)) & !is.null(vect))
  {hb<-table((!x %in% vect)&(!is.na(x)))[2]}
  h=as.data.frame(cbind(N,Moyenne,ecart_type,Variance, min, q25, q50,q75, max,  Na, hb))
  rownames(h) <- name
  return(h)
}



TDB<-function(tbl, var, sexe, c_age, nomvar){

  tbl_freq_s_a<-dcast(tbl, sexe + c_age ~ var)[,-c(1,2)]
  tbl_freq_a<-dcast(tbl,  c_age ~ var)[,-c(1)]
  tbl_freq_s_e<-rbind(dcast(tbl,  sexe ~ var)[,-c(1)], dcast(tbl,  . ~ var)[,-c(1)])

  tbl_pr_s_a<-round(tbl_freq_s_a/apply(tbl_freq_s_a,1,sum)*100,2)
  tbl_pr_a<-round(tbl_freq_a/apply(tbl_freq_a,1,sum)*100,2)
  tbl_pr_s_e<-round(tbl_freq_s_e/apply(tbl_freq_s_e,1,sum)*100,2)

  nClass<-length(levels(var))
  vect_c<-NULL
  for (i in 1:nClass)
  {vect_c<-append(vect_c,c(i,nClass+i))
  vect_c }

  tbl_M_F <-cbind(tbl_freq_s_a, tbl_pr_s_a)[, vect_c]
  tbl_E   <-cbind(tbl_freq_a, tbl_pr_a)[, vect_c]
  tbl_A   <-cbind(tbl_freq_s_e, tbl_pr_s_e)[, vect_c]

  nClass_age<-length(levels(c_age))
  h<-1:(3*nClass_age)
  vect_a <- c(rbind(matrix(h, nrow = nClass_age), (3*nClass_age+1):(3*nClass_age+3)))

  tdb<-rbind(tbl_M_F,tbl_E,tbl_A)[vect_a,]
  names(tdb)<-nomvar
  tdb
}

tbl_char<- function(x,entete,label){

  y=sapply(x, function(x) iconv(x,  "UTF-8", "latin1"))
  ny=nrow(y)
  cvide = rep('', length(label))
  rvide = rep('', ncol(x)+1)

  k_h  <- cbind(cvide, label, y[1:(ny/3),])
  k1_h <- rbind(c('Homme', rvide), k_h)

  k_f  <- cbind(cvide, label, y[((ny/3)+1):(2*(ny/3)),])
  k1_f <- rbind(c('Femme', rvide), k_f)

  k_e  <- cbind(cvide, label, y[(2*((ny/3))+1):(3*(ny/3)),])
  k1_e <- rbind(c('Ensemble', rvide), k_e)

  all_ch <- rbind(k1_h, k1_f, k1_e)
  colnames(all_ch) <- entete
  return(all_ch)
}

# define dict paraclinique
dict_para<-list(POI_MesPoi =c( 40,200 ),
                HAU_MesTail =c( 40 ,200 ),
                SPI_VEMS1 =c( 1,7 ),
                SPI_VEMS2 =c( 1,7 ),
                SPI_VEMS3 =c( 1,7 ),
                SPI_CVF1 =c( 1,7 ),
                SPI_CVF2 =c( 1,7 ),
                SPI_CVF3 =c( 1,7 ),
                TAR_TenArtSysDr =c( 80 ,250 ),
                TAR_TenArtSysGa =c( 80 ,250 ),
                TAR_TenArtDiaDr =c( 30 ,120 ),
                TAR_TenArtDiaGa =c( 30 ,120 ),
                VDL_OeDrSaCorr =c(0,12 ),
                VDL_OeDrAvCorr =c(0,12 ),
                VDL_OeGaAvCorr =c(0,12 ),
                VDL_OeGaSaCorr =c(0,12 ),
                BIO_Glyc =c( 2.5 ,20 ),
                BIO_Crea =c(10 ,2000 ),
                BIO_Gam =c( 2 ,300 ),
                BIO_Alat =c( 2 ,200 ),
                BIO_ChoTot =c( 1.5,15  ),
                BIO_ChoHDL =c( 0.25 ,3.5 ),
                BIO_Trig =c( 0.1 ,30 ),
                HEM_GloBla =c( 1.0 ,50 ),
                HEM_GloRou =c( 2 ,7 ),
                HEM_VolGlobMoy =c( 60 ,120 ),
                HEM_Hemo =c( 50 ,200 ),
                HEM_Plaq =c( 50 ,700 ),
                HEM_NeuPhi =c( 15 ,90 ),
                HEM_EosiPhi =c(0.5,20 ),
                HEM_BasoPhi =c(0.5,10 ),
                HEM_Lympho =c( 5,80 ),
                HEM_Monocy =c( 1,30 ),
                BIR_MicAlb =c( 5 ,500 ),
                BIR_Gluc =c( 0 ,30 ),
                BIR_Prot =c( 0.05 ,5 ),
                BIR_Creat =c( 2 ,30 ),
                VDP_OeDrSaCorr = c(1.5,2,3,4,5,6,7,8,10,14,20,28 ),
                VDP_OeDrAvCorr = c(1.5,2,3,4,5,6,7,8,10,14,20,28 ),
                VDP_OeGaAvCorr = c(1.5,2,3,4,5,6,7,8,10,14,20,28 ),
                VDP_OeGaSaCorr = c(1.5,2,3,4,5,6,7,8,10,14,20,28 ),
                AUD_AuDr500 = c(-10 , -5 , 0 , 5 , 10 , 15, 20 , 25 , 30 , 35, 40 , 45, 50 , 55 , 60, 65 , 70, 75 , 80 , 85, 90 ),
                AUD_AuDr1000 = c(-10 , -5 , 0 , 5 , 10 , 15, 20 , 25 , 30 , 35, 40 , 45, 50 , 55 , 60, 65 , 70, 75 , 80 , 85, 90 ),
                AUD_AuDr2000 = c(-10 , -5 , 0 , 5 , 10 , 15, 20 , 25 , 30 , 35, 40 , 45, 50 , 55 , 60, 65 , 70, 75 , 80 , 85, 90 ),
                AUD_AuDr4000 =c(-10 , -5 , 0 , 5 , 10 , 15, 20 , 25 , 30 , 35, 40 , 45, 50 , 55 , 60, 65 , 70, 75 , 80 , 85, 90 ),
                AUD_AuGa500 =c(-10 , -5 , 0 , 5 , 10 , 15, 20 , 25 , 30 , 35, 40 , 45, 50 , 55 , 60, 65 , 70, 75 , 80 , 85, 90 ),
                AUD_AuGa1000 = c(-10 , -5 , 0 , 5 , 10 , 15, 20 , 25 , 30 , 35, 40 , 45, 50 , 55 , 60, 65 , 70, 75 , 80 , 85, 90 ),
                AUD_AuGa2000 = c(-10 , -5 , 0 , 5 , 10 , 15, 20 , 25 , 30 , 35, 40 , 45, 50 , 55 , 60, 65 , 70, 75 , 80 , 85, 90 ),
                AUD_AuGa4000 = c(-10 , -5 , 0 , 5 , 10 , 15, 20 , 25 , 30 , 35, 40 , 45, 50 , 55 , 60, 65 , 70, 75 , 80 , 85, 90))

