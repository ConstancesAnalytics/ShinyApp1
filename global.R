library('shiny')
library('ggplot2')  # for the diamonds dataset
library('DT')
library('dplyr')

#setwd("U:/R/App3")
# load table "para"
load('./paracl_aff2016_05_10.RData')

source('./function.R')




#selections des variavles dans le catalogues des donn?es.
para <- select(para, PARACL_SOC_NConstances,PARACL_SOC_CES_NCes,PARACL_SOC_CES_Antenne,PARACL_SOC_DNaissance,PARACL_SOC_Sex,PARACL_SOC_DatExam,PARACL_SAN_Regle,PARACL_HAU_MesTail,PARACL_POI_MesPoi,PARACL_VDP_OeDrSaCorr,PARACL_VDP_OeGaSaCorr,PARACL_VDP_OeDrAvCorr,PARACL_VDP_OeGaAvCorr,PARACL_VDP_BiSaCorr,PARACL_VDP_BiAvCorr,PARACL_VDP_CeProAmbMono,PARACL_VDL_OeDrSaCorr,PARACL_VDL_OeGaSaCorr,PARACL_VDL_OeDrAvCorr,PARACL_VDL_OeGaAvCorr,PARACL_VDL_BiSaCorr,PARACL_VDL_BiAvCorr,PARACL_AUD_RealCab,PARACL_AUD_AuDr500,PARACL_AUD_AuDr1000,PARACL_AUD_AuDr2000,PARACL_AUD_AuDr4000,PARACL_AUD_AuDr8000,PARACL_AUD_AuGa500,PARACL_AUD_AuGa1000,PARACL_AUD_AuGa2000,PARACL_AUD_AuGa4000,PARACL_AUD_AuGa8000,PARACL_SPI_CriAcc,PARACL_SPI_CriRepr,PARACL_SPI_VEMS1,PARACL_SPI_CVF1,PARACL_SPI_VEMS2,PARACL_SPI_CVF2,PARACL_SPI_VEMS3,PARACL_SPI_CVF3,PARACL_TAR_TenArtSysDr,PARACL_TAR_TenArtDiaDr,PARACL_TAR_TenArtSysGa,PARACL_TAR_TenArtDiaGa,PARACL_TAR_TenArtSysBraRefDr,PARACL_TAR_TenArtDiaBraRefDr,PARACL_TAR_TenArtSysBraRefGa,PARACL_TAR_TenArtDiaBraRefGa,PARACL_BIO_Glyc,PARACL_BIO_Crea,PARACL_BIO_Gam,PARACL_BIO_Alat,PARACL_BIO_ChoTot,PARACL_BIO_ChoHDL,PARACL_BIO_Trig,PARACL_HEM_GloBla,PARACL_HEM_GloRou,PARACL_HEM_Hemo,PARACL_HEM_VolGlobMoy,PARACL_HEM_Hemato,PARACL_HEM_Plaq,PARACL_HEM_NeuPhi,PARACL_HEM_EosiPhi,PARACL_HEM_BasoPhi,PARACL_HEM_Lympho,PARACL_HEM_Monocy,PARACL_BIR_MicAlb,PARACL_BIR_Gluc,PARACL_BIR_Prot,PARACL_BIR_Creat,PARACL_TAI_MesToTai,PARACL_HAN_MesToHan )




#selection des noms des variables


para_cols <- gsub("PARACL_","", colnames(para))
colnames(para) <- para_cols

# change "," to "." while keeping data types
para_1 <- para[ , apply(para, 2, function(x) any(grepl(",", x)))]
para_2 <- para[ , !(apply(para, 2, function(x) any(grepl(",", x))))]
para_1_corr <- as.data.frame(apply(para_1, 2, function(x)  as.numeric(gsub(",", ".", x))))
para <- cbind(para_1_corr, para_2)



#S?paration entre les variables Numerique et facteur

para_dic=para[names(dict_para)]


para_1 <- para_dic[ , sapply(dict_para, function(x) length(x)==2)]
para_2 <- para_dic[,!sapply(dict_para, function(x) length(x)==2) ]
para_1_corr <- as.data.frame(apply(para_1, 2, function(x) as.numeric( x)))
para_2_corr <- as.data.frame(apply(para_2, 2, function(x) as.factor( x)))

para_else= para[setdiff(para_cols,names(dict_para))]
para <- cbind(para_1_corr, para_2_corr,para_else)




# manual modifications (a voir)
para$SOC_NConstances <- as.factor(para$SOC_NConstances)
para$SOC_Sex <- as.factor(para$SOC_Sex)
para$SOC_CES_NCes <- as.factor(para$SOC_CES_NCes)
para$SOC_DNaissance <- as.Date(para$SOC_DNaissance, "%Y-%m-%d")
para$SOC_DatExam <- as.Date(para$SOC_DatExam, "%Y-%m-%d")


# define para_num et others
para_num  <- para[ , sapply(para,  is.numeric)]
para_fac  <- para[ , sapply(para,  is.factor)]

para_num$CESantenne <- para$SOC_CES_Antenne

all.list_num<-colnames(para_num)







