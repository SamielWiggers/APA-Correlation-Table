#Das Script erstellt eine Korrelationstabelle im APA Stil.
#Das Script ist nicht auf nominale Variablen ausgelegt/ nominale Variablen müssen schon als Dummies angegeben werden.


#Die benötigten Pakete
library(sjPlot)
library(stringr)

daten <- read.csv2('Datensastz.csv')#Datensatz laden

variablen <- as.data.frame(daten[,c("Variable1","Variable2","Variable3","Variable4","Variable5")])#Hier die entsprechenden Variablennamen angeben, wie sie im Datensatz benannt sind



mean_values <- sapply(variablen, mean, na.rm = TRUE)#Mittelwerte berechnen
sd_values <- sapply(variablen, sd, na.rm = TRUE)#Standardabweichung berechnen
n<-c()
for (i in 1:ncol(variablen)){#n berechnen
  n<-c(n, sum(!is.na(variablen[,i])))
}

corr_matrix <- cor(variablen[, 1:ncol(variablen)], use = "pairwise.complete.obs", method = "pearson")#Es wird Pearson verwendet, wenn Spearman gewünscht, hier umstellen
corr_matrix <- round(corr_matrix, 2)#runden auf 2 Nachkommastellen
for (i in 1:ncol(variablen)){#Sternchen an die signifikanten Werte machen
  x<-i
  for (j in 1:ncol(variablen)){
    y<-j
    if(i==j) next
    if(y==14){y<-0}
    z<-cor.test(variablen[,i],variablen[,j])
    z<-z$p.value
    if(z < 0.001){
      corr_matrix[x,y]<-paste(corr_matrix[x,y],"***",sep="")
    }
    else if(z < 0.01){
      corr_matrix[x,y]<-paste(corr_matrix[x,y],"**",sep="")
    }
    else if(z < 0.05){
      corr_matrix[x,y]<-paste(corr_matrix[x,y],"*",sep="")
    }
  }
}
for(i in 1:ncol(corr_matrix)){#oberes rechtes Dreieck mit leeren Feldern füllen
  corr_matrix[i,i:ncol(corr_matrix)]<-" "
}
corr_df <- as.data.frame(corr_matrix)
n<-as.data.frame(n)
mean_values<-round(as.data.frame(mean_values),2)
sd_values<-round(as.data.frame(sd_values),2)
corr_df <- cbind(n,Mean = mean_values, SD = sd_values,corr_df)#n,M,SD an die Korrelationsmatrix anfügen
formatted_corr <- as.matrix(corr_df)
for (i in 1:nrow(formatted_corr)){#M und SD in eine Spalte packen
  formatted_corr[i,2]<-paste0(str_trim(formatted_corr[i,2])," (",str_trim(formatted_corr[i,3]),")")
}
formatted_corr<-formatted_corr[,-3]#SD Spalte löschen
col_names<-1:(ncol(formatted_corr)-2)#Spaltenbezeichnungen mit Zahlen machen
for(i in 1:length(col_names)){
  col_names[i]<-paste0("(",col_names[i],")")
}
col_names<-c("n","M (SD)",col_names)
row_names<-c(rownames(formatted_corr))
for (i in 1:length(row_names)){#gleichen Zahlen auch an die Zeilen schreiben
  row_names[i]<-paste0(" (",i,")",row_names[i])
}
rownames(formatted_corr)<-row_names#Zeilenbeschriftungen anpassen
colnames(formatted_corr)<-col_names#Spaltenbeschriftungen anpassen
print(formatted_corr)#Einmal in R zeigen lassen
write.csv2(formatted_corr, "Korrelationstabelle.csv")#als .csv Datei abspeichern