library(ggplot2)
library(ggpubr)
library(plyr)
library(dplyr)
library(GGally)
library(gridExtra)
library(FactoMineR)


#DATASET

#Load predictors
X_train_bis <- read.csv('C:\\Users\\joana\\OneDrive\\Escriptori\\TFG\\data\\Data_model\\train_X_BIS.csv',header=FALSE)
X_test_bis <- read.csv('C:\\Users\\joana\\OneDrive\\Escriptori\\TFG\\data\\Data_model\\test_X_BIS.csv',header=FALSE)
X_bis <- rbind(X_train_bis,X_test_bis)

X_train_mov <- read.csv('C:\\Users\\joana\\OneDrive\\Escriptori\\TFG\\data\\Data_model\\train_X_MOV.csv',header=FALSE)
X_test_mov <- read.csv('C:\\Users\\joana\\OneDrive\\Escriptori\\TFG\\data\\Data_model\\test_X_MOV.csv',header=FALSE)
X_mov <- rbind(X_train_mov,X_test_mov)

X_train_nibp <- read.csv('C:\\Users\\joana\\OneDrive\\Escriptori\\TFG\\data\\Data_model\\train_X_NIBP.csv',header=FALSE)
X_test_nibp <- read.csv('C:\\Users\\joana\\OneDrive\\Escriptori\\TFG\\data\\Data_model\\test_X_NIBP.csv',header=FALSE)
X_nibp <- rbind(X_train_nibp,X_test_nibp)


#Load outcome
y_train_bis=read.csv('C:\\Users\\joana\\OneDrive\\Escriptori\\TFG\\data\\Data_model\\train_y_BIS.csv',header=FALSE)
y_test_bis=read.csv('C:\\Users\\joana\\OneDrive\\Escriptori\\TFG\\data\\Data_model\\test_y_BIS.csv',header=FALSE)
y_bis=rbind(y_train_bis,y_test_bis)

y_train_mov=read.csv('C:\\Users\\joana\\OneDrive\\Escriptori\\TFG\\data\\Data_model\\train_y_MOV.csv',header=FALSE)
y_test_mov=read.csv('C:\\Users\\joana\\OneDrive\\Escriptori\\TFG\\data\\Data_model\\test_y_MOV.csv',header=FALSE)
y_mov=rbind(y_train_mov,y_test_mov)

y_train_nibp=read.csv('C:\\Users\\joana\\OneDrive\\Escriptori\\TFG\\data\\Data_model\\train_y_NIBP.csv',header=FALSE)
y_test_nibp=read.csv('C:\\Users\\joana\\OneDrive\\Escriptori\\TFG\\data\\Data_model\\test_y_NIBP.csv',header=FALSE)
y_nibp=rbind(y_train_nibp,y_test_nibp)


#SCATTERPLOT OF 3 FIRST PC


#Create a function to compute PCA and plot it
create_plot_pca <- function(X,y){
  
  
  #Perform PCA on predictors
  df_pca <- prcomp(X)
  
  
  #Merge predictors with outcomes
  df_out <- as.data.frame(df_pca$x)
  df_out$group <- 0
  df_out$group[y$V1==0] <- 'Negative'
  df_out$group[y$V1==1] <- 'Positive'
  
  
  #Select 3 principal components
  df_out <- select(df_out,PC1,PC2,PC3,group)
  
  
  #Plot correlation matrix
  p <- ggpairs(df_out, aes(colour = group, alpha = 0.9),
               upper = list(continuous = wrap("cor", size = 4, alignPercent=0.8)),
               lower = list(continuous = wrap("points", alpha = 0.3,    size=0.1))) +
    theme(title=element_text(size=14,face='bold'),
          plot.subtitle=element_text(size=10),
          axis.title=element_text(size=10),
          panel.background = element_blank(),
          panel.grid.major=element_line(color='#c7c7c7'),
          strip.background=element_rect(fill='#575757'),
          strip.text=element_text(face='bold',color = 'white'),
          legend.key=element_rect(fill='white',colour = '#575757'),
          panel.border = element_rect(colour = "#cccccc", fill=NA, size=1))
  
  
  #iterate over each plot in the grid
  for(i in 1:p$nrow) {
    for(j in 1:p$ncol){
      
      
      #choose color
      p[i,j] <- p[i,j] + 
        scale_color_manual(values=c('#ff873d','#c53dff'))+
        scale_fill_manual(values=c('#ff873d','#c53dff'))
      
      
      #for last column put axis text bold
      if (j==4){
        p[i,j] <- p[i,j] +
          theme(panel.grid.major.x = element_blank(),
                axis.text.x = element_text(face='bold'))
      } 
      
      
      #for written correlations and distribution plot remove grid
      if (((i==1)&(j==2))|((i==1)&(j==3))|((i==2)&(j==3))|((i==4)&(j==4))){
        p[i,j] <- p[i,j] +
          theme(panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_blank())
      }
    }
  }
  
  
  #return plot
  return(p)
  
  
}


#create PCA plots for all variables
p_bis <- create_plot_pca(X_bis,y_bis)
p_mov <- create_plot_pca(X_mov,y_mov)
p_nibp <- create_plot_pca(X_nibp,y_nibp)



#Add labels to plots
p_bis + labs(title = 'Prinicipal Components (PC) Analysis by BIS',
             subtitle = expression(atop('Positive' %->% 'BIS above 60% ','Negative' %->% 'BIS below 60%')))
p_mov + labs(title = 'Prinicipal Components (PC) Analysis by MOV',
             subtitle = expression(atop('Positive' %->% 'Movement      ','Negative' %->% 'No movement')))
p_nibp + labs(title = 'Prinicipal Components (PC) Analysis by NIBP',
             subtitle = expression(atop('Positive' %->% 'Mean NIBP below 60mmHg ','Negative' %->% 'Mean NIBP above 60mmHg')))


#EXPLAINED VARIABILITY + CONTRIBUTION

#pca
df_pca <- prcomp(X_bis)

#explained variability
df_con <- df_pca$rotation[,1]
df_var <- df_pca$sdev/sum(df_pca$sdev)

#plot explained variablity of 
#30 first PC+cumulative
x <- 1:30
p_pca <- ggplot()+
  geom_col(aes(x=factor(x),y=df_var[x]),alpha=0.8,fill='Purple')+
  geom_hline(yintercept = .9,linetype='dashed',size=1.2,col='red')+
  geom_line(aes(x=x,y=cumsum(df_var)[x]),size=1)+
  labs(title = 'PCA explained variability',x='PC',y='Explained variability')+
  scale_y_continuous(expand = c(0, 0), limits = c(0, .95),breaks = c(seq(0,1,0.1)))+
  theme(title=element_text(size=14,face='bold'),
        plot.subtitle=element_text(size=10),
        axis.title=element_text(size=10),
        panel.background = element_blank(),
        panel.grid.major.y =element_line(color='#c7c7c7'),
        panel.grid.major.x =element_blank(),
        axis.ticks.x = element_line(size=3),
        strip.background=element_rect(fill='#575757'),
        strip.text=element_text(face='bold',color = 'white'),
        legend.key=element_rect(fill='white',colour = '#575757'),
        panel.border = element_rect(colour = "#cccccc", fill=NA, size=1))

#main contributors to PC1
pred=c('BIS', 'EMGBIS',
        'BSBIS', 'SQI09', 'CpREMI', 'CeREMI', 'InfVolREMI', 'InfRateREMI',
        'CpPROPO', 'CePROPO', 'InfVolPROPO', 'InfRatePROPO', 'Age',
        'Height', 'Weight', 'LBM', 'BSA', 'HR', 'NIBPsys',
        'NIBPdia', 'NIBPmean', 'RespiRate', 'SPO2', 
        'qCON', 'qCONEMG', 'qCONBS', 'qCONSQI', 'qCONqNOX',c(0:30))
indices <- abs(df_con)>.2
p_con <- ggplot()+
  geom_col(aes(x=pred[indices],y=abs(df_con[indices])),alpha=0.8,fill='Purple')+
  labs(x='Predictors',y='Contribution',title='Top 5 contributors to PC1')+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.65))+
  theme(title=element_text(size=14,face='bold'),
        plot.subtitle=element_text(size=10),
        axis.title=element_text(size=10),
        panel.background = element_blank(),
        panel.grid.major.y =element_line(color='#c7c7c7'),
        panel.grid.major.x =element_blank(),
        axis.ticks.x = element_line(size=3),
        strip.background=element_rect(fill='#575757'),
        strip.text=element_text(face='bold',color = 'white'),
        legend.key=element_rect(fill='white',colour = '#575757'),
        panel.border = element_rect(colour = "#cccccc", fill=NA, size=1))

#joint plots
ggarrange(p_pca,p_con,
                  labels = c("A", "B"),
                  ncol = 2, nrow = 1, widths = c(1.6,1))

