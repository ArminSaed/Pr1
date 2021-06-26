library(agricolae)
library(readxl)
df <- read_excel(file.choose(), sheet=1)
View(df)
#====@====@====@===@===@===@===@===@===@===@===@===@===@===")

MCt <- function(df){
  
  DIR <- choose.dir()
  setwd(DIR)
  
  #if "agricolae" is installed?
  if ("agricolae" %in% library()$results[,1]) {
    print("the necessary package (agricolae) is installed")
  } else {
    install.packages("agricolae")
  } 
  
  # load agricolae if it's not loaded yet
  if("agricolae" %in% (.packages())){
    print("agricolae is loaded")
  } else {
    require(agricolae)
  }
  
  tlev<-length(unique(df$t));al <-matrix("", tlev, 3)
  Rlev<-length(unique(df$r))
  ####@###@###@###@###@###@###@###@###@###@###@###@###@
  
  df$t<-as.factor(df$t)
  df$r<-as.factor(df$r)
  #====@====@====@===@===@===@===@===@===@===@===@===@===
  
  n <- ncol(df)-3
  s1 <- vector(mode = "list", length = n)
  s2 <- vector(mode = "list", length = n)
  MSerror <- vector(mode = "list", length = n)
  mn <- vector(mode = "list", length = n)
  cv <- vector(mode = "list", length = n)
  x <- matrix('', n, 1)
  #====@====@====@===@===@===@===@===@===@===@===@===@===
  
  for (i in 1:n){
    s1[i] <- as.list(summary(aov(as.matrix(df[i+2]) ~ t, data=df)))
    model <- aov(as.matrix(df[i+2])~df$t)
    dff<-as.numeric(df.residual(model))
    MSerror[i]<-as.numeric(deviance(model))/dff
    mn[i] <- as.numeric(sum(df[i+2])/(tlev*Rlev))
    x[i] <- paste("x",i,sep="")
    cv[i] <- (sqrt(as.numeric(MSerror[i]))/as.numeric(mn[i]))*100
    s2[i]  <- list(as.matrix(LSD.test(model, trt="df$t")[[5]]))
  }
  #====@====@====@===@===@===@===@===@===@===@===@===@===
  
  write.csv(s1, paste(DIR, "\\ANOVA.csv", sep=""))
  print("====@====@====@===@===@===@===@===@===@===@===@===@===@===")
  
  cv <- as.matrix(cv)
  row.names(cv) <- x; colnames(cv) <- "CV (%)"
  write.csv(cv, paste(DIR, "\\CV.csv", sep=""))
  print("====@====@====@===@===@===@===@===@===@===@===@===@===@===")
  
  ss <- vector(mode = "list", length = n)
  for (i in 1:n){
    ss[i] =""}
  f <- matrix("", tlev, 2)
  for (i in 1:n){
    rn <- as.data.frame(rownames(as.data.frame(s2[i])))
    f <- paste("f", i, sep="")
    as.data.frame.factor(f)
    f <- as.data.frame(s2[i])
    f <- cbind(rn, f)
    colnames(f) <- c('factor.[i]', 'mean.[i]', 'letter.[i]')
    #rownames(f) <- rn
    ss[i] <- list(f)
    print(ss)
    write.csv(ss, paste(DIR, "\\T_Mean.csv", sep=""))
  }
  print("====@====@====@===@===@===@===@===@===@===@===@===@===@===")
  
  std.t <- aggregate(df[, 3:ncol(df)], list(df$t), sd)
  m.t <- aggregate(df[, 3:ncol(df)], list(df$t), mean)
  Factor.t <- std.t$Group
  set <- std.t[, 2:ncol(std.t)]/length(unique(df$r))
  SE.t <- cbind(Factor.t, set)
  M.t <- cbind(Factor.t, m.t)
  print("====@====@====@===@===@===@===@===@===@===@===@===@===@===")
  
  path3 <- paste(DIR, '\\Standard_Error.csv', sep="")
  path4 <- paste(DIR, '\\Average_Final.csv', sep="")
  write.csv(as.matrix(SE.t), path3)
  write.csv(as.matrix(M.t), path4)
  print("====@====@====@===@===@===@===@===@===@===@===@===@===@===")
}

MCt(df=df)

