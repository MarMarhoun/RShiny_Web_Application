# Web Application for 3 ML algorithms using RShiny

In this repository, we have programmed web application in R using the "Shiny" package. This application handle three machine learning methods: PCA for data analysis, SVM, and decision tree for classification.

In our program we have three scripts are:
 
  *Login.r: To login into the main page.
  *Server.r: This is the main scrpit. In this scrpit, we wrote our necessary codes for functionality and visualization.
  *ui.r: For visualization.
  
  
> To login please use admin for both id & password

The main libraries used are:
  *library ("Shiny")
  *library (ggplot2) # For plotting.
  *library (FactoMineR)
  *library (factoextra)
  *For ACP algorithm: we used these two packages: FactoMineR (Exploratory Multivariate Data Analysis with R) and factoextra (Extract and Visualize the Results of Multivariate Data Analyses).
  *library (rpart) # Package required for the decision tree.
  *library (rpart.plot) # Plotting the trees rpart.
  *library (e1071) ## Package required for SVM.
  *library (shinythemes) # To change the theme of the page.
