models.df <- data.frame(Models = c("Random Forest", "XGBoost", "Linear SVM", "Gaussian KSVM"),
                        Tuning = c("Features", "Features & Parameters",
                                   "Grid-search Cost",
                                   "Grid-search Cost & Sigma"),
                        Training = c("20% of data", "20% of data", "0.05%-20% of data", "0.05%-20% of data"),
                        Predicting = c("All data", "All data", "?", "?"))                        


CV.df <- data.frame(XGBoost = c("", "", "", "", ""), 
                    Test = c("CV Accuracy", "CV AUC", "", "CV Accuracy", "CV AUC"), 
                    Score = c(0.7724891, 0.8706949, "", 0.7429594, 0.8410817), 
                    Data = c("290 BOW + 4 Scores", "", "", "290 BOW", ""),
                    Method = c("5-Fold CV", "", "", "5-Fold CV", ""))

save(models.df, CV.df, file = "report/showCV_table.RData")
