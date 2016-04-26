##########################################################################################
### Submission File (how to)
##########################################################################################

# Load in all features
load("submission/April25_finalDF_55features.rda")

some_model_here.myPredict = predict(some_model_here, final_55features[, -1])
save(some_model_here.myPredict, file = "submission/Some_model_here_myPredict.rda")
submission = data.frame(id = 1:length(some_model_here.myPredict), y = some_model_here.myPredict)
write.csv(submission, file = "submission/Submission_COP.csv", row.names = FALSE)
