wd <- "C:/Users/jsardinha/Documents/GitHub/PML_JHU_PeerAssessment_01"
setwd(wd)

predict(modelFit_rf_CV, newdata=testDataTidy, type = "prob")
ans <- predict(modelFit_rf_CV, newdata=testDataTidy, type = "raw")



pml_write_files = function(x){
     n = length(x)
     for(i in 1:n){
          filename = paste0("./predictions/problem_id_",i,".txt")
          write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
     }
}

answers <- pml_write_files(ans)
pml_write_files(answers)
