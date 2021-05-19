calculateScore<-function(model,x,y) {
  pred=predict(model,newdata=x)
  data=data.frame(cbind(obs=y,pred=pred))
  baseScore=model$control$summaryFunction(data)
}

shuffleVarImp.train<-function(model,test_x,test_y,shuffleTimes=100) {  
  baseScore=calculateScore(model,test_x,test_y)
  
  feats=setdiff(names(model$trainingData),".outcome")
  imps=lapply(feats,FUN=function(feat) {
    cat(paste0(feat,"\n"))
    pb <- txtProgressBar()
    scores=lapply(1:shuffleTimes,FUN=function(rep) {                
      shuffleData<-test_x
      shuffleData[,feat]=sample(shuffleData[,feat])
      score=calculateScore(model,shuffleData,test_y)      
      setTxtProgressBar(pb, rep/shuffleTimes)      
      score
    })
    close(pb)
    if (model$maximize) {
      meanWorse<-mean(unlist(scores)<baseScore)  
    } else {
      meanWorse<-mean(unlist(scores)>baseScore)  
    }
    meanWorse    
  })
  shuffleImps <- data.frame('feature'=feats, 'importance'=unlist(imps))
  shuffleImps[order(shuffleImps$importance,decreasing = T),]
}

"shuffleVarImp" <-
  function(object, ...){
    UseMethod("shuffleVarImp")
  }
