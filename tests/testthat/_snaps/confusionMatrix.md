# Confusion matrix works

    Levels are not in the same order for reference and data. Refactoring data to match.

---

    Levels are not in the same order for reference and data. Refactoring data to match.

---

    Code
      confusionMatrix(dat5, ref2)
    Condition
      Error in `confusionMatrix.default()`:
      ! The data contain levels not found in the data.

---

    Code
      confusionMatrix(dat5, ref)
    Condition
      Error in `confusionMatrix.default()`:
      ! The data contain levels not found in the data.

# confusionMatrix.table rejects bad input

    Code
      confusionMatrix(cm_tab2, mode = "nope")
    Condition
      Error in `confusionMatrix.table()`:
      ! `mode` should be either 'sens_spec', 'prec_recall', or 'everything'

---

    Code
      confusionMatrix(as.table(matrix(1:6, nrow = 2)))
    Condition
      Error in `confusionMatrix.table()`:
      ! the table must nrow = ncol

---

    Code
      confusionMatrix(one)
    Condition
      Error in `confusionMatrix.table()`:
      ! there must be at least 2 factors levels in the data

---

    Code
      confusionMatrix(cm_tab2, positive = 1)
    Condition
      Error in `confusionMatrix.table()`:
      ! positive argument must be character

---

    Code
      confusionMatrix(mismatched)
    Condition
      Error in `confusionMatrix.table()`:
      ! the table must the same classes in the same order

# confusionMatrix.table rejects mis-sized prevalence vectors

    Code
      confusionMatrix(cm_tab2, prevalence = c(0.2, 0.3))
    Condition
      Error in `confusionMatrix.table()`:
      ! with two levels, one prevalence probability must be specified

---

    Code
      confusionMatrix(cm_tab3, prevalence = c(0.5, 0.5))
    Condition
      Error in `confusionMatrix.table()`:
      ! the number of prevalence probability must be the same as the number of levels

---

    Code
      confusionMatrix(cm_tab3, prevalence = c(0.3, 0.3, 0.4))
    Condition
      Error in `confusionMatrix.table()`:
      ! with >2 classes, the prevalence vector must have names

# as.matrix.confusionMatrix returns each requested piece

    Code
      as.matrix(cm, what = "nope")
    Condition
      Error in `as.matrix.confusionMatrix()`:
      ! what must be either xtabs, overall or classes

# print.confusionMatrix renders each mode

    Code
      print(confusionMatrix(cm_tab2, mode = "sens_spec"))
    Output
      Confusion Matrix and Statistics
      
                Reference
      Prediction yes no
             yes   8  1
             no    2  9
                                                
                     Accuracy : 0.85            
                       95% CI : (0.6211, 0.9679)
          No Information Rate : 0.5             
          P-Value [Acc > NIR] : 0.001288        
                                                
                        Kappa : 0.7             
                                                
       Mcnemar's Test P-Value : 1.000000        
                                                
                  Sensitivity : 0.8000          
                  Specificity : 0.9000          
               Pos Pred Value : 0.8889          
               Neg Pred Value : 0.8182          
                   Prevalence : 0.5000          
               Detection Rate : 0.4000          
         Detection Prevalence : 0.4500          
            Balanced Accuracy : 0.8500          
                                                
             'Positive' Class : yes             
                                                

---

    Code
      print(confusionMatrix(cm_tab2, mode = "prec_recall"))
    Output
      Confusion Matrix and Statistics
      
                Reference
      Prediction yes no
             yes   8  1
             no    2  9
                                                
                     Accuracy : 0.85            
                       95% CI : (0.6211, 0.9679)
          No Information Rate : 0.5             
          P-Value [Acc > NIR] : 0.001288        
                                                
                        Kappa : 0.7             
                                                
       Mcnemar's Test P-Value : 1.000000        
                                                
                    Precision : 0.8889          
                       Recall : 0.8000          
                           F1 : 0.8421          
                   Prevalence : 0.5000          
               Detection Rate : 0.4000          
         Detection Prevalence : 0.4500          
            Balanced Accuracy : 0.8500          
                                                
             'Positive' Class : yes             
                                                

---

    Code
      print(confusionMatrix(cm_tab2, mode = "everything"))
    Output
      Confusion Matrix and Statistics
      
                Reference
      Prediction yes no
             yes   8  1
             no    2  9
                                                
                     Accuracy : 0.85            
                       95% CI : (0.6211, 0.9679)
          No Information Rate : 0.5             
          P-Value [Acc > NIR] : 0.001288        
                                                
                        Kappa : 0.7             
                                                
       Mcnemar's Test P-Value : 1.000000        
                                                
                  Sensitivity : 0.8000          
                  Specificity : 0.9000          
               Pos Pred Value : 0.8889          
               Neg Pred Value : 0.8182          
                    Precision : 0.8889          
                       Recall : 0.8000          
                           F1 : 0.8421          
                   Prevalence : 0.5000          
               Detection Rate : 0.4000          
         Detection Prevalence : 0.4500          
            Balanced Accuracy : 0.8500          
                                                
             'Positive' Class : yes             
                                                

---

    Code
      print(confusionMatrix(cm_tab3))
    Output
      Confusion Matrix and Statistics
      
                Reference
      Prediction A B C
               A 5 1 0
               B 1 4 1
               C 1 1 6
      
      Overall Statistics
                                               
                     Accuracy : 0.75           
                       95% CI : (0.509, 0.9134)
          No Information Rate : 0.35           
          P-Value [Acc > NIR] : 0.0003106      
                                               
                        Kappa : 0.6241         
                                               
       Mcnemar's Test P-Value : 0.8012520      
      
      Statistics by Class:
      
                           Class: A Class: B Class: C
      Sensitivity            0.7143   0.6667   0.8571
      Specificity            0.9231   0.8571   0.8462
      Pos Pred Value         0.8333   0.6667   0.7500
      Neg Pred Value         0.8571   0.8571   0.9167
      Prevalence             0.3500   0.3000   0.3500
      Detection Rate         0.2500   0.2000   0.3000
      Detection Prevalence   0.3000   0.3000   0.4000
      Balanced Accuracy      0.8187   0.7619   0.8516

---

    Code
      print(confusionMatrix(cm_tab2), printStats = FALSE)
    Output
      Confusion Matrix and Statistics
      
                Reference
      Prediction yes no
             yes   8  1
             no    2  9

# confusionMatrix.default validates its arguments

    Code
      confusionMatrix(a, b, mode = "nope")
    Condition
      Error in `confusionMatrix.default()`:
      ! `mode` should be either 'sens_spec', 'prec_recall', or 'everything'

---

    Code
      confusionMatrix(1:4, b)
    Condition
      Error:
      ! `data` and `reference` should be factors with the same levels.

---

    Code
      confusionMatrix(a, b, positive = 1)
    Condition
      Error in `confusionMatrix.default()`:
      ! positive argument must be character

---

    Code
      confusionMatrix(more, b)
    Condition
      Error in `confusionMatrix.default()`:
      ! the data cannot have more levels than the reference

---

    Code
      confusionMatrix(x, y)
    Condition
      Error in `confusionMatrix.default()`:
      ! The data must contain some levels that overlap the reference.

# confusionMatrix errors on regression and non-resampled fits

    Code
      confusionMatrix(reg_fit)
    Condition
      Error in `confusionMatrix.train()`:
      ! confusion matrices are only valid for classification models

---

    Code
      confusionMatrix(loo_fit)
    Condition
      Error in `confusionMatrix.train()`:
      ! cannot compute confusion matrices for leave-one-out, out-of-bag resampling, or no resampling

# confusionMatrix needs at least two factor levels

    Code
      confusionMatrix(one, one)
    Condition
      Error in `confusionMatrix.default()`:
      ! there must be at least 2 factors levels in the data

# confusionMatrix.matrix rejects non-square matrices

    Code
      confusionMatrix(matrix(1:6, nrow = 2))
    Condition
      Error in `confusionMatrix.matrix()`:
      ! matrix must have equal dimensions

# confusionMatrix.table rejects tables with other than 2 dimensions

    Code
      confusionMatrix(as.table(array(1:8, dim = c(2, 2, 2))))
    Condition
      Error in `confusionMatrix.table()`:
      ! the table must have two dimensions

# confusionMatrix requires named prevalences for 3+ classes

    Code
      confusionMatrix(cm_tab3, prevalence = c(0.2, 0.3, 0.5))
    Condition
      Error in `confusionMatrix.table()`:
      ! with >2 classes, the prevalence vector must have names

# print.confusionMatrix defaults and validates the mode

    Code
      print(cm, mode = NULL)
    Output
      Confusion Matrix and Statistics
      
                Reference
      Prediction yes no
             yes   8  1
             no    2  9
                                                
                     Accuracy : 0.85            
                       95% CI : (0.6211, 0.9679)
          No Information Rate : 0.5             
          P-Value [Acc > NIR] : 0.001288        
                                                
                        Kappa : 0.7             
                                                
       Mcnemar's Test P-Value : 1.000000        
                                                
                  Sensitivity : 0.8000          
                  Specificity : 0.9000          
               Pos Pred Value : 0.8889          
               Neg Pred Value : 0.8182          
                   Prevalence : 0.5000          
               Detection Rate : 0.4000          
         Detection Prevalence : 0.4500          
            Balanced Accuracy : 0.8500          
                                                
             'Positive' Class : yes             
                                                

---

    Code
      print(cm, mode = "nope")
    Condition
      Error in `print.confusionMatrix()`:
      ! `mode` should be either 'sens_spec', 'prec_recall', or 'everything'

# print.confusionMatrix filters multiclass statistics by mode

    Code
      print(cm, mode = "prec_recall")
    Output
      Confusion Matrix and Statistics
      
                  
                   setosa versicolor virginica
        setosa         50          0         0
        versicolor      0         50         0
        virginica       0          0        50
      
      Overall Statistics
                                           
                     Accuracy : 1          
                       95% CI : (0.9757, 1)
          No Information Rate : 0.3333     
          P-Value [Acc > NIR] : < 2.2e-16  
                                           
                        Kappa : 1          
                                           
       Mcnemar's Test P-Value : NA         
      
      Statistics by Class:
      
                           Class: setosa Class: versicolor Class: virginica
      Precision                   1.0000            1.0000           1.0000
      Recall                      1.0000            1.0000           1.0000
      F1                          1.0000            1.0000           1.0000
      Prevalence                  0.3333            0.3333           0.3333
      Detection Rate              0.3333            0.3333           0.3333
      Detection Prevalence        0.3333            0.3333           0.3333
      Balanced Accuracy           1.0000            1.0000           1.0000

# print.confusionMatrix.train shows counts for a single resample

    Code
      print(cm)
    Output
      Repeated Train/Test Splits Estimated (1 reps, 75%) Confusion Matrix 
      
      (entries are un-normalized aggregated counts)
       
      Confusion Matrix and Statistics
      
                  Reference
      Prediction   setosa versicolor virginica
        setosa         12          0         0
        versicolor      0         12         0
        virginica       0          0        12
      
      Overall Statistics
                                           
                     Accuracy : 1          
                       95% CI : (0.9026, 1)
          No Information Rate : 0.3333     
          P-Value [Acc > NIR] : < 2.2e-16  
                                           
                        Kappa : 1          
                                           
       Mcnemar's Test P-Value : NA         
      
      Statistics by Class:
      
                           Class: setosa Class: versicolor Class: virginica
      Sensitivity                 1.0000            1.0000           1.0000
      Specificity                 1.0000            1.0000           1.0000
      Pos Pred Value              1.0000            1.0000           1.0000
      Neg Pred Value              1.0000            1.0000           1.0000
      Prevalence                  0.3333            0.3333           0.3333
      Detection Rate              0.3333            0.3333           0.3333
      Detection Prevalence        0.3333            0.3333           0.3333
      Balanced Accuracy           1.0000            1.0000           1.0000

# confusion matrices are refused for regression fits

    Code
      caret:::train_resampledCM(fake)
    Condition
      Error in `caret:::train_resampledCM()`:
      ! confusion matrices are only valid for classification models

# the resampled-table helpers explain what 50+ class objects need

    Code
      caret:::train_resampledCM(fake_train)
    Condition
      Error in `caret:::train_resampledCM()`:
      ! When there are 50+ classes, `train` does not automatically pre-compute the resampled confusion matrices. You can get them from this function using a value of `savePredictions` other than FALSE.

---

    Code
      caret:::rfe_resampledCM(fake_rfe)
    Condition
      Error in `caret:::rfe_resampledCM()`:
      ! When there are 50+ classes, `the function does not automatically pre-compute the resampled confusion matrices. You can get them when the object has a `pred` element.

---

    Code
      caret:::sbf_resampledCM(fake_sbf)
    Condition
      Error in `caret:::sbf_resampledCM()`:
      ! When there are 50+ classes, the function does not automatically pre-compute the resampled confusion matrices. You can get them when the option `saveDetails = TRUE`.

