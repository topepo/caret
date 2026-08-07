# calibration requires a formula

    Code
      calibration(lift_data)
    Condition
      Error in `calibration.default()`:
      ! 'x' should be a formula

# calibration needs a factor on the left-hand side

    Code
      calibration(prob1 ~ prob2, data = lift_data)
    Condition
      Error in `calibration.formula()`:
      ! the left-hand side of the formula must be a factor of classes

# print.calibration reports the models and event

    Code
      print(cal)
    Output
      
      Call:
      calibration.formula(x = Class ~ prob1, data = lift_data)
      
      Models: prob1 
      Event:  yes 
      Cuts: 11 

