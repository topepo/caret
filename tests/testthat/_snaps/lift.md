# lift requires a formula

    Code
      lift(lift_data)
    Condition
      Error in `lift.default()`:
      ! 'x' should be a formula

# lift validates the formula and labels

    Code
      lift(prob1 ~ prob2, data = lift_data)
    Condition
      Error in `lift.formula()`:
      ! the left-hand side of the formula must be a factor of classes

---

    Code
      lift(Class ~ prob1, data = lift_data, labels = c("a", "b"))
    Condition
      Error in `lift.formula()`:
      ! labels should have an element for each term on the rhs of the formula

# xyplot.lift rejects an unknown plot type

    Code
      caret:::xyplot.lift(lf, plot = "nope")
    Condition
      Error:
      ! `plot`` should be either 'lift' or 'gain'

# print.lift reports the models and event rate

    Code
      print(lf)
    Output
      
      Call:
      lift.formula(x = Class ~ prob1, data = lift_data)
      
      Models: prob1 
      Event: yes (50%)

# the lift plot methods reject an unknown plot type

    Code
      xyplot(lf, plot = "nope")
    Condition
      Error:
      ! `plot`` should be either 'lift' or 'gain'

---

    Code
      ggplot(lf, plot = "nope")
    Condition
      Error:
      ! `plot`` should be either 'lift' or 'gain'

# lift needs a two-class outcome

    Code
      lift(Class ~ prob, data = three)
    Condition
      Error in `sensitivity.default()`:
      ! input data must have the same two levels

