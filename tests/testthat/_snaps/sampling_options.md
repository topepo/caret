# check bad sampling name

    Code
      caret:::parse_sampling("what?")
    Condition
      Error:
      ! That sampling scheme is not in caret's built-in library

# check bad first arg

    Code
      caret:::parse_sampling(list(name = "yep", func = sampling_methods[["up"]],
      first = 2), check_install = FALSE)
    Condition
      Error:
      ! The element 'first' should be a logical

# check bad func arg

    Code
      caret:::parse_sampling(list(name = "yep", func = I, first = 2), check_install = FALSE)
    Condition
      Error:
      ! the 'sampling' function should have arguments 'x' and 'y'

# check incomplete list

    Code
      caret:::parse_sampling(list(name = "yep"), check_install = FALSE)
    Condition
      Error:
      ! the 'sampling' list should have elements first, func, name

# check call

    Code
      caret:::parse_sampling(14, check_install = FALSE)
    Condition
      Error:
      ! The sampling argument should be either a string, function, or list. See http://topepo.github.io/caret/model-training-and-tuning.html

# check missing method

    Code
      getSamplingInfo("plum")
    Condition
      Error:
      ! That sampling method is not in caret's built-in library

