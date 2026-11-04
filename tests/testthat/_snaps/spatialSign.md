# errors working

    Code
      spatialSign(iris$Species)
    Condition
      Error:
      ! spatial sign is not defined for character or factor data

---

    Code
      spatialSign(as.matrix(iris))
    Condition
      Error:
      ! spatial sign is not defined for character data

---

    Code
      spatialSign(iris)
    Condition
      Error:
      ! spatial sign is not defined for character or factor data

# spatialSign rejects data frames that are not numeric

    Code
      spatialSign(data.frame(a = c(1 + 0+2i, 3 + 0+1i)))
    Condition
      Error:
      ! a character matrix was the result of as.matrix

