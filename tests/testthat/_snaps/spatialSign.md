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

