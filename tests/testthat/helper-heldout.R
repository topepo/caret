# Fixtures for test-heldout.R.

# train_lev() has an isS4() branch for S4 model containers; this minimal class
# routes $ access into a list payload so the fake behaves like a train object.
setClass("fake_s4_train", representation(payload = "list"))
setMethod("$", "fake_s4_train", function(x, name) x@payload[[name]])
