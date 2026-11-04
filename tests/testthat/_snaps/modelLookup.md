# modelLookup errors for an unknown model

    Code
      modelLookup("nnnotamodel")
    Condition
      Error in `modelLookup()`:
      ! Model 'nnnotamodel' is not in the set of existing models

# getModelInfo returns the model definition

    Code
      getModelInfo("zzznomatchxyz")
    Condition
      Error in `getModelInfo()`:
      ! That model is not in caret's built-in library

# checkInstall is silent for installed packages and errors otherwise

    Code
      caret:::checkInstall("nopkg99xyz")
    Condition
      Error:
      ! Required packages are missing: nopkg99xyz

# checkInstall errors when the install is declined

    Code
      caret:::checkInstall("nopkg99xyz")
    Condition
      Error:
      ! Required packages are missing: nopkg99xyz

# install_prompt shows the message and returns the menu choice

    Code
      choice <- caret:::install_prompt("Install them now?")
    Output
      Install them now?

