# Posit Connect deployment helpers --------------------------------------------
#
# Run this script once after cloning or after adding/removing packages to
# (re)generate manifest.json, which Posit Connect uses for git-backed deploys.
#
# Prerequisites:
#   - renv::restore() has been run so the project library is up to date.
#   - rsconnect is installed (it is in renv.lock).

# 1. Regenerate manifest.json -------------------------------------------------
rsconnect::writeManifest()

# 2. (Optional) Push-button deploy to Posit Connect ---------------------------
# Fill in your server URL and account name, then uncomment.
#
# rsconnect::deployApp(
#   server  = "<your-posit-connect-server>",
#   account = "<your-account>",
#   appName = "shiny-firjan-ifdm"
# )
