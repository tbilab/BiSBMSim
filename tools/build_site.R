# Build docs
devtools::document()

# Make sure all tests are passing
devtools::test(stop_on_failure = TRUE)

# Build site
pkgdown::build_site()

# Deploy package site to RStudio connect
site_loc <- here::here('docs/')
rsconnect::deployApp(
  site_loc,                                          # the directory containing the content
  appFiles = list.files(site_loc, recursive = TRUE), # the list of files to include as dependencies (all of them)
  appPrimaryDoc = "index.html",                      # the primary file
  appName = "biSBMSim_Package",                      # name of the endpoint (unique to your account on Connect)
  appTitle = "biSBMSim Package",                     # display name for the content
  account = 'nstrayer',                              # your Connect username
  server = 'prod.tbilab.org'                         # the Connect server, see rsconnect::accounts()
)

