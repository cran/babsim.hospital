options(babsim.run.full.test = FALSE)

if(!getOption("babsim.run.full.test")){
    skip("Skipping shorter tests due to option babsim.run.full.tests = FALSE in test.000TestSetup.R")
}