# pg-analyze

## TODOs
- [x] Make an init-db script
- [ ] As well as writing the EXPLAIN output to a file, print out a high-level overview of the tests using colonnade.
- [ ] Add the init command, which creates a testing 'environment' and runs the init-db script
- [ ] Use optparse-applicative to handle commands
- [ ] Readme instructions
- [ ] Play with ReaderT with IO to implicitly pass the database connection around as well as default values like `./tests`
- [ ] Possibly introduce exception handling
- [ ] Add functionality to automatically open the explain outputs onto a browser?

## QUESTIONS
- Is there a way to avoid parsing an Explain.Config and then converting it to an ExplainPlan? As in can one 'tap' into fromJSON and supply any
additional information (in this case the filename)
- Tests? It's basically a bunch of side effects with a tiny bit of logic
