# pg-analyze

## TODOs
- [ ] Make an init-db script
- [ ] As well as writing the EXPLAIN output to a file, print out a high-level overview of the tests using colonnade.
- [ ] Add the init command, which creates a testing 'environment' and runs the init-db script
- [ ] Use optparse-applicative to have handle commands
- [ ] Play with ReaderT with IO to implicitly pass the database connection around as well as default values like `./tests`
- [ ] Readme instructions
- [ ] Possibly introduce exception handling