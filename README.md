# pg-analyze

A simple tool to troubleshoot postgreSQL query performance, built around the `EXPLAIN ANALYZE` command.

## Rationale

SQL is fantastic at letting you worry about the _what_ rather than the _how_.

However, if you are trying to figure out a slow query, you are likely going to use the [EXPLAIN](https://www.postgresql.org/docs/current/sql-explain.html)
command, which shows you _how_ a given query is going to be executed.

The problem is that if you are trying out different indexes/queries (let's call these tests), you need to:

- reset the database to an 'initial state' between every test
- keep track of the outcomes for each test, so you can compare them

This tool helps you with the above:

1. you set up as many tests as you want in the `./tests` directory. Tests look like this:

```yaml
# ./tests/someTest.yml

setup: >
  CREATE INDEX first_name ON public.employees (first_name);
query: >
  SELECT * FROM public.employees
  WHERE first_name = 'mario'
```

2. you run the tool and get a high-level output table as well as each `EXPLAIN` output stored to the `./explained`
directory. The high-level output looks like this:
   
```
running explain tests...
+-----------------+------------+
| Test name       | Total cost |
+-----------------+------------+
| someTest.yml    | 177.24     |
| sampleTest.yml  | 1554.0     |
+-----------------+------------+
```

## Installation

### Note on security and trust
The binary we distribute is not signed for MacOS. This has two implications:

1. Apple have not verified/security-scanned this software.
2. If you are happy to proceed and use this, you will need to add an exception for this program in the
   'Security & Privacy' menu.
   
If you are not comfortable with this, an alternative is to clone this repository and build the binary yourself
using [stack](https://github.com/commercialhaskell/stack)

If you are happy with the above:

- Download a release from [this page](https://github.com/andreamazza89/pg-analyze/releases)
- Mark the downloaded binary as executable with `chmod +x <PATH YOU DOWNLOADED TO>/pg-analyze`
- To run the program, either 
  - move the binary into a directory within your path. You can now run with just `pg-analyze`
  - use the full path to the binary `<PATH YOU DOWNLOADED TO>/pg-analyze`

## How to use

- Create a directory for your tests - e.g. `mkdir myPgTests && cd myPgTests`
- Initialise the directory for pg-analyze - `pg-analyze initialise` (this creates a few files/folders within the current
  directory, and a test_db database in your postgres server)
- You can now run the example with `pg-analyze analyze`
- Create a dump of your database with `pg_dump --inserts -c <NAME OF DB> > dump.sql`
- Replace the sample dump.sql with the one you created and load it onto the test db `psql test_db < dump.sql`
- Update (or remove) the sample query in `/tests/sampleTest` and create new ones as needed.
- You can now run your tests with  `pg-analyze analyze`

## TODOs
- [x] Make an init-db script
- [x] As well as writing the EXPLAIN output to a file, print out a high-level overview of the tests using colonnade.
- [x] Add the init command, which creates a testing 'environment' and runs the init-db script
- [x] Use optparse-applicative to handle commands
- [ ] Readme instructions
  - [x] Rationale
  - [x] Installation
  - [x] How to use
- [ ] Configurable db details/connection
- [x] Play with ReaderT (with IO?) to implicitly pass the database connection around as well as default values like `./tests`
- [ ] Possibly introduce exception handling
- [ ] Add functionality to automatically open the explain outputs onto a browser?

## QUESTIONS
- Is there a way to avoid parsing an Explain.Config and then converting it to an ExplainPlan? As in can one 'tap' into fromJSON and supply any
additional information (in this case the filename)
- Tests? It's basically a bunch of side effects with a tiny bit of logic
