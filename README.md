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

## TODOs
- [x] Make an init-db script
- [x] As well as writing the EXPLAIN output to a file, print out a high-level overview of the tests using colonnade.
- [x] Add the init command, which creates a testing 'environment' and runs the init-db script
- [x] Use optparse-applicative to handle commands
- [ ] Readme instructions
  - [x] Rationale
  - [ ] Installation
  - [ ] How to use
- [ ] Configurable db details/connection
- [x] Play with ReaderT (with IO?) to implicitly pass the database connection around as well as default values like `./tests`
- [ ] Possibly introduce exception handling
- [ ] Add functionality to automatically open the explain outputs onto a browser?
- [ ] Add functionality to compare the output of different queries for equality?

## QUESTIONS
- Is there a way to avoid parsing an Explain.Config and then converting it to an ExplainPlan? As in can one 'tap' into fromJSON and supply any
additional information (in this case the filename)
- Tests? It's basically a bunch of side effects with a tiny bit of logic
