## Bash commands

- `cabal build` build the project
- `cabal test` run tests
- `rg` (ripgrep) can be used to quickly search for strings in the codebase

## Code Style

- prefer using the `lens` package, don't use record accessors starting with '_' if there are lenses available
- use `wreq` package for http calls, tutorial at http://www.serpentine.com/wreq/tutorial.html
- use `tasty` for tests
- the TUI is built using `brick`, user guide at https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst

## Workflow

- run `cabal test` after implementing your change to make sure everything is okay
- keep a running list of TODOs when you are implementing a feature and
  check them one by one while doing the task
