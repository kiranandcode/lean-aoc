# Advent of Code 2024 Lean4

Playing around with Advent of Code in Lean~

Misc programming utils and macros I need will be placed under `Utils/`

This library also provides a wrapper around the AOC API (using curl internally)
```
let input : IO String := AOC.getInput 3
-- retrieves input for day 3 for the current year (call is cached, so repeated evaluations will not make network calls)

let input : IO String := AOC.getInput 4 (.some 2023)
-- retrieves input for day 4 for 2023
```

Solutions will show up under `Solutions/`

Most solutions will be written using a simple evaluation macro I wrote:
```lean
#example (1 + 1  : Nat)
   evaluates to (2: Nat)
```
