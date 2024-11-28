import Lake
open System Lake DSL

require "leanprover-community" / "batteries" @ git "main"

package AdventOfCode where

lean_lib AdventOfCode where

@[default_target]
lean_exe main_exe where 
  root := `Main
  name := `main

@[default_target]
lean_exe get_input where 
  root := `Utils.AoCAPI
  name := `main


