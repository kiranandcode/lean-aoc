import Lake
open System Lake DSL

require "leanprover-community" / "batteries" @ git "main"

package AdventOfCode where
  srcDir := "."


@[default_target]
lean_lib Utils where

@[default_target]
lean_lib Solutions where



-- @[default_target]
-- lean_exe main_exe where 
--   root := `Main
--   name := `main

@[default_target]
lean_exe get_input where 
  root := `scripts.get_input
  name := `main


