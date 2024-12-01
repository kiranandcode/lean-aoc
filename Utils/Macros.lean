import Batteries
import Lean.Elab.BuiltinEvalCommand

open Lean.Meta Lean.Elab

-- Most of this stolen from Lean/Elab/BuiltinEvalcommand.lean

/--
Try to make a `@projFn ty inst e` application, even if it takes unfolding the type `ty` of `e` to synthesize the instance `inst`.
-/
private partial def mkDeltaInstProj (inst projFn : Lean.Name) (e : Lean.Expr) (ty? : Option Lean.Expr := none) (tryReduce : Bool := true) : MetaM Lean.Expr := do
  let ty ← ty?.getDM (inferType e)
  if let .some inst ← trySynthInstance (← mkAppM inst #[ty]) then
    mkAppOptM projFn #[ty, inst, e]
  else
    let ty ← whnfCore ty
    let some ty ← unfoldDefinition? ty
      | guard tryReduce
        -- Reducing the type is a strategy `#eval` used before the refactor of #5627.
        -- The test lean/run/hlistOverload.lean depends on it, so we preserve the behavior.
        let ty ← reduce (skipTypes := false) ty
        mkDeltaInstProj inst projFn e ty (tryReduce := false)
    mkDeltaInstProj inst projFn e ty tryReduce

/-- Try to make a `toString e` application, even if it takes unfolding the type of `e` to find a `ToString` instance. -/
private def mkToString (e : Lean.Expr) (ty? : Option Lean.Expr := none) : MetaM Lean.Expr := do
  mkDeltaInstProj ``ToString ``toString e ty?

/-- Try to make a `repr e` application, even if it takes unfolding the type of `e` to find a `Repr` instance. -/
private def mkRepr (e : Lean.Expr) (ty? : Option Lean.Expr := none) : MetaM Lean.Expr := do
  mkDeltaInstProj ``Repr ``repr e ty?

def mkFormat (e : Lean.Expr) : MetaM Lean.Expr := open Lean in do
  mkRepr e <|> (do mkAppM ``Std.Format.text #[← mkToString e])
  <|> do
    if let .const name _ := (← whnf (← inferType e)).getAppFn then
      try
        trace[Elab.eval] "Attempting to derive a 'Repr' instance for '{.ofConstName name}'"
        liftCommandElabM do applyDerivingHandlers ``Repr #[name]
        resetSynthInstanceCache
        return ← mkRepr e
      catch ex =>
        trace[Elab.eval] "Failed to use derived 'Repr' instance. Exception: {ex.toMessageData}"
    throwError m!"could not synthesize a 'Repr' or 'ToString' instance for type{indentExpr (← inferType e)}"


private def elabTermForEval (term : Lean.Syntax) (expectedType? : Option Lean.Expr) : TermElabM Lean.Expr :=
  open Lean in do
  let ty ← expectedType?.getDM mkFreshTypeMVar
  let e ← Term.elabTermEnsuringType term ty
  synthesizeWithHinting ty
  let e ← instantiateMVars e
  if (← Term.logUnassignedUsingErrorInfos (← getMVars e)) then throwAbortTerm
  if ← isProof e then
    throwError m!"cannot evaluate, proofs are not computationally relevant"
  let e ← if (← isProp e) then mkDecide e else pure e
  if ← isType e then
    throwError m!"cannot evaluate, types are not computationally relevant"
  trace[Elab.eval] "elaborated term:{indentExpr e}"
  return e
where
  /-- Try different strategies to make `Term.synthesizeSyntheticMVarsNoPostponing` succeed. -/
  synthesizeWithHinting (ty : Lean.Expr) : TermElabM Unit := do
    Term.synthesizeSyntheticMVarsUsingDefault
    let s ← Term.saveState
    try
      Term.synthesizeSyntheticMVarsNoPostponing
    catch ex =>
      let exS ← Term.saveState
      -- Try hinting that `ty` is a monad application.
      for m in #[``Command.CommandElabM, ``TermElabM, ``IO] do
        s.restore true
        try
          if ← isDefEq ty (← mkFreshMonadApp m) then
            Term.synthesizeSyntheticMVarsNoPostponing
            return
        catch _ => pure ()
      -- None of the hints worked, so throw the original error.
      exS.restore true
      throw ex
  mkFreshMonadApp (n : Lean.Name) : MetaM Lean.Expr := do
    let m ← mkConstWithFreshMVarLevels n
    let (args, _, _) ← forallMetaBoundedTelescope (← inferType m) 1
    return Lean.mkAppN m args



private def addAndCompileExprForEval (declName : Lean.Name) (value : Lean.Expr) (allowSorry := false) : TermElabM Unit := do
  -- Use the `elabMutualDef` machinery to be able to support `let rec`.
  -- Hack: since we are using the `TermElabM` version, we can insert the `value` as a metavariable via `exprToSyntax`.
  -- An alternative design would be to make `elabTermForEval` into a term elaborator and elaborate the command all at once
  -- with `unsafe def _eval := term_for_eval% $t`, which we did try, but unwanted error messages
  -- such as "failed to infer definition type" can surface.
  let defView := Command.mkDefViewOfDef { isUnsafe := true }
    (← `(Lean.Parser.Command.definition|
          def $(Lean.mkIdent <| `_root_ ++ declName) := $(← Term.exprToSyntax value)))
  Term.elabMutualDef #[] { header := "" } #[defView]
  unless allowSorry do
    let axioms ← Lean.collectAxioms declName
    if axioms.contains ``sorryAx then
      throwError "\
        can not run the example as one of the expressions depends on the 'sorry' axiom, \
        which can lead to runtime instability and crashes.\n\n."

private unsafe def elabExampleCoreUnsafe (tk userStx expectedStx : Lean.Syntax) :
   Command.CommandElabM (String × String × Bool) :=
  open Lean in withRef tk do
  let wrapWithFormat e : TermElabM _ :=
      try mkFormat e catch ex => do
              try
                let some (m, ty) ← isTypeApp? (← inferType e) | failure
                guard <| (← isMonad? m).isSome
                -- Verify that there is a way to form some representation:
                discard <| withLocalDeclD `x ty fun x => mkFormat x
              catch _ =>
                throw ex
              throwError m!"unable to synthesize '{.ofConstName ``MonadEval}' instance \
                to adapt{indentExpr (← inferType e)}\n\
                to '{.ofConstName ``IO}' or '{.ofConstName ``Command.CommandElabM}'."  
  let toString e : TermElabM _ := Lean.withoutModifyingEnv do
    let declName := `_exampleToString
    let r ← wrapWithFormat e
    addAndCompileExprForEval declName r (allowSorry := false)
    -- `evalConst` may emit IO, but this is collected by `withIsolatedStreams` below.
    let r ← Lean.MessageData.ofFormat <$> evalConst Format declName
    return <- MessageData.toString r
  let declEq :=  `_exampleEq
  let checkMonadEvalIO e : TermElabM _ := observing? (mkAppOptM  ``MonadEvalT.monadEval #[none, mkConst ``IO, none, none, e])
  let (output, exOrRes) ← IO.FS.withIsolatedStreams do
    try
       Command.liftTermElabM $ Lean.withoutModifyingEnv $ do
          let eExpt <- elabTermForEval expectedStx .none
          let eUser <- elabTermForEval userStx .none
          if eExpt.hasSyntheticSorry || eUser.hasSyntheticSorry then throwAbortTerm
          match
            (<- Term.withDeclName declEq $ checkMonadEvalIO eUser)
          with
          | .none =>
             let eEq <- mkAppM ``BEq.beq  #[eExpt, eUser]
             Term.withDeclName declEq $ addAndCompileExprForEval declEq eEq (allowSorry := false)
             let sExpt <- toString eExpt
             let sUser <- toString eUser
             let bl <- Lean.evalConst Bool declEq
             return Sum.inr (sExpt, sUser, bl)
          | .some e => 
             -- term is not pure
             let eType := e.appFn!.appArg!
             -- function to apply Format
             -- function to apply eq
             let rfPrintEq ← withLocalDeclD `x eType fun x => do mkLambdaFVars #[x] (←
                mkPProdMk
                  (← mkFormat x)
                  (<- mkAppM ``BEq.beq #[x, eExpt]))
             let rPrintEq ← mkAppM ``Functor.map #[rfPrintEq, e]
             Term.withDeclName declEq $ addAndCompileExprForEval declEq rPrintEq (allowSorry := false)
             let sExpt <- toString eExpt
             let mUserEq : IO (Format × Bool) ← evalConst (IO (Format × Bool)) declEq
             let (fUser, bl) <- mUserEq
             let sUser <- MessageData.toString fUser
             return Sum.inr (sExpt, sUser, bl)
    catch ex =>
      return Sum.inl ex
  if !output.isEmpty then logInfoAt tk output
  match exOrRes with
  | .inl ex => throw ex
  | .inr res => return res

@[implemented_by  elabExampleCoreUnsafe]
private opaque elabExampleCore (tk userStx expectedStx : Lean.Syntax) : Command.CommandElabM (String × String × Bool)

syntax "#example" term " evaluates " " to " term : command

elab_rules : command
| `(command| #example%$tk $tUserStx:term evaluates to $tExpectedStx:term) => do
   -- get the type of expected, as probs this can be worked out statically
   let (RHS, LHS, tExpected) <- elabExampleCore tk tUserStx tExpectedStx

   if !tExpected then
     throwErrorAt tUserStx
     "LHS {tUserStx}
does not evaluate to {RHS}.
but instead {LHS}"
   

-- usage of the macro
#example ([1].map fun x => x + 1) evaluates to [3 - 1]
