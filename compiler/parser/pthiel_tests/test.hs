main :: IO ()
main = return ()


-- ############ Example Parsing Traces: ############
-- Right hand side is fexp -> fexp aexp -> fexp aexp aexp -> aexp aexp aexp
x = take 3 [1..3]

-- 'type' type '=' ctypedoc
type NewType = Int

-- type from the example above:
--     type -> btype -> tyapps -> tyapp -> atype ->
--         ntgtycon -> oqtycon -> qtycon -> tycon -> CONID


-- ############ Parsing Tests and Code: ############
-- Parsing example that definitely uses ntgtycon:
newtype N = N Int


-- The type of the exp -> type function we want to write should be the following:
pabloExpToType :: LHsExpr GhcPs -> LHsType GhcPs
pabloExpToType (sL1 x _) = sL1 x (HsTyVar noExt NotPromoted x)
-- Note: The above doesn't work since you can't pattern match on function
--   applications :(


-- ############ Notes: ############
{-
Currently, building fails when the '[' ']' production is left in ntgtycon
  because of the ambiguity of "[]" being in both ntgtycon as well as identifier
  or aexp2 as shown below:
    '[' ']' -> sysdcon -> con -> qcon -> identifier
                                      -> aexp2
  We can get GHC to compile by simply commenting out the '[' ']' production in
  ntgtycon, but it still parses LooksLikeATypeConButShouldParseAsAnExpressionNow
  as a data constructor (most likely due to precedence in face of reduce/reduce
  ambiguity). Need to figure out how to resolve this ambiguity while still
  allowing both kinds of usage (type constructors and exp variables).

  Maybe the way forward would be to have the parser not care about the
  distinction (i.e. have it treat both usages as the same up to a certain point,
  just like we've been talking about).

  Well actually, I guess what we really need is to remove ntgtycon from atype
  and put in a reference to ntgtycon from aexp2 (either by putting in aexp/aexp2
  as a production of atype directly, or by first making a smaller case so we
  don't have to think about the issues with putting all of aexp/aexp2's
  productions into atype at once. In any case, it's now crucial to write the
  parser function converting exps to types).

  ALSO: Where are the errors being generated? After parsing? If that's the case,
  in order to make the above parse we would need to change that part of the
  process, not just the parsing rules. And maybe there is no ambiguity above
  and things are parsing just how we'd like them to, and ghc is just rejecting
  the AST we've given it for non-parsing reasons (which would be great! We could
  just start working on the exp -> type function and moving stuff from atype
  to aexp2 and friends)
-}
