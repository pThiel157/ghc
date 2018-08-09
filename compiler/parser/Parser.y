--                                                              -*-haskell-*-
-- ---------------------------------------------------------------------------
-- (c) The University of Glasgow 1997-2003
---
-- The GHC grammar.
--
-- Author(s): Simon Marlow, Sven Panne 1997, 1998, 1999
-- ---------------------------------------------------------------------------

{
-- | This module provides the generated Happy parser for Haskell. It exports
-- a number of parsers which may be used in any library that uses the GHC API.
-- A common usage pattern is to initialize the parser state with a given string
-- and then parse that string:
--
-- @
--     runParser :: DynFlags -> String -> P a -> ParseResult a
--     runParser flags str parser = unP parser parseState
--     where
--       filename = "\<interactive\>"
--       location = mkRealSrcLoc (mkFastString filename) 1 1
--       buffer = stringToStringBuffer str
--       parseState = mkPState flags buffer location
-- @
module Parser (parseModule, parseSignature, parseImport, parseStatement, parseBackpack,
               parseDeclaration, parseExpression, parsePattern,
               parseTypeSignature,
               parseStmt, parseIdentifier,
               parseType, parseHeader) where

-- base
import Control.Monad    ( unless, liftM )
import GHC.Exts
import Data.Char
import Control.Monad    ( mplus )
import Control.Applicative ((<$))

-- compiler/hsSyn
import HsSyn
import HsTerm

-- compiler/main
import HscTypes         ( IsBootInterface, WarningTxt(..) )
import DynFlags
import BkpSyn
import PackageConfig

-- compiler/utils
import OrdList
import BooleanFormula   ( BooleanFormula(..), LBooleanFormula(..), mkTrue )
import FastString
import Maybes           ( isJust, orElse )
import Outputable

-- compiler/basicTypes
import RdrName
--EF
import OccName          ( NameSpace, varName, dataName, tcClsName, tvName, startsWithUnderscore, occNameFS, occNameSpace, isVarNameSpace, isDataConNameSpace, isTvNameSpace )
import Name             (nameUnique, wiredInNameTyThing_maybe)
import TysWiredIn       (nilDataConKey)
import PrelNames        (consDataConKey)
import Id               (isDataConId_maybe)
import TyCoRep
--EF
--import OccName          --( OccName, varName, dataName, tcClsName, tvName, startsWithUnderscore )
-- EF
import DataCon          ( DataCon, dataConName, dataConTyCon )
import SrcLoc
import Module
import BasicTypes

-- compiler/types
import Type             ( funTyCon )
import Kind             ( Kind )
import Class            ( FunDep )

-- compiler/parser
import RdrHsSyn
import Lexer
import HaddockUtils
import ApiAnnotation
--EF
import Lexeme           (isLexVarSym, isLexVarId)
--EF

-- compiler/typecheck
import TcEvidence       ( emptyTcEvBinds )

-- compiler/prelude
import ForeignCall
import TysPrim          ( eqPrimTyCon )
import PrelNames        ( eqTyCon_RDR )
import TysWiredIn       ( unitTyCon, unitDataCon, tupleTyCon, tupleDataCon, nilDataCon,
                          unboxedUnitTyCon, unboxedUnitDataCon,
                          listTyCon_RDR, consDataCon_RDR )

-- compiler/utils
import Util             ( looksLikePackageName )
import GhcPrelude

import qualified GHC.LanguageExtensions as LangExt
}

--%expect 235 -- shift/reduce conflicts

{- Last updated: 04 June 2018

If you modify this parser and add a conflict, please update this comment.
You can learn more about the conflicts by passing 'happy' the -i flag:

    happy -agc --strict compiler/parser/Parser.y -idetailed-info

How is this section formatted? Look up the state the conflict is
reported at, and copy the list of applicable rules (at the top, without the
rule numbers).  Mark *** for the rule that is the conflicting reduction (that
is, the interpretation which is NOT taken).  NB: Happy doesn't print a rule
in a state if it is empty, but you should include it in the list (you can
look these up in the Grammar section of the info file).

Obviously the state numbers are not stable across modifications to the parser,
the idea is to reproduce enough information on each conflict so you can figure
out what happened if the states were renumbered.  Try not to gratuitously move
productions around in this file.

-------------------------------------------------------------------------------

state 0 contains 1 shift/reduce conflicts.

    Conflicts: DOCNEXT (empty missing_module_keyword reduces)

Ambiguity when the source file starts with "-- | doc". We need another
token of lookahead to determine if a top declaration or the 'module' keyword
follows. Shift parses as if the 'module' keyword follows.

-------------------------------------------------------------------------------

state 57 contains 2 shift/reduce conflicts.

    *** strict_mark -> unpackedness .
        strict_mark -> unpackedness . strictness

    Conflicts: '~' '!'

-------------------------------------------------------------------------------

state 61 contains 1 shift/reduce conflict.

        context -> btype .
    *** type -> btype .
        type -> btype . '->' ctype

    Conflicts: '->'

-------------------------------------------------------------------------------

state 62 contains 46 shift/reduce conflicts.

    *** btype -> tyapps .
        tyapps -> tyapps . tyapp

    Conflicts: '_' ':' '~' '!' '.' '`' '{' '[' '[:' '(' '(#' '`' SIMPLEQUOTE
      VARID CONID VARSYM CONSYM QCONID QVARSYM QCONSYM
      STRING INTEGER TH_ID_SPLICE '$(' TH_QUASIQUOTE TH_QQUASIQUOTE
      and all the special ids.

Example ambiguity:
    'if x then y else z :: F a'

Shift parses as (per longest-parse rule):
    'if x then y else z :: (F a)'

-------------------------------------------------------------------------------

state 144 contains 15 shift/reduce conflicts.

        exp -> infixexp . '::' sigtype
        exp -> infixexp . '-<' exp
        exp -> infixexp . '>-' exp
        exp -> infixexp . '-<<' exp
        exp -> infixexp . '>>-' exp
    *** exp -> infixexp .
        infixexp -> infixexp . qop exp10

    Conflicts: ':' '::' '-' '!' '-<' '>-' '-<<' '>>-'
               '.' '`' '*' VARSYM CONSYM QVARSYM QCONSYM

Examples of ambiguity:
    'if x then y else z -< e'
    'if x then y else z :: T'
    'if x then y else z + 1' (NB: '+' is in VARSYM)

Shift parses as (per longest-parse rule):
    'if x then y else (z -< T)'
    'if x then y else (z :: T)'
    'if x then y else (z + 1)'

-------------------------------------------------------------------------------

state 149 contains 67 shift/reduce conflicts.

    *** exp10 -> fexp .
        fexp -> fexp . aexp
        fexp -> fexp . TYPEAPP atype

    Conflicts: TYPEAPP and all the tokens that can start an aexp

Examples of ambiguity:
    'if x then y else f z'
    'if x then y else f @ z'

Shift parses as (per longest-parse rule):
    'if x then y else (f z)'
    'if x then y else (f @ z)'

-------------------------------------------------------------------------------

state 204 contains 27 shift/reduce conflicts.

        aexp2 -> TH_TY_QUOTE . tyvar
        aexp2 -> TH_TY_QUOTE . gtycon
    *** aexp2 -> TH_TY_QUOTE .

    Conflicts: two single quotes is error syntax with specific error message.

Example of ambiguity:
    'x = '''
    'x = ''a'
    'x = ''T'

Shift parses as (per longest-parse rule):
    'x = ''a'
    'x = ''T'

-------------------------------------------------------------------------------

state 300 contains 1 shift/reduce conflicts.

        rule -> STRING . rule_activation rule_forall infixexp '=' exp

    Conflict: '[' (empty rule_activation reduces)

We don't know whether the '[' starts the activation or not: it
might be the start of the declaration with the activation being
empty.  --SDM 1/4/2002

Example ambiguity:
    '{-# RULE [0] f = ... #-}'

We parse this as having a [0] rule activation for rewriting 'f', rather
a rule instructing how to rewrite the expression '[0] f'.

-------------------------------------------------------------------------------

state 310 contains 1 shift/reduce conflict.

    *** type -> btype .
        type -> btype . '->' ctype

    Conflict: '->'

Same as state 61 but without contexts.

-------------------------------------------------------------------------------

state 354 contains 1 shift/reduce conflicts.

        tup_exprs -> commas . tup_tail
        sysdcon_nolist -> '(' commas . ')'
        commas -> commas . ','

    Conflict: ')' (empty tup_tail reduces)

A tuple section with NO free variables '(,,)' is indistinguishable
from the Haskell98 data constructor for a tuple.  Shift resolves in
favor of sysdcon, which is good because a tuple section will get rejected
if -XTupleSections is not specified.

-------------------------------------------------------------------------------

state 409 contains 1 shift/reduce conflicts.

        tup_exprs -> commas . tup_tail
        sysdcon_nolist -> '(#' commas . '#)'
        commas -> commas . ','

    Conflict: '#)' (empty tup_tail reduces)

Same as State 354 for unboxed tuples.

-------------------------------------------------------------------------------

state 417 contains 67 shift/reduce conflicts.

    *** exp10 -> '-' fexp .
        fexp -> fexp . aexp
        fexp -> fexp . TYPEAPP atype

Same as 149 but with a unary minus.

-------------------------------------------------------------------------------

state 481 contains 1 shift/reduce conflict.

        oqtycon -> '(' qtyconsym . ')'
    *** qtyconop -> qtyconsym .

    Conflict: ')'

Example ambiguity: 'foo :: (:%)'

Shift means '(:%)' gets parsed as a type constructor, rather than than a
parenthesized infix type expression of length 1.

-------------------------------------------------------------------------------

state 675 contains 1 shift/reduce conflicts.

    *** aexp2 -> ipvar .
        dbind -> ipvar . '=' exp

    Conflict: '='

Example ambiguity: 'let ?x ...'

The parser can't tell whether the ?x is the lhs of a normal binding or
an implicit binding.  Fortunately, resolving as shift gives it the only
sensible meaning, namely the lhs of an implicit binding.

-------------------------------------------------------------------------------

state 752 contains 1 shift/reduce conflicts.

        rule -> STRING rule_activation . rule_forall infixexp '=' exp

    Conflict: 'forall' (empty rule_forall reduces)

Example ambiguity: '{-# RULES "name" forall = ... #-}'

'forall' is a valid variable name---we don't know whether
to treat a forall on the input as the beginning of a quantifier
or the beginning of the rule itself.  Resolving to shift means
it's always treated as a quantifier, hence the above is disallowed.
This saves explicitly defining a grammar for the rule lhs that
doesn't include 'forall'.

-------------------------------------------------------------------------------

state 986 contains 1 shift/reduce conflicts.

        transformqual -> 'then' 'group' . 'using' exp
        transformqual -> 'then' 'group' . 'by' exp 'using' exp
    *** special_id -> 'group' .

    Conflict: 'by'

-------------------------------------------------------------------------------

state 1367 contains 1 shift/reduce conflict.

    *** atype -> tyvar .
        tv_bndr -> '(' tyvar . '::' kind ')'

    Conflict: '::'

Example ambiguity: 'class C a where type D a = ( a :: * ...'

Here the parser cannot tell whether this is specifying a default for the
associated type like:

'class C a where type D a = ( a :: * ); type D a'

or it is an injectivity signature like:

'class C a where type D a = ( r :: * ) | r -> a'

Shift means the parser only allows the latter.

-------------------------------------------------------------------------------
-- API Annotations
--

A lot of the productions are now cluttered with calls to
aa,am,ams,amms etc.

These are helper functions to make sure that the locations of the
various keywords such as do / let / in are captured for use by tools
that want to do source to source conversions, such as refactorers or
structured editors.

The helper functions are defined at the bottom of this file.

See
  https://ghc.haskell.org/trac/ghc/wiki/ApiAnnotations and
  https://ghc.haskell.org/trac/ghc/wiki/GhcAstAnnotations
for some background.

If you modify the parser and want to ensure that the API annotations are processed
correctly, see the README in (REPO)/utils/check-api-annotations for details on
how to set up a test using the check-api-annotations utility, and interpret the
output it generates.

Note [Parsing lists]
---------------------
You might be wondering why we spend so much effort encoding our lists this
way:

importdecls
        : importdecls ';' importdecl
        | importdecls ';'
        | importdecl
        | {- empty -}

This might seem like an awfully roundabout way to declare a list; plus, to add
insult to injury you have to reverse the results at the end.  The answer is that
left recursion prevents us from running out of stack space when parsing long
sequences.  See: https://www.haskell.org/happy/doc/html/sec-sequences.html for
more guidance.

By adding/removing branches, you can affect what lists are accepted.  Here
are the most common patterns, rewritten as regular expressions for clarity:

    -- Equivalent to: ';'* (x ';'+)* x?  (can be empty, permits leading/trailing semis)
    xs : xs ';' x
       | xs ';'
       | x
       | {- empty -}

    -- Equivalent to x (';' x)* ';'*  (non-empty, permits trailing semis)
    xs : xs ';' x
       | xs ';'
       | x

    -- Equivalent to ';'* alts (';' alts)* ';'* (non-empty, permits leading/trailing semis)
    alts : alts1
         | ';' alts
    alts1 : alts1 ';' alt
          | alts1 ';'
          | alt

    -- Equivalent to x (',' x)+ (non-empty, no trailing semis)
    xs : x
       | x ',' xs

-- -----------------------------------------------------------------------------

-}

%token
 '_'            { L _ ITunderscore }            -- Haskell keywords
 'as'           { L _ ITas }
 'case'         { L _ ITcase }
 'class'        { L _ ITclass }
 'data'         { L _ ITdata }
 'default'      { L _ ITdefault }
 'deriving'     { L _ ITderiving }
 'do'           { L _ ITdo }
 'else'         { L _ ITelse }
 'hiding'       { L _ IThiding }
 'if'           { L _ ITif }
 'import'       { L _ ITimport }
 'in'           { L _ ITin }
 'infix'        { L _ ITinfix }
 'infixl'       { L _ ITinfixl }
 'infixr'       { L _ ITinfixr }
 'instance'     { L _ ITinstance }
 'let'          { L _ ITlet }
 'module'       { L _ ITmodule }
 'newtype'      { L _ ITnewtype }
 'of'           { L _ ITof }
 'qualified'    { L _ ITqualified }
 'then'         { L _ ITthen }
 'type'         { L _ ITtype }
 'where'        { L _ ITwhere }

 'forall'       { L _ (ITforall _) }                -- GHC extension keywords
 'foreign'      { L _ ITforeign }
 'export'       { L _ ITexport }
 'label'        { L _ ITlabel }
 'dynamic'      { L _ ITdynamic }
 'safe'         { L _ ITsafe }
 'interruptible' { L _ ITinterruptible }
 'unsafe'       { L _ ITunsafe }
 'mdo'          { L _ ITmdo }
 'family'       { L _ ITfamily }
 'role'         { L _ ITrole }
 'stdcall'      { L _ ITstdcallconv }
 'ccall'        { L _ ITccallconv }
 'capi'         { L _ ITcapiconv }
 'prim'         { L _ ITprimcallconv }
 'javascript'   { L _ ITjavascriptcallconv }
 'proc'         { L _ ITproc }          -- for arrow notation extension
 'rec'          { L _ ITrec }           -- for arrow notation extension
 'group'    { L _ ITgroup }     -- for list transform extension
 'by'       { L _ ITby }        -- for list transform extension
 'using'    { L _ ITusing }     -- for list transform extension
 'pattern'      { L _ ITpattern } -- for pattern synonyms
 'static'       { L _ ITstatic }  -- for static pointers extension
 'stock'        { L _ ITstock }    -- for DerivingStrategies extension
 'anyclass'     { L _ ITanyclass } -- for DerivingStrategies extension
 'via'          { L _ ITvia }      -- for DerivingStrategies extension

 'unit'         { L _ ITunit }
 'signature'    { L _ ITsignature }
 'dependency'   { L _ ITdependency }

 '{-# INLINE'             { L _ (ITinline_prag _ _ _) } -- INLINE or INLINABLE
 '{-# SPECIALISE'         { L _ (ITspec_prag _) }
 '{-# SPECIALISE_INLINE'  { L _ (ITspec_inline_prag _ _) }
 '{-# SOURCE'             { L _ (ITsource_prag _) }
 '{-# RULES'              { L _ (ITrules_prag _) }
 '{-# CORE'               { L _ (ITcore_prag _) }      -- hdaume: annotated core
 '{-# SCC'                { L _ (ITscc_prag _)}
 '{-# GENERATED'          { L _ (ITgenerated_prag _) }
 '{-# DEPRECATED'         { L _ (ITdeprecated_prag _) }
 '{-# WARNING'            { L _ (ITwarning_prag _) }
 '{-# UNPACK'             { L _ (ITunpack_prag _) }
 '{-# NOUNPACK'           { L _ (ITnounpack_prag _) }
 '{-# ANN'                { L _ (ITann_prag _) }
 '{-# MINIMAL'            { L _ (ITminimal_prag _) }
 '{-# CTYPE'              { L _ (ITctype _) }
 '{-# OVERLAPPING'        { L _ (IToverlapping_prag _) }
 '{-# OVERLAPPABLE'       { L _ (IToverlappable_prag _) }
 '{-# OVERLAPS'           { L _ (IToverlaps_prag _) }
 '{-# INCOHERENT'         { L _ (ITincoherent_prag _) }
 '{-# COMPLETE'           { L _ (ITcomplete_prag _)   }
 '#-}'                    { L _ ITclose_prag }

 '..'           { L _ ITdotdot }                        -- reserved symbols
 ':'            { L _ ITcolon }
 '::'           { L _ (ITdcolon _) }
 '='            { L _ ITequal }
 '\\'           { L _ ITlam }
 'lcase'        { L _ ITlcase }
 '|'            { L _ ITvbar }
 '<-'           { L _ (ITlarrow _) }
 '->'           { L _ (ITrarrow _) }
 '@'            { L _ ITat }
 '~'            { L _ ITtilde }
 '=>'           { L _ (ITdarrow _) }
 '-'            { L _ ITminus }
 '!'            { L _ ITbang }
 '*'            { L _ (ITstar _) }
 '-<'           { L _ (ITlarrowtail _) }            -- for arrow notation
 '>-'           { L _ (ITrarrowtail _) }            -- for arrow notation
 '-<<'          { L _ (ITLarrowtail _) }            -- for arrow notation
 '>>-'          { L _ (ITRarrowtail _) }            -- for arrow notation
 '.'            { L _ ITdot }
 TYPEAPP        { L _ ITtypeApp }

 '{'            { L _ ITocurly }                        -- special symbols
 '}'            { L _ ITccurly }
 vocurly        { L _ ITvocurly } -- virtual open curly (from layout)
 vccurly        { L _ ITvccurly } -- virtual close curly (from layout)
 '['            { L _ ITobrack }
 ']'            { L _ ITcbrack }
 '[:'           { L _ ITopabrack }
 ':]'           { L _ ITcpabrack }
 '('            { L _ IToparen }
 ')'            { L _ ITcparen }
 '(#'           { L _ IToubxparen }
 '#)'           { L _ ITcubxparen }
 '(|'           { L _ (IToparenbar _) }
 '|)'           { L _ (ITcparenbar _) }
 ';'            { L _ ITsemi }
 ','            { L _ ITcomma }
 '`'            { L _ ITbackquote }
 SIMPLEQUOTE    { L _ ITsimpleQuote      }     -- 'x

 VARID          { L _ (ITvarid    _) }          -- identifiers
 CONID          { L _ (ITconid    _) }
 VARSYM         { L _ (ITvarsym   _) }
 CONSYM         { L _ (ITconsym   _) }
 QVARID         { L _ (ITqvarid   _) }
 QCONID         { L _ (ITqconid   _) }
 QVARSYM        { L _ (ITqvarsym  _) }
 QCONSYM        { L _ (ITqconsym  _) }

 IPDUPVARID     { L _ (ITdupipvarid   _) }              -- GHC extension
 LABELVARID     { L _ (ITlabelvarid   _) }

 CHAR           { L _ (ITchar   _ _) }
 STRING         { L _ (ITstring _ _) }
 INTEGER        { L _ (ITinteger _) }
 RATIONAL       { L _ (ITrational _) }

 PRIMCHAR       { L _ (ITprimchar   _ _) }
 PRIMSTRING     { L _ (ITprimstring _ _) }
 PRIMINTEGER    { L _ (ITprimint    _ _) }
 PRIMWORD       { L _ (ITprimword   _ _) }
 PRIMFLOAT      { L _ (ITprimfloat  _) }
 PRIMDOUBLE     { L _ (ITprimdouble _) }

 DOCNEXT        { L _ (ITdocCommentNext _) }
 DOCPREV        { L _ (ITdocCommentPrev _) }
 DOCNAMED       { L _ (ITdocCommentNamed _) }
 DOCSECTION     { L _ (ITdocSection _ _) }

-- Template Haskell
'[|'            { L _ (ITopenExpQuote _ _) }
'[p|'           { L _ ITopenPatQuote  }
'[t|'           { L _ ITopenTypQuote  }
'[d|'           { L _ ITopenDecQuote  }
'|]'            { L _ (ITcloseQuote _) }
'[||'           { L _ (ITopenTExpQuote _) }
'||]'           { L _ ITcloseTExpQuote  }
TH_ID_SPLICE    { L _ (ITidEscape _)  }     -- $x
'$('            { L _ ITparenEscape   }     -- $( exp )
TH_ID_TY_SPLICE { L _ (ITidTyEscape _)  }   -- $$x
'$$('           { L _ ITparenTyEscape   }   -- $$( exp )
TH_TY_QUOTE     { L _ ITtyQuote       }      -- ''T
TH_QUASIQUOTE   { L _ (ITquasiQuote _) }
TH_QQUASIQUOTE  { L _ (ITqQuasiQuote _) }

%monad { P } { >>= } { return }
%lexer { (lexer True) } { L _ ITeof }
%tokentype { (Located Token) }

-- Exported parsers
%name parseModule module
%name parseSignature signature
%name parseImport importdecl
%name parseStatement stmt
%name parseDeclaration topdecl
%name parseExpression exp
%name parsePattern pat
%name parseTypeSignature sigdecl
%name parseStmt   maybe_stmt
%name parseIdentifier  identifier
%name parseType ctype
%name parseBackpack backpack
%partial parseHeader header
%%

-----------------------------------------------------------------------------
-- Identifiers; one of the entry points
identifier :: { Located RdrName }
        : qvar                          { $1 }
        | qcon                          { $1 }
        | qvarop                        { $1 }
        | qconop                        { $1 }
    | '(' '->' ')'      {% ams (sLL $1 $> $ getRdrName funTyCon)
                               [mj AnnOpenP $1,mu AnnRarrow $2,mj AnnCloseP $3] }

-----------------------------------------------------------------------------
-- Backpack stuff

backpack :: { [LHsUnit PackageName] }
         : implicit_top units close { fromOL $2 }
         | '{' units '}'            { fromOL $2 }

units :: { OrdList (LHsUnit PackageName) }
         : units ';' unit { $1 `appOL` unitOL $3 }
         | units ';'      { $1 }
         | unit           { unitOL $1 }

unit :: { LHsUnit PackageName }
        : 'unit' pkgname 'where' unitbody
            { sL1 $1 $ HsUnit { hsunitName = $2
                              , hsunitBody = fromOL $4 } }

unitid :: { LHsUnitId PackageName }
        : pkgname                  { sL1 $1 $ HsUnitId $1 [] }
        | pkgname '[' msubsts ']'  { sLL $1 $> $ HsUnitId $1 (fromOL $3) }

msubsts :: { OrdList (LHsModuleSubst PackageName) }
        : msubsts ',' msubst { $1 `appOL` unitOL $3 }
        | msubsts ','        { $1 }
        | msubst             { unitOL $1 }

msubst :: { LHsModuleSubst PackageName }
        : modid '=' moduleid { sLL $1 $> $ ($1, $3) }
        | modid VARSYM modid VARSYM { sLL $1 $> $ ($1, sLL $2 $> $ HsModuleVar $3) }

moduleid :: { LHsModuleId PackageName }
          : VARSYM modid VARSYM { sLL $1 $> $ HsModuleVar $2 }
          | unitid ':' modid    { sLL $1 $> $ HsModuleId $1 $3 }

pkgname :: { Located PackageName }
        : STRING     { sL1 $1 $ PackageName (getSTRING $1) }
        | litpkgname { sL1 $1 $ PackageName (unLoc $1) }

litpkgname_segment :: { Located FastString }
        : VARID  { sL1 $1 $ getVARID $1 }
        | CONID  { sL1 $1 $ getCONID $1 }
        | special_id { $1 }

litpkgname :: { Located FastString }
        : litpkgname_segment { $1 }
        -- a bit of a hack, means p - b is parsed same as p-b, enough for now.
        | litpkgname_segment '-' litpkgname  { sLL $1 $> $ appendFS (unLoc $1) (consFS '-' (unLoc $3)) }

mayberns :: { Maybe [LRenaming] }
        : {- empty -} { Nothing }
        | '(' rns ')' { Just (fromOL $2) }

rns :: { OrdList LRenaming }
        : rns ',' rn { $1 `appOL` unitOL $3 }
        | rns ','    { $1 }
        | rn         { unitOL $1 }

rn :: { LRenaming }
        : modid 'as' modid { sLL $1 $> $ Renaming $1 (Just $3) }
        | modid            { sL1 $1    $ Renaming $1 Nothing }

unitbody :: { OrdList (LHsUnitDecl PackageName) }
        : '{'     unitdecls '}'   { $2 }
        | vocurly unitdecls close { $2 }

unitdecls :: { OrdList (LHsUnitDecl PackageName) }
        : unitdecls ';' unitdecl { $1 `appOL` unitOL $3 }
        | unitdecls ';'         { $1 }
        | unitdecl              { unitOL $1 }

unitdecl :: { LHsUnitDecl PackageName }
        : maybedocheader 'module' modid maybemodwarning maybeexports 'where' body
             -- XXX not accurate
             { sL1 $2 $ DeclD ModuleD $3 (Just (sL1 $2 (HsModule (Just $3) $5 (fst $ snd $7) (snd $ snd $7) $4 $1))) }
        | maybedocheader 'signature' modid maybemodwarning maybeexports 'where' body
             { sL1 $2 $ DeclD SignatureD $3 (Just (sL1 $2 (HsModule (Just $3) $5 (fst $ snd $7) (snd $ snd $7) $4 $1))) }
        -- NB: MUST have maybedocheader here, otherwise shift-reduce conflict
        -- will prevent us from parsing both forms.
        | maybedocheader 'module' modid
             { sL1 $2 $ DeclD ModuleD $3 Nothing }
        | maybedocheader 'signature' modid
             { sL1 $2 $ DeclD SignatureD $3 Nothing }
        | 'dependency' unitid mayberns
             { sL1 $1 $ IncludeD (IncludeDecl { idUnitId = $2
                                              , idModRenaming = $3
                                              , idSignatureInclude = False }) }
        | 'dependency' 'signature' unitid
             { sL1 $1 $ IncludeD (IncludeDecl { idUnitId = $3
                                              , idModRenaming = Nothing
                                              , idSignatureInclude = True }) }

-----------------------------------------------------------------------------
-- Module Header

-- The place for module deprecation is really too restrictive, but if it
-- was allowed at its natural place just before 'module', we get an ugly
-- s/r conflict with the second alternative. Another solution would be the
-- introduction of a new pragma DEPRECATED_MODULE, but this is not very nice,
-- either, and DEPRECATED is only expected to be used by people who really
-- know what they are doing. :-)

signature :: { Located (HsModule GhcPs) }
       : maybedocheader 'signature' modid maybemodwarning maybeexports 'where' body
             {% fileSrcSpan >>= \ loc ->
                ams (L loc (HsModule (Just $3) $5 (fst $ snd $7)
                              (snd $ snd $7) $4 $1)
                    )
                    ([mj AnnSignature $2, mj AnnWhere $6] ++ fst $7) }

module :: { Located (HsModule GhcPs) }
       : maybedocheader 'module' modid maybemodwarning maybeexports 'where' body
             {% fileSrcSpan >>= \ loc ->
                ams (L loc (HsModule (Just $3) $5 (fst $ snd $7)
                              (snd $ snd $7) $4 $1)
                    )
                    ([mj AnnModule $2, mj AnnWhere $6] ++ fst $7) }
        | body2
                {% fileSrcSpan >>= \ loc ->
                   ams (L loc (HsModule Nothing Nothing
                               (fst $ snd $1) (snd $ snd $1) Nothing Nothing))
                       (fst $1) }

maybedocheader :: { Maybe LHsDocString }
        : moduleheader            { $1 }
        | {- empty -}             { Nothing }

missing_module_keyword :: { () }
        : {- empty -}                           {% pushModuleContext }

implicit_top :: { () }
        : {- empty -}                           {% pushModuleContext }

maybemodwarning :: { Maybe (Located WarningTxt) }
    : '{-# DEPRECATED' strings '#-}'
                      {% ajs (Just (sLL $1 $> $ DeprecatedTxt (sL1 $1 (getDEPRECATED_PRAGs $1)) (snd $ unLoc $2)))
                             (mo $1:mc $3: (fst $ unLoc $2)) }
    | '{-# WARNING' strings '#-}'
                         {% ajs (Just (sLL $1 $> $ WarningTxt (sL1 $1 (getWARNING_PRAGs $1)) (snd $ unLoc $2)))
                                (mo $1:mc $3 : (fst $ unLoc $2)) }
    |  {- empty -}                  { Nothing }

body    :: { ([AddAnn]
             ,([LImportDecl GhcPs], [LHsDecl GhcPs])) }
        :  '{'            top '}'      { (moc $1:mcc $3:(fst $2)
                                         , snd $2) }
        |      vocurly    top close    { (fst $2, snd $2) }

body2   :: { ([AddAnn]
             ,([LImportDecl GhcPs], [LHsDecl GhcPs])) }
        :  '{' top '}'                          { (moc $1:mcc $3
                                                   :(fst $2), snd $2) }
        |  missing_module_keyword top close     { ([],snd $2) }


top     :: { ([AddAnn]
             ,([LImportDecl GhcPs], [LHsDecl GhcPs])) }
        : semis top1                            { ($1, $2) }

top1    :: { ([LImportDecl GhcPs], [LHsDecl GhcPs]) }
        : importdecls_semi topdecls_semi        { (reverse $1, cvTopDecls $2) }
        | importdecls_semi topdecls             { (reverse $1, cvTopDecls $2) }
        | importdecls                           { (reverse $1, []) }

-----------------------------------------------------------------------------
-- Module declaration & imports only

header  :: { Located (HsModule GhcPs) }
        : maybedocheader 'module' modid maybemodwarning maybeexports 'where' header_body
                {% fileSrcSpan >>= \ loc ->
                   ams (L loc (HsModule (Just $3) $5 $7 [] $4 $1
                          )) [mj AnnModule $2,mj AnnWhere $6] }
        | maybedocheader 'signature' modid maybemodwarning maybeexports 'where' header_body
                {% fileSrcSpan >>= \ loc ->
                   ams (L loc (HsModule (Just $3) $5 $7 [] $4 $1
                          )) [mj AnnModule $2,mj AnnWhere $6] }
        | header_body2
                {% fileSrcSpan >>= \ loc ->
                   return (L loc (HsModule Nothing Nothing $1 [] Nothing
                          Nothing)) }

header_body :: { [LImportDecl GhcPs] }
        :  '{'            header_top            { $2 }
        |      vocurly    header_top            { $2 }

header_body2 :: { [LImportDecl GhcPs] }
        :  '{' header_top                       { $2 }
        |  missing_module_keyword header_top    { $2 }

header_top :: { [LImportDecl GhcPs] }
        :  semis header_top_importdecls         { $2 }

header_top_importdecls :: { [LImportDecl GhcPs] }
        :  importdecls_semi                     { $1 }
        |  importdecls                          { $1 }

-----------------------------------------------------------------------------
-- The Export List

maybeexports :: { (Maybe (Located [LIE GhcPs])) }
        :  '(' exportlist ')'       {% ams (sLL $1 $> ()) [mop $1,mcp $3] >>
                                       return (Just (sLL $1 $> (fromOL $2))) }
        |  {- empty -}              { Nothing }

exportlist :: { OrdList (LIE GhcPs) }
        : expdoclist ',' expdoclist   {% addAnnotation (oll $1) AnnComma (gl $2)
                                         >> return ($1 `appOL` $3) }
        | exportlist1                 { $1 }

exportlist1 :: { OrdList (LIE GhcPs) }
        : expdoclist export expdoclist ',' exportlist1
                          {% (addAnnotation (oll ($1 `appOL` $2 `appOL` $3))
                                            AnnComma (gl $4) ) >>
                              return ($1 `appOL` $2 `appOL` $3 `appOL` $5) }
        | expdoclist export expdoclist             { $1 `appOL` $2 `appOL` $3 }
        | expdoclist                               { $1 }

expdoclist :: { OrdList (LIE GhcPs) }
        : exp_doc expdoclist                           { $1 `appOL` $2 }
        | {- empty -}                                  { nilOL }

exp_doc :: { OrdList (LIE GhcPs) }
        : docsection    { unitOL (sL1 $1 (case (unLoc $1) of (n, doc) -> IEGroup noExt n doc)) }
        | docnamed      { unitOL (sL1 $1 (IEDocNamed noExt ((fst . unLoc) $1))) }
        | docnext       { unitOL (sL1 $1 (IEDoc noExt (unLoc $1))) }


   -- No longer allow things like [] and (,,,) to be exported
   -- They are built in syntax, always available
export  :: { OrdList (LIE GhcPs) }
        : qcname_ext export_subspec  {% mkModuleImpExp $1 (snd $ unLoc $2)
                                          >>= \ie -> amsu (sLL $1 $> ie) (fst $ unLoc $2) }
        |  'module' modid            {% amsu (sLL $1 $> (IEModuleContents noExt $2))
                                             [mj AnnModule $1] }
        |  'pattern' qcon            {% amsu (sLL $1 $> (IEVar noExt (sLL $1 $> (IEPattern $2))))
                                             [mj AnnPattern $1] }

export_subspec :: { Located ([AddAnn],ImpExpSubSpec) }
        : {- empty -}             { sL0 ([],ImpExpAbs) }
        | '(' qcnames ')'         {% mkImpExpSubSpec (reverse (snd $2))
                                      >>= \(as,ie) -> return $ sLL $1 $>
                                            (as ++ [mop $1,mcp $3] ++ fst $2, ie) }


qcnames :: { ([AddAnn], [Located ImpExpQcSpec]) }
  : {- empty -}                   { ([],[]) }
  | qcnames1                      { $1 }

qcnames1 :: { ([AddAnn], [Located ImpExpQcSpec]) }     -- A reversed list
        :  qcnames1 ',' qcname_ext_w_wildcard  {% case (head (snd $1)) of
                                                    l@(L _ ImpExpQcWildcard) ->
                                                       return ([mj AnnComma $2, mj AnnDotdot l]
                                                               ,(snd (unLoc $3)  : snd $1))
                                                    l -> (ams (head (snd $1)) [mj AnnComma $2] >>
                                                          return (fst $1 ++ fst (unLoc $3),
                                                                  snd (unLoc $3) : snd $1)) }


        -- Annotations re-added in mkImpExpSubSpec
        |  qcname_ext_w_wildcard                   { (fst (unLoc $1),[snd (unLoc $1)]) }

-- Variable, data constructor or wildcard
-- or tagged type constructor
qcname_ext_w_wildcard :: { Located ([AddAnn], Located ImpExpQcSpec) }
        :  qcname_ext               { sL1 $1 ([],$1) }
        |  '..'                     { sL1 $1 ([mj AnnDotdot $1], sL1 $1 ImpExpQcWildcard)  }

qcname_ext :: { Located ImpExpQcSpec }
        :  qcname                   { sL1 $1 (ImpExpQcName $1) }
        |  'type' oqtycon           {% do { n <- mkTypeImpExp $2
                                          ; ams (sLL $1 $> (ImpExpQcType n))
                                                [mj AnnType $1] } }

qcname  :: { Located RdrName }  -- Variable or type constructor
        :  qvar                 { $1 } -- Things which look like functions
                                       -- Note: This includes record selectors but
                                       -- also (-.->), see #11432
        |  oqtycon_no_varcon    { $1 } -- see Note [Type constructors in export list]

-----------------------------------------------------------------------------
-- Import Declarations

-- importdecls and topdecls must contain at least one declaration;
-- top handles the fact that these may be optional.

-- One or more semicolons
semis1  :: { [AddAnn] }
semis1  : semis1 ';'  { mj AnnSemi $2 : $1 }
        | ';'         { [mj AnnSemi $1] }

-- Zero or more semicolons
semis   :: { [AddAnn] }
semis   : semis ';'   { mj AnnSemi $2 : $1 }
        | {- empty -} { [] }

-- No trailing semicolons, non-empty
importdecls :: { [LImportDecl GhcPs] }
importdecls
        : importdecls_semi importdecl
                                { $2 : $1 }

-- May have trailing semicolons, can be empty
importdecls_semi :: { [LImportDecl GhcPs] }
importdecls_semi
        : importdecls_semi importdecl semis1
                                {% ams $2 $3 >> return ($2 : $1) }
        | {- empty -}           { [] }

importdecl :: { LImportDecl GhcPs }
        : 'import' maybe_src maybe_safe optqualified maybe_pkg modid maybeas maybeimpspec
                {% ams (L (comb4 $1 $6 (snd $7) $8) $
                  ImportDecl { ideclExt = noExt
                             , ideclSourceSrc = snd $ fst $2
                             , ideclName = $6, ideclPkgQual = snd $5
                             , ideclSource = snd $2, ideclSafe = snd $3
                             , ideclQualified = snd $4, ideclImplicit = False
                             , ideclAs = unLoc (snd $7)
                             , ideclHiding = unLoc $8 })
                   ((mj AnnImport $1 : (fst $ fst $2) ++ fst $3 ++ fst $4
                                    ++ fst $5 ++ fst $7)) }

maybe_src :: { (([AddAnn],SourceText),IsBootInterface) }
        : '{-# SOURCE' '#-}'        { (([mo $1,mc $2],getSOURCE_PRAGs $1)
                                      ,True) }
        | {- empty -}               { (([],NoSourceText),False) }

maybe_safe :: { ([AddAnn],Bool) }
        : 'safe'                                { ([mj AnnSafe $1],True) }
        | {- empty -}                           { ([],False) }

maybe_pkg :: { ([AddAnn],Maybe StringLiteral) }
        : STRING  {% let pkgFS = getSTRING $1 in
                     if looksLikePackageName (unpackFS pkgFS)
                        then return ([mj AnnPackageName $1], Just (StringLiteral (getSTRINGs $1) pkgFS))
                        else parseErrorSDoc (getLoc $1) $ vcat [
                             text "parse error" <> colon <+> quotes (ppr pkgFS),
                             text "Version number or non-alphanumeric" <+>
                             text "character in package name"] }
        | {- empty -}                           { ([],Nothing) }

optqualified :: { ([AddAnn],Bool) }
        : 'qualified'                           { ([mj AnnQualified $1],True)  }
        | {- empty -}                           { ([],False) }

maybeas :: { ([AddAnn],Located (Maybe (Located ModuleName))) }
        : 'as' modid                           { ([mj AnnAs $1]
                                                 ,sLL $1 $> (Just $2)) }
        | {- empty -}                          { ([],noLoc Nothing) }

maybeimpspec :: { Located (Maybe (Bool, Located [LIE GhcPs])) }
        : impspec                  {% let (b, ie) = unLoc $1 in
                                       checkImportSpec ie
                                        >>= \checkedIe ->
                                          return (L (gl $1) (Just (b, checkedIe)))  }
        | {- empty -}              { noLoc Nothing }

impspec :: { Located (Bool, Located [LIE GhcPs]) }
        :  '(' exportlist ')'               {% ams (sLL $1 $> (False,
                                                      sLL $1 $> $ fromOL $2))
                                                   [mop $1,mcp $3] }
        |  'hiding' '(' exportlist ')'      {% ams (sLL $1 $> (True,
                                                      sLL $1 $> $ fromOL $3))
                                               [mj AnnHiding $1,mop $2,mcp $4] }

-----------------------------------------------------------------------------
-- Fixity Declarations

prec    :: { Located (SourceText,Int) }
        : {- empty -}           { noLoc (NoSourceText,9) }
        | INTEGER
                 {% checkPrecP (sL1 $1 (getINTEGERs $1,fromInteger (il_value (getINTEGER $1)))) }

infix   :: { Located FixityDirection }
        : 'infix'                               { sL1 $1 InfixN  }
        | 'infixl'                              { sL1 $1 InfixL  }
        | 'infixr'                              { sL1 $1 InfixR }

ops     :: { Located (OrdList (Located RdrName)) }
        : ops ',' op       {% addAnnotation (oll $ unLoc $1) AnnComma (gl $2) >>
                              return (sLL $1 $> ((unLoc $1) `appOL` unitOL $3))}
        | op               { sL1 $1 (unitOL $1) }

-----------------------------------------------------------------------------
-- Top-Level Declarations

-- No trailing semicolons, non-empty
topdecls :: { OrdList (LHsDecl GhcPs) }
        : topdecls_semi topdecl        { $1 `snocOL` $2 }

-- May have trailing semicolons, can be empty
topdecls_semi :: { OrdList (LHsDecl GhcPs) }
        : topdecls_semi topdecl semis1 {% ams $2 $3 >> return ($1 `snocOL` $2) }
        | {- empty -}                  { nilOL }

topdecl :: { LHsDecl GhcPs }
        : cl_decl                               { sL1 $1 (TyClD noExt (unLoc $1)) }
        | ty_decl                               { sL1 $1 (TyClD noExt (unLoc $1)) }
        | inst_decl                             { sL1 $1 (InstD noExt (unLoc $1)) }
        | stand_alone_deriving                  { sLL $1 $> (DerivD noExt (unLoc $1)) }
        | role_annot                            { sL1 $1 (RoleAnnotD noExt (unLoc $1)) }
        | 'default' '(' comma_types0 ')'    {% ams (sLL $1 $> (DefD noExt (DefaultDecl noExt $3)))
                                                         [mj AnnDefault $1
                                                         ,mop $2,mcp $4] }
        | 'foreign' fdecl          {% ams (sLL $1 $> (snd $ unLoc $2))
                                           (mj AnnForeign $1:(fst $ unLoc $2)) }
        | '{-# DEPRECATED' deprecations '#-}'   {% ams (sLL $1 $> $ WarningD noExt (Warnings noExt (getDEPRECATED_PRAGs $1) (fromOL $2)))
                                                       [mo $1,mc $3] }
        | '{-# WARNING' warnings '#-}'          {% ams (sLL $1 $> $ WarningD noExt (Warnings noExt (getWARNING_PRAGs $1) (fromOL $2)))
                                                       [mo $1,mc $3] }
        | '{-# RULES' rules '#-}'               {% ams (sLL $1 $> $ RuleD noExt (HsRules noExt (getRULES_PRAGs $1) (fromOL $2)))
                                                       [mo $1,mc $3] }
        | annotation { $1 }
        | decl_no_th                            { $1 }

        -- Template Haskell Extension
        -- The $(..) form is one possible form of infixexp
        -- but we treat an arbitrary expression just as if
        -- it had a $(..) wrapped around it
        | infixexp_top                          { sLL $1 $> $ mkSpliceDecl $1 }

-- Type classes
--
cl_decl :: { LTyClDecl GhcPs }
        : 'class' tycl_hdr fds where_cls
                {% amms (mkClassDecl (comb4 $1 $2 $3 $4) $2 $3 (snd $ unLoc $4))
                        (mj AnnClass $1:(fst $ unLoc $3)++(fst $ unLoc $4)) }

-- Type declarations (toplevel)
--
ty_decl :: { LTyClDecl GhcPs }
           -- ordinary type synonyms
        : 'type' type '=' ctypedoc
                -- Note ctype, not sigtype, on the right of '='
                -- We allow an explicit for-all but we don't insert one
                -- in   type Foo a = (b,b)
                -- Instead we just say b is out of scope
                --
                -- Note the use of type for the head; this allows
                -- infix type constructors to be declared
                {% amms (mkTySynonym (comb2 $1 $4) $2 $4)
                        [mj AnnType $1,mj AnnEqual $3] }

           -- type family declarations
        | 'type' 'family' type opt_tyfam_kind_sig opt_injective_info
                          where_type_family
                -- Note the use of type for the head; this allows
                -- infix type constructors to be declared
                {% amms (mkFamDecl (comb4 $1 $3 $4 $5) (snd $ unLoc $6) $3
                                   (snd $ unLoc $4) (snd $ unLoc $5))
                        (mj AnnType $1:mj AnnFamily $2:(fst $ unLoc $4)
                           ++ (fst $ unLoc $5) ++ (fst $ unLoc $6)) }

          -- ordinary data type or newtype declaration
        | data_or_newtype capi_ctype tycl_hdr constrs maybe_derivings
                {% amms (mkTyData (comb4 $1 $3 $4 $5) (snd $ unLoc $1) $2 $3
                           Nothing (reverse (snd $ unLoc $4))
                                   (fmap reverse $5))
                                   -- We need the location on tycl_hdr in case
                                   -- constrs and deriving are both empty
                        ((fst $ unLoc $1):(fst $ unLoc $4)) }

          -- ordinary GADT declaration
        | data_or_newtype capi_ctype tycl_hdr opt_kind_sig
                 gadt_constrlist
                 maybe_derivings
            {% amms (mkTyData (comb4 $1 $3 $5 $6) (snd $ unLoc $1) $2 $3
                            (snd $ unLoc $4) (snd $ unLoc $5)
                            (fmap reverse $6) )
                                   -- We need the location on tycl_hdr in case
                                   -- constrs and deriving are both empty
                    ((fst $ unLoc $1):(fst $ unLoc $4)++(fst $ unLoc $5)) }

          -- data/newtype family
        | 'data' 'family' type opt_datafam_kind_sig
                {% amms (mkFamDecl (comb3 $1 $2 $4) DataFamily $3
                                   (snd $ unLoc $4) Nothing)
                        (mj AnnData $1:mj AnnFamily $2:(fst $ unLoc $4)) }

inst_decl :: { LInstDecl GhcPs }
        : 'instance' overlap_pragma inst_type where_inst
       {% do { (binds, sigs, _, ats, adts, _) <- cvBindsAndSigs (snd $ unLoc $4)
             ; let cid = ClsInstDecl { cid_ext = noExt
                                     , cid_poly_ty = $3, cid_binds = binds
                                     , cid_sigs = mkClassOpSigs sigs
                                     , cid_tyfam_insts = ats
                                     , cid_overlap_mode = $2
                                     , cid_datafam_insts = adts }
             ; ams (L (comb3 $1 (hsSigType $3) $4) (ClsInstD { cid_d_ext = noExt, cid_inst = cid }))
                   (mj AnnInstance $1 : (fst $ unLoc $4)) } }

           -- type instance declarations
        | 'type' 'instance' ty_fam_inst_eqn
                {% ams $3 (fst $ unLoc $3)
                >> amms (mkTyFamInst (comb2 $1 $3) (snd $ unLoc $3))
                    (mj AnnType $1:mj AnnInstance $2:(fst $ unLoc $3)) }

          -- data/newtype instance declaration
        | data_or_newtype 'instance' capi_ctype tycl_hdr constrs
                          maybe_derivings
            {% amms (mkDataFamInst (comb4 $1 $4 $5 $6) (snd $ unLoc $1) $3 $4
                                      Nothing (reverse (snd  $ unLoc $5))
                                              (fmap reverse $6))
                    ((fst $ unLoc $1):mj AnnInstance $2:(fst $ unLoc $5)) }

          -- GADT instance declaration
        | data_or_newtype 'instance' capi_ctype tycl_hdr opt_kind_sig
                 gadt_constrlist
                 maybe_derivings
            {% amms (mkDataFamInst (comb4 $1 $4 $6 $7) (snd $ unLoc $1) $3 $4
                                   (snd $ unLoc $5) (snd $ unLoc $6)
                                   (fmap reverse $7))
                    ((fst $ unLoc $1):mj AnnInstance $2
                       :(fst $ unLoc $5)++(fst $ unLoc $6)) }

overlap_pragma :: { Maybe (Located OverlapMode) }
  : '{-# OVERLAPPABLE'    '#-}' {% ajs (Just (sLL $1 $> (Overlappable (getOVERLAPPABLE_PRAGs $1))))
                                       [mo $1,mc $2] }
  | '{-# OVERLAPPING'     '#-}' {% ajs (Just (sLL $1 $> (Overlapping (getOVERLAPPING_PRAGs $1))))
                                       [mo $1,mc $2] }
  | '{-# OVERLAPS'        '#-}' {% ajs (Just (sLL $1 $> (Overlaps (getOVERLAPS_PRAGs $1))))
                                       [mo $1,mc $2] }
  | '{-# INCOHERENT'      '#-}' {% ajs (Just (sLL $1 $> (Incoherent (getINCOHERENT_PRAGs $1))))
                                       [mo $1,mc $2] }
  | {- empty -}                 { Nothing }

deriv_strategy_no_via :: { LDerivStrategy GhcPs }
  : 'stock'                     {% ams (sL1 $1 StockStrategy)
                                       [mj AnnStock $1] }
  | 'anyclass'                  {% ams (sL1 $1 AnyclassStrategy)
                                       [mj AnnAnyclass $1] }
  | 'newtype'                   {% ams (sL1 $1 NewtypeStrategy)
                                       [mj AnnNewtype $1] }

deriv_strategy_via :: { LDerivStrategy GhcPs }
  : 'via' type              {% ams (sLL $1 $> (ViaStrategy (mkLHsSigType $2)))
                                            [mj AnnVia $1] }

deriv_standalone_strategy :: { Maybe (LDerivStrategy GhcPs) }
  : 'stock'                     {% ajs (Just (sL1 $1 StockStrategy))
                                       [mj AnnStock $1] }
  | 'anyclass'                  {% ajs (Just (sL1 $1 AnyclassStrategy))
                                       [mj AnnAnyclass $1] }
  | 'newtype'                   {% ajs (Just (sL1 $1 NewtypeStrategy))
                                       [mj AnnNewtype $1] }
  | deriv_strategy_via          { Just $1 }
  | {- empty -}                 { Nothing }

-- Injective type families

opt_injective_info :: { Located ([AddAnn], Maybe (LInjectivityAnn GhcPs)) }
        : {- empty -}               { noLoc ([], Nothing) }
        | '|' injectivity_cond      { sLL $1 $> ([mj AnnVbar $1]
                                                , Just ($2)) }

injectivity_cond :: { LInjectivityAnn GhcPs }
        : tyvarid '->' inj_varids
           {% ams (sLL $1 $> (InjectivityAnn $1 (reverse (unLoc $3))))
                  [mu AnnRarrow $2] }

inj_varids :: { Located [Located RdrName] }
        : inj_varids tyvarid  { sLL $1 $> ($2 : unLoc $1) }
        | tyvarid             { sLL $1 $> [$1]            }

-- Closed type families

where_type_family :: { Located ([AddAnn],FamilyInfo GhcPs) }
        : {- empty -}                      { noLoc ([],OpenTypeFamily) }
        | 'where' ty_fam_inst_eqn_list
               { sLL $1 $> (mj AnnWhere $1:(fst $ unLoc $2)
                    ,ClosedTypeFamily (fmap reverse $ snd $ unLoc $2)) }

ty_fam_inst_eqn_list :: { Located ([AddAnn],Maybe [LTyFamInstEqn GhcPs]) }
        :     '{' ty_fam_inst_eqns '}'     { sLL $1 $> ([moc $1,mcc $3]
                                                ,Just (unLoc $2)) }
        | vocurly ty_fam_inst_eqns close   { let L loc _ = $2 in
                                             L loc ([],Just (unLoc $2)) }
        |     '{' '..' '}'                 { sLL $1 $> ([moc $1,mj AnnDotdot $2
                                                 ,mcc $3],Nothing) }
        | vocurly '..' close               { let L loc _ = $2 in
                                             L loc ([mj AnnDotdot $2],Nothing) }

ty_fam_inst_eqns :: { Located [LTyFamInstEqn GhcPs] }
        : ty_fam_inst_eqns ';' ty_fam_inst_eqn
                                      {% let L loc (anns, eqn) = $3 in
                                         asl (unLoc $1) $2 (L loc eqn)
                                         >> ams $3 anns
                                         >> return (sLL $1 $> (L loc eqn : unLoc $1)) }
        | ty_fam_inst_eqns ';'        {% addAnnotation (gl $1) AnnSemi (gl $2)
                                         >> return (sLL $1 $>  (unLoc $1)) }
        | ty_fam_inst_eqn             {% let L loc (anns, eqn) = $1 in
                                         ams $1 anns
                                         >> return (sLL $1 $> [L loc eqn]) }
        | {- empty -}                 { noLoc [] }

ty_fam_inst_eqn :: { Located ([AddAnn],TyFamInstEqn GhcPs) }
        : type '=' ctype
                -- Note the use of type for the head; this allows
                -- infix type constructors and type patterns
              {% do { (eqn,ann) <- mkTyFamInstEqn $1 $3
                    ; return (sLL $1 $> (mj AnnEqual $2:ann, eqn))  } }

-- Associated type family declarations
--
-- * They have a different syntax than on the toplevel (no family special
--   identifier).
--
-- * They also need to be separate from instances; otherwise, data family
--   declarations without a kind signature cause parsing conflicts with empty
--   data declarations.
--
at_decl_cls :: { LHsDecl GhcPs }
        :  -- data family declarations, with optional 'family' keyword
          'data' opt_family type opt_datafam_kind_sig
                {% amms (liftM mkTyClD (mkFamDecl (comb3 $1 $3 $4) DataFamily $3
                                                  (snd $ unLoc $4) Nothing))
                        (mj AnnData $1:$2++(fst $ unLoc $4)) }

           -- type family declarations, with optional 'family' keyword
           -- (can't use opt_instance because you get shift/reduce errors
        | 'type' type opt_at_kind_inj_sig
               {% amms (liftM mkTyClD
                        (mkFamDecl (comb3 $1 $2 $3) OpenTypeFamily $2
                                   (fst . snd $ unLoc $3)
                                   (snd . snd $ unLoc $3)))
                       (mj AnnType $1:(fst $ unLoc $3)) }
        | 'type' 'family' type opt_at_kind_inj_sig
               {% amms (liftM mkTyClD
                        (mkFamDecl (comb3 $1 $3 $4) OpenTypeFamily $3
                                   (fst . snd $ unLoc $4)
                                   (snd . snd $ unLoc $4)))
                       (mj AnnType $1:mj AnnFamily $2:(fst $ unLoc $4)) }

           -- default type instances, with optional 'instance' keyword
        | 'type' ty_fam_inst_eqn
                {% ams $2 (fst $ unLoc $2) >>
                   amms (liftM mkInstD (mkTyFamInst (comb2 $1 $2) (snd $ unLoc $2)))
                        (mj AnnType $1:(fst $ unLoc $2)) }
        | 'type' 'instance' ty_fam_inst_eqn
                {% ams $3 (fst $ unLoc $3) >>
                   amms (liftM mkInstD (mkTyFamInst (comb2 $1 $3) (snd $ unLoc $3)))
                        (mj AnnType $1:mj AnnInstance $2:(fst $ unLoc $3)) }

opt_family   :: { [AddAnn] }
              : {- empty -}   { [] }
              | 'family'      { [mj AnnFamily $1] }

opt_instance :: { [AddAnn] }
              : {- empty -} { [] }
              | 'instance'  { [mj AnnInstance $1] }

-- Associated type instances
--
at_decl_inst :: { LInstDecl GhcPs }
           -- type instance declarations, with optional 'instance' keyword
        : 'type' opt_instance ty_fam_inst_eqn
                -- Note the use of type for the head; this allows
                -- infix type constructors and type patterns
                {% ams $3 (fst $ unLoc $3) >>
                   amms (mkTyFamInst (comb2 $1 $3) (snd $ unLoc $3))
                        (mj AnnType $1:$2++(fst $ unLoc $3)) }

        -- data/newtype instance declaration, with optional 'instance' keyword
        -- (can't use opt_instance because you get reduce/reduce errors)
        | data_or_newtype capi_ctype tycl_hdr constrs maybe_derivings
               {% amms (mkDataFamInst (comb4 $1 $3 $4 $5) (snd $ unLoc $1) $2 $3
                                    Nothing (reverse (snd $ unLoc $4))
                                            (fmap reverse $5))
                       ((fst $ unLoc $1):(fst $ unLoc $4)) }

        | data_or_newtype 'instance' capi_ctype tycl_hdr constrs maybe_derivings
               {% amms (mkDataFamInst (comb4 $1 $4 $5 $6) (snd $ unLoc $1) $3 $4
                                    Nothing (reverse (snd $ unLoc $5))
                                            (fmap reverse $6))
                       ((fst $ unLoc $1):mj AnnInstance $2:(fst $ unLoc $5)) }

        -- GADT instance declaration, with optional 'instance' keyword
        -- (can't use opt_instance because you get reduce/reduce errors)
        | data_or_newtype capi_ctype tycl_hdr opt_kind_sig
                 gadt_constrlist
                 maybe_derivings
                {% amms (mkDataFamInst (comb4 $1 $3 $5 $6) (snd $ unLoc $1) $2
                                $3 (snd $ unLoc $4) (snd $ unLoc $5)
                                (fmap reverse $6))
                        ((fst $ unLoc $1):(fst $ unLoc $4)++(fst $ unLoc $5)) }

        | data_or_newtype 'instance' capi_ctype tycl_hdr opt_kind_sig
                 gadt_constrlist
                 maybe_derivings
                {% amms (mkDataFamInst (comb4 $1 $4 $6 $7) (snd $ unLoc $1) $3
                                $4 (snd $ unLoc $5) (snd $ unLoc $6)
                                (fmap reverse $7))
                        ((fst $ unLoc $1):mj AnnInstance $2:(fst $ unLoc $5)++(fst $ unLoc $6)) }

data_or_newtype :: { Located (AddAnn, NewOrData) }
        : 'data'        { sL1 $1 (mj AnnData    $1,DataType) }
        | 'newtype'     { sL1 $1 (mj AnnNewtype $1,NewType) }

-- Family result/return kind signatures

opt_kind_sig :: { Located ([AddAnn], Maybe (LHsKind GhcPs)) }
        :               { noLoc     ([]               , Nothing) }
        | '::' kind     { sLL $1 $> ([mu AnnDcolon $1], Just $2) }

opt_datafam_kind_sig :: { Located ([AddAnn], LFamilyResultSig GhcPs) }
        :               { noLoc     ([]               , noLoc (NoSig noExt)         )}
        | '::' kind     { sLL $1 $> ([mu AnnDcolon $1], sLL $1 $> (KindSig noExt $2))}

opt_tyfam_kind_sig :: { Located ([AddAnn], LFamilyResultSig GhcPs) }
        :              { noLoc     ([]               , noLoc     (NoSig    noExt)   )}
        | '::' kind    { sLL $1 $> ([mu AnnDcolon $1], sLL $1 $> (KindSig  noExt $2))}
        | '='  tv_bndr { sLL $1 $> ([mj AnnEqual $1] , sLL $1 $> (TyVarSig noExt $2))}

opt_at_kind_inj_sig :: { Located ([AddAnn], ( LFamilyResultSig GhcPs
                                            , Maybe (LInjectivityAnn GhcPs)))}
        :            { noLoc ([], (noLoc (NoSig noExt), Nothing)) }
        | '::' kind  { sLL $1 $> ( [mu AnnDcolon $1]
                                 , (sLL $2 $> (KindSig noExt $2), Nothing)) }
        | '='  tv_bndr '|' injectivity_cond
                { sLL $1 $> ([mj AnnEqual $1, mj AnnVbar $3]
                            , (sLL $1 $2 (TyVarSig noExt $2), Just $4))}

-- tycl_hdr parses the header of a class or data type decl,
-- which takes the form
--      T a b
--      Eq a => T a
--      (Eq a, Ord b) => T a b
--      T Int [a]                       -- for associated types
-- Rather a lot of inlining here, else we get reduce/reduce errors
tycl_hdr :: { Located (Maybe (LHsContext GhcPs), LHsType GhcPs) }
        : context '=>' type         {% addAnnotation (gl $1) (toUnicodeAnn AnnDarrow $2) (gl $2)
                                       >> (return (sLL $1 $> (Just $1, $3)))
                                    }
        | type                      { sL1 $1 (Nothing, $1) }

capi_ctype :: { Maybe (Located CType) }
capi_ctype : '{-# CTYPE' STRING STRING '#-}'
                       {% ajs (Just (sLL $1 $> (CType (getCTYPEs $1) (Just (Header (getSTRINGs $2) (getSTRING $2)))
                                        (getSTRINGs $3,getSTRING $3))))
                              [mo $1,mj AnnHeader $2,mj AnnVal $3,mc $4] }

           | '{-# CTYPE'        STRING '#-}'
                       {% ajs (Just (sLL $1 $> (CType (getCTYPEs $1) Nothing  (getSTRINGs $2, getSTRING $2))))
                              [mo $1,mj AnnVal $2,mc $3] }

           |           { Nothing }

-----------------------------------------------------------------------------
-- Stand-alone deriving

-- Glasgow extension: stand-alone deriving declarations
stand_alone_deriving :: { LDerivDecl GhcPs }
  : 'deriving' deriv_standalone_strategy 'instance' overlap_pragma inst_type
                {% do { let { err = text "in the stand-alone deriving instance"
                                    <> colon <+> quotes (ppr $5) }
                      ; ams (sLL $1 (hsSigType $>)
                                 (DerivDecl noExt (mkHsWildCardBndrs $5) $2 $4))
                            [mj AnnDeriving $1, mj AnnInstance $3] } }

-----------------------------------------------------------------------------
-- Role annotations

role_annot :: { LRoleAnnotDecl GhcPs }
role_annot : 'type' 'role' oqtycon maybe_roles
          {% amms (mkRoleAnnotDecl (comb3 $1 $3 $4) $3 (reverse (unLoc $4)))
                  [mj AnnType $1,mj AnnRole $2] }

-- Reversed!
maybe_roles :: { Located [Located (Maybe FastString)] }
maybe_roles : {- empty -}    { noLoc [] }
            | roles          { $1 }

roles :: { Located [Located (Maybe FastString)] }
roles : role             { sLL $1 $> [$1] }
      | roles role       { sLL $1 $> $ $2 : unLoc $1 }

-- read it in as a varid for better error messages
role :: { Located (Maybe FastString) }
role : VARID             { sL1 $1 $ Just $ getVARID $1 }
     | '_'               { sL1 $1 Nothing }

-- Pattern synonyms

-- Glasgow extension: pattern synonyms
pattern_synonym_decl :: { LHsDecl GhcPs }
        : 'pattern' pattern_synonym_lhs '=' pat
         {%      let (name, args,as ) = $2 in
                 ams (sLL $1 $> . ValD noExt $ mkPatSynBind name args $4
                                                    ImplicitBidirectional)
               (as ++ [mj AnnPattern $1, mj AnnEqual $3])
         }

        | 'pattern' pattern_synonym_lhs '<-' pat
         {%    let (name, args, as) = $2 in
               ams (sLL $1 $> . ValD noExt $ mkPatSynBind name args $4 Unidirectional)
               (as ++ [mj AnnPattern $1,mu AnnLarrow $3]) }

        | 'pattern' pattern_synonym_lhs '<-' pat where_decls
            {% do { let (name, args, as) = $2
                  ; mg <- mkPatSynMatchGroup name (snd $ unLoc $5)
                  ; ams (sLL $1 $> . ValD noExt $
                           mkPatSynBind name args $4 (ExplicitBidirectional mg))
                       (as ++ ((mj AnnPattern $1:mu AnnLarrow $3:(fst $ unLoc $5))) )
                   }}

pattern_synonym_lhs :: { (Located RdrName, HsPatSynDetails (Located RdrName), [AddAnn]) }
        : con vars0 { ($1, PrefixCon $2, []) }
        | varid conop varid { ($2, InfixCon $1 $3, []) }
        | con '{' cvars1 '}' { ($1, RecCon $3, [moc $2, mcc $4] ) }

vars0 :: { [Located RdrName] }
        : {- empty -}                 { [] }
        | varid vars0                 { $1 : $2 }

cvars1 :: { [RecordPatSynField (Located RdrName)] }
       : var                          { [RecordPatSynField $1 $1] }
       | var ',' cvars1               {% addAnnotation (getLoc $1) AnnComma (getLoc $2) >>
                                         return ((RecordPatSynField $1 $1) : $3 )}

where_decls :: { Located ([AddAnn]
                         , Located (OrdList (LHsDecl GhcPs))) }
        : 'where' '{' decls '}'       { sLL $1 $> ((mj AnnWhere $1:moc $2
                                           :mcc $4:(fst $ unLoc $3)),sL1 $3 (snd $ unLoc $3)) }
        | 'where' vocurly decls close { L (comb2 $1 $3) ((mj AnnWhere $1:(fst $ unLoc $3))
                                          ,sL1 $3 (snd $ unLoc $3)) }

pattern_synonym_sig :: { LSig GhcPs }
        : 'pattern' con_list '::' sigtypedoc
                   {% ams (sLL $1 $> $ PatSynSig noExt (unLoc $2) (mkLHsSigType $4))
                          [mj AnnPattern $1, mu AnnDcolon $3] }

-----------------------------------------------------------------------------
-- Nested declarations

-- Declaration in class bodies
--
decl_cls  :: { LHsDecl GhcPs }
decl_cls  : at_decl_cls                 { $1 }
          | decl                        { $1 }

          -- A 'default' signature used with the generic-programming extension
          | 'default' infixexp '::' sigtypedoc
                    {% do { v <- checkValSigLhs $2
                          ; let err = text "in default signature" <> colon <+>
                                      quotes (ppr $2)
                          ; ams (sLL $1 $> $ SigD noExt $ ClassOpSig noExt True [v] $ mkLHsSigType $4)
                                [mj AnnDefault $1,mu AnnDcolon $3] } }

decls_cls :: { Located ([AddAnn],OrdList (LHsDecl GhcPs)) }  -- Reversed
          : decls_cls ';' decl_cls      {% if isNilOL (snd $ unLoc $1)
                                             then return (sLL $1 $> (mj AnnSemi $2:(fst $ unLoc $1)
                                                                    , unitOL $3))
                                             else ams (lastOL (snd $ unLoc $1)) [mj AnnSemi $2]
                                           >> return (sLL $1 $> (fst $ unLoc $1
                                                                ,(snd $ unLoc $1) `appOL` unitOL $3)) }
          | decls_cls ';'               {% if isNilOL (snd $ unLoc $1)
                                             then return (sLL $1 $> (mj AnnSemi $2:(fst $ unLoc $1)
                                                                                   ,snd $ unLoc $1))
                                             else ams (lastOL (snd $ unLoc $1)) [mj AnnSemi $2]
                                           >> return (sLL $1 $>  (unLoc $1)) }
          | decl_cls                    { sL1 $1 ([], unitOL $1) }
          | {- empty -}                 { noLoc ([],nilOL) }

decllist_cls
        :: { Located ([AddAnn]
                     , OrdList (LHsDecl GhcPs)) }      -- Reversed
        : '{'         decls_cls '}'     { sLL $1 $> (moc $1:mcc $3:(fst $ unLoc $2)
                                             ,snd $ unLoc $2) }
        |     vocurly decls_cls close   { $2 }

-- Class body
--
where_cls :: { Located ([AddAnn]
                       ,(OrdList (LHsDecl GhcPs))) }    -- Reversed
                                -- No implicit parameters
                                -- May have type declarations
        : 'where' decllist_cls          { sLL $1 $> (mj AnnWhere $1:(fst $ unLoc $2)
                                             ,snd $ unLoc $2) }
        | {- empty -}                   { noLoc ([],nilOL) }

-- Declarations in instance bodies
--
decl_inst  :: { Located (OrdList (LHsDecl GhcPs)) }
decl_inst  : at_decl_inst               { sLL $1 $> (unitOL (sL1 $1 (InstD noExt (unLoc $1)))) }
           | decl                       { sLL $1 $> (unitOL $1) }

decls_inst :: { Located ([AddAnn],OrdList (LHsDecl GhcPs)) }   -- Reversed
           : decls_inst ';' decl_inst   {% if isNilOL (snd $ unLoc $1)
                                             then return (sLL $1 $> (mj AnnSemi $2:(fst $ unLoc $1)
                                                                    , unLoc $3))
                                             else ams (lastOL $ snd $ unLoc $1) [mj AnnSemi $2]
                                           >> return
                                            (sLL $1 $> (fst $ unLoc $1
                                                       ,(snd $ unLoc $1) `appOL` unLoc $3)) }
           | decls_inst ';'             {% if isNilOL (snd $ unLoc $1)
                                             then return (sLL $1 $> (mj AnnSemi $2:(fst $ unLoc $1)
                                                                                   ,snd $ unLoc $1))
                                             else ams (lastOL $ snd $ unLoc $1) [mj AnnSemi $2]
                                           >> return (sLL $1 $> (unLoc $1)) }
           | decl_inst                  { sL1 $1 ([],unLoc $1) }
           | {- empty -}                { noLoc ([],nilOL) }

decllist_inst
        :: { Located ([AddAnn]
                     , OrdList (LHsDecl GhcPs)) }      -- Reversed
        : '{'         decls_inst '}'    { sLL $1 $> (moc $1:mcc $3:(fst $ unLoc $2),snd $ unLoc $2) }
        |     vocurly decls_inst close  { L (gl $2) (unLoc $2) }

-- Instance body
--
where_inst :: { Located ([AddAnn]
                        , OrdList (LHsDecl GhcPs)) }   -- Reversed
                                -- No implicit parameters
                                -- May have type declarations
        : 'where' decllist_inst         { sLL $1 $> (mj AnnWhere $1:(fst $ unLoc $2)
                                             ,(snd $ unLoc $2)) }
        | {- empty -}                   { noLoc ([],nilOL) }

-- Declarations in binding groups other than classes and instances
--
decls   :: { Located ([AddAnn],OrdList (LHsDecl GhcPs)) }
        : decls ';' decl    {% if isNilOL (snd $ unLoc $1)
                                 then return (sLL $1 $> (mj AnnSemi $2:(fst $ unLoc $1)
                                                        , unitOL $3))
                                 else do ams (lastOL $ snd $ unLoc $1) [mj AnnSemi $2]
                                           >> return (
                                          let { this = unitOL $3;
                                                rest = snd $ unLoc $1;
                                                these = rest `appOL` this }
                                          in rest `seq` this `seq` these `seq`
                                             (sLL $1 $> (fst $ unLoc $1,these))) }
        | decls ';'          {% if isNilOL (snd $ unLoc $1)
                                  then return (sLL $1 $> ((mj AnnSemi $2:(fst $ unLoc $1)
                                                          ,snd $ unLoc $1)))
                                  else ams (lastOL $ snd $ unLoc $1) [mj AnnSemi $2]
                                           >> return (sLL $1 $> (unLoc $1)) }
        | decl                          { sL1 $1 ([], unitOL $1) }
        | {- empty -}                   { noLoc ([],nilOL) }

decllist :: { Located ([AddAnn],Located (OrdList (LHsDecl GhcPs))) }
        : '{'            decls '}'     { sLL $1 $> (moc $1:mcc $3:(fst $ unLoc $2)
                                                   ,sL1 $2 $ snd $ unLoc $2) }
        |     vocurly    decls close   { L (gl $2) (fst $ unLoc $2,sL1 $2 $ snd $ unLoc $2) }

-- Binding groups other than those of class and instance declarations
--
binds   ::  { Located ([AddAnn],Located (HsLocalBinds GhcPs)) }
                                         -- May have implicit parameters
                                                -- No type declarations
        : decllist          {% do { val_binds <- cvBindGroup (unLoc $ snd $ unLoc $1)
                                  ; return (sL1 $1 (fst $ unLoc $1
                                                    ,sL1 $1 $ HsValBinds noExt val_binds)) } }

        | '{'            dbinds '}'     { sLL $1 $> ([moc $1,mcc $3]
                                             ,sL1 $2 $ HsIPBinds noExt (IPBinds noExt (reverse $ unLoc $2))) }

        |     vocurly    dbinds close   { L (getLoc $2) ([]
                                            ,sL1 $2 $ HsIPBinds noExt (IPBinds noExt (reverse $ unLoc $2))) }


wherebinds :: { Located ([AddAnn],Located (HsLocalBinds GhcPs)) }
                                                -- May have implicit parameters
                                                -- No type declarations
        : 'where' binds                 { sLL $1 $> (mj AnnWhere $1 : (fst $ unLoc $2)
                                             ,snd $ unLoc $2) }
        | {- empty -}                   { noLoc ([],noLoc emptyLocalBinds) }


-----------------------------------------------------------------------------
-- Transformation Rules

rules   :: { OrdList (LRuleDecl GhcPs) }
        :  rules ';' rule              {% addAnnotation (oll $1) AnnSemi (gl $2)
                                          >> return ($1 `snocOL` $3) }
        |  rules ';'                   {% addAnnotation (oll $1) AnnSemi (gl $2)
                                          >> return $1 }
        |  rule                        { unitOL $1 }
        |  {- empty -}                 { nilOL }

rule    :: { LRuleDecl GhcPs }
        : STRING rule_activation rule_forall infixexp '=' exp
         {%ams (sLL $1 $> $ (HsRule noExt (L (gl $1) (getSTRINGs $1,getSTRING $1))
                                  ((snd $2) `orElse` AlwaysActive)
                                  (snd $3) $4 $6))
               (mj AnnEqual $5 : (fst $2) ++ (fst $3)) }

-- Rules can be specified to be NeverActive, unlike inline/specialize pragmas
rule_activation :: { ([AddAnn],Maybe Activation) }
        : {- empty -}                           { ([],Nothing) }
        | rule_explicit_activation              { (fst $1,Just (snd $1)) }

rule_explicit_activation :: { ([AddAnn]
                              ,Activation) }  -- In brackets
        : '[' INTEGER ']'       { ([mos $1,mj AnnVal $2,mcs $3]
                                  ,ActiveAfter  (getINTEGERs $2) (fromInteger (il_value (getINTEGER $2)))) }
        | '[' '~' INTEGER ']'   { ([mos $1,mj AnnTilde $2,mj AnnVal $3,mcs $4]
                                  ,ActiveBefore (getINTEGERs $3) (fromInteger (il_value (getINTEGER $3)))) }
        | '[' '~' ']'           { ([mos $1,mj AnnTilde $2,mcs $3]
                                  ,NeverActive) }

rule_forall :: { ([AddAnn],[LRuleBndr GhcPs]) }
        : 'forall' rule_var_list '.'     { ([mu AnnForall $1,mj AnnDot $3],$2) }
        | {- empty -}                    { ([],[]) }

rule_var_list :: { [LRuleBndr GhcPs] }
        : rule_var                              { [$1] }
        | rule_var rule_var_list                { $1 : $2 }

rule_var :: { LRuleBndr GhcPs }
        : varid                         { sLL $1 $> (RuleBndr noExt $1) }
        | '(' varid '::' ctype ')'      {% ams (sLL $1 $> (RuleBndrSig noExt $2
                                                       (mkLHsSigWcType $4)))
                                               [mop $1,mu AnnDcolon $3,mcp $5] }

-----------------------------------------------------------------------------
-- Warnings and deprecations (c.f. rules)

warnings :: { OrdList (LWarnDecl GhcPs) }
        : warnings ';' warning         {% addAnnotation (oll $1) AnnSemi (gl $2)
                                          >> return ($1 `appOL` $3) }
        | warnings ';'                 {% addAnnotation (oll $1) AnnSemi (gl $2)
                                          >> return $1 }
        | warning                      { $1 }
        | {- empty -}                  { nilOL }

-- SUP: TEMPORARY HACK, not checking for `module Foo'
warning :: { OrdList (LWarnDecl GhcPs) }
        : namelist strings
                {% amsu (sLL $1 $> (Warning noExt (unLoc $1) (WarningTxt (noLoc NoSourceText) $ snd $ unLoc $2)))
                     (fst $ unLoc $2) }

deprecations :: { OrdList (LWarnDecl GhcPs) }
        : deprecations ';' deprecation
                                       {% addAnnotation (oll $1) AnnSemi (gl $2)
                                          >> return ($1 `appOL` $3) }
        | deprecations ';'             {% addAnnotation (oll $1) AnnSemi (gl $2)
                                          >> return $1 }
        | deprecation                  { $1 }
        | {- empty -}                  { nilOL }

-- SUP: TEMPORARY HACK, not checking for `module Foo'
deprecation :: { OrdList (LWarnDecl GhcPs) }
        : namelist strings
             {% amsu (sLL $1 $> $ (Warning noExt (unLoc $1) (DeprecatedTxt (noLoc NoSourceText) $ snd $ unLoc $2)))
                     (fst $ unLoc $2) }

strings :: { Located ([AddAnn],[Located StringLiteral]) }
    : STRING { sL1 $1 ([],[L (gl $1) (getStringLiteral $1)]) }
    | '[' stringlist ']' { sLL $1 $> $ ([mos $1,mcs $3],fromOL (unLoc $2)) }

stringlist :: { Located (OrdList (Located StringLiteral)) }
    : stringlist ',' STRING {% addAnnotation (oll $ unLoc $1) AnnComma (gl $2) >>
                               return (sLL $1 $> (unLoc $1 `snocOL`
                                                  (L (gl $3) (getStringLiteral $3)))) }
    | STRING                { sLL $1 $> (unitOL (L (gl $1) (getStringLiteral $1))) }
    | {- empty -}           { noLoc nilOL }

-----------------------------------------------------------------------------
-- Annotations
annotation :: { LHsDecl GhcPs }
    : '{-# ANN' name_var aexp '#-}'      {% ams (sLL $1 $> (AnnD noExt $ HsAnnotation noExt
                                            (getANN_PRAGs $1)
                                            (ValueAnnProvenance $2) $3))
                                            [mo $1,mc $4] }

    | '{-# ANN' 'type' tycon aexp '#-}'  {% ams (sLL $1 $> (AnnD noExt $ HsAnnotation noExt
                                            (getANN_PRAGs $1)
                                            (TypeAnnProvenance $3) $4))
                                            [mo $1,mj AnnType $2,mc $5] }

    | '{-# ANN' 'module' aexp '#-}'      {% ams (sLL $1 $> (AnnD noExt $ HsAnnotation noExt
                                                (getANN_PRAGs $1)
                                                 ModuleAnnProvenance $3))
                                                [mo $1,mj AnnModule $2,mc $4] }


-----------------------------------------------------------------------------
-- Foreign import and export declarations

fdecl :: { Located ([AddAnn],HsDecl GhcPs) }
fdecl : 'import' callconv safety fspec
               {% mkImport $2 $3 (snd $ unLoc $4) >>= \i ->
                 return (sLL $1 $> (mj AnnImport $1 : (fst $ unLoc $4),i))  }
      | 'import' callconv        fspec
               {% do { d <- mkImport $2 (noLoc PlaySafe) (snd $ unLoc $3);
                    return (sLL $1 $> (mj AnnImport $1 : (fst $ unLoc $3),d)) }}
      | 'export' callconv fspec
               {% mkExport $2 (snd $ unLoc $3) >>= \i ->
                  return (sLL $1 $> (mj AnnExport $1 : (fst $ unLoc $3),i) ) }

callconv :: { Located CCallConv }
          : 'stdcall'                   { sLL $1 $> StdCallConv }
          | 'ccall'                     { sLL $1 $> CCallConv   }
          | 'capi'                      { sLL $1 $> CApiConv    }
          | 'prim'                      { sLL $1 $> PrimCallConv}
          | 'javascript'                { sLL $1 $> JavaScriptCallConv }

safety :: { Located Safety }
        : 'unsafe'                      { sLL $1 $> PlayRisky }
        | 'safe'                        { sLL $1 $> PlaySafe }
        | 'interruptible'               { sLL $1 $> PlayInterruptible }

fspec :: { Located ([AddAnn]
                    ,(Located StringLiteral, Located RdrName, LHsSigType GhcPs)) }
       : STRING var '::' sigtypedoc     { sLL $1 $> ([mu AnnDcolon $3]
                                             ,(L (getLoc $1)
                                                    (getStringLiteral $1), $2, mkLHsSigType $4)) }
       |        var '::' sigtypedoc     { sLL $1 $> ([mu AnnDcolon $2]
                                             ,(noLoc (StringLiteral NoSourceText nilFS), $1, mkLHsSigType $3)) }
         -- if the entity string is missing, it defaults to the empty string;
         -- the meaning of an empty entity string depends on the calling
         -- convention

-----------------------------------------------------------------------------
-- Type signatures

opt_sig :: { ([AddAnn], Maybe (LHsType GhcPs)) }
        : {- empty -}                   { ([], Nothing) }
        | '::' sigtype                  { ([mu AnnDcolon $1], Just $2) }

opt_tyconsig :: { ([AddAnn], Maybe (Located RdrName)) }
             : {- empty -}              { ([], Nothing) }
             | '::' gtycon              { ([mu AnnDcolon $1], Just $2) }

sigtype :: { LHsType GhcPs }
        : ctype                            { $1 }

sigtypedoc :: { LHsType GhcPs }
        : ctypedoc                         { $1 }


sig_vars :: { Located [Located RdrName] }    -- Returned in reversed order
         : sig_vars ',' var           {% addAnnotation (gl $ head $ unLoc $1)
                                                       AnnComma (gl $2)
                                         >> return (sLL $1 $> ($3 : unLoc $1)) }
         | var                        { sL1 $1 [$1] }

sigtypes1 :: { (OrdList (LHsSigType GhcPs)) }
   : sigtype                 { unitOL (mkLHsSigType $1) }
   | sigtype ',' sigtypes1   {% addAnnotation (gl $1) AnnComma (gl $2)
                                >> return (unitOL (mkLHsSigType $1) `appOL` $3) }

-----------------------------------------------------------------------------
-- Types

strict_mark :: { Located ([AddAnn],HsSrcBang) }
        : strictness { sL1 $1 (let (a, str) = unLoc $1 in (a, HsSrcBang NoSourceText NoSrcUnpack str)) }
        | unpackedness { sL1 $1 (let (a, prag, unpk) = unLoc $1 in (a, HsSrcBang prag unpk NoSrcStrict)) }
        | unpackedness strictness { sLL $1 $> (let { (a, prag, unpk) = unLoc $1
                                                   ; (a', str) = unLoc $2 }
                                                in (a ++ a', HsSrcBang prag unpk str)) }
        -- Although UNPACK with no '!' without StrictData and UNPACK with '~' are illegal,
        -- we get a better error message if we parse them here

strictness :: { Located ([AddAnn], SrcStrictness) }
        : '!' { sL1 $1 ([mj AnnBang $1], SrcStrict) }
        | '~' { sL1 $1 ([mj AnnTilde $1], SrcLazy) }

unpackedness :: { Located ([AddAnn], SourceText, SrcUnpackedness) }
        : '{-# UNPACK' '#-}'   { sLL $1 $> ([mo $1, mc $2], getUNPACK_PRAGs $1, SrcUnpack) }
        | '{-# NOUNPACK' '#-}' { sLL $1 $> ([mo $1, mc $2], getNOUNPACK_PRAGs $1, SrcNoUnpack) }

-- A ctype is a for-all type
ctype   :: { LHsType GhcPs }
        : 'forall' tv_bndrs '.' ctype   {% hintExplicitForall (getLoc $1) >>
                                           ams (sLL $1 $> $
                                                HsForAllTy { hst_bndrs = $2
                                                           , hst_xforall = noExt
                                                           , hst_body = $4 })
                                               [mu AnnForall $1, mj AnnDot $3] }
        | context '=>' ctype          {% addAnnotation (gl $1) (toUnicodeAnn AnnDarrow $2) (gl $2)
                                         >> return (sLL $1 $> $
                                            HsQualTy { hst_ctxt = $1
                                                     , hst_xqual = noExt
                                                     , hst_body = $3 }) }
        | ipvar '::' type             {% ams (sLL $1 $> (HsIParamTy noExt $1 $3))
                                             [mu AnnDcolon $2] }
        | type                        { $1 }

----------------------
-- Notes for 'ctypedoc'
-- It would have been nice to simplify the grammar by unifying `ctype` and
-- ctypedoc` into one production, allowing comments on types everywhere (and
-- rejecting them after parsing, where necessary).  This is however not possible
-- since it leads to ambiguity. The reason is the support for comments on record
-- fields:
--         data R = R { field :: Int -- ^ comment on the field }
-- If we allow comments on types here, it's not clear if the comment applies
-- to 'field' or to 'Int'. So we must use `ctype` to describe the type.

ctypedoc :: { LHsType GhcPs }
        : 'forall' tv_bndrs '.' ctypedoc {% hintExplicitForall (getLoc $1) >>
                                            ams (sLL $1 $> $
                                                 HsForAllTy { hst_bndrs = $2
                                                            , hst_xforall = noExt
                                                            , hst_body = $4 })
                                                [mu AnnForall $1,mj AnnDot $3] }
        | context '=>' ctypedoc       {% addAnnotation (gl $1) (toUnicodeAnn AnnDarrow $2) (gl $2)
                                         >> return (sLL $1 $> $
                                            HsQualTy { hst_ctxt = $1
                                                     , hst_xqual = noExt
                                                     , hst_body = $3 }) }
        | ipvar '::' type             {% ams (sLL $1 $> (HsIParamTy noExt $1 $3))
                                             [mu AnnDcolon $2] }
        | typedoc                     { $1 }

----------------------
-- Notes for 'context'
-- We parse a context as a btype so that we don't get reduce/reduce
-- errors in ctype.  The basic problem is that
--      (Eq a, Ord a)
-- looks so much like a tuple type.  We can't tell until we find the =>

-- We have the t1 ~ t2 form both in 'context' and in type,
-- to permit an individual equational constraint without parenthesis.
-- Thus for some reason we allow    f :: a~b => blah
-- but not                          f :: ?x::Int => blah
-- See Note [Parsing ~]
context :: { LHsContext GhcPs }
        :  btype                        {% do { (anns,ctx) <- checkContext $1
                                                ; if null (unLoc ctx)
                                                   then addAnnotation (gl $1) AnnUnit (gl $1)
                                                   else return ()
                                                ; ams ctx anns
                                                } }

context_no_ops :: { LHsContext GhcPs }
        : btype_no_ops                 {% do { ty <- splitTilde (reverse (unLoc $1))
                                             ; (anns,ctx) <- checkContext ty
                                             ; if null (unLoc ctx)
                                                   then addAnnotation (gl ty) AnnUnit (gl ty)
                                                   else return ()
                                             ; ams ctx anns
                                             } }

{- Note [GADT decl discards annotations]
~~~~~~~~~~~~~~~~~~~~~
The type production for

    btype `->`         ctypedoc
    btype docprev `->` ctypedoc

add the AnnRarrow annotation twice, in different places.

This is because if the type is processed as usual, it belongs on the annotations
for the type as a whole.

But if the type is passed to mkGadtDecl, it discards the top level SrcSpan, and
the top-level annotation will be disconnected. Hence for this specific case it
is connected to the first type too.
-}

type :: { LHsType GhcPs }
        : btype                        { $1 }
        | btype '->' ctype             {% ams (sLL $1 $> $ HsFunTy noExt $1 $3)
                                              [mu AnnRarrow $2] }


typedoc :: { LHsType GhcPs }
        : btype                          { $1 }
        | btype docprev                  { sLL $1 $> $ HsDocTy noExt $1 $2 }
        | docnext btype                  { sLL $1 $> $ HsDocTy noExt $2 $1 }
        | btype '->'     ctypedoc        {% ams $1 [mu AnnRarrow $2] -- See note [GADT decl discards annotations]
                                         >> ams (sLL $1 $> $ HsFunTy noExt $1 $3)
                                                [mu AnnRarrow $2] }
        | btype docprev '->' ctypedoc    {% ams $1 [mu AnnRarrow $3] -- See note [GADT decl discards annotations]
                                         >> ams (sLL $1 $> $
                                                 HsFunTy noExt (L (comb2 $1 $2)
                                                            (HsDocTy noExt $1 $2))
                                                         $4)
                                                [mu AnnRarrow $3] }
        | docnext btype '->' ctypedoc    {% ams $2 [mu AnnRarrow $3] -- See note [GADT decl discards annotations]
                                         >> ams (sLL $1 $> $
                                                 HsFunTy noExt (L (comb2 $1 $2)
                                                            (HsDocTy noExt $2 $1))
                                                         $4)
                                                [mu AnnRarrow $3] }



-- See Note [Parsing ~]
btype :: { LHsType GhcPs }
      : tyapps                      {%  mergeOps (unLoc $1) }

-- Used for parsing Haskell98-style data constructors,
-- in order to forbid the blasphemous
-- > data Foo = Int :+ Char :* Bool
-- See also Note [Parsing data constructors is hard] in RdrHsSyn
btype_no_ops :: { Located [LHsType GhcPs] } -- NB: This list is reversed
        : atype_docs                    { sL1 $1 [$1] }
        | btype_no_ops atype_docs       { sLL $1 $> $ $2 : (unLoc $1) }

tyapps :: { Located [Located TyEl] } -- NB: This list is reversed
      --  : tyapp
      --EF                       { sL1 $1 [$1] }
        : atype                         { sL1 (sL1 $1 $ TyElOpd (unLoc $1))
                                              [sL1 $1 $ TyElOpd (unLoc $1)] }
      --  | '*'
      --EF                         { undefined }
        | tyapps tyapp                  { sLL $1 $> $ $2 : (unLoc $1) }

tyapp :: { Located TyEl }
        : atype                         { sL1 $1 $ TyElOpd (unLoc $1) }
        | qtyconop                      { sL1 $1 $ TyElOpr (unLoc $1) }
        | tyvarop                       { sL1 $1 $ TyElOpr (unLoc $1) }
        | SIMPLEQUOTE qconop            {% ams (sLL $1 $> $ TyElOpr (unLoc $2))
                                               [mj AnnSimpleQuote $1] }
        | SIMPLEQUOTE varop             {% ams (sLL $1 $> $ TyElOpr (unLoc $2))
                                               [mj AnnSimpleQuote $1] }

atype_docs :: { LHsType GhcPs }
        : atype docprev                 { sLL $1 $> $ HsDocTy noExt $1 $2 }
        | atype                         { $1 }

atype :: { LHsType GhcPs }
        -- : parse_type_in_exp                   { lhsExpr_to_lhsType $1 }
        : ntgtycon                       { sL1 $1 (HsTyVar noExt NotPromoted $1) }      -- Not including unit tuples
        | tyvar                          { sL1 $1 (HsTyVar noExt NotPromoted $1) }      -- (See Note [Unit tuples])
        | '*'                            {% do { warnStarIsType (getLoc $1)
                                               ; return $ sL1 $1 (HsStarTy noExt (isUnicode $1)) } }
        | strict_mark atype              {% ams (sLL $1 $> (HsBangTy noExt (snd $ unLoc $1) $2))
                                                (fst $ unLoc $1) }  -- Constructor sigs only
        | '{' fielddecls '}'             {% amms (checkRecordSyntax
                                                    (sLL $1 $> $ HsRecTy noExt $2))
                                                        -- Constructor sigs only
                                                 [moc $1,mcc $3] }
      {---EF  | '(' ')'                        {% ams (sLL $1 $> $ HsTupleTy noExt
      --                                              HsBoxedOrConstraintTuple [])
                                                  [mop $1,mcp $2] } --EF-}
        | '(' ctype ',' comma_types1 ')' {% addAnnotation (gl $2) AnnComma
                                                          (gl $3) >>
                                            ams (sLL $1 $> $ HsTupleTy noExt

                                             HsBoxedOrConstraintTuple ($2 : $4))
                                                [mop $1,mcp $5] }
      {---EF  | '(#' '#)'                   {% ams (sLL $1 $> $ HsTupleTy noExt HsUnboxedTuple [])
                                               [mo $1,mc $2] }  --EF-}
        | '(#' comma_types1 '#)'      {% ams (sLL $1 $> $ HsTupleTy noExt HsUnboxedTuple $2)
                                             [mo $1,mc $3] }
        | '(#' bar_types2 '#)'        {% ams (sLL $1 $> $ HsSumTy noExt $2)
                                             [mo $1,mc $3] }
        | '[' ctype ']'               {% ams (sLL $1 $> $ HsListTy  noExt $2) [mos $1,mcs $3] }
        | '(' ctype ')'               {% ams (sLL $1 $> $ HsParTy   noExt $2) [mop $1,mcp $3] }
        | '(' ctype '::' kind ')'     {% ams (sLL $1 $> $ HsKindSig noExt $2 $4)
                                             [mop $1,mu AnnDcolon $3,mcp $5] }
        | quasiquote                  { sL1 $1 (HsSpliceTy noExt (unLoc $1) ) }
        | '$(' exp ')'                {% ams (sLL $1 $> $ mkHsSpliceTy HasParens $2)
                                             [mj AnnOpenPE $1,mj AnnCloseP $3] }
        | TH_ID_SPLICE                {%ams (sLL $1 $> $ mkHsSpliceTy HasDollar $ sL1 $1 $ HsVar noExt $
                                             (sL1 $1 (mkUnqual varName (getTH_ID_SPLICE $1))))
                                             [mj AnnThIdSplice $1] }
                                      -- see Note [Promotion] for the followings
        | SIMPLEQUOTE qcon_nowiredlist {% ams (sLL $1 $> $ HsTyVar noExt Promoted $2) [mj AnnSimpleQuote $1,mj AnnName $2] }
        | SIMPLEQUOTE  '(' ctype ',' comma_types1 ')'
                             {% addAnnotation (gl $3) AnnComma (gl $4) >>
                                ams (sLL $1 $> $ HsExplicitTupleTy noExt ($3 : $5))
                                    [mj AnnSimpleQuote $1,mop $2,mcp $6] }
        | SIMPLEQUOTE  '[' comma_types0 ']'     {% ams (sLL $1 $> $ HsExplicitListTy noExt Promoted $3)
                                                       [mj AnnSimpleQuote $1,mos $2,mcs $4] }
        | SIMPLEQUOTE var                       {% ams (sLL $1 $> $ HsTyVar noExt Promoted $2)
                                                       [mj AnnSimpleQuote $1,mj AnnName $2] }

        -- Two or more [ty, ty, ty] must be a promoted list type, just as
        -- if you had written '[ty, ty, ty]
        -- (One means a list type, zero means the list type constructor,
        -- so you have to quote those.)
        | '[' ctype ',' comma_types1 ']'  {% addAnnotation (gl $2) AnnComma
                                                           (gl $3) >>
                                             ams (sLL $1 $> $ HsExplicitListTy noExt NotPromoted ($2 : $4))
                                                 [mos $1,mcs $5] }
        | INTEGER              { sLL $1 $> $ HsTyLit noExt $ HsNumTy (getINTEGERs $1)
                                                           (il_value (getINTEGER $1)) }
        | STRING               { sLL $1 $> $ HsTyLit noExt $ HsStrTy (getSTRINGs $1)
                                                                     (getSTRING  $1) }
        | '_'                  { sL1 $1 $ mkAnonWildCardTy }

-- An inst_type is what occurs in the head of an instance decl
--      e.g.  (Foo a, Gaz b) => Wibble a b
-- It's kept as a single type for convenience.
inst_type :: { LHsSigType GhcPs }
        : sigtype                       { mkLHsSigType $1 }

deriv_types :: { [LHsSigType GhcPs] }
        : typedoc                       { [mkLHsSigType $1] }

        | typedoc ',' deriv_types       {% addAnnotation (gl $1) AnnComma (gl $2)
                                           >> return (mkLHsSigType $1 : $3) }

comma_types0  :: { [LHsType GhcPs] }  -- Zero or more:  ty,ty,ty
        : comma_types1                  { $1 }
        | {- empty -}                   { [] }

comma_types1    :: { [LHsType GhcPs] }  -- One or more:  ty,ty,ty
        : ctype                        { [$1] }
        | ctype  ',' comma_types1      {% addAnnotation (gl $1) AnnComma (gl $2)
                                          >> return ($1 : $3) }

bar_types2    :: { [LHsType GhcPs] }  -- Two or more:  ty|ty|ty
        : ctype  '|' ctype             {% addAnnotation (gl $1) AnnVbar (gl $2)
                                          >> return [$1,$3] }
        | ctype  '|' bar_types2        {% addAnnotation (gl $1) AnnVbar (gl $2)
                                          >> return ($1 : $3) }

tv_bndrs :: { [LHsTyVarBndr GhcPs] }
         : tv_bndr tv_bndrs             { $1 : $2 }
         | {- empty -}                  { [] }

tv_bndr :: { LHsTyVarBndr GhcPs }
        : tyvarid                         { sL1 $1 (UserTyVar noExt $1) }
        | '(' tyvarid '::' kind ')'       {% ams (sLL $1 $>  (KindedTyVar noExt $2 $4))
                                               [mop $1,mu AnnDcolon $3
                                               ,mcp $5] }
{- --EF
        : tyvar                         { sL1 $1 (UserTyVar noExt $1) }
        | '(' tyvar '::' kind ')'       {% ams (sLL $1 $>  (KindedTyVar noExt $2 $4))
                                               [mop $1,mu AnnDcolon $3
                                               ,mcp $5] }
--EF -}


fds :: { Located ([AddAnn],[Located (FunDep (Located RdrName))]) }
        : {- empty -}                   { noLoc ([],[]) }
        | '|' fds1                      { (sLL $1 $> ([mj AnnVbar $1]
                                                 ,reverse (unLoc $2))) }

fds1 :: { Located [Located (FunDep (Located RdrName))] }
        : fds1 ',' fd   {% addAnnotation (gl $ head $ unLoc $1) AnnComma (gl $2)
                           >> return (sLL $1 $> ($3 : unLoc $1)) }
        | fd            { sL1 $1 [$1] }

fd :: { Located (FunDep (Located RdrName)) }
        : varids0 '->' varids0  {% ams (L (comb3 $1 $2 $3)
                                       (reverse (unLoc $1), reverse (unLoc $3)))
                                       [mu AnnRarrow $2] }

varids0 :: { Located [Located RdrName] }
        : {- empty -}                   { noLoc [] }
        | varids0 tyvar                 { sLL $1 $> ($2 : unLoc $1) }

{-
Note [Parsing ~]
~~~~~~~~~~~~~~~~

Due to parsing conflicts between laziness annotations in data type
declarations (see strict_mark) and equality types ~'s are always
parsed as laziness annotations, and turned into HsOpTy's in the
correct places using RdrHsSyn.splitTilde.

Since strict_mark is parsed as part of atype which is part of type,
typedoc and context (where HsEqTy previously appeared) it made most
sense and was simplest to parse ~ as part of strict_mark and later
turn them into HsOpTy's.

-}


-----------------------------------------------------------------------------
-- Kinds

kind :: { LHsKind GhcPs }
        : ctype                  { $1 }

{- Note [Promotion]
   ~~~~~~~~~~~~~~~~

- Syntax of promoted qualified names
We write 'Nat.Zero instead of Nat.'Zero when dealing with qualified
names. Moreover ticks are only allowed in types, not in kinds, for a
few reasons:
  1. we don't need quotes since we cannot define names in kinds
  2. if one day we merge types and kinds, tick would mean look in DataName
  3. we don't have a kind namespace anyway

- Name resolution
When the user write Zero instead of 'Zero in types, we parse it a
HsTyVar ("Zero", TcClsName) instead of HsTyVar ("Zero", DataName). We
deal with this in the renamer. If a HsTyVar ("Zero", TcClsName) is not
bounded in the type level, then we look for it in the term level (we
change its namespace to DataName, see Note [Demotion] in OccName). And
both become a HsTyVar ("Zero", DataName) after the renamer.

-}


-----------------------------------------------------------------------------
-- Datatype declarations

gadt_constrlist :: { Located ([AddAnn]
                          ,[LConDecl GhcPs]) } -- Returned in order

        : 'where' '{'        gadt_constrs '}'    {% checkEmptyGADTs $
                                                      L (comb2 $1 $3)
                                                        ([mj AnnWhere $1
                                                         ,moc $2
                                                         ,mcc $4]
                                                        , unLoc $3) }
        | 'where' vocurly    gadt_constrs close  {% checkEmptyGADTs $
                                                      L (comb2 $1 $3)
                                                        ([mj AnnWhere $1]
                                                        , unLoc $3) }
        | {- empty -}                            { noLoc ([],[]) }

gadt_constrs :: { Located [LConDecl GhcPs] }
        : gadt_constr_with_doc ';' gadt_constrs
                  {% addAnnotation (gl $1) AnnSemi (gl $2)
                     >> return (L (comb2 $1 $3) ($1 : unLoc $3)) }
        | gadt_constr_with_doc          { L (gl $1) [$1] }
        | {- empty -}                   { noLoc [] }

-- We allow the following forms:
--      C :: Eq a => a -> T a
--      C :: forall a. Eq a => !a -> T a
--      D { x,y :: a } :: T a
--      forall a. Eq a => D { x,y :: a } :: T a

gadt_constr_with_doc :: { LConDecl GhcPs }
gadt_constr_with_doc
        : maybe_docnext ';' gadt_constr
                {% return $ addConDoc $3 $1 }
        | gadt_constr
                {% return $1 }

gadt_constr :: { LConDecl GhcPs }
    -- see Note [Difference in parsing GADT and data constructors]
    -- Returns a list because of:   C,D :: ty
        : con_list '::' sigtypedoc
                {% ams (sLL $1 $> (mkGadtDecl (unLoc $1) $3))
                       [mu AnnDcolon $2] }

{- Note [Difference in parsing GADT and data constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GADT constructors have simpler syntax than usual data constructors:
in GADTs, types cannot occur to the left of '::', so they cannot be mixed
with constructor names (see Note [Parsing data constructors is hard]).

Due to simplified syntax, GADT constructor names (left-hand side of '::')
use simpler grammar production than usual data constructor names. As a
consequence, GADT constructor names are restricted (names like '(*)' are
allowed in usual data constructors, but not in GADTs).
-}

constrs :: { Located ([AddAnn],[LConDecl GhcPs]) }
        : maybe_docnext '=' constrs1    { L (comb2 $2 $3) ([mj AnnEqual $2]
                                                     ,addConDocs (unLoc $3) $1)}

constrs1 :: { Located [LConDecl GhcPs] }
        : constrs1 maybe_docnext '|' maybe_docprev constr
            {% addAnnotation (gl $ head $ unLoc $1) AnnVbar (gl $3)
               >> return (sLL $1 $> (addConDoc $5 $2 : addConDocFirst (unLoc $1) $4)) }
        | constr                                          { sL1 $1 [$1] }

constr :: { LConDecl GhcPs }
        : maybe_docnext forall context_no_ops '=>' constr_stuff
                {% ams (let (con,details,doc_prev) = unLoc $5 in
                  addConDoc (L (comb4 $2 $3 $4 $5) (mkConDeclH98 con
                                                       (snd $ unLoc $2)
                                                       (Just $3)
                                                       details))
                            ($1 `mplus` doc_prev))
                        (mu AnnDarrow $4:(fst $ unLoc $2)) }
        | maybe_docnext forall constr_stuff
                {% ams ( let (con,details,doc_prev) = unLoc $3 in
                  addConDoc (L (comb2 $2 $3) (mkConDeclH98 con
                                                      (snd $ unLoc $2)
                                                      Nothing   -- No context
                                                      details))
                            ($1 `mplus` doc_prev))
                       (fst $ unLoc $2) }

forall :: { Located ([AddAnn], Maybe [LHsTyVarBndr GhcPs]) }
        : 'forall' tv_bndrs '.'       { sLL $1 $> ([mu AnnForall $1,mj AnnDot $3], Just $2) }
        | {- empty -}                 { noLoc ([], Nothing) }

constr_stuff :: { Located (Located RdrName, HsConDeclDetails GhcPs, Maybe LHsDocString) }
    -- See Note [Parsing data constructors is hard] in RdrHsSyn
        : btype_no_ops                     {% do { c <- splitCon (unLoc $1)
                                                 ; return $ sL1 $1 c } }
        | btype_no_ops conop maybe_docprev btype_no_ops
            {% do { lhs <- splitTilde (reverse (unLoc $1))
                  ; (_, ds_l) <- checkInfixConstr lhs
                  ; let rhs1 = foldl1 mkHsAppTy (reverse (unLoc $4))
                  ; (rhs, ds_r) <- checkInfixConstr rhs1
                  ; return $ if isJust (ds_l `mplus` $3)
                               then sLL $1 $> ($2, InfixCon lhs rhs1, $3)
                               else sLL $1 $> ($2, InfixCon lhs rhs, ds_r) } }

fielddecls :: { [LConDeclField GhcPs] }
        : {- empty -}     { [] }
        | fielddecls1     { $1 }

fielddecls1 :: { [LConDeclField GhcPs] }
        : fielddecl maybe_docnext ',' maybe_docprev fielddecls1
            {% addAnnotation (gl $1) AnnComma (gl $3) >>
               return ((addFieldDoc $1 $4) : addFieldDocs $5 $2) }
        | fielddecl   { [$1] }

fielddecl :: { LConDeclField GhcPs }
                                              -- A list because of   f,g :: Int
        : maybe_docnext sig_vars '::' ctype maybe_docprev
            {% ams (L (comb2 $2 $4)
                      (ConDeclField noExt (reverse (map (\ln@(L l n) -> L l $ FieldOcc noExt ln) (unLoc $2))) $4 ($1 `mplus` $5)))
                   [mu AnnDcolon $3] }

-- Reversed!
maybe_derivings :: { HsDeriving GhcPs }
        : {- empty -}             { noLoc [] }
        | derivings               { $1 }

-- A list of one or more deriving clauses at the end of a datatype
derivings :: { HsDeriving GhcPs }
        : derivings deriving      { sLL $1 $> $ $2 : unLoc $1 }
        | deriving                { sLL $1 $> [$1] }

-- The outer Located is just to allow the caller to
-- know the rightmost extremity of the 'deriving' clause
deriving :: { LHsDerivingClause GhcPs }
        : 'deriving' deriv_clause_types
              {% let { full_loc = comb2 $1 $> }
                 in ams (L full_loc $ HsDerivingClause noExt Nothing $2)
                        [mj AnnDeriving $1] }

        | 'deriving' deriv_strategy_no_via deriv_clause_types
              {% let { full_loc = comb2 $1 $> }
                 in ams (L full_loc $ HsDerivingClause noExt (Just $2) $3)
                        [mj AnnDeriving $1] }

        | 'deriving' deriv_clause_types deriv_strategy_via
              {% let { full_loc = comb2 $1 $> }
                 in ams (L full_loc $ HsDerivingClause noExt (Just $3) $2)
                        [mj AnnDeriving $1] }

deriv_clause_types :: { Located [LHsSigType GhcPs] }
        : qtycondoc           { sL1 $1 [mkLHsSigType $1] }
        | '(' ')'             {% ams (sLL $1 $> [])
                                     [mop $1,mcp $2] }
        | '(' deriv_types ')' {% ams (sLL $1 $> $2)
                                     [mop $1,mcp $3] }
             -- Glasgow extension: allow partial
             -- applications in derivings

-----------------------------------------------------------------------------
-- Value definitions

{- Note [Declaration/signature overlap]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There's an awkward overlap with a type signature.  Consider
        f :: Int -> Int = ...rhs...
   Then we can't tell whether it's a type signature or a value
   definition with a result signature until we see the '='.
   So we have to inline enough to postpone reductions until we know.
-}

{-
  ATTENTION: Dirty Hackery Ahead! If the second alternative of vars is var
  instead of qvar, we get another shift/reduce-conflict. Consider the
  following programs:

     { (^^) :: Int->Int ; }          Type signature; only var allowed

     { (^^) :: Int->Int = ... ; }    Value defn with result signature;
                                     qvar allowed (because of instance decls)

  We can't tell whether to reduce var to qvar until after we've read the signatures.
-}

-- [t1, t2, t3, t4, t5]

docdecl :: { LHsDecl GhcPs }
        : docdecld { sL1 $1 (DocD noExt (unLoc $1)) }

docdecld :: { LDocDecl }
        : docnext                               { sL1 $1 (DocCommentNext (unLoc $1)) }
        | docprev                               { sL1 $1 (DocCommentPrev (unLoc $1)) }
        | docnamed                              { sL1 $1 (case (unLoc $1) of (n, doc) -> DocCommentNamed n doc) }
        | docsection                            { sL1 $1 (case (unLoc $1) of (n, doc) -> DocGroup n doc) }

decl_no_th :: { LHsDecl GhcPs }
        : sigdecl               { $1 }

        | '!' aexp rhs          {% do { let { e = sLL $1 $2 (SectionR noExt (sL1 $1 (HsVar noExt (sL1 $1 bang_RDR))) $2)
                                            ; l = comb2 $1 $> };
                                        (ann, r) <- checkValDef empty SrcStrict e Nothing $3 ;
                                        hintBangPat (comb2 $1 $2) (unLoc e) ;
                                        -- Depending upon what the pattern looks like we might get either
                                        -- a FunBind or PatBind back from checkValDef. See Note
                                        -- [FunBind vs PatBind]
                                        case r of {
                                          (FunBind _ n _ _ _) ->
                                                ams (L l ()) [mj AnnFunId n] >> return () ;
                                          (PatBind _ (L lh _lhs) _rhs _) ->
                                                ams (L lh ()) [] >> return () } ;

                                        _ <- ams (L l ()) (ann ++ fst (unLoc $3) ++ [mj AnnBang $1]) ;
                                        return $! (sL l $ ValD noExt r) } }

        | infixexp_top opt_sig rhs  {% do { (ann,r) <- checkValDef empty NoSrcStrict $1 (snd $2) $3;
                                        let { l = comb2 $1 $> };
                                        -- Depending upon what the pattern looks like we might get either
                                        -- a FunBind or PatBind back from checkValDef. See Note
                                        -- [FunBind vs PatBind]
                                        case r of {
                                          (FunBind _ n _ _ _) ->
                                                ams (L l ()) (mj AnnFunId n:(fst $2)) >> return () ;
                                          (PatBind _ (L lh _lhs) _rhs _) ->
                                                ams (L lh ()) (fst $2) >> return () } ;
                                        _ <- ams (L l ()) (ann ++ (fst $ unLoc $3));
                                        return $! (sL l $ ValD noExt r) } }
        | pattern_synonym_decl  { $1 }
        | docdecl               { $1 }

decl    :: { LHsDecl GhcPs }
        : decl_no_th            { $1 }

        -- Why do we only allow naked declaration splices in top-level
        -- declarations and not here? Short answer: because readFail009
        -- fails terribly with a panic in cvBindsAndSigs otherwise.
        | splice_exp            { sLL $1 $> $ mkSpliceDecl $1 }

rhs     :: { Located ([AddAnn],GRHSs GhcPs (LHsExpr GhcPs)) }
        : '=' exp wherebinds    { sL (comb3 $1 $2 $3)
                                    ((mj AnnEqual $1 : (fst $ unLoc $3))
                                    ,GRHSs noExt (unguardedRHS (comb3 $1 $2 $3) $2)
                                   (snd $ unLoc $3)) }
        | gdrhs wherebinds      { sLL $1 $>  (fst $ unLoc $2
                                    ,GRHSs noExt (reverse (unLoc $1))
                                                    (snd $ unLoc $2)) }

gdrhs :: { Located [LGRHS GhcPs (LHsExpr GhcPs)] }
        : gdrhs gdrh            { sLL $1 $> ($2 : unLoc $1) }
        | gdrh                  { sL1 $1 [$1] }

gdrh :: { LGRHS GhcPs (LHsExpr GhcPs) }
        : '|' guardquals '=' exp  {% ams (sL (comb2 $1 $>) $ GRHS noExt (unLoc $2) $4)
                                         [mj AnnVbar $1,mj AnnEqual $3] }

sigdecl :: { LHsDecl GhcPs }
        :
        -- See Note [Declaration/signature overlap] for why we need infixexp here
          infixexp_top '::' sigtypedoc
                        {% do v <- checkValSigLhs $1
                        ; _ <- ams (sLL $1 $> ()) [mu AnnDcolon $2]
                        ; return (sLL $1 $> $ SigD noExt $
                                  TypeSig noExt [v] (mkLHsSigWcType $3)) }

        | var ',' sig_vars '::' sigtypedoc
           {% do { let sig = TypeSig noExt ($1 : reverse (unLoc $3))
                                     (mkLHsSigWcType $5)
                 ; addAnnotation (gl $1) AnnComma (gl $2)
                 ; ams ( sLL $1 $> $ SigD noExt sig )
                       [mu AnnDcolon $4] } }

        | infix prec ops
              {% ams (sLL $1 $> $ SigD noExt
                        (FixSig noExt (FixitySig noExt (fromOL $ unLoc $3)
                                (Fixity (fst $ unLoc $2) (snd $ unLoc $2) (unLoc $1)))))
                     [mj AnnInfix $1,mj AnnVal $2] }

        | pattern_synonym_sig   { sLL $1 $> . SigD noExt . unLoc $ $1 }

        | '{-# COMPLETE' con_list opt_tyconsig  '#-}'
                {% let (dcolon, tc) = $3
                   in ams
                       (sLL $1 $>
                         (SigD noExt (CompleteMatchSig noExt (getCOMPLETE_PRAGs $1) $2 tc)))
                    ([ mo $1 ] ++ dcolon ++ [mc $4]) }

        -- This rule is for both INLINE and INLINABLE pragmas
        | '{-# INLINE' activation qvar '#-}'
                {% ams ((sLL $1 $> $ SigD noExt (InlineSig noExt $3
                            (mkInlinePragma (getINLINE_PRAGs $1) (getINLINE $1)
                                            (snd $2)))))
                       ((mo $1:fst $2) ++ [mc $4]) }

        | '{-# SCC' qvar '#-}'
          {% ams (sLL $1 $> (SigD noExt (SCCFunSig noExt (getSCC_PRAGs $1) $2 Nothing)))
                 [mo $1, mc $3] }

        | '{-# SCC' qvar STRING '#-}'
          {% do { scc <- getSCC $3
                ; let str_lit = StringLiteral (getSTRINGs $3) scc
                ; ams (sLL $1 $> (SigD noExt (SCCFunSig noExt (getSCC_PRAGs $1) $2 (Just ( sL1 $3 str_lit)))))
                      [mo $1, mc $4] } }

        | '{-# SPECIALISE' activation qvar '::' sigtypes1 '#-}'
             {% ams (
                 let inl_prag = mkInlinePragma (getSPEC_PRAGs $1)
                                             (NoUserInline, FunLike) (snd $2)
                  in sLL $1 $> $ SigD noExt (SpecSig noExt $3 (fromOL $5) inl_prag))
                    (mo $1:mu AnnDcolon $4:mc $6:(fst $2)) }

        | '{-# SPECIALISE_INLINE' activation qvar '::' sigtypes1 '#-}'
             {% ams (sLL $1 $> $ SigD noExt (SpecSig noExt $3 (fromOL $5)
                               (mkInlinePragma (getSPEC_INLINE_PRAGs $1)
                                               (getSPEC_INLINE $1) (snd $2))))
                       (mo $1:mu AnnDcolon $4:mc $6:(fst $2)) }

        | '{-# SPECIALISE' 'instance' inst_type '#-}'
                {% ams (sLL $1 $>
                                  $ SigD noExt (SpecInstSig noExt (getSPEC_PRAGs $1) $3))
                       [mo $1,mj AnnInstance $2,mc $4] }

        -- A minimal complete definition
        | '{-# MINIMAL' name_boolformula_opt '#-}'
            {% ams (sLL $1 $> $ SigD noExt (MinimalSig noExt (getMINIMAL_PRAGs $1) $2))
                   [mo $1,mc $3] }

activation :: { ([AddAnn],Maybe Activation) }
        : {- empty -}                           { ([],Nothing) }
        | explicit_activation                   { (fst $1,Just (snd $1)) }

explicit_activation :: { ([AddAnn],Activation) }  -- In brackets
        : '[' INTEGER ']'       { ([mj AnnOpenS $1,mj AnnVal $2,mj AnnCloseS $3]
                                  ,ActiveAfter  (getINTEGERs $2) (fromInteger (il_value (getINTEGER $2)))) }
        | '[' '~' INTEGER ']'   { ([mj AnnOpenS $1,mj AnnTilde $2,mj AnnVal $3
                                                 ,mj AnnCloseS $4]
                                  ,ActiveBefore (getINTEGERs $3) (fromInteger (il_value (getINTEGER $3)))) }

-----------------------------------------------------------------------------
-- Expressions

quasiquote :: { Located (HsSplice GhcPs) }
        : TH_QUASIQUOTE   { let { loc = getLoc $1
                                ; ITquasiQuote (quoter, quote, quoteSpan) = unLoc $1
                                ; quoterId = mkUnqual varName quoter }
                            in sL1 $1 (mkHsQuasiQuote quoterId (RealSrcSpan quoteSpan) quote) }
        | TH_QQUASIQUOTE  { let { loc = getLoc $1
                                ; ITqQuasiQuote (qual, quoter, quote, quoteSpan) = unLoc $1
                                ; quoterId = mkQual varName (qual, quoter) }
                            in sL (getLoc $1) (mkHsQuasiQuote quoterId (RealSrcSpan quoteSpan) quote) }


exp   :: { LHsExpr GhcPs }
        : infixexp '::' sigtype {% ams (sLL $1 $> $ ExprWithTySig (mkLHsSigWcType $3) $1)
                                       [mu AnnDcolon $2] }
        | infixexp '-<' exp     {% ams (sLL $1 $> $ HsArrApp noExt $1 $3
                                                        HsFirstOrderApp True)
                                       [mu Annlarrowtail $2] }
        | infixexp '>-' exp     {% ams (sLL $1 $> $ HsArrApp noExt $3 $1
                                                      HsFirstOrderApp False)
                                       [mu Annrarrowtail $2] }
        | infixexp '-<<' exp    {% ams (sLL $1 $> $ HsArrApp noExt $1 $3
                                                      HsHigherOrderApp True)
                                       [mu AnnLarrowtail $2] }
        | infixexp '>>-' exp    {% ams (sLL $1 $> $ HsArrApp noExt $3 $1
                                                      HsHigherOrderApp False)
                                       [mu AnnRarrowtail $2] }
        | infixexp              { $1 }

infixexp :: { LHsExpr GhcPs }
        : exp10 { $1 }
        | infixexp qop exp10  {% ams (sLL $1 $> (OpApp noExt $1 $2 $3))
                                     [mj AnnVal $2] }
                 -- AnnVal annotation for NPlusKPat, which discards the operator

infixexp_top :: { LHsExpr GhcPs }
        : exp10_top               { $1 }
        | infixexp_top qop exp10_top
                                  {% ams (sLL $1 $> (OpApp noExt $1 $2 $3))
                                         [mj AnnVal $2] }


exp10_top :: { LHsExpr GhcPs }
        : '-' fexp                      {% ams (sLL $1 $> $ NegApp noExt $2 noSyntaxExpr)
                                               [mj AnnMinus $1] }


        | hpc_annot exp        {% ams (sLL $1 $> $ HsTickPragma noExt (snd $ fst $ fst $ unLoc $1)
                                                                (snd $ fst $ unLoc $1) (snd $ unLoc $1) $2)
                                      (fst $ fst $ fst $ unLoc $1) }

        | '{-# CORE' STRING '#-}' exp  {% ams (sLL $1 $> $ HsCoreAnn noExt (getCORE_PRAGs $1) (getStringLiteral $2) $4)
                                              [mo $1,mj AnnVal $2
                                              ,mc $3] }
                                          -- hdaume: core annotation
        | fexp                         { $1 }

exp10 :: { LHsExpr GhcPs }
        : exp10_top            { $1 }
        | scc_annot exp        {% ams (sLL $1 $> $ HsSCC noExt (snd $ fst $ unLoc $1) (snd $ unLoc $1) $2)
                                      (fst $ fst $ unLoc $1) }

optSemi :: { ([Located a],Bool) }
        : ';'         { ([$1],True) }
        | {- empty -} { ([],False) }

scc_annot :: { Located (([AddAnn],SourceText),StringLiteral) }
        : '{-# SCC' STRING '#-}'      {% do scc <- getSCC $2
                                            ; return $ sLL $1 $>
                                               (([mo $1,mj AnnValStr $2
                                                ,mc $3],getSCC_PRAGs $1),(StringLiteral (getSTRINGs $2) scc)) }
        | '{-# SCC' VARID  '#-}'      { sLL $1 $> (([mo $1,mj AnnVal $2
                                         ,mc $3],getSCC_PRAGs $1)
                                        ,(StringLiteral NoSourceText (getVARID $2))) }

hpc_annot :: { Located ( (([AddAnn],SourceText),(StringLiteral,(Int,Int),(Int,Int))),
                         ((SourceText,SourceText),(SourceText,SourceText))
                       ) }
      : '{-# GENERATED' STRING INTEGER ':' INTEGER '-' INTEGER ':' INTEGER '#-}'
                                      { sLL $1 $> $ ((([mo $1,mj AnnVal $2
                                              ,mj AnnVal $3,mj AnnColon $4
                                              ,mj AnnVal $5,mj AnnMinus $6
                                              ,mj AnnVal $7,mj AnnColon $8
                                              ,mj AnnVal $9,mc $10],
                                                getGENERATED_PRAGs $1)
                                              ,((getStringLiteral $2)
                                               ,( fromInteger $ il_value $ getINTEGER $3
                                                , fromInteger $ il_value $ getINTEGER $5
                                                )
                                               ,( fromInteger $ il_value $ getINTEGER $7
                                                , fromInteger $ il_value $ getINTEGER $9
                                                )
                                               ))
                                             , (( getINTEGERs $3
                                                , getINTEGERs $5
                                                )
                                               ,( getINTEGERs $7
                                                , getINTEGERs $9
                                                )))
                                         }

terms     :: { LHsTerms }
          -- : term                          { [$1] }
          : term terms                    { pprTrace "Using terms!!!!!" empty ($1 : $2) }
          | {- empty -}                   { [] }

-- covers `exp`, `atype`, `ctype`, `aexp`, `aexp1`, `aexp2`    -- NOTE: possibly also texp
term      :: { LHsTerm }
          : '(' terms ')'                 { sLL $1 $> $ HsParTerm $2 }
          -- | '(' tup_terms ')'             { sLL $1 $> $ HsTupParTerm $2 }
          | '(#' terms '#)'               { sLL $1 $> $ HsBoxParTerm $2 }
          -- | '(#' tup_terms '#)'           { sLL $1 $> $ HsBoxTupParTerm $2 }
          | '[' terms ']'                 { sLL $1 $> $ HsBracketTerm $2 }
          | '`' terms '`'                 { sLL $1 $> $ HsBacktickTerm $2 }
          | infixexp '-<' exp             { sLL $1 $> $ HsArrAppTerm $1 $3 HsFirstOrderApp True }
          | infixexp '>-' exp             { sLL $1 $> $ HsArrAppTerm $3 $1 HsFirstOrderApp False }
          | infixexp '-<<' exp            { sLL $1 $> $ HsArrAppTerm $1 $3 HsHigherOrderApp True }
          | infixexp '>>-' exp            { sLL $1 $> $ HsArrAppTerm $3 $1 HsHigherOrderApp False }
          | scc_annot exp                 { sLL $1 $> $ HsSccAnnTerm (snd $ fst $ unLoc $1) (snd $ unLoc $1) $2 }
          | hpc_annot exp                 { sLL $1 $> $ HsTickPragmaTerm (snd $ fst $ fst $ unLoc $1)
                                                                         (snd $ fst $ unLoc $1)
                                                                         (snd $ unLoc $1)
                                                                         $2 }
          | '{-# CORE' STRING '#-}' exp   { sLL $1 $> $ HsCoreAnnTerm (getCORE_PRAGs $1) (getStringLiteral $2) $4 }
          | 'static' aexp                 { sLL $1 $> $ HsStaticTerm $2 }
          | TYPEAPP atype                 { sLL $1 $> $ HsTypeAppTerm $2 }
          | '$(' exp ')'                  { sLL $1 $> $ HsDollarParenTerm $2 }
          | '$$(' exp ')'                 { sLL $1 $> $ HsDoubleDollarParenTerm $2 }
          | '[|' exp '|]'                 { sLL $1 $> $ HsExpQuoteTerm $2}
          | '[||' exp '||]'               { sLL $1 $> $ HsTExpQuoteTerm $2 }
          | '[t|' ctype '|]'              { sLL $1 $> $ HsTypQuoteTerm $2 }
          | '[p|' infixexp '|]'           { sLL $1 $> $ HsPatQuoteTerm $2 }
          | '[d|' cvtopbody '|]'          { sLL $1 $> $ HsDecQuoteTerm $2 }
          | '(|' aexp2 cmdargs '|)'       { sLL $1 $> $ HsParenBarTerm $2 $3 }
          | '{' fbinds '}'                { sLL $1 $> $ HsFBindsTerm $2 }     -- for fbinds inside of aexp1
          | 'let' binds 'in' exp          { sLL $1 $> $ HsLetTerm $2 $4 }
          | 'if' exp optSemi 'then' exp optSemi 'else' exp
                                          {% checkDoAndIfThenElse $2 (snd $3) $5 (snd $6) $8 >>
                                             return (sLL $1 $> $ HsIfThenElseTerm $2 $5 $8) }
          | 'if' ifgdpats                 { sLL $1 $> $ HsOnlyIfTerm $2 } -- `ifgdpats` contains `binds`   -- 'if' ifgdpats inside of aexp
          | '\\' 'lcase' altslist         { sLL $1 $> $ HsLcaseTerm $3 }
          | '\\' apat apats '->' exp      { sLL $1 $> $ HsApatsArrowTerm $2 $3 $5 }
          | '::' ctype                    { sLL $1 $> $ HsDconTerm $2 }    -- type on right
          | 'case' exp 'of' altslist      { sLL $1 $> $ HsCaseTerm $2 $4 }
          | 'do' stmtlist                 { sLL $1 $> $ HsDoTerm $2 }
          | 'mdo' stmtlist                { sLL $1 $> $ HsMdoTerm $2 }
          | 'proc' aexp '->' exp          { sLL $1 $> $ HsProcTerm $2 $4 }
          | SIMPLEQUOTE                   { sL1 $1    $ HsSimplequoteTerm }       -- How to encode these? They don't come with a FastString... make one ourselves?  -- generalized structure
          | TH_TY_QUOTE                   { sL1 $1    $ HsThTyQuoteTerm } -- multiple fastStrings and a realsrcspan... what to do for right hand side?      -- generalized structure
          | strict_mark                   { sL1 $1    $ HsStrictmarkTerm $1 }
          | unpackedness atype            { sLL $1 $> $ HsUnpackednessTerm $1 $2 }
          | unpackedness strictness atype { sLL $1 $> $ HsUnpackednessStrictnessTerm $1 $2 $3 }
          | qvar '@' aexp                 { sLL $1 $> $ HsAtTerm $1 $3 }
          | '_'                           { sL1 $1 $ HsUnderscoreTerm }
          -- | list                          { sL1 $1 $ HsListTerm $1 } -- Is this right?
          | context '=>' ctype            { sLL $1 $> $ HsFatArrowTerm $1 $3 }
          | gen_name                      { sL1 $1 $ HsGenName (unLoc $1) }
          -- need to add case: 'name' '@' pattern
          -- '_', ';', '|', '..'

{-
gen_name :: { LHsTerm }
          : SIMPLEQUOTE gen_name       -- How to encode these? They don't come with a FastString... make one ourselves?
          | TH_TY_QUOTE gen_name       -- multiple fastStrings and a realsrcspan... what to do for right hand side?
          | gen_name2
-}

gen_name  :: { Located GenData }
          : 'unsafe'          { sL1 $1 $! mkUnsafeData (fsLit "unsafe") }
          | 'safe'            { sL1 $1 $! mkSafeData (fsLit "safe") }
          | 'interruptible'   { sL1 $1 $! mkInterruptibleData (fsLit "interruptible") }
          | 'forall'          { sL1 $1 $! mkForallData (fsLit "forall") }
          | 'family'          { sL1 $1 $! mkFamilyData (fsLit "family") }
          | 'role'            { sL1 $1 $! mkRoleData (fsLit "role") }
          | special_id        { sL1 $1 $! mkSpecialIdData (unLoc $1) }
          | literal           { sL1 $1 $! mkLiteralData (unLoc $1) }
          | INTEGER           { sL1 $1 $! mkIntegerData (mkHsIntegral   (getINTEGER $1)) }
          | RATIONAL          { sL1 $1 $! mkRationalData (mkHsFractional (getRATIONAL $1)) }
          | QCONID            { sL1 $1 $! mkQConidData (getQCONID $1) }
          | CONID             { sL1 $1 $! mkConidData (getCONID $1) }
          | QCONSYM           { sL1 $1 $! mkQConsymData (getQCONSYM $1) }
          | CONSYM            { sL1 $1 $! mkConsymData (getCONSYM $1) }
          | QVARID            { sL1 $1 $! mkQVaridData (getQVARID $1) }
          | VARID             { sL1 $1 $! mkVaridData (getVARID $1) }
          | QVARSYM           { sL1 $1 $! mkQVarsymData (getQVARSYM $1) }
          | VARSYM            { sL1 $1 $! mkVarsymData (getVARSYM $1) }
          | IPDUPVARID        { sL1 $1 $! mkIPDupVaridData (getIPDUPVARID $1) }      -- from ipvar
          | LABELVARID        { sL1 $1 $! mkLabelVaridData (getLABELVARID $1) }      -- from overloaded_label
          | TH_ID_SPLICE      { sL1 $1 $! mkThIdSpliceData (getTH_ID_SPLICE $1) }    -- from splice_exp
          | TH_ID_TY_SPLICE   { sL1 $1 $! mkThIdTySpliceData (getTH_ID_TY_SPLICE $1) } -- from splice_exp
          | quasiquote        { sL1 $1 $! mkQuasiquoteData (unLoc $1) }
          | ':'               { sL1 $1 $! mkColonData (fsLit ":") }
          | '->'              { sL1 $1 $! mkArrowData (fsLit "->") }
          | '~'               { sL1 $1 $! mkTwiddleData (fsLit "~") }
          | '-'               { sL1 $1 $! mkMinusSignData (fsLit "-") }
          | special_sym       { sL1 $1 $! mkSpecialSymData (unLoc $1) } -- {special_sym contains '!', '.', '*'}

-- tup_terms :: { [HsTerm] }  -- expression in texp and type-level from atype
--           : terms commas_tup_tail_terms   { $1 ++ $2 }
--           | terms bars                    { $1 ++ (sL1 $2 $ HsTupBars $2) }
--           -- | bar_terms2                    { [$1] } -- covers the case '(#' bar_types '#)' in atype
--           | commas tup_tail_terms         { (sL1 $1 $ HsTupCommas $1) :  $2}
--           | bars terms bars0              { (sL1 $1 $ HsTupBars $1) : $2 ++ (sl1 $3 $ HsTupBars0 $3) }
--
-- commas_tup_tail_terms :: { [HsTerm] }
--           : commas tup_tail_terms         { (sL1 $1 $ HsTupCommas $1) : $2 }  -- [HsCommaTerm $1] ++ $2
--
-- tup_tail_terms :: { [HsTerm] }
--           : terms commas_tup_tail_terms   { $1 ++ $2 }
--           | terms                         { $1 }
--           | {- empty -}                   { [] } -- We might not need this case since terms can also boil down to {- empty -}

-- bar_terms2 :: { HsTerm }
--           : terms '|' terms               { HsBarTerms2 [$1, $3] }  -- sL1 (HsTermBar $1) : $3 }
--           | terms '|' bar_terms2          { HsBarTerms2 ($1 : (getTerms $3)) }

fexp    :: { LHsExpr GhcPs }
        : fexp aexp                  {% checkBlockArguments $1 >> checkBlockArguments $2 >>
                                        return (sLL $1 $> $ (HsApp noExt $1 $2)) }
        | fexp TYPEAPP atype         {% checkBlockArguments $1 >>
                                        ams (sLL $1 $> $ HsAppType (mkHsWildCardBndrs $3) $1)
                                            [mj AnnAt $2] }
        | 'static' aexp              {% ams (sLL $1 $> $ HsStatic noExt $2)
                                            [mj AnnStatic $1] }
        | aexp                       { $1 }

aexp    :: { LHsExpr GhcPs }
        : qvar '@' aexp         {% ams (sLL $1 $> $ EAsPat noExt $1 $3) [mj AnnAt $2] }
            -- If you change the parsing, make sure to understand
            -- Note [Lexing type applications] in Lexer.x

        | '~' aexp              {% ams (sLL $1 $> $ ELazyPat noExt $2) [mj AnnTilde $1] }

        | '\\' apat apats '->' exp
                   {% ams (sLL $1 $> $ HsLam noExt (mkMatchGroup FromSource
                            [sLL $1 $> $ Match { m_ext = noExt
                                               , m_ctxt = LambdaExpr
                                               , m_pats = $2:$3
                                               , m_grhss = unguardedGRHSs $5 }]))
                          [mj AnnLam $1, mu AnnRarrow $4] }
        | 'let' binds 'in' exp          {% ams (sLL $1 $> $ HsLet noExt (snd $ unLoc $2) $4)
                                               (mj AnnLet $1:mj AnnIn $3
                                                 :(fst $ unLoc $2)) }
        | '\\' 'lcase' altslist
            {% ams (sLL $1 $> $ HsLamCase noExt
                                   (mkMatchGroup FromSource (snd $ unLoc $3)))
                   (mj AnnLam $1:mj AnnCase $2:(fst $ unLoc $3)) }
        | 'if' exp optSemi 'then' exp optSemi 'else' exp
                           {% checkDoAndIfThenElse $2 (snd $3) $5 (snd $6) $8 >>
                              ams (sLL $1 $> $ mkHsIf $2 $5 $8)
                                  (mj AnnIf $1:mj AnnThen $4
                                     :mj AnnElse $7
                                     :(map (\l -> mj AnnSemi l) (fst $3))
                                    ++(map (\l -> mj AnnSemi l) (fst $6))) }
        | 'if' ifgdpats                 {% hintMultiWayIf (getLoc $1) >>
                                           ams (sLL $1 $> $ HsMultiIf noExt
                                                     (reverse $ snd $ unLoc $2))
                                               (mj AnnIf $1:(fst $ unLoc $2)) }
        | 'case' exp 'of' altslist      {% ams (L (comb3 $1 $3 $4) $
                                                   HsCase noExt $2 (mkMatchGroup
                                                   FromSource (snd $ unLoc $4)))
                                               (mj AnnCase $1:mj AnnOf $3
                                                  :(fst $ unLoc $4)) }
        | 'do' stmtlist              {% ams (L (comb2 $1 $2)
                                               (mkHsDo DoExpr (snd $ unLoc $2)))
                                               (mj AnnDo $1:(fst $ unLoc $2)) }
        | 'mdo' stmtlist            {% ams (L (comb2 $1 $2)
                                              (mkHsDo MDoExpr (snd $ unLoc $2)))
                                           (mj AnnMdo $1:(fst $ unLoc $2)) }
        | 'proc' aexp '->' exp
                       {% checkPattern empty $2 >>= \ p ->
                           checkCommand $4 >>= \ cmd ->
                           ams (sLL $1 $> $ HsProc noExt p (sLL $1 $> $ HsCmdTop noExt cmd))
                                            -- TODO: is LL right here?
                               [mj AnnProc $1,mu AnnRarrow $3] }

        | aexp1                 { $1 }

aexp1   :: { LHsExpr GhcPs }
        : aexp1 '{' fbinds '}' {% do { r <- mkRecConstrOrUpdate $1 (comb2 $2 $4)
                                                                   (snd $3)
                                     ; _ <- ams (sLL $1 $> ()) (moc $2:mcc $4:(fst $3))
                                     ; checkRecordSyntax (sLL $1 $> r) }}
        | aexp2                { $1 }

-- parse_type_in_exp :: { LHsExpr GhcPs }
--         : qvar                          { sL1 $1 (HsVar noExt   $! $1) }
--         | qcon                          { sL1 $1 (HsVar noExt   $! $1) }
--         | '(' '->' ')'                  { sLL $1 $> (TArrow noExt) }    -- Annotations here?
--         | '(' '~' ')'                   { sLL $1 $> (TTwiddle noExt) }  -- Annotations here?
        --ntgtycon                      { sL1 $1 (HsVar noExt   $! $1) }
                                        -- uses pprTrace to trace this funciton whenever it iscalled
                                        -- { pprTrace "TEST_PT" empty (sL1 $1 (HsVar noExt $! $1)}
        -- qcon   {converted to type level, and apply function used for ntgtycon}
        -- qvar {converted to type level, and apply function used for tyvar}


aexp2   :: { LHsExpr GhcPs }
        : qvar                          { sL1 $1 (HsVar noExt   $! $1) }
        | qcon                          { sL1 $1 (HsVar noExt   $! $1) }
        | ipvar                         { sL1 $1 (HsIPVar noExt $! unLoc $1) }
        | overloaded_label              { sL1 $1 (HsOverLabel noExt Nothing $! unLoc $1) }
        | literal                       { sL1 $1 (HsLit noExt  $! unLoc $1) }
      --  | ntgtycon                      { sL1 $1 (HsVar noExt   $! $1) }
-- This will enable overloaded strings permanently.  Normally the renamer turns HsString
-- into HsOverLit when -foverloaded-strings is on.
--      | STRING    { sL (getLoc $1) (HsOverLit $! mkHsIsString (getSTRINGs $1)
--                                       (getSTRING $1) noExt) }
        | INTEGER   { sL (getLoc $1) (HsOverLit noExt $! mkHsIntegral   (getINTEGER $1) ) }
        | RATIONAL  { sL (getLoc $1) (HsOverLit noExt $! mkHsFractional (getRATIONAL $1) ) }

        -- N.B.: sections get parsed by these next two productions.
        -- This allows you to write, e.g., '(+ 3, 4 -)', which isn't
        -- correct Haskell (you'd have to write '((+ 3), (4 -))')
        -- but the less cluttered version fell out of having texps.
        | '(' texp ')'                  {% ams (sLL $1 $> (HsPar noExt $2)) [mop $1,mcp $3] }
        | '(' tup_exprs ')'             {% do { e <- mkSumOrTuple Boxed (comb2 $1 $3) (snd $2)
                                              ; ams (sLL $1 $> e) ((mop $1:fst $2) ++ [mcp $3]) } }

        | '(#' texp '#)'                {% ams (sLL $1 $> (ExplicitTuple noExt [L (gl $2)
                                                         (Present noExt $2)] Unboxed))
                                               [mo $1,mc $3] }
        | '(#' tup_exprs '#)'           {% do { e <- mkSumOrTuple Unboxed (comb2 $1 $3) (snd $2)
                                              ; ams (sLL $1 $> e) ((mo $1:fst $2) ++ [mc $3]) } }

        | '[' list ']'      {% ams (sLL $1 $> (snd $2)) (mos $1:mcs $3:(fst $2)) }
        | '_'               { sL1 $1 $ EWildPat noExt }

        -- Template Haskell Extension
        | splice_exp            { $1 }

        | SIMPLEQUOTE  qvar     {% ams (sLL $1 $> $ HsBracket noExt (VarBr noExt True  (unLoc $2))) [mj AnnSimpleQuote $1,mj AnnName $2] }
        | SIMPLEQUOTE  qcon     {% ams (sLL $1 $> $ HsBracket noExt (VarBr noExt True  (unLoc $2))) [mj AnnSimpleQuote $1,mj AnnName $2] }
        | TH_TY_QUOTE tyvar     {% ams (sLL $1 $> $ HsBracket noExt (VarBr noExt False (unLoc $2))) [mj AnnThTyQuote $1,mj AnnName $2] }
        | TH_TY_QUOTE gtycon    {% ams (sLL $1 $> $ HsBracket noExt (VarBr noExt False (unLoc $2))) [mj AnnThTyQuote $1,mj AnnName $2] }
        | TH_TY_QUOTE {- nothing -} {% reportEmptyDoubleQuotes (getLoc $1) }
        | '[|' exp '|]'       {% ams (sLL $1 $> $ HsBracket noExt (ExpBr noExt $2))
                                      (if (hasE $1) then [mj AnnOpenE $1, mu AnnCloseQ $3]
                                                    else [mu AnnOpenEQ $1,mu AnnCloseQ $3]) }
        | '[||' exp '||]'     {% ams (sLL $1 $> $ HsBracket noExt (TExpBr noExt $2))
                                      (if (hasE $1) then [mj AnnOpenE $1,mc $3] else [mo $1,mc $3]) }
        | '[t|' ctype '|]'    {% ams (sLL $1 $> $ HsBracket noExt (TypBr noExt $2)) [mo $1,mu AnnCloseQ $3] }
        | '[p|' infixexp '|]' {% checkPattern empty $2 >>= \p ->
                                      ams (sLL $1 $> $ HsBracket noExt (PatBr noExt p))
                                          [mo $1,mu AnnCloseQ $3] }
        | '[d|' cvtopbody '|]' {% ams (sLL $1 $> $ HsBracket noExt (DecBrL noExt (snd $2)))
                                      (mo $1:mu AnnCloseQ $3:fst $2) }
        | quasiquote          { sL1 $1 (HsSpliceE noExt (unLoc $1)) }

        -- arrow notation extension
        | '(|' aexp2 cmdargs '|)'  {% ams (sLL $1 $> $ HsArrForm noExt $2
                                                           Nothing (reverse $3))
                                          [mu AnnOpenB $1,mu AnnCloseB $4] }

splice_exp :: { LHsExpr GhcPs }
        : TH_ID_SPLICE          {% ams (sL1 $1 $ mkHsSpliceE HasDollar
                                        (sL1 $1 $ HsVar noExt (sL1 $1 (mkUnqual varName
                                                           (getTH_ID_SPLICE $1)))))
                                       [mj AnnThIdSplice $1] }
        | '$(' exp ')'          {% ams (sLL $1 $> $ mkHsSpliceE HasParens $2)
                                       [mj AnnOpenPE $1,mj AnnCloseP $3] }
        | TH_ID_TY_SPLICE       {% ams (sL1 $1 $ mkHsSpliceTE HasDollar
                                        (sL1 $1 $ HsVar noExt (sL1 $1 (mkUnqual varName
                                                        (getTH_ID_TY_SPLICE $1)))))
                                       [mj AnnThIdTySplice $1] }
        | '$$(' exp ')'         {% ams (sLL $1 $> $ mkHsSpliceTE HasParens $2)
                                       [mj AnnOpenPTE $1,mj AnnCloseP $3] }

cmdargs :: { [LHsCmdTop GhcPs] }
        : cmdargs acmd                  { $2 : $1 }
        | {- empty -}                   { [] }

acmd    :: { LHsCmdTop GhcPs }
        : aexp2                 {% checkCommand $1 >>= \ cmd ->
                                    return (sL1 $1 $ HsCmdTop noExt cmd) }

cvtopbody :: { ([AddAnn],[LHsDecl GhcPs]) }
        :  '{'            cvtopdecls0 '}'      { ([mj AnnOpenC $1
                                                  ,mj AnnCloseC $3],$2) }
        |      vocurly    cvtopdecls0 close    { ([],$2) }

cvtopdecls0 :: { [LHsDecl GhcPs] }
        : topdecls_semi         { cvTopDecls $1 }
        | topdecls              { cvTopDecls $1 }

-----------------------------------------------------------------------------
-- Tuple expressions

-- "texp" is short for tuple expressions:
-- things that can appear unparenthesized as long as they're
-- inside parens or delimitted by commas
texp :: { LHsExpr GhcPs }
        : exp                           { $1 }

        -- Note [Parsing sections]
        -- ~~~~~~~~~~~~~~~~~~~~~~~
        -- We include left and right sections here, which isn't
        -- technically right according to the Haskell standard.
        -- For example (3 +, True) isn't legal.
        -- However, we want to parse bang patterns like
        --      (!x, !y)
        -- and it's convenient to do so here as a section
        -- Then when converting expr to pattern we unravel it again
        -- Meanwhile, the renamer checks that real sections appear
        -- inside parens.
        | infixexp qop        { sLL $1 $> $ SectionL noExt $1 $2 }
        | qopm infixexp       { sLL $1 $> $ SectionR noExt $1 $2 }

       -- View patterns get parenthesized above
        | exp '->' texp   {% ams (sLL $1 $> $ EViewPat noExt $1 $3) [mu AnnRarrow $2] }

-- Always at least one comma or bar.
tup_exprs :: { ([AddAnn],SumOrTuple) }
           : texp commas_tup_tail
                          {% do { addAnnotation (gl $1) AnnComma (fst $2)
                                ; return ([],Tuple ((sL1 $1 (Present noExt $1)) : snd $2)) } }

           | texp bars    { (mvbars (fst $2), Sum 1  (snd $2 + 1) $1) }

           | commas tup_tail
                {% do { mapM_ (\ll -> addAnnotation ll AnnComma ll) (fst $1)
                      ; return
                           ([],Tuple (map (\l -> L l missingTupArg) (fst $1) ++ $2)) } }

           | bars texp bars0
                { (mvbars (fst $1) ++ mvbars (fst $3), Sum (snd $1 + 1) (snd $1 + snd $3 + 1) $2) }

-- Always starts with commas; always follows an expr
commas_tup_tail :: { (SrcSpan,[LHsTupArg GhcPs]) }
commas_tup_tail : commas tup_tail
       {% do { mapM_ (\ll -> addAnnotation ll AnnComma ll) (tail $ fst $1)
             ; return (
            (head $ fst $1
            ,(map (\l -> L l missingTupArg) (tail $ fst $1)) ++ $2)) } }

-- Always follows a comma
tup_tail :: { [LHsTupArg GhcPs] }
          : texp commas_tup_tail {% addAnnotation (gl $1) AnnComma (fst $2) >>
                                    return ((L (gl $1) (Present noExt $1)) : snd $2) }
          | texp                 { [L (gl $1) (Present noExt $1)] }
          | {- empty -}          { [noLoc missingTupArg] }

-----------------------------------------------------------------------------
-- List expressions

-- The rules below are little bit contorted to keep lexps left-recursive while
-- avoiding another shift/reduce-conflict.
list :: { ([AddAnn],HsExpr GhcPs) }
        : texp    { ([],ExplicitList noExt Nothing [$1]) }
        | lexps   { ([],ExplicitList noExt Nothing (reverse (unLoc $1))) }
        | texp '..'             { ([mj AnnDotdot $2],
                                      ArithSeq noExt Nothing (From $1)) }
        | texp ',' exp '..'     { ([mj AnnComma $2,mj AnnDotdot $4],
                                  ArithSeq noExt Nothing
                                                             (FromThen $1 $3)) }
        | texp '..' exp         { ([mj AnnDotdot $2],
                                   ArithSeq noExt Nothing
                                                               (FromTo $1 $3)) }
        | texp ',' exp '..' exp { ([mj AnnComma $2,mj AnnDotdot $4],
                                    ArithSeq noExt Nothing
                                                (FromThenTo $1 $3 $5)) }
        | texp '|' flattenedpquals
             {% checkMonadComp >>= \ ctxt ->
                return ([mj AnnVbar $2],
                        mkHsComp ctxt (unLoc $3) $1) }

lexps :: { Located [LHsExpr GhcPs] }
        : lexps ',' texp          {% addAnnotation (gl $ head $ unLoc $1)
                                                            AnnComma (gl $2) >>
                                      return (sLL $1 $> (((:) $! $3) $! unLoc $1)) }
        | texp ',' texp            {% addAnnotation (gl $1) AnnComma (gl $2) >>
                                      return (sLL $1 $> [$3,$1]) }

-----------------------------------------------------------------------------
-- List Comprehensions

flattenedpquals :: { Located [LStmt GhcPs (LHsExpr GhcPs)] }
    : pquals   { case (unLoc $1) of
                    [qs] -> sL1 $1 qs
                    -- We just had one thing in our "parallel" list so
                    -- we simply return that thing directly

                    qss -> sL1 $1 [sL1 $1 $ ParStmt noExt [ParStmtBlock noExt qs [] noSyntaxExpr |
                                            qs <- qss]
                                            noExpr noSyntaxExpr]
                    -- We actually found some actual parallel lists so
                    -- we wrap them into as a ParStmt
                }

pquals :: { Located [[LStmt GhcPs (LHsExpr GhcPs)]] }
    : squals '|' pquals
                     {% addAnnotation (gl $ head $ unLoc $1) AnnVbar (gl $2) >>
                        return (sLL $1 $> (reverse (unLoc $1) : unLoc $3)) }
    | squals         { L (getLoc $1) [reverse (unLoc $1)] }

squals :: { Located [LStmt GhcPs (LHsExpr GhcPs)] }   -- In reverse order, because the last
                                        -- one can "grab" the earlier ones
    : squals ',' transformqual
             {% addAnnotation (gl $ head $ unLoc $1) AnnComma (gl $2) >>
                ams (sLL $1 $> ()) (fst $ unLoc $3) >>
                return (sLL $1 $> [sLL $1 $> ((snd $ unLoc $3) (reverse (unLoc $1)))]) }
    | squals ',' qual
             {% addAnnotation (gl $ head $ unLoc $1) AnnComma (gl $2) >>
                return (sLL $1 $> ($3 : unLoc $1)) }
    | transformqual        {% ams $1 (fst $ unLoc $1) >>
                              return (sLL $1 $> [L (getLoc $1) ((snd $ unLoc $1) [])]) }
    | qual                                { sL1 $1 [$1] }
--  | transformquals1 ',' '{|' pquals '|}'   { sLL $1 $> ($4 : unLoc $1) }
--  | '{|' pquals '|}'                       { sL1 $1 [$2] }

-- It is possible to enable bracketing (associating) qualifier lists
-- by uncommenting the lines with {| |} above. Due to a lack of
-- consensus on the syntax, this feature is not being used until we
-- get user demand.

transformqual :: { Located ([AddAnn],[LStmt GhcPs (LHsExpr GhcPs)] -> Stmt GhcPs (LHsExpr GhcPs)) }
                        -- Function is applied to a list of stmts *in order*
    : 'then' exp               { sLL $1 $> ([mj AnnThen $1], \ss -> (mkTransformStmt ss $2)) }
    | 'then' exp 'by' exp      { sLL $1 $> ([mj AnnThen $1,mj AnnBy  $3],\ss -> (mkTransformByStmt ss $2 $4)) }
    | 'then' 'group' 'using' exp
             { sLL $1 $> ([mj AnnThen $1,mj AnnGroup $2,mj AnnUsing $3], \ss -> (mkGroupUsingStmt ss $4)) }

    | 'then' 'group' 'by' exp 'using' exp
             { sLL $1 $> ([mj AnnThen $1,mj AnnGroup $2,mj AnnBy $3,mj AnnUsing $5], \ss -> (mkGroupByUsingStmt ss $4 $6)) }

-- Note that 'group' is a special_id, which means that you can enable
-- TransformListComp while still using Data.List.group. However, this
-- introduces a shift/reduce conflict. Happy chooses to resolve the conflict
-- in by choosing the "group by" variant, which is what we want.

-----------------------------------------------------------------------------
-- Guards

guardquals :: { Located [LStmt GhcPs (LHsExpr GhcPs)] }
    : guardquals1           { L (getLoc $1) (reverse (unLoc $1)) }

guardquals1 :: { Located [LStmt GhcPs (LHsExpr GhcPs)] }
    : guardquals1 ',' qual  {% addAnnotation (gl $ head $ unLoc $1) AnnComma
                                             (gl $2) >>
                               return (sLL $1 $> ($3 : unLoc $1)) }
    | qual                  { sL1 $1 [$1] }

-----------------------------------------------------------------------------
-- Case alternatives

altslist :: { Located ([AddAnn],[LMatch GhcPs (LHsExpr GhcPs)]) }
        : '{'            alts '}'  { sLL $1 $> ((moc $1:mcc $3:(fst $ unLoc $2))
                                               ,(reverse (snd $ unLoc $2))) }
        |     vocurly    alts  close { L (getLoc $2) (fst $ unLoc $2
                                        ,(reverse (snd $ unLoc $2))) }
        | '{'                 '}'    { sLL $1 $> ([moc $1,mcc $2],[]) }
        |     vocurly          close { noLoc ([],[]) }

alts    :: { Located ([AddAnn],[LMatch GhcPs (LHsExpr GhcPs)]) }
        : alts1                    { sL1 $1 (fst $ unLoc $1,snd $ unLoc $1) }
        | ';' alts                 { sLL $1 $> ((mj AnnSemi $1:(fst $ unLoc $2))
                                               ,snd $ unLoc $2) }

alts1   :: { Located ([AddAnn],[LMatch GhcPs (LHsExpr GhcPs)]) }
        : alts1 ';' alt         {% if null (snd $ unLoc $1)
                                     then return (sLL $1 $> (mj AnnSemi $2:(fst $ unLoc $1)
                                                  ,[$3]))
                                     else (ams (head $ snd $ unLoc $1)
                                               (mj AnnSemi $2:(fst $ unLoc $1))
                                           >> return (sLL $1 $> ([],$3 : (snd $ unLoc $1))) ) }
        | alts1 ';'             {% if null (snd $ unLoc $1)
                                     then return (sLL $1 $> (mj AnnSemi $2:(fst $ unLoc $1)
                                                  ,snd $ unLoc $1))
                                     else (ams (head $ snd $ unLoc $1)
                                               (mj AnnSemi $2:(fst $ unLoc $1))
                                           >> return (sLL $1 $> ([],snd $ unLoc $1))) }
        | alt                   { sL1 $1 ([],[$1]) }

alt     :: { LMatch GhcPs (LHsExpr GhcPs) }
           : pat alt_rhs  {%ams (sLL $1 $> (Match { m_ext = noExt
                                                  , m_ctxt = CaseAlt
                                                  , m_pats = [$1]
                                                  , m_grhss = snd $ unLoc $2 }))
                                      (fst $ unLoc $2)}

alt_rhs :: { Located ([AddAnn],GRHSs GhcPs (LHsExpr GhcPs)) }
        : ralt wherebinds           { sLL $1 $> (fst $ unLoc $2,
                                            GRHSs noExt (unLoc $1) (snd $ unLoc $2)) }

ralt :: { Located [LGRHS GhcPs (LHsExpr GhcPs)] }
        : '->' exp            {% ams (sLL $1 $> (unguardedRHS (comb2 $1 $2) $2))
                                     [mu AnnRarrow $1] }
        | gdpats              { sL1 $1 (reverse (unLoc $1)) }

gdpats :: { Located [LGRHS GhcPs (LHsExpr GhcPs)] }
        : gdpats gdpat                  { sLL $1 $> ($2 : unLoc $1) }
        | gdpat                         { sL1 $1 [$1] }

-- layout for MultiWayIf doesn't begin with an open brace, because it's hard to
-- generate the open brace in addition to the vertical bar in the lexer, and
-- we don't need it.
ifgdpats :: { Located ([AddAnn],[LGRHS GhcPs (LHsExpr GhcPs)]) }
         : '{' gdpats '}'                 { sLL $1 $> ([moc $1,mcc $3],unLoc $2)  }
         |     gdpats close               { sL1 $1 ([],unLoc $1) }

gdpat   :: { LGRHS GhcPs (LHsExpr GhcPs) }
        : '|' guardquals '->' exp
                                  {% ams (sL (comb2 $1 $>) $ GRHS noExt (unLoc $2) $4)
                                         [mj AnnVbar $1,mu AnnRarrow $3] }

-- 'pat' recognises a pattern, including one with a bang at the top
--      e.g.  "!x" or "!(x,y)" or "C a b" etc
-- Bangs inside are parsed as infix operator applications, so that
-- we parse them right when bang-patterns are off
pat     :: { LPat GhcPs }
pat     :  exp          {% checkPattern empty $1 }
        | '!' aexp      {% amms (checkPattern empty (sLL $1 $> (SectionR noExt
                                                     (sL1 $1 (HsVar noExt (sL1 $1 bang_RDR))) $2)))
                                [mj AnnBang $1] }

bindpat :: { LPat GhcPs }
bindpat :  exp            {% checkPattern
                                (text "Possibly caused by a missing 'do'?") $1 }
        | '!' aexp        {% amms (checkPattern
                                     (text "Possibly caused by a missing 'do'?")
                                     (sLL $1 $> (SectionR noExt (sL1 $1 (HsVar noExt (sL1 $1 bang_RDR))) $2)))
                                  [mj AnnBang $1] }

apat   :: { LPat GhcPs }
apat    : aexp                  {% checkPattern empty $1 }
        | '!' aexp              {% amms (checkPattern empty
                                            (sLL $1 $> (SectionR noExt
                                                (sL1 $1 (HsVar noExt (sL1 $1 bang_RDR))) $2)))
                                        [mj AnnBang $1] }

apats  :: { [LPat GhcPs] }
        : apat apats            { $1 : $2 }
        | {- empty -}           { [] }

-----------------------------------------------------------------------------
-- Statement sequences

stmtlist :: { Located ([AddAnn],[LStmt GhcPs (LHsExpr GhcPs)]) }
        : '{'           stmts '}'       { sLL $1 $> ((moc $1:mcc $3:(fst $ unLoc $2))
                                             ,(reverse $ snd $ unLoc $2)) } -- AZ:performance of reverse?
        |     vocurly   stmts close     { L (gl $2) (fst $ unLoc $2
                                                    ,reverse $ snd $ unLoc $2) }

--      do { ;; s ; s ; ; s ;; }
-- The last Stmt should be an expression, but that's hard to enforce
-- here, because we need too much lookahead if we see do { e ; }
-- So we use BodyStmts throughout, and switch the last one over
-- in ParseUtils.checkDo instead

stmts :: { Located ([AddAnn],[LStmt GhcPs (LHsExpr GhcPs)]) }
        : stmts ';' stmt  {% if null (snd $ unLoc $1)
                              then return (sLL $1 $> (mj AnnSemi $2:(fst $ unLoc $1)
                                                     ,$3 : (snd $ unLoc $1)))
                              else do
                               { ams (head $ snd $ unLoc $1) [mj AnnSemi $2]
                               ; return $ sLL $1 $> (fst $ unLoc $1,$3 :(snd $ unLoc $1)) }}

        | stmts ';'     {% if null (snd $ unLoc $1)
                             then return (sLL $1 $> (mj AnnSemi $2:(fst $ unLoc $1),snd $ unLoc $1))
                             else do
                               { ams (head $ snd $ unLoc $1)
                                               [mj AnnSemi $2]
                               ; return $1 } }
        | stmt                   { sL1 $1 ([],[$1]) }
        | {- empty -}            { noLoc ([],[]) }


-- For typing stmts at the GHCi prompt, where
-- the input may consist of just comments.
maybe_stmt :: { Maybe (LStmt GhcPs (LHsExpr GhcPs)) }
        : stmt                          { Just $1 }
        | {- nothing -}                 { Nothing }

stmt  :: { LStmt GhcPs (LHsExpr GhcPs) }
        : qual                          { $1 }
        | 'rec' stmtlist                {% ams (sLL $1 $> $ mkRecStmt (snd $ unLoc $2))
                                               (mj AnnRec $1:(fst $ unLoc $2)) }

qual  :: { LStmt GhcPs (LHsExpr GhcPs) }
    : bindpat '<-' exp                  {% ams (sLL $1 $> $ mkBindStmt $1 $3)
                                               [mu AnnLarrow $2] }
    | exp                               { sL1 $1 $ mkBodyStmt $1 }
    | 'let' binds                       {% ams (sLL $1 $>$ LetStmt noExt (snd $ unLoc $2))
                                               (mj AnnLet $1:(fst $ unLoc $2)) }

-----------------------------------------------------------------------------
-- Record Field Update/Construction

fbinds  :: { ([AddAnn],([LHsRecField GhcPs (LHsExpr GhcPs)], Bool)) }
        : fbinds1                       { $1 }
        | {- empty -}                   { ([],([], False)) }

fbinds1 :: { ([AddAnn],([LHsRecField GhcPs (LHsExpr GhcPs)], Bool)) }
        : fbind ',' fbinds1
                {% addAnnotation (gl $1) AnnComma (gl $2) >>
                   return (case $3 of (ma,(flds, dd)) -> (ma,($1 : flds, dd))) }
        | fbind                         { ([],([$1], False)) }
        | '..'                          { ([mj AnnDotdot $1],([],   True)) }

fbind   :: { LHsRecField GhcPs (LHsExpr GhcPs) }
        : qvar '=' texp {% ams  (sLL $1 $> $ HsRecField (sL1 $1 $ mkFieldOcc $1) $3 False)
                                [mj AnnEqual $2] }
                        -- RHS is a 'texp', allowing view patterns (Trac #6038)
                        -- and, incidentally, sections.  Eg
                        -- f (R { x = show -> s }) = ...

        | qvar          { sLL $1 $> $ HsRecField (sL1 $1 $ mkFieldOcc $1) placeHolderPunRhs True }
                        -- In the punning case, use a place-holder
                        -- The renamer fills in the final value

-----------------------------------------------------------------------------
-- Implicit Parameter Bindings

dbinds  :: { Located [LIPBind GhcPs] }
        : dbinds ';' dbind
                      {% addAnnotation (gl $ last $ unLoc $1) AnnSemi (gl $2) >>
                         return (let { this = $3; rest = unLoc $1 }
                              in rest `seq` this `seq` sLL $1 $> (this : rest)) }
        | dbinds ';'  {% addAnnotation (gl $ last $ unLoc $1) AnnSemi (gl $2) >>
                         return (sLL $1 $> (unLoc $1)) }
        | dbind                        { let this = $1 in this `seq` sL1 $1 [this] }
--      | {- empty -}                  { [] }

dbind   :: { LIPBind GhcPs }
dbind   : ipvar '=' exp                {% ams (sLL $1 $> (IPBind noExt (Left $1) $3))
                                              [mj AnnEqual $2] }

ipvar   :: { Located HsIPName }
        : IPDUPVARID            { sL1 $1 (HsIPName (getIPDUPVARID $1)) }

-----------------------------------------------------------------------------
-- Overloaded labels

overloaded_label :: { Located FastString }
        : LABELVARID          { sL1 $1 (getLABELVARID $1) }

-----------------------------------------------------------------------------
-- Warnings and deprecations

name_boolformula_opt :: { LBooleanFormula (Located RdrName) }
        : name_boolformula          { $1 }
        | {- empty -}               { noLoc mkTrue }

name_boolformula :: { LBooleanFormula (Located RdrName) }
        : name_boolformula_and                      { $1 }
        | name_boolformula_and '|' name_boolformula
                           {% aa $1 (AnnVbar, $2)
                              >> return (sLL $1 $> (Or [$1,$3])) }

name_boolformula_and :: { LBooleanFormula (Located RdrName) }
        : name_boolformula_and_list
                  { sLL (head $1) (last $1) (And ($1)) }

name_boolformula_and_list :: { [LBooleanFormula (Located RdrName)] }
        : name_boolformula_atom                               { [$1] }
        | name_boolformula_atom ',' name_boolformula_and_list
            {% aa $1 (AnnComma, $2) >> return ($1 : $3) }

name_boolformula_atom :: { LBooleanFormula (Located RdrName) }
        : '(' name_boolformula ')'  {% ams (sLL $1 $> (Parens $2)) [mop $1,mcp $3] }
        | name_var                  { sL1 $1 (Var $1) }

namelist :: { Located [Located RdrName] }
namelist : name_var              { sL1 $1 [$1] }
         | name_var ',' namelist {% addAnnotation (gl $1) AnnComma (gl $2) >>
                                    return (sLL $1 $> ($1 : unLoc $3)) }

name_var :: { Located RdrName }
name_var : var { $1 }
         | con { $1 }

-----------------------------------------
-- Data constructors
-- There are two different productions here as lifted list constructors
-- are parsed differently.

qcon_nowiredlist :: { Located RdrName }
        : gen_qcon                     { $1 }
        | sysdcon_nolist               { sL1 $1 $ nameRdrName (dataConName (unLoc $1)) }

qcon :: { Located RdrName }
  : gen_qcon              { $1 }
  | sysdcon               { sL1 $1 $ nameRdrName (dataConName (unLoc $1)) }
 -- EF: sysdcon not highly related to ntgtycon
gen_qcon :: { Located RdrName }
  : qconid                { $1 }
  | '(' qconsym ')'       {% ams (sLL $1 $> (unLoc $2))
                                   [mop $1,mj AnnVal $2,mcp $3] }
  -- EF: qconsym similar to qtyconsym in ntgtycon
con     :: { Located RdrName }
        : conid                 { $1 }
        | '(' consym ')'        {% ams (sLL $1 $> (unLoc $2))
                                       [mop $1,mj AnnVal $2,mcp $3] }
        | sysdcon               { sL1 $1 $ nameRdrName (dataConName (unLoc $1)) }

con_list :: { Located [Located RdrName] }
con_list : con                  { sL1 $1 [$1] }
         | con ',' con_list     {% addAnnotation (gl $1) AnnComma (gl $2) >>
                                   return (sLL $1 $> ($1 : unLoc $3)) }

sysdcon_nolist :: { Located DataCon }  -- Wired in data constructors
        : '(' ')'               {% ams (sLL $1 $> unitDataCon) [mop $1,mcp $2] }
        | '(' commas ')'        {% ams (sLL $1 $> $ tupleDataCon Boxed (snd $2 + 1))
                                       (mop $1:mcp $3:(mcommas (fst $2))) }
        | '(#' '#)'             {% ams (sLL $1 $> $ unboxedUnitDataCon) [mo $1,mc $2] }
        | '(#' commas '#)'      {% ams (sLL $1 $> $ tupleDataCon Unboxed (snd $2 + 1))
                                       (mo $1:mc $3:(mcommas (fst $2))) }

sysdcon :: { Located DataCon }
        : sysdcon_nolist                 { $1 }
        | '[' ']'               {% ams (sLL $1 $> nilDataCon) [mos $1,mcs $2] }

conop :: { Located RdrName }
        : consym                { $1 }
        | '`' conid '`'         {% ams (sLL $1 $> (unLoc $2))
                                       [mj AnnBackquote $1,mj AnnVal $2
                                       ,mj AnnBackquote $3] }

qconop :: { Located RdrName }
        : qconsym               { $1 }
        | '`' qconid '`'        {% ams (sLL $1 $> (unLoc $2))
                                       [mj AnnBackquote $1,mj AnnVal $2
                                       ,mj AnnBackquote $3] }

----------------------------------------------------------------------------
-- Type constructors


-- See Note [Unit tuples] in HsTypes for the distinction
-- between gtycon and ntgtycon
gtycon :: { Located RdrName }  -- A "general" qualified tycon, including unit tuples
        : ntgtycon                     { $1 }
        | '(' ')'                      {% ams (sLL $1 $> $ getRdrName unitTyCon)
                                              [mop $1,mcp $2] }
        | '(#' '#)'                    {% ams (sLL $1 $> $ getRdrName unboxedUnitTyCon)
                                              [mo $1,mc $2] }

ntgtycon :: { Located RdrName }  -- A "general" qualified tycon, excluding unit tuples
        : oqtycon               { $1 }
        | '(' commas ')'        {% ams (sLL $1 $> $ getRdrName (tupleTyCon Boxed
                                                        (snd $2 + 1)))
                                       (mop $1:mcp $3:(mcommas (fst $2))) }
        | '(#' commas '#)'      {% ams (sLL $1 $> $ getRdrName (tupleTyCon Unboxed
                                                        (snd $2 + 1)))
                                       (mo $1:mc $3:(mcommas (fst $2))) }
        | '(' '->' ')'          {% ams (sLL $1 $> $ getRdrName funTyCon)
                                       [mop $1,mu AnnRarrow $2,mcp $3] }
        | '[' ']'               {% ams (sLL $1 $> $ listTyCon_RDR) [mos $1,mcs $2] }


oqtycon :: { Located RdrName }  -- An "ordinary" qualified tycon;
                                -- These can appear in export lists
      --  : qconid                        { loc_rdr_exp_to_type $1 }
        : qtycon                        { $1 }
        | '(' qtyconsym ')'             {% ams (sLL $1 $> (unLoc $2))
                                               [mop $1,mj AnnVal $2,mcp $3] }
        | '(' '~' ')'                   {% ams (sLL $1 $> $ eqTyCon_RDR)
                                               [mop $1,mj AnnVal $2,mcp $3] }

oqtycon_no_varcon :: { Located RdrName }  -- Type constructor which cannot be mistaken
                                          -- for variable constructor in export lists
                                          -- see Note [Type constructors in export list]
        :  qtycon            { $1 }
        | '(' QCONSYM ')'    {% let name = sL1 $2 $! mkQual tcClsName (getQCONSYM $2)
                                in ams (sLL $1 $> (unLoc name)) [mop $1,mj AnnVal name,mcp $3] }
        | '(' CONSYM ')'     {% let name = sL1 $2 $! mkUnqual tcClsName (getCONSYM $2)
                                in ams (sLL $1 $> (unLoc name)) [mop $1,mj AnnVal name,mcp $3] }
        | '(' ':' ')'        {% let name = sL1 $2 $! consDataCon_RDR
                                in ams (sLL $1 $> (unLoc name)) [mop $1,mj AnnVal name,mcp $3] }
        | '(' '~' ')'        {% ams (sLL $1 $> $ eqTyCon_RDR) [mop $1,mj AnnTilde $2,mcp $3] }

{- Note [Type constructors in export list]
~~~~~~~~~~~~~~~~~~~~~
Mixing type constructors and data constructors in export lists introduces
ambiguity in grammar: e.g. (*) may be both a type constructor and a function.

-XExplicitNamespaces allows to disambiguate by explicitly prefixing type
constructors with 'type' keyword.

This ambiguity causes reduce/reduce conflicts in parser, which are always
resolved in favour of data constructors. To get rid of conflicts we demand
that ambiguous type constructors (those, which are formed by the same
productions as variable constructors) are always prefixed with 'type' keyword.
Unambiguous type constructors may occur both with or without 'type' keyword.

Note that in the parser we still parse data constructors as type
constructors. As such, they still end up in the type constructor namespace
until after renaming when we resolve the proper namespace for each exported
child.
-}

qtyconop :: { Located RdrName } -- Qualified or unqualified
        : qtyconsym                     { $1 }
        | '`' qtycon '`'                {% ams (sLL $1 $> (unLoc $2))
                                               [mj AnnBackquote $1,mj AnnVal $2
                                               ,mj AnnBackquote $3] }


qtycon :: { Located RdrName }   -- Qualified or unqualified
        -- EF
        -- : qconid              { loc_rdr_exp_to_type $1 }
        -- EF
        -- Original:
        : QCONID            { sL1 $1 $! mkQual tcClsName (getQCONID $1) }
        | tycon             { $1 }


qtycondoc :: { LHsType GhcPs } -- Qualified or unqualified
        : qtycon            { sL1 $1                           (HsTyVar noExt NotPromoted $1)      }
        | qtycon docprev    { sLL $1 $> (HsDocTy noExt (sL1 $1 (HsTyVar noExt NotPromoted $1)) $2) }

tycon   :: { Located RdrName }  -- Unqualified
        : CONID                   { sL1 $1 $! mkUnqual tcClsName (getCONID $1) }


qtyconsym :: { Located RdrName }
      --  : qconsym            { loc_rdr_exp_to_type $1 }
        : QCONSYM            { sL1 $1 $! mkQual tcClsName (getQCONSYM $1) }
        | QVARSYM            { sL1 $1 $! mkQual tcClsName (getQVARSYM $1) }
        | tyconsym           { $1 }


-- Does not include "!", because that is used for strictness marks
--               or ".", because that separates the quantified type vars from the rest
tyconsym :: { Located RdrName }
        : CONSYM                { sL1 $1 $! mkUnqual tcClsName (getCONSYM $1) }
        | VARSYM                { sL1 $1 $! mkUnqual tcClsName (getVARSYM $1) }
        | ':'                   { sL1 $1 $! consDataCon_RDR }
        | '-'                   { sL1 $1 $! mkUnqual tcClsName (fsLit "-") }


-----------------------------------------------------------------------------
-- Operators

op      :: { Located RdrName }   -- used in infix decls
        : varop                 { $1 }
        | conop                 { $1 }

varop   :: { Located RdrName }
        : varsym                { $1 }
        | '`' varid '`'         {% ams (sLL $1 $> (unLoc $2))
                                       [mj AnnBackquote $1,mj AnnVal $2
                                       ,mj AnnBackquote $3] }

qop     :: { LHsExpr GhcPs }   -- used in sections
        : qvarop                { sL1 $1 $ HsVar noExt $1 }
        | qconop                { sL1 $1 $ HsVar noExt $1 }
        | hole_op               { $1 }

qopm    :: { LHsExpr GhcPs }   -- used in sections
        : qvaropm               { sL1 $1 $ HsVar noExt $1 }
        | qconop                { sL1 $1 $ HsVar noExt $1 }
        | hole_op               { $1 }

hole_op :: { LHsExpr GhcPs }   -- used in sections
hole_op : '`' '_' '`'           {% ams (sLL $1 $> $ EWildPat noExt)
                                       [mj AnnBackquote $1,mj AnnVal $2
                                       ,mj AnnBackquote $3] }

qvarop :: { Located RdrName }
        : qvarsym               { $1 }
        | '`' qvarid '`'        {% ams (sLL $1 $> (unLoc $2))
                                       [mj AnnBackquote $1,mj AnnVal $2
                                       ,mj AnnBackquote $3] }

qvaropm :: { Located RdrName }
        : qvarsym_no_minus      { $1 }
        | '`' qvarid '`'        {% ams (sLL $1 $> (unLoc $2))
                                       [mj AnnBackquote $1,mj AnnVal $2
                                       ,mj AnnBackquote $3] }

-----------------------------------------------------------------------------
-- Type variables

tyvar   :: { Located RdrName }
tyvar   : tyvarid               { $1 }

tyvarop :: { Located RdrName }
tyvarop : '`' tyvarid '`'       {% ams (sLL $1 $> (unLoc $2))
                                       [mj AnnBackquote $1,mj AnnVal $2
                                       ,mj AnnBackquote $3] }
        | '.'                   {% hintExplicitForall' (getLoc $1) }

tyvarid :: { Located RdrName }
        : VARID            { sL1 $1 $! mkUnqual tvName (getVARID $1) }
        | special_id       { sL1 $1 $! mkUnqual tvName (unLoc $1) }
        | 'unsafe'         { sL1 $1 $! mkUnqual tvName (fsLit "unsafe") }
        | 'safe'           { sL1 $1 $! mkUnqual tvName (fsLit "safe") }
        | 'interruptible'  { sL1 $1 $! mkUnqual tvName (fsLit "interruptible") }

-----------------------------------------------------------------------------
-- Variables

var     :: { Located RdrName }
        : varid                 { $1 }
        | '(' varsym ')'        {% ams (sLL $1 $> (unLoc $2))
                                       [mop $1,mj AnnVal $2,mcp $3] }

 -- Lexing type applications depends subtly on what characters can possibly
 -- end a qvar. Currently (June 2015), only $idchars and ")" can end a qvar.
 -- If you're changing this, please see Note [Lexing type applications] in
 -- Lexer.x.
qvar    :: { Located RdrName }
        : qvarid                { $1 }
        | '(' varsym ')'        {% ams (sLL $1 $> (unLoc $2))
                                       [mop $1,mj AnnVal $2,mcp $3] }
        | '(' qvarsym1 ')'      {% ams (sLL $1 $> (unLoc $2))
                                       [mop $1,mj AnnVal $2,mcp $3] }

-- We've inlined qvarsym here so that the decision about
-- whether it's a qvar or a var can be postponed until
-- *after* we see the close paren.

qvarid :: { Located RdrName }
        : varid               { $1 }
        | QVARID              { sL1 $1 $! mkQual varName (getQVARID $1) }

-- Note that 'role' and 'family' get lexed separately regardless of
-- the use of extensions. However, because they are listed here,
-- this is OK and they can be used as normal varids.
-- See Note [Lexing type pseudo-keywords] in Lexer.x
varid :: { Located RdrName }
        : VARID            { sL1 $1 $! mkUnqual varName (getVARID $1) }
        | special_id       { sL1 $1 $! mkUnqual varName (unLoc $1) }
        | 'unsafe'         { sL1 $1 $! mkUnqual varName (fsLit "unsafe") }
        | 'safe'           { sL1 $1 $! mkUnqual varName (fsLit "safe") }
        | 'interruptible'  { sL1 $1 $! mkUnqual varName (fsLit "interruptible")}
        | 'forall'         { sL1 $1 $! mkUnqual varName (fsLit "forall") }
        | 'family'         { sL1 $1 $! mkUnqual varName (fsLit "family") }
        | 'role'           { sL1 $1 $! mkUnqual varName (fsLit "role") }
{--EF
        : tyvarid          {$1 }
        | 'forall'         { sL1 $1 $! mkUnqual varName (fsLit "forall") }
        | 'family'         { sL1 $1 $! mkUnqual varName (fsLit "family") }
        | 'role'           { sL1 $1 $! mkUnqual varName (fsLit "role") }
--EF-}



qvarsym :: { Located RdrName }
        : varsym                { $1 }
        | qvarsym1              { $1 }

qvarsym_no_minus :: { Located RdrName }
        : varsym_no_minus       { $1 }
        | qvarsym1              { $1 }

qvarsym1 :: { Located RdrName }
qvarsym1 : QVARSYM              { sL1 $1 $ mkQual varName (getQVARSYM $1) }

varsym :: { Located RdrName }
        : varsym_no_minus       { $1 }
        | '-'                   { sL1 $1 $ mkUnqual varName (fsLit "-") }

varsym_no_minus :: { Located RdrName } -- varsym not including '-'
        : VARSYM               { sL1 $1 $ mkUnqual varName (getVARSYM $1) }
        | special_sym          { sL1 $1 $ mkUnqual varName (unLoc $1) }


-- These special_ids are treated as keywords in various places,
-- but as ordinary ids elsewhere.   'special_id' collects all these
-- except 'unsafe', 'interruptible', 'forall', 'family', 'role', 'stock', and
-- 'anyclass', whose treatment differs depending on context
special_id :: { Located FastString }
special_id
        : 'as'                  { sL1 $1 (fsLit "as") }
        | 'qualified'           { sL1 $1 (fsLit "qualified") }
        | 'hiding'              { sL1 $1 (fsLit "hiding") }
        | 'export'              { sL1 $1 (fsLit "export") }
        | 'label'               { sL1 $1 (fsLit "label")  }
        | 'dynamic'             { sL1 $1 (fsLit "dynamic") }
        | 'stdcall'             { sL1 $1 (fsLit "stdcall") }
        | 'ccall'               { sL1 $1 (fsLit "ccall") }
        | 'capi'                { sL1 $1 (fsLit "capi") }
        | 'prim'                { sL1 $1 (fsLit "prim") }
        | 'javascript'          { sL1 $1 (fsLit "javascript") }
        | 'group'               { sL1 $1 (fsLit "group") }
        | 'stock'               { sL1 $1 (fsLit "stock") }
        | 'anyclass'            { sL1 $1 (fsLit "anyclass") }
        | 'via'                 { sL1 $1 (fsLit "via") }
        | 'unit'                { sL1 $1 (fsLit "unit") }
        | 'dependency'          { sL1 $1 (fsLit "dependency") }
        | 'signature'           { sL1 $1 (fsLit "signature") }

special_sym :: { Located FastString }
special_sym : '!'       {% ams (sL1 $1 (fsLit "!")) [mj AnnBang $1] }
            | '.'       { sL1 $1 (fsLit ".") }
            | '*'       { sL1 $1 (fsLit (if isUnicode $1 then "★" else "*")) }

-----------------------------------------------------------------------------
-- Data constructors

qconid :: { Located RdrName }   -- Qualified or unqualified
        : conid              { $1 }
        | QCONID             { sL1 $1 $! mkQual dataName (getQCONID $1) }

conid   :: { Located RdrName }
        : CONID                { sL1 $1 $ mkUnqual dataName (getCONID $1) }

qconsym :: { Located RdrName }  -- Qualified or unqualified
        : consym               { $1 }
        | QCONSYM              { sL1 $1 $ mkQual dataName (getQCONSYM $1) }

consym :: { Located RdrName }
        : CONSYM              { sL1 $1 $ mkUnqual dataName (getCONSYM $1) }
        -- CONSYM begins with colon
        -- ':' means only list cons
        | ':'                { sL1 $1 $ consDataCon_RDR }

-----------------------------------------------------------------------------
-- Literals

literal :: { Located (HsLit GhcPs) }
        : CHAR              { sL1 $1 $ HsChar       (getCHARs $1) $ getCHAR $1 }
        | STRING            { sL1 $1 $ HsString     (getSTRINGs $1)
                                                    $ getSTRING $1 }
        | PRIMINTEGER       { sL1 $1 $ HsIntPrim    (getPRIMINTEGERs $1)
                                                    $ getPRIMINTEGER $1 }
        | PRIMWORD          { sL1 $1 $ HsWordPrim   (getPRIMWORDs $1)
                                                    $ getPRIMWORD $1 }
        | PRIMCHAR          { sL1 $1 $ HsCharPrim   (getPRIMCHARs $1)
                                                    $ getPRIMCHAR $1 }
        | PRIMSTRING        { sL1 $1 $ HsStringPrim (getPRIMSTRINGs $1)
                                                    $ getPRIMSTRING $1 }
        | PRIMFLOAT         { sL1 $1 $ HsFloatPrim  noExt $ getPRIMFLOAT $1 }
        | PRIMDOUBLE        { sL1 $1 $ HsDoublePrim noExt $ getPRIMDOUBLE $1 }

-----------------------------------------------------------------------------
-- Layout

close :: { () }
        : vccurly               { () } -- context popped in lexer.
        | error                 {% popContext }

-----------------------------------------------------------------------------
-- Miscellaneous (mostly renamings)

modid   :: { Located ModuleName }
        : CONID                 { sL1 $1 $ mkModuleNameFS (getCONID $1) }
        | QCONID                { sL1 $1 $ let (mod,c) = getQCONID $1 in
                                  mkModuleNameFS
                                   (mkFastString
                                     (unpackFS mod ++ '.':unpackFS c))
                                }

commas :: { ([SrcSpan],Int) }   -- One or more commas
        : commas ','             { ((fst $1)++[gl $2],snd $1 + 1) }
        | ','                    { ([gl $1],1) }

bars0 :: { ([SrcSpan],Int) }     -- Zero or more bars
        : bars                   { $1 }
        |                        { ([], 0) }

bars :: { ([SrcSpan],Int) }     -- One or more bars
        : bars '|'               { ((fst $1)++[gl $2],snd $1 + 1) }
        | '|'                    { ([gl $1],1) }

-----------------------------------------------------------------------------
-- Documentation comments

docnext :: { LHsDocString }
  : DOCNEXT {% return (sL1 $1 (mkHsDocString (getDOCNEXT $1))) }

docprev :: { LHsDocString }
  : DOCPREV {% return (sL1 $1 (mkHsDocString (getDOCPREV $1))) }

docnamed :: { Located (String, HsDocString) }
  : DOCNAMED {%
      let string = getDOCNAMED $1
          (name, rest) = break isSpace string
      in return (sL1 $1 (name, mkHsDocString rest)) }

docsection :: { Located (Int, HsDocString) }
  : DOCSECTION {% let (n, doc) = getDOCSECTION $1 in
        return (sL1 $1 (n, mkHsDocString doc)) }

moduleheader :: { Maybe LHsDocString }
        : DOCNEXT {% let string = getDOCNEXT $1 in
                     return (Just (sL1 $1 (mkHsDocString string))) }

maybe_docprev :: { Maybe LHsDocString }
        : docprev                       { Just $1 }
        | {- empty -}                   { Nothing }

maybe_docnext :: { Maybe LHsDocString }
        : docnext                       { Just $1 }
        | {- empty -}                   { Nothing }

{
happyError :: P a
happyError = srcParseFail

getVARID        (L _ (ITvarid    x)) = x
getCONID        (L _ (ITconid    x)) = x
getVARSYM       (L _ (ITvarsym   x)) = x
getCONSYM       (L _ (ITconsym   x)) = x
getQVARID       (L _ (ITqvarid   x)) = x
getQCONID       (L _ (ITqconid   x)) = x
getQVARSYM      (L _ (ITqvarsym  x)) = x
getQCONSYM      (L _ (ITqconsym  x)) = x
getIPDUPVARID   (L _ (ITdupipvarid   x)) = x
getLABELVARID   (L _ (ITlabelvarid   x)) = x
getCHAR         (L _ (ITchar   _ x)) = x
getSTRING       (L _ (ITstring _ x)) = x
getINTEGER      (L _ (ITinteger x))  = x
getRATIONAL     (L _ (ITrational x)) = x
getPRIMCHAR     (L _ (ITprimchar _ x)) = x
getPRIMSTRING   (L _ (ITprimstring _ x)) = x
getPRIMINTEGER  (L _ (ITprimint  _ x)) = x
getPRIMWORD     (L _ (ITprimword _ x)) = x
getPRIMFLOAT    (L _ (ITprimfloat x)) = x
getPRIMDOUBLE   (L _ (ITprimdouble x)) = x
getTH_ID_SPLICE (L _ (ITidEscape x)) = x
getTH_ID_TY_SPLICE (L _ (ITidTyEscape x)) = x
getINLINE       (L _ (ITinline_prag _ inl conl)) = (inl,conl)
getSPEC_INLINE  (L _ (ITspec_inline_prag _ True))  = (Inline,  FunLike)
getSPEC_INLINE  (L _ (ITspec_inline_prag _ False)) = (NoInline,FunLike)
getCOMPLETE_PRAGs (L _ (ITcomplete_prag x)) = x

getDOCNEXT (L _ (ITdocCommentNext x)) = x
getDOCPREV (L _ (ITdocCommentPrev x)) = x
getDOCNAMED (L _ (ITdocCommentNamed x)) = x
getDOCSECTION (L _ (ITdocSection n x)) = (n, x)

getINTEGERs     (L _ (ITinteger (IL src _ _))) = src
getCHARs        (L _ (ITchar       src _)) = src
getSTRINGs      (L _ (ITstring     src _)) = src
getPRIMCHARs    (L _ (ITprimchar   src _)) = src
getPRIMSTRINGs  (L _ (ITprimstring src _)) = src
getPRIMINTEGERs (L _ (ITprimint    src _)) = src
getPRIMWORDs    (L _ (ITprimword   src _)) = src

-- See Note [Pragma source text] in BasicTypes for the following
getINLINE_PRAGs       (L _ (ITinline_prag       src _ _)) = src
getSPEC_PRAGs         (L _ (ITspec_prag         src))     = src
getSPEC_INLINE_PRAGs  (L _ (ITspec_inline_prag  src _))   = src
getSOURCE_PRAGs       (L _ (ITsource_prag       src)) = src
getRULES_PRAGs        (L _ (ITrules_prag        src)) = src
getWARNING_PRAGs      (L _ (ITwarning_prag      src)) = src
getDEPRECATED_PRAGs   (L _ (ITdeprecated_prag   src)) = src
getSCC_PRAGs          (L _ (ITscc_prag          src)) = src
getGENERATED_PRAGs    (L _ (ITgenerated_prag    src)) = src
getCORE_PRAGs         (L _ (ITcore_prag         src)) = src
getUNPACK_PRAGs       (L _ (ITunpack_prag       src)) = src
getNOUNPACK_PRAGs     (L _ (ITnounpack_prag     src)) = src
getANN_PRAGs          (L _ (ITann_prag          src)) = src
getMINIMAL_PRAGs      (L _ (ITminimal_prag      src)) = src
getOVERLAPPABLE_PRAGs (L _ (IToverlappable_prag src)) = src
getOVERLAPPING_PRAGs  (L _ (IToverlapping_prag  src)) = src
getOVERLAPS_PRAGs     (L _ (IToverlaps_prag     src)) = src
getINCOHERENT_PRAGs   (L _ (ITincoherent_prag   src)) = src
getCTYPEs             (L _ (ITctype             src)) = src

getStringLiteral l = StringLiteral (getSTRINGs l) (getSTRING l)

isUnicode :: Located Token -> Bool
isUnicode (L _ (ITforall         iu)) = iu == UnicodeSyntax
isUnicode (L _ (ITdarrow         iu)) = iu == UnicodeSyntax
isUnicode (L _ (ITdcolon         iu)) = iu == UnicodeSyntax
isUnicode (L _ (ITlarrow         iu)) = iu == UnicodeSyntax
isUnicode (L _ (ITrarrow         iu)) = iu == UnicodeSyntax
isUnicode (L _ (ITlarrowtail     iu)) = iu == UnicodeSyntax
isUnicode (L _ (ITrarrowtail     iu)) = iu == UnicodeSyntax
isUnicode (L _ (ITLarrowtail     iu)) = iu == UnicodeSyntax
isUnicode (L _ (ITRarrowtail     iu)) = iu == UnicodeSyntax
isUnicode (L _ (IToparenbar      iu)) = iu == UnicodeSyntax
isUnicode (L _ (ITcparenbar      iu)) = iu == UnicodeSyntax
isUnicode (L _ (ITopenExpQuote _ iu)) = iu == UnicodeSyntax
isUnicode (L _ (ITcloseQuote     iu)) = iu == UnicodeSyntax
isUnicode (L _ (ITstar           iu)) = iu == UnicodeSyntax
isUnicode _                           = False

hasE :: Located Token -> Bool
hasE (L _ (ITopenExpQuote HasE _)) = True
hasE (L _ (ITopenTExpQuote HasE))  = True
hasE _                             = False

getSCC :: Located Token -> P FastString
getSCC lt = do let s = getSTRING lt
                   err = "Spaces are not allowed in SCCs"
               -- We probably actually want to be more restrictive than this
               if ' ' `elem` unpackFS s
                   then failSpanMsgP (getLoc lt) (text err)
                   else return s

-- Utilities for combining source spans
comb2 :: Located a -> Located b -> SrcSpan
comb2 a b = a `seq` b `seq` combineLocs a b

comb3 :: Located a -> Located b -> Located c -> SrcSpan
comb3 a b c = a `seq` b `seq` c `seq`
    combineSrcSpans (getLoc a) (combineSrcSpans (getLoc b) (getLoc c))

comb4 :: Located a -> Located b -> Located c -> Located d -> SrcSpan
comb4 a b c d = a `seq` b `seq` c `seq` d `seq`
    (combineSrcSpans (getLoc a) $ combineSrcSpans (getLoc b) $
                combineSrcSpans (getLoc c) (getLoc d))

-- strict constructor version:
{-# INLINE sL #-}
sL :: SrcSpan -> a -> Located a
sL span a = span `seq` a `seq` L span a
-- EF `seq` force evaluation to avoid laziness

-- See Note [Adding location info] for how these utility functions are used

-- replaced last 3 CPP macros in this file
{-# INLINE sL0 #-}
sL0 :: a -> Located a
sL0 = L noSrcSpan       -- #define L0   L noSrcSpan

{-# INLINE sL1 #-}
sL1 :: Located a -> b -> Located b
sL1 x = sL (getLoc x)   -- #define sL1   sL (getLoc $1)

{-# INLINE sLL #-}
sLL :: Located a -> Located b -> c -> Located c
sLL x y = sL (comb2 x y) -- #define LL   sL (comb2 $1 $>)

-- convert_nameSpace :: Located RdrName -> Located RdrName
-- convert_nameSpace (L sp (Unqual occname)) = L sp (mkUnqual tvName fs)
--   where fs = occNameFS occ_name


-- -- converting LhsExpr to LhsType
-- lhsExpr_to_lhsType :: LHsExpr GhcPs -> LHsType GhcPs
-- lhsExpr_to_lhsType (L sp (HsVar _ t))    = L sp (HsTyVar noExt NotPromoted $ loc_rdr_exp_to_type t)
-- lhsExpr_to_lhsType (L sp (TArrow _))   = L sp (HsTyVar noExt NotPromoted $ (L sp $ getRdrName funTyCon))
-- lhsExpr_to_lhsType (L sp (TTwiddle _)) = L sp (HsTyVar noExt NotPromoted $ (L sp eqTyCon_RDR))

-- -- converts namespace for a given faststring
-- convertNS :: FastString -> NameSpace -> NameSpace
-- convertNS fs ns
--   | (isVarNameSpace ns) && (isLexVarSym fs)    = tcClsName
--   | (isVarNameSpace ns) && (fsLit "-" == fs)   = tcClsName
--   | isDataConNameSpace ns                      = tcClsName
--   | isVarNameSpace ns                          = tvName
--   -- | (isTvNameSpace ns) && (! fs `elem` fs_lis) = varName
--   --   where fs_lis = [fsLit "forall", fsLit "family", fsLit "role"]
--
-- loc_rdr_exp_to_type :: Located RdrName -> Located RdrName
-- loc_rdr_exp_to_type (L sp (Unqual occ_name)) = L sp (mkUnqual ns fs)
--   where fs = occNameFS occ_name
--         ns  = convertNS fs (occNameSpace occ_name)
--
-- loc_rdr_exp_to_type (L sp (Qual mn occ_name)) = L sp (mkQual ns (mfs, fs))
--   where fs = occNameFS occ_name
--         mfs = moduleNameFS mn                        -- mfs: module fast string
--         ns = convertNS fs (occNameSpace occ_name)
-- loc_rdr_exp_to_type c@(L sp (Exact name))
--   | (nameUnique name == nilDataConKey)  = L sp listTyCon_RDR
--   | (nameUnique name == consDataConKey) = c
--   | otherwise = case (wiredInNameTyThing_maybe name) of
--                   Just t@(AnId id) -> case (isDataConId_maybe id) of
--                                         Just datacon -> L sp (getRdrName $ dataConTyCon datacon)
--                                         Nothing -> error "The impossible has happened again!"
--                   Nothing -> error "The impossible has happened!"
-- loc_rdr_exp_to_type _ = error "Trying to run loc_rdr_exp_to_type on unhandled case!"

-- Old version for '(' ':' ')' case, now handled above
--loc_rdr_exp_to_type c@(L _ (Exact _)) = c

-- helper function for extracting value from HsTerm
getTerms :: HsTerm -> [LHsTerms]
getTerms (HsBarTerms2 ts) = ts
getTerms _                = error "An error occurred while constructing bar_terms2"


-- ############ Check function for `exp`: ############
check_exp :: LHsTerms -> (Maybe (P (LHsExpr GhcPs)), LHsTerms)
check_exp ((L sp (HsArrAppTerm infixe e HsFirstOrderApp True)) : rest)    = (Just (return (L sp $ HsArrApp noExt infixe e     -- infixexp '-<' exp
                                                                                        HsFirstOrderApp True)), rest)
check_exp ((L sp (HsArrAppTerm aat1 aat2 HsHigherOrderApp False)) : rest) = (Just (return (L sp $ HsArrApp noExt aat1 aat2    -- infixexp '>>-' exp
                                                                                        HsHigherOrderApp False)), rest)
check_exp ((L sp (HsArrAppTerm aat1 aat2 HsHigherOrderApp True)) : rest)  = (Just (return (L sp $ HsArrApp noExt aat1 aat2    -- infixexp '-<<' exp
                                                                                        HsHigherOrderApp True)), rest)
check_exp ((L sp (HsArrAppTerm aat1 aat2 HsHigherOrderApp False)) : rest) = (Just (return (L sp $ HsArrApp noExt aat1 aat2    -- infixexp '>>-' exp
                                                                                        HsHigherOrderApp False)), rest)
check_exp t
  | x@(Just infixexp, rest) <- check_infixexp t
  = case rest of
      ((L sp (HsDconTerm tp)) : rest') -> let result = do { infixexp' <- infixexp
                                                         ; return (L sp $ ExprWithTySig (mkLHsSigWcType tp) infixexp') }
                                         in (Just result, rest')                                                              -- infixexp '::' sigtype
      _                                -> x

check_exp rest = (Nothing, rest)


-- ############ Check function for `infixexp`: ############
check_infixexp :: LHsTerms -> (Maybe (P (LHsExpr GhcPs)), LHsTerms)
check_infixexp t
  | (Just exp10, rest) <- check_exp10 t --(Just exp10, rest)
  = go exp10 [] rest
      where
        go :: P (LHsExpr GhcPs)
           -> [P (LHsExpr GhcPs)]
           -> LHsTerms
           -> (Maybe (P (LHsExpr GhcPs)), LHsTerms)
        go firstExpr acc rest
          | (Just qop, rest') <- check_qop rest
          = case (check_exp10 rest') of
              (Just exp10, rest'') -> go firstExpr (exp10 : qop : acc) rest''
              otherwise            -> (Nothing, rest')
          | otherwise
          = (Just (build_opapps firstExpr (reverse acc)), rest)
        build_opapps :: P (LHsExpr GhcPs) -> [P (LHsExpr GhcPs)] -> P (LHsExpr GhcPs)
        build_opapps firstExpr [] = firstExpr
        build_opapps firstExpr (exp10 : qop : rest) = do { rest' <- build_opapps firstExpr rest
                                                         ; exp10' <- exp10
                                                         ; qop' <- qop
                                                         ; return (sLL rest' exp10' $ (OpApp noExt rest' qop' exp10')) }

check_infixexp rest = (Nothing, rest)

-- ############ Check function for `infixexp_top`: ############
check_infixexp_top :: LHsTerms -> (Maybe (P (LHsExpr GhcPs)), LHsTerms)
check_infixexp_top t
  | (Just exp10_top, rest) <- check_exp10_top t
  = go exp10_top [] rest
      where
        go :: P (LHsExpr GhcPs)
           -> [P (LHsExpr GhcPs)]
           -> LHsTerms
           -> (Maybe (P (LHsExpr GhcPs)), LHsTerms)
        go firstExpr acc rest
          | (Just qop, rest') <- check_qop rest
          = case (check_exp10_top rest') of
              (Just exp10_top, rest'') -> go firstExpr (exp10_top : qop : acc) rest''
              otherwise                -> (Nothing, rest')
          | otherwise
          = (Just (build_opapps firstExpr (reverse acc)), rest)
        build_opapps :: P (LHsExpr GhcPs) -> [P (LHsExpr GhcPs)] -> P (LHsExpr GhcPs)
        build_opapps firstExpr [] = firstExpr
        build_opapps firstExpr (exp10_top : qop : rest) = do { rest' <- build_opapps firstExpr rest
                                                             ; exp10_top' <- exp10_top
                                                             ; qop' <- qop
                                                             ; return (sLL rest' exp10_top' $ (OpApp noExt rest' qop' exp10_top')) }

check_infixexp_top rest = (Nothing, rest)

-- ############ Check function for `qop`: ############
check_qop :: LHsTerms -> (Maybe (P (LHsExpr GhcPs)), LHsTerms)
check_qop ((L sp (HsBacktickTerm t)) : rest)
  | (Just qvarid, []) <- check_qvarid t = let result = do { qvarid' <- qvarid
                                                           ; return (L sp $ HsVar noExt (L sp $ (unLoc qvarid'))) }
                                           in (Just result, rest)             -- '`' qvarid '`'
  | (Just qconid, []) <- check_qconid t = let result = do { qconid' <- qconid
                                                           ; return (L sp $ HsVar noExt (L sp $ (unLoc qconid'))) }
                                           in (Just result, rest)             -- '`' qconid '`'
check_qop t
 | (Just qvarsym, rest) <- check_qvarsym t = let result = do { qvarsym' <- qvarsym                             -- qvarsym
                                                             ; return (sL1 qvarsym' $ HsVar noExt qvarsym') }
                                             in (Just result, rest)
 | (Just qconsym, rest) <- check_qconsym t = let result = do { qconsym' <- qconsym                             -- qconsym
                                                             ; return (sL1 qconsym' $ HsVar noExt qconsym') }
                                             in (Just result, rest)
check_qop ((L sp (HsBacktickTerm (L _ HsUnderscoreTerm : []))) : rest) = (Just (return (L sp $ EWildPat noExt)), rest)   -- '`' '_' '`'
check_qop rest = (Nothing, rest)

-- ############ Check function for `qopm`: ############
check_qopm :: LHsTerms -> (Maybe (P (LHsExpr GhcPs)), LHsTerms)
-- Same as qop, but we filter out minus signs here:
check_qopm rest@((L _ (HsGenName (MinusSignData _))) : _) = (Nothing, rest)
check_qopm ((L sp (HsBacktickTerm t)) : rest)
  | (Just qvarid, []) <- check_qvarid t = let result = do { qvarid' <- qvarid                    -- '`' qvarid '`'
                                                          ; return (L sp $ HsVar noExt (L sp $ (unLoc qvarid'))) }
                                          in (Just result, rest)
  | (Just qconid, []) <- check_qconid t = let result = do { qconid' <- qconid                    -- '`' qconid '`'
                                                          ; return (L sp $ HsVar noExt (L sp $ (unLoc qconid'))) }
                                          in (Just result, rest)
check_qopm t
  | (Just qvarsym, rest) <- check_qvarsym t = let result = do { qvarsym' <- qvarsym                             -- qvarsym
                                                              ; return (sL1 qvarsym' $ HsVar noExt qvarsym') }
                                              in (Just result, rest)
  | (Just qconsym, rest) <- check_qconsym t = let result = do { qconsym' <- qconsym                             -- qconsym
                                                              ; return (sL1 qconsym' $ HsVar noExt qconsym') }
                                              in (Just result, rest)

check_qopm ((L sp (HsBacktickTerm (L _ HsUnderscoreTerm : []))) : rest) = (Just (return (L sp $ EWildPat noExt)), rest)   -- '`' '_' '`'
check_qopm rest = (Nothing, rest)

-- ############ Check function for `exp10`: ############
check_exp10 :: LHsTerms -> (Maybe (P (LHsExpr GhcPs)), LHsTerms)
check_exp10 t
  | x@(Just _, _) <- check_exp10_top t = x                     -- exp10_top
check_exp10 ((L sp (HsSccAnnTerm sccann1 sccann2 e)) : rest) = (Just (return (L sp $ HsSCC noExt sccann1 sccann2 e)), rest)              -- scc_annot exp
check_exp10 rest = (Nothing, rest)

-- ############ Check function for `exp10_top`: ############
check_exp10_top :: LHsTerms -> (Maybe (P (LHsExpr GhcPs)), LHsTerms)
check_exp10_top ((L sp (HsGenName (MinusSignData _))) : rest)                        -- '-' fexp
  | (Just fexp, rest') <- check_fexp rest = let result = do { fexp' <- fexp
                                                            ; return (L sp $ NegApp noExt fexp' noSyntaxExpr)}
                                            in (Just result, rest')

check_exp10_top ((L sp (HsTickPragmaTerm tpt1 tpt2 tpt3 tpt4)) : rest) = (Just (return (L sp $ HsTickPragma noExt tpt1 tpt2 tpt3 tpt4)), rest)  -- hpc_annot exp
check_exp10_top ((L sp (HsCoreAnnTerm cprags strlit e)) : rest)        = (Just (return (L sp $ HsCoreAnn noExt cprags strlit e)), rest)  -- '{-# CORE' STRING '#-}' exp
check_exp10_top t                                   -- fexp
  | x@(Just _, _) <- check_fexp t = x
check_exp10_top rest = (Nothing, rest)

-- ############ Check function for `fexp`: ############
check_fexp :: LHsTerms -> (Maybe (P (LHsExpr GhcPs)), LHsTerms)
check_fexp t
  | (Just aexp, rest@(x : xs)) <- check_aexp t
  = fexp_go aexp [] rest
check_fexp ((L sp (HsStaticTerm saexp)) : rest)
  = fexp_go processed_saexp [] rest
      where processed_saexp = return (L sp $ HsStatic noExt saexp)
check_fexp rest = (Nothing, rest)

-- helper functions:
data HsArg = ValArg (P (LHsExpr GhcPs)) | TAppArg (P (LHsType GhcPs))
fexp_go :: P (LHsExpr GhcPs)
        -> [HsArg]
        -> LHsTerms
        -> (Maybe (P (LHsExpr GhcPs)), LHsTerms)
fexp_go firstExpr acc rest
  | ((L sp' (HsTypeAppTerm atp)) : rest') <- rest
  = fexp_go firstExpr (TAppArg (return atp) : acc) rest'
  | (Just aexp', rest') <- check_aexp rest
  = fexp_go firstExpr (ValArg aexp' : acc) rest'
  | otherwise = (Just (build_apps firstExpr (reverse acc)), rest)

build_apps :: P (LHsExpr GhcPs) -> [HsArg] -> P (LHsExpr GhcPs)
build_apps firstExpr [] = firstExpr
build_apps firstExpr (ValArg  e : es) = do { es' <- build_apps firstExpr es
                                           ; e' <- e
                                           ; checkBlockArguments es'
                                           ; return (sLL es' e' $ HsApp noExt es' e') }
build_apps firstExpr (TAppArg e : es) = do { es' <- build_apps firstExpr es
                                           ; e' <- e
                                           ; checkBlockArguments es'
                                           ; return (sLL es' e' $ HsAppType (mkHsWildCardBndrs e') es') }

-- ############ Check function for `aexp`: ############
check_aexp :: LHsTerms -> (Maybe (P (LHsExpr GhcPs)), LHsTerms)   -- Returns the produced rule followed by the rest of the unconsumed tokens
check_aexp ((L sp (HsAtTerm qvr axp)) : rest) = (Just (return (L sp $ EAsPat noExt qvr axp)), rest)

check_aexp  ((L sp (HsGenName (TwiddleData td))) : rest)                -- '~' aexp
  | (Just aexp, rest') <- check_aexp rest
  = let result = do { aexp' <- aexp
                    ; return (L sp $ ELazyPat noExt aexp')}
    in (Just result, rest')

check_aexp ((L sp (HsApatsArrowTerm apt apts e)) : rest)  -- '\\' apat apats '->' exp
  = (Just (return (L sp $ HsLam noExt (mkMatchGroup FromSource
                  [L sp $ Match { m_ext = noExt
                                     , m_ctxt = LambdaExpr
                                     , m_pats = apt:apts
                                     , m_grhss = unguardedGRHSs e }]))), rest)
check_aexp ((L sp (HsLetTerm bds e)) : rest) = (Just (return (L sp $ HsLet noExt (snd $ unLoc bds) e)), rest)  -- 'let' binds 'in' exp
check_aexp ((L sp (HsLcaseTerm altslis)) : rest)                                              -- '\\' 'lcase' altslist
  = (Just (return (L sp $ HsLamCase noExt (mkMatchGroup FromSource (snd $ unLoc altslis)))), rest)
check_aexp ((L sp (HsIfThenElseTerm itet1 itet2 itet3)) : rest) = (Just (return (L sp $ mkHsIf itet1 itet2 itet3)), rest)               -- 'if' exp optSemi 'then' exp optSemi 'else' exp
check_aexp ((L sp (HsOnlyIfTerm ifgdpts)) : rest)     = (Just (return (L sp $ HsMultiIf noExt (reverse $ snd $ unLoc ifgdpts))), rest)  -- 'if' ifgdpats
check_aexp ((L sp (HsCaseTerm cexp caltlist)) : rest) = (Just (return (L sp $ HsCase noExt cexp (mkMatchGroup                   -- 'case' exp 'of' altslist
                                                                                      FromSource (snd $ unLoc caltlist)))), rest)
check_aexp ((L sp (HsDoTerm stmtlis)) : rest)  = (Just (return (L sp (mkHsDo DoExpr (snd $ unLoc stmtlis)))), rest)                     -- 'do' stmtlist
check_aexp ((L sp (HsMdoTerm stmtlis)) : rest) = (Just (return (L sp (mkHsDo MDoExpr (snd $ unLoc stmtlis)))), rest)                    -- 'mdo' stmtlist
check_aexp ((L sp (HsProcTerm ptaexp ptexp)) : rest) = (Just (checkPattern empty ptaexp >>= \ p ->
                                                                     checkCommand ptexp >>= \ cmd ->
                                                                     return (L sp $ HsProc noExt p (L sp $ HsCmdTop noExt cmd)))
                                                              , rest)
check_aexp t
  | x@(Just _, _) <- check_aexp1 t = x   -- aexp1
check_aexp rest = (Nothing, rest)                                            -- otherwise return Nothing


-- ############ Check function for `aexp1`: ############
check_aexp1 :: LHsTerms -> (Maybe (P (LHsExpr GhcPs)), LHsTerms)
check_aexp1 t
  | (Just aexp2, rest) <- check_aexp2 t
  = go aexp2 [] rest
      where
        go :: P (LHsExpr GhcPs)
           -> [Located ([AddAnn],([LHsRecField GhcPs (LHsExpr GhcPs)], Bool))]
           -> LHsTerms
           -> (Maybe (P (LHsExpr GhcPs)), LHsTerms)
        go aexp2 acc rest
          | ((L sp (HsFBindsTerm fbinds)) : rest') <- rest
          = go aexp2 ((L sp fbinds) : acc) rest'
          | otherwise
          = (Just (build aexp2 (reverse acc)), rest)
        build aexp2 [] = aexp2
        build aexp2 (loc_fbinds@(L sp fbinds) : rest) = do { aexp1 <- build aexp2 rest
                                         ; r <- mkRecConstrOrUpdate aexp1 sp
                                                                          (snd fbinds)
                                         ; checkRecordSyntax (sLL aexp1 loc_fbinds r) }

check_aexp1 rest = (Nothing, rest)

-- ############ Check function for `aexp2`: ############
check_aexp2 :: LHsTerms -> (Maybe (P (LHsExpr GhcPs)), LHsTerms)
check_aexp2 t
  | (Just qvar, rest) <- check_qvar t = let result = do { qvar' <- qvar
                                                        ; return (L (getLoc qvar') $ HsVar noExt $! qvar')}
                                        in (Just result, rest)  -- qvar
  | (Just qcon, rest) <- check_qcon t = let result = do { qcon' <- qcon
                                                        ; return (L (getLoc qcon') $ HsVar noExt $! qcon')}
                                        in (Just result, rest)  -- qcon

check_aexp2 ((L sp (HsGenName (IPDupVaridData d))) : rest) = (Just (return (L sp (HsIPVar noExt $! HsIPName d))), rest)        -- ipvar
check_aexp2 ((L sp (HsGenName (LabelVaridData d))) : rest) = (Just (return (L sp (HsOverLabel noExt Nothing $! d))), rest)       -- overloaded_label
check_aexp2 ((L sp (HsGenName (LiteralData d))) : rest)    = (Just (return (L sp (HsLit noExt $! d))), rest)          -- literal
check_aexp2 ((L sp (HsGenName (IntegerData d))) : rest)    = (Just (return (L sp (HsOverLit noExt $! d))), rest)      -- INTEGER
check_aexp2 ((L sp (HsGenName (RationalData d))) : rest)   = (Just (return (L sp (HsOverLit noExt $! d))), rest)
check_aexp2 ((L sp (HsGenName (ThIdSpliceData d))) : rest) = (Just (return (L sp $ mkHsSpliceE HasDollar      -- TH_ID_SPLICE in splice_exp
                                                                                   (L sp $ HsVar noExt (L sp (mkUnqual varName d))))), rest)
check_aexp2 ((L sp (HsGenName (ThIdTySpliceData d))) : rest) = (Just (return (L sp $ mkHsSpliceTE HasDollar         -- TH_ID_TY_SPLICE in splice_exp
                                                                                     (L sp $ HsVar noExt (L sp (mkUnqual varName d))))), rest)

check_aexp2 ((L sp (HsGenName (QuasiquoteData d))) : rest)        = (Just (return (L sp $ HsSpliceE noExt d)), rest)      -- quasiquote
check_aexp2 ((L sp (HsBracketTerm (L _ ((HsListTerm l)) : []))) : rest) = (Just (return (L sp (snd l))), rest) -- '[' list ']'
check_aexp2 ((L sp HsUnderscoreTerm) : rest)                            = (Just (return (L sp $ EWildPat noExt)), rest)               -- '_'
check_aexp2 ((L sp (HsDollarParenTerm dpt)) : rest)                     = (Just (return (L sp $ mkHsSpliceE HasParens dpt)), rest)    -- '$(' exp ')' in splice_exp
check_aexp2 ((L sp (HsDoubleDollarParenTerm ddpt)) : rest)              = (Just (return (L sp $ mkHsSpliceTE HasParens ddpt)), rest)  -- '$$(' exp ')' in splice_exp

check_aexp2 ((L sp (HsTupParTerm tpt)) : rest)  -- Tupley stuff
  | (Just tup_exprs, rest) <- check_tup_exprs tpt = let result = do { tup_exprs' <- tup_exprs
                                                                    ; e <- mkSumOrTuple Boxed sp (snd tup_exprs')
                                                                    ; return (L sp e) }
                                                    in (Just result, rest)

check_aexp2 ((L sp HsSimplequoteTerm) : rest)
  | (Just qvarRdrn, rest') <- check_qvar rest = let result = do { qvarRdrn' <- qvarRdrn
                                                                ; return (L sp $ HsBracket noExt (VarBr noExt True (unLoc qvarRdrn'))) }
                                                in (Just result, rest')  -- SIMPLEQUOTE  qvar
  | (Just qconRdrn, rest') <- check_qcon rest = let result = do { qconRdrn' <- qconRdrn
                                                                ; return (L sp $ HsBracket noExt (VarBr noExt True (unLoc qconRdrn'))) }
                                                in (Just result, rest')  -- SIMPLEQUOTE  qcon


-- check_aexp2 ((L sp (HsThTyQuoteTerm (L _ (HsGenName (SpecialSymData specialSym))))) : rest)
--   = if specialSym == fsLit "." then (Just (return (L sp $ HsBracket noExt (VarBr noExt False (hintExplicitForall' sp)))), rest) else (error "don't know")

check_aexp2 ((L sp HsThTyQuoteTerm) : rest)
  | ((L _ (HsGenName (VaridData vd))) : rest')         <- rest = (Just (return (L sp $ HsBracket noExt (VarBr noExt False (mkUnqual tvName vd)))), rest')     -- VARID in tyvarid
  | ((L _ (HsGenName (SpecialIdData sd))) : rest')     <- rest = (Just (return (L sp $ HsBracket noExt (VarBr noExt False (mkUnqual tvName sd)))), rest')     -- special_id in tyvarid
  | ((L _ (HsGenName (UnsafeData ud))) : rest')        <- rest = (Just (return (L sp $ HsBracket noExt (VarBr noExt False (mkUnqual tvName ud)))), rest')     -- 'unsafe' in tyvarid
  | ((L _ (HsGenName (SafeData safed))) : rest')       <- rest = (Just (return (L sp $ HsBracket noExt (VarBr noExt False (mkUnqual tvName safed)))), rest')  -- 'safe' in tyvarid
  | ((L _ (HsGenName (InterruptibleData id))) : rest') <- rest = (Just (return (L sp $ HsBracket noExt (VarBr noExt False (mkUnqual tvName id)))), rest')     -- 'interruptible' in tyvarid
  | ((L _ (HsGenName (QConidData qd))) : rest')        <- rest = (Just (return (L sp $ HsBracket noExt (VarBr noExt False (mkQual tcClsName qd)))), rest')    -- 'QCONID' in qtycon (oqtycon, ntgtycon, gtycon)
  | ((L _ (HsGenName (ConidData cd))) : rest')         <- rest = (Just (return (L sp $ HsBracket noExt (VarBr noExt False (mkUnqual tcClsName cd)))), rest')  -- 'CONID' in tycon (qtycon, oqtycon, ntgtycon, gtycon)
  | ((L _ (HsParTerm [])) : rest')                     <- rest = (Just (return (L sp $ HsBracket noExt (VarBr noExt False (getRdrName unitTyCon)))), rest')          -- '(' ')' in gtycon
  | ((L _ (HsParTerm ((L _ (HsGenName (ArrowData _))) : []))) : rest')      <- rest = (Just (return (L sp $ HsBracket noExt (VarBr noExt False (getRdrName funTyCon)))), rest')     -- '(' '->' ')' in ntgtycon (gtycon)
  | ((L _ (HsParTerm ((L _ (HsGenName (TwiddleData _))) : []))) : rest')    <- rest = (Just (return (L sp $ HsBracket noExt (VarBr noExt False eqTyCon_RDR))), rest')               -- '(' '~' ')' in ntgtycon (gtycon)
  | ((L _ (HsParTerm ((L _ (HsGenName (QConsymData qcd))) : []))) : rest')  <- rest = (Just (return (L sp $ HsBracket noExt (VarBr noExt False (mkQual tcClsName qcd)))), rest')    -- QCONSYM in '(' qtyconsym')', oqtycon, ntgtycon
  | ((L _ (HsParTerm ((L _ (HsGenName (QVarsymData qvd))) : []))) : rest')  <- rest = (Just (return (L sp $ HsBracket noExt (VarBr noExt False (mkQual tcClsName qvd)))), rest')    -- QVARSYM in '(' qtyconsym')', oqtycon, ntgtycon
  | ((L _ (HsParTerm ((L _ (HsGenName (ConsymData csd))) : []))) : rest')   <- rest = (Just (return (L sp $ HsBracket noExt (VarBr noExt False (mkUnqual tcClsName csd)))), rest')    -- CONSYM in tyconsym, '(' qtyconsym')', oqtycon, ntgtycon
  | ((L _ (HsParTerm ((L _ (HsGenName (VarsymData vsd))) : []))) : rest')   <- rest = (Just (return (L sp $ HsBracket noExt (VarBr noExt False (mkUnqual tcClsName vsd)))), rest')    -- VARSYM in tyconsym, '(' qtyconsym')', oqtycon, ntgtycon
  | ((L _ (HsParTerm ((L _ (HsGenName (ColonData _))) : []))) : rest')      <- rest = (Just (return (L sp $ HsBracket noExt (VarBr noExt False consDataCon_RDR))), rest')           -- ':' in tyconsym, '(' qtyconsym')', oqtycon, ntgtycon
  | ((L _ (HsParTerm ((L _ (HsGenName (MinusSignData md))) : []))) : rest') <- rest = (Just (return (L sp $ HsBracket noExt (VarBr noExt False (mkUnqual tcClsName md)))), rest')     -- '-' in tyconsym, '(' qtyconsym')', oqtycon, ntgtycon
  | ((L _ (HsBoxParTerm [])) : rest')                                       <- rest = (Just (return (L sp $ HsBracket noExt (VarBr noExt False (getRdrName unboxedUnitTyCon)))), rest')  -- '(#' '#)' in tyvarid
  | ((L _ (HsBracketTerm [])) : rest')                                      <- rest = (Just (return (L sp $ HsBracket noExt (VarBr noExt False listTyCon_RDR))), rest')                  -- '[' ']' in ntgtycon
  | ((L _ (HsBoxTupParTerm ((L _ (HsTupCommas c)) : []))) : rest')          <- rest = (Just (return (L sp $ HsBracket noExt (VarBr noExt False (getRdrName (tupleTyCon Unboxed (snd c + 1)))))), rest') -- '(' commas ')'  in ntgtycon
  | ((L _ (HsTupParTerm ((L _ (HsTupCommas c)) : []))) : rest')             <- rest = (Just (return (L sp $ HsBracket noExt (VarBr noExt False (getRdrName (tupleTyCon Boxed (snd c + 1)))))), rest')   -- '(#' commas '#)' in ntgtycon

check_aexp2 ((L sp (HsExpQuoteTerm eqt)) : rest)       = (Just (return (L sp $ HsBracket noExt (ExpBr noExt eqt))), rest)             -- '[|' exp '|]'
check_aexp2 ((L sp (HsTExpQuoteTerm teqt)) : rest)     = (Just (return (L sp $ HsBracket noExt (TExpBr noExt teqt))), rest)           -- '[||' exp '||]'
check_aexp2 ((L sp (HsTypQuoteTerm tqt)) : rest)       = (Just (return (L sp $ HsBracket noExt (TypBr noExt tqt))), rest)             -- '[t|' ctype '|]'
check_aexp2 ((L sp (HsPatQuoteTerm pqt)) : rest)       = let result = checkPattern empty pqt >>= \p -> return (L sp $ HsBracket noExt (PatBr noExt p))                   -- '[p|' infixexp '|]'
                                                         in (Just result, rest)
check_aexp2 ((L sp (HsDecQuoteTerm dqt)) : rest)       = (Just (return (L sp $ HsBracket noExt (DecBrL noExt (snd dqt)))), rest)      -- '[d|' cvtopbody '|]'
check_aexp2 ((L sp (HsParenBarTerm pbt1 pbt2)) : rest) = (Just (return (L sp $ HsArrForm noExt pbt1 Nothing (reverse pbt2))), rest)   -- '(|' aexp2 cmdargs '|)'

check_aexp2 rest = (Nothing, rest)

-- ######## Check function for texp: ########
check_texp :: LHsTerms -> (Maybe (P (LHsExpr GhcPs)), LHsTerms)
check_texp t
  | x@(Just infixexp, rest) <- check_infixexp t
  = case check_qop rest of
      (Just qop, rest') -> let result = do { infixexp' <- infixexp
                                           ; qop' <- qop
                                           ; return (sLL infixexp' qop' $ SectionL noExt infixexp' qop') }
                           in (Just result, rest)
      -- We accept just infixexp here because infixexp is a valid production of exp, which is an explicit production for texp:
      _                 -> x

  | (Just qopm, rest) <- check_qopm t
  = case check_infixexp rest of
      (Just infixexp, rest') -> let result = do { qopm' <- qopm
                                                ; infixexp' <- infixexp
                                                ; return (sLL qopm' infixexp' $ SectionL noExt qopm' infixexp') }
                                in (Just result, rest)
      _                      -> (Nothing, t)
  | (Just exp, rest) <- check_exp t
  = case rest of
      ((L sp (HsGenName (ArrowData _))) : rest') -> case check_texp rest' of
        (Just texp, rest'') -> let result = do { exp' <- exp
                                               ; texp' <- texp
                                               ; return (sLL exp' texp' $ EViewPat noExt exp' texp') }
                               in (Just result, rest'')
        _                   -> (Just exp, rest)
      _                   -> (Just exp, rest)


-- ######## Check function for qcon: ########
check_qcon :: LHsTerms -> (Maybe (P (Located RdrName)), LHsTerms)
check_qcon t
  | x@(Just _, _) <- check_qconid t = x                                                -- qconid
check_qcon ((L sp (HsParTerm t)) : rest)
  | (Just qconsym, []) <- check_qconsym t = let result = do { qconsym' <- qconsym
                                                            ; return (L sp $ unLoc qconsym') }
                                            in (Just result, rest)                                       -- '(' qconsym ')'
check_qcon ((L sp (HsParTerm [])) : rest)    = (Just (return (L sp $ nameRdrName (dataConName unitDataCon))), rest)           -- '(' ')' in qcon
check_qcon ((L sp (HsBoxParTerm [])) : rest) = (Just (return (L sp $ nameRdrName (dataConName unboxedUnitDataCon))), rest)    -- '(#' '#)' in qcon
check_qcon ((L sp (HsTupParTerm ((L _ (HsTupCommas tc)) : []))) : rest)
    = (Just (return (L sp $ nameRdrName (dataConName $ tupleDataCon Boxed (snd tc + 1)))), rest)                                --'(' commas ')' in qcon
check_qcon ((L sp (HsBoxTupParTerm ((L _ (HsTupCommas tc)) : []))) : rest)
    = (Just (return (L sp $ nameRdrName (dataConName $ tupleDataCon Unboxed (snd tc + 1)))), rest)                         -- '(#' commas '#)' in qcon
check_qcon ((L sp (HsBracketTerm [])) : rest)
    = (Just (return (L sp $ nameRdrName (dataConName nilDataCon))), rest)                                       -- '[' ']' in qcon
check_qcon rest = (Nothing, rest)

-- ######## Check function for qvar: ########
check_qvar :: LHsTerms -> (Maybe (P (Located RdrName)), LHsTerms)
check_qvar t
  | x@(Just _, _) <- check_qvarid t = x                                 -- qvarid
check_qvar ((L sp (HsParTerm t)) : rest)
  | (Just qvarsym, []) <- check_qvarsym t = let result = do { qvarsym' <- qvarsym
                                                            ; return (L sp $ unLoc qvarsym') }
                                            in (Just result, rest)
check_qvar rest = (Nothing, rest)

-- ######## Check function for qconid: ########
check_qconid :: LHsTerms -> (Maybe (P (Located RdrName)), LHsTerms)
check_qconid ((L sp (HsGenName (ConidData d))) : rest)       = (Just (return (L sp $ mkUnqual dataName d)), rest)       -- CONID
check_qconid ((L sp (HsGenName (QConidData d))) : rest)      = (Just (return (L sp $ mkQual dataName d)), rest)         -- QCONID
check_qconid rest = (Nothing, rest)

-- ######## Check function for qvarid: ########
check_qvarid :: LHsTerms -> (Maybe (P (Located RdrName)), LHsTerms)
check_qvarid ((L sp (HsGenName (VaridData d))) : rest)         = (Just (return (L sp $ mkUnqual varName d)), rest)            -- VARID
check_qvarid ((L sp (HsGenName (SpecialIdData d))) : rest)     = (Just (return (L sp $ mkUnqual varName d)), rest)            -- special_id
check_qvarid ((L sp (HsGenName (UnsafeData d))) : rest)        = (Just (return (L sp $ mkUnqual varName d)), rest)            -- 'unsafe'
check_qvarid ((L sp (HsGenName (SafeData d))) : rest)          = (Just (return (L sp $ mkUnqual varName d)), rest)            -- 'safe'
check_qvarid ((L sp (HsGenName (InterruptibleData d))) : rest) = (Just (return (L sp $ mkUnqual varName d)), rest)            -- 'interruptible'
check_qvarid ((L sp (HsGenName (ForallData d))) : rest)        = (Just (return (L sp $ mkUnqual varName d)), rest)            -- 'forall'
check_qvarid ((L sp (HsGenName (FamilyData d))) : rest)        = (Just (return (L sp $ mkUnqual varName d)), rest)            -- 'family'
check_qvarid ((L sp (HsGenName (RoleData d))) : rest)          = (Just (return (L sp $ mkUnqual varName d)), rest)            -- 'role'
check_qvarid ((L sp (HsGenName (QVaridData d))) : rest)        = (Just (return (L sp $ mkQual varName d)), rest)              -- QVARID
check_qvarid rest = (Nothing, rest)

-- ######## Check function for qconsym: ########
check_qconsym :: LHsTerms -> (Maybe (P (Located RdrName)), LHsTerms)
check_qconsym ((L sp (HsGenName (QConsymData d))) : rest) = (Just (return (L sp $ mkQual dataName d)), rest)         -- QCONSYM
check_qconsym ((L sp (HsGenName (ConsymData d))) : rest)  = (Just (return (L sp $ mkUnqual dataName d)), rest)       -- CONSYM
check_qconsym ((L sp (HsGenName (ColonData d))) : rest)   = (Just (return (L sp $ consDataCon_RDR)), rest)           -- ':'
check_qconsym rest = (Nothing, rest)


-- ######## Check function for qvarsym: ########
check_qvarsym :: LHsTerms -> (Maybe (P (Located RdrName)), LHsTerms)
check_qvarsym ((L sp (HsGenName (QVarsymData d))) : rest)    = (Just (return (L sp $ mkQual varName d)), rest)      -- QVARYSM
check_qvarsym ((L sp (HsGenName (VarsymData d))) : rest)     = (Just (return (L sp $ mkUnqual varName d)), rest)    -- VARYSM
check_qvarsym ((L sp (HsGenName (SpecialSymData d))) : rest) = (Just (return (L sp $ mkUnqual varName d)), rest)    -- special_sym
check_qvarsym ((L sp (HsGenName (MinusSignData d))) : rest)  = (Just (return (L sp $ mkUnqual varName d)), rest)    -- ''-'
check_qvarsym rest = (Nothing, rest)

-- ######## Check function for tup_exprs inside of aexp2: ########
check_tup_exprs :: LHsTerms -> (Maybe (P ([AddAnn],SumOrTuple)), LHsTerms)
check_tup_exprs t
  | (Just texp, rest) <- check_texp t
  = case (check_commas_tup_tail rest) of
      (Just commas_exprs, rest') -> let result = do { texp' <- texp
                                                    ; commas_exprs' <- commas_exprs
                                                    ; return ([],Tuple ((sL1 texp' (Present noExt texp')) : snd commas_exprs')) }
                                    in (Just result, rest')
      _                          -> case rest of
        ((L _ (HsTupBars bars)) : rest') -> let result = do { texp' <- texp
                                                      ; return (mvbars (fst bars), Sum 1  (snd bars + 1) texp') }
                                      in (Just result, rest')
        _                          -> (Nothing, t)
  | ((L _ (HsTupCommas commas)) : rest) <- t
  = case check_tup_tail rest of
      (Just tup_tail, rest') -> let result = do { tup_tail' <- tup_tail
                                                ; return ([],Tuple (map (\l -> L l missingTupArg) (fst commas) ++ tup_tail')) }
                                in (Just result, rest')
      _                      -> (Nothing, t)
  | ((L _ (HsTupBars bars)) : rest) <- t
  = case check_texp rest of
      (Just texp, rest) -> case rest of
        ((L _ (HsTupBars0 bars0)) : rest') -> let result = do { texp' <- texp
                                                        ; return (mvbars (fst bars) ++ mvbars (fst bars0), Sum (snd bars + 1) (snd bars + snd bars0 + 1) texp') }
                                        in (Just result, rest')
        _                            -> (Nothing, t)
      _                 -> (Nothing, t)

check_tup_exprs rest = (Nothing, rest)


-- ######## Check function for `commas_tup_tail` & `tup_tail`: ########
check_commas_tup_tail :: LHsTerms -> (Maybe (P (SrcSpan,[LHsTupArg GhcPs])), LHsTerms)
check_commas_tup_tail ((L _ (HsTupCommas commas)) : rest)
  | (Just tup_tail, rest') <- check_tup_tail rest
  = let result = do { tup_tail' <- tup_tail
                    ; return (head $ fst commas ,(map (\l -> L l missingTupArg) (tail $ fst commas)) ++ tup_tail') }
                 in (Just result, rest')

check_commas_tup_tail rest = (Nothing, rest)


check_tup_tail :: LHsTerms -> (Maybe (P [LHsTupArg GhcPs]), LHsTerms)
check_tup_tail t
  | (Just texp, rest) <- check_texp t
  = case check_commas_tup_tail rest of
      (Just ctt, rest') -> let result = do { texp' <- texp
                                           ; ctt' <- ctt
                                           ; return ((L (gl texp') (Present noExt texp')) : snd ctt') }
                           in (Just result, rest')
      (Nothing, _)      -> let result = do { texp' <- texp
                                           ; return [L (gl texp') (Present noExt texp')] }
                           in (Just result, rest)

check_tup_tail rest = (Just (return [noLoc missingTupArg]), rest)

  -- | (Just texp, [])   <- check_texp t = (Just [L (gl $1) (Present noExt $1)], [])
  -- | Nothing           <- check_texp t = (Just [noLoc missingTupArg], [])
  -- | (Just texp, rest@(HsTupCommas commas : xs)) <- check_texp t
  -- = case (check_commas_tup_tail rest) of
  --   (Just commasExpr, rest') -> (Just ((L (gl texp) (Present noExt texp)) : snd commasExpr), rest')
  --   (Nothing, rest' )        -> (Nothing, rest')


-- ######## old check function for qvar and qcon: ########
-- check_qvar :: LHsTerms -> Maybe (Located RdrName)
-- check_qvar ((L sp (HsGenName name)) : [])
--   = case name of
--       (L _ (VaridData d) : [])   -> Just (L sp $ mkUnqual varName d)            -- VARID in qvar
--       (L _ (QVaridData d) : [])  -> Just (L sp $ mkQual varName d)              -- QVARID in qvar
--       (L _ (SpecialIdData d)     -> Just (L sp $ mkUnqual varName d)            -- special_id in qvar
--       L _ (UnsafeData d)         -> Just (L sp $ mkUnqual varName d)            -- 'unsafe' in qvar
--       L _ (SafeData d)           -> Just (L sp $ mkUnqual varName d)            -- 'safe' in qvar
--       L _ (InterruptibleData d)  -> Just (L sp $ mkUnqual varName d)            -- 'interruptible' in qvar
--       L _ (ForallData d)         -> Just (L sp $ mkUnqual varName d)            -- 'forall' in qvar
--       L _ (FamilyData d)         -> Just (L sp $ mkUnqual varName d)            -- 'family' in qvar
--       L _ (RoleData d)           -> Just (L sp $ mkUnqual varName d)            -- 'role' in qvar
-- check_qvar ((L sp (HsParTerm pt) : [])
--   = case pt of
--       ((L _ (HsGenName (L _ (QVarsymData d)))) : []))     -> Just $ L sp (mkQual varName d)      --  '(' QVARYSM ')' in qvar
--       ((L _ (HsGenName (L _ (VarsymData d)))) : []))      -> Just $ L sp (mkUnqual varName d)    --  '(' VARYSM ')' in qvar
--       ((L _ (HsGenName (L _ (SpecialSymData d)))) : []))  -> Just $ L sp (mkUnqual varName d)    -- '(' special_sym ')' in qvar
--       ((L _ (HsGenName (L _ (MinusSignData d)))) : []))   -> Just $ L sp (mkUnqual varName d)    -- '(' '-' ')' in qvar


-- check_qcon :: LHsTerms -> Maybe (Located RdrName)
-- check_qcon ((L sp (HsGenName name)) : [])
--   = case name of
--       L _ (ConidData d)          -> Just (L sp $ mkUnqual dataName d) -- CONID in qcon
--       L _ (QConidData d)         -> Just (L sp $ mkQual dataName d)   -- QCONID in qcon
-- check_qcon ((L sp (HsParTerm pt) : [])
--   = case pt of
--       ((L _ (HsGenName (L _ (ConsymData d)))) : [])       -> Just (L sp $ mkUnqual varName d)    -- '(' CONSYM ')' in qcon
--       ((L _ (HsGenName (L _ (ColonData d)))) : [])        -> Just (L sp $ consDataCon_RDR)       -- '(' ':' ')' in qcon
--       ((L _ (HsGenName (L _ (QConsymData d)))) : [])      -> Just (L sp $ mkQual dataName d)     -- '(' QCONSYM ')' in qcon
--       []                                                  -> Just (L sp $ nameRdrName (dataConName unitDataCon))   -- '(' ')' in qcon
-- check_qcon ((L sp (HsBoxParTerm bpt) : [])
--   = case bpt of
--       [] ->   Just (L sp $ nameRdrName (dataConName unboxedUnitDataCon))  -- '(#' '#)' in qcon
-- check_qcon ((L sp (HsTupParTerm tpt) : [])
--   = case tpt of
--       ((L _ (HsTupCommas c)) : []) ->   Just (L sp $ nameRdrName (dataConName (tupleDataCon Boxed (snd c + 1)))) --'(' commas ')' in qcon
-- check_qcon ((L sp (HsBoxTupParTerm btpt)) : [])
--   = case btpt of
--       ((L _ (HsTupCommas tc)) : []) -> Just (L sp $ nameRdrName (dataConName $ tupleDataCon Unboxed (snd commas + 1))) -- '(#' commas '#)' in qcon
-- check_qcon ((L sp (HsBracketTerm []]) : []) =  L sp (HsVar noExt $! nameRdrName (dataConName nilDataCon))  -- '[' ']' in qcon

-- ##########################################

-- code from before

-- check_aexp2 ((L sp (HsGenName name)) : [])
-- = case name of
      -- -- L _ (ConidData d)          -> L sp (HsVar noExt $! mkUnqual dataName d )          -- CONID in qcon
      -- -- L _ (QConidData d)         -> L sp (HsVar noExt $! mkQual dataName d)             -- QCONID in qcon
      -- -- L _ (VaridData d)          -> L sp (HsVar noExt $! mkUnqual varName d)            -- VARID in qvar
      -- -- L _ (QVaridData d)         -> L sp (HsVar noExt $! mkQual varName d)              -- QVARID in qvar
      -- -- L _ (SpecialIdData d)      -> L sp (HsVar noExt $! mkUnqual varName d)            -- special_id in qvar
      -- -- L _ (UnsafeData d)         -> L sp (HsVar noExt $! mkUnqual varName d)            -- 'unsafe' in qvar
      -- -- L _ (SafeData d)           -> L sp (HsVar noExt $! mkUnqual varName d)            -- 'safe' in qvar
      -- -- L _ (InterruptibleData d)  -> L sp (HsVar noExt $! mkUnqual varName d)            -- 'interruptible' in qvar
      -- -- L _ (ForallData d)         -> L sp (HsVar noExt $! mkUnqual varName d)            -- 'forall' in qvar
      -- -- L _ (FamilyData d)         -> L sp (HsVar noExt $! mkUnqual varName d)            -- 'family' in qvar
      -- -- L _ (RoleData d)           -> L sp (HsVar noExt $! mkUnqual varName d)            -- 'role' in qvar
      -- L _ (IPDupVaridData d)     -> L sp (HsIPVar noExt $! d)             -- ipvar
      -- L _ (LabelVaridData d)     -> L sp (HsOverLabel noExt $! d)         -- overloaded_label
      -- L _ (LiteralData d)        -> L sp (HsLit noExt $! d)               -- literal
      -- L _ (IntegerData d)        -> L sp (HsOverLit noExt $! d )          -- INTEGER
      -- L _ (RationalData d)       -> L sp (HsOverLit noExt $! d )          -- RATIONAL
      -- L _ (ThIdSpliceData d)     -> L sp $ mkHsSpliceE HasDollar          -- TH_ID_SPLICE in splice_exp
      --                               (L sp $ HsVar noExt (L sp (mkUnqual varName
      --                                                         (getTH_ID_SPLICE $1))))
      -- L _ (ThIdTySpliceData d)   -> L sp $ mkHsSpliceTE HasDollar         -- TH_ID_TY_SPLICE in splice_exp
      --                               (L sp $ HsVar noExt (L sp (mkUnqual varName
      --                                                         (getTH_ID_TY_SPLICE $1))))
      -- L _ (QuasiquoteData d)     -> L sp $ HsSpliceE noExt (unLoc d)      -- quasiquote

-- check_aexp2 ((L sp (HsParTerm pt) : [])
--   = case pt of
      -- ((L _ (HsGenName (L _ (ConsymData d)))) : [])       -> L sp (HsVar noExt $! mkUnqual varName d)    -- '(' CONSYM ')' in qcon
      -- ((L _ (HsGenName (L _ (ColonData d)))) : [])        -> L sp (HsVar noExt $! consDataCon_RDR)       -- '(' ':' ')' in qcon
      -- ((L _ (HsGenName (L _ (QConsymData d)))) : [])      -> L sp (HsVar noExt $! mkQual dataName d)     -- '(' QCONSYM ')' in qcon
      -- ((L _ (HsGenName (L _ (VarsymData d)))) : []))      -> L sp (HsVar noExt $! mkUnqual varName d)    --  '(' VARYSM ')' in qvar
      -- ((L _ (HsGenName (L _ (QVarsymData d)))) : []))     -> L sp (HsVar noExt $! mkQual varName d)      --  '(' QVARYSM ')' in qvar
      -- ((L _ (HsGenName (L _ (SpecialSymData d)))) : []))  -> L sp (HsVar noExt $! mkUnqual varName d)    -- '(' special_sym ')' in qvar
      -- ((L _ (HsGenName (L _ (MinusSignData d)))) : []))   -> L sp (HsVar noExt $! mkUnqual varName d)    -- '(' '-' ')' in qvar
      -- []                                                  -> L sp (HsVar noExt $! nameRdrName (dataConName unitDataCon))   -- '(' ')' in qcon

-- check_aexp2 ((L sp (HsBoxParTerm bpt) : [])
--   = case bpt of
      -- [] ->   L sp (HsVar noExt $! nameRdrName (dataConName unboxedUnitDataCon))  -- '(#' '#)' in qcon

-- check_aexp2 ((L sp (HsTupParTerm tpt) : [])
--   = case tpt of
      -- ((L _ (HsTupCommas c)) : []) -> L sp $ HsVar noExt $! nameRdrName (dataConName (tupleDataCon Boxed (snd c + 1))) -- '(' commas ')' in qcon

-- check_aexp2 ((L sp (HsBoxTupParTerm btpt)) : [])
--   = case btpt of
      -- ((L _ (HsTupCommas tc)) : []) -> L sp $ HsVar noExt $! nameRdrName (dataConName $ tupleDataCon Unboxed (snd commas + 1)) -- '(#' commas '#)' in qcon

-- check_aexp2 ((L sp (HsBracketTerm bt) : [])
--   = case bt of
--       ((L _ (HsListTerm l)) : []) -> L sp (snd l)                                   -- '[' list ']'
--       -- []                          ->  L sp (HsVar noExt $! nameRdrName (dataConName nilDataCon))  -- '[' ']' in qcon

-- check_aexp2 ((L sp (HsThTyQuoteTerm t)) : [])
--   = case t of
--       L sp2 (HsGenName (SpecialSymData specialSym)) -> if specialSym == fsLit "." then L sp $ HsBracket noExt (VarBr noExt False (hintExplicitForall' sp)) else error "don't know"

-- check_aexp2 :: LHsTerms -> LHsExpr GhcPs
-- check_aexp2 lt =
--   case (check_qvar lt) of
--     Just (L sp rdrn) -> L sp (HsVar noExt $! rdrn)
--     Nothing -> -- continue to check other cases

-- = case htqt of
--     (L _ (HsGanName hn))
--       -> case hn of
--           VaridData vd         -> L sp $ HsBracket noExt (VarBr noExt False (mkUnqual tvName vd))     -- VARID in tyvarid
--           SpecialIdData sd     -> L sp $ HsBracket noExt (VarBr noExt False (mkUnqual tvName sd))     -- special_id in tyvarid
--           UnsafeData ud        -> L sp $ HsBracket noExt (VarBr noExt False (mkUnqual tvName sd))     -- 'unsafe' in tyvarid
--           SafeData safed       -> L sp $ HsBracket noExt (VarBr noExt False (mkUnqual tvName safed))  -- 'safe' in tyvarid
--           InterruptibleData id -> L sp $ HsBracket noExt (VarBr noExt False (mkUnqual tvName id))     -- 'interruptible' in tyvarid
--           QConidData qd        -> L sp $ HsBracket noExt (VarBr noExt False (mkQual tcClsName qd))    -- 'QCONID' in qtycon (oqtycon, ntgtycon, gtycon)
--           ConidData cd         -> L sp $ HsBracket noExt (VarBr noExt False (mkUnqual tcClsName cd))  -- 'CONID' in tycon (qtycon, oqtycon, ntgtycon, gtycon)
--     (L _ (HsParTerm ht))
--       case ht of
--         [] -> L sp $ HsBracket noExt (VarBr noExt False (getRdrName unitTyCon))                                           -- '(' ')' in gtycon
--         L _ (HsGenName (L _ (ArrowData _)))       -> L sp $ HsBracket noExt (VarBr noExt False (getRdrName funTyCon))     -- '(' '->' ')' in ntgtycon (gtycon)
--         L _ (HsGenName (L _ (TwiddleData _)))     -> L sp $ HsBracket noExt (VarBr noExt False eqTyCon_RDR)               -- '(' '~' ')' in ntgtycon (gtycon)
--         L _ (HsGenName (L _ (QConsymData qcd)))   -> L sp $ HsBracket noExt (VarBr noExt False (mkQual tcClsName qcd))    -- QCONSYM in '(' qtyconsym')', oqtycon, ntgtycon
--         L _ (HsGenName (L _ (QVarsymData qvd)))   -> L sp $ HsBracket noExt (VarBr noExt False (mkQual tcClsName qvd))    -- QVARSYM in '(' qtyconsym')', oqtycon, ntgtycon
--         L _ (HsGenName (L _ (ConsymData csd)))    -> L sp $ HsBracket noExt (VarBr noExt False (mkQual tcClsName csd))    -- CONSYM in tyconsym, '(' qtyconsym')', oqtycon, ntgtycon
--         L _ (HsGenName (L _ (VarsymData vsd)))    -> L sp $ HsBracket noExt (VarBr noExt False (mkQual tcClsName vsd))    -- VARSYM in tyconsym, '(' qtyconsym')', oqtycon, ntgtycon
--         L _ (HsGenName (L _ (ColonData _)))       -> L sp $ HsBracket noExt (VarBr noExt False consDataCon_RDR)           -- ':' in tyconsym, '(' qtyconsym')', oqtycon, ntgtycon
--         L _ (HsGenName (L _ (MinusSignData md)))  -> L sp $ HsBracket noExt (VarBr noExt False (mkQual tcClsName md))     -- '-' in tyconsym, '(' qtyconsym')', oqtycon, ntgtycon
--     (L _ (HsBoxParTerm []))  -> L sp $ HsBracket noExt (VarBr noExt False (getRdrName unboxedUnitTyCon))  -- '(#' '#)' in tyvarid
--     (L _ (HsBracketTerm [])) -> L sp $ HsBracket noExt (VarBr noExt False listTyCon_RDR)                  -- '[' ']' in ntgtycon
--     (L _ (HsBoxTupParTerm ((L _ (HsTupCommas c)) : [])))                                                  -- '(' commas ')'  in ntgtycon
--       -> L sp $ HsBracket noExt (VarBr noExt False (getRdrName (tupleTyCon Unboxed (snd c + 1))))
--     (L _ (HsTupParTerm ((L _ (HsTupCommas c)) : [])))                                                     -- '(#' commas '#)' in ntgtycon
--       ->  L sp $ HsBracket noExt (VarBr noExt False (getRdrName (tupleTyCon Boxed (snd c + 1))))




{- Note [Adding location info]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is done using the three functions below, sL0, sL1
and sLL.  Note that these functions were mechanically
converted from the three macros that used to exist before,
namely L0, L1 and LL.

They each add a SrcSpan to their argument.

   sL0  adds 'noSrcSpan', used for empty productions
     -- This doesn't seem to work anymore -=chak

   sL1  for a production with a single token on the lhs.  Grabs the SrcSpan
        from that token.

   sLL  for a production with >1 token on the lhs.  Makes up a SrcSpan from
        the first and last tokens.

These suffice for the majority of cases.  However, we must be
especially careful with empty productions: sLL won't work if the first
or last token on the lhs can represent an empty span.  In these cases,
we have to calculate the span using more of the tokens from the lhs, eg.

        | 'newtype' tycl_hdr '=' newconstr deriving
                { L (comb3 $1 $4 $5)
                    (mkTyData NewType (unLoc $2) $4 (unLoc $5)) }

We provide comb3 and comb4 functions which are useful in such cases.

Be careful: there's no checking that you actually got this right, the
only symptom will be that the SrcSpans of your syntax will be
incorrect.

-}

-- Make a source location for the file.  We're a bit lazy here and just
-- make a point SrcSpan at line 1, column 0.  Strictly speaking we should
-- try to find the span of the whole file (ToDo).
fileSrcSpan :: P SrcSpan
fileSrcSpan = do
  l <- getSrcLoc;
  let loc = mkSrcLoc (srcLocFile l) 1 1;
  return (mkSrcSpan loc loc)

-- Hint about the MultiWayIf extension
hintMultiWayIf :: SrcSpan -> P ()
hintMultiWayIf span = do
  mwiEnabled <- liftM ((LangExt.MultiWayIf `extopt`) . options) getPState
  unless mwiEnabled $ parseErrorSDoc span $
    text "Multi-way if-expressions need MultiWayIf turned on"

-- Hint about if usage for beginners
hintIf :: SrcSpan -> String -> P (LHsExpr GhcPs)
hintIf span msg = do
  mwiEnabled <- liftM ((LangExt.MultiWayIf `extopt`) . options) getPState
  if mwiEnabled
    then parseErrorSDoc span $ text $ "parse error in if statement"
    else parseErrorSDoc span $ text $ "parse error in if statement: "++msg

-- Hint about explicit-forall, assuming UnicodeSyntax is on
hintExplicitForall :: SrcSpan -> P ()
hintExplicitForall span = do
    forall      <- extension explicitForallEnabled
    rulePrag    <- extension inRulePrag
    unless (forall || rulePrag) $ parseErrorSDoc span $ vcat
      [ text "Illegal symbol '\x2200' in type" -- U+2200 FOR ALL
      , text "Perhaps you intended to use RankNTypes or a similar language"
      , text "extension to enable explicit-forall syntax: \x2200 <tvs>. <type>"
      ]

-- Hint about explicit-forall, assuming UnicodeSyntax is off
hintExplicitForall' :: SrcSpan -> P (GenLocated SrcSpan RdrName)
hintExplicitForall' span = do
    forall    <- extension explicitForallEnabled
    let illegalDot = "Illegal symbol '.' in type"
    if forall
      then parseErrorSDoc span $ vcat
        [ text illegalDot
        , text "Perhaps you meant to write 'forall <tvs>. <type>'?"
        ]
      else parseErrorSDoc span $ vcat
        [ text illegalDot
        , text "Perhaps you intended to use RankNTypes or a similar language"
        , text "extension to enable explicit-forall syntax: forall <tvs>. <type>"
        ]

-- When two single quotes don't followed by tyvar or gtycon, we report the
-- error as empty character literal, or TH quote that missing proper type
-- variable or constructor. See Trac #13450.
reportEmptyDoubleQuotes :: SrcSpan -> P (GenLocated SrcSpan (HsExpr GhcPs))
reportEmptyDoubleQuotes span = do
    thEnabled <- liftM ((LangExt.TemplateHaskellQuotes `extopt`) . options) getPState
    if thEnabled
      then parseErrorSDoc span $ vcat
        [ text "Parser error on `''`"
        , text "Character literals may not be empty"
        , text "Or perhaps you intended to use quotation syntax of TemplateHaskell,"
        , text "but the type variable or constructor is missing"
        ]
      else parseErrorSDoc span $ vcat
        [ text "Parser error on `''`"
        , text "Character literals may not be empty"
        ]

{-
%************************************************************************
%*                                                                      *
        Helper functions for generating annotations in the parser
%*                                                                      *
%************************************************************************

For the general principles of the following routines, see Note [Api annotations]
in ApiAnnotation.hs

-}

-- |Construct an AddAnn from the annotation keyword and the location
-- of the keyword itself
mj :: AnnKeywordId -> Located e -> AddAnn
mj a l s = addAnnotation s a (gl l)

-- |Construct an AddAnn from the annotation keyword and the Located Token. If
-- the token has a unicode equivalent and this has been used, provide the
-- unicode variant of the annotation.
mu :: AnnKeywordId -> Located Token -> AddAnn
mu a lt@(L l t) = (\s -> addAnnotation s (toUnicodeAnn a lt) l)

-- | If the 'Token' is using its unicode variant return the unicode variant of
--   the annotation
toUnicodeAnn :: AnnKeywordId -> Located Token -> AnnKeywordId
toUnicodeAnn a t = if isUnicode t then unicodeAnn a else a

gl = getLoc

-- |Add an annotation to the located element, and return the located
-- element as a pass through
aa :: Located a -> (AnnKeywordId,Located c) -> P (Located a)
aa a@(L l _) (b,s) = addAnnotation l b (gl s) >> return a

-- |Add an annotation to a located element resulting from a monadic action
am :: P (Located a) -> (AnnKeywordId, Located b) -> P (Located a)
am a (b,s) = do
  av@(L l _) <- a
  addAnnotation l b (gl s)
  return av

-- | Add a list of AddAnns to the given AST element.  For example,
-- the parsing rule for @let@ looks like:
--
-- @
--      | 'let' binds 'in' exp    {% ams (sLL $1 $> $ HsLet (snd $ unLoc $2) $4)
--                                       (mj AnnLet $1:mj AnnIn $3
--                                         :(fst $ unLoc $2)) }
-- @
--
-- This adds an AnnLet annotation for @let@, an AnnIn for @in@, as well
-- as any annotations that may arise in the binds. This will include open
-- and closing braces if they are used to delimit the let expressions.
--
ams :: Located a -> [AddAnn] -> P (Located a)
ams a@(L l _) bs = addAnnsAt l bs >> return a

-- |Add all [AddAnn] to an AST element wrapped in a Just
aljs :: Located (Maybe a) -> [AddAnn] -> P (Located (Maybe a))
aljs a@(L l _) bs = addAnnsAt l bs >> return a

-- |Add all [AddAnn] to an AST element wrapped in a Just
ajs a@(Just (L l _)) bs = addAnnsAt l bs >> return a

-- |Add a list of AddAnns to the given AST element, where the AST element is the
--  result of a monadic action
amms :: P (Located a) -> [AddAnn] -> P (Located a)
amms a bs = do { av@(L l _) <- a
               ; addAnnsAt l bs
               ; return av }

-- |Add a list of AddAnns to the AST element, and return the element as a
--  OrdList
amsu :: Located a -> [AddAnn] -> P (OrdList (Located a))
amsu a@(L l _) bs = addAnnsAt l bs >> return (unitOL a)

-- |Synonyms for AddAnn versions of AnnOpen and AnnClose
mo,mc :: Located Token -> AddAnn
mo ll = mj AnnOpen ll
mc ll = mj AnnClose ll

moc,mcc :: Located Token -> AddAnn
moc ll = mj AnnOpenC ll
mcc ll = mj AnnCloseC ll

mop,mcp :: Located Token -> AddAnn
mop ll = mj AnnOpenP ll
mcp ll = mj AnnCloseP ll

mos,mcs :: Located Token -> AddAnn
mos ll = mj AnnOpenS ll
mcs ll = mj AnnCloseS ll

-- |Given a list of the locations of commas, provide a [AddAnn] with an AnnComma
--  entry for each SrcSpan
mcommas :: [SrcSpan] -> [AddAnn]
mcommas ss = map (\s -> mj AnnCommaTuple (L s ())) ss

-- |Given a list of the locations of '|'s, provide a [AddAnn] with an AnnVbar
--  entry for each SrcSpan
mvbars :: [SrcSpan] -> [AddAnn]
mvbars ss = map (\s -> mj AnnVbar (L s ())) ss

-- |Get the location of the last element of a OrdList, or noSrcSpan
oll :: OrdList (Located a) -> SrcSpan
oll l =
  if isNilOL l then noSrcSpan
               else getLoc (lastOL l)

-- |Add a semicolon annotation in the right place in a list. If the
-- leading list is empty, add it to the tail
asl :: [Located a] -> Located b -> Located a -> P()
asl [] (L ls _) (L l _) = addAnnotation l          AnnSemi ls
asl (x:_xs) (L ls _) _x = addAnnotation (getLoc x) AnnSemi ls
}
