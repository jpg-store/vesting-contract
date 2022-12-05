{-# LANGUAGE NoImplicitPrelude #-}

module Canonical.Shared where

import Plutus.V1.Ledger.Scripts
import PlutusTx
import PlutusTx.Prelude

#include "DebugUtilities.h"
{-# INLINEABLE extractDatumBytes #-}
extractDatumBytes :: [(DatumHash, Datum)] -> DatumHash -> BuiltinData
extractDatumBytes datums dh = getDatum $ extractDatum datums dh

{-# INLINEABLE extractDatum #-}
extractDatum :: [(DatumHash, Datum)] -> DatumHash -> Datum
extractDatum datums dh = go datums
  where
    go = \case
      [] -> TRACE_ERROR ("Failed to find datum")
      (x, y) : xs ->
        if x == dh
          then y
          else go xs

{-# INLINEABLE extractData #-}
extractData :: forall a. DataConstraint (a) => [(DatumHash, Datum)] -> DatumHash -> a
extractData ds dh =
  let a = extractDatumBytes ds dh
   in FROM_BUILT_IN_DATA ("extractData failed", a)

wrap ::
  forall a b c.
  ( DataConstraint (a)
  , DataConstraint (b)
  , DataConstraint (c)
  ) =>
  (a -> b -> c -> Bool) ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  ()
wrap f a b c =
  check
    ( f
        (FROM_BUILT_IN_DATA ("datum failed", a))
        (FROM_BUILT_IN_DATA ("redeemer failed", b))
        (FROM_BUILT_IN_DATA ("script context failed", c))
    )
