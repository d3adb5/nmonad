------------------------------------------------------------------------------------------------------------------------
-- |
-- Module      : NMonad.Internal.TH
-- Description : TemplateHaskell helpers for NMonad
-- Copyright   : (c) d3adb5 2023
-- License     : BSD3
-- Maintainer  : d3adb5 <me@d3adb5.net>
-- Stability   : experimental
--
-- This module provides TemplateHaskell helpers for NMonad, primarily due to the adoption of lenses.
------------------------------------------------------------------------------------------------------------------------

module NMonad.Internal.TH
  ( simpleNoPrefixLenses
  , simpleNoPrefixNamer
  ) where

import Control.Lens
import Language.Haskell.TH.Syntax (nameBase, mkName)

-- | A 'FieldNamer' that produces lenses with no prefix from field names that aren't prefixed with an underscore.
simpleNoPrefixNamer :: FieldNamer
simpleNoPrefixNamer _ _ = return . TopName . mkName . nameBase

-- | Rules for generating lenses with no prefix from field names that aren't prefixed with an underscore.
simpleNoPrefixLenses :: LensRules
simpleNoPrefixLenses = lensRules & lensField .~ simpleNoPrefixNamer
