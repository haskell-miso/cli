{-# LANGUAGE CPP #-}
-- | The version of the library, from @include/version.h@.
module Shapes.Version (version) where

#include "version.h"

-- | The version, as a string.  It has no signature: its type is the type
-- checker's, which needs the header (in include-dirs, as miso build has it).
version = SHAPES_VERSION
