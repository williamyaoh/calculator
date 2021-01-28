-- |
-- For the top-level components that we put as children of our router,
-- there's no need to specify types for input/output/queries.
-- So we provide an easier type synonym for the child slots.

module Calculator.Components.OpaqueSlot
  ( OpaqueSlot )
where

import Prelude

import Halogen as H

type OpaqueSlot slot = forall query. H.Slot query Void slot
