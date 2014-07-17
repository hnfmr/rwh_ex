{-# LINE 1 "RegexConst.hsc" #-}
-- file: ch17/RegexConst.hsc
{-# LINE 2 "RegexConst.hsc" #-}
import Regex

caseless        :: PCREOption
caseless        = PCREOption 1
{-# LINE 6 "RegexConst.hsc" #-}

dollar_endonly  :: PCREOption
dollar_endonly  = PCREOption 32
{-# LINE 9 "RegexConst.hsc" #-}

dotall          :: PCREOption
dotall          = PCREOption 4
{-# LINE 12 "RegexConst.hsc" #-}
