-- file: ch17/RegexConst.hsc
import Regex

caseless        :: PCREOption
caseless        = PCREOption #const PCRE_CASELESS

dollar_endonly  :: PCREOption
dollar_endonly  = PCREOption #const PCRE_DOLLAR_ENDONLY

dotall          :: PCREOption
dotall          = PCREOption #const PCRE_DOTALL
