﻿-- file: ch6/JSONClass.hs

import SimpleJSON

{-# LANGUAGE TypeSynonymInstances #-}

type JSONError = String

class JSON a where
    toJValue :: a -> JValue
    fromJValue :: JValue -> Either JSONError a

instance JSON JValue where
    toJValue = id
    fromJValue = Right

instance JSON Bool where
    toJValue = JBool
    fromJValue (JBool b) = Right b
    fromJValue _ = Left "not a JSON boolean"

instance JSON String where
    toJValue               = JString
    fromJValue (JString s) = Right s
    fromJValue _           = Left "not a JSON string"
