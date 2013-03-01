{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Snap.Restful.TH where

------------------------------------------------------------------------------
import           Control.Applicative
import           Language.Haskell.TH
import           Snap.Restful
import           Text.Digestive
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Gets a list of constructors for a Name.
nameCons :: Name -> Q [Con]
nameCons n = do
    info <- reify n
    case info of
      TyConI dec -> decCons dec
      _ -> return []


------------------------------------------------------------------------------
-- | Gets a list of constructors for a Dec.
decCons :: Dec -> Q [Con]
decCons (DataD _ _ _ cons _) = return cons
decCons (NewtypeD _ _ _ con _) = return [con]
decCons (TySynD _ _ t) = typeCons t
decCons _ = return []


------------------------------------------------------------------------------
-- | Gets a list of constructors for a Type.
typeCons :: Type -> Q [Con]
typeCons (AppT a _) = typeCons a
typeCons (ConT n) = nameCons n
typeCons _ = return []


------------------------------------------------------------------------------
-- | Derives a HasFormlet instance for a data type.
deriveHasFormlet :: Name -> Q [Dec]
deriveHasFormlet n = do
    cons <- nameCons n
    case cons of
      [RecC conName fields] -> do
        defName <- newName "d"
        let fieldFormlet (fn,_,_) = do
              let name = litE $ StringL $ nameBase fn
              [| $name .: formlet ( $(varE fn) <$> $(varE defName) ) |]
        (f:fs) <- mapM fieldFormlet fields
        let start = UInfixE (ConE conName) (VarE '(<$>)) f
            splat = VarE '(<*>)
            res = foldl (\a b -> UInfixE a splat b) start fs
            func = [FunD 'formlet [Clause [VarP defName] (NormalB res) []]]
        return $ [InstanceD [] (AppT (ConT ''HasFormlet) (ConT n)) func]
      _ -> error "You can only generate formlets for a data type with a single constructor and named record fields"


------------------------------------------------------------------------------
-- | Generates interpreted splices for a data type.  All of the data type's
-- fields must be instances of the PrimSplice type class.
--
-- Usage:
--
-- > fooSplices :: Monad m => Foo -> [(Text, I.Splice m)]
-- > fooSplices = $(iSplices ''Foo)
iSplices :: Name -> Q Exp
iSplices n = do
    cons <- nameCons n
    case cons of
      [RecC conName fields] -> do
        param <- newName "x"
        let fieldToTuple (fn,_,_) = do
              f <- [| iPrimSplice $ $(appE (varE fn) (varE param)) |]
              return $ TupE [LitE $ StringL $ nameBase fn, f]
        fs <- mapM fieldToTuple fields
        return $ LamE [VarP param] (ListE fs)
      _ -> error "You can only generate splices for a data type with a single constructor and named record fields"


------------------------------------------------------------------------------
-- | Generates compiled splices for a data type.  All of the data type's
-- fields must be instances of the PrimSplice type class.
--
-- Usage:
--
-- > fooSplices = $(cSplices ''Foo)
cSplices :: Name -> Q Exp
cSplices n = do
    cons <- nameCons n
    case cons of
      [RecC conName fields] -> do
        let fieldToTuple (fn,_,_) = do
              f <- [| cPrimSplice . $(varE fn) |]
              return $ TupE [LitE $ StringL $ nameBase fn, f]
        fs <- mapM fieldToTuple fields
        return $ ListE fs
      _ -> error "You can only generate splices for a data type with a single constructor and named record fields"



