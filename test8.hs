{-# LANGUAGE InstancesSigs #-}

module HigherOrderFunctions where
-- data Chain
isPrefixOf :: Eq txs => Chain txs -> Chain txs -> Bool
isPrefixOf GenesisBlock GenesisBlock = True
isPrefixOf (Block _ _) GenesisBlock = False
isPrefixOf c        d@(Block xs x) = 
    isPrefixOf c xs || c == d

allEquals :: Eq a => [a] -> Bool
allEquals []        = True
allEquals [_]       = True
allEquals (x : xs@(y : _)) = x == y && allEquals xs