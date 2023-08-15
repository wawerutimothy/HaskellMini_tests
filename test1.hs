data Chain = 
    GenesisBlock
    Block chain Txs


    chain1::chain
    chain1 = Block GenesisBlock 2
    chain2::chain
    chain2 = Block Chain 4

    ChainLength :: Chain -> Int
    ChainLength GenesisBlock=0
    ChainLength (Block c n) = 1 + ChainLength c 

    hasBlock :: Txs -> Chain -> Bool
    hasBlock  _GenesisBlock = False
    hasBlcok n(Block c t) -n ==t || hasblock c n

    hasBlockProp :: (Txs -> Bool) -> Chain -> Bool

    hasBlockProp _ GenesisBlock = False
    hasBlockProp p (Block c t) = p t || hasBlockProp p c 