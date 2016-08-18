module Utils (Block(..),
              blockify)
where

data Block = Block { blckFirstBlock :: Int
                   , blckBlockSize :: Int
                   , blckBlocksCount :: Int
                   , blckOffsetInFirstBlock :: Int }
           deriving (Show, Eq)

blockify :: (Integral blockSize, Integral bytesCount, Integral offset) => blockSize -> bytesCount -> offset -> Block
blockify blockSize bytesCount offsetFromStart = 
    Block firstBlock bs blocksCount inBlockOffset
    where offset        = (fromIntegral offsetFromStart)
          bs            = (fromIntegral blockSize)
          firstBlock    = offset `div` bs
          inBlockOffset = offset `mod` bs
          blocksCount   = (((fromIntegral bytesCount) + inBlockOffset) `div` bs) + 1
