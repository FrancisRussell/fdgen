module FDGEN.Tensor ( Tensor, getElement, setElement, add, sub, inner
                    , outer, dot, pairwiseWithOp, innerWithOp
                    , outerWithOp, dotWithOp, constructTensor
                    , generateTensor, TensorIndex, divide, mapWithIndex
                    , flattenIndex, getShape, TensorShaped(..)
                    , constructShape, numEntries, unflattenIndex) where
import Control.Applicative ((<$>))
import Data.Foldable (foldl')
import Data.List.Split (chunksOf)
import Data.List (transpose, genericIndex, genericSplitAt)
import FDGEN.Pretty (PrettyPrintable(..), structureDoc, vListDoc)

data Tensor e = Tensor
  { _tensorDim :: Integer
  , _tensorRank :: Integer
  , _tensorEntries :: [e]
  } deriving Show

data TensorShape
  = TensorShape Integer Integer
  deriving (Eq, Ord, Show)

class TensorShaped s where
  tensorRank :: s -> Integer
  tensorDim :: s -> Integer

instance PrettyPrintable e => PrettyPrintable (Tensor e)
  where
  toDoc tensor = structureDoc "Tensor"
    [ ("rank", toDoc $ _tensorRank tensor)
    , ("dimension", toDoc $ _tensorDim tensor)
    , ("entries", vListDoc $ _tensorEntries tensor)
    ]

type TensorIndex = [Integer]

instance TensorShaped (Tensor e) where
  tensorRank = _tensorRank
  tensorDim = _tensorDim

instance TensorShaped TensorShape where
  tensorRank (TensorShape _ r) = r
  tensorDim (TensorShape d _) = d

numEntries :: TensorShaped e => e -> Integer
numEntries t = (tensorDim t) ^ (tensorRank t)

getShape :: TensorShaped e => e -> TensorShape
getShape t = TensorShape (tensorDim t) (tensorRank t)

constructShape :: Integer -> Integer -> TensorShape
constructShape dim rank = TensorShape dim rank

constructTensor :: Integer -> Integer -> [e] -> Tensor e
constructTensor dim rank entries = if dim < 0
  then error "constructTensor: tensor dimension must be > 0"
  else if rank < 0
  then error "constructTensor: tensor rank must be > 0"
  else if (toInteger $ length entries) /= dim ^ rank then
  error "constructTensor: called with wrong number of entries"
  else Tensor
    { _tensorDim = dim
    , _tensorRank = rank
    , _tensorEntries = entries
    }

instance Functor Tensor where
  fmap f t = Tensor
    { _tensorDim = tensorDim t
    , _tensorRank = tensorRank t
    , _tensorEntries = f <$> _tensorEntries t
    }

mapWithIndex :: (TensorIndex -> e -> f) -> Tensor e -> Tensor f
mapWithIndex f t = Tensor
    { _tensorDim = tensorDim t
    , _tensorRank = tensorRank t
    , _tensorEntries = tensorEntries
    }
    where
    tensorEntries = zipWith f [unflattenIndex t i | i <- [0..]] (_tensorEntries t)

generateTensor :: Integer -> Integer -> (TensorIndex -> e) -> Tensor e
generateTensor dim rank gen = genEntry <$> indexTensor
  where
  genEntry = gen . unflattenIndex indexTensor
  indexTensor = constructTensor dim rank indices
  indices = [0 .. (dim ^ rank - 1)]

unflattenIndex :: TensorShaped e => e -> Integer -> TensorIndex
unflattenIndex t i = if i < 0 || i >= numEntries t
  then error "unflattenIndex: tensor index out of range"
  else index (tensorRank t) i
    where
    dim = tensorDim t
    index rank remainder = if rank > 0
      then (remainder `div` divisor):(index (rank - 1) (remainder `mod` divisor))
      else []
      where divisor = dim ^ (rank - 1)

flattenIndex :: TensorShaped e => e -> TensorIndex -> Integer
flattenIndex t entries = if (toInteger $ length entries) /= tensorRank t
  then error "flattenIndex: wrong number of entries in index"
  else if (length $ filter (\x -> x < 0 || x >= tensorDim t) entries) /= 0
  then error "flattenIndex: index component out of range"
  else index (tensorRank t) entries
    where
    index rank (r:remainder) = r * (tensorDim t) ^ (rank - 1) + index (rank - 1) remainder
    index _ [] = 0

setElement :: Tensor e -> TensorIndex -> e -> Tensor e
setElement t idx value = t { _tensorEntries = updatedEntries }
  where
  updatedEntries = set (_tensorEntries t) (flattenIndex t idx) value
  set lst index v = first ++ (v:second)
    where
    (first, _:second) = genericSplitAt index lst

getElement :: Tensor e -> TensorIndex -> e
getElement t idx = genericIndex (_tensorEntries t) (flattenIndex t idx)

sameShape :: Tensor a -> Tensor b -> Bool
sameShape a b = getShape a == getShape b

pairwiseWithOp :: (a -> b -> c) -> Tensor a -> Tensor b -> Tensor c
pairwiseWithOp f a b = if not $ sameShape a b
  then error "pairwiseWithOp: both operands must be same shape"
  else constructTensor (tensorDim a) (tensorRank a) entries
    where
    entries = zipWith f (_tensorEntries a) (_tensorEntries b)

add :: Num a => Tensor a -> Tensor a -> Tensor a
add = pairwiseWithOp (+)

sub :: Num a => Tensor a -> Tensor a -> Tensor a
sub = pairwiseWithOp (-)

divide :: Fractional a => Tensor a -> Tensor a -> Tensor a
divide a b = if tensorDim a /= tensorDim b
  then error "div: operarands must have same dimension"
  else if tensorRank b /= 0
    then error "div: RHS must be scalar-valued tensor"
    else (/ bValue) <$> a
      where [bValue] = _tensorEntries b

outerWithOp :: (a -> b -> c) -> Tensor a -> Tensor b -> Tensor c
outerWithOp op a b = if tensorDim a /= tensorDim b
  then error "outerWithOp: operands must have same dimension"
  else constructTensor newDim newRank entries
    where
    newRank = (tensorRank a) + (tensorRank b)
    newDim = tensorDim a
    entries = [aElem `op` bElem | aElem <- _tensorEntries a, bElem <- _tensorEntries b]

outer :: Num a => Tensor a -> Tensor a -> Tensor a
outer = outerWithOp (*)

innerWithOp :: (a -> b -> c) -> (c -> c -> c) -> Tensor a -> Tensor b -> Tensor c
innerWithOp mul combine a b = if not $ sameShape a b
  then error "innerWithOp: both operands must be same shape"
  else constructTensor (tensorDim a) 0 [summed]
    where
    toSum = zipWith mul (_tensorEntries a) (_tensorEntries b)
    summed = foldl' combine (head toSum) (tail toSum)

inner :: Num a => Tensor a -> Tensor a -> Tensor a
inner = innerWithOp (*) (+)

dotWithOp :: (a -> b -> c) -> (c -> c -> c) -> Tensor a -> Tensor b -> Tensor c
dotWithOp mul combine a b = if tensorDim a /= tensorDim b
  then error "dotWithOp: operands must have same dimension"
  else if newRank < 0
  then error "dotWithOp: ranks too low to perform dot product"
  else constructTensor dim newRank resultEntries
    where
    dim = tensorDim a
    newRank = (tensorRank a) + (tensorRank b) - 2
    leftElems = _tensorEntries a
    rightElems = _tensorEntries b
    left = chunksOf (fromInteger dim) leftElems
    right = transpose $ chunksOf (fromInteger $ dim ^ (tensorRank b - 1)) (rightElems)
    resultEntries = [inner' l r | l <- left, r <- right]
    inner' a' b' = foldl' combine (head toSum) (tail toSum)
      where
      toSum = zipWith mul a' b'

dot :: Num a => Tensor a -> Tensor a -> Tensor a
dot = dotWithOp (*) (+)
