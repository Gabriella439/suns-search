{-| The sole purpose of this module was to remove the dependency on `hmatrix`
    since UCSF does not allow publication of code that is GPLv3 licensed.  So
    there are lots of hacks in here.
-}

{-# LANGUAGE CPP, ForeignFunctionInterface #-}

import qualified Data.Vector.Storable as V
import Data.Vector.Storable ((!))
import Data.List.Split (chunksOf)
import Foreign.C
import Foreign.C.String
import Foreign.Safe
import Foreign.Marshal.Unsafe (unsafeLocalState)

#include <cblas.h>
#include <lapacke.h>

data Matrix = Matrix
    { buf        :: {-# UNPACK #-} !(V.Vector Double)
    , rows       :: {-# UNPACK #-} !Int
    , cols       :: {-# UNPACK #-} !Int
    }
    deriving (Show)

instance Num Matrix where
    (Matrix buf1 rows1 cols1) - (Matrix buf2 rows2 cols2) =
        if (rows1 == rows2 && cols1 == cols2)
        then Matrix (V.zipWith (-) buf1 buf2) rows1 cols1
        else error "(-): Dimensions don't match"
    (Matrix buf1 rows1 cols1) + (Matrix buf2 rows2 cols2) =
        if (rows1 == rows2 && cols1 == cols2)
        then Matrix (V.zipWith (+) buf1 buf2) rows1 cols1
        else error "(+): Dimensions don't match"
    (*) = error "(*): Undefined for Matrix -- use (<>) instead"
    abs = error "abs: Undefined for Matrix"
    signum      = error "signum: Undefined for Matrix"
    fromInteger = error "fromInteger: Undefined for Matrix"

-- Does not check that the inner lists all have the same length
fromLists :: [[Double]] -> Matrix
fromLists xss = case xss of
    []     -> Matrix V.empty 0 0
    ys:yss ->
        let rs = length xss
            cs = length ys
            check row =
                let (prefix, suffix) = splitAt cs row
                in  case suffix of
                        [] -> prefix
                        _  -> error "fromLists: Row lengths don't match"
        in  Matrix (V.fromList (concat (map check xss))) rs cs

toLists :: Matrix -> [[Double]]
toLists cs = chunksOf (cols cs) (V.toList (buf cs))

scale :: Double -> Matrix -> Matrix
scale n (Matrix b rs cs) = Matrix (V.map (n *) b) rs cs

ones :: Int -> Int -> Matrix
ones m n = Matrix (V.replicate (m * n) 1) m n

foreign import ccall "cblas.h cblas_dgemm" c_dgemm
    :: CInt        -- const enum CBLAS_ORDER Order
    -> CInt        -- const enum CBLAS_TRANSPOSE TransA
    -> CInt        -- const enum CBLAS_TRANSPOSE TransB
    -> CInt        -- const int M
    -> CInt        -- const int N
    -> CInt        -- const int K
    -> CDouble     -- const double alpha
    -> Ptr CDouble -- const double *A
    -> CInt        -- const int lda
    -> Ptr CDouble -- const double *B
    -> CInt        -- const int ldb
    -> CDouble     -- const double beta
    -> Ptr CDouble -- double *C
    -> CInt        -- const int ldc
    -> IO Int

c_cblasRowMajor :: CInt
c_cblasRowMajor = #const CblasRowMajor

c_cblasNoTrans :: CInt
c_cblasNoTrans = #const CblasNoTrans

foreign import ccall unsafe "string.h memset" c_memset
    :: Ptr a -> CInt -> CSize -> IO (Ptr Word8)

clear :: Ptr CDouble -> Int -> IO ()
clear p n = do
    c_memset p 0 (fromIntegral $ sizeOf (undefined :: CDouble) * n)
    return ()

(<>) :: Matrix -> Matrix -> Matrix
(Matrix bufA rowsA colsA) <> (Matrix bufB rowsB colsB) =
    let 
    in  if (colsA == rowsB)
        then unsafeLocalState $ do
                 let sizeC = rowsA * colsB
                 fpC <- mallocForeignPtrArray sizeC :: IO (ForeignPtr CDouble)
                 V.unsafeWith (V.map realToFrac bufA) $ \pA -> do
                     V.unsafeWith (V.map realToFrac bufB) $ \pB -> do
                     withForeignPtr fpC $ \pC -> do
                         clear pC sizeC
                         c_dgemm
                             c_cblasRowMajor
                             c_cblasNoTrans
                             c_cblasNoTrans
                             (fromIntegral rowsA)
                             (fromIntegral colsB)
                             (fromIntegral colsA)
                             1.0
                             pA
                             (fromIntegral colsA)
                             pB
                             (fromIntegral colsB)
                             0.0
                             pC
                             (fromIntegral colsB)
                 let bufC = V.map realToFrac $ V.unsafeFromForeignPtr0 fpC sizeC
                 return (Matrix bufC rowsA colsB)
        else error "(<>): Dimensions do not match"

trans :: Matrix -> Matrix
trans (Matrix buf_ rows_ cols_) =
    let buf' = V.generate (rows_ * cols_) $ \i ->
            let (r', c') = quotRem i rows_
            in  buf_ ! (c' * cols_ + r')
    in  Matrix buf' cols_ rows_

foreign import ccall "lapacke.h LAPACKE_dgesvd" c_dgesvd
    :: CInt         -- int matrix_order
    -> CChar        -- char jobu
    -> CChar        -- char jobvt
    -> CInt         -- lapack_int m
    -> CInt         -- lapack_int n
    -> Ptr CDouble  -- double *a
    -> CInt         -- lapack_int lda
    -> Ptr CDouble  -- double *s
    -> Ptr CDouble  -- double *u
    -> CInt         -- lapack_int ldu
    -> Ptr CDouble  -- double *vt
    -> CInt         -- lapack_int ldvt
    -> Ptr CDouble  -- double *superb
    -> IO Int

-- TODO: Check 'info' return value of c_dgesvd
svd :: Matrix -> (Matrix, Matrix, Matrix)
svd (Matrix bufA rowsA colsA) = unsafeLocalState $ do
    let sizeU  = rowsA * rowsA
        sizeVT = colsA * colsA
        sizeS  = min rowsA colsA
        sizeB  = min rowsA colsA - 1
    fpU  <- mallocForeignPtrArray sizeU  :: IO (ForeignPtr CDouble)
    fpVT <- mallocForeignPtrArray sizeVT :: IO (ForeignPtr CDouble)
    fpS  <- mallocForeignPtrArray sizeS  :: IO (ForeignPtr CDouble)
    fpB  <- mallocForeignPtrArray sizeB  :: IO (ForeignPtr CDouble)
    V.unsafeWith (V.map realToFrac bufA) $ \pA -> do
        withForeignPtr fpU  $ \pU -> do
        withForeignPtr fpVT $ \pVT -> do
        withForeignPtr fpS  $ \pS -> do
        withForeignPtr fpB  $ \pB -> do
            clear pU  sizeU
            clear pVT sizeVT
            clear pS  sizeS
            clear pB  sizeB
            c_dgesvd
                c_lapackRowMajor
                (castCharToCChar 'A')
                (castCharToCChar 'A')
                (fromIntegral rowsA)
                (fromIntegral colsA)
                pA
                (fromIntegral colsA)
                pS
                pU
                (fromIntegral rowsA)
                pVT
                (fromIntegral colsA)
                pB
    let bufU  = V.map realToFrac $ V.unsafeFromForeignPtr0 fpU  sizeU
        bufVT = V.map realToFrac $ V.unsafeFromForeignPtr0 fpVT sizeVT
        vecS  = V.map realToFrac $ V.unsafeFromForeignPtr0 fpS  sizeS
        bufS  = V.generate (rowsA * colsA) $ \i ->
            let (r, c) = quotRem i colsA
            in  if (r == c) then vecS ! r else 0
        u  = Matrix bufU  rowsA rowsA
        s  = Matrix bufS  rowsA colsA
        vT = Matrix bufVT colsA colsA
    return (u, s, trans vT)

c_lapackRowMajor:: CInt
c_lapackRowMajor = #const LAPACK_ROW_MAJOR

-- LAPACK doesn't provide a way to compute determinants, so this is a big hack:
-- Only compute the determinant for 3x3 matrices, since that is all that
-- Kabsch.hs needs.
det :: Matrix -> Double
det (Matrix v rowsA colsA) =
    if (rowsA == 3 && colsA == 3)
    then   (v ! 0) * ((v ! 4) * (v ! 8) - (v ! 5) * (v ! 7))
         + (v ! 1) * ((v ! 5) * (v ! 6) - (v ! 3) * (v ! 8))
         + (v ! 2) * ((v ! 3) * (v ! 7) - (v ! 4) * (v ! 6))
    else error "det: Not a 3x3 matrix"

mapMatrix :: (Double -> Double) -> Matrix -> Matrix
mapMatrix f (Matrix bufA rowsA colsA) = Matrix (V.map f bufA) rowsA colsA

sumElements :: Matrix -> Double
sumElements (Matrix bufA _ _) = V.sum bufA

-- This is yet another hack that takes advantage of the fact that the second
-- argument of `repmat` is always 1 in Kabsch.hs
repmat :: Matrix -> Int -> Int -> Matrix
repmat (Matrix bufA rowsA colsA) n 1 = Matrix bufA' (rowsA * n) colsA
  where
    bufA' = V.generate (rowsA * colsA * n) $ \i ->
        bufA ! (i `rem` (colsA * rowsA))
repmat _ _ _ = error "repmat: Third argument was not 1"
