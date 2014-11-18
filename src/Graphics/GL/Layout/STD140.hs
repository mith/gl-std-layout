{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
module Graphics.GL.Layout.STD140
    (STD140(..)) where

import Foreign.Ptr
import Foreign.Storable
import Linear
import Control.Applicative
import Graphics.GL.Types
import GHC.Generics
import Debug.Trace

class STD140 a where
    sizeOf140 :: a -> Int
    alignment140 :: a -> Int
    peek140 :: Ptr a -> IO a
    poke140 :: Ptr a -> a -> IO ()
    peekElemOff140 :: Ptr a -> Int -> IO a
    pokeElemOff140 :: Ptr a -> Int -> a -> IO ()
    peekByteOff140 :: Ptr b -> Int -> IO a
    pokeByteOff140 :: Ptr b -> Int -> a -> IO ()

    peek140 p = peekElemOff140 p 0
    poke140 p = pokeElemOff140 p 0

    peekElemOff140 p o = peekByteOff140 p (o * 16)
    pokeElemOff140 p o = pokeByteOff140 p (o * 16)

    peekByteOff140 p o = peek140 (p `plusPtr` o)
    pokeByteOff140 p o = poke140 (p `plusPtr` o)

    default sizeOf140 :: (Generic a, GSTD140 (Rep a)) => a -> Int
    sizeOf140 = gSizeOf140 0 . from

    default alignment140 :: (Generic a, GSTD140 (Rep a)) => a -> Int
    alignment140 = gAlignment140 . from

    {-# MINIMAL sizeOf140 , alignment140,
                (peek140 | peekElemOff140 | peekByteOff140),
                (poke140 | pokeElemOff140 | pokeByteOff140) #-}

instance STD140 GLint where
    sizeOf140 = sizeOf
    alignment140 = alignment
    peek140 = peek
    poke140 = poke

instance STD140 GLuint where
    sizeOf140 = sizeOf
    alignment140 = alignment
    peek140 = peek
    poke140 = poke

instance STD140 GLfloat where
    sizeOf140 = sizeOf
    alignment140 = alignment
    peek140 = peek
    poke140 = poke

instance STD140 GLdouble where
    sizeOf140 = sizeOf
    alignment140 = alignment
    peek140 = peek
    poke140 = poke

instance STD140 GLchar where
    sizeOf140 = sizeOf
    alignment140 = alignment
    peek140 = peek
    poke140 = poke

instance STD140 GLshort where
    sizeOf140 = sizeOf
    alignment140 = alignment
    peek140 = peek
    poke140 = poke

instance STD140 GLushort where
    sizeOf140 = sizeOf
    alignment140 = alignment
    peek140 = peek
    poke140 = poke

instance STD140 GLbyte where
    sizeOf140 = sizeOf
    alignment140 = alignment
    peek140 = peek
    poke140 = poke

instance STD140 GLubyte where
    sizeOf140 = sizeOf
    alignment140 = alignment
    peek140 = peek
    poke140 = poke

instance STD140 a => STD140 (V2 a) where
    sizeOf140 _ = 2 * sizeOf140 (undefined :: a) 
    alignment140 _ = 2 * sizeOf140 (undefined :: a)
    peek140 p = let ap = castPtr $ alignedPtr p :: Ptr a
                in V2 <$> peek140 ap 
                      <*> peekByteOff140 ap (sizeOf140 (undefined :: a))
    poke140 p (V2 a b) = let ap = castPtr $ alignedPtr p :: Ptr a
                         in poke140 ap a 
                         >> pokeByteOff140 ap (sizeOf140 b) b
    peekElemOff140 p o = peekByteOff140 p (o * (alignOffset 16 $ sizeOf140 (undefined :: a)))
    pokeElemOff140 p o = pokeByteOff140 p (o * (alignOffset 16 $ sizeOf140 (undefined :: a)))

instance STD140 a => STD140 (V3 a) where
    sizeOf140 _ = 3 * sizeOf140 (undefined :: a)
    alignment140 _ = 4 * max 4 (sizeOf140 (undefined :: a))
    peek140 p = let ap = castPtr $ alignedPtr p :: Ptr a
                    sz = sizeOf140 (undefined :: a)
                in V3 <$> peek140 ap
                      <*> peekByteOff140 ap sz
                      <*> peekByteOff140 ap (2 * sz)
    poke140 p (V3 a b c) = let ap = castPtr $ alignedPtr p :: Ptr a
                               sz = sizeOf140 a
                           in poke140 ap a 
                              >> pokeByteOff140 ap sz b
                              >> pokeByteOff140 ap (2 * sz) c
    peekElemOff140 p o = peekByteOff140 p (o * (alignOffset 16 $ sizeOf140 (undefined :: a)))
    pokeElemOff140 p o = pokeByteOff140 p (o * (alignOffset 16 $ sizeOf140 (undefined :: a)))

instance STD140 a => STD140 (V4 a) where
    sizeOf140 _ = 4 * sizeOf140 (undefined :: a)
    alignment140 _ = 4 * max 4 (sizeOf140 (undefined :: a))
    peek140 p = let ap = castPtr $ alignedPtr p :: Ptr a
                    sz = sizeOf140 (undefined :: a)
                in V4 <$> peek140 ap
                      <*> peekByteOff140 ap sz
                      <*> peekByteOff140 ap (2 * sz)
                      <*> peekByteOff140 ap (3 * sz)
    poke140 p (V4 a b c d) = let ap = castPtr $ alignedPtr p :: Ptr a
                                 sz = sizeOf140 a
                             in poke140 ap a
                                >> pokeByteOff140 ap sz b
                                >> pokeByteOff140 ap (2 * sz) c
                                >> pokeByteOff140 ap (3 * sz) d
    peekElemOff140 p o = peekByteOff140 p (o * (alignOffset 16 $ sizeOf140 (undefined :: a)))
    pokeElemOff140 p o = pokeByteOff140 p (o * (alignOffset 16 $ sizeOf140 (undefined :: a)))

class GSTD140 f where
    gSizeOf140 :: Int -> f a -> Int
    gAlignment140 :: f a -> Int
    --gPeek140 :: Ptr a -> IO a
    gPoke140 :: Ptr a -> f a -> IO ()
    --gPeekElemOff140 :: Ptr a -> Int -> IO a
    --gPokeElemOff140 :: Ptr a -> Int -> f a -> IO ()
    --gPeekByteOff140 :: Ptr b -> Int -> IO a
    --gPokeByteOff140 :: Ptr b -> Int -> f a -> IO ()

instance GSTD140 a => GSTD140 (M1 i c a) where
    gSizeOf140 o = gSizeOf140 o . unM1
    gAlignment140 = gAlignment140 . unM1
    --gPeek140
    gPoke140 p = gPoke140 p . unM1
    --gPeekElemOff140 
    --gPokeElemOff140 p i = gPokeElemOff140 p i . unM1
    --gPeekByteOff140 
    --gPokeByteOff140 p i = gPokeByteOff140 p i . unM1

instance (GSTD140 a, GSTD140 b) => GSTD140 (a :*: b) where
    gSizeOf140 o (a :*: b) = let aSize = gSizeOf140 o a
                                 ao = alignOffset (o + aSize) 
                                                  (gAlignment140 b)
                                 bSize = gSizeOf140 ao b
                             in bSize + ao - o
    gAlignment140 (a :*: b) = let ma = max (gAlignment140 a)
                                           (gAlignment140 b)
                              in alignOffset ma 16
    gPoke140 p (a :*: b) = do let aSize = gSizeOf140 0 a
                                  ap = alignPtr p $ gAlignment140 a
                              gPoke140 ap a
                              flip gPoke140 b $ alignPtr (plusPtr ap aSize) 
                                                         (gAlignment140 b)

instance forall a c. STD140 a => GSTD140 (K1 c a) where
    gSizeOf140 _ (K1 a) = sizeOf140 a
    gAlignment140 (K1 a) = alignment140 a
    gPoke140 p (K1 a) = poke140 p a

alignOffset :: Int -- ^ offset
            -> Int -- ^ alignment
            -> Int
alignOffset offset aln = case rem offset aln of
                           0 -> offset
                           n -> offset + aln - n

alignedPtr :: forall a. STD140 a => Ptr a -> Ptr a
alignedPtr p = alignPtr p $ alignment140 (undefined :: a)
