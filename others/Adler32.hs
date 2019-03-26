-- file: ch04/Adler32.hs

import Data.Char (ord)
import Data.Bits (shiftL, (.&.), (.|.))
base = 65521

adler32_foldl xs = let (a, b) = foldl step (1, 0) xs
                   in (b `shiftL` 16) .|. a
    where step (a, b) x = let a' = a + (ord x .&. 0xff)
                          in (a' `mod` base, (a' + b) `mod` base)


{-
    public class Adler32 
    {
        private static final int base = 65521;
        public static int compute(byte[] data, int offset, int length)
        {
            int a = 1, b = 0;
            for (int i = offset; i < offset + length; i++) {
                a = (a + (data[i] & 0xff)) % base;
                b = (a + b) % base;
            }
            return (b << 16) | a;
        }
    }

-}




