-- Requires RSA package
--
-- Write the output of this program to Network/ADB/adb_auth_private_key.h
import Crypto.Random
import Codec.Crypto.RSA
import Data.Bits
import Data.Char
import Data.List
import Data.Maybe
import Numeric
import Data.Int
import Data.Word


public_n :: PublicKey -> Integer
public_n k = extract "public_n = " (show k)

public_e :: PublicKey -> Integer
public_e k = extract "public_e = " (show k)

private_d :: PrivateKey -> Integer
private_d k = extract "private_d = " (show k)

extract :: String -> String -> Integer
extract n [] = error $ "can't extract "++show n
extract n h | n `isPrefixOf` h = read $ takeWhile isDigit (drop (length n) h)
extract n (_:h) = extract n h

main :: IO ()
main = do
    g <- newGenIO :: IO SystemRandom
    let (pub, priv, _) = generateKeyPair g 2048
    putStr $ dumpBigInt "public_n" (public_n pub)
    putStr $ dumpBigInt "public_e" (public_e pub)
    putStr $ dumpBigInt "private_d" (private_d priv)
    let rsaNumBytes = 256
        rsaNumWords = rsaNumBytes `div` 4
        r32 = 2^32
        r = 2 ^ (rsaNumWords * 32)
        rr = (r^2) `mod` public_n pub
        rem = public_n pub `mod` r32
        n0inv =
            let one = fromMaybe (error "no modular inverse") $ rem `modInv` r32
            in  fromIntegral (-one) .&. (2^32 - 1) :: Int32
    --putStrLn $ show rem ++ " -> " ++ show (rem `modInv` 32)
    putStr $ dumpRSAKey rsaNumWords n0inv (public_n pub) rr (public_e pub)

dumpRSAKey rsaNumWords n0inv n rr e =
    "const RSAPublicKey public_key = {\n" ++
    "    "++show rsaNumWords++",\n" ++
    "    "++show n0inv++",\n" ++
    "    {"++(intercalate "," . map show . toWords $ n)++"},\n" ++
    "    {"++(intercalate "," . map show . toWords $ rr)++"},\n" ++
    "    "++show e++"\n"++
    "};\n"

toWords :: Integer -> [Int32]
toWords i = go i
  where
    go i | i == 0 = []
    go i = let (hi, lo) = i `divMod` (2^32)
           in  fromIntegral lo : go hi

-- Extended Euclidean algorithm.  Given non-negative a and b, return x, y and g
-- such that ax + by = g, where g = gcd(a,b).  Note that x or y may be negative.
gcdExt a 0 = (1, 0, a)
gcdExt a b = let (q, r) = a `quotRem` b
                 (s, t, g) = gcdExt b r
             in (t, s - q * t, g)
 
-- Given a and m, return Just x such that ax = 1 mod m.  If there is no such x
-- return Nothing.
modInv a m = let (i, _, g) = gcdExt a m
             in if g == 1 then Just (mkPos i) else Nothing
  where mkPos x = if x < 0 then x + m else x
 
dumpBigInt :: String -> Integer -> String
dumpBigInt name i =
    "const uint8_t "++name++"[] = {\n"++
    hex ++ "\n" ++
    "};\n"++
    "const int "++name++"_length = "++show n++";\n\n" 
  where
    bytes = toBytes i :: [Word8]
    n = length bytes
    hex = intercalate "," .
          map (\x -> "0x"++showHex x "") $ bytes
    toBytes i = reverse (go i)
      where
        go i | i == 0 = []
        go i = let (hi, lo) = i `divMod` 256
               in  fromIntegral lo : go hi
