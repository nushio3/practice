import Data.Maybe
import Data.SBV


f :: SWord8 ->  SWord8
f x = ite (x .>= 65 &&& x .<= 90) ((x-65+13) `sMod` 26 + 65) $
      ite (x .>= 97 &&& x .<=122) ((x-97+13) `sMod` 26 + 97) $
      x

encryptChar :: Char -> IO Char
encryptChar c = do
  let x :: SWord8
      x = fromIntegral $ fromEnum c
  ret <- sat $ forSome ["y"] (\y-> y .== f x)
  return $ toEnum  $ fromIntegral $ fromJust (extractModel ret :: Maybe Word8)

encrypt :: String -> IO String
encrypt = mapM encryptChar

proveF = do
  ret <- prove $ do
    x <- sWord8 "c"
    return $ f (f x) .== x
  print ret
