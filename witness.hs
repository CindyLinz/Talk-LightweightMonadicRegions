{-# LANGUAGE RankNTypes, GeneralizedNewtypeDeriving #-}
-- Hi nested

import Control.Monad.Writer
import System.IO

newtype IORT s m v =
  IORT { unIORT :: WriterT [Handle] m v }
  deriving (Monad, Applicative, Functor, MonadIO)

type SIO s = IORT s IO

data SHandle s = SHandle Handle

runSIO :: (forall s. SIO s v) -> IO v
runSIO m = do
  (v, hdrs) <- runWriterT (unIORT m)
  forM_ hdrs $ \h -> do
    putStrLn $ "close file " ++ show h
    hClose h
  return v

sOpenFile :: FilePath -> IOMode -> SIO s (SHandle s)
sOpenFile path mode = IORT $ do
  hd <- liftIO $ openFile path mode
  liftIO $ putStrLn $ "open file " ++ show hd
  tell [hd]
  return (SHandle hd)

sHGetLine :: SHandle s -> SIO s String
sHGetLine (SHandle h) = do
  line <- liftIO $ hGetLine h
  liftIO $ putStrLn $ "read file " ++ show h
  return line

liftSIO :: (SIO r 

---------------------------------------------------

--sampleProgram :: forall s. SIO s String
sampleProgram = do
  fileSimpleH <- sOpenFile "simple.hs" ReadMode
  firstLine <- sHGetLine fileSimpleH

  secondLine <- liftIO $ runSIO $ do
  --(secondLine, fileIgnoreH) <- liftIO $ runSIO $ do
    fileIgnoreH <- sOpenFile ".gitignore" ReadMode
    secondLine <- sHGetLine fileIgnoreH

    --outerLine <- sHGetLine fileSimpleH

    return secondLine
    --return (secondLine, fileIgnoreH)

  thirdLine <- sHGetLine fileSimpleH

  let result = firstLine ++ "\n" ++ secondLine ++ "\n" ++ thirdLine
  return result
  --return (result, fileSimpleH)

main = do
  result <- runSIO sampleProgram
  --(result, fileSimpleH) <- runSIO sampleProgram
  putStrLn result
