{-# LANGUAGE RankNTypes, GeneralizedNewtypeDeriving #-}
import Control.Monad.Writer
import System.IO

newtype SafeT s m v =
  SafeT { unSafeT :: WriterT [Handle] m v }
  deriving (Monad, Applicative, Functor, MonadIO)

type SIO s = SafeT s IO

data SHandle s = SHandle Handle

runSIO :: (forall s. SIO s v) -> IO v
runSIO m = do
  (v, hdrs) <- runWriterT (unSafeT m)
  forM_ hdrs $ \h -> do
    putStrLn $ "close file " ++ show h
    hClose h
  return v

sOpenFile :: FilePath -> IOMode -> SIO s (SHandle s)
sOpenFile path mode = SafeT $ do
  hd <- liftIO $ openFile path mode
  liftIO $ putStrLn $ "open file " ++ show hd
  tell [hd]
  return (SHandle hd)

sHGetLine :: SHandle s -> SIO s String
sHGetLine (SHandle h) = do
  line <- liftIO $ hGetLine h
  liftIO $ putStrLn $ "read file " ++ show h
  return (show h ++ " " ++ line)

---------------------------------------------------

--sampleProgram :: forall s. SIO s String -- (case 1)
--sampleProgram :: forall s. SIO s (String, ?) -- (case 2)
sampleProgram = do
  fileSimpleH <- sOpenFile "text1" ReadMode
  firstLine <- sHGetLine fileSimpleH

  secondLine <- sampleSubProgram -- case 2-
  --(secondLine, fileIgnoreH) <- sampleSubProgram -- case 2+

  thirdLine <- sHGetLine fileSimpleH

  let result = firstLine ++ "\n" ++ secondLine ++ "\n" ++ thirdLine

  return result -- case 3-
  --return (result, fileSimpleH) -- case 3+

sampleSubProgram = do
  fileIgnoreH <- sOpenFile "text2" ReadMode
  secondLine <- sHGetLine fileIgnoreH
  return secondLine -- case 2-
  --return (secondLine, fileIgnoreH) -- case 2+

main = do
  result <- runSIO sampleProgram -- case 3-
  --(result, fileSimpleH) <- runSIO sampleProgram -- case 3+
  putStrLn result
