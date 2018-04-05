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

importSHandle :: SHandle p -> SHandle s
importSHandle (SHandle h) = SHandle h

---------------------------------------------------

--sampleProgram :: forall s. SIO s String
sampleProgram = do
  fileSimpleH <- sOpenFile "text1" ReadMode
  firstLine <- sHGetLine fileSimpleH

  secondLine <- liftIO $ runSIO $ do -- case 2-
  --(secondLine, fileIgnoreH) <- liftIO $ runSIO $ do -- case 2+
    fileIgnoreH <- sOpenFile "text2" ReadMode
    secondLine <- sHGetLine fileIgnoreH

    outerLine <- sHGetLine (importSHandle fileSimpleH)

    let result = secondLine ++ "\n" ++ outerLine
    return result -- case 2-
    --return (result, importSHandle fileIgnoreH) -- case 2+

  thirdLine <- sHGetLine fileSimpleH

  let result = firstLine ++ "\n" ++ secondLine ++ "\n" ++ thirdLine
  return result

main = do
  result <- runSIO sampleProgram
  putStrLn result
