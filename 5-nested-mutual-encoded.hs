{-# LANGUAGE RankNTypes, GeneralizedNewtypeDeriving #-}
import Control.Monad.Writer
import System.IO
import Data.Monoid

newtype SafeT s m v =
  SafeT { unSafeT :: WriterT (MultiLayers [Handle]) m v }
  deriving (Monad, Applicative, Functor, MonadIO)

type SIO s = SafeT s IO

data SHandle s = SHandle Handle

newtype MultiLayers a = MultiLayers [a]

instance Monoid a => Monoid (MultiLayers a) where
  mempty = MultiLayers (repeat mempty)
  mappend (MultiLayers as) (MultiLayers bs) = MultiLayers (zipWith mappend as bs)
--instance Monoid (MultiLayers [a]) where
--  mempty = MultiLayers (repeat [])
--  mappend (MultiLayers as) (MultiLayers bs) = MultiLayers (zipWith (++) as bs)
--instance Monoid (MultiLayers [Handle]) where
--  mempty = MultiLayers (repeat [])
--  mappend (MultiLayers as) (MultiLayers bs) = MultiLayers (zipWith (++) as bs)

runSIO :: (forall s. SIO s v) -> IO v
runSIO m = do
  (v, MultiLayers (hdrs:_)) <- runWriterT (unSafeT m)
  forM_ hdrs $ \h -> do
    putStrLn $ "close file " ++ show h
    hClose h
  return v

sOpenFile :: FilePath -> IOMode -> SIO s (SHandle s)
sOpenFile path mode = SafeT $ do
  hd <- liftIO $ openFile path mode
  liftIO $ putStrLn $ "open file " ++ show hd
  tell (MultiLayers ([hd]:repeat []))
  return (SHandle hd)

sHGetLine :: SHandle s -> SIO s String
sHGetLine (SHandle h) = do
  line <- liftIO $ hGetLine h
  liftIO $ putStrLn $ "read file " ++ show h
  return (show h ++ " " ++ line)

importSHandle :: SHandle p -> SHandle (s, p)
importSHandle (SHandle h) = SHandle h

runNestedSIO :: (forall s. SIO (s, p) v) -> SIO p v
runNestedSIO m = SafeT $ do
  (v, parentHdrs) <- liftIO $ do
    (v, MultiLayers (hdrs:parentHdrs)) <- runWriterT (unSafeT m)
    forM_ hdrs $ \h -> do
      putStrLn $ "close file " ++ show h
      hClose h
    return (v, parentHdrs)
  tell (MultiLayers parentHdrs)
  return v

liftSIO :: SIO p v -> SIO (s, p) v
liftSIO m = SafeT $ do
  (v, MultiLayers parentHdrs) <- liftIO $ runWriterT (unSafeT m)
  tell (MultiLayers ([]:parentHdrs))
  return v

---------------------------------------------------

--sampleProgram :: forall s. SIO s String
sampleProgram = do
  fileSimpleH <- sOpenFile "text1" ReadMode
  firstLine <- sHGetLine fileSimpleH

  secondLine <- runNestedSIO $ do -- case 2-, case 3-
  --(secondLine, fileIgnoreH) <- runNestedSIO $ do -- case 2+
  --(secondLine, fileOuterH) <- runNestedSIO $ do -- case 3+
    fileIgnoreH <- sOpenFile "text2" ReadMode
    secondLine <- sHGetLine fileIgnoreH

    fileOuterH <- liftSIO $ sOpenFile "text3" ReadMode
    outerLine <- sHGetLine (importSHandle fileOuterH)

    let result = secondLine ++ "\n" ++ outerLine
    return result -- case 2-, case 3-
    --return (result, importSHandle fileIgnoreH) -- case 2+
    --return (result, fileOuterH) -- case 3+

  thirdLine <- sHGetLine fileSimpleH

  let result = firstLine ++ "\n" ++ secondLine ++ "\n" ++ thirdLine
  return result

main = do
  result <- runSIO sampleProgram
  putStrLn result
