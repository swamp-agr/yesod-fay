{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
-- | Utility functions for using Fay from a Yesod application.
--
-- This module is intended to be used from your Yesod application, not from
-- your Fay programs.
--
-- We assume a specific file structure, namely that there is a @fay@ folder
-- containing client-side code, and @fay-shared@ containing code to be used by
-- both the client and server.
--
-- The @Language.Fay.Yesod@ module (part of this package) is
-- required by both client and server code. However, since Fay does not
-- currently have package management support, we use a bit of a hack: the TH
-- calls in this package will automatically create the necessary
-- @fay\/Language\/Fay\/Yesod.hs@ file.  Ultimately, we will use a more elegant
-- solution.
--
-- In the future, if this package proves popular enough, Fay support will
-- likely be built into the scaffolding. In the meantime, you must manually
-- integrate it. In order to take advantage of this module, you should modify
-- your Yesod application as follows:
--
-- * Modify your @cabal@ file to include the @fay-shared@ folder when
--   compiling. This can be done by adding a @hs-source-dirs: ., fay-shared@ line
--   to your library section.
--
-- * Create the module @SharedTypes@ in @fay-shared@ and create a @Command@
--   datatype. For an example of what this file should look like, see
--   <https://github.com/snoyberg/yesod-fay/blob/master/sample/fay-shared/SharedTypes.hs>.
--
-- * Add the function @fayFile@ to your @Import@ module. See
--   <https://github.com/snoyberg/yesod-fay/blob/master/sample/Import.hs> for an
--   example.
--
-- * Add a new route at for the Fay subsite. Generally, it will look like
--   @/fay-command FaySiteR FaySite getFaySite@.
--
-- * Import the @SharedTypes@ and @Yesod.Fay@ modules into your @Foundation.hs@
--   module. Add an instance of @YesodFay@ for your application. You should set
--   the @YesodFayCommand@ associated type to the @Command@ datatype you created.
--   (You may also need to add a @YesodJquery@ instance.) Note that this instance
--   must appear after your @parseRoutes@. Set the method @fayRoute@ to
--   @FaySiteR@ (or whatever you called the subsite route), and implement
--   @yesodFayCommand@. It will generally look something like @yesodFayCommand
--   render command = case command of { ... }@.
--
-- * In order to use Fay, add @$(fayFile \"MyModule\")@ to a widget, and then
--   write the corresponding @fay/MyModule.hs@ file. For an example, see
--   <https://github.com/snoyberg/yesod-fay/blob/master/sample/fay/Home.hs>.
module Yesod.Fay
    ( -- * Typeclass
      YesodFay (..)
    , YesodFaySettings (..)
    , yesodFaySettings
      -- * Include Fay programs
    , fayFileProd
    , fayFileReload
    , FayFile
      -- * Commands
    , CommandHandler
    , Returns
      -- * Subsite
    , FaySite
    , getFaySite
    , Route (..)
      -- * Reexports
    , YesodJquery (..)
    ) where

import           Control.Monad              (unless, when)
import           Control.Monad.Loops        (anyM)
import           Control.Applicative
import           Data.Aeson                 (decode)
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.UTF8  as BSU
import           Data.Data                  (Data)
#if !MIN_VERSION_fay(0,20,0)
import           Data.Default               (def)
#endif
import           Data.Digest.Pure.MD5       (md5)
import           Data.List                  (isPrefixOf)
import           Data.Maybe                 (isNothing)
import           Data.Monoid                ((<>), mempty)
import           Data.Text                  (pack, unpack)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import qualified Data.Text.Lazy             as TL
import           Data.Text.Encoding         (encodeUtf8)
import qualified Data.Text.Lazy.Encoding as TLE
import           Data.Text.Lazy.Builder     (fromText, toLazyText, Builder)
import           Filesystem                 (createTree, isFile, readTextFile)
import           Filesystem.Path.CurrentOS  (directory, encodeString, decodeString)
import           Fay                        (getRuntime, showCompileError)
import           Fay.Convert                (showToFay)
#if MIN_VERSION_fay(0,20,0)
import           Fay                        (Config(..),
                                             addConfigDirectoryIncludePaths,
                                             addConfigPackages,
                                             compileFileWithResult,
                                             configDirectoryIncludes,
                                             configTypecheck,
                                             configExportRuntime,
                                             configPrettyPrint,
                                             defaultConfig,
                                             CompileError)
import           Fay.Types.CompileResult    (CompileResult (..))
#else
import           Fay                        (CompileState(..), compileFileWithState)
import           Fay.Compiler.Config        (addConfigDirectoryIncludePaths,
                                             addConfigPackages)
import           Fay.Types                  (CompileConfig(..),
                                             configDirectoryIncludes,
                                             configTypecheck,
                                             configExportRuntime,
                                             configPrettyPrint,
                                             CompileError)
#endif
import           Language.Fay.Yesod         (Returns (Returns))
import           Language.Haskell.TH.Syntax (Exp (LitE, AppE, VarE), Lit (StringL, StringPrimL, IntegerL),
                                             Q,
                                             qAddDependentFile, qRunIO)
import           Data.Text.Encoding (decodeUtf8)
import           Data.ByteString.Unsafe (unsafePackAddressLen)
import           Control.Exception (IOException,catch)
import           Prelude hiding (catch)
import           System.Directory
import           System.Environment         (getEnvironment)
import           Text.Julius                (Javascript (Javascript), julius)
import           Yesod.Core
import           Yesod.Fay.Data
import           Yesod.Form.Jquery          (YesodJquery (..))
import           Yesod.Static

-- | Type class for applications using Fay.
--
-- We depend on @YesodJquery@ since the generated client-side code uses jQuery
-- for making Ajax calls. We have an associated type stating the command
-- datatype. Since this datatype must be used by both the client and server,
-- you should place its definition in the @fay-shared@ folder.
class YesodJquery master => YesodFay master where
    -- | User-defined function specifying how to respond to commands. Using the
    -- above datatype, this might look like:
    --
    -- > yesodFayCommand render command =
    -- >     case command of
    -- >         GetFib index r = render r $ fibs !! index
    yesodFayCommand :: CommandHandler master

    -- | Where in the routing tree our Fay subsite is located. This is
    -- generally named @FaySiteR@, e.g.:
    --
    -- > mkYesod "YourSite" [parseRoutes
    -- > ...
    -- > /fay-command FaySiteR FaySite getFaySite
    -- > |]
    -- >
    -- > instance YesodFay YourSite where
    -- >     fayRoute = FaySiteR
    fayRoute :: Route FaySite -> Route master

    -- | User-defined function specifying how to encode data as json for fay.
    --
    --   Most users won't need to define this, the default is @const showToFay@.
    --   Custom definitions will usually be in terms of 'encodeFay'.
    fayEncode :: (Show a, Data a) => master -> a -> Maybe Value
    fayEncode = const showToFay

-- | A function provided by the developer describing how to answer individual
-- commands from client-side code.
--
-- Due to restrictions of the type system in Fay, we use a relatively simple
-- approach for encoding the return type. In order to specify this, an extra
-- parameter- @Returns@- is passed around, with a phantom type variable stating
-- the expected return type.
--
-- The first argument to your function is the \"respond\" function: it takes
-- the extra @Returns@ parameter as well as the actual value to be returned,
-- and produces the expected result.
type CommandHandler master
    = forall s.
      (forall a. (Show a, Data a) => Returns a -> a -> HandlerT master IO s)
   -> Value
   -> HandlerT master IO s

-- | A setttings data type for indicating whether the generated Javascript
-- should contain a copy of the Fay runtime or not.
data YesodFaySettings = YesodFaySettings
    { yfsModuleName      :: String
    , yfsSeparateRuntime :: Maybe (FilePath, Exp)
    , yfsPostProcess     :: String -> IO String
    , yfsExternal        :: Maybe (FilePath, Exp)
    , yfsRequireJQuery   :: Bool
    -- ^ Note that the server call functions provided for your Fay code require
    -- jQuery to be present. If you disable this option and still use the
    -- provided server call functions, your code will break.
    , yfsPackages        :: [String]
    , yfsTypecheckDevel  :: Bool
    -- ^ Perform typechecking when in devel mode?
    --
    -- Default: False
    }

yesodFaySettings :: String -> YesodFaySettings
yesodFaySettings moduleName = YesodFaySettings
    { yfsModuleName = moduleName
    , yfsSeparateRuntime = Nothing
    , yfsPostProcess = return
    , yfsExternal = Nothing
    , yfsRequireJQuery = True
    , yfsPackages = ["fay-base"]
    , yfsTypecheckDevel = False
    }

updateRuntime :: FilePath -> IO ()
updateRuntime fp = getRuntime >>= \js -> createTree (directory $ decodeString fp) >> copyFile js fp

instance YesodFay master => YesodSubDispatch FaySite (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesFaySite)

-- | To be used from your routing declarations.
getFaySite :: a -> FaySite
getFaySite _ = FaySite

postFayCommandR :: forall master. YesodFay master => HandlerT FaySite (HandlerT master IO) Value
postFayCommandR =
    lift $ runCommandHandler yesodFayCommand
  where
    -- | Run a command handler. This provides server-side responses to Fay queries.
    runCommandHandler :: YesodFay master
                      => CommandHandler master
                      -> HandlerT master IO Value
    runCommandHandler f = do
        mtxt <- lookupPostParam "json"
        case mtxt of
            Nothing -> invalidArgs ["No JSON provided"]
            Just txt ->
                case decode (L.fromChunks [encodeUtf8 txt]) of
                    Nothing -> error $ "Unable to parse input: " ++ show txt
                    Just cmd -> f go cmd
      where
        go Returns value = do
          master <- getYesod
          returnJson $ fayEncode master value -- FIXME what should we do for Nothing values?

langYesodFay :: String
langYesodFay = $(qRunIO $ fmap (LitE . StringL . unpack) $ readTextFile "Language/Fay/Yesod.hs")

writeYesodFay :: IO ()
writeYesodFay = do
    let fp = "fay/Language/Fay/Yesod.hs"
        content = "-- NOTE: This file is auto-generated.\n" ++ langYesodFay
    exists <- isFile fp
    mcurrent <-
        if exists
            then fmap (Just . unpack) $ readTextFile fp
            else return Nothing
    unless (mcurrent == Just content) $ do
        createTree $ directory fp
        writeFile (encodeString fp) content

maybeRequireJQuery :: YesodFay master => Bool -> WidgetT master IO ()
maybeRequireJQuery needJQuery = when needJQuery requireJQuery

requireJQuery :: YesodFay master => WidgetT master IO ()
requireJQuery = do
    master <- getYesod
    addScriptEither $ urlJqueryJs master
    render <- getUrlRender
    -- FIXME get rid of toLazyText call below
    toWidgetHead [julius|window.yesodFayCommandPath = #{toJSON $ render $ fayRoute FayCommandR};|]

mkfp :: String -> FilePath
mkfp name = "fay/" ++ name ++ ".hs"

requireFayRuntime :: YesodFaySettings -> Q Exp
requireFayRuntime settings = do
    maybe (return ())
        (\(path,_) -> qRunIO $ updateRuntime (path ++ "/fay-runtime.js"))
        (yfsSeparateRuntime settings)
    case yfsSeparateRuntime settings of
        Nothing -> [| return () |]
        Just (_, exp') -> do
            hash <- qRunIO $ getRuntime >>= fmap base64md5 . L.readFile
            [| addScript ($(return exp') (StaticRoute ["fay-runtime.js"] [(T.pack hash, "")])) |]

-- | A function that takes a String giving the Fay module name, and returns an
-- TH splice that generates a @Widget@.
type FayFile = String -> Q Exp

-- | Compile a Fay file or return the cached version if no compile is
-- necessary.
compileFayFile :: FilePath
               -> Config
               -> IO (Either CompileError ([FilePath],String))
compileFayFile fp conf = do
  result <- getFileCache fp
  case result of
    Right cache -> return (Right cache)
    Left refreshTo -> do
      packageConf <- fmap (lookup "HASKELL_PACKAGE_SANDBOX") getEnvironment
      result <- compile conf {configPackageConf = packageConf}
                        fp
      case result of
        Left e -> return (Left e)
        Right (sourceAndFiles -> (source',files)) -> do
          let fps = filter ours files
              source = "\n(function(){\n" ++ source' ++ "\n})();\n"
              (fp_hi,fp_o) = refreshTo
          writeFile fp_hi (unlines fps)
          writeFile fp_o source
          return (Right (fps,source))
  where ours x = isPrefixOf "fay/" x || isPrefixOf "fay-shared/" x

-- | Return the cached output file of the given source file, if:
--
--  1. The source file hasn't changed.
--  2. Any of the source file's dependencies haven't changed.
--
--  Otherwise, return filepaths needed to store meta data and the new cache.
--
getFileCache :: FilePath -> IO (Either (FilePath,FilePath) ([FilePath],String))
getFileCache fp = do
  let dir = "dist/yesod-fay-cache/"
      guid = show (md5 (BSU.fromString fp))
      fp_hi = dir ++ guid ++ ".hi"
      fp_o = dir ++ guid ++ ".o"
      refresh = return $ Left (fp_hi,fp_o)
  createDirectoryIfMissing True dir
  catch (do thisModTime <- getModificationTime fp_o
            modules <- fmap ((fp :) . lines . T.unpack) (T.readFile fp_hi)
            changed <- anyM (fmap (> thisModTime) . getModificationTime) modules
            if changed
               then refresh
               else fmap (Right . (modules,) . T.unpack) (T.readFile fp_o))
        -- If any IO exceptions occur at this point, just invalidate the cache.
        (\(_ :: IOException) -> refresh)

-- | Does a full compile of the Fay code via GHC for type checking, and then
-- embeds the Fay-generated Javascript as a static string. File changes during
-- runtime will not be reflected.
fayFileProd :: YesodFaySettings -> Q Exp
fayFileProd settings = do
    let needJQuery = yfsRequireJQuery settings
    qAddDependentFile fp
    qRunIO writeYesodFay
    eres <- qRunIO $ compileFayFile fp
                   $ addConfigPackages packages
                   $ config { configExportRuntime = exportRuntime }
    case eres of
        Left e -> throwFayError name e
        Right (modules,s) -> do
            mapM_ qAddDependentFile modules
            s' <- qRunIO $ yfsPostProcess settings s
            let contents = fromText (pack s')

            case yfsExternal settings of
                Nothing ->
                    [| do
                        maybeRequireJQuery needJQuery
                        $(requireFayRuntime settings)
                        let bs =
                                  $(do
                                      let lt = toLazyText contents
                                          lenE = LitE $ IntegerL $ fromIntegral $ TL.length lt
                                          strE = LitE $ StringPrimL $ TL.unpack lt
                                          packer = VarE 'unsafePackAddressLen
                                      return $ packer `AppE` lenE `AppE` strE)
                        toWidget $ const $ Javascript $ fromText $ decodeUtf8 bs
                    |]
                Just (fp', exp') -> do
                    let name' = concat ["faygen-", hash, ".js"]
                        hash = base64md5 contents'
                        contents' = TLE.encodeUtf8 $ toLazyText contents
                    qRunIO $ L.writeFile (concat [fp', "/", name']) contents'
                    [| do
                        maybeRequireJQuery needJQuery
                        $(requireFayRuntime settings)
                        addScript $ $(return exp') $ StaticRoute [pack name'] []
                        |]
  where
    name = yfsModuleName settings
    exportRuntime = isNothing (yfsSeparateRuntime settings)
    packages = yfsPackages settings
    fp = mkfp name

config :: Config
config = addConfigDirectoryIncludePaths ["fay", "fay-shared"]
#if MIN_VERSION_fay(0,20,0)
    defaultConfig
#else
    def
#endif

-- | Performs no type checking on the Fay code. Each time the widget is
-- requested, the Fay code will be compiled from scratch to Javascript.
fayFileReload :: YesodFaySettings -> Q Exp
fayFileReload settings = do
    let needJQuery = yfsRequireJQuery settings
    qRunIO writeYesodFay
    [|
        liftIO (compileFayFile (mkfp name)
          $ addConfigPackages packages
          $ config
                { configTypecheck = typecheckDevel
                , configExportRuntime = exportRuntime
#if MIN_VERSION_fay(0, 19, 0)
                , configSourceMap = True
#endif
                , configPrettyPrint = True
                })
                >>= \eres -> do
        (case eres of
              Left e -> throwFayError name e
              Right (_,s) -> do
                maybeRequireJQuery needJQuery
                $(requireFayRuntime settings)
                toWidget (const $ Javascript $ fromText (pack s)))|]
  where
    name = yfsModuleName settings
    exportRuntime = isNothing (yfsSeparateRuntime settings)
    packages = yfsPackages settings
    typecheckDevel = yfsTypecheckDevel settings

-- | Throw a fay error.
throwFayError :: String -> CompileError -> error
throwFayError name e =
  error $ "Unable to compile Fay module \"" ++ name ++ "\":\n\n" ++ showCompileError e



-- Fay cross-version compatible functions

#if !MIN_VERSION_fay(0,20,0)
type Config = CompileConfig
#endif

#if MIN_VERSION_fay(0,20,0)
compile = compileFileWithResult
#else
compile = compileFileWithState
#endif

#if MIN_VERSION_fay(0,20,0)
sourceAndFiles (source,_,res) = (source,map snd (resImported res))
#else
#if MIN_VERSION_fay(0,18,0)
sourceAndFiles (source,_,state) = (source,map snd (stateImported state))
#else
sourceAndFiles (source,state) = (source,map snd (stateImported state))
#endif
#endif
