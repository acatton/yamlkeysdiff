-- Copyright (c) 2015 Antoine Catton
--
-- This file is part of YAMLKeysDiff and is distributed under EUPLv1.1,
-- see the LICENSE file for more information. If a LICENSE file wasn't
-- provided with this piece of software, you will find a copy at:
-- <http://ec.europa.eu/idabc/eupl.html>
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
-- THE SOFTWARE.

import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, ExitCode(ExitSuccess, ExitFailure))
import qualified Data.Yaml as Yaml
import qualified Data.List as List
import qualified Data.Either as Either
import qualified Data.HashMap.Lazy as HashMap
import Control.Monad (foldM)
import Data.Text (pack)

import qualified YamlKeysDiff.Opts as Opts
import YamlKeysDiff.Filename (parseFileName)
import YamlKeysDiff.Diff (diff, isSimilar)

decodeFilename :: String -> IO (FilePath, [String])
decodeFilename fileName =
    case parseFileName fileName of
        Either.Right a -> return a
        Either.Left e ->
            ioError $ userError $ "couldn't parse filename " ++ fileName


decodeFile :: FilePath -> IO Yaml.Value
decodeFile path = do
    (fileName, keys) <- decodeFilename path
    maybeContent <- Yaml.decodeFile fileName
    content <- case maybeContent of
        Just c -> return c
        Nothing -> ioError $ userError $ "couldn't decode file " ++ fileName
    case getValue keys content of
        Just v -> return v
        Nothing -> ioError $ userError $ "couldn't find the key " ++ List.intercalate ":" keys

getValue :: [String] -> Yaml.Value -> Maybe Yaml.Value
getValue keys value =
    let getKey key value =
            case value of
                Yaml.Object obj -> HashMap.lookup key obj
                _ -> Nothing
    in foldM (flip getKey) value $ map pack keys

getFiles :: [String] -> IO (String, String)
getFiles args =
    case args of
        [a, b] -> return (a, b)
        _ -> ioError (userError "Please specify two files")

main :: IO ()
main = do
    progName <- getProgName
    argv <- getArgs
    (flags, args) <- Opts.getOptions progName argv
    if List.elem Opts.Help flags then
        putStr $ Opts.usage progName
    else do
        formattingFunction <- Opts.getFormattingFunction flags
        (filePathA, filePathB) <- getFiles args
        contentA <- decodeFile filePathA
        contentB <- decodeFile filePathB
        let diffLines = diff contentA contentB
        putStr $ formattingFunction diffLines
        -- FIXME: ioError also exit with 1
        exitWith $ if all isSimilar diffLines then ExitSuccess
                   else (ExitFailure 1)
