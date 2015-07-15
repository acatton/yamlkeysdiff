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
import Data.Yaml (decodeFile, Value(Object))
import qualified Data.List as List

import qualified YamlKeysDiff.Opts as Opts
import YamlKeysDiff.Diff (diff)

decodeFile' :: FilePath -> IO Value
decodeFile' path = do
    maybeContent <- decodeFile path
    case maybeContent of
        Just content -> return content
        Nothing -> ioError $ userError $ "couldn't decode file " ++ path

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
        contentA <- decodeFile' filePathA
        contentB <- decodeFile' filePathB
        let diffLines = diff contentA contentB
        putStr $ formattingFunction diffLines
        -- FIXME: ioError also exit with 1
        exitWith $ if List.null diffLines then ExitSuccess
                   else (ExitFailure 1)
