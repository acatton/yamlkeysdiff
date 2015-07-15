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

module YamlKeysDiff.Opts where

import System.Console.GetOpt
import Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromJust)

import qualified YamlKeysDiff.Formatting as Formatting
import YamlKeysDiff.Diff (DiffLine)


data Flag = Copied
          | Help
          | IgnoreCase
          | Normal
          | SideBySide
          | Unified
          | Version
                deriving (Show, Ord, Eq)

options :: [OptDescr Flag]
options =
    [ Option ['i'] ["ignore-case"]  (NoArg IgnoreCase) "ignore keys' case"
    , Option ['v'] ["version"]      (NoArg Version)    "show version number"
    , Option ['u'] ["unified"]      (NoArg Unified)    "unified diff"
    , Option ['y'] ["side-by-side"] (NoArg SideBySide) "side by side diff"
    , Option ['n'] ["normal"]       (NoArg Normal)     "normal diff"
    -- FIXME: This is not how it's supposed to be done
    , Option ['h'] ["help"]         (NoArg Help)       "display this message"
    ]

getFileArgs :: [String] -> IO (String, String)
getFileArgs args =
    case args of
        [a, b] -> return (a, b)
        _ -> ioError (userError "You need to specify two files to compute the diff from")


getOptions :: [String] -> IO ([Flag], [String])
getOptions argv =
    case getOpt Permute options argv of
        (o, n, []) -> return (o, n)
        (_, _, errs) ->
            ioError (userError (concat errs ++ usageInfo header options))
                where header = "Usage: yamlkeysdiff [OPTION...] FILES..."


defaultFormattingFlag :: Flag
defaultFormattingFlag = Normal

formattingFunctions = Map.fromList
    [ (Normal, Formatting.normalFormatting)
    , (SideBySide, Formatting.sideBySideFormatting)
    , (Unified, Formatting.unifiedFormatting)
    ]

getFormattingFunction :: [Flag] -> IO ([DiffLine] -> String)
getFormattingFunction flags =
    let formattingFlags = Map.keysSet formattingFunctions
        flags' = Set.fromList flags
        specifiedFormattingFlags = Set.intersection flags' formattingFlags
        getFunction flag = fromJust $ Map.lookup flag formattingFunctions
    in if Set.null specifiedFormattingFlags then
            return $ getFunction $ defaultFormattingFlag
       else if Set.size specifiedFormattingFlags == 1 then
            return $ getFunction $ Set.elemAt 0 specifiedFormattingFlags
       else ioError (userError "Multiple format flags specified")
