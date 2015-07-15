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

module YamlKeysDiff.Diff where

import qualified Data.HashMap.Lazy as HashMap
import Data.Maybe (fromMaybe)
import qualified Data.List as List
import Data.Text (unpack)
import Data.Yaml (Value(Object))

data DiffLine = DiffMissing [String]
              | DiffAdded [String]
              | DiffSimilar [String]


isSimilar :: DiffLine -> Bool
isSimilar (DiffSimilar key) = True
isSimilar _ = False


getKeys :: Value -> [[String]]
getKeys value =
    let getObject (Object obj) = Just obj
        getObject _ = Nothing
        go path obj = concat $ map (go' path) $ HashMap.toList obj
        go' path (key, value) =
            let path' = path ++ [key] in
            fromMaybe [path'] $ fmap (go path') $ getObject value
    in map (map unpack) $ fromMaybe [] $ fmap (go []) $ getObject value


diff :: Value -> Value -> [DiffLine]
diff contentA contentB =
    let keysA = List.sort $ getKeys contentA
        keysB = List.sort $ getKeys contentB

        go al@(a:at) bl@(b:bt) =
            if a == b then (DiffSimilar a) : go at bt
            else if a < b then (DiffAdded a) : (go at bl)
            else (DiffMissing b) : (go al bt)
        go [] (b:t) = (DiffMissing b) : (go [] t)
        go (a:t) [] = (DiffAdded a) : (go t [])
        go [] [] = []

    in go keysA keysB
