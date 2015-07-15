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

module YamlKeysDiff.Formatting where

import qualified Data.List as List

import YamlKeysDiff.Diff (DiffLine(DiffMissing, DiffAdded, DiffSimilar), isSimilar)

normalFormatting :: [DiffLine] -> String
normalFormatting lines =
    -- FIXME: The formatKey thing is horrible
    let formatKey key = List.intercalate ":" key
        go (DiffMissing key) = "< " ++ (formatKey key)
        go (DiffAdded key) = "> " ++ (formatKey key)
    in unlines $ map go $ filter (not . isSimilar) lines

sideBySideWidth :: Int
sideBySideWidth = 79

sideBySideFormatting :: [DiffLine] -> String
sideBySideFormatting lines =
    let pad width string =
            let len = List.length string in
            if len < width then
                string ++ (List.take (width - len) (List.repeat ' '))
            else if len > width then
                List.take width string
            else string
        fmt (columnA, columnB) c =
            let columnWidth = (sideBySideWidth - 3) `div` 2
                -- FIXME: The intercalate thing is horrible
                columnA' = List.intercalate ":" columnA
                columnB' = List.intercalate ":" columnB
            in (pad columnWidth columnA') ++ [' ', c, ' '] ++ (pad columnWidth columnB')

        go (DiffMissing key) = fmt ([], key) '<'
        go (DiffAdded key) = fmt (key, []) '>'
        go (DiffSimilar key) = fmt (key, key) '|'

    in unlines $ map go lines


unifiedFormatting lines =
    -- FIXME: The format key thing is horrible
    let formatKey = List.intercalate ":"
        go (DiffMissing key) = "- " ++ (formatKey key)
        go (DiffAdded key) = "+ " ++ (formatKey key)
    in unlines $ map go $ filter (not . isSimilar) lines
