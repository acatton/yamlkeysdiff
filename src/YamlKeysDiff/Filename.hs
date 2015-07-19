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

module YamlKeysDiff.Filename where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (eof, many1)
import Text.Parsec.Char (anyChar, noneOf, string, char)
import Text.Parsec.Error (ParseError)
import Data.Either (Either)
import Data.Char (Char)
import Control.Applicative ((<*), (<*>), (<$>))

filenameParser :: Parser (String, [String])
filenameParser =
    let backslash = try $ char '\\' >> char '\\'
        hash = try $ char '\\' >> char '#'
        colon = try $ char '\\' >> char ':'
        separator = char '#'
        fileName = many1 $ choice [noneOf "\\#", backslash, hash]
        key = sepBy (many1 $ choice [noneOf "\\:", backslash, colon]) (char ':')
    in (,) <$> fileName <*> (option [] $ separator >> key) <* eof

parseFileName :: String -> Either ParseError (String, [String])
parseFileName = parse filenameParser ""
