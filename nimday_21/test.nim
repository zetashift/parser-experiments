import narcec

let aParser = charParser('a') # charParsers a single letter
echo $aParser.parse("abc")

let abcParser = parseString("aoo")
echo $abcParser.parse("aooooo")
