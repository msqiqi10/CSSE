from __future__ import absolute_import
from softwareprocess import convertString2Dictionary
import urllib
import unittest


class TestConvertString2Dictionary(unittest.TestCase):

    def test_emptyString(self):
        inputVal = urllib.quote("")
        outputVal = convertString2Dictionary.convertString2Dictionary(inputVal)
        key, value = outputVal.items()[0]
        self.assertTrue(key == 'error' and value == 'true', "empty string")

    def test_containInvalidSymbol(self):
        inputVal = urllib.quote("function = get_stars")
        outputVal = convertString2Dictionary.convertString2Dictionary(inputVal)
        key, value = outputVal.items()[0]
        self.assertTrue(key == 'error' and value == 'true', 'contain invalid symbol')

        inputVal_2 = urllib.quote("function = get*Stars")
        outputVal_2 = convertString2Dictionary.convertString2Dictionary(inputVal_2)
        key_2, value_2 = outputVal_2.items()[0]
        self.assertTrue(key_2 == "error" and value == "true", "invalid symbol*")

    def test_keyNotStartWithAlpha(self):
        inputVal = urllib.quote(".function = getStars")
        outputVal = convertString2Dictionary.convertString2Dictionary(inputVal)
        key, value = outputVal.items()[0]
        self.assertTrue(key == "error" and value == "true", "start with period")

        inputVal_2 = urllib.quote("1function = getStars")
        outputVal_2 = convertString2Dictionary.convertString2Dictionary(inputVal_2)
        key_2, value_2 = outputVal_2.items()[0]
        self.assertTrue(key_2 == "error" and value == "true", "start with digit")

    def test_spaceInKeyValue(self):
        inputVal = urllib.quote("function = va lue")
        outputVal = convertString2Dictionary.convertString2Dictionary(inputVal)
        key, value = outputVal.items()[0]
        self.assertTrue(key == "error" and value == "true", "space in value")

        inputVal_2 = urllib.quote("k ey = value ")
        outputVal_2 = convertString2Dictionary.convertString2Dictionary(inputVal_2)
        key_2, value_2 = outputVal_2.items()[0]
        self.assertTrue(key_2 == "error" and value_2 == "true", "space in key")

    def test_missOnePart(self):
        inputVal = urllib.quote("function= ")
        outputVal = convertString2Dictionary.convertString2Dictionary(inputVal)
        key, value = outputVal.items()[0]
        self.assertTrue(key == "error" and value == "true", "missing value")

        inputVal_2 = urllib.quote(" =value")
        outputVal_2 = convertString2Dictionary.convertString2Dictionary(inputVal_2)
        key_2, value_2 = outputVal_2.items()[0]
        self.assertTrue(key_2 == "error" and value_2 == "true", "missing key")


    def test_onePairValidStr(self):
        inputVal = urllib.quote("abc=123")
        outputVal = convertString2Dictionary.convertString2Dictionary(inputVal)
        key, value = outputVal.items()[0]
        self.assertTrue(key == "abc" and value == "123", 'one=pair')

    def test_twoPairValidStr(self):
        inputVal = urllib.quote("aunction= calculate, wighting=Betelgeuse")
        outputVal = convertString2Dictionary.convertString2Dictionary(inputVal)
        key = outputVal.keys()[0]
        key_2 = outputVal.keys()[1]
        self.assertTrue(key == "aunction" and outputVal[key] == "calculate", "two pairs")
        self.assertTrue(key_2 == "wighting" and outputVal[key_2] == "Betelgeuse", "two pairs")

    def test_duplicateInputStr(self):
        inputVal = urllib.quote("wong=ze,  wong= ze")
        outputVal = convertString2Dictionary.convertString2Dictionary(inputVal)
        key, value = outputVal.items()[0]
        self.assertTrue(key == "error" and value == "true", "duplicate key")

        inputVal_2 = urllib.quote("wo=ze, alex=hv., wo =ze")
        outputVal_2 = convertString2Dictionary.convertString2Dictionary(inputVal_2)
        key_2, value_2 = outputVal_2.items()[0]
        self.assertTrue(key_2 == "error" and value_2 == "true", "duplicate key")

    def test_badDelimiter(self):
        inputVal = urllib.quote("key = value = key2 =value2")
        outputVal = convertString2Dictionary.convertString2Dictionary(inputVal)
        key, value = outputVal.items()[0]
        self.assertTrue(key == "error" and value == "true", "wrong delimiter")



if __name__ == "__main__":
    unittest.main()
