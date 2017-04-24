from unittest import TestCase
from math import *
from softwareprocess.dispatch import dispatch
from softwareprocess.OPpredict import *
from softwareprocess.OPcorrect import *


class TestDispatch(TestCase):
    # -----------------------------------------------------------------------
    # ---- Acceptance Tests
    # 100 constructor
    # desired acceptance of dispatch:
    # observation is a mandatory parameter of observation xdy.y where x is positive integer >=0 and <=90
    # and y.y is a float .GE. 0.0 and .LT. 60.0.
    # Height is a string of a numeric value .GE. 0. Optional, deaults to '0' if missing.
    # Temperature is a string of an integer in the range .GE. -20 and .LE. 120.  Optional, defaults to '72' if missing.
    # Pressure is a string of an integer in the range .GE. 100 and .LE. 1100.  Optional, defaults to '1010' if missing.
    # Horizon is one of the following case-insensitive strings:  "artificial" or "natural".
    # Optional, defaults to "natural" if missing.

    def test_dispatch100_000_emptyInput(self):
        inputVal = {}
        returnedValue = dispatch(inputVal)
        self.assertTrue(returnedValue == {'error': 'no op is specified'})

    def test_dispatch100_001_observationMissing(self):
        inputVal = {'op': 'adjust'}
        returnedValue = dispatch(inputVal)
        self.assertTrue(returnedValue == {'error': 'mandatory information observation missing'})

    def test_dispatch100_002_observationValueIllegal(self):
        inputVal = {'op': 'adjust', 'observation': '123321'}
        returnedValue = dispatch(inputVal)
        inputVal['error'] = 'value of observation is illegal'
        self.assertTrue(returnedValue == inputVal)

    def test_dispatch100_003_observationValueIllegal(self):
        inputVal = {'op': 'adjust', 'observation': '180d123.4'}
        returnedValue = dispatch(inputVal)
        inputVal['error'] = 'degree of observation value is illegal'
        self.assertTrue(returnedValue == inputVal)

    def test_dispatch100_004_observationValueIllegal(self):
        inputVal = {'op': 'adjust', 'observation': '45d123.4'}
        returnedValue = dispatch(inputVal)
        inputVal['error'] = 'minute of observation value is illegal'
        self.assertTrue(returnedValue == inputVal)

    def test_dispatch100_005_observationValueIllegal(self):
        inputVal = {'op': 'adjust', 'observation': '45d123.4'}
        returnedValue = dispatch(inputVal)
        inputVal['error'] = 'minute of observation value is illegal'
        self.assertTrue(returnedValue == inputVal)

    def test_dispatch100_006_heightValueIllegal(self):
        inputVal = {'op': 'adjust', 'observation': '45d23.4', 'height': '-9'}
        returnedValue = dispatch(inputVal)
        inputVal['error'] = 'value of height is illegal'
        self.assertTrue(returnedValue == inputVal)

    def test_dispatch100_007_tempValueIllegal(self):
        inputVal = {'op': 'adjust', 'observation': '45d23.4', 'height': '4', 'temperature': '140'}
        returnedValue = dispatch(inputVal)
        inputVal['error'] = 'value of temperature is illegal'
        self.assertTrue(returnedValue == inputVal)

    def test_dispatch100_008_tempValueIllegal(self):
        inputVal = {'op': 'adjust', 'observation': '45d23.4', 'height': '4', 'temperature': '-70'}
        returnedValue = dispatch(inputVal)
        inputVal['error'] = 'value of temperature is illegal'
        self.assertTrue(returnedValue == inputVal)

    def test_dispatch100_009_horizonValueIllegal(self):
        inputVal = {'op': 'adjust', 'observation': '45d23.4', 'height': '7', 'temperature': '70',
                    'horizon': 'not artificial'}
        returnedValue = dispatch(inputVal)
        inputVal['error'] = 'value of horizon is illegal'
        self.assertTrue(returnedValue == inputVal)

    def test_dispatch100_010_givenCase1(self):
        inputVal = {'observation': '101d15.2', 'height': '6', 'pressure': '1010', 'horizon': 'natural', 'op': 'adjust', 'temperature': '71'}
        returnedValue = dispatch(inputVal)
        inputVal['error'] = 'degree of observation value is illegal'
        self.assertTrue(returnedValue == inputVal)

    def test_dispatch100_011_givenCase2(self):
        inputVal = {'observation': '45d15.2', 'height': 'a', 'pressure': '1010', 'horizon': 'natural', 'op': 'adjust', 'temperature': '71'}
        returnedValue = dispatch(inputVal)
        inputVal['error'] = 'cast error'
        self.assertTrue(returnedValue == inputVal)

    def test_dispatch100_012_givenCase3(self):
        inputVal = {'observation': '45d15.2', 'height': '6', 'horizon': '   ', 'pressure': '1010', 'op': 'adjust', 'temperature': '71'}
        returnedValue = dispatch(inputVal)
        inputVal['error'] = 'value of horizon is illegal'
        self.assertTrue(returnedValue == inputVal)

    def test_dispatch100_013_givenCase4(self):
        inputVal = None
        returnedValue = dispatch(inputVal)
        self.assertTrue(returnedValue == {'error':'parameter is missing'})

    def test_dispatch100_014_happyPathTest(self):
        inputVal = {'op': 'adjust', 'observation': '45d23.4', 'height': '7', 'temperature': '70',
                    'horizon': 'artificial'}
        returnedValue = dispatch(inputVal)
        self.assertTrue(returnedValue == inputVal)

    # -----------------------------------------------------------------------
    # ---- op: adjust Tests
    # 200 constructor:
    # calculate the altitude value to be returned
    # always happy test

    def test_calculateAdjsut200_001_happyPath(self):
        inputVal = {'observation': '30d1.5', 'height': '19.0', 'pressure': '1000', 'horizon': 'artificial', 'op': 'adjust', 'temperature': '85'}
        returnedDict = dispatch(inputVal)
        desiredOutput = {'altitude':'29d59.9', 'observation': '30d1.5', 'height': '19.0', 'pressure': '1000', 'horizon': 'artificial', 'op': 'adjust', 'temperature': '85'}
        self.assertTrue(returnedDict == desiredOutput)

    def test_calculateAdjsut200_002_happyPath(self):
        inputVal = {'observation': '45d15.2', 'height': '6', 'pressure': '1010', 'horizon': 'natural', 'op': 'adjust', 'temperature': '71'}
        returnedDict = dispatch(inputVal)
        desiredOutput = {'altitude':'45d11.9', 'observation': '45d15.2', 'height': '6', 'pressure': '1010', 'horizon': 'natural', 'op': 'adjust', 'temperature': '71'}
        self.assertTrue(returnedDict == desiredOutput)

    def test_calculateAdjsut200_003_happyPath(self):
        inputVal = {'observation': '42d0.0',  'op': 'adjust'}
        returnedDict = dispatch(inputVal)
        desiredOutput = {'altitude':'41d59.0', 'observation': '42d0.0',  'op': 'adjust'}
        self.assertTrue(returnedDict == desiredOutput)

    def test_calculateAdjsut200_004_happyPath(self):
        inputVal = {'observation': '42d0.0',  'op': 'adjust', 'extraKey':'ignore'}
        returnedDict = dispatch(inputVal)
        desiredOutput = {'altitude':'41d59.0', 'observation': '42d0.0',  'op': 'adjust', 'extraKey':'ignore'}
        self.assertTrue(returnedDict == desiredOutput)

    # -----------------------------------------------------------------------
    # ---- Acceptance test
    # 300 constructor:
    # op is "predict", mandatory
    # body is mandatory, and is a star's name, if the star's name isnt in the table, report an error
    # date is a string in yyyy-mm-dd format, where yyyy is .GE. 2001.  Optional, defaults to "2001-01-01" if missing.
    # time is a string in hh:mm:ss  format.   Optional, defaults to "00:00:00" if missing.

    def test_calculatePredict300_001_missingOpPredict(self):
        inputVal = {}
        returnedDict = dispatch(inputVal)
        desiredOutput = {'error' : 'no op is specified'}
        self.assertTrue(returnedDict == desiredOutput)

    def test_calculatePredict300_002_missingBody(self):
        inputVal = {'op': 'predict'}
        returnedDict = dispatch(inputVal)
        desiredOutput = {'error':'mandatory information body missing', 'op': 'predict'}
        self.assertTrue(returnedDict == desiredOutput)

    def test_calculatePredict300_003_bodyNotInTable(self):
        inputVal = {'op': 'predict', 'body' : 'lalala'}
        returnedDict = dispatch(inputVal)
        desiredOutput = {'error':'star not in catalog', 'body' : 'lalala', 'op' : 'predict'}
        self.assertTrue(returnedDict == desiredOutput)

    def test_calculatePredict300_004_dateFormatIllegal(self):
        inputVal = {'op': 'predict', 'body' : 'Akamar', 'date' : 'randomString'}
        returnedDict = dispatch(inputVal)
        desiredOutput = {'error':'date value is illegal','date' : 'randomString', 'body' : 'Akamar', 'op' : 'predict'}
        self.assertTrue(returnedDict == desiredOutput)

    def test_calculatePredict300_005_dateValueIllegal(self):
        inputVal = {'op': 'predict', 'body' : 'Akamar', 'date' : '1992-12-12'}
        returnedDict = dispatch(inputVal)
        desiredOutput = {'error':'date value is illegal','date' : '1992-12-12', 'body' : 'Akamar', 'op' : 'predict'}
        self.assertTrue(returnedDict == desiredOutput)

    def test_calculatePredict300_006_dateValueIllegal(self):
        inputVal = {'op': 'predict', 'body' : 'Akamar', 'date' : '2002-99-12'}
        returnedDict = dispatch(inputVal)
        desiredOutput = {'error':'date value is illegal','date' : '2002-99-12', 'body' : 'Akamar', 'op' : 'predict'}
        self.assertTrue(returnedDict == desiredOutput)

    def test_calculatePredict300_006_dateValueIllegal(self):
        inputVal = {'op': 'predict', 'body' : 'Akamar', 'date' : '2002-02-30'}
        returnedDict = dispatch(inputVal)
        desiredOutput = {'error':'date value is illegal','date' : '2002-02-30', 'body' : 'Akamar', 'op' : 'predict'}
        self.assertTrue(returnedDict == desiredOutput)

    def test_calculatePredict300_007_dateValueIllegal(self):
        inputVal = {'op': 'predict', 'body' : 'Akamar', 'date' : '2004-02-30'}
        returnedDict = dispatch(inputVal)
        desiredOutput = {'error':'date value is illegal','date' : '2004-02-30', 'body' : 'Akamar', 'op' : 'predict'}
        self.assertTrue(returnedDict == desiredOutput)

    def test_calculatePredict300_008_dateValueIllegal(self):
        inputVal = {'op': 'predict', 'body' : 'Akamar', 'date' : '2002-11-31'}
        returnedDict = dispatch(inputVal)
        desiredOutput = {'error':'date value is illegal','date' : '2002-11-31', 'body' : 'Akamar', 'op' : 'predict'}
        self.assertTrue(returnedDict == desiredOutput)

    # def test_calculatePredict300_009_dateHappyPath(self):
    #     inputVal = {'op': 'predict', 'body' : 'Akamar', 'date' : '2002-02-15'}
    #     returnedDict = dispatch(inputVal)
    #     print(returnedDict)
    #     desiredOutput = {'date' : '2002-02-15', 'body' : 'Akamar', 'op' : 'predict'}
    #     self.assertTrue(returnedDict == desiredOutput)

    # -----------------------------------------------------------------------
    # ---- GHA calculation test
    # 400 constructor:
    # always happy path

    def test_calculateGHA400_001_dateHappyPath(self):
        inputVal = {'time' : '03:15:42','date' : '2016-01-17'}
        returnedDict = calculateGHA(inputVal)
        desiredOutput = '164d54.6'
        self.assertTrue(returnedDict == desiredOutput)

    # -----------------------------------------------------------------------
    # ---- Adjust calculation test
    # 500 constructor:
    # Given cases

    def test_calculatePredict500_001_HappyPath(self):
        inputVal = {'op' : 'predict', 'body' : 'Betelgeuse', 'time' : '03:15:42','date' : '2016-01-17'}
        returnedDict = dispatch(inputVal)
        desiredOutput = {'body': 'Betelgeuse', 'long': '75d53.7', 'lat': '7d24.3', 'time':
            '03:15:42', 'date': '2016-01-17', 'op': 'predict'}
        self.assertTrue(returnedDict == desiredOutput)

    def test_calculatePredict500_002_HappyPath(self):
        inputVal = {}
        returnedDict = dispatch(inputVal)
        desiredOutput = {'error':'no op is specified'}
        self.assertTrue(returnedDict == desiredOutput)

    def test_calculatePredict500_003_HappyPath(self):
        inputVal = {'op':'predict', 'body': 'unknown', 'date': '2016-01-17', 'time': '03:15:42'}
        returnedDict = dispatch(inputVal)
        desiredOutput = {'op':'predict', 'body': 'unknown', 'date': '2016-01-17', 'time': '03:15:42', 'error':'star not in catalog'}
        self.assertTrue(returnedDict == desiredOutput)

    def test_calculatePredict500_004_HappyPath(self):
        inputVal = {'op': 'predict', 'body': 'Betelgeuse', 'date': '2016-99-17', 'time': '03:15:42'}
        returnedDict = dispatch(inputVal)
        desiredOutput = {'op':'predict', 'body': 'Betelgeuse', 'date': '2016-99-17', 'time': '03:15:42', 'error':'date value is illegal'}
        self.assertTrue(returnedDict == desiredOutput)

    def test_calculatePredict500_005_HappyPath(self):
        inputVal = {'op': 'predict', 'body': 'Betelgeuse', 'date': '2016-01-17', 'time': '03:15:99'}
        returnedDict = dispatch(inputVal)
        desiredOutput = {'op':'predict', 'body': 'Betelgeuse', 'date': '2016-01-17', 'time': '03:15:99', 'error':'time value is illegal'}
        self.assertTrue(returnedDict == desiredOutput)

    # def test_calculatePredict500_006_HappyPath(self):
    #     inputVal = {'op': 'correct'}
    #     returnedDict = dispatch(inputVal)
    #     desiredOutput = {'op':'correct'}
    #     self.assertTrue(returnedDict == desiredOutput)

    def test_calculatePredict500_007_HappyPath(self):
        inputVal = {'op': 'locate'}
        returnedDict = dispatch(inputVal)
        desiredOutput = {'op':'locate'}
        self.assertTrue(returnedDict == desiredOutput)

    def test_calculatePredict500_008_opIllegal(self):
        inputVal = {'op': 'lalala'}
        returnedDict = dispatch(inputVal)
        desiredOutput = {'op': 'lalala', 'error': 'op is not a legal operation'}
        self.assertTrue(returnedDict == desiredOutput)

    def test_calculatePredict500_009_longContainedInInput(self):
        inputVal = {'op': 'predict', 'body': 'Betelgeuse', 'date': '2016-01-17', 'time': '03:15:15', 'long': 'whatever'}
        returnedDict = dispatch(inputVal)
        desiredOutput = {'op': 'predict', 'body': 'Betelgeuse', 'date': '2016-01-17', 'time': '03:15:15', 'long': 'whatever', 'error': 'input contains key lat or long'}
        self.assertTrue(returnedDict == desiredOutput)

    def test_calculatePredict500_009_latContainedInInput(self):
        inputVal = {'op': 'predict', 'body': 'Betelgeuse', 'date': '2016-01-17', 'time': '03:15:15', 'lat': 'whatever'}
        returnedDict = dispatch(inputVal)
        desiredOutput = {'op': 'predict', 'body': 'Betelgeuse', 'date': '2016-01-17', 'time': '03:15:15', 'lat': 'whatever', 'error': 'input contains key lat or long'}
        self.assertTrue(returnedDict == desiredOutput)


    # -----------------------------------------------------------------------
    # ---- function to validate input parameter of correct test
    # 600 constructor:
    # not a str, string not in 'xdyy.y' format, and happypath test

    def test_inputStrCheck600_001_inputNoString(self):
        inputVal = 24
        output = inputStrCheck(inputVal)
        self.assertTrue(output == False)

    def test_inputStrCheck600_002_IllegalInputFormat(self):
        inputVal = '87.3'
        output = inputStrCheck(inputVal)
        self.assertTrue(output == False)

    def test_inputCheck600_00_HappyPath(self):
        inputVal = '43d43.6'
        output = inputStrCheck(inputVal)
        self.assertTrue(output == True)

    # -----------------------------------------------------------------------
    # ---- correct function test
    # 700 constructor:
    # Mandatory information missing,

    def test_calculateCorrect700_001_latmissing(self):
        inputVal = {'op': 'correct'}
        returnedDict = dispatch(inputVal)
        desiredOutput = {'error':'Mandatory information missing', 'op': 'correct'}
        self.assertTrue(returnedDict == desiredOutput)

    def test_calculateCorrect700_002_informationMissing(self):
        inputVal = {'op':'correct', 'long':'95d41.6', 'altitude':'13d42.3',  'assumedLat':'-53d38.4',
                    'assumedLong':' 74d35.3'}
        returnedDict = dispatch(inputVal)
        desiredOutput = {'op':'correct', 'long':'95d41.6', 'altitude':'13d42.3',
                         'assumedLat':'-53d38.4', 'assumedLong':' 74d35.3', 'error':'Mandatory information missing'}
        self.assertTrue(returnedDict == desiredOutput)

    def test_calculateCorrect700_003_IllegalLat(self):
        inputVal = {'op':'correct', 'lat':'16.0d32.3', 'long':'95d41.6', 'altitude':'13d42.3',
                    'assumedLat':'-53d38.4', 'assumedLong':' 74d35.3'}
        returnedDict = dispatch(inputVal)
        desiredOutput = {'op':'correct', 'lat':'16.0d32.3', 'long':'95d41.6', 'altitude':'13d42.3',
                         'assumedLat':'-53d38.4', 'assumedLong':' 74d35.3', 'error':'invalid lat'}
        self.assertTrue(returnedDict == desiredOutput)

    def test_calculateCorrect700_004_IllegalAssumedLat(self):
        inputVal = {'op':'correct', 'lat':'16d32.3', 'long':'95d41.6', 'altitude':'13d42.3',
                    'assumedLat':'-153d38.4', 'assumedLong':' 74d35.3'}
        returnedDict = dispatch(inputVal)
        desiredOutput = {'op':'correct', 'lat':'16d32.3', 'long':'95d41.6', 'altitude':'13d42.3',
                         'assumedLat':'-153d38.4', 'assumedLong':' 74d35.3', 'error':'invalid assumedLat'}
        self.assertTrue(returnedDict == desiredOutput)

    # -----------------------------------------------------------------------
    # ---- correct function test
    # 800 constructor:
    # Given cases, always happy path test

    def test_calculateCorrect800_001_happyPathTest(self):
        inputVal = {'op':'correct', 'lat':'89d20.1', 'long':'154d5.4', 'altitude':'37d17.4',
                    'assumedLat':'35d59.7', 'assumedLong':'74d35.3'}
        returnedDict = dispatch(inputVal)
        desiredOupt = {'assumedLat': '35d59.7', 'correctedDistance': '104', 'altitude': '37d17.4',
                       'assumedLong': '74d35.3', 'long': '154d5.4', 'correctedAzimuth': '0d36.8', 'lat': '89d20.1',
                       'op': 'correct'}
        self.assertTrue(returnedDict == desiredOupt)

    def test_calculateCorrect800_002_happyPathTest(self):
        inputVal = {'op':'correct', 'lat':'16d32.3', 'long':'95d41.6', 'altitude':'13d42.3',  'assumedLat':'-53d38.4',
                    'assumedLong':'74d35.3'}
        returnedDict = dispatch(inputVal)
        print (returnedDict)
        desiredOupt = {'op':'correct', 'lat':'16d32.3', 'long':'95d41.6', 'altitude':'13d42.3',  'assumedLat':'-53d38.4',
                       'assumedLong':'74d35.3', 'correctedDistance':'3950', 'correctedAzimuth':'164d42.9'}
        self.assertTrue(returnedDict == desiredOupt)