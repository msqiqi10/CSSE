from unittest import TestCase
from softwareprocess.dispatch import dispatch


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
        self.assertTrue(returnedValue == {'error': 'mandatory information missing'})

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
        desiredOutput = {'error':'mandatory information is missing', 'op': 'predict'}
        self.assertTrue(returnedDict == desiredOutput)

    def test_calculatePredict300_003_bodyNotInTable(self):
        inputVal = {'op': 'predict', 'body' : 'lalala'}
        returnedDict = dispatch(inputVal)
        desiredOutput = {'error':'body not in table', 'body' : 'lalala', 'op' : 'predict'}
        self.assertTrue(returnedDict == desiredOutput)