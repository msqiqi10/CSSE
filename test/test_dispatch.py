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

    def test_dispatch100_001_observationMissing(self):
        inputVal = {'op': 'adjust'}
        returnedValue = dispatch(inputVal)
        print(returnedValue)
        self.assertTrue(returnedValue == {'error': 'missing observation'})
