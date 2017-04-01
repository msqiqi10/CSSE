import dispatch
from softwareprocess.adjust import *
import math
#
# inputVal = {'observation': '30d1.5', 'height': '19.0', 'pressure': '1000', 'horizon': 'artificial', 'op': 'adjust',
#             'temperature': '85'}
#
# outputVal = dispatch.dispatch(inputVal)
# print(outputVal)
#
# a = '-4d23.241'
# b = degreeToFloat(a)
# print(b)

# inputVal = {'time' : '03:15:42','date' : '2016-01-17'}
# returnedDict = calculateEarthGHA(inputVal)
# print(returnedDict)

inputVal = {'op': 'adjust', 'body': 'Betelgeuse', 'time': '03:15:42', 'date': '2016-01-97'}
returnedDict = calculatePredict(inputVal)
print(returnedDict)