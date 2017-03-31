import dispatch
import math

inputVal = {'observation': '30d1.5', 'height': '19.0', 'pressure': '1000', 'horizon': 'artificial', 'op': 'adjust',
            'temperature': '85'}

outputVal = dispatch.dispatch(inputVal)
print(outputVal)