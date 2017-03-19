import re
import math
def dispatch(values=None):

    # Set default values
    key = 'height'
    if key not in values:
        values['height'] = '0'
    key = 'temperature'
    if key not in values:
        values['temperature'] = '72'
    key = 'pressure'
    if key not in values:
        values['pressure'] = '1010'
    key = 'horizon'
    if key not in values:
        values['horizon'] = 'natural'

    # Validate parm
    if(values == None):
        return {'error': 'parameter is missing'}
    if not(isinstance(values,dict)):
        return {'error': 'parameter is not a dictionary'}
    if (not('op' in values)):
        values['error'] = 'no op  is specified'
        return values

    # Validate parm in dic
    for key in values:
        if (key == 'observation'):
            break
        else:
            return {'error': 'missing observation'}

    # validate values
    value = values['observation']
    if not re.match("^\d*d\d*\.\d*$", value):
        return {'error': 'value of observation is illegal'}
    observationSet = value.split('d')
    degree = int(observationSet[0])
    minute = float(observationSet[1])
    if degree > 90 or degree < 0:
        return {'error': 'degree of observation value is illegal'}
    if minute > 60 or minute < 0:
        return {'error': 'minute of observation value is illegal'}
    if float(values['height']) < 0:
        return {'error': 'value of height is illegal'}
    if int(values['temperature']) < -20 or int(values['temperature']) > 120:
        return {'error': 'value of temperature is illegal'}
    if int(values['pressure']) < 100 or int(values['pressure']) > 1100:
        return {'error': 'value of pressure is illegal'}
    if values['horizon'] != 'natural' and values['horizon'] != 'artificial':
        return {'error': 'value of horizon is illegal'}




    #Perform designated function
    if(values['op'] == 'adjust'):
        if values['horizon'] == 'natural':
            dip = (-.97 * math.sqrt(float(values['height']))) / 60
        else:
            dip = 0
        refraction = (-.00452 * float(values['pressure']))
        degreeOfAltitude = degree + int(dip) + int(refraction)
        minuteOfAltitude = minute + (round(dip, 1) - int(dip)) * 60 + (round(refraction, 1) - int(refraction)) * 60
        if minuteOfAltitude < 0:
            degreeOfAltitude = degreeOfAltitude - 1
            minuteOfAltitude = minuteOfAltitude + 60
        if minuteOfAltitude > 60:
            degreeOfAltitude = degreeOfAltitude + int(minuteOfAltitude / 60)
            minuteOfAltitude = minuteOfAltitude - int(minuteOfAltitude / 60) * 60
        altitude = str(degreeOfAltitude) + 'd' + str(minuteOfAltitude)
        values['altitude'] = altitude
        return values
    elif(values['op'] == 'predict'):
        return values    #This calculation is stubbed out
    elif(values['op'] == 'correct'):
        return values    #This calculation is stubbed out
    elif(values['op'] == 'locate'):
        return values    #This calculation is stubbed out
    else:
        values['error'] = 'op is not a legal operation'
        return values
