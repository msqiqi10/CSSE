import re
import math
import os

def dispatch(values=None):

    #Validate parm
    if(values == None):
        return {'error': 'parameter is missing'}
    if(not(isinstance(values,dict))):
        return {'error': 'parameter is not a dictionary'}
    if (not('op' in values)):
        values['error'] = 'no op is specified'
        return values

    #Perform designated function
    if(values['op'] == 'adjust'):
        values = calculateAltitude(values)
        return values
    elif(values['op'] == 'predict'):
        values = calculatePredict(values)
        return values
    elif(values['op'] == 'correct'):
        return values    #This calculation is stubbed out
    elif(values['op'] == 'locate'):
        return values    #This calculation is stubbed out
    else:
        values['error'] = 'op is not a legal operation'
        return values

def calculatePredict(values):
    key = 'body'
    if key not in values:
        values['error'] = 'mandatory information is missing'
        return values
    fileName = os.path.join(os.path.dirname(__file__),'stars.txt')
    stars = open(fileName)
    starsDict = {}
    for line in stars:
        eachLine = line
        eachLine = eachLine.split()
        starsDict[eachLine[0]] = str(eachLine[1]) + ' ' + str(eachLine[2])
    stars.close()
    value = values['body']
    if value not in starsDict:
        values['error'] = 'star not in catalog'
        return values
    value = values['date']
    if not re.match("^\d\d\d\d-\d\d-\d\d$", value):
        values['error'] = 'date format is illegal'
        return values


def calculateAltitude(values):
    key = 'observation'
    if key not in values:
        return {'error': 'mandatory information missing'}
    for key in values:
        if key == 'altitude':
            return {'error': 'altitude has already been in parameter'}
    # setting default values in Dict
    keys = ['altitude']
    for key in values:
        keys.append(key)
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

    # validate parameters
    value = values['observation']
    if not re.match("^\d*d\d*\.\d*$", value):
        values['error'] = 'value of observation is illegal'
        return values
    observationSet = value.split('d')
    degree = int(observationSet[0])
    minute = float(observationSet[1])
    if degree > 90 or degree < 0:
        values['error'] = 'degree of observation value is illegal'
        return values
    if minute > 60 or minute < 0:
        values['error'] = 'degree of observation value is illegal'
        return values
    try:
        if float(values['height']) < 0:
            values['error'] = 'value of height is illegal'
            return values
        if int(values['temperature']) < -20 or int(values['temperature']) > 120:
            values['error'] = 'value of temperature is illegal'
            return values
        if int(values['pressure']) < 100 or int(values['pressure']) > 1100:
            values['error'] = 'value of pressure is illegal'
            return values
        if values['horizon'] != 'natural' and values['horizon'] != 'artificial':
            values['error'] = 'value of horizon is illegal'
            return values
    except ValueError:
        values['error'] = 'cast error'
        return values

    # calculate altitude
    if values['horizon'] == 'natural':
        dip = (-.97 * math.sqrt(float(values['height']))) / 60
    else:
        dip = 0
    degreeInRadians = math.radians(degree + minute / 60)
    refraction = ((-.00452 * float(values['pressure'])) / (273 + (int(values['temperature']) - 32) * 5 / 9)) / math.tan(
        degreeInRadians)
    degree = degree + minute / 60
    degree = float(degree + dip + refraction)
    minute = str("{:.1f}".format((degree - int(degree)) * 60))
    minute = minute.split('.')
    var1 = minute[0].zfill(2)
    var2 = minute[1]
    minute = var1 + '.' + var2
    altitude = str(int(degree)) + 'd' + minute
    values['altitude'] = altitude
    for key in values.keys():
        if key not in keys:
            del values[key]
    return values