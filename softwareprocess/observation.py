import math
import re

def calculateAltitude(values):
    key = 'observation'
    if key not in dict.keys(values):
        return {'error': 'mandatory information observation missing'}
    for key in dict.keys(values):
        if key == 'altitude':
            return {'error': 'altitude has already been in parameter'}

    # setting default values in Dict
    keys = ['altitude']
    for key in dict.keys(values):
        keys.append(key)
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
    altitude = degreeToString(degree)
    values['altitude'] = altitude
    for key in dict.keys(values):
        if key not in keys:
            del values[key]
    return values

def degreeToFloat(degree):
    degree = degree.split('d')
    minute = float(degree[1])
    if int(degree[0]) != 0:
        if degree[0] < 0:
            degree = int(degree[0]) - minute / 60
        else:
            degree = int(degree[0]) + minute / 60
    else:
        if degree[0][0] == '-':
            degree = - minute / 60
        else:
            degree = minute / 60
    return degree

def degreeToString(degree):
    minute = str("{:.1f}".format((degree - int(degree)) * 60))
    if '-' in minute:
        minute = minute.replace('-', '')
    minute = minute.split('.')
    var1 = minute[0].zfill(2)
    var2 = minute[1]
    minute = var1 + '.' + var2
    degree = str(int(degree)) + 'd' + minute
    return degree