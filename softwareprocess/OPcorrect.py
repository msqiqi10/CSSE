from math import *
import re


def calculateCorrect(values):

# validate inputs
    keys = ['lat', 'long', 'altitude', 'assumedLat', 'assumedLong']
    alternatives = {}
    for key in keys:
        if key not in dict.keys(values):
            values['error'] = 'Mandatory information missing'
            return values
        value = values[key]
        if values[key][0] == '-':
            value = value.replace('-','')
            if not re.match("^\d*d\d*\.\d*$", value):
                values['error'] = 'invalid ' + key
                return values
            value = '-' + value

        else:
            if not re.match("^\d*d\d*\.\d*$", value):
                values['error'] = 'invalid ' + key
                return values
        boolVar = numericCheck(key,value)
        if boolVar == False:
            values['error'] = 'invalid ' + key
            return values
        alternatives[key] = degreeToFloat(value)
    LHA = alternatives['long'] + alternatives['assumedLong']
    for key in dict.keys(alternatives):
        alternatives[key] = radians(alternatives[key])
    intermediateDistance = (sin(alternatives['lat']) * sin(alternatives['assumedLat'])) + (cos(alternatives['lat'])
                                                     * cos(alternatives['assumedLat']) * cos(radians(LHA)))
    correctedAltitude = asin(intermediateDistance)
    correctedDistance = (alternatives['altitude'] - correctedAltitude)
    var1 = (sin(radians(degreeToFloat(values['lat']))) - (sin(radians(degreeToFloat(values['assumedLat']))) * intermediateDistance))
    var2 = cos(radians(degreeToFloat(values['assumedLat']))) * cos(asin(intermediateDistance))
    correctedAzimuth = (acos(var1 / var2)) * 180 / pi
    correctedDistance = int(correctedDistance * 180 / pi * 60)
    values['correctedDistance'] = str(correctedDistance)
    values['correctedAzimuth'] = degreeToString(correctedAzimuth)
    return values

def numericCheck(name,value):
    if name == 'long' or name == 'assumedLong':
        param = [0,360]
    if name == 'lat' or name == 'assumedLat':
        param = [-90,90]
    if name == 'altitude':
        param = [0,90]
    List = value.split('d')
    degree = int(List[0])
    minute = float(List[1])
    if not ((degree < param[1] and degree > param[0]) and (minute > 0 and minute < 60)):
        return False
    return True

def degreeToFloat(degree):
    degree = degree.split('d')
    minute = float(degree[1])
    if int(degree[0]) != 0:
        if int(degree[0]) < 0:
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