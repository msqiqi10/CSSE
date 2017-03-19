import re
import math
def dispatch(values=None):

    # Validate parm
    if values == None:
        return {'error': 'parameter is missing'}
    if not(isinstance(values,dict)):
        return {'error': 'parameter is not a dictionary'}
    if not'op' in values:
        return {'error': 'no op specified'}
    if values['op'] != 'adjust' and values['op'] != 'predict' and values['op'] != 'correct' and values['op'] != 'locate':
        return {'error': 'op is not a legal operator'}

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

    key = 'observation'
    if key not in values:
        return {'error': 'mandatory information missing'}

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
    try:
        if float(values['height']) < 0:
            return {'error': 'value of height is illegal'}
        if int(values['temperature']) < -20 or int(values['temperature']) > 120:
            return {'error': 'value of temperature is illegal'}
        if int(values['pressure']) < 100 or int(values['pressure']) > 1100:
            return {'error': 'value of pressure is illegal'}
        if values['horizon'] != 'natural' and values['horizon'] != 'artificial':
            return {'error': 'value of horizon is illegal'}
    except ValueError:
        return {'error': 'cast error'}




    #Perform designated function
    if(values['op'] == 'adjust'):
        for key in values:
            if key == 'altitude':
                return {'error': 'altitude has already in parameter'}
        valueOfObservation = values['observation'].split('d')
        if valueOfObservation[0] == 0 and valueOfObservation[1] < 0.1:
            values['error': 'value of observation is too small']
        if values['horizon'] == 'natural':
            dip = (-.97 * math.sqrt(float(values['height']))) / 60
        else:
            dip = 0
        refraction = -.00452 * float(values['pressure'])
        degree = degree + minute / 60
        print(degree)
        degree = float("{:.1f}".format(degree + dip + refraction))
        print(degree)
        minute = str((degree - int(degree)) * 60)
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
    elif(values['op'] == 'predict'):
        return values    #This calculation is stubbed out
    elif(values['op'] == 'correct'):
        return values    #This calculation is stubbed out
    elif(values['op'] == 'locate'):
        return values    #This calculation is stubbed out
    else:
        values['error'] = 'op is not a legal operation'
        return values
