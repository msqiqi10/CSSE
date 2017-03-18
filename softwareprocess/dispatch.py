import re
def dispatch(values=None):

    #Validate parm
    if(values == None):
        return {'error': 'parameter is missing'}
    if not(isinstance(values,dict)):
        return {'error': 'parameter is not a dictionary'}
    if (not('op' in values)):
        values['error'] = 'no op  is specified'
        return values

    #Validate parm in dic
    for key in values:
        if not (key == 'observation'):
            return {'error': 'missing observation'}

    value = values['observation']
    value = value.split('d')
    if not re.match("^[A-Za-z0-9=.,\s]*$",value):
        return {'error': 'value of observation is not '}


    #Perform designated function
    if(values['op'] == 'adjust'):
        return values    #<-------------- replace this with your implementation
    elif(values['op'] == 'predict'):
        return values    #This calculation is stubbed out
    elif(values['op'] == 'correct'):
        return values    #This calculation is stubbed out
    elif(values['op'] == 'locate'):
        return values    #This calculation is stubbed out
    else:
        values['error'] = 'op is not a legal operation'
        return values
