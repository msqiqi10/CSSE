import urllib
import re
from collections import OrderedDict


def convertString2Dictionary(inputString = ""):
    errorDict = {'error': 'true'}
    untestedString = urllib.unquote(inputString)
    #check if string is empty
    if untestedString == '':
        return errorDict
    #check string content
    if not re.match("^[A-Za-z0-9=.,\s]*$", untestedString):
        return errorDict
    untestedStringList = untestedString.split(',')
    #do the rest checks and put pairs into an ordered lsit
    orderedList = []
    dic1 = {}
    for subString in untestedStringList:
        kvpair = subString.split('=')
        if not len(kvpair) == 2:
            return errorDict
        keyVal = kvpair[0].strip()
        value = kvpair[1].strip()
        if ' ' in keyVal or ' ' in value:
            return errorDict
        if keyVal == '' or value == '':
            return errorDict
        if not keyVal[0].isalpha():
            return errorDict
        for key in dic1.keys():
            if keyVal == key:
                return errorDict
        orderedList.append((keyVal, value))
        dic1[keyVal] = value
    dic = OrderedDict(orderedList)
    return dic