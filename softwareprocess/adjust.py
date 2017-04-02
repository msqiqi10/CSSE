import re
import os
import datetime

def calculatePredict(values):
    # checking important information
    key = 'body'
    if key not in values:
        values['error'] = 'mandatory information is missing'
        return values


    # calculation of long and lat
    fileName = os.path.join(os.path.dirname(__file__), 'stars.txt')
    stars = open(fileName)
    starsDict = {}
    for line in stars:
        eachLine = line
        eachLine = eachLine.split()
        starsDict[eachLine[0]] = str(eachLine[1]) + ' ' + str(eachLine[2])
    stars.close()
    starName = values['body']
    if starName not in starsDict: # do a star name check
        values['error'] = 'star not in catalog'
        return values

    # setting default values
    keys = ['long','lat']
    for key in dict.keys(values):
        keys.append(key)
    key = 'date'
    if key not in dict.keys(values):
        values[key] = '2001-01-01'
    else:
        flag = dateTest(values['date'])
        if flag == False:
            values['error'] = 'date value is illegal'
            return values
    key = 'time'
    if key not in dict.keys(values):
        values[key] = '00:00:00'
    else:
        flag = timeTest(values['time'])
        if flag == False:
            values['error'] = 'time value is illegal'
            return values


    starParameters = starsDict[starName]
    starParameters = starParameters.split()
    latitude = starParameters[1]
    SHA = starParameters[0]
    timeParameters = {'date' : values['date'], 'time' : values['time']}
    earthGHA = calculateEarthGHA(timeParameters)
    long = degreeToFloat(earthGHA) + degreeToFloat(SHA)
    long = long - (int(long / 360) * 360)
    long = degreeToString(long)
    values['long'] = long
    values['lat'] = latitude
    for key in dict.keys(values):
        if key not in keys:
            del values[key]
    return values

def timeTest(values):
    # validate time
    time = values['time']
    time = time.split(':')
    if (int(time[0]) > 24 or int(time[0]) < 0) or (int(time[1]) > 60 or int(time[1]) < 0) or (int(time[2]) > 60 or int(time[2]) <0):
        return False


def dateTest(value):
    # validate date
    if not re.match("^\d\d\d\d-\d\d-\d\d$", value):
        return False
    value = value.split('-')
    if int(value[0]) < 2001 or int(value[1]) > 12:
        return False
    date = int(value[2])
    month = int(value[1])
    year = int(value[0])
    if month == 2 and year % 4 != 0:
        if date > 28:
            return False
    if month == 2 and year % 4 == 0:
        if date > 29:
            return False
    if month in [1, 3, 5, 7, 8, 10, 12]:
        if date > 31:
            return False
    if month in [4, 6, 9, 11]:
        if date > 30:
            return False


#calculate the GHA to the date
def calculateEarthGHA(timeParameters):
    originalGHA = '100d42.6'
    originalGHA = degreeToFloat(originalGHA)
    date = timeParameters['date']
    time = timeParameters['time']
    year = int(date.split('-')[0])
    month = int(date.split('-')[1])
    day = int(date.split('-')[2])
    yearGap = year - 2001
    cumulativeProgress = yearGap * degreeToFloat('-0d14.31667')
    leapYears = int(yearGap / 4)
    dailyRotation = degreeToFloat('0d59.0')
    totalProgression = dailyRotation * leapYears
    # the rotation to the time
    beginningOfTheYear = datetime.date(year,1,1)
    currentDate = datetime.date(year,month,day)
    diff = currentDate - beginningOfTheYear
    dayGap = int(diff.days)
    time = time.split(':')
    sec = dayGap * 86400 + int(time[0]) * 3600 + int(time[1]) * 60 + int(time[2])
    rotationInYear = (sec - int(sec / 86164.1) * 86164.1) / 86164.1 * degreeToFloat('360d0')
    GHA = originalGHA + cumulativeProgress + totalProgression + rotationInYear
    GHA = degreeToString(GHA)
    return GHA

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