from softwareprocess.dispatch import dispatch
inputVal = {'op': 'predict', 'body' : 'Akamar', 'date' : '2002-02-30'}
returnedDict = dispatch(inputVal)
print(returnedDict)