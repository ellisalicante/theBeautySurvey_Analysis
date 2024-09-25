"""
This object is not meant to have any analysis associated with it, it's just an easier way to get information stored in the name
"""

class WrongStudyTypeError(Exception):
    pass

def genericSplitName(imageName,indexToGet):
    splitString = imageName.split('-')
    return splitString[indexToGet]

def studyType(imageName):
    return genericSplitName(imageName,0)

def isBeautified(imageName):
    btStatus = genericSplitName(imageName,1)
    if btStatus == "beautified":
        return True
    elif btStatus == "original":
        return False
    else:
        raise Exception("Unrecognised beautification status")
    
def beautifiedTag(imageName):
    return genericSplitName(imageName,1)

def gender(imageName):
    return genericSplitName(imageName,2)

def ethnicity(imageName):
    if studyType(imageName) == 'cfd':
        return genericSplitName(imageName,3)
    raise WrongStudyTypeError("FACES images don't have a defined ethnicity")

def ageGroup(imageName):
    if studyType(imageName) == 'faces':
        return genericSplitName(imageName,3)
    raise WrongStudyTypeError("CFD images don't have a defined ageGroup")

def getCondition(imageName):
    return genericSplitName(imageName,3)

def getImageTag(imageName):
    l = imageName.split("-")
    return "%s-%s-%s-%s"%(l[0],l[2],l[3],l[4])

def getImageNameFromTag(imageTag,kind):
    assert kind == "beautified" or kind == "original", "Unknown image kind"
    l = imageTag.split('-')
    return "%s-%s-%s-%s-%s"%(l[0],kind,l[1],l[2],l[3])