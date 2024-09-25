import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import modules.beautySurveyImage as bsi
import modules.participant
import modules.userstudy
import seaborn as sns
from copy import deepcopy
import os
import createHelperFiles

cwd = os.getcwd()

# set file paths for csv and figure storage
# easier to work with absolute paths - relative paths set for now for cleanliness. Remember to run scripts here from the main folder for python
storePath = cwd + "/../csv_storage"
figureBasePath = cwd + "/../figures"

# results store path is where the csv's from the computations will be kept to make it easy to upload to google drive

#Use the dictionaries - the lists are outdated

genderPalette = ["#ADD8E6", "#FFB6C1"]
#beautificationPalette = ["#AD1519", "#FABD00"]

# 0 is beautified, 1 is original
beautificationPaletteOptions = {
    "purpleOrange": ['#B15EFF','#FFA33C']
}

beautificationPaletteUsed = "purpleOrange"

genderPaletteDict = {"male":genderPalette[0],"female":genderPalette[1]}
#beautificationPaletteDict = {"beautified":beautificationPalette[0],"original":beautificationPalette[1]}
beautificationPaletteDict = {"beautified":beautificationPaletteOptions[beautificationPaletteUsed][0],
                             "original":beautificationPaletteOptions[beautificationPaletteUsed][1]}

## rater variables
raterPostingFrequencyList = ["severalTimesADay","aboutOnceADay","fewTimesAWeek","everyFewWeeks","lessOften","never"]
raterFilterUsageList = ['always','often','sometimes','rarely','never']
raterSocialMediaPlatformsList = ["facebook","instagram","tiktok","snapchat","other","dontUseSocialMedia"]

attributeList = ['perceived_attractiveness','intelligence','trustworthy','sociability','happy']
allCollectedAttributesList = ['perceived_attractiveness','feminine','happy','intelligence','trustworthy','unusual','sociability']

attributeNameMap = { # names to be used for the axis labels
    'perceived_attractiveness': "Attractiveness",
    'feminine': "Femininity",
    'happy': "Happiness",
    'intelligence': "Intelligence",
    'trustworthy': "Trustworthiness",
    'unusual': "Unusualness",
    'sociability': "Sociability"
}

ethnicitiesList = ['black','white','indian','asian','latino','mixed']

### All the rescaling results

## Read out from the rescaling done in R and then placed here
collapsePointsDict = {
    "original":{
        "perceived_attractiveness":{
            6:"6-7"
        },
        "intelligence":{
            5:"5-6-7"
        },
        "happy":{
            # everything is separable!
        },
        "sociability":{
            6:"6-7"
        },
        "unusual":{
            1:"1-2",
            2:"3-4",
            3:"5-6-7"
        },
        "trustworthy":{
            6:"6-7"
        },
        "feminine":{
            # everything is separable!
        }
    },
    "beautified":{
        "perceived_attractiveness":{
            1:"1-2-3"
        },
        "intelligence":{
            6:"6-7"
        },
        "happy":{
            # everything is separable!
        },
        "sociability":{
            1:"1-2"
        },
        "unusual":{
            1:"1-2-3-4",
            3:"6-7"
        },
        "trustworthy":{
            5:"5-6"
        },
        "feminine":{
            1:"1-2"
        }
    }
}

newPhisDict = {
    "original":{
        "perceived_attractiveness":"0.0000000 0.1427194 0.3212982 0.4273851 0.7484056 1.0000000",
        "intelligence":"0.0000000 0.4247487 0.7088311 0.8779911 1.0000000",
        "happy":"0.0000000 0.1093876 0.2809764 0.3738948 0.4591615 0.5617957 1.0000000",
        "sociability":"0.0000000 0.3023451 0.5592290 0.7263451 0.8579263 1.0000000",
        "unusual":"0.000000 0.731398 1.000000",
        "trustworthy":"0.0000000 0.2646554 0.5021536 0.7106732 0.8408876 1.0000000",
        "feminine": "0.0000000 0.1803165 0.3438653 0.4496197 0.6071358 0.8383664 1.0000000"
    },
    "beautified":{
        "perceived_attractiveness":"0.0000000 0.1217957 0.3361430 0.7067340 1.0000000",
        "intelligence":"0.0000000 0.1593817 0.4199120 0.6001768 0.7797382 1.0000000",
        "happy":"0.0000000 0.1978624 0.3729320 0.5209853 0.6271055 0.8182060 1.0000000",
        "sociability":"0.0000000 0.2510557 0.4549311 0.5904113 0.7737772 1.0000000",
        "unusual":"0.0000000 0.4626228 1.0000000",
        "trustworthy":"0.0000000 0.1566801 0.3635567 0.5156657 0.7254324 1.0000000",
        "feminine":"0.0000000 0.1346507 0.2668609 0.4078255 0.6503819 1.0000000"
    }
}

newPhisDict_numeric = deepcopy(newPhisDict)
for beautification in ["original","beautified"]:
    for attr in attributeList:
        s = newPhisDict[beautification][attr]
        newPhisDict_numeric[beautification][attr] = list(map(lambda x: float(x),s.split(" ")))
        # print(attr,":",newPhisDict_numeric[beautification][attr])

numLevelsDict = {"original":{},"beautified":{}}
for beautification in ["original","beautified"]:
    for attr in attributeList:
        numLevelsDict[beautification][attr] = len(newPhisDict_numeric[beautification][attr])

ratings_df = pd.read_csv(storePath + "/fullRatingsFile.csv")

imageNamesList = ratings_df["imageName"].unique()
imageTagsSet = set()
for imageName in imageNamesList:
    imageTagsSet.add(bsi.getImageTag(imageName))

imageTagsList = list(imageTagsSet)    

# list of dataframes to be loaded in here
allAttributesMedians_df = None
orderedMedianRatingsBeautified_df = None
orderedMedianRatingsOriginal_df = None
pairedMedians_df = None
onlyParticipantInfo_df = None
delta_attraciveness_df = None
original_data_rescaled_numeric = None
beautified_data_rescaled_numeric = None

dataframesList = [allAttributesMedians_df, orderedMedianRatingsBeautified_df, orderedMedianRatingsOriginal_df, pairedMedians_df, onlyParticipantInfo_df,delta_attraciveness_df,original_data_rescaled_numeric,beautified_data_rescaled_numeric]
fileNamesList = ["allAttributesMedians_df.csv","orderedMedianRatingsBeautified_df.csv","orderedMedianRatingsOriginal_df.csv","pairedMedians_df.csv","onlyParticipantInfo_df.csv","delta_attraciveness_df.csv","original_data_rescaled_numeric.csv","beautified_data_rescaled_numeric.csv"]

try:
    allAttributesMedians_df = pd.read_csv(storePath + "/allAttributesMedians_df.csv")
    print("Loaded allAttributesMedians_df")
except FileNotFoundError:
    createHelperFiles.generateHelperFiles()

try:
    orderedMedianRatingsBeautified_df = pd.read_csv(storePath + "/orderedMedianRatingsBeautified_df.csv")
    print("Loaded orderedMedianRatingsBeautified_df")
except FileNotFoundError:
    createHelperFiles.generateHelperFiles()

try:
    orderedMedianRatingsOriginal_df = pd.read_csv(storePath + "/orderedMedianRatingsOriginal_df.csv")
    print("Loaded orderedMedianRatingsOriginal_df")
except FileNotFoundError:
    createHelperFiles.generateHelperFiles()

try:
    pairedMedians_df = pd.read_csv(storePath + "/pairedMedians_df.csv")
    print("Loaded pairedMedians_df")
except FileNotFoundError:
    createHelperFiles.generateHelperFiles()

try:
    onlyParticipantInfo_df = pd.read_csv(storePath + "/onlyParticipantInfo_df.csv")
    print("Loaded onlyParticipantInfo_df")
except FileNotFoundError:
    createHelperFiles.generateHelperFiles()

try:
    delta_attraciveness_df = pd.read_csv(storePath + "/delta_attraciveness_df.csv")
    print("Loaded delta_attraciveness_df")
except FileNotFoundError:
    createHelperFiles.generateHelperFiles()

try:
    original_data_rescaled_numeric = pd.read_csv(storePath + "/original_data_rescaled_numeric.csv")
    print("Loaded original_data_rescaled_numeric")
except FileNotFoundError:
    createHelperFiles.generateHelperFiles()

try:
    beautified_data_rescaled_numeric = pd.read_csv(storePath + "/beautified_data_rescaled_numeric.csv")
    print("Loaded beautified_data_rescaled_numeric")
except FileNotFoundError:
    createHelperFiles.generateHelperFiles()