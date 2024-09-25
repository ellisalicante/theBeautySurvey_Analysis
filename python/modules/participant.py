import pandas as pd
from copy import deepcopy
import modules.beautySurveyImage
from datetime import datetime
import sys
import math

class Participant:
    nonImagePageTypes = ['bginfo','surveyStarted','providedConsent']
    attributesRatedNamesInDataFrame = ['feminine','happy','intelligence','perceived_age','perceived_attractiveness','perceived_gender','time_spent_on_page','trustworthy','unusual','sociability']
    attributesRated = ['feminine','happy','intelligence','age','attractiveness','gender','timeTaken','trustworthy','unusual','sociability']

    def __init__(self, fullStudyDataFrame, participantId):
        self.id = participantId
        self.imagesRated = []
        self.imageRatingsDict = {}

        # fullStudyDf = pd.read_csv(studyFilePath)
        self.df = deepcopy(fullStudyDataFrame[fullStudyDataFrame['prolificParticipantID'] == self.id])
        
        self.studyType = self._getStudyType()
        self._populateImagesRated()
        self.attentionChecksReceived = self._populateAttentionChecks()
        self.numAttChecksFailed = self.df["numAttCheckFailed"].max()

        # populate all the participant attributes from the bgInfo
        bgInfoRow = self.df[self.df["pageType"] == "bginfo"]
        self.filterUsage = bgInfoRow["filter_usage"].to_string(index=False)
        self.postingFrequency = bgInfoRow["posting_frequency"].to_string(index=False)
        self.selfRatedAttractiveness = int(bgInfoRow["self_rated_attractiveness"])
        self.socialMediaAccounts = bgInfoRow["social_media_accounts"].to_string(index=False)
        self.socialMediaUsage = bgInfoRow["social_media_usage"].to_string(index=False)

        # populate the order of the attributes
        self.questionOrder = self.df[self.df["pageType"] == "surveyStarted"]["questionOrderList"].to_string(index=False)

        # populate the time spent
        self.timeSpendList = self._populateTimeSpendList() #(index,pagetype/imageName,timeSpent)

        self._populateImagesRated()
    
    def convertTimeStringToTimeObject(self, timeString):
        dateFormat = "%Y-%m-%dT%H:%M:%S.%f%z"
        return timeString.apply(lambda x: datetime.strptime(x,dateFormat)) 

    def _getStudyType(self):
        temp = self.df['study_type'].unique()
        temp.sort()

        return temp[0] # possible types are 'cfd','faces','notYetLoaded'

    def _populateImagesRated(self):
        self.imagesRated = []
        self.imageRatingsDict = {}

        pagesAnswered = list(self.df['pageType'])
        for pageName in pagesAnswered:
            pageNameSplit = pageName.split('_') 
            
            if pageNameSplit[0] == 'img':
                dfRow = self.df[self.df["pageType"]==pageName]
                imageName = pageNameSplit[-1]
                self.imagesRated.append(imageName)
                self.imageRatingsDict[imageName] = {}
                for i in range(len(Participant.attributesRatedNamesInDataFrame)):
                    if Participant.attributesRated[i] == 'gender':
                        self.imageRatingsDict[imageName]['gender'] = dfRow['perceived_gender'].to_string(index=False) 
                    else:
                        self.imageRatingsDict[imageName][Participant.attributesRated[i]] = int(dfRow[Participant.attributesRatedNamesInDataFrame[i]])

    def _populateAttentionChecks(self):
        attCheckList = []
        pagesAnswered = list(self.df['pageType'])
        for pageName in pagesAnswered:
            pageNameSplit = pageName.split('_') 
            if pageNameSplit[0] == 'img':
                if pageNameSplit[1] == "attPassed":
                    attCheckList.append(("passed",int(pageNameSplit[2]))) # question number
                elif pageNameSplit[1] == "attFailed":
                    attCheckList.append(("failed",int(pageNameSplit[2]),pageNameSplit[3])) # question number, incorrect answer
        return attCheckList

    def getListOfImagesRated(self):
        return self.imagesRated

    def answeredAllNonImageSections(self):
        pagesAnswered = list(self.df['pageType'])
        for pageName in self.nonImagePageTypes:
            if pageName not in pagesAnswered:
                return (False,pageName)
        return (True,)

    def totalTimeTaken(self):        
        return sum(list(self.df['time_spent_on_page']))   

    def _populateTimeSpendList(self):
        listToReturn = []
        miniDF = deepcopy(self.df[['pageType','time_spent_on_page','question_end_time']])
        miniDF = miniDF.sort_values("question_end_time",key=self.convertTimeStringToTimeObject,ignore_index=True)
        
        for idx,row in miniDF.iterrows():
            listToReturn.append((idx,row["pageType"].split('_')[-1],row['time_spent_on_page']))

        return listToReturn
    
    def getAttentionCheckPositions(self):
        listToReturn = []
        miniDF = deepcopy(self.df[['pageType', 'question_end_time']])
        miniDF = miniDF.sort_values(by=['question_end_time'])
        count = 0
        for _,row in miniDF.iterrows():
            l = len(row['pageType'].split('_'))
            if l > 1:
                count += 1
                if count == 11:
                    count = 1
                if l > 2:
                    listToReturn.append(count)
        return listToReturn