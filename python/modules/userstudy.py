import modules.beautySurveyImage as bsi
from modules.participant import Participant
import pandas as pd
from tqdm import tqdm
import matplotlib.pyplot as plt
import numpy as np
import seaborn as sns

# sns.set_theme()

class UserStudy:
    def __init__(self, studyFilePath):
        self.studyFilePath = studyFilePath
        self.participantDict = {}
        self.imageRatingsDict = {}

        self.df = pd.read_csv(self.studyFilePath)
        for pid in tqdm(self.df['prolificParticipantID'].unique()):
            self.participantDict[pid] = Participant(self.df, pid)

        self._populateCompleteImageRatingsDict()
        # print(self.imageRatingsDict)

    def numParticipants(self):
        return len(self.participantDict)
    
    def _populateCompleteImageRatingsDict(self):
        for participantID in self.participantDict:
            p = self.participantDict[participantID]
            for imageName in p.imageRatingsDict:
                if imageName not in self.imageRatingsDict:
                    self.imageRatingsDict[imageName] = {}
                    for attribute in p.imageRatingsDict[imageName]:
                        self.imageRatingsDict[imageName][attribute] = []
                
                for attribute in p.imageRatingsDict[imageName]:
                        self.imageRatingsDict[imageName][attribute].append(p.imageRatingsDict[imageName][attribute])
    
def checkIfEveryoneAnsweredAllPages(studyObject):
    nonAnsweredList = []
    for pid in studyObject.participantDict:
        resp = studyObject.participantDict[pid].answeredAllNonImageSections()
        if not resp[0]:
            nonAnsweredList.append((pid,resp[1]))
    return nonAnsweredList

def checkIfEveryoneRatedAllImages(studyObject):
    l = []
    for pid in studyObject.participantDict:
        n = len(studyObject.participantDict[pid].getListOfImagesRated())
        if n != 10:
            l.append((pid,n))
    return l

def createBoxPlot(studyObj, 
                  func, 
                  xLabel = '', 
                  yLabel = '', 
                  ylim = (0.9,7.1), 
                  title = '', 
                  addMeanToTitle = True, 
                  categoriesToPlot = [], 
                  labelsOfCategories = [],
                  colorsOfBoxes = []):
    # func is a function that takes the imageName and the studyObject as parameters
    # returns a tuple of (class, listOfRatings)
    # the only thing I don't like with this set-up is that I'm passing this object around a lot

    groupedRatings = {}

    for imageName in studyObj.imageRatingsDict:
        imageGroup, listOfRatings = func(imageName,studyObj)
        try:
            groupedRatings[imageGroup].extend(listOfRatings)
        except KeyError:
            groupedRatings[imageGroup] = listOfRatings

    xticks = list(groupedRatings.keys())
    if categoriesToPlot == []: # plot everything in xticks
        pass
    else:
        for cat in categoriesToPlot:
            assert cat in xticks, "Unrecognised category in categoriesToPlot"
        xticks = categoriesToPlot

    if labelsOfCategories == []:
        labelsOfCategories = xticks

    assert len(labelsOfCategories) == len(xticks), "Unable to match labels to ticks"

    listOfRatings = []
    for group in xticks:
        listOfRatings.append(np.array(groupedRatings[group]))

    if addMeanToTitle:
        if title != '':
            title += ' ; '

        for i in range(len(xticks)):
            title += '%s:%0.2f'%(labelsOfCategories[i],np.mean(listOfRatings[i]))
            title += ', '

        title = title[:-2]

    plt.clf()
    bplot = plt.violinplot(listOfRatings,showmedians=True)

    if colorsOfBoxes != []:
        assert len(colorsOfBoxes) == len(xticks), "Unable to match colots to ticks"
        for patch, color in zip(bplot['boxes'],colorsOfBoxes):
            patch.set_facecolor(color)

    plt.title(title)
    plt.xticks([i+1 for i in range(len(xticks))],labelsOfCategories)
    plt.ylim(ylim[0],ylim[1])
    plt.xlabel(xLabel)
    plt.ylabel(yLabel)
    plt.show()
