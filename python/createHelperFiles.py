'''
The code in python and R works on processed versions
of the original datafile. This script helps generate
these processed versions. 

Before running any experiments, please run this script
to generate the processed data files.

Datafiles needed to generate the processed versions:
- fullRatingsFile.csv
- onlyParticipantInfo_df.csv
- original_data_rescaled.csv (created during the rescaling done in R)
- beautified_data_rescaled.csv (created during the rescaling done in R)
'''

import pandas as pd
import numpy as np
import modules.beautySurveyImage as bsi
import modules.sharedVariables as sv
from copy import deepcopy
import os


### treat the python folder as the current working directory.
### use absolute paths when possible to avoid issues with the paths

cwd = os.getcwd()
assert cwd.split("/")[-1] == "python", "Incorrect correct working directory. Run file from the python folder to avoid issues with the paths"

storePath = cwd + "/../csv_storage"

def getRescalingFunction(beautification,attr):
    N = len(sv.newPhisDict_numeric[beautification][attr])
    def rescaleAttr(x):
        phi_list = sv.newPhisDict_numeric[beautification][attr] # Can push this outside the function because it'll still be in the closure, but keeping it here because I don't need very efficient performance
        assert x>= 1 and x <= N
        return (N-1) * phi_list[x-1] + 1
    return rescaleAttr

def get01RescalingFunction(beautification,attr):
    N = len(sv.newPhisDict_numeric[beautification][attr])
    def rescaleAttr(x):
        phi_list = sv.newPhisDict_numeric[beautification][attr] # Can push this outside the function because it'll still be in the closure, but keeping it here because I don't need very efficient performance
        assert x>= 1 and x <= N
        return phi_list[x-1]
    return rescaleAttr

def generateHelperFiles():
    ratings_df = pd.read_csv(storePath + "/fullRatingsFile.csv")

    imageNamesList = ratings_df["imageName"].unique()

    ## --(allAttributesMedians_df)--

    attrListToUse = sv.attributeList + ["perceived_age"]
    columns = ["imageName","imageTag","dataset","beautification","imageGender","condition"] + attrListToUse
    allAttributesMedians_df = pd.DataFrame(columns=columns)

    for imageName in imageNamesList:
        l = imageName.split('-')
        temp_df = pd.DataFrame.from_dict({"imageName":[imageName],
                                          "imageTag": [bsi.getImageTag(imageName)],
                                          "dataset":[l[0]],
                                          "beautification":[l[1]],
                                          "imageGender":[l[2]],
                                          "condition":[l[3]]})

        for attr in attrListToUse:
            participantRatingsList = np.array(ratings_df[ratings_df["imageName"]==imageName][attr])
            temp_df[attr] = np.median(participantRatingsList)
            
        allAttributesMedians_df = pd.concat([allAttributesMedians_df,temp_df])

    allAttributesMedians_df.to_csv(storePath + "/allAttributesMedians_df.csv",index=False)

    ## --(pairedMedians_df)--

    assert len(allAttributesMedians_df) == len(allAttributesMedians_df["imageName"].unique())

    attrListToUse = sv.attributeList + ["perceived_age"]

    columns = ["imageName","imageTag",'dataset', 'beautification', 'imageGender', 'condition'] + attrListToUse
    orderedMedianRatingsBeautified_df = pd.DataFrame(columns=columns)
    orderedMedianRatingsOriginal_df = pd.DataFrame(columns=columns)

    imageTagsSet = set()
    for imageName in imageNamesList:
        imageTagsSet.add(bsi.getImageTag(imageName))

    imageTagsList = list(imageTagsSet)
    imageTagsList.sort() 
    # so both lists for now are ordered alphabetically by the image tag
    # this choice was arbitrary, just needed some attribute to impose an ordering and this was the easiest

    for imageTag in imageTagsList:
        for beautStatus in ["beautified","original"]:
            imageName = bsi.getImageNameFromTag(imageTag,kind=beautStatus)

            rowToAdd = allAttributesMedians_df[allAttributesMedians_df["imageName"]==imageName] # asserted that I have only one row for each image name in this df

            if beautStatus == "beautified":
                orderedMedianRatingsBeautified_df = pd.concat([orderedMedianRatingsBeautified_df,rowToAdd],ignore_index=True)
            else:
                orderedMedianRatingsOriginal_df = pd.concat([orderedMedianRatingsOriginal_df,rowToAdd],ignore_index=True)
            
    # now both the sets are ordered by the imageNames so I can run paired tests on the two sets
    # run a sanity check first

    orderedMedianRatingsBeautified_df["imageTag"] = orderedMedianRatingsBeautified_df.apply(lambda x: bsi.getImageTag(x["imageName"]),axis=1)
    orderedMedianRatingsOriginal_df["imageTag"] = orderedMedianRatingsOriginal_df.apply(lambda x: bsi.getImageTag(x["imageName"]),axis=1)

    pairedMedians_df = deepcopy(orderedMedianRatingsOriginal_df)
    pairedMedians_df = deepcopy(orderedMedianRatingsOriginal_df)
    #rename all the column names to have the original tag
    renameDict = {}
    attrListToUse = sv.attributeList + ["perceived_age"]
    for attr in attrListToUse:
        renameDict[attr] = "original_" + attr
    pairedMedians_df = pairedMedians_df.rename(columns=renameDict)    

    pairedMedians_df = pd.merge(orderedMedianRatingsOriginal_df,orderedMedianRatingsBeautified_df,on="imageTag",suffixes=("_original","_beautified"))

    # sanity check - based on the indexing and the names. Assuming that the rows still stay grouped the same way, which is safe
    for idx,row in pairedMedians_df.iterrows():
        assert bsi.getImageTag(row["imageName_original"]) == row["imageTag"]
        assert bsi.getImageTag(row["imageName_beautified"]) == row["imageTag"]

    columnsToDrop = ["imageName",'dataset', 'beautification', 'imageGender', 'condition']

    pairedMedians_df = pairedMedians_df.drop(labels=[x+"_beautified" for x in columnsToDrop],axis=1)
    pairedMedians_df = pairedMedians_df.rename(columns={x+"_original":x for x in columnsToDrop[1:]})

    pairedMedians_df.to_csv(storePath + "/pairedMedians_df.csv",index=False)
    # pairedMedians_df.rename()    

    for idx,row in orderedMedianRatingsBeautified_df.iterrows():
        assert row["imageTag"] == orderedMedianRatingsOriginal_df.iloc[idx]["imageTag"]
    
    orderedMedianRatingsBeautified_df.to_csv(storePath + "/orderedMedianRatingsBeautified_df.csv",index=False)
    orderedMedianRatingsOriginal_df.to_csv(storePath + "/orderedMedianRatingsOriginal_df.csv",index=False)
    
    ## --(delta_attraciveness_df)--
    
    delta_attraciveness_df = deepcopy(pairedMedians_df)

    delta_attraciveness_df = delta_attraciveness_df[["imageName_original","imageTag","dataset","imageGender","condition","perceived_attractiveness_original","perceived_attractiveness_beautified"]]
    delta_attraciveness_df['increaseInAttrac'] = delta_attraciveness_df["perceived_attractiveness_beautified"] - delta_attraciveness_df["perceived_attractiveness_original"]
    delta_attraciveness_df.to_csv(sv.storePath + '/delta_attractiveness_df.csv')

    ## --(rescaled data files)--

    trivialColumnsToDrop = ["perceived_age_3cat","participantAge_3cat","perceived_attractivenessfact","perceived_attractiveness_3cat"]

    for beautification in ["original","beautified"]:
        df = pd.read_csv(sv.storePath + "/models/dependentAttributes/%s_data_rescaled.csv"%(beautification))
        
        # drop all the columns which definitely have to go
        for col in trivialColumnsToDrop:
            df = df.drop(col,axis=1)

        # add 7L tag to perceived_attractiveness
        df = df.rename(columns={"perceived_attractiveness":"perceived_attractiveness_7L"})

        # for attributes that have 7L after collapsing, nothing changes
        for attr in sv.attributeList:
            if sv.numLevelsDict[beautification][attr] == 7:
                df["%s_collapsed"%(attr)] = df["%s_7L"%(attr)]

        # rename all the 7L columns to origRating
        for attr in sv.attributeList:
            df = df.rename(columns={"%s_7L"%(attr):"%s_origRating"%(attr)})

        # now remove all the columns that do not correspond to a final rating
        for attr in sv.attributeList:
            numLevels = sv.numLevelsDict[beautification][attr]
            colsToRemove = list(range(6,numLevels,-1)) # 7 renamed to collapsed. If the finalRating has 6 levels, that is preserved
            
            for col in colsToRemove:
                df = df.drop("%s_%dL"%(attr,col),axis=1)

        # now rename all the columns with the final number of levels to "_collapsed"
        for attr in sv.attributeList:
            numLevels = sv.numLevelsDict[beautification][attr]
            df = df.rename(columns={"%s_%dL"%(attr,numLevels):"%s_collapsed"%(attr)})

        # run sanity checks on the columns generated
        # check to run - num levels in each of the columns matches to what is supposed to be there
        for attr in sv.attributeList:
            # check that origRating has 7 levels
            origList = list(df["%s_origRating"%(attr)].unique())
            origList.sort()
            assert origList == list(range(1,8))

            # check that the collapsed column has the right number of levels
            collapsedList = list(df["%s_collapsed"%(attr)].unique())
            collapsedList.sort()
            assert collapsedList == list(range(1,sv.numLevelsDict[beautification][attr]+1))

        df.to_csv(sv.storePath + "/%s_data_collapsed_clean.csv"%(beautification))

    for beautification in ["original","beautified"]:
        regrouped_df = pd.read_csv(sv.storePath + "/%s_data_collapsed_clean.csv"%(beautification))
        # this just collapsed the levels, but now I want to also apply the new distances
        for attr in sv.attributeList:
            func = get01RescalingFunction(beautification,attr)
            regrouped_df["%s_rescaledNumeric_01"%(attr)] = regrouped_df.apply(lambda x: func(x["%s_collapsed"%(attr)]),axis=1)

            func = getRescalingFunction(beautification,attr)
            regrouped_df["%s_rescaledNumeric"%(attr)] = regrouped_df.apply(lambda x: func(x["%s_collapsed"%(attr)]),axis=1)

        regrouped_df.to_csv(sv.storePath + "/%s_data_rescaled_numeric.csv"%(beautification))

        # run a num levels sanity check
        for attr in sv.attributeList:
            assert len(regrouped_df["%s_collapsed"%(attr)]) == len(regrouped_df["%s_rescaledNumeric_01"%(attr)])
        

if __name__ == "__main__":
    generateHelperFiles()