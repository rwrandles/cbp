# Comparative Agendas Project

Author: Rohnin Randles, University of Washington-Seattle

## Instructions in Brief:

   1. Collect U.S. Congress BillStatus information using the code and instructions given at the USCongress github

   2. Place downloaded bill data into the ./Input/ directory (NOTE: make sure to follow directory structure given below--the code is expecting this particular structure)

   3. Run main.R (make sure to set the working directory to the location of this folder!), this will output two CSV files in the ./Output/ directory

## Files:

   - `main.R`: this file contains all of the code necessary to navigate the USCongress file structures, parse data from the individual JSON files, and format the final CSV

   - `mlCode.R`: this file contains all of the code to train and create the supervised machine learning models to classify topics, subtopics, and private issue.

   - `./Dependencies/bills97-114.csv`: CBP data on bills from 97rd-114th congresses. This data is used as training data by `mlCode.R`

   - `./Dependencies/HSall_members.csv`: data from https://voteview.com/articles/data_help_members - this is used as a crosswalk between Congress's BioguideID and ICPSR, as well as providing other useful sponsor information. 
(NOTES: This file does NOT contain data on delegates, so only sponsors from the 50 states are represented. Given that this is a static dataset, this file will be out of date without updating upon any changes to the congressional roster)

   - Machine learning objects: files in ./Dependencies/ including `rf_major.rds`, `rf_private.rds`, and `rf_subNoXX.rds` represent trained RandomForest models used by `main.R`. `trainedDTM_major.rds` represents the DocumentTermMatrix used for training.

---

## Note on ./Input/ File Structure:

Depending on the Congress, the USCongress github returns data in a variety of structures. Generally, they all take the form of ./XXX/congress/data/XXX/bills/, where XXX represents the Congress number. Inside this .../bills/ directory is a set of subfolders breaking down each bill type (.../bills/hr, .../bills/sjres, etc.). **These folders are the ones that go into the ./Input/ directory**. The easiest route was generally for me to copy the contents of .../bills/ into ./Input/

### Here is an example of what the ./Input/ directory should look like

   - Input
      - hconres
      - hr
      - hjres
         - hjres1
         - hjres2
            - data.json
            - ...
         - hjres3
         - ...
      - hres
      - ...
