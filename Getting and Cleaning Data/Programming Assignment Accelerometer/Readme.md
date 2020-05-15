---
title: "Readme"
author: "Hari"
date: "4/5/2020"
output: 
  html_document:
    keep_md: true
---

## Getting and Cleaning Data - Course Project

The included R script, run_analysis.R, conducts the following:

1.assumes the dataset already the working directory.

2.Read both the train and test datasets and merge them.

3.modify column names using the features.txt file

4.Extract requiered data i.e all the mean and std parameters and use regex to modify their names

5.Generate 'Tidy Dataset' that consists of the average (mean) of each variable for each subject and each activity. The result is shown in the file tidy_dataset.txt.
