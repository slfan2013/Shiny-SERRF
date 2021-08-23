# Shiny-SERRF

## To run SERRF in local R

Click the green code button. 

Download Zip. 

Open the app.R file in RStudio. 

Then in RStudio, click Run App button.

Click Open in Browser.

## To run SERRF online
https://slfan.shinyapps.io/ShinySERRF/


## Input file format
Follow the [example dataset](https://github.com/slfan2013/Shiny-SERRF/raw/master/SERRF%20example%20dataset.xlsx).

It requries _batch_, _sampleType_, _time_, _label_ for samples, and _No_ for compounds.

_batch_ tells SERRF which samples/qcs belongs to one batch, e.g. machine, running period.

_sampleType_ requires _qc_, _sample_. It can also take other validate samples, e.g. _validate_. 

Note, if you have blank samples which are suggested not to be normalized, leave it empty cells. Any sample that you do not want to be normalized should be left empty.

_time_ is the processing order. It can be real time values, or simply integer indicating the processing order of the samples/qcs.

_label_ is the sample labels (row #4) and compound labels (column B).

_No_ is the compound index.

## Contact
slfan at ucdavis at edu
