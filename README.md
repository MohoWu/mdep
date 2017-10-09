## mdep; midas data extraction and pre-processing
#### This repository contains some blocks of code used for the traffic data pre-processing part of my PhD work with Dr Richard Connors of the University of Leeds. Particularly, for my thesis I have used the [Motorway Incident Detection and Automatic Signalling (MIDAS) data base](https://www.midas-data.org.uk/) to extract the necessary data for studying 'uncertainty propagation in the traffic flow and vehicle exhaust emission modelling chain'. In my work I have automated some of the work involved since my methodology was mainly based on an 'ensemble-based' approach to modelling. So, in this repository, I have added some blocks of code related to automating the process of downloading/extracting measurement site data and transforming it to prepare for the macroscopic traffic flow models. 

#### Now my final thesis can be found [here](http://etheses.whiterose.ac.uk/17917/) which this package was part off. 

#### At some point, I might add some code related to the calibration and simulation of two traffic flow models directly linked with the MIDAS data base to make the calibration and validation of these models easier and faster and also as an attempt to provide open source code for traffic flow models. However, the application of these traffic flow models can only be for simple motorway networks (i.e. road links with on-ramps and off-ramps located a long the route).

#### NOTE: As part of the privacy agreement with the Highway England, none of the data will be given here nor access to data. Data needs to be directly requested from the Highways England/Mott Mcdonald.

#### Hope this repository helps in some way. Please [get in touch](mailto:arwa.sayegh@gmail.com) if you have any question/comment... 

#### Thank you
#### Arwa



##### Note on installation: you can use devtools::install_github("author/package") to install package
##### install.packages("devtools")
##### library(devtools)
##### install_github("arwasayegh/mdep")