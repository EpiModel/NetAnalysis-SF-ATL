# The HIV/STI Epidemic Potential of Dynamic Sexual Networks of Men Who Have Sex With Men in Atlanta and San Francisco
This repository holds the source to code to reproduce the analysis featured in our network analysis of MSM in San Francisco and Atlanta. This study explored how the foward reachable path measure may be used to predict the HIV/STI epidemic potential in empirical sexual networks.

## Citation
> Anderson EJ, Weiss KM, Morris MM, Sanchez TH, Prasad P, Jenness SM. The HIV Epidemic Potential of Dynamic Sexual Networks of Men Who Have Sex With Men in San Francisco and Atlanta. medRxiv 2020.10.12.20211540; doi: https://doi.org/10.1101/2020.10.12.20211540. [Pre-Print]

<img src="https://github.com/EpiModel/NetAnalysis-SF-ATL/blob/master/Figures/Picture1.png">

## Abstract

#### Background
The potential speed through which a pathogen may circulate in a network is a function of network connectivity. Network features like degree (number of ongoing partnerships) determine the cross-sectional network connectivity. The overall transmission potential of a pathogen involves connectivity over time, which can be measured using the forward reachable path (FRP). We modeled dynamic sexual networks of MSM in San Francisco and Atlanta to estimate the FRP as a predictor of HIV/STI epidemic potential.

#### Methods
We used exponential random graph models to obtain parameter estimates for each city’s sexual network and then simulated the complete networks over time. The FRP was estimated in each city overall and stratified by demographics.

#### Results
The overall mean and median FRPs were higher in San Francisco than in Atlanta, suggesting a greater epidemic potential for HIV and STIs in San Francisco. At one year, in both cities, the average FRP among casual partnerships was highest in the youngest age group and lowest in the oldest age group, contrasting with the cross-sectional network parameters we estimated, where the youngest age category had the lowest mean degree and the oldest age category had the highest mean degree.

#### Conclusions
The FRP results correspond to the observed STI epidemics but not HIV epidemics between the cities. In San Francisco, rates of HIV have been declining over the last few years, whereas they have been steady in Atlanta. The FRP by age group resulted in fundamentally different conclusions about connectivity in the network compared with the cross-sectional network measures.

## Model Code
These models are written and executed in the R statistical software language. To run these files, it is necessary to first install our epidemic modeling software, [EpiModel](http://epimodel.org/), and our extension package specifically for modeling HIV/STI transmission dynamics among MSM, [EpiModelHIV](http://github.com/statnet/EpiModelHIV). 
