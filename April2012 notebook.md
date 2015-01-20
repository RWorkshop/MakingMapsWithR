

Exact description of repeated measures.
Sampling Protocols
Outline of Section 2
Statement of LME model
Carstensen's LME Model
Description of Roy's Three Tests
Calculation of LMEs (further to Roy and BXC)
Established methodologies for method comparison studies
Use of LME models
Replicate measurements
LME models for MCS
Formatting the data and variable names.
The Blood JSR data set
Implementing the variability tests.
Limits of Agreement computed using LME models.
BXCs modelling of MCS using LMEs
Limits of Agreement computed using LMEs
Variability Tests
Case Study 1 : Carstensens "Fat" Data set
Case Study 2 : Carstensens "Oximetry" Data set
Repeatability
Expansion to three method case
 
 
Objective of paper.
 
Propose and demonstrate the implementation (using R) of method comparison study, based on LME models, that will perform the following tasks;
 
1) Compute the inter-method bias between two methods of measurement.
2) Compute the limits of agreement.
3) Assess the between subject variability of both methods
4) Assess the within-subject variability of both methods
5) Further, assess the overall variability of both methods.
6) Determining the coeffiecient of repeatibility
 
This methodology is based on two existing approaches
1) Bendix Carstensen et al.      (tasks 1 ,2 and 6)
2) Anuradha Roy                    (tasks 1,3,4, 5 and 6)
 
 
    N.B. Biostatistics Style File 
 
Notation
The paper will follow Roy's notation and formulations

Sampling Protocols  
Paired tests
No matching
linked replicates
unlinked replicates
 
Carstensen sampling
 
Carstensen describes the sampling method employed on the fat dataswet.
Outline of Section 2
1) Statement of LME models
2) Description of Roy's Three tests
3) Calculations of LoA's (further to BXC)
 
 
Statement of LME model
In general, an LME model is any model which satsfies Laird and Ware (1982)
 
yi=Xi+Zibi+i
 
biN(0,D)
iN(0,Ri)
 
 
The matrix  represents the variance covariance matrix of two response variables at a given time point.
It is assumed that  doesnt depend on a given time point, and is the same for all time points.
 
 
Carstensen's LME Model
 
ymir=m+mi+cmi+emir
 
m+mi    Fixed effects ( can assume m= 1 for measurements in same units)
cmi+emir      Random interaction term and random measurement error
 
 
Limits of agreement
 
 
1-21.9622+12+22

 


Calculation of LMEs (further to Roy and BXC)
 
 Established methodologies for method comparison studies
Graphical methods, such as the Bland and Altman's difference plot and the scatterplot, have become ubiquitous.
Classical Approach to Limits of Agreement
Limits of agreement using Bland And Altmans approach
Alternative method of computing LoAs as prediction intervals, further to BXC2008.
" Limits of Agreement can only be interpreted as prediction limits for the difference between means of a series of measurements by both methods, which is not normally relevant" (BXC2008 pg 3)
 
Use of LME models
Echoing BXCs comments, the time has come to use computer based approaches to MCS, as opposed to remaining reliant on pen and paper methods.
Linear mixed effects models are very suitable for MCS.
What is an LME model?
 
Replicate measurements
Precise definition of replicate measurements
BXCs definition
BAs definition
Replicate measurements are an extra source of variance
Advised on how to deal with Replicate measurements in Hamlett. 
importance of the interaction term in BXCs model
 
               xy0
Compound Symmetric     	x2=y2
Symmetric                        
Formatting the data and variable names.
Both Roy and Carstensen require that method comparison data follow a specified formatting, using four variables.
Further to BXC Specific uses of variable names are advised (meth,repl,item,etc).
This allows users not proficient in R to quickly and easily edit the code.
The 4 models are inteplemented using variatons of code given at the end of this document.
The other models are implemented by interchanging the "compsymm" and"symm" as necessary.
 
The Blood JSR data set
Bland Altman 99 introduces a data set known as "Blood" (or "JSR") which Roy uses to demonstrate the model.
 
Implementing the variability tests.
What are the three tests?
R implementation using the NLME package. 
The necessary tests are performed using the "anova()" function in R. 
 
Limits of Agreement computed using LME models.
The estimates of the variance components are given directly in computer output and can be used directly to generate
limits of agreement and measures of repeatability for both methods.

BXCs modelling of MCS using LMEs
ymir=m+i+cmi+emir
 
Further to BXC2008 pg 4
Variation between items is  specified by m2
Variation within items is  specified by m2
 
Limits of Agreement computed using LMEs
The Limits of agreement are computed by 1-2222+12+22
Variability Tests
Variability tests proposed by Roy 2009 affords the opportunity to expand upon Carstensen's approach.
Implemententation using R
Precise Definition of a Likellihood Ratio test.
Test 1: Comparing the begin-subject variability of two methods
Test 2: Comparing the within-subject variability of two methods
Computing the coefficients of repeatability for both methods
Test 3: Comparing the overall variability of two methods 
 
Case Study 2 : Carstensens "Oximetry" Data set
BXC computes the LoAs for two cases.
BXC computed the LoAs as (-9.87,14.81) when an addtional interaction term is specified.
BXC computed the LoAs as (-12.18,17.12) when an addtional interaction term is not specified.
 
Repeatability
Repeatability can only be assessed when multiple measurements by each methods are available.
The repeatability is based on the residual standard deviation 22m= 2.83m
 
Expansion to three method case
Consider the VC structure
 
Compound Symmetric     	x2=y2=z2
Symmetric                        
 
The question arises of how to deal with the case when two of the three methods are considered equal (further to a 2-method test). 

Three Basic Types of Residuals in a Linear Model
clear definition of the fundamentally different types of residual in the linear model
All technical results applyu to the GLS solutions
[Haslett and Haslett 2007 ]
Residuals
1) Marginal Residuals
2) Model Specfied residuals
3) Full conditional residuals

Case Deletion Diagnostics
One step parameter estimates for random effects
Computationally efficient methods to refit models.
In the mixed model there is two types of residual – marginal and conditional
A marginal residual is the difference between the observed data and the estimated marginal
A conditional residual is the difference between the observed data and the predicted value of the observation.
Variance component may not be quantifiable

Desirable properties of residuals
Schabenberger [2004] influence diagnostics
subsets and singletons
Deletem = Replace

Deletion diagnostics
condtional delections


 
 
