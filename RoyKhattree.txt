Testing the Hypothesis of a Kroneckar Product Covariance Matrix in Multivariate Repeated Measures Data
*http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.176.2096&rep=rep1&type=pdf*

**Authors** : Roy and Khattree

*Anuradha Roy, The University of Texas at San Antonio, San Antonio, TX  
*Ravindra Khattree, Oakland University, Rochester, MI  



<hr>
### CONCLUSIONS
Objective of this note was to provide a usable SAS algorithm to test the Kronecker product covariance
structure hypothesis using the likelihood ratio test. The algorithm presented here successfully completes
the task for all the data sets that we tried. 

While we have confined our alternative to the case when the repeated measures have AR(1) structure, the program can be suitably modified for other structures as well. More details about certain mathematical expression needed for such modification can be found in Roy (2002).

<hr>
###Introductory Section 
Data that contain multiple measurements over time on the same response variable for each subject
or experimental unit are very common, in many fields such as biomedical, pharmaceutical, industrial
engineering, business etc. This type of data are commonly called (univariate) repeated measures data.
Multivariate repeated measures data or doubly multivariate data are those where multiple measurements
are made over time on more than one response variable on each subject or unit.
<hr>

<hr>
The Likelihood Ratio Test
The likelihood ratio test compares the maximum value of the likelihood function L restricted to the
region defined by the null hypothesis Ho, to the maximum of likelihood function L in the unrestricted
region, Ha.

<hr>
*http://www.sciencedirect.com/science/article/pii/S1572312705000389*
Under the assumption of multivariate normality the likelihood ratio test is derived to test a hypothesis for Kronecker product structure on a covariance matrix in the context of multivariate repeated measures data. Although the proposed hypothesis testing can be computationally performed by indirect use of Proc Mixed of SAS, the Proc Mixed algorithm often fails to converge. We provide an alternative algorithm. The algorithm is illustrated with two real data sets. A simulation study is also conducted for the purpose of sample size consideration.
