# DAG-Price-Discovery-Paper
Codes to implement the DAG methodology for price discovery via Information Shares. 
The DAG methodology is entirely based on the LiNGAM approach introduced by Shimizu et al.(2006) published on the Journal of Machine Learning Research
and cited in my paper, meaning in this repository you will find their codes (just slightly modified in some parts). 
This README thus contains a quick guide to understand the different steps and logic of the code.


The repository is composed of 6 key scripts:

1) dataimport&processing: This script take the original TAQ data for IBM and created the event-time and natural-time (1-second resolution) datasets upon which the methodology will be implemented. NOTE: The underlying TAQ data cannot be disclosed publicly and should be purchased. For further information please contact myself directly. The VECM should be estimated on these datasets containing trades and quotes data. You should then extract the residuals of the models: All the next scripts should be runned on the residuals of the VECM, the identification process is on the transitory shocks (short-term identification)!

2, 3, 4) lingam, estimate, prune: These 3 functions are needed to perform the DAG methodology to identify the causal chain on the transitory shocks. The script 'lingam' perform the lingam analysis and runs on top of the estimate and prune functions. The script 'estimate' perform the ICA estimation procedure + columns and rows permutation to get the DAG chain, while the script 'prune' perform the pruning edges method to eliminate a link in the causal chain when the coefficient is not statistically significant.

supportingfunctions: To run the scripts 'lingam','estimate', and 'prune' we need some supporting functions which are contained in this script.

One practical example of the procedure performed on real data can be found in the script IBMlistVSother-1sec (remember that the data are not located in this repository).

THE ZIP FOLDER VARLiNGAM, PLACED HERE JUST FOR CONVENIENCE OF THE READERS, CONTAINS THE CODES ORIGINALLY WRITTEN BY THE AUTHORS OF THE LiNGAM PROCEDURES WHICH I HAVE EXPLOITED TO PROPOSE THE DAG METHODOLOGY IN THE PRICE-DISCOVERY CONTEXT. I AM NOT THE ORIGINAL AUTHOR OF THE LiNGAM CODES WHICH CAN BE FOUND EITHER AT
https://www.shimizulab.org/lingam UNDER THE 'ICA-based LiNGAM' OR https://sites.google.com/site/dorisentner/publications/VARLiNGAM.
