Problem Statements:
1.	What are the features of suspicious accounts and the activity of those accounts that are likely to get them suspended by Twitter? How do the features that get them suspended differ across different actors (i.e. China, Russia, Iran, etc. )? 
2.	How are accounts changing behavior or adapting to evade Twitter detection and suspension? 
3.	At what rates are suspicious accounts being suspended by Twitter once created and once activated from dormancy? 

Approach:
1.	Pull 10,000 of the most recent followers of the accounts of @SpokespersonCHN, @zlj517, and @HuXijin_GT, respectively (30,000 total) using the TWINT script
2.	Prune the dataset to get the accounts that are most likely to be suspended by looking at large spikes in account creation dates, only those with no location dates, and bio similarity (potentially) (There is already a script established to examine this)
3.	Use the TWINT script to pull information on those accounts each week until sufficient suspended account sample size 
4.	Pull weekly Brandwatch pulls (Do on Saturday to avoid impacting ongoing TA requests) to get weekly post information on accounts and assess which have been suspended until sufficient suspended account sample size
5.	Conduct exploratory data analysis to evaluate average duration of existence before suspension, proportion of accounts suspended, etc. 
6.	Develop classification models that use both dynamic account information (change in follower/following numbers for example) and NLP on posts as covariates and suspension status as target variable
7.	Analyze features with strong relationship to suspension and evaluate qualitatively factors behind accounts in same sample evading suspension
Intended Purpose: This can be used to inform product generation, tool development, and interagency and intercountry collaboration. The resulting paper can be used for both internal external (paper submission and conferences) purposes provided the right permissions are granted. 

