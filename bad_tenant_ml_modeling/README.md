#  Define and Predict when a Tenant is Good or Bad From the Point of View of a Landlord 


## Executive Summary

- 10,200 tenants, with 1,254 holding more than one contract\*
- Majority of tenants (6,622) have contract history of at least 24 months
- 86.7% deposits returned while 5.1% partially returned and 8.2% forfeited
- 32 tenants appeared likely to have been evicted based on gaps in amount of expected rent and number of expected payments
- Almost all likely evictions had pattern of late rent payment and three-quarters paid the highest late fee on at least one occasion
- Random Forest on an upsampled version of the data offered the best performance as measured by AUC
- Contract history, age at the start of contract history, and use of cash for payment appeared to be most important in predicting bad tenants

*\*Does not include transactions without contract data*

## Exploratory Data Analysis<br><sup> Understanding tenant and contract history</sup>

### Contracts and Transactions Data
![](https://github.com/christianmconroy/Projects/blob/master/bad_tenant_ml_modeling/images/num_con_by_ten.png)

- Total of 11,579 contracts spread across the transactions and contracts data
- 55 contract ids in contracts data without transactions data and 1,980 transactions (55 contract ids) without contract data
- 10,200 tenants included (does not include transactions without contract data)
- 1,254 tenants held more than one contract
- No tenant has more than 4 contracts

### Contracts Length and History Data
<p float="left">
  <img src="https://github.com/christianmconroy/Projects/blob/master/bad_tenant_ml_modeling/images/length_contract_hist.png" width="500" />
  <img src="https://github.com/christianmconroy/Projects/blob/master/bad_tenant_ml_modeling/images/length_contract_by_tenant.png" width="500" /> 
</p>

- The majority of tenants (6,622) have a contract history of at least 24 months
- The second largest number of tenants had a history of 12 months or shorter
- While 3 month contracts were permitted, few tenants selected that option
- Not all contract history consists of finished contracts as there is no contract end date listed for tenants still within their contract as of 2017-12-31
- Length of contract history is therefore defined as length from start of first contract to 2017-12-31 for any tenant still within a contract

### Payments and Transactions
![](https://github.com/christianmconroy/Projects/blob/master/bad_tenant_ml_modeling/images/num_pay_meth_ten.png)

- 9,839 tenants used only one payment method while 309 used 2
- Bank transfer was the most used payment method followed by direct debit and then cash
- The deposit is always 3 times the amount of the monthly rent
- For the contracts that have ended, 86.7% received the deposit back while 5.1% received a partial deposit back
- 8.2% forfeited their deposit

| Deposit Status        | Number of Contracts|
| --------------------- |:------------------:|
| Contract Not Ended Yet| 9,270              |
| Deposit Returned      | 1,988              |
| Partially Returned    | 117                |
| Not Returned          | 188                |

### Evictions and Bad Tenants

- 32 tenants appeared likely to have been evicted based on having gaps on:
  -The amount of rent they paid versus how much they owed based on contract start and end date or 12/31/2017
  - The number of payments short they were based on how many payments they should have made based on contract start and end date of 12/31/2017
- Almost all had a pattern of late rent payment and three-quarters paid the highest late fee on at least one occasion

| Likely Eviction Cause | Number of Tenants                                              |
| -------------------------------------------------------------------------------- |:---:|
| Late payment more than 3 months                                                  | 31  |
| Rent lateness hitting or exceeding largest late fee (Half of monthly rent extra) | 24  |

## Developing a Predictive Model for Bad Tenants<br><sup> Methods and Evaluation</sup>

### The Target Variable
- Target created by setting a 25% threshold on the average of 8 normalized features indicative of bad tenant behavior 
- 25% threshold used to capture “bad” tenants and not just tenants likely to get evicted
- Aggregated values based on whatever the time period is for the tenant 
- The labelling window encompasses the about 72-month period between the first contract date and the last transaction payments recorded on 12/31/17
- Because transactions begin 12/02/2014, it is assumed that earlier payments were paid on time and tenants were charged 3x deposit rate
- The final dataset has 1,203 bad tenants and 10,266 good tenants

| Bad Tenant Indicator                                                                              | Format                                                                                                              |
| ------------------------------------------------------------------------------------------------- |:------------------------------------------------------------------------------------------------------------------:|
| Rent amount short  based on expected amount between contract start and end or 12/31/17            | Amount                                                                                                              |
| Number of rent payments short based on expected amount between contract start and end or 12/31/17 | Volume                                                                                                              |
| Number of late payments                                                                           | Volume                                                                                                              |
| Average late fee percentage                                                                       | Percent                                                                                                            |
| Maximum late fee percentage                                                                       | Percent                                                                                                            |
| Latest payment                                                                                    | Days                                                                                                                |
| Deposit Status                                                                                    | 0 for deposit returned or contract term not yet ended; 0.5 for partial return, and 1 for non-return               |

### The Features

- 8 features serve as input variables for the model
- The aggregated values are based on whatever the time period is for the tenant 
- A control variable is included to indicate whether a tenant has concluded contract history or is still within contract as of 12/31/17
- Percentages are used to control for the length of contract history and payment methods for contracts that began before 12/02/2014
- The labelling window encompasses the 72-month period between the first contract date and the last transaction payments recorded on 12/31/17

| Feature                           | Format  |
| --------------------------------- |:-------:|
| Contract History                  | Days    |
| Age at Start of Contract History  | Years   |
| Number of Contracts               | Volume  |
| Number of Payment Methods         | Percent |
| Tenant Reached End of Contract    | Boolean |
| Direct Debit Portion of Payments  | Percent |
| Cash Portion of Payments          | Percent |
| Bank Transfer Portion of Payments | Percent |

### The Approach

- 5 different supervised learning methods are evaluated
  - Logistic Regression
  - Decision Tree
  - Random Forest
  - Gradient Boosted Regression Tree
  - Support Vector Machine
- Due to class imbalance (bad tenants represented only 10.5% of the tenants overall), both upsampled and downsampled versions of the data are evaluated
- L1 regularization, or lasso, is applied where relevant for feature selection 
- Feature importance plots are employed to provide explainability for models 
- Random Search and Random Grid are used for hyperparameter optimization 
- Models are primarily compared based on the Area Under the ROC Curve (AUC) metric providing an aggregate measure of performance across classification thresholds
- AUC is more useful than accuracy as an evaluation measure due to class imbalance 

### Model Results - Full Dataset

| Model\*                          | AUC   | Precision | Recall    | F1 | Accuracy\*\* |
| -------------------------------- |:-----:| --------- | --------- | ----|-------------|      
| Logistic Regression              | 59.8% | 68%       | 21% | 32% | 32% | 90.5%       |
| Decision Tree                    | 59.8% | 68%       | 21% | 32% | 32% | 90.5%       |
| Random Forest                    | 60.5% | 40%       | 26% | 31% | 31% | 87.9%       |
| Gradient Boosted Regression Tree | 58%   | 72%       | 17% | 27% | 27% | 90.4%       |
| Support Vector Machine           | 60%   | 68%       | 21% | 32% | 32% | 90.5%       | 

*\ The above results are based on evaluating the trained model on a hold-out test set
*\*\ Accuracy is not a valuable metric here given imbalance with the target variable

