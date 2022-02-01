# Executive Summary

- 10,200 tenants, with 1,254 holding more than one contract*
- Majority of tenants (6,622) have contract history of at least 24 months
- 86.7% deposits returned while 5.1% partially returned and 8.2% forfeited
- 32 tenants appeared likely to have been evicted based on gaps in amount of expected rent and number of expected payments
- Almost all likely evictions had pattern of late rent payment and three-quarters paid the highest late fee on at least one occasion
- Random Forest on an upsampled version of the data offered the best performance as measured by AUC
- Contract history, age at the start of contract history, and use of cash for payment appeared to be most important in predicting bad tenants

## Exploratory Data Analysis<br><sup> Understanding tenant and contract history</sup>

### Contracts and Transactions Data
![](https://github.com/christianmconroy/Projects/blob/master/bad_tenant_ml_modeling/images/num_con_by_ten.png)

- Total of 11,579 contracts spread across the transactions and contracts data
- 55 contract ids in contracts data without transactions data and 1,980 transactions (55 contract ids) without contract data
- 10,200 tenants included (does not include transactions without contract data)
- 1,254 tenants held more than one contract
- No tenant has more than 4 contracts

### Contracts Length and History Data


