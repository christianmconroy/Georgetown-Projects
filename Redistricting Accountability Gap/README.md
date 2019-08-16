# Redistricting-Accountability-Gap
Developing Means for Assessing the Impact of Gerrymandering on Accountability

Problem 
State legislatures have through a practice known as gerrymandering hijacked U.S. representative democracy by designing electoral districts to include or exclude specific demographic and political groups that are disadvantageous to the interests of one political party. In Maryland, for example, lawmakers redrew districts after 2010 to ensure that only one Republican could be elected in a state where 40 percent of voters reportedly lean Republican. In Pennsylvania, the Republican-dominated state legislature packed Democratic-leaning voters into a few districts where Democratic candidates would win by an average of 76 percent, diluting voting strength and providing Republican candidates victory in 13 of 18 elections. Using an “efficiency gap” metric that identifies votes cast in excess of what a candidate would need to win (packing) or for a losing candidate by a wide margin (cracking), Stephanopoulos and Mcghee (2015) have shown an increase in gerrymandering nationwide from 0.94 in the 1980s to 6.07 percent in 2012. Far from giving the average citizen agency in electing a representative who will actively address the insecurities he or she faces, the modern system of gerrymandered democracy disenfranchises citizens by creating artificial fiefdoms unresponsive to the needs of the citizens living within.

With the next decennial census in 2020 presenting another opportunity for state legislatures to further use the redistricting process to protect incumbency and marginalize voter agency, it is increasingly important that we work to create a fair and equitable redistricting process now. 

Intervention 
To show how pernicious gerrymandering schemes have decreased political representation and accountability, I developed a visualization tool that used randomly generated maps meeting standard redistricting criteria to show how opportunistic gerrymandering has significantly hurt the alignment of policy priorities between district constituents and those elected to supposedly represent them. The visual tool communicates clearly to the average voter the decline in political representation that comes with gerrymandering. The goal is to eventually scale the tool to other states and the national level and integrate into wider outreach platforms that will inspire political action centered around independent redistricting commissions and free and fair redistricting criteria. 

The initial analysis phase consisted of the following steps: 

1. Break down precinct map of Virginia into adacency list. Given that some small geographic gaps in the electoral precinct map are irrelevant, it was necessary to construct a small buffer around each precinct and then intersect with the original precincts. 

2. Apply [random sampling algorithm](https://github.com/mpancia/virginia_mapmaking/tree/9226694e01beaab97c24006b8e37bc1a6f2983b5) to produce a sample of 234 maps that apportion thousands of precincts into 100 state house electoral districts. The process involves picking a bunch of random precincts and then randomly growing them by adding in neighbors until reaching a fully parsed map. 

The sampling algorithm applied an MCMC procedure that takes into account the entropy of the political party distribution for each district as well as the population variation. Criteria applied include: 

 - Compactness: Having the minimum distance between all the parts of a constituency (convex hull compactness used here)
 - Contiguity: All parts of a district being connected at some point with the rest of the district
 - Population equality: All districts must have a population with a variance of less than 5 percent from the mean across districts
 - Racial Dissimilarity: Measures the percentage of a group’s population that would have to change residence for each neighborhood to have the same percentage of that group as the metropolitan area overall
 - Political Competition: Defined as having the margin between Dem/Repub as less than 20%

3. Access the Data

Political Accountability and Policy Liberalism

First thing was to use Maptitude for Redistricting to access a dataset that included 2013 Virginia House of Delegates election results, as well as demographic and household information from the 2010 Census. 
Drawing from the methodology introduced by Shor and McCarty (2011), I use the absolute value of distance between the ideal points of elected legislators and the median voter of his or her district as a proxy for how closely a state legislator is representing the policy priorities of his or her constituency. 
*Legislator Ideal Points*
An ideal point is a normalized state-level public ideology metric derived from applying multilevel regression with poststratification to aggregate data on how often state legislators vote with other legislators on roll call votes and information from the National Political Awareness Test (NPAT) questionnaire administered by Project Vote Smart to state representatives every year starting in the 1996. The questions on the NPAT questionnaire cover a number of common policy concerns, including:
* Social welfare (e.g., AFDC/TANF benefit levels)
* Taxation
* Labor (e.g., right-to-work)
* Civil rights (e.g., fair housing laws)
* Women’s rights (e.g., jury service for women)
* Morals legislation (e.g., antisodomy laws)
* Family planning (e.g., ban on partial birth abortion)
* The environment (e.g., state endangered species acts)
* Religion (e.g., public schools allowed to post Ten Commandments)
* Criminal justice (e.g., death penalty), and 
* Drugs (e.g., marijuana decriminalization)

A larger (positive) number indicates that a legislator is voting more with other conservatives and smaller (negative) numbers that he or she is voting more consistently with liberals. 
*Median Voter Policy Preferences*
To estimate where the median voter for each district falls, we employ information generated by using multilevel regression with poststratification (MRP) to generate estimates of the policy preferences of citizens (Tausanovitch and Warshaw 2013). Zip-code level information on the policy preferences of 275,000 Americans came from seven recent large-scale surveys, including the 2006, 2007, 2008, 2010, and 2011 Cooperative Congressional Election Surveys (CCES) and the 2000 and 2004 Annenberg National Election Surveys (NAES). The majority of the surveys asked between 14 and 32 policy questions that could all be dichotomized into binary variables. The number of respondents and policy items for each survey are included below: 

First Header  | Second Header| Third Header
------------- | ------------- | -------------
2010 CCES module | 1,300 | 136
2011 CCES module | 2,500 | 41
2006 CCES | 30,000 | 16
2007 CCES | 10,150 | 14
2008 CCES | 32,800 | 15
2010 CCES (Common Content) | 55,000 | 22
2011 CCES (Common Content) | 20,000 | 14
2000 NAES | 58,400 | 28
2004 NAES | 81,400 | 25

The policy preferences of respondents to the surveys were jointly scaled using an item response model previously employed by other scholars evaluating the median ideal points of legislators at the state and national levels (Clinton, Jackman, and Rivers 2004; Shor and McCarty 2011). From there, MRP is employed to model the policy preferences of citizens based on the demographic features of different levels of geography (Lax and Phillips 2009b; Park, Gelman, and Bafumi 2004; Warshaw and Rodden 2012). To enable comparison between the median ideal points of elected House of Delegates representatives and the median policy preference of citizens down to the zipcode level, a survey asking questions identical to those asked previously on CCES and NAES surveys was administered to 1,300 respondents in order to calibrate NPAT, CCES, and NAES responses to a common scale. 

In order to compare our metric of political accountability with the dominant metric used to evaluate gerrymandering today, the efficiency gap is also produced for each of the 100 Virginia House of Delegates electoral districts. The efficiency gap quantifies gerrymandering by measuring the difference in the votes the winning party receives above that needed to win and the amount the losing party receives above that needed to concede over the total number of votes cast (Stephanopoulos and McGhee 2015). Given the natural variation that comes from both citizens of each party naturally self-selecting into specific geographic locations and efforts made to ensure the representation of racial minorities, the threshold proposed for concluding that a particular district has been gerrymandered and is therefore in violation of the constitution is 7 percent.  

4. Merge the Layers

The citizen policy preference data collected by Tausanovitch and Warshaw is collected at the zipcode level. Given that the borders of zipcodes and the electoral precincts used as the base layer for constructing the randomly generated new maps do not align perfectly, geographical aggregation was employed in order to generate an estimate of the proportion of residents that would fall in each precinct when a precinct boundary intersects that of a zipcode. 

To accomplish this, we first import a shape file of all the zipcode polygons in the US. After ensuring that the two use the same coordinate reference system (CRS), we clip the US shapefile to only those zipcode polygons within the defined boundary of the state of Virginia. From there, we import the Tausanovitch and Warshaw dataset containing the quantification of where each respondent to one of the seven surveys described above falls on the policy liberalism scale coded at the zipcode level. 

After ensuring that the two use the same CRS, we attach the collapsed zipcode level data to the shape file of zipcodes for the state of Virginia. We then calculate the intersection of subsets of probability space to overlay the zipcode level data on top of the precincts of the randomly generated maps and aggregate upwards to get a district average for all 100 districts for each map drawn for the Virginia House of Delegates through the MCMC method. 

5. Results and Comparison
## The Current Map ##
The Virginia House of Delegates District Map that was in place for the 2013 electoral season maintains a -0.946 value on the policy liberalism spectrum, indicating relatively extreme conservatism on average across state legislators. 

On average, the mean difference between where a legislator and the median voter of his or her constituency respectively fall on the policy liberalism scale is 0.654 and the margin of victory of a Republican candidate over a Democratic candidate and vice versa is 0.511. 

The extremity on the policy scale makes sense given that the efficiency gap of the map was 15.3%, a number more than twice the 7% constitutional violation threshold Stephanopoulos and McGhee employ (Howard 2017). 

## The New Maps ##
Under the new maps, the average efficiency gap across the 234 maps is 3.34%. Policy liberalism has been reduced to .019, a number that far better captures the centrism shared by many voters across the state. 

WORK IN PROGRESS. Editing.



