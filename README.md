# Replication data and code 

- Co-authors: Jae Yeon Kim (jkim638@jhu.edu, corresponding author), [Taeku Lee](https://www.law.berkeley.edu/our-faculty/faculty-profiles/taeku-lee/) 

- Paper: Contested Identity and Prejudice Against Co-ethnic Refugees: Evidence from South Korea (Accepted at *Political Research Quarterly*)

## Session information 

**Session information**

1. Programming languages

* R version 4.0.4 (2021-02-15)
* Python 3.8.8
* Bash 5.1.4(1)-release

2. Operation system

* Platform: x86_64-pc-linux-gnu (64-bit)
* Running under: Ubuntu 21.04

## Research design

### Random assignment

The experimental manipulation is in the statements that are read. We divided survey participants into control and treatment groups. The control group was exposed to a list of naive statements (e.g., about the weather or sports). The treatment groups were exposed to an identical list plus one sensitive statement that accounts for ethnic bias. Subsequently, we asked them to report how many statements they supported. Since the researcher did not know which specific items a respondent agreed with, the respondents knew that their privacy was protected.

#### Blocking and Stratifying

When randomly assigning participants to different conditions, we blocked on political ideology and stratified on region to reduce sampling variance in the data and increase statistical power ([Gerber and Green 2012](https://isps.yale.edu/FEDAI): 73). 

### Random ordering

The order of the two experiments is rotated so that we can circumvent the possible contamination effects of question order.

### Direct and indirect bias measures

In addition, we distinguished direct (stereotype) and indirect bias (more policy-driven) and measured both of them. For want of space, I did not delve into why their differences matter from the perspective of political psychology and racial and ethnic politics.

## Data collection: Mobile Survey Using Matched Random Sampling

To reduce the cost of survey collection, we used a smartphone app-based survey. The mobile survey is good as increasingly more people only use mobile phones. The 2015 National Health Interview Survey conducted by the National Center for Health Statistics reports that nearly one-half of American homes (47.4%) use only wireless telephones. Using a smartphone app-based survey is particularly relevant in South Korea as the country is the most wired in the world. The South Korean smartphone ownership rate is the world's highest according to the 2016 Pew Research Center report. Among the 18–34 age group, the rate was 100%. Even for the 35+ age group, the rate was still impressive at 83%.

One problem with the smartphone app approach is the non-representativeness of the sample. The online panel provided by the survey firm we hired uses an opt-in sample. As such, it does not guarantee a representative sample. To tackle that problem, we used a matched sampling method advocated by previous studies ([Rivers 2007](https://static.texastribune.org/media/documents/Rivers_matching4.pdf)). The survey firm and I matched the firm's online survey pool with a randomly chosen subset of a nationally representative sample. We then recruited participants from the online survey pool from that list. The step by step procedure is as follows.

1. I created a target sample by randomly choosing a sample of 2,303 from the 2015 Korea Welfare Panel Study (KOWEPS), the Korean equivalent of the U.S. Panel Study of Income Dynamics (PSID). The KOWEPS sample size is N=16,664.

2. After creating the target sample, I helped the survey firm to match the target sample with their survey pool based on age, gender, income, education level, occupation, and region. Since the survey pool data is proprietary, most of the matching process was carried out by the survey firm with technical assistance from me.

3. The survey firm recruited participants using this matched sample as a sampling list. We did a pre-survey, as the survey pool data did not include political ideology and other covariates of interest. I randomly created five groups (one control and four treatment groups) blocking on ideology. Overall, 1,543 people agreed to participate in the pre-survey (67% response rate). Of them, 1,418 participated in the main survey (92% retention rate).

## Workflow

In the rest of the document, I state how I have **wrangled**, **analyzed**, and **visualized** data. I shared the R code I used in each step.

### Data wrangling [[Code](https://github.com/jaeyk/analyzing-list-experiments/blob/master/code/01_data_wrangling.Rmd)]

- There is nothing particular here. I dropped irrelevant columns from the survey data, changed key variable names to make them more intelligible, and created some dummy variables.

### Data analysis [[Code](https://github.com/jaeyk/analyzing-list-experiments/blob/master/code/02_data_analysis.Rmd)]

- Average treatment effect (ATE): We used the average treatment effects (ATEs), the difference between the mean of the treatment ($E[Y_{i}(1)]$) and that of the control group ($E[Y_{i}(0)]$), to estimate the causal effects of the manipulations. This estimator is unbiased because random assignment ensures independence between the treatment condition and the potential outcomes.

- Multivariate regression analysis: We investigated what factors contribute to South Koreans' direct and indirect biases against North Korean refugees. Using the ['list'](https://cran.r-project.org/web/packages/list/list.pdf) package in R. We constructed a regression model as specified below. We then fitted it to three subgroups of co-ethnic data (North Korean refugees, low-income South Koreans, and Korean Chinese migrants). We used either partisanship or ideology variables across these models, as they are highly correlated. If changing these variables does not alter results, then the reliability of the findings increases. Other covariates were selected due to their known relationship with partisanship or ideology variables.

$Y_{i} = \beta_{0} + \beta_{1}\textrm{Partisanship/Ideology} + \beta_{2}\textrm{Gender}_{i} + \beta_{3}\textrm{Age}_{i} + \beta_{4}\textrm{Income}_{i} +
\beta_{5}\textrm{Education}_{i} + \epsilon$

### Difference-in-means analysis [[Outputs](https://github.com/jaeyk/analyzing-list-experiments/blob/master/outputs/)]

![](https://github.com/jaeyk/analyzing-list-experiments/blob/master/outputs/ate_results_plot.png)
Figure 1. Estimated Average Treatment Effects

## Multivarite regression analysis [[Outputs](https://github.com/jaeyk/analyzing-list-experiments/blob/master/outputs/)]

![](https://github.com/jaeyk/analyzing-list-experiments/raw/master/outputs/multi_combined.png)

Figure 2. Multivariate regression analysis outputs
