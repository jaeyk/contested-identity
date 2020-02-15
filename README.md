# analyzing-list-experiments

**Analyzing List Experiments Using Block Random Assignment**

## Motivation

In this project, [Taeku Lee](https://www.law.berkeley.edu/our-faculty/faculty-profiles/taeku-lee/) and I have tried to understand to what extent South Korean citizens hold a biased attitude towards North Korean refugees. However, measuring sensitive attitudes in a survey is difficult because people tend to underreport their socially unpopular opinions, an effect also known as the [social desirability bias](https://en.wikipedia.org/wiki/Social_desirability_bias). We overcame this problem by using a survey questionnaire technique called [list experiment](https://dimewiki.worldbank.org/wiki/List_Experiments). In a list experiment, rather than bluntly ask the respondents what they think about North Korean refugees, respondents are instead given a list of items that include a sensitive statement about North Korean refugees and simply asked how many items they agreed with. Taeku Lee and I contributed equally to the research design, and I was responsible for most of the data analysis.

## Research design

### Random assignment
The experimental manipulation is in the statements that are read. We divided survey participants into control and treatment groups. The control group was exposed to a list of naive statements (e.g., about the weather or sports). The treatment groups were exposed to an identical list plus one sensitive statement that accounts for ethnic bias. Subsequently, we asked them to report how many statements they supported. Since the researcher did not know which specific items a respondent agreed with, the respondents knew that their privacy was protected.

### Block random assignment
We assumed that political ideology is a covariate because the Korean conservative ideology has emphasized anti-Communism and the liberal political ideology has stressed ethnic unity. We hypothesized that this contrasting position toward North Korea could also be manifested in the way in which conservatives and liberals view North Korean refugees. For that reason, we did a random assignment blocking on political ideology. Random assignment ensures covariate balances between treatment and control groups by design. However, variability exists in sampling. Even though the differences-in-means are unbiased estimators of average treatment effects (ATEs; the difference between two potential outcomes), we should still worry about uncertainty around these estimates. For example, by chance, most liberals could be assigned to the control group, whereas most conservatives could be assigned to the treatment group. Even if an unbalanced assignment occurs, it does not mean that these individuals are selected into these groups. Nevertheless, in a circumstance like this, the difference-in-means becomes a less precise estimate of the ATE. Block random assignment reduces sampling variability by making sure a specific proportion of a subgroup of interest is assigned to treatment ([Gerber and Green 2012](https://isps.yale.edu/FEDAI): 73).

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

- There is nothing particular here. I dropped irrelevant columns from the survey data and changed key variable names to make them more intelligible.

### Data analysis and visualization [[Code](https://github.com/jaeyk/analyzing-list-experiments/blob/master/code/02_data_analysis.Rmd)]

- Average treatment effect (ATE): As alluded, I used difference-in-means as an estimator of the average treatment effect. As can be seen below, using `dplyr` is quite handy in dealing with multiple treatment groups and treatment conditions. I calculated 95% confidence intervals using two-paired t-tests.

- Conditional average treatment effect (CATE): Ideology was used as a blocking variable. Thus, ideology is unrelated to the assignment process. I subdivided the survey data according to the respondents' ideological position and calculated difference-in-means within each stratum. These differences-in-means are unbiased estimators of average treatment effects that are conditional on ideology.

- Bootstrapping confidence intervals: CATE requires subgroup analysis. Subgroup analysis reduces sample size and increases type II error (false negative). Particularly concerning is t-tests that are vulnerable to outliers. When we have a few observations, the effects of outliers could get stronger. To address this concern, I also calculated bootstrapped 95% confidence intervals. Bootstrapping is a non-parametric method, and it helps get more precise estimates of confidence intervals.

```{R}
diff_means_test <- function(data, treat, direct, indirect) {

  diff_summary <- data %>%

    # Summarize
    summarise_each(
      funs(

        # Different in means
        diff_t1 = mean(.[treat == 2], na.rm = T) - mean(.[treat == 1], na.rm = T),
        diff_t2 = mean(.[treat == 3], na.rm = T) - mean(.[treat == 1], na.rm = T),
        diff_t3 = mean(.[treat == 4], na.rm = T) - mean(.[treat == 1], na.rm = T),
        diff_t4 = mean(.[treat == 5], na.rm = T) - mean(.[treat == 1], na.rm = T),

        # Calculating confidence intervals
        conf_t1 = ((t.test(.[treat == 2], .[treat == 1])$conf.int[2]) - t.test(.[treat == 1], .[treat == 1])$conf.int[1]) / 2,
        conf_t2 = ((t.test(.[treat == 3], .[treat == 1])$conf.int[2]) - t.test(.[treat == 1], .[treat == 1])$conf.int[1]) / 2,
        conf_t3 = ((t.test(.[treat == 4], .[treat == 1])$conf.int[2]) - t.test(.[treat == 1], .[treat == 1])$conf.int[1]) / 2,
        conf_t4 = ((t.test(.[treat == 5], .[treat == 1])$conf.int[2]) - t.test(.[treat == 1], .[treat == 1])$conf.int[1]) / 2
      ),
      direct, indirect
    )

  diff_summary %>%
    gather(stat, val) %>% # stat = variables, val = values
    separate(stat, into = c("var", "stat", "treat"), sep = "_") %>% # var = measures, stat = diff or conf, group = treatment status, val = values  
    spread(stat, val) %>% # reorder columns
    mutate(var = replace(var, var == "direct", "Direct bias")) %>% # rename variables
    mutate(var = replace(var, var == "indirect", "Indirect bias"))

}
```

![](https://github.com/jaeyk/analyzing-list-experiments/blob/master/outputs/ate_results_plot.png)
Figure 1. Estimated Average Treatment Effects

Figure 1 shows the effects of estimated average treatment. We only find evidence of indirect bias toward North Korean refugees but not direct bias. Another noticeable fact is that the extent to which South Korean citizens hold a bias toward North Korean refugees is similar to their attitude toward Indonesian migrant workers.

![](https://github.com/jaeyk/analyzing-list-experiments/blob/master/outputs/cate_comparison_plot.png)
Figure 2. Estimated Conditional Average Treatment Effects with or without Bootstrapped Confidence Intervals

Figure 2 compares the estimated effects of conditional average treatment with or without bootstrapped confidence intervals. Interestingly, no strong partisan difference exists concerning South Korean citizens' attitudes towards North Korean refugees. Bootstrapped confidence intervals made very marginal differences (they were slightly narrower than the non-bootstrapped confidence intervals).

## Conclusion remarks

- In the additional analysis, we have found something interesting about how party ID interacts with responses. However, we are cautioned to make a strong claim about this pattern because we did not use party ID as a blocking variable. This relationship is an association.
- Ideological moderates are not exactly positioned in the middle. They could be leaning towards either side of the ideological spectrum. If that is the case, replacing their responses with NAs and then imputing them using multiple imputations could be a more precise way to investigate the causal effect of ideology on ethnic relations in South Korea.
- We would also note that list experiments have many limitations. As [this World Bank blog](https://dimewiki.worldbank.org/wiki/List_Experiments) nicely summarized, this design introduces noise to the data and potentially influences the treatment on the distribution of responses. [Blair and Imai](https://imai.fas.harvard.edu/research/files/listP.pdf) (2012) developed a set of statistical methods to address these problems.
