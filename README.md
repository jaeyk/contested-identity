# analyzing-list-experiments

**Analyzing list experiments using blocked random assignment**

## Motivation

In this project, [Taeku Lee](https://www.law.berkeley.edu/our-faculty/faculty-profiles/taeku-lee/) and I have tried to understand to what extent South Korean citizens hold biased attitude towards North Korean refugees. However, measuring sensitive attitudes in a survey is difficult because people tend to underreport their socially unpopular opinions, an effect also known as the [social desirability bias](https://en.wikipedia.org/wiki/Social_desirability_bias). We overcame this problem by using a survey questionnaire technique called [list experiment](https://dimewiki.worldbank.org/wiki/List_Experiments). In a list experiment, rather than bluntly asking respondents what they think about North Korean refugees, respondents instead are given a list of items which include a sensitive statement about North Korean refugees and simply asked how many items they agreed with. Taeku and I equally contributed to the research design and I was responsible for the most of the data analysis.

## Research design

### Random assignment
The experimental manipulation is in the statements that are read. We divided survey participants into control and treatment groups. The control group was exposed to a list of naive statements (e.g., about the weather or sports). The treatment groups were exposed to an identical list plus one sensitive statement that accounts for an ethnic bias. Subsequently, we asked them to report how many statements they supported. Since the researcher will not know which specific items the respondent agrees with, the respondent knows that their privacy is protected.

### Block random assignment
We assumed that political ideology is a covariate because the Korean conservative ideology has emphasized anti-Communism and the liberal political ideology has stressed ethnic unity. We hypothesized that this contrasting position toward North Korea could also be manifested in the way in which conservatives and liberals view North Korean refugees. For that reason, we did random assignment blocking on political ideology. Random assignment ensures covariate balances between treatment and control groups by design. However, there exists sampling variability. Even though the difference-in-means are unbiased estimators of average treatment effects (ATEs; the difference between two potential outcomes), we still should worry about uncertainty around these estimates. For example, by chance, most liberals could be assigned to the control group whereas most conservatives could be assigned to the treatment group. Even if unbalanced assignment occurs, it does not mean that these individuals are selected into these groups. Nevertheless, in a circumstance like this, the difference-in-means become a less precise estimate of the ATE. Block random assignment reduces sampling variability by making sure a specific proportion of a subgroup of interest will be assigned to treatment ([Gerber and Green 2012](https://isps.yale.edu/FEDAI): 73).

### Random ordering
The order of the two experiments is rotated so that we can circumvent the possible contamination effects of question order.

### Direct and indirect bias mesures
In addition, we distinguished direct (stereotype) and indirect bias (more policy-driven) and measured both of them. For the sake of the space, I did not delve into why their differences matter from the perspective of political psychology and racial and ethnic politics.

## Data collection: Mobile Survey using Matched Random Sampling
To reduce the cost of survey collection, we used a smartphone app-based survey. Mobile survey is good as increasingly more people only use mobile phones. The 2015 National Health Interview Survey conducted by the National Center for Health Statistics reports that nearly one-half of American homes (47.4%) only use wireless telephones. Using a smartphone app-based survey is particularly relevant in South Korea as the country is most wired in the world. The South Korean smartphone ownership rate is the world's highest according to the 2016 Pew Research Center report. Among the 18-34 age group, the rate was 100%. Even over the 35+ age group, the rate was still impressive 83%.

One problem with that approach is the non-representatives in the sample. The online panel provided by the survey firm we hired uses an opt-in sample. As such, it does not guarantee a representative sample. To tackle with that problem, we used a matched sampling method advocated by previous studies ([Rivers 2007](https://static.texastribune.org/media/documents/Rivers_matching4.pdf)). The survey firm and I matched the firm's online survey pool with a randomly chosen subset of a nationally representative sample. We then recruited participants from the online survey pool from that list. The step by step procedure is as follows.

1. I created a target sample by randomly choosing a sample of 2,303 from the 2015 Korea Welfare Panel Study (KOWEPS) the Korean equivalent to the U.S. Panel Study of Income Dynamics (PSID). The KOWEPS sample size is N=16,664.

2. After creating the target sample, I helped the survey firm to match the target sample with their survey pool based on age, gender, income, education level, occupation, and region. Since the survey pool data is proprietary, most of the matching process was carried out by the survey firm with the technical assistance from me.

3. The survey firm recruited participants using this matched sample as a sampling list. We did a pre-survey as the survey pool data did not include political ideology and other covariatres of interest. I randomly created five groups (one control and four treatment groups) blocking on ideology. Overall, 1,543 participants agreed to participate in the pre-survey (67% response rate). Of them, 1,418 participated in the main survey (92% retention rate).

## Workflow

In the rest of the document, I document how I have **wrangled**, **analyzed**, and **visualized** data. I shared the R code I used in each step.

### Data wrangling [[Code](https://github.com/jaeyk/analyzing-list-experiments/blob/master/code/01_data_wrangling.Rmd)]

### Data analysis and visualization [[Code](https://github.com/jaeyk/analyzing-list-experiments/blob/master/code/02_data_analysis.Rmd)]

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
Figure 1. Estimated Average Treatment Effect

![](https://github.com/jaeyk/analyzing-list-experiments/blob/master/outputs/cate_comparison_plot.png)
Figure 2. Estimated Conditional Average Treatment Effect with or without Bootstrapped Confidence Intervals

## Conclusion remarks

- We have found something interesting about the relationship between party ID and responses. However, as I did not use party ID as a blocking variable, this relationship is an association.
- List experiments also have many problems. As [this World Bank blog](https://dimewiki.worldbank.org/wiki/List_Experiments) nicely summarized, this design introduces noise to the data and potentially influences the treatment on the distribution of responses. [Blair and Imai](https://imai.fas.harvard.edu/research/files/listP.pdf) (2012) developed a set of statistical methods to address these problems.
