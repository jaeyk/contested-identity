# analyzing-list-experiments

**Analyzing list experiments using blocked random assignment**

## Motivation

In this project, [Taeku Lee](https://www.law.berkeley.edu/our-faculty/faculty-profiles/taeku-lee/) and I have tried to identify to what extent South Korean citizens hold biased attitude towards North Korean refugees. However, measuring sensitive attitudes in a survey is difficult because people tend to underreport their socially unpopular opinions, an effect also known as the social desirability bias. We overcame this problem by using a survey questionnaire technique called list experiment. In a list experiment, rather than bluntly asking respondents if they think North Koreans are less intelligent or if they waste tax dollars, respondents instead are given a list of statements and simply asked how many items they agreed with.

## Research design

### Random assignment
The experimental manipulation is in the statements that are read. In our version of the list experiment, respondents randomly assigned to the "control" condition receive three "naive" statement, that is three statements which are chosen for their irrelevance to the statement of interest; respondents randomly assigned to the "treatment" condition are read the same three naive statements and the statement of interest.  Since the researcher will not know which specific items the respondent agrees with, the respondent knows that their privacy is protected.

### Blocked random assignment
We assumed that political ideology is a covariate because the Korean political ideology has been known for its strong ties to the political attitude towards North Korea. We hypothesized that this relationship could also be demonstrated in the South Korean attitude towards North Korean refugees. For that reason, we did random assignment blocking on political ideology.

### Random ordering
The order of the two experiments is rotated so that we can circumvent the possible contamination effects of question order.

### Direct and indirect bias mesures
In addition, we measured both direct (stereotype) and indirect bias (more policy-driven) to analyze the main characteristic of the bias.

## Data collection: Mobile Survey using Matched Random Sampling

To reduce the cost of survey collection, we used a smartphone app-based survey. One problem with that approach is the non-representatives in the sample. We partially corrected that sampling bias by matching the online panel data, provided by the survey firm, with an existing probability sample before carrying out the survey. These experimental and sampling approaches are useful to elicit information on hidden attitudes from a limited sample.

## Workflow

In this article, I focus on the rest of the research process: **data wrangling, analysis, and visualization.**

### Matched random sampling

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

![](https://github.com/jaeyk/analyzing-list-experiments/blob/master/outputs/cate_comparison_plot.pngg)
Figure 2. Estimated Conditional Average Treatment Effect with or without Bootstrapped Confidence Intervals

## Conclusion
