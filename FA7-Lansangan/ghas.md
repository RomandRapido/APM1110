Formative Assessment 7
================

#### Probability and Probability Distribution

#### By Romand Lansangan

### 1) In Example 16.3 with $\lambda = 4$ per minute, use R to obtain:

``` r
lambda_1 <- 4
```

#### (a)

$P(T \leq 0.25) = P($ time between submissions is at most 15 seconds $)$
;

``` r
pexp(0.25, rate =lambda_1)
```

    ## [1] 0.6321206

#### (b)

$P(T > 0.5) = P($ time between submissions is greater than 30 seconds
$)$ ;

``` r
1 - pexp(0.5, lambda_1)
```

    ## [1] 0.1353353

Note: this is the complementary approach or $1 - P(T \leq 0.4)$

### (b)

$P(0.25 < T < 1) = P($ time between submissions is between 15 seconds
and 1 minute $)$ ;

``` r
pexp(1, lambda_1) - pexp(0.25,lambda_1)
```

    ## [1] 0.3495638

### 2) The average rate of job submissions in a computer center is 2 per minute. If it can be assumed that the number of submissions per minute has a Poisson distribution, calculate the probability that:

``` r
lambda_2 <- 2
```

#### (a) more than two jobs will arrive in a minute;

or

$P(X > 2) = 1 - P(X \leq 2)$

In terms of Poisson probability.

``` r
1 - ppois(2, lambda_2)
```

    ## [1] 0.3233236

#### (b) at least 30 seconds will elapse between any two jobs;

or $P(X > 0.5) = 1 - P(X \leq 0.5)$

``` r
1 - pexp(0.5, lambda_2)
```

    ## [1] 0.3678794

In terms of exponential probability.

#### (c) less than 30 seconds will elapse between jobs;

or

$P(X < 0.5) = P(X \leq 0.5)$

In terms of Exponential probability.

``` r
pexp(0.5, lambda_2)
```

    ## [1] 0.6321206

#### (d) a job will arrive in the next 30 seconds, if no jobs have arrived in the last 30 seconds.

or

$P(X \leq 1 | X < 0.5) = P(X \leq 0.5)$

due to the fact that Markov or “memoryless property” holds in
Exponential probability.

So the answers the same as (c)

``` r
pexp(0.5, lambda_2)
```

    ## [1] 0.6321206

### 3) A website receives an average of 15 visits per hour, which arrive following a Poisson distribution.

``` r
lambda_3 <- 15
```

#### (a) Calculate the probability that at least 10 minutes will elapse without a visit.

``` r
10 / 60
```

    ## [1] 0.1666667

or

$P(X > 0.1666667) = 1 - P(X \leq 0.1666667)$

In terms of Exponential distribution.

``` r
1 - pexp(0.1666667, lambda_3)
```

    ## [1] 0.08208496

#### (b) What is the probability that in any hour, there will be less than eight visits?

$P(X < 8) = P(P \leq 7)$

In terms of Poisson distribution.

``` r
ppois(7, lambda_3)
```

    ## [1] 0.01800219

#### (c) Suppose that 15 minutes have elapsed since the last visit, what is the probability that a visit will occur in the next 15 minutes.

``` r
15/60
```

    ## [1] 0.25

or

$P(X \leq 0.50 | X < 0.25) = P(X \leq 0.25)$

due to Markov or “memoryless property.”

``` r
pexp(0.25, lambda_3) 
```

    ## [1] 0.9764823

4)  Calculate the top quartile, and explain what it means.

``` r
qpois(0.75, lambda_3)
```

    ## [1] 18

This indicates that 75% of the time (top quartile), 18 or fewer visits
will occur in the website
