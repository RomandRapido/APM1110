Summative Assessment 1
================

#### Probability and Probability Distribution

#### By Romand Lansangan

### 1)

A company has three factories producing a product. Factory 1 produces
$x_1$ of the product, Factory 2 produces $x_2$ , and Factory 3 produces
$x_3$ , where $\Sigma_{i=1}^3x_i = 1$ . The defective rates of the
products are $y_1$ , $y_2$ , and \$y_3% , respectively, where
$\Sigma_{i=1}^3y_i = 0.12$ . Write a program (user input for $x_i$ and
$y_1$ ) to calculate the probability that a randomly selected product is
defective.

#### Input algorithm for x:

    repeat {
      x <- 1;
      halu_x = c()
      while (x <= 3){
        var = as.double(readline(prompt = paste("Enter the probability of Factory ", x, " producing a product: ")))
        if (0.10 <= var && var <= 0.40){
          halu_x = c(halu_x, var)
          x <- x + 1;
        }else{
          print(paste("Probability should be greater than 0.10 and less than 0.40"))
        }
      }
      sum_x <- sum(halu_x);
      if (sum_x == 1){
        break
      }else{
        print("Probabilities must add up to 1")
      }
    }

### Input algorithm for y:

    repeat {
      y <- 1;
      halu_y = c()
      while (y <= 3){
        var = as.double(readline(prompt = paste("Enter the probability of Factory ", y, " to produce a defective: ")))
        if (0.10 <= var && var <= 0.40){
          halu_y = c(halu_y, var)
          y <- y + 1;
        }else{
          print(paste("Probability should be greater than 0.10 and less than 0.40"))
        }
      }
      sum_y <- sum(halu_y);
      if (sum_y == 0.12){
        break
      }else{
        print("Probabilities must add up to 0.12")
      }
    }

Note: I commented it out for the sake of presentation in markdown
format. Nevertheless, the code does work when ran through R compiler.

We shall use the following arbitrary values to calculate for the desired
probability.

``` r
halu_x <- c(0.33, 0.33, 0.34)
halu_y <- c(0.0322, 0.0637, 0.0241)
```

``` r
data_1 <- data.frame(
  factory = c(1:3),
  prob_production = halu_x,
  prob_defective_withRespectTo_factory = halu_y
  )
data_1
```

    ##   factory prob_production prob_defective_withRespectTo_factory
    ## 1       1            0.33                               0.0322
    ## 2       2            0.33                               0.0637
    ## 3       3            0.34                               0.0241

#### Checking if the values did met the criteria:

The sum of all x (prob_production) should be equals to 1,

``` r
sum(data_1$prob_production) == 1
```

    ## [1] TRUE

The sum of all y (prob_defective_withRespectTo_factory) should be equals
to 0.12,

``` r
tolerance <- .Machine$double.eps^0.5

sum(data_1$prob_defective_withRespectTo_factory) - 0.12 < tolerance
```

    ## [1] TRUE

Tolerance is a very small number to avoid floating-point arithmetic
issues.

``` r
tolerance
```

    ## [1] 1.490116e-08

#### Calculating for the probability of defective products among all the factories

Let $E$ be the event where a factory produces the defective product.

Let $A_1$ be the event where Factory 1 produces a product.

Let $A_2$ be the event where Factory 2 produces a product.

Let $A_3$ be the event where Factory 3 produces a product.

With that, the $E \cap A_k$ where $k \in \{1,2,3\}$ means that a product
produced from Factory $k$ is defective.

To calculate the probability of each factory producing a defection (with
respect to the whole sample space), we shall use the Multiplication Law
of Probability.

Given the event $E_1$ and $E_2$ , the Multiplication Law of Probability
is defined as the following:

$$
P(E_{1} \cap E_{2}) = P(E_{2}|E_{1})P(E_{1})
$$

Applying the Multiplication Law of Probability to the problem:

$$
P(E \cap A_1) = P(E|A_1)P(A_1)
$$

$$
P(E \cap A_2) = P(E|A_2)P(A_2)
$$

$$
P(E \cap A_3) = P(E|A_3)P(A_3)
$$

Note that $P(E|A_1)$ is the rate at which the Factory 1 produced a
defective. This goes the same for the rest of factories.

$$
P(E \cap A_1) = 0.0322 \times 0.33
$$

$$
P(E \cap A_2) = 0.0637 \times 0.33
$$

$$
P(E \cap A_3) = 0.0241 \times 0.33
$$

``` r
data_1$prob_defective_withRespectTo_all = data_1$prob_defective_withRespectTo_factory * data_1$prob_production
data_1
```

    ##   factory prob_production prob_defective_withRespectTo_factory
    ## 1       1            0.33                               0.0322
    ## 2       2            0.33                               0.0637
    ## 3       3            0.34                               0.0241
    ##   prob_defective_withRespectTo_all
    ## 1                         0.010626
    ## 2                         0.021021
    ## 3                         0.008194

$$
P(E \cap A_1) = 0.010626
$$

$$
P(E \cap A_2) = 0.021021
$$

$$
P(E \cap A_3) = 0.016147
$$

To calculate for $P(E)$ we shall use the Law of Total Probability.

The Law of Total Probability is defined as the following:

If a sample space $S$ can be partitioned into k mutually exclusive and
exhaustive events, $A_{1}$, $A_{2}$, $A_{3}$, … , $A_{k}$, then for any
event $E$:

$$
P(E) = P(A_{1})P(E|A_{1}) + P(A_{2})P(E|A_{2}) + P(A_{3})P(E|{A_{3}}) + ... P(A_{k})P(E|A_{k})
$$

Applying the Law of Total Probability to our problem,

$$
P(E) = P(E \cap A_1) + P(E \cap A_2) + P(E \cap A_3)
$$

Which is also,

$$
P(E) = 0.010626 + 0.021021 + 0.016147
$$

``` r
sum(data_1$prob_defective_withRespectTo_all)
```

    ## [1] 0.039841

The probability that a randomly selected product is defective is:

$$
P(E) = 0.047794
$$
