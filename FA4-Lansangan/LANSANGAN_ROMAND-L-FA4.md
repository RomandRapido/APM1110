Formative Assessment 4
================

#### Probability and Probability Distribution

#### By Romand Lansangan

### 1)

A geospatial analysis system has four sensors supplying images. The
percentage of images supplied by each sensor and the percentage of
images relevant to a query are shown in the following table:

``` r
dataTable_1 <- data.frame(
  Sensor = c(1,2,3,4),
  Percentage_of_Images_Supplied = c(15, 20, 25, 40),
  Percentage_of_Relevant_Images = c(50, 60, 80, 85)
)
```

![](https://github.com/RomandRapido/APM1110/blob/main/FA4-Lansangan/table1.png?raw=true)

What is the overall percentage of relevant images?

#### Solution:

This is a rather straight forward problem that only requires a
straightforward solution.

Before anything else though, let’s convert the percentage to their
decimal equivalent. I will also change the column names for readability
and accuracy purposes.

``` r
dataTable_1$Percentage_of_Images_Supplied <- dataTable_1$Percentage_of_Images_Supplied / 100
dataTable_1$Percentage_of_Relevant_Images <- dataTable_1$Percentage_of_Relevant_Images / 100
colnames(dataTable_1) <- c("Sensor", "Prob_of_Being_Supplied", "Prob_of_Providing_Relevant_Images")
```

![](https://github.com/RomandRapido/APM1110/blob/main/FA4-Lansangan/table2.png?raw=true)

Let $E$ be the event in which the sensor supplied a relevant image;

Let $A_{1}$ be the event in which the images was supplied to sensor \#1;

Let $A_{2}$ be the event in which the images was supplied to sensor \#2;

Let $A_{3}$ be the event in which the images was supplied to sensor \#3;

Let $A_{4}$ be the event in which the images was supplied to sensor \#4;

To get the overall probability (previously percentage) of relevant
images or the probability of relevant images with regards to the whole
sample space ( $P(E)$ ), we’ll first have to get the percentage of the
relevant images given to each sensors.

$$
P(E \cap A_{k})
$$

where $k \in \{1,2,3,4\}$ or the sensor number.

By Multiplication Law of Probability,

$$
P(E \cap A_{k}) = P(E|A_{k})P(A_{k})
$$

With that, we’ll have

``` r
dataTable_1$Prob_Relevant_Images_Relative_Sample_Space <- dataTable_1$Prob_of_Being_Supplied * dataTable_1$Prob_of_Providing_Relevant_Images
```

![](https://github.com/RomandRapido/APM1110/blob/main/FA4-Lansangan/table3.png?raw=true)

To get the overall percentage of relevant images, we can assert the Law
of Total Probability:

$$
P(E) = P(A_1 \cap E) + P(A_2 \cap E) + P(A_3 \cap E) + P(A_4 \cap E)
$$

$$
P(E) = 0.075 + 0.120 + 0.200 + 0.340
$$

``` r
sum(dataTable_1$Prob_Relevant_Images_Relative_Sample_Space)
```

    ## [1] 0.735

Thus, we have $0.735$ or $73.5$% as the overall percentage.

### 2)

A fair coin tossed twice.

Let $E_1$ be the event that both tosses have the same outcome, that is,
$E_1 = (HH, TT)$ . Let $E_2$ be the event that the first toss is a head,
that is $E_2 = (HH, HT)$ . Let $E_3$ be the event that the second toss
is a head, that is, $E_3 = (TH, HH)$ .

Show that $E_1$ , $E_2$ , and $E_2$ are pairwise independent but not
mutually independent.

#### Solution:

A pairwise independence is defined as:

$$
P(A \cap B) = P(A)P(B)
$$

Where A and B is mutually exclusive.

However, the three pair of events mentioned in the given can only become
mutually independent if:

    (a) Each pair is pairwise independent and
    (b) The intersection of all of them are mutually exclusive and is given by the following equation

$$
P(A \cap B \cap C) = P(A)P(B)P(C)
$$

##### a. Proving the piecewise independence.

When tossing a coin, the probability of getting a side (either heads or
tails) is $\frac{1}{2}$

Thus, when we toss two coins successively, the probability of all the
result will be:

$$
P(E_{k}) = \frac{1}{2}*\frac{1}{2}
$$

$$
P(E_k) = (\frac{1}{2})^2
$$

Where $k\in\{1,2,3\}$ .

$$
\forall k \in\{1,2,3\},
$$

$$
P(E_k) = (\frac{1}{2})^2 + (\frac{1}{2})^2
$$

$$
P(E_k) = \frac{1}{4} + \frac{1}{4}
$$

$$
P(E_k) = \frac{1}{2}
$$

Also, when tossing a coin twice, there are four possible cases,

$$
\{ (H_1, H_2),(T_1, T_2),(H_1, T_2), (T_1, T_2) \}
$$

Also, as one can notice, if we exhausts all the possible pairs of all
three given events, their intersections will just consists of 1 element.

For $E_1 \cap E_2$ , $E_2 \cap E_3$ , and $E_1 \cap E_3$ alike, its
“HH.” Giving us the probability of $\frac{1}{4}$ for all the pairs.

Meaning:

$$
P(E_1 \cap E_2) = \frac{1}{4}
$$

$$
P(E_2 \cap E_3) = \frac{1}{4}
$$

$$
P(E_3 \cap E_1) = \frac{1}{4}
$$

By definition of pairwise independence,

$$
P(E_1 \cap E_2) = P(E_1)P(E_2)
$$

$$
P(E_2 \cap E_3) = P(E_2)P(E_3)
$$

$$
P(E_3 \cap E_1) = P(E_3)P(E_1)
$$

Substituting the values, we’ll have

$$
\frac{1}{4} = \frac{1}{2}*\frac{1}{2}
$$

$$
\frac{1}{4} = \frac{1}{2}*\frac{1}{2}
$$

$$
\frac{1}{4} = \frac{1}{2}*\frac{1}{2}
$$

Which are all

$$
\frac{1}{4} = \frac{1}{4}
$$

Making all pairs pairwise independent.

##### b. Disproving mutual independence.

To test for the mutual independence however, we shall have to following:

$$
P(E_1 \cap E_2 \cap E_3) = P(E_1)P(E_2)P(E_3)
$$

$$
P(E_1 \cap E_2 \cap E_3) = \frac{1}{2}*\frac{1}{2}*\frac{1}{2}
$$

$$
P(E_1 \cap E_2 \cap E_3) = \frac{1}{8}
$$

But if we look at the possible outcomes, only “HH” can be found among
all three events. Thus making

$$
P(E_1 \cap E_2 \cap E_3) = \frac{1}{4}
$$

However, $\frac{1}{4} \neq \frac{1}{8}$ .

Making $E_1$ , $E_2$ , and $E_3$ NOT mutually independent.

Therefore, $E_1$ , $E_2$ , and $E_2$ are pairwise independent but not
mutually independent.
