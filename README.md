# axelrod
R functions for a repeated prisoner's dilemma.

## Intro
In the early 1980s, Robert Axelrod organized a competition to identify tactics that, in a prisoner's dilemma repeated 200 times, would generate the highest outcomes. I propose you to start it all again with 'axelrod', a tournament based on the same principle but with a more open competition and taking advantage of the progress made in computer science since.

The basic idea is to let anyone submit its own algorithm and to test them against each other during a tournament that will take place on the first Friday of March, each year. The winner of the tournament will be the algo with the highest average payoff.

What follows introduces the general principles of the tournament.

## Round
A round takes the form of a prisoner's dilemma where each algo - the *Player* and its *Opponent* - has the ability to cooperate (**TRUE**) or defect (**FALSE**). The payment matrix is that used by Axelrod in the 1980s:

 Player/Opponent  | TRUE | FALSE
 ------- | ------- | -------
TRUE | 3,3 | 0,5
FALSE | 5,0 | 1,1

In other words:
* If both players cooperate [TRUE, TRUE], they will both earn 3 points;
* If both players defect [FALSE, FALSE], they will both earn 1 point;
* If *Player* cooperates while *Opposent* defects [TRUE, FALSE], *Player* will get nothing and *Opposent* will earn 5 points;
* Symetrically, if *Player* defects while *Opposent* cooperates [FALSE, TRUE], *Player* will earn 5 points and *Opposent* won't get anything.

## Match
A match is composed of 200 successive rounds against the same opponent. For each round, your algo will receive the history of all the moves it made and all the moves its opponent made since the beginning of the match *but nothing else*.

All matches will be executed using the **Match** function (see code). Please use it to test your algo.

## Tournament
During the tournament, each algo will be confronted with all others **30 times** (since many algos use some randomness, it will help to stabilize the results). Your objective is to create the algo that will generate the highest average gain.

You can easily check that the minimum expected gain for one given match is equal to 0: this is what would happen to an algo that cooperates systematically with an algo that defects systematically (200 * 0 = 0). Symmetrically, the maximum expected gain equals 1000 (this is what the systematic defector would earn in my previous example: 200 * 5 = 1000). More reasonably, your gains on one single match are likely to range between 200 and 600.

You may test the performance of you algo using the **Tournament** function. Make sure that you have added your function to the list of competitors (**comp**).

## Specifications
These are the standards that we impose on developers. Its reading is a little technical if you do not know how to code but some important restrictions are accessible to all.

Your algo must be a R function with the following structure:

``` R
foo = function(p, o) {
    # do something
    return(res)
}
```
Where:
* `foo` is the name of you function: **you may not use the name of an existing R function** (any function from the `datasets`, `utils`, `grDevices`, `graphics`, `stats` and `methods` packages) and you may not use a name already attributed to one of your competitor;
* The arguments `p` and `o` are logical vector (e.g. `TRUE` or `FALSE`) representing the moves (cooperate of defect) of your algo (`p`) and the moves of you opponent (`o`) since the beginning of a match. At the beginning of the first round their length is 0 and they'll get one more item at each round. **You may not use any other argument** (a match will always be 200-rounds long: you don't need an aurgument for that).
* `res` is a logical vector of lenght 1: `TRUE` means that given `p` and `o` your algo cooperates and `FALSE` means it defects. **If your algo returns anything else, it will be disqualified**.

For instance, here is the code for *Tit-For-Tat*:

``` R
tft = function(p, o) {
	# Number of round already played + 1 = current round
	round <- length(p)+1
	# If this is the first round cooperate (res == TRUE),
	# else, just reproduce the last move of the opponent.
	res <- ifelse(round == 1, TRUE, tail(o, 1))
	return(res)
}
```
Let's simulate a first round :

``` R
> o <- p <- logical(0)
> tft(p, o)
[1] TRUE
>
```
Suppose the opponent has defected on the first round. Here is what will happen on the 2nd round:

``` R
> p <- TRUE
> o <- FALSE
> tft(p, o)
[1] FALSE
>
```
And so on and so forth...

## Restrictions
Here are the things you may not do:

* A 200-round match between your function and `tft` (see above) must not last more than 2 seconds (and that's already a lot);
* I will impose restrictions on additional packages, please ask before using one;
* I will not accept identical algos;
* You may not use other inputs than the arguments of your function (don't even try to look for the name of your opponent);
* You may not create a *king maker* (e.g. an algo designed to hurt one specific opponent);
* You may not exploit a potential bug in the testing environnement: if you find one, please tell me;
* You may only submit one single algo. If you submit more than one, I'll only use your latest.

## Submit
Feel free to submit your own algo using [this form](https://docs.google.com/forms/d/e/1FAIpQLSc8TlbrGz2mPecRDrqVwp5huqZFBsAOkwiDz2o3s_aFSFBsyg/viewform?usp=sf_link). 

There's nothing to gain but a good opportunity to learn and have fun.
