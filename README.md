Repository containing the scripts used for the analysis done in the manuscript "Tractable inference for latent ability in competitive games: approximating bayesian rank-ordered models using a simpler proportion model".


The project focuses on the evaluation of the Beta Regression model as an approximation to the Rank-Ordered logit model (ROL) in the inference of competitors' latent abilities in competitive games.

The ROL model can be considered the gold standard approach to produce this kind of inference. Following the approach used in Glickman and Hennessy (2015), under the ROL model, the ranking of competitors given in each game can be directly modeled from a vector of parameters, each of them representing the latent ability of a ranked competitor. Understanding how this model work is not straightforward and its estimation becomes computationally intensive when the number of ranked competitors is large. The use of the Beta Regression model in the estimation of competitors' ability is inspired by what was done in an earlier version of Van Kesteren and Bergkamp (2023). This approach is based on the transformation of the rank of each competitor in each game into the proportion of outperformed competitors. This proportion can then be modeled by using only dummy variables of competitors as predictors. The associated regression coefficients can then be used as a proxy to the ability parameters inferred by the ROL model. We compared the four models in two different simulation studies and we also applied all the models to a real-world example.


FIRST SIMULATION STUDY (focus on the inference performance) 
In order to compare the two models, we simulated finishing positions of competitors in different games according to the ROL model. We varied the considered number of games, the total number of potential competitors, the number of entrants in the game and the competitiveness levels. We generated 1000 datasets for each condition. We then applied to each dataset the ROL model and three different Beta regression models, which differed on the way we defined the precision for each observation. The specification of each of this models can be found in the "Stan models" folder. We estimated the models using Stan probabilistic programming language and use the MAP estimates produced by the optimisation algorithm. We then computed for each model the mean squared errors between the estimated abilities and the real ones for each condition.

The scripts relative to this study can be found int the folder named “First simulation”; use the README_thousand_it file to understand how to use them to reproduce the results.



SECOND SIMULATION STUDY (focus on the ranking performance) 

Similarly to what we did in the first simulation, we generated finishing positions of competitors in different games according to the ROL model. We varied the considered number of games, the total number of potential competitors, the number of entrants in the game and the competitiveness levels. We generated 100 datasets for each condition. We then applied to each dataset the ROL model and three different Beta regression models, which differed on the way we defined the precision for each observation. The specification of each of this models can be found in the "Stan models" folder. We estimated the models using Stan probabilistic programming language and use the MAP estimates produced by the optimisation algorithm. We then computed for each model the normalised Kendall distances between the ranking of competitors derived by the ordering of their true abilities and the ranking of competitors derived by the ordering of estimated abilities, for each condition.

The scripts relative to this study can be found int the folder named “Second simulation”; use the README_hundred_it file to understand how to use them to reproduce the results.



EMPIRICAL EXAMPLE: Analysis of Formula 2 race results

We used Formula 2 race results for seasons 2017-2023 to compare the four models in a real-world example.  We estimated the abilities using sampling algorithms and considered the mean of the posterior distribution and the uncertainty around the estimated was provided by the 95% confidence intervals. We just considered those drivers who finished  at least 30 races. We also compared the rankings of drivers as estimated by the different models.

The scripts relative to this study can be found int the folder named “Formula 2”; use the README_formula2 file to understand how to use them to reproduce the results.

The archive also contains  a folder named “models specification”,  inside of which there is another folder name “stanfiles” where  the Stan specification of the ROL model and the three Beta regression model can be found. (Beta0 = Fixed-Beta regression model, Beta1=Varying-Beta regression model, Beta2 = Dependent-Beta regression model). In “models specification” folder can also be found the script (“ppc.R”)  relative to prior predictive checks for the precision parameters of these models.




