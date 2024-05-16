Repository containing the scripts used for the analysis done in the manuscript "Tractable inference for latent ability in competitive games: 
approximating bayesian rank-ordered models using a simpler proportion model".

The project focuses on the evaluation of the Beta Regression model as an approximation to the Rank-Ordered logit model (ROL) in the inference
of competitors' latent abilities in competitive games. 

The ROL model can be considered the gold standard approach to produce this kind of inference.
Following the approach used in Glickman and Hennessy (2015), under the ROL model, the ranking of competitors given in each game can be direclty modeled 
from a vector of parameters, each of them representing the latent ability of a ranked competitor. Understanding how this model work is not straightforward and
its estimation becomes computationally intensive when the number of ranked competitors is large. 
The use of the Beta Regression model in the estimation of competitors' ability is inspired by what was done in an earlier version of Van Kesteren and Bergkamp (2023).
This approach is based on the transformation of the rank of each competitor in each game into the proporion of outperformed competitors. This proportion can then 
be modeled by using only dummy variables of competitors as predictors. The associated regression coeffiecients can then be used as a proxy to the ability parameters
inferred by the ROL model. 
We compared the four models in two different simulation studies and we also applied all the models to a real-world example. 

FIRST SIMULATION STUDY (focus on the inference perfomance)
In order to compare the two models, we simulated finishing positions of competitors in different games according to the ROL model.
We varied the considered number of games, the total number of potential competitors, the number of entrants in the game and the competitiveness levels. We  generated 1000 
datasets for each condition.  We then applied to each dataset the ROL model and three different Beta regression models, which differed on the way we defined the precision 
for each observation. The specification of each of this models can be found in the "Stan models" folder. We estimated the models using Stan probabilistic programming language
and use the MAP estimates produced by the optimisation algoritihm. We then computed for each model the mean squared errors between the estimated abilities and the real ones for 
each condition. 

How to reproduce results:



SECOND SIMULATION STUDY (focus on the ranking perfomance)
Similarly to what we did in the first simulation, we generated finishing positions of competitors in different games according to the ROL model. 
We varied the considered number of games, the total number of potential competitors, the number of entrants in the game and the competitiveness levels. We  generated 100 
datasets for each condition.  We then applied to each dataset the ROL model and three different Beta regression models, which differed on the way we defined the precision 
for each observation. The specification of each of this models can be found in the "Stan models" folder. We estimated the models using Stan probabilistic programming language
and use the MAP estimates produced by the optimisation algoritihm. We then computed for each model the normalised Kendall distances between the ranking of competitors derived by the ordering
of their true abilities and the ranking of competitors derived by the ordering of estimated abilties, for each condition.ù

How to reproduce results: 


