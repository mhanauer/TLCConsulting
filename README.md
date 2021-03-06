

Here is the model that I am trying to recreate:

Level 1: There is the intercept that varies for each person over time.  Then there is the slope for time that varies for each person over time.  Finally there is the error term that is unique for each data point.

$$ Level~1:~~~{y_{ij} = \beta_{0j} + \beta_{1j}Time_{ij} + e_{ij}}~~~ (1.1)$$
Level 2 Intercept: Here the intercept is broken down into the constant plus the effect of the intervention1 and intervention 2, which is at level 2 in the intercept because it does not vary over time only by person and the error term which varies by person.  I am only including intervention's one and two because I want to compare intervention versus 2 and 3 and intervention 2 against 1 and 3.    

$$ Level~2~Intercept:~~~{\beta_{0j} = \gamma_{00} + \gamma_{01}Intervention1_{j} +  \gamma_{02}Intervention2_{j} + u_{0j}} ~~~ (1.2)$$
Then there is level the two slope which has the constant effect, plus the slope for the intervention1 and intervention2 for each person, plus a random error term that unique to each person.  

$$ Level~2~Slope~Time:~~~{\beta_{1j} = \gamma_{10} + \gamma_{11}Intervention1_{j} +  \gamma_{12}Intervention2_{j} + u_{1j}} ~~~ (1.3)$$

Then we have the mixed model, which has all the components combined
$$Mixed~model: ~~~{y_{ij} =   (\gamma_{00}+ \gamma_{01}Intervention_{j} + u_{0j}) + (\gamma_{10}}+\gamma_{11}Intervention1_{j} +\gamma_{12}Intervention2_{j} +u_{1j})*Time_{ij} + e_{ij} $$
Here I am creating the data, which has 600 peoeple over two time points (pre and post), over three treatments.  I am only including two treatments, because I need to measure the two (1 versus 2 and 3; 2 versus 1 and 3) to answer my research questions for this design.
```{r}
n = 600
timepoints = 2
time = rep(0:1, times=n)
subject = rep(1:n, each=2)
treat = c(1,2,3)
intervention = sample(treat, replace = TRUE, prob = c(.3, .4, .3), n)
intervention = rep(intervention, each = 2)
intervention1 = ifelse(intervention == 1, 1, 0)
intervention2 = ifelse(intervention ==2, 1, 0)
```
I am assuming I have an outcome that is normally distributed and in standard normal form.     

Then I am setting the intercept to .5, a slope for the variable time to .25, and a slope for the intervention variable to .25.

Then I am creating the random effects for the intercept and time, because each person gets a unique intercept and a unique slope for time.  

I am also creating a slope for the interaction effect between time and intervention1 and 2, which each are also set .25

Now we need to add slopes for each intervention and each intervention effect

Need to generate a random slope and intercept for each person and the correlation between them is .2.  
```{r}
library(MASS)
n = 600
intercept = .5
slopeT1 = .25
slopeT2 = .25
slopeI1 = .25
slopeI2 = .25
slopeT1I = .25
slopeT2I = .25

randomEffectsCorr = matrix(c(1,.2,.2, 1), ncol = 2)
randomEffectsCorr

randomEffects = mvrnonnorm(n, mu = c(0,0), Sigma = randomEffectsCorr, empirical = TRUE)
randomEffects = data.frame(randomEffects)
dim(randomEffects)
colnames(randomEffects) = c("Int", "Slope")
```
Now I am trying to create the outcome variable that has the parameters above.  I am creating random effects for the intercept and slope across time because each person will get their own random effect over this variable, because it is nested within people.  Then I am creating the fixed effects, which are constant across the people according the variable.  For example, the slope for the intervention1 only varies across the whether someone get the intervention or not. 

Ok so when you have [subject] that just means that each subject gets the same value.  Then it makes sense, because you want the random effects for the person to be the same and then vary by time for the slope, because we want this estimate to vary over time instead of just the same data point for each person.
```{r}
sigma = .05
y1 = (intercept + randomEffects$Int[subject])+(slopeT1 + slopeT2 +randomEffects$Slope[subject])*time + slopeI1*intervention1 + slopeI2*intervention2 + slopeT1I*time*intervention1 + slopeT2I*time*intervention2+ rnorm(n*timepoints, mean = 0, sd = sigma)
d = data.frame(subject, time, intervention1, intervention2, y1)
head(d)
```
Generate the data using the model that has the intervention effect that I want with time nested within participants.
```{r}
library(nlme)
model1 = lme(y1 ~ time + intervention2*time + intervention1*time, random =~ time | subject, data = d)
summary(model1)
```
Now I am struggling with finding an ANOVA package that can handle unbalanced groups and repeated measures.  The closest thing I can find is the ezANOVA package, but it is telling me that it cannot handle unbalanced designs and to use exMixed instead.  I am fine with multilevel modeling, but the actual data set that I have only has about 115 people across two-time points so I am a little worried about power particularly since we are comparing three different interventions and just an intervention versus control.
```{r}
library(ez)
ezANOVA(data = d, dv = .(y1), wid = .(subject), within = .(time), between = .(intervention1), type = 3)

```




