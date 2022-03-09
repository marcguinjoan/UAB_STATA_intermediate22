********************************************************************************
************************** STATA INTERMEDIATE COURSE ***************************
********************************************************************************

********************************************************************************
******************************   Marc Guinjoan    ******************************
*********************** Universitat Oberta de Catalunya ************************
****************************** mguinjoan@uoc.edu *******************************
********************************************************************************


********************************************************************************
********* 2nd day: Transformation of variables and logistic regression *********
********************************************************************************

/***OUTLINE
- Indices
- Transformation of variables
- Logistic regression
- Predicted values */

***Today we will work with the Ukranian sample of the 2020 World Values Survey. We wil practice the creation of new complex variables (indexes) and the mathematical transformation of existent variables. Also, we will jump into logistic regression for binary dependent variables and the plotting of results. 

use "databases\WVS_Wave_7_Ukraine_Stata_v2.0.dta", clear

***1. Indices
*In some occasions we want to create a new variable that is an aggregation of different variables. For instance, we may be interested in determining the extent to which someone is member of a political or social group.

*We have the following variables:
describe Q96 Q97 Q98 Q99 Q100 Q101 Q102 Q103 Q104 Q105

*In the survey we identify 10 different ways to be member of a political or social organisation. We now want to create a new variable that is an *aggregation* of the different ways an individual can be member of an organisation. First, we need to recode all variables so that membership==1 and no membership==0.

fre Q96  // 0: do not belong; 1: inactive; 2: active

*Since we are interested just in belonging/not belonging we will code 1 and 2 as 1

recode Q96 Q97 Q98 Q99 Q100 Q101 Q102 Q103 Q104 Q105 (-2/0=0) (1 2=1)
label define membership 0 "Not member" 1 "Member"
label values Q96 Q97 Q98 Q99 Q100 Q101 Q102 Q103 Q104 Q105 membership

fre Q96 // Good! we have now all variables properly recoded. 

*We now just need to create a new variable which is the sum of the different values. There are several ways to do this: 

*1. By creating a new variable and summing the values
gen membership = (Q96 + Q97 + Q98 + Q99 + Q100 + Q101 + Q102 + Q103 + Q104 + Q105)/10  // We divide it by 10 to range from 0 to 1. We could just not do it and it would potentially range from 0 to 10 instead. 

hist membership

*2. Another way to do this is by using the alpha option
alpha Q96 Q97 Q98 Q99 Q100 Q101 Q102 Q103 Q104 Q105, gen(membership2)

*3. And we can also use the egen option
egen membership3 = rowtotal(Q96 Q97 Q98 Q99 Q100 Q101 Q102 Q103 Q104 Q105) // from 0 to 10, let's divide it by 10
replace membership3 = membership3 /10

cor membership membership2 membership3 // the three variables are identical!

drop membership2 membership3

*Why indexes? while it is true that we are loosing some information (in this case, membership in which type of association), we are obtaing other information (how many asociations does the individual belongs to). In fact, we can only now in which association(s) does (or does not) the individual belongs to when he/she does not belong to any association (value 0) or, else, if he/she belongs to all of them (value 1). 
*Again in this case the variable may well range from 0 to 1 or just be the count of associations--in this case it would  range from 0 to 10. This is a personal decision with no further empirical consequences. 


***2. Indexes with reversed variables

*Sometimes we have a series of variables but they are not "oriented" towards the same direction. For instance:
describe Q235 Q236 Q237 Q238 Q239 // All these variables are supporttive of non-democratic forms of government, except for Q238. Let's create a measure of non-democracy. 

*First, we recode all variables except for Q238
fre Q235
recode Q235 Q236 Q237 Q239 (-2 -1=.) (1=1) (2=0.66) (3=0.33) (4=0)

*and now we recode Q238
fre Q238
recode Q238 (-2 -1=.) (1=0) (2=0.33) (3=0.66) (4=1)

*finally we create a new variable:
alpha Q235 Q236 Q237 Q238 Q239, gen(nondemocracy)
hist nondemocracy


***3. More complex variables with the egen option

*In some occasions we just want to take the maximim or the minimum value of a serie of variables:

egen max_nondemocracy = rowmax(Q235 Q236 Q237 Q238 Q239)
fre max_nondemocracy // this is the highest level of non-democratic attitues of each indididual

egen min_nondemocracy = rowmin(Q235 Q236 Q237 Q238 Q239)
fre min_nondemocracy  // and this the minimum

*sometimes we want to take the standard deviation of a series of variables
egen sd_nondemocracy = rowsd(Q235 Q236 Q237 Q238 Q239)

*or the median...
egen median_nondemocracy = rowmedian(Q235 Q236 Q237 Q238 Q239)

*In some occasions we do not want to take a row value (i.e. an individual's), but rather the column one, by a selected variable. For instance, we may want to take the mean (but also any other arithmetic measure) importance for democracy by, say, municipality. For doign so we will use the variable Q250. We have to recode it before.
recode Q250 (-2 -1=0), gen(impdemocracy)
fer impdemocracy

*The municipality variable is N_TOWN
egen mean_impdemocracy = mean(impdemocracy), by(N_TOWN)
browse

list N_TOWN impdemocracy mean_impdemocracy in 1/100

*how do plot this?
hist mean_impdemocracy // this is sensitive to the number of observation (surveys) each municipality has

*how can we solve this. Well, just by sorting and counting the observations BY EACH MUNICIPALITY
bysort N_TOWN: gen count = _n
list N_TOWN impdemocracy mean_impdemocracy count in 1/100

hist mean_impdemocracy if count==1

*what if we want just to include those municiplaities that have more than 5 people interviewed
bysort N_TOWN: gen totalcount=_N
list N_TOWN impdemocracy mean_impdemocracy count totalcount in 1/100

hist mean_impdemocracy if count==1 & totalcount>4

*what if we want to plot the importance for democracy just for one municipality?
*let's identify Kyiv
fre N_TOWN, all   // value 804067
hist impdemocracy if N_TOWN==804067, xtitle(Importance of democracy) title(Importance of democracy in Kyiv)


***4. Transformation of variables
*Sometimes variables are related to another variable in a non-linear way. For instance, we have seen that the membership variable was extremely skewed to the left. Why? well, there is an individal restriction in the number of organisations in which someone can participate. If this is true, changing from 0 to 1 membership would be a *more substantive* change than changing from 9 to 10. In other words, a change from 0 to 1 is bigger than a marginal change from 9 top 10. Indeed, in the first case we are changing from some who does *not* participate at all, to someone who *does* participate. In the later, we are just changing from someone who participates *a lot* to some that participated *even more*.
*Hence, if we assume that changes in this variable are not monotonically increasing, we can transformate the variable so that changes in higher values are given less importane. How? with a "logarithmic" tranformation.

*Let's do it with the variables identifying political/social and online participation (Q209 Q210 Q211 Q212 Q213 Q214 Q215 Q216 Q217 Q218 Q219 Q220), as well as all the previous variables of membership: 

fre Q209  // 1=Have done
recode Q209 Q210 Q211 Q212 Q213 Q214 Q215 Q216 Q217 Q218 Q219 Q220 (-2/0 2 3=0) (1=1)
fre Q209
 
alpha Q209 Q210 Q211 Q212 Q213 Q214 Q215 Q216 Q217 Q218 Q219 Q220   Q96 Q97 Q98 Q99 Q100 Q101 Q102 Q103 Q104 Q105, gen(participation)
hist participation, name(histpart, replace)

gen log_participation = log(participation)  // 662 mising values?
hist log_participation // negative values?

*Log(0)=. & log(0-1)=negative values --> log(1+participation)
drop log_participation

gen log_participation = log(1+participation)
hist log_participation, name(histlog, replace)

*It is very usual to use the logarithmic transformation when speaking about monetary issues--such as income, for instance
fre Q288
recode Q288 (-2 -1=.),gen(income)
label variable income "Income level"
hist income // the idea for using the log transformation is that changing from a high income (e.g. 8) to an additoinal level (9) may not carry the same consequences as in the case of those individials changing from 1 to 2. 

gen log_income = log(income)
hist log_income

***Instead, if we believe that for a variable, changes become more impotant as values increase, then we square the variable. For instance, changes happen in late years of the life of a person (not in the earliest ones). 

gen sq_age = Q262^2
*or
gen sq_age2 = Q262*Q262

drop sq_age2


***4. Logistic regression 
*Along the lines of what we did in the previous day, when our dependent variable is dichotomous it is recommendable to use a logistic regression instead of an OLS regression. 
*The procedure is exactly the same....

*Let's use the variable vote in national elections. Those who always vote vs the others

fre Q222
recode Q222 (-2 -1 2 3=0 "Does not always vote") (1=1 "Always votes"), gen(vote)
fre vote

**let's recode some additional variables

recode Q240 (-2 -1=.), gen(ideol)  // ideology. There are a lot of missing values, we will treat it as a categorical variable:

recode Q249 (-2 -1=3 "Not ideologically placed") (1/4=0 "Left") (5=1 "Centre") (6/10=2 "Right"), gen(ideol3)

rename Q262 age
recode Q260 (2=1 "Female") (1=0 "Men"), gen(female)
recode H_URBRURAL (1=1 "Urban") (2=0 "Rural"), gen(urban)
recode Q272 (-1 9000=.) (3630=0 "Russian") (4410=1 "Ukranian"), gen(ukranian)

**And the model...
logit vote i.ideol3 age i.female i.urban i.ukranian

*To change the reference category we can use the "ib`num category'.varname" option:
logit vote ib1.ideol3 age i.female i.urban i.ukranian // now the reference category are center individuals
logit vote ib2.ideol3 age i.female i.urban i.ukranian // and now right-wing individuals


***5. Predicted values
*The procedure is exactly the same than with the linear regression...
quiet logit vote i.ideol3 age i.female i.urban i.ukranian  // if we do not want the output to be displayed we can use the quiet option

margins, at(ideol3=(0 1 2 3))
marginsplot

marginsplot, xtitle(Ideology) ytitle(Likelihod to 'always' vote, height(6)) title(Ideology and vote in Ukraine) xlabel(0 "Left" 1 "Centre" 2 "Right" 3 `""Not ideologically             ""placed       ""') sch(s1mono) scale(0.9)


************************************ PRACTICE ***********************************

*1. Reload the WVS database
*2. Create a new index variable. Take into accout the "direction" of each answer
*3. Chose another dichotomous variable to run a regression model. Recode the other independent variables
*4. Plot the predicted values for a given variable