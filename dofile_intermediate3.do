********************************************************************************
************************** STATA INTERMEDIATE COURSE ***************************
********************************************************************************

********************************************************************************
******************************   Marc Guinjoan    ******************************
*********************** Universitat Oberta de Catalunya ************************
****************************** mguinjoan@uoc.edu *******************************
********************************************************************************


**********************************************************************
********* 3rd day: Coefficient plots and interaction effects *********
**********************************************************************

/***OUTLINE
- Append data 
- Coefficient plots (coefplot)
- Interaction models
- Predicted values
- Marginal effects */

***Today we will continue working with the Ukranian sample of the 2020 World Values Survey, but we will compare it with the Russian sample, also from 2020. So, first, we will need to "add" the Russian sample (2017) to the Ukranian one


***1. Append new database
*The append option is useful when two datasets have the same variable names (and categories). It just adds new rows of data. 

use "databases\WVS_Wave_7_Ukraine_Stata_v2.0.dta", clear
append using "databases\WVS_Wave_7_Russian_Federation_Stata_v2.0.dta"

fre B_COUNTRY
label define country 643 "Russia" 804 "Ukraine"
label values B_COUNTRY country

recode B_COUNTRY (643=1 "Russia") (804=0 "Ukraine"), gen(russia)

*Let's fist replicate the same recodifictions that we did in the previous class
recode Q96 Q97 Q98 Q99 Q100 Q101 Q102 Q103 Q104 Q105 (-2/0=0) (1 2=1)
label define participation 0 "Not member" 1 "Member"
label values Q96 Q97 Q98 Q99 Q100 Q101 Q102 Q103 Q104 Q105 participation
gen membership = (Q96 + Q97 + Q98 + Q99 + Q100 + Q101 + Q102 + Q103 + Q104 + Q105)/10
recode Q235 Q235 Q236 Q237 Q239 (-2 -1=.) (1=1) (2=0.66) (3=0.33) (4=0)
recode Q238 (-2 -1=.) (1=0) (2=0.33) (3=0.66) (4=1)
alpha Q235 Q235 Q236 Q237 Q238 Q239, gen(nondemocracy)
egen max_nondemocracy = rowmax(Q235 Q235 Q236 Q237 Q238 Q239)
egen min_nondemocracy = rowmin(Q235 Q235 Q236 Q237 Q238 Q239)
egen sd_nondemocracy = rowsd(Q235 Q235 Q236 Q237 Q238 Q239)
egen median_nondemocracy = rowmedian(Q235 Q235 Q236 Q237 Q238 Q239)
recode Q209 Q210 Q211 Q212 Q213 Q214 Q215 Q216 Q217 Q218 Q219 Q220 (-2/0 2 3=0) (1=1)
alpha Q209 Q210 Q211 Q212 Q213 Q214 Q215 Q216 Q217 Q218 Q219 Q220, gen(participation)
gen log_participation = log(1+participation)
recode Q222 (-2 -1 2 3=0 "Does not always vote") (1=1 "Always votes"), gen(vote)
recode Q240 (-2 -1=.), gen(ideol)  
recode Q249 (-2 -1=3 "Not ideologically placed") (1/4=0 "Left") (5=1 "Centre") (6/10=2 "Right"), gen(ideol3)
rename Q262 age
recode Q260 (2=1 "Female") (1=0 "Men"), gen(female)
recode H_URBRURAL (1=1 "Urban") (2=0 "Rural"), gen(urban)
rename X003R age6 

*here we will create a new variable that identifies the 'national' language--whatever that means
gen nat_language=0
replace nat_language=1 if Q272==3630 & russia==1  // in Russia, russian
replace nat_language=1 if Q272==4410 & russia==0  // in Ukraine, ukanian 
label variable nat_language "National language"
label define nat_language 0 "Another language" 1 "National language"
label values nat_language nat_language


***2. Coefplot 
*Coefplot allows plotting the beta coefficients from a regression model in a horizontal-type of graph. Each coefficient is identified with a dot (the coefficient) and whiskers, corresponing to the 95% confidence interval (95% default, other CI can be chosen).
*We will use the coefplot option with a continuous variable: “an essential characteristic of democracy: Civil rights protect people’s liberty from state oppression"

fre Q246
bysort russia: fre Q246 // it is curious that in Russia the variable ranges from 0 to 10 and in Ukraine from 1 to 10.  We will recode the Ukranian sample accordingly. 
recode Q246 (-2 -1=.) (0=1), gen(democracy)  

*The OLS regression model
reg democracy i.ideol3 age i.female i.urban i.nat_language

coefplot

coefplot, title(Civil rights protection) xline(0) drop(_cons) sch(s1mono) scale(0.9)  // drop constant

coefplot, title(Civil rights protection) xline(0, lc(red)) drop(_cons) sch(s1mono) scale(0.9) baselevel // Includes the reference category

coefplot, title(Civil rights protection) xline(0, lc(red)) drop(_cons) sch(s1mono) scale(0.9) baselevel  coeflabels(0.ideol3 ="Ideology: Left" 0.female= "Gender: Male" 0.urban="Type of settlement: Rural" 0.nat_language = "Language: Another") // Includes a title for each variable

coefplot, title(Civil rights protection) xline(0, lc(red)) drop(_cons) sch(s1mono) scale(0.9) baselevel  headings(0.ideol3 ="{bf:Ideology}" age="{bf:Age}" 0.female= "{bf:Gender}" 0.urban="{bf:Type of settlement}" 0.nat_language = "{bf:Language}") coeflabels(age="In years") // Includes a title for each variable

*and if we want the variable age to range from 0 to 1....
sum age
gen age1 = (age-18)/73
sum age1
gen age_sq = (1+age1)^2

reg democracy i.ideol3 age1 i.female i.urban i.nat_language
coefplot, title(Civil rights protection) xline(0, lc(red)) drop(_cons) sch(s1mono) scale(0.9) baselevel  headings(0.ideol3 ="{bf:Ideology}" age1="{bf:Age}" 0.female= "{bf:Gender}" 0.urban="{bf:Type of settlement}" 0.nat_language = "{bf:Language}") coeflabels(age1="From 0 to 1")  // Includes a title for each variable

coefplot, title(Civil rights protection) xline(0, lc(red)) drop(_cons) sch(s1mono) scale(0.9) baselevel  headings(0.ideol3 ="{bf:Ideology}" age1="{bf:Age}" 0.female= "{bf:Gender}" 0.urban="{bf:Type of settlement}" 0.nat_language = "{bf:Language}") coeflabels(age1="From 0 to 1") level(95 90) fxsize(90) note("Whiskers indicate 90 and 95% CI", pos(7) size(small))  // 90 and 95% CI


***3. Coefplot in two regression models
eststo Ukraine: reg democracy i.ideol3 age1 i.female i.urban i.nat_language if russia==0
eststo Russia: reg democracy i.ideol3 age1 i.female i.urban i.nat_language if russia==1

coefplot Ukraine Russia, title(Civil rights protection) xline(0, lc(red)) drop(_cons) scale(0.9) baselevel  headings(0.ideol3 ="{bf:Ideology}" age1="{bf:Age}" 0.female= "{bf:Gender}" 0.urban="{bf:Type of settlement}" 0.nat_language = "{bf:Language}") coeflabels(age1="From 0 to 1")  graphregion(fcolor(white)) level(95 90) fxsize(90) note("Whiskers indicate 90 and 95% CI", pos(7) size(small)) sch(s1mono)  // By country


***4. Coefplot in two parallel figures
coefplot Ukraine || Russia, xline(0, lc(red)) drop(_cons) scale(0.9) baselevel  headings(0.ideol3 ="{bf:Ideology}" age1="{bf:Age}" 0.female= "{bf:Gender}" 0.urban="{bf:Type of settlement}" 0.nat_language = "{bf:Language}") coeflabels(age1="From 0 to 1")  graphregion(fcolor(white)) level(95 90) fxsize(90) sch(s1mono)  // By country


***5. Interaction models: Sometimes the effect of two different independent variables on a dependent variable is not additive, but rather this is conditional. What does this mean? 
*Let's easy the understanding with an example from the institutions literature. In it, there is an agreement that the main drivers of party system fractionalisation (how many parties obtain representation in a country) is a function of the permissiveness of the electoral system and the ethnic/linguistic heterogeneity. Hence, the higher the number of seats elected in a constituency, the more likely that small parties will obtain representation. At the same time, the higher the issues of content, the more fragmented will be the vote (it is not the same to compete in a single left-right debate than if in this debate we include the centralisation-decentralisation debate). Hence, what the literature has shown is that *only* when there is an enough permissive electoral system *and* some heterogeneous fragmentation, the party system will be fractionalised. Instead, if there is ethnic fractionalisation but a non-permissive electoral system; or a permissive electoral system but no ethnic fractionalisation, the party system will not become fractionalised. In sum, permissiveness and heterogeneity are required at the same time to trigger fractionalisation.

*To put it simply. When do we use interation models? When we assume that the slope of one category will be different from the slope of the other, or when we assume that only through the *concurrence* of two factors a phenomenon will occur. 

*Let's work again only on the Ukranian sample. Now the dependent variable will be ideology (self-placement in the left-righ axis). A constitutive model tells us that, as individuals become older, the are less rightist; the model also tells us that those who have ukranian as their language, are also to the right of those who have Russian as their language: 
reg ideol c.age i.nat_language urban i.female if russia==0
margins, at(age=(18(1)80))
marginsplot, title(Age) sch(s1mono) name(age, replace)

margins, at(nat_language=(0 1))
marginsplot, title(Ukranian language) xlabel(0 "Russian" 1 "Ukranian") sch(s1mono) name(lang, replace)
graph combine age lang, sch(s1mono)

*Now, we can create a new hypothesis. We know that the Soviet Union left a strong legacy in the country among those individuals that experienced it. Probably for this reason we can see that older people are more to the left than younger ones. However, we could hypothesise that this legacy was stronger among those individuals that were linguistically aligned with the majoritarian group in the USSR--i.e., the Russian. 
*Hence, we will next test if the slope for age is different depending on whether we focus on Ukranian or Russian speakers in Russia.

*How do we create interaction models? with the ##. A single # stands for the multiplication of one variable with another (var1*var2). The ## stands for the multiplication of the two variables, while also including the two constitutive terms (variables) in the model: var1 + var2 + var1*var2. This is how this is done (for more information, see Brambor et al 2004: https://www.cambridge.org/core/journals/political-analysis/article/abs/understanding-interaction-models-improving-empirical-analyses/9BA57B3720A303C61EBEC6DDFA40744B)

reg ideol c.age##i.nat_language urban i.female if russia==0

/*How to interpret this?
- the age coefficient (-0.028) stands for the slope of age when nat_language==0
- the nat_language coefficient (-0.361) stands for the slope of nat_language when age==0
- the nat_language#c.age is the interaction coefficient and displays what happens to age when nat_language==1, as compare to the previous age slope
*/

*In a model it will be easier to understand: 
margins, at(age=(25 65) nat_language=(0 1))
marginsplot, title("Ideology in Ukraine, by age and language") sch(s1mono) name(age_lang, replace) legend(order(3 "Russian" 4 "Ukranian"))

*Finally, we can plot what is called the "marginal effects", this is, the marginal change at every category of age between Russian and Ukranian speakers.
*we can expect no marginal change at lower ages and instead a positive change as people become older.
margins, dydx(nat_language) at(age=(18(1)80))
marginsplot, title("Marginal effects of Russian language on ideology, by age") sch(s1mono) yline(0, lc(red))


***6. Quadratic terms
*A derivative of an interaction model is the use of quadratic terms to identify an independent variable that is related with a depenedent variable in a non-linear way. On the second day we explained that we could transform a variable and make it quadratic if we assumed that changes in this variable become more and more substantive at higher values. As I said, however, this is not very frequent. Instead, what is usual is to have an independent variable whose relationship with the dependent variable is not identifical accross its different categories. 
*This isually happens with the age variable. Very often the slope for younger individuals is not the same than for older ones. We modulate this in an empirical model with the inclusion not only of a constitutive term (age), but also its quadratic term (age_sq or age##age). In this case, the age variable will identify the slope for lower values of age, and age##age will do so for higher values. Let's replicate the previous model on democracy by including the quadratic term. 

reg democracy i.ideol3 age c.age#c.age i.female i.urban i.nat_language  // or, which is the same...
reg democracy i.ideol3 c.age##c.age i.female i.urban i.nat_language  // caution: it is important to use the ## command to let STATA know that age and age##age are part of the same variable. Otherwise, if we would type age and age_sq STATA would not consider these two variables part of the same fuction and this would affect the calculation of predicted values. 

*Now we can calculate the predicted varlues
margins, at(age=(18(1)80))
marginsplot, title(Age and support for Civil rights protection) ytitle(Civil right protection, height(5)) sch(s1mono)

*similarly we can replicate the coefplot with the age square term...
reg democracy i.ideol3 c.age1##c.age1 i.female i.urban i.nat_language 
coefplot, title(Civil rights protection) xline(0, lc(red)) drop(_cons) sch(s1mono) scale(0.9) baselevel  headings(0.ideol3 ="{bf:Ideology}" age1="{bf:Age}" 0.female= "{bf:Gender}" 0.urban="{bf:Type of settlement}" 0.nat_language = "{bf:Language}") coeflabels(age1="Age, in years" c.age1#c.age1="Age squared")  // Note that here I have also changed the coeflabels in "age1"

 
************************************ PRACTICE ***********************************

*1. Reload the WVS database
*2. Choose a DV and run a regression model
*3. Plot the results with the coefplot option
*4. Split the sample in two and show the results for each one (either with parallel plots or in the same plot)
*5. Include an interaction in the model
*6. Plot the results. What is best: predicted values or the average marginal effects?
