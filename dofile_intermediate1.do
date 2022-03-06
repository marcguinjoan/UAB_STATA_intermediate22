********************************************************************************
************************** STATA INTERMEDIATE COURSE ***************************
********************************************************************************

********************************************************************************
******************************   Marc Guinjoan    ******************************
*********************** Universitat Oberta de Catalunya ************************
****************************** mguinjoan@uoc.edu *******************************
********************************************************************************


********************************************************************************
************************ 1st day: Inferential statistics ***********************
********************************************************************************

/***OUTLINE
- Linear regression
- Regression tables (estout) and edition
- Graphs and predicted values */

***Today we will work with individual-level data coming from the CEO, the Catalan polling institute. You will find the questionnaire in Catalan, Spanish and (Google translated) in English. We will get into inferential statistics. According to this, we will use data from a sample of Catalan individuals interviewed in June 2021 to make generalizations about a population (Catalans). The class requires some basic knowledge on inferential statistics that we will unfortunatelly not be able to cover it. To begin with, we will perform some basic recodes of the variables that we will use during this fourth class. 

import spss using "http://upceo.ceo.gencat.cat/wsceop/7988/Microdades_anonimitzades_996.sav", clear

recode SEXE (1=0 "Man") (2=1 "Female"), gen(female)
label variable female "Gender"

recode IDEOL_1_7 (98/99=.), gen(ideology)
label variable ideology "Ideology"

rename EDAT age
label variable age "Age"

recode INTERES_POL (4=0 "Not at all") (3=0.333) (2=0.666) (1=1 "A lot"), gen(interest)
label variable interest "Interest in politics"

recode HABITAT (1=1 "Less than 2.000") (2=2 "From 2.001 to 10.000") (3=3 "From 10.001 to 50.000") (4=4 "From 50.001 to 150.000") (5=5 "From 150.001 to 1.000.000") (6=6 "More than 1.000.000"), gen(sizemun)
label variable sizemun "Size municipality"

recode LLOC_NAIX (1=1 "Catalonia") (2=2 "Other regions from Spain") (3 4=3 "Rest of the world"), gen(born)
label variable born "Place born"

recode SATIS_DEMOCRACIA (1=1 "Very satisfied") (2=0.666) (3=0.333) (4=0 "Not at all") (98/99=.), gen(satisfaction)
label variable satisfaction "Satisfaction with democracy"

recode MONARQUIA_REPUBLICA (1=1 "Monarchy") (2=0 "Republic") (else=.), gen(monarchy)
label variable monarchy "Form of government"

recode RELACIONS_CAT_ESP (1 2=1 "Region or autonomous community") (3=2 "Federal state") (4=3 "Independent state") (*=.), gen(territorial)
label variable territorial "Territorial preferences Catalonia vs Spain"

recode ACTITUD_INDEPENDENCIA (1=1 "Yes") (2=0 "No") (*=.), gen(independence)
label variable independence "Support for independence"

recode PART_PARLAMENT (1 2 3 4=0 "Did not vote") (5=1 "Voted") (98/99=.), gen(vote)
label variable vote "Voted in the Catalan Parliament elections"

recode REC_PARLAMENT_VOT (3=1 "ERC") (21=2 "Junts") (4=3 "PSC") (12 13 22=4 "Catalunya en Comú") (10=5 "CUP") (1=6 "PP") (23=7 "Vox") (6=8 "C's") (98/99=.) (else=9 "Other options"), gen(party)
label variable party "Party voted in Parliamentary elections"

********************************************************************************

***1. Linear regression
*We will run a model where the dependent variable is satisfaction with democracy and we will include several independent variables. 

reg satisfaction ideology 
reg satisfaction ideology age // Continuous variables

reg satisfaction ideology age female  // Female is a dichotomous variable and, as such, STATA will treat the value 0 (men) as the reference category and will provide a coefficient for 1 (women)
*What happens when we have a variable that, despite having more than 2 categories, has to be treated as categorical? We use "i." in front of the variable

reg satisfaction ideology age i.female i.born  // It is a good advice to mark always all categorical variables with the "i."

reg satisfaction ideology age i.female i.born i.territorial

*But why do we assume that ideology is a continuos variable and hence its is linearily related to satisfaction with democracy?
*Another option is to divide ideology between leftist, centrist and rightist indivdiudsls:
recode ideology (1/3=1 "Left") (4=2 "Centre") (5/7=3 "Right"), gen(ideol3)  /// And now we can replicate the same model with ideology as a  categorical variable:

reg satisfaction i.ideol3 age i.female i.born i.territorial  // Despite evidence is not statistically significan we can see that the relationship was ideed no linear. 


***2. Regression tables
*We want to make regression tables from stored estimates. 

capture ssc install estout  // again, we use the capture in order to allow STATA to proceed if it find an error (programm installed or no internet)

eststo m1: reg satisfaction ideology 
eststo m2: reg satisfaction ideology age
eststo m3: reg satisfaction ideology age i.female i.born
eststo m4: reg satisfaction ideology age i.female i.born i.territorial

*Tables: 
esttab m1 m2 m3 m4 using "results/table1.html", replace  // first approach

esttab m1 m2 m3 m4 using "results/table1.html", b(3) se(3) r2 replace  // Beta-coefficients and standard errors, 3 decimals, and R2

esttab m1 m2 m3 m4 using "results/table1.html", b(3) se(3) r2 title(Table 1. OLS model on satisfaction with democracy) replace  // Beta-coefficients and standard errors, 3 decimals, and R2

esttab m1 m2 m3 m4 using "results/table1.html", b(3) se(3) r2 title(Table 1. OLS model on satisfaction with democracy) starlevels(+ 0.1 * 0.05 ** 0.01 *** 0.001) replace  // Customise star levels

esttab m1 m2 m3 m4 using "results/table1.html", b(3) se(3) r2 title(Table 1. OLS model on satisfaction with democracy) starlevels(+ 0.1 * 0.05 ** 0.01 *** 0.001) label replace  // Labels instead of variable name

esttab m1 m2 m3 m4 using "results/table1.html", b(3) se(3) r2 title(Table 1. OLS model on satisfaction with democracy) starlevels(+ 0.1 * 0.05 ** 0.01 *** 0.001) label  mtitles ("M1" "M2" "M3" "M4" "M5") nonumbers  replace  // Remove numbers in first raw and edit names

esttab m1 m2 m3 m4 using "results/table1.html", b(3) se(3) r2 title(Table 1. OLS model on satisfaction with democracy) starlevels(+ 0.1 * 0.05 ** 0.01 *** 0.001) label  mtitles ("M1" "M2" "M3" "M4" "M5") nonumbers nogaps replace  // Remove gaps between variables

esttab m1 m2 m3 m4 using "results/table1.html", b(3) se(3) r2 title(Table 1. OLS model on satisfaction with democracy) starlevels(+ 0.1 * 0.05 ** 0.01 *** 0.001) label  mtitles ("M1" "M2" "M3" "M4" "M5") nonumbers nogaps drop(0.female 1.born 1.territorial) refcat(1.female "Gender: Men" 2.born "Place born: Catalonia" 2.territorial "Territorial preference: Region or AC", label("[Ref.]")) replace  // drop coefficients for the reference categories and place [Ref.]

*Results can be stored in a RTF document (to open in Word) and edit it.
esttab m1 m2 m3 m4 using "results/table1.rtf", b(3) se(3) r2 title(Table 1. OLS model on satisfaction with democracy) starlevels(+ 0.1 * 0.05 ** 0.01 *** 0.001) label  mtitles ("M1" "M2" "M3" "M4" "M5") nonumbers nogaps drop(0.female 1.born 1.territorial) refcat(1.female "Gender: Men" 2.born "Place born: Catalonia" 2.territorial "Territorial preference: Region or AC", label("[Ref.]")) replace 

*Caution, once we want to re-run the model we need to clear the stored results: eststo clear
eststo clear

*For more information on estout, check: http://repec.org/bocode/e/estout/advanced.html


***3. Predicted values graph
*We want to plot the results for some variables. For this, we will use the margins and marginsplot commands.

*Let's start with female
reg satisfaction ideology age i.female i.born i.territorial
margins, at(female=(0 1))
marginsplot  // First attempt
marginsplot, title("Satisfaction with democracy, by gender") ytitle("Predicted satisfaction with democracy", height(5))  // Improved

*Now with the territorial prefernce variable
reg satisfaction ideology age i.female i.born i.territorial
margins, at(territorial=(1 2 3))
marginsplot  
marginsplot, title("Satisfaction with democracy, by gender") ytitle("Predicted satisfaction with democracy", height(5)) xlabel(1 `""Region or" "Autonomous Com.""' 2 `""Federal" "State""' 3 `""Independent" "State""') xtitle("Territorial preferences Catalonia vs Spain", height(5)) scale(0.9)  // Not bad but the xlabels cannot be seen. We need to work around this

marginsplot, title("Satisfaction with democracy, by gender") ytitle("Predicted satisfaction with democracy", height(5)) xlabel(1 `""        Region or" "        Autonomous Com.""' 2 `""Federal" "State""' 3 `""Independent        " "State        ""') xtitle("Territorial preferences Catalonia vs Spain", height(5)) scale(0.9) // Now we can read them! :)


************************************ PRACTICE ***********************************

*1. Reload the CEO database
*2. Run a new regression model (if necessary recode new variables)
*3. Run various models and make regression tables. Edit the output. 
*4. Plot the predicted values for one of the independent variables.  
