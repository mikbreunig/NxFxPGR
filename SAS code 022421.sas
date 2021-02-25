* edited 8/4/20 ;
*edited 9/10/20 ;
*mmost up to date as fo 1/15/21;

proc import datafile="C:\Users\mbreu\OneDrive - Michigan State University\Fertility 033120\Compiled_Total_CSV.csv"
        out=total_fert
			dbms=csv
			REPLACE;
		getnames=yes;
run;

proc print data=Sub18;
run;

* model with all 4 years of data, so year is a factor;

proc mixed data=total_fert method=type3;
title Overall Anova for Yield; 
class Year Treatment Block;
model Yield=Treatment Year Year*Treatment ;
random Block(Year);
run;
*checked a variety of variables (in outline file), and year was a signifcant factor except in one case. So decided to analyze separately; 

*subsetting data by year;
  
DATA Sub16;
    SET total_fert;
    IF year=2016 THEN OUTPUT;
RUN;
DATA Sub17;
    SET total_fert;
    IF year=2017 THEN OUTPUT;
RUN;
DATA Sub18;
    SET total_fert;
    IF year=2018 THEN OUTPUT;
RUN;
DATA Sub15;
    SET total_fert;
    IF year=2015 THEN OUTPUT;
RUN;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              




proc mixed data=Sub18 method=type3;
title ;
class Treatment Block;
model DS = Treatment;
random Block;
lsmeans Treatment;
contrast 'N and F interaction' 
Treatment 0 -1 1 0 0 1 -1 0 0 0 0 0,
Treatment -1 1 0 0 1 -1 0 0 0 0 0 0, 
Treatment 0 0 1 -1 0 0 -1 1 0 0 0 0;
contrast 'Fungicide and PGR interaction'
Treatment  0 0 0 0 0 -1 1 0 0 1 -1 0,
Treatment  0 0 0 0 0 0 -1 1 0 0 1 -1,
Treatment  0 0 0 0 1 0 0 -1 -1 0 0 1; 
estimate 'PGR v No' Treatment 0 0 0 0 -0.25 -0.25 -0.25 -0.25 0.25 0.25 0.25 0.25/ Cl;
estimate 'only PGR' intercept 1 Treatment 0 0 0 0 0.25 0.25 0.25 0.25/ Cl;
estimate 'high' intercept 1 Treatment 0 0 0 0 0.25 0.25 0.25 0.25 0 0 0 0 / Cl;
estimate 'low' intercept 1 Treatment 0.25 0.25 0.25 0.25 0 0 0 0 0 0 0 0 /Cl;
estimate 'high w pgr' intercept 1 Treatment 0 0 0 0 0.125 0.125 0.125 0.125 0.125 0.125 0.125 0.125/ Cl;
estimate 'high only pgr' intercept 1 Treatment 0 0 0 0 0 0 0 0 0.25 0.25 0.25 0.25/ Cl;
estimate 'H v L Nitrogen'  Treatment  -0.25 -0.25 -0.25 -0.25 0.25 0.25 0.25 0.25 0 0 0 0/ Cl;
estimate 'H v L N wpgr' Treatment -0.25 -0.25 -0.25 -0.25 .125 0.125 0.125 0.125 0.125 0.125 0.125 0.125 / Cl;
estimate 'untreated' intercept 1 Treatment 0.33333 0 0 0 0.33333 0 0 0 0.33333 0 0 0/ Cl;
estimate 'fungicide treated' intercept 1 Treatment 0 0.11111 0.11111 0.11111 0 0.111111 0.11111 0.11111 0 0.11111 0.11111 0.11111/ Cl ;
estimate 'UTC v Fungicide' Treatment 0.33333 -0.11111 -0.11111 -0.11111 0.333333 -0.111111 -0.11111 -0.11111 0.33333 -0.11111 -0.11111 -0.11111/ Cl ;
estimate 'T1 alone' intercept 1 Treatment 0 0.33333 0 0 0 0.33333 0 0 0 0.333333 0 0/ Cl;
estimate 'UTC v T1 only' Treatment -.33333 0.33333 0 0 -.333333 0.33333 0 0 -0.333333 0.333333 0 0/ Cl;
estimate 'T3 only ' intercept 1 Treatment 0 0 0.33333 0 0 0 0.33333 0 0 0 0.333333 0/ Cl;
estimate 'T3 any' intercept 1 Treatment 0 0 0.166666 0.166666 0 0 0.166666 0.166666 0 0 0.166666 0.166666/ Cl;
estimate 'UTC v only T3' Treatment -0.33333 0 0.33333 0 -0.33333 0 0.33333 0 -0.333333 0 0.33333 0/ Cl;
estimate 'No T3 vs T3' Treatment -0.166666 -0.166666 0.166666 0.166666 -0.166666 -0.166666 0.166666 0.166666 -0.166666 -0.166666 0.166666 0.166666/ Cl;
estimate 'Addition of T1 to T3' Treatment 0 0 -0.33333 0.33333 0 0 -0.33333 0.33333 0 0 -0.33333 0.33333/ Cl;
estimate 'Both apps' intercept 1 Treatment 0 0 0 0.33333 0 0 0 0.33333 0 0 0 0.33333/ Cl;
estimate 'both apps vs any 1 app' Treatment 0 -0.166666 -0.166666 0.33333 0 -0.166666 -0.166666 0.33333 0 -0.166666 -0.166666 0.33333/ Cl;
estimate 'UTC vs Both apps' Treatment -0.33333 0 0 0.33333 -0.33333 0 0 0.33333 -0.33333 0 0 0.33333/ Cl;
Run;   
                                                                                                                                                                                        


* This is the data work ;

DATA  Sub18;
    SET Sub18;
    IncDiff = DI_Lodged - DI;
	SevDiff = DS_Lodged - DS;
RUN;

Data Sub18;
	SET Sub18;
	AvgComb= (DI_Lodged*(HarvestLodging/100))+(DI*((100-HarvestLodging)/100));
	RUN;

Proc print data=Sub17;
run;
proc ttest data=Sub18 ;
title Paired t-test for lodged and unlodged portions of plot;
PAIRED DS * DS_Lodged;
Run;

proc ttest data=Sub18 ;
title Paired t-test for lodged and unlodged portions of plot;
PAIRED DI * DI_Lodged;
Run;

proc ttest data=Sub18 ;
title Paired t-test for lodged and unlodged portions of plot--Foliar disease ;
PAIRED DI * DI_Lodged;
Run;

proc ttest data=Sub18 ;
title Paired t-test for lodged and unlodged portions of plot--Foliar disease ;
PAIRED JunNon_Flag * JunLodged_Flag;
Run;

Proc corr data=Sub18 Plots=(scatter);
var DON Flag DIX DI DS perc_FDK;
With HarvestLodging lodging_assess;
run;



*2017 mean lodging;
Proc print data=Sub15;
run;
PROC MEANS DATA = Sub17 mean SUM MAXDEC=3 ;
var HarvestLodging;
RUN;



*subsetting, just lodged plots;
*creating additional variable where they are absolute values for severity since hav positive and negative; 
DATA Sub18_earlylodged;
    SET Sub18;
    IF DS_Lodged>0;
	IF NOT (treatment='2');
	IncDiffABS = abs(IncDiff);
	SevDiffABS= abs(SevDiff);
RUN;

Proc print data=Sub18_earlylodged;
run;
proc ttest data=Sub18 ;
title Paired t-test for lodged and unlodged portions of plot;
PAIRED DS * DS_Lodged;
Run;
Proc corr data=Sub18_earlylodged Plots=(scatter);
var DON Flag;
With HarvestLodging lodging_assess;
run;

* testing to see if the difference between lodged and unlodged is affected by treatment;

proc mixed data=Sub18_earlylodged method=type3;
class  Treatment Block;
model SevDiff=Treatment ;
random Block;
lsmeans treatment;
run;




