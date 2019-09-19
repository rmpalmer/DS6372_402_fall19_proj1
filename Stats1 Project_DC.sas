proc import datafile="C:\\Users\\dancr\\OneDrive\\Documents\\Data Science\\SMU\\MSDS 6372 - Applied Stats\\Project 1\\Hitters.csv"
     dbms=dlm out=hitters replace;
     delimeter=',';
     getnames=yes;
run;

/* View Data */
proc print data=hitters;
run;

/* Remove NAs. Create Numeric attribute for Salary */
data hitters;
set hitters;
If Salary = 'NA' Then Delete;
SalaryNumeric = input(Salary, best12.);
run;

/* Scatter Plots */
proc sgscatter data=hitters;
  matrix SalaryNumeric AtBat Hits HmRun Runs RBI Walks /
  diagonal = (histogram);
run;

proc sgscatter data=hitters;
  matrix SalaryNumeric CAtBat CHits CHmRun CRuns CRBI CWalks /
  diagonal = (histogram);
run;

proc sgscatter data=hitters;
  matrix SalaryNumeric PutOuts Assists Errors Years /
  diagonal = (histogram);
run;

/* Check Residuals */
proc reg data=hitters plots(label)=(rstudentleverage cooksd);
model SalaryNumeric = AtBat Hits HmRun Runs RBI Walks / VIF;
run;
quit;

proc reg data=hitters plots(label)=(rstudentleverage cooksd);
model SalaryNumeric = CAtBat CHits CHmRun CRuns CRBI CWalks / VIF;
run;
quit;

/* I purposely left out League Division NewLeague */
proc reg data=hitters plots(label)=(rstudentleverage cooksd);
model SalaryNumeric = PutOuts Assists Errors Years / VIF;
run;
quit;

/* Looks like we could benefit from logging Salary */
data hitters;
set hitters;
SalaryLog = log(SalaryNumeric);
run;

/* Scatter Plots with SalaryLog */
proc sgscatter data=hitters;
  matrix SalaryLog AtBat Hits HmRun Runs RBI Walks /
  diagonal = (histogram);
run;

proc sgscatter data=hitters;
  matrix SalaryLog CAtBat CHits CHmRun CRuns CRBI CWalks /
  diagonal = (histogram);
run;

proc sgscatter data=hitters;
  matrix SalaryLog PutOuts Assists Errors Years /
  diagonal = (histogram);
run;

/* Check Residuals with SalaryLog */
proc reg data=hitters plots(label)=(rstudentleverage cooksd);
model SalaryLog = AtBat Hits HmRun Runs RBI Walks / VIF;
run;
quit;

proc reg data=hitters plots(label)=(rstudentleverage cooksd);
model SalaryLog = CAtBat CHits CHmRun CRuns CRBI CWalks / VIF;
run;
quit;

/* I purposely left out League Division NewLeague */
proc reg data=hitters plots(label)=(rstudentleverage cooksd);
model SalaryLog = PutOuts Assists Errors Years / VIF;
run;
quit;

/* For the first 6 features, I don't think we need to do a transform */
/* For the career attributes (Middle 6 attributes) let's log them */
/* For the last 4, let's log them as well and see what our plots look like */

/* Log features */
data hitters;
set hitters;
CRBILog = log(CRBI+1);
CHitsLog = log(CHits+1);
CAtBatLog = log(CAtBat+1);
CHmRunLog = log(CHmRun+1);
CRunsLog = log(CRuns+1);
CWalksLog = log(CWalks+1);
PutOutsLog = log(PutOuts+1);
AssistsLog = log(Assists+1);
ErrorsLog = log(Errors+1);
YearsLog = log(Years+1);
run;

proc sgscatter data=hitters;
  matrix SalaryLog CAtBatLog CHitsLog CHmRunLog CRunsLog CRBILog CWalksLog /
  diagonal = (histogram);
run;

proc sgscatter data=hitters;
  matrix SalaryLog PutOutsLog AssistsLog ErrorsLog YearsLog /
  diagonal = (histogram);
run;

proc reg data=hitters plots(label)=(rstudentleverage cooksd);
model SalaryLog = CAtBatLog CHitsLog CHmRunLog CRunsLog CRBILog CWalksLog / VIF;
run;
quit;

/* I purposely left out League Division NewLeague */
proc reg data=hitters plots(label)=(rstudentleverage cooksd);
model SalaryLog = PutOutsLog AssistsLog ErrorsLog YearsLog / VIF;
run;
quit;

/* With the scatter/residual plots, the career attributes (those starting with C) look better */
/* Questionable with PutOuts, Assists, Errors and Years, so I will NOT use the log for them */


/* Below use LASSO. Note, the L1 (lamda) value was taken from what I did in R to compute best lambda */
/* Is there a way to find best lamda in SAS!!! */
/* NOTE!! Below stop = 19. The same result is obtained when doing stop = L1 */
proc glmselect data = hitters plots(stepaxis = number) = (criterionpanel ASEPlot) seed = 1;
partition fraction(test = .5);
class Division League NewLeague; 
model SalaryLog = AtBat Hits HmRun Runs RBI Walks
				CAtBatLog CHitsLog CHmRunLog CRunsLog CRBILog CWalksLog
				PutOuts Assists Errors Years Division League NewLeague
				/ selection = lasso(L1=.07 choose=cv stop = 19) CVDETAILS;
run;

