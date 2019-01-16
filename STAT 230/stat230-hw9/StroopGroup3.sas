* Sam Tenney;
* Homework 9;
* Section 2;

* Read in the data;
data stroop;
	infile datalines dlm=",";
	input run block $ blockRun task $ timerDisplay $ time;
	datalines;
1,Thomas,1,Color,No,63.84
2,Thomas,2,Shape,No,69.069
3,Thomas,3,Shape,Yes,69.869
4,Thomas,4,Color,Yes,59.477
5,Sam,1,Shape,No,54.02
6,Sam,2,Color,Yes,51.949
7,Sam,3,Color,No,48.16
8,Sam,4,Shape,Yes,50.113
9,Wade,1,Shape,No,61
10,Wade,2,Color,No,60.997
11,Wade,3,Shape,Yes,57.159
12,Wade,4,Color,Yes,57.176
13,Cory,1,Color,No,59.205
14,Cory,2,Shape,Yes,55.484
15,Cory,3,Shape,No,55.556
16,Cory,4,Color,Yes,52.77
;
run;

* Look at the data;
proc contents data=stroop;
run;

* Calculate summary statistics and perform ANOVA without block and display graph;
proc glm data=stroop;
	class task timerDisplay task:timerDisplay;
	model time = task|timerDisplay;
	means task*timerDisplay / tukey;
run;

* Calculate ANOVA with block;
proc glm data=stroop;
	class block;
	model time = block;
run;

* Check assumptions;
* Calculate residuals;
proc glm data=stroop;
	class task timerDisplay task:timerDisplay;
	model time = task|timerDisplay;
	output out=resData residual=res;
run;

* Create index plot;
proc gplot data=resData;
	symbol1 color=black interpol=join value=dot;
	plot res*run / vref=0;
run;

* Create normal QQ plot;
proc univariate data=resData;
	var res;
	probplot / normal(mu=est sigma=est color=red);
run;