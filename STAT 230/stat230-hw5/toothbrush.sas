/* 
 * Sam Tenney
 * Homework 5
 * Toothbrush Wars
 * Data from https://blades.byu.edu/stat230data/ toothbrushes.txt
 */
ods rtf file="/home/samuelj10e0/stat230/hw5/toothbrush.rtf";
data toothbrush;
	infile datalines dlm=" " firstobs=2;
	input id treatment $ changeMGI;
	datalines;
id treatment changeMGI
1 OralB 0.441
2 OralB 0.326
3 OralB 0.381
4 OralB 0.299
5 OralB 0.429
6 OralB 0.459
7 OralB 0.53
8 OralB 0.448
9 OralB 0.423
10 OralB 0.361
11 OralB 0.478
12 OralB 0.503
13 OralB 0.511
14 OralB 0.448
15 OralB 0.449
16 OralB 0.495
17 OralB 0.419
18 OralB 0.268
19 OralB 0.387
20 OralB 0.432
21 OralB 0.42
22 OralB 0.328
23 OralB 0.522
24 OralB 0.434
25 OralB 0.492
26 OralB 0.434
27 OralB 0.292
28 OralB 0.399
29 OralB 0.363
30 OralB 0.456
31 OralB 0.457
32 OralB 0.467
33 OralB 0.486
34 OralB 0.525
35 OralB 0.474
36 OralB 0.253
37 OralB 0.422
38 OralB 0.239
39 OralB 0.446
40 OralB 0.553
41 OralB 0.371
42 OralB 0.414
43 OralB 0.436
44 OralB 0.425
45 OralB 0.392
46 OralB 0.4
47 OralB 0.528
48 OralB 0.309
49 OralB 0.397
50 OralB 0.407
51 OralB 0.437
52 OralB 0.234
53 OralB 0.446
54 OralB 0.354
55 OralB 0.398
56 OralB 0.446
57 OralB 0.516
58 OralB 0.3
59 OralB 0.435
60 OralB 0.5
61 OralB 0.44
62 OralB 0.375
63 OralB 0.46
64 OralB 0.403
65 OralB 0.4
66 Sonicare 0.366
67 Sonicare 0.352
68 Sonicare 0.198
69 Sonicare 0.267
70 Sonicare 0.266
71 Sonicare 0.222
72 Sonicare 0.185
73 Sonicare 0.338
74 Sonicare 0.172
75 Sonicare 0.308
76 Sonicare 0.296
77 Sonicare 0.398
78 Sonicare 0.26
79 Sonicare 0.463
80 Sonicare 0.379
81 Sonicare 0.198
82 Sonicare 0.319
83 Sonicare 0.43
84 Sonicare 0.272
85 Sonicare 0.392
86 Sonicare 0.322
87 Sonicare 0.299
88 Sonicare 0.373
89 Sonicare 0.335
90 Sonicare 0.3
91 Sonicare 0.222
92 Sonicare 0.393
93 Sonicare 0.305
94 Sonicare 0.346
95 Sonicare 0.247
96 Sonicare 0.326
97 Sonicare 0.323
98 Sonicare 0.434
99 Sonicare 0.271
100 Sonicare 0.424
101 Sonicare 0.285
102 Sonicare 0.417
103 Sonicare 0.386
104 Sonicare 0.286
105 Sonicare 0.374
106 Sonicare 0.26
107 Sonicare 0.317
108 Sonicare 0.403
109 Sonicare 0.333
110 Sonicare 0.379
111 Sonicare 0.287
112 Sonicare 0.287
113 Sonicare 0.325
114 Sonicare 0.34
115 Sonicare 0.22
116 Sonicare 0.34
117 Sonicare 0.151
118 Sonicare 0.365
119 Sonicare 0.285
120 Sonicare 0.273
121 Sonicare 0.369
122 Sonicare 0.471
123 Sonicare 0.242
124 Sonicare 0.292
125 Sonicare 0.428
126 Sonicare 0.394
127 Sonicare 0.19
128 Sonicare 0.263
129 Sonicare 0.277
130 Sonicare 0.303
;
run;

* Part 5e - Calculate the mean and standard deviation of change in MGI;
proc means data=toothbrush mean std noprint;
	by treatment;
	var changeMGI;
	output out=brush_summary (drop=_FREQ_ _TYPE_)
		mean=MGImean std=MGIstd;
run;

title1 'Mean and Standard Deviation Change in MGI by Toothbrush';
proc print data=brush_summary noobs;
run;

* Part 5f - 95% Confidnce Interval;
title1 '95% Confidence Interval for Mean Change in MGI';
proc ttest data=toothbrush;
	class treatment;
	var changeMGI;
run;

title1 'Box Plots for Oral-B vs. Sonicare';
proc boxplot data=toothbrush;
	plot changeMGI*treatment  / boxstyle=schematic;
run;

title1 'ANOVA Table';
proc glm data=toothbrush plots=diagnostics;
	class treatment;
	model changeMGI = treatment;
	output out=toothbrushRes residual=res;
run;

data toothbrushRes;
	set toothbrushRes;
	id=_N_;
run;

title1 'Index Plot of Residuals vs. Order';
proc gplot data=toothbrushRes;
	symbol1 color=black interpol=join value=dot;
	plot res*id / vref = 0;
run;

title1 'QQ Plot of Residuals';
proc univariate data=toothbrushRes;
	var res;
	probplot / normal(mu=est sigma=est color=red);
run;

title;
ods rtf close;
