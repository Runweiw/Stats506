	/*Import csv data*/
libname mylib '~/Assignment4-506/';
PROC IMPORT datafile = 'Medicare_Provider_Utilization_and_Payment_Data__Physician_and_Other_Supplier_PUF_CY2016.csv' out = med;
RUN;
/*Select data with HCPCS_Description contains 'MRI' and HCPCS_Code start with 7*/
DATA med1;
	SET med;
	WHERE HCPCS_Description contains 'MRI' and HCPCS_Code > 69999 and HCPCS_Code < 80000;
RUN;
/*Have a view for med1's first 10 rows*/
PROC PRINT data = med1(obs=10);
RUN;
/*Add one column to med1 called Volume which returns the sum of 'Number_of_Services' by class HCPCS_Code.*/
PROC SUMMARY data = med1;
	CLASS HCPCS_Code;
	VAR Number_of_Services;
	OUTPUT OUT = Service_Volume
		SUM(Number_of_Services) = Volume;
RUN;
PROC PRINT data = Service_Volume;
RUN;
/*Add one column to med1 called Total_Payment which returns 
the sum of 'Average_Medicare_Payment_Amount' by class HCPCS_Code.*/
PROC SUMMARY data = med1;
	CLASS HCPCS_Code;
	VAR Average_Medicare_Payment_Amount;
	WEIGHT Number_of_Services;
	OUTPUT OUT = Total_Payment
		SUM(Average_Medicare_Payment_Amount) = Total_Payment;
RUN;
PROC PRINT data = Total_Payment;
RUN;
/*Add one column to med1 called Average_Payment which returns 
the sum of 'Average_Medicare_Payment_Amount' by class HCPCS_Code.*/

PROC SUMMARY data = med1;
	CLASS HCPCS_Code;
	VAR Average_Medicare_Payment_Amount;
	WEIGHT Number_of_Services;
	OUTPUT OUT = Average_Payment
		MEAN(Average_Medicare_Payment_Amount) = Average_Payment;
RUN;
PROC PRINT data = Average_Payment;
RUN;
/*Merge these three columns as a new table called Total.*/
DATA Total;
	MERGE Service_Volume Total_Payment Average_Payment;
	BY HCPCS_Code;
	DROP _TYPE_ _FREQ_
RUN;
PROC PRINT data = Total;
RUN;
PROC SORT data = Total out = v1; 
	BY DESCENDING Volume; 
RUN;
DATA mv;
	SET v1;
	IF _n_ = 2; 
RUN;
PROC PRINT data = mv;
RUN;
PROC SORT data = Total out = v2; 
	BY DESCENDING Total_Payment; 
RUN;
DATA mtp;
	SET v2;
	IF _n_ = 2; 
RUN;
PROC PRINT data = mtp;
RUN;
PROC SORT data = Total out = v3; 
	BY DESCENDING Average_Payment; 
RUN;
DATA map;
	SET v3;
	IF _n_ = 2; 
RUN;
PROC PRINT data = map;
RUN;

proc append base=mv  data=mtp;
run;
proc append base=mv  data=map;
run;
proc print data=mv;
run;

/*Use SQL to select data that satisfy conditions.*/

PROC SQL;
	CREATE TABLE med2 as
	SELECT *
	FROM med
	WHERE HCPCS_Description contains 'MRI' and HCPCS_Code > 69999 and HCPCS_Code < 80000;
PROC PRINT data = med2(obs=10);
RUN;
/*SQL*/

PROC SQL;
	CREATE TABLE Total1 as
	SELECT HCPCS_Code, 
		   COUNT(HCPCS_Code) as Count, 
		   SUM(Number_of_Services) as Volume,
		   SUM(Average_Medicare_Payment_Amount*Number_of_Services) as Average_Pyment,
		   SUM(Average_Medicare_Payment_Amount*Number_of_Services)/SUM(Number_of_Services) as Total_Payment
	FROM med2
	GROUP BY HCPCS_Code
	ORDER BY HCPCS_Code;
PROC PRINT data = Total1;
RUN;

PROC EXPORT data = mv
  outfile = './ps4_q3c.csv'
  dbms = csv
  replace;
run; 
PROC EXPORT data = Total1
  outfile = './ps4_q3d.csv'
  dbms = csv
  replace;
run; 


