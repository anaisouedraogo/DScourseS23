--create a table that would hold data from csv file

 CREATE TABLE insurance(
    policyID integer,
   state code integer,
   county char,
    eq_site_limit integer,
    hu_site_limit integer,
   fl_site_limit integer,
    fr_site_limit integer,
   tiv_2011 integer,
    tiv_2012 integer,
   eq_site_deductible integer,
   hu_site_deductible integer,
    fl_site_deduuctible integer,
   fr_site_deductible integer,
    point_latitude integer,
  point_longitude integer,
    line char,
    construction char,
   point_granularity integer
   );

--import data from csv file
.import FL_insurance_sample.csv insurance

--Print out first 10 rows of the data
sqlite> SELECT * FROM insurance LIMIT 10;

--list of unique county names
SELECT DISTINCT county FROM insurance;

--compute average property appreciation from 2011 to 2012
SELECT AVG(tiv_2012-tiv_2011) FROM insurance;

--create frequency table of the construction variable
SELECT construction, COUNT(*) FROM insurance GROUP BY construction;

