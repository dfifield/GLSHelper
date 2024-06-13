A. Meaning of Index scores:

Index1: the proportion of the day that the bird spend at temperatures that existed in a given search area.
		ie. if bird spent:
			 20 % of day at 10 deg
			 30 % of day at 12 deg
			 rest of day in air

		then 
			any search box that contains at least one pixel at 10 deg will get a score of 0.20
			any search box that contains at least one pixel at 12 deg will get a score of 0.30
			any search box that contains at least one pixel at 10 and 12 deg will get a score of 0.50
				which is the highest possible index1 score for that day.

Index2: is the score in index1 times the number of pixels available at those temperatures. Ie. it takes into account
	the number of pixel which match, not just the number of bird measured temperatures that match.

Index 3:

B. Examining hdf files:

	> run hdftool




C. Importing positions and SST temps into MATLAB from logger databases:

1. Open CSM 74_3.mdb and import all forms, queries, modules. Should change to a generic location?

2. if Temperature table doesn't exist, then will need to read in raw temperature data from device file.

3. Modify the design of table Temperature to contain an integer SST field and a long int Period field (not used?)

4. Open form "frm Get Surface Temps" in logger dbase

5. Select date range of interest, 

6. Click "Extract logger positions"

7. Choose "File|Export". Export as text file. Use export spec "Position export for SST correction"

8. For temperature export.....Make sure temperature data has been read into the database. See....

9. Set minimun on surface time (usually 20mins), and max delta temp to 
be considered same (usualy 0). Also, choose the coding for numeric codes to be associated with both the surface and
in flight temps. Default 255 for water, 128 for flight. Using these as the color codes for graphing in Matlab will
make a nice graph.

10. Click "Encode Temperatures". This will run the on water temperature extraction algorithm and update the 
"SST" field in the Temperature table. 

11. Click "Extract temps for SST correction". This query is currenlty setup to extract only the on temps
where the bird was on the surface (which is what the MATLAB SST algorithm needs).

12. Export the results from this query as a text file using export spec "my SST temp export"

13. update the input files in the Matlab GUI