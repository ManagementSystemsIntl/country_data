********************************************************************************	
	* 	Gustavo Castillo
	*	This do file: 
		* Cleans data and prepares them for graphs
		* Makes graphs
	*	April 10, 2021						
	* 	Saves: drc_acc_end_student_01_preproc.dta
********************************************************************************

*set working DIRECTORIES 	
global dirfolder "C:\Users\gustavo.castillo\Desktop\Arab Spring STATA\" 

*IMPORT data
clear
use "$dirfolder\wgidataset-fixed.dta", clear

* CREATE variables
encode countryname, generate(countryname1) label(countryname)
order countryname1, after (countryname)

* DROP unnecessary variables
keep if countryname1==3 | countryname1==196 | countryname1==89 | countryname1==96 | countryname1==110 | countryname1==58 | countryname1==131

keep if year>2007

* Tunisia = 196 | Libya = 110 | Egypt = 58

************** VAE Graph *******************

graph twoway (scatter vae year if countryname1==196, connect(direct) color(yellow)) (scatter vae year if countryname1==110, connect(direct) color(green) title("Voice and Accountability") ytitle("Index" "Score", orient(horizontal)) ylabel(,angle(0)) xtitle("") xlabel(2008(2)2020,grid) ylabel(,grid) text(0.4 2019.5 "Tunisia") text(-1.55 2019.5 "Libya") text(-1.30 2019.5 "Egypt")) (scatter vae year if countryname1==58, connect(direct) color(purple) tline(2010, lc(gray)) tlabel(2010 `""Arab" "Spring""', add labsize(*.75)) legend(label(1 "Tunisia") label(2 "Libya") label(3 "Egypt") col(3)))

graph export VoiceandAccountability.png


************** PVE Graph *******************

graph twoway (scatter pve year if countryname1==196, connect(direct) color(yellow)) (scatter pve year if countryname1==110, connect(direct) color(green) title("Political Stability and Absence of Violence/Terrorism") ytitle("Index" "Score", orient(horizontal)) ylabel(,angle(0)) xtitle("") xlabel(2008(2)2020,grid) ylabel(,grid) text(-0.6 2019.5 "Tunisia") text(-2.55 2019.7 "Libya") text(-1.05 2019.7 "Egypt")) (scatter pve year if countryname1==58, connect(direct) color(purple) tline(2010, lc(gray)) tlabel(2010 `""Arab" "Spring""', add labsize(*.75)) legend(label(1 "Tunisia") label(2 "Libya") label(3 "Egypt") col(3)))

graph export PoliticalStabilityandAbsenceofViolenceTerrorism.png

************** GEE Graph *******************

graph twoway (scatter gee year if countryname1==196, connect(direct) color(yellow)) (scatter gee year if countryname1==110, connect(direct) color(green) title("Government Effectiveness") ytitle("Index" "Score", orient(horizontal)) ylabel(,angle(0)) xtitle("") xlabel(2008(2)2020,grid) ylabel(,grid) text(0 2019.5 "Tunisia") text(-1.90 2019.7 "Libya") text(-0.4 2019.7 "Egypt")) (scatter gee year if countryname1==58, connect(direct) color(purple) tline(2010, lc(gray)) tlabel(2010 `""Arab" "Spring""', add labsize(*.75)) legend(label(1 "Tunisia") label(2 "Libya") label(3 "Egypt") col(3)))

graph export GovernmentEffectiveness.png

************** RQE Graph *******************

graph twoway (scatter rqe year if countryname1==196, connect(direct) color(yellow)) (scatter rqe year if countryname1==110, connect(direct) color(green) title("Regulatory Quality") ytitle("Index" "Score", orient(horizontal)) ylabel(,angle(0)) xtitle("") xlabel(2008(2)2020,grid) ylabel(,grid) text(-0.3 2019.5 "Tunisia") text(-2.35 2019.7 "Libya") text(-0.8 2019.7 "Egypt")) (scatter rqe year if countryname1==58, connect(direct) color(purple) tline(2010, lc(gray)) tlabel(2010 `""Arab" "Spring""', add labsize(*.75)) legend(label(1 "Tunisia") label(2 "Libya") label(3 "Egypt") col(3)))

graph export RegulatoryQuality.png


************** RLE Graph *******************

graph twoway (scatter rle year if countryname1==196, connect(direct) color(yellow)) (scatter rle year if countryname1==110, connect(direct) color(green) title("Rule of Law") ytitle("Index" "Score", orient(horizontal)) ylabel(,angle(0)) xtitle("") xlabel(2008(2)2020,grid) ylabel(,grid) text(0.15 2019.5 "Tunisia") text(-1.85 2019.7 "Libya") text(-0.4 2019.7 "Egypt")) (scatter rle year if countryname1==58, connect(direct) color(purple) tline(2010, lc(gray)) tlabel(2010 `""Arab" "Spring""', add labsize(*.75)) legend(label(1 "Tunisia") label(2 "Libya") label(3 "Egypt") col(3)))

graph export RuleofLaw.png


*********************************************************************************************

*Sources to make these graphs:
*https://www.statalist.org/forums/forum/general-stata-discussion/general/179553-a-visualization-question-how-to-add-labels-for-a-tline-in-a-scatter-plot
*https://www.youtube.com/watch?v=XDPKz9JxOKE
*https://www.youtube.com/watch?v=A7JaI-B5t-0
*https://www.statalist.org/forums/forum/general-stata-discussion/general/1604161-data-visualization-how-to-break-a-label-on-tline-into-multiple-lines (my own thread)

* 
*use "http://www.stata-press.com/data/r15/u...​", clear
*describe
*graph twoway (scatter le year)
*graph twoway (scatter le year, connect(direct))
*graph twoway (scatter le year, connect(stairstep))
*graph twoway (scatter le year, connect(direct) msymbol(circle))
*graph twoway (scatter le year, connect(direct) msymbol(triangle))
*graph twoway (scatter le year, connect(direct) msymbol(smtriangle))
*graph twoway (scatter le year, connect(direct) msymbol(smtriangle_hollow))
*graph twoway (scatter le year, connect(direct) msymbol(none))
*graph twoway (scatter le year, connect(direct) msymbol(circle) mcolor(green))
*graph twoway (scatter le year, connect(direct) msymbol(circle) mcolor(green) title("Scatterplot"))
*graph twoway (scatter le year, connect(direct) msymbol(circle) mcolor(green) title("Scatterplot") subtitle("Life expectancy in US"))

*graph twoway (scatter le year, connect(direct) msymbol(circle) mcolor(green) title("Scatterplot") subtitle("Life expectancy in US") xtitle("Year") ytitle("Life Expectancy"))

*graph twoway (scatter le year, connect(direct) msymbol(circle) mcolor(green) title("Scatterplot") subtitle("Life expectancy in US") xtitle("Year") ytitle("Life Expectancy") xlabel(1900(5)1940))

*graph twoway (scatter le year, connect(direct) msymbol(circle) mcolor(green) title("Scatterplot") subtitle("Life expectancy in US") xtitle("Year") ytitle("Life Expectancy") xlabel(1900(5)1940,angle(45)))

*graph twoway (scatter le year, connect(direct) msymbol(circle) mcolor(green) title("Scatterplot") subtitle("Life expectancy in US") xtitle("Year") ytitle("Life Expectancy") xlabel(1900(5)1940,angle(45) grid))

*graph twoway (scatter le year, connect(direct) msymbol(circle) mcolor(green) title("Scatterplot") subtitle("Life expectancy in US") xtitle("Year") ytitle("Life Expectancy") xlabel(1900(5)1940,angle(45) grid) ylabel(,nogrid))

*graph twoway (scatter le year, connect(direct) msymbol(circle) mcolor(green) title("Scatterplot") subtitle("Life expectancy in US") xtitle("Year") ytitle("Life Expectancy") xlabel(1900(5)1940,angle(45) grid) yline(50) xline(1910))

*graph twoway (scatter le year, connect(direct) msymbol(circle) mcolor(green) title("Scatterplot") subtitle("Life expectancy in US") xtitle("Year") ytitle("Life Expectancy") xlabel(1900(5)1940,angle(45) grid) legend(on))

*graph twoway (scatter le year, connect(none) msymbol(circle) mcolor(green) title("Scatterplot") subtitle("Life expectancy in US") xtitle("Year") ytitle("Life Expectancy") xlabel(1900(5)1940,angle(45) grid)) (lfit le year)

*graph twoway (scatter le year, connect(none) msymbol(circle) mcolor(green) title("Scatterplot") subtitle("Life expectancy in US") xtitle("Year") ytitle("Life Expectancy") xlabel(1900(5)1940,angle(45) grid)) (lfit le year), legend(label(1 "Life Expectancy") label(2 "Fitted Line"))

*graph twoway (scatter le year, connect(none) msymbol(circle) mcolor(green) title("Scatterplot") subtitle("Life expectancy in US") xtitle("Year") ytitle("Life Expectancy") xlabel(1900(5)1940,angle(45) grid)) (lfit le year), legend(label(1 "Life Expectancy") label(2 "Fitted Line") ring(0) pos(5) col(1))

*graph twoway (scatter le year, connect(none) msymbol(circle) mcolor(green) msize(small) xtitle("Year") ytitle("Life Expectancy") xlabel(1900(5)1940,angle(45) grid)) (lfit le year, lwidth(medthick)), legend(label(1 "Life Expectancy") label(2 "Fitted Line") ring(0) pos(5) col(1)) title("Scatterplot and OLS fitted line")

*graph export 1.png, replace

*graph export 1.pdf, replace