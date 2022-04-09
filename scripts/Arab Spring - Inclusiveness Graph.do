********************************************************************************	
	* 	Gustavo Castillo
	*	This do file: 
		* Cleans data and prepares them for graphs
		* Makes graphs
	*	May 3, 2021						
********************************************************************************

*set working DIRECTORIES 	
global dirfolder "C:\Users\gustavo.castillo\Desktop\Arab Spring STATA\Inclusiveness Data" 

*IMPORT data
clear
import delimited "$dirfolder\global_data_for_website_2016-2020.csv", varnames(1) case(lower) 

* RENAME and label variables
rename ïcountry country
label var country .
label var country "Name of Country" 

* REPLACE missing data and DESTRING
replace inclusivenessindex="." if inclusivenessindex== "No data"
destring inclusivenessindex, replace

************** VAE Graph *******************

graph twoway (scatter inclusivenessindex year if country=="Tunisia", connect(direct) color(yellow) title("Inclusiveness Index") ytitle("Index" "Score", orient(horizontal)) ylabel(,angle(0)) xtitle("") xlabel(2016(1)2020,grid) ylabel(,grid) text(31 2019.85 "Tunisia") text(25.5 2019.9 "Egypt") text(1.5 2019.2 "Yemen")) (scatter inclusivenessindex year if country=="Egypt", connect(direct) color(purple) legend(label(1 "Tunisia") label(2 "Egypt") label(3 "Yemen") col(3))) (scatter inclusivenessindex year if country=="Yemen", connect(direct) color(red))

graph export Inclusivenessindex.png



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