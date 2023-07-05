// 2.2
// 2015, Comunidad Valenciana y Murcia
clear all

*ssc install outreg2

global datos "Z:\Documents\Mercado de trabajo\Stata\Datos"
global graficos1 "Z:\Documents\Mercado de trabajo\Stata\Video 2\Submuestra - Parte 2"
global graficos2 "Z:\Documents\Mercado de trabajo\Stata\Video 2\España - Parte 2"
cd "$datos\"


// PASOS PREVIOS
* Definimos una función para llamar a nuestra submuestra cuando sea necesario
program define clean_data, rclass
	
	* Argumento de entrada
    local submuestra ""
    if "`1'" != "" local submuestra "`1'"

	* Quitamos todo lo que no sea la submuestra
	* Año 2015
	keep if year == 2015
	
	if "`submuestra'" == "Submuestra" {
	    * En este caso los datos están por provincia
		* Alicante: 3
		* Castellón: 12
		* Valencia: 46
		* Murcia: 30
		keep if provc1 == 3 | provc1 == 12 | provc1 == 46 | provc1 == 30
	}
	
	
	* Creamos la variable ccaa
	gen byte ccaa = .
	replace ccaa = 10 if provc1 == 3 | provc1 == 12 | provc1 == 46
	replace ccaa = 14 if provc1 == 30
	
	order ccaa
	
end

* Definimos función para establecer labels
program define add_labels, rclass
	
	* Variable horas_trim
	label variable horas_trim "Horas trabajadas en el trimestre"
	
	* Variable wage_trim
	label variable wage_trim "Salario medio mensual durante el trimestre"
	
	* Variable edad
	label variable edad "Edad del individuo"
	
	* Variable mujer
	label variable mujer "Dummy mujer"
	label define mujer 0 "Hombre" 1 "Mujer"
	label values mujer mujer
	
	* Variable nhijos
	label variable nhijos "Numero de hijos"
	
	* Variable provincia
	label define provc1 3 "Alicante" 12 "Castellon" 46 "Valencia" 30 "Murcia"
	label values provc1 provc1
	
	* Variable Comunidad Autónoma
	label define ccaa 10 "Comunidad Valenciana" 14 "Region de Murcia"
	label values ccaa ccaa

	* Variable trimestre
	label define trim 1 "T1" 2 "T2" 3 "T3" 4 "T4"
	label values trim trim
	
		* Variable nivel_educativo
	label variable nivel_educ "Nivel educativo"
	label define newnivel_educ 1 "Analfabetos" 2 "Primaria incompleta" 3 "Primaria completa" 4 "Graduado escolar - ESO" 5 "Bachiller" 6 "FP - educacion postsecundaria no universitaria" 7 "Educacion superior"
	label values nivel_educ newnivel_educ
	
end

* Definimos una función para llamar a nuestra submuestra cuando sea necesario
program define define_scalars, rclass
	
	* Argumentos de entrada
    local name "`1'"
	local sexo "`2'"
	
	* Calculamos
	if "`sexo'" == "" {
		qui sum wage_trim, detail
	}
	if "`sexo'" == "mujeres" {
	    qui sum wage_trim if mujer == 1, detail
	}
	if "`sexo'" == "hombres" {
	    qui sum wage_trim if mujer == 0, detail
	}
	
	* Definimos escalares
	scalar wage_trim_mean_`name' = r(mean)
	scalar wage_trim_p1_`name' = r(p1)
	scalar wage_trim_p10_`name' = r(p10)
	scalar wage_trim_p25_`name' = r(p25)
	scalar wage_trim_p50_`name' = r(p50)
	scalar wage_trim_p75_`name' = r(p75)
	scalar wage_trim_p90_`name' = r(p90)
	scalar wage_trim_p99_`name' = r(p99)
	
end



////////////////////////////////// SUBMUESTRA //////////////////////////////////
use "panel-oferta-trim.dta", clear
cd "$graficos1\"
clean_data "Submuestra"
add_labels




/*
El objetivo principal es conocer si la
oferta de trabajo es poco o muy elástica a los salarios.

La BD ya viene truncada a aquellos que participan en el mercado.
*/



// PRIMERA FASE: Construcción de la base de datos y estadística básica.
* Definimos los scalars necesarios
define_scalars "submuestra"
define_scalars "submuestra_m" "mujeres"
define_scalars "submuestra_h" "hombres"


// APARTADO A
browse
summarize

describe
di `r(k)'
di `r(N)'


// APARTADO B
global x edad c.edad#c.edad nhijos i.mujer i.nivel_educ
/*
Construimos wage_trim^2 porque creemos que la relación con otras variables
puede no ser lineal. Vamos a hacer utilizando el operador de producto cruzado
de Stata.
*/
global wage wage_trim c.wage_trim#c.wage_trim

/*
También vamos a necesitar en algunos casos la variable generada aparte.
*/
gen wage_trimc = wage_trim^2


// APARTADO C
tabstat edad nhijos mujer nivel_educ, stat(n mean sd p10 p25 p50 p75 p90 min max) save

putexcel set tablas_video2_p2.xlsx, sheet("Fase 1 C", replace) replace
putexcel A1 = "Submuestra" B2 = matrix(r(StatTotal)) B1 = "Edad" C1 = "Nº hijos" D1 = "Sexo" E1 = "Nivel educativo" A2 = "Nº obs." A3 = "Media" A4 = "Des. Están." A5 = "Percentil 10" A6 = "Percentil 25" A7 = "Mediana" A8 = "Percentil 75" A9 = "Percentil 90" A10 = "Mínimo" A11 = "Máximo"




// SEGUNDA FASE: Estimar la función de oferta de trabajo e interpretar los resultados.

// APARTADO A
sum wage_trim horas_trim, detail


hist wage_trim, freq ytitle("Frecuencia") xtitle("Salario mensual medio en el trimestre") yla(, format(%7.0fc) ang(h)) name(g1, replace)
hist horas_trim, freq ytitle("") xtitle("Horas trabajadas en el trimestre") yla(, format(%7.0fc) ang(h)) name(g2, replace)

graph combine g1 g2, title("Gráfico 1. Histograma para el salario y horas trabajadas para la submuestra" , pos(6)) note("Fuente: Panel de oferta trimestral", pos(7)) xsize(8)
graph export "video2_p2_graph1.png", width(3800) height(1800) replace
graph close



// APARTADO B
regress horas_trim $wage $x, robust
estimates store mco1
testparm $wage $x
outreg2 using ols_video2_p2.xls, replace ctitle("MCO") label addstat("F-test", `r(F)', "Prob > F", `r(p)')

qui regress horas_trim wage_trim wage_trimc $x, robust
estimates store mco2
/*
Con este ultimo modelo no podemos usar el comando margins
*/


// APARTADO C


// APARTADO D



// APARTADO E
// i.
* Aumento una unidad
estimates restore mco2
nlcom _b[wage_trim]+2*_b[wage_trimc]

matrix unidad_submuestra = r(b)

* Efectos marginales para distintos percentiles y mediana
estimates restore mco1
margins, dydx(wage_trim) at(wage_trim=(`=wage_trim_p10_submuestra' `=wage_trim_p25_submuestra' `=wage_trim_p50_submuestra' `=wage_trim_p75_submuestra' `=wage_trim_p90_submuestra'))

matrix at_submuestra = r(at)[1...,1]
matrix margins_submuestra = r(b)'

* Punto en el que la pendiente cambia de signo
estimates restore mco2
nlcom -_b[wage_trim]/(2*_b[wage_trimc])

matrix pte_submuestra = r(b)

putexcel set tablas_video2_p2.xlsx, sheet("E.i.", replace) modify
putexcel C2 = matrix(unidad_submuestra) B3 = matrix(at_submuestra) C3 = matrix(margins_submuestra) B8 = matrix(pte_submuestra) A1 = "Submuestra" B1 = "Valores" C1 = "Efectos marginales" A2 = "Una unidad" A3 = "Percentil 10" A4 = "Percentil 25" A5 = "Mediana" A6 = "Percentil 75" A7 = "Percentil 90" A8 = "Cambio pendiente"
putexcel close

// ii.
preserve
qui regress horas_trim $wage, robust
predict phoras_trim

collapse phoras_trim, by(wage_trim)

graph twoway (scatter wage_trim phoras_trim), ytitle("Salario mensual medio en el trimestre") xtitle("Horas trabajadas en el trimestre") title("Gráfico 3. Oferta de trabajo estimada para la submuestra") yline(2626.3941) xsize(8)
graph export "video2_p2_graph3.png", width(2800) height(1800) replace
graph close

restore


// iii.
estimates restore mco1

* Elasticidades para la media, p1, p10, p25, p50, p75, p90 y p99 en el salario en media del resto de variables
margins, eyex(wage_trim)  at(wage_trim=(`=wage_trim_mean_submuestra' `=wage_trim_p1_submuestra' `=wage_trim_p10_submuestra' `=wage_trim_p25_submuestra' `=wage_trim_p50_submuestra' `=wage_trim_p75_submuestra' `=wage_trim_p90_submuestra' `=wage_trim_p99_submuestra'))

putexcel set tablas_video2_p2.xlsx, sheet("E.iii.", replace) modify
putexcel B2 = matrix(r(at)[1...,1]) C2 = matrix(r(b)') A1 = "Submuestra" B1 = "Valores" C1 = "Elasticidades" A2 = "Media" A3 = "Percentil 1" A4 = "Percentil 10" A5 = "Percentil 25" A6 = "Mediana" A7 = "Percentil 75" A8 = "Percentil 90" A9 = "Percentil 99"
putexcel close


// APARTADO F
local ln_vars horas_trim wage_trim edad nhijos
foreach var of local ln_vars {
    gen ln_`var' = ln(`var')
	label variable ln_`var' "ln(`: var lab `var'')"
}

regress ln_horas_trim ln_wage_trim ln_edad ln_nhijos i.mujer i.nivel_educ, robust
estimates store loglog
testparm ln_wage_trim ln_edad ln_nhijos i.mujer i.nivel_educ
outreg2 using loglog_video2_p2.xls, replace ctitle("log-log") label addstat("F-test", `r(F)', "Prob > F", `r(p)')

estimates restore mco1
testparm $wage $x
outreg2 using ols_loglog_video2_p2.xls, replace ctitle("MCO") label addstat("F-test", `r(F)', "Prob > F", `r(p)')

estimates restore loglog
testparm ln_wage_trim ln_edad ln_nhijos i.mujer i.nivel_educ
outreg2 using ols_loglog_video2_p2.xls, append ctitle("log-log") label addstat("F-test", `r(F)', "Prob > F", `r(p)')


// APARTADO H
global x edad c.edad#c.edad nhijos i.nivel_educ
* Estadísticas salario mujer
sum wage_trim if mujer == 1, detail
* Estadísticas salario hombre
sum wage_trim if mujer == 0, detail

* Regresión mujer
regress horas_trim $wage $x if mujer == 1, robust
testparm $wage $x
outreg2 using ols_mujer_hombre_video2_p2.xls, replace ctitle("MCO Mujer") label addstat("F-test", `r(F)', "Prob > F", `r(p)')

* Elasticidades mujer
margins, eyex(wage_trim)  at(wage_trim=(`=wage_trim_mean_submuestra_m' `=wage_trim_p1_submuestra_m' `=wage_trim_p10_submuestra_m' `=wage_trim_p25_submuestra_m' `=wage_trim_p50_submuestra_m' `=wage_trim_p75_submuestra_m' `=wage_trim_p90_submuestra_m' `=wage_trim_p99_submuestra_m'))

putexcel set tablas_video2_p2.xlsx, sheet("G Mujeres", replace) modify
putexcel B2 = matrix(r(at)[1...,1]) C2 = matrix(r(b)') A1 = "Submuestra mujeres" B1 = "Valores" C1 = "Elasticidades" A2 = "Media" A3 = "Percentil 1" A4 = "Percentil 10" A5 = "Percentil 25" A6 = "Mediana" A7 = "Percentil 75" A8 = "Percentil 90" A9 = "Percentil 99"
putexcel close

* Regresión hombre
regress horas_trim $wage $x if mujer == 0, robust
testparm $wage $x
outreg2 using ols_mujer_hombre_video2_p2.xls, append ctitle("MCO Hombre") label addstat("F-test", `r(F)', "Prob > F", `r(p)')

* Elasticidades hombre
margins, eyex(wage_trim)  at(wage_trim=(`=wage_trim_mean_submuestra_h' `=wage_trim_p1_submuestra_h' `=wage_trim_p10_submuestra_h' `=wage_trim_p25_submuestra_h' `=wage_trim_p50_submuestra_h' `=wage_trim_p75_submuestra_h' `=wage_trim_p90_submuestra_h' `=wage_trim_p99_submuestra_h'))

putexcel set tablas_video2_p2.xlsx, sheet("G Hombres", replace) modify
putexcel B2 = matrix(r(at)[1...,1]) C2 = matrix(r(b)') A1 = "Submuestra hombres" B1 = "Valores" C1 = "Elasticidades" A2 = "Media" A3 = "Percentil 1" A4 = "Percentil 10" A5 = "Percentil 25" A6 = "Mediana" A7 = "Percentil 75" A8 = "Percentil 90" A9 = "Percentil 99"
putexcel close






//////////////////////////////////// ESPAÑA ////////////////////////////////////
cd "$datos\"
use "panel-oferta-trim.dta", clear
cd "$graficos2\"
clean_data "España"
add_labels



// PRIMERA FASE: Construcción de la base de datos y estadística básica.
define_scalars "esp"
define_scalars "esp_m" "mujeres"
define_scalars "esp_h" "hombres"

// APARTADO A
browse
summarize

describe
di `r(k)'
di `r(N)'


// APARTADO B
global x edad c.edad#c.edad nhijos i.mujer i.nivel_educ
/*
Construimos wage_trim^2 porque creemos que la relación con otras variables
puede no ser lineal. Vamos a hacer utilizando el operador de producto cruzado
de Stata.
*/
global wage wage_trim c.wage_trim#c.wage_trim

/*
También vamos a necesitar en algunos casos la variable generada aparte.
*/
gen wage_trimc = wage_trim^2


// APARTADO C
tabstat edad nhijos mujer nivel_educ, stat(n mean sd p10 p25 p50 p75 p90 min max) save

putexcel set tablas_video2_p2.xlsx, sheet("Fase 1 C", replace) replace
putexcel A1 = "España" B2 = matrix(r(StatTotal)) B1 = "Edad" C1 = "Nº hijos" D1 = "Sexo" E1 = "Nivel educativo" A2 = "Nº obs." A3 = "Media" A4 = "Des. Están." A5 = "Percentil 10" A6 = "Percentil 25" A7 = "Mediana" A8 = "Percentil 75" A9 = "Percentil 90" A10 = "Mínimo" A11 = "Máximo"




// SEGUNDA FASE: Estimar la función de oferta de trabajo e interpretar los resultados.

// APARTADO A
sum wage_trim horas_trim, detail


hist wage_trim, freq ytitle("Frecuencia") xtitle("Salario mensual medio en el trimestre") yla(, format(%7.0fc) ang(h)) name(g1, replace)
hist horas_trim, freq ytitle("") xtitle("Horas trabajadas en el trimestre") yla(, format(%7.0fc) ang(h)) name(g2, replace)

graph combine g1 g2, title("Gráfico 2. Histograma para el salario y horas trabajadas España" , pos(6)) note("Fuente: Panel de oferta trimestral", pos(7)) xsize(8)
graph export "video2_p2_graph2.png", width(3800) height(1800) replace
graph close



// APARTADO B
regress horas_trim $wage $x, robust
estimates store mco1
testparm $wage $x
outreg2 using ols_video2_p2.xls, replace ctitle("MCO") label addstat("F-test", `r(F)', "Prob > F", `r(p)')

qui regress horas_trim wage_trim wage_trimc $x, robust
estimates store mco2
/*
Con este ultimo modelo no podemos usar el comando margins
*/


// APARTADO C


// APARTADO D



// APARTADO E
// i.
* Aumento una unidad
estimates restore mco2
nlcom _b[wage_trim]+2*_b[wage_trimc]

matrix unidad_esp = r(b)

* Efectos marginales para distintos percentiles y mediana
estimates restore mco1
margins, dydx(wage_trim) at(wage_trim=(`=wage_trim_p10_esp' `=wage_trim_p25_esp' `=wage_trim_p50_esp' `=wage_trim_p75_esp' `=wage_trim_p90_esp'))

matrix at_esp = r(at)[1...,1]
matrix margins_esp = r(b)'

* Punto en el que la pendiente cambia de signo
estimates restore mco2
nlcom -_b[wage_trim]/(2*_b[wage_trimc])

matrix pte_esp = r(b)

putexcel set tablas_video2_p2.xlsx, sheet("E.i.", replace) modify
putexcel C2 = matrix(unidad_esp) B3 = matrix(at_esp) C3 = matrix(margins_esp) B8 = matrix(pte_esp) A1 = "España" B1 = "Valores" C1 = "Efectos marginales" A2 = "Una unidad" A3 = "Percentil 10" A4 = "Percentil 25" A5 = "Mediana" A6 = "Percentil 75" A7 = "Percentil 90" A8 = "Cambio pendiente"
putexcel close


// ii.
preserve
qui regress horas_trim $wage, robust
predict phoras_trim

collapse phoras_trim, by(wage_trim)

graph twoway (scatter wage_trim phoras_trim), ytitle("Salario mensual medio en el trimestre") xtitle("Horas trabajadas en el trimestre") title("Gráfico 4. Oferta de trabajo estimada para España") yline(2626.3941) xsize(8)
graph export "video2_p2_graph4.png", width(2800) height(1800) replace
graph close

restore


// iii.
estimates restore mco1

* Elasticidades para la media, p1, p10, p25, p50, p75, p90 y p99 en el salario en media del resto de variables
margins, eyex(wage_trim)  at(wage_trim=(`=wage_trim_mean_esp' `=wage_trim_p1_esp' `=wage_trim_p10_esp' `=wage_trim_p25_esp' `=wage_trim_p50_esp' `=wage_trim_p75_esp' `=wage_trim_p90_esp' `=wage_trim_p99_esp'))

putexcel set tablas_video2_p2.xlsx, sheet("E.iii.", replace) modify
putexcel B2 = matrix(r(at)[1...,1]) C2 = matrix(r(b)') A1 = "España" B1 = "Valores" C1 = "Elasticidades" A2 = "Media" A3 = "Percentil 1" A4 = "Percentil 10" A5 = "Percentil 25" A6 = "Mediana" A7 = "Percentil 75" A8 = "Percentil 90" A9 = "Percentil 99"
putexcel close


// APARTADO F
local ln_vars horas_trim wage_trim edad nhijos
foreach var of local ln_vars {
    gen ln_`var' = ln(`var')
	label variable ln_`var' "ln(`: var lab `var'')"
}

regress ln_horas_trim ln_wage_trim ln_edad ln_nhijos i.mujer i.nivel_educ, robust
estimates store loglog
testparm ln_wage_trim ln_edad ln_nhijos i.mujer i.nivel_educ
outreg2 using loglog_video2_p2.xls, replace ctitle("log-log") label addstat("F-test", `r(F)', "Prob > F", `r(p)')

estimates restore mco1
testparm $wage $x
outreg2 using ols_loglog_video2_p2.xls, replace ctitle("MCO") label addstat("F-test", `r(F)', "Prob > F", `r(p)')

estimates restore loglog
testparm ln_wage_trim ln_edad ln_nhijos i.mujer i.nivel_educ
outreg2 using ols_loglog_video2_p2.xls, append ctitle("log-log") label addstat("F-test", `r(F)', "Prob > F", `r(p)')


// APARTADO H
global x edad c.edad#c.edad nhijos i.nivel_educ
* Estadísticas salario mujer
sum wage_trim if mujer == 1, detail
* Estadísticas salario hombre
sum wage_trim if mujer == 0, detail

* Regresión mujer
regress horas_trim $wage $x if mujer == 1, robust
testparm $wage $x
outreg2 using ols_mujer_hombre_video2_p2.xls, replace ctitle("MCO Mujer") label addstat("F-test", `r(F)', "Prob > F", `r(p)')

* Elasticidades mujer
margins, eyex(wage_trim)  at(wage_trim=(`=wage_trim_mean_esp_m' `=wage_trim_p1_esp_m' `=wage_trim_p10_esp_m' `=wage_trim_p25_esp_m' `=wage_trim_p50_esp_m' `=wage_trim_p75_esp_m' `=wage_trim_p90_esp_m' `=wage_trim_p99_esp_m'))

putexcel set tablas_video2_p2.xlsx, sheet("G Mujeres", replace) modify
putexcel B2 = matrix(r(at)[1...,1]) C2 = matrix(r(b)') A1 = "España mujeres" B1 = "Valores" C1 = "Elasticidades" A2 = "Media" A3 = "Percentil 1" A4 = "Percentil 10" A5 = "Percentil 25" A6 = "Mediana" A7 = "Percentil 75" A8 = "Percentil 90" A9 = "Percentil 99"
putexcel close

* Regresión hombre
regress horas_trim $wage $x if mujer == 0, robust
testparm $wage $x
outreg2 using ols_mujer_hombre_video2_p2.xls, append ctitle("MCO Hombre") label addstat("F-test", `r(F)', "Prob > F", `r(p)')

* Elasticidades hombre
margins, eyex(wage_trim)  at(wage_trim=(`=wage_trim_mean_esp_h' `=wage_trim_p1_esp_h' `=wage_trim_p10_esp_h' `=wage_trim_p25_esp_h' `=wage_trim_p50_esp_h' `=wage_trim_p75_esp_h' `=wage_trim_p90_esp_h' `=wage_trim_p99_esp_h'))

putexcel set tablas_video2_p2.xlsx, sheet("G Hombres", replace) modify
putexcel B2 = matrix(r(at)[1...,1]) C2 = matrix(r(b)') A1 = "España hombres" B1 = "Valores" C1 = "Elasticidades" A2 = "Media" A3 = "Percentil 1" A4 = "Percentil 10" A5 = "Percentil 25" A6 = "Mediana" A7 = "Percentil 75" A8 = "Percentil 90" A9 = "Percentil 99"
putexcel close