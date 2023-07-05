// 1
// 2020 T4, Comunidad Valenciana y Murcia
clear all

global datos "Z:\Documents\Mercado de trabajo\Stata\Datos"
global graficos "Z:\Documents\Mercado de trabajo\Stata\Video 1\Gráficos"
cd "$datos\"



// PASOS PREVIOS
* Definimos una función para llamar a nuestra submuestra cuando sea necesario
program define submuestra, rclass

	* Quitamos todo lo que no sea la submuestra
	keep if ccaa == "10" | ccaa == "14"
	
end

* Definimos una función para la transformación de las variables y creación de las necesarias
program define clean_data, rclass

	* Ponemos en minúscula todas las variables
	foreach var of varlist *{
		rename `var' `=lower("`var'")'
	}

	* Trasformar a numerica
	destring ciclo, generate(trim)
	destring aoi, replace
	destring edad5, generate(edad1)
	destring sexo1, generate(genero)
	encode nforma, generate(nivel_educativo)
	
	* Corregir definición de la variable
	replace genero=0 if genero==6
	
	* Población en edad de trabajar
	gen byte edad_trab = .
	replace edad_trab = 1 if aoi != .
	replace edad_trab = 0 if aoi == .
	
	* Población activa
	gen byte activo = .
	replace activo = 1 if aoi <= 6 & aoi < .
	replace activo = 0 if aoi > 6 & aoi < .
	
	* Población inactiva
	gen byte inactivo = .
	replace inactivo = 1 if aoi > 6 & aoi < .
	replace inactivo = 0 if aoi <= 6 & aoi < .

	* Creación de la variable parado
	gen byte parado = .
	replace parado = 1 if (aoi == 5 | aoi == 6) & aoi < .
	replace parado = 0 if aoi < 5 & aoi < .

	* Creación de la variable empleado (población potencialmente empleados)
	gen byte empleado = .
	replace empleado = 1 if (aoi == 3 | aoi == 4 ) & aoi < .
	replace empleado = 0 if (parado == 1 | aoi == 7 | aoi == 8 | aoi == 9) & aoi < .

	* Modificaciones previas necesarias para que ciertas observaciones no se consideren en el cálculo de las tasas. Atendiendo a las definiciones de las diferentes tasas, vamos a generar missing values. 
	replace parado = . if activo == 0
	replace empleado = . if edad_trab == 0
	replace activo = . if edad_trab == 0
	replace inactivo = . if edad_trab == 0

end

* Definimos función para establecer labels
program define add_labels, rclass
	
	* Variable género
	label define genero 0 Mujer 1 Hombre 
	label values genero genero
	
	* Variable trimestre
	label define trim 193 2020T4 
	label values trim trim
	
	* Variable nivel_educativo
	label define newnivel_educativo 1 "Analfabetos" 2 "Educación primaria incompleta" 3 "Educación primaria" 4 "Primera etapa de educación secundaria" 5 "Segunda etapa de educación secundaria. Orientación general" 6 "Segunda etapa de educación secundaria. Orientación profesional" 7 "Educación superior"
	label values nivel_educativo newnivel_educativo

end

* Definimos función para convertir a porcentaje
program define to_pct, rclass

	/*
	Esta función nos va venir bien para no escribir repetitivamente
	los porcentajes después de hacer el collapse
	*/

	replace activo=activo*100
	replace activo=round(activo, 0.01)

	replace empleado=empleado*100
	replace empleado=round(empleado, 0.01)

	replace parado=parado*100
	replace parado=round(parado, 0.01)

end

/*
AOI	La variable AOI indica la clasificación de los entrevistados por relación con la actividad económica según criterios OIT
Código 	Descripción
03	Ocupados subempleados por insuficiencia de horas
04	Resto de ocupados
05	Parados que buscan primer empleo
06	Parados que han trabajado antes
07	Inactivos 1 (desanimados)
08	Inactivos 2 (junto con los desanimados forman los activos potenciales)
09	Inactivos 3 (resto de inactivos)
*/

use "EPA_2020T4.dta", clear

clean_data
add_labels

cd "$graficos\"

// APARTADO A
* España
tabstat edad_trab inactivo activo empleado parado [aweight = factorel], by(trim) s(sum) nototal format(%20.0fc) save
return list

putexcel set tablas_video1.xlsx, sheet("Apartado A", replace) replace
putexcel A1 = matrix(r(Stat1)), names
putexcel close

* Submuestra
preserve

submuestra

tabstat edad_trab inactivo activo empleado parado [aweight = factorel], by(trim) s(sum) nototal format(%20.0fc) save
return list

putexcel set tablas_video1.xlsx, modify sheet("Apartado A")
putexcel B3 = matrix(r(Stat1)) A2 = "España" A3 = "Submuestra" B1 = "Pob. edad trabajar" C1 = "Inactivo" D1 = "Activo" E1 = "Empleado" F1 = "Parado"
putexcel B2:F3, nformat("#,##0") 
putexcel close

restore


// APARTADO B
preserve
* España
collapse activo empleado parado [aweight = factorel], by(trim)

tabstat activo empleado parado, by(trim) s(sum) nototal format(%3.2f) save
return list

putexcel set tablas_video1.xlsx, sheet("Apartado B", replace) modify
putexcel A1 = matrix(r(Stat1)), names

restore

* Submuestra
preserve

submuestra

collapse activo empleado parado [aweight = factorel], by(trim)

tabstat activo empleado parado, by(trim) s(sum) nototal format(%3.2f) save
return list

putexcel set tablas_video1.xlsx, modify sheet("Apartado B")
putexcel B3 = matrix(r(Stat1)) A2 = "España" A3 = "Submuestra" B1 = "Tasa actividad" C1 = "Tasa empleo" D1 = "Tasa paro"
putexcel B2:D3, nformat("0.00%") 
putexcel close

restore


// APARTADO C
// Tablas
preserve
* España
collapse activo empleado parado [aweight = factorel], by(genero)

tabstat activo empleado parado, by(genero) s(sum) nototal format(%3.2f) save
return list

putexcel set tablas_video1.xlsx, sheet("Apartado C", replace) modify
putexcel B2 = "`r(name1)'"
putexcel C2 = matrix(r(Stat1))
putexcel B3 = "`r(name2)'"
putexcel C3 = matrix(r(Stat2))

restore

* Submuestra
preserve

submuestra

collapse activo empleado parado [aweight = factorel], by(genero)

tabstat activo empleado parado, by(genero) s(sum) nototal format(%3.2f) save
return list

putexcel set tablas_video1.xlsx, sheet("Apartado C") modify
putexcel B4 = "`r(name1)'"
putexcel C4 = matrix(r(Stat1))
putexcel B5 = "`r(name2)'"
putexcel C5 = matrix(r(Stat2))

putexcel A2 = "España" A3 = "España" A4 = "Submuestra" A5 = "Submuestra" B1 = "Género" C1 = "Tasa actividad" D1 = "Tasa empleo" E1 = "Tasa paro"

putexcel B2 = "`r(name1)'"
putexcel C2:E5, nformat("0.00%") 
putexcel close

restore

// Gráficos
* España
preserve

collapse activo empleado parado [aweight = factorel], by(trim genero)

* Tanto por ciento
to_pct

graph hbar (asis) activo, over(genero, label(nolabel)) legend(off) over(trim) blabel(bar, color(white) size(6) format(%3.2f) orientation(horizontal) position(inside)) ytitle("Tasa de actividad") title("Tasa de actividad") subtitle("(% de la población en edad de trabajar)") asyvars name(graph1, replace)

graph hbar (asis) empleado, over(genero, label(nolabel)) legend(off) over(trim) blabel(bar, color(white) size(6) format(%3.2f) orientation(horizontal) position(inside)) ytitle("Tasa de empleo") title("Tasa de empleo") subtitle("(% de la población en edad de trabajar)") asyvars name(graph2, replace)

graph hbar (asis) parado, over(genero, label(nolabel)) legend(position(right) rows(1)) over(trim) blabel(bar, color(white) size(5) format(%3.2f) orientation(horizontal) position(inside)) ytitle("Tasa de paro") title("Tasa de paro") subtitle("(% de la población activa)") asyvars name(graph3, replace)

graph combine graph1 graph2, name(row1, replace)
graph combine graph3, name(row2, replace)

graph combine row1 row2, row(2) scale(0.75) title("Gráfico 1. Tasa de actividad, empleo y paro, por hombres y mujeres para España en 2020T4.", pos(6)) note("Fuente: INE (EPA)", pos(7)) xsize(8)
graph export "video1_graph1.png", width(3800) height(1800) replace
graph close


restore

* Submuestra
preserve

submuestra

collapse activo empleado parado [aweight = factorel], by(trim genero)

* Tanto por ciento
to_pct

graph hbar (asis) activo, over(genero, label(nolabel)) legend(off) over(trim) blabel(bar, color(white) size(6) format(%3.2f) orientation(horizontal) position(inside)) ytitle("Tasa de actividad") title("Tasa de actividad") subtitle("(% de la población en edad de trabajar)") asyvars name(graph1, replace)

graph hbar (asis) empleado, over(genero, label(nolabel)) legend(off) over(trim) blabel(bar, color(white) size(6) format(%3.2f) orientation(horizontal) position(inside)) ytitle("Tasa de empleo") title("Tasa de empleo") subtitle("(% de la población en edad de trabajar)") asyvars name(graph2, replace)

graph hbar (asis) parado, over(genero, label(nolabel)) legend(position(right) rows(1)) over(trim) blabel(bar, color(white) size(5) format(%3.2f) orientation(horizontal) position(inside)) ytitle("Tasa de paro") title("Tasa de paro") subtitle("(% de la población activa)") asyvars name(graph3, replace)

graph combine graph1 graph2, name(row1, replace)
graph combine graph3, name(row2, replace)

graph combine row1 row2, row(2) scale(0.75) title("Gráfico 2. Tasa de actividad, empleo y paro, por hombres y mujeres para submuestra en 2020T4.", pos(6)) note("Fuente: INE (EPA)", pos(7)) xsize(8)
graph export "video1_graph2.png", width(3800) height(1800) replace
graph close


restore


// APARTADO D
// Tablas
preserve
* España
collapse activo empleado parado [aweight = factorel], by(nivel_educativo)

tabstat activo empleado parado, by(nivel_educativo) s(sum) nototal format(%3.2f) save
return list

putexcel set tablas_video1.xlsx, sheet("Apartado D", replace) modify
putexcel B2 = "`r(name1)'"
putexcel C2 = matrix(r(Stat1))
putexcel B3 = "`r(name2)'"
putexcel C3 = matrix(r(Stat2))
putexcel B4 = "`r(name3)'"
putexcel C4 = matrix(r(Stat3))
putexcel B5 = "`r(name4)'"
putexcel C5 = matrix(r(Stat4))
putexcel B6 = "`r(name5)'"
putexcel C6 = matrix(r(Stat5))
putexcel B7 = "`r(name6)'"
putexcel C7 = matrix(r(Stat6))
putexcel B8 = "`r(name7)'"
putexcel C8 = matrix(r(Stat7))

restore

* Submuestra
preserve

submuestra

collapse activo empleado parado [aweight = factorel], by(nivel_educativo)

tabstat activo empleado parado, by(nivel_educativo) s(sum) nototal format(%3.2f) save
return list

putexcel set tablas_video1.xlsx, sheet("Apartado D") modify
putexcel B9 = "`r(name1)'"
putexcel C9 = matrix(r(Stat1))
putexcel B10 = "`r(name2)'"
putexcel C10 = matrix(r(Stat2))
putexcel B11 = "`r(name3)'"
putexcel C11 = matrix(r(Stat3))
putexcel B12 = "`r(name4)'"
putexcel C12 = matrix(r(Stat4))
putexcel B13 = "`r(name5)'"
putexcel C13 = matrix(r(Stat5))
putexcel B14 = "`r(name6)'"
putexcel C14 = matrix(r(Stat6))
putexcel B15 = "`r(name7)'"
putexcel C15 = matrix(r(Stat7))

putexcel A2:A8 = "España" 
putexcel A9:A15 = "Submuestra"
putexcel B1 = "Nivel educativo" C1 = "Tasa actividad" D1 = "Tasa empleo" E1 = "Tasa paro"

putexcel B2 = "`r(name1)'"
putexcel C2:E15, nformat("0.00%") 
putexcel close

restore

// Gráficos
* España
preserve

collapse activo empleado parado [aweight = factorel], by(trim nivel_educativo)

* Tanto por ciento
to_pct

graph hbar (asis) activo, over(nivel_educativo, label(nolabel)) legend(off) over(trim) blabel(bar, color(white) size(6) format(%3.2f) orientation(horizontal) position(inside)) ytitle("Tasa de actividad") title("Tasa de actividad") subtitle("(% de la población en edad de trabajar)") xsize(10) asyvars name(graph1, replace)

graph hbar (asis) empleado, over(nivel_educativo, label(nolabel)) legend(off) over(trim) blabel(bar, color(white) size(6) format(%3.2f) orientation(horizontal) position(inside)) ytitle("Tasa de empleo") title("Tasa de empleo") subtitle("(% de la población en edad de trabajar)") xsize(10) asyvars name(graph2, replace)

graph hbar (asis) parado, over(nivel_educativo, label(nolabel)) legend(position(right) rows(7) size(small)) over(trim) blabel(bar, color(white) size(5) format(%3.2f) orientation(horizontal) position(inside)) ytitle("Tasa de paro") title("Tasa de paro") subtitle("(% de la población activa)") xsize(10) asyvars name(graph3, replace)

graph combine graph1 graph2, scale(0.75) name(row1, replace)
graph combine graph3, scale(0.75) name(row2, replace)

graph combine row1 row2, row(2) scale(0.75) title("Gráfico 3. Tasa de actividad, empleo y paro, por nivel educativo para España en 2020T4.", pos(6) size(medium)) note("Fuente: INE (EPA)", pos(7))
graph export "video1_graph3.png", width(3800) height(1800) replace
graph close


restore

* Submuestra
preserve

submuestra

collapse activo empleado parado [aweight = factorel], by(trim nivel_educativo)

* Tanto por ciento
to_pct

graph hbar (asis) activo, over(nivel_educativo, label(nolabel)) legend(off) over(trim) blabel(bar, color(white) size(6) format(%3.2f) orientation(horizontal) position(inside)) ytitle("Tasa de actividad") title("Tasa de actividad") subtitle("(% de la población en edad de trabajar)") xsize(10) asyvars name(graph1, replace)

graph hbar (asis) empleado, over(nivel_educativo, label(nolabel)) legend(off) over(trim) blabel(bar, color(white) size(6) format(%3.2f) orientation(horizontal) position(inside)) ytitle("Tasa de empleo") title("Tasa de empleo") subtitle("(% de la población en edad de trabajar)") xsize(10) asyvars name(graph2, replace)

graph hbar (asis) parado, over(nivel_educativo, label(nolabel)) legend(position(right) rows(7) size(small)) over(trim) blabel(bar, color(white) size(5) format(%3.2f) orientation(horizontal) position(inside)) ytitle("Tasa de paro") title("Tasa de paro") subtitle("(% de la población activa)") xsize(10) asyvars name(graph3, replace)

graph combine graph1 graph2, scale(0.75) name(row1, replace)
graph combine graph3, scale(0.75) name(row2, replace)

graph combine row1 row2, row(2) scale(0.75) title("Gráfico 4. Tasa de actividad, empleo y paro, por nivel educativo para submuestra en 2020T4.", pos(6) size(medium)) note("Fuente: INE (EPA)", pos(7))
graph export "video1_graph4.png", width(3800) height(1800) replace
graph close


restore