// 3
clear all

global datos "Z:\Documents\Mercado de trabajo\Stata\Datos"
global graficos "Z:\Documents\Mercado de trabajo\Stata\Video 3\Gráficos"
cd "$datos\"



// PASOS PREVIOS
* Definimos una función para llamar a nuestra submuestra cuando sea necesario
program define clean_data, rclass
	
	* Argumento de entrada
    local submuestra ""
    if "`1'" != "" local submuestra "`1'"

	* Quitamos todo lo que no sea la submuestra
	
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
	
	* Anualizamos tiempo de permanencia en la empresa
	gen tenure = tenuremes / 12
	gen tenurec = tenure^2
	
	* Anualizamos experiencia laboral
	gen explab = explabmes / 12
	gen explabc = explab^2
	
	* Edadc
	gen edadc = edad^2
	
	* Anualizamos salario real
	gen wager = wager_mensual * 12
	drop if wager == . // Eliminamos valores missing
	gen ln_wager = ln(wager) // Generamos la variable logarítmica
	
	
	sort indiv year
	
end

* Definimos función para establecer labels
program define add_labels, rclass

	* Variable indiv
	label variable indiv "Identificador individuo"
	
	* Variable year
	label variable year "Año"

	* Variable tenure
	label variable tenure "Años de permanencia en la empresa"
	label variable tenure "(Años de permanencia en la empresa)^2"
	
	* Variable explab y explabc
	label variable explab "Años de pexperiencia laboral"
	label variable explabc "(Años de pexperiencia laboral)^2"
	
	* Variable wager
	label variable wager "Salario real"
	
	* Variable ln_wager
	label variable ln_wager "ln(Salario real)"
	
	* Variable edad y edadc
	label variable edad "Edad"
	label variable edadc "(Edad)^2"
	
	* Variable mujer
	label variable mujer "Dummy mujer"
	label define mujer 0 "Hombre" 1 "Mujer"
	label values mujer mujer
	
	* Variable inmigra
	label variable inmigra "Inmigrante"
	
	* Variable nhijos y otras
	label variable nhijos "Numero de hijos"
	label variable nhijopeq04 "Numero de hijos menores de 5"
	label variable nhijopeq59 "Numero de hijos 5-9"
	label variable nhijopeq1015 "Numero de hijos 10-15"
	
	* Variable fijo
	label variable fijo "Contrato fijo"
	
	* Variable fijodisc
	label variable fijodisc "Contrato fijo discontinuo"
	
	* Variable privada
	label variable privada "Emplado sector privado"
	
	* Variable provincia
	label variable provc1 "Provincia"
	label define provc1 3 "Alicante" 12 "Castellon" 46 "Valencia" 30 "Murcia"
	label values provc1 provc1
	
	* Variable Comunidad Autónoma
	label define ccaa 10 "Comunidad Valenciana" 14 "Region de Murcia"
	label values ccaa ccaa

	* Variable trimestre
	label define trim 1 "T1" 2 "T2" 3 "T3" 4 "T4"
	label values trim trim
	
	* Variable sector_actividad
	label variable sector_actividad "Sector de actividad"
	label define sector_actividad 1 "Agricultura, ganaderia, silvicultura y pesca" 2 "Industria" 3 "Construccion" 4 "Comercio, reparacion auto. y hosteleria" 5 "Transporte e informacion y comunicaciones" 6 "Actividades profesionales, científicas y adminis." 7 "Admon. Publica" 8 "Otros servicios"
	label values sector_actividad sector_actividad
	
	* Variable tamemp
	label variable tamemp "Tamaño empresa"
	
	* Variable nivel_educativo
	label variable nivel_educ "Nivel educativo"
	label define newnivel_educ 1 "Analfabetos" 2 "Primaria incompleta" 3 "Primaria completa" 4 "Graduado escolar - ESO" 5 "Bachiller" 6 "FP - educacion postsecundaria no universitaria" 7 "Educacion superior"
	label values nivel_educ newnivel_educ
	
end

////////////////////////////////// SUBMUESTRA //////////////////////////////////
use "data-video3-p1.dta", clear
clean_data "Submuestra"
add_labels
cd "$graficos\"

// PARTE 1
/*
Estimación de los rendimientos salariales asociados a la acumulación de capital
humano general (aproximado por la experiencia laboral)
*/
// FASE 1: Estadística y planteamiento del modelo
// 1.
// a y b.
xtset indiv year
xtdescribe


// 2.
xtsum
/*
variabilidad entre unidades (between) y variabilidad dentro de las unidades (within)
*/
// a.

// b.

// c.

// 3.


// FASE 2: Estimación
global exp explab explabc
global X edad edadc nhijopeq04 nhijopeq59 nhijopeq1015 i.fijo i.privada
global Z i.mujer i.inmigra i.nivel_educ
global Y i.sector_actividad
global time i.year
global provincia i.provc1

// 4.
reg ln_wager $exp $X $Z $Y $time $provincia, vce(cluster indiv)
estimate store mco1
*outreg2 using mco1_video3.xls, replace ctitle("MCO") label
*outreg2 using regs_parte1_video3.xls, replace ctitle("MCO") label

* Rendimientos estimados
* Experiencia laboral
qui sum explab, detail
scalar mexplab = r(mean)
di mexplab
scalar p25explab = r(p25)
di p25explab
scalar p50explab = r(p50)
di p50explab
scalar p75explab = r(p75)
di p75explab

nlcom _b[explab] + 2 * _b[explabc] * mexplab

nlcom _b[explab] + 2 * _b[explabc] * p25explab

nlcom _b[explab] + 2 * _b[explabc] * p50explab

nlcom _b[explab] + 2 * _b[explabc] * p75explab

nlcom -_b[explab] / (2 * _b[explabc])


// 5.

// 6.
xtreg ln_wager $exp $X $Z $Y $time $provincia, vce(cluster indiv) fe
/*
Al especificar "vce(cluster indiv)" en el comando "xtreg", se están estimando
los errores estándar ajustados por conglomerados a nivel de individuo para
controlar por la heterocedasticidad y correlación de los datos de panel.
*/
estimates store fe1
*outreg2 using fe1_video3.xls, replace ctitle("FE") label
*outreg2 using regs_parte1_video3.xls, append ctitle("FE") label

* Rendimientos estimados
* Experiencia laboral
nlcom _b[explab] + 2 * _b[explabc] * mexplab

nlcom _b[explab] + 2 * _b[explabc] * p25explab

nlcom _b[explab] + 2 * _b[explabc] * p50explab

nlcom _b[explab] + 2 * _b[explabc] * p75explab

nlcom -_b[explab] / (2 * _b[explabc])


// 7.
estimates table mco1 fe1

// 8.

// 9.
xtreg ln_wager $exp $X $Z $Y $time $provincia, vce(cluster indiv) re
estimates store re1
*outreg2 using re1_video3.xls, replace ctitle("RE") label
*outreg2 using regs_parte1_video3.xls, append ctitle("RE") label

* Rendimientos estimados
* Experiencia laboral
nlcom _b[explab] + 2 * _b[explabc] * mexplab

nlcom _b[explab] + 2 * _b[explabc] * p25explab

nlcom _b[explab] + 2 * _b[explabc] * p50explab

nlcom _b[explab] + 2 * _b[explabc] * p75explab

nlcom -_b[explab] / (2 * _b[explabc])

// 10.
estimates table mco1 fe1 re1

// 11.

// 12.
/*
Para realizar el test de Hausman en Stata es necesario que los errores estándar
no estén ajustados por conglomerados, por lo que quitamos la opción vce.

El test de Hausman se utiliza para comparar dos estimaciones de efectos
aleatorios y fijos y determinar cuál es más apropiada para un modelo de datos
de panel en particular. La hipótesis nula del test es que los estimadores
aleatorios y fijos son consistentes y eficientes, pero que el estimador
fijo es más eficiente que el estimador aleatorio. Por lo tanto, si el test de
Hausman rechaza la hipótesis nula, se debe utilizar el estimador fijo en lugar
del aleatorio.
*/
xtreg ln_wager $exp $X $Z $Y $time $provincia, fe
estimates store fe2

xtreg ln_wager $exp $X $Z $Y $time $provincia, re
estimates store re2

hausman fe2 re2

// 13.

// 14.
gen universitario = (nivel_educ == 7)

global X i.universitario edad edadc nhijopeq04 nhijopeq59 nhijopeq1015 i.fijo i.privada

xtreg ln_wager $exp i.universitario#c.explab $X $Z $Y $time $provincia, vce(cluster indiv) re
estimates store re_uni
*outreg2 using re3_video3.xls, replace ctitle("RE") label

global X edad edadc nhijopeq04 nhijopeq59 nhijopeq1015 i.fijo i.privada i.nivel_educ

// 15.


// PARTE 2
/*
Estimación de los rendimientos salariales asociados a la acumulación de capital
humano específico (aproximado por la tenure)
*/
global ftenure tenure tenurec

// 1.
xtsum tenure tenurec

// 2.
preserve
replace tenure = round(tenure, 1)
tabstat wager, by(tenure)
restore

// 3.
reg ln_wager $exp $ftenure $X $Z $Y $time $provincia, vce(cluster indiv)
estimate store mco3
*outreg2 using regs_parte2_video3.xls, replace ctitle("MCO") label

xtreg ln_wager $exp $ftenure $X $Z $Y $time $provincia, vce(cluster indiv) fe
estimates store fe3
*outreg2 using regs_parte2_video3.xls,append ctitle("FE") label

xtreg ln_wager $exp $ftenure $X $Z $Y $time $provincia, vce(cluster indiv) re
estimates store re3
*outreg2 using regs_parte2_video3.xls, append ctitle("RE") label

estimates table mco3 fe3 re3

// 4.
estimates restore mco3
nlcom _b[tenure] + 2 * _b[tenurec]

estimates restore fe3
nlcom _b[tenure] + 2 * _b[tenurec]

estimates restore re3
nlcom _b[tenure] + 2 * _b[tenurec]

// 5.

// 6.
xtreg ln_wager $exp $ftenure $X $Z $Y $time $provincia, fe
estimates store fe4

xtreg ln_wager $exp $ftenure $X $Z $Y $time $provincia, re
estimates store re4

hausman fe4 re4


// 7.
*estimates restore re1
*outreg2 using regs_eleccion_video3.xls, replace ctitle("RE P1") label

*estimates restore re3
*outreg2 using regs_eleccion_video3.xls, append ctitle("RE P2") label

estimates table re1 re3