// 2.1
// 2020 T4 y 2019 T4, Comunidad Valenciana y Murcia
clear all

global datos "Z:\Documents\Mercado de trabajo\Stata\Datos"
global graficos1 "Z:\Documents\Mercado de trabajo\Stata\Video 2\Submuestra - Parte 1"
global graficos2 "Z:\Documents\Mercado de trabajo\Stata\Video 2\España - Parte 1"
cd "$datos\"

*ssc install outreg2
*ssc install coefplot

// PASOS PREVIOS
* Definimos una función para llamar a nuestra submuestra cuando sea necesario
program define submuestra, rclass

	* Quitamos todo lo que no sea la submuestra
	keep if ccaa == 10 | ccaa == 14
	
end

* Definimos una función para la transformación de las variables y creación de las necesarias
program define clean_data, rclass

	* Trasformar a numerica
	destring ciclo ccaa prov sexo1 eciv1 dcom horash horasp horase, replace
	destring edad5, generate(edad1)
	encode nforma, gen(nivel_educ)
	
	* Eliminamos la población que no está en edad de trabajar
	drop if edad1 < 16
	drop if edad1 >= 65
	drop if edad1 == .

	* Corregimos variable sexo
	replace sexo1 = 0 if sexo1 == 6
	
	* Creamos una variable para saber si es soltero
	gen soltero = .
	replace soltero = 0 if eciv1 != .
	replace soltero = 1 if eciv1 == 1
	
	* Anualizamos tiempo en la empresa
	replace dcom = dcom / 12

end

* Definimos función para establecer labels
program define add_labels, rclass
	
	* Variable género
	label variable sexo1 "Sexo"
	label define sexo1 0 Mujer 1 Hombre 
	label values sexo1 sexo1
	
	* Variable trimestre
	label variable ciclo "Periodo"
	label define ciclo 193 2020T4 189 2019T4
	label values ciclo ciclo
	
	* Variable nivel_educativo
	label variable nivel_educ "Nivel educativo"
	label define newnivel_educ 1 "Analfabetos" 2 "Primaria incompleta" 3 "Primaria completa" 4 "Graduado escolar - ESO" 5 "Bachiller" 6 "FP - educacion postsecundaria no universitaria" 7 "Educacion superior"
	label values nivel_educ newnivel_educ
	
	* Variable Comunidad Autónoma
	label variable ccaa "Comunidad Autonoma"
	label define ccaa 10 "Comunidad Valenciana" 14 "Región de Murcia"
	label values ccaa ccaa
	
	* Variable situ
	label variable dcom "Años en la empresa"

end

*Definimos una función para la transformación de las variables de horas
program define convertir_horas, rclass

	// Argumentos de entrada
    local varhoras "`1'"
	local varname "`2'"

	tostring `varhoras', generate(varhoras_) 

	replace varhoras_ = "0" + varhoras_ if length(varhoras_) == 3

	gen horas_= substr(varhoras_, 1,2)
	gen min= substr(varhoras_,-2,2)

	destring horas_ min, replace

	replace horas_=. if horas_==99
	replace min=. if min==99

	gen `varname'= horas_+ (min/60)

	drop horas_ min varhoras_

end





use "EPA_2020T4.dta", clear

* Ponemos en minúscula todas las variables para que coincidan las BBDD
foreach var of varlist *{
	rename `var' `=lower("`var'")'
}

append using "EPA_2019T4.dta"


clean_data
add_labels




/*
El objetivo de esta práctica es contrastar la teoría del ciclo vital, esto es, testar si existe una
relación no lineal entre las horas trabajadas y la edad del individuo.
*/




///////////////////////////////SUBMUESTRA///////////////////////////////////////
preserve
cd "$graficos1\"

// APARTADO A
submuestra

summarize

describe
di `r(k)'
di `r(N)'


// APARTADO B
/* Para este apartado hemos usado la funcion convertir_horas que hemos defnido previamente */
* horasp: Horas pactadas en contrato o acuerdo de trabajo (hhmm)
convertir_horas "horasp" "horasp_"
* horash: Número de horas semanales que dedica a este trabajo habitualmente (hhmm)
convertir_horas "horash" "horash_"
* horase: Número de horas efectivas que dedicó a este trab. la semana pasada (hhmm)
convertir_horas "horase" "horase_"

global horas  horasp_  horash_  horase_ 

// i.
/*
En ese caso podríamos usar las horas habituales (o las pactadas) para rellenar
los valores que no estén en las efectivas para ese mismo individuo.

En el caso de que no exista tampoco este dato o prefiramos otro, también podría hacerse calculando
la media (u otros tipos de cálculos/regresiones) para ese tipo de individuo.
Segmentaríamos por tipos de individuos según ciertas características
que consideremos (clusterizar) y realizaríamos dicha media (lo mejor sería hacerse
regresiones con distintas relaciones lineales, cudráticas, etc. y ver cuál se
ajusta mejor) y calcularlo para nuestros individuos.
*/


// APARTADO C
* Horas efectivas
tabstat horase_ [aweight=factorel], stats(mean sd p10 p25 p50 p75 p90) save
return list

putexcel set tablas_video2_p1.xlsx, sheet("Apartado C", replace) replace
putexcel A2 = "Horas efectivas"
matrix trpos3 = r(StatTotal)'
putexcel B2 = matrix(trpos3)

putexcel B1 = "Media" C1 = "Desviación estándar" D1 = "Percentil 10" E1 = "Percentil 25" F1 = "Mediana" G1 = "Percentil 75" H1 = "Percentil 90"
putexcel B2:C2, nformat("##0.#0") 

putexcel close


hist horase_, bin(10) ytitle("Frecuencia") fcolor(navy) lcolor(gs2) xtitle("Horas efectivas") title("Gráfico 1. Horas efectivas para la submuestra" , pos(6)) note("Fuente: INE (EPA 2020T4 & 2019T4)", pos(7)) name(horas1, replace)

graph export "video2_p1_graph1.png", width(2800) height(1800) replace
graph close



// APARTADO D
* Modelo 1: Tramos de edad como variable explicativa

regress horase_ i.edad1 [aweight=factorel], robust

estimates store ols1_horase

testparm i.edad1
outreg2 using ols_video2_parte1.xls, replace ctitle("Modelo 1") addstat("F-test", `r(F)', "Prob > F", `r(p)')

coefplot, keep(20.edad1 25.edad1 30.edad1 35.edad1 40.edad1 45.edad1 50.edad1 55.edad1 60.edad1) vertical nolabel rename(20.edad1 = 20 25.edad1=25 30.edad1=30 35.edad1=35 40.edad1=40 45.edad1=45 50.edad1=50 55.edad1=55 60.edad1=60 65.edad1=65 ) xtitle("Edad") ytitle("Coeficientes") recast(line) title("Gráfico 3. Oferta laboral sobre el ciclo de vida - Submuestra Modelo 1" , pos(6)) note("Fuente: INE (EPA 2020T4 & 2019T4)", pos(7)) name(coef1, replace)

graph export "video2_p1_coefplot1.png", width(3800) height(1800) replace
graph close



* Modelo 2: Tramos de edad, niveles educativos y sexo como variables explicativas
/*
Utilizamos la anotación de factores de Stata, que utiliza cada valor como una
variable dummy.
*/

regress horase_ i.edad1 i.nivel_educ i.sexo1 [aweight=factorel], robust

estimates store ols2_horase

testparm i.edad1 i.nivel_educ i.sexo1
outreg2 using ols_video2_parte1.xls, append ctitle("Modelo 2") addstat("F-test", `r(F)', "Prob > F", `r(p)')

coefplot, keep(20.edad1 25.edad1 30.edad1 35.edad1 40.edad1 45.edad1 50.edad1 55.edad1 60.edad1) vertical nolabel rename(20.edad1 = 20 25.edad1=25 30.edad1=30 35.edad1=35 40.edad1=40 45.edad1=45 50.edad1=50 55.edad1=55 60.edad1=60 65.edad1=65 ) xtitle("Edad") ytitle("Coeficientes") recast(line) title("Gráfico 5. Oferta laboral sobre el ciclo de vida - Submuestra Modelo 2" , pos(6)) note("Fuente: INE (EPA 2020T4 & 2019T4)", pos(7)) name(coef2, replace)

graph export "video2_p1_coefplot2.png", width(3800) height(1800) replace
graph close



// APARTADO E

// i.
/*
En general todas la variables son significativas al menos al 95% de confianza.
Exceptuándo las variables de niveles educativos. Aunque éstas no sean
estadísticamente hablando relevantes, no significa que no sean relevantes para
el modelo.

Es posible que estén correlacionadas con otras variables que no se están
incluyendo en el modelo o que su efecto sea indirecto a través de otras variables.
Por lo tanto, es importante considerar la teoría detrás del modelo y el contexto
en el que se está trabajando para interpretar los resultados de la regresión
de manera adecuada.
*/

// ii.

// iii.

// iv.


// APARTADO F

* Modelo 3: Tramos de edad, niveles educativos, sexo y años de experiencia como variables explicativas
regress horase_ i.edad1 i.nivel_educ i.sexo1 dcom [aweight=factorel], robust

estimates store ols3_horase

testparm i.edad1 i.nivel_educ i.sexo1 dcom
outreg2 using ols3_video2_parte1.xls, replace ctitle("Modelo 3") label addstat("F-test", `r(F)', "Prob > F", `r(p)')

coefplot, keep(20.edad1 25.edad1 30.edad1 35.edad1 40.edad1 45.edad1 50.edad1 55.edad1 60.edad1) vertical nolabel rename(20.edad1 = 20 25.edad1=25 30.edad1=30 35.edad1=35 40.edad1=40 45.edad1=45 50.edad1=50 55.edad1=55 60.edad1=60 65.edad1=65 ) xtitle("Edad") ytitle("Coeficientes") recast(line) title("Gráfico 7. Oferta laboral sobre el ciclo de vida - Submuestra Modelo 3" , pos(6)) note("Fuente: INE (EPA 2020T4 & 2019T4)", pos(7)) name(coef2, replace)

graph export "video2_p1_coefplot3.png", width(3800) height(1800) replace
graph close




// APARTADO G
* Correlograma
corr dcom edad1 sexo1 nivel_educ
return list
matrix list r(C)

putexcel set tablas_video2_p1.xlsx, modify sheet("Apartado G", replace)

putexcel A1 = matrix(r(C)), names

putexcel close

* Modelo 1
estimates restore ols1_horase

testparm i.edad1
outreg2 using ols_todos_video2_parte1.xls, replace ctitle("Modelo 1") label addstat("F-test", `r(F)', "Prob > F", `r(p)')

* Modelo 2
estimates restore ols2_horase

testparm i.edad1 i.nivel_educ i.sexo1
outreg2 using ols_todos_video2_parte1.xls, append ctitle("Modelo 2") label addstat("F-test", `r(F)', "Prob > F", `r(p)')

* Modelo 3
estimates restore ols3_horase

testparm i.edad1 i.nivel_educ i.sexo1 dcom
outreg2 using ols_todos_video2_parte1.xls, append ctitle("Modelo 3") label addstat("F-test", `r(F)', "Prob > F", `r(p)')


/*
La correlación de las variables junto con las estimaciones nos sirve para saber
si la variable en cuestión no está relacionada con variables que ya están
en el modelo. Solo tiene una correlación mediana con la edad, pero eso
no significa que no pueda aportar valor a la estimación, además de no correlacionarse
con ninguna otra.
Para saber si existe problema de omisión de variables relevantes es necsario
comparar los modelos con pocas variables independiente y otro con la/las que
queramos ver su existencia.
De inicio vemos que la probabilidad de que todas los coeficientes sean 0 es nula.
A posteriori la significatividad de la variable años en la empresa es del 99%
o 90%, lo cual nos dice algo positivo al respecto.
Finalmente el R2 para los modelos que poseen la variable años de experiencia son mayores,
pero no excesivamente, diciéndonos que esta variable aporta valor al modelo
pero no significa que estemos cometiendo un error de omisión de
variable redundante.
*/


// APARTADO H
* Modelo 3
estimates restore ols3_horase

testparm i.edad1 i.nivel_educ i.sexo1 dcom
outreg2 using ols_loglin_video2_parte1.xls, replace ctitle("Modelo 3") label addstat("F-test", `r(F)', "Prob > F", `r(p)')

* Generamos la variable horas en logaritmos
gen log_horase_ = ln(horase_)


* Modelo 4: log-lin tramos de edad, niveles educativos, sexo y años en la empresa
regress log_horase_ i.edad1 i.nivel_educ i.sexo1 dcom [aweight=factorel] if ciclo == 189, robust

estimates store ols4_horase

testparm i.edad1 i.nivel_educ i.sexo1 dcom
outreg2 using ols_loglin_video2_parte1.xls, append ctitle("Modelo 4") label addstat("F-test", `r(F)', "Prob > F", `r(p)')

coefplot, keep(20.edad1 25.edad1 30.edad1 35.edad1 40.edad1 45.edad1 50.edad1 55.edad1 60.edad1) vertical nolabel rename(20.edad1 = 20 25.edad1=25 30.edad1=30 35.edad1=35 40.edad1=40 45.edad1=45 50.edad1=50 55.edad1=55 60.edad1=60 65.edad1=65 ) xtitle("Edad") ytitle("Coeficientes") recast(line) title("Gráfico 9. Oferta laboral sobre el ciclo de vida - Submuestra Modelo 4" , pos(6)) note("Fuente: INE (EPA 2020T4 & 2019T4)", pos(7)) name(coef2, replace)

graph export "video2_p1_coefplot4.png", width(3800) height(1800) replace
graph close

/*
Podré perder muestras en el caso de que las horas efectivas sean 0 ya que al
hacer el logaritmo natural de 0, Stata me genera un missing value. Igualmente
nosotros no hemos perdido observaciones porque no tenemos horas efectivas
con valor 0, sino que son missing que ya se descartaban previamente.
Por lo tanto puede ser la forma en la que se relacionan el resto de variables
con la variable horas efectivas en logaritmos no sea la más idónea.
*/

restore





///////////////////////////////ESPAÑA///////////////////////////////////////
preserve
cd "$graficos2\"

// APARTADO A
summarize

describe
di `r(k)'
di `r(N)'


// APARTADO B

* horasp: Horas pactadas en contrato o acuerdo de trabajo (hhmm)
convertir_horas "horasp" "horasp_"
* horash: Número de horas semanales que dedica a este trabajo habitualmente (hhmm)
convertir_horas "horash" "horash_"
* horase: Número de horas efectivas que dedicó a este trab. la semana pasada (hhmm)
convertir_horas "horase" "horase_"

global horas  horasp_  horash_  horase_ 

// i.
/*
En ese caso podríamos usar las horas habituales (o las pactadas) para rellenar
los valores que no estén en las efectivas para ese mismo individuo.

En el caso de que no exista tampoco este dato o prefiramos otro, también podría hacerse calculando
la media (u otros tipos de cálculos/regresiones) para ese tipo de individuo.
Segmentaríamos por tipos de individuos según ciertas características
que consideremos (clusterizar) y realizaríamos dicha media (lo mejor sería hacerse
regresiones con distintas relaciones lineales, cudráticas, etc. y ver cuál se
ajusta mejor) y calcularlo para nuestros individuos.
*/


// APARTADO C
* Horas efectivas
tabstat horase_ [aweight=factorel], stats(mean sd p10 p25 p50 p75 p90) save
return list

putexcel set tablas_video2_p1.xlsx, sheet("Apartado C", replace) replace
putexcel A2 = "Horas efectivas"
matrix trpos3 = r(StatTotal)'
putexcel B2 = matrix(trpos3)

putexcel B1 = "Media" C1 = "Desviación estándar" D1 = "Percentil 10" E1 = "Percentil 25" F1 = "Mediana" G1 = "Percentil 75" H1 = "Percentil 90"
putexcel B2:C2, nformat("##0.#0") 

putexcel close


hist horase_, bin(10) ytitle("Frecuencia") fcolor(navy) lcolor(gs2) xtitle("Horas efectivas") title("Gráfico 2. Horas efectivas para España" , pos(6)) note("Fuente: INE (EPA 2020T4 & 2019T4)", pos(7)) name(horas1, replace)

graph export "video2_p1_graph1.png", width(2800) height(1800) replace
graph close



// APARTADO D
* Modelo 1: Tramos de edad como variable explicativa

regress horase_ i.edad1 [aweight=factorel], robust

estimates store ols1_horase

testparm i.edad1
outreg2 using ols_video2_parte1.xls, replace ctitle("Modelo 1") addstat("F-test", `r(F)', "Prob > F", `r(p)')

coefplot, keep(20.edad1 25.edad1 30.edad1 35.edad1 40.edad1 45.edad1 50.edad1 55.edad1 60.edad1) vertical nolabel rename(20.edad1 = 20 25.edad1=25 30.edad1=30 35.edad1=35 40.edad1=40 45.edad1=45 50.edad1=50 55.edad1=55 60.edad1=60 65.edad1=65 ) xtitle("Edad") ytitle("Coeficientes") recast(line) title("Gráfico 4. Oferta laboral sobre el ciclo de vida - España Modelo 1" , pos(6)) note("Fuente: INE (EPA 2020T4 & 2019T4)", pos(7)) name(coef1, replace)

graph export "video2_p1_coefplot1.png", width(3800) height(1800) replace
graph close



* Modelo 2: Tramos de edad, niveles educativos y sexo como variables explicativas
/*
Utilizamos la anotación de factores de Stata, que utiliza cada valor como una
variable dummy.
*/

regress horase_ i.edad1 i.nivel_educ i.sexo1 [aweight=factorel], robust

estimates store ols2_horase

testparm i.edad1 i.nivel_educ i.sexo1
outreg2 using ols_video2_parte1.xls, append ctitle("Modelo 2") addstat("F-test", `r(F)', "Prob > F", `r(p)')

coefplot, keep(20.edad1 25.edad1 30.edad1 35.edad1 40.edad1 45.edad1 50.edad1 55.edad1 60.edad1) vertical nolabel rename(20.edad1 = 20 25.edad1=25 30.edad1=30 35.edad1=35 40.edad1=40 45.edad1=45 50.edad1=50 55.edad1=55 60.edad1=60 65.edad1=65 ) xtitle("Edad") ytitle("Coeficientes") recast(line) title("Gráfico 6. Oferta laboral sobre el ciclo de vida - España Modelo 2" , pos(6)) note("Fuente: INE (EPA 2020T4 & 2019T4)", pos(7)) name(coef2, replace)

graph export "video2_p1_coefplot2.png", width(3800) height(1800) replace
graph close



// APARTADO E

// i.
/*
En general todas la variables son significativas al menos al 95% de confianza.
Exceptuándo las variables de niveles educativos. Aunque éstas no sean
estadísticamente hablando relevantes, no significa que no sean relevantes para
el modelo.

Es posible que estén correlacionadas con otras variables que no se están
incluyendo en el modelo o que su efecto sea indirecto a través de otras variables.
Por lo tanto, es importante considerar la teoría detrás del modelo y el contexto
en el que se está trabajando para interpretar los resultados de la regresión
de manera adecuada.
*/

// ii.

// iii.

// iv.


// APARTADO F

* Modelo 3: Tramos de edad, niveles educativos, sexo y años de experiencia como variables explicativas
regress horase_ i.edad1 i.nivel_educ i.sexo1 dcom [aweight=factorel], robust

estimates store ols3_horase

testparm i.edad1 i.nivel_educ i.sexo1 dcom
outreg2 using ols3_video2_parte1.xls, replace ctitle("Modelo 3") label addstat("F-test", `r(F)', "Prob > F", `r(p)')

coefplot, keep(20.edad1 25.edad1 30.edad1 35.edad1 40.edad1 45.edad1 50.edad1 55.edad1 60.edad1) vertical nolabel rename(20.edad1 = 20 25.edad1=25 30.edad1=30 35.edad1=35 40.edad1=40 45.edad1=45 50.edad1=50 55.edad1=55 60.edad1=60 65.edad1=65 ) xtitle("Edad") ytitle("Coeficientes") recast(line) title("Gráfico 8. Oferta laboral sobre el ciclo de vida - España Modelo 3" , pos(6)) note("Fuente: INE (EPA 2020T4 & 2019T4)", pos(7)) name(coef2, replace)

graph export "video2_p1_coefplot3.png", width(3800) height(1800) replace
graph close




// APARTADO G
* Correlograma
corr dcom edad1 sexo1 nivel_educ
return list
matrix list r(C)

putexcel set tablas_video2_p1.xlsx, modify sheet("Apartado G", replace)

putexcel A1 = matrix(r(C)), names

putexcel close

* Modelo 1
estimates restore ols1_horase

testparm i.edad1
outreg2 using ols_todos_video2_parte1.xls, replace ctitle("Modelo 1") label addstat("F-test", `r(F)', "Prob > F", `r(p)')

* Modelo 2
estimates restore ols2_horase

testparm i.edad1 i.nivel_educ i.sexo1
outreg2 using ols_todos_video2_parte1.xls, append ctitle("Modelo 2") label addstat("F-test", `r(F)', "Prob > F", `r(p)')

* Modelo 3
estimates restore ols3_horase

testparm i.edad1 i.nivel_educ i.sexo1 dcom
outreg2 using ols_todos_video2_parte1.xls, append ctitle("Modelo 3") label addstat("F-test", `r(F)', "Prob > F", `r(p)')


/*
La correlación de las variables junto con las estimaciones nos sirve para saber
si la variable en cuestión no está relacionada con variables que ya están
en el modelo. Solo tiene una correlación mediana con la edad, pero eso
no significa que no pueda aportar valor a la estimación, además de no correlacionarse
con ninguna otra.
Para saber si existe problema de omisión de variables relevantes es necsario
comparar los modelos con pocas variables independiente y otro con la/las que
queramos ver su existencia.
De inicio vemos que la probabilidad de que todas los coeficientes sean 0 es nula.
A posteriori la significatividad de la variable años en la empresa es del 99%
o 90%, lo cual nos dice algo positivo al respecto.
Finalmente el R2 para los modelos que poseen la variable años de experiencia son mayores,
pero no excesivamente, diciéndonos que esta variable aporta valor al modelo
pero no significa que estemos cometiendo un error de omisión de
variable redundante.
*/


// APARTADO H
* Modelo 3
estimates restore ols3_horase

testparm i.edad1 i.nivel_educ i.sexo1 dcom
outreg2 using ols_loglin_video2_parte1.xls, replace ctitle("Modelo 3") label addstat("F-test", `r(F)', "Prob > F", `r(p)')

* Generamos la variable horas en logaritmos
gen log_horase_ = ln(horase_)


* Modelo 4: log-lin tramos de edad, niveles educativos, sexo y años en la empresa
regress log_horase_ i.edad1 i.nivel_educ i.sexo1 dcom [aweight=factorel] if ciclo == 189, robust

estimates store ols4_horase

testparm i.edad1 i.nivel_educ i.sexo1 dcom
outreg2 using ols_loglin_video2_parte1.xls, append ctitle("Modelo 4") label addstat("F-test", `r(F)', "Prob > F", `r(p)')

coefplot, keep(20.edad1 25.edad1 30.edad1 35.edad1 40.edad1 45.edad1 50.edad1 55.edad1 60.edad1) vertical nolabel rename(20.edad1 = 20 25.edad1=25 30.edad1=30 35.edad1=35 40.edad1=40 45.edad1=45 50.edad1=50 55.edad1=55 60.edad1=60 65.edad1=65 ) xtitle("Edad") ytitle("Coeficientes") recast(line) title("Gráfico 10. Oferta laboral sobre el ciclo de vida - España Modelo 4" , pos(6)) note("Fuente: INE (EPA 2020T4 & 2019T4)", pos(7)) name(coef2, replace)

graph export "video2_p1_coefplot4.png", width(3800) height(1800) replace
graph close

restore