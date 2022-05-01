*Do file para identificar códigos CIE-10 

*fuente: https://www.stata.com/stata14/icd-10/
clear all
*Cargar base de datos de incapacidades
*import delimited "C:\Users\gaga1\OneDrive\Documents\MEPI\Semestre 3\Proyecto de grado I\Datos\muestra.csv"
import delimited "C:\Users\gaga1\OneDrive\Documents\MEPI\Semestre 3\Proyecto de grado I\Datos filtrados\base_filtrada.csv" 

*Determinar que códigos son inválidos
icd10 check diag, version(2015) generate(invalid) 
tab invalid
*Códigos invalidos: 
tab invalid diag if invalid == 8

*Dummy Enfermedad isquemica cardíaca 
icd10 generate eic = diag, range(I20/I216 | I219/I259)
tab eic


*Dummy Stroke
icd10 generate stroke = diag, range(G45/G468 | I60/I62 | I629/I64 | I641 | I65/I69998)
tab stroke

*Dummy Cancer de traquea, bronquios y pulmón
icd10 generate ctbp = diag, range(C33| C34/C3492)
tab ctbp

*Dummy Cancer de Estomago
icd10 generate ce = diag, range(C16/C169)
tab ce

*Dummy Cancer de Páncreas
icd10 generate cp = diag, range(C25/C259)
tab cp

*Dummy Cancer de cervix
icd10 generate cc = diag, range(C53/C539)
tab cc

*Dummy EPOC
icd10 generate epoc = diag, range (J41/J424 | J43/J449)
tab epoc

*Dummy Infecciones respiratoria de las vías bajas
icd10 generate ir = diag, range (A481 | A70 | B960/B961 | B9721 | B974/B976 | J09/J18.2 | J188/J189 | J196/J229 | J851 | J910 | P23/P239 | U04/U049)
tab ir

*Revisar que códigos tienen problemas y que son de posible interés

*list diag if invalid != 0 

*Asignar Nombres a códigos CIE-10
icd10 generate descr_diag = diag, description

*Crear una variable numérica y label sus valores: *Fuente:https://www.stata.com/support/faqs/data-management/labeling-icd-codes/
icd10 generate descr = diag, description long
encode descr, generate(dxlabeled) label(descrip)
compress 
drop descr

*Diagnósticos a los que no se les asignó nombre
/*
      DIAG |      Freq.     Percent        Cum.
------------+-----------------------------------
       C33X |          4        0.02        0.02
       G45X |          2        0.01        0.03
       I64X |      2,670       11.85       11.88
       J09X |         27        0.12       12.00
       J13X |        237        1.05       13.05
       J14X |         19        0.08       13.13
       J22X |     17,171       76.22       89.35
       J42X |      1,979        8.78       98.14
       J91* |          3        0.01       98.15
       J91X |        417        1.85      100.00
------------+-----------------------------------
      Total |     22,529      100.00

*/

*Guardar base de datos
export delimited using "C:\Users\gaga1\OneDrive\Documents\MEPI\Semestre 3\Proyecto de grado I\Datos filtrados\base_filtrada_stata.csv", replace

