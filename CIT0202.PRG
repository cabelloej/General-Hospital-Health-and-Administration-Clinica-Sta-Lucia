SELECT AFIAFI
STORE 05 TO LIBRCAR
STORE 00 TO CIBRCAR
on key label f4 do winbus
DEFI WIND BRCAR FROM LIBRCAR,CIBRCAR TO LIBRCAR+19,CIBRCAR+79;
                TITLE " BUSQUEDA DE AFILIADOS POR CEDULA DEL TITULAR ";
                FOOTER " F4=Buscar cedula, ESC=Seleccionar ";
                DOUBLE NOFLOAT NOZOOM NOGROW SHADOW COLOR SCHEME 10
ACTI WIND BRCAR
BROWSE FIELDS CEDTITU:H="C.I.TITULAR",CARGA:H="No.",AFIPAREN.DESCRI:H="PARENTESCO",;
              PAPELLIDO:H="PRIMER APELLIDO", SAPELLIDO:H="SEGUNDO APELLIDO",;
              PNOMBRE:H="PRIMER NOMBRE", SNOMBRE:H="SEGUNDO NOMBRE", CEDAFI:H="CED. CARGA",;
              SEXO:H="S", NACIMIENTO:H="FECHA NAC.";
              NOAPPEND NODELETE NOEDIT NOMENU NOOPTIMIZE REST SAVE IN WINDOW BRCAR
RELEASE WIND BRCAR
STORE AFIAFI.CEDTITU+AFIAFI.CARGA TO WCODPAC
RETURN
procedure winbus
DEFI WIND winbus FROM LIBRCAR+3,CIBRCAR TO LIBRCAR+5,CIBRCAR+79;
                TITLE " BUSQUEDA DE AFILIADOS POR CEDULA DEL TITULAR ";
                DOUBLE NOFLOAT NOZOOM NOGROW SHADOW COLOR SCHEME 10
acti wind winbus
@ 00,00 clear
store space(10) to m.cedtitu
@ 00,00 get m.cedtitu
read
if lastkey()=27
   rele wind winbus
   return
endif
select afiafi
set order to afiafi1
seek alltrim(m.cedtitu)
if .not. found()
   store "NO EXISTE, VERIFIQUE" to wtext
   do aviso with wtext
endif
rele wind winbus
return


