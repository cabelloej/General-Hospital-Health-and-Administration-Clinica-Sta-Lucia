SAVE SCRE TO WSCRECAR1
DEFI WIND WINDPA FROM 03,00 TO 07,79 ;
                 TITLE  " PACIENTE " ;
                 FOOTER " ESC=Salir ";
                 DOUBLE NOFLOAT NOZOOM NOGROW COLOR SCHEME 10
ACTI WIND WINDPA
@ 00,02 SAY "Codigo              :"
@ 01,02 SAY "Cama                :"
@ 02,02 SAY "Cuenta de cargos    :"
*** INICIALIZA VARIABLES
STORE SPACE(30)                 TO WNOMBRE
STORE SPACE(10)                 TO WHISTORIA
STORE SPACE(20)                 TO WTIPDES
STORE SUBSTR(WCODPAC,1,10)      TO WCODPAC1
STORE SUBSTR(WCODPAC,11,4)      TO WCODPAC2
STORE SPACE(4)                  TO M.CAMA
STORE SPACE(10)                 TO M.CUENTA

STORE VAL(WCODPAC2)             TO WCODPAC2
@ 00,25 GET WCODPAC1
@ 00,35 SAY "-"
@ 00,36 GET WCODPAC2 PICTURE "9999" VALID VALPAC()
READ
STORE STR(WCODPAC2,4)                     TO WCODPAC2
STORE WCODPAC1+WCODPAC2                   TO WCODPAC
STORE RTRIM(WCODPAC1)+"-"+LTRIM(WCODPAC2) TO WXCODPAC
IF LASTKEY()=27
   RELE WIND WINDPA
   RETURN
ENDIF
@ 00,25 SAY SPACE(40)
@ 00,25 SAY WXCODPAC+" "+WNOMBRE+" ("+wtipdes+")"
@ 01,25 SAY ADMADM.CAMA
@ 02,25 SAY ADMADM.CUENTA
STORE ADMADM.CUENTA TO WCUENTA
SAVE SCRE TO WSCRECAR2
DEFI WIND WINDCA FROM 08,00 TO 19,79 ;
                 TITLE  " CARGOS " ;
                 FOOTER " F6=Presupuesto y Edo.de Cuenta, ESC=Salir ";
                 DOUBLE NOFLOAT NOZOOM NOGROW COLOR SCHEME 10
DEFI WIND WINDTT FROM 20,00 TO 23,79 ;
                 DOUBLE NOFLOAT NOZOOM NOGROW COLOR SCHEME 10
ACTI WIND WINDCA
ON KEY LABEL F1
ON KEY LABEL F2
ON KEY LABEL F3
ON KEY LABEL F4
ON KEY LABEL F6 DO PROCPRE
ON KEY LABEL F7
ON KEY LABEL F8
ON KEY LABEL F9
ON KEY LABEL F10
SELECT SERDCGE
SET ORDER TO SERDCGE4
BROWSE FIELDS NUMERO:H="COMPROBANTE", REFERENCIA:H="REFERENCIA",ELABORADO:H="FECHA", ORDENADO:H="SOLICITANTE",;
              PROVEEDOR:H="EJECUTOR",MONTO:H="MONTO";
              KEY ADMADM.CUENTA FOR SUBSTR(NUMERO,1,3)="COM";
              NOAPPEND NODELETE NOEDIT NOMENU NOOPTIMIZE REST SAVE IN WINDOW WINDCA WHEN FUNTOT()
ON KEY LABEL F1
ON KEY LABEL F2
ON KEY LABEL F3
ON KEY LABEL F4
ON KEY LABEL F5
ON KEY LABEL F6
ON KEY LABEL F7
ON KEY LABEL F8
ON KEY LABEL F9
ON KEY LABEL F10
RELEASE WIND WINDPA
RELEASE WIND WINDCA
RELEASE WIND WINDTT
RETURN
***********
FUNC FUNTOT
***********
STORE RECNO() TO WREGINI
STORE 0 TO WTOTCTA
DO TOTCTA
GO WREGINI
ACTI WIND WINDTT
@ 0,58 SAY "TOTAL :"
@ 0,64 SAY WTOTCTA PICTURE "999,999,999.99"
ACTI WIND WINDCA
RETURN
***********
PROC TOTCTA
***********
SELECT SERDCGE
SEEK ADMADM.CUENTA
DO WHILE .NOT.EOF().AND.CUENTA=ADMADM.CUENTA
   IF SUBSTR(NUMERO,1,3)="COM"
      STORE WTOTCTA+MONTO TO WTOTCTA
   ENDIF
   SKIP
ENDDO
RETURN
****************
PROCEDURE VALPAC
****************
SELECT ADMADM
SET ORDER TO ADMADM2
STORE WCODPAC1+STR(WCODPAC2,4) TO WCODPAC
SEEK WCODPAC
IF .NOT.FOUND()
   STORE "PACIENTE NO INGRESADO POR INGRESOS HOSPITALARIOS" TO WTEXT
   DO AVISO WITH WTEXT
   RETURN .F.
ELSE
   STORE RTRIM(PAPELLIDO)+" "+SUBSTR(SAPELLIDO,1,1)+" "+RTRIM(PNOMBRE)+" "+SUBSTR(SNOMBRE,1,1) TO WNOMBRE
ENDIF
IF TIPPAC="A"
   STORE "Afiliado"    to wtipdes
ELSE
   STORE "No afiliado" to wtipdes
ENDIF
RETURN
************
PROC PROCPRE
************
***
DEFI WIND WINDREP FROM 0,0 TO 24,79
ACTI WIND WINDREP
PUSH KEY CLEAR
SELECT ADMADM
STORE RECNO() TO WRECPAC
***
DO PROCCTA
***
SELECT 20
USE
SELECT 21
USE
SELECT 22
USE
@ 0,0 CLEAR
RELE WIND WINDREP
SELECT SERDCGE
SET ORDER TO SERDCGE4
POP KEY
RETURN
************
PROC PROCCTA
************
***
SELECT 20
USE FACPREGE INDEX FACPREG1,FACPREG2
SELECT 21
USE FACPREDE INDEX FACPREDE
SELECT 22
USE ADMCTA   INDEX ADMCTA
DO WHILE .T.
   SEEK ADMADM.CUENTA
   IF FOUND()
      IF RECLOC()
         DELETE
         UNLOCK ALL
      ENDIF
   ELSE
      EXIT
   ENDIF
ENDDO
***
SELECT FACPREGE
SET ORDER TO FACPREG1
SEEK ADMADM.REFERE
IF .NOT. FOUND()
   STORE "PACIENTE SIN REFERENCIA PRESUPUESTARIA" TO WTEXT
   DO AVISO WITH WTEXT
ENDIF
STORE NOMTRA1 TO WNOMTRA1
STORE NOMTRA2 TO WNOMTRA2
SELECT FACPREDE
SEEK ADMADM.REFERE
DO WHILE .NOT. EOF() .AND. NUMERO=ADMADM.REFERE
   STORE TIPITEM    TO WTIPITEM
   STORE ITEM       TO WITEM
   STORE CANTIDAD   TO WCANTIDAD
   *STORE PRECIO     TO WPRECIO
   STORE DESCUENTO  TO WDESCUENTO
   ***
   SELECT ADMCTA
   IF FILLOC()
      APPEND BLANK
      REPLACE NUMERO     WITH ADMADM.CUENTA
      REPLACE TIPITEM    WITH WTIPITEM
      REPLACE ITEM       WITH WITEM
      REPLACE CANPRE     WITH WCANTIDAD
      REPLACE CANTIDAD   WITH 0
      *REPLACE PRECIO     WITH WPRECIO
      REPLACE DESCUENTO  WITH WDESCUENTO
      UNLOCK
   ELSE
      STORE "OPERACION ABORTADA, REINTENTE" TO MES
      DO AVISO WITH MES
      RETURN
   ENDIF
   SELECT FACPREDE
   SKIP
ENDDO
*** TRANSFIENE LOS COMPROBANTES
SELECT SERDCGE
SET ORDER TO SERDCGE4
SEEK ADMADM.CUENTA
DO WHILE .NOT. EOF() .AND. ADMADM.CUENTA=CUENTA
   IF SUBSTR(NUMERO,1,3)<>"COM"
      SELECT SERDCGE
      SKIP
      LOOP
   ENDIF
   STORE NUMERO TO WNUMDCTO
   SELECT SERDCDE
   SEEK WNUMDCTO
   DO WHILE .NOT. EOF() .AND. NUMERO=WNUMDCTO
      STORE TIPITEM  TO WTIPITEM
      STORE ITEM     TO WITEM
      STORE CANTIDAD TO WCANTIDAD
      STORE PRECIO   TO WPRECIO
      STORE EJECUTOR TO WEJECUTOR
      STORE .F. TO WFLAGESTA
      SELECT ADMCTA
      SEEK ADMADM.CUENTA
      DO WHILE .NOT.EOF().AND.NUMERO=ADMADM.CUENTA
         IF TIPITEM=WTIPITEM.AND.ITEM=WITEM
            STORE .T.   TO WFLAGESTA
            EXIT
         ENDIF
         SKIP
      ENDDO
      IF .NOT. WFLAGESTA
         ***
         SELECT ADMCTA
         IF FILLOC()
            APPEND BLANK
            REPLACE NUMERO     WITH ADMADM.CUENTA
            REPLACE TIPITEM    WITH WTIPITEM
            REPLACE ITEM       WITH WITEM
         ELSE
            STORE "OPERACION ABORTADA, REINTENTE" TO MES
            DO AVISO WITH MES
            RETURN
         ENDIF
      ENDIF
      REPLACE PRECIO           WITH ((CANTIDAD*PRECIO)+(WCANTIDAD*WPRECIO))/(WCANTIDAD+CANTIDAD)
      REPLACE CANTIDAD         WITH CANTIDAD+WCANTIDAD
      UNLOCK ALL
      ***
      SELECT SERDCDE
      SKIP
   ENDDO
   SELECT SERDCGE
   SKIP
ENDDO

*** IMPRIME
DEFI WIND WINDVER FROM 12,19 TO 14,59;
          TITLE " SALIDA ";
          DOUBLE NOFLOAT NOZOOM NOGROW SHADOW COLOR SCHEME 10
ACTIVATE WIND WINDVER
DO WHILE .T.
   STORE 1 TO WOP
   @ 00,03 SAY "OPCIONES: ";
   GET WOP PICT "@*H MONITOR  ;IMPRESORA "
   READ
   IF LASTKEY()=13
      EXIT
   ENDIF
ENDDO
RELE WIND WINDVER
STORE ADMADM.CUENTA TO WNUMERO
STORE WOP           TO WSALIDA

*SELECT FACPREGE
*SEEK WNUMERO
*IF .NOT. FOUND()
*   STORE "NUMERO DE PRESUPUESTO NO REGISTRADO, VERIFIQUE" TO WTEXT
*   DO AVISO
*   RETURN
*ENDIF
STORE 0   TO WPAGINA
STORE 0   TO WTOTREN
STORE 0   TO WTOTPRE
STORE 100 TO WLINEA
IF WSALIDA = 1
   DEFI WIND WINDSAL FROM 0,0 TO 25,81;
          NONE NOFLOAT NOZOOM NOGROW NOSHADOW COLOR SCHEME 10
   ACTI WIND WINDSAL
   STORE 21 TO WSALTO
ELSE
   SET DEVI TO PRINT
   STORE 60 TO WSALTO
ENDIF

SELECT ADMCTA
SEEK ADMADM.CUENTA
STORE "**" TO WRUPTIP
STORE "**" TO WRUPESP
STORE "**" TO WRUPGRU
STORE .T.  TO WFLAGMED
DO WHILE .NOT.EOF().AND.NUMERO=ADMADM.CUENTA
   STORE SUBSTR(ITEM,1,2) TO WTIP
   STORE SUBSTR(ITEM,1,5) TO WESP
   STORE SUBSTR(ITEM,1,8) TO WGRU
   DO RUPTURA
   ***
   IF ADMCTA.TIPITEM="B"
      SELECT ALMART
   ELSE
      SELECT SYSSERVI
   ENDIF
   SEEK ALLTRIM(ADMCTA.ITEM)
   IF FOUND()
      STORE  DESCRI TO WNOMITEM
   ELSE
      STORE ADMCTA.ITEM+" NO REG." TO WNOMITEM
   ENDIF
   STORE WLINEA+1 TO WLINEA
   DO VERSALTO
   SELECT ADMCTA
   IF WSALIDA=1
      @ WLINEA,00 SAY SUBSTR(WNOMITEM,1,40)
   ELSE
      @ WLINEA,00 SAY CHR(18)+CHR(15)+WNOMITEM+CHR(18)
   ENDIF
   @ WLINEA,41 SAY CANPRE     PICTURE "9999.99"
   @ WLINEA,49 SAY CANTIDAD   PICTURE "9999.99"
   @ WLINEA,57 SAY PRECIO     PICTURE "9999999.99"
   @ WLINEA,68 SAY DESCUENTO  PICTURE "99.99"
   STORE CANTIDAD*(PRECIO-(PRECIO*DESCUENTO/100)) TO WTOTREN
   *@ WLINEA,66 SAY WTOTREN    PICTURE "99,999,999.99"
   STORE WTOTPRE+WTOTREN TO WTOTPRE
   SELECT ADMCTA
   SKIP
ENDDO
STORE WLINEA+1 TO WLINEA
DO VERSALTO
@ WLINEA,00 SAY REPLICATE(".",79)
STORE WLINEA+1 TO WLINEA
DO VERSALTO
@ WLINEA,40 SAY "TOTAL CUENTA :"
@ WLINEA,40 SAY "TOTAL CUENTA :"
@ WLINEA,66 SAY WTOTPRE PICTURE "99,999,999.99"

IF WSALIDA=1
   STORE "OPRIMA ENTER PARA FINALIZAR" TO WTEXT
   DO AVISO WITH WTEXT
   RELE WIND WINDSAL
ELSE
   EJECT
   SET DEVI TO SCRE
ENDIF
RETURN
************************************************************************
*************
PROC VERSALTO
*************
IF WLINEA>WSALTO
   IF WSALIDA=1.AND.WPAGINA>0
      STORE "OPRIMA ENTER PARA CONTINUAR" TO WTEXT
      DO AVISO WITH WTEXT
   ENDIF
   DO HEADING
ENDIF
RETURN
*************
PROC HEADING
************
STORE WPAGINA+1 TO WPAGINA
IF WSALIDA = 1
   @ 00,00 CLEAR
   @ 00,00 SAY QQWW
ELSE
   @ 00,00 SAY CHR(14)+QQWW
   @ 00,00 SAY CHR(14)+QQWW
ENDIF
@ 02,55 SAY "Cuenta      :"+ADMCTA.NUMERO
@ 03,55 SAY "Fecha       :"+DTOC(DATE())
@ 04,55 SAY "Pagina      :"+ALLTRIM(STR(WPAGINA,3))
@ 05,00 SAY "Paciente   :"+ADMADM.PAPELLIDO+" "+ADMADM.PNOMBRE
*IF SUBSTR(FACPREGE.CODCLI,1,1)="*"
*   STORE FACPREGE.NOMCLI          TO WNOMCLI
*ELSE
*   SELECT FACCLI
*   SEEK FACPREGE.CODCLI
*   IF FOUND()
*      STORE DESCLI                TO WNOMCLI
*   ELSE
*      STORE "NO REG. EN CLIENTES" TO WNOMCLI
*   ENDIF
*ENDIF
*@ 07,00 SAY "Cliente    :"+ALLTRIM(FACPREGE.CODCLI)+" "+WNOMCLI
@ 09,00 SAY replicate("-",79)
@ 09,00 SAY replicate("-",79)
@ 10,00 say "Servicio"
@ 10,00 say "Servicio"
@ 10,41 say "Presup."
@ 10,41 say "Presup."
@ 10,49 say "Ejecut."
@ 10,49 say "Ejecut."
@ 10,57 say "    Precio"
@ 10,57 say "    Precio"
@ 10,68 say "%Desc."
@ 10,68 say "%Desc."
*@ 10,66 say "  Total Reng."
*@ 10,66 say "  Total Reng."
@ 11,00 SAY replicate("-",79)
@ 11,00 SAY replicate("-",79)
STORE 12 TO WLINEA
RETURN
************
PROC RUPTURA
************
IF WTIP<>WRUPTIP.OR.WESP<>WRUPESP.OR.WGRU<>WRUPGRU
   IF ADMCTA.TIPITEM="S"
      SELECT SYSSERVI
      IF WTIP<>WRUPTIP
         SEEK WTIP+"-  -  -   "
         IF FOUND()
            STORE  DESCRI     TO WNOMTIP
         ELSE
            STORE SPACE(1)    TO WNOMTIP
         ENDIF
         STORE WLINEA+1 TO WLINEA
         DO VERSALTO
         @ WLINEA,40-LEN(ALLTRIM(WNOMTIP))/2 SAY ALLTRIM(WNOMTIP)
         @ WLINEA,40-LEN(ALLTRIM(WNOMTIP))/2 SAY ALLTRIM(WNOMTIP)
         *STORE WLINEA+1 TO WLINEA
         *DO VERSALTO
         *@ WLINEA,40-LEN(ALLTRIM(WNOMTIP))/2 SAY REPLICATE("-",LEN(ALLTRIM(WNOMTIP)))
         STORE WTIP TO WRUPTIP
      ENDIF
      IF WESP<>WRUPESP
         SEEK WESP+"-  -   "
         IF FOUND()
            STORE  ". "+DESCRI TO WNOMESP
         ELSE
            STORE SPACE(1)    TO WNOMESP
         ENDIF
         STORE WLINEA+1 TO WLINEA
         DO VERSALTO
         @ WLINEA,00 SAY WNOMESP
         STORE WESP TO WRUPESP
      ENDIF
      IF WGRU<>WRUPGRU
         SEEK WGRU+"-   "
         IF FOUND()
            STORE  "._ "+DESCRI TO WNOMGRU
         ELSE
            STORE SPACE(1)    TO WNOMGRU
         ENDIF
         STORE WLINEA+1 TO WLINEA
         DO VERSALTO
         @ WLINEA,00 SAY WNOMGRU
         STORE WGRU TO WRUPGRU
      ENDIF
   ELSE
      IF WFLAGMED
         STORE WLINEA+1 TO WLINEA
         DO VERSALTO
         @ WLINEA,20 SAY  "MEDICAMENTOS y MATERIAL MEDICO-QUIRURJICO"
         @ WLINEA,20 SAY  "MEDICAMENTOS y MATERIAL MEDICO-QUIRURJICO"
         *STORE WLINEA+1 TO WLINEA
         *DO VERSALTO
         *@ WLINEA,20 SAY  "-----------------------------------------"
         STORE .F. TO WFLAGMED
      ENDIF
   ENDIF
ENDIF
RETURN
******

