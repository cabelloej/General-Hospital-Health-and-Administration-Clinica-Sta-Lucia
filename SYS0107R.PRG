SET TALK OFF
SET ECHO OFF
DEFI WIND WINREPLIS FROM 10,10 TO 16,70; 
          TITLE "IMPRIMIR LISTA PERSONAL MEDICO";
          DOUBLE NOFLOAT NOZOOM NOGROW SHADOW COLOR SCHEME 10
ACTI WIND WINREPLIS

STORE SPACE(2) TO WCODFILTRO
@ 00,01 SAY "ESPECIALIDAD   :" 
@ 00,20 GET WCODFILTRO 
READ
IF LASTKEY()=27
   RELE WIND WINREPLIS
   RETURN
ENDIF

store 1 to wop
do while .t.
   @ 04,20  get wop pict "@*H Aceptar;Cancelar" defa wop
   read
   if lastkey()=13
      exit
   endif
enddo
rele wind winreplis
if lastkey()=27.or.wop=2
   return
endif

STORE 0 TO WPAGE
STORE 0 TO WLINE
SELECT SYSMED
STORE RECNO() TO WSTARTREC
SET DEVI TO PRINT
DO SALTO
*SELECT 1
*USE SYSSERVI INDEX SYSSERVI
GO TOP
DO WHILE .NOT. EOF()
   IF WCODFILTRO<>SPACE(2).AND.WCODFILTRO<>ESPECI
      SKIP
   ENDIF
   STORE WLINE + 1 TO WLINE
   IF WLINE > 55
      DO SALTO
   ENDIF
   @ WLINE,00     SAY MEDICO  
   @ WLINE,05     SAY USERCODE
   @ WLINE,15     SAY NOMBRE   
   @ WLINE,50     SAY ESPECI
   SKIP
ENDDO
SET DEVI TO SCRE
GO WSTARTREC
RETURN
**********
PROC SALTO
**********
@ 00,00  SAY CHR(18)+CHR(14)+QQWW
@ 01,00  SAY CHR(18)+"Lista de personal medico"
@ 03,00  SAY CHR(18)
@ 03,00  SAY "N�"
@ 03,05  SAY "Codigo"
@ 03,15  SAY "Nombre"
@ 03,50  SAY "Especialidad"         
@ 04,00  SAY "---"
@ 04,05  SAY "--------"
@ 04,15  SAY "------------------------------"
@ 04,50  SAY "--"  
STORE 5 TO WLINE
RETURN
