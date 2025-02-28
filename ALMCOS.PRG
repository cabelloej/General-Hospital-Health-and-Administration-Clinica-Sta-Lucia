STORE SPACE(3)  TO WLINEA
STORE SPACE(20) TO WCODIGO
STORE SPACE(1)  TO WULTCOS
STORE 0         TO WFACTOR
@ 10,54 CLEAR TO 18,77
@ 10,54       TO 18,77
@ 11,55  SAY "     RECOSTEO  "
@ 12,55  SAY "LINEA DE VENTA:"
@ 13,55  SAY "CODIGO ARTICULO"
@ 15,55  SAY "ULT.COSTO(S/N):"
@ 16,55  SAY "FACTOR (xCOST):"
@ 12,71 GET WLINEA
READ
IF READKEY() = 12 .OR. READKEY() = 268
   RETURN
ENDIF
IF WLINEA = SPACE(3)
   @ 12,70 SAY "TODAS"
ELSE
   SELECT ALMLIN
   FIND &WLINEA
   IF EOF()
      STORE "LINEA DE VENTA NO REGISTRADA. <ENTER>" TO MES
      DO AVISO WITH MES
      RETURN
   ENDIF
ENDIF
@ 14,55 GET WCODIGO
READ
IF READKEY() = 12 .OR. READKEY() = 268
   RETURN
ENDIF
IF WCODIGO = SPACE(20)
   @ 14,55 SAY "TODOS"
ELSE
   SELECT ALMART
   FIND &WCODIGO
   IF EOF()
      STORE "ARTICULO NO REGISTRADO. <ENTER>" TO MES
      DO AVISO WITH MES
      RETURN
   ENDIF
ENDIF
STORE .T. TO VERCOS
DO WHILE VERCOS
   STORE SPACE(1) TO WULTCOS
   @ 15,70 GET WULTCOS
   READ
   STORE UPPER(WULTCOS) TO WULTCOS
   IF WULTCOS = "S" .OR. WULTCOS = "N"
      STORE .F. TO VERCOS
      LOOP
   ENDIF
ENDDO
IF WULTCOS = "N"
   @ 16,70 GET WFACTOR PICTURE "##.##" RANGE 0.1, 99.99
   READ
   IF WFACTOR = 0 .OR. READKEY() = 12 .OR. READKEY() = 268
      RETURN
   ENDIF
ENDIF
STORE "CONFORME ? (S/N)" TO TEX
STORE "NS" TO WCH
DO PREGUNTA
IF WCH = "N"
   RETURN
ENDIF
STORE "RECOSTEANDO ARTICULOS SELECCIONADOS. FAVOR ESPERAR..." TO MES
DO MENSAJE WITH MES

SELECT ALMART
GO TOP
DO WHILE .NOT. EOF()
   IF WLINEA <> SPACE(3) .AND. WLINEA <> LINEA
      SKIP
      LOOP
   ENDIF
   IF WCODIGO<> SPACE(20) .AND. WCODIGO <> CODIGO
      SKIP
      LOOP
   ENDIF
   STORE CODIGO     TO WCODIGO
   STORE WFECACT    TO WFECHAOP
   STORE "AC"       TO WOPE
   STORE "RECOSTEO" TO WNUMERO
   STORE "INV"      TO WORIGEN
   STORE 0          TO WUNIDADES
   STORE 0          TO WTOTAL
   STORE COSEXI TO WULCOEX
   IF WULTCOS = "S"
      IF ULTCOSTO > 0
         IF RECLOC()
            REPLACE COSEXI WITH ULTCOSTO
            UNLOCK ALL
         ELSE
            RETURN
         ENDIF
      ELSE
         SELECT ALMART
         SKIP
         LOOP
      ENDIF
   ELSE
      IF RECLOC()
         REPLACE COSEXI WITH COSEXI*WFACTOR
         UNLOCK ALL
      ELSE
         RETURN
      ENDIF
   ENDIF
   STORE COSEXI-WULCOEX   TO WVARCOS
   STORE WVARCOS * UNIEXI TO WCOSTO

   SELECT ALMTRA
   IF FILLOC()
      APPEND BLANK
      REPLACE SERIAL WITH ALMDATA->SERIAL+1
      replace codigo with wcodigo
      replace fecha with wfechaop
      replace operacion with wope
      replace referencia with wnumero
      replace origen with WORIGEN
      REPLACE UNIDADES WITH WUNIDADES
      REPLACE COSTO WITH WCOSTO
      UNLOCK ALL
      FLUSH
   ELSE
      RETURN
   ENDIF
   SELECT ALMDATA
   IF RECLOC()
      REPLACE SERIAL WITH SERIAL+1
      FLUSH
      UNLOCK ALL
   ELSE
      RETURN
   ENDIF
   SELECT ALMART
   SKIP
ENDDO
RETURN



