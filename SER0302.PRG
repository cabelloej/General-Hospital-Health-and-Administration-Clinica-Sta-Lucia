DO WHILE .T.
   STORE 0 TO WMONTO
   @ 04,00 CLEAR
   @ 04,20 SAY "COMPROBANTES DE SERVICIO"
   @ 05,00 TO 22,79
   *@ 06,5 SAY    "EDO.DE SOLIC.:"
   @ 07,5 SAY    "DIRIGIDO A   :"
   @ 08,5 SAY    "ORDENADO POR :"
   @ 09,5 SAY    "PACIENTE     :"
   @ 10,5 SAY    "DESDE        :"
   @ 11,5 SAY    "HASTA        :"
      
   @ 14,5 SAY    "* SALIDA *"
   @ 15,5 SAY    "MONITOR ...1"
   @ 16,5 SAY    "IMPRESORA..2"
   @ 17,5 SAY    "          [ ]"
   STORE 1 TO WSALIDA
   *STORE 0 TO WRECIBI
   *STORE SPACE(10) TO WREC
   *STORE "Todas=0, Procesadas=1, Pendientes=2, Esc=Salir" TO MES
   *DO MENSAJE WITH MES
   *@ 06,20 GET WRECIBI PICT "9" RANGE 0,2
   *READ
   *IF LASTKEY()=27
   *   RETURN
   *ENDIF
   *DO CASE
   *   CASE WRECIBI=0
   *        WREC="Todas"
   *   CASE WRECIBI=1
   *        WREC="Procesadas"
   *   CASE WRECIBI=2
   *        WREC="Pendientes"
   *ENDCASE
   *@ 06,38 SAY WREC
       
   STORE "CODIGO DEL PROVEEDOR DE SERVICIOS (EJECUTOR), ENTER=<Todos>" TO MES
   DO MENSAJE WITH MES
   STORE SPACE(4) TO WPRO
   STORE SPACE(3) TO WPROVE
   @ 07,20 GET WPROVE
   READ
   IF LASTKEY()=27
      RETURN
   ENDIF
   IF WPROVE=SPACE(3)
      WPRO="Todos"
      @ 07,38 SAY WPRO
   ENDIF

   STORE SPACE(3) TO WORDEN
   STORE SPACE(4)  TO WORD
   STORE "ESCRIBA CODIGO DEL SOLICITANTE, <ENTER>=TODOS" TO MES
   DO MENSAJE WITH MES
   @ 08,20 GET WORDEN
   READ
   IF WORDEN=SPACE(3)
      WORD="Todos"
      @ 08,38 SAY "Todos"
   ENDIF

   STORE SPACE(14) TO WCODPAC
   STORE SPACE(5) TO WRES
   STORE "ESCRIBA CODIGO DEL PACIENTE, <ENTER>=TODOS" TO MES
   DO MENSAJE WITH MES
   @ 09,20 GET WCODPAC
   READ
   IF WCODPAC=SPACE(14)
      WRES="Todos"
      @ 09,38 SAY WRES
   ENDIF
      
   STORE DATE() TO WDESDE
   STORE DATE() TO WHASTA
   STORE "RANGO DE FECHAS DE ELABORACION DE DOCUMENTOS" TO MES
   DO MENSAJE WITH MES
   @ 10,20 GET WDESDE
   @ 11,20 GET WHASTA
   READ
   IF WHASTA<WDESDE
      STORE "ERROR EN RANGO DE FECHAS, VERIFIQUE" TO MES
      DO AVISO WITH MES
      LOOP
   ELSE
      EXIT
   ENDIF   
ENDDO

@ 17,16 GET WSALIDA PICT "#" RANGE 1,2
READ
IF WSALIDA=1
   STORE "Monitor" TO WSAL
   STORE 22 TO WSALTO
ELSE
   STORE "Impresora" TO WSAL
   STORE 60 TO WSALTO
ENDIF
@ 17,38 SAY WSAL

SAVE SCRE TO INF1
STORE "CONTINUAR ? (S/N)" TO TEX
STORE "SN" TO WCH
DO PREGUNTA
IF WCH = "N"
   RETURN
ENDIF

@ 17,38 SAY WSAL
STORE 0 TO PAGINA
STORE 100 TO LINE
IF WSALIDA = 1
   @ 0,0 CLEAR
ELSE
   SET DEVI TO PRINT
ENDIF
SELECT SERDCGE
GO TOP
DO WHILE .NOT. EOF()
   ***************** FILTROS
   IF SUBSTR(NUMERO,1,3)<>"COM"
      SELECT SERDCGE
      SKIP
      LOOP
   ENDIF
   *****************
   *IF WRECIBI<>0
   *   IF WRECIBI=1
   *      IF COMPROB=SPACE(10)
   *         SELECT SERDCGE
   *         SKIP
   *         LOOP
   *      ENDIF
   *   ELSE
   *      IF COMPROB<>SPACE(10)
   *         SELECT SERDCGE
   *         SKIP
   *         LOOP
   *      ENDIF
   *   ENDIF
   *ENDIF
   IF WPROVE<>SPACE(3).AND.PROVEEDOR<>WPROVE
      SELECT SERDCGE
      SKIP
      LOOP
   ENDIF
   *****************
   IF WORDEN<>SPACE(3).AND.ORDENADO<>WORDEN
      SELECT SERDCGE
      SKIP
      LOOP
   ENDIF
   *****************
   IF ELABORADO>=WDESDE.AND.ELABORADO<=WHASTA
      **** TA' FACIL
   ELSE
      SELECT SERDCGE
      SKIP
      LOOP
   ENDIF

      STORE LINE+1 TO LINE
      IF LINE >= WSALTO
         STORE PAGINA + 1 TO PAGINA
         IF WSALIDA=1 .AND. PAGINA<>1
            STORE "CONTINUAR ? (S/N)" TO TEX
            STORE "SN" TO WCH
            DO PREGUNTA
            IF WCH = "N"
               RETURN
            ENDIF
            @ 0,0 CLEAR
         ENDIF
      
         @ 1,0   SAY QQWW
         @ 1,60  SAY "FECHA :"+DTOC(DATE())
         @ 2,0   SAY "COMPROBANTES DE SERVICIO"
         @ 2,60  SAY "PAGINA:"+STR(PAGINA,2)
         *@ 4,0   SAY "EDO.: "+WREC
         IF WPROVE=SPACE(3)
            @ 4,20  SAY "EJEC.: Todas"
         ELSE
            @ 4,20 SAY "EJEC.: "+WPROVE
         ENDIF
         IF WORDEN=SPACE(3)
            @ 4,40  SAY "SOL. : Todas"
         ELSE
            @ 4,40  SAY "SOL. : "+WORDEN
         ENDIF
         IF WCODPAC=SPACE(14)
            @ 4,60  SAY "C.PAC: Todos"
         ELSE
            @ 4,60  SAY "C.PAC: "+WCODPAC
         ENDIF
         @ 05,00 SAY "DESDE: "+DTOC(WDESDE)
         @ 05,20 SAY "HASTA: "+DTOC(WHASTA)
         *@ 07,00 SAY "EDO."
         @ 07,10 SAY "PROVEEDOR"
         @ 07,21 SAY "ORDENADO"
         @ 07,36 SAY "COD. PAC."
         @ 07,51 SAY "ELABORAD."
         @ 07,62 SAY "       MONTO" 
         @ 8,0   SAY REPLICATE("-",80)
         STORE 9 TO LINE
      ENDIF
      *IF COMPROB<>SPACE(10)
      *   @ LINE,0  SAY "Procesada"
      *ELSE
      *   @ LINE,0  SAY "Pendiente"
      *ENDIF
      @ LINE,10 SAY PROVEEDOR
      @ LINE,21 SAY ORDENADO
      @ LINE,36 SAY CODPAC
      @ LINE,51 SAY ELABORADO
      @ LINE,62 SAY MONTO
      WMONTO=WMONTO+MONTO
      SKIP
ENDDO
   *** COLOCAR TOTALES
   *******************
   IF WMONTO > 0
      STORE LINE+2 TO LINE
      @ LINE,49 SAY "MONTO TOTAL:"
      @ LINE,62 SAY "============"
      STORE LINE+1 TO LINE
      @ LINE,62 SAY WMONTO    
   ENDIF
   IF WSALIDA = 2
      SET DEVI TO SCRE
      EJECT
   ELSE
      STORE "OPRIMA (�) PARA FINALIZAR" TO MES
      DO AVISO WITH MES
      RESTORE SCRE FROM INF1
   ENDIF
RETURN
