STORE "ACTUALIZANDO ALMACEN, FAVOR ESPERAR..." TO MES
DO MENSAJE WITH MES
STORE .T. TO REVERSA
DO WHILE REVERSA
   SELECT ALMTRA
   STORE WNUMERO2 TO WXX
   FIND &WXX
   IF EOF()
      EXIT
   ENDIF
   STORE CODIGO   TO WCODIGO
   STORE UNIDADES TO WUNIDADES
   STORE COSTO    TO WCOSTO
   IF RECLOC()
      DELETE
      UNLOCK ALL
   ELSE
      STORE "ACTUALIZACION DE ALMACEN ABORTADA, FAVOR REINTENTAR" TO MES
      DO AVISO WITH MES
      STORE .F. TO REVERSA
      EXIT
   ENDIF
   IF WCODIGO = SPACE(20)
      STORE "ERROR, CODIGO DE ARTICULO VACION EN TRANS. ALM., VERIFIQUE" TO MES
      DO AVISO WITH MES
      LOOP
   ENDIF
   SELECT ALMART
   FIND &WCODIGO
   IF EOF()
      STORE "CODIGO DE ART:"+RTRIM(WCODIGO)+", EN MOV. NO EXISTE EN ALMACEN, VERIFIQUE" TO MES
      DO AVISO WITH MES
      LOOP
   ENDIF
   IF RECLOC()
      IF WTIPODOC="REC"
         IF UNIEXI - WUNIDADES <> 0
            REPLACE COSEXI WITH ((COSEXI*UNIEXI)-(WCOSTO)) / (UNIEXI-WUNIDADES)
         ELSE
            REPLACE COSEXI WITH 0
         ENDIF
         REPLACE UNIEXI WITH UNIEXI - WUNIDADES
      ELSE
         IF WTIPODOC="DEV"
            IF UNIEXI + WUNIDADES <> 0
               REPLACE COSEXI WITH ((COSEXI*UNIEXI)+(WCOSTO)) / (UNIEXI+WUNIDADES)
            ELSE
               REPLACE COSEXI WITH 0
            ENDIF
            REPLACE UNIEXI WITH UNIEXI + WUNIDADES
         ENDIF
      ENDIF
      UNLOCK ALL
      FLUSH
   ELSE
      STORE "OPERACION ABORTADA EN CALCULO DE COSTO. REINTENTE" TO MES
      DO AVISO WITH MES
   ENDIF
ENDDO
IF .NOT.REVERSA
   RETURN
ENDIF
**** REVERSANDO LOS MOVIMIENTOS DE LOTES AFECTADOS
SELECT ALMLOTTR
GO TOP
LOCATE FOR REFERENCIA = WNUMERO2
DO WHILE .NOT. EOF()
   STORE CODART    TO WCODART
   STORE CODLOT    TO WCODLOT
   STORE OPERACION TO WOPERACION
   STORE UNIDADES  TO WUNIX
   IF FILLOC()
      DELETE
      UNLOCK ALL
      FLUSH
   ENDIF
   SELECT ALMLOT
   STORE WCODART+WCODLOT TO WCLA
   FIND &WCLA
   IF .NOT. EOF()
      IF RECLOC()
         IF WOPERACION = "EN"
            REPLACE UNIDADES WITH UNIDADES - WUNIX
         ELSE
            IF WOPERACION = "SA"
               REPLACE UNIDADES WITH UNIDADES + WUNIX
            ENDIF
         ENDIF
      ELSE
         STORE "OPERACION ABORTADA EN RECALCULO DE LOTES. REINTENTE" TO MES
         DO AVISO WITH MES
      ENDIF
   ENDIF
   SELECT ALMLOTTR
   CONTINUE
ENDDO

*** GRABA LOS MOVIMIENTOS DE ALMACEN
SELECT COMDCDE
FIND &WNUMERO2
DO WHILE .NOT. EOF() .AND. NUMERO = WNUMERO2
   IF TIPITEM<>"B"
      SKIP
      LOOP
   ENDIF
   STORE ITEM     TO WITEM
   STORE ITEM     TO WCODIGO
   STORE CANTIDAD TO WCANTIDAD
   STORE PRECIO   TO WPRECIO
   IF WITEM = SPACE(20)
      STORE "ERROR, CODIGO DE ARTICULO VACIO EN RENGLONES. VERIFIQUE" TO MES
      DO AVISO WITH MES
      SKIP
      LOOP
   ENDIF
   SELECT ALMART
   FIND &WITEM
   IF EOF()
      STORE "ERROR, ARTI: "+RTRIM(WITEM)+" DOC.:"+WNUMERO2+" NO EXISTE EN ALMACEN" TO MES
      DO AVISO WITH MES
      SELECT COMDCDE
      SKIP
      LOOP
   ENDIF
   STORE (COSEXI*UNIEXI)      TO WCOSTACT
   STORE (WPRECIO*WCANTIDAD)  TO WCOSTFAC
   IF RECLOC()
      IF WTIPODOC="REC"
         REPLACE COSEXI WITH (WCOSTACT+WCOSTFAC)/(UNIEXI+(WCANTIDAD*PRESCAP))
         REPLACE UNIEXI WITH UNIEXI + (WCANTIDAD*PRESCAP)
         REPLACE ULTCOMPRA    WITH COMDCGE->ELABORADO
         REPLACE ULTCOSTO     WITH COMDCDE->PRECIO*COMDCDE->CANTIDAD
         REPLACE ULTCANTI     WITH COMDCDE->CANTIDAD*PRESCAP
         REPLACE ULTREFEREN   WITH COMDCGE->NUMERO
         REPLACE ULTPROVE     WITH COMDCGE->PROVEEDOR
      ELSE
         IF WTIPODOC="DEV"
            REPLACE COSEXI WITH (WCOSTACT-WCOSTFAC)/(UNIEXI-(WCANTIDAD*PRESCAP))
            REPLACE UNIEXI WITH UNIEXI - (WCANTIDAD*PRESCAP)
         ELSE
            STORE "ERROR, TIPO DE DOCUMENTO DESCONOCIDO PARA COMACTIN" TO MES
            DO AVISO WITH MES
         ENDIF
      ENDIF
      FLUSH
      UNLOCK ALL
   ELSE
      STORE "OPERACION ABORTADA EN RECALCULO DE COSTO, REINTENTE" TO MES
      DO AVISO WITH MES
      SELECT COMDCDE
      SKIP
      LOOP
   ENDIF
   SELECT ALMTRA
   IF FILLOC()
      append blank
      REPLACE SERIAL     WITH ALMDATA->SERIAL+1
      replace codigo     with COMDCDE->ITEM
      replace fecha      with COMDCGE->ELABORADO
      IF WTIPODOC="REC"
         replace operacion  with "EN"
      ELSE
         IF WTIPODOC="DEV"
            REPLACE OPERACION WITH "DE"
         ENDIF
      ENDIF
      replace referencia with COMDCGE->NUMERO
      replace origen     with "COM"
      REPLACE PROCLI     WITH COMDCGE->PROVEEDOR
      REPLACE UNIDADES   WITH WCANTIDAD*ALMART->PRESCAP
      REPLACE COSTO      WITH WCOSTFAC
      UNLOCK ALL
      FLUSH
      IF ALMART->LOTE = "S"
         SAVE SCRE TO S9
         STORE COMDCGE->ELABORADO TO WFECHAOP
         STORE (WCANTIDAD*ALMART->PRESCAP) TO WUNIDADES
         IF WTIPODOC="REC"
            STORE "EN"            TO WOPE
         ELSE
            IF WTIPODOC="DEV"
               STORE "SA"         TO WOPE
            ENDIF
         ENDIF
         STORE COMDCGE->NUMERO    TO WRESPALDO
         DO ALMTRL
         RESTORE SCRE FROM S9
      ENDIF
      SELECT ALMDATA
      IF RECLOC()
         REPLACE SERIAL WITH SERIAL+1
         UNLOCK ALL
         FLUSH
      ENDIF
   ELSE
      STORE "OPERACION ABORTADA, REPROCESE ALMACEN Y ESTE DOCUMENTO" TO MES
      DO AVISO WITH MES
   ENDIF
   SELECT COMDCDE
   SKIP
ENDDO 
RETURN