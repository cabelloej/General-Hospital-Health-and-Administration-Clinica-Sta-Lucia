IF WTIPODOC="REC"
   STORE "ACTUALIZANDO CUENTAS POR PAGAR, FAVOR ESPERAR..." TO MES
   DO MENSAJE WITH MES
   SELECT COMCXP
   SET ORDER TO COMCXP3
   STORE WNUMERO2 TO WXX
   FIND &WXX
   SET ORDER TO COMCXP1
   IF .NOT. FOUND()
      IF FILLOC()
         SELECT COMDATA
         IF RECLOC()
            REPLACE COMSERICXP WITH COMSERICXP+1
            STORE COMSERICXP TO WCODCXP
            UNLOCK ALL
            FLUSH
            STORE LTRIM(STR(WCODCXP,7))                 TO WCODCXP
            STORE REPLICATE("0",7-LEN(WCODCXP))+WCODCXP TO WCODCXP
         ELSE
            STORE "OPERACION ABORTADA, REINTENTE" TO MES
            DO AVISO WITH MES
            SELECT COMCXP
            UNLOCK ALL
            RETURN
         ENDIF
         SELECT COMCXP
         APPEND BLANK
         REPLACE CODCXP WITH WCODCXP
         UNLOCK ALL
         FLUSH
      ELSE
         STORE "OPERACION ABORTADA, REINTENTE" TO MES
         DO AVISO WITH MES
         SELECT COMCXP
         UNLOCK ALL
         RETURN
      ENDIF
   ENDIF
   IF RECLOC()
      REPLACE BENEFI     WITH COMDCGE->PROVEEDOR
      REPLACE REFERENCIA WITH COMDCGE->NUMERO
      REPLACE REFERPROVE WITH COMDCGE->REFERPROVE
      REPLACE ELABORADO  WITH COMDCGE->ELABORADO
      REPLACE SUBDOC     WITH COMDCGE->SUBDOC
      REPLACE PORIMP     WITH COMDCGE->PORIMP
      REPLACE SUBIMP     WITH COMDCGE->SUBIMP
      REPLACE TOTDOC     WITH COMDCGE->MONTO
      REPLACE VENCE      WITH COMDCGE->VENCE
      REPLACE ORIGEN     WITH "COM"
      REPLACE DEPTO      WITH WUSERUBI
      REPLACE USUARIO    WITH WUSERCODE
      REPLACE USUARIOF   WITH DATE()
      UNLOCK ALL
      FLUSH
   ELSE
      STORE "OPERACION ABORTADA, REINTENTE" TO MES
      DO AVISO WITH MES
      RETURN
   ENDIF
   RETURN
ELSE
   IF WTIPODOC="DEV"
      STORE "ACTUALIZANDO CUENTAS POR PAGAR, FAVOR ESPERAR..." TO MES
      DO MENSAJE WITH MES

      SELECT COMCXP
      SET ORDER TO COMCXP3
      SEEK COMDCGE->REFERENCIA
      SET ORDER TO COMCXP1
      IF .NOT. FOUND()
         STORE "ERROR, NO EXISTE CxP PARA RECIBO QUE GENERO DEVOLUCION. VERIFIQUE" TO MES
         DO AVISO WITH MES
         RETURN
      ELSE
         STORE CODCXP TO WCODCXP
      ENDIF

      SELECT COMCXPTR
      SET ORDER TO COMCXPT2
      SEEK COMDCGE->NUMERO
      SET ORDER TO COMCXPT1
      IF .NOT. FOUND()
          IF FILLOC()
            APPEND BLANK
            REPLACE CODCXP   WITH WCODCXP
            UNLOCK ALL
            FLUSH
          ELSE
            STORE "OPERACION ABORTADA, REINTENTE" TO MES
            DO AVISO WITH MES
            RETURN
         ENDIF
      ENDIF
      IF CODCXP<>WCODCXP
         STORE "ADVERTENCIA:REF.CRUZADA EN CXP, DEV. AFECTA A CXP DIF. A LA CXP DE SU REC." TO MES
         DO AVISO WITH MES
      ENDIF
      IF RECLOC()
         REPLACE FECHA      WITH COMDCGE->ELABORADO
         REPLACE MONTO      WITH COMDCGE->MONTO
         REPLACE REFERENCIA WITH COMDCGE->NUMERO
         REPLACE ORIGEN     WITH "COM"
         REPLACE DEPTO      WITH WUSERUBI
         REPLACE USUARIO    WITH WUSERCODE
         REPLACE USUARIOF   WITH DATE()
         UNLOCK ALL
         FLUSH
      ENDIF
   ELSE
      STORE "ERROR, DOCUMENTO NO AFECTA LAS CXP" TO MES
      DO AVISO WITH MES
   ENDIF
ENDIF
RETURN
