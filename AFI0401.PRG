SELECT AFITITU
SET ORDER TO AFITITU1
STORE .T. TO WMODI
DO WHILE WMODI
   @ 4,0 CLEAR TO 9,60
   @ 5,0 TO 9,60
   @ 4,0 SAY "CAMBIO DE CEDULAS ERRADAS (TITULARES)"
   @ 6,1 SAY "CEDULA ERRADA :"
   @ 7,1 SAY "CEDULA REAL   :"
   STORE SPACE(10) TO WOLDCED
   STORE SPACE(10) TO WNEWCED
   @ 6,17 GET WOLDCED
   READ
   IF WOLDCED = SPACE(10) .OR. READKEY() = 12 .OR. READKEY() = 268
      EXIT
   ENDIF
   SELECT AFITITU
   FIND &WOLDCED
   IF EOF()
      STORE "NO REGISTRADA EN TITULALES" TO WTEXT
      DO AVISO WITH WTEXT
      LOOP
   ENDIF
   @ 6,32 SAY RTRIM(PAPELLIDO)+" "+RTRIM(SAPELLIDO)+", "+RTRIM(PNOMBRE)+" "+RTRIM(SNOMBRE)
   @ 7,17 GET WNEWCED
   READ
   IF WNEWCED = SPACE(10) .OR. READKEY() = 12 .OR. READKEY() = 268
      EXIT
   ENDIF
   SELECT AFITITU
   FIND &WNEWCED
   IF .NOT. EOF()
      STORE "YA REGISTRADA EN TITULARES COMO: "+RTRIM(PAPELLIDO)+" "+RTRIM(SAPELLIDO)+", "+RTRIM(PNOMBRE)+" "+RTRIM(SNOMBRE) TO WTEXT
      DO AVISO WITH WTEXT
      LOOP
   ENDIF
   store 1 to wop
   do while .t.
      @ 08,08  get wop pict "@*H Aceptar  ;Cancelar" defa wop
      read
      if lastkey()=13
         exit
      endif
   enddo
   IF WOP = 2
      LOOP
   ENDIF
   STORE "REEMPLAZANDO EN TITULARES.DBF..." TO WTEXT
   DO MENSAJE WITH WTEXT
   SELECT AFITITU
   FIND &WOLDCED
   IF .NOT. EOF()
      IF RECLOC()
         REPLACE CEDTITU WITH WNEWCED
         UNLOCK
      ELSE
         STORE "OPERACION ABORTADA, REINTENTE" TO WTEXT
         DO AVISO WITH WTEXT
         LOOP
      ENDIF
   ELSE
      STORE "ERROR, CEDULA VIEJA NO EXISTE EN TITULARES" TO WTEXT
      DO AVISO WITH WTEXT
      LOOP
   ENDIF
   STORE "REEMPLAZANDO EN AFILIADOS..." TO WTEXT
   DO MENSAJE WITH WTEXT
   SELECT AFIAFI
   SET ORDER TO AFIAFI1
   STORE .T. TO WREPLACEING
   DO WHILE WREPLACEING
      FIND &WOLDCED
      IF EOF()
         EXIT
      ELSE
         IF RECLOC()
            REPLACE CEDTITU WITH WNEWCED
            UNLOCK
         ENDIF
      ENDIF
   ENDDO
ENDDO
RETURN



