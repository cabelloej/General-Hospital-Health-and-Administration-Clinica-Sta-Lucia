SELECT AFITITU
SET ORDER TO AFITITU3
SAVE SCRE TO WSCRREP
DO WHILE .T.
   RESTORE SCRE FROM WSCRREP
   SHOW MENU MENUAFI
   @ 5,0 TO 13,40 DOUBLE
   @ 5,5 SAY "< DATOS LABORALES TITULARES >"
   @ 6,1 CLEAR TO 12,39
   STORE 0 TO WTOTAL
   STORE WORGA TO XORGA
   @ 07,1 say "ORGANIZACION:"
   @ 09,1 say "SALIDA (M/I):"
   @ 07,14 GET XORGA
   READ
   IF LASTKEY()=27
      SELECT AFITITU
      SET ORDER TO AFITITU1
      EXIT
   ENDIF
   IF XORGA = SPACE(4)
      STORE "TODOS" TO WORGADES
   ELSE
      STORE "  "    TO WORGADES
   ENDIF
   @ 07,20 SAY WORGADES
   store "Monitor" to wop
   do while .t.
      @ 09,15  get wop pict "@*H Monitor   ;Impresora" defa wop
      read
      if lastkey()=13
         exit
      endif
   enddo
   STORE WOP TO WSALIDA
   STORE 0   TO WPAGINA
   STORE 100 TO WLINEA
   IF ALLTRIM(WSALIDA) = "Impresora"
      STORE 48          TO WSALTO
      STORE "IMPRESORA" TO WSALIDES
      STORE "I"         TO WSALIDA
   ELSE
      STORE 20          TO WSALTO
      STORE "MONITOR"   TO WSALIDES
      STORE "M"         TO WSALIDA
   ENDIF
   @ 09,15 SAY SPACE(24)
   @ 09,15 SAY WSALIDES
   store 1 to wop
   do while .t.
      @ 11,8  get wop pict "@*H Aceptar  ;Cancelar" defa wop
      read
      if lastkey()=13
         exit
      endif
   enddo
   IF WOP = 2
      LOOP
   ENDIF
   HIDE MENU MENUAFI
   SELECT AFITITU
   SEEK XORGA
   IF .NOT. FOUND()
      STORE "NO EXISTEN AFILIADOS DE ESTA ORGANIZACION-CLIENTE" TO WTEXT
      DO AVISO WITH WTEXT
      LOOP
   ENDIF
   IF WSALIDA = "I"
      SET DEVI TO PRINT
   ELSE
      SET DEVI TO SCRE
   ENDIF
   DO WHILE .NOT.EOF()
      *** FILTROS
      IF XORGA <> SPACE(4) .AND. XORGA  <> ORGA
         SELECT AFITITU
         SKIP
         LOOP
      ENDIF
      *** FIN FILTROS
      STORE WLINEA+1 TO WLINEA
      IF WLINEA >=WSALTO
         STORE WPAGINA + 1 TO WPAGINA
         IF WSALIDA = "M"
            if WPAGINA <> 1
               STORE "OPRIMA <ENTER> PARA CONTINUAR o <ESC> PARA SALIR" TO WTEXT
               DO AVISO WITH WTEXT
               IF LASTKEY()=27
                  EXIT
               ENDIF
            endif
            @ 0,0 clear
         ENDIF
         IF WSALIDA = "M"
            @ 0,0 CLEAR TO 24,79
            @ 0,0 SAY QQWW
         ELSE
            @ 0,0 SAY CHR(14)+QQWW
         ENDIF
         @ 1,60 SAY "PAGINA:"+STR(WPAGINA,4)
         @ 2,00 SAY "LISTADO DE DATOS LABORALES DE TITULARES"
         @ 2,60 SAY "FECHA :"+DTOC(DATE())
         @ 3,00 SAY "ORGANIZACION:"+XORGA
         STORE 4 TO WLINEA
      ENDIF
      @ WLINEA+00,00 SAY  REPL("*",80)
      @ WLINEA+01,02 SAY "Cedula              :"+CEDTITU
      @ WLINEA+02,02 SAY "Nonmbre completo    :"+rtrim(papellido)+" "+rtrim(sapellido)+", "+rtrim(pnombre)+" "+rtrim(snombre)
      @ WLINEA+03,02 SAY "Organizacion-Cliente:"+ORGA
      @ WLINEA+04,02 SAY "Nivel Org. 1        :"+NIVORG1
      @ WLINEA+05,02 SAY "Nivel Org. 2        :"+NIVORG2
      @ WLINEA+06,02 SAY "Nivel Org. 3        :"+NIVORG3
      @ WLINEA+07,02 SAY "Tipo                :"+TIPLAB
      @ WLINEA+08,02 SAY "Condicion           :"+CONLAB
      @ WLINEA+09,02 SAY "Estatus             :"+ESTLAB
      @ WLINEA+10,02 SAY "Nivel Academico     :"+NIVACA
      @ WLINEA+11,02 SAY "Titulo Obtenido     :"+TITULO
      @ WLINEA+12,02 SAY "Cargo               :"+CARGO
      @ WLINEA+13,02 SAY "Sueldo              :"+STR(SUELDO,10,2)
      @ WLINEA+14,02 SAY "Telefono            :"+TELLAB
      @ WLINEA+15,02 SAY "Ingreso             :"+DTOC(INGLAB)
      @ WLINEA+16,02 SAY "Egreso              :"+DTOC(EGRLAB)
      STORE WLINEA+16 TO WLINEA
      SELECT AFITITU
      SKIP
   ENDDO
   IF WSALIDA = "M"
      STORE "OPRIMA <ENTER> PARA FINALIZAR" TO WTEXT
      DO AVISO WITH WTEXT
   ELSE
      EJECT
      SET DEVI TO SCRE
   ENDIF
ENDDO
RETURN


