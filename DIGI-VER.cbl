      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. DIGI-VER.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
       WORKING-STORAGE SECTION.
      *-----------------------
       01  FILLER             PIC X(30) VALUE 'INICIO WORKING DIGI-VER'.

       01  WS-CEDULA                        PIC 9(10).
       01  WS-CEDULA2 REDEFINES WS-CEDULA.
           05 WS-DIGI-CEDULA OCCURS 10 TIMES.
              10 WK-DIG-CEDULA               PIC 9.

       01  CN-NUM2-1                        PIC x(9) VALUE '212121212'.
       01  WS-CT-NUM2-1  REDEFINES CN-NUM2-1.
           05 WS-NUM2-1 OCCURS 9 TIMES.
              10 WK-NUMERO2-1               PIC 9.

       01  WS-RESULTADO-MULTI               PIC 99.
       01  WS-RESULTADO-MULTI2   REDEFINES WS-RESULTADO-MULTI.
           05 WK-DIGI-MULTI-1               PIC 9.
           05 WK-DIGI-MULTI-2               PIC 9.

       01  WS-SUMA-TOTAL                    PIC 99.
       01  WS-SUMA   REDEFINES WS-SUMA-TOTAL.
           05 WK-DIGI1-SUMA                 PIC 9.
           05 WK-DIGI2-SUMA                 PIC 9.

       01  CONTADOR.
           05 CT-CONTADOR                   PIC 99.

       01  VARIABLES.
           05 WS-DECENA-SUPERIOR            PIC 99.
           05 WS-RESULTADO-FINAL            PIC 9.
           05 WS-OPCION           PIC 9.


       01  FILLER               PIC X(30) VALUE 'FIN WORKING DIGI-VER'.


       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       0000-COMIENZO-DIGI-VER.
           PERFORM 10000-INICIO
           PERFORM 20000-PROCESO
           PERFORM 30000-FINAL
             .


       10000-INICIO.
           INITIALIZE WS-CEDULA
                   CT-CONTADOR
                   WS-DECENA-SUPERIOR
                   WS-RESULTADO-FINAL
             .


       20000-PROCESO.
            PERFORM UNTIL WS-OPCION = 2
             DISPLAY 'ELIJA OPCION'
             ACCEPT WS-OPCION
             EVALUATE WS-OPCION
               WHEN 1
               PERFORM 20210-DIGITO-VERIFICADOR
             END-EVALUATE
            END-PERFORM.
        20210-DIGITO-VERIFICADOR.
      *HAY QUE INICIALIAR D ENUEVO LA SUMA PORQUE SINO ME SUMA EL PRIMER RESULTADO DE
      * LA BUSQUEDA DE CEDULA CON LA SEGUNTA BUSQUEDA ... ETC
           INITIALIZE WS-SUMA-TOTAL.
           DISPLAY 'INGRESE CEDULA'.
           ACCEPT WS-CEDULA.
           MOVE 1 TO CT-CONTADOR.

           PERFORM 20100-SUMA VARYING CT-CONTADOR FROM 1 BY 1
                                                  UNTIL CT-CONTADOR > 9.
           PERFORM 20200-DECENA-MAYOR.
           PERFORM 20300-VERIFICACION-CEDULA.
       20100-SUMA.

           COMPUTE WS-RESULTADO-MULTI =
           (WK-DIG-CEDULA(CT-CONTADOR) * WK-NUMERO2-1(CT-CONTADOR)).

           IF (WS-RESULTADO-MULTI>9)
              COMPUTE WS-SUMA-TOTAL =
              WS-SUMA-TOTAL + (WK-DIGI-MULTI-1 + WK-DIGI-MULTI-2)
           ELSE
              COMPUTE WS-SUMA-TOTAL = WS-SUMA-TOTAL + WS-RESULTADO-MULTI
           END-IF.


       20200-DECENA-MAYOR.


           IF (WK-DIGI2-SUMA=0)
               COMPUTE WS-DECENA-SUPERIOR= (WK-DIGI1-SUMA *10)
           ELSE
              COMPUTE WS-DECENA-SUPERIOR= ((WK-DIGI1-SUMA + 1) * 10)
           END-IF.

           COMPUTE  WS-RESULTADO-FINAL=
           WS-DECENA-SUPERIOR - WS-SUMA-TOTAL.
\         DISPLAY WS-SUMA-TOTAL.
           DISPLAY WS-DECENA-SUPERIOR.

       20300-VERIFICACION-CEDULA.
           IF (WS-RESULTADO-FINAL = WK-DIG-CEDULA(10))
               DISPLAY 'VERIFICADO. CEDULA EXISTENTE'
           ELSE
               DISPLAY 'CEDULA NO EXISTENTE.'

           END-IF.


       30000-FINAL.

            STOP RUN.
