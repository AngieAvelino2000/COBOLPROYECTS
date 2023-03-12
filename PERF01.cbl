      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. practic6.
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
       01  WS-FINALIZAR PIC X VALUE'X'.
           88 FINAL-CT        VALUE 'S'.
           88 NO-FINAL-CT     VALUE 'N'.

       01  CONTADOR.
           05  WS-A         PIC 9.
           05  WS-B         PIC 9.
           05 CT-CONTADOR  PIC 9(3).

       01   WS-OPCION-FIN    PIC X(1).


      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       0000-MAIN-PROCEDURE.
           MOVE 1 TO CT-CONTADOR.
           DISPLAY CT-CONTADOR.
           DISPLAY 'DESEA FINALIZAR EL CONTADOR(DIGITE S PARA SI O N)?'.
           ACCEPT WS-OPCION-FIN
           MOVE WS-OPCION-FIN TO WS-FINALIZAR
             IF FINAL-CT
                 DISPLAY 'TERMINO LA EJECUCION'
                 STOP RUN
             ELSE
             IF  NO-FINAL-CT
               PERFORM 10000-PARRA1
                   VARYING CT-CONTADOR FROM 5 BY 5 UNTIL CT-CONTADOR>15

               PERFORM 30000-FINAL

               END-IF

           END-IF.




       10000-PARRA1.
           DISPLAY 'ESTOY EN 10000-PARRA1'.
           DISPLAY CT-CONTADOR.



       30000-FINAL.

            STOP RUN.
