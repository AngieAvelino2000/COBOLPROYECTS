      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. CARGAM.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
       WORKING-STORAGE SECTION.

       01  FILLER      PIC X(50) VALUE 'INICIO DE LA WORKING CARGAM'.

       01  WS-CLIENTES.
           05 WS-CLIENTES-DATOS OCCURS 10 TIMES INDEXED BY TB-CLIENTES.
              10 WK-CEDULA            PIC 9(10).
              10 WK-NOMBRE            PIC X(30).
              10 WK-EDAD              PIC 9(3).
              10 WK-ESTADO            PIC X.
              10 WK-MENSAJE           PIC X(50).

       01  WS-VARIABLES.
           05  WS-OPCION              PIC 9.
           05  WS-INPUT-IMPAR         PIC 9.
           05  WS-RESIDUO             PIC 9.
           05  WS-NEW-ESTADO          PIC X.
           05  WS-NEW-MSJ             PIC X(50).

       01  WS-TITULO.
           05 FILLER                  PIC X(15) VALUE 'CEDULA'.
           05 FILLER                  PIC X(28) VALUE 'NOMBRE' .
           05 FILLER                  PIC X(9)  VALUE 'EDAD' .
           05 FILLER                  PIC X(16) VALUE 'ESTADO' .
           05 FILLER                  PIC X(10) VALUE 'MENSAJE' .
       01  WS-DETALLE.
           05 WS-IMP-CEDULA           PIC 9(10).
           05 FILLER                  PIC X(5).
           05 WS-IMP-NOMBRE           PIC X(24).
           05 FILLER                  PIC X(5).
           05 WS-IMP-EDAD             PIC 99.
           05 FILLER                  PIC X(5).
           05 WS-IMP-ESTADO           PIC X(10).
           05 FILLER                  PIC X(07).
           05 WS-IMP-MENSAJE          PIC X(50).

       01  SW-COTINUAR                PIC X  VALUE 'X'.
           88 SW-COTINUAR-SI                 VALUE 'S'.
           88 SW-COTINUAR-NO                 VALUE 'N'.
       01  SW-PAR-IMPAR               PIC X  VALUE 'X'.
           88 SW-SI-IMPAR                    VALUE 'S'.
           88 SW-NO-IMPAR                    VALUE 'N'.


       01  FILLER       PIC X(50) VALUE 'FIN DE LA WORKING CARGAM'.
      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       COMIENZO-TICKET.

           PERFORM 10000-INICIO.
           PERFORM 20000-PROCESO.
           PERFORM 30000-FINAL.

       10000-INICIO.
           INITIALIZE WS-CLIENTES
                      WS-VARIABLES
                      TB-CLIENTES
                      REPLACING NUMERIC BY ZEROES ALPHABETIC BY SPACES
           .
       20000-PROCESO.
           PERFORM UNTIL WS-OPCION = 4
               DISPLAY "****** MENU ********"
               DISPLAY "1. CARGA MASIVA"
               DISPLAY "2. CONSULTA MASIVA"
               DISPLAY "3. MODIFICAR"
               DISPLAY "4. SALIR"
               DISPLAY "*********************"
               DISPLAY "DIGITE UNA OPCION: "
               ACCEPT WS-OPCION
               PERFORM 20100-INGRESO-OPCION
           END-PERFORM
           .

       20100-INGRESO-OPCION.
           EVALUATE WS-OPCION
               WHEN 1 PERFORM 20200-INGRESO-CARGA-MASIVA
               WHEN 2 PERFORM 20300-CONSULTA-DATOS
               WHEN 3 PERFORM 20500-MODIFICACION
               WHEN 4 DISPLAY "SALIENDO DEL PROGRAMA"
               WHEN OTHER DISPLAY "OPCION INCORRECTO"
           END-EVALUATE
           .

       20200-INGRESO-CARGA-MASIVA .
           DISPLAY "*****************************"
           DISPLAY "** SE REALIZA CARGA MASIVA **"
           DISPLAY "*****************************"
           MOVE 0999597992 TO WK-CEDULA(1)
           MOVE "LEONARDO" TO WK-NOMBRE(1)
           MOVE 30 TO WK-EDAD(1)
           MOVE 'CORRECTO' TO WK-ESTADO(1)
           MOVE 'SIN OBSERVACIONES' TO WK-MENSAJE(1)

           MOVE 0999597992 TO WK-CEDULA(2)
           MOVE 'CHRISTIAN' TO WK-NOMBRE(2)
           MOVE 30 TO WK-EDAD(2)
           MOVE 'CORRECTO' TO WK-ESTADO(2)
           MOVE 'SIN OBSERVACIONES' TO WK-MENSAJE(2)

           MOVE 0999597992 TO WK-CEDULA(3)
           MOVE 'DANIEL' TO WK-NOMBRE(3)
           MOVE 30 TO WK-EDAD(3)
           MOVE 'CORRECTO' TO WK-ESTADO(3)
           MOVE 'SIN OBSERVACIONES' TO WK-MENSAJE(3)

           MOVE 0991237992 TO WK-CEDULA(4)
           MOVE 'CARLOS' TO WK-NOMBRE(4)
           MOVE 25 TO WK-EDAD(4)
           MOVE 'CORRECTO' TO WK-ESTADO(4)
           MOVE 'SIN OBSERVACIONES' TO WK-MENSAJE(4)

           MOVE 0999597123 TO WK-CEDULA(5)
           MOVE 'ABDALA' TO WK-NOMBRE(5)
           MOVE 50 TO WK-EDAD(5)
           MOVE 'CORRECTO' TO WK-ESTADO(5)
           MOVE 'SIN OBSERVACIONES' TO WK-MENSAJE(5)

           MOVE 0999597992 TO WK-CEDULA(6)
           MOVE 'RAFAEL' TO WK-NOMBRE(6)
           MOVE 55 TO WK-EDAD(6)
           MOVE 'CORRECTO' TO WK-ESTADO(6)
           MOVE 'SIN OBSERVACIONES' TO WK-MENSAJE(6)

           MOVE 0999597992 TO WK-CEDULA(7)
           MOVE 'NEBOT' TO WK-NOMBRE(7)
           MOVE 65 TO WK-EDAD(7)
           MOVE 'CORRECTO' TO WK-ESTADO(7)
           MOVE 'SIN OBSERVACIONES' TO WK-MENSAJE(7)

           MOVE 0999597992 TO WK-CEDULA(8)
           MOVE 'LASSO' TO WK-NOMBRE(8)
           MOVE 75 TO WK-EDAD(8)
           MOVE 'CORRECTO' TO WK-ESTADO(8)
           MOVE 'SIN OBSERVACIONES' TO WK-MENSAJE(8)

           MOVE 0999597992 TO WK-CEDULA(9)
           MOVE 'ROLDOS' TO WK-NOMBRE(9)
           MOVE 45 TO WK-EDAD(9)
           MOVE 'CORRECTO' TO WK-ESTADO(9)
           MOVE 'SIN OBSERVACIONES' TO WK-MENSAJE(9)

           MOVE 0999597992 TO WK-CEDULA(10)
           MOVE 'LUCIO' TO WK-NOMBRE(10)
           MOVE 75 TO WK-EDAD(10)
           MOVE 'CORRECTO' TO WK-ESTADO(10)
           MOVE 'SIN OBSERVACIONES' TO WK-MENSAJE(10)
           .

       20300-CONSULTA-DATOS.
           DISPLAY WS-TITULO
           SET TB-CLIENTES TO 1
           PERFORM VARYING TB-CLIENTES FROM 1 BY 1 UNTIL
                                             (TB-CLIENTES >10)
                IF (WK-CEDULA(TB-CLIENTES)>0)
                  MOVE WK-CEDULA(TB-CLIENTES) TO WS-IMP-CEDULA
                  MOVE WK-NOMBRE(TB-CLIENTES) TO WS-IMP-NOMBRE
                  MOVE WK-EDAD(TB-CLIENTES) TO WS-IMP-EDAD
                  MOVE WK-ESTADO(TB-CLIENTES) TO WS-IMP-ESTADO
                  MOVE WK-MENSAJE(TB-CLIENTES) TO WS-IMP-MENSAJE
                  DISPLAY WS-DETALLE
               END-IF
           END-PERFORM
           .

       20500-MODIFICACION.
           SET SW-NO-IMPAR TO TRUE
           PERFORM 20600-VALIDACION-IMPAR UNTIL SW-SI-IMPAR
           .

       20600-VALIDACION-IMPAR.
           DISPLAY "DIGITE UN REGISTRO IMPAR QUE QUIERA MODIFICAR"
           ACCEPT WS-INPUT-IMPAR
           DIVIDE WS-INPUT-IMPAR BY 2 GIVING  WS-RESIDUO
           REMAINDER WS-RESIDUO
           DISPLAY WS-RESIDUO
           IF (WS-RESIDUO=0)
              DISPLAY "EL NUMERO: " WS-INPUT-IMPAR " ES PAR"
           ELSE
              DISPLAY "EL NUMERO: " WS-INPUT-IMPAR " ES IMPAR"
              PERFORM 20700-DESPLIEGUE
              DISPLAY "INGRESE EL NUEVO ESTADO,SOLO 'I' PARA INCORRECTO"
              ACCEPT WS-NEW-ESTADO
              DISPLAY "INGRESE EL MOTIVO DEL CAMBIO: "
              ACCEPT WS-NEW-MSJ
              IF (WS-NEW-ESTADO='I')
                 IF (WK-ESTADO(TB-CLIENTES)='INCORRECTO')
                    DISPLAY        'ERROR, VOLVERA AL MENU YA QUE YA SE
      -                     ' ENCONTRABA DADO DE BAJA'
                    MOVE 'N' TO SW-COTINUAR
                 ELSE
                    MOVE 'INCORRECTO' TO WK-ESTADO(TB-CLIENTES)
                    MOVE WS-NEW-MSJ TO WK-MENSAJE(TB-CLIENTES)
                    DISPLAY "DESEA CONTINUAR MODIFICANDO S/N: "
                    ACCEPT SW-COTINUAR
                END-IF
              ELSE
                 DISPLAY 'ERROR, FIN DE EJECUCION'
                 PERFORM 30000-FINAL
              END-IF

              IF (SW-COTINUAR-SI)
                  SET SW-NO-IMPAR TO TRUE
              ELSE
                  SET SW-SI-IMPAR TO TRUE
              END-IF
           END-IF
           .

       20700-DESPLIEGUE.
           DISPLAY WS-TITULO
           SET TB-CLIENTES TO 1
           SEARCH WS-CLIENTES-DATOS
                  AT END
                       DISPLAY "NO SE ENCONTRO EL REGISTRO"
                  WHEN (TB-CLIENTES = WS-INPUT-IMPAR)
                       MOVE WK-CEDULA(TB-CLIENTES) TO WS-IMP-CEDULA
                       MOVE WK-NOMBRE(TB-CLIENTES) TO WS-IMP-NOMBRE
                       MOVE WK-EDAD(TB-CLIENTES) TO WS-IMP-EDAD
                       MOVE WK-ESTADO(TB-CLIENTES) TO WS-IMP-ESTADO
                       MOVE WK-MENSAJE(TB-CLIENTES) TO WS-IMP-MENSAJE
                       DISPLAY WS-DETALLE
           END-SEARCH
           .

       30000-FINAL.
            STOP RUN.
      *
       END PROGRAM CARGAM.
