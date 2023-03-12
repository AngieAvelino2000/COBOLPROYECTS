      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. CARG-MAS.
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
       01  WS-OPCION                               PIC 9.
       01  WS-IND-IMPAR                            PIC 9.
       01  WS-REGISTROS.
           05 WS-CAMPOS  OCCURS 10 TIMES INDEXED BY IND-REGISTROS.
              10 WK-CEDULA              PIC 9(10).
              10 WK-NOMBRE              PIC X(23).
              10 WK-APELLIDO            PIC X(20).
              10 WK-EDAD                PIC 9(3).
              10 WK-ESTADO              PIC X.
              10 WK-MENSAJE             PIC X(31).

       01  WS-TITULO.
           05 FILLER                  PIC X(23) VALUE 'CEDULA     '.
           05 FILLER                  PIC X(23) VALUE 'EDAD       '.
           05 FILLER                  PIC X(23) VALUE 'ESTADO:    '.
           05 FILLER                  PIC X(23) VALUE 'MENSAJE:   '.
           05 FILLER                  PIC X(23) VALUE 'NOMBRE:    '.
           05 FILLER                  PIC X(23) VALUE 'APELLIDO:  '.
       01  WS-DETALLE.
           05 WS-IMP-CEDULA            PIC X(23).
           05 WS-IMP-EDAD              PIC X(23).
           05 WS-IMP-ESTADO            PIC X(15).
           05 WS-IMP-MENSAJE           PIC X(31).
           05 WS-IMP-NOMBRE            PIC X(23).
           05 WS-IMP-APELLIDO          PIC X(15).
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       COMIENZO-CARG-MAS.
           PERFORM 10000-INICIO
           PERFORM 20000-PROCESOS
           PERFORM 30000-FINAL
             .

       10000-INICIO.
           INITIALIZE WS-OPCION
                      WS-REGISTROS
             .

       20000-PROCESOS.
           PERFORM UNTIL WS-OPCION =4

           DISPLAY '***************** MENU **************************'
           DISPLAY '*1. CARGA MASIVA                                *'
           DISPLAY '*2. CONSULTA MASIVA                             *'
           DISPLAY '*3. MODIFICAR REGISTRO                          *'
           DISPLAY '*4. SALIR                                       *'
           DISPLAY '*************************************************'

               DISPLAY 'DIGITE UNA OPCION: '
               ACCEPT WS-OPCION
               PERFORM 20100-INGRESO-OPCION
           END-PERFORM.

       20100-INGRESO-OPCION.
           EVALUATE WS-OPCION
             WHEN 1 PERFORM 20200-CARGA-MASIVA
             WHEN 2 PERFORM 20300-CONSULTA-MASIVA
             WHEN 3 PERFORM 20500-MODIFICAR-REGISTRO
             WHEN 4 PERFORM 30000-FINAL
             WHEN OTHER
             DISPLAY 'INGRESA UNA OPCION VALIDA'
           END-EVALUATE.


       20200-CARGA-MASIVA.

           MOVE 0955393855 TO WK-CEDULA(1).
           MOVE 'ISAAC' TO WK-NOMBRE(1).
           MOVE 'VILLACRECES' TO WK-APELLIDO(1).
           MOVE 23 TO WK-EDAD(1).
           MOVE 'C' TO WK-ESTADO(1).
           INITIALIZE WK-MENSAJE(1).

           MOVE 0943812545 TO WK-CEDULA(2).
           MOVE 'ANGIE' TO WK-NOMBRE(2).
           MOVE 'AVELINO' TO WK-APELLIDO(2).
           MOVE 22 TO WK-EDAD(2).
           MOVE 'C' TO WK-ESTADO(2).
           INITIALIZE WK-MENSAJE(2).

           MOVE 0955393856 TO WK-CEDULA(3).
           MOVE 'PEPE' TO WK-NOMBRE(3).
           MOVE 'VILLACRECES' TO WK-APELLIDO(3).
           MOVE 45 TO WK-EDAD(3).
           MOVE 'C' TO WK-ESTADO(3).
           INITIALIZE WK-MENSAJE(3).

           MOVE 0955393834 TO WK-CEDULA(4).
           MOVE 'MARIA' TO WK-NOMBRE(4).
           MOVE 'DE LAS NIEVES' TO WK-APELLIDO(4).
           MOVE 67 TO WK-EDAD(4).
           MOVE 'C' TO WK-ESTADO(4).
           INITIALIZE WK-MENSAJE(4).

           MOVE 0955393822 TO WK-CEDULA(5).
           MOVE 'LUCHO' TO WK-NOMBRE(5).
           MOVE 'VALENCIA' TO WK-APELLIDO(5).
           MOVE 32 TO WK-EDAD(5).
           MOVE 'C' TO WK-ESTADO(5).
           INITIALIZE WK-MENSAJE(5).

           MOVE 0955393898 TO WK-CEDULA(6).
           MOVE 'FLOR' TO WK-NOMBRE(6).
           MOVE 'MACIAS' TO WK-APELLIDO(6).
           MOVE 54 TO WK-EDAD(6).
           MOVE 'C' TO WK-ESTADO(6).
           INITIALIZE WK-MENSAJE(6).

           MOVE 0955393235 TO WK-CEDULA(7).
           MOVE 'LUCIA' TO WK-NOMBRE(7).
           MOVE 'DE LAS CASAS' TO WK-APELLIDO(7).
           MOVE 15 TO WK-EDAD(7).
           MOVE 'C' TO WK-ESTADO(7).
           INITIALIZE WK-MENSAJE(7).

           MOVE 0953493855 TO WK-CEDULA(8).
           MOVE 'LUIS' TO WK-NOMBRE(8).
           MOVE 'MEDINA' TO WK-APELLIDO(8).
           MOVE 39 TO WK-EDAD(8).
           MOVE 'C' TO WK-ESTADO(8).
           INITIALIZE WK-MENSAJE(8).

           MOVE 0950393855 TO WK-CEDULA(9).
           MOVE 'LESLY' TO WK-NOMBRE(9).
           MOVE 'BAÑOS' TO WK-APELLIDO(9).
           MOVE 47 TO WK-EDAD(9).
           MOVE 'C' TO WK-ESTADO(9).
           INITIALIZE WK-MENSAJE(9).

           MOVE 0955323855 TO WK-CEDULA(10).
           MOVE 'JOEL' TO WK-NOMBRE(10).
           MOVE 'IGLESIAS' TO WK-APELLIDO(10).
           MOVE 78 TO WK-EDAD(10).
           MOVE 'C' TO WK-ESTADO(10).
           INITIALIZE WK-MENSAJE(10).

           DISPLAY 'SE HAN CARGADO LOS DATOS DE FORMA EXITOSA'.

           PERFORM 20000-PROCESOS.


       20300-CONSULTA-MASIVA.
           SET IND-REGISTROS TO 1.
           DISPLAY WS-TITULO.
           PERFORM VARYING IND-REGISTROS FROM 1 BY 1 UNTIL
                                              IND-REGISTROS>10

           IF WK-CEDULA(IND-REGISTROS) >  0

           MOVE WK-CEDULA(IND-REGISTROS)TO WS-IMP-CEDULA
           MOVE WK-NOMBRE(IND-REGISTROS) TO WS-IMP-NOMBRE
           MOVE WK-APELLIDO(IND-REGISTROS)
           TO WS-IMP-APELLIDO
           MOVE WK-EDAD(IND-REGISTROS)
           TO WS-IMP-EDAD
           MOVE WK-ESTADO(IND-REGISTROS) TO WS-IMP-ESTADO
           MOVE WK-MENSAJE(IND-REGISTROS) TO WS-IMP-MENSAJE
           DISPLAY WS-DETALLE

           END-IF

           END-PERFORM.


       20500-MODIFICAR-REGISTRO.
           SET IND-REGISTROS TO 1
           PERFORM UNTIL IND-REGISTROS >5
               COMPUTE WS-IND-IMPAR = 2 * (IND-REGISTROS) - 1
               DISPLAY 'SE MUESTRA EL REGISTRO NUMERO:  ' WS-IND-IMPAR
               DISPLAY WS-CAMPOS(WS-IND-IMPAR)
               DISPLAY 'INGRESE EL NUEVO ESTADO'
               ACCEPT WK-ESTADO(WS-IND-IMPAR)
              EVALUATE WK-ESTADO(WS-IND-IMPAR)
                WHEN 'I'
                 DISPLAY 'EL ESTADO SE HA GUARDADO CORRECTAMENTE'
                 MOVE 'REGISTRO MODIFICADO ' TO WK-MENSAJE(WS-IND-IMPAR)

                WHEN OTHER
                DISPLAY 'ESTADO INCORRECTO'
                PERFORM 30000-FINAL

               END-EVALUATE

               ADD 1 TO IND-REGISTROS
           END-PERFORM.
           PERFORM 20000-PROCESOS.


       30000-FINAL.


            STOP RUN.
