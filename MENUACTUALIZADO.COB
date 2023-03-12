      ******************************************************************
      * Author: CREDIMATIC S.A
      * Date:  18/02/23
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. MenuActualizado.
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
      *-----------------------
      *DECLARACION DE VARIABLES PARA MENU DE OPERACIONES.

       01  NUMERO-1                   PIC 9(4)V9(2).
       01  NUMERO-2                   PIC 9(4)V9(2).
       01  RESULTADO                  PIC 9(5)V9(2).
       01  RESIDUO                    PIC 9(2) VALUE ZEROES.
       01  WS-OPCION                  PIC 9(1).

      *ARREGLO DE DIAS DE LA SEMANA DEL 1 AL 7
       01  TABLA-SEMANA.
           05 FILLER                  PIC X(11) VALUE '1 LUNES    '.
           05 FILLER                  PIC X(11) VALUE '2 MARTES   '.
           05 FILLER                  PIC X(11) VALUE '3 MIERCOLES'.
           05 FILLER                  PIC X(11) VALUE '4 JUEVES   '.
           05 FILLER                  PIC X(11) VALUE '5 VIERNES  '.
           05 FILLER                  PIC X(11) VALUE '6 SABADO   '.
           05 FILLER                  PIC X(11) VALUE '7 DOMINGO  '.

      *REDEFINICION DE LA TABLA-SEMANA A TB-SEMANA
       01  TB-SEMANA REDEFINES TABLA-SEMANA.
           05 TB-DIAS-SEMANA OCCURS 7 TIMES INDEXED BY TB-INDICE.
             10 TB-DIA  PIC 9.
             10 FILLER PIC X.
             10 NOM-DIA PIC X(9).

      *VARIABLE QUE INDIQUE EL NUMERO DE DIA A BUSCAR
       01  WS-DIA       PIC 9.
      *VARIABLE QUE MUESTRE MENSAJE "EL DIA DIGITADO ES: "
       01  WS-MSJ-DIA   PIC X(22) VALUE 'EL DIA DIGITADO ES:'.

       PROCEDURE DIVISION.

       20000-PROCESO.
      *ESTE PARRAFO INICIA DE NUEVO EL MENU PARA DIGITAR UNA OPCION
           DISPLAY '***************** MENU **************************'
           DISPLAY '* OPCION 1: SUMA                                *'
           DISPLAY '* OPCION 2: RESTA                               *'
           DISPLAY '* OPCION 3: MULTIPLICACION                      *'
           DISPLAY '* OPCION 4: DIVISION                            *'
           DISPLAY '* OPCION 5: DIA                                 *'
           DISPLAY '* OPCION 6: SALIR                               *'
           DISPLAY '*************************************************'

           DISPLAY'- SI ELIGE LA OPCION DEL 1 AL 4 TENDRA LA POSIBILIDAD
      -' DE INGRESAR DOS NUMEROS Y REALIZAR UNA OPERACION ARITMETICA.'
           DISPLAY '- LA OPCION 5 LE MOSTRARA UN DIA DE LA SEMANA Y SI
      -'DESEA SALIR DIGITE LA OPCION 6, MUCHAS GRACIAS. '

           DISPLAY 'INGRESE UN NUMERO DE OPCION : '.
           ACCEPT WS-OPCION.


      *VALIDA QUE LA OPCION INGRESADA ESTE DENTRO DEL RANGO 1-6
           IF WS-OPCION >6 OR WS-OPCION=0
               DISPLAY SPACES
               DISPLAY 'DISCULPE, SOLO PUEDE ESCOGER OPCION DEL 1 AL 6.
      -'A CONTINUACION LE MUESTRO EL MENU NUEVAMENTE. '
               DISPLAY SPACES
               PERFORM 20000-PROCESO
           ELSE
      *SI LA OPCION ES DEL 1 AL 4 SE REALIZARA OPERACIONES ARITMETICAS
      *PIDIENDO DOS NUMEROS
             IF  WS-OPCION<5

             DISPLAY 'INGRESA NUMERO 1'
              ACCEPT NUMERO-1
             DISPLAY 'INGRESA NUMERO 2'
              ACCEPT NUMERO-2

            EVALUATE WS-OPCION
             WHEN 1
               ADD NUMERO-1 NUMERO-2 TO RESULTADO
               DISPLAY 'EL RESULTADO DE LA SUMA ES: ' RESULTADO

             WHEN 2
               SUBTRACT NUMERO-1 FROM NUMERO-2 GIVING RESULTADO
               DISPLAY 'EL RESULTADO DE LA RESTA ES: ' RESULTADO

             WHEN 3
               MULTIPLY NUMERO-1 BY NUMERO-2 GIVING RESULTADO
               DISPLAY 'EL RESULTADO DE LA MULTIPLICACION ES: 'RESULTADO

             WHEN 4
                 IF NUMERO-2= 0
                  DISPLAY 'LO SIENTO, NO SE PUEDE DIVIDIR PARA CERO'
                  PERFORM 20000-PROCESO
                 ELSE
                  DIVIDE NUMERO-1 BY NUMERO-2 GIVING RESULTADO
                                          REMAINDER RESIDUO
                 DISPLAY 'EL RESULTADO DE LA DIVISION ES : 'RESULTADO
                 DISPLAY 'RESIDUO: ' RESIDUO
              END-EVALUATE
            END-IF.

      *ESTE PARRAFO REINICIA LA OPCION DIA PARA DIGITAR NUEVO NUMERO.
       20000-DIA.
      *SI LA OPCION INGRESADA ES MAYOR A 4 NO PEDIRA DOS NUMEROS.
              IF WS-OPCION >4
            EVALUATE WS-OPCION
              WHEN 5
              DISPLAY 'INGRESE EL NUMERO DE DIA QUE DESEA VISUALIZAR '
              ACCEPT WS-DIA
              IF WS-DIA<1  OR WS-DIA>7
              DISPLAY SPACES
              DISPLAY 'LO SIENTO, DEBE SER UN NUMERO DEL 1 AL 7'
                   PERFORM 20000-DIA

              ELSE
                 SET TB-INDICE TO 1
                 SEARCH TB-DIAS-SEMANA
                   AT END
                     STOP RUN
                   WHEN WS-DIA = TB-DIA(TB-INDICE)
                    DISPLAY WS-MSJ-DIA NOM-DIA(TB-INDICE)
                  END-SEARCH
                END-IF
               PERFORM 30000-FIN
             WHEN 6
                DISPLAY 'SALISTE DEL PROGRAMA'
             END-EVALUATE
           END-IF.


       30000-FIN.

            STOP RUN.
