      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRUDLIB.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
         SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         SELECT LIBRO
         ASSIGN TO
         'C:\Users\angie\CAPACITACION\QUIELIB\DAT\LIBRO.dat'
         ORGANIZATION IS INDEXED
         ACCESS MODE IS DYNAMIC
         RECORD KEY IS REG-LIBRO-CLAVE
         FILE STATUS IS FS-LIBRO.

         SELECT AUDIOLIBRO
         ASSIGN TO
         'C:\Users\angie\CAPACITACION\QUIELIB\DAT\AUDIOLIBRO.dat'
         ORGANIZATION IS INDEXED
         ACCESS MODE IS DYNAMIC
         RECORD KEY IS REG-AUDIOLIBRO-CLAVE
         FILE STATUS IS FS-AUDIOLIBRO.

         SELECT CLIENTE
         ASSIGN TO
         'C:\Users\angie\CAPACITACION\QUIELIB\DAT\CLIENTE.dat'
         ORGANIZATION IS INDEXED
         ACCESS MODE IS DYNAMIC
         RECORD KEY IS CLTEID
         FILE STATUS IS FS-CLIENTE.

         SELECT REGISTROALQUILER
         ASSIGN TO
         'C:\Users\angie\CAPACITACION\QUIELIB\DAT\REGISTROALQUILER.dat'
         ORGANIZATION IS INDEXED
         RECORD KEY IS REG-REG-ALQUILER-CLAVE
         FILE STATUS IS FS-REGISTROAL.

         SELECT DET-REG-ALQUILER
         ASSIGN TO
         'C:\Users\angie\CAPACITACION\QUIELIB\DAT\DET-REGISTROAL.dat'
         ORGANIZATION IS INDEXED
         RECORD KEY IS REG-DET-REGISTRO-CLAVE
         FILE STATUS IS FS-DET-REGISTRO.

         SELECT PAGO
         ASSIGN TO
         'C:\Users\angie\CAPACITACION\QUIELIB\DAT\PAGO.dat'
         ORGANIZATION IS INDEXED
         RECORD KEY IS REG-PAGO
         FILE STATUS IS FS-PAGO.


       DATA DIVISION.
       FILE SECTION.
       FD LIBRO.
       COPY 'C:\Users\angie\CAPACITACION\QUIELIB\CPY\LIBRO'.

       FD AUDIOLIBRO.
       COPY 'C:\Users\angie\CAPACITACION\QUIELIB\CPY\AUDIOLIBRO'.

       FD CLIENTE.
       COPY 'C:\Users\angie\CAPACITACION\QUIELIB\CPY\CLIENTE'.

       FD REGISTROALQUILER.
       COPY 'C:\Users\angie\CAPACITACION\QUIELIB\CPY\REGISTROALQUILER'.

       FD DET-REG-ALQUILER.
       COPY 'C:\Users\angie\CAPACITACION\QUIELIB\CPY\DETREGISTROAL'.

       FD PAGO.
       COPY 'C:\Users\angie\CAPACITACION\QUIELIB\CPY\PAGO'.



       WORKING-STORAGE SECTION.

       01  FILLER PIC X(30) VALUE 'INICIO DE LA WORKING CRUDLIB'.

       01  WS-VARIABLES-WORKING.
           05  WS-FSTATUS.
               10 FS-LIBRO                PIC XX.
               10 FS-AUDIOLIBRO           PIC XX.
               10 FS-CLIENTE              PIC XX.
               10 FS-REGISTROAL           PIC XX.
               10 FS-DET-REGISTRO         PIC XX.
               10 FS-PAGO                 PIC XX.
           05 WS-CRUD.
               10 WS-OPCION               PIC 9.
               10 WS-INPUT-CLTEID         PIC 9(10).
               10 WS-ELIMINAR             PIC X.
               10 WS-ACTUALIZAR           PIC X.
               10 WS-NUEVO-NOMBRE         PIC X(30).
           05 WS-COD-ERROR                PIC 99.

           01 SWITCHES.
               05 SW-GRABAR               PIC X VALUE 'X'.
                  88 SW-SI-GRABAR               VALUE 'S'.
                  88 SW-NO-GRABAR               VALUE 'N'.
               05 SW-FIN-ARCH-LIBRO       PIC 9 VALUE 0.
                  88 SW-HAY-FIN-ARCH-LIB        VALUE 1.
                  88 SW-NO-HAY-ARCH-LIB         VALUE 0.
               05 SW-FIN-ARCH-AUDIPLIBRO  PIC 9 VALUE 0.
                  88 SW-HAY-FIN-ARCH-AULIB      VALUE 1.
                  88 SW-NO-HAY-ARCH-AULIB       VALUE 0.
               05 SW-FIN-ARCH-CLTE        PIC 9  VALUE 0.
                  88 SW-HAY-FIN-ARCH-CLTE       VALUE 1.
                  88 SW-NO-HAY-ARCH-CLTE        VALUE 0.
               05 SW-FIN-ARCH-REG-AL      PIC 9 VALUE 0.
                  88 SW-HAY-FIN-ARCH-REG        VALUE 1.
                  88 SW-NO-HAY-FIN-ARCH-REG     VALUE 0.
               05 SW-FIN-ARCH-DET-REG     PIC 9 VALUE 0.
                  88 SW-HAY-FIN-ARCH-DET        VALUE 1.
                  88 SW-NO-HAY-FIN-ARCH-DET     VALUE 0.

           01  CONSTANTES.
           05 CT-VALOR1                   PIC 9 VALUE 1.
           05 CT-VALOR0                   PIC 9 VALUE 0.
           05 CT-MSJ-ERROR.
              10 CT-ERROR-OPEN            PIC X(100) VALUE
                 'ERROR AL ABRIR ARCHIVO FS = '.
              10 CT-ERROR-WRITE           PIC X(100) VALUE
                 'ERROR AL WRITE ARCHIVO FS = '.
              10 CT-ERROR-READ            PIC X(100) VALUE
                 'ERROR AL READ ARCHIVO FS = '.
              10 CT-ERROR-REWRITE         PIC X(100) VALUE
                 'ERROR AL RE WRITE ARCHIVO FS = '.
              10 CT-ERROR-DELETE          PIC X(100) VALUE
                 'ERROR AL DELETE ARCHIVO FS = '.
              10 CT-ERROR-START           PIC X(100) VALUE
                 'ERROR AL START ARCHIVO FS = '.

       01  FILLER PIC X(30) VALUE 'FIN DE LA WORKING CRUDLIB'.


       PROCEDURE DIVISION.
       0000-COMIENZO-CRUDLIB.
           PERFORM 10000-INICIO
           PERFORM 20000-PROCESO UNTIL WS-OPCION=6
           PERFORM 30000-FIN
           .
       10000-INICIO.
           INITIALIZE WS-VARIABLES-WORKING
                      WS-FSTATUS
                      WS-CRUD
                      REPLACING NUMERIC BY ZEROES ALPHABETIC BY SPACES.

       20000-PROCESO.
           DISPLAY '***********MENU************************'.
           DISPLAY '1.- INGRESO DE REGISTROS DE ALQUILER.'
           DISPLAY '2.- CONSULTA MASIVA DE REGISTROS.'
           DISPLAY '3.- CONSULTA POR CODIGO DE REGISTRO.'
           DISPLAY '4.- MODIFICAR DATOS.'
           DISPLAY '5.- ELIMINAR REGISTROS.'
           DISPLAY '6.-SALIR'
           DISPLAY '***************************************'.
           DISPLAY SPACES
           DISPLAY 'INGRESE OPCION '
           ACCEPT WS-OPCION.
           EVALUATE WS-OPCION
           WHEN 1 PERFORM 20100-INGRESO-REGISTROS
           WHEN 2 PERFORM 20200-CONSULTA-MASIVA
           WHEN 3 PERFORM 20300-CONSULTA-IND
           WHEN 4 PERFORM 20400-MODIFICAR-REGISTROS
           WHEN 5 PERFORM 20500-ELIMINAR-REGISTROS
           WHEN 6 PERFORM 30000-FIN
           WHEN OTHER
           DISPLAY 'OPCION INVALIDA'
           END-EVALUATE.

       20100-INGRESO-REGISTROS.
           MOVE 0001 TO LIBROID.
           MOVE 'HARRY POTTER Y LA PIEDRA FILOSOFAL' TO LIBRO-TITULO.
           MOVE 'JK ROWLING ' TO LIBRO-AUTOR.
           MOVE 12052003 TO LIBRO-F-PUBLIC.
           MOVE 350 TO LIBRO-NUMPAGS.
           MOVE 'FANTASIA, MAGIA ' TO LIBRO-CATEGORIA.
           MOVE 65,00 TO LIBRO-PRECIO.
           MOVE 5 TO LIBRO-NUMREPLICAS.
           MOVE 12 TO LIBRO-RESTRICCION-EDAD.

       20200-CONSULTA-MASIVA.


       20300-CONSULTA-IND.


       20400-MODIFICAR-REGISTROS.


       20500-ELIMINAR-REGISTROS.

       30000-FIN.
           DISPLAY 'FINALIZO EL PROGRAMA'
           CLOSE LIBRO AUDIOLIBRO CLIENTE REGISTROALQUILER
           DET-REG-ALQUILER PAGO

            STOP RUN.
