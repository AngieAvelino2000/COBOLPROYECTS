      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. PRACTICA2.

       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
       WORKING-STORAGE SECTION.
      * Indicador de inicio
       01  FILLER                 PIC X(6) VALUE 'INICIO'.

      * Se declaran los mensajes
       01  MEN-MENSAJES.
           05  MEN-SI-ENCONTRADO  PIC X(16) VALUE 'BÚSQUEDA EXITOSA'.
           05  MEN-NO-ENCONTRADO  PIC X(13) VALUE 'NO ENCONTRADO'.
           05  MEN-ERR-OPCION     PIC X(16) VALUE 'OPCIÓN NO VÁLIDA'.
           05  MEN-ERR-ENTRADA    PIC X(17) VALUE 'ENTRADA NO VÁLIDA'.
           05  MEN-SEGUIR         PIC X(37) VALUE 'DESEA SEGUIR INGRESAN
      -    'DO DATOS (S/N)?'.
           05  MEN-FINAL          PIC X(17) VALUE 'FIN DEL PROGRAMA.'.

      * Se declaran las variables

       01  SW-PREGUNTA            PIC X.
           88 SW-CONTINUAR        VALUE 'S'.
           88 SW-NO-CONTINUAR     VALUE 'N'.

       01  WS-OPCION              PIC 9.

       01  CN-CONTADORES.
           05 CN-CONT1            PIC 999.

      * Se declara el arreglo de N elementos, EN ESTE CASO SON MÁXIMO 7
       01  WS-ARR-DATOS.
           05  WS-ELEMENTOS OCCURS 7 TIMES INDEXED BY IND1.
               10 WS-NOMBRE                 PIC X(30).
               10 WS-APELLIDO               PIC X(30).
               10 WS-TIPO-INDENTIF          PIC X(9).
               10 WS-IDENTIF                PIC 9(13).
               10 WS-FECHA-NAC.
                  15 WS-ANIO                PIC 9(4).
                  15 FILLER                 PIC X(1) VALUE '/'.
                  15 WS-MES-EN-NUMERO       PIC 9(2).
                  15 FILLER                 PIC X(1) VALUE '/'.
                  15 WS-DIA-EN-NUMERO       PIC 9(2).
               10  WS-DIRECCION             PIC X(200).
               10  WS-FONO                  PIC 9(10).
               10  WS-NUM-FACTURA           PIC X(18).
               10  WS-TASA-IVA              PIC 9(2)V99.
               10  WS-VALOR-TOTAL-FACTURA   PIC 9(10)V99.
               10  WS-TABLA-INFO-FACTURA.
                   15  COD-PRODUCTO         PIC 9(13).
                   15  FILLER               PIC X(2) VALUE '++'.
                   15  CANTIDAD             PIC 9(4).
                   15  FILLER               PIC X(2) VALUE '++'.
                   15  PRECIO-UNIT          PIC 9(10)V99.
                   15  FILLER               PIC X(2) VALUE '++'.
                   15  VALOR-TOTAL          PIC 9(10)V99.
                   15  FILLER               PIC X(2) VALUE '++'.
                   15  DESCUENTO            PIC 9(10)V99.
                   15  FILLER               PIC X(2) VALUE '++'.
                   15  VALOR-IVA            PIC 9(10)V99.

       01  WS-CONSULTA.
           05 WS-IDENT-CONS                 PIC 9(13).
           05 WS-COD-PROD-CONS              PIC 9(13).

       01  FILLER PIC X(6) VALUE 'FINAL'.

       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       COMIENZO-PRACTICA2.

           PERFORM 10000-INICIO

           PERFORM 20000-PROCESO

           PERFORM 30000-FINAL
           .

       10000-INICIO.
           DISPLAY 'DESEA CONTINUAR AL MENÚ? (S/N)'.
           ACCEPT SW-PREGUNTA.
           EVALUATE TRUE
              WHEN SW-CONTINUAR
                 DISPLAY 'BIENVENIDO. SELECCIONE UNA OPCIÓN.'
                 PERFORM 20000-PROCESO
              WHEN SW-NO-CONTINUAR
                 DISPLAY MEN-FINAL
                 PERFORM 30000-FINAL
              WHEN OTHER
                 DISPLAY MEN-ERR-ENTRADA
                 PERFORM 10000-INICIO
           END-EVALUATE.

       20000-PROCESO.
           PERFORM 20100-MENU.

       20100-MENU.
           DISPLAY 'OPCIONES:'.
           DISPLAY '1. INGRESO DE FACTURAS'
           DISPLAY '2. CONSULTA DE FACTURAS'
           DISPLAY '3. SALIR'
           ACCEPT WS-OPCION.

           EVALUATE WS-OPCION
              WHEN 1
                 PERFORM 20200-INGRESO-FACTURAS-DATOS
                                       VARYING CN-CONT1 FROM 1 BY 1
                                    UNTIL CN-CONT1>7 OR SW-NO-CONTINUAR
              WHEN 2
                 PERFORM 20300-CONSULTA-FACTURA
              WHEN 3
                 DISPLAY MEN-FINAL
                 PERFORM 30000-FINAL
              WHEN OTHER
                 DISPLAY MEN-ERR-OPCION
                 PERFORM 20100-MENU
           END-EVALUATE.





       20200-INGRESO-FACTURAS-DATOS.
           DISPLAY 'INGRESE LOS DATOS SOLICITADOS A CONTINUACIÓN.'
           PERFORM 20201-INGRESO-NOMBRES.

       20201-INGRESO-NOMBRES.
           DISPLAY 'INGRESE SU PRIMER NOMBRE [MAYÚSCULAS]:'.
           ACCEPT WS-NOMBRE(CN-CONT1).

           DISPLAY 'INGRESE SU PRIMER APELLIDO [MAYÚSCULAS]:'.
           ACCEPT WS-APELLIDO(CN-CONT1).
           PERFORM 20202-INGRESO-TIPO-IDENT.

       20202-INGRESO-TIPO-IDENT.
           DISPLAY 'ESCOJA UNA OPCIÓN (CEDULAIDE/PASAPORTE) [MAYÚSCULAS]
      -    ':'.
           ACCEPT WS-TIPO-INDENTIF(CN-CONT1).
           EVALUATE WS-TIPO-INDENTIF(CN-CONT1)
              WHEN 'CEDULAIDE'
              WHEN 'CÉDULIDE'
                 PERFORM 20203-INGRESO-CEDULA
              WHEN 'PASAPORTE'
                 PERFORM 20204-INGRESO-PASAPORTE
              WHEN OTHER
                 DISPLAY MEN-ERR-ENTRADA
                 PERFORM 20202-INGRESO-TIPO-IDENT
           END-EVALUATE.


       20203-INGRESO-CEDULA.
           DISPLAY 'INGRESE SU NÚMERO DE CÉDULA (10 DÍGITOS):'.
           ACCEPT WS-IDENTIF(CN-CONT1).
           PERFORM 20205-INGRESO-FCHA-NAC.

       20204-INGRESO-PASAPORTE.
           DISPLAY 'INGRESE SU NÚMERO DE PASAPORTE (13 CARACTERES):'.
           ACCEPT WS-IDENTIF(CN-CONT1).
           PERFORM 20205-INGRESO-FCHA-NAC.



       20205-INGRESO-FCHA-NAC.
           DISPLAY 'INGRESE SU FECHA DE NACIMIENTO (DD/MM/AAAA):'.
           DISPLAY 'PRIMERO INGRESE EL DÍA (DD, DOS DÍGITOS):'.
           ACCEPT WS-DIA-EN-NUMERO(CN-CONT1).

           DISPLAY 'INGRESE EL MES (MM, DOS DÍGITOS):'.
           ACCEPT WS-MES-EN-NUMERO(CN-CONT1).

           DISPLAY 'INGRESE EL AÑO (AAAA, CUATRO DÍGITOS):'.
           ACCEPT WS-ANIO(CN-CONT1).
           PERFORM 20206-INGRESO-DIRECCION.

       20206-INGRESO-DIRECCION.
           DISPLAY 'INGRESE LA DIRECCIÓN DE SU DOMICILIO [MAYÚSCULAS]:'.
           ACCEPT WS-DIRECCION(CN-CONT1).
           PERFORM 20207-INGRESO-FONO.

       20207-INGRESO-FONO.
           DISPLAY 'INGRESE SU TELÉFONO (10 DÍGITOS):'.
           ACCEPT WS-FONO(CN-CONT1).
           PERFORM 20208-INGRESO-NUM-FACTURA.

       20208-INGRESO-NUM-FACTURA.
           DISPLAY 'INGRESE EL NÚMERO DE LA FACTURA (18 DÍGITOS):'.
           ACCEPT WS-NUM-FACTURA(CN-CONT1).
           PERFORM 20209-INGRESO-TASA-IVA.

       20209-INGRESO-TASA-IVA.
           DISPLAY 'INGRESE LA TASA DEL IVA A APLICAR (2 ENTEROS, 2 DECI
      -    'MALES):'.
           ACCEPT WS-TASA-IVA(CN-CONT1).
           PERFORM 20210-INGRESO-COD-PRODUCTO.

      * Se inicia el ingreso de los datos de la tabla WS-TABLA-INFO-FACTURA

       20210-INGRESO-COD-PRODUCTO.
           DISPLAY 'INGRESE EL CÓDIGO DEL PRODUCTO (13 DÍGITOS):'
           ACCEPT COD-PRODUCTO(CN-CONT1).
           PERFORM 20220-INGRESO-CANTIDAD.

       20220-INGRESO-CANTIDAD.
           DISPLAY 'INGRESE LA CANTIDAD A COMPRAR (4 DÍGITOS):'
           ACCEPT CANTIDAD(CN-CONT1).
           PERFORM 20230-INGRESO-PREC-UNIT.

       20230-INGRESO-PREC-UNIT.
           DISPLAY 'INGRESE EL PRECIO UNITARIO (HASTA 10 ENTEROS Y 2 DEC
      -    'IMALES):'
           ACCEPT PRECIO-UNIT(CN-CONT1).
           PERFORM 20240-CAL-VAL-TOTAL.

       20240-CAL-VAL-TOTAL.
           COMPUTE VALOR-TOTAL(CN-CONT1)=
                               CANTIDAD(CN-CONT1)*PRECIO-UNIT(CN-CONT1).
           PERFORM 20250-INGRESO-DESCUENTO.

       20250-INGRESO-DESCUENTO.
           DISPLAY 'INGRESE EL VALOR DEL DESCUENTO (HASTA 10 ENTEROS Y 2
      -    ' DECIMALES):'
           ACCEPT DESCUENTO(CN-CONT1).
           PERFORM 20260-CAL-VAL-IVA.

       20260-CAL-VAL-IVA.
           COMPUTE VALOR-IVA(CN-CONT1)=(VALOR-TOTAL(CN-CONT1)*
                             (WS-TASA-IVA(CN-CONT1)/100))
           COMPUTE WS-VALOR-TOTAL-FACTURA(CN-CONT1)=
              (VALOR-TOTAL(CN-CONT1))+(VALOR-IVA(CN-CONT1))-
                             (DESCUENTO(CN-CONT1))

           PERFORM 20211-VERIF-INGRESO.


       20211-VERIF-INGRESO.
           DISPLAY MEN-SEGUIR.
           ACCEPT SW-PREGUNTA.
           EVALUATE TRUE
              WHEN SW-CONTINUAR
                  CONTINUE
              WHEN SW-NO-CONTINUAR
                 INITIALIZE CN-CONT1
                 PERFORM 10000-INICIO
              WHEN OTHER
                 DISPLAY MEN-ERR-OPCION
                 PERFORM 20211-VERIF-INGRESO
           END-EVALUATE.


       20300-CONSULTA-FACTURA.
           DISPLAY 'INGRESE SU NÚMERO DE CÉDULA:'.
           ACCEPT WS-IDENT-CONS.
           DISPLAY 'INGRESE EL CÓDIGO DEL PRODUCTO:'
           ACCEPT WS-COD-PROD-CONS.

      * Comienza la búsqueda
           SET IND1 TO 1.
           SEARCH WS-ELEMENTOS
              AT END
                 DISPLAY MEN-NO-ENCONTRADO
                 PERFORM 20300-CONSULTA-FACTURA
              WHEN WS-IDENTIF(IND1)=WS-IDENT-CONS
                   AND COD-PRODUCTO(IND1)=WS-COD-PROD-CONS
                 DISPLAY MEN-SI-ENCONTRADO
                 DISPLAY 'SE PRESENTAN LOS DATOS A CONTINUACIÓN:'
                 DISPLAY WS-ELEMENTOS(IND1)
                 PERFORM 10000-INICIO
           END-SEARCH.



       30000-FINAL.
           STOP RUN.
