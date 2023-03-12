      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. MENU01.
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
       01  FILLER               PIC X(30) VALUE 'INICIO WORKING MENU01'.
       01  WS-OPCION                   PIC 9.
       01  WS-CLIENTES.
           05 WS-DATOS-CLIENTES OCCURS 10 TIMES INDEXED BY IND-CLIENTES.
              10 WK-NOMBRE             PIC X(30).
              10 WK-APELLIDO           PIC X(30).
              10 WK-FECHA-NACIMIENTO   PIC X(11).
              10 WK-IDENTIFICACION     PIC 9(13).
              10 WK-TIPO-ID            PIC X.
              10 WK-DIRECCION          PIC X(50).
              10 WK-TELEFONO           PIC X(14).
              10 WK-NUM-FACTURA        PIC 9(6).
              10 WK-TASA-IVA           PIC 9(2).
              10 WK-TOTAL-FACTURA      PIC 9(10)V9(2).
              10 WK-COD-PRODUCTO       PIC X(10).
              10 WK-CANTIDAD           PIC 9(5).
              10 WK-PRECIO-UNITARIO    PIC 9(5)V9(2).
              10 WK-TOTAL              PIC 9(6)V9(2).
              10 WK-DESCUENTO          PIC 9(3)V9(2) .
              10 WK-IVA                PIC 9(5)V9(2).
       01  SW-CONTINUAR                PIC X VALUE 'X'.
           88 SW-SI-CONTINUA           VALUE 'S'.
           88 SW-NO-CONTINUA           VALUE 'N'.
       01  WS-IDENTIFICACION-ENTRADA   PIC 9(13).
       01  WS-COD-PRODUCTO-ENTRADA     PIC X(10).

       01  FILLER                PIC X(30) VALUE 'FIN WORKING MENU01'.

       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       COMIENZO-MENU01.
           PERFORM 10000-INICIO
           PERFORM 20000-PROCESOS
           PERFORM 30000-FINAL
           .

       10000-INICIO.
           INITIALIZE WS-OPCION
                      WS-CLIENTES
                      SW-CONTINUAR
                      WS-IDENTIFICACION-ENTRADA
                      IND-CLIENTES
                      REPLACING NUMERIC BY ZEROES ALPHANUMERIC BY SPACES
           .


       20000-PROCESOS.
           PERFORM UNTIL WS-OPCION =3

           DISPLAY '***************** MENU **************************'
           DISPLAY '*1. INGRESO DE CLIENTES                         *'
           DISPLAY '*2. BUSQUEDA DE CLIENTES                        *'
           DISPLAY '*3. SALIR                                       *'
           DISPLAY '*************************************************'

             DISPLAY 'DIGITE UNA OPCION: '
             ACCEPT WS-OPCION
             PERFORM 20100-INGRESO-OPCION
           END-PERFORM.

       20100-INGRESO-OPCION.
           EVALUATE WS-OPCION
             WHEN 1 PERFORM 20200-INGRESO-CLIENTES
             WHEN 2 PERFORM 20300-BUSQUEDA-CLIENTES
           END-EVALUATE.

       20200-INGRESO-CLIENTES.
           SET SW-SI-CONTINUA TO TRUE.
           SET IND-CLIENTES TO 1.
           INITIALIZE WS-CLIENTES.
           PERFORM 20210-INGRESO-DATOS UNTIL SW-NO-CONTINUA OR
              IND-CLIENTES>10 .

           PERFORM 20230-MOSTRAR-TABLA.

       20210-INGRESO-DATOS.
           SET IND-CLIENTES UP BY 1  .

           DISPLAY 'INGRESO CLIENTES'.
           DISPLAY 'DIGITE NOMBRE   '.
           ACCEPT   WK-NOMBRE(IND-CLIENTES).
           DISPLAY 'DIGITE APELLIDO '.
           ACCEPT   WK-APELLIDO(IND-CLIENTES).
           DISPLAY 'DIGITE TIPO IDENTIFICACION:  '.
           ACCEPT   WK-TIPO-ID(IND-CLIENTES).
           DISPLAY 'DIGITE NUMERO DE IDENTIFICACION:  '.
           ACCEPT   WK-IDENTIFICACION(IND-CLIENTES).
           DISPLAY 'DIGITE FECHA DE NACIMIENTO   '.
           ACCEPT   WK-FECHA-NACIMIENTO(IND-CLIENTES).
           DISPLAY 'DIGITE DIRECCION:  '.
           ACCEPT   WK-DIRECCION(IND-CLIENTES).
           DISPLAY 'DIGITE NUMERO DE TELEFONO   '.
           ACCEPT   WK-TELEFONO(IND-CLIENTES).
           DISPLAY 'DIGITE NUMERO DE FACTURA   '.
           ACCEPT   WK-NUM-FACTURA(IND-CLIENTES).
           DISPLAY 'INGRESE CODIGO PRODUCTO'.
           ACCEPT WK-COD-PRODUCTO(IND-CLIENTES).

           DISPLAY 'INGRESE CANTIDAD'.
           ACCEPT WK-CANTIDAD(IND-CLIENTES).

           DISPLAY 'INGRESE PRECIO'.
           ACCEPT WK-PRECIO-UNITARIO(IND-CLIENTES).

           PERFORM 20220-CALCULOS-FACTURA.


           DISPLAY 'DESEA CONTINUAR? (S/N)'.
           ACCEPT  SW-CONTINUAR.


       20220-CALCULOS-FACTURA .
       COMPUTE WK-TOTAL(IND-CLIENTES)= WK-CANTIDAD(IND-CLIENTES) *
              WK-PRECIO-UNITARIO(IND-CLIENTES).

             COMPUTE WK-DESCUENTO(IND-CLIENTES)= WK-TOTAL(IND-CLIENTES)
              * 0,10.
              COMPUTE WK-TOTAL(IND-CLIENTES)= WK-TOTAL(IND-CLIENTES) -
              WK-DESCUENTO(IND-CLIENTES).

       DISPLAY 'INGRESE EL PORCENTAJE DE TASA DE IVA (DEBE SER ENTERO)'.
           ACCEPT WK-TASA-IVA(IND-CLIENTES).
           MOVE WK-TASA-IVA(IND-CLIENTES) TO WK-IVA(IND-CLIENTES).
           COMPUTE WK-IVA(IND-CLIENTES)= WK-IVA(IND-CLIENTES)* 10/1000.

           COMPUTE WK-IVA(IND-CLIENTES)= WK-TOTAL(IND-CLIENTES)*
           WK-IVA(IND-CLIENTES).
           COMPUTE WK-TOTAL(IND-CLIENTES)=
           WK-TOTAL(IND-CLIENTES)+WK-IVA(IND-CLIENTES).

       20230-MOSTRAR-TABLA.
       DISPLAY SPACES.
       DISPLAY '********************************************'
       DISPLAY 'NOMBRE:        'WK-NOMBRE(IND-CLIENTES).
       DISPLAY 'APELLIDO:      'WK-APELLIDO(IND-CLIENTES).
       DISPLAY 'TIPO ID:       'WK-TIPO-ID(IND-CLIENTES).
       DISPLAY 'NUMERO ID:     'WK-IDENTIFICACION(IND-CLIENTES).
       DISPLAY 'FECHA NACIMIENTO:'WK-FECHA-NACIMIENTO(IND-CLIENTES).
       DISPLAY 'DIRECCION:     'WK-DIRECCION(IND-CLIENTES).
       DISPLAY 'TELEFONO:      'WK-TELEFONO(IND-CLIENTES).
       DISPLAY 'NUMERO FACTURA:'WK-NUM-FACTURA(IND-CLIENTES).
       DISPLAY 'TASA DE IVA:   'WK-TASA-IVA(IND-CLIENTES)'%'.
       DISPLAY 'CODIGO:        'WK-COD-PRODUCTO(IND-CLIENTES).
       DISPLAY 'CANTIDAD:      'WK-CANTIDAD(IND-CLIENTES).
       DISPLAY 'PRECIO:        $'WK-PRECIO-UNITARIO(IND-CLIENTES).
       DISPLAY 'DESCUENTO 10%: 'WK-DESCUENTO(IND-CLIENTES).
       DISPLAY 'IVA :          'WK-IVA(IND-CLIENTES).
       DISPLAY 'TOTAL:         'WK-TOTAL (IND-CLIENTES).
       DISPLAY '********************************************'.

       20300-BUSQUEDA-CLIENTES.
           DISPLAY 'INGRESE CEDULA '
           ACCEPT WS-IDENTIFICACION-ENTRADA
           DISPLAY 'INGRESE CODIGO DE PRODUCTO'
           ACCEPT WS-COD-PRODUCTO-ENTRADA
           SET IND-CLIENTES TO 1
           SEARCH WS-DATOS-CLIENTES
              AT END
                  DISPLAY 'NO SE ENCONTRO, DESEA BUSCAR DE NUEVO (S/N)?'
                  PERFORM 20250-FINALIZAR-BUSQUEDA

               WHEN WK-IDENTIFICACION(IND-CLIENTES)=
                  WS-IDENTIFICACION-ENTRADA
                  AND WK-COD-PRODUCTO(IND-CLIENTES)=
                  WS-COD-PRODUCTO-ENTRADA
                    DISPLAY WS-DATOS-CLIENTES(IND-CLIENTES)
                    DISPLAY 'SE ENCONTRO LA INFORMACION '
                    DISPLAY 'DESA BUSCAR DE NUEVO?'
                    PERFORM 20250-FINALIZAR-BUSQUEDA
           END-SEARCH.

       20250-FINALIZAR-BUSQUEDA.
           ACCEPT SW-CONTINUAR
                  EVALUATE  TRUE
                  WHEN  SW-SI-CONTINUA
                         PERFORM 20300-BUSQUEDA-CLIENTES
                  WHEN SW-NO-CONTINUA
                         DISPLAY 'FINALIZO LA BUSQUEDA'
                         DISPLAY SPACES
                         PERFORM 20000-PROCESOS
                  WHEN OTHER
                        DISPLAY'DEBE SER S O N'
                  END-EVALUATE.


       30000-FINAL.

           STOP RUN.
