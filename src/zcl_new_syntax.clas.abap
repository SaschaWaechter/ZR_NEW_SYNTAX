CLASS zcl_new_syntax DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        it_method_calls TYPE stringtab.
  PROTECTED SECTION.
  PRIVATE SECTION.
    "! Meshes
    "! https://help.sap.com/doc/abapdocu_750_index_htm/7.50/de-de/abenabap_meshes.htm
    METHODS mesh.
    "! Inline-Deklarationen
    "! https://blogs.sap.com/2013/05/23/abap-news-for-release-740-inline-declarations/
    METHODS inline.
    "! SWITCH und COND
    "! https://blogs.sap.com/2013/05/28/abap-news-for-release-740-constructor-operators-cond-and-switch/
    METHODS sw_co.
    "! Tabellenausdrücke
    "! https://blogs.sap.com/2013/05/29/abap-news-for-release-740-table-expressions/
    "! https://blogs.sap.com/2013/06/22/abap-news-for-release-740-new-internal-table-functions/
    METHODS itab.
    "! For-Ausdrücke
    "! https://blogs.sap.com/2014/09/30/abap-news-for-740-sp08-iteration-expressions/
    METHODS for.
    CLASS-METHODS output
        EXPORTING
            e_par1 TYPE String
            e_par2 TYPE String
    .
ENDCLASS.



CLASS zcl_new_syntax IMPLEMENTATION.

  METHOD constructor.
    LOOP AT it_method_calls ASSIGNING FIELD-SYMBOL(<lv_method>).
      CALL METHOD (<lv_method>).
    ENDLOOP.
  ENDMETHOD.

  METHOD mesh.
    TYPES:
      t_scarr    TYPE HASHED TABLE OF scarr
                 WITH UNIQUE KEY carrid,
      t_spfli    TYPE HASHED TABLE OF spfli
                 WITH UNIQUE KEY carrid connid ,
      t_sflight  TYPE HASHED TABLE OF sflight
                 WITH UNIQUE KEY carrid connid fldate,
      t_sairport TYPE HASHED TABLE OF sairport
                 WITH UNIQUE KEY id,
      BEGIN OF MESH t_flights,
        scarr    TYPE t_scarr
          ASSOCIATION _spfli TO spfli
                   ON carrid = carrid,
        spfli    TYPE t_spfli
          ASSOCIATION _sflight TO sflight
                   ON carrid = carrid AND
                      connid = connid
          ASSOCIATION _sairport TO sairport
                   ON id = airpfrom,
        sflight  TYPE t_sflight,
        sairport TYPE t_sairport,
      END OF MESH t_flights.

    DATA:
      flights    TYPE t_flights,
      name       TYPE scarr-carrname VALUE 'Lufthansa',
      id         TYPE spfli-carrid   VALUE 'LH',
      connection TYPE spfli-connid   VALUE '0400',
      date       TYPE sflight-fldate.

    SELECT *
         FROM scarr
         INTO TABLE @flights-scarr.
    SELECT *
           FROM spfli
           INTO TABLE @flights-spfli.
    SELECT *
           FROM sflight
           INTO TABLE @flights-sflight.
    SELECT *
           FROM sairport
           INTO TABLE @flights-sairport.

    date = flights-sflight[ carrid = id
                         connid = connection ]-fldate.

    BREAK-POINT.

    "Inner Join von scarr und spfli mit den entsprechenden Bedingungen im Tabellenausdruck
    "scarr und spfli sind über eine Assoziation miteinander verbunden
    DATA(spfli_wa) =
          flights-scarr\_spfli[ flights-scarr[ carrname = name ]
                                  connid = connection ].

    "Die Assoziation wird jetzt quasi rückwärts gegangen, da scarr auf spfli zeigt
    DATA(scarr_wa) =
          flights-spfli\^_spfli~scarr[
            flights-spfli[ carrid = id connid = connection ] ].

    "Erstes Beispiel + eine Verlängerung auf das Ergebnis des Tabellenausrucks mit der Assoziation sflight
    DATA(sflight_wa) =
      flights-scarr\_spfli[ flights-scarr[ carrname = name ]
                              connid = connection
                            ]\_sflight[ fldate = date ].
    "Das obige Beispiel erweitert um einen Feldzugriff aus der Workarea
    DATA(price) =
      flights-scarr\_spfli[ flights-scarr[ carrname = name ]
                              connid = connection
                            ]\_sflight[ fldate = date ]-price.

    "Feldsymbol Zuweisung
    ASSIGN
      flights-scarr\_spfli[ flights-scarr[ carrname = name ]
                              connid = connection
                            ]\_sflight[ fldate = date ]
      TO FIELD-SYMBOL(<sflight_wa>).

    "Zuweisung eines Wertes über Assoziation bzw. MESH-Pfade
    flights-scarr\_spfli[ flights-scarr[ carrname = name ]
                            connid = connection
                          ]\_sflight[ fldate = date
                          ]-price = price - 10.

    "Prüfung ob eine Zeile innerhalb des MESHES existiert
    IF line_exists(
      flights-scarr\_spfli[ flights-scarr[ carrname = name ]
                              connid = connection
                            ]\_sflight[ fldate = date
                                          price = price - 10 ] ).
    ENDIF.

  ENDMETHOD.



  METHOD for.

  BREAK-POINT.

  "FOR zum Erstellen von internen Tabellen

  "Struktur der internen Tabelle festlegen
  TYPES: BEGIN OF struc,
         lv_quadrat TYPE i,
  END OF struc.

  "Tabellentyp festlegen
  TYPES itab_type TYPE STANDARD TABLE OF struc.

  "interne Tabelle deklarieren
  DATA itab TYPE itab_type.

  "interne Tabelle füllen --> bei VALUE ist FOR optional
  itab =
    VALUE #(
        "i startet mit dem Wert 1, wird bei jedem Schritt verdoppelt und läuft solange weiter, bis i > 150
        FOR i = 1 THEN i + i UNTIL i > 150 (
            "bei jedem Durchlauf wird itab eine Zeile in der Spalte lv_quadrat hinzugefügt und mit dem Wert von i belegt
            lv_quadrat = i
        )
    ).



  "FOR zum Reduzieren bzw. Durchiterieren von internen Tabellen

  "interne Tabelle deklarieren
  DATA itab2 TYPE STANDARD TABLE OF i.

  "interne Tabelle füllen mit den Werten 1 bis 10
  itab2 =
    VALUE #(
        FOR i = 1
            WHILE i <= 10 ( i )
    ).

  "Einträge aus zweiter Tabelle aufsummieren --> bei REDUCE ist FOR ein MUSS
  DATA(lv_sum) =
    REDUCE
        #(
            "i startet mit dem Wert 0
            INIT i = 0
            "der aktuelle Eintrag in der entsprechenden Zeile von itab2 wird zwischengespeichert in der Workarea
            FOR lv_workarea_itab
            IN itab2
            "in jedem Schritt wird i um den in der Workarea gespeicherten Wert erhöht
            NEXT i = i + lv_workarea_itab
        ).

  ENDMETHOD.



  METHOD inline.

  BREAK-POINT.

  "lokale Variablen deklarieren (wie bisher)
  DATA:
          lv_firstName TYPE c LENGTH 10,
          lv_surname TYPE String.

  "lokale Variablen initialisieren (wie bisher)
  lv_firstname = 'Peter'.
  lv_surname = 'Meier'.

  "Deklaration & Initialisierung in einem Schritt (neu) --> Datentyp & Länge werden automatisch zur Laufzeit bestimmt
  DATA(lv_firstName2) = 'Peter'.
  DATA(lv_surname2) = 'Meier'.



  "Aufbau der internen Tabelle festlegen
  TYPES: BEGIN OF itab_struc,
         lv_brand TYPE String,
         lv_operatingSystem TYPE String,
         lv_processor TYPE String,
         lv_graphicsCard TYPE String,
         lv_memorySize TYPE i,
         lv_color TYPE c LENGTH 3,
         lv_price TYPE f,
         END OF itab_struc.

  "Tabellentyp festlegen
  TYPES itab_type TYPE STANDARD TABLE OF itab_struc.

  "interne Tabelle deklarieren
  DATA itab TYPE itab_type.

  itab =
    VALUE #(
    ( lv_brand = 'Lenovo' lv_operatingSystem = 'Windows 10' lv_processor = 'Intel Core i7' lv_graphicsCard = 'andere' lv_memorySize = 512 lv_color = 'BLA' lv_price = '1045.15' )
    ( lv_brand = 'Acer' lv_operatingSystem = 'Chrome OS' lv_processor = 'Intel Celeron N400' lv_graphicsCard = 'UHD Graphics 600' lv_color = 'GRE' lv_price = '347.90' )
    ( lv_brand = 'ASUS' lv_operatingSystem = 'Windows 10 Home' lv_processor = 'Intel Core i7' lv_graphicsCard = 'UHD Graphics 630' lv_memorySize = 256 lv_color = 'BLA' lv_price = '2601.53' )
    ( lv_brand = 'Lenovo' lv_operatingSystem = 'Windows 10 Pro' lv_processor = 'Intel Core i3' lv_graphicsCard = 'andere' lv_memorySize = 512 lv_color = 'RED' lv_price = '499.00' )
    ).

  "Workarea deklarieren (wie bisher)
  DATA lv_workarea_itab TYPE itab_struc.

  "über interne Tabelle loopen mit vorhandener Workarea (wie bisher)
  LOOP AT itab INTO lv_workarea_itab.
    WRITE: / 'Loop successful'.
  ENDLOOP.

  "zweite interne Tabelle anlegen (neu)
  DATA(itab2) = itab.

  "über zweite interne Tabelle loopen (neu)
  LOOP AT itab2 INTO DATA(lv_workarea_itab2).
    WRITE: / 'Loop 2 successful'.
  ENDLOOP.

  LOOP AT itab2 ASSIGNING FIELD-SYMBOL(<lv_fieldSymbol>).
    WRITE: / 'Loop 3 successful'.
  ENDLOOP.



  "Parameter deklarieren
  DATA lv_par1 TYPE String.
  DATA lv_par2 TYPE String.

  "Methode aufrufen & zuvor deklarierte Parameter übergeben (wie bisher)
  zcl_new_syntax=>OUTPUT(
    importing
      E_PAR1 = lv_par1
      E_PAR2 = lv_par2
  ).

  WRITE: / lv_par1, lv_par2, '(gewohnter Methodenaufruf)'.

  "Methode nochmal aufrufen mit Inline-Deklaration (neu) --> NICHT BEI GENERISCHEN DATENTYPEN
  zcl_new_syntax=>OUTPUT(
    importing
      E_PAR1 = DATA(lv_param1)
      E_PAR2 = DATA(lv_param2)
  ).

  WRITE: / lv_param1, lv_param2, '(neuer Methodenaufruf)'.

  "zugehörige statische Methode output
*  CLASS-METHODS output
*        EXPORTING
*            e_par1 TYPE String
*            e_par2 TYPE String
*    .

*  Method output.
*    e_par1 = 'method call '.
*    e_par2 = 'was successful'.
*  ENDMETHOD.

  ENDMETHOD.



  Method output.
    e_par1 = 'method call '.
    e_par2 = 'was successful'.
  ENDMETHOD.



  METHOD itab.

  BREAK-POINT.

  "Aufbau der internen Tabelle festlegen
  TYPES: BEGIN OF itab_struc,
         lv_brand TYPE String,
         lv_operatingSystem TYPE String,
         lv_processor TYPE String,
         lv_graphicsCard TYPE String,
         lv_memorySize TYPE i,
         lv_color TYPE c LENGTH 3,
         lv_price TYPE f,
         END OF itab_struc.

  "Tabellentyp festlegen
  TYPES itab_type TYPE STANDARD TABLE OF itab_struc.

  "interne Tabelle deklarieren
  DATA itab TYPE itab_type.

  "interne Tabelle füllen
  itab =
    VALUE #(
    ( lv_brand = 'Lenovo' lv_operatingSystem = 'Windows 10' lv_processor = 'Intel Core i7' lv_graphicsCard = 'andere' lv_memorySize = 512 lv_color = 'BLA' lv_price = '1045.15' )
    ( lv_brand = 'Acer' lv_operatingSystem = 'Chrome OS' lv_processor = 'Intel Celeron N400' lv_graphicsCard = 'UHD Graphics 600' lv_color = 'GRE' lv_price = '347.90' )
    ( lv_brand = 'ASUS' lv_operatingSystem = 'Windows 10 Home' lv_processor = 'Intel Core i7' lv_graphicsCard = 'UHD Graphics 630' lv_memorySize = 256 lv_color = 'BLA' lv_price = '2601.53' )
    ( lv_brand = 'Lenovo' lv_operatingSystem = 'Windows 10 Pro' lv_processor = 'Intel Core i3' lv_graphicsCard = 'andere' lv_memorySize = 512 lv_color = 'RED' lv_price = '499.00' )
    ).

  "Workarea deklarieren
  DATA lv_workarea_itab TYPE itab_struc.

  "interne Tabelle an der Stelle Index = 3 lesen (wie bisher)
  READ TABLE itab INDEX 3 INTO lv_workarea_itab.

  IF sy-subrc = 0.
    DATA(lv_tab) = lv_workarea_itab.
    WRITE: / 'READ statement succeed / Index 3'.
  ENDIF.

  "interne Tabelle an der Stelle Index = 1 lesen (neu)
  lv_workarea_itab = itab[ 1 ].

  IF sy-subrc = 0.
    DATA(lv_tab2) = lv_workarea_itab.
    WRITE: / 'READ statement succeed / Index 1'.
  ENDIF.

  "Index der Zeile mit dem Laptop der Marke Acer
  DATA(lv_index) = line_index( itab[ lv_brand = 'Acer' ] ).

*  CLEAR sy-lsind.
*  sy-lsind = 0.

  ENDMETHOD.



  METHOD sw_co.

  BREAK-POINT.

  "COND --> Wert der Variable lv_resultCo ist abhängig von logischen Ausdrücken
  DATA(o_randomNumber) = cl_abap_random_int=>CREATE(
                           SEED = cl_abap_random=>seed( )
                           MIN  = 1
                           MAX  = 3
                         )->GET_NEXT( ).
  .

*  DATA lv_randomChar TYPE String.

*  CALL FUNCTION 'GENERAL_GET_RANDOM_STRING'
*    exporting
*      NUMBER_CHARS  = 1
*    importing
*      RANDOM_STRING = lv_randomChar
*    .

*    DATA(lv_resultCo) =
*        COND #(
*            WHEN lv_randomChar = 'A'
*                THEN 'lv_resultCo = A'
*            WHEN lv_randomChar = 'B'
*                THEN 'lv_resultCo = B'
*            WHEN lv_randomChar = 'C'
*                THEN 'lv_resultCo = C'
*            ELSE 'lv_resultCo = anderer'
*        ).

    DATA(lv_resultCo) =
        COND #(
            WHEN o_randomNumber = 1
                THEN 'lv_resultCo = 1'
            WHEN o_randomnumber = 2
                THEN 'lv_resultCo = 2'
            WHEN o_randomNumber = 3
                THEN 'lv_resultCo = 3'
    ).

    WRITE: / lv_resultCo.



  "SWITCH --> Fallunterscheidung
  DATA(lv_resultSw) =
    SWITCH #(
        o_randomNumber
            WHEN 1
                THEN 'lv_resultSw < 2'
            WHEN 2
                THEN 'lv_resultSw = 2'
            WHEN 3
                THEN 'lv_resultSw > 2'
    ).

*  DATA lv_resultSw2 TYPE String.
*  lv_resultSw2 =
*    SWITCH #(
*        lv_randomChar
*            WHEN 'A' OR 'E' OR 'I' OR 'O' OR 'U'
*                THEN 'Vokal'
*            ELSE 'Konsonant'
*    ).

    WRITE: / lv_resultSw.

  ENDMETHOD.

ENDCLASS.
