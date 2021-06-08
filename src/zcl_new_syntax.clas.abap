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

  ENDMETHOD.

  METHOD inline.

  BREAK-POINT.

  ENDMETHOD.

  METHOD itab.

  BREAK-POINT.

  ENDMETHOD.

  METHOD sw_co.

  BREAK-POINT.

  ENDMETHOD.

ENDCLASS.
