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
    "! SELECT UNION
    "! https://blogs.sap.com/2015/11/09/abap-news-for-release-750-select-union/
    METHODS sel_un.
    "! Dynamischer RFC-Aufruf
    "! https://blogs.sap.com/2015/11/27/abap-news-for-release-750-dynamic-rfc-destinations/
    METHODS dynrfc.
    "! Corresponding
    "! https://blogs.sap.com/2014/02/06/abap-news-for-release-740-sp05/
    METHODS corr.
    "! Conv
    "! https://abap-workbench.de/index.php/what-s-new/abap-740/item/35-conversion-operator-conv
    METHODS conv.
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

  METHOD dynrfc.

    BREAK-POINT.

  ENDMETHOD.

  METHOD sel_un.

    BREAK-POINT.

  ENDMETHOD.

  METHOD corr.

    TYPES:
      BEGIN OF ty_flight,
        carrid   TYPE spfli-carrid,
        connid   TYPE spfli-connid,
        cityfrom TYPE spfli-cityfrom,
        cityto   TYPE spfli-cityto,
      END OF ty_flight.
    TYPES tty_flights TYPE SORTED TABLE OF ty_flight WITH UNIQUE KEY carrid connid.
    DATA lt_flights TYPE tty_flights.

    BREAK-POINT.

    SELECT *
           FROM spfli
           INTO TABLE @DATA(lt_spfli).


    MOVE-CORRESPONDING lt_spfli TO lt_flights.
    "Alternativ
    DATA(lt_flights_alt) = CORRESPONDING tty_flights( lt_spfli ).



    TYPES:
      BEGIN OF ls_1,
        matnr TYPE matnr,
        mng   TYPE i,
        flag  TYPE abap_bool,
      END OF ls_1,
      BEGIN OF ls_2,
        matnr TYPE matnr,
        menge TYPE i,
      END OF ls_2.

    DATA ls_dst TYPE ls_1.
    DATA ls_src TYPE ls_2.

    ls_src-matnr = 'ABCD'.
    ls_src-menge = 100.

    ls_dst = CORRESPONDING #( ls_src MAPPING mng = menge ).

    ls_dst = VALUE #( BASE CORRESPONDING #( ls_src MAPPING mng = menge ) flag = 'X' ).

    ls_dst = VALUE #( BASE CORRESPONDING #(
            ls_src MAPPING mng = menge )
            flag = COND #(
                    WHEN ls_src-menge > 0 THEN 'H'
                    WHEN ls_src-menge < 0 THEN 'S'
                    ELSE 'X' )  ).

  ENDMETHOD.

  METHOD conv.

    DATA lv_text TYPE c LENGTH 255.

    BREAK-POINT.

    DATA(lv_xstr) = cl_abap_codepage=>convert_to( source = CONV string( lv_text ) ).
    lv_xstr = cl_abap_codepage=>convert_to( source = CONV #( lv_text ) ).

  ENDMETHOD.

ENDCLASS.
