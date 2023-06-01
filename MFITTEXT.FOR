C                                                                               
C                                                                               
C     PROGRAMM  "MULTIFIT"   ZUR ANPASSUNG EINER SUPERPOSITION VON  <= 25 GAUSS-
C     --------------------   ODER LORENTZKURVEN  AN  <= 800  MESSPUNKTE         
C                                                                               
C     METHODE:  GEGEBEN SEIEN  <= 800  (FREQUENZ)MESSPUNKTE (NICHT NOTWENDIGER- 
C     WEISE AEQUIDISTANT) UND DIE ZUGEHOERIGEN ZAEHLRATEN (EVTL. MIT FEHLERN).  
C     AN DIESE MESSWERTE WIRD EINE UEBERLAGERUNG VON  NK<=25  GAUSS- ODER       
C     LORENTZKURVEN ('TEILKURVEN') ANGEPASST UND DABEI DURCH NICHTLINEAREN LEAST
C     SQUARES FIT  3*NK PARAMETER X0,HWB,A BESTIMMT. ES KANN AUCH EIN ALLEN     
C     TEILKURVEN GEMEINSAMER WERT FUER DIE HALBWERTSBREITE ANGEPASST ODER       
C     FEST VORGEGEBEN WERDEN. DIE AMPLITUDEN KOENNEN UNABHAENGIG VARIIERT ODER  
C     RELATIV ZUEINANDER AUF DEM VERHAELTNIS IHRER STARTWERTE FESTGEHALTEN WER- 
C     DEN. FERNER KOENNEN DIE ZAEHLRATEN AUF DIE KLEINSTE ZAEHLRATE ODER EINEN  
C     UNTERGRUNDWERT NORMIERT WERDEN, DER MITVARIIERT ODER FEST VORGEGEBEN WIRD.
C                                                                               
C                                                                               
C     DAS PROGRAMM ENTHAELT:                                                    
C                                                                               
C     HAUPTPROGRAMM  MAIN  (ORGANISATION)                                       
C     FUNKTIONSUNTERPROGRAMM  FWERT  (BERECHNUNG DER GAUSS/LORENTZ-FKT.WERTE)   
C     UNTERPROGRAMME   FITSUB (PART. ABLEITUNGEN DER FKT. UND LEAST-SQUARES-FIT)
C                             MIT DEN UNTER-ROUTINES   LEASTS , DMINV           
C                      PLTSUB (PLOT DER MESSWERTE UND DER ANGEPASSTEN KURVE)    
C                             MIT DEN UNTER-ROUTINES DES  SPLOT-SYSTEMS         
C                                                                               
C     AUSSERDEM WIRD AUS DER LOAD-MODULE-BIBLIOTHEK   'UNQ00D.UNQ00.SUB.LOAD'   
C     DAS SORTIERPROGRAMM   'SORT'   BENOETIGT                                  
C                                                                               
C                                                                               
C     REGION = 1024K                                                            
C                                                                               
C     AUTOREN:  S. BUETTGENBACH  UND  F. TRAEBER                                
C                                                                               
C     VERSION VOM  MAERZ  1984                                                  
C                                                                               
C                                                                               
C     EINGABE                                                                   
C     -------                                                                   
C                                                                               
C     KARTE    SPALTE  FORMAT    NAME      BEDEUTUNG                            
C     --------------------------------------------------------------------------
C     1         1-4      I4      NR        NR. DER ZU FITTENDEN KURVE           
C               5-8      I4      KFIT      =1 : NUR LORENTZFIT GEWUENSCHT       
C                                          =2 : GAUSSFIT UND LORENTZFIT         
C                                          =0 (ODER SONST. WERT): NUR GAUSSFIT  
C               9-12     I4      NI        (MAXIMALE) ITERATIONSZAHL            
C              13-16    F4.2     SKTX      MULTIPLIKATIONSFAKTOR FUER GROESSE   
C                                          DES UEBERSCHRIFTENTEXTES BEIM PLOT   
C                                          LEER: STANDARTWERT  SKTX=1. VERWENDET
C                                          <0. : BEZ. 'GAUSSKURVE' BZW. 'LORENTZ
C                                                KURVE' WIRD NICHT MITGEPLOTTET 
C              17-80    8A8      TEXT(I)   BELIEBIGER UEBERSCHRIFTENTEXT        
C     --------------------------------------------------------------------------
C     2         1-14   D14.6     AF        1. FREQUENZPUNKT (=NIEDRIGSTE FREQU.)
C                                          LEER:  FREQUENZEN SIND NICHT AEQUIDI-
C                                                 STANT UND WERDEN EINZELN AUF- 
C                                                 GEFUEHRT                      
C              15-28   D14.6     DF        FREQU.DIFFERENZ BENACHB. MESSPUNKTE  
C                                          LEER:  FREQU.PUNKTE SIND NICHT AEQUI-
C                                                 DISTANT                       
C              29-32     I4      KPLT      LEER: KEIN PLOT GEWUENSCHT           
C                                          =1 :  PLOT DER MESSWERTE U. DER KURVE
C                                          =2 :  PLOT OHNE FEHLERBALKEN (ANGABE 
C                                                NUR NOTWENDIG, WENN BEIM FIT   
C                                                ZAEHLRATENFEHLER VERWENDET)    
C                                          =3 :  PLOT OHNE UEBERSCHRIFTENTEXT   
C                                          >3 :  WIE KPLOT=3 , ABER MESSWERT- U.
C                                                KURVENDARSTELLUNG BEI GLEICHEN 
C                                                PLOTMASSEN UM KPLT VERGROESSERT
C              33-37    F5.1     SX        LAENGE DER X-ACHSE DES PLOTS (IN CM) 
C                                          LEER: STANDARTWERT  SX=25.  VERWENDET
C              38-42    F5.1     SY        LAENGE DER Y-ACHSE DES PLOTS (IN CM) 
C                                          LEER: STANDARTWERT  SY=18.  VERWENDET
C              43-44     I2      NORM      LEER: KEINE NORMIERUNG AUF UNTERGRUND
C                                          =1 :  UNTERGRUNDZAEHLRATE  ZER  WIRD 
C                                                MITVARIIERT, BEI PLOT ABGEZOGEN
C                                          =2 :  KONSTANTER UNTERGRUND ZER, WIRD
C                                                BEIM PLOT ABGEZOGEN            
C                                          =3 :  WIE NORM=1, ABER OHNE UNTER-   
C                                                GRUNDABZUG BEIM PLOT           
C                                          =4 :  WIE NORM=2, ABER OHNE UNTER-   
C                                                GRUNDABZUG BEIM PLOT           
C              45-54   D10.4     ZER       STARTWERT (NORM=1,3) ODER KONSTANTER 
C                                          WERT (NORM=2,4) FUER UNTERGRUND      
C                                          LEER: FUER  NORM>0  UNTERGRUNDNORMIE-
C                                                RUNG AUF NIEDRIGSTE ZAEHLRATE  
C              55-62    D8.2     YMAX      OBERE ZAEHLRATENGRENZE FUER DEN PLOT 
C                                          LEER: WIRD PROGRAMMINTERN BERECHNET  
C              63-64     I2      KP        LEER: ALLE HALBWERTSBREITEN VARIIERT 
C                                          =1 :  GEMEINSAME HALBWERTSBREITE FUER
C                                                ALLE TEILKURVEN, WIRD VARIIERT 
C                                          =2 :  ALLE HALBWERTSBREITEN FEST = WP
C              65-74   D10.4     WP        FUER KP=0  OHNE BEDEUTUNG            
C                                          FUER KP=1  STARTWERT FUER GEMEINSAME 
C                                          HALBWERTSBREITE ALLER TEILKURVEN     
C                                          FUER KP=2  FESTER WERT FUER HWB.     
C              75-76     I2      NSTAT     =1 :  BERECHNE MESSFEHLER AUS DEN    
C                                                WURZELN DER ZAEHLRATEN         
C                                          >1 :  VERWENDE DEN  NSTAT-FACHEN     
C                                                STATISTISCHEN FEHLER           
C                                          LEER: KEIN EFFEKT                    
C              77-78     I2      KBZ       LEER: TEILKURVENBEZEICHNUNGEN  BZ(J) 
C                                                NUR IM DRUCKOUTPUT AUFGEFUEHRT 
C                                          =1 :  TEILKURVENBEZEICHNUNGEN WERDEN 
C                                                IM PLOT MITGEZEICHNET          
C                                          =2 :  VORDERER U. HINTERER NAMENSTEIL
C                                                (JE 4 ZEICHEN) WERDEN IM PLOT  
C                                                UEBEREINANDER LINKSBUENDIG     
C                                                UEBER DEN TEILKURVEN GEZEICHNET
C              79-80     I2      KA        =1 :  AMPLITUDEN WERDEN RELATIV ZU-  
C                                                EINANDER IM VERHAELTNIS IHRER  
C                                                STARTWERTE FESTGEHALTEN        
C                                          LEER: KEIN EFFEKT                    
C     --------------------------------------------------------------------------
C     FUER JEDE ANZUPASSENDE TEILKURVE FOLGT EINE KARTE VOM TYP 3  MIT STARTWER-
C     TEN FUER DIE ZUGEHOERIGEN PARAMETER. DIE REIHENFOLGE DER KARTEN 3  IST BE-
C     LIEBIG, ES WIRD PROGRAMMINTERN NACH AUFSTEIGENDER MITTENFREQUENZ SORTIERT.
C     HINTER DIE STARTWERTEKARTE FUER DIE LETZTE TEILKURVE MUSS 1 LEERKARTE EIN-
C     GEFUEGT WERDEN, UM DAS ENDE DER STARTWERTEINGABE ANZUZEIGEN.              
C     BEI KP=0 WIRD FUER ALLE LEERGELASSENEN HALBWERTSBR.-STARTWERTE INTERN DER 
C     WERT  STS(1)  DER 1. EINGELESENEN TEILKURVE VERWENDET. ES MUSS DANN AUCH  
C     BEI UNABHAENGIGER VARIATION DER HWB. NUR EIN STARTWERT ANGEGEBEN WERDEN.  
C     3FF       1-20   D20.12    STF(J)    STARTWERT FUER LINIENMITTE DER TEILK.
C              21-40   D20.12    STS(J)    STARTWERT FUER HALBWERTSBR. D. TEILK.
C              41-60   D20.12    STA(J)    STARTWERT FUER AMPLITUDE DER TEILK.  
C              61-68     A8      BZ(J)     BELIEBIGE BEZEICHNUNG FUER TEILKURVE 
C     FALLS KARTEN MIT DEN F-TERMQUANTENZAHLEN, DEN GEFITTETEN MITTENFREQUENZEN 
C     UND DEREN FEHLERN ZUR EINGABE IN DAS PROGRAMM  'HFSFIT'  GESTANZT ODER IN 
C     EINE DATEI MIT FILE-NR. 7 GESCHRIEBEN WERDEN SOLLEN, MUESSEN AUF DEN KAR- 
C     TEN VOM TYP 3  NOCH DIE F-QUANTENZAHLEN DER UEBERGAENGE ANGEGEBEN WERDEN: 
C              69-74    F6.1     FO(J)     F-QUANTENZAHL FUER OBEREN TERM DES   
C                                          ZUR TEILKURVE GEHOERENDEN UEBERGANGS 
C              75-80    F6.1     FU(J)     F-QUANTENZAHL FUER UNTEREN TERM DES  
C                                          ZUR TEILKURVE GEHOERENDEN UEBERGANGS 
C     --------------------------------------------------------------------------
C     4FF       1-20   D20.12    WZ(I)     ZAEHLRATE DES MESSPUNKTS             
C              21-40   D20.12    FZ(I)     FEHLER DER ZAEHLRATE  (WIRD  =1.  GE-
C                                          SETZT, FALLS KEIN WERT ANGEGEBEN)    
C              41-60   D20.12    WF(I)     FREQUENZWERT DES MESSPUNKTS  (NUR BEI
C                                          NICHT AEQUIDIST. FREQU.PUNKTEN ANGE- 
C                                          BEN, WENN  AF=0.  UND  DF=0. )       
C     STATT KARTEN VOM TYP 4 KANN AUCH EINE DATEI MIT FILE-NR.11 GELESEN WERDEN.
C     DANN MUSS JE 1 KARTE 5 IM KARTENDECK U. IN DER DATEI DEN DATENSATZ BEENDEN
C     --------------------------------------------------------------------------
C     5        21-40   D20.12              1.                                   
C     --------------------------------------------------------------------------
C     EIN KOMPLETTER DATENSATZ BESTEHT AUS JE 1 KARTE VOM TYP 1 UND TYP 2 ,     
C     NK (<=25) KARTEN VOM TYP 3 , 1 LEERKARTE, UND  NM (<=800) KARTEN VOM TYP 4
C     (ALSO FUER JEDEN MESSWERT EINE). ER MUSS DURCH EINE KARTE VOM TYP 5  ABGE-
C     SCHLOSSEN WERDEN. ES KOENNEN BELIEBIG VIELE DATENSAETZE AUFEINANDERFOLGEN.
C                                                                               
C                                                                               
C                                                                               
C     AUSGABE                                                                   
C     -------                                                                   
C                                                                               
C     DRUCKOUTPUT:                                                              
C     EINGABEDATEN, BEI JEDER ITERATION KORREKTUREN UND NEUE WERTE DER PARAMETER
C     AM SCHLUSS ENDERGEBNISSE FUER DIE PARAMETER UND DEREN FEHLER; MESSWERTE,  
C     FUNKTIONSWERTE UND RESIDUALS                                              
C                                                                               
C     PLOTOUTPUT:                                                               
C     FUER  KPLT>0  WIRD VOM PROGRAMM AUSSER DEM DRUCKOUTPUT EINE PLOTZEICHNUNG 
C     DER MESSWERTE (GGF. MIT FEHLERBALKEN) UND DER ANGEPASSTEN KURVE ERZEUGT,  
C     WENN  NI ITERATIONEN DURCHGEFUEHRT SIND ODER BEREITS VORHER KONVERGENZ ER-
C     REICHT IST. FALLS  KFIT=2  IST, WERDEN GAUSS- UND LORENTZKURVE INS GLEICHE
C     PLOTDIAGRAMM GEZEICHNET.                                                  
C                                                                               
C     STANZOUTPUT:    (NUR FUER TEILK., BEI DENEN FO(J) U. FU(J) ANGEGEBEN SIND)
C     ES WERDEN EINGABEDATEN FUER DAS PROGRAMM  'HFSFIT'  AUF KARTEN GESTANZT   
C     (1 KARTE PRO TEILKURVE) : MITTENFREQUENZ MIT FEHLER, F(OBEN) , F(UNTEN) , 
C                               TEILKURVENBEZEICHNUNG                           
C                                                                               
                                                                
