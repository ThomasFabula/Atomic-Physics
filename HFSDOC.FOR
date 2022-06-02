C
C                                                                               
C     PROGRAMM  HFSFIT  (LINEARER LEAST-SQUARES FIT FUER DIE HFS-PARAMETER EINER
C     ----------------                                                          
C     FEINSTRUKTURLINIE, D.H. A- UND B-FAKTOREN DES OBEREN UND DES UNTEREN ZU-  
C     STANDS UND LINIENSCHWERPUNKT S, AN   NM <= 40  FREQUENZMESSWERTE DER HFS- 
C     KOMPONENTEN. DIE KOEFFIZIENTEN DER PARAMETER BRAUCHEN NICHT EXPLIZIT EIN- 
C     GEGEBEN WERDEN, SONDERN WERDEN VOM PROGRAMM ANHAND DER EINGEGEBENEN QUAN- 
C     TENZAHLEN FO(J) BZW. FU(J) BERECHNET. DIE PARAMETER KOENNEN WIE BEIM      
C     PROGRAMM LSQFIT AUF KONSTANTEN WERTEN ODER RELATIV ZU ANDEREN AUF EINEM   
C     BESTIMMTEN VERHAELTNIS FESTGEHALTEN WERDEN. AUCH DIE DATENEINGABE ERFOLGT 
C     AEHNLICH WIE BEI LSQFIT. WENN ALLE HFS-KONSTANTEN FESTGEHALTEN WERDEN,    
C     WIRD KEIN FIT DURCHGEFUEHRT, SONDERN DIE BERECHNETEN KOMPONENTENFREQUENZEN
C     UND RELATIVEN INTENSITAETEN GEDRUCKT. ES DUERFEN DANN KEINE KARTEN VOM    
C     TYP 3 ODER 4  MIT MESSWERTEN EINGEGEBEN WERDEN.                           
C                                                                               
C                                                                               
C                                                                               
C     AUTOREN: S. BUETTGENBACH  UND  F. TRAEBER                                 
C                                                                               
C     OKTOBER  1982                                                             
C                                                                               
C                                                                               
C     DAS PROGRAMM BESTEHT AUS :   HAUPTPROGRAMM  (MAIN)
C                                  UNTERPROGRAMM  HFSCO  (HFS-KOEFFIZIENTENBER.)
C                                  UNTERPROGRAMM  SORT   (SORTIERPROGRAMM)      
C                                  UNTERPROGRAMM  SECHSJ (BER. VON 6J-SYMBOLEN) 
C                                  UNTERPROGRAMM  LEASTS (LEAST-SQUARES-FIT)    
C                                  FORTLIB-PROGRAMM  DMINV  (MATRIXINVERSION)   
C                                                                               
C     UM DAS HAUPTPROGRAMM UND ALLE UNTERPROGRAMME ZU LADEN, MUESSEN DIE LOAD-  
C     MODULE-DATEIEN   UNQ00D.UNQ00.HFS.LOAD ,  UNQ00D.UNQ00.SUB.LOAD   UND     
C     UNQ001.UNQ00.SUBR.LOAD   AUFGERUFEN WERDEN                                
C                                                                               
C                                                                               
C     REGION = 256K                                                             
C     TIME:  < 3 SEC , AUCH FUER MEHRERE KOMPLETTE DATENSAETZE                  
C                                                                               
C                                                                               
C     ------------------------------------------------------------------------- 
C                                                                               
C     EINGABE                                                                   
C                                                                               
C                                                                               
C     KARTE  SPALTE   FORMAT    NAME      GROESSE    KOMMENTAR                  
C     --------------------------------------------------------------------------
C     1        1-4     F4.1     XI        KERNSPIN  I
C              5-8     F4.1     XJO       HUELLENDREHIMPULS DES OBEREN ZUSTANDS 
C              9-12    F4.1     XJU       HUELLENDREHIMPULS DES UNTEREN ZUSTANDS
C             13-16     I4      NFP       ZAHL DER FESTGEHALTENEN PARAMETER     
C                                         (MUSS FUER  NM=0  NUR MIT ANGEGEBEN   
C                                          WERDEN, WENN  NFP=5 , D.H. WENN VON  
C                                          DEN KARTEN VOM TYP 2 FUER ALLE 5 PA- 
C                                          RAMETER, ALSO AUCH FUER DEN LINIEN-  
C                                          SCHWERPUNKT S, FESTE WERTE GELESEN   
C                                          WERDEN SOLLEN)                       
C                                         LEER :  ALLE PARAMETER WERDEN VARIIERT
C             17-20     I4      NM        ZAHL DER MESSWERTE                    
C                                         NM=0 :  KEIN FIT, NUR BERECHNUNG VON  
C                                         FREQUENZEN UND RELAT. INTENSITAETEN,  
C                                         ALLE HFS-KONST. WERDEN FEST EINGEGEBEN
C             21-24     I4      NMS       ZAHL DER MESSWERTSAETZE MIT GLEICHEN  
C                                         HFS-KOMPONENTEN (D.H. ZUR GLEICHEN    
C                                         KOEFFIZIENTENMATRIX)                  
C                                         MUSS NUR FUER  NMS>1  ANGEGEBEN WERDEN
C             25-80    7A8      TEXT(I)   BELIEBIGER UEBERSCHRIFTENTEXT         
C     --------------------------------------------------------------------------
C     WENN EINER ODER MEHRERE DER PARAMETER FESTGEHALTEN WERDEN SOLLEN, FOLGEN  
C     1-2 KARTEN VOM TYP 2 MIT ANGABEN UEBER WERTE ODER VERHAELTNISSE DER FESTEN
C     PARAMETER. DIE PROGRAMMINTERNE PARAMETERREIHENFOLGE IST : S (SCHWERPUNKT),
C     A(OBEN), A(UNTEN), B(OBEN), B(UNTEN); FUER DIE FESTEN PARAMETER MUESSEN   
C     ALSO ENTSPRECHENDE NUMMERN VERWENDET WERDEN. SOLLEN ALLE PARAM. VARIIERT  
C     WERDEN, ENTFAELLT KARTE 2                                                 
C     2        3-4      I2      KP1(1)    NR. DES ERSTEN FESTEN PARAMETERS      
C              5-18   F14.7     WP(1)     WERT ODER VERHAELTN. D. 1. FESTEN PAR.
C             19-20     I2      KP2(1)    NR. DES ERSETZENDEN PARAMETERS        
C                                         LEER  =    PARAM.NR. KP1(1) HAT FESTEN
C                                                    WERT  WP(1)                
C             23-24     I2      KP1(2)    NR. DES ZWEITEN FESTEN PARAMETERS     
C             25-38   F14.7     WP(2)     WERT ODER VERHAELTN. D. 2. FESTEN PAR.
C             39-40     I2      KP2(2)    LEER ODER NR. DES ERSETZENDEN PARAM.  
C             43-44     I2      KP1(3)               USW. , BIS ALLE FESTGEHAL- 
C             45-58   F14.7     WP(3)                TENEN PARAMETER (MAX. 5)   
C             59-60     I2      KP2(3)               BERUECKSICHTIGT WORDEN SIND
C             63-64     I2      KP1(4)                                          
C             65-78   F14.7     WP(4)                                           
C             79-80     I2      KP2(4)                                          
C     --------------------------------------------------------------------------
C     FUER  NFP=5  UND  NM=0  MUSS EINE WEITERE KARTE VOM TYP 2  MIT DEN WERTEN 
C     VON  KP1(5) , WP(5)  BEGONNEN WERDEN                                      
C     --------------------------------------------------------------------------
C     3        1-80    10A8     BEZ1(I)   BELIEB. UEBERSCHRIFT FUER MESSWERTSATZ
C     --------------------------------------------------------------------------
C     4        1-2      I2      NRM(J)    BELIEB.    NR. DES MESSWERTS          
C              3-20   D18.11    B(J)                 MESSWERT DER HFS-KOMPONENTE
C             21-40   D20.12    C(J)      > 0.       MESSFEHLER                 
C                                         LEER       C(J) = 1.  (FESTGESETZT)   
C             41-60   D20.12    GEW(J)    > 0.       GEWICHTSFAKTOR             
C                                         LEER       GEW(J) = 1.  (FESTGESETZT) 
C             61-66    F6.1     FO(J)     F1 (F-TERM DES OBEREN ZUSTANDS)       
C             67-72    F6.1     FU(J)     F2 (F-TERM DES UNTEREN ZUSTANDS)      
C             73-80     A8      BEZ2(J)   BELIEB. BEZEICHNUNG DER HFS-KOMPONENTE
C     --------------------------------------------------------------------------
C     FUER JEDE MESSUNG EINE KARTE 4 , AM SCHLUSS EINE NEUE KARTE VOM TYP 3 ,   
C     FALLS WEITERE MESSWERTSAETZE ZUR GLEICHEN KOEFFIZIENTENMATRIX FOLGEN,     
C     SONST NAECHSTER KOMPLETTER DATENSATZ ODER ENDKARTE. FUER  NM=0  ENTFALLEN 
C     DIE KARTEN 3 UND 4                                                        
C                                                                               
C     ES KOENNEN BELIEBIG VIELE KOMPLETTE DATENSAETZE AUFEINANDERFOLGEN         
C     --------------------------------------------------------------------------
C                                                                               
C                                                                               
C
C
C
C
