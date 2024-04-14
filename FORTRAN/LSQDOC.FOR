C
C                                                                               
C     PROGRAMM  LSQFIT  (LINEARER LEAST-SQUARES FIT  FUER  NP <= 40  PARAMETER  
C     ----------------   AN  NM <= 50  MESSWERTE )                              
C                                                                               
C                                                                               
C     AUTOREN: S. BUETTGENBACH  UND  F. TRAEBER                                 
C                                                                               
C     VERSION VOM  APRIL 1984                                                   
C                                                                               
C                                                                               
C     DAS PROGRAMM BESTEHT AUS :   HAUPTPROGRAMM  (MAIN)                        
C                                  UNTERPROGRAMM  LEASTS  (LEAST-SQUARES FIT)   
C                                  FORTLIB-PROGRAMM  DMINV  (MATRIXINVERSION)   
C                                                                               
C     REGION = 512K                                                             
C                                                                               
C                                                                               
C     METHODE :  ES KOENNEN BIS ZU  40 PARAMETER AN BIS ZU  50 MESSUNGEN        
C                ANGEPASST WERDEN. ES KOENNEN EINZELNE PARAMETER AUF EINEM      
C                KONSTANTEN WERT FESTGEHALTEN WERDEN UND/ODER IN EINEM KON-     
C                STANTEN VERHAELTNIS ZU ANDEREN PARAMETERN STEHEN               
C                                                                               
C
C     ------------------------------------------------------------------------- 
C                                                                               
C     EINGABE                                                                   
C                                                                               
C                                                                               
C     KARTE  SPALTE   FORMAT    NAME      GROESSE    KOMMENTAR                  
C     --------------------------------------------------------------------------
C     1        1-4      I4      NP        <= 40      GESAMTZAHL DER PARAMETER   
C              5-8      I4      NMS       BELIEBIG   ZAHL DER MESSWERTSAETZE    
C                                                    EINES DATENSATZES          
C                                                    (LEER  ODER  1  BEI 1 SATZ)
C              9-80    9A8      TEXT(I)   BELIEB. UEBERSCHRIFT FUER DEN OUTPUT  
C     --------------------------------------------------------------------------
C     2        1-10   A8,A2     PNAME(1)  BELIEB. NAME FUER DEN 1. PARAMETER    
C             11-20   A8,A2     PNAME(2)           "    "    "  2.     "        
C                                         USW. , GGF. BIS ZUM 8. PARAMETER      
C                                         FUER NP>8 WEITERE KARTEN VOM TYP 2    
C                                         ENTSPRECHEND BESCHRIFTEN UND ANFUEGEN 
C     --------------------------------------------------------------------------
C     3        1-2      I2      NFP       ZAHL DER FESTEN PARAMETER             
C                                         LEERKARTE: ALLE PARAM. WERDEN VARIIERT
C              3-4      I2      KP1(1)    NR. DES ERSTEN FESTEN PARAMETERS      
C              5-18   F14.7     WP(1)     WERT ODER VERHAELTN. D. 1. FESTEN PAR.
C             19-20     I2      KP2(1)    NR. DES ERSETZENDEN PARAMETERS        
C                                         LEER  =    PARAM.NR. KP1(1) HAT FESTEN
C                                                    WERT  WP(1)                
C             23-24     I2      KP1(2)    NR. DES ZWEITEN FESTEN PARAMETERS     
C             25-38   F14.7     WP(2)     WERT ODER VERHAELTN. D. 2. FESTEN PAR.
C             39-40     I2      KP2(2)    LEER ODER NR. DES ERSETZENDEN PARAM.  
C             43-44     I2      KP1(3)               USW.                       
C             45-58   F14.7     WP(3)                                           
C             59-60     I2      KP2(3)               KARTEN VOM TYP 3 SO OFT    
C             63-64     I2      KP1(4)               HINTEREINANDER, BIS ALLE   
C             65-78   F14.7     WP(4)                FESTEN PAR. 'ABGEARBEITET' 
C             79-80     I2      KP2(4)                                          
C     --------------------------------------------------------------------------
C     4        1-2                        LEER       ANDERNF. FALSCHE INTERPRET.
C              3-20   D18.11    COEF(1,1) KOEFF. DES 1. PAR. BEI 1. MESSUNG     
C             21-40   D20.13    COEF(1,2)      "     2.  "         "            
C             41-60   D20.13    COEF(1,3)      "     3.  "         "            
C             61-77   D17.10    COEF(1,4)      "     4.  "         "            
C             78-80                       BEL. KENNZIFFER DER KARTE , DIENT GGF.
C                                         ALS SORTIERHILFE, WIRD NICHT GELESEN  
C     --------------------------------------------------------------------------
C     KARTE 4  FUER 1. MESSWERT SO OFT HINTEREINANDER, BIS ALLE PARAMETER-      
C     KOEFFIZIENTEN DIESER MESSUNG ABGEARBEITET SIND, DANN NEUE KARTE BEGINNEN
C     MIT DEM KOEFF. COEF(2,1)  USW., BIS AUCH ALLE MESSUNGEN ABGEARBEITET SIND 
C     DIE KARTEN VOM TYP 4  KOENNEN AUCH VON EINER DATEI MIT FILE-NR.11 GELESEN 
C     WERDEN. FUER N KOMPLETTE DATENSAETZE MUESSEN DANN N STEUERKARTEN DER FORM 
C     //GO.FT11F00N DD DSN=DATEINAME     FUER DEN AUFRUF VON N DATEIEN IM JOB   
C     ENTHALTEN SEIN.                                                           
C     --------------------------------------------------------------------------
C     5        1-2      I2      NRMS      > 0        NR. DES MESSWERTSATZES     
C             17-80    8A8      BEZ3(I)   BELIEB.    UEBERSCHRIFT FUER DEN MESS-
C                                                    WERTSATZ (ERST AB NRMS >1 )
C     --------------------------------------------------------------------------
C     6        1-2      I2      NRM(J)    BELIEB.    NR. DER MESSUNG            
C              3-20   D18.11    B(J)                 MESSWERT                   
C             21-40   D20.12    C(J)      > 0.       MESSFEHLER                 
C                                         LEER       C(J) = 1.  (FESTGESETZT)   
C             41-60   D20.12    GEW(J)    > 0.       GEWICHTSFAKTOR             
C                                         LEER       GEW(J) = 1.  (FESTGESETZT)
C             61-76    2A8      BEZ(J)    BELIEB.    BEZEICHNUNG FUER MESSWERT
C     --------------------------------------------------------------------------
C     FUER JEDE MESSUNG EINE KARTE 6 (RICHTIGE REIHENFOLGE ENTSPRECHEND FOLGE
C     DER ZUGEHOERIGEN KOEFFIZIENTENKARTEN BEACHTEN), AM SCHLUSS EINE KARTE 5 ,
C     FALLS WEITERE MESSWERTSAETZE ZUR GLEICHEN KOEFFIZIENTENMATRIX FOLGEN,
C     SONST NAECHSTER KOMPLETTER DATENSATZ ODER ENDKARTE
C
C     ES KOENNEN BELIEBIG VIELE KOMPLETTE DATENSAETZE AUFEINANDERFOLGEN
C     --------------------------------------------------------------------------
C
C
C
