C
C                                                                               
C     PROGRAMM  "GAULOFIT"   ZUR ANPASSUNG EINER GAUSSKURVE ODER EINER LORENTZ- 
C     --------------------   KURVE  AN  <= 1000  MESSPUNKTE                     
C                                                                               
C                                                                               
C     METHODE:  GEGEBEN SEIEN  <=1000  (FREQUENZ)MESSPUNKTE (NICHT NOTWENDIGER- 
C     WEISE AEQUIDISTANT) UND DIE ZUGEHOERIGEN ZAEHLRATEN (EVTL. MIT FEHLERN).  
C     ES WIRD EINE GAUSSKURVE   F(X,X0,SIGMA,A)=A*EXP(-0.5*((X-X0)/SIGMA)**2)   
C     ODER EINE  LORENTZKURVE   F(X,X0, HWB ,A)=A/(1+(2*(X-X0)/HWB)**2)         
C     AN DIE MESSWERTE ANGEPASST UND DABEI DURCH NICHTLINEAREN LEAST-SQUARES-FIT
C     DIE PARAMETER  X0, HWB, A  BESTIMMT. ES KANN AUCH EINER DER 3 PARAMETER   
C     AUF EINEM EINGEGEBENEN WERT FESTGEHALTEN WERDEN. FERNER KOENNEN DIE ZAEHL-
C     RATEN AUF DIE KLEINSTE ZAEHLRATE ODER EINEN EINGEGEBENEN UNTERGRUNDWERT   
C     NORMIERT WERDEN.                                                          
C                                                                               
C                                                                               
C     DAS PROGRAMM ENTHAELT:                                                    
C     HAUPTPROGRAMM  MAIN  (ORGANISATION)                                       
C     FUNKTIONSUNTERPROGRAMM  FWERT  (BERECHNUNG DER GAUSS/LORENTZ-FKT.WERTE)   
C     UNTERPROGRAMME   FSTART  (STARTWERT FUER LINIENMITTE)                     
C                      SIGST   (    "      "   HALBWERTSBREITE)                 
C                      DERIV   (PART. ABLEITUNGEN DER FUNKTION UND RESIDUALS)   
C                      LSQ     (LEAST-SQUARES-FIT)  MIT DEN UNTER-SUBROUTINES
C                                                   LEASTS                      
C                                                   DMINV  (FORTLIB)            
C                                                                               
C                                                                               
C     REGION = 256 K                                                            
C                                                                               
C                                                                               
C     AUTOREN:  S. BUETTGENBACH  UND  F. TRAEBER                                
C                                                                               
C     APRIL  1983                                                               
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
C              17-80    8A8      TEXT(I)   BELIEBIGER UEBERSCHRIFTENTEXT
C     --------------------------------------------------------------------------
C     2         1-14   D14.6     AF        1. FREQUENZPUNKT (=NIEDRIGSTE FREQU.)
C                                          LEER:  FREQUENZEN SIND NICHT AEQUIDI-
C                                                 STANT UND WERDEN EINZELN AUF- 
C                                                 GEFUEHRT                      
C              15-28   D14.6     DF        FREQU.DIFFERENZ BENACHB. MESSPUNKTE  
C                                          LEER:  FREQU.PUNKTE SIND NICHT AEQUI-
C                                                 DISTANT                       
C              43-44     I2      NORM      =1 :  NORMIERE ALLE ZAEHLRATEN       
C                                          LEER:  KEINE NORMIERUNG              
C              45-54   D14.6     ZER       NORMIERUNG AUF  Z' = Z - ZER         
C                                          LEER:  FUER NORM=0 KEINE NORMIERUNG  
C                                                 FUER NORM=1 NORMIERUNG AUF DIE
C                                                 NIEDRIGSTE ZAEHLRATE          
C              59-60     I2      KP        KENNZIFFER FUER FESTEN PARAMETER:    
C                                          LEER= ALLE 3 PARAM. VARIIERT         
C                                          1 = MITTENFREQUENZ FEST              
C                                          2 = HALBWERTSBREITE FEST             
C                                          3 = AMPLITUDE FEST                   
C              61-74   D14.6     WP        WERT DES FESTEN PARAM. (FALLS KP>0)  
C              75-76     I2      NSTAT     =1 :  BERECHNE MESSFEHLER AUS DEN    
C                                                WURZELN DER ZAEHLRATEN         
C                                          >1 :  VERWENDE DEN  NSTAT-FACHEN
C                                                STATISTISCHEN FEHLER
C                                          LEER: KEIN EFFEKT
C     --------------------------------------------------------------------------
C     3FF       1-20   D20.12    WZ(I)     ZAEHLRATE DES MESSPUNKTS
C              21-40   D20.12    FZ(I)     FEHLER DER ZAEHLRATE  (WIRD  =1.  GE-
C                                          SETZT, FALLS KEIN WERT ANGEGEBEN)
C              41-60   D20.12    WF(I)     FREQUENZWERT DES MESSPUNKTS  (NUR BEI
C                                          NICHT AEQUIDIST. FREQU.PUNKTEN ANGE-
C                                          BEN, WENN  AF=0.  UND  DF=0. )
C     --------------------------------------------------------------------------
C     4        21-40   D20.12              1.
C     --------------------------------------------------------------------------
C     EIN KOMPLETTER DATENSATZ BESTEHT AUS JE 1 KARTE VOM TYP 1 UND TYP 2 UND
C     NM KARTEN VOM TYP 3 (ALSO FUER JEDEN MESSWERT EINE). ER MUSS DURCH EINE
C     KARTE VOM TYP 4  ABGESCHLOSSEN WERDEN. ES KOENNEN BELIEBIG VIELE KOMPLETTE
C     DATENSAETZE AUFEINANDERFOLGEN.
C
C
