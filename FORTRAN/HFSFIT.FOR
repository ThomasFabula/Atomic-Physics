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
      IMPLICIT REAL*8(A-H,O-Z)                                                  
      LOGICAL TEST,FIT                                                          
      DIMENSION A(800),X(20),B(40),C(40),CA(400),ZZ(400),LL(20),MM(20)          
      DIMENSION KP1(20),KP2(20),WP(20),NRM(40),COEF(40,5),TEXT(7)               
      DIMENSION GEW(40),KZ(20),KEP(20),P(20),PF(20),RES(40),BEZ1(10),           
     1BEZ2(40),PNM(5),PNAME(5),NUP(20),CORR(20,20),COMP(40)                     
      DIMENSION W(40),NRW(40)                                                   
      DIMENSION COA1(40),COA2(40),COB1(40),COB2(40),FO(40),FU(40)               
      DATA IP/'P'/                                                              
      DATA PNM/'S','A (OBEN)','A (UNT.)','B (OBEN)','B (UNT.)'/                 
      DATA COMP/'A','B','C','D','E','F','G','H','I','J','K','L','M','N',        
     1'O','P','Q','R','S','T','U','V','W','X','Y','Z','A1','B1','C1','D1        
     2','E1','F1','G1','H1','I1','J1','K1','L1','M1','N1'/                      
C                                                                               
C     EINGABEDATEN                                                              
C                                                                               
    1 DO 8 J=1,40                                                               
      DO 7 I=1,5                                                                
      COEF(J,I)=0.                                                              
    7 CONTINUE                                                                  
      B(J)=0.                                                                   
      C(J)=1.                                                                   
      GEW(J)=1.                                                                 
    8 CONTINUE                                                                  
      DO 9 I=1,5                                                                
      KZ(I)=0                                                                   
      KP1(I)=0                                                                  
      KP2(I)=0                                                                  
      WP(I)=0.                                                                  
      P(I)=0.                                                                   
    9 CONTINUE                                                                  
      READ(5,10,END=9000) XI,XJO,XJU,NFP,NM,NMS,(TEXT(I),I=1,7)                 
   10 FORMAT(3F4.1,3I4,7A8)                                                     
   14 IF(NMS.LE.0) NMS=1                                                        
      IE=MAX0(4,NFP)                                                            
      IF(NFP.GT.0.OR.NM.LE.0) READ(5,30) (KP1(I),WP(I),KP2(I),I=1,IE)           
   30 FORMAT(4(2X,I2,F14.7,I2))                                                 
      TEST=NM.LE.0                                                              
      FIT=NM.GT.0                                                               
      DO 8000 NRMS=1,NMS                                                        
      IF(TEST) GO TO 17                                                         
      READ(5,40) (BEZ1(I),I=1,10)                                               
   40 FORMAT(10A8)                                                              
    5 READ(5,60) (NRM(J),B(J),C(J),GEW(J),FO(J),FU(J),BEZ2(J),J=1,NM)           
   60 FORMAT(I2,D18.11,2D20.12,2F6.1,A8)                                        
      IF(NRMS.GT.1) GO TO 108                                                   
   17 IF((XI.GT.0.).AND.(DABS(XJO-XJU).LE.1.D0)) GO TO 220                      
  200 WRITE(6,210)                                                              
  210 FORMAT(1H1,'FALSCHE QUANTENZAHL FUER KERN- ODER HUELLENSPIN')             
      GO TO 1                                                                   
  220 IF((XJO.LE.0.).OR.(XJU.LE.0.)) GO TO 200                                  
      CALL HFSCO(XI,XJO,COA1,COB1,NF1,KZB1,FMAXO)                               
      CALL HFSCO(XI,XJU,COA2,COB2,NF2,KZB2,FMAXU)                               
      NP=5-KZB1-KZB2                                                            
      I=5-KZB1                                                                  
      IF(FIT) GO TO 221                                                         
C                                                                               
      NFP=NP                                                                    
      J=1                                                                       
      F2=FMAXU                                                                  
      DO 212 K=1,NF2                                                            
      F1=FMAXO                                                                  
      DO 211 L=1,NF1                                                            
      IF(DABS(F1-F2).GT.1.D0) GO TO 209                                         
      IF((F1.EQ.0.).AND.(F2.EQ.0.)) GO TO 213                                   
      CALL SECHSJ(XJO,F1,XI,F2,XJU,1.,RES1,IER)                                 
      IF(IER.NE.0) WRITE(6,207) IER                                             
  207 FORMAT(1H0,'FEHLER',I4,'  SECHSJ')                                        
      W(J)=(2.*F1+1.)*(2.*F2+1.)*RES1*RES1                                      
      NRM(J)=J                                                                  
      FO(J)=F1                                                                  
      FU(J)=F2                                                                  
      BEZ2(J)=COMP(J)                                                           
      J=J+1                                                                     
  209 F1=F1-1.D0                                                                
  211 CONTINUE                                                                  
      F2=F2-1.D0                                                                
  212 CONTINUE                                                                  
  213 NM=J-1                                                                    
C                                                                               
  221 DO 230 J=1,NM                                                             
      LO=FMAXO-FO(J)+1.1                                                        
      LU=FMAXU-FU(J)+1.1                                                        
      IF((FO(J).GT.FMAXO).OR.(FU(J).GT.FMAXU)) GO TO 222                        
      IF((LO.LE.NF1).AND.(LU.LE.NF2)) GO TO 226                                 
  222 WRITE(6,224) NRM(J)                                                       
  224 FORMAT(1H1,'FALSCHE QUANTENZAHL F BEI MESSUNG NR.',I3)                    
      GO TO 1                                                                   
  226 COEF(J,1)=1.                                                              
      COEF(J,2)=COA1(LO)                                                        
      COEF(J,3)=-COA2(LU)                                                       
      IF(NP.EQ.3) GO TO 230                                                     
      COEF(J,4)=COB1(LO)                                                        
      COEF(J,I)=-COB2(LU)                                                       
  230 CONTINUE                                                                  
      DO 240 K=1,NP                                                             
  240 PNAME(K)=PNM(K)                                                           
      PNAME(I)=PNM(5)                                                           
C                                                                               
C     AUSDRUCKEN DER EINGABEDATEN                                               
C                                                                               
      WRITE(6,100) (TEXT(I),I=1,7),NP                                           
  100 FORMAT(1H1/1H ,130(1H=)//30X,7A8/1H0,130(1H=)////' GESAMTZAHL DER         
     1PARAMETER =',I3//)                                                        
  102 WRITE(6,120)                                                              
  120 FORMAT(1H0,'KOEFFIZIENTENMATRIX'/1H ,19(1H-))                             
      WRITE(6,124)                                                              
  124 FORMAT(4H0NR.)                                                            
      WRITE(6,122) (PNAME(I),I=1,NP)                                            
  122 FORMAT(1H+, 9X,5(8X,A8,4X)/)                                              
      DO 104 J=1,NM                                                             
      WRITE(6,130) NRM(J)                                                       
  130 FORMAT(1H0,I2)                                                            
      WRITE(6,140) (COEF(J,I),I=1,NP)                                           
  140 FORMAT(1H+,T7,5F20.6)                                                     
  104 CONTINUE                                                                  
      IF(NFP.EQ.0) GO TO 99                                                     
      WRITE(6,145)                                                              
  145 FORMAT(//1H-,'FESTGEHALTENE PARAMETER'/)                                  
      DO 106 I=1,NP                                                             
      KZ(I)=0                                                                   
      KEP(I)=0                                                                  
      DO 105 K=1,NFP                                                            
      IF(KP1(K).NE.I) GO TO 105                                                 
      M=0                                                                       
      WRITE(6,150) I,PNAME(I),WP(K)                                             
  150 FORMAT(2H P,I2,5H  =  ,A8,3H  =,F14.7)                                    
      IF(KP2(K).EQ.0) GO TO 101                                                 
      WRITE(6,160) KP2(K)                                                       
  160 FORMAT(1H+,T37,1HP,I2)                                                    
      M=1                                                                       
  101 IF(KZ(I).EQ.0) GO TO 103                                                  
      WRITE(6,170)                                                              
  170 FORMAT(1H+,T35,'PARAMETER ERHAELT MEHRERE WERTE ZUGEWIESEN')              
      GO TO 3008                                                                
  103 KEP(I)=KP2(K)                                                             
      P(I)=WP(K)                                                                
      KZ(I)=M+1                                                                 
  105 CONTINUE                                                                  
  106 CONTINUE                                                                  
      IF(FIT) GO TO 99                                                          
C                                                                               
C     BERECHNUNG UND AUSDRUCK VON FREQUENZEN AUS VORGEGEBENEN HFS-KONSTANTEN    
C                                                                               
      WRITE(6,171) P(1)                                                         
  171 FORMAT(1H1,'BERECHNETE FREQUENZWERTE (SORTIERT NACH F-TERMEN) FUER        
     1 DIE HFS-KOMPONENTEN DES UEBERGANGS, LINIENSCHWERPUNKT BEI  S =',         
     2F10.3/1H ,127(1H-)//)                                                     
      WRITE(6,176)                                                              
  176 FORMAT(1H-,3HNR.,15X,5HKOMP.,16X,8HF (OBEN),10X,9HF (UNTEN),17X,          
     116HFREQUENZ (CALC.),15X,15HINTENSITAET (%)/)                              
      CALL SORT(W,NM,NRW)                                                       
      MAX=NRW(NM)                                                               
      WMAX=W(MAX)                                                               
      DO 174 J=1,NM                                                             
      DO 173 I=1,NP                                                             
  173 B(J)=B(J)+COEF(J,I)*P(I)                                                  
      W(J)=W(J)*100./WMAX                                                       
      WRITE(6,172) J,BEZ2(J),FO(J),FU(J),B(J),W(J)                              
  172 FORMAT(1H0,I2,16X,A2,18X,F4.1,14X,F4.1,20X,F15.6,19X,F5.1)                
  174 CONTINUE                                                                  
      CALL SORT(B,NM,NRM)                                                       
      WRITE(6,175)                                                              
  175 FORMAT(1H1,'BERECHNETE FREQUENZWERTE FUER DIE HFS-KOMPONENTEN  (NI        
     1EDRIGSTE FREQUENZ = 0  GESETZT)'/1H ,85(1H-)//)                           
      WRITE(6,176)                                                              
      L=NRM(1)                                                                  
      ZERO=B(L)                                                                 
      DO 178 K=1,NM                                                             
      J=NRM(K)                                                                  
      B(J)=B(J)-ZERO                                                            
      WRITE(6,172) K,BEZ2(J),FO(J),FU(J),B(J),W(J)                              
  178 CONTINUE                                                                  
      GO TO 1                                                                   
C                                                                               
   99 NV=NP-NFP                                                                 
      WRITE(6,180) NV                                                           
  180 FORMAT(1H-,'ZAHL DER VARIIERTEN PARAMETER =',I3)                          
  108 WRITE(6,185) NRMS,(BEZ1(I),I=1,10)                                        
  185 FORMAT(1H1,'MESSREIHE  NR.',I2,10X,10A8/1H ,16(1H=)//)                    
      WRITE(6,186)                                                              
  186 FORMAT(1H-,3HNR.,13X,8HMESSWERT,16X,6HFEHLER,14X,7HGEWICHT,15X,8HF        
     1 (OBEN),6X,9HF (UNTEN),7X,11HBEZEICHNUNG/)                                
      DO 112 J=1,NM                                                             
      IF(GEW(J).EQ.0.) GEW(J)=1.D0                                              
      IF(C(J).EQ.0.) C(J)=1.D0                                                  
      WRITE(6,190) NRM(J),B(J),C(J),GEW(J),FO(J),FU(J),BEZ2(J)                  
  190 FORMAT(1H0,I2,6X,F20.6,2(8X,F12.6),13X,F4.1,10X,F4.1,13X,A8)              
      C(J)=(C(J)/GEW(J))**2                                                     
  112 CONTINUE                                                                  
C                                                                               
C     ZUSAMMENSTELLUNG DES GLEICHUNGSSYSTEMS                                    
C                                                                               
      IF(NFP.EQ.0) GO TO 26                                                     
      DO 24 L=1,NFP                                                             
      I=KP1(L)                                                                  
      IF(KP2(L).EQ.0) GO TO 22                                                  
      K=KP2(L)                                                                  
      IF(KZ(K)-1) 15,19,11                                                      
   11 M=KEP(K)                                                                  
      IF(M.NE.I) GO TO 12                                                       
      KZ(I)=0                                                                   
      WRITE(6,6) I,K,P(K),M                                                     
    6 FORMAT(1H0,12HPARAMETER  P,I2,'  WIRD DURCH SICH SELBST ERSETZT. A        
     1BER NICHT MIT MIR ! ICH AKZEPTIERE NUR :'/2H P,I2,3H  =,F14.7,3H          
     2P,I2)                                                                     
      GO TO 24                                                                  
   12 DO 13 J=1,NM                                                              
      COEF(J,M)=COEF(J,M)+COEF(J,I)*P(I)*P(K)                                   
   13 CONTINUE                                                                  
      GO TO 24                                                                  
   15 DO 16 J=1,NM                                                              
      COEF(J,K)=COEF(J,K)+COEF(J,I)*P(I)                                        
   16 CONTINUE                                                                  
      GO TO 24                                                                  
   19 DO 21 J=1,NM                                                              
      B(J)=B(J)-COEF(J,I)*P(I)*P(K)                                             
   21 CONTINUE                                                                  
      GO TO 24                                                                  
   22 DO 23 J=1,NM                                                              
      B(J)=B(J)-COEF(J,I)*P(I)                                                  
   23 CONTINUE                                                                  
   24 CONTINUE                                                                  
C                                                                               
C     LEAST-SQUARES FIT                                                         
C                                                                               
   26 IF(NRMS.GT.1) GO TO 29                                                    
      K=1                                                                       
      DO 28 I=1,NP                                                              
      IF(KZ(I).NE.0) GO TO 28                                                   
      DO 27 J=1,NM                                                              
      A(K)=COEF(J,I)                                                            
      K=K+1                                                                     
   27 CONTINUE                                                                  
   28 CONTINUE                                                                  
   29 CALL LEASTS(A,C,B,NM,NV,CA,X,CHISQU,IER,ZZ,LL,MM)                         
      IF(IER.EQ.0) GO TO 3002                                                   
      IF(IER.EQ.4) WRITE(6,3004)                                                
      IF(IER.EQ.1) WRITE(6,3005)                                                
      IF(IER.EQ.2) WRITE(6,3006) I                                              
      IF(IER.EQ.3) WRITE(6,3003)                                                
 3004 FORMAT(1H1,'TROTTEL,GIB GEFAELLIGST BEI EINEM VARIATIONSPROGRAMM W        
     1ENIGSTENS EINEN PARAMETER AN,DER VARIIERT WERDEN SOLL !')                 
 3005 FORMAT(1H1,'ANZAHL DER MESSUNGEN IST KLEINER ALS ANZAHL DER VARIIE        
     1RTEN PARAMETER')                                                          
 3006 FORMAT(1H1,'FEHLER BEI MESSUNG',I3,'  IST  <= 0')                         
 3003 FORMAT(1H1,'DETERMINANTE = 0 ,DIE MESSUNGEN SIND NICHT LINEAR UNAB        
     1HAENGIG')                                                                 
 3008 IF(NRMS.EQ.NMS) GO TO 1                                                   
      GO TO 9000                                                                
C                                                                               
C     FITWERTE DER PARAMETER                                                    
C                                                                               
 3002 NFG=NM-NV                                                                 
      RMS=1.                                                                    
      IF(NFG.NE.0) RMS=DSQRT(CHISQU/NFG)                                        
      K=1                                                                       
      L=1                                                                       
      DO 34 I=1,NP                                                              
      IF(KZ(I).NE.0) GO TO 32                                                   
      P(I)=X(K)                                                                 
      PF(I)=DSQRT(CA(L))*RMS                                                    
      K=K+1                                                                     
      L=L+NV+1                                                                  
      GO TO 33                                                                  
   32 PF(I)=0.                                                                  
   33 WP(I)=P(I)                                                                
   34 CONTINUE                                                                  
C                                                                               
C     RESIDUALS                                                                 
C                                                                               
      DO 42 J=1,NM                                                              
      RES(J)=B(J)                                                               
      DO 41 I=1,NV                                                              
      K=(I-1)*NM+J                                                              
      RES(J)=RES(J)-A(K)*X(I)                                                   
   41 CONTINUE                                                                  
   42 CONTINUE                                                                  
C                                                                               
C     AUSDRUCK DER ERGEBNISSE                                                   
C                                                                               
      WRITE(6,51) NRMS                                                          
   51 FORMAT(1H1,'ERGEBNISSE DES FITS FUER MESSREIHE  NR.',I2/1H ,41(1H-        
     1)////20H WERTE DER PARAMETER,26X,10HRMS-FEHLER,45X,13HPARAMETERNAM        
     2E/1H ,130(1H-))                                                           
      DO 58 I=1,NP                                                              
      IF(KZ(I).NE.2) GO TO 54                                                   
      K=KEP(I)                                                                  
      WP(I)=P(I)*P(K)                                                           
      PF(I)=DABS(P(I))*PF(K)                                                    
      IF(KZ(K).NE.2) GO TO 54                                                   
      M=KEP(K)                                                                  
      PF(I)=DABS(WP(I))*PF(M)                                                   
      WP(I)=WP(I)*P(M)                                                          
   54 WRITE(6,55) I,WP(I),PF(I),PNAME(I)                                        
   55 FORMAT(1H0,1HP,I2,3H  =,F16.8,18X,F15.8,45X,A8)                           
      IF(KZ(I).EQ.0) WRITE(6,56)                                                
   56 FORMAT(1H+,T77,'VARIIERT')                                                
   58 CONTINUE                                                                  
C                                                                               
C     AUSDRUCK DER RESIDUALS                                                    
C                                                                               
      WRITE(6,61) CHISQU,RMS                                                    
   61 FORMAT(1H0,130(1H-)//14H CHI-QUADRAT =,D16.8,16X,'RMS-FAKTOR =',D1        
     14.6///4H-NR.,12X,15HMESSWERT (EXP.),10X,16HMESSWERT (CALC.),12X,24        
     2HEXP. - CALC.  (RESIDUAL),24X,11HBEZEICHNUNG/)                            
      DO 64 J=1,NM                                                              
      CALC=B(J)-RES(J)                                                          
      IF(DABS(CALC).LT.1.D-12) CALC=1.D-12                                      
      PROZ=RES(J)*100./CALC                                                     
      WRITE(6,62) NRM(J),B(J),CALC,RES(J),PROZ,BEZ2(J)                          
   62 FORMAT(1H0,I2,3(10X,F15.6),F18.4,7H  ( % ),14X,A8)                        
   64 CONTINUE                                                                  
C                                                                               
C     AUSDRUCK DER KOVARIANZMATRIX                                              
C                                                                               
      WRITE(6,81)                                                               
   81 FORMAT(//1H1,'KOVARIANZMATRIX'/1H ,15(1H-)///)                            
      K=1                                                                       
      DO 86 L=1,NP                                                              
      IF(KZ(L).NE.0) GO TO 86                                                   
      NUP(K)=L                                                                  
      K=K+1                                                                     
   86 CONTINUE                                                                  
      WRITE(6,87) (IP,NUP(K),K=1,NV)                                            
   87 FORMAT((1H+,4X,8(8X,A1,I2,5X))/)                                          
      K1=1                                                                      
      K2=NV                                                                     
      DO 84 J=1,NV                                                              
      WRITE(6,82) NUP(J)                                                        
      WRITE(6,83) (CA(I),I=K1,K2)                                               
   82 FORMAT(1H0,1HP,I2)                                                        
   83 FORMAT(1H+,T6,8D16.6/)                                                    
      K1=K1+NV                                                                  
      K2=K2+NV                                                                  
   84 CONTINUE                                                                  
C                                                                               
C     AUSDRUCK DER KORRELATIONSMATRIX                                           
C                                                                               
      WRITE(6,91)                                                               
   91 FORMAT(///1H-,'KORRELATIONSMATRIX'/1H ,18(1H-)///)                        
      WRITE(6,87) (IP,NUP(K),K=1,NV)                                            
      DO 94 J=1,NV                                                              
      K3=(J-1)*NV+J                                                             
      WRITE(6,82) NUP(J)                                                        
      DO 92 I=1,NV                                                              
      K4=(I-1)*NV+I                                                             
      K5=(J-1)*NV+I                                                             
      CORR(J,I)=CA(K5)/DSQRT(CA(K3)*CA(K4))                                     
   92 CONTINUE                                                                  
      WRITE(6,83) (CORR(J,I),I=1,NV)                                            
   94 CONTINUE                                                                  
C                                                                               
 8000 CONTINUE                                                                  
      GO TO 1                                                                   
 9000 STOP                                                                      
      END                                                                       
      
