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
      IMPLICIT REAL*8(A-H,O-Z)                                                  
      DIMENSION A(2000),X(40),B(50),C(50),CA(1600),ZZ(1600),LL(40),             
     1MM(40)                                                                    
      DIMENSION KP1(40),KP2(40),WP(40),NRM(50),COEF(50,40),TEXT(9)              
      DIMENSION GEW(50),KZ(40),KEP(40),P(40),PF(40),RES(50),BEZ1(50),           
     1BEZ2(50),BEZ3(8),PNAME(40),NAM(40),NUP(40),CORR(40,40)                    
      DATA IP/'P'/                                                              
C                                                                               
C     EINGABEDATEN                                                              
C                                                                               
    1 DO 8 J=1,50                                                               
      DO 7 I=1,40                                                               
      COEF(J,I)=0.                                                              
    7 CONTINUE                                                                  
      B(J)=0.                                                                   
      C(J)=1.                                                                   
      GEW(J)=1.                                                                 
    8 CONTINUE                                                                  
      DO 9 I=1,40                                                               
      KZ(I)=0                                                                   
      KP1(I)=0                                                                  
      KP2(I)=0                                                                  
      WP(I)=0.                                                                  
      P(I)=0.                                                                   
    9 CONTINUE                                                                  
      READ(5,10,END=9000) NP,NMS,(TEXT(I),I=1,9)                                
   10 FORMAT(2I4,9A8)                                                           
      READ(5,2) (PNAME(I),NAM(I),I=1,NP)                                        
    2 FORMAT(8(A8,A2))                                                          
      READ(5,20) NFP,(KP1(I),WP(I),KP2(I),I=1,4)                                
   20 FORMAT(2I2,F14.7,I2,3(2X,I2,F14.7,I2))                                    
      J=0                                                                       
      NM=0                                                                      
      IF(NFP.LE.4) GO TO 3                                                      
      READ(5,30) (KP1(I),WP(I),KP2(I),I=5,NFP)                                  
   30 FORMAT(4(2X,I2,F14.7,I2))                                                 
    3 J=J+1                                                                     
      READ(5,40) NRMS,(COEF(J,I),I=1,4)                                         
   40 FORMAT(I2,D18.11,2D20.13,D17.10)                                          
      IF(NRMS.NE.0) GO TO 52                                                    
      NM=NM+1                                                                   
      IF(NP.LE.4) GO TO 3                                                       
      READ(5,50) (COEF(J,I),I=5,NP)                                             
   50 FORMAT(3D20.13,D17.10)                                                    
      GO TO 3                                                                   
   52 IF(NM.GT.0) GO TO 5                                                       
   53 READ(11,50,END=5) (COEF(J,I),I=1,NP)                                      
      NM=NM+1                                                                   
      J=J+1                                                                     
      GO TO 53                                                                  
    5 READ(5,60) (NRM(J),B(J),C(J),GEW(J),BEZ1(J),BEZ2(J),J=1,NM)               
   60 FORMAT(I2,D18.11,2D20.12,2A8)                                             
      IF(NRMS.GT.1) GO TO 108                                                   
C                                                                               
C     AUSDRUCKEN DER EINGABEDATEN                                               
C                                                                               
      WRITE(6,100) (TEXT(I),I=1,9),NP                                           
  100 FORMAT(1H1/1H ,130(1H=)//30X,9A8/1H0,130(1H=)////' GESAMTZAHL DER         
     1PARAMETER =',I3//)                                                        
      IF(NP.LE.0) GO TO 1                                                       
      IF(NM.GT.0) GO TO 102                                                     
      WRITE(6,110) NM                                                           
  110 FORMAT(1H0,'ZAHL DER MESSWERTE =',I3)                                     
      GO TO 1                                                                   
  102 WRITE(6,120)                                                              
  120 FORMAT(1H-,'KOEFFIZIENTENMATRIX'/1H ,19(1H-))                             
      WRITE(6,124)                                                              
  124 FORMAT(4H0NR.)                                                            
      WRITE(6,122) (PNAME(I),NAM(I),I=1,NP)                                     
  122 FORMAT((1H+,5X,6(8X,A8,A2,3X))/)                                          
      DO 104 J=1,NM                                                             
      WRITE(6,130) NRM(J)                                                       
  130 FORMAT(1H0,I2)                                                            
      WRITE(6,140) (COEF(J,I),I=1,NP)                                           
  140 FORMAT(1H+,T7,6D21.10/)                                                   
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
      WRITE(6,150) I,WP(K)                                                      
  150 FORMAT(2H P,I2,3H  =,F14.7)                                               
      IF(KP2(K).EQ.0) GO TO 101                                                 
      WRITE(6,160) KP2(K)                                                       
  160 FORMAT(1H+,T24,1HP,I2)                                                    
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
   99 NV=NP-NFP                                                                 
      WRITE(6,180) NV                                                           
  180 FORMAT(1H-,'ZAHL DER VARIIERTEN PARAMETER =',I3)                          
      WRITE(6,185)                                                              
  185 FORMAT(1H1,'MESSREIHE  NR. 1'/1H ,16(1H=)//)                              
  108 WRITE(6,186)                                                              
  186 FORMAT(1H-,3HNR.,11X,8HMESSWERT,21X,6HFEHLER,15X,7HGEWICHT,16X,11H        
     1BEZEICHNUNG/)                                                             
      DO 112 J=1,NM                                                             
      IF(GEW(J).EQ.0.) GEW(J)=1.D0                                              
      IF(C(J).EQ.0.) C(J)=1.D0                                                  
      WRITE(6,190) NRM(J),B(J),C(J),GEW(J),BEZ1(J),BEZ2(J)                      
  190 FORMAT(1H0,I2,6X,D25.12,2(9X,D12.5),12X,2A8)                              
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
 3008 IF(NMS.LE.NRMS) GO TO 1                                                   
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
     1)////20H WERTE DER PARAMETER,28X,10HRMS-FEHLER,45X,13HPARAMETERNAM        
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
   54 WRITE(6,55) I,WP(I),PF(I),PNAME(I),NAM(I)                                 
   55 FORMAT(1H0,1HP,I2,3H  =,D21.12,19X,D13.6,43X,A8,A2)                       
      IF(KZ(I).EQ.0) WRITE(6,56)                                                
   56 FORMAT(1H+,T80,'VARIIERT')                                                
   58 CONTINUE                                                                  
C                                                                               
C     AUSDRUCK DER RESIDUALS                                                    
C                                                                               
      WRITE(6,61) CHISQU,RMS                                                    
   61 FORMAT(1H0,130(1H-)//14H CHI-QUADRAT =,D16.8,18X,'RMS-FAKTOR =',D1        
     14.6////4H0NR.,9X,15HMESSWERT (EXP.),12X,16HMESSWERT (CALC.),13X,24        
     2HEXP. - CALC.  (RESIDUAL)/)                                               
      DO 64 J=1,NM                                                              
      CALC=B(J)-RES(J)                                                          
      IF(DABS(CALC).LT.1.D-12) CALC=1.D-12                                      
      PROZ=RES(J)*100./CALC                                                     
      WRITE(6,62) NRM(J),B(J),CALC,RES(J),PROZ                                  
   62 FORMAT(1H0,I2,2D27.10,D27.8,F17.4,7H  ( % ))                              
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
C     FIT FUER NEUEN MESSWERTSATZ                                               
C                                                                               
      IF(NMS.LE.NRMS) GO TO 1                                                   
   73 READ(5,74) NRMS,(BEZ3(I),I=1,8)                                           
   74 FORMAT(I2,14X,8A8)                                                        
      WRITE(6,76) NRMS,(BEZ3(I),I=1,8)                                          
   76 FORMAT(1H1,'MESSREIHE NR.',I2,'   ZUR GLEICHEN KOEFFIZIENTENMATRIX        
     1',14X,8A8/1H ,50(1H=)//)                                                  
      GO TO 5                                                                   
 9000 STOP                                                                      
      END                                                                       
      
