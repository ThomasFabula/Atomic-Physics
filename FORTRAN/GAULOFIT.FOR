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
      IMPLICIT REAL*8(A-H,O-Z)                                                  
      DIMENSION WF(1000),WF1(1000),WZ(1000),FZ(1000),X(3),PF(3),FMT(3)          
      DIMENSION TEXT(8),FW(1000),RES(1000),ABL(3,1000)                          
      DATA FMT/'  GAUSS-',' LORENTZ','  GAUSS-'/                                
    1 READ(5,2,END=1000) NR,KFIT,NI,(TEXT(I),I=1,8)                             
    2 FORMAT(3I4,4X,8A8)                                                        
      IF(KFIT.NE.1.AND.KFIT.NE.2) KFIT=0                                        
      WRITE(6,3) NR,(TEXT(I),I=1,8)                                             
    3 FORMAT(1H1,'MESSKURVE  NR.',I4,10X,8A8/1H ,130(1H=))                      
      READ(5,7) AF,DF,KPLT,SX,SY,NORM,ZER,KP,WP,NSTAT                           
    7 FORMAT(2D14.6,I4,2F5.1,2(I2,D14.6),I2)                                    
      FS=DSQRT(DLOG(2.D0)*8.D0)                                                 
      IF(KFIT.EQ.1) FS=1.D0                                                     
      AMP=0.                                                                    
      ZERO=1.D20                                                                
      I=1                                                                       
    4 READ(5,8,END=9) WZ(I),FZ(I),WF(I)                                         
    8 FORMAT(3D20.12)                                                           
      IF((WZ(I).EQ.0.).AND.(FZ(I).EQ.1.)) GO TO 9                               
      IF(NSTAT.GE.1) FZ(I)=DSQRT(DABS(WZ(I)))*NSTAT                             
      IF(FZ(I).LE.0.D0) FZ(I)=1.D0                                              
      AMP=DMAX1(AMP,WZ(I))                                                      
      ZERO=DMIN1(ZERO,WZ(I))                                                    
      WF1(I)=WF(I)-WF(1)                                                        
      I=I+1                                                                     
      GO TO 4                                                                   
    9 NM=I-1                                                                    
      IF((NM+MIN0(1,KP)).GE.3) GO TO 6                                          
      WRITE(6,5) NM                                                             
    5 FORMAT(1H0,'ZAHL DER MESSWERTE =',I3,'   ,D. H. KLEINER ALS DIE ZA        
     1HL DER FREIEN PARAMETER. DIE RECHNUNG WIRD DESHALB BEENDET.')             
      GO TO 1                                                                   
    6 IF(NI.LE.0) NI=1                                                          
      WRITE(6,30) NM,NI                                                         
   30 FORMAT(1H-,'ZAHL DER MESSWERTE  =',I4,40X,'MAXIMALE ANZAHL DER ITE        
     1RATIONEN =',I3)                                                           
      IF((AF.EQ.0.D0).AND.(DF.EQ.0.D0)) GO TO 11                                
      WF(1)=AF                                                                  
      DO 10 I=2,NM                                                              
      WF(I)=WF(I-1)+DF                                                          
      WF1(I)=(I-1)*DF                                                           
   10 CONTINUE                                                                  
   11 WRITE(6,12)                                                               
   12 FORMAT(1H-,14X,'FREQUENZ',22X,'MESSWERT',22X,'FEHLER'//)                  
      WRITE(6,13) (WF(I),WZ(I),FZ(I),I=1,NM)                                    
   13 FORMAT(1H+,3D30.10/)                                                      
      IF(NORM.NE.1) GO TO 16                                                    
      IF(ZER.NE.0.D0) ZERO=ZER                                                  
      WRITE(6,14) ZERO                                                          
   14 FORMAT(1H0,'ZAEHLRATEN WERDEN NORMIERT AUF UNTERGRUND  =',F14.4)          
      DO 15 I=1,NM                                                              
   15 WZ(I)=WZ(I)-ZERO                                                          
      AMP=AMP-ZERO                                                              
   16 S=WF1(2)                                                                  
      IF(NM.EQ.2) GO TO 18                                                      
      DO 17 I=3,NM                                                              
      DIFF=DABS(WF1(I)-WF1(I-1))                                                
      S=DMIN1(S,DIFF)                                                           
   17 CONTINUE                                                                  
   18 S=S/1000.                                                                 
      SA=AMP*1.D-4                                                              
C                                                                               
      WRITE(6,19)                                                               
   19 FORMAT(1H-)                                                               
      IN=1                                                                      
      IF(KP.LE.0) GO TO 20                                                      
      WRITE(6,26) WP                                                            
   26 FORMAT(1H ,'WERT DES FESTEN PARAMETERS     =',F14.6//)                    
      NP=2                                                                      
      IF(KP-2) 100,200,300                                                      
C                                                                               
C                                                                               
C     ALLE 3 PARAMETER VARIIERT                                                 
C                                                                               
   20 NP=3                                                                      
      CALL FSTART(NM,WF,WF1,WZ,F0)                                              
      CALL SIGST(NM,WF1,WZ,FS,F0,SIG,AMP)                                       
      WRITE(6,22) AMP                                                           
   22 FORMAT(1H ,'STARTWERT FUER DIE AMPLITUDE   =',F14.6)                      
   23 WRITE(6,24) FMT(KFIT+1)                                                   
   24 FORMAT(1H1,'KORREKTUREN UND NEUE WERTE DER PARAMETER DER',A8,'KURV        
     1E'/1H ,57(1H-)//)                                                         
C                                                                               
C     ITERATIONEN                                                               
C                                                                               
      DO 90 N=1,NI                                                              
      CALL DERIV(NM,WF1,WZ,F0,SIG,AMP,ABL,RES,KFIT)                             
      IA=1                                                                      
      IE=3                                                                      
      CALL LSQ(NM,NP,IA,IE,IN,ABL,RES,FZ,X,PF,CHISQU)                           
      F0=F0+X(1)                                                                
      F1=F0+WF(1)                                                               
      SIG=DABS(SIG+X(2))                                                        
      AMP=AMP+X(3)                                                              
      WRITE(6,94) N,CHISQU                                                      
   94 FORMAT(//1H-,'ITERATION NR.',I2/1H ,15(1H-)/1H0,'CHI-QUADRAT =',          
     1D16.8//)                                                                  
      WRITE(6,91) X(1),F1                                                       
   91 FORMAT(1H ,7HDNUE0 =,F12.6,20X,6HNUE0 =,F14.6)                            
      HWB=SIG*FS                                                                
      DHWB=X(2)*FS                                                              
      WRITE(6,92) DHWB,HWB                                                      
   92 FORMAT(1H ,7HDHWB  =,F12.6,20X,6HHWB  =,F14.6)                            
      WRITE(6,93) X(3),AMP                                                      
   93 FORMAT(1H ,7HDAMPL =,F12.6,20X,6HAMPL =,F14.6)                            
      IF(DABS(X(3)).GT.SA) GO TO 90                                             
      IF(DABS(X(2)).GT.S) GO TO 90                                              
      IF(DABS(X(1)).LE.S) GO TO 80                                              
   90 CONTINUE                                                                  
C                                                                               
C     AUSDRUCK DER ERGEBNISSE                                                   
C                                                                               
   80 WRITE(6,82) FMT(KFIT+1),NR                                                
   82 FORMAT(1H1,'ERGEBNISSE DES',A8,'FITS FUER KURVE  NR.',I4/1H ,46(1H        
     1=))                                                                       
      WRITE(6,83) F1,PF(1)                                                      
   83 FORMAT(/1H-,'MITTENFREQUENZ',10X,'NUE0  =',F18.9,15X,'FEHLER  =',         
     1F15.9)                                                                    
      IF(KP.NE.1) WRITE(6,84)                                                   
   84 FORMAT(1H+,T105,'VARIIERT')                                               
      FHWB=PF(2)*FS                                                             
      WRITE(6,85) HWB,FHWB                                                      
   85 FORMAT(1H0,'HALBWERTSBREITE',9X,'HWB   =',F18.9,15X,'FEHLER  =',          
     1F15.9)                                                                    
      IF(KP.NE.2) WRITE(6,84)                                                   
      WRITE(6,86) AMP,PF(3)                                                     
   86 FORMAT(1H0,'AMPLITUDE',15X,'AMPL  =',F18.9,15X,'FEHLER  =',F15.9)         
      IF(KP.LT.3) WRITE(6,84)                                                   
      WRITE(6,87) CHISQU                                                        
   87 FORMAT(//1H-,'CHI-QUADRAT  =',D18.10/1H-,3X,'FREQUENZ',25X,'MESSWE        
     1RT',16X,'FEHLER',20X,'FUNKTIONSWERT',22X,'RESIDUAL')                      
      DO 89 I=1,NM                                                              
      FW(I)=FWERT(F0,SIG,AMP,WF1(I),KFIT)                                       
      RES(I)=WZ(I)-FW(I)                                                        
      WRITE(6,88) WF(I),WZ(I),FZ(I),FW(I),RES(I)                                
   88 FORMAT(1H0,F14.6,20X,F12.4,10X,F10.4,2(20X,F12.4))                        
   89 CONTINUE                                                                  
      IF(KFIT.NE.2) GO TO 1                                                     
      KFIT=1                                                                    
      FS=1.D0                                                                   
      SIG=HWB                                                                   
      IF(KP.LE.0) GO TO 23                                                      
      IF(KP-2) 102,202,302                                                      
C                                                                               
C     MITTENFREQUENZ ALS FESTER PARAMETER                                       
C                                                                               
  100 F0=WP-WF(1)                                                               
      F1=WP                                                                     
      PF(1)=0.D0                                                                
      CALL SIGST(NM,WF1,WZ,FS,F0,SIG,AMP)                                       
      WRITE(6,22) AMP                                                           
  102 WRITE(6,24) FMT(KFIT+1)                                                   
C                                                                               
C     ITERATIONEN                                                               
C                                                                               
      DO 110 N=1,NI                                                             
      CALL DERIV(NM,WF1,WZ,F0,SIG,AMP,ABL,RES,KFIT)                             
      IA=2                                                                      
      IE=3                                                                      
      CALL LSQ(NM,NP,IA,IE,IN,ABL,RES,FZ,X,PF,CHISQU)                           
      SIG=DABS(SIG+X(1))                                                        
      AMP=AMP+X(2)                                                              
      WRITE(6,94) N,CHISQU                                                      
      HWB=SIG*FS                                                                
      DHWB=X(1)*FS                                                              
      WRITE(6,92) DHWB,HWB                                                      
      WRITE(6,93) X(2),AMP                                                      
      IF(DABS(X(2)).GT.SA) GO TO 110                                            
      IF(DABS(X(1)).LE.S) GO TO 80                                              
  110 CONTINUE                                                                  
      GO TO 80                                                                  
C                                                                               
C     HALBWERTSBREITE ALS FESTER PARAMETER                                      
C                                                                               
  200 SIG=WP/FS                                                                 
      HWB=WP                                                                    
      PF(2)=0.D0                                                                
      CALL FSTART(NM,WF,WF1,WZ,F0)                                              
      WRITE(6,22) AMP                                                           
  202 WRITE(6,24) FMT(KFIT+1)                                                   
C                                                                               
C     ITERATIONEN                                                               
C                                                                               
      DO 210 N=1,NI                                                             
      CALL DERIV(NM,WF1,WZ,F0,SIG,AMP,ABL,RES,KFIT)                             
      IA=1                                                                      
      IE=3                                                                      
      IN=2                                                                      
      CALL LSQ(NM,NP,IA,IE,IN,ABL,RES,FZ,X,PF,CHISQU)                           
      F0=F0+X(1)                                                                
      F1=F0+WF(1)                                                               
      AMP=AMP+X(2)                                                              
      WRITE(6,94) N,CHISQU                                                      
      WRITE(6,91) X(1),F1                                                       
      WRITE(6,93) X(2),AMP                                                      
      IF(DABS(X(2)).GT.SA) GO TO 210                                            
      IF(DABS(X(1)).LE.S) GO TO 80                                              
  210 CONTINUE                                                                  
      GO TO 80                                                                  
C                                                                               
C     AMPLITUDE ALS FESTER PARAMETER                                            
C                                                                               
  300 AMP=WP                                                                    
      IF(NORM.EQ.1) AMP=WP-ZERO                                                 
      PF(3)=0.D0                                                                
      CALL FSTART(NM,WF,WF1,WZ,F0)                                              
      CALL SIGST(NM,WF1,WZ,FS,F0,SIG,AMP)                                       
  302 WRITE(6,24) FMT(KFIT+1)                                                   
C                                                                               
C     ITERATIONEN                                                               
C                                                                               
      DO 310 N=1,NI                                                             
      CALL DERIV(NM,WF1,WZ,F0,SIG,AMP,ABL,RES,KFIT)                             
      IA=1                                                                      
      IE=2                                                                      
      CALL LSQ(NM,NP,IA,IE,IN,ABL,RES,FZ,X,PF,CHISQU)                           
      F0=F0+X(1)                                                                
      F1=F0+WF(1)                                                               
      SIG=DABS(SIG+X(2))                                                        
      WRITE(6,94) N,CHISQU                                                      
      WRITE(6,91) X(1),F1                                                       
      HWB=SIG*FS                                                                
      DHWB=X(2)*FS                                                              
      WRITE(6,92) DHWB,HWB                                                      
      IF(DABS(X(2)).GT.S) GO TO 310                                             
      IF(DABS(X(1)).LE.S) GO TO 80                                              
  310 CONTINUE                                                                  
      GO TO 80                                                                  
C                                                                               
 1000 STOP                                                                      
      END                                                                       
      REAL FUNCTION FWERT*8(X1,X2,X3,W,KF)                                      
C     BERECHNUNG DER GAUSS- BZW. DER LORENTZ-FUNKTIONSWERTE                     
      IMPLICIT REAL*8(A-H,O-Z)                                                  
      F=(W-X1)/X2                                                               
      IF(KF.NE.1) GO TO 2                                                       
      FWERT=X3/(4.D0*F*F+1.D0)                                                  
      RETURN                                                                    
    2 FWERT=0.D0                                                                
      IF(DABS(F).GT.10.D0) RETURN                                               
      FWERT=X3/DEXP(F*F/2.D0)                                                   
      RETURN                                                                    
      END                                                                       
      SUBROUTINE FSTART(N,WF,WF1,Z,F0)                                          
C     STARTWERT FUER DIE LINIENMITTE                                            
      IMPLICIT REAL*8(A-H,O-Z)                                                  
      DIMENSION WF(1),WF1(1),Z(1)                                               
      S1=0.                                                                     
      S2=0.                                                                     
      DO 2 I=1,N                                                                
      S1=S1+WF1(I)*Z(I)                                                         
      S2=S2+Z(I)                                                                
    2 CONTINUE                                                                  
      F0=S1/S2                                                                  
      F=F0+WF(1)                                                                
      WRITE(6,3) F                                                              
    3 FORMAT(1H ,'STARTWERT FUER MITTENFREQUENZ  =',F14.6)                      
      RETURN                                                                    
      END                                                                       
      SUBROUTINE SIGST(N,WF1,Z,FS,X1,X2,X3)                                     
C     STARTWERT FUER DIE HALBWERTSBREITE                                        
      IMPLICIT REAL*8(A-H,O-Z)                                                  
      DIMENSION WF1(1),Z(1)                                                     
      S3=Z(1)*DABS(WF1(2)-WF1(1))                                               
      IF(Z(1).LE.0.) S3=0.                                                      
      DO 2 I=2,N                                                                
      IF(Z(I).LE.0.D0) GO TO 2                                                  
      S3=S3+Z(I)*DABS(WF1(I)-WF1(I-1))                                          
    2 CONTINUE                                                                  
      PI2=4.*DARSIN(1.D0)                                                       
      X2=S3/(X3*DSQRT(PI2))                                                     
      FH=DSQRT(DLOG(2.D0)*8.D0)                                                 
      HWB=X2*FH                                                                 
      WRITE(6,3) HWB                                                            
    3 FORMAT(1H ,'STARTWERT FUER HALBWERTSBREITE =',F14.6)                      
      X2=HWB/FS                                                                 
      RETURN                                                                    
      END                                                                       
      SUBROUTINE DERIV(N,WF1,Z,X1,X2,X3,ABL,RES,KF)                             
C     BERECHNUNG DER RESIDUALS UND DER PARTIELLEN ABLEITUNGEN DER FKT.          
      IMPLICIT REAL*8(A-H,O-Z)                                                  
      DIMENSION WF1(1),Z(1),RES(1),ABL(3,1000),FW(1000)                         
      DO 2 I=1,N                                                                
      FW(I)=FWERT(X1,X2,X3,WF1(I),KF)                                           
      ABL(3,I)=FW(I)/X3                                                         
      ABL(1,I)=FW(I)*(WF1(I)-X1)/(X2*X2)                                        
      IF(KF.EQ.1) ABL(1,I)=8.D0*ABL(1,I)*ABL(3,I)                               
      ABL(2,I)=ABL(1,I)*(WF1(I)-X1)/X2                                          
      RES(I)=Z(I)-FW(I)                                                         
    2 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE LSQ(N,M,I1,I2,I3,ABL,B,F,X,PF,CHISQU)                          
C     ZUSAMMENSTELLUNG DES GLEICHUNGSSYSTEMS UND LEAST-SQUARES-FIT              
      IMPLICIT REAL*8(A-H,O-Z)                                                  
      DIMENSION B(1),F(1),X(1),PF(1),CA(9),ZZ(9),LL(3),MM(3)                    
      DIMENSION A(3000),C(1000),ABL(3,1000)                                     
      L=1                                                                       
      DO 2 J=I1,I2,I3                                                           
      DO 2 I=1,N                                                                
      A(L)=ABL(J,I)                                                             
      L=L+1                                                                     
    2 CONTINUE                                                                  
      DO 4 I=1,N                                                                
    4 C(I)=F(I)*F(I)                                                            
      CALL LEASTS(A,C,B,N,M,CA,X,CHISQU,IER,ZZ,LL,MM)                           
      IF(IER.NE.0) WRITE(6,5) IER                                               
    5 FORMAT(1H0,'FEHLER IN LEASTS , IER =',I2)                                 
      K=1                                                                       
      DO 6 J=I1,I2,I3                                                           
      PF(J)=DSQRT(DABS(CA(K)))                                                  
      IF(N.NE.M) PF(J)=PF(J)*DSQRT(CHISQU/(N-M))                                
      K=K+M+1                                                                   
    6 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
                       
