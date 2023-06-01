      IMPLICIT REAL*8(A-H,O-Z)                                                  
      REAL*4 SX,SY,FOB(25),FUN(25),FO(26),FU(26),SKTX                           
      DIMENSION WF(800),WF1(800),WZ(800),FZ(800),FW(800),FMT(5)                 
      DIMENSION STF(26),STS(26),STA(26),KPAR(25),BEZ(25),BZ(26)                 
      DIMENSION TEXT(8),X(76),PF(76),PAR(3,25),F0(25),SIG(25),A(25)             
      DATA FMT/'  GAUSS-',' LORENTZ','  GAUSS-','START',' FEST'/                
C                                                                               
C     EINLESEN DER DATEN                                                        
C                                                                               
      NPLT=0                                                                    
      KENDE=0                                                                   
      LDEND=0                                                                   
    1 IF(KENDE.EQ.1) GO TO 1000                                                 
      READ(5,2,END=1000) NR,KFIT,NI,SKTX,(TEXT(I),I=1,8)                        
    2 FORMAT(3I4,F4.2,8A8)                                                      
      IF(NR.LE.0) NR=1                                                          
      IF(KFIT.NE.1.AND.KFIT.NE.2) KFIT=0                                        
      IF(ABS(SKTX).LT.0.2)  SKTX=1.                                             
      WRITE(6,3) NR,(TEXT(I),I=1,8)                                             
    3 FORMAT(1H1,'MESSKURVE  NR.',I4,10X,8A8/1H ,130(1H=))                      
      READ(5,7) AF,DF,KPLT,SX,SY,NORM,ZER,YMAX,KP,WP,NSTAT,KBZ,KA               
    7 FORMAT(2D14.6,I4,2F5.1,I2,D10.4,D8.2,I2,D10.4,3I2)                        
      FS=DSQRT(DLOG(2.D0)*8.D0)                                                 
      IF(KFIT.EQ.1) FS=1.D0                                                     
      IF(IABS(KP-1).GT.1) KP=0                                                  
      KNRM=NORM/2.+0.1                                                          
      KNOR=NORM-2*KNRM                                                          
      IF(KA.NE.1) KA=0                                                          
      J=0                                                                       
   31 J=J+1                                                                     
      READ(5,32) STF(J),STS(J),STA(J),BZ(J),FO(J),FU(J)                         
   32 FORMAT(3D20.12,A8,2F6.1)                                                  
      IF((DABS(STF(J))+STS(J)+DABS(STA(J))).NE.0.) GO TO 31                     
      IF(J.EQ.1) WRITE(6,19)                                                    
   19 FORMAT(1H-,'KEINE PARAMETERSTARTWERTE EINGEGEBEN'/)                       
      NK=MAX0(1,J-1)                                                            
      CALL SORT(STF,NK,KPAR)                                                    
      DO 33 K=1,NK                                                              
      J=KPAR(K)                                                                 
      PAR(1,K)=STF(J)                                                           
      PAR(2,K)=STS(J)                                                           
      PAR(3,K)=STA(J)                                                           
      BEZ(K)=BZ(J)                                                              
      FOB(K)=FO(J)                                                              
      FUN(K)=FU(J)                                                              
   33 CONTINUE                                                                  
      IF(KP.NE.0.OR.NK.EQ.1) GO TO 21                                           
      DO 20 J=1,NK                                                              
      IF(PAR(2,J).EQ.0.) PAR(2,J)=STS(1)                                        
   20 CONTINUE                                                                  
   21 IF(KP.NE.0) PAR(2,1)=WP                                                   
      AMP=PAR(3,1)                                                              
      ZERO=1.D20                                                                
      I=1                                                                       
      L=5                                                                       
    4 READ(L,8,END=9) WZ(I),FZ(I),WF(I)                                         
    8 FORMAT(3D20.12)                                                           
      IF((WZ(I).EQ.0.).AND.(FZ(I).EQ.1.)) GO TO 22                              
      IF(NSTAT.GE.1.AND.FZ(I).LE.1.D0) FZ(I)=DSQRT(DABS(WZ(I)))*NSTAT           
      IF(FZ(I).LE.0.D0) FZ(I)=DMAX1(1.D0,DABS(FZ(I)))                           
      ZERO=DMIN1(ZERO,WZ(I))                                                    
      WF1(I)=WF(I)-WF(1)                                                        
      I=I+1                                                                     
      GO TO 4                                                                   
    9 IF(L.EQ.5)  KENDE=1                                                       
      IF(L.EQ.11) LDEND=1                                                       
   22 L=11                                                                      
      IF(I.EQ.1.AND.LDEND.NE.1) GO TO 4                                         
C                                                                               
C     VORBEREITUNG FUER DEN FIT UND AUSDRUCK DER EINGABEDATEN                   
C                                                                               
      NM=I-1                                                                    
      NK2=2.*NK-KA*(NK-1)+0.1                                                   
      NP=2.*(NK+1)-KP-KA*(NK-1)+KNOR+0.1                                        
      IF(KP.EQ.0) NP=NP+NK-2                                                    
      IF(NM.GE.NP) GO TO 6                                                      
      WRITE(6,5) NM                                                             
    5 FORMAT(1H0,'ZAHL DER MESSWERTE =',I3,'   ,D. H. KLEINER ALS DIE ZA        
     1HL DER FREIEN PARAMETER. DIE RECHNUNG WIRD DESHALB BEENDET.')             
      GO TO 1                                                                   
    6 WRITE(6,30) NM,NP,NI                                                      
   30 FORMAT(1H-,'ZAHL DER MESSWERTE  =',I4,16X,'ZAHL DER VARIIERTEN PAR        
     1AMETER =',I3,19X,'MAXIMALE ANZAHL DER ITERATIONEN =',I3)                  
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
      WRITE(6,34)                                                               
   34 FORMAT(1H-,'STARTWERTE FUER :',20X,'MITTENFREQUENZ',42X,'AMPLITUDE        
     1')                                                                        
      IF(KP.EQ.0) WRITE(6,35)                                                   
   35 FORMAT(1H+,T68,'HALBWERTSBREITE')                                         
      WRITE(6,40)                                                               
   40 FORMAT(1H )                                                               
      DO 38 J=1,NK                                                              
      WRITE(6,36) J,BEZ(J),PAR(1,J),PAR(3,J)                                    
   36 FORMAT(1H ,I2,8X,A8,14X,F16.6 ,41X,F16.6)                                 
      F0(J)=PAR(1,J)-WF(1)                                                      
      SIG(J)=PAR(2,1)/FS                                                        
      A(J)=PAR(3,J)                                                             
      AMP=DMIN1(AMP,A(J))                                                       
      IF(KP.NE.0) GO TO 38                                                      
      SIG(J)=PAR(2,J)/FS                                                        
      WRITE(6,37) PAR(2,J)                                                      
   37 FORMAT(1H+,T61,F16.6)                                                     
   38 CONTINUE                                                                  
      IF(KP.NE.0) WRITE(6,43) FMT(KP+3),PAR(2,1)                                
      IF(KA.EQ.1) WRITE(6,45)                                                   
   43 FORMAT(1H-,  A5 , 'WERT FUER GEMEINSAME HALBWERTSBREITE ALLER TEIL        
     1KURVEN  =',F12.6)                                                         
   45 FORMAT(1H-,'AMPLITUDEN WERDEN RELATIV ZUEINANDER IM VERHAELTNIS IH        
     1RER STARTWERTE FESTGEHALTEN')                                             
      IF(NORM.EQ.0) GO TO 16                                                    
      IF(ZER.NE.0.D0) ZERO=ZER                                                  
      WRITE(6,14) FMT(5-KNOR),ZERO                                              
   14 FORMAT(1H-,  A5 , 'WERT FUER UNTERGRUND :    ZERO  =',F12.4)              
      AMP=AMP-ZERO                                                              
      DO 41 J=1,NK                                                              
      PAR(3,J)=PAR(3,J)-ZERO                                                    
   41 A(J)=A(J)-ZERO                                                            
   16 S=WF1(2)                                                                  
      IF(NM.EQ.2) GO TO 18                                                      
      DO 17 I=3,NM                                                              
      DIFF=DABS(WF1(I)-WF1(I-1))                                                
      S=DMIN1(S,DIFF)                                                           
   17 CONTINUE                                                                  
   18 S=S/1000.                                                                 
      SA=DABS(AMP)*1.D-4                                                        
      IF(NORM.EQ.0) ZERO=0.D0                                                   
      WRITE(6,40)                                                               
      IF((KFIT+KPLT+NORM+NSTAT+KBZ).GT.0) WRITE(6,39) KFIT,KPLT,NORM,           
     1NSTAT,KBZ,SKTX                                                            
   39 FORMAT(1H-,'VERWENDETE OPTIONEN :     KFIT =',I2,10X,'KPLT =',I3,         
     110X,'NORM =',I2,10X,'NSTAT =',I2,10X,'KBZ =',I2,10X,'SKTX =',F5.2)        
C                                                                               
   23 WRITE(6,24) FMT(KFIT+1)                                                   
   24 FORMAT(1H1,'KORREKTUREN UND NEUE WERTE DER PARAMETER DER',A8,'KURV        
     1E'/1H ,57(1H-)/)                                                          
      CHI=0.                                                                    
      IF(NI.LE.0) GO TO 77                                                      
C                                                                               
C     ITERATIONEN                                                               
C                                                                               
      DO 90 N=1,NI                                                              
      CALL FITSUB(NM,NP,NK,WF1,WZ,FZ,F0,SIG,A,X,PF,CHI,KA,KFIT,KP,KNOR,         
     1ZERO)                                                                     
      IF(CHI.EQ.-1.D0) GO TO 98                                                 
      KSF=0                                                                     
      KSS=0                                                                     
      KSA=0                                                                     
      IF(DABS(X(NK2+1)).GT.S.AND.KP.EQ.1) KSS=1                                 
      WRITE(6,94) N,CHI                                                         
      IF(KP.EQ.0) WRITE(6,95)                                                   
      IF(KP.NE.0) WRITE(6,96)                                                   
   94 FORMAT (/1H-,'ITERATION NR.',I2/1H ,15(1H-)/1H ,'CHI-QUADRAT =',          
     1D16.8/)                                                                   
   95 FORMAT(1H ,20X,'DNUE0',12X,'NUE0 (NEU)',16X,'DHWB',12X,'HWB (NEU)'        
     1,16X,'DAMPL',11X,'AMPL (NEU)'/)                                           
   96 FORMAT(1H ,25X,'DNUE0',18X,'NUE0 (NEU)',20X,'DAMPL',18X,'AMPL (NEU        
     1)'/)                                                                      
      DO 70 J=1,NK                                                              
      F0(J)=F0(J)+X(J)                                                          
      F1=F0(J)+WF(1)                                                            
      L=J-KA*(J-1)+0.1                                                          
      XAMP=X(NK+L)*(KA*A(J)+1-KA)                                               
      A(J)=A(J)+XAMP                                                            
      IF(DABS(X(J)).GT.S) KSF=1                                                 
      IF(DABS(XAMP).GT.SA) KSA=1                                                
      IF(KP-1) 50,60,65                                                         
   50 SIG(J)=DABS(SIG(J)+X(NK2+J))                                              
      HWB=SIG(J)*FS                                                             
      DHWB=X(NK2+J)*FS                                                          
      IF(DABS(X(NK2+J)).GT.S) KSS=1                                             
      WRITE(6,91) J,X(J),F1,DHWB,HWB,XAMP,A(J)                                  
   91 FORMAT(1H ,I2,14X,F12.6,5X,F14.6,12X,F12.6,3X,F14.6,12X,F12.4,4X,         
     1F14.4)                                                                    
      GO TO 70                                                                  
   60 SIG(J)=DABS(SIG(J)+X(NK2+1))                                              
      HWB=SIG(J)*FS                                                             
      DHWB=X(NK2+1)*FS                                                          
   65 WRITE(6,93) J,X(J),F1,XAMP,A(J)                                           
   93 FORMAT(1H ,I2,19X,F12.6,11X,F14.6,16X,F12.4,11X,F14.4)                    
   70 CONTINUE                                                                  
      IF(KP.EQ.1) WRITE(6,92) DHWB,HWB                                          
   92 FORMAT(1H0,'DHWB  =',F12.6,16X,'HWB (NEU)  =',F13.6)                      
      IF(KNOR.EQ.0) GO TO 71                                                    
      ZERO=ZERO+X(NP)                                                           
      WRITE(6,97) X(NP),ZERO                                                    
   97 FORMAT(1H0,'DZERO =',F10.4,18X,'ZERO (NEU) =',F11.4)                      
   71 IF((KSF+KSS+KSA).EQ.0) GO TO 80                                           
   90 CONTINUE                                                                  
C                                                                               
C     AUSDRUCK DER ERGEBNISSE                                                   
C                                                                               
   80 WRITE(6,82) FMT(KFIT+1),NR                                                
   82 FORMAT(1H1,'ERGEBNISSE DES',A8,'FITS FUER KURVE  NR.',I4/1H ,46(1H        
     1=))                                                                       
      WRITE(6,83)                                                               
   83 FORMAT(/1H-,'TEILKURVE',8X,'MITTENFREQUENZ',7X,'FEHLER',16X,              
     1'HALBWERTSBR.',   29X,'AMPLITUDE',11X,'FEHLER')                           
      IF(KP.NE.2) WRITE(6,84)                                                   
   84 FORMAT(1H+,T81,'FEHLER')                                                  
      WRITE(6,40)                                                               
      K=1                                                                       
      DO 99 J=1,NK                                                              
      F1=F0(J)+WF(1)                                                            
      HWB=SIG(J)*FS                                                             
      L=J-KA*(J-1)+0.1                                                          
      FAMP=PF(NK+L)*(KA*A(J)+1-KA)                                              
      WRITE(6,81) J,BEZ(J),F1,PF(J),HWB,A(J),FAMP                               
   81 FORMAT(1H0,I2, 4X,A8,F16.8,F18.8,7X,F16.8,26X,F16.6,5X,F14.6)             
      IF(KP.EQ.2) GO TO 75                                                      
      IF(KP.EQ.1) PF(NK2+J)=PF(NK2+1)                                           
      FHWB=PF(NK2+J)*FS                                                         
      WRITE(6,85) FHWB                                                          
   85 FORMAT(1H+,T75,F16.8)                                                     
   75 IF((FOB(J)+FUN(J)).EQ.0.) GO TO 99                                        
      WRITE(7,76) K,F1,PF(J),FOB(J),FUN(J),BEZ(J)                               
   76 FORMAT(I2,F16.6,4X,F14.6,24X,2F6.1,A8)                                    
      K=K+1                                                                     
   99 CONTINUE                                                                  
      IF(NORM.GT.0) WRITE(6,72) ZERO                                            
      IF(KNOR.EQ.1) WRITE(6,73) PF(NP)                                          
   72 FORMAT(1H-,'UNTERGRUND   =',F15.6)                                        
   73 FORMAT(1H+,T46,'FEHLER  =',F13.6)                                         
   77 WRITE(6,87) CHI                                                           
   87 FORMAT(//1H-,'CHI-QUADRAT  =',D18.10/1H-,3X,'FREQUENZ',25X,'MESSWE        
     1RT',16X,'FEHLER',20X,'FUNKTIONSWERT',22X,'RESIDUAL')                      
      DO 89 I=1,NM                                                              
      FW(I)=ZERO                                                                
      DO 86 J=1,NK                                                              
   86 FW(I)=FW(I)+FWERT(F0(J),SIG(J),A(J),WF1(I),KFIT)                          
      RES=WZ(I)-FW(I)                                                           
      CHI=CHI+(RES/FZ(I))**2                                                    
      WRITE(6,88) WF(I),WZ(I),FZ(I),FW(I),RES                                   
   88 FORMAT(1H0,F14.6,18X,F14.4, 8X,F12.4,2(18X,F14.4))                        
   89 CONTINUE                                                                  
      IF(NI.LE.0)  WRITE(6,79) CHI                                              
   79 FORMAT(//1H0,'CHI-QUADRAT FUER  0. ITERATION =',D14.6)                    
      IF(KPLT.GE.1)CALL PLTSUB(NM,WF,WZ,FZ,FW,SY,SX,KFIT,NPLT,KPLT,TEXT,        
     1NORM,ZERO,YMAX,NK,F0,BEZ,KBZ,SKTX)                                        
   98 IF(KFIT.NE.2) GO TO 1                                                     
      KFIT=1                                                                    
      FS=1.D0                                                                   
      DO 78 J=1,NK                                                              
      F0(J)=PAR(1,J)-WF(1)                                                      
      SIG(J)=PAR(2,1)                                                           
      A(J)=PAR(3,J)                                                             
      IF(KP.EQ.0) SIG(J)=PAR(2,J)                                               
   78 CONTINUE                                                                  
      GO TO 23                                                                  
 1000 IF(NPLT.GE.1) CALL PLTSUB(NM,WF,WZ,FZ,FW,SY,SX,3,NPLT,KPLT,TEXT,          
     1NORM,ZERO,YMAX,NK,F0,BEZ,KBZ,SKTX)                                        
      STOP                                                                      
      END                                                                       
      REAL FUNCTION FWERT*8(X1,X2,X3,W,KF)                                      
C                                                                               
C     BERECHNUNG DER GAUSS- BZW. DER LORENTZ-FUNKTIONSWERTE                     
C                                                                               
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
      SUBROUTINE FITSUB(N,M,NK,WF1,Z,F,F0,SIG,AMP,X,PF,CHISQU,KA,KF,KP,         
     1KN,ZER)                                                                   
C                                                                               
C     ZUSAMMENSTELLUNG DES GLEICHUNGSSYSTEMS UND LEAST-SQUARES-FIT              
C                                                                               
      IMPLICIT REAL*8(A-H,O-Z)                                                  
      DIMENSION WF1(1),Z(1),F(1),F0(1),SIG(1),AMP(1),X(1),PF(1)                 
      DIMENSION CA(5776),ZZ(5776),LL(76),MM(76),B(800),C(800),A(60800)          
      DO 4 I=1,N                                                                
      SFW=0.D0                                                                  
      SAB=0.D0                                                                  
      DO 2 J=1,NK                                                               
      TFW=FWERT(F0(J),SIG(J),AMP(J),WF1(I),KF)                                  
      SFW=SFW+TFW                                                               
      N1=(J-1)*N+I+0.1                                                          
      N2=N1+N*NK+0.1                                                            
      N3=N2+N*NK+0.1                                                            
      IF(KA.NE.1) GO TO 1                                                       
      N2=N*NK+I+0.1                                                             
      N3=N1+N*(NK+1)+0.1                                                        
    1 A(N2)=TFW/AMP(J)                                                          
      A(N1)=TFW*(WF1(I)-F0(J))/(SIG(J)*SIG(J))                                  
      IF(KF.EQ.1) A(N1)=8.D0*A(N1)*A(N2)                                        
      A(N3)=A(N1)*(WF1(I)-F0(J))/SIG(J)                                         
      SAB=SAB+A(N3)                                                             
    2 CONTINUE                                                                  
      N4=M*N-I+1                                                                
      IF(KN.EQ.1) A(N4)=1.D0                                                    
      IF(KA.EQ.1) A(N2)=SFW                                                     
      IF(KP.EQ.1) A(N2+N)=SAB                                                   
      B(I)=Z(I)-SFW-ZER                                                         
      C(I)=F(I)*F(I)                                                            
    4 CONTINUE                                                                  
      CALL LEASTS(A,C,B,N,M,CA,X,CHISQU,IER,ZZ,LL,MM)                           
      IF(IER.NE.3) GO TO 3                                                      
      WRITE(6,5)                                                                
    5 FORMAT(1H-,'FEHLER IN DMINV, FIT DIVERGIERT ODER IST NICHT LINEAR         
     1UNABHAENGIG. DIE RECHNUNG WIRD DESHALB ABGEBROCHEN')                      
      CHISQU=-1.D0                                                              
      RETURN                                                                    
    3 K=1                                                                       
      DO 6 J=1,M                                                                
      IF(CA(K).GE.0.D0) GO TO 7                                                 
      WRITE(6,72)                                                               
   72 FORMAT(1H-,'FIT DIVERGIERT,DIE RECHNUNG WIRD DESHALB ABGEBROCHEN')        
      CHISQU=-1.D0                                                              
      RETURN                                                                    
    7 PF(J)=DSQRT(CA(K))                                                        
      IF(N.NE.M) PF(J)=PF(J)*DSQRT(CHISQU/(N-M))                                
      K=K+M+1                                                                   
    6 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
                                                                                                             
