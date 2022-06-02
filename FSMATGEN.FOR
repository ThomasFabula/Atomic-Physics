C                                                                               
C     PROGRAMM  'FSMATRIX L**N S '  ZUR BERECHNUNG DER FEINSTRUKTUR-MATRIXELE-  
C     MENTE INNERHALB EINER KONFIGURATION  L**N S  ODER  L**N   (L=P,D,F)       
C                                                                               
C                                                                               
      IMPLICIT REAL*8(A-H,O-Z)                                                  
      PARAMETER(NCO=120,NS=60,NP=12)                                            
      INTEGER RM,R(14)                                                          
      REAL*4 RME(NCO,NCO,6)                                                     
      CHARACTER TEXT(12)*4,TNAM(NCO)*8,PN(NP)*8,Y(13)*1,YSTR(6)*1               
      CHARACTER PN1(NP)*12,T*5,GN(5)*2,FMT1(6)*60,FMT2(6)*60                    
      DIMENSION XS(NCO),XL(NCO),IV(NCO),N0(14)                                  
      DIMENSION XS1(NCO),XS2(NCO),XL1(NCO),XL2(NCO),NC(NCO),KZC(NCO,NCO)        
      DIMENSION XS0(NCO),XL0(NCO),NC0(NCO),UK(10),V1K(10),CFP(NCO,NCO)          
      DIMENSION S1(NS),CL1(NS),S2(NS),CL2(NS),CK(5),SJ(9)                       
      DIMENSION S(NS),CL(NS),KZ(NS,14),GJ(NS,14),C(NP),KZP(NP,3)                
      DATA PN/'F0','F2','F4','F6','ZETA','ALPHA','BETA','Q2','Q4','Q6'/         
      DATA T/'COEFF'/,GN/'G1','G2','G3','G4','G5'/                              
      DATA Y/'S','P','D','F','G','H','I','K','L','M','N','O','Q'/               
      DATA YSTR/'2',' ','2','3','4','5'/                                        
      DATA FMT1/'3X,5(A,4X))', '2X,7(A,3X))', '4(A,3X),A10,2(A,3X),A9)',        
     1         '2X,5F16.7)', '2X,7F15.7)', '4F15.7,F11.3,F14.7,2F15.7)'/        
      DATA KZP/5,6,8,12,8*0,3,5,6,7,8,9,12,5*0,3,4,5,6,8,9,10,12,4*0/           
C                                                                               
      READ(5,10) NKONF,NZ,NZ0,ZL,ZLSTR,XMIN,XMAX,KENZ,KENP,(TEXT(I),            
     1I=1,12)                                                                   
   10 FORMAT(I2,2I3,4F4.1,2I4,12A4)                                             
      DO 7 I=1,NCO                                                              
      IF(NZ0.LE.0) GO TO 203                                                    
      DO 202 N=1,NCO                                                            
  202 CFP(I,N)=0.                                                               
  203 DO 6 J=1,NCO                                                              
      DO 4 K=1,6                                                                
    4 RME(I,J,K)=0.D0                                                           
    6 CONTINUE                                                                  
    7 CONTINUE                                                                  
      ZLL=2.*ZL+1.                                                              
      KKONF=NKONF                                                               
      IF(NKONF.GT.(ZLL+0.1))KKONF=2.*ZLL-NKONF+0.1                              
      MMAX=DABS(XMAX-XMIN)+1.1                                                  
      XJ=DMIN1(XMIN,XMAX)-1.                                                    
      IF(KENZ.EQ.1) READ(5,16) (N0(M),M=1,MMAX)                                 
   16 FORMAT(20I4)                                                              
      SSTR=0.                                                                   
      DO 200 I=1,NZ                                                             
      READ(5,14)  TNAM(I),XS(I),XL(I),IV(I),XS1(I),XL1(I),NC(I),XS2(I),         
     1XL2(I)                                                                    
   14 FORMAT(2X,A8,6X,2(2D8.1,I4),2D8.1)                                        
      SSTR=DMAX1(SSTR,XS2(I))                                                   
  200 CONTINUE                                                                  
      LMAX=ZL+0.1                                                               
      KMAX=ZLL-1.9                                                              
      LSTR=ZLSTR+0.1                                                            
      ISSTR=2.*SSTR+1.1                                                         
      JSTR=LSTR+ISSTR                                                           
      WRITE(6,11) Y(LMAX+1),NKONF,Y(LSTR+1),YSTR(ISSTR),(TEXT(I),I=1,12)        
   11 FORMAT(1H1,'KONFIGURATION  ',A1,' HOCH',I2,2X,A1,1X,A1,20X,12A4/1H        
     1 ,130(1H=))                                                               
      ASSIGN 300 TO LBL                                                         
      IF((ZL.LT.1.).OR.(ZL.GT.5.)) GO TO LBL                                    
      LD=5                                                                      
      NL=0                                                                      
C                                                                               
      IF(NZ0.GT.0) GO TO 205                                                    
   17 READ(LD,18,END=60)NC1,NC2,(RME(NC1,NC2,K),K=1,6)                          
   18 FORMAT(2I4,6D12.6)                                                        
      NL=1                                                                      
      IF(NC1.EQ.NC2) GO TO 17                                                   
      DO 15 I=1,NZ                                                              
      IF(NC(I).EQ.NC1) N1=I                                                     
      IF(NC(I).EQ.NC2) N2=I                                                     
   15 CONTINUE                                                                  
      IEX=DABS(XS1(N1)-XS1(N2)+XL1(N1)-XL1(N2))+0.1                             
      DO 19 K=1,6                                                               
   19 RME(NC2,NC1,K)=(-1)**IEX*RME(NC1,NC2,K)                                   
      GO TO 17                                                                  
   60 LD=11                                                                     
      IF(NL) 300,17,20                                                          
C                                                                               
  205 DO 208 I=1,NZ                                                             
      DO 207 J=1,NZ                                                             
  207 KZC(NC(I),NC(J))=0                                                        
  208 CONTINUE                                                                  
      READ(5,210) (XS0(J),XL0(J),NC0(J),J=1,NZ0)                                
  210 FORMAT(16X,2D8.1,20X,I4,24X)                                              
  215 READ(LD,220,END=70) NC1,N1,(NCP,CFP(NC1,NCP),N=1,MIN0(N1,4))              
  220 FORMAT(2(I4,4X),4(I4,D12.6))                                              
      IF(N1.GT.4) READ(LD,222)(NCP,CFP(NC1,NCP),N=5,N1)                         
  222 FORMAT((16X,4(I4,D12.6)))                                                 
      NL=1                                                                      
      GO TO 215                                                                 
   70 LD=11                                                                     
      IF(NL) 300,215,225                                                        
  225 DO 250 I=1,NZ                                                             
      NC1=NC(I)                                                                 
      DO 240 J=1,NZ                                                             
      NC2=NC(J)                                                                 
      IF(NC1.GT.NC2.OR.KZC(NC1,NC2).NE.0) GO TO 240                             
      IF(DABS(XS1(I)-XS1(J)).GT.1.) GO TO 240                                   
      CALL RMEGEN(KKONF,NZ0,NCO,ZL,XS1(I),XL1(I),XS1(J),XL1(J),NC1,NC2,         
     1XS0,XL0,NC0,CFP,UK,V1K,RMESUM)                                            
      KZC(NC1,NC2)=1                                                            
      IF(RMESUM.LT.1.D-6) GO TO 240                                             
      SUMV=DABS(UK(2))                                                          
      DO 230 K=1,3                                                              
      SUMV=SUMV+DABS(V1K(K))                                                    
      KK=2.*K+0.1                                                               
      IF(NKONF.EQ.KKONF) GO TO 228                                              
      UK(KK)=-UK(KK)                                                            
      V1K(KK-1)=-V1K(KK-1)                                                      
  228 RME(NC1,NC2,K)=UK(KK)                                                     
      RME(NC1,NC2,K+3)=V1K(KK-1)                                                
  230 CONTINUE                                                                  
      IF(SUMV.GE.1.D-6) WRITE(12,232) NC1,NC2,V1K(2),UK(2),V1K(1),V1K(3)        
  232 FORMAT(2I4,4F16.8)                                                        
      KM=MIN0(6,KMAX+1)                                                         
      WRITE(13,233)NC1,NC2,(UK(K),K=2,KM)                                       
      WRITE(13,234)NC1,NC2,(V1K(K),K=1,KM)                                      
  233 FORMAT(2I4,12X,5F12.8)                                                    
  234 FORMAT(2I4,6F12.8)                                                        
      IF(NC1.EQ.NC2) GO TO 240                                                  
      IEX=DABS(XS1(I)-XS1(J)+XL1(I)-XL1(J))+0.1                                 
      DO 235 K=1,6                                                              
  235 RME(NC2,NC1,K)=(-1)**IEX*RME(NC1,NC2,K)                                   
  240 CONTINUE                                                                  
  250 CONTINUE                                                                  
C                                                                               
C                                                                               
   20 PN(12)=GN(LMAX-LSTR)                                                      
      LMAX=MIN0(3,LMAX)                                                         
      L1MX=LMAX+3                                                               
      FAK1=DSQRT(ZL*(ZL+1.)*ZLL)                                                
      FAK2=ZLL*ZLL/2.                                                           
      DO 41 L=1,LMAX                                                            
      ZK=2.*L                                                                   
      CALL DREIJ(ZL,ZK,ZL,0.,0.,0.,DRJ,IER)                                     
      IF(IER.EQ.0) GO TO 46                                                     
      WRITE(6,45) IER                                                           
   45 FORMAT(1H0,'FEHLER',I4,'  DREIJ')                                         
      GO TO LBL                                                                 
   46 CK(L)=FAK2*DRJ*DRJ                                                        
   41 CONTINUE                                                                  
      DO 43 K=1,KMAX                                                            
      ZK=DFLOAT(K)                                                              
      CALL SECHSJ(1.,ZK,ZK+1.,ZL,ZL,ZL,SJ(K),IER)                               
      IF(IER.EQ.0) GO TO 43                                                     
      WRITE(6,37) IER                                                           
      GO TO LBL                                                                 
   43 CONTINUE                                                                  
      WRITE(6,13)                                                               
   13 FORMAT(1H-,'ALLE DIAGONALEN KOEFFIZIENTEN VON F0 WERDEN  =1.  GESE        
     1TZT UND DESHALB NICHT AUSGEDRUCKT')                                       
C                                                                               
      J=5                                                                       
      IF(LMAX.EQ.3.AND.JSTR.GT.1) J= 4                                          
      DO 8 I=2,NP                                                               
      N=5.1-(I-6)**2                                                            
      N=MAX0(2,N)                                                               
      PN1(I)=T(1:J)//'('//PN(I)(1:N)//')'                                       
    8 CONTINUE                                                                  
      IF((LMAX+JSTR).GT.3) THEN                                                 
      PN1(6)='C(ALPHA)'                                                         
         IF(LMAX.EQ.2) THEN                                                     
                    FMT1(2)='3(A,3X),A10,3(A,3X),A9)'                           
                    FMT1(5)='3F15.7,F11.3,F14.7,3F15.7)'                        
         END IF                                                                 
         IF(J.EQ.4) THEN                                                        
                    FMT1(3)='4(A,1X),A10,3(A,1X),A8)'                           
                    FMT1(6)='1X,4F13.6,F10.3,4F13.6)'                           
         END IF                                                                 
      END IF                                                                    
      FMT2(LMAX)='(1H0,17HMATRIXELEMENT VON,6X,'//FMT1(LMAX)                    
      FMT2(L1MX)='(1H ,A8,1X,A8,'//FMT1(L1MX)                                   
C                                                                               
      NPL=MIN0(JSTR,2)+4                                                        
      IF(LMAX.GT.1) NPL=NPL+LMAX+1                                              
      DO 30 I=3,NPL                                                             
      K=KZP(I-2,LMAX)                                                           
      PN(I)=PN(K)                                                               
      PN1(I)=PN1(K)                                                             
   30 CONTINUE                                                                  
C                                                                               
C                                                                               
      DO 100 M=1,MMAX                                                           
   21 XJ=XJ+1.                                                                  
      IF(XJ.GT.DMAX1(XMIN,XMAX)) GO TO 101                                      
      WRITE(6,12) M,XJ                                                          
   12 FORMAT(1H-,'SUBMATRIX',I2,10X,'J =',F4.1/1H ,11(1H=),10X,7(1H=))          
      IF(JSTR.LE.1) WRITE(6,22)                                                 
      IF(JSTR.GT.1) WRITE(6,23)                                                 
   22 FORMAT(1H0,'NR.',10X,'2S+1',4X,'L',7X,'SEN',5X,'2S1+1',4X,'L1',5X,        
     1'CORENR.',7X,'NAME')                                                      
   23 FORMAT(1H0,'NR.',10X,'2S+1',4X,'L',7X,'SEN',5X,'2S1+1',4X,'L1',5X,        
     1'CORENR.',6X,'S2',4X,'L2',10X,'NAME')                                     
      J=0                                                                       
      DO 26 I=1,NZ                                                              
      YJ=DABS(XS(I)-XL(I))                                                      
      ZJ=XS(I)+XL(I)                                                            
      IF(XJ.GT.ZJ.OR.XJ.LT.YJ) GO TO 26                                         
      J=J+1                                                                     
      S(J)=XS(I)                                                                
      CL(J)=XL(I)                                                               
      S1(J)=XS1(I)                                                              
      S2(J)=XS2(I)                                                              
      CL1(J)=XL1(I)                                                             
      CL2(J)=XL2(I)                                                             
      KZ(J,M)=I                                                                 
      GJ(J,M)=SLGJ(S(J),CL(J),XJ)                                               
      IS=2.*S(J)+1.1                                                            
      IS1=2.*S1(J)+1.1                                                          
      LL=CL(J)+1.1                                                              
      LL1=CL1(J)+1.1                                                            
      LL2=CL2(J)+1.1                                                            
      IF(JSTR.LE.1) WRITE(6,24)J,IS,Y(LL),IV(I),IS1,Y(LL1),NC(I),TNAM(I)        
      IF(JSTR.GT.1) WRITE(6,25)J,IS,Y(LL),IV(I),IS1,Y(LL1),NC(I),S2(J),         
     1Y(LL2),TNAM(I)                                                            
   24 FORMAT(1H ,I2,12X,I2,5X,A1,8X,I2,7X,I2,6X,A1,7X,I2,10X,A8)                
   25 FORMAT(1H ,I2,12X,I2,5X,A1,8X,I2,7X,I2,6X,A1,7X,I2,7X,F4.1,5X,A1,         
     110X,A8)                                                                   
   26 CONTINUE                                                                  
      IF(J.EQ.0) THEN                                                           
      WRITE(6,27) XJ                                                            
   27 FORMAT(1H0,'J =',F4.1,'  KANN VON KEINEM DER EINGEGEBENEN ZUSTAEND        
     1E GEBILDET WERDEN')                                                       
      GO TO 21                                                                  
      END IF                                                                    
      R(M)=J                                                                    
      WRITE(7,29) M,R(M),XJ                                                     
   29 FORMAT(2I6,20X,'UNTERMATRIX ZU  J =',F4.1)                                
      WRITE(6,FMT2(LMAX)) (PN1(I),I=2,NPL)                                      
C                                                                               
C                                                                               
      DO 98 JL=1,J                                                              
      DO 99 JK=JL,J                                                             
      IF((DABS(S(JL)-S(JK)).GT.1.).OR.(DABS(CL(JL)-CL(JK)).GT.1.)) GO TO        
     199                                                                        
      IF(DABS(S1(JL)-S1(JK)).GT.1.) GO TO 99                                    
      DO 35 I=1,NP                                                              
   35 C(I)=0.                                                                   
      MHOCH=S1(JL)+2.*S(JK)+CL1(JL)+XJ+S2(JL)+1.1                               
      NHOCH=S1(JL)+2.*S(JK)+CL1(JK)+XJ+S2(JL)+0.1                               
      CALL SECHSJ(S(JL),S(JK),1.,CL(JK),CL(JL),XJ,RES1,IER)                     
      IF(IER.EQ.0) GO TO 38                                                     
   36 WRITE(6,37) IER                                                           
   37 FORMAT(1H0,'FEHLER',I4,'  SECHSJ')                                        
      GO TO 99                                                                  
   38 CALL SECHSJ(S1(JL),S(JL),S2(JL),S(JK),S1(JK),1.,RES6,IER)                 
      IF(IER.NE.0) GO TO 36                                                     
      N1=KZ(JL,M)                                                               
      N2=KZ(JK,M)                                                               
      NC1=NC(N1)                                                                
      NC2=NC(N2)                                                                
      FAK=DSQRT((2.*S(JL)+1.)*(2.*S(JK)+1.))                                    
      C(5)=(-1)**MHOCH*FAK1*FAK*RES1*RES6*RME(NC1,NC2,4)                        
C                                                                               
      DO 2 I=1,NZ                                                               
      IF (S(JL).NE.XS(I).OR.S1(JL).NE.XS1(I)) GO TO 2                           
      K=NC(I)                                                                   
      DO 39 L=1,LMAX                                                            
      IF(S(JL).NE.S(JK).OR.CL(JL).NE.CL(JK)) GO TO 5                            
      IF(S1(JL).NE.S1(JK).OR.CL1(JL).NE.CL1(JK)) GO TO 5                        
      C(L+1)=C(L+1)+RME(NC1,K,L)*RME(NC2,K,L)                                   
    5 ZK=2.*L                                                                   
      KK=ZK+0.1                                                                 
      CALL SECHSJ(CL1(JL),CL1(JK),1.,ZK-1.,ZK,XL1(I),RES2,IER)                  
      IF(IER.NE.0) GO TO 36                                                     
      SUM=(2.*ZK-1.)*SJ(KK-1)*RES2*RME(K,NC2,L+3)                               
      IF(L.EQ.LMAX) GO TO 33                                                    
      CALL SECHSJ(CL1(JL),CL1(JK),1.,ZK+1.,ZK,XL1(I),RES3,IER)                  
      IF(IER.NE.0) GO TO 36                                                     
      SUM=(2.*ZK+3.)*SJ(KK)*RES3*RME(K,NC2,L+4)+SUM                             
   33 C(L+7)=C(L+7)+RME(NC1,K,L)*SUM                                            
   39 CONTINUE                                                                  
    2 CONTINUE                                                                  
C                                                                               
      FL=2.*CL1(JL)+1.                                                          
      DO 47 L=1,LMAX                                                            
      C(L+1)=C(L+1)*CK(L)/FL                                                    
   47 C(L+7)= 2.*FAK1* (-1)**NHOCH*RES1*RES6*FAK*C(L+7)                         
      IF(JK.NE.JL) GO TO 9                                                      
      C(1)=1.                                                                   
      C(6)=CL1(JL)*(CL1(JL)+1.)                                                 
      IF(ZL.EQ.2.) C(7)=IV(N1)*(1.-IV(N1)/12.D0)-S1(JL)*(S1(JL)+1.)/3.D0        
      IF(JSTR.GT.1)C(12)=(DSIGN(2*S1(JL)+1,S1(JL)-S(JL))-NKONF+1)*.5/ZLL        
      DO 48 L=1,LMAX                                                            
   48 C(L+1)=C(L+1)-NKONF*CK(L)/ZLL                                             
    9 DO 50 I=3,NPL                                                             
      K=KZP(I-2,LMAX)                                                           
      C(I)=C(K)                                                                 
   50 CONTINUE                                                                  
      WRITE(6,FMT2(L1MX)) TNAM(N1),TNAM(N2),(C(I),I=2,NPL)                      
      NL=JL+N0(M)                                                               
      NK=JK+N0(M)                                                               
      DO 44 I=1,NPL                                                             
      N=I+KENP                                                                  
      IF(DABS(C(I)).GE.1.D-6)  WRITE(7,42) M,NL,NK,N,C(I),Y(LMAX+1),            
     1NKONF,TNAM(N1),TNAM(N2),PN(I)                                             
   42 FORMAT(4I6,D12.6,3X,A1, I2,2X,A8,2X,A8,6X,A8)                             
   44 CONTINUE                                                                  
   99 CONTINUE                                                                  
   98 CONTINUE                                                                  
C                                                                               
C                                                                               
      WRITE(6,3)                                                                
    3 FORMAT(1H0)                                                               
      WRITE(7,1)                                                                
    1 FORMAT(1H )                                                               
  100 CONTINUE                                                                  
C                                                                               
C                                                                               
  101 MMAX=M+XMAX-XJ-0.9                                                        
      DO 110 M=1,MMAX                                                           
      RM=R(M)                                                                   
      WRITE(7,102) (GJ(J,M),J=1,RM)                                             
  102 FORMAT(6F12.8,8X)                                                         
  110 CONTINUE                                                                  
      DO 120 M=1,MMAX                                                           
      IE=R(M)                                                                   
      DO 116 I=1,IE                                                             
      N1=KZ(I,M)                                                                
      NC1=NC(N1)                                                                
      WRITE(7,112) TNAM(N1),XS(N1),XL(N1),IV(N1),XS1(N1),XL1(N1),NC1,XS2        
     1(N1),XL2(N1),NKONF,M                                                      
  112 FORMAT(2X,A8,6X,3(F4.1,4X,F4.1,5X,I3),I2,2X)                              
  116 CONTINUE                                                                  
  120 CONTINUE                                                                  
  300 STOP                                                                      
      END                                                                       
                                                                                                               