C										
C										
C     PROGRAMM PERJ48 ZUR BERECHNUNG AUSSERDIAGONALER HFS- UND ZEEMAN-STOERUNGEN
C     DURCH  <= 48  FEINSTRUKTURZUSTAENDE MIT  <= 48 CORES			
C										
      IMPLICIT REAL*8(A-H,O-Z)							
      DIMENSION EM(2),F(2),A(48,48),B(48,48),VALU(48),ERG(48),EN(48),		
     1VCF(2,48),NOB(2),QP(48,2),X(7),EGG(2),QWP(48)				
      DIMENSION S2B(48),S2K(48,48),HH(48),PT(2),ENY(48),PH1B(48),		
     1PH2B(48),PH3B(48),VCB(48),ESB(48),ELB(48),S1B(48),EL1B(48),		
     2NCB(48),XS1(48),XS2(48),XS3(48),EJ(48),NOPHK(48),PH1K(48,48),		
     3PH2K(48,48),PH3K(48,48),VCK(48,48),ESK(48,48),ELK(48,48),S1K(48,		
     448),EL1K(48,48),NCK(48,48),OHO(48),AN(1176),RN(2304),NG6(48)		
      DIMENSION PTFF(2),PTFH(2),PTHH(2),NDKONF(3),CONFIG(3)			
      DIMENSION AAL1(9),AASC1(9),AAS1(9),AAS2(9),BBL1(9),BB13(9),BB11(9)	
      REAL*8 U2(48,48),V12(48,48),V13(48,48),V11(48,48),VV,UU,WW,XX		
      INTEGER SENB(48),SENK(48,48),NCORB(48),NCORK(48,48),OP1,OP2,OP3		
      INTEGER NZ1(48),NZ2(48),KZKET(200)					
      DATA KZKET/200*0/, NOK/0/, NON/0/, NKO/0/ 				
      DATA X/'S','P','D','F','G','H','I'/					
      CALL COFRAP								
      READ(5,19) (QWP(I),I=1,19)						
   19 FORMAT(19A4)								
      WRITE(6,24) (QWP(I),I=1,19)						
   24 FORMAT(1H1,19A4)								
      READ(5,1) KEZ,MAZ,NEZ,N0,PS1,PS2,PS3,AJ,EYE,ENG,NOC,NOPHB,OP1,OP2,	
     1EVMINB,EVMINK								
    1 FORMAT(I1,3I3,A2,2A4,2F5.1,F10.2,4I5,5X,2D5.1)				
      IF(NOC.EQ.0) NOC=99							
      IF(EYE.EQ.0.) OP1=0							
      IF(EYE.NE.0.) OP3=1							
      LD=5									
      IF(KEZ.EQ.0) GO TO 80							
      LD=12									
      DJ=AJ									
      ED=ENG									
      IF(DJ.EQ.0.) DJ=2.							
      IF(EYE.EQ.0.)DJ=DMIN1(DJ,1.D0)						
      IF(ED.EQ.0.) ED=1.D6							
      IF(N0.EQ.0) N0=1								
      NZ=IABS(N0)								
      IF(KEZ.GE.2) READ(5,6) (NZ1(I),NZ2(I),I=1,NZ)				
    6 FORMAT(10(2I2,4X))							
   71 READ(12,5,END=100) PS1,PS2,PS3,AJ,ENG,NOPHB,MBR,NBR			
      IF(MBR.NE.MAZ.OR.NBR.NE.NEZ) GO TO 71					
   80 WRITE(6,81)PS1,PS2,PS3,AJ,EYE,ENG,NOC,NOPHB,OP1,OP2,OP3			
      IF((EVMINB.LE.0.).AND.(EVMINK.LE.0.)) WRITE(6,68) 			
      IF((EVMINB.GT.0.).OR.(EVMINK.GT.0.))  WRITE(6,69) EVMINB,EVMINK		
   81 FORMAT(1H0,'BRA:',2X,A2,A4,A4,5X,'J=',F4.1,2X,'I=',F4.1,5X,		
     1'ENG=',F10.1,5X,'NOC=',I2,5X,'NOPHB=',I2,5X,'OP1=',I2,4X,'OP2=',I2	
     2,4X,'OP3=',I2)								
   68 FORMAT(1H )								
   69 FORMAT(1H+,T116,'VCMIN(BRA)=',F7.4/1H ,T116,'VCMIN(KET)=',F7.4)		
      READ(5,2) AY,BE,GJ,GI							
    2 FORMAT(10X,7(F10.6))							
      WRITE(6,82) AY,BE,GJ,GI							
   82 FORMAT(1H ,'A=',F12.5,2X,'B=',F12.5,5X,'GJ=',F10.6,2X,'GI=',F9.6) 	
      KZA=9									
      I=1									
      DO 25 K=1,NOPHB								
      READ(LD,3)VCB(I),ESB(I),ELB(I),SENB(I),S1B(I),EL1B(I),NCB(I),		
     1S2B(I),NCORB(I)								
    3 FORMAT(F16.1,2F8.1,I4,2F8.1,I4,F8.1,8X,I4,2I2)				
      IF(DABS(VCB(I)).LT.EVMINB) GO TO 25					
      KZA=MIN0(NCORB(I),KZA)							
      NKO=MAX0(NCORB(I),NKO)							
      NON=MAX0(NON,NCB(I))							
      IXZU=ELB(I)+1.1D0 							
      ISZU=2.0D0*ESB(I)+1.1D0							
      IF(S2B(I).GT.0.2D0) GO TO 2877						
      WRITE(6,8) I,ISZU,X(IXZU),AJ,VCB(I),ESB(I),ELB(I),S1B(I),EL1B(I), 	
     1NCB(I),SENB(I),S2B(I),NCORB(I)						
    8 FORMAT(1H0,3X,'BRA VEC(',I2,'):',1X,I1,A1,F4.1,8X,			
     1'VC=',F9.6,3X,'S=',F4.1,3X,'L=',F4.1,3X,'S1=',F4.1,3X,'L1=',		
     2F4.1,3X,'NC=',I2,2X,'SEN=',I2,2X,'S2=',F4.1,3X,'KONF: D HOCH',I2) 	
      GO TO 23									
 2877 IS1ZU=2.0D0*S1B(I)+1.1D0							
      WRITE(6,2878) I,ISZU,X(IXZU),AJ,IS1ZU,X(IXZU),VCB(I),ESB(I),		
     1ELB(I),S1B(I),EL1B(I),NCB(I),SENB(I),S2B(I),NCORB(I)			
 2878 FORMAT(1H0,3X,'BRA VEC(',I2,'):',1X,I1,A1,F4.1,' (',I1,A1,')',3X, 	
     1'VC=',F9.6,3X,'S=',F4.1,3X,'L=',F4.1,3X,'S1=',F4.1,3X,'L1=',		
     2F4.1,3X,'NC=',I2,2X,'SEN=',I2,2X,'S2=',F4.1,3X,'KONF: D HOCH',I2, 	
     3'  S')									
   23 I=I+1									
   25 CONTINUE									
      NELVEC=NOPHB-I+1								
      IF(NELVEC.GT.0) WRITE(6,89) NELVEC					
      NOPHB=I-1 								
      IF(KEZ.EQ.0) GO TO 78							
      REWIND 12 								
      K=0									
   72 K=K+1									
      READ(12,73,END=77)A1J,ENER,NUT,KZ1,KZ2					
   73 FORMAT(25X,F5.1,F10.2,8X,I2,26X,2I2)					
      IF(KEZ.GE.3) GO TO 83							
      IF((KZ1.NE.MAZ.OR.KZ2.NE.NEZ).AND.(DABS(ENER-ENG).LE.DABS(ED).OR. 	
     1(ENER.LT.ENG.AND.ED.LT.0.)).AND.DABS(A1J-AJ).LE.DABS(DJ)) 		
     2KZKET(K)=1								
      IF(DABS(A1J-AJ).NE.-DJ.AND.DJ.LT.0.) KZKET(K)=0				
      IF(KEZ.EQ.1) GO TO 84							
   83 DO 74 I=1,NZ								
   74 IF(NZ1(I).EQ.KZ1.AND.NZ2(I).EQ.KZ2) KZKET(K)=0.6+N0/(2.*NZ)		
   84 DO 76 L=1,NUT								
      READ(12,75) EVKO,NRCOR							
   75 FORMAT(F16.8,38X,I2)							
      IF(KZKET(K).EQ.0.OR.DABS(EVKO).LT.EVMINK) GO TO 76			
      NON=MAX0(NON,NRCOR)							
   76 CONTINUE									
      IF(KZKET(K).EQ.1) NOK=NOK+1						
      GO TO 72									
   77 NUCONF=NKO-KZA+1								
      NOAK=K-1									
      REWIND 12 								
      GO TO 79									
C										
   78 READ(5,4) NOK,NON,NUCONF							
    4 FORMAT(8(8X,I2))								
      DO 85 K=1,NOK								
   85 KZKET(K)=1								
      NOAK=NOK									
   79 WRITE(6,86) NOK,NON,NUCONF						
   86 FORMAT(1H-,'NOK=',I2,5X,'NON=',I2,5X,'NUCONF=',I2)			
      IF(NOK.LE.48) GO TO 88							
      WRITE(6,87)								
   87 FORMAT(1H0,'MEHR ALS  48  STOERENDE ZUSTAENDE EINGEGEBEN, DIE RECH        
     1NUNG WIRD DESHALB ABGEBROCHEN')                                           
      GO TO 4000								
   88 IF(EYE.EQ.0.) GO TO 90							
      READ(5,2801)(CONFIG(K),K=1,3)						
 2801 FORMAT(3(A8,2X))								
      DO 2701 KLAUF=1,NUCONF							
      READ(5,2702) K,AAL1(K),AASC1(K),AAS1(K),AAS2(K),BBL1(K),BB13(K),		
     1BB11(K)									
 2702 FORMAT(I2,8X,7(F10.6))							
      NDKONF(KLAUF)=K								
 2701 CONTINUE									
   90 BL=2.									
      PL=0.									
      I=1									
      DO 13 K=1,NOAK								
      READ(LD,5)XS1(I),XS2(I),XS3(I),EJ(I),ENY(I),NUT,JKZ,LEVKZ 		
    5 FORMAT(10X,A2,A4,A4,5X,F5.1,F10.2,8X,I2,26X,2I2)				
      IF(KZKET(K).EQ.1) WRITE(6,7)XS1(I),XS2(I),XS3(I),EJ(I),ENY(I),NUT,	
     1JKZ,LEVKZ 								
    7 FORMAT(1H-,'KET:',A2,A4,A4,5X,'J=',F4.1,5X,'ENG=',F8.1,			
     15X,'NVC=',I2,7X,'ZUSTAND ',2I3)						
      L=1									
      DO 26 J=1,NUT								
      READ(LD,3)VCK(I,L),ESK(I,L),ELK(I,L),SENK(I,L),S1K(I,L),EL1K(I,L),	
     1NCK(I,L),S2K(I,L),NCORK(I,L),JKZ1,LKZ1					
      IF(KZKET(K).EQ.0.OR.DABS(VCK(I,L)).LT.EVMINK) GO TO 26			
      IXZU=ELK(I,L)+1.1D0							
      ISZU=2.0D0*ESK(I,L)+1.1D0 						
      IF(S2K(I,L).GT.0.2D0) GO TO 2879						
      WRITE(6,9) I,L,ISZU,X(IXZU),						
     1EJ(I),VCK(I,L),ESK(I,L),ELK(I,L),S1K(I,L),EL1K(I,L),NCK(I,L),		
     2SENK(I,L),S2K(I,L),NCORK(I,L)						
    9 FORMAT(1H0,3X,'KET VEC(',I2,',',I2,'):',I2,A1,F4.1,7X,			
     1'VC=',F9.6,3X,'S=',F4.1,3X,'L=',F4.1,3X,'S1=',F4.1,3X,'L1=',		
     2F4.1,3X,'NC=',I2,2X,'SEN=',I2,2X,'S2=',F4.1,3X,'KONF: D HOCH',I2) 	
      GO TO 258 								
 2879 IS1ZU=2.0D0*S1K(I,L)+1.1D0						
      WRITE(6,2888) I,L,ISZU,X(IXZU),EJ(I),IS1ZU,X(IXZU),			
     1VCK(I,L),ESK(I,L),ELK(I,L),S1K(I,L),EL1K(I,L),NCK(I,L),SENK(I,L), 	
     2S2K(I,L),NCORK(I,L)							
 2888 FORMAT(1H0,3X,'KET VEC(',I2,',',I2,'):',I2,A1,F4.1,'(',I1,A1,')', 	
     13X,'VC=',F9.6,3X,'S=',F4.1,3X,'L=',F4.1,3X,'S1=',F4.1,3X,'L1=',		
     2F4.1,3X,'NC=',I2,3X,'SEN=',I2,3X,'S2=',F4.1,3X,'KONF: D HOCH',I2, 	
     3'  S')									
  258 IF(JKZ.NE.JKZ1.OR.LEVKZ.NE.LKZ1) WRITE(6,259)				
  259 FORMAT(' EIGENVEKTORKOMPONENTE GEHOERT NICHT ZU DIESEM ZUSTAND')		
      L=L+1									
   26 CONTINUE									
      IF(KZKET(K).EQ.0) GO TO 13						
      NELVEC=NUT-L+1								
      IF(NELVEC.GT.0) WRITE(6,89) NELVEC					
   89 FORMAT(1H0,'ZAHL DER ELIMINIERTEN EV-KOMPONENTEN =',I3)			
      NOPHK(I)=L-1								
      I=I+1									
   13 CONTINUE									
      IF(EYE.EQ.0.) GO TO 3000							
C										
C     BERECHNUNG DER REDUZIERTEN MATRIXELEMENTE 				
      DO 27 K=1,48								
      DO 27 L=1,48								
      V12%L,K<#0.								
      U2%L,K<#0.								
      V11%L,K<#0.								
      V13%L,K<#0.								
   27 CONTINUE									
      IF(OP2.EQ.1) WRITE(6,101) 						
  101 FORMAT(1H-,11X,'U2',18X,'V12',17X,'V13',17X,'V11')			
      KTEST=0									
      DO 102 J5=1,NOPHB 							
      L#NCB%J5< 								
      DO 103 J6#1,NOK								
      NOKK#NOPHK%J6<								
      DO 103 J7#1,NOKK								
      K#NCK%J6,J7<								
      IF%NCORB%J5<.NE.NCORK%J6,J7<< GO TO 103					
      KONF=NCORB(J5)								
      DS1B=S1B(J5)								
      DEL1B=EL1B(J5)								
      ISENB=SENB(J5)								
      DS1K=S1K(J6,J7)								
      DEL1K=EL1K(J6,J7) 							
      ISENK=SENK(J6,J7) 							
      CALL REDMAT(KONF,KTEST,DS1B,DEL1B,ISENB,DS1K,DEL1K,ISENK, 		
     1VV,UU,WW,XX)								
      V12(L,K)=VV								
      U2(L,K)=UU								
      V11(L,K)=WW								
      V13(L,K)=XX								
  103 CONTINUE									
  102 CONTINUE									
      IF(OP2.NE.1) GO TO 105							
      DO 104 L#1,NON								
      DO 104 K#1,NON								
      WRITE%6,106<L,K,U2%L,K<,V12%L,K<,V13%L,K<,V11%L,K<			
  106 FORMAT(1X,2(I2,3X),4(D16.9,4X))						
  104 CONTINUE									
  105 WRITE(6,68)								
      DO 2705 KLAUF=1,NUCONF							
      WRITE(6,2707)CONFIG(KLAUF)						
 2707 FORMAT(1H0,2X,'CONFIGURATION:  ',A8)					
      K=NDKONF(KLAUF)								
      WRITE(6,2706) AAL1(K),AASC1(K),AAS1(K),AAS2(K),BBL1(K),			
     1BB13(K),BB11(K)								
 2706 FORMAT(1H ,'A01(D)=',F9.3,4X,'A12(D)=',F9.3,4X,'A10(D)=', 		
     1F9.3,4X,'A10(S)=',F9.3,4X,'B02=',F9.3,4X,'B13=',F9.3,4X,			
     2'B11=',F9.3)								
      IF(K.GE.KZA.AND.K.LE.NKO.AND.K.LT.10) GO TO 2705				
      WRITE(6,2703) K								
 2703 FORMAT(1H ,'FALSCHE ODER IM BRA-EIGENVEKTOR NICHT AUFTRETENDE KONF        
     1IGURATION  D',I2,'  AUF RADIALPARAMETERKARTE')                            
      GO TO 4000								
 2705 CONTINUE									
 3000 READ(5,22,END=4000) K,H,F(1),EM(1),F(2),EM(2)				
   22 FORMAT(I2,10X,F10.3,22X,4F6.1)						
      IF(KEZ.NE.0.AND.IABS(K-5).LT.5) GO TO 3000				
C										
C     BERECHNUNG DER ENERGIEMATRIX						
      DO 1000 M=1,2								
      IF(EYE.NE.0.) GO TO 34							
      NE=1									
      II=0									
      IAJ=2.*AJ+.1								
      IM=2.*EM(M)+DSIGN(0.2D+00,EM(M))						
      F(M)=AJ									
      VCF(M,1)=1.								
      EGG(M)=EM(M)*GJ*H*1.3996108						
      GO TO 119 								
   34 DO 36 LP=1,48								
      DO 36 LQ=1,48								
   36 A(LP,LQ)=0.								
      EMM#DABS%EM%M<<								
      IF%EMM.LE.DABS%EYE-AJ<<GO TO 20						
      NE#IDINT%EYE&AJ-EMM&1.1<							
      GO TO 30									
   20 NE#IDINT%EYE&AJ-DABS%EYE-AJ<&1.1< 					
   30 EF=EYE+AJ+1.								
      NOB(M)=NE 								
      DO 40 IQ=1,NE								
      EF=EF-1.									
      QOP=0.									
      CAY=EF*(EF+1.)-AJ*(AJ+1.)-EYE*(EYE+1.)					
      IF(AJ.LE..7.OR.EYE.LE..7) GO TO 35					
      QOP=.375*CAY*(CAY+1.)/AJ/EYE/(2.*AJ-1.)/(2.*EYE-1.)			
   35 ERG(IQ)=AY*.5*CAY+QOP*BE							
      IF(EF.EQ.F(M)) ERGO=ERG(IQ)						
      IF1=2.*EF+.1								
      IF2=IF1-2 								
      IAJ=2.*AJ+.1								
      II=2.*EYE+.1								
      IM=2.*EM(M)+DSIGN(0.2D+00,EM(M))						
      A(IQ,IQ)=ZM(IF1,IF1,II,IAJ,IM,GJ,GI,H)+ERG(IQ)				
      IF(IQ.EQ.NE) GO TO 40							
      A(IQ,IQ+1)=ZM(IF1,IF2,II,IAJ,IM,GJ,GI,H)					
      A(IQ+1,IQ)=A(IQ,IQ+1)							
   40 CONTINUE									
      DO 43 L1=1,NE								
      K=0									
      D=ERG(L1) 								
      DO 42 L2=1,NE								
      IF(D-ERG(L2))41,41,42							
   41 K=K+1									
   42 CONTINUE									
      EN(K)=ERG(L1)								
   43 IF(EN(K).EQ.ERGO) IMP=K							
      IF(OP1.EQ.0) GO TO 60							
      WRITE(6,61)								
   61 FORMAT(1H0,14HENERGY MATRIX: )						
      DO 32 LX=1,NE								
      WRITE(6,117) (A(LX,LU),LU=1,NE)						
   32 CONTINUE									
  117 FORMAT(1H0,5(F13.6,5X))							
   60 CONTINUE									
      DO 812 NG5=1,1176 							
  812  AN(NG5)=0.								
      NG3=1									
      NG4=1									
      DO 810 NG1=1,NE								
      DO 811 NG2=1,NG4								
      IF(NG1.EQ.NG2) NG6(NG1)=NG3						
      AN(NG3)=A(NG2,NG1)							
      NG3=NG3+1 								
  811 CONTINUE									
      NG4=NG4+1 								
  810 CONTINUE									
      CALL DEIGEN(AN,RN,NE,0)							
      DO 813 NG7=1,NE								
      NG8=NG6(NG7)								
      VALU(NG7)=AN(NG8) 							
  813 CONTINUE									
      NG11=0									
      DO 814 NG9=1,NE								
      DO 815 NG10=1,NE								
      NG11=NG11+1								
      A(NG10,NG9)=RN(NG11)							
  815 CONTINUE									
  814 CONTINUE									
      IF(OP1.EQ.0) GO TO 65							
      WRITE(6,62)								
   62 FORMAT(1H0,20HEIGENVECTOR COLUMNS: )					
      DO 37 LX=1,NE								
      WRITE(6,117) (A(LX,LU),LU=1,NE)						
   37 CONTINUE									
   65 DO 48 LK=1,NE								
   48 VCF(M,LK)=A(LK,IMP)							
      EGG(M)=VALU(IMP)								
  119 INDF=NE+2 								
C										
C     BERECHNUNG DER STOERUNGEN 						
  120 S=0.									
      SFF=0.									
      SFH=0.									
      SHH=0.									
      DO 600 I=1,NOK								
      FF=AJ+EYE+2.								
      DEN=(ENG-ENY(I))*29979.2458D0						
      PAR#0.									
  140 DO 500 L=1,INDF								
      FF=FF-1.									
      IF%FF.LE.%EJ%I<&EYE<.AND.FF.GE.DABS%EJ%I<-EYE<< GO TO 160 		
      GO TO 500 								
  160 U#0.									
      UFF=0.									
      UHH=0.									
      FEF=AJ+EYE+1.								
  180 DO 400 NF=1,NE								
      FEF=FEF-1.								
      IF%FF.EQ.FEF.OR.DABS%FF-FEF<.EQ.1.< GO TO 200				
      GO TO 400 								
C										
  200 DO 401 NB=1,NOPHB 							
      S2#S2B%NB<								
      NAP=NOPHK(I)								
      DO 402 NP=1,NAP								
      IF(NCORB(NB).NE.NCORK(I,NP)) GO TO 402					
      IF(EYE.EQ.0.) GO TO 250							
      NCONF=NCORB(NB)								
      AL1=AAL1(NCONF)								
      ASC1=AASC1(NCONF) 							
      AS1=AAS1(NCONF)								
      AS2=AAS2(NCONF)								
      BL1=BBL1(NCONF)								
      B13=BB13(NCONF)								
      B11=BB11(NCONF)								
  250 IFF=2.*FF+.01								
      IFEF=2.*FEF+.01								
      IEJ=2.*EJ(I)+.01								
      ISB=2.*ESB(NB)+.01							
      LB=2.*ELB(NB)+.01 							
      L1B=2.*EL1B(NB)+.01							
      IS1B=2.*S1B(NB)+.01							
      ISK=2.*ESK(I,NP)+.01							
      LK=2.*ELK(I,NP)+.01							
      L1K=2.*EL1K(I,NP)+.01							
      IS1K=2.*S1K(I,NP)+.01							
      NC1=NCB(NB)								
      NC2=NCK(I,NP)								
      IPL=0									
      IBL=2.*BL+.01								
      IS2=2.*S2+.01								
      IF(IFF.NE.IFEF.OR.EYE.EQ.0.) GO TO 300					
      UU2=U2(NC1,NC2)								
      VV2=V12(NC1,NC2)								
      VV13=V13(NC1,NC2) 							
      VV11=V11(NC1,NC2) 							
      HFSC=HFS(IAJ,IEJ,II,IFEF,LB,LK,ISB,ISK,L1B,L1K,IS1B,IS1K, 		
     1IS2,IPL,IBL,AL1,ASC1,AS1,AS2,BL1,B13,B11,UU2,				
     2VV2,VV13,VV11,NC1,NC2)							
      GO TO 350 								
  300 HFSC#0.									
  350 IF(NC1.NE.NC2.OR.LB.NE.LK.OR.ISB.NE.ISK) GO TO 360			
      GO TO 380 								
  360 ZE=0.									
      GO TO 390 								
  380 ZE=ZMN(IFEF,IFF,IM,II,IAJ,IEJ,LB,ISB,H)					
  390 W=(HFSC+ZE)*VCF(M,NF)*VCB(NB)*VCK(I,NP)					
      WFF=ZE*VCF(M,NF)*VCB(NB)*VCK(I,NP)					
      WHH=HFSC*VCF(M,NF)*VCB(NB)*VCK(I,NP)					
      U=U+W									
      UFF=UFF+WFF								
      UHH=UHH+WHH								
      IF(OP1.EQ.0) GO TO 70							
      WRITE(6,211) FEF,FF,EJ(I),ESB(NB),ELB(NB),S1B(NB),EL1B(NB),ESK(I, 	
     1NP),ELK(I,NP),S1K(I,NP),EL1K(I,NP)					
  211 FORMAT(1H0,2HF=,F4.1,3X,3HFP=,F4.1,3X,3HJP=,F4.1,3X,3HSB=,F3.1,3X,	
     13HLB=,F3.1,3X,4HS1B=,F3.1,3X,4HL1B=,F3.1,3X,3HSK=,F3.1,3X,		
     23HLK=,F3.1,3X,4HS1K=,F3.1,3X,4HL1K=,F3.1) 				
      WRITE(6,116) ZE,HFSC,W							
  116 FORMAT(1H0,2HZ=,F12.5,5X,3HHF=,F12.5,5X,4HMEL=,F12.5)			
   70 CONTINUE									
  402 CONTINUE									
  401 CONTINUE									
  400 CONTINUE									
      S=S+U*U/DEN								
      SFF=SFF+UFF*UFF/DEN							
      SFH=SFH+2.*UFF*UHH/DEN							
      SHH=SHH+UHH*UHH/DEN							
      PAR=PAR+U*U/DEN								
  500 CONTINUE									
      QP(I,M)=PAR								
  600 CONTINUE									
      PT(M)=S									
      PTFF(M)=SFF								
      PTFH(M)=SFH								
      PTHH(M)=SHH								
 1000 CONTINUE									
C										
      WRITE(6,112) H,F(1),EM(1),F(2),EM(2),PS1,PS2,PS3,AJ,ENG			
  112 FORMAT(1H1,2HH=,F10.4,5X,3HF1=,F5.1,5X,3HM1=,F5.1,5X,3HF2=,F5.1,		
     15X,3HM2=,F5.1,12X,6HBRA:	,A2,2A4,5X,2HJ=,F4.1,5X,4HENG=,F10.3)		
      DEG=EGG(1)-EGG(2) 							
      WRITE(6,114) DEG,EGG(1),EGG(2)						
  114 FORMAT(1H0,3HNU=,F14.8,5X,3HE1=,F10.4,5X,3HE2=,F10.4)			
      IF(EYE.EQ.0.) GO TO 115							
      DO 91 L=1,2								
      JJ=NOB(L) 								
      WRITE(6,113) (VCF(L,KP),KP=1,JJ)						
  113 FORMAT(1H0,5HVCF: ,5(F12.6,5X))						
   91 CONTINUE									
  115 PTB=PT(1)-PT(2)								
      PTBFF=PTFF(1)-PTFF(2)							
      PTBFH=PTFH(1)-PTFH(2)							
      PTBHH=PTHH(1)-PTHH(2)							
      WRITE(6,111)PTB,PT(1),PT(2)						
  111 FORMAT(1H0,4HPTB=,F12.6,5X,4HPT1=,F12.6,5X,4HPT2=,F12.6)			
      IF(OP3.EQ.1) WRITE(6,121) PTBFF,PTFF(1),PTFF(2)				
      IF(OP3.EQ.1) WRITE(6,122) PTBFH,PTFH(1),PTFH(2)				
      IF(OP3.EQ.1) WRITE(6,123) PTBHH,PTHH(1),PTHH(2)				
  121 FORMAT(1H0,16HPTB(FELD-FELD) =,F10.6,6X,4HPT1=,F10.6,5X,4HPT2=,		
     1F10.6)									
  122 FORMAT(1H ,16HPTB(FELD-HFS ) =,F10.6,6X,4HPT1=,F10.6,5X,4HPT2=,		
     1F10.6)									
  123 FORMAT(1H ,16HPTB(HFS - HFS) =,F10.6,6X,4HPT1=,F10.6,5X,4HPT2=,		
     1F10.6)									
      DO 29 I=1,NOK								
      DD=ENG-ENY(I)								
      PAX=QP(I,1)-QP(I,2)							
      WRITE(6,50) XS1(I),XS2(I),XS3(I),DD,ENY(I),EJ(I)				
   50 FORMAT(1H0,4HKET:,A2,A4,A4,5X,3HDE=,F10.3,5X,4HENG=,F10.3,5X,2HJ=,	
     1F4.1)									
      WRITE(6,51) PAX,QP(I,1),QP(I,2)						
   51 FORMAT(1H0,10X,6HPERTB=,F11.6,5X,3HP1=,F11.6,5X,3HP2=,F11.6)		
   29 CONTINUE									
      GO TO 3000								
  100 WRITE(6,99) MAZ,NEZ							
   99 FORMAT(1H0,'EINGABEFEHLER, GEWUENSCHTER BRA-ZUSTAND  (',I2,',',I2,	
     1')  IST IM DATENSATZ NICHT ENTHALTEN')					
 4000 STOP									
      END									
