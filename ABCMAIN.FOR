C                                                                               
C                                                                               
C     PROGRAMM   A B C F A K                                                    
C                ===========                                                    
C                                                                               
C                                                                               
C     BERECHNUNG DER A-,B- UND C-FAKTOREN ALS LINEARKOMBINATION DER             
C     EFFEKTIVEN RADIALPARAMETER IN MITTLERER KOPPLUNG FUER                     
C     KONFIGURATIONEN VOM TYP (D+S) HOCH N                                      
C                                                                               
C     AUTOREN: S.BUETTGENBACH, M.HERSCHEL UND F.TRAEBER                         
C     MAERZ  1982                                                               
C                                                                               
C     BESTEHEND AUS HAUPTPROGRAMM UND DEN UNTERPROGRAMMEN:                      
C                                                                               
C     AFAK, BFAK,CFAK (BEITRAG ZUM A-,B- BZW. C-FAKTOR VON MATRIXELEMENT        
C                      ZWISCHEN ZWEI SL-ZUSTAENDEN)                             
C     FRAPAR  (MIT DEN ENTRIES  RMECFP  UND  REDMAT) , ATDATA                   
C     (BERECHNUNG DER REDUZ. MATRIXELEMENTE  V11, V12, V13, V14, U2, U3)        
C     PARENT (BERECHNUNG VON CFP'S)                                             
C     DREIJ, SECHSJ, NEUNJ, DFAKUL                                              
C     DATENKARTEN NCFP/ICFP (BESTANDTEIL DES KARTENDECKS)                       
C                                                                               
C                                                                               
C     REGION = 768K                                                             
C     CARDS = 500                                                               
C                                                                               
C     WENN KEINE LSQFIT-KOEFFIZIENTENKARTEN GESTANZT WERDEN SOLLEN, ZUSAETZLICHE
C     STEUERKARTE VOR DEN JOB :  //GO.FT07F001 DD DUMMY                         
C                                                                               
C                                                                               
C     EINGABE                                                                   
C     BELIEBIG VIELE DATENSAETZE BESTEHEND AUS KARTE 1 UND NE KARTEN            
C     VOM TYP 2.                                                                
C     ERSTE DATENKARTE HINTER LETZTE ICFP/NCFP-KARTE                            
C                                                                               
C     KARTE    SPALTE  FORMAT  BEMERKUNG                                        
C        1     1-4     I       NE=ZAHL DER EIGENVEKTORKOMPONENTEN DES           
C                              ZUSTANDES <=48                                   
C              5-8     F       DREHIMPULS J DES ZUSTANDES                       
C              18      I       LABEL1=1=> AUSDRUCK DER SL-HFS-                  
C                                         KOEFFIZIENTEN,SONST LEER              
C              19      I       LABEL2=1=> AUSDRUCK DER REDUZIERTEN              
C                                         MATRIXELEMENTE,SONST LEER             
C              20      I       KTEST =1=> AUSDRUCK ALLER CFP-KOEFFIZIEN-        
C                                         TEN FUER D-ELEKTRONEN UND DER         
C                                         REDUZIERTEN MATRIXELEMENTE DER        
C                                         KONFIGURATION, SONST LEER             
C              21-80   A       BELIEBIGER TEXT                                  
C                                                                               
C     JE EINE KARTE VOM TYP 2 PRO EIGENVEKTORKOMPONENTE, D.H. NE KARTEN         
C     VOM TYP 2                                                                 
C                                                                               
C        2     1-16    F       AMPLITUDE DER EIGENVEKTORKOMPONENTE              
C              17-24   F       GESAMTSPIN S                                     
C              25-32   F       GESAMTBAHNDREHIMPULS L                           
C              33-36   I       SENIORITY         )  DES CORE-                   
C              37-44   F       SPIN S1           )  ZUSTANDES                   
C              45-52   F       BAHNDREHIMPULS L1 )  D HOCH N                    
C              53-56   I       NCOR <= NE: NUMMER DES CORE-ZUSTANDES            
C              57-64   F       S2=0.5 FUER D HOCH N S                           
C                                =0.0 FUER D HOCH N UND D HOCH N S HOCH2        
C              76      I       N = ANZAHL DER D-ELEKTRONEN                      
C                                                                               
C     AUSGABE                                                                   
C     EINGABEDATEN, BENUTZTE V11, V12, V13, V14, U2, U3,  A-, B- UND C-         
C     FAKTOREN ALS LINEARKOMBINATION DER RADIALPARAMETER, BEITRAEGE DER         
C     EINZELNEN MATRIXELEMENTE ZWISCHEN SL-ZUSTAENDEN                           
C                                                                               
C                                                                               
      IMPLICIT REAL*8 (A-H,O-Z)                                                 
      REAL*4 INFO(15),Y(7),Y1(3),Y2                                             
      REAL*4 BLSTR(3),SSTR(3)                                                   
      INTEGER SEN(48)                                                           
      DIMENSION ALPHA(48),BL(48),S1(48),BL1(48),BL2(48),S2(48),V12(48,48        
     1),NCOR(48),A(48,48,9),S(48),B(48,48,9),AF(6),BF(6),U2(48,48),V11(4        
     28,48),V13(48,48),NCORKO(48),KCORKO(3),AX(3,6),BX(3,6),CX(3,6),U3(4        
     38,48),V14(48,48),CF(6),C(48,48,9),AOD(3),BOD(3),ASL(9),BSL(9),            
     4CSL(9),NKO(3),KZ(48),ISSTR(3),KZF(48,48)                                  
      DIMENSION COA(18),COB(18),COC(18)                                         
      DATA Y/'S','P','D','F','G','H','I'/                                       
      DATA Y1/')  ','S) ','S2)'/,Y2/'S,D'/                                      
      DATA AF/'A01(D',2*'A12(D',2*'A10(D','AS (D'/                              
      DATA BF/2*'B02(D','B13(D',' ','B11(D',' '/                                
      DATA CF/'C03(D',' ','C14(D',' ','C12(D',' '/                              
      CALL FRAPAR                                                               
      CL=2.                                                                     
 1002 CLA=0.                                                                    
 1001 READ(5,1,END=1000)N,BJ,LABEL1,LABEL2,KTEST,(INFO(I),I=1,15)               
    1 FORMAT(I4,D4.1,9X,3I1,15A4)                                               
      WRITE(6,419)                                                              
      WRITE (6,20) (INFO(I),I=1,15)                                             
   20 FORMAT(1H0,'HFS-RECHNUNGEN FUER KONFIGURATIONEN  (D+S) HOCH N',           
     110X,15A4/1H ,49(1H-),10X,60(1H=))                                         
      WRITE(6,100) BJ                                                           
  100 FORMAT(1H-,'J  =',F5.1/1H ,9(1H=))                                        
      WRITE(6,101)                                                              
  101 FORMAT(1H0,'NR.',7X,'AMPLITUDE',9X,'2S+1',4X,'L',5X,'SENI',4X,'2S1        
     1+1',4X,'L1',4X,'NCOR',5X,'S2',10X,'COREKONF',3X,'L2'/1H ,99(1H-))         
      MTEST=1                                                                   
      TSTSUM=0.                                                                 
      IF(N.LE.0) N=1                                                            
      DO 2 I=1,N                                                                
      READ (5,3) ALPHA(I),S(I),BL(I),SEN(I),S1(I),BL1(I),NCOR(I),S2(I),B        
     1L2(I),NCORKO(I)                                                           
    3 FORMAT (D16.1,2D8.1,I4,2D8.1,I4,2D8.1,I4)                                 
      IS=2.*S(I)+1.1                                                            
      IL=BL(I)+1.1                                                              
      IS1=2.*S1(I)+1.1                                                          
      IL1=BL1(I)+1.1                                                            
      IL2=BL2(I)+1.1                                                            
      KZ(I)=I                                                                   
      TSTSUM=TSTSUM+ALPHA(I)*ALPHA(I)                                           
      WRITE(6,102) I,ALPHA(I),IS,Y(IL),SEN(I),IS1,Y(IL1),NCOR(I),S2(I),         
     1NCORKO(I)                                                                 
  102 FORMAT(I3,6X,F14.10,5X,I4,5X,A1,4X,I4,5X,I4,5X,A1,4X,I4,D13.1,5X,         
     1'D HOCH',I2)                                                              
      IF(S2(I).NE.0.) WRITE(6,108) Y(IL2)                                       
  108 FORMAT(1H+,T99,A1)                                                        
      IF(BJ.GE.DABS(S(I)-BL(I)).AND.BJ.LE.(S(I)+BL(I))) GO TO 2                 
      WRITE(6,110) I,BJ                                                         
  110 FORMAT(1H0,'EIGENVEKTORKOMPONENTE NR.',I3,'  KANN BEI ZUSTAND MIT         
     1 J =',F4.1,'  NICHT AUFTRETEN'/)                                          
      CLA=20.                                                                   
    2 CONTINUE                                                                  
      IF(CLA.GT.10.) GO TO 1002                                                 
      WRITE(7,31) (INFO(I),I=1,15)                                              
   31 FORMAT(15HA-KOEFFIZIENTEN,5X,15A4)                                        
      IF((TSTSUM-1.).GE.1.D-4) WRITE(6,107) TSTSUM                              
      IF((1.-TSTSUM).GE.1.D-4) WRITE(6,109) TSTSUM                              
  107 FORMAT(1H0,'QUADRATSUMME DER EIGENVEKTORAMPLITUDEN  =',F12.8/1H ,         
     1'AMPLITUDEN SIND FEHLERHAFT')                                             
  109 FORMAT(1H0,'QUADRATSUMME DER EIGENVEKTORAMPLITUDEN  =',F12.8/1H ,         
     1'ZUSTAND IST NICHT VOLLSTAENDIG')                                         
C                                                                               
   33 NKONF=1                                                                   
      KCORKO(1)=NCORKO(1)                                                       
      BLSTR(1)=BL2(1)                                                           
      SSTR(1)=S2(1)                                                             
      ISSTR(1)=2.*SSTR(1)+0.1                                                   
      KCORKO(2)=0                                                               
      KCORKO(3)=0                                                               
      IF(N.EQ.1) GO TO 501                                                      
      DO 320 L=2,N                                                              
      IF(NKONF-2) 321,322,323                                                   
  321 IF(NCORKO(L).EQ.KCORKO(1)) GO TO 320                                      
      NKONF=2                                                                   
      KCORKO(2)=NCORKO(L)                                                       
      SSTR(2)=S2(L)                                                             
      IF(IABS(KCORKO(2)-KCORKO(1)).GT.2) WRITE(6,324)KCORKO(1),KCORKO(2)        
  324 FORMAT(1H0,'KONFIGURATION D HOCH',I3,'  UNTERSCHEIDET SICH UM MEHR        
     1 ALS 2 ELEKTRONEN VON KONFIGURATION D HOCH',I3,'  BZW. D HOCH',I3)        
      GO TO 320                                                                 
  322 IF((NCORKO(L).EQ.KCORKO(1)).OR.(NCORKO(L).EQ.KCORKO(2))) GO TO 320        
      NKONF=3                                                                   
      KCORKO(3)=NCORKO(L)                                                       
      SSTR(3)=S2(L)                                                             
      IF(IABS(KCORKO(3)-KCORKO(1)).GT.2.OR.IABS(KCORKO(3)-KCORKO(2)).GT.        
     12) WRITE(6,324) KCORKO(3),KCORKO(1),KCORKO(2)                             
  320 CONTINUE                                                                  
  323 IF(NKONF-2) 501,325,331                                                   
  325 IF((SSTR(1).NE.0.5).AND.(SSTR(2).NE.0.5)) GO TO 327                       
      KK=SSTR(2)-SSTR(1)+1.6                                                    
      NKO(1)=KCORKO(KK)                                                         
      LL=(NKO(1)-KCORKO(3-KK)+5)/2.+0.1                                         
      NKO(LL)=0                                                                 
      NKO(5-LL)=KCORKO(3-KK)                                                    
      GO TO 335                                                                 
  327 NKO(1)=0                                                                  
      NKO(2)=MIN0(KCORKO(1),KCORKO(2))                                          
      NKO(3)=MAX0(KCORKO(1),KCORKO(2))                                          
      GO TO 335                                                                 
  331 NKO(2)=MIN0(KCORKO(1),KCORKO(2),KCORKO(3))                                
      NKO(3)=MAX0(KCORKO(1),KCORKO(2),KCORKO(3))                                
      NKO(1)=(KCORKO(1)+KCORKO(2)+KCORKO(3))/3.+0.1                             
  335 DO 339 K=1,3                                                              
      IF((SSTR(K).EQ.0.5).AND.(KCORKO(K).NE.NKO(1))) WRITE(6,332)               
  332 FORMAT(1H0,'EINGEGEBENE KONFIGURATIONEN KOENNEN NICHT VON  (D+S) H        
     1OCH N  ELEKTRONEN GEBILDET WERDEN. DIE ZWISCHENKONFIGURATIONS-HFS-        
     2KONSTANTEN'/' WERDEN DAHER MOEGLICHERWEISE FALSCH BERECHNET')             
      KCORKO(K)=NKO(K)                                                          
      IF(KCORKO(K).EQ.0) GO TO 338                                              
      DO 336 L=1,N                                                              
      IF(KCORKO(K).EQ.NCORKO(L)) GO TO 337                                      
  336 CONTINUE                                                                  
  338 BLSTR(K)=0.                                                               
      SSTR(K)=0.                                                                
      ISSTR(K)=0                                                                
      GO TO 339                                                                 
  337 BLSTR(K)=BL2(L)                                                           
      SSTR(K)=S2(L)                                                             
      ISSTR(K)=2.*S2(L)+0.1                                                     
  339 CONTINUE                                                                  
C                                                                               
  501 DO 4 K=1,48                                                               
      DO 4 L=1,48                                                               
      U2(L,K)=0.                                                                
      U3(L,K)=0.                                                                
      V11(L,K)=0.                                                               
      V12(L,K)=0.                                                               
      V13(L,K)=0.                                                               
      V14(L,K)=0.                                                               
      KZF(L,K)=0                                                                
    4 CONTINUE                                                                  
      IF(LABEL2.EQ.0) GO TO 500                                                 
      WRITE(6,103)                                                              
  103 FORMAT(/1H-,'REDUZIERTE MATRIXELEMENTE'/1H ,25(1H-))                      
      WRITE(6,603)                                                              
  603 FORMAT(1H )                                                               
  500 DO 302 J4=1,3                                                             
      IF(KCORKO(J4).EQ.0) GO TO 302                                             
      KONF=KCORKO(J4)                                                           
      DO 300 J5=1,N                                                             
      IF(NCORKO(J5).NE.KONF) GO TO 300                                          
      L=NCOR(J5)                                                                
      DO 301 J6=1,N                                                             
      K=NCOR(J6)                                                                
      IF(L.GT.K) GO TO 301                                                      
      IF(L.NE.K.OR.J5.EQ.J6) GO TO 503                                          
      T1=DABS(S1(J5)-S1(J6))                                                    
      T2=DABS(BL1(J5)-BL1(J6))                                                  
      IT1=IABS(SEN(J5)-SEN(J6))                                                 
      IT2=IABS(NCORKO(J5)-NCORKO(J6))                                           
      IF((T1+T2+IT1+IT2).LT.0.1) GO TO 503                                      
      WRITE(6,502) J5,J6,L                                                      
  502 FORMAT(1H0,'EIGENVEKTORKOMPONENTEN  NR.',I3,'  UND  NR.',I3,'  TRA        
     1GEN GLEICHE  CORE-NR.',I3,' , HABEN ABER VERSCHIEDENE CORES')             
      GO TO 1001                                                                
  503 IF(NCORKO(J5).NE.NCORKO(J6).OR.KZF(L,K).EQ.1) GO TO 301                   
      CALL REDMAT(KONF,KTEST,S1(J5),BL1(J5),SEN(J5),S1(J6),BL1(J6),SEN(J        
     16),V,U,W,X,T,Z,R,MTEST)                                                   
      U2(L,K)=U                                                                 
      U3(L,K)=T                                                                 
      V11(L,K)=W                                                                
      V12(L,K)=V                                                                
      V13(L,K)=X                                                                
      V14(L,K)=Z                                                                
      KZF(L,K)=1                                                                
      IF(LABEL2.EQ.0) GO TO 301                                                 
      KS=2.*S1(J5)+1.1                                                          
      KL=BL1(J5)+1.1                                                            
      KY=SEN(J5)                                                                
      KS1=2.*S1(J6)+1.1                                                         
      KL1=BL1(J6)+1.1                                                           
      KY1=SEN(J6)                                                               
      WRITE(6,601) KY,KS,Y(KL),KY1,KS1,Y(KL1),U2(L,K),KY,KS,Y(KL),KY1,KS        
     11,Y(KL1),V13(L,K),KY,KS,Y(KL),KY1,KS1,Y(KL1),V11(L,K)                     
  601 FORMAT(1H0,'U2 (',I1,I2,A1,',',I1,I2,A1,') =',D18.9,6X,'V13 (',I1,        
     1I2,A1,',',I1,I2,A1,') =',D18.9,6X,'V11 (',I1,I2,A1,',',I1,I2,A1,')        
     2 =',D18.9)                                                                
      WRITE(6,106) U3(L,K),V14(L,K),V12(L,K)                                    
  106 FORMAT(1H ,'U3 (    "    ) =',D18.9,6X,'V14 (    "    ) =',D18.9,6        
     1X,'V12 (    "    ) =',D18.9)                                              
  301 CONTINUE                                                                  
  300 CONTINUE                                                                  
  302 CONTINUE                                                                  
      IF(LABEL2.EQ.1) WRITE(6,419)                                              
  419 FORMAT(1H1)                                                               
C                                                                               
    7 DO 8 K=1,6                                                                
      DO 330 KX=1,3                                                             
      AX(KX,K)=0.                                                               
      BX(KX,K)=0.                                                               
      CX(KX,K)=0.                                                               
  330 CONTINUE                                                                  
    8 CONTINUE                                                                  
      IF(NKONF.EQ.1) GO TO 16                                                   
      DO 333 K=1,3                                                              
      AOD(K)=0.                                                                 
      BOD(K)=0.                                                                 
  333 CONTINUE                                                                  
      NKO(1)=KCORKO(3)                                                          
      NKO(2)=KCORKO(1)                                                          
      NKO(3)=KCORKO(2)                                                          
      J=0                                                                       
      DO 6 M=1,3                                                                
      IF(NKO(M).EQ.0) GO TO 6                                                   
      DO 5 L=1,N                                                                
      IF(NCORKO(L).NE.NKO(M)) GO TO 5                                           
      J=J+1                                                                     
      KZ(J)=L                                                                   
    5 CONTINUE                                                                  
    6 CONTINUE                                                                  
C                                                                               
   16 DO 9 KI=1,N                                                               
      I=KZ(KI)                                                                  
      DO 10 KJ=KI,N                                                             
      J=KZ(KJ)                                                                  
      IF(NCOR(I).LE.NCOR(J)) GO TO 11                                           
      IL=BL1(I)+0.1                                                             
      ILS=BL1(J)+0.1                                                            
      ILM=(IL-ILS)                                                              
      U=((-1)**ILM)*U2(NCOR(J),NCOR(I))                                         
      T=((-1)**ILM)*U3(NCOR(J),NCOR(I))                                         
      IS=2.*S1(I)+0.1                                                           
      ISS=2.*S1(J)+0.1                                                          
      IS=(IS-ISS)/2                                                             
      IL=IL-ILS+IS                                                              
      W=((-1)**IL)*V11(NCOR(J),NCOR(I))                                         
      V=((-1)**IL)*V12(NCOR(J),NCOR(I))                                         
      X=((-1)**IL)*V13(NCOR(J),NCOR(I))                                         
      Z=((-1)**IL)*V14(NCOR(J),NCOR(I))                                         
      GO TO 12                                                                  
   11 U=U2(NCOR(I),NCOR(J))                                                     
      T=U3(NCOR(I),NCOR(J))                                                     
      W=V11(NCOR(I),NCOR(J))                                                    
      V=V12(NCOR(I),NCOR(J))                                                    
      X=V13(NCOR(I),NCOR(J))                                                    
      Z=V14(NCOR(I),NCOR(J))                                                    
   12 CALL AFAK (BJ,CL,CLA,S(I),BL(I),SEN(I),S1(I),BL1(I),S2(I),BL2(I),S        
     1(J),BL(J),SEN(J),S1(J),BL1(J),S2(J),BL2(J),V,ASL,NCORKO(I),NCORKO(        
     2J))                                                                       
   62 CALL BFAK (BJ,CL,CLA,S(I),BL(I),SEN(I),S1(I),BL1(I),S2(I),BL2(I),S        
     1(J),BL(J),SEN(J),S1(J),BL1(J),S2(J),BL2(J),U,W,X,BSL,NCORKO(I),NCO        
     1RKO(J))                                                                   
      CALL CFAK (BJ,CL,CLA,S(I),BL(I),SEN(I),S1(I),BL1(I),S2(I),BL2(I),S        
     1(J),BL(J),SEN(J),S1(J),BL1(J),S2(J),BL2(J),T,Z,V,CSL,NCORKO(I),NCO        
     2RKO(J))                                                                   
      F=1.                                                                      
      IF(I.NE.J) F=2.                                                           
      DO 14 K=1,9                                                               
      A(I,J,K)=F*ASL(K)                                                         
      B(I,J,K)=F*BSL(K)                                                         
      C(I,J,K)=F*CSL(K)                                                         
      IF(I.GE.J) GO TO 14                                                       
      A(J,I,K)=A(I,J,K)                                                         
      B(J,I,K)=B(I,J,K)                                                         
      C(J,I,K)=C(I,J,K)                                                         
   14 CONTINUE                                                                  
   13 DO 15 K=1,6                                                               
      DO 340 NX=1,3                                                             
      IF(NCORKO(I).NE.KCORKO(NX)) GO TO 340                                     
      AX(NX,K)=AX(NX,K)+A(I,J,K)*ALPHA(I)*ALPHA(J)                              
      BX(NX,K)=BX(NX,K)+B(I,J,K)*ALPHA(I)*ALPHA(J)                              
      CX(NX,K)=CX(NX,K)+C(I,J,K)*ALPHA(I)*ALPHA(J)                              
      GO TO 15                                                                  
  340 CONTINUE                                                                  
   15 CONTINUE                                                                  
      IF(NKONF.EQ.1) GO TO 10                                                   
      DO 18 K=1,3                                                               
      AOD(K)=AOD(K)+A(I,J,K+6)*ALPHA(I)*ALPHA(J)                                
      BOD(K)=BOD(K)+B(I,J,K+6)*ALPHA(I)*ALPHA(J)                                
   18 CONTINUE                                                                  
   10 CONTINUE                                                                  
    9 CONTINUE                                                                  
C                                                                               
      IL=CL+1.1                                                                 
      ILA=CLA+1.1                                                               
      WRITE(6,19)                                                               
   19 FORMAT(/1H-,'HFS-KONSTANTEN'/1H ,14(1H=)//)                               
      LA1=0                                                                     
      LB1=0                                                                     
      LC1=0                                                                     
      LI1=2                                                                     
      LI2=3                                                                     
      LI3=1                                                                     
      DO 350 NX=1,3                                                             
      IF(KCORKO(NX).EQ.0) GO TO 516                                             
      LI=ISSTR(NX)+1                                                            
      IF(NX.EQ.2) LI=LI+2                                                       
      WRITE(6,351)(AX(NX,I),AF(I),KCORKO(NX),Y1(LI),I=1,5,2)                    
      IF(LI.EQ.2) WRITE(6,352) AX(NX,6),KCORKO(NX),Y1(LI)                       
      WRITE(6,351)(BX(NX,I),BF(I),KCORKO(NX),Y1(LI),I=1,5,2)                    
      WRITE(6,351)(CX(NX,I),CF(I),KCORKO(NX),Y1(LI),I=1,5,2)                    
  351 FORMAT(1H0,3(D17.8,2X,A5,I1,A3,8X))                                       
  352 FORMAT(1H+,T108,D15.8,'  AS(D',I1,A3)                                     
      WRITE(6,604)                                                              
  604 FORMAT(1H0)                                                               
  516 DO 356 M=1,5,2                                                            
      LA1=LA1+1                                                                 
      LB1=LB1+1                                                                 
      LC1=LC1+1                                                                 
      COA(LA1)=AX(NX,M)                                                         
      COB(LB1)=BX(NX,M)                                                         
      COC(LC1)=CX(NX,M)                                                         
  356 CONTINUE                                                                  
      IF(ISSTR(NX).NE.1) GO TO 358                                              
  359 LA1=LA1+1                                                                 
      COA(LA1)=AX(NX,6)                                                         
  358 IF(NKONF.EQ.1) GO TO 361                                                  
  350 CONTINUE                                                                  
      IF(KCORKO(1).EQ.0) GO TO 361                                              
      IF(NKONF.EQ.2) GO TO 511                                                  
      WRITE(6,513) AOD(1),AF(2),KCORKO(1),Y2,KCORKO(2),Y1(LI2),AOD(2),          
     1AF(3),KCORKO(1),Y2,KCORKO(3),Y1(LI3),AOD(3),AF(4),KCORKO(1),Y2,           
     2KCORKO(2),Y1(LI2)                                                         
      WRITE(6,517) BOD(1),KCORKO(1),Y2,KCORKO(2),Y1(LI2),BOD(2),                
     1KCORKO(1),Y2,KCORKO(3),Y1(LI3)                                            
      GO TO 512                                                                 
  511 IF(KCORKO(2).EQ.0) GO TO 510                                              
      WRITE(6,513) (AOD(I),AF(I+1),KCORKO(1),Y2,KCORKO(2),Y1(LI2),              
     1I=1,3,2)                                                                  
      WRITE(6,517)  BOD(1),KCORKO(1),Y2,KCORKO(2),Y1(LI2)                       
      GO TO 512                                                                 
  510 WRITE(6,513)  AOD(2),AF(3),KCORKO(1),Y2,KCORKO(3),Y1(LI3)                 
      WRITE(6,517)  BOD(2),KCORKO(1),Y2,KCORKO(3),Y1(LI3)                       
  513 FORMAT(1H0,3(D17.8,2X,A5,I1,A3,I1,A3,4X))                                 
  517 FORMAT(1H0,2(D17.8,2X,'B02(D',I1,A3,I1,A3,4X))                            
  512 LA1=LA1+3                                                                 
      LB1=LB1+2                                                                 
      COA(LA1-2)=AOD(1)                                                         
      COA(LA1-1)=AOD(2)                                                         
      COA(LA1)=AOD(3)                                                           
      COB(LB1-1)=BOD(1)                                                         
      COB(LB1)=BOD(2)                                                           
  361 WRITE(6,360)                                                              
  360 FORMAT(1H0)                                                               
      WRITE(7,365) (COA(L),L=1,LA1)                                             
      WRITE(7,362)                                                              
      WRITE(7,365) (COB(L),L=1,LB1)                                             
      WRITE(7,363)                                                              
      WRITE(7,365) (COC(L),L=1,LC1)                                             
  362 FORMAT(15HB-KOEFFIZIENTEN)                                                
  363 FORMAT(15HC-KOEFFIZIENTEN)                                                
  365 FORMAT(3(5X,D15.8),D17.8,3X)                                              
      IF(LABEL1.EQ.0)GO TO 1001                                                 
C                                                                               
      WRITE(6,400)                                                              
  400 FORMAT(1H1,'HFS-KONSTANTEN IN REINER SL-KOPPLUNG'/1H ,36(1H-)//)          
      DO 23 I=1,N                                                               
      NI=I                                                                      
      LI=2.*S2(I)+1.1                                                           
      IF(NCORKO(I).EQ.KCORKO(2)) LI=LI+2                                        
      DO 24 J=1,NI                                                              
      IF(I.EQ.J) GO TO 27                                                       
      DO 28 K=1,9                                                               
      A(I,J,K)=A(I,J,K)/2.                                                      
      B(I,J,K)=B(I,J,K)/2.                                                      
      C(I,J,K)=C(I,J,K)/2.                                                      
   28 CONTINUE                                                                  
   27 IS=2.*S(I)+1.1                                                            
      ISS=2.*S(J)+1.1                                                           
      ILL=BL(I)+1.1                                                             
      ILS=BL(J)+1.1                                                             
      LJ=2.*S2(J)+1.1                                                           
      IF(NCORKO(J).EQ.KCORKO(2)) LJ=LJ+2                                        
      IF(IABS(NCORKO(I)-NCORKO(J))-1) 21,22,24                                  
   21 WRITE(6,92) I,J,SEN(I),IS,Y(ILL),SEN(J),ISS,Y(ILS),(A(I,J,K),AF(K)        
     1,NCORKO(I),Y1(LI),K=1,5,2)                                                
      IF(LI.EQ.2) WRITE(6,93) A(I,J,6),AF(6),NCORKO(I),Y1(2)                    
      WRITE(6,95) I,J,SEN(I),IS,Y(ILL),SEN(J),ISS,Y(ILS),(B(I,J,K),BF(K)        
     1,NCORKO(I),Y1(LI),K=1,5,2)                                                
      WRITE(6,98) I,J,SEN(I),IS,Y(ILL),SEN(J),ISS,Y(ILS),(C(I,J,K),CF(K)        
     1,NCORKO(I),Y1(LI),K=1,5,2)                                                
   92 FORMAT(1H0,'A(',I2,',',I2,') = A(',I1,',',I1,A1,' , ',I1,',',I1,A1        
     1,') =',3(D18.8,2X,A5,I1,A3,6X))                                           
   93 FORMAT(1H ,97X,D18.8,2X,A5,I1,A3)                                         
   95 FORMAT(1H0,'B(',I2,',',I2,') = B(',I1,',',I1,A1,' , ',I1,',',I1,A1        
     1,') =',3(D18.8,2X,A5,I1,A3,6X))                                           
   98 FORMAT(1H0,'C(',I2,',',I2,') = C(',I1,',',I1,A1,' , ',I1,',',I1,A1        
     1,') =',3(D18.8,2X,A5,I1,A3,6X))                                           
      GO TO 26                                                                  
   22 IF(LI.EQ.1.OR.LJ.EQ.1) GO TO 25                                           
      WRITE(6,94) I,J,SEN(I),IS,Y(ILL),SEN(J),ISS,Y(ILS),(A(I,J,K),AF(K-        
     15),KCORKO(1),Y2,KCORKO(2),Y1(LI2),K=7,9,2)                                
      WRITE(6,96) I,J,SEN(I),IS,Y(ILL),SEN(J),ISS,Y(ILS), B(I,J,7),BF(1)        
     1  ,KCORKO(1),Y2,KCORKO(2),Y1(LI2)                                         
   94 FORMAT(1H0,'A(',I2,',',I2,') = A(',I1,',',I1,A1,' , ',I1,',',I1,A1        
     1,') =',2(D18.8,2X,A5,I1,A3,I1,A3,2X))                                     
   96 FORMAT(1H0,'B(',I2,',',I2,') = B(',I1,',',I1,A1,' , ',I1,',',I1,A1        
     1,') =',  D18.8,2X,A5,I1,A3,I1,A3)                                         
      GO TO 26                                                                  
   25 WRITE(6,94) I,J,SEN(I),IS,Y(ILL),SEN(J),ISS,Y(ILS),A(I,J,8),AF(3),        
     1KCORKO(1),Y2,KCORKO(3),Y1(LI3)                                            
      WRITE(6,96) I,J,SEN(I),IS,Y(ILL),SEN(J),ISS,Y(ILS),B(I,J,8),BF(1),        
     1KCORKO(1),Y2,KCORKO(3),Y1(LI3)                                            
   26 WRITE(6,97)                                                               
   97 FORMAT(1H0,126(1H-))                                                      
   24 CONTINUE                                                                  
   23 CONTINUE                                                                  
      GO TO 1001                                                                
1000  STOP                                                                      
      END                                                                       
                                                                                                                             
