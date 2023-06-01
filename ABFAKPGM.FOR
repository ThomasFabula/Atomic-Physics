C										
C     PROGRAMM	'ABFAK (ALLGEMEIN)'  ZUR BERECHNUNG DER HFS-KONSTANTEN A,B EINER
C     BELIEBIGEN KONFIGURATION	L**N L'  IN MITTLERER KOPPLUNG.                 
C										
C     VERSION VOM FEBRUAR 1984							
C										
C     REGION=512K								
C										
C     ALS UNTERPROGRAMME WERDEN AUSSER	AFAK  UND  BFAK  NOCH DIE  3J-,6J-,9J-	
C     LOAD MODULES AUS	UNQ001.UNQ00.SUBR.LOAD(SECHSJ)	BENOETIGT		
C										
C										
C     EINGABE:									
C     --------									
C     KARTE    SPALTE	 FORMAT    NAME   BEDEUTUNG				
C     1 	1-4	   I4	   N	  ZAHL DER EIGENVEKTORKOMPONENTEN (<=50)
C		5-8	  F4.1	   BJ	  GESAMTDREHIMPULS J DES ZUSTANDS	
C		9-12	  F4.1	   CL	  BAHNDREHIMPULS L DER L**N-SCHALE	
C	       13-15	  F3.1	   CLA	  BAHNDREHIMPULS L' DER L'-SCHALE	
C	       16-17	   I2	   NRM	  NR. DER EV-UNTERMATRIX DES ZUSTANDS	
C	       18-19	   I2	   NRZ	  NR. DES ZUSTANDS IN DER UNTERMATRIX	
C					  (NRM,NRZ NUR ANGEBEN, WENN DIE "EIGEN-
C					   VEKTORKARTEN" VOM TYP 2  VON EINER   
C					   DATEI GELESEN WERDEN)		
C		20	   I1	   KRME   KENNZIFFER FUER DIE EINGABE DER REDUZ.
C					  MATRIXELEMENTE  V12, U2, V11, V13	
C					  LEER: M.E. SIND IM DATENSATZ ENTHALTEN
C					  >0 : M.E. SIND BEREITS IN EINEM DATEN-
C					  SATZ MIT  KRME=0  EINGEGEBEN WORDEN	
C	       21-80	  15A4	   INFO(I)	 BELIEBIGER UEBERSCHRIFTENTEXT	
C     --------------------------------------------------------------------------
C     ES FOLGEN N KARTEN VOM TYP 2  MIT DEN EIGENVEKTORAMPLITUDEN DES ZUSTANDS. 
C     BESCHRIFTUNG WIE IM PROGRAMM  'ABCXFAK(D+S)**N'  ANGEGEBEN, AUSNAHMEN:	
C	       57-64	  F8.1	   S2	  SPIN DES L'-CORE-ZUSTANDS             
C	       65-72	  F8.1	   L2	  BAHNDREHIMPULS DES L'-CORE-ZUSTANDS   
C	       73-76			  LEER					
C     DIE KARTEN 2  KOENNEN AUCH VON EINER VOM PROGRAMM 'FSFIT' ERSTELLTEN DATEI
C     MIT FILE-NR. 12  GELESEN WERDEN, WENN AUF DER KARTE VOM TYP 1  DIE KENN-	
C     ZIFFERN  NRM,NRZ	DES GEWUENSCHTEN ZUSTANDS ANGEGEBEN SIND.		
C     --------------------------------------------------------------------------
C     FUER  KRME=0  FOLGEN KARTEN VOM TYP 3 MIT DEN REDUZ. MATRIXELEMENTEN :	
C     3 	1-4	   I4	   L	  CORE-NR. DES BRA-ZUSTANDS  ( <=50 )	
C		5-8	   I4	   K	  CORE-NR. DES KET-ZUSTANDS  (	 "  )   
C		9-72	 4D16.1    V,U,W,X	 V12, U2, V11, V13		
C     ES MUESSEN NUR DIE M.E. ^0. EINGEGEBEN WERDEN. NACH DER LETZTEN M.E.-KARTE
C     MUSS EINE LEERKARTE DEN DATENSATZ ABSCHLIESSEN. DIE M.E. KOENNEN AUCH VON 
C     EINER DATEI MIT FILE-NR. 11 GELESEN WERDEN, WENN EINE STEUERKARTE DER FORM
C     //GO.FT11F001 DD DSN=...	 VERWENDET WIRD, DIE ABSCHLIESSENDE LEERKARTE	
C     MUSS DANN IM KARTENSATZ DIREKT DEN "EIGENVEKTORKARTEN" VOM TYP 2	FOLGEN. 
C     ES KOENNEN DANACH BELIEBIG VIELE WEITERE DATENSAETZE EINGEGEBEN WERDEN.	
C     DIESE KOENNEN DIESELBE DATEI FUER DIE REDUZ. MATRIXELEMENTE WIEDER BE-	
C     NUTZEN. FUER KRME>0 ENTFALLEN DIE KARTEN VOM TYP 3 UND AUCH DIE LEERKARTE.
C										
C     AUSGABE:	 EINGABEDATEN, REDUZIERTE MATRIXELEMENTE, HFS-KONSTANTEN IN	
C     --------	 MITTLERER KOPPLUNG UND IN REINER SL-KOPPLUNG. DIE HFS-KONSTAN- 
C		 TEN IN MITTLERER KOPPLUNG KOENNEN AUCH IN EINE DATEI MIT	
C		 FILE-NR. 7  GESCHRIEBEN WERDEN.				
C										
      IMPLICIT REAL*8 (A-H,O-Z) 						
      REAL*4 INFO(15),Y(13)							
      INTEGER SEN(50)								
      DIMENSION ALPHA(50),BL(50),S1(50),BL1(50),BL2(50),S2(50),V12(50,50	
     1),NCOR(50),AF(6),A(50,50,6),S(50),BF(6),B(50,50,6),U2(50,50),V11(5	
     10,50),V13(50,50),COA(6),COB(6)						
      DATA Y/'S','P','D','F','G','H','I','K','L','M','N','O','Q'/		
 1001 READ(5,1,END=1000)N,BJ,CL,CLA,NRM,NRZ,KRME,(INFO(I),I=1,15)		
    1 FORMAT(I4,2D4.1,D3.1,2I2,I1,15A4) 					
      WRITE (6,20) (INFO(I),I=1,15)						
   20 FORMAT(1H1,15A4)								
      LD=5									
      IF(NRM.LE.0.OR.NRZ.LE.0) GO TO 40 					
      LD=12									
      REWIND 12 								
   33 READ(12,36,END=38) BJ,N,KMA,KNZ						
   36 FORMAT(25X,F5.1,18X,I2,26X,2I2)						
      IF(KMA.EQ.NRM.AND.KNZ.EQ.NRZ) GO TO 40					
      READ(12,37) (DUMMY,I=1,N) 						
   37 FORMAT(A8)								
      GO TO 33									
   38 WRITE(6,39) NRM,NRZ							
   39 FORMAT('-GEWUENSCHTER ZUSTAND ',2I3,'   EXISTIERT NICHT AUF DER LE        
     1SEDATEI')                                                                 
      GO TO 1001								
   40 WRITE(6,100) BJ,CL,CLA							
  100 FORMAT(1H0,'J =',D9.2,'   L =',D8.1,'   LST=',D8.1)			
      WRITE(6,101)								
  101 FORMAT(1H0,'NR',11X,'AMPLITUDE',4X,'2S+1',4X,'L',4X,'SENI',4X,'2S1        
     1+1',4X,'L1',4X,'NCOR',10X,'S2',4X,'L2')                                   
      I=1									
      DO 2 K=1,N								
      READ(LD,3) ALPHA(I),S(I),BL(I),SEN(I),S1(I),BL1(I),NCOR(I),S2(I),B	
     1L2(I)									
    3 FORMAT (D16.1,2D8.1,I4,2D8.1,I4,2D8.1)					
      IF(DABS(ALPHA(I)).LT.1.D-10) GO TO 2					
      IS=2.*S(I)+1.1								
      IL=BL(I)+1.1								
      IS1=2.*S1(I)+1.1								
      IL1=BL1(I)+1.1								
      IL2=BL2(I)+1.1								
      WRITE(6,102) I,ALPHA(I),IS,Y(IL),SEN(I),IS1,Y(IL1),NCOR(I),S2(I),Y	
     1(IL2)									
  102 FORMAT(1H ,I2,D20.9,4X,I4,4X,A1,4X,I4,5X,I4,5X,A1,4X,I4,D12.1,5X,A	
     11)									
      I=I+1									
    2 CONTINUE									
      N=I-1									
      IF(KRME.NE.0) GO TO 7							
      DO 4 L=1,50								
      DO 4 K=1,50								
      V12(L,K)=0.								
      U2(L,K)=0.								
      V11(L,K)=0.								
      V13(L,K)=0.								
    4 CONTINUE									
      WRITE(6,103)								
  103 FORMAT(1H0,'V12-MATRIXELEMENTE')						
      WRITE(6,603)								
  603 FORMAT(1H0,'U2-MATRIXELEMENTE',21X,'V13-MATRIXELEMENTE',20X,'V11-M        
     1ATRIXELEMENTE')                                                           
      LD=5									
      NL=0									
    6 READ(LD,5,END=18)L,K,V,U,W,X						
    5 FORMAT (2I4,4D16.1)							
      IF (L.NE.0) GO TO 17							
      IF(NL.NE.0) GO TO 7							
      LD=11									
      GO TO 6									
   17 NL=1									
      V12(L,K)=V								
      U2(L,K)=U 								
      V11(L,K)=W								
      V13(L,K)=X								
      DO 104 I=1,N								
      IF(NCOR(I).EQ.L) GO TO 107						
  104 CONTINUE									
      GO TO 6									
  107 KS=2.*S1(I)+1.1								
      KL=BL1(I)+1.1								
      KY=SEN(I) 								
      DO 105 I=1,N								
      IF(NCOR(I).EQ.K) GO TO 108						
  105 CONTINUE									
      GO TO 6									
  108 KS1=2.*S1(I)+1.1								
      KL1=BL1(I)+1.1								
      KY1=SEN(I)								
      WRITE(6,106) KY,KS,Y(KL),KY1,KS1,Y(KL1),V12(L,K)				
  106 FORMAT(1H ,'V12(',I1,I2,A1,',',I1,I2,A1,') =',D18.9)			
      WRITE(6,601) KY,KS,Y(KL),KY1,KS1,Y(KL1),U2(L,K),KY,KS,Y(KL),KY1,KS	
     11,Y(KL1),V13(L,K),KY,KS,Y(KL),KY1,KS1,Y(KL1),V11(L,K)			
  601 FORMAT(1H ,'U2(',I1,I2,A1,',',I1,I2,A1,') =',D18.9,4X,'V13(',I1,I2	
     1,A1,',',I1,I2,A1,') =',D18.9,4X,'V11(',I1,I2,A1,',',I1,I2,A1,') ='	
     1,D18.9)									
      GO TO 6									
C										
   18 REWIND 11 								
    7 DO 8 K=1,6								
      AF(K)=0.									
      BF(K)=0.									
    8 CONTINUE									
      DO 9 I=1,N								
      NI=I									
      DO 10 J=1,NI								
      IF(NCOR(I).LE.NCOR(J)) GO TO 11						
      IL=BL1(I)+0.1								
      ILS=BL1(J)+0.1								
      ILM=(IL-ILS)								
      U=((-1)**ILM)*U2(NCOR(J),NCOR(I)) 					
      IS=2.*S1(I)+0.1								
      ISS=2.*S1(J)+0.1								
      IS=(IS-ISS)/2								
      IL=IL-ILS+IS								
      V=((-1)**IL)*V12(NCOR(J),NCOR(I)) 					
      W=((-1)**IL)*V11(NCOR(J),NCOR(I)) 					
      X=((-1)**IL)*V13(NCOR(J),NCOR(I)) 					
      GO TO 12									
   11 V=V12(NCOR(I),NCOR(J))							
      U=U2(NCOR(I),NCOR(J))							
      W=V11(NCOR(I),NCOR(J))							
      X=V13(NCOR(I),NCOR(J))							
   12 CALL AFAK (BJ,CL,CLA,S(I),BL(I),SEN(I),S1(I),BL1(I),S2(I),BL2(I),S	
     1(J),BL(J),SEN(J),S1(J),BL1(J),S2(J),BL2(J),V,A(I,J,1),A(I,J,2),A(I	
     1,J,3),A(I,J,4),A(I,J,5),A(I,J,6),NCOR(I),NCOR(J)) 			
   62 CALL BFAK (BJ,CL,CLA,S(I),BL(I),SEN(I),S1(I),BL1(I),S2(I),BL2(I),S	
     1(J),BL(J),SEN(J),S1(J),BL1(J),S2(J),BL2(J),U,W,X,B(I,J,1),B(I,J,2)	
     1,B(I,J,3),B(I,J,4),B(I,J,5),B(I,J,6),NCOR(I),NCOR(J))			
      IF(I.EQ.J) GO TO 13							
      DO 14 K=1,6								
      A(I,J,K)=2.*A(I,J,K)							
      B(I,J,K)=2.*B(I,J,K)							
   14 CONTINUE									
   13 DO 15 K=1,6								
      AF(K)=AF(K)+A(I,J,K)*ALPHA(I)*ALPHA(J)					
      BF(K)=BF(K)+B(I,J,K)*ALPHA(I)*ALPHA(J)					
   15 CONTINUE									
   10 CONTINUE									
    9 CONTINUE									
      IL=CL+1.1 								
      ILA=CLA+1.1								
      WRITE(6,21) AF(1),Y(IL),AF(3),Y(IL),AF(5),Y(IL)				
      WRITE(6,22) AF(2),Y(ILA),AF(4),Y(ILA),AF(6),Y(ILA)			
   21 FORMAT(1H0,'A = ',D17.8,'  A01(',A1,')   ',D17.8,'  A12(',A1,')           
     1',D17.8,'  A10(',A1,')')                                                  
   22 FORMAT(1H ,4X,D17.8,'  A01(',A1,')   ',D17.8,'  A12(',A1,')   ',D1	
     17.8,'  A10(',A1,')')							
      WRITE(6,82) BF(1),Y(IL),BF(3),Y(IL),BF(5),Y(IL)				
      WRITE(6,83) BF(2),Y(ILA),BF(4),Y(ILA),BF(6),Y(ILA)			
   82 FORMAT(1H0,'B = ',D17.8,'  B02(',A1,')   ',D17.8,'  B13(',A1,')',D	
     117.8,'  B11(',A1,')')							
   83 FORMAT(1H ,4X,D17.8,'  B02(',A1,')   ',D17.8,'  B13(',A1,')',D17.8	
     1,'  B11(',A1,')') 							
      WRITE(7,31) (INFO(I),I=1,15)						
   31 FORMAT(15HA-KOEFFIZIENTEN,5X,15A4)					
      COA(4)=AF(6)								
      JE=MIN0(ILA,2)								
      I=1									
      DO 32 J=1,JE								
      K=J									
      DO 32 M=1,3								
      COA(I)=AF(K)								
      COB(I)=BF(K)								
      I=I+1									
      K=K+2									
   32 CONTINUE									
      IA=2.*JE+2.1								
      IB=3.*JE+0.1								
      WRITE(7,34) (COA(I),I=1,IA)						
      WRITE(7,35)								
      WRITE(7,34) (COB(I),I=1,IB)						
   34 FORMAT(3(5X,D15.8),D17.8,3X)						
   35 FORMAT(15HB-KOEFFIZIENTEN)						
C										
      DO 23 I=1,N								
      NI=I									
      DO 24 J=1,NI								
      IF(I.EQ.J) GO TO 27							
      DO 28 K=1,6								
      A(I,J,K)=A(I,J,K)/2.							
      B(I,J,K)=B(I,J,K)/2.							
   28 CONTINUE									
   27 IS=2.*S(I)+1.1								
      ISS=2.*S(J)+1.1								
      ILL=BL(I)+1.1								
      ILS=BL(J)+1.1								
      WRITE(6,25) SEN(I),IS,Y(ILL),SEN(J),ISS,Y(ILS),A(I,J,1),Y(IL),A(I,	
     1J,3),Y(IL),A(I,J,5),Y(IL) 						
      WRITE(6,26) A(I,J,2),Y(ILA),A(I,J,4),Y(ILA),A(I,J,6),Y(ILA)		
   25 FORMAT (1H0,'A(',I1,',',I1,A1,' , ',I1,',',I1,A1,') = ',D17.8,'  A        
     101(',A1,')   ',D17.8,'  A12(',A1,')   ',D17.8,'  A10(',A1,')')            
   26 FORMAT(1H ,17X,D17.8,'  A01(',A1,')   ',D17.8,'  A12(',A1,')   ',D	
     117.8,'  A10(',A1,')')							
      WRITE(6,95) SEN(I),IS,Y(ILL),SEN(J),ISS,Y(ILS),B(I,J,1),Y(IL),B(I,	
     1J,3),Y(IL),B(I,J,5),Y(IL) 						
      WRITE(6,96) B(I,J,2),Y(ILA),B(I,J,4),Y(ILA),B(I,J,6),Y(ILA)		
   95 FORMAT(1H0,'B(',I1,',',I1,A1,' , ',I1,',',I1,A1,') = ',D17.8,'  B0        
     12(',A1,')   ',D17.8,'  B13(',A1,')   ',D17.8,'  B11(',A1,')')             
   96 FORMAT(1H ,17X,D17.8,'  B02(',A1,')   ',D17.8,'  B13(',A1,')   ',D	
     117.8,'  B11(',A1,')')							
   24 CONTINUE									
   23 CONTINUE									
      GO TO 1001								
1000  STOP									
      END									
      SUBROUTINE AFAK (BJ,CL,CLA,SI,BLI,SENI,S1I,BL1I,S2I,BL2I,SJ,BLJ,SE	
     1NJ,S1J,BL1J,S2J,BL2J,V,AIJ1,AIJ2,AIJ3,AIJ4,AIJ5,AIJ6,NCI,NCJ)		
      IMPLICIT REAL*8 (A-H,O-Z) 						
      INTEGER SENI,SENJ 							
      AIJ1=0.									
      AIJ2=0.									
      AIJ3=0.									
      AIJ4=0.									
      AIJ5=0.									
      AIJ6=0.									
      IF(S2I.NE.S2J) GO TO 500							
      D=((2.*BJ+1.)/(BJ*(BJ+1.)))**0.5						
      CALL NEUNJ (SI,SJ,1.,BLI,BLJ,2.,BJ,BJ,1.,RES1,IER)			
      IF(IER.NE.0) GO TO 300							
      CALL SECHSJ (SI,SJ,1.,S1J,S1I,S2I,RES2,IER)				
      IF(IER.NE.0) GO TO 400							
      CALL SECHSJ (BLI,BLJ,2.,BL1J,BL1I,CLA,RES3,IER)				
      IF(IER.NE.0) GO TO 400							
      W2=((CL*(CL+1.)*(2.*CL+1.))/((2.*CL-1.)*(2.*CL+3.)))**0.5 		
      Z2IJ=((30.**0.5)*RES1*((2.*SI+1.)*(2.*SJ+1.)*(2.*BLI+1.)*(2.*BLJ+1	
     1.))**0.5) 								
      IEX1=S1I+BL1I+S2I+CLA+1.1+SJ+BLJ						
      AIJ3=Z2IJ*RES2*RES3*W2*D*V*((-1.)**(IEX1))				
      IF(NCI.NE.NCJ) RETURN							
      CALL SECHSJ (SI,SJ,1.,S2I,S2I,S1I,RES4,IER)				
      IF(IER.NE.0) GO TO 400							
      CALL SECHSJ(BLI,BLJ,2.,CLA,CLA,BL1I,RES5,IER)				
      IF(IER.NE.0) GO TO 400							
      W3=((3.*CLA*(CLA+1.)*(2.*CLA+1.))/(2.*(2.*CLA-1.)*(2.*CLA+3.)))**0	
     1.5									
      IEX2=S1I+BL1I+S2I+CLA+1.1+SI+BLI						
      AIJ4=Z2IJ*RES4*RES5*W3*D*((-1.)**(IEX2))					
      IF(SI.NE.SJ) GO TO 200							
      CALL SECHSJ(BJ,BJ,1.,BLJ,BLI,SI,RES6,IER) 				
      IF(IER.NE.0) GO TO 400							
      CALL SECHSJ(BLI,BLJ,1.,BL1I,BL1I,CLA,RES7,IER)				
      IF(IER.NE.0) GO TO 400							
      CALL SECHSJ(BLI,BLJ,1.,CLA,CLA,BL1I,RES8,IER)				
      IF(IER.NE.0) GO TO 400							
      Z1IJ=RES6*((2.*BLI+1.)*(2.*BLJ+1.))**0.5					
      IX3=SI+BLJ+BJ+BL1I+CLA+BLJ+0.1						
      AIJ1=Z1IJ*RES7*D*((-1.)**IX3)*(BL1I*(BL1I+1.)*(2.*BL1I+1.))**0.5		
      W1=(CLA*(CLA+1.)*(2.*CLA+1.))**0.5					
      IX4=SI+BLJ+BJ+BL1I+CLA+BLI+0.1						
      AIJ2=Z1IJ*RES8*D*W1*((-1.)**IX4)						
  200 IF(BLI.NE.BLJ) GO TO 500							
      CALL SECHSJ(BJ,BJ,1.,SJ,SI,BLI,RES9,IER)					
      IF(IER.NE.0) GO TO 400							
      CALL SECHSJ(SI,SJ,1.,S1I,S1I,S2I,RES10,IER)				
      IF(IER.NE.0) GO TO 400							
      CALL SECHSJ(SI,SJ,1.,S2I,S2I,S1I,RES11,IER)				
      IF(IER.NE.0) GO TO 400							
      Z3IJ=RES9*((2.*SI+1.)*(2.*SJ+1.))**0.5					
      I5=SI+BLI+BJ+S1I+S2I+SJ+0.1						
      AIJ5=Z3IJ*D*RES10*((-1.)**I5)*(S1I*(S1I+1.)*(2.*S1I+1.))**0.5		
      I6=SI+BLI+BJ+S1I+S2I+SI+0.1						
      AIJ6=Z3IJ*D*RES11*((-1.)**I6)*(S2I*(S2I+1.)*(2.*S2I+1.))**0.5		
      GO TO 500 								
  400 WRITE (6,401) IER 							
  401 FORMAT(1H0,'FEHLER',I4,'  SECHSJ')					
      GO TO 500 								
  300 WRITE (6,301) IER 							
  301 FORMAT (1H0,'FEHLER',I4,'  NEUNJ')					
  500 RETURN									
      END									
      SUBROUTINE BFAK(BJ,CL,CLA,SI,BLI,SENI,S1I,BL1I,S2I,BL2I,SJ,BLJ,SEN	
     1J,S1J,BL1J,S2J,BL2J,U2,V11,V13,BIJ1,BIJ2,BIJ3,BIJ4,BIJ5,BIJ6,NCI, 	
     2NCJ)									
      IMPLICIT REAL*8 (A-H,O-Z) 						
      INTEGER SENI,SENJ 							
      BIJ1=0.									
      BIJ2=0.									
      BIJ3=0.									
      BIJ4=0.									
      BIJ5=0.									
      BIJ6=0.									
      IF(S2I.NE.S2J) GO TO 900							
      E=(((4.*BJ*(2.*BJ-1.)*(2.*BJ+1.))/((BJ+1.)*(2.*BJ+3.)))**0.5)*((2.	
     1*SI+1.)*(2.*SJ+1.)*(2.*BLI+1.)*(2.*BLJ+1.))**0.5				
      CALL NEUNJ(SI,SJ,1.,BLI,BLJ,3.,BJ,BJ,2.,RES21,IER)			
      IF(IER.NE.0) GO TO 700							
      CALL SECHSJ(SI,SJ,1.,S1J,S1I,S2I,RES22,IER)				
      IF(IER.NE.0) GO TO 800							
      CALL SECHSJ(BLI,BLJ,3.,BL1J,BL1I,CLA,RES23,IER)				
      IF(IER.NE.0) GO TO 800							
      IEY1=S1I+BL1I+S2I+CLA+SJ+BLJ+0.1						
      BIJ3=E*RES21*RES22*RES23*((-1.)**IEY1)*V13				
      CALL NEUNJ(SI,SJ,1.,BLI,BLJ,1.,BJ,BJ,2.,RES24,IER)			
      IF(IER.NE.0) GO TO 700							
      CALL SECHSJ(SI,SJ,1.,S1J,S1I,S2I,RES25,IER)				
      IF(IER.NE.0) GO TO 800							
      CALL SECHSJ(BLI,BLJ,1.,BL1J,BL1I,CLA,RES26,IER)				
      BIJ5=E*RES24*RES25*RES26*V11*((-1.)**IEY1)				
      IF(S1I.NE.S1J) GO TO 900							
      IF(SI.NE.SJ) GO TO 901							
      CALL SECHSJ(BLI,BLJ,2.,BJ,BJ,SI,RES27,IER)				
      IF(IER.NE.0) GO TO 800							
      CALL SECHSJ(BLI,BLJ,2.,BL1J,BL1I,CLA,RES28,IER)				
      IF(IER.NE.0) GO TO 800							
      W4=((CL*(CL+1.)*(2.*CL+1.))/((2.*CL-1.)*(2.*CL+3.)))**0.5 		
      IEY2=S1I+BL1I+S2I+CLA+BLJ+BJ+SI+S1I+S2I+SI+SJ+BLJ+0.1			
      BIJ1=(E*RES27*RES28*W4/(2.*SI+1.))*((-1.)**IEY2)*U2			
      IF(NCI.NE.NCJ) RETURN							
      CALL SECHSJ(BLI,BLJ,2.,CLA,CLA,BL1I,RES29,IER)				
      IF(IER.NE.0) GO TO 800							
      W5=((CLA*(CLA+1.)*(2.*CLA+1.))/((2.*CLA-1.)*(2.*CLA+3.)))**0.5		
      IEY3=S1I+BL1I+S2I+CLA+BLJ+BJ+SI+S1I+S2I+SI+SI+BLI+0.1			
      BIJ2=((E*W5*RES27*RES29)/(2.*SI+1.))*((-1.)**IEY3)*U2			
  901 IF(NCI.NE.NCJ) RETURN							
      CALL SECHSJ(SI,SJ,1.,S2I,S2I,S1I,RES30,IER)				
      IF(IER.NE.0) GO TO 800							
      CALL SECHSJ(BLI,BLJ,3.,CLA,CLA,BL1I,RES31,IER)				
      IF(IER.NE.0) GO TO 800							
      IEY4=S1I+BL1I+S2I+CLA+SI+BLI+0.1						
      BIJ4=E*RES21*RES30*RES31*(1.5**0.5)*((-1.)**IEY4)*V13			
      CALL SECHSJ(BLI,BLJ,1.,CLA,CLA,BL1I,RES32,IER)				
      BIJ6=E*RES32*RES24*RES30*(1.5**0.5)*((-1.)*IEY4)*V11			
      RETURN									
  800 WRITE (6,801) IER 							
  801 FORMAT(1H0,'FEHLER',I4,'  SECHSJ')					
      RETURN									
  700 WRITE(6,701) IER								
  701 FORMAT(1H0,'FEHLER',I4,'  NEUNJ') 					
  900 RETURN									
      END											
  700 WRITE(6,701) IER
