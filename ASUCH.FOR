CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC	
C										
C PROGRAMM  >>>>> ASUCH.FOR<<<<<						
C										
C ZUR HILFE BEI DER SUCHE NACH A-FAKTOREN					
C										
C VERSION 26.3.85								
C										
C BESCHREIBUNG IN ASUCH.TXT							
C										
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC	
C										
C										
      IMPLICIT REAL*8(A-H,O-Z)							
      GENERIC									
      DIMENSION NMW(35),NMW2(35),NR(99,35),B1(99,35),C1(99,35),D1(99,35)	
      DIMENSION ISOTOP(5) ,A7(99),A1(99,35)					
      DIMENSION A3(99),B3(99),A4(99),JX(99),A6(5),B6(99),A8(99),A5(99)		
      DIMENSION  FO(35),FU(35),IT(6)  ,IX(99)					
      DIMENSION NR3(99),C3(99),D3(99)						
      DIMENSION T1(100),T2(100) 						
      DIMENSION BMIT(100),BSIG(100),NRB(100),BMIT1(100,35),BSIG1(100,35)	
      DIMENSION NRB1(100,35)							
      DIMENSION B2(99,35),DIAG(4,35)						
      CHARACTER*1 TEXT(60)							
      CHARACTER*10 T(100,35)							
      READ (5,1) NMK,XI,XJO,XJU,TEXT						
 1    FORMAT(I4,3F4.1,60A1)							
      IF(ABS(XJO-XJU).LE.1.)GO TO 3						
      WRITE(6,*)'*****FEHLER*****,EINGABEWERT XJO=',XJO,'XJU=',XJU		
      STOP									
 3    IF(NMK.LE.35)GOTO 7							
      WRITE(6,*)'*****FEHLER*****,EINGABEWERT NMK=',NMK,'>35 -> NMK=            
     135 GESETZT'                                                               
      NMK=35									
      GOTO 7									
 7    READ(5,2) NMW								
 2    FORMAT(20I4/20I4) 							
      DO 5J1=1,NMK								
      IF (NMW(J1).GT.99)WRITE(6,*)'*****FEHLER***** DIE ANZAHL                  
     1DER MESSWERTE IN KURVE NR',J1,'IST ZU GROSS: NMW(',J1,')=',NMW(J1)        
      DO 5 I1=1,NMW(J1) 							
      READ(5,4) NR(I1,J1),B1(I1,J1),C1(I1,J1),A1(I1,J1),D1(I1,J1),T(I1, 	
     1J1)									
 4    FORMAT(I4,4F10.2,A10)							
      IF (C1(I1,J1).NE.0.)GO TO 6						
      C1(I1,J1)=1.D0								
 6    IF(D1(I1,J1).NE.0.)GO TO 8						
      D1(I1,J1)=1.D0								
 8    IF(A1(I1,J1).NE.0.)GO TO 5						
      A1(I1,J1)=1.D0								
 5    CONTINUE									
C										
C-----AUSGABE DER EINGABEWERTE							
C										
      CALL TIME(TM,DT)								
      WRITE(6,80) TEXT,TM,DT							
 80   FORMAT('1',T20,'ROUTINE ZUR UNTERSTUETZUNG BEI DER ZUORDNUNG TERM-        
     1FREQUENZ'/T49,'ZUR C09-AUSWERTUNG'//T47,'*****EINGABEWERTE*****'//        
     2T28,60A1/' ',T40,'ZEIT=',A8,10X,'DATUM=',A8//)				
      WRITE(6,81) NMK,XI,XJO,XJU						
 81   FORMAT(' ANZAHL DER MESSKURVEN     NMK :',I4				
     1	    /' KERNSPIN',19X,'XI :',F4.1					
     2	    /' OBERER  HUELLENDREHIMPULS XJO :',F4.1				
     3	    /' UNTERER HUELLENDREHIMPULS XJU :',F4.1/)				
      DO 78 J1=1,NMK								
      WRITE(6,83) J1,NMW(J1)							
 83   FORMAT('0ANZAHL DER MESSWERTE VON MESSKURVE J=',I4,':NMW(J)=',I4//	
     1' NR',T8,'NR DER',T19,'MITTEN',T31,'FREQ',T43,'AMPL',T55,'AMPL',T6	
     27,'BEZ',T90,'DEL-FREQ',T105,'NR'/T8,'TEILKURVE',T19,			
     3'FREQ',T31,'FEHLER',							
     4T55,'FEHLER',T90,'B1(I-1,J)-'/' I',T9,'NR(I,J)',T19,'B1(I,J)',T31,	
     5'C1(I,J)',T42,'A1(I,J)',T55,'D1(I,J)',					
     6T67,'T(I,J)',T90,'B1(I,J)',T105,'I'/)					
      DO 78 I1=1,NMW(J1)							
      WRITE(6,84) I1,NR(I1,J1),B1(I1,J1),C1(I1,J1),A1(I1,J1),D1(I1,J1), 	
     1T(I1,J1)									
 84   FORMAT(' ',I4,2X,I4,3X,4(F10.2,2X),A10)					
 78   IF(I1.GT.1)WRITE(6,79)B1(I1-1,J1)-B1(I1,J1),I1				
 79   FORMAT('+',T90,F10.2,3X,I4)						
C										
C-----ANSCHLUESSE BERECHNEN: NEUER PROGRAMMTEIL-----------------------		
C										
      DO 10 I=1,NMW(1)								
      B2(I,1)=B1(I,1)								
      B3(I)=B1(I,1)								
      C3(I)=C1(I,1)								
      D3(I)=D1(I,1)								
      A3(I)=A1(I,1)								
      NR3(I)=NR(I,1)								
      BMIT(I)=B1(I,1)								
      BSIG(I)=B1(I,1)*B1(I,1)							
 10   NRB(I)=1									
      NDIM=NMW(1)								
      NMW2(1)=NMW(1)								
      NMW(1)=0									
      NRGLEI=0									
      NZAEHL=NMK								
 14   NZAEHL=NZAEHL-1								
      CALL SORT(B3,B3,NDIM,JX)							
      NDIM=NDIM-NRGLEI								
      DO 16 I=1,NDIM								
      A4(I)=D3(JX(I))								
      A5(I)=A3(JX(I))								
      A8(I)=C3(JX(I))								
 16   IX(I)=NR3(JX(I))								
      DO 17 I=1,NDIM								
      D3(I)=A4(I)								
      A3(I)=A5(I)								
      C3(I)=A8(I)								
 17   NR3(I)=IX(I)								
      DO 18 I=1,NDIM								
      A4(I)=BMIT(JX(I)) 							
      A5(I)=BSIG(JX(I)) 							
 18   IX(I)=NRB(JX(I))								
      DO 19 I=1,NDIM								
      BMIT(I)=A4(I)								
      BSIG(I)=A5(I)								
 19   NRB(I)=IX(I)								
      IF (NZAEHL.EQ.0)GO TO 75							
      DO 12 I3=1,NDIM								
      DO 12 J1=1,NMK								
      IF (NMW(J1).EQ.0)GO TO 12 						
      DO 13 I2=1,NMW(J1)							
      IF (NR3(I3).EQ.NR(I2,J1)) GO TO 15					
 13   CONTINUE									
 12   CONTINUE									
      WRITE(6,*)'*****FEHLER*****,ZU NR=',(NR3(I),I=1,NDIM),' GIBT ES KE        
     1INE NUMMER IN DEN ',NZAEHL,' RESTLICHEN KURVEN'                           
      STOP									
 15   NRJ=J1									
C										
C-----BERECHNUNG DER AMPLITUDENPRODUKTE UND FREQUENZSUMMANDEN			
C-----DER NAECHSTEN TEILKURVE							
C										
      SUMFF=0.D0								
      SUMF=0.D0 								
      SUMAF=0.D0								
      PRD2AF=0.D0								
      NRGLEI=0									
      PRODA=1.D0								
      I3=0									
      TMAX=0.D0 								
      DO 20 I1=1,NDIM								
      DO 21 I2=1,NMW(NRJ)							
      IF(NR3(I1).NE.NR(I2,NRJ)) GO TO 21					
      I3=I3+1									
      FF=1/(C3(I1)*C3(I1)+C1(I2,NRJ)*C1(I2,NRJ))				
      SUMFF=SUMFF+FF								
      SUMF=SUMF+(B3(I1)-B1(I2,NRJ))*FF						
      AF=1/((D1(I2,NRJ)/A1(I2,NRJ)/A1(I2,NRJ)*A3(I1))**2+(D3(I1)/A1(I2, 	
     1NRJ))**2) 								
      SUMAF=SUMAF+AF								
      T1(I3)=A3(I1)/A1(I2,NRJ)							
      T2(I3)=AF 								
      TMAX=DMAX1(TMAX,T2(I3))							
      PRD2AF=((D3(I1)/A3(I1))**2+(D1(I2,NRJ)/A1(I2,NRJ))**2)*AF*AF		
     1+PRD2AF									
      NRGLEI=NRGLEI+1								
 21   CONTINUE									
 20   CONTINUE									
      DO 23 I1=1,I3								
 23   PRODA=PRODA*T1(I1)**(T2(I1)/TMAX) 					
      PRODA=PRODA**(TMAX/SUMAF) 						
      DIAG(1,NRJ)=SUMF/SUMFF							
      DIAG(2,NRJ)=1/DSQRT(SUMFF)						
      DIAG(3,NRJ)=PRODA 							
      DIAG(4,NRJ)=DSQRT(PRD2AF)*PRODA/SUMAF					
      DO 22 I1=1,NMW(NRJ)							
      B1(I1,NRJ)=B1(I1,NRJ)+DIAG(1,NRJ) 					
      B2(I1,NRJ)=B1(I1,NRJ)							
      BMIT1(I1,NRJ)=B1(I1,NRJ)							
      BSIG1(I1,NRJ)=B1(I1,NRJ)*B1(I1,NRJ)					
      NRB1(I1,NRJ)=1								
      C1(I1,NRJ)=DSQRT(C1(I1,NRJ)*C1(I1,NRJ)+1/SUMFF)				
      AM=A1(I1,NRJ)								
      A1(I1,NRJ)=A1(I1,NRJ)*PRODA						
 22   D1(I1,NRJ)=DSQRT((A1(I1,NRJ)*D1(I1,NRJ)/AM)**2+(PRD2AF*(A1(I1,NRJ)	
     1/SUMAF)**2))								
C										
C-----MITTELWERTE BILDEN UND IN B3 ABSPEICHERN					
C										
      DO 30 I1=1,NDIM								
      DO 31 I2=1,NMW(NRJ)							
      IF (NR3(I1).NE.NR(I2,NRJ)) GO TO 31					
      BMIT1(I2,NRJ)=BMIT(I1)+B1(I2,NRJ) 					
      BSIG1(I2,NRJ)=BSIG(I1)+BSIG1(I2,NRJ)					
      NRB1(I2,NRJ)=NRB(I1)+1							
      SUMFF=1/C3(I1)/C3(I1)+1/C1(I2,NRJ)/C1(I2,NRJ)				
      B1(I2,NRJ)=(B1(I2,NRJ)/C1(I2,NRJ)/C1(I2,NRJ)+B3(I1)/C3(I1)/		
     1C3(I1))/SUMFF								
      C1(I2,NRJ)=DSQRT(1/SUMFF) 						
      SUMFA=1/D1(I2,NRJ)/D1(I2,NRJ)+1/D3(I1)/D3(I1)				
      PRODA=A1(I2,NRJ)**(1/D1(I2,NRJ)/D1(I2,NRJ)/SUMFA)*A3(I1)			
     1**(1/D3(I1)/D3(I1)/SUMFA) 						
      D1(I2,NRJ)=PRODA/SUMFA*DSQRT((1/D1(I2,NRJ)/A1(I2,NRJ))**2+		
     1(1/D3(I1)/A3(I1))**2)							
      A1(I2,NRJ)=PRODA								
      B3(I1)=-9D9								
 31   CONTINUE									
 30   CONTINUE									
      DO 40 I=1,NMW(NRJ)							
      A3(NDIM+I)=A1(I,NRJ)							
      C3(NDIM+I)=C1(I,NRJ)							
      D3(NDIM+I)=D1(I,NRJ)							
      NR3(NDIM+I)=NR(I,NRJ)							
      B3(NDIM+I)=B1(I,NRJ)							
      BMIT(NDIM+I)=BMIT1(I,NRJ) 						
      BSIG(NDIM+I)=BSIG1(I,NRJ) 						
 40   NRB(NDIM+I)=NRB1(I,NRJ)							
      NDIM=NDIM+NMW(NRJ)							
      NMW2(NRJ)=NMW(NRJ)							
      NMW(NRJ)=0								
      GO TO 14									
C										
C-----ORDNUNG DER AMPLITUDEN,FREQUENZEN NACH AMPLITUDEN UND BERECHNUNG		
C-----RELEVANTER    GROESSEN							
C										
 75   JRNMW2=NDIM								
      CALL SORT(A3,A4,JRNMW2,JX)						
      DATA A6/23.7,16.5,15.8,9.6,9.1/,ISOTOP/98,96,92,100,94/			
      CB3=B3(JX(1))								
      DO 70 KA1=1,5								
      A5(KA1)=A4(KA1)/A4(1)*A6(1)						
 70   B3(KA1)=B3(KA1)-CB3							
      DO 71 KA2=6,JRNMW2							
      A5(KA2)=A4(KA2)/A4(6)*100 						
      A7(KA2)=A5(KA2)*15.7/9.5							
 71   B3(KA2)=B3(KA2)-CB3							
      DO 72 KA3=1,JRNMW2							
      DO 73 KA4=1,JRNMW2							
      KX=KA4									
 73   IF(KA3.EQ.JX(KA4)) GO TO 74						
 74   IX(KA3)=KX								
 72   B6(KA3)=B3(JX(KA3))							
      CALL INTTH(XI,XJO,XJU,A8,IA8,FO,FU)					
C										
C-----ERGEBNIS DER ZUSAMMENFUEGUNG						
C										
      WRITE(6,95)								
 95   FORMAT('1',T28,'ERGEBNIS DER ZUSAMMENFUEGUNG DER MESSKURVEN ZUM GE        
     1SAMTSPEKTRUM'//'	 I  KURVEN    FREQ',10X,'FREQ',6X,'AMPLITUDE',4X        
     2,'AMPLITUDEN',7X,'UNGEW FREQ  UNGEW FREQ  DREIFACHE',			
     33X,'UEBEREINST   I'/8X,'NR',19X,'FEHLER',19X,'FEHLER',			
     410X,'MITTELWERT STANDARDABW STANDARDABW',4X,'FREQ'/			
     56X,'NR3(I)',4X,'B3(I)',9X,'C3(I)',6X,'A3(I)',8X,'D3(I)',13X,		
     6'BMIT(I)',5X,'BSIG(I)   3*BSIG(I)',5X,'NRB(I)'//) 			
      CBMIT=BMIT(JX(1))/NRB(JX(1))						
      DO 96 I=1,JRNMW2								
      IF(NRB(I).LE.1)GO TO 94							
      BSIG(I)=DSQRT((BSIG(I)-BMIT(I)*BMIT(I)/NRB(I))/(NRB(I)-1))		
 94   IF(NRB(I).LE.1)BSIG(I)=0.D0						
      BMIT(I)=BMIT(I)/NRB(I)-CBMIT						
 96   WRITE(6,97) I,NR3(I),B3(I),C3(I),A3(I),D3(I),BMIT(I),BSIG(I),		
     13*BSIG(I),NRB(I),I							
 97   FORMAT (I4,3X,I4,2X,4(F10.2,2X),8X,F10.2,2X,F10.2,2X,F10.2,		
     15X,I4,5X,I4)								
C										
C-----DIAGNOSETEIL:TEST DER EINZELAWEICHUNGEN DER KURVEN VON DEM BERECHN	
C-----MITTELWERT								
C										
      WRITE(6,56)								
 56   FORMAT('1',T28,'DIAGNOSE ZUR UEBERPRUEFUNG DER QUALITAET',		
     1' DER EINZELKURVEN'///' NR DER',T10,'ANZ DER',T20,'VERSCHIEBUNG', 	
     1T34,'VERSCHIEBUNG',T49,'AMPL FAKT',T61,'AMPL FAKT',T74,			
     2'MITT VERH',T87,'STANDARD',T99,'ANZ DER',T107,'NR DER'/			
     3' KURVE',T10,'MESSW',T37,'FEHLER',T62,'FEHLER',T74,			
     4'FREQ/MIT-F',T87,'ABWEICH',T100,'WERTE ',T107,'KURVE'/			
     5'   J',T10,'NMW(J)',T21,'DIAG(1,J)',T35,'DIAG(2,J)',T49,			
     6'DIAG(3,J)',T61,'DIAG(4,J)',T75,'BMITTE',T88,'BQUAD',T99 ,		
     7'NRQUOT',T109,'J'//)							
      DO 50 J=1,NMK								
      DIAG(1,J)=DIAG(1,J)-CB3							
      DO 50 I=1,NMW2(J) 							
 50   B2(I,J)=B2(I,J)-CB3							
      DO 51 J=1,NMK								
      BMITTE =0.D0								
      BQUAD  =0.D0								
      NRQUOT =0 								
      DO 52 I=1,NMW2(J) 							
      IF(ABS(B2(I,J)).LT.30.D0)GO TO 52 					
      DO 53 K=1,JRNMW2								
 53   IF (NR(I,J).EQ.NR3(K)) GO TO 54						
 54   NRQUOT =NRQUOT+1								
      B2(I,J)=B3(K)/B2(I,J)							
      BMITTE =BMITTE +B2(I,J)							
      BQUAD  =BQUAD  +B2(I,J)*B2(I,J)						
 52   CONTINUE									
      WRITE(6,55) J,NMW2(J),DIAG(1,J),						
     1DIAG(2,J),DIAG(3,J),DIAG(4,J),NRQUOT,J					
      IF (NRQUOT.GT.1) WRITE(6,57) BMITTE/NRQUOT,				
     1DSQRT((BQUAD-BMITTE*BMITTE/NRQUOT)/(NRQUOT-1))				
 51   CONTINUE									
 55   FORMAT(' ',I4,T10,I4,T18,F10.2,T32,F10.2,T47,				
     1F10.2,T59,F10.2,T99,I4,4X,I4)						
 57   FORMAT('+',T70,F10.2,T83,F10.2)						
C										
C=======================================================================	
C										
C=====AUSGABETEIL								
C										
C										
C-----ERSTE TABELLE DER AUSGABEWERTE,ANORDNUNG DER FREQUENZEN NACH AMPLI	
C										
      WRITE(6,86) TEXT								
 86   FORMAT('1',T47,'*****AUSGABEWERTE*****'//T28,60A1///			
     1					       T36,'ZUORDNUNG DER 5GERAD        
     2EN ISOTOPE,NACH AMPLITUDEN GEORDNET'//' NR    ISOTOP	HAEUFIGK	
     3EIT  AMPL NORMIERT  ZUG FREQ'/32X,'AUF A6(1)'/' KA    ISOTOP(KA)		
     4A6(KA)',7X,'A5(KA)',9X,'B6(KA)'/)                                         
      DO 88 KA=1,5								
 88   WRITE(6,87) KA,ISOTOP(KA),A6(KA),A5(KA),B6(KA)				
 87   FORMAT(' ',I4,2X,I4,8X,F10.1,3X,F10.1,5X,F10.2)				
      WRITE(6,89)								
 89   FORMAT('0',T30,'THEORETISCHE UND GEMESSENE AMPLITUDEN DER BEIDEN U        
     1NGERADEN ISOTOPE,'/T51,'NACH AMPLITUDEN GEORDNET:'/)                      
      WRITE(6,90)								
 90   FORMAT(' NR',10X,'AMPL FUER',20X,'THEORET  OBERER',6X,'UNTERER',20	
     1X,'AMPL FUER  NR IN    FREQ IN'/13X,'MO95',25X,'AMPL',5X, 		
     2'GESDREHIMP  GESDREHIMP',17X,'MO97',7X,'UNTERER TABELLE'/ 		
     3' KA',10X,'A5(KA+5)'							
     4,21X,'A8(KA)   FO(KA)',6X,'FU(KA)',21X,'A7(KA+5)',3X,			
     5'JX(KA+5) B6(KA+5)'/)							
      DO 92 KA=1,IA8								
 92   WRITE(6,91) KA,A5(KA+5),A8(KA),FO(KA),FU(KA),A7(KA+5),JX(KA+5),B6(	
     1KA+5)									
 91   FORMAT(' ',I4,8X,F7.2,22X,F7.2,2X,F4.1,8X,F4.1,23X,F7.2,4X,I4,3X,F	
     110.2)									
      DO 85 KA=IA8+1,JRNMW2-5							
 85   WRITE(6,93) KA,A5(KA+5),A7(KA+5),JX(KA+5),B6(KA+5)			
 93   FORMAT(' ',I4,8X,F7.2,70X,F7.2,4X,I4,3X,F10.2)				
C										
C-----EINIGE VERSUCHE ZUR ZUORDNUNG						
C										
      WRITE(6,100)								
 100  FORMAT('1TABELLE DER ZUORDNUNGEN FREQUENZ-QUANTENZAHL NACH FREQUEN        
     1Z GEORDNET,'/' WOBEI DIE THEORETISCHEN WERTE ERRECHNET WURDEN,'/          
     2' INDEM 3 FREQUENZWERTE AUFGRUND IHRER INTENSITAETEN DEN QUANTENZA        
     3HLEN FO,FU ZUGEORDNET WERDEN'/' UND DIE SICH ERGEBENDEN A-FAKTOREN	
     4 BENUTZT WERDEN,'/' DIE RESTLICHEN FREQUENZEN ZU BERECHNEN'//)            
      FLAG=0.D0 								
      I=0									
 98   FORMAT(8I4,6F8.2) 							
 99   READ(5,98,ERR=105,END=105)INDEX,IT,ITEST,TS5,DTS5,TAO,DTAO,TAU,		
     1DTAU									
      WRITE(6,106)INDEX,ITEST,IT,TS5,DTS5,TAO,DTAO,TAU,DTAU			
 106  FORMAT(' INDEX        :',I4/						
     1	     ' ITEST        :',I4/						
     2	     ' DER VEKTOR IT:',6I4/						
     3	     ' DIE SCHWERPUNKTSVERSCHIEBUNG:',F10.2,				
     4	     '  UND DEREN  ERLAUBTE ABWEICHUNG:',F10.2/ 			
     5	     ' DER OBERE  A-FAKTOR         :',F10.2,				
     6	     '  UND DESSEN ERLAUBTE ABWEICHUNG:',F10.2/ 			
     7	     ' DER UNTERE A-FAKTOR         :',F10.2,				
     8	     '  UND DESSEN ERLAUBTE ABWEICHUNG:',F10.2///)			
      IF(INDEX.NE.0)I=6 							
 101  K=0									
 103  IF((XJO.NE.XJU).OR.(FO(IT(4)).NE.FU(IT(4))).OR.(FO(IT(5)).NE.FU(IT	
     1(5))).OR.(FO(IT(6)+K).NE.FU(IT(6)+K)))   GO TO 102			
      K=K+1									
      GO TO 103 								
 102  IF(IT(1).GT.JRNMW2.OR.IT(2).GT.JRNMW2.OR.IT(3).GT.JRNMW2			
     1.OR.IT(4).GT.IA8.OR.IT(5).GT.IA8.OR.IT(6).GT.IA8) 			
     2GO TO 105 								
      IF(IT(1)*IT(2)*IT(3).EQ.0)   GO TO 150					
      CALL PRINT(XI,XJO,XJU,B6(IT(1)),B6(IT(2)),B6(IT(3)+K),FO(IT(4)),FO	
     1(IT(5)),FO(IT(6)+K),FU(IT(4)),FU(IT(5)),FU(IT(6)+K),IA8,FO,FU,B6,B	
     23,JRNMW2,A8,JX,ITEST,IX,TS5,DTS5,TAO,DTAO,TAU,DTAU)			
      IF(FLAG.NE.1.)GO TO 99							
C										
C-----STANDARDSUCHPROGRAMM 4 ZUORDNUNGEN FREQUENZEN-UEBERGAENGE NACH		
C-----INTENSITAETEN								
C										
 105  FLAG=1.D0 								
      I=I+1									
      GO TO (110,120,130,140),I 						
      STOP									
 110  DO 111 J=1,3								
      IT(J)=5+J 								
 111  IT(J+3)=J 								
      GO TO 101 								
 120  IT(3)=9									
      GO TO 102 								
 130  IT(2)=8									
      IT(3)=9									
      GO TO 102 								
 140  IT(3)=7									
      GO TO 102 								
C										
C-----SCAN ,D.H. FUER IT(X)=0(X=1,2 ODER 3) WIRD DER ENTSPRECHENDE UEBER	
C-----FO(IT(X+3))-FU(IT(X+3))ALLEN GEMESSENEN FREQUENZEN NACHEINANDER ZU	
C-----GEORDNET									
C										
 150  KA1=IT(1) 								
      KA2=IT(2) 								
      KA3=IT(3) 								
      KE1=IT(1) 								
      KE2=IT(2) 								
      KE3=IT(3) 								
      IF(IT(1).EQ.0)   KA1=1							
      IF(IT(1).EQ.0) KE1=JRNMW2 						
      IF(IT(2).EQ.0)   KA2=1							
      IF(IT(2).EQ.0)   KE2=JRNMW2						
      IF(IT(3).EQ.0)   KA3=1							
      IF(IT(3).EQ.0)   KE3=JRNMW2						
      ITEST=1									
      DO 155 IT1=KA1,KE1							
      DO 155 IT2=KA2,KE2							
      DO 155 IT3=KA3,KE3							
      IF((IT1.EQ.IT2).OR.(IT2.EQ.IT3).OR.(IT3.EQ.IT1))	GO TO 155		
      CALL PRINT(XI,XJO,XJU,B6(IT1  ),B6(IT2  ),B6(IT3	  ),FO(IT(4)),FO	
     1(IT(5)),FO(IT(6)+K),FU(IT(4)),FU(IT(5)),FU(IT(6)+K),IA8,FO,FU,B6,B	
     23,JRNMW2,A8,JX,ITEST,IX,TS5,DTS5,TAO,DTAO,TAU,DTAU)			
 155  CONTINUE									
      GO TO 99									
      END									
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC	
C										
C UNTERPROGRAMM PRINT								
C										
C AUFGABE:									
C ZU DEN ZUORDNUNGEN   FO1 - FU1 -> FREQUENZ A					
C		       FO2 - FU2 -> FREQUENZ B					
C		       FO3 - FU3 -> FREQUENZ C					
C 1. DIE AFAKTOREN FINDEN (UNTERPROGRAMM AFAKT),				
C 2. AUSDRUCKEN DER SICH  DARAUS ERGEBENDEN FREQUENZEN(UNTERPROGRAMM FRE	
C 3. VERIFIKATION DER SIEBBEDINGUNGEN.						
C										
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC	
C										
C										
      SUBROUTINE PRINT(XI,XJO,XJU,A,B,C,FO1,FO2,FO3,FU1,FU2,FU3,IA,FO,FU	
     1,B6,B3,JRNMW2,A8,JX,ITEST,IX,TS5,DTS5,TAO,DTAO,TAU,DTAU)			
      IMPLICIT REAL*8(A-H,O-Z)							
      DIMENSION B5(99),FO(99),FU(99),B7(99),	   B3(99),JY5(99),JY7(99	
     1),B6(99),A8(99),B8(99),B9(99),J8(99),JX(99),IX(99),TE3(99)		
      I=1									
      K=1									
      JR=JRNMW2-5								
      DO 15 KA=1,JR								
 15   B8(KA)=B6(KA+5)								
      CALL SORT(B8,B9,JR,J8)							
      CC=C									
      XFO3=FO3									
      XFU3=FU3									
 13   CALL AFAKT(XI,XJO,XJU,A,B,CC,FO1,FO2,XFO3,FU1,FU2,XFU3,AO,AU,S)		
      IF((AO.EQ.0.D0).AND.(AU.EQ.0.D0))  RETURN 				
      IF(DABS(TS5).GT.1.D-05.AND.DABS(TS5-S ).GT.DTS5)GO TO (10,14),I		
      IF(DABS(TAO).GT.1.D-05.AND.DABS(TAO-AO).GT.DTAO)GO TO (10,14),I		
      IF(DABS(TAU).GT.1.D-05.AND.DABS(TAU-AU).GT.DTAU)GO TO (10,14),I		
      CALL FREQ(AO,AU,S,B5,FO,FU,IA,XJO,XJU,XI,JY5)				
      S7=S									
      IF((JR.LT.1.2*IA).OR.(A8(JY5(1)).LT.15.).OR.(A8(JY5(IA)).LT.15.)) 	
     1GO TO 16									
      IF(B9(1) -B5(1) .LT.20.D0)S7=B9(JR)-(B5(IA)-S)/0.979D0			
      IF(B5(IA)-B9(JR).LT.20.D0)S7=B9(1) -(B5(1) -S)/0.979D0			
 16   CALL FREQ(AO/0.979,AU/0.979,S7,B7,FO,FU,IA,XJO,XJU,XI,JY7)		
 100  FORMAT(100(F10.0))							
      IF(S.EQ.S7) GO TO 17							
      IF((ABS(B9(1)-DMAX1(B5(1),B7(1))).GT.100.).AND.(ABS(B9(JR)-		
     1DMIN1(B5(IA),B7(IA))).GT.100.))	GO TO (10,14),I 			
 17   IF((ABS(B9(JR)-B5(IA)).GT.1000.).OR.(ABS(B9(1)-B5(1)).GT.1000.))		
     1GO TO (10,14),I								
      IKT=0									
      DO 19 IF1=1,IA								
      IF((B5(IF1).GT.B3(1)+20.).OR.(B5(IF1).LT.B3(JRNMW2)-20.)			
     1.OR.(A8(JY5(IF1)).LT.15.)) GO TO 19					
      TE1=ABS(B5(IF1)-B6(1))							
      DO 18 IF2=1,JRNMW2							
      TE1= DMIN1(TE1,ABS(B5(IF1)-B6(IF2)))					
      IF(TE1.EQ.ABS(B5(IF1)-B6(IF2))) TE2=B5(IF1)				
 18   CONTINUE									
      IKT=IKT+1 								
      TE3(IKT)=TE2								
      IF(TE1.GT.150.)GO TO (10,14),I						
 19   CONTINUE									
      IF(IKT.LE.5) GO TO (10,14),I						
      DO 21 IF1=1,IKT								
      DO 21 IF2=1,IKT								
      DO 20 IF3=1,IKT								
      IF((IF2.EQ.IF1).OR.(IF2.EQ.IF3).OR.(IF1.EQ.IF3)) GO TO 20 		
      IF((TE3(IF1).EQ.TE3(IF2)).AND.(TE3(IF1).EQ.TE3(IF3)).AND. 		
     1(TE3(IF2).EQ.TE3(IF3)))GO TO (10,14),I					
 20   CONTINUE									
 21   CONTINUE									
C										
C-----AUSGABETEIL, AUSGABE DER A-FAKTOREN UND DER RUECKGERECHNETEN FREQU	
C										
      WRITE(6,1)A,FO1,FU1,B,FO2,FU2,CC,XFO3,XFU3				
 1    FORMAT('0 FESTGEHALTEN WURDEN FREQ1=',F10.2,'MIT FO=',F4.1,'UND FU        
     1=',F4.1/22X,'FREQ2=',F10.2,'MIT FO=',F4.1,'UND FU=',F4.1/22X,'FREQ	
     23=',F10.2,'MIT FO=',F4.1,'UND FU=',F4.1//)                                
      WRITE(6,2)								
 2    FORMAT(' NR    THEORET FREQ  ZUG     ZUG     NR',T50,			
     1'NR    GEMESSENE   NR IN',T90,'THEORET FREQ  ZUG     ZUG     NR           
     2 NR'/T8,'FUER MO95     FO      FU',T56,'FREQUENZEN  OB TAB',T90,          
     3'FUER MO97     FO      FU'/' KF    B5(MF)',8X,'FO(MA)  FU(MA)  MA         
     4',T50,'KF    B3(KF)      KA',T90,'B7(NF)',8X,'FO(NA)  FU(NA)  NA		
     5	KF'/)                                                                   
      DO 3 KF=1,IA								
 3    WRITE(6,4)KF,B5(KF),FO(JY5(KF)),FU(JY5(KF)),JY5(KF),KF,B3(KF),IX(K	
     1F),B7(KF),FO(JY7(KF)),FU(JY7(KF)),JY7(KF),KF				
 4    FORMAT(' ',I4,2X,F10.2,4X,F4.1,4X,F4.1,4X,I4,T50,I4,2X,F10.2,		
     12X,I4,T90,F10.2,4X,F4.1,4X,F4.1,4X,I4,4X,I4)				
      DO 5 KF=IA+1,JRNMW2							
 5    WRITE(6,6) KF,B3(KF),IX(KF)						
 6     FORMAT(T50,I4,2X,F10.2,2X,I4)						
      AO1=AO/0.979								
      AU1=AU/0.979								
      WRITE(6,7) AO,AO1,AU,AU1,S,S7						
 7    FORMAT(T8 ,'AO=',F10.2,T90,'AO=',F10.2/T8 ,'AU=',F10.2,T90,'AU=',F	
     110.2/T8 ,' S=',F10.2,T90,' S=',F10.2)					
C										
C-----ENDE DES AUSGABETEILS							
C										
      IF(ITEST.EQ.1)  GO TO 10							
      IF(I.EQ.2)GO TO 14							
      I=2									
      DO8 KF=1,IA								
      JF=IA-KF+1								
      CTH=B5(JF)								
 8    IF((A8(JY5(JF)).GT.10.).AND.((FO1.NE.FU1).OR.(FO2.NE.FU2).OR.(FO(J	
     1Y5(JF)).NE.FU(JY5(JF)))))   GO TO 9					
      GO TO 10									
 9    DO 11 KF=1,JRNMW2 							
      NF=KF									
 11   IF(ABS(B3(KF)-CTH).LE.150.) GO TO 12					
      GO TO 10									
 12   XFO3=FO(JY5(JF))								
      XFU3=FU(JY5(JF))								
      CC=B3(NF) 								
      GO TO 13									
 14   IF(ABS(B3(NF+K)-CTH).GT.150.)GO TO 10					
      CC=B3(NF+K)								
      K=K+1									
      GO TO 13									
 10   RETURN									
      END									
      SUBROUTINE SORT(A,B,N,J)							
      IMPLICIT REAL*8 (A-H,O-Z) 						
      DIMENSION A(N),B(N),J(N)							
      DIMENSION  C(99)								
      DO 3 M=1,N								
 3    C(M)=A(M) 								
      DO 1 K=1,N								
      D=C(1)									
      DO 2 L=1,N								
      D=DMAX1(C(L),D)								
      IF(D.NE.C(L))GO TO 2							
      J(K)=L									
 2    CONTINUE									
      B(K)=D									
      C(J(K))=-9.D+20								
 1    CONTINUE									
      RETURN									
      END									
      SUBROUTINE AFAKT(XI,XJO,XJU,A,B,D,FO1,FO2,FO3,FU1,FU2,FU3,AO,AU,S)	
      IMPLICIT REAL*8 (A-H,P-Z) 						
      C(X,Y,Z)=0.5D0*(Z*(Z+1.D0)-Y*(Y+1.D0)-X*(X+1.D0)) 			
      AO=0.D0									
      AU=0.D0									
      S=0.D0									
      D1=A-B									
      D2=B-D									
      D11=C(XI,XJO,FO1)-C(XI,XJO,FO2)						
      D12=C(XI,XJO,FO2)-C(XI,XJO,FO3)						
      D21=C(XI,XJU,FU1)-C(XI,XJU,FU2)						
      D22=C(XI,XJU,FU2)-C(XI,XJU,FU3)						
      IF(ABS(D22*D11-D12*D21).LT.0.0001)  RETURN				
      AU=(D1*D12-D2*D11)/(D22*D11-D21*D12)					
      AO=(D1+D21*AU)/D11							
      S=-C(XI,XJO,FO1)*AO+C(XI,XJU,FU1)*AU+A					
      RETURN									
      END									
      SUBROUTINE FREQ(AO,AU,S,B,FO,FU,I,XJO,XJU,XI,JY)				
      IMPLICIT REAL*8 (A-H,O-Z) 						
      DIMENSION B(I),FO(I),FU(I),JY(I)						
      DIMENSION C(99)								
      CF(X,Y,Z)=0.5D0*(Z*(Z+1.D0)-X*(X+1.D0)-Y*(Y+1.D0))			
      DO 1 K=1,I								
 1    C(K)=S+CF(XI,XJO,FO(K))*AO-CF(XI,XJU,FU(K))*AU				
      CALL SORT(C,B,I,JY)							
      RETURN									
      END									
      SUBROUTINE INTTH(XI,XJO,XJU,A,I,FO,FU)					
      IMPLICIT REAL*8 (A-H,O-Z) 						
      DIMENSION A(I),FO(I),FU(I)						
      DIMENSION B(99),YFO(99),YFU(99),J(99)					
      ISUM=1									
      I1=2*XJU									
      I2=2*XI									
      IF(XJO-XJU)10,20,30							
 10   DO 11 I3=ABS(I1-I2),I1+I2,2						
      I4=I3-2									
      IF(I4.LT.ABS(2*XJO-I2)) GO TO 12						
      B(ISUM)=(I3+I1-I2-2)*(I3+I1-I2)*(I1+I2+I3+2.)*(I1+I2+I3)/I3		
      YFU(ISUM)=I3/2.								
      YFO(ISUM)=I4/2.								
      ISUM=ISUM+1								
 12   I4=I3									
      IF(I4.GE.I1+I2)GO TO 11							
      IF(I4.LT.ABS(2*XJO-I2)) GO TO 13						
      B(ISUM)=(2*I3+2)*(I3+I1-I2)*(I2-I1+2+I3)*(I2+I1-I3)*(I1+I2+I3+2.)/	
     1I3/(I3+2.)								
      YFU(ISUM)=I3/2.								
      YFO(ISUM)=I4/2.								
      ISUM=ISUM+1								
 13   I4=I3+2									
      IF(I4.GE.I1+I2)GO TO 11							
      B(ISUM)=(I3+I2-I1+2)*(I1+I2-I3)*(I3+I2-I1+4)*(I1+I2-I3-2)/(I3+2.) 	
      YFU(ISUM)=I3/2.								
      YFO(ISUM)=I4/2.								
      ISUM=ISUM+1								
 11   CONTINUE									
      GO TO 40									
 20   DO 21 I3=ABS(I2-I1),I1+I2,2						
      I4=I3+2									
      IF(I4.GT.I1+I2)GO TO 22							
      B(ISUM)=(I3-I2+I1+2)*(I3+I2-I1+2)*(I3+I2+I1+4)*(I2+I1-I3)/(I3+2.) 	
      YFU(ISUM)=I3/2.								
      YFO(ISUM)=I4/2.								
      ISUM=ISUM+1								
 22   I4=I3									
      B(ISUM)=(2*I3+2.)*(I3*(I3+2)-I2*(I2+2)+I1*(I1+2))**2/I3/(I3+2.)		
      YFU(ISUM)=I3/2.								
      YFO(ISUM)=I4/2.								
      ISUM=ISUM+1								
      I4=I3-2									
      IF(I4.LT.ABS(I2-I1))GO TO 21						
      B(ISUM)=(I3-I2+I1)*(I3+I2-I1)*(I2+I1+I3+2.)*(I2+I1+2.-I3)/I3		
      YFU(ISUM)=I3/2.								
      YFO(ISUM)=I4/2.								
      ISUM=ISUM+1								
  21  CONTINUE									
      GO TO 40									
 30   DO 31 I3=ABS(I1-I2),I1+I2,2						
      I4=I3+2									
      B(ISUM)=(I4+I1-I2)*(I4+I1+2-I2)*(I2+I1+4+I4)*(I2+I1+2.+I4)/I4		
      YFU(ISUM)=I3/2.								
      YFO(ISUM)=I4/2.								
      ISUM=ISUM+1								
      I4=I3									
      IF(I4.LT.ABS(2*XJO-I2))  GO TO 31 					
      B(ISUM)=(2*I4+2)*(I4+I1+2-I2)*(I2-I1+I4)*(I2+I1+I4+4)*(I1+I2-I4+2.	
     1)/I4/(I4+2.)								
      YFU(ISUM)=I3/2.								
      YFO(ISUM)=I4/2.								
      ISUM=ISUM+1								
      I4=I3-2									
      IF(I4.LT.ABS(2*XJO-I2))  GO TO 31 					
      B(ISUM)=(I4+I2-I1)*(I1+2+I2-I4)*(I4+I2-I1+2)*(I1+I2-I4)/(I4+2.)		
      YFU(ISUM)=I3/2.								
      YFO(ISUM)=I4/2.								
      ISUM=ISUM+1								
 31   CONTINUE									
 40   I=ISUM-1									
      CALL SORT(B,A,I,J)							
      A1=A(1)									
      DO 41K=1,I								
      A(K)=A(K)/A1*100								
      FO(K)=YFO(J(K))								
 41   FU(K)=YFU(J(K))								
      RETURN									
      END									
=A