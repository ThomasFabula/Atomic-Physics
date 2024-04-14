C										
C     LEAST SQUARES FITTING OF PARAMETERS TO LEVELS				
C										
C     WRITTEN BY BOB JULKE							
C     REVISED BY CYNTHIA CHAMOT, MARTY KRAIMER, MARK FRED, AND			
C     STEPHANUS BUETTGENBACH							
C										
C     DESCRIPTION OF THE PROBLEM:						
C     GIVEN A SET OF COUPLING COEFFICIENTS COEFF(I,K,P,M)			
C     (I,K=1,...,RANK(M); P=1,...,MAXPAR; M=1,...,MAXSM)			
C     AND OBSERVED TERMS LEVEL(J,M) (J=1,...,RANK(M))				
C     DETERMINE THE INTERACTION PARAMETERS PARAM(P), THE COEFFICIENTS		
C     FOR A SECOND ORDER CORRECTION TO THE H MATRIX ETA(IATETA) 		
C     (IATETA=1,...,MAXETA), THE ADJUSTED EIGENVALUES EVAL(J,M),		
C     AND THE SET OF ORTHONORMAL EIGENVECTORS OF H.				
C										
C     PROGRAMM BESTEHT AUS DEM HAUPTPROGRAMM UND DEN UNTERPROGRAMMEN		
C     EIGEN(EIGENWERTE UND EIGENVEKTOREN EINER SYMMETRISCHEN MATRIX)		
C     UND MATINV(MATRIXINVERSION).						
C										
C     EINGABEDATEN:								
C     KARTE  SPALTE    FORMAT  VARIABLE        BEMERKUNG			
C	 1   1-80      10A8    TITLE	       BELIEBIGER TEXT			
C     *******************************************************************	
C	 2   1-80      10A8    TITLE	       BELIEBIGER TEXT			
C     *******************************************************************	
C	 3   1-3       I3      MAXSM	       ZAHL DER UNTERMATRIZEN		
C					       <=14				
C	     4-6       I3      MAXPAR	       ZAHL DER PARAMTER <=50		
C	     7-9       I3      MAXITR	       MAXIMALE ZAHL VON		
C					       ITERATIONEN			
C	     10-12     I3      MAXETA	       ZAHL DER ETAS <=1		
C	     13-15     I3      MAXCHG	       0: KEIN EFFEKT			
C					       1: MAXIMALE AENDERUNG VON	
C					       ETAS UND PARAMETERN = 300	
C	     16-18     I3      PCOEFF	       0: KOEFFICIENTEN WERDEN		
C					       NICHT GEDRUCKT			
C					       -1:WERDEN GEDRUCKT		
C	     19-21     I3      PEVEC	       0: EIGENVEKTOREN WERDEN		
C					       NICHT GEDRUCKT			
C					       1: WERDEN NACH KONVERGENZ	
C					       GEDRUCKT 			
C					       2: WERDEN NACH JEDER		
C					       ITERATION GEDRUCKT		
C					       3: WERDEN NACH KONVERGENZ	
C					       ODER NACH MAXITR ITERATI-	
C					       ONEN GEDRUCKT			
C	     22-61     5A8     INFO	       BELIEBIGER TEXT			
C	     62-64     I3      LOCH	       0: EINGABE DER TERMBE-		
C					       SCHREIBUNGEN IM FORMAT		
C					       6(A8,4X) PRO KARTE VOM		
C					       TYP 7; KEIN SINNVOLLER		
C					       STANZOUTPUT MOEGLICH,DES-	
C					       HALB ZUSAETZLICHE KARTE		
C					       //GO.FT07F001 DD DUMMY		
C					       1:KOEFFIZIENTEN, SL-GJ-		
C					       FAKTOREN UND TERMBESCHREI	
C					       BUNGEN MUESSEN IN DEM VOM	
C					       PROGRAMM 'FSMATRIX' ER-		
C					       STELLTEN FORMAT EINGEGE- 	
C					       BEN WERDEN;EIGENVEKTOREN 	
C					       WERDEN GESTANZT; FALLS		
C					       KEIN STANZOUTPUT ER-		
C					       WUENSCHT, ZUSAETZLICHE		
C					       KARTE				
C					       //GO.FT07F001 DD DUMMY		
C					       ERFORDERLICH			
C					       2: WIE LOCH=1,ABER DIE		
C					       KARTEN VOM TYP 7' WERDEN         
C					       AUS EINER ANDEREN DATEI		
C					       GELESEN ALS DIE KARTEN		
C					       VOM TYP 4-6			
C					       3: WIE LOCH=1, ABER EIGEN	
C					       VEKTOREN WERDEN AUCH FUER	
C					       ZUSTAENDE OHNE EXPERIMEN-	
C					       TELL BEKANNTE ENERGIEN		
C					       GESTANZT 			
C	     65-67     I3      JKZ	       0: HUELLENDREHIMP. J  IST GANZZ. 
C					       1: J IST HALBZAHLIG		
C					       (NUR WICHTIG FUER STANZEN)	
C	     68-70     I3      PDERIV	       0: ABLEITUNGEN WERDEN		
C					       NICHT GEDRUCKT			
C					       1: WERDEN NACH KONVERGENZ	
C					       GEDRUCKT 			
C					       2: WERDEN NACH JEDER		
C					       ITERATION GEDRUCKT		
C     *******************************************************************	
C     FUER JEDE UNTERMATRIX MUSS 1 KARTE VOM TYP 4 VORHANDEN SEIN.		
C     HINTER JEDER KARTE VOM TYP 4 KOENNEN SOVIEL KARTEN VOM TYP 5 WIE		
C     NOTWENDIG FOLGEN. NUR KOEFFIZIENTEN MIT I<=K DUERFEN EINGEGEBEN		
C     WERDEN. NUR KOEFFIZIENTEN ^=0 BRAUCHEN EINGEGEBEN WERDEN. DIE		
C     MATRIX WIRD ALS SYMMETRISCH VORAUSGESETZT. HINTER DER LETZTEN		
C     KARTE VOM TYP 5 FUER JEDE UNTERMATRIX MUSS EINE LEERE KARTE		
C     FOLGEN.									
C	 4   1-6       I6      M	       NUMMER DER UNTERMATRIX		
C	     7-12      I6      RANK(M)	       ORDNUNG DER UNTERMATRIX		
C					       <=40				
C	     30-77     6A8     INFO	       BELIEBIGER TEXT			
C     *******************************************************************	
C	 5   1-6       I6      M	       NUMMER DER UNTERMATRIX		
C	     7-12      I6      I	       ZEILENINDEX DES MATRIX-		
C					       ELEMENTES			
C	     13-18     I6      K	       SPALTENINDEX			
C	     19-24     I6      P	       INDEX DES PARAMETERS, DER	
C					       ZU DIESEM KOEFFIZIENTEN		
C					       GEHOERT				
C	     25-36     E12.0   COEFF(I,K,P,M)  KOEFFIZIENT			
C	     37-76     5A8     INFO	       BELIEBIGER TEXT ZUR		
C					       IDENTIFIZIERUNG DES ME		
C     FALLS AUF KARTE 5 M=-NUMMER DER UNTERMATRIX, DANN WIRD DIESE		
C     KARTE GEDRUCKT, AUCH WENN PCOEFF=0 IST.					
C     *******************************************************************	
C	 6   1-12      F12.0   LSG(1,1)        GJ-FAKTOREN IN SL-KOPP-		
C	     13-24     F12.0   LSG(2,1)        LUNG. 6 WERTE PRO KARTE. 	
C					       DABEI BEGINNT JEDE UNTER-	
C					       MATRIX AUF EINER NEUEN		
C					       KARTE				
C	     61-72     F12.0   LSG(RANK(MAXSM),MAXSM)				
C     *******************************************************************	
C     FALLS LOCH=0  FOLGEN JETZT KARTEN VOM TYP 7:				
C     *******************************************************************	
C	 7   1-12      (A8,4X) EVNAME(1,1)     TERMBESCHREIBUNG. 6 PRO		
C	     13-24     (A8,4X) EVNAME(2,1)     KARTE. JEDE UNTERMATRIX		
C					       BEGINNT AUF EINER NEUEN		
C					       KARTE.				
C	     61-72     (A8,4X) EVNAME(RANK(MAXSM),MAXSM)			
C     *******************************************************************	
C     FALLS LOCH^=0 FOLGEN ANSTATT KARTEN VOM TYP 7 SOLCHE VOM TYP 7'           
C	 7'  1-16      (2X,A8,6X) EVNAME(1,1)  TERMBESCHREIBUNG                 
C	     17-76     15A4		       INFORMATIONEN FUER DEN		
C					       PERJ/ABCFAK-INPUT		
C					       (ENTSPRECHEND KARTE 2		
C					       FUER DEN INPUT DES ABCFAK	
C					       PROGRAMMS			
C     FUER JEDEN ZUSTAND 1 KARTE VOM TYP 7'                                     
C     ******************************************************************	
C	 8   1-3       I3      P	       NUMMER DES PARAMETERS		
C					       FALLS PMOD =0,1,2.		
C					       NUMMER DES ETA FALLS		
C					       PMOD =3,4 IST.			
C	     4-6       I3      PMOD	       0: P-TER PARAMETER WIRD		
C					       KONSTANT GEHALTEN		
C					       1: WIRD VARIIERT 		
C					       2: VERHAELTNIS DES P-TEN 	
C					       PARAMETERS ZU EINEM ZU		
C					       VARIIERENDEN PARAMETER		
C					       WIRD KONSTANT GEHALTEN.		
C					       SIEHE KARTE 8'                   
C					       3: P-TES ETA WIRD KON-		
C					       STANT GEHALTEN.			
C					       4: WIRD VARIIERT.		
C	     7-9       I3      ETAMOD	       0: P-TER PARAMETER WIRD		
C					       NICHT DURCH ETA MODIFI-		
C					       ZIERT				
C					       1: WIRD DURCH ETA MODOFI-	
C					       ZIERT. FALLS PMOD=3,4:		
C					       DIESES FELD FREILASSEN.		
C	     10-12     I3      NUMETA	       FALLS ETAMOD=1, IST NUM- 	
C					       ETA DIE NUMMER DES ETA,		
C					       DAS DEN P-TEN PARAMETER		
C					       MODIFIZIEREN SOLL. FALLS 	
C					       ETAMOD ^=1, DIESES FELD		
C					       FREILASSEN.			
C	     16-27     F12.3   PARAM	       ANFANGSWERT DES P-TEN		
C					       PARAMETERS ODER DES P-TEN	
C					       ETA*10**9			
C	     28-39     12H     NAME	       NAME DES P-TEN PARAME-		
C					       TERS ODER ETAS.			
C     DIE REIHENFOLGE DER KARTEN DES TYPS 8 IST BELIEBIG. ES MUESSEN		
C     MAXPAR KARTEN MIT PMOD=0,1,2 UND MAXETA KARTEN MIT PMOD=3,4 VOR-		
C     HANDEN SEIN.								
C     *******************************************************************	
C	8'   1-3       I3      PNUM                                             
C	     4-6       I3      PDENOM						
C	     10-21     F12.9   RATIO						
C     DAS VERHAELTNIS DES PNUM-TEN PARAMETERS ZUM PDENOM-TEN PARAMETER		
C     WIRD KONSTANT GEHALTEN. DER WERT IST RATIO. DAS VERHAELTNIS DER		
C     ANFANGSWERTE DER BEIDEN PARAMETER MUSS = RATIO SEIN. AUSSERDEM		
C     MUSS PMOD(PNUM)=2 UND PMOD(PDENOM)=1 SEIN. DIE KARTE 8' MUSS              
C     DIREKT DER KARTE 8 MIT PMOD=2 FOLGEN.					
C     *******************************************************************	
C	 9   1-3       I3      M	       NUMMER DER UNTERMATRIX		
C	     7-18      F12.0   LEVEL	       EXPERIMENTELLE ENERGIE		
C					       ODER 1.0, FALLS ENERGIE		
C					       NICHT VORHANDEN			
C	     23-30     F8.5    LEVINF(J,M,1)   EXP. GJ-WERTE, SONST LEER	
C	     31-46     2A8     LEVINF	       BELIEBIGER TEXT ZUR		
C					       ENERGIE GEHOEREND.		
C     KARTEN VOM TYP 9 MUESSEN IN DER REIHENFOLGE DER UNTERMATRIZEN		
C     FOLGEN. INNERHALB DES UNTERMATRIZENBEREICHS MUESSEN DIE LEVEL IN		
C     STEIGENDER ORDNUNG FOLGEN.						
C     *******************************************************************	
C	10   1-3       I3      -3						
C     ES KOENNEN BELIEBIG VIELE FAELLE FOLGEN. JEWEILS EIN OBIGER		
C     DATENSATZ PRO FALL.							
C										
C     KERNSPEICHERBEDARF: 1024K 						
C										
C     MAXIMALE ZAHL NICHTDIAGONALER KOEFFIZIENTEN PRO UNTERMATRIX =1500 	
C										
C										
C     ZUSAETZLICHE STEUERKARTE							
C     //GO.FT15F001 DD DSN=UNQXYZ.TEMP.DATA,UNIT=WORK,DISP=(NEW,DELETE),	
C     //      SPACE(8,28000),DCB=(BLKSIZE=8,DSORG=DA,RECFM=F)			
C     IST ERFORDERLICH								
C										
C										
      IMPLICIT REAL*8(A-H,O-Z)							
      LOGICAL DRUCK								
      INTEGER MAXSM,MAXPAR,MAXITR,MAXETA,PCOEFF,PDERIV,PEVEC			
      INTEGER RANK(14),NEVAL(14),P,P1,RANKM,RANKM1				
      INTEGER INDSAV(1500),COLPNT(6),I1EV(50),I2EV(50)				
      INTEGER VARPAR(50),ETAMOD(50),VARETA,PNUM(50),PDENOM(50)			
      REAL*4 LSG(40,14),LSGCAL(40),DIFFG(40),ETA,OLDETA 			
      REAL*4 RATIO(50),RMS(51),H1H1P(40,40),H1PRIM(40,40)			
      REAL*4 BIGEV(40),HPRNT(6),COEFFD(40,50),COEFFN(1500),SECEV(40)		
      REAL*8 TITLE(20),INFO(10) 						
      REAL*8 PARAM(50),OLDPAR(50),NAME(50,3),DIFF(50),DERIV(40,50)		
      REAL*8 EVNAM1(40,14),H(40,40),H1(40,40)					
      REAL*8 SCRTCH(50,50),EVEC(40,40),EVECSQ(40,40),EVAL(40)			
      REAL*8 A(50,50),B(50),LEVEL(40,14),LEVINF(40,14,3),BB(50) 		
      REAL*8 LEV,LEV1,LEV2,LEV3,ETANAM(2),NEWERR				
      REAL*8 GEVNAM(40,14),EIGVAL(40,14)					
      REAL*4 TEXT(14,40,15),TX(6,15)						
      INTEGER BNOFFD (14), BINDSA (14,1500)					
      REAL*4 BCOEFD (14,40,50), BCOEFN(14,1500) 				
      REAL*8 BEVEC(14,40,40),LTEMP						
C										
C										
      DEFINE FILE 15 (28000,8,L,ID1)						
C										
 9876 CONTINUE									
      NEIGEN=40 								
      NPARAM=50 								
      ERRTOL=0.0001								
      I9=512									
      I15=32768 								
      I24=I9*I15								
 1000 CONTINUE									
C			    *************************				
C * * * * * * * * * * * * * *  G O    G O    G O    * * * * * * * * * * * * * * 
C			    *************************				
      OLDRES=0.0								
      WRONG=0.0 								
      READ(5,10,END=1005) (TITLE(I1),I1=1,20)					
   10 FORMAT(10A8)								
      READ(5,20) MAXSM,MAXPAR,MAXITR,MAXETA,MAXCHG,PCOEFF,			
     1PEVEC,(INFO(I1),I1=1,5),LOCH,JKZ,PDERIV					
   20 FORMAT(7I3,5A8,3I3)							
      WRITE(6,30) (TITLE(I1),I1=1,20)						
   30 FORMAT(1H1,10A8/1H ,10A8) 						
      WRITE(6,40) MAXSM,MAXPAR,MAXETA,MAXITR,(INFO(I1),I1=1,5)			
   40 FORMAT(1H0,I3,'SUBMATRICES, ',I3,' PARAMETERS, ', 			
     1 I3,' ETA, UP TO',I2,' ITERATIONS ',5X,6A8)				
C			    *****************************************		
C * * * * * * * * * * * * * *  R E A D	 C O E F F I C I E N T S    * * 	
C			    *****************************************		
      DO 1100 M=1,MAXSM 							
      WRITE(6,45)								
   45 FORMAT(1H0,80(1H-))							
      READ(11,50) M1,RANK(M),(INFO(I1),I1=1,6)					
   50 FORMAT(I6,I6,17X,6A8)							
      RANKM=RANK(M)								
      DO 1017 P=1,MAXPAR							
      DO 1017 I=1,RANKM 							
 1017 COEFFD(I,P)=0.0								
      NOFFD=0									
      WRITE(6,70) M1,RANK(M),(INFO(I1),I1=1,6)					
   70 FORMAT(1H0,'SUBMATRIX ',I2,' HAS RANK ',I3,5X,6A8)			
      IF(PCOEFF.LT.0) WRITE(6,75)						
   75 FORMAT(1H0,5X,1HM,5X,1HI,5X,1HK,5X,1HP,6X,5HCOEFF,/)			
 1020 READ(11,80) M1,I,K,P,COEFF,(INFO(I1),I1=1,5)				
   80 FORMAT(4I6,E12.0,5A8)							
      IF(M1.EQ.0) GO TO 1098							
      IF((PCOEFF.LT.0).OR.(M1.LT.0))						
     1WRITE(6,90) M1,I,K,P,COEFF,(INFO(I1),I1=1,5)				
   90 FORMAT(1H ,4I6,F12.6,5A8) 						
      IF(I.NE.K) GO TO 1035							
      COEFFD(I,P)=COEFF 							
      GO TO 1020								
 1035 NOFFD=NOFFD+1								
      IF (NOFFD.GT.1500) GO TO 4310						
      COEFFN(NOFFD)=COEFF							
      INDSAV(NOFFD)=K+I15*(P+I9*I)						
      GO TO 1020								
 1098 IF (WRONG.EQ.1.0) GO TO 1100						
      BNOFFD(M)=NOFFD								
      RANKM=RANK(M)								
      DO 7000 I=1,RANKM 							
      DO 7000 P=1,MAXPAR							
 7000 BCOEFD (M,I,P)=COEFFD(I,P)						
      DO 7001 IATOFD=1,NOFFD							
      BCOEFN(M,IATOFD)=COEFFN(IATOFD)						
 7001 BINDSA(M,IATOFD)=INDSAV(IATOFD)						
 1100 CONTINUE									
C										
C	   READ G-FACTORS, EIGENVECTOR COMPONENT NAMES				
C	   ******************************************** 			
      DO 1110 M=1,MAXSM 							
      RANKM=RANK(M)								
 1110 READ(11,100)(LSG(I,M),I=1,RANKM)						
  100 FORMAT(6F12.0)								
      WRITE (6,101)								
  101 FORMAT (1H0,'L,S G-FACTORS READ') 					
      IF(LOCH.EQ.2) END FILE 11 						
      DO 1120 M=1,MAXSM 							
      RANKM=RANK(M)								
      IF(LOCH.NE.0) READ(11,121)(EVNAM1(I,M),(TEXT(M,I,J),J=1,15),		
     1	I=1,RANKM)								
  121 FORMAT(2X,A8,6X,15A4,4X)							
      IF(LOCH.EQ.0) READ(11,110) (EVNAM1(I,M),I=1,RANKM)			
 1120 CONTINUE									
  110 FORMAT(6(A8,4X))								
      WRITE (6,111)								
  111 FORMAT (1H0,'EIGENVECTOR COMPONENT NAMES READ')				
      IF (PCOEFF.EQ.0) GO TO 1130						
      WRITE (6,120)								
  120 FORMAT(1H1,53X,'L,S G-FACTORS')						
      DO 1125 M=1,MAXSM 							
      RANKM=RANK(M)								
      WRITE (6,130)M								
  130 FORMAT(1H0,132(1H-),/,1H0,57X,'SUBMATRIX',I3,/)				
 1125 WRITE(6,135)(LSG(I,M),EVNAM1(I,M),I=1,RANKM)				
  135 FORMAT (1H ,F8.6,1X,A8,4X,1H/,F8.6,1X,A8,4X,1H/,F8.6,1X,A8,4X,1H/,	
     1 F8.6,1X,A8,4X,1H/,F8.6,1X,A8,4X,1H/,F8.6,1X,A8,4X,1H/)			
 1130 CONTINUE									
C		   *******************************************			
C * * * * * * * * * *  R E A D	  P A R A M E T E R S	     * * * * * * * * * *
C		   *******************************************			
      DO 1135 P=1,MAXPAR							
 1135 ETAMOD(P)=0								
 1140 CONTINUE									
      NDERIV=MAXPAR+MAXETA							
      NVART2=0									
      INDPAR=0									
      DO 1200 IP=1,NDERIV							
      READ(5,140)P,IVAR,IETAMD,PARM,TNAM1,TNAM2,TNAM3				
  140 FORMAT(3I3,6X,F12.3,3A6)							
      IF(IVAR.EQ.1.OR.IVAR.EQ.4)INDPAR=INDPAR+1 				
      IF(IVAR.GT.2)GO TO 1150							
      VARPAR(P)=IVAR								
      IF(IETAMD.NE.0)ETAMOD(P)=IETAMD						
      PARAM(P)=PARM								
      NAME(P,1)=TNAM1								
      NAME(P,2)=TNAM2								
      NAME(P,3)=TNAM3								
      IF(IVAR.NE.2) GO TO 1200							
      NVART2=NVART2+1								
      READ(5,150)PNUM(NVART2),PDENOM(NVART2),RATIO(NVART2)			
  150 FORMAT(2I3,3X,F12.9)							
      GO TO 1200								
 1150 CONTINUE									
      ETA=PARM*10.0**(-9)							
      VARPAR(MAXPAR+1)=IVAR-3							
      VARETA=IVAR-3								
      ETANAM(1)=TNAM1								
      ETANAM(2)=TNAM2								
 1200 CONTINUE									
      WRITE (6,151)								
  151 FORMAT (1H0,'PARAMETERS READ')						
C		   **********************************				
C * * * * * * * * * *	R E A D   L E V E L S	    * * * * * * * * * * * * * * 
C		   **********************************				
      IF (MAXITR.GT.0) GO TO 1205						
      DO 1202 M=1,MAXSM 							
      NEVAL(M)=RANK(M)								
 1202 CONTINUE									
      GO TO 1220								
 1205 DO 1210 M=1,MAXSM 							
 1210 NEVAL(M)=0								
      M=0									
 1220 READ(5,160)M1,LEV,LEV1,LEV2,LEV3						
  160 FORMAT(I3,3X,F12.0,4X,F8.5,2A8)						
      IF(M1.LE.-1) GO TO 1250							
      IF(M1.NE.M) I=0								
      M=M1									
      NEVAL(M)=NEVAL(M)+1							
      I=I+1									
      LEVEL(I,M)=LEV								
      LEVINF(I,M,1)=LEV1							
      LEVINF(I,M,2)=LEV2							
      LEVINF(I,M,3)=LEV3							
      GO TO 1220								
 1250 CONTINUE									
      IF (MAXITR.EQ.0) GO TO 1265						
      WRITE (6,161)								
  161 FORMAT (1H0,'LEVELS READ')						
      INDLEV=0									
      DO 1260 M=1,MAXSM 							
      NEVALM=NEVAL(M)								
      INDLEV=INDLEV+NEVALM							
      DO 1260 I=1,NEVALM							
      IF(LEVEL(I,M).EQ.1.0) INDLEV=INDLEV-1					
 1260 CONTINUE									
 1265 USEOLD=M1 								
      IF(MAXITR.EQ.0) GO TO 1280						
      DEG=INDLEV-INDPAR 							
      IF(DEG.GT.0) GO TO 1270							
      IF(DEG.LT.0) GO TO 6600							
      DEG = 1.D0								
      WRITE(6,170)								
  170 FORMAT(1H ,'FREIHEITSGRAD IST 0, WIRD FUER DIE FOLGENDE RECHNUNG =        
     1 1 GESETZT')                                                              
      GO TO 1270								
 6600 WRITE(6,6601)								
 6601 FORMAT(1H ,'ZAHL DER UNABHAENGIGEN PARAMETER IST GROESSER ALS ZAHL        
     1 DER LEVEL   ABBRUCH DER RECHNUNG')                                       
      GO TO 1000								
 1270 CONTINUE									
C		   ***********************************************		
C * * * * * * * * * *	 COMPUTE  NUMERICAL  MATRIX  ELEMENTS	* * * * * * * * 
C		   ***********************************************		
      NUMITR=0									
      Q=INDPAR/DEG								
 1280 IF (MAXITR.EQ.0) GO TO 2020						
      NEWERR=0.0								
      NUMDIV=0									
 2000 CONTINUE									
      NUMITR=NUMITR+1								
      IF (NUMITR.GT.MAXITR) GO TO 1000						
      RESID=0.0 								
      IF (NUMITR.GT.1) GO TO 2011						
      DO 2010 P=1,NDERIV							
      DO 2005 IP=1,NDERIV							
 2005 A(P,IP)=0.0								
      BB(P)=0.0 								
 2010 B(P)=0.0									
      GO TO 2020								
 2011 CONTINUE									
      DO 2019 P=1,NDERIV							
      IF (VARPAR(P).EQ.0) GO TO 2019						
      DO 2015 IP=1,NDERIV							
      IF(VARPAR(IP).EQ.0) GO TO 2015						
      A(P,IP)=0.0								
 2015 CONTINUE									
      B(P)=0.0									
 2019 CONTINUE									
      IF(MAXITR.EQ.0) GO TO 2030						
 2020 WRITE(6,30)(TITLE(I1),I1=1,20)						
      WRITE(6,180)NUMITR							
  180 FORMAT(//,50X,'ITERATION NO.',I3/)					
 2030 WRITE(6,190)								
  190 FORMAT(1H0,' M  N  OBS.LEVEL.(N)  EIGENVALUE(N)   T-W    LARGEST',	
     1 ' EV COMP.     NEXT LGST.EV     CALC. G       OBS. G, IS, ETC.', 	
     2 '      OBS.G-CAL.G',/1H )						
      ID1=1									
      DO 4000 M=1,MAXSM 							
      RANKM=RANK(M)								
      NOFFD=BNOFFD(M)								
      DO 7002 I=1,RANKM 							
      DO 7002 P=1,MAXPAR							
 7002 COEFFD(I,P)=BCOEFD(M,I,P) 						
      DO 7003 IATOFD=1,NOFFD							
      COEFFN(IATOFD)=BCOEFN(M,IATOFD)						
 7003 INDSAV(IATOFD)=BINDSA(M,IATOFD)						
      DO 3020 I=1,RANKM 							
      H1(I,I)=0.0								
      H(I,I)=0.0								
      IF(MAXETA.EQ.0) GO TO 3010						
      H1PRIM(I,I)=0.0								
 3010 CONTINUE									
      DO 3020 P=1,MAXPAR							
      H1(I,I)=COEFFD(I,P)*PARAM(P)+H1(I,I)					
      IF(MAXETA.EQ.0) GO TO 3020						
      IF(ETAMOD(P).EQ.0) GO TO 3020						
      H1PRIM(I,I)=H1PRIM(I,I)+COEFFD(I,P)*PARAM(P)				
 3020 CONTINUE									
      NEVALM=NEVAL(M)								
      RANKM1=RANKM-1								
      IF(RANKM1.LT.1)GO TO 3071 						
      DO 3035 I=1,RANKM1							
      KZ=I+1									
      DO 3035 K=KZ,RANKM							
      H1(I,K)=0.0								
      H(I,K)=0.0								
      IF(MAXETA.EQ.0) GO TO 3035						
      H1PRIM(I,K)=0.0								
 3035 CONTINUE									
      IF(NOFFD.EQ.0) GO TO 3065 						
      DO 3060 IATOFD=1,NOFFD							
      K1=INDSAV(IATOFD)/I15							
      I=INDSAV(IATOFD)/I24							
      K=INDSAV(IATOFD)-K1*I15							
      P=K1-I*I9 								
      H1(I,K)=H1(I,K)+COEFFN(IATOFD)*PARAM(P)					
      IF (MAXETA.EQ.0) GO TO 3060						
      IF(ETAMOD(P).EQ.0) GO TO 3060						
      H1PRIM(I,K)=H1PRIM(I,K)+COEFFN(IATOFD)*PARAM(P)				
 3060 CONTINUE									
 3065 CONTINUE									
      DO 3070 I=1,RANKM1							
      KZ=I+1									
      DO 3070 K=KZ,RANKM							
      H1(K,I)=H1(I,K)								
      IF(MAXETA.EQ.0) GO TO 3070						
      H1PRIM(K,I)=H1PRIM(I,K)							
 3070 CONTINUE									
 3071 CONTINUE									
      DO 3082 I=1,RANKM 							
      DO 3082 K=1,RANKM 							
      H(I,K)=H1(I,K)								
      IF(MAXETA.EQ.0) GO TO 3080						
      TEMP=0.0									
      DO 3075 J=1,RANKM 							
      TEMP=TEMP+H1(I,J)*H1PRIM(J,K)+H1PRIM(I,J)*H1(J,K) 			
 3075 CONTINUE									
      H1H1P(I,K)=TEMP								
      H(I,K)=H(I,K)-H1H1P(I,K)*ETA*0.5						
 3080 H(I,K)=-H(I,K)								
 3082 CONTINUE									
C		  ********************************************			
C * * * * * * * * *   COMPUTE EIGENVALUES AND EIGENVECTORS   * * * * * *	
C		  ********************************************			
      IF(RANKM.NE.1) GO TO 3090 						
      EVAL(1)=H(1,1)								
      H(1,1)=1.0								
      GO TO 3100								
 3090 CALL EIGEN(H(1,1),EVEC(1,1),RANKM,EVAL(1),NEVALM,ANORM,NEIGEN)		
 3100 CONTINUE									
      DO 3110 I=1,NEVALM							
 3110 EVAL(I)=-EVAL(I)								
      DO 3120 I=1,NEVALM							
      BIGGST=0.0								
      SECOND=0.0								
      I1EV(I)=0 								
      DO 3116 K=1,RANKM 							
      EVEC(I,K)=H(K,I)								
      TESTEV=DABS(EVEC(I,K))							
      IF(BIGGST.GE.TESTEV) GO TO 3114						
      SECOND=BIGGST								
      I2EV(I)=I1EV(I)								
      BIGGST=TESTEV								
      I1EV(I)=K 								
      GO TO 3116								
 3114 IF(SECOND.GE.TESTEV) GO TO  3116						
      SECOND=TESTEV								
      I2EV(I)=K 								
 3116 CONTINUE									
      BIGEV(I)=BIGGST								
      SECEV(I)=SECOND								
 3120 CONTINUE									
      DO 7004 I=1,NEVALM							
      DO 7004 K=1,RANKM 							
 7004 BEVEC(M,I,K)=EVEC(I,K)							
 3123 CONTINUE									
      DO 3125 I=1,NEVALM							
      LSGCAL(I)=0.0								
      DO 3125 K=1,RANKM 							
      EVECSQ(I,K)=EVEC(I,K)**2							
 3125 LSGCAL(I)=LSG(K,M)*EVECSQ(I,K)+LSGCAL(I)					
      DO 3130 J=1,NEVALM							
      DO 3130 P=1,NDERIV							
 3130 DERIV(J,P)=0.0								
      IF(MAXETA.EQ.0) GO TO 3141						
      IF(VARETA.EQ.0) GO TO 3141						
      P=MAXPAR+1								
      DO 3138 J=1,NEVALM							
      TEMP1=0.0 								
      DO 3136 I=1,RANKM 							
      TEMP2=0.0 								
      DO 3134 K=1,RANKM 							
      TEMP2=TEMP2 + EVEC(J,K)*H1H1P(I,K)					
 3134 CONTINUE									
      TEMP1=TEMP1+EVEC(J,I)*TEMP2						
 3136 CONTINUE									
      DERIV(J,P)=DERIV(J,P)-0.5*TEMP1						
 3138 CONTINUE									
 3141 CONTINUE									
      IF(NOFFD.EQ.0) GO TO 3152 						
      DO 3150 IATOFD=1,NOFFD							
      I=INDSAV(IATOFD)/I24							
      K1=INDSAV(IATOFD)/I15							
      K=INDSAV(IATOFD) - K1*I15 						
      P=K1-I*I9 								
      IF(VARPAR(P).EQ.0) GO TO 3150						
      DO 3145 J=1,NEVALM							
 3145 DERIV(J,P)=DERIV(J,P)+EVEC(J,I)*EVEC(J,K)*COEFFN(IATOFD)			
 3150 CONTINUE									
 3152 CONTINUE									
      DO 3170 P=1,MAXPAR							
      IF(VARPAR(P).EQ.0) GO TO 3170						
      DO 3165 J=1,NEVALM							
      DERIV(J,P)=2.0*DERIV(J,P) 						
      DO 3161 I=1,RANKM 							
 3161 DERIV(J,P)=DERIV(J,P)+EVECSQ(J,I)*COEFFD(I,P)				
 3165 CONTINUE									
 3170 CONTINUE									
      IF(PDERIV.EQ.0) GO TO 3998						
      DO 6100 P=1,NDERIV							
      DO 6100 J=1,NEVALM							
 6100 WRITE(15'ID1) DERIV(J,P)                                                  
 3998 IF(MAXITR.EQ.0) GO TO 3202						
      DO 3180 P=1,NDERIV							
      IF(VARPAR(P).EQ.0) GO TO 3180						
      DO 3178 P1=1,NDERIV							
      IF(VARPAR(P1).EQ.0) GO TO 3178						
      ATEMP=0.0 								
      DO 3175 J=1,NEVALM							
      IF(LEVEL(J,M).EQ.1.0) GO TO 3175						
      ATEMP=DERIV(J,P)*DERIV(J,P1)+ATEMP					
 3175 CONTINUE									
      A(P,P1)=A(P,P1)+ATEMP							
 3178 CONTINUE									
 3180 CONTINUE									
      DO 6000 J=1,NEVALM							
      GTEMP=LEVINF(J,M,1)							
      IF(GTEMP.NE.0.0) GO TO 6001						
      DIFFG(J)=2.0								
      GO TO 6000								
 6001 DIFFG(J)=GTEMP-LSGCAL(J)							
 6000 CONTINUE									
 3182 RSUM=0.0									
      DO 3190 J=1,NEVALM							
      LTEMP=LEVEL(J,M)								
      IF (LTEMP.NE.1.0) GO TO 3184						
      DIFF(J)=1.0								
      GO TO 3190								
 3184 DIFF(J)=LTEMP-EVAL(J)							
      RSUM=DIFF(J)**2+RSUM							
 3190 CONTINUE									
      RESID=RESID+RSUM								
      DO 3200 P=1,NDERIV							
      IF (VARPAR(P).EQ.0) GO TO 3200						
      BTEMP=0.0 								
      DO 3195 J=1,NEVALM							
      IF(LEVEL(J,M).EQ.1.0) GO TO 3195						
      BTEMP=DERIV(J,P)*DIFF(J)+BTEMP						
 3195 CONTINUE									
      BB(P)=B(P)								
      B(P)=B(P)+BTEMP								
 3200 CONTINUE									
 3202 WRITE(6,200)								
  200 FORMAT(1H0)								
      DO 3205 J=1,NEVALM							
      J1EV=I1EV(J)								
      J2EV=I2EV(J)								
      GEVNAM(J,M)=EVNAM1(J1EV,M)						
      EIGVAL(J,M)=EVAL(J)							
      IF (MAXITR.EQ.0) GO TO 3203						
      IF(DIFFG(J).EQ.2.0) GO TO 6002						
      WRITE(6,210)M,J,LEVEL(J,M),EVAL(J),DIFF(J),I1EV(J),BIGEV(J),		
     1 EVNAM1(J1EV,M),I2EV(J),SECEV(J),EVNAM1(J2EV,M),LSGCAL(J),		
     2	LEVINF(J,M,1),(LEVINF(J,M,II),II=2,3),DIFFG(J)				
  210 FORMAT(2I3,F16.3,F12.3,F10.3,2(I4,1H(,F4.2,1H),1X,A8),			
     1 F10.6,4X,F8.5,2A8,F8.4)							
      GO TO 3205								
 6002 WRITE(6,211)M,J,LEVEL(J,M),EVAL(J),DIFF(J),I1EV(J),BIGEV(J),		
     1 EVNAM1(J1EV,M),I2EV(J),SECEV(J),EVNAM1(J2EV,M),LSGCAL(J),		
     2 (LEVINF(J,M,II),II=2,3)							
  211 FORMAT(2I3,F16.3,F12.3,F10.3,2(I4,1H(,F4.2,1H),1X,A8),			
     1 F10.6,12X,2A8)								
      GO TO 3204								
 3203 WRITE (6,212) M,J,EVAL(J),I1EV(J),BIGEV(J),EVNAM1(J1EV,M),		
     1 I2EV(J),SECEV(J),EVNAM1(J2EV,M),LSGCAL(J)				
  212 FORMAT (2I3,16X  ,F12.3,10X  ,2(I4,1H(,F4.2,1H),1X,A8),F10.6)		
 3204 CONTINUE									
 3205 CONTINUE									
 4000 CONTINUE									
C		      *************************************			
C * * * * * * * * * * *   COMPUTE PARAMETERS CHANGES	   * * * * * * * * * * *
C		      *************************************			
      ID1=1									
      IF (MAXITR.EQ.0) GO TO 4072						
      SIGMA=DSQRT(RESID/DEG)							
      IF (NVART2.LT.1) GO TO 4020						
      DO 4005 K=1,NVART2							
      INUM=PNUM(K)								
      IDENOM=PDENOM(K)								
      DO 4005 I=1,NDERIV							
      A(I,IDENOM)=A(I,IDENOM)+A(I,INUM)*RATIO(K)				
 4005 A(I,INUM)=0.0								
      DO 4015 K=1,NVART2							
      INUM=PNUM(K)								
      IDENOM=PDENOM(K)								
      DO 4010 I=1,NDERIV							
      A(IDENOM,I)=A(IDENOM,I)+A(INUM,I) 					
 4010 A(INUM,I)=0.0								
      B(IDENOM)=B(IDENOM)+B(INUM)						
 4015 B(INUM)=0.0								
 4020 CALL MATINV(A,NDERIV,B,1,DETERM,NPARAM)					
      OLDERR=NEWERR								
      DRESID=OLDRES-RESID							
      DAMPNG=DEXP(-Q*DABS(DRESID)/RESID)					
      IF (DAMPNG.LT.0.1) DAMPNG=0.1						
      OLDRES=RESID								
      NEWERR=ERRTOL*OLDRES							
      DO 4025 P=1,MAXPAR							
      OLDPAR(P)=PARAM(P)							
      RMS(P)=SIGMA*DSQRT(DABS(A(P,P)))						
      PARAM(P)=OLDPAR(P) + DAMPNG*B(P)						
      IF (MAXCHG.NE.0.AND.DABS(PARAM(P)-OLDPAR(P)).GT.300.0)			
     1 PARAM(P)=OLDPAR(P)+300.0*B(P)/DABS(B(P)) 				
 4025 CONTINUE									
      IF(MAXETA.EQ.0) GO TO 4031						
      P=MAXPAR+1								
      RMS(P)=SIGMA*DSQRT(DABS(A(P,P)))						
      OLDETA=ETA								
      ETA=ETA+DAMPNG*B(P)							
      IF(MAXCHG.NE.0.AND.ABS(ETA-OLDETA).GT.3*10.0**(-7))			
     1 ETA=OLDETA+3*10.0**(-7)*B(P)/DABS(B(P))					
 4030 CONTINUE									
 4031 CONTINUE									
      IF(NVART2.LT.1) GO TO 4050						
      DO 4035 K=1,NVART2							
      INUM=PNUM(K)								
      IDENOM=PDENOM(K)								
 4035 PARAM(INUM)=RATIO(K)*PARAM(IDENOM)					
 4050 WRITE(6,500)NUMITR							
  500 FORMAT(///50X,'ITERATION NO ',I3//)					
      KONV=MAXITR-NUMITR-PEVEC+3						
      IF(NUMITR.EQ.1) GO TO 4070						
      IF (DABS(DRESID).GT.NEWERR) GO TO 4060					
      WRITE(6,510)								
  510 FORMAT(52X,'CONVERGED'//) 						
      KONV=0									
      GO TO 4070								
 4060 IF(NEWERR.GT.OLDERR) WRITE (6,520)					
  520 FORMAT(52X,'DIVERGED')							
      IF(SIGMA.LE.1.D-3) KONV=0 						
 4070 WRITE (6,530) OLDRES,DRESID,SIGMA,INDPAR,INDLEV,DEG			
  530 FORMAT(12H0 RESIDUAL =, 1PE13.3,3X,20HCHANGE IN RESIDUAL =,		
     1 E13.3,6X,'SIGMA=',E13.3,/						
     2 1H ,'NUMBER OF INDEPENDENT PARAMETERS = ',I4,				
     3 '   NUMBER OF LEVELS FITTED = ',I4,					
     4 '   DEGREES OF FREEDOM =',0PF4.0)					
 4072 WRITE(6,540)								
  540 FORMAT(1H0,'  P  FIX ETA    OLD PARAMETER      ERROR(RMS)',		
     1 '  NAME                   CHANGE      NEW PARAMETER',			
     2 '          DR/DP'/)							
      DO 4080 P=1,MAXPAR							
      IF (MAXITR.EQ.0) GO TO 4078						
      WRITE(6,550)P,VARPAR(P),ETAMOD(P),OLDPAR(P),RMS(P),NAME(P,1),		
     1 NAME(P,2),NAME(P,3),B(P),PARAM(P),BB(P)					
  550 FORMAT(1H ,I3,2I4,F17.3,F15.3,3X,3A6,F13.3,F18.3,F18.3)			
      GO TO 4080								
 4078 WRITE (6,552) P,VARPAR(P),ETAMOD(P),PARAM(P),NAME(P,1),NAME(P,2), 	
     1 NAME(P,3)								
  552 FORMAT (1H ,I3,I4,I4,F17.3,17X,3A6)					
 4080 CONTINUE									
      IF (MAXETA.EQ.0) GO TO 4088						
      WRITE (6,555)								
  555 FORMAT(1H0,'ETA FIX      ',8X,'OLD ETA',6X,'ERROR(RMS)   NAME',		
     1 '          CHANGE       NEW ETA'/)					
      P=MAXPAR+1								
      IF (MAXITR.EQ.0) GO TO 4082						
      WRITE (6,560) VARETA,OLDETA,RMS(P),ETANAM(1),ETANAM(2),B(P),ETA		
  560 FORMAT(1H ,I4,9PF17.3,F16.3,1X,2A6,2F18.3)				
      GO TO 4085								
 4082 WRITE (6,562) VARETA,ETA,ETANAM(1),ETANAM(2)				
  562 FORMAT(1H ,I4,9PF17.3,17X,2A6)						
 4085 CONTINUE									
 4088 CONTINUE									
      IF(NVART2.GT.0)WRITE(6,570)(PNUM(I1),PDENOM(I1),RATIO(I1),		
     1 I1=1,NVART2)								
  570 FORMAT (1H0,(I3,1H/,I3,1H=,F12.6))					
 4100 DRUCK=(PEVEC.EQ.2).OR.((PEVEC.NE.0).AND.(KONV.EQ.0))			
      VALJ=JKZ/2.								
      DO 4115 M=1,MAXSM 							
      IF(DRUCK) WRITE(6,601) M							
  601 FORMAT(1H0,132(1H-)/1H0,40X,'EIGENVECTORS,SUBMATRIX',I3/) 		
      NEVALM=NEVAL(M)								
      RANKM=RANK(M)								
      IF(DRUCK) WRITE(6,135) (LSG(I,M),EVNAM1(I,M),I=1,RANKM)			
      IF(DRUCK) WRITE(6,602)							
  602 FORMAT (1H0,'(N, I)    EVEC(N,I)')					
      DO 7005 I=1,NEVALM							
      DO 7005 K=1,RANKM 							
 7005 EVEC(I,K)=BEVEC(M,I,K)							
      DO 4110 I=1,NEVALM							
      IF(DRUCK) WRITE(6,587)							
  587 FORMAT(1H )								
      IF(KONV.EQ.0.AND.LOCH.EQ.3.AND.LEVEL(I,M).EQ.1.)				
     1LEVEL(I,M)=EIGVAL(I,M)							
      IF(KONV.EQ.0.AND.LOCH.NE.0.AND.LEVEL(I,M).NE.1.)				
     1WRITE(7,588)								
     2GEVNAM(I,M),VALJ,LEVEL(I,M),RANKM,M,I					
  588 FORMAT(11X,A8,6X,F5.1,F10.2,8X,I2,26X,2I2)				
      NUMPNT=0									
      DO 4114 K=1,RANKM 							
      NUMPNT=NUMPNT+1								
      COLPNT(NUMPNT)=K								
      HPRNT(NUMPNT)=EVEC(I,K)							
      DO 599 L=1,15								
      TX(NUMPNT,L)=TEXT(M,K,L)							
  599 CONTINUE									
      IF(NUMPNT.LT.6) GO TO 4114						
      IF(DRUCK) WRITE(6,603) (I,COLPNT(J),HPRNT(J),J=1,6)			
  603 FORMAT (6(1H ,1H(,I2,1H,,I2,1H),1X,F12.6,1H/))				
      IF(KONV.EQ.0.AND.LOCH.NE.0.AND.LEVEL(I,M).NE.1.)				
     1WRITE(7,605)								
     2(HPRNT(J),(TX(J,L),L=1,15),M,I,J=1,6)					
  605 FORMAT(F10.6,6X,15A4,2I2) 						
      DO 4105 J=1,6								
 4105 HPRNT(J)=HPRNT(J)*HPRNT(J)						
      IF(DRUCK) WRITE(6,604) (HPRNT(J),J=1,6)					
  604 FORMAT (6(1H ,8X,F12.6,1H/))						
      NUMPNT=0									
 4114 CONTINUE									
      IF(NUMPNT.EQ.0) GO TO 4110						
      IF(DRUCK) WRITE(6,603) (I,COLPNT(J),HPRNT(J),J=1,NUMPNT)			
      IF(KONV.EQ.0.AND.LOCH.NE.0.AND.LEVEL(I,M).NE.1.)				
     1WRITE(7,605)								
     2(HPRNT(J),(TX(J,L),L=1,15),M,I,J=1,NUMPNT)				
      DO 4106 J=1,NUMPNT							
 4106 HPRNT(J)=HPRNT(J)*HPRNT(J)						
      IF(DRUCK) WRITE(6,604) (HPRNT(J),J=1,NUMPNT)				
 4110 CONTINUE									
      VALJ=VALJ+1.								
 4115 CONTINUE									
 4200 CONTINUE									
      IF(PDERIV.EQ.0) GO TO 4300						
      IF((PDERIV.EQ.1).AND.(KONV.NE.0)) GO TO 4300				
      DO 4210 M=1,MAXSM 							
      WRITE(6,607) M								
  607 FORMAT(1H0,132(1H-)/1H0,40X,'DERIVATIVES,SUBMATRIX',I3/)			
      NEVALM=NEVAL(M)								
      DO 6200 P=1,NDERIV							
      DO 6200 J=1,NEVALM							
 6200 READ(15'ID1) DERIV(J,P)                                                   
      RANKM=RANK(M)								
      WRITE(6,135) (LSG(I,M),EVNAM1(I,M),I=1,RANKM)				
      WRITE(6,608)								
  608 FORMAT(1H0,'( N, P)   DERIV(N,P)')					
      DO 4205 J=1,NEVALM							
      WRITE(6,587)								
      IF(MAXETA.EQ.0) GO TO 4202						
      DERIV(J,MAXPAR+1)=DERIV(J,MAXPAR+1)*(10.0**(-9))				
 4202 CONTINUE									
      WRITE(6,590) (J,P,DERIV(J,P),P=1,NDERIV)					
  590 FORMAT(6(1H ,1H(,I2,1H,,I2,1H),1X,F12.3,1H/))				
      IF(MAXETA.EQ.0) GO TO 4205						
      DERIV(J,MAXPAR+1)=DERIV(J,MAXPAR+1)*(10.0**(9))				
 4205 CONTINUE									
 4210 CONTINUE									
 4300 CONTINUE									
      IF(MAXITR.EQ.0) GO TO 1000						
      IF(NUMITR.EQ.1) GO TO 2000						
      IF (KONV.EQ.0) GO TO 1000 						
      IF (NEWERR.LT.OLDERR) GO TO 2000						
      NUMDIV=NUMDIV+1								
      IF(DABS(DRESID).GT.OLDRES*10.0) GO TO 1000				
      IF(NUMDIV.LT.50) GO TO 2000						
      GO TO 1000								
 4310 WRITE (6,620) M1								
  620 FORMAT(1H1,'MORE THAN 1500 OFF-DIAGONAL COEFFICIENTS FOR SUBMATRIX        
     1',I6)                                                                     
 1005 STOP									
      END									
      SUBROUTINE EIGEN (A,B,NSUB,VALU,MSUB,ANORM,NMAX)				
C										
C     EIGENVALUES AND  EIGENVEKTORS OF A REAL SYMMETRIC MATRIX			
C										
C**** REMOVE NEXT STATEMENT IN SINGLE PRECISION VERSION 			
      IMPLICIT REAL*8 (A-H,O-Z) 						
      INTEGER*2 INDEX								
      DIMENSION A(NMAX,NSUB), B(NMAX,NSUB), VALU(MSUB)				
      DIMENSION    DIAG(100), SUPERD(100) , WVEC(100), PVEC(100),		
     1		   QVEC(100), VALL(100), Q(100), U(100),			
     2		   INDEX(100), FACTOR(100), V(100), T(100,3)			
      COMMON /F202/ DIAG, SUPERD, WVEC, PVEC, T 				
      EQUIVALENCE (WVEC(1),VALL(1),FACTOR(1),U(1)),				
     1		  (PVEC(1),QVEC(1),Q(1),V(1))					
C**** REPLACE DATA VALUE WITH Z3C200000 IN SINGLE PRECISION VERSION		
      DATA E1/Z3420000000000000/						
C										
C     HOUSEHOLDER SIMILARITY TRANSFORMATION TO CO-DIAGONAL FORM 		
C     ------------ ---------- ------------- -------------- ---- 		
      N=NSUB									
      M=MSUB									
      IF (N.LT.0) GO TO 220							
      IF (M.LE.0.OR.N.LE.2) GO TO 50						
C										
C     GENERATE IDENTITY MATRIX							
C										
      DO 40 I=2,N								
      DO 40 J=2,N								
      B(J,I)=0.0								
      IF (I.EQ.J) B(J,I)=1.0							
   40 CONTINUE									
C										
   50 DO 200 I=1,N								
C										
C     REDUCE COLUMN OF MATRIX							
C										
      I1=I+1									
      I2=I1+1									
      IF (I2.GT.N) GO TO 160							
      SUM=0.0									
      DO 70 J=I2,N								
   70 SUM=SUM+A(J,I)**2 							
      IF (SUM.EQ.0.0) GO TO 160 						
      J=I1									
      TEMP=A(J,I)								
C**** REPLACE FUNCTION NAMES IN NEXT FOUR STATEMENTS IN SINGLE PRECISION	
      SUM=DSQRT(SUM+ TEMP**2)							
      A(J,I)=-DSIGN(SUM, TEMP)							
      WVEC(J)=DSQRT(	  1.0+DABS( TEMP  )/SUM)				
      DIV=DSIGN(      WVEC(J)*SUM, TEMP   )					
      DO 85 J=I2,N								
   85 WVEC(J)=A(J,I)/DIV							
      SCALAR=0.0								
      DO 95 J=I1,N								
      PVEC(J)=0.0								
      DO 90 K=I1,N								
   90 PVEC(J)=PVEC(J)+A(K,J)*WVEC(K)						
      SCALAR=SCALAR+PVEC(J)*WVEC(J)						
   95 CONTINUE									
      SCALAR=SCALAR/2.0 							
      DO 120 J=I1,N								
      QVEC(J)=PVEC(J)-SCALAR*WVEC(J)						
      DO 120 K=I1,J								
      A(K,J)=A(K,J)-(WVEC(K)*QVEC(J)+WVEC(J)*QVEC(K))				
      A(J,K)=A(K,J)								
  120 CONTINUE									
      IF (M.LE.0) GO TO 160							
C										
C     SAVE ROTATION FOR LATER APPLICATION TO CO-DIAGONAL VECTORS		
C										
      DO 150 K=2,N								
      TEMP=0.0									
      DO 140 J=I1,N								
  140 TEMP=TEMP+WVEC(J)*B(J,K)							
      DO 150 J=I1,N								
      B(J,K)=B(J,K)-WVEC(J)*TEMP						
  150 CONTINUE									
C										
C     MOVE CO-DIAGONAL FORM ELEMENTS FOR ITERATIVE PROCEDURE			
C										
  160 J=I									
      DIAG(I)=A(J,I)								
      IF (I.NE.N) SUPERD(I)=A(J+1,I)						
  200 CONTINUE									
C										
C     GIVENS EIGENVALUE ITERATION FROM STURM CHAIN OF CO-DIAGONAL MINORS	
C     ------ ---------- --------- ---------- ----- -- ----------- ------	
  220 N=IABS (N)								
      M=IABS (M)								
C										
C     CALCULATE NORM OF MATRIX AND INITIALIZE EIGENVALUE BOUNDS 		
C										
      ANORM2=DIAG(1)**2 							
      IF (N.EQ.1) GO TO 235							
      DO 230 L=2,N								
      Q(L-1)=SUPERD(L-1)**2							
      ANORM2=DIAG(L)**2+Q(L-1)+Q(L-1)+ANORM2					
  230 CONTINUE									
C**** REPLACE FUNCTION NAME IN NEXT STATEMENT IN SINGLE PRECISION		
  235 ANORM=DSQRT(ANORM2)							
      IF (M.EQ.0) RETURN							
      DO 240 L=1,M								
      VALU(L)=ANORM								
      VALL(L)=-ANORM								
  240 CONTINUE									
      EPS1=ANORM*E1								
      IF (EPS1.EQ.0.0) RETURN							
C										
  250 DO 570 L=1,M								
C										
C     CHOOSE NEW TRIAL VALUE WHILE TESTING BOUNDS FOR CONVERGENCE		
C										
  260 TAU=(VALU(L)+VALL(L))/2.0 						
      IF (2.0*(TAU-VALL(L)).LE.E1*DABS(TAU)) GO TO 570				
C										
C     DETERMINE SIGNS OF PRINCIPAL MINORS					
C										
      MATCH=0									
      T2=0.0									
      T1=1.0									
      DO 450 L1=1,N								
      P=DIAG(L1)-TAU								
      IF (T2.NE.0.0) GO TO 330							
C**** REPLACE FUNCTION NAME IN NEXT STATEMENT IN SINGLE PRECISION		
  300 T1=DSIGN(1.0D0,T1)							
  330 IF (T1.NE.0.0) GO TO 400							
C**** REPLACE FUNCTION NAME IN NEXT STATEMENT IN SINGLE PRECISION		
      T0=-DSIGN(1.0D0,T2)							
      T2=0.0									
      IF (Q(L1-1).NE.0.0) GO TO 410						
      GO TO 300 								
  400 T0=P-Q(L1-1)*T2/T1							
      T2=1.0									
C										
C     COUNT AGREEMENTS IN SIGN (ZERO CONSIDERED POSITIVE)			
C										
  410 IF (T0) 440, 420, 430							
  420 T2=T1									
      IF (T2) 440, 430, 430							
  430 MATCH=MATCH+1								
  440 T1=T0									
  450 CONTINUE									
C										
C     ESTABLISH TIGHTER BOUNDS ON EIGENVALUES					
C										
      DO 530 L1=L,M								
      IF (L1.LE.MATCH) GO TO 500						
      IF (VALU(L1).LE.TAU) GO TO 260						
      VALU(L1)=TAU								
      GO TO 530 								
  500 VALL(L1)=TAU								
  530 CONTINUE									
      GO TO 260 								
C										
  570 CONTINUE									
C										
C     EIGENVECTORS OF CO-DIAGONAL SYMMETRIC MATRIX--INVERSE ITERATION		
C     ------------ -- ----------- --------- ------  ------- ---------		
      M=MSUB									
      IF (M.LT.0) RETURN							
C										
      DO 970 I=1,M								
C										
C     CHECK FOR REPEATED VALUE							
C										
      IF (I.EQ.1) GO TO 725							
      IF (VALU(I-1)-VALU(I).LT.1.0E+4*EPS1) GO TO 730				
  725 I1=-1									
  730 I1=I1+1									
C										
C     TRIANGULARIZE CO-DIAGONAL FORM AFTER EIGENVALUE SUBTRACTION		
C										
      DO 760 L=1,N								
      V(L)=EPS1*(I1*L+1)							
      T(L,2)=DIAG(L)-VALU(I)							
      IF (L.LT.N) GO TO 740							
      T(L,3)=0.0								
      GO TO 760 								
  740 T(L,3)=SUPERD(L)								
      IF (T(L,3).EQ.0.0) T(L,3)=EPS1						
      T(L+1,1)=T(L,3)								
  760 CONTINUE									
      DO 820 J=1,N								
      T(J,1)=T(J,2)								
      T(J,2)=T(J,3)								
      T(J,3)=0.0								
      GO TO 780 								
  770 T(J,1)=EPS1								
C**** REPLACE FUNCTION NAME IN NEXT STATEMENT IN SINGLE PRECISION		
  780 VTEMP=DABS(T(J,1))							
      IF (VTEMP.LT.EPS1) GO TO 770						
      IF (J.EQ.N) GO TO 820							
      INDEX(J)=0								
C**** REPLACE FUNCTION NAME IN NEXT STATEMENT IN SINGLE PRECISION		
      IF (DABS(T(J+1,1)).LE.VTEMP) GO TO 810					
       INDEX(J)=1								
      DO 800 K=1,3								
      VTEMP=T(J,K)								
      T(J,K)=T(J+1,K)								
      T(J+1,K)=VTEMP								
  800 CONTINUE									
  810 VTEMP=T(J+1,1)/T(J,1)							
      FACTOR(J)=VTEMP								
      T(J+1,2)=T(J+1,2)-VTEMP*T(J,2)						
      T(J+1,3)=T(J+1,3)- VTEMP *T(J,3)						
  820 CONTINUE									
      ITER=1									
       IF (I1.GT.0) GO TO 920							
C										
C     BACK SUBSTITUTE TO OBTAIN EIGENVECTOR					
C										
  860 DO 870 L1=1,N								
      L=N+1-L1									
      V( L )=(V( L )-T(L,2)*V( L+1 )-T(L,3)*V( L+2 ))/T(L,1)			
  870 CONTINUE									
      GO TO (875,920), ITER							
C										
C     PERFORM SECOND ITERATION							
C										
  875 ITER=2									
      IF (N.EQ.1) GO TO 860							
  880 DO 910 L=2,N								
      IF (INDEX(L-1).EQ.0) GO TO 900						
      VTEMP=V(L-1)								
      V(L-1)=V(L)								
      V(L)=VTEMP								
  900 V(L)=V(L)-FACTOR(L-1)*V(L-1)						
  910 CONTINUE									
      GO TO 860 								
C										
C     ORTHOGONALIZE VECTOR TO OTHERS ASSOCIATED WITH REPEATED VALUE		
C										
  920 IF (I1.EQ.0) GO TO 945							
      DO 940 L1=1,I1								
      K=I-L1									
      VTEMP=0.0 								
      DO 930 J=1,N								
  930 VTEMP=VTEMP+A(J,K)*V(J)							
      DO 940 J=1,N								
  940 V(J)=V(J)-A(J,K)*VTEMP							
      GO TO (880,945), ITER							
C										
C     NORMALIZE VECTOR								
C										
  945 VNORM2=0.0								
      DO 950 L=1,N								
  950 VNORM2=VNORM2+V( L )**2							
C**** REPLACE FUNCTION NAME IN NEXT STATEMENT IN SINGLE PRECISION		
      VNORM=DSQRT(VNORM2)							
      DO 960 J=1,N								
  960 A(J,I)=V( J )/VNORM							
C										
  970 CONTINUE									
C										
C     ROTATION OF CO-DIAGONAL VECTORS INTO MATRIX EIGENVECTORS			
C     -------- -- ----------- ------- ---- ------ ------------			
      N=NSUB									
      IF (N.LE.2) RETURN							
      DO 990 I=1,M								
      DO 980 K=2,N								
      U(K)=0.0									
      DO 980 J=2,N								
  980 U(K)=U(K)+B(J,K)*A(J,I)							
      DO 990 J=2,N								
  990 A(J,I)=U(J)								
C										
      RETURN									
      END									
      SUBROUTINE MATINV (A,N,B,M,DETERM,NMAX)					
C										
C     MATRIX INVERSION WITH ACCOMPANYING SOLUTION OF LINEAR EQUTIONS		
C										
C**** REMOVE NEXT STATEMENT IN SINGLE PRECISION VERSION 			
      IMPLICIT REAL*8 (A-H,O-Z) 						
      REAL*4 PIVOT								
      DIMENSION A(NMAX,N),B(NMAX,M)						
      COMMON /F402/ PIVOT(100), INDEX(100)					
C										
C     INITIALIZE DETERMINANT AND PIVOT ELEMENT ARRAY				
C										
      DETERM=1.0								
      DO 20 I=1,N								
      PIVOT(I)=0.0								
   20 INDEX(I)=0.0								
C										
C     PERFORM SUCCESSIVE PIVOT OPERATIONS (GRAND LOOP)				
C										
      DO 550 I=1,N								
C										
C     SEARCH FOR PIVOT ELEMENT AND EXTEND DETERMINANT PARTIAL PRODUCT		
C										
      AMAX=0.0									
      DO 105 J=1,N								
      IF (PIVOT(J).NE.0.0) GO TO 105						
      DO 100 K=1,N								
      IF (PIVOT(K).NE.0.0) GO TO 100						
      TEMP=DABS(A(J,K)) 							
      IF (TEMP.LT.AMAX) GO TO 100						
      IROW=J									
      ICOLUM=K									
      AMAX=TEMP 								
  100 CONTINUE									
  105 CONTINUE									
      INDEX(I)=4096*IROW+ICOLUM 						
      J=IROW									
      AMAX=A(J,ICOLUM)								
      DETERM=AMAX*DETERM							
C										
C     RETURN IF MATRIX IS SINGULAR (ZERO PIVOT) AFTER COLUMN INTERCHANGE	
C										
      IF (DETERM.EQ.0.0) GO TO 600						
C										
      PIVOT(ICOLUM)=AMAX							
C										
C     INTERCHANGE ROWS TO PUT PIVOT ELEMENT ON DIAGONAL 			
C										
      IF (IROW.EQ.ICOLUM) GO TO 260						
      DETERM=-DETERM								
      DO 200 K=1,N								
      SWAP=A(J,K)								
      A(J,K)=A(ICOLUM,K)							
      A(ICOLUM,K)=SWAP								
  200 CONTINUE									
      IF (M.LE.0) GO TO 260							
      DO 250 K=1,M								
      SWAP=B(J,K)								
      B(J,K)=B(ICOLUM,K)							
      B(ICOLUM,K)=SWAP								
  250 CONTINUE									
C										
C     DIVIDE PIVOT ROW BY PIVOT ELEMENT 					
C										
  260 K=ICOLUM									
      A(ICOLUM,K)=1.0								
      DO 350 K=1,N								
      A(ICOLUM,K)=A(ICOLUM,K)/AMAX						
  350 CONTINUE									
      IF (M.LE.0) GO TO 380							
      DO 370 K=1,M								
      B(ICOLUM,K)=B(ICOLUM,K)/AMAX						
  370 CONTINUE									
C										
C     REDUCE NON-PIVOT ROWS							
C										
  380 DO 550 J=1,N								
      IF (J.EQ.ICOLUM) GO TO 550						
      T=A(J,ICOLUM)								
      A(J,ICOLUM)=0.0								
      DO 450 K=1,N								
      A(J,K)=A(J,K)-A(ICOLUM,K)*T						
  450 CONTINUE									
      IF (M.LE.0) GO TO 550							
      DO 500 K=1,M								
      B(J,K)=B(J,K)-B(ICOLUM,K)*T						
  500 CONTINUE									
  550 CONTINUE									
C										
C     INTERCHANGE COLUMNS AFTER ALL PIVOT OPERATIONS HAVE BEEN PERFORMED	
C										
  600 DO 710 I=1,N								
      I1=N+1-I									
      K=INDEX(I1)/4096								
      ICOLUM=INDEX(I1)-4096*K							
      IF (K.EQ.ICOLUM) GO TO 710						
      DO 705 J=1,N								
      SWAP=A(J,K)								
      A(J,K)=A(J,ICOLUM)							
      A(J,ICOLUM)=SWAP								
  705 CONTINUE									
  710 CONTINUE									
C										
      RETURN									
      END									
C
