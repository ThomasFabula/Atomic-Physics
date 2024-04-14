C     FORTRAN77-PROGRAMM 'HELPLSQF' ZUR UMWANDLUNG EINER 'ABCXFAK'/'ABFAKGEN'-	
C     AUSGABEDATEI IN EINE ZUR EINGABE IN DAS PROGRAMM 'LSQFIT' GEEIGNETE DATEI 
C     DIE  'ABFAK'-DATEI  WIRD VON FILE-NR. 11	GELESEN, DIE 'LSQFIT'-DATEI AUF 
C     FILE-NR.12  GESCHRIEBEN, SIE ENTHAELT DIE KOEFFIZIENTEN DER GEWUENSCHTEN	
C     HFS-KONSTANTEN FUER DIE IN DEN LSQFIT EINZUBEZIEHENDEN ZUSTAENDE		
C										
C     EINGABE:									
C     SP. 1	A1     TX     GEWUENSCHTE HFS-ORDNUNG  (A, B ODER C)		
C	  2	I1     KZ     LEER: 'ABFAK'-DATEI MIT 'ABCXFAK' ERSTELLT	
C				    (D-ELEKTRONEN, 3 KONFIGURATIONEN)		
C			      = 1 : 'ABFAK'-DATEI MIT 'ABFAKGEN' ERSTELLT	
C				    (L-ELEKTRONEN, 1 KONFIGURATION)		
C	 3-5	I3     M      NR. DES LETZTEN 'ABFAK'-ERGEBNISSATZES,DER NOCH IN
C			      DIE 'LSQFIT'-BERECHNUNG UEBERNOMMEN WERDEN SOLL	
C			      LEER: ALLE ERGEBNISSAETZE DER DATEI WERDEN VERWEN-
C				    DET (AUSSER DEN DURCH NZ SPEZIFIZIERTEN)	
C	 6-80  25I3    NZ     NZ-TER ERGEBNISSATZ SOLL NICHT UEBERNOMMEN WERDEN 
C			      (FUER JEDEN SATZ DIE NR. IM FORMAT  I3  ANGEBEN)	
C     FUER N DIESER KARTEN MUESSEN IM JOB N 'LSQFIT'-DATEIEN ANGELEGT WERDEN MIT
C     //GO.FT12F00N DD DSN=DATEINAME  , DIESE DUERFEN KEINE PDS-DATEIEN SEIN !	
C     BEI PDS-DATEIEN DARF PRO 'HELPLSQF'-JOB NUR 1 PDS-MEMBER ANGELEGT WERDEN, 
C     ALSO NUR 1 //GO.FT12F001...-KARTE UND 1 DATENKARTE IM JOB ENTHALTEN SEIN	
C										
      CHARACTER TX*1,S*80,T*400 						
      INTEGER NZ(25)								
    1 READ(5,2,END=10) TX,KZ,M,(NZ(N),N=1,25)					
    2 FORMAT(A1,I1,26I3)							
      IF(M.EQ.0) M=100								
      I=0									
    4 READ(11,3,END=9) S							
    3 FORMAT(A80)								
    7 IF(S(1:1).NE.TX) GO TO 4							
      I=I+1									
      DO 6 N=1,25								
    6 IF(NZ(N).EQ.I) GO TO 4							
      WRITE(6,15)								
   15 FORMAT(1H )								
      WRITE(6,13) S								
   13 FORMAT(1H ,A80)								
C										
      J=0									
    5 K=80*J+0.1								
      J=J+1									
      READ(11,3,END=9) T(K+1:K+80)						
      IF(T(K+1:K+1).EQ.' ') GO TO 5						
      S=T(K+1:K+80)								
      T(K+1:400)=' '								
      IF(KZ.EQ.1.OR.J.EQ.5.OR.(J.EQ.4.AND.TX.NE.'A')) GO TO 20			
      IF(J-3) 14,18,16								
   14 IF(TX.NE.'A') GO TO 18							
      T(101:140)=T(:40) 							
      T(141:157)=T(44:60)							
      GO TO 17									
   16 T(201:240)=T(161:200)							
      T(184:200)=T(141:157)							
      T(161:180)=T(121:140)							
      T(141:157)=T(104:120)							
      T(121:140)=T(81:100)							
      T(104:120)=T(61:77)							
   17 T(:100)=' '								
   18 K=240.1+80.*INDEX(TX,'A') 						
      J=206.1+60.*INDEX(TX,'A') 						
      DO 19 L=6,J,20								
   19 IF(T(L:L+1).EQ.'  ') T(L+1:L+3)='0.0'					
   20 WRITE(12,3)(T(L-79:L),L=80,K,80)						
      WRITE(6,13)(T(L-79:L),L=80,K,80)						
      IF(I.LT.M) GO TO 7							
    9 ENDFILE 12								
      REWIND 11 								
      IF(I.EQ.0) WRITE(6,12) TX 						
   12 FORMAT('0HFS-KONSTANTE  ',A1,'  EXISTIERT NICHT IM DATENSATZ')		
      GO TO 1									
   10 STOP									
      END									
