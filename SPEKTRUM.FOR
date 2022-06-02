C                                                                               
C                                                                               
C     PROGRAMM  'SPEKTRUM'  (TERMZUORDNUNG EINES FEINSTRUKTURSPEKTRUMS)         
C                --------   DAS PROGRAMM BERECHNET AUS <=800 EINGEGEBENEN FEIN- 
C                           STRUKTURENERGIEN DIE BZGL. J UND PARITAET ERLAUBTEN 
C                           UEBERGAENGE UND DEREN (LUFT)WELLENLAENGEN.          
C                           ES DUERFEN ALLERDINGS NUR MAX. 10000 ERLAUBTE UEBER-
C                           GAENGE IM BETRACHTETEN WELLENLAENGENBEREICH LIEGEN. 
C                                                                               
C     EINGABE                                                                   
C                                                                               
C     KARTE  SPALTE   FORMAT     NAME      BEDEUTUNG                            
C              1-10   F10.2      WMIN      UNTERE GRENZE FUER AUSZUDRUCKENDEN   
C                                          WELLENLAENGENBEREICH                 
C             11-20   F10.2      WMAX      OBERE GRENZE FUER AUSZUDRUCKENDEN    
C                                          WELLENLAENGENBEREICH                 
C                                          LEER: ES WERDEN ALLE BERECHNETEN     
C                                                WELLENLAENGEN AUSGEDRUCKT      
C             21-24     I4         Z       Z=1 :EIN-PHOTONEN-UEBERGAENGE        
C                                          Z=2 :ZWEI-PHOTONEN-UEBERGAENGE       
C                                          Z=3 :EIN- UND ZWEI- PHOTONEN         
C                                          UEBERGAENGE                          
C             25-80    14A4      TEXT(I)   BELIEB. UEBERSCHRIFTENTEXT           
C                                                                               
C     2        1-20   D20.12     E(I)      FEINSTRUKTURENERGIE (CM-1)           
C             21-36   D16.8      XJ(I)     HUELLENDREHIMPULS  J                 
C             37-40     I4       IP        KENNZIFFER F. PARITAET, LEER FUER    
C                                          GERADE PARITAET,  =1  F. UNGERADE P. 
C             41-52    3A4       BEZ(I)    TERMBEZEICHNUNG                      
C                                                                               
C     FUER JEDEN ENERGIETERM MUSS EINE KARTE VOM TYP 2 EINGEGEBEN WERDEN.       
C     AM SCHLUSS EINE LEERKARTE, DANACH KOENNEN WEITERE KOMPLETTE DATENSAETZE   
C     FOLGEN ODER DIE ENDKARTE.                                                 
C     ACHTUNG:  DA EIN GRUNDZUSTAND MIT  J = 0  UND GERADER PARITAET ALS KARTE  
C     --------  VOM TYP 2 EINE LEERKARTE ERFORDERN UND DIESE ALS ENDE DES DATEN-
C               SATZES INTERPRETIERT WUERDE, MUSS IN DEM FALL DER GRUNDZUSTAND  
C               MIT EINER VON  0.  VERSCHIEDENEN TERMENERGIE EINGEGEBEN WERDEN. 
C                                                                               
C     REGION=512K                                                               
C                                                                               
C                                                                               
      IMPLICIT REAL*8(A-H,O-Z)                                                  
      REAL*4 TEXT(15),BEZ(800,3),BEZO(800,3),BEZE(800,3)                        
      INTEGER*4 Z                                                               
      DIMENSION E(800),XJ(800),EV(800),EO(800),XJE(800),XJO(800)                
      DIMENSION NRE(800),NRO(800),W(10000),T1(10000),T2(10000)                  
      DIMENSION NRW(10000),KZ1(10000),KZ2(10000)                                
  100 READ (5,1,END=1000) WMIN,WMAX,Z,(TEXT(I),I=1,14)                          
    1 FORMAT (2D10.2,I4,14A4)                                                   
      WRITE (6,2) (TEXT(I),I=1,14)                                              
    2 FORMAT (1H1,30X,14A4/30X,62(1H=))                                         
      I=1                                                                       
      K=1                                                                       
      L=1                                                                       
    4 READ (5,5,END=20) E(I),XJ(I),IP,(BEZ(I,J),J=1,3)                          
    5 FORMAT (D20.12,D16.8,I4,3A4)                                              
      IF((E(I)+XJ(I)+IP).EQ.0.) GO TO 20                                        
      IF(IP-1) 7,8,8                                                            
    7 EV(K)=E(I)                                                                
      XJE(K)=XJ(I)                                                              
      DO 201 J=1,3                                                              
      BEZE(K,J)=BEZ(I,J)                                                        
  201 CONTINUE                                                                  
      K=K+1                                                                     
      GO TO 3                                                                   
    8 EO(L)=E(I)                                                                
      XJO(L)=XJ(I)                                                              
      DO 202 J=1,3                                                              
      BEZO(L,J)=BEZ(I,J)                                                        
  202 CONTINUE                                                                  
      L=L+1                                                                     
    3 I=I+1                                                                     
      GO TO 4                                                                   
   20 IF(K.NE.1.AND.L.NE.1) GO TO 9                                             
      IF(I.GE.3) WRITE(6,6)                                                     
      IF(I.LT.3) WRITE(6,13)                                                    
    6 FORMAT(1H-,'EINGEGEBENE ZUSTAENDE HABEN ALLE GLEICHE PARITAET')           
   13 FORMAT(1H-,'WENIGER ALS 2 ZUSTAENDE EINGEGEBEN')                          
      GO TO 100                                                                 
    9 NE=K-1                                                                    
      NO=L-1                                                                    
      CALL SORT(EV,NE,NRE)                                                      
      CALL SORT(EO,NO,NRO)                                                      
      WRITE(6,10)                                                               
   10 FORMAT (//1H-,'ZUSTAENDE MIT GERADER PARITAET'/1H ,30(1H-)/'0NR.'         
     1,7X,'ENERGIE (CM-1)',7X,'  TERMBEZ.  ',5X,'J'/)                           
      DO 12 I=1,NE                                                              
      K=NRE(I)                                                                  
      WRITE (6,11) I,EV(K),(BEZE(K,J),J=1,3),XJE(K)                             
   11 FORMAT (1H ,I3,8X,F10.3,10X,3A4,3X,F4.1)                                  
   12 CONTINUE                                                                  
      WRITE(6,14)                                                               
   14 FORMAT (1H-,'ZUSTAENDE MIT UNGERADER PARITAET'/1H ,32(1H-)/'0NR.'         
     1,7X,'ENERGIE (CM-1)',7X,'  TERMBEZ.  ',5X,'J'/)                           
      DO 16 I=1,NO                                                              
      K=NRO(I)                                                                  
      WRITE (6,11) I,EO(K),(BEZO(K,J),J=1,3),XJO(K)                             
   16 CONTINUE                                                                  
      IF (Z.EQ.2) GOTO 101                                                      
C                                                                               
      M=0                                                                       
      DO 24 K=1,NE                                                              
      DO 22 L=1,NO                                                              
      IF(DABS(XJE(K)-XJO(L)).GE.1.1) GO TO 22                                   
      IF((XJE(K).EQ.0.).AND.(XJO(L).EQ.0.)) GO TO 22                            
      IF(DABS(EV(K)-EO(L)).LE.1.D-2) GO TO 22                                   
      M=M+1                                                                     
      W(M)=1.D8/DABS(EV(K)-EO(L))                                               
      W(M)=W(M)-W(M)/3668.-123./W(M)-3.56D8/(W(M)**3)                           
      IF((W(M).GT.WMIN.AND.W(M).LT.WMAX).OR.(WMAX.EQ.0.)) GO TO 21              
      M=M-1                                                                     
      GO TO 22                                                                  
   21 KZ1(M)=K                                                                  
      KZ2(M)=L                                                                  
   22 CONTINUE                                                                  
   24 CONTINUE                                                                  
      IF (M.EQ.0) GOTO 208                                                      
      WRITE(6,30)                                                               
   30 FORMAT (1H1,'BERECHNETE WELLENLAENGEN (LUFT) FUER EIN-PHOTONEN-UEB        
     1ERGAENGE'/1H ,61(1H-)/'0 NR.',7X,'LAMBDA (A)',8X,'TERM 1 (CM-1)',1        
     29X,'J1',9X,'TERM 2 (CM-1)',19X,'J2',9X,'T2-T1 (CM-1)'/)                   
      CALL SORT(W,M,NRW)                                                        
      DO 32 I=1,M                                                               
      K=NRW(I)                                                                  
      K1=KZ1(K)                                                                 
      K2=KZ2(K)                                                                 
      IF(EV(K1).LT.EO(K2)) GOTO203                                              
      TD=EV(K1)-EO(K2)                                                          
      WRITE(6,31) I,W(K),EO(K2),(BEZO(K2,J),J=1,3),XJO(K2),EV(K1),(BEZE(        
     1K1,J),J=1,3),XJE(K1),TD                                                   
   31 FORMAT (1H ,I4,3X,F14.3,8X,2(F10.3,6X,3A4,2X,F4.1,9X),F12.3)              
      GOTO 206                                                                  
  203 TD=EO(K2)-EV(K1)                                                          
      WRITE (6,31) I,W(K),EV(K1),(BEZE(K1,J),J=1,3),XJE(K1),EO(K2),(BEZO        
     1(K2,J),J=1,3),XJO(K2),TD                                                  
  206 CONTINUE                                                                  
   32 CONTINUE                                                                  
      GOTO 210                                                                  
  208 WRITE (6,135)                                                             
  135 FORMAT (1H0,'KEINE ERLAUBTEN UEBERGAENGE IM GEWAEHLTEN WELLENLAENG        
     1ENBEREICH')                                                               
  210 CONTINUE                                                                  
      IF (Z.EQ.3) GOTO 101                                                      
      GO TO 100                                                                 
C                                                                               
  101 WRITE (6,231)                                                             
  231 FORMAT (1H1,'BERECHNETE WELLENLAENGEN (LUFT) FUER ZWEI-PHOTONEN-UE        
     1BERGAENGE'/1H ,62(1H-)/)                                                  
      WMIN=WMIN/2.                                                              
      WMAX=WMAX/2.                                                              
C   GERADE PARITAET                                                             
      WRITE (6,212)                                                             
  212 FORMAT (1H0,'GERADE PARITAET')                                            
      N=0                                                                       
  120 M=0                                                                       
      DO 124 K=1,NE                                                             
      J=K+1                                                                     
      DO 122 L=J,NE                                                             
      IF (DABS(XJE(K)-XJE(L)).GE.2.1) GOTO122                                   
      IF ((XJE(K).EQ.1.).AND.(XJE(L).EQ.0.)) GOTO122                            
      IF ((XJE(K).EQ.0.).AND.(XJE(L).EQ.1.)) GOTO122                            
      IF (DABS(EV(K)-EV(L)).LE.1.D-2) GOTO122                                   
      M=M+1                                                                     
      W(M)=1.D8/DABS(EV(K)-EV(L))                                               
      W(M)=W(M)-W(M)/3668.-123./W(M)-3.56D8/(W(M)**3)                           
      IF((W(M).GT.WMIN.AND.W(M).LT.WMAX).OR.(WMAX.EQ.0.)) GOTO121               
      M=M-1                                                                     
      GOTO122                                                                   
  121 KZ1(M)=K                                                                  
      KZ2(M)=L                                                                  
  122 CONTINUE                                                                  
  124 CONTINUE                                                                  
      IF (M.EQ.0) GOTO 209                                                      
      WRITE (6,130)                                                             
  130 FORMAT ('0 NR.',7X,'LAMBDA (A)',8X,'TERM 1 (CM-1)',19X,'J1',9X,'TE        
     1RM 2 (CM-1)',19X,'J2',9X,'T2-T1 (CM-1)'/)                                 
      CALL SORT (W,M,NRW)                                                       
      DO 132 I=1,M                                                              
      K=NRW(I)                                                                  
      W(K)=W(K)*2.                                                              
      K1=KZ1(K)                                                                 
      K2=KZ2(K)                                                                 
      IF (EV(K1).LT.EV(K2)) GOTO204                                             
      TD=EV(K1)-EV(K2)                                                          
      WRITE(6,131) I,W(K),EV(K2),(BEZE(K2,J),J=1,3),XJE(K2),EV(K1),(BEZE        
     1(K1,J),J=1,3),XJE(K1),TD                                                  
  131 FORMAT (1H ,I4,3X,F14.3,8X,2(F10.3,6X,3A4,2X,F4.1,9X),F12.3)              
      GOTO 207                                                                  
  204 TD=EV(K2)-EV(K1)                                                          
      WRITE(6,131) I,W(K),EV(K1),(BEZE(K1,J),J=1,3),XJE(K1),EV(K2),(BEZE        
     1(K2,J),J=1,3),XJE(K2),TD                                                  
  207 CONTINUE                                                                  
  132 CONTINUE                                                                  
      GOTO 211                                                                  
  209 WRITE (6,135)                                                             
  211 CONTINUE                                                                  
      IF (N.EQ.1) GOTO133                                                       
C   UNGERADE PARITAET                                                           
      NE=NO                                                                     
      WRITE (6,213)                                                             
  213 FORMAT (1H0,'UNGERADE PARITAET')                                          
      N=1                                                                       
      DO 134 I=1,NE                                                             
      XJE(I)=XJO(I)                                                             
      EV(I)=EO(I)                                                               
      DO 205 J=1,3                                                              
      BEZE(I,J)=BEZO(I,J)                                                       
  205 CONTINUE                                                                  
  134 CONTINUE                                                                  
      GOTO120                                                                   
  133 GOTO100                                                                   
 1000 WRITE(6,50)                                                               
   50 FORMAT(1H0,'ENDE')                                                        
      STOP                                                                      
      END                                                                       
                                 