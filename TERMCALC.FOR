C                                                                               
C     PROGRAMME  "TERMCALC"  UND  "HFSPLOT"  ZUM PLOTTEN VON HYPERFEINSTRUKTUR- 
C                 --------         -------   TERMSCHEMATA UND -UEBERGAENGEN     
C                                                                               
C     METHODE:  FUER <=100 VORGEGEBENE MAGNETFELDWERTE (IN AEQUIDIST. SCHRITTEN)
C     WERDEN VOM PROGRAMM "TERMCALC" MIT DEN SUBROUTINES DES GI-SUCHPROGRAMMS   
C     DIE ENERGIEWERTE ALLER HFS-TERME EINES FEINSTRUKTURZUSTANDS BERECHNET.    
C     AUSSERDEM KOENNEN DIE FREQUENZEN VON ERLAUBTEN UEBERGAENGEN ZWISCHEN DIE- 
C     SEN HFS-TERMEN BESTIMMT WERDEN. DIE DATEN WERDEN AUF EINER DATEI MIT FILE-
C     NR. 15  ZWISCHENGESPEICHERT UND VOM PROGRAMM "HFSPLOT" GELESEN. DIESES BE-
C     NUTZT DIE MAGNETFELDWERTE ALS X-WERTE UND DIE TERMENERGIEN ODER DIE UEBER-
C     GANGSFREQUENZEN ALS Y-WERTE EINER RASTER- ODER TROMMELPLOTDARSTELLUNG.    
C     DABEI MUSS DIE ZAHL DER ZU PLOTTENDEN TERME BZW. UEBERGAENGE  <=500  SEIN.
C                                                                               
C                                                                               
C     BENOETIGTE PROGRAMME:                                                     
C     PROGRAMM  TERMCALC                                                        
C     UNTERPROGRAMME  TSCHEM,XMATRI,ENERAB,UNTERM   MIT DEN ZUGEHOERIGEN        
C                     UNTER-ROUTINES  FAKUL,VEKTOR,XEIGEN,DPRQD,ARRAY,MINV  UND 
C                     SYS3.FORTLIB-PROGRAMM  DGELG                              
C     UNTERPROGRAMM   SORT   (SORTIERUNG NACH ABNEHMENDER FREQUENZ)             
C     PROGRAMM  HFSPLOT  (QUELLENTEXT IN FORTRAN 77)  MIT DEN UNTERPROGRAMMEN   
C                                                     DES SPLOT-SYSTEMS         
C                                                                               
C     REGION = 512K                                                             
C                                                                               
C     AUTOREN:  S. BUETTGENBACH  UND  F. TRAEBER                                
C                                                                               
C     JUNI  1983                                                                
C                                                                               
C                                                                               
C     EINGABE                                                                   
C     -------                                                                   
C                                                                               
C     KARTE    SPALTE  FORMAT    NAME      BEDEUTUNG                            
C     --------------------------------------------------------------------------
C     1         1-80   10A8      TXT(I)    BELIEB. UEBERSCHRIFTENTEXT FUER PLOT 
C     --------------------------------------------------------------------------
C     2         1-4     F4.1     AJ        HUELLENDREHIMPULS  J                 
C               5-8     F4.1     AI        KERNSPIN  I                          
C               9-20   D12.6     AGJ       GJ-FAKTOR                            
C              21-32   D12.6     AGI       GI*1.D4                              
C              33-46   D14.6     AA        A-FAKTOR  (MHZ)                      
C              47-60   D14.6     AB        B-FAKTOR  (MHZ)                      
C              61-70   D10.3     AC        C-FAKTOR  (MHZ)                      
C              71-80   D10.3     AD        D-FAKTOR  (MHZ)                      
C     ES KOENNEN DIE KARTEN VOM TYP 2  DES PROGRAMMS  "M04AP"  VERWENDET WERDEN 
C     --------------------------------------------------------------------------
C     3         1-12   F12.5     H1        ANFANGSWERT FUER MAGNETFELD  (GAUSS) 
C                                          LEER:  H1 = 0.001 GAUSS              
C              13-24   F12.5     H2        ENDWERT FUER MAGNETFELD  (GAUSS)     
C              25-36   F12.5     DH        SCHRITTBREITE FUER MAGNETFELD (GAUSS)
C                                          LEER:  DH = (H2-H1)/50.              
C              37-42    F6.1     EMIN      UNTERE Y-GRENZE (MHZ) FUER PLOTRAHMEN
C              43-48    F6.1     EMAX      OBERE  Y-GRENZE (MHZ) FUER PLOTRAHMEN
C                                          EMIN=0 , EMAX=0 : PLOTRAHMENGRENZEN  
C                                          WERDEN INTERN AUS Y-WERTEN BERECHNET 
C                                          EMIN^0 , EMAX=0 , |KD|>3 :  PLOTDAR- 
C                                          STELLUNG IN Y-RICHTUNG UM FAKTOR |KD|
C                                          GESTRECKT MIT  EMIN  ALS ZENTRUMSWERT
C              49-54    F6.1     FTERM     FUER KP=0 :  F-QUANTENZAHL FUER TERM 
C                                          = F : PLOT DER UNTERNIVEAUS IM TERM F
C                                          =-1. : PLOT ALLER HFS-TERME          
C                                          LEER:  PLOT VON  DF=0 - UEBERGAENGEN 
C                                          FUER KP^0 :  F FUER UEBERGANG F<-->F'
C              55-60    F6.1     FTER2     =F, KP=0, FTERM=0 : PLOT DER  DF=0 - 
C                                          UEBERGAENGE IM TERM F                
C                                          =0, KP=0, FTERM=0 : PLOT ALLER DF=0 -
C                                          UEBERGAENGE                          
C                                          =F', KP^0, FTERM=F : PLOT DER UEBER- 
C                                          GAENGE  F <---> F'                   
C                                          =0, KP^0 : PLOT ALLER  |DF| <= KP -  
C                                                     BZW. |DF|=-KP -UEBERGAENGE
C              61-64     I4      KD        LEER: SYMMETRISCHE (MJ=-MJ'), SCHIEF-
C                                                SYMMETR. (|MJ+MJ'|=1)  UND UN- 
C                                                SYMMETR. UEBERGAENGE WERDEN IN 
C                                                VERSCHIEDENEN FARBEN (TROMMEL- 
C                                                PLOT) BZW. LINIENARTEN (RASTER-
C                                                PLOT) GEPLOTTET. NICHT DIREKT  
C                                                NACHWEISBARE (MJ=MJ') UEBER-   
C                                                GAENGE WERDEN WEGGELASSEN      
C                                          = 1 : WIE KD=0, ABER OHNE DRUCKOUTPUT
C                                                VON TERMEN UND FREQUENZEN      
C                                          =-1 : WIE KD=1, ABER PLOTDARSTELLUNG 
C                                                ALLER GEZEICHNETER UEBERGAENGE 
C                                                IN GLEICHER FARBE UND LINIENART
C                                          = 2 : NUR SYMMETR. UND SCHIEFSYMMETR.
C                                                UEBERGAENGE WERDEN GEPLOTTET   
C                                          =-2 : NUR DIE NICHT DIREKT NACHWEIS- 
C                                                BAREN UND DIE UNSYMMETR. UEBER-
C                                                GAENGE WERDEN GEPLOTTET        
C                                          = 3 : NUR SYMMETR. UEBERG. GEPLOTTET 
C                                          =-3 : NUR SCHIEFSYMMETR. UEBERGAENGE 
C                                          > 3 : WIE KD=0, ABER PLOTDARSTELLUNG 
C                                                IN Y-RICHTUNG UM KD GESTRECKT, 
C                                                KEINE UEBERSCHRIFT FUER PLOT   
C                                          <-3 : WIE KD>3, ABER ALLE UEBERGAENGE
C                                                IN GLEICHER FARBE UND LINIENART
C              65-68     I4      KP        LEER: PLOT VON HFS-TERMEN  (FTERM^0) 
C                                                ODER VON  DF=0 - UEBERGAENGEN  
C                                          > 0 : PLOT ALLER  |DF| <= KP -UEBERG.
C                                          <0, FTER2=0 : PLOT ALLER  |DF|=-KP - 
C                                          UEBERGAENGE                          
C                                          <0, FTERM=F, FTER2=F' : PLOT DER     
C                                          UEBERGAENGE  F <---> F'  (|F-F'|=-KP)
C              69-74    F6.2     ZX        BREITE DES PLOTRAHMENS  (IN CM)      
C                                          LEER: STANDARTWERT  ZX=25.  VERWENDET
C                                                (FUER TROMMELPLOT: ZX=50.)     
C              75-80    F6.2     ZY        HOEHE  DES PLOTRAHMENS  (IN CM)      
C                                          LEER: STANDARTWERT  ZY=18.  VERWENDET
C                                                (FUER TROMMELPLOT: ZY=36.)     
C     --------------------------------------------------------------------------
C     ES KOENNEN BELIEBIG VIELE DATENSAETZE AUFEINANDERFOLGEN                   
C                                                                               
C     AUSGABE                                                                   
C     -------                                                                   
C     DRUCKOUTPUT: WIRD VOM PROGRAMM  "TERMCALC"  ERZEUGT UND LIEFERT FUER JEDES
C                  MAGNETFELD DIE QUANTENZAHLEN UND TERMENERGIEN BZW. DIE UEBER-
C                  GANGSFREQUENZEN, SORTIERT NACH TERMFOLGE BEI NULLFELD UND BEI
C                  FELD H (ZUM IDENTIFIZIEREN DER GEPLOTTETEN TERME/UEBERGAENGE)
C     PLOTOUTPUT:  WIRD VOM PROGRAMM  "HFSPLOT"  ERZEUGT. EINGABEDATEN SIND DIE 
C                  VON  "TERMCALC"  VORHER AUF EINE DATEI MIT FILE-NR. 15  GE-  
C                  SCHRIEBENEN ERGEBNISSE                                       
C                                                                               
C                                                                               
      IMPLICIT REAL*8(A-H,O-Z)                                                  
      REAL*4 H1,H2,ZX,ZY,EMX,EMN,EMIN,EMAX,H(100)                               
      REAL*4 AM(19,18),AKO(18,18),EPS,CDP(18,361)                               
      DIMENSION XM(361),XMJ(361),SX(39),SY(19,39),SZ(19,39),TXT(10)             
      DIMENSION FR(1000),NFR(1000),JW(1000),LW(1000),KZ(1000)                   
      COMMON /PAR/AGI,AGJ,AA,AB,AC,AD                                           
      COMMON /FEHLER/ADGI,ADGJ,ADA,ADB,ADC,ADD                                  
      COMMON /DREHIM/AI,AJ /FKLEFE/XF(19),XW(19),XDW(19),XGF(19)                
      COMMON /TERM/SF(361),SM(361),SMJ(361)                                     
      COMMON /GEORDN/XXF(19),XXM(39)                                            
      COMMON /ABL/AM,AKO                                                        
      COMMON /TE/XT(361),XAL(361),ADP(18,361)                                   
      COMMON /FELD/XH /VEK/AV(361,19)                                           
      COMMON /CONAB/CDP                                                         
C                                                                               
    1 READ(5,2,END=20) (TXT(I),I=1,10)                                          
    2 FORMAT(10A8)                                                              
      READ(5,3) AJ,AI,AGJ,GI,AA,AB,AC,AD                                        
    3 FORMAT(2F4.1,2D12.6,2D14.6,2D10.3)                                        
      AGI=GI*1.D-4                                                              
      READ(5,4) H1,H2,DH,EMIN,EMAX,FTERM,FTER2,KD,KP,ZX,ZY                      
    4 FORMAT(3F12.5,4F6.1,2I4,2F6.2)                                            
      IF(DH.EQ.0.) DH=ABS(H2-H1)/50.                                            
      NH=1                                                                      
      IF(H2.NE.H1) NH=ABS(H2-H1)/DH+1.                                          
      DDH=0.                                                                    
      IF(H1.GT.0.) GO TO 13                                                     
      H1=0.001                                                                  
      DDH=H1                                                                    
      IF(H2.EQ.0.) H2=H1                                                        
   13 XH=AMIN1(H1,H2)                                                           
      EMN=1.E5                                                                  
      EMX=-1.E5                                                                 
      CALL TSCHEM(KF,KM,KMJ,KMI,KT,XM,XMJ)                                      
      NT=KT                                                                     
      LD=IABS(KD)                                                               
C                                                                               
C     AUSWAHL DER ZU ZEICHNENDEN UEBERGAENGE                                    
C                                                                               
      IF(KP.EQ.0.AND.FTERM.NE.0.) GO TO 6                                       
      N=1                                                                       
      KT1=KT-1                                                                  
      DF=DFLOAT(IABS(KP))                                                       
      DO 23 J=1,KT1                                                             
      J1=J+1                                                                    
      DO 22 L=J1,KT                                                             
      IF(DABS(SM(J)-SM(L)).GT.1.) GO TO 22                                      
      IF(DABS(SF(J)-SF(L)).GT.DF) GO TO 22                                      
      IF(DABS(SF(J)-SF(L)).NE.DF.AND.KP.LT.0) GO TO 22                          
      IF((FTER2.EQ.0.).OR.(KP.EQ.0.AND.SF(J).EQ.FTER2)) GO TO 21                
      IF((SF(J).NE.FTERM.OR.SF(L).NE.FTER2).AND.(SF(L).NE.FTERM.OR.SF(J)        
     1.NE.FTER2)) GO TO 22                                                      
   21 IF(SMJ(J).EQ.SMJ(L).AND.KD.NE.-2) GO TO 22                                
      KZ(N)=4                                                                   
      SUMJ=DABS(SMJ(J)+SMJ(L))                                                  
      IF(SUMJ.LE.1.) KZ(N)=4.*SUMJ+1.1                                          
      IF(SMJ(J).EQ.SMJ(L)) KZ(N)=3                                              
      IF(KD.EQ.2.AND.KZ(N).EQ.4) GO TO 22                                       
      KZAS=KZ(N)-3.5                                                            
      IF(KD.EQ.-2.AND.KZAS.NE.0) GO TO 22                                       
      IF(KD.EQ.3.AND.KZ(N).NE.1) GO TO 22                                       
      IF(KD.EQ.-3.AND.KZ(N).NE.5)GO TO 22                                       
      JW(N)=J                                                                   
      LW(N)=L                                                                   
      N=N+1                                                                     
   22 CONTINUE                                                                  
   23 CONTINUE                                                                  
      NT=N-1                                                                    
      IF(NT.EQ.0) GO TO 1                                                       
      IF(KP.EQ.0.AND.H1.LE.50.) EMN=0.                                          
C                                                                               
    6 WRITE(15,11) NH,NT,KP,KD,KT                                               
   11 FORMAT(5I4)                                                               
      WRITE(6,18)  (TXT(I),I=1,10),AJ,AI,AGJ,AGI,AA,AB,AC,AD                    
      WRITE(6,19) H1,H2,DH,FTERM,FTER2,KD,KP                                    
   18 FORMAT(1H1,10A8/1H ,80(1H-)/1H-,'J =',F4.1,8X,'I =',F4.1,12X,'GJ =        
     1',F11.8,9X,'GI =',F12.8,15X,'A =',F12.6,'  MHZ'/90X,'B =',F12.6,          
     2'  MHZ'/90X,'C =',F12.6,'  MHZ'/90X,'D =',F12.6,'  MHZ')                  
   19 FORMAT('0H(1) =',F10.3,' G',9X,'H(N) =',F10.3,' G',9X,'DH =',F9.3,        
     1' G',12X,'F =', F5.1,6X,4HF' =,F5.1,7X,'KD =',I3,6X,'KP =',I3)            
C                                                                               
C     BERECHNUNG UND AUSDRUCK VON TERMENERGIEN UND FREQUENZEN BEI MAGNETFELD H  
C                                                                               
      DO 5 I=1,NH                                                               
      H(I)=SNGL(XH)                                                             
      CALL XMATRI(KF,KM,SX,SY,SZ)                                               
      CALL ENERAB(KF)                                                           
      KNN1=0                                                                    
      CALL UNTERM(KF,KM,KT,SX,SY,SZ,KNN1)                                       
C                                                                               
      IF(KP.EQ.0.AND.FTERM.NE.0.) GO TO 15                                      
      IF(LD.NE.1) WRITE(6,26) H(I)                                              
   26 FORMAT(1H-,'H  =',F10.3,'  GAUSS'/'0NR.     F1',7X,'MF1',7X,'MJ1',        
     110X,'F2',7X,'MF2',7X,'MJ2',12X,'TERM1 (MHZ)',10X,'TERM2 (MHZ)',           
     212X,'FREQUENZ (MHZ)'/)                                                    
      DO 24 N=1,NT                                                              
      J=JW(N)                                                                   
      L=LW(N)                                                                   
      FREQ=XT(J)-XT(L)                                                          
      FR(N)=DABS(FREQ)                                                          
      EMX=AMAX1(EMX,SNGL(FR(N)))                                                
      EMN=AMIN1(EMN,SNGL(FR(N)))                                                
      IF(LD.NE.1) WRITE(6,27) SF(J),SM(J),SMJ(J),SF(L),SM(L),SMJ(L),            
     1XT(J),XT(L),FREQ                                                          
   27 FORMAT(1H ,6X,2(F5.1,4X,F5.1,5X,F5.1,8X),2(F15.6,6X),2X,F15.6)            
   24 CONTINUE                                                                  
      WRITE(15,7) (FR(N),N=1,NT)                                                
      IF(LD.EQ.1) GO TO 29                                                      
      WRITE(6,28)                                                               
   28 FORMAT(1H )                                                               
      CALL SORT(FR,NT,NFR)                                                      
      DO 25 K=1,NT                                                              
      M=NT+1-K                                                                  
      N=NFR(M)                                                                  
      J=JW(N)                                                                   
      L=LW(N)                                                                   
      IF(XT(J).GE.XT(L)) GO TO 31                                               
      J=LW(N)                                                                   
      L=JW(N)                                                                   
   31 WRITE(6,37) K,SF(J),SM(J),SMJ(J),SF(L),SM(L),SMJ(L),XT(J),XT(L),          
     1FR(N)                                                                     
   37 FORMAT(I4,3X, 2(F5.1,4X,F5.1,5X,F5.1,8X),2(F15.6,6X),2X,F15.6)            
   25 CONTINUE                                                                  
      GO TO 29                                                                  
C                                                                               
   15 IF(LD.NE.1) WRITE(6,16) H(I)                                              
   16 FORMAT(1H-,'H  =',F10.3,'  GAUSS'/1H0,2X ,'F',14X,'MF',13X,'MJ',          
     113X,'TERMENERGIE (MHZ)'/)                                                 
      WRITE(15,7) (XT(J),J=1,KT)                                                
      DO 12 J=1,KT                                                              
      IF((FTERM.NE.-1.).AND.(FTERM.NE.SF(J))) GO TO 12                          
      EMX=AMAX1(EMX,SNGL(XT(J)))                                                
      EMN=AMIN1(EMN,SNGL(XT(J)))                                                
      IF(LD.NE.1) WRITE(6,17) SF(J),SM(J),SMJ(J),XT(J)                          
   17 FORMAT(1H ,3(F5.1,10X),F14.6)                                             
   12 CONTINUE                                                                  
   29 XH=XH+DH-DDH                                                              
      DDH=0.                                                                    
    5 CONTINUE                                                                  
C                                                                               
C     AUSGABE DER FUER DEN PLOT BENOETIGTEN DATEN AUF DATEI MIT FILE-NR. 15     
C                                                                               
      IF((EMIN.EQ.0.).AND.(EMAX.EQ.0.)) GO TO 33                                
      IF(LD.GT.3.AND.EMAX.EQ.0.) GO TO 32                                       
      EMX=AMAX1(EMIN,EMAX)                                                      
      EMN=AMIN1(EMIN,EMAX)                                                      
      GO TO 33                                                                  
   32 DM=(EMX-EMN)*0.5D0/LD                                                     
      EMX=EMIN+SNGL(DM)                                                         
      EMN=EMIN-SNGL(DM)                                                         
   33 WRITE(15,8) (TXT(K),K=1,10),H1,H2,EMN,EMX,FTERM,ZX,ZY                     
      WRITE(15,9)  (SF(J),J=1,KT)                                               
      IF(KP.NE.0.OR.FTERM.EQ.0.) WRITE(15,10) (KZ(N),N=1,NT)                    
      WRITE(15,7) (H(I),I=1,NH)                                                 
    8 FORMAT(10A8/4F14.6,3F8.2)                                                 
    9 FORMAT(16F5.1)                                                            
   10 FORMAT(40I2)                                                              
    7 FORMAT(8F10.3)                                                            
      GO TO 1                                                                   
   20 REWIND 15                                                                 
      WRITE(6,30)                                                               
   30 FORMAT(' ENDE')                                                           
      STOP                                                                      
      END                                                                       
                                                                                                             