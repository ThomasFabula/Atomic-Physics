C                                                                               
C-----PROGRAMM >>>>>FREQKOR<<<<< ZUR KORREKTUR DER FREQUENZEN DER               
C-----C09-AUFNAHMEN UND MITTELUNG UEBER C09-AUFNAHME UND SCHREIBER-             
C-----AUFNAHME                                                                  
C                                                                               
C=======================================================================        
C                                                                               
C VARIABLE   :                                                                  
C                                                                               
C FREQF(I)   :    ENTHAELT FREQUENZEN DER SCHREIBERAUFNAHMEN .                  
C                 FREQF(I)=0 BEDEUTET : DIESE KURVE WURDE NICHT GESEHEN         
C DFREQF(I)  :    FEHLER DER FREQUENZEN DER SCHREIBERAUFNAHMEN .                
C                 DFREQF(I)=0 :DANN WIRD DFREQ(I)=1. GESETZT.                   
C FREQP(I)   :    ENTHAELT FREQUENZEN DER C09-AUFNAHMEN                         
C                 FREQP(I)=0 BEDEUTET:  DIESE KURVE WURDE NICHT GESEHEN         
C DFREQP(I)  :    FEHLER DER FREQUENZEN DER C09-AUFNAHMEN                       
C                 DFREQP(I)=0 :DANN WIRD DFREQ(I)=1. GESETZT.                   
C BEZ(I)     :    BEZEICHNUNG DER EINZELNEN KURVEN                              
C DQU(I)     :    INVERSES FEHLERQUADRAT   QUOTIENT FREQF(I)/FREQP(I)           
C NR         :    ANZAHL DER VERSCHIEDENEN KURVEN                               
C QU         :    QUOTIENT FREQF(I)/FREQP(I)                                    
C SQU        :    SUMMATION DER  QU                                             
C TQU        :    UNGEWICHTETER MITTELWERT DER QUOTIENTEN                       
C QQU        :    MITTELWERT DES QUADRATS DER QUOTIENTEN                        
C NQU        :    ANZAHL DER QUOTIENTEN                                         
C SFE        :    HILFSGROESSE ZUM BERECHNEN DES FEHLERS DES FAKTORS            
C FAKTOR     :    PRODUKT ALLER GEWICHTETEN QUOTIENTEN                          
C DFAKT      :    FEHLER DIESES PRODUKTES                                       
C TEXT(76)   :    UEBERSCHRIFTENTEXT                                            
C                                                                               
C=======================================================================        
C EINGABE    :                                                                  
C                                                                               
C KARTE       SPALTE     FORMAT    VARIABLE  KOMMENTAR                          
C                                                                               
C   1         1-4          I4        NR      ANZAHL DER VERSCHIEDENEN KU        
C             4-80       76A1      BEZ(I)    BELIEBIGER TEXT                    
C                                                                               
C   2         1-10       F10.2     FREQF(I)  FREQUENZ DER ITEN KURVE DER        
C                                            SCHREIBERAUFNAHME                  
C             11-20      F10.2     DFREQF(I) FEHLER DER FREQUENZ DER            
C                                            SCHREIBERAUFNAHME                  
C             21-30      F10.2     FREQP(I)  FREQUENZ DER ITEN KURVE DER        
C                                            C09 -AUFNAHME                      
C             31-40      F10.2     DFREQP(I) FEHLER DER FREQUENZ DER            
C                                            C09- AUFNAHME                      
C             41-50       A10      BEZ(I)    BEZEICHNUNG DER ITEN KURVE         
C                                                                               
C=======================================================================        
C                                                                               
C                                                                               
C                                                                               
C                                                                               
C-----EINGABE DER WERTE                                                         
C                                                                               
      DIMENSION FREQF(50),FREQP(50),DFREQF(50),DFREQP(50),DQU(50)               
      CHARACTER*10 BEZ(50)                                                      
      CHARACTER*1  TEXT(76)                                                     
      REAL*8 TM,DT                                                              
      READ(5,10) NR,TEXT                                                        
 10   FORMAT(I4,76A1)                                                           
      IF(NR.LE.50)GO TO 13                                                      
      WRITE(6,*)'*****FEHLER*****,NR IST GROESSER ALS DIE ERLAUBTEN 50'         
      STOP                                                                      
 13   DO 12 I=1,NR                                                              
      READ(5,11) FREQF(I),DFREQF(I),FREQP(I),DFREQP(I),BEZ(I)                   
      IF(DFREQF(I).EQ.0.)DFREQF(I)=1.                                           
 12   IF(DFREQP(I).EQ.0.)DFREQP(I)=1.                                           
 11   FORMAT (4F10.2,A10)                                                       
C                                                                               
C-----BERECHNEN DES FAKTORS FUER C09 UND DESSEN FEHLER                          
C                                                                               
      SQU=0.                                                                    
      FAKTOR=1.                                                                 
      TQU=0.                                                                    
      QQU=0.                                                                    
      NQU=0                                                                     
      SFE=0.                                                                    
      DO 21 I=1,NR                                                              
      IF (FREQF(I)*FREQP(I).EQ.0.)GO TO 21                                      
      DQU(I)=1/(DFREQF(I)*DFREQF(I)/FREQP(I)/FREQP(I)+                          
     1(DFREQP(I)*FREQF(I)/FREQP(I)/FREQP(I))**2)                                
 21   SQU=SQU+DQU(I)                                                            
      DO 20 I=1,NR                                                              
      IF(FREQF(I)*FREQP(I).EQ.0) GO TO 20                                       
      QU=FREQF(I)/FREQP(I)                                                      
      IF (QU.LE.0.) GO TO 20                                                    
      TQU=TQU+QU                                                                
      QQU=QQU+QU*QU                                                             
      NQU=NQU+1                                                                 
      FAKTOR=FAKTOR*QU**(DQU(I)/SQU)                                            
      SFE=(DFREQF(I)*DFREQF(I)/FREQF(I)/FREQF(I)+DFREQP(I)*DFREQP(I)            
     1/FREQP(I)/FREQP(I))*DQU(I)*DQU(I)+SFE                                     
 20   CONTINUE                                                                  
      DFAKT =SQRT(SFE)/SQU*FAKTOR                                               
C                                                                               
C-----AUSGABE DER EINGABEWERTE UND DES FAKTORS                                  
C                                                                               
      CALL TIME(TM,DT)                                                          
      WRITE(6,30)TEXT,TM,DT,NR                                                  
 30   FORMAT('1',T20,'PROGRAMM >>>>>FREQKOR<<<<< ZUR AUSWERTUNG',               
     1' VON SCHREIBER UND C09-AUFNAHMEN'//                                      
     2T25,76A1/T40,'ZEIT=',A8,10X,'DATUM=',A8///                                
     3' ',T45,'*****EINGABEWERTE*****'////                                      
     4' ANZAHL DER VERSCHIEDENEN KURVEN :',I4///                                
     5' NR',T10,'BEZ',T25,'SCHREIBERFR',T40,'SCHREIBERFR',                      
     6T55,'C09-FREQ',T70,'C09-FREQ',T84,'C09-FREQ(CALC)',T99,                   
     7'C09-FREQ(CALC)','   NR'/                                                 
     8T43,'FEHLER',T71,'FEHLER',T104,'FEHLER'/                                  
     9'  I',T10,'BEZ(I)',T27,'FREQF(I)',T41,'DFREQF(I)',                        
     ZT55,'FREQP(I)',T70,'DFREQP(I)',T87,'FREQP(I)',T102,                       
     1'DFREQP(I)',T117,'I'//)                                                   
      DO 31 I=1,NR                                                              
      WRITE(6,32)I,BEZ(I),I                                                     
      IF(FREQF(I).NE.0.) WRITE(6,36)FREQF(I),DFREQF(I)                          
      IF(FREQP(I).NE.0.) WRITE(6,37)FREQP(I),DFREQP(I),                         
     1FAKTOR*FREQP(I),SQRT((DFREQP(I)*FAKTOR)**2+                               
     2(DFAKT*FREQP(I))**2)                                                      
      IF (FREQP(I).EQ.0.) GO TO 31                                              
      DFREQP(I)=SQRT((DFREQP(I)*FAKTOR)**2+(DFAKT*FREQP(I))**2)                 
      FREQP(I)=FAKTOR*FREQP(I)                                                  
 31   CONTINUE                                                                  
 32   FORMAT(' ',I4,T10,A10,T114,I4)                                            
 36   FORMAT('+',T23,F10.2,T38,F10.2)                                           
 37   FORMAT('+',T53,F10.2,T68,F10.2,T83,F10.2,T98,F10.2,T114,)                 
      IF(NQU.GT.1)GO TO 35                                                      
      WRITE(6,*)'*****FEHLER*****,DIE ANZAHL DER UEBEREINSTIMMENDEN KURV        
     1EN IST ZU KLEIN'                                                          
      STOP                                                                      
 35   WRITE( 6,33) FAKTOR,DFAKT,TQU/NQU,                                        
     1SQRT((QQU-TQU*TQU/NQU)/(NQU-1)),NQU                                       
 33   FORMAT('0DER ERMITTELTE FAKTOR IST                    :',F11.3/           
     1       ' DER FEHLER DES FAKTORS                       :',F11.3/           
     2       ' DER UNGEWICHTETE MITTELWERT                  :',F11.3/           
     3       ' DIE ZUGEHOERIGE STANDARDABWEICHUNG           :',F11.3/           
     4       ' DIE ANZAHL DER DABEI BERUECKSICHTIGTEN WERTE :   ',I4)           
C                                                                               
C-----BILDEN DER MITTELWERTE DER FREQENZEN                                      
C                                                                               
      WRITE(6,40)                                                               
 40   FORMAT('1',T45,'*****AUSGABEWERTE*****'///                                
     1' NR',T10,'BEZ',T25,'SCHREIBERFR',T40,'SCHREIBERFR',                      
     6T55,'C09-FREQ',T70,'C09-FREQ',T85,'ERGEBNIS-FREQ',T100,                   
     7'ERGEBNIS-FREQ','   NR'/                                                  
     8T43,'FEHLER',T71,'FEHLER',T104,'FEHLER'/                                  
     9'  I',T10,'BEZ(I)',T27,'FREQF(I)',T41,'DFREQF(I)',                        
     ZT55,'FREQP(I)',T70,'DFREQP(I)',T117 ,'I'//)                               
      DO 41 I=1,NR                                                              
      WRITE(6,42)I,BEZ(I),I                                                     
      IF(FREQF(I).EQ.0..AND.FREQP(I).EQ.0.) GO TO 41                            
      IF (FREQF(I).EQ.0.) WRITE(6,45)FREQP(I),DFREQP(I),                        
     1FREQP(I),DFREQP(I)                                                        
      IF (FREQP(I).EQ.0.) WRITE(6,46)FREQF(I),DFREQF(I),                        
     1FREQF(I),DFREQF(I)                                                        
      IF (FREQF(I)*FREQP(I).NE.0.)WRITE(6,47)FREQF(I),DFREQF(I),                
     1FREQP(I),DFREQP(I),(FREQF(I)/DFREQF(I)/DFREQF(I)+                         
     2FREQP(I)/DFREQP(I)/DFREQP(I))/(1/DFREQP(I)/DFREQP(I)+                     
     31/DFREQF(I)/DFREQF(I)),1/SQRT(1/DFREQP(I)/DFREQP(I)                       
     4+1/DFREQF(I)/DFREQF(I))                                                   
 41   CONTINUE                                                                  
 42   FORMAT(' ',I4,T10,A10,T114,I4)                                            
 45   FORMAT('+',T53,F10.2,                                                     
     1T68,F10.2,T83,F10.2,T98,F10.2)                                            
 46   FORMAT('+',T23,F10.2,T38,F10.2,T83,F10.2,T98,F10.2)                       
 47   FORMAT('+',T23,F10.2,T38,F10.2,T53,F10.2,                                 
     1T68,F10.2,T83,F10.2,T98,F10.2)                                            
      STOP                                                                      
      END                                                                       
      
