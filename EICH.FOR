CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC        
C                                                                               
C        PROGRAMM NAME = EICH.FOR                                               
C                                                                               
C *** PROGRAMM ZUR AUSWERTUNG VON SCHREIBERAUFNAHMEN                            
C                                                                               
C                                                                               
C DAS PROGRAMM BERECHNET  AUS DEN POSITIONEN DER EICHMARKEN EINE                
C SPLINEFUNKTION UND BERECHNET ANSCHLIESSEND DIE FREQUENZEN DER                 
C EINGEGEBENEN LINIEN.DABEI SIND DIE POSITIONEN DER EICHMARKEN BZW.             
C DER LINIEN IN ZENTIMETER ANZUGEBEN , DA DAS PROGRAMM EINEN ABLESE-            
C FEHLER VON EINEM MILLIMETER ANNIMT.                                           
C                                                                               
C EINGABE FORMATE                                                               
C                                                                               
C     ZEILENNR.     POS     FORMAT        GROESSE       BEMERKUNG               
C                                                                               
C          1       1-80     10A8           TEXT          UEBERSCHRIFT           
C          2       1-8      F8.3            FSR          IN MHZ                 
C          2       9-16     F8.3           DFSR             "                   
C          2      17-20     I4             INULL         NR DER LINIE DI        
C                                                        NULL GESETZT WE        
C                                                        SOLL                   
C          2      21-28     F8.2           OFFSET        VERSCHIEBUNG EI        
C                                                        MARKEN VOR MESS        
C                                                                               
C          3       1-8      F8.2           EICHPOS       IN CM                  
C          3       9-12     I4             NEICH         ANZAHL DER EICH        
C                                                        EN DIE UEBERSPR        
C                                                        WURDEN                 
C          3      13-16     I4             EICHEND       STEHT IN DIESER        
C                                                        SPALTEN EINE ZA        
C                                                        SO IST DIE EICH        
C                                                        EINGABE BEENDET        
C                                                                               
C          4       1-8      F8.2           LINIENPOS     IN CM                  
C          4       9-12     I4             LINIENEND     ENTSP.  EICHEND        
C          4      13-20     A8             LINIEN BEZ                           
C                                                                               
C                                                                               
C     ES KOENNEN BELIEBIG VIELE DATENSAETZE HINTEREINANDER EINGEGEBEN WE        
C                                                                               
C                                                                               
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC        
      IMPLICIT REAL * 8  (A-H,O-Z),INTEGER * 4 (I-M )                           
      REAL * 8 LINE(100,3)                                                      
      DIMENSION AUXARR(7,510),WY(501),A(501),B(501),C(501),D(501)               
      DIMENSION EICH(501,2),TEXT(10),AUX(4)                                     
      DIMENSION X(100),Y(100),YY(100),YF(100)                                   
      EQUIVALENCE (LINE(1),YY(1)),(LINE(101),YF(1))                             
      EQUIVALENCE (EICH(1),X(1)),(EICH(502),Y(1))                               
1     FORMAT(10A8)                                                              
2     FORMAT(2F8.3,I4,F8.2)                                                     
3     FORMAT(F8.2,2I4)                                                          
4     FORMAT(F8.2,I4,A8)                                                        
100   FORMAT('1')                                                               
101   FORMAT(' ',10A8)                                                          
102   FORMAT(' ',2('  ',F8.3),'  ',I4,'  ',F8.2)                                
103   FORMAT(' ','  ',F8.2,2('  ',I4))                                          
104   FORMAT('   ',F8.2,'  ',I4,'   ',A8)                                       
105   FORMAT(' ',9X,'<*** EINGABE FEHLER ***>',/,/)                             
106   FORMAT(' ',12X,'DIE ANZAHL DER EINGEGEBENEN EICHMARKEN/LINIEN '           
     *,'UEBERSCHREITET DAS ERLAUBTE MAXIMUM',/,/)                               
109   FORMAT ('1',9X,'AUSGABE DER ERMITTELTEN FREQUENZEN DER',                  
     *' SPLINE ANPASSUNG',10X,'FSR=',F8.3,' MHZ',10X,'DFSR=',F8.3,              
     *' MHZ',/,/)                                                               
110   FORMAT(10X,10A8,/,/)                                                      
111   FORMAT('+',T30,F8.2)                                                      
119   FORMAT ('1',9X,'AUSGABE DER ERMITTELTEN FREQUENZEN DES',                  
     *' SPLINE-FIT',10X,'FSR=',F8.3,' MHZ',10X,'DFSR=',F8.3,                    
     *' MHZ',/,/)                                                               
120   FORMAT(10X,' NR.',8X,'   BEZ  ',8X,'FREQUENZ(MHZ)',                       
     *8X,'FREQUENZ(FSR)')                                                       
130   FORMAT(10X,I4,8X,A8,8X,F12.2,8X,F12.4)                                    
140   FORMAT(10X,I4,A8,/,/)                                                     
150   FORMAT(' ')                                                               
160   FORMAT(10X,'EINE LINIE LIEGT AUSSERHALB DES EICHMARKENBEREICHES',         
     */,/)                                                                      
170   FORMAT(10X,'NX IST KLEINER ALS 2',/,/)                                    
180   FORMAT(10X,'INPUT IST NICHT SORTIERT',/,/)                                
190   READ(5,1,ERR=2300,END=2300)(TEXT(I),I=1,10)                               
      WRITE(6,100)                                                              
      WRITE(6,101)(TEXT(I),I=1,10)                                              
      READ(5,2) FSR,DFSR,INULL,OFFSET                                           
      WRITE(6,102) FSR,DFSR,INULL,OFFSET                                        
      MAXL=100                                                                  
      MAXE=500                                                                  
      I=0                                                                       
      IJ=0                                                                      
1000  I=I+1                                                                     
      READ(5,3,ERR=1010,END=1010) EICH(I,1),IHELP,IEND                          
      WRITE(6,103) EICH(I,1),IHELP,IEND                                         
      IF(I.GT.1)WRITE(6,111)EICH(I,1)-EICH(I-1,1)                               
      IJ=IJ+IHELP                                                               
      EICH(I,2)=I+IJ                                                            
      IF(IEND.NE.0) GOTO 1020                                                   
      IF(I.LT.MAXE) GOTO 1000                                                   
1010  WRITE(6,105)                                                              
      WRITE(6,106)                                                              
      STOP                                                                      
1020  IEICH=I                                                                   
      WRITE(6,150)                                                              
      WRITE(6,150)                                                              
      I=0                                                                       
1030  I=I+1                                                                     
      READ(5,4,ERR=1010,END=1035) LINE(I,1),IEND,LINE(I,3)                      
      WRITE(6,104) LINE(I,1),IEND,LINE(I,3)                                     
      IF (IEND.NE.0) GOTO 1040                                                  
      IF (I.LT.MAXL) GOTO 1030                                                  
      GOTO 1010                                                                 
1035  I=I-1                                                                     
1040  ILINE=I                                                                   
      HELP=EICH(1,1)                                                            
      DO 1050 I=1,IEICH                                                         
1050  EICH(I,1)=EICH(I,1)-HELP                                                  
      DO 1060 I=1,ILINE                                                         
1060  LINE(I,1)=LINE(I,1)-HELP-OFFSET                                           
      IF ((INULL.LE.0).OR.(INULL.GT.ILINE)) INULL=1                             
      WYY=.1*(Y(IEICH)-Y(1))/(X(IEICH)-X(1))                                    
      HELP=IEICH*WYY*WYY                                                        
      DO 2000 I=1,IEICH                                                         
2000  WY(I)=1.0D0                                                               
      CALL DCUSP(1,IEICH,510,HELP,X,Y,WY,A,B,C,D,ICTRL,AUXARR)                  
      IF (ICTRL.EQ.-1) WRITE (6,2010)                                           
2010  FORMAT('1 FEHLER IN DCUSP',/,/)                                           
      WRITE(6,119) FSR,DFSR                                                     
      WRITE(6,110)(TEXT(I),I=1,10)                                              
      WRITE(6,2020)                                                             
2020  FORMAT(10X,' NR.',8X,'   BEZ  ',                                          
     *8X,'FREQUENZ(MHZ)',10X,'FEHLER(MHZ)',8X,'FREQUENZ(FSR)',/)                
      DO 2200 I=1,ILINE                                                         
      DO 2100 J=1,IEICH                                                         
      IF (X(J).GT.YY(I)) GOTO 2110                                              
2100  CONTINUE                                                                  
2110  J=J-1                                                                     
      H=YY(I)-X(J)                                                              
      YF(I)=((D(J)*H+C(J))*H+B(J))*H+A(J)                                       
2200  CONTINUE                                                                  
      HELP=YF(INULL)                                                            
      DO 2205 I=1,ILINE                                                         
      YF(I)=YF(I)-HELP                                                          
      H=YF(I)*FSR                                                               
      DYF=DSQRT((FSR*WYY*2)**2+(YF(I)*DFSR)**2)                                 
      WRITE(6,2030) I,LINE(I,3),H,DYF,YF(I)                                     
2030  FORMAT(10X,I4,8X,A8,8X,F12.2,8X,F12.6,8X,F12.5)                           
2205  CONTINUE                                                                  
      WRITE(6,2210)                                                             
2210  FORMAT('1',10X,'AUSGABE DER SPLINEKOEFFIZIENTEN',/,/,/)                   
      WRITE(6,2212)                                                             
2212  FORMAT(' ',14X,'A',14X,'B',14X,'C',14X,'D',/,/)                           
2213  FORMAT(6X,8(5X,F10.6))                                                    
      DO 2215 I=1,IEICH                                                         
      WRITE(6,2213) A(I),B(I),C(I),D(I)                                         
2215  CONTINUE                                                                  
      GOTO190                                                                   
2300  CONTINUE                                                                  
      STOP                                                                      
      END                                                                       
                                                                                                   