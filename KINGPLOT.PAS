$TITLE KINGPLOT  : BESCHREIBUNG                                                 
PROGRAM KINGPLOT(INPUT,OUTPUT) ;                                                
(* ************************************************************************* *) 
(*                                                                           *) 
(*   PROGRAMM ZUR BERECHNUNG EINES KINGPLOTES                                *) 
(*                                                                           *) 
(*   KARTE  NR 1                                                             *) 
(*                                                                           *) 
(*   DATUM  FORMAT     VARIABLE         BEZEICHNUNG                          *) 
(*                                                                           *) 
(*    1       I          AR             MASSENZAHL DES REF. ISOTOPES         *) 
(*    2       R          MAR            MASSE IN AMU DES REF. ISOTOPES       *) 
(*    3       R          MMUE           UEBERGANGENERGIE DER REF. LINIE      *) 
(*                                      DIESER WERT MUSS IN DER GLEICHEN     *) 
(*                                      EINHEIT WIE XW ANGEGEBEN WERDEN.     *) 
(*    4       I          OP1            OP1 = 1   REF. LINIE OPTISCH         *) 
(*                                      OP1 = 2   REF. LINIE MUEONISCH       *) 
(*    5       I          OP2            OP2 = 1   KEIN PLOT                  *) 
(*                                      OP2 = 2   PLOT                       *) 
(*    6       I          A1                                                  *) 
(*    7       I          A2                                                  *) 
(*    8      A20        XTEXT           BESCHRIFTUNG DER X-ACHSE DES PLOTES  *) 
(*    9       A         YTEXT                 "       "  Y-ACHSE  "    "     *) 
(*                                                                           *) 
(*                                                                           *) 
(*   KARTE  NR 2                                                             *) 
(*                                                                           *) 
(*    1      A80        TEXTX           TEXT ZU REF. LINIE                   *) 
(*                                                                           *) 
(*                                                                           *) 
(*  KARTEN NR. 3FF    (DIE HIER ANGEGEBENEN WERTE BEZIEHEN SICH AUF DIE      *) 
(*                     REFERENZLINIE                                         *) 
(*                                                                           *) 
(*    1      I           AX             MASSENZAHL                           *) 
(*    2      R           MASS           ZUGEH. MASSE IN AMU                  *) 
(*    3      R           XW             DELNUEX (AX-AR)                      *) 
(*    4      R           DXW            FEHLER VON DELNUEX                   *) 
(*                                                                           *) 
(*  ZUM ABSCHLUSS EINE KARTE FOLGENDER FORM                                  *) 
(*                                                                           *) 
(*    0   0   0   0                                                          *) 
(*                                                                           *) 
(*  KARTEN NR. 4                                                             *) 
(*                                                                           *) 
(*    1      R        LAMBDA            WELLENLAENGE DER LINIE(IN A)         *) 
(*    2      A60      TEXTY             TEXT ZUR LINIE                       *) 
(*                                                                           *) 
(*  KARTEN NR. 5FF    WICHTIG ! :DIE KARTEN VOM TYP 5 MUESSEN IN GLEICHER    *) 
(*                               REIHENFOLGE UND ANZAHL WIE DIE KARTEN VOM   *) 
(*                               TYP 3 EINGEGEBEN WERDEN .                   *) 
(*                                                                           *) 
(*   1       I           AY             MASSENZAHL                           *) 
(*   2       R           YW             DELNUEY (AX-AR)    (IN MHZ)          *) 
(*   3       R           DYW            FEHLER VON DELNUEY                   *) 
(*                                                                           *) 
(* ************************************************************************* *) 
$EJECT                                                                          
(* ************************************************************************* *) 
(*                                                                           *) 
(*  ES KOENNEN BELIEBIG VIELE DATENSAETZ ZUR GLEICHEN REF. LINIE EINGE-      *) 
(*  GEBEN WERDEN. DAZU WIRD JEWEILS MIT DEN KARTEN VOM TYP 4 BEGONNEN.       *) 
(*                                                                           *) 
(*  BEACHTE : DA PASCAL KEINE FORMATIERTE EINGABE ZULAESST MUSS AN JEDER     *) 
(*            POSITION AUCH ETWAS STEHEN. DABEI IST ES JEDOCH EGAL WENN      *) 
(*            REALZAHLEN IN EINEM INTEGER FORMAT EINGEGEBEN WERDEN .         *) 
(*                                                                           *) 
(*            DIE OBENSTEHENDEN BEZEICHNUNGEN STEHEN FUER I (INTEGER) UND    *) 
(*            R (REAL) .                                                     *) 
(*                                                                           *) 
(* ************************************************************************* *) 
$TITLE KINGPLOT : DEKLARATIONEN                                                 
(* ************************************************************************* *) 
(*             DEKLARATIONSPART  DES PASCAL PROGRAMMS                        *) 
(* ************************************************************************* *) 
LABEL 1000 ;                                                                    
CONST C    = 20 ;                                                               
      C2   = 10 ;                                                               
      VMUE = 8.8802 ;                                                           
TYPE VEKTOR  = ARRAY (.1..2.) OF REAL ;                                         
     MATRIX  = ARRAY (.1..2,1..2.) OF REAL ;                                    
     WARRAY  = ARRAY (.1..C.) OF REAL ;                                         
     IARRAY  = ARRAY (.1..C.) OF INTEGER ;                                      
     TEXTT   = PACKED ARRAY (.1..80.) OF CHAR ;                                 
     TEXT2   = PACKED ARRAY (.1..20.) OF CHAR ;                                 
     WMATRIX = ARRAY(.1..C2.) OF WARRAY ;                                       
     VEKTOR2 = ARRAY(.1..C2.) OF REAL  ;                                        
VAR TEXTX,TEXTY                                    : TEXTT ;                    
    XTEXT,YTEXT                                    : TEXT2 ;                    
    MASS,HELP,XW,X,DXW,DX,YW,Y,DYW,DY,DDY,Z,ZW,DZW : WARRAY ;                   
    LAMB                                           : VEKTOR2 ;                  
    AY,AX                                          : IARRAY ;                   
    I,J,K,L,M,N,AR,OP1,OP2,NANZ,A1,A2,PAGE,NPLT    : INTEGER ;                  
    FLAG1,PLOTF,IER,FLAG2,FLAG,FLAGF               : BOOLEAN ;                  
    MAR,AUX,MMUE,AUX1,CHI,RMS,LAMBDA,DET,YM,YMX    : REAL ;                     
    MINV                                           : MATRIX ;                   
    P,DP                                           : VEKTOR ;                   
    Y1,Z1,DY1                                      : WMATRIX ;                  
$TITLE KINGPLOT : FACTOR WERT CALCDATA KOPF                                     
FUNCTION FACTOR ( VAR MA,MASTR : REAL ) : REAL ;           (* F A C T O R *)    
BEGIN                                                                           
   FACTOR := 1 ;                                 (* FACTOR BERECHNET  DEN    *) 
   IF ( ABS(MA-MASTR)) > 0.5 THEN FACTOR := MASTR*MA/(MASTR-MA)/1000 ;          
END ;                                            (* TERM A*A'/(A-A')         *) 
$SPACE 5                                                                        
FUNCTION WERT ( VAR X : REAL ;                             (* W E R T     *)    
                VAR P : VEKTOR ) : REAL ;                                       
BEGIN                                                                           
   WERT := P(.1.) + P(.2.) * X ;                                                
END ;                                                                           
$SPACE 5                                                                        
PROCEDURE CALCDATA ( VAR X,Z,ZW,DZW,DDY,HELP : WARRAY ;   (* C A L C D A T A *) 
                     VAR P                   : VEKTOR ;                         
                     VAR NANZ                : INTEGER ) ;                      
VAR I : INTEGER ;                                                               
BEGIN                                            (* BERECHNUNG VON YCALC     *) 
   FOR I := 1 TO NANZ DO BEGIN                                                  
      Z(.I.) := WERT ( X(.I.) ,P) ;                                             
      ZW(.I.) := Z(.I.) / HELP(.I.) ;                                           
      DZW(.I.) := ABS( DDY(.I.) / HELP(.I.)) ;                                  
   END ;                                                                        
END ;                                                                           
$SPACE 5                                                                        
PROCEDURE KOPF (VAR T:TEXTT ; VAR PAGE : INTEGER ) ;       (* K O P F   *)      
BEGIN                                                                           
   WRITELN('1SEITE : ',PAGE : 3,'         ',T:80,'          SEITE : ',PAGE:3);  
   WRITELN('-') ;                                                               
   PAGE := PAGE + 1 ;                                                           
END ;                                                                           
$TITLE KINGPLOT : READDATA  MODFEH                                              
PROCEDURE READDATA ( VAR  AX,AY             : IARRAY ;   (* R E A D D A T A *)  
                     VAR  HELP,Y,DY,YW,DYW  : WARRAY ;                          
                     VAR  TEXTY             : TEXTT  ;                          
                     VAR  LAMBDA            : REAL   ;                          
                     VAR  NANZ,PAGE         : INTEGER ) ;                       
VAR I : INTEGER ;                                                               
BEGIN                                                                           
   READLN(LAMBDA,TEXTY) ;                                                       
   KOPF (TEXTY,PAGE) ;                                                          
   WRITELN('0LAMBDA = ',LAMBDA :12:3) ;                                         
   WRITELN('-') ;                                                               
   FOR I := 1 TO NANZ DO BEGIN                                                  
      READLN (AY(.I.),YW(.I.),DYW(.I.) ) ;                                      
      IF ( AY(.I.) <> AX (.I.) ) THEN BEGIN                                     
         WRITELN ('1  EINGABE FEHLER IN <READDATA>  : ',                        
                  'DIE RECHNUNG WIRD ABGEBROCHEN ') ;                           
         GOTO 1000 ;                                                            
      END ;                                                                     
      WRITELN('0',AY(.I.): 3,YW(.I.):20:5,DYW(.I.):20:5 ) ;                     
      Y(.I.) := YW(.I.) * HELP(.I.) ;                                           
      DY(.I.) := DYW(.I.) * HELP(.I.) ;                                         
   END ;                                                                        
END ;                                                                           
$SPACE 5                                                                        
PROCEDURE MODFEH ( VAR DX,DY,DDY : WARRAY ;                (* M O D F E H *)    
                   VAR P         : VEKTOR ;                                     
                   VAR NANZ      : INTEGER ) ;                                  
VAR I : INTEGER ;                                                               
BEGIN                                                                           
   FOR I:= 1 TO NANZ DO DDY(.I.) := SQRT( DY(.I.)**2 +(DX(.I.)*P(.2.))**2) ;    
END ;                                                                           
$TITLE KINGPLOT : WRITEOUT                                                      
PROCEDURE WRITEOUT ( VAR Y,DY,MASS       : WARRAY ;       (* W R I T E O U T *) 
                     VAR AY              : IARRAY ;                             
                     VAR P,DP            : VEKTOR ;                             
                     VAR LAMBDA,MAR      : REAL   ;                             
                     VAR AR,A1,A2,NANZ   : INTEGER ) ;                          
CONST VOPT = 1836.1515 ;                                                        
VAR DMAX,MAX,AUX,AUX2,MNMS,MSMS,DMSMS : REAL ;                                  
    DNUE,DFS,DDFS,DR,DELR             : WARRAY ;                                
    AA,AB                             : IARRAY ;                                
    I,J,N,M,MD                        : INTEGER ;                               
    FLAG                              : BOOLEAN ;                               
   PROCEDURE CHANGE ( VAR A,B:INTEGER ) ;                                       
   VAR C : INTEGER ;                                                            
   BEGIN                                                                        
     C := A ;                                                                   
     A := B ;                                                                   
     B := C ;                                                                   
   END ;                                                                        
$EJECT                                                                          
BEGIN                                     (* BERECHNUNG VON FS UND DELR *)      
   FLAG := TRUE ;                         (* UND DEREN AUSGABE          *)      
   N := 1 ;                                                                     
   MNMS := -3E9/LAMBDA/VOPT ;                                                   
   MSMS := P(.1.) - MNMS ;                                                      
   FOR I := 1 TO NANZ DO                                                        
      FOR J:= I TO NANZ DO BEGIN                                                
         M := ABS(AY(.I.) - AY(.J.)) ;                                          
         MD := ABS(AY(.I.)-AR) ;                                                
         IF(((M <= 2) AND (M>0)) OR ((M=0) AND (MD<=2))) THEN BEGIN             
            IF M = 0 THEN  BEGIN                                                
               DNUE(.N.) := Y(.I.) ;                                            
               AUX := FACTOR ( MASS(.I.),MAR ) ;                                
               AA(.N.) := AY(.I.) ;                                             
               AB(.N.) := AR ;                                                  
            END ELSE BEGIN                                                      
               DNUE(.N.) := YW(.J.) - YW(.I.) ;                                 
               AUX := FACTOR ( MASS(.J.),MASS(.I.) ) ;                          
               AA(.N.) := AY(.J.) ;                                             
               AB(.N.) := AY(.I.) ;                                             
            END ;                                                               
            DFS(.N.) := DNUE(.N.) - P(.1.)/AUX ;                                
            AUX2:=( DY(.I.)**2 + DY(.J.)**2 +( DP(.1.)/AUX)**2 ) ;              
            DDFS(.N.) := SQRT(AUX2) ;                                           
            IF AA(.N.) < AB(.N.) THEN BEGIN                                     
               DNUE(.N.) := -DNUE(.N.) ;                                        
               DFS(.N.) := -DFS(.N.) ;                                          
               CHANGE ( AA(.N.),AB(.N.)) ;                                      
            END ;                                                               
            IF N = 1 THEN MAX := DFS(.N.) ;                                     
            IF FLAG THEN IF ABS(DFS(.N.)) > MAX THEN BEGIN                      
               MAX := DFS(.N.) ;                                                
               DMAX:= DDFS(.N.) ;                                               
            END ;                                                               
            IF((AA(.N.)=A1) AND (AB(.N.)=A2))OR((AA(.N.)=A2) AND (AB(.N.)=A1))  
            THEN BEGIN                                                          
               FLAG := FALSE ;                                                  
               MAX := DFS(.N.) ;                                                
               DMAX:= DDFS(.N.);                                                
            END ;                                                               
            N := N + 1 ;                                                        
         END ;                                                                  
      END ;                                                                     
   N := N-1 ;                                                                   
   WRITELN('0 MSMS = ',MSMS:15:6,' GHZ      MNMS = ',MNMS:15:6,' GHZ') ;        
   WRITELN('0') ;                                                               
   WRITELN('- A  -  A''            DNUE(MHZ)               FS(MHZ)           ', 
           '   DFS(MHZ)               ','DELR                 DDELR') ;         
   FOR I:= 1 TO N DO BEGIN                                                      
      AUX := DFS(.I.) / MAX ;                                                   
      AUX2 := AUX*SQRT((DDFS(.I.)/DFS(.I.))**2 + (DMAX/MAX)**2 ) ;              
      WRITELN('0',AA(.I.):3,' - ',AB(.I.):3,'     ',DNUE(.I.):17:6,             
              '     ',DFS(.I.):17:6,'     ',DDFS(.I.):17:7,'     ',AUX:17:6,    
              '     ',AUX2:17:7 ) ;                                             
   END ;                                                                        
END ;                                                                           
$TITLE KINGPLOT : FIT                                                           
PROCEDURE FIT ( VAR X,Y,DY   : WARRAY ;                    (* F I T *)          
                VAR P,DP     : VEKTOR ;                                         
                VAR MINV     : MATRIX ;                                         
                VAR NANZ     : INTEGER;                                         
                VAR CHI,RMS,DET : REAL   ;                                      
                VAR IER      : BOOLEAN ) ;                                      
VAR  MAT        : MATRIX ;                                                      
     V          : VEKTOR ;                                                      
     I,J,K,FR   : INTEGER ;                                                     
BEGIN                                            (* AUSFUEHREN DES FITS  *)     
   FOR I:=1 TO 2 DO BEGIN                                                       
      FOR J:=1 TO 2 DO BEGIN                     (* BER. DER MATRIX      *)     
         MAT(.I,J.) := 0 ;                                                      
         FOR K:= 1 TO NANZ DO MAT(.I,J.) := MAT(.I,J.)+DY(.K.)**(-2)            
                                            *X(.K.)**(I+J-2) ;                  
      END ;                                                                     
      V(.I.) := 0 ;                                                             
      FOR K:=1 TO NANZ DO V(.I.) := V(.I.) +DY(.K.)**(-2)*X(.K.)**(I-1)         
                                    *Y(.K.) ;                                   
   END ;                                                                        
   DET := MAT(.1,1.)*MAT(.2,2.) - MAT(.1,2.)*MAT(.2,1.) ;  (* DETERMINANTE *)   
   IF ABS(DET) < 1E-12 THEN BEGIN                                               
      IER := FALSE ;                                                            
      WRITELN('0 DETERMINANTE IST NULL : RECHNUNG WIRD ABGEBROCHEN ') ;         
   END ELSE BEGIN                                                               
      IER := TRUE ;                              (* INV. DER MATRIX     *)      
      MINV(.1,1.) :=   MAT(.2,2.)/DET ;                                         
      MINV(.2,2.) :=   MAT(.1,1.)/DET ;                                         
      MINV(.1,2.) :=  -MAT(.2,1.)/DET ;                                         
      MINV(.2,1.) :=  -MAT(.1,2.)/DET ;                                         
      FOR I:=1 TO 2 DO BEGIN                     (* BER. DER PARAMETER  *)      
         P(.I.) := 0 ;                                                          
         FOR J:=1 TO 2 DO P(.I.) := P(.I.) + MINV(.I,J.)*V(.J.) ;               
      END ;                                                                     
      CHI := 0 ;                                 (* BER. VON CHIQUADRAT *)      
      FOR I:=1 TO NANZ DO CHI := CHI + (Y(.I.)-WERT(X(.I.),P))**2/DY(.I.)**2 ;  
      FR := NANZ -2 ;                                                           
      IF FR > 0 THEN RMS := SQRT(CHI/FR) ELSE RMS := 1 ;                        
      FOR I:= 1 TO 2 DO DP(.I.) := SQRT(ABS(MINV(.I,I.)))*RMS ;                 
   END ;                                         (* FEHLER DER PARAMETER *)     
END ;                                                                           
$TITLE KINGPLOT :  WRITEDAT                                                     
PROCEDURE WRITEDAT ( VAR XW,YW,ZW,DZW  : WARRAY ;          (* W R I T D A T *)  
                     VAR P,DP          : VEKTOR ;                               
                     VAR MINV          : MATRIX ;                               
                     VAR CHI,RMS,DET   : REAL ;                                 
                     VAR NANZ,AR       : INTEGER ;                              
                     VAR AY            : IARRAY  ;                              
                     VAR TEXTV         : TEXTT) ;                               
VAR I,J   : INTEGER ;                                                           
    AUX   : REAL ;                                                              
BEGIN                                           (* AUSGABE DES GESAMTEN FIT- *) 
   WRITELN('0 AUSGABE DES FITERGEBNISSES  :  ') ;  (* ERGEBNISSES            *) 
   WRITELN('0') ;                                                               
   WRITELN('-CHI2 = ',CHI:15:7,'     RMS = ',RMS:15:7) ;                        
   WRITELN('-KONST  = ',P(.1.):15:6,' +/- ',DP(.1.):15:7) ;                     
   WRITELN('0LINEAR = ',P(.2.):15:6,' +/- ',DP(.2.):15:7) ;                     
   WRITELN('0') ;                                                               
   WRITELN('0KOVARIANZMATRIX :') ;                                              
   WRITE('0') ;                                                                 
   FOR I:=1 TO 2 DO                                                             
      FOR J:=1 TO 2 DO WRITE('A(',I:1,',',J:1,') = ',MINV(.I,J.):16:6,          
                             '       ');                                        
   WRITELN ;                                                                    
   WRITELN('-KORRELATIONSMATRIX :') ;                                           
   WRITE('0') ;                                                                 
   FOR I:=1 TO 2 DO                                                             
      FOR J:= 1 TO 2 DO BEGIN                                                   
         AUX := MINV(.I,J.) /SQRT(ABS(MINV(.I,I.)*MINV(.J,J.))) ;               
         WRITE('K(',I:1,',',J:1,') = ',AUX:16:6,'       ');                     
   END ;                                                                        
   WRITELN ;                                                                    
   WRITELN('-DETERMINANTE DES FITS = ',DET) ;                                   
   WRITELN('0') ;                                                               
   WRITELN('0') ;                                                               
   WRITELN('- A  -  A''              X                     Y(MHZ)            ', 
           '  YCALC(MHZ)          Y-YCALC(MHZ)            DXY(MHZ)') ;          
   FOR I:=1 TO NANZ DO BEGIN                                                    
      AUX := YW(.I.) -ZW(.I.) ;                                                 
      WRITELN('0',AY(.I.):3,' - ',AR:3,'     ',XW(.I.):17:6,                    
              '     ',YW(.I.):17:6,'     ',ZW(.I.):17:7,'     ',AUX:17:6,       
              '     ',DZW(.I.):17:7 ) ;                                         
   END ;                                                                        
END ;                                                                           
$TITLE KINGPLOT : READX                                    (* R E A D X *)      
PROCEDURE READX( VAR XTEXT,YTEXT           : TEXT2 ;                            
                 VAR TEXTX                 : TEXTT ;                            
                 VAR MAR,MUE               : REAL  ;                            
                 VAR AR,A1,A2,NANZ         : INTEGER ;                          
                 VAR MASS,HELP,X,XW,DX,DXW : WARRAY ;                           
                 VAR AX                    : IARRAY ;                           
                 VAR FLAG1,PLOTF,FLAG2     : BOOLEAN ) ;                        
CONST VMUE = 8.8802 ;                                                           
VAR I,J,OP1,OP2  : INTEGER ;                                                    
    AUX          : REAL    ;         (* EINLESEN ALLER ALLGEMEIN BENOETIGTEN *) 
                                     (* VARIABLEN UND DEREN AUSDRUCK         *) 
BEGIN                                                                           
   READLN ( AR,MAR,MMUE,OP1,OP2,A1,A2,XTEXT,YTEXT ) ;                           
   IF OP1 = 2 THEN FLAG1 := TRUE ELSE FLAG1 := FALSE ;                          
   IF OP2 = 2 THEN PLOTF := TRUE ELSE PLOTF := FALSE ;                          
   IF MMUE = 0 THEN FLAG2 := FALSE ELSE FLAG2 := TRUE ;                         
   READLN (TEXTX) ;                                                             
   I := 1 ;                                                                     
   LOOP UNTIL ENDFILE,LIMIT :                                                   
      READLN (AX(.I.),MASS(.I.),XW(.I.),DXW(.I.)) ;                             
      IF EOF(INPUT) THEN ENDFILE ;                                              
      IF ((AX(.I.) <1) OR (AX(.I.) > 250)) THEN LIMIT ;                         
      I := I+1 ;                                                                
   POSTLUDE                                                                     
      ENDFILE   : BEGIN                                                         
                     WRITELN('1 EOF WURDE GEFUNDEN : ABBRUCH DER RECHNUNG') ;   
                     GOTO 1000 ;                                                
                  END ;                                                         
      LIMIT     : BEGIN                                                         
                     NANZ := I-1 ;                                              
                     IF NANZ <2 THEN BEGIN                                      
                        WRITELN('1 DIE ANZAHL DER FREIHEITSGRADE IST KLEINER',  
                                'NULL : ABBRUCH DER RECHNUNG') ;                
                        GOTO 1000 ;                                             
                     END ;                                                      
                  END                                                           
   END ;                                                                        
   WRITE('1AREFF = ',AR:3,'     MREFF = ',MAR:8:4) ;                            
   IF FLAG1 THEN WRITE('     MASSENP. = ',MMUE:12:6 ) ;                         
   WRITELN ;                                                                    
   WRITELN('0ES WURDEN FOLGENDE OPTIONEN GEWAEHLT : MUON = ',FLAG1:5,           
           '      PLOT = ',PLOTF) ;                                             
   WRITELN ('0XTEXT = ',XTEXT:20,'    YTEXT = ',YTEXT:20 ) ;                    
   WRITELN ('0') ;                                                              
   WRITELN ('0ES WURDEN FUER DIE REFERENZLINIE FOLGENDE DATEN EINGEGEBEN') ;    
   WRITELN('0',TEXTX ) ;                                                        
   WRITELN('-  A         MASSE            IS             FEHLER ') ;            
   FOR I:=1 TO NANZ DO WRITELN('0',AX(.I.):3,MASS(.I.):17:6,XW(.I.):17:6,       
                                   DXW(.I.):17:7) ;                             
   IF FLAG1 THEN AUX := -MMUE/1000/VMUE ELSE AUX := 0 ;                         
   FOR I:=1 TO NANZ DO BEGIN                    (* BER. DER MOD. IS UND      *) 
      HELP(.I.) := FACTOR(MASS(.I.),MAR) ;      (* ABSPEIERN DER FAKTOREN IN *) 
      X(.I.) := XW(.I.) * HELP(.I.) -AUX   ;    (* HELP                      *) 
      DX(.I.):= DXW(.I.)* HELP(.I.) ;                                           
   END ;                                                                        
END ;                                                                           
$TITLE KINGPLOT :  MODWERTE                               (* M O D W E R T E *) 
PROCEDURE MODWERTE ( VAR X,Y,HELP     : WARRAY ;                                
                     VAR NANZ,AR      : INTEGER ;                               
                     VAR AX           : IARRAY  ;                               
                     VAR P,DP         : VEKTOR ) ;                              
VAR N,XX,YY   : REAL ;                           (* BERECHNUNG DES LOTES VON *) 
    I         : INTEGER ;                        (* MESSPUNKT AUF GERADE UND *) 
                                                 (* AUSGABE DER SCHNITTPUNKTE*) 
BEGIN                                                                           
   WRITELN('0 AUSGABE DER MODIFIZIERTEN ISOTOPIEVERSCHIEBUNGEN ') ;             
   WRITELN('0') ;                                                               
   WRITELN('- A  -  A''               X(MHZ)             Y(MHZ)');              
   FOR I := 1 TO NANZ DO BEGIN                                                  
      N := (X(.I.)-P(.1.)*P(.2.)+Y(.I.)*P(.2.))/(1+P(.2.)**2) ;                 
      XX:= N/HELP(.I.) ;                                                        
      YY :=( P(.1.) + N*P(.2.))/HELP(.I.) ;                                     
      WRITELN('0',AX(.I.):3,' - ',AR:3, XX:22:6,YY:22:6 ) ;                     
   END ;                                                                        
END ;                                                                           
$TITLE KINGPLOT :  PLOTSUB : DEFINITIONEN                                       
PROCEDURE PLOTSUB ( VAR  X,Y,DX,DY,Z     : WARRAY ;        (* P L O T S U B *)  
                    VAR  LAMB            : VEKTOR2 ;                            
                    VAR XTEXT,YTEXT      : TEXT2   ;                            
                    VAR TITLE            : TEXTT ;                              
                    VAR FLAG             : BOOLEAN ;                            
                    VAR NANZ,N,AR        : INTEGER  ;                           
                    VAR AX               : IARRAY   ;                           
                    VAR Y1,Z1,DY1        : WMATRIX ;                            
                    VAR YMIN,YMAX,LAMBDA : REAL ) ;                             
TYPE CHARVEK = PACKED ARRAY (.1..5.) OF CHAR ;                                  
VAR LENT,LENXY,INC,IREP1,IREP2,MT,I,IX : INTEGER ;                              
    YN,DDY,XN,XMAX,XX  : REAL ;                                                 
    TX                 : CHARVEK ;                                              
   PROCEDURE PTITLE ( VAR  TITLE  : TEXTT ;                (* P T I T L E *)    
                      VAR  LEN    : INTEGER ;                                   
                      VAR  YN     : REAL   ) ; FORTRAN ;                        
   PROCEDURE SMTYPE ( VAR MTYPE : INTEGER ) ; FORTRAN ;    (* S M T Y P E *)    
   PROCEDURE SYSCAL (VAR X1,X2 : REAL ;                    (* S Y S C A L *)    
                     VAR IY : INTEGER ) ; FORTRAN ;                             
   PROCEDURE ENDPLT ; FORTRAN ;                            (* E N D P L T *)    
   PROCEDURE PXYTEXT ( VAR XTEXT,YTEXT : TEXT2 ;           (* P X Y T E X T *)  
                       VAR LEN         : INTEGER ) ;                            
      PROCEDURE PXTEXT( VAR XTEXT : TEXT2 ;                                     
                        VAR LEN   : INTEGER ) ; FORTRAN ;                       
      PROCEDURE PYTEXT( VAR YTEXT : TEXT2 ;                                     
                        VAR LEN   : INTEGER ) ; FORTRAN ;                       
   BEGIN                                                                        
      PXTEXT(XTEXT,LEN) ;                                                       
      PYTEXT(YTEXT,LEN) ;                                                       
   END ;                                                                        
   PROCEDURE PDATA ( VAR X  : WARRAY ;                     (* P D A T A *)      
                      VAR IX : INTEGER ;                                        
                      VAR Y  : WARRAY ;                                         
                      VAR IY,NANZ,IREP : INTEGER ;                              
                      VAR DX : WARRAY ;                                         
                      VAR IDX: INTEGER ;                                        
                      VAR DY : WARRAY ;                                         
                      VAR IDY: INTEGER ) ; FORTRAN ;                            
$EJECT                                                                          
   PROCEDURE PTEXT ( TEXT : CHARVEK ;                      (* P T E X T *)      
                     LEN  : INTEGER ;                                           
                     XN,YN: REAL   ) ; FORTRAN ;                                
   PROCEDURE PNUMB ( VAL    : REAL ;                       (* P N U M B *)      
                    LEN,ND : INTEGER ;                                          
                    XN,YN  : REAL ) ; FORTRAN ;                                 
   PROCEDURE USCALE(     ITR         : INTEGER ;           (* U S C A L E *)    
                     VAR C1,C2,C3,C4 : REAL ) ; FORTRAN ;                       
   PROCEDURE SCSCAL ( SC : REAL ) ; FORTRAN ;              (* S C S C A L *)    
   PROCEDURE DPSIZE ( FORM, DINA : REAL ) ; FORTRAN ;      (* D P S I Z E *)    
   PROCEDURE DPBORD ( I : INTEGER ) ; FORTRAN ;            (* D P B O R D *)    
$SPACE 5                                                                        
BEGIN                                            (* ENDE DER DEKLARATIONEN *)   
   IF FLAG THEN BEGIN                            (* INITIALISIERUNG VON VAR.*)  
      YMIN := Y(.1.) ;                                                          
      YMAX := Y(.1.) ;                                                          
      N    := 1 ;                                                               
      FLAG := FALSE;                                                            
   END ;                                                                        
   IF N <= 10 THEN BEGIN                         (* SOLANGE N<10 WERDEN DIE *)  
      FOR I:= 1 TO NANZ DO BEGIN                 (* ZU ZEICHNENDEN DATEN    *)  
         IF Z(.I.) > YMAX THEN YMAX := Z(.I.) ;  (* ZWISCHEN GESPEICHERT    *)  
         IF Z(.I.) < YMIN THEN YMIN := Z(.I.) ;  (* FUER DIE SKALIERUNG     *)  
      END ;                                      (* WERDEN MINIMUM UND MAX- *)  
      Y1(.N.) := Y ;                             (* MUM BESTIMMT            *)  
      DY1(.N.):= DY ;                                                           
      Z1(.N.) := Z ;                                                            
      LAMB(.N.) := LAMBDA ;                                                     
      N := N+1 ;                                                                
   END ;                                                                        
$TITLE KINGPLOT : PLOTSUB                                                       
   IF ( N>10) OR (EOF(INPUT)) THEN BEGIN         (* FALLS DIESE BEDINGUNG   *)  
      DDY := (YMAX - YMIN) / 20 ;                (* ZUTRIFFT WIRD MIT DEM   *)  
      YMIN := YMIN - DDY ;                       (* ZEICHENEN ANGEFANGEN    *)  
      YMAX := YMAX + DDY ;                                                      
      DPSIZE ( -2.0,3.0 ) ;                      (* WAHL DER PLOT GROESSE   *)  
      DPBORD ( 0 ) ;                             (* KEIN RAND ZEICHENEN     *)  
      I := 0 ;                                                                  
      SYSCAL(YMIN,YMAX,I) ;                      (* SKALIERUNG DER Y-ACHSE  *)  
      INC := 2 ;                                                                
      LENT := 80 ;                                                              
      LENXY := 20 ;                                                             
      IREP1 := -3  ;                                                            
      IREP2 := 4 ;                                                              
      MT := 0 ;                                                                 
      SMTYPE(MT) ;                               (* WAHL DES PUNKTSYMBOLS   *)  
      FOR I:= 1 TO N-1 DO BEGIN                  (* ZEICHNEN DER PUNKTE UND *)  
         PDATA(X,INC,Y1(.I.),INC,NANZ,IREP1,DX,INC,DY1(.I.),INC) ;              
         PDATA(X,INC,Z1(.I.),INC,NANZ,IREP2,DX,INC,DY,INC) ;                    
      END ;                                      (* DER GERADEN             *)  
      YN := -0.2 ;                                                              
      SCSCAL ( 0.7) ;                            (* VERKLEINERN DER CHARAKTER*) 
      PXYTEXT(XTEXT,YTEXT,LENXY) ;               (* GROESSE UND ZEICHNEN VON *) 
      PTITLE ( TITLE,LENT,YN) ;                  (* ACHSENTEXTEN,UNTERTITLE  *) 
      SCSCAL ( 0.5) ;                            (* VERKL. DER CHARAKTERGR.  *) 
      TX := 'A'' = ' ;                                                          
      PTEXT (TX,5,0.0,1.08) ;                    (* ZEICHNEN DER BILDUEBER-  *) 
      XX := AR + 0.1   ;                         (* SCHRIFT                  *) 
      PNUMB ( XX,3,0,999.0,999.0 ) ;                                            
      XMAX := X(.1.) ;                                                          
      IX := 1 ;                                                                 
      FOR I:= 1 TO NANZ DO BEGIN                 (* ZEICHNEN DER MASSENZAHLEN*) 
         USCALE ( 1,X(.I.),YMAX,XN,YN) ;                                        
         XX := AX(.I.) + 0.1  ;                                                 
         PNUMB (XX,3,0,XN,1.04) ;                                               
         IF X(.I.) > XMAX THEN BEGIN             (* SUCHEN DES GROESSTEN     *) 
            XMAX := X(.I.) ;                     (* X - WERTES               *) 
            IX := I ;                                                           
         END ;                                                                  
      END ;                                                                     
      FOR I := 1 TO N-1 DO BEGIN                 (* ZEICHNEN DER WELLENLAENGE*) 
         USCALE ( 1,XMAX,Y1(.I,IX.),XN,YN ) ;                                   
         PNUMB ( LAMB(.I.),8,2,1.01,YN ) ;                                      
         TX := ' A   ' ;                                                        
         PTEXT (TX,2,999,999) ;                                                 
      END ;                                                                     
      ENDPLT ;                                   (* E N D   P L O T S U B    *) 
   END ;                                                                        
END ;                                                                           
                                                                                
$TITLE KINGPLOT : HAUPTPROGRAMM                                                 
(* ************************************************************************* *) 
(*                     H A U P T P R O G R A M M                             *) 
(* ************************************************************************* *) 
BEGIN                                                                           
   FLAG := TRUE ;                                                               
   PAGE := 1 ;                                                                  
   READX (XTEXT,YTEXT,TEXTX,MAR,MMUE,AR,A1,A2,NANZ,MASS,HELP,X,XW,DX,DXW,       
          AX,FLAG1,PLOTF,FLAG2 ) ;                                              
   LOOP                                          (* EINGABE SCHLEIFE         *) 
      IF EOF(INPUT) THEN EXIT ;                                                 
      READDATA (AX,AY,HELP,Y,DY,YW,DYW,TEXTY,LAMBDA,NANZ,PAGE) ;                
      FIT (X,Y,DY,P,DP,MINV,NANZ,CHI,RMS,DET,IER) ;                             
      IF IER THEN BEGIN                                                         
         MODFEH (DX,DY,DDY,P,NANZ) ;                                            
         FIT (X,Y,DDY,P,DP,MINV,NANZ,CHI,RMS,DET,IER) ;                         
         IF IER THEN BEGIN                                                      
            KOPF(TEXTY,PAGE) ;                                                  
            CALCDATA (X,Z,ZW,DZW,DDY,HELP,P,NANZ) ;                             
            WRITEDAT (XW,YW,ZW,DZW,P,DP,MINV,CHI,RMS,DET,NANZ,AR,AY,TEXTY) ;    
            KOPF(TEXTY,PAGE) ;                                                  
            MODWERTE(X,Y,HELP,NANZ,AR,AX,P,DP) ;                                
            IF PLOTF THEN PLOTSUB(X,Y,DX,DY,Z,LAMB,XTEXT,YTEXT,TEXTX,FLAG,NANZ, 
                                  NPLT,AR,AX,Y1,Z1,DY1,YM,YMX,LAMBDA) ;         
            IF FLAG1 THEN BEGIN                                                 
               KOPF(TEXTY,PAGE) ;                                               
               WRITEOUT (YW,DYW,MASS,AY,P,DP,LAMBDA,MAR,AR,A1,A2,NANZ);         
            END ;                                                               
         END ;                                                                  
      END ;                                                                     
   END ;                                                                        
1000 : END .                                                                    
                                     