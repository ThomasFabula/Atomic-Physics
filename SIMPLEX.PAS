PROGRAM SIMPLEX (INPUT,OUTPUT) ;                                                
(* ************************************************************************* *) 
(*                                                                           *) 
(* PROGRAMM : SIMPLEX                                                        *) 
(*                                                                           *) 
(* EINGABEN   I = INTEGER    R = REAL                                        *) 
(*                                                                           *) 
(*                                                                           *) 
(*                                                                           *) 
(* K.-TYP        FORMAT    VARIABLE        BEDEUTUNG                         *) 
(*                                                                           *) 
(*   1            A80        KOPF          UEBERSCHRIFTEN TEXT ( ERSCHEINT   *) 
(*                                         AUCH IM PLOT )                    *) 
(*                                                                           *) 
(*   2            I          NBER          ANZAHL BEREICHE                   *) 
(*   2            I          NSTAT         =1 => FEHLER DER MESSWERTE ER-    *) 
(*                                            GIBT SICH AUS DER WURZEL.      *) 
(*                                         =BELIEBIG FEHLER WIRD 1 GESETZT   *) 
(*                                         FALLS EIN FEHLER EXPLIZIT ANGE-   *) 
(*                                         GEBEN WIRD SO WIRD DIESER WERT    *) 
(*                                         ANGENOMMEN .                      *) 
(*   2            I          NIT           MAX. ANZAHL ITERATIONEN           *) 
(*   2            I          NCHI          CHI2 DES FITES WIRD FUER JEDE     *) 
(*                                         NCHI-TE ITERATION AUSGEDRUCKT.    *) 
(*   2            I          OP            =1 => EINGELESENEN MESSWERTE      *) 
(*                                               WERDEN AUSGEDRUCKT .        *) 
(*   2            I          NAQUI         =1 => MESSWERTE FOLGEN IN AEQUI-  *) 
(*                                               DISTANTEN ABSTAENDEN.       *) 
(*   2            I          NPLOT         =1 => ES WIRD EIN PLOT FUER       *) 
(*                                            JEDEN EINGEGEBENEN BEREICH     *) 
(*                                            DURCHGEFUEHRT .                *) 
(*                                            UMFASST EIN BEREICH MEHR ALS   *) 
(*                                            1000 PUNKTE SO WERDEN PRO      *) 
(*                                            1000 PUNKTE EIN PLOT ERZEUGT.  *) 
(*  2             R           SX           DIESE BEIDEN WERTE BESTIMMEN DIE  *) 
(*  2             R           SY           GROESSE DES PLOTRAHMENS.SIE       *) 
(*                                         MUESSEN AUCH EINGEGEBEN WERDEN    *) 
(*                                         WENN KEIN PLOT GEWUENSCHT WIRD.   *) 
(*                                                                           *) 
(*                                                                           *) 
(* FALLS NAQUI AUF 1 GESETZT WIRD MUSS  EINE KARTE VOM TYP 2' FOLGEN         *) 
(*                                                                           *) 
(*                                                                           *) 
(*  2'            R           X0           STARTWERT UND INCREMENT DER       *) 
(*  2'            R           DX           X - EINGABE .                     *) 
(*                                                                           *) 
(*                                                                           *) 
(*                                                                           *) 
(*  FUER JEDEN DER NBER - BEREICHE MUESSEN KARTEN VOM TYP 3 UND 4 FOLGEN     *) 
(*                                                                           *) 
(*                                                                           *) 
(*  3             R           XANF         INTERVALLGRENZEN DES BEREICHES    *) 
(*  3             R           XEND                                           *) 
(*  3             R           PCONST       STARTWERT FUER UNTERGRUND         *) 
(*  3             I           PCFLAG       1= PARAMETER WIRD NICHT VAR.      *) 
(*                                         0= PARAMETER WIRD VARIIERT        *) 
(*  3             I           NCOMP        ANZAHL DER TEILKURVEN             *) 
(*                                                                           *) 
(*  FUER JEDE TEILKURVE MUSS EINE KARTE VOM TYP 4 FOLGEN                     *) 
(*                                                                           *) 
(*  4             R           X0           STARTWERTE FUER KURVE             *) 
(*  4             R           HWB                     ""                     *) 
(*  4             R           AMP                     ""                     *) 
(*  4             I           NGAUSS       =1 => ES WIRD GAUSSKURVE GEFITTET *) 
(*                                               SONST LORENTZKURVE          *) 
(*  4             R           NFIX         =1 => HWB WIRD FESTGEHALTEN       *) 
(*                                              SONST FREI VARIIERT          *) 
(*  4            A10          BEZ          BEZEICHNUNG DER TEILKURVE         *) 
(*                                                                           *) 
(*                                                                           *) 
(*  FUER DEN NAECHSTEN BEREICH FOLGEN NUN WIEDER DIE KARTEN VOM TYP 3        *) 
(*  WENN ALLE BEREICHE EINGEGEBEN WURDEN FOLGEN DIE MESSDATEN                *) 
(*                                                                           *) 
(*                                                                           *) 
(*   5            R            Y            MESSWERT                         *) 
(*   5            R            DY           FEHLER DES MESSWERTES            *) 
(*   5            R            X                                             *) 
(*                                                                           *) 
(*                                                                           *) 
(*                                                                           *) 
(*  DAS PROGRAMM SIMPLEX FUEHRT NUN FUER JEDEN BEREICH EINEN FIT FUER DIE    *) 
(*  EINGEGEBENEN KURVEN DURCH.FALLS GEWUENSCHT WIRD AUCH EIN PLOT ERSTELLT.  *) 
(*                                                                           *) 
(*                                                                           *) 
(*  BEACHTE : DIESES PROGRAMM WURDE IN PASCAL GESCHRIEBEN, D. H. ES KENNT    *) 
(*            NUR DIE UNFORMATIERTE EINGABE.DESHALB MUSS UEBERALL WO         *) 
(*            EINE EINGABE VERLANGT WIRD AUCH ETWAS STEHEN. DIE EINZELNEN    *) 
(*            WERTE MUESSEN MIT MINDESTENS EINEM BLANK GETRENNT SEIN .       *) 
(*            EINE AUSNAHME BESTEHT BEI DEN KARTEN VOM TYP 5.                *) 
(*                                                                           *) 
(*                                                                           *) 
(*                                                                           *) 
(*                                                                           *) 
(*                                                                           *) 
(*                                                                           *) 
(*                                                                           *) 
(*                                                                           *) 
(*                                                                           *) 
(*                                                                           *) 
(* ************************************************************************* *) 
$EJECT                                                                          
LABEL 1000 ;                                                                    
CONST NCURVE = 15 ;                                                             
      NPAR = 46 ;                                                               
      NWERTE = 5000 ;                                                           
TYPE TISIMP = ARRAY (.1..NPAR.) OF INTEGER ;                                    
     TTEXT  = PACKED ARRAY (.1..80.) OF CHAR ;                                  
     TRARR30 = ARRAY (.1..30.) OF REAL ;                                        
     TRARR3 = ARRAY (.1..3.) OF REAL ;                                          
     TBARR30= ARRAY (.1..30.) OF BOOLEAN ;                                      
     TRARRNPAR = ARRAY (.1..NPAR.) OF REAL ;                                    
     TWERTE = ARRAY (.1..NWERTE.) OF TRARR3 ;                                   
     TSIMP = ARRAY (.0..NPAR.) OF TRARRNPAR ;                                   
     TCHI  = ARRAY (.0..NPAR.) OF REAL ;                                        
     TBEZ = ARRAY (.1..NCURVE.) OF PACKED ARRAY (.1..10.) OF CHAR ;             
     BERPOINT = @BERRECORD ;                                                    
     BERRECORD = RECORD                                                         
                    XANF,XEND,PCONST : REAL ;                                   
                    PCFIX            : BOOLEAN ;                                
                    NCOMP            : INTEGER ;                                
                    NEXTB            : BERPOINT ;                               
                    X0               : TRARR30 ;                                
                    SIG              : TRARR30 ;                                
                    A                : TRARR30 ;                                
                    FIX              : TBARR30 ;                                
                    GAUSS            : TBARR30 ;                                
                    BEZ              : TBEZ ;                                   
                    BEST             : INTEGER ;                                
                    YMAX             : REAL ;                                   
                 END ;                                                          
$SPACE 2                                                                        
VAR CHI : TCHI ;                                                                
    Y   : TWERTE ;                                                              
    SIMP : TSIMP ;                                                              
    MSIMP,HELP : TRARRNPAR ;                                                    
    KOPFBER,ACTB : BERPOINT ;                                                   
    NNIT,I,NW,NBER,NIT,IIT,INCHI,NCHI,OP,NP,IBA,IBE,NWORSE,NBEST,AUX :INTEGER ; 
    STAT,AQUI,PLOT,FLAG : BOOLEAN ;                                             
    SX,SY,X0,DX,MCHI,HCHI : REAL ;                                              
    KOPF : TTEXT ;                                                              
    ISIMP : TISIMP ;                                                            
$EJECT                                                                          
PROCEDURE CHANGE ( VAR N:INTEGER ; VAR B:BOOLEAN ) ;                            
BEGIN                                                                           
  B:=FALSE ;                                                                    
  IF N=1 THEN B:= TRUE ;                                                        
END ;                                                                           
$SPACE 5                                                                        
PROCEDURE ERROR ;                                                               
BEGIN                                                                           
  WRITE('-  <*** ERROR ***>  :  ') ;                                            
END ;                                                                           
$SPACE 5                                                                        
PROCEDURE READPOINTS ( VAR Y:TWERTE ; VAR NW : INTEGER ;                        
                       VAR STAT,AQUI:BOOLEAN ; X0,DX : REAL ) ;                 
VAR N : INTEGER ;                                                               
    X : REAL ;                                                                  
BEGIN                                                                           
  N := 1 ;                                                                      
  IF AQUI THEN X := X0 ;                                                        
  LOOP                                                                          
    IF EOF(INPUT) THEN EXIT ;                                                   
    IF AQUI THEN BEGIN                                                          
      IF STAT THEN BEGIN                                                        
         READLN (Y(.N,1.));                                                     
         Y(.N,2.) := SQRT(ABS(Y(.N,1.))) ;                                      
      END ELSE READLN ( Y(.N,1.) , Y(.N,2.));                                   
      Y(.N,3.) := X ;                                                           
      X := X + DX ;                                                             
    END ELSE BEGIN                                                              
      IF STAT THEN BEGIN                                                        
         READLN(Y(.N,1.),Y(.N,3.));                                             
         Y(.N,2.) := SQRT(ABS(Y(.N,1.))) ;                                      
      END ELSE READLN (Y(.N,1.),Y(.N,2.),Y(.N,3.)) ;                            
    END ;                                                                       
    N := N+1 ;                                                                  
  END ;                                                                         
  NW := N-2 ;                                                                   
END ;                                                                           
$EJECT                                                                          
PROCEDURE READBER ( VAR KOPFBER:BERPOINT ; VAR NBER : INTEGER ) ;               
VAR I,J,K,F1,F2 : INTEGER ;                                                     
    ACTB,LASTB : BERPOINT ;                                                     
    FLAG1       : BOOLEAN ;                                                     
BEGIN                                                                           
  FLAG1 := TRUE ;                                                               
  FOR I:=1 TO NBER DO BEGIN                                                     
    NEW (ACTB) ;                                                                
    IF FLAG1 THEN BEGIN                                                         
      FLAG1 := FALSE ;                                                          
      LASTB := ACTB ;                                                           
      KOPFBER := ACTB ;                                                         
    END ELSE BEGIN                                                              
      LASTB@.NEXTB := ACTB ;                                                    
      LASTB := ACTB ;                                                           
    END ;                                                                       
    WITH ACTB@ DO BEGIN                                                         
      READLN ( XANF,XEND,PCONST,F1,NCOMP) ;                                     
      CHANGE (F1,PCFIX) ;                                                       
      FOR J:=1 TO NCOMP DO BEGIN                                                
        READLN ( X0(.J.),SIG(.J.),A(.J.),F1,F2,BEZ(.J.) ) ;                     
        CHANGE(F1,GAUSS(.J.)) ;                                                 
        CHANGE(F2,FIX(.J.)) ;                                                   
      END ;                                                                     
    END ;                                                                       
    LASTB@.NEXTB := NIL ;                                                       
  END ;                                                                         
END ;                                                                           
$EJECT                                                                          
PROCEDURE READDATA ( VAR KOPF : TTEXT ;                                         
                     VAR NBER,NIT,NCHI,OP : INTEGER ;                           
                     VAR STAT,AQUI,PLOT : BOOLEAN ;                             
                     VAR SX,SY,X0,DX : REAL ;                                   
                     VAR KOPFBER : BERPOINT ;                                   
                     VAR Y : TWERTE ;                                           
                     VAR NW : INTEGER ) ;                                       
VAR NSTAT,NPLOT,NAQUI : INTEGER ;                                               
BEGIN                                                                           
  READLN ( KOPF ) ;                                                             
  READLN ( NBER,NSTAT,NIT,NCHI,OP,NAQUI,NPLOT,SX,SY ) ;                         
  CHANGE(NSTAT,STAT) ;                                                          
  CHANGE(NAQUI,AQUI) ;                                                          
  CHANGE(NPLOT,PLOT) ;                                                          
  IF AQUI THEN READLN ( X0,DX) ;                                                
  READBER (KOPFBER , NBER ) ;                                                   
  READPOINTS ( Y,NW,STAT,AQUI,X0,DX) ;                                          
END ;                                                                           
$SPACE 5                                                                        
PROCEDURE FINDUP ( VAR ACTB : BERPOINT ;                                        
                   VAR SIMP : TRARRNPAR ;                                       
                   VAR ISIMP: TISIMP ;                                          
                   VAR NP   : INTEGER ;                                         
                       NPAR : INTEGER ;                                         
                   VAR FLAG : BOOLEAN ) ;                                       
VAR I : INTEGER ;                                                               
BEGIN                                                                           
  NP := 1 ;                                                                     
  IF NOT ACTB@.PCFIX  THEN BEGIN                                                
    ISIMP(.NP.) := 0 ;                                                          
    SIMP(.NP.) := ACTB@.PCONST ;                                                
    NP := 2 ;                                                                   
  END ;                                                                         
  FOR I := 1 TO ACTB@.NCOMP DO BEGIN                                            
    WITH ACTB@ DO BEGIN                                                         
      SIMP(.NP.) := X0(.I.) ; ISIMP(.NP.) := 1 ;                                
      IF NOT FIX(.I.) THEN BEGIN                                                
        SIMP(.NP+1.) := SIG(.I.) ; ISIMP(.NP+1.) := 2 ;                         
        NP := NP+3 ;                                                            
      END ELSE NP := NP+2 ;                                                     
      SIMP(.NP-1.) := A(.I.) ; ISIMP(.NP-1.) := 3 ;                             
    END ;                                                                       
  END ;                                                                         
  NP := NP -1 ;                                                                 
END ;                                                                           
$EJECT ;                                                                        
FUNCTION YCURV ( VAR X,X0,SIG,A : REAL ; FLAG : BOOLEAN ) : REAL ;              
VAR AUX : REAL ;                                                                
BEGIN                                                                           
  IF FLAG THEN BEGIN                                                            
    AUX := SIG / 2.35482 ;                                                      
    AUX :=-0.5 *((X-X0)/AUX)**2 ;                                               
    IF AUX < -50 THEN YCURV := 0 ELSE YCURV := A * EXP ( AUX ) ;                
  END ELSE BEGIN                                                                
    AUX := (SIG/2)**2 ;                                                         
    YCURV := A * AUX/((X-X0)**2 + AUX ) ;                                       
  END ;                                                                         
END ;                                                                           
$SPACE 5                                                                        
PROCEDURE FINDIB ( VAR Y : TWERTE ; VAR IBA,IBE,NW : INTEGER ;                  
                   VAR ACTB : BERPOINT  ) ;                                     
VAR I : INTEGER ;                                                               
    X : REAL ;                                                                  
BEGIN                                                                           
  ACTB@.YMAX := 0 ;                                                             
  IBA := 1 ;                                                                    
  IBE := NW ;                                                                   
  WITH ACTB@ DO BEGIN                                                           
    IF XANF > XEND THEN BEGIN                                                   
      X := XANF ;                                                               
      XANF := XEND ;                                                            
      XEND := X ;                                                               
    END ;                                                                       
    I := 1 ;                                                                    
    REPEAT                                                                      
      IF XANF >= Y(.I,3.) THEN IBA := I ;                                       
      IF XEND <= Y(.I,3.) THEN BEGIN                                            
        IBE := I ;                                                              
        I := NW ;                                                               
      END ;                                                                     
      I := I+1 ;                                                                
    UNTIL I >= NW ;                                                             
    FOR I := IBA TO IBE DO BEGIN                                                
      IF Y(.I,1.) > YMAX THEN YMAX := Y(.I,1.) ;                                
    END ;                                                                       
  END ;                                                                         
END ;                                                                           
$EJECT                                                                          
PROCEDURE CREATSIMP ( VAR SIMP : TSIMP ; VAR ISIMP : TISIMP ;                   
                      VAR NP : INTEGER ) ;                                      
VAR I : INTEGER ;                                                               
    V : REAL ;                                                                  
BEGIN                                                                           
  V := 0.3 ;                                                                    
  FOR I:= 1 TO NP DO BEGIN                                                      
    SIMP(.I.) := SIMP(.0.) ;                                                    
    CASE ISIMP (.I.) OF                                                         
      0,2,3  : BEGIN                                                            
                 SIMP(.I,I.) := (1-V)* SIMP(.I,I.) ;                            
               END ;                                                            
      1      : BEGIN                                                            
                 SIMP(.I,I.) := SIMP(.I,I.) + 10.0 ;                            
               END ;                                                            
    END ;                                                                       
  V := -V ;                                                                     
  END ;                                                                         
END ;                                                                           
$SPACE 5                                                                        
PROCEDURE YCALC( VAR SIMP:TRARRNPAR ; VAR NP : INTEGER ;                        
                 VAR ACTB : BERPOINT ; VAR Y,X : REAL ) ;                       
VAR I,K  : INTEGER ;                                                            
    XX0,XSIG,XA : REAL ;                                                        
BEGIN                                                                           
  IF ACTB@.PCFIX THEN BEGIN                                                     
    K := 1 ;                                                                    
    Y := ACTB@.PCONST ;                                                         
  END ELSE BEGIN                                                                
    Y := SIMP(.1.) ;                                                            
    K := 2 ;                                                                    
  END ;                                                                         
  FOR I := 1 TO ACTB@.NCOMP DO BEGIN                                            
    XX0 := SIMP(.K.) ;                                                          
    IF NOT ACTB@.FIX(.I.) THEN BEGIN                                            
      XSIG := SIMP(.K+1.) ;                                                     
      K := K+3;                                                                 
    END ELSE BEGIN                                                              
      XSIG := ACTB@.SIG(.I.) ;                                                  
      K := K+2 ;                                                                
    END ;                                                                       
    XA := SIMP(.K-1.) ;                                                         
    Y := Y + YCURV ( X,XX0,XSIG,XA,ACTB@.GAUSS(.I.)) ;                          
  END ;                                                                         
END ;                                                                           
$EJECT                                                                          
PROCEDURE PCHI ( VAR SIMP : TRARRNPAR ; VAR NP,IBA,IBE : INTEGER ;              
                 VAR ACTB : BERPOINT ; VAR Y:TWERTE ;VAR CHI:REAL) ;            
VAR I,J : INTEGER ;                                                             
    YY  : REAL ;                                                                
BEGIN                                                                           
  CHI := 0 ;                                                                    
  FOR J:=IBA TO IBE DO BEGIN                                                    
    YCALC(SIMP,NP,ACTB,YY,Y(.J,3.)) ;                                           
    CHI := CHI +((Y(.J,1.)-YY)/Y(.J,2.))**2 ;                                   
  END ;                                                                         
END ;                                                                           
$SPACE 5                                                                        
PROCEDURE FINDBEST ( VAR CHI : TCHI ; VAR NP,NW,NB : INTEGER ) ;                
VAR I : INTEGER ;                                                               
    HB,HW : REAL ;                                                              
BEGIN                                                                           
  HW := CHI(.0.) ; HB:= HW ;                                                    
  NW := 0 ; NB := 0 ;                                                           
  FOR I:=0 TO NP DO BEGIN                                                       
    IF HW < CHI(.I.) THEN BEGIN                                                 
      HW := CHI(.I.) ;                                                          
      NW := I ;                                                                 
    END ;                                                                       
    IF HB > CHI(.I.) THEN BEGIN                                                 
      HB := CHI(.I.) ;                                                          
      NB := I ;                                                                 
    END ;                                                                       
  END ;                                                                         
END ;                                                                           
$EJECT                                                                          
PROCEDURE REFLECT ( VAR SIMP : TSIMP ; VAR MSIMP,HELP : TRARRNPAR ;             
                    VAR NP,NWORSE : INTEGER ) ;                                 
VAR I,J : INTEGER ;                                                             
BEGIN                                                                           
  FOR I:= 1 TO NP DO BEGIN                                                      
    HELP(.I.) := 0 ;                                                            
    FOR J:=0 TO NP DO IF J <> NWORSE THEN                                       
                      HELP(.I.) := HELP(.I.) + SIMP(.J,I.) ;                    
    HELP(.I.) := HELP(.I.)/NP -SIMP(.NWORSE,I.);                                
    MSIMP(.I.) :=2.0*  HELP(.I.) + SIMP(.NWORSE,I.) ;                           
  END ;                                                                         
END ;                                                                           
$SPACE 5                                                                        
PROCEDURE EXPAND ( VAR MSIMP,HELP : TRARRNPAR ; NP : INTEGER ) ;                
VAR I : INTEGER ;                                                               
BEGIN                                                                           
  FOR I:= 1 TO NP DO HELP(.I.) :=   MSIMP(.I.) + HELP(.I.) ;                    
END ;                                                                           
$SPACE 5                                                                        
PROCEDURE CONTRACT ( VAR MSIMP,HELP : TRARRNPAR ;VAR NP : INTEGER ) ;           
VAR I : INTEGER ;                                                               
BEGIN                                                                           
  FOR I:=1 TO NP DO MSIMP(.I.):= MSIMP(.I.)-1.5*HELP(.I.) ;                     
END ;                                                                           
$SPACE 5                                                                        
PROCEDURE CONTRCALL ( VAR SIMP : TSIMP ; VAR MSIMP,HELP : TRARRNPAR ;           
                     VAR NP,NBEST : INTEGER ) ;                                 
VAR I,J : INTEGER ;                                                             
BEGIN                                                                           
  FOR I:=0 TO NP DO IF I<>NBEST THEN BEGIN                                      
    FOR J:=1 TO NP DO SIMP (.I,J.) := SIMP(.I,J.) +                             
                                    0.5*(SIMP(.NBEST,J.)-SIMP(.I,J.)) ;         
  END ;                                                                         
END ;                                                                           
$EJECT                                                                          
PROCEDURE WRITEXY ( VAR Y : TWERTE ; VAR NW : INTEGER ) ;                       
VAR I : INTEGER ;                                                               
BEGIN                                                                           
  WRITELN('1AUSGABE DER MESSPUNKTE ') ;                                         
  WRITELN('0') ;                                                                
  FOR I:=1 TO NW DO WRITELN (' X = ',Y(.I,3.):10:3,'     Y = ',Y(.I,1.):10:3,   
                              '     DY = ',Y(.I,2.):12:5 ) ;                    
END ;                                                                           
$SPACE 5                                                                        
PROCEDURE WRITEKOPF( VAR KOPF : TTEXT ; VAR NBER,NIT,NCHI,OP : INTEGER ;        
                     VAR STAT,AQUI,PLOT : BOOLEAN ; SX,SY,X0,DX : REAL ) ;      
BEGIN                                                                           
  WRITELN('1',KOPF ) ;                                                          
  WRITELN('-NIT = ',NIT:5,'   NCHI = ',NCHI:5,'   NBER = ',NBER:5 ) ;           
  WRITELN('-OP  = ',OP :5,'   STAT = ',STAT ) ;                                 
  WRITE('-AQUI= ',AQUI:5);                                                      
  IF AQUI THEN WRITELN ('   X0 = ',X0:10:2,'   DX = ',DX:10:2 )                 
          ELSE WRITELN ;                                                        
  WRITE('-PLOT= ',PLOT:5);                                                      
  IF PLOT THEN WRITELN('   SX = ',SX:10:2,'   SY = ',SY:10:2 )                  
          ELSE WRITELN ;                                                        
END ;                                                                           
$SPACE 5                                                                        
PROCEDURE WRITEBER ( VAR ACTB : BERPOINT ) ;                                    
VAR I : INTEGER ;                                                               
    TEX1,TEX2 : PACKED ARRAY (.1..7.) OF CHAR ;                                 
BEGIN                                                                           
  WRITELN('1AUSGABE DER STARTWERTE FUER AKTUELLEN BEREICH') ;                   
  IF ACTB@.PCFIX THEN TEX1 := 'FIX    ' ELSE TEX1 := '       ' ;                
  WITH ACTB@ DO WRITELN ('-XANF = ',XANF:10:3,'   XEND = ',XEND:10:3,           
               '   NCOMP = ',NCOMP:4,'   PCONST = ',PCONST:10:3,'  ',TEX1 ) ;   
  FOR I :=1 TO ACTB@.NCOMP DO WITH ACTB@ DO BEGIN                               
    IF FIX(.I.) THEN TEX1 := 'FIX    ' ELSE TEX1 := '       ' ;                 
    IF GAUSS(.I.) THEN TEX2 := 'GAUSS  ' ELSE TEX2 := 'LORENTZ' ;               
    WRITELN('0',BEZ(.I.):10,'     X    = ',X0(.I.):10:3,                        
            '    SIG = ',SIG(.I.):10:3,'   A     = ',A(.I.):10:3,               
            '       ',TEX1,'  ',TEX2);                                          
  END ;                                                                         
  WRITELN('-') ;                                                                
END ;                                                                           
$EJECT                                                                          
PROCEDURE WRITEOUT ( VAR SIMP : TSIMP ; VAR CHI : TCHI ; VAR Y : TWERTE ;       
                     VAR IBA,IBE,NP,IIT,NBEST,NWORSE:INTEGER ;                  
                     VAR ACTB : BERPOINT ;FLAG : BOOLEAN ) ;                    
VAR AUX,YC,XCONST,XX0,XSIG,XA : REAL ;                                          
    J,I,K : INTEGER ;                                                           
    TEX1,TEX2 : PACKED ARRAY (.1..7.) OF CHAR ;                                 
BEGIN                                                                           
  IF ACTB@.PCFIX  THEN BEGIN                                                    
    XCONST := ACTB@.PCONST ;                                                    
    K := 1 ;                                                                    
  END ELSE BEGIN                                                                
    K := 2;                                                                     
    XCONST := SIMP(.NBEST,1.) ;                                                 
  END ;                                                                         
  IF FLAG THEN  WRITELN('1AUSGABE DER PARAMETER.',                              
                       '  ANZAHL DURCHGEFUEHRTER ITERATIONEN = ',IIT ) ;        
  IF NOT FLAG THEN WRITELN('0ITERATION NR. ',IIT ) ;                            
  IF FLAG THEN WRITELN('0BEREICH              :  XANF = ',ACTB@.XANF:10:2,      
                       '     XEND = ',ACTB@.XEND:10:2 ) ;                       
  WRITELN('0CHI(BEST) = ',CHI(.NBEST.) : 20:6,',        CHI(WORST) = ',         
          CHI(.NWORSE.):20:6) ;                                                 
  IF FLAG THEN WRITELN('-');                                                    
  IF ACTB@.PCFIX THEN TEX1 := 'FIX    ' ELSE TEX1 := '       ' ;                
  WRITELN('0UNTERGERUND = ',XCONST :10:4,'  ',TEX1 ) ;                          
  FOR I:=1 TO ACTB@.NCOMP DO WITH ACTB@ DO BEGIN                                
    XX0 := SIMP(.NBEST,K.) ;                                                    
    IF FIX(.I.) THEN BEGIN                                                      
      XSIG := SIG(.I.) ;                                                        
      K := K+2 ;                                                                
    END ELSE BEGIN                                                              
      XSIG := SIMP(.NBEST,K+1.) ;                                               
      K := K+3 ;                                                                
    END ;                                                                       
    XA := SIMP(.NBEST,K-1.) ;                                                   
    IF FIX(.I.) THEN TEX1 := 'FIX    ' ELSE TEX1 := '       ' ;                 
    IF GAUSS(.I.) THEN TEX2 := 'GAUSS  ' ELSE TEX2 := 'LORENTZ' ;               
    WRITELN('0',ACTB@.BEZ(.I.):10,'    X = ',XX0:10:3,'   SIG = ',XSIG:10:3,    
            '   A = ',XA:10:3,'    ',TEX1,'  ',TEX2);                           
  END ;                                                                         
  IF FLAG THEN BEGIN                                                            
    WRITELN ('1 AUSGABE DER EINGEGEBENEN UND DER RUECHGERECHNETEN WERTE ') ;    
    WRITELN ('0') ;                                                             
    FOR I := IBA TO IBE DO BEGIN                                                
      YCALC (SIMP(.NBEST.),NP,ACTB,YC,Y(.I,3.) ) ;                              
      AUX := Y(.I,1.)-YC ;                                                      
      WRITELN('0 X = ',Y(.I,3.):15:5,'     Y = ',Y(.I,1.):15:5,'     YCALC = ', 
              YC:15:5,'   Y-YC = ',AUX:15:6,'     DY = ',Y(.I,2.):15:6) ;       
    END ;                                                                       
  END ;                                                                         
  WRITELN('-') ;                                                                
END;                                                                            
$TITLE SIMPLEX :  PLOTSUB : DEFINITIONEN                                        
PROCEDURE PLOTSUB (VAR SIMP:TSIMP ; VAR NP,NW,NBEST,NIT : INTEGER;              
                   VAR ACTB : BERPOINT ;VAR Y : TWERTE  ; VAR KOPF : TTEXT ;    
                   VAR SX,SY : REAL ) ;                                         
TYPE CHARVEK = PACKED ARRAY (.1..5.) OF CHAR ;                                  
     WARRAY  = ARRAY(.1..800.) OF REAL ;                                        
     TEXTT   = TTEXT ;                                                          
     TEXT2  = PACKED ARRAY (.1..20.) OF CHAR ;                                  
VAR LENT,LENXY,INC,IREP1,IREP2,MT,I,IX : INTEGER ;                              
    YN,DDY,XN,XMAX,XX ,AUX   : REAL ;                                           
    TX                 : CHARVEK ;                                              
    XTEX,YTEX          : TEXT2 ;                                                
    X,YW,YC            : WARRAY ;                                               
    J,IACT             : INTEGER ;                                              
    FLAG,TFLAG         : BOOLEAN ;                                              
$SPACE 5                                                                        
   PROCEDURE PTITLE ( VAR  TITLE  : TEXTT ;                (* P T I T L E *)    
                           LEN    : INTEGER ;                                   
                           YN     : REAL   ) ; FORTRAN ;                        
   PROCEDURE SMTYPE (     MTYPE : INTEGER ) ; FORTRAN ;    (* S M T Y P E *)    
   PROCEDURE SMSCAL (     XG    : REAL ) ; FORTRAN    ;    (* S M S C A L *)    
   PROCEDURE SYSCAL (    X1,X2 : REAL ;                    (* S Y S C A L *)    
                         IY : INTEGER ) ; FORTRAN ;                             
   PROCEDURE SXSCAL (    X1,X2 : REAL ;                                         
                         IY : INTEGER ) ; FORTRAN ;        (* S X S C A L *)    
   PROCEDURE NEWPAG ; FORTRAN ;                           (* N E W P A G E *)   
   PROCEDURE DVROT( IROT : INTEGER ) ; FORTRAN ;           (* D V R O T *)      
   PROCEDURE PXYTEXT ( VAR XTEXT,YTEXT : TEXT2 ;           (* P X Y T E X T *)  
                           LEN         : INTEGER ) ;                            
      PROCEDURE PXTEXT( VAR XTEXT : TEXT2 ;                                     
                            LEN   : INTEGER ) ; FORTRAN ;                       
      PROCEDURE PYTEXT( VAR YTEXT : TEXT2 ;                                     
                            LEN   : INTEGER ) ; FORTRAN ;                       
   BEGIN                                                                        
      PXTEXT(XTEXT,LEN) ;                                                       
      PYTEXT(YTEXT,LEN) ;                                                       
   END ;                                                                        
   PROCEDURE PDATA ( VAR X  : WARRAY ;                     (* P D A T A *)      
                         IX : INTEGER ;                                         
                     VAR Y  : WARRAY ;                                          
                         IY,NANZ,IREP : INTEGER ) ; FORTRAN ;                   
   PROCEDURE PTEXT ( TEXT : CHARVEK ;                      (* P T E X T *)      
                     LEN  : INTEGER ;                                           
                     XN,YN: REAL   ) ; FORTRAN ;                                
   PROCEDURE PNUMB ( VAL    : REAL ;                       (* P N U M B *)      
                    LEN,ND : INTEGER ;                                          
                    XN,YN  : REAL ) ; FORTRAN ;                                 
   PROCEDURE USCALE( ITR         : INTEGER ;           (* U S C A L E *)        
                     C1,C2,C3,C4 : REAL ) ; FORTRAN ;                           
   PROCEDURE SCSCAL ( SC : REAL ) ; FORTRAN ;              (* S C S C A L *)    
   PROCEDURE DPSIZE ( FORM, DINA : REAL ) ; FORTRAN ;      (* D P S I Z E *)    
   PROCEDURE DPBORD ( I : INTEGER ) ; FORTRAN ;            (* D P B O R D *)    
BEGIN                                                                           
  TFLAG := TRUE ;                                                               
  DPBORD (0) ;                                                                  
  SX := ABS(SX) ;                                                               
  SY := ABS (SY) ;                                                              
  IF SX < 6 THEN SX := 6 ;                                                      
  IF SY > 45 THEN SY := 45 ;                                                    
  IF SY < 6 THEN SY := 6 ;                                                      
  IF SX > 85  THEN SX := 85  ;                                                  
  DPSIZE(SX,SY) ;                                                               
  AUX := ACTB@.YMAX * 1.1 ;                                                     
  SMTYPE(5)  ;                                                                  
  SMSCAL(0.2) ;                                                                 
  XTEX := 'FREQUENZ (MHZ)      ' ;                                              
  YTEX := 'ZAEHLRATE           ' ;                                              
  IACT := IBA ;                                                                 
  FLAG := TRUE ;                                                                
  LOOP                                                                          
    I := 1 ;                                                                    
    REPEAT                                                                      
      X(.I.) := Y(.IACT,3.)    ;                                                
      YW(.I.) := Y(.IACT,1.) ;                                                  
      YCALC(SIMP(.NBEST.),NP,ACTB,YC(.I.),Y(.IACT,3.)) ;                        
      IACT := IACT + 1 ;                                                        
      I := I+ 1 ;                                                               
    UNTIL ( I > 800) OR ( IACT > IBE)  ;                                        
    I := I-1 ;                                                                  
    IF FLAG THEN XX := X(.I.) - X (.1.) ;                                       
    FLAG := FALSE ;                                                             
    SXSCAL (X(.1.),X(.1.)+XX,0) ;                                               
    SYSCAL ( 0.0 , AUX, 0 ) ;                                                   
    IF NIT > 0 THEN BEGIN                                                       
      PDATA (X,2,YW,2,I,0) ;                                                    
      PDATA (X,2,YC,2,I,4) ;                                                    
    END ELSE BEGIN                                                              
      PDATA (X,2,YW,2,I,0) ;                                                    
    END ;                                                                       
    SCSCAL (0.7) ;                                                              
    PXYTEXT ( XTEX,YTEX,20) ;                                                   
    SCSCAL (0.5) ;                                                              
    PTITLE ( KOPF ,80, 1.10) ;                                                  
    NEWPAG ;                                                                    
    IF IACT >= IBE THEN EXIT ;                                                  
  END ;                                                                         
END ;                                                                           
$SPACE 2                                                                        
   PROCEDURE ENDPLT ; FORTRAN ;                            (* E N D P L T *)    
                                                                                
FUNCTION NFLAG (B : BOOLEAN) : INTEGER ;                                        
BEGIN                                                                           
  IF B THEN NFLAG := 1 ELSE NFLAG := 0 ;                                        
END ;                                                                           
                                                                                
PROCEDURE DATA ( VAR SIMP :TRARRNPAR ; VAR ACTB : BERPOINT ) ;                  
VAR I,K : INTEGER ;                                                             
BEGIN                                                                           
  WITH ACTB@ DO BEGIN                                                           
    IF PCFIX THEN BEGIN                                                         
      K := 1 ;                                                                  
    END ELSE BEGIN                                                              
      PCONST := SIMP(.1.) ;                                                     
      K := 2 ;                                                                  
    END ;                                                                       
    FOR I := 1 TO NCOMP DO BEGIN                                                
      X0(.I.) := SIMP(.K.) ;                                                    
      IF NOT FIX(.I.) THEN BEGIN                                                
        SIG(.I.) := SIMP(.K+1.) ;                                               
        K := K+3 ;                                                              
      END ELSE K := K + 2 ;                                                     
      A(.I.) := SIMP(.K-1.) ;                                                   
    END ;                                                                       
  END ;                                                                         
END ;                                                                           
(* ************************************************************************ *)  
(*                                                                          *)  
(*                      H A U P T P R O G R A M M                           *)  
(*                                                                          *)  
(* ************************************************************************ *)  
BEGIN                                                                           
  READDATA(KOPF,NBER,NIT,NCHI,OP,STAT,AQUI,PLOT,SX,SY,X0,DX,KOPFBER,Y,NW);      
  WRITEKOPF(KOPF,NBER,NIT,NCHI,OP,STAT,AQUI,PLOT,SX,SY,X0,DX) ;                 
  NNIT := NIT ;                                                                 
  IF NCHI < 1 THEN BEGIN                                                        
    NCHI := 50 ;                                                                
    WRITELN('- <*** NCHI <1 :   WIRD AUF NCHI = 50 FESTGESETZT ***>') ;         
  END ;                                                                         
  IF OP = 1 THEN WRITEXY(Y,NW) ;                                                
  ACTB := KOPFBER ;                                                             
  LOOP                                                                          
    FLAG := TRUE ;                                                              
    WRITEBER(ACTB) ;                                                            
    FINDUP( ACTB,SIMP(.0.),ISIMP,NP,NPAR,FLAG) ;                                
    IF NNIT < 0 THEN BEGIN                                                      
      NIT := 100 * NP ;                                                         
      WRITELN ('-<*** NIT <0 :   WIRD AUF NIT = ',NIT,                          
               '   PROGRAMMINTERN FESTGESETZT ***>');                           
    END ;                                                                       
    FINDIB(Y,IBA,IBE,NW,ACTB) ;                                                 
    IF (IBE-IBA) < NP THEN BEGIN                                                
      ERROR ;                                                                   
      WRITELN('ANZAHL DER FREIHEITSGRADE <= 1') ;                               
      WRITELN('   A B B R U C H ') ;                                            
      FLAG := FALSE ;                                                           
    END ;                                                                       
    INCHI := NCHI ;                                                             
    IF FLAG THEN BEGIN                                                          
      CREATSIMP(SIMP,ISIMP,NP) ;                                                
      FOR I:=0 TO NP DO PCHI(SIMP(.I.),NP,IBA,IBE,ACTB,Y,CHI(.I.)) ;            
      FINDBEST(CHI,NP,NWORSE,NBEST) ;                                           
      IIT := 0 ;                                                                
      IF NIT >0 THEN BEGIN                                                      
      LOOP                                                                      
        IF INCHI > NCHI THEN BEGIN                                              
          INCHI := 1 ;                                                          
          WRITEOUT (SIMP,CHI,Y,IBA,IBE,NP,IIT,NBEST,NWORSE,ACTB,FALSE ) ;       
        END ;                                                                   
        LOOP UNTIL NEXIT,SEXIT :                                                
          REFLECT(SIMP,MSIMP,HELP,NP,NWORSE) ;                                  
          PCHI(MSIMP,NP,IBA,IBE,ACTB,Y,MCHI) ;                                  
          IF MCHI <= CHI(.NBEST.) THEN BEGIN                                    
            EXPAND(MSIMP,HELP,NP) ;                                             
            PCHI(HELP,NP,IBA,IBE,ACTB,Y,HCHI) ;                                 
            IF HCHI <= CHI(.NBEST.) THEN BEGIN                                  
              IF HCHI < MCHI THEN BEGIN                                         
                MSIMP := HELP ;                                                 
                MCHI := HCHI ;                                                  
              END ;                                                             
              SEXIT ;                                                           
            END ELSE SEXIT ;                                                    
          END ELSE BEGIN                                                        
            IF MCHI < CHI(.NWORSE.) THEN SEXIT ;                                
            CONTRACT(MSIMP,HELP,NP) ;                                           
            PCHI(MSIMP,NP,IBA,IBE,ACTB,Y,MCHI) ;                                
            IF MCHI <= CHI(.NWORSE.) THEN SEXIT ;                               
            CONTRCALL(SIMP,MSIMP,HELP,NP,NBEST) ;                               
            NEXIT ;                                                             
          END ;                                                                 
        POSTLUDE                                                                
          SEXIT : BEGIN                                                         
                    SIMP(.NWORSE.) := MSIMP ;                                   
                    CHI(.NWORSE.) := MCHI ;                                     
                  END ;                                                         
          NEXIT : BEGIN                                                         
                    FOR I:= 0 TO NP DO PCHI ( SIMP(.I.),NP,IBA,IBE,ACTB,Y,      
                                              CHI(.I.)) ;                       
                  END                                                           
        END ;                                                                   
        IF IIT > NIT THEN EXIT ;                                                
        IIT := IIT +1 ;                                                         
        INCHI := INCHI + 1 ;                                                    
        FINDBEST(CHI,NP,NWORSE,NBEST) ;                                         
        IF (CHI(.NWORSE.)-CHI(.NBEST.)) <= (1E-8 * CHI(.NBEST.) + 1E-6)         
                                     THEN EXIT ;                                
      END ;                                                                     
      END ;                                                                     
      FINDBEST (CHI,NP,NWORSE,NBEST) ;                                          
      ACTB@.BEST := NBEST ;                                                     
      DATA(SIMP(.NBEST.),ACTB) ;                                                
      WRITEOUT(SIMP,CHI,Y,IBA,IBE,NP,IIT,NBEST,NWORSE,ACTB,TRUE) ;              
      IF  PLOT THEN PLOTSUB(SIMP,NP,NW,NBEST,NIT,ACTB,Y,KOPF,SX,SY) ;           
    END ;                                                                       
    IF ACTB@.NEXTB = NIL THEN EXIT ;                                            
    ACTB := ACTB@.NEXTB ;                                                       
  END ;                                                                         
1000 : IF PLOT THEN ENDPLT ;                                                    
  ACTB := KOPFBER ;                                                             
  WRITELN('1',KOPF:80) ;                                                        
  WRITELN(' ', NBER:4,NFLAG(STAT):4,NIT:8,NCHI:4,OP:4,NFLAG(AQUI):4,            
           NFLAG(PLOT):4,' ',SX:7:1,' ',SY:7:1);                                
  IF AQUI THEN WRITELN(' ',X0:20:2,' ',DX:19:2) ;                               
  REPEAT                                                                        
    WITH ACTB@ DO BEGIN                                                         
      WRITELN(' ',XANF:8:1,' ',XEND:9:1,' ',PCONST:9:2,                         
              NFLAG(PCFIX):4,NCOMP:4);                                          
      FOR I := 1 TO NCOMP DO                                                    
        WRITELN(' ',X0(.I.):20:3,SIG(.I.):20:3,A(.I.):20:3,                     
               NFLAG(GAUSS(.I.)):4,NFLAG(FIX(.I.)):2,' ',BEZ(.I.):10) ;         
    END ;                                                                       
    ACTB := ACTB@.NEXTB ;                                                       
  UNTIL ACTB = NIL ;                                                            
END .                                                                           
                                                     