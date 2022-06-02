PROGRAM SPEKTRUM(INPUT,OUTPUT,INDATA) ;
(* ********************************************************************)
(* PROGRAMM ZUR  BERECHNUNG DER ERLAUBTEN DIPOLUEBERGAENGE. DIE HIERZU*)
(* BENUTZTEN AUSWAHLREGELN LAUTEN :                                   *)
(*                                                                    *)
(*                     DJ <= 1 AUSSER J=0 -> J=0                      *)
(*                                                                    *)
(* ES GELTEN FOLGENDE EINGABEFORMATE FUER DIE DATENKARTEN             *)
(*                                                                    *)
(*  SPALTE       FORMAT      VARIABLE        BEDEUTUNG                *)
(*                                                                    *)
(*  1 - 10         F          LMIN           MINIMUM DER BER. WELLENL *)
(* 11 - 20         F          LMAX           MAXIMUM        "         *)
(*                 F          ELIM           ENERGIE LIMIT FUER ANREGU*)
(* 21 - 24         I                                                  *)
(* 26 - 80         A          KOPF           UEBERSCHRIFTEN TEXT      *)
(*                                                                    *)
(*  1 - 20         F          E             ENERGIE EINES LEVELS(CM-1)*)
(* 21 - 36         F          JJ             HUELLENDREHIMPULS        *)
(* 37 - 40         I          P              PARITAET DES ZUSTANDES   *)
(* 42 - 50         A          CIN            BEZEICHNUNG DES ZUSTANDES*)
(*                                                                    *)
(* WEITERE KARTEN DES TYPES 2 .                                       *)
(* DIE PARITAETEN DER ZUSTAENDE KOENNEN INNERHALB DER EINGABEDATEN BEL*)
(* BELIEBIG GEMISCHT WERDEN                                           *)
(*                                                                    *)
(* ********************************************************************)
 
(* -------------------------------------------------------------------*)
(*                                                                    *)
(*               DEKLARATIONSPART DES HAUPTPROGRAMMES                 *)
(*                                                                    *)
(* -------------------------------------------------------------------*)
 
CONST MAX    = 1000 ;
     CMAX    = 10 ;
     INDAMAX = 1200 ;
 
TYPE WARRAY    = ARRAY(.1..MAX.) OF REAL ;
     CARRAY    = PACKED ARRAY (.1..MAX,1..CMAX.) OF CHAR ;
     KOPFTYPE  = PACKED ARRAY (.1..55.) OF CHAR ;
     TINT      = ARRAY(.1..4,1..INDAMAX.) OF REAL;
     TCOM      = ARRAY(.1..INDAMAX.) OF CHAR ;
 
VAR WODD,JODD,WEVEN,JEVEN : WARRAY         ;
    CODD,CEVEN            : CARRAY         ;
    KOPF                  : KOPFTYPE       ;
    LMIN,LMAX,ELIM        : REAL           ;
    P,I,IEVEN,IODD,NT     : INTEGER        ;
    E,JJ                  : REAL           ;
    CIN                   : PACKED ARRAY (.1..CMAX.) OF CHAR  ;
    INDATA                : TEXT ;
    INT                   : TINT ;
    COM                   : TCOM ;
 
(* -------------------------------------------------------------------*)
(*         UNTERPROGRAMM ZUR BERECHNUNG DES BRECHUNGSINDEX            *)
(* -------------------------------------------------------------------*)
 
FUNCTION BINDEX ( VAR L : REAL  ) : REAL ;
VAR H,H2 : REAL ;
BEGIN
  H := L/10000 ;
  H2 := 1/( H * H) ;
  BINDEX := 1 + (272.643+1.2288*H2+0.03555*H2*H2)*1E-6 ;
END ;
 
 
 
(* -------------------------------------------------------------------*)
(* UNTERPROGRAMM ZUR BERECHNUNG DER WELLENLAENGE                      *)
(* -------------------------------------------------------------------*)
 
FUNCTION LAMBDA ( VAR E1,E2 : REAL ) : REAL ;
 
VAR AUX : REAL ;
BEGIN
  AUX := 1E8 / ABS(E2-E1) ;
  LAMBDA := AUX / BINDEX(AUX) ;
END ;
 
 
(* -------------------------------------------------------------------*)
(* UNTERPROGRAMM ZUR BESTIMMUNG VON ERLAUBTEN UND NICHT ERLAUBTEN UEBE*)
(* GAENGEN                                                            *)
(* -------------------------------------------------------------------*)
 
FUNCTION AUSWAHL( VAR J,JJ : REAL ) : BOOLEAN ;
VAR AUX : BOOLEAN ;
    J1,J2: INTEGER ;
BEGIN
  J1 := ROUND(J) ;
  J2 := ROUND(JJ) ;
  AUX := FALSE;
  IF ABS ( J1 - J2 ) <= 1 THEN
  BEGIN
    AUX := TRUE ;
    IF ((J1=J2) AND ( J1=0)) THEN AUX := FALSE ;
  END ;
AUSWAHL := AUX ;
END ;
(*  PROCEDURE FINDL *)
PROCEDURE FINDL ( VAR L     : REAL ;
                  VAR INT   : TINT ;
                  VAR NT,KK,NL : INTEGER ) ;
VAR I : INTEGER ;
BEGIN
  NL := 0 ;
  IF NT > 0 THEN BEGIN
    I := 1 ;
    REPEAT
      IF ( ABS(L-INT(.2,I.)) < 0.2 ) THEN NL := I ;
      I := I+1 ;
    UNTIL ((NL <> 0) OR ( I > NT)) ;
    IF NL <> 0 THEN BEGIN
      KK := -1;
      REPEAT
        KK := KK+1 ;
      UNTIL (( NL+KK > NT-1 ) OR ((INT(.2,NL+KK.)-L) > 0.2 )) ;
      KK := KK-1 ;
    END ;
  END ;
END ;
 
(* -------------------------------------------------------------------*)
(*                AUSGABE DER BERECHNETEN UEBERGAENGE                 *)
(* -------------------------------------------------------------------*)
 
PROCEDURE OUT ( VAR OPT,PAGE , N,K,PN : INTEGER ;
                    L            : REAL    ;
                    W0    : WARRAY         ;
                    C0    : CARRAY         ;
                    W1    : WARRAY         ;
                    C1    : CARRAY         ;
                    KOPF  : KOPFTYPE       ;
                    INT   : TINT           ;
                    COM   : TCOM           ;
                    NT    : INTEGER      ) ;
VAR I,NL,KK : INTEGER ;
BEGIN
  IF PN > 52 THEN
  BEGIN
    PN :=1 ;
    PAGE := PAGE +1 ;
  END ;
  IF PN = 1 THEN
  BEGIN
    PN := PN + 1;
    WRITELN('1       ',KOPF,'                      SEITE ',PAGE:3) ;
    WRITELN ;
    WRITELN ;
    WRITELN(' ZUSTAND    E(CM-1)       L(A)          ',
            ' ZUSTAND    E(CM-1)       L(A)      INT ',
            ' ZUSTAND    E(CM-1)');
  END ;
  IF OPT = 0 THEN  BEGIN
    PN := PN+3 ;
    WRITELN ;
    WRITELN ;
    WRITELN(' ',C0(.N.):10,W0(.N.):11:3,L:11:3,'       ',C1(.K.):10,
            W1(.K.):11:3) ;
  END ;
                                    (* OPT = 1 BEI FLUORESZENZW *)
  IF OPT = 1 THEN BEGIN
    FINDL (L,INT,NT,KK,NL) ;
    PN := PN+1 ;
    IF NL = 0 THEN
       WRITELN ('                               ',
               '                               '
               ,L:11:3,'       ', C0(.K.):10, W0(.K.):11:3 ) ;
    IF NL <> 0 THEN BEGIN
       WRITELN ('                               ',
               '                               ',
               L:11:3,INT(.1,NL.):6:1,COM(.NL.):1, C0(.K.):10,
                W0(.K.):11:3,'  (',INT(.3,NL.):8:1,'-',
                INT(.4,NL.):8:1,') ',INT(.2,NL.) :8:2);
       IF KK > 0 THEN FOR I := 1 TO KK DO BEGIN
         NL := NL+1 ;
         PN := PN+1 ;
         WRITELN ('                               ',
                  '                               ',
                  ' ':11,INT(.1,NL.):6:1,COM(.NL.):1,' ':10,
                  ' ':11,'  (',INT(.3,NL.):8:1,'-',
                  INT(.4,NL.):8:1,') ',INT(.2,NL.) :8:2);
      END ;
    END ;
  END ;
END ;
 
(* ------------------------------------------------------------------*)
(*                    BERECHNUNG DER UEBERGAENGE                     *)
(* ------------------------------------------------------------------*)
 
PROCEDURE COUNT ( VAR W0,J0 : WARRAY  ;
                      C0    : CARRAY  ;
                      I0    : INTEGER ;
                      W1,J1 : WARRAY  ;
                      C1    : CARRAY  ;
                      I1    : INTEGER ;
                      LMIN,LMAX,ELIM:REAL ;
                      KOPF  : KOPFTYPE;
                      NT    : INTEGER ;
                      INT   : TINT    ;
                      COM   : TCOM ) ;
VAR PN,I,J,OPT,K,II,JJ,KK,PAGE,NL : INTEGER ;
    L                          : REAL    ;
BEGIN
  PAGE := 0 ;
  FOR II := 1 TO I0 DO
    IF W0(.II.)<= ELIM THEN
  BEGIN
    I := II ;
    PN := 70 ;
    FOR JJ := 1 TO I1 DO
    BEGIN
      J := JJ ;
      OPT := 0 ;
      IF W0(.I.) < W1(.J.) THEN IF AUSWAHL(J0(.I.),J1(.J.)) THEN
      BEGIN
        L := LAMBDA ( W0(.I.),W1(.J.)) ;
        IF((L>LMIN) AND (L<LMAX)) THEN
        BEGIN
          OUT(OPT,PAGE,I,J,PN,L,W0,C0,W1,C1,KOPF,INT,COM,NT) ;
          OPT :=1 ;
          FOR KK :=1 TO I0 DO
          BEGIN
            K := KK ;
            IF ( AUSWAHL(J1(.J.),J0(.K.)) AND ( W0(.K.) < W1(.J.)))
            THEN BEGIN
              L := LAMBDA ( W1(.J.),W0(.K.)) ;
              IF ((L>0) AND (L < 8000.0)) THEN BEGIN
                OUT(OPT,PAGE,J,K,PN,L,W0,C0,W1,C1,KOPF,INT,COM,NT) ;
              END ;
            END ;
          END ;
        END ;
      END ;
    END ;
  END ;
END ;
 
(* ****************************************************************** *)
(*                                                                    *)
(*                       H A U P T P R O G R A M M                    *)
(*                                                                    *)
(*                            EINGABEROUTINE                          *)
(*                                                                    *)
(* ****************************************************************** *)
 
BEGIN
  RESET ( INDATA) ;
  NT := 1 ;
  WHILE NOT EOF(INDATA) DO BEGIN
      READLN (INDATA,INT(.1,NT.),COM(.NT.),INT(.2,NT.),INT(.3,NT.),
              INT(.4,NT.) ) ;
      NT := NT+1 ;
  END ;
  NT := NT-1 ;
  READLN(LMIN,LMAX,ELIM,I,KOPF) ;
  IODD :=1 ;
  IEVEN := 1 ;
  WRITELN('1',LMIN:10:2,LMAX:10:2,ELIM:10:4,I:4,KOPF) ;
  WRITELN ;
  WHILE NOT EOF(INPUT) DO BEGIN
    READLN ( E,JJ,P,CIN) ;
    WRITELN(' ',E:20:3,JJ:16:1,P:4,CIN) ;
       CASE P OF
     1    : BEGIN
              WODD(.IODD.) := E   ;
              JODD(.IODD.) := JJ  ;
              CODD(.IODD.) := CIN ;
              IODD := IODD + 1    ;
            END ;
 
     0    : BEGIN
              WEVEN(.IEVEN.) := E   ;
              JEVEN(.IEVEN.) := JJ  ;
              CEVEN(.IEVEN.) := CIN ;
              IEVEN := IEVEN +1     ;
            END ;
 
     OTHERWISE WRITELN(' PARITAET IST NICHT BESTIMMT') ;
    END ;
  END ;
  IODD := IODD -1 ;
  IEVEN := IEVEN -1 ;
  IF WODD(.1.) < WEVEN(.1.) THEN
     COUNT(WODD,JODD,CODD,IODD,WEVEN,JEVEN,CEVEN,IEVEN,LMIN,LMAX,ELIM,
           KOPF,NT,INT,COM) ;
     IF WODD(.1.) >= WEVEN(.1.) THEN
     COUNT(WEVEN,JEVEN,CEVEN,IEVEN,WODD,JODD,CODD,IODD,LMIN,LMAX,ELIM,
           KOPF,NT,INT,COM) ;
END .
                                       