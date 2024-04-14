PROGRAM HFSFIT ( INPUT,OUTPUT ) ;
(* ***************************************************************** *)
(*                                                                   *)
(*     PROGRAMM : HFSFIT                                             *)
(*                                                                   *)
(*     DIESES PROGRAMM BERECHNET AUS EINGEGEBENEN MESSWERTEN         *)
(*     HFS-PARAMETER . DIE PARAMETER TRAGEN DABEI FOLGENDE           *)
(*     INTERNE NUMMERIERUNG  :                                       *)
(*                                                                   *)
(*         PARAMETER-NR.          PARAMETER-BEZ.                     *)
(*                                                                   *)
(*               1             S1       SCHWERPUNKT DES 1.TEN ISOT.  *)
(*               2             A(OBEN)  A-FAKTOR          "          *)
(*               3             A(UNTEN) A-FAKTOR          "          *)
(*               4             B(OBEN)  B-FAKTOR          "          *)
(*               5             B(UNTEN) B-FAKTOR          "          *)
(*                                                                   *)
(*               6             S1       SCHWERPUNKT DES 2.TEN ISOT.  *)
(*               7             A(OBEN)  A-FAKTOR          "          *)
(*               8             A(UNTEN) A-FAKTOR          "          *)
(*               9             B(OBEN)  B-FAKTOR          "          *)
(*              10             B(UNTEN) B-FAKTOR          "          *)
(*                                                                   *)
(*                                                                   *)
(*     DABEI KOENNEN EINZELNE PARAMETER RELATIV ZU EINEM ANDEREN     *)
(*     FESTGEHALTEN WERDEN, ODER AUCH ABSOLUT AUF EINEN BESTIMMTEN   *)
(*     WERT GEHALTEN WERDEN.                                         *)
(*                                                                   *)
(*       WENN ALLE HFS-KONSTANTEN FESTGEHALTEN WERDEN, WIRD KEIN FIT *)
(*    DURCHGEFUEHRT. ES WERDEN STATTDESSEN DIE FREQUENZEN UND REL.   *)
(*    INTENSITAETEN DER EINZELNEN KOMPONENTEN BERECHNET UND BUCH-    *)
(*    STABEN BEZEICHNUNGEN FUER DIE KOMPONENTEN ERMITTELT.           *)
(*                                                                   *)
(*    BESCHR?NKUNGEN DES PROGRAMMES:                                 *)
(*                                                                   *)
(*           MAXIMALE ANZAHL VON MESSWERTEN = 100                    *)
(*                                                                   *)
(*           ES IST AUCH NICHT ERLAUBT EINEN PARAMETER AUF EINEN     *)
(*           PARAMTER FESTZUHALTEN, DER SELBST DURCH EIN VERH?LTNIS  *)
(*           BESTIMMT IST. NAT?RLICH IST ES AUCH NICHT ERLAUBT       *)
(*           SCHLEIFEN ZU BILDEN .                                   *)
(*                                                                   *)
(*                                                                   *)
(*    EINGABE - DATEN   :                                            *)
(*                                                                   *)
(*    ZEILE     SPALTE    FORMAT     NAME      BEZEICHNUNG           *)
(*                                                                   *)
(*     0                    C80      KOPF       BERSCHRIFT           *)
(*                                                                   *)
(*                                                                   *)
(*                                                                   *)
(*     1                    I       NISOTOP    ANZAHL ISOTOPE        *)
(*     1                    I       FIXED_PAR  ANZAHL FESTGEHALTENER *)
(*                                             PARAMETER             *)
(*     1                    R         XJO      J DES OBEREN ZUSTANDES*)
(*     1                    R         XJU      J DES UNTEREN ZUST.   *)
(*     1                    R         XI1      I KERNSPIN 1.TE ISO   *)
(*     1                    R         XI2      I KERNSPIN 2.TE ISO.  *)
(*                                             DIESER WERT MUSS AUCH *)
(*                                             EINGEGEBEN WERDEN     *)
(*                                             WENN NISOTOP = 1 IST  *)
(*                                                                   *)
(*                                                                   *)
(*  WENN PARAMETER FESTGEHALTEN WERDEN FOLGT FUER JEDEN DER          *)
(*  'FIXED_PAR' PARAMETER EIN ZEILE VOM TYP 2.                       *)
(*                                                                   *)
(*     2                    I       NFP(.I.)      NR. DES FESTGEH.   *)
(*                                              PARAMETERS           *)
(*     2                    R       VALFP(.I.)  WERT ODER VERHAELTNIS*)
(*                                              DES FESTEN PAR.      *)
(*     2                    I       REFP(.I.)     BEZUGS-PARAMETER   *)
(*                                              IST REFP(.I.) = 0 SO *)
(*                                              WIRD DER PARAMETER   *)
(*                                            AUF DEN WERT VALFP(.I.)*)
(*                                              FESTGEHALTEN         *)
(*                                                                   *)
(*   PARAMETER DUERFEN AUCH UEBER EIN VERHAELTNIS AUF EINEN ABSOLUT  *)
(*   FESTGEHALTENEN PARAMETER BEZOGEN WERDEN. NICHT ERLAUBT IST      *)
(*   JEDOCH DER BEZUG AUF EINEN 'RELATIV' FESTGEHALTENEN PARAMETER.  *)
(*                                                                   *)
(*   ES FOLGEN DIE ZEILEN MIT DEN MESSWERTEN                         *)
(*                                                                   *)
(*     3        1           C1    ISOTOP(.I.) 2 =  DATUM GEHOERT ZUM *)
(*                                               2.TEN ISOTOP.       *)
(*                                               ZUM 1.TEN ISOTOP .  *)
(*                                            BELIEBIG = DATUM GE-   *)
(*                                               HOERT ZUM 1.TEN     *)
(*                                               ISOTOP              *)
(*                                                                   *)
(*     3       2-5          C4    XNAME(.I.)   BEZEICHNUNG DES I.TEN *)
(*                                             EINGABE WERTES        *)
(*     3        6           C1    LIMIT        WIRD HIER EIN CHAR.   *)
(*                                             UNGLEICH BLANK EINGE- *)
(*                                             TRAGEN SO WIRD DIES   *)
(*                                             ALS ENDE DES DATEN-   *)
(*                                             SATZES VERSTANDEN     *)
(*     3                    R       FO(.I.)     F-QUANTENZAHL        *)
(*     3                    R       FU(.I.)     F-QUANTENZAHL        *)
(*     3                    R       Y(.I.)      MESSWERT             *)
(*     3                    R       DY(.I.)     FEHLER D. MESSWERTES *)
(*                                                                   *)
(*                                                                   *)
(*                                                                   *)
(*                                                                   *)
(* ***************************************************************** *)
 
CONST PMAX   = 14 ;         (* MAXIMAL ZAHL VON PARAMETERN + 4 *)
      WMAX   = 100 ;        (* MAXIMAL ZAHL VON WERTEPAAREN *)
 
TYPE REAL8 = REAL ;
     TYPE_QMATRIX = ARRAY (.1..PMAX,1..PMAX.) OF REAL8 ;
     TYPE1_MATRIX = TYPE_QMATRIX  ;
     TYPE2_MATRIX = ARRAY (.1..WMAX,1..PMAX.) OF REAL8 ;
     TYPE3_MATRIX = TYPE2_MATRIX  ;
     TYPEX_VEKTOR = ARRAY (.1..WMAX.) OF REAL8 ;
     TYPEXN_VEKTOR = ARRAY (.1..WMAX.) OF PACKED ARRAY(.1..4.) OF CHAR ;
     TYPEISO_VEKTOR = ARRAY (.1..WMAX.) OF CHAR ;
     TYPEP_VEKTOR = ARRAY (.1..PMAX.) OF REAL8 ;
     TYPEPINT_VEKTOR = ARRAY (.1..PMAX.) OF INTEGER ;
     TYPE_KOPF    = PACKED ARRAY (.1..8.) OF CHAR ;
     KOMMENTAR    = PACKED ARRAY(.1..70.) OF CHAR ;
     TYPE_PNAME   = ARRAY(.-1..PMAX.) OF PACKED ARRAY(.1..10.) OF CHAR ;
 
VAR CA                                        : TYPE_QMATRIX ;
    DER,DER2                                  : TYPE2_MATRIX ;
    FO,FU,Y,DY,YY                             : TYPEX_VEKTOR ;
    XNAME                                     : TYPEXN_VEKTOR ;
    PNAME                                     : TYPE_PNAME ;
    NFP,REFP,SORT                             : TYPEPINT_VEKTOR ;
    ISOTOP                                    : TYPEISO_VEKTOR ;
    P,PP,VALFP                                : TYPEP_VEKTOR ;
    COMMENT                                   : KOMMENTAR ;
    I,J,K,NIT,R,RR,N,OPT,IER                  : INTEGER ;
    D,CHI,CHIO,XJO,XJU,XI1,XI2                : REAL8 ;
    KOPF                                      : TYPE_KOPF ;
    CC                                        : CHAR ;
    GAUSS,FLAG                                : BOOLEAN ;
    FIXED_PAR,NISOTOP                         : INTEGER ;
 
FUNCTION FAKULT ( N : INTEGER ) : REAL8 ;        (* FAKULT     *)
BEGIN
  IF N IN (.0..33.) THEN BEGIN
    IF N = 0 THEN FAKULT := 1
             ELSE FAKULT := N * FAKULT (N-1) ;
  END ELSE BEGIN
    FAKULT := 0 ;
    WRITELN('0ARGUMENT VON FAKULT?T > ALS 33 , KLEINER NULL ODER ',
                'NICHT GANZZAHLIG ') ;
  END ;
END ;                                            (* END FAKULT *)
 
PROCEDURE SECHSJ(     A1,A2,A3,B1,B2,B3  : REAL8 ; (* SECHSJ *)
                 VAR  RES                : REAL8 ;
                 VAR  IER                : INTEGER ) ;
 
VAR J1,J2,J3,L1,L2,L3                               : INTEGER ;
    T1,T2,T3,T4,T5,T6                               : REAL8 ;
    M11,M12,M13,M21,M22,M23,M31,M32,M33,M41,M42,M43 : INTEGER ;
    M51,M52,M53,M54,M61,M62,M63                     : INTEGER ;
    D1,D2,D3,D4,D,W1,W2,W3                          : REAL8 ;
    KA,KE,L,I,VOR                                   : INTEGER ;
 
   FUNCTION DD ( M1,M2,M3,M4 : INTEGER ) : REAL8 ;
   BEGIN
      DD := SQRT( FAKULT(M1) * FAKULT(M2) * FAKULT(M3) / FAKULT(M4+1) )
   END ;
 
BEGIN
  RES := 0.0 ;
  J1 :=  ROUND(2.0 * A1 ) ;       (* QUANTENZAHLEN -> INTEGER *)
  J2 :=  ROUND(2.0 * A2 ) ;
  J3 :=  ROUND(2.0 * A3 ) ;
  L1 :=  ROUND(2.0 * B1 ) ;
  L2 :=  ROUND(2.0 * B2 ) ;
  L3 :=  ROUND(2.0 * B3 ) ;
  T1 := J1 ;                      (* TEST AUF HALBZAHLIGKEIT *)
  T2 := J2 ;
  T3 := J3 ;
  T4 := L1 ;
  T5 := L2 ;
  T6 := L3 ;
  T1 := ABS(2.0 * A1 - T1 ) ;
  T2 := ABS(2.0 * A2 - T2 ) ;
  T3 := ABS(2.0 * A3 - T3 ) ;
  T4 := ABS(2.0 * B1 - T4 ) ;
  T5 := ABS(2.0 * B2 - T5 ) ;
  T6 := ABS(2.0 * B3 - T6 ) ;
  IF (T1+T2+T3+T4+T5+T6) > 0.001 THEN BEGIN
     IER := 2  ;
     WRITELN('0QUANTENZAHL NICHT HALB- ODER GANZZAHLIG') ;
  END ELSE BEGIN                       (* BERECHNUNG DES 6-J SYMBOLS *)
     M11 := (J1+J2-J3) DIV 2 ;
     M12 := (J1+J3-J2) DIV 2 ;
     M13 := (J2+J3-J1) DIV 2 ;
     M21 := (J1+L2-L3) DIV 2 ;
     M22 := (J1+L3-L2) DIV 2 ;
     M23 := (L2+L3-J1) DIV 2 ;
     M31 := (L1+J2-L3) DIV 2 ;
     M32 := (L1+L3-J2) DIV 2 ;
     M33 := (L3+J2-L1) DIV 2 ;
     M41 := (L1+L2-J3) DIV 2 ;
     M42 := (L1+J3-L2) DIV 2 ;
     M43 := (L2+J3-L1) DIV 2 ;
     M51 := (J1+J2+J3) DIV 2 ;
     M52 := (J1+L2+L3) DIV 2 ;
     M53 := (L1+J2+L3) DIV 2 ;
     M54 := (L1+L2+J3) DIV 2 ;
     M61 := (J1+J2+L1+L2) DIV 2 ;
     M62 := (J2+J3+L2+L3) DIV 2 ;
     M63 := (J3+J1+L3+L1) DIV 2 ;
     D1 := DD (M11,M12,M13,M51) ;
     D2 := DD (M21,M22,M23,M52) ;
     D3 := DD (M31,M32,M33,M53) ;
     D4 := DD (M41,M42,M43,M54) ;
     D := D1*D2*D3*D4 ;
     KA := M51 ;
     IF M52 > KA THEN KA := M52 ;
     IF M53 > KA THEN KA := M53 ;
     IF M54 > KA THEN KA := M54 ;
     IF KA = 0 THEN RES := 1.0
     ELSE BEGIN
       KE := M61 ;
       IF M62 < KE THEN KE := M62 ;
       IF M63 < KE THEN KE := M63 ;
       IF KA > KE THEN IER := 3
       ELSE BEGIN
         FOR L := KA TO KE DO BEGIN
            IF ODD (L) THEN W1 := -1
                       ELSE W1 := 1 ;
            W1 := W1 * FAKULT(L+1) ;
            W2 := FAKULT(L-M51)*FAKULT(L-M52)*FAKULT(L-M53)*
                  FAKULT(L-M54) ;
            W3 := FAKULT(M61-L)*FAKULT(M62-L)*FAKULT(M63-L) ;
            RES := RES + W1/(W2*W3) ;
         END ;
         RES := RES * D ;
         IER := 0 ;
       END ;
     END ;
   END ;
END ;                                            (* END SECHSJ *)
 
PROCEDURE MATOUT ( VAR A     : TYPE_QMATRIX ;    (* MATOUT *)
                   VAR M,N   : INTEGER      ;
                       W     : INTEGER     );
(*    A  MATIX VOM RANG (MXN) DIE AUF FILE OUT AUSGEGEBEN *)
(*       WERDEN SOLL.                                     *)
(*  M,N  RANG DER  MATRIX A                               *)
(*   W   ANZAHL DER WERTE PRO ZEILE                       *)
 
VAR I,J,Z,K : INTEGER ;
 
BEGIN
  K := TRUNC(70/(W+2)) ;
  Z := 1;
  FOR I := 1 TO M DO BEGIN
    WRITE('0',I:2,'/ 1  ');
    FOR J := 1 TO N DO BEGIN
      WRITE(A(.I,J.):K,'  ') ;
      IF ((J MOD W = 0 ) AND ( J <> N )) THEN BEGIN
        WRITELN ;
        WRITE('   /',J+1:2,'  ') ;
      END ;
    END ;
    WRITELN ;
  END ;
  WRITELN('0') ;
END ;                                             (* ENDE MATOUT *)
 
PROCEDURE MATINV ( VAR B,A : TYPE_QMATRIX ;        (* MATINV    *)
                       N   : INTEGER      ;
                   VAR D   : REAL8    )   ;
 
                     (*  B   : INPUT MATRIX      *)
                     (*  A   : INVERSE MATRIX    *)
                     (*  D   : DETERMINATE       *)
                     (*  N   : ORDNUNG VON B,A   *)
 
LABEL  9999 ;
VAR   L,ML      : ARRAY (.1..PMAX.) OF INTEGER ;
      BIGA,HOLD : REAL8 ;
      KK,I,J     : INTEGER ;
 
  PROCEDURE CHANGE ( VAR A,B : REAL8 ) ;
  VAR C : REAL8 ;
  BEGIN
    C := -A ;
    A := B ;
    B := C ;
  END ;
 
BEGIN
  FOR I := 1 TO N DO (* KOPIERE INPUTMATRIX NACH MATINV (A) *)
    FOR J := 1 TO N DO A(.I,J.) := B(.I,J.) ;
  D := 1.0 ;
  FOR KK := 1 TO N DO BEGIN
    L(.KK.):= KK ;
    ML(.KK.) := KK ;
    BIGA := A(.KK,KK.) ;
    FOR J := KK TO N DO
      FOR I := KK TO N DO IF ABS(BIGA) < ABS(A(.I,J.)) THEN BEGIN
        BIGA := A(.I,J.) ;
        L(.KK.) := I ;
        ML(.KK.) := J ;
    END ;
    J := L(.KK.) ;              (*  INTERCHANGE ROWS *)
    IF J > KK THEN  FOR I := 1 TO N DO CHANGE (A(.KK,I.),A(.J,I.)) ;
    I := ML(.KK.) ;              (*  INTERCHANGE COLUMNS *)
    IF I > KK THEN  FOR J := 1 TO N DO CHANGE (A(.J,KK.),A(.J,I.)) ;
    IF BIGA = 0 THEN BEGIN   (* DIVIDE COLUMN BY -BIGA *)
      D := 0.0 ;
      GOTO 9999 ;
    END ;
    FOR I := 1 TO N DO IF I <> KK THEN A(.I,KK.) := -A(.I,KK.)/BIGA ;
    FOR I := 1 TO N DO BEGIN (* REDUCE MATRIX *)
      HOLD := A (.I,KK.) ;
      FOR J := 1 TO N DO
        IF ((I<>KK)AND(J<>KK)) THEN A(.I,J.) := HOLD*A(.KK,J.)+A(.I,J.) ;
    END ;
    FOR J := 1 TO N DO        (* DIVIDE ROW BY BIGA *)
      IF J <> KK THEN A(.KK,J.) := A(.KK,J.) / BIGA ;
    D := D * BIGA ;
    A(.KK,KK.) := 1 / BIGA ;
  END ;
           (* FINAL ROW AND COLUMN INTERCHANGE *)
  IF N > 1 THEN  FOR KK := N-1 DOWNTO 1 DO BEGIN
    I := L(.KK.) ;
    IF I > KK THEN FOR J := 1 TO N DO CHANGE (A(.J,I.),A(.J,KK.)) ;
    J := ML(.KK.) ;
    IF J > KK THEN FOR I := 1 TO N DO CHANGE (A(.J,I.),A(.KK,I.)) ;
  END ;
9999 : END ;                                     (* END MATINV *)
 
PROCEDURE MATMULT (VAR MATPROD : TYPE1_MATRIX ;  (* MATMULT *)
                   VAR MAT1    : TYPE2_MATRIX ;
                   VAR MAT2    : TYPE3_MATRIX ;
                   VAR SP1,Z1,Z2 : INTEGER ) ;
 
(* ES WIRD DAS PRODUKT MATPROD = MAT1 * MAT2 AUSGEF?HRT     *)
(* MAT1 HAT DEN RANG(SP1 X Z1 ) UND MAT2 DEN RANG (Z1 X Z2 )*)
 
VAR I,J,K : INTEGER ;
 
BEGIN
  FOR I := 1 TO SP1 DO BEGIN
    FOR J := 1 TO Z2 DO BEGIN
      MATPROD(.I,J.) := 0.0 ;
      FOR K := 1 TO Z1 DO
        MATPROD(.I,J.) := MATPROD(.I,J.) + MAT1(.I,K.) * MAT2(.J,K.) ;
    END ;
  END ;
END ;                                              (* END MATMULT *)
 
PROCEDURE MAVEKMULT( VAR A   : TYPE_QMATRIX ;      (* MAVEKMULT *)
                     VAR V,P : TYPEP_VEKTOR ;
                         N,M : INTEGER ) ;
 
VAR I,J : INTEGER ;
 
BEGIN
  FOR I := 1 TO N DO BEGIN
    P(.I.) := 0.0 ;
    FOR J := 1 TO M DO P(.I.) := P(.I.) + A(.I,J.) * V(.J.) ;
  END ;
END ;                                             (* END MAVEKMULT *)
 
FUNCTION YCALC ( VAR DER : TYPE2_MATRIX ;         (* YCALC *)
                 VAR P   : TYPEP_VEKTOR ;
                     I,R : INTEGER ) : REAL8 ;
 
VAR J : INTEGER ;
   AUX : REAL8 ;
 
BEGIN
  AUX := 0.0 ;
  FOR J := 1 TO R DO AUX := AUX + DER(.I,J.) * P(.J.) ;
  YCALC := AUX ;
END ;                                            (* END YCALC *)
 
PROCEDURE LEASTS ( VAR DER     : TYPE2_MATRIX ;  (* LEASTS *)
                   VAR DY,YY   : TYPEX_VEKTOR ;
                       N,R     : INTEGER      ;
                   VAR CB      : TYPE_QMATRIX ;
                   VAR PP,P    : TYPEP_VEKTOR ;
                   VAR CHI     : REAL8        ;
                   VAR IER     : INTEGER    ) ;
 
(*    DER       KOEFFIZIENTENMATRIX (N X R )                 *)
(*    DY,Y      FEHLER- UND WERTEVEKTOR (N)                  *)
(*    N         ZAHL DER MESSUNGEN                           *)
(*    R         ZAHL DER PARAMETER                           *)
(*    CB        KOVARIANZMATRIX   ( R X R )                  *)
(*    PP,P      PARAMETERVEKTOR,BZW. KOREKTUREN              *)
(*    CHI       CHI-QUADRAT                                  *)
(*    IER       FEHLERCODE   = 0  ALLES O.K.                 *)
(*                           = 4  PARAMETERZAHL IST NULL     *)
(*                           = 3  DETERMINATE NULL           *)
(*                           = 1  WENIGE WIE 1 FREIHEITSGRAD *)
 
LABEL 9998 ;
VAR I,J,K  : INTEGER ;
    YN     : TYPEP_VEKTOR ;
    D,H0   : REAL8 ;
    Q,II   : TYPE_QMATRIX ;
 
BEGIN                (* TEST AUF FEHLER *)
  IER := 0 ;
  IF R < 1 THEN BEGIN
    IER := 4 ;
    WRITELN('0ES SOLLEN WENIGER WIE 1 PARAMETER GEFITTET WERDEN :') ;
    GOTO 9998 ;
  END ;
  IF N < R THEN BEGIN
    IER := 1 ;
    WRITELN('0DIE ZAHL DER FREIHEITSGRADE IST KLEINER NULL :') ;
    GOTO 9998 ;
  END ;
  FOR I := 1 TO R DO BEGIN       (* BERECHNUNG DER VARIANZ-MATRIX *)
    FOR J := 1 TO R DO BEGIN
      Q(.I,J.) := 0.0 ;
      FOR K := 1 TO N DO BEGIN
        IF DY(.K.) = 0 THEN DY(.K.) := 1.0 ;
        Q(.I,J.) := Q(.I,J.) + DER(.K,I.) / DY(.K.) / DY(.K.) *
                   DER(.K,J.) ;
      END ;
    END ;
    YN(.I.) := 0.0 ;               (* UND DES VARIANZVEKTORS *)
    FOR K := 1 TO N DO
      YN(.I.) := YN(.I.) + DER(.K,I.) / DY(.K.) / DY(.K.) * YY(.K.) ;
  END ;
  MATINV(Q,CB,R,D) ;             (* BILDUNG DER KOVARIANZMATRIX *)
  IF D = 0 THEN BEGIN
    IER := 3 ;
    WRITELN('0DIE DETERMINATE IST NULL  :') ;
    GOTO 9998 ;
  END ;
  MAVEKMULT(CB,YN,P,R,R) ;    (* BERECHNUNG DER PARAMETERKORREKTUREN *)
  FOR J := 1 TO R DO  PP(.J.) := PP(.J.) + P(.J.) ;
  CHI := 0.0 ;                  (* BERECHNUNG VON CHI-QUADRAT *)
  FOR I := 1 TO N DO BEGIN
    D := (YY(.I.)-YCALC(DER,PP,I,R) ) /DY(.I.) ;
    CHI := CHI + D * D ;
  END ;
9998 : END ;                                     (* END LEASTS *)
 
FUNCTION C ( VAR F,J,I : REAL8 ) :REAL8 ;        (* HFSKOEF *)
 
BEGIN
   C := F * (F+1) -I * (I+1) - J * (J+1 ) ;
END ;
 
FUNCTION COEFA( VAR F,J,I : REAL8 ) : REAL8 ;
BEGIN
   COEFA := C(F,J,I ) / 2.0 ;
END ;
 
FUNCTION COEFB ( VAR F,J,I: REAL8) : REAL8 ;
VAR AUX : REAL8 ;
BEGIN
   AUX := 2 * I * (2*I-1) * J * (2*J-1) ;
   IF ABS(AUX) < 0.001 THEN COEFB := 0.0
   ELSE BEGIN
      AUX := ( 0.75 * C(F,J,I)*( C(F,J,I)+1 ) - I*(I+1)*J*(J+1) )/AUX ;
      COEFB := AUX ;
   END ;
END ;                                            (* END HFSKOEF *)
 
PROCEDURE DERIV ( VAR DER     : TYPE2_MATRIX  ;  (* DERIV *)
                  VAR XJO,XJU,XI1,XI2 : REAL8 ;
                  VAR FO,FU   : TYPEX_VEKTOR ;
                  VAR ISOTOP  : TYPEISO_VEKTOR ;
                      N,R     : INTEGER ) ;
 
VAR I,J,K             : INTEGER ;
    AUX,H0,H1,H2,H3,XI : REAL8 ;
BEGIN
   FOR I := 1 TO N DO BEGIN ;
     AUX := 1.0 ;
     IF ISOTOP(.I.) = '2' THEN BEGIN
        J := 5  ;
        XI := XI2 ;
     END ELSE BEGIN
        J := 0 ;
        XI := XI1 ;
     END ;
      DER (.I,J+1.) := 1.0 ;
      DER (.I,J+2.) := COEFA (FO(.I.),XJO,XI ) ;
      DER (.I,J+3.) := -COEFA (FU(.I.),XJU,XI ) ;
      DER (.I,J+4.) := COEFB (FO(.I.),XJO,XI ) ;
      DER (.I,J+5.) := -COEFB (FU(.I.),XJU,XI ) ;
   END ;
END ;                                            (* END DERIV *)
 
PROCEDURE WRITECHAR  ( C : CHAR ;                (* WRITECHAR *)
                       N : INTEGER ) ;
 
VAR I : INTEGER ;
 
BEGIN
  WRITE(' ') ;
  FOR I := 1 TO N DO WRITE(C:1) ;
  WRITELN ;
  WRITELN ;
END ;                                            (* END WRITECHAR *)
 
PROCEDURE WRITEOUT ( VAR P             : TYPEP_VEKTOR ; (* WRITEOUT *)
                     VAR CA            : TYPE_QMATRIX  ;
                     VAR XJO,XJU,XI1,XI2 : REAL8 ;
                     VAR FO,FU         : TYPEX_VEKTOR ;
                     VAR ISOTOP        : TYPEISO_VEKTOR ;
                     VAR Y,DY          : TYPEX_VEKTOR ;
                     VAR DER           : TYPE2_MATRIX ;
                         N,R,RR        : INTEGER ;
                     VAR CHI           : REAL8   ;
                     VAR KOPF          : TYPE_KOPF  ;
                     VAR COMMENT       : KOMMENTAR ;
                     VAR SORT,NFP,REFP : TYPEPINT_VEKTOR ;
                     VAR VALP          : TYPEP_VEKTOR  ;
                     VAR PNAME         : TYPE_PNAME  ;
                     VAR XNAME         : TYPEXN_VEKTOR ) ;
 
VAR I,J,K,FR     : INTEGER ;
    AUX          : REAL8 ;
    Q            : TYPE_QMATRIX ;
    PP,DP        : TYPEP_VEKTOR ;
    VARIIERT     : PACKED ARRAY (.1..8.) OF CHAR ;
 
BEGIN
  VARIIERT := 'VARIIERT' ;
  WRITELN('1',COMMENT) ;
  WRITECHAR('=',70) ;
  WRITELN('0','ES WURDE EIN ',KOPF,' DURCHGEFUEHRT') ;
  WRITECHAR('-',70) ;
  WRITELN('0',' J(OBEN) =',XJO:6:1, '     J(UNTEN) =',XJU:6:1,
              '      I1 =',XI1:6:1,'    I2 =' ,XI2:6:1);
  WRITELN('0','ERGEBNISSE :  ') ;
  WRITECHAR('-',14) ;
  FOR I := 1 TO RR DO BEGIN (* BERECHNUNG DER FESTGEHALTENEN PARAMETER*)
    K := SORT(.I.) ;
    IF K > 0 THEN BEGIN
      PP(.I.) := P(.K.) ;
      DP(.I.) := SQRT(ABS(CA(.K,K.))) ;
    END ;
  END ;
  FOR I := 1 TO RR-R DO IF REFP(.I.) = 0 THEN BEGIN
     DP(.NFP(.I.).) := 1.0 ;
     PP(.NFP(.I.).) := VALP(.I.) ;
  END ;
  FOR I:= 1 TO RR-R DO IF REFP(.I.) > 0 THEN BEGIN
     DP(.NFP(.I.).) := VALP(.I.) * DP(.REFP(.I.).) ;
     PP(.NFP(.I.).) := VALP(.I.) * PP(.REFP(.I.).) ;
  END ;
  WRITELN('0') ;
  WRITELN(' CHI-QUADRAT          = ',CHI :10 ) ;
  FR := N-R ;
  IF FR <=1 THEN FR := 1 ;
  AUX := SQRT(CHI/FR)  ;
  WRITELN('0','ANZAHL FREIHEISGRADE = ',FR : 10 ) ;
  WRITELN('0','RMS - FAKTOR         = ',AUX:10 ) ;
  WRITELN('0','ES WURDEN ',R:2,' PARAMETER FREI VARIIERT ') ;
  WRITELN('0') ;
  WRITELN(' AUSGABE DER GEFITTETEN PARAMETER UND DEREN FEHLER') ;
  WRITECHAR('-',70) ;
  FOR I := 1 TO RR DO BEGIN
       WRITE('0',PNAME(.I.):10,' = ',PP(.I.):15:5,'   (',DP(.I.):15:7,
                 ')') ;
       IF SORT(.I.) > 0 THEN WRITELN(VARIIERT:12)
                        ELSE WRITELN(PNAME(.SORT(.I.).):14) ;
  END ;
  WRITELN('1AUSGABE DER RUECKGERECHNETEN WERTE UND DER EINGABEN :') ;
  WRITECHAR('-',70) ;
  WRITELN('0','             FO    FU            Y            DY',
              '          YCALC        YCALC-Y') ;
  WRITELN ;
  FOR I := 1 TO N DO BEGIN
    AUX := YCALC(DER,PP,I,RR) ;
    WRITELN(' ',ISOTOP(.I.):4,XNAME(.I.):6,FO(.I.):6:1,FU(.I.):6:1,
                Y(.I.):15:3,DY(.I.):14:3,AUX:14:3,(AUX-Y(.I.)):14:3 ) ;
  END ;
  WRITELN('1','KOVARIANZMATRIX    :') ;
  WRITECHAR('-',20) ;
  MATOUT (CA,R,R,6) ;
  FOR I := 1 TO R DO
    FOR J := 1 TO R DO Q(.I,J.) := CA(.I,J.)/SQRT(ABS(CA(.I,I.)*
                                   CA(.J,J.))) ;
  WRITELN('-','KORRELATIONSMATRIX :') ;
  WRITECHAR('-',20) ;
  MATOUT(Q,R,R,6) ;
END ;
 
PROCEDURE WRITEBEZ ( VAR XJO,XJU,XI1,XI2 : REAL8 ;  (* WRITEBEZ *)
                     VAR NFP             : TYPEPINT_VEKTOR ;
                     VAR VALFP           : TYPEP_VEKTOR ;
                     VAR COMMENT         : KOMMENTAR ;
                         ISOTOPE,PMAX    : INTEGER ) ;
 
VAR FU,FO,FMAXO,FMINO,FMAXU,FMINU : REAL8 ;
    I,K,H,IER                     : INTEGER ;
    AUX,RES6MAX,INT,XI            : REAL8 ;
    CHARACTER                     : CHAR ;
    P                             : TYPEP_VEKTOR ;
    CCHR                          : PACKED ARRAY (.1..26.) OF CHAR ;
 
BEGIN
   CHARACTER := ' ';
   CCHR := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' ;
   WRITELN('0',COMMENT ) ;
   WRITECHAR ( '=',70 ) ;
   WRITELN('0','AUSGABE DER BERECHNETEN FREQUENZEN UND DEREN ',
               'INTENSITAET') ;
   WRITECHAR('-',70 ) ;
   FOR I := 1 TO PMAX DO P(.NFP(.I.).) := VALFP(.I.) ;
   FOR I := 1 TO ISOTOPE DO BEGIN
      IF I = 2 THEN BEGIN
         H := 5 ;
         XI := XI2 ;
      END  ELSE BEGIN
         H := 0 ;
         XI := XI1 ;
      END ;
      K := 1 ;
      FMAXU := XJU + XI ;
      FMINU := ABS(XJU-XI) ;
      FU := FMAXU ;
      REPEAT
         FMAXO := XJO + XI ;
         FMINO := ABS(XJO - XI) ;
         FO := FMAXO ;
         REPEAT
            IF (ABS(FO-FU) <= 1) AND ((FO*FU) <> 0.0) THEN BEGIN
               AUX := P(.1+H.) + COEFA(FO,XJO,XI) * P(.2+H.)
                             - COEFA(FU,XJU,XI) * P(.3+H.)
                             + COEFB(FO,XJO,XI) * P(.4+H.)
                             - COEFB(FU,XJU,XI) * P(.5+H.) ;
               SECHSJ(XJO,FO,XI,FU,XJU,1.0,INT,IER ) ;
               INT := (2*FU+1) * (2*FO+1) *INT * INT ;
               IF (FO = FMAXO) AND (FU = FMAXU ) THEN RES6MAX := INT ;
               INT := INT / RES6MAX * 100 ;
               WRITELN (' ',CCHR(.K.),CHARACTER,FO:8:1,FU:8:1,AUX:20:5,
                            INT:10:2,'%') ;
               K := K+1
            END ;
            FO := FO - 1 ;
         UNTIL FO < FMINO ;
         FU := FU - 1 ;
      UNTIL FU < FMINU ;
      CHARACTER :='"' ;
      WRITELN ;
   END ;
END ;                                            (* END WRITEBEZ *)
 
PROCEDURE CHECK( VAR NFP,REFP        : TYPEPINT_VEKTOR ; (* CHECK *)
                 VAR VALFP           : TYPEP_VEKTOR ;
                 VAR PNAME           : TYPE_PNAME ;
                 VAR XJO,XJU,XI1,XI2 : REAL8 ;
                 VAR N,NISOTOP       : INTEGER ;
                 VAR FLAG            : BOOLEAN ) ;
 
VAR I,J,K : INTEGER ;
 
   PROCEDURE ADD (VAR N : INTEGER ;
                      NR: INTEGER ) ;
   BEGIN
      N := N+1 ;
      NFP(.N.) :=  NR ;
      VALFP(.N.) := 0.0 ;
      REFP(.N.) := 0 ;
   END ;
 
BEGIN
   IF (XJO < 1.0) OR (XI1 < 1.0 ) THEN ADD(N,4) ;
   IF NISOTOP = 2 THEN
       IF (XJO < 1.0) OR (XI2 < 1.0 ) THEN ADD(N,9) ;
   IF (XJU < 1.0) OR (XI1 < 1.0 ) THEN ADD(N,5) ;
   IF NISOTOP = 2 THEN
       IF (XJU < 1.0) OR (XI2 < 1.0 ) THEN ADD(N,10) ;
   I := 1 ;
   IF N > 1 THEN REPEAT
      J := 1 ;
      REPEAT
         IF I <> J THEN BEGIN
          IF (REFP(.I.) = NFP(.J.)) AND (REFP(.J.) <> 0) THEN BEGIN
            WRITELN('0','PARAMETER NR. ',PNAME(.NFP(.I.).),' WIRD AUF',
                        ' EINEN PARAMETER FESTGEHALTEN DER WEDER') ;
            WRITELN(' VARIIERT WIRD NOCH ABSOLUT FESTGEHALTEN WIRD');
            FLAG := FALSE ;
            END;
            IF NFP(.I.) = NFP(.J.) THEN BEGIN
               IF (REFP(.I.) = REFP(.J.)) AND (REFP(.I.) = 0 ) AND
                  (VALFP(.I.) = VALFP(.J.)) THEN BEGIN
                  IF J < N THEN FOR K := J+1 TO N DO BEGIN
                     NFP(.K-1.) := NFP(.K.) ;
                     VALFP(.K-1.) := VALFP(.K.) ;
                     REFP(.K-1.) := REFP(.K.) ;
                     J := J-1 ;
                  END ;
                  N := N-1 ;
               END ELSE BEGIN
                  WRITELN('0','DER PARAMETER ',PNAME(.NFP(.I.).),
                  ' WIRD MEHRMALS FESTGEHALTEN') ;
                  FLAG := FALSE ;
               END ;
            END ;
 
         END ;
         J := J + 1 ;
      UNTIL J > N ;
      IF NFP(.I.) = REFP(.I.) THEN BEGIN
         WRITELN ('0DER PARAMETER ' ,PNAME(.NFP(.I.).),
                 ' WIRD AUF SICH SELBST BEZOGEN' ) ;
         FLAG := FALSE ;
      END ;
      I := I + 1 ;
   UNTIL I > N ;
END ;                                         (* END CHECK *)
 
PROCEDURE READDATA ( VAR N,NISOTOP        : INTEGER ;    (* READDATA *)
                     VAR COMMENT          : KOMMENTAR ;
                     VAR FIXED_PAR        : INTEGER ;
                     VAR FLAG             : BOOLEAN ;
                     VAR XJO,XJU,XI1,XI2  : REAL8 ;
                     VAR FO,FU            : TYPEX_VEKTOR ;
                     VAR ISOTOP           : TYPEISO_VEKTOR ;
                     VAR NFP,REFP         : TYPEPINT_VEKTOR ;
                     VAR VALFP            : TYPEP_VEKTOR ;
                     VAR Y,DY             : TYPEX_VEKTOR  ;
                     VAR XNAME            : TYPEXN_VEKTOR ) ;
 
VAR I,J,MAX : INTEGER ;
    C       : CHAR ;
 
BEGIN
  FLAG := TRUE ;
  READLN(COMMENT ) ;
  WRITELN('1',COMMENT) ;
  READLN(NISOTOP,FIXED_PAR,XJO,XJU,XI1,XI2) ;
  WRITELN('0','J(OBEN) =',XJO:5:1,'  J (UNTEN) =',XJU:5:1,
              '      I1 =',XI1:5:1,'  I2  =',XI2:5:1 ) ;
  MAX := 5 ;
  IF NISOTOP >2 THEN NISOTOP := 2 ;
  IF NISOTOP <1 THEN NISOTOP := 1 ;
  MAX := NISOTOP * MAX ;
  IF (XJO < 1.0) OR(XI1<1.0) THEN MAX := MAX - 1 ;
  IF (XJU < 1.0) OR(XI1<1.0) THEN MAX := MAX - 1 ;
  IF (XJO < 1.0) OR(XI2<1.0) THEN MAX := MAX - 1 ;
  IF (XJU < 1.0) OR(XI2<1.0) THEN MAX := MAX - 1 ;
  IF FIXED_PAR <> 0  THEN BEGIN
     WRITELN('0','ES WURDEN FOLGENDE ',FIXED_PAR:2,' PARAMETER ',
                 'FESTGEHALTEN  :') ;
     WRITELN('0','FIXED PARAMETER        WERT/VERHAELTNIS  ',
                 '    BEZUGS-PARAMETER') ;
     WRITELN ;
     FOR I := 1 TO FIXED_PAR DO BEGIN
       READLN(NFP(.I.),VALFP(.I.),REFP(.I.) ) ;
       WRITELN(' ',PNAME(.NFP(.I.).):10,VALFP(.I.):25:5,
                   PNAME(.REFP(.I.).):20 );
     END ;
  END ;
  N := 1 ;
  IF NOT EOF(INPUT) THEN BEGIN
     WRITELN('0','AUSGABE DER EINGABE WERTE  :') ;
     WRITELN('0','                FO    FU              Y',
                 '                 DY') ;
     WRITELN ;
     REPEAT                                 (* EINLESEN DER WERTE *)
       READLN ( ISOTOP(.N.) ,XNAME(.N.),C, FO(.N.) ,FU(.N.) , Y(.N.) ,
                DY(.N.)) ;
       IF DY(.N.) <= 0.0 THEN DY(.N.) := 1 ;
       WRITELN(' ',N:3,ISOTOP(.N.):4,XNAME(.N.):6,FO(.N.):6:1,
                   FU(.N.):6:1,Y(.N.):17:3,DY(.N.):17:3) ;
       N := N + 1 ;
     UNTIL EOF(INPUT) OR (N > WMAX) OR ( C <> ' ' );
  END ;
  N := N-1 ;
  IF N >= WMAX THEN WRITELN('0ANZAHL DER WERTE GROESSER ALS',
                                ' ERLAUBTES MAXIMUM (',WMAX:3,')') ;
END ;
 
PROCEDURE MODDER ( VAR D  ,DER2      : TYPE2_MATRIX ; (* MODDER *)
                   VAR NPAR,FIXP     : INTEGER      ;
                   VAR NFP,REFP,SORT : TYPEPINT_VEKTOR ;
                   VAR VALFP         : TYPEP_VEKTOR  ;
                   VAR Y,YY          : TYPEX_VEKTOR ) ;
 
VAR I,J,K,L,M : INTEGER ;
    FLAG      : BOOLEAN ;
    DER       : TYPE2_MATRIX ;
    SORTHELP  : TYPEPINT_VEKTOR ;
 
BEGIN
   DER := D ;
   L := 1 ;
   YY := Y ;
   FOR I := 1 TO NPAR DO BEGIN
     FLAG := FALSE ;
     IF FIXP >0 THEN FOR K := 1 TO FIXP DO IF NFP(.K.) = I THEN BEGIN
        FLAG := TRUE ;
        M := K ;
     END ;
     IF FLAG THEN BEGIN
        IF REFP(.M.) = 0 THEN FOR J := 1 TO N DO BEGIN
           SORT(.I.) := 0 ;
           SORTHELP(.I.) := M ;
        END ELSE FOR J := 1 TO N DO BEGIN
           SORT(.I.) := -1 ;
           DER(.J,REFP(.M.).) := DER(.J,REFP(.M.).) + VALFP(.M.)*
                                 DER(.J,NFP(.M.).) ;
        END ;
     END ELSE BEGIN
        SORT(.I.) := L ;
        L := L + 1 ;
     END ;
   END ;
   FOR I := 1 TO NPAR DO BEGIN
      IF SORT(.I.) > 0 THEN FOR J := 1 TO N DO DER2(.J,SORT(.I.).)
       := DER(.J,I.) ;
      IF SORT(.I.) = 0 THEN FOR J := 1 TO N DO
         YY(.J.) := YY(.J.) - DER(.J,NFP(.SORTHELP(.I.).).) *
          VALFP(.SORTHELP(.I.).) ;
   END ;
   NPAR := NPAR - FIXP ;
END ;                                            (* END MODDER *)
 
(************************************************************)
(*                                                          *)
(*            H A U P T P R O G R A M M                     *)
(*                                                          *)
(************************************************************)
BEGIN
  KOPF := ' HFSFIT '  ;
  PNAME(.-1.) := 'RELATIV   ' ;
  PNAME(.0.) := 'FIXED     ' ;
  PNAME(.1.) := 'S1        ' ;
  PNAME(.2.) := 'A1 (OBEN) ' ;
  PNAME(.3.) := 'A1 (UNTEN)';
  PNAME(.4.) := 'B1 (OBEN) ';
  PNAME(.5.) := 'B1 (UNTEN)';
  PNAME(.6.) := 'S2        ';
  PNAME(.7.) := 'A2 (OBEN) ';
  PNAME(.8.) := 'A2 (UNTEN)';
  PNAME(.9.) := 'B2 (OBEN) ';
  PNAME(.10.):= 'B2 (UNTEN)';
  WHILE NOT EOF(INPUT) DO BEGIN
    READDATA (N,NISOTOP,COMMENT,FIXED_PAR,FLAG,XJO,XJU,XI1,XI2,FO,FU,
              ISOTOP,NFP,REFP,VALFP,Y,DY,XNAME ) ;
    CHECK(NFP,REFP,VALFP,PNAME,XJO,XJU,XI1,XI2,FIXED_PAR,NISOTOP,FLAG);
    IF (XI1<=0.0) OR (XJO < 0.0) OR (XJU < 0.0) OR (XI2 < 0.0)THEN BEGIN
      WRITELN('0FALSCHE QUANTENZAHL FUER KERN- ODER H?LLENSPIN ')
    END ELSE IF FLAG THEN BEGIN
       IF FIXED_PAR = NISOTOP * 5  THEN BEGIN
          WRITEBEZ(XJO,XJU,XI1,XI2,NFP,VALFP,COMMENT,NISOTOP,FIXED_PAR);
       END ELSE BEGIN
          RR := NISOTOP * 5 ;
          R := RR ;
          FOR J := 1 TO R DO BEGIN
            FOR I := 1 TO N DO DER(.I,J.) := 0.0 ;
            PP(.J.) := 0.0 ;
            P(.J.) := 0.0 ;
          END ;
          DERIV(DER,XJO,XJU,XI1,XI2,FO,FU,ISOTOP,N,R ) ;
          MODDER( DER,DER2,R,FIXED_PAR,NFP,REFP,SORT,VALFP,Y,YY) ;
          LEASTS (DER2,DY,YY,N,R,CA,PP,P,CHI,IER ) ;
          IF IER = 0 THEN WRITEOUT(PP,CA,XJO,XJU,XI1,XI2,FO,FU,ISOTOP,Y,
                DY,DER,N,R,RR,CHI,KOPF,COMMENT,SORT,NFP,REFP,VALFP,
                PNAME,XNAME);
       END ;
     END;
     IF NOT FLAG THEN WRITELN('0','DER FIT WIRD DESHALB ABGEBROCHEN') ;
   END ;
END.
                                
