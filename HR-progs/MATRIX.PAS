(*$include: 'matrix.int' *)

(* Version vom 1.1.1986  *)

IMPLEMENTATION OF matrix  ;

(*$debug- *)
(*$line- *)

FUNCTION getmqq( wants : WORD) : adsmem ;             EXTERNAL ;

PROCEDURE dismqq(block:adsmem) ;                      EXTERNAL ;

PROCEDURE copy_matrix(VARS   mat1 : matrix_matrix ;
                      CONSTS mat2 : matrix_matrix ;
                             n    : INTEGER ) ;

VAR i,j : INTEGER ;

BEGIN
   FOR i := 1 TO n DO FOR j := 1 TO n DO mat1[i,j] := mat2[i,j] ;
END ;

PROCEDURE matinv ;
                     (*  b   : input matrix      *)
                     (*  a   : inverse matrix    *)
                     (*  d   : determinate       *)
                     (*  n   : ordnung von b,a   *)

CONST maxmatrix = 40 ;

VAR   l,ml      : ARRAY [1..maxmatrix] OF INTEGER ;
      biga,hold : REAL8 ;
      kk,i,j     : INTEGER ;

  PROCEDURE change ( VARS a,b : REAL8 ) ;
  VAR c : REAL8 ;
  BEGIN
    c := -a ;
    a := b ;
    b := c ;
  END ;


BEGIN
  FOR i := 1 TO n DO (* Kopiere Inputmatrix nach matinv (a) *)
    FOR j := 1 TO n DO a[i,j] := b[i,j] ;
  d := 1.0 ;
  IF n > maxmatrix THEN BEGIN
     writeln(output,'Matrix zu gross.  n = ',n) ;
     RETURN ;
  END ;
  FOR kk := 1 TO n DO BEGIN
    l[kk] := kk ;
    ml[kk] := kk ;
    biga := a[kk,kk] ;
    FOR j := kk TO n DO
      FOR i := kk TO n DO IF abs(biga) < abs(a[i,j]) THEN BEGIN
        biga := a[i,j] ;
        l[kk] := i ;
        ml[kk] := j ;
    END ;
    j := l[kk] ;              (*  interchange rows *)
    IF j > kk THEN  FOR i := 1 TO n DO change (a[kk,i],a[j,i]) ;
    i := ml[kk] ;              (*  interchange columns *)
    IF i > kk THEN  FOR j := 1 TO n DO change (a[j,kk],a[j,i]) ;
    IF biga = 0 THEN BEGIN   (* divide column by -biga *)
      d := 0.0 ;
      RETURN ;
    END ;
    FOR i := 1 TO n DO IF i <> kk THEN a[i,kk] := -a[i,kk]/biga ;
    FOR i := 1 TO n DO BEGIN (* reduce matrix *)
      hold := a [i,kk] ;
      FOR j := 1 TO n DO
        IF ((i<>kk)AND(j<>kk)) THEN a[i,j] := hold*a[kk,j]+a[i,j] ;
    END ;
    FOR j := 1 TO n DO        (* divide row by biga *)
      IF j <> kk THEN a[kk,j] := a[kk,j] / biga ;
    d := d * biga ;
    a[kk,kk] := 1 / biga ;
  END ;
           (* final row AND column interchange *)
  IF n > 1 THEN  FOR kk := n-1 DOWNTO 1 DO BEGIN
    i := l[kk] ;
    IF i > kk THEN FOR j := 1 TO n DO change (a[j,i],a[j,kk]) ;
    j := ml[kk] ;
    IF j > kk THEN FOR i := 1 TO n DO change (a[j,i],a[kk,i]) ;
  END ;
END ;
(*--------------------------------------------------------------------*)
PROCEDURE matmult  ;

(* Es wird das Produkt matprod = mat1 * mat2 ausgefhrt     *)
(* mat1 hat den Rang(sp1 x z1 ) und mat2 den Rang (z1 x z2 )*)

VAR i,j,k : INTEGER ;
BEGIN
  FOR i := 1 TO sp1 DO BEGIN
    FOR j := 1 TO z2 DO BEGIN
      matprod[i,j] := 0.0 ;
      FOR k := 1 TO z1 DO
        matprod[i,j] := matprod[i,j] + mat1[i,k] * mat2[j,k] ;
    END ;
  END ;
END ;                                                  (* End MATMULT *)
(*--------------------------------------------------------------------*)
PROCEDURE mavekmult ;

VAR i,j : INTEGER ;

BEGIN
  FOR i := 1 TO n DO BEGIN
    p[i] := 0.0 ;
    FOR j := 1 TO m DO p[i] := p[i] + a[i,j] * v[j] ;
  END ;
END ;                                             (* End MAVEKMULT *)
(*--------------------------------------------------------------------*)
PROCEDURE leasts   ;                                        (* LEASTS *)

(*    der       Koeffizientenmatrix (n x r )                 *)
(*    dy,y      Fehler- und Wertevektor (n)                  *)
(*    n         Zahl der Messungen                           *)
(*    r         Zahl der Parameter                           *)
(*    cb        Kovarianzmatrix   ( r x r )                  *)
(*    pp,p      Parametervektor,bzw. korekturen              *)
(*    chi       Chi-Quadrat                                  *)
(*    ier       Fehlercode   = 0  alles o.k.                 *)
(*                           = 4  Parameterzahl ist Null     *)
(*                           = 3  Determinate Null           *)
(*                           = 1  wenige wie 1 freiheitsgrad *)
(*    out       Output File                                  *)

VAR i,j,k  : INTEGER ;
    yn     : matrix_vektor(eigen_max) ;
    d,h0   : REAL8 ;
    q      : ADS OF matrix_matrix(eigen_max,eigen_max) ;

BEGIN                (* Test auf Fehler *)
  ier := 0 ;
  IF r < 1 THEN BEGIN
    ier := 4 ;
    writeln(out) ;
    writeln(out,'Es sollen weniger wie 1 Parameter gefittet werden :') ;
    RETURN ;
  END ;
  IF n < r THEN BEGIN
    ier := 1 ;
    writeln(out) ;
    writeln(out,'Die Zahl der Freiheitsgrade ist kleiner Null :') ;
    RETURN ;
  END ;
  q := getmqq(SIZEOF(q^)) ;
  FOR i := 1 TO r DO BEGIN       (* Berechnung der Varianz-Matrix *)
    FOR j := 1 TO r DO BEGIN
      q^[i,j] := 0.0 ;
      FOR k := 1 TO n DO
            q^[i,j] := q^[i,j] + der[k,i] / dy[k] / dy[k] * der[k,j] ;
    END ;
    yn[i] := 0.0 ;               (* Und des Varianzvektors *)
    FOR k := 1 TO n DO
      yn[i] := yn[i] + der[k,i] / dy[k] / dy[k] * yy[k] ;
  END ;
  matinv(q^,cb,r,d) ;             (* Bildung der Kovarianzmatrix *)
  dismqq(q) ;
  IF d = 0 THEN BEGIN
    ier := 3 ;
    writeln(out) ;
    writeln(out,'Die Determinate ist Null  :') ;
    RETURN ;
  END ;
  mavekmult(cb,yn,p,r,r) ;      (* Berechnung der Parameterkorrekturen *)
  FOR j := 1 TO r DO  pp[j] := pp[j] + p[j] ;
  chi := 0.0 ;                  (* Berechnung von Chi-Quadrat *)
  FOR i := 1 TO n DO BEGIN
    d := - yy[i] ;
    FOR j := 1 TO r DO d := d + der[i,j] * p[j] ;
    d := d / dy[i] ;
    chi := chi + d * d ;
  END ;
END ;                                                   (* End LEASTS *)
(*--------------------------------------------------------------------*)
PROCEDURE matout ;

(*    a  matix vom Rang (mxn) die auf FILE out ausgegeben *)
(*       werden soll.                                     *)
(*  m,n  Rang der  matrix a                               *)
(*   w   Anzahl der werte pro Zeile                       *)
(*  opt  1 => Warten wenn Bildschirmseite voll.           *)
(*       sonst kein Warten                                *)
(* out    File OF TEXT                                    *)

VAR i,j,z,k : INTEGER ;
    c       : CHAR    ;

  PROCEDURE wait( VAR z,opt : INTEGER ) ;
  VAR c : CHAR ;
  BEGIN
    IF opt = 1 THEN BEGIN
       z := z + 1 ;
       IF ( z MOD 22 = 0 ) THEN BEGIN
         writeln(output) ; writeln(output) ;
         c := chr(0) ;
         write(output,'weiter : beliebige Taste drücken ') ;
         REPEAT
           read(c) ;
         UNTIL c <> chr(0) ;
         writeln(output) ;
       END ;
     END ;
   END ;

BEGIN
  k := trunc(70/(w+2)) ;
  z := 1;
  FOR i := 1 TO m DO BEGIN
    writeln(out) ;
    wait(z,opt) ;
    write(out,i:2,'/ 1  ');
    FOR j := 1 TO n DO BEGIN
      write(out,a[i,j]:k,'  ') ;
      IF ((j MOD w = 0 ) AND ( j <> n )) THEN BEGIN
        writeln(out) ;
        wait(z,opt) ;
        write(out,'  /',j+1:2,'  ') ;
      END ;
    END ;
    writeln(out) ;
    wait(z,opt)
  END ;
  writeln(out) ; writeln(out) ;
  z := 21 ;
  wait(z,opt) ;
  writeln(out) ;
END ;                                                  (* Ende MATOUT *)
(*--------------------------------------------------------------------*)
PROCEDURE eigen ;

CONST limit = 1e-8 ;

TYPE hilfs_matrix = ADS OF matrix_matrix(eigen_max,eigen_max) ;

VAR i,j,k,m,maxit,ii,kk,e  : INTEGER ;
    c,s,xt,xk,aux,spur,max : REAL8 ;
    work,energie           : hilfs_matrix ;

BEGIN
   energie := getmqq( SIZEOF (energie^)) ;
   work := getmqq( SIZEOF (work^)) ;
   spur := 0 ;
   maxit := (n * n) ;
   m := 0 ;
   copy_matrix(energie^,eigenvek,n) ;
   FOR i := 1 TO n DO BEGIN
       FOR j := 1 TO n DO eigenvek[i,j] := 0 ;
       eigenvek[i,i] := 1;
   END ;
   spur := 0 ;
   FOR i := 1 TO n DO spur := spur + abs(energie^[i,i]) ;
   REPEAT
      max := spur / n * limit ;
      aux := energie^[1,2] ;
      i := 1 ;
      k := 2 ;
      FOR ii := 1 TO n-1 DO FOR kk := ii+1 TO n DO
         IF abs(energie^[ii,kk]) > aux THEN BEGIN
            aux := abs(energie^[ii,kk]) ;
            i := ii ;
            k := kk ;
      END ;
      copy_matrix(work^,energie^,n) ;
      m := m+1 ;
      IF (abs(energie^[i,i]-energie^[k,k]) < 1e-6) THEN BEGIN
         IF (energie^[i,k] < 0) THEN e := -1 ELSE e := 1 ;
         c := 1 / sqrt(2) ;
         s := e * c ;
      END ELSE BEGIN
         xt := 2 * energie^[i,k] / ( energie^[i,i] - energie^[k,k]) ;
         IF xt < 0 THEN e := -1 ELSE e := 1 ;
         xk := 1/sqrt(1+xt*xt) ;
         s := sqrt((1-xk)/2) * e ;
         c := sqrt((1+xk)/2) ;
      END ;
      FOR j := 1 TO n DO IF ( j <> k) AND (i <> j) THEN BEGIN
          work^[j,i] := c*energie^[j,i] + s*energie^[j,k] ;
          work^[i,j] := work^[j,i] ;
          work^[j,k] := s*energie^[j,i] - c*energie^[j,k] ;
          work^[k,j] := work^[j,k] ;
      END ;
      work^[i,i] := c*c*energie^[i,i] + s*s*energie^[k,k] +
                   2*s*c*energie^[i,k] ;
      work^[k,k] := s*s*energie^[i,i] + c*c*energie^[k,k] -
                   2*s*c*energie^[i,k] ;
      work^[i,k] := 0 ;
      work^[k,i] := 0 ;
      spur := spur - abs(energie^[i,i]) - abs(energie^[k,k])
                   + abs(work^[i,i]) + abs(work^[k,k]) ;
      copy_matrix(energie^,work^,n) ;
      copy_matrix(work^,eigenvek,n) ;
      FOR j := 1 TO n DO BEGIN
         eigenvek[j,i] := c * work^[j,i] + s * work^[j,k] ;
         eigenvek[j,k] := s * work^[j,i] - c * work^[j,k] ;
      END ;
   UNTIL (( m > maxit) OR (aux < max) ) ;
   FOR i := 1 TO n DO energ_vek[i] := energie^[i,i] ;
   dismqq(work) ;
   dismqq(energie) ;
END ;
(*--------------------------------------------------------------------*)
END .

