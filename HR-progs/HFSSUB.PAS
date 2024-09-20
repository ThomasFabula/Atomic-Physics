(*$include: 'hfssub.int' *)

(* Version vom 24.1.86 *)

IMPLEMENTATION OF hfssub ;

(*$debug- *)
(*--------------------------------------------------------------*)
FUNCTION fakult  ;

VAR i : INTEGER  ;
    x : REAL8	 ;

TYPE fakult_vektor = ARRAY [0..30] OF REAL8 ;

CONST f = fakult_vektor  (  1.000000000000000E+000,
			    1.000000000000000E+000,
			    2.000000000000000E+000,
			    6.000000000000000E+000,
			    2.400000000000000E+001,
			    1.200000000000000E+002,
			    7.200000000000000E+002,
			    5.040000000000000E+003,
			    4.032000000000000E+004,
			    3.628800000000000E+005,
			    3.628800000000000E+006,
			    3.991680000000000E+007,
			    4.790016000000000E+008,
			    6.227020800000000E+009,
			    8.717829120000000E+010,
			    1.307674368000000E+012,
			    2.092278988800000E+013,
			    3.556874280960000E+014,
			    6.402373705728000E+015 ,
			    1.216451004088320E+017 ,
			    2.432902008176640E+018 ,
			    5.109094217170944E+019 ,
			    1.124000727777608E+021,
			    2.585201673888498E+022,
			    6.204484017332394E+023,
			    1.551121004333099E+025,
			    4.032914611266057E+026,
			    1.088886945041835E+028,
			    3.048883446117138E+029,
			    8.841761993739701E+030,
			    2.652528598121910E+032  ) ;

BEGIN
   IF n IN [0..90] THEN BEGIN
      IF n < 31 THEN fakult := f[n]
      ELSE BEGIN
	 x := f[30] ;
	 FOR i := 31 TO n DO x := x * i ;
	 fakult := x ;
      END ;
  END ELSE BEGIN
    fakult := 0 ;
    writeln(output,'Argument von Fakult„t nicht gltig n =',n:4) ;
  END ;
END ;
(*--------------------------------------------------------------*)
PROCEDURE min ( m1,m2,m3 : INTEGER ;VAR m : INTEGER) ;
BEGIN
   IF m1 < m THEN m := m1 ;
   IF m2 < m THEN m := m2 ;
   IF m3 < m THEN m := m3 ;
END ;
(*--------------------------------------------------------------*)
PROCEDURE max ( m1,m2,m3 : INTEGER ;VAR m : INTEGER) ;
BEGIN
   IF m1 > m THEN m := m1 ;
   IF m2 > m THEN m := m2 ;
   IF m3 > m THEN m := m3 ;
END ;
(*--------------------------------------------------------------*)
FUNCTION dd ( m1,m2,m3,m4 : INTEGER ) : REAL8 ;

BEGIN
   dd :=  fakult(m1) * fakult(m2) * fakult(m3) / fakult(m4+1)  ;
END ;
(*--------------------------------------------------------------*)
FUNCTION sechsj ;

VAR j1,j2,j3,l1,l2,l3				    : INTEGER ;
    m11,m12,m13,m21,m22,m23,m31,m32,m33,m41,m42,m43 : INTEGER ;
    m51,m52,m53,m54,m61,m62,m63,m		    : INTEGER ;
    d1,d2,d3,d4,d,w1,w2,res			    : REAL8   ;
    ke,l,ka,v					    : INTEGER ;

BEGIN
   sechsj := 0.0 ;
   ier := 0 ;
   j1 :=  round(2.0 * a1 ) ;	   (* quantenzahlen -> INTEGER *)
   j2 :=  round(2.0 * a2 ) ;
   j3 :=  round(2.0 * a3 ) ;
   l1 :=  round(2.0 * b1 ) ;
   l2 :=  round(2.0 * b2 ) ;
   l3 :=  round(2.0 * b3 ) ;
   m11 := (j1+j2-j3) DIV 2 ;
   m12 := (j3+j1-j2) DIV 2 ;
   m13 := (j2+j3-j1) DIV 2 ;
   m21 := (j1+l2-l3) DIV 2 ;
   m22 := (l3+j1-l2) DIV 2 ;
   m23 := (l2+l3-j1) DIV 2 ;
   m31 := (l1+j2-l3) DIV 2 ;
   m32 := (l3+l1-j2) DIV 2 ;
   m33 := (j2+l3-l1) DIV 2 ;
   m41 := (l1+l2-j3) DIV 2 ;
   m42 := (j3+l1-l2) DIV 2 ;
   m43 := (l2+j3-l1) DIV 2 ;
   m := m11 ;
   min(m11,m12,m13,m) ;
   min(m21,m22,m23,m) ;
   min(m31,m32,m33,m) ;
   min(m41,m42,m43,m) ;
   IF m < 0 THEN RETURN ;
   m51 := (j1+j2+j3) DIV 2 ;
   m52 := (j1+l2+l3) DIV 2 ;
   m53 := (l1+j2+l3) DIV 2 ;
   m54 := (l1+l2+j3) DIV 2 ;
   m61 := (j1+j2+l1+l2) DIV 2 ;
   m62 := (j2+j3+l2+l3) DIV 2 ;
   m63 := (j3+j1+l3+l1) DIV 2 ;
   d1 := dd (m11,m12,m13,m51) ;
   d2 := dd (m21,m22,m23,m52) ;
   d3 := dd (m31,m32,m33,m53) ;
   d4 := dd (m41,m42,m43,m54) ;
   d := sqrt(d1*d2*d3*d4) ;
   ka := m51 ;
   max(m52,m53,m54,ka) ;
   ke := m61 ;
   min(m61,m62,m63,ke) ;
   IF (ke < ka) THEN BEGIN
      ier := 3 ;
      RETURN;
   END ;
   res := 0 ;
   FOR l := ka TO ke DO BEGIN
      IF odd (l) THEN v := -1
		 ELSE v := 1 ;
      w1 := v * fakult(l+1) ;
      w2 := fakult(l-m51)*fakult(l-m52)*fakult(l-m53)*fakult(l-m54) *
	    fakult(m61-l)*fakult(m62-l)*fakult(m63-l) ;
      res := res + w1/w2 ;
   END ;
   sechsj := res * d ;
END ;						 (* End SECHSJ *)
(*--------------------------------------------------------------*)
FUNCTION dreij ;

VAR j1,j2,j3,m1,m2,m3,n1,n2,n3,n4 : INTEGER ;
    l1,l2,l3,l4,l5,l6,l 	  : INTEGER ;
    k1,k2,k3,ke,v,ka		  : INTEGER ;
    r2,r3,r4,res		  : REAL8   ;

BEGIN
   dreij := 0 ;
   ier := 0 ;
   IF (a1 < 0 ) OR ( a2 < 0 ) OR ( a3 < 0 ) THEN BEGIN
      ier := 1 ;
      RETURN ;
   END ;
   j1 := round( 2 * a1 ) ;
   j2 := round( 2 * a2 ) ;
   j3 := round( 2 * a3 ) ;
   m1 := round( 2 * p1 ) ;
   m2 := round( 2 * p2 ) ;
   m3 := round( 2 * p3 ) ;
   n1 := ( j1 + j2 - j3 ) DIV 2 ;
   n2 := ( j1 + j3 - j2 ) DIV 2 ;
   n3 := ( j2 + j3 - j1 ) DIV 2 ;
   IF (j1 < abs(m1)) OR (j2 < abs(m2) ) OR (j3 < abs(m3)) THEN BEGIN
      ier := 4 ;
      RETURN ;
   END ;
   IF ( n1 < 0 ) OR (n2 < 0) OR (n3 < 0) THEN RETURN ;
   IF ( m1+m2+m3) <> 0 THEN RETURN ;
   k1 := (j1-j2-m3) DIV 2 ;
   IF odd(k1) THEN v := -1 ELSE v := 1 ;
   n4 := (j1+j2+j3) DIV 2 ;
   r2 := dd(n1,n2,n3,n4) ;
   l1 := (j1+m1) DIV 2 ;
   l2 := (j1-m1) DIV 2 ;
   l3 := (j2+m2) DIV 2 ;
   l4 := (j2-m2) DIV 2 ;
   l5 := (j3+m3) DIV 2 ;
   l6 := (j3-m3) DIV 2 ;
   r3 := fakult(l1) * fakult(l2) * fakult(l3) * fakult(l4) *
	 fakult(l5) * fakult(l6) ;
   r4 := sqrt( r2 * r3) ;
   k2 := (j2-j3-m1) DIV 2 ;
   k3 := ( j1-j3+m2) DIV 2;
   ke := n1 ;
   min(n1,l2,l3,ke) ;
   ka := k2 ;
   IF k3 > ka THEN ka := k3 ;
   IF (ke < ka) THEN BEGIN
      ier := 5 ;
      RETURN ;
   END ;
   IF ka < 0 THEN ka := 0 ;
   res := 0 ;
   FOR l := ka TO ke DO BEGIN
      r2 := fakult(l) * fakult(n1-l) * fakult( l2-l) * fakult(l3-l) *
	    fakult(l-k2) * fakult(l-k3) ;
      IF odd(l) THEN r2 := -r2 ;
      res := res + 1/r2 ;
   END ;
   dreij := res * r4 * v ;
END ;
(*--------------------------------------------------------------*)
FUNCTION zeeman ;

VAR z1,z2,z3	 : REAL8 ;
    aux 	 : REAL4 ;
    ip1,ip2,ier  : INTEGER ;

BEGIN
   IF (round(abs(f2-f1)) <= 1) THEN BEGIN
      z1 := dreij(f1,f2,1.0,xm,-xm,0.0,ier) * 1.3996108 *
	    sqrt((2*f1+1)*(2*f2+1)) ;
      z2 := sechsj(f1,f2,1.0,xj,xj,xi,ier) * sqrt(xj*(xj+1)*(2.0*xj+1)) ;
      z3 := sechsj(f1,f2,1.0,xi,xi,xj,ier) * sqrt(xi*(xi+1)*(2.0*xi+1)) ;
      aux := f1-xm+xj+xi+1.0 ;
      IF odd(round(aux+f2)) THEN ip1 := -1 ELSE ip1 := 1 ;
      IF odd(round(aux+f1)) THEN ip2 := -1 ELSE ip2 := 1 ;
      zeeman := z1 * ( z2 * ip1 * gj + z3 * ip2 * gi ) ;
   END ELSE zeeman := 0.0 ;
END ;
(*--------------------------------------------------------------*)
FUNCTION acasimir  ;
BEGIN
   acasimir := f*(f+1) - i*(i+1) - j*(j+1) ;
END ;
(*--------------------------------------------------------------*)
FUNCTION bcasimir ;
VAR aux : REAL4 ;
BEGIN
   IF (i > 0.7) AND ( j > 0.7) THEN BEGIN
      aux := acasimir(f,i,j ) ;
      aux := aux*(aux+1) * 0.75 - i*j*(i+1)*(j+1) ;
      bcasimir := aux / (2*i*j*(2*j-1)*(2*i-1)) ;
   END ELSE bcasimir := 0.0 ;
END ;
(*--------------------------------------------------------------*)
END.
