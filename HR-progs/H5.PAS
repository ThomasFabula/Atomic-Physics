(*$include: 'matrix.int' *)
(*$include: 'hfssub.int' *)

(*$debug- *)
(*$line- *)
(*$entry- *)

PROGRAM h5fit(input,output,inp,out) ;

USES matrix ;
USES hfssub ;

CONST parameter_max  = 2 ;
      werte_max      = 40 ;
      maxmatrix      = 10 ;

TYPE Kommentar_zeile = LSTRING(80) ;
     Datum_string    = STRING(8) ;
     parameter1      = ARRAY [1..parameter_max] OF LSTRING(1) ;
     quantenzahl     = ARRAY[1..werte_max] OF REAL4 ;

CONST fittype	     =	'H5 - Fit';
      parameter_name =	parameter1 ( 'A','B' ) ;

VAR   out,inp: TEXT ;

PROCEDURE date ( VAR x : STRING ) ;		      EXTERNAL ;

PROCEDURE time ( VAR x : STRING ) ;		      EXTERNAL ;

FUNCTION getmqq( wants : WORD) : adsmem ;	      EXTERNAL ;

PROCEDURE dismqq(block:adsmem) ;		      EXTERNAL ;

(*-------------------------------------------------------------------*)
PROCEDURE main ;

VAR  datum,time1	   : datum_string ;
     deriv		   : matrix_matrix (werte_max,parameter_max+1) ;
     kovarianz		   : matrix_matrix (parameter_max,parameter_max) ;
     y,dy,yy,dyy	   : matrix_vektor (werte_max ) ;
     f1,f2,m1,m2,h,dh	   : quantenzahl ;
     gi,gj,xi,xj	   : REAL4 ;
     p,pp		   : matrix_vektor (parameter_max+1) ;
     Kommentar		   : kommentar_zeile ;
     i,j,k,nit,r,n,ier	   : INTEGER ;
     iterations_max	   : INTEGER ;
     d,chi,chio 	   : REAL8 ;
(*-------------------------------------------------------------------*)

PROCEDURE leerzeilen(n	 : INTEGER ) ;

VAR i : INTEGER ;

BEGIN
   FOR i := 1 TO n DO writeln(out) ;
END ;
(*-------------------------------------------------------------------*)

PROCEDURE writechar( c : CHAR ; n : INTEGER ) ;

VAR i : INTEGER ;
BEGIN
   FOR i := 1 TO n DO write(out,c:1) ;
   writeln(out) ;
END ;
(*-------------------------------------------------------------------*)

FUNCTION ycalc (       n,r : INTEGER ;
		CONST  der : matrix_matrix ;
		CONST  p   : matrix_vektor ) : REAL8 ;

VAR i : INTEGER ;
    aux : REAL8 ;

BEGIN
   aux := 0.0 ;
   FOR i := 1 TO r DO aux := aux + der[n,i] * p[i] ;
   aux := aux + der[n,parameter_max+1] * p[parameter_max+1] ;
   ycalc := aux ;
END ;


PROCEDURE readdata ( VAR y,dy		   : matrix_vektor ;
		     VAR f1,m1,f2,m2,h,dh  : quantenzahl ;
		     VAR aa,bb		   : REAL8 ;
		     VAR gi,gj,xi,xj	   : REAL4 ;
		     VAR n,itmax	   : INTEGER  ;
		     VAR kommentar	   : Kommentar_zeile ) ;
var c : char ;

BEGIN
   writeln(out,'Es wurden folgende Daten Eingegeben :') ;
   writechar('-',72) ;
   leerzeilen(1) ;
   readln(inp,itmax,kommentar) ;
   writeln(out,' ',kommentar) ;
   readln(inp,aa,bb,gi,gj,xi,xj) ;
   writeln(out,aa:13:6,bb:13:6,gi:13:6,gj:13:6,xi:10:1,xj:10:1) ;
   n := 1 ;
   c := ' ';
   WHILE (NOT eof(inp)) AND (n < werte_max) AND ( c = ' ') DO BEGIN
      readln(inp,c,f1[n],m1[n],f2[n],m2[n],h[n],dh[n],y[n],dy[n] ) ;
      writeln(out,f1[n]:4:1,m1[n]:6:1,f2[n]:6:1,m2[n]:6:1,h[n]:12:4,
		  dh[n]:12:4,y[n]:14:6,dy[n]:12:6) ;
      IF abs(dy[n]) < 1e-10 THEN dy[n] := 1.0 ;
      n := n+1 ;
   END ;
   n := n-1 ;
   leerzeilen(3) ;
END ;

(*------------------------------------------------------------------*)
PROCEDURE get_derivlevel(VAR	ac,bc,hc		 : REAL8 ;
			 CONST	f,m,xi,xj,gi,gj,h	 : REAL4 ;
			 CONST	aa,bb			 : REAL8 ) ;

VAR i,j,k,nevek,ne    : INTEGER ;
    fmax,fmin,f1,f2   : REAL4 ;
    aux 	      : REAL8 ;
    hcoef,energie     : ADS OF matrix_matrix(maxmatrix,maxmatrix) ;
    energie_vek       : matrix_vektor(maxmatrix) ;

   FUNCTION suchsort(nsuch,n : INTEGER ) : INTEGER ;

   VAR i,help	: INTEGER ;
       index	: ARRAY[1..maxmatrix] OF INTEGER ;
       element	: REAL8 ;
       feld	: ARRAY[1..maxmatrix] OF REAL8 ;

      PROCEDURE quick( lg,rg : INTEGER ) ;

      VAR left,right : INTEGER ;

      BEGIN
	 IF lg < rg THEN BEGIN
	    left := lg ;
	    right := rg ;
	    help := index[(lg+rg) DIV 2] ;
	    element := energie_vek[help] ;
	    WHILE left < right DO BEGIN
	       WHILE energie_vek[index[left]] < element DO
		     left := left + 1 ;
	       WHILE energie_vek[index[right]] > element DO
		     right := right - 1 ;
	       IF right >= left THEN BEGIN
		  help := index[left] ;
		  index[left] := index[right] ;
		  index[right] := help ;
		  left := left + 1 ;
		  right := right - 1 ;
	       END ;
	    END ;
	    quick(lg,right) ;
	    quick(left,rg) ;
	 END ;
      END;

   BEGIN
      FOR i := 1 TO n DO index[i] := i ;
      quick(1,n) ;
      suchsort := index[nsuch] ;
   END ;
(*-----------------------------------------------------------------*)

BEGIN
   hcoef := getmqq(SIZEOF (hcoef^)) ;
   energie := getmqq(SIZEOF(energie^)) ;
   fmax := xi + xj ;
   fmin := abs(xi-xj) ;
   IF abs(m) > fmin THEN ne := round(fmax - abs(m))
		    ELSE ne := round(fmax - fmin) ;
   ne := ne+1 ;
   nevek := round(fmax - f) + 1;
   FOR i := 1 TO ne DO FOR j := 1 TO ne DO BEGIN
      hcoef^[i,j] := 0.0 ;
      energie^[i,j] := 0.0 ;
   END ;
   FOR k := 1 TO ne DO BEGIN
      f1 := fmax - k+1 ;
      energie^[k,k] := acasimir(f1,xi,xj) / 2.0  * aa +
		       bcasimir(f1,xi,xj) * bb ;
   END ;
   aux := energie^[nevek,nevek] ;
   k := 0 ;
   FOR i := 1 TO ne DO IF energie^[i,i] < aux THEN k := k +1 ;
   nevek := k+1 ;
   FOR i := 1 TO ne DO BEGIN
      f1 := fmax-i+1 ;
      FOR j := i TO i+1 DO IF j <= ne THEN BEGIN
	 f2 := fmax-j+1 ;
	 hcoef^[i,j]   := zeeman(f1,f2,xi,xj,m,gj,gi) ;
	 energie^[i,j] := energie^[i,j] + hcoef^[i,j] * h ;
	 energie^[j,i] := energie^[i,j] ;
	 hcoef^[j,i]   := hcoef^[i,j] ;
      END;
   END ;
   eigen(energie^,energie_vek,ne) ;
   nevek := suchsort(nevek,ne) ;
   ac := 0.0 ;
   bc := 0.0 ;
   hc := 0.0 ;
   FOR i := 1 TO ne DO BEGIN
      f1 := fmax - i+1 ;
      aux := sqr(energie^[i,nevek]) ;
      ac := ac + aux * acasimir(f1,xi,xj) / 2.0 ;
      bc := bc + aux * bcasimir(f1,xi,xj) ;
      FOR j := 1 TO ne DO
	 hc := hc + energie^[i,nevek]*hcoef^[i,j]
		  * energie^[j,nevek] ;
   END ;
   dismqq(energie) ;
   dismqq(hcoef) ;
END ;
(*-----------------------------------------------------------------*)

PROCEDURE get_deriv( CONST  f1,m1,f2,m2,h,dh	     : quantenzahl ;
		     CONST  xi,xj,gii,gj	     : REAL4  ;
		     CONST  aa,bb		     : REAL8  ;
		     VARS   deriv		     : matrix_matrix ;
		     CONST  dy,y		     : matrix_vektor ;
		     VAR    dyy,yy		     : matrix_vektor ;
			    n			     : INTEGER ;
		     VAR    r			     : INTEGER ) ;

VAR ac,bc,hc,aco,bco,hco,yx : REAL8 ;
    gi			    : REAL4 ;
    i,v 		    : INTEGER ;

BEGIN
   IF (xi < 1) OR ( xj < 1) THEN r := 1
			    ELSE r := 2 ;
   gi := gii *1.0e-4 ;
   FOR i := 1 TO n DO BEGIN
      get_derivlevel(aco,bco,hco,f1[i],m1[i],xi,xj,gi,gj,h[i],aa,bb) ;
      get_derivlevel(ac,bc,hc,f2[i],m2[i],xi,xj,gi,gj,h[i],aa,bb) ;
      ac := ac-aco ;
      bc := bc-bco ;
      hc := hc-hco ;
      yx := ac * aa + bc * bb + hc * h[i] ;
      IF yx < 0 THEN v := -1 ELSE v := 1 ;
      yy[i] := y[i] - yx * v ;
      deriv[i,1] := v * ac ;
      deriv[i,2] := v * bc ;
      deriv[i,parameter_max+1] := v * hc ;
      dyy[i] := sqrt( sqr(dy[i]) + sqr(hc * dh[i]) ) ;
   END ;
END ;

PROCEDURE writeout (	    r,n 	    : INTEGER ;
		     CONST  p		    : matrix_vektor ;
		     CONST  kovarianz,deriv : matrix_matrix ) ;

VAR i,j 	: INTEGER ;
    korrelation : matrix_matrix(parameter_max,parameter_max) ;
    aux 	: REAL8 ;

BEGIN
   leerzeilen(3) ;
   writeln(out,'Es wurde ein ',fittype,' angepasst.') ;
   writechar('=',72) ;
   leerzeilen(2) ;
   writeln(out,kommentar) ;
   leerzeilen(2) ;
   IF (n-r) >0 THEN aux := sqrt(chi/(n-r))
	       ELSE aux := 1.0 ;
   writeln(out,'Iterationszahl =', nit );
   writeln(out,'Chi-Quadrat    = ',chi : 20:6 ) ;
   writeln(out,'rms-Faktor     = ',aux : 20:6 ) ;
   leerzeilen(2) ;
   writeln(out,'Es wurden folgende Parameter bestimmt : ');
   writechar('-',40) ;
   writeln(out) ;
   FOR i := 1 TO r DO
      writeln(out,parameter_name[i] ,' = ',
		  pp[i] :20:6,' +/- ',sqrt(abs(kovarianz[i,i])):20:6) ;
   leerzeilen(3) ;
   writeln(out,'  f1   m1   f2   m2      H','y':11,'dy':12,'ycalc':12,
	       'y-ycalc':14) ;
   writechar('-',78) ;
   writeln(out) ;
   FOR i := 1 TO n DO BEGIN
      pp[parameter_max+1] := h[i] ;
      aux := ycalc(i,r,deriv,pp) ;
      writeln(out,f1[i]:4:1,m1[i]:5:1,f2[i]:5:1,m2[i]:5:1,h[i]:10:4,
		  y[i]:12:4,dyy[i]:10:4,aux:12:4,(y[i]-aux):11:4) ;
   END ;
   leerzeilen (5) ;
   writeln(out,chr(12),'Ausgabe der Kovarianzmatrix     : ');
   writechar( '-', 40 ) ;
   matout(kovarianz,r,r,4,0,out) ;
   leerzeilen(2) ;
   FOR i := 1 TO r DO FOR j := 1 TO r DO korrelation[i,j] :=
      kovarianz[i,j] / sqrt(abs(kovarianz[i,i] * kovarianz[j,j])) ;
   writeln(out,'Ausgabe der Korrelationsmatrix : ');
   writechar( '-', 40 ) ;
   matout(korrelation,r,r,4,0,out) ;
END ;
(*-------------------------------------------------------------------*)
PROCEDURE make_kopf( VAR out  : TEXT) ;

BEGIN
   Writeln(out,'<********************************* H 5 ***************',
	       '*****************>') ;
   writeln(out) ;
   writeln(out) ;
   writeln(out,'Prozess Begin am ',datum,' um ',time1,' Uhr') ;
END ;
(*-------------------------------------------------------------------*)
procedure write_iteration ( var out :text ) ;

begin
   writeln(out,'nr.',nit:3,'chi = ':12,chi:15,'A =':8,pp[1]:12:4,
	       'B =':8,pp[2]:12:4) ;
end ;
(*-------------------------------------------------------------------*)
procedure write_endtime(var out : text ) ;
begin
   writeln(out,'Prozess Ende um ',time1,' Uhr') ;
   writeln(out,chr(12) ) ;
end ;
(*-------------------------------------------------------------------*)
BEGIN
   rewrite(out) ;
   reset (inp) ;
   WHILE NOT eof(inp) DO BEGIN
      date(datum) ;
      time(time1) ;
      make_kopf(output) ;
      make_kopf(out) ;
      For i := 1 to parameter_max do for j := 1 to parameter_max do
	 kovarianz[i,j] := 0.0 ;
      for i := 1 to parameter_max+1 do begin
	 p[i] := 0.0 ;
	 pp[i] := 0.0 ;
	 for j := 1 to werte_max do deriv[j,i] := 0.0 ;
      end ;
      readdata(y,dy,f1,m1,f2,m2,h,dh,pp[1],pp[2],gi,gj,xi,xj,n,
	       iterations_max,kommentar) ;
      nit := 0 ;
      chi := 0.0 ;
      REPEAT
	 get_deriv(f1,m1,f2,m2,h,dh,xi,xj,gi,gj,pp[1],pp[2],deriv,
		   dy,y,dyy,yy,n,r) ;
	 chio := chi ;
	 leasts(deriv,dyy,yy,n,r,kovarianz,pp,p,chi,ier,out) ;
	 nit := nit + 1 ;
	 write_iteration(out) ;
	 write_iteration(output) ;
	 IF ier <> 0 THEN BEGIN
	    chi := 0 ;
	    BREAK ;
	 END ;
      UNTIL (nit > iterations_max) OR (abs(chio-chi) < 1e-5 * chi+1e-5) ;
      writeout(r,n,pp,kovarianz,deriv) ;
      time(time1) ;
      write_endtime(out) ;
      write_endtime(output) ;
   END ;
   close(inp) ;
   close(out) ;
END  ;

begin
   main ;
end.
