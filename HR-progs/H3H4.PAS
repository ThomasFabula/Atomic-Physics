
(* ************************    H 3 H 4	 ***************************** *)

(*  Version vom 27.1.86 *)

(*$include: 'hfssub.int' *)
(*$include: 'matrix.int' *)

PROGRAM h3h4(input,output,inp) ;

USES hfssub ;
USES matrix ;

(*$debug- *)
(*$entry- *)
(*$line- *)

VAR inp : TEXT ;

FUNCTION getmqq( wants : WORD) : adsmem ;	      EXTERNAL ;

PROCEDURE dismqq(block:adsmem) ;		      EXTERNAL ;
(*------------------------------------------------------------------*)
PROCEDURE main ;

   CONST maxmatrix = 10 ;

   VAR xj,xi,gi,gj,h,aa,bb,daa,dbb,dh,h1,h2   : REAL4 ;
       v				      : INTEGER ;
       ac,bc,hc,aco,bco,hco,auxe,aux	      : REAL8 ;
       f,m				      : ARRAY[1..2] OF REAL4 ;
       comment				      : LSTRING(80) ;

(*------------------------------------------------------------------*)
   PROCEDURE get_deriv(VAR ac,bc,hc		    : REAL8 ;
		       VAR f,m,xi,xj,gi,gj,aa,bb,h  : REAL4 ) ;

   VAR i,j,k,nevek,ne	 : INTEGER ;
       fmax,fmin,f1,f2	 : REAL4 ;
       aux		 : REAL8 ;
       hcoef,energie	 : ADS OF matrix_matrix(maxmatrix,maxmatrix) ;
       energie_vek	 : matrix_vektor(maxmatrix) ;

      FUNCTION suchsort(nsuch,n : INTEGER ) : INTEGER ;

      VAR i,help   : INTEGER ;
	  index    : ARRAY[1..maxmatrix] OF INTEGER ;
	  element  : REAL8 ;
	  feld	   : ARRAY[1..maxmatrix] OF REAL8 ;

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
(*------------------------------------------------------------------*)

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
      FOR i := 1 TO ne DO FOR j := i TO i+1 DO BEGIN
	 f1 := fmax-i+1 ;
	 f2 := fmax-j+1 ;
	 IF j <= ne THEN BEGIN
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
(*------------------------------------------------------------------*)
BEGIN
   reset(inp) ;
   WHILE NOT eof(inp) DO BEGIN
      readln(inp,comment) ;
      readln(inp,f[1],m[1],f[2],m[2],gj,gi,xj,xi) ;
      readln(inp,aa,daa,bb,dbb,h1,h2,dh) ;
      writeln(output,comment:-80) ;
      writeln(output,'šbergang : (',f[1]:3:1,'/',m[1]:4:1,')-(',
	   f[2]:3:1,'/',m[2]:4:1,')','I  =':13,xi:4:1,'J  =':12,
	   xj:4:1) ;
      writeln(output) ;
      writeln(output,'A  =':6,aa:13:5,' +/-',daa:9:5,' MHz     B  =',
	   bb:13:5,' +/-',dbb:9:5,' MHz') ;
      writeln(output) ;
      writeln(output,'gj =':6,gj:13:5,'gi =':26,gi:13:5) ;
      writeln(output) ;
      writeln(output) ;
      writeln(output,'Feld(Gauss)           Frequenz(MHz)',
	     '          dF/dA      dF/dB      dF/dH') ;
      writeln(output) ;
      gi := gi * 1.0e-4 ;
      h := h1 ;
      REPEAT
	 get_deriv(aco,bco,hco,f[1],m[1],xi,xj,gi,gj,aa,bb,h) ;
	 get_deriv(ac,bc,hc,f[2],m[2],xi,xj,gi,gj,aa,bb,h) ;
	 ac  := ac - aco ;
	 bc  := bc - bco ;
	 hc  := hc - hco ;
	 aux  := ac * aa + bc * bb + hc * h ;
	 auxe := sqrt( sqr(ac * daa) + sqr(bc * dbb) ) ;
	 IF aux < 0 THEN v := -1 ELSE v := 1 ;
	 writeln(output,h:10:4,(v*aux):15:6,' +/-',auxe:10:6,
	     (v*ac):13:6,(v*bc):11:6,(v*hc):11:6) ;
	 writeln(output) ;
	 h := h + dh ;
      UNTIL h > h2 ;
      writeln(output) ;
      writeln(output) ;
      writeln(output) ;
   END ;
END ;

BEGIN
   main ;
END .
