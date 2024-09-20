(*$include: 'matrix.int' *)

(*$debug- *)
(*$line- *)
(*$entry- *)

PROGRAM fit(input,output,inp,out) ;

USES matrix ;

CONST parameter_max  = 4 ;
      werte_max      = 100 ;
      iterations_max = 20 ;
      fitmax	     = 9 ;

TYPE Kommentar_zeile = LSTRING(80) ;
     Datum_string    = STRING(8) ;
     fitstring	     = ARRAY [0..fitmax] OF LSTRING(20) ;
     parameter1      = ARRAY [1..parameter_max] OF LSTRING(3) ;
     parameter	     = ARRAY [0..fitmax] OF parameter1 ;

CONST fittype = fitstring ( 'Mittelwert','Gerade','Quadratisches Poly.',
			    'Kubisches Polynom','Gausskurve',
			    'Lorentzkurve','Gausskurve/Unt',
			    'Lorentzkurve/Un','c + a/(x-x0)',
			    'Exponentialfunktion') ;
     parameter_name  =	parameter ( parameter1('M',' ',' ',' '),
				    parameter1('c0','c1',' ',' '),
				    parameter1('c0','c1','c2',' '),
				    parameter1('c0','c1','c2','c3'),
				    parameter1('x0','Sig','Amp',' '),
				    parameter1('x0','Hwb','Amp',' ' ),
				    parameter1('x0','Sig','Amp','Unt' ),
				    parameter1('x0','Hwb','Amp','Unt'),
				    parameter1('C','Amp','x0',' '),
				    parameter1('C','Amp','E',' '));
VAR  out,inp : TEXT ;

PROCEDURE date ( VAR x : STRING ) ; EXTERNAL ;

PROCEDURE time ( VAR x : STRING ) ; EXTERNAL ;
(*------------------------------------------------------------------*)
PROCEDURE main ;

VAR  datum,time1	   : datum_string ;
     deriv		   : matrix_matrix (werte_max,parameter_max) ;
     kovarianz		   : matrix_matrix (parameter_max,parameter_max) ;
     x,y,dy,yy		   : matrix_vektor (werte_max ) ;
     p,pp		   : matrix_vektor (parameter_max ) ;
     Kommentar		   : kommentar_zeile ;
     i,j,k,nit,r,n,ier,fit : INTEGER ;
     d,chi,chio 	   : REAL8 ;
     iterations_flag	   : BOOLEAN ;

(*------------------------------------------------------------------*)
PROCEDURE leerzeilen(n	 : INTEGER ) ;
VAR i : INTEGER ;
BEGIN
   FOR i := 1 TO n DO writeln(out) ;
END ;
(*------------------------------------------------------------------*)
PROCEDURE writechar( c : CHAR ; n : INTEGER ) ;
VAR i : INTEGER ;
BEGIN
   FOR i := 1 TO n DO write(out,c:1) ;
   writeln(out) ;
END ;
(*------------------------------------------------------------------*)
FUNCTION ycalc (       x   : REAL8 ;
		       fit : INTEGER ;
		CONSTS p   : matrix_vektor ) : REAL8 ;

VAR i,r      : INTEGER ;
    aux,auxy : REAL8 ;

BEGIN
   CASE fit OF
      0..3     :  BEGIN
		     r := fit + 1 ;
		     aux := p[r] ;
		     FOR i := r-1 DOWNTO 1 DO aux := aux * x + p[i] ;
		     ycalc := aux ;
		  END ;
      4,6      :  BEGIN
		     aux := (x-p[1]) / p[2] ;
		     auxy := 0.0 ;
		     IF abs(aux) < 10.0 THEN
				 auxy := p[3] / exp(aux*aux /2.0 ) ;
		     IF fit = 6 THEN auxy := auxy + p[4] ;
		     ycalc := auxy ;
		  END ;
      5,7      :  BEGIN
		     aux := (x-p[1]) / p[2] ;
		     auxy := p[3] / (4.0 * aux * aux + 1) ;
		     IF fit = 7 THEN auxy := auxy + p[4] ;
		     ycalc := auxy ;
		  END ;
      8        :  ycalc := p[1] + p[2] /(x-p[3]) ;
      9        :  ycalc := p[1] + p[2] * exp( p[3] * x) ;
   END ; (* Case End *)
END ;
(*------------------------------------------------------------------*)
PROCEDURE startwerte ( CONSTS x,y : matrix_vektor ;
			      fit : INTEGER ;
		       VARS   p   : matrix_vektor ) ;

VAR i,j,r	      : INTEGER ;
    aux,h0,h1,h2,h3   : REAL8	;

BEGIN
   CASE fit OF
      0..3   :	  BEGIN
		     r := fit + 1 ;
		     FOR i := 1 TO r DO p[i] := 0 ;
		  END ;
      4..7   :	  BEGIN
		     h0 := y[i] ;
		     h1 := 0 ;
		     h2 := 0 ;
		     h3 := 0 ;
		     FOR i := 1 TO n DO BEGIN
			IF abs(y[i]) > abs(h0) THEN h0 := y[i] ;
			h1 := h1 + y[i] ;
			h2 := h2 + x[i] * y[i] ;
			h3 := h3 + x[i] * x[i] * y[i] ;
		     END ;
		     IF fit IN [5,7] THEN p[4] := 0 ;
		     p[3] := h0 ;
		     p[1] := h2 / h1 ;
		     p[2] := sqrt( abs( h3/h1 - p[1]*p[1] )) ;
		  END ;
      8      :	  BEGIN
		     p[1] := 0 ;
		     p[2] := y[1]*x[1] ;
		     p[3] := 0 ;
		  end ;
      9      :	  begin
		     p[1] := 0 ;
		     p[3] := ln(y[1] / y[2]) / (x[1] - x[2]) ;
		     p[2] := y[1] / exp(p[3] * x[1]) ;
		  end ;
   END ;
END ;
(*------------------------------------------------------------------*)
PROCEDURE readdata ( VAR x,y,dy    : matrix_vektor ;
		     VAR n,fit	   : INTEGER  ;
		     VAR kommentar : Kommentar_zeile ) ;
VAR c : CHAR ;

BEGIN
   writeln(out,'Es wurden folgende Daten Eingegeben :') ;
   writechar('-',72) ;
   leerzeilen(1) ;
   readln(inp,fit,kommentar) ;
   writeln(out,fit:3,' ',kommentar) ;
   n := 1 ;
   c := ' ';
   WHILE (NOT eof(inp)) AND (n < werte_max) AND ( c = ' ') DO BEGIN
      readln(inp,c,x[n],y[n],dy[n]) ;
      writeln(out,x[n]:24:6,y[n]:24:6,dy[n]:24:6);
      IF abs(dy[n]) < 1e-10 THEN dy[n] := 1.0 ;
      n := n+1 ;
   END ;
   n := n-1 ;
   leerzeilen(3) ;
END ;
(*------------------------------------------------------------------*)
PROCEDURE Get_deriv ( VAR    deriv   : matrix_matrix ;
		      CONSTS x,p     : matrix_vektor ;
		      VAR    r	     : INTEGER	     ;
			     n,fit   : INTEGER ) ;

VAR i,j  : INTEGER ;
    aux  : REAL8 ;

BEGIN
   CASE fit OF
      0..3   :	  BEGIN    (* Polynom *)
		     r := fit + 1 ;
		     FOR i := 1 TO n DO BEGIN
			aux := 1 ;
			FOR j := 1 TO r DO BEGIN
			   deriv[i,j] := aux ;
			   aux := aux * x[i] ;
			END ;
		     END ;
		  END ;
      4,6    :	  BEGIN   (* Gausskurve *)
		     IF fit = 4 THEN r := 3
				ELSE r := 4 ;
		     FOR i := 1 TO n DO BEGIN
			aux := ycalc(x[i],fit,p) ;
			IF fit = 6 THEN BEGIN
			   deriv[i,4] := 1.0 ;
			   aux := aux - p[4] ;
			END ;
			deriv[i,3] := aux / p[3] ;
			deriv[i,1] := aux * (x[i]-p[1]) / p[2] / p[2] ;
			deriv[i,2] := deriv[i,1] * (x[i]-p[1]) / p[2] ;
		     END ;
		  END ;
      5,7    :	  BEGIN   (* Lorentzkurve *)
		     IF fit = 5 THEN r := 3
				ELSE r := 4 ;
		     FOR i := 1 TO n DO BEGIN
			aux := ycalc(x[i],fit,p) ;
			IF fit = 7 THEN BEGIN
			   deriv[i,4] := 1.0 ;
			   aux := aux - p[4] ;
			END ;
			deriv[i,3] := aux / p[3] ;
			deriv[i,1] := aux * (x[i]-p[1]) / p[2] / p[2] ;
			deriv[i,1] := 8 * deriv[i,1] * deriv[i,3] ;
			deriv[i,2] := deriv[i,1] * (x[i]-p[1]) / p[2] ;
		     END ;
		  END ;
      8      :	  BEGIN  (* c + a/(x-x0) *)
		     r := 3 ;
		     for i := 1 to n do begin
			aux := x[i] - p[3] ;
			deriv[i,1] := 1 ;
			deriv[i,2] := 1 / aux ;
			deriv[i,3] := p[2] / (aux * aux) ;
		     end ;
		  end ;
      9     :	  begin  (* Exponentialfunktion *)
		     r := 3 ;
		     for i := 1 to n do begin
			aux := ycalc(x[i],fit,p) - p[1] ;
			deriv[i,1] := 1 ;
			deriv[i,2] := aux / p[2] ;
			deriv[i,3] := aux  * x[i] ;
		     end ;
		  end ;
   END ; (* End Case *)
END;
(*------------------------------------------------------------------*)
PROCEDURE writeout (	    r,n,fit   : INTEGER ;
		     CONSTS p	      : matrix_vektor ;
		     CONSTS kovarianz : matrix_matrix ) ;

VAR i,j 	: INTEGER ;
    korrelation : matrix_matrix(parameter_max,parameter_max) ;
    aux 	: REAL8 ;

BEGIN
   leerzeilen(5) ;
   writeln(out,'es wurde ein(e) ',fittype[fit],' angepasst') ;
   writechar('=',72) ;
   leerzeilen(2) ;
   writeln(out,kommentar) ;
   leerzeilen(2) ;
   IF (n-r) >0 THEN aux := sqrt(chi/(n-r))
	       ELSE aux := 1.0 ;
   writeln(out,'iterationszahl =', nit );
   writeln(out,'chi            = ',chi : 20:6 ) ;
   writeln(out,'rms-Faktor     = ',aux : 20:6 ) ;
   leerzeilen(2) ;
   writeln(out,'Es wurden folgende Parameter bestimmt : ');
   writechar('-',40) ;
   writeln(out) ;
   FOR i := 1 TO r DO
      writeln(out,parameter_name[fit , i] :-4,' = ',
		  pp[i] :20:6,' +/- ',sqrt(abs(kovarianz[i,i])):20:6) ;
   leerzeilen(3) ;
   writeln(out,'x':9,'y':14,'dy':14,'ycalc':16,'y-ycalc':14) ;
   writechar('-',72) ;
   writeln(out) ;
   FOR i := 1 TO n DO BEGIN
      aux := ycalc(x[i],fit,pp) ;
      writeln(out,x[i]:14:5,y[i]:14:5,dy[i]:14:5,aux:14:5,(y[i]-aux):14:5) ;
   END ;
   leerzeilen (5) ;
   writeln(out,CHR(12),'Ausgabe der Kovarianzmatrix     : ');
   writechar( '-', 40 ) ;
   matout(kovarianz,r,r,4,0,out) ;
   leerzeilen(2) ;
   FOR i := 1 TO r DO FOR j := 1 TO r DO korrelation[i,j] :=
      kovarianz[i,j] / sqrt(abs(kovarianz[i,i] * kovarianz[j,j])) ;
   writeln(out,'Ausgabe der Korrelationsmatrix : ');
   writechar( '-', 40 ) ;
   if chi <> 0.0 then matout(korrelation,r,r,4,0,out) ;
END ;
(*-------------------------------------------------------------------*)
PROCEDURE make_kopf( VAR out  : TEXT) ;

BEGIN
   writeln(out,'<****************************  L S Q F I T ***********',
	       '*****************>') ;
   writeln(out) ;
   writeln(out) ;
   writeln(out,'Prozess Begin am ',datum,' um ',time1,' Uhr') ;
END ;
(*-------------------------------------------------------------------*)
PROCEDURE write_iteration ( VAR out :text ) ;

BEGIN
   writeln(out,'nr.',nit:3,'chi = ':12,chi:15) ;
END ;
(*-------------------------------------------------------------------*)
PROCEDURE write_endtime(VAR out : TEXT ) ;
BEGIN
   writeln(out,'Prozess Ende um ',time1,' Uhr') ;
   writeln(out,chr(12) ) ;
END ;
(*-------------------------------------------------------------------*)
BEGIN
   reset(inp) ;
   rewrite(out) ;
   WHILE NOT eof(inp) DO BEGIN
      date(datum) ;
      time(time1) ;
      make_kopf(output) ;
      make_kopf(out) ;
      readdata(x,y,dy,n,fit,kommentar) ;
      IF fit IN [0..3] THEN iterations_flag := FALSE
		       ELSE iterations_flag := TRUE ;
      startwerte(x,y,fit,pp) ;
      nit := 0 ;
      chi := 1e300 ;
      REPEAT
	 get_deriv(deriv,x,pp,r,n,fit) ;
	 IF iterations_flag THEN
		 FOR i := 1 TO n DO yy[i] := y[i] - ycalc(x[i],fit,pp)
		 ELSE yy := y ;
	 chio := chi ;
	 leasts(deriv,dy,yy,n,r,kovarianz,pp,p,chi,ier,out) ;
	 nit := nit + 1 ;
	 write_iteration(out) ;
	 write_iteration(output) ;
	 IF ier <> 0 THEN BEGIN
	    chi := 0 ;
	    BREAK ;
	 END ;
      UNTIL (nit > iterations_max) OR (abs(chio-chi) < 1e-5 * chi+1e-5) OR
	    ( NOT iterations_flag) ;
      writeout(r,n,fit,pp,kovarianz) ;
      time(time1) ;
      write_endtime(output) ;
      write_endtime(out) ;
   END ;
   close(out) ;
   close(inp) ;
END ;

BEGIN
   main ;
END .
