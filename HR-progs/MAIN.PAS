(* $pagesize:62 *)
(* $linesize:95 *)
(* $include : 'c:\-ibmpas\grafmod.int'*)
(* $include : 'c:\-ibmpas\ibmclock.int' *)

program test(input,output,n) ;

uses grafmod ;
uses ibmclock ;

CONST   maxwindows = 4 ;
        maxpoints  = 600 ;

VAR hr,min,sec,hund            : INTEGER ;
    start,ende,x,y,x1,y1       : REAL8 ;
    i,j,k,n,ix,iy,ix1,ix2      : INTEGER ;
    window                     : ARRAY [1..maxwindows] OF window_type ;
    scal                       : ARRAY [1..maxwindows] OF windowpar_type ;
    xvek,yvek ,yvek1,yvek2     : real_vektor(maxpoints) ;
    valid_keys                 : set_of_byte ;
    taste                      : byte ;

(*-------------------------------------------------------------------*)
(* $include : '\-ibmpas\grextrn.inc' *)
(*-------------------------------------------------------------------*)
PROCEDURE wait ;
VAR taste : BYTE ;
    flag : BOOLEAN ;
BEGIN
   flag := get_key(taste) ;
END ;

(*-------------------------------------------------------------------*)
BEGIN
   valid_keys := [ ] ;
   init_grafik ;
   clear_gscreen ;
   gettim(hr,min,sec,hund) ;
   start := ( hr*60+min) * 60 + sec + hund/100 ;
   axis( 0,0,719,171,6,6,0.0,100.0,-1.0,1.0,'Ueberschrift fuer Graphik',
         'Bild Unterschrift') ;
   query_window(window[1],scal[1]) ;
   axis( 0,172,719,347,6,6,0.0,100.0,-100.0,100.0,'Ueberschrift fuer Graphik',
         'Bild Unterschrift') ;
   query_window(window[2],scal[2]) ;
   rasterop(replace) ;
   FOR i := 0 TO 599 DO BEGIN
      x := i / 599.0 * 100.0 ;
      xvek[i+1] := x ;
      yvek[i+1] := sin(x)*x ;
      yvek1[i+1] := cos(x)*exp(-x/100);
   END ;
   FOR k := 0 TO n DO BEGIN
      set_window(window[1],scal[1]) ;
      plot_vektor(xvek,yvek1,600,invert,true ) ;
      set_window(window[2],scal[2]) ;
      plot_vektor(xvek,yvek,600,invert,false) ;
   END ;
   gettim(hr,min,sec,hund) ;
   ende := ( hr*60+min) * 60 + sec + hund/100 ;
   init_cursor(0,0) ;
   center_cursor ;
   if cursor(taste,valid_keys) then begin
      query_cursor(ix,iy) ;
      user_koordinaten(x,y,ix,iy) ;
   end else beep ;
   init_cursor(0,0) ;
   center_cursor ;
   if cursor(taste,valid_keys) then begin
      query_cursor(ix,iy) ;
      user_koordinaten(x1,y1,ix,iy) ;
   end else beep ;
   axis( 0,0,719,347,6,6,x,x1,y,y1,'Ueberschrift fuer Graphik',
         'Bild Unterschrift') ;
   query_window(window[1],scal[1]) ;
   plot_vektor(xvek,yvek,600,invert,true ) ;
   rasterop(invert) ;
   wait ;
   setdisplay(textmode,textmode,textmode) ;
   writeln(output,'Zeit fr test  = ',(ende-start) :10:2,' sec') ;
END.
