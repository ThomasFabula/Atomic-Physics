(* $pagesize:62 *)
(* $linesize:95 *)
(* $include : '\-horst\testgraf\grafmod.int' *)

IMPLEMENTATION OF grafmod ;

CONST deltacur       = 10 ;
      strlen         =  5 ;

TYPE manstring  = LSTRING(STRLEN) ;
     expstring  = LSTRING(8) ;

VAR default_fill,default_raster   : INTEGER ;
    xfak,xoffs,yfak,yoffs         : REAL8   ;
    act_window                    : window_type ;
    xcur,ycur,delta_cursor        : INTEGER ;
    scr_xmax,scr_ymax,graf_mode   : INTEGER ;

(*-------------------------------------------------------------------*)
(* Definition von Externen Prozeduren                                *)
(*-------------------------------------------------------------------*)
PROCEDURE endxqq ;  EXTERNAL ;

FUNCTION pidrqq(CONSTS p:real8 ;CONSTS i:integer4 ) : REAL8 ; EXTERNAL;

FUNCTION lddrqq(CONSTS p : REAL8 ) : REAL8 ; EXTERNAL ;

FUNCTION dosxqq(commnad : BYTE ; parm : WORD ) : BYTE ; EXTERNAL ;

(* $include: 'c:\-ibmpas\grextrn.inc' *)

(*-------------------------------------------------------------------*)
PROCEDURE beep ;
VAR result : BYTE ;
BEGIN
   result := dosxqq(2,7) ;
END ;
(*-------------------------------------------------------------------*)
FUNCTION get_key ;

BEGIN
   taste := dosxqq(7,0) ;
   IF taste = 0 THEN BEGIN
      taste := dosxqq(7,0) ;
      get_key := TRUE ;
   END ELSE get_key := FALSE ;
END ;
(*-------------------------------------------------------------------*)
(* Zeichnen eines Punktes                                            *)
(*-------------------------------------------------------------------*)
PROCEDURE plot ;
BEGIN
   moveto(x,y) ;
   lineto(x,y) ;
END ;

(*-------------------------------------------------------------------*)
(* Zeichnen einer Linie                                              *)
(*-------------------------------------------------------------------*)
PROCEDURE draw_line ;
BEGIN
   moveto(x1,y1) ;
   lineto(x2,y2) ;
END ;

(*-------------------------------------------------------------------*)
(* Die Procedure view_act_window schaltet auf das aktuelle Fenster um*)
(*-------------------------------------------------------------------*)
PROCEDURE view_act_window ;
BEGIN
   viewport(act_window[1,1],act_window[2,1],
            act_window[1,2],act_window[2,2]) ;
END ;

(*-------------------------------------------------------------------*)
(* Löschen der Skalierungs - Faktoren des aktuellen Windows          *)
(*-------------------------------------------------------------------*)
PROCEDURE clear_faktoren ;
BEGIN
    xfak := 1.0 ;
    yfak := 1.0 ;
    xoffs := 0.0 ;
    yoffs := 0.0 ;
END ;

(*-------------------------------------------------------------------*)
(* Definition des aktuellen Fensters                                 *)
(*-------------------------------------------------------------------*)
PROCEDURE define_window ;
BEGIN
   act_window[1,1] := x1 ;
   act_window[1,2] := y1 ;
   act_window[2,1] := x2 ;
   act_window[2,2] := y2 ;
   clear_faktoren ;
   view_act_window ;
END ;

(*-------------------------------------------------------------------*)
(* Löschen des aktuellen Fensters                                    *)
(*-------------------------------------------------------------------*)
PROCEDURE clear_window ;
BEGIN
   fillstyle ( filltype) ;
   fillport ;
   fillstyle(default_fill) ;
END ;

(*-------------------------------------------------------------------*)
(* Löschen des gesamten Bildschirmes und Definition des selben       *)
(* als aktuelles Fenster.                                            *)
(*-------------------------------------------------------------------*)
PROCEDURE clear_gscreen ;
BEGIN
   define_window(0,0,scr_xmax,scr_ymax) ;
   clear_window(default_fill) ;
END ;

(*-------------------------------------------------------------------*)
(* Procedure zum Abfragen des aktuellen Fensters                     *)
(*-------------------------------------------------------------------*)
PROCEDURE query_window ;
BEGIN
   window := act_window ;
   scale_par[1] := xfak ;
   scale_par[2] := xoffs;
   scale_par[3] := yfak ;
   scale_par[4] := yoffs;
END ;
(*-------------------------------------------------------------------*)
PROCEDURE set_window ;
BEGIN
   act_window := window ;
   xfak := scale_par[1] ;
   xoffs := scale_par[2] ;
   yfak := scale_par[3] ;
   yoffs := scale_par[4] ;
   view_act_window ;
END ;

(*-------------------------------------------------------------------*)
(* Routinen zu Steuerung eines Graphik-Cursors                       *)
(*-------------------------------------------------------------------*)
PROCEDURE move_cursor ;
BEGIN
   rasterop(invert) ;
   draw_line(xcur-deltacur,ycur,xcur+deltacur,ycur) ;
   draw_line(xcur,ycur+deltacur,xcur,ycur-deltacur) ;
   draw_line(x-deltacur,y,x+deltacur,y) ;
   draw_line(x,y+deltacur,x,y-deltacur) ;
   xcur := x ;
   ycur := y ;
   rasterop(default_raster) ;
END ;

(*-------------------------------------------------------------------*)
PROCEDURE init_cursor ;
BEGIN
   rasterop(invert ) ;
   draw_line(x-deltacur,y,x+deltacur,y) ;
   draw_line(x,y+deltacur,x,y-deltacur) ;
   xcur := x ;
   ycur := y ;
   delta_cursor := 8 ;
   rasterop(default_raster) ;
END ;
(*-------------------------------------------------------------------*)
PROCEDURE del_cursor ;
BEGIN
   rasterop(invert ) ;
   draw_line(xcur-deltacur,ycur,xcur+deltacur,ycur) ;
   draw_line(xcur,ycur+deltacur,xcur,ycur-deltacur) ;
   rasterop(default_raster) ;
END ;
(*-------------------------------------------------------------------*)
PROCEDURE center_cursor ;
BEGIN
   move_cursor ((act_window[1,1] + act_window[2,1]) DIV 2 ,
                (act_window[1,2] + act_window[2,2]) DIV 2 ) ;
END ;
(*-------------------------------------------------------------------*)
PROCEDURE query_cursor ;
BEGIN
   x := xcur ;
   y := ycur ;
END ;
(*-------------------------------------------------------------------*)
FUNCTION cursor ;

VAR x,y   : INTEGER ;
    ende  : BOOLEAN ;

BEGIN
   ende := FALSE ;
   query_cursor(x,y) ;
   REPEAT
      IF get_key(taste) THEN BEGIN
         CASE taste OF
(* C-END *) 117   :  BEGIN
                        ende := TRUE ;
                        cursor := true ;
                        taste := 0 ;
                     END ;
(* pgup *)  73    :  IF delta_cursor < 32 THEN delta_cursor := delta_cursor * 2 ;
(* pgdn *)  81    :  IF delta_cursor > 1 THEN delta_cursor := delta_cursor DIV 2
                                         ELSE delta_cursor := 1 ;

(* cleft *) 75    :  BEGIN
                        x := x - delta_cursor ;
                        IF x < act_window[1,1] THEN x := act_window[1,1] ;
                     END ;
(* cright*) 77    :  BEGIN
                        x := x + delta_cursor ;
                        IF x > act_window[2,1] THEN x := act_window[2,1] ;
                     END ;
(* cup *)   72    :  BEGIN
                        y := y + delta_cursor ;
                        IF y > act_window[2,2] THEN y := act_window[2,2] ;
                     END ;
(* cdn *)   80    :  BEGIN
                        y := y - delta_cursor ;
                        IF y < act_window[1,2] THEN y := act_window[1,2] ;
                     END ;
(* Home *)  71    :  BEGIN
                        center_cursor ;
                        query_cursor(x,y) ;
                     END ;
            OTHERWISE BEGIN
                         IF taste IN valid_fktkeys THEN BEGIN
                             ende := true ;
                             cursor := false ;
                         END ELSE beep ;
                      END ;
         END ;
         IF (ord(taste) IN [75,77,72,80]) THEN move_cursor(x,y) ;
      END ELSE beep ;
   UNTIL ende ;
END ;
(*-------------------------------------------------------------------*)
(* Routine zur Initialisierung der Grafik                            *)
(*-------------------------------------------------------------------*)
PROCEDURE set_defaults ;
BEGIN
   default_raster := rast ;
   default_fill := filltype ;
   fillstyle(filltype) ;
   rasterop(rast) ;
END ;

(*-------------------------------------------------------------------*)
PROCEDURE query_defaults ;
BEGIN
   rast := default_raster ;
   filltype := default_fill ;
END ;
(*-------------------------------------------------------------------*)
PROCEDURE init_grafik ;
BEGIN
   graf_mode := querygrafix ;
   IF graf_mode <> nografik THEN BEGIN
      initgrafix(graf_mode) ;
      screensize(scr_xmax,scr_ymax) ;
      setdisplay(page1,page1,page1) ;
      CASE graf_mode OF
        ibmcard  : fontgr(0) ;          (* IBM Monochrome *)
        herkules : fontgr(2) ;
      END ;
      set_defaults(replace,black) ;
      clear_faktoren ;
      define_window(0,0,scr_xmax,scr_ymax) ;
   END ELSE BEGIN
      setdisplay(textmode,textmode,textmode) ;
      writeln(output,'No graphics card installed') ;
      endxqq ;
   END ;
END ;

(*-------------------------------------------------------------------*)
(* Bestimmung der Umrechnungsfaktoren von User  - Koordinaten auf    *)
(* Bildschirm - Koordinaten                                          *)
(*-------------------------------------------------------------------*)
PROCEDURE scalexy ;

VAR idelta          : INTEGER ;
    xdelta,fak,offs : REAL8 ;

BEGIN
   idelta := abs(max - min) ;
   xdelta := abs(xmax - xmin) ;
   IF xdelta = 0 THEN xdelta := 1.0 ;
   fak := idelta / xdelta ;
   offs := min - fak * xmin ;
   CASE xy OF
      'x','X'    : BEGIN
                     xfak   := fak ;
                     xoffs  := offs ;
                  END ;
       'y','Y'   : BEGIN
                     yfak   := fak ;
                     yoffs  := offs ;
                  END ;
   END ;
END ;

(*-------------------------------------------------------------------*)
(* Funktionen zur Umrechnung von User - Koordinaten IN Bildschirm -  *)
(* Koordinaten                                                       *)
(* Eine Dieser Proceduren muá immer aufgerufen werden wenn Punkte    *)
(* IN User - Koordinaten gezeichnet werden sollen.                   *)
(*-------------------------------------------------------------------*)
FUNCTION xbild ;
BEGIN
   xbild := round(xfak * x + xoffs) ;
END ;
(*-------------------------------------------------------------------*)
FUNCTION ybild ;
BEGIN
   ybild := round(yfak * y + yoffs) ;
END ;
(*-------------------------------------------------------------------*)
PROCEDURE bild_koordinaten ;
BEGIN
   ix := xbild(x) ;
   iy := ybild(y) ;
END ;
(*-------------------------------------------------------------------*)
FUNCTION xuser ;
BEGIN
   xuser := ( x - xoffs) / xfak ;
END ;
(*-------------------------------------------------------------------*)
FUNCTION yuser ;
BEGIN
   yuser := ( y - yoffs) / yfak ;
END ;
(*-------------------------------------------------------------------*)
PROCEDURE  user_koordinaten ;
BEGIN
   x := xuser(ix) ;
   y := yuser(iy) ;
END ;

(*-------------------------------------------------------------------*)
(* Zeichnen einer Rechteckigen Box                                   *)
(*-------------------------------------------------------------------*)
PROCEDURE draw_box ;

BEGIN
   moveto(x1,y1) ;
   lineto(x1,y2) ;
   lineto(x2,y2) ;
   lineto(x2,y1) ;
   lineto(x1,y1) ;
END ;

(*-------------------------------------------------------------------*)
PROCEDURE grafik_text;
BEGIN
   moveto( x,y) ;
   writegr(s) ;
END ;

(*-------------------------------------------------------------------*)
(* Hilfsroutinen für AXIS                                            *)
(*-------------------------------------------------------------------*)
PROCEDURE change( VAR a,b : INTEGER ) ;
 VAR aux : INTEGER ;
 BEGIN
   IF a > b THEN BEGIN
      aux := a ;
      a := b ;
      b := aux ;
   END ;
END ;
(*-------------------------------------------------------------------*)
PROCEDURE change_real( VAR a,b : REAL8 ) ;
 VAR aux : REAL8 ;
 BEGIN
   IF a > b THEN BEGIN
      aux := a ;
      a := b ;
      b := aux ;
   END ;
END ;
(*-------------------------------------------------------------------*)
PROCEDURE split_real (       xaux,xmax  : REAL8 ;
                       VAR   m          : manstring ;
                       VAR   e          : expstring ) ;
VAR encode_flag : BOOLEAN  ;
    iexp        : INTEGER4 ;
    man         : REAL8    ;
    aux         : LSTRING(20) ;

BEGIN
    iexp := trunc( lddrqq(xmax) ) ;
    xaux := xaux * pidrqq(10.0,-iexp) ;
    encode_flag := encode(m,xaux:strlen:2) ;
    encode_flag := encode(aux,iexp:1) ;
    e := '* 10^' ;
    concat (e,aux) ;
END ;

(*-------------------------------------------------------------------*)
(* AXIS : Routine zur Skalierung eines Bildes und Zeichnen           *)
(* eines beschrifteten Achsenkreuzes                                 *)
(*-------------------------------------------------------------------*)
PROCEDURE axis ;

VAR strm               : manstring ;
    strexp             : expstring ;
    i,iaux             : INTEGER ;
    x1,y1,x2,y2        : INTEGER ;
    deltax,deltay,xaux : REAL8 ;
    mantisse,scale     : REAL8 ;
    exponent           : INTEGER ;
    scale_par          : windowpar_type ;
    window             : window_type ;

BEGIN
    change(x11,x12) ;
    change(y11,y12) ;
    change_real(xmin,xmax) ;
    change_real(ymin,ymax) ;
    rasterop(replace) ;
    define_window(x11,y11,x12,y12) ;
    clear_window(black) ;
    x1 := x11 + strlen * 8 + 16 ;  (* Definition des Zeichen Fensters *)
    y1 := y11 + 28 ;
    y2 := y12 - 18 ;
    x2 := x12 - 8 ;
    draw_box(x1,y1,x2,y2) ;        (* Zeichnen der Achsen *)
    scalexy ('x',x1+7,x2-7,xmin,xmax) ;
    deltax := (xmax - xmin) / nx ;
    IF deltax = 0.0 THEN deltax := 1.0 / nx ;
    scale := abs(xmax) ;                (* Skaliere X - Achse *)
    IF abs(xmin) > scale THEN scale := abs(xmin) ;
    FOR i := 0 TO nx DO BEGIN
       xaux := xmin + i * deltax  ;
       iaux := xbild(xaux) ;
       draw_line(iaux,y1,iaux,y1+5) ;
       draw_line(iaux,y2,iaux,y2-5) ;
       split_real(xaux,scale,strm,strexp) ;
       grafik_text(iaux-32,y1-14,strm) ;
    END ;
    grafik_text( x2-68,y11,strexp) ;
    scale := abs(ymax) ;            (* Skaliere Y - Achse *)
    IF abs(ymin) > scale THEN scale := abs(ymin) ;
    scalexy ('y',y1+7,y2-7,ymin,ymax) ;
    deltay := (ymax - ymin) / ny ;
    IF deltay = 0.0 THEN deltay := 1.0 / ny ;
    FOR i := 0 TO ny DO BEGIN
       xaux := ymin + i * deltay ;
       iaux := ybild(xaux) ;
       draw_line(x1,iaux,x1+5,iaux) ;
       draw_line(x2,iaux,x2-5,iaux) ;
       split_real(xaux,scale,strm,strexp) ;
       grafik_text(x11+8,iaux-5,strm) ;
    END ;
    grafik_text(x11+8,y2+4,strexp) ;
    grafik_text(((x2+x1) DIV 2) - lengthgr(header),y2+4,header) ;
    grafik_text(((x2+x1) DIV 2) - lengthgr(xheader),y11,xheader) ;
    query_window(window,scale_par) ;
    define_window(x1+1,y1+1,x2-1,y2-1) ;
    set_window(act_window,scale_par) ;
    rasterop(default_raster) ;
END ;

(*-------------------------------------------------------------------*)
PROCEDURE  user_plot ;

VAR ix,iy : INTEGER ;

BEGIN
   bild_koordinaten( x,y,ix,iy) ;
   plot(ix,iy) ;
END ;

(*-------------------------------------------------------------------*)
PROCEDURE  user_lineto ;

VAR ix,iy : INTEGER ;

BEGIN
   bild_koordinaten( x,y,ix,iy) ;
   lineto(ix,iy) ;
END ;
(*-------------------------------------------------------------------*)
PROCEDURE  user_moveto ;

VAR ix,iy : INTEGER ;

BEGIN
   bild_koordinaten( x,y,ix,iy) ;
   moveto(ix,iy) ;
END ;
(*-------------------------------------------------------------------*)
PROCEDURE  user_line ;

VAR ix1,iy1,ix2,iy2 : INTEGER ;

BEGIN
   bild_koordinaten( x1,y1,ix1,iy1) ;
   bild_koordinaten( x2,y2,ix2,iy2) ;
   moveto(ix1,iy1) ;
   lineto(ix2,iy2) ;
END ;
(*-------------------------------------------------------------------*)
PROCEDURE plot_vektor ;

VAR i,ix,iy : INTEGER ;

BEGIN
   rasterop(rast) ;
   IF lineflag THEN BEGIN
      user_moveto(xvek[1],yvek[1]) ;
      FOR i := 2 TO n-1 DO BEGIN
         user_lineto(xvek[i],yvek[i]) ;
         user_plot(xvek[i],yvek[i]) ;
      END ;
      user_lineto(xvek[n],yvek[n]) ;
   END ELSE FOR i := 1 TO n DO user_plot(xvek[i],yvek[i]) ;
END ;
(*-------------------------------------------------------------------*)
END .
