{ $include: 'io.int'  }

{  Version vom 3.7.86	 }

IMPLEMENTATION OF io ;

{ warn-  }
{ debug-  }

CONST cursorleft     = 75 ;
      cursorright    = 77 ;
      cursorhome     = 71 ;
      characterins   = 82 ;
      characterdel   = 83 ;
      endline	     = 79 ;
      ctrl_endline   = 117 ;
      backspace      = 8 ;
      scr_zeilen     = 25 ;
      scr_spalten    = 80 ;
      editset	     = set_of_integer [cursorleft,cursorright,cursorhome,
		       characterins,characterdel,
		       endline,ctrl_endline,backspace] ;

var get_flag : boolean ;
{----------------------------------------------------------------------}
{  Definition Von externen Proceduren				       }
{----------------------------------------------------------------------}

FUNCTION dosxqq ( command : BYTE ; parm : WORD ) : BYTE ; EXTERNAL  ;

PROCEDURE outp( portno:WORD; data:BYTE) ;		  EXTERNAL  ;

PROCEDURE endxqq ;					  EXTERNAL  ;

FUNCTION key	 ;					  EXTERNAL  ;

PROCEDURE fill	 ;					  EXTERNAL  ;

PROCEDURE fillstr ;					  EXTERNAL  ;

{----------------------------------------------------------------------}
PROCEDURE beepio ;
VAR result : BYTE ;
BEGIN
   result := dosxqq(2,7) ;
END ;
							    { END beepio }
{----------------------------------------------------------------------}
PROCEDURE locate ;
VAR result : BYTE ;
    w	   : WORD ;

BEGIN
   row := ((row-1) MOD scr_zeilen) +1 ;
   col := ((col-1) MOD scr_spalten) +1 ;
   result := dosxqq(2,27) ;	     {	ESC  }
   result := dosxqq(2,91) ;	     {	[    }
   IF row >=10 THEN BEGIN
      w := wrd((row DIV 10 ) + 48) ;
      result := dosxqq(2,w) ;
      row := row MOD 10
   END ;
   w := wrd(row + 48) ;
   result := dosxqq(2,w) ;
   result := dosxqq(2,59);	     {	;  }
   IF col >= 10 THEN BEGIN
      w := wrd((col DIV 10) + 48) ;
      result :=dosxqq(2,w) ;
      col := col MOD 10
   END ;
   w := wrd(col + 48) ;
   result := dosxqq(2,w) ;
   result := dosxqq(2,102) ;	     {	f  }
END ;
							{  End LOCATE  }
{----------------------------------------------------------------------}
PROCEDURE fillscreen ;

CONST count = scr_zeilen * scr_spalten	;

BEGIN
   fill ( 0,fillchar,attribute,count) ;
END ;
						    {  END FILLSCREEN  }
{----------------------------------------------------------------------}
PROCEDURE clearscreen ;

BEGIN
   fillscreen( ' ',normal_attr) ;
   locate(1,1) ;
END ;
						     { END clearscreen }
{----------------------------------------------------------------------}
PROCEDURE fillfield ;

CONST scr_limit   = scr_zeilen * scr_spalten * 2 ;

VAR scr_offset,scr_end : WORD ;

BEGIN
   row := ((row-1) MOD scr_zeilen)  ;
   col := ((col-1) MOD scr_spalten) ;
   scr_offset := wrd(row * scr_spalten + col ) * 2 ;
   scr_end := scr_offset + wrd(count * 2) ;
   IF scr_end >scr_limit THEN
	count := ord((scr_limit - scr_offset +1 ) DIV 2) ;
   fill( scr_offset,fillchar,attribute,count) ;
END ;
						     {	End FILLFIELD  }
{----------------------------------------------------------------------}
PROCEDURE fillline ;

BEGIN
   fillfield (row,1,80,fillchar,attribute) ;
END ;
							{ END fillline }
{----------------------------------------------------------------------}
PROCEDURE fillstring ;

VAR scr_offset : WORD ;
    lens       : INTEGER ;

BEGIN
   lens := ord(s.len) ;
   IF len > lens THEN len := lens ;
   IF len > 0  THEN BEGIN
      row := ((row-1) MOD scr_zeilen)  ;
      col := ((col-1) MOD scr_spalten) ;
      scr_offset := wrd(row * scr_spalten + col ) * 2 ;
      IF (col + len) > 80 THEN	len := 81 - col ;
      fillstr ( scr_offset,s,attribute,len) ;
   END ;
END ;
						      { END fillstring }
{----------------------------------------------------------------------}
PROCEDURE Make_frame ;

TYPE symbole = ARRAY [0..3] OF STRING(8) ;

CONST sym = symbole ('…Õª∫Ãπ»º','⁄ƒø≥√¥¿Ÿ','÷ƒ∑∫«∂”Ω','’Õ∏≥∆µ‘æ') ;

VAR k,lenx,lens :  INTEGER;


BEGIN
   IF ( (x2-x1) > 3 ) AND ( y2 >= y1) THEN BEGIN
      opt :=  opt MOD 4 ;
      lenx := x2  - x1 - 1 ;
      {  Zeichnen der Kopfzeile  }
      IF y2 > y1 THEN BEGIN
	 fillfield(y1,x1,1,sym[opt,1],attribute) ;
	 fillfield(y1,x1+1,lenx,sym[opt,2],attribute) ;
	 fillfield(y1,x2,1,sym[opt,3],attribute) ;
      {  Zeichenen der senkrechten Balken  }
	 IF (y2 - y1) > 1 THEN FOR k := y1+1 TO y2-1 DO BEGIN
	    fillfield(k,x1,1,sym[opt,4],attribute) ;
	    fillfield(k,x2,1,sym[opt,4],attribute) ;
	 END ;
      {  Zeichnen des Abschlu· Balkens	}
	 fillfield(y2,x1,1,sym[opt,7],attribute) ;
	 fillfield(y2,x1+1,lenx,sym[opt,2],attribute) ;
	 fillfield(y2,x2,1,sym[opt,8],attribute) ;
      END ELSE fillfield(y1,x1,lenx+2,sym[opt,2],attribute) ;
      {  Schreiben der öberschrift   }
      lens := ord(topic.len) ;
      IF (lenx >= (lens + 2)) AND ( lens > 0 ) THEN BEGIN
	 k := x1 + (lenx-lens) DIV 2;
	 fillfield(y1,k,1,sym[opt,6],attribute) ;
	 fillstring(y1,k+1,lens,topic,topic_attr) ;
	 fillfield(y1,k+lens+1,1,sym[opt,5],attribute) ;
      END ;
   END;
END ;
						   {  Ende Make_Frame  }
{----------------------------------------------------------------------}
FUNCTION getkey ;
BEGIN
   getkey := FALSE ;
   REPEAT
      keycode := key ;
      IF keycode = 0 THEN getkey := TRUE
		     ELSE IF keycode < 0 THEN getkey := FALSE ;
   UNTIL keycode > 0 ;
END ;
							{  End GETKEY  }
{----------------------------------------------------------------------}
PROCEDURE default_iopoint ;

BEGIN
   WITH iopoint DO BEGIN
      validchars  := ['a'..'z','A'..'Z','0'..'9','.','\',' ','Ñ','î',
		      'Å','é','ô','ö',':'] ;
      validpf	  := [59..68,84..113] ;
      validedit   := [] ;
      breakenable := FALSE ;
      pfenable	  := TRUE  ;
      editenable  := TRUE  ;
      default	  := TRUE  ;
      display	  := FALSE ;
      fieldchar   := ' '   ;
      break_occ   := FALSE ;
   END;
END;
						 { END default_iopoint }
{----------------------------------------------------------------------}
PROCEDURE set_cursor (flag : BOOLEAN) ;

CONST indexregister = 16#3b4 ;
      dataregister  = 16#3b5 ;

VAR a : BYTE ;

BEGIN
   IF flag THEN a := 4 ELSE a := 16#0b ;
   outp(indexregister,10) ;
   outp(dataregister,a) ;
END ;
						      { END set_cursor }
{----------------------------------------------------------------------}
FUNCTION getchar ;

VAR fkt,ok	 : BOOLEAN ;
    keycode,h	 : INTEGER ;
    char_flag	 : BOOLEAN ;

BEGIN
   WITH iopoint DO BEGIN
      okio := TRUE ;
      pfkey := 0 ;
      editkey := 0 ;
      char_flag := FALSE ;
      break_occ := FALSE ;
      REPEAT
	 fkt := getkey (keycode) ;
	 IF (NOT fkt) AND ( keycode = 8 ) THEN	fkt := TRUE ;
	 ok := FALSE ;
	 IF fkt THEN BEGIN
	    IF ( keycode IN validpf) AND pfenable THEN BEGIN
	       ok := TRUE ;
	       CASE keycode OF
		  59..68   :  pfkey := keycode-58 ;
		  84..113  :  pfkey := keycode-73 ;
		  OTHERWISE  ok := FALSE ;
	       END ;
	    END;
	    IF (( keycode IN validedit) AND editenable) OR
		 (( keycode IN editset) AND get_flag) THEN BEGIN
	       editkey := keycode ;
	       ok := TRUE ;
	    END ;
	 END ELSE BEGIN
	    IF (ord(keycode) = 3 ) THEN BEGIN	 { BREAK pressed }
	       IF breakenable THEN BEGIN
		  set_cursor(FALSE) ;
		  endxqq ;
	       END ELSE BEGIN
		  break_occ := TRUE ;
		  ok := TRUE ;
	       END ;
	    END ELSE BEGIN			 { Beliebiger Buchstabe }
	       charkey := chr(keycode) ;
	       IF (charkey IN validchars) OR (charkey = chr(13)) THEN BEGIN
		  ok := TRUE ;
		  char_flag := TRUE ;
	       END;
	    END ;
	 END ;
	 IF NOT ok THEN beepio ;
      UNTIL ok ;
      getchar := char_flag ;
   END ;
END;
						       {  End GETCHAR  }
{----------------------------------------------------------------------}
FUNCTION getstring ;

TYPE hilfsarray = STRING(80) ;

VAR i,j,k,len	 : INTEGER ;
    col1	 : INTEGER ;
    instr	 : hilfsarray ;
    okflag	 : BOOLEAN ;
    c		 : CHAR ;
    insflag	 : BOOLEAN ;

   PROCEDURE writefield(	row,col     : INTEGER	 ;
				len	    : INTEGER ;
			 CONSTS instr	    : hilfsarray ;
				attr	    : CHAR ) ;
   VAR i : INTEGER ;

   BEGIN
      FOR i := 1 TO len DO fillfield(row,col+i-1,1,instr[i],attr) ;
   END ;

BEGIN
   insflag := FALSE ;			   {  Cursor Undescore	}
   set_cursor(insflag) ;
   row := ((row-1) MOD scr_zeilen) +1  ;   {  Zeilen und Spaltenzahl auf  }
   col := ((col-1) MOD scr_spalten) +1 ;   {  gÅltige Werte reduzieren	  }
   WITH iopoint DO BEGIN		  {  Flags initialisieren	 }
      getstring := FALSE ;
      okflag := FALSE ;
      FOR j := 1 TO max DO instr[j] := fieldchar ; {  Eingabe Puffer mit       }
						   {  Feldcharacter fÅllen     }
      IF default THEN BEGIN		  {  Wenn default wahr ist wird das    }
	 len := ord(sin.len) ;		  {  Eingabe Puffer mit Defaultstring  }
					  {  gefÅllt			       }
	 IF col+len > 80 THEN len := (80-col) ; { Abschneiden wenn s Åber die  }
	 IF len > max THEN len := max ; 	{  Zeile hinaus geht.	       }
	 FOR j := 1 TO len DO instr[j] := sin[j];{ String in Puffer kopieren   }
      END ELSE len := 0 ;
      writefield(row,col,max,instr,attr) ; {   Puffer in Bildschirm schreiben  }
      IF display THEN BEGIN	{  Nur Defaultstring Anzeigen : keine Eingabe  }
	 sout.len := 0 ;
	 j := ord(sin.len) ;
	 IF j > upper(sout) THEN j := upper(sout) ;
	 for i := 1 to j do concat(sout,sin[i]) ;
	 getstring := TRUE ;
	 RETURN ;
      END ;
      i := len+1 ;		   {  i ist ZÑhler fÅr Cursor im Eingabefeld   }
      if I > max then i := max ;
      get_flag := true ;
      REPEAT ;			   {  Begin der Eingabeschleife 	       }
	 IF i > max THEN BEGIN
	    i := max ;
	    beepio ;
	 END ;
	 col1 := col+i-1 ;
	 locate (row,col1) ;
	 IF getchar(c,iopoint) THEN BEGIN  {  True => Normales Zeichen	       }
	    IF ord (c) = 13 THEN BEGIN	   {  CR eingegeben => Abbruch	       }
	       okflag := TRUE ;
	       getstring := TRUE ;
	    END ELSE IF i <= max THEN BEGIN {  Sonstiges Zeichen in Puffer     }
	       IF insflag  THEN BEGIN	    {  einfÅgen 		       }
		  IF instr[max] = fieldchar THEN BEGIN
		     FOR j := max-1 DOWNTO i DO instr[j+1] := instr[j] ;
		     instr[i] := c ;
		     writefield(row,col,max,instr,attr) ;
		     i := i + 1 ;
		  END ELSE beepio ;
	       END ELSE BEGIN
		  instr[i] := c ;
		  fillfield(row,col1,1,c,attr) ;
		  i := i + 1 ;
	       END ;
	    END ELSE beepio ;	    {  BEEP wenn Cursor Au·erhalb des Feldes  }
	 END ELSE BEGIN 	  {  Funktionscode wurde eingegeben	    }
	    CASE editkey OF	  {  Definition der Edit-Funkionen	    }
	       0	   :   ;  {  Kein Editkey gedrÅckt		    }
	       backspace   :  BEGIN
				 IF i > 1 THEN BEGIN
				    i := i -1 ;
				    FOR j := i+1 TO max DO
				       instr[j-1] := instr[j] ;
				    instr[max] := fieldchar ;
				    writefield(row,col,max,instr,attr) ;
				 END ;
			      END ;
	       cursorhome  :  i := 1 ;
	       cursorleft  :  IF i > 1 THEN i := i -1
				       ELSE beepio ;
	       cursorright :  IF i < max THEN i := i + 1
					 ELSE beepio ;
	       endline	   :  BEGIN
				 i := max ;
				 WHILE (instr[i] = fieldchar) AND
				    (i > 0 )  DO i := i - 1 ;
				 IF i < max THEN i := i + 1 ;
			      END ;
	       characterdel : BEGIN
				 FOR j := i+1 TO max DO
				     instr[j-1] := instr[j] ;
				 instr [max] := fieldchar ;
				 writefield(row,col,max,instr,attr) ;
			      END ;
	       characterins : BEGIN
				 insflag := NOT insflag ;
				 set_cursor(insflag) ;
			      END ;
	       ctrl_endline : BEGIN
				 FOR j := i TO max DO instr[j] := fieldchar ;
				 writefield(row,col,max,instr,attr) ;
			      END;
	       OTHERWISE      BEGIN
				 getstring := FALSE ;
				 okflag := TRUE ;
			      END;
	    END ;
	    IF ((pfkey <> 0) OR break_occ) THEN BEGIN  {  ? gÅltiger Pfkey }
	       getstring := FALSE ;
	       okflag := TRUE ;
	    END ;
	 END ;
      UNTIL okflag ;
      get_flag := false ;
   END ;
   set_cursor(FALSE) ;	    {  Cursor auf normal zurÅcksetzen		   }
   j := max ;		    {  Zeichenkette aus Puffer in String kopieren  }
   k := 0 ;		    {  und ÅberzÑhlige Blanks am Ende lîschen	   }

   WHILE (instr[j] = iopoint.fieldchar) AND ( j > 0) DO j := j-1 ;
   sout.len := 0 ;
   IF j > upper(sout) THEN j := upper(sout) ;
   FOR i := 1 TO j DO concat(sout,instr[i]) ;

   {  Ist der eingegebene String kÅrzer als das verlangte Minimum so wird  }
   {  das okio flag in der Variable iopoint auf False gesetzt		   }
   {  Getstring ist True wenn mit CR abgeschlo·en wurde, sonst False	   }

   IF (j < min ) OR NOT okflag THEN iopoint.okio := FALSE ;
END;
						       { END getstring }
{----------------------------------------------------------------------}
PROCEDURE write_leiste ;

VAR i,p   : INTEGER ;
    c	  : CHAR ;

BEGIN
   fillline(25,' ',normal_attr) ;
   FOR i := 1 TO 10 DO BEGIN
      c := chr(( i MOD 10) + 48) ;
      p := ( i-1) * 8+1;
      fillfield(25,p,1,c,reverse_attr) ;
      fillstring(25,p+1,6,leiste[i],normal_attr) ;
   END ;
END ;

{----------------------------------------------------------------------}
function getreal ;

var sin,sout   : lstring(80) ;
    b,gets : boolean ;

begin
   b := encode(sin,xin:max:f) ;
   gets := getstring(row,col,min,max,sin,sout,attr,iop) ;
   b := decode(sout,xout) ;
   if not iop.display then begin
      b := encode(sin,xout:max:f) ;
      fillstring(row,col,max,sin,attr) ;
   end ;
   getreal := gets ;
end ;

{----------------------------------------------------------------------}
END.
