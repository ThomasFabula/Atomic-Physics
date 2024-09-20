PROGRAM change(input,output,data,inp,out) ;

(*$debug- *)
(*$line- *)
(*$entry- *)

TYPE single_word    = LSTRING(15) ;
     res_words      = ARRAY [1..300] OF single_word ;
     limit_set      = SET OF CHAR ;
     set_of_char    = SET OF CHAR ;
     set_of_integer = SET OF 0..255 ;

CONST limiter = limit_set  [ '(' , ')' , '[' , ']' , ' ' , ';' , '.'] ;

VAR upc_words,words : res_words ;
    inp,out,data    : TEXT ;
    max             : INTEGER ;

(*-----------------------------------------------------------------*)
PROCEDURE upperwords ( CONSTS words        : res_words ;
                       VARS   upc_words    : res_words ;
                              maxwords     : Integer ) ;

VAR i,j,l : INTEGER ;
    c     : CHAR ;

BEGIN
   FOR i := 1 TO maxwords DO BEGIN
      upc_words[i] := NULL ;
      FOR j := 1 TO ORD(words[i].len) DO
      CASE words[i,j] OF
         'a'..'z'  : BEGIN
                        c := CHR(ORD(words[i,j]) - 32 ) ;
                        CONCAT(upc_words[i],c) ;
                     END ;

         OTHERWISE   CONCAT (upc_words[i],words[i,j]) ;
      END ;
   END ;
END ;
(*-----------------------------------------------------------------*)
PROCEDURE change (VAR inp,out         : TEXT ;
                      words,upc_words : res_words ;
                      maxwords        : INTEGER   ) ;

VAR s,s1,s2    : LSTRING(255) ;
    len,p,k,pa,pe,lw,i,n : INTEGER ;
    act_word   : single_word ;
    c,c1,c2 : CHAR ;

BEGIN
   RESET(inp) ;
   REWRITE(out) ;
   WHILE NOT EOF(inp) DO BEGIN
      s := NULL ;
      READLN (inp,s) ;
      s1 := s ;
      len := ORD(s.len)  ;
      IF len >= 2 THEN FOR i := 1 TO maxwords DO BEGIN
         act_word := words[i] ;
         lw := ORD(act_word.len) ;
         p := 0 ;
         REPEAT
            k := POSITN(act_word,s,p+1) ;
            IF k <> 0 THEN BEGIN
               p := k ;
               pa := p-1 ;
               pe := p + lw  ;
               IF pa = 0 THEN c1 := ' '
                         ELSE c1 := s[pa] ;
               IF pe > len THEN c2 := ' '
                           ELSE c2 := s[pe] ;
               IF (c2 IN limiter) AND ( c1 IN limiter) THEN BEGIN
                  DELETE(s,p,lw) ;
                  INSERT(upc_words[i],s,p) ;
                  p := pe ;
               END ;
            END ;
         UNTIL (k = 0) OR ((len-p+1) < lw) ;
      END ;
      WRITELN(out,s) ;
      WRITELN(output,s) ;
   END ;
   CLOSE (out) ;
   CLOSE (inp) ;
END;
(*-----------------------------------------------------------------*)
(*                                                                 *)
(*-----------------------------------------------------------------*)
BEGIN
   max := 1 ;
   RESET(data) ;
   WHILE NOT EOF(data) DO BEGIN
      READLN(data,words[max]) ;
      max := max+1 ;
   END ;
   CLOSE(data) ;
   max := max-1 ;
   upperwords ( words,upc_words,max) ;
   change (inp,out,words,upc_words,max) ;
END .
