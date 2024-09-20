; Hilfsroutinen fr IO.pas
;
;  Version vom 21.12.1985 ;
;
data  segment public   'data'
data  ends
;
dgroup   group   data
;
ioasm    segment  para public 'code'
         assume   cs:ioasm,ds:data,ss:data
;
         public key,fill,fillstr
;
;------------------------------------------------------------------
key      proc     far                  ;                  Begin KEY
;
;  Deklaration :
;
;  function key : integer ; extern ;
;
;  Die Funktion KEY fragt ob ein Zeichen im Eingabe Puffer vorliegt.
;  Liegt ein Zeichen vor so wird dieses Zeichen als Funktions Resultat
;  zurckgegeben; andernfalls wird FF00H bergeben.
;
         push     bp                   ; Save framepointer
;
         mov      ah,06H               ; console I/O
         mov      dl,0ffh              ; auf Input setzen
;
         int      21h                  ; DOS Call
;
         jnz      found                ; Key gefunden ->
         mov      ah,0ffh              ; Nein  Ergebnis negativ
         jmp      keyout               ; fertig
;
found:
         xor      ah,ah                ; ah auf null setzen
;
keyout:
         pop      bp                   ; restore Framepointer
         ret
;
key      endp
;
;------------------------------------------------------------------
fill     proc  far                     ;                Begin  FILL
;
; Deklaration in Pascal :
;
; procedure fill (    pos   : word   ;
;                     c     : char ;
;                     attr  : char ;
;                     count : integer ) ; extern ;
;
; Fill fllt den Bildspeicherbereich beginnend mit Pos mit dem Word , daá
; durch Char und Attr gebildet wird. Count gibt die Anzahl an .
;
frame    struc
   Basep1   dw       ?                 ; saved Framepointer
   retadr1  dd       ?                 ; Return-Adresse
   count1   dw       ?                 ; Z„hler
   attr1    dw       ?                 ; zugeh”rige Attribute
   char1    dw       ?                 ; Auszugebende Character
   pos1     dw       ?                 ; Adresse in Bildschirmspeicher
frame    ends
;
         push     bp                ; Save Framepointer
         mov      bp,sp
;
         mov      ax,0b000H         ; Lade ES:DI mit Bildschirm Adresse
         mov      es,ax
         mov      di,[bp].pos1
         mov      ax,[bp].char1     ; Charcter in al register
         mov      bx,[bp].attr1     ; Attribute in bl register
         mov      ah,bl
         mov      cx,[bp].count1    ; Z„hler nach cx
;
         cld                        ; l”schen des Richtungsflags
;                                     => incrementierend
rep      stosw                      ; Bildschirm fllen
;
         pop      bp                ; Restore Framepointer
         ret      8
;
fill     endp
;------------------------------------------------------------------
fillstr  proc  far                     ;              Begin FILLSTR
;
;  Deklaration :
;
;  procedure fillstr (        pos   : word    ;
;                      consts s     : lstring  ;
;                             attr  : char   ;
;                             count : integer ) ; extern ;
;
frame    struc
   dataseg  dw    ?
   basep2   dw    ?
   return2  dd    ?
   count2   dw    ?
   attr2    dw    ?
   strofs2  dw    ?
   strseg2  dw    ?
   strlen   dw    ?
   pos2     dw    ?
frame    ends
;
         push        bp                ; Save Framepointer
         push        ds
         mov         bp,sp
;
         mov         cx,[bp].count2    ; Count -> CX
         cmp         cx,0              ; count Null ?
         jz          fill_none         ; wenn ja -> ENDE
;
         mov         ax,0b000H         ; Lade ES mit Bildschirmsegment
         mov         es,ax
         mov         di,[bp].pos2      ; adresse -> ES:DI
;
         mov         ax,[bp].attr2     ; attr  -> AH
         mov         ah,al
;
         mov         si,[bp].strofs2   ; Stringoffs -> si
         mov         dx,[bp].strseg2   ; Stringseg -> ds
         mov         ds,dx
         inc         si                ; šberspringe L„nge
;
nextchar:
         mov         al,ds:[si]        ; Ascii -> al
         mov         es:[di],ax        ; in Bildschirmspeicher schreiben
         inc         si                ; erh”hen der Pointer
         inc         di
         inc         di
         loop        nextchar          ; Z„hler erniedrigen und ver-
                                       ; zweigen  wenn cx >0
;
fill_none:
         pop         ds                ; Restore Register
         pop         bp
;
         ret         10
;
fillstr  endp
;-------------------------------------------------------------------
ioasm    ends
         end
