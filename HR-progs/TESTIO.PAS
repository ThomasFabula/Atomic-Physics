(* $include: 'c:\-ibmpas\io.int' *)
(* $debug- *)
(* $line- *)
(* $entry- *)

program testio(input,output) ;

uses io ;

const leiste = leisten_array ('input','output','sonst','weiter','ruppi',
                              'messen','break','skal','NEW','ENDE') ;
      Topic  = ' Test für IOMODULE ';

var  parameter : array [1..20] of lstring(7) ;
     par       : array [1..20] of real8 ;
     i,j,k     : integer ;
     b         : boolean ;

procedure init_par ;
var i : integer ;
    s : lstring (2) ;
    f : boolean ;
begin
   for i := 1 to 20 do begin
      par[i] := i ;
      f := encode(s,i:2) ;
      parameter[i] := 'par';
      concat(parameter[i],s) ;
      concat(parameter[i],':') ;
   end ;
end ;

procedure fill_mask (vars iopoint : iorecord ) ;

var i : integer ;
    b : boolean ;
    iop : iorecord ;

begin
   make_frame(1,1,80,24,1,normal_attr,topic,reverse_attr) ;
   make_frame(9,2,18,23,2,normal_attr,' parametername ',normal_attr) ;
   make_frame(19,2,36,23,2,normal_attr,' parameterwert ',normal_attr) ;
   write_leiste(leiste) ;
   iop := iopoint ;
   iop.display := true ;
   for i := 1 to 20 do begin
      fillstring(i+2,10,7,parameter[i],high_attr) ;
      b := getreal(i+2,20,1,15,3,par[i],par[i],normal_attr,iop) ;
   end ;
end ;

procedure get_data(vars iopoint:iorecord) ;
var i : integer ;
    b,ende : boolean ;

begin
   fill_mask(iopoint) ;
   i := 1 ;
   ende := false ;
   repeat
      if i < 1 then i := 20 ;
      i := ((i-1) mod 20) + 1 ;
      b := getreal(i+2,20,1,15,3,par[i],par[i],normal_attr,iopoint) ;
      if b then begin
         i := i + 1 ;
      end else with iopoint do begin
         case editkey of
             72   : i := i -1 ;
             80   : i := i + 1 ;
             0    : ;
         end ;
         case pfkey of
             10   : begin
                       ende := true ;
                       clearscreen ;
                    end ;
              9   : begin
                       clearscreen ;
                       fill_mask(iopoint) ;
                    end ;
              0   : ;
         end ;
      end ;
   until ende ;
end ;

procedure init_iopoint( vars iopoint : iorecord ) ;
begin
   default_iopoint(iopoint) ;
   with iopoint do begin
      display := false ;
      validchars := valid_num ;
      validedit := [72,80] ;
      validpf   := [67,68] ;
   end ;
end ;

begin
   clearscreen ;
   init_par ;
   init_iopoint(iopoint) ;
   get_data(iopoint) ;
end.
