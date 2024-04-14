CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC 
C									                                                            C 
C     'PERJ48'-VERSION (DEZEMBER 1983) ZUM WAHLWEISEN LESEN DER EIGENVEKTOR-  C 
C      ------	       DATEN VON KARTE ODER VON EINER DATEI MIT FILE-NR. 12     C 
C									                                                            C 
C     ERGAENZUNG ZUR PROGRAMMBESCHREIBUNG FUER STOERUNGSPROGRAMM 'PERJ48'     C 
C									                                                            C 
C									                                                            C 
C     FUER DAS EINLESEN DER EIGENVEKTORDATEN UEBER LOCHKARTEN IST KEINERLEI   C 
C     AENDERUNG DER DATENKARTEN ERFORDERLICH, NUR DIE OPTION 'OP2' (BISHER    C 
C     OHNE WIRKUNG) ERHAELT JETZT DIE UNTEN ANGEGEBENE BEDEUTUNG. AUSSERDEM   C 
C     KOENNEN AUF DER KARTE VOM TYP 2  UNTERE GRENZEN FUER DIE EIGENVEKTOR-   C 
C     AMPLITUDEN VON BRA UND KET EINGEGEBEN WERDEN, BEI DER RECHNUNG WERDEN   C 
C     DANN ALLE KLEINEREN KOMPONENTEN ELIMINIERT.			                        C 
C     BEI EINEM ISOTOP OHNE KERNSPIN (EYE=0.) WERDEN DIE ZEEMAN-STOERUNGEN    C 
C     AUF UEBERGAENGE	MJ1 <---> MJ2	BERECHNET. AUF DER KARTE VOM TYP 3        C 
C     BRAUCHT DANN NUR DER GJ-FAKTOR ANGEGEBEN ZU WERDEN, UND DIE KARTEN      C 
C     VOM TYP 6 UND 7 (KONFIGURATIONEN UND RADIALPARAMETER) ENTFALLEN.	      C 
C     FUER DAS LESEN DER KARTEN VOM TYP 4,8,9 (BRA- UND KET-VEKTOREN) AUS     C 
C     EINER DATEI MIT FILE-NR.12 (DIESE DATEI MUSS DIE EIGENVEKTOREN IN DER   C 
C     VOM PROGRAMM 'FSFIT' ERZEUGTEN FORM ENTHALTEN) MUESSEN DIE FOLGENDEN    C 
C     'PERJ'-KARTEN GEAENDERT WERDEN:					                                C 
C									                                                            C
C     TYP  SPALTE  FORMAT  NAME    BEDEUTUNG				                          C 
C     2     1	     I1    KEZ	   LEER: EIGENVEKTORDATEN SIND IM KARTEN-       C 
C					 SATZ ENTHALTEN (ALTE PERJ-VERSION)                                 C 
C				   =1 :  EIGENVEKTORDATEN WERDEN VON DATEI                            C 
C					 GELESEN, AUSWAHL DER VERWENDETEN                                   C 
C					 KETS NUR DURCH ANGABE VON DJ U. ED                                 C 
C				   =2 :  WIE KEZ=1, ABER ZUSAETZLICH ZU VER-                          C 
C					 WENDENDE ODER ZU ELIMINIERENDE KETS                                C 
C					 AUF KARTE(N) VOM TYP 2' ANGEGEBEN                                  C 
C				   =3 :  NUR DIE AUF KARTE(N) 2' EXPLIZIT                             C 
C					 GENANNTEN KETS WERDEN VERWENDET                                    C 
C	    2-4      I3    MAZ	   NR. DER EV-UNTERMATRIX DES BRA-ZUSTANDS          C 
C	    5-7      I3    NEZ	   NR. DES BRA-ZUSTANDS IN DER UNTERMATRIX          C 
C	    8-10     I3    N0	   >0 :  ANZAHL DER AUF KARTE(N) 2' EXPLIZIT          C 
C					 GENANNTEN (ZUSAETZLICHEN) KETS                                     C 
C					 (NUR FUER KEZ>1 ANGEBEN)	                                          C 
C				   <0 :  ES WERDEN DIE -N0 AUF KARTE(N) 2'                            C 
C					 GENANNTEN KETS ELIMINIERT  (KEZ=2)                                 C 
C	   21-25    F5.1   DJ	   >0. : KETS MIT   |J(BRA)-J'(KET)| <= DJ            C 
C					 WERDEN VERWENDET		                                                C 
C				   LEER: DJ=2. (WIRD PROGRAMMINTERN GESETZT)                          C 
C					(DJ=1. FUER ISOTOP OHNE KERNSPIN)                                   C 
C				   <0. : KETS MIT   |J-J'| = -DJ   VERWENDET                          C 
C	   26-30    F5.1   EYE	   KERNSPIN DES ISOTOPS 		                        C 
C	   31-40   F10.2   ED	   >0. : KETS MIT   |E-E'| <= ED   VERWENDET          C 
C				   LEER: ED=10**6/CM  (WIRD INTERN GESETZT)                           C 
C				   <0. : KETS MIT   E' <= E-ED   VERWENDET                            C 
C	   41-45     I5    NOC	   ANZAHL DER FAELLE  (=ZAHL DER KARTEN 10)         C 
C				   ANGABE NICHT NOTWENDIG  (LEER: NOC=99)                             C 
C	   51-55     I5    OP1	   =1 :  AUSDRUCK VON ZWISCHENERGEBNISSEN           C 
C	   56-60     I5    OP2	   =1 :  AUSDRUCK DER R.M.E. U2,V12,V13,V11         C 
C				   LEER: KEIN AUSDRUCK			                                          C 
C	   66-70    F5.1   EVMINB  UNTERE GRENZE FUER EV-AMPLITUDEN BEI BRA         C 
C	   71-75    F5.1   EVMINK  UNTERE GRENZE FUER EV-AMPLITUDEN DER KETS        C 
C     ----------------------------------------------------------------------  C 
C     FUER  KEZ>1  FOLGEN KARTEN VOM TYP 2' MIT DEN KENNZIFFERN DER (ZUSAETZ- C 
C     LICH) ZU VERWENDENDEN  (N0>0)  BZW. ZU ELIMINIERENDEN  (N0<0)  KETS     C 
C     2'    1-2      I2    NZ1(1)  NR. DER EV-UNTERMATRIX DES KET-ZUSTANDS    C 
C	    3-4      I2    NZ2(1)  NR. DES KET-ZUSTANDS IN DER UNTERMATRIX          C 
C	    9-10     I2    NZ1(2)  (ENTSPRECHEND FUER NAECHSTES KET)	              C 
C	   11-12     I2    NZ2(2)	 "        "	  "      "	                          C 
C	   USW. IM FORMAT  2I2,4X ; BIS ALLE  |N0| KETS GENANNT SIND	              C 
C     ----------------------------------------------------------------------  C 
C     4,8,9  ENTFALLEN	(WERDEN VON DATEI GELESEN)			                      C 
C     5      ENTFAELLT	(NOK,NON,NUCONF WERDEN PROGRAMMINTERN BERECHNET)      C 
C     6,7    WIE BISHER, ABER NUR FUER KONFIGURATIONEN EINGEBEN, DIE IM BRA   C 
C	     NACH ELIMINIEREN ALLER EV-KOMPONENTEN <EVMINB  NOCH AUFTRETEN.         C 
C	     ENTFALLEN FUER  EYE=0. (ISOTOP OHNE KERNSPIN)		                      C 
C     10     WIE BISHER, ABER FUER  EYE=0.  BRAUCHEN  F1,F2  NICHT ANGEGEBEN  C 
C	     ZU WERDEN; M1,M2  ENTSPRECHEN DANN  MJ1,MJ2		                        C 
C									                                                            C 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC 
