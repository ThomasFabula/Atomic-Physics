page ,96
doscall  macro intno
	 mov	 ah,intno
	 int	 21h
	 endm
;
prnout	 equ	 05h
count	 equ	 04h
prnmode  equ	 "Z"
ESC	 equ	 1bh
nfeed	 equ	 18h
CR	 equ	 0dh
LF	 equ	 0ah
linescr  equ	 15ch
lineprn  equ	 ( linescr * count)
;
code segment 'code'
;
assume	 cs:code,ss:nothing,ds:nothing,es:nothing
;
	 ORG	 100h
;
Scrprn proc far
;
start :  JMP	 L019F
help :	 JMP	 l0197
;
; Enable Interrupts und Save State
;
intin :  STI
	 PUSH	 AX
	 PUSH	 BX
	 PUSH	 CX
	 PUSH	 DX
	 PUSH	 SI
	 PUSH	 DS
	 PUSH	 ES
;
; Lade ES mit Bildschirm Segment
;
	 MOV	 AX,0B000h
	 MOV	 ES,AX
;
; Setze DS auf Dos Workarea
;
	 MOV	 AX,0050h
	 MOV	 DS,AX
;
; Abfragen eines Steuerflags
; ? Prtscr Funktion aktiv
;
	 MOV	 BX,0000h
	 MOV	 AL,01h
	 CMP	 [BX],AL
;
; Falls aktiv verlassen der Interrupt - Routine
;
	 JZ	 help
;
; Steuerflag setzen ( disable Prtscr )
;
	 MOV	 [BX],AL
;
; Console Input ohne Echo
;
	 doscall 08h
	 CMP	 AL,"1"
	 JNZ	 l0130
;
; bei "1" ES um 800h erhöhen
;
	 MOV	 BX,ES
	 ADD	 BX,0800h
	 MOV	 ES,BX
l0130 :  MOV	 BX,7E3Ch
	 MOV	 SI,0000h
;
; Initialisierung des Druckers.  Ausgabe von ESC,"3",18h
; Paper feed 24/216" setting
;
	 MOV	 DL,esc
	 doscall prnout
	 MOV	 DL,"3"
	 doscall prnout
	 MOV	 DL,nfeed
	 doscall prnout
;
; Begin des Ausdruckens einer Zeile
; Steuerzeichen fuer Graphik an Drucker ausgeben
;
l0148 :  MOV	 DL,LF
	 doscall prnout
	 MOV	 DL,CR
	 doscall prnout
	 MOV	 DL,ESC
	 doscall prnout
	 MOV	 DL,prnmode
	 doscall prnout
	 MOV	 DL,low lineprn
	 doscall prnout
	 MOV	 DL,high lineprn
	 doscall prnout
;
; CX enthält die Zahl Pixel pro Zeile
; BX enthält Startadresse des Bilspeichers
;
	 MOV	 CX,linescr
	 MOV	 BX,7E3Ch
;
; Schleife zum Drucken einer Zeile
;
l0172 :
    rept count
	 MOV	 DL,ES:[BX+SI]
	 doscall prnout
    endm
	 SUB	 BH,20h
	 JNS	 l0184
	 AND	 BH,7Fh
	 SUB	 BX,+5Ah
;
l0184 :  LOOP	 l0172
	 INC	 SI
	 CMP	 SI,+5Ah
	 JNZ	 l0148
;
; Steuerbyte ruecksetzen ( Enable Prtscr )
;
	 MOV	 AX,0050h
	 MOV	 DS,AX
	 MOV	 BX,0000
	 MOV	 BYTE PTR [BX],00
;
; Restore State
;
l0197 :  POP	 ES
	 POP	 DS
	 POP	 SI
	 POP	 DX
	 POP	 CX
	 POP	 BX
	 POP	 AX
	 IRET
;
; Einsprung fuer Installation : Kopieren von CS nach ES & DS
;
l019f :  MOV	 AX,CS
	 MOV	 DS,AX
	 MOV	 ES,AX
;
; Laden der Startadresse der Interrupt Routine
;
	 LEA	 DX,[intin]
;
; Aufruf des Dos Calls fuer Laden einer Interrupt - Routine
;	 AH    Call Nr
;	 AL    Interruptnr die ueberschrieben werden soll
;	 DS:DX Adresse des Interrupt-Handlers
;
	 MOV	 AL,05h
	 doscall 25h
;
;  Aufruf der Dos Funktion " Terminate but Stay Resident "
;  DS:DX Adresse von erstem freiem Speicherplatz
;
	 LEA	 DX,[l019F]
	 INT	 27h
	 RET
;
scrprn	 endp
code	 ends
	 end	 start
