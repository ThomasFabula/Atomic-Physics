page ,96
code segment 'code'
;
assume   cs:code,ss:nothing,ds:nothing,es:nothing
;
         ORG     100h
;
Scrprn proc far
;
start :  JMP     L019F
;
; Enable Interrupts und Save State
;
intin :  STI
         PUSH    AX
         PUSH    BX
         PUSH    CX
         PUSH    DX
         PUSH    SI
         PUSH    DS
         PUSH    ES
;
; Lade ES mit Bildschirm Segment
;
         MOV     AX,0B000h
         MOV     ES,AX
;
; Setze DS auf Dos Workarea
;
         MOV     AX,0050h
         MOV     DS,AX
;
; Abfragen eines Dos Steuerflags
; ? Prtscr Funktion aktiv
;
         MOV     BX,0000h
         MOV     AL,01h
         CMP     [BX],AL
;
; Falls aktiv verlassen der Interrupt - Routine
;
         JZ      l0197
;
; Steuerflag setzen ( disable Prtscr )
;
         MOV     [BX],AL
;
; Console Input ohne Echo
;
         MOV     AH,08h
         INT     21h
         CMP     AL,31h
         JNZ     l0130
;
; bei "1" ES um 800h erh”hen
;
         MOV     BX,ES
         ADD     BX,0800h
         MOV     ES,BX
l0130 :  MOV     BX,7E3Ch
         MOV     SI,0000h
;
; Initialisierung des Druckers; Ausgabe von ESC,"3",18h
; Paper feed 24/216" setting
;
         MOV     DL,1Bh      ; ESC
         MOV     AH,05h
         INT     21h
         MOV     DL,33h      ; "3"
         MOV     AH,05h
         INT     21h
         MOV     DL,18h      ; 18h
         MOV     AH,05h
         INT     21h
;
; Begin des Ausdruckens einer Zeile
; Steuerzeichen fuer Graphik an Drucker ausgeben
;
l0148 :  MOV     DL,0Ah      ; LF
         MOV     AH,05h
         INT     21h
         MOV     DL,0Dh      ; CR
         MOV     AH,05h
         INT     21h
         MOV     DL,1Bh      ; ESC
         MOV     AH,05h
         INT     21h
         MOV     DL,4Bh      ; "K"
         MOV     AH,05h
         INT     21h
         MOV     DL,5Ch      ; 348
         MOV     AH,05h
         INT     21h
         MOV     DL,01h      ;
         MOV     AH,05h
         INT     21h
;
; CX enth„lt die Zahl Pixel pro Zeile
; BX Startadresse im Bildspeicher
;
         MOV     CX,015Ch
         MOV     BX,7E3Ch
;
; Schleife zum Drucken einer Zeile
;
l0172 :  MOV     DL,ES:[BX+SI]
         MOV     AH,05h
         INT     21h
         SUB     BH,20h
         JNS     l0184
         AND     BH,7Fh
         SUB     BX,+5Ah
;
l0184 :  LOOP    l0172
         INC     SI
         CMP     SI,+5Ah
         JNZ     l0148
;
; Steuerbyte ruecksetzen ( Enable Prtscr )
;
         MOV     AX,0050h
         MOV     DS,AX
         MOV     BX,0000
         MOV     BYTE PTR [BX],00
;
; Restore State
;
l0197 :  POP     ES
         POP     DS
         POP     SI
         POP     DX
         POP     CX
         POP     BX
         POP     AX
         IRET
;
; Einsprung fuer Installation; Kopieren von CS nach ES & DS
;
l019f :  MOV     AX,CS
         MOV     DS,AX
         MOV     ES,AX
;
; Laden der Startadresse der Interrupt Routine
;
         LEA     DX,[intin]
;
; Aufruf des Dos Calls fuer Laden einer Interrupt - Routine
;      AH    Call Nr
;      AL    Interruptnr die ueberschrieben werden soll
;      DS:DX Adresse des Interrupt-Handlers
;
         MOV     AL,05h
         MOV     AH,25h
         INT     21h
;
;  Aufruf der Dos Funktion " Terminate but Stay Resident "
;  DS:DX Adresse von erstem freiem Speicherplatz
;
         LEA     DX,[l019F]
         INT     27h
         RET
;
scrprn   endp
code     ends
         end     start
