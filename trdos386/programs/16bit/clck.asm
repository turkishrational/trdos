; ****************************************************************************
; clck.asm (for MSDOS)
; ----------------------------------------------------------------------------
; CLCK.COM ! VGA (Mode 13h) CLOCK DEMO program ! NASM version by Erdogan TAN
; (04/11/2017)

; ************************************************************************
; *  This is a small example of VGA mode 13h screen programming.
; *  It has the intent to show how to program in the specified mode,
; *  therefore does not contain small or efficient code.
; *  It is however quite fast, though probably could be a bit faster.
; *
; *  You may use this code as you would like.
; *  As always, If this code crashes you machine in anyway I am
; *  not held responsible.  Use at your own risk.
; *   Benjamin David Lunt
; *   Forever Young Software
; *   Copyright 1984-2015
; *   All rights reserved.
; *    08 Dec 1998
; *    NBASM
; *

DSpc      equ 42

[Bits 16]  ; no 32 bit stuff needed
[Org 100h]
           mov  ax,0013h
           int  10h
           mov  ax,0A000h
           mov  es,ax
           xor  di,di
           mov  cx,32767
           mov  ax,[BackG]
           rep  stosw

           mov  di,13860                ; print full colons
           call FColon                  ;
           mov  di,13970                ;  + 110
           call FColon                  ;
           mov  di,20900                ;  + 7040
           call FColon                  ;
           mov  di,21010                ;  + 110 + 7040
           call FColon                  ;

MyLoop:    call GetTime                 ; get the current time
           mov  di,9927                 ; print hour
           mov  al,[Hour]               ;
           call DoHour                  ;
           mov  di,10037                ; print minutes
           mov  al,[Min]                  ;
           call DoHour                  ;
           mov  di,10146                ; print seconds
           mov  al,[Sec]                  ;
           call DoHour                  ;
           mov  ah,01h                  ; see if key pressed
           int  16h                     ;
           jz   short MyLoop            ; loop if no key pressed
           xor  ah,ah                   ; key pressed so clear it
           int  16h                     ;   from keyboard
           
Done:      mov  ax,0003h
           int  10h
           mov  ax,4C00h
           int  21h
;here:
;	   jmp	short here

DoVLine:
	   push	ax
	   push cx
	   push si
	   push di
 	
           mov  si, VLine
           mov  cx,25
VLoop1:    push cx
           mov  cx,10
VLoop2:    lodsb
           or   al,al
           jz   short VNoDis
           stosb
           dec  di
VNoDis:    inc  di
           loop VLoop2
           add  di,310
           pop  cx
           loop VLoop1

	   pop  di
 	   pop  si
 	   pop  cx
	   pop 	ax
	
           retn

DoRVLine:
	   push	ax
	   push cx
	   push si
	   push di

	   mov  si, VLine
           mov  cx,25
VLoop1r:   push cx
           mov  cx,10
VLoop2r:   lodsb
           or   al,al
           jz   short VNoDisr
           mov  ax,[BackG]
           stosb
           dec  di
VNoDisr:   inc  di
           loop VLoop2r
           add  di,310
           pop  cx
           loop VLoop1r

	   pop  di
 	   pop  si
 	   pop  cx
	   pop 	ax

           retn
DoHLine:
	   push cx
	   push si
	   push di

	   mov  si, HLine
           mov  cx,07
HLoop1:    push cx
           mov  cx,30
HLoop2:    lodsb
           or   al,al
           jz   short HNoDis
           stosb
           dec  di
HNoDis:    inc  di
           loop HLoop2
           add  di,290
           pop  cx
           loop HLoop1

	   pop  di
 	   pop  si
 	   pop  cx

           retn
DoRHLine:
	   push cx
	   push si
	   push di

           mov  si, HLine
           mov  cx,07
HLoop1r:   push cx
           mov  cx,30
HLoop2r:   lodsb
           or   al,al
           jz   short HNoDisr
           mov  ax,[BackG]
           stosb
           dec  di
HNoDisr:   inc  di
           loop HLoop2r
           add  di,290
           pop  cx
           loop HLoop1r

	   pop  di
 	   pop  si
 	   pop  cx

           retn

GetTime:
	   push ax
	   push cx
	   push dx

           mov  ah,2Ch
           int  21h
           mov  [Hour],ch
           mov  [Min],cl
           mov  [Sec],dh

	   pop	dx
	   pop	cx
	   pop	ax

           retn

FColon:
	   push ax
	   push si
	   push di

	   mov  si, FCln
           mov  cx,07
FLoop1:    push cx
           mov  cx,07
FLoop2:    lodsb
           or   al,al
           jz   short FNoDis
           stosb
           dec  di
FNoDis:    inc  di
           loop FLoop2
           add  di,313
           pop  cx
           loop FLoop1

	   pop	di
	   pop	si
	   pop	ax

           retn

DoHour:
	   push ax
	   push	di

           or   al,al
           jnz  short Not0
           call Do0
           add  di,DSpc
           call Do0
           jmp  HDone
Not0:      cmp  al,01
           jne  short Not1
           call Do0
           add  di,DSpc
           call Do1
           jmp  HDone
Not1:      cmp  al,02
           jne  short Not2
           call Do0
           add  di,DSpc
           call Do2
           jmp  HDone
Not2:      cmp  al,03
           jne  short Not3
           call Do0
           add  di,DSpc
           call Do3
           jmp  HDone
Not3:      cmp  al,04
           jne  short Not4
           call Do0
           add  di,DSpc
           call Do4
           jmp  HDone
Not4:      cmp  al,05
           jne  short Not5
           call Do0
           add  di,DSpc
           call Do5
           jmp  HDone
Not5:      cmp  al,06
           jne  short Not6
           call Do0
           add  di,DSpc
           call Do6
           jmp  HDone
Not6:      cmp  al,07
           jne  short Not7
           call Do0
           add  di,DSpc
           call Do7
           jmp  HDone
Not7:      cmp  al,08
           jne  short Not8
           call Do0
           add  di,DSpc
           call Do8
           jmp  HDone
Not8:      cmp  al,09
           jne  short Not9
           call Do0
           add  di,DSpc
           call Do9
           jmp  HDone
Not9:      cmp  al,10
           jne  short Not10
           call Do1
           add  di,DSpc
           call Do0
           jmp  HDone
Not10:     cmp  al,11
           jne  short Not11
           call Do1
           add  di,DSpc
           call Do1
           jmp  HDone
Not11:     cmp  al,12
           jne  short Not12
           call Do1
           add  di,DSpc
           call Do2
           jmp  HDone
Not12:     cmp  al,13
           jne  short Not13
           call Do1
           add  di,DSpc
           call Do3
           jmp  HDone
Not13:     cmp  al,14
           jne  short Not14
           call Do1
           add  di,DSpc
           call Do4
           jmp  HDone
Not14:     cmp  al,15
           jne  short Not15
           call Do1
           add  di,DSpc
           call Do5
           jmp  HDone
Not15:     cmp  al,16
           jne  short Not16
           call Do1
           add  di,DSpc
           call Do6
           jmp  HDone
Not16:     cmp  al,17
           jne  short Not17
           call Do1
           add  di,DSpc
           call Do7
           jmp  HDone
Not17:     cmp  al,18
           jne  short Not18
           call Do1
           add  di,DSpc
           call Do8
           jmp  HDone
Not18:     cmp  al,19
           jne  short Not19
           call Do1
           add  di,DSpc
           call Do9
           jmp  HDone
Not19:     cmp  al,20
           jne  short Not20
           call Do2
           add  di,DSpc
           call Do0
           jmp  HDone
Not20:     cmp  al,21
           jne  short Not21
           call Do2
           add  di,DSpc
           call Do1
           jmp  HDone
Not21:     cmp  al,22
           jne  short Not22
           call Do2
           add  di,DSpc
           call Do2
           jmp  HDone
Not22:     cmp  al,23
           jne  short Not23
           call Do2
           add  di,DSpc
           call Do3
           jmp  HDone
Not23:     cmp  al,24
           jne  short Not24
           call Do2
           add  di,DSpc
           call Do4
           jmp  HDone
Not24:     cmp  al,25
           jne  short Not25
           call Do2
           add  di,DSpc
           call Do5
           jmp  HDone
Not25:     cmp  al,26
           jne  short Not26
           call Do2
           add  di,DSpc
           call Do6
           jmp  HDone
Not26:     cmp  al,27
           jne  short Not27
           call Do2
           add  di,DSpc
           call Do7
           jmp  HDone
Not27:     cmp  al,28
           jne  short Not28
           call Do2
           add  di,DSpc
           call Do8
           jmp  HDone
Not28:     cmp  al,29
           jne  short Not29
           call Do2
           add  di,DSpc
           call Do9
           jmp  HDone
Not29:     cmp  al,30
           jne  short Not30
           call Do3
           add  di,DSpc
           call Do0
           jmp  HDone
Not30:     cmp  al,31
           jne  short Not31
           call Do3
           add  di,DSpc
           call Do1
           jmp  HDone
Not31:     cmp  al,32
           jne  short Not32
           call Do3
           add  di,DSpc
           call Do2
           jmp  HDone
Not32:     cmp  al,33
           jne  short Not33
           call Do3
           add  di,DSpc
           call Do3
           jmp  HDone
Not33:     cmp  al,34
           jne  short Not34
           call Do3
           add  di,DSpc
           call Do4
           jmp  HDone
Not34:     cmp  al,35
           jne  short Not35
           call Do3
           add  di,DSpc
           call Do5
           jmp  HDone
Not35:     cmp  al,36
           jne  short Not36
           call Do3
           add  di,DSpc
           call Do6
           jmp  HDone
Not36:     cmp  al,37
           jne  short Not37
           call Do3
           add  di,DSpc
           call Do7
           jmp  HDone
Not37:     cmp  al,38
           jne  short Not38
           call Do3
           add  di,DSpc
           call Do8
           jmp  HDone
Not38:     cmp  al,39
           jne  short Not39
           call Do3
           add  di,DSpc
           call Do9
           jmp  HDone
Not39:     cmp  al,40
           jne  short Not40
           call Do4
           add  di,DSpc
           call Do0
           jmp  HDone
Not40:     cmp  al,41
           jne  short Not41
           call Do4
           add  di,DSpc
           call Do1
           jmp  HDone
Not41:     cmp  al,42
           jne  short Not42
           call Do4
           add  di,DSpc
           call Do2
           jmp  HDone
Not42:     cmp  al,43
           jne  short Not43
           call Do4
           add  di,DSpc
           call Do3
           jmp  HDone
Not43:     cmp  al,44
           jne  short Not44
           call Do4
           add  di,DSpc
           call Do4
           jmp  HDone
Not44:     cmp  al,45
           jne  short Not45
           call Do4
           add  di,DSpc
           call Do5
           jmp  HDone
Not45:     cmp  al,46
           jne  short Not46
           call Do4
           add  di,DSpc
           call Do6
           jmp  HDone
Not46:     cmp  al,47
           jne  short Not47
           call Do4
           add  di,DSpc
           call Do7
           jmp  HDone
Not47:     cmp  al,48
           jne  short Not48
           call Do4
           add  di,DSpc
           call Do8
           jmp  HDone
Not48:     cmp  al,49
           jne  short Not49
           call Do4
           add  di,DSpc
           call Do9
           jmp  HDone
Not49:     cmp  al,50
           jne  short Not50
           call Do5
           add  di,DSpc
           call Do0
           jmp  HDone
Not50:     cmp  al,51
           jne  short Not51
           call Do5
           add  di,DSpc
           call Do1
           jmp  short HDone
Not51:     cmp  al,52
           jne  short Not52
           call Do5
           add  di,DSpc
           call Do2
           jmp  short HDone
Not52:     cmp  al,53
           jne  short Not53
           call Do5
           add  di,DSpc
           call Do3
           jmp  short HDone
Not53:     cmp  al,54
           jne  short Not54
           call Do5
           add  di,DSpc
           call Do4
           jmp  short HDone
Not54:     cmp  al,55
           jne  short Not55
           call Do5
           add  di,DSpc
           call Do5
           jmp  short HDone
Not55:     cmp  al,56
           jne  short Not56
           call Do5
           add  di,DSpc
           call Do6
           jmp  short HDone
Not56:     cmp  al,57
           jne  short Not57
           call Do5
           add  di,DSpc
           call Do7
           jmp  short HDone
Not57:     cmp  al,58
           jne  short Not58
           call Do5
           add  di,DSpc
           call Do8
           jmp  short HDone
Not58:     cmp  al,59
           jne  short HDone
           call Do5
           add  di,DSpc
           call Do9
HDone:   
	   pop	di
	   pop	ax

	   retn

Do0:
	   push ax
	   push	di

           call DoVLine                 ; 1
           push di
           add  di,8000
           call DoVLine                 ; 7
           pop  di
           inc  di
           inc  di
           push di
           sub  di,957
           call DoHLine                 ; 2
           add  di,8000
           call DoRHLine                ; 4
           add  di,8000
           call DoHLine                 ; 6
           pop  di
           add  di,29
           call DoVLine                 ; 3
           add  di,8000
           call DoVLine                 ; 5

	   pop	di
	   pop	ax

           retn

Do1:
	   push ax
	   push	di

           call DoRVLine                ; 1
           push di
           add  di,8000
           call DoRVLine                ; 7
           pop  di
           inc  di
           inc  di
           push di
           sub  di,957
           call DoRHLine                ; 2
           add  di,8000
           call DoRHLine                ; 4
           add  di,8000
           call DoRHLine                ; 6
           pop  di
           add  di,29
           call DoVLine                 ; 3
           add  di,8000
           call DoVLine                 ; 5

	   pop	di
	   pop	ax

           retn
Do2:
	   push ax
	   push	di

           call DoRVLine                ; 1
           push di
           add  di,8000
           call DoVLine                 ; 7
           pop  di
           inc  di
           inc  di
           push di
           sub  di,957
           call DoHLine                 ; 2
           add  di,8000
           call DoHLine                 ; 4
           add  di,8000
           call DoHLine                 ; 6
           pop  di
           add  di,29
           call DoVLine                 ; 3
           add  di,8000
           call DoRVLine                ; 5

	   pop	di
	   pop	ax

           retn
Do3:
	   push ax
	   push	di

           call DoRVLine                ; 1
           push di
           add  di,8000
           call DoRVLine                ; 7
           pop  di
           inc  di
           inc  di
           push di
           sub  di,957
           call DoHLine                 ; 2
           add  di,8000
           call DoHLine                 ; 4
           add  di,8000
           call DoHLine                 ; 6
           pop  di
           add  di,29
           call DoVLine                 ; 3
           add  di,8000
           call DoVLine                 ; 5

	   pop	di
	   pop	ax

           retn
Do4:
	   push ax
	   push	di

           call DoVLine                 ; 1
           push di
           add  di,8000
           call DoRVLine                ; 7
           pop  di
           inc  di
           inc  di
           push di
           sub  di,957
           call DoRHLine                ; 2
           add  di,8000
           call DoHLine                 ; 4
           add  di,8000
           call DoRHLine                ; 6
           pop  di
           add  di,29
           call DoVLine                 ; 3
           add  di,8000
           call DoVLine                 ; 5

	   pop	di
	   pop	ax

           retn
Do5:
	   push ax
	   push	di

           call DoVLine                 ; 1
           push di
           add  di,8000
           call DoRVLine                ; 7
           pop  di
           inc  di
           inc  di
           push di
           sub  di,957
           call DoHLine                 ; 2
           add  di,8000
           call DoHLine                 ; 4
           add  di,8000
           call DoHLine                 ; 6
           pop  di
           add  di,29
           call DoRVLine                ; 3
           add  di,8000
           call DoVLine                 ; 5

	   pop	di
	   pop	ax

           retn
Do6:
	   push ax
	   push	di

           call DoVLine                 ; 1
           push di
           add  di,8000
           call DoVLine                 ; 7
           pop  di
           inc  di
           inc  di
           push di
           sub  di,957
           call DoHLine                 ; 2
           add  di,8000
           call DoHLine                 ; 4
           add  di,8000
           call DoHLine                 ; 6
           pop  di
           add  di,29
           call DoRVLine                ; 3
           add  di,8000
           call DoVLine                 ; 5

	   pop	di
	   pop	ax

           retn
Do7:
	   push ax
	   push	di

           call DoRVLine                ; 1
           push di
           add  di,8000
           call DoRVLine                ; 7
           pop  di
           inc  di
           inc  di
           push di
           sub  di,957
           call DoHLine                 ; 2
           add  di,8000
           call DoRHLine                ; 4
           add  di,8000
           call DoRHLine                ; 6
           pop  di
           add  di,29
           call DoVLine                 ; 3
           add  di,8000
           call DoVLine                 ; 5

	   pop	di
	   pop	ax

           retn
Do8:
	   push ax
	   push	di

           call DoVLine                 ; 1
           push di
           add  di,8000
           call DoVLine                 ; 7
           pop  di
           inc  di
           inc  di
           push di
           sub  di,957
           call DoHLine                 ; 2
           add  di,8000
           call DoHLine                 ; 4
           add  di,8000
           call DoHLine                 ; 6
           pop  di
           add  di,29
           call DoVLine                 ; 3
           add  di,8000
           call DoVLine                 ; 5

	   pop	di
	   pop	ax

           retn
Do9:
	   push ax
	   push	di

           call DoVLine                 ; 1
           push di
           add  di,8000
           call DoRVLine                ; 7
           pop  di
           inc  di
           inc  di
           push di
           sub  di,957
           call DoHLine                 ; 2
           add  di,8000
           call DoHLine                 ; 4
           add  di,8000
           call DoHLine                 ; 6
           pop  di
           add  di,29
           call DoVLine                 ; 3
           add  di,8000
           call DoVLine                 ; 5

	   pop	di
	   pop	ax

           retn

Hour:     db  00h
Min:      db  00h
Sec:      db  00h
BackG:    dw  0707h
VLine:    db  00,00,00,00,15,00,00,00,00,00
          db  00,00,00,15,07,08,00,00,00,00
          db  00,00,15,07,07,07,08,00,00,00
          db  00,15,07,07,07,07,07,08,00,00
          db  15,07,07,07,07,07,07,07,08,00
          db  15,07,07,07,07,07,07,07,08,00
          db  15,07,07,07,07,07,07,07,08,00
          db  15,07,07,07,07,07,07,07,08,00
          db  15,07,07,07,07,07,07,07,08,00
          db  15,07,07,07,07,07,07,07,08,00
          db  15,07,07,07,07,07,07,07,08,00
          db  15,07,07,07,07,07,07,07,08,00
          db  15,07,07,07,07,07,07,07,08,00
          db  15,07,07,07,07,07,07,07,08,00
          db  15,07,07,07,07,07,07,07,08,00
          db  15,07,07,07,07,07,07,07,08,00
          db  15,07,07,07,07,07,07,07,08,00
          db  15,07,07,07,07,07,07,07,08,00
          db  15,07,07,07,07,07,07,07,08,00
          db  15,07,07,07,07,07,07,07,08,00
          db  15,07,07,07,07,07,07,07,08,00
          db  00,08,07,07,07,07,07,08,00,00
          db  00,00,08,07,07,07,08,00,00,00
          db  00,00,00,08,07,08,00,00,00,00
          db  00,00,00,00,08,00,00,00,00,00
HLine:    db  00,00,00,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,00,00,00
          db  00,00,15,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,08,00,00
          db  00,15,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,08,00
          db  15,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,08
          db  00,08,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,08,00
          db  00,00,08,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,07,08,00,00
          db  00,00,00,08,08,08,08,08,08,08,08,08,08,08,08,08,08,08,08,08,08,08,08,08,08,08,08,00,00,00
FCln:     db  00,00,00,15,00,00,00
          db  00,00,15,07,08,00,00
          db  00,15,07,07,07,08,00
          db  15,07,07,07,07,07,08
          db  00,15,07,07,07,08,00
          db  00,00,15,07,08,00,00
          db  00,00,00,08,00,00,00

;         2
;       -----
;      |     |
;    1 |     | 3
;       -----  4
;      |     |
;    7 |     | 5
;       -----
;         6
