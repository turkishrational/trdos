; ****************************************************************************
; clck.s (for TRDOS 386)
; ----------------------------------------------------------------------------
; CLCK.PRG ! VGA DEMO program by Erdogan TAN
;
; 04/11/2017
;
; [ Last Modification: 04/11/2017 ] 
;
; Derived from 'clck.asm' by Benjamin David Lunt (08/12/1998)
;
; Assembler: NASM 2.11
; ----------------------------------------------------------------------------
;	   nasm  clck.s -l clck.txt -o CLCK.PRG	
; ****************************************************************************

; ****************************************************************************
; clck.asm (for MSDOS)
; ----------------------------------------------------------------------------
; CLCK.COM ! VGA (Mode 13h) CLOCK DEMO program ! NASM version by Erdogan TAN
; (04/11/2017)

;------------------------------------------------------------------------------
; TRDOS 386, TRDOS v2.0
;------------------------------------------------------------------------------

; 01/03/2017
; 16/10/2016
; 29/04/2016
; TRDOS 386 system calls (temporary list!)
_ver 	equ 0
_exit 	equ 1
_fork 	equ 2
_read 	equ 3
_write	equ 4
_open	equ 5
_close 	equ 6
_wait 	equ 7
_creat 	equ 8
_link 	equ 9
_unlink	equ 10
_exec	equ 11
_chdir	equ 12
_time 	equ 13
_mkdir 	equ 14
_chmod	equ 15
_chown	equ 16
_break	equ 17
_stat	equ 18
_seek	equ 19
_tell 	equ 20
_mount	equ 21
_umount	equ 22
_setuid	equ 23
_getuid	equ 24
_stime	equ 25
_quit	equ 26	
_intr	equ 27
_fstat	equ 28
_emt 	equ 29
_mdate 	equ 30
_video 	equ 31
_audio	equ 32
_timer	equ 33
_sleep	equ 34
_msg    equ 35
_geterr	equ 36
_fpsave	equ 37
_pri	equ 38
_rele	equ 39
_fff	equ 40
_fnf	equ 41
_alloc	equ 42
_dalloc equ 43
_calbac equ 44		

%macro sys 1-4
    ; 29/04/2016 - TRDOS 386 (TRDOS v2.0)	
    ; 03/09/2015	
    ; 13/04/2015
    ; Retro UNIX 386 v1 system call.	
    %if %0 >= 2   
        mov ebx, %2
        %if %0 >= 3    
            mov ecx, %3
            %if %0 = 4
               mov edx, %4   
            %endif
        %endif
    %endif
    mov eax, %1
    ;int 30h
    int 40h ; TRDOS 386 (TRDOS v2.0)	   
%endmacro

; TRDOS 386 (and Retro UNIX 386 v1) system call format:
; sys systemcall (eax) <arg1 (ebx)>, <arg2 (ecx)>, <arg3 (edx)>

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

[Bits 32]  ; TRDOS 386 - 32 bit code
[Org 0]
           mov  ax,0013h
           ;int	10h
	   int  31h ; TRDOS 386 VIDEO BIOS Interrupt

	   ; DIRECT VGA MEMORY ACCESS
	   ; bl = 0, bh = 5
	   ; Direct access/map to VGA memory (0A0000h)

	   sys	_video, 0500h
	   cmp	eax, 0A0000h
	   jne	Done
	  	
	   mov  edi,0A0000h
           mov  ecx,32767
           mov  ax,[BackG]
           rep  stosw

           mov  edi,0A0000h+13860       ; print full colons
           call FColon                  ;
           mov  edi,0A0000h+13970       ;  + 110
           call FColon                  ;
           mov  edi,0A0000h+20900       ;  + 7040
           call FColon                  ;
           mov  edi,0A0000h+21010       ;  + 110 + 7040
           call FColon                  ;

MyLoop:
	   call GetTime                 ; get the current time
		
           mov  edi,0A0000h+9927        ; print hour
           mov  al,[Hour]               ;
           call DoHour                  ;
           mov  edi,0A0000h+10037       ; print minutes
           mov  al,[Min]                ;
           call DoHour                  ;
           mov  edi,0A0000h+10146       ; print seconds
           mov  al,[Sec]                ;
           call DoHour                  ;

           mov  ah,01h                  ; see if key pressed
           ;int 16h
	   int  32h ; TRDOS 386 keyboard interrupt
           jz   short MyLoop            ; loop if no key pressed
           xor  ah,ah                   ; key pressed so clear it
           ;int 16h                     ;   from keyboard
	   int  32h ; TRDOS 386 keyboard interrupt
           
Done:      mov  ax,0003h
           ;int 10h
	   int  31h  ; TRDOS 386 video bios interrupt	

           ;mov ax,4C00h
           ;int 21h

	   sys _exit	
;here:
;	   jmp	short here

DoVLine:
	   push	eax
	   push ecx
	   push esi
	   push edi
 	
           mov  esi,VLine
           mov  cl,25
VLoop1:    push ecx
           mov  cl,10
VLoop2:    lodsb
           or   al,al
           jz   short VNoDis
           stosb
           dec  edi
VNoDis:    inc  edi
           loop VLoop2
           add  di,310
           pop  ecx
           loop VLoop1

	   pop  edi
 	   pop  esi
 	   pop  ecx
	   pop 	eax
	
           retn

DoRVLine:
	   push	eax
	   push ecx
	   push esi
	   push edi

	   mov  esi,VLine
           mov  cl,25
VLoop1r:   push ecx
           mov  cl,10
VLoop2r:   lodsb
           or   al,al
           jz   short VNoDisr
           mov  ax,[BackG]
           stosb
           dec  edi
VNoDisr:   inc  edi
           loop VLoop2r
           add  di,310
           pop  ecx
           loop VLoop1r

	   pop  edi
 	   pop  esi
 	   pop  ecx
	   pop 	eax

           retn
DoHLine:
	   push ecx
	   push esi
	   push edi

	   mov  esi,HLine
           mov  cl,07
HLoop1:    push ecx
           mov  cl,30
HLoop2:    lodsb
           or   al,al
           jz   short HNoDis
           stosb
           dec  edi
HNoDis:    inc  edi
           loop HLoop2
           add  di,290
           pop  ecx
           loop HLoop1

	   pop  edi
 	   pop  esi
 	   pop  ecx

           retn
DoRHLine:
	   push ecx
	   push esi
	   push edi

           mov  esi,HLine
           mov  cl,07
HLoop1r:   push ecx
           mov  cl,30
HLoop2r:   lodsb
           or   al,al
           jz   short HNoDisr
           mov  ax,[BackG]
           stosb
           dec  edi
HNoDisr:   inc  edi
           loop HLoop2r
           add  di,290
           pop  ecx
           loop HLoop1r

	   pop  edi
 	   pop  esi
 	   pop  ecx

           retn

GetTime:
	   push eax
	   push ecx
	   push edx

           ;mov ah,2Ch
           ;int 21h

	   ;mov [Hour],ch
           ;mov [Min],cl
           ;mov [Sec],dh
	
	   mov	ah,02h  
	   int  35h  ; TRDOS 386 date & time ýnterrupt

	   mov	al,ch
	   shr	al,4
	   mov	ah,10
	   mul  ah
	   and  ch,0Fh
	   add	al,ch	 	   	
	   mov  [Hour],al

	   mov	al,cl
	   shr	al,4
	   mov	ah,10
	   mul  ah 			   	
	   and  cl,0Fh
	   add	al,cl	
           mov  [Min],al

	   mov	al,dh
	   shr	al,4
	   mov	ah,10
	   mul  ah 
	   and  dh,0Fh
	   add	al,dh	
           mov  [Sec],al

	   pop	edx
	   pop	ecx
	   pop	eax

           retn

FColon:
	   push eax
	   push esi
	   push edi

	   mov  esi,FCln
           mov  cl,07
FLoop1:    push ecx
           mov  cl,07
FLoop2:    lodsb
           or   al,al
           jz   short FNoDis
           stosb
           dec  edi
FNoDis:    inc  edi
           loop FLoop2
           add  di,313
           pop  ecx
           loop FLoop1

	   pop	edi
	   pop	esi
	   pop	eax

           retn

DoHour:
	   push eax
	   push	edi

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
           jmp  HDone
Not51:     cmp  al,52
           jne  short Not52
           call Do5
           add  di,DSpc
           call Do2
           jmp  HDone
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
	   pop	edi
	   pop	eax

	   retn

Do0:
	   push eax
	   push	edi

           call DoVLine                 ; 1
           push edi
           add  di,8000
           call DoVLine                 ; 7
           pop  edi
           inc  edi
           inc  edi
           push edi
           sub  di,957
           call DoHLine                 ; 2
           add  di,8000
           call DoRHLine                ; 4
           add  di,8000
           call DoHLine                 ; 6
           pop  edi
           add  di,29
           call DoVLine                 ; 3
           add  di,8000
           call DoVLine                 ; 5

	   pop	edi
	   pop	eax

           retn

Do1:
	   push eax
	   push	edi

           call DoRVLine                ; 1
           push edi
           add  di,8000
           call DoRVLine                ; 7
           pop  edi
           inc  edi
           inc  edi
           push edi
           sub  di,957
           call DoRHLine                ; 2
           add  di,8000
           call DoRHLine                ; 4
           add  di,8000
           call DoRHLine                ; 6
           pop  edi
           add  di,29
           call DoVLine                 ; 3
           add  di,8000
           call DoVLine                 ; 5

	   pop	edi
	   pop	eax

           retn
Do2:
	   push eax
	   push	edi

           call DoRVLine                ; 1
           push edi
           add  di,8000
           call DoVLine                 ; 7
           pop  edi
           inc  edi
           inc  edi
           push edi
           sub  di,957
           call DoHLine                 ; 2
           add  di,8000
           call DoHLine                 ; 4
           add  di,8000
           call DoHLine                 ; 6
           pop  edi
           add  di,29
           call DoVLine                 ; 3
           add  di,8000
           call DoRVLine                ; 5

	   pop	edi
	   pop	eax

           retn
Do3:
	   push eax
	   push	edi

           call DoRVLine                ; 1
           push edi
           add  di,8000
           call DoRVLine                ; 7
           pop  edi
           inc  edi
           inc  edi
           push edi
           sub  di,957
           call DoHLine                 ; 2
           add  di,8000
           call DoHLine                 ; 4
           add  di,8000
           call DoHLine                 ; 6
           pop  edi
           add  di,29
           call DoVLine                 ; 3
           add  di,8000
           call DoVLine                 ; 5

	   pop	edi
	   pop	eax

           retn
Do4:
	   push eax
	   push	edi

           call DoVLine                 ; 1
           push edi
           add  di,8000
           call DoRVLine                ; 7
           pop  edi
           inc  edi
           inc  edi
           push edi
           sub  di,957
           call DoRHLine                ; 2
           add  di,8000
           call DoHLine                 ; 4
           add  di,8000
           call DoRHLine                ; 6
           pop  edi
           add  di,29
           call DoVLine                 ; 3
           add  di,8000
           call DoVLine                 ; 5

	   pop	edi
	   pop	eax

           retn
Do5:
	   push eax
	   push	edi

           call DoVLine                 ; 1
           push edi
           add  di,8000
           call DoRVLine                ; 7
           pop  edi
           inc  edi
           inc  edi
           push edi
           sub  di,957
           call DoHLine                 ; 2
           add  di,8000
           call DoHLine                 ; 4
           add  di,8000
           call DoHLine                 ; 6
           pop  edi
           add  di,29
           call DoRVLine                ; 3
           add  di,8000
           call DoVLine                 ; 5

	   pop	edi
	   pop	eax

           retn
Do6:
	   push eax
	   push	edi

           call DoVLine                 ; 1
           push edi
           add  di,8000
           call DoVLine                 ; 7
           pop  edi
           inc  edi
           inc  edi
           push edi
           sub  di,957
           call DoHLine                 ; 2
           add  di,8000
           call DoHLine                 ; 4
           add  di,8000
           call DoHLine                 ; 6
           pop  edi
           add  di,29
           call DoRVLine                ; 3
           add  di,8000
           call DoVLine                 ; 5

	   pop	edi
	   pop	eax

           retn
Do7:
	   push eax
	   push	edi

           call DoRVLine                ; 1
           push edi
           add  di,8000
           call DoRVLine                ; 7
           pop  edi
           inc  edi
           inc  edi
           push edi
           sub  di,957
           call DoHLine                 ; 2
           add  di,8000
           call DoRHLine                ; 4
           add  di,8000
           call DoRHLine                ; 6
           pop  edi
           add  di,29
           call DoVLine                 ; 3
           add  di,8000
           call DoVLine                 ; 5

	   pop	edi
	   pop	eax

           retn
Do8:
	   push eax
	   push	edi

           call DoVLine                 ; 1
           push edi
           add  di,8000
           call DoVLine                 ; 7
           pop  edi
           inc  edi
           inc  edi
           push edi
           sub  di,957
           call DoHLine                 ; 2
           add  di,8000
           call DoHLine                 ; 4
           add  di,8000
           call DoHLine                 ; 6
           pop  edi
           add  di,29
           call DoVLine                 ; 3
           add  di,8000
           call DoVLine                 ; 5

	   pop	edi
	   pop	eax

           retn
Do9:
	   push eax
	   push	edi

           call DoVLine                 ; 1
           push edi
           add  di,8000
           call DoRVLine                ; 7
           pop  edi
           inc  edi
           inc  edi
           push edi
           sub  di,957
           call DoHLine                 ; 2
           add  di,8000
           call DoHLine                 ; 4
           add  di,8000
           call DoHLine                 ; 6
           pop  edi
           add  di,29
           call DoVLine                 ; 3
           add  di,8000
           call DoVLine                 ; 5

	   pop	edi
	   pop	eax

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