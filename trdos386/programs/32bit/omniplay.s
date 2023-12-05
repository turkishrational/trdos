; ****************************************************************************
; omniplay.s (TRDOS 386, TRDOS v2.0 - sample binary file, 'omniplay.prg')
; ----------------------------------------------------------------------------
; OMNIPLAY.PRG ! TEST program !
;
; 09/04/2017
;
; [ Last Modification: 09/04/2017 ]
;
; Derived from source code of 'OMNISCENT.ASM' by Dirk Küppers
;          SNC_OMNI.COM	 (MSDOS) intro file, 1997
;
; Assembler: NASM 2.11

; (Original TASM -msdos- code has been modifed for TRDOS 386 system calls and
; other protected mode (TRDOS 386) interrupts.)
; ****************************************************************************

;		   ----====> Omniscent <====----
;
;   Omniscent was done by Pinker of SANCTION for the Mekka '97. The song
;   was written by Nyphton. It place XXX out of XXX.
;
;   Special thanks in alphabetical order :
;
;       Andreas Mautsch     (beta testing)
;       Axel Scheel Meyer   (MACM sources, nice IRC chat's)
;       Christian Cohnen    (for his help on perspective texture mapping and
;	   		designing world and script)
;       Daniel Weinand	    (song)
;       Funk                (for give me the idea trying a 4K Descent)
;       Stephanie Schepers  (moral and food support ;-) )
;
;       and all other SANCTION dudes for supporting this product !

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
_rsvd1	equ 37
_pri	equ 38
_rele	equ 39
_fff	equ 40
_fnf	equ 41

[BITS 32] ; 80386 Protected Mode (32 bit) intructions

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


;==============================================================================
;		   constants
;==============================================================================

GMPort		equ 0331h
CMD_NOTEON      equ 090h
CMD_NOTEOFF     equ 080h
CMD_CHANGEPARAM equ 0B0h
CMD_CHANGEPRG   equ 0C0h
MOD_ALLNOTESOFF equ 07Bh
SONGSPEED   	equ 29

;==============================================================================
;		   structures
;==============================================================================

struc channel
 .del:	resw 1
 .trk:	resb 1
 .ln:	resb 1
 .adr:	resw 1
endstruc

;==============================================================================
;		      %macros
;==============================================================================
;***********************************************
;* descript. : start timer interrupt 70/s      *
;* parameter : none			       *
;* sideeffect: all		 	       *
;* back      : none			       *
;***********************************************
%macro		startTimer 0
                ;mov     ax,03508h
                ;int     21h
                ;mov     [word ptr Old08Irqseg],es
                ;mov     [word ptr Old08Irqofs],bx
                ;mov     ax,02508h
                ;mov     dx,offset irqHandler08
                ;int     21h
		
		mov	bx, 3409 ; 1193180/3409 = 350 ticks per second (*!)
		call	setTimer

		; start timer event
		mov	ebx, 8400h ; Current Timer setup, Callback method 	
		mov	ecx, 1	 ; 1 tick 
		mov	edx, timer_callback ; timer callback service addr
		mov	eax, 33	; 'systimer'
		int	40h	; TRDOS 386 system call
		jc	short _err_exit

		mov	[timer_event_number], al 

%endmacro

;***********************************************
;* descript. : stop timer interrupt            *
;* parameter : none			       *
;* sideeffect: all		 	       *
;* back      : none			       *
;***********************************************
%macro		stopTimer 0
                ;push    ds
                ;xor     bx,bx                  ; set timer to default
                ;call    setTimer		; 18.2 ticks per second
                ;mov     ax,02508h              ; restore old IRQ-vector
                ;lds     dx,[Old08Irqptr]
                ;int     21h
                ;pop     ds

		xor	bx, bx	; set timer to default (18.2 Hz) value
		call	setTimer

		; Stop timer event
		movzx	ebx, byte [timer_event_number]
		;and	bl, bl
		;jz	short r_t_m ; 0 = error (no timer event)
 		; bh = 0 -> stop timer event
		mov	eax, 33	; 'systimer'
		int	40h	; TRDOS 386 system call
;r_t_m:
%endmacro

;***********************************************
;* descript. : start keyboard handler          *
;* parameter : none			       *
;* sideeffect: all		 	       *
;* back      : none			       *
;***********************************************
%macro		startKBDHandler 0
                ;mov     ax,03509h
                ;int     21h
                ;mov     [word ptr Old09Irqseg],es
                ;mov     [word ptr Old09Irqofs],bx
                ;mov     ax,02509h
                ;mov     dx,offset irqHandler09
                ;int     21h

		; TRDOS 386 already have a CRTL+BRK
		; preview feature to call 'sysexit'
		; /// nothing to do here ! ///
		; 10/12/2016 - Erdogan Tan
		nop
%endmacro

;***********************************************
;* descript. : stop keyboard handler           *
;* parameter : none			       *
;* sideeffect: all		               *
;* back      : none		               *
;***********************************************
%macro		stopKBDHandler 0
                ;push    ds
                ;mov     ax,02509h             ; restore old IRQ-vector
                ;lds     dx,[Old09Irqptr]      ;
                ;int     21h
                ;pop     ds

		; TRDOS 386 already have a CRTL+BRK
		; preview feature to call 'sysexit'
		; /// nothing to do here ! ///
		; 10/12/2016 - Erdogan Tan
		nop
%endmacro

;***********************************************
;* descript. : zero null-initialized data      *
;* parameter : none			       *
;* sideeffect: all		               *
;* back      : none		               *
;***********************************************
%macro		nullData 0
		;mov	di, nullstart
                mov	edi, nullstart
		;mov	cx, nullend-nullstart
                ;mov	ecx, nullend-nullstart ; ??
		xor	eax, eax
                ;rep	stosb
		mov	ecx, (bss_end-nullstart)+3 ; 06/02/2017
		shr	cx, 2
		rep	stosd
%endmacro

;***********************************************
;* descript. : reset the GM-Port and switch to *
;*             UART mode.		       *
;* parameter : none			       *
;* sideeffect: dx,al		               *
;* back      : none		               *
;***********************************************
%macro		resetGM 0
		mov	dx, GMPort
		mov	al, 0FFh
		;out	dx, al
	
		mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt
		; cl = 0
		dec	ah ; 0
		;sub	cl, cl
resGMbusy1:
		dec	cl
		jz	short rGMerror	;{ timeout }
		;in	al, dx		;{ read acknowledge }
		
		;mov	ah, 0 ; in (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt
		
		test	al, 80h
		jnz	short resGMbusy1
		dec	dx
		;in	al, dx

		;mov	ah, 0 ; in (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt

		cmp	al, 0FEh
		jne	short rGMerror
		inc	dx		;{ switch into UART mode }
resGMbusy2:
		;in	al, dx

		;mov	ah, 0 ; in (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt
		
		test	al, 40h
		jnz	short resGMbusy2
		mov	al, 3Fh
		;out	dx, al

		;mov	ah, 1 ; out (byte)
		inc	ah ; 1
		int	34h ; TRDOS 386 - IOCTL interrupt
rGMerror:
%endmacro

;***********************************************
;* descript. : mute midi channels 0..15        *
;* parameter : none                	       *
;* sideeffect: ax,cx		               *
;* back		 : none                	       *
;***********************************************
%macro		silence 0
		mov	cx, 15 ; 0Fh
Siloop:	
		mov	ax, cx
		add	al, CMD_CHANGEPARAM ; 0B0h
		call	writeGM
		mov	al, MOD_ALLNOTESOFF ; 7Bh
		call	writeGM
		xor	al, al
		call	writeGM
		loop	Siloop
Sinosound:
%endmacro

;***********************************************
;* descript. : expand a song       	       *
;* parameter : si:song base adress	       *
;* sideeffect: ax,bx,cx,dx,si,di               *
;* back      : none                	       *
;***********************************************
%macro		expandSong 0
                mov	esi, credits
                mov	ebx, channels
                mov	edi, songdata
EPSwhile:	lodsw
                dec	al
                js      short EPSendwhile
                mov	[ebx+channel.trk], al ; [ebx+2]
                add	al, CMD_CHANGEPRG ; 0C0h
                call	setinstr
                mov	[ebx+channel.adr], di ; [ebx+4]	
               	call	expand
                xor	al, al
                stosb
                add	bx, 6
                inc	word [tracks]
                jmp	short EPSwhile
EPSendwhile:
%endmacro

;***********************************************
;* descript. : play a tick of the song	       *
;* parameter : none                	       *
;* sideeffect: ax,bx,cx,dx,si		       *
;* back      : none                	       *
;***********************************************
%macro		playsong 0
		;mov	cx, [tracks]
		movzx	ecx, word [tracks] ; ??
		;mov	si, channels
		mov	esi, channels
PSmloop:
		dec	word [esi+channel.del]
		jg	short PSdelayed
		mov	ax, [esi+channel.trk]
		add	al, CMD_NOTEOFF	; 80h
		call	setnote
		;mov	bx, [esi+channel.adr]
		movzx	ebx, word [esi+channel.adr] ; ??
		mov	ah, 127 	; 7Fh
		cmp	byte [ebx], 0
		jz	short PStrackend
		mov	ah, [ebx]
		mov	[esi+channel.ln], ah
		mov	al, [esi+channel.trk]
		add	al, CMD_NOTEON	; 90h
		call	setnote
		mov	al, [ebx+1]
		mov	bl, SONGSPEED	; 1Dh
		mul	bl
PStrackend:
		mov	[esi+channel.del], ax
		add	word [esi+channel.adr], 2
PSdelayed:
		add	si, 6
		loop	PSmloop
PSnosound:
%endmacro

;=============================================================================
;               entry point
;=============================================================================
;***********************************************
;* descript. : entry point		       *
;* parameter : none		               *
;* sideeffect: all		               *
;* back      : none		               *
;***********************************************

[ORG 0] 

start:
		; clear bss (18/12/2016)
		;mov	edi, bss_start ; ??		
		mov	di, bss_start
		mov	cx, (bss_end - bss_start)/4
		;mov	ecx, (bss_end - bss_start)/4
		xor	eax, eax
		rep	stosd

		sys	_msg, prg_msg, 255, 0Fh

		resetGM
		expandSong

		startTimer
		startKBDHandler
mainloop:
		nop
		nop
		nop
		mov	ah, 1	; Check keyboard buffer
		int	32h	; TRDOS 386 Keyboard interrupt
		;jnz	_exit_  ; exit
		;jmp	mainloop
		jz	mainloop ; 11/02/2017
_err_exit:
_exit_:
		silence
		stopKBDHandler
		stopTimer
terminate:
		sys 	_exit   ; INT 40h
here:
		jmp	short here

;==============================================================================
;		interrupt handler
;==============================================================================
;***********************************************
;* descript. : timer routine                   *
;* parameter : none		               *
;* sideeffect: none		               *
;* back      : none		               *
;***********************************************

timer_callback:
		playsong

		mov	eax, 39	; 'sysrele'
		int	40h	; TRDOS 386 system call
	
;==============================================================================
;		sub routines
;==============================================================================

expand:
EPwhile: 	mov	cl, byte [esi]
		or	cl, cl
		jz	short EPendwhile
		jns	short EPnote
EPcall:
		push	ecx
		push	esi
		inc	esi
		call	expand
		pop	esi
		pop	ecx
		inc	cl
		jnz	short EPcall
		mov	esi, eax
		jmp	short EPwhile 
EPnote:
		movsw
		jmp	short EPwhile
EPendwhile:
		inc	esi
		mov	eax, esi
		retn

;***********************************************
;* descript. : set timer speed to 1193180/AX   *
;*             interrupts per second           *
;* parameter : bx		               *
;* sideeffect: ax		               *
;* back      : none		               *
;***********************************************

setTimer:	; set timer speed to 1193180/BX (TRDOS 386)
		mov	al, 36h
		;out	43h, al		; Timer	8253-5 (AT: 8254.2).

		mov	ah, 1 ; out (byte)
		mov	dx, 43h
		int	34h ; TRDOS 386 - IOCTL interrupt

		mov	al, bl
		;out	40h, al		; Timer	8253-5 (AT: 8254.2).
		
		mov	dx, 40h
		;mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt

		mov	al, bh
		;out	40h, al		; Timer	8253-5 (AT: 8254.2).
		
		;mov	ah, 1 ; out (byte)
		;mov	dx, 40h
		int	34h ; TRDOS 386 - IOCTL interrupt

		retn

;***********************************************
;* descript. : send a byte to the GM-Port      *
;* parameter : al:midi command                 *
;* sideeffect: dx		               *
;* back      : none		               *
;***********************************************

writeGM:
		mov	dx, GMPort  ; 331h
		push	cx
		push	ax
		xor	cl, cl
busy:
		dec	cl
		jz	short timeOut
		;in	al, dx

		mov	ah, 0	; in (byte)
  		int	34h	; TRDOS 386 - IOCTL interrupt
		
		test	al, 40h
		jnz	short busy

timeOut:				; CODE XREF: writeGM+8j
		pop	ax
		pop	cx
		dec	dx
		;out	dx, al
		
		mov	ah, 1	; out (byte)
  		int	34h	; TRDOS 386 - IOCTL interrupt

		retn

;***********************************************
;* descript. : send NOTEON command, volume 127 *
;* parameter : al:channel;ah:note              *
;* sideeffect: dx,al		               *
;* back      : none		               *
;***********************************************

setnote:
		call	setinstr
		mov	al, 127	; 7Fh
		call	writeGM
		retn

;***********************************************
;* descript. : send CHANGEPRG command	       *
;* parameter : al:channel;ah:instrument        *
;* sideeffect: dx,al		               *
;* back      : none		               *
;***********************************************

setinstr:
		call	writeGM
		mov	al, ah
		call	writeGM
		retn

prg_msg:
		db	'ERDOGAN TAN - TRDOS 386 Test - OMNIPLAY.PRG'
		db	0Dh, 0Ah
		db	'09/04/2017'
		db	0Dh, 0Ah
		db	0 

align 4

;=============================================================================
;               preinitialized data
;=============================================================================

credits:
		db	 2, 80,-5
		db      -2,50,4,69,2,62,2,69,2,81,2,62,2,69,2,79,2,62,2,69,2
		db	81,2,62,2,69,2,79,2,81,2,0	; { pat 0,2,4,6,8 }
		db      -2,50,4,70,2,62,2,70,2,81,2,62,2,70,2,79,2,62,2,70,2
		db	82,2,62,2,70,2,79,2,82,2,0	; { pat 1,3,5,7,9 }
		db       0
		db      55,32,56,32			; { pat 10 }
		db      51,32,53,24,55,8                ; { pat 11 }
		db      57,32,50,32		 	; { pat 12 }
		db      60,48,58,8,57,8                 ; { pat 13 }
		db      55,64		   		; { pat 14 }
		db      -2,67,16,62,16,69,16,70,16,0	; { pat 15 }
		db      -2,70,16,69,16,62,16,65,16,0    ; { pat 16 }
		db       0
		db       4, 50,1,128,1,128              ;{ pat 0-3 }
		db      65,28,64,2,65,2,64,16,62,8,60,8 ;{ pat 4 }
		db      67,28,65,4,60,16,65,8,64,8      ;{ pat 5 }
		db      62,32,50,16,64,8,65,8           ;{ pat 6 }
		db      67,32,60,16,62,8,64,8           ;{ pat 7 }
		db      62,128		  		;{ pat 8,9 }
		db      67,128		  		;{ pat 10,11 }
		db     	67,16,64,16,66,16,64,8,66,8	;{ pat 12 }
		db     	72,48,70,16                 	;{ pat 13 }
		db     	67,32,55,32                 	;{ pat 14 }
		db     	-2,31,32,55,32,0                ;{ pat 15 }
		db      -2,26,32,50,32,0                ;{ pat 16 }
		db       0
		db       5, 89,-5,1,128,0		;{ pat 0-9 }
		db      67,8,74,24,68,8,75,24           ;{ pat 10 }
		db      63,8,70,24,60,8,69,16,67,8      ;{ pat 11 }
		db      62,8,69,16,67,8,62,8,66,24	;{ pat 12 }
		db      63,8,67,24,60,8,67,16,69,8	;{ pat 13 }
		db      55,8,67,24,43,8,50,24           ;{ pat 14 }
		db      -2,31,1,38,63,0                	;{ pat 15 }
		db      -2,26,1,33,63,0                	;{ pat 16 }
		db       0
		db	10,  0,-16,42,2,42,2,42,2,42,1,42,1,0 ;{ pat 0,1 }
		db     -16,36,2,42,2,42,2,42,1,42,1,0   ;{ pat 2,3 }
		db     -30,36,2,42,2,46,2,36,1,42,1,38,2
		db	42,2,46,2,38,1,42,1     	;{ pat 4-16 }
		db      36,2,38,2,36,2,42,1,42,1,38,2,42,2,46,2,38,1,42,1,0
		db     	 0
		db	 0

bss_start:

ABSOLUTE bss_start

alignb 4

;=============================================================================
;        	null-initialized data
;=============================================================================

nullstart:
tracks:		resw	1
channels:       resb	16*6
nullend:

;=============================================================================
;       	uninitialized data
;=============================================================================

timer_event_number: resb 1
songdata:	resb	3605

bss_end: