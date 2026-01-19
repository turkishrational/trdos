; ****************************************************************************
; setmode4.s - TRDOS 386 (TRDOS v2.0.3) Test Program - Set Video Mode
; ----------------------------------------------------------------------------
;
; 05/12/2020 (Erdogan Tan)
;
; ****************************************************************************

; 14/07/2020
; 31/12/2017
; TRDOS 386 (v2.0) system calls
_ver 	equ 0
_exit 	equ 1
_fork 	equ 2
_read 	equ 3
_write	equ 4
_open	equ 5
_close 	equ 6
_wait 	equ 7
_create	equ 8
_rename	equ 9
_delete	equ 10
_exec	equ 11
_chdir	equ 12
_time 	equ 13
_mkdir 	equ 14
_chmod	equ 15
_rmdir	equ 16
_break	equ 17
_drive	equ 18
_seek	equ 19
_tell 	equ 20
_memory	equ 21
_prompt	equ 22
_path	equ 23
_env	equ 24
_stime	equ 25
_quit	equ 26	
_intr	equ 27
_dir	equ 28
_emt 	equ 29
_ldrvt 	equ 30
_video 	equ 31
_audio	equ 32
_timer	equ 33
_sleep	equ 34
_msg    equ 35
_geterr	equ 36
_fpstat	equ 37
_pri	equ 38
_rele	equ 39
_fff	equ 40
_fnf	equ 41
_alloc	equ 42
_dalloc equ 43
_calbac equ 44
_dma	equ 45	

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

;========================================================================
; SET VIDEO MODE
;========================================================================

; Modified from setmod3.s (19/11/2020)

[BITS 32] ; We need 32-bit intructions for protected mode

[ORG 0] 
START_CODE:
		mov	esi, message1
		call	print_msg

		xor	ah, ah
		int	32h	; real mode: int 16h

		mov	ax, 13h	; mode 13h ; 
		int	31h	; real mode: int 10h

		;; DIRECT ACCESS TO VIDEO MEMORY
		;;xor	ebx, ebx
		;mov	bh, 4 ; Direct access/map to CGA memory (0B8000h)
		;;mov	eax, _video ; 1Fh
		;mov	ax, 1Fh ; sys _video ; TRDOS 386 Video functions
		;int	40h   ; TRDOS 386 system call

		; eax = 0B8000h
		;and	eax, eax
		;jz	terminate ; error (eax = 0)

		mov	esi, message2
		;call	print_msg_x
		call	print_msg	
key_loop:
		xor	ah, ah
		int	32h	; real mode: int 16h

		cmp	al, 13 ; ENTER key
		je	short crlf
			
		cmp	al, 27 ; ESCape key
		je	short terminate

		cmp	al, 3 ; CTRL+C
		je	short terminate

		cmp	ax, 0 ; CTRL+BREAK
		je	short terminate
_next_char:
		; 17/11/2020
		;call	print_char

print_char:
		mov	ah, 0Eh
		mov	bx, 07h
		int	31h	; real mode: int 10h
		jmp	short key_loop 
crlf:
		mov	ah, 0Eh	
		;mov	al, 0Dh ; Carriage return
		mov	bx, 07h
		int	31h 	; real mode: int 10h
		mov	al, 0Ah ; Line feed

		jmp	short print_char

		; 17/11/2020
;		jmp	short key_loop
;crlf:
;		call	print_char
;		mov	al, 10
;		jmp	short _next_char 	

terminate:
		mov	ax, 3	; mode 3 ; 
		int	31h	; real mode: int 10h	

		sys	_exit
here:
		jmp	short here

_return:
		retn

		; 19/11/2020
print_msg:
		sys 	_msg, esi, 255, 0Eh ; 05/12/2020
				; message with yellow color 
				; (max. 255 chars)
		retn

;		mov	ah, 0Eh
;		mov	bx, 0Fh
;p_loop:
;		lodsb
;		or	al, al
;		jz	short _return
;		int	31h		
;		jmp	short p_loop

;		; 17/11/2020
;print_msg_x:
;		mov	edi, eax  ; 0B8000h
;		xor	eax, eax
;		xor	edx, edx
;		mov	bh, 0Fh	; color (attribute)
;_0:	
;		lodsb
;		or	al, al
;		jz	short _ok
;		
;		call	print_char
;_ok:
;		retn
;
;print_char:
;		cmp	al, 13	; Carriage return
;		je	short cr
;		cmp	al, 10	; Line feed
;		je	short lf
;
;		cmp	al, 8 ; Backspace
;		jne	short _alfanumeric
;
;		mov	bl, al
;		mov	al, 80	; columns * 2
;		mul	dh	; * row
;		shl	dl, 1
;		add	al, dl	; column * 2
;		adc	ah, 0	; +
;		shr	dl, 1
;
;		mov	word [edi+eax], 0720h ; clear current char
;
;		dec	dl
;		jns	short _retn
;		mov	dl, 39 ; column 39
;		dec	dh
;		jns	short _retn
;		xor	edx, edx
;		retn
;_alfanumeric:
;		mov	bl, al
;		mov	al, 80	; columns * 2
;		mul	dh	; * row
;		shl	dl, 1
;		add	al, dl	; column * 2
;		adc	ah, 0	; +
;		shr	dl, 1
;		
;		mov	bh, 0Fh		
;		mov	[edi+eax], bx ; char and color/attrib
;
;		inc	dl  ; next column
;		cmp	dl, 40
;		jb	short _retn
;
;		xor	dl, dl ; column 0
;		inc	dh  ; next row	
;		cmp	dh, 25
;		jb	short _retn
;		xor	dh, dh ; row 0
;_retn:
;		retn
;cr:
;		xor	dl, dl
;		jmp	short _retn
;lf:
;		inc	dh
;		cmp	dh, 25		
;		jb	short _retn
;		xor	dh, dh
;		jmp	short _retn		

;-----------------------------------------------------------------
;  MESSAGES
;-----------------------------------------------------------------

message1:
	db	0Dh, 0Ah
	db	"Press a key to set video mode 13h"
	db	0Dh, 0Ah, 0
message2:
	db	0Dh, 0Dh
	db	"Video Mode 13h (320x200 graphics)"
	db	0Dh, 0Ah, 0Dh, 0Ah
	db	"Press ESC to exit"
	db	0Dh, 0Ah, 0Dh, 0Ah, 0