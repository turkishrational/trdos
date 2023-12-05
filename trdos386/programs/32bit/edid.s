; ****************************************************************************
; edid.s - TRDOS 386 (TRDOS v2.0.3) Test Program - 'sysvideo' read edid
; ----------------------------------------------------------------------------
;
; 19/01/2021 (Erdogan Tan)
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
; READ VESA EDID (MONITOR INFO) 
;========================================================================

[BITS 32] ; We need 32-bit intructions for protected mode

[ORG 0] 
START_CODE:
		mov	esi, TrDOS_Welcome
		call	print_msg

		sys	_video, 0F00h, edid_buffer  ; read edid
	
		cmp	eax, 128
		jne	short edid_read_error

		mov	esi, edid_buffer
		call	convert_to_hex_tbl
	
		mov	byte [color], 0Fh ; white

		mov	esi, edid_header
		call	print_msg	
terminate:
		sys	_exit
here:
		jmp	short here

edid_read_error:
		mov	esi, edid_rerr_msg
		call	print_msg
		jmp	short terminate

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Subroutine - print text/message on display
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

print_msg:
		mov	dl, [color] 
		sys 	_msg, esi, 255
				; message with color 
				; (max. 255 chars)
		retn

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Subroutine - Convert to hexadecimal character table
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

convert_to_hex_tbl:
	; esi = binary table address

	mov	ecx, 8
_h_1:
	push	ecx
	mov	cl, 16
	mov	edi, edid_row_chars - 2
_h_2:
	;add	edi, 2
	inc	edi
	inc	edi
	lodsb
	xor	ebx, ebx
	mov	bl, al
	and	bl, 0Fh ; low 4 bits

	mov	ah, [_hexchars+ebx]
	shr	al, 4  ; high 4 bits
	mov	bl, al
	mov	al, [_hexchars+ebx]	
	stosw	
	loop	_h_2		

	push	esi
	mov	esi, edid_row
	call	print_msg
	pop	esi

	pop	ecx
	loop	_h_1

	retn

_hexchars: 
	db	"0123456789ABCDEF"
color:	db	07h

;=============================================================================
;        	initialized data
;=============================================================================		

;-----------------------------------------------------------------------------
;  MESSAGES
;-----------------------------------------------------------------------------

TrDOS_Welcome:
	db	0Dh, 0Ah
	db	"VESA EDID reading program for TRDOS 386 v2"
	db	0Dh, 0Ah
	db	"by Erdogan TAN (19/01/2021)"
	db	0Dh, 0Ah, 0Dh, 0Ah, 0

edid_header:
	db	"EDID :", 0Dh, 0Ah, 0Dh, 0Ah	
edid_row:
	db	20h
edid_row_chars:	
	db	"00h 00h 00h 00h 00h 00h 00h 00h "
	db	"00h 00h 00h 00h 00h 00h 00h 00h "
	db	0Dh, 0Ah, 0

edid_rerr_msg:
	db	"EDID read error !"
	db	0Dh, 0Ah
	db	"(EDID is not ready or Video Bios function error!)"
	db	0Dh, 0Ah, 0

;=============================================================================
;        	uninitialized data
;=============================================================================

bss_start:

ABSOLUTE bss_start

alignb 2

buffer:

edid_buffer:

	resb	128
		
bss_clear_end:

bss_end:	 	