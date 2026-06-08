; ****************************************************************************
; beeptest.s (TRDOS 386, TRDOS v2.0.11 - sample binary file)
; ----------------------------------------------------------------------------
; BEEPTEST.PRG ! 'INT 32h, ah = 13h' TEST program for TRDOS 386 !
;
; 08/06/2026
;
; [ Last Modification: 08/06/2026 ]
;
; ****************************************************************************
; nasm beeptest.s -l beeptest.txt -o BEEPTEST.PRG

; 21/05/2026
; 30/04/2026
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
_stdio	equ 46
_fstat	equ 47  

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

;-----------------------------------------------------------------
; code
;-----------------------------------------------------------------

[BITS 32] ; 32-bit intructions

[ORG 0]

START_CODE:
	mov	esi, program
	call	print_msg

check_kb_buffer:
	;mov	ah, 11h
	mov	ah, 1
	int	32h
	jz	short bf_beep ; buffer empty

	mov	ah, 0
	;mov	ah, 10h	; getchar (enchanced)
	int	32h
	jmp	short check_kb_buffer

bf_beep:
	mov	esi, bf_beep_test
	call	print_msg

get_kb_buffer_status:
	mov	al, 1 ; keyboard buffer full beep
	mov	ah, 13h
	int	32h   ; get keyboard buffer status

	cmp	eax, -1
	jne	short get_kb_buffer_status

	;mov	ah, 10h
	mov	ah, 0
	int	32h

	cmp	al, 0Dh	; ENTER key
	je	short skip_kb_buffer_status

	mov	dx, 60h
	mov	ah, 0
	int	34h   ; in al, 60h
	cmp	al, 01h ; scancode
	je	short nl_exit ; ESC key 
	cmp	al, 1Ch ; scancode ; ENTER key 
	jne	short get_kb_buffer_status
	
skip_kb_buffer_status:
	mov	esi, key_press_beep_test
	call	print_msg

empty_kb_buffer:
	;mov	ah, 10h
	mov	ah, 0
	int	32h

	;mov	ah, 11h
	mov	ah, 1
	int	32h
	jnz	short empty_kb_buffer	

key_press_beep:
	mov	ah, 13h
	mov	al, 2 ; key press beep
	int	32h   ; get keyboard buffer status

	and	eax, eax
	jz	short key_press_beep

	;mov	ah, 10h
	mov	ah, 0
	int	32h
	
	cmp	al, 1Bh	; ESCape key
	jne	short key_press_beep

nl_exit:
	mov	esi, nextline
	call	print_msg

terminate:
	sys	_exit, 0

hang:
	jmp	short hang

print_msg:
	mov	ebx, 0Fh       ; white characters (bl)
		               ; video page 0 (bh)
print_msg_@:
	mov	ah, 0Eh ; teletype output (write tty)
	lodsb
_p_nextchar:
	int	31h
	lodsb
	and	al, al
	jnz	short _p_nextchar
	retn

;-----------------------------------------------------------------
; data
;-----------------------------------------------------------------

program:
	db "TRDOS 386 v2.0.11 - INT 32h (AH = 13h) test program",0Dh,0Ah
	db "by Erdogan Tan [June 2026]"
nextline:
	db 0Dh,0Ah,0

bf_beep_test:
	db 0Dh,0Ah
	db "Press any keys (16 times/keys at least)"  
	db 0Dh,0Ah
	db "      for Keyboard Buffer Full - BEEP test"
	db 0Dh,0Ah
	db "(Press ENTER to skip)",0Dh,0Ah,0

key_press_beep_test:
	db 0Dh,0Ah
	db "Press a key to test key-press BEEP"  
	db 0Dh,0Ah
	db "(Press ESC to exit)",0Dh,0Ah,0
