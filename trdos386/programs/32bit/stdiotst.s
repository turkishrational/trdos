; ****************************************************************************
; stdiotst.s - TRDOS 386 (TRDOS v2.0) Kernel - SYSSTDIO function test
; ----------------------------------------------------------------------------
;
; 23/08/2024
;
; [ Last Modification: 19/09/2024 ]
;
; ****************************************************************************

; 20/08/2024 ; TRDOS 386 v2.0.9 (exit code)
; 20/08/2017
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
_dma	equ 45
_stdio  equ 46	;  TRDOS 386 v2.0.9

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
; EXEC TEST - CODE
;========================================================================

[BITS 32] ; 32-bit intructions

[ORG 0] 

START_CODE:
	sys	_msg, program, len0, 0Bh
	sys	_msg, owner, 255, 0Eh

	; If TRDOS 386 kernel version < 2.0.9 ..
	; (because sysstdio sysstem call was not existing before v2.0.9)
	; Program will be terminated with Invalid Function Call error msg

	; sysstdio Reset STDIN & STDOUT redirections
	; NOTE: In fact, there is not a redirection at start stage of
	; a program. (Redirections are done in a program.)
	; ((I am putting this here for IFC test.))
	sys	_stdio, 4, 0  ; Reset STDIN redirection
	sys	_stdio, 5, 0  ; Reset STDOUT redirection

restart:
	sys	_msg, press_F_key, 255, 0Fh
	sys	_msg, functions1, 255, 07h
	sys	_msg, functions2, 255, 07h
	sys	_msg, functions3, 255, 07h

getchar_@:
	; getchar
	;mov	ah, 10h
	;int	32h

	; stdio getchar (TRDOS 386 Kernel v2.0.9 and later)
	;mov	bl, 6	; read character (ascii and scancode) on stdin
			; -no redirection, wait-
	sys	_stdio, 6

	cmp	al, 1Bh ; ESC key
	je	short terminate

	cmp	ah, 3Bh ; F1 key
	jb	short getchar_@
	cmp	ah, 44h ; F10 key
	ja	short getchar_@

	mov	bl, ah
	sub	bl, 3Bh
	shl	ebx, 2
	add	ebx, function_table
	call	dword [ebx]
	sys	_msg, crlf, 255, 07h
	sys	_msg, continue_msg, 255, 07h
	sys	_stdio, 6
	cmp	al, 1Bh ; ESC key
	je	short _ok
	jmp	restart

getchar_@@:

_ok:
	sys	_msg, msg_ok, 255, 07h
terminate:
	sys	_msg, crlf, 2, 07h

	sys	_exit, 0  ; ebx = exit code
	;sys	_exit
here:
	nop
	jmp	short here

function_table:
	dd	F1_function
	dd	F2_function
	dd	F3_function
	dd	F4_function
	dd	F5_function
	dd	F6_function
	dd	F7_function
	dd	F8_function
	dd	F9_function
	dd	F10_function

F1_function:
	sys	_stdio, 4, 0 ; reset STDIN
	; ecx = 0
	sys	_stdio, 5    ; reset STDOUT

	sys	_msg, F1_header, 255, 0Ch
	sys	_msg, F1_text, 255, 07h

	sub	ecx, ecx
	mov	ch, 0Fh ; color = white
F1_nextchar:
	sys	_stdio, 6  ; read character (from STDIN)
	mov	cl, al
	sys	_stdio, 8  ; write character (to STDOUT)
	cmp	cl, 0Dh  ; ENTER/CR key
	jne	short F1_nextchar
	retn

F2_function:
	sys	_stdio, 4, 0 ; reset STDIN
	; ecx = 0
	sys	_stdio, 5    ; reset STDOUT

	sys	_msg, F2_header, 255, 0Ch
	sys	_msg, F2_text, 255, 07h

	sub	ecx, ecx
	mov	ch, 0Fh ; color = white
F2_nextchar:	
	sys	_stdio, 7  ; read character (from STDIN)
	and	eax, eax
	jz	short F2_nextchar ; not a character input
	mov	cl, al
	sys	_stdio, 8  ; write character (to STDOUT)
	cmp	cl, 0Dh  ; ENTER/CR key
	jne	short F2_nextchar
	retn

F3_function:
	sys	_msg, F3_header, 255, 0Ch
	sys	_msg, F3_text, 255, 0Fh
F6_continue:
	sys	_msg, stdio_at_first, 255, 07h

	; create STDIO.TXT or truncate it if it is existing
	sys	_creat, stdio_file, 0 ; create normal file
	jnc	short F3_continue_1 ; eax = file handle
F3_error:
	sys	_msg, create_error, 255, 0Dh
	retn

F3_continue_1:
	; 18/09/2024 (TRDOS 386 v2.0.9)
	;sys	_close, eax 
	;	; needed for now (TRDOS 386 v2.0.9 defect/bug)
	;sys	_open, stdio_file, 1 ; open for write
	;jc	short F3_error
	
	inc	eax ; file descriptor + 1 (for SYSSTDIO)
	mov	[filehandle], eax
	sys	_intr, 0 ; CRTL+BRK disabled
	sys	_msg, redir_option_hdr, 255, 0Eh
	sys	_msg, redir_option, 255, 0Fh

	; write SAMPLE text
	; redirect STDOUT to the (open) file
	sys	_stdio, 5, [filehandle]

F3_continue_2:
	sys	_stdio, 6 ; read character (from STDIN)
	cmp	al, 0Dh ; ENTER
	je	short F3_continue_4
	cmp	al, 1Bh ; ESC
	jne	short F3_continue_2

	sys	_msg, redir_text_hdr, 255, 0Fh
	
	mov	esi, redir_text
	mov	edi, redir_text_size
F3_continue_3:
	lodsb	; load a character from the sample text
	; write to redirected STDOUT (STDIO.TXT)
	sys	_stdio, 2, eax
	; 18/09/2024
	; Also write to STDERR 
	;sys	_stdio, 3 ; ecx = character to be written
	mov	ch, 0Bh ; color (cyan)
	sys	_stdio, 8 ; write char and color to STDERR
	dec	edi
	jnz	short F3_continue_3
	jmp	short F3_continue_9

F3_continue_4:
	; read from STDIN
	sys	_msg, F3_enter_msg, 255, 07h
F3_continue_5:
	sys	_stdio, 6 ; read char (no redir, wait)
	cmp	al, 20h
	jb	short F3_continue_6
	; write to redirected STDOUT (STDIO.TXT)
	sys	_stdio, 2, eax
	;mov	eax, ecx
	; echo to STDERR (colored)
	mov	ah, 0Fh ; white
	sys	_stdio, 8, eax	 
F3_continue_6:
	cmp	al, 0Dh
	je	short F3_continue_8
F3_continue_7:
	cmp	al, 1Bh
	jne	short F3_continue_5
	mov	al, 0Dh
F3_continue_8:
	; AL = CR
	sys	_stdio, 2, eax
	mov	cl, 0Ah ; LF
	sys	_stdio, 2
	;jmp	short F3_continue_9

F3_continue_9:
	; close STDIO.TXT file
	mov	ebx, [filehandle] ; file descriptor + 1
	dec	ebx ; file descriptor (0 based)
	sys	_close

	sys	_intr, 1 ; CRTL+BRK enabled again

	; cancel STDOUT redirection
	sys	_stdio, 5, 0 ; reset stdout
	
	; written message (success/completed msg)
	sys	_msg, msg_written, 255, 07h
	retn

F5_function:
	sys	_msg, F5_header, 255, 0Ch
	sys	_msg, F5_text, 255, 0Fh
	
	; CL = 0E7h -> except volume names and directories
	; CH = 0 -> 24 bytes buffer
	sys	_fff, stdio_file, 0E7h, fff_buffer
	jc	short F5_nf_error

	cmp	dword [edx+6], 0 ; file size
	jna	short F5_zf_error

	sys	_open, stdio_file, 0 ; open for read
	jnc	short F5_continue_1
	; access denied error !?
F5_error:
	sys	_msg, open_error, 255, 0Dh
	retn

F5_nf_error:
	sys	_msg, not_found, 255, 0Fh
	retn
F5_zf_error:
	sys	_msg, zero_file, 255, 0Fh
	retn

F5_continue_1:
	inc	eax ; file descriptor + 1 (for SYSSTDIO)
	mov	[filehandle], eax
	
	sys	_intr, 0 ; CRTL+BRK disabled
	; redirect STDIN to the (open) file
	sys	_stdio, 4, [filehandle]
F5_continue_2:
	sys	_stdio, 0 ; read character (from STDIN)
	jc	short F5_continue_3 ; error or EOF

	; al  = character
	mov	ah, 0Bh ; character color (cyan)
	; write character/byte to STDOUT (non redirected)
	sys	_stdio, 8, eax

	jmp	short F5_continue_2

F5_continue_3:
	; close STDIO.TXT file
	mov	ebx, [filehandle]
	dec	ebx ; file descriptor (0 based)
	sys	_close

	sys	_intr, 1 ; CRTL+BRK enabled again

	; cancel STDIN redirection
	sys	_stdio, 4, 0 ; reset stdin
F4_ok:
	sys	_msg, msg_ok, 255, 07h
	retn

F4_function:
	sys	_msg, F4_header, 255, 0Ch

	mov	esi, F4_text_1
F4_nextchar_1:
	lodsb
	or	al, al
	jz	short F4_function_@	
	sys	_stdio, 3, eax  ; write character (to STDERR)
	jmp	short F4_nextchar_1

F4_function_@:
	cmp	esi, F4_text_1_end
	jne	short F4_function_@@
	mov	esi, F4_text_2
	jmp	short F4_nextchar_1

F4_function_@@:
	sys	_msg, F4_string, F4_string_len, 0Fh
F4_nextchar_2:	
	sys	_stdio, 6 ; read character (from STDIN) -wait-
	cmp	al, 1Bh  ; ESC
	je	short F4_ok
	mov	cl, al
	sys	_stdio, 3  ; write character (to STDERR)
	cmp	cl, 0Dh  ; ENTER/CR key
	jne	short F4_nextchar_2
	jmp	F4_ok

F6_function:
	; same with F3 (except the caption)
	sys	_msg, F6_header, 255, 0Ch
	sys	_msg, F6_text, 255, 0Fh
	jmp	F6_continue

F9_function:
	sys	_msg, F9_header, 255, 0Ch

	; select color message
	sys	_msg, F9_text, 255, 07h

	mov	esi, colortable
F9_function_@:
	sys	_stdio, 6 ; read char (from STDIN) -wait-
	; ah = scan code
	
	cmp	al, 1Bh  ; ESC
	je	short F9_ok
	cmp	al, 0Dh	 ; ENTER/CR key
	je	short F9_function_@@

	cmp	al, '0'
	jb	short F9_function_@
 	cmp	al, '9'
	ja	short F9_function_@

	xor	ah, ah	; clear scan code
	sub	al, '0'
	;jz	short F9_string
	add	esi, eax

F9_function_@@:
	sys	_msg, F4_string, F4_string_len, 0Fh

F9_nextchar:
	sys	_stdio, 7 ; read char (from STDIN) -no wait-
	; ah = scan code

	and	eax, eax
	jz	short F9_nextchar ; not a character input

	cmp	al, 1Bh  ; ESC
	je	short F9_ok

	mov	cl, al
	mov	ch, [esi] ; colortable + index
	sys	_stdio, 8 ; write char & color (to STDOUT)
	cmp	cl, 0Dh  ; ENTER/CR key
	jne	short F9_nextchar
F9_ok:
	jmp	F4_ok ; write 'OK' & return back to the menu 
	
F10_function:
	sys	_msg, F10_header, 255, 0Ch
	sys	_msg, F10_text_1, 255, 0Fh
	sys	_msg, F10_text_2, 255, 07h

	xor	esi, esi
	xor	edx, edx
F10_nextchar:
	mov	ch, 0Fh ; white 
F10_nextchar_@:
	sys	_stdio, 0 ; read char (from STDIN) -wait-

	cmp	al, 1Bh  ; ESC
	je	short F10_ok

	or	esi, esi
	jnz	short F10_nextchar_@@

	mov	esi, eax  ; the 1st char
F10_nextchar_@@:
	mov	cl, al
	sys	_stdio, 8 ; write char & color (to STDOUT)
	cmp	cl, 0Dh  ; ENTER/CR key
	je	short F10_ok
	inc	edx
	cmp	edx, 4
	jne	short F10_nextchar
	mov	ecx, esi
	sys	_stdio, 9 ; ungetchar (the 1st char) 
	mov	ch, 0Bh ; cyan
	jmp	short F10_nextchar_@

F10_ok:
	jmp	F4_ok

F8_function:
	sys	_msg, F8_header, 255, 0Ch
	sys	_msg, F8_text, 255, 07h
	mov	edi, 7 ; read char (no wait)
	jmp	short F8_getchar

F7_function:
	sys	_msg, F7_header, 255, 0Ch
	sys	_msg, F7_text, 255, 07h
	mov	edi, 6 ; read char (wait)
F7_getchar:
F8_getchar:
	sys	_stdio, edi ; read character (from STDIN)

	cmp	edi, 6
	je	short F7_getchar_@ ; (wait)

	and	eax, eax
	jz	short F8_getchar ; (no wait)
F7_getchar_@:
	call	chk_ctrl_char

	push	eax
	sys	_msg, chartext, 255, 0Fh
	sys	_msg, char, 255, 0Bh
	pop	eax
	
	mov	ebx, ascii
	call	write_hex_number  ; ascii code
	push	eax ; +
	push	eax
	sys	_msg, asciitext, 255, 0Fh
	sys	_msg, ascii, 255, 0Bh
	pop	eax
	xchg	ah, al
	mov	ebx, scanc
	call	write_hex_number  ; scan code
	sys	_msg, scanctext, 255, 0Fh
	sys	_msg, scanc, 255, 0Bh
	pop	eax ; +
F7_nextchar:
F8_nextchar:
	cmp	al, 1Bh ; ESC
	je	short F8_ok
	jmp	F8_getchar
F8_ok:
 	; write 'OK' & return back to the menu
	jmp	F4_ok

F7_write_char:
	mov	ah, 0Bh ; cyan
	sys	_stdio, 8, eax ; write char (to STDOUT)
	retn

chk_ctrl_char:
	mov	ecx, 20202020h ; 4 space chars
	xor	esi, esi
	cmp	al, 20h
	jb 	short chk_ctrl_char_@
	cmp	al, 127 ; DEL
	je	short chk_ctrl_char_@@@
	mov	cl, al
ctrl_char_retn:
	mov	[char], ecx
	retn
chk_ctrl_char_@:
	inc	esi
	cmp	al, 1Bh  ; ESC
	je	short chk_ctrl_char_@@
	inc	esi
	cmp	al, 0Dh  ; CR 
	je	short chk_ctrl_char_@@
	inc	esi
	cmp	al, 0Ah  ; LF 
	je	short chk_ctrl_char_@@
	inc	esi
	cmp	al, 09h  ; TAB
	je	short chk_ctrl_char_@@
	inc	esi
	cmp	al, 08h  ; BS
	je	short chk_ctrl_char_@@
	inc	esi
	cmp	al, 07h  ; BEEP
	jne	short ctrl_char_retn
chk_ctrl_char_@@:
	shl	esi, 2 ; *4
chk_ctrl_char_@@@:
	add	esi, ctrltable
	mov	ecx, [esi]
	jmp	short ctrl_char_retn

write_hex_number:
	; ebx = hex number (text) address
	push	eax
	xor	ah, ah
	mov	cl, 16
	div	cl
	mov	dl, al  ; edx = al
	mov	al, [edx+hextable]
	mov	dl, ah  ; edx = ah
	mov	ah, [edx+hextable]
	mov	[ebx], ax
	pop	eax
	retn

;-----------------------------------------------------------------

ctrltable: db "DEL ESC CR  LF  TAB BS  BEEP"

hextable: db "0123456789ABCDEF"

; CGA
colortable: db 07h,09h,0Ah,0Bh,0Ch,0Dh,0Eh,0Fh,06h,07h

;-----------------------------------------------------------------
;  messages
;-----------------------------------------------------------------

program:
		db 0Dh, 0Ah
		db "TRDOS 386 v2.0.9 - SYSSTDIO Function Test"
		db 0Dh, 0Ah
len0	equ $-program
		db 0
owner:
		db "Erdogan Tan - 19/09/2024"
		db 0Dh, 0Ah, 0

fff_buffer:	times 24 db 0FFh

press_F_key:
		db 0Dh, 0Ah
		db "Press one of F1-F10 keys to test or press ESC to exit."
		db 0Dh, 0Ah, 0
functions1:
		db 0Dh, 0Ah
		db "F1 - read a character on stdin (wait)"
		db 0Dh, 0Ah
		db "F2 - read a character on stdin (no wait)"
		db 0Dh, 0Ah
		db "F3 - write a character onto stdout (redirection)"
F4_header:
		db 0Dh, 0Ah
		db "F4 - write a character onto stderr (no redirection)"
		db 0Dh, 0Ah,0
functions2:		
		db "F5 - redirect stdin to file (if cl > 0)"
		db 0Dh, 0Ah
		db "F6 - redirect stdout to file (if cl > 0)"
		db 0Dh, 0Ah
		db "F7 - read character (ascii & scancode) on stdin (no redir, wait)"
F8_header:
		db 0Dh, 0Ah
		db "F8 - read character (ascii & scancode) on stdin (no redir, no wait)"
		db 0Dh, 0Ah,0
functions3:
		db "F9 - write character and color onto stdout (no redirection)"
F10_header:
		db 0Dh, 0Ah
		db "F10 - ungetchar (put back the ascii code in u.getc)"
		db 0Dh, 0Ah
		db 0Dh, 0Ah, 0

redir_option_hdr:
		db 0Dh, 0Ah
		db "Select INPUT option to STDOUT:"
		db 0
redir_option:
		db 0Dh, 0Ah
		db "ESC - Sample Text"
		db 0Dh, 0Ah
		db "ENTER - Keyboad (STDIN)"
		db 0Dh, 0Ah, 0

msg_written:	
		db 0Dh, 0Ah
		db "written ..."
		db 0Dh, 0Ah, 0

redir_text_hdr:
		db 0Dh, 0Ah
		db "Sample Text for Redirection Test:"
		db 0Dh, 0Ah, 0
redir_text:
		db "This is a text for TRDOS 386 v2.0.9 SYSSTDIO system call,"
		db 0Dh, 0Ah
		db "STDIN/STDOUT redirection test."
		db 0Dh, 0Ah
		db 0Dh, 0Ah
		db "I run each teen me? Each team. Catch bar duck each teen?"
		db 0Dh, 0Ah
		db "On bar duck each team. Why high one why!"
		db 0Dh, 0Ah, 0
redir_text_size equ $ - (redir_text+1) ;  except 0 

continue_msg:
		db 0Dh, 0Ah
		db "Press a key to continue or press ESC to exit."
crlf:
		db 0Dh, 0Ah, 0
F1_header:
		db 0Dh, 0Ah
		db "F1 - read a character on stdin (wait)"
		db 0Dh, 0Ah, 0
F2_header:
		db 0Dh, 0Ah
		db "F2 - read a character on stdin (no wait)"
		db 0Dh, 0Ah, 0
F1_text:
F2_text:
		db 0Dh, 0Ah
		db "Keyboard is set as STDIN."
		db 0Dh, 0Ah
		db "Console Screen is set as STDOUT."
		db 0Dh, 0Ah
		db "Pressed keys will be sent to STDOUT."
F4_text_2:
		db 0Dh, 0Ah
		db "(ENTER a character string to display.)"
		db 0Dh, 0Ah, 0

F3_header:
		db 0Dh, 0Ah
		db "F3 - write a character onto stdout (redirection)"
		db 0Dh, 0Ah, 0
F3_text:
		db 0Dh, 0Ah
		db "STDOUT will be redirected to STDIO.TXT file."
		db 0Dh, 0Ah, 0
stdio_at_first:
		db 0Dh, 0Ah
		db "At First, the STDIO.TXT file will be opened (from scratch)."
		db 0Dh, 0Ah, 0

stdio_file:
		db "STDIO.TXT", 0
create_error:
		db 0Dh, 0Ah
		db "SYSCREAT - File creating/open error !"
		db 0Dh, 0Ah,0
open_error:
		db 0Dh, 0Ah
		db "SYSOPEN - File open error !"
		db 0Dh, 0Ah,0
zero_file:
		db "STDIO.TXT File Size is zero !", 0Dh, 0Ah, 0
not_found:
		db "STDIO.TXT not found!", 0Dh, 0Ah, 0

F3_enter_msg:
		db 0Dh, 0Ah
		db "Pressed keys will be written into STDOUT file."
		db 0Dh, 0Ah
		db "Written characters will be echoed to STDERR/screen."
		db 0Dh, 0Ah
		db "Then... Press ENTER to stop and close the file."
		db 0Dh, 0Ah
		db "CRLF will be added to the file instead of ENTER/CR."
		db 0Dh, 0Ah, 0

F5_header:
		db 0Dh, 0Ah
		db "F5 - redirect stdin to file"
		db 0Dh, 0Ah, 0
F5_text:
		db 0Dh, 0Ah
		db "STDIN will be redirected to STDIO.TXT file."
		db 0Dh, 0Ah, 0

F4_text_1:
		db 0Dh, 0Ah
		db "This is STDERR message."
		db 0
F4_text_1_end:
F4_string:	db "String: ",0
F4_string_len	equ $ - (F4_string+1)

F6_header:
		db 0Dh, 0Ah
		db "F6 - redirect stdout to file"
		db 0Dh, 0Ah, 0
F6_text:
		db 0Dh, 0Ah
		db "STDOUT will be redirected to STDIO.TXT file."
		db 0Dh, 0Ah, 0

F9_header:
		db 0Dh, 0Ah
		db "F9 - write character and color onto stdout"
		db 0Dh, 0Ah, 0

F9_text:
		db 0Dh, 0Ah
		db "Select a color:"
		db 0Dh, 0Ah
		db "       1 = BLUE   "
		db 0Dh, 0Ah
		db "       2 = GREEN  "
		db 0Dh, 0Ah
		db "       3 = CYAN   "
		db 0Dh, 0Ah
		db "       4 = RED    "
		db 0Dh, 0Ah
		db "       5 = MAGENTA"
		db 0Dh, 0Ah
		db "       6 = YELLOW "
		db 0Dh, 0Ah
		db "       7 = WHITE  "
		db 0Dh, 0Ah
		db "       8 = BROWN  "
		db 0Dh, 0Ah
		db "       9 = GRAY   "
		db 0Dh, 0Ah
		db "       0 = Default"
		db 0Dh, 0Ah, 0

F10_text_1:
		db 0Dh, 0Ah
		db "ENTER a string to test ungetchar feature..."
		db 0Dh, 0Ah, 0

F10_text_2:
		db 0Dh, 0Ah
		db "The 1st char will be written as 5th char without any keypress."
		db 0Dh, 0Ah
		db "(a getchar just after ungetchar works as a virtual keypress)" 
		db 0Dh, 0Ah, 0

F7_header:
		db 0Dh, 0Ah
		db "F7 - read character (ascii & scancode) on stdin (no redir, wait)"
		db 0Dh, 0Ah, 0
F7_text:
F8_text:
		db 0Dh, 0Ah
		db "Press any keys to continue or press ESC to cancel."
		db 0Dh, 0Ah, 0

chartext:	db 0Dh, 0Ah
		db "Character: ",0
char:		db "     ",0 
asciitext:	db "Ascii Code: ",0
ascii:		db "00h  ",0
scanctext:	db "Scan Code: ",0
scanc:		db "00h ",0

filehandle:	dd 0

msg_ok:
		db 0Dh, 0Ah
		db "OK. "
		db 0