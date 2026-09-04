; ****************************************************************************
; hextobin.asm - TRDOS 386 v2 - sysopen & syscreat & & sysread & syswrite TEST
; ----------------------------------------------------------------------------
; 04/09/2026 - Erdogan Tan
; ****************************************************************************
; convert hex (binfilehex.exe compatible hex) file to binary file
; ----------------------------------------------------------------------------
; nasm hextobin.asm -l hextobin.txt -o HEXTOBIN.PRG -Z error.txt

; TRDOS 386 Sytem Calls
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
    int 40h 
%endmacro

; ============================================================================
; CODE
; ============================================================================

[BITS 32]
[ORG 0]

START_CODE:
	; Clear BSS section
	;mov	di, bss_start
	;mov	ecx, (bss_end - bss_start)/4
	;rep	stosd

GetCommandLineArgs:
	mov	esi, esp
	lodsd				; EAX = Number of arguments (argc)
	cmp	eax, 2			; Two arguments are needed: "bintohex filename.ext"
	jb	pmsg_usage		; Show usage message

	lodsd				; Skip "bintohex.prg" file name address (argv[0])

	lodsd				; EAX = Binary file name addr to be converted (argv[1])
	mov	esi, eax
	mov	edi, hexfilename

.scan_target_name:
	lodsb
	cmp	al, 20h			; Boşluk kontrolü
	je	short .scan_target_name
	jb	pmsg_usage
	stosb
        xor	edx, edx ; 0 
.copy_target_name:
	lodsb
	cmp	al, 20h
	jna	short .target_name_done
        cmp	al, '.'
        jne     .not_dot
        mov	edx, edi
.not_dot:
        stosb
        jmp	short .copy_target_name

.target_name_done:
	xor	al, al
	stosb				; Hedef dosya adını NULL ile sonlandır

        ; "HEX" extension check
        and	edx, edx
	jz	err_not_hex
	mov	eax, [edx]
        and	eax, 0DFDFDFFFh		; convert to uppercase ('.HEX')
        cmp	dword [edx], '.HEX'
        je      err_not_hex

	; Open file
        sys	_open, hexfilename, 0   ; Open for read
        jc	err_open
        mov	[in_fd], eax

        ; create file  with same file name and with "bin" extension
        mov	eax, '.BIN'
        cmp	byte [edx+1], 'h'
        jne	short .skip_bin
        or	eax, 20202000h		; convert to lowercase ('.bin')
.skip_bin:
        mov	esi, hexfilename
        mov	edi, binfilename
	mov	ecx, edx
        sub	ecx, esi		; offset the dot - offset filename   
	rep	movsb
        stosd				; '.bin' or '.BIN' 
        mov	al, 0
        stosb

        ; Create bin (outut) file
        sys	_create, binfilename, 0 ; (attributes) ecx = 0 : Normal file
        jc	err_create
        mov	[out_fd], eax

        ; now, validate hex file (according to 'binfilehex.exe' hex format)
        sys	_read, [in_fd], readbuffer, 49
        jc	err_read

	mov	[readcount], eax
 
        cmp	eax, 4	; XX+CRLF
        jb	err_not_valid

        cmp	eax, 49
	jb	.very_small_file

        cmp	word [readbuffer+47], 0A0Dh ; CRLF
        jne	err_not_valid

	mov	eax, [readbuffer]
.check_al:
	cmp	al, '0'
	jb	err_not_valid
        cmp	al, '9'
	jna	.check_ah
.check_al_A:
        cmp	al, 'A'
	jb	err_not_valid
	cmp	al, 'F'
	ja	err_not_valid
.check_ah:
	cmp	ah, '0'
	jb	err_not_valid
        cmp	ah, '9'
	jna	.check_space
.check_ah_A:
        cmp	ah, 'A'
	jb	err_not_valid
	cmp	ah, 'F'
	ja	err_not_valid
.check_space:
        shr	eax, 16
	cmp	al, 20h
	jne	err_not_valid
	cmp	ah, '0'
	jb	err_not_valid

	; valid HEX file
        ; read hex words
.read_row:
	mov	esi, readbuffer
	mov	edi, writebuffer
.next_byte:
	lodsw
	call	hex_to_byte
	stosb
	lodsb
	cmp	al, 20h
	jb	.end_of_row
	jmp	.next_byte

.very_small_file:
	sub	eax, 2
	add	eax, readbuffer		; CRLF position 
        cmp	word [eax], 0A0Dh	; CRLF
        jne	err_not_valid
	jmp	.read_row

.end_of_row:
	sub	edi, writebuffer	; write count
        sys	_write, [out_fd], writebuffer, edi 

	cmp	byte [readcount], 49
	jb	.end_of_file

        ; now, next row 
        sys	_read, [in_fd], readbuffer, 49
        jc	err_read

	;and	eax, eax
	;jz	.end_of_file
	cmp	eax, 4			; XX+CRLF
	jb	.end_of_file

	mov	[readcount], eax
	jmp	.read_row

.end_of_file:
	sys	_close, [in_fd]
        sys	_close, [out_fd]
	mov	byte [color], 07h
	mov	esi, msg_ok
	call	print_msg
        sys	_exit, 0 

pmsg_usage:
	mov	byte [color], 0Eh
	mov	esi, msg_usage
	call	print_msg
	sys	_exit, 0

err_not_hex:
	mov	esi, err_msg_not_hex
	jmp	print_and_terminate

err_open:
	mov	esi, err_msg_open
	jmp	print_and_terminate

err_create:
	mov	esi, err_msg_create
	jmp	close_print_terminate_1
err_read:
	mov	esi, err_msg_read
	jmp	close_print_terminate_2

err_not_valid:
	mov	esi, err_msg_not_valid
	jmp	close_print_terminate_1

err_write:
	mov	esi, err_msg_write
close_print_terminate_2:
        sys	_close, [out_fd]
close_print_terminate_1:
        sys	_close, [in_fd]
print_and_terminate:
	mov	byte [color], 0Fh
	call	print_msg
	sys	_exit, 1
hang:
	nop
	jmp	hang

print_msg:
	sys	_msg, esi, 255, [color]
	retn

hex_to_byte:
	cmp	ah, 'A'
	jb	short .numeric_ah
        sub	ah, 'A'-10
        jmp	.check_al
.numeric_ah:
	sub	ah, '0'
.check_al:
	cmp	al, 'A'
	jb	short .numeric_al
        sub	al, 'A'-10
        jmp	.set_al
.numeric_al:
	sub	al, '0'
.set_al:
	shl	al, 4
	or	al, ah
	retn

; ============================================================================
; DATA
; ============================================================================

color:	db 0

msg_usage:
	db 0Dh, 0Ah
	db 'Usage: hextobin <hex file name>'
CRLF:
	db 0Dh, 0Ah, 0

err_msg_open: db "[ERROR] Input file not found!", 0Dh, 0Ah, 0
err_msg_create: db "[ERROR] Output file can not be created!", 0Dh, 0Ah, 0
err_msg_read: db "File read error!", 0Dh, 0Ah, 0
err_msg_write: db "File write error!", 0Dh, 0Ah, 0

err_msg_not_hex: db "[ERROR] Not HEX file!", 0Dh, 0Ah, 0
err_msg_not_valid: db "[ERROR] Not a valid HEX file!", 0Dh, 0Ah, 0

msg_ok:
	;db 0Dh, 0Ah
        db "OK.", 0Dh, 0Ah, 0

; ============================================================================
; UNINITIALIZED DATA
; ============================================================================
bss_start:
ABSOLUTE bss_start

in_fd: resd 1
out_fd: resd 1
readcount: resd 1
hexfilename: resb 80
binfilename: resb 80

readbuffer: resb 52 ; 49 bytes + 0 will be used (one row)
writebuffer: resb 20  ; 16 bytes + 0 will be used (one row)

bss_end:

