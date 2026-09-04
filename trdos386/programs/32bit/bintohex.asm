; ****************************************************************************
; bintohex.asm - TRDOS 386 v2 - sysopen & syscreat & & sysread & syswrite TEST
; ----------------------------------------------------------------------------
; 04/09/2026 - Erdogan Tan
; ****************************************************************************
; convert binary file to hex file (binfilehex.exe compatible hex output)
; ----------------------------------------------------------------------------
; nasm bintohex.asm -l bintohex.txt -o BINTOHEX.PRG -Z error.txt

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
	mov	edi, binfilename

.scan_target_name:
	lodsb
	cmp	al, 20h			; Boşluk kontrolü
	je	short .scan_target_name
	jb	pmsg_usage
	stosb

.copy_target_name:
	lodsb
	cmp	al, 20h
	jna	short .target_name_done
	stosb
	jmp	short .copy_target_name

.target_name_done:
	xor	al, al
	stosb				; Hedef dosya adını NULL ile sonlandır

	; Open file
        sys	_open, binfilename, 0   ; Open for read
        jc	err_open
        mov	[in_fd], eax

        ; create file  with same file name and with "hex" extension
        mov	esi, binfilename
        mov	edi, hexfilename
copy_file_name:
        lodsb
        cmp	al, '.'
        je	.hex_extension
        or	al, al
        je	.hex_extension
        stosb
        jmp	copy_file_name 

.hex_extension:
        mov	eax, '.hex'
        stosd
        mov	byte [edi], 0

        ; Create file
        sys	_create, hexfilename, 0  ; (attributes) ecx = 0 : Normal file
        jc	err_create
        mov	[out_fd], eax

	; read one row (16 bytes) from binary file
read_one_row:
	sys	_read, [in_fd], readbuffer, 16
	jc	err_read

	cmp	eax, 1
        jb	.end_of_file

        mov	ecx, eax

        mov	esi, readbuffer
        mov	edi, writebuffer

.read_next_byte:
        lodsb
        call	byte_to_hex
	stosw

        dec	ecx
        jz	.end_of_row

        mov	al, 20h			; space
        stosb

        jmp	.read_next_byte

.end_of_row:
        mov	eax, 0A0Dh              ; CRLF
	stosw
       
        sub	edi, writebuffer  
 
	; write one row (47+CRLF bytes) to hex file            
        sys	_write, [out_fd], writebuffer, edi
	jc	err_write
        
        cmp	edx, 49
        jnb	read_one_row

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

err_open:
	mov	esi, err_msg_open
	jmp	print_and_terminate
err_create:
	mov	esi, err_msg_create
	jmp	close_print_terminate_1
err_read:
	mov	esi, err_msg_read
	jmp	close_print_terminate_2
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

byte_to_hex:
	movzx	ebx, al
        and	bl, 0Fh
        mov	ah, [ebx+hexchars]
        mov	bl, al
        shr	bl, 4
        mov	al, [ebx+hexchars]
        retn

; ============================================================================
; DATA
; ============================================================================

msg_usage:
	db 0Dh, 0Ah
	db 'Usage: bintohex <binary file name>'
CRLF:
	db 0Dh, 0Ah, 0

err_msg_open: db "[ERROR] Input file not found!", 0Dh, 0Ah, 0
err_msg_create: db "[ERROR] Output file can not be created!", 0Dh, 0Ah, 0
err_msg_read: db "File read error!", 0Dh, 0Ah, 0
err_msg_write: db "File write error!", 0Dh, 0Ah, 0

msg_ok:
	;db 0Dh, 0Ah
        db "OK.", 0Dh, 0Ah, 0

hexchars: db "0123456789ABCDEF"

color: db 0

; ============================================================================
; UNINITIALIZED DATA
; ============================================================================
bss_start:
ABSOLUTE bss_start

in_fd: resd 1
out_fd: resd 1
binfilename: resb 80
hexfilename: resb 80

readbuffer: resb 20  ; 16 bytes + 0 will be used (one row)
writebuffer: resb 52 ; 49 bytes + 0 will be used (one row)

bss_end:

