; ****************************************************************************
; hex2bin.s (TRDOS 386, TRDOS v2.0 - sample binary file, 'hex2bin.prg')
; ----------------------------------------------------------------------------
; HEX2BIN.PRG ! 'sysopen', 'sysread', 'syswrite' TEST program for TRDOS 386 !
;
; 05/11/2016
;
; [ Last Modification: 06/11/2016 ]
;
; ****************************************************************************
; Note: This program is inverse of the BIN2HEX.PRG (05/11/2016, 'bin2hex.s')

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

[BITS 32] ; We need 32-bit intructions for protected mode

[ORG 0] 

START_CODE:
	mov	esi, esp
	lodsd
	cmp	eax, 2 ; two arguments (program file name & hex file name)
	jb	prg_msg ; nothing to do

	lodsd ; program file name address 
	lodsd ; text file name address (file to be read)
	; EAX = arg2 ; file name address

open_hex_file:
	sys	_open, eax, 0 ; open for reading
	jc	open_error

	mov	[fhandle], eax ; file handle/index number

	mov	byte [color], 0Eh ; Yellow
	mov	esi, msg_reading
	call	print_msg

read_file:
	sys	_read, [fhandle], BUFFER, 0FFFFFFFFh ; read all bytes 
 	jc	rw_error	; disk read or memory allocation error

	mov	[fsize], eax ; file size (read bytes)

	call	print_msg_ok

close_hex_file:
	sys	_close, [fhandle] ; close file

	mov	byte [color], 0Fh ; White
	mov	esi, msg_checking
	call	print_msg

	mov	esi, BUFFER
	mov	ecx, [fsize]
	sub	ah, ah  ; 0
	mov	edi, esi
	add	edi, ecx
	sub	edx, edx
chk_hex_0:
	mov	dl, 16
chk_hex_1:
	call	chk_hex_2
	mov	ah, al
	dec	ecx
	jz	short chk_error
	call	chk_hex_2
	shl	ah, 4 ; * 16
	;and	al, 0Fh
	or	al, ah
	stosb
	dec	ecx
	jz	short write_to_bin_file
	lodsb
	dec	edx
	jnz	short chk_hex_5

	cmp	al, 0Dh
	jne	short chk_error
	dec	ecx
	jz	short chk_error
	lodsb
	cmp	al, 0Ah
	jne	short chk_error
	dec	ecx
	jnz	short chk_hex_0
	jmp	write_to_bin_file

rw_error:
	sys	_close, [fhandle] ; close file
chk_error:
	mov	byte [color], 0Ch ; Light Red
	mov	esi, msg_error
	call	print_msg		   ; INT 31h
	jmp	terminate

chk_hex_2:
	lodsb
	cmp	al, '9'
	ja	short chk_hex_3
	cmp	al, '0'
	jb	short chk_hex_4
	sub	al, '0'
	retn
chk_hex_3:
	cmp	al, 'A'
	jb	short chk_hex_4
	cmp	al, 'F'
	ja	short chk_hex_4
	sub	al, 'A'-10
	retn
chk_hex_4:
	pop	eax ; return address
	jmp	short chk_error	

chk_hex_5:
	cmp	al, 20h
	jne	short chk_error
	loop	chk_hex_1
	;jmp	write_to_bin_file

write_to_bin_file:
	mov	ecx, BUFFER
	add	ecx, [fsize]
	cmp	edi, ecx
	jna	chk_error ; ZERO file size

	call	print_msg_ok

	mov	edx, ecx ; 1st byte of converted data
	
create_bin_file:
	sys	_creat, bin_file, 0 ; create (normal) file
	jc	short create_error

	mov	[fhandle], eax

	mov	byte [color], 0Bh ; Light Cyan
	mov	esi, msg_writing
	call	print_msg

	sub	edi, edx ; file size 
	; edx = start address of the text

	sys	_write, [fhandle], edx, edi ; write to 'DB2BIN.BIN' 
 	jc	rw_error	; disk write (or free space) error

	call	print_msg_ok

	sys	_close, [fhandle] ; close file

	mov	byte [color], 07h ; Light Gray (Default Color)
	mov	esi, nextline
	jmp	short print_nl

prg_msg:
	mov	esi, msg_program
print_nl:
	call	print_msg	
terminate:
	sys 	_exit			   ; INT 40h
here:
	jmp	short here

create_error:
	mov	esi, msg_create_error
	jmp	short open_create_error
open_error:
	mov	esi, msg_open_error
open_create_error:
	mov	byte [color], 0Ch ; Light Red
	call	print_msg		   ; INT 31h
	jmp	short terminate	

print_msg_ok:
	mov	byte [color], 0Ah ; Light Green
	mov	esi, msg_ok
print_msg:
	movzx	ebx, byte [color] ; text color (bl)
		               ; video page 0 (bh)
	mov	ah, 0Eh ; teletype output (write tty)
	lodsb
_p_nextchar:
	int	31h
	lodsb
	and	al, al
	jnz	short _p_nextchar
	retn

;-----------------------------------------------------------------
;  messages
;-----------------------------------------------------------------

msg_program:
	db 0Dh, 0Ah
	db "HEX2BIN.PRG /// TRDOS 386 syswrite test program"
	db 0Dh, 0Ah
	db "by Erdogan Tan, 06/11/2016", 0Dh, 0Ah, 0

msg_open_error:
	db 0Dh, 0Ah
	db 'sysopen error !'
	db 0Dh, 0Ah, 0

msg_create_error:
	db 0Dh, 0Ah
	db 'syscreate error !'
nextline:
	db 0Dh, 0Ah, 0

msg_error:
	db 'ERROR ! ', 0Dh, 0Ah, 0

msg_reading:
	db 0Dh, 0Ah
	db 'Reading... ', 0

msg_checking:
	db 0Dh, 0Ah
	db 'Checking & Converting... ', 0

msg_writing:
	db 0Dh, 0Ah
	db 'Writing... ', 0

msg_ok:
	db 'OK. ', 0

bin_file:
	db 'HEX2BIN.BIN', 0

color:	db 0Fh ; White

Digit1:
	db 0
Digit2:
	db 0

bss:

ABSOLUTE bss

alignb 4

fhandle: resd 1
fsize:	 resd 1


BUFFER:	