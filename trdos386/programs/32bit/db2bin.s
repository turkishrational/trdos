; ****************************************************************************
; db2bin.s (TRDOS 386, TRDOS v2.0 - sample binary file, 'db2bin.prg')
; ----------------------------------------------------------------------------
; DB2BIN.PRG ! 'sysopen', 'sysread', 'syswrite' TEST program for TRDOS 386 !
;
; 03/11/2016
;
; [ Last Modification: 05/11/2016 ]
;
; ****************************************************************************
; Note: This program is inverse of the BIN2DB.PRG (03/11/2016, 'bin2db.s')


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
	cmp	eax, 2 ; two arguments (program file name & text file name)
	jb	prg_msg ; nothing to do

	lodsd ; program file name address 
	lodsd ; text file name address (file to be read)
	; EAX = arg2 ; file name address

open_txt_file:
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

close_txt_file:
	sys	_close, [fhandle] ; close file

	mov	byte [color], 0Fh ; White
	mov	esi, msg_checking
	call	print_msg

	mov	esi, BUFFER
	mov	ecx, [fsize]
	sub	ah, ah  ; 0
	mov	edi, esi
	add	edi, ecx
chk_db_0:
	lodsb
	cmp	al, 'd'
	je	short chk_db_6
	cmp	al, 'D'
	je	short chk_db_6
	cmp	al, ';' ; comment line
	je	chk_db_17
	cmp	al, 09h ; TAB key
	je	short chk_db_1
	cmp	al, 20h ; SPACE
	je	short chk_db_1
	cmp	al, 0Dh
	je	short chk_db_2
	cmp	al, 0Ah
	je	short chk_db_3

chk_db_1:
	loop	chk_db_0
	jmp	write_to_bin_file

chk_db_2:
	cmp	ah, 'h'
	je	short chk_db_1
	cmp	ah, 'H'
	je	short chk_db_1
	cmp	ah, 0Ah
	je	short chk_db_4
	cmp	ah, 0
	jne	chk_error
	mov	ah, al ; 0Dh
	jmp	short chk_db_1

chk_db_3:
	cmp	ah, 0Dh
	je	short chk_db_4
	cmp	ah, 'h'
	je	short chk_db_1
	cmp	ah, 'H'
	je	short chk_db_1
	cmp	ah, 0
	jne	chk_error
	mov	ah, al ; 0Ah
	jmp	short chk_db_1

chk_db_4:
	mov	ah, 0
	jmp	short chk_db_1

chk_db_5: ; al  = 'd'
	mov	ah, al
	lodsb
	cmp	al, 'b'
	je	short chk_db_9
	cmp	al, 'B'
	je	short chk_db_9
	jmp	chk_error	

chk_db_6:
	loop	chk_db_5
	jmp	chk_error

chk_db_7:
	lodsb
	cmp	al, 20h ; space
	jne	short chk_db_10
	loop	chk_db_7
	jmp	write_to_bin_file

chk_db_8:
	lodsb
	cmp	al, 20h ; space
	jne	chk_error
	loop	chk_db_7
	jmp	chk_error

chk_db_9: ; al  = 'b'
	loop	chk_db_8
	jmp	chk_error

chk_db_10:
	jb	chk_db_24
	cmp	al, '0'
	jb	chk_db_18
	cmp	al, '9'
	jna	short chk_db_11
	cmp	al, 'A'
	jb	chk_db_16
	cmp	al, 'F'
	jna	short chk_db_11
	cmp	al, 'a'
	jb	chk_error
	cmp	al, 'f'
	jna	short chk_db_11

	cmp	ah, '0' ; numeric chars
	jne	chk_error
	and	al, 0DFh ; capital letter
	cmp	al, 'H'
	jne	chk_error
	cmp	[Digit2], ah  ;'0'
	jnb	chk_db_19
	mov	al, [Digit1]
	mov	[Digit1], ah  ;'0'
	mov	[Digit2], al
	jmp	short chk_db_19

chk_db_11:
	mov 	ah, '0'
	cmp	[Digit1], ah ;'0'
	ja	short chk_db_12
	je	short chk_db_14

	mov	[Digit1], al
	jmp	short chk_db_13

chk_db_12:
	cmp	[Digit2], ah ;'0'
	jnb	chk_error

	mov	[Digit2], al
chk_db_13:
	;loop	chk_db_7
	dec	ecx
	jnz	chk_db_7
	jmp	chk_error
	
chk_db_14:
	cmp	[Digit2], ah ; '0'
	jna	short chk_db_15
	mov	ah, [Digit2]
	mov	[Digit1], ah ; 'A'..'F', 'a'..'f'
	mov	ah, '0'
chk_db_15:
	mov	[Digit2], al
	jmp	short chk_db_13

chk_db_16:
	cmp	al, ';' ; comment line
	jne	short chk_db_18
chk_db_17:
	loop	chk_db_20
	jmp	write_to_bin_file

chk_db_18:
	cmp	al, ',' 
	jne	chk_error
	;and	ah, 0DFh
	;cmp	ah, 'H'
	cmp	ah, 'h'
	jne	chk_error
	dec	ecx
	jz	chk_error
	mov	ah, al
	jmp	chk_db_7

chk_db_19:
	mov	al, [Digit1]
	;cmp	al, '0'
	;jb	chk_error
	mov	ah, [Digit2]
	;cmp	ah, '0'
	;jb	chk_error

	call	calc_number
	xchg	ah, al
	shl	ah, 4 ; * 16
	call	calc_number
	;and	al, 0Fh
	or	al, ah
	stosb

	;mov	byte [Digit1], 0
	;mov	byte [Digit2], 0

	xor	ax, ax
	mov	[Digit1], ax

	mov	ah, 'h'

	;loop	chk_db_7	
	dec	ecx
	jnz	chk_db_7
	
	jmp	write_to_bin_file
	
chk_db_20:
	cmp	al, 20h
	jnb	short chk_db_22
	cmp	al, 09h ; TAB key
	je	short chk_db_22
	mov	ah, 0Dh
	cmp	al, 0Ah
	je	short chk_db_21
	mov	ah, 0Ah
	cmp	al, 0Dh
	je	short chk_db_21
	loop	chk_db_23
	jmp	chk_error

chk_db_21:
	dec	ecx
	jz	chk_error
	lodsb
	cmp	al, ah
	jne	chk_error
	dec	ecx
	jnz	chk_db_0
	jmp	write_to_bin_file

chk_db_22:	
	loop	chk_db_23 		
	jmp	short write_to_bin_file

chk_db_23:
	lodsb
	jmp	short chk_db_20

chk_db_24:
	cmp	al, 0Ah
	jne	short chk_db_27
	cmp	ah, 0Dh
	je	short chk_db_26
	cmp	ah, 'h'
	;je	short chk_db_25
	;cmp	ah, 'H'
	jne	chk_error

chk_db_25:
	mov	ah, al

	;loop	chk_db_7
	dec	ecx
	jnz	chk_db_7	

	jmp	chk_error

chk_db_26:
	mov	ah, 0
	
	;loop	chk_db_0
	dec	ecx
	jnz	chk_db_0

	jmp	short write_to_bin_file

chk_db_27:
	cmp	al, 0Dh
	jne	short chk_db_28
	cmp	ah, 0Ah
	je	short chk_db_26
	cmp	ah, 'h'
	;je	short chk_db_25
	;cmp	ah, 'H'
	je	short chk_db_25

	jmp	chk_error

chk_db_28:
	cmp	al, 09h ; TAB key
	jne	chk_error
	
	;loop	chk_db_7
	
	dec	ecx
	jnz	chk_db_7
	
	;jmp	short write_to_bin_file

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
 	jc	short rw_error  ; disk write (or free space) error

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

rw_error:
	sys	_close, [fhandle] ; close file
chk_error:
	mov	byte [color], 0Ch ; Light Red
	mov	esi, msg_error
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

calc_number:
	; AL = numeric character
	cmp	al, '9'
	ja	short calc_num1
	sub	al, '0'
	retn
calc_num1:
	and	al, 0DFh ; convert to capital letter
	sub	al, 'A'-10
	retn


;-----------------------------------------------------------------
;  messages
;-----------------------------------------------------------------

msg_program:
	db 0Dh, 0Ah
	db "DB2BIN.PRG /// TRDOS 386 syswrite test program"
	db 0Dh, 0Ah
	db "by Erdogan Tan, 05/11/2016", 0Dh, 0Ah, 0

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
	db 'DB2BIN.BIN', 0

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