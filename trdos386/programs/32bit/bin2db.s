; ****************************************************************************
; bin2db.s (TRDOS 386, TRDOS v2.0 - sample binary file, 'bin2db.prg')
; ----------------------------------------------------------------------------
; BIN2DB.PRG ! 'sysopen', 'sysread', 'syswrite' TEST program for TRDOS 386 !
;
; 25/10/2016
;
; [ Last Modification: 05/11/2016 ]
;
; ****************************************************************************

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
	cmp	eax, 2 ; two arguments (program file name & binary file name)
	jb	prg_msg ; nothing to do

	lodsd ; program file name address 
	lodsd ; binary file name address (file to be read)
	; EAX = arg2 ; file name address

open_bin_file:
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

close_bin_file:
	sys	_close, [fhandle] ; close file

convert_byte_to_db:
	mov	esi, BUFFER
	mov	edx, esi
	add	edx, [fsize]
	mov	edi, edx 

	xor	ecx, ecx
c_0:
	mov	cl, 8
c_1:
	mov	ax, 'db'
	stosw
	mov	al, ' '
	stosb
c_2: 
	lodsb
	call	hex
	cmp	al, '9'
	jna	short c_3	

	mov	byte [edi], '0'
	inc	edi
c_3:
	stosw
	mov	al, 'h'
	stosb

	dec	dword [fsize]
	jz	short c_5

	dec	cl
	jz	short c_4

	mov	ax, ', '
	stosw
	jmp	short c_2
c_4:
	mov 	ax, 0A0Dh
	stosw
	jmp	short c_0
c_5:
	mov 	ax, 0A0Dh
	stosw

create_txt_file:
	sys	_creat, txt_file, 0 ; create (normal) file
	jc	short create_error

	mov	[fhandle], eax

	mov	byte [color], 0Bh ; Light Cyan
	mov	esi, msg_writing
	call	print_msg

	sub	edi, edx ; file size 
	; edx = start address of the text

	sys	_write, [fhandle], edx, edi ; write to 'BIN2DB.TXT' 
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

hex:
	movzx	ebx, al
	shr	bl, 4
	mov	bl, [ebx+hexchrs] 	 	
	xchg	bl, al
	and	bl, 0Fh
	mov	ah, [ebx+hexchrs] 
	retn

hexchrs:
	db '0123456789ABCDEF'

;-----------------------------------------------------------------
;  messages
;-----------------------------------------------------------------

msg_program:
	db 0Dh, 0Ah
	db "BIN2DB.PRG /// TRDOS 386 syswrite test program"
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

msg_writing:
	db 0Dh, 0Ah
	db 'Writing... ', 0

msg_ok:
	db 'OK. ', 0

txt_file:
	db 'BIN2DB.TXT', 0

color:	db 0Fh ; White

bss:

ABSOLUTE bss

alignb 4

fhandle: resd 1
fsize:	 resd 1

BUFFER:	