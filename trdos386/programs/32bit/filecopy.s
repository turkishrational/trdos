; ****************************************************************************
; filecopy.s (TRDOS 386, TRDOS v2.0 - sample binary file, 'filecopy.prg')
; ----------------------------------------------------------------------------
; FILECOPY.PRG ! 'sysopen', 'sysread', 'syscreate', 'syswrite'
;		 TEST program for TRDOS 386 !
;
; 31/10/2016
;
; [ Last Modification: 13/01/2026 ]
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

[BITS 32] ; 80386 protected mode (32 bit instructions)

[ORG 0]   ; virtual start address  = 0

START_CODE:
	mov	esi, esp
	lodsd
	cmp	eax, 3 ; two arguments (program file name & binary file name)
	jb	prg_msg ; nothing to do

	lodsd ; program file name address
	lodsd ; binary file name address (source file to be read)
	; EAX = arg2 ; source file name address

open_bin_file:
	sys	_open, eax, 0 ; open for reading
	jc	open_error

	mov	[fhandle1], eax ; file handle/index number

	lodsd
	; EAX = arg3 ; destination file name address

	mov	[dfname_addr], eax

	sys	_open, eax, 1 ; open for writing
	; 13/01/2026
	jc	short _1

	; 13/01/2026
	; eax = file number
	sys	_close, eax ; close file

	;;;;
	mov	esi, Msg_DoYouWantOverWriteFile
	call	print_msg

	mov	esi, [dfname_addr] ; file (path) name
	call	print_msg

	mov	esi, Msg_YesNo
	call	print_msg

ask_for_owr_again:
	; Y/N ?
	; read a character
	mov     ah, 0
	int     32h

	cmp	al, 1Bh	; ESCape ?
        je      short response_no ; yes

	and	al, 0DFh
	cmp	al, 'Y'
	jne	short check_for_no

	mov	esi, msg_yes
	call	print_msg

	jmp	short read_file

check_for_no:
	cmp	al, 'N'
      	jne	short ask_for_owr_again
response_no:
	mov	word [msg_no], 'No'
	mov	esi, msg_no
	;call	print_msg
	;jmp	terminate
	jmp	print_nl
	;;;;

_1:
	cmp	eax, 2 ; file not found ?
	jne	open_error

read_file:
	mov	byte [color], 0Eh ; Yellow

	mov	esi, msg_reading
	call	print_msg

	sys	_read, [fhandle1], BUFFER, 0FFFFFFFFh ; read all bytes
 	jc	r_error		; disk read or memory allocation error

	mov	[fsize], eax ; file size (read bytes)

	call	print_msg_ok

close_bin_file:
	sys	_close, [fhandle1] ; close file

create_or_owr_file:
	sys	_create, [dfname_addr], 0 ; create (normal) file
	jc	short create_error

	mov	[fhandle2], eax
_2:
	mov	byte [color], 0Bh ; Light Cyan
	mov	esi, msg_writing
	call	print_msg

	sys	_write, [fhandle2], BUFFER, [fsize] ; write to destination
 	jc	short w_error     ; disk write (or free space) error

	call	print_msg_ok

	sys	_close, [fhandle2] ; close file

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

r_error:
	sys	_close, [fhandle1] ; close file
	jmp	short rw_error
w_error:
	sys	_close, [fhandle2] ; close file
rw_error:
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
	db "FILECOPY.PRG /// TRDOS 386 syswrite test program"
	db 0Dh, 0Ah
	db "by Erdogan Tan, 13/01/2026", 0Dh, 0Ah
	db 0Dh, 0Ah
	db 'Usage: FILECOPY <Source File> <Destination File>'
	db 0Dh, 0Ah, 0

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

color:	db 0Fh ; White

; 13/01/2026
Msg_DoYouWantOverWriteFile:
	db 'Do you want to overwrite file ',0
Msg_YesNo:
	db ' (Y/N) ? ', 0
msg_yes:
	db 'Y'
msg_no:
	db 'es '
	db 0Dh, 0Ah, 0

bss:

ABSOLUTE bss

alignb 4

fhandle1:    resd 1
fhandle2:    resd 1
dfname_addr: resd 1
fsize:	     resd 1

BUFFER: