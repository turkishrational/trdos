; ****************************************************************************
; fstat.s (TRDOS 386, TRDOS v2.0.11 - sample binary file)
; ----------------------------------------------------------------------------
; FSTAT.PRG ! 'sysfstat' system call TEST program for TRDOS 386 !
;
; 10/06/2026
;
; [ Last Modification: 10/06/2026 ]
;
; ****************************************************************************
; nasm fstat.s -l fstat.txt -o FSTAT.PRG 

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

; TRDOS 386 (and Retro UNIX 386 v1) system call format:
; sys systemcall (eax) <arg1 (ebx)>, <arg2 (ecx)>, <arg3 (edx)>

; 22/05/2026 - 'tdosk6.s' - 'sysfstat:'
; ref: tcc (include/sys) stat.h ((modified))
; stat structure:
st_dev	 equ 0
st_ino	 equ 4
st_mode	 equ 8
st_nlink equ 10
st_uid	 equ 12
st_gid   equ 14
st_rdev	 equ 16
st_size  equ 20
st_atime equ 28
st_mtime equ 32
st_ctime equ 36

;-----------------------------------------------------------------
; code
;-----------------------------------------------------------------

[BITS 32] ; 32-bit intructions

[ORG 0]

START_CODE:
	mov	esi, program
	call	print_msg

	mov	esi, esp
	lodsd
	cmp	eax, 2 ; two arguments (program file name & text file name)
	jnb	short open_file

	; nothing to do
	mov	esi, usage

fstat_ok:
	call	print_msg

	;mov	ebx, 0 ; not error
terminate:
	sys 	_exit		; INT 40h
here:
	jmp	short here

open_file:
	lodsd ; program file name address
	lodsd ; file name (argument) address
	; EAX = arg2 ; file name address

	mov	edi, eax

	; Find First File (basic parameters)
	sys	_fff, eax, 003Fh, fff_buffer
	jc	short not_found_err
	
	; check valid file attributes
	test	byte [fff_buffer], 18h	; dir name or volume label
	jnz	short not_found_err	

	mov	esi, txt_file_name
	call	print_msg

	;mov	esi, edi	; file name (input) address
	;call	print_msg

	mov	esi, fff_buffer+10	; short file name (ASCIIZ)
	call	print_msg

	mov	esi, nextline
	call	print_msg

	; Open File for reading
	sys	_open, edi, 0
	jnc	short get_file_info

not_found_err:
	mov	esi, not_found
	call	print_msg

	;mov	ebx, -1 ; error
	jmp	short terminate

get_file_info:
	; eax = file number
	push	eax
	sys	_fstat, eax, fstat_buffer
	pop	eax
	pushf
	; close the file at first
	sys	_close, eax
	popf
	jnc	short write_file_info

	mov	esi, fstat_error
	call	print_msg
	;mov	ebx, -1 ; error
	jmp	terminate

write_file_info:
	; device (logical dos drive)
	mov	esi, txt_st_dev
	call	print_msg

	mov	eax, [fstat_buffer+st_dev]
	cmp	eax, 26
	ja	short get_st_dev_number
	add	al, 'A'
	mov	[txt_drv], al
	sub	al, 'A'
get_st_dev_number:
	mov	edi, number_str
	call	num_to_text

	mov	esi, number_str
	call	print_msg

	mov	esi, txt_ldrv
	call	print_msg

	; inode (first cluster)
	mov	esi, txt_st_ino
	call	print_msg

	mov	eax, [fstat_buffer+st_ino]
	mov	edi, number_str
	call	num_to_text

	mov	esi, number_str
	call	print_msg

	mov	esi, txt_fcluster
	call	print_msg

	; mode (unix type file attributes)
	mov	esi, txt_st_mode
	call	print_msg

	movzx	eax, word [fstat_buffer+st_mode]

	push	eax

	mov	edi, number_str
	call	num_to_htext

	mov	esi, number_str
	call	print_msg	

	pop	eax

	cmp	eax, 81A4h
	je	short octal_644
	cmp	eax, 8124h
	je	short octal_444
	mov	esi, nextline
	jmp	short write_it
octal_444:
	mov	esi, txt_mode_ro
	jmp	short write_it
octal_644:
	mov	esi, txt_mode_norm
write_it:
	call	print_msg

	; number of links (=0) -not used-
	mov	esi, txt_st_nlink
	call	print_msg

	;movzx	eax, word [fstat_buffer+st_nlink]
	mov	ax, [fstat_buffer+st_nlink]
	mov	edi, number_str
	call	num_to_text

	mov	esi, number_str
	call	print_msg

	; user id (=0) -not used-
	mov	esi, txt_st_uid
	call	print_msg

	;movzx	eax, word [fstat_buffer+st_uid]
	mov	ax, [fstat_buffer+st_uid]
	mov	edi, number_str
	call	num_to_text

	mov	esi, number_str
	call	print_msg

	; group id (=0) -not used-
	mov	esi, txt_st_gid
	call	print_msg

	;movzx	eax, word [fstat_buffer+st_gid]
	mov	ax, [fstat_buffer+st_gid]
	mov	edi, number_str
	call	num_to_text

	mov	esi, number_str
	call	print_msg

	; root device (=0) -not used-
	mov	esi, txt_st_rdev
	call	print_msg

	mov	eax, [fstat_buffer+st_rdev]
	mov	edi, number_str
	call	num_to_text

	mov	esi, number_str
	call	print_msg

	; file size (8 bytes)
	mov	esi, txt_st_size
	call	print_msg

	mov	eax, [fstat_buffer+st_size]

	cmp	eax, 1
	ja	short many_bytes

	; 'bytes' -> 'byte '
	mov	[txt_s], 20h	; space

many_bytes:
	mov	edi, number_str
	call	num_to_text

	mov	esi, number_str
	call	print_msg

	cmp	dword [fstat_buffer+st_size+4], 0
	jna	short write_size_bytes

	; write exclamation mark (!)
	mov	esi, exc_mark
	call	print_msg

write_size_bytes:
	mov	esi, txt_bytes
	call	print_msg

	; access date/time (unix epoch time format)
	mov	esi, txt_st_atime
	call	print_msg

	mov	eax, [fstat_buffer+st_atime]
	call	write_date_time

	; last modification date/time (unix epoch time format)
	mov	esi, txt_st_mtime
	call	print_msg

	mov	eax, [fstat_buffer+st_mtime]
	call	write_date_time

	; creating date/time (unix epoch time format)
	mov	esi, txt_st_ctime
	call	print_msg

	mov	eax, [fstat_buffer+st_ctime]
	call	write_date_time

	; next line (CRLF) and exit
	mov	esi, nextline
	jmp	fstat_ok

;-----------------------------------------------------------------

	; write/print message (string/text) on console display
print_msg:
	; esi = asciiz text/string address
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
	
	; binary number to decimal number string/text
num_to_text:
	; eax = binary number
	; edi = decimal string buffer (up to 16 bytes)
	mov	ebp, esp
	mov	ebx, 10
num2text_div:
	xor	edx, edx
	div	ebx
	add	dl, '0'
	push	edx
	and	eax, eax
	jnz	short num2text_div
pop_next:
	pop	eax
	stosb
	cmp	esp, ebp
	jb	short pop_next
	mov	[edi],0
num2text_ok:
	retn

;-----------------------------------------------------------------

	; binary number to hexadecimal number string/text
num_to_htext:
	; eax = binary number (16 bit number)
	; edi = hexadecimal string buffer (6 bytes)
	mov	ebx, eax
	;mov	ecx, 8
	shl	ebx, 16 ; discard high word
	mov	ecx, 4
dd2hex:
	rol	ebx, 4
	mov	dl, bl
	and	edx, 15
	add	edx, hex_digits
	mov	al, [edx]
	stosb
	loop	dd2hex
	mov	al, 'h'
	stosb
	mov	[edi], 0
	retn

;-----------------------------------------------------------------

	; write date & time (as epoch number and as normal)
write_date_time:
	; eax = unix epoch time
	; edi = epoch time string/text buffer address
	push	edi ; **
	push	eax ; *
	mov	esi, edi
	call	epoch_to_str
	; esi = asciiz string/text address
	call	print_msg	
	pop	eax ; *
	; eax = unix (epoch) time ticks/seconds
	call	convert_from_epoch
	mov	edi, [esp] ; **

	; dd/mm/yyyy
	mov	eax, [day]
	call	bin_to_str_2
	mov	byte [edi], '/'
 	inc	edi
	mov	eax, [month]
	call	bin_to_str_2
	mov	byte [edi], '/'
 	inc	edi
	mov	eax, [year]
	call	bin_to_str_4

	mov	byte [edi], 20h ; space
	inc	edi
	
	; hh:mm:ss
	mov	eax, [hour]
	call	bin_to_str_2

	mov	byte [edi], ':'
 	inc	edi

	mov	eax, [minute]
	call	bin_to_str_2

	mov	byte [edi], ':'
 	inc	edi

	mov	eax, [second]
	call	bin_to_str_2

	mov	byte [edi], 0

	mov	esi, lbracket
	call	print_msg

	pop	esi ; *	; string/text buffer address
	call	print_msg

	mov	esi, rbracket
	;call	print_msg
	;retn
	jmp	print_msg

;-----------------------------------------------------------------

bin_to_str_4:
	mov	ebx, 4
	jmp	short bin_to_str
bin_to_str_2:
	mov	ebx, 2
bin_to_str:
	mov	ebp, esp
	mov	ecx, 10
bin2str_div:
	xor	edx, edx
	div	ecx
	add	dl, '0'
	push	edx
	cmp	eax, 0
	ja	short bin2str_div
bin2str_pop:
	pop	eax
	or	ebx, ebx
	jz	short skip_stosb
	stosb
	dec	ebx
skip_stosb:
	cmp	esp, ebp
	jb	short bin2str_pop
zero_prefix:
	or	ebx, ebx
	jz	short bin2str_ok
	mov	byte [edi-1],'0'
	stosb
	dec	ebx
	jmp	short zero_prefix
bin2str_ok:
	retn

;-----------------------------------------------------------------
	
	; convert unix epoch time to string/text
epoch_to_str:
	; eax = unix epoch time
	; edi = epoch time string/text buffer address
	mov	ebp, esp
	mov	ecx, 10
epoch2str_div:
	xor	edx, edx
	div	ecx
	add	dl, '0'
	push	edx
	cmp	eax, 0
	ja	short epoch2str_div
e2s_pop_next:
	pop	eax
	stosb
e2s_skip_stosb:
	cmp	esp, ebp
	jb	short e2s_pop_next
epoch2str_ok:
	mov	byte [edi], 0
	retn

;-----------------------------------------------------------------

	; ref: TRDOS 386 v2.0.11 Kernel - 'trdosk1.s'
convert_from_epoch:
	; 25/07/2022 (v2.0.5)
	; 18/04/2021 (v2.0.4)
	; 31/12/2017 (v2.0.0)
	; 30/12/2017 (TRDOS 386 = TRDOS v2.0)
	; 15/03/2015 (Retro UNIX 386 v1 - 32 bit version)
	; 20/06/2013 (Retro UNIX 8086 v1)
	; 'convert_from_epoch' procedure prototype:
	; 	            UNIXCOPY.ASM, 10/03/2013
	;
	; ((Modified registers: EAX, EDX, ECX, EBX))
	;
	; Derived from DALLAS Semiconductor
	; Application Note 31 (DS1602/DS1603)
	; 6 May 1998
	;
	; INPUT:
	; EAX = Unix (Epoch) Time
	;
	xor 	edx, edx
	;mov 	ecx, 60
	; 25/07/2022
	sub	ecx, ecx
	mov	cl, 60
	div	ecx
	;mov 	[imin], eax   ; whole minutes
			  ; since 1/1/1970
	mov 	[second], dx  ; leftover seconds
	sub 	edx, edx
	div	ecx
	;mov 	[ihrs], eax   ; whole hours
	;		      ; since 1/1/1970
	mov 	[minute], dx  ; leftover minutes
	xor	edx, edx
	;mov 	cx, 24
	mov 	cl, 24
	div	ecx
	;mov 	[iday], ax   ; whole days
			     ; since 1/1/1970
	mov 	[hour], dx   ; leftover hours
	add 	eax, 365+366 ; whole day since
			     ; 1/1/1968
	;mov 	[iday], ax
	push 	eax
	sub	edx, edx
	;mov 	ecx, (4*365)+1 ; 4 years = 1461 days
	; 25/07/2022
	mov	cx, (4*365)+1
	div	ecx
	pop 	ecx
	;mov 	[lday], ax   ; count of quadyrs (4 years)
	;push 	dx
	; 18/04/2021
	push	edx
	;mov 	[qday], dx   ; days since quadyr began
	cmp 	dx, 31+29    ; if past feb 29 then
	cmc		     ; add this quadyr's leap day
	adc 	eax, 0	     ; to # of qadyrs (leap days)
	;mov 	[lday], ax   ; since 1968
	;mov 	cx, [iday]
	xchg 	ecx, eax     ; ECX = lday, EAX = iday
	sub 	eax, ecx     ; iday - lday
	;mov 	ecx, 365
	; 25/07/2022
	mov	cx, 365
	xor	edx, edx
	; EAX = iday-lday, EDX = 0
	div	ecx
	;mov 	[iyrs], ax   ; whole years since 1968
	;jday = iday - (iyrs*365) - lday
	;mov	[jday], dx   ; days since 1/1 of current year
	;add	eax, 1968
	add 	ax, 1968     ; compute year
	mov 	[year], ax
	;mov 	cx, dx
	; 25/07/2022
	mov	ecx, edx
	;;mov 	dx, [qday]
	;pop 	dx
	; 18/04/2021
	pop	edx
	cmp 	dx, 365	     ; if qday <= 365 and qday >= 60
	ja 	short cfe1   ; jday = jday +1
	cmp 	dx, 60       ; if past 2/29 and leap year then
        cmc		     ; add a leap day to the # of whole
	;adc 	cx, 0        ; days since 1/1 of current year
	; 25/07/2022
	adc	ecx, 0
cfe1:
	;mov 	[jday], cx
	;mov 	bx, 12       ; estimate month
	; 18/04/2021
	sub	ebx, ebx
	mov	bl, 12
	mov 	dx, 366      ; mday, max. days since 1/1 is 365
	and 	ax, 11b      ; year mod 4 (and dx, 3)
cfe2:	; Month calculation  ; 0 to 11  (11 to 0)
	;cmp 	cx, dx       ; mday = # of days passed from 1/1
	; 25/07/2022
	cmp	ecx, edx
	jnb 	short cfe3
	;dec 	bx           ; month = month - 1
	;shl 	bx, 1
	; 18/04/2021
	dec	bl
	shl	bl, 1 
	mov 	dx, [EBX+DMonth] ; # elapsed days at 1st of month
	; 18/04/2021
	;shr 	bx, 1        ; bx = month - 1 (0 to 11)
	shr	bl, 1
	;cmp	bx, 1        ; if month > 2 and year mod 4  = 0	
	cmp	bl, 1
	jna 	short cfe2   ; then mday = mday + 1
	jna 	short cfe2   ; then mday = mday + 1
	or 	al, al       ; if past 2/29 and leap year then
	jnz 	short cfe2   ; add leap day (to mday)
	;inc 	dx           ; mday = mday + 1
	; 25/07/2022
	inc	edx
	jmp 	short cfe2
cfe3:
	;inc 	bx	     ; -> bx = month, 1 to 12
	; 18/04/2021
	inc	bl
	mov 	[month], bx
	;sub 	cx, dx	     ; day = jday - mday + 1
	; 25/07/2022
	sub	ecx, edx
	;inc 	cx
	; 18/04/2021
	inc	cl
	;mov 	[day], cx
	mov	[day], cl

	; eax, ebx, ecx, edx is changed at return
	; output ->
	; [year], [month], [day], [hour], [minute], [second]

	retn	; 31/12/2017 (TRDOS 386)

;-----------------------------------------------------------------
;  data
;-----------------------------------------------------------------

program:
	db 0Dh, 0Ah
	db "FSTAT.PRG /// TRDOS 386 (v2.0.11) sysfstat TEST program"
	db 0Dh,0Ah
	db "by Erdogan Tan, 10/06/2026",0Dh,0Ah,0
usage:
	db 0Dh,0Ah
	db "fstat <filename.ext>"
nextline:
	db 0Dh,0Ah,0

not_found:
	db 0Dh,0Ah
	db "Error ! "
	db "File not found !", 0Dh,0Ah,0

fstat_error:
	db 0Dh,0Ah
	db "'sysfstat' system call error !"
	db 0Dh,0Ah,0

txt_file_name:
	db 0Dh,0Ah
	db "File Name    : ",0
txt_st_dev:
	db 0Dh,0Ah
	db "st_dev       : ",0

txt_ldrv:
	db " ("
txt_drv:
	db "?:"
	db ")", 0

txt_st_ino:
	db 0Dh,0Ah
	db "st_ino       : ",0

txt_fcluster:
	db " (first cluster)", 0

txt_st_mode:
	db 0Dh,0Ah
	db "st_mode      : ",0

txt_mode_norm:
	db " (Normal: rw-r--r--)",0
txt_mode_ro:
	db " (Readonly: r--r--r--)",0

txt_st_nlink:
	db 0Dh,0Ah
	db "st_nlink     : ",0

txt_st_uid:
	db 0Dh,0Ah
	db "st_uid       : ",0

txt_st_gid:
	db 0Dh,0Ah
	db "st_gid       : ",0

txt_st_rdev:
	db 0Dh,0Ah
	db "st_rdev      : ",0

txt_st_size:
	db 0Dh,0Ah
	db "st_size      : ",0

txt_bytes:
	db " byte"
txt_s:	db "s ",0	

txt_st_atime:
	db 0Dh,0Ah
	db "st_atime     : ",0

txt_st_mtime:
	db 0Dh,0Ah
	db "st_mtime     : ",0

txt_st_ctime:
	db 0Dh,0Ah
	db "st_ctime     : ",0

exc_mark:
	db ' ! ',0

hex_digits:
	db '0123456789ABCDEF'

lbracket:
	db " [",0
rbracket:
	db "]",0	

;-----------------------------------------------------------------

DMonth:
	dw 0
	dw 31
	dw 59
	dw 90
	dw 120
	dw 151
	dw 181
	dw 212
	dw 243
	dw 273
	dw 304
	dw 334

year:	dd 0
month:	dd 0
day:	dd 0
hour:	dd 0
minute: dd 0
second: dd 0

;-----------------------------------------------------------------
;  uninitialized data
;-----------------------------------------------------------------

bss:

ABSOLUTE bss

alignb 4
fstat_buffer:	resb 40
fff_buffer:	resb 24
number_str:	resb 16
		resb 20
