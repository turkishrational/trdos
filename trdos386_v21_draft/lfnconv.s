; ****************************************************************************
; lfnconv.s (TRDOS 386 v2.1 test - long file name to short file name convert)
; ----------------------------------------------------------------------------
; LFNCONV.PRG ! TEST program !
;
; 26/05/2024
;
; Modified from 'fsfnconv.s' source code (25/05/2025)
;
; [ Last Modification: 28/05/2025 ]
;
; ****************************************************************************

; 20/10/2024
; 20/08/2024 ; TRDOS 386 v2.0.9
; TRDOS 386 system calls
_ver 	equ 0
_exit 	equ 1
_fork 	equ 2
_read 	equ 3
_write	equ 4
_open	equ 5
_close 	equ 6
_wait 	equ 7
_creat 	equ 8
_rename equ 9
_delete equ 10
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
_mem	equ 21
_prompt	equ 22
_path	equ 23
_env	equ 24
_stime	equ 25
_quit	equ 26
_intr	equ 27
_dir	equ 28
_emt 	equ 29
_ldvrt 	equ 30
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

; Retro UNIX 386 v1 system call format:
; sys systemcall (eax) <arg1 (ebx)>, <arg2 (ecx)>, <arg3 (edx)>

[BITS 32] ; 32-bit intructions

[ORG 0] 

START_CODE:
	sys	_msg, version, 255, 0Ah
	mov	esi, esp
	lodsd
	and	eax, eax
	jz	short terminate
	cmp	eax, 2	; number of arguments
	jb	short show_usage
	mov	ebp, eax
	lodsd	; skip program file name
	dec	ebp
	mov	ebx, [esi]
	cmp	byte [ebx], '~'
	jne	short nextarg
	dec	ebp
	jz	short show_usage
	inc	ebx ; skip tilde
	call	convert_to_order_number
	lodsd	; skip  order number
nextarg:
	mov	edi, lfn_name ; long file name
	mov	ecx, 128 ; long name limit
next_word:
	lodsd
	call	move_file_name
	jcxz	put_zero
	dec	ebp
	jnz	short next_word
put_zero:
	xor	eax, eax
	stosb

	mov	esi, lfn_name
	mov	edi, target_name

	call	convert_name_from_lfn
	
	inc	eax ; -1 -> 0
	jz	short inv_file_name
	dec	eax ; 1 -> 0
	jz	short show_name
		; exact short name without tilde
	
	mov	eax, [order_number]
	and	eax, eax
	jz	short show_name

	; 27/05/2025
	cmp	byte [lossy_conversion], 0
	jna	short show_name

	; 28/05/2025
	;mov	edi, target_name
	
	call	change_tilde_number
show_name:
	call	print_short_name

terminate: 
	sys	_exit
halt:
	jmp	short halt

show_usage:
	sys	_msg, usage, 255, 0Fh
	jmp	short terminate

nul_name:
inv_file_name:
	sys	_msg, msg_invalid_fn, 255, 07h
	jmp	short terminate

print_short_name:
	sys	_msg, nexline, 2, 07h
	; 28/05/2025
	sys	_msg, target_name, 255, 0Fh

	cmp	byte [conv_ucase], 0
	ja	short skip_special_status

	; 26/05/2025 - Erdogan Tan
	; Windows note:
	; If lfn 'filename' is converted to
	; DOS 8.3 file name as 'FILENAME'
	; it will be shown as 'filename' (by Windows)
	; without using long name entry.
	; In this case, [DIR_NTRes] byte will be set to 08h.
	;
	; Ref: FAT32 File System Specification, v1.03, Page 23
	;      (FAT 32 byte directory entry structure)

	sys	_msg, special_status, 255, 07h

skip_special_status:
	sys	_msg, nexline, 2, 07h
	retn

convert_to_order_number:
	; ebx = order number
	;    (which will be used after tilde)
	mov	ecx, 5 ; max. 5 chars/digits
	xor	eax, eax ; 0
	mov	[order_number], eax
ctordn_1:
	mov	al, [ebx]
	cmp	al, '9'
	ja	short ctordn_2
	cmp	al, '0'
	jb	short ctordn_2
	sub	al, '0'
	push	eax
	mov	al, 10
	mul	dword [order_number]
	mov	[order_number], eax
	pop	eax
	add	[order_number], eax
	xor	eax, eax
	inc	ebx
	loop	ctordn_1
	cmp	dword [order_number], 65535
	ja	short ctordn_3
ctordn_2:
	retn
ctordn_3:
	mov	dword [order_number], 65535
	retn

move_file_name:
	; eax = argument, fs file name text
	; ecx = remain byte count
	push	esi
	mov	esi, eax
mfn_@:
	lodsb
	cmp	al, 20h
	jb	short mfn_skip
	stosb
	loop	mfn_@
	; ecx = 0
	;xor	eax, eax
	;stosb
mfn_skip:
	pop	esi
	retn

convert_invalid_chars:
	; 27/05/2025
	; 26/05/2025
	;   upper case conversion counter is added
	;
	; 20/05/2025 - TRDOS 386 v2.0.10
	;
	; INPUT:
	;  al = character
	; OUTPUT:
	;  al = converted character
	;  ah = uppercase of AL input
	;  (invalid char will be converted to '_')
	;  (lowercase char will be converted to uppercase)
	;
	; Modified registers: EAX, EDX

	mov	ah, al

	cmp	al, 128
	jnb	short cic_3
	
	;cmp	al, 20h
	;jb	short cic_3

	;cmp	al, 'A'
	cmp	al, '@'
	jb	short cic_1
	cmp	al, 'Z'
	jna	short cic_3
	cmp	al, 'a'
	jb	short cic_1
	cmp	al, 'z'
	jna	short cic_4 ; convert to upper case
cic_1:
	push	edi
	push	ecx
	;mov	edi, invalid_fname_chars_@
 	;mov	ecx, sizeInvFnChars@
	; 27/05/2025
	mov	edi, invalid_fname_chars
 	mov	ecx, sizeInvFnChars
	repne	scasb
	pop	ecx
	pop	edi
	jnz	short cic_3 ; 27/05/2025
	; invalid char
cic_2:
	mov	al, '_'
cic_3:
	retn
cic_4:
	; simple ucase
	inc	byte [conv_ucase]
	and	al, 0DFh
	;and	al, 5Fh
	mov	ah, al
	retn

; 26/05/2025
; TRDOS 386 v2.0.10 - Long to short file name conversion
;
; REF:
; Microsoft - Long Filename Specification
; Version 0.5, December 4, 1992
;
; The following steps are used to form an 8.3 basis from a long name.
;
; 1. Remove all spaces. For example "My File" becomes "MyFile".
;
; 2. Initial periods, trailing periods, and extra periods prior to 
;    the last embedded period are removed.
;
;    For example ".logon" becomes "logon", "junk.c.o" becomes "junkc.o",
;    and "main." becomes "main".
;
; 3. Translate all illegal 8.3 characters into "_" (underscore).
;    
;    For example, "The[First]Folder" becomes "The_First_Folder".
;
; 4. If the name does not contain an extension then truncate it to 6 characters.
;    If the names does contain an extension, then truncate the first part 
;    to 6 characters and the extension to 3 characters.
;
;    For example, "I Am A Dingbat" becomes "IAmADi" and not "IAmADing.bat",
;    and "Super Duper Editor.Exe" becomes "SuperD.Exe".
;
; 5. If the name does not contain an extension then a "~1" is appended to
;    the name. If the name does contain an extension, then a "~1" is appended
;    to the first part of the name, prior to the extension.
;
;    For example, "MyFile" becomes "MyFile~1, and "Junk.bat" becomes "Junk~1.bat".
;
;    This numeric value is always added to help reduce the conflicts
;    in the 8.3 name space for automatic generated names.
;
; 6. This step is optional dependent on the name colliding with an existing file.
;    To resolve collisions the decimal number, that was set to 1, is incremented
;    until the name is unique. The number of characters needed to store the number
;    will grow as necessary from 1 digit to 2 digits and so on.
;    If the  length of the basis (ignoring the extension) plus the dash and number
;    exceeds 8 characters then the length of the basis is shortened until
;    the new name fits in 8 characters.
;
;    For example, if "FILENA~1.EXE" conflicts the next names tried are
;    "FILENA~2.EXE", "FILENA~3.EXE", ..., "FILEN~10.EXE", "FILEN~11.EXE", etc
;
; 26/05/2025 - Erdogan Tan
; Note: If the long name is the same as it's short name converted to lower case,
;	Windows 7 sets DIR_NTRes (directory entry) byte to 08h.
;       For example: The created file name "denemefn" is written into
;	the directory as the short name "DENEMEFN" without the long name entry,
;	but 'DIR_NTRes' byte (reserved and suggested -by microsoft-
;	to set it as zero) is set to 08h.)
; 	((DIR_NTRes byte is at offset 12 of a directory entry.))

convert_name_from_lfn:
	; 27/05/2025
	; 26/05/2025 - TRDOS 386 v2.0.10
	;
	; INPUT:
	;	ESI = long file name (asciiz format)
	; 	      (max. 128 chars)
	;	EDI = target file name address
	;
	; OUTPUT:
	;	EDI = target file name address of
	;	      dos 8.3 file name
	;	EAX = 0 if the name does not contain tilde
	;	    = tilde position (if > 0)
	;	if EAX = -1 -> Invalid file name error !
	;
	; Modified registers: EAX, EBX, ECX, EDX

	;mov	[f_base_start], edi
	mov	[f_target], edi
	xor	ebx, ebx
	mov	[f_base_count], ebx ; 0
	mov	[f_ext_start], ebx ; 0
	mov	[f_ext_count], ebx ; 0
	mov	[lossy_conversion], bl ; 0
	mov	[conv_ucase], bl ; 0

	push	edi ; *
	push	esi ; **

	; temporary name space on bss section
	mov	edi, temp_name ; max. 128 bytes + zero

	mov	ecx, 128

	;xor	ebx, ebx

	; remove spaces and dots at first
	; and save the last dot position
	; (if the last dot is not the end of file
	;  it will be inserted in same position)

	mov	edx, edi ; *
	;mov	[f_base_start], edi
conv_f_lfn_0:	; remove spaces & dots
	lodsb
	cmp	al, 20h
	je	short conv_f_lfn_2
	jb	short conv_f_lfn_3
	cmp	al, '.'
	jne	short conv_f_lfn_1
	mov	ebx, edi ; the last dot
	sub	ebx, edx
	jmp	short conv_f_lfn_2
conv_f_lfn_1:	; not_dot
	stosb
conv_f_lfn_2:
	loop	conv_f_lfn_0
conv_f_lfn_3:
	mov	esi, edx ; *
	mov	ecx, edi

	; zero tail
	;xor	eax, eax ; 0
	;stosb

	; ebx = the last dot position (index)

	sub	ecx, esi ; number of chars
	 	 ; (without spaces)
	jz	nul_name  ; invalid file name !

	mov	[f_base_count], ecx

	mov	edi, [f_target] ; (*)

	;; eax = 0

	and	ebx, ebx
	jz	short check_base ; ecx > 0 ; not dot

	cmp	ecx, ebx
	je	short skip_extension
		; the last char of the LFN is dot

	sub	ecx, ebx ; base count - dot position
	xchg	ecx, ebx
	mov	[f_ext_count], ebx
	mov	[f_base_count], ecx

	add	edx, ecx ; temp_name + dot pos
	mov	[f_ext_start], edx

	cmp	ebx, 3 ; extension length > 3
	jna	short check_base

	inc	byte [lossy_conversion]
	mov	byte [f_ext_count], 3
check_base:
	cmp	ecx, 8 ; basis length > 8
	jna	short proper_base

	inc	byte [lossy_conversion]

	mov	cl, 8
	mov	[f_base_count], cl

proper_base:
	rep	movsb	; copy/move basis
	mov	cl, [f_ext_count]
	jecxz	skip_extension
	mov	al, '.' ; insert DOT
	stosb
	mov	esi, [f_ext_start]
	rep	movsb
skip_extension:
	xor	al, al	; put zero/NUL at the end
	stosb

	; Translate all illegal 8.3 characters into "_".

	mov	edi, [f_target] ; (*)
	mov	esi, edi
conv_f_lfn_4:
	lodsb
	and	al, al
	jz	short conv_f_lfn_6
	cmp	al, '.'
	je	short conv_f_lfn_5
	dec	byte [conv_ucase] 
	call	convert_invalid_chars
	stosb
	cmp	ah, al
	je	short conv_f_lfn_4
	inc	byte [lossy_conversion]
	jmp	short conv_f_lfn_4

conv_f_lfn_5:
	inc	edi
	mov	[f_ext_start], edi
	jmp	short conv_f_lfn_4

conv_f_lfn_6:
	;stosb 	; NUL

	cmp	byte [lossy_conversion], 0
	jna	short conv_f_lfn_10 ; exact 8.3 name

	; mark for it is not a lower case 8.3 name 
	mov	byte [conv_ucase], -1

	; 27/05/2025
	; put '~1' at the end of the base name
	mov	ebx, [f_ext_start] ; '.ext' possible
	mov	edx, [ebx]
	mov	eax, 6 ; char 7 ('~') and 8 ('1')
	cmp	eax, [f_base_count]
	jna	short conv_f_lfn_7
	mov	eax, [f_base_count]
conv_f_lfn_7:
	mov	edi, [f_target] ; [f_base_start]
	add	edi, eax
	; default !
	mov	ax, '~1'
		; base name (<=6 bytes) + '~1'
	stosw
	cmp	byte [f_ext_count], 0
	jna	short conv_f_lfn_9
	mov	al, '.'
	stosb
	mov	cl, [f_ext_count]
conv_f_lfn_8:
	mov	al, dl
	stosb
	shr	edx, 8
	loop	conv_f_lfn_8
conv_f_lfn_9:
	sub	al, al ; 0
		; '.ext+'0 or '.ex'+ or '.e'+0
	stosb
conv_f_lfn_10:
	pop	esi ; **
	pop	edi ; *
	retn

	; 28/05/2025
	; 27/05/2025
	; 26/05/2025 - TRDOS 386 v2.0.10
change_tilde_number:
	; Input:
	;    eax = tilde/order number
	;	 = 0 for default ('~1')
	;    edi = file name address (12 bytes)
	; Output:
	;    file name will have '~n' at the
	;    end of the basis (base name)
	;
	; Modified registers: eax, ebx, ecx, edx

	; Note: In fact, if the filename
	; already contains '~n', the number n
	; after the tilde ('~n') should be increased;
	; decreasing may cause unnecessary loss
	; of filename characters
	; (if the number of digits is decreased).
	; (But there is no problem for a test only.)
	;
	; For normal use of this subroutine,
	; if the previous variant of the (translated)
	; short name already exists, the tilde number
	; will be incremented on subsequent calls.

	;mov	eax, [order_number]

	or	eax, eax
	jnz	short ctn_1 ; skip

	mov	al, 1	; default ('~1')
ctn_1:
	;cmp	eax, 65535
	;jna	short skip_num_limit
	;mov	eax, 65535
;skip_num_limit:

	push	esi ; +

	mov	esi, edi

	push	edi ; *

	mov	edi, order_num_str

	call	convert_num_to_str
	; ecx = cl = numeric string chars/digits
	;    	minimum: 1, maximum: 5 (for 65535)

	; find the dot position
	; or the end of the file name

	;mov	esi, [esp] ; *

	xor	ebx, ebx
	xor	edx, edx

	push	ecx ; **
	; ecx <= 5
	mov	cl, 6
ctn_gfnc:
	;mov	al, [esi]
	mov	eax, [esi]
	and	al, al
	jz	short ctn_eofn
	cmp	al, '.'
	je	short ctn_dotpos
	cmp	al, '~'
	jne	short ctn_2
	mov	ebx, esi ; previous tilde position
ctn_2:
	inc	esi
	loop	ctn_gfnc
	xor	eax, eax ; 0
	;jmp	short ctn_eofn
ctn_dotpos:
	mov	edx, eax ; save extension
ctn_eofn:
	pop	ecx ; **
	mov	edi, [esp] ; *
	add	edi, 8
	inc	ecx ; + tilde (2 to 6)
	sub	edi, ecx ; required tilde position

	mov	eax, edx

	or	ebx, ebx
	jz	short ctn_3 ; no previous tilde
	cmp	ebx, edi
	jnb	short ctn_3
	mov	edi, ebx  ; previous tilde
ctn_3:
	mov	esi, tilde_string
	; ecx = numeric digits + 1
	; edi = tilde position
	rep	movsb
	;mov	[edi], eax ; 0 or '.ext'
	; al = '.' or 0
	stosd
	xor	eax, eax
	stosb	; 0	
;ctn_4:
	;stosb
	;and	al, al
	;jz	short ctn_5
	;shr	eax, 8	; next .ext char
	;jmp	short ctn_4
;ctn_5:
	pop	edi ; *
	pop	esi ; +
	retn

	; 27/05/2025 - TRDOS 386 v2.0.10
increase_tilde_number:
	; Increase tilde number
	;
	; Input:
	;    edi = file name address (12 bytes)
	; Output:
	;    file name will have '~n+1' at the
	;    end of the basis (base name)
	;
	;    [order_number] = previous tilde num
	;
	;    cf = 1 -> failed
	;
	;   eax = previous tilde number (0-98)
	;
	; Modified registers: eax, ebx, ecx, edx

	; find the tilde position

	push	esi ; *
	push	edi ; **
	mov	esi, edi
	mov	ecx, 8
	xor	eax, eax
	xor 	ebx, ebx
itn_1:
	lodsb
	and	al, al
	jz	short itn_eofn
	cmp	al, '.'
	je	short itn_dotpos
	cmp	al, '~'
	jne	short itn_2
	mov	edi, esi
	dec	edi
itn_2:
	; check for numeric character
	cmp	byte [edi], '~'
	jne	short itn_3
	cmp	al, '0'
	jb	short itn_4 ; improper
	cmp	al, '9'
	ja	short itn_4 ; improper
	sub	al, '0'
	push	eax
	mov	al, 10
	mul	ebx ; [order_number]
	pop	ebx
	add	ebx, eax
	xor	eax, eax
itn_3:
	loop	itn_1
itn_eofn:
itn_dotpos:
	mov	eax, ebx
	mov	[order_number], ebx
	inc	eax
	pop	edi ; **
	pop	esi ; *
	jmp	ctn_1 ; change tilde number
itn_4:
	sub	ebx, ebx
	jmp	short itn_3

convert_num_to_str:
	; 27/05/2025
	; 26/05/2025 - TRDOS 386 v2.0.10
	;
	; INPUT:
	;  eax = number
	;  edi = numeric string address
	;
	;  (if ecx = 65535 -> string length is 5 bytes) 
	;
	; OUTPUT:
	;  ecx = numeric string chars/digits
	;
	;  (if ecx input = 65535 -> ecx = 5) 
	;
	; Modified registers: eax, ecx, edx

	push	ebp ; *
	push	edi ; **
	mov	ebp, esp
	mov	ecx, 10
cnvtnstr_next:
	xor	edx, edx
	div	ecx
	push	edx ; *#*
	and	eax, eax
	jnz	short cnvtnstr_next
	xor	ecx, ecx ; 0
cnvtnstr_@:
	pop	eax ; *#*
	add	al, '0' ; convert to numeric char
	stosb
	inc	ecx
	cmp	esp, ebp
	jb	short cnvtnstr_@
	;xor	al, al
	;stosb
	pop	edi ; **
	pop	ebp ; *	
	retn

; 27/05/2025
; 20/05/2025 - TRDOS 386 v2.0.10
; Ref: https://en.wikipedia.org/wiki/8.3_filename#Directory_table
;
; DOS filenames include the following:
;  UpperCase letters A-Z
;  Numbers 0-9
;  Space (20h)
;  !, #, $, %, &, ', (, ), -, @, ^, _, `, {, }, ~
;  Values 128–255
;
; This excludes the following ASCII characters:
;  ", *, +, ,, /, :, ;, <, =, >, ?, \, [, ], |
;  . (DOT) within name and extension fields,
;			 except in . and .. entries
;  Lowercase letters a–z, stored as A–Z on FAT12/FAT16/FAT32
;  Control characters 0–31
;  Value 127 (DEL)

; 27/05/2025
	; 20/05/2025
;invalid_fname_chars_@:
;	;db 20h ; SPACE
;	db 2Eh ; .

	; 20/05/2025
invalid_fname_chars:
	;   "   *   +   ,   /   :   ;   <   =   >   ?   \   [   ]   |
	db 22h,2Ah,2Bh,2Ch,2Fh,3Ah,3Bh,3Ch,3Dh,3Eh,3Fh,5Ch,5Bh,5Dh,7Ch

sizeInvFnChars  equ ($ - invalid_fname_chars)

;sizeInvFnChars@  equ ($ - invalid_fname_chars_@) ; 20/05/2025

version:
	db 0Dh, 0Ah
	db 'Test Program for ASCIIZ long file name to DOS short name conversion'
	db 0Dh, 0Ah
	db 'by Erdogan Tan - 28/05/2025'
	db 0Dh, 0Ah, 0
usage:
	db 0Dh, 0Ah
	db 'Usage: lfnconv <test file name, max. 128 bytes>'
nexline:
	db 0Dh, 0Ah, 0

msg_invalid_fn:
	db 0Dh, 0Ah
	db 'Invalid file name !'
	db 0Dh, 0Ah, 0

order_number:
	dd 1

tilde_string:
	db '~'
order_num_str: ; max. 5 bytes + NUL
	db '1'
	dd 0 ; 4 bytes
	db 0

lfn_name:
	times 128 db 0
	db 0

special_status:
	db ' ... ([DIR_NTRes]=08h)'
	db 0

bss:

ABSOLUTE bss

;f_base_start:	resd 1
f_target:	resd 1
f_base_count:	resd 1
f_ext_start:	resd 1
f_ext_count:	resd 1
;f_name_count:	resd 1
formal_size:	resd 1
lossy_conversion:
		resb 1
target_name:	resb 13
temp_name:	resb 129
conv_ucase:	resb 1