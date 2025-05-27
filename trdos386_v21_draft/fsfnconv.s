; ****************************************************************************
; fsfnconv.s (TRDOS 386 v2.1 test - singlix fs filename to shortname convert)
; ----------------------------------------------------------------------------
; FSFNCONV.PRG ! TEST program !
;
; 23/05/2024
;
; Derived from 'args386.s' source code for TRDOS 386 v2.0 (11/05/2016)
;
; [ Last Modification: 27/05/2025 ]
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
	cmp	eax, 3	; number of arguments
	jb	short show_usage
	mov	ebp, eax
	lodsd	; skip program file name
	dec	ebp
nextarg:
	lodsd	; number address
	;call	convert_to_formal_str
	call	convert_to_fdt_number

	mov	edi, fs_name ; fs (long) file name
	mov	ecx, 64	; file name limit
	dec	ebp
next_word:
	lodsd
	call	move_file_name
	jcxz	put_zero
	dec	ebp
	jnz	short next_word
put_zero:
	xor	eax, eax
	stosb

	mov	esi, fs_name
	mov	edx, 12 ; max. 12 bytes
	mov	edi, target_name
	mov	eax, [fdt_number]

	call	convert_name_from_trfs

	call	print_short_name
terminate: 
	sys	_exit
halt:
	jmp	short halt

show_usage:
	sys	_msg, usage, 255, 0Fh
	jmp	short terminate

print_short_name:
	sys	_msg, nexline, 2, 07h
	sys	_msg, edi, 255, 0Fh
	sys	_msg, nexline, 2, 07h
	retn

; 23/05/2025
%if 0
convert_to_formal_str:
	; eax = FDT (or DDT) number text
	push	esi
	mov	ecx, 12
	mov	esi, eax
	mov	edi, formal_number
ctfstr_@:
	lodsb
	cmp	al, '9'
	ja	short ctfstr_ok
	cmp	al, '0'
	jb	short ctfstr_ok
	stosb
	loop	ctfstr_@
ctfstr_ok:
	mov	al, ']'
	stosb
	sub	eax, eax
	stosb
	pop	esi
	retn
%endif

convert_to_fdt_number:
	; eax = FDT (or DDT) number text
	push	esi
	mov	ecx, 12
	mov	esi, eax
	xor	eax, eax ; 0
	mov	[fdt_number], eax
cfdtnum_1:
	lodsb
	cmp	al, '9'
	ja	short cfdtnum_2
	cmp	al, '0'
	jb	short cfdtnum_2
	sub	al, '0'
	mov	ebx, eax
	mov	al, 10
	mul	dword [fdt_number]
	or	edx, edx
	;jnz	short cfdtnum_2
	jnz	short cfdtnum_3
	mov	[fdt_number], eax
	add	ebx, [fdt_number]
	;jc	short cfdtnum_2
	jc	short cfdtnum_3
	mov	[fdt_number], ebx
	xor	eax, eax
	loop	cfdtnum_1
cfdtnum_2:
	pop	esi
	retn
cfdtnum_3:
	mov	dword [fdt_number], 0FFFFFFFFh
	jmp	short cfdtnum_2

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
	; Modified registers: EAX

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
	mov	edi, invalid_fname_chars_@
 	mov	ecx, sizeInvFnChars@
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
	and	al, 0DFh
	;and	al, 5Fh
	mov	ah, al
	retn

convert_name_from_trfs:
	; 25/05/2025
	; 24/05/2025
	; 23/05/2025
	; 21/05/2025
	; 20/05/2025
	;
	; INPUT:
	;	ESI = long file name (singlix fs, asciiz format)
	; 	      (max. 64 chars)
	;	EDX = (max.) number of the target format chars
	;	      (space: min. 12 chars + trailing NUL)
	;	EDI = target file name address
	;
	; OUTPUT:
	;	ECX = length of the asciiz file name
	;		 (except NUL tail)
	;	EDI = target file name address
	;	EAX = 0 if the name does not contain FDT number
	;	    = FDT number (if > 0)
	;
	; Modified registers: EAX, EBX, ECX

	; 21/05/2025
	mov	[fdt_number], eax
	mov	[f_name_limit], edx
	;mov	[f_base_start], edi
	mov	[f_target], edi
	xor	ebx, ebx
	mov	[f_base_count], ebx ; 0
	mov	[f_ext_start], ebx ; 0
	mov	[f_ext_count], ebx ; 0
	; 24/05/2025
	;mov	[f_name_count], ebx ; 0
	mov	[formal_size], ebx ; 0
	mov	[insert_fdtnum], bl ; 0

	; temporary name space on bss section
	; 25/05/2025
	mov	edi, temp_name ; max. 64 bytes + zero

	mov	ecx, 64

	;xor	ebx, ebx

	; remove spaces at first
	; and save the last dot position

	push	edi ; *
	;mov	[f_base_start], edi
conv_f_fs_0:	; remove_spaces
	lodsb
	cmp	al, 20h
	je	short conv_f_fs_2 ; rms_next
	jb	short conv_f_fs_3 ; end_rm_space
	cmp	al, '.'
	jne	short conv_f_fs_1 ; not_dot
	mov	ebx, edi ; the last dot
conv_f_fs_1:	; not_dot
	stosb
conv_f_fs_2:	; rms_next
	loop	conv_f_fs_0 ; remove spaces
conv_f_fs_3:	; end_rm_space
	pop	esi ; *
	mov	ecx, edi

	; zero tail
	xor	al, al ; 0
	stosb

	; ebx = the last dot position

	; 24/05/2025
	sub	ecx, esi ; number of chars
	 	 ; (without spaces)
	jz	nul_name

	; 24/05/2025
	;mov	[f_name_count], ecx
	mov	[f_base_count], ecx

	; 25/05/2025
	mov	edi, [f_target] ; (*)

	and	ebx, ebx
	jz	conv_f_fs_6 ; ecx > 0 ; not dot

	;sub	ebx, esi ; chars before the last DOT
	;jz	dot_first ; name starts with '.'

	; 24/05/2025
	cmp	byte [esi], '.'
	je	dot_first ; name starts with '.'

	mov	eax, ebx ; the last DOT position
	inc	eax

	; check the 1st char after the dot
	cmp	byte [eax], 0
	jna	dot_last ; name ends with '.' !

	sub	ebx, esi
	mov	[f_base_count], bl

	mov	[f_ext_start], eax

	sub	ecx, ebx ; > 1
		; total name size - base name size
	dec	ecx ; except '.'

	cmp	cl, 4	; .ext size limit is 4 (.html)
	jna	short conv_f_fs_4
	inc	byte [insert_fdtnum] ; not exact name
	mov	cl, 4
conv_f_fs_4:
	mov	[f_ext_count], ecx
	mov	eax, edx ; = [f_name_limit] ; >= 12
	sub	eax, ecx ; ecx <= 4
	dec	eax ; '.'
	cmp	eax, [f_base_count]
	jnb	short conv_f_fs_7 ; proper
	; (for example: [f_base_count] > 7)
	inc	byte [insert_fdtnum] ; not exact name
	cmp	cl, 3
	jna	short conv_f_fs_5
	dec	ecx
	jmp	short conv_f_fs_4

	; 24/05/2025
check_fn_limit:
	cmp	[f_base_count], edx ; limit (minimum 12)
	jna	short cfnl_ok
	inc	byte [insert_fdtnum]
	mov	[f_base_count], edx
cfnl_ok:
	retn

conv_f_fs_5:
	; minimum 1 byte base name is needed
	dec	byte [f_base_count]
	jnz	short conv_f_fs_4 ; ok

	; 24/05/2025
	jmp	short conv_f_fs_10

	; 23/05/2025
dot_first:
	; 25/05/2025
	;mov	edi, [f_target] ; (*)
	movsb	; skip '.'
	inc	byte [insert_fdtnum]
	call	check_fn_limit
	;dec	ecx
	;jmp	short conv_f_fs_8
	jmp	short conv_f_fs_9

	; 23/05/2025
dot_last:
	mov	byte [ebx], 0 ; clear the last '.'
	; 24/05/2025
	;mov	ecx, [f_base_count]
	;dec	ecx ; without DOT
	;mov	[f_base_count], ecx
	dec	dword [f_base_count]
	inc	byte [insert_fdtnum]

conv_f_fs_6:	; not dot
	; esi = [f_target] = [f_base_start]
	;;ecx = [f_base_count] = [f_name_count]
	; edx = [f_name_limit]

	; check file name limit and
	; change/descrease if it is required
	call	check_fn_limit

conv_f_fs_7:
	; 25/05/2025
	;mov	edi, [f_target] ; (*)
	mov	ecx, [f_base_count]
conv_f_fs_8:
	lodsb
	call	convert_invalid_chars
	stosb
	cmp	ah, al
	je	short conv_f_fs_9
	inc	byte [insert_fdtnum]
conv_f_fs_9:
	loop	conv_f_fs_8

conv_f_fs_10:
	mov	ecx, [f_ext_count]
	jecxz	conv_f_fs_13

	mov	al, '.'
	stosb

	mov	esi, [f_ext_start]
	mov	[f_ext_start], edi
conv_f_fs_11:
	lodsb
	call	convert_invalid_chars
	stosb
	cmp	ah, al
	je	short conv_f_fs_12
	inc	byte [insert_fdtnum]
conv_f_fs_12:
	loop	conv_f_fs_11

conv_f_fs_13:
	mov	byte [edi], 0

	cmp	byte [insert_fdtnum], 0
	jna	conv_f_fs_ok

nul_name: ; 24/05/2025 ; [f_base_count] = 0

	mov	eax, [fdt_number]

	mov	edi, formal_string ; space = 13 bytes

	; "[numbertext]" = fdt number which will inserted
	;		between '[' and ']'

	call	convert_num_to_formalstr

	; ecx = formal string length ("[....]" char count)
	mov	[formal_size], ecx

	;mov	esi, formal_string
	mov	esi, edi ; semi-raw short name address
	mov	edi, [f_target] ; = [f_base_start]

	; 25/05/2025
conv_f_fs_14:
	; 24/05/2025
	mov	edx, [f_name_limit] ; max. length
	mov	eax, [f_ext_count]
	or	eax, eax
	jz	short conv_f_fs_15
	sub	edx, eax  ; - extension length
	dec	edx ; except DOT
	;jz	short use_only_formal_str ; fdt only
conv_f_fs_15:
	; 25/05/2025
	sub	edx, ecx
	ja	short conv_f_fs_16 ; edx > 0
		 ; min. 1 base name char at the beginning

	; there is not base name space for a base name char
	; (check name ext. and decrease it to 3 if it is 4)
	; ((start with a base name char would be better))

	cmp	al, 4 ; [f_ext_count]
	jb	short conv_f_fs_18 ; edx <= 0

	dec	dword [f_ext_count] ; 4 -> 3

	; put zero after the 3th extension char
	; (the nul was after the 4th extension char)
	mov	edx, [f_ext_start]
	mov	byte [edx+3], 0

	jmp	short conv_f_fs_14 ; check again

conv_f_fs_16:
	; edx = number of base name chars
	;	before formal string (*)

	mov	eax, [f_base_count]

	;and	eax, eax
	;jz	short insert_formal_str

	cmp	edx, eax
	jna	short conv_f_fs_17
	mov	edx, eax
conv_f_fs_17:
	add	edi, edx

insert_formal_str:
	; 24/05/2025
	; edi = [f_target] + edx (*)
	; esi = formal_string address
	; ecx = [formal_size] ; string length
	mov	ebx, [f_ext_start] ; extension start
	; ebx = the 1st byte addr after the last DOT
	or	ebx, ebx
	jz	short add_formal_str ; not a name.ext

	xor	edx, edx ; 0
ins_formal_1:
	mov	al, [ebx]
	push	eax
	and	al, al
	jz	short ins_formal_2 ; end of the name
	inc	ebx
	inc	edx
	jmp	short ins_formal_1

ins_formal_2:
	; place [#] formal string to in the name
	rep	movsb
	mov	al, '.' ; and the DOT
	stosb
	mov	ebx, edi
	add	ebx, edx
ins_formal_3:
	; add the extension (and a nul at the end)
	; (by moving backward, from end to start)
	pop	eax
	mov	[ebx], al
	cmp	ebx, edi
	jna	short ins_formal_4 ; ok.
	dec	ebx
	jmp	short ins_formal_3

conv_f_fs_18:
	; 25/05/2025
	; edx <= 0  (from sub edx, ecx)
	or	edx, edx
	jz	short insert_formal_str
	; edx < 0
	;jmp	short use_only_formal_str

	; 24/05/2025
add_formal_str:
	; edi = [f_target] + edx (*)
use_only_formal_str:
	; 21/05/2025
	; edi = [f_target] = [f_base_start]
	; esi = formal_string address
 	; ecx = [formal_size]
	rep	movsb
	xor	al, al
	stosb
ins_formal_4:
	mov	eax, [fdt_number] ; return value
conv_f_fs_ok:
	; 23/05/2025
	mov	edi, [f_target]
	retn

convert_num_to_formalstr:
	; 20/05/2025 - TRDOS 386 v2.0.10
	; Singlix FS short name specific procedure
	;
	; INPUT:
	;  eax = number (FDT/DDT number)
	;  edi = [#], formal string address
	;	 has minimum 12 bytes size/space
	; OUTPUT:
	;  [.......] = max. 10 digits between '[' & ']'
	;  ecx = formal string size including '[' & ']'

	;
	; Modified registers: eax, ecx, edx

	push	ebp ; *
	push	edi ; **
	mov	byte [edi],'['
	inc	edi
	mov	ebp, esp
	mov	ecx, 10
cntfs_next:
	xor	edx, edx
	div	ecx
	push	edx ; *#*
	and	eax, eax
	jnz	short cntfs_next

	mov	cl, 2 ; '[' & ']'
cntfs_@:
	pop	eax ; *#*
	add	al, '0' ; convert to numeric char
	stosb
	inc	ecx
	cmp	esp, ebp
	jb	short cntfs_@
	mov	al, ']'
	stosb
	;xor	al, al
	;stosb
	pop	edi ; **
	pop	ebp ; *	
	retn


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

	; 20/05/2025
invalid_fname_chars_@:
	;db 20h ; SPACE
	db 2Eh ; .
	; 20/05/2025
invalid_fname_chars:
	;   "   *   +   ,   /   :   ;   <   =   >   ?   \   [   ]   |
	db 22h,2Ah,2Bh,2Ch,2Fh,3Ah,3Bh,3Ch,3Dh,3Eh,3Fh,5Ch,5Bh,5Dh,7Ch

sizeInvFnChars  equ ($ - invalid_fname_chars)

sizeInvFnChars@  equ ($ - invalid_fname_chars_@) ; 20/05/2025


version:
	db 0Dh, 0Ah
	db 'Test Program for Singlix FS file name to TRDOS 386 short name conversion'
	db 0Dh, 0Ah
	db 'by Erdogan Tan - 27/05/2025'
	db 0Dh, 0Ah, 0
usage:
	db 0Dh, 0Ah
	db 'Usage: fsfnconv <decimal fdt number> <test file name, max. 64 bytes>'
nexline:
	db 0Dh, 0Ah, 0

formal_string:
	db '['
formal_number:
	times 12 db 0
	db ']'
	db 0

fs_name:
	times 64 db 0
	db 0

bss:

ABSOLUTE bss

fdt_number:	resd 1
f_name_limit:	resd 1
;f_base_start:	resd 1
f_target:	resd 1
f_base_count:	resd 1
f_ext_start:	resd 1
f_ext_count:	resd 1
;f_name_count:	resd 1
formal_size:	resd 1
insert_fdtnum:	resb 1
target_name:	resb 13
temp_name:	resb 65