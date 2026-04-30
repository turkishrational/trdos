; ****************************************************************************
; dirlfn2.s (TRDOS 386, TRDOS v2.0.11 - sample binary file, 'dirlfn2.prg')
; ----------------------------------------------------------------------------
; DIRLFN2.PRG ! 'sysfff' and 'sysfnf' TEST program for TRDOS 386 !
;
; 30/04/2026
;
; [ Last Modification: 30/04/2026 ]
;
; ****************************************************************************
; ref: 'dirlist.s' & 'dirlfn.s' (30/04/2026) 

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

DirEntry_Name equ 0
DirEntry_Attr equ 11
DirEntry_NTRes equ 12
DirEntry_CrtTimeTenth equ 13
DirEntry_CrtTime equ 14
DirEntry_CrtDate equ 16
DirEntry_LastAccDate equ 18
DirEntry_FstClusHI equ 20
DirEntry_WrtTime equ 22
DirEntry_WrtDate equ 24
DirEntry_FstClusLO equ 26
DirEntry_FileSize equ 28

; TRDOS 386 (and Retro UNIX 386 v1) system call format:
; sys systemcall (eax) <arg1 (ebx)>, <arg2 (ecx)>, <arg3 (edx)>

; sysFFF (Find First File), ch = 2 Buffer:
; buffer offset 0-31: 32 byte msdos/fat directory entry
; buffer offset 32-163: 132 byte Unicode Long Name buffer (v2.0.11)
;		      : 260 byte Unicode Long Name buffer (v2.1.0)

; sysFFF (Find First File), ch = 3 Buffer:
; buffer offset 0-31: 32 byte msdos/fat directory entry
; buffer offset 32: Long File Name length (0 = short name only)	
; buffer offset 33-163: 66 byte Asciiz Long Name buffer (v2.0.11)
;		      : 131 byte Asciiz Long Name buffer (v2.1.0)

[BITS 32] ; 32-bit intructions

[ORG 0] 

START_CODE:
	mov	esi, esp
	lodsd
	cmp	eax, 2 ; two arguments (program file name & text file name)
	jb	short terminate ; nothing top do
	lodsd ; program file name address 
	lodsd ; text file name address
	; EAX = arg2 ; file name address

	; EBX = EAX = file name or path address
	; CL = file attributes (archive = 20h, read only = 01h)
	;	(21h = Archived & Read Only files are included)
	; CH = 0 = basic parameters (24 bytes)
	; EDX = DTA = buffer address (24 bytes for basic parameters)
	; EAX = _fff = 'Find First File' system call for TRDOS 386

	push	eax

	mov	esi, msg_program
	call	print_msg

	xor	ah, ah
	int	32h

	pop	eax

	; Find First File
	sys	_fff, eax, 0337h, DTA
_0:
	;jc	short terminate	 ; terminate if there is 
	jc	short dirlist_ok ; file not found error or another error
_@@:
	mov	ah, 1
	int	32h
	jz	short _@
	mov	ah, 0
	int	32h
	jmp	short _@@
_@:
	call	print_dir_entry

	dec	byte [RowCounter]
	jnz	short direntry_p_ok

pause_dir_scroll:
	sub	ah, ah
	int	32h
	cmp	al, 1Bh
	je	short dirlist_ok
	mov	byte [RowCounter], 20 ; Reset counter
direntry_p_ok:
_1:
	; Find Next File
	sys	_fnf	; if the first file is not proper file
			; check for next file
	jmp	short _0

dirlist_ok:
terminate:
	mov	esi, nextline
	;mov	ebx, 07h	; Light Gray	
	;call	print_msg_@
	call	print_msg

	sys 	_exit		; INT 40h
here:
	jmp	short here

_skip:
_print_fn_ok:
	retn

print_dir_entry:
	;test	[DTA+DirEntry_Attr], 08h
	;jnz	short _skip

	mov	esi, nextline
	call	print_msg

	mov	bl, [DTA+DirEntry_Attr]
	
	mov	esi, file
	test	bl, 10h
	jz	short _not_dir

	mov	esi, directory
_not_dir:
	call	print_msg

	call	get_file_name

	dec	edi
	mov	ecx, short_name+12
	sub	ecx, edi
	mov	al, 20h
	rep	stosb
	mov	al, 0
	stosb

	mov	esi, short_name
	call	print_msg

	mov	byte [tire+1], '-'

	cmp	byte [lfn_l], 0
	jna	_no_lfn
	
	mov	byte [tire+1], '+' 

_no_lfn: 
	mov	esi, tire
	call	print_msg

	mov	esi, LFN
	;call	print_msg
	;retn
print_msg:
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

	; ref: 'trdosk8.s', "get_file_name", 29/04/2026
get_file_name:
	mov	esi, DTA
	mov	edi, short_name

	mov	ecx, 8
gfn_basename:
	lodsb
	cmp	al, 20h
	ja	short gfn_nextchar
	add	esi, ecx
	dec	esi
	jmp	short gfn_chk_ext
gfn_nextchar:
	stosb		; ?*
	loop	gfn_basename
gfn_chk_ext:
	cmp	byte [esi], 20h
	jna	short gfn_ok
	mov	al, '.' ; ?*.
	stosb
	movsb		; .?
	lodsb
	cmp	al, 20h
	jna	short gfn_ok
	stosb		; .??
	lodsb
	cmp	al, 20h
	jna	short gfn_ok
	stosb		; .???
gfn_ok:
	xor	al, al
	stosb
	retn

;-----------------------------------------------------------------
;  messages
;-----------------------------------------------------------------

msg_program:
	db 0Dh, 0Ah
	db "DIRLFN2.PRG /// TRDOS 386 sysfff, sysfnf test program"
	db 0Dh, 0Ah
	db "by Erdogan Tan, 30/04/2026", 0Dh, 0Ah, 0
nextline:
	db 0Dh, 0Ah, 0

;-----------------------------------------------------------------

directory:	db '<D> ',0
file:		db '<F> ',0

tire:		db ' ? ',0

RowCounter:	db 16

;-----------------------------------------------------------------
;  uninitialized data
;-----------------------------------------------------------------

bss:

ABSOLUTE bss

alignb 4

DTA:	resb 32	 ; Directory Entry buffer
lfn_l:  resb 1	 ; Long Name length 
LFN:	resb 131 ; Long File Name buffer
	resd 4
short_name: resb 13

