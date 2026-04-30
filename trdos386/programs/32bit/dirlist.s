; ****************************************************************************
; dirlist.s (TRDOS 386, TRDOS v2.0.11 - sample binary file, 'dirlist.prg')
; ----------------------------------------------------------------------------
; DIRLIST.PRG ! 'sysfff' and 'sysfnf' TEST program for TRDOS 386 !
;
; 30/04/2026
;
; [ Last Modification: 30/04/2026 ]
;
; ****************************************************************************
; ref: 'flist.s' (17/10/2026) 

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

	sub	eax, eax

	;mov	byte [RowCounter], 16

	;mov 	[File_Count], eax ; 0
	;mov	[Dir_Count], eax ; 0
	;mov 	[Total_FSize], eax ; 0
	mov	edi, File_Count
	stosd
	stosd
	stosd

	pop	eax

	; Find First File
	sys	_fff, eax, 013Fh, DTA
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
	mov	esi, nextline
	call	print_msg

	call	dir_summary
terminate:
	mov	esi, nextline
	;mov	ebx, 07h	; Light Gray	
	;call	print_msg_@
	call	print_msg

	sys 	_exit		; INT 40h
here:
	jmp	short here

	; ref: 'trdosk3.s', "print_directory:", 27/07/2022
print_dir_entry:
	mov	esi, DTA

	mov	eax, [esi+DirEntry_FileSize]

	mov	bl, [esi+DirEntry_Attr]

	test	bl, 10h  ; Is it a directory?
	jz	short not_dir

	inc	dword [Dir_Count]
	mov	edx, esi
 	mov	esi, Type_Dir	; '<DIR>     '
	mov	edi, Dir_Or_FileSize
	; move 10 bytes
	movsd
	movsd
	movsw
	mov	esi, edx
	jmp     short dir_attribute

not_dir:
	cmp	bl, 08h	; volume name
	jne	short direntry_file
	mov	esi, Type_Volume
	mov	edi, Dir_Or_FileSize
	; move 10 bytes
	movsd
	movsd
	movsw
	mov	esi, DTA
	mov	dword [File_Attribute], 20202020h
	jmp     short dir_file_name

direntry_file:
	inc	dword [File_Count]
	add	[Total_FSize], eax

	mov	ecx, 10  ; 32 bit divisor
	mov	edi, ecx
	add	edi, Dir_Or_FileSize
dir_rdivide:
	sub	edx, edx
	div	ecx 	 ; remainder in dl (< 10)
	add     dl, '0'	 ; to make visible (ascii)
	dec	edi
	mov     [edi], dl
	and	eax, eax
	jnz	short dir_rdivide

dir_fill_space:
	cmp     edi, Dir_Or_FileSize
	jna     short dir_attribute
	dec     edi
	mov     byte [edi], 20h
	jmp     short dir_fill_space

dir_attribute:
	mov	dword [File_Attribute], 20202020h

	cmp	bl, 20h  ; Is it an archive file?
	jb	short dir_file_attribs
	mov	byte [File_Attribute+3], 'A'

dir_file_attribs:
	and	bl, 7
	jz	short dir_file_name
	mov	bh, bl
	and	bl, 3
	cmp	bh, bl
	jna	short dir_pass_s
	mov	byte [File_Attribute], 'S'

dir_pass_s:
	and     bl,2
	jz      short dir_pass_h
	mov     byte [File_Attribute+1], 'H'
dir_pass_h:
	and     bh,1
	jz      short dir_file_name
	mov     byte [File_Attribute+2], 'R'
dir_file_name:
	;mov	bx, [esi+18h]	; Date
	;mov	dx, [esi+16h]	; Time
	mov	ebx, [esi+16h]
	mov	ecx, esi	; Directory Entry address
	mov     edi, File_Name
	; move 8 bytes
	movsd
	movsd
	mov	byte [edi], 20h
	inc	edi
	; move 3 bytes
	movsw
	movsb
	mov	esi, ecx

Dir_Time_start:
	;mov	ax, dx		; Time
	mov	ax, bx
	shr	ax, 5		; shift right 5 times
	and	ax, 0000111111b	; Minute Mask
	aam			; Q([AL]/10)->AH
				; R([AL]/10)->AL
				; [AL]+[AH]= Minute as BCD
	or	ax, '00'	; Convert to ASCII
	xchg	ah, al
	mov	[File_Minute], ax

	;mov	al, dh
	mov	al, bh
	shr	al, 3		; shift right 3 times
	aam			; [AL]+[AH]= Hours as BCD
	or	ax, '00'
	xchg	ah, al
	mov     [File_Hour], ax

	shr	ebx, 16		; BX = Date

Dir_Date_start:
	;mov	ax, bx		; Date
	mov	eax, ebx
	and	ax, 00011111b	; Day Mask
	aam			; Q([AL]/10)->AH
				; R([AL]/10)->AL
				; [AL]+[AH]= Day as BCD
	or	ax, '00'	; Convert to ASCII
	xchg	al, ah

	mov	[File_Day], ax

	;mov	ax, bx
	mov	eax, ebx
	;shr	ax, 5		; shift right 5 times
	shr	eax, 5
	and	ax, 00001111b	; Month Mask
	aam
	or	ax, '00'
	xchg	ah, al
	mov	[File_Month], ax

	;mov	ax, bx
	mov	eax, ebx
	;shr	ax, 9
	shr	eax, 9
	and	ax, 01111111b	; Result = Year - 1980
	add	ax, 1980

	mov	cl, 10
	div	cl		; Q -> AL, R -> AH 
	or	ah, '0'
	mov	[File_Year+3], ah
	aam
	xchg	ah, al
	or	ah, '0'	  ; Convert to ASCII
	mov	[File_Year+2], ah
	aam
	xchg	al, ah
	or	ax, '00'
	mov	[File_Year], ax

loc_show_line:
	mov     esi, File_Name
	call	print_msg
	mov	esi, nextline
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

dir_summary:
	mov     ecx, 10
	mov	eax, [Dir_Count]
	mov	edi, Decimal_Dir_Count
	cmp	eax, ecx
	jb	short pass_ddc
	inc	edi
	cmp	eax, 100
	jb	short pass_ddc
	inc	edi
	cmp	eax, 1000
	jb	short pass_ddc
	inc	edi
	cmp	eax, 10000
	jb	short pass_ddc
	inc	edi
pass_ddc:
	mov     [edi+1], ch ; 0
ddc_rediv:
	xor     edx, edx
	div	ecx	; 10
	add     dl, '0'
	mov     [edi], dl
	dec     edi
	or	eax, eax
	jnz	short ddc_rediv

	mov     eax, [File_Count]
	mov     edi, Decimal_File_Count
	cmp	eax, ecx ; 10
	jb      short pass_dfc
	inc     edi
	cmp     eax, 100
	jb      short pass_dfc
	inc     edi
	cmp     eax, 1000
	jb      short pass_dfc
	inc     edi
	cmp     eax, 10000
	jb      short pass_dfc
	inc     edi
pass_dfc:
	mov     [edi+1], ch ; 0
loc_dfc_rediv:
	xor	edx, edx
	div	ecx
	add	dl, '0'
	mov	[edi], dl
	dec	edi
	or	eax, eax
	jnz	short loc_dfc_rediv

	mov     edi, TFS_Dec_End
	mov     eax, [Total_FSize]
	;mov    ecx, 10
rediv_tfs_hex:
	sub	edx, edx
	;sub	dl, dl
	div	ecx
	add	dl, '0'
	dec     edi
	mov     [edi], dl
	and	eax, eax
	jnz	short rediv_tfs_hex

	mov	[TFS_Dec_Begin], edi
	mov	esi, Decimal_File_Count_Header
	call	print_msg
	mov	esi, str_files
	call	print_msg
	mov	esi, str_dirs
	call	print_msg
	mov	esi, [TFS_Dec_Begin]
	call	print_msg
	mov	esi, str_bytes
	jmp	print_msg

;-----------------------------------------------------------------
;  messages
;-----------------------------------------------------------------

msg_program:
	db 0Dh, 0Ah
	db "DIRLIST.PRG /// TRDOS 386 sysfff, sysfnf test program"
	db 0Dh, 0Ah
	db "by Erdogan Tan, 30/04/2026", 0Dh, 0Ah, 0Dh, 0Ah, 0
nextline:
	db 0Dh, 0Ah, 0

;-----------------------------------------------------------------

Type_Volume:    db '<VOLUME>  ' ; 10 bytes
Type_Dir:       db '<DIR>     ' ; 10 bytes

File_Name:
                times 12 db 20h
		db 20h
Dir_Or_FileSize:
                times 10 db 20h
		db 20h
File_Attribute:
		dd 20202020h
		db 20h
File_Day:
                db '0','0'
		db '/'
File_Month:
                db '0','0'
		db '/'
File_Year:
                db '0','0','0','0'
		db 20h
File_Hour:
                db '0','0'
		db ':'
File_Minute:
                db '0','0'
		db 0

Decimal_File_Count_Header:
		db 0Dh, 0Ah
Decimal_File_Count:
		times 6 db 0

str_files:	db " file(s) & "
Decimal_Dir_Count: 
		times 6 db 0
str_dirs:
		db " directory(s) "
		db 0Dh, 0Ah, 0

str_bytes:	db " byte(s) in file(s)"
		db 0Dh, 0Ah, 0

RowCounter:	db 16

;-----------------------------------------------------------------
;  uninitialized data
;-----------------------------------------------------------------

bss:

ABSOLUTE bss

alignb 4

DTA:	resb 32	; Directory Entry buffer

File_Count:	resd 1
Dir_Count:	resd 1
Total_FSize:	resd 1
TFS_Dec_Begin:	resd 1
                resb 10
TFS_Dec_End:	resb 1
		resb 1
