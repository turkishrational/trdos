; ****************************************************************************
; TRDOS386.ASM (TRDOS 386 Kernel - v2.0.10) - Directory Functions : trdosk4.s
; ----------------------------------------------------------------------------
; Last Update: 04/06/2025 (Previous: 03/09/2024, v2.0.9)
; ----------------------------------------------------------------------------
; Beginning: 24/01/2016
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.15 (trdos386.s)
; ----------------------------------------------------------------------------
; Derived from TRDOS Operating System v1.0 (8086) source code by Erdogan Tan
; DIR.ASM (09/10/2011)
; ****************************************************************************

; DIR.ASM  [ TRDOS KERNEL - COMMAND EXECUTER SECTION - DIRECTORY FUNCTIONS ]
; (c) 2004-2010  Erdogan TAN  [ 17/01/2004 ]  Last Update: 09/10/2011
; FILE.ASM [ FILE FUNCTIONS ] Last Update: 09/10/2011

change_prompt_dir_string:
	; 05/10/2016
	; 24/01/2016 (TRDOS 386 = TRDOS v2.0)
	; 27/03/2011
	; 09/10/2009
	; INPUT/OUTPUT => none
	; this procedure changes current directory string/text
	; 2005

	mov	esi, PATH_Array
change_prompt_dir_str: ; 05/10/2016 (call from 'set_working_path')
	mov	edi, Current_Directory
	mov	ah, [Current_Dir_Level]
	call	set_current_directory_string
	mov	[Current_Dir_StrLen], cl

	retn

set_current_directory_string:
	; 16/05/2025 (TRDOS 386 Kernel v2.0.10)
	; 11/08/2022 (TRDOS 386 Kernel v2.0.5)
	; 24/01/2016 (TRDOS 386 = TRDOS v2.0)
	; 27/03/2011
	; 09/10/2009
	; INPUT:
	;    ESI = Path Array Address
	;    EDI = Current Directory String Buffer
	;    AH = Current Directory Level
	; OUTPUT => EAX, EBX, ESI will be changed
	;    EDI will be same with input
	;    ECX = Current Directory String Length

	push    edi
	cmp     ah, 0
	jna	short pass_write_path
	; 16/05/2025
	; (8 sub dir levels)
	;add	esi, 16
	mov	ebx, esi
loc_write_path:
	;mov	ecx, 8
	; 11/08/2022
	sub	ecx, ecx
	mov	cl, 8
path_write_dirname1:
	lodsb
	cmp	al, 20h
	jna	short pass_write_dirname1
	stosb
	cmp	edi, End_Of_Current_Dir_Str
	jnb	short pass_write_path
	loop	path_write_dirname1
	cmp	byte [esi], 20h
	jna	short pass_write_dirname2
	jmp     short loc_put_dot_cont_ext
pass_write_dirname1:
	mov	esi, ebx
	add	esi, 8
	cmp	byte [esi], 20h
	jna	short pass_write_dirname2
loc_put_dot_cont_ext:
	mov	byte [edi], "."
	;mov	ecx, 3
	mov	cl, 3
loc_check_dir_name_ext:
	lodsb
	inc	edi
	cmp	al, 20h
	jna	short pass_write_dirname2
	mov	[edi], al
	cmp	edi, End_Of_Current_Dir_Str
	jnb	short pass_write_path
	loop    loc_check_dir_name_ext
	inc	edi
pass_write_dirname2:
	dec	ah
	jz      short pass_write_path
	add	ebx, 16
	mov	esi, ebx
	mov	byte [edi],"/"
	inc	edi
	jmp	short loc_write_path
pass_write_path:
	mov	byte [edi], 0
	inc	edi
	mov	ecx, edi
	pop	edi
	sub	ecx, edi
	; ECX = Current Directory String Length
	retn

get_current_directory:
	; 16/05/2025 (TRDOS 386 Kernel v2.0.10)
	; 29/08/2023 (TRDOS 386 Kernel v2.0.6)
	; 11/08/2022
	; 28/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 15/10/2016
	; 14/02/2016
	; 24/01/2016 (TRDOS 386 = TRDOS v2.0)
	; 27/03/2011
	;
	; INPUT-> ESI = Current Directory Buffer
	;         DL = TRDOS Logical Dos Drive Number + 1
	;              (0 = Default/Current Drive)
	;
	;   Note: Required dir buffer length may be <= 92 bytes
	;         for TRDOS (7*12 name chars + 7 slash + 0)
	; 	; 16/05/2025 - buffer length <= 104 bytes
	;	  (8*12 name chars + 7 slashes + 0)

	; OUTPUT ->  ESI = Current Directory Buffer
	;            EAX, EBX, ECX, EDX, EDI will be changed
	;            ECX/CL = Current Directory String Length
	;	     DL = Drive Number (0 based)
	;            (If input is 0, output is current drv number)
	;            DH = same with input 
	;   cf = 0 -> AL = 0
	;   cf = 1 -> error code in AL

loc_get_current_drive_0:
	; 29/08/2023
	sub	eax, eax ; 0
	;cmp	dl, 0
	cmp	dl, al
	ja	short loc_get_current_drive_1
	mov	dl, [Current_Drv]
	; 29/08/2023
	;jmp	short loc_get_current_drive_2

loc_get_current_drive_2:
	; 29/08/2023
	;xor	eax, eax
	; eax = 0
	mov	ah, dl
	push	esi ; (*)
	mov	esi, Logical_DOSDisks
	add	esi, eax
	mov	al, [esi+LD_Name]
	cmp	al, 'A'
	jb	short loc_get_current_drive_not_ready_retn

	; 28/07/2022
	xor	ecx, ecx

	mov	ah, [esi+LD_CDirLevel]
	or	ah, ah
	jnz	short loc_get_current_drive_3

	;xor	ah, ah ; mov ah, 0

	; 11/08/2022 - BugFix (*)
	pop	esi ; (*) Current Directory Buffer address

	mov	[esi], ah
	; 28/07/2022
	;xor	ecx, ecx
	jmp	short loc_get_current_drive_4

	; 29/08/2023
loc_get_current_drive_1:
	dec 	dl
	cmp	dl, [Last_DOS_DiskNo]
	jna	short loc_get_current_drive_2
	;mov	eax, 0Fh ; Invalid drive (Drive not ready!)
	;cmc 	; stc
	; 28/07/2022
	;sub	eax, eax ; 29/08/2023
	; eax = 0
	mov	al, 0Fh
	stc
	retn

loc_get_current_drive_not_ready_retn:
	pop	esi
	;mov	eax, 15
	mov	ax, 15 ; Drive not ready
	retn

loc_get_current_drive_3:
	; 16/05/2025
	;mov	edi, PATH_Array
	;push	edi
	add	esi, LD_CurrentDirectory
	;;mov	ecx, 32
	;; 28/07/2022
	;mov	cl, 32
	;rep	movsd
	;pop	esi ; Path Array Address
	pop	edi ; pushed esi (current dir buffer offset)
	;
	call	set_current_directory_string
	mov	esi, edi

loc_get_current_drive_4:
	xor	al, al
	retn

change_current_directory:
	; 14/05/2025 (TRDOS 386 Kernel v2.0.10)
	;	(8 sub directory levels, + root directory)
	; 28/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 02/03/2021 (TRDOS 386 v2.0.3) ((BugFix))
	; 19/02/2016
	; 11/02/2016
	; 10/02/2016
	; 08/02/2016
	; 06/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 18/09/2011 (DIR.ASM, 09/10/2011)
	; 04/10/2009
	; 2005
	; INPUT ->
	;	ESI = Directory string
	;	ah = CD command (CDh = save current dir string)
	; OUTPUT ->
	; 	EDI = DOS Drive Description Table
	; 	cf = 1 -> error
	;	   EAX = Error code (in AL) ; 14/05/2025
	;	cf = 0 -> successful
	;	   ESI = PATH_Array
	;	   EAX = Current Directory First Cluster
	;
	; (EAX, EBX, ECX, EDX, ESI, EDI will be changed)

	mov	[CD_COMMAND], ah
	cmp	byte [esi], '/'
	jne	short loc_ccd_cdir_level
	inc	esi
	xor	al, al
	jmp	short loc_ccd_parse_path_name
loc_ccd_cdir_level:
	mov	al, [Current_Dir_Level]
loc_ccd_parse_path_name:
	mov	ah, al
	mov	edi, PATH_Array

; Reset directory levels > cdir level
	; is this required !?
	;
	; Relations:
	; MAINPROG.ASM (pass_ccdrv_reset_cdir_FAT_fcluster)
	; proc_parse_dir_name,
	; proc_change_current_directory (this procedure)
	; proc_change_prompt_dir_string

	movzx	ecx, al
	; 14/05/2025 (8 sub dir levels, 9 dir levels)
	;inc	cl
	shl	cl, 4
	add	edi, ecx
	;mov	cl, 7
	mov	cl, 8
	sub	cl, al
	shl	cl, 2
	mov	ebx, eax
	xor	eax, eax ; 0
	rep	stosd
	mov	eax, ebx

	mov	edi, PATH_Array

	cmp	byte [esi], 20h
	cmc
	jnc	short pass_ccd_parse_dir_name

		; ESI = Path name
		; AL = CCD_Level
	call	parse_dir_name
		; AL = CCD_Level
		; AH = Last Sub Directory Level
		; (EDI = PATH_Array)

pass_ccd_parse_dir_name:
	pushf

	;mov	[CCD_Level], al
        ;mov	[Last_Dir_Level], ah
	mov	[CCD_Level], ax

	xor	ebx, ebx
	mov	bh, [Current_Drv]
	mov	esi, Logical_DOSDisks
	add	esi, ebx

	popf
	jc	short loc_ccd_bad_path_name_retn

	mov	[CCD_DriveDT], esi

	; 14/05/2025 - TRDOS 386 v2.0.9
	cmp	al, 8
	;cmp	al, 7
	jb	short loc_ccd_load_child_dir

loc_ccd_bad_path_name_retn:
	xchg	esi, edi
	;mov	eax, 19 ; Bad directory/path name
	; 28/07/2022
	sub	eax, eax
	mov	al, 19	; ERR_INV_PATH_NAME
	stc
loc_ccd_retn_p:
	retn

loc_ccd_load_child_dir:
	; AL = CCD_Level
	; AH = Last_Dir_Level
	or	al, al
	jz	short loc_ccd_load_root_dir

	; 28/07/2022
	mov	ecx, eax

	; 14/05/2025 - TRDOS 386 v2.0.10 (8 sub dir levels)
	dec	eax  ; 1 -> 0, 8 -> 7

	shl	al, 4
	movzx	esi, al
     	add	esi, edi  ; offset PATH_Array

	mov	eax, [esi+12]
	cmp	cl, ch ; ch = 0
	;je	loc_ccd_load_sub_directory
	; 28/07/2022
	jne	short loc_ccd_1
	jmp	loc_ccd_load_sub_directory

loc_ccd_1:	; 28/07/2022
	mov	[Current_Dir_FCluster], eax

loc_ccd_load_child_dir_next:
	add	esi, 16 ; DOS DirEntry Format FileName Address

 	; Directory attribute : 10h
	mov	al, 00010000b ; 10h (Attrib AND mask)
	;mov	ah, 11001000b ; C8h
	; Volume name attribute: 8h
	mov	ah, 00001000b ; 08h (Attrib NAND, AND --> zero mask)

	xor	ecx, ecx ; 02/03/2021
	call	locate_current_dir_file
	jnc	short loc_ccd_set_dir_cluster_ptr

	; 19/02/2016
	;mov	edi, [CCD_DriveDT]
	mov	ah, [CCD_Level]
	cmp	byte [CD_COMMAND], 0CDh ;'CD' command or another
	jne	short loc_ccd_load_child_dir_err
	; It is better to save recent successful part
	; of the (requested) path as current directory.
	; (Otherwise the path would be reset to back
	; on the next 'CD' command.)
	mov	cl, ah
	push	eax
	call	loc_ccd_save_current_dir
	pop	eax
loc_ccd_load_child_dir_err:
	cmp	al, 3	; AL = 2 => File not found error
	jb	short loc_ccd_path_not_found_retn
	stc
	retn

loc_ccd_path_not_found_retn:
	mov	al, 3	; Path not found
	retn

loc_ccd_load_FAT_root_dir:
	;mov	esi, [CCD_DriveDT]
	; 14/05/2025
	cmp	byte [esi+LD_FATType], 2
	;cmp	byte [Current_FATType], 2
	ja	short loc_ccd_load_FAT32_root_dir

	;push	esi
	call	load_FAT_root_directory
	;pop	edi ; Dos Drv Description Table

	mov	edi, esi
	mov	esi, PATH_Array
	jc	short loc_ccd_retn_p

	xor	eax, eax
        jmp	short loc_ccd_set_cdfc

loc_ccd_load_root_dir:
	; 14/05/2025
	cmp	byte [esi+LD_FATType], 1
	;cmp	byte [Current_FATType], 1
	jnb	short loc_ccd_load_FAT_root_dir

loc_ccd_load_FS_root_dir:
	call	load_FS_root_directory
	jmp	short pass_ccd_load_FAT_sub_directory

loc_ccd_load_FS_sub_directory_next:
	call	load_FS_sub_directory
	jmp	short pass_ccd_set_dir_cluster_ptr

loc_ccd_set_dir_cluster_ptr:
	; EDI = Directory Entry
	mov	ax, [edi+20] ; First Cluster High Word
	shl	eax, 16
	mov	ax, [edi+26] ; First Cluster Low Word

	mov	esi, [CCD_DriveDT]
	; 14/05/2025
	cmp	byte [esi+LD_FATType], 1
	;cmp	byte [Current_FATType], 1
	jb	short loc_ccd_load_FS_sub_directory_next
	;push	esi
	call	load_FAT_sub_directory
	;pop	edi ; Dos Drv Description Table

pass_ccd_set_dir_cluster_ptr:
	;mov	edi, esi
	mov	esi, PATH_Array
	jc	short loc_ccd_retn_c

	;mov	eax, [DirBuff_Cluster]
	; 14/05/2025
	mov	eax, [CLUSNUM]

	; 14/05/2025 - TRDOS 386 v2.0.10
	;	(8 sub dir levels, + root directory)
	movzx	ebx, byte [CCD_Level]
	inc	byte [CCD_Level]
	;movzx	ebx, byte [CCD_Level]
	shl	bl, 4 ; * 16 (<= 128)
	add	esi, ebx ; 19/02/2016
	mov	[esi+12], eax
	jmp	short loc_ccd_set_cdfc

loc_ccd_load_FAT32_root_dir:
	; 14/05/2025
	;mov	esi, PATH_Array
	;mov	eax, [esi+12]
	;mov	esi, [CCD_DriveDT]
	mov	eax, [esi+LD_BPB+BPB_RootClus]

loc_ccd_load_FAT_sub_directory:
	;push	esi
	call	load_FAT_sub_directory
	;pop	edi ; Dos Drv Description Table

pass_ccd_load_FAT_sub_directory:
	;mov	edi, esi
	mov	esi, PATH_Array
	jc	short loc_ccd_retn_c

	;mov	eax, [DirBuff_Cluster]
	; 14/05/2025
	mov	eax, [CLUSNUM]

loc_ccd_set_cdfc:
	mov	cl, [CCD_Level]
	mov	[Current_Dir_Level], cl
	mov	[Current_Dir_FCluster], eax

	mov	ch, [Last_Dir_Level]
	cmp	cl, ch
	;jb	loc_ccd_load_child_dir_next
	; 28/07/2022
	jnb	short loc_ccd_2	
	jmp	loc_ccd_load_child_dir_next
loc_ccd_2:
	cmp	byte [CD_COMMAND], 0CDh ;'CD' command or another
	je	short loc_ccd_save_current_dir

        ; jne -> don't save, restore (the previous cdir) later !
        ; (saving the cdir would prevent previous cdir restoration!)

	clc

loc_ccd_retn_c:
	mov	edi, [CCD_DriveDT]
	retn

loc_ccd_load_sub_directory:
	mov	esi, [CCD_DriveDT]
	cmp	byte [Current_FATType], 1
	jnb	short loc_ccd_load_FAT_sub_directory
	call	load_FS_sub_directory
	jmp	short pass_ccd_load_FAT_sub_directory

loc_ccd_save_current_dir:
	; 14/05/2025 (TRDOS 386 v2.0.10)
	; 02/03/2021 (TRDOS 386 v2.0.3) ((BugFix))
	; ('find_directory_entry' has been fixed to prevent large
	; ECX value > 65535)
	;
	mov	esi, PATH_Array ; 19/02/2016
	mov	edi, [CCD_DriveDT]
	push	edi
        add     edi, LD_CDirLevel
	mov	[edi], cl
	inc	edi ; LD_CurrentDirectory
	push	esi
	;mov	cx, 32
	; 14/05/2025
	mov	ecx, 32
	rep	movsd
	; Current directory has been saved to
	; the DOS drive description table, cdir area !
	pop	esi  ; PATH_Array
	pop	edi  ; Dos Drv Description Table

	retn

parse_dir_name:
	; 14/05/2025 - TRDOS 386 v2.0.10
	; 11/02/2016
	; 10/02/2016
	; 07/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 18/09/2011
	; 17/10/2009
	; INPUT ->
	;	ESI = ASCIIZ Directory String Address
	;	AL = Current Directory Level
	;	EDI = Destination Adress
	;	     (8 sub dir levels, each one 12+4 bytes)
	; OUTPUT ->
	;	EDI = Dir Entry Formatted Array
	;	     with zero cluster pointer at the last level
	;	AH = Last Dir Level
	;	AL = Current Dir Level
	;
	; (esi, ebx, ecx will be changed)

	;mov	[PATH_Array_Ptr], edi
	mov	ah, al
	mov	[PATH_CDLevel], ax
repeat_ppdn_check_slash:
	lodsb
	cmp	al, '/'
	je	short repeat_ppdn_check_slash
	cmp	al, 21h
	jb	short loc_ppdn_retn
	push	edi
loc_ppdn_get_dir_name:
	mov	ecx, 12
	mov	edi, Dir_File_Name
repeat_ppdn_get_dir_name:
	stosb
	lodsb
	cmp	al, '/'
	je	short loc_check_level_dot_conv_dir_name
	cmp	al, 20h
	jna	short loc_ppdn_end_of_path_scan
	loop	repeat_ppdn_get_dir_name
	pop	edi
	stc
loc_ppdn_retn:
	retn

loc_ppdn_end_of_path_scan:
	dec	esi
loc_check_level_dot_conv_dir_name:
	xor	eax, eax
	stosb
	mov	ebx, esi
	mov	esi, Dir_File_Name
	lodsb
repeat_ppdn_name_check_dot:
	cmp	al, '.'
	jne	short loc_ppdn_convert_sub_dir_name
repeat_ppdn_name_dot_dot:
	lodsb
	cmp	al, '.'
	je	short loc_ppdn_dot_dot
	cmp	al, 21h
	jb	short pass_ppdn_convert_sub_dir_name
loc_ppdn_convert_sub_dir_name:
	mov	ah, [PATH_Level]
	; 14/05/2025
	cmp	ah, 8
	;cmp	ah, 7
	jnb	short pass_ppdn_convert_sub_dir_name
	;inc	ah
	;mov	[PATH_Level], ah
	; 14/05/2025
	;dec	ah
	inc	byte [PATH_Level] ; Last sub dir level
	;
	mov	esi, Dir_File_Name
	;mov	edi, [PATH_Array_Ptr]
	mov	al, 16
	mul	ah
	mov	edi, [esp]
	;push	edi
	add	edi, eax
	call	convert_file_name
	;pop	edi
pass_ppdn_convert_sub_dir_name:
	mov	esi, ebx
repeat_ppdn_check_last_slash:
	lodsb
	cmp	al, '/'
	je	short repeat_ppdn_check_last_slash
	cmp	al, 21h
	jnb	short loc_ppdn_get_dir_name
end_of_parse_dir_name:
	pop	edi
	cmc
	;mov	al, [PATH_CDLevel]
	;mov	ah, [PATH_Level] ; Last sub dir level
	mov	ax, [PATH_CDLevel]
	retn

loc_ppdn_dot_dot:
	lodsb
	cmp	al, 21h
	jnb	short end_of_parse_dir_name
loc_ppdn_dot_dot_prev_level:
	mov	ax, [PATH_CDLevel]
	sub	ah, 1
	adc	ah, 0 ; Last sub dir level - 1 (if > 0)
	cmp	al, ah
	jna	short pass_ppdn_set_al_to_ah
	mov	al, ah
pass_ppdn_set_al_to_ah:
	mov	[PATH_CDLevel], ax
	jmp	short pass_ppdn_convert_sub_dir_name

locate_current_dir_file:
	; 29/05/2025
	; 18/05/2025
	; 17/05/2025
	; 16/05/2025 (TRDOS 386 v2.0.10)
	; 26/08/2024 (TRDOS 386 v2.0.9)
	; 28/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 20/11/2017
	; 14/02/2016
	; 13/02/2016
	; 10/02/2016
	; 06/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 14/08/2010
	; 19/09/2009
        ; 2005
	; INPUT ->
	;	ESI = DOS DirEntry Format FileName Address
	;	AL = Attributes Mask
	;	(<AL AND EntryAttrib> must be equal to AL)
	;	AH = Negative Attributes Mask (If AH>0)
	;	(<AH AND EntryAttrib> must be ZERO)
	;	CH > 0 Find First Free Dir Entry or Deleted Entry
	;	CL = 0 -> Return the First Free Dir Entry
	;	CL = E5h -> Return the 1st deleted entry
	;	CL = FFh -> Return the 1st deleted or free entry
	;	CL > 0 and CL <> E5h and CL <> FFh -> Return the first
	;	     proper entry (which fits with Atributes Masks)
	;	CX = 0 Find Valid File/Directory/VolumeName
	;	? = Any One Char
	;	* = Every Chars
	; OUTPUT ->
	;	EDI = Directory Entry Address (in Directory Buffer)
	;	ESI = DOS DirEntry Format FileName Address
	;	CF = 0 -> No Error, Proper Entry
	;	DL = Attributes
	;	DH = Previous Entry Attr (LongName Check)
	;	;AL > 0 -> Ambiguous filename wildcard "?" used
	;	;AH > 0 -> Ambiguous filename wildcard "*" used
	;	;AX = 0 -> Filename full fits with directory entry
	;       18/05/2025
	;	AL bit 0, 1 -> "?" used (bit 1 for name ext.)
	;	AL bit 2, 3 -> '*' used (bit 3 for name ext.)
	;	AL = 0 -> Filename full fits with directory entry
	;	CH = The 1st Name Char of Current Dir Entry
	;	CF = 1 -> Proper entry not found, Error Code in EAX/AL
	;	CL = 0 and CH = 0 -> Free Entry (End Of Dir)
	;	CL = 0 and CH = E5h -> Deleted Entry fits with filters
	;	CL > 0 -> Entry not found, CH invalid
	;	CF = 0 ->
	;	EBX = Current Directory Entry Index/Number (BX)
	;
	;	; 29/05/2025
	; 	If cf = 0 ->
	; 	[DirEntry_Counter] = directory entry index number
	;		from the start of the directory (found)
	; 	If cf= 1 ->
	;   	[DirEntry_Counter] = the last direntry index number
	;		from the start of the directory (not found)
	
	;mov	word [DirBuff_EntryCounter], 0 ; Zero Based

	; 17/05/2025
	; 16/05/2025 - TRDOS 386 v2.0.10
	xor	edx, edx

	; 29/05/2025
	; Reset directory entry (index) counter
	mov	[DirEntry_Counter], edx ; 0 
	; 29/05/2025
	mov	[PreviousAttr], dl ; 0

	mov	dh, [Current_Drv]

	; 17/05/2025
	mov	ebx, [Current_Dir_FCluster]

locate_current_dir_file_@:
	; edx = LDRVT address
	; ebx = current (search, fff/fnf) cluster

	mov	[CDLF_FNAddress], esi
	mov	[CDLF_AttributesMask], ax
	mov	[CDLF_DEType], cx

	; 17/05/2025
	;xor	eax, eax
	; 29/05/2025
	;mov	[PreviousAttr], al ; 0  ; 13/02/2016

	mov	eax, ebx
	and	eax, eax
	jnz	short locate_current_sub_dir_file

	; root directory
	mov	[DirBuff_Cluster], eax ; 0
	mov	ecx, [edx+LD_DATABegin]
	mov	eax, [edx+LD_ROOTBegin]
	sub	ecx, eax
	;mov	[DirBuff_sectors], cl ; (MSDOS -> [CLUSFAC])
	mov	[CLUSFAC], cl

locate_current_dir_file_ns:
	mov	cl, [edx+LD_PhyDrvNo]
	jmp	short locate_current_sub_dir_file_@

loc_locatefile_next_cluster:
	;mov	edx, esi	; LDRVT address
locate_current_sub_dir_file:
	mov	[DirBuff_Cluster], eax ; 17/05/2025

	mov	cl, [edx+LD_BPB+SecPerClust]
	;mov	[DirBuff_sectors], cl
	mov	[CLUSFAC], cl
	
	; 17/05/2025
	;sub	ebx, ebx

	; edx = Logical DOS Drive Description Table address
	; eax = Cluster Number (28bit for FAT32 fs)
	;;ebx = Sector position in cluster = 0

	call	FIGREC

locate_current_sub_dir_file_@:
	; eax = physical sector number
	;  cl = physical drive/disk number
	;       (needed for GETBUFFER procedure)
	mov	[DIRSEC], eax

	call	GETBUFFER
	jc	short loc_cdir_locate_file_retn
	
	;mov	esi, [CurrentBuffer]
	or	byte [esi+BUFFINFO.buf_flags], buf_isDIR

loc_cdir_locatefile_search:
	; 16/05/2025
	lea	edi, [esi+BUFINSIZ]

	mov	esi, [CDLF_FNAddress]
	mov	ax, [CDLF_AttributesMask] ; 29/05/2025
	mov	cx, [CDLF_DEType]

	;mov	byte [DirBuff_LastEntry], 16 ; 512/32

	xor	ebx, ebx ; 0  ; current entry index in the dir buff 
	; edi = directory buffer (data) start address

	push	edx ; LDRVT address
	call	find_directory_entry
	pop	ebp
	jnc	short loc_cdir_locate_file_retn

loc_locatefile_check_stc_reason:
	or	ch, ch
	jz	short loc_cdir_locate_file_stc_retn

loc_locatefile_check_next_entryblock:
	; 16/05/2025
        mov	edx, ebp ; Logical DOS Drive Desc. Table address

	dec	byte [CLUSFAC] ; (MSDOS -> [CLUSFAC])
	jz	short loc_locatefile_check_root_dir
	
	mov	eax, [DIRSEC]
	inc	eax

	jmp	short locate_current_dir_file_ns

loc_locatefile_check_root_dir:
	mov	eax, [DirBuff_Cluster]
	or	eax, eax
	jz	short loc_locatefile_file_notfound

	mov	esi, edx
	call	get_next_cluster
	jnc	short loc_locatefile_next_cluster  ; edx = esi

	or	eax, eax
	jnz	short loc_locatefile_drv_not_ready_read_err

	; 16/05/2025
loc_locatefile_file_notfound:
	;mov	eax, 2 ; File/Directory/VolName not found
	;xor	eax, eax
	; 26/08/2024
	; eax = 0
	mov	al, 2
	stc
	retn

loc_locatefile_drv_not_ready_read_err:
	;mov	eax, 17 ;Drive not ready or read error
loc_cdir_locate_file_stc_retn:
	cmc ;stc
loc_cdir_locate_file_retn:
	retn

reload_current_directory:
	; 28/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 06/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 13/06/2010
	; 22/09/2009
        ;
	; INPUT ->
	;	ESI = Dos drive description table address

	;mov	al, [esi+LD_FATType]
	mov	al, [Current_FATType]
	cmp	al, 2
	ja	short loc_reload_FAT_sub_directory
	mov	ah, [Current_Dir_Level]
	or	al, al
	jz	short loc_reload_FS_directory
	or	ah, ah
	jnz	short loc_reload_FAT_sub_directory
loc_reload_FAT_12_16_root_directory:
	;call	load_FAT_root_directory
	;retn
	; 28/07/2022
	jmp	load_FAT_root_directory
loc_reload_FS_directory:
	and	ah, ah
	jnz	short loc_reload_FS_sub_directory 
loc_reload_FS_root_directory: 
	;call	load_FS_root_directory
	;retn
	; 28/07/2022
	jmp	load_FS_root_directory
loc_reload_FS_sub_directory:
	mov	eax, [Current_Dir_FCluster]
	;call	load_FS_sub_directory
	;retn
	jmp	load_FS_sub_directory 
loc_reload_FAT_sub_directory:
	mov	eax, [Current_Dir_FCluster]
	;call	load_FAT_sub_directory
	;retn
	; 28/07/2022
	jmp	load_FAT_sub_directory

find_directory_entry:
	; 02/06/2025
	; 29/05/2025
	; 18/05/2025
	; 17/05/2025
	; 16/05/2025 (TRDOS 386 Kernel v2.0.10)	
	; 28/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 02/03/2021 (TRDOS 386 v2.0.3) ((BugFix))
	; 14/02/2016
	; 13/02/2016
	; 10/02/2016
	; 06/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 14/08/2010 (DIR.ASM, "proc_find_direntry")
	; 19/09/2009
	; 2005
	; INPUT ->
	;	16/05/2025
	;	EDI = Directory Buffer Address (512 bytes, data)
	;	;;;
	;	ESI = Sub Dir or File Name Address
	;	AL = Attributes Mask
	;	(<AL AND EntryAttrib> must be equal to AL)
	;	AH = Negative Attributes Mask (If AH>0)
	;	(<AH AND EntryAttrib> must be ZERO)
	;	CH > 0 Find First Free Dir Entry or Deleted Entry
	;	CL = 0 -> Return the First Free Dir Entry
	;	CL = E5h -> Return the 1st deleted entry
	;	CL = FFh -> Return the 1st deleted or free entry
	;	CL > 0 and CL <> E5h and CL <> FFh -> Return the first
	;            proper entry (which fits with Atributes Masks)
	;	CX = 0 -> Find Valid File/Directory/VolumeName
	;	? = Any One Char
	;	* = Every Chars
	;	EBX = Current Dir Entry (BX) < 16 ; 17/05/2025
	;
	;	; 29/05/2025
	;	[DirBuff_EntryCounter] = entry index from the start
	;
	; OUTPUT ->
	;	EDI = Directory Entry Address (in DirectoryBuffer)
	;	ESI = Sub Dir or File Name Address
	;	CF = 0 -> No Error, Proper Entry
	;	DL = Attributes
	;	DH = Previous Entry Attr (LongName Check)
	;	;AL > 0 -> Ambiguous filename wildcard "?" used
	;	;AH > 0 -> Ambiguous filename wildcard "*" used
	;	;AX = 0 -> Filename full fits with directory entry
	;       18/05/2025
	;	AL bit 0, 1 -> "?" used (bit 1 for name ext.)
	;	AL bit 2, 3 -> '*' used (bit 3 for name ext.)
	;	AL = 0 -> Filename full fits with directory entry
	;	EBX = CurrentDirEntry (BX)
	;	CH = The 1st Name Char of Current Dir Entry
	;	CF = 1 -> Proper entry not found, Error Code in AX/AL
	;	CL = 0 and CH = 0 -> Free Entry (End Of Dir)
	;	CL = 0 and CH = E5h -> Deleted Entry fits with filters
	;	CL > 0 -> Entry not found, CH invalid
	;
	;	; 29/05/2025
	;	[DirBuff_EntryCounter] = entry index from the start
	;				(or the last entry found)
	;
	; (EAX, EBX, ECX, EDX, EDI, EBP will be changed)

	;cmp	bx, [DirBuff_LastEntry]
	;ja	short loc_ffde_stc_retn_255 ; 28/07/2022
	; 17/05/2025
	; 16/05/2025
	;cmp	ebx, 16 ; 512/32
	;jnb	short loc_ffde_stc_retn_255

	;mov    [DirBuff_CurrentEntry], bx

	; 16/05/2025
  	;mov	edi, Directory_Buffer
	mov	[FDE_AttrMask], ax

	sub	eax, eax

	;;mov	[PreviousAttr], al ; 0 ;; 13/02/2016
	; 18/05/2025
	;mov	[AmbiguousFileName], ax ; 0

	;mov	ax, bx
	; 16/05/2025
	;mov	eax, ebx
	; 17/05/2025
	mov	al, bl
	;shl	ax, 5 ; ; * 32 ; Directory entry size
	; 28/07/2022
	shl	eax, 5
	add     edi, eax

	or	ch, ch
	;jnz	loc_find_free_deleted_entry_0
	; 28/07/2022
	jz	short loc_fde_any_valid_entry_opt
	jmp	loc_find_free_deleted_entry_0

loc_fde_any_valid_entry_opt:
	or      cl, cl
        ;jnz	loc_ffde_stc_retn_255
	; 28/07/2022
	jz	short check_find_dir_entry

	; 28/07/2022
loc_ffde_stc_retn_255:
	; 02/03/2021 (TRDOS 386 v2.0.3) ((BugFix))
	; (ECX must not be > 65535)
	; ((because 'loc_ccd_save_current_dir'
	;  sets CX to 32 for 'rep movsd'))
	mov	cx, 0FFFFh
	;xor	ecx, ecx
	;dec	ecx ; 0FFFFFFFFh
	;xor	eax, eax
loc_find_direntry_stc_retn:
loc_check_ffde_retn_1:
	;mov	ax, 2
	;mov	eax, 2 ; File Not Found
	; 28/07/2022
	sub	eax, eax
	mov	al, 2
	mov	dh, [PreviousAttr]
	;mov	[DirBuff_CurrentEntry], bx
	mov	[DirBuff_CurrentEntry], bl
	stc
	retn

	; 28/07/2022
loc_find_dir_next_entry_prevdeleted:
	or	dl, 80h  ; Bit 7 -> deleted entry sign
	;jmp	short loc_find_dir_next_entry

	; 28/07/2022
loc_find_dir_next_entry:
	mov	byte [PreviousAttr], dl ; LongName check
loc_find_dir_next_entry_1:
	pop	esi
	; 
	; 29/05/2025
	; Directory entry index from the start of
	; the directory (from the 1st cluster)
	inc	dword [DirEntry_Counter]
	;
	add	edi, 32
	;;inc	word [DirBuff_EntryCounter]
	;inc	bx
	; 28/07/2022
	inc	ebx
	;cmp	bx, [DirBuff_LastEntry]
	;ja	short loc_ffde_stc_retn_255
	; 16/05/2025
	cmp	bl, 16 ; 512/32
	jnb	short loc_ffde_stc_retn_255
        ; 28/07/2022
	;jmp	short check_find_dir_entry

check_find_dir_entry:
	mov	ax, [FDE_AttrMask]
	mov	ch, [edi]
	cmp     ch, 0 ; Is it never used entry?
	;jna	loc_find_direntry_stc_retn
	; 28/07/2022
	ja	short loc_fde_check_attrib
	; end of directory entries
	jmp	loc_find_direntry_stc_retn
loc_fde_check_attrib:
	push	esi
	mov	dl, [edi+0Bh] ; File attributes
	cmp	ch, 0E5h ; Is it a deleted file?
	je	short loc_find_dir_next_entry_prevdeleted

	cmp	dl, 0Fh ; longname sub component check
	jne	short loc_check_attributes_mask
	call	save_longname_sub_component

loc_check_attributes_mask:
	mov	dh, al
	and	dh, dl
	; 28/07/2022
	cmp	al, dh
	jne	short loc_find_dir_next_entry
	and	ah, dl
        jnz	short loc_find_dir_next_entry
	cmp	dl, 0Fh
	jne	short pass_direntry_attr_check

	cmp	al, 0Fh ; AL = 0Fh -> find long name
	jne	short loc_find_dir_next_entry

	pop	esi
	;xor	ax, ax
	; 28/07/2022
	;sub	eax, eax
	xor	al, al
	mov	dh, [PreviousAttr]
	;mov	[DirBuff_CurrentEntry], bx
	; 16/05/2025
	mov	[DirBuff_CurrentEntry], bl
	retn

pass_direntry_attr_check:
	;mov	ebp, edi ; 14/02/2016
	; 18/05/2025
	push	edi ; *

; 18/05/2025 - TRDOS 286 v2.0.10
%if 0
	;mov	ecx, 8
	; 28/07/2022
	sub	ecx, ecx
	mov	cl, 8
loc_lodsb_find_dir:
	lodsb
	cmp	al, '*'
	jne	short pass_fde_ambiguous1_check
        inc     byte [AmbiguousFileName+1]
	jmp	short loc_check_direntry_extension

pass_fde_ambiguous1_check:
	cmp	al, '?'
	jne	short pass_fde_ambiguous2_check
	inc	byte [AmbiguousFileName]
	cmp	byte [edi], 20h
	jna	short loc_find_dir_next_entry_ebp
	jmp	short loc_scasb_find_dir_inc_di

pass_fde_ambiguous2_check:
	cmp	al, 20h
	jne	short loc_scasb_find_dir
	cmp	byte [edi], 20h
	jne	short loc_find_dir_next_entry_ebp
	jmp	short loc_check_direntry_extension

loc_scasb_find_dir:
	cmp	al, [edi]
	jne	short loc_find_dir_next_entry_ebp
loc_scasb_find_dir_inc_di:
	inc	edi
	loop	loc_lodsb_find_dir

loc_check_direntry_extension:
	mov	esi, 8
	mov	edi, esi ; 8
	add	esi, [esp] ; Sub Dir or File Name Address
	add	edi, ebp
	mov	cl, 3
loc_lodsb_find_dir_ext:
	lodsb
	cmp	al, '*'
	jne	short pass_fde_ambiguous3_check
	inc	byte [AmbiguousFileName+1]
	jmp	short loc_find_dir_proper_direntry

pass_fde_ambiguous3_check:
	cmp	al, '?'
	jne	short pass_fde_ambiguous4_check
	inc	byte [AmbiguousFileName]
	cmp	byte [edi], 20h
	;jna	short loc_find_dir_next_entry_ebp
	;jmp	short loc_scasb_find_dir_ext_inc_di
	; 28/07/2022
	ja	short loc_scasb_find_dir_ext_inc_di

loc_find_dir_next_entry_ebp:
	mov	edi, ebp ; 14/02/2016
	jmp	loc_find_dir_next_entry ; 28/07/2022

pass_fde_ambiguous4_check:
	cmp	al, 20h
	jne	short loc_scasb_find_dir_ext
	cmp	byte [edi], 20h
	; 28/07/2022
	jne	short loc_find_dir_next_entry_ebp
	;jmp	short loc_find_dir_proper_direntry

loc_scasb_find_dir_ext:
	cmp	al, [edi]
	jne	short loc_find_dir_next_entry_ebp
loc_scasb_find_dir_ext_inc_di:
	inc	edi
	loop    loc_lodsb_find_dir_ext
	jmp	short loc_find_dir_proper_direntry_1
%else
	; 18/05/2025
	; compare 11 byte dir entr name
	; esi = 11 chars DirEntry format name with possible '?'
	; edi = 11 chars DirEntry format name, no '?'

	call	METACOMPARE
	pop	edi ; *
	jnz	short loc_find_dir_next_entry
%endif

loc_find_dir_proper_direntry:
	xor	cl, cl
loc_find_dir_proper_direntry_1:
	pop	esi
	; 18/05/2025
        ;mov	edi, ebp
	mov	ch, [edi]
	mov	dl, [edi+0Bh] ; Dir entry attributes
	;mov	ax, [AmbiguousFileName]
	; 18/05/2025
	mov	al, [AmbiguousFileName]
loc_find_dir_proper_direntry_2:
	mov	dh, [PreviousAttr]
	;mov	[DirBuff_CurrentEntry], bx
	; 16/05/2025
	mov	[DirBuff_CurrentEntry], bl
	retn

loc_find_free_deleted_entry_0:
	mov	ax, [FDE_AttrMask]
	mov	ch, [edi]
	mov	dl, [edi+0Bh] ; File attributes
	or	cl, cl
	jz	short loc_check_ffde_0_repeat
	;cmp	cl, 0E5h
	;je	short pass_loc_check_ffde_0_err
	cmp	cl, 0FFh
	je	short loc_find_free_deleted_entry_1
	jmp	short pass_loc_check_ffde_0_err

loc_check_ffde_0_repeat:
	or	ch, ch
	jnz	short loc_check_ffde_0_next

loc_check_ffde_retn_2:
	;sub	ax, ax
	; 28/07/2022
	sub	eax, eax
	mov	dh, [PreviousAttr]
	;mov	[DirBuff_CurrentEntry], bx
	; 16/05/2025
	mov	[DirBuff_CurrentEntry], bl
	retn

loc_check_ffde_0_next:
	;inc	bx
	; 28/07/2022
	inc	ebx
	add	edi, 32
	;inc	word [DirBuff_EntryCounter]
	 
        ;cmp	bx, [DirBuff_LastEntry]
	;;ja	short loc_ffde_stc_retn_255
	;; 07/08/2022
	;ja	short jmp_ffde_stc_retn_255
	; 16/05/2025
	cmp	bl, 16
	jnb	short jmp_ffde_stc_retn_255

	mov	[PreviousAttr], dl
	mov	ch, [edi]
	mov	dl, [edi+0Bh] ; file attributes
	jmp	short loc_check_ffde_0_repeat

loc_find_free_deleted_entry_1:
	sub	dl, dl
loc_find_free_deleted_entry_2:
	and	ch, ch  
	jz	short loc_check_ffde_retn_2
	cmp	ch, 0E5h
	je	short loc_check_ffde_retn_2
	;inc	bx
	; 28/07/2022
	inc	ebx
	add	edi, 32

        ;cmp	bx, [DirBuff_LastEntry]
	;;ja	short loc_ffde_stc_retn_255
	;; 07/08/2022
	;ja	short jmp_ffde_stc_retn_255
	; 16/05/2025
	cmp	bl, 16
	jnb	short jmp_ffde_stc_retn_255

	mov	ch, [edi]
	jmp	short loc_find_free_deleted_entry_2

pass_loc_check_ffde_0_err:
	cmp	ch, cl
	je	short loc_check_ffde_attrib

	;inc	bx
	; 28/07/2022
	inc	ebx
	add	edi, 32

	;cmp	bx, [DirBuff_LastEntry]
        ;;ja	loc_ffde_stc_retn_255
	;; 28/07/2022
	;jna	short loc_ffe_save_prev_attr
	; 16/05/2025
	cmp	bl, 16 ; 512/32
	jb	short loc_ffe_save_prev_attr

jmp_ffde_stc_retn_255:	; 07/08/2022
	jmp	loc_ffde_stc_retn_255

loc_ffe_save_prev_attr: ; 28/07/2022
	mov	[PreviousAttr], dl
	mov	ch, [edi]
	mov	dl, [edi+0Bh]
	jmp	short pass_loc_check_ffde_0_err

loc_check_ffde_attrib:
	mov	dh, al
	and	dh, dl
	cmp	al, dh
	jne	short loc_check_ffde_0_next
	and	ah, dl
	jnz	short loc_check_ffde_0_next
	xor	cl, cl 
        jmp	short loc_check_ffde_retn_2

convert_file_name:
	; 18/05/2025 (TRDOS 386 Kernel v2.0.10)
	; 29/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 06/03/2016
	; 11/02/2016
	; 07/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 06/10/2009
	; 2005
	;
	; INPUT  ->
	;	ESI = Dot File Name Location
	;	EDI = Dir Entry Format File Name Location
	; OUTPUT ->
	;	EDI = Dir Entry Format File Name Location
	;	ESI = Dot File Name Location (capitalized)
	;
	;	18/05/2025
	;      [AmbiguousFileName] bits will be set 
	;	if DOT file name contains '?' or '*'
	;	   bit 0,1 -> '?' (bit 1 -> extension)
	;	   bit 2,3 -> '*' (bit 3 -> extension)			
	;
	; (ECX, AL will be changed)

	push	esi
	push	edi

	;mov	ecx, 11
	; 29/07 2022
	sub	ecx, ecx
	mov	cl, 11
	mov	al, 20h
	rep	stosb
	; 18/05/2025
	;xor	eax, eax
	;stosb

	mov	edi, [esp]

; 18/05/2025
%if 0
	mov	cl, 12 ; file name length (max.)
	; 06/03/2016
	; Directory entry name limit (11 bytes) check for
	; 'rename_directory_entry' procedure.
	; (EDI points to Directory Entry)
	; (If the file name would not contain a dot
	; and file name length would be 12, this would cause to
	; overwrite the attributes byte of the directory entry.)
	;
	mov	ch, 11 ; directory entry's name length
loc_check_first_dot:
	mov	al, [esi]
	cmp	al, 2Eh
	jne	short pass_check_first_dot
	mov	[edi], al
	inc	edi
	inc	esi
	dec	cl
	jnz	short loc_check_first_dot
	;;(ecx <= 12)
	;;loop	loc_check_first_dot 
	jmp	short stop_convert_file

loc_get_fchar:
	mov	al, [esi]
pass_check_first_dot:
	cmp	al, 61h ; 'a'
	jb	short pass_name_capitalize
	cmp	al, 7Ah ; 'z'
	ja	short pass_name_capitalize
	and	al, 0DFh
	mov	[esi], al
pass_name_capitalize:
	cmp	al, 21h
	jb	short stop_convert_file
	cmp	al, 2Eh ; '.'
	jne	short pass_dot_space
add_dot_space:
	cmp	cl, 4
	jna	short inc_and_loop
	inc	edi
	dec	ch ; 06/03/2016
	dec	cl
	jmp	short add_dot_space

	;mov	al, 4
	;cmp	cl, al
	;jna	short inc_and_loop
	;sub	cl, al
	;add	edi, ecx
	;mov	cl, al
	;jmp	short inc_and_loop

pass_dot_space:
	mov	[edi], al
loc_after_double_dot:
	; 06/03/2016
	dec	ch ; count down for 11 bytes dir entry limit
	jz	short stop_convert_file_x
	inc	edi
inc_and_loop:
	dec	cl ; count down for 12 bytes filename limit 
	jz	short stop_convert_file	
	inc	esi
	;;(ecx <= 12)
	;;loop	loc_get_fchar
	jmp	short loc_get_fchar

stop_convert_file:
	; 06/03/2016
	xor	ch, ch
	; ECX < 256 ; 'find_first_file' -> xor cl, cl
stop_convert_file_x:
	pop	edi
	pop	esi
	retn
%else
	; 18/05/2025 - TRDOS 386 v2.0.10
	;
	; *.ext -> 8 bytes ? then '.ext'
	; name.* -> 3 bytes ?
	; *.* -> 11 bytes '? -> '???????????'
	;
	; (modified for METACOMPARE procedure)

	mov	byte [AmbiguousFileName], 0 ; reset

	;mov	ecx, 8
	mov	cl, 8
check_nch:
	lodsb

	cmp	al, 20h
	jna	short convert_ok

	cmp	al, '.'
	je	short convert_ext
check_s:
	cmp	al, '*'
	jne	short not_star

	or	byte [AmbiguousFileName], 4

	mov	al, '?'
	stosb
	dec	ecx
	jz	short check_ext
	push	ecx
	rep	stosb
	pop	ecx

check_dot:
	lodsb
	cmp	al, '.'
	je	short convert_ext
	loop	check_dot
	jmp	short convert_ok

not_star:
	cmp	al, '?'
	jne	short check_char_ucase
	stosb
	or	byte [AmbiguousFileName], 1
	loop	check_nch
	jmp	short check_ext

check_char_ucase:
	call	simple_ucase
	stosb
	loop	check_nch

check_ext:
	lodsb
	cmp	al, '.'
	;je	short convert_ext
	;retn
	jne	short convert_ok

convert_ext:
	mov	cl, 3
convert_ext_@:
	lodsb
	cmp	al, 20h
	jna	short convert_ok

	cmp	al, '.'
	je	short convert_ok

check_s_@:
	cmp	al, '*'
	jne	short not_star_@

	or	byte [AmbiguousFileName], 8

	mov	al, '?'
	rep	stosb

	jmp	short convert_ok

not_star_@:
	cmp	al, '?'
	jne	short check_char_ucase_@

	or	byte [AmbiguousFileName], 2

	stosb
	loop	convert_ext_@

convert_ok:
	pop	edi
	pop	esi
	retn

check_char_ucase_@:
	call	simple_ucase
	stosb
	loop	convert_ext_@
	jmp	short convert_ok

	; 18/05/2025 - TRDOS 386 v2.0.10
simple_ucase:
	cmp	al, 61h ; 'a'
	jb	short simple_ucase_skip
	cmp	al, 7Ah ; 'z'
	ja	short simple_ucase_skip
	and	al, 0DFh
simple_ucase_skip:
	retn
%endif

save_longname_sub_component:
	; 25/05/2025
	; 20/05/2025 (TRDOS 386 v2.0.10)
	;	-Major Modification-
	; 13/02/2016
	; 06/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 28/02/2010
	; 17/10/2009
	; INPUT ->
	;	EDI = Directory Entry
	;     	// This procedure is called
	;	// from 'find_directory_entry' procedure.
	;	// If the last entry returns with
	;	// a non-zero LongnameFound value and
	;	// if LFN_CheckSum value is equal to
	;	// the next shortname checksum,
	;	// long name is valid.
	;	// If a longname is longer than 65 bytes,
	;	// it is invalid for trdos. (>45h)
	;
	; 20/05/2025 - TRDOS 386 v2.01.0
	;	According to
	;	Microsoft FAT32 File System Specification
	;   ... Long names are limited to 255 chars,
	;	(UCS-2, 2-byte character set)
	;	not including the trailing NUL. ...
	;   ... Total path length of can not exceed
	;	260 chars including the trailing NUL. ...
	;	(520 bytes)
	;   So, in this version of TRDOS 386
	;	UNICODE chars (two bytes) wil be converted
	;	to ASCII chars only using low bytes of
	;	of two byte UNICODE chars,
	;	as a very simple method.
	;	(if the high byte is not zero,
	;	 low byte will be returned as '_')
	;
	;	! 25/05/2025 !
	;	TRDOS 386 v2.1 (v2.0.10) will use only
	;	260 bytes (130 2-byte characters) buffer
	;	for FAT LFN type long names. And TRDOS 386
	;	kernel will ignore long file names if the
	;	length is longer/more than 260 bytes.
	;	(in other words: more than 10 LFN entries)
	;	And the kernel will use unicode to ascii
	;	conversion.. As result of the conversion,
	;	ASCIIZ equivalent of the long name
	;	will be returned with max. 128+NUL chars.
	;	(260 byte limit -> LDIR_Ord byte,
	;	"Last Long Entry" mark is 41h to 4Ah.)
	;
	;	(ASCIIZ name buffer size will be 129 bytes)
	;	((unicode LFN buffer will be 260 bytes))
	;
	; Modified registers: ECX ; 20/05/2025
	;

	push	edi
	push	esi
	;push	ebx
	;push	ecx
	;push	edx
	push	eax

	sub	ecx, ecx
	;sub	eax, eax
	
	; 25/05/2025
	mov	cl, 26
	
	movzx	eax, byte [edi] ; LDIR_Order
	
	;cmp	al, 41h  ; 40h (last long entry sign) + 1
	;jb	short pass_pslnsc_last_long_entry
	; 20/05/2025
	test	al, 40h
	jz	short pass_pslnsc_last_long_entry

	mov	ah, al
	sub	ah, 40h

	mov	[LFN_EntryLength], ah

	;cmp	al, 45h  ; 40h (last long entry sign) + 5
 		; Max 130 byte length is usable in TRDOS
	; 26*5 = 130
	; 20/05/2025 - TRDOS 386 v2.0.10
	cmp	al, 4Ah
	; 26*10 = 260
	ja	short loc_pslnsc_retn

	;and	al, 07h ; 0Fh
	; 20/05/2025
	;and	al, ~40h ; NOT 40h
	and	al, 0Fh
	mov	[LongNameFound], al

	dec	al
	; 25/05/2025
	;mov	cl, 26
	mul	cl

	; 25/05/2025
	;mov	esi, eax
	;add	esi, ecx
	;	; to make is an ASCIIZ string
	;	; with eax+26 bytes length
	;add	esi, LongFileName
	;mov	word [esi], 0
	
	; 25/05/2025
	; eax = offset from beginning of LongFileName 

	jmp	short loc_pslsc_move_ldir_name2

pass_pslnsc_last_long_entry:
	;cmp	al, 04h
	; 20/05/2025 - TRDOS 386 v2.0.10
	cmp	al, 09h
	ja	short loc_pslnsc_retn
	dec	byte [LongNameFound]
	cmp	al, [LongNameFound]
	jne	short loc_pslnsc_retn

loc_pslsc_move_ldir_name1:
	dec	al
	; 25/05/2025
	;mov	cl, 26
	mul	cl

loc_pslsc_move_ldir_name2:
	mov	cl, [edi+0Dh] ; long name checksum
	mov	[LFN_CheckSum], cl
	mov	esi, edi ; LDIR_Order
	mov	edi, LongFileName
	add	edi, eax
	inc	esi
	mov	cl, 5 ; chars 1 to 5
	rep	movsw
	
	add	esi, 3

	movsd	; char 6 & 7
	movsd	; char 8 & 9
	movsd	; char 10 & 11
	inc	esi
	inc	esi
	movsd   ; char 12 & 13

loc_pslnsc_retn:
 	pop	eax
	;pop	edx
	;pop	ecx
	;pop	ebx
	pop	esi
	pop	edi

    	retn

parse_path_name:
	; 03/06/2025
	; 19/05/2025 (TRDOS 386 v2.0.10)
	; 03/09/2024 (TRDOS 386 v2.0.9)
	; 09/08/2022
	; 29/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 10/02/2016
	; 08/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 10/009/2011 ('proc_parse_pathname')
	; 27/11/2009
	; 05/12/2004
	;
	; INPUT ->
	;	ESI = Beginning of ASCIIZ pathname string
	;       EDI = Destination Address
	;	      (which is TR-DOS FindFile data buffer)
	; OUTPUT ->
	;	CF = 1 -> Error
	;	     EAX = Error Code (AL)
	;
	; (Modified registers: eax, ecx, esi, edi)

	; Clear the pathname bytes in TR-DOS Findfile data buffer
	push	edi
	;mov	ecx, 20	; 80 bytes
	; 29/07/2022
	sub	ecx, ecx
	;mov	cl, 20
	; 19/05/2025 - TRDOS 386 v2.0.10
	;		; drv: 1 byte
	;		; dir: 104 bytes
	;		: name: 13 bytes
	;		: attrmask: 2 bytes (dword alignment)
	mov	cl, 30	; total 120 bytes, 120/4
	xor	eax, eax ; 0
	rep	stosd	; clear
	pop	edi

	mov	ax, [esi]
	cmp	ah, ':'
	je	short loc_ppn_change_drive
	mov	al, [Current_Drv]
	jmp	short pass_ppn_change_drive

pass_ppn_cdir:
	mov	esi, [First_Path_Pos]
	lodsb
loc_ppn_get_filename:
	;add	edi, 65 ; FindFile_Name location
	; 19/05/2025 - TRDOS 386 v2.0.10
	add	edi, 104 ; FindFile_Name location
	; TRDOS Filename length must not be more than 12 bytes
	;mov	ecx, 12
	mov	cl, 12
loc_ppn_get_fnchar_next:
	stosb
	lodsb
	cmp	al, 21h
	jb	short loc_ppn_clc_return
        loop    loc_ppn_get_fnchar_next
loc_ppn_return:
	retn

loc_ppn_change_drive:
	; 29/07/2022
	; ecx = 0
	and	al, 0DFh
	sub	al, 'A' ; A:
	jc	short loc_ppn_invalid_drive
	cmp	[Last_DOS_DiskNo], al
	jb	short loc_ppn_invalid_drive

	inc	esi
	inc	esi
	mov	ah, [esi]
	cmp	ah, 21h
	jnb	short pass_ppn_change_drive

loc_ppn_cmd_failed:
	; File or directory name is not existing
	mov	[edi], al ; Drv 
	mov	ax, 1 ; eax = 1
	; TR-DOS Error Code 01h = Bad Command Argument
	; MS-DOS Error Code 01h : Invalid Function Number
	;stc
	; (MainProg ErrMsg: "Bad command or file name!")
	retn

pass_ppn_change_drive:
	mov	[First_Path_Pos], esi
	;mov	dword [Last_Slash_Pos], 0
	; 29/07/2022
	mov	[Last_Slash_Pos], ecx ; 0
	stosb	; drive number (0 = 'A:', 2 = 'c:') 
	mov	al, [esi]
loc_scan_ppn_dslash:
	cmp	al, '/'
  	jne	short loc_scan_next_slash_pos
	mov	[Last_Slash_Pos], esi
loc_scan_next_slash_pos:
	inc	esi
	mov	al, [esi]
	cmp	al, 20h
	ja	short loc_scan_ppn_dslash
	;cmp	dword [Last_Slash_Pos], 0
	; 09/08/2022
	;cmp	[Last_Slash_Pos], ecx ; 0 ?
	;jna	short pass_ppn_cdir

	mov	ecx, [Last_Slash_Pos]
	; 03/09/2024
	; jcxz	pass_ppn_cdir
	; 03/06/2025
	jecxz	pass_ppn_cdir
	mov	esi, [First_Path_Pos]
	sub	ecx, esi
	inc	ecx
	;;cmp	ecx, 64
	;cmp	cl, 64
	; 19/05/2025 - TRDOS 386 v2.0.10
	cmp	cl, 103
	ja	short loc_ppn_invalid_drive_stc

	mov	eax, edi ; Dest Dir String Location (104 bytes)
			 ; ((It was 65 bytes before v2.0.10))
	rep	movsb
	;mov	[edi], cl ; 0, End of Dir String
	mov	esi, [Last_Slash_Pos]
	inc	esi
	mov	edi, eax
	lodsb
	cmp	al, 21h
	jnb	short loc_ppn_get_filename
loc_ppn_clc_return:
	;clc
	xor	eax, eax
	retn

loc_ppn_invalid_drive_stc:
	cmc	 ; stc
loc_ppn_invalid_drive:
	; cf = 1
	; The Drive Letter/Char < "A" or > "Z"
	mov	ax, 0Fh
	; MS-DOS Error Code 0Fh = Disk Drive Invalid
	; (MainProg ErrMsg: "Drive not ready or read error!")
	retn

find_longname:
	; 20/05/2025 - TRDOS 386 v2.0.10
	;	-verified-
	; 29/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 13/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 24/01/2010 (DIR.ASM, 'proc_find_longname')
	; 17/10/2009

	; INPUT ->
	;	ESI = DOS short file name address
	; 	for example: "filename.ext"
	;
	; OUTPUT ->
	; 	ESI = ASCIIZ longname address (cf = 0)
	;	cf = 1 -> error number returns in EAX (AL)
	;	AL = 0 & CF=1 -> longname not found
	;	     the file/directory has no longname
	; 	cf = 0 -> AL = FAT Type 

	; 17/10/2009
	; ASCIIZ string will be returned
	; as LongFileName
	; clearing/reset is not needed
	;mov	ecx, 33
	;mov	edi, LongFileName
	;sub	ax, ax ; 0
	;rep	stosw

	;mov	byte [LongNameFound], 0

	; ESI = ASCIIZ file/directory name address
	;   AL = Attributes AND mask 
	;	(Result of AND must be equal to AL)
	;   AH = Negative attributes mask
	;	(Result of AND must be ZERO)
	mov	ax, 0800h 
		; it must not be volume name or longname
	call	find_first_file
	jc	short loc_fln_retn

loc_fln_check_FAT_Type:
	cmp	byte [Current_FATType], 1
	jnb	short loc_fln_check_longname_yes_sign

	;call	get_fs_longname
	;retn
	; 29/07/2022
	jmp	get_fs_longname

loc_fln_check_longname_yes_sign:
	or	bh, bh
	jnz	short loc_fln_check_longnamefound_number
loc_fln_longname_not_found_retn:
	xor	eax, eax 
	; cf = 1 & al = 0 -> longname not found
	stc
loc_fln_retn:
	retn

	; 20/05/2025 - TRDOS 386 v2.0.10
validate_long_name:	; called from SysFFF ; 20/05/2025
loc_fln_check_longnamefound_number:
	; 'LongNameFound' is set by
        ; by 'save_longname_sub_component'
	; which is called from
	; 'find_directory_entry'
	; which is called from
	; 'find_first_file'
	; It must 1 if the longname is valid
        cmp     byte [LongNameFound], 1
	jne	short loc_fln_longname_not_found_retn

loc_fln_calculate_checksum: 
	call	calculate_checksum
	; AL = shortname checksum

loc_fln_longname_validation:
	; 'LFN_CheckSum' has been set already
	; by 'save_longname_sub_component'
	; which is called from
	; 'find_directory_entry'
	; which is called from
	; 'find_first_file'
	cmp	[LFN_CheckSum], al
	jne	short loc_fln_longname_not_found_retn

	mov	esi, LongFileName
	mov	al, [Current_FATType]
	retn

calculate_checksum:
	; 20/05/2025 - TRDOS 386 v2.0.10
	;	-verified-
	; 29/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 13/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 17/10/2009 (DIR.ASM, 'proc_calculate_checksum')
        ;
	; INPUT ->
	;	ESI = 11 byte DOS File Name location
	;	(in DOS Directory Entry Format)
	; OUTPUT ->
	;	 AL = 8 bit checksum (CRC) value
	;
	; (Modified registers: EAX, ECX, ESI)

	; Erdogan Tan [ 17-10-2009 ]
	;  'ror al, 1' instruction

	; Erdogan Tan [ 20-06-2004 ]
	; This 8086 assembly code is an original code
	; which is adapted from C code in
	; Microsoft FAT32 File System Specification
	; Version 1.03, December 6, 2000
	; Page 28

	xor	al, al
	;mov	ecx, 11
	; 29/07/2022
	sub	ecx, ecx
	mov	cl, 11
loc_next_sum:
	;xor	ah, ah
	;test	al, 1
	;jz	short pass_ah_80h
	;mov	ah, 80h
;pass_ah_80h:
	;shr	al, 1
	ror	al, 1 ; 17/10/2009
	add	al, [esi]
	inc	esi
	;add	al, ah
	loop	loc_next_sum
	retn

get_fs_longname:
	; temporary (13/02/2016)
	xor	eax, eax
	stc
	retn

make_sub_directory:
	; 07/08/2022
	; 29/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 16/10/2016
	; 02/03/2016, 03/03/2016
	; 26/02/2016, 27/02/2016
	; 21/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 01/08/2011 (DIR.ASM, 'proc_make_directory')
	; 10/07/2010
	; INPUT ->
	; 	ESI = ASCIIZ Directory Name
	;	CL = Directory Attributes
	; OUTPUT ->
	;	EAX = New sub dir's first cluster
	;	ESI = Logical Dos Drv Descr. Table Addr.
	;	CF = 1 -> error code in AL (EAX)

	;test	cl, 10h  ; directory
	;jz	short loc_make_directory_access_denied
	;test	cl, 08h ; volume name
	;jnz	short loc_make_directory_access_denied

	and	cl, 07h
	mov	byte [mkdir_attrib], cl

	push	esi
	xor	ebx, ebx
	mov	bh, [Current_Drv]
	mov	esi, Logical_DOSDisks
	add	esi, ebx
	pop	ebx

	; 10/07/2010 -> 1st writable disk check for trdos
	; LD_DiskType = 0 for write protection (read only)
	cmp	byte [esi+LD_DiskType], 1 ; 0 = Invalid
	jnb	short loc_mkdir_check_file_sytem
	; 16/10/2016 (13h -> 30)
	;mov	eax, 30 ; 'Disk write-protected' error
	;mov	edx, 0
	; 29/07/2022
	sub	eax, eax
	mov	al, 30
	sub	edx, edx ; 0
	stc
	; err retn: EDX = 0, EBX = Dir name offset
	;ESI = Logical DOS drive description table address
	retn

;loc_make_directory_access_denied:
	;mov	ax, 05h ; access denied (invalid attributes input)
	;stc
	;retn

loc_mkdir_check_file_sytem:
	cmp	byte [esi+LD_FATType], 1
	jnb	short loc_mkdir_check_free_sectors

loc_make_fs_directory:
	mov	eax, [Current_Dir_FCluster]

	; EAX = Parent directory DDT Address
	; ESI = Logical DOS Drive DT Address
	; EBX = Directory name offset (as ASCIIZ name)

	;call	make_fs_directory
	;retn
	; 29/07/2022
	jmp	make_fs_directory

loc_mkdir_check_free_sectors:
        movzx   eax, byte [esi+LD_BPB+SecPerClust]
	mov	ecx, [esi+LD_FreeSectors]
	cmp	ecx, eax
	jb	short loc_mkdir_insufficient_disk_space

loc_make_fat_directory:
	mov	[mkdir_DirName_Offset], ebx
	mov	[mkdir_FreeSectors], ecx

	;mov	al, [esi+LD_BPB+SecPerClust]
	mov	byte [mkdir_SecPerClust], al

loc_mkdir_gffc_1:
	call	get_first_free_cluster
	jc	short loc_mkdir_gffc_retn

;loc_mkdir_gffc_1_cont: 
	;cmp	eax, 2
	;jb	short loc_mkdir_gffc_insufficient_disk_space

;loc_mkdir_gffc_1_save_fcluster:  
	mov	[mkdir_FFCluster], eax

loc_mkdir_locate_ffe:
	; Current directory fcluster <> Directory buffer cluster
	; Current directory will be reloaded by
	; 'locate_current_dir_file' procedure
	;
	; ESI = Logical DOS Drive Description Table Address
	;push	esi ; 27/02/2016
	xor	eax, eax
        mov	ecx, eax
	dec	cx ; FFFFh  
	; CX = FFFFh -> find first deleted or free entry
	; ESI would be ASCIIZ filename address if the call
	; would not be for first free or deleted dir entry
	call	locate_current_dir_file
	jnc	short loc_mkdir_set_ff_dir_entry_1
	;pop	esi
	; ESI = Logical DOS Drive Description Table Address
	cmp	eax, 2  ; cmp al, 2 ; File/Dir not found !
	jne	short loc_mkdir_stc_return

loc_mkdir_add_new_cluster:
	cmp	byte [Current_FATType], al ; 2
	;cmp	byte ptr [esi+LD_FATType], 2
	ja	short loc_mkdir_add_new_cluster_check_fsc
	cmp	byte [Current_Dir_Level], 1
	;cmp	byte [esi+LD_CDirLevel], 1
	jnb	short loc_mkdir_add_new_cluster_check_fsc

	mov	al, 12 ; No more files 
loc_mkdir_gffc_retn:
	retn

loc_mkdir_add_new_cluster_check_fsc:
	mov	ecx, [mkdir_FreeSectors]
	;movzx	eax, byte [mkdir_SecPerClust]
	mov	al, [mkdir_SecPerClust]
	;shl	ax, 1 ; AX = 2 * AX
	; 29/07/2022
	shl	eax, 1
	cmp	ecx, eax
	jnb	short loc_mkdir_add_new_subdir_cluster

loc_mkdir_insufficient_disk_space:
	;mov	edx, ecx
;loc_mkdir_gffc_insufficient_disk_space:
	; 29/07/2022
	;mov	ax, 27h ; MSDOS err => insufficient disk space

	; err retn: EDX = Free sectors, EBX = Dir name offset
        ; ESI -> Dos drive description table address
	;; ecx = edx

	;retn

	; 29/07/2022
	xor	ah, ah
	mov	al, 27h

loc_mkdir_stc_return:
	stc
	retn

loc_mkdir_gffc_2:
	call	get_first_free_cluster
	jc	short loc_mkdir_gffc_retn

;loc_mkdir_gffc_1_cont:
	;cmp	eax, 2
	;jb	short loc_mkdir_gffc_insufficient_disk_space

;loc_mkdir_gffc_2_save_fcluster:
	mov	[mkdir_FFCluster], eax

	mov	eax, [mkdir_LastDirCluster]

	call	load_FAT_sub_directory
	jc	short loc_mkdir_gffc_retn

	xor	edi, edi
loc_mkdir_set_ff_dir_entry_1:
	; 27/02/2016
	push	esi ; Logical DOS Drv Desc. Tbl. address
	; EDI = Directory Entry Address
	mov	esi, [mkdir_DirName_Offset]
	mov	eax, [mkdir_FFCluster]

	mov	cx, 10h	; CL = Directory attribute
			; CH = 0 -> File size is 0
	or	cl, [mkdir_attrib] ; S, H, R
	call	make_directory_entry

	pop	esi

	mov	byte [DirBuff_ValidData], 2
	call	save_directory_buffer
        ;jnc	loc_mkdir_set_ff_dir_entry_2
	; 29/07/2022
	jc	short loc_mkdir_return
	jmp	loc_mkdir_set_ff_dir_entry_2

loc_mkdir_return:
	retn

loc_mkdir_add_new_subdir_cluster:
	mov	edx, [DirBuff_Cluster]
	mov	[mkdir_LastDirCluster], edx

	mov	eax, [mkdir_FFCluster]
	call	load_FAT_sub_directory
	jc	short loc_mkdir_return
	; eax = 0
	; ecx = directory buffer sector count (<= 128)

pass_mkdir_add_new_subdir_cluster:

; 29/07/2022
;	;sub	edi, edi ; 0
;	; 29/07/2022 - BUGFIX !
;	mov	edi, Directory_Buffer
;
;	;mov	al, 128 ; double word
;	;mul	ecx ; ecx =  directory buffer sector count
;	;mov	ecx, eax
;	;shl	cx, 7 ; 128 * sector count
;	mov	ax, [esi+LD_BPB+BytesPerSec] ; 512
;	;shr	ax, 2 ; 'byte count / 4' for 'stosd'
;	; 29/07/2022
;	shr	eax, 2
;	;mul	cx ; max = 128*(512/4) -> 16384 (stosd)
;	;mov	cx, ax
;	;sub	ax, ax ; 0
;	; 29/07/2022
;	mul	ecx
;	mov	ecx, eax
;	sub	eax, eax
;	rep	stosd ; clear directory buffer

	; 29/07/2022
	call	clear_directory_buffer

	mov	byte [DirBuff_ValidData], 2
	call	save_directory_buffer 
	jc	short loc_mkdir_return

loc_mkdir_save_added_cluster:
	mov	eax, [mkdir_LastDirCluster]
	mov	ecx, [mkdir_FFCluster]
	; 01/03/2016
	xor	edx, edx
	mov	[FAT_ClusterCounter], edx ; 0 ; reset
	call	update_cluster
	jnc	short loc_mkdir_save_fat_buffer_0
	or	eax, eax ; EAX = 0 -> cluster value is 0 or eocc
	jnz	short loc_mkdir_save_fat_buffer_stc_retn

loc_mkdir_save_fat_buffer_0:
	mov	eax, [mkdir_FFCluster]
	mov	[mkdir_LastDirCluster], eax

	xor	ecx, ecx
	dec	ecx ; FFFFFFFFh
	; ESI = Logical DOS Drive Description Table address
	call	update_cluster
	jnc	short loc_mkdir_save_fat_buffer_1
	or	eax, eax
	jz	short loc_mkdir_save_fat_buffer_1

loc_mkdir_save_fat_buffer_stc_retn:
	; 01/03/2016
	cmp	byte [FAT_ClusterCounter], 1
	jb	short loc_mkdir_save_fat_buffer_retn

	mov	bx, 0FF00h ; recalculate free space (BL = 0)
			   ; (BH = FFh -> Use ESI as Drv Param. Tbl.)
	push	eax
	call	calculate_fat_freespace
	pop	eax
	stc
loc_mkdir_save_fat_buffer_retn:
	retn

loc_mkdir_save_fat_buffer_1:
	; byte [FAT_BuffValidData] = 2
	call	save_fat_buffer
	jc	short loc_mkdir_save_fat_buffer_stc_retn

	; 01/03/2016
	cmp	byte [FAT_ClusterCounter], 1
	jb	short loc_mkdir_save_fat_buffer_2

	; ESI = Logical DOS Drive Description Table address
	mov	eax, [FAT_ClusterCounter]
	mov	bx, 0FF01h ; add free clusters
	call	calculate_fat_freespace

	;inc	eax ; 0FFFFFFFFh -> 0 ; recalculation is needed!
	;jnz	short loc_mkdir_save_fat_buffer_2

	; ecx > 0 -> Recalculation is needed
	or	ecx, ecx 
	jz	short loc_mkdir_save_fat_buffer_2

	mov	bx, 0FF00h ; ; recalculate free space
	call	calculate_fat_freespace

loc_mkdir_save_fat_buffer_2:
	mov	byte [mkdir_add_new_cluster], 1
	jmp	loc_mkdir_upd_parent_dir_lmdt

loc_mkdir_update_sub_dir_cluster:
	mov	eax, [mkdir_FFCluster]
	sub	ecx, ecx ; 0
	; 01/03/2016
	mov	[FAT_ClusterCounter], ecx ; 0 ; Reset
	dec	ecx ; 0FFFFFFFFh

	; ESI = Logical DOS Drive Descisption Table address
	call	update_cluster
	jnc	short loc_mkdir_save_fat_buffer_3
	or	eax, eax ; EAX = 0 -> cluster value is 0 or eocc
	jz	short loc_mkdir_save_fat_buffer_3
	; 01/03/2016
	jmp	short loc_mkdir_save_fat_buffer_stc_retn

loc_mkdir_set_ff_dir_entry_2:
	; ESI = Logical DOS Drive Description Table address
	mov	eax, [mkdir_FFCluster]
	; Load disk sectors as a directory cluster
	call	load_FAT_sub_directory
	jc	short retn_make_fat_directory

	; eax = 0
	; ecx = directory buffer sector count (<= 128)

; 29/07/2022
;	mov	edi, Directory_Buffer + 64 ; 26/02/2016
;
;	; 02/03/2016
;	mov	ax, [esi+LD_BPB+BytesPerSec] ; 512
;	;shr	ax, 2 ; 'byte count / 4' for 'stosd'
;	; 29/07/2022
;	shr	eax, 2
;	mul 	ecx
;	mov	ecx, eax
;	;
;	; 29/07/2022 - BUGFIX !
;	sub	ecx, 16 ; - 64 bytes
;			; (space for '.' & '..' entries)
;	;sub	ax, ax
;	sub	eax, eax
;	rep	stosd
;
;	;;mov	al, 128 ; double word (count in sector)
;	;;mul	ecx ; ecx = directory buffer sector count
;	;;mov	ecx, eax
;	;shl	cx, 7 ; 128 * sector count
;	;sub	ecx, 64 ; 29/07/2022
;	;;sub	eax, eax
;	;;sub	al, al ; 0
;	;rep	stosd ; clear directory buffer

	; 29/07/2022
	call	clear_directory_buffer

	mov	edi, Directory_Buffer ; 26/02/2016

	push	esi

	mov	esi, mkdir_Name
	mov	word [esi], 2Eh ; db '.', '0'

	mov	eax, [mkdir_FFCluster]
	;mov	cx, 10h ; CL = Directory attribute
			; CH = 0 -> File size is 0
	; 29/07/2022
	mov	cl, 10h
	call	make_directory_entry

	mov	edi, Directory_Buffer + 32 ; 26/02/2016

	; 03/03/2016
	; Following modification has been done according to
	; 'Microsoft Extensible Firmware Initiative
	; FAT32 File System Specification' document,
	; 'FAT: General Overview of On-Disk Format—Page 25'.
	; "Finally, you set DIR_FstClusLO and DIR_FstClusHI
	; for the dotdot entry (the second entry) to the
	; first cluster number of the directory in which you
	; just created the directory (value is 0 if this directory
	; is the root directory even for FAT32 volumes)."
	; (Correctness of this modification has been verified
	;  by using Windows 98 'scandisk.exe'.)

	sub	eax, eax
	cmp	byte [Current_Dir_Level], al ; 0
	jna	short loc_mkdir_set_ff_dir_entry_3
	mov	eax, [Current_Dir_FCluster] ; parent dir
loc_mkdir_set_ff_dir_entry_3:
	mov	word [esi+1], 2Eh ; db '.', '0'

	;mov	cx, 10h
	call	make_directory_entry

	pop	esi

	mov	byte [DirBuff_ValidData], 2
	call	save_directory_buffer
	;jnc	loc_mkdir_update_sub_dir_cluster
	; 29/07/2022
	jc	short retn_make_fat_directory
	jmp	loc_mkdir_update_sub_dir_cluster

retn_make_fat_directory:
	retn

loc_mkdir_save_fat_buffer_3:
	; 01/03/2016
	; byte [FAT_BuffValidData] = 2
	call	save_fat_buffer
	;jc	short loc_mkdir_save_fat_buffer_stc_retn
	; 07/08/2022
	jnc	short loc_mkdir_save_fat_buffer_4
	jmp	loc_mkdir_save_fat_buffer_stc_retn

loc_mkdir_save_fat_buffer_4:
	cmp	byte [FAT_ClusterCounter], 1
	jb	short loc_mkdir_save_fat_buffer_5

	; ESI = Logical DOS Drive Description Table address
	mov	eax, [FAT_ClusterCounter]
	mov	bx, 0FF01h ; add free clusters
	call	calculate_fat_freespace

	;inc	eax ; 0FFFFFFFFh -> 0 ; recalculation is needed!
        ;jnz    short loc_mkdir_save_fat_buffer_5

	; ecx > 0 -> Recalculation is needed
	or	ecx, ecx
        jz      short loc_mkdir_save_fat_buffer_5

	mov	bx, 0FF00h ; recalculate free space
	call	calculate_fat_freespace

loc_mkdir_save_fat_buffer_5:
	mov	byte [mkdir_add_new_cluster], 0

loc_mkdir_upd_parent_dir_lmdt:
	call	update_parent_dir_lmdt

	; 01/03/2016
	cmp	byte [mkdir_add_new_cluster], 0
	;ja	loc_mkdir_gffc_2
	; 29/07/2022
	jna	short loc_mkdir_retn_new_dir_cluster
	jmp	loc_mkdir_gffc_2

loc_mkdir_retn_new_dir_cluster:
	mov	eax, [mkdir_FFCluster]
	xor	edx, edx
loc_mkdir_retn:
	retn

clear_directory_buffer:
	; 29/07/2022 (TRDOS 386 Kernel v2.0.5)
	;
	; eax = 0
	; ecx = directory buffer sector count (<= 128)
	;
	mov	edi, Directory_Buffer
	;mov	al, 128 ; double word
	;mul	ecx ; ecx = directory buffer sector count
	;mov	ecx, eax
	;shl	cx, 7 ; 128 * sector count
	mov	ax, [esi+LD_BPB+BytesPerSec] ; 512
	shr	eax, 2 ; 'byte count / 4' for 'stosd'
	mul	ecx ; max = 128*(512/4) -> 16384 (stosd)
	mov	ecx, eax
	sub	eax, eax
	rep	stosd ; clear directory buffer
	retn

make_directory_entry:
	; 29/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 02/03/2016
	; 21/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 09/08/2010 (DIR.ASM, 'proc_make_directory_entry')
	; 17/07/2010
	; INPUT ->
	; 	EDI = Directory Entry Address
	;	ESI = Dot File Name Location
	;	EAX = First Cluster
	;	File Size = 0 (Must be set later)
	;	CL = Attributes
	;	CH = 0 (File size = 0)
	;	(If CH>0, File size is in dword [EBX]) (*)
	; OUTPUT ->
	;	EDI = Directory Entry Address
	;	ESI = Dot File Name Location (Capitalized)
	;	If CH input = 0, File Size = 0
	;	Otherwise file size is as dword [EBX] (*)
	;	DX = Date, AX = Time in DOS Dir Entry format
	;	EBX = same
	;	ECX = same

	push	ecx

	mov	[edi+11], cl ; Attributes
	mov	[edi+26], ax ; FClusterLw, 26
	shr	eax, 16
	mov	[edi+20], ax ; FClusterHw, 20
	;xor	ax, ax
	; 29/07/2022
	xor	eax, eax
	mov	[edi+12], ax ; NTReserved, 12
			     ; CrtTimeTenth, 13
	or	ch, ch
	jz	short loc_make_direntry_set_filesize

	mov	eax, [ebx]

loc_make_direntry_set_filesize:
	mov	[edi+28], eax ; FileSize, 28

	call	convert_file_name
	; EDI = Dir Entry Format File Name Location
	; ESI = Dot File Name Location (capitalized)

	call	convert_current_date_time
	; OUTPUT -> DX = Date in dos dir entry format
        ; 	    AX = Time in dos dir entry format
	mov	[edi+14], ax ; CrtTime, 14
	mov	[edi+16], dx ; CrtDate, 16
	mov	[edi+18], dx ; LastAccDate, 18
	mov	[edi+22], ax ; WrtTime, 14
	mov	[edi+24], dx ; WrtDate, 16
	pop	ecx

	retn

convert_current_date_time:
	; 29/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 21/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 13/06/2010 (DIR.ASM, 'proc_convert_current_date_time')
	; converts date&time to dos dir entry format
	; INPUT -> none
	; OUTPUT -> DX = Date in dos dir entry format
	;           AX = Time in dos dir entry format

	mov	ah, 04h ; Return Current Date
	call	int1Ah

	mov	al, ch ; <- century BCD
	and	al, 0Fh
	mov	ah, ch
	shr	ah, 4
	aad
	mov	ch, al ; -> century

	mov	al, cl ; <- year BCD
	and	al, 0Fh
	mov	ah, cl
	shr	ah, 4
	aad
	mov	cl, al ; -> year

	;mov	al, ch
	;mov	ah, 100
	;mul	ah
	; 29/07/2022
	mov	al, 100
	mul	ch

	;xor	ch, ch
	;add	ax, cx
	; 29/07/2022
	add	al, cl
	adc	ah, 0
	sub	ax, 1980 ; ms-dos epoch
	;mov	cx, ax
	; 29/07/2022
	mov	cl, al
	;mov	ecx, eax

	mov	al, dh ; <- month in bcd
	and	al, 0Fh
	mov	ah, dh
	shr	ah, 4
	aad
	mov	dh, al ; -> month

	mov	al, dl ; <- day BCD
	and	al, 0Fh
	mov	ah, dl
	shr	ah, 4
	aad
	mov	dl, al ; -> day

	mov	al, cl ; count of years from 1980
	;shl	ax, 4
	; 29/07/2022
	;mov	eax, ecx
	shl	eax, 4

	or	al, dh ; month of year, 1 to 12
	;shl	ax, 5
	; 29/07/2022
	shl	eax, 5
	or	al, dl ; day of year, 1 to 31

	;push	ax ; push date
	; 29/07/2022
	push	eax

	mov	ah, 02h ; Return Current Time
	call	int1Ah

	mov	al, ch ; <- hours BCD
	and	al, 0Fh
	mov	ah, ch
	shr	ah, 4
	aad
	mov	ch, al ; -> hours

	mov	al, cl ; <- minutes BCD
	and	al, 0Fh
	mov	ah, cl
	shr	ah, 4
	aad
	mov	cl, al ; -> minutes

	mov	al, dh ; <- seconds BCD
	and	al, 0Fh
	mov	ah, dh
	shr	ah, 4
	aad
	mov	dh, al ; -> seconds

	mov	al, ch ; hours
	;shl	ax, 6
	; 29/07/2022
	shl	eax, 6
	or	al, cl ; minutes
	;shl	ax, 5
	; 29/07/2022
	shl	eax, 5
	shr	dh, 1 ; 2 seconds
	; There is a bug in TRDOS v1 here !
	; it was 'or al, dl' !
	or	al, dh ; seconds

	;pop	dx ; pop date
	; 29/07/2022
	pop	edx

	retn

save_directory_buffer:
	; 30/07/2022
	; 29/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 15/10/2016
	; 23/03/2016
	; 26/02/2016
	; 22/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 01/08/2011
	; 14/03/2010
	; INPUT ->
	; 	 none
	; OUTPUT ->
	;  cf = 0 -> write OK...
	;  cf = 1 -> error code in AL (EAX)
	;  cf = 1 & AL = 0Dh => CH & CL = FS & FAT type
	;  EBX = Directory Buffer Address
	;
	;  (EAX, ECX, EDX will be modified)

	mov	ebx, Directory_Buffer
	cmp	byte [DirBuff_ValidData], 2
	je	short loc_save_dir_buffer
	xor	eax, eax
	retn

loc_save_dir_buffer:
	push	esi
	xor	ebx, ebx
        mov     bh, [DirBuff_DRV]
	sub	bh, 'A'
        mov     esi, Logical_DOSDisks
	add	esi, ebx
        mov     cx, [esi+LD_FATType]
	; CH = FS Type (A1h for FS)
	; CL = FAT Type (0 for FS)
	or	cl, cl
	jz	short loc_save_dir_buff_stc_retn

loc_save_dir_buffer_check_cluster_no:
	mov	eax, [DirBuff_Cluster]
	sub	bh, bh ; ebx = 0
	or	eax, eax
	jnz	short loc_save_sub_dir_buffer
	mov	ah, [DirBuff_FATType]
	inc	bl ;  bl = 1
	cmp	ah, bl
	jb	short loc_save_dir_buff_inv_data_retn
	inc	bl ; bl = 2
	cmp	bl, ah
	jb	short loc_save_dir_buff_inv_data_retn

loc_save_root_dir_buffer:
	mov	bx, [esi+LD_BPB+RootDirEnts]
	add	bx, 15
	;shr	bx, 4 ; 16 dir entries per sector
	; 29/07/2022
	shr	ebx, 4
	;or	bx, bx
	or	ebx, ebx
	jz	short loc_save_dir_buff_stc_retn
	;mov	ecx, ebx
	mov	eax, [esi+LD_ROOTBegin] ; 26/02/2016
	jmp	short loc_write_directory_to_disk

loc_save_dir_buff_stc_retn:
	stc
loc_save_dir_buff_inv_data_retn:
	; 15/10/2016 (0Dh -> 29)
	mov	al, 29 ; Invalid data !
	mov	byte [DirBuff_ValidData], 0
	jmp	short loc_save_dir_buff_retn

loc_write_directory_to_disk_err:
	; 15/10/2016 (disk write error code, 1Dh -> 18)
	mov	eax, 18 ; Drive not ready or write error

loc_save_dir_buff_retn:
	mov	ebx, Directory_Buffer
	pop	esi
	retn

loc_save_sub_dir_buffer:
	; ebx  = 0
	;sub	eax, 2
	; 30/07/2022
	dec	eax
	dec	eax
	mov	bl, [esi+LD_BPB+SecPerClust]
	mul	ebx
        add     eax, [esi+LD_DATABegin]
 	;mov	ecx, ebx

loc_write_directory_to_disk:
 	mov	ecx, ebx
	mov	ebx, Directory_Buffer
	call	disk_write
	jc	short loc_write_directory_to_disk_err

loc_save_dir_buff_validate_retn:
	mov	byte [DirBuff_ValidData], 1
	xor	eax, eax
	; 26/02/2016
	jmp	short loc_save_dir_buff_retn

update_parent_dir_lmdt:
	; 29/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 29/12/2017
	; 22/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 01/08/2011
	; 16/10/2010
	;
	; INPUT ->
	;	none
 	; OUTPUT ->
	;	(last modification date & time of the parent dir
	;	will be changed/updated)
	;
	; (EAX, EBX, ECX, EDX, EDI will be changed)

	sub	eax, eax
	mov	ah, [Current_Dir_Level]
	mov	al, [Current_FATType]
	cmp	al, 1
	jb	short loc_UPDLMDT_proc_retn

loc_update_parent_dir_lm_date_time:
	or	ah, ah
	jz	short loc_UPDLMDT_proc_retn

	push	esi ; *
	mov	[UPDLMDT_CDirLevel], ah
	mov	edx, [Current_Dir_FCluster]
	mov	[UPDLMDT_CDirFCluster], edx

	dec	ah
	;mov	ecx, 12
        ; 29/07/2022
	sub	ecx, ecx
	mov	cl, 12
	;
	mov     esi, PATH_Array

	mov	[Current_Dir_Level], ah
	or	ah, ah
	jnz	short loc_update_parent_dir_lmdt_load_sub_dir_1
	cmp	byte [Current_FATType], 2
	ja	short loc_update_parent_dir_lmdt_load_sub_dir_2
	sub	al, al ; eax = 0
	jmp	short loc_update_parent_dir_lmdt_load_sub_dir_3

loc_UPDLMDT_proc_retn:
	retn

loc_update_parent_dir_lmdt_load_sub_dir_1:
	mov	al, 16
	mul	ah
	add	esi, eax

loc_update_parent_dir_lmdt_load_sub_dir_2:
	mov	eax, [esi+12] ; Parent Dir First Cluster

loc_update_parent_dir_lmdt_load_sub_dir_3:
	mov	[Current_Dir_FCluster], eax

	add	esi, 16
	mov	di, Dir_File_Name
	rep	movsb
	
	mov	esi, Logical_DOSDisks
	sub	ebx, ebx
	mov	bh, [Current_Drv]
	add	esi, ebx
	call	reload_current_directory
	jc	short loc_update_parent_dir_lmdt_restore_cdirlevel

loc_update_parent_dir_lmdt_locate_dir:
	mov	esi, Dir_File_Name
	;xor	cx, cx
	; 29/07/2022
	xor	ecx, ecx
	mov	ax, 0810h ; Only directories
        call    locate_current_dir_file
	; EDI = DirBuff Directory Entry Address
	jc	short loc_update_parent_dir_lmdt_restore_cdirlevel

	call	convert_current_date_time
	mov	[edi+18], dx ; Last Access Date
	mov	[edi+24], dx ; Last Write Date
	mov	[edi+22], ax ; Last Write Time

	mov	byte [DirBuff_ValidData], 2
	call	save_directory_buffer
	; 29/12/2017
	;jc	short loc_update_parent_dir_lmdt_restore_cdirlevel
	;xor	al, al
loc_update_parent_dir_lmdt_restore_cdirlevel:
 	;current directory level restoration
	mov	ah, [UPDLMDT_CDirLevel]
	mov	[Current_Dir_Level], ah
        mov     edx, [UPDLMDT_CDirFCluster]
	mov	[Current_Dir_FCluster], edx

	pop	esi ; *
	retn

delete_longname:
	; 29/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 27/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 01/08/2011 (DIR.ASM, 'proc_delete_longname')
	; 14/03/2010
	; INPUT ->
	; 	EAX = Directory Entry (Index) Number (< 65536)
	; OUTPUT ->
	;	cf = 0 -> OK  (EAX = 0)
	; 	cf = 1 -> error code in EAX (AL)
	;
	; (Modified registers: EAX, EDX, ECX, EBX, EDI)

	mov	[DLN_EntryNumber], ax
        mov     byte [DLN_40h], 40h

	call	locate_current_dir_entry
	jnc	short loc_dln_check_attributes
	retn

loc_dln_longname_not_found:
	;mov	eax, 2
	; 29/07/2022
	sub	eax, eax
	mov	al, 2
	stc
	retn

loc_dln_check_attributes:
	mov	al, 0Fh  ; long name
	mov	ah, [edi+0Bh] ; dir entry attributes
	cmp	ah, al
	jne	short loc_dln_longname_not_found
	mov	ah, [edi]
	sub	ah, [DLN_40h]
	jna	short loc_dln_longname_not_found
	cmp	ah, 14h ; 84-64=20 -> 20*13=260 bytes
	ja	short loc_dln_longname_not_found

	mov	byte [edi], 0E5h  ; deleted sign
	mov	byte [DirBuff_ValidData], 2 ; changed/write sign
	mov	byte [DLN_40h], 0 ; 40h -> 0

loc_dln_delete_next_ln_entry:
	cmp	ah, 1
	jna	short loc_dln_longname_retn
loc_dln_delete_next_ln_entry_0:
	inc	word [DLN_EntryNumber]
	movzx	eax, word [DLN_EntryNumber]
	call	locate_current_dir_entry
	jnc	short loc_dln_check_attributes

loc_dln_longname_stc_retn:
	retn

loc_dln_longname_retn:
	;cmp	byte [DirBuff_ValidData], 2
	;jne	short loc_dln_longname_retn_xor_eax
	call	save_directory_buffer
	jc	short loc_dln_longname_stc_retn

loc_dln_longname_retn_xor_eax:
	xor	eax, eax
	retn

locate_current_dir_entry:
	; 30/07/2022
	; 29/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 16/10/2016
	; 15/10/2016
	; 23/03/2016
	; 27/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 01/08/2011 (DIR.ASM, 'proc_locate_current_dir_entry')
	; 07/03/2010
	; INPUT ->
	;	EAX = Directory Entry (Index) Number (< 65536)
	; OUTPUT ->
	;	EDI = Directory Entry Address
	; 	EAX = Cluster Number of Directory Buffer
	;	EBX = Directory Buffer Entry Offset
	;	ECX = DirBuff Valid Data identifier (CL)
	;   	If CF = 0 and CL = 2 then
	;	   directory buffer modified and
	;	   must be written to disk.
	; 	If CF = 0  and CL = 1 then
	;	   dir buffer has been written to disk, already.
	;	CF = 1 -> Error code in EAX (AL)
	;
	; (Modified registers: EAX, EDX, ECX, EBX, EDI)

loc_locate_current_dir_entry:
	push	esi
	mov	ecx, eax
	;mov	edx, 32
	; 29/07/2022
	sub	edx, edx
	mov	dl, 32
	mul	edx 
	mov	[LCDE_ByteOffset], eax
	xor	ebx, ebx
	mov	bh, [Current_Drv]
        mov     al, [DirBuff_DRV]
	sub	al, 'A'
        mov     esi, Logical_DOSDisks
	add	esi, ebx
	cmp	bh, al
	;jne	loc_lcde_reload_current_directory
	; 29/07/2022
	je	short loc_lcde_cdl_check
	jmp	loc_lcde_reload_current_directory
loc_lcde_cdl_check:
	; 29/07/2022
	xor	eax, eax
	cmp	byte [Current_Dir_Level], 0
	ja	short loc_lcde_calc_dirbuff_cluster_offset
	; 27/02/2016
	; TRDOS v1 has bug here for FAT32 fs !
	; (Root Directory Entries for FAT32 = 0)
	cmp	byte [esi+LD_FATType], 3  ; FAT32
	jnb	short loc_lcde_calc_dirbuff_cluster_offset

loc_lcde_cdl_check_FAT12_16:
	; 29/07/2022
	;xor	eax, eax
	mov	ax, [esi+LD_BPB+RootDirEnts]
	;dec	ax
	dec	eax
	;xor	dx, dx
	;cmp	ax, cx ; cx = Directory Entry (Index) Number
	; 29/07/2022
	cmp	eax, ecx
	jb	short loc_lcde_stc_12h_retn
	mov	[LCDE_EntryIndex], cx
	xor	eax, eax
        jmp     loc_lcde_check_dir_buffer_cluster

loc_lcde_stc_12h_retn:
	pop	esi
	mov	ebx, ecx
	mov	ecx, edx
	; 16/10/2016 (12h -> 12)
	mov	eax, 12 ; No more files
	retn

loc_lcde_calc_dirbuff_cluster_offset:
	;mov	bl, [esi+LD_BPB+SecPerClust]
	;xor	bh, bh
	;mov	ax, [esi+LD_BPB+BytesPerSec]
	;mul	bx
 	;;or	dx, dx ; If bytes per cluster > 32KB it is invalid
	;; 29/07/2022
	;or	dl, dl
	;jnz	short loc_lcde_invalid_format
	; 29/07/2022
	xor	ebx, ebx
	mov	bl, [esi+LD_BPB+SecPerClust]
	mov	ax, [esi+LD_BPB+BytesPerSec]
	mul	ebx
	mov	ecx, eax
	;mov	cx, ax ; BYTES PER CLUSTER
	mov	eax, [LCDE_ByteOffset]
	;sub	edx, edx
	div	ecx
	cmp	eax, 65535
	ja	short loc_lcde_invalid_format

	; cluster sequence number of directory (< 65536)
	mov	[LCDE_ClusterSN], ax

	;mov	ax, dx ; byte offset in cluster (directory buffer)
	; 29/07/2022
	mov	eax, edx
	;mov	bx, 32	; 1 dir entry = 32 bytes
        mov	bl, 32
	;sub	dx, dx	; 0
	;div	bx
	sub	edx, edx
	div	ebx
	mov	[LCDE_EntryIndex], ax ; dir entry index/sequence number
				      ; (in directory buffer/cluster)
loc_lcde_get_current_sub_dir_fcluster:
	mov	eax, [Current_Dir_FCluster]

loc_lcde_get_next_cluster:
	cmp	word [LCDE_ClusterSN], 0
	jna	short loc_lcde_check_dir_buffer_cluster
	mov	[LCDE_Cluster], eax
	call	get_next_cluster
	jc	short loc_lcde_check_gnc_error
  	dec	word [LCDE_ClusterSN]
	jmp	short loc_lcde_get_next_cluster

loc_lcde_reload_current_directory:
	push	ecx
	call	reload_current_directory
	pop	ecx
	;jnc	loc_lcde_cdl_check
	;pop	esi
	;retn
	; 09/08/2022
	jc	short loc_lcde_retn
	jmp	loc_lcde_cdl_check

loc_lcde_invalid_format:
	; 15/10/2016 (0Bh -> 28)
	;mov	eax, 28 ; Invalid Format !
	; 29/07/2022
	sub	eax, eax
	mov	al, 28
loc_lcde_drive_not_ready_read_err:
	stc
	pop	esi
	retn

loc_lcde_check_gnc_error:
	or	eax, eax
	jnz	short loc_lcde_drive_not_ready_read_err
	dec	word [LCDE_ClusterSN]
	jnz	short loc_lcde_invalid_format
	mov	eax, [LCDE_Cluster]

loc_lcde_check_dir_buffer_cluster:
	cmp	eax, [DirBuff_Cluster]
	jne	short loc_lcde_load_dir_cluster
	cmp	byte [DirBuff_ValidData], 0
	ja	short lcde_check_dir_buffer_cluster_next
	cmp	byte [Current_Dir_Level], 0
	ja	short loc_lcde_load_dir_cluster_0
	; 27/02/2016
	; TRDOS v1 has bug here for FAT32 fs !
	cmp	byte [esi+LD_FATType], 3  ; FAT32
	jnb	short loc_lcde_load_dir_cluster_0
	;
	movzx	ecx, word [esi+LD_BPB+RootDirEnts]
	add	cx, 15 ; round up (16 entries per sector)
	;shr	cx, 4 ; 1 sector contains 16 dir entries
	; 29/07/2022
	shr	ecx, 4
        mov     eax, [esi+LD_ROOTBegin]
	jmp	short loc_lcde_load_dir_cluster_1

loc_lcde_validate_dirBuff:
	mov	byte [DirBuff_ValidData], 1

lcde_check_dir_buffer_cluster_next:
	movzx	ebx, word [LCDE_EntryIndex]
	cmp	bx, [DirBuff_LastEntry]
	ja	short loc_lcde_invalid_format
	;mov	eax, 32
	; 29/07/2022
	xor	eax, eax
	mov	al, 32
	mul	ebx
	;or	edx, edx
	;jnz	short loc_lcde_invalid_format

	mov	edi, Directory_Buffer
	add	edi, eax ; add entry offset to buffer address

loc_lcde_dir_buffer_last_check:
	mov	eax, [DirBuff_Cluster]
	movzx	ecx, byte [DirBuff_ValidData]

loc_lcde_retn:
	pop	esi
	retn

loc_lcde_load_dir_cluster:
	;cmp	byte [DirBuff_ValidData], 2
	;jne	short loc_lcde_load_dir_cluster_n2
	push	eax
	call	save_directory_buffer
	pop	eax
	jc	short loc_lcde_retn

loc_lcde_load_dir_cluster_n2:
	mov	byte [DirBuff_ValidData], 0
	mov	[DirBuff_Cluster], eax

loc_lcde_load_dir_cluster_0:
	;sub	eax, 2
	; 30/07/2022
	dec	eax
	dec	eax
	movzx	ecx, byte [esi+LD_BPB+SecPerClust]
	mul	ecx
        add     eax, [esi+LD_DATABegin]

loc_lcde_load_dir_cluster_1:
	mov	ebx, Directory_Buffer
	; ecx = sector count
	call	disk_read
	jnc	short loc_lcde_validate_dirBuff

	; 15/10/2016
	; (Disk read error instead of drv not ready err)
	mov	eax, 17 ; Drive not ready or read error !
	jmp	short loc_lcde_retn

remove_file:
	; 29/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 15/10/2016
	; 28/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 10/04/2011 (FILE.ASM, 'proc_delete_file')
	; 09/08/2010
	; INPUT ->
	;	EDI = Directory Buffer Entry Address
	;	 CX = Directory Buffer Entry Counter/Index
	;	 BL = Longname Entry Length
	;	 BH = Logical DOS Drive Number

	sub	eax, eax
	mov	ah, bh
	mov	esi, Logical_DOSDisks
	add	esi, eax

	cmp	byte [esi+LD_FATType], 1
	jnb	short loc_del_fat_file

	cmp	byte [esi+LD_FSType], 0A1h
	je	short loc_del_fs_file

loc_del_file_invalid_format:
	xor	ah, ah
	; 15/10/2016 (0Bh -> 28)
	mov	al, 28  ; Invalid Format
	stc 
	retn

loc_del_fs_file:
	;call	delete_fs_file
	;retn
	; 29/07/2022
	jmp	delete_fs_file

loc_del_fat_file:
	call	delete_directory_entry
	jc	short loc_del_file_err_retn

loc_delfile_unlink_cluster_chain:
	call	truncate_cluster_chain
	;jc	short loc_del_file_err_retn

loc_delfile_return:
loc_del_file_err_retn:
	retn

delete_directory_entry:
	; 15/10/2016
	; 28/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 01/08/2011 (DIR.ASM, 'proc_delete_directory_entry')
	; 10/04/2011
	; INPUT ->
	; 	ESI = Logical Dos Drive Descripton Table Address 
	;	EDI = Directory Buffer Entry Address
	;	 CX = Directory Buffer Entry Counter/Index
	;	 BL = Longname Entry Length
	; OUTPUT ->
	; 	ESI = Logical dos drive descripton table address 
	;	EAX = First cluster to be truncated/unlinked
	;       CF = 1 -> Error code in EAX (AL)
	;       CF = 0 & BH <> 0 -> LMDT write error  (BH = 1)
	;       CF = 0 & BL <> 0 -> Long name delete error (BL = FFh) 
	;
	;  (EDI, EBX, ECX register contents will be changed)

	mov	[DelFile_LNEL], bl
	mov	[DelFile_EntryCounter], cx

	mov	ax, [edi+20] ; First Cluster High Word
	shl	eax, 16
	mov	ax, [edi+26] ; First Cluster Low Word

	mov	[DelFile_FCluster], eax

loc_del_short_name:
	mov	byte [edi], 0E5h  ; Deleted sign

	mov	byte [DirBuff_ValidData], 2
	call	save_directory_buffer
	jc	short loc_delete_direntry_err_return

loc_del_long_name:
	movzx	edx, byte [DelFile_LNEL]
	or	dl, dl
	jz	short loc_del_dir_entry_update_parent_dir_lm_date

	mov	[DelFile_LNEL], dh ; 0

	movzx	eax, word [DelFile_EntryCounter]
	sub	eax, edx
	;jnc	short loc_del_long_name_continue
	jc	short loc_del_dir_entry_update_parent_dir_lm_date

;loc_del_direntry_inv_data_return: ; 15/10/2016 (0Dh -> 29)
;	mov	eax, 29 ; 0Dh (TRDOS 8086) ; Invalid data
;	retn

loc_del_long_name_continue: 
	; AX = Directory Entry Number of the long name last entry
	call	delete_longname
	;jc	short loc_delete_direntry_err_return

loc_del_dir_entry_update_parent_dir_lm_date:
	sbb	byte [DelFile_LNEL], 0 ; 0FFh if cf = 1

	call	update_parent_dir_lmdt
	mov	bh, 0
	adc	bh, 0

	mov	bl, byte [DelFile_LNEL]

loc_delete_direntry_return:
	mov	eax, [DelFile_FCluster]
loc_delete_direntry_err_return:
	retn

rename_directory_entry:
	; 29/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 13/11/2017
	; 15/10/2016
	; 06/03/2016 (TRDOS 386 = TRDOS v2.0)
	; 01/08/2011 (DIR.ASM, 'proc_rename_directory_entry')
	; 19/11/2010
	; INPUT -> (Current Directory)
	;	CX = Directory Entry Number
	;      EAX = First Cluster number of file or directory
	;      EBX = Longname Length (dir entry count) (< 256)
	;      ESI = New file (or directory) name (no path).
	;           (ASCIIZ string)
	; OUTPUT -> 
	;      CF = 0 -> successfull
	;      CF = 1 -> error code in EAX (AL)
	;
	; (EAX, EBX, ECX, EDX, ESI, EDI will be changed)

	cmp	byte [Current_FATType], 0
	ja	short loc_rename_directory_entry

	;call	rename_fs_file_or_directory
	;retn
	; 29/07/2022
 	jmp	rename_fs_file_or_directory
	
loc_rename_directory_entry:
	mov	[DelFile_LNEL], bl
	mov	[DelFile_EntryCounter], cx
	mov	[DelFile_FCluster], eax

	movzx	eax, cx
	call	locate_current_dir_entry
	jnc	short loc_rename_direntry_check_fcluster

loc_rename_direntry_pop_retn:
	retn

loc_rename_direntry_pop_invd_retn:
	; 29/07/2022
	;stc
loc_rename_direntry_invd_retn:
	; 15/10/2016 (0Dh -> 29)
	;mov	eax, 29 ; Invalid data
	; 29/07/2022
	sub	eax, eax
	mov	al, 29
	stc
loc_rename_retn:
	retn

loc_rename_direntry_check_fcluster:
	mov	dx, [edi+20] ; First Cluster HW
	shl	edx, 16 ; 13/11/2017
	mov	dx, [edi+26] ; First Cluster LW
	cmp	edx, [DelFile_FCluster]
	jne	short loc_rename_direntry_pop_invd_retn
	; ESI = New file (or directory) name. (ASCIIZ string)
	; 06/03/2016
	; TRDOS v2 - NOTE: 'convert_file_name' procedure
	; has been modified for eliminating following situation.
	; 
	; TRDOS v1 - NOTE: If file/dir name is more than 11 bytes
	; without a dot, attributes (edi+11) byte will be overwritten !
	; (Dot file name input must be proper for 11 byte dir entry
	;  type file name output.)
	call	convert_file_name

        mov     byte [DirBuff_ValidData], 2
	call	save_directory_buffer
	jc	short loc_rename_retn

loc_rename_direntry_del_ln:
	movzx	edx, byte [DelFile_LNEL]
	or	dl, dl
	jz	short loc_rename_direntry_update_parent_dir_lm_date

	movzx	eax, word [DelFile_EntryCounter]
	sub	eax, edx
	jc	short loc_rename_direntry_invd_retn

loc_rename_direntry_del_ln_continue:
	; EAX = Directory Entry Number of the long name last entry
	call	delete_longname

loc_rename_direntry_update_parent_dir_lm_date:
	call	update_parent_dir_lmdt
	xor	eax, eax
	retn

move_source_file_to_destination_file:
	; 07/08/2022
	; 29/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 15/10/2016
	; 11/03/2016
	; 10/03/2016 (TRDOS 386 = TRDOS v2.0)
	; 01/08/2011 (FILE.ASM)
	; 04/08/2010
	;
	;   Phase 1 -> Check destination file,
	;              'not found' is required
	;   Phase 2 -> Check source file
	;              'found' and proper attributes is required
	;   Phase 3 -> Make destination directory entry,
	;           add new dir cluster or section if it is required
	;   Phase 4 -> Delete source directory entry.
	;       cf = 1 causes to return before the phase 4.
	;    (source file protection against any possible errors)
	;
	; 08/05/2011 major modification
	;            -> destination file deleting is removed
	;            for msdos move/rename compatibility.
	;            (Access denied error will return if
	;            the destination file is found...)
	; INPUT ->
	;	 ESI = Source File Pathname (Asciiz)
	;        EDI = Destination File Pathname (Asciiz)
	;        AL = 0 --> Interrupt (System call)
	;        AL > 0 --> Command Interpreter (Question)
	;        AL = 1 --> Question Phase
	;        AL = 2 --> Progress Phase
	; OUTPUT ->
	;	 cf = 0 -> OK
	;        EAX = Destination directory first cluster
	;        ESI = Logical DOS drive description table
	;        EBX = Destination file structure offset
	;        CX = 0 (CX > 0 --> calculate free space error)
	;        cf = 1 -> Error code in EAX (AL) 
	;
	;  (EDX, ECX, EBX, ESI, EDI will be changed)

	cmp	al, 2
	;je	msftdf_df2_check_directory
	; 29/07/2022
	jne	short msftdf
	jmp	msftdf_df2_check_directory
msftdf:
	mov	[move_cmd_phase], al

msftdf_parse_sf_path:
	; ESI = ASCIIZ pathname (Source)
	push	edi 
	mov	edi, SourceFile_Drv
	call	parse_path_name
	pop	esi
	jc	short msftdf_psf_retn

msftdf_parse_df_path:
	; ESI = ASCIIZ pathname	(Destination)
	mov	edi, DestinationFile_Drv
	call	parse_path_name
	jnc	short msftdf_check_sf_drv

	cmp	al, 1 ; File or directory name is not existing
	jna	short msftdf_check_sf_drv

msftdf_stc_retn:
	stc
msftdf_psf_retn:
	retn

msftdf_check_sf_drv:
	mov	al, [SourceFile_Drv]

msftdf_check_df_drv:
	mov	dl, [DestinationFile_Drv]

msftdf_compare_sf_df_drv:
	sub	ebx, ebx
	mov	bh, [Current_Drv]
	cmp	dl, al
	je	short msftdf_check_sf_df_drv_ok

msftdf_not_same_drv:
        ; DL = source file's drive number
	mov	dh, al ; destination file's drive number
	; 15/10/2016 (11h -> 21)
	;mov	eax, 21 ; Not the same drive
	; 29/07/2022
	sub	eax, eax
	mov	al, 21
	stc
	retn 

msftdf_check_sf_df_drv_ok:
	mov	[msftdf_sf_df_drv], dl

        sub	eax, eax
	mov	ah, dl
	add	eax, Logical_DOSDisks
	mov	[msftdf_drv_offset], eax

	cmp	dl, bh ; byte [Current_Drv]
	je	short msftdf_df_check_directory

msftdf_change_drv:
	call 	change_current_drive
	jc	short msftdf_df_error_retn
	  
msftdf_check_destination_file:
msftdf_df_check_directory:
	mov	esi, DestinationFile_Directory
	cmp	byte [esi], 20h
	jna	short msftdf_df_find_1

msftdf_df_change_directory:
	inc	byte [Restore_CDIR]
	xor	ah, ah ; CD_COMMAND sign -> 0
	call	change_current_directory
	jc	short msftdf_df_error_retn

;msftdf_df_change_prompt_dir_string:
;	call 	change_prompt_dir_string

msftdf_df_find_1:
        mov     esi, DestinationFile_Name
	cmp	byte [esi], 20h
	jna	short msftdf_df_copy_sf_name

msftdf_df_find_2:
	xor	ax, ax ; DestinationFile_AttributesMask -> any/zero
	call	find_first_file
	;jnc	msftdf_permission_denied_retn
	; 29/07/2022
	jc	short msftdf_df_check_error_code
	jmp	msftdf_permission_denied_retn

msftdf_df_check_error_code:
	;cmp	eax, 2 ; File not found error
	cmp	al, 2
	jne	short msftdf_df_stc_retn

msftdf_df_check_fname:
	; 15/10/2016
	mov	esi, DestinationFile_Name ; *
	call	check_filename
	jnc	short msftdf_convert_df_direntry_name
	; invalid file name chars !
	mov	eax, ERR_INV_FILE_NAME  ; 26
	jmp	short msftdf_df_stc_retn

msftdf_convert_df_direntry_name:
	; mov	esi, DestinationFile_Name ; *
	mov	edi, DestinationFile_DirEntry
	call	convert_file_name
  	jmp	short msftdf_restore_current_dir_1

msftdf_df_copy_sf_name:
	mov	edi, esi
	push	edi
        mov     esi, SourceFile_Name
	;mov	ecx, 12
	; 29/07/2022
	sub	ecx, ecx
	mov	cl, 12
msftdf_df_copy_sf_name_loop:
	lodsb
        stosb
	or	al, al
	jz	short msftdf_df_copy_sf_name_ok
        loop    msftdf_df_copy_sf_name_loop
msftdf_df_copy_sf_name_ok:
	pop	esi
	jmp	short msftdf_df_find_2

msftdf_df_stc_retn:
	stc
msftdf_restore_cdir_failed:
msftdf_df_error_retn:
	retn

msftdf_restore_current_dir_1:
	cmp	byte [Restore_CDIR], 0
	jna	short msftdf_sf_check_directory
	mov	esi, [msftdf_drv_offset] 
	call	restore_current_directory
	jc	short msftdf_restore_cdir_failed

msftdf_sf_check_directory:
	mov	esi, SourceFile_Directory
	cmp	byte [esi], 20h
	jna	short msftdf_sf_find
msftdf_sf_change_directory:
	inc	byte [Restore_CDIR]
	xor	ah, ah ; CD_COMMAND sign -> 0
	call	change_current_directory
	jc	short msftdf_return

;msftdf_sf_change_prompt_dir_string:
;	call	change_prompt_dir_string

msftdf_sf_find:
        mov     esi, SourceFile_Name  ; Offset 66
	mov	ax, 1800h ; Only files
	call	find_first_file
	jc	short msftdf_return

msftdf_sf_ambgfn_check:
	or	dx, dx ; Ambiguous filename chars used sign (DX>0)
	jz	short msftdf_sf_found

msftdf_ambiguous_file_name_error:
	;mov	eax, 2 ; File not found error
	; 29/07/2022
	sub	eax, eax
	mov	al, 2
	stc
	retn

msftdf_sf_found:
	and	bl, 1Fh ; Attributes, D-V-S-H-R
	jz	short msftdf_save_sf_structure

msftdf_permission_denied_retn:
	;mov	eax, 05h ; Access (Permission) denied !
	; 29/07/2022
	sub	eax, eax
	mov	al, 5
	stc
msftdf_rest_cdir_err_retn:
msftdf_return:
	retn

msftdf_phase_1_return:
	xor	eax, eax
	mov	[move_cmd_phase], al ; 0
	inc	al ; mov al, 1
	mov	ebx, msftdf_df2_check_directory
	;mov	edx, 0FFFFFFFFh 
	retn

msftdf_save_sf_structure:
	mov	esi, FindFile_DirEntry
	mov	edi, SourceFile_DirEntry
	;mov	ecx, 8
	; 29/07/2022
	sub	ecx, ecx
	mov	cl, 8
	rep	movsd

msftdf_df_copy_sf_parameters:
	mov	esi, 11
	mov	edi, esi
	add	esi, SourceFile_DirEntry
	add	edi, DestinationFile_DirEntry
	;mov	ecx, 21
	mov	cl, 21
	rep	movsb

msftdf_restore_current_dir_2:
	cmp	byte [Restore_CDIR], 0
	jna	short msftdf_df2_check_move_cmd_phase
 	mov	esi, [msftdf_drv_offset]
	call	restore_current_directory
	jc	short msftdf_rest_cdir_err_retn

msftdf_df2_check_move_cmd_phase:
	cmp	byte [move_cmd_phase], 1
	je	short msftdf_phase_1_return

msftdf_df2_check_directory:
	mov	esi, DestinationFile_Directory
	cmp	byte [esi], 20h
	jna	short msftdf_make_dfde_locate_ffe_on_directory
msftdf_df2_change_directory:
	inc	byte [Restore_CDIR]
	xor	ah, ah ; CD_COMMAND sign -> 0
	call	change_current_directory
	jc	short msftdf_return

;msftdf_df2_change_prompt_dir_string:
;	call	change_prompt_dir_string

msftdf_make_dfde_locate_ffe_on_directory:
	; Current directory fcluster <> Directory buffer cluster
	; Current directory will be reloaded by
	; 'locate_current_dir_file' procedure
	;
	;xor	ax, ax
	xor	eax, eax
	mov	ecx, eax
	dec	cx ; FFFFh
		; CX = FFFFh -> find first deleted or free entry
		; ESI would be ASCIIZ filename address if the call
		; would not be for first free or deleted dir entry
	call	locate_current_dir_file
	; 07/08/2022
	jnc	short msftdf_make_dfde_set_ff_dir_entry

	;cmp	eax, 2
        cmp	al, 2
	jne	short msftdf_error_retn

msftdf_add_new_dir_entry_check_fs:
	mov	esi, [msftdf_drv_offset]
	mov 	eax, [DirBuff_Cluster]
	cmp	byte [esi+LD_FATType], 0
	ja	short msftdf_add_new_subdir_cluster

msftdf_add_new_fs_subdir_section:
	;CL=0, CH=E5h --> deleted entry, CH=0 --> free entry
        ;xor	cx, cx
	xor	ch, ch ; cx = 0 --> add a new subdir section
	call	add_new_fs_section
        jc	short msftdf_dsfde_error_retn
	;mov	[createfile_LastDirCluster], eax

	call	load_FS_sub_directory
	;mov	ebx, Directory_Buffer
	jnc	short msftdf_add_new_fs_subdir_section_ok
	retn

msftdf_add_new_subdir_cluster:
	call	add_new_cluster
	jc	short msftdf_dsfde_error_retn

	;mov	[createfile_LastDirCluster], eax

	call	load_FAT_sub_directory
	jnc	short msftdf_add_new_subdir_cluster_ok
	; EBX = Directory buffer address

msftdf_ansdc_update_parent_dir_lmdt:
msftdf_make_dfde_err_upd_pdir_lmdt:
	push	eax
	call	update_parent_dir_lmdt
	pop	eax

msftdf_error_retn:
	stc
msftdf_dsfde_restore_cdir_failed:
msftdf_dsfde_error_retn:
	retn

msftdf_add_new_fs_subdir_section_ok:
msftdf_add_new_subdir_cluster_ok:
	mov	edi, ebx ; Directory buffer address

msftdf_make_dfde_set_ff_dir_entry:
	mov	edx, [Current_Dir_FCluster]
	mov	[createfile_FFCluster], edx
	; EDI = Directory entry offset
	mov	esi, DestinationFile_DirEntry
	;mov	ecx, 8
	; 29/07/2022
	sub	ecx, ecx
	mov	cl, 8
	rep	movsd

	mov	byte [DirBuff_ValidData], 2
	call	save_directory_buffer
	jc	short msftdf_make_dfde_err_upd_pdir_lmdt

msftdf_make_dfde_update_pdir_lmdt:
	call	update_parent_dir_lmdt

msftdf_dsfde_restore_current_dir_1:
	cmp	byte [Restore_CDIR], 0
	jna	short msftdf_dsfde_check_directory
 	mov	esi, [msftdf_drv_offset]
	call	restore_current_directory
	jc	short msftdf_dsfde_restore_cdir_failed

msftdf_dsfde_check_directory:
	mov	esi, SourceFile_Directory
	cmp	byte [esi], 20h
	jna	short msftdf_dsfde_find_file

msftdf_dsfde_change_directory:
	inc	byte [Restore_CDIR]
	sub	ah, ah ; CD_COMMAND sign -> 0
	call	change_current_directory
	jc	short msftdf_dsfde_error_retn

;msftdf_dsfde_sf_change_prompt_dir_string:
;	call	change_prompt_dir_string

msftdf_dsfde_find_file:
	mov	esi, SourceFile_Name  ; Offset 66
	mov	ax, [esi+14] ; 80 -> SourceFile_AttributesMask
	call	find_first_file
	jc	short msftdf_dsfde_error_retn

msftdf_dsfde_delete_direntry:
	mov	esi, [msftdf_drv_offset]

	cmp	byte [esi+LD_FATType], 0
	ja	short msftdf_delete_FAT_direntry

	xor	bl, bl
	; BL = 0 -> File
	; EDI -> Directory buffer entry offset/address 
	call	delete_fs_directory_entry
	jnc	short msftdf_dsfde_restore_current_dir_2
	retn

msftdf_delete_FAT_direntry:
	mov	bl, [FindFile_LongNameEntryLength]
	mov	cx, [FindFile_DirEntryNumber]
	; ESI = Logical DOS drive description table address
	; EDI = Directory buffer entry offset/address
	call	delete_directory_entry
	jc	short msftdf_retn

msftdf_dsfde_restore_current_dir_2:
	cmp	byte [Restore_CDIR], 0
	jna	short msftdf_new_dir_fcluster_retn
	;mov	esi, [msftdf_drv_offset]
	call	restore_current_directory
	jc	short msftdf_retn

msftdf_new_dir_fcluster_retn:
	xor	ecx, ecx 
	mov	eax, [createfile_FFCluster]
	mov	ebx, DestinationFile_Drv

msftdf_retn:
	retn

copy_source_file_to_destination_file:
	; 31/08/2024
	; 30/08/2024
	; 29/08/2024
	; 26/08/2024
	; 25/08/2024 (TRDOS 386 v2.0.9)
	; 29/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 17/10/2016
	; 16/10/2016
	; 15/10/2016
	; 30/03/2016, 31/03/2016
	; 24/03/2016, 25/03/2016, 28/03/2016
	; 21/03/2016, 22/03/2016, 23/03/2016
	; 16/03/2016, 17/03/2016, 18/03/2016
	; 15/03/2016 (TRDOS 386 = TRDOS v2.0)
	; 02/09/2011 (FILE.ASM 'copy_source_file_to_destination_file')
	; 01/08/2010 - 18/05/2011
	;
	;   Command Interpreter phase 1 enter ->
	;           AL = 1 -> Caller is command interpreter
	;           AL = 2 -> The second call, re-enter/continue
	;   Phase 1 -> Check source file
	;              'found' is required
	;   Phase 2 -> Check destination file,
	;              save 'found' or 'not found' status
	;              'permission denied' error will be return
	;              if attributes have not for ordinary file
	;              without readonly attribute
	;   Command Interpreter phase 1 return ->
	;              DH = Source file attributes
	;              DL = Destination file found status
	;              EAX = 0
	;   Command Interpreter phase 2 enter ->
	;              AL = 2 -> Continue from the last position
	;              AH =
	;   Phase 3 -> Load source file or use read/write cluster method
	;   Phase 4 -> Create destination file if it is not found
	;   Phase 5 -> Open destination file
	;   Phase 6 -> Read from source and write to destination
	;   Phase 7 -> Unload source file, if it is loaded at memory
	;       cf = 1 causes to return before the phase 7
	;              but loaded file will be unloaded
	;	       (allocated memory block will be deallocated)
	;
	; INPUT ->
	;	 ESI = Source File Pathname (Asciiz)
	;        EDI = Destination File Pathname (Asciiz)
	;        AL = 0 --> Interrupt (System call)
	;        AL > 0 --> Command Interpreter (Question)
	;        AL = 1 --> Question Phase
	;        AL = 2 --> Progress Phase
	;
	; OUTPUT ->
	;	cf = 0 -> OK ; (*)
	;	EAX = Destination file first cluster
	;
	;	31/08/2024 - TRDOS v2.0.9 ; (*)
	; obsolete ;CL > 0 if there is file reading error before EOF
	;          ;	        (incomplete copy) 
	;          ;CH > 0 if file is (full) loaded at memory
	;
	;	cf = 1 -> Error code in AL (EAX) ; (*)
	;
	; (EBX, ECX, ESI, EDI register contents will be changed)

	cmp	al, 2
	;je	csftdf2_check_cdrv
	; 29/07/2022
	jb	short csftdf_ph1
	;jmp	csftdf2_check_cdrv
	jmp	csftdf_ph2

; Phase 1
	; 29/07/2022
csftdf_ph1:
	mov	byte [copy_cmd_phase], al

	push	edi ; *

csftdf_parse_sf_path:
	mov	edi, SourceFile_Drv
	call	parse_path_name
	jc	short csftdf_parse_sf_path_failed

csftdf_parse_df_path:
	pop	esi ; * (pushed edi)

csftdf_sf_check_filename_exists:
	cmp	byte [SourceFile_Name], 21h
	jb	short csftdf_sf_file_not_found_error

	mov	edi, DestinationFile_Drv
	call	parse_path_name
	jnc	short csftdf_check_sf_cdrv

	cmp	al, 1 ; File or directory name is not existing
	jna	short csftdf_check_sf_cdrv

csftdf_parse_df_path_failed:
	stc 
csftdf_sf_error_retn:
	retn

csftdf_parse_sf_path_failed:
	pop	edi ; *
	;jmp	short csftdf_sf_error_retn
	; 25/08/2024
	retn

csftdf_sf_file_not_found_error:
	mov	eax, 2 ; File not found
	;jmp	short csftdf_sf_error_retn
	; 25/08/2024
	retn

csftdf_check_sf_cdrv:
	mov	bh, [Current_Drv]

	mov	[csftdf_cdrv], bh ; 23/03/2016

	mov	dl, [SourceFile_Drv]
	cmp	dl, bh ; byte [Current_Drv]
	je	short csftdf_sf_check_directory

	call	change_current_drive
	jc	short csftdf_sf_error_retn

csftdf_sf_check_directory:
	mov	esi, SourceFile_Directory
	cmp	byte [esi], 20h
	jna	short csftdf_find_sf

csftdf_sf_change_directory:
	inc	byte [Restore_CDIR]
	xor	ah, ah ; CD_COMMAND sign -> 0
	call	change_current_directory
	jc	short csftdf_sf_error_retn

;csftdf_sf_change_prompt_dir_string:
;	call	change_prompt_dir_string

csftdf_find_sf:
	mov	esi, SourceFile_Name
	mov	ax, 1800h ; Except volume label and dirs
	call	find_first_file
	jc	short csftdf_sf_error_retn

csftdf_sf_ambgfn_check:
	and	dx, dx ; Ambiguous filename chars used sign (DX>0)
	jz	short csftdf_sf_found

csftdf_ambiguous_file_name_error:
	;mov	eax, 2 ; File not found error
	; 29/07/2022
	sub	eax, eax
	mov	al, 2
	stc
	retn

csftdf_sf_found:
	mov	[csftdf_filesize], eax

	or	eax, eax
	jnz	short csftdf_set_source_file_direntry

;csftdf_sf_file_size_zero:
	;mov	eax, 20 ; TRDOS zero length (file size) error
	; 25/08/2024
	; eax = 0
	; 29/07/2022
	;sub	eax, eax
	mov	al, 20
	stc
	retn

csftdf_set_source_file_direntry:
	mov	esi, FindFile_DirEntry
	mov	edi, SourceFile_DirEntry
	;mov	ecx, 8
	; 29/07/2022
	xor	ecx, ecx
	mov	cl, 8
	rep	movsd

csftdf_sf_restore_cdrv:
	; 22/03/2016
	mov	dl, [csftdf_cdrv]
	cmp	dl, [Current_Drv]
	je	short csftdf_sf_restore_cdir
	call	change_current_drive
	jc	short csftdf_df_error_retn ; 30/03/2016

csftdf_sf_restore_cdir:
	cmp	byte [Restore_CDIR], 0
	jna	short csftdf_df_check_filename_exists
	sub	eax, eax
	mov	esi, Logical_DOSDisks
	mov	ah, dl ; byte [csftdf_cdrv]
	add	esi, eax
	call	restore_current_directory
	jc	short csftdf_df_error_retn

csftdf_df_check_filename_exists:
	cmp	byte [DestinationFile_Name], 20h
	ja	short csftdf_check_df_cdrv

csftdf_copy_sf_name:
	mov	edi, DestinationFile_Name
	mov	esi, SourceFile_Name
	mov	cl, 12

csftdf_df_copy_sf_name_loop:
	lodsb
	stosb
	or	al, al
	jz	short csftdf_check_df_cdrv
	dec	cl
	jnz	csftdf_df_copy_sf_name_loop

csftdf_check_df_cdrv:
	mov	dl, [DestinationFile_Drv]
	cmp	dl, [Current_Drv]
	je	short csftdf_df_check_directory

	call	change_current_drive
	jnc	short csftdf_df_check_directory

csftdf_df_error_retn:
	retn

csftdf_df_check_directory:
	mov	esi, DestinationFile_Directory
        cmp     byte [esi], 20h
	jna	short csftdf_find_df

csftdf_df_change_directory:
	inc	byte [Restore_CDIR]
	sub	ah, ah ; CD_COMMAND sign -> 0
	call	change_current_directory
	jc	short csftdf_df_error_retn

;csftdf_df_change_prompt_dir_string:
;	call	change_prompt_dir_string

csftdf_find_df:
	; 23/03/2016
	sub	ebx, ebx
	mov	bh, [DestinationFile_Drv]
	add	ebx, Logical_DOSDisks
	mov	[csftdf_df_drv_dt], ebx

	mov	esi, DestinationFile_Name
	;xor	ax, ax
	; 25/08/2024
	xor	eax, eax
		; DestinationFile_AttributesMask -> any/zero
	call	find_first_file
	jc	short csftdf_df_check_error_code

csftdf_df_ambgfn_check:
	or	dx, dx ; Ambiguous filename chars used sign (DX>0)
	jnz	short csftdf_df_error_inv_fname

csftdf_df_found:
	mov	byte [DestinationFileFound], 1
	; 17/10/2016 (cl -> bl)
	and	bl, 1Fh ; Attributes, D-V-S-H-R
	jz	short csftdf_df_save_first_cluster

csftdf_df_permission_denied_retn:
	;mov	eax, 05h ; Access/Permission denied.
	; 29/07/2022
	sub	eax, eax
	mov	al, 5
csftdf_df_error_stc_retn:
	stc
	retn

csftdf_df_check_error_code:
	;cmp	eax, 2
	cmp	al, 2
	jne	short csftdf_df_error_stc_retn

	mov	byte [DestinationFileFound], 0

	; 15/10/2016
	mov	esi, FindFile_Name ; *
	call	check_filename
	jnc	short csftdf_df_valid_fname
csftdf_df_error_inv_fname: ; 'invalid file name !'
	;mov 	eax, ERR_INV_FILE_NAME  ; 26
	; 29/07/2022
	sub	eax, eax
	mov	al, ERR_INV_FILE_NAME ; 26
	stc
	retn

csftdf_df_valid_fname:
	; 21/03/2016
	; (Capitalized file name)
	;mov	esi, FindFile_Name ; * ; 15/10/2016
	mov	edi, DestinationFile_Name
	movsd
	movsd
	movsd
	;movsb

csftdf_check_disk_free_size_0:
	mov	eax, [SourceFile_DirEntry+DirEntry_FileSize]

csftdf_check_disk_free_size_1:
	;sub	ebx, ebx
	;mov 	esi, Logical_DOSDisks
	;mov	bh,  [DestinationFile_Drv]
	;add	esi, ebx

	mov	esi, [csftdf_df_drv_dt] ; 23/03/2016

	movzx	ecx, word [esi+LD_BPB+BytesPerSec] ; 17, LD_BPB + 0Bh
	add	eax, ecx
	dec	eax  ; file size (additional bytes) + 511 (round up)
csftdf_check_disk_free_size_3: ; 16/03/2016
	sub	edx, edx
	div	ecx ; bytes per sector

csftdf_check_disk_free_size:
	cmp	eax, [esi+LD_FreeSectors]
        ;jb	csftdf_check_disk_free_size_ok
	; 25/08/2024
	; 29/07/2022
	;jb	short csftdf_check_dfs_ok
	ja	short csftdf_df_insufficient_disk_space

	; 25/08/2024
	;cmp	byte [esi+LD_FATType], 0 ; FS needs FDT sector also.
	;;ja	csftdf_check_disk_free_size_ok
	;; 29/07/2022
	;jna	short csftdf_df_insufficient_disk_space

csftdf_check_dfs_ok:
	jmp	csftdf_check_disk_free_size_ok

csftdf_df_insufficient_disk_space:
	;mov	eax, 27h ; insufficient disk space
	; 29/07/2022
	sub	eax, eax
	mov	al, 27h
	;jmp	short csftdf_df_error_stc_retn
	; 25/08/2024
	stc
	retn

csftdf_df_save_first_cluster:
	; ESI = FindFile_DirEntry (for the old destination file)
	; EAX = Old destination file size
	; 24/03/2016
	; EDI = Directory entry address (within Dir Buffer boundaries)
	sub	edi, Directory_Buffer  ; (<65536)
	;shr	di, 5 ; Convert entry offset to entry index/number
	; 29/07/2022
	shr	edi, 5
	mov	[DestinationFile_DirEntryNumber], di ; (<2048)

csftdf_df_check_sf_df_fcluster:
	mov	dx, [esi+DirEntry_FstClusHI]
	shl	edx, 16
	mov	dx, [esi+DirEntry_FstClusLO]
	mov	[csftdf_df_cluster], edx
csftdf_df_check_sf_df_fcluster_1:
	mov	dx, [SourceFile_DirEntry+DirEntry_FstClusHI]
	shl	edx, 16
	mov	dx, [SourceFile_DirEntry+DirEntry_FstClusLO]
	cmp	edx, [csftdf_df_cluster]
	jne	short csftdf_df_check_sf_df_fcluster_ok
csftdf_df_check_sf_df_drv:
	mov	dl, [SourceFile_Drv]
	cmp	dl, [DestinationFile_Drv]
	jne	short csftdf_df_check_sf_df_fcluster_ok

	; source and destination files are same !
	; (they have same first cluster value on same logical disk)

	xor	eax, eax ; mov eax, 0 -> Bad command or file name !
	stc
	retn

csftdf_df_check_sf_df_fcluster_ok:
csftdf_df_move_findfile_struct:
	; mov	esi, FindFile_DirEntry
	mov	edi, DestinationFile_DirEntry
	;mov	ecx, 8
	xor	ecx, ecx
	mov	cl, 8
	rep	movsd

;csftdf_check_disk_free_size_2:
	mov	edx, eax ; Old destination file size

	;mov	eax, [SourceFile_DirEntry+DirEntry_FileSize]
	mov	eax, [csftdf_filesize] ; 23/03/2016

	;;sub	ecx, ecx ; 0
	;mov 	esi, Logical_DOSDisks
	;mov	ch, [DestinationFile_Drv]
	;add	esi, ecx
	;
	;mov	[csftdf_df_drv_dt], esi

	mov	esi, [csftdf_df_drv_dt] ; 23/03/2016

	mov	cx, [esi+LD_BPB+BytesPerSec] ; 17, LD_BPB + 0Bh
	add	edx, ecx ; + 512
	add	eax, ecx ; + 512
	dec	edx ; old file size + 511 (round up)
	dec	eax ; new file size + 511 (round up)
	neg	ecx ; -512 ; 0FFFFFE00h
	and	edx, ecx ; = old sector count * 512
	and	eax, ecx ; = new sector count * 512

	sub	eax, edx ; new file size - old file size (on disk)
	jna	short csftdf_check_disk_free_size_ok

	neg	ecx ; 512 (bytes per sector) ; 200h
	; check free space for additional sectors
	; eax = number of additional sectors * bytes per sector
	; esi = Logical DOS drive number (of destination disk)
        jmp	csftdf_check_disk_free_size_3

csftdf_check_disk_free_size_ok:
	; 18/03/2016
csftdf_df_check_copy_cmd_phase:
	mov	al, [copy_cmd_phase]
	cmp	al, 1
	jne	short csftdf2_check_cdrv

	xor	eax, eax
	mov	[copy_cmd_phase], al ; 0

	mov	dl, [DestinationFileFound]
	mov	dh, [SourceFile_DirEntry+11] ; Attributes

csftdf_return:
	retn

; Phase 2
csftdf_ph2:
	; 29/07/2022
csftdf2_check_cdrv:
	; 18/03/2016
	; Here, destination drive and directory are ready !
	; (checking/restoring is not needed)
	; (Since at the end of the phase 1)

;	mov	dl, [DestinationFile_Drv]
;	cmp	dl, [Current_Drv]
;	je	short csftdf2_df_check_directory
;
;	call	change_current_drive
;	jc	short csftdf2_read_error
;
;csftdf2_df_check_directory:
;	mov	esi, DestinationFile_Directory
;	cmp	byte [esi], 20h
;	jna	short csftdf2_df_check_found_or_not
;
;csftdf2_df_change_directory:
;	inc	byte [Restore_CDIR]
;	xor	ah, ah ; CD_COMMAND sign -> 0
;	call	change_current_directory
;	jc	short csftdf2_stc_return
;
;;csftdf2_df_change_prompt_dir_string:
;;	call	change_prompt_dir_string

csftdf2_df_check_found_or_not:
	; 21/03/2016
	cmp	byte [DestinationFileFound], 0
	ja	short csftdf2_set_sf_percentage

csftdf2_create_file:
	mov	esi, DestinationFile_Name
	mov	eax, [csftdf_filesize]
	xor	cl, cl ; 0

	xor	ebx, ebx ; 0
	dec	ebx ; 0FFFFFFFFh 

	; INPUT ->
	; 	EAX -> File Size
	; 	ESI = ASCIIZ File name
	;	 CL = File attributes
	;	EBX = FFFFFFFFh -> empty file sign for FAT fs
	;	EBX <> FFFFFFFFh -> use file size for FAT fs 
	;
	; OUTPUT ->
	;	EAX = New file's first cluster
	;		 (0 for empty file) ; 29/08/2024
	;	ESI = Logical Dos Drv Descr. Table Addr.
	;	;EBX = CreateFile_Size address
	;	;ECX = Sectors per cluster (<256)
	;	;EDX = Directory Entry Index/Number (<65536)
	;	; 29/08/2024
	;	EBX = File Size (0 for a new, empty file)
	;	ECX = Directory Entry Index/Number (<2048)
	;	     (in directory cluster, not in directory)
	;	EDX = Directory Cluster Number (of the file)
	;
	;	cf = 1 -> error code in AL (EAX)

	call	create_file
	;pop	esi
	;jc	csftdf2_rw_error
	; 29/07/2022
	jnc	short csftdf2_create_file_OK
	jmp	csftdf2_rw_error

csftdf2_create_file_OK:

	mov	[csftdf_df_cluster], eax
	; 27/08/2024
	; eax = 0

	; 24/03/2016
	;mov	[DestinationFile_DirEntryNumber], dx
	; 29/08/2024
	; (these 3 parameters are needed for
	;  loading same dir entry for first cluster update)
	mov	[DestinationFile_DirCluster], edx
	mov	[DestinationFile_DirEntryNumber], cx

	; 30/08/2024
	; edx -> ecx
	; 21/03/2016
	mov	esi, Directory_Buffer
	;shl	edx, 5 ; 32 * index number
	shl	ecx, 5
	;add	esi, edx
	add	esi, ecx
	mov	edi, DestinationFile_DirEntry
	;mov	cl, 8 ; 32 bytes
	; 29/08/2024
	sub	ecx, ecx
	mov	cl,8
	rep	movsd

	;mov	cl, [esi] ; L.D.D.D.T.
	;mov	[DestinationFile_Drv], cl

csftdf2_set_sf_percentage:
	; 17/03/2016
	xor	eax, eax
	mov 	[csftdf_percentage], al ; 0, reset

	mov	[csftdf_sf_rbytes], eax ; 0, reset
	mov	[csftdf_df_wbytes], eax ; 0, reset

	mov	ah, [SourceFile_Drv]
	mov	esi, Logical_DOSDisks
	add	esi, eax

	mov	[csftdf_sf_drv_dt], esi ; 23/03/2016

	mov	dx, [SourceFile_DirEntry+DirEntry_FstClusHI]
	shl	edx, 16
	mov	dx, [SourceFile_DirEntry+DirEntry_FstClusLO]
	mov	[csftdf_sf_cluster], edx

	; 16/03/2016
	; Note: Singlix FS boot sector parameters (for cluster
	;	related calculations) has same offset
	;	values from LD_BPB as in FAT file system.
	;	[esi+LD_BPB+SecPerClust] is 1 for Singlix FS.
	;
	movzx	ecx, byte [esi+LD_BPB+SecPerClust]
	mov	[SourceFile_SecPerClust], cl

	; 17/03/2016
	cmp	[esi+LD_FATType], ch ; 0
	ja	short csftdf2_set_sf_percent_rsize1

	mov	eax, 65536 ; read/write buffer size for Singlix FS
	jmp	short csftdf2_set_sf_percent_rsize2

csftdf2_set_sf_percent_rsize1:
	mov	ax, [esi+LD_BPB+BytesPerSec]
	mul	ecx
	;sub	edx, edx
csftdf2_set_sf_percent_rsize2:
	mov	[csftdf_r_size], eax

csftdf2_set_df_percentage:
	;sub	eax, eax
	;mov	ah, [DestinationFile_Drv]
	;mov	edi, Logical_DOSDisks
	;add	edi, eax
	;mov	[csftdf_df_drv_dt], edi ; 17/03/2016

	mov	edi, [csftdf_df_drv_dt] ; 23/03/2016

	; 16/03/2016
	; Note: Singlix FS boot sector parameters (for cluster
	;	related calculations) has same offset
	;	values from LD_BPB as in FAT file system.
	;	[edi+LD_BPB+SecPerClust] is 1 for Singlix FS.
	;	
	;movzx	ecx, byte [edi+LD_BPB+SecPerClust]
	mov	cl, [edi+LD_BPB+SecPerClust]
	mov	[DestinationFile_SecPerClust], cl

	; 17/03/2016
	cmp	[edi+LD_FATType], ch ; 0
	ja	short csftdf2_set_df_percent_wsize1

	mov	eax, 65536 ; read/write buffer size for Singlix FS
	jmp	short csftdf2_set_df_percent_wsize2

csftdf2_set_df_percent_wsize1:
	movzx	eax, word [edi+LD_BPB+BytesPerSec]
	mul	ecx
	;sub	edx, edx
csftdf2_set_df_percent_wsize2:
	mov	[csftdf_w_size], eax

	mov	eax, [csftdf_filesize]

	cmp	eax, 65536 ; 64KB	; small file
	jb	short csftdf2_load_file ; do not display percentage

csftdf2_reset_wf_percent_ptr_chk_64k:
	mov	dl, 1 ; 25/03/2016

	cmp	eax, 65536*4 ; 256KB
	jnb	short csftdf2_enable_percentage_display ; big file

	; 64-128KB file size for floppy disks
	cmp	byte [SourceFile_Drv], dl ; 1 ; read from floppy disk ?
	jna	short csftdf2_enable_percentage_display

	cmp	byte [DestinationFile_Drv], dl ; 1 ; write to floppy disk ?
	ja	short csftdf2_load_file

csftdf2_enable_percentage_display:
	mov	[csftdf_percentage], dl ; 1

csftdf2_load_file:
	; 13/05/2016
	; 19/03/2016
	; 18/03/2016
	; 17/03/2016
	mov	ah, 0Fh
	call	_int10h
	; 13/05/2016
	mov	[csftdf_videopage], bh ; active video page
	mov	ah, 03h
	call	_int10h
	mov	[csftdf_cursorpos], dx

	sub	eax, eax
	mov	[csftdf_rw_err], al ; 0

; ///
csftdf_sf_amb: ; 15/03/2016
	mov	ecx, [csftdf_filesize]	; 23/03/2016

	; TRDOS 386 (TRDOS v2.0)
	; Allocate contiguous memory block for loading the file

	;mov	ecx, [SourceFile_DirEntry+DirEntry_FileSize]

	;sub	eax, eax ; First free memory aperture

	; eax = 0 (Allocate memory from the beginning)
	; ecx = File (Allocation) size in bytes

; 31/08/2024 - temporary
;%if 0
	call	allocate_memory_block
	jnc	short loc_check_sf_save_loading_parms
;%endif

csftdf_use_cluster_buff: ; 27/08/2024

	sub	eax, eax
	sub	ecx, ecx

loc_check_sf_save_loading_parms:
	mov	[csftdf_sf_mem_addr], eax ; loading address
	mov	[csftdf_sf_mem_bsize], ecx ; block size
; ///
	; 19/03/2016
	mov	esi, [csftdf_sf_drv_dt] ; logical dos drv desc. tbl.

	; 17/03/2016
	or	eax, eax ; contiguous free memory block address
	;jz	csftdf2_read_sf_cluster
	; 29/07/2022
	jnz	short csftdf2_1
	jmp	csftdf2_read_sf_cluster

csftdf2_1:
	; 18/03/2016
	mov	ebx, [csftdf_sf_mem_addr] ; memory block address

	cmp	byte [esi+LD_FATType], 0
        ;jna	csftdf2_load_fs_file
	; 29/07/2022
	ja	short csftdf2_load_fat_file
	jmp	csftdf2_load_fs_file

csftdf2_load_fat_file:
	push	ebx ; *

csftdf2_load_fat_file_next:
	mov	esi, msg_reading
	call	print_msg

	cmp	byte [csftdf_percentage], 0
	jna	short csftdf2_load_fat_file_1

	call	csftdf2_print_percentage ; 19/03/2016

csftdf2_load_fat_file_1:
	mov	esi, [csftdf_sf_drv_dt]
	pop	ebx ; *

csftdf2_load_fat_file_2:
	call	csftdf2_read_fat_file_sectors ; 19/03/2016
	;jc	csftdf2_rw_error ; eocc! or disk error!
	; 29/07/2022
	jnc	short csftdf2_load_fat_file_3
	jmp	csftdf2_rw_error
csftdf2_load_fat_file_3:
	or	edx, edx ; edx > 0 -> EOF
	jnz	short csftdf2_load_fat_file_ok

	cmp	byte [csftdf_percentage], 0
	jna	short csftdf2_load_fat_file_2

	push	ebx ; *

	; Set cursor position
	; AH= 02h, BH= Page Number, DH= Row, DL= Column
	mov	bh, [csftdf_videopage]
	mov	dx, [csftdf_cursorpos]
	mov	ah, 2
	call	_int10h
	jmp	short csftdf2_load_fat_file_next

csftdf2_load_fat_file_ok:
	cmp	byte [csftdf_percentage], 0
	;jna	csftdf2_save_file ; 25/03/2016
	; 29/07/2022
	ja	short csftdf2_2
	jmp	csftdf2_save_file
csftdf2_2:
	; "Reading... 100%"
	mov	edi, percentagestr
	mov	al, '1'
	stosb
	mov	al, '0'
	stosb
	stosb

	mov	bh, [csftdf_videopage]
	mov	dx, [csftdf_cursorpos]
	mov	ah, 2
	call	_int10h

	mov	esi, msg_reading
	call	print_msg

	mov	esi, percentagestr
	call	print_msg

        jmp	csftdf2_save_file ; 25/03/2016

csftdf2_print_percentage:
	; 09/12/2017
	; 19/03/2016
	; 18/03/2016
	mov	al, 20h
	mov	edi, percentagestr
	stosb
	stosb
	mov	eax, [csftdf_sf_rbytes]
	;mov	edx, 100
	; 29/07/2022
	sub	edx, edx
	mov	dl, 100
	mul	edx
	mov	ecx, [csftdf_filesize]
	div	ecx
	mov	cl, 10
	div	cl
	add	ah, '0'
	mov	[edi], ah
	and	al, al
	jz	short csftdf2_print_percent_1
	dec	edi
	;cbw
	sub	ah, ah ; 09/12/2017
	div	cl
	add	ah, '0'
	mov	[edi], ah
	;and	al, al
	;jz	short csftdf2_print_percent_1
	;dec	edi
	;mov	[edi], '1' ; 100%

csftdf2_print_percent_1:
	mov	esi, percentagestr
	;call	print_msg
	;retn
	jmp	print_msg

csftdf2_read_file_sectors:
	; 19/03/2016
	cmp	byte [esi+LD_FATType], 0
	;jna	csftdf2_read_fs_file_sectors
	; 29/07/2022
	ja	short csftdf2_read_fat_file_sectors
	jmp	csftdf2_read_fs_file_sectors

csftdf2_read_fat_file_sectors:
	; 19/03/2016
	; 18/03/2016
	; return:
	;   CF = 0 & EDX > 0 -> END OF FILE
	;   CF = 0 & EDX = 0 -> not EOF
	;   CF = 1 -> read error (error code in AL)

csftdf2_read_fat_file_secs_0:
	mov	edx, [csftdf_filesize]
	sub	edx, [csftdf_sf_rbytes]
	cmp	edx, [csftdf_r_size]
	jnb	short csftdf2_read_fat_file_secs_1
	mov	[csftdf_r_size], edx

csftdf2_read_fat_file_secs_1:
	mov	eax, [csftdf_r_size]
	sub	edx, edx
	movzx	ecx, word [esi+LD_BPB+BytesPerSec]
	add	eax, ecx
	dec	eax
	div	ecx
	mov	ecx, eax ; sector count
	mov	eax, [csftdf_sf_cluster]

	; EBX = memory block address (current)

	call	read_fat_file_sectors
	jc	short csftdf2_read_fat_file_secs_3

	; EBX = next memory address

	mov	eax, [csftdf_sf_rbytes]
	add	eax, [csftdf_r_size]
	mov	edx, [csftdf_filesize]
	cmp	eax, edx
	jnb	short csftdf2_read_fat_file_secs_3 ; edx > 0
	mov	[csftdf_sf_rbytes], eax

	push	ebx ; *
	; get next cluster (csftdf_r_size! bytes)
	mov	eax, [csftdf_sf_cluster]
	call	get_next_cluster
	pop	ebx ; *
	jnc	short csftdf2_read_fat_file_secs_2

	; 15/10/2016
	; Disk read error instead of drv not ready err
	mov	eax, 17 ; Read error !
	retn

csftdf2_read_fat_file_secs_2:
	sub	edx, edx ; 0
	mov	[csftdf_sf_cluster], eax ; next cluster

csftdf2_read_fat_file_secs_3:
	retn

csftdf2_read_sf_cluster:
	; 19/03/2016
	mov	ebx, Cluster_Buffer ; buffer address (64KB)

	cmp	byte [csftdf_percentage], 0
	jna	short csftdf2_read_sf_clust_2

	push	ebx ; *

csftdf2_read_sf_clust_next:
	call	csftdf2_print_percentage

csftdf2_read_sf_clust_0:
	mov	esi, [csftdf_sf_drv_dt]
csftdf2_read_sf_clust_1:
	pop	ebx ; *

csftdf2_read_sf_clust_2:
	mov	edx, ebx
	add	edx, [csftdf_r_size]
	cmp	edx, Cluster_Buffer + 65536
	ja	short csftdf2_write_df_cluster

	call	csftdf2_read_file_sectors ; 19/03/2016
	;jc	csftdf2_save_fat_file_err2 ; eocc! or disk error!
	; 29/07/2022
	jc	short csftdf2_3

	or	edx, edx ; edx > 0 -> EOF
	jnz	short csftdf2_write_df_cluster

	cmp	byte [csftdf_percentage], 0
	jna	short csftdf2_read_sf_clust_2

	push	ebx ; *

	; Set cursor position
	; AH= 02h, BH= Page Number, DH= Row, DL= Column
	mov	bh, [csftdf_videopage]
	mov	dx, [csftdf_cursorpos]
	mov	ah, 2
	call	_int10h
	jmp	short csftdf2_read_sf_clust_next

csftdf2_write_df_cluster:

; 31/08/2024
%if 1
	; 27/08/2024
	; 19/03/2016
	;mov	esi, [csftdf_df_drv_dt] ; (!)
	;mov	ebx, Cluster_Buffer ; buffer address (64KB)
	;;;
	; 27/08/2024
	call	csftdf2_update_df_fclust
	; 31/08/2024
	jnc	short csftdf2_update_df_fclust_ok
	jmp	csftdf2_rw_error

csftdf2_update_df_fclust_ok:
%endif
	; esi = [csftdf_df_drv_dt]

	;mov	esi, [csftdf_df_drv_dt]
	mov	ebx, Cluster_Buffer ; buffer address (64KB)

csftdf2_write_df_clust_next:
	call	csftdf2_write_file_sectors ; 19/03/2016
	;jc	csftdf2_save_fat_file_err2 ; eocc! or disk error!
	; 29/07/2022
	jnc	short csftdf2_4
csftdf2_3:
	jmp	csftdf2_save_fat_file_err2
csftdf2_4:
	or	edx, edx ; edx > 0 -> EOF
	jnz	short csftdf2_rw_f_clust_ok

	cmp	ebx, Cluster_Buffer + 65536
	jb	short csftdf2_write_df_clust_next

	; 31/08/2024
	mov	esi, [csftdf_sf_drv_dt]

	jmp	csftdf2_read_sf_cluster ; 31/08/2024

csftdf2_rw_f_clust_ok:
	cmp	byte [csftdf_percentage], 0
	;jna	csftdf2_save_fat_file_4 ; 25/03/2016
	; 29/07/2022
	jna	short csftdf2_5

	; "100%"
	mov	edi, percentagestr
	mov	al, '1'
	stosb
	mov	al, '0'
	stosb
	stosb

	mov	bh, [csftdf_videopage]
	mov	dx, [csftdf_cursorpos]
	mov	ah, 2
	call	_int10h

	mov	esi, percentagestr
	call	print_msg
csftdf2_5:
        jmp     csftdf2_save_fat_file_4

csftdf2_load_fs_file:
	; temporary - 18/03/2016
        jmp     csftdf2_read_error

csftdf2_write_file_sectors:
	; 31/08/2024
	; 30/08/2024
	; 19/03/2016
	cmp	byte [esi+LD_FATType], 0
	;jna	csftdf2_write_fs_file_sectors
	; 29/07/2022
	ja	short csftdf2_write_fat_file_sectors
	jmp	csftdf2_write_fs_file_sectors

csftdf2_write_fat_file_sectors:
	; 30/08/2024
	; 19/03/2016
	; 18/03/2016
	; return:
	;   CF = 0 & EDX > 0 -> END OF FILE
	;   CF = 0 & EDX = 0 -> not EOF
	;   CF = 1 -> write error (error code in AL)

csftdf2_write_fat_file_secs_0:
	mov	edx, [csftdf_filesize]
	sub	edx, [csftdf_df_wbytes]
	cmp	edx, [csftdf_w_size]
	jnb	short csftdf2_write_fat_file_secs_1
	mov	[csftdf_w_size], edx

csftdf2_write_fat_file_secs_1:

; 31/08/2024
%if 0
	;;;
	; 30/08/2024
	mov	eax, [csftdf_df_cluster]
	and	eax, eax
	jnz	short csftdf2_write_fat_file_secs_@
	push	ebx
	call	add_new_cluster
	pop	ebx
	jc	short csftdf2_write_fat_file_secs_5
	mov	[csftdf_df_cluster], eax
csftdf2_write_fat_file_secs_@:
	;;;
%endif

	mov	eax, [csftdf_w_size]
	sub	edx, edx
	movzx	ecx, word [esi+LD_BPB+BytesPerSec]
	add	eax, ecx
	dec	eax
	div	ecx
	mov	ecx, eax ; sector count
	mov	eax, [csftdf_df_cluster]

	; EBX = memory block address (current)

	call	write_fat_file_sectors
	jc	short csftdf2_write_fat_file_secs_4

	; EBX = next memory address

	mov	eax, [csftdf_df_wbytes]
	add	eax, [csftdf_w_size]
	mov	edx, [csftdf_filesize]
	cmp	eax, edx
	jnb	short csftdf2_write_fat_file_secs_4
	mov	[csftdf_df_wbytes], eax
	;
	mov	[DestinationFile_DirEntry+DirEntry_FileSize], eax


	push	ebx ; *

	; 30/08/2024
	mov	eax, [csftdf_df_cluster] ; last cluster

	cmp	byte [DestinationFileFound], 1
	jb	short csftdf2_write_fat_file_secs_2

	; get next cluster (csftdf_w_size! bytes)
	;mov	eax, [csftdf_df_cluster]
	call	get_next_cluster
	jnc	short csftdf2_write_fat_file_secs_3

	and	eax, eax ; end of cluster chain!?
	jnz	short csftdf2_write_fat_file_secs_5 ; disk error !

csftdf2_write_fat_file_secs_2:
	;mov	eax, [csftdf_df_cluster] ; last cluster
	; 30/08/2024
	call	add_new_cluster
	jc	short csftdf2_write_fat_file_secs_5

	; NOTE: Destination file size may be bigger than
	; source file size when the last reading fails after here.
	; (The last -empty- cluster of destination file must be
	; truncated and LMDT must be current date&time for partial
	; copy result!)
	mov	edx, [csftdf_w_size] ; < = bytes per cluster
	add	[DestinationFile_DirEntry+DirEntry_FileSize], edx

csftdf2_write_fat_file_secs_3:
	; 30/08/2024
	;pop	ebx ; *
	sub	edx, edx ; 0
	mov	[csftdf_df_cluster], eax ; next cluster

	; 30/08/2024
	pop	ebx ; *

csftdf2_write_fat_file_secs_4:
	retn

csftdf2_write_fat_file_secs_5:
	pop	ebx ; *
	mov	eax, 18 ; Write error !
	; 16/10/2016 (1Dh -> 18)
	retn

csftdf2_save_file:
	; 31/08/2024
	; 26/08/2024, 30/08/2024
	; 09/12/2017
	; 19/03/2016, 25/03/2016
	; 18/03/2016 
	mov	esi, [csftdf_df_drv_dt] ; logical dos drv desc. tbl.
	mov	ebx, [csftdf_sf_mem_addr] ; memory block address

	cmp	byte [esi+LD_FATType], 0
	;jna	csftdf2_save_fs_file
	; 29/07/2022
	ja	short csftdf2_save_fat_file
	jmp	csftdf2_save_fs_file

csftdf2_save_fat_file:

; 31/08/2024
%if 1
	; 30/08/2024
	push	ebx ; *
	;;;
	; 27/08/2024
	;call	csftdf2_update_df_fclust
	call	csftdf2_update_df_fclust_@ ; 31/08/2024
	pop	ebx ; *
	jc	short csftdf2_save_fat_file_err
	; esi = [csftdf_df_drv_dt] ; (*)
	;;;
%endif

	cmp	byte [csftdf_percentage], 0
	ja	short csftdf2_save_fat_file_0 ; 30/08/2024

	push	ebx ; **

	; Set cursor position
	; AH= 02h, BH= Page Number, DH= Row, DL= Column
	mov	bh, [csftdf_videopage]
	mov	dx, [csftdf_cursorpos]
	mov	ah, 2
	call	_int10h

	mov	esi, msg_writing
	call	print_msg

csftdf2_save_fat_file_next:
	pop	ebx ; **

	; 31/08/2024
	mov	esi, [csftdf_df_drv_dt]

csftdf2_save_fat_file_0: ; 30/08/2024

csftdf2_save_fat_file_1:
	call	csftdf2_write_file_sectors ; 19/03/2016
	;jc	csftdf2_rw_error ; eocc! or disk error!
	; 29/07/2022
	jnc	short csftdf2_6
csftdf2_save_fat_file_err: ; 27/08/2024
	jmp	csftdf2_rw_error

csftdf2_6:
	or	edx, edx ; edx > 0 -> EOF
        jnz     short csftdf2_save_fat_file_3 ; 25/03/2016

	cmp	byte [csftdf_percentage], 0
	jna	short csftdf2_save_fat_file_1

	mov	al, 20h
	mov	edi, percentagestr
	stosb
	stosb
	mov	eax, [csftdf_df_wbytes]
	;mov	edx, 100
	; 29/07/2022
	xor	edx, edx
	mov	dl, 100
	mul	edx
	mov	ecx, [csftdf_filesize]
	div	ecx
	mov	cl, 10
	div	cl
	add	ah, '0'
	mov	[edi], ah
	and	al, al
	jz	short csftdf2_save_fat_file_2
	dec	edi
	;cbw
	xor	ah, ah ; 09/12/2017
	div	cl
	add	ah, '0'
	mov	[edi], ah
	;and	al, al
	;jz	short csftdf2_save_fat_file_2
	;dec	edi
	;mov	[edi], '1' ; 100%

csftdf2_save_fat_file_2:
	push	ebx ; *

	call	csftdf2_print_wr_percentage ; 25/03/2016

        jmp     csftdf2_save_fat_file_next

csftdf2_print_wr_percentage:
	; Set cursor position
	; AH= 02h, BH= Page Number, DH= Row, DL= Column
	mov	bh, [csftdf_videopage]
	mov	dx, [csftdf_cursorpos]
	mov	ah, 2
	call	_int10h

	mov	esi, msg_writing
	call	print_msg

	mov	esi, percentagestr
	;call	print_msg
	;retn
	jmp	print_msg

csftdf2_save_fat_file_3:
	cmp	byte [csftdf_percentage], 0
        ;jna	csftdf2_save_fat_file_4 ; 25/03/2016
	; 29/07/2022
	;ja	short csftdf2_7
	;jmp	csftdf2_save_fat_file_4
	; 31/08/2024
	jna	short csftdf2_save_fat_file_4

csftdf2_7:
	; "100%"
	mov	edi, percentagestr
	mov	al, '1'
	stosb
	mov	al, '0'
	stosb
	stosb

	call	csftdf2_print_wr_percentage

csftdf2_save_fat_file_4:
	cmp	byte [DestinationFileFound], 0
	jna	short csftdf2_save_fat_file_6

	mov	esi, [csftdf_df_drv_dt] ; 31/03/2016

	mov	eax, [csftdf_df_cluster] ; last cluster
	call	get_next_cluster
	jc	short csftdf2_save_fat_file_6 ; eocc! or disk error!

	mov	eax, [csftdf_df_cluster] ; last cluster
	; 29/08/2024
	xor	ecx, ecx
	;mov	[FAT_ClusterCounter], ecx ; 0 ; reset
	dec	ecx ; 0FFFFFFFFh
	;shr	ecx, 4 ; 28 bit ; 0FFFFFFFh
	; 29/08/2024
	; (update_cluster will save 28 bit cluster value)
	;mov	ecx, 0FFFFFFFh
	call	update_cluster
	jc	short csftdf2_save_fat_file_6 ; really last cluster!?

	mov	[csftdf_df_cluster], eax ; next cluster
	
	; byte [FAT_BuffValidData] = 2
	call	save_fat_buffer
	jnc	short csftdf2_save_fat_file_5
	
	mov	edx, [csftdf_filesize]
	mov	[DestinationFile_DirEntry+DirEntry_FileSize], edx
	jmp	short csftdf2_save_fat_file_err3

csftdf2_save_fat_file_5:
	mov	eax, [csftdf_df_cluster]

	; EAX = First cluster to be truncated/unlinked
	; ESI = Logical dos drive description table address
	call	truncate_cluster_chain

csftdf2_save_fat_file_6:
	; 28/03/2016
	mov	esi, SourceFile_DirEntry+DirEntry_Attr ; +11 to + 18
	mov	edi, DestinationFile_DirEntry+DirEntry_Attr ; +11 to + 18
	movsb ; +11
	movsd ; +12 .. +15
	movsw ; +16 .. +17
		; + 18
	add	esi, 4
	add	edi, 4
	movsd	; DirEntry_WrtTime ; +22 .. +25

	mov	edx, [csftdf_filesize]
	mov	[DestinationFile_DirEntry+DirEntry_FileSize], edx

	call	convert_current_date_time
	; DX = Date in dos dir entry format
	; AX = Time in dos dir entry format
	jmp	short csftdf2_save_fat_file_7

	; 31/08/2024
;csftdf2_save_fat_file_err1:
	;pop	ebx ; *

csftdf2_save_fat_file_err2:
	mov	eax, [csftdf_df_wbytes]
	mov	edx, [DestinationFile_DirEntry+DirEntry_FileSize]
	cmp	edx, eax
	jna	short csftdf2_save_fat_file_err3
	mov	eax, [csftdf_df_cluster] ; last (empty) cluster
	; ESI = Logical dos drive description table address
	call	truncate_cluster_chain
	jc	short csftdf2_save_fat_file_err3
	mov	eax, [csftdf_df_wbytes]
	mov	[DestinationFile_DirEntry+DirEntry_FileSize], eax
csftdf2_save_fat_file_err3:
	call	convert_current_date_time
	; DX = Date in dos dir entry format
	; AX = Time in dos dir entry format
	mov	byte [DestinationFile_DirEntry+DirEntry_CrtTimeTenth], 0
	mov	[DestinationFile_DirEntry+DirEntry_CrtTime], ax
	mov	[DestinationFile_DirEntry+DirEntry_CrtDate], dx
	mov	[DestinationFile_DirEntry+DirEntry_WrtTime], ax
	mov	[DestinationFile_DirEntry+DirEntry_WrtDate], dx
	stc
csftdf2_save_fat_file_7:
	pushf
	mov	[DestinationFile_DirEntry+DirEntry_LastAccDate], dx
	mov	esi, DestinationFile_DirEntry
	mov	edi, Directory_Buffer
	movzx	ecx, word [DestinationFile_DirEntryNumber] ; (<2048)
	;shl	cx, 5 ; 32 * directory entry number
	; 29/07/2022
	shl	ecx, 5
	add	edi, ecx
	;mov	ecx, 8
	;mov	cx, 8
	; 31/08/2024
	; 29/07/2022
	sub	ecx, ecx
	;sub	ch, ch
	mov	cl, 8
	rep	movsd
	popf
	jnc	short csftdf2_write_file_OK

csftdf2_write_error:
	; 18/03/2016
	;mov	al, 1Dh ; write error
	; 31/08/2024
	mov	al, ERR_DRV_WRITE ; 18 ; write error
	jmp	short csftdf2_rw_error

	; 16/03/2016
csftdf2_read_error:
	mov	al, 17 ; ; Drive not ready or read error!
csftdf2_rw_error:
	mov	[csftdf_rw_err], al

csftdf2_write_file_OK:
	; 18/03/2016
	mov	byte [DirBuff_ValidData], 2
	call	save_directory_buffer

 	; Update last modification date&time of destination
	; file's (parent) directory
	call	update_parent_dir_lmdt
	;
	mov	eax, [csftdf_sf_mem_addr] ; start address

	and	eax, eax
	jnz	short csftdf2_dealloc_mblock

	; 31/08/2024
	;mov	ch, al ; 0 (Cluster r/w, not full loading)
csftdf2_dealloc_retn:
	;mov	cl, [csftdf_rw_err]
	;mov	eax, [csftdf_df_cluster]
	; 31/08/2024
	mov	al, [csftdf_rw_err]
	cmp	al, 1
	cmc
	; if al > 0 -> cf = 1
csftdf2_write_df_error:	; 31/08/2024
csftdf2_write_df_clust_@@: ; 31/08/2024
	retn

csftdf2_dealloc_mblock:
	mov	ecx, [csftdf_sf_mem_bsize] ; block size
	call	deallocate_memory_block
	; 31/08/2024
        ;mov	ch, 0FFh ; (File was full loaded at memory)
	jmp	short csftdf2_dealloc_retn

	;;;;
csftdf2_update_df_fclust:
	; 31/08/2024
	; 27/08/2024 - TRDOS 386 v2.0.9
	; update (directory entry of)
	; the new (created) destination file
	; with new first cluster (it was 0 before)
	
	mov	esi, [csftdf_df_drv_dt]	; (!)

csftdf2_update_df_fclust_@: ; 31/08/2024
	mov	eax, [csftdf_df_cluster]
	and	eax, eax
	jnz	short csftdf2_write_df_clust_@@
	; if eax = 0, add new cluster
; 31/08/2024
%if 0
	call	get_first_free_cluster
	jc	short csftdf2_write_df_error
	; EAX >= 2 and EAX < FFFFFFFFh is valid

	cmp	eax, 0FFFFFFFFh ; no free space
	jne	short csftdf2_write_df_clust_@
	inc	eax ; 0
	stc
	mov	al, 27h ; insufficient disk space
csftdf2_write_df_error:
	retn
%else
	; 31/08/2024
	call	add_new_cluster
	jc	short csftdf2_write_df_error
%endif

csftdf2_write_df_clust_@:
	; 31/08/2024
	mov	[csftdf_df_cluster], eax
	;
	; eax = (new) first cluster
	push	esi
	mov	esi, DestinationFile_DirEntry
	movzx	edi, word [DestinationFile_DirEntryNumber]
	shl	edi, 5 ; 32 * index number
	add	edi, Directory_Buffer
	rol	eax, 16
	mov	[esi+DirEntry_FstClusHI],ax
	ror	eax, 16
	mov	[esi+DirEntry_FstClusLO],ax
	mov	ecx, 8 ; 32 bytes
	rep	movsd
	pop	esi
	;
	mov	byte [DirBuff_ValidData], 2 ; change sign

; 31/08/2024
%if 0
	call	save_directory_buffer
	jc	short csftdf2_write_df_error
	;
	; +1 cluster is in use (as first cluster)
	mov	ebx, [esi+LD_FreeSectors]
	inc	ebx  ; 0FFFFFFFFh ? invalid ?
	jz	short csftdf2_write_df_clust_@@ ; yes
	xor	ebx, ebx
	mov	bl, [esi+LD_BPB+BPB_SecPerClust]
	sub	[esi+LD_FreeSectors], ebx
	;
csftdf2_write_df_clust_@@:
	retn
	;;;;
%else
	; 31/08/2024
	jmp	save_directory_buffer
%endif

csftdf2_save_fs_file:
	; 16/10/2016 (1Dh -> 18)
	; temporary - (21/03/2016)
	;mov	eax, 18 ; write error
	; 29/07/2022
	xor	eax, eax
	mov	al, 18
	stc
	retn

create_file:
	; 31/08/2024
	; 29/08/2024
	; 27/08/2024
	; 26/08/2024 (TRDOS 386 v2.0.9)
	; 30/07/2022
	; 29/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 16/10/2016
	; 24/03/2016, 31/03/2016
	; 20/03/2016, 21/03/2016, 23/03/2016
	; 19/03/2016 (TRDOS 396 = TRDOS v2.0)
	; 03/09/2011 (FILE.ASM, 'proc_create_file')
	; 09/08/2010
	;
	; INPUT ->
	; 	EAX = File Size
	; 	ESI = ASCIIZ File Name
	; 	CL = File Attributes 
	;	EBX = FFFFFFFFh -> create empty file
	;			 (only for FAT fs)
	; OUTPUT ->
	;	EAX = New file's first cluster
	;		 (0 for empty file) ; 29/08/2024
	;	ESI = Logical Dos Drv Descr. Table Addr.
	;	;EBX = CreateFile_Size address
	;	;ECX = Sectors per cluster (<256)
	;	;EDX = Directory Entry Index/Number (<65536)
	;	; 29/08/2024
	;	EBX = File Size (0 for a new, empty file)
	;	ECX = Directory Entry Index/Number (<2048)
	;	     (in directory cluster, not in directory)
	;	EDX = Directory Cluster Number (of the file)
	;	
	;	cf = 1 -> error code in AL (EAX)
	;
	; (Modified registers: eax, ebx, ecx, edx, esi, edi)
	;

;	test	cl, 18h (directory or volume name)
;	jnz	short loc_createfile_access_denied
	and	cl, 07h ; S, H, R
        mov     [createfile_attrib], cl

	mov	ecx, ebx
	mov	ebx, esi ; ASCIIZ File Name address
	sub	edx, edx
        mov     dh, [Current_Drv]
        mov     esi, Logical_DOSDisks
	add	esi, edx

	mov	[createfile_UpdatePDir], dl ; 0 ; 31/03/2016

	; LD_DiskType = 0 for write protection (read only)
	cmp	byte [esi+LD_DiskType], 1 ; 0 = Invalid
	jnb	short loc_createfile_check_file_sytem
	; 16/10/2016 (TRDOS Error code: 30, disk write protected)
	;mov	eax, 30 ; 13h, MSDOS err : Disk write-protected
	;mov	dx, 0
	; 29/07/2022
	xor	edx, edx ; 0
	xor	eax, eax ; 0
	mov	al, 30
	stc
	; err retn: EDX = 0, EBX = File name offset
	; ESI -> Dos drive description table address
	retn

;loc_createfile_access_denied:
;	mov	eax, 05h ; access denied (invalid attributes input)
;	stc
;	retn

loc_createfile_check_file_sytem:
	; 27/08/2024
	mov	[createfile_size], eax
	;
	cmp	byte [esi+LD_FATType], 1
	jnb	short loc_createfile_chk_empty_FAT_file_sign1

	; 27/08/2024
	;mov	[createfile_size], eax
	; ESI = Logical Dos Drive Description Table address
	; EBX = ASCIIZ File Name address
	jmp	create_fs_file

loc_createfile_chk_empty_FAT_file_sign1:
	; ECX = FFFFFFFFh -> create empty file if drive has FAT fs
	inc	ecx
	jnz	short loc_createfile_chk_empty_FAT_file_sign2

	mov	[createfile_size], ecx ; 0  ; empty file

loc_createfile_chk_empty_FAT_file_sign2:
	; 23/03/2016
	mov	cx, [esi+LD_BPB+BytesPerSec]
	mov	[createfile_BytesPerSec], cx

	; EBX = ASCIIZ File Name address
	movzx	edx, byte [esi+LD_BPB+SecPerClust]
	mov	[createfile_SecPerClust], dl
	mov	ecx, [esi+LD_FreeSectors]
	cmp	ecx, edx ; byte [createfile_SecPerClust]
	jnb	short loc_create_fat_file

loc_createfile_insufficient_disk_space:
	mov	eax, 27h
loc_createfile_gffc_retn:
	retn

loc_create_fat_file:
	mov	[createfile_Name_Offset], ebx
	mov	[createfile_FreeSectors], ecx

	; 27/08/2024
;loc_createfile_gffc_1:
	;call	get_first_free_cluster
	;jc	short loc_createfile_gffc_retn
	;
	;mov	[createfile_FFCluster], eax

loc_createfile_locate_ffe_on_directory:
	; Current directory fcluster <> Directory buffer cluster
	; Current directory will be reloaded by
	; 'locate_current_dir_file' procedure
	;
	; ESI = Logical Dos Drv Desc. Table Adress
	push	esi ; *
	xor	eax, eax

	mov	[FAT_ClusterCounter], eax ; 0
	; 21/03/2016
	mov	[createfile_wfc], al ; 0

 	mov	ecx, eax
	dec	cx ; FFFFh
	; CX = FFFFh -> find first deleted or free entry
	; ESI would be ASCIIZ filename address if the call
	; would not be for first free or deleted dir entry
	call	locate_current_dir_file
	;jnc	loc_createfile_set_ff_dir_entry
	; 29/07/2022
	jc	short loc_createfile_locate_file_err
	; 26/08/2024
	; cl = 0FFh, ch=0 or ch=0E5h
	; 27/08/2024
	;xor	ecx, ecx ; 26/08/2024
	jmp	loc_createfile_set_ff_dir_entry

loc_createfile_locate_file_err: ; 29/07/2022
	pop	esi ; *
	; ESI = Logical DOS Drv. Description Table Address
	cmp	eax, 2
	je	short loc_createfile_add_new_cluster
loc_createfile_locate_file_stc_retn:
	stc
	retn

loc_createfile_add_new_cluster:
	cmp	byte [Current_FATType], 2
	;cmp	byte [esi+LD_FATType], 2
	ja	short loc_createfile_add_new_cluster_check_fsc
	cmp	byte [Current_Dir_Level], 1
	;cmp	byte [esi+LD_CDirLevel], 1
	jnb	short loc_createfile_add_new_cluster_check_fsc

	;mov	eax, 12
	mov	al, 12 ; No more files 

loc_createfile_anc_retn:
	retn

loc_createfile_add_new_cluster_check_fsc:
	mov	ecx, [createfile_FreeSectors]
	movzx	eax, byte [createfile_SecPerClust]
	;shl	ax, 1 ; AX = 2 * AX
	; 29/07/2022
	shl	eax, 1
	cmp	ecx, eax
        jb	short loc_createfile_insufficient_disk_space

loc_createfile_add_new_subdir_cluster:
	mov	edx, [DirBuff_Cluster]
	mov	[createfile_LastDirCluster], edx

	; 27/08/2024
	;;;
	call	get_first_free_cluster
	jc	short loc_createfile_anc_retn
	mov	[createfile_FFCluster], eax
	;;;
	; 27/08/2024
	;;;
	;mov	eax, [createfile_FFCluster]
	; eax = cluster address of the new directory sector
	;;;

	call	load_FAT_sub_directory
	jc	short loc_createfile_anc_retn

	; 27/08/2024
	; [DirBuff_Cluster] = [createfile_FFCluster]

pass_createfile_add_new_subdir_cluster:
	; clear directory buffer (new dir sector) ; 27/08/2024
	;movzx	eax, word [esi+LD_BPB+BytesPerSec]
	movzx	eax, word [createfile_BytesPerSec] ; 23/03/2016
	; ecx = directory buffer sector count 
	;	(sectors per cluster)
	mul	ecx
	mov	ecx, eax 
		; (bytes per cluster or directory buffer size)
	shr	ecx, 2 ; dword count
	sub	eax, eax ; 0
	rep	stosd
	;
	mov	byte [DirBuff_ValidData], 2
	call	save_directory_buffer
	jc	short loc_createfile_anc_retn

loc_createfile_save_added_subdir_cluster:
	mov	eax, [createfile_LastDirCluster]
	mov	ecx, [createfile_FFCluster]
	call	update_cluster
	jnc	short loc_createfile_save_fat_buffer_0
	or	eax, eax ; EAX = 0 -> cluster value is 0 or eocc
	jnz	short loc_createfile_save_fat_buffer_stc_retn

loc_createfile_save_fat_buffer_0:
	mov	eax, [createfile_FFCluster]
	mov	[createfile_LastDirCluster], eax
	;mov	ecx, 0FFFFFFFh ; 28 bit
	; 29/08/2024
	; (update_cluster will save 28 bit cluster value)
	;mov	ecx, 0FFFFFFFFh
	sub	ecx, ecx
	dec	ecx ; 0FFFFFFFFh
	call	update_cluster
	jnc	short loc_createfile_save_fat_buffer_1
	or	eax, eax ; Was it EOF ?
	jz	short loc_createfile_save_fat_buffer_1 ; yes

loc_createfile_save_fat_buffer_stc_retn:
	stc
loc_createfile_save_fat_buffer_retn:
loc_createfile_gffc_2_stc_retn:
	retn

loc_createfile_save_fat_buffer_1:
	; byte [FAT_BuffValidData] = 2
	call	save_fat_buffer
	jc	short loc_createfile_save_fat_buffer_retn

	cmp	byte [FAT_ClusterCounter], 1
	jb	short loc_createfile_save_fat_buffer_2

	; ESI = Logical DOS Drive Description Table address
	mov	eax, [FAT_ClusterCounter]

	mov	byte [FAT_ClusterCounter], 0 ; 21/03/2016

	mov	bx, 0FF01h ; add free clusters 
	call	calculate_fat_freespace

	;inc	eax ; 0FFFFFFFFh -> 0 ; recalculation is needed!
	;jnz	short loc_createfile_save_fat_buffer_2

	; ecx > 0 -> Recalculation is needed
	or	ecx, ecx 
	jz	short loc_createfile_save_fat_buffer_2

	mov	bx, 0FF00h ; recalculate free space
	call	calculate_fat_freespace

loc_createfile_save_fat_buffer_2:
	;call	update_parent_dir_lmdt

	; 27/08/2024
;loc_createfile_gffc_2:
	;call	get_first_free_cluster
	;jc	short loc_createfile_gffc_2_stc_retn
	;
	;mov	[createfile_FFCluster], eax
	;

	mov	eax, [createfile_LastDirCluster]

	call	load_FAT_sub_directory
	jc	short loc_createfile_gffc_2_stc_retn

	mov	edi, Directory_Buffer

	; 27/08/2024
	;sub	bx, bx ; directory entry index/number = 0
	sub	ebx, ebx
	; 29/07/2022
	;sub	ecx, ecx

	push	esi ; * ; 23/03/2016

loc_createfile_set_ff_dir_entry:
	;;;
	; 29/08/2024
	mov	eax, [DirBuff_Cluster]
	mov	[createfile_LastDirCluster], eax
	;;;

	; 27/08/2024
	mov	[createfile_DirIndex], bx
	; 29/07/2022
	;mov	[createfile_DirIndex], cx ; 0

	;;;
	mov	eax, [createfile_size]
	or	eax, eax 
	jz	short loc_createfile_sffc
	; 31/08/2024
	mov	esi, [esp] ; * ; LDDDT address
	call	get_first_free_cluster
	jc	short loc_createfile_gffc_2_stc_retn
loc_createfile_sffc:
	mov	[createfile_FFCluster], eax
	sub	ecx, ecx ; 0
	;;;

        ; EDI = Directory entry address
	mov	esi, [createfile_Name_Offset]
	;;;
	; 27/08/2024
	;mov	eax, [createfile_FFCluster]
	;;;
	mov	[createfile_Cluster], eax ; 24/03/2016
	;mov	ch, 0FFh
        ; 29/07/2022
	; ecx = 0
	dec	ch ; 0 -> 0FFh
	mov	cl, [createfile_attrib] ; file attributes
	; CH > 0 -> File size is in [EBX]
	mov	ebx, createfile_size

	call	make_directory_entry

	pop	esi ; * ; ESI = Logical Dos Drv Desc. Table address

	mov	byte [DirBuff_ValidData], 2
	call	save_directory_buffer
	jc	short loc_createfile_set_ff_dir_entry_retn

	mov	byte [createfile_UpdatePDir], 1 ; 31/03/2016

loc_createfile_get_set_write_file_cluster:
	mov	eax, [createfile_size]
	or	eax, eax ; 0 ?
	jnz	short loc_createfile_get_set_wfc_cont ; no
	inc	eax ; eax = 1

	; 27/08/2024 (empty file)
	;;;
	; 23/03/2016
	;movzx	ebx, byte [createfile_SecPerClust]
	;;movzx	ecx, word [esi+LD_BPB+BytesPerSec] ; 512
        ;movzx   ecx, word [createfile_BytesPerSec] ; 512
	;;;
	jmp	loc_createfile_set_cluster_count

loc_createfile_set_ff_dir_entry_retn:
	retn

loc_createfile_write_fcluster_to_disk:
	add	eax, [esi+LD_DATABegin] ; convert to physical address
	mov	ebx, Cluster_Buffer
	; ESI = Logical DOS Drv. Desc. Tbl. address
	; EAX = Disk address
	; EBX = Sector Buffer
	; ECX = sectors per cluster
	call	disk_write
	jc	short loc_createfile_dsk_wr_err

loc_createfile_update_fat_cluster:
	; 21/03/2016
	cmp	byte [createfile_wfc], 0
	ja	short loc_createfile_update_fat_cluster_n1

	inc	byte [createfile_wfc] ; 1
	jmp	short loc_createfile_update_fat_cluster_n2

loc_createfile_dsk_wr_err:
	; 16/10/2016 (1Dh -> 18)
	; 23/03/2016
	;mov	eax, 18 ; Drive not ready or write error !
	; 29/07/2022
	sub	eax, eax
	mov	al, 18
loc_cf_stc_retn:
	jmp	loc_createfile_stc_retn

loc_createfile_update_fat_cluster_n1:
	mov	eax, [createfile_PCluster]
	mov	ecx, [createfile_Cluster]
	call	update_cluster
	jnc	short loc_createfile_update_fat_cluster_n2
	or	eax, eax ; EAX = 0 -> cluster value is 0 or eocc
	;jnz	loc_createfile_stc_retn
	; 29/07/2022
	jnz	short loc_cf_stc_retn

loc_createfile_update_fat_cluster_n2:
        mov	eax, [createfile_Cluster]
	;mov	ecx, 0FFFFFFFh
	; 29/08/2024
	; (update_cluster will save 28 bit cluster value)
	;mov	ecx, 0FFFFFFFFh
	sub	ecx, ecx
	dec	ecx
	call	update_cluster
	jnc	short loc_createfile_save_fat_buffer_3
	or	eax, eax ; EAX = 0 -> cluster value is 0 or eocc
	jz	short loc_createfile_save_fat_buffer_3

loc_cf_upd_fat_fcluster_stc_retn:
	jmp	loc_createfile_stc_retn

loc_createfile_get_set_wfc_cont:
	;movzx	ecx, word [esi+LD_BPB+BytesPerSec] ; 512
	movzx	ecx, word [createfile_BytesPerSec] ; 512
	add	eax, ecx
	dec	eax  ; add eax, 511
	sub	edx, edx
	div	ecx
	movzx	ebx, byte [createfile_SecPerClust]
	add	eax, ebx
	dec	eax  ; add eax, SecPerClust - 1
	;xor	dx, dx
	; 27/08/2024
	xor	edx, edx
	div	ebx

loc_createfile_set_cluster_count:
	mov 	[createfile_CCount], eax

	; 31/08/2024
	;;;
	cmp	dword [createfile_size], 0 ; empty file ?
	jz	short loc_createfile_save_fat_buffer_3
	;;;

	mov	edi, Cluster_Buffer
	mov	eax, ecx ; Bytes per Sector
	mul	ebx ; Sectors per Cluster
	; EAX = Bytes per Cluster
	mov	ecx, eax
	shr	ecx, 2 ; dword count
	xor	eax, eax
	rep	stosd ; clear cluster buffer

	mov	eax, [createfile_Cluster] ; 24/03/2016

	mov	ecx, ebx

loc_createfile_get_set_wf_fclust_cont:
	;sub	eax, 2
	; 30/07/2022
	dec	eax
	dec	eax
	mul	ecx
	; EAX = Logical DOS disk address (offset)
        jmp     loc_createfile_write_fcluster_to_disk

loc_createfile_save_fat_buffer_3:
	; byte [FAT_BuffValidData] = 2
	call	save_fat_buffer
	;jc	loc_createfile_stc_retn
	; 29/07/2022
	jc	short loc_cf_upd_fat_fcluster_stc_retn

	; 21/03/2016
	;cmp	byte [FAT_ClusterCounter], 1
	;jb	short loc_createfile_save_fat_buffer_4
	; 31/08/2024
	cmp	byte [FAT_ClusterCounter], 0
	jna	short loc_createfile_save_fat_buffer_4 ; cf = 0

	; ESI = Logical DOS Drive Description Table address
	mov	eax, [FAT_ClusterCounter]
	mov	bx, 0FF01h ; add free clusters
	call	calculate_fat_freespace

	;inc	eax ; 0FFFFFFFFh -> 0 ; recalculation is needed!
	;jnz	short loc_createfile_save_fat_buffer_4

	; ecx > 0 -> Recalculation is needed
	or	ecx, ecx 
	jz	short loc_createfile_save_fat_buffer_4

	mov	bx, 0FF00h ; ; recalculate free space
	call	calculate_fat_freespace

loc_createfile_save_fat_buffer_4:
	dec	dword [createfile_CCount]
	;jz	short loc_createfile_upd_dir_modif_date_time
	jz	short loc_createfile_stc_retn_cc ; 31/03/2016

loc_createfile_get_set_write_next_cluster:
	call	get_first_free_cluster
	jc	short loc_createfile_stc_retn

loc_createfile_get_set_write_next_cluster_1:
	cmp	eax, 0FFFFFFFFh
	jb	short loc_createfile_get_set_write_next_cluster_2

;loc_createfile_wnc_insufficient_disk_space:
	;mov	eax, 27h ; Insufficient disk space
	; 29/07/2022
	;xor	eax, eax
	; 29/08/2024
	; eax = FFFFFFFFh
	inc	eax ; eax = 0
	mov	al, 27h

loc_createfile_stc_retn:
	cmp	byte [createfile_wfc], 1
	jnb	short loc_createfile_err_retn
	retn

loc_createfile_wnc_inv_format_retn:
	;mov	eax, 28
	mov	al, 28 ; Invalid format
	jmp	short loc_createfile_stc_retn

loc_createfile_get_set_write_next_cluster_2:
	cmp	eax, 2
	jb	short loc_createfile_wnc_inv_format_retn

loc_createfile_get_set_write_next_cluster_3:
	mov	ecx, [createfile_Cluster]
	mov	[createfile_Cluster], eax
	mov	[createfile_PCluster], ecx
	movzx	ecx, byte [createfile_SecPerClust]
	jmp	short loc_createfile_get_set_wf_fclust_cont

loc_createfile_err_retn:
	stc

;loc_createfile_upd_dir_modif_date_time:
loc_createfile_stc_retn_cc: ; 31/03/2016
	pushf	; cpu is here for an error return or completion
	push	eax ; error code if cf = 1

	;call	update_parent_dir_lmdt

;loc_createfile_stc_retn_cc:
	mov	eax, [FAT_ClusterCounter]
	or	eax, eax
	jz	short loc_createfile_stc_retn_pop_eax
	mov	bh, [Current_Drv]
	mov	bl, 01h ; BL = 1 -> add clusters
	; NOTE: EAX value will be added to Free Cluster Count
	; (If EAX value is negative, Free Cluster Count will be decreased)
  	call	calculate_fat_freespace
        ; ESI = Logical DOS Drive Description Table Address
        ;jc	short loc_createfile_stc_retn_pop_eax_cf
	and	ecx, ecx ; cx = 0 -> valid free sector count
	jz	short loc_createfile_stc_retn_pop_eax

loc_createfile_stc_retn_recalc_FAT_freespace:
	mov	bx, 0FF00h ; bh = 0FFh ->
	; ESI = Logical DOS Drv DT Addr
	; BL = 0 -> Recalculate 
	call	calculate_fat_freespace

loc_createfile_stc_retn_pop_eax:
	pop	eax
	popf
	jc	short loc_createfile_retn

loc_createfile_retn_fcluster:
	mov	eax, [createfile_FFCluster]
	;mov	ebx, createfile_size
	;;movzx	ecx, byte [esi+LD_BPB+SecPerClust]
	;movzx	ecx, byte [createfile_SecPerClust] ; 23/03/2016
	;movzx	edx, word [createfile_DirIndex]
	; 29/08/2024 - TRDOS 386 v2.0.9
	mov	ebx, [createfile_size]
	movzx	ecx, word [createfile_DirIndex] ; = directory entry index
	mov	edx, [createfile_LastDirCluster] ; = [DirBuff_Cluster]
	; esi = Logical DOS Drive Description Table address 
	; byte [esi] = drive name ; 'A','B','C','D'

loc_createfile_retn:
	retn

; 28/05/2025 - TRDOS 386 v2.0.10
;----------------------------------------------------------
; (Windows) FAT long file name (LFN) to DOS short file name
; conversion

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
	; 28/05/2025
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

	; 28/05/2025 - Erdogan Tan
	; if the long name contains space(s) between words
	;    it must be accepted as 'lossy conversion'.
	; for example:
	;  'de ne me.txt' is not same with 'deneme.txt';
	;  so, short name of 'de ne me.txt' must
	;  be 'DENEME~1.TXT', not 'DENEME.TXT'.
	
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
conv_f_lfn_1:	; remove spaces & dots
	lodsb
	cmp	al, 20h
	jb	short conv_f_lfn_6
	ja	short conv_f_lfn_2
	or	byte [lossy_conversion], 1
	jmp	short conv_f_lfn_5
conv_f_lfn_2:
	cmp	al, '.'
	jne	short conv_f_lfn_4
	mov	al, bl
	mov	ebx, edi ; the last dot
	sub	ebx, edx
	jz	short conv_f_lfn_3 ; .fname
	and	al, al
	jz	short conv_f_lfn_5 ; the 1st dot
conv_f_lfn_3:
	or	byte [lossy_conversion], 1
	jmp	short conv_f_lfn_5

	; 28/05/2025
nul_lfn:
	pop	esi ; **
	pop	edi ; *
	mov	byte [edi], 0
	; Invalid file name error !
	mov	eax, -1
	stc	; cf = 1
	retn

conv_f_lfn_4:	; not_dot
	stosb
conv_f_lfn_5:
	loop	conv_f_lfn_1
conv_f_lfn_6:
	mov	esi, edx ; *
	mov	ecx, edi

	; zero tail
	;xor	eax, eax ; 0
	;stosb

	; ebx = the last dot position (index)

	sub	ecx, esi ; number of chars
	 	 ; (without spaces)
	jz	short nul_lfn  ; invalid file name !

	mov	[f_base_count], ecx

	mov	edi, [f_target] ; (*)

	;; eax = 0

	and	ebx, ebx
	jz	short check_base ; ecx > 0 ; not dot

	cmp	ecx, ebx
	;je	short skip_extension
		; the last char of the LFN is dot
	ja	short conv_f_lfn_7

	; the last char of the LFN is dot
	or	byte [lossy_conversion], 1
	jmp	short skip_extension

conv_f_lfn_7:
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
	;mov	[f_ext_start], edi ; 28/05/2025
	rep	movsb
skip_extension:
	xor	al, al	; put zero/NUL at the end
	stosb

	; Translate all illegal 8.3 characters into "_".

	mov	edi, [f_target] ; (*)
	mov	esi, edi
conv_f_lfn_8:
	lodsb
	and	al, al
	jz	short conv_f_lfn_10
	cmp	al, '.'
	je	short conv_f_lfn_9
	dec	byte [conv_ucase] 
	call	convert_invalid_chars
	stosb
	cmp	ah, al
	je	short conv_f_lfn_8
	inc	byte [lossy_conversion]
	jmp	short conv_f_lfn_8

conv_f_lfn_9:
	inc	edi
	mov	[f_ext_start], edi
	jmp	short conv_f_lfn_8

conv_f_lfn_10:
	;stosb 	; NUL

	cmp	byte [lossy_conversion], 0
	jna	short conv_f_lfn_14 ; exact 8.3 name

	; mark for it is not a lower case 8.3 name 
	mov	byte [conv_ucase], -1

	; 27/05/2025
	; put '~1' at the end of the base name
	mov	ebx, [f_ext_start] ; '.ext' possible
	mov	edx, [ebx]
	mov	eax, 6 ; char 7 ('~') and 8 ('1')
	cmp	eax, [f_base_count]
	jna	short conv_f_lfn_11
	mov	eax, [f_base_count]
conv_f_lfn_11:
	mov	edi, [f_target] ; [f_base_start]
	add	edi, eax
	; 28/05/2025
	; default !
	mov	[lossy_conversion], al ; tilde pos
	mov	ax, '~1'
		; base name (<=6 bytes) + '~1'
	stosw
	cmp	byte [f_ext_count], 0
	jna	short conv_f_lfn_13
	mov	al, '.'
	stosb
	mov	cl, [f_ext_count]
conv_f_lfn_12:
	mov	al, dl
	stosb
	shr	edx, 8
	loop	conv_f_lfn_12
conv_f_lfn_13:
	sub	al, al ; 0
		; '.ext+'0 or '.ex'+ or '.e'+0
	stosb

conv_f_lfn_14:
	; 28/05/2025
	xor 	eax, eax ; clc
	mov	al, [lossy_conversion]
	; eax = tilde position (0 to 6)
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
	mov	cl, 8
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
	; 28/05/2025
	mov	eax, [esi]
	cmp	al, '.'
	je	short ctn_dotpos
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

	; 25/05/2025 - TRDOS 386 v2.0.10
unicode_to_ascii:
	; Convert UNICODE long name
	;	  to ASCIIZ long name
	; Input:
	;  esi = UNICODE file name buffer
	;	(max. 260 bytes)
	;  ecx = ASCIIZ buffer size (64 or 128)
	;  edi = ASCIIZ file name buffer
	;	(max. ECX+1 bytes)
	;
	; Output:
	;  ecx = remain bytes in buffer
	;	  (after the last zero)
	;   al = the last char converted
	;  edi = next byte position
	;	  in ASCIIZ string/name buffer
	;
	; Modified registers: eax, ecx, esi, edi
	;

	; Note: if the last char is not NUL
	;	ASCIIZ string will be done
	;	by puting a NUL/ZERO at
	;	65th or 129th char position.
	;	(by the caller procedure)
	;    EDI points to that.
	;     AL contains the last character

	; 25/05/2025
	; (temporary code before version 2.1)
u_to_a_@:
	lodsw
	or	ah, ah
	jz	short u_to_a_next ; ascii char

	; here.. unicode to ascii table
	; conversion will be used
	;	in TRDOS 386 version 2.1
	; the unicode char will be searched in a table
	; ((which will be prepared by using indexes
	;	0 to 127 for ascii chars 128 to 255))
	; (((the table will contain 128 words)))

	; put default character instead of
	; non-ascii unicode character

	mov	al, '_'	; (temporary!)
u_to_a_next:
	stosb
	and	al, al
	jz	short u_to_a_ok ; ecx > 0
	loop	u_to_a_@
	; the last char is not NUL/zero
	retn
u_to_a_ok:
	dec	ecx
	retn

	; 01/06/2025 - TRDOS 386 v2.0.10
ascii_from_unicode:
	; Convert UNICODE character
	;	  to ASCII character
	; 
	; INPUT:
	;	ax = unicode character/letter
	; OUTPUT:
	;	al = ascii character
	;
	; Modified registers: eax

	or	ah, ah
	jz	short a_from_u_ok ; ascii char

	; here.. unicode to ascii table
	; conversion will be used
	;	in TRDOS 386 version 2.1
	; the unicode char will be searched in a table
	; ((which will be prepared by using indexes
	;	0 to 127 for ascii chars 128 to 255))
	; (((the table will contain 128 words)))

	; put default character instead of
	; non-ascii unicode character

	mov	al, '_'	; (temporary!)
a_from_u_ok:
	retn

%if 0
	; 01/06/2025 - TRDOS 386 v2.0.10
unicode_from_ascii:
	; Convert ASCII character
	;	  to UNICODE character
	; INPUT:
	;	al = ascii character
	; OUTPUT:
	;	ax = unicode character/letter
	;
	; Modified registers: eax

	; here.. unicode to ascii table
	; conversion will be used
	;	in TRDOS 386 version 2.1
	; the unicode char will be searched in a table
	; ((which will be prepared by using indexes
	;	0 to 127 for ascii chars 128 to 255))
	; (((the table will contain 128 words)))

	; put default character instead of
	; non-ascii unicode character

a_from_u_ok:
	retn
%endif	
	
	; 01/06/2025 - - TRDOS 386 v2.0.10
check_invalid_lfn_chars:
	; INPUT:
	;  esi = ASCIIZ long name (max. 128 bytes, + NUL)
	;
	; OUTPUT:
	;  If cf = 1 -> string/text contains an invalid
	;	        long name character (in AL)
	;  If cf = 0 -> proper
	;		
	; Modified registers: EAX, ECX

	push	esi
	mov	ecx, 128
cilfnc_1:
	lodsb
	
	cmp	al, 128
	jnb	short cilfnc_5

	cmp	al, 20h
	jnb	short cilfnc_2

	or	al, al
	jz	short cilfnc_6

cilfnc_4:
	; invalid character
	pop	esi
	stc
	retn

cilfnc_2:
	cmp	al, '@'
	jb	short cilfnc_3
	cmp	al, 'Z'
	jna	short cilfnc_5
	cmp	al, 'a'
	jb	short cilfnc_3
	cmp	al, 'z'
	jna	short cilfnc_5
cilfnc_3:
	push	edi
	push	ecx
	mov	edi, invalid_lfname_chars
 	mov	cl, sizeInvLfnChars
	repne	scasb
	pop	ecx
	pop	edi
	jz	short cilfnc_4 ; invalid
cilfnc_5:
	loop	cilfnc_1
	clc
cilfnc_6:
	pop	esi
	retn

	; 01/06/2025 - TRDOS 386 v2.0.10
ascii_to_unicode:
	; Convert ASCIIZ long name
	;	  to UNICODE long name
	; Input:
	;  esi = ASCIIZ file name buffer
	;	(max. 128 + NUL bytes)
	;  edi = UNICODE file name buffer
	;	(max. 260 bytes)
	;
	; Output:
	;  byte	[LFN_level] = Required LFN Entry Count
	;  eax = long name length (except NUL tail)
	;	 -asciiz string length, except zero-
	;
	; Modified registers: eax, ebx, ecx, esi, edi
	;

	; 01/06/2025
	; (temporary code before version 2.1)

	; here.. unicode to ascii table
	; conversion will be used
	;	in TRDOS 386 version 2.1
	; the unicode char will be searched in a table
	; ((which will be prepared by using indexes
	;	0 to 127 for ascii chars 128 to 255))
	; (((the table will contain 128 words)))

	xor	eax, eax
	mov	[LFN_level], al ; 0

	mov	ecx, 128
a_to_u_sub:
	mov	ebx, 13
	inc	byte [LFN_level]
a_to_u_sub_next:
	lodsb
	stosw
	or	al, al
	jz	short a_to_u_exit_2
	dec	ecx
	jz	short a_to_u_exit_1
	dec	ebx
	jnz	short a_to_u_sub_next
	jmp	short a_to_u_sub

a_to_u_exit_1:
	sub	al, al
a_to_u_exit_2:
	; eax = 0
	stosd
	mov	al, 128
	sub	eax, ecx
	retn

	; 04/06/2025 - TRDOS 386 v2.0.10
calc_LFN_level:
	; Calculate LFN level for a long name
	; (before/without converting it to UNICODE)
	;
	; Input:
	;  esi = ASCIIZ file name buffer
	;	(max. 128 + NUL bytes)
	;
	; Output:
	;  byte	[LFN_level] = Required LFN Entry Count
	;  eax = long name length (except NUL tail)
	;	 -asciiz string length, except zero-
	;
	; Modified registers: eax, ebx, ecx, esi
	;

	xor	eax, eax
	mov	[LFN_level], al ; 0

	mov	ecx, 128
clfnl_1:
	mov	ebx, 13
	inc	byte [LFN_level]
clfnl_2:
	lodsb
	or	al, al
	jz	short clfnl_3
	dec	ecx
	jz	short clfnl_3
	dec	ebx
	jnz	short clfnl_2
	jmp	short clfnl_1

clfnl_3:
	mov	al, 128
	sub	eax, ecx
	retn

	; 04/06/2025
	; 03/06/2025
	; 01/06/2025 - TRDOS 386 v2.0.10
search_longname:
	; Search DOS/Windows LFN in current directory
	;
	; Input:
	;   esi = ASCIIZ long name (not UNICODE!)
	;   	  (max. 128 bytes, + NUL)
	;
	;   [Current_Drv] = Logical Dos Drive Number
	;   [Current_Dir_FCluster] = 1st clust of the dir
	;		which the LFN will be searched
	;	          = 0 -> root directory	
	; Output:
	;   eax = LFN position (found)
	;   ebx = the 1st cluster of the directory
	;   ecx = number of LFN sub components
	;   edx = Logical Dos Drive parameters Table
	;   esi = ASCIIZ long name (same with input)
	;   edi = short dir entry (in the dir buff)
	;   ebp = Long File Name (UNICODE) address
	;
	;   If CF = 1 -> error code in EAX
	;
	; Modified registers:
	;	eax, ebx, ecx, edx, edi, ebp
	;

	mov	[f_target], esi ; save string address

	call	check_invalid_lfn_chars
	;jc	short slfn_invalid
	jnc	short sln_1

slfn_invalid:
	; invalid file name
	mov	eax, ERR_INV_FILE_NAME ;  26
	stc
	retn

sln_1:
	;mov	edi, LongFileName

	; edi = Unicode long name buffer
	;	(max. 260 bytes)

	; convert to unicode file name
	;call	ascii_to_unicode

	; calculate count of long dir entries
	call	calc_LFN_level

	; asciiz long name length except zero tail
	;mov	ecx, eax

	; [LFN_level] = long name sub components

	mov	al, [LFN_level]

	or	al, 40h
	mov	[LDIR_Ord], al

	xor	edx, edx
	mov	dh, [Current_Drv]

	mov	ebx, [Current_Dir_FCluster]

	xor	eax, eax ; 0

	or	ebx, ebx ; root ?
	jz	short sln_3

	cmp	byte [edx+LD_FATType], 3
	jb	short sln_2

	; Check if it is FAT32 root dir cluster
	cmp	ebx, [edx+LD_BPB+BPB_RootClus]
	je	short sln_3
sln_2:
	mov	al, 2	; skip '.' and '..'
sln_3:
	mov	[DirEntry_Counter], eax

	call	get_direntry_@
	jc	short sln_fail

sln_4:
	; edi = directory entry
	; esi = directory buffer header
	; eax = directory entry offset (0-480)

	lea	ebx, [esi+BUFINSIZ+512]
sln_5:
	mov	al, [edi]
	or	al, al
	jz	short sln_not_found ; end of directory

	;cmp	byte [edi+0Bh], 0Fh
	cmp	byte [edi+dir_entry.dir_attr], ATTR_LONGNAME
	jne	short sln_9

	cmp	al, [LDIR_Ord]
	jne	short sln_10

	mov	esi, [f_target]

	call	check_lfn_sub_component
	jnz	short sln_9

	;mov	cl, [edi+0Dh] ; long name checksum
	mov	cl, [edi+LDIR.Chksum]
	mov	[LFN_CheckSum], cl

	test	byte [LDIR_Ord], 40h
	jz	short sln_6

	and	byte [LDIR_Ord], 0Fh ; ~40h
	mov	eax, [DirEntry_Counter]
	mov	[LFN_Pos], eax

sln_6:
	dec	byte [LDIR_Ord]
	jz	short sln_11

sln_7:
	inc	dword [DirEntry_Counter]
	add	edi, 32
sln_8:
	cmp	edi, ebx ; buffer end
	jb	short sln_5

	mov	eax, [DirEntry_Counter]
	mov	ebx, [Current_Dir_FCluster]
	call	get_direntry
	jc	short sln_fail
	jmp	short sln_4

sln_not_found:
	; Long File Name not found
	mov	eax, ERR_FILE_NOT_FOUND ; 12
sln_fail:
	; error code in EAX
	stc
	retn

sln_9:
	test	byte [LDIR_Ord], 40h
	jnz	short sln_7
	mov	al, [LFN_level]
	or	al, 40h
	mov	[LDIR_Ord], al
	jmp	short sln_7

sln_10:
	and	eax, 0Fh
	add	[DirEntry_Counter], eax
	shl	eax, 5 ; * 32
	add	edi, eax
	jmp	short sln_8

sln_11:
	inc	dword [DirEntry_Counter]
	add	edi, 32
	cmp	edi, ebx ; buffer end
	jnb	short sln_13

sln_12:
	;test	byte [edi+0Bh], 0C8h
	test	byte [edi+dir_entry.dir_attr], 0C8h
		; not volume, also bit 6&7 must be 0
	jnz	short sln_fail_flags
	mov	esi, edi
	call	calculate_checksum
	; ecx = 0
	cmp	al, [LFN_CheckSum]
	jne	short sln_fail_chksum

sln_found:
	;clc
	mov	eax, [LFN_Pos]
	mov	ebx, [Current_Dir_FCluster]
	mov	esi, [f_target] ; asciiz long name
	mov	ebp, LongFileName ; UNICODE name
	mov	cl, [LFN_level]
	; ecx = number of LFN sub components
	; edi = short directory entry
	; edx = LDRVT
	retn

sln_13:
	mov	eax, [DirEntry_Counter]
	mov	ebx, [Current_Dir_FCluster]
	call	get_direntry
	jc	short sln_fail
	jmp	short sln_12

sln_fail_chksum:
	; checksum error
	mov	eax, ERR_CHECKSUM ; 31
	stc
	retn

sln_fail_flags:
	; invalid (short dir entry) flags
	mov	eax, ERR_INV_FLAGS
	stc
	retn

	; 03/06/2025 - TRDOS 386 v2.0.10
check_lfn_sub_component:
	; 04/06/2025
	; Compare asciiz string (long name) with
	; Unicode Long File Name sub-component
	; (letter by letter after a conversion to ascii)
	;
	; Input:
	;   esi = asciiz long name address
	;	(max. 128 bytes + NUL)
	;
	;   edi = long directory entry address
	;    al = long directory entry order/ordinal
	;
	;   LongFileName = Unicode long file name
	;	buffer (260 bytes)
	;
	; Output:
	;   If zf = 1 -> match
	;   	LongFileName = unicode long name
	;	ecx = 0
	;	al = matching (lowercase) character
	;   If zf = 0 -> not match
	;	al = mismatched lowercase character
	;
	;   If cf = 1 -> condition mismatch
	;	al = LDIR order (0-15)
	;
	; Note: If the characters do not match,
	;     they are compared again in lower case.
	;  (dir entry char is converted to lower case)
	;
	; Modified registers:
	;	eax, ecx, esi, ebp
	;

	xor	ecx, ecx
	and	eax, 0Fh
	cmp	al, 0Ah	; limit for TRDOS 386 v2.0.10
	ja	short clfnsc_fail

	cmp	al, 1
	jb	short clfnsc_fail

	cmp	al, 10 ; 0Ah
	jb	short clfnsc_0

	; check 128 bytes limit (117+11)
	; the 12th char must be ZERO or 0FFFFh
	mov	cx, [edi+LDIR.Name3]
	jecxz	clfnsc_0 ; 0
	inc	cx ; -1 -> 0
	;jnz	short clfnsc_fail
	jz	short clfnsc_0

clfnsc_fail:
	; cf = 1
	stc
	retn 

clfnsc_0:
	dec	al	; 1 -> 0 or 41h -> 0

	mov	cl, 13	; Order-1 * 13
	mul	cl
	; eax = offset
	;	(0-13-26-39-52-65-78-91-104-117)

	push	edi	; *

	mov	ebp, esi ; asciiz file name
	add	ebp, eax

	shl	eax, 1	; Order-1 * 26 
	mov	edi, eax 

	mov	esi, edi ; unicode LFN sub component

	add	edi, LongFileName

	inc	esi	; LDIR.Name1

	mov	cl, 5	; Characters 1-5
clfnsc_1:
	lodsw
	stosw
	call	ascii_from_unicode
	cmp	al, [ebp]
	je	short clfnsc_1_next
	call	check_lcase
	jne	short clfnsc_fail_@
clfnsc_1_next:
	inc	ebp
	loop	clfnsc_1

	add	esi, 3
	mov	cl, 6	; Characters 6-11
clfnsc_2:
	lodsw
	stosw
	call	ascii_from_unicode
	cmp	al, [ebp]
	je	short clfnsc_2_next
	call	check_lcase
	jne	short clfnsc_fail_@
clfnsc_2_next:
	inc	ebp
	loop	clfnsc_2
	lodsw

	mov	cl, 2	; Characters 12-13
clfnsc_3:
	lodsw
	stosw
	call	ascii_from_unicode
	cmp	al, [ebp]
	je	short clfnsc_3_next
	call	check_lcase
	jne	short clfnsc_fail_@
clfnsc_3_next:
	inc	ebp
	loop	clfnsc_3

clfnsc_ok:
	; 04/06/2025
	; ecx = 0
clfnsc_fail_@:	; ecx > 0
	or	ecx, ecx ; zf = 1

	; zf = 1 -> match
	; zf = 0 -> not match

	pop	edi	; *

	; zf = 0
	; ecx = 0

	retn

; -----------------------------------------------

	; 02/06/2025
get_direntry_@:
	; initialization
	mov	dword [GDE_CCLUST], -1

	; 03/06/2025
	; 02/06/2025
	; 01/06/2025 - TRDOS 386 v2.0.10
get_direntry:
	; Get Directory Entry
	;  (with Index/Sequence Number)
	;
	; Input:
	;  eax = Index number
	;  ebx = first cluster
	;  edx = Logical DOS Drive Desc. Table
	;
	; Output:
	;  edi = directory entry address
	;  esi = directory buffer header
	;  (eax = directory entry offset)
	;  edx = LDRVT
	;
	;  if cf = 1 -> error code in eax
	;
	; Modified registers:
	;	 eax, ebx, ecx, esi, edi, ebp
	;

	xor	ecx, ecx

	mov	cl, al
	and	cl, 15
	mov	[GDE_BINDEX], ecx

	mov	[GDE_INDEX], eax

	or	ebx, ebx
	jnz	short gde_2

	; root directory

	cmp	byte [edx+LD_FATType], 3
	jb	short gde_0

	mov	ebx, [edx+LD_BPB+FAT32_RootFClust]
	jmp	short gde_2

gde_0:
	mov	cx, [edx+LD_BPB+RootDirEnts]
	cmp	eax, ecx
	jb	short gde_1

	; out of root directory !
	mov	eax, -1
	stc
	retn

gde_1: 
	;shr	ecx, 4 ; 512/32 = 16 entries/sector
	; ecx = root dir sectors
	;mov	[GDE_SPC], cl
	shr	eax, 4
	mov	[GDE_SINDEX], eax
	sub	eax, eax ; 0
	jmp	short gde_3

	; sub directory
gde_2:
	mov	cl, [edx+LD_BPB+SecPerClust]
	;mov	[GDE_SPC], cl
	push	edx
	shr	eax, 4 ; 512/32
	xor	edx, edx
	div	ecx
	; eax = cluster index
	mov	[GDE_SINDEX], dl ; sector index
	pop	edx
gde_3:
	mov	ecx, [GDE_CINDEX]
	mov	[GDE_CINDEX], eax

	; ecx = old index
	; eax = new index

	mov	[GDE_SKIP], eax

	cmp	edx, [GDE_DRVT]
	jne	short gde_5
	cmp	ebx, [GDE_FCLUST]
	jne	short gde_6

	;cmp	ecx, [GDE_CINDEX]
	;je	short gde_4

	sub	eax, ecx ; current index
	; eax = skip count if [GDE_CCLUST] is valid
	jz	short gde_4 ; same
	jb	short gde_7 ; less (start from fclust)

	cmp	dword [GDE_CCLUST], -1 ; invalid ?
	jnb	short gde_8  ; yes, do chain from fclust

	mov	[GDE_SKIP], eax ; > 0
	mov	ebx, [GDE_CCLUST]

	jmp	short gde_8  ; chain from current clust
gde_4:
	mov	eax, [GDE_CCLUST]
	cmp	eax, -1 ; invalid ?
	jb	short gde_14 ; no, valid

	jecxz	gde_13 ; First Cluster

gde_5:
	mov	[GDE_DRVT], edx
gde_6:
	mov	[GDE_FCLUST], ebx
gde_7:
	xor	ecx, ecx
	cmp	[GDE_CINDEX], ecx ; 0
	je	short gde_13

	; ebx = [GDE_FCLUST] >= 2

	dec	ecx
	mov	[GDE_CCLUST], ecx ; -1
gde_8:
	mov	esi, edx ; LDRVT
	mov	eax, ebx ; current cluster
gde_9:
	call	get_next_cluster
	jnc	short gde_12 ; edx = esi

	or	eax, eax
	jnz	short gde_10

	; (end of cluster chain)
	; Directory entry not found
	mov	al, ERR_NOT_FOUND ; 2
	stc
	retn

gde_10:
	; Drive not ready or read error
	mov	eax, ERR_DRV_READ ; 17
	stc
gde_11:
	retn

gde_12:
	; eax = next cluster
	; ecx = current cluster
	dec	dword [GDE_SKIP]
	jnz	short gde_9
	mov	[GDE_CCLUST], eax
	jmp	short gde_15

gde_13:
	mov	[GDE_CCLUST], ebx
	;mov	eax, [GDE_CCLUST]
	mov	eax, ebx
gde_14:
	or	eax, eax
	jnz	short gde_15
	mov	eax, [edx+LD_ROOTBegin]
	jmp	short gde_16
gde_15:
	; edx = LDRVT
	; eax = cluster number
	call	FIGREC
gde_16:
	add	eax, [GDE_SINDEX]

	; eax = physical sector number
	;  cl = physical drive/disk number
	;       (needed for GETBUFFER procedure)

	call	GETBUFFER
	jc	short gde_11

	;mov	esi, [CurrentBuffer]
	or	byte [esi+BUFFINFO.buf_flags], buf_isDIR

	lea	edi, [esi+BUFINSIZ]

	mov	eax, [GDE_BINDEX]
	shl	eax, 5 ; * 32
	add	edi, eax

	retn

; -----------------------------------------------

check_lcase:
	; 03/06/2025
	; 01/06/2025 - Erdogan Tan
	; if lower case character matches,
	; it will be accepted.
	; (The user may write a directory/file name
	;  with lowercase letters.
	;  Uppercase to lowercase conversion
	;  is for directory entry only,
	;  not for the user.)

	; al = character 
	;      (in directory entry)
	;  -already converted to ascii from unicode-
	;
	; [ebp] = user's character (asciiz string)
 
	call	simple_lcase
	; zf = 1 -> match
	; zf = 0 -> not match
	cmp	al, [ebp]
	retn

; -----------------------------------------------

	; 01/06/2025 - TRDOS 386 v2.0.10
simple_lcase:
	cmp	al, 41h ; 'A'
	jb	short simple_lcase_skip
	cmp	al, 5Ah ; 'Z'
	ja	short simple_lcase_skip
	or	al, 20h
simple_lcase_skip:
	retn


; 20/05/2025 - TRDOS 386 v2.0.10
;------------------------------------------------------
; Singlix FS1 (TRFS1) to TRDOS 386 file name conversion

convert_name_from_trfs:
	; 27/05/2025
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
	;	EAX = FDT/DDT number
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
	;call	convert_invalid_chars
	; 27/05/2025
	call	convert_invalid_chars_@
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
	;call	convert_invalid_chars
	; 27/05/2025
	call	convert_invalid_chars_@
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

find_last_dot:
	; 25/05/2025
	; 20/05/2025 - TRDOS 386 v2.0.10
	;
	; INPUT:
	;  ESI = file name
	;  ECX = search limit (64 bytes or 128 bytes)
	; OUTPUT:
	;  EBX = position (-1 = not found)
	;  ECX = asciiz (file name) string length,
	;	 except zero/NUL tail (or CR, <20h)
	;
	; Modified registers: EAX, EBX, ECX
	;

	push	esi

	xor	ebx, ebx

	;cmp	ecx, 128
	;jnb	short f_l_dot_ok
	;or	ecx, ecx
	;jz	short f_l_dot_ok

f_l_dot_1:
	lodsb
	cmp	al, 20h
	jb	short f_l_dot_ok
	cmp	al, '.'
	jne	short f_l_dot_2
	mov	ebx, esi
f_l_dot_2:
	loop	f_l_dot_1
	inc	esi
f_l_dot_ok:
	mov	ecx, esi
	dec	ecx
	dec	ebx
	pop	esi
	sub	ecx, esi
	retn

convert_invalid_chars_@:
	; 27/05/2025 - TRDOS 386 v2.0.10

	cmp	al, '.'
	jne	short convert_invalid_chars
	mov	ah, al
	jmp	short cic_2

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
	and	al, 0DFh
	;and	al, 5Fh
	mov	ah, al
	retn

	; 24/05/2025 - TRDOS 386 v2.0.10
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
	push esi
	mov esi, eax
mfn_@:
	lodsb
	cmp al, 20h
	jb  short mfn_skip
	stosb
	loop mfn_@
	; ecx = 0
	;xor eax, eax
	;stosb
mfn_skip:
	pop esi
	retn

	; 31/05/2025 - TRDOS 386 v2.0.10
compare_fs_short_name:
	;
	; INPUT:
	;	esi = short name to be searched...
	;	      d[...].ext
	;	      d*.*
	;	      d????.???
	;	edi = short name of the name in the FDT/DDT
	;
	; OUTPUT:
	;	if zf = 0 -> not same/match
	;	if zf = 1 -> same/match
	;
	; Modified registers: eax, ecx, esi, edi

	mov	ecx, 13 ; max 12 chars + NUL
cfs_sn_1:
	lodsb
	scasb
	je	short cfs_sn_2

	cmp	al, '*'
	je	short cfs_sn_4

	cmp	al, '?'
	je	short cfs_sn_3

	; zf = 0
	retn

cfs_sn_2:
	or	al, al
	jz	short cfs_sn_5
cfs_sn_3:
	loop	cfs_sn_1

	; if AL = '?' zf = 1
	; if AL <> '?' zf = 0
	retn

cfs_sn_4:
	cmp	byte [esi], '.'
	je	short cfs_sn_1 ; *.* or *.ext
	xor	eax, eax
cfs_sn_5:
	; zf = 1
	retn

	; 31/05/2025 - TRDOS 386 v2.0.10
compare_fs_long_name:
	; 
	; INPUT:
	;	esi = long name to be searched...
	;	      d*.*
	;	      d????.???
	;	edi = the name in the FDT/DDT
	;
	; OUTPUT:
	;	if zf = 0 -> not same/match
	;	if zf = 1 -> same/match
	;
	; Modified registers: eax, ebx, ecx, esi, edi

	mov	ecx, 64 ; max. 64 chars
cfs_ln_1:
	lodsb
	scasb
	jz	short cfs_ln_2

	cmp	al, '*'
	je	short cfs_ln_4

	cmp	al, '?'
	je	short cfs_ln_3

	; zf = 0
	retn

cfs_ln_2:
	or	al, al
	jz	short cfs_ln_5 ; same (with NUL)
cfs_ln_3:
	dec	ecx
	jnz	short cfs_ln_1

	; zf = 1 ; (same 64 bytes)
	retn

cfs_ln_4:
	cmp	byte [esi], '.'
	jne	short cfs_ln_5
	; *.* or *.ext
	push	esi
	push	ecx
	mov	esi, edi
	call	find_last_dot
	pop	ecx
	pop	esi
	inc	ebx ; -1 -> 0
	jz	short cfs_ln_6 ; dot not found
	dec	ebx
	; ebx = the last dot position
	mov	edi, ebx
	jmp	short cfs_ln_3
	
cfs_ln_5:
	xor	eax, eax
	; zf = 1
	retn

cfs_ln_6:
	inc	ebx ; 0 -> 1
	; zf = 0
	retn

	; 31/05/2025 - TRDOS 386 v2.0.10
get_fdt_number:
	; Singlix FS (TRFS) function/subroutine
	;
	; INPUT:
	;	eax = FDT (or DDT) number
	;	  0 = any
	;	 -1 = deleted
	;	ebx = dir entry index number (< 65536)
	;	esi = directory buffer address
	;
	; OUTPUT:
	;	eax = 0 -> not found or
	;		   end of directory
	;	eax > 0 -> found
	;
	;	ecx = eax input
	;	ebx = directory entry index number
	;	esi = directory entry address
	;
	; Modified registers: eax, ebx, ecx, esi

	mov	ecx, eax
g_fdt_num_1:
	inc	ebx

	mov	eax, [esi]
	cmp	eax, ecx
	je	short g_fdt_num_2

	or	eax, eax
	jz	short g_fdt_num_2

	test	bl, 127 ; the last directory entry
			; in the fs directory buffer
	jz	short g_fdt_num_2

	add	esi, 4
	jmp	short g_fdt_num_1

g_fdt_num_2:
	dec	ebx
	retn

	; 31/05/2025 - TRDOS 386 v2.0.10
search_fdt_number:
	; Singlix FS (TRFS) function/subroutine
	;
	; INPUT:
	;	eax = FDT (or DDT) number
	;	  0 = any
	;	 -1 = deleted
	;	ebx = dir entry index number (< 66536)
	;	      (for all of the directory)
	;	edx = Logical DOS Desc. Table Addr
	;
	; OUTPUT:
	;	eax = 0 -> not found or
	;		   end of directory
	;	eax > 0 -> found
	;
	;	ecx = eax input
	;	ebx = directory entry index number
	;	esi = directory entry address
	;
	;    If cf = 1 -> Error code in EAX
	;
	; Modified registers:
	;	eax, ebx, ecx, esi, edi, ebp
	;

	mov	[FDT_Number], eax
	;mov	[FS_Dir_Index], ebx
	mov	dword [FS_DDT_Buffer], 0

search_fdt_number_next:
	mov	[FS_Dir_Index], ebx
	mov	eax, [FS_CurrenDirectory]
		; DDT number of the current dir
	add	eax, [edx+LD_FS_BeginSector]
		; start LBA of TRFS partition
	mov	cl, [edx+LD_FS_PhyDrvNo]
		; physical (rombios) drive number
	
	call	GETBUFFER
	jc	short s_fdt_num_err

	; esi = FS DDT buffer header address
	;  cl = physical disk number
	; edx = logical DOS drive table address

	add	esi, BUFINSIZ ; + 24 bytes
	; esi = FS directory buffer addres

	cmp	esi, [FS_DDT_Buffer]
	je	short search_fdt_number_next_@

	mov	eax, [esi] ; [esi+DDT.Signature]

	;and	eax, 0FFFFFFh
	cmp	eax ,'DDT'
	jne	short s_fdt_num_dnf
	;mov	eax, [esi+8]
	mov	eax, [esi+DDT.DirectoryNumber]
	cmp	eax, [FS_CurrenDirectory]
	jne	short s_fdt_num_dnf

	mov	[FS_DDT_Buffer], esi

search_fdt_number_next_@:
	; esi = DDT buffer address
	; ebx = directory entry index number
	; edx = LDRVT address

	call 	get_fs_sector

	; eax = sector address

	add	eax, [edx+LD_FS_BeginSector]
		; start LBA of TRFS partition
	mov	cl, [edx+LD_FS_PhyDrvNo]
		; physical (rombios) drive number
	call	GETBUFFER
	jc	short s_fdt_num_err

	add	esi, BUFINSIZ ; + 24 bytes
	; esi = FS directory buffer (data start)

	;mov	ebx, [FS_Dir_Index]
	; ebx = directory entry index number
	; (ebx < 65536) 

	mov	eax, [FDT_Number]

	call	get_fdt_number

	; eax = 0 -> not found or end of directory
	; eax > 0 -> found
	; ecx = eax input
	; ebx = directory entry index number
	; esi = directory entry address

	mov	[FS_Dir_Index], ebx

	cmp	ecx, eax
	je	short s_fdt_num_found

	and	eax, eax
	jz	short s_fdt_num_not_found

	inc	ebx ; next entry number
	;mov	[FS_Dir_Index], ebx

	;cmp	ebx, 65535
	;ja	short s_fdt_num_not_found

	jmp	short search_fdt_number_next

s_fdt_num_not_found_@:
	pop	ebx ; *
s_fdt_num_not_found:
	mov	eax, ERR_NOT_FOUND ; 'file not found !'
s_fdt_num_err:
	stc
s_fdt_num_found:
	retn

s_fdt_num_dnf_@:
	pop	ebx ; *
s_fdt_num_dnf:
	mov	eax, ERR_PATH_NOT_FOUND
			; 'path not found !' error
			; 'dir not found !'
	stc
	retn

	; 31/05/2025 - TRDOS 386 v2.0.10
get_fs_sector:
	; INPUT:
	;   esi = DDT buffer address
	;   ebx = directory entry index number
	;   edx = LDRVT address
	;
	; OUTPUT:
	;   esi = DDT buffer address
	;   ebx = directory entry index number
	;   edx = LDRVT address
	;   eax = sector address
	;
	; Modified registers: eax, ecx

	push	ebx ; *
	shr	ebx, 7 ; / 128
	; ebx = sector sequence (index) number
	cmp	ebx, [esi+DDT.SectorCount]
	jnb	short s_fdt_num_not_found_@

	mov	al, [esi+DDT.ExtentAllocType]
	cmp	al, 3
		; TRIPLE indirect tables are not usable
		; for current TRDOS 386 version. (*)
	jnb	short s_fdt_num_dnf_@ ; (*)
	
	push	esi ; **	

	cmp	al, 1
	ja	short get_fs_sector_dblindirect
			; double indirect extent tables
	jb	short get_fs_sector_direct
			; direct extents table in the DDT
	; indirect extent tables
get_fs_sector_indirect:
	;push	esi ; **
	add	esi, DDT.ExtentsTable ; +128
	mov	ecx, 16 ; 16 indirect tables
get_fs_sector_indirect_@:
get_fs_sector_indirect_next:
	lodsd
	; eax = sector index/sequence number (base)
	lodsd
	and	eax, eax ; is this table address valid ?
	jz	short gfssid_out ; no, end of file

	dec	ecx
	jz	short gfssid_ok ; the last table

	; eax = table address

	cmp	ebx, [esi]
		; sector index num of the next table
	jnb	short get_fs_sector_indirect_next

gfssid_ok:
	; eax = indirect extent table address

	add	eax, [edx+LD_FS_BeginSector]
		; start LBA of TRFS partition
	mov	cl, [edx+LD_FS_PhyDrvNo]
		; physical (rombios) drive number
	call	GETBUFFER
	jc	short gfssid_err

	add	esi, BUFINSIZ ; + 24 bytes
	; esi = FS directory buffer (data start)
	mov	ecx, 64 ; 512/8
	jmp	short get_fs_sector_direct_@

gfssdid_err:
gfssid_err:
	pop	esi ; **
	pop	ebx ; *
	retn

gfssd_out:
	pop	edx ; ***
gfssid_out:
gfssdid_out:
	pop	esi ; **
	jmp	short s_fdt_num_dnf_@
		
get_fs_sector_direct:
	;push	esi ; **
	add	esi, DDT.ExtentsTable ; +128
	mov	ecx, 16 ; 16 direct extents
get_fs_sector_direct_@:
	push	edx ; ***
get_fs_sector_direct_next:
	lodsd
	; eax = sector index/sequence number (base)
	mov	edx, eax
	lodsd
	and	eax, eax ; is this extent address valid ?
	jz	short gfssd_out ; no, end of file

	dec	ecx
	jz	short gfssd_ok ; the last extent

	; eax = extent address
	;cmp	ebx, edx
	;jna	short gfssd_ok
	cmp	ebx, [esi]
		; sector index of the next extent
	jnb	short get_fs_sector_direct_next

gfssd_ok:
	; edx = beginning sector of the extent
	; eax = extent address
	; ebx = sector sequence (index) number
	sub	ebx, edx 
	; ebx = sector offset in the extent
	add	eax, ebx
	; eax = sector address
	pop	edx ; ***
	pop	esi ; **
	pop	ebx ; *
	retn

get_fs_sector_dblindirect:
	;push	esi ; **
	add	esi, DDT.ExtentsTable ; +128
	mov	ecx, 16 ; 16 double indirect tables
get_fs_sector_dblindir_next:
	lodsd
	; eax = sector index/sequence number (base)
	lodsd
	and	eax, eax ; is this table address valid ?
	jz	short gfssdid_out ; no, end of file

	dec	ecx
	jz	short gfssdid_ok ; the last table

	; eax = table address

	cmp	ebx, [esi]
		; sector index num of the next table
	jnb	short get_fs_sector_dblindir_next

gfssdid_ok:
	; eax = double indirect table address
	; ebx = sector index/sequence number

	add	eax, [edx+LD_FS_BeginSector]
		; start LBA of TRFS partition
	mov	cl, [edx+LD_FS_PhyDrvNo]
		; physical (rombios) drive number
	call	GETBUFFER
	jc	short gfssdid_err

	add	esi, BUFINSIZ ; + 24 bytes
	mov	ecx, 64 ; 64 indirect tables
	jmp	short get_fs_sector_indirect_@


; 28/07/2022 (TRDOS 386 Kernel v2.0.5)

create_fs_file:
	; temporary (21/03/2016)
	;retn

delete_fs_file:
	; temporary (28/02/2016)
	;retn

rename_fs_file_or_directory:
	;retn

make_fs_directory:
	; temporary (21/02/2016)
	;retn

add_new_fs_section:
	; temporary (11/03/2016)
	;retn

delete_fs_directory_entry:
	; temporary (11/03/2016)
	;retn

csftdf2_read_fs_file_sectors:
	; temporary (19/03/2016)
	;retn

csftdf2_write_fs_file_sectors:
	; temporary (19/03/2016)
	retn
