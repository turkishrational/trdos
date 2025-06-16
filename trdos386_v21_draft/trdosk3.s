; ****************************************************************************
; TRDOS386.ASM (TRDOS 386 Kernel - v2.0.10) - MAIN PROGRAM : trdosk3.s
; ----------------------------------------------------------------------------
; Last Update: 16/06/2025  (Previous: 26/09/2024, v2.0.9)
; ----------------------------------------------------------------------------
; Beginning: 06/01/2016
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.15 (trdos386.s)
; ----------------------------------------------------------------------------
; Derived from TRDOS Operating System v1.0 (8086) source code by Erdogan Tan
; MAINPROG.ASM (09/11/2011)
; ****************************************************************************
; MAINPROG.ASM [ TRDOS KERNEL - COMMAND EXECUTER SECTION - MAIN PROGRAM ]
; (c) 2004-2011  Erdogan TAN  [ 17/01/2004 ]  Last Update: 09/11/2011
; CMD_INTR.ASM [ TRDOS Command Interpreter Procedure ] Last Update: 09/11/2011
; DIR.ASM [ DIRECTORY FUNCTIONS ] Last Update: 09/10/2011
; FILE.ASM [ FILE FUNCTIONS ] Last Update: 09/10/2011

change_current_drive:
	; 09/05/2025
	; 08/05/2025 (TRDOS 386 Kernel v2.0.10)
	; 26/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 16/10/2016
	; 02/02/2016
	; 15/01/2016 (TRDOS 386 = TRDOS v2.0)
	; 18/08/2011
	; 09/09/2009
	; INPUT:
	;   DL = Logical DOS Drive Number
	; OUTPUT:
	;  cf=1 -> Not successful
	;   EAX = Error code
	;  cf=0 ->
	;   EAX = 0 (successful)

	xor	ebx, ebx
	mov	bh, dl

	;cmp	dl, 1
	;jna	short loc_ccdrv_initial_media_change_check
	;cmp	bh, [Last_Dos_DiskNo]
	;ja	short loc_ccdrv_drive_not_ready_err

loc_ccdrv_initial_media_change_check:
	mov	esi, Logical_DOSDisks
	add	esi, ebx
loc_ccdrv_dos_drive_name_check:
	cmp	dl, 2
	jb	short loc_ccdrv_dos_drive_name_check_ok

	mov	al, [esi+LD_Name]
	sub	al, 'A'
	cmp	al, dl
	je	short loc_ccdrv_dos_drive_name_check_ok

loc_ccdrv_drive_not_ready_err:
	; 16/10/2016 (15h -> 15)
	;mov	eax, 15 ; Drive not ready
	; 26/07/2022
	sub	eax, eax
	;mov	al, 15
	; 09/05/2025
	mov	al, ERR_DRV_NOT_RDY ; 15
loc_change_current_drive_stc_retn:
	stc
	retn

loc_ccdrv_dos_drive_name_check_ok:
	mov	ah, [esi+LD_MediaChanged]
	cmp	ah, 6  ; VOLUME NAME CHECK/MOVE SIGN
	je	short loc_ccdrv_get_FAT_volume_name_0

	cmp	dl, 1
	ja	short loc_gmcs_init_drv_hd

loc_gmcs_init_drv_fd:
	or	ah, ah 
	; AH = 1 is initialization sign (invalid_fd_parameter)
	jnz	short loc_ccdrv_call_fd_init

	call	get_media_change_status
	jc	short loc_ccdrv_drive_not_ready_err

	and	ah, ah
	jz	short loc_change_current_drv3

	xor	ah, 6
	jnz	short loc_ccdrv_drive_not_ready_err

loc_ccdrv_call_fd_init_check_vol_id:
	call	get_volume_serial_number
	jnc	short loc_ccdrv_check_vol_serial

loc_ccdrv_call_fd_init:
	call	floppy_drv_init
	jnc	short loc_reset_drv_fd_current_dir

loc_ccdrv_fdinit_fail_retn:
	; 16/10/2016
	;mov	eax, 15 ; Drive not ready
	; 09/05/2025
	mov	eax, ERR_DRV_NOT_RDY ; 15
	retn

loc_ccdrv_check_vol_serial:
	mov	[Current_VolSerial], eax
	;mov	dl, bh
	call	floppy_drv_init
	jc	short loc_ccdrv_fdinit_fail_retn

	cmp	eax, [Current_VolSerial]
	je	short loc_change_current_drv2

loc_reset_drv_fd_current_dir:
	xor	eax, eax
        mov	[esi+LD_CDirLevel], al ; 0
	mov	edi, esi
	add	edi, LD_CurrentDirectory
	mov	ecx, 32
	rep	stosd

loc_ccdrv_get_FAT_volume_name_0:
	mov	al, [esi+LD_FATType]
	or	al, al
	jz	short loc_change_current_drv2

	push	esi 
	cmp	al, 2
	ja	short loc_ccdrv_get_FAT32_vol_name

loc_ccdrv_get_FAT2_16_vol_name:
	add	esi, LD_BPB + VolumeLabel
	jmp	short loc_ccdrv_get_FAT_volume_name_1

loc_ccdrv_get_FAT32_vol_name:
	add	esi, LD_BPB + FAT32_VolLab
loc_ccdrv_get_FAT_volume_name_1:
	push	ebx
	push	esi
	call	get_FAT_volume_name
	pop	edi
	pop	ebx
	; BL = 0
	jc	short loc_change_current_drv1

	; 08/05/2025 - TRDOS 386 v2.0.10
	;and	al, al
	;jz	short loc_change_current_drv1

loc_ccdrv_move_FAT_volume_name:
	mov	ecx, 11
	rep	movsb

loc_change_current_drv1:
	pop	esi
	jmp	short loc_change_current_drv2

loc_gmcs_init_drv_hd:
	or	ah, ah
	jz	short loc_change_current_drv3
	; BL = 0, BH = Logical DOS drive number
loc_change_current_drv2:
	mov	byte [esi+LD_MediaChanged], 0
loc_change_current_drv3:
	mov	[Current_Drv], bh

	;call	restore_current_directory
	;retn

restore_current_directory:
	; 15/06/2025
	; 02/06/2025
	; 16/05/2025
	; 14/05/2025
	; 09/05/2025 (TRDOS 386 Kernel v2.0.10)
	; 26/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 11/02/2016
	; 15/01/2016 (TRDOS 386 = TRDOS v2.0)
	; 25/01/2010
	; 12/10/2009
	;
	; INPUT:
	;   ESI = Logical DOS Drive Description Table
	;
	; OUTPUT:
	;   ESI = Logical DOS Drive Description Table
	;   EDI = offset Current_Dir_Drv

	mov	al, [esi+LD_FATType]
	mov	[Current_FATType], al

	mov	ah, [esi+LD_Name]
	mov	[Current_Dir_Drv], ah

	and	al, al
	jz	short loc_restore_FS_current_directory

loc_restore_FAT_current_directory:
	; 14/05/2025
	;mov	ah, [esi+LD_CDirLevel]
	;mov	[Current_Dir_Level], ah
	;or	ah, ah
	movzx	edx, byte [esi+LD_CDirLevel]
	mov	[Current_Dir_Level], dl
	or	dl, dl        
	jz	short loc_ccdrv_reset_cdir_FAT_12_16_32_fcluster

	; 14/05/2025 (8 sub dir levels)
	;movzx	edx, ah
	dec	edx ; level 1 -> the first field/entry 

	shl	dl, 4 ; * 16
        add	edx, esi
	mov	eax, [edx+LD_CurrentDirectory+12]
	jmp	short loc_ccdrv_reset_cdir_FAT_fcluster

loc_restore_FS_current_directory:
	;call	load_current_FS_directory
	;retn
	; 26/07/2022
	;jmp	load_current_FS_directory
	; 09/05/2025
	mov	eax, [esi+LD_FS_RootDirD] ; root directory DDT
	;jmp	short loc_ccdrv_check_rootdir_sign
	; 02/06/2025
	jmp	short loc_ccdrv_reset_cdir_FAT_fcluster

loc_ccdrv_reset_cdir_FAT_12_16_32_fcluster:
	; 09/06/2025
	cmp	al, 3
	jb	short loc_ccdrv_reset_cdir_FAT_12_16_fcluster
loc_ccdrv_reset_cdir_FAT32_fcluster:
	mov	eax, [esi+LD_BPB+FAT32_RootFClust]
	;jmp	short loc_ccdrv_check_rootdir_sign
	; 02/06/2025
	jmp	short loc_ccdrv_reset_cdir_FAT_fcluster

loc_ccdrv_reset_cdir_FAT_12_16_fcluster:
	;xor	al, al  ; xor eax, eax
	; 26/07/2022
	xor	eax, eax

; 14/05/2025	
;	;xor	edx, edx
;loc_ccdrv_check_rootdir_sign:
;	cmp	byte [esi+LD_CurrentDirectory], 0
;	jne	short loc_ccdrv_reset_cdir_FAT_fcluster
;loc_ccdrv_set_rootdir_FAT_fcluster:
;	mov	[esi+LD_CurrentDirectory+12], eax
;	mov	dword [esi+LD_CurrentDirectory], 'ROOT'

loc_ccdrv_reset_cdir_FAT_fcluster:
	mov	[Current_Dir_FCluster], eax

	; 15/06/2025
	; 16/05/2025 - TRDOS 386 v2.0.10
	mov	edi, PATH_Array
	mov	edx, esi
	add	esi, LD_CurrentDirectory
	mov	ecx, 32
	rep	movsd

	; 15/06/2025
	call	change_prompt_dir_string
	; 16/05/2025
	;call	change_prompt_dir_str

	mov	esi, edx

        sub	eax, eax
       ;sub	edx, edx
	mov	edi, Current_Dir_Drv

	mov	[Restore_CDIR], al ; 0
	retn

dos_prompt:
	; 16/06/2025
	; 14/05/2025 (TRDOS 386 Kernel v2.0.5)
	; (8 sub dir levels, max. 103 chars curdir string, +zero )
	; ((max. 66 bytes curdir ASCIIZ string will be displayed))
	;
	; 26/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 06/05/2016
	; 30/01/2016
	; 29/01/2016
	; 16/01/2016 (TRDOS 386 = TRDOS v2.0)
	; 15/09/2011
	; 13/09/2009
	; 2004-2005

	; 06/05/2016
	mov	dword [mainprog_return_addr], return_from_cmd_interpreter

loc_TRDOS_prompt:
	mov	edi, TextBuffer
	mov	byte [edi], "["
	inc	edi
	mov	esi, TRDOSPromptLabel
get_next_prompt_label_char:
	cmp	byte [esi], 20h
	jb	short pass_prompt_label
	movsb
	jmp	short get_next_prompt_label_char
pass_prompt_label:
	mov	byte [edi], "]"
	inc	edi
	mov	byte [edi], 20h
	inc	edi
	mov	esi, Current_Dir_Drv
	movsw	; '?:'
	movsb	; '/'
loc_prompt_current_directory:
	; esi = offset Current_Directory
	cmp	byte [esi], 20h
	jb	short pass_prompt_current_directory

	;;;
	; 14/05/2025
	; decrease string length depending on prompt label
	mov	eax, TextBuffer+11+62 ; '[!] C:/'
	sub	eax, edi
	; if the prompt is '[TRDOS] C:/', AL = 62
	; if the prompt is '[!] C:/', AL = 66
	; if the prompt is '[11BYTES-MAX] C:/', AL = 56

trim_cdir_str_0:
	;cmp	byte [Current_Dir_StrLen], 65
	cmp	byte [Current_Dir_StrLen], al ; path limit for prompt
	jna	short skip_trim_cdir_str

	; trim path string
	mov	dword [edi], '.../'
	; 16/06/2025
	add	edi, 4
	movzx	ecx, byte [Current_Dir_StrLen]
	;sub	cl, 65-4
	sub	al, 4 ; '.../'
	sub	cl, al
	lodsd	; add esi, 4
	; skip excessive chars
	add	esi, ecx
	
trim_cdir_str_1:
	lodsb
	cmp	al, '/'
	jne	short trim_cdir_str_1

	; esi points to the 1st non-path ('/') char

skip_trim_cdir_str:
	;;;

	movsb

	;;;
	;jmp	short loc_prompt_current_directory
	; 14/05/2025
	cmp	byte [esi], 20h
	jnb	short skip_trim_cdir_str

	; here, zero byte found at the end of string
	;;;

pass_prompt_current_directory:
	mov	byte [edi], '>'
	inc	edi
	mov	byte [edi], 0
	mov	esi, TextBuffer
	call	print_msg

	;sub	bh, bh ; video page = 0
	;call	get_cpos ; get cursor position
	mov	dx, [CURSOR_POSN] ; video page 0
	mov	[CursorColumn], dl

	; 30/01/2016 (to show cursor on the row, again)
	; (Initial color attributes of video page 0 is 0)
	; (see: 'StartPMP' in trdos386.s)
	;
	;mov	edi, 0B8000h ; start of video page 0
	;movzx	ecx, dl ; column
	;mov	al, 80
	;mul	dh
	;add	ax, cx
	;shl	ax, 1 ; character + attribute
	;add	di, ax ; (2*80*row) + (2*column)
	;neg	cl
	;add	cl, 80
	;mov	ax, 700h ;  ah = 7 (color attribute)
	;rep	stosw

loc_rw_char:
	call	rw_char
loc_move_command:
	mov	esi, CommandBuffer
	mov	edi, esi
	xor	ecx, ecx
first_command_char:
	lodsb
	cmp	al, 20h
	ja	short pass_space_control
	jb	short loc_move_cmd_arguments_ok
	cmp	esi, CommandBuffer + 79
	jb	short first_command_char
	jmp	short loc_move_cmd_arguments_ok

	; 26/07/2022
pass_space_control:
	cmp	al, 61h
	jb	short pass_capitalize
	cmp	al, 7Ah
	ja	short pass_capitalize
	and	al, 0DFh
pass_capitalize:
	stosb   
	inc     cl
	cmp     esi, CommandBuffer + 79
	;jb	short next_command_char
	; 26/07/2022
	jnb	short loc_move_cmd_arguments_ok

next_command_char:
	lodsb
	cmp	al, 20h
	ja	short pass_space_control
	jb	short loc_move_cmd_arguments_ok

loc_1st_cmd_arg: ; 30/01/2016
	lodsb
	cmp	al, 20h
	je	short loc_1st_cmd_arg
	jb	short loc_move_cmd_arguments_ok

        mov     byte [edi], 0
	inc	edi

loc_move_cmd_arguments:
	stosb
	cmp	esi, CommandBuffer + 79
	jnb	short loc_move_cmd_arguments_ok
        lodsb
	cmp	al, 20h
	jnb	short loc_move_cmd_arguments
	; 26/07/2022
	;jmp	short loc_move_cmd_arguments_ok

; 26/07/2022
;pass_space_control:
;	cmp	al, 61h
;	jb	short pass_capitalize
;	cmp	al, 7Ah
;	ja	short pass_capitalize
;	and	al, 0DFh
;pass_capitalize:
;	stosb
;	inc     cl
;	cmp     esi, CommandBuffer + 79
;	jb      short next_command_char

loc_move_cmd_arguments_ok:
        mov     byte [edi], 0

call_command_interpreter:
	call    command_interpreter

return_from_cmd_interpreter:
        mov	ecx, 80
	;mov	cx, 80
	mov	edi, CommandBuffer
	xor	al, al
	rep	stosb
	;cmp	byte [Program_Exit], 0
	;ja	short loc_terminate_trdos

	; 16/01/2016
	cmp	byte [CRT_MODE], 3 ; 80*25 color
	je	short pass_set_txt_mode

	call	set_txt_mode ; set vide mode to 03h
	; 07/01/2017
	xor	al, al

loc_check_active_page:
	;xor	al, al
	cmp	[ACTIVE_PAGE], al ; 0
        ;je	loc_TRDOS_prompt
	; 26/07/2022
	je	short loc_prompt_again

	; AL = 0 = video page 0
	call	set_active_page
loc_prompt_again: ; 26/07/2022
        jmp     loc_TRDOS_prompt ; infinitive loop

pass_set_txt_mode:
	mov	esi, nextline
	call	print_msg
	; al = 0
	jmp     short loc_check_active_page

;rw_char:
	; 26/07/2022 (TRDOS 386 Kernel v2.0.5)
loc_arrow:
	cmp	ah, 4Bh
	je	short loc_back
	cmp	ah, 53h
	je      short loc_back
	cmp	ah, 4Dh
	jne	short readnextchar
	cmp	dl, 79
	jnb	short readnextchar
	inc	dl
	jmp	short set_cursor_pos

rw_char: 
	; 26/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 13/05/2016
	; 30/01/2016
	; 29/01/2016
	; 17/01/2016 (TRDOS 386 = TRDOS v2.0)
	; 2004-2005

	; DH = cursor row, DL = cursor column
	; BH = 0 = video page number (active page)

	;xor	bh, bh ; 0 = video page 0

readnextchar:
	xor     ah, ah
	call	int16h
	and	al, al
	jz	short loc_arrow
	cmp	al, 0E0h
	je	short loc_arrow
	cmp	al, 08h
	jne	short char_return
loc_back:
	cmp	dl, [CursorColumn]
	jna     short readnextchar
prev_column:
	dec	dl
set_cursor_pos:
	;push	dx
	push	edx ; 29/12/2017
	;xor	bh, bh ; 0 = video page 0
	; DH = Row, DL = Column
	call	_set_cpos ; 17/01/2016
        pop	edx ; 29/12/2017
	;pop	dx
	;movzx	ebx, dl
	mov	bl, dl
	sub	bl, [CursorColumn]
	mov	al, 20h
	mov	[CommandBuffer+ebx], al
	;sub	bh, bh ; video page 0
	;mov	cx, 1
	mov	bl, 7 ; color attribute
	; bh = 0 ; 26/07/2022
	call	_write_c_current ; 17/01/2016
	;mov	dx, [CURSOR_POSN]
	jmp	short readnextchar

	; 26/07/2022
;loc_arrow:    
;	cmp	ah, 4Bh
;	je	short loc_back
;	cmp	ah, 53h
;	je      short loc_back
;	cmp	ah, 4Dh
;	jne	short readnextchar
;	cmp	dl, 79
;	jnb	short readnextchar
;	inc	dl
;	jmp	short set_cursor_pos

char_return:
	movzx	ebx, dl
	sub	bl, [CursorColumn]
	cmp	al, 20h
	jb	short loc_escape
	mov	[CommandBuffer+ebx], al
	cmp	dl, 79
	jnb	short readnextchar
	mov	bx, 7 ; color attribute
	call	_write_tty
	mov	dx, [CURSOR_POSN] ; video page 0
        jmp	short readnextchar ; 26/07/2022

loc_escape:
	cmp	al, 1Bh
	je	short rw_char_retn
	;
	cmp	al, 0Dh ; CR
        jne	short readnextchar ; 26/07/2022

	; 13/05/2016
	mov	bx, 7 ; attribute/color (bl)
		      ; video page 0 (bh=0)
	call	_write_tty
	;mov	bx, 7 ; attribute/color
		      ; video page 0 (bh=0)
	mov	al, 0Ah ; LF
	call	_write_tty
rw_char_retn:
	retn

show_date:
	; 26/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 18/01/2016 (TRDOS 386 = TRDOS v2.0)
        ; 2004-2005

	;mov	ah, 04h
	;call	int1Ah
	call	RTC_40	; GET RTC DATE

	mov	al, dl
  	call	bcd_to_ascii
	mov	[Day], ax

	mov	al, dh
  	call	bcd_to_ascii
	mov	[Month], ax

	mov	al, ch
  	call	bcd_to_ascii
	mov	[Century], ax

	mov	al, cl
  	call	bcd_to_ascii
	mov	[Year], ax

	mov	esi, Msg_Show_Date
	;call	print_msg
	;retn
	; 26/07/2022
	jmp	print_msg

set_date:
	; 26/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 13/05/2016
	; 18/01/2016 (TRDOS 386 = TRDOS v2.0)
        ; 2004-2005

	mov	esi, Msg_Enter_Date
	call	print_msg

loc_enter_day_1:
	xor     ah, ah
	call	int16h
	; AL = ASCII Code of the Character
	cmp	al, 13
	;je	loc_set_date_retn
	; 26/07/2022
	je	short set_date_0
	cmp	al, 27
	;je	loc_set_date_retn
	; 26/07/2022
	jne	short set_date_1

set_date_0:
;loc_set_date_retn:
	mov	esi, nextline
	;call	print_msg
	;retn
	; 26/07/2022
	jmp	print_msg

	; 26/07/2022
loc_set_date_stc_0:
	;xor	bh, bh ; video page 0
	call	beeper ; BEEP !
	jmp	short loc_enter_day_1

	; 26/07/2022
set_date_1:
	mov	[Day], al
	cmp	al, '0'
	jb	short loc_set_date_stc_0
	cmp	al, '3'
	ja	short loc_set_date_stc_0
	; 13/05/2016
	;mov	bx, 7 ; attribute/color (bl)
		      ; video page 0 (bh)
	mov	bl, 7
	call	_write_tty
loc_enter_day_2:
	xor     ah, ah
	call	int16h
	; AL = ASCII Code of the Character
	cmp	al, 27
        ;je	loc_set_date_retn
	; 26/07/2022
	je	short set_date_0
	mov	[Day+1], al
	cmp	al, '0'
        jb      short loc_set_date_stc_1
	cmp	al, '9'
        ja      short loc_set_date_stc_1
	cmp	byte [Day], '3'
	jb	short pass_set_day_31
	cmp	al, '1'
        ;ja	loc_set_date_stc_1
	; 26/07/2022
	jna	short pass_set_day_31

	; 26/07/2022
loc_set_date_stc_1:
	call	check_for_backspace
	je	short loc_set_date_bs_1
	;xor	bh, bh ; video page 0
	call	beeper ; BEEP !
	jmp	short loc_enter_day_2
loc_set_date_bs_1:
	call	write_backspace
	jmp     short loc_enter_day_1

pass_set_day_31:
	; 13/05/2016
	;mov	bx, 7 ; attribute/color (bl)
		      ; video page 0 (bh)
	mov	bl, 7
	call	_write_tty
loc_enter_separator_1:
	sub     ah, ah ; 0
	call	int16h
	; AL = ASCII Code of the Character
	cmp	al, 27
        ;je	loc_set_date_retn
	; 26/07/2022
	je	short set_date_0
	cmp	al, '-'
	je	short pass_set_date_separator_1
	cmp	al, '/'
	;jne	loc_set_date_stc_2
	; 26/07/2022
	je	short pass_set_date_separator_1

	; 26/07/2022
loc_set_date_stc_2:
	call	check_for_backspace
	je	short loc_set_date_bs_2
	;xor	bh, bh ; video page 0
	call	beeper ; BEEP !
	jmp	short loc_enter_separator_1
loc_set_date_bs_2:
	call	write_backspace
	jmp	short loc_enter_day_2

	; 26/07/2022
loc_set_date_stc_3:
	call	check_for_backspace
	je	short loc_set_date_bs_3
	;xor	bh, bh ; video page 0
	call	beeper ; BEEP !
	jmp	short loc_enter_month_1
loc_set_date_bs_3:
	call	write_backspace
	jmp	short loc_enter_separator_1

	; 26/07/2022
loc_set_date_stc_4:
	call	check_for_backspace
	je	short loc_set_date_bs_4
	;xor	bh, bh ; video page 0
	call	beeper ; BEEP !
	jmp	short loc_enter_month_2
loc_set_date_bs_4:
	call	write_backspace
	jmp	short loc_enter_month_1
	
pass_set_date_separator_1:
	; 13/05/2016
	;mov	bx, 7 ; attribute/color (bl)
		      ; video page 0 (bh)
	mov	bl, 7	
	call	_write_tty
loc_enter_month_1:
	xor     ah, ah ; 0
	call	int16h
	; AL = ASCII Code of the Character
	cmp	al, 27
        ;je	loc_set_date_retn
	; 26/07/2022
	;je	short loc_set_date_ok
	; 07/08/2022
	je	short jmp_loc_set_date_ok
	mov	[Month], al
	cmp	al, '0'
        jb      short loc_set_date_stc_3
	cmp	al, '1'
        ja	short loc_set_date_stc_3
	; 13/05/2016
	;mov	bx, 7 ; attribute/color (bl)
		      ; video page 0 (bh)
	mov	bl, 7
	call	_write_tty
loc_enter_month_2:
	xor     ah, ah
	call	int16h
	; AL = ASCII Code of the Character
	cmp	al, 27
        ;;je	loc_set_date_retn
	; 26/07/2022
	;je	short loc_set_date_ok
	; 07/08/2022
	jne	short loc_enter_month_3
jmp_loc_set_date_ok:
	jmp	loc_set_date_ok
loc_enter_month_3:
	mov	[Month+1], al
	cmp	al, '0'
        jb	short loc_set_date_stc_4
	cmp	al, '9'
        ja	short loc_set_date_stc_4
	cmp	byte [Month], '1'
	jb	short pass_set_month_12
	cmp	al, '2'
        ja	short loc_set_date_stc_4
pass_set_month_12:
	; 13/05/2016
	;mov	bx, 7 ; attribute/color (bl)
		      ; video page 0 (bh)
	mov	bl, 7	
	call	_write_tty
loc_enter_separator_2:
	sub     ah, ah
	call	int16h
	; AL = ASCII Code of the Character
	cmp	al, 27
        ;je	loc_set_date_retn
	; 26/07/2022
	je	short loc_set_date_ok
	cmp	al, '-'
	je	short pass_set_date_separator_2
	cmp	al, '/'
        jne	short loc_set_date_stc_5
pass_set_date_separator_2:
	; 13/05/2016
	;mov	bx, 7 ; attribute/color (bl)
		      ; video page 0 (bh)
	mov	bl, 7
	call	_write_tty
loc_enter_year_1:
	xor    ah, ah
	call	int16h
	; AL = ASCII Code of the Character
	cmp	al, 27
        ;je	loc_set_date_retn
	; 26/07/2022
	je	short loc_set_date_ok
	mov	[Year], al
	cmp	al, '0'
        jb	short loc_set_date_stc_6
	cmp	al, '9'
        ja	short loc_set_date_stc_6
	; 13/05/2016
	;mov	bx, 7 ; attribute/color (bl)
		      ; video page 0 (bh)
	mov	bl, 7
	call	_write_tty
loc_enter_year_2:
	xor	ah, ah
	call	int16h
	; AL = ASCII Code of the Character
	cmp	al, 27
	;je	short loc_set_date_retn
	; 26/07/2022
	je	short loc_set_date_ok
	mov	byte [Year+1], al
	cmp	al, '0'
        jb 	short loc_set_date_stc_7
	cmp	al, '9'
        ja	short loc_set_date_stc_7
	; 13/05/2016
	;mov	bx, 7 ; attribute/color (bl)
		      ; video page 0 (bh)
	mov	bl, 7
	call	_write_tty
loc_set_date_get_lchar_again:
	sub	ah, ah ; 0
	call	int16h
	; AL = ASCII Code of the Character
	cmp	al, 13 ; ENTER key
	je	short loc_set_date_progress
	cmp	al, 27 ; ESC key
	;je	short loc_set_date_retn
	; 26/07/2022
	je	short loc_set_date_ok
	;
	call	check_for_backspace
	jne	short loc_set_date_get_lchar_again

loc_set_date_bs_8:
	call	write_backspace
	jmp	short loc_enter_year_2

loc_set_date_ok:
;loc_set_date_retn:
	mov	esi, nextline
	;call	print_msg
	;retn
	; 26/07/2022
	jmp	print_msg

	; 26/07/2022
;loc_set_date_stc_0:
;	;xor	bh, bh ; video page 0
;	call	beeper ; BEEP !
;	jmp	loc_enter_day_1
;loc_set_date_stc_1:
;	call	check_for_backspace
;	je	short loc_set_date_bs_1
;	;xor	bh, bh ; video page 0
;	call	beeper ; BEEP !
;	jmp	loc_enter_day_2
;loc_set_date_bs_1:
;	call	write_backspace
;	jmp     loc_enter_day_1
;loc_set_date_stc_2:
;	call	check_for_backspace
;	je	short loc_set_date_bs_2
;	;xor	bh, bh ; video page 0
;	call	beeper ; BEEP !
;	jmp	loc_enter_separator_1
;loc_set_date_bs_2:
;	call	write_backspace
;	jmp	loc_enter_day_2
;loc_set_date_stc_3:
;	call	check_for_backspace
;	je	short loc_set_date_bs_3
;	;xor	bh, bh ; video page 0
;	call	beeper ; BEEP !
;	jmp	loc_enter_month_1
;loc_set_date_bs_3:
;	call	write_backspace
;	jmp	loc_enter_separator_1
;loc_set_date_stc_4:
;	call	check_for_backspace
;	je	short loc_set_date_bs_4
;	;xor	bh, bh ; video page 0
;	call	beeper ; BEEP !
;	jmp	loc_enter_month_2
;loc_set_date_bs_4:
;	call	write_backspace
;	jmp	loc_enter_month_1

	; 26/07/2022
loc_set_date_stc_5:
	call	check_for_backspace
	je	short loc_set_date_bs_5
	;xor	bh, bh ; video page 0
	call	beeper ; BEEP !
	jmp	loc_enter_separator_2
loc_set_date_bs_5:
	call	write_backspace
	jmp	loc_enter_month_2
loc_set_date_stc_6:
	call	check_for_backspace
        je      short loc_set_date_bs_6
	;xor	bh, bh ; video page 0
	call	beeper ; BEEP !
	jmp	loc_enter_year_1
loc_set_date_bs_6:
	call	write_backspace
	jmp	loc_enter_separator_2
loc_set_date_stc_7:
	call	check_for_backspace
	je	short loc_set_date_bs_7
	;xor	bh, bh ; video page 0
	call	beeper ; BEEP !
	jmp	loc_enter_year_2
loc_set_date_bs_7:
	call	write_backspace
	jmp	loc_enter_year_1

loc_set_date_progress:
	; Get Current Date
	;mov	ah, 04h
	;call	int1Ah
	call	RTC_40	; GET RTC DATE
	; CH = century (in BCD)

	mov	ax, [Year]
	sub	ax, '00'
	shl	al, 4 ; * 16
	mov	cl, al
	add	cl, ah
	mov	ax, [Month]
	sub	ax, '00'
	shl	al, 4 ; * 16
	mov	dh, al
	add	dh, ah
	mov	ax, [Day]
	sub	ax, '00'
	shl	al, 4 ; * 16
	mov	dl, al
	add	dl, ah

	;mov	ah, 05h
	;call	int1Ah
	call	RTC_50	; SET RTC DATE

;loc_set_date_retn:
;	mov	esi, nextline
;	;call	print_msg
;	;retn
;	; 26/07/2022
;	jmp	print_msg

	; 26/07/2022
	jmp	loc_set_date_ok 

write_backspace:
	; 18/01/2016 (TRDOS 386 = TRDOS v2.0)
	mov	al, 08h ; BACKSPACE
	; 13/05/2016
	mov	bx, 7 ; bl = attribute/color
		      ; bh = video page = 0
	call	_write_tty
	mov	al, 20h ; BLANK/SPACE char 
	;mov	bx, 7 ; attribute/color
	;call	_write_c_current
	;retn
	jmp	_write_c_current

check_for_backspace:
	; 18/01/2016 (TRDOS 386 = TRDOS v2.0)
	cmp	ax, 0E08h
	je	short cfbs_retn
	cmp	ax, 4BE0h
	je	short cfbs_retn
	cmp	ax, 4B00h
	je	short cfbs_retn
	cmp	ax, 53E0h
cfbs_retn:
	retn

show_time:
	; 26/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 18/01/2016 (TRDOS 386 = TRDOS v2.0)
        ; 2004-2005

	;mov	ah, 02h
	;call	int1Ah
	call	RTC_20	; GET RTC TIME
	
	mov	al, ch
	call	bcd_to_ascii
	mov	[Hour], ax

	mov	al, cl
	call	bcd_to_ascii
	mov	[Minute], ax

	mov	al, dh
	call	bcd_to_ascii
	mov	[Second], ax

	mov	esi, Msg_Show_Time
	;call	print_msg
	;retn
	; 26/07/2022
	jmp	print_msg

set_time:
	; 26/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 13/05/2016
	; 18/01/2016 (TRDOS 386 = TRDOS v2.0)
        ; 2004-2005

	mov 	esi, Msg_Enter_Time
	call	print_msg

loc_enter_hour_1:
	xor     ah, ah
	call	int16h
	; AL = ASCII Code of the Character
	cmp	al, 13 ; ENTER key
        je	short loc_set_time_retn
	cmp	al, 27 ; ESC key
        je	short loc_set_time_retn
set_time_0:
	mov	[Hour], al
	cmp	al, '0'
        jb	short loc_set_time_stc_0
	cmp	al, '2'
 	;ja	loc_set_time_stc_0
	; 26/07/2022
	jna	short set_time_1

	; 26/07/2022
loc_set_time_stc_0:
	;xor	bh, bh ; video page 0
	call	beeper ; BEEP !
	jmp	short loc_enter_hour_1

loc_set_time_stc_1:
	call	check_for_backspace
	je	short loc_set_time_bs_1
	;xor	bh, bh ; video page 0
	call	beeper ; BEEP !
	jmp	short loc_enter_hour_2
loc_set_time_bs_1:
	call	write_backspace
	jmp	short loc_enter_hour_1

	; 26/07/2022
loc_set_time_retn:
	mov 	esi, nextline
	;call	print_msg
	;retn
	jmp	print_msg

set_time_1:
	; 13/05/2016
	;mov	bx, 7 ; attribute/color (bl)
		      ; video page 0 (bh)
	mov	bl, 7
	call	_write_tty
loc_enter_hour_2:
	xor     ah, ah
	call	int16h
	; AL = ASCII Code of the Character
	cmp	al, 27
	je	short loc_set_time_retn
	mov	[Hour+1], al
	cmp	al, '0'
	jb	short loc_set_time_stc_1
	cmp	al, '9'
	ja	short loc_set_time_stc_1
        cmp     byte [Hour], '2'
	jb	short pass_set_time_24
	cmp	al, '4'
	ja	short loc_set_time_stc_1
pass_set_time_24:
	; 13/05/2016
	;mov	bx, 7 ; attribute/color (bl)
		      ; video page 0 (bh)
	mov	bl, 7
	call	_write_tty
loc_enter_time_separator_1:
	sub    ah, ah ; 0
	call	int16h
	; AL = ASCII Code of the Character
	cmp	al, 27
	je	short loc_set_time_retn
	cmp	al, ':'
	;jne	loc_set_time_stc_2
	; 26/07/2022
	je	short set_time_2

	; 26/07/2022
loc_set_time_stc_2:
	call	check_for_backspace
	je	short loc_set_time_bs_2
	;xor	bh, bh ; video page 0
	call	beeper ; BEEP !
	jmp	short loc_enter_time_separator_1
loc_set_time_bs_2:
	call	write_backspace
	jmp	short loc_enter_hour_2

set_time_2:
	; 13/05/2016
	;mov	bx, 7 ; attribute/color (bl)
		      ; video page 0 (bh)
	mov	bl, 7
	call	_write_tty
loc_enter_minute_1:
	xor     ah, ah
	call	int16h
	; AL = ASCII Code of the Character
	cmp	al, 27
	je	short loc_set_time_retn
	mov	[Minute], al
	cmp	al, '0'
	jb	short loc_set_time_stc_3
	cmp	al, '5'
	;ja	loc_set_time_stc_3
	; 26/07/2022
	jna	short set_time_3

	; 26/07/2022
loc_set_time_stc_3:
	call	check_for_backspace
	je	short loc_set_time_bs_3
	;xor	bh, bh ; video page 0
	call	beeper ; BEEP !6
	jmp	short loc_enter_minute_1
loc_set_time_bs_3:
	call	write_backspace
	jmp	short loc_enter_time_separator_1

set_time_3:
	; 13/05/2016
	;mov	bx, 7 ; attribute/color (bl)
		      ; video page 0 (bh)
	mov	bl, 7
	call	_write_tty
loc_enter_minute_2:
	xor     ah, ah
	call	int16h
	; AL = ASCII Code of the Character
	cmp	al, 27
	;je	short loc_set_time_retn
	; 07/08/2022
	jne	short loc_enter_minute_3
	jmp	loc_set_time_retn
loc_enter_minute_3:
	mov	[Minute+1], al
	cmp	al, '0'
	jb	short loc_set_time_stc_4
	cmp	al, '9'
	;ja	loc_set_time_stc_4
	; 26/07/2022
	jna	short set_time_4

	; 26/07/2022
loc_set_time_stc_4:
	call	check_for_backspace
	je	short loc_set_time_bs_4
	;xor	bh, bh ; video page 0
	call	beeper ; BEEP !
	jmp	short loc_enter_minute_2
loc_set_time_bs_4:
	call	write_backspace
	jmp	short loc_enter_minute_1

set_time_4:
	; 13/05/2016
	;mov	bx, 7 ; attribute/color (bl)
		      ; video page 0 (bh)
	mov	bl, 7
	call	_write_tty
loc_enter_time_separator_2:
	mov	word [Second], 3030h
	sub     ah, ah
	call	int16h
	; AL = ASCII Code of the Character
	cmp	al, 13
        ;je	short loc_set_time_progress
	; 07/08/2022
	jne	short loc_enter_time_separator_3
jmp_loc_set_time_progress:
	jmp	loc_set_time_progress
loc_enter_time_separator_3:
	cmp	al, 27
	;;je	loc_set_time_retn
	; 26/07/2022
	;je	short loc_set_time_ok
	; 07/08/2022
	jne	short loc_enter_time_separator_4
	jmp	loc_set_time_ok
loc_enter_time_separator_4:
	cmp	al, ':'
        jne	short loc_set_time_stc_5

	; 13/05/2016
	;mov	bx, 7 ; attribute/color (bl)
		      ; video page 0 (bh)
	mov	bl, 7
	call	_write_tty
loc_enter_second_1:
	xor     ah, ah
	call	int16h
	; AL = ASCII Code of the Character
	cmp	al, 13
	;je	short loc_set_time_progress
	; 07/08/2022
	je	short jmp_loc_set_time_progress
	cmp	al, 27
	;;je	loc_set_time_retn
	; 26/07/2022
	;je	short loc_set_time_ok
	; 07/08/2022
	jne	short loc_enter_second_2
	jmp	loc_set_time_ok
loc_enter_second_2:
	mov	[Second], al
	cmp	al, '0'
	jb	short loc_set_time_stc_6
	cmp	al, '5'
	ja	short loc_set_time_stc_6
	; 13/05/2016
	;mov	bx, 7 ; attribute/color (bl)
		      ; video page 0 (bh)
	mov	bl, 7
	call	_write_tty
loc_enter_second_3:
	xor     ah, ah
	call	int16h
	; AL = ASCII Code of the Character
	cmp	al, 27
	;je	short loc_set_time_retn
	; 26/07/2022
	je	short loc_set_time_ok
	cmp	al, '0'
        jb	short loc_set_time_stc_7
	cmp	al, '9'
        ja	short loc_set_time_stc_7
	; 13/05/2016
	;mov	bx, 7 ; attribute/color (bl)
		      ; video page 0 (bh)
	mov	bl, 7
	call	_write_tty
loc_set_time_get_lchar_again:
	sub	ah, ah ; 0
	call	int16h
	; AL = ASCII Code of the Character
	cmp	al, 13
	je	short loc_set_time_progress
	cmp	al, 27
	;je	short loc_set_time_retn
	; 07/08/2022
	je	short loc_set_time_ok
	;
	call	check_for_backspace
	jne	short loc_set_time_get_lchar_again

loc_set_time_bs_8:
	call	write_backspace
	jmp	short loc_enter_second_3

;	; 26/07/2022
;loc_set_time_retn:
;	mov 	esi, nextline
;	;call	print_msg
;	;retn
;	jmp	print_msg

	; 26/07/2022
;loc_set_time_stc_0:
;	;xor	bh, bh ; video page 0
;	call	beeper ; BEEP !
;	jmp	loc_enter_hour_1
;loc_set_time_stc_1:
;	call	check_for_backspace
;	je	short loc_set_time_bs_1
;	;xor	bh, bh ; video page 0
;	call	beeper ; BEEP !
;	jmp	loc_enter_hour_2
;loc_set_time_bs_1:
;	call	write_backspace
;	jmp	loc_enter_hour_1
;loc_set_time_stc_2:
;	call	check_for_backspace
;	je	short loc_set_time_bs_2
;	;xor	bh, bh ; video page 0
;	call	beeper ; BEEP !
;	jmp	loc_enter_time_separator_1
;loc_set_time_bs_2:
;	call	write_backspace
;	jmp	loc_enter_hour_2
;loc_set_time_stc_3:
;	call	check_for_backspace
;	je	short loc_set_time_bs_3
;	;xor	bh, bh ; video page 0
;	call	beeper ; BEEP !6
;	jmp	loc_enter_minute_1
;loc_set_time_bs_3:
;	call	write_backspace
;	jmp	loc_enter_time_separator_1
;loc_set_time_stc_4:
;	call	check_for_backspace
;	je	short loc_set_time_bs_4
;	;xor	bh, bh ; video page 0
;	call	beeper ; BEEP !
;	jmp	loc_enter_minute_2
;loc_set_time_bs_4:
;	call	write_backspace
;	jmp	loc_enter_minute_1

	; 26/07/2022
loc_set_time_stc_5:
	call	check_for_backspace
	je	short loc_set_time_bs_5
	;xor	bh, bh ; video page 0
	call	beeper ; BEEP !
	jmp	loc_enter_time_separator_2
loc_set_time_bs_5:
	call	write_backspace
	jmp	loc_enter_minute_2

	; 26/07/2022
loc_set_time_ok:
	mov 	esi, nextline
	;call	print_msg
	;retn
	jmp	print_msg

	; 07/08/2022
loc_set_time_stc_6:
	call	check_for_backspace
	je	short loc_set_time_bs_6
	;xor	bh, bh ; video page 0
	call	beeper ; BEEP !
	mov	word [Second], 3030h
	jmp	loc_enter_second_1
loc_set_time_bs_6:
	call	write_backspace
	jmp	loc_enter_time_separator_2
loc_set_time_stc_7:
	call	check_for_backspace
	je	short loc_set_time_bs_7
	;xor	bh, bh ; video page 0
	call	beeper ; BEEP !
	jmp	loc_enter_second_3
loc_set_time_bs_7:
	call	write_backspace
	jmp	loc_enter_second_1

loc_set_time_progress:
	; Get Current Time
	;mov 	ah, 02h
	;call	int1Ah
	call	RTC_20	; GET RTC TIME
	;DL = Daylight Savings Enable option (0-1)

	mov	ax, [Hour]
	sub	ax, '00'
	shl	al, 4 ; * 16
	mov	ch, al
	add	ch, ah
	mov	ax, [Minute]
	sub	ax, '00'
	shl	al, 4 ; * 16
	mov	cl, al
	add	cl, ah
	mov	ax, [Second]
	sub	ax, '00'
	shl	al, 4 ; * 16
	mov	dh, al
	add	dh, ah
	
	;mov	ah, 03h
	;call	int1Ah
	call	RTC_30	; SET RTC TIME

	; 26/07/2022
	jmp	loc_set_time_ok

print_volume_info:
	; 09/05/2025 - TRDOS 386 v2.0.10
	; 01/03/2016
	; 08/02/2016
	; 06/02/2016
	; 04/02/2016
	; 18/01/2016 (TRDOS 386 = TRDOS v2.0)
	; 25/10/2009
	;
	; "Volume Serial No: "
 	;
	; INPUT  : AL = DOS Drive Number
	; OUTPUT : AH = FS Type
	;          AL = DOS Drive Name
	; CF = 0 -> OK
	; CF = 1 -> Drive not ready

	mov	ah, al
	sub	al, al
	movzx	esi, ax	
	add	esi, Logical_DOSDisks
	mov	al, [esi]
	cmp	al, 'A'  
	jnb	short loc_pvi_set_vol_name
	mov	ah, [esi+LD_FSType]
	retn

loc_pvi_set_vol_name:
	mov	[Vol_Drv_Name], al
	push	esi
	call	move_volume_name_and_serial_no
	jnc	short loc_pvi_mvn_ok
	pop	esi
	retn

loc_pvi_mvn_ok:
	mov	esi, [esp]
	cmp	byte [esi+LD_FSType], 0A1h
	jne	short loc_pvi_fat_vol_size
	mov	eax, [esi+LD_FS_VolumeSize]
	movzx	ebx, word [esi+LD_FS_BytesPerSec]
	jmp	short loc_vol_size_mul32
loc_pvi_fat_vol_size:
	mov	eax, [esi+LD_TotalSectors]
	movzx	ebx, word [esi+LD_BPB+BPB_BytsPerSec]
loc_vol_size_mul32:
	mul	ebx
	or	edx, edx
	jnz	short loc_vol_size_in_kbytes
loc_vol_size_in_bytes:
	mov	ecx, VolSize_Bytes
	jmp	short loc_write_vol_size_str
loc_vol_size_in_kbytes:
	mov	bx, 1024
	div	ebx
	mov 	ecx, VolSize_KiloBytes
	xor	edx, edx ; 0
loc_write_vol_size_str:
	mov	[VolSize_Unit1], ecx
	; 
	mov	edi, Vol_Tot_Sec_Str_End
        ;mov	byte [edi], 0
	mov	ecx, 10
loc_write_vol_size_chr:
	div	ecx
	add	dl, '0'
	dec	edi
	mov	[edi], dl
	test	eax, eax
	jz	short loc_write_vol_size_str_ok
	sub	dl, dl ; 0
	jmp	short loc_write_vol_size_chr

loc_write_vol_size_str_ok:
	mov	[Vol_Tot_Sec_Str_Start], edi
	;
	mov	edi, Vol_FS_Name
	mov	cl, [esi+LD_FATType]
	and	cl, cl ; 0 ?
	jnz	short loc_write_vol_FAT_str_1
	mov	word [edi], 'TR'
	mov	dword [edi+4], ' FS1'
	;movzx	ebx, word [esi+LD_FS_BytesPerSec]
	mov	bx, [esi+LD_FS_BytesPerSec]
	mov	eax, [esi+LD_FS_FreeSectors]
	jmp	short loc_vol_freespace_mul32

loc_write_vol_FAT_str_1:
	mov	ax, '32' ; FAT32
	cmp	cl, 2 ; [esi+LD_FATType]
	ja	short loc_write_vol_FAT_str_2
	mov	ax, '12' ; FAT12
	jb	short loc_write_vol_FAT_str_2
	mov	ah, '6'  ; FAT16
loc_write_vol_FAT_str_2:
	mov	dword [edi], 'FAT '
	mov	word [edi+4], ax
	;
	;movzx	ebx, word [esi+LD_BPB+BPB_BytsPerSec]
	mov	bx, [esi+LD_BPB+BPB_BytsPerSec]
	mov	eax, [esi+LD_FreeSectors]
	; 09/05/2025
	; LD_FreeSectors = LD_FS_FreeSectors = offset 116

loc_vol_freespace_recalc0:
	; 09/05/2025
	and	cl, cl ; byte [esi+LD_FATType]
	jz	short loc_vol_freespace_mul32
	; 10/05/2025
	and	eax, eax
	jz	short loc_vol_freespace_recalc1
	; 01/03/2016
	cmp	eax, 0FFFFFFFFh
	jb	short loc_vol_freespace_mul32
	;inc	eax ; 0

; 10/05/2025
%if 0
	push	ebx
	mov	bx, 0FF00h ; recalculate free sectors
	call	calculate_fat_freespace
	pop	ebx
%else
loc_vol_freespace_recalc1:
	push	ebx
	call	get_free_FAT_sectors
	pop	ebx
	jnc	short loc_vol_fspc_skip_fsects
	mov	eax, -1 ; invalidate
loc_vol_fspc_skip_fsects:
	mov	[esi+LD_FreeSectors], eax ; Free sectors
%endif

loc_vol_freespace_mul32:
	; 10/05/2025
	mov	ecx, VolSize_Bytes
	mov	edi, Vol_Free_Sectors_Str_End
	;mov	byte [edi], 0
	inc	eax ; * ; -1 -> 0
	jnz	short loc_vol_freespace_mul32_@
	dec	edi
	mov	byte [edi], '?'
	jmp	short loc_write_vol_fspace_str_ok

loc_vol_freespace_mul32_@:
	dec	eax ; * 
	;
	mul	ebx
	or	edx, edx
	;jnz	short loc_vol_fspace_in_kbytes
loc_vol_fspace_in_bytes:
	; 10/05/2025
	;mov	ecx, VolSize_Bytes
	;jmp	short loc_write_vol_fspace_str
	jz	short loc_write_vol_fspace_str
loc_vol_fspace_in_kbytes:
	mov	bx, 1024
	div	ebx
	mov 	ecx, VolSize_KiloBytes
	xor	edx, edx ; 0
loc_write_vol_fspace_str:
	mov	[VolSize_Unit2], ecx
	;
	; 10/05/2025
	;mov	edi, Vol_Free_Sectors_Str_End
        ;mov	byte [edi], 0
	mov	ecx, 10
loc_write_vol_fspace_chr:
	div	ecx
	add	dl, '0'
	dec	edi
	mov	[edi], dl
	test	eax, eax
	jz	short loc_write_vol_fspace_str_ok
	sub	dl, dl ; 0
	jmp	short loc_write_vol_fspace_chr

loc_write_vol_fspace_str_ok:
	mov	[Vol_Free_Sectors_Str_Start], edi
	;
	mov	esi, Volume_in_drive
	call	print_msg
	mov	esi, Vol_Name
	call	print_msg
	mov	esi, nextline
	call	print_msg
	;
	mov	esi, Vol_Total_Sector_Header
	call	print_msg
	mov	esi, [Vol_Tot_Sec_Str_Start]
	call	print_msg
	mov	esi, [VolSize_Unit1]
	call	print_msg
	;
	mov	esi, Vol_Free_Sectors_Header
	call	print_msg
	mov	esi, [Vol_Free_Sectors_Str_Start]
	call	print_msg
	mov	esi, [VolSize_Unit2]
	call	print_msg
	;
	pop	esi

	;mov	ah, [esi+LD_FSType]
	;mov	al, [esi+LD_FATType]
	mov	ax, [esi+LD_FATType]

	retn

move_volume_name_and_serial_no:
	; 26/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 08/02/2016  (TRDOS 386 = TRDOS v2.0)
	; this routine will be called by
	; "print_volume_info" and "print_directory"
	; INPUT ->
	;	ESI = Logical DOS drv descripton table address
	; OUTPUT ->
	;	*Volume name will be moved to text area
	;	*Volume serial number will be converted to
	;	 text and will be moved to text area
	;   cf = 1 -> invalid/unknown dos drive
	;   cf = 0 -> ecx = 0
	;
	; (eax, edx, ecx, esi, edi will be changed)

	; 26/07/2022
	xor	ecx, ecx

	mov 	edi, Vol_Name

	;mov	ah, [esi+LD_FSType]
	;mov	al, [esi+LD_FATType]
	mov	ax, [esi+LD_FATType]
	cmp	ah, 0A1h
	je	short mvn_2
	or	ah, ah
	jz	short mvn_0
	or	al, al
	jnz	short mvn_1
mvn_0:
	mov	al, [esi]
	stc
	retn
mvn_1:
	cmp	al, 2
	ja	short mvn_3 
	;or	al, al
	;jz	short mvn_2
	mov	eax, [esi+LD_BPB+VolumeID]
	add	esi, LD_BPB+VolumeLabel
	jmp	short mvn_4
mvn_2:
	mov	eax, [esi+LD_FS_VolumeSerial]
	add	esi, LD_FS_VolumeName
	;mov	ecx, 16
	; 26/07/2022
	mov	cl, 16
	rep	movsd
	jmp	short mvn_5
mvn_3:
	mov	eax, [esi+LD_BPB+FAT32_VolID]
	add	esi, LD_BPB+FAT32_VolLab
mvn_4:
	;mov	ecx, 11
	; 26/07/2022
	mov	cl, 11
	rep	movsb
	mov	byte [edi], 0
mvn_5:
	;mov	[Current_VolSerial], eax
	call	dwordtohex
	mov	[Vol_Serial1], edx
	mov	[Vol_Serial2], eax
	; ecx = 0
	retn

get_volume_serial_number:
	; 19/01/2016 (TRDOS 386 = TRDOS v2.0)
	; 08/08/2010
	;
	; INPUT -> DL = Logical DOS Drive number
	; OUTPUT -> EAX = Volume serial number
	;          BL= FAT Type	    
	;          BH = Logical DOS drv Number (DL input)
	; cf = 1 -> Drive not ready

	xor	ebx, ebx
	mov	bh, dl
	cmp	[Last_DOS_DiskNo], dl
	jnb	short loc_gvsn_start
loc_gvsn_stc_retn:
	xor	eax, eax
	stc
        retn
loc_gvsn_start:
	push	esi
	mov	esi, Logical_DOSDisks
	add	esi, ebx
	mov	bl, [esi+LD_FATType]
	and	bl, bl
	jz	short loc_gvsn_fs
	cmp	bl, 2
	ja	short loc_gvsn_fat32
loc_gvsn_fat:
	add	esi, LD_BPB + VolumeID
	jmp	short loc_gvsn_return
loc_gvsn_fat32: 
	add	esi, LD_BPB + FAT32_VolID
	jmp	short loc_gvsn_return 
loc_gvsn_fs:
	cmp	byte [esi+LD_FSType], 0A1h
	jne	short loc_gvsn_stc_retn 
	add	esi, LD_FS_VolumeSerial
loc_gvsn_return:
	mov	eax, [esi]
	pop	esi
	retn

; CMD_INTR.ASM [ TRDOS Command Interpreter Procedure ]
; 09/11/2011
; 29/01/2005

command_interpreter:
	; 16/06/2025
	; 14/06/2025 (TRDOS 386 Kernel v2.0.10)
	; 26/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 16/10/2016
	; 12/10/2016
	; 13/05/2016
	; 07/05/2016
	; 04/03/2016
	; 04/02/2016
	; 03/02/2016
	; 30/01/2016
	; 29/01/2016 (TRDOS 386 = TRDOS 2.0)
	; 15/09/2011
	; 29/01/2005

	; Input: ecx = command word length (CL)
	;	 CommandBuffer = Command string offset

	mov	byte [Program_Exit], 0

	;cmp	cl, 4
	;ja	c_6
	;jb	c_2

	; 26/07/2022
	cmp	cl, 3
	ja	short c_4
	je	short c_3
	jmp	c_2
c_3:
cmp_cmd_dir:
	mov	edi, Cmd_Dir
	call	cmp_cmd
	;jnc	print_directory_list
	; 26/07/2022
	jc	short cmp_cmd_cls
	jmp	print_directory_list

cmp_cmd_cls:
	mov	cl, 3
	mov	edi, Cmd_Cls
	call	cmp_cmd
        ;jnc	clear_screen
	; 26/07/2022
	jc	short cmp_cmd_ver
	jmp	clear_screen

cmp_cmd_ver:
	mov	cl, 3
	mov	edi, Cmd_Ver
	call	cmp_cmd
	jc	short cmp_cmd_mem

	mov	esi, mainprog_Version
	;call	print_msg
	jmp	print_msg
	;retn

cmp_cmd_mem:
	mov	cl, 3
	mov	edi, Cmd_Mem
	call	cmp_cmd
	;jnc	memory_info
	; 26/07/2022
	jc	short cmp_cmd_del
	jmp	memory_info

cmp_cmd_del:
	mov	cl, 3
	mov	edi, Cmd_Del
	call	cmp_cmd
        ;jnc	delete_file
	; 26/07/2022
	jc	short cmp_cmd_set
	jmp	delete_file

cmp_cmd_set:
	mov	cl, 3
	mov	edi, Cmd_Set
	call	cmp_cmd
	;jnc	set_get_env
	; 26/07/2022
	jc	short cmp_cmd_run
	jmp	set_get_env

	; 07/08/2022
c_4:
	; 26/07/2022
	cmp	cl, 4
	je	short cmp_cmd_4
	jmp	c_6

cmp_cmd_run:
	mov	cl, 3
	mov	edi, Cmd_Run
	call	cmp_cmd
	; 07/05/2016
        ;jc	cmp_cmd_external
	; 26/07/2022
	jnc	short c3_run
	jmp	cmp_cmd_external
c3_run:
	jmp	load_and_execute_file

cmp_cmd_4:
	; 26/07/2022
cmp_cmd_exit:
	mov	edi, Cmd_Exit
	call	cmp_cmd
	jc	short cmp_cmd_date

        mov     byte [Program_Exit], 1
        retn

cmp_cmd_date:
	mov	cl, 4
	mov	edi, Cmd_Date
	call	cmp_cmd
        jc	short cmp_cmd_time
	
	call	show_date
	;call	set_date
	;retn
	; 26/07/2022
	jmp	set_date

cmp_cmd_time:
	mov	cl, 4
	mov	edi, Cmd_Time
   	call	cmp_cmd
	jc	short cmp_cmd_show

	call	show_time
	;call	set_time
	;retn
	; 26/07/2022
	jmp	set_time

cmp_cmd_show:
	mov	cl, 4
	mov	edi, Cmd_Show
   	call	cmp_cmd
        ;jnc	show_file
	; 26/07/2022
	jc	short cmp_cmd_echo
	jmp	show_file

cmp_cmd_echo:
	mov	cl, 4
	mov	edi, Cmd_Echo
   	call	cmp_cmd
	jc	short cmp_cmd_copy

	; 22/11/2017
	; AL = 0
	cmp	byte [esi], 20h
	jb	short cmd_echo_nextline
	; 14/04/2016
	push	esi
cmd_echo_asciiz:
	;inc	esi
	;mov	al, [esi]
	; 22/11/2017
	lodsb
	cmp	al, 20h
	jnb	short cmd_echo_asciiz
	dec	esi
	mov	byte [esi], 0
	pop	esi
	mov	edi, esi
	call	print_msg
	mov	byte [edi], 0
cmd_echo_nextline:
	mov	esi, NextLine
	;call	print_msg
	;retn
	jmp	print_msg

cmp_cmd_copy:
	mov	cl, 4
	mov	edi, Cmd_Copy
   	call	cmp_cmd
	;jnc	copy_file
	; 26/07/2022
	jc	short cmp_cmd_move
	jmp	copy_file

cmp_cmd_move:
	mov	cl, 4
	mov	edi, Cmd_Move
   	call	cmp_cmd
	;jnc	move_file
	; 26/07/2022
	jc	short cmp_cmd_path
	jmp	move_file

cmp_cmd_path:
	mov	cl, 4
	mov	edi, Cmd_Path
   	call	cmp_cmd
	;jnc	set_get_path
	; 26/07/2022
	jc	short cmp_cmd_beep
	jmp	set_get_path

cmp_cmd_beep:
	mov	cl, 4
	mov	edi, Cmd_Beep
   	call	cmp_cmd
	jc	short cmp_cmd_find
	; 13/05/2016
	mov	bh, [ptty] ; [ACTIVE_PAGE]
	jmp	beeper

cmp_cmd_find:
	mov	cl, 4
	mov	edi, Cmd_Find
   	call	cmp_cmd
        ;jc	cmp_cmd_external
	; 26/07/2022
	jnc	short c4_find
	jmp	cmp_cmd_external
c4_find:
	;call	find_and_list_files
	jmp	find_and_list_files
	;retn

c_1:
	lodsd
cmp_cmd_help:
	cmp	al, '?'
        jne     short cmp_cmd_remark

	mov	esi, Command_List
cmd_help_next_w:
	call	print_msg

	cmp	byte [esi], 20h ; 0
	jb	short cmd_help_retn

	push	esi
	mov	esi, nextline
	call	print_msg
	pop	esi
	jmp	short cmd_help_next_w

cmp_cmd_remark:
	cmp	al, '*'
	;jne	cmp_cmd_external
	; 26/07/2022
	je	short cmp_cmd_rem
	jmp	cmp_cmd_external
cmp_cmd_rem:
	inc	esi
	mov	edi, Remark
	mov	al, [esi]
	cmp	al, 20h
	ja	short cmd_remark_write
	mov	esi, edi ; Remark
	jmp	print_msg

cmd_remark_write:
	stosb
	lodsb
	cmp	al, 20h
	jnb	short cmd_remark_write
	mov	byte [edi], 0

cmd_help_retn:
cmd_remark_retn:
cd_retn:
	retn
c_2:
	; 26/07/2022
	mov	esi, CommandBuffer
	cmp	cl, 2
	;ja	c_3
	;mov	esi, CommandBuffer
	jb	short c_1
	; 26/07/2022
	;jne	short c_1

cmp_cmd_cd:
	lodsw
	cmp	ax, 'CD'
	jne	short cmp_cmd_drive
        inc	esi
cd_0:
	mov	ax, [esi]
	cmp	al, 20h
	jna	short cd_retn
	; 10/02/2016
	cmp	ah, ':'
	jne	short cd_1
	inc	esi
	inc	esi
	jmp	short cd_2

cd_1:	; change current directory
	; 29/11/2009
	; AH = CDh	; to separate 'CD' command from others
			; for restoring current directory
			; 0CDh sign is for saving cdir into
			; DOS drv description table cdir area

	mov	ah, 0CDh ; mov byte [CD_COMMAND], 0CDh

	call	change_current_directory
	;jnc	change_prompt_dir_string
	; 26/07/2022
	jc	short cd_error_messages
	jmp	change_prompt_dir_string

cd_error_messages:
	cmp	al, 3
	je	short cd_path_not_found
	; 16/10/2016 (15h -> 15)
	cmp	al, 15 ; drive not ready error
	je	short cd_drive_not_ready
	cmp	al, 17 ; read error
	je	short cd_drive_not_ready
	cmp	al, 19 ; bad directory/path name
	je	short cd_command_failed
	; 14/06/2025 - TRDOS 386 v2.0.10
	cmp	al, 11 ; permission denied
	je	short cd_command_failed

cd_path_not_found:
	push	eax ; 29/12/2017
	;push	ax
	mov	esi, Msg_Dir_Not_Found
	call	print_msg
	;pop	ax
	pop	eax ; 29/12/2017
	cmp	ah, [Current_Dir_Level]
        ;jnb	change_prompt_dir_string
	; 26/07/2022
	jnb	short cd_cpds
	mov	[Current_Dir_Level], ah
cd_cpds:
        jmp     change_prompt_dir_string

cmp_cmd_drive: ; change current drive
	; C:, D:, E: etc.
	cmp	ah, ':'
	;jne	cmp_cmd_external
	; 26/07/2022
	je	short cd_2
cmd_ext:
	jmp	cmp_cmd_external

cd_2:	; 'CD C:', 'CD D:' ...
	cmp	byte [esi], 20h
	;ja	loc_cmd_failed
	; 26/07/2022
	ja	short cd_failed
	and	al, 0DFh
	sub	al, 'A'
	;jc	loc_cmd_failed
	; 26/07/2022
	jnc	short cd_3
cd_failed:
	jmp	loc_cmd_failed
cd_3:
        cmp     al, [Last_DOS_DiskNo]
        ja	short cd_drive_not_ready

	mov	dl, al
	call 	change_current_drive
	jc	short cd_drive_not_ready
	retn

cd_drive_not_ready:
	mov	esi, Msg_Not_Ready_Read_Err
	call	print_msg

cd_fail_drive_restart:
	mov	dl, [Current_Drv]
	;call 	change_current_drive
        jmp     change_current_drive
	;retn

cd_command_failed:
	mov	esi, Msg_Bad_Command
	call	print_msg
	jmp	short cd_fail_drive_restart

c_5:
cmp_cmd_mkdir:
	mov	edi, Cmd_Mkdir
	call	cmp_cmd
	;jnc	make_directory
	; 26/07/2022
	jc	short cmp_cmd_rmdir
	jmp	make_directory

cmp_cmd_rmdir:
	mov	cl, 5
	mov	edi, Cmd_Rmdir
	call	cmp_cmd
	;jnc	delete_directory
	; 26/07/2022
	jc	short cmp_cmd_chdir
	jmp	delete_directory

cmp_cmd_chdir:
	mov	cl, 5
	mov	edi, Cmd_Chdir
	call	cmp_cmd
	;jc	cmp_cmd_external
	; 26/07/2022
	jc	short cmd_ext

	jmp	cd_0

c_6:
	cmp	cl, 6
	;ja	c_8
	; 26/07/2022
	jb	short c_5
	je	short cmd_6
	jmp	c_8

cmd_6:
cmp_cmd_prompt:
	mov	edi, Cmd_Prompt
	call	cmp_cmd	
        jc	short cmp_cmd_volume
get_prompt_name_fchar:
	lodsb
	cmp	al, 20h
	je	short get_prompt_name_fchar
	ja	short loc_change_prompt_label
default_command_prompt: ; 31/12/2017 ('sysprompt')
	mov	esi, TRDOSPromptLabel
	mov	dword [esi], "TRDO"
       	mov	word [esi+4], "S"
loc_cmd_prompt_return:
	retn

set_command_prompt: ; 31/12/2017 ('sysprompt')
	lodsb
loc_change_prompt_label:
	;mov	cx, 11
	; 26/07/2022
	sub	ecx, ecx
	mov	cl, 11
	mov	edi, TRDOSPromptLabel
put_char_new_prompt_label:
	stosb
	lodsb
	;cmp	al, 20h
	;jb	short pass_put_new_prompt_label
	; 16/06/2025 - TRDOS 386 v2.0.10
	cmp	al, 21h	
	jb	short pass_put_new_prompt_label
	loop	put_char_new_prompt_label
pass_put_new_prompt_label:
	mov	byte [edi], 0
	retn

cmp_cmd_volume:
	mov	cl, 6
	mov	edi, Cmd_Volume
	call	cmp_cmd
        jc	short cmp_cmd_attrib

cmd_vol1:
	lodsb
	cmp	al, 20h
	ja	short cmd_vol2
	mov	al, [Current_Drv]
	jmp	short cmd_vol4
cmd_vol2:
	cmp	al, 'A'
	;jb	loc_cmd_failed
	; 26/07/2022
	jb	short cmd_vol_failed_1
	cmp	al, 'z'
	;ja	loc_cmd_failed
	; 26/07/2022
	ja	short cmd_vol_failed_2
	cmp	al, 'Z'
	jna	short cmd_vol3
	cmp	al, 'a'
        ;jb	loc_cmd_failed
	; 26/07/2022
	jb	short cmd_vol_failed_3
	and	al, 0DFh
cmd_vol3:
	mov	ah, [esi]
	cmp	ah, ':'
        jne     loc_cmd_failed
	sub	al, 'A'
        cmp     al, [Last_DOS_DiskNo]
	jna	short cmd_vol4

	mov	esi, Msg_Not_Ready_Read_Err
	jmp	print_msg

	; 26/07/2022
	; (numeric characters and underscore are allowed)
cmd_vol_failed_1:
	; check for numeric characters
	cmp	al, '0'
	jb	short cmd_vol_failed_2
	cmp	al, '9'
	jna	short cmd_vol3
cmd_vol_failed_2:
	jmp	loc_cmd_failed
cmd_vol_failed_3:
	cmp	al, '_' ; underline ?
	je	short cmd_vol3 ; is ok..
	jmp	short cmd_vol_failed_2

cmd_vol4:
	call	print_volume_info
	;jc	cd_drive_not_ready
	; 26/07/2022
	jnc	short cmd_vol5
	jmp	cd_drive_not_ready
;cmd_vol5:
;	retn

cmp_cmd_attrib:
	mov	cl, 6
	mov	edi, Cmd_Attrib
	call	cmp_cmd
	;jnc	set_file_attributes
	; 26/07/2022
	jc	short cmp_cmd_rename
	jmp	set_file_attributes

cmp_cmd_rename:
	mov	cl, 6
	mov	edi, Cmd_Rename
	call	cmp_cmd
	;jnc	rename_file
	; 26/07/2022
	jc	short cmp_cmd_device
	jmp	rename_file

cmp_cmd_device:
	mov	cl, 6
	mov	edi, Cmd_Device
	call	cmp_cmd
	jc	short cmp_cmd_external
	; 26/07/2022
cmd_vol5:
cmd_dev:
	retn

c_7:
cmp_cmd_devlist:
	mov	edi, Cmd_DevList
	call	cmp_cmd
        jc	short cmp_cmd_external

loc_cmd_return:
	retn

c_8:
        cmp	cl, 8
	ja	short cmp_cmd_external
	jb	short c_7

cmp_cmd_longname:
	mov	edi, Cmd_LongName
	call	cmp_cmd	
	;jnc	get_and_print_longname
	; 26/07/2022
	jc	short cmp_cmd_external
	jmp	get_and_print_longname

cmp_cmd_external:
	; 07/05/2016
	; 22/04/2016
	mov	esi, CommandBuffer
	jmp	loc_run_check_filename

loc_cmd_failed:
	cmp	byte [CommandBuffer], 20h
	jna	short loc_cmd_return
	mov	esi, Msg_Bad_Command
;	call	print_msg
;loc_cmd_return:
;	retn
	jmp	print_msg

cmp_cmd:
	; 26/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 29/01/2016 (TRDOS 386 = TRDOS v2.0)
        mov	esi, CommandBuffer
        ; edi = internal command word (ASCIIZ)
	; ecx = command length (<=8)
cmp_cmd_1:
	lodsb
	scasb
	jne	short cmp_cmd_3
	loop	cmp_cmd_1
 	lodsb
	cmp	al, 20h
	ja	short cmp_cmd_2
	xor	al, al
	; ZF = 1 -> internal command word matches
	retn
cmp_cmd_2:
	; ZF = 0 (CF = 0) -> external command word
	pop	eax ; no return to the caller from here
	jmp	short cmp_cmd_external ; 26/07/2022
cmp_cmd_3:
	stc
	; CF = 1 -> internal command word does not match
	retn

loc_run_cmd_failed:
	; 26/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 15/03/2016
	; 15/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 07/12/2009 (CMD_INTR.ASM)
	; 29/11/2009

	call	restore_cdir_after_cmd_fail

loc_run_cmd_failed_cmp_al:
	; End of Restore_CDIR code (29/11/2009)

	cmp	al, 1 ; Bad command or file name
	je	short loc_cmd_failed ; 26/07/2022
loc_run_dir_not_found:
	cmp	al, 3
	jne	short loc_run_file_notfound_msg
	; Path not found (MS-DOS Error Code = 3)
	mov	esi, Msg_Dir_Not_Found
	jmp	print_msg

loc_run_file_notfound_msg:
	cmp	al, 2 ; File not found
	jne	short loc_run_file_drv_read_err

loc_print_file_notfound_msg:
        mov     esi, Msg_File_Not_Found
	;call	proc_printmsg
	;retn
	jmp	print_msg

loc_run_file_drv_read_err:
	; Err: 17 (Read fault)
	cmp	al, 17 ; Drive not ready or read error
	je	short loc_run_file_print_drv_read_err
	;
	cmp	al, 15 ; Drive not ready (or read error)
	jne	short loc_run_file_toobig

loc_run_file_print_drv_read_err:
	mov	esi, Msg_Not_Ready_Read_Err
	jmp	print_msg

loc_run_file_toobig:
	cmp	al, 8 ; Not enough free memory to load&run file
	jne	short loc_run_file_perm_denied
	mov	esi, Msg_Insufficient_Memory
	jmp	print_msg

loc_run_file_perm_denied:
	; 29/12/2017
	cmp	al, ERR_PERM_DENIED ; 11 ; Permission denied
	jne	short loc_run_misc_error
	mov	esi, Msg_Permission_Denied
	jmp	print_msg

	; 15/03/2016
print_misc_error_msg:
loc_run_misc_error:
	; AL = Error code
	call	bytetohex
        mov     [error_code_hex], ax

	mov	esi, Msg_Error_Code 
	;call	print_msg 
	;retn
	jmp	print_msg

restore_cdir_after_cmd_fail:
	; 15/02/2016 (TRDOS 386 = TRDOS v2.0)
	push	eax
	mov	bh, [RUN_CDRV] ; it is set at the beginning
				; of the 'run' command.
	cmp	bh, [Current_Drv]
	je	short loc_run_restore_cdir
	mov	dl, bh
	call	change_current_drive 
	jmp	short loc_run_err_pass_restore_cdir

loc_run_restore_cdir:
	cmp	byte [Restore_CDIR], 0
	jna	short loc_run_err_pass_restore_cdir
	xor	bl, bl
	movzx	esi, bx
	add	esi, Logical_DOSDisks
	call	restore_current_directory

loc_run_err_pass_restore_cdir:
	pop	eax
	retn

print_directory_list:
	; 02/12/2023 (TRDOS 386 v2.0.7)
	; 07/08/2022
	; 27/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 10/02/2016
	; 08/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 06/12/2009 ('cmp_cmd_dir')
	;

; 07/06/2025 - temporary (for test)
%if 0
 	push	esi
 	mov	edi, Dir_File_Name
 	call	convert_file_name
 	mov	esi, Dir_File_Name
 	call	print_msg
 	pop	esi
 	xor	ah, ah
 	int	32h
%endif
	mov	word [AttributesMask], 0800h ; ..except volume names..
	mov	al, [Current_Drv]
	mov	[RUN_CDRV], al
	; 02/12/2023
	cmp	al, 2
	jnb	short get_dfname_fchar
	mov	al, [TIMER_LOW+1]
	shr	al, 1	; 512/18.2 (>= 28 seconds)
	xchg	al, [P_TIMER]  ; 28 seconds
	cmp	al, [P_TIMER]
	je	short get_dfname_fchar
	call	get_media_change_status
	jc	short pdl_chdrv
	cmp	ah, 6
	jne	short get_dfname_fchar
pdl_chdrv:
	push	esi
	mov	dl, [RUN_CDRV]
	call	change_current_drive
	pop	esi
	jnc	short get_dfname_fchar
	mov	esi, Msg_Not_Ready_Read_Err
	;call	print_msg
	;retn
	jmp	print_msg

get_dfname_fchar:
	lodsb
	cmp	al, 20h
	je	short get_dfname_fchar
	;jb	loc_print_dir_call_all
	; 02/12/2023
	ja	short get_dfname_fchar_2
	jmp	loc_print_dir_call_all
get_dfname_fchar_2:
	cmp	al, '-'
	jne	short loc_print_dir_call_flt
get_next_attr_char:
	lodsb
	cmp	al, 20h
	je	short get_next_attr_char
	;jb	loc_cmd_failed
	; 27/07/2022
	jnb	short pdl_1
pdl_0:
	jmp	loc_cmd_failed
pdl_1:
	and	al, 0DFh
	cmp	al, 'D' ; directories only ?
	jne	short pass_only_directories
	lodsb
	cmp	al, 20h
	;ja	loc_cmd_failed
	; 27/07/2022
	ja	short pdl_0
	or	byte [AttributesMask], 10h ; ..directory..
	jmp	short get_dfname_fchar_attr
pass_only_directories:
	cmp	al, 'F'	; files only ?
	jne	short check_attr_s ; 27/07/2022
	lodsb
	cmp	al, 20h
	;ja	loc_cmd_failed
	; 27/07/2022
	ja	short pdl_0
	or	byte [AttributesMask+1], 10h ; ..except directories..
get_dfname_fchar_attr:
	lodsb
	cmp	al, 20h
	je	short get_dfname_fchar_attr
	;jb	short loc_print_dir_call_all
	; 07/08/2022
	jnb	short loc_print_dir_call_flt
	jmp	loc_print_dir_call_all
loc_print_dir_call_flt:
	dec	esi
	mov	edi, FindFile_Drv
	call	parse_path_name
 	jnc	short loc_print_dir_change_drv_1
	cmp	al, 1
	;ja	loc_run_cmd_failed
	; 27/07/2022
	jna	short loc_print_dir_change_drv_1
pdl_2:
	jmp	loc_run_cmd_failed

	; 27/07/2022
check_attr_s_cap:
	and	al, 0DFh
check_attr_s:
	cmp	al, 'S'
	jne	short pass_attr_s
	or	byte [AttributesMask], 4 ; system
	lodsb
	cmp	al, 20h
	je	short get_dfname_fchar_attr
	jb	short loc_print_dir_call_all
	and	al, 0DFh
pass_attr_s:
	cmp	al, 'H'
	jne	short pass_attr_h
	or	byte [AttributesMask], 2 ; hidden
pass_attr_shr:
	lodsb
	cmp	al, 20h
	je	short get_dfname_fchar_attr
	jb	short loc_print_dir_call_all
	jmp	short check_attr_s_cap
pass_attr_h:
	cmp	al, 'R'
	jne	short pass_attr_r
	or	byte [AttributesMask], 1 ; read only
	jmp	short pass_attr_shr
pass_attr_r:
	cmp	al, 'A'
	;jne	loc_cmd_failed
	; 27/07/2022
	je	short pass_attr_a
	jmp	loc_cmd_failed
pass_attr_a:
	or	byte [AttributesMask], 20h ; archive
	jmp	short pass_attr_shr

	; 07/08/2022
loc_print_dir_change_drv_1:
	mov	dl, [FindFile_Drv]
loc_print_dir_change_drv_2:
	cmp	dl, [RUN_CDRV]
	je	short loc_print_dir_change_directory 
	call	change_current_drive
        ;jc	loc_run_cmd_failed
	; 27/07/2022
	jc	short pdl_2
loc_print_dir_change_directory:
	cmp	byte [FindFile_Directory], 20h ; 0 or 20h ?
	jna	short pass_print_dir_change_directory

	inc	byte [Restore_CDIR]
	mov	esi, FindFile_Directory
	xor	ah, ah ; CD_COMMAND sign -> 0
	call	change_current_directory
        ;jc	loc_run_cmd_failed
	; 27/07/2022
	jc	short pdl_2

loc_print_dir_change_prompt_dir_string:
	call	change_prompt_dir_string

pass_print_dir_change_directory:
	mov	esi, FindFile_Name
	cmp	byte [esi], 20h	; 0 or 20h ?
	ja	short loc_print_dir_call

loc_print_dir_call_all:
	mov	dword [esi], '*.*'
loc_print_dir_call:
	call	print_directory

	mov	dl, [RUN_CDRV]	; it is set at the beginning
	cmp	dl, [Current_Drv]
	je	short loc_print_dir_call_restore_cdir_retn
	;call	change_current_drive
	;retn
	; 27/07/2022
	jmp	change_current_drive

loc_print_dir_call_restore_cdir_retn:
	cmp	byte [Restore_CDIR], 0
	jna	short pass_print_dir_call_restore_cdir_retn

	mov	esi, Logical_DOSDisks
	xor	eax, eax
	mov	ah, dl
	add	esi, eax

	;call	restore_current_directory
	; 27/07/2022
	jmp	restore_current_directory

pass_print_dir_call_restore_cdir_retn:
	retn

print_directory:
	; 16/06/2025 (TRDOS 386 Kernel v2.0.10)
	; 27/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 13/05/2016
	; 11/02/2016
	; 10/02/2016
	; 08/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 30/10/2010 ('proc_print_directory')
	; 19/09/2009
	; 2005
	; INPUT ->
	;	ESI = Asciiz File/Dir Name Address

	push	esi

	sub	eax, eax

	mov	[Dir_Count], ax ; 0
	mov 	[File_Count], ax ; 0
	mov 	[Total_FSize], eax ; 0

	call    clear_screen

	xor	ecx, ecx
	mov     ch, [Current_Drv] ; DirBuff_Drv - 'A'
	mov     al, [Current_Dir_Drv]
	mov     [Dir_Drive_Name], al
	mov	esi, Logical_DOSDisks
	add	esi, ecx

	call	move_volume_name_and_serial_no
	jnc	short print_dir_strlen_check

	pop	esi
	mov	bh, [ptty] ; [ACTIVE_PAGE]
	;call	beeper
	;retn
	jmp	beeper  ; beep ! and return

print_dir_strlen_check:
	mov	esi, Current_Dir_Root
	; 16/06/2025 - TRDOS 386 v2.0.10
	;mov	edi, Dir_Str_Root
	mov	edi, Dir_Str

	;xor	ecx, ecx
        mov     cl, [Current_Dir_StrLen]
	inc	cl
	; 16/06/2025
	cmp	cl, 67
	;cmp	cl, 64
	jna	short pass_print_dir_strlen_shorting
	inc	esi
	add	esi, ecx
	;sub	esi, 64
	; 16/06/2025
	mov	cl, 63
	sub	esi, ecx ; 63 ; '.../'
	;inc	edi
	;mov	eax, '... '
	; 16/06/2025
	mov	eax, '.../'
	stosd

	; 16/06/2025
print_dir_strlen_shorting_@:
	lodsb
	cmp	al, '/'
	jne	short print_dir_strlen_shorting_@

	; esi points to the 1st non-path ('/') char

pass_print_dir_strlen_shorting:
	rep	movsb

	mov	esi, Dir_Drive_Str
	call	print_msg

	mov	esi, Vol_Serial_Header
	call	print_msg

	mov	esi, Dir_Str_Header
	call	print_msg
	
	mov	esi, next2line
	call	print_msg

loc_print_dir_first_file:
	mov	byte [PrintDir_RowCounter], 16
	mov	ax, [AttributesMask]
	pop	esi

	call	find_first_file
	;jc	loc_dir_ok
	; 27/07/2022
	jnc	short loc_dfname_use_this
	jmp	loc_dir_ok
	 
loc_dfname_use_this:
	; bl =	File Attributes (bh = Long Name Entry Length)
	test	bl, 10h  ; Is it a directory?
	jz	short loc_not_dir

	inc	word [Dir_Count]
	mov	edx, esi 	; FindFile_DirEntry address
 	mov	esi, Type_Dir	; '<DIR>     '
	mov	edi, Dir_Or_FileSize
	; move 10 bytes
	movsd
	movsd
	movsw
	mov	esi, edx
	jmp     short loc_dir_attribute

loc_not_dir:
	inc	word [File_Count]
	add	[Total_FSize], eax

	mov	ecx, 10  ; 32 bit divisor
	mov	edi, ecx
	add	edi, Dir_Or_FileSize
loc_dir_rdivide:
	sub	edx, edx
	div	ecx 	 ; remainder in dl (< 10)
	add     dl, '0'	 ; to make visible (ascii)
	dec	edi
	mov     [edi], dl
	and	eax, eax
	jnz	short loc_dir_rdivide

loc_dir_fill_space:
	cmp     edi, Dir_Or_FileSize
	jna     short loc_dir_attribute
	dec     edi
	mov     byte [edi], 20h
	jmp     short loc_dir_fill_space

loc_dir_attribute:
	mov	dword [File_Attribute], 20202020h

	cmp	bl, 20h  ; Is it an archive file?
	jb	short loc_dir_pass_arch
	mov	byte [File_Attribute+3], 'A'

loc_dir_pass_arch:
	and	bl, 7
	jz	short loc_dir_file_name
	mov	bh, bl
	and	bl, 3
	cmp	bh, bl
	jna	short loc_dir_pass_s
	mov	byte [File_Attribute], 'S'

loc_dir_pass_s:
	and     bl,2
	jz      short loc_dir_pass_h
	mov     byte [File_Attribute+1], 'H'
loc_dir_pass_h:
	and     bh,1
	jz      short loc_dir_file_name
	mov     byte [File_Attribute+2], 'R'
loc_dir_file_name:
	;mov	bx, [esi+18h] ; Date
	;mov	dx, [esi+16h] ; Time
	mov	ebx, [esi+16h]
	mov	ecx, esi ; FindFile_DirEntry address
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
	; 27/07/2022
	mov	eax, ebx
	and	ax, 00011111b	; Day Mask
	aam			; Q([AL]/10)->AH
				; R([AL]/10)->AL
				; [AL]+[AH]= Day as BCD
	or	ax, '00'	; Convert to ASCII
	xchg	al, ah

	mov	[File_Day], ax

	;mov	ax, bx
	; 27/07/2022
	mov	eax, ebx
	;shr	ax, 5		; shift right 5 times
	shr	eax, 5
	and	ax, 00001111b	; Month Mask
	aam
	or	ax, '00'
	xchg	ah, al
	mov	[File_Month], ax

	;mov	ax, bx
	; 27/07/2022
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
	push	esi
	mov     esi, File_Name
	call	print_msg
	mov	esi, nextline
	call	print_msg
	pop	esi

	dec	byte [PrintDir_RowCounter]
	jz	short pause_dir_scroll ; 27/07/2022

loc_next_entry:
	call	find_next_file
	;jnc	loc_dfname_use_this
	; 27/07/2022
	jc	short loc_dir_ok
	jmp	loc_dfname_use_this

	; 27/07/2022
pause_dir_scroll:
	sub	ah, ah
	call	int16h
	cmp	al, 1Bh
	je	short loc_dir_ok
	mov	byte [PrintDir_RowCounter], 16 ; Reset counter
        jmp     short loc_next_entry

loc_dir_ok:
	mov     ecx, 10
	;mov	ax, [Dir_Count]
	; 27/07/2022
	movzx	eax, word [Dir_Count]
	mov	edi, Decimal_Dir_Count
	;cmp	ax, cx ; 10
	; 27/07/2022
	cmp	eax, ecx
	jb	short pass_ddc
	inc	edi
	cmp	ax, 100
	jb	short pass_ddc
	inc	edi
	cmp	ax, 1000
	jb	short pass_ddc
	inc	edi
	cmp	ax, 10000
	jb	short pass_ddc
	inc	edi
pass_ddc:
	mov     [edi+1], ch ; 0
loc_ddc_rediv:
	xor     edx, edx
	;div	cx  ; 10
	; 27/07/2022
	div	ecx
	add     dl, '0'
	mov     [edi], dl
	dec     edi
	;or	ax, ax
	; 27/07/2022
	or	eax, eax
	jnz	short loc_ddc_rediv

	mov     ax, [File_Count]
	mov     edi, Decimal_File_Count
	;cmp	ax, cx ; 10
	; 27/07/2022
	cmp	eax, ecx ; 10
	jb      short pass_dfc
	inc     edi
	cmp     ax, 100
	jb      short pass_dfc
	inc     edi
	cmp     ax, 1000
	jb      short pass_dfc
	inc     edi
	cmp     ax, 10000
	jb      short pass_dfc
	inc     edi
pass_dfc:
	;mov    cx, 10
	mov     [edi+1], ch ; 00
loc_dfc_rediv:
	;;xor	dx, dx
	;xor	dl, dl
	;div	cx
	; 27/07/022
	xor	edx, edx
	div	ecx
	add	dl, '0'
	mov	[edi], dl
	dec	edi
	;or	ax, ax
	; 27/07/2022
	or	eax, eax
	jnz	short loc_dfc_rediv

	mov     edi, TFS_Dec_End
        ;mov    byte [edi], 0
	mov     eax, [Total_FSize]
	;mov    ecx, 10
rediv_tfs_hex:
	sub	edx, edx ; 27/07/2022
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
	;call	print_msg
	;retn
	; 27/07/2022
	jmp	print_msg

find_first_file:
	; 10/06/2025
	; 02/06/2025
	; 29/05/2025
	; 19/05/2025
	; 18/05/2025
	; 17/05/2025
	; 15/05/2025 (TRDOS 386 Kernel v2.0.10)
	; 28/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 11/02/2016
	; 10/02/2016
	; 08/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 09/10/2011
	; 17/09/2009
	; 2005
	; INPUT ->
	;	ESI = ASCIIZ File/Dir Name Address (in Current Directory)
	;	AL = Attributes AND mask (The AND result must be equal to AL)
	;	      bit 0 = Read Only
	;	      bit 1 = Hidden
	;	      bit 2 = System
	;	      bit 3 = Volume Label
	;	      bit 4 = Directory
	;	      bit 5 = Archive
	;	      bit 6 = Reserved, must be 0
	;	      bit 7 = Reserved, must be 0
	;       AH = Attributes Negative AND mask (The AND result must be ZERO)
	;
	; OUTPUT ->
	;	CF = 1 -> Error, Error Code in EAX (AL)
	;	CF = 0 ->
	;	     ESI = Directory Entry (FindFile_DirEntry) Location
	;	     EDI = Directory Buffer Directory Entry Location
	;	     EAX = File Size
	;	      BL = Attributes of The File/Directory
	;	      BH = Long Name Yes/No Status (>0 is YES)
	; 	      ;DX > 0 : Ambiguous filename chars are used
	;	      18/05/2025
	;	      DL > 0 : Ambiguous filename chars are used

	; (EAX, EBX, ECX, EDX, ESI, EDI will be changed)

	mov	[FindFile_AttributesMask], ax
	mov	edi, FindFile_DirEntry ; TR-DOS Fullfilename formatted buffer
	xor	eax, eax
	;mov	ecx, 11
	; 28/07/2022
	xor	ecx, ecx
	mov	cl, 11
	rep	stosd	; 44 bytes
	;stosw		; +2 bytes

	; 10/06/2025
	mov	[FindFile_MatchCounter], ax ; 0

	mov	edi, FindFile_Name ; FFF structure, offset 105 ; 15/05/2025
	cmp	esi, edi
	je	short loc_fff_mfn_ok
	mov	edx, edi
	; move 13 bytes
	movsd
	movsd
	movsd
	stosb
	mov	esi, edx
loc_fff_mfn_ok:
	;mov	edi, Dir_Entry_Name ; Dir Entry Format File Name
	; 18/05/2025
	mov	edi, FindFile_DirEntryName
	call	convert_file_name
	mov	esi, edi ; offset Dir_Entry_Name
	; esi = offset FindFile_DirEntryName

	; 29/05/2025
	;mov	word [FindFile_Reserved1], -1
	mov	word [FindFile_LastEntryNumber], -1 ; 65535

	mov	ax, [FindFile_AttributesMask]
	;xor	ecx, ecx
	xor	cl, cl
	call	locate_current_dir_file
	jc	short loc_fff_retn
	; EDI = Directory Entry
	; EBX = Directory Buffer Entry Index/Number

loc_fff_fnf_found:
	; 17/05/2025
	mov	cl, [CLUSFAC]
	mov	[FindFile_DirSectorCount], cl
	mov	ecx, [DIRSEC]
	mov	[FindFile_DirSector], ecx
	; 19/05/2025
	;mov	ecx, [CurrentBuffer]
	;add	ecx, BUFINSIZ
	;mov	[FindFile_DirBuffer], ecx

loc_fff_fnf_ln_check:
	;xor	ch, ch
	; 17/05/2025
	xor 	ecx, ecx
	xor	dh, 0Fh
	jz	short loc_fff_longname_yes
	mov	[FindFile_LongNameYes], ch ; 0
	jmp	short loc_fff_longname_no

	; 02/06/2025
loc_fff_retn:
	retn

loc_fff_longname_yes:
	;inc	byte [FindFile_LongNameYes]
	mov	cl, [LFN_EntryLength]
	mov	[FindFile_LongNameEntryLength], cl ; FindFile_LongNameYes

loc_fff_longname_no:
	;mov	bx, [DirBuff_CurrentEntry]
	;mov	[FindFile_DirEntryNumber], bx
	; 17/05/2025
	mov	[FindFile_DirEntryNumber], bl
	;mov	dx, ax ; Ambiguous Filename chars used sign > 0
	; 28/07/2022
	mov	edx, eax

	;;;
	; 29/05/2025
	mov	eax, [DirEntry_Counter]
	;cmp	eax, 65535
	cmp	eax, 65534
	ja	short loc_fff_fnf_chk_mc
	mov	[FindFile_LastEntryNumber], ax
loc_fff_fnf_chk_mc:
	;;;

	; 17/05/2025
	cmp	word [FindFile_MatchCounter], 0
	jnz	short loc_fff_fnf_inc_mc ; find next file

	mov	al, [Current_Drv]
	mov	[FindFile_Drv], al

	mov	eax, [Current_Dir_FCluster]
	mov	[FindFile_DirFirstCluster], eax

loc_fff_fnf_inc_mc:
	mov	eax, [DirBuff_Cluster]
	mov	[FindFile_DirCluster], eax

	inc	word [FindFile_MatchCounter]

	mov	ebx, edi
	mov	esi, edi
	mov	edi, FindFile_DirEntry
	mov	eax, edi
	mov	cl, 8
	rep	movsd
	mov	esi, eax
	mov	edi, ebx

	mov	eax, [FindFile_DirEntry+28] ; File Size

	mov	bl, [FindFile_DirEntry+11] ; File Attributes
	mov	bh, [FindFile_LongNameYes]

	;mov	cx, [DirBuff_EntryCounter]
	;mov	[FindFile_DirEntryNumber], cx
	;mov	cx, [FindFile_DirEntryNumber]
	; ecx = 0

	retn

find_next_file:
	; 29/05/2025
	; 19/05/2025
	; 18/05/2025
	; 17/05/2025 (TRDOS 386 Kernel v2.0.10)
	; 28/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 15/10/2016
	; 10/02/2016
	; 08/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 06/02/2011
	; 17/09/2009
	; 2005
	; INPUT ->
	;	NONE, Find First File Parameters
	; OUTPUT ->
	;	CF = 1 -> Error, Error Code in EAX (AL)
	;	CF = 0 ->
	;	    ESI = Directory Entry (FindFile_DirEntry) Location
	;	    EDI = Directory Buffer Directory Entry Location
	;	    EAX = File Size
	;	      BL = Attributes of The File/Directory
	;	      BH = Long Name Yes/No Status (>0 is YES)
	; 	      ;DX > 0 : Ambiguous filename chars are used
	;	      18/05/2025
	;	      DL > 0 : Ambiguous filename chars are used
	;
	; (EAX, EBX, ECX, EDX, ESI, EDI will be changed)

	cmp	word [FindFile_MatchCounter], 0
	jna	short loc_fnf_stc_retn

loc_start_search_next_file:
	; 17/05/2025
	;;mov	bx, [FindFile_DirEntryNumber]
	;movzx	ebx, byte [FindFile_DirEntryNumber]
	mov	bl, [FindFile_DirEntryNumber]
	;inc	bx
	; 28/07/2022
	inc	ebx
	; 19/05/2025
	and	bl, 0Fh ; 15
	mov	[FindFile_DirEntryNumber], bl
	jz	short loc_cont_search_next_file

	;;cmp	bx, [DirBuff_LastEntry]
	;;ja	short loc_cont_search_next_file
	;; 17/05/2025
	;cmp	bl, 16 ; 512/32
	;jnb	short loc_cont_search_next_file

loc_fnf_search:
	; 19/05/2025
	mov	eax, [FindFile_DirSector]
	xor	edx, edx
	mov	dh, [FindFile_Drv]
	add	edx, Logical_DOSDisks
	mov	cl, [edx+LD_PhyDrvNo]
	
	call	GETBUFFER
	jc	short loc_fnf_retn
	
	;mov	esi, [CurrentBuffer]
	or	byte [esi+BUFFINFO.buf_flags], buf_isDIR
	
	lea	edi, [esi+BUFINSIZ]

	;movzx	ebx, byte [FindFile_DirEntryNumber]
	mov	bl, [FindFile_DirEntryNumber]

loc_fnf_search_@:
	; 19/05/2025
	; ebx (bl) = [FindFile_DirEntryNumber]

	;mov	esi, Dir_Entry_Name
	; 18/05/2025
	mov	esi, FindFile_DirEntryName
	mov	ax, [FindFile_AttributesMask]
	;xor	cx, cx
	; 28/07/2022
	xor	ecx, ecx
	call	find_directory_entry
	;jnc	loc_fff_fnf_ln_check
	; 17/05/2025
	; 28/07/2022
	jc	short loc_cont_search_next_file_@
	jmp	loc_fff_fnf_ln_check

loc_fnf_stc_retn:
	stc
loc_fnf_ax12h_retn:
	mov	eax, 12 ; No More files
loc_fnf_retn:
	retn

loc_cont_search_next_file_@:
	; 17/05/2025
	;or	cx, cx
	or	ecx, ecx
	jz	short loc_fnf_stc_retn ; end of dir
	
loc_cont_search_next_file:
	; 17/05/2025
	dec	byte [FindFile_DirSectorCount]
	jz	short loc_cont_search_next_file_nc

	inc	dword [FindFile_DirSector]
	;mov	eax, [FindFile_DirSector]
	; 19/05/2025
	jmp	short loc_fnf_search

loc_cont_search_next_file_nc:
	; 17/05/2025
	xor	edx, edx
	;mov	eax, [DirBuff_Cluster]
	mov	eax, [FindFile_DirCluster]
	cmp	eax, edx ; 0
	jna	short loc_fnf_stc_retn ; end of root dir

	mov	dh, [FindFile_Drv]
	add	edx, Logical_DOSDisks

	; 19/05/2025
	;mov	cl, [edx+LD_BPB+SecPerClust]
	;mov	[FindFile_DirSectorCount], cl

	mov	cl, [edx+LD_PhyDrvNo]
	mov	esi, edx

	call	get_next_cluster
	jnc	short loc_fnf_load_next_dir_cluster
	or	eax, eax
	jz	short loc_fnf_stc_retn
	;mov	eax, 17 ;Drive not ready or read error
 	cmc	;stc
;loc_fnf_retn:
	retn

loc_fnf_load_next_dir_cluster:
	; 17/05/2025
	;mov	[FindFile_DirCluster], eax
	mov	ebx, eax
	;mov	esi, Dir_Entry_Name
	; 18/05/2025
	mov	esi, FindFile_DirEntryName
	mov	ax, [FindFile_AttributesMask]
	;xor	edx, edx
	;mov	dh, [FindFile_Drv]
	;add	edx, Logical_DOSDisks
	; edx = LDRVT address
	xor	ecx, ecx ; 0
	call	locate_current_dir_file_@
	jc	short loc_fnf_check_err_reason
	jmp	loc_fff_fnf_found

loc_fnf_check_err_reason:
	cmp	eax, 2 ; file not found
	jne	short loc_fnf_stc_retn_@
	jmp	short loc_fnf_stc_retn ; no more files !
loc_fnf_stc_retn_@:
	stc
	retn

get_and_print_longname:
	; 14/06/2025
	; 05/06/2025
	; 04/06/2025
	; 20/05/2025 (TRDOS 386 v2.0.10)
	;	-Major Modification-
	; 28/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 16/10/2016
	; 13/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 24/01/2010
	; 17/10/2009 (CMD_INTR.ASM, 'cmp_cmd_longname')
get_longname_fchar:
	cmp	byte [esi], 20h
	;ja	short loc_find_longname
	; 04/06/2025
	ja	short get_longname_option
	;jb	short loc_longname_retn
	;inc	esi
	;je	short get_longname_fchar
;loc_longname_retn:
	retn

get_longname_option:
	; 04/06/2025
	cmp	byte [esi], '"'
	jne	short loc_find_longname

	lodsb	; skip the 1st dbl quote
	mov	ecx, 128
	mov	edi, temp_name
get_longname_option_1:
	lodsb
	cmp	al, '"'
	je	short get_longname_option_2
			; the 2nd dbl quote, stop
	cmp	al, 20h
	jb	short inv_longname_err
	stosb
	loop	get_longname_option_1

inv_longname_err:
	mov	esi, Msg_Invalid_LongName
	jmp	print_msg

get_longname_option_2:
	xor	al, al
	stosb
	mov	esi, temp_name
	; 14/06/2025
	mov	ebx, [Current_Dir_FCluster]
	call	search_longname
	jnc	short get_longname_option_3

	cmp	al, ERR_FILE_NOT_FOUND ; 12
	je	short loc_longname_not_found

	; 05/06/2025
	cmp	al, ERR_INV_FILE_NAME ; 26
	je	short inv_longname_err

	jmp	short loc_fln_error

get_longname_option_3:
	; convert short name to 8.3 format
	
	; edi = short dir entry address
	mov	esi, Dir_File_Name
	; esi = dos dot (8.3) file name buffer
	xchg	esi, edi
	call	convert_to_8_3_fname
	; 05/06/2025
	mov	esi, edi
	; esi = dos (8.3) dot file name (asciiz)
	jmp	loc_print_shortname

loc_find_longname:
	call	find_longname
	jnc	short loc_print_longname

	or	al, al
	jz	short loc_longname_not_found

	; 04/06/2025
loc_fln_error:
	; 16/10/2016 (15h -> 15, 17)
	cmp	al, ERR_DRV_NOT_RDY ; 15
	;je	cd_drive_not_ready ; drive not ready
	; 28/07/2022
	jne	short loc_fln_err2
loc_fln_err1:
	jmp	cd_drive_not_ready
loc_fln_err2:
				   ; or
	cmp	al, ERR_DRV_READ ; 17 ; read error
	;je	cd_drive_not_ready
	; 04/06/2025
	; 28/07/2022
	je	short loc_fln_err1

loc_ln_file_dir_not_found:
	mov	esi, Msg_File_Directory_Not_Found
	;;call	print_msg
        ;;retn
	;jmp	print_msg
	; 28/07/2022
	jmp	short loc_lfn_err3

loc_longname_not_found:
        mov     esi, Msg_LongName_Not_Found
	;;call	print_msg
        ;;retn
	;jmp	print_msg
	; 28/07/2022
	jmp	short loc_lfn_err3

	; 20/05/2025 - TRDOS 386 v2.0.10
	; (LongName format here: ASCIIZ string)
loc_print_longname:
	;mov	esi, LongFileName ; (max. 130 bytes)
	mov	edi, TextBuffer ; (max. space: 256 bytes)
	push	edi 
	; 04/06/2025
	; 20/05/2025
	; TRDOS 386 v2.0.10 limit for FAT/FAT32 long name
	mov	ecx, 130 ; asciiz or full 130 bytes
	cmp	al, 0
	ja	short loc_print_longname_1
		; asciiz name length limit for Singlix FS
	mov	cl, 64  ; asciiz or full 64 bytes
loc_print_FS_longname: ; Singlix FS (64 byte ASCIIZ file name)
	;lodsb
	;stosb
	;or	al, al
	;jnz	short loc_print_FS_longname
	;jmp	short loc_print_longname_2

	;;;;
	; 20/02/2025
	rep	movsb
		; may be better to put a zero at the end
	sub	al, al ; 0
	stosb
	; 04/06/2025
	jmp	short loc_print_longname_2
	;;;;

loc_print_longname_1: ; MS Windows long name (UNICODE chars)
	lodsw
	; 04/06/2025
	call	ascii_from_unicode
	stosb
	or	al, al
	jz	short loc_print_longname_2
	loop	loc_print_longname_1

loc_print_longname_2:
	pop	esi
loc_print_shortname: ; 04/06/2025
	call	print_msg
  	mov	esi, nextline
loc_lfn_err3:
	;call	print_msg
	;retn
	jmp	print_msg

; burada kaldým... 20/05/2025

show_file:
	; 18/05/2025 (TRDOS 386 Kernel v2.0.10) 
	; 07/08/2022
	; 28/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 18/02/2016
	; 17/02/2016
	; 15/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 13/09/2011 (CMD_INTR.ASM, 'cmp_cmd_show')
	; 08/11/2009

loc_show_parse_path_name:
	mov	edi, FindFile_Drv
	call	parse_path_name
	;jc	loc_cmd_failed
	; 28/07/2022
	jnc	short loc_show_check_filename_exists
show_file_err1:
	jmp	loc_cmd_failed

loc_show_check_filename_exists:
	mov	esi, FindFile_Name
	cmp	byte [esi], 20h
	;jna	loc_cmd_failed
	; 28/07/2022
	jna	short show_file_err1

	; 15/02/2016 (invalid file name check)
	call	check_filename 	
	jnc	short loc_show_change_drv

	mov	esi, Msg_invalid_name_chars
	jmp	print_msg

loc_show_change_drv:
	mov	dh, [Current_Drv]
	mov	[RUN_CDRV], dh
	mov	dl, [FindFile_Drv]
	cmp	dl, dh
	je	short loc_show_change_directory
	call	change_current_drive
	;;jc	loc_file_rw_cmd_failed
	;jc	loc_run_cmd_failed
	; 28/07/2022
	jnc	short loc_show_change_directory
show_file_err2:
	jmp	loc_run_cmd_failed

loc_show_change_directory:
	cmp	byte [FindFile_Directory], 20h
	jna	short loc_findload_showfile

	inc	byte [Restore_CDIR]
	mov	esi, FindFile_Directory
	xor	ah, ah ; CD_COMMAND sign -> 0
	call	change_current_directory
	;;jc	loc_file_rw_cmd_failed
	;jc	loc_run_cmd_failed
	; 28/07/2022
	jc	short show_file_err2

;loc_show_change_prompt_dir_string:
	;call	change_prompt_dir_string

loc_findload_showfile:
	; 15/02/2016
	mov	esi, FindFile_Name
	;mov	edi, Dir_Entry_Name ; Dir Entry Format File Name
	; 18/05/2025
	mov	edi, FindFile_DirEntryName
	call	convert_file_name
	mov	esi, edi ; offset Dir_Entry_Name
	; esi = offset FindFile_DirEntryName

	sub	al, al	; Attrib AND mask = 0
	; Directory attribute : 10h
	; Volume name attribute: 8h
	mov	ah, 00011000b ; 18h (Attrib NAND, AND --> zero mask)
	;
	;xor	cx, cx
	; 28/07/2022
	xor	ecx, ecx
	call	locate_current_dir_file
	;;jc	loc_file_rw_cmd_failed
	;jc	loc_run_cmd_failed
	; 28/07/2022
	jc	short show_file_err2

loc_show_load_file:
	; EDI = Directory Entry
	mov	ax, [edi+DirEntry_FstClusHI] ; First Cluster High Word
	shl	eax, 16
	mov	ax, [edi+DirEntry_FstClusLO] ; First Cluster Low Word
	mov	[Show_Cluster], eax
	mov	eax, [edi+DirEntry_FileSize] ; File Size
	and	eax, eax ; Empty file !
	;jz	end_of_show_file
	; 28/07/2022
	jnz	short loc_show_load_file_set_size
	jmp	end_of_show_file 

loc_show_load_file_set_size: ; 28/07/2022
	mov	[Show_FileSize], eax
	xor	eax, eax
	mov	[Show_FilePointer], eax ; 0
	mov	[Show_ClusterPointer], ax ; 0
	sub	ebx, ebx
	mov	bh, [Current_Drv]
	mov	esi, Logical_DOSDisks
	add	esi, ebx
	mov	[Show_LDDDT], esi ; Logical DOS Drv Description Table addr

	cmp	byte [esi+LD_FATType], 0
	ja	short loc_show_calculate_cluster_size
	; Singlix FS
	; First Cluster Number is FDT number (in compatibility buffer)
	mov	edx, [Show_Cluster] ; Compatibility dir. buffer value (FDT)
	mov	[Show_FDT], edx
	xor	eax, eax
	mov	[Show_Cluster], eax ; Sector index  = 0
				    ; (next time it will be 1)
loc_show_calculate_cluster_size:
	mov	bx, [esi+LD_BPB+BPB_BytsPerSec] ; FAT 12-16-32 (512)
	; BX = 512 = [esi+LD_FS_BytesPerSec] ; Singlix FS
	mov	al, [esi+LD_BPB+BPB_SecPerClust] ; FAT 12-16-32 (<= 128)
	; AL = 1 = [esi+LD_FS_Reserved2] ; SectPerClust for Singlix FS
	mul	ebx

	;cmp	eax, 65536 ; non-compatible (very big) cluster size
	;ja	short end_of_show_file
	mov	[Show_ClusterSize], ax

loc_start_show_file:
	mov	esi, nextline
	call	print_msg

	mov	eax, [Show_Cluster]
	mov	byte [Show_RowCount], 23

	; 17/02/2016
	mov	esi, [Show_LDDDT]

	; 07/08/2022
loc_show_next_cluster:
	; 15/02/2016
	mov	ebx, Cluster_Buffer ; 70000h (for current TRDOS 386 version)
	; ESI = Logical DOS drv description table address
	call	read_cluster
	;;jc	loc_file_rw_cmd_failed
	;jc	loc_run_cmd_failed
	; 07/08/2022
	jnc	short loc_show_nc_rc_ok
	jmp	loc_run_cmd_failed
loc_show_nc_rc_ok:
	xor 	ebx, ebx
loc_show_next_byte:
	cmp	byte [Show_RowCount], 0
	jne	short pass_show_wait_for_key
	xor	ah, ah
	call	int16h
	cmp	al, 1Bh
	;jne	short pass_exit_show
	; 28/07/2022
	je	short end_of_show_file

;end_of_show_file:
;pass_show_file:
;	mov	esi, nextline
;	call	print_msg
;	jmp	loc_file_rw_restore_retn

pass_exit_show:
	mov	byte [Show_RowCount], 20
pass_show_wait_for_key:
	add	ebx, Cluster_Buffer
	mov	al, [ebx]
	cmp	al, 0Dh
 	;jne	loc_show_check_tab_space
	; 28/07/2022
	je	short loc_show_dec_row_count
	jmp	loc_show_check_tab_space

	; 07/08/2022
loc_show_next:
	;and	bx, bx ; 65536 -> 0
	; 28/07/2022
	and	ebx, ebx
	jnz	short loc_show_next_byte
	jmp     short loc_show_next_cluster

loc_show_dec_row_count:	; 28/07/2022
	dec	byte [Show_RowCount]
pass_show_dec_rowcount:
	mov	bl, 7 ; (light gray character color, black background)
	mov	bh, [ACTIVE_PAGE] ; [ptty]
	call	_write_tty
loc_show_check_eof:
	inc	dword [Show_FilePointer]
	mov	eax, [Show_FilePointer]
	cmp	eax, [Show_FileSize]
	jnb	short end_of_show_file
	inc	word [Show_ClusterPointer]
	movzx	ebx, word [Show_ClusterPointer]

	; 17/02/2016
	; (sector boundary -9 bits- check, 512 = 0)
        test    bx, 1FFh ; 1 to 511
	jnz	short loc_show_next_byte

	; 16/02/2016
	mov	esi, [Show_LDDDT]
	;
	cmp	byte [esi+LD_FATType], 0
	ja	short loc_show_check_fat_cluster_size

	; Singlix FS
	; 1 sector, more... (cluster size = 1 sector)
	mov	eax, [Show_Cluster]
	inc	eax
	mov	[Show_Cluster], eax

	; 07/08/2022
	jmp	short loc_show_next

	; 28/07/2022
end_of_show_file:
pass_show_file:
	mov	esi, nextline
	call	print_msg
	jmp	loc_file_rw_restore_retn
	 
loc_show_check_fat_cluster_size:
	; 17/02/2016
	cmp	bx, [Show_ClusterSize] ; cluster size in bytes
        ;jb	short loc_show_next_byte ; 28/07/2022
	; 07/08/2022
	jnb	short loc_show_file_cluster_ok
	jmp	loc_show_next_byte

loc_show_file_cluster_ok:
	mov	word [Show_ClusterPointer], 0

	mov	eax, [Show_Cluster]
	;mov	esi, [Show_LDDDT]
loc_show_get_next_cluster:
	call	get_next_cluster
	;;jc	loc_file_rw_cmd_failed
	;jc	loc_run_cmd_failed
	; 28/07/2022
	jnc	short loc_show_update_ccluster
	jmp	loc_run_cmd_failed

loc_show_check_tab_space:
	cmp	al, 09h
	;jne	short pass_show_dec_rowcount ; 28/07/2022
	; 07/08/2022
	je	short loc_show_put_tab_space
	jmp	pass_show_dec_rowcount
loc_show_put_tab_space:
	mov	bh, [ACTIVE_PAGE] ; [ptty]
	call	get_cpos
	; dl = cursor column
	and	dl, 7 ; 18/02/2016
	;shr	bh, 1 ; [ACTIVE_PAGE]
	mov	bh, [ACTIVE_PAGE]
	mov	bl, 7 ; color attribute
loc_show_put_space_chars:
	mov	al, 20h ; space
	;mov	bh, [ACTIVE_PAGE] ; [ptty]
	;mov	bl, 7 ; color attribute
	;push	dx
	push	edx ; 29/12/2017
	call	_write_tty
	pop	edx ; 29/12/2017
	;pop	dx
	; 18/02/2016
	cmp	dl, 7
	;jnb	short loc_show_check_eof ; 28/07/2022
	; 07/08/2022
	jb	short loc_show_next_tab_space
	jmp	loc_show_check_eof
loc_show_next_tab_space:
	inc	dl
	jmp	short loc_show_put_space_chars

loc_show_update_ccluster:
	mov	[Show_Cluster], eax
        jmp     loc_show_next_cluster

check_filename:
	; 28/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 10/10/2016
	; 15/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 07/08/2010 (FILE.ASM, 'proc_check_filename')
	; 10/07/2010
	; Derived from 'proc_check_filename'
	; in the old TRDOS.ASM (09/02/2005).
	;
	; INPUT ->
	;	ESI = Dot File Name Location
	; OUTPUT ->
	;	cf = 1 -> error code in AL
	;	     AL = ERR_INV_FILE_NAME (=26)
	;		  Invalid file name chars
	;	cf = 0 -> valid file name
	;
	; (EAX, ECX, EDI will be changed)

check_invalid_filename_chars:
	; 28/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 15/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 10/07/2010 (FILE.ASM, 'proc_check_invalid_filename_chars')
	; 10/02/2010
	; Derived from 'proc_check_invalid_filename_chars'
	; in the old TRDOS.ASM (09/02/2005).
	;
	; INPUT ->
	;	ESI = ASCIIZ FileName
	; OUTPUT ->
	;	cf = 1 -> invalid
	;	cf = 0 -> valid
	;
	;(EAX, ECX, EDI will be changed)

	push	esi

        mov     edi, invalid_fname_chars
	lodsb
check_filename_next_char:
	mov	ecx, sizeInvFnChars
	mov	edi, invalid_fname_chars
loc_scan_invalid_filename_char:
	scasb
	je	short loc_invalid_filename_stc
	loop	loc_scan_invalid_filename_char
	lodsb
	cmp	al, 1Fh  ; 20h and above 
	ja	short check_filename_next_char

check_filename_dot:
	mov	esi, [esp]

	mov	ah, 21h
	;mov	ecx, 8
	; 28/07/2022
	sub	ecx, ecx
	mov	cl, 8
loc_check_filename_next_char:
	lodsb
	cmp	al, 2Eh
	jne	short pass_check_fn_dot_check
loc_check_filename_ext_0:
	lodsb
	cmp	al, ah ; 21h
	jb	short loc_invalid_filename
	cmp	al, 2Eh
	jne	short loc_check_filename_ext_1

loc_invalid_filename_stc:
loc_check_fn_stc_rtn:
	stc
loc_invalid_filename:
	; 10/10/2016 (0Bh -> 26)
	mov	eax, ERR_INV_FILE_NAME ; (=26)
	; Invalid file name chars
loc_check_fn_rtn:
	pop	esi
	retn

pass_check_fn_dot_check:
	cmp	al, ah ; 21h
	jb	short loc_check_fn_clc_rtn
	loop	loc_check_filename_next_char
	lodsb
	cmp	al, ah ; 21h
	jb	short loc_check_fn_clc_rtn
	cmp	al, 2Eh
	jne	short loc_check_fn_stc_rtn
	jmp	short loc_check_filename_ext_0

loc_check_filename_ext_1:
	lodsb
	cmp	al, ah ; 21h
	jb	short loc_check_fn_clc_rtn
	cmp	al, 2Eh
	je	short loc_check_fn_stc_rtn
	lodsb
	cmp	al, ah ; 21h
	jb	short loc_check_fn_clc_rtn
	cmp	al, 2Eh
	je	short loc_check_fn_stc_rtn
	lodsb
	cmp	al, ah ; 21h
	jnb	short loc_check_fn_stc_rtn

loc_check_fn_clc_rtn:
	pop	esi
	clc
	retn

loc_print_deleted_message:
	mov	esi, Msg_Deleted
	call	print_msg

	;clc

loc_file_rw_restore_retn:
	; 15/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 28/02/2010 (CMD_INTR.ASM)
loc_file_rw_cmd_failed:
	pushf 
	call	restore_cdir_after_cmd_fail
	popf
	jc	short loc_file_rw_check_write_fault
	retn

loc_permission_denied:
	; 27/02/2016
	mov	esi, Msg_Permission_Denied
	call	print_msg
	jmp	short loc_file_rw_restore_retn

loc_file_rw_check_write_fault:
	;cmp	al, 1Dh ; Write Fault
        cmp	al, 18 ; 05/11/2016
	;jne	loc_run_cmd_failed_cmp_al
	; 28/07/2022
	je	short loc_file_rw_fault
	jmp	loc_run_cmd_failed_cmp_al

loc_file_rw_fault:
	mov	esi, Msg_Not_Ready_Write_Err
	;call	print_msg
	;retn
	jmp	print_msg

make_directory:
	; 28/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 21/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 12/03/2011 (CMD_INTR.ASM, 'cmp_cmd_mkdir')
	; 14/08/2010
	; 10/07/2010
	; 29/11/2009
	;
get_mkdir_fchar:
	; esi = directory name
	cmp	byte [esi], 20h
        ja	short loc_mkdir_parse_path_name

loc_mkdir_nodirname_retn:
	retn

loc_mkdir_parse_path_name:
	mov	edi, FindFile_Drv
        call    parse_path_name
	;jc	loc_cmd_failed
	; 28/07/2022
	jnc	short loc_mkdir_check_dirname_exists
loc_mkdir_cmd_failed:
	jmp	loc_cmd_failed

loc_mkdir_check_dirname_exists:
	mov	esi, FindFile_Name
	cmp	byte [esi], 20h
	;jna	loc_cmd_failed
	; 28/07/2022
	jna	short loc_mkdir_cmd_failed
	mov	[DelFile_FNPointer], esi
	call	check_filename
	jc	short loc_mkdir_invalid_dir_name_chars

loc_mkdir_drv:
	mov	dh, [Current_Drv]
	mov	[RUN_CDRV], dh
	
	mov	dl, [FindFile_Drv]
	cmp	dl, dh
	je	short loc_mkdir_change_directory

	call	change_current_drive
	;jc	loc_file_rw_cmd_failed
	; 28/07/2022
	jnc	short loc_mkdir_change_directory
	jmp	loc_file_rw_cmd_failed

loc_mkdir_change_directory:
	cmp	byte [FindFile_Directory], 20h
	jna	short loc_mkdir_find_directory

	inc	byte [Restore_CDIR]
	mov	esi, FindFile_Directory
	xor	ah, ah ; CD_COMMAND sign -> 0
	call	change_current_directory
	jc	short loc_mkdir_check_error_code

;loc_mkdir_change_prompt_dir_string:
	;call	change_prompt_dir_string

loc_mkdir_find_directory:
	;mov	esi, FindFile_Name
	mov	esi, [DelFile_FNPointer]
	;xor	eax, eax
	xor	ax, ax ; any name (dir, file, volume)
	call	find_first_file
	jc	short loc_mkdir_check_error_code

loc_mkdir_directory_found:
	mov	esi, Msg_Name_Exists
loc_mkdir_inv_dname_chrs_msg:
	call	print_msg
	jmp	loc_file_rw_restore_retn

loc_mkdir_invalid_dir_name_chars:
	mov	esi, Msg_invalid_name_chars
	;call	print_msg
        ;jmp	loc_file_rw_restore_retn
	; 28/07/2022
	jmp	short loc_mkdir_inv_dname_chrs_msg

loc_mkdir_check_error_code:
	cmp	al, 2
	;je	short loc_mkdir_directory_not_found
	je	short loc_mkdir_ask_for_yes_no
	stc
        jmp     loc_file_rw_cmd_failed

loc_mkdir_directory_not_found:
loc_mkdir_ask_for_yes_no:
	mov	esi, Msg_DoYouWantMkdir
	call	print_msg
	mov	esi, [DelFile_FNPointer]
	call	print_msg
	mov	esi, Msg_YesNo
	call	print_msg

	mov	byte [Y_N_nextline], 20h

loc_mkdir_ask_again:
	xor	ah, ah
	call	int16h
	cmp	al, 1Bh
	;je	short loc_do_not_make_directory
	je	short loc_mkdir_y_n_escape
	and	al, 0DFh ; y -> Y, n -> N
	cmp	al, 'Y' ; 'yes'
	je	short loc_mkdir_yes_make_directory
	cmp	al, 'N' ; 'no'
	jne	short loc_mkdir_ask_again

loc_do_not_make_directory:
loc_mkdir_yes_make_directory:
	call	y_n_answer ; 29/12/2017
	;cmp	al, 'Y' ; 'yes'
	;cmc
        ;jnc	loc_file_rw_restore_retn
	cmp	al, 'N' ; 'no'
	;je	loc_file_rw_restore_retn
	; 28/07/2022
	jne	short loc_mkdir_call_make_sub_dir
	jmp	loc_file_rw_restore_retn

loc_mkdir_call_make_sub_dir:
	mov	esi, [DelFile_FNPointer]
	mov	cl, 10h ; Directory attributes 
	call	make_sub_directory
loc_rename_file_ok: ; 06/03/2016
        ;jc	loc_file_rw_cmd_failed
	; 28/07/2022
	jnc	short move_source_file_to_dest_OK
	jmp	loc_file_rw_cmd_failed
move_source_file_to_dest_OK:
	mov	esi, Msg_OK
	call	print_msg
	jmp	loc_file_rw_restore_retn

loc_mkdir_y_n_escape:
	mov	al, 'N' ; 'no'
	jmp	short loc_do_not_make_directory

y_n_answer:
	; 29/12/2017
	mov	[Y_N_nextline], al
	;push	ax
	push	eax
	mov	esi, Y_N_nextline
	call	print_msg
	pop	eax
	;pop	ax
	retn

delete_directory:
	; 28/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 29/12/2017
	; 15/10/2016
	; 01/03/2016, 06/03/2016
	; 27/02/2016, 28/02/2016, 29/02/2016
	; 26/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 16/10/2010 (CMD_INTR.ASM, 'cmp_cmd_rmdir')
	; 05/06/2010
	;
get_fchar:
	; esi = directory name
	cmp	byte [esi], 20h
        ja	short loc_rmdir_parse_path_name

loc_rmdir_nodirname_retn:
	retn

loc_rmdir_parse_path_name:
	mov	edi, FindFile_Drv
	call	parse_path_name
	;jc	loc_cmd_failed
	; 28/07/2022
	jnc	short loc_rmdir_check_dirname_exists
lc_del_dir_failed:
	jmp	loc_cmd_failed

loc_rmdir_check_dirname_exists:
	mov	esi, FindFile_Name
	cmp	byte [esi], 20h
	;jna	loc_cmd_failed
	; 28/07/2022
	jna	short lc_del_dir_failed
	mov	[DelFile_FNPointer], esi

loc_rmdir_drv:
	mov	dh, [Current_Drv]
	mov	[RUN_CDRV], dh

	mov	dl, [FindFile_Drv]
	cmp	dl, dh
	je	short loc_rmdir_change_directory

	call	change_current_drive
	;jc	loc_file_rw_cmd_failed
	; 28/07/2022
	;jnc	short loc_rmdir_change_directory
	;jmp	loc_file_rw_cmd_failed
	jc	short loc_rmdir_chdrv_failed

loc_rmdir_change_directory:
	cmp	byte [FindFile_Directory], 20h
	jna	short loc_rmdir_find_directory

	inc	byte [Restore_CDIR]
	mov	esi, FindFile_Directory
	xor	ah, ah ; CD_COMMAND sign -> 0
	call	change_current_directory
	jc	short loc_rmdir_check_error_code

;loc_rmdir_change_prompt_dir_string:
	;call	change_prompt_dir_string

loc_rmdir_find_directory:
	;mov	esi, FindFile_Name
	mov	esi, [DelFile_FNPointer]
	mov	ax, 0810h ; Only directories
	call	find_first_file
	jnc	short loc_rmdir_ambgfn_check

loc_rmdir_check_error_code:
	cmp	al, 2
	je	short loc_rmdir_directory_not_found
	stc
loc_rmdir_chdrv_failed:
	jmp	loc_file_rw_cmd_failed

loc_rmdir_ambgfn_check:
	and	dx, dx ; Ambiguous filename chars used sign (DX>0)
	jz	short loc_rmdir_directory_found

loc_rmdir_directory_not_found:
	mov	esi, Msg_Dir_Not_Found
	call	print_msg

	jmp	loc_file_rw_restore_retn

loc_rmdir_directory_found:
	and	bl, 07h ; Attributes
	;jnz	loc_permission_denied
	; 28/07/2022
	jz	short loc_rmdir_save_lnel
	jmp	loc_permission_denied

loc_rmdir_save_lnel: ; 28/02/2016
       ;mov	bh, [LongName_EntryLength]
	mov	[DelFile_LNEL], bh ; Long name entry length (if > 0)
	; edi = Directory Entry Offset (DirBuff)
	; esi = Directory Entry (FFF Structure)
	;mov	[DelFile_DirEntryAddr], edi ; not required
	;mov	ax, [edi+20] ; First Cluster High Word
        ;shl	eax, 16
	;mov	ax, [edi+26] ; First Cluster Low Word
	; ROOT Dir First Cluster = 0
        ;cmp	eax, 2
	;jb	loc_update_direntry_1

pass_rmdir_fc_check:
	push	edi ; * (29/02/2016)

	mov	esi, Msg_DoYouWantRmDir
	call	print_msg
	mov	esi, [DelFile_FNPointer]
	call	print_msg
	mov	esi, Msg_YesNo
	call	print_msg

loc_rmdir_ask_again:
	xor	ah, ah
	call	int16h
	cmp	al, 1Bh
	;je	short loc_do_not_delete_directory
        je	short loc_rmdir_y_n_escape ; 06/03/2016
	and	al, 0DFh
	mov	[Y_N_nextline], al
	cmp	al, 'Y'
	je	short loc_rmdir_yes_delete_directory
	cmp	al, 'N'
	jne	short loc_rmdir_ask_again

loc_do_not_delete_directory:
loc_rmdir_yes_delete_directory:
	call	y_n_answer ; 29/12/2017
	pop	edi ; * (29/02/2016)
	;cmp	al, 'Y' ; 'yes'
	;cmc
        ;jnc	loc_file_rw_restore_retn
	cmp	al, 'N' ; 'no'
	;je	loc_file_rw_restore_retn
	; 28/07/2022
	;jne	short loc_delete_sub_dir
	;jmp	loc_file_rw_restore_retn
	je	short loc_rmdir_rw_restore_retn

loc_delete_sub_dir:
	; 29/12/2017
	call	delete_sub_directory
	jc	short loc_rmdir_cmd_failed

loc_rmdir_ok:
	mov	esi, Msg_OK
	call	print_msg
loc_rmdir_rw_restore_retn: ; 28/07/2022
	jmp	loc_file_rw_restore_retn

loc_rmdir_y_n_escape:
	mov	al, 'N' ; 'no'
        jmp     loc_do_not_delete_directory

loc_rmdir_cmd_failed:
	; 29/12/2017
	or	eax, eax ; EAX = 0 -> Directory not empty!
	jz	short loc_rmdir_directory_not_empty

	; EAX > 0 -> Error code in AL (or AX or EAX)

	cmp	dword [FAT_ClusterCounter], 1
	;jb	loc_file_rw_cmd_failed
	; 28/07/2022
	jnb	short loc_rmdir_failed
	jmp	loc_file_rw_cmd_failed

loc_rmdir_failed:
	stc
loc_rmdir_cmd_return:
	; 01/03/2016
	pushf
	; ESI = Logical DOS Drive Description Table address
	mov	bx, 0FF00h ; BH = FFh -> use ESI for Drive parameters
	           ; BL = 0 -> Recalculate free cluster count
	push	eax
	call	calculate_fat_freespace	
	pop	eax
	popf
	;jc	loc_file_rw_cmd_failed
	;jmp	loc_file_rw_restore_retn
	; 28/07/2022
	jnc	short loc_rmdir_rw_restore_retn
	jmp	loc_file_rw_cmd_failed

loc_rmdir_directory_not_empty:
	mov	esi, Msg_Dir_Not_Empty
	call	print_msg
	; 01/03/2016
	mov	eax, [FAT_ClusterCounter]
	or	eax, eax ; 0 ?
	;jz	loc_file_rw_restore_retn
	; 28/07/2022
	jz	short loc_rmdir_rw_restore_retn	

	; ESI = Logical DOS Drive Description Table address
	mov	bx, 0FF01h ; BH = FFh -> use ESI for Drive parameters
	           ; BL = 1 -> add free clusters
	call	calculate_fat_freespace
	or	ecx, ecx
	;jz	loc_file_rw_restore_retn ; ecx = 0 -> OK
	;; ecx > 0 -> Error (Recalculation is needed)
	;jmp	short loc_rmdir_cmd_return
	; 28/07/2022
	jnz	short loc_rmdir_cmd_return
	jmp	short loc_rmdir_rw_restore_retn
	;jmp	loc_file_rw_restore_retn


delete_sub_directory:
	; 28/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 29/12/2017 
	; (moved here from 'delete_directory' for 'sysrmdir' )

	; EDI = Directory buffer entry offset/address

loc_rmdir_delete_short_name_check_dir_empty:
	mov	ax, [edi+20] ; First Cluster High Word
        shl	eax, 16
	mov	ax, [edi+26] ; First Cluster Low Word

	;mov 	[DelFile_FCluster], eax

	;;mov	bx, [DirBuff_EntryCounter]
	;mov	bx, [FindFile_DirEntryNumber] ; 27/02/2016
	;mov	[DelFile_EntryCounter], bx

    	sub	ebx, ebx
	; 29/12/2017
	mov	[FAT_ClusterCounter], ebx ; 0 ; Reset

	mov	bh, [FindFile_Drv]
	mov	esi, Logical_DOSDisks
	add	esi, ebx

	cmp	word [edi+DirEntry_NTRes], 01A1h
	je	short loc_rmdir_check_fs_directory

	;cmp	byte [esi+LD_FATType], 1
	;jb	short loc_rmdir_get__last_cluster_0

	; 29/12/2017
	cmp	eax, 2
	jnb	short loc_rmdir_get_last_cluster_1
	; eax < 2
loc_rmdir_get_last_cluster_0:
	;mov	eax, ERR_INV_FORMAT ; invalid format!
	mov	eax, ERR_NOT_DIR ; not a valid directory!
	;stc
	retn

loc_rmdir_get_last_cluster_1:
	cmp	byte [esi+LD_FATType], 3 ; FAT32
	jne	short loc_rmdir_get_last_cluster_2

	; is it root directory ?
	cmp	eax, [esi+LD_BPB+BPB_RootClus]
	jne	short loc_rmdir_get_last_cluster_2

	; root directory can not be deleted !!
loc_rmdir_permission_denied:
	mov	eax, ERR_PERM_DENIED ; permission denied!
	stc
	retn

loc_rmdir_get_last_cluster_2:
	; 29/12/2017
	mov 	[DelFile_FCluster], eax

	;mov	dx, [DirBuff_EntryCounter]
	mov	dx, [FindFile_DirEntryNumber] ; 27/02/2016
	mov	[DelFile_EntryCounter], dx

	mov	edx, [DirBuff_Cluster]
	mov	[RmDir_ParentDirCluster], edx

	mov	[RmDir_DirEntryOffset], edi

	; 01/03/2016
	;mov	dword [FAT_ClusterCounter], 0 ; Reset

loc_rmdir_get_last_cluster_3:
	call	get_last_cluster
        ;jc	loc_rmdir_cmd_failed
	jc	short loc_delete_sub_dir_retn ; 29/12/2017

	cmp	eax, [DelFile_FCluster]
	jne	short loc_rmdir_multi_dir_clusters

	mov	byte [RmDir_MultiClusters], 0
	jmp	short pass_rmdir_multi_dir_clusters

loc_rmdir_check_fs_directory:
	; 29/12/2017
	cmp	byte [esi+LD_FSType], 0A1h
	jne	short loc_rmdir_permission_denied

loc_rmdir_delete_fs_directory:
	call	delete_fs_directory
	;jnc	loc_print_deleted_message
	jnc	short loc_delete_sub_dir_retn ; 29/12/2017

	; EAX=0 -> Directory not empty !
	; EAX>0 -> Disk r/w error or another (misc) error

	;or	eax, eax
	;jz	loc_rmdir_directory_not_empty_2
	;;stc
	;;jmp	loc_file_rw_cmd_failed

loc_delete_sub_dir_retn:
	retn

loc_rmdir_multi_dir_clusters:
	mov	byte [RmDir_MultiClusters], 1

pass_rmdir_multi_dir_clusters:
	mov 	[RmDir_DirLastCluster], eax
	mov	[RmDir_PreviousCluster], ecx

loc_rmdir_load_fat_sub_directory:
	call	load_FAT_sub_directory
	;jc	loc_rmdir_cmd_failed
	jc	short loc_delete_sub_dir_retn

loc_rmdir_find_last_dir_entry:
	push	esi
	mov	esi, Dir_File_Name
	mov	byte [esi], '*'
	mov	byte [esi+8], '*'
	xor	ebx, ebx ; Entry offset  = 0
loc_rmdir_find_last_dir_entry_next:
	;mov	ax, 0800h ; Except volume/long names
	;xor	cx, cx ; 0 = Find a valid file or dir name
	; 28/07/2022
	xor	eax, eax
	mov	ah, 8
	; eax = 0800h
	xor	ecx, ecx ; 0
	call	find_directory_entry
	jc	short loc_rmdir_empty_dir_cluster
	cmp	ebx, 1
	ja	short loc_rmdir_directory_not_empty_1
loc_rmdir_dot_entry_check:
	cmp	ch, '.' ; The first char of the dir entry
	jne	short loc_rmdir_directory_not_empty_1
	or	bl, bl
	jnz	short loc_rmdir_dotdot_entry_check
	cmp	byte [edi+1], 20h
	jmp	short pass_rmdir_dot_entry_check

loc_rmdir_dotdot_entry_check:
	cmp	word [edi+1], '. '
pass_rmdir_dot_entry_check:
	jne	short loc_rmdir_directory_not_empty_1 
	inc	bl
	jmp	short loc_rmdir_find_last_dir_entry_next 

loc_rmdir_directory_not_empty_1:
	pop	eax ; pushed esi 
	xor	eax, eax ; 0
loc_rmdir_directory_not_empty_2:
loc_delete_sub_dir_stc_retn:
	stc
	retn

loc_rmdir_empty_dir_cluster:
	pop	esi

loc_rmdir_set_prev_cluster_dir_last_cluster:
	cmp	byte [RmDir_MultiClusters], 0
	jna	short loc_rmdir_unlink_dir_last_cluster

	mov	eax, [RmDir_PreviousCluster]
	;xor	ecx, ecx
	dec	ecx ; FFFFFFFFh
	call	update_cluster
	jnc	short loc_rmdir_unlink_dir_last_cluster

	; 01/03/2016
	;cmp	eax, 1  ; eax = 0 -> end of cluster chain
	;cmc
	;jc	short loc_rmdir_cmd_failed
	;jmp	short loc_rmdir_save_fat_buffer
	; 29/12/2017 
	and	eax, eax
	jnz	short loc_delete_sub_dir_stc_retn
	jmp	short loc_rmdir_save_fat_buffer

loc_rmdir_unlink_dir_last_cluster:
	mov	eax, [RmDir_DirLastCluster]
	xor	ecx, ecx ; 0
	call	update_cluster
	jnc	short loc_rmdir_unlink_stc_retn_0Bh
	; Because of it is the last cluster
	; 'update_cluster' must return with eocc error
	or	eax, eax
	;jz	short loc_rmdir_save_fat_buffer ; eocc
	;stc
        ;jmp	short loc_rmdir_cmd_failed
	; 29/12/2017
	jnz	short loc_delete_sub_dir_stc_retn

loc_rmdir_save_fat_buffer:
	cmp	byte [FAT_BuffValidData], 2
	jne	short loc_rmdir_calculate_FAT_freespace
	call	save_fat_buffer
	;jc	short loc_rmdir_cmd_failed
	; 29/12/2017
	jc	short loc_rmdir_unlink_error_retn

	; 01/03/2016
	cmp	byte [RmDir_MultiClusters], 0
	jna	short loc_rmdir_calculate_FAT_freespace

	mov	eax, [DelFile_FCluster]
        jmp     loc_rmdir_get_last_cluster_3

loc_rmdir_unlink_stc_retn_0Bh:
	; 15/10/2016 (0Bh -> 28)
	mov	eax, ERR_INV_FORMAT ; 28 = Invalid format
loc_rmdir_unlink_stc_retn:
	stc
loc_rmdir_unlink_error_retn:
	retn

loc_rmdir_delete_short_name_invalid_data:
	;mov	eax, 29 ; Invalid data (15/10/2016)
	; 28/07/2022
	sub	eax, eax
	mov	al, 29
	;stc
        ;jmp	loc_rmdir_cmd_failed
	; 29/12/2017
	jmp	short loc_rmdir_unlink_stc_retn

loc_rmdir_calculate_FAT_freespace:
	;mov	eax, [FAT_ClusterCounter]
	; 29/12/2017
	sub	eax, eax ; 0
	xchg	eax, [FAT_ClusterCounter]
	;
	mov	bx, 0FF01h
	; BL = 1 -> Add EAX to free space count
	; BH = FFh ->
	; ESI = Logical DOS Drive Description Table address
	call	calculate_fat_freespace

	and	ecx, ecx ; ecx = 0 -> valid free sector count
	jz 	short loc_rmdir_delete_short_name_continue

loc_rmdir_recalculate_FAT_freespace:
        mov     bx, 0FF00h ; BL = 0 -> Recalculate free space
	call	calculate_fat_freespace

loc_rmdir_delete_short_name_continue:
	mov	eax, [RmDir_ParentDirCluster]
	cmp	eax, 2
	jnb	short loc_rmdir_del_short_name_load_sub_dir
	call	load_FAT_root_directory
	;jc	loc_file_rw_cmd_failed
	; 29/12/2017
	jc	short loc_rmdir_unlink_error_retn
	jmp	short loc_rmdir_del_short_name_ld_chk_fclust

loc_rmdir_del_short_name_load_sub_dir:
	call	load_FAT_sub_directory
	;jc	loc_file_rw_cmd_failed
	; 29/12/2017
	jc	short loc_rmdir_unlink_error_retn

loc_rmdir_del_short_name_ld_chk_fclust:
	movzx	edi, word [RmDir_DirEntryOffset]
	add	edi, Directory_Buffer

	mov	ax, [edi+20] ; First Cluster High Word
	shl	eax, 16
	mov	ax, [edi+26] ; First Cluster Low Word
        ; Not necessary... 
	cmp	eax, [DelFile_FCluster]
	jne	short loc_rmdir_delete_short_name_invalid_data
	;
	mov	byte [edi], 0E5h ; 'Deleted' sign
	; 27/02/2016
	; TRDOS v1 has a bug here! it does not set
	; 'DirBuff_ValidData' to 2; as result of this bug,
	; 'save_directory_buffer' would not save the change !
  	mov	byte [DirBuff_ValidData], 2 ; change sign
	;
	call	save_directory_buffer
	;jc	loc_file_rw_cmd_failed
	; 29/12/2017
	jc	short loc_rmdir_unlink_error_retn

loc_rmdir_del_long_name:
	movzx	edx, byte [DelFile_LNEL]
	or	dl, dl
	jz	short loc_rmdir_update_parent_dir_lmdt

	movzx	eax, word [DelFile_EntryCounter]
	sub	eax, edx
	; 29/12/2017
	jc	short loc_rmdir_update_parent_dir_lmdt

 	; EAX = Directory Entry Number of the long name last entry
	call	delete_longname

loc_rmdir_update_parent_dir_lmdt:
	call	update_parent_dir_lmdt
	;jc	short loc_file_rw_cmd_failed
	; 29/12/2017
	;jc	short loc_rmdir_unlink_error_retn

loc_delete_sub_directory_ok:
	; 29/12/2017
	xor	eax, eax ;  0 ;  cf = 0
	retn

delete_file:
	; 28/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 29/02/2016
	; 28/02/2016 (TRDOS 386 = TRDOS v2.0)
	; 09/08/2010 (CMD_INTR.ASM, 'cmp_cmd_del')
	; 28/02/2010

get_delfile_fchar:
	; esi = file name
	cmp	byte [esi], 20h
        ja	short loc_delfile_parse_path_name

loc_delfile_nofilename_retn:
	retn

loc_delfile_parse_path_name:
	mov	edi, FindFile_Drv
	call	parse_path_name
	;jc	loc_cmd_failed
	; 28/07/2022
	jnc	short loc_delfile_check_filename_exists
loc_delfile_failed:
	jmp	loc_cmd_failed

loc_delfile_check_filename_exists:
	mov	esi, FindFile_Name
	cmp	byte [esi], 20h
	;jna	loc_cmd_failed
	; 28/07/2022
	jna	short loc_delfile_failed
	mov	[DelFile_FNPointer], esi

loc_delfile_drv:
	mov	dl, [FindFile_Drv]
	mov	dh, [Current_Drv]
	mov	[RUN_CDRV], dh
	cmp	dl, dh
	je	short loc_delfile_change_directory

	call	change_current_drive
	;jc	loc_file_rw_cmd_failed
	; 28/07/2022
	;jnc	short loc_delfile_change_directory
	;jmp	loc_file_rw_cmd_failed
	jc	short loc_delfile_chdrv_failed

loc_delfile_change_directory:
	cmp	byte [FindFile_Directory], 20h
	jna	short loc_delfile_find

	inc	byte [Restore_CDIR]
	mov	esi, FindFile_Directory
	xor	ah, ah ; CD_COMMAND sign -> 0
	call	change_current_directory
	;jc	loc_file_rw_cmd_failed
	; 28/07/2022
	jnc	short loc_delfile_chdir_ok
loc_delfile_chdrv_failed:
loc_delfile_fff_failed:
	jmp	loc_file_rw_cmd_failed

loc_delfile_chdir_ok: ; 28/07/2022

;loc_delfile_change_prompt_dir_string:
	;call	change_prompt_dir_string

loc_delfile_find:
	;mov	esi, FindFile_Name
	mov	esi, [DelFile_FNPointer]
	mov	ax, 1800h ; Except volume label and dirs
	call	find_first_file
	;jc	loc_file_rw_cmd_failed
	; 28/07/2022
	jc	short loc_delfile_fff_failed

loc_delfile_ambgfn_check:
	and	dx, dx ; Ambiguous filename chars used sign (DX>0)
	jz	short loc_delfile_found

loc_file_not_found:
	;mov	eax, 2 ; File not found sign
	; 28/07/2022
	xor	eax, eax
	mov	al, 2
	stc
	jmp	loc_file_rw_cmd_failed

loc_delfile_found:
	and	bl, 07h ; Attributes
	;jnz	loc_permission_denied
	; 28/07/2022
	jz	short loc_delfile_attrb_ok
	jmp	loc_permission_denied

loc_delfile_attrb_ok: ; 28/07/2022

;loc_delfile_found_save_lnel:
;	mov	[DelFile_LNEL], bh ; Long name entry length (if > 0)

loc_delfile_ask_for_delete:
	push	edi ; * (29/02/2016)

	mov	esi, Msg_DoYouWantDelete
	call	print_msg
	mov	esi, [DelFile_FNPointer]
	call	print_msg
	mov	esi, Msg_YesNo
	call	print_msg

loc_delfile_ask_again:
	xor	ah, ah
	call	int16h
	cmp	al, 1Bh
	;je	short loc_do_not_delete_file
	je	short loc_delfile_y_n_escape ; 06/03/2016
	and	al, 0DFh
	mov	[Y_N_nextline], al
	cmp	al, 'Y'
	je	short loc_yes_delete_file
	cmp	al, 'N'
	jne	short loc_delfile_ask_again

loc_do_not_delete_file:
loc_yes_delete_file:
	call	y_n_answer ; 29/12/2017
	pop	edi ; * (29/02/2016)
	;cmp	al, 'Y' ; 'yes'
	;cmc
        ;jnc	loc_file_rw_restore_retn
	cmp	al, 'N' ; 'no'
        ;je	loc_file_rw_restore_retn
	; 28/07/2022
	jne	short loc_delete_file
	jmp	loc_file_rw_restore_retn

loc_delete_file:
	mov	bh, [FindFile_Drv]
	;mov	bl, [DelFile_LNEL]
	mov	bl, [FindFile_LongNameEntryLength]
	;mov	cx, [DirBuff_EntryCounter]
	mov	cx, [FindFile_DirEntryNumber]
	; (*) EDI = Directory buffer entry offset/address
	call	remove_file ; (FILE.ASM, 'proc_delete_file')
	;jnc	loc_print_deleted_message
	; 28/07/2022
	jc	short loc_delete_file_err1
	jmp	loc_print_deleted_message

loc_delete_file_err1:
	;cmp	al, 05h
	cmp	al, ERR_PERM_DENIED  ; 29/12/2017 (5 -> 11)
	;je	loc_permission_denied
	; 28/07/2022
	jne	short loc_delete_file_err2
	jmp	loc_permission_denied
loc_delete_file_err2:
	stc
	jmp	loc_file_rw_cmd_failed

loc_delfile_y_n_escape:
	mov	al, 'N' ; 'no'
	jmp	short loc_do_not_delete_file

set_file_attributes:
	; 26/09/2024 (TRDOS 386 v2.0.9)
	; 28/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 06/03/2016
	; 04/03/2016 (TRDOS 386 = TRDOS v2.0)
	; 10/07/2010 (TRDOS v1, CMD_INTR.ASM, 'cmp_cmd_attrib')
	; 23/05/2010 
	; 17/12/2000 (P2000.ASM)

	; esi = file or directory name
	;xor	ax, ax
	; 28/07/2022
	xor	eax, eax
	mov	[Attr_Chars], ax
	mov	[Attributes], al

get_attrib_fchar:
	; esi = file name
	mov	al, [esi]
	cmp	al, 20h
	jna	short loc_attr_file_nofilename_retn

loc_scan_attrib_params:
	cmp	al, '-'
	;ja	loc_attr_file_parse_path_name
	;je	short loc_attr_space
	; 28/07/2022
	jb	short loc_sfa_1
	je	short loc_attr_space
	jmp	loc_attr_file_parse_path_name

loc_sfa_1:
	; 28/07/2022
	cmp	al, '+'
	;jne	loc_cmd_failed
	je	short loc_attr_space
loc_sfa_2: 
	jmp	loc_cmd_failed
loc_attr_space:
	mov	ah, [esi+1]
 	cmp	ah, 20h
	ja	short pass_attr_space
	;jb	loc_cmd_failed
	; 28/07/2022
	jb	short loc_sfa_2
	inc	esi
	jmp	short loc_attr_space

loc_attr_file_nofilename_retn:
	retn

pass_attr_space:
	and	ah, 0DFh
	cmp	ah, 'S'
	;ja	loc_cmd_failed
	; 28/07/2022
	ja	short loc_sfa_2
	jb	short pass_attr_system
	mov	ah, 04h	; System
	jmp	short pass_attr_archive

pass_attr_system:
	cmp	ah, 'H'
	ja	short pass_attr_hidden
	jb	short pass_attr_read_only
	mov	ah, 02h	; Hidden
	jmp	short pass_attr_archive

pass_attr_hidden:
	cmp	ah, 'R'
	;ja	loc_cmd_failed
	; 28/07/2022
	ja	short loc_sfa_2
	jb	short pass_attr_read_only ; Read only
	mov	ah, 01h
	jmp	short pass_attr_archive

pass_attr_read_only:
	cmp	ah, 'A'
	jne	short loc_chk_attr_enter
	mov	ah, 20h	; Archive

pass_attr_archive:
	cmp	al, '-'
	jne	short pass_reducing_attributes
	or	[Attr_Chars], ah
	jmp	short loc_change_attributes_inc

pass_reducing_attributes:
	or	[Attr_Chars+1], ah

loc_change_attributes_inc:
	inc	esi
	mov	ah, [esi+1]
	cmp	ah, 20h
	jb	short pass_change_attr
	je	short loc_change_attributes_inc
	cmp	ah, '-'
	ja	short loc_chk_next_attr_char1
	je	short loc_chk_next_attr_char0
	cmp	ah, '+'
	jne	short loc_chk_next_attr_char1

loc_chk_next_attr_char0:
	inc	esi
	mov	ax, [esi]
	jmp	short pass_attr_space

loc_chk_next_attr_char1:
	cmp	byte [esi], '-'
	ja	short pass_attr_space
        jmp     loc_attr_file_check_fname_fchar

loc_chk_attr_enter:
	cmp	ah, 0Dh
	;jne	loc_cmd_failed
	; 28/07/202
	je	short pass_change_attr
	jmp	loc_cmd_failed

pass_change_attr:
	mov	al, [Attr_Chars]
	not	al
	and	[Attributes], al
	mov	al, [Attr_Chars+1]
	or	[Attributes], al

loc_show_attributes:
	mov	esi, nextline
	call	print_msg

loc_show_attributes_no_nextline:
	mov	dword [Attr_Chars], 'NORM'
	mov	word [Attr_Chars+4], 'AL'
	mov	esi, Attr_Chars
	mov	al, [Attributes]
	test	al, 04h
	jz	short pass_put_attr_s
	mov	word [esi], 0053h     ; S
	inc	esi

pass_put_attr_s:
	test	al, 02h
	jz	short pass_put_attr_h
	mov	word [esi], 0048h     ; H
	inc	esi

pass_put_attr_h:
	test	al, 01h
	jz	short pass_put_attr_r
	mov	word [esi], 0052h     ; R
	inc	esi

pass_put_attr_r:
	cmp	al, 20h
	jb	short pass_put_attr_a
	mov	word [esi], 0041h     ; A

pass_put_attr_a:
	mov	esi, Str_Attributes
	call	print_msg
	mov	esi, nextline
	call	print_msg
	jmp	loc_file_rw_restore_retn

loc_attr_file_check_fname_fchar:
	inc	esi
	cmp	byte [esi], 20h
	je	short loc_attr_file_check_fname_fchar
        ;jb	pass_change_attr
	; 28/07/2022
	ja	short loc_attr_file_parse_path_name
	jmp	pass_change_attr

loc_attr_file_parse_path_name:
	mov	edi, FindFile_Drv
	call	parse_path_name
	;jc	loc_cmd_failed
	; 28/07/2022
	jnc	short loc_attr_file_check_filename_exists
loc_sfa_3:
	jmp	loc_cmd_failed

loc_attr_file_check_filename_exists:
	mov	esi, FindFile_Name
	cmp	byte [esi], 20h
	;jna	loc_cmd_failed
	; 28/07/2022
	jna	short loc_sfa_3
	mov	[DelFile_FNPointer], esi

loc_attr_file_drv:
	mov	dh, [Current_Drv]
	mov	[RUN_CDRV], dh

	mov	dl, [FindFile_Drv]
	cmp	dl, dh
	je	short loc_attr_file_change_directory

	call	change_current_drive
	;jc	loc_file_rw_cmd_failed
	; 28/07/2022
	jc	short loc_sfa_4

loc_attr_file_change_directory:
        cmp     byte [FindFile_Directory], 20h
	jna	short loc_attr_file_find

	inc	byte [Restore_CDIR]

	mov	esi, FindFile_Directory
	xor	ah, ah ; CD_COMMAND sign -> 0
	call	change_current_directory
	;jc	loc_file_rw_cmd_failed
	; 28/07/2022
	jc	short loc_sfa_4

;loc_attr_file_change_prompt_dir_string:
	;call	change_prompt_dir_string

loc_attr_file_find:
	;mov	esi, FindFile_Name
	mov	esi, [DelFile_FNPointer]
	mov	ax, 0800h ; Except volume labels
	call	find_first_file
	;jc	loc_file_rw_cmd_failed
	; 28/07/2022
	jnc	short loc_attr_file_ambgfn_check
loc_sfa_4:
	jmp	loc_file_rw_cmd_failed

loc_attr_file_ambgfn_check:
	or	dx, dx ; Ambiguous filename chars used sign (DX>0)
	;	(Note: It was BX in TRDOS v1)
	;;jz	short loc_attr_file_found
	;jnz	loc_file_not_found ; 06/03/2016
	; 28/07/2022 
	jz	short loc_attr_file_found
	jmp	loc_file_not_found

	;mov	eax, 2 ; File not found sign
	;stc
	;jmp	loc_file_rw_cmd_failed

loc_attr_file_found:
	; EDI = Directory buffer entry offset/address
	; BL = File (or Directory) Attributes
	;	(Note: It was 'CL' in TRDOS v1)
	; mov	bl, [EDI+0Bh]

	cmp	word [Attr_Chars], 0
	ja	short loc_attr_file_change_attributes
	mov	[Attributes], bl
	jmp	loc_show_attributes

loc_attr_file_change_attributes:
	mov	al, [Attr_Chars]
	not	al
	and	bl, al
	mov	al, [Attr_Chars+1]
	or	bl, al

	cmp	word [edi+DirEntry_NTRes], 01A1h ; Singlix FS
	je	short loc_attr_file_fs_check

	mov	[Attributes], bl
	mov	[edi+0Bh], bl    ; Attributes (New!)

	; 04/03/2016
	; TRDOS v1 has a bug here! it does not set
	; 'DirBuff_ValidData' to 2; as result of this bug,
	; 'save_directory_buffer' would not save the new attributes !
	
	mov	byte [DirBuff_ValidData], 2

	call 	save_directory_buffer
	;jc	loc_file_rw_cmd_failed
	;jmp	short loc_print_attr_changed_message
	; 28/07/2022
	jnc	short loc_print_attr_changed_message
loc_sfa_5:
	jmp	loc_file_rw_cmd_failed 

loc_attr_file_fs_check:
	sub	eax, eax
        mov     ah, [DirBuff_DRV]
	; 26/09/2024 (BugFix)
	sub	ah, 'A'
	mov	esi, Logical_DOSDisks
        add     esi, eax
        cmp     byte [esi+LD_FSType], 0A1h
	jnc	short loc_attr_file_change_fs_file_attributes
	; 29/12/2017 (0Dh -> 29)
	mov	ax, 29 ; Invalid Data
	jmp	loc_file_rw_cmd_failed

loc_attr_file_change_fs_file_attributes:
	; BL = New MS-DOS File Attributes
	mov	al, bl ; File/Directory Attributes
	xor	ah, ah ; Attributes in MS-DOS format sign
	call	change_fs_file_attributes
	;jc	loc_file_rw_cmd_failed
	; 28/07/2022
	jc	short loc_sfa_5

	mov	[Attributes], bl

loc_print_attr_changed_message:
	mov	esi, Msg_New
	call	print_msg
	jmp	loc_show_attributes_no_nextline

rename_file:
	; 28/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 13/11/2017
	; 06/11/2016
	; 05/11/2016
	; 16/10/2016
	; 08/03/2016
	; 06/03/2016 (TRDOS 386 = TRDOS v2.0)
	; 20/11/2010 (TRDOS v1, CMD_INTR.ASM, 'cmp_cmd_rename')
	; 16/11/2010 

get_rename_source_fchar:
	; esi = file name
	cmp	byte [esi], 20h
        jna	short loc_rename_nofilename_retn

	mov	[SourceFilePath], esi

rename_scan_source_file:
	inc	esi
	cmp	byte [esi], 20h
	je	short rename_scan_destination_file_1
	;;jb	short loc_rename_nofilename_retn
	;jb	loc_cmd_failed
	;jmp	short rename_scan_source_file
	; 28/07/2022
	ja	short rename_scan_source_file
loc_rename_failed:
	jmp	loc_cmd_failed

loc_rename_nofilename_retn: ; 08/03/2016
	retn

rename_scan_destination_file_1:
	mov	byte [esi], 0

rename_scan_destination_file_2:
	inc	esi  
	cmp	byte [esi], 20h
	je	short rename_scan_destination_file_2
	;;jb	short loc_rename_nofilename_retn
	;jb	loc_cmd_failed
	; 28/07/2022
	jb	short loc_rename_failed

	mov	[DestinationFilePath], esi

rename_scan_destination_file_3:
	inc	esi  
	cmp	byte [esi], 20h
	ja	short rename_scan_destination_file_3

	mov	byte [esi], 0

loc_rename_save_current_drive:
	mov	dh, [Current_Drv]
	mov	byte [RUN_CDRV], dh

loc_rename_sf_parse_path_name:
	mov	esi, [SourceFilePath]
	mov	edi, FindFile_Drv
	call	parse_path_name
	;jc	loc_cmd_failed
	; 28/07/2022
	jc	short loc_rename_failed

loc_rename_sf_check_filename_exists:
	mov	esi, FindFile_Name
	cmp	byte [esi], 20h
	;jna	loc_cmd_failed
	; 28/07/2022
	jna	short loc_rename_failed

	;mov	[DelFile_FNPointer], esi

loc_rename_sf_drv:
	;mov	dh, [Current_Drv]
	;mov	[RUN_CDRV], dh

	mov	dl, [FindFile_Drv]
	cmp	dl, dh ; dh = [Current_Drv]
	je	short rename_sf_change_directory

	call	change_current_drive
 	;jc	loc_file_rw_cmd_failed
	; 28/07/2022
	jc	short loc_rename_fff_failed

rename_sf_change_directory:
	cmp	byte [FindFile_Directory], 20h
	jna	short rename_sf_find

	inc	byte [Restore_CDIR]
	mov	esi, FindFile_Directory
	xor	ah, ah ; CD_COMMAND sign -> 0
	call	change_current_directory
 	;jc	loc_file_rw_cmd_failed
	; 28/07/2022
	jc	short loc_rename_fff_failed

;rename_sf_change_prompt_dir_string:
	;call	change_prompt_dir_string

rename_sf_find:
	;mov	esi, [DelFile_FNPointer]
	mov	esi, FindFile_Name

	mov	ax, 0800h ; Except volume labels
	call	find_first_file
	;jc	loc_file_rw_cmd_failed
	; 28/07/2022
	jnc	short loc_rename_sf_ambgfn_check

loc_rename_fff_failed:
	jmp	loc_file_rw_cmd_failed

loc_rename_sf_ambgfn_check:
	and	dx, dx ; Ambiguous filename chars used sign (DX>0)
	;	(Note: It was BX in TRDOS v1)
	;;jz	short loc_rename_sf_found
	;jnz	loc_file_not_found
	; 28/07/2022
	jz	short loc_rename_sf_found
	jmp	loc_file_not_found

	;mov	eax, 2 ; File not found sign
	;stc
	;jmp	loc_file_rw_cmd_failed

loc_rename_sf_found:
	; EDI = Directory buffer entry offset/address
	; BL = File (or Directory) Attributes 
	;	(Note: It was 'CL' in TRDOS v1)
	; mov	bl, [EDI+0Bh]

	test	bl, 07h ; Attributes, S-H-R
	;jnz	loc_permission_denied
	; 28/07/2022
	jz	short loc_rename_attrb_ok
	jmp	loc_permission_denied

loc_rename_attrb_ok:
        mov     esi, FindFile_Drv
        mov     edi, SourceFile_Drv
	mov	ecx, 32
	rep	movsd

loc_rename_df_parse_path_name:
	mov	esi, [DestinationFilePath]
	mov	edi, FindFile_Drv
	call	parse_path_name
	jc	short loc_rename_df_cmd_failed

	;mov	dh, [RUN_CDRV]
	mov	dh, [Current_Drv]

	; 'rename' command is valid only for same dos drive and same dir!
	; ('move' command must be used if source file and destination file
	; directories are not same!)
	mov	dl, [FindFile_Drv]
	cmp	dl, dh ; are source and destination drives different ?!
	jne	short loc_rename_df_cmd_failed ; yes!

rename_df_check_dirname_exists:
	cmp	byte [FindFile_Directory], 0
	jna	short rename_df_check_filename_exists

	; different source file and destination file directories !
loc_rename_df_cmd_failed:
	mov	eax, 1 ; TRDOS 'Bad command or file name' error
	stc
	jmp	loc_file_rw_cmd_failed

rename_df_check_filename_exists:
	mov	esi, FindFile_Name
	call	check_filename
	;jc	loc_mkdir_invalid_dir_name_chars
	; 28/07/2022
	jnc	short loc_rename_file_name_ok
	jmp	loc_mkdir_invalid_dir_name_chars

loc_rename_file_name_ok:
	;mov	[DelFile_FNPointer], esi
	;cmp	byte [esi], 20h
	;ja	short loc_rename_df_find

	;mov	dh, [Current_Drv] ; dh has not been changed

rename_df_drv_check_writable:
	movzx	esi, dh
	;movzx	esi, byte [Current_Drv]
	add	esi, Logical_DOSDisks

	mov	dl, dh ; dl = [Current_Drv]
	mov	dh, [esi+LD_DiskType]

	cmp	dh, 1 ; 0 = Invalid
	jnb	short rename_df_compare_sf_df_name

	; 16/10/2016 (13h -> 30)
	mov	eax, 30 ; 'Disk write-protected' error
	mov	ebx, [DestinationFilePath]
	jmp	loc_file_rw_cmd_failed

rename_df_compare_sf_df_name:
	mov	esi, FindFile_Name
	mov	edi, SourceFile_Name
	;mov	ecx, 12
	; 28/07/2022
	sub	ecx, ecx
	mov	cl, 12
rename_df_compare_sf_df_name_next:
	lodsb
	scasb
	jne	short loc_rename_df_find
	or	al, al
	jz	short loc_rename_df_cmd_failed
	loop	rename_df_compare_sf_df_name_next

loc_rename_df_find:
	;mov	esi, [DelFile_FNPointer]
	mov	esi, FindFile_Name

	;xor	ax, ax ; Any
	; 28/07/2022
	xor	eax, eax ; 0 ; Any
	call	find_first_file
	;;jnc	short loc_rename_df_found
	;; 29/12/2017
	;jnc	loc_permission_denied
	; 28/07/2022
	jc	short loc_rename_df_check_error_code
	jmp	loc_permission_denied

loc_rename_df_check_error_code:
	;cmp	eax, 2
	cmp	al, 2 ; Not found error
	je	short rename_df_move_find_struct_to_dest
	stc
	jmp	loc_file_rw_cmd_failed

;loc_rename_df_found:
	; 05/11/2016
	; Permission denied error
	;mov	eax, ERR_PERM_DENIED ; 29/12/2017
	;stc
	;jmp	loc_permission_denied  ; 06/11/2016

rename_df_move_find_struct_to_dest:
        mov     esi, FindFile_Drv
        mov     edi, DestinationFile_Drv
	;mov	ecx, 32
	; 28/07/2022
	sub	ecx, ecx
	mov	cl, 32
	rep	movsd

loc_rename_df_process_q_sf:
	;mov	ecx, 12
	mov	cl, 12
 	mov	esi, SourceFile_Name
	mov	edi, Rename_OldName
rename_df_process_q_nml_1_sf:
	lodsb
        cmp	al, 20h
        jna	short rename_df_process_q_nml_2_sf
	stosb
	loop	rename_df_process_q_nml_1_sf

rename_df_process_q_nml_2_sf:
	mov	byte [edi], 0

loc_rename_df_process_q_df:
	;mov	ecx, 12
	mov	cl, 12
	mov	esi, DestinationFile_Name
	mov	edi, Rename_NewName
rename_df_process_q_nml_1_df:
	lodsb
	cmp	al, 20h
	jna	short loc_rename_df_process_q_nml_2_df
	stosb
	loop	rename_df_process_q_nml_1_df

loc_rename_df_process_q_nml_2_df:
	mov	byte [edi], 0

loc_rename_confirmation_question:
	mov	esi, Msg_DoYouWantRename
	call	print_msg

	mov	al, [SourceFile_DirEntry+11] ; Attributes
	and	al, 10h
	jnz	short rename_confirmation_question_dir

rename_confirmation_question_file:
	mov	esi, Rename_File
	call	print_msg 
	jmp	short rename_confirmation_question_as

rename_confirmation_question_dir:
	mov	esi, Rename_Directory
	call	print_msg

rename_confirmation_question_as:
	mov	esi, Rename_OldName
	call	print_msg
	mov	esi, Msg_File_rename_as
	call	print_msg
	mov	esi, Msg_YesNo
	call	print_msg

loc_rename_ask_again:
	xor	ah, ah
	call	int16h
	cmp	al, 1Bh
	je	short loc_do_not_rename_file
	and	al, 0DFh
	mov	[Y_N_nextline], al
	cmp	al, 'Y'
	je	short loc_yes_rename_file
	cmp	al, 'N'
	jne	short loc_rename_ask_again

loc_do_not_rename_file:
loc_yes_rename_file:
	call	y_n_answer ; 29/12/2017
	;cmp	al, 'Y' ; 'yes'
	;cmc
        ;jnc	loc_file_rw_restore_retn
	cmp	al, 'N' ; 'no'
	;je	loc_file_rw_restore_retn
	; 28/07/2022
	jne	short loc_rename_file_yes
	jmp	loc_file_rw_restore_retn

loc_rename_file_yes: ; 28/07/2022
	mov	esi, Rename_NewName
	mov	cx, [SourceFile_DirEntryNumber]
	mov	ax, [SourceFile_DirEntry+20] ; First Cluster, HW
	shl	eax, 16 ; 13/11/2017
	mov	ax, [SourceFile_DirEntry+26] ; First Cluster, LW

  	movzx	ebx, byte [SourceFile_LongNameEntryLength]
   	call	rename_directory_entry
	jmp	loc_rename_file_ok
;loc_rename_file_ok:
;	jc	loc_run_cmd_failed
;	mov	esi, Msg_OK
;	call	proc_printmsg
;	jmp	loc_file_rw_restore_retn

move_file:
	; 07/08/2022
	; 28/07/2022 (TRDOS 386 Kernel v2.0.5)
	; 11/03/2016
	; 09/03/2016
	; 08/03/2016 (TRDOS 386 = TRDOS v2.0)
	; 21/05/2011 (TRDOS v1, CMD_INTR.ASM, 'cmp_cmd_move')
	; 23/04/2011

get_move_source_fchar:
	; esi = file name
	cmp	byte [esi], 20h
        jna	short loc_move_nofilename_retn

	mov	[SourceFilePath], esi

move_scan_source_file:
	inc	esi
	cmp	byte [esi], 20h
        je      short move_scan_destination_1
	;;jb	short loc_move_nofilename_retn
	;jb	loc_cmd_failed
	;jmp	short move_scan_source_file
	; 28/07/2022
	ja	short move_scan_source_file
loc_move_failed:
	jmp	loc_cmd_failed

loc_move_nofilename_retn:
	retn

move_scan_destination_1:
	mov	byte [esi], 0

move_scan_destination_2:
	inc	esi  
	cmp	byte [esi], 20h
	je	short move_scan_destination_2
	;;jb	short loc_move_nofilename_retn
	;jb	loc_cmd_failed
	; 28/07/2022
	jb	short loc_move_failed

	mov	[DestinationFilePath], esi

move_scan_destination_3:
	inc	esi  
	cmp	byte [esi], 20h
	ja	short move_scan_destination_3
	mov	byte [esi], 0

loc_move_scan_destination_OK:
	mov	esi, [SourceFilePath]
	mov	edi, [DestinationFilePath]

	mov	al, 1  ; move procedure Phase 1
	call	move_source_file_to_destination_file
	jnc	short move_source_file_to_destination_question

loc_move_cmd_failed_1:
	or	al, al
	;jz	loc_cmd_failed
	; 28/07/2022
	jz	short loc_move_failed

	cmp	al, 11h
	je	short loc_msg_not_same_device
	;cmp	al, 05h
	;cmp	al, ERR_PERM_DENIED ; 29/12/2017
	;jne	loc_run_cmd_failed
	;jmp	loc_permission_denied
	cmp	al, ERR_PERM_DENIED
	;je	loc_permission_denied
	; 28/07/2022
	je	short loc_move_perm_denied
	jmp	loc_run_cmd_failed

	;mov	esi, Msg_Permission_denied
	;call	print_msg
	;jmp	loc_file_rw_restore_retn

loc_msg_not_same_device:
	mov	esi, msg_not_same_drv
	call	print_msg
	jmp	loc_file_rw_restore_retn

	; 28/07/2022
loc_move_perm_denied:
	jmp	loc_permission_denied

move_source_file_to_destination_question:
        mov     al, [SourceFile_Drv]
	add	al, 'A'
	mov	[msg_source_file_drv], al
        mov     al, [DestinationFile_Drv]
	add	al, 'A'
	mov	[msg_destination_file_drv], al

	push	edi ; *

	mov	esi, msg_source_file
	call	print_msg
	mov	esi, SourceFile_Directory
	cmp	byte [esi], 20h
	jna	short msftdfq_sfn
	call	print_msg
msftdfq_sfn:
	mov	esi, SourceFile_Name
	call	print_msg
	mov	esi, msg_destination_file
	call	print_msg
	mov	esi, DestinationFile_Directory
	cmp	byte [esi], 20h
	jna	short msftdfq_dfn
	call	print_msg
msftdfq_dfn:
	mov	esi, DestinationFile_Name
	call	print_msg
	mov	esi, msg_copy_nextline
	call	print_msg
	mov	esi, msg_copy_nextline
	call	print_msg

loc_move_ask_for_new_file_yes_no:
	mov	esi, Msg_DoYouWantMoveFile
	call	print_msg
	mov	esi, Msg_YesNo
	call	print_msg
loc_move_ask_for_new_file_again:
	xor	ah, ah
	call	int16h
	cmp	al, 1Bh
	;je	short loc_do_not_move_file
	je	short loc_move_y_n_escape
	and	al, 0DFh
        mov     [Y_N_nextline], al
	cmp	al, 'Y'
	je	short loc_yes_move_file
	cmp	al, 'N'
	jne	short loc_move_ask_for_new_file_again

loc_do_not_move_file:
loc_yes_move_file:
	call	y_n_answer ; 29/12/2017
	pop	edi ; *
	;cmp	al, 'Y' ; 'yes'
	;cmc
        ;jnc	loc_file_rw_restore_retn
	cmp	al, 'N' ; 'no'
        ;je	loc_file_rw_restore_retn
	; 28/07/2022
	je	short loc_move_rw_restore_retn

loc_move_yes_move_file:
	mov	al, 2 ; move procedure Phase 2
	call	move_source_file_to_destination_file
	;;jc	short loc_move_cmd_failed_2
        ;jnc	move_source_file_to_dest_OK
	; 28/07/2022
	jc	short loc_move_cmd_failed_2
	; 07/08/2022
	jmp	move_source_file_to_dest_OK

;move_source_file_to_destination_OK:
;	mov	esi, Msg_OK
;	call	print_msg
;	jmp	loc_file_rw_restore_retn

loc_move_cmd_failed_2:
	cmp	al, 27h
	;jne	loc_run_cmd_failed
	; 28/07/2022
	je	short loc_move_ids_err
	jmp	loc_run_cmd_failed

loc_move_ids_err: ; 28/07/2022
	mov	esi, msg_insufficient_disk_space
	call	print_msg

loc_move_rw_restore_retn: ; 28/07/2022
	jmp	loc_file_rw_restore_retn

loc_move_y_n_escape:
	mov	al, 'N' ; 'no'
	jmp	short loc_do_not_move_file

copy_file:
	; 31/08/2024 - TRDOS 386 v2.0.9
	; 25/07/2022 - TRDOS 386 Kernel v2.0.5
	; 15/10/2016
	; 24/03/2016
	; 21/03/2016
	; 15/03/2016 (TRDOS 386 = TRDOS v2.0)
	; 21/05/2011 (TRDOS v1, CMD_INTR.ASM, 'cmp_cmd_copy')
	; 01/08/2010

get_copy_source_fchar:
	; esi = file name
	cmp	byte [esi], 20h
        jna     short loc_copy_nofilename_retn

	mov	[SourceFilePath], esi

copy_scan_source_file:
	inc	esi  
	cmp	byte [esi], 20h
	je	short copy_scan_destination_1
	;;jb	short loc_copy_nofilename_retn
	;jb	loc_cmd_failed
	;jmp	short copy_scan_source_file
	; 25/07/2022
	jnb	short copy_scan_source_file
copy_scan_destination_0:
	jmp	loc_cmd_failed

loc_copy_nofilename_retn:
	retn

copy_scan_destination_1:
	mov	byte [esi], 0

copy_scan_destination_2:
	inc	esi  
	cmp	byte [esi], 20h
	je	short copy_scan_destination_2
	;;jb	short loc_copy_nofilename_retn
	;jb	loc_cmd_failed
	; 25/07/2022
	jb	short copy_scan_destination_0

	mov	[DestinationFilePath], esi

copy_scan_destination_3:
	inc	esi
	cmp	byte [esi], 20h
	ja	short copy_scan_destination_3
	mov	byte [esi], 0

loc_copy_save_current_drive:
	mov	dh, [Current_Drv]
	mov	[RUN_CDRV], dh

copy_source_file_to_destination_phase_1:
	mov	esi, [SourceFilePath]
	mov	edi, [DestinationFilePath]

	mov	al, 1  ; copy procedure Phase 1
	call	copy_source_file_to_destination_file
	jnc	short copy_source_file_to_destination_question

loc_copy_cmd_failed_1:
	; 18/03/2016 (restore current drive and directory)
	or	al, al
	jnz	short loc_copy_cmd_failed_2

        inc     al ; mov al, 1 ; Bad command or file name !
loc_copy_cmd_failed_3:	; 25/07/2022
	jmp	loc_run_cmd_failed

loc_copy_cmd_failed_2:
	cmp	al, 27h ; Insufficient disk space 
	je	short loc_file_write_insuff_disk_space_msg

	; 29/12/2017
	;cmp	al, 05h
	cmp	al, ERR_PERM_DENIED
	;jne	loc_run_cmd_failed
	; 25/07/2022
	jne	short loc_copy_cmd_failed_3

	jmp	loc_permission_denied

loc_file_write_insuff_disk_space_msg:
	mov	esi, msg_insufficient_disk_space
	call	print_msg
        jmp     loc_file_rw_restore_retn 

copy_source_file_to_destination_question:
	push	edi ; *

	; dh = source file attributes
	; dl > 0 -> destination file found
	and	dl, dl
	jz	short copy_source_file_to_destination_pass_owrq

loc_copy_ask_for_owr_yes_no:
	mov	esi, Msg_DoYouWantOverWriteFile
	call	print_msg
	mov	esi, DestinationFile_Name
	call	print_msg
	mov	esi, Msg_YesNo
	call	print_msg

loc_copy_ask_for_owr_again:
	xor	ah, ah
	call	int16h
	cmp	al, 1Bh
        ;je     loc_do_not_copy_file
        je      short loc_copy_y_n_escape
	and	al, 0DFh
        mov     [Y_N_nextline], al
	cmp	al, 'Y'
	;je	loc_yes_copy_file
	; 25/07/2022
	jne	short loc_copy_ask_for_owr_n
	jmp	loc_yes_copy_file

loc_copy_ask_for_owr_n: ; 25/07/2022
	cmp	al, 'N'
        ;je	loc_do_not_copy_file
	;jmp	short loc_copy_ask_for_owr_again
	; 25/07/2022
	jne	short loc_copy_ask_for_owr_again
loc_do_not_copy_file_j:
	jmp	loc_do_not_copy_file

loc_copy_y_n_escape:
	mov	al, 'N' ; 'no'
	;jmp	loc_do_not_copy_file
	; 25/07/2022
	jmp	short loc_do_not_copy_file_j

copy_source_file_to_destination_pass_owrq:
	mov     al, [SourceFile_Drv]
	add	al, 'A'
	mov	[msg_source_file_drv], al
        mov     al, [DestinationFile_Drv]
	add	al, 'A'
	mov	[msg_destination_file_drv], al

	mov	esi, msg_source_file
	call	print_msg
	mov	esi, SourceFile_Directory
	cmp	byte [esi], 20h
	jna	short csftdfq_sfn
	call	print_msg
csftdfq_sfn:
	mov	esi, SourceFile_Name
	call	print_msg
	mov	esi, msg_destination_file
	call	print_msg
	mov	esi, DestinationFile_Directory
	cmp	byte [esi], 20h
	jna	short csftdfq_dfn
	call	print_msg
csftdfq_dfn:
	mov	esi, DestinationFile_Name
	call	print_msg
	mov	esi, msg_copy_nextline
	call	print_msg
	mov	esi, msg_copy_nextline
	call	print_msg

loc_copy_ask_for_new_file_yes_no:
	mov	esi, Msg_DoYouWantCopyFile
	call	print_msg
	mov	esi, Msg_YesNo
	call	print_msg

loc_copy_ask_for_new_file_again:
	xor	ah, ah
	call	int16h
	cmp	al, 1Bh
	je	short loc_do_not_copy_file
	and	al, 0DFh
        mov     [Y_N_nextline], al
	cmp	al, 'Y'
	je	short loc_yes_copy_file
	cmp	al, 'N'
	jne	short loc_copy_ask_for_new_file_again

loc_do_not_copy_file:
loc_yes_copy_file:
	call	y_n_answer ; 29/12/2017
	pop	edi ; *
	;cmp	al, 'Y' ; 'yes'
	;cmc
        ;jnc	loc_file_rw_restore_retn
	cmp	al, 'N' ; 'no'
	;je	loc_file_rw_restore_retn
	; 25/07/2022
	jne	short copy_source_file_to_destination_pass_q
	jmp	loc_file_rw_restore_retn

copy_source_file_to_destination_pass_q:
	mov	al, 2  ; copy procedure Phase 2
	call	copy_source_file_to_destination_file
	;jc	short loc_file_write_check_disk_space_err

	; 31/08/2024
	pushf

	; 24/03/2016
	;;push	cx
	;push	ecx ; 29/12/2017

	mov	esi, msg_copy_nextline
	call	print_msg

	;pop	eax ; 29/12/2017
	;;pop	cx
	;pop	ax

	; 31/08/2024
	popf
	jnc	short copy_source_file_to_destination_OK

	; 31/08/2024
	;;or	cl, cl
	;;or	al, al
	;jz	short copy_source_file_to_destination_OK
	;
	;; 15/10/2016 (1Dh -> 18)
	;; 18/03/2016 (1Dh)
	;;cmp	cl, 18 ; write error
	;cmp	al, 18
	;jne	short copy_source_file_to_destination_not_OK
	;;
	;;mov	al, cl ; error number (write fault!)
	;stc

	jmp	loc_file_rw_cmd_failed

	; 31/08/2024
;copy_source_file_to_destination_not_OK:
	;mov	esi, Msg_read_file_error_before_EOF
	;call	print_msg
	;jmp	loc_file_rw_restore_retn

copy_source_file_to_destination_OK:
	mov	esi, Msg_OK
	call	print_msg

	jmp	loc_file_rw_restore_retn

;loc_file_write_check_disk_space_err:
	;cmp	al, 27h ; Insufficient disk space
	;je	loc_file_write_insuff_disk_space_msg
        ;jb	loc_file_rw_cmd_failed

	;call	print_misc_error_msg ; 15/03/2016
        ;jmp	loc_file_rw_restore_retn

change_fs_file_attributes:
	; 04/03/2016 ; Temporary
	; AL = File or directory attributes
	; AH = 0 -> Attributes are in MS-DOS format
	; AH > 0 -> Attributes are in SINGLIX format
	;push	ebx
	; ... do somethings here ...
	;pop	ebx
	; BL = File or directory attributes
	retn

set_get_env:
	; 25/07/2022 - TRDOS 386 Kernel v2.0.5
	; 11/04/2016 (TRDOS 386 = TRDOS v2.0)
	; 02/09/2011 (TRDOS v1, CMD_INTR.ASM, 'cmp_cmd_set')
	; 2005 - 28/08/2011 
get_setenv_fchar:
	; esi = environment variable/string
	mov	al, [esi]
	cmp	al, 20h
	ja	short loc_find_env

	mov	esi, Env_Page
loc_print_setline:
	cmp	byte [esi], 0
	jna	short loc_setenv_retn
	call	print_msg
	push	esi
	mov	esi, nextline
	call	print_msg
	pop	esi
	jmp	short loc_print_setline

loc_setenv_retn:
	retn

loc_find_env:
	cmp	al, '='
	;je	loc_cmd_failed
	; 25/07/2022
	jne	short loc_find_envr
	jmp	loc_cmd_failed

loc_find_envr:	; 25/07/2022
	push	esi
loc_repeat_env_equal_check:
	inc	esi
	cmp	byte [esi], '='
	je	short pass_env_equal_check
	cmp	byte [esi], 20h
	jnb	short loc_repeat_env_equal_check
	mov	byte [esi], 0
	pop	esi
	; 25/07/2022 (*)
loc_print_env_string:
	mov	edi, TextBuffer ; out buffer
	mov	ecx, 255 ; maximum size (limit)
	xor	al, al ; 0 -> use [ESI]
	call	get_environment_string
	jc	short loc_setenv_retn
	 ; 25/07/2022
;loc_print_env_string:
	mov	esi, TextBuffer
	call	print_msg
	mov	esi, nextline
	;call	print_msg
	;retn
	; 25/07/2022
	jmp	print_msg

pass_env_equal_check:
	inc	esi
	cmp	byte [esi], 20h
	jnb	short pass_env_equal_check
	mov	byte [esi], 0

loc_call_set_env_string:
	pop	esi
	call	set_environment_string
	jnc	short loc_setenv_retn

loc_set_cmd_failed:
	cmp	al, 08h
	;jne	loc_cmd_failed
	; 25/07/2022
	je	short loc_set_cmd_failed_spc
	jmp	loc_cmd_failed

loc_set_cmd_failed_spc:
	mov	esi, Msg_No_Set_Space
	;call	print_msg
	;retn
	; 25/07/2022
	jmp	print_msg

set_get_path:
	; 25/07/2022 - TRDOS 386 Kernel v2.0.5
	; 11/04/2016 (TRDOS 386 = TRDOS v2.0)
	; 03/09/2011 (TRDOS v1, CMD_INTR.ASM, 'cmp_cmd_path')
	; 2005
get_path_fchar:
 	; esi = path
	cmp	byte [esi], 20h
	ja	short loc_set_path

	mov	esi, Env_Page
loc_print_path:
	cmp	byte [esi], 0
	jna	short loc_path_retn

	mov	esi, Cmd_Path ; 'PATH' address
	; 25/07/2022 (*)
	jmp	short loc_print_env_string
; 25/07/2022
;	mov	edi, TextBuffer ; out buffer
;	xor	al, al  ; use [ESI]
;	mov	ecx, 255 ; maximum size (limit)
;	call	get_environment_string
;	;jc	short loc_path_retn
;	; 25/07/2022
;	jnc	short loc_print_env_string
;	retn

;	mov	esi, TextBuffer
;	call	print_msg
;	mov	esi, nextline
;	;call	print_msg
;loc_path_retn:
;	;retn
;	; 25/07/2022
;	jmp	print_msg

loc_set_path:
	push	esi
loc_set_path_find_end:
	inc	esi
	cmp	byte [esi], 20h
	jnb	short loc_set_path_find_end
	mov	byte [esi], 0
loc_set_path_header:
	pop	esi
set_path_x: ; 31/12/2017 ('syspath')
	dec	esi
	mov	byte [esi], '='
	dec	esi
	mov	byte [esi], 'H'
	dec	esi
	mov	byte [esi], 'T'
	dec	esi
	mov	byte [esi], 'A'
	dec	esi
	mov	byte [esi], 'P'

loc_path_call_set_env_string:
	call	set_environment_string
        jc	short loc_set_cmd_failed
loc_path_retn:	; 25/07/2022
	retn

get_environment_string:
	; 12/04/2016
	; 11/04/2016
	; 05/04/2016 (TRDOS 386 = TRDOS v2.0)
	; 02/09/2011 (TRDOS v1, MAINPROG.ASM)
	; 28/08/2011
	; INPUT->
	;	EDI = Output buffer
	;	CX = Buffer length (<= ENV_PAGE_SIZE)
	;
	;	AL > 0 = AL = String sequence number
	;	AL = 0 -> ESI = ASCIIZ Set word
	;		(environment variable)
	; OUTPUT ->
	;	ESI is not changed
	;	EDI is not changed
	;	EAX = String length (with zero tail)
	;	EDX = Environment variables page address
	;	CF = 1 -> Not found (EAX not valid)
	;
	; (Modified registers: EAX, EDX)

	mov	edx, Env_Page
	cmp	byte [edx], 0
	jz	short get_env_string_with_word_stc_retn

	mov	[env_var_length], cx

	push	ecx ; *
	push	esi ; **

	or	al, al
	jz	short get_env_string_with_word

get_env_string_with_seq_number:
	mov	cl, 1
	mov	ch, al
	xor	eax, eax
	mov	esi, edx ; Env_Page

get_env_string_seq_number_check:
	cmp	ch, cl
	ja	short get_env_string_seq_number_next

get_env_string_move_to_buff:
	push	edi ; ***

	sub	edx, edx

get_env_string_seq_number_repeat1:
	inc	edx
	lodsb
	stosb

	dec	word [env_var_length]
	jnz	short get_env_string_seq_number_repeat3

get_env_string_seq_number_repeat2:
	and	al, al
	jz	short get_env_string_seq_number_ok
	inc	edx
	lodsb
	jmp	short get_env_string_seq_number_repeat2

get_env_string_seq_number_repeat3:
	or	al, al
	jnz	short get_env_string_seq_number_repeat1

get_env_string_seq_number_ok:
	pop	edi ; ***
	mov	eax, edx ; Length of the environment string
			 ; (ASCIIZ, includes ZERO tail)
	mov	edx, Env_Page

get_env_string_stc_retn:
	pop	esi ; **
	pop	ecx ; *
	retn   
	
get_env_string_seq_number_next:
	lodsb
	or	al, al
	jnz	short get_env_string_seq_number_next

	cmp	esi, Env_Page + Env_Page_Size ; +512 (+4096)
	cmc
	jc	short get_env_string_stc_retn

	lodsb
	cmp	al, 1
	jb	short get_env_string_stc_retn
	inc	cl
	jmp	short get_env_string_seq_number_check

get_env_string_with_word:
	xor	ecx, ecx

get_env_string_calc_word_length:
	lodsb 
	cmp	al, 20h
	jb	short get_env_string_calc_word_length_ok
	;inc	cx
	inc	cl

	cmp	al, 'a'
	jb	short get_env_string_calc_word_length
	cmp	al, 'z'
	ja	short get_env_string_calc_word_length
	and	al, 0DFh
	mov	[esi-1], al
	jmp	short get_env_string_calc_word_length

get_env_string_calc_word_length_ok:
	or	cl, cl
	jnz	short get_env_string_calc_word_length_save

	pop	esi ; **

get_env_string_stc_retn1:
	pop	ecx ; *

get_env_string_with_word_stc_retn:
	xor	eax, eax
	stc
	retn

get_env_string_calc_word_length_save:
	xchg	ebx, [esp] ; **
	mov	esi, ebx 
		; Start of the env string (to be searched)

	push	edi ; ***
	mov	edi, edx ; Env_Page

get_env_string_compare:
	push	edi ; ****
	push	ecx ; ***** ; Variable name length

get_env_string_compare_rep:
	lodsb
	scasb
	jne	short get_env_string_compare_next1
	loop	get_env_string_compare_rep

	cmp	byte [edi], '='
	jne	short get_env_string_compare_next1

	pop	ecx ; *****
	pop	edi ; ****
	mov	esi, edi
	pop	edi ; ***
	xchg	ebx, [esp] ; **
	jmp	short get_env_string_move_to_buff

get_env_string_compare_next1:
	mov	esi, edi
	pop	ecx ; *****
	pop	edi ; ****
get_env_string_compare_next2:
	cmp	esi, Env_Page + Env_Page_Size - 1 ; +511 (+4095)
	jnb	short get_env_string_compare_not_ok
	and	al, al
	lodsb
	jnz	short get_env_string_compare_next2
	or	al, al
	jz	short get_env_string_compare_not_ok
	dec	esi ; 12/04/2016
	mov	edi, esi
	mov	esi, ebx
	jmp	short get_env_string_compare

get_env_string_compare_not_ok:
	pop	edi ; ***
	mov	esi, ebx
	pop	ebx ; **
	jmp	short get_env_string_stc_retn1

set_environment_string:
	; 25/07/2022 - TRDOS 386 Kernel v2.0.5
	; 13/04/2016
	; 12/04/2016
	; 11/04/2016
	; 06/04/2016
	; 05/04/2016 (TRDOS 386 = TRDOS v2.0)
	; 02/09/2011 (TRDOS v1, MAINPROG.ASM)
	; 29/08/2011
	; 29/08/2011
	; INPUT->
	;	ESI = ASCIIZ environment string
	; OUTPUT ->
	;	ESI is not changed
	;	CF = 1 -> Could not set,
	;	     insufficient environment space
	;
	; (EAX, EDX will be changed)
	;
	;    (EAX = Start address of the env string if > 0)
	;    (EDX = Environment string length)

	push 	esi ; *

	xor	eax, eax

set_env_chk_validation1:
	inc	ah ; variable (string) length
	lodsb
	cmp	al, '='
	je	short set_env_chk_validation2
	cmp	al, 20h
	jb	short set_env_string_stc

	; 06/04/2016
	cmp	al, 'a'
	jb	short set_env_chk_validation1
	cmp	al, 'z'
	ja	short set_env_chk_validation1
	sub	al, 'a'-'A'
	mov	[esi-1], al
	jmp	short set_env_chk_validation1

set_env_string_stc:
	pop	esi ; *
	;stc
	retn

set_env_chk_validation2:
	push	ecx ; **
	push	ebx ; ***
	push	edi ; ****

	; 12/04/2016
	;mov	ebx, [esp+12]
	; 25/07/2022
	mov	edx, [esp+12]

set_env_chk_validation2w:
	mov	edi, esi
	dec	edi

	cmp	byte [edi-1], 20h
	ja	short set_env_chk_validation2z

	push	esi
	mov	esi, edi
	dec	esi

set_env_chk_validation2x:
	dec	esi

	;cmp	esi, ebx
	cmp	esi, edx ; 25/07/2022
	jb	short set_env_chk_validation2y

	dec	edi

	mov	al, [esi]
	mov	[edi], al

	jmp	short set_env_chk_validation2x

set_env_chk_validation2y:
	pop	esi

	;;mov	byte [ebx], 20h
	; 25/07/2022
	;mov	byte [edx], 20h

	;inc 	ebx
	;mov	[esp+12], ebx
	; 25/07/2022
	inc	edx
	mov	[esp+12], edx

	dec 	ah ; 13/04/2016

	jmp	short set_env_chk_validation2w

set_env_chk_validation2z:
	;mov	edx, Env_Page
	;mov	edi, edx
	; 25/07/2022
	mov	ebx, Env_Page
	mov	edi, ebx

set_env_chk_validation3:
	lodsb
	cmp	al, 20h
	je	short set_env_chk_validation3

	pushf

	; 12/04/2016
set_env_chk_validation3n:
	cmp	al, 'a'
	jb	short set_env_chk_validation3c
	cmp	al, 'z'
	ja	short set_env_chk_validation3x
	sub	al, 'a'-'A'
	mov	[esi-1], al

set_env_chk_validation3x:
	lodsb
	jmp	short set_env_chk_validation3n

set_env_chk_validation3c:
	cmp	al, 20h
	jnb	short set_env_chk_validation3x
		
	cmp	byte [edi], 0
	ja	short set_env_chk_validation4

	popf
	jb	short set_env_string_nothing

	mov	ecx, Env_Page_Size ; 512 (4096)

	;mov	esi, ebx ; 12/04/2016
	; 25/07/2022
	mov	esi, edx

	; 25/07/2022
	mov	edx, ecx

set_env_string_copy_to_envb:
	lodsb
	cmp	al, 20h
	jb	short set_env_string_copy_to_envb_z
	stosb
	loop	set_env_string_copy_to_envb

	; 11/04/2016
	;mov	edi, edx ; Env_Page
	; 25/07/2022
	mov	edi, ebx
	; 25/07/2022
	;mov	ecx, Env_Page_Size
	mov	ecx, edx

set_env_string_copy_to_envb_z:
	; 25/07/2022
	;push	edx  ; Start address of the variable

	;;mov	edx, Env_Page_Size
	;; 25/07/2022
	sub	edx, ecx ; variable (string) length

	sub	al, al ; 0
 	rep	stosb ; clear remain bytes of the env page

	;pop	eax  ; Start address of the variable
	; 25/07/2022
	mov	eax, ebx

set_env_string_allocate_envb_retn:  ; stc or clc return
	pop	edi ; ****
	pop	ebx ; ***
	pop	ecx ; **
	pop	esi ; *
	retn

set_env_string_nothing:
	xor	eax, eax
	xor	edx, edx ; 11/04/2016
	jmp	short set_env_string_allocate_envb_retn

set_env_chk_validation4:
	; 11/04/2016
	popf

	;mov	esi, edx  ; Env_Page
	; 25/07/2022
	mov	esi, ebx

set_env_chk_validation5:
	;mov	edi, ebx ; ASCIIZ environment string address
	; 25/07/2022
	mov	edi, edx
	movzx	ecx, ah ; Variable (string) length (with '=')

set_env_chk_validation5_loop:
	lodsb
	scasb
	jne	short set_env_chk_validation6
	loop	set_env_chk_validation5_loop

	cmp	al, '='
        ;je	set_env_change_variable
	; 25/07/2022
	jne	short set_env_chk_validation6
	jmp	set_env_change_variable

set_env_chk_validation6:
	or	al, al ; 0
	jz	short set_env_chk_validation7

	lodsb
	jmp	short set_env_chk_validation6

set_env_chk_validation7:
	mov	cl, ah
	add	ecx, esi
	cmp	ecx, Env_Page + Env_Page_Size - 1
		; 511 (4095) 
		; strlen + '=' + 0
	jb	short set_env_chk_validation5

set_env_chk_validation8: ; variable not found
	movzx	esi, ah  ; variable name length (with '=')
	;add	esi, ebx ; position just after of the '='
	; 25/07/2022
	add	esi, edx

set_env_chk_validation8_loop:
	lodsb
	cmp	al, 20h
	je	short set_env_chk_validation8_loop
	jb	short set_env_string_nothing

set_env_chk_validation9:
	lodsb
	cmp	al, 20h
	jnb	short set_env_chk_validation9

	; End of ASCIIZ environment string

set_env_add_variable:
	;sub	esi, ebx ; variable+definition length
	; 25/07/2022
	sub	esi, edx

	push	esi ; *****

	;mov	esi, edx ; Environment page address
	; 25/07/2022
	mov	esi, ebx

	mov	ecx, Env_Page_Size ; 512 (4096)

set_env_add_variable_loop:
	lodsb
	and	al, al
	jz	short set_env_add_variable_chk1 ; 0
	loop	set_env_add_variable_loop

	; 11/04/2016
	mov	[esi-1], cl ; 0
	inc	ecx

set_env_add_variable_chk1:
	dec	ecx
	jz	short set_env_add_variable_nspc
	lodsb
	or 	al, al
	jz	short set_env_add_variable_chk2 ; 00
	dec	ecx
	jnz	short set_env_add_variable_loop

set_env_add_variable_nspc: ; no space on environment page
	pop	eax ; *****
	;mov	eax, 8 ; No space for new environment string
	; 25/07/2022
	sub	eax, eax
	mov	al, 8
	stc
        jmp     short set_env_string_allocate_envb_retn

set_env_add_variable_chk2:
	mov	ecx, [esp] ; *****
	dec	esi ; beginning address of the new variable
	mov	eax, esi
	add	eax, ecx ; string length (with CR)
	;add	edx, Env_Page_Size ; 512 (4096)
	; 25/07/2022
	add	ebx, Env_Page_Size
	;cmp	eax, edx 
	cmp	eax, ebx ; 25/07/2022
	ja	short set_env_add_variable_nspc
	dec	ecx ; except CR at the end
	;mov	edx, ecx ; 12/04/2016
	; 25/07/2022
	mov	ebx, ecx
	mov	edi, esi
	mov	[esp], edi ; ***** ; Start address of new variable
	;mov	esi, ebx ; ASCIIZ environment string address
	; 25/07/2022
	mov	esi, edx
	rep	movsb
	sub	al, al
	stosb
	pop	eax ; ***** ; Beginning address of new variable
        cmp     edi, Env_Page + Env_Page_Size ; 12/04/2016
	;jnb	set_env_string_allocate_envb_retn ; OK !
	; 25/07/2022
 	jnb	short set_env_add_variable_chk3
	mov	[edi], cl ; 0
	clc	; 13/04/2016
set_env_add_variable_chk3:
        jmp     set_env_string_allocate_envb_retn ; OK !

set_env_change_variable:
	; 06/04/2016
	; esi = Variable's address in environment page (after '=')
	; edi = ASCIIZ environment string address (after '=')

	; ah = variable length from start to the '='
	mov	[env_var_length], ah

	sub	cl, cl ; ecx = 0

	push	edi ; *****

	mov	edi, esi ; 11/04/2016

set_env_change_variable_calc1:
	lodsb
	or	al, al
	jz	short set_env_change_variable_calc2

	inc	ecx ; length of environment string (after the '=')

	jmp	short set_env_change_variable_calc1

set_env_change_variable_calc2:
	mov	esi, [esp] ; ASCIIZ environment string address

	sub	edx, edx

set_env_change_variable_calc3:
	lodsb
	cmp	al, 20h
	jb	short set_env_change_variable_calc4

	inc	edx ; length of ASCIIZ string (after the '=')

	jmp	short set_env_change_variable_calc3
	
set_env_change_variable_calc4:
	mov	byte [esi-1], 0  ; put ZERO instead of CR

	pop	esi ; ***** ; ASCIIZ string address (after '=')

	; EDI = Old variable's address (after '=')

	; compare the new string with the old string
	cmp	edx, ecx
	ja	short set_env_change_variable_calc5 ; longer
	;jb	set_env_change_variable_calc9 ; shorter
	; 25/07/2022
	je	short set_env_change_variable_calc22
	jmp	set_env_change_variable_calc9 

set_env_change_variable_calc22:
	;same length (simple copy)
	movzx	eax, ah
	add	edx, eax
	neg	eax
	add	eax, edi
	; EAX = Start address of the variable
	; EDX = Variable length (without ZERO at the end of variable)

	rep	movsb
	clc	; 13/04/2016
        jmp     set_env_string_allocate_envb_retn ; OK !

set_env_change_variable_calc5:
	; 11/04/2016
	push	edx ; *****
	sub	edx, ecx ; difference ; (the new string is longer)
	mov 	ebx, esi
	mov	esi, edi

set_env_change_variable_calc6:
	lodsb 
	and	al, al
	jnz	short set_env_change_variable_calc6

	cmp	esi, Env_Page + Env_Page_Size ; 512 (4096)
        jnb     set_env_add_variable_nspc

	mov	ecx, edi  ; current (old) variable's address
	mov	edi, esi  ; next variable's address

	lodsb
	or	al, al
	jz	short set_env_change_variable_calc8 ; 00

set_env_change_variable_calc7:
	lodsb
	and	al, al
	jnz	short set_env_change_variable_calc7

	cmp	esi, Env_Page + Env_Page_Size ; 512 (4096)
	;jnb	set_env_add_variable_nspc
	; 25/07/2022
	jb	short set_env_change_variable_calc24
set_env_change_variable_calc23:
	jmp	set_env_add_variable_nspc

set_env_change_variable_calc24:
	lodsb
	or	al, al
	jnz	short set_env_change_variable_calc7

set_env_change_variable_calc8:
	dec	esi ; address of the second (last) 0 of the 00

	add	edx, esi ; final position of the last 0

	cmp	edx, Env_Page + Env_Page_Size ; 512 (4096)
	;jnb     set_env_add_variable_nspc
	; 25/07/2022
	jnb	short set_env_change_variable_calc23

	mov	eax, ecx ; old variable's address (after '=')

	mov	ecx, esi 
	sub	ecx, edi ; count of bytes to move forward

	; 13/04/2016
	mov	byte [edx], 0
	mov	edi, edx
	sub	edx, esi ; difference (additional byte count)
	dec	edi ; the last zero address (first byte of the 00)
	mov	esi, edi
	sub	esi, edx ; - displacement

	cli	; disable interrupts
	std	; backward

	rep	movsb ; move ECX bytes from DS:ESI to ES:EDI

	cld	; forward (default)
	sti	; enable interrupts

	mov	edi, eax
	pop	ecx ; ***** ; byte count (after '=')
	mov	edx, ecx
	mov	esi, ebx ; ASCIIZ string address (after '=')
	mov	ebx, edi

	rep	movsb

	mov	[edi], cl ; 0 ; end of variable

	movzx	eax, byte [env_var_length]
	add	edx, eax ; variable length (total)
	neg	eax
	add	eax, ebx ; start address of the variable
	clc	; 13/04/2016
        jmp     set_env_string_allocate_envb_retn ; OK !

set_env_change_variable_calc9:
	; 11/04/2016
	and	edx, edx ; is empty ?
	jnz	short set_env_change_variable_calc15

	movzx	ebx, ah
	neg	ebx
	add	ebx, edi

	; EBX = Start address of the variable (in env page)
	; EDX = Variable length = 0

	mov	esi, edi

set_env_change_variable_calc10:
	lodsb
	or	al, al
	jnz	short set_env_change_variable_calc10

	mov	ecx, Env_Page + Env_Page_Size - 1

	cmp	esi, ecx ; +511 (+4095)
	jna	short set_env_change_variable_calc11

	mov	esi, ecx
	mov	[esi], al ; 0

set_env_change_variable_calc11:
	mov	edi, ebx ; old variable's start address

set_env_change_variable_calc12:
	lodsb
	stosb
	and	al, al
	jnz	short set_env_change_variable_calc12
	cmp	esi, ecx
	ja	short set_env_change_variable_calc13
	lodsb
	stosb
	and	al, al
	jnz	short set_env_change_variable_calc12

set_env_change_variable_calc13:
	sub	ecx, edi
	jb	short set_env_change_variable_calc14
	inc	ecx ; 1-512 (1-4096)
	rep	stosb ; al = 0

set_env_change_variable_calc14:
	sub	eax, eax ; Start address of the variable
	; EAX = 0 -> Variable is removed
	; EDX = Variable length = 0

        jmp     set_env_string_allocate_envb_retn ; OK !

set_env_change_variable_calc15:
	push	edx ; *****
	neg	edx
	add	edx, ecx ; difference (the old string is longer)
	mov 	ebx, esi
	mov	esi, edi

set_env_change_variable_calc16:
	lodsb
	and	al, al
	jnz	short set_env_change_variable_calc16

	mov	ecx, Env_Page + Env_Page_Size

	cmp	esi, ecx ; +512 (+4096)
	jna	short set_env_change_variable_calc17

	mov	esi, ecx
	mov	[esi-1], al ; 0

set_env_change_variable_calc17:
	mov	ecx, edi  ; current (old) variable's address
	mov	edi, esi  ; next variable's address

	lodsb
	or	al, al
	jz	short set_env_change_variable_calc20

set_env_change_variable_calc18:
	lodsb
	and	al, al
	jnz	short set_env_change_variable_calc18

	cmp	esi, Env_Page + Env_Page_Size
	jb	short set_env_change_variable_calc19
	je	short set_env_change_variable_calc20

	mov	esi, Env_Page + Env_Page_Size - 1
	mov	[esi], al ; 0
	jmp	short set_env_change_variable_calc21

set_env_change_variable_calc19:
	lodsb
	or	al, al
	jnz	short set_env_change_variable_calc18

set_env_change_variable_calc20:
	dec	esi ; address of the second (last) 0 of the 00

set_env_change_variable_calc21:
	; edx = difference (byte count)

	mov	eax, ecx ; old variable's address (after '=')

	mov	ecx, esi 
	sub	ecx, edi ; count of bytes to move backward

	mov	esi, edi ; next variable's address
	sub	edi, edx ; (displacement)

	rep	movsb

	mov	[edi], cl ; 0 ; 00 ; end of environment variables

	mov	edi, eax
	pop	edx ; ***** ; byte count (after '=')
	mov	ecx, edx
	mov	esi, ebx ; ASCIIZ string address (after '=')
	mov	ebx, edi

	rep	movsb

	mov	[edi], cl ; 0 ; end of variable

	movzx	eax, byte [env_var_length]
	add	edx, eax ; variable length (total)
	neg	eax
	add	eax, ebx ; start address of the variable
	clc	; 13/04/2016
        jmp     set_env_string_allocate_envb_retn ; OK !

mainprog_startup_configuration:
	; 25/07/2022 - TRDOS 386 Kernel v2.0.5
	; 22/11/2017
	; 06/05/2016
	; 14/04/2016 (TRDOS 386 = TRDOS v2.0)
	; 17/09/2011 (TRDOS v1, MAINPROG.ASM)
	;
loc_load_mainprog_cfg_file:
	mov	esi, MainProgCfgFile
	mov	ax, 1800h ; Except volume label and dirs
	call	find_first_file
	jc	short loc_load_mainprog_cfg_exit

	;or	eax, eax
	;jz	short loc_load_mainprog_cfg_exit

loc_start_mainprog_configuration:
	; ESI = FindFile_DirEntry Location
	; EAX = File Size

	mov	[MainProgCfg_FileSize], eax

	mov	dx, [esi+DirEntry_FstClusHI]
	shl	edx, 16
	mov	dx, [esi+DirEntry_FstClusLO]
	mov	[csftdf_sf_cluster], edx

	mov	ecx, eax
	sub	eax, eax

	; TRDOS 386 (TRDOS v2.0)
	; Allocate contiguous memory block for loading the file

	; eax = 0 (Allocate memory from the beginning)
	; ecx = File (Allocation) size in bytes

	call	allocate_memory_block
	jc	short loc_load_mainprog_cfg_exit

	mov	[csftdf_sf_mem_addr], eax ; loading address
	mov	[csftdf_sf_mem_bsize], ecx ; block size

	xor	ebx, ebx
	;mov	[csftdf_sf_rbytes], ebx ; 0, reset

	mov	bh, [Current_Drv] ; [FindFile_Drv]
	mov	esi, Logical_DOSDisks
	add	esi, ebx

	mov	ebx, [csftdf_sf_mem_addr] ; memory block address

	cmp	byte [esi+LD_FATType], 0
        ja	short loc_mcfg_load_fat_file

	mov	dword [csftdf_r_size], 65536
        jmp     loc_mcfg_load_fs_file

loc_load_mainprog_cfg_exit:
	retn

loc_mcfg_load_fat_file:
	movzx	eax, word [esi+LD_BPB+BytesPerSec]
	movzx	ecx, byte [esi+LD_BPB+SecPerClust]
	mul	ecx
	mov	[csftdf_r_size], eax

loc_mcfg_load_fat_file_next:
	call	mcfg_read_fat_file_sectors
        jc	short mcfg_deallocate_mem ; 25/07/2022

	or	edx, edx ; edx > 0 -> EOF
	jz	short loc_mcfg_load_fat_file_next

loc_mcfg_load_fat_file_ok:
	; 06/05/2016
	mov	dword [mainprog_return_addr], loc_mcfg_ci_return_addr
	;
	mov	esi, [csftdf_sf_mem_addr]
	mov	[MainProgCfg_LineOffset], esi

	mov	eax, [MainProgCfg_FileSize]
	mov	edx, eax
	add	edx, esi

loc_mcfg_process_next_line_check:
	mov	ecx, eax

	cmp	byte [esi], "*" ; Remark sign
	jne	short loc_mcfg_process_next_line
	inc	esi
	jmp	short loc_move_mainprog_cfg_nl1

loc_mcfg_process_next_line:
	cmp	ecx, 79
	jna	short loc_start_mainprog_cfg_process

	;mov	ecx, 79
	; 25/07/2022
	sub	ecx, ecx
	mov	cl, 79

loc_start_mainprog_cfg_process:
	mov	edi, CommandBuffer

loc_move_mainprog_cfg_line:
	lodsb
	cmp	al, 20h
	jb	short loc_move_mainprog_cfg_nl2
	stosb
	loop	loc_move_mainprog_cfg_line

loc_move_mainprog_cfg_nl1:
	cmp	esi, edx ; + configuration file size
	jnb	short loc_end_of_mainprog_cfg_line
	lodsb
	cmp	al, 20h
	jnb	short loc_move_mainprog_cfg_nl1

loc_move_mainprog_cfg_nl2:
	cmp	esi, edx
	jnb	short loc_end_of_mainprog_cfg_line
	mov	al, [esi]
	cmp	al, 20h
 	ja	short loc_end_of_mainprog_cfg_line
	inc	esi
	jmp	short loc_move_mainprog_cfg_nl2	

	; 25/07/2022
mcfg_deallocate_mem:
	mov	eax, [csftdf_sf_mem_addr] ; start address
	mov	ecx, [csftdf_sf_mem_bsize] ; block size
	;call	deallocate_memory_block
	;retn
	jmp	deallocate_memory_block

loc_end_of_mainprog_cfg_line:
	mov	byte [edi], 0

	mov	[MainProgCfg_LineOffset], esi

	; 22/11/2017
	mov	esi, CommandBuffer + 8
	sub	esi, edi
	jna	short loc_move_mainprog_cfg_command
	xor	al, al
loc_mainprog_cfg_clear_chrs:
	stosb
	dec	esi
	jnz	short loc_mainprog_cfg_clear_chrs

loc_move_mainprog_cfg_command:
	mov	esi, CommandBuffer
	mov	edi, esi
	xor	ebx, ebx
	;xor	ecx, ecx
	xor	cl, cl

loc_move_mcfg_first_cmd_char:
	mov	al, [esi+ebx]
	inc	bl
	cmp	al, 20h
	ja	short loc_move_mcfg_cmd_capitalizing
	jb	short loc_move_mcfg_cmd_arguments_ok
	cmp	bl, 79
	jb	short loc_move_mcfg_first_cmd_char
	jmp	short loc_move_mcfg_cmd_arguments_ok

loc_move_mcfg_next_cmd_char:
	mov	al, [esi+ebx]
	inc	bl
	cmp	al, 20h
	jna	short loc_move_mcfg_cmd_ok

loc_move_mcfg_cmd_capitalizing:
	cmp	al, 61h ; 'a'
	jb	short loc_move_mcfg_cmd_caps_ok
	cmp	al, 7Ah ; 'z'
	ja	short loc_move_mcfg_cmd_caps_ok
	and	al, 0DFh ; sub	al, 'a'-'A'

loc_move_mcfg_cmd_caps_ok:
	stosb
	inc	cl
	cmp	bl, 79
	jb	short loc_move_mcfg_next_cmd_char
	jmp	short loc_move_mcfg_cmd_arguments_ok

loc_move_mcfg_cmd_ok:
	xor	al, al ; 0

loc_move_mcfg_cmd_arguments:
	mov	[edi], al
	inc	edi
	cmp	bl, 79
	jnb	short loc_move_mcfg_cmd_arguments_ok
	mov	al, [esi+ebx]
	inc	bl
	cmp	al, 20h
	jnb	short loc_move_mcfg_cmd_arguments

loc_move_mcfg_cmd_arguments_ok:
	mov	byte [edi], 0

loc_mcfg_process_cmd_interpreter:
	call    command_interpreter

loc_mcfg_ci_return_addr:
	mov	eax, [MainProgCfg_FileSize]
	mov	edx, eax
	mov	esi, [MainProgCfg_LineOffset]
	add	edx, esi
	add	eax, [csftdf_sf_mem_addr]
	sub	eax, esi
        ja      loc_mcfg_process_next_line_check

	call	mcfg_deallocate_mem

 	mov	ecx, 79 ; 80 ?
	mov	edi, CommandBuffer
	xor	al, al
	rep	stosb

	; 06/05/2016
	mov	esi, nextline
	call	print_msg
	jmp	dos_prompt

mcfg_read_file_sectors:
	; 14/04/2016
	cmp	byte [esi+LD_FATType], 0
        jna	short mcfg_read_fs_file_sectors

mcfg_read_fat_file_sectors:
	; return:
	;   CF = 0 & EDX > 0 -> END OF FILE
	;   CF = 0 & EDX = 0 -> not EOF
	;   CF = 1 -> read error (error code in AL)

mcfg_read_fat_file_secs_0:
	mov	edx, [MainProgCfg_FileSize]
	sub	edx, [csftdf_sf_rbytes]
	cmp	edx, [csftdf_r_size]
	jnb	short mcfg_read_fat_file_secs_1
	mov	[csftdf_r_size], edx

mcfg_read_fat_file_secs_1:
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
	jc	short mcfg_read_fat_file_secs_3

	; EBX = next memory address

	mov	eax, [csftdf_sf_rbytes]
	add	eax, [csftdf_r_size]
	mov	edx, [MainProgCfg_FileSize]
	cmp	eax, edx
	jnb	short mcfg_read_fat_file_secs_3 ; edx > 0
	mov	[csftdf_sf_rbytes], eax

	push	ebx ; *
	; get next cluster (csftdf_r_size! bytes)
	mov	eax, [csftdf_sf_cluster]
	call	get_next_cluster
	pop	ebx ; *
	jnc	short mcfg_read_fat_file_secs_2

	;mov	eax, 17; Read error !
	retn

mcfg_read_fat_file_secs_2:
	sub	edx, edx ; 0
	mov	[csftdf_sf_cluster], eax ; next cluster

; 25/07/2022 - TRDOS 386 Kernel v2.0.5

mcfg_read_fat_file_secs_3:
	;retn

mcfg_read_fs_file_sectors:
	;retn

loc_mcfg_load_fs_file:
	retn

load_and_execute_file:
	; 03/09/2024 - TRDOS 386 Kernel v2.0.9
	; 30/08/2023 - TRDOS 386 Kernel v2.0.6
	; 25/07/2022 - TRDOS 386 Kernel v2.0.5
	; 04/01/2017
	; 06/05/2016 - 07/05/2016 - 11/05/2016
	; 23/04/2016 - 24/04/2016
	; 22/04/2016 (TRDOS 386 = TRDOS v2.0)
	; 05/11/2011 
	; (TRDOS v1, CMDINTR.ASM, 'cmp_cmd_run', 'cmp_cmd_external')
	; ('loc_run_check_filename')
	; 29/08/2011
	; 10/09/2011
	; INPUT->
	;	ESI = Path Name address (CommandBuffer address)
	; OUTPUT ->
	;	none (error message will be shown if an error will occur)
	;
	; (EAX, EBX, ECX, EDX, ESI, EDI, EBP will be changed) 
	;
loc_run_check_filename:
	cmp	byte [esi], 20h
	;jb	loc_cmd_failed
	; 25/07/2022
	jb	short loc_run_ppn_failed
	ja	short loc_run_check_filename_ok
	inc	esi
	jmp	short loc_run_check_filename

loc_run_check_filename_ok:
	mov	byte [CmdArgStart], 0 ; reset
	push	esi ; *
loc_run_get_first_arg_pos:
	inc	esi
	mov	al, [esi]
	cmp	al, 20h
	ja	short loc_run_get_first_arg_pos
	mov	byte [esi], 0
loc_run_get_external_arg_pos:
	; 11/05/2016
	inc	esi
	mov	al, [esi]
	cmp	al, 20h
	jna	short loc_run_parse_path_name
	mov	eax, esi
	sub	eax, CommandBuffer
	mov	byte [CmdArgStart], al
loc_run_parse_path_name:
	pop	esi ; *
	mov	edi, FindFile_Drv
	call	parse_path_name
	;jc	loc_cmd_failed
	; 25/07/2022
	jnc	short loc_run_check_filename_exists
loc_run_ppn_failed:
	jmp	loc_cmd_failed

loc_run_check_filename_exists:
	mov	esi, FindFile_Name
	cmp	byte [esi], 20h
	;jna	loc_cmd_failed
	; 25/07/2022
	jna	short loc_run_ppn_failed

loc_run_check_exe_filename_ext:
	call	check_prg_filename_ext
	;jc	loc_cmd_failed
	; 25/07/2022
	jc	short loc_run_ppn_failed

loc_run_check_exe_filename_ext_ok:
	mov	word [EXE_ID], ax

loc_run_drv:
	mov	byte [Run_Manual_Path], 0
	mov	eax, [Current_Dir_FCluster]
        mov     [Run_CDirFC], eax
	;
	mov	dh, [Current_Drv]
	mov	[RUN_CDRV], dh

	mov	dl, [FindFile_Drv]
	cmp	dl, dh
	je	short loc_run_change_directory

	add	byte [Run_Manual_Path], 2

	call	change_current_drive
	;jc	loc_run_cmd_failed
	; 25/07/2022
	jnc	short loc_run_change_directory
	jmp	loc_run_cmd_failed

loc_run_change_directory:
	cmp	byte [FindFile_Directory], 20h
	jna	short loc_run_find_executable_file

	inc	byte [Run_Manual_Path]

	inc	byte [Restore_CDIR]

	mov	esi, FindFile_Directory
	xor	ah, ah ; CD_COMMAND sign -> 0
	call	change_current_directory
	;jc	loc_run_cmd_failed
	; 25/07/2022
	jnc	short loc_run_change_prompt_dir_string
	jmp	loc_run_cmd_failed

loc_run_change_prompt_dir_string:
	call	change_prompt_dir_string

loc_run_find_executable_file:
	mov	word [Run_Auto_Path], 0

loc_run_find_executable_file_next:
	mov	esi, FindFile_Name
loc_run_find_program_file_next:
	mov	ax, 1800h ; Except volume label and dirs
	call	find_first_file
	; ESI = Directory Entry (FindFile_DirEntry) Location
	; EDI = Directory Buffer Directory Entry Location
	; EAX = File size
	;jnc	loc_load_and_run_file
	; 25/07/2022
	jc	short loc_run_program_file_not_found
	jmp	loc_load_and_run_file

loc_run_program_file_not_found:	 
	cmp	al, 2 ; file not found
	;jne	loc_run_cmd_failed
	; 25/07/2022
	je	short loc_run_progr_file_chk_prg_ext
	jmp	loc_run_cmd_failed

loc_run_progr_file_chk_prg_ext: ; 25/07/2022
	mov	ax, word [EXE_ID]
	cmp	ah, '.' ; File name has extension sign
	je	short loc_run_check_auto_path

	or	al, al
	jnz	short loc_run_check_auto_path

	cmp	ah, 8 ; count of file name chars
	ja	short loc_run_check_auto_path

loc_run_change_file_ext_to_prg:
	movzx	ebx, ah ; count of file name chars
	mov	esi, FindFile_Name
	add	ebx, esi
	; 07/05/2016
	mov	dword [ebx],  '.PRG'
	mov	word [EXE_ID], 'P.'
	jmp	short loc_run_find_program_file_next

loc_run_check_auto_path:
	; NOTE: /// 07/05/2016 ///
	; If the path is given, value of byte [Run_Manual_Path]
	; will not be ZERO. If so, file searching by using
	; Automatic Path (via 'PATH' environment variable)
	; will not be applicable, because the program file
	; is already/absolutely not found.

	mov	al, [Run_Manual_Path]
	or	al, al
	;jnz	loc_cmd_failed
	; 25/07/2022
	jnz	short loc_run_cap_failed

loc_run_check_auto_path_again:
	cmp	word [Run_Auto_Path], 0FFFFh
		; 0FFFFh = Not a valid run path (in ENV block) 
	;jnb	loc_cmd_failed
	; 25/07/2022
	jnb	short loc_run_cap_failed
	;xor	al, al
	mov	esi, Cmd_Path ; 'PATH'
	mov	edi, TextBuffer
	; 03/09/2024 (bugfix)
	mov	cx, 256 ; TextBuffer (maximum) length
	call	get_environment_string
	jnc	short loc_run_chk_filename_ext_again
	mov	word [Run_Auto_Path], 0FFFFh ; invalid
loc_run_cap_failed: ; 25/07/2022
	;jmp	loc_cmd_failed
	; 30/08/2023
	mov	al, 1 ; (force jump to loc_cmd_failed at the end)
	jmp	loc_run_cmd_failed ; (restore cdir if it is needed)

loc_run_chk_filename_ext_again:
	mov	ecx, eax ; string length (with zero tail)
	dec	ecx ; without zero tail
	mov	ax, [EXE_ID]
	cmp	ah, '.'
	je	short loc_run_chk_auto_path_pos

loc_run_change_file_ext_to_noext_again:
	movzx	ebx, ah
	mov	esi, FindFile_Name
	add 	ebx, esi
	sub	eax, eax
	mov	[ebx], eax ; 0 ; erase extension (.PRG)

loc_run_chk_auto_path_pos:
	;movzx	eax, word [Run_Auto_Path]
	mov	ax, [Run_Auto_Path]
	cmp	eax, ecx ; ecx = string length (except zero tail)
	;jnb	loc_cmd_failed
 	; 25/07/2022
	jnb	short loc_run_cap_failed

	;or	eax, eax
	or	ax, ax
	jnz	short loc_run_auto_path_pos_move
	mov	al, 5

loc_run_auto_path_pos_move:
	mov	esi, edi ; offset TextBuffer
	add	esi, eax

loc_run_auto_path_pos_space_loop:
	lodsb
	cmp	al, 20h
	je	short loc_run_auto_path_pos_space_loop
	;jb	loc_cmd_failed
 	; 25/07/2022
	jb	short loc_run_cap_failed
	;
loc_run_auto_path_pos_move_next_@:
	stosb
loc_run_auto_path_pos_move_next:
	lodsb
	cmp	al, ';'
	je	short loc_run_auto_path_pos_move_last_byte
	cmp	al, 20h
	je	short loc_run_auto_path_pos_move_next
	;jb	short loc_byte_ptr_end_of_path
	;stosb
	;jmp	short loc_run_auto_path_pos_move_next
	; 03/09/2024
	jnb	short loc_run_auto_path_pos_move_next_@

loc_byte_ptr_end_of_path: 
	mov	word [Run_Auto_Path], 0FFFFh ; end of path
	jmp	short loc_run_auto_path_move_ok

loc_run_auto_path_pos_move_last_byte:
	mov	eax, esi
	sub	eax, TextBuffer
	mov	[Run_Auto_Path], ax ; next path position

loc_run_auto_path_move_ok:
	dec	edi
	mov	al, '/'
	cmp	[edi], al
	je	short loc_run_auto_path_move_file_name
	inc	edi
	mov	[edi], al

loc_run_auto_path_move_file_name:
	inc	edi   
	mov	esi, FindFile_Name

loc_run_auto_path_move_fn_loop:
	lodsb
	stosb
	or	al, al
	jnz	short loc_run_auto_path_move_fn_loop

	mov	esi, TextBuffer
	mov	edi, FindFile_Drv
	call	parse_path_name
	;jc	loc_run_cmd_failed
	; 25/07/2022
	;jnc	short loc_run_change_current_drive
	;jmp	loc_run_cmd_failed
	jc	short loc_run_path_failed

loc_run_change_current_drive:
	mov	dh, [Current_Drv]
	mov	dl, [FindFile_Drv]
	cmp	dl, dh
	je	short loc_run_change_directory_again

	call	change_current_drive
	;jc	loc_run_cmd_failed
	; 25/07/2022
	;jnc	short loc_run_change_directory_again
	;jmp	loc_run_cmd_failed
	jc	short loc_run_path_failed

loc_run_change_directory_again:
	cmp	byte [FindFile_Directory], 20h
	jna	short loc_load_executable_cdir_chk_again

	inc	byte [Restore_CDIR]
	mov	esi, FindFile_Directory
	xor	ah, ah ; CD_COMMAND sign -> 0
	call	change_current_directory
	;jc	loc_run_cmd_failed
	; 25/07/2022
	jnc	short loc_run_chg_prompt_dir_str_again

loc_run_path_failed: ; 25/07/2022
	jmp	loc_run_cmd_failed

loc_run_chg_prompt_dir_str_again:
	call	change_prompt_dir_string

loc_load_executable_cdir_chk_again:
	mov	eax, [Current_Dir_FCluster]
	cmp	eax, [Run_CDirFC]
	;jne	loc_run_find_executable_file_next
	; 30/08/2023
	je	short jmp_loc_run_check_auto_path_again
	jmp	loc_run_find_executable_file_next
jmp_loc_run_check_auto_path_again:
	xor	al, al ; 0
	jmp	loc_run_check_auto_path_again

loc_load_and_run_file:
	; 25/07/2022 - TRDOS 386 Kernel v2.0.5
	; 13/11/2017
	; 04/01/2017
	; 23/04/2016
	mov	esi, FindFile_Name
	mov	edi, TextBuffer

 	; 24/04/2016
	xor	edx, edx
	;mov	word [argc], dx ; 0
	; 25/07/2022
	mov	dword [argc], edx
	mov	dword [u.nread], edx ; 0

loc_load_and_run_file_1:
	lodsb
	stosb
	inc	dword [u.nread]
	and	al, al
	jnz 	short loc_load_and_run_file_1

	mov	al, [CmdArgStart]
	and	al, al
	jz	short loc_load_and_run_file_7

	movzx	esi, al ; 11/05/2016
	;mov	ecx, 80
	; 25/07/2022
	xor	ecx, ecx
	mov	cl, 80
	;sub	ecx, esi
	sub	cl, al
	add	esi, CommandBuffer

	;inc	word [argc] ; 11/05/2016
	; 25/07/2022
	inc	dword [argc]

loc_load_and_run_file_2:
	lodsb
	cmp	al, 20h
	ja	short loc_load_and_run_file_5
	jb	short loc_load_and_run_file_6

loc_load_and_run_file_3:
	cmp	byte [esi], 20h
	ja	short loc_load_and_run_file_4
	jb	short loc_load_and_run_file_6
	inc	esi
	loop	loc_load_and_run_file_3
	jmp	short loc_load_and_run_file_6

loc_load_and_run_file_4:
	sub	al, al ; 0
	;inc	word [argc]
	; 25/07/2022
	inc	dword [argc]
loc_load_and_run_file_5:
	stosb
	inc	dword [u.nread]
	loop	loc_load_and_run_file_2
			
loc_load_and_run_file_6:
	xor	al, al ; 0
	stosb
	inc	dword [u.nread]
loc_load_and_run_file_7:
	mov 	[edi], al ; 0
	;inc	word [argc] ; 24/04/2016
	; 25/07/2022
	inc	dword [argc]
	inc	dword [u.nread] ; 24/04/2016
	mov	esi, TextBuffer
	mov	edx, [FindFile_DirEntry+DirEntry_FileSize]
	mov	ax, [FindFile_DirEntry+DirEntry_FstClusHI]
	shl	eax, 16 ; 13/11/2017
	mov	ax, [FindFile_DirEntry+DirEntry_FstClusLO]
	; EAX = First Cluster number
	; EDX = File Size
	; ESI = Argument list address
	; [argc] = argument count
	; [u.nread] = argument list length
	call	load_and_run_file ; trdosk6.s
        ;jc	loc_run_cmd_failed ; 04/01/2017
loc_load_and_run_file_8: ; 06/05/2016
	jmp	loc_file_rw_restore_retn

check_prg_filename_ext:
	; 23/04/2016 (TRDOS 386 = TRDOS v2.0)
	; 10/09/2011 
	; (TRDOS v1, CMDINTR.ASM, 'proc_check_exe_filename_ext')
	; 14/11/2009
	; INPUT ->
	;	ESI = Dot File Name
	; OUTPUT ->
	;     cf = 0 -> EXE_ID in AL
	;	ESI = Last char + 1 position
	;     cf = 1 -> Invalid executable file name
	;	or no file name extension if AH<=8
	;	AL = Last file name char     
	;     cf = 0 -> AL='P' (PRG), AL=0 (no extension)
	;
	; (Modified registers: EAX, ESI)

	xor	ah, ah
loc_run_check_filename_ext:
	lodsb
	cmp	al, 21h
	jb	short loc_check_exe_fn_retn 
	inc	ah
	cmp	al, '.'
	jne	short loc_run_check_filename_ext

loc_run_check_filename_ext_dot:
	cmp	ah, 2  ; .??? is not valid
	mov	ah, al ; '.' 
	jb	short loc_check_prg_fn_retn

loc_run_check_filename_ext_dot_ok:
	lodsb
	and	al, 0DFh

loc_run_check_filename_ext_prg:
	cmp	al, 'P'
	jb	short loc_check_prg_fn_retn
	ja	short loc_check_prg_fn_stc
	lodsb
	and	al, 0DFh
	cmp	al, 'R'
	jne	short loc_check_prg_fn_stc
	lodsb
	and	al, 0DFh
	cmp	al, 'G'
	jne	short loc_check_prg_fn_stc

	mov	al, 'P'
loc_check_prg_fn_retn:
	retn

; 25/07/2022 - TRDOS 386 Kernel v2.0.5

loc_check_prg_fn_stc:
	stc
	retn

loc_check_exe_fn_retn:
	sub	al, al ; 0
	;retn

find_and_list_files:
	;retn
set_exec_arguments:
	retn

delete_fs_directory:
	xor	eax, eax
	retn