; ****************************************************************************
; BOOTFILE.ASM (BOOTFILE.COM) 
; TRDOS 386, Singlix FS1 (A1h) File System STARTUP FILE Configuration Code
; 						      (for MSDOS/WINDOWS)
; ****************************************************************************
; Last Update: 28/01/2018
; ----------------------------------------------------------------------------
; Beginning: 27/01/2018
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.11
; ----------------------------------------------------------------------------
; Modified from 'BOOTFILE.ASM' (TRDOS v1.0) source code by Erdogan Tan
; (21/02/2010) 
; ****************************************************************************
; nasm bootfile.s -l bootfile.lst -o BOOTFILE.COM

; DTA (PSP+80h= Offset 128)
DTA_Attrib	equ 149 ; PDP+21
DTA_Time	equ 150 ; PSP+22
DTA_Date	equ 152 ; PSP 24
DTA_FileSize	equ 154 ; PSP + 26
DTA_FileName	equ 158 ; PSP + 30

; Masterboot / Partition Table at Beginning+1BEh
ptBootable      equ 0
ptBeginHead     equ 1
ptBeginSector   equ 2
ptBeginCylinder equ 3
ptFileSystemID	equ 4
ptEndHead       equ 5
ptEndSector     equ 6
ptEndCylinder   equ 7
ptStartSector   equ 8
ptSectors       equ 12

; TR-SINGLIX FS1 BootSector Identification (Data) Block
; Singlix OS project Issue:1, Revision:14
; (07/01/2018)
bsFSystemID	equ 3
bsBytesPerSec	equ 6
bsMediaAttrib	equ 8
bsPartitionID	equ 9
bsFSVersionMaj	equ 10
bsFSVersionMin	equ 11
bsBootSector	equ 12 
bsVolumeSize	equ 16
bsStartupFD	equ 20
bsMATLocation	equ 24
bsRootDirD	equ 28
bsSystemConfFD	equ 32
bsSwapFD	equ 36
bsUndeleteDirD	equ 40
bsDriveNumber	equ 44
bs_LBA_Ready	equ 45
bsMagicWord	equ 46
bs_Disk_SecPerTrack equ 46
bs_Disk_Heads	equ 47 
bsOperationSys	equ 48
bs_terminator	equ 64

; MAT
; TR-SINGLIX FS1 Master Allocation Table
; Singlix OS project Issue:5, Revision:5
; (07/01/2018)
MAT_DAT_Address	equ 12
MAT_SectorCount	equ 16
MAT_FreeSectors	equ 20
MAT_FirstFreeSector equ 24

; FDT
; TR-SINGLIX FS1 File Description Table
; Singlix OS project Issue:2, Revision:14
; (07/01/2018)
fdtSectorCount	equ 16
fdtFileSize	equ 28
fdtLMDate	equ 56
fdtLMTime	equ 60
fdtFileName	equ 64

;-----------------------------------------------------------------
;  CODE
;-----------------------------------------------------------------

[BITS 16]
[ORG 100h]
		mov	bx, SizeOfFile+100
		add	bx, 15
		shr	bx, 1
        	shr	bx, 1
		shr	bx, 1
		shr	bx, 1
		mov	ah, 4Ah ; modify memory allocation
               ;push	cs
               ;pop	es
		int	21h 
				  
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; see if drive specified
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
              
		mov	si, 80h			; PSP command tail
		mov	cl, [si]
		or	cl, cl		               
		jz	BF_9			; jump if zero
BF_1:
		inc	si
		mov	al, [si]
		cmp	al, ' '			; is it SPACE ?
		jne	short BF_2

		dec	cl				  
		jne	short BF_1		  
		jmp	BF_9
BF_2:
		cmp	al, 'f'
		jne	short BF_3
		inc	si
		mov	al, [si]
		cmp	al, 'd'
		jne	BF_9
		inc	si
		mov	al, [si]
		cmp	al, '0'		            
		jb	BF_9
		cmp	al, '1'
		ja	BF_9
		mov	[SINGLIX_FD_Number], al
		mov	dl, al
		sub	dl, '0'
		mov	[PhysicalDriveNumber], dl
		mov	ah, 08h
		int	13h
		; ES <> DS
		jc	BF_16
		inc	dh
		mov	[Physical_Disk_Heads], dh
		and	cl, 00111111b
		mov	[Physical_Disk_SecPerTrack], cl
		;; 27/01/2018
		;;mov	byte [Physical_Disk_Heads], 2
		;;mov	byte [Physical_Disk_SecPerTrack], 18
		;mov	[LBA_Ready], 0
                push	ds
                pop	es
		mov	dl, [PhysicalDriveNumber]
		call	load_masterboot
               ;mov	si, BSBUFFER
		mov	si, MasterBootBuff
		cmp	word [si+bsFSystemID], 'FS'
		je	short loc_check_fd_bs_A1h_sign
loc_not_singlix_fs_fd:
		mov	si, Msg_Not_Singlix_FS
		call	print_msg
		jmp	BF_13
loc_check_fd_bs_A1h_sign:
		cmp	byte [si+bsPartitionID], 0A1h
		jne	short loc_not_singlix_fs_fd
		mov	ax, write_chs_sector  
		mov	[write_sector], ax
		;; 28/01/2018
		;cmp	byte [si+bs_LBA_Ready], 0
		;ja	BF_6 ; ?
		;; Update CHS parameters (according to boot sector)
		;mov	ax, [si+bs_Disk_SecPerTrack]
		;mov	[Physical_Disk_SecPerTrack], al
		;mov	[Physical_Disk_Heads], ah
		jmp	BF_6
BF_3:
		cmp	al, 'h'
		jne	BF_9
		inc	si
		mov	al, [si]
		cmp	al, 'd'
		jne	BF_9
		inc	si
		mov	al, [si]
		cmp	al, '0'		            
		jb	BF_9
		cmp	al, '3'
		ja	BF_9
		mov	[SINGLIX_HD_Number], al
		inc	si
		mov	ax, [si]
		cmp	ah, 20h
		ja	BF_9
		cmp	al, 'f'
		je	short BF_4
		cmp	al, 's'
		je	short BF_4		   
		cmp	al, '1'
		jb	BF_9
		cmp	al, '4'
		ja	BF_9
BF_4:
		mov	[SINGLIX_HD_Number+1], al
		mov	dl, [SINGLIX_HD_Number]
		add	dl, 80h-'0'
		mov	[PhysicalDriveNumber], dl
BF_4_check_int13h_extensions:
               ; 05/01/2010 
		mov	ah, 41h ; Check INT 13h Extensions Present
		mov	bx, 55AAh
		int	13h
		jc	short BF_4_lba_not_ready
		cmp	bx, 0AA55h
		jne	short BF_4_lba_not_ready
		and	cl, 1		   ; Fixed disk access subset check
		jz	short BF_4_lba_not_ready 
BF_4_lba_ready:
		mov	ax, write_lba_sector
		;mov	[LBA_Ready], cl ; 1
		inc	byte [LBA_Ready]
		jmp	short BF_4_set_disk_write_procedure  
BF_4_lba_not_ready:
		;mov	[LBA_Ready], cl ; 0
		mov	ah, 08h
		;mov	dl, [PhysicalDriveNumber]
		int	13h
		; ES <> DS
		jc	BF_16
		inc	dh
		mov	[Physical_Disk_Heads], dh
		and	cl, 00111111b
		mov	[Physical_Disk_SecPerTrack], cl
		mov	ax, write_chs_sector  
BF_4_set_disk_write_procedure:
		mov	[write_sector], ax
BF_4_load_masterboot:
		push	ds
		pop	es 
		mov	dl, [PhysicalDriveNumber]
		call	load_masterboot
		jc	BF_16
		mov	si, PartitionTable
		mov	al, [SINGLIX_HD_Number+1]
		cmp	al, 'f'
		jne	short pass_check_first_singlix_partition
		xor	al, al
		mov	cx, 4
loc_check_fs_f_partition:
		inc	al
		cmp	byte [si+ptFileSystemID], 0A1h
		jne	short check_for_first_fs_partition_again
loc_fs_partition_found:
		mov	[fsPartitionNumber], al 
		add	al, '0'
		mov	[SINGLIX_HD_Number+1],al
		jmp	short BF_5
check_for_first_fs_partition_again:
		add	si, 10h
		loop	loc_check_fs_f_partition
		jmp	BF_16
pass_check_first_singlix_partition:
		cmp	al, 's'
		jne	short pass_check_second_singlix_partition
		xor	al, al 
		mov	cx, 4
loc_check_fs_s_partition:  
		cmp	byte [si+ptFileSystemID], 0A1h
		jne	short check_for_second_fs_partition_again
		inc	al
		cmp	al, 2
		jne	short check_for_second_fs_partition_again
		jmp	short loc_fs_partition_found
check_for_second_fs_partition_again:
		add	si, 10h
		loop	loc_check_fs_s_partition
		jmp	BF_16
pass_check_second_singlix_partition :
		sub	al, '1'
		mov	ah, 10h
		mul	ah   
		add	si, ax
		cmp	byte [si+ptFileSystemID], 0A1h
		jne	BF_16
BF_5:
		mov	ax, [si+ptStartSector] 
		mov	dx, [si+ptStartSector+2]
		mov	[fsBootSector], ax
		mov	[fsBootSector+2], dx

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Write message
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
BF_6:
		mov	si, Msg_DoYouWantToWrite
		call	print_msg
		cmp	byte [PhysicalDriveNumber], 80h
		jnb	short BF_7
		mov	si, SINGLIX_FD_Name
		call	print_msg
		jmp	short BF_7_yn
BF_7:   
		mov	si, SINGLIX_HD_Name
		call	print_msg
BF_7_yn: 
		mov	si, msg_yes_no
		call	print_msg
BF_8:
		xor	ax, ax
		int	16h			; wait for keyboard command
		cmp	al, 'C'-40h
		je	BF_13		   
		cmp	al, 27
		je	BF_13
		and	al, 0DFh
		cmp	al, 'Y'			; Yes?
		je	short BF_10		; write
		cmp	al, 'N'			; No?
		je	BF_15			; no write (exit)
		jmp	short BF_8
BF_9:
		mov	si, SINGLIX_Welcome
		call	print_msg
		jmp	BF_13

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; get drive parameters
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
BF_10:
		mov	si, msg_YES
		call	print_msg
loc_load_fs_boot_sector:
		mov	si, BSBUFFER ; 28/01/2018
		cmp	byte [PhysicalDriveNumber], 80h
		jb	short loc_check_current_sfdt_adress_fd
		mov	ax, [fsBootSector]
		mov	dx, [fsBootSector+2]
 		;mov	bx, BSBUFFER
		mov	bx, si ; 28/01/2018
		call	disk_read
		jc	BF_16
		cmp	word [si+bsFSystemID], 'FS'
		jne	short loc_not_singlix_fs_hdp
loc_check_hdp_bs_A1h_sign:
		cmp	byte [si+bsPartitionID], 0A1h
		je	short loc_check_current_sfdt_adress
loc_not_singlix_fs_hdp:
		mov	si, Msg_Not_Singlix_FS
		call	print_msg
		jmp	BF_13
loc_check_current_sfdt_adress_fd:
		;mov	si, BSBUFFER  
loc_check_current_sfdt_adress:
		; 28/01/2018
		cmp	byte [si+bs_LBA_Ready], 0
		ja	short pass_update_chs_parameters
		; Update CHS parameters (according to boot sector)
		mov	ax, [si+bs_Disk_SecPerTrack]
		mov	[Physical_Disk_SecPerTrack], al
		mov	[Physical_Disk_Heads], ah
pass_update_chs_parameters:
		mov	ax, [si+bsStartupFD]
		mov	dx, [si+bsStartupFD+2]
		or	ax, ax
		jnz	short loc_startup_file_exists
		or	dx, dx
		jnz	short loc_startup_file_exists		  
BF_11:
		mov	si, Msg_DosFile_Name
		call	print_msg
		call	rw_char
		jc	short pass_write_file_name_chr0
		mov	di, StartupFile_Name
		cmp	byte [si], 20h
		jna	short pass_write_file_name_chr0
		mov	cx, 64
loc_write_file_name_chr0:
		lodsb
		cmp	al, 20h
		;jnb	short loc_write_file_name_chr1 
		; 27/01/2018
		ja	short loc_write_file_name_chr1 
pass_write_file_name_chr0:
		mov	byte [di], 0
		inc	di
		loop	pass_write_file_name_chr0
		jmp	short loc_find_dos_file
loc_write_file_name_chr1:
		mov	[di], al
		inc	di
		loop	loc_write_file_name_chr0
loc_find_dos_file:	
		mov	si, SINGLIX_CRLF
 		call	print_msg
		mov	dx, StartupFile_Name
		mov	cx, 27h ; File Attributes
		mov	ah, 4Eh ; MS Dos Function = Find First File
		int	21h
		jnc	short loc_open_dos_file_1
		cmp	ax, 2
		jne	BF_16
		jmp	loc_dos_file_notfound
loc_startup_file_exists:
		mov	[StartupFile_FDT], ax
		mov	[StartupFile_FDT+2], dx
		mov	si, msg_Startup_File_Exists
		call	print_msg
loc_startup_file_exist_select_option:
		xor	ah, ah
		int	16h
		cmp	al, 1Bh
		je	BF_13
		cmp	al, 0Dh	
		jne	short loc_check_startupfile_delete_decision
		call	display_startupfile_info
		mov	si, msg_please_select_an_option
		call	print_msg
		jmp	short loc_startup_file_exist_select_option 
loc_check_startupfile_delete_decision:
		cmp	ah, 53h
		jne	short loc_startup_file_exist_select_option
		mov	si, BSBUFFER 
		call	delete_fs_startupfile
		jnc	short loc_startup_file_deleted_msg
		cmp	al, 0Bh
		jne	BF_16
		mov	si, msg_invalid_format
		call	print_msg
		jmp	BF_13
loc_startup_file_deleted_msg:
		mov	si, Msg_startup_file_deleted
		call	print_msg
               ;jmp	short BF_11 
		jmp	BF_13
loc_open_dos_file_1:
		mov	si, DTA_FileName
		mov	di, StartupFile_Name
		mov	cx, 64
loc_copy_found_file_name_chr0:
		lodsb
		cmp	al, 20h
		ja	short loc_copy_found_file_name_chr1 
pass_copy_found_file_name_chr0:
		mov	byte [di], 0
		inc	di
		loop	pass_copy_found_file_name_chr0
		jmp	short loc_set_startupfile_decriptor_table
loc_copy_found_file_name_chr1:
		mov	byte [di], al
		inc	di
		loop	loc_copy_found_file_name_chr0
loc_set_startupfile_decriptor_table:
		mov 	si, DTA_FileSize
		mov     ax, [si]
		mov 	dx, [si+2]
		mov 	[fdt_file_size], ax
		mov 	[fdt_file_size+2], dx
		mov     si, DTA_Date
		mov 	ax, [si]
		push 	ax
		and 	ax, 00011111b		; Day Mask
		aam				; Q([AL]/10)->AH
						; R([AL]/10)->AL
						; [AH]+[AL]= Day as BCD
		shl 	ah, 1
		shl 	ah, 1
		shl 	ah, 1
		shl 	ah, 1 
		add 	ah, al     
		mov 	[fdt_make_day], ah
		pop 	ax
		push 	ax 
		mov     cl, 5
		shr     ax, cl			; shift right 5 times
		and     ax, 00001111b		; Month Mask
  		shl 	ah, 1
		shl 	ah, 1
		shl 	ah, 1
		shl 	ah, 1 
		aam
		add	ah, al     
		mov	[fdt_make_month], ah
		pop	ax
		mov	cl, 9
		shr	ax, cl
	       ;and	ax, 01111111b		; Result = Year - 1980
		add	ax, 1980
		xor	dx, dx
		mov	cx, 100
		div	cx		
		aam
  		mov	cl, 4
		shl	ah, cl
		add	ah, al     
		mov	[fdt_make_year], ah
 		mov	al, dl
		aam
  	       ;mov	cl, 4
		shl	ah, cl
		add	ah, al      
		mov	[fdt_make_year+1], ah
		mov	si, DTA_Time
		mov	ax, [si]
		push	ax
		; 28/01/2018
		and	ax, 000011111b		; Second Mask
		shl	al, 1
		aam
  	       ;mov	cl, 4
		shl	ah, cl
		add	ah, al
		mov	[fdt_make_second], ah
		pop	ax
		push	ax
		mov	cl, 5
		shr	ax, cl			; shift right 5 times
		and	ax, 0000111111b		; Minute Mask
		aam
  		mov	cl, 4
		shl	ah, cl
		add	ah, al
		mov     [fdt_make_minute], ah
		pop	ax
		mov	cl, 11
		shr	ax, cl			; ax = hour
		aam
  	        mov	cl, 4
		shl	ah, cl 
		add	ah, al
		mov	[fdt_make_hour], ah
 		mov	ah, 02h			; Return Current Time
		int	1Ah
		xchg	ch, cl
		mov	[fdt_lm_hour], cx
		xchg	dh, dl
		mov	[fdt_lm_second], dx
		mov	[fdt_lm_hour], cx
		mov	[fdt_lm_second], dx
		mov	ah, 04h			; Return Current Date
		int	1Ah
		xchg	ch,cl
		mov	[fdt_lm_year], cx
		xchg	dh,dl
		mov	[fdt_lm_month], dx
loc_open_dos_file_2:
		mov	dx, StartupFile_Name
		mov	ah, 3Dh ; MS Dos Function = Open File
		xor	al, al  
		int	21h
		jnc	short loc_save_filehandle
	        cmp	ax, 2
		jne	BF_16		
loc_dos_file_notfound:
		mov	si, Msg_File_Not_Found
		call	print_msg
		jmp	BF_13
loc_save_filehandle:
		mov	[FileHandle], ax
loc_display_startup_file_name:
		mov	si, Msg_StartupFile_Name
		call	print_msg
		mov	ah, 03h
               ;xor	bh, bh
		int	10h
               ;push	dx         
		mov	si, StartupFile_Name
		call	print_msg
               ;pop	dx
		mov	ah, 02h
               ;xor	bh, bh
		int	10h
		call	rw_char
		jc	BF_13
		cmp	byte [si], 20h
		jna	BF_13
		mov	di, fdt_file_name
		mov	cx, 64
loc_rename_file_name_chr0:
		lodsb
		cmp	al, 20h
		ja	short loc_rename_file_name_chr1 
pass_rename_file_name_chr0:
		mov	byte [di], 0
		inc	di
		loop	pass_rename_file_name_chr0
		jmp	short loc_get_SFDT_Address
loc_rename_file_name_chr1:
		mov	[di], al
		inc	di
		loop	loc_rename_file_name_chr0
loc_get_SFDT_Address:
		mov	si, SINGLIX_CRLF
 		call	print_msg
		mov	ax, [fdt_file_size]
		mov	dx, [fdt_file_size+2]
		add	ax, 511
		adc	dx, 0
		mov	cx, 512
		call	Div32
		mov	[fdt_scount], ax
		mov	[fdt_scount+2], dx
		add	ax, 1
		adc	dx, 0
		or	dx, dx
		jnz	BF_16
		mov	cx, ax
		mov	si, BSBUFFER
		call	get_first_free_section
		jc	BF_16
		mov	[StartupFile_FDT], ax
		mov	[StartupFile_FDT+2], dx
loc_write_SF:
		mov	si, Msg_writing_sf
		call	print_msg
		mov	ax, [StartupFile_FDT]
		mov	dx, [StartupFile_FDT+2]
		; 28/01/2018
		mov	[fdt_location], ax
		mov	[fdt_location+2], dx
		add	ax, [fsBootSector]
		adc	dx, [fsBootSector+2]
		mov	bx, FDTBUFFER     
		call	word [write_sector]
		jc	BF_16
		mov	ax, [StartupFile_FDT]
		mov	dx, [StartupFile_FDT+2]
		mov	si, BSBUFFER
		mov	[si+bsStartupFD], ax
 		mov	[si+bsStartupFD+2], dx 
              	add	ax, 1
		adc	dx, 0
 		mov	[StartupFile_FDT], ax
		mov	[StartupFile_FDT+2], dx    
BF_12_rp:		
		mov	ah, 3Fh ; Read File
		mov	cx, 512
		mov	dx, SECBUFFER
		mov	bx, [FileHandle]
		int	21h
		jc	short BF_12_c
		push	ax
		mov	ax, [StartupFile_FDT]
		mov	dx, [StartupFile_FDT+2]
		add	ax, [fsBootSector]
		adc	dx, [fsBootSector+2]
		mov	bx, SECBUFFER     
		call	word [write_sector]
		pop	ax
		jc	short BF_12_c
		cmp	ax, 512
		jne	short BF_12_cmc
		xor	dx, dx
		mov	ax, [StartupFile_FDT]
		add	ax, 1
		adc	dx, [StartupFile_FDT+2]
 		mov	[StartupFile_FDT], ax
		mov	[StartupFile_FDT+2], dx    
               	jmp	short BF_12_rp
BF_12_cmc:
		cmc 
BF_12_c:
	        pushf
		mov	ah, 3Eh ; Close File
		mov	bx, [FileHandle]
		int	21h
		popf 
		jc	BF_16
  	       ;mov	si, BSBUFFER
		mov	ax, [si+bsStartupFD]
 		mov	dx, [si+bsStartupFD+2] 
		mov	[StartupFile_FDT], ax
		mov	[StartupFile_FDT+2], dx
		mov	bl, 06h
		call	update_dat
		jc	BF_14
		mov	cx, [fdt_scount]		
BF_12_loop:
		mov	ax, [StartupFile_FDT]
		mov	dx, [StartupFile_FDT+2]		
		add	ax, 1
		adc	dx, 0
 		mov	[StartupFile_FDT], ax
		mov	[StartupFile_FDT+2], dx
		push	cx
		mov	bl, 07h
		call	update_dat
		pop	cx  
		jc	short BF_14
		loop	BF_12_loop
		cmp	byte [DAT_Buffer_Updated], 0
		jna	short BF_12_um
		mov	ax, [DAT_Buffer_Sector]
		mov	dx, [DAT_Buffer_Sector+2]
		call	write_dat_sector
		jc	short BF_14
BF_12_um:
		mov	si, BSBUFFER
		mov	cx, 2 ; FDT(DDT) + 1 sector data
		call	get_first_free_section
		jnc	short BF_12_wm
		; Invalid data => 0FFFFFFFFh sign
		xor	ax, ax
		dec	ax 
		mov	dx, ax
BF_12_wm:
		mov	cx, [fdt_scount]
		inc	cx
		sub	[mat_dat_free_s], cx
		sbb	word [mat_dat_free_s+2], 0
		mov	[mat_dat_ffs], ax
		mov	[mat_dat_ffs+2], dx
 	       ;mov	bx, MATBUFFER
		mov	bx, si
		mov	si, BSBUFFER
		mov	ax, [si+bsMATLocation]
 		mov	dx, [si+bsMATLocation+2] 
		add	ax, [fsBootSector]
		adc	dx, [fsBootSector+2]
		call	word [write_sector]
		jc	short BF_14 
BF_12_bs:
   		mov	bx, BSBUFFER
		mov	ax, [fsBootSector]
		mov	dx, [fsBootSector+2]
		call	word [write_sector]
		jc	short BF_14
BF_12:        
		mov	si, Msg_OK
		call	print_msg
BF_13:
		mov	si, SINGLIX_CRLF
		call	print_msg
		int	20h
		int	19h 
BF_14:  
		mov	si, msg_singlix_drv_write_error
		call	print_msg
		jmp	short BF_17
BF_15:
		mov	si, msg_NO
		call	print_msg
		jmp	short BF_13 
BF_16:
		mov	si, msg_singlix_drv_read_error
		call	print_msg
BF_17:
		xor	ax, ax
		int	16h			; wait for keyboard command
		cmp	al, 'C'-40h
		je	short BF_13		   
		cmp	al, 27
		je	short BF_13
		and	al, 0DFh
		cmp	al, 'Y'
		je	short BF_18		; Retry
		cmp	al, 'N'
		je	short BF_13		; Exit
		jmp	short BF_17
BF_18:
		mov	dl, [PhysicalDriveNumber]
		mov	ah, 08h
		int	13h			; return disk parameters
		push	cs
		pop	es			; restore es
		jc	short BF_16
		cmp	bl, 4			; Drive Type
		jb	short BF_16
		xor	ah, ah
		mov	dl, [PhysicalDriveNumber]
		int	13h 
		jc	short BF_16  
		jmp	loc_load_fs_boot_sector

;-----------------------------------------------------------------
; convert binary number to decimar string
;-----------------------------------------------------------------

bin_to_decimal:
		; 6-5-2009
		;  Erdogan Tan
		; INPUT: 
		;	DX:AX = Binary Number
		; OUTPUT:
		;	Decimal chars at DS:SI
		; CX, AX, DX, SI, BX, BP will be changed.
		;
		push	bp
		mov	si, Decimal_Str
		push	si
		mov	cx, 9
loc_reset_str_NumberInput:
		mov	byte [si], '0'
		inc	si
		loop	loc_reset_str_NumberInput
		mov	bp, sp
		mov	cl, 10
loc_rediv_NumberInput:
		call	Div32
		add	bl,'0'
		push	bx
		dec	si
		cmp	ax, 0
		ja	short loc_rediv_NumberInput
		cmp	dx, 0
		ja	short loc_rediv_NumberInput
loop_popbx_NumberInput: 
		pop	bx
		mov	[si], bl
		inc	si
		cmp	bp, sp
		jne	short loop_popbx_NumberInput
		pop	si
		pop	bp  
		retn

;-----------------------------------------------------------------
; print message
;-----------------------------------------------------------------

print_msg:
		; DS:SI -> Message address
print_msg_LOOP:
		lodsb				; Load byte at DS:SI to AL
		and     al, al            
		jz      short print_msg_OK       
		mov     AH, 0Eh
		mov     BX, 07h             
		int     10h			; BIOS Service func ( ah ) = 0Eh
						; Write char as TTY
                                                ;INPUT: AL-char BH-page BL-color
		jmp	short print_msg_LOOP           
print_msg_OK:
                retn

;#############################################################################
;#
;#              PROCEDURE CHS_read
;#
;#############################################################################

CHS_read:
		; 27/01/2018
		; 31/01/2010
		; INPUT -> DX:AX = Logical Block Address
		; ES:BX = Destination Buffer
		; OUTPUT -> clc or stc
		push	si
		push	cx            
		mov	di, 5   
loc_read_disk_chs:
		push	ax			; Linear sector #
		push	dx			; DX_AX = Linear address (sector)
		mov	cx, [Physical_Disk_SecPerTrack]
		push	bx
		call	Div32			; Special 32 bit divide !!!
						; (To fix large disk problem.)
						; by Erdogan Tan
						; (October 20th, 1999)
		mov	cx, bx			; Sector (zero based)
		inc	cx			; To make it 1 based
		push	cx
		mov	cx, [Physical_Disk_Heads]
		call	Div32			; Convert track to head & cyl
		mov	dh, bl			; BX = Head (max. FFh)
		pop	cx			; AX=Cyl, DH=Head, CX=Sector
		pop	bx			; ES:BX = Buffer
		mov	dl, [PhysicalDriveNumber]
		mov	ch, al                   
		ror	ah, 1			; Rotate right
		ror	ah, 1                   
		or	cl, ah                   
		mov	ax, 0201h
		int	13h			; BIOS Service func ( ah ) = 2
						; Read disk sectors
						;INPUT:
						; AL-sec num CH-track CL-sec
						; DH-head DL-drive ES:BX-buffer
						;OUTPUT:
						; CF-flag AH-stat AL-sec read
						; If CF = 1 then (If AH > 0)
		pop	dx
		pop	ax
		jnc	short pass_read_disk_chs_error              
		dec	di
		;jz	short pass_read_disk_chs_error     
		;xor	ah, ah                   
		;mov	dl, [PhysicalDriveNumber]
		;int	13h			; BIOS Service func ( ah ) = 0
						; Reset disk system
		jnz	short loc_read_disk_chs                  
		;jmp	short loc_read_disk_chs          
pass_read_disk_chs_error:
		pop	cx
		pop	si
		retn				; db 0C3h


;#############################################################################
;#
;#              PROCEDURE LBA_read
;#
;#############################################################################

LBA_read:
		; 21/02/2010
		; 31/01/2010
		; INPUT -> DX:AX = LBA address
		; INPUT -> ES:BX = Buffer
                mov	di, 5 
loc_read_disk_lba:
               ;pusha				; db 60h
		db	60h
               ;push	0			; db 6Ah, 00h
                db	6Ah, 0
               ;push	0			; db 6Ah, 00h
                db	6Ah, 0
                push	dx
                push	ax
                push	es
                push	bx
               ;push	1			; db 6Ah, 01h
                db	6Ah, 01h                     
               ;push	10h			; db 6Ah, 10h
                db	6Ah, 10h
                mov	si, sp
		; DS:SI= DAP Location
		mov	ah, 42h  ; Extended Disk Read - LBA Read
		mov	dl, [PhysicalDriveNumber]
		int	13h
		;popa
		db	61h
		;popa
		db	61h
		jnc	short pass_read_disk_lba_error
		dec	di 
		jnz	short loc_read_disk_lba
pass_read_disk_lba_error:
		retn

;#############################################################################
;#
;#              PROCEDURE disk_read
;#
;#############################################################################

disk_read:
		; 31/01/2010
		; INPUT -> DX:AX = LBA address
		; INPUT -> ES:BX = Buffer
		cmp	byte [LBA_Ready], 0
		ja	short loc_read_lba_sectors
loc_read_chs_sectors:
		call	CHS_read
		jnc	short retn_read_sectors
retn_read_sectors_stc:
		mov	ax, 15h ; Drv not ready or read error !
retn_read_sectors:
		retn
loc_read_lba_sectors:
		call	LBA_read
		jc	short retn_read_sectors_stc
		retn

;#############################################################################
;#
;#              PROCEDURE write_lba_sector
;#
;#############################################################################

write_lba_sector:
		; 31/01/2010
loc_write_lba_sectors:                
		mov	di, 5
loc_0FFh:
		;pusha				; db 60h
		db	60h
		;push	0			; db 6Ah, 00h
		db	6Ah, 0
		;push	0			; db 6Ah, 00h
		db	6Ah, 0
		push	dx
		push	ax
		push	es
		push	bx
		;push	1			; db 6Ah, 01h
		db	6Ah, 01h                     
		;push	10h			; db 6Ah, 10h
		db	6Ah, 10h
		mov	dl, [PhysicalDriveNumber] 
		mov	ah, 43h
		xor	al, al			; Verify off
		mov	si, sp
		int	13h
		;popa
		db	61h
		;popa
		db	61h
                jnc	short loc_12Bh
		dec	di
		;jz	short loc_12Bh
		jnz	short loc_0FFh  
		;xor	ah, ah
		;mov	dl, [PhysicalDriveNumber] 
		;int	13h
		;jmp	short loc_0FFh                  
loc_12Bh:
		retn                            ; db 0C3h
                        

;#############################################################################
;#
;#              PROCEDURE write_chs_sector
;#
;#############################################################################

write_chs_sector:
		; 31/01/2010
loc_write_chs_sector:
		push	si
		push	cx            
loc_09Bh:
		mov	di, 5                    
loc_0CAh:               
		push	ax			; Linear sector #
		push	dx			; DX_AX = Linear address (sectors)
		mov	cx, [Physical_Disk_SecPerTrack]
		push	bx
		call	Div32			; Special 32 bit divide !!!
						; (To fix large disk problem.)
  						; by Erdogan Tan
						; (October 20th, 1999)
		mov	cx, bx			; Sector (zero based)
		inc	cx			; To make it 1 based
		push	cx
		mov	cx, [Physical_Disk_Heads]
		call	Div32			; Convert track to head & cyl
		mov	dh, bl			; BX = Head (max. FFh)
		pop	cx			; AX=Cyl, DH=Head, CX=Sector
		pop	bx			; ES:BX = Buffer
		mov	dl, [PhysicalDriveNumber]
		mov	ch, al                   
		ror	ah, 1			; Rotate right
		ror	ah, 1                   
		or	cl, ah                   
		mov	ax, 0301h
		int	13h			; BIOS Service func ( ah ) = 3
						; Write disk sectors
						;INPUT:
						; AL-sec num CH-track CL-sec
						; DH-head DL-drive ES:BX-buffer
						;OUTPUT: 
						; CF-flag AH-stat AL-sec read
						; If CF = 1 then (If AH > 0)
		pop	dx
		pop	ax
		jnc	short loc_chs_12Bh              
		dec	di                      
		;jz	short loc_chs_12Bh              
                ;xor	ah, ah                   
		;mov	dl, [PhysicalDriveNumber]
		;int	13h			; BIOS Service func ( ah ) = 0
						; Reset disk system
		jnz	short loc_0CAh                  
		;jmp	short loc_0CAh          
loc_chs_12Bh:
		pop	cx
		pop	si
		retn				; db 0C3h

;-----------------------------------------------------------------
; read character from keyboard and write it to screen
;-----------------------------------------------------------------

rw_char:
		; OUTPUT -> DS:SI = Entered String (ASCIIZ)
		mov	si, StartupFile_Name
		mov	bx, 7
		mov	ah, 3
		int	10h
		mov	[Cursor_Pos], dx
read_next_char:
		xor	ah, ah
		int	16h
		and	al, al
		jz	short loc_arrow    
		cmp	al, 0E0h          
		je	short loc_arrow
		cmp	al, 08h
		jne	short char_return
loc_back:
		mov	bl, 7
		mov	ah, 3
		int	10h
		cmp	dl, [Cursor_Pos]
		ja      short prev_column
loc_beep:
		mov     ah, 0Eh
		mov     al, 7
		int     10h
		jmp     short read_next_char
prev_column:
		dec	dl
set_cursor_pos:
		mov	ah, 2
		int	10h
		mov	bl, dl
		sub	bl, [Cursor_Pos] 
		mov	cx, 1
		mov	ah, 9
		mov	al, 20h
		mov	[si+bx], al
loc_write_it:
		mov	bl, 7
		int	10h
		mov	dx, [Cursor_Pos]
		jmp	short read_next_char
loc_arrow:    
		cmp	ah, 4Bh
		je	short loc_back
		cmp	ah, 53h
		je	short loc_back
		jmp	short read_next_char
char_return:
		mov	bl, 7
		mov	ah, 3
		int	10h
		mov	bl, dl
		sub	bl, [Cursor_Pos] 
		cmp	al, 20h
		jb	short loc_escape
		cmp	bl, 63
		ja	short loc_beep
		;cmp	al, 'z'
		;ja	short read_next_char
		;cmp	al, 'a'
		;jb	short pass_capitalize
		;and	al, 0DFh
pass_capitalize:
		xor	ah, ah
		mov	[si+bx], ax
		mov	ah, 0Eh
		mov	bl, 7
		int	10h
		jmp	short read_next_char
pass_escape:
		cmp	al, 0Dh
		jne	short read_next_char
		;mov	bl, 7
		mov	ah, 0Eh
		int	10h
		mov	al, 0Ah
		int	10h
		retn
loc_escape:
		cmp	al, 1Bh
		jne	short pass_escape
		stc
		retn

;-----------------------------------------------------------------
; read/load masterboopt sector
;-----------------------------------------------------------------

load_masterboot:
		; input -> dl = drive number
		xor	ah, ah
		int	13h
		;jnc	short pass_reset_error
		jc	short ret_from_load_mb ; 23/4/2009 FSFDISK.COM
;harddisk_error:
;		retn
pass_reset_error:
		mov	bx, MasterBootBuff
		mov	ax, 0201h
                mov	cx, 1
		xor	dh, dh
		;push	ds
		;pop	es
		int	13h
		;jc	short harddisk_error
		;cmp	word [MBIDCode],0AA55h ; 23/4/2009 FSFDISK.COM
		;jne	short loc_not_masterboot
                retn
;loc_not_masterboot:
;		stc
ret_from_load_mb:   ; 23/4/2009 FSFDISK.COM
		retn

;-----------------------------------------------------------------
; update disk allocation table
;-----------------------------------------------------------------

update_dat:
		; 28/01/2018
		; 27/01/2018 (bit allocation)
                ; 20/02/2010 [DAT_Buffer_Offset]
                ; 13/02/2010
                ;
                ; 02/05/2009
                ; DX:AX = Disk Sector (Beginning Sector, with Descriptor)
                ; BL = Allocation Type (Identifier)
		; 27/01/2018	
		;	BL = 90h --> Deallocation	
		;	BL <> 90h --> Allocation 
		;
                mov	[DAT_Identifier], bl
                mov	cx, 8*512 ; 27/01/2018
                call	Div32
                cmp	[DAT_Buffer_Sector+2], dx
                jne	short loc_write_prev_DAT_sector
                cmp	[DAT_Buffer_Sector], ax
                jne	short loc_write_prev_DAT_sector
                ;mov	byte [DAT_Buffer_Updated], 0                
loc_update_dat_buffer_x:
                push	si
                mov	si, bx	; Bit offset (0 to 4095)	
		mov	cl, bl
		and	cl, 7
		mov	al, 1
		shl	al, cl
		mov	bl, [DAT_Identifier]
		shr	si, 3 ; convert bit offset to byte offset
		add	si, DATBUFFER
		cmp	bl, 90h ; Deallocation
		je	short loc_update_dat_buffer_y ; deallocation
		; Allocation
		not	al ; Allocation bit is 0, others are 1
		and	[si], al
		jmp	short loc_update_dat_buffer_z		
loc_update_dat_buffer_y: ; Deallocation
		; Allocation bit is 1 (free), others are 0
		or	[si], al
loc_update_dat_buffer_z:
                mov	byte [DAT_Buffer_Updated], 1   
                pop	si 
return_from_dat_update:
                retn
loc_write_prev_DAT_sector:
                mov	[DAT_Buffer_Offset], bx ; Bit offset
                cmp	byte [DAT_Buffer_Updated], 0
                jna	short loc_read_DAT_sector
                push	dx
                push	ax 
                mov	ax, [DAT_Buffer_Sector]
                mov	dx, [DAT_Buffer_Sector+2]
                call	write_dat_sector
                jnc	short loc_read_DAT_sector_pop_ax_dx
                pop	dx
                pop	dx
                mov	bl, [DAT_Identifier]
                jmp	short return_from_dat_update
loc_read_DAT_sector_pop_ax_dx:
                pop	ax
                pop	dx
loc_read_DAT_sector:
                call	load_dat_sector
                jc	short return_from_dat_update 
		mov	bx, [DAT_Buffer_Offset] ; Bit offset
                jmp	short loc_update_dat_buffer_x

load_dat_sector:
		; 13/02/2010
		; Input ->  ; DX:AX = DAT Sector (Offset)
		; Output -> clc : load error (err code in al)
		;           clc -> loading successed
		; DX:AX = DAT Sector (Offset) Address
		; BX = DAT Buffer Address
		; [DAT_Buffer_Sector] is updated
		;
		push	dx
		push	ax
		add	ax, [mat_begin_sec]
		adc	dx, [mat_begin_sec+2]
		add	ax, [mat_dat_lba]
		adc	dx, [mat_dat_lba+2]
		mov	bx, DATBUFFER               
		call	disk_read
		jnc	short loc_load_DAT_sector_clc
		pop	dx
		pop	dx
		retn
loc_load_DAT_sector_clc:
		pop	ax
		pop	dx
		mov	[DAT_Buffer_Sector], ax
		mov	[DAT_Buffer_Sector+2], dx
		mov	byte [DAT_Buffer_Updated], 0
		retn

write_dat_sector:
                ; 20/02/2010
                ; Input ->  ; DX:AX = DAT Sector (Offset)
                ; Output -> clc : write error (err code in al)
                ;           clc -> write successed
                ; DX:AX = DAT Sector (Offset) Address
                ; BX = DAT Buffer Address
		;
		add	ax, [mat_begin_sec]
		adc	dx, [mat_begin_sec+2]
		add	ax, [mat_dat_lba]
		adc	dx, [mat_dat_lba+2]
		mov	bx, DATBUFFER               
		call	word [write_sector]
		jc	short loc_write_DAT_sector_stc_retn
loc_write_DAT_sector_clc:
	       ;mov	[DAT_Buffer_Sector], ax
	       ;mov	[DAT_Buffer_Sector+2], dx
                mov	byte [DAT_Buffer_Updated], 0
loc_write_DAT_sector_stc_retn:
		retn

;-----------------------------------------------------------------
; get first free section
;-----------------------------------------------------------------

get_first_free_section:
		; 28/01/2018
	   	; 27/01/2018
		; 13/02/2010, 20/02/2010
		; 31/01/2010, 07/02/2010
		; INPUT -> DS:SI = FS Boot Sector Buffer
		;          CX = Sector Count
		; OUTPUT -> DX:AX = First Free Section
		;           DS:SI = MAT Sector Buffer
		;
                mov	[CSectorCount], cx
                mov	word [CSCounter], 0 
                mov	bx, MATBUFFER
                cmp	word [bx], 'MA'
                jne	short loc_gfss_load_MAT
                cmp	byte [bx+2], 'T'
                je	short loc_check_MAT_sign_ok
loc_gfss_load_MAT:
 		mov	ax, [si+bsMATLocation]
                mov	dx, [si+bsMATLocation+2]
                add	ax, [fsBootSector]
                adc	dx, [fsBootSector+2]
                mov	si, bx ; MATBUFFER
                call	disk_read
                jc	short loc_gffs_stc_retn
loc_check_MAT_sign:
               ;mov	si, MATBUFFER
                cmp	word [si], 'MA'
                jne	loc_not_valid_fs_mat
                cmp	byte [si+2], 'T'
                jne	short loc_not_valid_fs_mat
loc_check_MAT_sign_ok:             
                mov	ax, [si+MAT_SectorCount]
                mov	dx, [si+MAT_SectorCount+2]
                sub	ax, 1
                sbb	dx, 0
                mov	[DAT_LastSector], ax
                mov	[DAT_LastSector+2], dx
                mov	ax, [si+MAT_FirstFreeSector]
                mov	dx, [si+MAT_FirstFreeSector+2]
		; 28/01/2018
		mov	cx, 0FFFFh
		cmp	ax, cx
		jne	short loc_check_MAT_valid_ok
		cmp	dx, cx
		jne	short loc_check_MAT_valid_ok
		xor	dx, dx
		mov	ax, 1 ; BS+14
loc_check_MAT_valid_ok:   		
                mov	[Current_FS_Sector], ax
                mov	[Current_FS_Sector+2], dx
                mov	[CFDT_Address], ax
                mov	[CFDT_Address+2], dx
               	jmp	short loc_gffs_load_dat_sector
loc_not_valid_fs_mat:
                mov	ax, 0Bh ; Invalid Format 
loc_gffs_stc_retn:
                xor	cx, cx
                stc 
                retn
loc_gffs_load_dat_sector:
                mov	cx, 8*512 ; 27/01/2018
                call	Div32
		mov	si, bx ; 27/01/2018
                cmp	dx, [DAT_LastSector+2]
                ja	loc_gffs_end_of_DAT_sectors
                jb	short pass_gffs_check_scount_ax
                cmp	ax, [DAT_LastSector]
                ja	short loc_gffs_end_of_DAT_sectors
pass_gffs_check_scount_ax:                 
                call	load_dat_sector
                jc	short loc_gffs_stc_retn
loc_gffs_check_dat_cell_value:
		; 27/01/2018
		mov	bx, si
		mov	cl, bl
		shr	bx, 3  ; convert bit offset to byte offset
		add	bx, DATBUFFER
		and	cl, 7
		mov	al, 1
		shl	al, cl ; bit position in allocation byte
                test	[bx], al ; test allocation bit (1 = free)
                jnz	short loc_gffs_inc_cscounter
                mov	cx, [CSCounter]
                cmp	cx, [CSCounter_MAX]
                jna	short pass_gffs_update_cscounter_max
                mov	[CSCounter_MAX], cx
                mov	ax, [CFDT_Address]
                mov	dx, [CFDT_Address+2]
                mov	[CFDT_Address_MAX], ax
                mov	[CFDT_Address_MAX+2], dx 
pass_gffs_update_cscounter_max:
                mov	word [CSCounter], 0
                mov	ax, [Current_FS_Sector]
                mov	dx, [Current_FS_Sector+2]
                add	ax, 1
                adc	dx, 0
                mov	[CFDT_Address], ax
                mov	[CFDT_Address+2], dx
loc_gffs_check_cfs:
                mov	[Current_FS_Sector], ax
                mov	[Current_FS_Sector+2], dx
                inc	si
		; 27/01/2018
		cmp	si, 8*512
		jb	short loc_gffs_check_dat_cell_value
		jmp	short loc_gffs_load_dat_sector
loc_gffs_inc_cscounter:
                inc	word [CSCounter]
                mov	cx, [CSCounter]
                cmp	cx, [CSectorCount]
                jnb	short loc_gffs_return_ffs
                mov	ax, [Current_FS_Sector]
                mov	dx, [Current_FS_Sector+2]
                add	ax, 1
                adc	dx, 0
                jmp	short loc_gffs_check_cfs
loc_gffs_end_of_DAT_sectors:
                mov	cx, [CSCounter]
               ;cmp	cx, [CSectorCount]
               ;jnb	short loc_gffs_return_ffs
		mov	si, MATBUFFER
                cmp	[CSCounter_MAX], cx
                jb	short loc_gffs_end_of_DAT_sectors_stc
                mov	cx, [CSCounter_MAX]
                or	cx, cx
                jnz	short pass_gffs_reset_CFDT_address
                mov	ax, cx
                mov	dx, cx
                stc
                retn
pass_gffs_reset_CFDT_address: 
 		mov	ax, [CFDT_Address_MAX]
                mov	dx, [CFDT_Address_MAX+2]
                stc
                retn
loc_gffs_return_ffs:
                mov	si, MATBUFFER
loc_gffs_end_of_DAT_sectors_stc: 
                mov	ax, [CFDT_Address]
                mov	dx, [CFDT_Address+2]
                retn

;-----------------------------------------------------------------
; delete current startup file
;-----------------------------------------------------------------

delete_fs_startupfile:
		; 13/02/2010
		; 09/02/2010
		; 07/02/2010
		; INPUT -> DS:SI = FS Boot Sector Buffer
		; OUTPUT -> CLC = No error
		mov	ax, [si+bsMATLocation]
                mov	dx, [si+bsMATLocation+2]
                add	ax, [fsBootSector]
                adc	dx, [fsBootSector+2]
                mov	[CSFS_MAT_Address], ax
                mov	[CSFS_MAT_Address+2], dx
                mov	bx, MATBUFFER
                call	disk_read
                jc	short loc_delfs_stc_retn
loc_delsf_check_MAT_sign:
                mov	si, MATBUFFER
                cmp	word [si], 'MA'
                jne	loc_not_valid_fs_mat
                cmp	byte [si+2], 'T'
                jne	loc_not_valid_fs_mat
loc_delsf_read_SF_FDT:
                mov	ax, [StartupFile_FDT]
                mov	dx, [StartupFile_FDT+2]
                add	ax, [fsBootSector]
                adc	dx, [fsBootSector+2]
                mov	bx, SECBUFFER
                call	disk_read
                jc	short loc_delfs_stc_retn 
loc_check_FDT_sign:
                mov	si, SECBUFFER
                cmp	word [si], 'FD'
                jne	short loc_delsf_not_valid_fs_fdt
                cmp	byte [si+2], 'T'
                jne	short loc_delsf_not_valid_fs_fdt
                mov	ax, [si+fdtSectorCount]
                mov	dx, [si+fdtSectorCount+2]
                add	ax, 1
                adc	dx, 0
                or	dx, dx
                jz	short loc_delsf_save_csfs_sectorcount
loc_delsf_not_valid_fs_fdt:
loc_delsf_invalid_format:
                mov	ax, 0Bh ; Invalid format
                stc
loc_delfs_stc_retn:
                retn
loc_delsf_save_csfs_sectorcount:                 
                mov	[CSFS_SectorCount], ax  
                mov	[CSFS_SectorCount+2], dx
loc_delsf_set_fdt_deleted_sign:
                mov	byte [si+2], 'E'
                mov	ax, [mat_begin_sec]
                mov	dx, [mat_begin_sec+2]
                add	ax, [StartupFile_FDT]
                adc	dx, [StartupFile_FDT+2]  
                mov	bx, SECBUFFER               
                call	word [write_sector]
                jc	short loc_delfs_stc_retn
loc_delsf_update_DAT:
                xor	ax, ax
                xor	dx, dx 
                call	load_dat_sector
                jc	short loc_delfs_stc_retn

                mov	bl, 90h ; Free Sector sign
                mov	cx, [CSFS_SectorCount]
                or	cx, cx 
                jz	short loc_delsf_not_valid_fs_fdt
                mov	ax, [StartupFile_FDT]
                mov	dx, [StartupFile_FDT+2]
loc_delsf_update_DAT_loop:
                push	dx
                push	ax
                push	cx 
                call	update_dat
                pop	cx
                jnc	short loc_delsf_update_DAT_loop_next
                pop	dx ; pushed ax
                pop	dx
                jmp	short loc_delfs_stc_retn
loc_delsf_update_DAT_loop_next:
                pop	ax
                pop	dx 
                add	ax, 1
                adc	dx, 0
                loop	loc_delsf_update_DAT_loop

                cmp	byte [DAT_Buffer_Updated], 0 
                jna	short loc_delfs_update_MAT
                       
                mov	ax, [DAT_Buffer_Sector]
                mov	dx, [DAT_Buffer_Sector+2]
                call	write_dat_sector
                jc	short loc_delfs_stc_retn
loc_delfs_update_MAT:
                mov	ax, [StartupFile_FDT]
                mov	dx, [StartupFile_FDT+2]
                cmp	dx, [mat_dat_ffs+2]
                ja	short pass_delfs_update_MAT_ffs
                jb	short loc_delfs_update_MAT_ffs
                cmp	ax, [mat_dat_ffs]
                jnb	short pass_delfs_update_MAT_ffs
loc_delfs_update_MAT_ffs:
		mov	[mat_dat_ffs], ax
                mov	[mat_dat_ffs+2], dx
pass_delfs_update_MAT_ffs:
                mov	ax, [CSFS_SectorCount]  
                mov	dx, [CSFS_SectorCount+2]
                add	[mat_dat_free_s], ax
                adc	[mat_dat_free_s+2], dx

                mov	ax, [CSFS_MAT_Address]
                mov	dx, [CSFS_MAT_Address+2]
                mov	bx, MATBUFFER               
                call	word [write_sector]
                jnc	short loc_reset_fsbs_sf_pointers
                retn 
loc_reset_fsbs_sf_pointers:
                xor	ax, ax
                xor	dx, dx
                ; 27/01/2018
		mov	bx, BSBUFFER
		mov	[bx+bsStartupFD], ax
 		mov	[bx+bsStartupFD+2], dx 
                mov	[StartupFile_FDT], ax
                mov	[StartupFile_FDT+2], dx
   		mov	ax, [fsBootSector]
                mov	dx, [fsBootSector+2]
                call	word [write_sector]
                retn

display_startupfile_info:
		; 01/02/2010
		call 	clear_screen
                mov	ax, [StartupFile_FDT]
                mov	dx, [StartupFile_FDT+2]
                add	ax, [fsBootSector]
                adc	dx, [fsBootSector+2]
                mov	bx, SECBUFFER
                call	disk_read
                jnc	short loc_pdsf_check_FDT_sign
                retn  
loc_pdsf_check_FDT_sign:
                mov	si, SECBUFFER
                cmp	word [si], 'FD'
                jne	short loc_pdsf_not_valid_FDT
                add	si, 2
                cmp	byte [si], 'T'
                jne	short loc_pdsf_not_valid_FDT
loc_pdsf_print_fshd_name:
                cmp	byte [PhysicalDriveNumber], 80h
                jb	short loc_pdsf_print_fsfd_name
                mov	si, SINGLIX_HD_Name
                call	print_msg
                jmp	short loc_pdsf_print_sfn
loc_pdsf_print_fsfd_name:
                mov	si, SINGLIX_FD_Name
                call	print_msg
loc_pdsf_print_sfn:
                mov	si, Msg_StartupFile_Name
                call	print_msg
                mov	si, SECBUFFER
                push	si
                add	si, fdtFileName
                call	print_msg
                pop	si
                add	si, fdtFileSize
                mov	ax, [si]
                mov	dx, [si+2]
		;mov	si, Decimal_Str
                call	bin_to_decimal 
                mov	si, Str_startup_file_size
                call	print_msg
		mov	si, Decimal_Str
loc_pdsf_print_sfs_loop:
                cmp	byte [si], '0'
                jne	short loc_pdsf_print_sfs
                inc	si
                jmp	short loc_pdsf_print_sfs_loop
loc_pdsf_not_valid_FDT:
                mov	ax, 0Bh ; Invalid Format 
                stc
                retn
loc_pdsf_print_sfs:
                call	print_msg
                mov	si, Str_Bytes	
                call	print_msg
loc_pdsf_print_sfdatetime:                
                ; 13/02/2010
                mov	si, Str_startup_file_date_time
                call	print_msg
		mov	si, SECBUFFER
                add	si, fdtLMDate
                ;mov	al, [si]
                lodsb ; 27/01/2018
		call	bin_to_hex
                mov	[Sf_Year_Str], ax
                ;inc	si
                ;mov	al, [si]
                lodsb
		call	bin_to_hex
                mov	[Sf_Year_Str+2], ax
 		;inc	si
		;mov	al, [si]
                lodsb
		call	bin_to_hex
 		mov	[Sf_Month_Str], ax 
                ;inc	si
		;mov	al, [si]
                lodsb
		call	bin_to_hex
 		mov	[Sf_Day_Str], ax
                ;inc	si
                ;mov	al, [si]
                lodsb
		call	bin_to_hex       
		mov	[Sf_Hour_Str], ax
 		;inc	si
		mov	al, [si]
                call	bin_to_hex
		mov	[Sf_Minute_Str], ax                
		mov	si, Sf_Date_Time_Str
                call	print_msg              
           	retn  

;------------------------------------------------------------;
; RXDOS  32 bit Divide                                       ;
; (Special version by Erdogan Tan)                           ;
;------------------------------------------------------------;
;                                                            ;
; input -> DX_AX = 32 bit dividend                           ;
; input -> CX = 16 bit divisor                               ;
; output -> DX_AX = 32 bit quotient                          ;
; output -> BX = 16 bit remainder                            ;
;                                                            ;
;  This procedure divides the requested 32 bit number        ;
;  and gives the result in DX, AX and BX (remainder)         ;
;                                                            ;
; Original Procedure by Michael Podanoffsky / Real Time DOS  ;
; Erdogan TAN - 1999                        [ RXDOSBIO.ASM ] ;
;------------------------------------------------------------;

Div32:
		mov	bx, dx
		xchg	ax, bx
		xor	dx, dx
		div	cx	; at first, divide DX
		xchg	ax, bx	; remainder is in DX
				; now, BX has quotient
				; save remainder
		div	cx 	; so, DX_AX divided and
				; AX has quotient
				; DX has remainder
		xchg	dx, bx	; finally, BX has remainder

		retn

clear_screen:	;21/09/2009
		mov	ax, 0600h
		mov	bh, 7
		xor	cx, cx
		mov	dx, 184Fh
		int	10h
		mov	ah, 2
		xor	bh, bh
		xor	dx, dx
		int	10h

               ;2004-2005
               ;mov	ah, 0Fh 
               ;int	10h
               ;mov	ah, 0
               ;int	10h

                retn

;------------------------------------------------------------;
; From binary (byte) to hexadecimal (character) converter    ;
;                                                            ;
; input -> AL = byte (binary number) to be converted         ;
; output -> AH = First character of hexadecimal number       ;
; output -> AL = Second character of hexadecimal number      ;
;------------------------------------------------------------;
; Erdogan Tan - 1998, 1999

bin_to_hex:
		db	0D4h, 10h	; Undocumented inst. AAM
					; AH = AL / 10h
					; AL = AL MOD 10h
		or	ax, '00'	; Make it ZERO (ASCII) based

                xchg	ah, al 
; 1999
		cmp	al, '9'
		jna	short pass_cc_al
		add	al, 7
pass_cc_al:
		cmp	ah, '9'
		jna	short pass_cc_ah
		add	ah, 7
pass_cc_ah:
; 1998
		retn

;-----------------------------------------------------------------
;  DATA
;-----------------------------------------------------------------

FileHandle:	dw 0
fsBootSector:	dd 0
StartupFile_FDT: dd 0

Cursor_Pos:	dw 0
Cursor_Type:	dw 0

read_sector:	dw 0
write_sector:	dw 0

PhysicalDriveNumber: db 0
Physical_Disk_Heads: dw 0
Physical_Disk_SecPerTrack: dw 0

;DAT_Buffer_Drv: db 0
DAT_Identifier:	db 0
DAT_Buffer_Updated: db 0
DAT_Buffer_Sector: dd 0
DAT_Buffer_Offset: dw 0

CSectorCount:	dw 0
CSCounter:	dw 0
DAT_LastSector: dd 0
CFDT_Address:	dd 0
Current_FS_Sector: dd 0
CFDT_Address_MAX: dd 0
CSCounter_MAX:	dw 0

CSFS_SectorCount: dd 0
CSFS_MAT_Address: dd 0   

LBA_Ready: db 0

StartupFile_Name: times 66 db 0

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;  messages
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

SINGLIX_Welcome:
                db 0Dh, 0Ah
                db 'TR-SINGLIX FS1 Startup File Configuration Utility'
                db 0Dh, 0Ah
                db 'v2.0.280118  (c) Erdogan TAN 2010-2018'
                db 0Dh,0Ah
                db 0Dh,0Ah
                db 'Usage: bootfile [Drive] '
                db 0Dh,0Ah
                db 0Dh,0Ah
                db "Drive names:"
                db 0Dh,0Ah
                db 0Dh,0Ah
                db "fd0     (Floppy Disk 1, A:)", 0Dh, 0Ah
                db "fd1     (Floppy Disk 2, B:)", 0Dh, 0Ah
                db 0Dh, 0Ah
                db "hd01    (Hard Disk 1, partition 1)", 0Dh, 0Ah
                db "hd02    (Hard Disk 1, partition 2)", 0Dh, 0Ah
                db "...", 0Dh, 0Ah
                db "hd11    (Hard Disk 2, partition 1)", 0Dh, 0Ah
                db "...", 0Dh, 0Ah
                db "hd34    (Hard Disk 4, partition 4)", 0Dh, 0Ah
                db 0Dh, 0Ah
                db "hd0f    (Hard Disk 1, the first fs partition)", 0Dh, 0Ah
                db "hd0s    (Hard Disk 1, the second fs partition)", 0Dh, 0Ah
                db "...", 0Dh, 0Ah
                db "hd3f    (Hard Disk 4, the first fs partition)", 0Dh, 0Ah
                db "hd3s    (Hard Disk 4, the second fs partition)", 0Dh, 0Ah
                db 0

Msg_DoYouWantToWrite:
                db 07h
                db 0Dh, 0Ah
                db 'Do you want to write FS1 Startup File onto drive ', 0
SINGLIX_HD_Name:
                db 'hd'
SINGLIX_HD_Number:
                db '00: ', 0
msg_yes_no:
                db '(Yes/No)? ', 0

SINGLIX_FD_Name:
                db 'fd'
SINGLIX_FD_Number:
                db '0: ', 0

msg_singlix_drv_read_error:
                db 0Dh, 0Ah
                db 'Drive not ready or read error! Try again? (Y/N) '
                db 0Dh, 0Ah
                db 0

Msg_File_Not_Found:
                db 0Dh, 0Ah
                db "File not found !", 0

Msg_Not_Singlix_FS:
                db 0Dh, 0Ah
                db "Drive has not got a SINGLIX FS !", 0

Msg_startup_file_deleted:
                db 0Dh, 0Ah
                db "SINGLIX FS startup file deleted...", 0

Msg_writing_sf:
                db 0Dh, 0Ah
                db "Writing SINGLIX FS startup file...",0

Msg_DosFile_Name:
                db 0Dh, 0Ah
                db "DOS File Name : ", 0

Msg_StartupFile_Name:
                db 0Dh, 0Ah
                db "Startup File Name : ", 0

Msg_3dot_OK:    db "..."
Msg_OK:
                db ' OK.', 0Dh, 0Ah, 0

msg_YES:        db ' YES'
                db 0
msg_NO:         db ' NO'
                db 0   

SINGLIX_CRLF:
                db 0Dh, 0Ah, 0

msg_singlix_drv_write_error:
                db 0Dh, 0Ah
                db 'Drive not ready or write error! Try again? (Y/N) '
                db 0Dh, 0Ah
                db 0

msg_invalid_format:
                db 0Dh, 0Ah
                db 'Invalid FS descriptor format !', 0
                                       
Error_Code:     db 0

fsPartitionNumber: db 0

msg_Startup_File_Exists:
                db 0Dh, 0Ah, 0Dh, 0Ah
                db 'Startup File Exists ! '
msg_please_select_an_option:
               ;db 'Please select an option: '
                db 0Dh, 0Ah
                db 0Dh, 0Ah
                db 'Press <DELETE> to delete current startup file.'
		db 0Dh, 0Ah
                db 'Press <ENTER> to see current startup file information.'
		db 0Dh, 0Ah
                db 'Press <ESC> to exit.'
                db 0Dh, 0Ah, 0
                       
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;  buffers
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
BSBUFFER:
MasterBootBuff:
MasterBootCode: times 1BEh db 0
PartitionTable: times 64 db 0
MBIDCode: dw 0

MATBUFFER:
                db 0,0,0 ; db 'MAT'               
                db 0
mat_volume_size:dd 0    ; Volume Size 
mat_begin_sec:  dd 0    ; Volume Beginning Sector
mat_dat_lba:    dd 0    ; DAT LBA = 2
mat_dat_secs:   dd 0    ; DAT Sectors
mat_dat_free_s: dd 0    ; Free Sectors on DAT
mat_dat_ffs:    dd 0    ; First Free sector
                times 484 db 0     
DATBUFFER:
                times 512 db 0
END_OF_DATBUFFER:
FDTBUFFER:
                db 'FDT'; File Description Table
                db 0 ; FDT Version
                dw 512 ; Bytes per Sector
                dw 0  ; FDT Number
fdt_location:   dd 0  ; The First FDT Address
                dd 0  ; Next FDT number                 
fdt_scount:     dd 0  ; Sector Count
                dd 0  ; Directory DT Address
                dd 0  ; Directory Serial Number  
fdt_file_size:                
                dd 0  ; File Size
                dw 0  ; 
                db 0  ; Attributes
                db 0  ; Extended Attributes
                dd 0
                dd 0
		db 0  ; Filename Type
		db 0  ; Longname Length	 		
                ; Offset 46
                db 0 ; Country
                db 0 ; Time Zone (+11)
                ; Offset 48
fdt_make_year:
                dw 0 ; Creating Year
fdt_make_month:
                db 0 ; Creating Month
fdt_make_day:
                db 0 ; Creating Day
fdt_make_hour:
                db 0 ; Creating Hour
fdt_make_minute:
                db 0 ; Creating Minute
fdt_make_second:
                db 0 ; Creating Second
fdt_make_dlstm:
                db 0 ; Daylight Saving Time Mode (0= standard time)
                ; Offset 56
fdt_lm_year:
                dw 0 ; Last Mofication Year
fdt_lm_month:
                db 0 ; Last Modification Month
fdt_lm_day:
                db 0 ; Last Modification Day
fdt_lm_hour:
                db 0 ; Last Modification Hour
fdt_lm_minute:
                db 0 ; Last Modification Minute
fdt_lm_second:
                db 0 ; Last Modification Second
fdt_lm_dlstm:
                db 0 ; Daylight Saving Time Mode (0= standard time)

                ; Offset 64
fdt_file_name:
		times 64 db 0
                ; Offset 128
		times 128 db 0
                ; Offset 256
                times 256 db 0
    
; End Of FDT Buffer

SECBUFFER:
		times 512 db 0

RetryCount:     dw 0

Str_startup_file_size:
                db 0Dh, 0Ah, 0Dh, 0Ah
                db 'Startup File Size : ', 0
Str_Bytes:
                db ' bytes', 0

Decimal_Str:    times 10 db  0

Str_startup_file_date_time:
                db 0Dh, 0Ah
                db 'LM Date & Time    : ', 0
Sf_Date_Time_Str:
Sf_Day_Str:	db '00'
                db '/'
Sf_Month_Str:	db '00'
                db '/'
Sf_Year_Str:	db '0000'
                db 20h, 20h
Sf_Hour_Str: 	db '00'
                db ':'
Sf_Minute_Str:  db '00'
                db 0

bootfile_CopyRight:
                db  '(c) Erdogan TAN - 28/01/2018'

                db  0

SizeOfFile      equ $-100