; ****************************************************************************
; Turkish Rational SINGLIX  
; Disk Operation System v1.0 Project
; Singlix FS1 (A1h) File System STARTUP FILE Configuration Code : BOOTFILE.COM
;
; Copyright (C) 2010  Erdogan TAN   5/2/2010, 3/2/2010, 2/2/2010, 3/1/2010
; HDFORMAT.ASM [ 03/01/2010 ]
;
; Last Update: 21/02/2010
;
; ****************************************************************************

; Masterboot / Partition Table at Beginning+1BEh
ptBootable       equ 0
ptBeginHead      equ 1
ptBeginSector    equ 2
ptBeginCylinder  equ 3
ptFileSystemName equ 4
ptEndHead        equ 5
ptEndSector      equ 6
ptEndCylinder    equ 7
ptStartSector    equ 8
ptSectors        equ 12

; TR-SINGLIX FS1 BootSector Identification (Data) Block
; 19-05-2009
bsFSystemID equ 3
bsBytesPerSec equ 6
bsMediaAttrib equ 8
bsPartitionID equ 9
bsFSVersionMaj equ 10
bsFSVersionMin equ 11
bsBootSector equ 12 
bsVolumeSize equ 16
bsStartupFD equ 20
bsMATLocation equ 24
bsRootDirD equ 28
bsSystemConfFD equ 32
bsSwapFD equ 36
bsUndeleteDirD equ 40
bsDriveNumber equ 44
bs_LBA_Ready equ 45
bsMagicWord equ 46
bs_Disk_SecPerTrack equ 46
bs_Disk_Heads equ 47 
bsOperationSys equ 48
bs_terminator equ 64

; DTA (PSP+80h= Offset 128)
DTA_Time equ 150 ; PSP+22
DTA_Date equ 152 ; PSP 24
DTA_FileSize equ 154 ; PSP + 26
DTA_FileName equ 158 ; PSP + 30

; MAT
MAT_DAT_Address equ 12
MAT_SectorCount equ 16
MAT_FreeSectors equ 20
MAT_FirstFreeSector equ 24

; FDT
fdtSectorCount equ 16
fdtFileSize equ 28
fdtLMDate equ 56
fdtLMTime equ 60
fdtFileName equ 64

.8086

SINGLIXBOOT     SEGMENT PUBLIC 'CODE'
                assume cs:SINGLIXBOOT,ds:SINGLIXBOOT,es:SINGLIXBOOT,ss:SINGLIXBOOT

                org 100h

START_CODE:

proc_start      proc near

                mov bx, SizeOfFile+100
                add bx, 15
                shr bx, 1
        	shr bx, 1
		shr bx, 1
		shr bx, 1
                mov ah, 4Ah ; modify memory allocation
               ;push cs
               ;pop es
                int 21h 
                                  
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; see if drive specified
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
              
                mov si, offset 80h                      ; PSP command tail
                mov cl, byte ptr [SI]
                or cl, cl                               
                jz WRITESTUB_12             ; jump if zero

WRITESTUB_06:
                inc si
                mov al, byte ptr [SI]
                cmp al, ' '                      ; is it SPACE ?
                jne short WRITESTUB_08

                dec cl                                  
                jne short WRITESTUB_06                  
                jmp WRITESTUB_12

WRITESTUB_08:
                cmp al, "f"
                jne WRITESTUB_08h
                inc si
                mov al, byte ptr [SI]
                cmp al, "d"
                jne WRITESTUB_12
                inc si
                mov al, byte ptr [SI]
                cmp al, '0'                            
                jb WRITESTUB_12
                cmp al, '1'
                ja WRITESTUB_12
                mov byte ptr [SINGLIX_FD_Number], al
                mov dl, al
                sub dl, '0'
                mov byte ptr [PhysicalDriveNumber], dl

                mov ah, 08h
                int 13h
                ; ES <> DS
                jc  WRITESTUB_30
                inc dh
                mov byte ptr [Physical_Disk_Heads], dh
                and cl, 00111111b
                mov byte ptr [Physical_Disk_SecPerTrack], cl
              
               ;mov byte ptr [Physical_Disk_Heads], 2
               ;mov byte ptr [Physical_Disk_SecPerTrack], 18

               ;mov byte ptr [LBA_ready], 0
               
               ;push ds
               ;pop es

		mov dl, byte ptr [PhysicalDriveNumber]

                call proc_load_masterboot

               ;mov si, offset BSBUFFER
                mov si, offset MasterBootBuff

                cmp word ptr [SI][bsFSystemID], 'SF'
                je  short loc_check_fd_bs_A1h_sign

loc_not_singlix_fs_fd:
                mov si, offset msg_Not_Singlix_FS
		call SINGLIX_PRINTMSG

                jmp WRITESTUB_60

loc_check_fd_bs_A1h_sign:
                cmp byte ptr [SI][bspartitionID], 0A1h
                jne short loc_not_singlix_fs_fd

                mov ax, offset proc_write_chs_sector  
                mov word ptr [proc_write_sector], ax
               
                jmp WRITESTUB_10

WRITESTUB_08h:
                cmp al, "h"
                jne WRITESTUB_12
                inc si
                mov al, byte ptr [SI]
                cmp al, "d"
                jne WRITESTUB_12
                inc si
                mov al, byte ptr [SI]
                cmp al, '0'                            
                jb WRITESTUB_12
                cmp al, '3'
                ja WRITESTUB_12
                mov byte ptr [SINGLIX_HD_Number], al
                inc si
                mov ax, word ptr [SI]
                cmp ah, 20h
                ja WRITESTUB_12
                cmp al, 'f'
                je short WRITESTUB_09
                cmp al, 's'
                je short WRITESTUB_09                   
                cmp al, '1'
                jb WRITESTUB_12
                cmp al, '4'
                ja WRITESTUB_12

WRITESTUB_09:
                mov byte ptr [SINGLIX_HD_Number]+1, al

                mov dl, byte ptr [SINGLIX_HD_Number]
                add dl, 80h-'0'

                mov byte ptr [PhysicalDriveNumber], dl

WRITESTUB_09_check_int13h_extensions:
               ; 05/01/2010 
                mov ah, 41h ; Check INT 13h Extensions Present
                mov bx, 55AAh
                int 13h
                jc short WRITESTUB_09_lba_not_ready
                cmp bx, 0AA55h
                jne short WRITESTUB_09_lba_not_ready

                and cl, 1                   ; Fixed disk access subset check
                jz short WRITESTUB_09_lba_not_ready 

WRITESTUB_09_lba_ready:
                mov ax, offset proc_write_lba_sector
               ;mov byte ptr [LBA_ready], cl ; 1
                inc byte ptr [LBA_ready]
                jmp short WRITESTUB_09_set_disk_write_procedure  

WRITESTUB_09_lba_not_ready:
               ;mov byte ptr [LBA_ready], cl ; 0
                mov ah, 08h
                ;mov dl, byte ptr [PhysicalDriveNumber]
                int 13h
                ; ES <> DS
                jc  WRITESTUB_30
                inc dh
                mov byte ptr [Physical_Disk_Heads], dh
                and cl, 00111111b
                mov byte ptr [Physical_Disk_SecPerTrack], cl

                mov ax, offset proc_write_chs_sector  

WRITESTUB_09_set_disk_write_procedure:
                mov word ptr [proc_write_sector], ax

WRITESTUB_09_load_masterboot:
               ;push ds
               ;pop es 

                mov dl, byte ptr [PhysicalDriveNumber]

                call proc_load_masterboot
                jc WRITESTUB_30

                mov si, offset PartitionTable
                mov al, byte ptr [SINGLIX_HD_Number]+1
                cmp al, "f"
                jne short pass_check_first_singlix_partition
                xor al, al
                mov cx, 4
loc_check_fs_f_partition:
                inc al
                cmp byte ptr [SI][ptFileSystemName], 0A1h
                jne short check_for_first_fs_partition_again
loc_fs_partition_found:
                mov byte ptr [fsPartitionNumber], al 
                add al, '0'
                mov byte ptr [SINGLIX_HD_Number]+1,al
                jmp short WRITESTUB_11
check_for_first_fs_partition_again:
                add si, 10h
                loop loc_check_fs_f_partition

                jmp WRITESTUB_30

pass_check_first_singlix_partition:
                cmp al, "s"
                jne short pass_check_second_singlix_partition
                xor al, al 
                mov cx, 4
loc_check_fs_s_partition:  
                cmp byte ptr [SI][ptFileSystemName], 0A1h
                jne short check_for_second_fs_partition_again
                inc al
                cmp al, 2
                jne  short check_for_second_fs_partition_again
                jmp short loc_fs_partition_found
check_for_second_fs_partition_again:
                add si, 10h
                loop loc_check_fs_s_partition

                jmp WRITESTUB_30

pass_check_second_singlix_partition :
                sub al, '1'
                mov ah, 10h
                mul ah   
                add si, ax
                cmp byte ptr [SI][ptFileSystemName], 0A1h
                jne WRITESTUB_30
WRITESTUB_11:
                mov ax, word ptr [SI][ptStartSector] 
                mov dx, word ptr [SI][ptStartSector]+2
                mov word ptr [fsBootSector], ax
                mov word ptr [fsBootSector]+2, dx

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Write message
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

WRITESTUB_10:
                mov si, offset Msg_DoYouWantToWrite
                call SINGLIX_PRINTMSG
               
                cmp byte ptr [PhysicalDriveNumber], 80h
                jnb short WRITESTUB_13
                mov si, offset SINGLIX_FD_Name
                call SINGLIX_PRINTMSG
                jmp short WRITESTUB_13_yn
WRITESTUB_13:   
                mov si, offset SINGLIX_HD_Name
                call SINGLIX_PRINTMSG
WRITESTUB_13_yn: 
		mov si, offset msg_yes_no
                call SINGLIX_PRINTMSG
WRITESTUB_14:
                xor ax, ax
                int 16h                                 ; wait for keyboard command
                cmp al, 'C'-40h
                je WRITESTUB_60                   
                cmp al, 27
                je WRITESTUB_60
                and al, 0DFh
                cmp al, 'Y'                             ; Yes?
                je short WRITESTUB_20_Y                 ; write
                cmp al, 'N'                             ; No?
                je WRITESTUB_60_N                         ; no write (exit)

                jmp short WRITESTUB_14

WRITESTUB_12:
                mov si, offset SINGLIX_Welcome
                call SINGLIX_PRINTMSG
                jmp WRITESTUB_60

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; get drive parameters
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

WRITESTUB_20_Y:
                mov si, offset msg_YES
                call SINGLIX_PRINTMSG

loc_load_fs_boot_sector:
                cmp byte ptr [PhysicalDriveNumber], 80h
                jb short loc_check_current_sfdt_adress_fd

                mov ax, word ptr [fsBootSector]
                mov dx, word ptr [fsBootSector]+2

 		mov bx, offset BSBUFFER
                call proc_disk_read
                jc WRITESTUB_30

                mov si, offset BSBUFFER
                
                cmp word ptr [SI][bsFSystemID], 'SF'
                jne short loc_not_singlix_fs_hdp

loc_check_hdp_bs_A1h_sign:
                cmp byte ptr [SI][bspartitionID], 0A1h
                je short loc_check_current_sfdt_adress

loc_not_singlix_fs_hdp:
                mov si, offset msg_Not_Singlix_FS
		call SINGLIX_PRINTMSG

                jmp WRITESTUB_60

loc_check_current_sfdt_adress_fd:
                mov si, offset BSBUFFER  
loc_check_current_sfdt_adress:
                mov ax, word ptr [SI][bsStartupFD]
		mov dx, word ptr [SI][bsStartupFD]+2
                or ax, ax
                jnz short loc_startup_file_exists
                or dx, dx
                jnz short loc_startup_file_exists                  
WRITESTUB_20:
                mov si, offset Msg_DosFile_Name
                call SINGLIX_PRINTMSG
                call proc_rw_char
                jc short pass_write_file_name_chr0
                mov di, offset StartupFile_Name
                cmp byte ptr [SI], 20h
                jna short pass_write_file_name_chr0
                mov cx, 64
loc_write_file_name_chr0:
                lodsb
                cmp al, 20h
                jnb short loc_write_file_name_chr1 
pass_write_file_name_chr0:
                mov byte ptr [DI], 0
                inc di
                loop pass_write_file_name_chr0
                jmp short loc_find_dos_file
loc_write_file_name_chr1:
                mov byte ptr [DI], al
                inc di
                loop loc_write_file_name_chr0
loc_find_dos_file:
		mov si, offset SINGLIX_CRLF
 		call SINGLIX_PRINTMSG
                mov dx, offset StartupFile_Name
                mov cx, 27h ; File Attributes
                mov ah, 4Eh ; MS Dos Function = Find First File
                int 21h
                jnc short loc_open_dos_file_1
                cmp ax, 2
                jne WRITESTUB_30

                jmp loc_dos_file_notfound

loc_startup_file_exists:
                mov word ptr [StartupFile_FDT], ax
                mov word ptr [StartupFile_FDT]+2, dx
                mov si, offset msg_Startup_file_exists
                call SINGLIX_PRINTMSG
loc_startup_file_exist_select_option:
                xor ah, ah
                int 16h
                cmp al, 1Bh
                je WRITESTUB_60
                cmp al, 0Dh
                jne short loc_check_startupfile_delete_decision

                call proc_display_startupfile_info
                mov si, offset msg_please_select_an_option
                call SINGLIX_PRINTMSG
                jmp short loc_startup_file_exist_select_option 
   
loc_check_startupfile_delete_decision:
                cmp ah, 53h
                jne short loc_startup_file_exist_select_option

                mov si, offset BSBUFFER 

                call proc_delete_fs_startupfile
                jnc short loc_startup_file_deleted_msg

                cmp al, 0Bh
                jne WRITESTUB_30
                
                mov si, offset msg_invalid_format
                call SINGLIX_PRINTMSG
		jmp WRITESTUB_60

loc_startup_file_deleted_msg:
                mov si, offset msg_startup_file_deleted
                call SINGLIX_PRINTMSG
               ;jmp WRITESTUB_20 
                jmp WRITESTUB_60

loc_open_dos_file_1:
                mov si, offset DTA_FileName
                mov di, offset StartupFile_Name
                mov cx, 64
loc_copy_found_file_name_chr0:
                lodsb
                cmp al, 20h
                jnb short loc_copy_found_file_name_chr1 
pass_copy_found_file_name_chr0:
                mov byte ptr [DI], 0
                inc di
                loop pass_copy_found_file_name_chr0
                jmp short loc_set_startupfile_decriptor_table
loc_copy_found_file_name_chr1:
                mov byte ptr [DI], al
                inc di
                loop loc_copy_found_file_name_chr0

loc_set_startupfile_decriptor_table:
                mov 	si, offset DTA_FileSize
                mov     ax, word ptr [SI]
                mov 	dx, word ptr [SI]+2
		mov 	word ptr [fdt_File_Size], ax
		mov 	word ptr [fdt_File_Size]+2, dx


                mov     si, offset DTA_Date
                mov 	ax, word ptr [SI]
                push 	ax
                and 	ax, 00011111b	         ; Day Mask
		aam                              ; Q([AL]/10)->AH
						 ; R([AL]/10)->AL
						 ; [AH]+[AL]= Day as BCD
                shl 	ah, 1
                shl 	ah, 1
                shl 	ah, 1
                shl 	ah, 1 
                add 	ah, al     
		mov 	byte ptr [fdt_make_day], ah
                pop 	ax
                push 	ax 
		mov     cl, 5
		shr     ax, cl                   ; shift right 5 times
		and     ax, 00001111b            ; Month Mask
  		shl 	ah, 1
                shl 	ah, 1
                shl 	ah, 1
                shl 	ah, 1 
		aam
                add 	ah, al     
		mov 	byte ptr [fdt_make_month], ah
                pop 	ax
		mov     cl, 9
		shr     ax, cl
	       ;and     ax, 01111111b            ; Result = Year - 1980
                add     ax, 1980
                xor     dx, dx
                mov     cx, 100
                div     cx                
                aam
  		mov     cl, 4
                shl 	ah, cl
                add 	ah, al     
                mov     byte ptr [fdt_make_year], ah
 		mov 	al, dl
                aam
  	       ;mov     cl, 4
                shl 	ah, cl
		add 	ah, al      
                mov     byte ptr [fdt_make_year]+1, ah
                
                mov     si, offset DTA_Time
                mov 	ax, word ptr [SI]
                push 	ax
		and     ax, 0000111111b          ; Second Mask
		aam
  	       ;mov     cl, 4
                shl 	ah, cl
		add     ah,al
                mov     byte ptr [fdt_make_second], ah
                pop 	ax
                push 	ax
		mov     cl, 5
		shr     ax, cl                   ; shift right 5 times
		and     ax, 0000111111b          ; Minute Mask
		aam
  		mov     cl, 4
                shl 	ah, cl
		add     ah,al
                mov     byte ptr [fdt_make_minute], ah
                pop 	ax
                shr   	al, 1
		shr     al, 1
		shr     al, 1                    ; ax = hours
		aam
  	       ;mov     cl, 4
                shl 	ah, cl 
		add	ah, al
                mov     byte ptr [fdt_make_hour], ah
           
                mov ah, 02h                             ; Return Current Time
                int 1Ah
                xchg ch,cl
                mov word ptr [fdt_lm_hour], cx
                xchg dh,dl
                mov word ptr [fdt_lm_second], dx
                mov word ptr [fdt_lm_hour], cx
                mov word ptr [fdt_lm_second], dx
                
                mov ah, 04h                             ; Return Current Date
                int 1Ah
                xchg ch,cl
                mov word ptr [fdt_lm_year], cx
                xchg dh,dl
                mov word ptr [fdt_lm_month], dx

loc_open_dos_file_2:
		mov dx, offset StartupFile_Name
                mov ah, 3Dh ; MS Dos Function = Open File
                xor al, al  
                int 21h
                jnc short loc_save_filehandle
	        cmp ax, 2
                jne WRITESTUB_30                

loc_dos_file_notfound:
                mov si, offset Msg_File_Not_Found
                call SINGLIX_PRINTMSG

                jmp WRITESTUB_60

loc_save_filehandle:
		mov word ptr [FileHandle], ax
loc_display_startup_file_name:
                mov si, offset Msg_StartupFile_Name
                call SINGLIX_PRINTMSG
                mov ah, 03h
               ;xor bh, bh
                int 10h
               ;push dx         
                mov si, offset StartupFile_Name
                call SINGLIX_PRINTMSG
               ;pop dx
                mov ah, 02h
               ;xor bh, bh
                int 10h
                call proc_rw_char
                jc WRITESTUB_60
                cmp byte ptr [SI], 20h
                jna WRITESTUB_60
                mov di, offset fdt_File_Name
                mov cx, 64
loc_rename_file_name_chr0:
                lodsb
                cmp al, 20h
                jnb short loc_rename_file_name_chr1 
pass_rename_file_name_chr0:
                mov byte ptr [DI], 0
                inc di
                loop pass_rename_file_name_chr0
                jmp short loc_get_SFDT_Address

loc_rename_file_name_chr1:
                mov byte ptr [DI], al
                inc di
                loop loc_rename_file_name_chr0

loc_get_SFDT_Address:
                mov si, offset SINGLIX_CRLF
 		call SINGLIX_PRINTMSG
                mov ax, word ptr [fdt_File_Size]
                mov dx, word ptr [fdt_File_Size]+2
                add ax, 511
                adc dx, 0
                mov cx, 512
                call RxDOS_Div32
                mov word ptr [fdt_SCount], ax
                mov word ptr [fdt_SCount]+2, dx
                add ax, 1
                adc dx, 0
                or dx, dx
                ja WRITESTUB_30

                mov cx, ax
                mov si, offset BSBUFFER

                call proc_get_first_free_section
                jc WRITESTUB_30

                mov word ptr [StartupFile_FDT], ax
                mov word ptr [StartupFile_FDT]+2, dx 

loc_write_SF:
                mov si, offset Msg_writing_sf
                call SINGLIX_PRINTMSG

                mov ax, word ptr [StartupFile_FDT]
                mov dx, word ptr [StartupFile_FDT]+2
                add ax, word ptr [fsBootSector]
                adc dx, word ptr [fsBootSector]+2

                mov bx, offset FDTBUFFER     
                call word ptr [proc_write_sector]
                jc WRITESTUB_30

                mov ax, word ptr [StartupFile_FDT]
                mov dx, word ptr [StartupFile_FDT]+2
                mov si, offset BSBUFFER
		mov word ptr [SI][bsStartupFD], ax
 		mov word ptr [SI][bsStartupFD]+2, dx 
              
                add ax, 1
                adc dx, 0
 		
		mov word ptr [StartupFile_FDT], ax
                mov word ptr [StartupFile_FDT]+2, dx    

WRITESTUB_38_rp:                
                mov ah, 3Fh ; Read File
                mov cx, 512
                mov dx, offset SECBUFFER
                mov bx, word ptr [FileHandle]
                int 21h
                jc short WRITESTUB_38_c

                push ax
                mov ax, word ptr [StartupFile_FDT]
                mov dx, word ptr [StartupFile_FDT]+2
                add ax, word ptr [fsBootSector]
                adc dx, word ptr [fsBootSector]+2

                mov bx, offset SECBUFFER     
                call word ptr [proc_write_sector]
                pop ax
                jc WRITESTUB_38_c
                
                cmp ax, 512
                jne short WRITESTUB_38_cmc

                xor dx, dx
                mov ax, word ptr [StartupFile_FDT]
                add ax, 1
                adc dx, word ptr [StartupFile_FDT]+2

 		mov word ptr [StartupFile_FDT], ax
                mov word ptr [StartupFile_FDT]+2, dx    
               
                jmp short WRITESTUB_38_rp

WRITESTUB_38_cmc:
                cmc 
WRITESTUB_38_c:
	        pushf
                mov ah, 3Eh ; Close File
                mov bx, word ptr [FileHandle]
                int 21h
                popf 
                jc WRITESTUB_30

  	       ;mov si, offset BSBUFFER
		mov ax, word ptr [SI][bsStartupFD]
 		mov dx, word ptr [SI][bsStartupFD]+2 
                mov word ptr [StartupFile_FDT], ax
                mov word ptr [StartupFile_FDT]+2, dx

		mov bl, 06h
                call proc_update_dat
                jc WRITESTUB_70
                mov cx, word ptr [fdt_Scount]                
WRITESTUB_38_loop:
                mov ax, word ptr [StartupFile_FDT]
                mov dx, word ptr [StartupFile_FDT]+2                
                add ax, 1
                adc dx, 0
 		mov word ptr [StartupFile_FDT], ax
                mov word ptr [StartupFile_FDT]+2, dx
                push cx
                mov bl, 07h
                call proc_update_dat
                pop cx  
                jc short WRITESTUB_70
                loop WRITESTUB_38_loop

                cmp byte ptr [DAT_Buffer_Updated], 0
                jna short WRITESTUB_38_um
                
                mov ax, word ptr [DAT_Buffer_Sector]
                mov dx, word ptr [DAT_Buffer_Sector]+2
                call proc_write_DAT_sector
                jc short WRITESTUB_70

WRITESTUB_38_um:
                mov si, offset BSBUFFER
                mov cx, 2 ; FDT(DDT) + 1 sector data
                call proc_get_first_free_section
                jnc short WRITESTUB_38_wm
                ; Invalid data => 0FFFFFFFFh sign
                xor ax, ax
                dec ax 
                mov dx, ax
WRITESTUB_38_wm:
                mov cx, word ptr [fdt_Scount]
                inc cx
                sub word ptr [mat_dat_free_s], cx
                sbb word ptr [mat_dat_free_s]+2, 0
                mov word ptr [mat_dat_ffs], ax
                mov word ptr [mat_dat_ffs]+2, dx
 	       ;mov bx, offset MATBUFFER
                mov bx, si
                mov si, offset BSBUFFER
                mov ax, word ptr [SI][bsMATLocation]
 		mov dx, word ptr [SI][bsMATLocation]+2 
                add ax, word ptr [fsBootSector]
                adc dx, word ptr [fsBootSector]+2
                call word ptr [proc_write_sector]
		jc short WRITESTUB_70 

WRITESTUB_38_bs:
   		mov bx, offset BSBUFFER
		mov ax, word ptr [fsBootSector]
                mov dx, word ptr [fsBootSector]+2
                call word ptr [proc_write_sector]
		jc short WRITESTUB_70

WRITESTUB_38:        
                mov si, offset Msg_OK
                call SINGLIX_PRINTMSG

WRITESTUB_60:
                mov si, offset SINGLIX_CRLF
                call SINGLIX_PRINTMSG

                int 20h
                
                int 19h 
WRITESTUB_70:  
                mov si, offset msg_singlix_drv_write_error
                call SINGLIX_PRINTMSG
                jmp short WRITESTUB_40

WRITESTUB_60_N:
                mov si, offset msg_NO
                call SINGLIX_PRINTMSG
                jmp short WRITESTUB_60 

WRITESTUB_30:
                mov si, offset msg_singlix_drv_read_error
                call SINGLIX_PRINTMSG

WRITESTUB_40:
                xor ax, ax
                int 16h                           ; wait for keyboard command
                cmp al, 'C'-40h
                je short WRITESTUB_60                   
                cmp al, 27
                je short WRITESTUB_60
                and al, 0DFh
                cmp al, 'Y'
                je short WRITESTUB_50             ; Retry
                cmp al, 'N'
                je short WRITESTUB_60             ; Exit
                jmp short WRITESTUB_40

WRITESTUB_50:
                mov dl, byte ptr [PhysicalDriveNumber]
                mov ah, 08h
                int 13h                              ; return disk parameters
                push cs
                pop  es                              ; restore es
                jc WRITESTUB_30

                cmp bl, 04                           ; Drive Type
                jb WRITESTUB_30

                xor ah,ah
                mov dl, byte ptr [PhysicalDriveNumber]
                int 13h 
                jc WRITESTUB_30  

                jmp loc_load_fs_boot_sector

FileHandle: dw 0
fsBootSector: dd 0
StartupFile_FDT: dd 0

proc_start     endp

proc_bin_to_decimal proc near
               ; 6-5-2009
               ;  Erdogan Tan
               ; INPUT: DS:SI = Target location
               ;        DX:AX = Binary Number
               ; OUTPUT: Decimal chars at DS:SI
               ; CX, AX, DX, SI, BX, BP will be changed.

                push bp
                mov si, offset Decimal_Str
               ;mov word ptr [Str_Number_Input], SI
                push si
                mov cx, 9
loc_reset_str_NumberInput:
                mov byte ptr [SI], "0"
                inc si
                loop loc_reset_str_NumberInput
                mov bp, sp
                mov cl, 10
loc_rediv_NumberInput:
                call RXDOS_Div32
                add bl,'0'
                push bx
                dec si
                cmp ax, 0
                ja short loc_rediv_NumberInput
                cmp dx, 0
                ja short loc_rediv_NumberInput
               ;mov SI, word ptr [Str_Number_Input]
loop_popbx_NumberInput: 
                pop bx
                mov byte ptr [SI],bl
                inc si
                cmp bp, sp
                jne short loop_popbx_NumberInput
                pop si
                pop bp  
 
                retn

;Str_Number_Input: dw 0

proc_bin_to_decimal endp

Cursor_Pos: dw 0
Cursor_Type: dw 0


SINGLIX_PRINTMSG     proc near
 
SINGLIX_PRINTMSG_LOOP:
                lodsb                           ; Load byte at DS:SI to AL
                and     AL,AL            
                jz      short SINGLIX_PRINTMSG_OK       
                mov     AH,0Eh                  
                mov     BX,07h             
                int     10h                     ; BIOS Service func ( ah ) = 0Eh
                                                ; Write char as TTY
                                                ;AL-char BH-page BL-color
                jmp     short SINGLIX_PRINTMSG_LOOP           

SINGLIX_PRINTMSG_OK:
                retn

SINGLIX_PRINTMSG     endp

proc_CHS_read proc near
                ; 31/01/2010
                ; INPUT -> DX:AX = Logical Block Address
                ; ES:BX = Destination Buffer
                ; OUTPUT -> clc or stc

                push    SI
                push    CX            
loc_read_disk_chs:
                mov     DI,5   
                push    AX                      ; Linear sector #
                push    DX                      ; DX_AX = Linear address (sectors)
                mov     CX, word ptr [Physical_Disk_SecPerTrack]
                push    BX

                call    RXDOS_DIV32            ; Special 32 bit divide !!!
                                                ; To fix large disk problem.
                                                ; (c) Erdogan Tan 1999
                                                ; (October 20th, 1999)

                mov     CX, BX                  ; Sector (zero based)
                inc     CX                      ; To make it 1 based
                push    CX
                mov     CX, word ptr [Physical_Disk_Heads]
                call    RXDOS_DIV32            ; Convert track to head & cyl
                mov     DH, BL                  ; BX = Head (max. FFh)
                pop     CX
                                                ; AX=Cyl, DH=Head, CX=Sector
                pop     BX                      ; ES:BX = Buffer

                mov     DL, byte ptr [PhysicalDriveNumber]
                mov     CH,AL                   
                ror     AH,1                    ; Rotate right
                ror     AH,1                   
                or      CL,AH                   
                mov     AX,0201h
                int     13h                     ; BIOS Service func ( ah ) = 2
                                                ; Read disk sectors
                                                ;AL-sec num CH-track CL-sec
                                                ; DH-head DL-drive ES:BX-buffer
                                                ;CF-flag AH-stat AL-sec read
                                                ; If CF = 1 then (If AH > 0)
                pop     DX
                pop     AX
                
                jnc     short pass_read_disk_chs_error              
                dec     DI                      
               ;jz      short pass_read_disk_chs_error     
                
               ;xor     AH,AH                   
               ;mov     DL, byte ptr [PhysicalDriveNumber]
               ;int     13h                     ; BIOS Service func ( ah ) = 0
                                                ; Reset disk system
                jnz     short loc_read_disk_chs                  
               ;jmp     short loc_read_disk_chs          

pass_read_disk_chs_error:
                pop     CX
                pop     SI
                retn                            ; db 0C3h

proc_CHS_read   endp

proc_LBA_read   proc near
                ; 21/02/2010
                ; 31/01/2010
                ; INPUT -> DX:AX = LBA address
                ; INPUT -> ES:BX = Buffer

                mov DI, 5 

loc_read_disk_lba:
               ;pusha                           ; db 60h
                db 60h

               ;push    0                       ; db 6Ah, 00h
                db 6Ah, 0
               ;push    0                       ; db 6Ah, 00h
                db 6Ah, 0
                push    dx
                push    ax
                push    es
                push    bx
               ;push    1                       ; db 6Ah, 01h
                db 6Ah, 01h                     
               ;push    10h                     ; db 6Ah, 10h
                db 6Ah, 10h
                mov SI, SP
                ; DS:SI= DAP Location
                mov ah, 42h  ; Extended Disk Read - LBA Read
                mov dl, byte ptr [PhysicalDriveNumber]
                int 13h

               ;popa
                db 61h
               ;popa
                db 61h

                jnc short pass_read_disk_lba_error
                dec DI 
                jnz short loc_read_disk_lba

pass_read_disk_lba_error:
                retn

proc_LBA_read   endp

proc_disk_read proc near
                ; 31/01/2010
                ; INPUT -> DX:AX = LBA address
                ; INPUT -> ES:BX = Buffer
                cmp byte ptr [LBA_Ready], 0
                ja short loc_read_lba_sectors
loc_read_chs_sectors:
                call proc_chs_read
                jnc short retn_read_sectors
retn_read_sectors_stc:
                mov ax, 15h ; Drv not ready or read error !
retn_read_sectors:
                retn
loc_read_lba_sectors:
                call proc_lba_read
                jc  short retn_read_sectors_stc

                retn

proc_disk_read endp


;#############################################################################
;#
;#              PROCEDURE proc_write_lba_sector
;#
;#############################################################################

; 31/01/2010
; (c) Erdogan Tan  [01/05/2009] 

proc_write_lba_sector   proc    near

loc_write_lba_sectors:                
                mov DI, 5
               
loc_0FFh:
               ;pusha                           ; db 60h
                db 60h

               ;push    0                       ; db 6Ah, 00h
                db 6Ah, 0
               ;push    0                       ; db 6Ah, 00h
                db 6Ah, 0
                push    dx
                push    ax
                push    es
                push    bx
               ;push    1                       ; db 6Ah, 01h
                db 6Ah, 01h                     
               ;push    10h                     ; db 6Ah, 10h
                db 6Ah, 10h

                mov     DL, byte ptr [PhysicalDriveNumber] 
                mov     AH, 43h
                xor     al, al                  ; Verify off
                mov     SI, SP
                int     13h

               ;popa
                db 61h
               ;popa
                db 61h

                jnc     short loc_12Bh
                
                dec     DI 

               ; jz      short loc_12Bh
               
                jnz     short loc_0FFh  
               ; xor     AH, AH
               ; mov     DL, byte ptr [PhysicalDriveNumber] 
               ; int     13h
                
               ; jmp     short loc_0FFh                  
 
loc_12Bh:
                retn                            ; db 0C3h
                        
proc_write_lba_sector   endp

proc_read_sector: dw 0
proc_write_sector: dw 0

;#############################################################################
;#
;#              PROCEDURE proc_write_chs_sector
;#
;#############################################################################

proc_write_chs_sector   proc    near
               ; 31/01/2010

loc_write_chs_sector:
                push    SI
                push    CX            
loc_09Bh:
                mov     DI,5                    
loc_0CAh:               
                push    AX                      ; Linear sector #
                push    DX                      ; DX_AX = Linear address (sectors)
                mov     CX, word ptr [Physical_Disk_SecPerTrack]
                push    BX

                call    RXDOS_DIV32            ; Special 32 bit divide !!!
                                                ; To fix large disk problem.
                                                ; (c) Erdogan Tan 1999
                                                ; (October 20th, 1999)

                mov     CX, BX                  ; Sector (zero based)
                inc     CX                      ; To make it 1 based
                push    CX
                mov     CX, word ptr [Physical_Disk_Heads]
                call    RXDOS_DIV32            ; Convert track to head & cyl
                mov     DH, BL                  ; BX = Head (max. FFh)
                pop     CX
                                                ; AX=Cyl, DH=Head, CX=Sector
                pop     BX                      ; ES:BX = Buffer

                mov     DL, byte ptr [PhysicalDriveNumber]
                mov     CH,AL                   
                ror     AH,1                    ; Rotate right
                ror     AH,1                   
                or      CL,AH                   
                mov     AX,0301h
                int     13h                     ; BIOS Service func ( ah ) = 3
                                                ; Write disk sectors
                                                ;AL-sec num CH-track CL-sec
                                                ; DH-head DL-drive ES:BX-buffer
                                                ;CF-flag AH-stat AL-sec read
                                                ; If CF = 1 then (If AH > 0)
                pop     DX
                pop     AX
                
                jnc     short loc_chs_12Bh              
                dec     DI                      
               ;jz      short loc_chs_12Bh              
                
               ;xor     AH,AH                   
               ;mov     DL, byte ptr [PhysicalDriveNumber]
               ;int     13h                     ; BIOS Service func ( ah ) = 0
                                                ; Reset disk system
                jnz     short loc_0CAh                  
               ;jmp     short loc_0CAh          

loc_chs_12Bh:
                pop     CX
                pop     SI
                retn                            ; db 0C3h

PhysicalDriveNumber: db 0
Physical_Disk_Heads: dw 0
Physical_Disk_SecPerTrack: dw 0

proc_write_chs_sector    endp

proc_rw_char   proc    near
               ; OUTPUT -> DS:SI = Entered String (ASCIIZ)
               mov     si, offset StartupFile_Name
               mov     bx,7
               mov     ah,3
               int     10h
               mov     Word Ptr [Cursor_Pos], dx
read_next_char:
               xor     ah,ah
               int     16h
               and     al,al
               jz      short loc_arrow    
               cmp     al,0E0h          
               je      short loc_arrow
               cmp     al,08h
               jne     short char_return
loc_back:
               mov     bl,7
               mov     ah,3
               int     10h
               cmp     dl,byte ptr [Cursor_Pos]
               ja      short prev_column
loc_beep:
               mov     ah, 0Eh
               mov     al, 7
               int     10h
               jmp     short read_next_char
prev_column:
               dec     dl
set_cursor_pos:
               mov     ah,02h
               int     10h
               mov     bl, dl
               sub     bl,byte ptr [Cursor_Pos] 
               mov     cx,1
               mov     ah,09h
               mov     al,20h
               mov     byte ptr [SI][BX],al
loc_write_it:
               mov     bl,7
               int     10h
               mov     dx,word ptr [Cursor_Pos]
               jmp     short read_next_char
loc_arrow:    
               cmp     AH,4Bh
               je      short loc_back
               cmp     AH,53h
               je      short loc_back
               jmp     short read_next_char
char_return:
               mov     bl,7
               mov     ah,3
               int     10h
               mov     bl,dl
               sub     bl,byte ptr [Cursor_Pos] 
               cmp     al,20h
               jb      short loc_escape
               cmp     bl,63
               ja      short loc_beep
             ; cmp     al, "z"
             ; ja      short read_next_char
             ; cmp     al, "a"
             ; jb      short pass_capitalize
             ; and     al, 0DFh
pass_capitalize:
               xor     ah, ah
               mov     word ptr [SI][BX],ax
               mov     ah, 0Eh
               mov     bl, 7
               int     10h
               jmp     short read_next_char
pass_escape:
               cmp     al,0Dh
               jne     short read_next_char
               mov     bl,7
               int     10h
               mov     al,0Ah
               int     10h
               retn
loc_escape:
               cmp     al,1Bh
               jne     short pass_escape
               stc
               retn

proc_rw_char   endp

proc_load_masterboot proc near
                ; input -> dl = drive number
                xor ah,ah
                int 13h
                ;jnc short pass_reset_error
                jc short ret_from_load_mb ; 23/4/2009 FSFDISK.COM
;harddisk_error:
;               retn
pass_reset_error:
                mov bx, offset MasterBootBuff
                mov ax,0201h
                mov cx,1
                xor dh,dh
                push ds
                pop es
                int 13h
                ;jc short harddisk_error

                ;cmp word ptr [MBIDCode],0AA55h ; 23/4/2009 FSFDISK.COM
                ;jne short loc_not_masterboot
                retn
;loc_not_masterboot:
;                stc
ret_from_load_mb:   ; 23/4/2009 FSFDISK.COM
                 retn

proc_load_masterboot endp

proc_update_dat proc near
                ; 20/02/2010 [DAT_Buffer_Offset]
                ; 13/02/2010
                ;
                ; 02/05/2009
                ; DX:AX = Disk Sector (Beginning Sector, with Descriptor)
                ; BL = Allocation Type (Identifier)

                mov byte ptr [DAT_Identifier], bl

                mov cx, 512
                call RXDOS_div32
                cmp word ptr [DAT_Buffer_Sector]+2, dx
                jne short loc_write_prev_DAT_sector
                cmp word ptr [DAT_Buffer_Sector], ax
                jne short loc_write_prev_DAT_sector

                mov byte ptr [DAT_Buffer_Updated], 0                
 
loc_update_dat_buffer_x:
                push si
                mov si, offset DATBUFFER
                add si, bx
                mov bl, byte ptr [DAT_Identifier]
                mov byte ptr [SI], bl
                mov byte ptr [DAT_Buffer_Updated], 1   
                pop si 
return_from_dat_update:
                retn
 
loc_write_prev_DAT_sector:
                mov word ptr [DAT_Buffer_Offset], bx
                cmp byte ptr [DAT_Buffer_Updated], 0
                jna short loc_read_DAT_sector
                push dx
                push ax 
                mov ax, word ptr [DAT_Buffer_Sector]
                mov dx, word ptr [DAT_Buffer_Sector]+2
                call proc_write_DAT_sector
                jnc short loc_read_DAT_sector_pop_ax_dx
                pop dx
                pop dx
                mov bl, byte ptr [DAT_Identifier]
                jmp short return_from_dat_update
loc_read_DAT_sector_pop_ax_dx:
                pop ax
                pop dx
loc_read_DAT_sector:
                call proc_load_dat_sector
                jc short return_from_dat_update 
		mov bx, word ptr [DAT_Buffer_Offset]
                jmp short loc_update_dat_buffer_x

;DAT_Buffer_Drv: db 0
DAT_Identifier: db 0
DAT_Buffer_Updated: db 0
DAT_Buffer_Sector: dd 0
DAT_Buffer_Offset: dw 0

proc_update_dat endp

proc_load_dat_sector proc near
                 ; 13/02/2010
                 ; Input ->  ; DX:AX = DAT Sector (Offset)
                 ; Output -> clc : load error (err code in al)
                 ;           clc -> loading successed
                 ; DX:AX = DAT Sector (Offset) Address
                 ; BX = DAT Buffer Address
                 ; DAT_Buffer_Sector pointer is updated

                 push dx
                 push ax
                 add ax, word ptr [mat_begin_sec]
                 adc dx, word ptr [mat_begin_sec]+2
                 add ax, word ptr [mat_Dat_lba]
                 adc dx, word ptr [mat_Dat_lba]+2
                 mov bx, offset DATBUFFER               
                 call proc_disk_read
                 jnc short loc_load_DAT_sector_clc
                 pop dx
                 pop dx
                 retn
loc_load_DAT_sector_clc:
		 pop ax
                 pop dx
                 mov word ptr [DAT_Buffer_Sector], ax
                 mov word ptr [DAT_Buffer_Sector]+2, dx
                 mov byte ptr [DAT_Buffer_Updated], 0
                 retn

proc_load_dat_sector endp 

proc_write_dat_sector proc near
                 ; 20/02/2010
                 ; Input ->  ; DX:AX = DAT Sector (Offset)
                 ; Output -> clc : write error (err code in al)
                 ;           clc -> write successed
                 ; DX:AX = DAT Sector (Offset) Address
                 ; BX = DAT Buffer Address

                 add ax, word ptr [mat_begin_sec]
                 adc dx, word ptr [mat_begin_sec]+2
                 add ax, word ptr [mat_Dat_lba]
                 adc dx, word ptr [mat_Dat_lba]+2
                 mov bx, offset DATBUFFER               
                 call word ptr [proc_write_sector]
                 jc short loc_write_DAT_sector_stc_retn
 
loc_write_DAT_sector_clc:
                ;mov word ptr [DAT_Buffer_Sector], ax
                ;mov word ptr [DAT_Buffer_Sector]+2, dx
                 mov byte ptr [DAT_Buffer_Updated], 0

loc_write_DAT_sector_stc_retn:
                 retn

proc_write_dat_sector endp 

proc_get_first_free_section proc near
               ; 20/02/2010
               ; 13/02/2010
               ; 07/02/2010
               ; 31/01/2010
               ; INPUT -> DS:SI = FS Boot Sector Buffer
               ;          CX = Sector Count
               ; OUTPUT -> DX:AX = First Free Section
               ;           DS:SI = MAT Sector Buffer

                mov word ptr [CSectorCount], cx
                mov word ptr [CSCounter], 0 
                mov ax, word ptr [SI][bsMATLocation]
                mov dx, word ptr [SI][bsMATLocation]+2
                add ax, word ptr [fsBootSector]
                adc dx, word ptr [fsBootSector]+2
                mov si, offset MATBUFFER
                cmp word ptr [SI], 'AM'
                jne short loc_gfss_load_MAT
                cmp byte ptr [SI]+2, 'T'
                je short loc_check_MAT_sign_ok
loc_gfss_load_MAT:
                mov bx, si
                call proc_disk_read
                jc short loc_gffs_stc_retn

loc_check_MAT_sign:
               ;mov si, offset MATBUFFER
                cmp word ptr [SI], 'AM'
                jne loc_not_valid_fs_mat
                cmp byte ptr [SI]+2, 'T'
                jne loc_not_valid_fs_mat
loc_check_MAT_sign_ok:             
                mov ax, word ptr [SI][MAT_SectorCount]
                mov dx, word ptr [SI][MAT_SectorCount]+2
                 
                sub ax, 1
                sbb dx, 0

                mov word ptr [DAT_LastSector], ax
                mov word ptr [DAT_LastSector]+2, dx

                mov ax, word ptr [SI][MAT_FirstFreeSector]
                mov dx, word ptr [SI][MAT_FirstFreeSector]+2

                mov word ptr [Current_FS_Sector], ax
                mov word ptr [Current_FS_Sector]+2, dx

                mov word ptr [CFDT_Address], ax
                mov word ptr [CFDT_Address]+2, dx

                mov cx, 512
                call RxDOS_Div32 

                mov si, bx
                add si, offset DATBUFFER
           
                jmp short loc_gffs_load_dat_sector_cmd_dx

loc_not_valid_fs_mat:
                mov ax, 0Bh ; Invalid Format 
loc_gffs_stc_retn:
                xor cx, cx
                stc 
                retn

loc_gffs_load_dat_sector:
                mov cx, 512
                call RxDOS_Div32
loc_gffs_load_dat_sector_cmd_dx:
                cmp dx, word ptr [DAT_LastSector]+2
                ja short loc_gffs_end_of_DAT_sectors
                jb short pass_gffs_check_scount_ax
                cmp ax, word ptr [DAT_LastSector]
                ja short loc_gffs_end_of_DAT_sectors

pass_gffs_check_scount_ax:                 
                call proc_load_dat_sector
                jc short loc_gffs_stc_retn

loc_gffs_check_dat_cell_value:
                cmp byte ptr [SI], 90h
                je short loc_gffs_inc_cscounter
                mov cx, word ptr [CSCounter]
                cmp cx, word ptr [CSCounter_MAX]
                jna short pass_gffs_update_cscounter_max
                mov word ptr [CSCounter_MAX], cx
                mov ax, word ptr [CFDT_Address]
                mov dx, word ptr [CFDT_Address]+2
                mov word ptr [CFDT_Address_MAX], ax
                mov word ptr [CFDT_Address_MAX]+2, dx 
pass_gffs_update_cscounter_max:
                mov word ptr [CSCounter], 0
                mov ax, word ptr [Current_FS_Sector]
                mov dx, word ptr [Current_FS_Sector]+2
                add ax, 1
                adc dx, 0
                mov word ptr [CFDT_Address], ax
                mov word ptr [CFDT_Address]+2, dx
loc_gffs_check_cfs:
                mov word ptr [Current_FS_Sector], ax
                mov word ptr [Current_FS_Sector]+2, dx
                inc si
                cmp si, offset END_OF_DATBUFFER
                jb short loc_gffs_check_dat_cell_value
                jmp short loc_gffs_load_dat_sector

loc_gffs_inc_cscounter:
                inc word ptr [CSCounter]
                mov cx, word ptr [CSCounter]
                cmp cx, word ptr [CSectorCount]
                jnb short loc_gffs_return_ffs

                mov ax, word ptr [Current_FS_Sector]
                mov dx, word ptr [Current_FS_Sector]+2
                add ax, 1
                adc dx, 0
                jmp short loc_gffs_check_cfs

loc_gffs_end_of_DAT_sectors:
                mov cx, word ptr [CSCounter]
               ;cmp cx, word ptr [CSectorCount]
               ;jnb short loc_gffs_return_ffs
		mov si, offset MATBUFFER
                cmp word ptr [CSCounter_MAX], cx
                jb short loc_gffs_end_of_DAT_sectors_stc
                mov cx, word ptr [CSCounter_MAX]
                or cx, cx
                jnz short pass_gffs_reset_CFDT_address
                mov ax, cx
                mov dx, cx
                stc
                retn

pass_gffs_reset_CFDT_address: 
 		mov ax, word ptr [CFDT_Address_MAX]
                mov dx, word ptr [CFDT_Address_MAX]+2
                stc
                retn

loc_gffs_return_ffs:
                mov si, offset MATBUFFER
loc_gffs_end_of_DAT_sectors_stc: 
                mov ax, word ptr [CFDT_Address]
                mov dx, word ptr [CFDT_Address]+2
                retn

CSectorCount: dw 0
CSCounter: dw 0
DAT_LastSector: dd 0
CFDT_Address: dd 0
Current_FS_Sector: dd 0
CFDT_Address_MAX: dd 0
CSCounter_MAX: dw 0

proc_get_first_free_section endp

proc_delete_fs_startupfile proc near
               ; 13/02/2010
               ; 09/02/2010
               ; 07/02/2010
               ; INPUT -> DS:SI = FS Boot Sector Buffer
               ; OUTPUT -> CLC = No error

                mov ax, word ptr [SI][bsMATLocation]
                mov dx, word ptr [SI][bsMATLocation]+2
                add ax, word ptr [fsBootSector]
                adc dx, word ptr [fsBootSector]+2
                mov word ptr [CSFS_MAT_Address], ax
                mov word ptr [CSFS_MAT_Address]+2, dx
                
                mov bx, offset MATBUFFER
                call proc_disk_read
                jc short loc_delfs_stc_retn

loc_delsf_check_MAT_sign:
                mov si, offset MATBUFFER
                cmp word ptr [SI], 'AM'
                jne loc_not_valid_fs_mat
                cmp byte ptr [SI]+2, 'T'
                jne loc_not_valid_fs_mat

loc_delsf_read_SF_FDT:
                mov ax, word ptr [StartupFile_FDT]
                mov dx, word ptr [StartupFile_FDT]+2
                add ax, word ptr [fsBootSector]
                adc dx, word ptr [fsBootSector]+2
                mov bx, offset SECBUFFER
                call proc_disk_read
                jc short loc_delfs_stc_retn 

loc_check_FDT_sign:
                mov si, offset SECBUFFER
                cmp word ptr [SI], 'DF'
                jne short loc_delsf_not_valid_fs_fdt
                cmp byte ptr [SI]+2, 'T'
                jne short loc_delsf_not_valid_fs_fdt
                mov ax, word ptr [SI][fdtSectorCount]
                mov dx, word ptr [SI][fdtSectorCount]+2
                add ax, 1
                adc dx, 0
                or dx, dx
                jz short loc_delsf_save_csfs_sectorcount
loc_delsf_not_valid_fs_fdt:
loc_delsf_invalid_format:
                mov ax, 0Bh ; Invalid format
                stc
loc_delfs_stc_retn:
                retn
loc_delsf_save_csfs_sectorcount:                 
                mov word ptr [CSFS_SectorCount], ax  
                mov word ptr [CSFS_SectorCount]+2, dx
loc_delsf_set_fdt_deleted_sign:
                mov byte ptr [SI]+2, 'E'
                mov ax, word ptr [mat_begin_sec]
                mov dx, word ptr [mat_begin_sec]+2
                add ax, word ptr [StartupFile_FDT]
                adc dx, word ptr [StartupFile_FDT]+2  
                mov bx, offset SECBUFFER               
                call word ptr [proc_write_sector]
                jc short loc_delfs_stc_retn

loc_delsf_update_DAT:
                xor ax, ax
                xor dx, dx 
                call proc_load_dat_sector
                jc short loc_delfs_stc_retn

                mov bl, 90h ; Free Sector sign
                mov cx, word ptr [CSFS_SectorCount]
                or cx, cx 
                jz short loc_delsf_not_valid_fs_fdt
                mov ax, word ptr [StartupFile_FDT]
                mov dx, word ptr [StartupFile_FDT]+2
loc_delsf_update_DAT_loop:
                push dx
                push ax
                push cx 
                call proc_update_dat
                pop cx
                jnc short loc_delsf_update_DAT_loop_next
                pop dx ; pushed ax
                pop dx
                jmp short loc_delfs_stc_retn
loc_delsf_update_DAT_loop_next:
                pop ax
                pop dx 
                add ax, 1
                adc dx, 0
                loop loc_delsf_update_DAT_loop

                cmp byte ptr [DAT_Buffer_Updated], 0 
                jna short loc_delfs_update_MAT
                       
                mov ax, word ptr [DAT_Buffer_Sector]
                mov dx, word ptr [DAT_Buffer_Sector]+2
                call proc_write_DAT_sector
                jc short loc_delfs_stc_retn
 
loc_delfs_update_MAT:
                mov ax, word ptr [StartupFile_FDT]
                mov dx, word ptr [StartupFile_FDT]+2
                cmp dx, word ptr [mat_dat_ffs]+2
                ja short pass_delfs_update_MAT_ffs
                jb short loc_delfs_update_MAT_ffs
                cmp ax, word ptr [mat_dat_ffs]
                jnb short pass_delfs_update_MAT_ffs
loc_delfs_update_MAT_ffs:
		mov word ptr [mat_dat_ffs], ax
                mov word ptr [mat_dat_ffs]+2, dx
pass_delfs_update_MAT_ffs:
                mov ax, word ptr [CSFS_SectorCount]  
                mov dx, word ptr [CSFS_SectorCount]+2
                add word ptr [mat_dat_free_s], ax
                adc word ptr [mat_dat_free_s]+2, dx

                mov ax, word ptr [CSFS_MAT_Address]
                mov dx, word ptr [CSFS_MAT_Address]+2
                mov bx, offset MATBUFFER               
                call word ptr [proc_write_sector]
                jnc short loc_reset_fsbs_sf_pointers
                retn 

loc_reset_fsbs_sf_pointers:
                xor ax, ax
                xor dx, dx
                mov si, offset BSBUFFER
		mov word ptr [SI][bsStartupFD], ax
 		mov word ptr [SI][bsStartupFD]+2, dx 
                mov word ptr [StartupFile_FDT], ax
                mov word ptr [StartupFile_FDT]+2, dx
   		mov ax, word ptr [fsBootSector]
                mov dx, word ptr [fsBootSector]+2
                mov bx, offset BSBUFFER               
                call word ptr [proc_write_sector]

                retn

CSFS_SectorCount: dd 0
CSFS_MAT_Address: dd 0   

proc_delete_fs_startupfile endp


proc_display_startupfile_info proc near
               ; 01/02/2010
                call proc_clear_screen
                ;
                mov ax, word ptr [StartupFile_FDT]
                mov dx, word ptr [StartupFile_FDT]+2
                add ax, word ptr [fsBootSector]
                adc dx, word ptr [fsBootSector]+2
                mov bx, offset SECBUFFER
                call proc_disk_read
                jnc short loc_pdsf_check_FDT_sign
                retn  
loc_pdsf_check_FDT_sign:
                mov si, offset SECBUFFER
                cmp word ptr [SI], 'DF'
                jne short loc_pdsf_not_valid_FDT
                add si, 2
                cmp byte ptr [SI], 'T'
                jne short loc_pdsf_not_valid_FDT
loc_pdsf_print_fshd_name:
                cmp byte ptr [PhysicalDriveNumber], 80h
                jb short loc_pdsf_print_fsfd_name
                mov si, offset SINGLIX_HD_Name
                call SINGLIX_PRINTMSG
                jmp short loc_pdsf_print_sfn
loc_pdsf_print_fsfd_name:
                mov si, offset SINGLIX_FD_Name
                call SINGLIX_PRINTMSG

loc_pdsf_print_sfn:
                mov si, offset Msg_StartupFile_Name
                call SINGLIX_PRINTMSG
                mov si, offset SECBUFFER
                push si
                add si, fdtFileName
                call SINGLIX_PRINTMSG
                pop si
                add si, fdtFileSize
                mov ax, word ptr [SI]
                mov dx, word ptr [SI]+2
		;mov si, offset Decimal_Str
                call proc_bin_to_decimal 
                mov si, offset Str_startup_file_size
                call SINGLIX_PRINTMSG
		mov si, offset Decimal_Str
loc_pdsf_print_sfs_loop:
                cmp byte ptr [SI], '0'
                jne short loc_pdsf_print_sfs
                inc si
                jmp short loc_pdsf_print_sfs_loop

loc_pdsf_not_valid_FDT:
                mov ax, 0Bh ; Invalid Format 
                stc
                retn

loc_pdsf_print_sfs:
                call SINGLIX_PRINTMSG
                mov si, offset Str_Bytes
                call SINGLIX_PRINTMSG

loc_pdsf_print_sfdatetime:                
                ; 13/02/2010
                mov si, offset Str_startup_file_date_time
                call SINGLIX_PRINTMSG

		mov si, offset SECBUFFER
                add si, fdtLMdate
                mov al, byte ptr [SI]
                call proc_hex
                mov word ptr [Sf_Year_Str], ax
                inc si
                mov al, byte ptr [SI]
                call proc_hex
                mov word ptr [Sf_Year_Str]+2, ax
 		inc si
		mov al, byte ptr [SI]
                call proc_hex
 		mov word ptr [Sf_Month_Str], ax 
                inc si
		mov al, byte ptr [SI]
                call proc_hex
 		mov word ptr [Sf_Day_Str], ax
                inc si
                mov al, byte ptr [SI]
                call proc_hex       
		mov word ptr [Sf_Hour_Str], ax
 		inc si
		mov al, byte ptr [SI]
                call proc_hex
		mov word ptr [Sf_Minute_Str], ax                

		mov si, offset Sf_Date_Time_Str
                call SINGLIX_PRINTMSG              
                
		retn  

proc_display_startupfile_info endp


;'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''';
; RXDOS  32 bit Divide                                       ;
; (Special version by Erdogan Tan)                           ;
;- - - - - - - - - - - - - - - - - - - - - - - - - -- - - - -;
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
; (c) Erdogan TAN  1999                     [ RXDOSBIO.ASM ] ;
;............................................................;

RxDos_Div32    proc near

		mov  bx, dx
		xchg ax, bx
		xor  dx, dx
		div  cx         ; at first, divide DX
		xchg ax, bx     ; remainder is in DX
				; now, BX has quotient
				; save remainder
		div  cx         ; so, DX_AX divided and
				; AX has quotient
				; DX has remainder
		xchg dx, bx     ; finally, BX has remainder

		retn

RxDos_Div32    endp

proc_clear_screen proc near
               ;21/09/2009
               mov ax, 0600h
               mov bh, 7
               xor cx, cx
               mov dx, 184Fh
               int 10h
               mov ah, 2
               xor bh, bh
               xor dx, dx
               int 10h

               ;2004-2005
               ;mov ah, 0Fh 
               ;int 10h
               ;mov ah, 0
               ;int 10h

                retn

proc_clear_screen endp


;'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''';
; From binary (byte) to hexadecimal (character) converter    ;
;                                                            ;
; input -> AL = byte (binary number) to be converted         ;
; output -> AH = First character of hexadecimal number       ;
; output -> AL = Second character of hexadecimal number      ;
;                                                            ;
; (c) Erdogan TAN  1998 - 1999                               ;
;............................................................;

; 1998

proc_hex        proc    near

		db 0D4h,10h                     ; Undocumented inst. AAM
						; AH = AL / 10h
						; AL = AL MOD 10h
		or AX,'00'                      ; Make it ZERO (ASCII) based

                xchg AH,AL 

; 1999
		cmp AL,'9'
		jna pass_cc_al
		add AL,7
pass_cc_al:
		cmp AH,'9'
		jna pass_cc_ah
		add AH,7
pass_cc_ah:

; 1998
		retn

proc_hex        endp


LBA_Ready: db 0

StartupFile_Name: db 66 dup(0)

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;  messages
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

SINGLIX_Welcome:
                db 0Dh, 0Ah
                db 'TR-SINGLIX FS1 Startup File Configuration Utility'
                db 0Dh, 0Ah
                db 'v1.0.210210  (c) Erdogan TAN 2010'
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
MasterBootCode: db 1BEh dup (0)
PartitionTable: db 64 dup (0)
MBIDCode: dw ?

MATBUFFER:
                db 0,0,0 ; db 'MAT'               
                db 0
mat_volume_size:dd 0    ; Volume Size 
mat_begin_sec:  dd 0    ; Volume Beginning Sector
mat_dat_lba:    dd 0    ; DAT LBA = 2
mat_dat_secs:   dd 0    ; DAT Sectors
mat_dat_free_s: dd 0    ; Free Sectors on DAT
mat_dat_ffs:    dd 0    ; First Free sector
                db 484 dup(0)     
DATBUFFER:
                db 512 dup(0)
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
                dw 0  ; File Type
                db 0  ; File Name Checksum
                db 2  ; Attributes 
                dd 0
                dd 0
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
                db 64 dup (0)
                ; Offset 128
                db 128 dup (0)
                ; Offset 256
                db 256 dup (0)
    
; End Of FDT Buffer

SECBUFFER:
                db 512 dup (0)

RetryCount:     dw 0

Str_startup_file_size:
                db 0Dh, 0Ah, 0Dh, 0Ah
                db 'Startup File Size : ', 0
Str_Bytes:
                db ' bytes', 0

Decimal_Str:    db 10 dup (0)

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

                db  '(c) Erdogan TAN - 21/02/2010'

                db  1 dup (0)

SizeOfFile      equ $-100

               
SINGLIXBOOT     ends

                end     START_CODE
