; *****************************************************************************
;
; DRV_FS.ASM  [ TRDOS Kernel - Singlix FS Procedures ] 24/09/2011
;
; 03-19/09/2011
; 01-07/08/2011
; 05-14-16-18-21-30/07/2011
; 19/06/2011
; 02-11-12/04/2011
; 19-20/03/2011
; 09-19-20/01/2011
; 05-25/12/2010
; 06-07-14-15-19-20/11/2010
; 10/10/2010
; 04-08-09-12-19/09/2010
; 03-04-05-07-24-25-26-27-28-29-31/07/2010
; 05-20-26-27-28-30/06/2010
; 01-02-03-08-09-16-17-18-19-22-25-26/05/2010 
; 18-21-23-24-25/04/2010
; 28/02/2010
; 16-17-18-24/01/2010
;
; Copyright (C) 2009 Erdogan TAN  [12/12/2009] 
;
; TRDOS.ASM (include drv_fs.asm)
;
; *****************************************************************************

; SINGLIX FS (FS1 & FS2) Root Directory Description Table format 
; according to SINGLIX RDT Format Revision 12 (19/5/2009) by Erdogan Tan
FS_RDT_Sign equ 0 ; 'RDT'
FS_RDT_Version equ 3
FS_RDT_Sector_Size equ 4
FS_RDT_Number equ 6
FS_RDT_Directory_Number equ 8
FS_RDT_Next_RDT_Number equ 12
FS_RDT_Sector_Count equ 16
FS_RDT_Beginning_Sector equ 20
FS_RDT_Reserved1 equ 24
FS_RDT_Volume_Serial equ 28
FS_RDT_Entry_Size equ 32
FS_RDT_Reserved2 equ 33 ; Sub directory level = 0
FS_RDT_Reserved3 equ 34
FS_RDT_Country equ 46
FS_RDT_Time_Zone equ 47
FS_RDT_Creating_Date equ 48
FS_RDT_Creating_Time equ 52
FS_RDT_C_Time_Mode equ 55
FS_RDT_Last_Modif_Date equ 56
FS_RDT_Last_Modif_Time equ 60
FS_RDT_LM_Time_Mode equ 63
FS_RDT_Volume_Name equ 64
FS_RDT_Reserved4 equ 128
FS_RDT_Reserved5 equ 256

; SINGLIX FS (FS1 & FS2) Sub Directory Description Table format 
; according to SINGLIX DDT Format Revision 12 (19/5/2009) by Erdogan Tan
FS_DDT_Sign equ 0 ; 'DDT'
FS_DDT_Version equ 3
FS_DDT_Sector_Size equ 4
FS_DDT_Number equ 6
FS_DDT_Directory_Number equ 8
FS_DDT_Next_DDT_Number equ 12
FS_DDT_Sector_Count equ 16
FS_DDT_Parent_Dir_Number equ 20
FS_DDT_Parent_Dir_Serial equ 24
FS_DDT_Directory_Serial equ 28
FS_DDT_Entry_Size equ 32
FS_DDT_Reserved1 equ 33 ; Sub directory level
FS_DDT_Directory_Type equ 34
FS_DDT_CheckSum equ 36 ; Directory name checksum 
FS_DDT_Attributes equ 37
FS_DDT_OwnerCode equ 38
FS_DDT_GroupCode equ 42
FS_DDT_Country equ 46
FS_DDT_Time_Zone equ 47
FS_DDT_Creating_Date equ 48
FS_DDT_Creating_Time equ 52
FS_DDT_C_Time_Mode equ 55
FS_DDT_Last_Modif_Date equ 56
FS_DDT_Last_Modif_Time equ 60
FS_DDT_LM_Time_Mode equ 63
FS_DDT_Directory_Name equ 64
FS_DDT_Dir_Description equ 128
FS_DDT_Reserved2 equ 256

; SINGLIX FS (FS1 & FS2) File Description Table format 
; according to SINGLIX FDT Format Revision 12 (19/5/2009) by Erdogan Tan
FS_FDT_Sign equ 0 ; 'FDT'
FS_FDT_Version equ 3
FS_FDT_Sector_Size equ 4
FS_FDT_Number equ 6
FS_FDT_File_Number equ 8
FS_FDT_Next_FDT_Number equ 12
FS_FDT_Sector_Count equ 16
FS_FDT_Parent_Dir_Number equ 20
FS_FDT_Parent_Dir_Serial equ 24
FS_FDT_File_Size equ 28
FS_FDT_File_Size_High equ 32
FS_FDT_File_Type equ 34
FS_FDT_CheckSum equ 36 ; File name checksum 
FS_FDT_Attributes equ 37
FS_FDT_OwnerCode equ 38
FS_FDT_GroupCode equ 42
FS_FDT_Country equ 46
FS_FDT_Time_Zone equ 47
FS_FDT_Creating_Date equ 48
FS_FDT_Creating_Time equ 52
FS_FDT_C_Time_Mode equ 55
FS_FDT_Last_Modif_Date equ 56
FS_FDT_Last_Modif_Time equ 60
FS_FDT_LM_Time_Mode equ 63
FS_FDT_File_Name equ 64
FS_FDT_File_Description equ 128
FS_FDT_Reserved1 equ 256

; DOS Directory Entry Structure
DirEntry_Name equ 0
DirEntry_Attr equ 11
DirEntry_NTRes equ 12
DirEntry_CrtTimeTenth equ 13
DirEntry_CrtTime equ 14
DirEntry_CrtDate equ 16
DirEntry_LstAccDate equ 18
DirEntry_FstClusHI equ 20
DirEntry_WrtTime equ 22
DirEntry_WrtDate equ 24
DirEntry_FstClusLO equ 26
DirEntry_FileSize equ 28

; SINGLIX FS (FS1 & FS2) Master Allocation Table format 
; according to SINGLIX MAT Format Revision 3 (2/2/2008) by Erdogan Tan
FS_MAT_Sign equ 0 ; 'MAT'
FS_MAT_Version equ 3
FS_MAT_VolumeSize equ 4
FS_MAT_BeginSector equ 8
FS_MAT_DAT_Address equ 12
FS_MAT_SectorCount equ 16
FS_MAT_FreeSectors equ 20
FS_MAT_FirstFreeSector equ 24

; TR-DOS Extension to Singlix MAT format/specification (19/05/2010)
; Operating system specific (reserved) area
; Reserved for Singlix Operating System or fdisk or format programs
FS_MAT_Singlix_OS_Reserved1 equ 28 ; Do not overwrite/change ! 
FS_MAT_Singlix_OS_Reserved2 equ 32 ; Do not overwrite/change ! 
FS_MAT_Singlix_OS_Reserved3 equ 36 ; Do not overwrite/change ! 
FS_MAT_Singlix_OS_Reserved4 equ 40 ; Do not overwrite/change ! 
; Present TR-DOS version will use following MAT table/entry areas
FS_MAT_LM_OS equ 44 ; TR-DOS -> 00A1h
FS_MAT_LM_OS_Version equ 46 ; Version 1.0 -> 0100h
FS_MAT_LM_Date equ 48
FS_MAT_LM_Time equ 52
fs_mat_lm_year equ 48   ; MAT Last modification year
fs_mat_lm_month equ 50  ; MAT Last modification month
fs_mat_lm_day equ 51    ; MAT Last modification day
fs_mat_lm_hour equ 52   ; MAT Last modification hour
fs_mat_lm_minute equ 53 ; MAT Last modification minute
fs_mat_lm_second equ 54 ; MAT Last modification second
fs_mat_lm_dlstm equ 55  ; MAT LM Daylight Saving Time Mode 
                        ; (0= standard time)
; Reserved for future TR-DOS versions...
FS_MAT_TRDOS_Reserved1 equ 56 ; 4 byte
FS_MAT_TRDOS_Reserved2 equ 60 ; 4 byte
; SINGLIX Operating system will use above areas as compatible with
; TR-DOS... It will not destroy tr-dos specific MAT structure...
; NOTE: Offset 63 is outside of TR-DOS specific areas, no more !
;

; FS FDT/DDT DateTime Format
fs_fdt_cr_year equ 48   ; FDT Creating year
fs_fdt_cr_month equ 50  ; FDT Creating month
fs_fdt_cr_day equ 51    ; FDT Creating day
fs_fdt_cr_hour equ 52   ; FDT Creating hour
fs_fdt_cr_minute equ 53 ; FDT Creating minute
fs_fdt_cr_second equ 54 ; FDT Creating second
fs_fdt_cr_dlstm equ 55  ; FDT Creating time mode 
fs_fdt_lm_year equ 56   ; FDT Last modification year
fs_fdt_lm_month equ 58  ; FDT Last modification month
fs_fdt_lm_day equ 59    ; FDT Last modification day
fs_fdt_lm_hour equ 60   ; FDT Last modification hour
fs_fdt_lm_minute equ 61 ; FDT Last modification minute
fs_fdt_lm_second equ 62 ; FDT Last modification second
fs_fdt_lm_dlstm equ 63  ; FDT Last modification time mode 

fs_ddt_cr_year equ 48   ; DDT Creating year
fs_ddt_cr_month equ 50  ; DDT Creating month
fs_ddt_cr_day equ 51    ; DDT Creating day
fs_ddt_cr_hour equ 52   ; DDT Creating hour
fs_ddt_cr_minute equ 53 ; DDT Creating minute
fs_ddt_cr_second equ 54 ; DDT Creating second
fs_ddt_cr_dlstm equ 55  ; DDT Creating time mode 
fs_ddt_lm_year equ 56   ; DDT Last modification year
fs_ddt_lm_month equ 58  ; DDT Last modification month
fs_ddt_lm_day equ 59    ; DDT Last modification day
fs_ddt_lm_hour equ 60   ; DDT Last modification hour
fs_ddt_lm_minute equ 61 ; DDT Last modification minute
fs_ddt_lm_second equ 62 ; DDT Last modification second
fs_ddt_lm_dlstm equ 63  ; DDT Last modification time mode 


proc_load_current_FS_directory proc near
               ; 20/12/2009
               ;
               ; INPUT-> DS:SI = DOs Drive Description Table
               ;         ES=DS
               ; OUTPUT ->  DS:SI = DOS Drive Description Table
               ;            DI = Offset Current_Dir_Drv 
               ;              
               mov ah, byte ptr [SI][LD_FS_CDirLevel]
               mov byte ptr [Current_Dir_Level], ah  
	       or ah, ah
               jz short loc_lcfsd_reset_cdir_FS_fcluster_0
               mov al, 16
               mul ah
               push si
               add si, ax
               mov ax, word ptr [SI][LD_FS_CDIR_Converted]+12
               mov dx, word ptr [SI][LD_FS_CDIR_Converted]+14
               pop si
               jmp short loc_lcfsd_reset_cdir_FS_fcluster_1
loc_lcfsd_reset_cdir_FS_fcluster_0:
               mov ax, word ptr [SI][LD_FS_RootDirD]
               mov dx, word ptr [SI][LD_FS_RootDirD]+2 
loc_lcfsd_check_FS_rootdir_sign:
               cmp byte ptr [SI][LD_FS_CDIR_Converted], 0
               jne short loc_lcfsd_reset_cdir_FS_fcluster_1
loc_lcfsd_set_rootdir_FS_fcluster:
               mov word ptr [SI][LD_FS_CDIR_Converted]+12, ax
               mov word ptr [SI][LD_FS_CDIR_Converted]+14, dx
               mov word ptr [SI][LD_FS_CDIR_Converted], 'OR'
               mov word ptr [SI][LD_FS_CDIR_Converted]+2,'TO'
loc_lcfsd_reset_cdir_FS_fcluster_1:
               call proc_load_FS_sub_directory
               jnc short loc_lcfsd_change_prompt_dir_string
               retn

loc_lcfsd_change_prompt_dir_string:
               mov word ptr [Current_Dir_FCluster], ax
               mov word ptr [Current_Dir_FCluster]+2, dx

               mov di, offset PATH_Array
               ; 03/10/2009
               push si
               add si, LD_FS_CDIR_Converted
               mov cx, 64
               rep movsw

               call proc_change_prompt_dir_string
               
               pop si 
               
               xor ax, ax
              ;xor dx, dx
 	       mov di, offset Current_Dir_Drv

               mov byte ptr [Restore_CDIR], al ; 0

               retn

proc_load_current_FS_directory endp

proc_get_next_section proc near
                ; 05/07/2011 SegmentBoundaryCheck modification
                ; 04/09/2010 -> Disk IO error dx = 0 return
                ; 24/01/2010 FS_CurrentSection bugfix
                ; 16/01/2010
                ; Sector count (BX:CX) return
                ; 02/01/2010
                ; 12/12/2009
                ; Loads FS section header at/into the sector buffer
                ; and returns the next section header address
                ;  
                ; INPUT -> AX = Section (DDT/FDT) header address, low 16 bit
                ; INPUT -> DX = Section (DDT/FDT) header address, high 16 bit
                ; INPUT -> DS:SI = Logical Dos Drive Parameters Table
                ; OUTPUT -> clc -> No Error, DX:AX valid
                ; OUTPUT -> stc & AX=0 -> The last section, no next one
                ; OUTPUT -> stc & AX>0 -> Error
                ; AX: Next Section Address, low 16 bit
                ; DX: Next Section Address, high 16 bit
                ; 16/01/2010
                ; CX = Sector count, low word
                ; BX = Sector count, high word
                ; stc -> ax>0 -> Error (invalid values in cx, bx, dx)
                ;
             
                mov bx, word ptr [Sector_Buffer]
                cmp bx, 0
                ja short gnc_pass_allocate_FS_sector_buffer
                
                ; 24/01/2010
                mov word ptr [FS_CurrentSection], ax
                mov word ptr [FS_CurrentSection]+2, dx

               ;mov byte ptr [FS_BuffValidData], bl

                mov al, byte ptr [SI][LD_FS_Name]
		mov byte ptr [FS_BuffDrvName], al                

                ; bx = 0 => Allocate the first free segment
                mov cx, 2048 ; FS Sector Buffer size is 2048 bytes
                mov ax, 0105h ; Buffer, AL= 5, FS buffer allocation
                mov dx, 1 ; 512 byte segment boundary check
                call proc_allocate_memory
                jc short loc_gnc_allocate_FS_sector_buffer_stc_retn
                
                mov word ptr [Sector_Buffer], bx

 		push es
                
                mov es, bx 

		mov ax, word ptr [FS_CurrentSection]
                mov dx, word ptr [FS_CurrentSection]+2

                jmp load_FS_descriptortable_3

loc_gnc_allocate_FS_sector_buffer_stc_retn:
                mov ax, 8 ; No space to allocate fs sector buffer !
                mov dx, 0
                retn

loc_FS_invalid_descriptor_table_stc_retn:
                pop es
                mov ax, 0Bh ; MSDOS Error code: Invalid Format
               ;mov ax, 0Dh ; MSDOS Error code: Invalid Data
                xor dx, dx
                stc
                retn    

gnc_pass_allocate_FS_sector_buffer:
                push es
                mov es, bx
get_FS_next_descriptortable:
                mov cl, byte ptr [SI][LD_FS_Name]
                cmp byte ptr [FS_BuffValidData], 0
                jna short load_FS_descriptortable_0
                mov byte ptr [FS_BuffValidData], 0
                cmp cl, byte ptr [FS_BuffDrvName]
                jne short load_FS_descriptortable_0
                cmp dx, word ptr [FS_CurrentSection]+2
                jne short load_FS_descriptortable_1
                cmp ax, word ptr [FS_Currentsection]
                jne short load_FS_descriptortable_2

loc_gfsndt_check_ddt_sign_0:
                xor bx, bx ; Reset (DDT/FDT header offset)
loc_gfsndt_check_ddt_sign_1:
                ; 16/01/2010 FDT signature check
                cmp byte ptr ES:[BX], 'F'
                je short loc_gfsndt_check_ddt_sign_2
                ;
                cmp byte ptr ES:[BX], 'D'
                je short loc_gfsndt_check_ddt_sign_2
                cmp byte ptr ES:[BX], 'R'
                jne short loc_FS_invalid_descriptor_table_stc_retn
             
loc_gfsndt_check_ddt_sign_2:
                inc bx ; 'DT' sign, offset 1

                cmp word ptr ES:[BX], 'TD' ; 'DT' sign
                jne short loc_FS_invalid_descriptor_table_stc_retn

               ;cmp byte ptr [FS_BuffValidData], bl ; 1 
               ;jne short get_FS_next_sectionheader_address

                mov byte ptr [FS_BuffValidData], bl ; 1

get_FS_next_sectionheader_address:
                mov bx, FS_DDT_Next_DDT_Number ; Offset 12
                mov ax, word ptr ES:[BX]
                mov dx, word ptr ES:[BX]+2
               ; 16/01/2010 <- sector count return
               ; mov bx,  FS_FDT_Sector_Count
               ; mov cx, word ptr ES:[BX]
               ; mov bx, word ptr ES:[BX]+2 
                mov cx, word ptr ES:[BX]+4 ; FS_FDT_Sector_Count, LW
                mov bx, word ptr ES:[BX]+6 ; FS_FDT_Sector_Count, HW
                ;
                or ax, ax
                jnz short loc_retn_next_descriptortable_address

                cmp dx, 1
                ; If dx=0, cf=1 at return (0:0 = no next section)
loc_retn_next_descriptortable_address:
                pop es
                retn

load_FS_descriptortable_0:
                mov byte ptr [FS_BuffDrvName], cl
load_FS_descriptortable_1:
                mov word ptr [FS_CurrentSection]+2, dx
load_FS_descriptortable_2:
                mov word ptr [FS_CurrentSection], ax
load_FS_descriptortable_3:
                xor cx, cx
                add ax, word ptr [SI][LD_FS_BeginSector]
                adc dx, word ptr [SI][LD_FS_BeginSector]+2

                ;inc cx
                inc cl
                xor bx, bx 
                call proc_disk_read

                jnc short loc_gfsndt_check_ddt_sign_1

loc_FS_sectionheader_load_error:
               ; 04/09/2010
               ; xor dx, dx
                mov dx, 0
               ;mov byte ptr [FS_BuffValidData], 0
               ;mov byte ptr [FS_BuffValidData], dl
                pop es
               ;mov ax, 15h ; Drv not ready or read error
               ;stc
                retn

proc_get_next_section endp

proc_load_FS_root_directory proc near
               ; 12/12/2009

               ; INPUT -> 
               ; DS:SI = DOS Drive Description Table
              
               ; OUTPUT -> STC = Root directory could not be loaded
               ; Error number is in AX (AL)
               ; BX = 0 (BX > 0 -> sectors are loaded but not all)

               ; DS:SI = DOS Drive Description Table
               ; CX = Directory buffer size in sectors
	       ; NOTE: DirBuffer_Size is in bytes !
               ; BX = Directory Buffer Segment

               mov ax, word ptr [SI][LD_FS_RootDirD]
               mov dx, word ptr [SI][LD_FS_RootDirD]+2 

               call proc_load_FS_sub_directory

               retn   

proc_load_FS_root_directory endp

proc_load_FS_sub_directory proc near
               ; 05/07/2011 SegmentBoundaryCheck modification
               ; 24/07/2010 Unknown media/fs type return (AX= 1Ah) 
               ; 25/04/2010 Directory buffer allocation bugfix
               ; 23/04/2010 
               ; 17/01/2010, 18/01/2010
               ; 02/01/2010
               ; 13/12/2009

               ; INPUT -> 
               ; DS:SI = DOS Drive Description Table
               ; DX:AX = DDT Address               

               ; OUTPUT -> STC = Sub directory could not be loaded
               ; Error number is in AX (AL)
               ; BX = 0 (BX > 0 -> sectors are loaded but not all)
               
               ; DS:SI = DOS Drive Description Table
               ; CX = Directory buffer size in sectors
               ; NOTE: DirBuffer_Size is in bytes !
               ; BX = Directory Buffer Segment
               ; DX:AX = DDT Address 
               ; 

               cmp byte ptr [SI][LD_FS_FSType], 0A1h 
               je short loc_load_FS_directory                

	       mov ax, 1Ah ; Unknown media type (non-DOS disk)
               xor bx, bx
               stc
loc_load_FS_directory_retn: 
               retn

loc_load_FS_directory:
               mov word ptr [FS_DirBuffer_Section], ax
	       mov word ptr [FS_DirBuffer_Section]+2, dx

               call proc_get_count_of_FS_dir_entries
               jc short loc_load_FS_directory_retn

               mov word ptr [FS_DirEntryCount], ax
	       mov word ptr [FS_DirEntryCount]+2, dx

loc_load_FS_directory_allocate_compatibility_buffer:
               or dx, dx
  	       jnz short loc_load_fs_dir_sc_mov_ax_2048
   
              ;or ax, ax
              ;jz short loc_load_FS_directory_err_invalid_format

               cmp ax, 2048
               jna short loc_load_fs_dir_sc_dir_buff_prep
 
              ;ja short loc_load_fs_dir_sc_mov_ax_2048
              ;jmp short loc_load_fs_dir_sc_dir_buff_prep   
;loc_load_FS_directory_err_invalid_format:
              ;mov ax, 0Bh ; Invalid format
              ;xor bx, bx
              ;stc
              ;retn  

loc_load_fs_dir_sc_mov_ax_2048:
               mov ax, 2048
loc_load_fs_dir_sc_dir_buff_prep:
               mov cx, ax
             ; 25/04/2010
               add cx, 15
             ;  
               shr cx, 1
               shr cx, 1
               shr cx, 1
               shr cx, 1
               mov word ptr [DirBuff_Sectors], cx 
               dec ax
              ;xor bl, bl
               mov bh, byte ptr [SI][LD_FS_Name]
               mov byte ptr [DirBuff_DRV], bh
               mov byte ptr [DirBuff_FATType], bl ; 0
               mov byte ptr [DirBuff_ValidData], bl ; 0
               mov word ptr [DirBuff_LastEntry], ax
               push word ptr [FS_DirBuffer_Section]
	       push word ptr [FS_DirBuffer_Section]+2
               pop word ptr [Dirbuff_Cluster]+2
               pop word ptr [Dirbuff_Cluster]
             
             ; 25/04/2010
               mov ax, 512
               mul cx
               or dx, dx
               jz short loc_load_fs_dir_cdbs_byte_mov_cx_ax
               mov cx, 65535
               jmp short loc_load_fs_dir_cdbs_byte_mov_cx_65535

loc_load_fs_dir_cdbs_byte_mov_cx_ax:
               mov cx, ax                 
loc_load_fs_dir_cdbs_byte_mov_cx_65535:
             ;
               mov bx, word ptr [Directory_Buffer] ; Segment
  	       or bx, bx
               jz short loc_load_fs_dir_allocate_dirbuff
  
               cmp word ptr [DirBuffer_Size], cx
               je short loc_load_FS_directory_reset_next
               
               mov word ptr [DirBuffer_Size], cx
               mov ax, 0104h ;Consequently for 04h allocation
               call proc_deallocate_memory

               mov cx, word ptr [DirBuffer_Size]

               xor bx, bx
               mov word ptr [Directory_Buffer], bx ; 0

loc_load_fs_dir_allocate_dirbuff:
               mov ax, 0104h ;Consequently allocate Directory Buffer
               mov dx, 1 ; 512 byte segment boundary check
               call proc_allocate_memory
               jnc short loc_load_FS_directory_reset
               or dx, dx
               jz short loc_load_fs_dir_dir_buff_alloc_err
               ; 17/01/2010 
               push dx
               mov ax, 256
               mul dx
               mov cx, ax
               pop ax
               shl ax,1
               shl ax,1
               shl ax,1
               dec ax
               mov word ptr [DirBuff_LastEntry], ax
               mov word ptr [DirBuffer_Size], cx
               ;
               xor bx, bx  
               jmp short loc_load_fs_dir_allocate_dirbuff        

loc_load_fs_dir_dir_buff_alloc_err:
               mov ax, 8 ; NO SPACE TO ALLOCATE DIRECTORY BUFFER !
loc_load_FS_directory_xor_stc_retn:
               xor bx, bx
               stc
               retn 

loc_load_FS_directory_reset:
               mov word ptr [Directory_Buffer], bx
loc_load_FS_directory_reset_next:
               push es
               mov es, bx
               xor di, di
               mov word ptr [DirEntry_Offset], di
               xor al, al
               ; cx = directory buffer size in bytes 
               rep stosb ; Reset directory buffer 
               pop es

               mov ax, word ptr [FS_DirBuffer_Section]
	       mov dx, word ptr [FS_DirBuffer_Section]+2

loc_load_FS_original_directory_next_1:
              ; 23/04/2010
               mov bx, word ptr [FS_Directory_Buffer]  
               call proc_allocate_fs_section_buffer                
              ; 25/04/2010
               jc short loc_load_FS_sub_directory_return

loc_load_FS_original_directory_next_2:
              ; 23/04/2010
               mov word ptr [FS_Directory_Buffer], bx
               mov word ptr [FS_DirBuff_SectorCount], cx
               mov word ptr [FS_DDT_Next_Section], ax
	       mov word ptr [FS_DDT_Next_Section]+2, dx

loc_convert_fs_dir_entries_to_dos_format:
               mov ax, 128
               mul cx
               mov cx, ax ; FS Dir Entry Count
               push es                
               push ds
               push si
               xor si, si
               mov di, word ptr [Directory_Buffer]
               mov es, di
               xor di, di
               mov ds, bx
               mov bx, word ptr CS:[DirEntry_Offset]  
               call proc_convert_fs_dir_entries_to_dos_format
               pop si
               pop ds
               pop es
               jc short loc_load_FS_sub_directory_return

               or bx, bx ; end sign = 0
               jz short validate_fs_DirBuff_and_return

               mov word ptr [DirEntry_Offset], bx  

               mov ax, word ptr [FS_DDT_Next_Section]
               mov dx, word ptr [FS_DDT_Next_Section]+2

               or ax, ax
               jnz short loc_load_FS_original_directory_next_1
               or dx, dx             
               jnz short loc_load_FS_original_directory_next_1

validate_fs_DirBuff_and_return:
               mov cx, word ptr [DirBuff_Sectors]
               mov bx, word ptr [Directory_Buffer]
               mov byte ptr [DirBuff_ValidData], 1
               mov ax, word ptr [FS_DirBuffer_Section]
	       mov dx, word ptr [FS_DirBuffer_Section]+2

loc_load_FS_sub_directory_return:
              ;25/04/2010 -> to get more free memory at return
               pushf
               push bx
               mov bx, word ptr [FS_Directory_Buffer]
               or bx, bx
               jz short loc_load_FS_sub_dir_return_popbx_popf
               push cx
               push dx
               push ax
               mov ax, 0106h ;Consequently for 06h allocation
                       ; 06h -> FS directory/section buffer
               call proc_deallocate_memory 
               mov word ptr [FS_Directory_Buffer], 0
               pop ax
               pop dx
               pop cx  
loc_load_FS_sub_dir_return_popbx_popf:
               pop bx
               popf

               retn

proc_load_FS_sub_directory endp

proc_get_count_of_fs_dir_entries proc near
               ; 18/01/2010 
               ; 16/01/2010 (BX:CX = sector count)
               ; 01/01/2010 
               ; 13/12/2009
                            
               ; INPUT -> 
               ; DS:SI = DOS Drive Description Table
               ; DX:AX = DDT Address               

               ; OUTPUT -> STC = Error
               ; Error number is in AX (AL)
               ; BX:CX = Sector count of FS dir entries
               ; CLC -> DX:AX = Count of FS dir entries
               ; CLC -> CX = Count of FS dir entry sectors
               ;        BX = 0
               ; DS:SI = DOS Drive Description Table

               xor cx, cx

loc_gcofsde_load_directory:
               mov word ptr [FS_DirEntryCount], cx
               mov word ptr [FS_DirEntryCount]+2, cx
loc_gcofsde_load_directory_next:
               call proc_get_next_section
               jnc short loc_gcofsde_save_next_fs_ddt_address
               and al, al ; If cf=1 and ax=0 there is no error,
                          ; it is the last section.
                          ; If cf=1 and al>0, error code is in al 
               jz short loc_gcofsde_save_next_fs_ddt_address
               stc
               retn

loc_gcofsde_invalid_format:
                mov ax, 0Bh ; MSDOS Error code: Invalid Format
               ;mov ax, 0Dh ; MSDOS Error code: Invalid Data
               ;xor dx, dx
                stc
                retn     

loc_gcofsde_save_next_fs_ddt_address:
              ;push dx  ; word ptr [DDT_Next_Section]+2
              ;push ax  ; word ptr [DDT_Next_Section]   

              ;push cs
              ;push si
              ;mov ax, word ptr [Sector_Buffer]
              ;mov ds, ax

              ;mov si, FS_DDT_Sector_Count
              ;mov ax, word ptr [SI]
              ;mov dx, word ptr [SI]+2
              ;pop si
              ;pop ds

              ;add word ptr [FS_DirEntryCount], ax
              ;adc word ptr [FS_DirEntryCount]+2, dx

              ;pop ax ; word ptr [DDT_Next_Section]
              ;pop dx ; word ptr [DDT_Next_Section]+2

               add word ptr [FS_DirEntryCount], cx
               adc word ptr [FS_DirEntryCount]+2, bx   

loc_gcofsde_check_next_ddt_low_word:
               or ax, ax
               jnz short loc_gcofsde_load_directory_next
loc_gcofsde_check_next_ddt_high_word:
               or dx, dx
               jnz short loc_gcofsde_load_directory_next
    
               mov cx, word ptr [FS_DirEntryCount]
	       mov bx, word ptr [FS_DirEntryCount]+2

               or bx, bx
               jnz short loc_gcofsde_invalid_format
               
               or cx, cx
               jz short loc_gcofsde_invalid_format  

             ; 18/01/2010      

               mov ax, cx
             ; mov dx, bx 

              ;mov bx, 128
              ;call proc_mul32
              ;xor bx, bx  
              
               mov dx, 128
               mul dx

               retn

proc_get_count_of_fs_dir_entries endp

proc_convert_fs_dir_entries_to_dos_format proc near
               ; 25/05/2010 attrib conversion modification
               ; 24/04/2010 
               ; 18/01/2010
               ; 17/01/2010, 09/01/2010, 02/01/2010
               ; 15/12/2009
        
     	       ; INPUT -> 
               ; DS:SI = FS original directory buffer address
               ; CX= count of fs directory entries
               ; ES:DI = Directory buffer address 
               ; BX= Directory entry offset               

               ; OUTPUT -> STC = Error
               ; Error number is in AX (AL)
               ; CLC -> BX = Directory entry offset (next)
               ; BX = 0 => end of directory entries

loc_cfsdetdf:
               push di
               push si
               mov ax, 32
               mul bx
               add di, ax
loc_cfsdetdf_next:
               mov ax, word ptr [SI]
               mov dx, word ptr [SI]+2
               or ax, ax
               jnz short loc_convert_fs_dt_data_to_dos_direntry_format_0
               or dx, dx
               jnz short loc_convert_fs_dt_data_to_dos_direntry_format_0

loc_cfsdetdf_return_pop_di_si_xor_bx:
               pop si
               pop di
               xor bx, bx
               retn               

loc_cfsdetdf_deleted_entry_loop:
              ; 18/01/2010
               pop si ; pop si, push si
              ; 17/01/2010
               add si, 4
               push si   
               jmp short loc_cfsdetdf_next

loc_convert_fs_dt_data_to_dos_direntry_format_0:
              ; 17/01/2010
               cmp dx, 0FFFFh
               jne short loc_convert_fs_dt_data_to_dos_direntry_format_1
               cmp ax, 0FFFFh
               jne short loc_convert_fs_dt_data_to_dos_direntry_format_1
               loop loc_cfsdetdf_deleted_entry_loop
               jmp short loc_cfsdetdf_return_pop_di_si_xor_bx
              
loc_convert_fs_dt_data_to_dos_direntry_format_1:                
               push cx 
               push es
               push ds
               push bx
               
               push cs
               pop ds 
               mov bx, word ptr [Sector_Buffer]
               mov es, bx

               xor bl, bl
               mov bh, byte ptr [FS_BuffDrvName]
               sub bh, 'A' 

               mov si, offset Logical_DosDisks
               add si, bx

 	      ; 01/05/2010 (FS_Sector_Buffer control parameters)
               mov byte ptr [FS_BuffValidData], bl
               mov word ptr [FS_CurrentSection], ax
               mov word ptr [FS_CurrentSection]+2, dx
              ;

               add ax, word ptr [SI][LD_FS_BeginSector]
               adc dx, word ptr [SI][LD_FS_BeginSector]+2

               xor bh, bh
               xor cx, cx 
              ;inc cx
               inc cl             
               call proc_disk_read
               pop bx
               pop ds
               pop es
               jnc short loc_convert_fs_dt_data_to_dos_direntry_format_2

               pop cx
               pop si
               pop di
               retn

loc_convert_fs_dt_data_to_dos_direntry_format_2:
              ; 01/05/2010
 	       mov byte ptr [FS_BuffValidData], 1
              ; 
               mov si, word ptr CS:[Sector_Buffer]
               push ds
               mov ds, si
                
               ; 10/1/2010
               mov si, FS_FDT_Last_Modif_Date
               mov al, byte ptr [SI] ; Year
               ; AL <= BCD (Singlix FS date-time bytes are BCD numbers)
               db 0D4h,10h                     ; Undocumented inst. AAM
						; AH = AL / 10h
						; AL = AL MOD 10h
               AAD ; AX= AH*10+AL
               mov ah, 100
               mul ah
               mov cx, ax

               mov al, byte ptr [SI]+1 ; Year
               ; AL <= BCD (Singlix FS date-time bytes are BCD numbers)
               db 0D4h,10h                     ; Undocumented inst. AAM
						; AH = AL / 10h
						; AL = AL MOD 10h
               AAD ; AX= AH*10+AL
               add cx, ax

               sub cx, 1980 ; ms-dos epoch = 1980

               mov ch, cl
               xor cl, cl
               shl ch, 1

               mov al, byte ptr [SI]+2 ; Month  
               db 0D4h,10h                     ; Undocumented inst. AAM
						; AH = AL / 10h
						; AL = AL MOD 10h
               AAD ; AX= AH*10+AL
               mov ah, 32
               mul ah
               add cx, ax
                            
               mov al, byte ptr [SI]+3 ; Day    
               db 0D4h,10h                      ; Undocumented inst. AAM
						; AH = AL / 10h
						; AL = AL MOD 10h
               AAD ; AX= AH*10+AL
               add ax, cx
               ; now date stamp is in ms-dos dir entry date format (ax)
               
               mov word ptr ES:[DI][DirEntry_WrtDate], ax 

               mov si, FS_FDT_Last_Modif_Time
               mov al, byte ptr [SI] ; Hour
               ; AL <= BCD (Singlix FS date-time bytes are BCD numbers)
               db 0D4h,10h                     ; Undocumented inst. AAM
						; AH = AL / 10h
						; AL = AL MOD 10h
               AAD ; AX= AH*10+AL
               xor cl, cl
               mov ch, al
               shl ch, 1
               shl ch, 1
               shl ch, 1 

               mov al, byte ptr [SI]+1 ; Minute
               db 0D4h,10h                     ; Undocumented inst. AAM
						; AH = AL / 10h
						; AL = AL MOD 10h
               AAD ; AX= AH*10+AL
               mov ah, 32
               mul ah
               add cx, ax
                            
               mov al, byte ptr [SI]+2 ; Second   
               db 0D4h,10h                      ; Undocumented inst. AAM
						; AH = AL / 10h
						; AL = AL MOD 10h
               AAD ; AX= AH*10+AL
               shr ax, 1 ; count of 2 seconds
               add ax, cx
               ; now time stamp is in ms-dos dir entry date format (ax)
               
               mov word ptr ES:[DI][DirEntry_WrtTime], ax

               ; 9/1/2010
               xor si, si
               mov al, byte ptr [SI]
               mov si, FS_FDT_Attributes
               ; 25/05/2010
               xor ah, ah
loc_cfsdetdf_check_fdt_sign:
               cmp al, 'F'
               je short loc_cfsdetdf_check_fs_file_attributes
               cmp al, 'D'
               je short loc_cfsdetdf_dir_attribute
	       or ah, 4 ; system file sign
               cmp al, 'R'
               jne short loc_cfsdetdf_check_fs_file_attributes
loc_cfsdetdf_dir_attribute:
               or ah, 10h ; Directory
loc_cfsdetdf_check_fs_file_attributes:
               mov al, byte ptr [SI]
               shr al, 1 ; cf -> owner's write permission, 0 = read only
               cmc
               adc ah, 0
              ;26/05/2010
               shr al, 1 ; Owner's execute permission bit 
               jnc short loc_cfsdetdf_check_fs_file_attributes_greadp
               test al, 4 ; Group User's execute permission bit
               jnz short loc_cfsdetdf_check_fs_file_attributes_greadp
               or ah, 4 ; system file sign 
              ;
loc_cfsdetdf_check_fs_file_attributes_greadp:
               shr al, 1 ;  cf -> group user's read permission, 0 = hidden    
               jc short loc_cfsdetdf_write_converted_attributes
               or ah, 2 ; hidden file sign
loc_cfsdetdf_write_converted_attributes:
               mov byte ptr ES:[DI][DirEntry_Attr], ah
               
               ; 9/1/2010
               mov si, FS_FDT_File_Size
               mov ax, word ptr [SI]
               mov dx, word ptr [SI]+2
               mov word ptr ES:[DI][DirEntry_FileSize], ax
               mov word ptr ES:[DI][DirEntry_FileSize]+2, dx 
               mov si, FS_FDT_File_Size_High
               mov ax, word ptr [SI]
               mov word ptr ES:[DI][DirEntry_LstAccDate], ax
               mov si, FS_FDT_File_Number
               mov ax, word ptr [SI]
               mov dx, word ptr [SI]+2
               mov word ptr ES:[DI][DirEntry_FstClusLO], ax
               mov word ptr ES:[DI][DirEntry_FstClusHI], dx

               ; 9/1/2010
               mov word ptr ES:[DI][DirEntry_NTRes], 01A1h 
               ; 01A1h is converted FS Dir Entry Sign
               ; DirEntry_NTRes is zero for ms-dos
 
               mov si, FS_FDT_File_Name ; 64

               call proc_generate_dos_basis_name
            
               ; 16/1/2010
               jnc short loc_cfsdetdf_move_shortname

               call proc_generate_dos_short_name_with_numeric_tail

               jc short loc_cfsdetdf_return_xor_bx
               ;
                
loc_cfsdetdf_move_shortname:
               mov cx, 11
               rep movsb
               cmp bx, word ptr CS:[DirBuff_LastEntry]
               jnb short loc_cfsdetdf_return_xor_bx
               pop ds
               pop cx
               pop si
               pop di
               inc bx
               loop loop_cfsdetdf
              ; 24/04/2010
               retn
               
loc_cfsdetdf_return_xor_bx:
              ; 24/04/2010
               pop ds
               pop cx
               pop si
               pop di
               xor bx, bx
               retn

loop_cfsdetdf:
               add si, 4
               jmp loc_cfsdetdf

proc_convert_fs_dir_entries_to_dos_format endp

proc_generate_dos_basis_name proc near
              ; 24/09/2011 .ext "Þ,-,#" BugFix 
              ; 19/09/2011 "Þ,-,#" valid
              ;    DOS invalid_fname_chars check
              ; 14/07/2011 '~' Numeric Tail BugFix
              ; 16/01/2010
              ; 05/01/2010
              ; 20/12/2009
              ; INPUT-> 
              ; DS:SI = FS file/dir name, asciiz
              ; OUTPUT -> 
              ; DS:SI = DOS DIR entry format file/dir name (11 byte)
              ; AX, CX will be changed (BX -> same)
              ; stc -> basis file name is not exact file name
              ; clc -> basic file name is exact file name   

                push es
                push di
                push cs
                pop es
                mov di, offset BasisName
                push di
                mov al, 20h
                mov cx, 11
                rep stosb ; Reset
                xor ax, ax
                stosw ; Reset 
                pop di

                mov cl, 0FFh
                push si               
loc_dbnga_check_fs_name_dots:
                inc cl
                lodsb
                cmp al, 20h
                jb short pass_dbnga_check_fs_name_dots
                je short loc_dbnga_check_fs_name_dots
                cmp al, 2Eh
                jne short loc_dbnga_check_fs_name_dots_next
                mov byte ptr CS:[DotOffset], cl
                jmp short loc_dbnga_check_fs_name_dots
loc_dbnga_check_fs_name_dots_next:
                inc ch
		jmp short loc_dbnga_check_fs_name_dots
pass_dbnga_check_fs_name_dots:
                pop si
                and ch, ch
                jz retn_from_convert_to_basisname_0

                mov al, 8
                mov ah, byte ptr CS:[DotOffset]
                or ah, ah
                jnz short loc_dbnga_check_file_name_length
                cmp cl, al  ; 8
                ja short loc_dbnga_check_file_name_length_nt
                jmp short loc_convert_to_basisname_0
  
loc_dbnga_check_file_name_length:
                cmp ah, al  ; 8
                ja short loc_dbnga_check_file_name_length_nt
                cmp cl, 12
                jna short loc_convert_to_basisname_0
loc_dbnga_check_file_name_length_nt:
                inc byte ptr CS:[BasisName_NT] ; Numeric tail sign

loc_convert_to_basisname_0:
                mov cx, 0FFh
loc_convert_to_basisname_0_next:
                inc cl                
                lodsb
                cmp al, 20h
                ja short loc_convert_to_basisname_1
                je short loc_convert_to_basisname_0_next
                jmp short retn_from_convert_to_basisname_1

loc_convert_to_basisname_1:
                cmp al, 2Eh
                jne short loc_convert_to_basisname_2
                cmp cl, byte ptr CS:[DotOffset]
                je short loc_convert_to_basisname_4
                jmp short loc_convert_to_basisname_0_next

loc_convert_to_basisname_2:
                inc ch
                cmp ch, 8
                ja short loc_convert_to_basisname_3
               
                cmp al, 61h
                jb short loc_convert_to_basisname_8  
                cmp al, 7Ah
              ; 14/07/2011
                ja short loc_convert_to_basisname_10

loc_convert_to_basisname_17:
                and al, 0DFh ; convert to capital letter

loc_convert_to_basisname_18:
                stosb
		jmp short loc_convert_to_basisname_0_next

loc_convert_to_basisname_10:
 		; 14/07/2011
                cmp al, '~' ; 7Eh
                je short loc_convert_to_basisname_18
                mov al,'_'
                jmp short loc_convert_to_basisname_18 

loc_convert_to_basisname_8:
                cmp al, 41h
                jb short loc_convert_to_basisname_9
                cmp al, 5Ah
                jna short loc_convert_to_basisname_18 
loc_convert_to_basisname_9:
                push cx
                push di
                mov cx, sizeInvFnChars
                mov di, offset invalid_fname_chars
loc_convert_to_basisname_19:
		scasb 
                je short loc_convert_to_basisname_20
                loop loc_convert_to_basisname_19
loc_convert_to_basisname_12:
                pop di
		pop cx
                jmp short loc_convert_to_basisname_18
loc_convert_to_basisname_20:
                mov al,'_'
                jmp short loc_convert_to_basisname_12            

loc_convert_to_basisname_3:
                ; 16/01/2010 (filenames without extension)
                or ah, ah  ; byte ptr CS:[DotOffset]
                jz short retn_from_convert_to_basisname_1 
                ;
                cmp cl, ah ; byte ptr CS:[DotOffset]
                jb short loc_convert_to_basisname_0_next

loc_convert_to_basisname_4:                 
                mov di, offset BasisName_Ext

                xor cl, cl
loc_convert_to_basisname_4_next:
                lodsb
                cmp al, 20h
                ja short loc_convert_to_basisname_5
                je short loc_convert_to_basisname_4_next
retn_from_convert_to_basisname_1:
                mov si, offset BasisName
                jmp short retn_from_convert_to_basisname_2

loc_convert_to_basisname_5:
                inc cl
                cmp cl, 3
                jna short loc_convert_to_basisname_6
                inc byte ptr CS:[BasisName_NT]
                jmp short retn_from_convert_to_basisname_1
  
loc_convert_to_basisname_6:
                cmp al, 61h
                jb short loc_convert_to_basisname_7  
                cmp al, 7Ah
                ja short loc_convert_to_basisname_7   
                
                and al, 0DFh ; convert to capital letter

loc_convert_to_basisname_16:
               ; 24/09/2011
                stosb
                jmp short loc_convert_to_basisname_4_next

loc_convert_to_basisname_7:
               ; 24/09/2011
                cmp al, 41h
                jb short loc_convert_to_basisname_11
                cmp al, 5Ah
                jna short loc_convert_to_basisname_16
loc_convert_to_basisname_11:
                push cx
                push di
                mov cx, sizeInvFnChars
                mov di, offset invalid_fname_chars
loc_convert_to_basisname_13:
		scasb 
                je short loc_convert_to_basisname_15
                loop loc_convert_to_basisname_13
loc_convert_to_basisname_14:
                pop di
		pop cx
                jmp short loc_convert_to_basisname_16
loc_convert_to_basisname_15:
                mov al,'_'
                jmp short loc_convert_to_basisname_14           
          
retn_from_convert_to_basisname_0:
                mov si, di
retn_from_convert_to_basisname_2:
                cmp byte ptr  CS:[BasisName_NT], 0
                je short retn_from_convert_to_basisname_3
                stc
retn_from_convert_to_basisname_3:
                pop di
                pop es
                push cs
                pop ds
                retn
                      
BasisName: db 8 dup (0)
BasisName_Ext: db 3 dup(0)
DotOffset:  db 0
BasisName_NT: db 0

proc_generate_dos_basis_name endp

proc_generate_dos_short_name_with_numeric_tail proc near
              ; 16/01/2010
              ; INPUT-> 
              ; DS:SI = DOS Basis Name (8+3, 11 bytes)
              ; Basis name is in dos dir entry format
              ; OUTPUT -> 
              ; DS:SI = DOS DIR entry format file/dir name (11 byte)
              ; AX, CX will be changed (BX -> same)

              push es
              push di
              push ds
              push si

              mov di, offset ShortName_NT
              push cs
              pop es
              mov cx, 11
              rep movsb

              push cs
              pop ds

              mov di, word ptr [Directory_Buffer]
            
            ; xor cx, cx

              mov es, di

              mov ax, 1

loc_gdsnwnt_write_numeric_tail_1:
              mov word ptr [NumericTail_Counter], ax
              add al, '0' 
              mov si,  (offset ShortName_NT_Ext)-2
             
              mov byte ptr [SI], '~'
              inc si
              mov byte ptr [SI], al

              xor di, di 

loc_gdsnwnt_compare_dir_entry_names_0:
              mov si, offset ShortName_NT
              push cx
              mov cx, 11
loc_gdsnwnt_compare_dir_entry_names_1:
              lodsb
              scasb
              jne short loc_gdsnwnt_compare_dir_entry_names_next
              loop loc_gdsnwnt_compare_dir_entry_names_1
              pop cx

              xor di, di
              xor cx, cx 

loc_gdsnwnt_compare_dir_entry_names_2:
              mov ax, word ptr [NumericTail_Counter]

;             cmp ax, 999999
;             jnb short loc_gdsnwnt_conversion_error 

              inc ax
              cmp ax, 9
              jna short loc_gdsnwnt_write_numeric_tail_1

              mov word ptr [NumericTail_Counter], ax 

              cmp ax, 99
              ja  short loc_gdsnwnt_write_numeric_tail_3

loc_gdsnwnt_write_numeric_tail_2:
              AAM ; Converts an 8 bit binary number in AL,
                  ; to an unpacked BCD number in AX
                  ; The most significiant digit in AH
                  ; The Least significiant digit in AL
              xchg ah, al
              add ax, '00'
              mov si,  (offset ShortName_NT_Ext)-3
              mov byte ptr [SI],'~'
              inc si
              mov word ptr [SI], ax

              jmp short loc_gdsnwnt_write_numeric_tail_4

loc_gdsnwnt_write_numeric_tail_3:
              cmp ax, 999
              ja short loc_gdsnwnt_conversion_error

              push dx
              mov dx, 10
              div dx
              AAM ; Converts an 8 bit binary number in AL,
                  ; to an unpacked BCD number in AX
                  ; The most significiant digit in AH
                  ; The Least significiant digit in AL
              add ax, '00'
              add dl, '0'
              mov si, (offset ShortName_NT_Ext)-4
              mov byte ptr [SI],'~'
              inc si 
              mov byte ptr [SI], ah
              inc si 
              mov byte ptr [SI], al
              inc si 
              mov byte ptr [SI], dl
              pop dx

              jmp short loc_gdsnwnt_write_numeric_tail_4

loc_gdsnwnt_compare_dir_entry_names_next:
              pop cx
              inc cx
              cmp word ptr [DirBuff_LastEntry], cx
              jb short loc_gdsnwnt_retn_1

loc_gdsnwnt_write_numeric_tail_4:
              mov di, cx
              shl di, 1
	      shl di, 1
	      shl di, 1
	      shl di, 1
	      shl di, 1
              cmp byte ptr ES:[DI], 0
              ja short loc_gdsnwnt_compare_dir_entry_names_0  

loc_gdsnwnt_retn_1:
              pop ax ; si
              pop ax ; ds

              xor ax, ax

              mov si, offset ShortName_NT

loc_gdsnwnt_retn_2:
       
              pop di
              pop es
 
              retn

loc_gdsnwnt_conversion_error: 
              mov ax, 12h ; No more files

              stc

              pop si
              pop ds

              jmp short loc_gdsnwnt_retn_2               
    
ShortName_NT: db 8 dup(0)
ShortName_NT_Ext: db 3 dup(0)
gecici: db 0Dh, 0Ah, 0
NumericTail_Counter: dw 0  

proc_generate_dos_short_name_with_numeric_tail endp

proc_load_fs_file  proc near
              ; 19/06/2011 ES:DI -> ES = Buffer Segment
              ; 20/01/2011
              ; 19/01/2011 BugFix
              ; 05/12/2010 BugFix
              ; 16/01/2010
              ; INPUT ->
              ; DS:SI = DOS logical drv description table
              ; ES = File Buffer Segment
              ; DX:AX = FDT Number
              ; CX = Sector Count
              ; OUTPUT ->
              ; AX = File Segment 
              ; STC -> Error Code in AL

              ; cmp byte ptr [SI][LD_FSType],0A1h
              ; je short loc_load_FS_file_sectors

              ; mov ax, 15h
              ; stc
              ; retn
                
loc_load_FS_file_sectors:
               ; 19/06/2011
                xor bx, bx
                mov word ptr [LoadFile_BufferOff], bx
               ; 20/01/2011
               ; 19/01/2011
                mov bx, es
                mov word ptr [LoadFile_Segment], bx
                push bx ; Segment

                mov word ptr [LoadFile_SectorCount], cx

loc_load_FS_file_sectors_next:
                mov word ptr [LoadFile_Cluster], ax
                mov word ptr [LoadFile_Cluster]+2, dx 

                call proc_get_next_section
                jnc short loc_load_fs_file_check_sector_count

                or al, al
                jz short loc_load_fs_file_check_sector_count

loc_load_fs_file_stc1_retn:
                ; 20/01/2011
                pop dx ; Segment 
                
                stc
                retn 

loc_load_fs_file_check_sector_count:
               ; 05/12/2010 
                or bx, bx
                jnz short loc_load_fs_file_sector_count_err
                or cx, cx
                jz short loc_load_fs_file_sector_count_err
                
               ; 20/01/2011 
                cmp cx, word ptr [LoadFile_SectorCount]
                jna short loc_load_fs_file_calculate_address
                mov cx, word ptr [LoadFile_SectorCount] 
                jmp short loc_load_fs_file_calculate_address


loc_load_fs_file_sector_count_err:
                mov ax, 0Bh ; MSDOS Error code: Invalid Format
               ;mov ax, 0Dh ; MSDOS Error code: Invalid Data
               ; 20/01/2011
                jmp short loc_load_fs_file_stc1_retn 

loc_load_fs_file_calculate_address:
               ; 20/01/2011
                push ax
                push dx
                mov ax, word ptr [LoadFile_Cluster]
                mov dx, word ptr [LoadFile_Cluster]+2
                add ax, word ptr [SI][LD_FS_BeginSector]
                adc dx, word ptr [SI][LD_FS_BeginSector]+2
                add ax, 1
                adc dx, 0 
                mov bx, word ptr [LoadFile_BufferOff]
                push dx
                push ax
                mov ax, word ptr [SI][LD_FS_BytesPerSec]
                mul cx
                add ax, bx
                adc dx, 0
                mov word ptr [LoadFile_BufferOff], ax
               ; 20/01/2011 
               ; 19/01/2011
                mov ax, 1000h
                mul dx
                mov dx, word ptr [LoadFile_Segment]
                mov es, dx
                add ax, dx
                mov word ptr [LoadFile_Segment], ax
                pop ax
                pop dx
                ; cx = Sectors in the file data section
                push cx
                call proc_disk_read
                pop cx
                pop dx
                jc short loc_load_fs_file_stc2_retn
                pop ax

                sub word ptr [LoadFile_SectorCount], cx
                ja short loc_load_FS_file_sectors_next
               ; jb short loc_load_fs_file_sector_count_err

loc_load_fs_file_retn:
               ; 20/01/2011 
                pop ax ;  Segment
                retn 
loc_load_fs_file_stc2_retn:
               ; 20/01/2011 
                pop dx
                pop dx ; Segment
                retn

proc_load_fs_file  endp

proc_get_fs_longname proc near
                ; 24/01/2010
              
                ; INPUT -> 
                ; DS:SI = Directory Entry (FindFile_DirEntry) Location
                ; ES=DS=CS

                ; OUTPUT ->
                ; DS:SI = ASCIZZ longname address (cf=0)
                ; cf=1, error number returns in AL
                ; AL= 0 & CF=1 -> longname not found
                ;       the file/directory has no longname
                ; cf=1 -> AH= Logical Dos Drive Number 
                ; AL>0 -> Disk read/write etc. error
                ; cf=0 -> AL= FAT Type = 0 

                xor al, al
                mov ah, byte ptr [FindFile_Drv]

                ; Check converted FS dir entry sign              
                cmp word ptr [SI][DirEntry_NTRes], 01A1h 
                jnb short loc_get_FS_long_name

                retn

loc_get_FS_long_name:
                mov bx, ax               
                mov ax, word ptr [SI][DirEntry_FstClusLO]
                mov dx, word ptr [SI][DirEntry_FstClusHI]

                mov si, offset Logical_DOSDisks
                add si, bx
               
                call proc_get_next_section
                jnc short loc_move_fs_long_name

                and al, al ; If cf=1 and ax=0 there is no error,
                          ; it is the last section.
                          ; If cf=1 and al>0, error code is in al 
                jz short loc_move_fs_long_name

               ;xor al, al
                mov ah, byte ptr [FindFile_Drv]

                stc
                retn

loc_move_fs_long_name:
                push ds
                mov bx, word ptr [Sector_Buffer]
                mov ds, bx
                mov si, FS_FDT_File_Name
                mov cx, 64
                mov di, Offset LongFileName
                push di 
                rep movsb
                mov ax, cx ; 0
                stosw
                pop si 
                pop ds

                retn

proc_get_fs_longname endp

proc_allocate_fs_section_buffer proc near
               ; 23/04/2010 separated from
               ; "proc_load_FS_sub_directory", 18/01/2010
               ; INPUT: DX:AX -> Section Address
               ;        BX = Section Buffer Address
               ; INPUT: DS:SI -> Logical DOS Drv Desc. Table Addr.
               ; OUTPUT:
               ;        DS:SI -> Same
               ;        DX:AX -> Next Section (Header) Adress
               ;        BX -> FS Section (Directory) Buffer Segment
               ;          (may be different than BX input value)  
               ;        CX -> Sector count of section buffer (BX)
               ; stc -> Error Code in AL      
               ;

               mov word ptr [FS_Section_Buffer], bx
               mov word ptr [FS_Section_Header], ax
               mov word ptr [FS_Section_Header]+2, dx

loc_allocate_fs_sb_get_next_section:
               call proc_get_next_section
               jnc short loc_save_next_fs_section_header_address

               and al, al ; If cf=1 and ax=0 there is no error,
                          ; it is the last section.
                          ; If cf=1 and al>0, error code is in al 
               jz short loc_save_next_fs_section_header_address

loc_get_next_FS_section_xor_stc_retn:
               xor bx, bx
               stc
               retn

loc_save_next_fs_section_header_address:
              ;18/01/2010
              ;bx:cx = sector count of current section
              ;
               or bx, bx ; bx>0 -> Very big dir sector count
                         ; nonsence but valid !
               jnz short loc_allocate_fs_section_sc_mov_cx_128

               cmp cx, 127
               ja short loc_allocate_fs_section_sc_mov_cx_128
               or cx, cx
               jnz short loc_allocate_fs_section_sc_mov_cx_ax

               mov ax, 0Bh ; Invalid format

               stc
               retn

loc_allocate_fs_section_sc_mov_cx_128:
               xor ax, ax
               mov word ptr [FS_Next_Section_Header], ax
               mov word ptr [FS_Next_Section_Header]+2, ax
               mov word ptr [FS_SecBuff_SectorCount], 128
               mov cx, 65535 ; max. 64K bytes - 1
               jmp short loc_allocate_fs_section_buffer_0

loc_allocate_fs_section_sc_mov_cx_ax:
               mov word ptr [FS_Next_Section_Header], ax
               mov word ptr [FS_Next_Section_Header]+2, dx
               mov word ptr [FS_SecBuff_SectorCount], cx
               mov ax, 512
               mul cx
               mov cx, ax
loc_allocate_fs_section_buffer_0:
               mov bx, word ptr [FS_Section_Buffer] ; Segment
  	       or bx, bx
               jz short loc_allocate_fs_section_buffer_1
  
               cmp word ptr [FS_SecBuffer_Size], cx
               je short loc_allocate_fs_section_buffer_next
               
               mov word ptr [FS_SecBuffer_Size], cx
               mov ax, 0106h ;Consequently for 06h allocation
               call proc_deallocate_memory

               mov cx, word ptr [FS_SecBuffer_Size]
               xor bx, bx
               mov word ptr [FS_Section_Buffer], bx ; 0

loc_allocate_fs_section_buffer_1:
               mov ax, 0106h ;Consequently allocate FS original dir buffer
               mov dx, 1 ; 512 byte segment boundary check
               call proc_allocate_memory
               jnc short loc_allocate_fs_section_buffer_reset

loc_allocate_fs_section_buffer_err:
               mov ax, 8 ; NO SPACE TO ALLOCATE DIRECTORY BUFFER !
               jmp short loc_get_next_FS_section_xor_stc_retn

loc_allocate_fs_section_buffer_reset:
               mov word ptr [FS_Section_Buffer], bx

loc_allocate_fs_section_buffer_next:
               push es
               mov es, bx
               xor di, di
               xor al, al
               ; cx = section buffer size in bytes 
               rep stosb ; Reset section buffer 
               pop es 

loc_allocate_fs_section_buffer_next_1:
               ; DS:SI = DOS Drive Description Table
               mov ax, word ptr [FS_Section_Header]
	       mov dx, word ptr [FS_Section_Header]+2

               add ax, word ptr [SI][LD_FS_BeginSector]
               adc dx, word ptr [SI][LD_FS_BeginSector]+2

               add ax, 1 ; to point dir data sector (header+1)
               adc dx, 0

               mov cx, word ptr [FS_SecBuff_SectorCount]
	       push cx
               push es
               mov es, bx
               xor bx, bx
               call proc_disk_read
               pop es
               mov bx, word ptr [FS_Section_Buffer]
               jnc short loc_allocate_fs_section_buffer_retn
               pop ax ; pushed cx
               sub ax, cx
               or  ax, ax
               jnz short pass_allocate_fs_section_buffer_reset_bx
               xor bx, bx
pass_allocate_fs_section_buffer_reset_bx:
               mov cx, 15h ; DRV NOT READY OR READ ERROR !
               xchg ax, cx ; CX = Number of loaded sectors
               stc
               retn

loc_allocate_fs_section_buffer_retn:
               pop cx
               mov ax, word ptr [FS_Next_Section_Header]
               mov dx, word ptr [FS_Next_Section_Header]+2
               ; cx = sector_count
               ; bx = FS_Section_Buffer address
               retn

FS_Next_Section_Header: dd 0
FS_Section_Header: dd 0
FS_Section_Buffer: dd 0
FS_SecBuffer_Size: dw 0
FS_SecBuff_SectorCount: dw 0

proc_allocate_fs_section_buffer endp

proc_change_fs_file_attributes proc near
              ; 05/06/2010 changing directory attributes ok (Bugfix)
              ; 26/05/2010
              ; 23/05/2010
              ; INPUT -> 
              ;DS:SI = Logical DOS Drv Desc. table 
              ;ES:DI= Directory Entry offset (in Compatibility Buffer)
              ;AL = New Attributes
              ;AH = 0 => AL has MS-DOS attributes not in FS format
              ;If AH > 0, AL has file attributes in FS format
              ;OUTPUT -> 
              ;DS:SI = Logical DOS Drv Desc. table 
              ;ES:DI= Directory Entry offset (in Compatibility Buffer)
              ;DX:AX= FDT Address (Valid if DirEntry_NTRES is 01A1h)
              ;CL = New Attributes
              ;CH = 0 if file attributes (CL) byte is in MS-DOS format
              ;CH > 0 if file attributes (CL) byte is in FS format
              ;BX = FDT (or DDT) Buffer Segment
              ;STC -> Error code in AL 
              ;      (DS, ES, SI, DI will not be changed)

              or ah, ah
              jnz short pass_convert_dos_type_file_attributes_to_fs
              dec ah ; mov ah, 0FFh
loc_convert_dos_type_file_attributes_to_fs:
              test al, 1 ; Read-Only file attribute
              jz short loc_check_s_file_attribute
              and ah, 10110110b ; No write permission
loc_check_s_file_attribute:
              test al, 4  ; System file attribute
              jz short loc_check_h_file_attribute
              and ah, 7 ; No permission to W and R/W to Group and others
loc_check_h_file_attribute:
              test al, 2  ; Hidden file attribute
              jz short pass_convert_sh_file_attribute_to_fs
             ; 26/05/2010
              and ah, 10011b ; No read permission to Group and other users
pass_convert_sh_file_attribute_to_fs:
              mov al, ah
              xor ah, ah
pass_convert_dos_type_file_attributes_to_fs:
              mov word ptr [FS_FileAttributes], ax
              
loc_change_fs_file_attributes_load_fdt:
              mov ax, word ptr ES:[DI]+DirEntry_FstClusLO
              mov dx, word ptr ES:[DI]+DirEntry_FstClusHI
 
              push es
              mov bx, word ptr [Sector_Buffer]
              mov es, bx

              xor bx, bx
              mov byte ptr [FS_BuffValidData], bl
              mov word ptr [FS_CurrentSection], ax
              mov word ptr [FS_CurrentSection]+2, dx
             
              or ax, ax
              jnz short pass_loc_ch_fs_file_attr_check_file_size
              or dx, dx
              jnz short pass_loc_ch_fs_file_attr_check_file_size
loc_change_file_attr_pop_es_stc_retn:
              pop es
              mov al, 0Dh
              stc
              retn

loc_change_attr_file_not_found:
              mov ax, 02h ; File Not Found error
              stc
loc_change_fs_file_attributes_pop_es_retn:
              pop es
              retn

pass_loc_ch_fs_file_attr_check_file_size:
              mov word ptr [FS_Attr_FNumber], ax
              mov word ptr [FS_Attr_FNumber]+2, dx
   
              add ax, word ptr [SI][LD_FS_BeginSector]
              adc dx, word ptr [SI][LD_FS_BeginSector]+2

             ;xor bx, bx
              xor cx, cx 
             ;inc cx
              inc cl      
              call proc_disk_read
              jc short loc_change_fs_file_attributes_pop_es_retn

loc_change_file_attr_write_fdt_ddt:
              mov byte ptr [FS_BuffValidData], 1
            ; 05/06/2010 
              mov al, byte ptr ES:[BX]
              cmp al, 'F'
              je short loc_change_file_attr_write_fdt
              cmp al, 'D'
              jne short loc_change_attr_file_not_found
loc_change_file_attr_write_fdt:
              inc bx ; 1
              cmp word ptr ES:[BX], 'TD'
              jne short loc_change_attr_file_not_found
              mov bx, FS_FDT_Attributes
             ; 26/05/2010  
              mov al, byte ptr [FS_FileAttributes]
              mov byte ptr ES:[BX], al
             ; 
              mov ax, word ptr [FS_Attr_FNumber]
              mov dx, word ptr [FS_Attr_FNumber]+2

              add ax, word ptr [SI][LD_FS_BeginSector]
              adc dx, word ptr [SI][LD_FS_BeginSector]+2

              xor bx, bx
             ; mov cx, 1
              ; DX:AX = FDT sector, ES:BX = Buffer
              ; DS:SI = DOS Drv Description Table Addr

              mov byte ptr [FS_BuffValidData], bl ; 0
              call proc_disk_write
              jc short loc_change_fs_file_attributes_pop_es_retn

loc_change_file_attr_check_fdt_sign_1:
              mov al, byte ptr ES:[BX]
              xor ah, ah
              cmp al, 'F'
              je short loc_change_fs_file_attr_set_dos_attributes_1
              cmp al, 'D'
              je short loc_change_file_attr_check_fdt_sign_2
             ; 25/05/2010 
              or ah, 4h ; system file sign
              cmp al, 'R'
              jne short loc_change_fs_file_attr_set_dos_attributes_1
loc_change_file_attr_check_fdt_sign_2:
              or ah, 10h ; Directory
loc_change_fs_file_attr_set_dos_attributes_1:
              mov bx, FS_FDT_Attributes
              mov al, byte ptr ES:[BX]
              shr al, 1 ; cf -> owner's write permission, 0 = read only
              cmc
              adc ah, 0
             ; 26/05/2010
              shr al, 1 ; Owner's execute permission bit 
              jnc short loc_change_fs_file_attr_set_dos_attributes_4
              test al, 4 ; Group User's execute permission bit
              jnz short loc_change_fs_file_attr_set_dos_attributes_4
              or ah, 4 ; system file sign 
             ;
loc_change_fs_file_attr_set_dos_attributes_4:
              shr al, 1 ;  cf -> group user's read permission, 0 = hidden    
              jc short loc_change_fs_file_attr_set_dos_attributes_2
              or ah, 2 ; hidden file sign
loc_change_fs_file_attr_set_dos_attributes_2:
              pop es
              mov byte ptr ES:[DI]+DirEntry_Attr, ah
              mov cx, word ptr [FS_FileAttributes]
              or ch, ch
              jnz short loc_change_fs_file_attr_set_dos_attributes_3
              mov cl, ah ; Set MS-DOS file attributes to return
loc_change_fs_file_attr_set_dos_attributes_3:                 
              mov bx, word ptr [Sector_Buffer]
              mov ax, word ptr [FS_Attr_FNumber]
	      mov dx, word ptr [FS_Attr_FNumber]+2

              retn

;23/05/2010
FS_FileAttributes: dw 0
FS_Attr_FNumber: dd 0

proc_change_fs_file_attributes endp

proc_delete_fs_file proc near
               ; 12/04/2011
               ; 10/04/2011 Directory entry deleting code is moved to
               ;            proc_delete_fs_directory_entry procedure
               ;            for rename file compatibility  
               ; 20/11/2010
               ; 03/07/2010 
               ; 26/06/2010
               ; 20/06/2010 "push ds, pop es" modification
               ; 22/05/2010
               ; 16-17-19/05/2010
               ; 09/05/2010 BugFix (xor bh, bh -> xor bl, bl)
               ; 02/05/2010, 01/05/2010
               ; 18/04/2010
               ; INPUT -> 
               ; DS:SI= Dos Drive Description Table Address
               ; ES:DI= Directory Entry offset (in Compatibility Buffer)
               ;

              ; 10/04/2011
               mov bl, 'F' 
               call proc_delete_fs_directory_entry
               jc short loc_delete_fs_file_retn

              ; mov word ptr [Delete_FS_FNumber], ax
	      ; mov word ptr [Delete_FS_FNumber]+2, dx

loc_delete_fs_file_load_fdt:
               mov bx, word ptr [Sector_Buffer]
               mov es, bx

              ; 01/05/2010 (FS_Sector_Buffer control parameters)
               xor bx, bx
               mov byte ptr [FS_BuffValidData], bl
               mov word ptr [FS_CurrentSection], ax
               mov word ptr [FS_CurrentSection]+2, dx
              ;

               add ax, word ptr [SI][LD_FS_BeginSector]
               adc dx, word ptr [SI][LD_FS_BeginSector]+2

               xor cx, cx 
              ;inc cx
               inc cl      
               call proc_disk_read
               jc short loc_delete_fs_file_retn

loc_delete_fs_file_set_fde:
              ; 10/04/2011
               mov ax, word ptr [Delete_FS_FNumber]
	       mov dx, word ptr [Delete_FS_FNumber]+2
              ; 01/05/2010
               mov byte ptr [FS_BuffValidData], 1
              ;
               cmp word ptr ES:[BX], 'DF'
               jne short loc_delete_fs_file_not_found
               inc bx ; 1
               inc bx ; 2
               cmp byte ptr ES:[BX], 'T'
               jne short loc_delete_fs_file_not_found
               mov byte ptr ES:[BX], 'E'
              ;add bx, 6 ; bx = 8
               add bl, 6
               cmp ax, word ptr ES:[BX]
               jne short loc_delete_fs_file_not_found
 	       cmp dx, word ptr ES:[BX]+2
              ; 22/05/2010
               je short loc_delete_fs_file_write_fde

              ; 22/05/2010
loc_delete_fs_file_not_found:
               mov dx, ax
               mov ax, 02h ; File Not Found error
               mov bx, word ptr [Sector_Buffer]
               xor cx, cx
               stc
              ; 20/06/2010  
               retn   

loc_delete_fs_file_retn:
              ; 20/06/2010 'push ds, pop es' is not needed
;loc_delete_fs_file_push_ds_pop_es_retn:
               ;push ds
               ;pop es
               retn  

loc_delete_fs_file_write_fde:
               add ax, word ptr [SI][LD_FS_BeginSector]
               adc dx, word ptr [SI][LD_FS_BeginSector]+2

              ; 16/05/2010
               mov bx, FS_FDT_Next_FDT_Number
               mov cx, word ptr ES:[BX]
               mov word ptr [Delete_FS_FNextSection], cx
               mov cx, word ptr ES:[BX]+2
               mov word ptr [Delete_FS_FNextSection]+2, cx
               mov bx, FS_FDT_Sector_Count
               mov cx, word ptr ES:[BX]
               mov word ptr [Delete_FS_FScount], cx
               mov cx, word ptr ES:[BX]+2
               mov word ptr [Delete_FS_FScount]+2, cx  
              ;
	       xor bx, bx
               mov cx, bx
              ;inc cx ; 1
               inc cl
               ; DX:AX = FDT sector, ES:BX = Buffer
               ; DS:SI = DOS Drv Description Table Addr

              ; 01/05/2010
               mov byte ptr [FS_BuffValidData], bl ; 0
              ; 20/11/2010 
               mov byte ptr [DirBuff_ValidData], bl ; 0 -> RESET
               call proc_disk_write
              ; 20/06/2010
               jc short loc_delete_fs_file_retn

loc_delete_fs_file_update_dat:
              ; 16/05/2010 
               mov ax, word ptr [Delete_FS_FNumber]
               mov dx, word ptr [Delete_FS_FNumber]+2
               xor bx, bx
              ;mov word ptr [Delete_FS_FSCount_Total], 0
	      ;mov word ptr [Delete_FS_FSCount_Total]+2, 0
               mov word ptr [Delete_FS_FSCount_Total], bx
	       mov word ptr [Delete_FS_FSCount_Total]+2, bx
loc_delete_fs_file_update_dat_next_2: 
               mov cx, word ptr [Delete_FS_FSCount]
              ; xor bx, bx 
               dec bx ; mov bx, 0FFFFh
               mov word ptr [DAT_Buffer_Sector], bx ; FFFFh
               mov word ptr [DAT_Buffer_Sector]+2, bx ; FFFFh
               inc bx
              ;mov word ptr [[DAT_Buffer_Offset], bx ; 0
               mov byte ptr [DAT_Buffer_Updated], bl ; 0
               add cx, 1 ; + FDT
               adc word ptr [Delete_FS_FSCount]+2, bx ; 0
               mov word ptr [Delete_FS_FSCount], cx
               add word ptr [Delete_FS_FSCount_Total], cx
	       adc word ptr [Delete_FS_FSCount_Total]+2, bx
               mov bl, 90h ; Free sector sign
loc_delete_fs_file_update_dat_next_1:
              ; 18/05/2010
              ; 16/05/2010
              ; push ax
              ; push dx   
               call proc_update_dat
              ; pop dx
              ;jc short loc_delete_fs_file_update_dat_pop_dx_retn
              ; 20/06/2010
               jc short loc_delete_fs_file_update_dat_retn
              ; pop ax
               dec word ptr [Delete_FS_FSCount]
               loop loc_delete_fs_file_update_dat_next_0
               cmp word ptr [Delete_FS_FSCount]+2, 0
               jna short loc_delete_fs_file_ud_check_next_section
               dec word ptr [Delete_FS_FSCount]+2
loc_delete_fs_file_update_dat_next_0:
              ; 18/05/2010
               add ax, 1
               adc dx, 0
               jmp short loc_delete_fs_file_update_dat_next_1

loc_delete_fs_file_ud_check_next_section:
              ;cmp byte ptr [DAT_Buffer_Updated], 0
              ;jna short loc_delete_fs_file_check_next_fdt_address

               xor bx, bx
               mov ax, word ptr [DAT_Buffer_Sector]
               mov dx, word ptr [DAT_Buffer_Sector]+2
               call proc_write_dat_sector
             ; 22/05/2010
               jnc short loc_delete_fs_file_check_next_fdt_address

              ; 20/06/2010 'pus ds, pop es' is not needed
;loc_delete_fs_file_update_dat_push_ds_pop_es_retn:
;              push ds
;              pop es
loc_delete_fs_file_update_dat_retn:
               retn

loc_delete_fs_file_check_next_fdt_address:
               mov ax, word ptr [Delete_FS_FNextSection]
               mov dx, word ptr [Delete_FS_FNextSection]+2
               or ax, ax
               jnz loc_delete_fs_file_ud_load_next_fdt
               or dx, dx
               jnz loc_delete_fs_file_ud_load_next_fdt

loc_delete_fs_file_update_mat:
              ; 16/05/2010
    
               mov cx, word ptr [Delete_FS_FNumber]
               mov bx, word ptr [Delete_FS_FNumber]+2

               CALL PROC_GECICIKOD

              ; 19/05/2010
               call proc_load_mat
              ; 20/06/2010
               jc short loc_delete_fs_file_update_dat_retn

               mov ax, word ptr [Delete_FS_FSCount_Total]
               mov dx, word ptr [Delete_FS_FSCount_Total]+2

               add ax, word ptr ES:[BX]+FS_MAT_FreeSectors
               adc dx, word ptr ES:[BX]+FS_MAT_FreeSectors+2

               mov word ptr [SI][LD_FreeSectors], ax
               mov word ptr [SI][LD_FreeSectors]+2, dx  

               mov word ptr ES:[BX]+FS_MAT_FreeSectors, ax
               mov word ptr ES:[BX]+FS_MAT_FreeSectors+2, dx

               mov ax, word ptr [Delete_FS_FNumber]
	       mov dx, word ptr [Delete_FS_FNumber]+2

               cmp dx, word ptr ES:[BX]+FS_MAT_FirstFreeSector+2
               ja short loc_delete_fs_file_pass_change_mat_ffs
               jb short loc_delete_fs_file_change_mat_ffs
               cmp ax, word ptr ES:[BX]+FS_MAT_FirstFreeSector
               jnb short loc_delete_fs_file_pass_change_mat_ffs
              
loc_delete_fs_file_change_mat_ffs:
	       mov word ptr ES:[BX]+FS_MAT_FirstFreeSector, ax
               mov word ptr ES:[BX]+FS_MAT_FirstFreeSector+2, dx
               
loc_delete_fs_file_pass_change_mat_ffs: 
               call proc_write_mat 
               jc short loc_delete_fs_file_ud_retn

loc_delete_fs_file_ud_retn:
               retn

loc_delete_fs_file_ud_load_next_fdt:
              ;mov bx, word ptr [Sector_Buffer]
              ;mov es, bx

              ; 01/05/2010 (FS_Sector_Buffer control parameters)
               xor bl, bl
               mov byte ptr [FS_BuffValidData], bl
               mov word ptr [FS_CurrentSection], ax
               mov word ptr [FS_CurrentSection]+2, dx
              ;

              ; 17/05/2010 -> PUSH AX, PUSH DX -> Absolute disk address 
               add ax, word ptr [SI][LD_FS_BeginSector]
               adc dx, word ptr [SI][LD_FS_BeginSector]+2

	       push ax
               push dx
              ;    
               xor bh, bh
               xor cx, cx 
              ;inc cx
               inc cl      
               call proc_disk_read
               pop dx
               jnc short loc_delete_fs_ud_next_fdt_set_fde
               pop dx
              ; 20/06/2010
               retn

loc_delete_fs_ud_next_fdt_set_fde:
               pop ax
               mov byte ptr [FS_BuffValidData], 1
               cmp word ptr ES:[BX], 'DF'
               jne short loc_delete_fs_file_ud_retn
               inc bx ; 1
               inc bx ; 2
               cmp byte ptr ES:[BX], 'T'
               jne short loc_delete_fs_file_ud_retn
               mov byte ptr ES:[BX], 'E'
               add bx, 4 ; bx = 6 ; FS_FDT_Number
               cmp word ptr ES:[BX], 0
               jna short loc_delete_fs_file_ud_retn

               mov bx, FS_FDT_Next_FDT_Number
               mov cx, word ptr ES:[BX]
               mov word ptr [Delete_FS_FNextSection], cx
               mov cx, word ptr ES:[BX]+2
               mov word ptr [Delete_FS_FNextSection]+2, cx
               mov bx, FS_FDT_Sector_Count
               mov cx, word ptr ES:[BX]
               mov word ptr [Delete_FS_FScount], cx
               mov cx, word ptr ES:[BX]+2
               mov word ptr [Delete_FS_FScount]+2, cx  

	       xor bx, bx
               mov cx, bx
              ;inc cx ; 1
               inc cl
               ; DX:AX = FDT sector, ES:BX = Buffer
               ; DS:SI = DOS Drv Description Table Addr

              ; 01/05/2010
               mov byte ptr [FS_BuffValidData], bl ; 0
              ; 17/05/2010 DX:AX -> Absolute FDT address
               call proc_disk_write
              ; 19/05/2010
               jc short loc_delete_fs_file_ud_retn
               mov ax, word ptr [FS_CurrentSection]
               mov dx, word ptr [FS_CurrentSection]+2
               jmp loc_delete_fs_file_update_dat_next_2 

proc_delete_fs_file endp

proc_find_fs_dir_entry_with_number proc near
               ; 04/07/2010  
               ; 03/07/2010 major modification
               ; 30/06/2010, 28/06/2010 
               ; 08/05/2010, 09/05/2010
               ; 03/05/2010, 02/05/2010, 01/05/2010
               ;  
               ; INPUT -> DX:AX = File (Entry) Number
               ;        DS:SI => Logical DOS Drv Table Addr
               ;        ES = Directory DT Buffer Segment (1 sector)
               ;        BX = 0
 	       ; OUTPUT -> DX:AX = Data Sector Adress
               ;        BX:CX = File (Entry) Number
               ;        DS:SI = Logical DOS Drv Table Addr
               ;        ES = Sector Buffer Segment
               ;        DI = Entry Offset
               ;        stc -> Error code in AL
               ;        BX:CX = The last file number (0=End)
               ;        If BX:CX is not valid (0 or FFFFFFFFh)
               ;        file is not found or not valid file no.
               ; OUTPUT -> Input buffer content will be changed !   
               ;

loc_find_fs_direntry_check_ddt:
               mov word ptr [Find_FS_direntry_number], ax
               mov word ptr [Find_FS_direntry_number]+2, dx

              ;xor bx, bx
               xor cx, cx
               ;or bx, bx
               ;jnz short loc_find_fs_direntry_invalid_format_retn

               mov word ptr [Find_FS_directory_tscount], cx ; 0
              ;cmp word ptr ES:[BX]+1, 'TD'
              ;jne short loc_find_fs_direntry_invalid_format_retn
              ;cmp byte ptr ES:[BX], 'D'
              ;je short pass_find_fs_direntry_check_ddt
              ;cmp byte ptr ES:[BX], 'R'
              ;jne short loc_find_fs_direntry_invalid_format_retn

pass_find_fs_direntry_check_ddt:
               mov ax, word ptr ES:[BX]+FS_DDT_Next_DDT_Number 
	       mov dx, word ptr ES:[BX]+FS_DDT_Next_DDT_Number+2
               mov word ptr [Find_FS_directory_nextsection], ax 
               mov word ptr [Find_FS_directory_nextsection]+2, dx

               mov ax, word ptr ES:[BX]+FS_DDT_Directory_Number
               mov dx, word ptr ES:[BX]+FS_DDT_Directory_Number+2
 
               cmp word ptr ES:[BX]+FS_DDT_Sector_Count+2, cx ; 0
               je short loc_find_fs_direntry_check_scount
               dec cx ; FFFFh
               jmp short pass_find_fs_direntry_check_scount

loc_find_fs_direntry_check_scount:
               mov cx, word ptr ES:[BX]+FS_DDT_Sector_Count
               or cx, cx
               jz short loc_find_fs_direntry_invalid_format_retn

pass_find_fs_direntry_check_scount:
               mov word ptr [Find_FS_directory_scount], cx

loc_find_fs_direntry_load_next_sector:
              ; 03/07/2010
               xor di, di
               add ax, 1
               adc dx, 0
               mov word ptr [Find_FS_directory_sector], ax
               mov word ptr [Find_FS_directory_sector]+2, dx
               add ax, word ptr [SI][LD_FS_BeginSector]
               adc dx, word ptr [SI][LD_FS_BeginSector]+2
              ;xor bx, bx
               xor cx, cx 
               inc cl ; mov cl, 1
               call proc_disk_read
               jc short loc_find_fs_direntry_retn

loc_find_fs_direntry_check_entry_next:
               mov cx, word ptr ES:[DI]
	       mov bx, word ptr ES:[DI]+2
               or cx, cx
               jnz short loc_find_fs_direntry_not_zero
               or bx, bx
               jz short loc_find_fs_direntry_not_found_retn
loc_find_fs_direntry_not_zero:
               cmp cx, word ptr [Find_FS_direntry_number]
               jne short loc_find_fs_direntry_check_entry_n1
               cmp bx, word ptr [Find_FS_direntry_number]+2
               jne short loc_find_fs_direntry_check_entry_n2

loc_find_fs_direntry_check_entry_retn:
               mov ax, word ptr [Find_FS_directory_sector]
               mov dx, word ptr [Find_FS_directory_sector]+2
loc_find_fs_direntry_retn:
               retn 

loc_find_fs_direntry_invalid_format_retn:
              ;xor dx, dx
               mov ax, 0Bh ; invalid format
               stc
               retn

loc_find_fs_direntry_not_found_retn:
              ;xor dx, dx
               mov ax, 02h ; file/directory not found
               stc
               retn

loc_find_fs_direntry_check_entry_n1:
               cmp di, 508
               jnb short loc_find_fs_direntry_check_entry_n2
               add di, 4
               jmp short loc_find_fs_direntry_check_entry_next

loc_find_fs_direntry_check_entry_n2:
               inc word ptr [Find_FS_directory_tscount]
               jz short loc_find_fs_direntry_not_found_retn
               dec word ptr [Find_FS_directory_scount]
               jnz short loc_find_fs_direntry_load_next_sector

loc_find_fs_direntry_check_entry_n3:
              ; 03/07/2010
               mov ax, word ptr [Find_FS_directory_nextsection]
	       mov dx, word ptr [Find_FS_directory_nextsection]+2
               or ax, ax
               jnz short loc_find_fs_direntry_load_next_section
               or dx, dx
               jz short loc_find_fs_direntry_not_found_retn

loc_find_fs_direntry_load_next_section:
               ;03/07/2010
               add ax, word ptr [SI][LD_FS_BeginSector]
               adc dx, word ptr [SI][LD_FS_BeginSector]+2
               xor bx, bx
               xor cx, cx 
               inc cl ; mov cl, 1
               call proc_disk_read
               jc short loc_find_fs_direntry_retn

loc_find_fs_direntry_check_next_ddt:
               dec cl  ; cl = 0
               cmp word ptr ES:[BX]+1, 'TD'
               jne short loc_find_fs_direntry_invalid_format_retn
               cmp byte ptr ES:[BX], 'D'
               je short loc_find_fs_direntry_get_cs_scount
               cmp byte ptr ES:[BX], 'R'
               jne short loc_find_fs_direntry_invalid_format_retn

loc_find_fs_direntry_get_cs_scount:
               mov ax, word ptr [Find_FS_directory_nextsection]
	       mov dx, word ptr [Find_FS_directory_nextsection]+2

               cmp word ptr ES:[BX]+FS_DDT_Sector_Count+2, 0 
               jz short pass_find_fs_direntry_get_cs_scount
              ;xor cx, cx ; mov cx, 0
               mov word ptr [Find_FS_directory_nextsection], cx ; 0
               mov word ptr [Find_FS_directory_nextsection]+2, cx ; 0
               dec cx ;  mov cx, 0FFFFh
               mov word ptr [Find_FS_directory_scount], cx  
               jmp loc_find_fs_direntry_load_next_sector

pass_find_fs_direntry_get_cs_scount:
               mov cx, word ptr ES:[BX]+FS_DDT_Sector_Count
               or cx, cx
               jz short loc_find_fs_direntry_invalid_format_retn
               mov word ptr [Find_FS_directory_scount], cx
               jmp loc_find_fs_direntry_load_next_sector

Find_FS_direntry_number: dd 0
Find_FS_directory_sector: dd 0 
Find_FS_directory_nextsection: dd 0
Find_FS_directory_scount: dw 0
Find_FS_directory_tscount: dw 0

proc_find_fs_dir_entry_with_number endp

proc_delete_fs_directory proc near
               ; 20/11/2010
               ; 05/07/2010
               ; 03-04/07/2010
               ; 20-26-27/06/2010
               ; derivation from proc_delete_fs_file (22/05/2010) 
               ; INPUT -> 
               ; DS:SI= Dos Drive Description Table Address
               ; DX:AX= DDT Address (Valid if DirEntry_NTRES is 01A1h)
               ; ES:DI= Directory Entry offset (in Compatibility Buffer)
               ;

;loc_delete_fs_directory_dt:
;              cmp word ptr ES:[DI]+DirEntry_NTRes, 01A1h
;              jb short loc_delete_fs_directory_permission_denied
              ;cmp ax, word ptr ES:[DI]+DirEntry_FstClusLO
              ;jne short loc_delete_fs_directory_permission_denied_stc
              ;cmp dx, word ptr ES:[DI]+DirEntry_FstClusHI
              ;jne short loc_delete_fs_directory_permission_denied_stc 

              ; 05/07/2010
               mov word ptr [Delete_FS_DCB_Offset], di
               push es               
               pop word ptr [Delete_FS_DCB_Segment]
              ; 

;              mov bl, byte ptr ES:[DI]+DirEntry_Attr  
;              and bl, 17h ; Attributes
;              cmp bl, 10h ; Directory but no S,H,R 
;              ja short loc_delete_fs_directory_permission_denied
;              je short loc_delete_fs_directory_load_ddt
;loc_delete_fs_directory_permission_denied_stc:
;              stc
;loc_delete_fs_directory_permission_denied:
;              mov ah, byte ptr ES:[DI]+DirEntry_Attr
;              mov al, 05h ; Access denied error, (Permission denied !)
;loc_delete_fs_directory_pop_ds_pop_es_retn:
;              push ds
;              pop es
;              retn

loc_delete_fs_directory_load_ddt:
               mov bx, word ptr [Sector_Buffer]
               mov es, bx

               xor bx, bx
               mov byte ptr [FS_BuffValidData], bl
               mov word ptr [FS_CurrentSection], ax
               mov word ptr [FS_CurrentSection]+2, dx

  	       push ax
               push dx    

               add ax, word ptr [SI][LD_FS_BeginSector]
               adc dx, word ptr [SI][LD_FS_BeginSector]+2

               xor cx, cx 
              ;inc cx
               inc cl      
               call proc_disk_read
               pop dx
               jnc short loc_delete_fs_directory_check_ddt
               pop dx

loc_delete_fs_directory_retn:
;loc_delete_fs_directory_pop_ds_pop_es_retn:
              ;push ds
              ;pop es
               retn

loc_delete_fs_directory_check_ddt:
               pop ax
               mov byte ptr [FS_BuffValidData], 1
               cmp word ptr ES:[BX], 'DD'
               jne short loc_delete_fs_directory_not_found
               inc bx ; 1
               inc bx ; 2
               cmp byte ptr ES:[BX], 'T'
               jne short loc_delete_fs_directory_not_found
              ;add bx, 6 ; bx = 8
               add bl, 6 
               cmp ax, word ptr ES:[BX]
               jne short loc_delete_fs_directory_not_found
               cmp dx, word ptr ES:[BX]+2
               je short loc_delete_fs_dir_check_last_section

loc_delete_fs_directory_not_found:
               mov dx, ax
               mov ax, 02h ; File Not Found error

loc_delete_fs_dir_path_not_found:
               mov bx, word ptr [Sector_Buffer]
               xor cx, cx
               stc
               retn

loc_delete_fs_dir_check_last_section:
              ; 20/06/2010
               mov word ptr [Delete_FS_FNumber], ax
	       mov word ptr [Delete_FS_FNumber]+2, dx

              ; 26/06/2010
               mov bx, FS_DDT_Parent_Dir_Number
               mov cx, word ptr ES:[BX]
               mov word ptr [Delete_FS_ParentDir], cx
               mov cx, word ptr ES:[BX]+2
               mov word ptr [Delete_FS_ParentDir]+2, cx

loc_delete_fs_dir_check_last_section_next:  ; 26/06/2010
              ; 20/06/2010 
               call proc_get_last_section
               jc short loc_delete_fs_directory_retn

               cmp ax, word ptr [Delete_FS_FNumber]
               jne short loc_delete_fs_dir_multi_dir_sections
               cmp dx, word ptr [Delete_FS_FNumber]+2
               jne short loc_delete_fs_dir_multi_dir_sections
               mov byte ptr [RmDir_MultiClusters], 0
               ; 20/11/2010
               mov byte ptr [DirBuff_ValidData], 0 ; RESET
               jmp short pass_delete_fs_dir_multi_dir_sections 
loc_delete_fs_dir_multi_dir_sections:
                mov byte ptr [RmDir_MultiClusters], 1
pass_delete_fs_dir_multi_dir_sections:
                mov word ptr [RmDir_DirLastCluster], ax
                mov word ptr [RmDir_DirLastCluster]+2, dx
                mov word ptr [RmDir_PreviousCluster], cx
                mov word ptr [RmDir_PreviousCluster]+2, bx
loc_delete_fs_dir_check_last_ddt_sec_buff:
               ; 26/06/2010
               ; mov di, word ptr [Sector_Buffer]
               ; mov es, di
                xor di, di
                cmp byte ptr ES:[DI], 'D'
                je short loc_delete_fs_dir_load_dir_sector_0
loc_delete_fs_dir_chk_lddt_stc_retn:
                mov dx, ax
                mov al, 0Dh ; Invalid Data
                xor ah, ah
                stc
                retn 

loc_delete_fs_dir_load_dir_sector_0:
                inc di
                cmp word ptr ES:[DI], 'TD'
                jne short loc_delete_fs_dir_chk_lddt_stc_retn
                mov di,  FS_DDT_Sector_Count
                mov cx, word ptr ES:[DI]
                mov bx, word ptr ES:[DI]+2
               ; 27/06/2010 
                mov word ptr [Delete_FS_FSCount], cx
               ;
                or bx, bx
                jz short loc_delete_fs_dir_load_dir_sector_1
                mov dx, ax
                mov ax, 18h  ; MS-DOS Error Code:
                             ; Bad request structure length 
                             ; Very big directory data !!!
                stc
                retn

loc_delete_fs_dir_load_dir_sector_1:        
                or cx, cx    ; If cx = 0 -> Invalid data !
                jz short loc_delete_fs_dir_chk_lddt_stc_retn
 
loc_delete_fs_dir_load_dir_sector_2:  
                add ax, 1
                adc dx, 0
               ; xor bx, bx
               ; ES:BX = Sector buffer

               push ax
               push dx    
               push cx
 
               add ax, word ptr [SI][LD_FS_BeginSector]
               adc dx, word ptr [SI][LD_FS_BeginSector]+2

               xor cx, cx 
              ;inc cx
               inc cl      
               call proc_disk_read
               jnc short loc_delete_fs_dir_load_dir_sector_3
               pop cx
               pop dx
               pop dx
               retn                                 

loc_delete_fs_dir_load_dir_sector_3:
               xor di, di
               mov cx, 128
loc_delete_fs_dir_check_any_valid_entry:
               mov ax, word ptr ES:[DI]
               mov dx, word ptr ES:[DI]+2   
               or ax, ax
               jnz short loc_delete_fs_dir_check_entry_FFFFh
loc_delete_fs_dir_check_entry_dx:
               or dx, dx
               jz short loc_delete_fs_dir_empty_section
loc_delete_fs_dir_check_entry_FFFFh:
               cmp dx, 0FFFFh ; Is it deleted entry ? hw
               jne short loc_delete_fs_dir_not_empty
               cmp ax, 0FFFFh ; Is it deleted entry ? lw
               jne short loc_delete_fs_dir_not_empty
loc_delete_fs_dir_check_entry_loop:
               add di, 4
               loop loc_delete_fs_dir_check_any_valid_entry
               pop cx
               pop dx
               pop ax
               loop loc_delete_fs_dir_load_dir_sector_2
              ; End of directory data, all entries deleted !
               jmp short loc_delete_fs_dir_del_last_section

loc_delete_fs_dir_not_empty:
               pop cx
               pop dx
               pop dx  
               xor ax, ax; Stc & ax = 0 -> Dir Not Empty
               stc
               retn 

loc_delete_fs_dir_empty_section:
               pop cx
               pop dx
               pop ax
              ; 26/06/2010 
               xor cx, cx

loc_delete_fs_dir_del_last_section:
              ; 26/06/2010 
                mov ax, word ptr [RmDir_DirLastCluster]
                mov dx, word ptr [RmDir_DirLastCluster]+2

               ;xor bl, bl
                mov byte ptr [FS_BuffValidData], bl
                mov word ptr [FS_CurrentSection], ax
                mov word ptr [FS_CurrentSection]+2, dx

                add ax, word ptr [SI][LD_FS_BeginSector]
                adc dx, word ptr [SI][LD_FS_BeginSector]+2

                push ax
                push dx  

               ;xor bh, bh
               ;xor cx, cx  
               ;inc cx
                inc cl      
                call proc_disk_read
                pop dx
                jnc short loc_delete_fs_directory_check_last_ddt
                pop dx
loc_delete_fs_dir_last_ddt_diskio_err_retn:
             ; 26/06/2010
                retn

loc_delete_fs_directory_check_last_ddt:
               ; 26/06/2010
                pop ax
                mov byte ptr [FS_BuffValidData], 1
;                cmp word ptr ES:[BX], 'DD'
;                jne short loc_delete_fs_dir_last_ddt_invalid_data
;                cmp byte ptr ES:[BX]+2, 'T'
;                je short loc_delete_fs_directory_last_ddt_write_dde

;loc_delete_fs_dir_last_ddt_invalid_data:
;                mov dx, ax
;                mov al, 0Dh ; Invalid data !
;                xor ah, ah
;                stc
;                retn

loc_delete_fs_directory_last_ddt_write_dde:
                mov byte ptr  ES:[BX]+2, 'E'
                ; mov cx, 1
                call proc_disk_write                                
                jc short loc_delete_fs_dir_last_ddt_diskio_err_retn

               ; 27/06/2010

                cmp byte ptr [RmDir_MultiClusters], 0
                ja short pass_delete_fs_dir_put_0E5h_deleted_sign

loc_delete_fs_dir_put_0E5h_deleted_sign:
               ; 05/07/2010
                push es
                mov di, word ptr [Delete_FS_DCB_Offset]
                push word ptr [Delete_FS_DCB_Segment]
                pop es 
                mov byte ptr ES:[DI], 0E5h
                pop es

pass_delete_fs_dir_put_0E5h_deleted_sign:
               ;
                mov ax, word ptr [RmDir_DirLastCluster]
                mov dx, word ptr [RmDir_DirLastCluster]+2
loc_delete_fs_dir_update_dat:
               ;xor bx, bx
               ;mov word ptr [Delete_FS_FSCount_Total], 0
  	       ;mov word ptr [Delete_FS_FSCount_Total]+2, 0
                mov word ptr [Delete_FS_FSCount_Total], bx
	        mov word ptr [Delete_FS_FSCount_Total]+2, bx
                   
loc_delete_fs_dir_update_dat_next_2:
              ; 27/06/2010
               mov cx, word ptr [Delete_FS_FSCount]
              ;mov cx, word ptr ES:[BX]+FS_DDT_Sector_Sount
              ;
              ; 26/06/2010
              ; xor bx, bx 
               dec bx ; mov bx, 0FFFFh
               mov word ptr [DAT_Buffer_Sector], bx ; FFFFh
               mov word ptr [DAT_Buffer_Sector]+2, bx ; FFFFh
               inc bx
              ;mov word ptr [DAT_Buffer_Offset], bx ; 0
               mov byte ptr [DAT_Buffer_Updated], bl ; 0
               add cx, 1 ; + DDT
               adc word ptr [Delete_FS_FSCount]+2, bx ; 0
               mov word ptr [Delete_FS_FSCount], cx
               add word ptr [Delete_FS_FSCount_Total], cx
	       adc word ptr [Delete_FS_FSCount_Total]+2, bx
               mov bl, 90h ; Free sector sign
loc_delete_fs_dir_update_dat_next_1:
               call proc_update_dat
               jc short loc_delete_fs_dir_update_dat_retn
               dec word ptr [Delete_FS_FSCount]
               loop loc_delete_fs_dir_update_dat_next_0
               cmp word ptr [Delete_FS_FSCount]+2, 0
               jna short loc_delete_fs_dir_update_dat_w_dat_s
               dec word ptr [Delete_FS_FSCount]+2
loc_delete_fs_dir_update_dat_next_0:
               add ax, 1
               adc dx, 0
               jmp short loc_delete_fs_dir_update_dat_next_1

loc_delete_fs_dir_update_dat_retn:               
              ; 26/06/2010
               retn

loc_delete_fs_dir_update_dat_w_dat_s:
              ;xor bx, bx
               xor bl, bl
               mov ax, word ptr [DAT_Buffer_Sector]
               mov dx, word ptr [DAT_Buffer_Sector]+2
               call proc_write_dat_sector
               jc short loc_delete_fs_dir_update_dat_retn

loc_delete_fs_dir_update_mat:
               call proc_load_mat
               jc short loc_delete_fs_dir_update_dat_retn

               mov ax, word ptr [Delete_FS_FSCount_Total]
               mov dx, word ptr [Delete_FS_FSCount_Total]+2

               add ax, word ptr ES:[BX]+FS_MAT_FreeSectors
               adc dx, word ptr ES:[BX]+FS_MAT_FreeSectors+2

               mov word ptr [SI][LD_FreeSectors], ax
               mov word ptr [SI][LD_FreeSectors]+2, dx  

               mov word ptr ES:[BX]+FS_MAT_FreeSectors, ax
               mov word ptr ES:[BX]+FS_MAT_FreeSectors+2, dx

               mov ax, word ptr [RmDir_DirLastCluster]
	       mov dx, word ptr [RmDir_DirLastCluster]+2

               cmp dx, word ptr ES:[BX]+FS_MAT_FirstFreeSector+2
               ja short loc_delete_fs_dir_pass_change_mat_ffs
               jb short loc_delete_fs_dir_change_mat_ffs
               cmp ax, word ptr ES:[BX]+FS_MAT_FirstFreeSector
               jnb short loc_delete_fs_dir_pass_change_mat_ffs
              
loc_delete_fs_dir_change_mat_ffs:
	       mov word ptr ES:[BX]+FS_MAT_FirstFreeSector, ax
               mov word ptr ES:[BX]+FS_MAT_FirstFreeSector+2, dx
               
loc_delete_fs_dir_pass_change_mat_ffs:
               call proc_write_mat 
               jc short loc_delete_fs_dir_update_dat_retn

              ; 27/06/2010
               cmp byte ptr [RmDir_MultiClusters], 0
               jna short loc_delete_fs_dir_load_parent_dir

loc_delete_fs_dir_load_prev_section:
               ; 26/06/2010
                mov ax, word ptr [RmDir_PreviousCluster]
                mov dx, word ptr [RmDir_PreviousCluster]+2
 
               ;xor bl, bl
                mov byte ptr [FS_BuffValidData], bl
                mov word ptr [FS_CurrentSection], ax
                mov word ptr [FS_CurrentSection]+2, dx

                add ax, word ptr [SI][LD_FS_BeginSector]
                adc dx, word ptr [SI][LD_FS_BeginSector]+2

                push ax
                push dx  

               ;xor bh, bh
               ;;xor cx, cx  
               ;;inc cx
               ;inc cl      
                call proc_disk_read
                pop dx
                jnc short loc_delete_fs_directory_check_prev_ddt
                pop dx
loc_delete_fs_dir_prev_ddt_diskio_err_retn:
             ; 26/06/2010
                retn

loc_delete_fs_directory_check_prev_ddt:
               ; 26/06/2010
                mov byte ptr [FS_BuffValidData], 1
;                cmp word ptr ES:[BX], 'DD'
;                jne short loc_delete_fs_dir_last_ddt_invalid_data
;                cmp byte ptr ES:[BX]+2, 'T'
;                jne short loc_delete_fs_dir_last_ddt_invalid_data
                 xor ax, ax               
                 mov word ptr ES:[BX]+FS_DDT_Next_DDT_Number, ax ;0
                 mov word ptr ES:[BX]+FS_DDT_Next_DDT_Number+2, ax ;0
                 pop ax

                ; mov cx, 1
                call proc_disk_write
                jc short loc_delete_fs_dir_prev_ddt_diskio_err_retn

                mov ax, word ptr [Delete_FS_FNumber]
	        mov dx, word ptr [Delete_FS_FNumber]+2

                jmp loc_delete_fs_dir_check_last_section_next 

loc_delete_fs_dir_load_parent_dir:
              ; 26/06/2010 
                mov ax, word ptr [Delete_FS_ParentDir]
                mov dx, word ptr [Delete_FS_ParentDir]+2

                mov byte ptr [FS_BuffValidData], 0
              
             ; 20/06/2010              

                mov word ptr [FS_CurrentSection], ax
                mov word ptr [FS_CurrentSection]+2, dx

    	        push ax
                push dx    

                add ax, word ptr [SI][LD_FS_BeginSector]
                adc dx, word ptr [SI][LD_FS_BeginSector]+2

              ;;xor bx, bx
              ;xor bl, bl
              ;xor cx, cx 
              ;;inc cx
              ;inc cl      
               call proc_disk_read
               pop dx
               jnc short loc_delete_fs_dir_check_parent_dir

              ; 22/05/2010
               pop dx
              ; 26/06/2010
loc_delete_fs_dir_parent_dir_upd_retn:
               retn 

loc_delete_fs_dir_parent_dir_not_found:
               mov dx, ax
               mov ax, 03h
             ; 26/06/2010
               jmp loc_delete_fs_dir_path_not_found

loc_delete_fs_dir_check_parent_dir:
               mov byte ptr [FS_BuffValidData], 1
               pop ax
               cmp word ptr ES:[BX]+1, 'TD'
               jne short loc_delete_fs_dir_parent_dir_not_found
               cmp byte ptr ES:[BX], 'D'
               je short loc_delete_fs_dir_parent_dir_found
               cmp byte ptr ES:[BX], 'R'
               jne short loc_delete_fs_dir_parent_dir_not_found
loc_delete_fs_dir_parent_dir_found:
               cmp ax, word ptr ES:[BX]+8 ;Dir Number low
               jne short loc_delete_fs_dir_parent_dir_not_found
               cmp dx, word ptr ES:[BX]+10 ;Dir Number high
               jne short loc_delete_fs_dir_parent_dir_not_found

             ; 03/07/2010
             ; Entry offset (cx) cancelled
             ; (search from beginning by using only dir number)

  	       mov ax, word ptr [Delete_FS_FNumber]
	       mov dx, word ptr [Delete_FS_FNumber]+2

              ; Reset Sector Buffer Validation Sign              
               mov byte ptr [FS_BuffValidData], 0
              ; BECAUSE
              ; Sub procedure will change sector buffer content
              ; but it will not set buffer control parameters !   
              ;

               call proc_find_fs_dir_entry_with_number
              ; 26/06/2010
               jc short loc_delete_fs_dir_parent_dir_upd_retn

loc_delete_fs_directory_delete_fs_dir_entry:
               ; DX:AX = Parent Directory Data Sector
               ; ES:DI = FS Parent Directory Entry Offset
               ; BX:CX = Directory Number
               ; DS:SI = Logical Dos Drive Decr. Table Address

              ;cmp cx, word ptr [Delete_FS_FNumber]
              ;jne loc_delete_fs_directory_not_found
	      ;cmp bx, word ptr [Delete_FS_FNumber]+2
              ;jne loc_delete_fs_directory_not_found
            
               xor cx, cx
               dec cx ;  mov cx, 0FFFFh
               mov word ptr ES:[DI], cx
               mov word ptr ES:[DI]+2, cx
               inc cx ; xor cx, cx
               mov bx, cx ; xor bx, bx
               inc cx ; mov cx, 1
               
               add ax, word ptr [SI][LD_FS_BeginSector]
               adc dx, word ptr [SI][LD_FS_BeginSector]+2 

              ; ES:BX = Directory (Data) Buffer
             
               call proc_disk_write
              ; 26/06/2010
               jc short loc_delete_fs_dir_parent_dir_upd_retn

loc_delete_fs_directory_parent_dir_lm_d_t:
              ; 20/06/2010 
               mov ah, 02h                        ; Return Current Time
               int 1Ah
               xchg ch,cl
               mov word ptr [DelFile_fs_ddt_lm_hour], cx
               xchg dh,dl
               mov word ptr [DelFile_fs_ddt_lm_second], dx
                 
               mov ah, 04h                        ; Return Current Date
               int 1Ah
               xchg ch,cl
               mov word ptr [DelFile_fs_ddt_lm_year], cx
               xchg dh,dl
               mov word ptr [DelFile_fs_ddt_lm_month], dx

loc_delete_fs_directory_update_parent_dir_date_time:
               mov ax, word ptr [Delete_FS_ParentDir]
               mov dx, word ptr [Delete_FS_ParentDir]+2
	       add ax, word ptr [SI][LD_FS_BeginSector]
               adc dx, word ptr [SI][LD_FS_BeginSector]+2
               mov cx, 1
               call proc_disk_read
              ; 26/06/2010
               jc short loc_del_fs_dir_pass_upd_parent_dir_d_t

               mov ax, word ptr [DelFile_fs_ddt_lm_hour]
               mov dx, word ptr [DelFile_fs_ddt_lm_second]
                 
               mov word ptr ES:[BX]+fs_ddt_lm_hour, ax
               mov word ptr ES:[BX]+fs_ddt_lm_second, dx

               mov ax, word ptr [DelFile_fs_ddt_lm_year]
               mov dx, word ptr [DelFile_fs_ddt_lm_month]

               mov word ptr ES:[BX]+fs_ddt_lm_year, ax
               mov word ptr ES:[BX]+fs_ddt_lm_month, dx

               mov ax, word ptr [Delete_FS_ParentDir]
               mov dx, word ptr [Delete_FS_ParentDir]+2
               push dx
               push ax
	       add ax, word ptr [SI][LD_FS_BeginSector]
               adc dx, word ptr [SI][LD_FS_BeginSector]+2
              ;mov cx, 1               
               call proc_disk_write
               pop ax
               pop dx
              ; 20/06/2010
               mov bx, es 
               jc short loc_del_fs_dir_pass_upd_parent_dir_d_t 
                 
               mov byte ptr [FS_BuffValidData], 1
               mov word ptr [FS_CurrentSection], ax
               mov word ptr [FS_CurrentSection]+2, dx

loc_del_fs_dir_pass_upd_parent_dir_d_t:
              ; 26/06/2010
               push ds
               pop es
   	       mov ax, word ptr [Delete_FS_FNumber]
	       mov dx, word ptr [Delete_FS_FNumber]+2
               xor cx, cx
              ;DS:SI = Logical DOS Drv Table
              ;BX = Parent Dir DDT Buffer Segment (0= Load Error)
               retn

proc_delete_fs_directory endp

proc_update_dat proc near
                ; 04/07/2010
                ; 19/05/2010
                ; 18/05/2010
                ; 16/05/2010 copy/derivation from proc_update_dat in
                ;            BOOTFILE.ASM (21/02/2010)
                ;            DAT_Buffer -> SectorBuffer
                ; INPUT -> DX:AX = Data Sector (Relative to File System)
                ;          BL = (New) DAT Identifier
                ;          ES = DAT Buffer Segment
                ;          DS:SI = Logical DOS Drv Descr. Table Addr.
                ;          CX = any  
                ; OUTPUT -> clc
                ;          DX:AX = Data Sector (Relative to File System)
                ;          BL = (New) DAT Identifier
                ;          ES = DAT Buffer Segment
                ;	   DS:SI = Logical DOS Drv Descr. Table Addr. 
                ;          CX = Same with input CX
                ;          stc -> Error code in AL 
                ;  
                ; 20/02/2010 [DAT_Buffer_Offset]
                ; 13/02/2010
                ;
                ; 02/05/2009
                ; DX:AX = Disk Sector (Beginning Sector, with Descriptor)
                ; BL = Allocation Type (Identifier)

               ; 04/07/2010 'word ptr' for returning bh input 
                ; mov byte ptr [DAT_Identifier], bl
                  mov word ptr [DAT_Identifier], bx
               ;
                push cx
               ; 19/05/2010
                push dx
                push ax 

                mov cx, 512
                call Rx_DOS_Div32

               ; 16/05/2010 
                mov di, bx
               ; 
                cmp word ptr [DAT_Buffer_Sector]+2, dx
                jne short loc_write_prev_DAT_sector
                cmp word ptr [DAT_Buffer_Sector], ax
                jne short loc_write_prev_DAT_sector

loc_update_dat_buffer_x:
               ;mov bl, byte ptr [DAT_Identifier]
                mov bx, word ptr [DAT_Identifier] ; 04/07/2010
                mov byte ptr ES:[DI], bl
                mov byte ptr [DAT_Buffer_Updated], 1   
return_from_dat_update:
               ; 19/05/2010
                pop ax
                pop dx
               ;
                pop cx
                retn

loc_write_prev_DAT_sector:
                mov word ptr [DAT_Buffer_Offset], di
               ;18/05/2010 (BugFix)
                xor bx, bx ; ES:BX -> DAT Buffer Address 
               ; 
                cmp byte ptr [DAT_Buffer_Updated], 0
                jna short loc_read_DAT_sector
                push ax
                push dx 
                mov ax, word ptr [DAT_Buffer_Sector]
                mov dx, word ptr [DAT_Buffer_Sector]+2
                call proc_write_DAT_sector
                pop dx
                jnc short loc_read_DAT_sector_pop_ax_dx
                pop dx
                mov bl, byte ptr [DAT_Identifier]
                jmp short return_from_dat_update
loc_read_DAT_sector_pop_ax_dx:
                pop ax
               ; 16/05/2010 
               ; mov byte ptr [DAT_Buffer_Updated], 0 
               ;
loc_read_DAT_sector:
                ; BX = 0 
                call proc_load_dat_sector
                jc short return_from_dat_update 
		mov di, word ptr [DAT_Buffer_Offset]
                jmp short loc_update_dat_buffer_x

;DAT_Buffer_Drv: db 0
;DAT_Identifier: db 0
DAT_Identifier: dw 0 ; 04/07/2010 
DAT_Buffer_Updated: db 0
DAT_Buffer_Sector: dd 0
DAT_Buffer_Offset: dw 0

proc_update_dat endp

proc_load_dat_sector proc near
		 ; 16/05/2010 copy/derivation from proc_load_dat_sector
                 ;            in BOOTFILE.ASM (21/02/2010)
                 ;            DAT_Buffer -> SectorBuffer
                 ; INPUT -> DX:AX = DAT Sector (Relative to DAT location)
                 ;          ES:BX = DAT Buffer Address
                 ;          DS:SI = Logical DOS Drv Descr. Table Addr.
                 ;          CX = any  
                 ; OUTPUT -> clc
                 ;          DX:AX = DAT Sector (Relative to DAT location)
                 ;          ES:BX = DAT Buffer Address
                 ;	    DS:SI = Logical DOS Drv Descr. Table Addr. 
                 ;          CX = Sector count (loaded to buffer) = 1
                 ;          stc -> Error code in AL 
                 ;
                 ; 13/02/2010
                 ; Input ->  ; DX:AX = DAT Sector (Offset)
                 ; Output -> clc : load error (err code in al)
                 ;           clc -> loading successed
                 ; DX:AX = DAT Sector (Offset) Address
                 ; ES:BX = DAT Buffer Address
                 ; DAT_Buffer_Sector pointer is updated

                 push ax
                 push dx
                 add ax, word ptr [SI][LD_FS_BeginSector]
                 adc dx, word ptr [SI][LD_FS_BeginSector]+2   
                 add ax, word ptr [SI][LD_FS_DATLocation]
                 adc dx, word ptr [SI][LD_FS_DATLocation]+2
                 mov cx, 1               
                 call proc_disk_read
                 pop dx
                 jnc short loc_load_DAT_sector_clc
                 pop dx
                 retn
loc_load_DAT_sector_clc:
		 pop ax
                 mov word ptr [DAT_Buffer_Sector], ax
                 mov word ptr [DAT_Buffer_Sector]+2, dx
                 mov byte ptr [DAT_Buffer_Updated], 0
                 retn

proc_load_dat_sector endp 

proc_write_dat_sector proc near
                 ; 16/05/2010 copy/derivation from proc_write_dat_sector
                 ;            in BOOTFILE.ASM (21/02/2010)
                 ;            DAT_Buffer -> SectorBuffer
                 ; INPUT -> DX:AX = DAT Sector (Relative to DAT location)
                 ;          ES:BX = DAT Buffer Address
                 ;          DS:SI = Logical DOS Drv Descr. Table Addr.
                 ;          CX = any  
                 ; OUTPUT -> clc
                 ;          DX:AX = DAT Sector (Relative to DAT location)
                 ;          ES:BX = DAT Buffer Address
                 ;	    DS:SI = Logical DOS Drv Descr. Table Addr. 
                 ;          CX = Sector count (written to disk) = 1
                 ;          stc -> Error code in AL 
                 ;
                 ; 20/02/2010
                 ; Input ->  ; DX:AX = DAT Sector (Offset)
                 ; Output -> clc : write error (err code in al)
                 ;           clc -> write successed
                 ; DX:AX = DAT Sector (Offset) Address
                 ; BX = DAT Buffer Address

                 push ax
                 push dx
                 add ax, word ptr [SI][LD_FS_BeginSector]
                 adc dx, word ptr [SI][LD_FS_BeginSector]+2   
                 add ax, word ptr [SI][LD_FS_DATLocation]
                 adc dx, word ptr [SI][LD_FS_DATLocation]+2
                 mov cx, 1               
                 call proc_disk_write
                 pop dx
                 jnc short loc_write_DAT_sector_clc
                 pop dx
                 retn
loc_write_DAT_sector_clc:
		 pop ax
                ;mov word ptr [DAT_Buffer_Sector], ax
                ;mov word ptr [DAT_Buffer_Sector]+2, dx
                 mov byte ptr [DAT_Buffer_Updated], 0
                 retn

proc_write_dat_sector endp 

proc_load_mat proc near
		 ; 19/05/2010 
                 ; INPUT -> DS:SI = Logical DOS Drv Descr. Table Addr.
                 ;          ES:BX = MAT Buffer Address
                 ; OUTPUT -> clc
                 ;          ES:BX = MAT Buffer Address
                 ;	    DS:SI = Logical DOS Drv Descr. Table Addr. 
                 ;          stc -> Error code in AL 
                 ;      DX, CX, AX contents will be changed at retn
                 ; 
                 mov ax, word ptr [SI][LD_FS_MATLocation]
                 mov dx, word ptr [SI][LD_FS_MATLocation]+2
                 add ax, word ptr [SI][LD_FS_BeginSector]
                 adc dx, word ptr [SI][LD_FS_BeginSector]+2   
                 mov cx, 1               
                 call proc_disk_read
                 jnc short loc_fs_load_mat_check_mat_sign
               
                 retn

loc_fs_load_mat_check_mat_sign:
                 cmp word ptr ES:[BX], 'AM'
                 jne short loc_fs_load_mat_invalid_format
                 cmp byte ptr ES:[BX]+2, 'T'
                 jne short loc_fs_load_mat_invalid_format

                 retn
                 
loc_fs_load_mat_invalid_format:
                 mov ax, 0Bh
                 stc
                 retn 

proc_load_mat endp 

proc_write_mat proc near
		 ; 19/05/2010 
                 ; INPUT -> DS:SI = Logical DOS Drv Descr. Table Addr.
                 ;          ES:BX = MAT Buffer Address
                 ; OUTPUT -> clc
                 ;          ES:BX = MAT Buffer Address
                 ;	    DS:SI = Logical DOS Drv Descr. Table Addr. 
                 ;          stc -> Error code in AL 
                 ;      DX, CX, AX contents will be changed at retn
                 ; 

                 cmp word ptr ES:[BX], 'AM'
                 jne short loc_fs_write_mat_invalid_format
                 cmp byte ptr ES:[BX]+2, 'T'
                 jne short loc_fs_write_mat_invalid_format

                 mov word ptr ES:[BX]+FS_MAT_LM_OS, 00A1h 
                                                    ; 00A1h = TRDOS
                                                    ; 01A1h = Singlix
                 
                 mov word ptr ES:[BX]+FS_MAT_LM_OS_Version, 0100h 
                                                    ; 0100h= Version 1.0

loc_write_MAT_set_LM_date_time:
                 mov ah, 02h                             ; Return Current Time
                 int 1Ah
                 xchg ch,cl
                 mov word ptr ES:[BX]+fs_mat_lm_hour, cx
                 xchg dh,dl
                 mov word ptr ES:[BX]+fs_mat_lm_second, dx
                 
                 mov ah, 04h                             ; Return Current Date
                 int 1Ah
                 xchg ch,cl
                 mov word ptr ES:[BX]+fs_mat_lm_year, cx
                 xchg dh,dl
                 mov word ptr ES:[BX]+fs_mat_lm_month, dx

                 mov ax, word ptr [SI][LD_FS_MATLocation]
                 mov dx, word ptr [SI][LD_FS_MATLocation]+2
                 add ax, word ptr [SI][LD_FS_BeginSector]
                 adc dx, word ptr [SI][LD_FS_BeginSector]+2
                 mov cx, 1               
                 call proc_disk_write
                 
                 retn

loc_fs_write_mat_invalid_format:
                 mov al, 0Bh
                 xor ah, ah
                 cmc
                 retn 

proc_write_mat endp

proc_fs_get_free_sectors proc near
		 ; 19/05/2010 
                 ; INPUT -> DS:SI = Logical DOS Drv Descr. Table Addr.
                 ;          AH=0 -> Do not load MAT, use ES:BX  
                 ;          ES:BX = MAT Buffer Address (If AH = 0)
                 ;          AL=0 -> Do not check MAT sign 
                            
                 ; OUTPUT -> clc
                 ;          DX:AX = Free sectors
                 ;          ES:BX = MAT Buffer Address
                 ;	    DS:SI = Logical DOS Drv Descr. Table Addr. 
                 ;          stc -> Error code in AL 
                 ; 
                 
                 or ah, ah
                 jz short loc_fs_gfsc_pass_load_mat

loc_fs_gfsc_load_mat:
                 mov bx, word ptr [Sector_Buffer]
                 mov es, bx
                 xor bx, bx
                 mov byte ptr [FS_BuffValidData], bl
                 dec bx
                 mov word ptr [FS_CurrentSection], bx ; FFFFh
                 mov word ptr [FS_CurrentSection]+2, bx ; FFFFh
                 inc bx ; 0

                 call proc_load_mat
                 jnc short loc_fs_gfsc_pass_check_mat_sign         
                 
                 retn
                 
loc_fs_gfsc_pass_load_mat:
                 or al, al
                 jz short loc_fs_gfsc_pass_check_mat_sign

loc_fs_gfsc_check_mat_sign:
                 cmp word ptr ES:[BX], 'AM'
                 jne short loc_fs_gfsc_mat_invalid_format
                 cmp byte ptr ES:[BX]+2, 'T'
                 je short loc_fs_gfsc_pass_check_mat_sign

loc_fs_gfsc_mat_invalid_format:
                 mov ax, 0Bh
                 stc
                 retn 
                       
loc_fs_gfsc_pass_check_mat_sign:
                 mov ax, word ptr ES:[BX]+FS_MAT_FreeSectors
                 mov dx, word ptr ES:[BX]+FS_MAT_FreeSectors+2

                 retn      
 
proc_fs_get_free_sectors endp

proc_fs_set_free_sectors proc near
		 ; 19/05/2010 
                 ; INPUT -> DS:SI = Logical DOS Drv Descr. Table Addr.
                 ;          DX:AX = Free sectors 
                 ;          CH=0 -> Do not load MAT, use ES:BX  
                 ;          ES:BX = MAT Buffer Address (If CH = 0)
                 ;          CL=0 -> Do not check MAT sign 
                            
                 ; OUTPUT -> clc
                 ;          DX:AX = Free sectors
                 ;          ES:BX = MAT Buffer Address
                 ;	    DS:SI = Logical DOS Drv Descr. Table Addr. 
                 ;          stc -> Error code in AL 
                 ; 
                 ; NOTE: This procedure will set free sectors in MAT
                 ; then will write MAT onto the disk, 
                 ; then will set Free Sectors in LD_FreeSectors field
                 ; of the Logical Dos Drv Table at DS:SI.
                 ; So, it is not required to set free sectors in
                 ; the Logical Dos Drv Table before this...
                 ; 

                 or ch, ch
                 jz short loc_fs_sfsc_pass_load_mat

loc_fs_sfsc_load_mat:
                 push ax
                 push dx

                 mov bx, word ptr [Sector_Buffer]
                 mov es, bx
                 xor bx, bx
                 mov byte ptr [FS_BuffValidData], bl
                 dec bx
                 mov word ptr [FS_CurrentSection], bx ; FFFFh
                 mov word ptr [FS_CurrentSection]+2, bx ; FFFFh
                 inc bx ; 0

                 call proc_load_mat
                 pop dx
                 jnc short loc_fs_sfsc_pass_check_mat_sign_pop_ax     
loc_fs_sfsc_pop_dx_retn:
                 pop dx
                 retn
                 
loc_fs_sfsc_pass_load_mat:
                 or cl, cl
                 jz short loc_fs_sfsc_pass_check_mat_sign

loc_fs_sfsc_check_mat_sign:
                 cmp word ptr ES:[BX], 'AM'
                 jne short loc_fs_sfsc_mat_invalid_format
                 cmp byte ptr ES:[BX]+2, 'T'
                 je short loc_fs_sfsc_pass_check_mat_sign

loc_fs_sfsc_mat_invalid_format:
                 mov ax, 0Bh
                 stc
                 retn 

loc_fs_sfsc_pass_check_mat_sign_pop_ax:
                 pop ax                       
loc_fs_sfsc_pass_check_mat_sign:
                 mov word ptr ES:[BX]+FS_MAT_FreeSectors, ax
                 mov word ptr ES:[BX]+FS_MAT_FreeSectors+2, dx
                 push ax
                 push dx
                 call proc_write_mat
                 pop dx
                 jc short loc_fs_sfsc_pop_dx_retn
                 pop ax

                 mov word ptr [SI][LD_FreeSectors], ax
                 mov word ptr [SI][LD_FreeSectors]+2, dx  
                 
                 retn  
 
proc_fs_set_free_sectors endp

proc_fs_get_first_free_sector proc near
		 ; 19/05/2010 
                 ; INPUT -> DS:SI = Logical DOS Drv Descr. Table Addr.
                 ;          AH=0 -> Do not load MAT, use ES:BX  
                 ;          ES:BX = MAT Buffer Address (If AH = 0)
                 ;          AL=0 -> Do not check MAT sign 
                            
                 ; OUTPUT -> clc
                 ;          DX:AX = First Free Sector
                 ;          ES:BX = MAT Buffer Address
                 ;	    DS:SI = Logical DOS Drv Descr. Table Addr. 
                 ;          stc -> Error code in AL 
                 ; 
                 
                 or ah, ah
                 jz short loc_fs_gffs_pass_load_mat

loc_fs_gffs_load_mat:
                 mov bx, word ptr [Sector_Buffer]
                 mov es, bx
                 xor bx, bx
                 mov byte ptr [FS_BuffValidData], bl
                 dec bx
                 mov word ptr [FS_CurrentSection], bx ; FFFFh
                 mov word ptr [FS_CurrentSection]+2, bx ; FFFFh
                 inc bx ; 0

                 call proc_load_mat
                 jnc short loc_fs_gffs_pass_check_mat_sign         
                 
                 retn
                 
loc_fs_gffs_pass_load_mat:
                 or al, al
                 jz short loc_fs_gffs_pass_check_mat_sign

loc_fs_gffs_check_mat_sign:
                 cmp word ptr ES:[BX], 'AM'
                 jne short loc_fs_gffs_mat_invalid_format
                 cmp byte ptr ES:[BX]+2, 'T'
                 je short loc_fs_gffs_pass_check_mat_sign

loc_fs_gffs_mat_invalid_format:
                 mov ax, 0Bh
                 stc
                 retn 
                       
loc_fs_gffs_pass_check_mat_sign:
                 mov ax, word ptr ES:[BX]+FS_MAT_FirstFreeSector
                 mov dx, word ptr ES:[BX]+FS_MAT_FirstFreeSector+2

                 retn      

proc_fs_get_first_free_sector endp

proc_fs_set_first_free_sector proc near
		 ; 19/05/2010 
                 ; INPUT -> DS:SI = Logical DOS Drv Descr. Table Addr.
                 ;          DX:AX = First Free sector 
                 ;          CH=0 -> Do not load MAT, use ES:BX  
                 ;          ES:BX = MAT Buffer Address (If CH = 0)
                 ;          CL=0 -> Do not check MAT sign 
                            
                 ; OUTPUT -> clc
                 ;          DX:AX = First Free Sector
                 ;          ES:BX = MAT Buffer Address
                 ;	    DS:SI = Logical DOS Drv Descr. Table Addr. 
                 ;          stc -> Error code in AL 
                 ; 

                 or ch, ch
                 jz short loc_fs_sffs_pass_load_mat

loc_fs_sffs_load_mat:
                 push ax
                 push dx

                 mov bx, word ptr [Sector_Buffer]
                 mov es, bx
                 xor bx, bx
                 mov byte ptr [FS_BuffValidData], bl
                 dec bx
                 mov word ptr [FS_CurrentSection], bx ; FFFFh
                 mov word ptr [FS_CurrentSection]+2, bx ; FFFFh
                 inc bx ; 0

                 call proc_load_mat
                 pop dx
                 jnc short loc_fs_sffs_pass_check_mat_sign_pop_ax     
loc_fs_sffs_pop_dx_retn:
                 pop dx
                 retn
                 
loc_fs_sffs_pass_load_mat:
                 or cl, cl
                 jz short loc_fs_sffs_pass_check_mat_sign

loc_fs_sffs_check_mat_sign:
                 cmp word ptr ES:[BX], 'AM'
                 jne short loc_fs_sffs_mat_invalid_format
                 cmp byte ptr ES:[BX]+2, 'T'
                 je short loc_fs_sffs_pass_check_mat_sign

loc_fs_sffs_mat_invalid_format:
                 mov ax, 0Bh
                 stc
                 retn 

loc_fs_sffs_pass_check_mat_sign_pop_ax:
                 pop ax                       
loc_fs_sffs_pass_check_mat_sign:
                 mov word ptr ES:[BX]+FS_MAT_FirstFreeSector, ax
                 mov word ptr ES:[BX]+FS_MAT_FirstFreeSector+2, dx
                 push ax
                 push dx
                 call proc_write_mat
                 pop dx
                 jc short loc_fs_sffs_pop_dx_retn
                 pop ax

                 retn  
 
proc_fs_set_first_free_sector endp


proc_get_last_section proc near
                ; 04/09/2010  section count return
                ; 20/06/2010
                ; INPUT -> AX = First Section Header Address, low 16 bit
                ; INPUT -> DX = First Cluster Header Address, high 16 bit
                ; INPUT -> DS:SI = Logical Dos Drive Parameters Table
                ; OUTPUT -> clc -> No Error, DX:AX valid
                ; OUTPUT -> stc & AX>0 -> Error
                ; AX: Last Section Header Address, low 16 bit
                ; DX: Last Section Header Address, high 16 bit
                ; BX:CX = Previous section (the section before the last)
                ;  04/09/2010 
                ;  DI = Section count
                ; stc -> Error
                 
                ; 04/09/2010
                mov word ptr [gls_sectioncount], 0
  
                ; 20/06/2010

loc_gls_get_next_section_1:
                mov word ptr [gls_prevsection], ax
                mov word ptr [gls_prevsection]+2, dx

loc_gls_get_next_section_2:
                inc word ptr [gls_sectioncount]
                push dx
                push ax 
                ; Loads FS section header at/into the sector buffer
                ; and returns the next section header address
                ;  
                ; INPUT -> AX = Section (DDT/FDT) header address, low 16 bit
                ; INPUT -> DX = Section (DDT/FDT) header address, high 16 bit
                ; INPUT -> DS:SI = Logical Dos Drive Parameters Table
                ; OUTPUT -> clc -> No Error, DX:AX valid
                ; OUTPUT -> stc & AX=0 -> The last section, no next one
                ; OUTPUT -> stc & AX>0 -> Error
                ; AX: Next Section Address, low 16 bit
                ; DX: Next Section Address, high 16 bit
                ; 16/01/2010
                ; CX = Sector count, low word
                ; BX = Sector count, high word
                ; stc -> Error (invalid values in cx, bx, dx)
                call proc_get_next_section
                jnc short loc_gls_get_next_section_3
                
                or ax, ax
                jnz short loc_gls_stc_retn

                ; cx:bx = previous section header address
                pop ax
                pop dx

loc_gls_prev_cluster_retn:
                mov cx, word ptr [gls_prevsection]
                mov bx, word ptr [gls_prevsection]+2
               
               ;04/09/2010 
                mov di, word ptr [gls_sectioncount]

                retn

loc_gls_get_next_section_3:
                pop word ptr [gls_prevsection]
                pop word ptr [gls_prevsection]+2
                jmp short loc_gls_get_next_section_2

loc_gls_stc_retn:
                dec word ptr [gls_sectioncount]
                pop cx
                pop bx
                cmc ;stc
                jmp short loc_gls_prev_cluster_retn

gls_prevsection: dd 0
; 04/09/2010  
gls_sectioncount: dw 0 

proc_get_last_section endp

proc_make_fs_directory proc near
               ; 02/04/2011
               ; 25/12/2010 BugFix
               ; 20/11/2010
               ; 07/11/2010
               ; 06/11/2010
               ; 31/07/2010
               ; 29/07/2010
               ; 28/07/2010
               ; 27/07/2010
               ; 25/07/2010
               ; 24/07/2010

               ; INPUT ->
               ; DS:SI -> Logical DOS Drive DT
               ; DX:AX -> Parent directory DDT Address
               ; BX -> Directory name offset as ASCIIZ name

               ; OUTPUT -> STC = Sub directory could not be loaded
               ; Error number is in AX (AL)
               ; BX = 0 (BX > 0 -> sectors are loaded but not all)
               
               ; DS:SI = DOS Drive Description Table
               ; CX = Directory buffer size in sectors
               ; NOTE: DirBuffer_Size is in bytes !
               ; BX = Directory Buffer Segment
               ; DX:AX = DDT Address 
               ; 

               cmp byte ptr [SI][LD_FS_FSType], 0A1h 
               je short loc_FS_mkdir_start_to_load_parent_directory

	       mov ax, 1Ah ; Unknown media type (non-DOS disk)
               xor dx, dx
               xor cx, cx 
               stc
loc_FS_mkdir_load_parent_dir_section_retn: 
               retn

loc_FS_mkdir_start_to_load_parent_directory:
               mov word ptr [FS_ParentDir_DDT], ax
	       mov word ptr [FS_ParentDir_DDT]+2, dx
               mov word ptr [FS_SubDir_NameOffset], bx

loc_FS_mkdir_load_parent_directory_section_0:
              ;call proc_get_count_of_FS_dir_entries
              ;jc short loc_FS_mkdir_load_parent_dir_section_retn

              ;mov word ptr [FS_DirEntryCount], ax
	      ;mov word ptr [FS_DirEntryCount]+2, dx
              
               mov cx, word ptr [FS_DirEntryCount]
               mov bx, word ptr [FS_DirEntryCount]+2  ;06/11/2010
               or bx, bx
  	       jnz short loc_FS_mkdir_load_parent_dir_permission_denied
  
              ; 24/07/2010
              ; TRDOS can not use FS directory entries beyond 2048
               cmp cx, 2048
               jna short loc_FS_mkdir_load_parent_dir_section_1

loc_FS_mkdir_load_parent_dir_permission_denied:
               mov ax, 05h ; Access denied
               mov dx, bx ; 06/11/2010
               mov bx, word ptr [FS_SubDir_NameOffset]
               ; DX:CX -> FS Dir Entry count
               stc
               retn 

loc_FS_mkdir_check_for_free_entry_loop_2:
               add si, 4
               jmp short loc_FS_mkdir_check_for_free_entry_next_1

loc_FS_mkdir_check_for_free_entry_next_2:
               cmp dx, 0FFFFh
               jne short loc_FS_mkdir_check_for_free_entry_loop_1
               cmp ax, 0FFFFh
               je short loc_FS_mkdir_save_new_dir_entry

loc_FS_mkdir_check_for_free_entry_loop_1:  
               loop loc_FS_mkdir_check_for_free_entry_loop_2

loc_FS_mkdir_check_next_parent_directory_section:
               pop ds
               pop si
               mov ax, word ptr [FS_DDT_Next_Section]
	       mov dx, word ptr [FS_DDT_Next_Section]+2

               and ax, ax
               jnz short loc_FS_mkdir_load_parent_dir_section_1_next
               and dx, dx
               jnz short loc_FS_mkdir_load_parent_dir_section_1_next

               mov cx, word ptr [FS_DirEntryCount]
               cmp cx, 2048
               jnb short loc_FS_mkdir_load_parent_dir_permission_denied   

               mov ax, word ptr [FS_DirBuffer_Section]
	       mov dx, word ptr [FS_DirBuffer_Section]+2

               xor cx, cx ; new directory directory section -> cx = 0

               call proc_add_new_fs_section
               jc loc_FS_mkdir_load_sub_directory_return

loc_FS_mkdir_load_parent_dir_section_1:
               mov bx, word ptr [FS_Directory_Buffer]  
loc_FS_mkdir_load_parent_dir_section_1_next:
               mov word ptr [FS_DirBuffer_Section], ax
	       mov word ptr [FS_DirBuffer_Section]+2, dx
               call proc_allocate_fs_section_buffer                
               jc loc_FS_mkdir_load_sub_directory_return

loc_FS_mkdir_load_parent_dir_section_2:
               mov word ptr [FS_Directory_Buffer], bx
               mov word ptr [FS_DirBuff_SectorCount], cx
               mov word ptr [FS_DDT_Next_Section], ax
	       mov word ptr [FS_DDT_Next_Section]+2, dx

loc_FS_mkdir_load_parent_dir_section_3:
               push si
               push ds
               xor si, si
               mov ds, bx
               mov ax, 128
               mul cx
               mov cx, ax ; count of FS dir entries in section 

loc_FS_mkdir_check_for_free_entry_next_1:
               mov ax, word ptr [SI]
               mov dx, word ptr [SI]+2
               or ax, ax
               jnz short loc_FS_mkdir_check_for_free_entry_next_2
               or dx, dx
               jnz short loc_FS_mkdir_check_for_free_entry_next_2

loc_FS_mkdir_save_new_dir_entry:
               pop ds  
               mov word ptr [FS_Mkdir_EntryOffset], si
               pop si

loc_FS_mkdir_get_first_free_section_0:
               mov cx, word ptr [SI][LD_FreeSectors]+2
               or cx, cx
               jnz short loc_FS_mkdir_get_first_free_section_1
               mov cx, word ptr [SI][LD_FreeSectors]                                 
               cmp cx, 2
               jb short loc_FS_mkdir_insufficient_disk_space
               mov cx, 2
               jmp short loc_FS_mkdir_get_first_free_section_4
loc_FS_mkdir_get_first_free_section_1:
               and ch, ch
               jz short loc_FS_mkdir_get_first_free_section_2
               mov cx, 16 ; for maximum 2048 entry (16*128)
               jmp short loc_FS_mkdir_get_first_free_section_3
loc_FS_mkdir_get_first_free_section_2:
               shr cl, 1
	       shr cl, 1
               shr cl, 1
               shr cl, 1
               or cl, cl
               jnz short loc_FS_mkdir_get_first_free_section_3
               inc cl
loc_FS_mkdir_get_first_free_section_3:
               inc cl ; add 1 for section header
loc_FS_mkdir_get_first_free_section_4:
               call proc_get_first_free_section
               jnc short pass_FS_mkdir_get_first_free_section
               cmp cx, 2
               jnb short pass_FS_mkdir_get_first_free_section

              ; 04/02/2011 
             
loc_FS_mkdir_insufficient_disk_space:
                mov ax, 27h ; MSDOS err => insufficient disk space
                mov dx, word ptr [SI][LD_FreeSectors]
                mov bx, word ptr [FS_SubDir_NameOffset]
                ; err retn: DX = Free sectors (LW)
                ; BX= Dir name offset
                ; DS:SI -> Dos drive description table address
                ; cx = Free consequent sectors
                ;
                stc 
                retn

pass_FS_mkdir_get_first_free_section:
               mov word ptr [FS_Mkdir_New_DDT], ax
               mov word ptr [FS_Mkdir_New_DDT]+2, dx            
               mov word ptr [FS_Mkdir_New_DDT_scount], cx 

loc_FS_mkdir_write_new_dir_entry:
               push si
               push ds

               mov si, word ptr [FS_Directory_Buffer]
               push si
               mov si, word ptr [FS_Mkdir_EntryOffset]
               pop ds ; FS_Directory_Buffer
               mov word ptr [SI], ax
               mov word ptr [SI]+2, dx
               pop ds
               pop si 

loc_FS_mkdir_initialize_new_section:
               mov bx, word ptr [Sector_Buffer] 
               mov es, bx
               xor bx, bx
               mov di, bx
               mov cx, 256
               xor ax, ax ; 0  
               rep stosw  ; mov word ptr ES:[DI], 0  

               mov ax, word ptr [FS_Mkdir_New_DDT]
              ;mov dx, word ptr [FS_Mkdir_New_DDT]+2            

               mov byte ptr [FS_BuffValidData], bl
               mov word ptr [FS_CurrentSection], ax
               mov word ptr [FS_CurrentSection]+2, dx

               mov cx, word ptr [FS_Mkdir_New_DDT_scount]
                            
               add ax, word ptr [SI][LD_FS_BeginSector]
               adc dx, word ptr [SI][LD_FS_BeginSector]+2

loc_FS_mkdir_initialize_new_section_write:
              ; 25/12/2010 
               push cx
               mov cx, 1     
               call proc_disk_write
               pop cx
               jc loc_FS_mkdir_load_sub_directory_return
               ; DX:AX = Next disk adress (+1)
               loop loc_FS_mkdir_initialize_new_section_write

               mov byte ptr [FS_BuffValidData], 1

               dec word ptr [FS_Mkdir_New_DDT_scount] ; except header 
                
loc_FS_mkdir_write_parent_dir_updated_sector:
               mov bx, word ptr [FS_Directory_Buffer]
               mov es, bx

               mov ax, word ptr [FS_Mkdir_EntryOffset]
               mov bx, ax
               xor dx, dx
               mov cx, 512 
               div cx
               sub bx, dx ; DX= remain/offset
               xor dx, dx               

               ; BX = Current FS Dir buffer offset (ES:BX)
               ; AX = Current FS dir buffer sector
               ; DX = 0

               add ax, word ptr [FS_DirBuffer_Section]
	       adc dx, word ptr [FS_DirBuffer_Section]+2
               add ax, word ptr [SI][LD_FS_BeginSector]
               adc dx, word ptr [SI][LD_FS_BeginSector]+2

               mov cx, 1 
               add ax, cx ;1
               adc dx, 0

               call proc_disk_write
               jc loc_FS_mkdir_load_sub_directory_return

loc_FS_mkdir_save_new_dir_section:
               mov bx, word ptr [Sector_Buffer]
               mov es, bx

               mov ax, word ptr [FS_ParentDir_DDT]
               mov dx, word ptr [FS_ParentDir_DDT]+2

               xor bx, bx
               mov byte ptr [FS_BuffValidData], bl
               mov word ptr [FS_CurrentSection], ax
               mov word ptr [FS_CurrentSection]+2, dx

               add ax, word ptr [SI][LD_FS_BeginSector]
               adc dx, word ptr [SI][LD_FS_BeginSector]+2

               xor cx, cx 
              ;inc cx
               inc cl      
               call proc_disk_read
               jc loc_FS_mkdir_load_sub_directory_return

loc_FS_mkdir_check_parent_ddt_1:
                 mov byte ptr [FS_BuffValidData], 1

                 mov cx, word ptr ES:[BX]+1
                 cmp cx, 'TD'
                 je short loc_FS_mkdir_check_parent_ddt_2

loc_FS_mkdir_invalid_parent_ddt:
                 xor dx, dx
                 mov al, 0Dh ; Invalid data !
                 xor ah, ah
                 stc
                 jmp loc_FS_mkdir_load_sub_directory_return

loc_FS_mkdir_check_parent_ddt_2:
                 mov cl, byte ptr ES:[BX]
                 cmp cl, 'D'
                 je short loc_FS_mkdir_get_parent_dir_serial
                 cmp cl, 'R'
                 jne short loc_FS_mkdir_invalid_parent_ddt 
loc_FS_mkdir_get_parent_dir_serial:
               ; 25/07/2010 
                 mov ax, word ptr ES:[BX]+FS_DDT_Directory_Number
                 mov dx, word ptr ES:[BX]+FS_DDT_Directory_Number+2

                 cmp ax, word ptr [FS_ParentDir_DDT]                  
                 jne short loc_FS_mkdir_invalid_parent_ddt
                 cmp dx, word ptr [FS_ParentDir_DDT]+2                  
                 jne short loc_FS_mkdir_invalid_parent_ddt

                ; 07/11/2010 BugFix
                 mov ax, word ptr ES:[BX]+FS_DDT_Directory_Serial
                 mov dx, word ptr ES:[BX]+FS_DDT_Directory_Serial+2
                ;
                 mov word ptr [FS_ParentDir_Serial], ax
                 mov word ptr [FS_ParentDir_Serial]+2, dx

                 mov al, byte ptr ES:[BX]+FS_DDT_Reserved1 ; Sub dir level
                 inc al
                 mov byte ptr [FS_Mkdir_SubDirlevel], al

loc_FS_mkdir_set_parent_dir_lm_date_time:
                 mov ah, 02h                        ; Return Current Time
                 int 1Ah
                 xchg ch,cl
                 mov word ptr [DelFile_fs_ddt_lm_hour], cx
                 xchg dh,dl
                 mov word ptr [DelFile_fs_ddt_lm_second], dx

                 mov word ptr ES:[BX]+fs_ddt_lm_hour, cx
                 mov word ptr ES:[BX]+fs_ddt_lm_second, dx
                 
                 mov ah, 04h                        ; Return Current Date
                 int 1Ah
                 xchg ch,cl
                 mov word ptr [DelFile_fs_ddt_lm_year], cx
                 xchg dh,dl
                 mov word ptr [DelFile_fs_ddt_lm_month], dx
 
                 mov word ptr ES:[BX]+fs_ddt_lm_year, cx
                 mov word ptr ES:[BX]+fs_ddt_lm_month, dx

                 mov ax, word ptr [FS_ParentDir_DDT]
                 mov dx, word ptr [FS_ParentDir_DDT]+2

                ;xor bl, bl
                 mov byte ptr [FS_BuffValidData], bl
                 mov word ptr [FS_CurrentSection], ax
                 mov word ptr [FS_CurrentSection]+2, dx

                 add ax, word ptr [SI][LD_FS_BeginSector]
                 adc dx, word ptr [SI][LD_FS_BeginSector]+2

                 xor cx, cx 
                 ;inc cx
                 inc cl      
                 call proc_disk_write
                 jc loc_FS_mkdir_load_sub_directory_return

                ; mov byte ptr [FS_BuffValidData], 1

loc_FS_mkdir_set_new_sub_dir_ddt:
             ; 25/07/2010
               ;mov bx, word ptr [Sector_Buffer]
               ;mov es, bx

               mov ax, word ptr [FS_Mkdir_New_DDT]
               mov dx, word ptr [FS_Mkdir_New_DDT]+2

              ;xor bx, bx
              ;mov byte ptr [FS_BuffValidData], bl
               mov word ptr [FS_CurrentSection], ax
               mov word ptr [FS_CurrentSection]+2, dx

               add ax, word ptr [SI][LD_FS_BeginSector]
               adc dx, word ptr [SI][LD_FS_BeginSector]+2

              ;mov cx, 1 
               call proc_disk_read
               jc loc_FS_mkdir_load_sub_directory_return

               mov byte ptr [FS_BuffValidData], 1

              ; 24/07/2010
               push si
               mov si, word ptr [FS_SubDir_NameOffset]
               mov di, FS_DDT_Directory_Name ; 64
               mov cx, 12 ;Max. length of dos dirname, it is 64 for FS
loc_fs_mkdir_move_dir_name:
               lodsb ;  mov al, byte ptr [SI]
               or al, al
               jz short pass_fs_mkdir_move_dir_name
               stosb
               loop loc_fs_mkdir_move_dir_name
              ;xor al, al

pass_fs_mkdir_move_dir_name:
              ;mov byte ptr ES:[DI], al
               pop si

               xor di, di
               mov byte ptr ES:[DI], 'D'
               inc di 
               mov word ptr ES:[DI], 'TD'
              ;mov di, 3
              ; mov byte ptr ES:[DI], 0

               mov di, FS_DDT_Sector_Size ; 4
               mov word ptr ES:[DI], 512

               mov ax, word ptr [FS_Mkdir_New_DDT]
               mov dx, word ptr [FS_Mkdir_New_DDT]+2

               mov di, FS_DDT_Directory_Number ; 8

               mov word ptr ES:[DI], ax
               mov word ptr ES:[DI]+2, dx   
               
               mov cx, word ptr [FS_Mkdir_New_DDT_scount] ; except header

               mov di, FS_DDT_Sector_Count ; 16
               mov word ptr ES:[DI], cx
              ;mov word ptr ES:[DI]+2, 0

               mov ax, word ptr [FS_ParentDir_DDT]
               mov dx, word ptr [FS_ParentDir_DDT]+2 
               mov di, FS_DDT_Parent_Dir_Number ; 20
               mov word ptr ES:[DI], ax
               mov word ptr ES:[DI]+2, dx

               mov ax, word ptr [FS_ParentDir_Serial]
               mov dx, word ptr [FS_ParentDir_Serial]+2 
               mov di, FS_DDT_Parent_Dir_Serial ; 24
               mov word ptr ES:[DI], ax
               mov word ptr ES:[DI]+2, dx

               xor ax, ax
               int 1Ah                 ; get time of day (tick count)
               mov di, FS_DDT_Directory_Serial ; 28
               mov word ptr ES:[DI], dx
               mov word ptr ES:[DI]+2, cx    ; set unique serial no        

               mov di, FS_DDT_Entry_Size ; 32
              ; mov di, FS_DDT_Reserved1 ; 33, Sub directory level
               mov al, 4
               mov ah, byte ptr [FS_Mkdir_SubDirlevel]
               mov word ptr ES:[DI], ax

               mov di, FS_DDT_Attributes ; 37
               mov byte ptr ES:[DI], 01101101b ; normal/open directory

               mov di, FS_DDT_Country ; 46
               mov byte ptr ES:[DI], 90 ; default configuration (trdos & singlix)
                                        ; (singlix fs prototype: english-turkish)

               mov ax, word ptr [DelFile_fs_ddt_lm_year]
               mov dx, word ptr [DelFile_fs_ddt_lm_month]
               mov di, fs_ddt_cr_year ; 48
               mov word ptr ES:[DI], ax
               mov di, fs_ddt_cr_month ; 50 (Day = offset 51)
               mov word ptr ES:[DI], dx

               mov ax, word ptr [DelFile_fs_ddt_lm_hour]
               mov dx, word ptr [DelFile_fs_ddt_lm_second]
               mov di, fs_ddt_cr_hour ; 52 (Minute = offset 53)
               mov word ptr ES:[DI], ax
               mov di, fs_ddt_cr_second ;54
               mov word ptr ES:[DI], dx

               mov ah, 02h                        ; Return Current Time
               int 1Ah
               xchg ch,cl
               xchg dh,dl
               mov di, fs_ddt_lm_hour ; 60 (Minute = offset 61)
               mov word ptr ES:[DI], cx
               mov di, fs_ddt_lm_second ;62
               mov word ptr ES:[DI], dx
                 
               mov ah, 04h                        ; Return Current Date
               int 1Ah
               xchg ch,cl
               xchg dh,dl
               mov di, fs_ddt_lm_year ; 56
               mov word ptr ES:[DI], cx
               mov di, fs_ddt_lm_month ; 58 (Day = offset 59)
               mov word ptr ES:[DI], dx

              ;xor bx, bx
               mov byte ptr [FS_BuffValidData], bl
               mov ax, word ptr [FS_CurrentSection]
               mov dx, word ptr [FS_CurrentSection]+2

               add ax, word ptr [SI][LD_FS_BeginSector]
               adc dx, word ptr [SI][LD_FS_BeginSector]+2

               ; 20/11/2010
               mov byte ptr [DirBuff_ValidData], bl ; 0 -> RESET 
               mov cx, 1
               call proc_disk_write
               jc short loc_FS_mkdir_load_sub_directory_return
               
              ;mov byte ptr [FS_BuffValidData], 1

              ; 27/07/2010
               mov cx,  word ptr [FS_Mkdir_New_DDT_scount] ; except header
              ;mov bx, word ptr [Sector_Buffer]
              ;mov es, bx
               mov bx, 0D00Dh  ;Directory header (0Dh) and directory data (D0h)
              ;mov ax, word ptr [FS_CurrentSection]
              ;mov dx, word ptr [FS_CurrentSection]+2
               mov ax, word ptr [FS_Mkdir_New_DDT]
               mov dx, word ptr [FS_Mkdir_New_DDT]+2 

               call proc_allocate_fs_disk_sectors
               jc short loc_FS_mkdir_load_sub_directory_return

loc_FS_mkdir_set_fs_buffer_validdata_for_MAT:
               push word ptr [SI][LD_FS_MATLocation]+2
               push word ptr [SI][LD_FS_MATLocation]

               pop word ptr [FS_CurrentSection]
               pop word ptr [FS_CurrentSection]+2

               inc byte ptr [FS_BuffValidData] ; 1

               push ds
               pop es
 
              ; retn 
    
loc_FS_mkdir_load_sub_directory_return:
              ;25/04/2010 -> to get more free memory at return
               pushf
               push bx
               mov bx, word ptr [FS_Directory_Buffer]
               or bx, bx
               jz short loc_FS_mkdir_load_sub_dir_return_popbx_popf
               push cx
               push dx
               push ax
               mov ax, 0106h ;Consequently for 06h allocation
                       ; 06h -> FS directory/section buffer
               call proc_deallocate_memory 
               mov word ptr [FS_Directory_Buffer], 0
               pop ax
               pop dx
               pop cx  
loc_FS_mkdir_load_sub_dir_return_popbx_popf:
               pop bx
               popf

               retn 

FS_Mkdir_SubDirlevel: db 0  
FS_Mkdir_EntryOffset: dw 0
FS_Mkdir_New_DDT: dd 0
FS_Mkdir_New_DDT_scount: dw 0
FS_SubDir_NameOffset: dw 0
FS_ParentDir_DDT: dd 0
FS_ParentDir_Serial: dd 0

proc_make_fs_directory endp

proc_get_first_free_section proc near
               ; 16/07/2011
               ; 02/04/2011
               ; 15/11/2010 -> CSCounter_Max reset
               ; 25/07/2010 
               ; Derivation form BOOTFILE.ASM, 21/02/2010
               ; 21/02/2010 (20/02/2010)
               ; BOOTFILE.ASM -> singlix fs startup file
               ;                 installation (dos) program 
               ; INPUT -> 
               ;    DS:SI = Logical DOS drive description table
               ;    CX = Sector Count (CX = 0 -> 65536)
               ;    ES = Sector Buffer Segment
               ;    BX = 0 (if BX>0, ES:BX will be initialized) 
               ; OUTPUT -> DX:AX = First Free Section
               ;           DS:SI = Logical DOS drive description table
               ;           ES:BX = DAT (or MAT sector) Buffer
               ;    CX = Consequent free sector count
               ;  stc -> cx = 0 -> error code in al, 
               ;                   al = 0 -> no free space
               ;  stc -> cx > 0 max. available consequent free sectors
               ;         dx:ax = First free section for cx and stc         
               ;  DI will be modified

                or bx, bx
                jz short loc_get_first_free_section
                
                mov bx, word ptr [Sector_Buffer]
                mov es, bx
                xor bx, bx
 
loc_get_first_free_section:
              ; 16/07/2011 SectorCount-1 modifications for 64K fixing
                dec cx  
                mov word ptr [CSectorCount], cx ; SectorCount - 1
                mov word ptr [CSCounter], bx ;0 
                mov word ptr [CSCounter_Max], bx ;0  ; 15/11/2010

                mov byte ptr [FS_BuffValidData], bl ; 0
                dec bx
                mov word ptr [FS_CurrentSection], bx ; FFFFh
                mov word ptr [FS_CurrentSection]+2, bx ; FFFFh
                inc bx
                
                call proc_load_mat
                jnc short loc_gff_section_get_MAT_parameters

loc_gff_section_stc_retn:
                xor cx, cx
                stc
                retn
                 
loc_gff_section_get_MAT_parameters:
                mov ax, word ptr ES:[BX]+FS_MAT_SectorCount
                mov dx, word ptr ES:[BX]+FS_MAT_SectorCount+2
                 
                sub ax, 1
                sbb dx, 0

                mov word ptr [DAT_LastSector], ax
                mov word ptr [DAT_LastSector]+2, dx

                mov ax, word ptr ES:[BX]+FS_MAT_FirstFreeSector
                mov dx, word ptr ES:[BX]+FS_MAT_FirstFreeSector+2

               ; 02/04/2011 BugFix (Invalid FirstFreeSector = 0FFFFFFFFh) 
                mov cx, 0FFFFh
                cmp dx, cx
                jb short loc_gff_section_valid_ffs
                cmp ax, cx
                jb short loc_gff_section_valid_ffs
                inc ax
                inc dx 
loc_gff_section_valid_ffs:
                mov word ptr [Current_FS_Sector], ax
                mov word ptr [Current_FS_Sector]+2, dx

                mov word ptr [CFDT_Address], ax
                mov word ptr [CFDT_Address]+2, dx

loc_gff_section_load_dat_sector:
                mov cx, 512
                call Rx_DOS_Div32
                mov di, bx
                xor bx, bx

loc_gff_section_load_dat_sector_check_limit:
                cmp dx, word ptr [DAT_LastSector]+2
                ja short loc_gff_section_end_of_DAT_sectors
                jb short pass_gff_section_load_dat_sector_check_limit
                cmp ax, word ptr [DAT_LastSector]
                ja short loc_gff_section_end_of_DAT_sectors

pass_gff_section_load_dat_sector_check_limit:
                call proc_load_dat_sector
                jc short loc_gff_section_stc_retn

loc_gff_section_check_dat_cell_value:
                cmp byte ptr ES:[DI], 90h
                je short loc_gff_section_inc_cscounter
                mov cx, word ptr [CSCounter]
                cmp cx, word ptr [CSCounter_MAX]
                jna short pass_gff_section_update_cscounter_max
                mov word ptr [CSCounter_MAX], cx
                mov ax, word ptr [CFDT_Address]
                mov dx, word ptr [CFDT_Address]+2
                mov word ptr [CFDT_Address_MAX], ax
                mov word ptr [CFDT_Address_MAX]+2, dx 
pass_gff_section_update_cscounter_max:
                mov word ptr [CSCounter], 0
                mov ax, word ptr [Current_FS_Sector]
                mov dx, word ptr [Current_FS_Sector]+2
                add ax, 1
                adc dx, 0
                mov word ptr [CFDT_Address], ax
                mov word ptr [CFDT_Address]+2, dx
loc_gff_section_check_cfs:
                mov word ptr [Current_FS_Sector], ax
                mov word ptr [Current_FS_Sector]+2, dx
                inc di
                cmp di, 512
                jb short loc_gff_section_check_dat_cell_value
                jmp short loc_gff_section_load_dat_sector

loc_gff_section_inc_cscounter:
               ; 16/07/2011
                inc word ptr [CSCounter]
                mov cx, word ptr [CSCounter]
                jz short loc_gff_section_return_ffs
                cmp cx, word ptr [CSectorCount]
                ja short loc_gff_section_return_ffs

                mov ax, word ptr [Current_FS_Sector]
                mov dx, word ptr [Current_FS_Sector]+2
                add ax, 1
                adc dx, 0
                jmp short loc_gff_section_check_cfs

loc_gff_section_end_of_DAT_sectors:
                mov cx, word ptr [CSCounter]
               ;cmp cx, word ptr [CSectorCount]
               ;ja short loc_gff_section_return_ffs
                cmp word ptr [CSCounter_MAX], cx
                jb short loc_gff_section_end_of_DAT_sectors_stc
                mov cx, word ptr [CSCounter_MAX]
                or cx, cx
                jnz short pass_gff_section_reset_CFDT_address
                mov ax, cx
                mov dx, ax
                stc
                retn

pass_gff_section_reset_CFDT_address: 
 		mov ax, word ptr [CFDT_Address_MAX]
                mov dx, word ptr [CFDT_Address_MAX]+2
                stc
                retn

loc_gff_section_return_ffs:
loc_gff_section_end_of_DAT_sectors_stc: 
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

proc_add_new_fs_section proc near
               ; 21/07/2011
               ; 05/12/2010 BugFix
               ; 15/11/2010
               ; 26/07/2010
               ; 25/07/2010
               ; INPUT ->
               ; DS:SI = Logical dos drv desc. table address
               ; DX:AX = Last section
               ;  CX = 0 for directory, 
               ;  CX = sector count (except FDT)
               ; OUTPUT ->
               ; DS:SI = Logical dos drv desc. table address
               ; DX:AX = New Last section (next section)
               ; stc -> error code in AL (DX = free sectors, lw)
               ; ES:BX = sector buffer
               ; CX = count of sectors (except header)
               ;  stc -> AL = 27h : insufficient disk space
               ;         CX = max. count of free consequent sectors 

loc_add_new_section:               
               mov word ptr [FS_ans_last_section], ax
               mov word ptr [FS_ans_last_section]+2, dx

               mov bx, word ptr [Sector_Buffer]
               mov es, bx
               xor bx, bx

               or cx, cx
               jnz short loc_add_new_section_set_sector_count

loc_add_new_section_get_dir_sector_count:
               mov cx, word ptr [SI][LD_FreeSectors]+2
               or cx, cx
               jnz short loc_add_new_section_get_first_free_section_1
               mov cx, word ptr [SI][LD_FreeSectors]                                 
               cmp cx, 2
               jb short loc_add_new_section_insufficient_disk_space
              ;je short loc_add_new_section_get_first_free_section_3
               mov cx, 2
               jmp short loc_add_new_section_get_first_free_section_3
loc_add_new_section_get_first_free_section_1:
               and ch, ch
               jz short loc_add_new_section_get_first_free_section_2
               mov cx, 9 ; for maximum 1024 entry (8*128)
               jmp short loc_add_new_section_get_first_free_section_3
loc_add_new_section_get_first_free_section_2:
               mov cx, 4   
               jmp short loc_add_new_section_get_first_free_section_3

loc_add_new_section_set_sector_count:
               mov word ptr [FS_ans_sector_count], cx
              ; 15/11/2010
               inc cx ; + header
              ; or cx, cx
              ; jz short loc_add_new_fs_section_invalid_data  
loc_add_new_section_get_first_free_section_3:
               call proc_get_first_free_section
               jnc short pass_add_new_section_get_first_free_section
               cmp cx, 1
               ja short pass_add_new_section_get_first_free_section
               je short loc_add_new_section_insufficient_disk_space

               or ax, ax
               jz short loc_add_new_section_insufficient_disk_space

               stc
               retn 

loc_add_new_section_insufficient_disk_space:
               mov ax, 27h ; MSDOS err => insufficient disk space
               mov dx, word ptr [SI][LD_FreeSectors]
               retn

pass_add_new_section_get_first_free_section:
               mov word ptr [FS_ans_new_section], ax
               mov word ptr [FS_ans_new_section]+2, dx
               dec cx ; except the header             
               mov word ptr [FS_ans_sector_count], cx 

               mov bx, word ptr [Sector_Buffer]
               mov es, bx
               xor bx, bx

               mov ax, word ptr [FS_ans_last_section]
               mov dx, word ptr [FS_ans_last_section]+2

loc_ans_check_sector_buffer_section_0:
               cmp byte ptr [FS_BuffValidData], 1
               jb short loc_ans_set_sector_buffer_section
loc_ans_check_sector_buffer_section_1:
               cmp ax, word ptr [FS_CurrentSection]
               jne short loc_ans_check_sector_buffer_section_2
               cmp dx, word ptr [FS_CurrentSection]+2
               je short pass_ans_set_sector_buffer_section
loc_ans_check_sector_buffer_section_2: 
               mov byte ptr [FS_BuffValidData], 0
loc_ans_set_sector_buffer_section:
               mov word ptr [FS_CurrentSection], ax
               mov word ptr [FS_CurrentSection]+2, dx

loc_ans_read_sector_buffer_section:
               add ax, word ptr [SI][LD_FS_BeginSector]
               adc dx, word ptr [SI][LD_FS_BeginSector]+2

               mov cx, 1      
               call proc_disk_read
               jc short loc_add_new_fs_section_retn
               
               mov byte ptr [FS_BuffValidData], 1

pass_ans_set_sector_buffer_section:
               mov ax, word ptr ES:[BX]+1
               cmp ah, 'T'
               jne short loc_add_new_fs_section_invalid_data0
               cmp al, 'D'
               jne short loc_add_new_fs_section_invalid_data0
               mov ah, byte ptr ES:[BX]
               cmp ah, 'F'
               jne short loc_ans_section_check_section_header_ddt
               mov word ptr [FS_ans_DT_type], 0F00Fh ; file
               jmp short loc_ans_section_check_section_header_addr2

loc_add_new_fs_section_invalid_data2:
               xchg ah, al
loc_add_new_fs_section_invalid_data0:
               mov dx, ax  
loc_add_new_fs_section_invalid_data1:
               mov ax, 0Dh ; Invalid data !
               stc
loc_add_new_fs_section_retn:
               retn

loc_ans_section_check_section_header_ddt:
               cmp ah, 'D'
               je short loc_ans_section_check_section_header_addr1
               cmp ah, 'R'
               jne short loc_add_new_fs_section_invalid_data2
               mov word ptr [FS_ans_DT_type], 0504h ; root directory
               jmp short loc_ans_section_check_section_header_addr2  
loc_ans_section_check_section_header_addr1:
               mov word ptr [FS_ans_DT_type], 0D00Dh ; sub directory 
loc_ans_section_check_section_header_addr2:
               xor dx, dx
               mov ax, word ptr ES:[BX]+FS_DDT_Directory_Number
               cmp ax, word ptr [FS_CurrentSection]
               jne short loc_add_new_fs_section_invalid_data1
               mov ax, word ptr ES:[BX]+FS_DDT_Directory_Number+2
               cmp ax, word ptr [FS_CurrentSection]+2
               jne short loc_add_new_fs_section_invalid_data1

               mov ax, word ptr ES:[BX]+FS_DDT_Next_DDT_Number
               or ax, word ptr ES:[BX]+FS_DDT_Next_DDT_Number+2
               jnz short loc_add_new_fs_section_invalid_data1

loc_ans_update_section_header:
               mov byte ptr [FS_BuffValidData], al ; 0
               mov ax, word ptr [FS_ans_new_section]
               mov dx, word ptr [FS_ans_new_section]+2
               mov word ptr ES:[BX]+FS_DDT_Next_DDT_Number, ax
               mov word ptr ES:[BX]+FS_DDT_Next_DDT_Number+2, dx

               mov ax, word ptr [FS_CurrentSection]
               mov dx, word ptr [FS_CurrentSection]+2

               add ax, word ptr [SI][LD_FS_BeginSector]
               adc dx, word ptr [SI][LD_FS_BeginSector]+2

               mov cx, 1      
               call proc_disk_write
               jc short loc_add_new_fs_section_retn
               
               mov byte ptr [FS_BuffValidData], 1

loc_ans_write_new_section_header:
               mov di, bx
              ; 21/07/2011
              ;add di, 20  ; Only the first 20 bytes are valid
               add di, 18 ; Offset FS_DDT_Sector_Count + 2
               xor ax, ax
              ; mov cx, 296 ; 492/2
               mov cx, 298 ; 496/2
               rep stosw
               ; 05/12/2010
               mov cx, word ptr [FS_ans_sector_count] 
               mov word ptr ES:[BX]+FS_DDT_Sector_Count, cx
               ; 21/07/2011 
               ;mov word ptr ES:[BX]+FS_DDT_Sector_Count+2, ax ; 0
               ;
               mov byte ptr [FS_BuffValidData], al ; 0
               mov word ptr ES:[BX]+FS_DDT_Next_DDT_Number, ax ; 0
               mov word ptr ES:[BX]+FS_DDT_Next_DDT_Number+2, ax ; 0
               inc word ptr ES:[BX]+FS_DDT_Number
               mov ax, word ptr [FS_ans_new_section]
               mov dx, word ptr [FS_ans_new_section]+2
               mov word ptr ES:[BX]+FS_DDT_Directory_Number, ax
               mov word ptr ES:[BX]+FS_DDT_Directory_Number+2, dx
  
               add ax, word ptr [SI][LD_FS_BeginSector]
               adc dx, word ptr [SI][LD_FS_BeginSector]+2

               mov cx, 1      
               call proc_disk_write
               jc short loc_add_new_fs_section_return
               
               xor di, di
               dec di
               mov word ptr [FS_CurrentSection], di ; FFFFh
               mov word ptr [FS_CurrentSection]+2, di ; FFFFh
               inc di

loc_ans_reset_new_section_data:
               mov cx, 10 ; 20/2
               push ax
               xor ax, ax
               rep stosw
               pop ax
               mov cx, word ptr [FS_ans_sector_count]
loc_ans_write_new_section_data:
              ; 05/12/2010
               push cx
               mov cx, 1  
               call proc_disk_write
               pop cx
               jc short loc_add_new_fs_section_return
               loop loc_ans_write_new_section_data  

              ;27/07/2010
               mov ax, word ptr [FS_ans_new_section]
               mov dx, word ptr [FS_ans_new_section]+2
               mov bx, word ptr [FS_ans_DT_type]
               mov cx, word ptr [FS_ans_sector_count]

               call proc_allocate_fs_disk_sectors

loc_add_new_fs_section_return:
               retn

FS_ans_DT_type: dw 0
FS_ans_sector_count: dw 0
FS_ans_last_section: dd 0
FS_ans_new_section: dd 0

proc_add_new_fs_section endp

proc_allocate_fs_disk_sectors proc near
                ; 20/03/2011
                ; 05/12/2010
                ; 31/07/2010 
                ; 27/07/2010
                ; 26/07/2010
                ; 25/07/2010
                ;
                ; INPUT ->
                ; ES = Sector buffer segment
                ; BL = Section header type
                ; BH = Section data type
                ; DS:SI = Logical dos drv desc. table address
                ; DX:AX = Section address
                ; CX = Sector count of section (except header)
                ; OUTPUT -> 
                ; ES:BX = Sector buffer address (BX = 0)
                ; DS:SI = Logical dos drv desc. table address
                ;         (DI will be changed at return)
                ; clc -> DX:AX = Allocated section address (same) 
                ; CX = count of sectors in section (except header)
                ;       

                mov word ptr [FS_ans_last_section], ax
                mov word ptr [FS_ans_last_section]+2, dx
                mov word ptr [FS_ans_sector_count], cx
               ;mov word ptr [FS_ans_DT_type], bx

               ; 20/03/2011 
                or cx, cx
                jnz short loc_allocate_fs_disk_sectors_update_dat1
                mov ax, 0Dh ; Invalid data
                stc
                retn 

loc_allocate_fs_disk_sectors_update_dat1:
               xor di, di
               dec di
               mov word ptr [DAT_Buffer_Sector], di ; FFFFh
               mov word ptr [DAT_Buffer_Sector]+2, di ; FFFFh
               inc di
              ;mov word ptr [DAT_Buffer_Offset], di
               mov cx, 1
               mov byte ptr [DAT_Buffer_Updated], ch ;0

loc_allocate_fs_disk_sectors_update_dat2:
               call proc_update_dat
               jc short loc_allocate_fs_disk_sectors_update_dat_retn

               add ax, 1
               adc dx, 0 

               mov cx, word ptr [FS_ans_sector_count]
               xchg bh, bl ; BL -> section data type

loc_allocate_fs_disk_sectors_update_dat_next:
               call proc_update_dat
               jc short loc_allocate_fs_disk_sectors_update_dat_retn
               add ax, 1
               adc dx, 0 
               loop loc_allocate_fs_disk_sectors_update_dat_next

loc_allocate_fs_disk_sectors_update_dat_w_dat_s:
               xor bx, bx
               mov ax, word ptr [DAT_Buffer_Sector]
               mov dx, word ptr [DAT_Buffer_Sector]+2
               call proc_write_dat_sector
               jnc short loc_allocate_fs_disk_sectors_update_mat

loc_allocate_fs_disk_sectors_update_dat_retn:               
               retn

loc_allocate_fs_disk_sectors_update_mat:
               call proc_load_mat
               jc short loc_allocate_fs_disk_sectors_update_dat_retn

               mov ax, word ptr ES:[BX]+FS_MAT_FreeSectors
               mov dx, word ptr ES:[BX]+FS_MAT_FreeSectors+2
              ; 05/12/2010 
               mov cx, word ptr [FS_ans_sector_count]
               inc cx
               sub ax, cx
               sbb dx, 0
 
               mov word ptr [SI][LD_FreeSectors], ax
               mov word ptr [SI][LD_FreeSectors]+2, dx  

               mov ax, word ptr [FS_ans_last_section]
	       mov dx, word ptr [FS_ans_last_section]+2

               dec cx

loc_allocate_fs_disk_sectors_check_ffs:
               cmp dx, word ptr ES:[BX]+FS_MAT_FirstFreeSector+2
               jne short loc_allocate_fs_disk_sectors_check_ffs_loop
               cmp ax, word ptr ES:[BX]+FS_MAT_FirstFreeSector
               jne short loc_allocate_fs_disk_sectors_check_ffs_loop

loc_alloc_fs_disk_sectors_get_DAT_sectorcount:             
                mov ax, word ptr ES:[BX]+FS_MAT_SectorCount
                mov dx, word ptr ES:[BX]+FS_MAT_SectorCount+2
                 
                sub ax, 1
                sbb dx, 0

                mov word ptr [DAT_LastSector], ax
                mov word ptr [DAT_LastSector]+2, dx

                mov ax, word ptr ES:[BX]+FS_MAT_FirstFreeSector
                mov dx, word ptr ES:[BX]+FS_MAT_FirstFreeSector+2

                xor cx, cx
                dec cx ; FFFFh
                cmp ax, cx
                jne short loc_alloc_fs_disk_sectors_load_dat_sector

                cmp dx, cx
                jne short loc_alloc_fs_disk_sectors_load_dat_sector

                xor ax, ax
                xor dx, dx
                jmp short loc_alloc_fs_disk_sectors_load_dat_sector_next

loc_alloc_fs_disk_sectors_load_dat_sector:
                mov cx, 512
                call Rx_DOS_Div32
                mov di, bx
                xor bx, bx

loc_alloc_fs_disk_sectors_check_dat_sector_limit:
                mov word ptr [Current_FS_Sector], ax
                mov word ptr [Current_FS_Sector]+2, dx

                cmp dx, word ptr [DAT_LastSector]+2
                ja short loc_alloc_fs_disk_sectors_end_of_DAT_sectors
                jb short loc_alloc_fs_disk_sectors_load_dat_sector_next
                cmp ax, word ptr [DAT_LastSector]
                ja short loc_alloc_fs_disk_sectors_end_of_DAT_sectors

loc_alloc_fs_disk_sectors_load_dat_sector_next:
                call proc_load_dat_sector
                jc short loc_allocate_fs_disk_sectors_update_dat_return

loc_alloc_fs_disk_sectors_check_dat_cell_value:
                cmp byte ptr ES:[DI], 90h
                je short loc_allocate_fs_disk_sectors_change_mat_ffs1

                inc di
                cmp di, 512
                jb short loc_alloc_fs_disk_sectors_check_dat_cell_value

               ;mov ax, word ptr [Current_FS_Sector]
               ;mov dx, word ptr [Current_FS_Sector]+2

                add ax, 1
                adc dx, 0
                jmp short loc_alloc_fs_disk_sectors_check_dat_sector_limit

loc_allocate_fs_disk_sectors_check_ffs_loop:
               add ax, 1
               adc dx, 0  
               loop loc_allocate_fs_disk_sectors_check_ffs
               jmp short loc_allocate_fs_disk_sectors_pass_change_mat_ffs

loc_alloc_fs_disk_sectors_end_of_DAT_sectors:
                xor ax, ax
                dec ax
                mov dx, ax ; FFFFh
                jmp short loc_allocate_fs_disk_sectors_change_mat_ffs2
              
loc_allocate_fs_disk_sectors_change_mat_ffs1:
               mov ax, word ptr [Current_FS_Sector]
               mov dx, word ptr [Current_FS_Sector]+2

               push bx
               mov bx, 512
               call proc_mul32
               pop bx
               add ax, di
               adc dx, 0

loc_allocate_fs_disk_sectors_change_mat_ffs2:
               mov word ptr [Current_FS_Sector], ax
               mov word ptr [Current_FS_Sector]+2, dx

               call proc_load_mat
               jc short loc_allocate_fs_disk_sectors_update_dat_return

               mov ax, word ptr [Current_FS_Sector]
               mov dx, word ptr [Current_FS_Sector]+2

loc_allocate_fs_disk_sectors_change_mat_ffs3:
	       mov word ptr ES:[BX]+FS_MAT_FirstFreeSector, ax
               mov word ptr ES:[BX]+FS_MAT_FirstFreeSector+2, dx

loc_allocate_fs_disk_sectors_pass_change_mat_ffs:
               mov ax, word ptr [SI][LD_FreeSectors]
               mov dx, word ptr [SI][LD_FreeSectors]+2  

               mov word ptr ES:[BX]+FS_MAT_FreeSectors, ax
               mov word ptr ES:[BX]+FS_MAT_FreeSectors+2, dx

               call proc_write_mat 
               jc short loc_allocate_fs_disk_sectors_update_dat_return

               mov ax, word ptr [FS_ans_last_section]
	       mov dx, word ptr [FS_ans_last_section]+2

               mov cx, word ptr [FS_ans_sector_count]

               xor di, di

loc_allocate_fs_disk_sectors_update_dat_return:               
                               
               retn

proc_allocate_fs_disk_sectors endp

proc_get_section_for_fs_file_sector proc near
              ; 21/07/2011
              ; 16/07/2011 
              ; 12/09/2010 
              ; 08/09/2010
              ; 04/09/2010
              ; INPUT ->
                ; DS:SI = Logical dos drv table
                ; DX:AX = Sector sequence number of the file
                ; BX:CX = File Number (the 1st FDT address)         
              ; OUTPUT ->
                ; DX:AX = Section header addr of the requested sector
                ; BX:CX = Sector offset of the section
                ; DI = Pointer to sector count of the section
                ; DI+4 = Pointer to next section header address
                ; stc & AX > 0 -> error except EOF
                ; stc & AX = 0 -> EOF ->
                ;  Last section before the req. sector number
                ;  BX:CX = Header address of the last section
                ;  DX = Required additional sectors
                ;  DX = 0 -> Beyond of 65535 additional sectors 

              ; 12/09/2010 
                mov word ptr [FS_File_Sec_SN], ax
                mov word ptr [FS_File_Sec_SN]+2, dx
                mov ax, cx
                mov dx, bx
              ; 04/09/2010
                xor cx, cx
                dec cx
                mov word ptr [FS_File_PS_LSN], cx   ; FFFFh
                mov word ptr [FS_File_PS_LSN]+2, cx ; FFFFh   

loc_gdsk_lba_for_fsector_gns:
                mov word ptr [FS_File_LFDT], ax
                mov word ptr [FS_File_LFDT]+2, dx
                call proc_get_next_section
                jnc short loc_gdsk_lba_for_fsector_chk_csc
               ; 08/09/2010
               ;xor dx, dx
                or ax, ax
                jnz short loc_gdsk_lba_for_fsector_stc_retn
                 
loc_gdsk_lba_for_fsector_chk_csc:
               ; 21/07/2011
                or cx, cx
                jnz short loc_gdsk_lba_for_fsector_chk_upl
                or bx, bx
                jnz short loc_gdsk_lba_for_fsector_chk_upl
loc_gdsk_lba_for_fsector_invld_format:
                mov ax, 0Bh ; Invalid fs format
                jmp short loc_gdsk_lba_for_fsector_stc_retn_invf    
loc_gdsk_lba_for_fsector_chk_upl:
               ; 08/09/2010
                mov word ptr [FS_CS_Sectors], cx
                mov word ptr [FS_CS_Sectors]+2, bx
                mov word ptr [FS_File_NFDT], ax
                mov word ptr [FS_File_NFDT]+2, dx

                add cx, word ptr [FS_File_PS_LSN]
		adc bx, word ptr [FS_File_PS_LSN]+2

                cmp bx, word ptr [FS_File_Sec_SN]+2
                ja short loc_gdsk_lba_for_fsector_retn1
                jb short loc_gdsk_lba_for_fsector_chk_upl_n
                cmp cx, word ptr [FS_File_Sec_SN]
                jnb short loc_gdsk_lba_for_fsector_retn1
loc_gdsk_lba_for_fsector_chk_upl_n:
                or ax, ax
                jnz short loc_gdsk_lba_for_fsector_gns_next
                or dx, dx
                jnz short loc_gdsk_lba_for_fsector_gns_next

                mov dx, word ptr [FS_File_Sec_SN]
                mov ax, word ptr [FS_File_Sec_SN]+2
                sub dx, cx
               ; jz or jc/jb is not possible here if ax=bx !
                sbb ax, bx
               ; jc/jb not possible here !
                jz short loc_gdsk_lba_for_fsector_retn_LFDT
               ; 16/07/2011
                ;
                ; Max. 65535 additional sectors are acceptable
                ; for one additional section (except FDT)
                ; Beyond of 64KB is not allowed for that.
loc_gdsk_lba_for_fsector_stc_retn_invd:
              ; 16/07/2011
                mov ax, 0Dh ; Invalid data
loc_gdsk_lba_for_fsector_stc_retn_invf:
                xor dx, dx
               ;
loc_gdsk_lba_for_fsector_retn_LFDT:
                mov cx, word ptr [FS_File_LFDT]
                mov bx, word ptr [FS_File_LFDT]+2
              
               ; 08/09/2010
loc_gdsk_lba_for_fsector_stc_retn:
                stc
                jmp short loc_gdsk_lba_for_fsector_retn2

loc_gdsk_lba_for_fsector_gns_next:
                mov word ptr [FS_File_PS_LSN], cx
		mov word ptr [FS_File_PS_LSN]+2, bx
                jmp short loc_gdsk_lba_for_fsector_gns
       
loc_gdsk_lba_for_fsector_retn1:
                mov cx, word ptr [FS_File_Sec_SN]
		mov bx, word ptr [FS_File_Sec_SN]+2
                mov ax, word ptr [FS_File_PS_LSN]
                mov dx, word ptr [FS_File_PS_LSN]+2
                add ax, 1
                adc dx, 0
                ;mov word ptr [FS_File_CS_FSN], ax
                ;mov word ptr [FS_File_CS_FSN]+2, dx  
                sub cx, ax ; FS_File_CS_FSN
		sbb bx, dx ; FS_File_CS_FSN]+2
               ; 21/07/2011
                jc short loc_gdsk_lba_for_fsector_invld_format
                mov ax, word ptr [FS_File_LFDT]
                mov dx, word ptr [FS_File_LFDT]+2
loc_gdsk_lba_for_fsector_retn2:
                mov di, offset FS_CS_Sectors
                retn  

; 04/09/2010                
FS_File_Sec_SN: dd 0
FS_File_LFDT: dd 0
; 08/09/2010
FS_CS_Sectors: dd 0 
; 19/09/2010
FS_File_NFDT: dd 0
; 08/09/2010
;FS_File_CS_FSN:
              ; dd 0 
; 04/09/2010                
FS_File_PS_LSN: dd 0

proc_get_section_for_fs_file_sector endp

proc_update_open_fs_file_size_lmdt proc near
               ; 07/11/2010 BugFix
               ; 29/10/2010 Modification
               ; 29/10/2010 Last modification date&time pointer
               ;            is in DS:DI 
               ;            or DI = 0 -> Current date&time 
               ; 09/09/2010
               ; 08/09/2010
               ; INPUT -> 
               ;  AX = File handle
               ;  DX:CX = File size
               ;  DS:SI = DOS drv table  
               ;    DI = 0 -> Use current date&time as lmdt
               ;    DI > 0 -> lmdt value is in DS:DI (dword ptr) 
               ; OUTPUT -> DS:SI = Logical DOS drv table addr.
               ;  clc (cf=0) -> updated
               ;  stc (cf=1) -> error code in AX
               ;  AX, BX, CX, DX, (DI) will be changed
               ;  ES:BX = sector buffer address (FDT)
              
               cmp byte ptr [SI][LD_FS_FSType], 0A1h
               je short loc_fs_ufdtlmdt_get_fdt_addr
               ja short loc_fs_ufdtlmdt_unknown_media_type_err
loc_fs_ufdtlmdt_invalid_drive_err:
               mov ax, 0Fh ;Invalid drive 
               retn

loc_fs_ufdtlmdt_unknown_media_type_err:
               stc
               mov ax, 1Ah  ;Unknown media type
               retn

loc_fs_ufdtlmdt_invalid_handle:
                mov ax, 06h ; Invalid handle
loc_fs_ufdtlmdt_stc_retn:
                stc
loc_fs_ufdtlmdt_retn:
                retn

loc_fs_ufdtlmdt_get_fdt_addr:
               ; 29/10/2010 
                mov es, ax
                cmp byte ptr ES:[OF_Type], 'F'
                jne short loc_fs_ufdtlmdt_invalid_handle

                mov al, byte ptr ES:[OF_OFL_SN]
                dec al 
                mov ah, OFL_EntrySize
                mul ah

                mov bx, offset Open_Files_List
                add bx, ax
                push es
                pop ax
                cmp ax, word ptr [BX]+OFL_Handle     ; +6
                jne short loc_fs_ufdtlmdt_invalid_handle

                push dx
 
                mov ax, word ptr [BX]+OFL_Cluster    ; +2
                mov dx, word ptr [BX]+OFL_Cluster+2  ; +4

               ; mov word ptr [FS_LMDT_ptr], di

              ; 08/09/2010
loc_write_file_fs_update_fdt_lm_dt:
               mov bx, word ptr [Sector_Buffer]
               mov es, bx

               xor bx, bx
               mov byte ptr [FS_BuffValidData], bl
               mov word ptr [FS_CurrentSection], ax
               mov word ptr [FS_CurrentSection]+2, dx

               add ax, word ptr [SI][LD_FS_BeginSector]
               adc dx, word ptr [SI][LD_FS_BeginSector]+2

              ; 07/11/2010
               mov word ptr [FS_ULMDT_FDT_LBA], ax
               mov word ptr [FS_ULMDT_FDT_LBA]+2, dx         

              ; 09/09/2010
               push cx
               xor cx, cx 
              ;inc cx
               inc cl      
               call proc_disk_read
               pop cx
              ; 29/10/2010 
               pop dx
               jc short loc_fs_ufdtlmdt_retn

               mov byte ptr [FS_BuffValidData], 1

               mov ax, word ptr ES:[BX]+1
               cmp ax, 'TD'
               jne short loc_fs_ufdtlmdt_invalid_handle
               mov al, byte ptr ES:[BX]
               cmp al, 'F'
               jne short loc_fs_ufdtlmdt_invalid_handle

               mov word ptr ES:[BX]+FS_FDT_File_Size, cx
               mov word ptr ES:[BX]+FS_FDT_File_Size+2, dx
              ;mov word ptr ES:[BX]+FS_FDT_File_Size+4, 0

              ; 29/10/2010
              ; mov di, word ptr [FS_LMDT_ptr]
               or di, di
               jz short loc_fs_updt_write_current_lm_dt

               mov ax, word ptr [DI]
               mov dx, word ptr [DI]+2
               push di
               mov di, bx
               add di, fs_fdt_lm_year
               call proc_convert_dos_date_time_to_fs_format
               pop di
               jmp short loc_fs_updt_write_lm_dt   

loc_fs_updt_write_current_lm_dt:
               mov ah, 02h                        ; Return Current Time
               int 1Ah
               xchg ch,cl
               xchg dh,dl
               mov word ptr ES:[BX]+fs_fdt_lm_hour, cx
               mov word ptr ES:[BX]+fs_fdt_lm_second, dx
                 
               mov ah, 04h                        ; Return Current Date
               int 1Ah
               xchg ch,cl
               xchg dh,dl
               mov word ptr ES:[BX]+fs_fdt_lm_year, cx
               mov word ptr ES:[BX]+fs_fdt_lm_month, dx

loc_fs_updt_write_lm_dt:
              ; 07/11/2010
               mov ax, word ptr [FS_ULMDT_FDT_LBA]
               mov dx, word ptr [FS_ULMDT_FDT_LBA]+2   
               xor cx, cx 
               ;inc cx
               inc cl      
               call proc_disk_write
               retn

; 29/10/2010
;FS_LMDT_ptr: dw 0
; 07/11/2010
FS_ULMDT_FDT_LBA: dd 0 

proc_update_open_fs_file_size_lmdt endp

proc_truncate_fs_file proc near
               ; 30/07/2011 dword ptr [DI] return
               ; 10/10/2010
               ; 19/09/2010
               ; 12/09/2010
               ; INPUT ->
               ; DS:SI = Logical dos drv desc. table address
               ; DX:AX = File Size in bytes
               ; BX:CX = File Description Table address 
               ; OUTPUT ->
               ;  DS:SI = Logical dos drv desc. table address
               ;  BX:CX = Count of truncated/removed sectors
               ;  clc -> successfull 
               ;   (BX:CX = 0 -> First section is not deleted)
               ;  DX:AX -> Free sectors
               ;  (ES = DS or ES same with input)
               ;    DI = (New) Last Section pointer (dd) 
               ;  stc -> error code in AL
               ;    DH = FS Type, DL = FAT Type
               ;    DI = 0 
               ; NOTE:
               ; This procedure does not update lm date&time !
               ;

              ;cmp byte ptr [SI][LD_FSType], 0A1h
              ;jne short loc_tfsf_invalid_drive

               sub ax, 1
               sbb dx, 0
              ;jc short loc_tfsf_not_truncated

               push bx
               push cx
              ;mov cx, word ptr [SI][LD_FS_BytesPerSec]
               mov cx, 512
               call Rx_DOS_Div32
               pop cx
               pop bx

loc_tfsf_get_file_section:
              ; 19/09/2010
               call proc_get_section_for_fs_file_sector
               jnc short loc_tfsf_get_next_section
              ; 30/07/2011
               or ax, ax
               jnz short loc_tfsf_error_retn

loc_tfsf_not_truncated:
              ; 30/07/2011
               xor cx, cx ; 0
               xor bx, bx ; 0
               xor di, di ; 0
               jmp short loc_tfsf_not_truncated_retn
loc_tfsf_truncated_retn:
               mov di, offset FS_LastFileSection 
loc_tfsf_not_truncated_retn:
               mov ax, word ptr [SI][LD_FreeSectors]
               mov dx, word ptr [SI][LD_FreeSectors]+2
               retn

;loc_tfsf_invalid_drive:
;              mov ax, 0Fh ; Invalid drive
;              xor cx, cx
;              xor bx, bx

loc_tfsf_error_retn:
              ; 30/07/2011
               xor di, di ; mov di, 0
               mov dx, word ptr [SI][LD_FATType]
               stc 
               retn

loc_tfsf_get_next_section:
              ; 30/07/2011
               mov word ptr [FS_LastFileSection], ax
               mov word ptr [FS_LastFileSection]+2, dx
              ; 
               add cx, 1
               adc bx, 0
               ; new sector count -> BX:CX
             ; 10/10/2010
               push bx
               push cx
               mov cx, word ptr [DI]
               mov bx, word ptr [DI]+2
               or bx, bx
               jnz short loc_tfsf_truncate_section
               cmp cx, 4
               ja short loc_tfsf_truncate_section
               pop ax
               pop ax
               mov ax, word ptr [DI]+4 ; Next section header, lw
               mov dx, word ptr [DI]+6 ; Next section header, hw
               mov cx, bx
               mov word ptr [FS_truncated_Sectors], cx ; 0
               mov word ptr [FS_truncated_Sectors]+2, bx ; 0
               jmp short pass_tfsf_truncate_section
loc_tfsf_truncate_section:
               pop cx
               pop bx
               mov di, word ptr [Sector_Buffer]
               mov es, di
               xor di, di
               call proc_truncate_fs_section
               ; FS_Truncated_Sectors = BX:CX
               jc short loc_tfsf_error_retn

pass_tfsf_truncate_section:
               or ax, ax
               jnz short loc_tfsf_truncate_section_next
               or dx, dx
               jz short loc_tfsf_truncated_retn

loc_tfsf_truncate_section_next:
             ; 10/10/2010
             ; mov word ptr [FS_truncated_Sectors], cx ; 0
             ; mov word ptr [FS_truncated_Sectors]+2, bx ; 0

loc_tfsf_delete_fs_section_next0:
               call proc_delete_fs_section
               jnc short loc_tfsf_delete_fs_section_next1
               or ax, ax
               jz short loc_tfsf_delete_fs_section_next2
               mov cx, word ptr [FS_truncated_Sectors]
               mov bx, word ptr [FS_truncated_Sectors]+2
               jmp short loc_tfsf_error_retn
               
loc_tfsf_delete_fs_section_next1:
               add word ptr [FS_truncated_Sectors], cx
               adc word ptr [FS_truncated_Sectors]+2, bx
               jmp short loc_tfsf_delete_fs_section_next0 
                
loc_tfsf_delete_fs_section_next2:
               add cx, word ptr [FS_truncated_Sectors]
               adc bx, word ptr [FS_truncated_Sectors]+2
              ; jnc short loc_tfsf_truncated_retn
               jmp short loc_tfsf_truncated_retn

;loc_tfsf_invalid_data:
;              mov ax, 0Dh ; Invalid data
;              jmp short loc_tfsf_error_retn 
           
FS_Truncated_Sectors: dd 0
 ; 30/07/2011 
FS_LastFileSection: dd 0

proc_truncate_fs_file endp

proc_truncate_fs_section proc near
               ; 10/10/2010
               ; 19/09/2010
               ; INPUT ->
               ;  DS:SI = Logical dos drv desc. table address
               ;  DX:AX = Section header address
               ;  ES:DI = FDT/DDT buffer address (DI=0)
               ;  BX:CX = (New) Sector count of the section
               ; OUTPUT ->
               ;  DS:SI = Logical dos drv desc. table address
               ;  BX:CX = Count of truncated/removed sectors
               ;  DX:AX = Next section
               ;  stc <-> BX:CX = 0 & DX:AX = any
               ;          truncating is not applicable
               ;  stc -> DX:AX = 0 & BX:CX = 0
               ;         if next section is not valid or
               ;         there is an error
               ;  ES=DS
               ; (DI=0) 
               ; NOTE:
               ; This procedure does not update lm date&time !
               ;

loc_tfss_chk_section_header:
              ; 10/10/2010 
               push bx
               mov bx, word ptr ES:[DI]
               cmp bh, 'D'
               jne short loc_tfss_inv_section_header_pop_retn
               cmp bl, 'F' 
               je short loc_tfss_compare_buffer_section1
               cmp bl, 'D'
               je short loc_tfss_compare_buffer_section1

loc_tfss_inv_section_header_pop_retn:
               pop ax
loc_tfss_invalid_section_header_retn:
               xor ax, ax
               xor dx, dx
loc_tfss_section_header_err_retn:
               xor cx, cx
               xor bx, bx
              ;xor di, di          
               stc
               push ds
               pop es 
               retn

loc_tfss_compare_buffer_section1:
               pop bx
               cmp byte ptr ES:[DI]+2, 'T'
               jne short loc_tfss_invalid_section_header_retn
loc_tfss_compare_buffer_section2:
               push word ptr ES:[DI]+FS_FDT_Sector_Count ;+ 16
               pop word ptr [FS_Truncated_Sectors]
               push word ptr ES:[DI]+FS_FDT_Sector_Count+2 ;+ 18
               pop word ptr [FS_Truncated_Sectors]+2
               cmp bx, word ptr [FS_Truncated_Sectors]+2 ;+ 18
               je short loc_tfss_compare_buffer_section3
               ja short loc_tfss_compare_buffer_section0
               mov word ptr ES:[DI]+FS_FDT_Sector_Count+2, bx
               jmp short loc_tfss_compare_buffer_section4
loc_tfss_compare_buffer_section3:
               cmp cx, word ptr [FS_Truncated_Sectors] ;+ 16
               jb short loc_tfss_compare_buffer_section4
loc_tfss_compare_buffer_section0:
               mov ax, word ptr ES:[DI]+FS_FDT_Next_FDT_Number ;+ 12
               mov dx, word ptr ES:[DI]+FS_FDT_Next_FDT_Number+2 ;+ 14
               jmp short loc_tfss_section_header_err_retn
loc_tfss_compare_buffer_section4:
               mov word ptr ES:[DI]+FS_FDT_Sector_Count, cx
               push word ptr ES:[DI]+FS_FDT_Next_FDT_Number
               push word ptr ES:[DI]+FS_FDT_Next_FDT_Number+2
               ;DX:AX = Section header address
               push dx
               push ax
               xor cx, cx
               mov word ptr ES:[DI]+FS_FDT_Next_FDT_Number, cx ; 0
               mov word ptr ES:[DI]+FS_FDT_Next_FDT_Number+2, cx ; 0
               inc cx ; 1
               mov bx, di
               add ax, word ptr [SI][LD_FS_BeginSector]
               adc dx, word ptr [SI][LD_FS_BeginSector]+2
               call proc_disk_write
               pop ax ; Section header address, lW
               pop dx ; Section header address, hW
               jnc short loc_tfss_unlink_disk_sectors
               pop dx ; Next section address, hW
               pop ax ; Next section address, lW
               jmp short loc_tfss_section_header_err_retn

loc_tfss_unlink_disk_sectors:
               mov cx, word ptr ES:[BX]+FS_FDT_Sector_Count 
               mov di, word ptr ES:[BX]+FS_FDT_Sector_Count+2 
               add ax, cx
               adc dx, di
               adc ax, 1
               adc dx, 0
               sub word ptr [FS_Truncated_Sectors], cx
               sbb word ptr [FS_Truncated_Sectors]+2,di
               mov cx, word ptr [FS_Truncated_Sectors]
               mov bx, word ptr [FS_Truncated_Sectors]+2
               push cx
               push bx 
                ;  DS:SI = Logical dos drv desc. table address
                ;  DX:AX = the first sector number to be unlinked
                ;          (offset to FS beginning sector)
                ;  BX:CX = Sector count
                ;  ES = Sector buffer segment
               call proc_deallocate_fs_disk_sectors
                ;  DS:SI = Logical dos drv desc. table address
                ;  DX:AX = Free sectors
                ;  ES:BX = Sector buffer segment (BX=0)
                ;  stc -> error code in ax, dx = 0
                ;         BX, CX, DI will be modified
               pop bx
               pop cx 
               pop dx ; Next section, hw
               pop ax ; Next section, lw
               jc loc_tfss_section_header_err_retn
              ;xor di, di
               push ds
               pop es
               retn 
                
proc_truncate_fs_section endp

proc_deallocate_fs_disk_sectors proc near
                ; 10/10/2010
                ; 19/09/2010
                ;  derivation for proc_allocate_fs_disk_sectors 
                ;
                ; INPUT ->
                ;  DS:SI = Logical dos drv desc. table address
                ;  DX:AX = the first sector number to be unlinked
                ;          (offset to FS beginning sector)
                ;  BX:CX = Sector count
                ;  ES = Sector buffer segment
                ; OUTPUT -> 
                ;  DS:SI = Logical dos drv desc. table address
                ;  DX:AX = Free sectors
                ;  ES:BX = Sector buffer address (BX=0)
                ;  stc -> error code in ax, dx = 0
                ;         BX, CX, DI will be modified
                ;       

                mov word ptr [FS_ans_last_section], ax
                mov word ptr [FS_ans_last_section]+2, dx
                mov word ptr [FS_ans_sector_count], cx
                mov word ptr [FS_ans_DT_type], bx

loc_deallocate_fs_disk_sectors_update_dat1:
               xor di, di
               dec di
               mov word ptr [DAT_Buffer_Sector], di ; FFFFh
               mov word ptr [DAT_Buffer_Sector]+2, di ; FFFFh
               inc di
              ;mov word ptr [DAT_Buffer_Offset], di
               mov byte ptr [DAT_Buffer_Updated], 0

loc_deallocate_fs_disk_sectors_update_dat2:
               push bx
               mov bl, 90h ; Free sector
loc_deallocate_fs_disk_sectors_update_dat_next:
               call proc_update_dat
               jc short loc_deallocate_fs_disk_sectors_ud_pop_retn
               add ax, 1
               adc dx, 0 
               loop loc_deallocate_fs_disk_sectors_update_dat_next
               pop bx
               or bx, bx
               jz short loc_deallocate_fs_disk_sectors_update_dat_w_dat_s
               dec bx
               jmp short loc_deallocate_fs_disk_sectors_update_dat2

loc_deallocate_fs_disk_sectors_ud_pop_retn:
               pop bx               
loc_deallocate_fs_disk_sectors_ud_retn:
               retn

loc_deallocate_fs_disk_sectors_update_dat_w_dat_s:
               mov ax, word ptr [DAT_Buffer_Sector]
               mov dx, word ptr [DAT_Buffer_Sector]+2
               call proc_write_dat_sector
               jc short loc_deallocate_fs_disk_sectors_ud_retn

loc_deallocate_fs_disk_sectors_update_mat:
               call proc_load_mat
               jc short loc_deallocate_fs_disk_sectors_ud_retn

               mov ax, word ptr ES:[BX]+FS_MAT_FreeSectors
               mov dx, word ptr ES:[BX]+FS_MAT_FreeSectors+2
               add ax, word ptr [FS_ans_sector_count]
               adc dx, word ptr [FS_ans_DT_type] ; sector count hW

               mov word ptr [SI][LD_FreeSectors], ax
               mov word ptr [SI][LD_FreeSectors]+2, dx  

               mov ax, word ptr [FS_ans_last_section]
	       mov dx, word ptr [FS_ans_last_section]+2

loc_deallocate_fs_disk_sectors_check_ffs:
              ;10/10/2010 
               cmp dx, word ptr ES:[BX]+FS_MAT_FirstFreeSector+2
               ja short loc_deallocate_fs_disk_write_mat
               jb short loc_deallocate_fs_disk_sectors_change_mat_ffs
               cmp ax, word ptr ES:[BX]+FS_MAT_FirstFreeSector
               ja short loc_deallocate_fs_disk_write_mat

loc_deallocate_fs_disk_sectors_change_mat_ffs:
               mov word ptr ES:[BX]+FS_MAT_FirstFreeSector, ax
               mov word ptr ES:[BX]+FS_MAT_FirstFreeSector+2, dx

loc_deallocate_fs_disk_write_mat:
               mov ax, word ptr [SI][LD_FreeSectors]
               mov dx, word ptr [SI][LD_FreeSectors]+2  
               mov word ptr ES:[BX]+FS_MAT_FreeSectors, ax
               mov word ptr ES:[BX]+FS_MAT_FreeSectors+2, dx
               call proc_write_mat 
               jc short loc_deallocate_fs_disk_sectors_ud_retn

               mov ax, word ptr ES:[BX]+FS_MAT_FreeSectors
               mov dx, word ptr ES:[BX]+FS_MAT_FreeSectors+2

               retn

proc_deallocate_fs_disk_sectors endp

proc_delete_fs_section proc near
               ; 19/09/2010
               ; INPUT ->
               ;  DS:SI = Logical dos drv desc. table address
               ;  DX:AX = Section header (FDT or DDT) address
               ; OUTPUT ->
               ;  DS:SI = Logical dos drv desc. table address
               ;  DX:AX = Next section
               ;  BX:CX = Deleted sector count
               ;  stc ->  AX = Error Code, DX = 0
               ;          BX:CX = Next section
               ;  stc ->  DX:AX = 0 -> No next section
               ;          BX:CX = Deleted sector count
               ;  ES=DS 
               ; NOTE:
               ; This procedure does not update lm date&time !
               ;

loc_dfss_read_buffer_section:
               mov bx, word ptr [Sector_Buffer]
               mov es, bx

               xor bl, bl
               mov byte ptr [FS_BuffValidData], bl
               mov word ptr [FS_CurrentSection], ax
               mov word ptr [FS_CurrentSection]+2, dx

               add ax, word ptr [SI][LD_FS_BeginSector]
               adc dx, word ptr [SI][LD_FS_BeginSector]+2

               push dx
               push ax 

               xor bh, bh
               xor cx, cx

               mov word ptr [FS_File_NFDT], cx ; 0
               mov word ptr [FS_File_NFDT]+2, bx ; 0
 
              ;inc cx
               inc cl      
               call proc_disk_read
               jc short loc_dfss_compare_buffer_err_retn0

loc_dfss_compare_buffer_section0:
               mov cx, word ptr ES:[BX]
               cmp cx, 'DF'
               je short loc_dfss_compare_buffer_section1
               cmp cx, 'DD'
               je short loc_dfss_compare_buffer_section1

loc_dfss_compare_buffer_inv_retn:
               mov ax, 0Bh ; Invalid format
loc_dfss_compare_buffer_err_retn1:
               xor dx, dx
               stc
loc_dfss_compare_buffer_err_retn0:
               pop cx
               pop cx
loc_dfss_compare_buffer_err_retn2:  
               mov cx, word ptr [FS_File_NFDT]
               mov bx, word ptr [FS_File_NFDT]+2
               push ds
               pop es 
               retn

loc_dfss_buffer_section_write_err:
              mov ax, 05h  ; Access denied 
                     ; (The 1'st FDT/DDT should not be deleted)
              jmp short loc_dfss_compare_buffer_err_retn1

loc_dfss_compare_buffer_section1:
              cmp byte ptr ES:[BX]+2, 'T'
              jne short loc_dfss_compare_buffer_inv_retn
              cmp word ptr ES:[BX]+FS_FDT_Number, 0
              jna short loc_dfss_buffer_section_write_err

loc_dfss_put_deleted_sign_onto_header:
              mov byte ptr ES:[BX]+2, 'E'
              pop ax
              pop dx
              xor cx, cx
              inc cl ; 1
              call proc_disk_write
              jc short loc_dfss_compare_buffer_err_retn2

loc_dfss_unlink_disk_sectors:
               mov cx, word ptr ES:[BX]+FS_FDT_Sector_Count 
               mov bx, word ptr ES:[BX]+FS_FDT_Sector_Count+2
               add cx, 1
               adc bx, 0

               mov word ptr [FS_CS_Sectors], cx
               mov word ptr [FS_CS_Sectors]+2, bx

               push word ptr ES:[DI]+FS_FDT_Next_FDT_Number
               pop  word ptr [FS_File_NFDT]
               push word ptr ES:[DI]+FS_FDT_Next_FDT_Number+2
               pop  word ptr [FS_File_NFDT]+2

               mov ax, word ptr [FS_CurrentSection]
               mov dx, word ptr [FS_CurrentSection]+2
                ;  DS:SI = Logical dos drv desc. table address
                ;  DX:AX = the first sector number to be unlinked
                ;          (offset to FS beginning sector)
                ;  BX:CX = Sector count
                ;  ES = Sector buffer segment
               call proc_deallocate_fs_disk_sectors
                ;  DS:SI = Logical dos drv desc. table address
                ;  DX:AX = Free sectors
                ;  ES:BX = Sector buffer segment (BX=0)
                ;  stc -> error code in ax, dx = 0
                ;         BX, CX, DI will be modified
               jc short loc_dfss_compare_buffer_err_retn2

               mov ax, word ptr [FS_File_NFDT]
               mov dx, word ptr [FS_File_NFDT]+2
               
               mov cx, ax
               add cx, dx
               cmp cx, 1
              ;pushf
               mov cx, word ptr [FS_CS_Sectors]
               mov dx, word ptr [FS_CS_Sectors]+2 
              ;popf
               retn 

proc_delete_fs_section endp

proc_convert_dos_date_time_to_fs_format proc near
               ; 14/11/2010 BugFix
               ; 29/10/2010
               ; INPUT ->
               ;  ES:DI = FS FDT/DDT LMDT Address/Offset 
               ;  DX = Date in DOS dir entry format
               ;  AX = Time in DOS dir entry format 
               ; OUTPUT ->
               ;  ES:DI (LMDT address) is updated
               ;  ES,DI,DX,AX,CX,BX have same (input) values 
 
                push    bx
                push    cx
                push    ds

                push    es
                pop     ds

                mov     bx, ax ; Time                 
loc_cddt_to_fs_date:
        	mov     ax, dx                   ; Date
		mov     cl, 9
		shr     ax, cl
	       ;and     ax, 01111111b            ; Result = Year - 1980
		add     ax, 1980

		mov     cl, 100
		div     cl                       ; Q -> AL, R -> AH 
                mov     ch, ah ; 14/11/2010
                aam     ; convert binary value in AL to unpacked BCD in AH and AL
                mov     cl, 4
                shl     ah, cl
                add     ah, al
                mov     byte ptr [DI],ah         ; AH = Century (BCD)
                mov     al, ch
                aam               
               ;mov     cl, 4
                shl     ah, cl
                add     ah, al                    
                mov     byte ptr [DI]+1,ah       ; AH = Year (BCD)                
                                                 ; LMDT Year
		mov     ax, dx
		inc     cl
                ;mov     cl, 5
		shr     ax, cl                   ; shift right 5 times
		and     al, 00001111b            ; Month Mask
		aam   
                dec     cl
                ;mov    cl, 4
                shl     ah, cl
                add     ah, al                  
                mov     byte ptr [DI]+2,ah       ; LMDT Month  

		mov     al, dl                   
		and     al, 00011111b            ; Day Mask
                aam   
                ;mov    cl, 4
                shl     ah, cl
                add     ah, al                  
                mov     byte ptr [DI]+3,ah       ; LMDT Day

loc_cddt_to_fs_time:
		mov     al, bh                   ; ax = Time
                dec     cl                 
	        ;shr     al, 1
		;shr     al, 1
		;shr     al, 1                    ; al = Hours
                shr      al, cl
                aam
                inc     cl   
                ;mov    cl, 4
                shl     ah, cl
                add     ah, al                  
                mov     byte ptr [DI]+4,ah       ; LMDT Hour

		mov     ax, bx                   
	        inc     cl
                ;mov     cl, 5
		shr     ax, cl                   ; shift right 5 times
		and     al, 00111111b            ; Minute Mask
                aam
                dec     cl   
                ;mov    cl, 4
                shl     ah, cl
                add     ah, al                  
                mov     byte ptr [DI]+5,ah       ; LMDT Minute             

                mov     al, bl                   
		and     al, 00011111b            ; Count of 2 seconds, 0-29
		shl     al, 1                    ; Shift left for 0-58 seconds
                aam
                ;mov    cl, 4
                shl     ah, cl
                add     ah, al                  
                mov     byte ptr [DI]+6,ah       ; LMDT Second

                mov     ax, bx

                pop     ds
                pop     cx
                pop     bx
                                  
                retn  
                
proc_convert_dos_date_time_to_fs_format endp

proc_create_fs_file proc near
               ; 03/09/2011
               ; 07/08/2011 BugFix
               ; 01/08/2011
               ; 21/07/2011
               ; 19/03/2011 bugfix
               ; 05/12/2010 bugfix
               ; 20/11/2010   
               ; 15/11/2010
               ; 07/11/2010
               ; 06/11/2010
               ; Derivation from proc_make_fs_directory

               ; INPUT ->
               ; DX:AX -> File Size
               ; DS:SI -> Logical DOS Drive DT
               ; BX -> File name offset as ASCIIZ name

               ; OUTPUT ->
               ; DX:AX = New file's FDT (section header) address
               ; CX = Sector count of the section (except FDT)
               ; DS:SI = Logical Dos Drv Descr. Table Addr.
               ; DS:BX = offset create_fs_file_size
               ; stc -> error code in AL
               ; NOTE: Different file size returns 
               ;           if input file size > 01FFFC00h
               ; Only 1 section will be allocated for the file
               ; at 'create file' stage. So, maximum consequent
               ; free sectors will be allocated for file section
               ; even if required sector count of the section
               ; can be up to FFFEh (except FDT/header)!  
               ; 

               cmp byte ptr [SI][LD_FS_FSType], 0A1h 
               je short loc_FS_createfile_start_to_load_parent_directory

	       mov ax, 1Ah ; Unknown media type (non-DOS disk)
               xor dx, dx
               xor cx, cx 
               stc
loc_FS_createfile_load_parent_dir_section_retn: 
               retn

loc_FS_createfile_start_to_load_parent_directory:
               mov word ptr [create_fs_file_size], ax
               mov word ptr [create_fs_file_size]+2, dx
               mov word ptr [createfile_Name_Offset], bx

loc_FS_createfile_load_parent_directory_section_0:
               mov cx, word ptr [FS_DirEntryCount]
               mov dx, word ptr [FS_DirEntryCount]+2
               or dx, dx
  	       jnz short loc_FS_createfile_load_parent_dir_permission_denied
  
              ; TRDOS can not use FS directory entries beyond 2048
               cmp cx, 2048
               ja short loc_FS_createfile_load_parent_dir_permission_denied

loc_FS_createfile_reset_progress_sign:
; 21/07/2011
mov ax, word ptr [create_fs_file_size]+2
or ax, ax
jnz short loc_FS_createfile_activate_progress_sign
mov byte ptr [FSCF_PSActive], al
jmp short pass_FS_createfile_reset_progress_sign
loc_FS_createfile_activate_progress_sign:
mov ah, 0Fh
mov byte ptr [FSCF_PSActive], ah
;mov ah, 0Fh
int 10h
; xor bl, bl
mov Word ptr [FSCF_VideoPage], bx
mov ah, 03h
int 10h
mov word ptr [FSCF_CursorPos], dx
mov al, "/"
mov byte ptr [FSCF_PSign], al
pass_FS_createfile_reset_progress_sign:
;
               mov ax, word ptr [Current_Dir_FCluster]
               mov dx, word ptr [Current_Dir_FCluster]+2
               mov word ptr [FS_ParentDir_DDT], ax
               mov word ptr [FS_ParentDir_DDT]+2, dx
               jmp short loc_FS_createfile_load_parent_dir_section_1

loc_FS_createfile_load_parent_dir_permission_denied:
               mov ax, 05h ; Access denied
             ; mov bx, word ptr [createfile_Name_Offset]
               ; DX:CX -> FS Dir Entry count
               stc
               retn 

loc_FS_createfile_check_for_free_entry_loop_2:
               add si, 4
               jmp loc_FS_createfile_check_for_free_entry_next_1

loc_FS_createfile_check_for_free_entry_next_2:
               cmp dx, 0FFFFh
               jne short loc_FS_createfile_check_for_free_entry_loop_1
               cmp ax, 0FFFFh
               je loc_FS_createfile_save_new_dir_entry

loc_FS_createfile_check_for_free_entry_loop_1:  
               loop loc_FS_createfile_check_for_free_entry_loop_2

loc_FS_createfile_check_next_parent_directory_section:
               pop ds
               pop si
               mov ax, word ptr [FS_DDT_Next_Section]
	       mov dx, word ptr [FS_DDT_Next_Section]+2

               and ax, ax
               jnz short loc_FS_createfile_load_parent_dir_section_1_next
               and dx, dx
               jnz short loc_FS_createfile_load_parent_dir_section_1_next

               mov cx, word ptr [FS_DirEntryCount]
               cmp cx, 2048
               jnb short loc_FS_createfile_load_parent_dir_permission_denied   

               mov ax, word ptr [FS_DirBuffer_Section]
	       mov dx, word ptr [FS_DirBuffer_Section]+2

               xor cx, cx ; new directory directory section -> cx = 0

               call proc_add_new_fs_section
               jc loc_FS_createfile_load_directory_return

loc_FS_createfile_load_parent_dir_section_1:
               mov bx, word ptr [FS_Directory_Buffer]  
loc_FS_createfile_load_parent_dir_section_1_next:
               mov word ptr [FS_DirBuffer_Section], ax
	       mov word ptr [FS_DirBuffer_Section]+2, dx
               call proc_allocate_fs_section_buffer                
               jc loc_FS_createfile_load_directory_return

loc_FS_createfile_load_parent_dir_section_2:
               mov word ptr [FS_Directory_Buffer], bx
               mov word ptr [FS_DirBuff_SectorCount], cx
               mov word ptr [FS_DDT_Next_Section], ax
	       mov word ptr [FS_DDT_Next_Section]+2, dx

loc_FS_createfile_load_parent_dir_section_3:
               push si
               push ds
               xor si, si
               mov ds, bx
               mov ax, 128
               mul cx
               mov cx, ax ; count of FS dir entries in section

loc_FS_createfile_check_for_free_entry_next_1:
               mov ax, word ptr [SI]
               mov dx, word ptr [SI]+2
               or ax, ax
               jnz loc_FS_createfile_check_for_free_entry_next_2
               or dx, dx
               jnz loc_FS_createfile_check_for_free_entry_next_2

loc_FS_createfile_save_new_dir_entry:
               pop ds  
               mov word ptr [FS_Mkdir_EntryOffset], si
               pop si

loc_FS_createfile_get_first_free_section:
               mov ax, word ptr [create_fs_file_size]
               mov dx, word ptr [create_fs_file_size]+2
               cmp dx, 1FFh 
               jb short loc_FS_createfile_get_first_free_section_2
               je short loc_FS_createfile_get_first_free_section_0
               mov dx, 1FFh
               jmp short loc_FS_createfile_get_first_free_section_1
loc_FS_createfile_get_first_free_section_0: 
               cmp ax, 0FC00h  
               jna short loc_FS_createfile_get_first_free_section_2
loc_FS_createfile_get_first_free_section_1: 
               mov ax, 0FC00h
               mov word ptr [create_fs_file_size], ax
               mov word ptr [create_fs_file_size]+2, dx 
loc_FS_createfile_get_first_free_section_2:
               mov cx, 511
               add ax, cx
               adc dx, 0
               inc cx ; mov cx, 512
               call Rx_DOS_Div32
              ; 20/03/2011
              ; 19/03/2011 BugFix (section's sector count can not be 0)            
               or ax, ax
               jnz short loc_FS_createfile_get_first_free_section_3   

               mov ax, word ptr [SI][LD_FreeSectors]+2
               or ax, ax
               jnz short loc_FS_createfile_get_first_free_section_5
               mov ax, word ptr [SI][LD_FreeSectors]                                 
               cmp ax, 2
               jb short loc_FS_createfile_insufficient_disk_space_2
               mov cx, 2
               jmp short loc_FS_createfile_get_first_free_section_6
loc_FS_createfile_get_first_free_section_5:
               and ah, ah
               jz short loc_FS_createfile_get_first_free_section_7
               mov cx, 16 ; maximum 16 sectors
               jmp short loc_FS_createfile_get_first_free_section_8
loc_FS_createfile_get_first_free_section_7:
               mov cx, ax 
               shr cl, 1
	       shr cl, 1
               shr cl, 1
               shr cl, 1
               cmp cx, 2
               jnb short loc_FS_createfile_get_first_free_section_8
               mov cx, 2
               jmp short loc_FS_createfile_get_first_free_section_8
 
loc_FS_createfile_get_first_free_section_3:   
               mov cx, ax
loc_FS_createfile_get_first_free_section_8:
               inc cx ; add 1 sector for section header
loc_FS_createfile_get_first_free_section_6:
               mov bx, word ptr [Sector_Buffer]
               mov es, bx
               xor bx, bx
               call proc_get_first_free_section
               jnc short loc_FS_createfile_write_new_dir_entry
               cmp cx, 2
               jnb short loc_FS_createfile_get_first_free_section_4

               or cx, cx
               jnz short loc_FS_createfile_insufficient_disk_space_1
              ; 15/11/2010 
               or ax, ax
               jnz short loc_FS_createfile_error_retn
                        
loc_FS_createfile_insufficient_disk_space_1:
                mov ax, word ptr [SI][LD_FreeSectors]
loc_FS_createfile_insufficient_disk_space_2:
                mov dx, 27h ; MSDOS err => insufficient disk space
                xchg ax, dx
                mov bx, word ptr [createfile_Name_Offset]
                ; err retn: DX = Free sectors (LW)
                ; BX= Dir name offset
                ; DS:SI -> Dos drive description table address
                ; cx = Free consequent sectors
                ;
loc_FS_createfile_error_retn:
               ; 21/07/2011  
                stc
                pushf
                jmp loc_FS_createfile_retn1   

loc_FS_createfile_get_first_free_section_4:
               push dx
               push ax
               mov ax, 512
               mul cx
               mov word ptr [create_fs_file_size], ax
               mov word ptr [create_fs_file_size]+2, dx                
               pop ax
               pop dx    

loc_FS_createfile_write_new_dir_entry:
               mov word ptr [FS_Mkdir_New_DDT], ax
               mov word ptr [FS_Mkdir_New_DDT]+2, dx            
               mov word ptr [FS_Mkdir_New_DDT_scount], cx 

               push si
               push ds
               mov si, word ptr [FS_Directory_Buffer]
               push si
               mov si, word ptr [FS_Mkdir_EntryOffset]
               pop ds ; FS_Directory_Buffer
               mov word ptr [SI], ax
               mov word ptr [SI]+2, dx
               pop ds
               pop si 

loc_FS_createfile_initialize_file_section:
               mov bx, word ptr [Sector_Buffer] 
               mov es, bx
               xor di, di ; 01/08/2011
               mov cx, 256
               xor ax, ax ; 0  
               rep stosw  ; mov word ptr ES:[DI], 0  

              ; 07/08/2011
               mov byte ptr [FS_BuffValidData], al ; 0

               mov ax, word ptr [FS_Mkdir_New_DDT]
              ;mov dx, word ptr [FS_Mkdir_New_DDT]+2            

               mov word ptr [FS_CurrentSection], ax
               mov word ptr [FS_CurrentSection]+2, dx

               mov cx, word ptr [FS_Mkdir_New_DDT_scount]
                            
               add ax, word ptr [SI][LD_FS_BeginSector]
               adc dx, word ptr [SI][LD_FS_BeginSector]+2

loc_FS_createfile_initialize_file_section_write:
; 07/08/2011
; 01/08/2011
cmp byte ptr [FSCF_PSActive], 0
jna short pass_FS_createfile_print_progress_sign_ok

cmp cl, 15 ; 1 progress sign printing per 16 sectors 
jne short pass_FS_createfile_print_progress_sign_ok
;

; 01/08/2011
loc_FS_createfile_print_progress_sign:
; 21/07/2011 printing create file in progress sign

; 07/08/2011
push dx
push ax

; 01/08/2011
mov dx, word ptr [FSCF_CursorPos]
mov bx, word ptr [FSCF_VideoPage]
mov ah, 2
int 10h

mov al, byte ptr [FSCF_PSign]
cmp al, "|"
je short loc_FS_createfile_pps_1
cmp al, "/"
je short loc_FS_createfile_pps_2
cmp al, "-"
je short loc_FS_createfile_pps_3
mov al, "|"
jmp short loc_FS_createfile_pps_4
loc_FS_createfile_pps_1:
mov al, "/"
jmp short loc_FS_createfile_pps_4
loc_FS_createfile_pps_2:
mov al, "-"
jmp short loc_FS_createfile_pps_4
loc_FS_createfile_pps_3:
mov al, "\"
loc_FS_createfile_pps_4:
mov byte ptr [FSCF_PSign], al
mov ah, 0Eh
int 10h

; 07/08/2011
pop ax
pop dx

pass_FS_createfile_print_progress_sign_ok:
              ; 01/08/2011
               xor bx, bx ; mov bx, 0 
              ; 05/12/2010
               push cx
               mov cx, 1     
               call proc_disk_write
               pop cx
               jc loc_FS_createfile_load_directory_return
               ; DX:AX = Next disk adress (+1)
               loop loc_FS_createfile_initialize_file_section_write

               mov byte ptr [FS_BuffValidData], 1

               dec word ptr [FS_Mkdir_New_DDT_scount] ; except header 
                
loc_FS_createfile_write_parent_dir_updated_sector:
               mov bx, word ptr [FS_Directory_Buffer]
               mov es, bx

               mov ax, word ptr [FS_Mkdir_EntryOffset]
               mov bx, ax
               xor dx, dx
               mov cx, 512 
               div cx
               sub bx, dx ; DX= remain/offset
               xor dx, dx               

               ; BX = Current FS Dir buffer offset (ES:BX)
               ; AX = Current FS dir buffer sector
               ; DX = 0

               add ax, word ptr [FS_DirBuffer_Section]
	       adc dx, word ptr [FS_DirBuffer_Section]+2
               add ax, word ptr [SI][LD_FS_BeginSector]
               adc dx, word ptr [SI][LD_FS_BeginSector]+2

               mov cx, 1 
               add ax, cx ;1
               adc dx, 0

               call proc_disk_write
               jc loc_FS_createfile_load_directory_return

loc_FS_createfile_save_new_dir_section:
               mov bx, word ptr [Sector_Buffer]
               mov es, bx

               mov ax, word ptr [FS_ParentDir_DDT]
               mov dx, word ptr [FS_ParentDir_DDT]+2

               xor bl, bl
               mov byte ptr [FS_BuffValidData], bl
               mov word ptr [FS_CurrentSection], ax
               mov word ptr [FS_CurrentSection]+2, dx

               add ax, word ptr [SI][LD_FS_BeginSector]
               adc dx, word ptr [SI][LD_FS_BeginSector]+2

               xor bh, bh
              ;xor cx, cx 
              ;inc cl      
               call proc_disk_read
               jc loc_FS_createfile_load_directory_return

loc_FS_createfile_check_parent_ddt_1:
                 mov byte ptr [FS_BuffValidData], 1

                 mov cx, word ptr ES:[BX]+1
                 cmp cx, 'TD'
                 je short loc_FS_createfile_check_parent_ddt_2

loc_FS_createfile_invalid_parent_ddt:
                 xor dx, dx
                 mov al, 0Dh ; Invalid data !
                 xor ah, ah
                 stc
                 jmp loc_FS_createfile_load_directory_return

loc_FS_createfile_check_parent_ddt_2:
                 mov cl, byte ptr ES:[BX]
                 cmp cl, 'D'
                 je short loc_FS_createfile_get_parent_dir_serial
                 cmp cl, 'R'
                 jne short loc_FS_createfile_invalid_parent_ddt 
loc_FS_createfile_get_parent_dir_serial:
                 mov ax, word ptr ES:[BX]+FS_DDT_Directory_Number
                 mov dx, word ptr ES:[BX]+FS_DDT_Directory_Number+2

                 cmp ax, word ptr [FS_ParentDir_DDT]                  
                 jne short loc_FS_createfile_invalid_parent_ddt
                 cmp dx, word ptr [FS_ParentDir_DDT]+2                  
                 jne short loc_FS_createfile_invalid_parent_ddt

                ; 07/11/2010
                 mov ax, word ptr ES:[BX]+FS_DDT_Directory_Serial
                 mov dx, word ptr ES:[BX]+FS_DDT_Directory_Serial+2
                 mov word ptr [FS_ParentDir_Serial], ax
                 mov word ptr [FS_ParentDir_Serial]+2, dx

loc_FS_createfile_set_parent_dir_lm_date_time:
                 mov ah, 02h                        ; Return Current Time
                 int 1Ah
                 xchg ch,cl
                 mov word ptr [DelFile_fs_ddt_lm_hour], cx
                 xchg dh,dl
                 mov word ptr [DelFile_fs_ddt_lm_second], dx

                 mov word ptr ES:[BX]+fs_ddt_lm_hour, cx
                 mov word ptr ES:[BX]+fs_ddt_lm_second, dx
                 
                 mov ah, 04h                        ; Return Current Date
                 int 1Ah
                 xchg ch,cl
                 mov word ptr [DelFile_fs_ddt_lm_year], cx
                 xchg dh,dl
                 mov word ptr [DelFile_fs_ddt_lm_month], dx
 
                 mov word ptr ES:[BX]+fs_ddt_lm_year, cx
                 mov word ptr ES:[BX]+fs_ddt_lm_month, dx

                 mov ax, word ptr [FS_ParentDir_DDT]
                 mov dx, word ptr [FS_ParentDir_DDT]+2

                ;xor bl, bl
                 mov byte ptr [FS_BuffValidData], bl
                 mov word ptr [FS_CurrentSection], ax
                 mov word ptr [FS_CurrentSection]+2, dx

                 add ax, word ptr [SI][LD_FS_BeginSector]
                 adc dx, word ptr [SI][LD_FS_BeginSector]+2

                 xor cx, cx 
                 ;inc cx
                 inc cl      
                 call proc_disk_write
                 jc loc_FS_createfile_load_directory_return

                ;mov byte ptr [FS_BuffValidData], 1

loc_FS_createfile_set_new_file_fdt:
               ;mov bx, word ptr [Sector_Buffer]
               ;mov es, bx

               mov ax, word ptr [FS_Mkdir_New_DDT]
               mov dx, word ptr [FS_Mkdir_New_DDT]+2

             ;xor bx, bx
              ; mov byte ptr [FS_BuffValidData], bl
               mov word ptr [FS_CurrentSection], ax
               mov word ptr [FS_CurrentSection]+2, dx

               add ax, word ptr [SI][LD_FS_BeginSector]
               adc dx, word ptr [SI][LD_FS_BeginSector]+2

              ;mov cx, 1 
               call proc_disk_read
               jc loc_FS_createfile_load_directory_return

               mov byte ptr [FS_BuffValidData], 1

               push si
               mov si, word ptr [createfile_Name_Offset]
               mov di, FS_FDT_File_Name ; 64
               mov cx, 12 ;Max. length of dos file name, it is 64 for FS
loc_FS_createfile_move_file_name:
               lodsb ;  mov al, byte ptr [SI]
               or al, al
               jz short pass_FS_createfile_move_file_name
               stosb
               loop loc_FS_createfile_move_file_name
              ;xor al, al

pass_FS_createfile_move_file_name:
              ;mov byte ptr ES:[DI], al
               pop si

               xor di, di
               mov byte ptr ES:[DI], 'F'
               inc di 
               mov word ptr ES:[DI], 'TD'
              ;mov di, 3
              ; mov byte ptr ES:[DI], 0

               mov di, FS_FDT_Sector_Size ; 4
               mov word ptr ES:[DI], 512

               mov ax, word ptr [FS_Mkdir_New_DDT]
               mov dx, word ptr [FS_Mkdir_New_DDT]+2

               mov di, FS_FDT_File_Number ; 8

               mov word ptr ES:[DI], ax
               mov word ptr ES:[DI]+2, dx   
               
               mov cx, word ptr [FS_Mkdir_New_DDT_scount] ; except header

               mov di, FS_FDT_Sector_Count ; 16
               mov word ptr ES:[DI], cx
              ;mov word ptr ES:[DI]+2, 0

               mov ax, word ptr [FS_ParentDir_DDT]
               mov dx, word ptr [FS_ParentDir_DDT]+2 
               mov di, FS_FDT_Parent_Dir_Number ; 20
               mov word ptr ES:[DI], ax
               mov word ptr ES:[DI]+2, dx

               mov ax, word ptr [FS_ParentDir_Serial]
               mov dx, word ptr [FS_ParentDir_Serial]+2 
               mov di, FS_FDT_Parent_Dir_Serial ; 24
               mov word ptr ES:[DI], ax
               mov word ptr ES:[DI]+2, dx

               mov ax, word ptr [create_fs_file_size]
               mov dx, word ptr [create_fs_file_size]+2

               mov di, FS_FDT_File_Size ; 28
               mov word ptr ES:[DI], ax
               mov word ptr ES:[DI]+2, dx  ; set file size     

               mov di, FS_FDT_Attributes ; 37
               mov byte ptr ES:[DI], 01101101b ; normal file

               mov di, FS_FDT_Country ; 46
               mov byte ptr ES:[DI], 90 ; default configuration (trdos & singlix)
                                        ; (singlix fs prototype: english-turkish)

               mov ax, word ptr [DelFile_fs_ddt_lm_year]
               mov dx, word ptr [DelFile_fs_ddt_lm_month]
               mov di, fs_ddt_cr_year ; 48
               mov word ptr ES:[DI], ax
               mov di, fs_ddt_cr_month ; 50 (Day = offset 51)
               mov word ptr ES:[DI], dx

               mov ax, word ptr [DelFile_fs_ddt_lm_hour]
               mov dx, word ptr [DelFile_fs_ddt_lm_second]
               mov di, fs_ddt_cr_hour ; 52 (Minute = offset 53)
               mov word ptr ES:[DI], ax
               mov di, fs_ddt_cr_second ;54
               mov word ptr ES:[DI], dx

               mov ah, 02h                        ; Return Current Time
               int 1Ah
               xchg ch,cl
               xchg dh,dl
               mov di, fs_ddt_lm_hour ; 60 (Minute = offset 61)
               mov word ptr ES:[DI], cx
               mov di, fs_ddt_lm_second ;62
               mov word ptr ES:[DI], dx
                 
               mov ah, 04h                        ; Return Current Date
               int 1Ah
               xchg ch,cl
               xchg dh,dl
               mov di, fs_ddt_lm_year ; 56
               mov word ptr ES:[DI], cx
               mov di, fs_ddt_lm_month ; 58 (Day = offset 59)
               mov word ptr ES:[DI], dx

              ;mov ax, word ptr [FS_Mkdir_New_DDT]
              ;mov dx, word ptr [FS_Mkdir_New_DDT]+2

               mov ax, word ptr [FS_CurrentSection]
               mov dx, word ptr [FS_CurrentSection]+2

               add ax, word ptr [SI][LD_FS_BeginSector]
               adc dx, word ptr [SI][LD_FS_BeginSector]+2

               ; 20/11/2010
               mov byte ptr [DirBuff_ValidData], 0 ; RESET

               mov cx, 1      
               call proc_disk_write
               jc short loc_FS_createfile_load_directory_return

              ;mov byte ptr [FS_BuffValidData], 1

               mov cx,  word ptr [FS_Mkdir_New_DDT_scount] ; except header
              ;mov bx, word ptr [Sector_Buffer]
              ;mov es, bx
               mov bx, 0F00Fh  ;File header (0Fh) and file data (F0h)
              ;mov ax, word ptr [FS_CurrentSection]
              ;mov dx, word ptr [FS_CurrentSection]+2
               mov ax, word ptr [FS_Mkdir_New_DDT]
               mov dx, word ptr [FS_Mkdir_New_DDT]+2 

               call proc_allocate_fs_disk_sectors
               jc short loc_FS_createfile_load_directory_return

loc_FS_createfile_set_fs_buffer_validdata_for_MAT:
               push word ptr [SI][LD_FS_MATLocation]+2
               push word ptr [SI][LD_FS_MATLocation]

               pop word ptr [FS_CurrentSection]
               pop word ptr [FS_CurrentSection]+2

              ;inc byte ptr [FS_BuffValidData] ; 1

               push ds
               pop es

               mov bx, offset create_fs_file_size
               mov cx, word ptr [FS_Mkdir_New_DDT_scount] ; except header
                  
              ; pushf
              ; jmp short loc_FS_createfile_retn1
    
loc_FS_createfile_load_directory_return:
              ; to get more free memory at return
               pushf
               push bx
               mov bx, word ptr [FS_Directory_Buffer]
               or bx, bx
               jz short loc_FS_createfile_load_dir_return_popbx_popf
               push cx
               push dx
               push ax
               mov ax, 0106h ;Consequently for 06h allocation
                       ; 06h -> FS directory/section buffer
               call proc_deallocate_memory 
               mov word ptr [FS_Directory_Buffer], 0
               pop ax
               pop dx
               pop cx  
loc_FS_createfile_load_dir_return_popbx_popf:
               pop bx

loc_FS_createfile_retn1:
;21/07/2011
cmp byte ptr [FSCF_PSActive], 0
jna short loc_FS_createfile_retn2

push dx
push ax
; Set cursor position
mov dx, word ptr [FSCF_CursorPos]
mov bx, word ptr [FSCF_VideoPage]
mov ah, 2
int 10h
;mov al, 20h ; space
;mov ah, 0Eh
;int 10h
pop ax
pop dx

loc_FS_createfile_retn2:
              ; 21/07/2011 
               popf
               retn

; 03/09/2011
create_fs_file_size: dd 0 

proc_create_fs_file endp

proc_rename_fs_file_or_directory proc near
               ; 20/11/2010  
               ; 19/11/2010
               ; INPUT -> (Current Directory)
               ;  DX:AX = FDT or DDT number of file or directory
               ;  DS:SI = New file (or directory) name (no path).
               ;          (ASCIZZ string)  
               ; OUTPUT -> 
               ;  clc (cf=0) -> successfull
               ;  stc (cf=1) -> error code in AX
               ;  ES, AX, BX, CX, DX, SI, DI will be changed
               
               push ds
               pop es
               push cs
               pop ds

               push es
               push si

               mov bh, byte ptr [Current_Drv]
               xor bl, bl
               mov si, offset Logical_DosDisks
               add si, bx

               cmp byte ptr [SI][LD_FS_FSType], 0A1h 
               je short loc_FS_rename_load_file_or_directory

	       mov ax, 1Ah ; Unknown media type (non-DOS disk)
               xor dx, dx
               xor cx, cx
 
loc_FS_rename_file_or_directory_stc_retn:
               stc
loc_FS_rename_file_or_directory_retn:
               pop di
               pop es 
               retn

loc_FS_rename_load_file_or_directory:
               mov bx, word ptr [Sector_Buffer]
               mov es, bx

               xor bx, bx
               mov byte ptr [FS_BuffValidData], bl
               mov word ptr [FS_CurrentSection], ax
               mov word ptr [FS_CurrentSection]+2, dx

               add ax, word ptr [SI][LD_FS_BeginSector]
               adc dx, word ptr [SI][LD_FS_BeginSector]+2

               xor cx, cx 
               inc cl      
               call proc_disk_read
               jc short loc_FS_rename_file_or_directory_retn

loc_FS_rename_check_fdt_ddt_header:
               mov byte ptr [FS_BuffValidData], 1
               mov ax, word ptr [FS_CurrentSection]
               mov dx, word ptr [FS_CurrentSection]+2
               cmp word ptr ES:[BX]+1, 'TD'
               jne short loc_delete_fs_file_directory_not_found
               cmp byte ptr ES:[BX], 'F'
               je short loc_FS_rename_check_fdt_ddt_number
loc_FS_rename_check_ddt_header:
               cmp byte ptr ES:[BX], 'D'
               jne short loc_delete_fs_file_directory_not_found
loc_FS_rename_check_fdt_ddt_number:                
              ;add bx, 8  
              ;add bl, 8
               mov bl, FS_FDT_File_Number ; 8
               cmp ax, word ptr ES:[BX]
               jne short loc_delete_fs_file_directory_not_found
               cmp dx, word ptr ES:[BX]+2
               je short loc_FS_rename_file_or_dir_change_name

loc_delete_fs_file_directory_not_found:
               mov dx, ax
               mov ax, 02h ; File Not Found error
               jmp short loc_FS_rename_file_or_directory_stc_retn

loc_FS_rename_file_or_dir_change_name:
               mov di, si        
               pop si
               pop ds
               push di 
               mov cx, 64
               mov di, cx ; 64
loc_FS_rename_file_or_dir_change_name_loop: 
               lodsb
               stosb
               or al, al
               jz short loc_FS_rename_write_FDT_or_DDT
               loop loc_FS_rename_file_or_dir_change_name_loop

loc_FS_rename_write_FDT_or_DDT:
               pop si
               push cs
               pop ds

              ;mov bx, FS_FDT_Parent_Dir_Number
               mov bl, FS_FDT_Parent_Dir_Number  ;20 

               mov ax, word ptr ES:[BX]
               mov dx, word ptr ES:[BX]+2                

               mov word ptr [Delete_FS_ParentDir], ax
               mov word ptr [Delete_FS_ParentDir]+2, dx

              ;xor bx, bx
               xor bl, bl

               mov ax, word ptr [FS_CurrentSection]
               mov dx, word ptr [FS_CurrentSection]+2

               add ax, word ptr [SI][LD_FS_BeginSector]
               adc dx, word ptr [SI][LD_FS_BeginSector]+2

              ;20/11/2010 
               mov byte ptr [DirBuff_ValidData], bl ; 0 = RESET

              ;xor ch, ch
               mov cl, 1      
               call proc_disk_write
               jc short loc_FS_rename_file_load_parent_dir_stc_retn

loc_FS_rename_file_or_dir_load_parent_dir:
               mov ax, word ptr [Delete_FS_ParentDir]
               mov dx, word ptr [Delete_FS_ParentDir]+2

               mov byte ptr [FS_BuffValidData], bl ;0

               mov word ptr [FS_CurrentSection], ax
               mov word ptr [FS_CurrentSection]+2, dx

               add ax, word ptr [SI][LD_FS_BeginSector]
               adc dx, word ptr [SI][LD_FS_BeginSector]+2

              ;xor bx, bx
              ;xor cx, cx 
              ;inc cl      
               call proc_disk_read
               jnc short loc_FS_rename_file_check_parent_dir

loc_FS_rename_file_load_parent_dir_retn:
loc_FS_rename_parent_dir_not_found:
               xor ax, ax
               xor dx, dx
               xor cx, cx 
               
loc_FS_rename_file_load_parent_dir_stc_retn:
               retn

loc_FS_rename_file_check_parent_dir:
               mov byte ptr [FS_BuffValidData], 1
               mov ax, word ptr [FS_CurrentSection]
               mov dx, word ptr [FS_CurrentSection]+2
               cmp word ptr ES:[BX]+1, 'TD'
               jne short loc_FS_rename_parent_dir_not_found
               cmp byte ptr ES:[BX], 'D'
               je short loc_FS_rename_dir_parent_dir_found
               cmp byte ptr ES:[BX], 'R'
               jne short loc_FS_rename_parent_dir_not_found
loc_FS_rename_dir_parent_dir_found:
               cmp ax, word ptr ES:[BX]+8 ;Dir Number low
               jne short loc_FS_rename_parent_dir_not_found
               cmp dx, word ptr ES:[BX]+10 ;Dir Number high
               jne short loc_FS_rename_parent_dir_not_found

loc_FS_rename_dir_parent_dir_lm_d_t:
               mov ah, 02h                        ; Return Current Time
               int 1Ah
               xchg ch,cl
               mov word ptr ES:[BX]+fs_ddt_lm_hour, cx
               xchg dh,dl
               mov word ptr ES:[BX]+fs_ddt_lm_second, dx
                 
               mov ah, 04h                        ; Return Current Date
               int 1Ah
               xchg ch,cl
               mov word ptr ES:[BX]+fs_ddt_lm_year, cx
               xchg dh,dl
               mov word ptr ES:[BX]+fs_ddt_lm_month, dx

loc_FS_rename_dir_update_parent_dir_lm_d_t:
               mov ax, word ptr [Delete_FS_ParentDir]
               mov dx, word ptr [Delete_FS_ParentDir]+2
	       add ax, word ptr [SI][LD_FS_BeginSector]
               adc dx, word ptr [SI][LD_FS_BeginSector]+2
               mov cx, 1
               call proc_disk_write
               jmp short loc_FS_rename_file_load_parent_dir_retn

proc_rename_fs_file_or_directory endp


proc_delete_fs_directory_entry proc near
               ; 12/04/2011 BugFix
               ; 11/04/2011  
               ; 10/04/2011 Directory entry deleting code is moved here
               ;            from proc_delete_fs_file procedure
               ;            for rename file compatibility  
               ; 20/11/2010
               ; 03/07/2010 
               ; 26/06/2010
               ; 20/06/2010 "push ds, pop es" modification
               ; 22/05/2010
               ; 16-17-19/05/2010
               ; 09/05/2010 BugFix (xor bh, bh -> xor bl, bl)
               ; 02/05/2010, 01/05/2010
               ; 18/04/2010
               ; INPUT -> 
               ; DS:SI= Dos Drive Description Table Address
               ; ES:DI= Directory Entry offset (in Compatibility Buffer)
               ;    BL = 'F' file, BL = 'D' directory, BL = 0 -> file
               ; OUTPUT -> 
               ;    DS:SI= Dos Drive Description Table Address
               ;    DX:AX= FDT Address
               ;    ES:DI= Directory Entry offset (in Compatibility Buffer)
               ;    BL = 'F' or BL ='D'  
               ;    BH <> 0 -> LMDT write error (BH = FFh)
               ;  stc-> error code in AL

               cmp bl, 'F'
               je short loc_delete_fs_direntry_file2
               cmp bl, 'D'
               je short loc_delete_fs_direntry_directory
               or bl, bl
               jz short loc_delete_fs_direntry_file1
               
               mov ax, 05h  ; Access denied
               stc
               retn 

              ; 12/04/2011
loc_delete_fs_direntry_permission_denied_stc:
               stc
loc_delete_fs_direntry_permission_denied:
               mov ah, byte ptr ES:[DI]+DirEntry_Attr
               mov al, 05h ; Access denied error, (Permission denied !)
               retn
                      
loc_delete_fs_direntry_file1: 
               mov bl, 'F'
loc_delete_fs_direntry_file2:
loc_delete_fs_direntry_directory:
               mov byte ptr [FS_DirEntry_Type], bl  
loc_delete_fs_direntry_check_fs_type:
              ; 12/04/2011
               cmp word ptr ES:[DI]+DirEntry_NTRes, 01A1h
               jb short loc_delete_fs_direntry_permission_denied

               mov bl, byte ptr ES:[DI]+DirEntry_Attr  
               and bl, 07h ; Attributes
               jnz short loc_delete_fs_direntry_permission_denied_stc

               mov ax, word ptr ES:[DI]+26 ; First Cluster Low Word
               mov dx, word ptr ES:[DI]+20 ; First Cluster High Word

               mov word ptr [Delete_FS_DCB_Segment], es
             ; mov word ptr [Delete_FS_DCB_Offset], di

  	       mov word ptr [Delete_FS_FNumber], ax
	       mov word ptr [Delete_FS_FNumber]+2, dx

loc_delete_fs_direntry_load_fdt:
               mov bx, word ptr [Sector_Buffer]
               mov es, bx

              ; 01/05/2010 (FS_Sector_Buffer control parameters)
               xor bx, bx
               mov byte ptr [FS_BuffValidData], bl
               mov word ptr [FS_CurrentSection], ax
               mov word ptr [FS_CurrentSection]+2, dx
              ;

  	       push ax
               push dx    

               add ax, word ptr [SI][LD_FS_BeginSector]
               adc dx, word ptr [SI][LD_FS_BeginSector]+2

               xor cx, cx 
              ;inc cx
               inc cl      
               call proc_disk_read
               pop dx
               jnc short loc_delete_fs_direntry_check_fdt
               pop dx
              ; 11/04/2011   
               retn

loc_delete_fs_direntry_check_fdt:
               pop ax
              ; 01/05/2010
               mov byte ptr [FS_BuffValidData], 1
              ;
               mov cl, byte ptr [FS_DirEntry_Type]
               cmp byte ptr ES:[BX], cl ; 'F' or 'D'
               jne short loc_delete_fs_direntry_not_found
               inc bx ; 1     
               cmp byte ptr ES:[BX], 'D'
               jne short loc_delete_fs_direntry_not_found
               inc bx ; 2
               cmp byte ptr ES:[BX], 'T'
               jne short loc_delete_fs_direntry_not_found
              ;add bx, 6 ; bx = 8
               add bl, 6
               cmp ax, word ptr ES:[BX]
               jne short loc_delete_fs_direntry_not_found
 	       cmp dx, word ptr ES:[BX]+2
              ; 11/04/2010
               je short loc_delete_fs_direntry_load_directory

              ; 22/05/2010
loc_delete_fs_direntry_not_found:
               mov dx, ax
               mov ax, 02h ; File Not Found error
               jmp short loc_delete_fs_direntry_path_not_found

loc_delete_fs_direntry_load_directory:
              ;mov bx, FS_FDT_Parent_Dir_Number
               mov bl, FS_FDT_Parent_Dir_Number
               mov ax, word ptr ES:[BX]
               mov dx, word ptr ES:[BX]+2

              ; 01/05/2010 (FS_Sector_Buffer control parameters)
              ;mov byte ptr [FS_BuffValidData], 0
               mov word ptr [FS_CurrentSection], ax
               mov word ptr [FS_CurrentSection]+2, dx
              ;

              ; 01/05/2010

  	       push ax
               push dx    

               add ax, word ptr [SI][LD_FS_BeginSector]
               adc dx, word ptr [SI][LD_FS_BeginSector]+2

              ;xor bx, bx
              ; 09/05/2010 BugFix ('xor bh, bh' was wrong here)
               xor bl, bl
              ;  
               xor cx, cx 
              ;inc cx
               inc cl      
               call proc_disk_read
               pop dx
               jnc short loc_delete_fs_check_parent_dir

              ; 11/04/2011
               pop dx
               retn 

loc_delete_fs_parent_dir_not_found:
              ; 22/05/2010
              ; 01/05/2010
               mov dx, ax
               mov ax, 03h

loc_delete_fs_direntry_path_not_found:
               mov bx, word ptr [Sector_Buffer]
               xor cx, cx
               stc
loc_delete_fs_direntry_return:
               retn

loc_delete_fs_check_parent_dir:
              ; 01/05/2010 (FS_Sector_Buffer control parameters) 
               mov byte ptr [FS_BuffValidData], 1
              ;
               pop ax
               cmp word ptr ES:[BX]+1, 'TD'
               jne short loc_delete_fs_parent_dir_not_found
               cmp byte ptr ES:[BX], 'D'
               je short loc_delete_fs_parent_dir_found
               cmp byte ptr ES:[BX], 'R'
               jne short loc_delete_fs_parent_dir_not_found
loc_delete_fs_parent_dir_found:
               cmp ax, word ptr ES:[BX]+8 ;Dir Number low
               jne short loc_delete_fs_parent_dir_not_found
               cmp dx, word ptr ES:[BX]+10 ;Dir Number high
               jne short loc_delete_fs_parent_dir_not_found

              ; 19/05/2010
               mov word ptr [Delete_FS_ParentDir], ax
               mov word ptr [Delete_FS_ParentDir]+2, dx
             
  	       mov ax, word ptr [Delete_FS_FNumber]
	       mov dx, word ptr [Delete_FS_FNumber]+2
             
              ; 03/07/2010
              ; Entry offset (CX) cancelled 
              ; (search from beginning, by using only file no)
             
              ; 01/05/2010 (FS_Sector_Buffer control parameters) 
                mov byte ptr [FS_BuffValidData], 0
              ; BECAUSE:
              ; Sub procedure will change sector buffer content
              ; but it will not set buffer control parameters !   
              ;

               call proc_find_fs_dir_entry_with_number
              ; 20/06/2010
              ;jc short loc_delete_fs_direntry_push_ds_pop_es_return
               jc short loc_delete_fs_direntry_return

loc_delete_fs_direntry_put_FFFFFFFFh:
               ; 16/05/2010
                ; DX:AX = Directory Data Sector
                ; ES:DI = FS Directory Entry Offset
                ; BX:CX = File Number
                ; DS:SI = Logical Dos Drive Decr. Table Address

               cmp cx, word ptr [Delete_FS_FNumber]
               jne loc_delete_fs_direntry_not_found
	       cmp bx, word ptr [Delete_FS_FNumber]+2
               jne loc_delete_fs_direntry_not_found
            
               xor cx, cx
               dec cx ;  mov cx, 0FFFFh
              ;mov bx, cx
               mov word ptr ES:[DI], cx
              ;mov word ptr ES:[DI]+2, bx  
               mov word ptr ES:[DI]+2, cx
               inc cx ; xor cx, cx
               mov bx, cx ; xor bx, bx
               inc cx ; mov cx, 1
               
               add ax, word ptr [SI][LD_FS_BeginSector]
               adc dx, word ptr [SI][LD_FS_BeginSector]+2 

              ; ES:BX = Directory (Data) Buffer
             
               call proc_disk_write
              ; 20/06/2010
              ;jc short loc_delete_fs_direntry_push_ds_pop_es_return
               jc short loc_delete_fs_direntry_return

             ; 11/04/2011 
loc_delete_fs_direntry_put_deleted_sign:
              push es
              mov di, word ptr [Delete_FS_DCB_Offset]
              mov es, word ptr [Delete_FS_DCB_Segment]
              mov byte ptr ES:[DI], 0E5h
              pop es

loc_delete_fs_direntry_parent_dir_lm_d_t:
             ; 19/05/2010
               mov ah, 02h                        ; Return Current Time
               int 1Ah
               xchg ch,cl
               mov word ptr [DelFile_fs_ddt_lm_hour], cx
               xchg dh,dl
               mov word ptr [DelFile_fs_ddt_lm_second], dx
                 
               mov ah, 04h                        ; Return Current Date
               int 1Ah
               xchg ch,cl
               mov word ptr [DelFile_fs_ddt_lm_year], cx
               xchg dh,dl
               mov word ptr [DelFile_fs_ddt_lm_month], dx

loc_delete_fs_direntry_update_parent_dir_date_time:
             ; 11/04/2011 
               mov byte ptr [FS_LMDT_Error], 0FFh
             ; 19/05/2010
               mov ax, word ptr [Delete_FS_ParentDir]
               mov dx, word ptr [Delete_FS_ParentDir]+2
	       add ax, word ptr [SI][LD_FS_BeginSector]
               adc dx, word ptr [SI][LD_FS_BeginSector]+2
               mov cx, 1
               call proc_disk_read
              ; 26/06/2010
               jc short loc_del_fs_file_pass_upd_parent_dir_d_t

               mov ax, word ptr [DelFile_fs_ddt_lm_hour]
               mov dx, word ptr [DelFile_fs_ddt_lm_second]
                 
               mov word ptr ES:[BX]+fs_ddt_lm_hour, ax
               mov word ptr ES:[BX]+fs_ddt_lm_second, dx

               mov ax, word ptr [DelFile_fs_ddt_lm_year]
               mov dx, word ptr [DelFile_fs_ddt_lm_month]

               mov word ptr ES:[BX]+fs_ddt_lm_year, ax
               mov word ptr ES:[BX]+fs_ddt_lm_month, dx

               mov ax, word ptr [Delete_FS_ParentDir]
               mov dx, word ptr [Delete_FS_ParentDir]+2
               push dx
               push ax
	       add ax, word ptr [SI][LD_FS_BeginSector]
               adc dx, word ptr [SI][LD_FS_BeginSector]+2
              ;mov cx, 1               
               call proc_disk_write
               pop ax
               pop dx
              ; 20/06/2010
               mov bx, es 
               jc short loc_del_fs_file_pass_upd_parent_dir_d_t 
                 
               mov byte ptr [FS_BuffValidData], 1
               mov word ptr [FS_CurrentSection], ax
               mov word ptr [FS_CurrentSection]+2, dx

              ; 11/04/2011 
               inc byte ptr [FS_LMDT_Error] ; 0
            
loc_del_fs_file_pass_upd_parent_dir_d_t:
             ; 10/04/2011 
               mov es, word ptr [Delete_FS_DCB_Segment]
             ; mov di, word ptr [Delete_FS_DCB_Offset]
             ; 26/06/2010  
   	       mov ax, word ptr [Delete_FS_FNumber]
	       mov dx, word ptr [Delete_FS_FNumber]+2
               xor cx, cx
              ;DS:SI = Logical DOS Drv Table
             ; 11/04/2011
             ; mov bl, byte ptr [FS_DirEntry_Type]
             ; mov bh, byte ptr [FS_LMDT_Error]
               mov bx, word ptr [FS_DirEntry_Type]   
             ; 20/06/2010 
               retn

Delete_FS_FNumber:
               dd 0
; 26/06/2010 -> rmdir only
Delete_FS_DCB_Offset:
               dw 0
;09/05/2010
Delete_FS_DCB_Segment:
               dw 0
; 16/05/2010
Delete_FS_FNextSection:
                dd 0
Delete_FS_FSCount:
                dd 0
Delete_FS_FSCount_Total:
                dd 0
; 19/05/2010
Delete_FS_ParentDir: dd 0
; 19/05/2010
DelFile_fs_ddt_lm_year: dw 0
DelFile_fs_ddt_lm_month: dw 0
DelFile_fs_ddt_lm_hour: dw 0
DelFile_fs_ddt_lm_second: dw 0
; 10/04/2011
FS_DirEntry_Type: db 0
; 11/04/2011
FS_LMDT_Error: db 0

proc_delete_fs_directory_entry endp                
 

PROC_GECICIKOD proc near
;pushf
push si
;push ax
;push bx
;push dx
;push cx

gecicikod:
               mov ax, cx
               push ax
               call proc_hex
               mov word ptr [Burasi3], ax 
               pop ax
               xchg ah, al
               call proc_hex
	       mov word ptr [Burasi2], ax 
               mov ax, bx
               push ax
               call proc_hex
	       mov word ptr [Burasi1], ax 
               pop ax
               xchg ah, al
               call proc_hex 
               mov word ptr [Burasi0], ax 
               mov si, offset Burasi
               call proc_printmsg
               
;pop cx
;pop dx
;pop bx
;pop ax
pop si
;popf

retn

Burasi: db 'Deleting '
Burasi4: db 'FS File Number : '
Burasi0: dw 3030h
Burasi1: dw 3030h
Burasi2: dw 3030h
Burasi3: dw 3030h
db 'h ', 0Dh, 0Ah, 0

PROC_GECICIKOD endp

PROC_GECICIKOD3 proc near
;pushf
push si
;push ax
;push bx
;push dx
;push cx

gecicikod3:
               push dx
               push ax
               call proc_hex
               mov word ptr [Burasi3], ax 
               pop ax
               xchg ah, al
               call proc_hex
	       mov word ptr [Burasi2], ax 
               pop ax
               push ax
               call proc_hex
	       mov word ptr [Burasi1], ax 
               pop ax
               xchg ah, al
               call proc_hex 
               mov word ptr [Burasi0], ax 
               mov si, offset Burasi4
               call proc_printmsg
;pop cx
;pop dx
;pop bx
;pop ax
pop si
;popf

retn

PROC_GECICIKOD3 endp

FS_DirBuffer_Section: dd 0
FS_DDT_Next_Section: dd 0
FS_DirEntryCount: dd 0

FS_BuffDescriptor:
FS_CurrentSection: dd 0
FS_BuffValidData: db 0
FS_BuffDrvName: db 0
Sector_Buffer:
FS_Sector_Buffer: dw 0

DirEntry_Offset: dw 0
DirBuff_Sectors: dw 0

FS_Directory_Buffer: dw 0
FS_DirBuff_SectorCount: dw 0

; 21/07/2011
FSCF_VideoPage: dw 0
FSCF_CursorPos: dw 0
FSCF_PSign: db 0
FSCF_PSActive: db 0FFh
