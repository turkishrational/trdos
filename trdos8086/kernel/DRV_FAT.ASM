; *****************************************************************************
;
; DRV_FAT.ASM  [ TRDOS Kernel FAT12, FAT16, FAT32 FS Procedures ]
; Copyright (C) 2009 Erdogan TAN  [21/08/2011]
;
; 21/08/2011 
; 11/08/2011 proc_save_fat_buffer FAT32 bugfix & proc_update_cluster modif. 
; 01/08/2011
; 30/07/2011
; 24/07/2011
; 16/07/2011
; 05/07/2011
; 21/05/2011
; 30/04/2011
; 09-22-25/01/2011
; 26/12/2010
; 25/12/2010
; 02/11/2010
; 11/09/2010 proc_truncate_FAT_file & proc_truncate_cluster_chain
; 10/09/2010 proc_get_cluster_for_file_sector 
;            & proc_update_open_FAT_file_size_lmdt
; 11/07/2010 proc_get_first_free_cluster & proc_set_first_free_cluster
; 06/06/2010 proc_get_last_cluster
; 18/04/2010
; 28/02/2010 proc_update_cluster
; 27/09/2009
; 23/09/2009 proc_get_next_cluster modif.
;
; 22/09/2009 proc_load_FAT_root_directory & proc_load_FAT_sub_directory modif.
; 20/09/2009
; 05/09/2009
; 23/08/2009 proc_load_FAT_root_directory
; 23/08/2009 proc_load_FAT_sub_directory
;
; 07/07/2009 proc_get_next_cluster
;
; TRDOS.ASM (include drv_fat.asm)
;
; *****************************************************************************

proc_get_next_cluster proc near
                ; 05/07/2011 -> SegmentBoundaryCheck modification
                ; 12/06/2010 -> BX:CX = Current (Previous) Cluster Retn
                ; 27/09/2009 fat sector limits check, bx (fat_buffer) > 0 check
                ; 23/09/2009
                ; 07/07/2009
                ; 2005
                ; INPUT -> AX = Cluster Number, low 16 bit
                ; INPUT -> DX = Cluster Number, high 16 bit
                ; INPUT -> DS:SI = Logical Dos Drive Parameters Table
                ; OUTPUT -> clc -> No Error, DX:AX valid
                ; OUTPUT -> stc & AX=0 -> End Of Cluster Chain
                ; OUTPUT -> stc & AX>0 -> Error
                ; OUTPUT -> BX:CX = Current/Previous cluster if clc
                ; AX: Next Cluster Number, low 16 bit
                ; DX: Next Cluster Number, high 16 bit
                ; stc -> Error
                mov word ptr [FAT_CurrentCluster], ax
                mov word ptr [FAT_CurrentCluster]+2, dx

                ; 23/09/2009
                mov bx, word ptr [FAT_Buffer]
                cmp bx, 0
                ja short gnc_pass_allocate_FAT_buffer

 	       ;mov word ptr [FAT_BuffSector], bx
               ;mov word ptr [FAT_BuffSector]+2, bx
               ;mov byte ptr [FAT_BuffValidData], bx

                mov al, byte ptr [SI][LD_Name]
		mov byte ptr [FAT_BuffDrvName], al                

                ; bx = 0 => Allocate the first free segment
                mov cx, 1536 ; FAT Buffer contains 3 FAT sectors 
                mov ax, 0103h ; Buffer, AL= 3, FAT buffer allocation
                mov dx, 1 ; 05/07/2011, 512 byte boundary check 
                call proc_allocate_memory
                jc short loc_gnc_allocate_FAT_buffer_stc_retn
                
                mov word ptr [FAT_Buffer], bx

 		push es
                
                mov es, bx 

		xor ax, ax
                xor dx, dx

                jmp load_FAT_sectors3

loc_gnc_allocate_FAT_buffer_stc_retn:
                mov ax, 8 ; No space to allocate fat buffer !
                mov dx, 0
                retn

gnc_pass_allocate_FAT_buffer:
                push es

check_next_cluster_fat_type_0:
                ; 23/09/2009
                ; mov bx, word ptr [FAT_Buffer]
                mov es, bx
                ;
check_next_cluster_fat_type_1:
                cmp byte ptr [SI][LD_FATType], 2
                jb short get_FAT12_next_cluster
                ja get_FAT32_next_cluster
get_FAT16_next_cluster:
                mov bx, 300h ;768
                div bx
                ; AX = Count of 3 FAT sectors
                ; DX = Cluster Offset
                shl dx, 1 ; Multiply by 2
                mov bx, dx ; Byte Offset
                mov dx, 3
                mul dx  
                ; AX = FAT Sector
                ; DX = 0
                mov cl, byte ptr [SI][LD_Name]
                cmp byte ptr [FAT_BuffValidData], 0
                jna load_FAT_sectors0
                cmp cl, byte ptr [FAT_BuffDrvName]
                jne load_FAT_sectors0
                cmp ax, word ptr [FAT_BuffSector]
                jne load_FAT_sectors1
                mov ax, word ptr ES:[BX]
                cmp al, 0F7h
                jb  short loc_pass_gnc_FAT16_eoc_check
                cmp ah, 0Fh
                jb  short loc_pass_gnc_FAT16_eoc_check
              ; 12/06/2010
                jmp short loc_pass_gnc_FAT16_eoc_check_xor_ax_ax

get_FAT12_next_cluster:
                mov bx, 400h ;1024
                div bx
                ; AX = Count of 3 FAT sectors
                ; DX = Cluster Offset
               	push ax
                mov ax, 3
                mul dx    ; Multiply by 3
                shr ax, 1 ; Divide by 2
                mov bx, ax ; Byte Offset
                pop ax
                mov dx, 3
                mul dx 
                ; AX = FAT Beginning Sector
                ; DX = 0
                mov cl, byte ptr [SI][LD_Name]
                cmp byte ptr [FAT_BuffValidData], 0
                jna load_FAT_sectors0
                cmp cl, byte ptr [FAT_BuffDrvName]
                jne short load_FAT_sectors0
                cmp ax, word ptr [FAT_BuffSector]
                jne short load_FAT_sectors1

                mov ax, word ptr [FAT_CurrentCluster]
                shr ax, 1
                mov ax, word ptr ES:[BX]
                jnc short get_FAT12_nc_even
                shr ax, 1
                shr ax, 1
                shr ax, 1
                shr ax, 1

loc_gnc_fat12_eoc_check:
                cmp al, 0F7h
                jb  short loc_pass_gnc_FAT16_eoc_check
                cmp ah, 0Fh
                jb  short loc_pass_gnc_FAT16_eoc_check

loc_pass_gnc_FAT16_eoc_check_xor_ax_ax:
              ; 12/06/2010
                xor ax, ax
loc_pass_gnc_FAT16_eoc_check:
              ; 12/06/2010
loc_pass_gnc_FAT32_eoc_check:
               ; 12/06/2010
                mov cx, word ptr [FAT_CurrentCluster]
                mov bx, word ptr [FAT_CurrentCluster]+2
               ;
                cmc
                pop es 
                retn

get_FAT12_nc_even:
                and ah,0Fh
                jmp short loc_gnc_fat12_eoc_check

get_FAT32_next_cluster:
                mov cx, 180h ;384
                call Rx_Dos_Div32
                ; DX:AX = Count of 3 FAT sectors
                ; BX = Cluster Offset
                shl bx, 1
                shl bx, 1 ; Multiply by 4
                push bx
                mov bx, 3
                call proc_mul32
                pop bx ; Byte Offset
                ; DX:AX = FAT Sector
               
                mov cl, byte ptr [SI][LD_Name]
                cmp byte ptr [FAT_BuffValidData], 0
                jna short load_FAT_sectors0
                cmp cl, byte ptr [FAT_BuffDrvName]
                jne short load_FAT_sectors0
                cmp dx, word ptr [FAT_BuffSector]+2
                jne short load_FAT_sectors1
                cmp ax, word ptr [FAT_BuffSector]
                jne short load_FAT_sectors2

                mov ax, word ptr ES:[BX]
                mov dx, word ptr ES:[BX]+2
                and dh, 0Fh ; 28 bit Cluster

                cmp al, 0F7h
                jb  short loc_pass_gnc_FAT32_eoc_check
                cmp ah, 0FFh
                jb  short loc_pass_gnc_FAT32_eoc_check
                cmp dx, 0FFFh
                jb short loc_pass_gnc_FAT32_eoc_check
loc_pass_gnc_FAT32_eoc_check_xor_dx_dx:
               ; 12/06/2010
                xor dx, dx
                jmp short loc_pass_gnc_FAT16_eoc_check_xor_ax_ax

load_FAT_sectors0:
                mov byte ptr [FAT_BuffDrvName], cl
load_FAT_sectors1:
                mov word ptr [FAT_BuffSector]+2, dx
load_FAT_sectors2:
                mov word ptr [FAT_BuffSector], ax
load_FAT_sectors3:
                add ax, word ptr [SI][LD_FATbegin]
                adc dx, word ptr [SI][LD_FATbegin]+2
                cmp byte ptr [SI][LD_FATType], 2
                ja short pass_FAT_load_scount_0
                mov cx, word ptr [SI][LD_BPB][BPB_FATSz16]
                sub cx, word ptr [FAT_BuffSector]
                cmp cx, 3
                jna short pass_FAT_load_scount_2
                jmp short pass_FAT_load_scount_1
pass_FAT_load_scount_0:
                push ax
                push dx
                mov ax, word ptr [SI][LD_BPB][BPB_FATSz32]
                mov dx, word ptr [SI][LD_BPB][BPB_FATSz32]+2
                sub ax, word ptr [FAT_BuffSector]
                sbb dx, word ptr [FAT_BuffSector]+2
               ;jnc short pass_FAT_sector_negative_check
 	       ;pop dx
               ;pop ax
               ;xor cx, cx
               ;jmp short loc_FAT_sectors_load_error
;pass_FAT_sector_negative_check:
                cmp dx, 0
                ja short pass_FAT32_load_scount
                cmp ax, 3
                ja short pass_FAT32_load_scount
                mov cx, ax
                pop dx
                pop ax
               ;or cx, cx
               ;jz short loc_FAT_sectors_load_error
                jmp short pass_FAT_load_scount_2
pass_FAT32_load_scount:
                pop dx
                pop ax
pass_FAT_load_scount_1:
                mov cx, 3
pass_FAT_load_scount_2:
                xor bx, bx 
                call proc_disk_read
                jnc short pass_FAT_sectors_load_error
                mov byte ptr [FAT_BuffValidData], 0
loc_FAT_sectors_load_error:
                jmp short loc_pass_gnc_FAT32_eoc_check_xor_dx_dx
pass_FAT_sectors_load_error:
                mov byte ptr [FAT_BuffValidData], 1
                mov ax, word ptr [FAT_CurrentCluster]
                mov dx, word ptr [FAT_CurrentCluster]+2
                jmp check_next_cluster_fat_type_1

proc_get_next_cluster endp

proc_load_FAT_root_directory proc near
               ; 05/07/2011 SegmentBoundaryCheck modification
               ; 21/05/2011 BugFix
               ; 29/10/2010 
               ; 22/09/2009
               ; 05/09/2009
               ; 22/08/2009 
               
               ; INPUT -> 
               ; DS:SI = DOS Drive Description Table
              
               ; OUTPUT -> STC = Root directory could not be loaded
               ; Error number is in AX (AL)
               ; BX = 0 (BX > 0 -> sectors are loaded but not all)

               ; DS:SI = DOS Drive Description Table
               ; CX = Directory buffer size in sectors
	       ; NOTE: DirBuffer_Size is in bytes !
               ; BX = Directory Buffer Segment 
               
	       xor ax, ax ; Error No. 0 = Not a valid FAT FS
                            
              ;xor bh, bh
               mov bl, byte ptr [SI][LD_FATType]
              ;cmp bl, 1
              ;jnb short loc_check_FAT12_FAT16   
            
              ;retn 

;loc_check_FAT12_FAT16:
               mov bh, byte ptr [SI][LD_Name] 
              ;cmp bl, 2 ; FAT12 = 1, FAT16 = 2, FAT32 = 3
              ;ja short loc_plrd_FAT_xor_cmc_stc_retn1
               mov cx, word ptr [SI][LD_BPB][RootDirEnts]
              ;or cx, cx
              ;jz short loc_plrd_FAT_xor_cmc_stc_retn2
               cmp cx, 512
              ;ja short loc_plrd_FAT_xor_cmc_stc_retn1
               je short pass_plrd_movcx_512
              ;cmp cx, 112 ; 224
              ;jb short loc_plrd_FAT_xor_cmc_stc_retn1
                                            
               mov ax, 32 ; Root dir entry size in bytes
               mul cx
               push cx    ; Number of Root Dir Entries
              ;mov cx, word ptr [SI][LD_BPB][BytesPerSec]
              ;dec cx
              ;add ax, cx
              ;adc dx, 0
              ;inc cx
               mov cx, 512
               add ax, 511
              ; adc dx, 0
               div cx
               xchg cx, ax ; cx = Root directory size in sectors
               mul cx
                          ; ax = Directy Buffer Size in bytes
               pop dx     ; Number of Root Dir Entries
               dec dx     ; Last entry number of root dir
                          ; cx = Dir Buffer sector count             
               jmp short loc_plrd_check_dir_buffer

;loc_plrd_FAT_xor_cmc_stc_retn1:
;               xor cx, cx
;loc_plrd_FAT_xor_cmc_stc_retn2:
;               xor bx, bx
;               cmc
;               retn

pass_plrd_movcx_512:
               mov cx, 32
               mov dx, 511
               mov ax, 32*512  
loc_plrd_check_dir_buffer:
               push cx ; Directory Buffer sector Count
               mov cx, ax ; Directory Buffer Size in bytes
               mov byte ptr [DirBuff_DRV], bh
               mov byte ptr [DirBuff_FATType], bl
               mov byte ptr [DirBuff_ValidData], 0
               mov word ptr [DirBuff_LastEntry], dx
               mov word ptr [Dirbuff_Cluster], 0
               mov word ptr [Dirbuff_Cluster]+2, 0
              ;29/10/2010
	       mov bx, word ptr [Directory_Buffer] ; Segment
  	       or bx, bx
              ;21/05/2011 BugFix
               jnz short loc_plrd_deallocate_dirbuff
               mov word ptr [DirBuffer_Size], cx
               jmp short loc_plrd_allocate_dirbuff             

loc_plrd_deallocate_dirbuff:  
               cmp word ptr [DirBuffer_Size], cx
               je short pass_plrd_allocate_directory_buffer 
               
               mov word ptr [DirBuffer_Size], cx
               mov ax, 0104h ;Consequently for 04h allocation
               call proc_deallocate_memory

               mov cx, word ptr [DirBuffer_Size]
               xor bx, bx
               mov word ptr [Directory_Buffer], bx ; 0
              ; mov bx, 352
loc_plrd_allocate_dirbuff:
               mov ax, 0104h ;Consequently allocate Directory Buffer
               mov dx, 1 ; 05/07/2011, 512 byte boundary check 
               call proc_allocate_memory
               jc short loc_plrd_dir_buff_alloc_err_1
               mov word ptr [Directory_Buffer], bx

pass_plrd_allocate_directory_buffer:
               ; DS:SI = DOS Drive Description Table
               mov ax, word ptr [SI][LD_ROOTbegin]
               mov dx, word ptr [SI][LD_ROOTbegin]+2
               pop cx ; Directory Buffer sector Count

	       push cx
               push es
               mov es, bx
               xor bx, bx
               call proc_disk_read
               pop es
               mov bx, word ptr [Directory_Buffer]
               jnc short validate_RDirBuff_and_return

               pop ax ; pushed cx
               sub ax, cx
               or  ax, ax
               jnz short pass_plrd_reset_bx
               xor bx, bx
pass_plrd_reset_bx:
               mov cx, 15h ; DRV NOT READY OR READ ERROR !
               xchg ax, cx ; CX = Number of loaded sectors
               stc
               retn
validate_RDirBuff_and_return:
               pop cx
               mov byte ptr [DirBuff_ValidData], 1
               xor ax, ax
               retn

loc_plrd_dir_buff_alloc_err_1:
               pop cx
               mov ax, 8 ; NO SPACE TO ALLOCATE DIRECTORY BUFFER !
               xor bx, bx
               stc
               retn

proc_load_FAT_root_directory endp

proc_load_FAT_sub_directory proc near
               ; 05/07/2011 SegmentBoundaryCheck modification
               ; 29/10/2010
               ; 22/09/2009
               ; 05/09/2009
               ; 23/08/2009 
               
               ; INPUT -> 
               ; DS:SI = DOS Drive Description Table
               ; DX:AX = Cluster Number               

               ; OUTPUT -> STC = Sub directory could not be loaded
               ; Error number is in AX (AL)
               ; BX = 0 (BX > 0 -> sectors are loaded but not all)
               
               ; DS:SI = DOS Drive Description Table
               ; CX = Directory buffer size in sectors
               ; NOTE: DirBuffer_Size is in bytes !
               ; BX = Directory Buffer Segment 

               xor ch, ch
 	       mov cl, byte ptr [SI][LD_BPB][SecPerClust]
              ;or cl, cl
              ;jnz short loc_plsd_reset_dir_buff_decs

              ;xor bx, bx
              ;stc 
              ;retn                

;loc_plsd_reset_dir_buff_decs:
 	       mov byte ptr [DirBuff_ValidData], ch ; 0
 	       mov word ptr [Dirbuff_Cluster], ax
               mov word ptr [Dirbuff_Cluster]+2, dx
               mov al, byte ptr [SI][LD_Name]
               mov ah, byte ptr [SI][LD_FATType]
 	      ;mov byte ptr [DirBuff_DRV], al
              ;mov byte ptr [DirBuff_FATType], ah
               mov word ptr [DirBuff_DRV], ax
               mov ax, word ptr [SI][LD_BPB][BytesPerSec]
               mul cx
               push ax
               mov cx, 32 ; Entry size in Bytes
               div cx
               dec ax
               pop cx 
               mov word ptr [DirBuff_LastEntry], ax

loc_plsd_check_dir_buffer:
              ; 29/10/2010
               mov bx, word ptr [Directory_Buffer]
  	       or bx, bx
               jnz short loc_plsd_deallocate_dirbuff
               mov word ptr [DirBuffer_Size], cx
               jmp short loc_plsd_allocate_dirbuff            

loc_plsd_deallocate_dirbuff:                          
               cmp word ptr [DirBuffer_Size], cx
               je short pass_plsd_allocate_directory_buffer 

	       mov word ptr [DirBuffer_Size], cx
               mov ax, 0104h ;Consequently for 04h allocation
               call proc_deallocate_memory

               mov cx, word ptr [DirBuffer_Size]
               xor bx, bx
               mov word ptr [Directory_Buffer], bx ; 0
loc_plsd_allocate_dirbuff:
               mov ax, 0104h ;Consequently allocate Directory Buffer
               mov dx, 1 ; 05/07/2011, 512 byte boundary check 
               call proc_allocate_memory
 	       jc short loc_plsd_dir_buff_alloc_err_1

               mov word ptr [Directory_Buffer], bx

pass_plsd_allocate_directory_buffer:
               ; DS:SI = DOS Drive Description Table
               mov ax, word ptr [DirBuff_Cluster]
               mov dx, word ptr [DirBuff_Cluster]+2
	       sub ax, 2
               sbb dx, 0
               push bx
               xor bh, bh
               mov bl, byte ptr [SI][LD_BPB][SecPerClust]
               mov cx, bx
               call proc_mul32
               add ax, word ptr [SI][LD_DATAbegin]
               adc dx, word ptr [SI][LD_DATAbegin]+2
               pop bx
               push cx
               push es
               mov es, bx
               xor bx, bx
               call proc_disk_read
               pop es
               mov bx, word ptr [Directory_Buffer]
               jnc short validate_SDirBuff_and_return
               pop ax
               sub ax, cx
               or ax, ax
               jnz short pass_plsd_reset_bx
               xor bx, bx
pass_plsd_reset_bx:
               mov cx, 15h ; DRV NOT READY OR READ ERROR
               xchg ax, cx ; CX = Number of loaded sectors
               stc
               retn
validate_SDirBuff_and_return:
               pop cx
               mov byte ptr [DirBuff_ValidData], 1
               xor ax, ax
               retn

loc_plsd_dir_buff_alloc_err_1:
               mov ax, 8 ; NO SPACE TO ALLOCATE DIRECTORY BUFFER
               xor bx, bx
               stc
               retn

proc_load_FAT_sub_directory endp

proc_update_cluster proc near
                ; 11/08/2011  
                ; 30/07/2011
                ; 05/07/2011 SegmentBoundaryCheck modification
                ; 26/12/2010 BugFix (loc_update_fat32_cell)
                ; 24/10/2010 BX:CX BugFix (it was CX:BX inside)
                ; 13/06/2010 loc_FAT_sectors_rw_error (common)
                ; 10/04/2010
                ; 04/04/2010
                ; 09/02/2005
                ; INPUT -> AX = Cluster Number, low 16 bit
                ; INPUT -> DX = Cluster Number, high 16 bit
                ; INPUT -> CX = New Cluster Value, low 16 bit
                ; INPUT -> BX = New Cluster Value, high 16 bit
                ; INPUT -> DS:SI = Logical Dos Drive Parameters Table
                ; OUTPUT -> clc -> No Error, DX:AX valid
                ; OUTPUT -> stc & AX=0 -> End Of Cluster Chain
                ; OUTPUT -> stc & AX>0 -> Error (BX, CX -> any value)
                ; AX: Next Cluster, low 16 bit
                ; DX: Next Cluster, high 16 bit
                ; CX: New Cluster Value, low 16 bit
                ; BX: New Cluster Value, high 16 bit
                ; stc -> Error
                mov word ptr [FAT_CurrentCluster], ax
                mov word ptr [FAT_CurrentCluster]+2, dx
                mov word ptr [ClusterValue], cx
                mov word ptr [ClusterValue]+2, bx

                ; 28/02/2010
                ; 23/09/2009
                mov bx, word ptr [FAT_Buffer]
                cmp bx, 0
                ja short loc_update_cluster_check_fat_buffer

 	       ;mov word ptr [FAT_BuffSector], bx
               ;mov word ptr [FAT_BuffSector]+2, bx
               ;mov byte ptr [FAT_BuffValidData], bx

                mov al, byte ptr [SI][LD_Name]
		mov byte ptr [FAT_BuffDrvName], al                

                ; bx = 0 => Allocate the first free segment
                mov cx, 1536 ; FAT Buffer contains 3 FAT sectors 
                mov ax, 0103h ; Buffer, AL= 3, FAT buffer allocation
                mov dx, 1 ; 05/07/2011, 512 byte boundary check  
                call proc_allocate_memory
                jnc short loc_uc_allocate_FAT_buffer_stc_retn                

                mov word ptr [FAT_Buffer], bx

 		xor ax, ax
                xor dx, dx
                jmp load_uc_FAT_sectors_zero

loc_uc_allocate_FAT_buffer_stc_retn:
                mov ax, 8 ; No space to allocate fat buffer !
                mov dx, 0
                retn

update_cluster_inv_data:
              ; 11/08/2011 
                mov ax, 0Dh  ; Invalid Data
                retn 

loc_update_cluster_check_fat_buffer:
               ; 11/08/2011  
                mov bl, byte ptr [SI][LD_Name]
         	cmp byte ptr [FAT_BuffDrvName], bl
                je short loc_update_cluster_check_fat_type
                cmp byte ptr [FAT_BuffValidData], 2
                je loc_uc_save_fat_buffer
loc_uc_reset_fat_buffer_validation:
               ; 11/08/2011 
                mov byte ptr [FAT_BuffValidData], 0
loc_uc_check_fat_type_reset_drvname:
               ; 11/08/2011 
                mov byte ptr [FAT_BuffDrvName], bl
loc_update_cluster_check_fat_type:
               ; 11/08/2011 
                mov bl, byte ptr [SI][LD_FATType]
                cmp bl, 2 
                ja update_FAT32_cluster
                cmp ax, 2
                jb short update_cluster_inv_data
                mov cx, word ptr [SI][LD_Clusters]
                inc cx  
                mov word ptr [LastCluster], cx
                cmp ax, cx ; word ptr [LastCluster]
                ja short return_uc_FAT_stc
                or bl, bl ; cmp bl, 0
                jz short update_FAT12_cluster

update_FAT16_cluster:
               ; 11/08/2011  
pass_uc_fat16_errc:
                mov bx, 300h ;768
                div bx
                ; AX = Count of 3 FAT sectors
                ; DX = Cluster Offset in FAT buffer
               ; 11/08/2011
                mov bx, dx  
                shl bx, 1 ; Multiply by 2
                mov dx, 3
                mul dx  
                ; AX = FAT Sector
                ; DX = 0
                ; BX = Byte offset in FAT buffer
                mov cl, byte ptr [FAT_BuffValidData]
                cmp cl, 2
                jne short loc_uc_check_fat16_buff_sector_load

loc_uc_check_fat16_buff_sector_save:
                cmp ax, word ptr [FAT_BuffSector]
                jne short loc_uc_save_FAT_Buffer
                jmp short loc_update_fat16_cell

loc_uc_check_fat16_buff_sector_load:
                cmp cl, 1 ; byte ptr [FAT_BuffValidData]
                jne loc_uc_load_FAT_sectors
                cmp ax, word ptr [FAT_BuffSector]
                jne loc_uc_load_FAT_sectors

loc_update_fat16_cell:
loc_update_fat16_buffer:
                push es
		mov ax, word ptr [FAT_Buffer]
		mov es, ax
		mov ax, word ptr ES:[BX]
               ;xor dx, dx
                mov word ptr [FAT_CurrentCluster], ax
                mov word ptr [FAT_CurrentCluster]+2, dx
                mov cx, word ptr [ClusterValue]
                mov word ptr ES:[BX],cx
                inc word ptr [FAT_ClusterCounter]
                mov byte ptr [FAT_BuffValidData], 2
                pop es
                cmp ax, 2
               ; 24/10/2010
                jb short return_uc_FAT_stc_bx
                cmp ax, word ptr [LastCluster]
                ja short return_uc_FAT_stc_bx

loc_fat_buffer_updated:
               ; 24/10/2010
                mov bx, word ptr [ClusterValue]+2
                clc
                retn

return_uc_FAT_stc_bx:
                ; 24/10/2010
                mov bx, word ptr [Clustervalue]+2
return_uc_FAT_stc:
                xor ax, ax
                xor dx, dx
                stc
                retn

loc_uc_save_fat_buffer:
               ; 11/08/2011
               ; Byte ptr [FAT_BuffValidData] = 2 
                call proc_save_fat_buffer
                jc loc_FAT_sectors_rw_error2 ; 30/07/2011
               ;mov byte ptr [FAT_BuffValidData], 1
                mov ax, word ptr [FAT_CurrentCluster]
                mov dx, word ptr [FAT_CurrentCluster]+2
               ;11/08/2011
               ;mov cx, word ptr [ClusterValue]
               ;mov bx, word ptr [ClusterValue]+2
                jmp loc_update_cluster_check_fat_buffer

update_FAT12_cluster:
               ; 11/08/2011
pass_uc_fat12_errc:
                mov bx, 400h ;1024
                div bx
                ; AX = Count of 3 FAT sectors
                ; DX = Cluster Offset in FAT buffer
                mov cx, 3
                mov bx, ax
                mov ax, cx ; 3
                mul dx     ; Multiply by 3
                shr ax, 1  ; Divide by 2
                xchg bx, ax
                ; AX = Count of 3 FAT sectors
                ; BX = Byte Offset in FAT buffer   
                mul cx  ; 3 * AX
                ; AX = FAT Beginning Sector
                ; DX = 0
                mov cl, byte ptr [FAT_BuffValidData]
                jne short loc_uc_check_fat12_buff_sector_load

loc_uc_check_fat12_buff_sector_save:
                cmp ax, word ptr [FAT_BuffSector]
                jne short loc_uc_save_FAT_Buffer
                jmp short loc_update_fat12_cell

loc_uc_check_fat12_buff_sector_load:
                cmp cl, 1 ; byte ptr [FAT_BuffValidData]
                jne loc_uc_load_FAT_sectors
                cmp ax, word ptr [FAT_BuffSector]
                jne loc_uc_load_FAT_sectors

loc_update_fat12_cell:
		push es
		mov cx, word ptr [FAT_Buffer]
		mov es, cx
		mov cx, word ptr [FAT_CurrentCluster]
                shr cx, 1
                mov ax, word ptr ES:[BX]
                mov dx, ax ; 11/08/2011
                jnc short uc_FAT12_nc_even

                and ax, 0Fh
                mov cx, word ptr [ClusterValue]
                shl cx, 1
                shl cx, 1
                shl cx, 1
                shl cx, 1
                or cx, ax
                mov ax, dx ; 11/08/2011
                mov word ptr ES:[BX],cx
                shr ax, 1
                shr ax, 1
                shr ax, 1
                shr ax, 1

update_FAT12_buffer:
                pop es
                inc word ptr [FAT_ClusterCounter]
                mov word ptr [FAT_CurrentCluster], ax
                xor dx,dx
                mov word ptr [FAT_CurrentCluster]+2, dx
                mov byte ptr [FAT_BuffValidData], 2
                cmp ax,2
                jb return_uc_FAT_stc_bx
                cmp ax, word ptr [LastCluster]
                ja return_uc_FAT_stc_bx
                jmp loc_fat_buffer_updated

uc_FAT12_nc_even:
                and ax, 0F000h
                mov cx, word ptr [ClusterValue]
                and ch, 0Fh
                or cx, ax
                mov ax, dx ; 11/08/2011
                mov word ptr ES:[BX],cx
                and ah, 0Fh
                jmp short update_fat12_buffer

update_FAT32_cluster:
                cmp dx,0
                ja short pass_uc_FAT32_c_zero_check_1
                cmp ax,2
                jb update_cluster_inv_data
pass_uc_FAT32_c_zero_check_1:
               ; 11/08/2011
                mov cx, word ptr [SI][LD_Clusters]
                mov bx, word ptr [SI][LD_Clusters]+2
                add cx, 1
                adc bx, 0
                mov word ptr [LastCluster], cx
                mov word ptr [LastCluster]+2, bx
                cmp dx, bx
                jb short pass_uc_fat32_errc
                ja return_uc_FAT_stc
                cmp ax, cx
                ja return_uc_FAT_stc
pass_uc_fat32_errc:
                mov cx, 180h ;384
                call Rx_Dos_Div32
                ; DX:AX = Count of 3 FAT sectors
                ; BX = Cluster Offset in FAT buffer
                shl bx, 1
                shl bx, 1 ; Multiply by 4
               ; 11/08/2011 
                mov cx, bx
                mov bx, 3
                call proc_mul32
                mov bx, cx ; Cluster Offset in FAT buffer
                ; DX:AX = FAT Sector
                mov cl, byte ptr [FAT_BuffValidData]
                cmp cl, 2
                jne short loc_uc_check_fat32_buff_sector_load

loc_uc_check_fat32_buff_sector_save:
                cmp ax, word ptr [FAT_BuffSector]
                jne loc_uc_save_FAT_Buffer
                cmp dx, word ptr [FAT_BuffSector]+2
                jne loc_uc_save_FAT_Buffer
                jmp short loc_update_fat32_cell

loc_uc_check_fat32_buff_sector_load:
                cmp cl, 1 ; byte ptr [FAT_BuffValidData]
                jne short loc_uc_load_FAT_sectors
                cmp ax, word ptr [FAT_BuffSector]
                jne short loc_uc_load_FAT_sectors
		cmp dx, word ptr [FAT_BuffSector]+2
                jne short loc_uc_load_FAT_sectors

loc_update_fat32_cell:
loc_update_fat32_buffer:
                push es
                mov ax, word ptr [FAT_Buffer]
                mov es, ax ; 26/12/2010 BugFix bx -> ax
                mov ax, word ptr ES:[BX]
                mov dx, word ptr ES:[BX]+2
                and dh, 0Fh ; 28 bit Cluster
                mov word ptr [FAT_CurrentCluster], ax
                mov word ptr [FAT_CurrentCluster]+2, dx
                mov cx, word ptr [ClusterValue]+2
                mov word ptr ES:[BX]+2,cx
                mov cx, word ptr [ClusterValue]
                mov word ptr ES:[BX],cx
                add word ptr [FAT_ClusterCounter],1
                adc word ptr [FAT_ClusterCounter]+2,0
                mov byte ptr [FAT_BuffValidData], 2
                pop es
                cmp dx,0
                ja short pass_uc_FAT32_c_zero_check_2
                cmp ax,2
                jb return_uc_FAT_stc_bx
pass_uc_FAT32_c_zero_check_2:
                cmp dx, word ptr [LastCluster]+2
                jb loc_fat_buffer_updated
                ja return_uc_FAT_stc_bx
                cmp ax, word ptr [LastCluster]
                ja return_uc_FAT_stc_bx
                jmp loc_fat_buffer_updated

loc_FAT_sectors_rw_error1:
                mov byte ptr [FAT_BuffValidData], 0
loc_FAT_sectors_rw_error2:
               ; mov ax, error code
                mov dx, 0
                mov cx, word ptr [ClusterValue]
                mov bx, word ptr [ClusterValue]+2
                retn

loc_uc_load_FAT_sectors:
                mov word ptr [FAT_BuffSector]+2, dx
                mov word ptr [FAT_BuffSector], ax
load_uc_FAT_sectors_zero:
                add ax, word ptr [SI][LD_FATbegin]
                adc dx, word ptr [SI][LD_FATbegin]+2
                push es
                mov bx, word ptr [FAT_Buffer]
                mov es, bx
                xor bx, bx
                mov cx, 3
                call proc_disk_read
                pop es
                jc short loc_FAT_sectors_rw_error1
                mov byte ptr [FAT_BuffValidData], 1
                mov ax, word ptr [FAT_CurrentCluster]
                mov dx, word ptr [FAT_CurrentCluster]+2
                mov cx, word ptr [ClusterValue]
                mov bx, word ptr [ClusterValue]+2
                jmp loc_update_cluster_check_fat_type

ClusterValue:   dd 0

proc_update_cluster endp

proc_save_FAT_buffer proc near
                ; 11/08/2011 BugFix
                ; 01/08/2011
                ; 30/07/2011 
                ; 11/09/2010 -> retn -> BX= FAT_Buffer
                ; 18/04/2010
                ; 10/04/2010
                ; 28/02/2010 
                ; 09/02/2005 
                ; INPUT: None
                ; OUTPUT: stc -> error code in AL
                ; AX, DX, CX is modified
                ; BX = FAT_Buffer segment

               ; 11/08/2011   
               ; 01/08/2011
              ;mov bx, word ptr [FAT_Buffer]
              ;cmp bx, 1
              ;jb short loc_save_fat_buffer_inv_data_retn          

               ;cmp byte ptr [FAT_BuffValidData], 2 
               ;je short loc_save_fat_buff

;loc_save_fat_buffer_retn:
               ; 01/08/2011
               ;xor ax, ax
               ; retn

loc_save_fat_buff:
                xor dl, dl
		mov dh, byte ptr [FAT_BuffDrvName]
               ; 01/08/2011  
                cmp dh, 'A'
                jb short loc_save_fat_buffer_inv_data_retn
                
                sub dh, 'A'

                push si

                mov si, offset Logical_DOSDisks
                add si, dx
               ;                 

               ; 01/08/2011
                mov dl, byte ptr [SI][LD_FATType]
                and dl, dl
                jz  short loc_save_fat_buffer_inv_data_pop_retn 
                mov ax, word ptr [FAT_BuffSector]
                cmp dl, 2
                ja short loc_save_fat32_buff

loc_save_fat_12_16_buff:
                mov dx, word ptr [FAT_BuffSector]
                xor dh, dh
                mov cx, word ptr [SI][LD_BPB][FATSecs] 
                sub cx, ax
                jna short loc_save_fat_buffer_inv_data_retn 
                jmp short loc_save_fat_buffer_check_rs3

loc_save_fat32_buff:
                mov dx, word ptr [FAT_BuffSector]+2
                mov bx, Word Ptr [SI][LD_BPB][FAT32_FAT_Size]+2
                mov cx, Word Ptr [SI][LD_BPB][FAT32_FAT_Size]
                sub cx, ax
                sbb bx, dx
               ; 01/08/2011
                jz  short loc_save_fat_buffer_check_rs3
               ; 11/08/2011
                jnc short loc_save_fat_buff_remain_sectors_3

loc_save_fat_buffer_inv_data_pop_retn:
                pop si
loc_save_fat_buffer_inv_data_retn:
              ; 01/08/2011
                mov ax, 0Dh ; Invalid DATA
                retn

loc_save_fat_buff_remain_sectors_3:
                mov cx, 3
                jmp short loc_save_fat_buff_continue

loc_save_fat_buffer_check_rs3:
               ; 01/08/2011
                cmp cx, 3
                ja short loc_save_fat_buff_remain_sectors_3

loc_save_fat_buff_continue:
              ; 11/08/2011 BugFix
                mov bx, word ptr [FAT_Buffer]
              ; 18/04/2010 
                add ax, word ptr [SI][LD_FATbegin]
                adc dx, word ptr [SI][LD_FATbegin]+2
                mov es, bx
                xor bx, bx
                push cx
                call proc_disk_write
                pop cx
                push ds
                pop es   
                jc short loc_save_FAT_buff_write_err
                cmp byte ptr [SI][LD_FATType], 2
                jna short loc_calc_2nd_fat12_16_addr
loc_calc_2nd_fat32_addr:
                mov ax, Word Ptr [SI][LD_BPB][FAT32_FAT_Size]
                mov dx, Word Ptr [SI][LD_BPB][FAT32_FAT_Size]+2
                jmp short loc_calc_2nd_fat_addr
loc_calc_2nd_fat12_16_addr:
                mov ax, word ptr [SI][LD_BPB][FATSecs]
                xor dx, dx
loc_calc_2nd_fat_addr:
                add ax, word ptr [SI][LD_FATbegin]
                adc dx, word ptr [SI][LD_FATbegin]+2
                add ax, word ptr [FAT_BuffSector]
                adc dx, word ptr [FAT_BuffSector]+2
                mov bx, word ptr [FAT_Buffer]
                mov es, bx
                xor bx, bx
                call proc_disk_write
                push ds
                pop es   
                jc short loc_save_FAT_buff_write_err
                mov byte ptr [FAT_BuffValidData], 1 ; Valid (but do not save)
loc_save_FAT_buff_write_err:
               ; 10/04/2010
                pop si
               ; 11/09/2010
      		mov bx, word ptr [FAT_Buffer]
                retn

proc_save_FAT_buffer endp

proc_calculate_FAT_freespace proc near
               ; 30/04/2011
               ; 21/08/2010 CX = 0
               ; 10/04/2010 BugFix
               ; 03/04/2010 modification
               ; 2005
               ;
               ; INPUT: DX:AX = Cluster count to be added or subtracted
               ; If BH = FFh, DS:SI = TR-DOS Logical Drive Description Table
               ; If BH < FFh, BH = TR-DOS Logical Drive Number
               ; BL: 0= Calculate, 1= Add, 2= Subtract, 3= Get (Not Set/Calc)
               ; OUTPUT: DX:AX = Free Space in sectors
               ; SI= Logical Dos Drive Table Offset
               ; BH= Logical Dos Drive Number (same with input value of BH)
               ; BL= Performed operation (same with input value of BL)
               ; ES = DS ; 30/04/2011 
               ; CX = 0 -> valid, CX > 0 -> error or invalid
               ;   If DX:AX = FFFFFFFFh, it is 're-calculation needed'
               ;   sign due to r/w error   

              ;30/04/2011
               push ds
               pop es

              ; 10/04/2010
	       mov word ptr [CFS_OPType], bx
 	       mov word ptr [CFS_CC], ax
               mov word ptr [CFS_CC]+2, dx  
               
               cmp bh, 0FFh
               je short pass_calculate_freespace_get_drive_dt_offset


loc_calculate_freespace_get_drive_dt_offset:     
               mov ah, bh
               xor al, al
               mov si, offset Logical_DOSdisks
               add si, ax

pass_calculate_freespace_get_drive_dt_offset:
              ; 21/08/2010
               cmp byte ptr [SI][LD_FATType], 2
               jna short pass_getFAT32_fsinfo_sector

loc_getFAT32_fsinfo_sector:
               mov ax, word ptr [SI][LD_StartSector]
               mov dx, word ptr [SI][LD_StartSector]+2
               add ax, word ptr [SI][LD_BPB][FAT32_FSInfoSec]
               adc dx, 0
               mov word ptr [CFS_FAT32FSINFOSEC], ax
               mov word ptr [CFS_FAT32FSINFOSEC]+2, dx

pass_getFAT32_fsinfo_sector:
               or bl, bl
               jz short loc_reset_fcc
               
loc_get_free_sectors:
               mov ax, word ptr [SI][LD_FreeSectors]
               mov dx, word ptr [SI][LD_FreeSectors]+2
               xor cx, cx
               dec cx ; FFFFh
               cmp dx, cx
               jne short loc_get_free_sectors_check_optype
               cmp ax, cx
               je short loc_get_free_sectors_retn
loc_get_free_sectors_check_optype:
               cmp bl, 3
               jb short loc_set_fcc
               inc cx
loc_get_free_sectors_retn:
               retn               

loc_set_fcc:
              ; 21/08/2010
               cmp byte ptr [SI][LD_FATType], 2
               ja loc_update_FAT32_fs_info_fcc

              ; 10/04/2010
               mov ax, word ptr [SI][LD_FreeSectors]
               mov dx, word ptr [SI][LD_FreeSectors]+2
               mov cl, byte ptr [SI][LD_BPB][SecPerClust]
              ; cmp cl, 1
              ; jna short pass_set_fcc_div32
               xor ch, ch
               call Rx_Dos_Div32
              ; or bx, bx 
              ;          ; BX -> Remain sectors < SecPerClust
              ;          ; BX > 0 -> invalid free sector count
              ; jnz short loc_reset_fcc 
              ; or dx, dx
              ; jnz short loc_reset_fcc  
;pass_set_fcc_div32:
               mov word ptr [FreeClusterCount], ax
	     ; mov word ptr [FreeClusterCount]+2, dx
 
               jmp loc_set_free_sectors_FAT12_FAT16

loc_reset_fcc:
              ; 03/04/2010 
               xor ax, ax
               mov word ptr [FreeClusterCount], ax ; 0
               mov word ptr [FreeClusterCount]+2, ax ; 0
               push word ptr [SI][LD_Clusters]
               pop word ptr [LastCluster]
               push word ptr [SI][LD_Clusters]+2
               pop word ptr [LastCluster]+2
               add word ptr [LastCluster],1
               adc word ptr [LastCluster]+2, ax ; 0

               cmp byte ptr [SI][LD_FATType],2
               jna short loc_count_free_fat_clusters_0  

               dec ax ; FFFFh
               mov word ptr [CFS_FAT32FC], ax ;FFFFh
               mov word ptr [CFS_FAT32FC]+2, ax  ;FFFFh
                
               mov ax, 2
               mov dx, 0
loc_count_fc_next_cluster_0:
               push ax
               push dx
               call proc_get_next_cluster
               jnc short loc_check_fat32_ff_cluster
              ; 21/08/2010 
               or ax, dx
               jz short pass_inc_cfs_fcc_0
loc_put_fcc_unknown_sign:
               pop dx
               pop ax
               ; "Free count is Unknown" sign
               ;mov word ptr [FreeClusterCount], 0FFFFh
               ;mov word ptr [FreeClusterCount]+2, 0FFFFh
               jmp loc_put_fcc_invalid_sign
loc_check_fat32_ff_cluster:
               ; 21/08/2010
               or ax, dx
               jnz short pass_inc_cfs_fcc_0
               pop dx
               pop ax
               mov word ptr [CFS_FAT32FC], ax
               mov word ptr [CFS_FAT32FC]+2, dx
              ; mov word ptr [FreeClusterCount], 1
               inc word ptr [FreeClusterCount]
               jmp short pass_inc_cfs_fcc_1
pass_inc_cfs_fcc_0:
               pop dx
               pop ax
pass_inc_cfs_fcc_0c:
               add ax,1
               adc dx,0
               cmp dx, word ptr [LastCluster]+2
               jb short loc_count_fc_next_cluster_0
               ja loc_update_FAT32_fs_info_fcc
               cmp ax, word ptr [LastCluster]
               jna short loc_count_fc_next_cluster_0
               jmp loc_update_FAT32_fs_info_fcc

loc_count_free_fat_clusters_0:
               mov ax, 2
               mov dx, 0
loc_count_fc_next_cluster:
               push ax
               push dx
               call proc_get_next_cluster
               jnc short loc_count_free_clusters_1
              ; 21/08/2010 
               or ax, dx
               jz short pass_inc_cfs_fcc
loc_count_fcc_stc:
               pop dx
               pop ax
               ; "Free count is Unknown" sign
              ; mov word ptr [FreeClusterCount], 0FFFFh
              ; mov word ptr [FreeClusterCount]+2, 0FFFFh
               jmp short loc_put_fcc_invalid_sign
loc_count_free_clusters_1:
               or ax, dx
               jnz short pass_inc_cfs_fcc
               add word ptr [FreeClusterCount],1
               adc word ptr [FreeClusterCount]+2,0
pass_inc_cfs_fcc:
               pop dx
               pop ax
pass_inc_cfs_fcc_1:
               add ax,1
               adc dx,0
               cmp dx, word ptr [LastCluster]+2
               jb short loc_count_fc_next_cluster
               ja short loc_set_free_sectors
               cmp ax, word ptr [LastCluster]
               jna short loc_count_fc_next_cluster
loc_set_free_sectors:
               cmp byte ptr [SI][LD_FATType], 2
               ja short loc_update_FAT32_fs_info_fcc
loc_set_free_sectors_FAT12_FAT16:
               cmp byte ptr [CFS_OPtype], 0
               jna short pass_FAT_add_sub_fcc
               mov ax, word ptr [CFS_CC]
               cmp byte ptr [CFS_OPtype], 1
               ja short pass_FAT_add_fcc
               add word ptr [FreeClusterCount], ax
               jmp short pass_FAT_add_sub_fcc
pass_FAT_add_fcc:
               sub word ptr [FreeClusterCount], ax
pass_FAT_add_sub_fcc:
               xor ah, ah
               mov al, byte ptr [SI][LD_BPB][SecPerClust]
               mov dx, word ptr [FreeClusterCount]
               mul dx
               ; 21/08/2010
               xor cx, cx 
               jmp short loc_cfs_retn_params

loc_update_FAT32_fs_info_fcc:
               mov ax, word ptr [CFS_FAT32FSINFOSEC]
               mov dx, word ptr [CFS_FAT32FSINFOSEC]+2
               mov bx, offset DOSBootSectorBuff
               mov cx, 1
              ;push ds
              ;pop es
               call proc_disk_read
               jnc short loc_check_fcc_FSINFO_signs
loc_put_fcc_invalid_sign:
              ; 21/08/2010 
               xor ax, ax ; 0
               dec ax ; FFFFh
               mov dx, ax
               mov cx, ax
loc_cfs_retn_params:
               mov word ptr [SI][LD_FreeSectors], ax
               mov word ptr [SI][LD_FreeSectors]+2, dx
               mov bx, word ptr [CFS_OPType]
               retn
loc_check_fcc_FSINFO_signs:
               cmp word ptr [DOSBootSectorBuff], 5252h
               jne short loc_put_fcc_invalid_sign
               cmp word ptr [DOSBootSectorBuff]+2, 4161h
               jne short loc_put_fcc_invalid_sign
               mov bx, offset DOSBootSectorBuff
               add bx, 484
               cmp word ptr [BX], 7272h
               jne short loc_put_fcc_invalid_sign
               cmp word ptr [BX]+2, 6141h
               jne short loc_put_fcc_invalid_sign
               add bx, 4
               cmp byte ptr [CFS_OPtype], 0
               jna short pass_FAT32_add_sub_fcc_0
               mov ax, word ptr [BX]
               mov dx, word ptr [BX]+2
               cmp byte ptr [CFS_OPtype], 1
               ja short pass_FAT32_add_fcc
               add ax, word ptr [CFS_CC]
               adc dx, word ptr [CFS_CC]+2
               mov word ptr [FreeClusterCount], ax
               mov word ptr [FreeClusterCount]+2, dx
               ; Following "FFFFFFFFh" replacement is a sign
               ; for the first free cluster reset, later
               ; This is only for bl=1 status (after deleting a file)
               mov word ptr [BX]+4, 0FFFFh ; Next free Cluster Low Word
               mov word ptr [BX]+6, 0FFFFh ; Next free Cluster High Word
               jmp short pass_FAT32_add_sub_fcc
pass_FAT32_add_fcc:
               sub ax, word ptr [CFS_CC]
               sbb dx, word ptr [CFS_CC]+2
               mov word ptr [FreeClusterCount], ax
               mov word ptr [FreeClusterCount]+2, dx
               jmp short pass_FAT32_add_sub_fcc
pass_FAT32_add_sub_fcc_0:
               push word ptr [CFS_FAT32FC]
               pop word ptr [BX]+4
               push word ptr [CFS_FAT32FC]+2
               pop word ptr [BX]+6
pass_FAT32_add_sub_fcc:
               push word ptr [FreeClusterCount]
               pop word ptr [BX]
               push word ptr [FreeClusterCount]+2
               pop word ptr [BX]+2
               mov ax, word ptr [CFS_FAT32FSINFOSEC]
               mov dx, word ptr [CFS_FAT32FSINFOSEC]+2
              ;push ds
              ;pop es
               mov bx, offset DOSBootSectorBuff
               mov cx, 1
               call proc_disk_write
               jc loc_put_fcc_invalid_sign
loc_set_FAT32_free_sectors:
               xor bh, bh
               mov bl, byte ptr [SI][LD_BPB][SecPerClust]
               mov ax, word ptr [FreeClusterCount]
               mov dx, word ptr [FreeClusterCount]+2
               call proc_mul32
              ; 21/08/2010
               ;xor cx, cx
               xor cl, cl
               jmp loc_cfs_retn_params

CFS_OPType:    db 0
CFS_Drv:       db 0
CFS_CC:        dd 0
CFS_FAT32FSINFOSEC: dd 0
CFS_FAT32FC:   dd 0

proc_calculate_FAT_freespace endp

proc_get_last_cluster proc near
                ; 12/06/2010 BX:CX -> previous cluster retn
                ; 06/06/2010
                ; INPUT -> AX = First Cluster Number, low 16 bit
                ; INPUT -> DX = First Cluster Number, high 16 bit
                ; INPUT -> DS:SI = Logical Dos Drive Parameters Table
                ; OUTPUT -> clc -> No Error, DX:AX valid
                ; OUTPUT -> stc & AX>0 -> Error
                ; AX: Last Cluster Number, low 16 bit
                ; DX: Last Cluster Number, high 16 bit
                ; stc -> Error

                mov cx, ax
                mov bx, dx

loc_glc_get_next_cluster_1:
                mov word ptr [glc_prevcluster], cx
                mov word ptr [glc_prevcluster]+2, bx

loc_glc_get_next_cluster_2:
                call proc_get_next_cluster
               ; cx:bx = previous cluster 
               ; dx:ax = the last cluster
                jnc short loc_glc_get_next_cluster_1

                or ax, ax
                jnz short loc_glc_stc_retn

                ; cx:bx = previous Cluster
                mov dx, bx
                mov ax, cx

loc_glc_prev_cluster_retn:
                mov cx, word ptr [glc_prevcluster]
                mov bx, word ptr [glc_prevcluster]+2

                retn

loc_glc_stc_retn:
                cmc ;stc
                jmp short loc_glc_prev_cluster_retn

;12/06/2010
glc_prevcluster: dd 0
                       
proc_get_last_cluster endp

proc_get_first_free_cluster proc near
               ; 26/12/2010 BugFix
               ; 25/12/2010 BugFix
               ; 11/07/2010
               ; 10/07/2010
               ; INPUT -> DS:SI = DOS Drive Description Table
               ; OUTPUT -> ES=DS
               ; DS:SI =  DOS Drive Description Table
               ; stc -> Error code in AL
               ; clc -> DX:AX = FFFFFFFFh if no free space
               ; BX = FAT32 FSI sector buffer address if > 0
               ; DX:AX >= 0 and DX:AX < FFFFFFFFh is valid
               ; but DX:AX >= 2 is expected for FAT/FAT32 
   
                push ds
                pop es

                ; 11/07/2010
                mov ax, word ptr [SI][LD_Clusters]
                mov dx, word ptr [SI][LD_Clusters]+2
                add ax, 1
                adc dx, 0

                mov word ptr [gffc_last_free_cluster], ax
                mov word ptr [gffc_last_free_cluster]+2, dx
                ;

              ; 25/12/2010
                cmp byte ptr [SI][LD_FATType], 2
                ja short loc_gffc_get_first_fat32_free_cluster

                mov ax, 2
                xor dx, dx
                jmp short loc_gffc_get_first_fat_free_cluster

loc_gffc_get_first_fat32_free_cluster:
                mov ax, word ptr [SI][LD_StartSector]
                mov dx, word ptr [SI][LD_StartSector]+2
                add ax, word ptr [SI][LD_BPB][BPB_FSInfo]
                adc dx, 0
                mov bx, offset DOSBootSectorBuff
               ; 26/12/2010 CX=1 BugFix
                xor cx, cx
                inc cl  
                call proc_disk_read
                jnc short loc_gffc_check_fsinfo_signs
retn_gffc_get_fsinfo_sec:
                retn
retn_gffc_get_fsinfo_stc:
              ; 11/07/2010
                xor bx, bx
              ; xor dx, dx
                mov ax, 0Bh ; Invalid format
                stc
                retn

loc_gffc_check_FSINFO_signs:
                cmp word ptr [DOSBootSectorBuff], 5252h
                jne short retn_gffc_get_fsinfo_stc
                cmp word ptr [DOSBootSectorBuff]+2, 4161h
                jne short retn_gffc_get_fsinfo_stc
               ;mov bx, offset DOSBootSectorBuff
                add bx, 484
                cmp word ptr [BX], 7272h
                jne short retn_gffc_get_fsinfo_stc
                cmp word ptr [BX]+2, 6141h
                jne short retn_gffc_get_fsinfo_stc
                add bx, 8 ; FSI_Next_Free
                push word ptr [BX]
                pop ax
                push word ptr [BX]+2
                pop dx

               ;DX:AX = First free cluster 
               ;(from FAT 32 FS Info sector)

                xor bx, bx ; 0
                dec bx  ;FFFFh

                cmp dx, bx
                jb short loc_gffc_get_first_fat_free_cluster
                cmp ax, bx
                jb short loc_gffc_get_first_fat_free_cluster

                inc dx ; 0
                inc ax ; 0
                inc ax ; 1
                inc ax ; 2 
                                  
loc_gffc_get_first_fat_free_cluster:
                mov word ptr [gffc_first_free_cluster], ax
                mov word ptr [gffc_first_free_cluster]+2, dx
                mov word ptr [gffc_next_free_cluster], ax
                mov word ptr [gffc_next_free_cluster]+2, dx

loc_gffc_loop_get_next_cluster:
                call proc_get_next_cluster
                jnc short loc_gffc_first_free_fat_cluster_cont
                or ax, ax
                jz short loc_gffc_first_free_fat_cluster_next
                cmc 
                retn

loc_gffc_first_free_fat_cluster_cont:
                cmp ax, 0
                ja short loc_gffc_first_free_fat_cluster_next
                cmp dx, 0
                ja short loc_gffc_first_free_fat_cluster_next
                mov dx, bx
                mov ax, cx
                jmp short loc_gffc_check_for_set
                 
loc_gffc_first_free_fat_cluster_next:
                mov ax, word ptr [gffc_next_free_cluster]
                mov dx, word ptr [gffc_next_free_cluster]+2
                cmp dx, word ptr [gffc_last_free_cluster]+2
                ja short retn_stc_from_get_first_free_cluster
                jb short pass_gffc_last_cluster_ax_check
                cmp ax, word ptr [gffc_last_free_cluster]
                jnb short retn_stc_from_get_first_free_cluster
pass_gffc_last_cluster_ax_check:
                add ax, 1
                adc dx, 0
                mov word ptr [gffc_next_free_cluster], ax
                mov word ptr [gffc_next_free_cluster]+2, dx
                jmp short loc_gffc_loop_get_next_cluster

retn_stc_from_get_first_free_cluster:
                mov ax, word ptr [gffc_first_free_cluster]
                mov dx, word ptr [gffc_first_free_cluster]+2
                cmp dx, 0
                ja short loc_gffc_check_previous_clusters
                cmp ax, 2
                ja short loc_gffc_check_previous_clusters
                ; xor dx, dx ; 0
                dec dx
                mov ax, dx ; FFFFh

loc_gffc_check_for_set:
                cmp byte ptr [SI][LD_FATType], 3
                jnb short loc_gffc_set_first_fat32_cluster_1
                xor bx, bx
                retn
loc_gffc_set_first_fat32_cluster_1:
                mov bx, offset DOSBootSectorBuff
                cmp ax, word ptr [BX]+492 ; FSI_Next_Free
                jne short loc_gffc_set_first_fat32_cluster_2
                cmp dx, word ptr [BX]+494
                je short retn_gffc_set_first_fat32_cluster
loc_gffc_set_first_fat32_cluster_2:
                call proc_set_first_free_cluster
retn_gffc_set_first_fat32_cluster:
                retn

loc_gffc_check_previous_clusters:
               ; 11/07/2010
                sub ax, 1
                sbb dx, 0
                mov word ptr [gffc_last_free_cluster], ax 
                mov word ptr [gffc_last_free_cluster]+2, dx
                mov ax, 2
                xor dx, dx
                jmp loc_gffc_get_first_fat_free_cluster
 
gffc_next_free_cluster: dd 0
gffc_first_free_cluster: dd 0
gffc_last_free_cluster: dd 0

proc_get_first_free_cluster endp

proc_set_first_free_cluster proc near
               ; 21/08/2011 BugFix
               ; 30/07/2011
               ; 11/07/2010 -> only for FAT32 file system
               ; INPUT -> DS:SI = DOS Drive Description Table
               ;          ES=DS
               ;          DX:AX = First free cluster
               ;  IF BX > 0 it is FSINFO sector buffer address
               ;  BX must be 0 is FSINFO sector is not loaded
               ;
               ; OUTPUT-> DS:SI = DOS Drive Description Table
               ;          ES=DS
               ;  IF BX > 0 it is FSINFO sector buffer address
               ;  BX is 0 if FSINFO sector could not be loaded
               ;  stc -> Error code in AL
               ;  clc -> first free cluster is successfully updated 


               ; 21/08/2011

                cmp byte ptr [SI][LD_FATType], 3
                jb short loc_sffc_invalid_drive

                mov cx, 1  ; 21/08/2011

                or bx, bx
                jnz short loc_sffc_check_FSINFO_signs_2  
   
loc_sffc_read_fsinfo_sector:
                push ax
                push dx
                mov ax, word ptr [SI][LD_StartSector]
                mov dx, word ptr [SI][LD_StartSector]+2
                add ax, word ptr [SI][LD_BPB][BPB_FSInfo]
                adc dx, 0
                mov bx, offset DOSBootSectorBuff
               ; mov cx, 1
                call proc_disk_read
                pop dx
                jnc short loc_sffc_check_fsinfo_signs_1
                pop dx
                mov bx, 0
                retn

              ; 21/08/2011 
loc_sffc_invalid_drive:
                mov ax, 0Fh ; MSDOS Error : Invalid drive
                retn 

loc_sffc_check_FSINFO_signs_1:
                pop ax
loc_sffc_check_FSINFO_signs_2:
                cmp word ptr [BX], 5252h
                jne short retn_sffc_get_fsinfo_stc
                cmp word ptr [BX]+2, 4161h
                jne short retn_sffc_get_fsinfo_stc
                cmp word ptr [BX]+484, 7272h
                jne short retn_sffc_get_fsinfo_stc
                cmp word ptr [BX]+486, 6141h
                jne short retn_sffc_get_fsinfo_stc
                mov word ptr [BX]+492, ax
                mov word ptr [BX]+494, dx
                mov ax, word ptr [SI][LD_StartSector]
                mov dx, word ptr [SI][LD_StartSector]+2
                add ax, word ptr [SI][LD_BPB][BPB_FSInfo]
                adc dx, 0
               ;mov bx, offset DOSBootSectorBuff
               ;mov cx, 1
                call proc_disk_write
                jc short loc_sffc_retn ; 30/07/2011
                mov ax, word ptr [BX]+492
                mov dx, word ptr [BX]+494
loc_sffc_retn:
                retn

retn_sffc_get_fsinfo_stc:
              ; 11/07/2010
                xor bx, bx
              ; xor dx, dx
                mov ax, 0Bh ; Invalid format
                stc
                retn

proc_set_first_free_cluster endp

proc_get_cluster_for_file_sector proc near
               ; 16/07/2011  
               ; 23/10/2010
               ; 19/10/2010
               ; 09/10/2010 
               ; 12/09/2010
               ; 11/09/2010 
               ;        major modification for optimization
               ; 10/09/2010
               ; 09/09/2010 (Adapting to FAT)
               ; 08/09/2010 (FS procedure)
               ; INPUT ->
               ;  DS:SI = Logical dos drv table
               ;  DX:AX = Sector sequence number of the file
               ;  BX:CX = First Cluster of the file
               ; OUTPUT ->
               ;  DX:AX = Cluster number of the requested sector
               ;  BX:CX = Sector offset of the cluster
               ;  DI = Sectors per cluster value
               ;  stc & AX > 0 -> error except EOF
               ;  stc & AX = 0 -> EOF ->
               ;  Last cluster before the req. sector number
               ;  BX:CX = The Last cluster number
               ;  DX = Required additional sectors ( 0 -> 64K)

               ; 12/09/2010 
                mov word ptr [FAT_File_Sec_SN], ax
                mov word ptr [FAT_File_Sec_SN]+2, dx
                push bx
                push cx
               ; 11/09/2010
                xor ch, ch
                mov cl, byte ptr [SI][LD_BPB][SecperClust] 
               ;mov word ptr [FAT_File_SecPerClust], cx
                mov byte ptr [FAT_File_SecPerClust], cl
                call Rx_DOS_Div32
                ; DX:AX = Cluster seq. number, zero based
                mov word ptr [FAT_File_LC_SN], ax
                mov word ptr [FAT_File_LC_SN]+2, dx
               ;12/09/2010 
                pop ax ; pushed cx
                pop dx ; pushed bx
               ; 11/09/2010
                xor cx, cx  ; 0
                xor bx, bx  ; 0
loc_get_cluster_for_fsector_gnc:
                mov word ptr [FAT_ClusterCounter], cx   
                mov word ptr [FAT_ClusterCounter]+2, bx
                mov word ptr [FAT_File_LCluster], ax
                mov word ptr [FAT_File_LCluster]+2, dx
                call proc_get_next_cluster
                jnc short loc_get_cluster_for_fsector_gnc_next1

               ;xor dx, dx
                or ax, ax
                jnz short loc_get_cluster_for_fsector_stc_retn

loc_get_cluster_for_fsector_check_lc_sn:
               ; 23/10/2010
                mov ax, word ptr [FAT_ClusterCounter]
                mov dx, word ptr [FAT_ClusterCounter]+2
                cmp dx, word ptr [FAT_File_LC_SN]+2
                jb short loc_get_cluster_for_fsector_rs
                cmp ax, word ptr [FAT_File_LC_SN]
                jnb short loc_get_cluster_for_fsector_calc_s_o

loc_get_cluster_for_fsector_rs:
               ; 23/10/2010
                add ax, 1
                adc dx, 0
                mov bx, word ptr [FAT_File_SecPerClust]
                call proc_mul32
               ; 16/07/2011
                ;or bx, bx
                ;jnz short loc_get_cluster_for_fsector_retn_invd
               ; 
                mov cx, ax
                mov bx, dx
                mov dx, word ptr [FAT_File_Sec_SN]
                mov ax, word ptr [FAT_File_Sec_SN]+2
                add dx, 1
                adc ax, 0
                sub dx, cx
                sbb ax, bx
                jz short loc_get_cluster_for_fsector_retn_LClust
               ;jc short loc_get_cluster_for_fsector_retn_invd
                 ; required additional file sectors 
                 ; must not be at the beyond (out) of 64K
                 ; in one stage (for one write request)
                ; 16/07/2011
                cmp ax, 1
                ja short loc_get_cluster_for_fsector_retn_invd
                or dx, dx
                jnz short loc_get_cluster_for_fsector_retn_invd   
                xor ax,ax
                ;
loc_get_cluster_for_fsector_retn_LClust:
                mov cx, word ptr [FAT_File_LCluster]
                mov bx, word ptr [FAT_File_LCluster]+2

               ; 09/10/2010
loc_get_cluster_for_fsector_stc_retn:
                stc
loc_get_cluster_for_fsector_retn:
                mov di, word ptr [FAT_File_SecPerClust]
                retn

loc_get_cluster_for_fsector_retn_invd: 
                mov ax, 0Dh ; Invalid data
                xor dx, dx 
                jmp short loc_get_cluster_for_fsector_stc_retn

loc_get_cluster_for_fsector_gnc_next1:
                mov cx, word ptr [FAT_ClusterCounter]
                mov bx, word ptr [FAT_ClusterCounter]+2
                cmp bx, word ptr [FAT_File_LC_SN]+2
                jb short loc_get_cluster_for_fsector_gnc_next2
                cmp cx, word ptr [FAT_File_LC_SN]
                jnb short loc_get_cluster_for_fsector_no_rs

loc_get_cluster_for_fsector_gnc_next2:                               
                add cx, 1
                adc bx, 0
                jmp loc_get_cluster_for_fsector_gnc

loc_get_cluster_for_fsector_no_rs:
               ; 23/10/2010
                mov ax, cx ; word ptr [FAT_ClusterCounter] 
                mov dx, bx ; word ptr [FAT_ClusterCounter]+2
loc_get_cluster_for_fsector_calc_s_o:
               ; 23/10/2010
                mov bx, word ptr [FAT_File_SecPerClust]
                call proc_mul32

                mov cx, word ptr [FAT_File_Sec_SN]
                mov bx, word ptr [FAT_File_Sec_SN]+2
                sub cx, ax
                sbb bx, dx  
                mov ax, word ptr [FAT_File_LCluster]
                mov dx, word ptr [FAT_File_LCluster]+2

                jmp short loc_get_cluster_for_fsector_retn                   

FAT_File_SecPerClust: dw 0
FAT_File_Sec_SN: dd 0
FAT_File_LCluster: dd 0
FAT_File_LC_SN: dd 0

proc_get_cluster_for_file_sector endp

proc_update_open_FAT_file_size_lmdt proc near
               ; 01/08/2011
               ; 30/10/2010 FAT Dir Entry (Byte) Offset BugFix 
               ; 29/10/2010
               ; 10/09/2010
               ; INPUT -> 
               ;  AX = File handle
               ;  DX:CX = File size
               ;  DS:SI = DOS drv table  
               ;  Last modification date&time -> current date&time
               ; OUTPUT -> DS:SI = Logical DOS drv table addr.
               ;  clc (cf=0) -> updated
               ;  stc (cf=1) -> error code in AX
               ;  AX, BX, CX, DX, will be changed
               ;  ES:BX = Directory entry address

             ; 29/10/2010
             ; cmp byte ptr [SI][LD_FATType], 0
             ; ja short loc_fat_uoflmdt_check_OFDT_sign
 
;loc_fat_uoflmdt_invalid_drive_err:
             ; mov ax, 0Fh ;Invalid drive 
             ; retn

loc_fat_uoflmdt_check_OFDT_sign:
               ; 29/10/2010
                mov es, ax
                cmp byte ptr ES:[OF_Type], 'F'
                je short loc_fat_uoflmdt_get_direntry_addr

loc_fat_uoflmdt_invalid_handle:
                mov ax, 06h ; Invalid handle
                stc
                ret

loc_fat_uoflmdt_get_direntry_addr:
               ; 29/10/2010
                mov al, byte ptr ES:[OF_OFL_SN]
                dec al 
                mov ah, OFL_EntrySize
                mul ah

                mov bx, offset Open_Files_List
                add bx, ax
                push es
                pop ax
                cmp ax, word ptr [BX][OFL_Handle]     ; +6
                jne short loc_fat_uoflmdt_invalid_handle

                mov word ptr [FAT_of_size], cx
                mov word ptr [FAT_of_size]+2, dx

                mov ax, word ptr [BX][OFL_Cluster]
                mov dx, word ptr [BX][OFL_Cluster]+2
                mov word ptr [FAT_of_fcluster], ax
                mov word ptr [FAT_of_fcluster]+2, dx

               ; 30/10/2010 
                mov bx, word ptr ES:[OF_DirEntryOffset]
                mov ax, 32  ; Byte offset = 32 * Entry offset
                mul bx
                mov word ptr [FAT_of_direntrybyteoff], ax
              
                mov ax, word ptr ES:[OF_DirCluster]
                mov dx, word ptr ES:[OF_DirCluster]+2

               ;mov word ptr [FAT_LMDT_ptr], di

                push ds
                pop es

                or ax, ax
                jnz short loc_fat_uoflmdt_load_sub_directory
                or dx, dx
                jnz short loc_fat_uoflmdt_load_sub_directory

loc_fat_uoflmdt_load_root_directory: 
                call proc_load_FAT_root_directory
                jnc short loc_fat_uoflmdt_update_file_size_lm_dt
loc_fat_uoflmdt_retn:
                retn
               
loc_fat_uoflmdt_load_sub_directory:
                call proc_load_FAT_sub_directory
                jc short loc_fat_uoflmdt_retn               

loc_fat_uoflmdt_update_file_size_lm_dt:
                mov es, bx ; Directory buffer segment
                mov bx, word ptr [FAT_of_direntrybyteoff]
                mov ax, word ptr [FAT_of_fcluster]
                mov dx, word ptr [FAT_of_fcluster]+2
                cmp ax, word ptr ES:[BX]+26 ;DirEntry_FstClusLO
                jne short loc_fat_uoflmdt_file_not_found
                cmp dx, word ptr ES:[BX]+20 ;DirEntry_FstClusHI                
                jne short loc_fat_uoflmdt_file_not_found
                mov ax, word ptr [FAT_of_size]
                mov dx, word ptr [FAT_of_size]+2
                mov word ptr ES:[BX]+28, ax ; DirEntry_FileSize, lw
                mov word ptr ES:[BX]+30, dx ; DirEntry_FileSize, hw

                call proc_convert_current_date_time
                mov word ptr ES:[BX]+18, dx ; DirEntry_LstAccDate

               ; 29/10/2010
               ; mov di, word ptr [FAT_LMDT_ptr]
                or di, di
                jz short loc_fat_uoflmdt_write_file_size_lm_dt

                mov ax, word ptr [DI]
                mov dx, word ptr [DI]+2

loc_fat_uoflmdt_write_file_size_lm_dt:
                mov word ptr ES:[BX]+24, dx ; DirEntry_WrtDate
                mov word ptr ES:[BX]+22, ax ; DirEntry_WrtTime
               
               ; 01/08/2011
                mov byte ptr [DirBuff_ValidData], 2 
                call proc_save_directory_buffer
                ; mov es, bx
                mov bx, word ptr [FAT_of_direntrybyteoff]
                retn                

loc_fat_uoflmdt_file_not_found:
                mov ax, 2  ; File not found
                mov dx, offset FAT_of_fcluster
                stc
                retn

FAT_of_size: dd 0
FAT_of_fcluster: dd 0
;FAT_of_dircluster: dd 0
FAT_of_direntrybyteoff: dw 0
; 29/10/2010
;FAT_LMDT_ptr: dw 0

proc_update_open_FAT_file_size_lmdt endp

proc_add_new_cluster proc near
               ;  30/07/2011
               ;  11/09/2010
               ; INPUT ->
               ;  DS:SI = Logical dos drv desc. table address
               ;  DX:AX = Last cluster
               ; OUTPUT ->
               ;  DS:SI = Logical dos drv desc. table address
               ;  DX:AX = New Last cluster (next cluster)
               ;  stc -> error code in AL
               ;  stc -> DX = sectors per cluster
               ;  BX:CX -> Free sectors
               ;  ES=DS
               ;       NOTE:
               ; This procedure does not update lm date&time ! 
               ;

loc_add_new_cluster:               
               mov word ptr [FAT_anc_LCluster], ax
               mov word ptr [FAT_anc_LCluster]+2, dx

               call proc_get_first_free_cluster
               jc short loc_add_new_cluster_retn
               ; DX:AX >=2 and DX:AX < FFFFFFFFh is valid

               mov cx, ax
               mov bx, dx
loc_add_new_cluster_check_ffc_dx:
               or bx, bx
               jz short loc_add_new_cluster_check_ffc_ax
               inc bx
               jnz short loc_add_new_cluster_save_fcc
               inc cx
               jnz short loc_add_new_cluster_save_fcc

loc_add_new_cluster_no_disk_space_retn:
               mov ax, 27h ; MSDOS err => insufficient disk space
loc_add_new_cluster_stc_retn:
               stc
loc_add_new_cluster_retn:
               mov dh, 0
               mov dl, byte ptr [SI][LD_BPB][SecPerClust]
               mov cx, word ptr [SI][LD_FreeSectors]
               mov bx, word ptr [SI][LD_FreeSectors]+2
               retn

loc_add_new_cluster_invalid_format_retn:
               mov ax, 0Bh ; Invalid format
               jmp short loc_add_new_cluster_stc_retn 

loc_add_new_cluster_check_ffc_ax:
              ; 30/07/2011 
               cmp cx, 2
               jb short loc_add_new_cluster_invalid_format_retn

loc_add_new_cluster_save_fcc:  
               mov word ptr [FAT_anc_FFCluster], ax
               mov word ptr [FAT_anc_FFCluster]+2, dx

loc_add_new_cluster_allocate_cluster:
               push ax
               mov di, offset DOSBootSectorBuff
               push ds
               pop es
               mov cx, 256
               xor ax, ax
               rep stosw  ; AX to ES:DI as CX times (DI= DI+2)
               pop ax

               sub ax, 2
               sbb dx, 0
               xor bh, bh
               mov bl, byte ptr [SI][LD_BPB][SecPerCLust]
               mov cx, bx
               call proc_mul32
               or bx, bx
               jnz short loc_add_new_cluster_invalid_format_retn

loc_add_new_cluster_write_nc_to_disk:
               add ax, word ptr [SI][LD_DATAbegin]
               adc dx, word ptr [SI][LD_DATAbegin]+2
              ; 30/07/2011
               jc short loc_add_new_cluster_invalid_format_retn 
               mov bx, offset DOSBootSectorBuff
               ; ES:BX = Sector Buffer
               ; CL = sectors per cluster
               ; CH = 0
loc_add_new_cluster_wnc_to_disk_loop: 
               push cx
              ;mov cx, 1 ; because of buffer size is 512 bytes
               mov cl, 1
               call proc_disk_write
               pop cx
               jc short loc_add_new_cluster_retn
               loop loc_add_new_cluster_wnc_to_disk_loop

loc_add_new_cluster_update_fat_nlc:
               mov ax, word ptr [FAT_anc_FFCluster]
               mov dx, word ptr [FAT_anc_FFCluster]+2
               xor cx, cx
               dec cx ; FFFFh
               mov bx, cx ; FFFFh
               call proc_update_cluster
               jnc short loc_add_new_cluster_update_fat_plc
               or ax, ax ;AX = 0 -> cluster value is 0 or eoc
               jnz short loc_add_new_cluster_stc_retn

loc_add_new_cluster_update_fat_plc:
               mov ax, word ptr [FAT_anc_LCluster]
               mov dx, word ptr [FAT_anc_LCluster]+2
               mov cx, word ptr [FAT_anc_FFCluster]
               mov bx, word ptr [FAT_anc_FFCluster]+2
               call proc_update_cluster
               jnc short loc_add_new_cluster_save_fat_buffer
               or ax, ax ; AX = 0 -> cluster value is 0 or eoc
               jnz loc_add_new_cluster_stc_retn

loc_add_new_cluster_save_fat_buffer:
              ;cmp byte ptr [FAT_BuffValidData], 2
              ;jne short loc_add_new_cluster_calc_FAT_freespace 
              ; Byte ptr [FAT_BuffValidData] =  2 
               call proc_save_fat_buffer
               jc loc_add_new_cluster_retn

loc_add_new_cluster_calc_FAT_freespace:
               xor dx, dx
               mov ax, 1 ; Only one Cluster
               mov bx, 0FF02h ; BH= FF -> DS:SI -> Dos drv desc. table
                              ; BL= 2 -> subtract cluster
               call proc_calculate_FAT_freespace
               ;cx = 0 -> no error, cx > 0 -> error or invalid return
               and cx, cx ; cx = 0 -> valid free sector count
               jz short loc_add_new_cluster_return_cluster_number
loc_add_new_cluster_recalc_FAT_freespace:
               mov bx, 0FF00h
               call proc_calculate_FAT_freespace
               and cx, cx
               jz short loc_add_new_cluster_calc_FAT_freespace

loc_add_new_cluster_return_cluster_number:
               mov cx, ax ; Free sector count, lw
               mov bx, dx ; Free sector count, hw
               mov ax, word ptr [FAT_anc_FFCluster]
               mov dx, word ptr [FAT_anc_FFCluster]+2

               retn 
     
FAT_anc_LCluster: dd 0
FAT_anc_FFCluster: dd 0

proc_add_new_cluster endp

proc_truncate_cluster_chain proc near
               ; 22/01/2011
               ; 02/11/2010
               ; 31/10/2010 cluster count bugfix
               ; 29/10/2010 calculate FAT free space (add) BugFix
               ; 16/10/2010 calculate FAT free space modification
               ; 11/09/2010 -> unlink/truncate cluster chain
               ;  moved from proc_delete_file (in FILE.ASM)
               ;  <- unlink file clusters
               ; INPUT ->
               ;  DS:SI = Logical dos drv desc. table address
               ;  DX:AX = First cluster to be truncated/unlinked 
               ; OUTPUT ->
               ;  DS:SI = Logical dos drv desc. table address
               ;  BX:CX = Count of truncated/removed clusters
               ;  clc -> 
               ;  DX:AX -> Free sectors
               ;  ES = DS
               ;  stc -> error code in AL
               ;    DX = 0
               ; NOTE:
               ; This procedure does not update lm date&time ! 
               ;

loc_truncate_cc:               
               ; 11/09/2010               
                xor cx, cx ; mov cx, 0
                mov bx, cx ; mov bx, 0 
              ; mov byte ptr [FAT_BuffValidData], 0
                mov word ptr [FAT_ClusterCounter], cx ; 0
                mov word ptr [FAT_ClusterCounter]+2,bx ; 0
loc_ttc_unlink_clusters:
		call proc_update_cluster
                ; DX:AX = Next Cluster
                ; BX:CX = Cluster Value
                ;   Note:
                ;   Returns count of unlinked clusters in
                ;   dword ptr FAT_ClusterCounter 
                jnc short loc_ttc_unlink_clusters
pass_tcc_unlink_clusters:
                mov byte ptr [TCC_FATErr], al
                cmp byte ptr [FAT_BuffValidData], 2
                jne short loc_truncate_cc_calculate_FAT_freespace
                call proc_save_fat_buffer
                jnc short loc_truncate_cc_calculate_FAT_freespace
                mov byte ptr [TCC_FATErr], al ; Error
              ; mov byte ptr [FAT_BuffValidData], 0

loc_truncate_cc_calculate_FAT_freespace:
	       ; 16/10/2010
               mov byte ptr [CFS_error], 0
loc_tcc_calculate_FAT_freespace_next:
              ; 31/10/2010 BugFix
                mov ax, word ptr [FAT_ClusterCounter]
                mov dx, word ptr [FAT_ClusterCounter]+2
              ; 02/11/2010 BugFix
               mov bx, 0FF01h ; BH= FF -> DS:SI -> Dos drv desc. table
                              ; BL= 1 -> add  cluster
               call proc_calculate_FAT_freespace
               and cx, cx ; cx = 0 -> valid free sector count
               jz short pass_truncate_cc_recalc_FAT_freespace
               cmp byte ptr [CFS_error], 0
               ja short loc_tcc_calculate_FAT_freespace_err

loc_tcc_recalculate_FAT_freespace:
              ; 16/10/2010 
               inc byte ptr [CFS_error]
              ; 21/08/2010
               mov bx, 0FF00h
               call proc_calculate_FAT_freespace
              ; 16/10/2010
               and cx, cx
               jz short loc_tcc_calculate_FAT_freespace_next
              
loc_tcc_calculate_FAT_freespace_err:
              ; 16/10/2010
pass_truncate_cc_recalc_FAT_freespace:
               ; 11/09/2010
                mov cx, word ptr [FAT_ClusterCounter]
                mov bx, word ptr [FAT_ClusterCounter]+2
               ;
                cmp byte ptr [TCC_FATErr], 0
                jna short loc_tcc_unlink_clusters_retn
loc_tcc_unlink_clusters_error:
                xor ah, ah
                mov al, byte ptr [TCC_FATErr]
                xor dx, dx
                stc
loc_tcc_unlink_clusters_retn:
                retn

; 04/04/2010
;DelFile_FATErr: db 0
TCC_FATErr: db 0
; 16/10/2010
CFS_Error: db 0

proc_truncate_cluster_chain endp

proc_truncate_FAT_file proc near
 	       ; 30/07/2011 dword ptr [DI] return
               ; 25/01/2011 BugFix 
               ; 22/01/2011 
               ; 12/09/2010
               ; 11/09/2010
               ; INPUT ->
               ;  DS:SI = Logical dos drv desc. table address
               ;  DX:AX = File size in bytes
               ;  BX:CX = First cluster of the file
               ; OUTPUT ->
               ;  DS:SI = Logical dos drv desc. table address
               ;  BX:CX = Count of truncated/removed clusters
               ;  clc -> successfull 
               ;   (BX:CX = 0 -> First cluster is not unlinked)
               ;  DX:AX -> Free sectors
               ;  (ES = DS or ES same with input)
               ;    DI = (New) Last Cluster pointer (dd) 
               ;  stc -> error code in AL
               ;    DH = FS Type, DL = FAT Type
               ;    DI = 0 
               ; NOTE:
               ; This procedure does not update lm date&time !

              ;cmp byte ptr [SI][LD_FATType], 0
              ;jna short loc_truncate_file_invalid_drive

               mov word ptr [FAT_File_LCluster], cx
               mov word ptr [FAT_File_LCluster]+2, bx
              
               add ax, 511
               adc dx, 0
               
              ;mov cx, word ptr [SI][LD_BPB][BytesPerSec]
               mov cx, 512
               call Rx_DOS_Div32
               ; DX:AX = count of file sectors 

               xor ch, ch
               mov cl, byte ptr [SI][LD_BPB][SecperClust] 
               ;mov word ptr [FAT_File_SecPerClust], cx
               mov byte ptr [FAT_File_SecPerClust], cl
             
               ; 25/01/2011
loc_tff_get_cluster_number:
               ; DX:AX = File Sectors
               ; CX = Sectors per cluster
               call Rx_DOS_Div32
               or bx, bx
               jz short loc_tff_get_clnum_pass_round_up
               add ax, 1
               adc dx, 0
loc_tff_get_clnum_pass_round_up:
               mov word ptr [FAT_ClusterCounter], ax   
               mov word ptr [FAT_ClusterCounter]+2, dx
               mov ax, word ptr [FAT_File_LCluster]
               mov dx, word ptr [FAT_File_LCluster]+2
loc_tff_get_cluster_gnc:
               call proc_get_next_cluster
               jnc short loc_tff_get_cluster_gnc_next

              ; 25/01/2011
               xor cx, cx
               xor bx, bx

               or ax, ax
               jnz short loc_tff_truncated_stc_retn

loc_tff_not_truncated:
               mov ax, word ptr [SI][LD_FreeSectors]
               mov dx, word ptr [SI][LD_FreeSectors]+2
 	      ; 30/07/2011
               jmp short loc_truncate_fat_file_retn

;loc_truncate_file_invalid_drive:
;              mov ax, 0Fh ; Invalid drive

loc_tff_truncated_stc_retn:
               mov dx, word ptr [SI][LD_FATType]
               ; dh = FS type, dl = FAT type
               xor di, di ; 30/07/2011
               stc
               retn                

loc_tff_get_cluster_gnc_next:
               sub word ptr [FAT_ClusterCounter], 1
               sbb word ptr [FAT_ClusterCounter]+2, 0
               mov cx, word ptr [FAT_ClusterCounter]
               mov bx, word ptr [FAT_ClusterCounter]+2
               or cx, bx
               jz short loc_tff_truncate_cluster_chain
               mov word ptr [FAT_File_LCluster], ax
               mov word ptr [FAT_File_LCluster]+2, dx
               jmp short loc_tff_get_cluster_gnc

loc_tff_truncate_cluster_chain:
               call proc_truncate_cluster_chain
               jc short loc_tff_truncated_stc_retn

loc_truncate_fat_file_retn:
              ; 30/07/2011 
               mov di, offset FAT_File_LCluster
               
               retn 

proc_truncate_FAT_file endp


FAT_BuffDescriptor:
FAT_CurrentCluster: dd 0
FAT_BuffValidData: db 0
FAT_BuffDrvName: db 0
FAT_BuffSector: dd 0
FAT_Buffer: dw 0

Dir_BuffDescriptor:
DirBuff_DRV: db 0
DirBuff_FATType: db 0
DirBuff_ValidData: db 0
DirBuff_CurrentEntry: dw 0
DirBuff_LastEntry: dw 0
DirBuff_Cluster: dd 0 
DirBuffer_Size: dw 0
Directory_Buffer: dw 0
DirBuff_EntryCounter: dw 0

; 28/02/2010
FAT_ClusterCounter: dd 0
LastCluster: dd 0
