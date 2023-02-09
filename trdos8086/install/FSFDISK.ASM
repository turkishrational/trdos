; *****************************************************************************
; FSFDISK.ASM  [ SINGLIX FIXED DISK PARTITIONING Utility ]
; Copyright (c) 2009  Erdogan TAN  27/04/2009 [ 23/04/2009 - 28/03/2009 ]
; *****************************************************************************

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

; BIOS Disk Parameters
DPDiskNumber  equ 0h
DPDType       equ 1h
DPReturn      equ 2h
DPHeads       equ 3h
DPCylinders   equ 4h
DPSecPerTrack equ 6h
DPDisks       equ 7h
DPTableOff    equ 8h
DPTableSeg    equ 0Ah
DPNumOfSecs   equ 0Ch

; BIOS INT 13h Extensions (LBA extensions)
; Just After DP Data (DPDiskNumber+)
DAP_PacketSize equ 10h  ; If extensions present, this byte will be >=10h
DAP_Reserved1 equ 11h   ; Reserved Byte 
DAP_NumOfBlocks equ 12h ; Value of this byte must be 0 to 127
DAP_Reserved2 equ 13h   ; Reserved Byte
DAP_Destination equ 14h ; Address of Transfer Buffer as SEGMENT:OFFSET
DAP_LBA_Address equ 18h ; LBA=(C1*H0+H1)*S0+S1-1
                        ; C1= Selected Cylinder Number
                        ; H0= Number Of Heads (Maximum Head Number + 1)
                        ; H1= Selected Head Number
                        ; S0= Maximum Sector Number
                        ; S1= Selected Sector Number
                        ; QUAD WORD
; DAP_Flat_Destination equ 20h ; 64 bit address, if value in 4h is FFFF:FFFFh
                             ; QUAD WORD (Also, value in 0h must be 18h) 
                             ; TR-DOS will not use 64 bit Flat Address

; INT 13h Function 48h "Get Enhanced Disk Drive Parameters"
; Just After DP Data (DPDiskNumber+)
GetDParams_48h equ 20h ; Word. Data Lenght, must be 26 (1Ah) for short data.
GDP_48h_InfoFlag equ 22h ; Word
; Bit 1 = 1 -> The geometry returned in bytes 4-15 is valid.
GDP_48h_NumOfPCyls equ 24h ; Double Word. Number physical cylinders.
GDP_48h_NumOfPHeads equ 28h ; Double Word. Number of physical heads.
GDP_48h_NumOfPSpT equ 2Ch ; Double word. Num of physical sectors per track.
GDP_48h_LBA_Sectors equ 30h ; 8 bytes. Number of physical/LBA sectors.
GDP_48h_BytesPerSec equ 38h ; Word. Number of bytes in a sector.

; Cursor Location
CCCpointer equ  0450h   ; BIOS data, current cursor column


Present segment Para 'code'

                assume CS:Present, DS:Present, ES:Present, SS:Present


;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
;±
;±              PROCEDURE proc_start
;±
;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±

proc_start      proc    far

                org 100h
start:
                call proc_drv_init  ; DRV_INIT0.ASM
loc_conv_ptotalsectors:
                mov si, offset DiskParams
                mov al, byte ptr [Current_Drive]
                mov ah, 40h
                mul ah
                add si, ax  
                mov ax, word ptr [SI][GDP_48h_LBA_Sectors]
                mov dx, word ptr [SI][GDP_48h_LBA_Sectors]+2
                mov word ptr [TotalSectors], ax
                mov word ptr [TotalSectors]+2, dx
                mov si, offset str_Total_Sectors 
                add si, 9
                mov cx, 10
loc_rediv_ptotalsectors:
                call Rx_Dos_Div32
                add bl,'0'
                mov byte ptr [SI], bl
                cmp si, offset str_Total_Sectors
                jna short loc_check_hd_partitions
                dec si
                and ax, ax
                jnz short loc_rediv_ptotalsectors
                and dx, dx
                jnz short loc_rediv_ptotalsectors
loc_check_hd_partitions:
                mov si, offset PTable_hd0
                mov al, byte ptr [Current_Drive]
                mov ah, 40h
                mul ah
                add si, ax  
                mov cx, 4
check_next_p_entry:
                cmp byte ptr [SI][ptFileSystemName],0
                jna short pass_inc_numberofpartitions
                inc byte ptr [NumberOfPartitions]
                jmp short pass_record_ff_partition 
pass_inc_numberofpartitions:
                cmp byte ptr [FF_Partition], 0
                ja short pass_record_ff_partition
                mov al, 5
                sub al, cl 
                mov byte ptr [FF_Partition], al
pass_record_ff_partition:
                add si, 10h
                loop check_next_p_entry

              ; cmp byte ptr [NumberOfPartitions], 0
              ; jna short loc_print_fdisk_options

loc_print_ptable:                
                mov al, byte ptr [Current_Drive]
                call proc_print_partition_table  

loc_print_fdisk_options:
                mov si, offset Print_TotalSectors
                call proc_printmsg
                mov si, offset FDISK_Options
                call proc_printmsg

                mov byte ptr [GetChar], 0

loc_loop_getchar:
                xor ah, ah              ; "Press any key to continue"
                int 16h

                cmp al, 1Bh
                je end_of_prog_fs_fdisk

                cmp al, 0Dh
                je short pass_getchar_enter
                
                cmp al, "1"
                jb short pass_valid_command_input

                cmp al, "5"
                ja short pass_valid_command_input

                mov byte ptr [GetChar], al

                mov ah, 09h
                mov cx, 1
                int 10h

                jmp short pass_valid_command_input 

pass_getchar_enter:
                mov al, byte ptr [GetChar]
               
		cmp al, "5"
                je short loc_change_drive
                
                cmp al, "2"
                je loc_set_active_partition

                cmp al, "3"
                je loc_delete_partition

                cmp al, "1"
                je loc_create_singlix_partition

                cmp al, "4"
                je loc_write_partition_table
               
pass_valid_command_input:
                cmp al, "?"
                je loc_write_program_version     
               
                mov bx, 7
                mov al, 7            
                mov ah, 0Eh                  
                int 10h

                jmp short loc_loop_getchar

loc_display_ptable:
                ; call proc_clear_screen
                mov si, offset msg_crlf
                call proc_printmsg
              
                jmp short loc_print_ptable

loc_change_drive:
                mov si, offset msg_crlf
                call proc_printmsg
                mov al, byte ptr [Current_Drive]
                mov byte ptr [GetChar], al
                add byte ptr [GetChar], "0"    
                call proc_print_partition_table 
                mov si, offset Print_TotalSectors
                call proc_printmsg
               
                mov si, offset msg_FDisk_Drive_No
                call proc_printmsg

loc_loop_getchar_chdrv:
                xor ah, ah              ; "Press any key to continue"
                int 16h

                cmp al, 1Bh
                je short loc_display_ptable

                cmp al, 0Dh
                je short pass_getchar_enter_chdrv

                cmp al, "0"
                jb short loc_invalid_drv_no

                cmp al, "3"
                ja short loc_invalid_drv_no

                mov byte ptr [GetChar], al

                mov ah, 09h
                mov cx, 1
                int 10h

                jmp short loc_invalid_drv_no

pass_getchar_enter_chdrv:
                mov al, byte ptr [GetChar]  
                sub al, "0"
                cmp al, byte ptr [Current_Drive]
                jne short loc_cont_chg_cdrive 

loc_invalid_drv_no:                
                mov bx, 7
                mov al, 7            
                mov ah, 0Eh                  
                int 10h

                jmp short loc_loop_getchar_chdrv

loc_cont_chg_cdrive:
                push ax
                mov ah, 40h
                mul ah
                mov si, offset Disk_hd0
                add si, ax
                pop ax                   
                cmp byte ptr [SI]+1, 03h
                jne short loc_drv_not_ready
                
                mov byte ptr [Current_Drive], al
             
                mov byte ptr [NumberOfPartitions], 0
                mov byte ptr [FF_Partition], 0

                mov si, offset msg_crlf
                call proc_printmsg

                jmp loc_conv_ptotalsectors 
                                                  
loc_drv_not_ready:
                add al, "0"
                mov byte ptr [NotReadyDrv], al  
                mov si, offset msg_DrvNotReady
                call proc_printmsg
                xor ah, ah
                int 16h   
                jmp loc_print_ptable                  

loc_set_active_partition:
                cmp byte ptr [NumberOfPartitions], 0
                jna pass_valid_command_input 
                mov si, offset msg_crlf
                call proc_printmsg
                mov al, byte ptr [Current_Drive]
                call proc_print_partition_table  
                mov si, msg_Enter_Active_Partition_No
                call proc_printmsg

loc_loop_getchar_set_acpart:

                xor ah, ah              ; "Press any key to continue"
                int 16h

                cmp al, 1Bh
                je loc_display_ptable

                cmp al, 0Dh
                je short pass_getchar_enter_set_acpart
                
                cmp al, "1"
                jb short loc_invalid_part_no

                cmp al, "4"
                ja short loc_invalid_part_no

                mov byte ptr [GetChar], al

                mov ah, 09h
                mov cx, 1
                int 10h

                mov bx, 7
                mov al, 7            
                mov ah, 0Eh                  
                int 10h

                jmp short loc_loop_getchar_set_acpart

pass_getchar_enter_set_acpart:
                mov si, offset PTable_hd0
                mov al, byte ptr [Current_Drive]
                mov ah, 40h
                mul ah
                add si, ax 
                push si  
                mov al, byte ptr [GetChar]  
                sub al, "1"
                mov ah, 10h
                mul ah
                add si, ax
                cmp byte ptr [SI][ptFileSystemName], 0
                jna short loc_invalid_part_no_si
                pop ax
                push si
                mov si, ax    
                mov cx, 4
loc_reset_active_partition:
                mov byte ptr [SI], 0
                add si, 10h
                loop loc_reset_active_partition
                pop si
                mov byte ptr [SI], 80h

                jmp loc_display_ptable

loc_invalid_part_no_si:
                pop si 
loc_invalid_part_no:                
                mov bx, 7
                mov al, 7            
                mov ah, 0Eh                  
                int 10h

                jmp short loc_loop_getchar_set_acpart

loc_delete_partition:
                cmp byte ptr [NumberOfPartitions], 0
                jna pass_valid_command_input
                cmp word ptr [TotalSectors]+2, 0
                jna pass_valid_command_input
                mov si, offset msg_crlf
                call proc_printmsg
                mov al, byte ptr [Current_Drive]
                call proc_print_partition_table  
                mov si, offset msg_Delete_Partition_No
                call proc_printmsg

loc_loop_getchar_del_part:

                xor ah, ah              ; "Press any key to continue"
                int 16h

                cmp al, 1Bh
                je loc_display_ptable

                cmp al, 0Dh
                je short pass_getchar_enter_del_part
                
                cmp al, "1"
                jb short loc_invalid_del_part_no

                cmp al, "4"
                ja short loc_invalid_del_part_no

                mov byte ptr [GetChar], al

                mov ah, 09h
                mov cx, 1
                int 10h

loc_invalid_del_part_no:                
                mov bx, 7
                mov al, 7            
                mov ah, 0Eh                  
                int 10h

                jmp short loc_loop_getchar_del_part

pass_getchar_enter_del_part:
                mov si, offset PTable_hd0
                mov al, byte ptr [Current_Drive]
                mov ah, 40h
                mul ah
                add si, ax 
                mov al, byte ptr [GetChar]  
                sub al, "1"
                mov ah, 10h
                mul ah
                add si, ax
                
                cmp byte ptr [SI][ptFileSystemName], 0
                jna short loc_invalid_del_part_no

                mov al, byte ptr [GetChar]
                mov byte ptr [str_d_pn], al
   
                push si
                mov si, msg_Do_You_Want_to_Delete
                call proc_printmsg
                pop si

loc_delete_partition_yes_no:                    
                xor ah, ah              ; "Press any key to continue"
                int 16h

                cmp al, 1Bh
                je loc_display_ptable

                and al, 0DFh

                cmp al, 'N'
                jne short loc_check_delpart_yes

               ; push ax

                mov ah, 09h
                mov cx, 1
                int 10h      

                ; pop ax 

                 jmp loc_display_ptable
                  
loc_check_delpart_yes:

                cmp al, "Y"
                jne short loc_delete_partition_yes_no_beep          

               ; push ax

                mov ah, 09h
                mov cx, 1
                int 10h      

                ; pop ax      

                mov cx, 8
loc_reset_partition_entry:
                mov word ptr [SI], 0
                inc si
                inc si 
                loop loc_reset_partition_entry                

                mov si, offset msg_crlf
                call proc_printmsg
                
              ; mov al, byte ptr [NumberOfPartitions]
                mov byte ptr [NumberOfPartitions], 0
                mov byte ptr [FF_Partition], 0
              ; cmp al, 1
              ; ja loc_check_hd_partitions  

              ; mov bx, 7
              ; mov al, 7            
              ; mov ah, 0Eh                  
              ; int 10h
               
                jmp loc_check_hd_partitions

loc_delete_partition_yes_no_beep:
  
                mov bx, 7
                mov al, 7            
                mov ah, 0Eh                  
                int 10h
                
                jmp short loc_delete_partition_yes_no 

                  
loc_create_singlix_partition:
                mov si, offset msg_crlf
                call proc_printmsg
                mov al, byte ptr [Current_Drive]
                call proc_print_partition_table
                mov si, offset msg_Create_Singlix_Partition
                call proc_printmsg  
               
                mov al, byte ptr [NumberOfPartitions]
                cmp al, 3
                ja loc_no_free_entry

                cmp word ptr [TotalSectors]+2, 0
                jna loc_no_space

;loc_pte_check_no_return_here:
                mov word ptr [BeginningSector], 1
                mov word ptr [BeginningSector]+2, 0

                cmp al, 0
                ja short pass_first_ptable_entry
                ; mov ax, 1
                ; mov dx, 0               
                ; xor dx, dx
                           
                mov ax, word ptr [TotalSectors]
                mov dx, word ptr [TotalSectors]+2
                sub ax, 1
                sbb dx, 0 
                mov word ptr [SectorCount], ax
                mov word ptr [SectorCount]+2, dx
                
                jmp loc_msg_enter_beginning_sector   

pass_first_ptable_entry:
                mov al, byte ptr [Current_Drive]
                mov ah, 40h
                mul ah
                mov si, offset PTable_hd0
                add si, ax
                mov di, si
                xor bl, bl 
                mov cx, 4
loc_push_pte_bs_sc:
                mov al, byte ptr [SI][ptFileSystemName]               
                cmp al, 0
                ; jna short loc_next_pte_0
                jna short pass_push_pte_bs_sc_1
                mov ax, word ptr [SI][ptStartSector]  
                mov dx, word ptr [SI][ptStartSector]+2
                cmp dx, 0
                jna short pass_push_pte_bs_sc_0    
                sub ax, 1
                sbb dx, 0
                inc bl
                push dx ; ending sector hw
                push ax ; ending sector lw
                push word ptr [BeginningSector]+2
                push word ptr [BeginningSector]
pass_push_pte_bs_sc_0:
                mov ax, word ptr [SI][ptStartSector]  
                mov dx, word ptr [SI][ptStartSector]+2
                add ax, word ptr [SI][ptSectors]
                adc dx, word ptr [SI][ptSectors]+2                
                cmp dx, word ptr [TotalSectors]+2
                jnb short pass_push_pte_bs_sc_1
                inc bl
                mov word ptr [BeginningSector], ax
                mov word ptr [BeginningSector]+2, dx
                mov ax, word ptr [TotalSectors]
                mov dx, word ptr [TotalSectors]+2 
                sub ax, 1
                sbb dx, 0
                push dx ; ending sector hw
                push ax ; ending sector lw
                push word ptr [BeginningSector]+2
                push word ptr [BeginningSector]
                add ax, 1
                adc dx, 0
pass_push_pte_bs_sc_1:   
                add si, 10h             
                loop loc_push_pte_bs_sc
                ;cmp bl, 0
                ;jna short loc_pte_check_no_return_here
loc_check_pte_fspace:
                mov si, di
                mov cx, 4
                pop word ptr [BeginningSector]
                pop word ptr [BeginningSector]+2
                pop word ptr [EndingSector]
                pop word ptr [EndingSector]+2
loc_check_pte_fspace_next:
                mov al, byte ptr [SI][ptFileSystemName]               
                cmp al, 0
                jna short pass_check_pte_fspace_next
                
                mov ax, word ptr [SI][ptStartSector]
                mov dx, word ptr [SI][ptStartSector]+2
                sub ax, word ptr [BeginningSector]
                sbb dx, word ptr [BeginningSector]+2
                jb short loc_pte_bsc_es  ; The beginning sector greater than the ptStartSector, 
                                         ; also must be greater than the ptEndingSector !
                cmp dx, 0
                jna short loc_pte_bsc_bs_failed  ; Partition size can not be less than 32MB !
                mov ax, word ptr [EndingSector]
                mov dx, word ptr [EndingSector]+2                 
                sub ax, word ptr [SI][ptStartSector]
                sbb dx, word ptr [SI][ptStartSector]+2
                jb short pass_check_pte_fspace_next ; The ending sector less than the ptStartSector, it is proper !
                mov ax, word ptr [SI][ptStartSector]
                mov dx, word ptr [SI][ptStartSector]+2
                sub ax, 1
                sbb dx, 0
                mov word ptr [EndingSector], ax
                mov word ptr [EndingSector]+2, dx
                
pass_check_pte_fspace_next:
                add si, 10h
                loop loc_check_pte_fspace_next
                
                mov ax, word ptr [EndingSector]
                mov dx, word ptr [EndingSector]+2
                sub ax, word ptr [BeginningSector]
                sbb dx, word ptr [BeginningSector]+2
                add ax, 1
                adc dx, 0 
                mov word ptr [SectorCount], ax
                mov word ptr [SectorCount]+2, dx  

loc_pte_push_pop_reset:                
                sub bl, 1
                jna short loc_msg_enter_beginning_sector
                pop ax
                pop dx
                pop ax
                pop dx       
                jmp short loc_pte_push_pop_reset
                 
loc_pte_bsc_es:
                mov ax, word ptr [BeginningSector]
                mov dx, word ptr [BeginningSector]+2
                sub ax, word ptr [SI][ptStartSector]
                sbb dx, word ptr [SI][ptStartSector]+2 
                sub ax, word ptr [SI][ptSectors]
                sbb dx, word ptr [SI][ptSectors]+2    
                jnb short pass_check_pte_fspace_next

loc_pte_bsc_bs_failed:
                sub bl, 1
                jna loc_no_space ; All partition space combinations are failed ! 
                jmp loc_check_pte_fspace
                   
loc_msg_enter_beginning_sector:

                mov si, offset msg_Enter_Begin_Sector
                call proc_printmsg

                mov ax, word ptr [BeginningSector]
                mov dx, word ptr [BeginningSector]+2
                
                call proc_lba_sector_input

                jc loc_display_ptable  ; Esc key pressed ? carry is yes

loc_begsec_ax_change_check: 
                cmp ax, word ptr [BeginningSector]
                jne short loc_begsec_limit_overlap_check
loc_begsec_dx_change_check:
                cmp dx, word ptr [BeginningSector]+2
                je pass_begsec_limit_overlap_check

loc_begsec_limit_overlap_check:
                
                push dx
                push ax

                or ax, dx
                jz short loc_csp_sec_boundary_check_failed ; masterboot overlap !
                
                pop bx
                push bx 
                mov cx, dx
                mov ax, word ptr [TotalSectors]
                mov dx, word ptr [TotalSectors]+2
                sub ax, bx
                sbb dx, cx
                jc short loc_csp_sec_outofspace ; out of total space !
                cmp dx, 1
                jb short loc_csp_sec_outofspace ; out of total space ! < 32MB
                mov word ptr [SectorCount], ax
                mov word ptr [SectorCount]+2, dx

                cmp byte ptr [NumberOfPartitions], 0
                jna short pass_partition_begsec_overlap_check
                                
loc_partition_begsec_overlap_check:
                mov al, byte ptr [Current_Drive]
                mov ah, 40h
                mul ah
                mov si, offset PTable_hd0
                add si, ax
                mov cx, 4
loc_begsec_overlap_next_check:                
                mov bl, byte ptr [SI][ptFileSystemName]               
                cmp bl, 0
                jna short loc_begsec_overlap_next_check_1
                pop ax
                pop dx
                push dx
                push ax 
                sub ax, word ptr [SI][ptStartSector]
                sbb dx, word ptr [SI][ptStartSector]+2
                jb short loc_begsec_overlap_next_check_2
                je short loc_csp_sec_boundary_check_failed   
                sub ax, word ptr [SI][ptSectors]
                sbb dx, word ptr [SI][ptSectors]+2
                jna loc_csp_sec_boundary_check_failed      
               
loc_begsec_overlap_next_check_1:
                add si, 10h 
                loop loc_begsec_overlap_next_check
                jmp short pass_partition_begsec_overlap_check

loc_csp_sec_outofspace_popcx:
                pop cx
loc_csp_sec_outofspace:
                pop ax
                pop dx
               
                mov si, offset msg_out_of_space
                call proc_printmsg
                
                jmp loc_singlix_partition_created
                 
loc_csp_sec_boundary_check_failed:
                pop ax
                pop dx
                mov si, offset msg_boundary_check_failed
                call proc_printmsg
                
                jmp loc_singlix_partition_created

loc_begsec_overlap_next_check_2:
                pop ax
                pop dx
                push dx
                push ax
                push cx 
                mov bx, word ptr [SI][ptStartSector]
                mov cx, word ptr [SI][ptStartSector]+2
                sub bx, ax
                sbb cx, dx 
                cmp cx, 0
                jna short loc_csp_sec_outofspace_popcx
                mov word ptr word ptr [SectorCount], bx
                mov word ptr word ptr [SectorCount]+2, cx
                pop cx
                jmp short loc_begsec_overlap_next_check_1

pass_partition_begsec_overlap_check:

                pop ax
                pop dx

                mov word ptr [BeginningSector], ax
                mov word ptr [BeginningSector]+2, dx

pass_begsec_limit_overlap_check: 

                mov si, offset msg_Enter_Sector_Count
                call proc_printmsg

                mov ax, word ptr [SectorCount]
                mov dx, word ptr [SectorCount]+2

                call proc_lba_sector_input

                jc  loc_display_ptable ; Esc key pressed ? carry is yes

loc_seccount_ax_change_check: 
                cmp ax, word ptr [SectorCount]
                jne short loc_seccount_limit_overlap_check
loc_seccount_dx_change_check:
                cmp dx, word ptr [SectorCount]+2
                je short pass_seccount_limit_overlap_check

loc_seccount_limit_overlap_check:
                
                push dx
                push ax

                cmp dx, 0
                jna short loc_csp_sec_outofspace ; < 32MB             
                
                pop bx
                push bx 
                mov cx, dx
                add bx, word ptr [BeginningSector]
                adc cx, word ptr [BeginningSector]+2
                mov ax, word ptr [TotalSectors]
                mov dx, word ptr [TotalSectors]+2
                sub ax, bx
                sbb dx, cx
                jc short loc_csp_sec_outofspace ; out of total space !
                
                cmp byte ptr [NumberOfPartitions], 0
                jna short pass_partition_seccount_overlap_check
                                
loc_partition_seccount_overlap_check:
                mov al, byte ptr [Current_Drive]
                mov ah, 40h
                mul ah
                mov si, offset PTable_hd0
                add si, ax
                push cx
                push bx
                mov cx, 4
loc_seccount_overlap_next_check:
                pop ax
                pop dx
                push dx
                push ax  
                mov bl, byte ptr [SI][ptFileSystemName]               
                cmp bl, 0
                jna short loc_seccount_overlap_next_check_1
                sub ax, word ptr [SI][ptStartSector]
                sbb dx, word ptr [SI][ptStartSector]+2
                jb short loc_seccount_overlap_next_check_1
                ja short loc_seccount_overlap_next_check_upper
                cmp ax, 0 
                jna short loc_seccount_overlap_next_check_1   
loc_seccount_overlap_next_check_upper:
                sub ax, word ptr [SI][ptSectors]
                sbb dx, word ptr [SI][ptSectors]+2
                jb short loc_csp_seccount_boundary_check_failed
                ja short loc_seccount_overlap_next_check_1
                cmp ax, 0 
                ja short loc_seccount_overlap_next_check_1   
loc_csp_seccount_boundary_check_failed:
                pop ax
                pop dx  
                jmp loc_csp_sec_boundary_check_failed      
               
loc_seccount_overlap_next_check_1:
                add si, 10h 
                loop loc_seccount_overlap_next_check
                pop ax
                pop dx

pass_partition_seccount_overlap_check:

                pop ax
                pop dx

                mov word ptr [SectorCount], ax
                mov word ptr [SectorCount]+2, dx

pass_seccount_limit_overlap_check:

                call proc_get_chs_translated
                
                mov di, offset PTable_hd0
                mov al, byte ptr [Current_Drive]
                mov ah, 40h
                mul ah
                add di, ax
                mov al, byte ptr [FF_Partition]
                dec al
                mov ah, 10h
                mul ah
                add di, ax
                mov si, offset FS_Partition_Entry
                mov cx, 8
                rep movsw 

                mov si, offset msg_Singlix_Partition_Created
                call proc_printmsg

loc_singlix_partition_created:                
                mov si, offset msg_Press_Any_Key
                call proc_printmsg
                
                xor ah, ah              ; "Press any key to continue"
                int 16h

pass_create_singlix_partition:
                mov si, offset msg_crlf
                call proc_printmsg

                mov byte ptr [NumberOfPartitions], 0
                mov byte ptr [FF_Partition], 0
              
                jmp loc_check_hd_partitions   

loc_no_free_entry:
loc_no_space:
                mov si, offset msg_No_Space
                call proc_printmsg

                jmp short loc_singlix_partition_created

loc_write_partition_table:
                ; [26/04/2009]
                mov dl, byte ptr [Current_Drive]
                add dl, 80h
                call proc_load_masterboot
                jc loc_masterboot_read_error
                cmp word ptr [MBIDCode],0AA55h
                je short pass_change_masterboot_code
loc_writing_fs_mbr:
                mov si, offset msg_writing_ptable
                call proc_printmsg
                mov si, Offset FS_MBR
                mov di, offset MasterBootBuff
                mov cx, 100h
                rep movsw
                jmp short loc_move_pt_to_mb_buffer
pass_change_masterboot_code:
                cmp byte ptr [NumberOfPartitions], 1
                ja short loc_show_csp_options
                jb short loc_writing_fs_mbr
                mov al, byte ptr [Current_Drive]
                mov ah, 40h
                mul ah
                mov si, offset PTable_hd0
                add si, ax
                mov cx, 4
loc_loop_check_pte_0A1h:                  
                mov al, byte ptr [SI][ptFileSystemName]
                cmp al, 0A1h
                je short loc_writing_fs_mbr
                add si, 10h
                loop loc_loop_check_pte_0A1h 

loc_show_csp_options:
                mov si, offset msg_write_masterboot_sector
                call proc_printmsg

loc_loop_getchar_csp_ops:

                xor ah, ah              
                int 16h

                cmp al, 1Bh
                je loc_display_ptable

                cmp al, 0Dh
                je short pass_getchar_enter_csp_ops
                
                cmp al, "1"
                jb short loc_invalid_csp_option

                cmp al, "2"
                ja short loc_invalid_csp_option

                mov byte ptr [GetChar], al

                mov ah, 09h
                mov cx, 1
                int 10h

loc_invalid_csp_option:                
                mov bx, 7
                mov al, 7            
                mov ah, 0Eh                  
                int 10h

                jmp short loc_loop_getchar_csp_ops

pass_getchar_enter_csp_ops:
 
                mov al, byte ptr [GetChar]
                cmp al, "2"
                je short loc_writing_fs_mbr 

                mov si, offset msg_writing_ptable
                call proc_printmsg                               

loc_move_pt_to_mb_buffer:              
                mov al, byte ptr [Current_Drive]
                mov ah, 40h
                mul ah
                mov si, offset PTable_hd0
                add si, ax
                mov di, offset PartitionTable
                mov cx, 20h
                rep movsw

                mov dl, byte ptr [Current_Drive]
                add dl, 80h

                ; dl = drive number
                mov bx, offset MasterBootBuff
                mov ax,0301h
                mov cx,1
                xor dh,dh
                int 13h
                jc short loc_masterboot_write_error

                mov si, offset msg_OK
                call proc_printmsg

                jmp loc_singlix_partition_created 

loc_masterboot_read_error:
                mov si, offset Read_Error
                call proc_printmsg
                jmp loc_singlix_partition_created 

loc_masterboot_write_error:
                mov si, offset Write_Error
                call proc_printmsg
                jmp loc_singlix_partition_created

loc_write_program_version:
                call proc_clear_screen
                mov si, offset msg_fsfdisk_version
                call proc_printmsg

                jmp loc_singlix_partition_created

end_of_prog_fs_fdisk:

                int 20h

                xor ah, ah              ; "Press any key to continue"
                int 16h                

                int 19h 
     
proc_start      endp


proc_printmsg   proc near

                mov BX, 07
loc_print:
                lodsb                           ; Load byte at DS:SI to AL
                and     AL,AL            
                je      short loc_return        ; If AL = 00h then return
                mov     AH,0Eh                  
                int     10h                     ; BIOS Service func ( ah ) = 0Eh
                                                ; Write char as TTY
                                                ;AL-char BH-page BL-color
                jmp     short loc_print           
loc_return:
                retn

proc_printmsg   endp


proc_clear_screen proc near

                mov ah, 0Fh 
                int 10h
                mov ah, 0
                int 10h

                retn

proc_clear_screen endp

;'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''';
; Rx_DOS  32 bit Divide                                      ;
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

Rx_Dos_Div32    proc near
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

div32_retn:
                retn

Rx_Dos_Div32    endp

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

;'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''';
; 32 bit Multiply                                            ;
;- - - - - - - - - - - - - - - - - - - - - - - - - -- - - - -;
;                                                            ;
; input -> DX_AX = 32 bit multiplier                         ;
; input -> BX = 16 bit number to be multiplied by DX_AX      ;
; output -> BX_DX_AX = 48 bit (16+32 bit) result number      ;
;                                                            ;
; (c) Erdogan TAN  1999                                      ;
;............................................................;

proc_mul32    proc near

              push cx

              mov cx, bx
              mov bx, dx

              mul cx

              xchg ax, bx

              push dx

              mul cx 

              pop cx 

              add ax, cx 
              adc dx, 0

              xchg bx, ax
              xchg dx, bx

              pop cx

              retn

proc_mul32    endp

proc_print_partition_table proc near
                ; Input al: hard disk number 
                ; hd0 = 0
                ; hd1 = 1
                ; hd2 = 2
                ; hd3 = 3 
                
                push ax 
                mov si, offset P_Table_Header
                call proc_printmsg
                pop dx
                mov al, 40h
                mul dl
                mov si, offset PTable_hd0
                add si, ax
                add dl, "0" 
                mov word ptr [PTable_Drv],"dh"
                mov byte ptr [PTable_Drv]+2, dl
                mov byte ptr [P_Number],"1"
check_entry_next_0:
                mov cx, 4
check_p_entry_next_1:
                cmp byte ptr [SI][ptFileSystemName],0
                jna short pass_call_print_p_entry
                call proc_print_p_entry
pass_call_print_p_entry:
                add si, 10h
                inc byte ptr [P_Number]
                loop check_p_entry_next_1

                retn 

proc_print_partition_table endp

INCLUDE DRVINIT0.ASM  ; SINGLIX Drive Initialization Procedures

proc_print_p_entry proc near
                   ; Input -> DS:SI = Partition Entry Buffer
                   push cx
                   push si
                   mov al, byte ptr [SI][ptBootable]
                   call proc_hex
                   mov word ptr [P_Status], ax
                   mov al, byte ptr [SI][ptBeginHead]
                   call proc_hex
                   mov word ptr [Begin_Head], ax
                   mov al, byte ptr [SI][ptBeginSector]
                   call proc_hex
                   mov word ptr [Begin_Sec], ax
                   mov al, byte ptr [SI][ptBeginCylinder]
                   call proc_hex
                   mov word ptr [Begin_Cyl], ax
                   mov al, byte ptr [SI][ptFileSystemName]
                   call proc_hex
                   mov word ptr [FS_ID], ax
                   mov al, byte ptr [SI][ptEndHead]
                   call proc_hex
                   mov word ptr [End_Head], ax
                   mov al, byte ptr [SI][ptEndSector]
                   call proc_hex
                   mov word ptr [End_Sec], ax
                   mov al, byte ptr [SI][ptEndCylinder]
                   call proc_hex
                   mov word ptr [End_Cyl], ax
                   
                   mov ax, word ptr [SI][ptStartSector]
                   mov dx, word ptr [SI][ptStartSector]+2

                   mov si, offset P_Start_Sec
                   mov cx, 9
loc_write_space_PStartSec:
                   mov byte ptr [SI], 20h
                   inc si
                   loop loc_write_space_PStartSec
                   mov cx, 10 
loc_rediv_startsec:
                   call Rx_Dos_Div32
                   add bl,'0'
                   mov byte ptr [SI],bl
                   cmp si, offset P_Start_Sec
                   jna short loc_conv_psectors
                   dec si
                   and ax, ax
                   jnz short loc_rediv_startsec
                   and dx, dx
                   jnz short loc_rediv_startsec
loc_conv_psectors:
                   pop si
                   push si
                   mov ax, word ptr [SI][ptSectors]
                   mov dx, word ptr [SI][ptSectors]+2

                   mov si, offset P_Sectors 
                   mov cx, 9
loc_write_space_PSectors:
                   mov byte ptr [SI], 20h
                   inc si
                   loop loc_write_space_PSectors
                   mov cx, 10
loc_rediv_psectors:
                   call Rx_Dos_Div32
                   add bl,'0'
                   mov byte ptr [SI],bl
                   cmp si, offset P_Sectors
                   jna short loc_filesys_str
                   dec si
                   and ax, ax
                   jnz short loc_rediv_psectors
                   and dx, dx
                   jnz short loc_rediv_psectors
loc_filesys_str:
                   pop si
                   push si
                   mov  bl, byte ptr [SI][ptFileSystemName]
                   cmp  bl, 7
                   ja   short loc_non_dos_fs
                   mov  al, 0Bh
                   mul  bl
                   mov  si, offset FileSys_Names
                   add  si, ax
loc_move_fsname:
                   mov  di, offset P_FileSystem
                   push cx
                   mov  cx,11
                   rep  movsb
                   pop  cx
print_p_entry_row:
                   mov si, offset PTable_Drv
                   call proc_printmsg
                   pop si
                   pop cx
                   retn
loc_non_dos_fs:
                   mov si, offset FS_WIN_32
                   cmp bl, 0Bh
                   je short loc_move_fsname
                   cmp bl, 0Ch
                   je short loc_move_fsname
                   mov si, offset FS_WIN_P
                   cmp bl, 0Eh
                   je short loc_move_fsname
                   mov si, offset FS_WIN_EXT
                   cmp bl, 0Fh
                   je short loc_move_fsname
                   mov si, offset FS_SCO
                   cmp bl, 63h
                   je short loc_move_fsname
                   mov si, offset FS_Linux
                   cmp bl, 83h
                   je short loc_move_fsname
                   mov si, offset FS_LinuxSwap
                   cmp bl, 82h
                   je short loc_move_fsname
                   mov si, offset FS_LinuxExt
                   cmp bl, 85h
                   je short loc_move_fsname
                   mov si, offset FS_TR_MULTIX
                   cmp bl, 0A1h
                   je short loc_move_fsname
                   mov si, offset FS_ATAPITR
                   cmp bl, 0A2h
                   je short loc_move_fsname
                   mov si, offset RS_TRDD
                   cmp bl, 0A0h
                   je short loc_move_fsname
                   mov si, offset FS_Others
                   jmp short loc_move_fsname

proc_print_p_entry endp

proc_rw_numerical  proc    near
                ; proc_rw_char
                ; (c) Erdogan Tan, 2000-2009
                ; DS:SI = String destination address 
                push    es
                xor     cx,cx
                mov     es,cx
                mov     dh,byte ptr [CursorLine]
                mov     dl,byte ptr ES:[CCCpointer]
                mov     bx,07h   
readnextchar0:
                mov     ax, 0E07h
               ;mov     bx, 07h
                int     10h
readnextchar:
                xor     ah,ah
                int     16h
                and     al,al
                jz      short loc_arrow    
                cmp     al,0E0h          
                jz      short loc_arrow
                cmp     al,08h             
                jnz     short char_return
loc_back:
                cmp     dl,byte ptr [CursorColumn]
                ja      short prev_column
                jmp     short readnextchar0
prev_column:
                dec     dl
set_cursor_pos:
                mov     ah,02h
                int     10h
              ; xor     bh,bh
                mov     bl,dl
                sub     bl,byte ptr [CursorColumn]  
                mov     cx,1
                mov     ah,09h
                mov     al,20h
loc_write_it0:
                mov     byte ptr [SI][BX],al
                mov     bl,7
loc_write_it:
                int     10h
                mov     dl,Byte Ptr ES:[CCCpointer]
                jmp     short readnextchar
loc_arrow:    
                cmp     AH,4Bh
                jz      short loc_back
                cmp     AH,53h
                jz      short loc_back
                cmp     AH,4Dh
                jnz     short readnextchar
                mov     al,dl
                sub     al,byte ptr [CursorColumn] 
                cmp     al,10
                jnb     short readnextchar0
                inc     dl
                jmp     short set_cursor_pos
char_return:
                mov     bl,dl
              ; xor     bh,bh 
                sub     bl,byte ptr [CursorColumn] 
                mov     ah,0Eh
                cmp     al,20h
                jb      short loc_escape

                ; Numerical Input check
                ; Erdogan Tan [18-4-2009]
                cmp     al, "0"
                jb      short readnextchar0
                cmp     al, "9"
                ja      short readnextchar0
                ; End of numerical input check

                cmp     bl,10
                jb      short loc_write_it0
                jmp     short readnextchar0
pass_escape:
                cmp     al,0Dh
                jnz     short readnextchar
                int     10h
                mov     al,0Ah
                int     10h
                pop     es
                retn
loc_escape:
                cmp     al,1Bh
                jnz     short pass_escape
                pop     es
                retn

proc_rw_numerical endp


proc_lba_sector_input proc near
               
                ; Erdogan Tan
                ; 18-4-2009
                ; Input
                ; DX:AX = Beginning Sector or Sector Count (Default)
                ; Output
                ; DX:AX = Beginning Sector or Sector Count (Input value)
                ; STC = Excape key pressed
 
                push dx
                push ax
                 
                mov ah, 03h
                xor bh, bh
                int 10h

                mov byte ptr [CursorColumn], dl
                mov byte ptr [CursorLine], dh
 
                pop ax
                pop dx

                mov si, offset Str_SectorNumberInput 
                mov cx, 10
                push cx 
loc_reset_str_SectorNumberInput:
                mov byte ptr [SI], 0
                inc si
                loop loc_reset_str_SectorNumberInput
                pop cx
                mov bp, sp
loc_rediv_SectorNumberInput:
                call Rx_Dos_Div32
                add bl,'0'
                push bx
                and ax, ax
                jnz short loc_rediv_SectorNumberInput
                and dx, dx
                jnz short loc_rediv_SectorNumberInput
                mov si, offset Str_SectorNumberInput
loop_popbx_SectorNumberInput: 
                pop bx
                mov byte ptr [SI],bl
                inc si
                cmp bp, sp
                jne short loop_popbx_SectorNumberInput
                mov byte ptr [SI], 0  
loc_print_sectornumberinput:
                mov si, offset Str_SectorNumberInput
                push si
                call proc_printmsg
                xor al, al
                mov cx, 11
                pop si
		; mov si, offset Str_SectorNumberInput   
                ; call proc_rw_char ; (c) Erdogan Tan, 2000-2009
                call proc_rw_numerical ; (c) Erdogan Tan, 2000-2009

                cmp al, 1Bh
                jne short loc_convert_decimal_str_to_binary
                
                stc   ; Escape

                retn 

loc_convert_decimal_str_to_binary:
                ; (c) Erdogan Tan, 19-4-2009
                mov si, offset Str_SectorNumberInput 
                mov bl, byte ptr [SI]
                cmp bl, "0"
                jnb short loc_convert_decimal_str_to_binary_reset
                xor ax, ax
                xor dx, dx
                stc
                retn
loc_convert_decimal_str_to_binary_reset:
                mov word ptr [decimal_multipliers], 0
                mov word ptr [decimal_multipliers]+2,0
                xor bh, bh
                xor cx, cx
loop_convert_decimal_str_to_binary:
                sub bl, "0" 
                inc cx
                push bx
                inc si 
                mov bl, byte ptr [SI]
                cmp bl, "0"
                jnb short loop_convert_decimal_str_to_binary
loc_convert_decimal_str_to_binary_mul32:
                mov si, offset decimal_multipliers_1
loop_convert_decimal_str_to_binary_mul32:
                mov ax, word ptr [SI]
                inc si
                inc si
                mov dx, word ptr [SI] 
                inc si
                inc si
                pop bx
                call proc_mul32
                add word ptr [decimal_multipliers], ax
                adc word ptr [decimal_multipliers]+2, dx
                loop loop_convert_decimal_str_to_binary_mul32
                mov ax, word ptr [decimal_multipliers]
                mov dx, word ptr [decimal_multipliers]+2
              
                clc

                retn

proc_lba_sector_input endp


proc_get_chs_translated proc near
                ; (c) Erdogan Tan [ April 2009 ]
loc_get_chs_translated:
                ;mov dl, byte ptr [Current_Drive]
                ;push dx
                ;add dl, 80h
                ;mov ah, 08h
                ;int 13h    
                ;pop ax
                ;jc loc_drv_not_ready ; al= HDD number from 0 to 4
                
                ;mov byte ptr [DLastHead], dh
                ;mov ax, cx
                ;and cx, 3Fh                     
                ;mov byte ptr [DLastSec], cl ; 1 based
                ;xchg al, ah
                ;shr ah, 1
                ;shr ah, 1
                ;shr ah, 1
                ;shr ah, 1
                ;shr ah, 1
                ;shr ah, 1 
                ;mov word ptr [DLastCyl], ax  ; Zero based
                
                ;xor bh, bh
                ;mov bl, dh
                ;inc bx

                mov si, offset DiskParams
                mov al, byte ptr [Current_Drive]
                mov ah, 40h
                mul ah
                add si, ax

                xor ch, ch
                mov cl, byte ptr [SI][DPSecPerTrack]
                xor bh, bh
                mov bl, byte ptr [SI][DPHeads]

loc_set_fs_beginning_chs:
                ; xor ch, ch
                ; mov cl, byte ptr [DLastSec]
                ; xor bh, bh
                ; mov bl, byte ptr [DLastHead]
                ; inc bx 
                mov ax, word ptr [BeginningSector]
                mov dx, word ptr [BeginningSector]+2
                    
                push bx  
                
                call RX_DOS_DIV32           

                inc  bx                         ; Sector (1 based)
                pop  cx                         ; Heads 
                push bX
                
                call RX_DOS_DIV32
                pop  cx            
                                                ; AX=Cylinder, CX=Sector 
                cmp dx, 0
                jna short loc_check_chs_ax
loc_chs_over: 
                mov byte ptr [FSPart_BH], 0FEh
                mov byte ptr [FSPart_BS], 0FFh
                mov byte ptr [FSpart_BC], 0FFh
                mov byte ptr [FSPart_EH], 0FEh
                mov byte ptr [FSPart_ES], 0FFh
                mov byte ptr [FSpart_EC], 0FFh
                retn   
loc_check_chs_ax:
                cmp ax, 1023
                ja short loc_chs_over

pass_chs_over:           
                mov byte ptr [FSPart_BH], bl    ; BX = Head (max. FFh)
                mov ch, al
                ror ah, 1                       ; Rotate right
                ror ah, 1    
                or cl, ah  
                mov word ptr [FSPart_BS], cx

loc_set_fs_ending_chs:
                ;mov si, offset DiskParams
                ;mov al, byte ptr [Current_Drive]
                ;mov ah, 40h
                ;mul ah
                ;add si, ax

                xor ch, ch
                mov cl, byte ptr [SI][DPSecPerTrack]
                xor bh, bh
                mov bl, byte ptr [SI][DPHeads]

                mov ax, word ptr [BeginningSector]
                mov dx, word ptr [BeginningSector]+2
                add ax, word ptr [SectorCount]
                adc dx, word ptr [SectorCount]+2
                                
                ;xor ch, ch
                ;mov cl, byte ptr [DLastSec]
                ;xor bh, bh
                ;mov bl, byte ptr [DLastHead]
                ;inc bx  
                
                push bx  
                
                call RX_DOS_DIV32           

                inc  bx                         ; Sector (1 based)
                pop  cx                         ; Heads 
                push bX
                
                call RX_DOS_DIV32
                pop  cx            
                                                ; AX=Cylinder, CX=Sector 
                cmp dx, 0
                jna short loc_check_e_chs_ax
loc_e_chs_over: 
                mov byte ptr [FSPart_EH], 0FEh
                mov byte ptr [FSPart_ES], 0FFh
                mov byte ptr [FSpart_EC], 0FFh
                retn   
loc_check_e_chs_ax:
                cmp ax, 1023
                ja short loc_e_chs_over

pass_e_chs_over:           
                mov byte ptr [FSPart_EH], BL      ; BX = Head (max. FFh)
                mov ch, al
                ror ah, 1                       ; Rotate right
                ror ah, 1    
                or cl, ah  
                mov word ptr [FSPart_ES], cx
                retn

proc_get_chs_translated endp


decimal_multipliers:
                dd 0
decimal_multipliers_1:
                dd 1
                dd 10
                dd 100
                dd 1000
                dd 10000
                dd 100000
                dd 1000000 
                dd 10000000  
                dd 100000000
                dd 1000000000

                dd 0
msg_crlf:
               db 0Dh, 0Ah, 0    

               db 4
               db 1

P_Table_Header:
               db 7
               db 0Dh, 0Ah
               db "[SINGLIX] FIXED DISK PARTITION TABLE  (c) Erdogan TAN 2005-2009"
               db  0Dh, 0Ah, 0Dh, 0Ah
               db "DRV P  S  BH  BS  BC  FS  EH  ES  EC  START SEC  SECTORS  FILE SYSTEM"
               db 0Dh, 0Ah
               db "--- - --- --- --- --- --- --- --- --- --------- --------- -----------"
               db 0Dh, 0Ah, 0h
PTable_Drv:
               db  "hd"
               db  0,20h
P_Number:      db  0,20h
P_Status:      dw  0
               db  "h "
Begin_Head:    dw  0
               db  "h "
Begin_Sec:     dw  0
               db "h "
Begin_Cyl:     dw  0
               db "h "
FS_ID:         dw  0
               db "h "
End_Head:      dw  0
               db "h "
End_Sec:       dw  0
               db "h "
End_Cyl:       dw  0
               db "h"
P_Start_Sec:   db 20h   
               db 9 dup("0")
P_Sectors:     db 20h
               db 9 dup("0")
               db 20h 
P_FileSystem:  db 11 dup(20h)
               db 0Dh, 0Ah, 0

Print_TotalSectors:
               db 0Dh, 0Ah
               db "Total Disk Sectors : "
str_Total_Sectors:
               db 11 dup(20h)
               db 0Dh, 0Ah, 0   

FDisk_Options:
               db 0Dh, 0Ah, 0Dh, 0Ah 
               db "Singlix FS FDISK Command Options :" 
               db 0Dh, 0Ah, 0Dh, 0Ah
               db "1. Create Singlix FS Partition", 0Dh, 0Ah
               db "2. Set Active Partition", 0Dh, 0Ah
               db "3. Delete Partition", 0Dh, 0Ah  
               db "4. Write Current Partition Table", 0Dh, 0Ah
               db "5. Change Current Drive", 0Dh, 0Ah
               db 0Dh, 0Ah, 0Dh, 0Ah
               db "Enter the option number or press ESC to exit ...", 0Dh, 0Ah
               db 0Dh, 0Ah, 0           

msg_FDISK_Drive_No:
               db 0Dh, 0Ah, 0Dh, 0Ah 
               db "Singlix FS FDISK Command :", 0Dh, 0Ah
               db "Change Current Fixed Disk Drive" 
               db 0Dh, 0Ah, 0Dh, 0Ah
               db "Enter the drive number or press ESC to continue ...", 0Dh, 0Ah
               db "(hd?) 0,1,2,3" 
               db 0Dh, 0Ah, 0           

msg_DrvNotReady:
               db 7 
               db 0Dh, 0Ah, 0Dh, 0Ah 
               db "Drive "
               db "hd"
NotReadyDrv:   db "0"
               db " not ready !"
               db 0Dh, 0Ah, 0Dh, 0Ah
msg_Press_Any_Key:
               db "Press any key to continue ..."
               db 0Dh, 0Ah, 0           

Read_Error:    db 7 
               db 0Dh, 0Ah, 0Dh, 0Ah
               db "Drive not ready or read error !"
               db 0Dh, 0Ah, 0Dh, 0Ah, 0
Write_Error:   db 7 
               db 0Dh, 0Ah, 0Dh, 0Ah 
               db "Drive not ready or write error !"
               db 0Dh, 0Ah, 0Dh, 0Ah, 0

msg_Enter_Active_Partition_No:
               db 0Dh, 0Ah, 0Dh, 0Ah 
               db "Singlix FS FDISK Command :", 0Dh, 0Ah
               db "Set Active Partition" 
               db 0Dh, 0Ah, 0Dh, 0Ah
               db "Enter the partition number to set active or press ESC to continue ...", 0Dh, 0Ah
               db 0Dh, 0Ah, 0           

msg_Delete_Partition_No:
               db 0Dh, 0Ah, 0Dh, 0Ah 
               db "Singlix FS FDISK Command :", 0Dh, 0Ah
               db "Delete Partition" 
               db 0Dh, 0Ah, 0Dh, 0Ah
               db "Enter the partition number to delete or press ESC to continue ...", 0Dh, 0Ah
               db 0Dh, 0Ah, 0           

msg_Do_You_Want_to_Delete:
               db 7  
               db 0Dh, 0Ah, 0Dh, 0Ah 
               db "WARNING ! Data in the deleted partition will be lost !", 0Dh, 0Ah
               db 0Dh, 0Ah
               db "Do you want to delete partition "
str_d_pn:      db "0"            
               db " (Y/N) ? ", 0Dh, 0Ah
               db 0Dh, 0Ah, 0           

msg_Create_Singlix_Partition:
               db 0Dh, 0Ah, 0Dh, 0Ah 
               db "Singlix FS FDISK Command :", 0Dh, 0Ah
               db "Create Singlix FS Partition" 
               db 0Dh, 0Ah, 0
msg_No_Space:
               db 7  
               db 0Dh, 0Ah  
               db "No space to create a Singlix FS partition !", 0Dh, 0Ah
               db 0Dh, 0Ah, 0 

msg_Enter_Begin_Sector:
               db 0Dh, 0Ah, 0Dh, 0Ah 
               db "Enter the beginning sector number or press ESC to continue ...", 0Dh, 0Ah
               db 0Dh, 0Ah
               db "FS Partition - Beginning Sector : ", 0           

msg_Enter_Sector_Count:
               db 0Dh, 0Ah, 0Dh, 0Ah 
               db "Enter the sector count (volume size) or press ESC to continue ...", 0Dh, 0Ah
               db 0Dh, 0Ah
               db "FS Partition - Sector Count : ", 0    

msg_boundary_check_failed:
               db 7  
               db 0Dh, 0Ah  
               db "Partition overlap problem !", 0Dh, 0Ah
               db 0Dh, 0Ah, 0 

msg_out_of_space:
               db 7  
               db 0Dh, 0Ah  
               db "Out of space !", 0Dh, 0Ah
               db 0Dh, 0Ah, 0 

msg_Singlix_Partition_Created:
               db 0Dh, 0Ah  
               db "Singlix FS partition has been created ...", 0Dh, 0Ah
               db 0Dh, 0Ah, 0 

msg_write_masterboot_sector:
               db 0Dh, 0Ah, 0Dh, 0Ah 
               db "Singlix FS FDISK Command :", 0Dh, 0Ah
               db "Write Current Partition Table" 
               db 0Dh, 0Ah, 0Dh, 0Ah
               db " 1. Write Partition Table only", 0Dh, 0Ah
               db " 2. Write Partition Table and Singlix Master Boot Code", 0Dh, 0Ah
               db 0Dh, 0Ah
               db "Enter the option number or press ESC to cancel ...", 0Dh, 0Ah
               db 0Dh, 0Ah, 0           

msg_writing_ptable:
               db 0Dh, 0Ah, 0Dh, 0Ah  
               db "Writing partition table on disk ... ", 0
msg_OK:        db 7
               db  0Dh, 0Ah
               db "OK !"
               db  0Dh, 0Ah, 0Dh, 0Ah, 0     
     
msg_fsfdisk_version:
               db 7 
              ;db 0Dh, 0Ah, 0Dh, 0Ah    
               db "Singlix FS FDISK Partitioning Program v1.0  (c) Erdogan TAN  2009", 0Dh, 0Ah
               db 0Dh, 0Ah, 0
fsfdisk_com:   db "FSFDISK.COM [24/04/2009]"
               db 0 

FileSys_Names: ; 2003-2009 (Valid FileSystems for SINGLIX Project in 2009)
               db "           "   
               db "DOS FAT12  "  ; 01h = FAT12
               db "XENIX      "  ; 02h , XENIX System V root
               db "XENIX usr  "  ; 03h , XENIX System V user
               db "DOS FAT16  "  ; 04h = FAT16 < 32MB
               db "DOS EXT    "  ; 05h = Extended DOS Partition
               db "DOS FAT16  "  ; 06h = FAT16 > 32MB, CHS Mode
FS_NTFS:       db "WINDOWS NT "  ; 07h , WINDOWS 2000/XP NTFS Partition
FS_WIN_32:     db "WIN4 FAT32 "  ; OBh = FAT32 CHS, 0Ch = FAT32 LBA
FS_WIN_P:      db "WIN4 FAT16 "  ; 0Eh = FAT16, LBA Mode
FS_WIN_EXT:    db "WIN4 EXT   "  ; 0Fh = Extented Partition, LBA Mode
FS_SCO:        db "SCO Unix   "  ; 63h , SCO UNIX, UNIXWARE, OPENSERVER
FS_Linux:      db "Linux      "  ; 83h , LINUX NATIVE (ext2) Partition
FS_LinuxSwap:  db "Linux Swap "  ; 82h , LINUX SWAP Partition
FS_LinuxExt:   db "Linux Ext  "  ; 85h , LINUX EXTENDED Partition
FS_TR_MULTIX:  db "Singlix FS1"  ; A1h , (32 bit, 512 bytes per sector)
                                 ; TR-MULTIX LBA disk FS (ATA device LBA FS)
FS_ATAPITR:    db "Singlix FS2"  ; A2h , (32 bit, 2048 bytes per sector)
                                 ; TR-MULTIX ATAPI device LBA FS
RS_TRDD:       db "RDD        "  ; A0h , (Random Data Disk) LBA
                                 ; Reserved (New Record System)
                                 ; 14/3/2009
FS_Others:     db "Unknown FS "  ; Non Singlix, Another or Unknown File Systems

;DLastHead: db "4"
;DLastSec: db "1"
;DLastCyl: db "7"
CursorLine: db 0
CursorColumn: db 0
GetChar:        db 0
Current_Drive:  db 0
NumberOfPartitions: db 0
FF_Partition: db 0
TotalSectors: dd 0
FS_Partition_Entry: 
FSPart_Active: db 0
FSPart_BH: db 0
FSPart_BS: db 0
FSPart_BC: db 0
FSPart_FS: db 0A1h
FSPart_EH: db 0
FSPart_ES: db 0
FSPart_EC: db 0
BeginningSector: dd 0
SectorCount: dd 0
EndingSector: dd 0
Str_SectorNumberInput: db 11 dup(0)

                   dw offset fsfdisk_com - offset start
 
File_Size:         dw $-254+512

FS_MBR:
INCLUDE      FS1_MBR.TXT   

Present            ends

                   end start  
