; *****************************************************************************
;
; DRVINIT4.ASM  [ TRDOS Kernel Drive Initialization Procedures ]
; Copyright (C) 2009-2011 Erdogan TAN  [ 26/09/2009 ] Last Update: 07/08/2011
;
; 01/08/2011
; 31/07/2011
; 29/07/2011
; 28/07/2011 'proc_drv_init' modifications (23-24-25-26-27-28 July 2011)
; 23/07/2011 Major modifications
; 
; 16/07/2011 Fixup for FAT32 CHS partitions (with ZERO hidden sectors in BPB)
; 05/07/2011 512 byte segment boundary check modification in 'floppy_drv_init'
; 08/08/2010 'floppy_drv_init' volume serial number return in DX:AX
; 11/07/2010 'get_free_FAT_sectors' procedure modification 
; 03/01/2010 singlix FS disk_io parameters for CHS (SecPerTrack, Heads) r/w
;            now have same offset values with FAT LD struct (LD_BPB +) offsets
;            also parameter sizes have been changed from byte to word   
; 20/09/2009 dparam calls for floppy drives have been moved to floppy_drv_init 
; 12/09/2009 LD_MediaChanged = 6 sign for initial volume_name check/move proc.
; 09/09/2009 proc_get_media_change_status
; 05/09/2009 proc_get_FAT_volume_name modification
; 23/08/2009 proc_get_FAT_volume_name modification
; 12/07/2009 proc_get_FAT_volume_name
; 04/07/2009 fd_init FAT functionality, 07/07/2009 hd_init FAT functionality
; 07/06/2009 -> Singlix FS drive initialization functionality (drv_init v2)
;
; DRV_INIT.ASM (old FAT & FAT32 drive initialization functions for TRDOS)
; Copyright (C) 2005 Erdogan TAN  [ 29/01/2005 ] (FAT12, FAT16, FAT32)
;
; TRDOS.ASM (include drv_init.asm)
;
; *****************************************************************************

; TRFS

bs_FS_JmpBoot equ 0 ; jmp short bsBootCode
                ; db 0EBh, db 3Fh, db 90h
bs_FS_Identifier equ 3  ; db 'FS', db 0
bs_FS_BytesPerSec equ 6 ; dw 512
bs_FS_MediaAttrib equ 8 ; db 3
bs_FS_PartitionId equ 9 ; db 0A1h
bs_FS_VersionMaj equ 10 ; db 01h
bs_FS_VersionMin equ 11 ; db 0
bs_FS_BeginSector equ 12   ; dd 0 
bs_FS_VolumeSize equ 16 ; dd 2880
bs_FS_StartupFD equ 20 ; dd 0
bs_FS_MATLocation equ 24 ; dd 1
bs_FS_RootDirD equ 28 ; dd 8
bs_FS_SystemConfFD equ 32 ; dd 0
bs_FS_SwapFD equ 36 ; dd 0
bs_FS_UnDelDirD equ 40 ; dd 0
bs_FS_DriveNumber equ 44 ; db 0
bs_FS_LBA_Ready equ 45 ; db 0
bs_FS_MagicWord equ 46 
bs_FS_SecPerTrack equ 46 ; db 0A1h
bs_FS_Heads equ 47 ; db 01h 
bs_FS_OperationSys equ 48 ; db "TR-SINGLIX v1.0b"
bs_FS_Terminator equ 64 ; db 0
bs_FS_BootCode equ 65 

FS_MAT_DATLocation equ 12
FS_MAT_DATScount equ 16
FS_MAT_FreeSectors equ 20
FS_MAT_FirstFreeSector equ 24
FS_RDT_VolumeSerialNo equ 28
FS_RDT_VolumeName equ 64

; FAT12 + FAT16 + FAT32
BS_JmpBoot equ 0
BS_OEMName equ 3
BPB_BytsPerSec equ 11
BPB_SecPerClust equ 13
BPB_RsvdSecCnt equ 14
BPB_NumFATs equ 16
BPB_RootEntCnt equ 17
BPB_TotalSec16 equ 19
BPB_Media equ 21
BPB_FATSz16 equ 22
BPB_SecPerTrk equ 24
BPB_NumHeads equ 26
BPB_HiddSec equ 28
BPB_TotalSec32 equ 32

; FAT12 and FAT16 only
BS_DrvNum equ 36
BS_Reserved1 equ 37
BS_BootSig equ 38
BS_VolID equ 39
BS_VolLab equ 43
BS_FilSysType equ 54 ; 8 bytes
BS_BootCode equ 62

; FAT32 only
BPB_FATSz32 equ 36 ; FAT32, 4 bytes
BPB_ExtFlags equ 40 ; FAT32, 2 bytes
BPB_FSVer equ 42 ; FAT32, 2 bytes
BPB_RootClus equ 44 ; FAT32, 4 bytes
BPB_FSInfo equ 48 ; FAT 32, 2 bytes 
BPB_BkBootSec equ 50 ; FAT32, 2 bytes
BPB_Reserved equ 52 ; FAT32, 12 bytes
BS_FAT32_DrvNum equ 64 ; FAT32, 1 byte
BS_FAT32_Reserved1 equ 65 ; FAT32, 1 byte
BS_FAT32_BootSig equ 66 ; FAT32, 1 byte
BS_FAT32_VolID equ 67 ; FAT32, 4 bytes
BS_FAT32_VolLab equ 71 ; FAT32, 11 bytes
BS_FAT32_FilSysType equ 82 ; FAT32, 8 bytes
BS_FAT32_BootCode equ 90

BS_Validation equ 510


proc_drv_init   proc    near
                ; 07/08/2011
                ; 01/08/2011
                ; 31/07/2011
                ; 29/07/2011
                ; 28/07/2011
                ; 27/07/2011
                ; 26/07/2011
                ; 25/07/2011
                ; 24/07/2011
                ; 23/07/2011 Major modification
                ; 16/07/2011 -> 
                ; Fixup for primary FAT32 CHS (OBh) partitions
                ; has 0 hidden sectors in their boot sector's BPB.
                ; (For example: FAT32 CHS partition is on sector 63
                ; but hidden sectors value is 0 in the BPB.)
                ;
                ; 03/01/2010 -> Singlix FS CHS/BPB disk_io fixup
                ;               (SecPerTrack, Heads) 
                ;
                ; 20/09/2009 (fd dparam excluded)
                ; 2005

               ;23/07/2011
               ;mov byte ptr [Last_DOS_DiskNo], 1
               ;mov si, offset Diskparams
               ;mov dx, 80h
               ;add si, dx
                mov dl, 80h
                mov si, offset FixedDiskParams
                call proc_dparam
                jnc short pass_hd_15h_error
                retn ;cf = 1 -> No hard disks
pass_hd_15h_error:
                mov ah, byte ptr [SI][DPDisks]
                mov byte ptr [HardDisks], ah
                mov byte ptr [HDCounter], ah
next_hard_disk:
                dec byte ptr [HDCounter]
                jz  short load_hd_partition_tables
                inc dl
                add si, 40h
                call proc_dparam
               ; 29/07/2011
               ;jnc short next_hard_disk
               ;mov word ptr [SI][DPNumOfSecs], 0
               ;mov word ptr [SI][DPNumOfSecs]+2,0
                jmp short next_hard_disk

load_hd_partition_tables:
                mov si, offset Disk_hd0
                mov di, offset PTable_hd0
                push ds
                pop es
                mov dl, 80h
               ; 23/07/2011
                xor ch, ch
                mov cl, byte ptr [HardDisks]

load_next_hd_partition_table:
                push cx
                push di
                push si
                cmp byte ptr [SI]+1, 03h
                jne short pass_pt_this_hard_disk
                call proc_load_masterboot
                jc short pass_pt_this_hard_disk
                mov si, offset PartitionTable
               ; 28/07/2011
                mov bx, si
                mov cx, 32
                rep movsw
                mov si, bx 
               ; 27/07/2011
                mov byte ptr [HDCounter], 4
loc_validate_hdp_partition:
               ; 28/07/2011
                cmp byte ptr [SI][ptFileSystemID], 0
                jna short loc_validate_next_hdp_partition2
                push si ; Masterboot partition table offset
                push dx ; dl = Physical drive number 
                inc byte ptr [PP_Counter]
               ; 31/07/2011
                xor di, di  
                ; Input -> DS:SI = PartitionTable offset
                ; DL = Hard disk drive number 
                ; DI = 0 -> Primary Partition
                ; DI > 0 -> Extended Partition's Start Sector                 
                call proc_validate_hd_fat_partition
                jnc short loc_set_valid_hdp_partition_entry
                pop dx
                push dx 
                call proc_validate_hd_fs_partition
                jc short loc_validate_next_hdp_partition1
loc_set_valid_hdp_partition_entry:
                ; 26/07/2011
                ; 28/07/2011  
                ;;cl = byte ptr [Last_DOS_DiskNo]
                mov cl, byte ptr [Last_DOS_DiskNo] 
                add cl, "A"
                mov byte ptr [SI][LD_Name], cl
                mov ah, byte ptr [SI][LD_PhyDrvNo]
                mov al, ah ; Physical drive number
                sub al, 80h
                shl al, 1
                shl al, 1
                add al, 4 ; 0 Based
                sub al, byte ptr [HDCounter]
                ; al = partition entry, 0 based
                ;  0 -> hd 0, Partition Table offset = 0
                ; 15 -> hd 3, Partition Table offset = 3
                ;mov byte ptr [SI][LD_PartitionEntry], al 
                sub ah, 7Eh ;
                ; ah = DiskParams entry, zero based
                ;      0 for drive A:, 2 for drive C:
                ;mov byte ptr [SI][LD_DParamEntry], ah 
                mov word ptr [SI][LD_PartitionEntry], ax
loc_validate_next_hdp_partition1:
               ; 28/07/2011
                pop dx ; dl = Physical drive number 
                pop si ; Masterboot partition table offset
loc_validate_next_hdp_partition2:
                ; DS:SI = PartitionTable offset
                ; DL = Hard/Fixed disk drive number
                dec byte ptr [HDCounter]
                jz short pass_pt_this_hard_disk
                add si, 16 ; 10h
                jmp short loc_validate_hdp_partition

loc_next_hd_partition_table:
               ; 28/07/2011  
                inc dl
                add si, 40h 
                add di, 40h
                jmp short load_next_hd_partition_table

pass_pt_this_hard_disk:
               ; 28/07/2011
                pop si ; Dparam table, Disk_hd? 
                pop di ; Ptable_hd?
                pop cx
                loop loc_next_hd_partition_table

                cmp byte ptr [PP_Counter], 1
                jnb short load_extended_dos_partitions
                ; Empty partition table
                retn 

load_extended_dos_partitions:
                mov si, Offset PTable_hd0
                mov di, Offset PTable_ep0
                mov byte ptr [HDCounter], 80h

next_hd_extd_partition:
              ; 28/07/2011 
                push si ; PTable_hd? offset
                push di ; PTable_ep?
                mov cx, 4
                mov dl, byte ptr [HDCounter]

hd_check_fs_id_05h:
              ; 28/07/2011 
                mov al, byte ptr [SI][ptFileSystemID]
                cmp al, 05h ; Is it an extended dos partition ?
                je short loc_set_ep_start_sector
                cmp al, 0Fh ; Is it an extended win4 (LBA mode) partition ?
                jne short continue_to_check_ep

loc_set_ep_start_sector:
                inc byte ptr [EP_Counter]
                mov ah, dl ; byte ptr [HDCounter]
                xchg ah, al ; al = Drv Number, ah = Partition Identifier
                push ax 
                mov bx, offset EP_StartSector
                xor ah, ah  
                sub al, 80h
                push ax
                shl al, 1 ; al = al * 2
                shl al, 1 ; al = al * 2
                add bx, ax
                mov ax, word ptr [SI][ptStartSector]
                mov dx, word ptr [SI][ptStartSector]+2
               ; DX:AX = Extended partition's start sector
                mov word ptr [BX],ax
                mov word ptr [BX]+2,dx
                pop ax ; AL = Drv number - 80h, AH = 0 
                pop dx ; DL = Drv number, DH = Partition ID
                mov bx, offset HD_LBAyes
               ;xor ah, ah
                add bx, ax
               ; 28/07/2011  
                cmp byte ptr [BX], 1 ; LBA ready = Yes
                jb short loc_hd_load_ep_05h
                cmp dh, 05h
                je short loc_hd_load_ep_05h

loc_hd_load_ep_0Fh:
               ;07/08/2011
               ;23/07/2011 
               ;mov al, dl ; Drive number   
               ;sub al, 80h
                mov ah, 40h
                mul ah
                mov bx, offset Disk_hd0
                add bx, ax
ep_mov_dap_parameters:
               ;push ds
               ;pop es
                cmp byte ptr [BX][DAP_PacketSize], 10h
                jne short continue_to_check_ep
                mov word ptr [BX][DAP_Destination], offset MasterBootBuff
                push ds
                pop word ptr [BX][DAP_Destination]+2
                mov byte ptr [BX][DAP_NumOfBlocks],1
                mov di, bx
                push si
                add si, ptStartSector
                add di, DAP_LBA_Address
                movsw
                movsw
                mov si, bx
                add si, DAP_PacketSize ; DS:SI= DAP Location
                mov bx, offset MasterBootBuff
                mov ah, 42h  ; Extended Disk Read - LBA Read
               ;mov dl, byte ptr [HDCounter]
                int 13h
                pop si
                jnc short loc_hd_move_ep_table

continue_to_check_ep:
                add si, 10h ; 16
                loop hd_check_fs_id_05h
continue_check_ep_next_disk:
               ; 28/07/2011
                pop di ; PTable_ep?
                pop si ; PTable_hd?
                mov al, byte ptr [HardDisks]
                add al, 7Fh ; 26/07/2011 
                cmp byte ptr [HDCounter], al
                jnb loc_validating_hd_partitions_ok
                add si, 40h
                add di, 40h
                inc byte ptr [HDCounter]
                jmp next_hd_extd_partition

loc_hd_load_ep_05h:
               ; 28/07/2011
                push cx 
                mov dh, byte ptr [SI][ptBeginHead]
                mov cx, word ptr [SI][ptBeginSector]
                mov ax, 0201h    ; Read 1 sector
                mov bx, offset MasterBootBuff
               ;push ds
               ;pop es
                int 13h
                pop cx  
                jc short continue_to_check_ep

loc_hd_move_ep_table:
               ; 28/07/2011
                pop di
                push di  ; PTable_ep?
                mov si, offset PartitionTable ; Extended
                mov bx, si
                mov cx, 32
                rep movsw
                mov si, bx 
                
loc_set_hde_sub_partition_count:
              ; 31/07/2011
                mov byte ptr [PP_Counter], 4

loc_validate_hde_partition:
               ; 01/08/2011
               ; 28/07/2011
                cmp byte ptr [SI][ptFileSystemID], 0
                jna short loc_validate_next_hde_partition2
                push si ; Extended partition table offset
                mov dl, byte ptr [HDCounter]
               ; 31/07/2011
                mov al, dl
                sub al, 80h
                shl al, 1
                shl al, 1
                xor ah, ah
                push ax
                add al, 4
                sub al, byte ptr [PP_Counter] ; 4 to 1
                ; al = partition entry, 0 based
                ;  0 -> hd 0, Partition Table offset = 0
                ; 15 -> hd 3, Partition Table offset = 3
                ;mov byte ptr [SI][LD_PartitionEntry], al 
                mov ah, dl                 
                sub ah, 7Eh ;
                ; ah = DiskParams entry, zero based
                ;      0 for drive A:, 2 for drive C:
                ;mov byte ptr [SI][LD_DParamEntry], ah 
                mov word ptr [SI][LD_PartitionEntry], ax
                mov di, offset EP_StartSector
                pop ax
                add di, ax
                ; Input -> DS:SI = PartitionTable offset
                ; DL = Hard disk drive number                 
                ; DI = Extd. P. start sector pointer
                call proc_validate_hd_fat_partition
                jc short loc_validate_next_hde_partition1
loc_set_valid_hde_partition_entry:
                ; 28/07/2011  
                ;;cl = byte ptr [Last_DOS_DiskNo]
                mov cl, byte ptr [Last_DOS_DiskNo] 
                add cl, "A"
                mov byte ptr [SI][LD_Name], cl
              
loc_validate_next_hde_partition1:
               ; 28/07/2011
                pop si ; Extended partition table offset
loc_validate_next_hde_partition2:
                ; DS:SI = Extended partition table offset
                ; DL = Hard disk drive number
                dec byte ptr [PP_Counter]
                jz  continue_check_ep_next_disk
                add si, 16 ; 10h
                jmp short loc_validate_hde_partition

loc_validating_hd_partitions_ok:
               ; 28/07/2011
                mov al, byte ptr [Last_DOS_DiskNo]
                retn
 
; 23/07/2011
DRV_INIT_DosDrv:  dw 0

PP_Counter:     db 0
EP_Counter:     db 0

EP_StartSector: dd 0
                dd 0
                dd 0
                dd 0

loc_drv_init_retn:
                retn

proc_drv_init   endp

proc_dparam   proc near
              ; 29/07/2011 BugFix
              ; 23/07/2011 Modification
              ; input
              ; dl = Disk Drive Number
              ; ds:si = Parameters Table Buffer
              ; output
              ; cf=0 -> ah = 0
              ; cf=1 -> ah = error number
              ;  
              mov byte ptr [si][DPDiskNumber], dl
              mov byte ptr [si][DPDType], 0
              push dx
              mov ah, 08h
              int 13h
              mov byte ptr [si][DPReturn], ah
              jnc short dparam_no_error
dparam_error_retn:
              pop dx
              retn
dparam_no_error:
             ;or dl, dl
             ;jz short dparam_error_retn 
              mov byte ptr [si][DPDType], bl
              mov byte ptr [si][DPDisks], dl
              inc dh
              mov byte ptr [si][DPHeads], dh
              push cx
              and cl, 3Fh
              mov byte ptr [si][DPSecPerTrack], cl
              pop cx
              shr cl,1
              shr cl,1
              shr cl,1
              shr cl,1
              shr cl,1
              shr cl,1
              xchg ch,cl
              inc cx
              mov word ptr [si][DPCylinders], cx
              mov word ptr [si][DPTableOff], di
              push es
              pop word ptr [si][DPTableSeg]

             ; 23/07/2011
              pop dx 
              cmp dl, 80h
              jb short dparam_15h_return ;29/07/2011
              mov ah, 15h
              int 13h
              jc short dparam_15h_return
              mov byte ptr [si][DPDType], ah
              cmp ah, 3
              jne short dparam_15h_return 
              mov word ptr [si][DPNumOfSecs], cx
              mov word ptr [si][DPNumOfSecs]+2, dx
dparam_15h_return:
             ;29/07/2011
              mov dl, byte ptr [si][DPDiskNumber]
             ;mov byte ptr [SI][DAP_PacketSize], 0 ; Reset (No DAP)
              mov ah, 41h ; Check Extensions Present
              mov bx, 55AAh
              int 13h
              jc short dparam_48h_return
              cmp bx, 0AA55h
              jne short dparam_48h_return
              test cx, 01h ; Fixed Disk Access Subset - is LBA ready ? 
              jz short dparam_48h_return
              xor bh, bh
              mov bl, dl
              sub bl, 80h
              add bx, offset HD_LBAYes 
              mov byte ptr [BX],1
              mov byte ptr [SI][DAP_PacketSize], 10h
dparam_41h_return:
              push si
              add si, GetDParams_48h
              mov word ptr [SI], 0026 ; GDP Data Length - Set
            ; DS:SI= Address of Result Buffer
            ; DL (Drive Number) must be not changed before here...
              mov ah, 48h  ; Get Enhanced Disk Drive Parameters
              int 13h
              pop si
              jnc short dparam_48h_return
              xor al, al  ; mov al, 0
              mov byte ptr [SI][GetDParams_48h], al ; GDP Data Length - Reset
              retn

dparam_48h_return:
dparam_return:
              xor ah,ah
              retn

proc_dparam   endp

proc_validate_hd_fat_partition proc near
              ; 07/08/2011 BugFix
              ; 31/07/2011 DI input
              ; 28/07/2011 
              ; 23/07/2011
              ; Input
              ; DL = Hard/Fixed Disk Drive Number
              ; SI = PartitionTable offset
              ;    byte ptr [[Last_DOS_DiskNo]
              ; DI = Extend. Part. Start Sector Pointer
              ; DI = 0 -> Primary Partition 
              ; Output
              ; cf=0 -> Validated
              ;    SI = Logical dos drv desc. table
              ;    BX = FAT boot sector buffer
              ;    byte ptr [Last_DOS_DiskNo]
              ; cf=1 -> Not a valid FAT partition
              ; AX, DX, CX, DI -> changed 

              ;mov si, offset PartitionTable
              mov ah, byte ptr [SI][ptFileSystemId]
              cmp ah, 06h ; FAT16 CHS partition
              jb short loc_not_a_valid_fat_partition2
              mov al, 2
              je short loc_set_valid_hd_partition_params
              cmp ah, 0Eh ; FAT16 LBA partition
              ja short loc_not_a_valid_fat_partition1
              je short loc_set_valid_hd_partition_params
              inc al ; 3
              cmp ah, 0Bh ; FAT32 CHS partition 
              je short loc_set_valid_hd_partition_params
              jb short loc_not_a_valid_fat_partition2
              cmp ah, 0Ch ; FAT32 LBA partition
              je short loc_set_valid_hd_partition_params

loc_not_a_valid_fat_partition1:
              stc
loc_not_a_valid_fat_partition2:
              retn

loc_set_valid_hd_partition_params:
              inc byte ptr [Last_DOS_DiskNo] ; > 1
             ;mov byte ptr [Hard_Disk], dl
             ; 31/07/2011  
              mov bx, offset Logical_DOSDisks
              add bh, byte ptr [Last_DOS_DiskNo] 

              mov byte ptr [BX][LD_DiskType], 2
              mov byte ptr [BX][LD_PhyDrvNo], dl
             ;mov byte ptr [BX][LD_FATType], al ; 2 or 3
             ;mov byte ptr [BX][LD_FSType], ah ; 06h, 0Eh, 0Bh, 0Ch
              mov word ptr [BX][LD_FATType], ax

              mov ax, word ptr [SI][ptStartSector]
              mov dx, word ptr [SI][ptStartSector]+2
              or di, di 
              jz short pass_hd_FAT_ep_start_sector_adding
loc_add_hd_FAT_ep_start_sector:
             ; 31/07/2011 
              add ax, word ptr [DI]
              adc dx, word ptr [DI]+2
pass_hd_FAT_ep_start_sector_adding:              
              mov word ptr [BX][LD_StartSector], ax
              mov word ptr [BX][LD_StartSector]+2, dx
loc_hd_FAT_logical_drv_init:
             ; 28/07/2011
              mov word ptr [DRV_INIT_DosDrv], bx
              mov ax, bx 
              mov dl, byte ptr [BX][LD_PhyDrvNo] ; 07/08/2011
              mov cl, dl
              sub cl, 80h
              xor bh, bh
              mov bl, cl
              add bx, offset HD_LBAYes
              mov ch, byte ptr [BX]
              mov bx, ax
              mov byte ptr [BX][LD_LBAYes], ch

              or ch, ch
              jz short loc_hd_FAT_drv_init_load_bs_chs

loc_hd_FAT_drv_init_load_bs_lba:
             ; 28/07/2011
             ; dl = Physical Drive Number
             ; cl = Hard disk number - 80h                
              mov al, 40h
              mul cl
              add ax, offset FixedDiskParams ; Disk_hd0
             ;mov dl,byte ptr [BX][LD_PhyDrvNo]
              mov si, bx ; 07/08/2011
              mov bx, ax
              cmp byte ptr [BX][DAP_PacketSize], 10h
             ;jne short loc_not_a_valid_fat_partition1
              jb  short loc_not_a_valid_fat_partition2

              mov word ptr [BX][DAP_Destination], offset DOSBootSectorBuff
              push ds
              pop word ptr [BX][DAP_Destination]+2
              mov byte ptr [BX][DAP_NumOfBlocks],1
              mov di, bx
              add si, LD_StartSector ; 07/08/2011
              add di, DAP_LBA_Address
              movsw
              movsw
              mov si, bx
              add si, DAP_PacketSize ; DS:SI= DAP Location
              mov bx, offset DOSBootSectorBuff
              mov ah, 42h  ; Extended Disk Read - LBA Read
              int 13h
              jnc  short loc_hd_drv_FAT_boot_validation

loc_not_a_valid_fat_partition3:
              retn

loc_hd_FAT_drv_init_load_bs_chs:
              mov dh, byte ptr [SI][ptBeginHead]
              mov cx, word ptr [SI][ptBeginSector]
              mov ax, 0201h    ; Read 1 sector
              mov bx, offset DOSBootSectorBuff
              int 13h
              jc  short loc_not_a_valid_fat_partition3

loc_hd_drv_FAT_boot_validation:
             ; 28/07/2011 
             ;mov si, offset DOSBootSectorBuff
              mov si, bx
              cmp word ptr [SI][BS_Validation], 0AA55h
              jne short loc_not_a_valid_fat_partition4
     	      cmp byte ptr [SI][BPB_Media], 0F8h
              jne short loc_not_a_valid_fat_partition4
              cmp word ptr [SI][BPB_FATSz16], 0
              ja short loc_hd_FAT16_BPB
              cmp byte ptr [SI][BS_FAT32_BootSig], 29h
              jne short loc_not_a_valid_fat_partition4

loc_hd_FAT32_BPB:
              mov cx, 45
              jmp short loc_hd_move_FAT_BPB

loc_not_a_valid_FAT_partition4:
              stc
              retn

loc_hd_FAT16_BPB:
              cmp byte ptr [SI][BS_BootSig], 29h
              jne short loc_not_a_valid_fat_partition4
              mov cx, 32

loc_hd_move_FAT_BPB:
              mov di, word ptr [DRV_INIT_DosDrv]
             ;mov si, bx ; Boot sector
              push di
              add di, LD_BPB
              rep movsw 
              pop si
              mov ax, word ptr [SI][LD_StartSector]
              mov dx, word ptr [SI][LD_StartSector]+2
              add ax, Word Ptr [SI][LD_BPB][BPB_RsvdSecCnt]
              adc dx,0
              mov Word Ptr [SI][LD_FATBegin], ax
              mov Word Ptr [SI][LD_FATBegin]+2, dx
              cmp byte ptr [SI][LD_FATType], 3
              jb  short loc_set_FAT16_RootDirLoc
loc_set_FAT32_RootDirLoc:
              mov ax, Word Ptr [SI][LD_BPB][BPB_FATSz32]
              mov dx, Word Ptr [SI][LD_BPB][BPB_FATSz32]+2
              xor bh,bh
              mov bl,Byte Ptr [SI][LD_BPB][BPB_NumFATs]
              call proc_mul32
              add ax, Word Ptr [SI][LD_FATBegin]
              adc dx, Word Ptr [SI][LD_FATBegin]+2
loc_set_FAT32_data_begin:
              mov Word Ptr [SI][LD_DATABegin], ax
              mov Word Ptr [SI][LD_DATABegin]+2, dx
              mov Word Ptr [SI][LD_ROOTBegin], ax
              mov Word Ptr [SI][LD_ROOTBegin]+2, dx

              ; If Root Directory Cluster <> 2 then
              ; change the beginning sector value 
              ; of the root dir by adding sector offset.
              mov ax, word ptr [SI][LD_BPB][BPB_RootClus]
              mov dx, word ptr [SI][LD_BPB][BPB_RootClus]+2
              sub ax, 2
              push ax
              sbb dx, 0
              or ax, dx
              pop ax
              jz short short loc_set_32bit_FAT_total_sectors                

              ;xor bh, bh
              mov bl,Byte Ptr [SI][LD_BPB][BPB_SecPerClust]
              call proc_mul32
              add Word Ptr [SI][LD_RootBegin], ax
              adc Word Ptr [SI][LD_RootBegin]+2, dx

              jmp short loc_set_32bit_FAT_total_sectors

loc_set_FAT16_RootDirLoc:
              mov al,Byte Ptr [SI][LD_BPB][BPB_NumFATs]
              cbw
              mul Word Ptr [SI][LD_BPB][BPB_FATSz16]
              add ax, Word Ptr [SI][LD_FATBegin]                
              adc dx, Word Ptr [SI][LD_FATBegin]+2                
              mov Word Ptr [SI][LD_ROOTBegin], ax
              mov Word Ptr [SI][LD_ROOTBegin]+2, dx
loc_set_FAT16_data_begin:
              mov Word Ptr [SI][LD_DATABegin], ax 
              mov Word Ptr [SI][LD_DATABegin]+2, dx
              mov ax, 20h  ; Size of a directory entry
              mul Word Ptr [SI][LD_BPB][BPB_RootEntCnt]
              add ax, 511
              ; adc dx, 0
              mov cx, 512
              div cx
              xor dx, dx ; 0
              add Word Ptr [SI][LD_DATABegin],ax
              adc Word Ptr [SI][LD_DATABegin]+2,dx
              mov ax, word ptr [SI][LD_BPB][BPB_TotalSec16]
              test ax, ax
              jz short loc_set_32bit_FAT_total_sectors
loc_set_16bit_FAT_total_sectors:
              mov Word Ptr [SI][LD_TotalSectors], ax
              mov Word Ptr [SI][LD_TotalSectors]+2, dx ; 0
              jmp short loc_set_hd_FAT_cluster_count
loc_set_32bit_FAT_total_sectors:
              mov ax,Word Ptr [SI][LD_BPB][BPB_TotalSec32]
              mov Word Ptr [SI][LD_TotalSectors], ax
              mov dx,Word Ptr [SI][LD_BPB][BPB_TotalSec32]+2
              mov Word Ptr [SI][LD_TotalSectors]+2, dx
loc_set_hd_FAT_cluster_count:
              add ax,Word Ptr [SI][LD_StartSector]
              adc dx,Word Ptr [SI][LD_StartSector]+2
              sub ax,Word Ptr [SI][LD_DATABegin]
              sbb dx,Word Ptr [SI][LD_DATABegin]+2
              xor ch,ch
              mov cl,Byte Ptr [SI][LD_BPB][BPB_SecPerClust]
              call Rx_Dos_Div32 
              mov word ptr [SI][LD_Clusters], ax
              mov word ptr [SI][LD_Clusters]+2, dx
               ; Maximum Valid Cluster Number= DX:AX +1
               ; with 2 reserved clusters= DX:AX +2
loc_set_hd_FAT_fs_free_sectors:
             ;mov word ptr [SI][LD_FreeSectors],0
             ;mov word ptr [SI][LD_FreeSectors]+2,0
	      call get_free_FAT_sectors
              jc short loc_validate_hd_FAT_partition_retn

              mov word ptr [SI][LD_FreeSectors],ax
              mov word ptr [SI][LD_FreeSectors]+2,dx
           
              mov byte ptr [SI][LD_MediaChanged], 6  ; Volume Name Reset

             ;mov cl, byte ptr [Last_DOS_DiskNo] 
             ;add cl, "A"
             ;mov byte ptr [SI][LD_FS_Name], cl

loc_validate_hd_FAT_partition_retn:         
              retn
                 
proc_validate_hd_fat_partition endp

proc_validate_hd_fs_partition proc near
              ; 29/01/2011
              ; 28/07/2011
              ; 23/07/2011
              ; Input
              ; DL = Hard/Fixed Disk Drive Number
              ; SI = PartitionTable offset
              ;    byte ptr [Last_DOS_DiskNo]
              ; Output
              ; cf=0 -> Validated
              ;    SI = Logical dos drv desc. table
              ;    BX = Singlix FS boot sector buffer
              ;    byte ptr [Last_DOS_DiskNo]
              ; cf=1 -> Not a valid 'Singlix FS' partition
              ; AX, DX, CX, DI -> changed 

              ;mov si, offset PartitionTable
              mov ah, byte ptr [SI][ptFileSystemId]
              
              cmp ah, 0A1h ; SINGLIX FS1 (trfs1) partition
              jne short loc_validate_hd_fs_partition_stc_retn

loc_set_valid_hd_fs_partition_params:
              inc byte ptr [Last_DOS_DiskNo] ; > 1
              xor al, al ; mov al, 0
             ;mov byte ptr [Hard_Disk], dl
              mov bx, offset Logical_DOSDisks
              add bh, byte ptr [Last_DOS_DiskNo] 
              mov byte ptr [BX][LD_DiskType], 2
              mov byte ptr [BX][LD_PhyDrvNo], dl
             ;mov byte ptr [BX][LD_FATType], al ; 0
             ;mov byte ptr [BX][LD_FSType], ah
              mov word ptr [BX][LD_FATType], ax

             ;mov ax, word ptr [SI][ptStartSector]
             ;mov word ptr [BX][LD_StartSector], ax
             ;mov ax, word ptr [SI][ptStartSector]+2
             ;mov word ptr [BX][LD_StartSector]+2, ax

loc_hd_fs_logical_drv_init:
             ; 28/07/2011
              mov word ptr [DRV_INIT_DosDrv], bx
              mov ax, bx
             ;mov dl, byte ptr [BX][LD_PhyDrvNo]
              mov cl, dl
              sub cl, 80h
              xor bh, bh
              mov bl, cl 
              add bx, offset HD_LBAYes
              mov ch, byte ptr [BX]
              mov bx, ax
              mov byte ptr [BX][LD_LBAYes], ch
   
              or ch, ch
              jnz short loc_hd_fs_drv_init_load_bs_lba 

loc_hd_fs_drv_init_load_bs_chs:
              mov dh, byte ptr [SI][ptBeginHead]
              mov cx, word ptr [SI][ptBeginSector]
              mov ax, 0201h    ; Read 1 sector
              mov bx, offset DOSBootSectorBuff
              int 13h
              jc  short loc_validate_hd_fs_partition_err_retn
              jmp short loc_hd_drv_fs_boot_validation

loc_validate_hd_fs_partition_stc_retn:
              stc  
loc_validate_hd_fs_partition_err_retn:
              retn

loc_hd_fs_drv_init_load_bs_lba:
             ; 28/07/2011
             ; dl = Physical Drive Number
             ; cl = Hard disk number - 80h                
              mov al, 40h
              mul cl
              add ax, offset FixedDiskParams ; Disk_hd0
             ;mov dl,byte ptr [BX][LD_PhyDrvNo]  
              mov bx, ax
              cmp byte ptr [BX][DAP_PacketSize], 10h
             ;jne short loc_validate_hd_fs_partition_stc_retn
              jb short loc_validate_hd_fs_partition_err_retn
              mov word ptr [BX][DAP_Destination], offset DOSBootSectorBuff
              push ds
              pop word ptr [BX][DAP_Destination]+2
              mov byte ptr [BX][DAP_NumOfBlocks],1
              mov di, bx
              add si, ptStartSector
              add di, DAP_LBA_Address
              movsw
              movsw
              mov di, bx
              mov si, di ; mov si, bx
              add si, DAP_PacketSize ; DS:SI= DAP Location
              mov bx, offset DOSBootSectorBuff
              mov ah, 42h  ; Extended Disk Read - LBA Read
              int 13h
              jc  short loc_validate_hd_fs_partition_err_retn

loc_hd_drv_fs_boot_validation:
             ; 28/07/2011 
             ;mov si, offset DOSBootSectorBuff
              mov si, bx ; Boot sector buffer
              cmp word ptr [SI][BS_Validation], 0AA55h
              jne short loc_validate_hd_fs_partition_stc_retn
             ;Singlix FS Extensions to TR-DOS 7/6/2009 
              cmp word ptr [SI][bs_FS_Identifier], 'SF'
              jne short loc_validate_hd_fs_partition_stc_retn
             ;'A1h' check is not necessary if 'FS' check is passed as OK/Yes.
              cmp byte ptr [SI][bs_FS_PartitionID], 0A1h
              jne short loc_validate_hd_fs_partition_stc_retn

              mov di, word ptr [DRV_INIT_DosDrv]
              mov al, byte ptr [SI][bs_FS_LBA_Ready]
              mov byte ptr [DI][LD_FS_LBAYes], al

            ; 03/01/2010 CHS -> DOS FAT/BPB compatibility fix
              mov al, byte ptr [SI][bs_FS_MediaAttrib]
              mov byte ptr [DI][LD_FS_MediaAttrib], al
             
              mov al, byte ptr [SI][bs_FS_VersionMaj]
              mov byte ptr [DI][LD_FS_VersionMajor], al

              mov ax, word ptr [SI][bs_FS_BytesPerSec]
              mov word ptr [DI][LD_FS_BytesPerSec], ax

  	      mov al, byte ptr [SI][bs_FS_SecPerTrack]
              cbw
              mov word ptr [DI][LD_FS_SecPerTrack], ax
              mov al, byte ptr [SI][bs_FS_Heads]
             ;cbw
              mov word ptr [DI][LD_FS_NumHeads], ax
              ;
              mov ax, word ptr [SI][bs_FS_UnDelDirD]
              mov dx, word ptr [SI][bs_FS_UnDelDirD]+2
              mov word ptr [DI][LD_FS_UnDelDirD], ax
              mov word ptr [DI][LD_FS_UnDelDirD]+2, dx
              mov ax, word ptr [SI][bs_FS_MATLocation]
              mov dx, word ptr [SI][bs_FS_MATLocation]+2
              mov word ptr [DI][LD_FS_MATLocation], ax
              mov word ptr [DI][LD_FS_MATLocation]+2, dx
              mov ax, word ptr [SI][bs_FS_RootDirD]
 	      mov dx, word ptr [SI][bs_FS_RootDirD]+2
              mov word ptr [DI][LD_FS_RootDirD], ax
              mov word ptr [DI][LD_FS_RootDirD]+2, dx
 	      mov ax, word ptr [SI][bs_FS_BeginSector]
 	      mov dx, word ptr [SI][bs_FS_BeginSector]+2
              mov word ptr [DI][LD_FS_BeginSector], ax
              mov word ptr [DI][LD_FS_BeginSector]+2, dx  
              mov ax, word ptr [SI][bs_FS_VolumeSize]
 	      mov dx, word ptr [SI][bs_FS_VolumeSize]+2
              mov word ptr [DI][LD_FS_VolumeSize], ax
              mov word ptr [DI][LD_FS_VolumeSize]+2, dx

              mov ax, word ptr [DI][LD_FS_MATLocation]
              mov dx, word ptr [DI][LD_FS_MATLocation]+2
              add ax, word ptr [DI][LD_FS_BeginSector]
              adc dx, word ptr [DI][LD_FS_BeginSector]+2
              mov si, di ; 29/07/2011
read_hd_fs_MAT_sector:
             ;mov bx, offset DOSBootSectorBuff
              mov cx, 1
              call proc_disk_read
              jc short loc_validate_hd_fs_partition_retn
              ; DI will not be changed
              mov si, bx
use_hdfs_mat_sector_params:
              mov ax, word ptr [SI][FS_MAT_DATLocation]
              mov dx, word ptr [SI][FS_MAT_DATLocation]+2                                  
 	      mov word ptr [DI][LD_FS_DATLocation], ax
              mov word ptr [DI][LD_FS_DATLOcation]+2, dx

 	      mov ax, word ptr [SI][FS_MAT_DATScount]
              mov dx, word ptr [SI][FS_MAT_DATScount]+2                                  
 	      mov word ptr [DI][LD_FS_DATSectors], ax
              mov word ptr [DI][LD_FS_DATSectors]+2, dx

              mov ax, word ptr [SI][FS_MAT_FreeSectors]
              mov dx, word ptr [SI][FS_MAT_FreeSectors]+2                                  
 	      mov word ptr [DI][LD_FS_FreeSectors], ax
              mov word ptr [DI][LD_FS_FreeSectors]+2, dx

              mov ax, word ptr [SI][FS_MAT_FirstFreeSector]
              mov dx, word ptr [SI][FS_MAT_FirstFreeSector]+2                                  
 	      mov word ptr [DI][LD_FS_FirstFreeSector], ax
              mov word ptr [DI][LD_FS_FirstFreeSector]+2, dx

 	      mov ax, word ptr [DI][LD_FS_RootDirD]
              mov dx, word ptr [DI][LD_FS_RootDirD]+2
              add ax, word ptr [DI][LD_FS_BeginSector]
              adc dx, word ptr [DI][LD_FS_BeginSector]+2
              mov si, di ; 29/07/2011   
read_hd_fs_RDT_sector:
             ;mov bx, offset DOSBootSectorBuff
              mov cx, 1
              call proc_disk_read
              jc short loc_validate_hd_fs_partition_retn
             ; DI will not be changed
              mov si, bx ; 29/07/2011
use_hdfs_RDT_sector_params:
              mov ax, word ptr [SI][FS_RDT_VolumeSerialNo]
              mov dx, word ptr [SI][FS_RDT_VolumeSerialNo]+2                                  
              mov word ptr [DI][LD_FS_VolumeSerial], ax
              mov word ptr [DI][LD_FS_VolumeSerial]+2, dx

              push di
              mov cx, 32
              add si, FS_RDT_VolumeName
              add di, LD_FS_VolumeName
              rep movsw
              pop si
            
              mov byte ptr [SI][LD_FS_MediaChanged], 6  ; Volume Name Reset
          
             ;mov cl, byte ptr [Last_DOS_DiskNo] 
             ;add cl, "A"
             ;mov byte ptr [SI][LD_FS_Name], cl

loc_validate_hd_fs_partition_retn:
              retn

proc_validate_hd_fs_partition endp


proc_load_masterboot proc near
                ; input -> dl = drive number
                xor ah,ah
                int 13h
                jnc short pass_reset_error
harddisk_error:
                retn
pass_reset_error:
                mov bx, offset MasterBootBuff
                mov ax,0201h
                mov cx,1
                xor dh,dh
                push ds
                pop es
                int 13h
                jc short harddisk_error

                cmp word ptr [MBIDCode],0AA55h
                jnz short loc_not_masterboot
                retn
loc_not_masterboot:
                stc
                retn

proc_load_masterboot endp

get_free_FAT_sectors proc near
                ; 11/07/2010 modification beforr get_next_cluster
                ;            unnecessary/duplicated fat buffer
                ;            loading code removed 
                ;            because of allocation will be done
                ;            in get_next_cluster procedure
                ; 23/09/2009 get_next_cluster call modification
                ; 05/07/2009
                ; INPUT: DS:SI = Logical DOS Drv Parameters Table
                ; OUTPUT: STC => Error
                ; clc and DX:AX = Free FAT sectors
                ; Also, related parameters and FAT buffer will be reset and updated
                ; Erdogan Tan, 21-06-2009
   
                cmp byte ptr [SI][LD_FATType], 2
                jna short loc_gfc_get_fat_free_clusters

                mov ax, word ptr [SI][LD_StartSector]
                mov dx, word ptr [SI][LD_StartSector]+2
                add ax, word ptr [SI][LD_BPB][BPB_FSInfo]
                adc dx, 0
               ;push ds
               ;pop es
                mov bx, offset DOSBootSectorBuff
                mov cx, 1
                call proc_disk_read
                jnc short loc_gfc_check_fsinfo_signs
retn_gfc_get_fsinfo_sec:
                retn

loc_gfc_check_FSINFO_signs:
                cmp word ptr [DOSBootSectorBuff], 5252h
                jne short retn_gfc_get_fsinfo_stc
                cmp word ptr [DOSBootSectorBuff]+2, 4161h
                jne short retn_gfc_get_fsinfo_stc
               ;mov bx, offset DOSBootSectorBuff
                add bx, 484
                cmp word ptr [BX], 7272h
                jne short retn_gfc_get_fsinfo_stc
                cmp word ptr [BX]+2, 6141h
                jne short retn_gfc_get_fsinfo_stc
                add bx, 4
                push word ptr [BX]
                pop ax
                push word ptr [BX]+2
                pop dx

                jmp short retn_from_get_free_fat32_clusters

retn_gfc_get_fsinfo_stc:
                stc
                retn

loc_gfc_get_fat_free_clusters:
                or dx, dx
                jnz short retn_gfc_get_fsinfo_stc
                mov ax, 2
; 11/07/2010 -> FAT buffer check, allocation, loading
;                   code removed
               ;mov word ptr [FAT_CurrentCluster], ax
               ;mov word ptr [FAT_CurrentCluster]+2, dx
; 11/07/2010
loc_gfc_loop_get_next_cluster:
                call proc_get_next_cluster
                jnc short loc_gfc_free_fat_clusters_cont
                cmp ax, 0
                jna short loc_gfc_pass_inc_free_cluster_count
 
retn_from_get_free_fat_clusters:
                mov ax, word ptr [SI][LD_FreeSectors]
                mov dx, word ptr [SI][LD_FreeSectors]+2
 	       ;push ds
               ;pop es
retn_from_get_free_fat32_clusters:
                xor bh, bh
                mov bl, byte ptr [SI][LD_BPB][BPB_SecPerClust]
                call proc_Mul32
               ;mov word ptr [SI][LD_FreeSectors], ax
	       ;mov word ptr [SI][LD_FreeSectors]+2, dx

retn_get_free_sectors_calc:
                retn

loc_gfc_free_fat_clusters_cont:
                cmp ax, 0
                ja short loc_gfc_pass_inc_free_cluster_count
                inc word ptr [SI][LD_FreeSectors]
                 
loc_gfc_pass_inc_free_cluster_count:
                mov ax, word ptr [FAT_CurrentCluster]
                mov dx, word ptr [FAT_CurrentCluster]+2
                
               ;cmp dx, word ptr [SI][LD_Clusters]+2
               ;ja short retn_from_get_free_fat_clusters
               ;jb short pass_gfc_last_cluster_ax_check
                cmp ax, word ptr [SI][LD_Clusters]
                ja short retn_from_get_free_fat_clusters
pass_gfc_last_cluster_ax_check:
                inc ax
               ;add ax, 1
               ;adc dx, 0
                mov word ptr [FAT_CurrentCluster], ax
               ;mov word ptr [FAT_CurrentCluster]+2, dx
                jmp short loc_gfc_loop_get_next_cluster

get_free_FAT_sectors endp

floppy_drv_init proc near
                ; 24/07/2011
                ; 05/07/2011 -> Segment boundary check modification
                ; 08/08/2010 -> Volume Serial Number return
                ; 03/01/2010 -> Singlix FS CHS/BPB disk_io fixup
                ;               (SecPerTrack, Heads) 
                ; 26/09/2009
                ; 20/09/2009 (call proc_dparam <- drv_init) 
                ; 04/07/2009 
                ; INPUT -> dl = Drive Number (0,1)
                ; OUTPUT -> bl = drive name,
                ;           bh = drive number
                ;           DS:SI = Dos drv description table
                ; 08/08/2010
                ;           DX:AX = Volume Serial Number
                ;           ES=DS  
                ; 12/09/2009
                ; OUTPUT -> bh&bl instead of ah&al (4-7-2009) 
                ; 04/07/2009 

               ;xor dl,dl  (drv_init_old)
                push dx
                mov si, offset FloppyDiskParams
                or dl, dl
                jz short loc_get_fdparam
               ;cmp dl, 1
               ;ja short read_fd_boot_sector_stc_retn
                add si, 40h
loc_get_fdparam:
                call proc_dparam
              ; mov dl, byte ptr [SI][DPDisks] (drv_init_old)
              ; add si, 40h (drv_init_old)
              ; cmp dl, 2 (drv_init_old)
              ; jne short pass_floppy1 (drv_init_old)
              ; dec dl (drv_init_old)
              ; call proc_dparam (drv_init_old)
;pass_floppy1: (drv_init_old)
                pop dx
                mov si, offset Disk_fd0
                mov di, offset Logical_DOSDisks
                test dl, dl
                jz short pass_drv_init_fd1
                add si, 40h
                add di, 100h
pass_drv_init_fd1:
                mov byte ptr [DI][LD_MediaChanged],0
                cmp byte ptr [SI][DPDType], 1
                jb short read_fd_boot_sector_retn
                mov byte ptr [DI][LD_PhyDrvNo], dl
read_fd_boot_sector:
                xor dh, dh
                push ds
                pop es
                mov cx, 4 ; Retry Count
read_fd_boot_sector_again:
                push cx
                mov cx, 1
                mov ax, 0201h    ; Read 1 sector
                mov bx, offset DOSBootSectorBuff
                int 13h
                pop cx
                jnc short use_fd_boot_sector_params
                loop read_fd_boot_sector_again
read_fd_boot_sector_stc_retn:
                stc
read_fd_boot_sector_retn:
                retn
use_fd_boot_sector_params:
               ;mov si, offset DOSBootSectorBuff
                mov si, bx

                cmp word ptr [SI][BS_Validation], 0AA55h
                jne short read_fd_boot_sector_stc_retn
                cmp word ptr [SI][bs_FS_Identifier], 'SF'
                jne pass_use_fsfd_boot_sector_params

                mov al, byte ptr [SI][bs_FS_LBA_Ready]
                mov byte ptr [DI][LD_FS_LBAYes], al

		; 03/01/2010 CHS -> DOS FAT/BPB compatibility fix
                mov al, byte ptr [SI][bs_FS_MediaAttrib]
                mov byte ptr [DI][LD_FS_MediaAttrib], al
             
                mov al, byte ptr [SI][bs_FS_VersionMaj]
                mov byte ptr [DI][LD_FS_VersionMajor], al

	        mov ax, word ptr [SI][bs_FS_BytesPerSec]
                mov word ptr [DI][LD_FS_BytesPerSec], ax

  	        mov al, byte ptr [SI][bs_FS_SecPerTrack]
                cbw
                mov word ptr [DI][LD_FS_SecPerTrack], ax
                mov al, byte ptr [SI][bs_FS_Heads]
               ;cbw
                mov word ptr [DI][LD_FS_NumHeads], ax
                ;

                mov ax, word ptr [SI][bs_FS_UnDelDirD]
                mov dx, word ptr [SI][bs_FS_UnDelDirD]+2
                mov word ptr [DI][LD_FS_UnDelDirD], ax
                mov word ptr [DI][LD_FS_UnDelDirD]+2, dx
                mov ax, word ptr [SI][bs_FS_MATLocation]
                mov dx, word ptr [SI][bs_FS_MATLocation]+2
                mov word ptr [DI][LD_FS_MATLocation], ax
                mov word ptr [DI][LD_FS_MATLocation]+2, dx
                mov ax, word ptr [SI][bs_FS_RootDirD]
 		mov dx, word ptr [SI][bs_FS_RootDirD]+2
                mov word ptr [DI][LD_FS_RootDirD], ax
                mov word ptr [DI][LD_FS_RootDirD]+2, dx
 		mov ax, word ptr [SI][bs_FS_BeginSector]
 		mov dx, word ptr [SI][bs_FS_BeginSector]+2
                mov word ptr [DI][LD_FS_BeginSector], ax
                mov word ptr [DI][LD_FS_BeginSector]+2, dx  
                mov ax, word ptr [SI][bs_FS_VolumeSize]
 		mov dx, word ptr [SI][bs_FS_VolumeSize]+2
                mov word ptr [DI][LD_FS_VolumeSize], ax
                mov word ptr [DI][LD_FS_VolumeSize]+2, dx
		
                mov si, di
 		mov ax, word ptr [SI][LD_FS_MATLocation]
                mov dx, word ptr [SI][LD_FS_MATLocation]+2
read_fd_MAT_sector_again:
                ;mov bx, offset DOSBootSectorBuff
                mov cx, 1
                call proc_chs_read
                mov si, bx
                jnc short use_fdfs_mat_sector_params
                retn
                ;jc short read_fd_boot_sector_retn
use_fdfs_mat_sector_params:
 		mov ax, word ptr [SI][FS_MAT_DATLocation]
                mov dx, word ptr [SI][FS_MAT_DATLocation]+2                                  
 		mov word ptr [DI][LD_FS_DATLocation], ax
                mov word ptr [DI][LD_FS_DATLOcation]+2, dx

		mov ax, word ptr [SI][FS_MAT_DATScount]
                mov dx, word ptr [SI][FS_MAT_DATScount]+2                                  
 		mov word ptr [DI][LD_FS_DATSectors], ax
                mov word ptr [DI][LD_FS_DATSectors]+2, dx

                mov ax, word ptr [SI][FS_MAT_FreeSectors]
                mov dx, word ptr [SI][FS_MAT_FreeSectors]+2                                  
 		mov word ptr [DI][LD_FS_FreeSectors], ax
                mov word ptr [DI][LD_FS_FreeSectors]+2, dx

 		mov ax, word ptr [SI][FS_MAT_FirstFreeSector]
                mov dx, word ptr [SI][FS_MAT_FirstFreeSector]+2                                  
 		mov word ptr [DI][LD_FS_FirstFreeSector], ax
                mov word ptr [DI][LD_FS_FirstFreeSector]+2, dx

              	mov si, di
 		mov ax, word ptr [SI][LD_FS_RootDirD]
                mov dx, word ptr [SI][LD_FS_RootDirD]+2
read_fd_RDT_sector_again:
                ;mov bx, offset DOSBootSectorBuff
            	mov cx, 1
                call proc_chs_read
                mov si, bx
                jc short read_fd_RDT_sector_retn

use_fdfs_RDT_sector_params:
                mov ax, word ptr [SI][FS_RDT_VolumeSerialNo]
                mov dx, word ptr [SI][FS_RDT_VolumeSerialNo]+2                                  
                mov word ptr [DI][LD_FS_VolumeSerial], ax
                mov word ptr [DI][LD_FS_VolumeSerial]+2, dx

                push di
                mov cx, 32
                add si, FS_RDT_VolumeName
                add di, LD_FS_VolumeName
                rep movsw
                pop si

                mov byte ptr [SI][LD_FATType], 0
                mov byte ptr [SI][LD_FSType], 0A1h  
                
                jmp loc_cont_use_fd_boot_sector_params

read_fd_RDT_sector_stc_retn:
                stc
read_fd_RDT_sector_retn:
                retn

pass_use_fsfd_boot_sector_params:
		cmp byte ptr [SI][BS_BootSig], 29h
                jne short read_fd_RDT_sector_stc_retn
                cmp byte ptr [SI][BPB_Media], 0F0h
                jb  short read_fd_RDT_sector_retn

                push di
                add di, LD_BPB
                mov cx, 32
                rep movsw 
                pop si
                xor ax, ax
                xor dx, dx
                mov Word Ptr [SI][LD_StartSector], AX
                mov Word Ptr [SI][LD_StartSector]+2,DX
                mov ax,Word Ptr [SI][LD_BPB][BPB_RsvdSecCnt]
                mov Word Ptr [SI][LD_FATBegin], AX
                mov Word Ptr [SI][LD_FATBegin]+2, DX
                mov al,Byte Ptr [SI][LD_BPB][BPB_NumFATs] 
                cbw                             
                mul Word Ptr [SI][LD_BPB][BPB_FATSz16]
                add ax,Word Ptr [SI][LD_FATBegin]
                mov Word Ptr [SI][LD_ROOTBegin], AX
                mov Word Ptr [SI][LD_ROOTBegin]+2, DX
                mov Word Ptr [SI][LD_DATABegin], AX 
                mov Word Ptr [SI][LD_DATABegin]+2, DX
                mov ax,20h   ; Size of a directory entry
                mul Word Ptr [SI][LD_BPB][BPB_RootEntCnt]
                add ax,511
                mov cx,512
                div cx
                add Word Ptr [SI][LD_DATABegin], AX 
               ;adc Word Ptr [SI][LD_DATABegin]+2, 0 
                mov ax,Word Ptr [SI][LD_BPB][BPB_TotalSec16]
                xor dx,dx
                mov Word Ptr [SI][LD_TotalSectors], AX
                mov Word Ptr [SI][LD_TotalSectors]+2,DX
                sub ax,Word Ptr [SI][LD_DATABegin]
                xor ch,ch
                mov cl,Byte Ptr [SI][LD_BPB][BPB_SecPerClust]
                div cx
                ;xor dx, dx 
                mov word ptr [SI][LD_Clusters], AX
                mov word ptr [SI][LD_Clusters]+2, DX
                ;mov word ptr [SI][LD_Clusters]+2, 0  
                    ; Maximum Valid Cluster Number= AX +1
                    ; with 2 reserved clusters= AX +2
               
allocate_FAT12_buffer:                
               ;mov byte ptr [FAT_BuffValidData], 0
               ;mov byte ptr [FAT_BuffDrvName], 0
               ;mov word ptr [FAT_BuffSector], 0
               ;mov word ptr [FAT_BuffSector]+2, 0
                mov byte ptr [FAT_BuffValidData], dh
                mov byte ptr [FAT_BuffDrvName], dh
                mov word ptr [FAT_BuffSector], dx
                mov word ptr [FAT_BuffSector]+2, dx    
               
                mov bx, word ptr [FAT_Buffer]
                cmp bx, 0
                ja short pass_allocate_FAT12_buffer
                
                ; bx = 0 => Allocate the first free segment
                mov cx, 1536 ; FAT Buffer contains 3 FAT sectors 
                mov ax, 0103h ; Buffer, AL= 3, FAT buffer allocation
                mov dx, 1 ; 512 byte segment boundary check
                call proc_allocate_memory
                jc short read_fd_FAT_sectors_retn
                mov word ptr [FAT_Buffer], bx

pass_allocate_FAT12_buffer:
                mov es, bx
                xor bx, bx
read_fd_FAT_sectors_again:
                mov ax, word ptr [SI][LD_BPB][BPB_RsvdSecCnt]
                xor dx, dx 
                mov cx, 3
                call proc_chs_read
                jnc short use_fd_FAT_sectors
read_fd_FAT_sectors_push_ds_pop_es_retn:
                push ds
                pop es
read_fd_FAT_sectors_retn:
                retn            

use_fd_FAT_sectors:
                mov al, byte ptr [SI][LD_PhyDrvNo]
                add al, 'A'               
                mov byte ptr [FAT_BuffDrvName], al 
 
                mov byte ptr [FAT_BuffValidData], 1
                call fd_init_calculate_free_clusters
                jc short read_fd_FAT_sectors_push_ds_pop_es_retn
                
loc_cont_use_fd_boot_sector_params_FAT:
                mov byte ptr [SI][LD_FATType], 1
                mov byte ptr [SI][LD_FSType], 1
               ; 08/08/2010
                mov ax, word ptr [SI][LD_BPB][VolumeID]
                mov dx, word ptr [SI][LD_BPB][VolumeID]+2
               ;
loc_cont_use_fd_boot_sector_params:
                mov bh, byte ptr [SI][LD_PhyDrvNo]
                mov byte ptr [SI][LD_DParamEntry], bh
                mov bl, bh
                add bl, 'A'
                mov byte ptr [SI][LD_Name], bl
                mov byte ptr [SI][LD_DiskType], 1
                mov byte ptr [SI][LD_LBAYes], 0
                mov byte ptr [SI][LD_PartitionEntry], 0
                mov byte ptr [SI][LD_MediaChanged], 6 ; Volume Name Reset
                
                jmp short read_fd_FAT_sectors_push_ds_pop_es_retn

floppy_drv_init endp

fd_init_calculate_free_clusters proc near
               ; 04/07/2009

                mov ax, 2
              ; mov dx, 0
              ; xor dx, dx 

                mov word ptr [FAT_CurrentCluster]+2, dx
		mov word ptr [SI][LD_FreeSectors], dx
                mov word ptr [SI][LD_FreeSectors]+2, dx
               
fd_init_loop_get_next_cluster:
                call fd_init_get_next_cluster
		jc short fd_init_calculate_free_clusters_retn

fd_init_free_fat_clusters:
                cmp ax, 0
                ja short fd_init_pass_inc_free_cluster_count
                inc word ptr [SI][LD_FreeSectors]
            
fd_init_pass_inc_free_cluster_count:
                mov ax, word ptr [FAT_CurrentCluster]
                mov dx, word ptr [FAT_CurrentCluster]+2
                
                cmp ax, word ptr [SI][LD_Clusters]
                ja short short retn_from_fd_init_calculate_free_clusters

                inc ax
                jmp short fd_init_loop_get_next_cluster

retn_from_fd_init_calculate_free_clusters:
                mov cx, word ptr [SI][LD_FreeSectors] ; Count of free clusters
                mov al, byte ptr [SI][LD_BPB][BPB_SecPerClust]
                cbw
                mul cx
                mov word ptr [SI][LD_FreeSectors], ax
		
fd_init_calculate_free_clusters_retn:
                retn

fd_init_calculate_free_clusters endp

fd_init_get_next_cluster proc near
               ; 04/07/2009  
 		mov word ptr [FAT_CurrentCluster], ax
fd_init_get_next_cluster_readnext:
                mov bx, 400h ;1024
                div bx
                ; AX = Count of 3 FAT sectors
                ; DX = Buffer Entry Offset
                push ax
                mov ax, 3
                mul dx    ; Multiply by 3
                shr ax, 1 ; Divide by 2
                mov bx, ax ; Buffer Byte Offset
                pop ax
                mov dx, 3
                mul dx 
                
                ; AX = FAT Beginning Sector
                ; DX = 0
                mov cl, byte ptr [SI][LD_Name]
              ; cmp byte ptr [FAT_BuffValidData], 0
              ; jna short fd_init_load_FAT_sectors0
                cmp cl, byte ptr [FAT_BuffDrvName]
                jne short fd_init_load_FAT_sectors0
                cmp ax, word ptr [FAT_BuffSector]
                jne short fd_init_load_FAT_sectors1
                mov ax, word ptr [FAT_CurrentCluster]
                shr ax, 1
               ;mov ax, word ptr [FAT_Buffer]
               ;mov es, ax
                mov ax, word ptr ES:[BX]
                jnc short fd_init_gnc_even
                shr ax, 1
                shr ax, 1
                shr ax, 1
                shr ax, 1

fd_init_gnc_clc_retn:
                clc
                retn

fd_init_gnc_even:
                and ah,0Fh
                retn

fd_init_load_FAT_sectors0:
                mov byte ptr [FAT_BuffDrvName], cl
fd_init_load_FAT_sectors1:
		mov byte ptr [FAT_BuffValidData], 0
                mov word ptr [FAT_BuffSector]+2, dx
                mov word ptr [FAT_BuffSector], ax
                add ax, word ptr [SI][LD_FATbegin]
               ;adc dx, word ptr [SI][LD_FATbegin]+2
               ;xor dx, dx 
               ;mov bx, word ptr [FAT_Buffer]
               ;mov es, bx
                xor bx, bx
                mov cx, word ptr [SI][LD_BPB][BPB_FATSz16]
                sub cx, word ptr [FAT_BuffSector]
             
                cmp cx, 3
                jna short fdinit_pass_fix_sector_count_3
                mov cx, 3
fdinit_pass_fix_sector_count_3:  
                call proc_chs_read
                jnc short fd_init_FAT_sectors_no_load_error
                mov byte ptr [FAT_BuffValidData], 0
                mov ax, 15h ; Drv not ready or read Error !
                ;mov dx, 0
                ;xor dx, dx 
                retn
fd_init_FAT_sectors_no_load_error:
                mov byte ptr [FAT_BuffValidData], 1
                mov ax, word ptr [FAT_CurrentCluster]
               ;mov dx, word ptr [FAT_CurrentCluster]+2
                jmp fd_init_get_next_cluster_readnext

fd_init_get_next_cluster endp

proc_get_FAT_volume_name proc near
                ; Erdogan Tan, 12/09/2009
                ; 12/07/2009, 23/08/2009, 05/09/2009, 08/09/2009
                ; INPUT -> BH= Logical DOs Drive Number (0,1,2,3,4 ...)
                ;          BL= 0
                ; OUTPUT -> CF = 0 -> DS:SI = Volume Name Address
                ; CF = 1 -> Root volume name not found


               ;mov 	ah, 0FFh
               ;mov 	al, byte ptr [Last_Dos_DiskNo]
               ;cmp 	al, bh
               ;jb      short loc_gfvn_dir_load_err

                ;xor    bl, bl
                
                mov     si, offset Logical_DOSdisks
                add     si, bx
                mov     al, byte ptr [SI][LD_Name]
                mov     ah, byte ptr [SI][LD_FATType]
                cmp     ah, 1
                jb    	short loc_gfvn_dir_load_err
                cmp 	al, 'A'
                jb      short loc_gfvn_dir_load_err

                cmp 	ah, 2 
                ja      short get_FAT32_root_cluster

                call    proc_load_FAT_root_directory
                jnc     short loc_get_volume_name

loc_gfvn_dir_load_err:
                
                retn

get_FAT32_root_cluster:
                mov     ax, word ptr [SI][LD_BPB][BPB_RootClus]
                mov     dx, word ptr [SI][LD_BPB][BPB_RootClus]+2
                call    proc_load_FAT_sub_directory
                jc      short loc_get_volume_name_retn

loc_get_volume_name:
                mov     cx, word ptr [Directory_Buffer]
                mov     ds, cx 
                xor 	cx, cx
                mov     si, cx
                             
check_root_volume_name:
                mov     ax, word ptr [SI]
                or      al, al
                jz      short loc_get_volume_name_retn_pop_ds
                cmp     byte ptr [SI]+0Bh, 08h
                je      short loc_get_volume_name_retn
                cmp     cx, word ptr CS:[DirBuff_LastEntry]
                jnb     short pass_check_root_volume_name
                inc     cx
                add     si, 32
                jmp     short check_root_volume_name

loc_get_volume_name_retn_pop_ds:
                push 	cs
                pop 	ds

loc_get_volume_name_retn:
                retn
                                
pass_check_root_volume_name:
		push 	cs
                pop 	ds 
                cmp     byte ptr [DirBuff_FATType], 3
                jb	short loc_get_volume_name_retn_xor

               ;xor     ax, ax
               ;mov 	byte ptr [FAT_BuffValidData], ah
               ;mov 	byte ptr [FAT_BuffDrvName], ah
               ;mov 	word ptr [FAT_BuffSector], ax
               ;mov 	word ptr [FAT_BuffSector]+2, ax                 

                mov 	bx, word ptr [FAT_Buffer]
               ;cmp 	bx, 0
               ;ja 	short gfvn_pass_allocate_FAT_buffer
                
                ; bx = 0 => Allocate the first free segment
               ;mov 	cx, 1536 ; FAT Buffer contains 3 FAT sectors 
               ;mov 	ax, 0103h ; Buffer, AL= 3, FAT buffer allocation
               ;mov     dx, 1 ; 512 byte segment boundary check
               ;call 	proc_allocate_memory
               ;jc 	short loc_get_volume_name_retn
               ;mov 	word ptr [FAT_Buffer], bx

;gfvn_pass_allocate_FAT_buffer:
                mov 	es, bx
                xor 	bx, bx 
;read_gfvn_FAT_sectors_again:
;		mov 	ax, word ptr [SI][LD_BPB][BPB_HiddSec]
;		mov 	dx, word ptr [SI][LD_BPB][BPB_HiddSec]+2
;               add 	ax, word ptr [SI][LD_BPB][BPB_RsvdSecCnt]
;               adc 	dx, 0
;               mov 	cx, 3
;               call 	proc_disk_read
;               push 	ds
;               pop 	es
;               jc 	short loc_get_volume_name_retn
                
	       ;mov 	bx, word ptr [FAT_Buffer]
               ;mov     es, bx
               ;xor     bx, bx

                mov     si, offset Logical_DosDisks
                mov     ah, byte ptr [DirBuff_Drv]
                sub     ah, 'A' 
                xor     al, al 
                add     si, ax
               ;push    si
              
                mov     ax, word ptr [DirBuff_Cluster]
                mov     dx, word ptr [DirBuff_Cluster]+2
   
                call 	proc_get_next_cluster
                
                push 	ds
                pop 	es
               ;pop     si
                jnc 	short loc_gfvn_load_FAT32_dir_cluster
                
                cmp     ax, 1
                cmc
                retn
                
loc_gfvn_load_FAT32_dir_cluster:
                call    proc_load_FAT_sub_directory
                jnc     short loc_get_volume_name
                retn

loc_get_volume_name_retn_xor:
                xor 	ax, ax
                retn

proc_get_FAT_volume_name endp

proc_get_media_change_status proc near
                ; 9/9/2009
                ; INPUT: DL = Drive
                ; OUTPUT: clc & AH = 6 media changed
                ;         clc & AH = 0 media not changed         
                ;         stc -> Drive not ready or an error 
                mov ah, 16h
                int 13h
                cmp ah, 06h
                je short loc_gmc_status_retn
                or ah, ah
                jz short loc_gmc_status_retn
loc_gmc_status_stc_retn:                  
                stc
loc_gmc_status_retn:
		retn
proc_get_media_change_status endp


HardDisks:     db 0
HDCounter:     db 1

MasterBootBuff:
MasterBootCode: db 1BEh dup (?)
PartitionTable: db 64 dup (?)
MBIDCode: dw ?

PTable_Buffer:
PTable_hd0: db 64 dup (0)
PTable_hd1: db 64 dup (0)
PTable_hd2: db 64 dup (0)
PTable_hd3: db 64 dup (0)
PTable_ep0: db 64 dup (0)
PTable_ep1: db 64 dup (0)
PTable_ep2: db 64 dup (0)
PTable_ep3: db 64 dup (0)

DiskParams:
FloppyDiskParams:
Disk_fd0: db 16 dup (0)
DAP_fd0: db 16 dup(0)
GDP_fd0: db 26 dup(0)
TRDP_fd0: db 6 dup(0)
Disk_fd1: db 16 dup (0)
DAP_fd1: db 16 dup(0)
GDP_fd1: db 26 dup(0)
TRDP_fd1: db 6 dup(0)
FixedDiskParams:
Disk_hd0: db 16 dup (0)
DAP_hd0: db 16 dup(0)
GDP_hd0: db 26 dup(0)
TRDP_hd0: db 6 dup(0)
Disk_hd1: db 16 dup (0)
DAP_hd1: db 16 dup(0)
GDP_hd1: db 26 dup(0)
TRDP_hd1: db 6 dup(0)
Disk_hd2: db 16 dup (0)
DAP_hd2: db 16 dup(0)
GDP_hd2: db 26 dup(0)
TRDP_hd2: db 6 dup(0)
Disk_hd3: db 16 dup (0)
DAP_hd3: db 16 dup(0)
GDP_hd3: db 26 dup(0)
TRDP_hd3: db 6 dup(0)
end_of_dparams_buff:

DOSBootSectorBuff:
db 512 dup(0)

Last_DOS_DiskNo:   db 1
HD_LBAYes:         dd 0
