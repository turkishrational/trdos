; *****************************************************************************
;
; DRV_INIT0.ASM  [ SINGLIX Drive Initialization Procedures ]
; Copyright (C) 2009 Erdogan TAN  [ 28/03/2009 ]
;
; DRV_INIT.ASM  [ TRDOS Kernel Drive Initialization Procedures ]
; Copyright (C) 2005 Erdogan TAN  [ 29/01/2005 ]
;
; TRDOS.ASM (include drv_init.asm)
;
; *****************************************************************************

proc_drv_init   proc    near

                mov dl, 80h
                mov si, offset DiskParams
                call proc_dparam
                jnc short pass_hd_15h_error
                mov word ptr [si][DPNumOfSecs], 0
                mov word ptr [si][DPNumOfSecs]+2,0
pass_hd_15h_error:
                mov ah, byte ptr [SI][DPDisks]
                mov byte ptr [HDCounter], ah
next_hard_disk:
                dec byte ptr [HDCounter]
                cmp byte ptr [HDCounter], 0
                jna short load_hd_partition_tables
                inc dl
                add si, 40h
                call proc_dparam
                jnc short next_hard_disk
                mov word ptr [si][DPNumOfSecs], 0
                mov word ptr [si][DPNumOfSecs]+2,0
                jmp short next_hard_disk
load_hd_partition_tables:
                mov si, offset Disk_hd0
                mov di, offset PTable_hd0
                push ds
                pop es
                mov dl, 80h
                mov cx, 4
load_next_hd_partition_table:
                push cx
                push dx
                push si
                push di
                cmp byte ptr [SI]+1, 03h
                jne short pass_pt_this_hard_disk
                call proc_load_masterboot
                jc short pass_pt_this_hard_disk
                pop di
                push di
                mov si, offset PartitionTable
                mov cx, 32
                rep movsw
pass_pt_this_hard_disk:
                pop di
                pop si
                pop dx
                pop cx
                inc dl
                add si, 40h
                add di, 40h
                loop load_next_hd_partition_table

                retn

proc_drv_init   endp

proc_dparam   proc near
              ; input
              ; dl = Disk Drive Number
              ; ds:si = Parameters Table Buffer
              ; output
              ; ah = error number (0 = No Error if C flag is ZERO)
              mov byte ptr [si][DPDiskNumber], dl
              mov byte ptr [si][DPDType], 0
              push dx
              mov ah, 08h
              int 13h
              mov byte ptr [si][DPReturn], ah
              jnc short dparam_no_error
              pop dx
              retn
dparam_no_error:
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

              cmp byte ptr [si][DPDiskNumber], 80h
              jb short dparam_15h_return

              mov dl, byte ptr [si][DPDiskNumber]
              mov byte ptr [Hard_Disk], dl
              mov byte ptr [si][DPDType], 0
              mov ah, 15h
              int 13h
              jc short dparam_15h_return
              mov byte ptr [si][DPDType], ah
              mov word ptr [si][DPNumOfSecs], cx
              mov word ptr [si][DPNumOfSecs]+2, dx
dparam_15h_return:
              pop dx ; dl = Drive Number
              mov byte ptr [SI][DAP_PacketSize], 0 ; Reset (No DAP)
              mov ah, 41h ; Check Extensions Present
              mov bx, 55AAh
              int 13h
              jc short dparam_48h_return
              cmp bx, 0AA55h
              jne short dparam_48h_return
              test cx, 01h ; Fixed Disk Access Subset - is LBA ready ? 
              jz short dparam_48h_return
              xor bh, bh
              mov bl, byte ptr [Hard_Disk]
              sub bl, 80h
              mov byte ptr [HD_LBAYes][BX],1
              mov byte ptr [SI][DAP_PacketSize], 10h
dparam_41h_return:
              push si
              add si, GetDParams_48h
              mov word ptr [SI], 0026 ; GDP Data Lenght - Set
            ; DS:SI= Address of Result Buffer
            ; DL (Drive Number) must be not changed before here...
              mov ah, 48h  ; Get Enhanced Disk Drive Parameters
              int 13h
              pop si
              jc short dparam_48h_return
              mov word ptr [SI][GetDParams_48h], 0 ; GDP Data Lenght - Reset
dparam_48h_return:
              xor ah,ah
              retn
proc_dparam   endp

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

DiskParams:
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

Hard_Disk:      db 80h
HD_LBAYes:      dd 0

