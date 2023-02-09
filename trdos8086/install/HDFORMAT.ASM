; ****************************************************************************
; Turkish Rational SINGLIX  
; Disk Operation System v1.0 Project
; Hard Disk FS1 (A1h) Partition Formatting Code : HDFORMAT.COM
;
; Copyright (C) 2006-2010  Erdogan TAN  [ 14/05/2009 ] Last Update: 30/07/2011
; HDFORMAT.ASM [ 05/05/2009, 10/05/2009, 03/01/2010, 21/02/2010 ]
; MULTIXFD.ASM [ 15-1-2006 | 4-3-2007 | 29-3-2008 | 25-10-2008 | 24-3-2009 ]
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

; FS1 BOOT SECTOR
bs_FSystemID equ 7C03h
bs_BytesPerSec equ 7C06h
bs_MediaAttrib equ 7C08h
bs_PartitionID equ 7C09h
bs_FSVersionMaj equ 7C0Ah
bd_FSVersionMin equ 7C0Bh
bs_BeginningSec equ 7C0Ch
bs_VolumeSize equ 7C10h
bs_StartupFD equ 7C014h
bs_MATLocation equ 7C18h
bs_RootDirD equ 7C1Ch
bs_SystemConfFD equ 7C20h
bs_SwapFD equ 7C24h
bs_UndeleteDirD equ 7C28h
bs_DriveNumber equ 7C2Ch
bs_LBA_ready_yes equ 7C2Dh
bs_MagicWord equ 7C2Eh
bs_OperationSys equ 7C30h

; MAT
MAT_DAT_Address equ 12
MAT_FreeSectors equ 20
MAT_FirstFreeSector equ 24

.8086

SINGLIXBOOT     SEGMENT PUBLIC 'CODE'
                assume cs:SINGLIXBOOT,ds:SINGLIXBOOT,es:SINGLIXBOOT,ss:SINGLIXBOOT

                org 100h

SMART_STARTUP_CODE:
                cmp ax, 417   ; Am I Startup File Code or Formatting Code ?...
                jne SINGLIX_WRITESTUB ; Formatting Code

                xor ax, ax
                mov es, ax
                mov dl, byte ptr ES:[bs_DriveNumber]

                ;mov dl, 80h
                mov ah, 41h ; Check INT 13h Extensions Present
                mov bx, 55AAh
                int 13h
                jc loc_sf_lba_not_ready
                cmp bx, 0AA55h
                jne loc_sf_lba_not_ready

                test cx, 01h ; Fixed Disk Access Subset - LBA ready bit 
                jz loc_sf_lba_not_ready

                mov al, dl
                call loc_proc_hex
                mov word ptr [Sf_Drv_Number], ax
                
                mov bh, dl

                mov ax, word ptr ES:[bs_RootDirD]
                mov dx, word ptr ES:[bs_RootDirD]+2
                add ax, word ptr ES:[bs_BeginningSec]
                adc dx, word ptr ES:[bs_Beginningsec]+2

                mov word ptr [DAP_LBA_Address], ax 
                mov word ptr [DAP_LBA_Address]+2, dx
                mov dl, bh
                mov bh, 7Eh
                xor bl, bl 
                mov byte ptr [DAP_PacketSize], 10h
                mov word ptr [DAP_Destination], bx
                mov word ptr [DAP_Destination]+2, es
                mov byte ptr [DAP_NumOfBlocks], 1
                mov byte ptr [sf_RetryCount], 4
                mov si, offset DAP_Table
                call sf_disk_read
                jc failure_load_rootdir

                ;mov bx, 7E00h
 
                cmp word ptr ES:[BX], 'DR'
                jne short failure_load_rootdir
                cmp byte ptr ES:[BX]+2, 'T'
                jne short failure_load_rootdir
                cmp word ptr ES:[BX]+4, 512
                jne short failure_load_rootdir
                mov cx, word ptr ES:[BX]+10h
                cmp cx, 1
                jb short failure_load_rootdir

                mov si, offset msg_sf_volume_name
                call proc_printmsg
                push es
                pop ds
                mov si, 7E40h ; ES:[BX]+40h
                push cs
                pop es
                mov di, offset Str_Volume_Name
                push di
                mov cx, 32
                rep movsw
                push ds
                pop es
                push cs
                pop ds
                pop si
                call proc_printmsg
                mov ax, word ptr ES:[7E1Eh] ;[Volume_Serial_No]
                mov dx, word ptr ES:[7E1Ch] ;[Volume_Serial_No]+2
                push ax 
                call loc_proc_hex
                mov word ptr [msg_sf_Vol_Serial2]+2, ax
                pop ax
                xchg ah,al
                call loc_proc_hex
                mov word ptr [msg_sf_Vol_Serial2], ax
                mov ax, dx
                push ax
                call loc_proc_hex
                mov word ptr [msg_sf_Vol_Serial1]+2, ax
                pop ax
                xchg ah,al
                call loc_proc_hex
                mov word ptr [msg_sf_Vol_Serial1], ax
                mov si, offset msg_sf_vol_serial_header
                call proc_printmsg
loc_sf_reboot:
               ;push ds
               ;pop es 
                xor ax,ax
                int 16h
                int 19h

failure_load_rootdir:
                mov si, offset msg_read_error
                call proc_printmsg 
                jmp short loc_sf_reboot
                
msg_startup_file_int13h_extensions:
                db "Singlix FS1 default startup file running...", 0Dh, 0Ah, 0Dh, 0Ah
                db "DISK READ ERROR: "
                db "LBA not ready ! (This code does not support CHS mode)...", 0Dh, 0Ah, 0Dh, 0Ah, 0   

loc_sf_lba_not_ready:
                mov si, offset msg_startup_file_int13h_extensions
                call proc_printmsg 
                jmp loc_sf_reboot


loc_proc_hex:

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

sf_disk_read:
                ; INPUT -> DAP_Buffer
                ; DL -> Driver Number
                ; DS:SI= DAP Table
                ; OUTPUT -> clc or stc
sf_loc_read_disk_lba:
                mov ah, 42h  ; Extended Disk Read - LBA Read
                int 13h
                jnc short sf_pass_read_disk_lba_error
                dec byte ptr [sf_RetryCount]
                cmp byte ptr [sf_RetryCount],1
                jnb short sf_loc_read_disk_lba
sf_pass_read_disk_lba_error:
                retn

proc_printmsg:
                mov BX, 7
loc_print:
                lodsb                           ; Load byte at DS:SI to AL
                and     AL,AL            
                jz      short loc_return        ; If AL = 00h then return
                mov     AH,0Eh                  
                int     10h                     ; BIOS Service func ( ah ) = 0Eh
                                                ; Write char as TTY
                                                ;AL-char BH-page BL-color
                jmp     short loc_print           
loc_return:
                retn

Rx_Dos_Div32:
                mov  bx, dx
                xchg ax, bx
                xor  dx, dx
                div  cx         ; at first, divide DX
                                ; remainder is in DX 
                xchg ax, bx     ; now quotient is in BX
                                ; and initial DX value is in AX
                div  cx         ; now, DX_AX has been divided and
                                ; AX has quotient
                                ; DX has remainder
                xchg dx, bx     ; finally, BX has remainder
ret_div32:
                retn


sf_RetryCount:  dw 0

Str_Volume_Name: db 65 dup(0)

msg_sf_volume_name:
                db "Singlix FS1 default startup file running on disk "
Sf_Drv_Number:  dw 3030h
                db "h", 0Dh,0Ah, 0Dh, 0Ah
    
                db "Volume Name      : "
                db 0
msg_sf_Vol_Serial_Header:
                db 0Dh, 0Ah
                db "Volume Serial No : "
msg_sf_Vol_Serial2:
                db "0000"
                db "-"
msg_sf_Vol_Serial1: 
                db "0000"
                db 0Dh, 0Ah
                db 0Dh, 0Ah 
                db "Replace disk and press any key to reboot..."
                db 0Dh,0Ah,0Dh,0Ah,0
msg_read_error:
                db 'Disk read error! Press any key to reboot...'
                db 0Dh,0Ah,0Dh,0Ah,0

;BIOS INT 13h Extensions (LBA extensions)
;BIOS Enhanced Disk Drive Specification Version 3.0, Phoenix - 1998
;Disk Address Packet
DAP_Table:
; Offset 0
DAP_PacketSize: db 0    ; BYTE - If extensions present, this byte will be >=10h
; Offset 1
DAP_Reserved1: db 0     ; BYTE - Reserved byte, must be 0 
; Offset 2
DAP_NumOfBlocks: db 0   ; BYTE - Number of blocks to transfer.
                        ;        Value of this byte must be 0 to 127
DAP_Reserved2: db 0     ; BYTE - Reserved byte, must be 0 
DAP_Destination: dd 0   ; DOUBLE WORD - 32 bit Adress of Transfer Buffer
                        ;               as SEGMENT:OFFSET
DAP_LBA_Address: dd 0   ; QUAD WORD - Starting logical block adress
                 dd 0   ;             of target drive, 64 bit linear address
                        ;        If the device does not support LBA addressing
                        ;        BIOS will convert LBA address to a CHS value
                        ;        by using following formula:
                        ;                              
                        ;        LBA=(C1*H0+H1)*S0+S1-1
                        ;        C1= Selected Cylinder Number
                        ;        H0= Number Of Heads (Maximum Head Number + 1)
                        ;        H1= Selected Head Number
                        ;        S0= Maximum Sector Number
                        ;        S1= Selected Sector Number 
                        ;
;DAP_Flat_Destination: dd 0 ; QUAD WORD
                  dd 0  ; 64 bit address, if value at offset 4 is FFFF:FFFFh
                        ; (Also, value at offset 0 must be 18h) 
                        ; 64 bit Flat Address will not be used here

db 1 dup(0)
          
size_of_sf      equ ($- SMART_STARTUP_CODE)

proc_bin_to_decimal proc near
               ; 6-5-2009
               ;  Erdogan Tan
               ; INPUT: DS:SI = Target location
               ;        DX:AX = Binary Number
               ; OUTPUT: Decimal chars at DS:SI
               ; CX, AX, DX, SI, BX, BP will be changed.

                mov si, offset Sector_Str 
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
                call Rx_Dos_Div32
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
              ; mov byte ptr [SI], 0

print_decimal_sector_number:
               ; 23/05/2009
                mov ah, 1                      ; Set Cursor Type 
                mov ch, 20h                    ; Disable cursor  
                int 10h          
                mov ah, 2                      ; Set Cursor Position    
                mov dx, word ptr [Cursor_Pos]
                ;xor bh, bh
                int 10h  ; Set Cursor Position             
                pop si
               ;mov si, offset Sector_Str
                call SINGLIX_PRINTMSG
                mov ah, 1                      ; Set Cursor Type 
                mov cx, word ptr [Cursor_Type]  
                int 10h  

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

;#############################################################################
;#
;#              PROCEDURE proc_write_lba_sector
;#
;#############################################################################

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

                mov     DL, byte ptr [HDBS + bsDriveNumber] 
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
               ; mov     DL, byte ptr [HDBS + bsDriveNumber] 
               ; int     13h
                
               ; jmp     short loc_0FFh                  
 
loc_12Bh:
                retn                            ; db 0C3h
                        
proc_write_lba_sector   endp

proc_write_sector: dw 0

;#############################################################################
;#
;#              PROCEDURE proc_write_chs_sector
;#
;#############################################################################

proc_write_chs_sector   proc    near

loc_write_chs_sector:
                push    SI
                push    CX            
loc_09Bh:
                mov     DI,5                    
loc_0CAh:               
                push    AX                      ; Linear sector #
                push    DX                      ; DX_AX = Linear address (sectors)
                mov     CX,Word Ptr [Disk_SecPerTrack]
                push    BX

                call    RX_DOS_DIV32            ; Special 32 bit divide !!!
                                                ; To fix large disk problem.
                                                ; (c) Erdogan Tan 1999
                                                ; (October 20th, 1999)

                mov     CX, BX                  ; Sector (zero based)
                inc     CX                      ; To make it 1 based
                push    CX
                mov     CX,Word Ptr [Disk_Heads]
                call    RX_DOS_DIV32            ; Convert track to head & cyl
                mov     DH, BL                  ; BX = Head (max. FFh)
                pop     CX
                                                ; AX=Cyl, DH=Head, CX=Sector
                pop     BX                      ; ES:BX = Buffer

                mov     DL,Byte Ptr [HDBS + bsDriveNumber]
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
               ;mov     DL, byte ptr [HDBS + bsDriveNumber]
               ;int     13h                     ; BIOS Service func ( ah ) = 0
                                                ; Reset disk system
                jnz     short loc_0CAh                  
               ;jmp     short loc_0CAh          

loc_chs_12Bh:
                pop     CX
                pop     SI
                retn                            ; db 0C3h

Disk_SecPerTrack:
Disk_Heads:
                        
proc_write_chs_sector    endp

proc_rw_char   proc    near
               ; 30/07/2011
               ; 15/01/2006
               ; OUTPUT -> DS:SI = Entered String (ASCIIZ)
               mov     si, offset Str_Volume_Name
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
               mov     ah,3
               int     10h
               cmp     dl,byte ptr [Cursor_Pos]
               jna     short loc_beep
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
loc_beep:
               mov     ah, 0Eh
               mov     al, 7
               int     10h
               jmp     short read_next_char
loc_arrow:    
               cmp     AH,4Bh
               je      short loc_back
               cmp     AH,53h
               je      short loc_back
               jmp     short read_next_char
char_return:
              ; 30/07/2011
               mov     ah,3
               int     10h
check_char_type:
               cmp     al,20h
               jb      short loc_escape
               mov     ah,dl
               sub     ah,byte ptr [Cursor_Pos] 
               cmp     ah,63
               ja      short loc_beep
             ; cmp     al, "z"
             ; ja      short read_next_char
             ; cmp     al, "a"
             ; jb      short pass_capitalize
             ; and     al, 0DFh
pass_capitalize:
               mov     bl, ah  ; 30/07/2011
               xor     ah, ah
               mov     word ptr [SI][BX],ax
               mov     ah, 0Eh
               mov     bl, 7
               int     10h
               jmp     short read_next_char
pass_escape:
               cmp     al,0Dh
               jne     short read_next_char
               mov     ah,0Eh  ; 30/07/2011  
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

                ; 2/5/2009
                ; DX:AX = Disk Sector (Beginning Sector, with Descriptor)
                ; BL = Allocation Type (Identifier)

                mov byte ptr [DAT_Identifier], bl
                mov byte ptr [DAT_Buffer_Updated], 0 
            
                mov cx, 512
                call Rx_Dos_div32
                cmp word ptr [DAT_Buffer_Sector]+2, dx
                jne short loc_read_DAT_sector_x
                cmp word ptr [DAT_Buffer_Sector], ax
                jne short loc_read_DAT_sector_x               
 
loc_update_dat_buffer_x:
                mov si, offset HDFORMAT_DATBUFFER
                add si, bx
                mov al, byte ptr [DAT_Identifier]
                mov byte ptr [SI], al

return_from_dat_update:
                retn
 
loc_read_DAT_sector_x:
                push bx
                push dx   
                push ax
                mov ax, word ptr [mat_begin_sec]
                mov dx, word ptr [mat_begin_sec]+2
                add ax, word ptr [mat_Dat_lba]
                adc dx, word ptr [mat_Dat_lba]+2
                add ax, word ptr [DAT_Buffer_Sector]
                adc dx, word ptr [DAT_Buffer_Sector]+2
                mov cx, 1
                mov bx, offset HDFORMAT_DATBUFFER               
                call word ptr [proc_write_sector]
                pop ax
                pop dx
                pop bx
                jc short return_from_dat_update

                mov word ptr [DAT_Buffer_Sector], ax
                mov word ptr [DAT_Buffer_Sector]+2, dx

                inc byte ptr [DAT_Buffer_Updated]
                
               
              ; RESET (only for format procedure)

                mov di, offset HDFORMAT_DATBUFFER
                mov cx, 256
                xor ax, ax
                rep stosw
                
                jmp short loc_update_dat_buffer_x

;DAT_Buffer_Drv: db 0
DAT_Identifier: db 0
DAT_Buffer_Updated: db 0
DAT_Buffer_Sector: dd 0

proc_update_dat endp

hdformat_stage:  db 0
LBA_Ready: db 0

SINGLIX_WRITESTUB:

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
                mov byte ptr [SINGLIX_DRIVE], al
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
                mov byte ptr [SINGLIX_DRIVE]+1, al

                mov dl, byte ptr [SINGLIX_DRIVE]
                add dl, 80h-'0'

                mov byte ptr [HDBS + bsDriveNumber], dl

               ; 03/01/2010 
                mov ah, 41h ; Check INT 13h Extensions Present
                mov bx, 55AAh
                int 13h
                jc short WRITESTUB_09_lba_not_ready
                cmp bx, 0AA55h
                je short WRITESTUB_09_lba_ready

WRITESTUB_09_lba_not_ready:
                mov ah, 08h
                ;mov dl, byte ptr [HDBS + bsDriveNumber]
                int 13h
                jc  short WRITESTUB_09_load_masterboot
                inc dh
                mov byte ptr [HDBS + bs_Disk_Heads], dh
                mov byte ptr [HDBS + bs_Disk_SecPerTrack], cl
                xor cl,cl
WRITESTUB_09_lba_ready: 
 		; Fixed Disk Access Subset - LBA ready bit 
                and byte ptr [HDBS + bs_LBA_ready], cl

WRITESTUB_09_load_masterboot:
                call proc_load_masterboot
                jc WRITESTUB_30

                mov si, offset PartitionTable
                mov al, byte ptr [SINGLIX_DRIVE]+1
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
                mov byte ptr [SINGLIX_DRIVE]+1,al
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
                mov word ptr [HDBS + bsBootSector], ax
                mov word ptr [HDBS + bsBootSector]+2, dx
                mov word ptr [mat_begin_sec], ax
                mov word ptr [mat_begin_sec]+2, dx
                mov word ptr [rdt_begin_sec], ax
                mov word ptr [rdt_begin_sec]+2, dx
                
                mov ax, word ptr [SI][ptSectors] 
                mov dx, word ptr [SI][ptSectors]+2
                mov word ptr [HDBS + bsVolumeSize], ax
                mov word ptr [HDBS + bsVolumeSize]+2, dx
                mov word ptr [mat_volume_size], ax
                mov word ptr [mat_volume_size]+2, dx

                mov word ptr [mat_dat_free_s], ax
                mov word ptr [mat_dat_free_s]+2, dx

                add ax, 511
                adc dx, 0
                mov cx, 512
                call Rx_Dos_div32
                mov word ptr [mat_dat_secs], ax
                mov word ptr [mat_dat_secs]+2, dx
                add ax, word ptr [mat_dat_lba]
                adc dx, word ptr [mat_dat_lba]+2
               
                mov word ptr [HDBS + bsRootDirD], ax
                mov word ptr [HDBS + bsRootDirD]+2, dx 
                mov word ptr [RDT_Location], ax
                mov word ptr [RDT_Location]+2, dx

                add ax, word ptr [RDT_Scount]
                adc dx, word ptr [RDT_Scount]+2

                add ax, 1
                adc dx, 0

                mov word ptr [HDBS + bsStartupFD], ax
                mov word ptr [HDBS + bsStartupFD]+2, dx
                mov word ptr [fdt_location], ax
                mov word ptr [fdt_location]+2, dx

                push dx
                push ax
                mov ax, size_of_sf
                mov word ptr [fdt_file_size], ax 
                add ax, 511
                ;mov dx, 0 
                xor dx, dx
                ; mov cx, 512
                call Rx_Dos_div32
                mov word ptr [fdt_scount], ax
                mov word ptr [fdt_scount]+2, dx
                pop cx
                add ax, cx
                pop cx
                adc dx, cx 
                add ax, 1
                adc dx, 0 
                mov word ptr [mat_dat_ffs], ax
                mov word ptr [mat_dat_ffs]+2, dx
                sub word ptr [mat_dat_free_s], ax
                sbb word ptr [mat_dat_free_s]+2, dx
                 
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Write message
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

WRITESTUB_10:
                mov si, offset Msg_DoYouWantToFormat
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
               ;30/07/2011
                jne short RxDOS_WRITESTUB_14          

RxDOS_WRITESTUB_60_N:                                   ; no write (exit)  
               ; 30/07/2011
                mov si, offset Msg_No
                call RxDOS_PRINT
                jmp RxDOS_WRITESTUB_60

WRITESTUB_12:
                mov si, offset SINGLIX_Welcome
                call SINGLIX_PRINTMSG
                jmp WRITESTUB_60

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; get drive parameters
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

WRITESTUB_20_Y:
                mov si, offset msg_YES
                call proc_printmsg
WRITESTUB_20:
                mov si, offset Msg_Volume_Name
                call SINGLIX_PRINTMSG
                call proc_rw_char
                jc short pass_write_vol_name_chr1
                cmp byte ptr [SI], 20h
                jna short pass_write_vol_name_chr1
                mov di, offset Volume_Name
                mov cx, 64
loc_write_vol_name_chr0:
                lodsb
                cmp al, 20h
                jnb short loc_write_vol_name_chr1 
pass_write_vol_name_chr0:
                mov byte ptr [DI], 0
                inc di
                loop pass_write_vol_name_chr0
                jmp short pass_write_vol_name_chr1
loc_write_vol_name_chr1:
                mov byte ptr [DI], al
                inc di
                loop loc_write_vol_name_chr0
pass_write_vol_name_chr1:
                xor ax, ax
                int 1Ah                                 ; get time of day
                mov word ptr [Volume_Serial_No], dx
                mov word ptr [Volume_Serial_No]+2, cx   ; set unique volume ID

                mov ah, 02h                             ; Return Current Time
                int 1Ah
                xchg ch,cl
                mov word ptr [rdt_make_hour], cx
                xchg dh,dl
                mov word ptr [rdt_make_second], dx
                mov word ptr [rdt_lm_hour], cx
                mov word ptr [rdt_lm_second], dx

		;26-5-2009
                add cx, dx  
                add word ptr [Volume_Serial_No]+2, cx
               
                mov ah, 04h                             ; Return Current Date
                int 1Ah
                xchg ch,cl
                mov word ptr [rdt_make_year], cx
                xchg dh,dl
                mov word ptr [rdt_make_month], dx
                mov word ptr [rdt_lm_year], cx
                mov word ptr [rdt_lm_month], dx

 		;26-5-2009
                add cx, dx  
                add word ptr [Volume_Serial_No]+2, cx

WRITESTUB_20_Reset:
                xor ah,ah
                mov dl, byte ptr [HDBS + bsDriveNumber]
                int 13h
                jc  WRITESTUB_30

WRITESTUB_22:
                mov  si, offset Msg_Writing_Boot_Sector
                call SINGLIX_PRINTMSG

                mov     BX, 55AAh               ; db 0BBh, 0AAh, 55h
                mov     AH, 41h                 ; db 0B4h, 41h
                int     13h                     ; db 0CDh, 13h  
                jc      short loc_LBA_not_ready  
                
                cmp     BX, 0AA55h              ; Extensions present
                jne     short loc_LBA_not_ready
                test    CL, 1                   ; Fixed disk access subset check
                jz      short loc_LBA_not_ready      
                
                mov ax, offset proc_write_lba_sector
                inc byte ptr [LBA_ready]
                jmp short loc_write_boot_sector  
loc_LBA_not_ready:
                mov ah, 08h
                mov dl, byte ptr [HDBS + bsDriveNumber]
                int 13h
                jc WRITESTUB_70
                inc dh
                mov byte ptr [Disk_Heads], dh
                mov byte ptr [Disk_SecPerTrack], cl
                mov ax, offset proc_write_chs_sector
loc_write_boot_sector:
                mov word ptr [proc_write_sector], ax

                mov ax, word ptr [HDBS + bsBootSector]
                mov dx, word ptr [HDBS + bsBootSector]+2

                mov bx, offset HDFORMAT_BOOTSECBUFFER
                call word ptr [proc_write_sector]
                jc WRITESTUB_70

WRITESTUB_25:
                mov  si, offset Msg_OK
                call SINGLIX_PRINTMSG
             
WRITESTUB_27:
                inc byte ptr [hdformat_stage]

WRITESTUB_27_rp:
                mov si, offset Msg_Writing_MAT
                call SINGLIX_PRINTMSG

                mov ax, 1 ; MAT Beginning Address
                xor dx, dx
                add ax, word ptr [HDBS + bsBootSector]
                adc dx, word ptr [HDBS + bsBootSector]+2
                mov bx, offset HDFORMAT_MATBUFFER
loc_write_mat_sector:
                call word ptr [proc_write_sector]
                jc WRITESTUB_70

                mov  si, offset Msg_OK
                call SINGLIX_PRINTMSG

WRITESTUB_28:
                inc byte ptr [hdformat_stage]
WRITESTUB_28_rp:
                mov si, offset Msg_Writing_Root_Dir
                call SINGLIX_PRINTMSG

                mov ax, word ptr [HDBS + bsRootDirD]   ; Root Directory Descriptor Address
                mov dx, word ptr [HDBS + bsRootDirD]+2
                add ax, word ptr [HDBS + bsBootSector]
                adc dx, word ptr [HDBS + bsBootSector]+2
                mov bx, offset HDFORMAT_ROOTBUFFER
loc_write_root_dir_sectors:
                call word ptr [proc_write_sector]
                jc WRITESTUB_70
                
                mov cx, 4
loc_write_root_directory_data_sectors:
                add ax, 1
                adc dx, 0
                mov bx, offset root_dir_data_sector
                call word ptr [proc_write_sector]
                jc WRITESTUB_70
                loop loc_write_root_directory_data_sectors

                mov  si, offset Msg_OK
                call SINGLIX_PRINTMSG

WRITESTUB_37:
                inc byte ptr [hdformat_stage]
WRITESTUB_37_rp:
                
loc_write_sf_dt_data:
                mov ah, 02h                             ; Return Current Time
                int 1Ah
                xchg ch,cl
                mov word ptr [fdt_make_hour], cx
                xchg dh,dl
                mov word ptr [fdt_make_second], dx
                mov word ptr [fdt_lm_hour], cx
                mov word ptr [fdt_lm_second], dx
                
                mov ah, 04h                             ; Return Current Date
                int 1Ah
                xchg ch,cl
                mov word ptr [fdt_make_year], cx
                xchg dh,dl
                mov word ptr [fdt_make_month], dx
                mov word ptr [fdt_lm_year], cx
                mov word ptr [fdt_lm_month], dx
               ;mov cx, size_of_sf
               ;mov word ptr [fdt_file_size], cx
     
WRITESTUB_38_rp: 
                mov si, offset Msg_writing_sf
                call SINGLIX_PRINTMSG
                mov ax, word ptr [fdt_location]
                mov dx, word ptr [fdt_location]+2
                add ax, word ptr [HDBS + bsBootSector]
                adc dx, word ptr [HDBS + bsBootSector]+2
                mov bx, offset HDFORMAT_FDTBUFFER     
                call word ptr [proc_write_sector]
                jc WRITESTUB_30
                
                mov cx, word ptr [fdt_scount]
                mov bx, offset fdt_file_data
                mov si, offset SMART_STARTUP_CODE
WRITESTUB_38:        
                push cx
                mov cx, 512
                mov di, bx
                rep movsb
                pop cx 
                add ax, 1
                adc dx, 0
                call word ptr [proc_write_sector]
                jc WRITESTUB_30
                loop WRITESTUB_39 
                
                mov si, offset Msg_OK
                call SINGLIX_PRINTMSG
                jmp short WRITESTUB_55
WRITESTUB_39:                
                push cx
                push ax
                mov ax, 0
                mov cx, 256
                mov di, bx
                rep stosw
                pop ax
                pop cx
                jmp short WRITESTUB_38
WRITESTUB_55:
                inc byte ptr [hdformat_stage]
WRITESTUB_55_rp:
                mov si, offset Msg_Writing_DATA_Sectors
                call SINGLIX_PRINTMSG

                mov ah, 3
               ;xor bh, bh
                int 10h ; Return Cursor Position
                ; DL = Column, DH= Line
                ; CH/CL = Cursor beginning/ending line 
                mov word ptr [Cursor_Pos], DX
                mov word ptr [Cursor_Type], CX
                mov si, offset Sector_Str
                call SINGLIX_PRINTMSG

                xor ax, ax
                xor dx, dx
                mov word ptr [DAT_Buffer_Sector], ax
                mov word ptr [DAT_Buffer_Sector]+2, dx

                mov di, offset HDFORMAT_DATBUFFER
                mov word ptr [DI], 0201h
                inc di
                inc di
                mov cx, 255
                rep stosw

                mov bx, word ptr [mat_dat_secs]+2
                mov cx, word ptr [mat_dat_secs]

                mov ax, word ptr [mat_dat_lba]
                mov dx, word ptr [mat_dat_lba]+2

loc_update_dat_03h_loop:
                push bx
                push cx
                push dx
                push ax  
                mov bl, 03h
                call proc_update_dat
                pop ax
                pop dx
                jnc short WRITESTUB_56
                pop ax
                pop dx
                jmp WRITESTUB_70

WRITESTUB_56:
                push dx
                push ax
                call proc_bin_to_decimal
                pop ax
                pop dx
                add ax, 1
                adc dx, 0   
                pop cx
                pop bx

                sub cx, 1
                sbb bx, 0
                cmp cx, 0
                ja short loc_update_dat_03h_loop
                cmp bx, 0
                ja short loc_update_dat_03h_loop 

                mov ax, word ptr [HDBS + bsRootDirD]   ; Root Directory Descriptor Address
                mov dx, word ptr [HDBS + bsRootDirD]+2
                push dx
                push ax
                mov bl, 04h
                call proc_update_dat
                pop ax
                pop dx
                jc WRITESTUB_70
                push dx
                push ax
                call proc_bin_to_decimal
                pop ax
                pop dx 
                add ax, 1
                adc dx, 0
                mov cx, 4
loc_update_05h_loop:
                push dx
                push ax
                mov bl, 05h
                push cx
                call proc_update_dat
                pop cx
                pop ax
                pop dx
                jc WRITESTUB_70
                push cx
                push dx
                push ax
                call proc_bin_to_decimal
                pop ax
                pop dx 
                pop cx
                add ax, 1
                adc dx, 0
                loop loc_update_05h_loop 

                mov ax, word ptr [fdt_location]   ; Startup file FDT Address
                mov dx, word ptr [fdt_location]+2
                push dx
                push ax
                mov bl, 06h
                call proc_update_dat
                pop ax
                pop dx
                jc WRITESTUB_70
                push dx
                push ax
                call proc_bin_to_decimal
                pop ax
                pop dx 
                add ax, 1
                adc dx, 0
                mov cx, word ptr [fdt_scount] ; Startup file data section sectors
loc_update_07h_loop:
                push dx
                push ax
                mov bl, 07h
                push cx
                call proc_update_dat
                pop cx
                pop ax
                pop dx
                jc WRITESTUB_70
                push cx
                push dx
                push ax
                call proc_bin_to_decimal
                pop ax
                pop dx 
                pop cx
                add ax, 1
                adc dx, 0
                loop loc_update_07h_loop 
                
                mov bx, word ptr [mat_volume_size]+2
                mov cx, word ptr [mat_volume_size]
                sub cx, ax
                sbb bx, dx
loc_update_dat_90h_loop:
                push bx
                push cx
                push dx
                push ax  
                mov bl, 90h
                call proc_update_dat
                pop ax
                pop dx
                jnc short WRITESTUB_57
                pop ax
                pop dx
                jmp short WRITESTUB_70
WRITESTUB_57:
                push dx
                push ax
                call proc_bin_to_decimal
                pop ax
                pop dx
                add ax, 1
                adc dx, 0   
                pop cx
                pop bx
                sub cx, 1
                sbb bx, 0
                cmp cx, 0
                ja short loc_update_dat_90h_loop
                cmp bx, 0
                ja short loc_update_dat_90h_loop                                                
              
                cmp byte ptr [DAT_buffer_updated], 0
                jna short WRITESTUB_58
 
                mov ax, word ptr [mat_begin_sec]
                mov dx, word ptr [mat_begin_sec]+2
                add ax, word ptr [mat_Dat_lba]
                adc dx, word ptr [mat_Dat_lba]+2
                add ax, word ptr [DAT_Buffer_Sector]
                adc dx, word ptr [DAT_Buffer_Sector]+2
                mov bx, offset HDFORMAT_DATBUFFER               
                call word ptr [proc_write_sector]
                jc short WRITESTUB_70
               ;inc byte ptr [DAT_buffer_updated]

WRITESTUB_58:
                mov si, offset Msg_OK
                call SINGLIX_PRINTMSG

WRITESTUB_60:
                mov si, offset SINGLIX_CRLF
                call SINGLIX_PRINTMSG

                int 20h
                
                ; for standalone execution: INT 20h will return here
                xor ah, ah
                int 16h

                int 19h  

WRITESTUB_70:  
                mov si, offset msg_singlix_drv_write_error
                call SINGLIX_PRINTMSG
                jmp short WRITESTUB_40

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
                mov dl, byte ptr [HDBS + bsDriveNumber]
                mov ah, 08h
                int 13h                              ; return disk parameters
                push cs
                pop  es                              ; restore es
                jc WRITESTUB_30

                cmp bl, 04                           ; Drive Type
                jb WRITESTUB_30

                xor ah,ah
                mov dl, byte ptr [HDBS + bsDriveNumber]
                int 13h 
                jc WRITESTUB_30  
             
                cmp byte ptr [hdformat_stage], 1
                je WRITESTUB_27_rp ; DATA Sectors
                jb WRITESTUB_22 ; BOOT Sector
                cmp byte ptr [hdformat_stage], 2
                je WRITESTUB_28_rp ; STARTUP_FILE
                cmp byte ptr [hdformat_stage], 3
                je WRITESTUB_37_rp ; STARTUP_FILE
                jmp WRITESTUB_55_rp ; DAT Sectors


;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;  messages
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

SINGLIX_Welcome:
                db 0Dh, 0Ah
                db 'TR-SINGLIX FS1 Hard Disk Partition (A1h) Format Utility'
                db 0Dh, 0Ah
                db '(c) Erdogan TAN 2006-2010'
                db 0Dh,0Ah
                db 0Dh,0Ah
                db 'Usage: hdformat [Drive] '
                db 0Dh,0Ah
                db 0Dh,0Ah
                db "Drive names:"
                db 0Dh,0Ah
                db 0Dh,0Ah
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
                db 0Dh, 0Ah
                db 0

Msg_DoYouWantToFormat:
                db 07h
                db 0Dh, 0Ah
                db 'WARNING!'
                db 0Dh, 0Ah
                db 'All data on the drive will be erased.'
                db 0Dh, 0Ah
                db 0Dh, 0Ah
                db 'Do you want to format drive hd'
SINGLIX_DRIVE:
                db '00: (Yes/No)? ', 0

msg_singlix_drv_read_error:
                db 0Dh, 0Ah
                db 'Drive not ready or read error! Try again? (Y/N) '
                db 0

Msg_Writing_Boot_Sector:
                db 0Dh, 0Ah
                db "Writing SINGLIX FS1 boot sector...", 0
Msg_Writing_Root_Dir:
                db 0Dh, 0Ah
                db "Writing root directory sectors...", 0
Msg_Writing_DATA_Sectors:
                db 0Dh, 0Ah, 0Dh, 0Ah
                db "Writing disk allocation table and data sectors..."
                db 0Dh, 0Ah
                db "Writing data sector: ", 0

Sector_Str:     db 10 dup(0)

Msg_Writing_MAT:
                db 0Dh, 0Ah
                db "Writing master allocation table...", 0

Msg_writing_sf:
                db 0Dh, 0Ah
                db "Writing SINGLIX startup file...",0

Msg_Volume_Name:
                db 0Dh, 0Ah
                db "Volume Name: ", 0

Msg_3dot_OK:    db "..."
Msg_OK:
                db ' OK.', 0

msg_YES:        db ' YES'
                db 0
msg_NO:         db ' NO'
                db 0   

SINGLIX_CRLF:
                db 0Dh, 0Ah, 0

msg_singlix_drv_write_error:
                db 0Dh, 0Ah
                db 'Drive not ready or write error! Try again? (Y/N) '
                db 0

Error_Code:     db 0

fsPartitionNumber: db 0
                       
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;  buffers
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

MasterBootBuff:
MasterBootCode: db 1BEh dup (?)
PartitionTable: db 64 dup (?)
MBIDCode: dw ?

HDFORMAT_SECBUFFER:
                db 512 dup(0F6h)
HDFORMAT_MATBUFFER:
                db 'MAT'                 
                db 0
mat_volume_size:dd 0    ; Volume Size 
mat_begin_sec:  dd 0    ; Volume Beginning Sector
mat_dat_lba:    dd 2    ; DAT LBA
mat_dat_secs:   dd 0    ; DAT Sectors
mat_dat_free_s: dd 0    ; Free Sectors on DAT
mat_dat_ffs:    dd 0    ; First Free sector
                db 484 dup(0)     
HDFORMAT_DATBUFFER:
                db 512 dup(0)
HDFORMAT_ROOTBUFFER:
                db 'RDT'; Root Directory Description Table
                db 0 ; RDT Version
                dw 512 ; Bytes per Sector
                dw 0 ; RDT Number
RDT_Location:   dd 0 ; The First RDT Address
                dd 0 ; Next RDT number                 
RDT_SCount:     dd 4 ; Sector Count                 
rdt_begin_sec:  dd 0 ; Volume Beginning Sector
                dd 0 ; Reserved
                     ; Offset 28                
Volume_Serial_No:
                dd 0
                db 4 ; Entry Size (Bytes)                
                db 0 ; Sub Directory Level
                     ; Offset 34
                db 12 dup(0) 
                     ; Offset 46
                db 0 ; Country
                db 0 ; Time Zone (+11)
                     ; Offset 48
rdt_make_year:  dw 0 ; Creating Year
rdt_make_month: db 0 ; Creating Month
rdt_make_day:   db 0 ; Creating Day
rdt_make_hour:  db 0 ; Creating Hour
rdt_make_minute:db 0 ; Creating Minute
rdt_make_second:db 0 ; Creating Second
rdt_make_dlstm: db 0 ; Daylight Saving Time Mode (0= standard time)
                     ; Offset 56
rdt_lm_year:    dw 0 ; Last Mofication Year
rdt_lm_month:   db 0 ; Last Modification Month
rdt_lm_day:     db 0 ; Last Modification Day
rdt_lm_hour:    db 0 ; Last Modification Hour
rdt_lm_minute:  db 0 ; Last Modification Minute
rdt_lm_second:  db 0 ; Last Modification Second
rdt_lm_dlstrm:  db 0 ; Daylight Saving Time Mode (0= standard time)
                     ; Offset 64
Volume_Name:
                db 64 dup(0)
                     ; Offset 128
                db 128 dup (0)
                ; Offset 256
                db 256 dup (0)
root_dir_data_sector:
                db 512 dup (0)   
    
; End Of Root Dir Descriptor Buffer

HDFORMAT_FDTBUFFER:
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
                db 0  ; Attributes
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

                ; Offset 64 [ Volume Name 64 bytes zero ended ]
fdt_file_name:
                db 'Turkish Rational SINGLIX STARTUP File v1.0'
                db 0
                db 21 dup (0)
                ; Offset 128
                db 128 dup (0)
                ; Offset 256
                db 256 dup (0)
fdt_file_data:
                db 512 dup (0)   
    
; End Of FDT Buffer

RetryCount:     dw 0


                db  '(c) Erdogan TAN 2006-2010'

                db  1 dup (0)                           
                                                                                                              
                
                ;org 7C00h                  

; TR-SINGLIX FS1 BootSector Identification (Data) Block
; 19-05-2009 FS1 HD Partition Boot Sector
; 17-05-2009 [ TRFS Issue 1, Revision 13 ]
; 10-05-2009 [ TRFS Issue 1, Revision 12 ]
; 03-03-2007 [ TRFS Issue 1, Revision 11 ]

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

HDBS:
HDFORMAT_BOOTSECBUFFER:

include FS1_HDBS.TXT  ; FS1 Boot Sector [21/02/2010]

SINGLIXBOOT     ends

                end     SMART_STARTUP_CODE
