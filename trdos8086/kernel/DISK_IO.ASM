; *****************************************************************************
;
; DISK_IO.ASM  [ TRDOS Kernel Disk IO (Read, Write) Procedures ]
; Copyright (C) 2009-2010 Erdogan TAN [20/07/2011]
;
; 19/06/2011, 03/07/2011, 05/07/2011, 06/07/2011
; 01/05/2010, 16/05/2010, 17/05/2010, 17/10/2010, 05/12/2010 
; 28/02/2010, 18/04/2010, 21/04/2010, 24/04/2010
; 24/09/2009, 04/07/2009
;
; TRDOS.ASM (include disk_io.asm)
;
; *****************************************************************************

proc_CHS_read proc near
                ; 20/07/2011
                ; 06/07/2011
                ; 05/07/2011
                ; 19/06/2011 
                ; 05/12/2010
                ; 17/05/2010 DX:AX at return -> Next LBA Adress
                ; 18/04/2010 ES:BX -> Segment limit check
                ; 04/07/2009
                ;
                ; INPUT -> DX:AX = Logical Block Address
                ; CX = Number of sectors to read
                ; DS:SI = Logical Dos Disk Table Offset (DRV)
                ; ES:BX = Destination Buffer
                ; OUTPUT -> clc or stc
                ;

loc_read_disk_chs_reset_rc: ; 05/12/2010
                mov byte ptr [CHS_RetryCount], 4
              
loc_read_disk_chs:
                push    DX                      ; DX_AX = Linear address (sectors)
                push    AX                      ; Linear sector #
                push    CX                      ; # of FAT/FILE/DIR sectors
                
		mov     CX,Word Ptr [SI][LD_BPB][SecPerTrack]
                push    BX

                call    RX_DOS_DIV32            ; Special 32 bit divide !!!
                                                ; To fix large disk problem.
                                                ; (c) Erdogan Tan 1999
                                                ; (October 20th, 1999)

                mov     CX, BX                  ; Sector (zero based)
                inc     CX                      ; To make it 1 based
                push    CX
                mov     CX,Word Ptr [SI][LD_BPB][Heads]
                call    RX_DOS_DIV32            ; Convert track to head & cyl
                mov     DH, BL                  ; BX = Head (max. FFh)
                pop     CX
                                                ; AX=Cyl, DH=Head, CX=Sector
                pop     BX                      ; ES:BX = Buffer

                mov     DL,Byte Ptr [SI][LD_PhyDrvNo]

                mov     CH,AL                   
                ror     AH,1                    ; Rotate right
                ror     AH,1                   
                or      CL,AH                   
                mov     AX,0201h
                int     13h                     ; BIOS Service func ( ah ) = 2
                                                ; Read disk sectors
                                                ; AL-sec num CH-track CL-sec
                                                ; DH-head DL-drive ES:BX-buffer
                                                ; CF-flag AH-stat AL-sec read
                                                ; If CF = 1 then (If AH > 0)

               ; 19/06/2011
                mov     byte ptr [Disk_IO_err_Code], AH
                pop     CX
                pop     AX
                pop     DX
                jnc     short loc_read_disk_chs_no_error

               ; 05/07/2011 
                cmp     byte ptr [Disk_IO_err_code], 09h
                                             ; DMA crossed 64K segment boundary
                je      short loc_read_disk_chs_error_retn ; 06/07/2011
               ;
             
               ; 20/07/2011
                dec     byte ptr [CHS_RetryCount]
                jnz     short loc_read_disk_chs 

loc_read_disk_chs_error_retn:
               ; 20/07/2011
               ; 06/07/2011
                stc
                retn

loc_read_next_sector:
                add     BX, 512
               ; 18/04/2010            
                jnc     short loc_read_disk_chs_reset_rc ; 03/07/2011
                push    BX
                mov     BX, ES
                add     BX, 1000h
                mov     ES, BX
                pop     BX
                ; 
                jmp     short loc_read_disk_chs_reset_rc ; 05/12/2010

loc_read_disk_chs_no_error:
               ; 17/05/2010 DX:AX retn -> next LBA address
                add     AX,1
                adc     DX,0
               ;      
                loop    loc_read_next_sector
                retn

proc_CHS_read   endp

proc_LBA_read   proc near
                ; 20/07/2011
                ; 19/06/2011
                ; 01/05/2010
                ; 04/07/2009
                ; 2005
                ; INPUT -> DAP_Buffer
                ; OUTPUT -> clc or stc
                ;           cx = number of blocks read
loc_read_disk_lba:
                push si
                mov si, offset DAP_Buffer
loc_read_disk_lba_again:
                ; DS:SI= DAP Location
                mov ah, 42h  ; Extended Disk Read - LBA Read
               ;mov dl, byte ptr [DAP_BuffDisk]
                int 13h
		mov byte ptr [Disk_IO_err_Code], ah ; 19/06/2011
                jnc short pass_read_disk_lba_error
                dec byte ptr [DAP_RetryCount]
               ; 20/07/2011 
                jnz short loc_read_disk_lba_again
                ;xor ch, ch
                mov cl, byte ptr [DAP_BuffNumOfBlocks]
                stc
pass_read_disk_lba_error:
                pop si
                retn
proc_LBA_read   endp

proc_disk_read proc near
                ; 17/10/2010 LBA bugfix 
                ; 16/05/2010 read error -> 1Eh retn
                ; 01/05/2010
                ; 24/04/2010 sector count bugfix
                ; 21/04/2010 ES:BX bugfix
                ; 18/04/2010 
                ;  -> read all lba sectors even if cx > 127
		; 04/07/2009
                ; INPUT -> DS:SI = DOS Drive Description Table 
                ; INPUT -> DX:AX = LBA address
                ; INPUT -> CX = Sector Count (CH=0)
                ; INPUT/OUTPUT -> ES:BX = Buffer
 		; OUTPUT -> DX:AX = Next LBA address
                ; OUTPUT -> stc -> Error code in AL (AX)
                ; OUTPUT -> CX = Sector count (loaded to buffer)
                ; 

               ; 24/04/2010, 18/04/2010
                mov word ptr [Disk_RW_SectorCount], cx

               ;21/04/2010
                push es
                push bx
               ;01/05/2010
                push cx  
               ;
                cmp byte ptr [SI][LD_LBAYes], 0
                ja short loc_read_lba_sectors
loc_read_chs_sectors:
                call proc_chs_read
                jc short retn_read_sectors_stc
                ;01/05/2010
                or cx, cx
                jz short retn_read_sectors
retn_read_sectors_stc:
               ; 01/05/2010
                pop ax ; pushed cx
                sub ax, cx ; cx = remain sectors to read
                mov cx, ax
                stc
               ; 16/05/2010
               ; mov ax, 1Eh ; Drive not ready or read error !
               ;01/05/2010
               ;jmp short retn_read_sectors_pop_bx_pop_es
                push cx
retn_read_sectors:
              ; 01/05/2010
                pop cx
retn_read_sectors_pop_bx_pop_es:
 	      ; 21/04/2010
                pop bx
                pop es
               ;
                retn

loc_read_lba_sectors:
               ; 18/04/2010
                cmp cx, 127
                jna short pass_read_lba_sectors_count_fix
                mov cx, 127             
pass_read_lba_sectors_count_fix:
              ; mov byte ptr [DAP_BuffLBAyes], 1
                mov word ptr [DAP_BuffLBA_Address], ax
                mov word ptr [DAP_BuffLBA_Address]+2, dx
                mov byte ptr [DAP_BuffPacketSize], 10h
                mov word ptr [DAP_BuffDestination], bx
                push es
                pop word ptr [DAP_BuffDestination]+2
                mov byte ptr [DAP_BuffNumOfBlocks], cl
                mov byte ptr [DAP_RetryCount], 4
               ; 17/10/2010 BugFix
                push ax
                push dx
                mov dl, byte ptr [SI][LD_PhyDrvNo]
                mov byte ptr [DAP_BuffDisk], dl
                call proc_lba_read
               ; 17/10/2010
                pop dx
                pop ax
                jc short retn_lba_read_sectors_stc
               ; 18/04/2010
                add ax, cx  ; cx <= 127
                adc dx, 0
                push dx
                push ax
                mov ax, 512
                mul cx
                add bx, ax
                jnc short pass_read_disk_lba_next_segment
                mov dx, es
                add dx, 1000h
                mov es, dx
                
pass_read_disk_lba_next_segment:
                pop ax
                pop dx
                cmp cx, word ptr [Disk_RW_SectorCount]
                jnb short retn_read_sectors
                sub word ptr [Disk_RW_SectorCount], cx
                mov cx, word ptr [Disk_RW_SectorCount]
                jmp short loc_read_lba_sectors

retn_lba_read_sectors_stc:
               ;01/05/2010
                sub word ptr [Disk_RW_SectorCount], cx
                mov cx, word ptr [Disk_RW_SectorCount]
                jmp short retn_read_sectors_stc

proc_disk_read endp

proc_disk_write proc near
                ; 17/10/2010 LBA bugfix 
                ; 01/05/2010
 		; 24/04/2010 sector count bugfix
                ; 21/04/2010 ES:BX bugfix
                ; 18/04/2010
                ; 09/02/2005 
                ; INPUT -> DS:SI = DOS Drive Description Table 
                ; INPUT -> DX:AX = LBA address
                ; INPUT -> CX = Sector Count (CH=0)
                ; INPUT/OUTPUT -> ES:BX = Buffer
                ; OUTPUT -> DX:AX = Next LBA address
                ; OUTPUT -> stc -> Error code in AL (AX)
                ; OUTPUT -> CX = Sector count (written to disk)
                ;

               ;24/04/2010, 18/04/2010
                mov word ptr [Disk_RW_SectorCount], cx
               
               ;21/04/2010
                push es
                push bx
               ;01/05/2010
                push cx  
               ;
                cmp byte ptr [SI][LD_LBAYes], 0
                ja short loc_write_lba_sectors

                call proc_CHS_write
                jc short retn_write_sectors_stc
                ;01/05/2010
                or cx, cx
                jz short retn_write_sectors
retn_write_sectors_stc:
               ; 01/05/2010
                pop ax ; pushed cx
                sub ax, cx ; cx = remain sectors to write
                mov cx, ax
                stc
               ; 18/04/2010
               ; mov ax, 1Dh ; Drive not ready or write error !
               ;01/05/2010
               ;jmp short retn_write_sectors_pop_bx_pop_es
                push cx
retn_write_sectors:
              ; 01/05/2010
                pop cx
retn_write_sectors_pop_bx_pop_es:
 	      ; 21/04/2010
                pop bx
                pop es
               ; 
                retn

loc_write_lba_sectors:
               ; 18/04/2010
                cmp cx, 127
                jna short pass_write_lba_sectors_count_fix
                mov cx, 127             
pass_write_lba_sectors_count_fix:
               ;mov byte ptr [DAP_BuffLBAyes], 1
                mov word ptr [DAP_BuffLBA_Address], ax
                mov word ptr [DAP_BuffLBA_Address]+2, dx
                mov byte ptr [DAP_BuffPacketSize], 10h
                mov word ptr [DAP_BuffDestination], bx
                push es
                pop word ptr [DAP_BuffDestination]+2
                mov byte ptr [DAP_BuffNumOfBlocks], cl
                mov byte ptr [DAP_RetryCount], 4
               ; 17/10/2010 BugFix
                push ax
                push dx
                mov dl, byte ptr [SI][LD_PhyDrvNo]
                mov byte ptr [DAP_BuffDisk], dl
                call proc_LBA_write
               ; 17/10/2010
                pop dx 
                pop ax
                jc short retn_lba_write_sectors_stc
		; 18/04/2010
                add ax, cx  ; cx <= 127
                adc dx, 0
                push dx
                push ax
                mov ax, 512
                mul cx
                add bx, ax
                jnc short pass_write_disk_lba_next_segment
                mov dx, es
                add dx, 1000h
                mov es, dx
                
pass_write_disk_lba_next_segment:
                pop ax
                pop dx
                cmp cx, word ptr [Disk_RW_SectorCount]
                jnb short retn_write_sectors   
                sub word ptr [Disk_RW_SectorCount], cx
                mov cx, word ptr [Disk_RW_SectorCount]
                jmp short loc_write_lba_sectors
retn_lba_write_sectors_stc:
               ;01/05/2010
                sub word ptr [Disk_RW_SectorCount], cx
                mov cx, word ptr [Disk_RW_SectorCount]
                jmp short retn_write_sectors_stc
                 
proc_disk_write endp

proc_CHS_write  proc near
                ; 20/07/2011
                ; 09/07/2011
                ; 20/06/2011
                ; 19/06/2011
                ; 05/12/2010 
                ; 17/05/2010 DX:AX at return -> Next LBA Adress
                ; 01/05/2010
                ; 18/04/2010
                ; 09/02/2005
                ; INPUT -> DX:AX = Logical Block Address
                ; CX = Number of sectors to read
                ; DS:SI = Logical Dos Disk Table Offset (DRV)
                ; ES:BX = Source Buffer
                ; OUTPUT -> clc or stc
                ;

loc_write_disk_chs_reset_rc: ; 05/12/2010
                mov byte ptr [CHS_RetryCount], 4
loc_write_disk_chs:
                push    DX                      ; DX_AX = Linear address (sectors)
                push    AX                      ; Linear sector #
                push    CX                      ; # of FAT/FILE/DIR sectors

                mov     CX,Word Ptr [SI][LD_BPB][SecPerTrack]
                push    BX

                call    RX_DOS_DIV32            ; Special 32 bit divide !!!
                                                ; To fix large disk problem.
                                                ; (c) Erdogan Tan 1999
                                                ; (October 20th, 1999)

                mov     CX, BX                  ; Sector (zero based)
                inc     CX                      ; To make it 1 based
                push    CX
                mov     CX,Word Ptr [SI][LD_BPB][Heads]
                call    RX_DOS_DIV32            ; Convert track to head & cyl
                mov     DH, BL                  ; BX = Head (max. FFh)
                pop     CX
                                                ; AX=Cyl, DH=Head, CX=Sector
                pop     BX                      ; ES:BX = Buffer

                mov     DL,Byte Ptr [SI][LD_PhyDrvNo]
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
               ; 19/06/2011
                mov 	byte ptr [Disk_IO_err_Code], ah
                pop     CX
                pop     AX
                pop     DX
                jnc     short loc_write_disk_chs_no_error
                
               ; 20/07/2011
                dec     byte ptr [CHS_RetryCount]
                jnz     short short loc_write_disk_chs  

loc_write_disk_chs_error_retn:
               ; 20/07/2011
                stc
                retn

loc_write_next_sector:
                add     BX, 512
               ; 18/04/2010            
                jnc     short loc_write_disk_chs_reset_rc ; 03/07/2011
                push    BX
                mov     BX, ES
                add     BX, 1000h
                mov     ES, BX
                pop     BX
                ; 
                jmp     short loc_write_disk_chs_reset_rc ; 05/12/2010

loc_write_disk_chs_no_error:
  	       ; 17/05/2010 DX:AX retn -> next LBA address
                add     AX,1
                adc     DX,0
                loop    loc_write_next_sector
                retn

proc_CHS_write  endp

proc_LBA_write  proc near
                ; 20/07/2011
                ; 19/06/2011
                ; 01/05/2010
                ; 09/02/2005
                ; INPUT -> DAP_Buffer
                ; OUTPUT -> clc or stc
                ;           cx = number of blocks written
loc_write_disk_lba:
                push si
                mov si, offset DAP_Buffer
loc_write_disk_lba_again:
                ; DS:SI= DAP Location
                mov ax, 4300h  ; Extended Disk Write - LBA write (verify off) 
                ;mov dl, byte ptr [DAP_BuffDisk]
                int 13h
                mov byte ptr [Disk_IO_err_Code], ah ; 19/06/2011
                jnc short pass_write_disk_lba_error
                dec byte ptr [DAP_RetryCount]
               ;20/07/2011 
                jnz short loc_write_disk_lba_again
                ;xor ch, ch
                mov cl, byte ptr [DAP_BuffNumOfBlocks]
                stc
pass_write_disk_lba_error:
                pop si
                retn
proc_LBA_write  endp

; 24/04/2010, 18/04/2010
Disk_RW_SectorCount: dw 0
;

CHS_RetryCount: db 0
; 19/06/2011
Disk_IO_err_Code: db 0

; Disk Read Procedure Parameters
DAP_RetryCount: db 4
DAP_BuffCDRV_BPB: dw 0
DAP_BuffDisk: db 0
DAP_BuffLBAyes: db 0
DAP_Buffer:
DAP_BuffPacketSize: db 0
DAP_BuffReservd1: db 0
DAP_BuffNumOfBlocks: db 0
DAP_BuffReservd2: db 0
DAP_BuffDestination: dd 0
DAP_BuffLBA_Address: dd 0
DAP_BuffLBA_Addressq: dd 0
