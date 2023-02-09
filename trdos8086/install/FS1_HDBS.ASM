; ****************************************************************************
; Turkish Rational SINGLIX  
; Disk Operation System v1.0 Project
; Hard Disk FS1 (A1h) Partition's BOOT SECTOR Code
;
; Copyright (C) 2006-2009  Erdogan TAN  [ 21/02/2010 ]
; FS1_HDBS.ASM [ 19/05/2009 ] -> FS1_HDBS.TXT (db EBh, 3Fh ...)
; HDFORMAT.ASM [ from5to19/may/2009 ] (include FS1_HDBS.TXT)
; MULTIXFD.ASM [ 15-1-2006 | 4-3-2007 | 29-3-2008 | 25/10/2008 | 24/03/2009 ]
;
; ****************************************************************************


SINGLIXBOOT     SEGMENT PUBLIC 'CODE'
                assume cs:SINGLIXBOOT,ds:SINGLIXBOOT,es:SINGLIXBOOT,ss:SINGLIXBOOT

                org 7C00h

;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
;±
;±              PROCEDURE proc_start
;±
;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±

proc_start      proc    near

Start:
                jmp     short loc_offset_65
                nop

; TR-SINGLIX FS1 BootSector Identification (Data) Block
; 19-05-2009 FS1 HD Partition Boot Sector
; 17-05-2009 [ TRFS Issue 1, Revision 13 ]
; 10-05-2009 [ TRFS Issue 1, Revision 12 ]
; 03-03-2007 [ TRFS Issue 1, Revision 11 ]

bsFSystemID:    db 'FS'
                db 0   
                ; Offset 6  
bsBytesPerSec:  dw 512
                ; Offset 8
bsMediaAttrib:  db 1
bsPartitionID:  db 0A1h
bsFSVersionMaj: db 01h
bsFSVersionMin: db 0
                ; Offset 12    
bsBootSector:   dd 0 
bsVolumeSize:   dd 0
                ; Offset 20
;bsStartupFD:   dd 0
bsStartupFD:    dd 0
bsMATLocation:  dd 1
bsRootDirD:     dd 0
bsSystemConfFD: dd 0
bsSwapFD:       dd 0
bsUndeleteDirD: dd 0
		; Offset 44
bsDriveNumber:  db 0
bs_LBA_Ready:   db 01h
bsMagicWord:
bs_Disk_SecPerTrack:
		db 0A1h
bs_Disk_Heads: 
                db 01h 
                
                ; Offset 48
bsOperationSys: db "TR-SINGLIX v1.0b"
terminator:     db 0
                ;nop 
loc_offset_65:  
                push    cs
                pop     ds
                cli
                push    ds
                pop     ss
                mov     sp, 0FFFEh
                sti

                mov     word ptr [bsMagicWord], ax

                ;mov byte ptr [bsDriveNumber], DL ; from INT 19h
                mov dl, 80h

                mov ah, 41h ; Check INT 13h Extensions Present
                mov bx, 55AAh
                ; mov dl, drive number
                int 13h
                jc short loc_bs_lba_not_ready
                cmp bx, 0AA55h
                je short loc_bs_lba_ready

loc_bs_lba_not_ready:
                mov ah, 08h
                ;mov dl, byte ptr [bsDriveNumber]
                int 13h
                jc  short loc_drv_reading_err
                inc dh
                mov byte ptr [bs_Disk_Heads], dh
                mov byte ptr [bs_Disk_SecPerTrack], cl
                xor cl,cl
loc_bs_lba_ready: 
 		; Fixed Disk Access Subset - LBA ready bit 
                and byte ptr [bs_LBA_ready], cl
                                         
		mov ax, word ptr [bsStartupFD]
                mov dx, word ptr [bsStartupFD]+2

               	mov bx, ax
                or bx, dx
                jnz short loc_load_startupfile

pass_load_startupfile:
                mov si, offset msg_press_any_key
loc_print_msg:                
                call proc_printmsg
 		xor ah,ah
                int 16h
                int 19h
loc_drv_reading_err:
                mov si, offset msg_drv_reading_err
                jmp short loc_print_msg

loc_load_startupfile:
 		mov bp, 7E00h
loc_load_startupfile_bp_already_7E00h:
                mov word ptr [SF_FDT_Number], ax
                mov word ptr [SF_FDT_Number]+2, dx
 		
		push    ds
                pop     es

		;mov bx, 7E00h                        
                mov bx, bp              

                mov cx, 1
                call proc_disk_read
                jc short loc_drv_reading_err

               ;mov bx, 7E00h
               ;mov bp, 7E00h
                cmp word ptr [BP], 'DF'
                jne short loc_drv_reading_err
               ;cmp byte ptr [BP]+2, 'T'
               ;jne short loc_drv_reading_err
                cmp word ptr [BP]+4, 512
                jne short loc_drv_reading_err

                mov ax, word ptr [SF_Loaded_Sectors]
                mov cx, word ptr [BP]+10h
                mov dx, word ptr [BP]+12h
                mov bx, ax
                cmp dx, 0
                ja short pass_load_startupfile
                cmp cx, 1
                jb short pass_load_startupfile
                add bx, cx
                jc short pass_load_startupfile  
                cmp bx, 1151
                ja short pass_load_startupfile 
                
                mov bx, 32
                mul bx
                cmp dx, 0
                ja short pass_load_startupfile 
                add ax, 1010h 
                ;jc short pass_load_startupfile
                ;cmp ax, 0A00h
                ;jnb short pass_load_startupfile 
                mov es, ax
                xor bx, bx
                mov ax, word ptr [SF_FDT_Number]
                mov dx, word ptr [SF_FDT_Number]+2 
                add ax, 1
                adc dx, 0  
                call proc_disk_read
                jc short loc_drv_reading_err

                ;push cs
                ;pop es
 		add word ptr [SF_Loaded_Sectors], cx
                mov ax, word ptr [BP]+0Ch ; Next FDT Number lw
                mov dx, word ptr [BP]+0Eh ; Next FDT Number hw

                mov bx, ax
                or bx, dx
		jnz short loc_load_startupfile_bp_already_7E00h


loc_launch_startupfile:
                mov si, offset msg_CRLF
                call proc_printmsg
               
                ; To get warranty for proper running of any COM program
                ; 25-10-2008
                mov ax, 1000h
                mov ds, ax
                mov es, ax
                cli
                mov ss, ax
                mov sp, 0FFFEh 
                sti

               ;push ax
               ;mov ax, 100h
               ;push ax 

                ; 2006
                mov ax, 417 ; Startup File Loader Sign (Boot Time Loading)
                ; 21/02/2010
                mov dl, 80h ; DL = 80h Drive Number
              
             ; MASM.EXE don't accept
             ; jmp 1000h:0100h
             ; for OP Code: EA00010010
               db 0EAh
               dw 0100h
               dw 1000h

               ;retf  
                  
;NeverComeHere:  jmp NeverComeHere               
               
SF_Loaded_sectors: dw 0
SF_FDT_Number: dd 0

proc_start endp

proc_printmsg   proc near

                mov BX, 07
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

proc_printmsg   endp

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
ret_div32:
                retn

Rx_Dos_Div32    endp

;#############################################################################
;#
;#              PROCEDURE proc_disk_lba_chs_read
;#
;#############################################################################

; (c) Erdogan Tan  [10/05/2009]   // Compact code (LBA and CHS read) //

proc_disk_read  proc    near
 		
		;pusha                           ; db 60h
                db 60h

                ; to absolute location on the disk 
                add 	ax, word ptr [bsBootSector]
                adc 	dx, word ptr [bsBootSector]+2

loc_read_lba_chs_sector:                
                mov 	DI,5
loc_read_lba_chs_0FFh:
 		;pusha
                db 60h
                
		cmp 	byte ptr [bs_LBA_ready], 0
                jna 	short loc_read_chs_0CAh

loc_read_lba_0FFh:
               

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

               ;mov     DL, byte ptr [bsDriveNumber] 
                mov     DL, 80h
                mov     AH, 42h
                mov     SI, SP
                int     13h

                ;popa
                db 61h
 		
loc_check_int13h_read_return:
 		;popa
                db 61h
                jnc     short loc_read_lba_chs_12Bh
                
                dec     DI 

               ; jz      short loc_read_lba_chs_12Bh
               
                jnz     short loc_read_lba_chs_0FFh  
               ; xor     AH, AH
               ; mov     DL, byte ptr [bsDriveNumber] 
              ; mov     DL, 80h
               ; int     13h
                
               ; jmp     short loc_read_lba_chs_0FFh                  
 
               jmp 	short loc_read_lba_chs_popa_retn
              
loc_read_lba_chs_12Bh:
                loop 	loc_read_lba_chs_next_sector

loc_read_lba_chs_popa_retn:                 
               ;popa
                db 61h

                retn                            ; db 0C3h

loc_read_lba_chs_next_sector:
                add 	ax, 1
                adc 	dx, 0 
                add 	bx, 512
                jnc 	short loc_read_lba_chs_sector
                mov 	di, es
                add 	di, 1000h
                mov 	es, di
                jmp 	short loc_read_lba_chs_sector

loc_read_chs_0CAh:
                ;push    CX			; Sector Count               
                ;push    AX                     ; Linear sector #
                ;push    DX                     ; DX_AX = Linear address (sectors)
                xor     CH,CH
                mov     CL,byte ptr [bs_Disk_SecPerTrack]
                push    BX

                call    RX_DOS_DIV32            ; Special 32 bit divide !!!
                                                ; To fix large disk problem.
                                                ; (c) Erdogan Tan 1999
                                                ; (October 20th, 1999)

                mov     CX,BX                   ; Sector (zero based)
                inc     CX                      ; To make it 1 based
                push    CX
                mov     CL,byte ptr [bs_Disk_Heads]
                call    RX_DOS_DIV32            ; Convert track to head & cyl
                mov     DH,BL                   ; BX = Head (max. FEh)
                pop     CX
                                                ; AX=Cyl, DH=Head, CX=Sector
                pop     BX                      ; ES:BX = Buffer

               ;mov     DL,byte ptr [bsDriveNumber]
                mov     DL,80h
                mov     CH,AL
                ror     AH,1                    ; Rotate right
                ror     AH,1                   
                or      CL,AH                   
                mov     AX,0201h
                int     13h                     ; BIOS Service func ( ah ) = 2
                                                ; read disk sectors
                                                ;AL-sec num CH-track CL-sec
                                                ; DH-head DL-drive ES:BX-buffer
                                                ;CF-flag AH-stat AL-sec read
                                                ; If CF = 1 then (If AH > 0)
                ;pop     DX
                ;pop     AX
                                                
                ;pop    CX
		jmp 	short loc_check_int13h_read_return

               ;jnc     short loc_read_lba_chs_12Bh              
               ;dec     DI                      
               ;jz      short loc_read_lba_chs_12Bh              
                
               ;xor     AH,AH                   
               ;mov     DL,byte ptr [bsDriveNumber]
               ;mov     DL,80h
               ;int     13h                     ; BIOS Service func ( ah ) = 0
                                                ; Reset disk system
               ;jnz     short loc_read_chs_0CAh                  
               ;jmp     short loc_read_chs_0CAh

               ;retn           
                 
proc_disk_read  endp

msg_drv_reading_err:
                db 7 
                db 0Dh,0Ah
                db "Disk read error!"
msg_press_any_key:
                db 0Dh,0Ah
                db "Replace disk & press any key to reboot."
                db 0Dh,0Ah
msg_CRLF:       db 0Dh,0Ah
                db 1 dup(0)

                org 7DFEh

bsBootSign:     dw 0AA55h

SINGLIXBOOT     ends

                end     START
