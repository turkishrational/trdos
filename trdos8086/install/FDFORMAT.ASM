; ****************************************************************************
;
; FDFORMAT.ASM
;
; Turkish Rational SINGLIX
; Disk Operation System v1.0 Project
; 1.44 MB Floppy Disk Formatting Code 
;
; Copyright (C) 2006-2010  Erdogan TAN  [ Last Modification: 21/02/2010 ]
; [ 03/01/2010 ]
; MULTIXFD.ASM :
;[ 15-1-2006 | 4-3-2007 | 29-3-2008 | 25/10/2008 | 24/03/2009 | 26/05/2009 ]
;
; ****************************************************************************


SINGLIXBOOT     SEGMENT PUBLIC 'CODE'
                assume cs:SINGLIXBOOT,ds:SINGLIXBOOT,es:SINGLIXBOOT,ss:SINGLIXBOOT

                org 100h

        ;''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''';
        ;  Write Stub                                                   ;
        ;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -;
        ;                                                               ;
        ;  The stub loads at the normal 100h load address and writes    ;
        ;  the boot sector to drive A:                                 ;
        ;...............................................................;

SMART_STARTUP_CODE:
                cmp ax, 417   ; Am I Startup File Code or Formatting Code ?...
                jne SINGLIX_WRITESTUB ; Formatting Code
                xor ax, ax
                mov ds, ax
                mov es, ax
                mov si, 7E40h
                call loc_startup_file_printmsg
                mov si, msg_CRLF
                call loc_startup_file_printmsg     
                mov ax, 8 ; RDT Location
                mov bx, 7E00h ; Root Dir. Descriptor Table Memory Location
                mov cx, 1
                call sf_proc_read
                jc short failure_load_rootdir
                cmp word ptr [BX], 'DR'
                jne short failure_load_rootdir
                cmp byte ptr [BX]+2, 'T'
                jne short failure_load_rootdir
                cmp word ptr [BX]+4, 512
                jne short failure_load_rootdir
                mov cx, word ptr [BX]+16
                cmp cx, 1
                jb short failure_load_rootdir
                push cs
                pop ds
                mov si, offset msg_sf_volume_name
                call loc_startup_file_printmsg
                push es
                pop ds
                mov si, 7E40h ; [BX]+64
                call loc_startup_file_printmsg
                mov ax, word ptr [7E1Eh] ;[Volume_Serial_No]
                mov dx, word ptr [7E1Ch] ;[Volume_Serial_No]+2
                push cs
                pop ds
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
                call loc_startup_file_printmsg
loc_sf_reboot:
                ;push es
                ;pop ds
                xor ax,ax
                int 16h
                int 19h

loc_startup_file_printmsg:
                lodsb                           ; Load byte at DS:SI to AL
                and     AL,AL            
                jz      short loc_sf_printmsg_OK       
                mov     AH,0Eh                  
                mov     BX,07h             
                int     10h                     ; BIOS Service func ( ah ) = 0Eh
                                                ; Write char as TTY
                                                ; AL-char BH-page BL-color
                jmp     short loc_startup_file_printmsg           

loc_sf_printmsg_OK:
                retn

failure_load_rootdir:
                push cs
                pop ds 
                mov si, offset msg_read_error
                call loc_startup_file_printmsg 
                jmp short loc_sf_reboot
                
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

sf_proc_read:
                ; Only for 1.44 MB Floppy Disks (18 sector/track)
                
                mov     byte ptr [sf_RetryCount], 4
sf_loop_loc_14:
                push    CX
                push    AX                      ; PHYSICAL ADRESS CALCULATION
                mov     CL, 18                  ; Sectors per track
                div     CL                      
                mov     CL, AH                  ; Sector (zero based)
                inc     CL                      ; To make it 1 based
                xor     AH, AH
                mov     DL, 2                   ; Heads 
                div     DL
                                                ; AL=cyl, AH=head, CL=sector
                mov     DH, AH
                mov     DL, byte ptr [bsDriveNumber] 
                mov     dl,0 
                mov     CH, AL

                mov     AX,0201h
                int     13h                     ; BIOS Service func ( ah ) = 2
                                                ; Read disk sectors
                                                ;AL-sec num CH-track CL-sec
                                                ; DH-head DL-drive ES:BX-buffer
                                                ;CF-flag AH-stat AL-sec read
                pop     AX                      
                pop     CX
                jc      short sf_loc_16
                inc     AX
sf_loc_15:                 
                dec     CX
                jz      short sf_loc_17
                add     BX, 512
                mov     byte ptr [sf_RetryCount], 4
                jmp     short sf_loop_loc_14
sf_loc_16:         
                dec     byte ptr [sf_RetryCount]
                jnz     short sf_loop_loc_14
                stc
sf_loc_17:
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
                db "Remove the disk and press any key to reboot..."
                db 0Dh,0Ah,0Dh,0Ah,0
msg_read_error:
                db 'Disk read error! Press any key to reboot...'
                db 0Dh,0Ah,0Dh,0Ah,0
                
size_of_sf      equ ($- SMART_STARTUP_CODE)  
                

SINGLIX_WRITESTUB:

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; see if drive specified
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

                mov si, offset 80h               ; PSP command tail
                mov cl, byte ptr [ si ]
                or cl, cl                               
                jz WRITESTUB_12                  ; jump if zero

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
                jne WRITESTUB_08ab
                inc si
                mov al, byte ptr [SI]
                cmp al, "d"
                jne WRITESTUB_12
                inc si
                mov ax, word ptr [SI]
                cmp al, '0'                            
                jb WRITESTUB_12
                cmp al, '1'
                ja WRITESTUB_12
                cmp ah, 20h
                ja WRITESTUB_12
                mov byte ptr [SINGLIX_DRIVE], al
                sub al, '0'
                jmp short WRITESTUB_10

WRITESTUB_08ab:
                cmp al, 'A'
                jc short WRITESTUB_12
                cmp al, 'B'                             ; A - Z
                jna short WRITESTUB_10ab                    
                cmp al, 'a'                             ; a - z 
                jc short WRITESTUB_12                  
                cmp al, 'b'                           
                ja short WRITESTUB_12                 

                sub al, 'a'-'A'                         ; to upper case

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Write message
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

WRITESTUB_10ab:
                mov byte ptr SINGLIX_DRIVE, al
                sub al, 'A'                             ; make it zero based 
WRITESTUB_10:
                mov dl, al
                mov byte ptr [bsDriveNumber], dl
                mov ah, 08h
                int 13h                                 ; return disk parameters
                push cs
                pop es                                  ; restore es
                jc WRITESTUB_30

                cmp bl, 04                              ; Drive Type
                jb WRITESTUB_30

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
                je WRITESTUB_60_N                         ; no write (exit)

                jmp short WRITESTUB_14

WRITESTUB_09:
                add al, 'A'-'0'                         ; 0 based -> A based
                jmp short WRITESTUB_10 

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
               
                mov ah, 04h                             ; Return Current Date
                int 1Ah
                xchg ch,cl
                mov word ptr [rdt_make_year], cx
                xchg dh,dl
                mov word ptr [rdt_make_month], dx
                mov word ptr [rdt_lm_year], cx
                mov word ptr [rdt_lm_month], dx

                mov ax, size_of_sf
                mov word ptr [fdt_file_size], ax
                
                add ax, 511
                ;mov dx, 0 
                xor dx, dx
                mov cx, 512
                div cx
               ;xor dx, dx
                mov word ptr [fdt_scount], ax
               ;mov word ptr [fdt_scount]+2, dx
                add ax, 10
                inc ax
                mov word ptr [mat_dat_ffs], ax
               ;mov word ptr [mat_dat_ffs]+2, dx
               
               ;sub word ptr [mat_dat_free_s], ax
               ;sbb word ptr [mat_dat_free_s]+2, dx

 		mov dx, 2880
                sub dx, ax
                mov word ptr [mat_dat_free_s], dx

               ;push ds
               ;pop es
                mov ax, word ptr [fdt_location]
                mov DI, offset FDFORMAT_DATBUFFER
                add DI, ax
                mov byte ptr [DI], 06h ;Startup File Descriptor sign
                inc DI
                mov cx, word ptr [fdt_scount]
                mov al, 07h
                rep stosb

WRITESTUB_20_Reset:
                xor ah,ah
                mov dl, byte ptr [bsDriveNumber]
                int 13h
                jc  WRITESTUB_30

WRITESTUB_22:
                mov  si, offset Msg_Writing_Boot_Sector
                call SINGLIX_PRINTMSG

                mov  byte ptr [RetryCount], 4
loc_write_bootsector:

                mov ax, 0301h                           ; write to disk
                mov bx, offset start                    ; location of boot code

                mov cx, 1                               ; cylinder = 0
                                                        ; sector = 1
                mov dh, 0                               ; head = 0
                mov dl, byte ptr [bsDriveNumber]
                int 13h
                jnc short WRITESTUB_25
                dec byte ptr [RetryCount]
                jz WRITESTUB_30
                jmp short loc_write_bootsector

WRITESTUB_25:
                mov  si, offset Msg_OK
                call SINGLIX_PRINTMSG
             
                inc byte ptr [fdformat_rp]
WRITESTUB_25_rp:
                mov si, offset Msg_Writing_Root_Dir
                call SINGLIX_PRINTMSG

                mov AX, 8 ; Root Directory Descriptor Address
                mov BX, offset FDFORMAT_ROOTBUFFER
loc_write_root_dir_sectors:
                call proc_write_fd_sector
                jc WRITESTUB_30
                inc AX
                add BX, 512
                call proc_write_fd_sector
                jc WRITESTUB_30

                mov  si, offset Msg_OK
                call SINGLIX_PRINTMSG

WRITESTUB_27:
                inc byte ptr [fdformat_rp]
WRITESTUB_27_rp:
                mov si, offset Msg_Writing_Data_Sectors
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

                mov AX, 10 ; Data Sectors Beginning Address
loc_write_data_sectors:
                push AX
                call proc_bin_to_decimal
                pop AX
                mov BX, offset FDFORMAT_SECBUFFER
                call proc_write_fd_sector
                jnc short loc_write_data_sectors_cont
                and ah, 16h  ; Errors: 2h, 4h, 10h
                jz WRITESTUB_30 ; Drive not ready msg
                ;DX= LBA sector value
WRITESTUB_31:
                push dx
                mov bx, offset FDFORMAT_DATBUFFER
                add bx, ax
                mov byte ptr [BX], 9Bh ; BAD SECTOR Sign
                pop ax
loc_write_data_sectors_cont:
                inc AX
                cmp AX, 2880
                jb short loc_write_data_sectors

                mov  si, offset Msg_3dot_OK
                call SINGLIX_PRINTMSG
WRITESTUB_35:
                inc byte ptr [fdformat_rp]
WRITESTUB_35_rp:
                mov si, offset Msg_Writing_DAT_Sectors
                call SINGLIX_PRINTMSG

                mov AX, 1 ; MAT Beginning Address
                mov BX, offset FDFORMAT_MATBUFFER
loc_write_dat_sectors:
                call proc_write_fd_sector
                jc WRITESTUB_30
                inc AX
                add BX, 512
                cmp AX, 7
                jna short loc_write_dat_sectors
  
WRITESTUB_37:
                mov si, offset Msg_OK
                call SINGLIX_PRINTMSG

                inc byte ptr [fdformat_rp]

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
               
WRITESTUB_37_rp: 
                mov si, offset Msg_writing_sf
                call SINGLIX_PRINTMSG
                mov ax, 10 ; STARTUP FILE FDT ADDRESS
                mov bx, offset FDFORMAT_FDTBUFFER     
                call proc_write_fd_sector
                jc short WRITESTUB_30
                
                mov cx, word ptr [fdt_scount]
                mov bx, offset fdt_file_data
                mov si, offset SMART_STARTUP_CODE
WRITESTUB_38:        
                push cx
                mov cx, 512
                mov di, bx
                rep movsb
                pop cx 
                inc ax
                call proc_write_fd_sector
                jc WRITESTUB_30
                loop WRITESTUB_39 
 
		mov si, offset Msg_OK
                call SINGLIX_PRINTMSG
                
                jmp short WRITESTUB_60
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

WRITESTUB_60_N:
                mov si, offset msg_NO
                call proc_printmsg
WRITESTUB_60:
                mov si, offset SINGLIX_CRLF
                call SINGLIX_PRINTMSG

                int 20h

WRITESTUB_30:
                mov si, offset SINGLIX_disk_NotReadyOrError
                call SINGLIX_PRINTMSG
WRITESTUB_40:
                xor ax, ax
                int 16h                                 ; wait for keyboard command
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
                mov dl, byte ptr [bsDriveNumber]
                mov ah, 08h
                int 13h                              ; return disk parameters
                push cs
                pop  es                              ; restore es
                jc WRITESTUB_30

                cmp bl, 04                           ; Drive Type
                jb WRITESTUB_30

                xor ah,ah
                mov dl, byte ptr [bsDriveNumber]
                int 13h 
                jc WRITESTUB_30  
                
                
                cmp byte ptr [fdformat_rp], 2
                je WRITESTUB_27_rp ; DATA Sectors
                jb short check_multix_rp_1
                cmp byte ptr [fdformat_rp], 3
                ja WRITESTUB_37_rp ; STARTUP_FILE
                jmp WRITESTUB_35_rp ; DAT Sectors
check_multix_rp_1:                        
                cmp byte ptr [fdformat_rp], 1
                je WRITESTUB_25_rp ; ROOT Dir
                jmp WRITESTUB_22   ; BOOT Sector

fdformat_rp:    db 0


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


proc_write_fd_sector    proc near
                ; Only for 1.44 MB Floppy Disks
                ; INPUT -> AX = Logical Block Address
                ; ES:BX = Sector Buffer
                ; OUTPUT -> clc or stc
                ; CLC -> AX = Logical Block Address
                ; STC -> DX = Logical Block Address
                ; STC -> AH = Error Number
                PUSH    CX
                mov     CX, 4  ; Retry Count
loc_write_fdisk_chs:
                push    AX                      ; Linear sector number
                push    CX                      
                mov     DX, 18                  ; Sectors per Track
                div     DL
                mov     CL, AH                  ; Sector (zero based)
                inc     CL                      ; To make it 1 based
                shr     AL, 1                   ; Convert Track to Cylinder
                adc     DH, 0                   ; Head (0 or 1)
                mov     DL, Byte Ptr [bsDriveNumber]
                mov     CH,AL                   
                mov     AX,0301h
                int     13h                     ; BIOS Service func ( ah ) = 3
                                                ; Write disk sectors
                mov     byte ptr [Error_Code], AH
                pop     CX
                pop     AX

                jnc     short pass_write_fdisk_chs_error
                loop    loc_write_fdisk_chs
                mov     DX, AX
                mov     AH, byte ptr [Error_Code]
                stc
pass_write_fdisk_chs_error:
                POP     CX
                retn

proc_write_fd_sector    endp


proc_bin_to_decimal proc near
               ; 1-3-2005, 25-5-2009
               ;  Erdogan Tan
               ; INPUT: DS:SI = Target location
               ;        AX= Binary Number (Integer)
               ; OUTPUT: Decimal char at DS:SI
               ; SI decremented after every division
               ; till AX<10.
               ; CX, DX will be changed.

               mov si, offset Sector_Str 
               push si
               mov cx, 3
loc_btd_reset_sector_str:
               mov word ptr [SI], 30h
               inc si
               loop loc_btd_reset_sector_str
               mov cl, 10
loc_btd_re_divide:
               xor dx, dx
               div cx
               add dl, '0'
               mov byte ptr [SI], dl
               cmp ax, 0
               jna short print_decimal_sector_number
               dec si
               jmp short loc_btd_re_divide

print_decimal_sector_number:
               ; 23/05/2009
                mov ah, 1                      ; Set Cursor Type 
                mov ch, 20h                    ; Disable cursor  
                int 10h          
                mov ah, 2                      ; Set Cursor Position    
                mov dx, word ptr [Cursor_Pos]
                xor bh, bh
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


proc_rw_char   proc    near
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


;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;  messages
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

SINGLIX_Welcome:
                db 0Dh, 0Ah
                db 'TR-SINGLIX 1.44 MB Floppy Disk (FS1) Format Utility'
                db 0Dh, 0Ah
                db '(c) Erdogan TAN 2006-2010'
                db 0Dh,0Ah
                db 0Dh,0Ah
                db 'Usage: fdformat [Drive] '
                db 0Dh,0Ah
                db 0Dh,0Ah
                db "Drive names:"
                db 0Dh,0Ah
                db 0Dh,0Ah
                db "fd0    (Floppy Disk 1)", 0Dh, 0Ah
                db "fd1    (Floppy Disk 2)", 0Dh, 0Ah
                db "...", 0Dh, 0Ah
                db "A:     (Floppy Disk 1)", 0Dh, 0Ah
                db "B:     (Floppy Disk 2)", 0Dh, 0Ah
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
                db 'Do you want to format drive '
SINGLIX_DRIVE:
                db 'A: (Yes/No)? ', 0

Msg_Writing_Boot_Sector:
                db 0Dh, 0Ah
                db "Writing SINGLIX boot sector...", 0
Msg_Writing_Root_Dir:
                db 0Dh, 0Ah
                db "Writing root directory sectors...", 0
Msg_Writing_Data_Sectors:
                db 0Dh, 0Ah
                db "Writing data sector: ", 0

Sector_Str:     db "0000", 0
Cursor_Pos:     dw 0
Cursor_Type:    dw 0

Msg_Writing_DAT_Sectors:
                db 0Dh, 0Ah
                db "Writing disk allocation table sectors...", 0
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

SINGLIX_disk_NotReadyOrError:
                db 0Dh, 0Ah
                db 'Disk error or drive not ready. Try again? (Y/N) '
                db 0

Error_Code:     db 0

FDFORMAT_SECBUFFER:
                db 512 dup(0F6h)

FDFORMAT_MATBUFFER:
                db 'MAT'                 
                db 0
mat_volume_size:dd 2880 ; Volume Size 
mat_begin_sec:  dd 0    ; Volume Beginning Sector
mat_dat_lba:    dd 2    ; DAT LBA
mat_dat_secs:   dd 6    ; DAT Sectors
mat_dat_free_s: dd 2870 ; Free Sectors on DAT
mat_dat_ffs:    dd 10   ; First Free sector
		db 484 dup(0)  

FDFORMAT_DATBUFFER:
                db 01   ; Volume Boot Sector
                db 02   ; Master Allocation Table
                db 6 dup(03h) ; Disk Allocation Table Sectors
                db 04   ; Root Directory Descriptor
                db 05h  ; Directory Data
                db 2870 dup(90h) ; 90h = free sector in volume
                db 192 dup(0h) ; 0 = out of volume

FDFORMAT_ROOTBUFFER:
                db 'RDT'; Root Directory Description Table
                db 0 ; RDT Version
                dw 512 ; Bytes per Sector
                dw 0 ; RDT Number
RDT_Location:   dd 8 ; The First RDT Address
                dd 0 ; Next RDT number                 
RDT_SCount:     dd 1 ; Sector Count                 
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


FDFORMAT_FDTBUFFER:
                db 'FDT'; File Description Table
                db 0 ; FDT Version
                dw 512 ; Bytes per Sector
                dw 0  ; FDT Number
fdt_location:   dd 10 ; The First FDT Address
                dd 0  ; Next FDT number                 
fdt_scount:     dd 1  ; Sector Count
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
                db 'Turkish Rational SINGLIX FD STARTUP File v1.0'
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

                db  1 dup (?)                           ; trick for assembler
                                                        ; to keep 'start'
                                                        ; at 7C00h
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
; 25-05-2009 FS1 1.44MB FD Boot Sector
; 17-05-2009 [ TRFS Issue 1, Revision 13 ]
; 10-05-2009 [ TRFS Issue 1, Revision 12 ]
; 03-03-2007 [ TRFS Issue 1, Revision 11 ]

bsFSystemID:    db 'FS'
                db 0   
                ; Offset 6  
bsBytesPerSec:  dw 512
                ; Offset 8
bsMediaAttrib:  db 3
bsPartitionID:  db 0A1h
bsFSVersionMaj: db 01h
bsFSVersionMin: db 0
                ; Offset 12    
bsBootSector:   dd 0 
bsVolumeSize:   dd 2880
                ; Offset 20
;bsStartupFD:   dd 0
bsStartupFD:    dd 10
bsMATLocation:  dd 1
bsRootDirD:     dd 8
bsSystemConfFD: dd 0
bsSwapFD:       dd 0
bsUndeleteDirD: dd 0
		; Offset 44
bsDriveNumber:  db 0
bs_LBA_Ready:   db 0
;bsMagicWord:
bs_Disk_SecPerTrack:
		db 18
bs_Disk_Heads: 
                db 2
                
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

                mov byte ptr [bsDriveNumber], DL ; from INT 19h
	       ;mov byte ptr [bs_Disk_SecPerTrack], 18
               ;mov byte ptr [bs_Disk_Heads], 2 

                mov ax, word ptr [bsStartupFD]
                mov dx, word ptr [bsStartupFD]+2 
                
                cmp dx, 0
                ja short pass_load_startupfile

                cmp ax, 0
                ja short loc_load_startupfile

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
                		
		push    ds
                pop     es

		;mov bx, 7E00h                        
                mov bx, bp              

                mov cx, 1
                call proc_disk_read
                jc short loc_drv_reading_err

                cmp word ptr [BP], 'DF'
                jne short loc_drv_reading_err
                cmp byte ptr [BP]+2, 'T'
                jne short loc_drv_reading_err
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
                ja short loc_drv_reading_err
                add ax, 1010h 
                jc short loc_drv_reading_err
                cmp ax, 0A000h
                jnb short loc_drv_reading_err
                mov es, ax
                xor bx, bx
                mov ax, word ptr [SF_FDT_Number]
                add ax, 1
                call proc_disk_read
                jc short loc_drv_reading_err
                
                add word ptr [SF_Loaded_Sectors], cx
                mov ax, word ptr [BP]+0Ch ; Next FDT Number lw
                mov dx, word ptr [BP]+0Eh ; Next FDT Number hw
                cmp dx, 0
                ja short loc_drv_reading_err
                cmp ax, 0
                ja short loc_load_startupfile_bp_already_7E00h

loc_launch_startupfile:
                mov si, offset msg_CRLF
                call proc_printmsg
               
                ; To get warranty for proper running of any COM program
                ; 25-10-2008
                mov ax, 1000h
                push ax
                pop ds
                push ax
                pop es
                cli
                ;cld 
                push ax
                pop ss
                mov sp, 0FFFEh 
                sti

                ; 2006
                mov ax, 417 ; Startup File Loader Sign (Boot Time Loading)
                ; 21/02/2010
                xor dx, dx  ; DL = Drive Number

             ; MASM.EXE don't accept
             ; jmp 1000h:0100h
             ; for OP Code: EA00010010
                db 0EAh
                dw 0100h
                dw 1000h 
                  
NeverComeHere:  jmp NeverComeHere               

proc_start endp

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


proc_disk_read       proc    near

                ; Only for 1.44 MB Floppy Disks (18 sector/track)
                
                mov     byte ptr [bs_RetryCount], 4
loop_loc_14:
                push    CX
                push    AX                      ; PHYSICAL ADRESS CALCULATION
                mov     CL, 18                  ; Sectors per track
                div     CL                      
                mov     CL, AH                  ; Sector (zero based)
                inc     CL                      ; To make it 1 based
                xor     AH, AH
                mov     DL, 2                   ; Heads 
                div     DL
                                                ; AL=cyl, AH=head, CL=sector
                mov     DH, AH
                mov     DL, byte ptr [bsDriveNumber] 
                mov     dl, 0
                mov     CH, AL

                mov     AX,0201h
                int     13h                     ; BIOS Service func ( ah ) = 2
                                                ; Read disk sectors
                                                ;AL-sec num CH-track CL-sec
                                                ; DH-head DL-drive ES:BX-buffer
                                                ;CF-flag AH-stat AL-sec read
                pop     AX                      
                pop     CX
                jc      short loc_16
                inc     AX
loc_15:                 
                dec     CX
                jz      short loc_17
                add     BX, 512
                mov     byte ptr [bs_RetryCount], 4
                jmp     short loop_loc_14
loc_16:         
                dec     byte ptr [bs_RetryCount]
                jnz     short loop_loc_14
                stc
loc_17:
                retn

proc_disk_read  endp

SF_FDT_Number:  dw 0
SF_Loaded_Sectors: dw 0

bs_RetryCount:  db 4
                db 1
msg_CRLF_beep:  db 7
msg_CRLF:
                db 0Dh,0Ah,0

msg_drv_reading_err:
                db 0Dh,0Ah
                db "Drive not ready or read error !"
                db 0Dh,0Ah,0
msg_press_any_key:
                db 0Dh,0Ah
                db "Remove the disk and press any key to reboot..."
                db 0Dh,0Ah,0Dh,0Ah,0

                db 1 dup(0)

                org 7DFEh

bsBootSign:     dw 0AA55h

SINGLIXBOOT     ends

                end     SMART_STARTUP_CODE
