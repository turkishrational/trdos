; ****************************************************************************
;
; FDFORM1.ASM
;
; Turkish Rational DOS
; Disk Operation System v1.0 Project
; 1.44 MB Floppy Disk Formatting Code
;
; Copyright (C) 2005  Erdogan TAN  [ Last Modification: 06/08/2011 ]
;
; 15/03/2005 - 31/07/2011
; 
; ****************************************************************************

bsDriveNumber   equ offset start + 36
bsVolumeID      equ offset start + 39
bsVolumeLabel   equ offset start + 43

extern  BootSector:proc    ; in TRDOS Boot Sector OBJ file    
                           ; <binobj trdosb1.com trdos BootSector>

RxDOSBOOT       SEGMENT PUBLIC 'CODE'
                assume cs:RxDOSBOOT, ds:RxDOSBOOT, es:RxDOSBOOT, ss:RxDOSBOOT

                org 100h

        ;''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''';
        ;  Write Stub                                                   ;
        ;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -;
        ;                                                               ;
        ;  The stub loads at the normal 100h load address and writes    ;
        ;  the boot sector to drive A:                                  ;
        ;...............................................................;

RxDOS_WRITESTUB:

                cli
                cld
                push cs
                pop ss
                mov sp, 0FFFEh
                sti

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; see if drive specified
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

                mov si, offset 80h                      ; PSP command tail
                mov cl, byte ptr [ si ]
                or cl, cl                               
                jz short RxDOS_WRITESTUB_12             ; jump if zero

RxDOS_WRITESTUB_06:
                inc si
                mov al, byte ptr [ si ]
                cmp al, ' '                             ; is it SPACE ?
                jnz short RxDOS_WRITESTUB_08

                dec cl                                  
                jnz short RxDOS_WRITESTUB_06                  
                jmp short RxDOS_WRITESTUB_12

RxDOS_WRITESTUB_08:
                cmp al, '0'                             ; 0 - 9
                jc short RxDOS_WRITESTUB_12
                cmp al, '9' + 1                         ; allow number for drive
                jc short RxDOS_WRITESTUB_09                    
                cmp al, 'A'
                jc short RxDOS_WRITESTUB_12
                cmp al, 'Z' + 1                         ; A - Z
                jc short RxDOS_WRITESTUB_10                    
                cmp al, 'a'                             ; a - z 
                jc short RxDOS_WRITESTUB_12                  
                cmp al, 'z' + 1                           
                jnc short RxDOS_WRITESTUB_12                 

                sub al, 'a'-'A'                         ; to upper case

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Write message
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

RxDOS_WRITESTUB_10:
                mov byte ptr RxDOS_DRIVE, al
                sub al, 'A'                             ; make it zero based 
                mov dl, al                           
                mov si, bsDriveNumber
                mov byte ptr [SI], al
                mov ah, 08h
                int 13h                                 ; return disk parameters
                jc RxDOS_WRITESTUB_30

                push cs
                pop es                                  ; restore es

                cmp bl, 04                              ; Drive Type
                jb RxDOS_WRITESTUB_30

                mov si, offset Msg_DoYouWantToFormat
                call RxDOS_PRINT

RxDOS_WRITESTUB_14:
                xor ax, ax
                int 16h                                 ; wait for keyboard command
                cmp al, 'C'-40h
                je RxDOS_WRITESTUB_60                   
                cmp al, 27
                je RxDOS_WRITESTUB_60
                and al, 0DFh
               ; 30/07/2011
                cmp al, 'Y'                             ; Yes?
                je short RxDOS_WRITESTUB_20_Y           ; write
                cmp al, 'N'                             ; No?
                jne short RxDOS_WRITESTUB_14          

RxDOS_WRITESTUB_60_N:                                   ; no write (exit)  
                mov si, offset Msg_No
                call RxDOS_PRINT
                jmp RxDOS_WRITESTUB_60

RxDOS_WRITESTUB_09:
                add al, 'A'-'0'                         ; 0 based -> A based
                jmp short RxDOS_WRITESTUB_10 

RxDOS_WRITESTUB_12:
                mov si, offset RxDOS_Welcome
                call RxDOS_PRINT
                jmp RxDOS_WRITESTUB_60

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; get drive parameters
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

RxDOS_WRITESTUB_20_Y:
                mov si, offset Msg_YES ; 30/07/2011
                call RxDOS_PRINT
RxDOS_WRITESTUB_20:
                mov si, offset CRLF
                call RxDOS_PRINT

RxDOS_WRITESTUB_20_retry:
                mov si, offset Msg_Writing_Root_Dir
                call RxDOS_PRINT

                mov AX, 19  ; Root Directory Address
                mov BX, offset FDFORMAT_FATBUFFER_S9
loc_write_root_dir_sectors:
                call proc_write_fd_sector
                jc RxDOS_WRITESTUB_30
                inc AX
                cmp AX, 32
                jna short loc_write_root_dir_sectors

                mov  si, offset Msg_OK
                call RxDOS_PRINT

                mov si, offset Msg_Writing_Data_Sectors
                call RxDOS_PRINT
                mov AH, 3
                mov BX, 7
                int 10h ; Return Cursor Position
                ; DL = Column, DH= Line
                mov word ptr  [Cursor_Pos], DX
                mov AX, 33  ; First Data Sector
loc_write_data_sectors:
                push AX
               ; 30/07/2011
                inc ax ; 1 based printing of 0 based sectors
               ;
                mov SI, offset Sector_Str + 3
                call proc_bin_to_decimal
                mov DX, word ptr [Cursor_Pos]
                mov AH, 2
                int 10h  ; Set Cursor Position
                call RxDOS_PRINT
                pop AX
                mov BX, offset FDFORMAT_SECBUFFER
                call proc_write_fd_sector
                jnc short loc_write_data_sectors_cont
                and ah, 16h  ; Errors: 2h, 4h, 10h
                jz RxDOS_WRITESTUB_30 ; Drive not ready msg
                ;DX= LBA sector value
RxDOS_WRITESTUB_37:
                push dx
                call proc_mark_bad_cluster
                pop ax
loc_write_data_sectors_cont:
                mov BX, 7
                inc AX
               ; 30/07/2011
                cmp AX, 2880
                jb short loc_write_data_sectors

                mov  si, offset Msg_3dot_OK
                call RxDOS_PRINT

                mov si, offset Msg_Writing_FAT_Sectors
                call RxDOS_PRINT
              ; 06/08/2011
                mov AX, 1  ; FAT Beginning Address
                mov BX, offset FDFORMAT_FATBUFFER
                call proc_write_fd_sector
                jc  RxDOS_WRITESTUB_30
                mov BX, offset FDFORMAT_FATBUFFER_S9
loc_write_fat1_sectors:
              ; 06/08/2011
                inc ax
                call proc_write_fd_sector
                jc  RxDOS_WRITESTUB_30
                cmp AX, 9
                jb short loc_write_fat1_sectors
                mov BX, offset FDFORMAT_FATBUFFER
                inc ax
                call proc_write_fd_sector
                jc  RxDOS_WRITESTUB_30
                mov BX, offset FDFORMAT_FATBUFFER_S9
loc_write_fat2_sectors:
              ; 06/08/2011
                inc ax 
                call proc_write_fd_sector
                jc  RxDOS_WRITESTUB_30
                cmp AX, 18
                jb short loc_write_fat2_sectors

                mov  si, offset Msg_OK
                call RxDOS_PRINT

                mov si, offset Msg_Volume_Name
                call RxDOS_PRINT
                call proc_rw_char
                jc short pass_write_vol_name_chr1
               ; 30/07/2011
                mov al, byte ptr [SI]
                cmp al, 20h
                jna short pass_write_vol_name_chr1
                mov di, bsVolumeLabel
                mov cx, 11
                inc si  
                jmp short loc_write_vol_name_chr  

loc_write_vol_name_chr_next:
               ; 30/07/2011
                lodsb
                inc di
                cmp al, 20h
                jna short pass_write_vol_name_chr0
loc_write_vol_name_chr:
                mov Byte Ptr [DI], al
                loop loc_write_vol_name_chr_next

pass_write_vol_name_chr1:
                mov  si, offset Msg_Writing_Boot_Sector
                call RxDOS_PRINT

                mov  byte ptr [RetryCount], 4
loc_write_bootsector_vl:
              ; 30/07/2011
                mov si, bsVolumeID

                xor ax, ax
                int 1Ah                                 ; get time of day
                mov word ptr [SI], dx
                mov word ptr [SI]+2, cx   		; set unique volume ID

                mov ah, 02h                             ; Return Current Time
                int 1Ah
                xchg ch,cl
                xchg dh,dl

                add cx, dx  
                add word ptr [SI]+2, cx
               
                mov ah, 04h                             ; Return Current Date
                int 1Ah
                xchg ch,cl
                xchg dh,dl

                add cx, dx  
                add word ptr [SI]+2, cx
              ; 

                mov ax, 0301h                           ; write to disk
                mov bx, offset start                    ; location of boot code

                mov cx, 1                               ; cylinder = 0
                                                        ; sector = 1
                mov dh, 0                               ; head = 0
                mov si, bsDriveNumber
                mov dl, byte ptr [SI]
                int 13h
                jnc short RxDOS_WRITESTUB_31
                dec byte ptr [RetryCount]
              ; 30/07/2011
                jnz short loc_write_bootsector_vl

RxDOS_WRITESTUB_30:
                mov si, offset RxDOS_disk_NotReadyOrError
                call RxDOS_PRINT

RxDOS_WRITESTUB_40:
                xor ax, ax
                int 16h                                 ; wait for keyboard command
                cmp al, 'C'-40h
                je short RxDOS_WRITESTUB_60                   
                cmp al, 27
                je short RxDOS_WRITESTUB_60
                and al, 0DFh
                cmp al, 'Y'
                je short RxDOS_WRITESTUB_50             ; Retry
                cmp al, 'N'
                jne short short RxDOS_WRITESTUB_40

RxDOS_WRITESTUB_60:					; Exit
                mov si, offset CRLF
                call RxDOS_PRINT

                int 20h

pass_write_vol_name_chr0:
                mov byte ptr [DI], 20h
                inc di
                loop pass_write_vol_name_chr0
                jmp short pass_write_vol_name_chr1

RxDOS_WRITESTUB_50:
                mov si, bsDriveNumber
                mov dl, byte ptr [SI]
                mov ah, 08h
                int 13h                              ; return disk parameters
                jc short RxDOS_WRITESTUB_30

                push cs
                pop es                               ; restore es

                cmp bl, 04                           ; Drive Type
                jb short RxDOS_WRITESTUB_30

                jmp RxDOS_WRITESTUB_20

RxDOS_WRITESTUB_31:
                mov  si, offset Msg_OK
                call RxDOS_PRINT
                jmp short RxDOS_WRITESTUB_60

RxDOS_PRINT     proc near

RxDOS_PRINT_LOOP:
                lodsb                           ; Load byte at DS:SI to AL
                and     AL,AL            
                jz      short RxDOS_PRINT_OK       
		mov	AH,0Eh			
                mov     BX,07h             
		int	10h			; BIOS Service func ( ah ) = 0Eh
                                                ; Write char as TTY
						;AL-char BH-page BL-color
                jmp     short RxDOS_PRINT_LOOP           

RxDOS_PRINT_OK:
                retn

RxDOS_PRINT     endp

proc_write_fd_sector    proc near
                ; Only for 1.44 MB FAT12 Floppy Disks
                ; INPUT -> AX = Logical Block Address
                ; ES:BX = Sector Buffer
                ; OUTPUT -> clc or stc
                ; CLC -> AX = Logical Block Address
                ; STC -> DX = Logical Block Address
                ; STC -> AH = Error Number
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
                mov     si, bsDriveNumber
                mov     DL, Byte Ptr [SI]
                mov     CH,AL                   
                mov     AX,0301h
                int     13h                     ; BIOS Service func ( ah ) = 3
                                                ; Write disk sectors
                mov     byte ptr [Error_Code], ah
                pop     CX
                pop     AX

                jnc     short pass_write_fdisk_chs_error
                loop    loc_write_fdisk_chs
                mov     DX, AX
                mov     ah, byte ptr [Error_Code]
                stc
pass_write_fdisk_chs_error:
                retn

proc_write_fd_sector    endp

proc_mark_bad_cluster   proc near
                ; Only for FAT12 Floppy Disks (Full FAT Buffer)
                ; INPUT -> AX = Cluster/Sector Number
                ; OUTPUT -> 0FF7h = BAD Cluster Value
                mov     dx, 3
                mul     dx
                shr     ax, 1 ; Divide by 2
                mov     bx, ax  ; FAT Buffer Byte Offset
                mov     dx, 0FF7h ; "BAD CLUSTER" sign
loc_update_fat12_cell:
                mov     ax, word ptr [FDFORMAT_FATBUFFER][BX]
                jnc     short uc_FAT12_nc_even
                and     ax, 0Fh
                shl     dx, 1
                shl     dx, 1
                shl     dx, 1
                shl     dx, 1
                or      dx, ax
                mov     word ptr [FDFORMAT_FATBUFFER][BX], dx
                retn
uc_FAT12_nc_even:
                and     ax, 0F000h
                and     dh, 0Fh
                or      dx, ax
                mov     word ptr [FDFORMAT_FATBUFFER][BX], dx
                retn

proc_mark_bad_cluster   endp

proc_bin_to_decimal proc near
               ; 1-3-2005
               ; ¸ Erdogan Tan
               ; INPUT: DS:SI = Target location
               ;        AX= Binary Number (Integer)
               ; OUTPUT: Decimal char at DS:SI
               ; SI decremented after every division
               ; till AX<10.
               ; CX, DX will be changed.
               mov cx, 10
loc_btd_re_divide:
               xor dx, dx
               div cx
               add dl,"0"
               mov byte ptr [SI], dl
               cmp ax, 0
               jna short pass_btd_re_divide
               dec SI
               jmp short loc_btd_re_divide
pass_btd_re_divide:
               retn

proc_bin_to_decimal endp

proc_rw_char   proc    near
               ; 30/07/2011
               ; 15/03/2005 
               ; OUTPUT -> DS:SI = Entered String (ASCIIZ)
               mov     si, offset StrVolumeName
               mov     bx, 7
               mov     ah, 3
               int     10h
               mov     Word Ptr [Cursor_Pos], dx
read_next_char:
               xor     ah, ah
               int     16h
               and     al, al
               jz      short loc_arrow    
               cmp     al, 0E0h          
               je      short loc_arrow
               cmp     al, 8
               jne     short char_return
loc_back:
               mov     ah, 3
               int     10h
               cmp     dl, Byte ptr [Cursor_Pos]
               jna     short loc_beep ; 30/07/2011
prev_column:
               dec     dl
set_cursor_pos:
               mov     ah, 2
               int     10h
               mov     bl, dl
               sub     bl, byte ptr [Cursor_Pos] 
               mov     cx,1
               mov     ah, 9
               mov     al, 20h
               mov     Byte Ptr [SI][BX], al
loc_write_it:
               mov     bl, 7
               int     10h
               mov     dx, Word Ptr [Cursor_Pos]
               jmp     short read_next_char
loc_beep:
               mov     ah, 0Eh
               mov     al, 7
               int     10h
               jmp     short read_next_char
loc_arrow:    
               cmp     AH, 4Bh
               je      short loc_back
               cmp     AH, 53h
               je      short loc_back
               jmp     short read_next_char
char_return:
             ; 30/07/2011
               mov     ah, 3
               int     10h
check_char_type:
               cmp     al, 20h
               jb      short loc_escape
               mov     ah, dl
               sub     ah, byte ptr [Cursor_Pos] 
               cmp     ah, 10
               ja      short loc_beep
               cmp     al, "z"
               ja      short read_next_char
               cmp     al, "a"
               jb      short pass_capitalize
               and     al, 0DFh
pass_capitalize:
               mov     bl, ah  ; 30/07/2011
               xor     ah, ah
               mov     Word Ptr [SI][BX], ax
               mov     bl, 7
               mov     ah, 0Eh
               int     10h
               jmp     short read_next_char
pass_escape:
               cmp     al, 0Dh
               jne     short read_next_char
	       mov     ah, 0Eh ; 30/07/2011
               int     10h
               mov     al, 0Ah
               int     10h
               retn
loc_escape:
               cmp     al, 1Bh
               jne     short pass_escape
               stc
               retn

proc_rw_char   endp


;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;  messages
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

RxDOS_Welcome:
                db 0Dh, 0Ah
                db 'TR-DOS 1.44 MB FAT12 Floppy Disk Format Utility'
       		db 0Dh, 0Ah
                db "v1.0.060811  (c) Erdogan TAN 2005-2011"
                db 0Dh,0Ah
                db 0Dh,0Ah
                db 'Usage: fdformat [drive] '
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
RxDOS_Drive:
                db 'A: (Yes/No)? ', 0

Msg_Writing_Boot_Sector:
                db 0Dh, 0Ah
                db "Writing trdos boot sector...", 0

Msg_Writing_Root_Dir:
                db "Writing root directory sectors...", 0

Msg_Writing_Data_Sectors:
                db "Writing data sector: ", 0

Sector_Str:     db "0000", 0
Cursor_Pos:     dw 0

Msg_Writing_FAT_Sectors:
                db "Writing FAT sectors...", 0

StrVolumeName:  db 12 dup(0)

Msg_Volume_Name:
                db "Volume Name: ", 0

Msg_3dot_OK:    db "..."
Msg_OK:         db ' OK.'
CRLF:           db 0Dh, 0Ah, 0
; 30/07/2011
Msg_YES:        db ' YES', 0
Msg_NO:         db ' NO', 0
;
RxDOS_disk_NotReadyOrError:
                db 0Dh, 0Ah
                db 'Disk error or drive not ready. Try again? (Y/N) '
                db 0

Error_Code:     db 0

FDFORMAT_SECBUFFER:
                db 512 dup(0F6h)
FDFORMAT_FATBUFFER:
                db 0F0h
                db 0FFh
                db 0FFh
FDFORMAT_FATBUFFER_S9:
                db 512 dup(0)
 
                db  '(c) Erdogan TAN 1998-2011'

RetryCount:     db  1 dup (?) 

Start: 

RxDOSBOOT       ends

                end     RxDOS_WRITESTUB