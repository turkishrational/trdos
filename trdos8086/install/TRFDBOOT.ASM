; ****************************************************************************
;
; TRFDBOOT.ASM
;
; Turkish Rational DOS
; Disk Operation System v1.0 Project
; 1.44 MB Floppy Disk Boot Code
;
; Copyright (C) 1998-2011  Erdogan TAN  [ Last Modification: 31/07/2011 ]
;
; 12/06/2011 -> Boot sector bugfix (dl = bsDriveNumber <-> DS=CS)
;
; 27/09/2009 -> 'Operation System' has been changed to 'Operating System'
;
; 2005 -> trfdboot utility
; 1998 -> beginning year of trdos project
;
; ****************************************************************************

notvalidfmask   equ    0018h

root_dir_buff   equ    0700h
rts_segment     equ    1010h
FAT_Buffer      equ    0700h

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
; get drive code
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

RxDOS_WRITESTUB_10:
                mov byte ptr RxDOS_DRIVE, al
                jmp short RxDOS_WRITESTUB_20_rdp

RxDOS_WRITESTUB_09:
                add al, 'A'-'0'                         ; 0 based -> A based
                jmp short RxDOS_WRITESTUB_10 

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Write message
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

RxDOS_WRITESTUB_12:
                mov si, offset RxDOS_Welcome
                call RxDOS_PRINT
                cmp cl,0
                ja short RxDOS_WRITESTUB_60

RxDOS_WRITESTUB_13:
                mov si, offset RxDOS_PressKeyWhenReady
                call RxDOS_PRINT

RxDOS_WRITESTUB_14:
                xor ax, ax
                int 16h                                 ; wait for keyboard command
                cmp al, 'M'-40h                         ; Enter (OK) key
                je short RxDOS_WRITESTUB_20             ; write
                cmp al, 'C'-40h
                je short RxDOS_WRITESTUB_60             ; no write (exit)
                cmp al, 27
                je short RxDOS_WRITESTUB_60
                jmp short RxDOS_WRITESTUB_14            

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; get drive parameters
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

RxDOS_WRITESTUB_20:
                mov si, offset RxDOS_CRLF
                call RxDOS_PRINT

RxDOS_WRITESTUB_20_rdp:
                mov ah, 08h
                mov dl, byte ptr RxDOS_DRIVE            ; drive
                sub dl, 'A'                             ; make it zero based 
                int 13h                                 ; return disk parameters

                push cs
                pop es                                  ; restore es

                cmp bl, 04                              ; Drive Type
                jb short RxDOS_WRITESTUB_30

RxDOS_WRITESTUB_20_retry_4:

                mov byte ptr [RetryCount], 4

RxDOS_WRITESTUB_20_retry:

                xor ax, ax
                int 1Ah                                 ; get time of day

              ; 31/07/2011
                mov si, offset bsVolumeID 

                mov word ptr [SI], dx
                mov word ptr [SI]+2, cx		        ; set unique volume ID

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
                mov dl, byte ptr RxDOS_DRIVE            ; drive 
                sub dl,'A'                              ; make it zero based
                mov byte ptr bsDriveNumber, dl
                int 13h

                jc short RxDOS_WRITESTUB_30             ; if everything is ok -->


;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; success. try again ?
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

RxDOS_WRITESTUB_36:
                mov si, offset RxDOS_disk_WrittenSuccesfully
                call RxDOS_PRINT

RxDOS_WRITESTUB_60:
                mov si, offset RxDOS_CRLF
                call RxDOS_PRINT
                mov ax, 4c00h                           ; terminate
                int 21h

RxDOS_WRITESTUB_40:
                xor ax, ax
                int 16h                                 ; wait for keyboard command
                cmp al, 'y'
                je short RxDOS_WRITESTUB_50             ; retry
                cmp al, 'Y'
                je short RxDOS_WRITESTUB_50
                cmp al, 'n'
                je short RxDOS_WRITESTUB_60             ; exit
                cmp al, 'N'
                je short RxDOS_WRITESTUB_60
                cmp al, 'C'-40h
                je short RxDOS_WRITESTUB_60                   
                cmp al, 27
                je short RxDOS_WRITESTUB_60
                jmp short RxDOS_WRITESTUB_40

RxDOS_WRITESTUB_30:

                dec byte ptr [RetryCount]
                jnz short RxDOS_WRITESTUB_20_retry

                mov si, offset RxDOS_disk_NotReadyOrError
                call RxDOS_PRINT
                jmp short RxDOS_WRITESTUB_40

RxDOS_WRITESTUB_50:
                mov si, offset RxDOS_CRLF
                call RxDOS_PRINT
                jmp  RxDOS_WRITESTUB_20_retry_4

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

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;  messages
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

RxDOS_Welcome:
                db 0Dh, 0Ah
                db 'TR-DOS Floppy Disk Boot Sector Rebuilder v3.0'
                db 0Dh, 0Ah
                db '(c) Erdogan TAN 1998-2005'
                db 0Dh,0Ah
                db 0Dh,0Ah
                db 'Usage: trfdboot [drive] '
                db 0

RxDOS_PressKeyWhenReady:
                db 0Dh, 0Ah
                db 0Dh, 0Ah
                db 'Press Enter to write boot sector on disk '
RxDOS_Drive:
                db 'A: ', 0

RxDOS_disk_WrittenSuccesfully:
                db 0Dh, 0Ah
                db 'Boot sector updated to TRDOS v1.0 format...'
RxDOS_CRLF:
                db 0Dh, 0Ah, 0

RxDOS_disk_NotReadyOrError:
                db 0Dh, 0Ah
                db 'Disk error or drive not ready. Try again? (Y/N) '
                db 0

                db  '(c) Erdogan TAN 1998-2011'

                db  1 dup (?)                           ; trick for assembler
                                                        ; to keep 'start'
                                                        ; at 7C00h
                org 7C00h

;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
;±
;±		PROCEDURE proc_start
;±
;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±

proc_start      proc    near

Start:
                jmp     short loc_2
                nop

; BootSector Identification (Data) Block

bsOemName       db 'TRDOS1.0'          
bsBytesPerSec   dw 512
bsSecPerClust   db 1
bsResSectors    dw 1
bsFATs          db 2
bsRootDirEnts   dw 224
bsSectors       dw 2880
bsMedia         db 0F0h
bsFATsecs       dw 9
bsSecPerTrack   dw 18
bsHeads         dw 2
bsHidden1       dw 0
bsHidden2       dw 0
bsHugeSectors   dd 2880
bsDriveNumber   db 0
bsReserved1     db 0
bsBpbSignature  db 29h                 
bsVolumeID      dd 0
bsVolumeLabel   db 'TRDOS      '
bsFileSysType   db 'FAT12   '          
bsReserved2     dw 'RT'

loc_2:          
                xor     AX, AX            

                push    cs
                pop     ds
                push    ds
                pop     es
                cli
                push    es
                pop     ss
                mov     sp, 0FFFEh
                sti
loc_3:
                mov     DL, byte ptr bsDriveNumber
                int     13h                     ; BIOS Service func ( ah ) = 0
                                                ; Reset disk system
                jc      short loc_ioerr         ; To print i/o error message
                                                ; AX = 0 no error
loc_4:                                 
                mov     AX, 19                  ; Root Dir Location
                mov     CX, 14                  ; Root Directory Sectors
                mov     BX, root_dir_buff       ; Destination offset = 700h
                mov     SI, BX
                call    proc_read
loc_5:          jc      short loc_ioerr         ; Disk read error message

                mov     BX, 224                 ; Number of root dir entries

loc_6:          cmp     Byte Ptr [SI],0         ; Is it null entry?
                je      short loc_filenotfound  ; Jump if zero ( = )
                mov     CX, 0Bh                 ; Size of file/directory name
                push    SI
                mov     DI, rtsfilename   
                repe    cmpsb                   ; Repeat if ZF = 1, CX > 0
						; Cmp byte at DS:SI to ES:DI
                pop	SI
                je      short loc_rtsfile_found ; If the file name found
                dec     BX                      
                jz      short loc_filenotfound  ; Jump if no next entry
                add     SI, 32                  ; To next directory entry
                jmp     short loc_6             ; Jump for next sector
loc_filenotfound:
                mov     SI, offset Replace_Msg
                jmp     short loc_10
loc_ioerr:      
                mov     SI, offset Error_Msg
loc_10:          
                call    proc_printmsg

                xor	AX,AX
		int	16h			; BIOS Service func ( ah ) = 0
						; Read next kbd char
						;AH-scan code AL-char code
		int	19h			; Reboot

loc_rtsfile_found:
                mov     AL, Byte Ptr [SI+0Bh]   ; Move attributes byte to BL
                and     AL, notvalidfmask       ; Is it a file, really?
                ja      short loc_filenotfound  ; Jump if above ( > )
                mov     AX, Word Ptr [SI+1Ah]   ; First cluster of the file
                cmp     AX, 2                   ; Start cluster
                jb      short loc_ioerr

                mov     Word Ptr [bsReserved2], AX ; Save the first cluster
                                                ; for using by the Kernel
loc_load_FAT:
                mov     AX, 1                   ; FAT Location
                mov     CX, 9                   ; FAT Sectors
                mov     BX, FAT_Buffer    
                call    proc_read
                jc      short loc_ioerr
loc_load_rtsfile:
                mov     si, offset Loading_Msg
                call    proc_printmsg
                mov     AX, word ptr [bsReserved2] ; The First cluster
                mov     BX, rts_segment
                mov     ES, BX
                xor     BX, BX
                call    proc_load_file
                jc      short loc_ioerr
loc_launch_rts:
                mov     dl, byte ptr [bsDriveNumber] ; 12/06/2011
                mov     ax, 1000h
                mov     ds, ax
                mov     es, ax
                cli
                mov     ss, ax
                mov     sp, 0FFFEh
                sti

                mov     ax, 417 ; TRDOS boot sector sign for TRDOS.COM

             ; MASM.EXE don't accept
             ; jmp     1000h:0100h
             ; for OP Code: EA00010010
                db      0EAh
                dw      0100h
                dw      1000h 

proc_start	endp

proc_printmsg   proc near
loc_9:
		lodsb				; Load byte at DS:SI to AL
                and     AL, AL            
                jnz     short loc_write_char    ; If AL = 00h then stop

                retn
loc_write_char:
                mov     AH, 0Eh
                mov     BX, 07h             
		int	10h			; BIOS Service func ( ah ) = 0Eh
						; Write char as TTY
						;AL-char BH-page BL-color
                jmp     short loc_9

proc_printmsg   endp

proc_read       proc    near

                ; Only for FAT12 Floppy Disks (18 sector/track)
                
                mov     byte ptr [RetryCount], 4
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
                mov     DL, byte Ptr bsDriveNumber 
                mov     CH, AL            

                mov     AX,0201h
		int	13h			; BIOS Service func ( ah ) = 2
						; Read disk sectors
						;AL-sec num CH-track CL-sec
						; DH-head DL-drive ES:BX-buffer
						;CF-flag AH-stat AL-sec read
		pop	AX			
                pop     CX
                jc      short loc_16
                inc     AX
loc_15:                 
                dec     CX
                jz      short loc_17
                add     BX, 512
                mov     byte ptr [RetryCount], 4
                jmp     short loop_loc_14
loc_16:         
                dec     byte ptr [RetryCount]
                jnz     short loop_loc_14
                stc
loc_17:
                retn

proc_read       endp

proc_load_file  proc near
              ; ES:BX = File Buffer
              ; AX = First Cluster Number
              ; Only for FAT12 Floppy Disks (1 sector per cluster)
loc_load_file_next_cluster:
                mov     word ptr [File_Cluster], ax
                mov     word ptr [File_BufferOff], bx
                dec     ax                      ; First cluster is cluster 2
                dec     ax               
                mov     cx, 1                   ; Sector count
                add     ax, 33                  ; Beginning sector of Data
                call    proc_read
                jc      short loc_load_file_retn
loc_cont_load_file:
                mov     ax, word ptr [File_Cluster]
                call    proc_get_next_cluster
                jnc     short loc_cont_load_file_2
                cmp     ax, 1
                cmc
loc_load_file_retn:
                retn
loc_cont_load_file_2:
                mov     bx, word ptr [File_BufferOff]
                add     bx, 512
                jmp     short loc_load_file_next_cluster

proc_load_file  endp

proc_get_next_cluster   proc near
                ; INPUT -> AX = Cluster Number
                ; OUTPUT -> clc -> No Error, AX = Next Cluster
                ; OUTPUT -> stc & AX=0 -> End Of Cluster Chain
                ; OUTPUT -> stc & AX>0 -> Error
get_FAT12_next_cluster:
                mov     bx, ax
                mov     dx, 3
                mul     dx
                shr     ax, 1 ; Divide by 2
                xchg    bx, ax
                ; BX = Buffer Byte Offset
                ; AX = Current Cluster
                mov     ax, word ptr [FAT_Buffer][BX]
                jnc     short get_FAT12_nc_even
                shr     ax, 1
                shr     ax, 1
                shr     ax, 1
                shr     ax, 1
loc_gnc_fat12_eoc_check:
                cmp     ax, 0FF7h
                jc      short loc_pass_gnc_FAT12_eoc_check
                xor     ax, ax
loc_pass_gnc_FAT12_eoc_check:
                cmc
                retn
get_FAT12_nc_even:
                and     ah, 0Fh
                jmp     short loc_gnc_fat12_eoc_check
proc_get_next_cluster endp

File_Cluster:   dw 0
File_BufferOff: dw 0

rtsfilename:
                db      'TRDOS   COM'
                db      0
Error_Msg:
                db      0Dh, 0Ah
                db      'TRDOS Loading Error!'
Replace_Msg:    db      0Dh, 0Ah
                db      'Replace the disk and press any key to reboot.'
                db      0Dh, 0Ah,0
Loading_Msg:    db      0Dh, 0Ah
                db      "Loading Operating System TRDOS ..."
                db      0Dh, 0Ah, 0
RetryCount:     db      1 dup(0)
                org     7DFEh
bootsignature:  db      55h, 0AAh

RxDOSBOOT       ends

                end     RxDOS_WRITESTUB
