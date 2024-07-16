; ****************************************************************************
;
; TRHDBOOT.ASM
;
; Turkish Rational's
; (MS-DOS 6.2 Clone) Disk Operation System v1.0 Project
; TR-DOS Executer (MS-DOS Filesystem) Presentation 2000 Fixed Disk Boot Code
;
; Copyright (C) 2000 Erdogan TAN [ 14/03/2000 ] [ Modification: 18/04/2000 ]
;
; ****************************************************************************

trmagicword     equ    01A1h
notvalidfmask   equ    0018h
destination     equ    7E00h
bootstack       equ    7BFAh
sizeofdirentry  equ    0020h

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

; Boot Sector Parameters at 7C00h
DataArea1     equ -4
DataArea2     equ -2
BootStart     equ 0h
OemName       equ 03h
BytesPerSec   equ 0Bh
SecPerClust   equ 0Dh
ResSectors    equ 0Eh
FATs          equ 10h
RootDirEnts   equ 11h
Sectors       equ 13h
Media         equ 15h
FATsecs       equ 16h
SecPerTrack   equ 18h
Heads         equ 1Ah 
Hidden1       equ 1Ch
Hidden2       equ 1Eh
HugeSec1      equ 20h
HugeSec2      equ 22h
DriveNumber   equ 24h
Reserved1     equ 25h
bootsignature equ 26h                 
VolumeID      equ 27h
VolumeLabel   equ 2Bh
FileSysType   equ 36h          
Reserved2     equ 3Eh         ; Starting cluster of P2000

RxDOSBOOT       SEGMENT PUBLIC 'CODE'
                assume cs:RxDOSBOOT, ds:RxDOSBOOT, es:RxDOSBOOT, ss:RxDOSBOOT

                org 100h

        ;''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''';
        ;  Write Stub                                                   ;
        ;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -;
        ;                                                               ;
        ;  The stub loads at the normal 100h load address and writes    ;
        ;  the boot sector to drive C:                                  ;
        ;...............................................................;

RxDOS_WRITESTUB:
                cld
                push cs
                pop ss
                mov sp, 0FFFEh

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
                cmp al, 'C'
                jb short RxDOS_WRITESTUB_12
                cmp al, 'Z' + 1
                jb short RxDOS_WRITESTUB_10                    
                cmp al, 'c'                              
                jb short RxDOS_WRITESTUB_12                  
                cmp al, 'z' + 1                           
                jnb short RxDOS_WRITESTUB_12                 
RxDOS_WRITESTUB_10:
                sub al, 'c'-'C'                         ; to upper case
                mov byte ptr RxDOS_DRIVE, al
RxDOS_WRITESTUB_13:
                mov si, offset RxDOS_PressKeyWhenReady
                call proc_printmsg
RxDOS_WRITESTUB_11:
                xor ax, ax
                int 16h                                 ; wait for keyboard command
                push ax
                mov si, offset RxDOS_CRLF
                call proc_printmsg
                pop ax
                cmp al, 'M'-40h                         ; Enter (OK) key
                jz short RxDOS_WRITESTUB_20             ; write
                cmp al, 'C'-40h
                jz short return_from_trhdboot           ; no write (exit)
                cmp al, 27
                jz short return_from_trhdboot
                jmp short RxDOS_WRITESTUB_13            ; No write (exit)

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Write message
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

RxDOS_WRITESTUB_12:
                mov si, offset RxDOS_Welcome
                call proc_printmsg
                int 20h

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; get drive parameters
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

RxDOS_WRITESTUB_20:
                mov dl, byte ptr RxDOS_DRIVE            ; drive
                add dl, 80h                             
                sub dl, 'C'                             
                mov byte ptr [Hard_Disk], dl
                call proc_load_masterboot
                jc short return_from_trhdboot
                mov si, offset Boot_Sector_Buffer_1BEh
                mov cx, 4
		xor al, al
loc_scan_dos_p:
                inc al
                mov ah, byte ptr [SI][ptFileSystemName]
                cmp ah, 06h
                jz  short pass_scan_dos_partition
                cmp ah, 04h
                jz  short pass_scan_dos_partition
                cmp ah, 1                                 ; DOS FAT12
                jz  short pass_scan_dos_partition
                cmp ah, 0Eh                               ; WIN 95/98 FAT16
                jz  short pass_scan_dos_partition
                add si, 10h
                xor ah, ah
                loop loc_scan_dos_p
pass_scan_dos_partition:
                cmp ah, 0
                ja  short load_dos_partition
                mov si, offset Dos_P_Not_Found
                call proc_printmsg
return_from_trhdboot:
                int 20h

load_dos_partition:
                mov dl, byte ptr [Hard_Disk]
                mov bx, offset Boot_Sector_Buffer
                mov dh, byte ptr [SI] [ptBeginHead]
                mov byte ptr [varBeginHead], dh
                mov cx, word ptr [SI] [ptBeginSector]
                mov word ptr [varBeginSector], cx
                mov ax, 0201h
                int 13h
                jc  short RxDOS_WRITESTUB_30
                mov si, offset Boot_Sector_Buffer_1FEh
                cmp word ptr [SI], 0AA55h
                jz  short RxDOS_WRITESTUB_21
                mov si, offset Dos_B_Not_Found
                call proc_printmsg
                int 20h
RxDOS_WRITESTUB_21:
                mov cx,33h
                mov si, offset boot_sector_buffer_prms
                mov di, offset bsBytesPerSec
                rep movsb
                xor ax, ax
                int 1Ah                                 ; get time of day
                mov word ptr [bsVolumeID], dx
                mov word ptr [bsVolumeID+2], cx         ; set unique volume ID
                mov ax, 0301h                           ; write to disk
                mov bx, offset start                    ; location of boot code
                mov cx, word ptr [varBeginSector]
                mov dh, byte ptr [varBeginHead]
                mov dl, byte ptr [Hard_Disk] 
                int 13h
                jnc short RxDOS_WRITESTUB_36            ; if everything is ok -->
RxDOS_WRITESTUB_30:
                mov si, offset HdFailedMsg
                call proc_printmsg
                
                int 20h

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; success.
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

RxDOS_WRITESTUB_36:
                mov si, offset RxDOS_disk_WrittenSuccesfully
                call proc_printmsg
                int 20h

proc_printmsg   proc near

loc_print:          
		lodsb                           ; Load byte at DS:SI to AL
		and     AL,AL            
		je      short loc_return        ; If AL = 00h then return
		mov     AH,0Eh                  
		mov     BX,07h             
		int     10h                     ; BIOS Service func ( ah ) = 0Eh
						; Write char as TTY
						;AL-char BH-page BL-color
		jmp     short loc_print           
loc_return:
		retn

proc_printmsg   endp

proc_load_masterboot proc near

                mov dl, Byte ptr [Hard_Disk]
                xor ah,ah
                int 13h
                jnc short pass_hard_disk_error
hard_disk_error:
                mov si, offset HdFailedMsg
                call proc_printmsg
                stc
                retn
pass_hard_disk_error:
                mov bx, offset Boot_Sector_Buffer
                mov ax,0201h
                mov cx,1
                xor dh,dh
                int 13h
                jc short hard_disk_error
                mov word ptr [Boot_Sector_Buffer_1FEh], 0AA55h
                jne short loc_not_masterboot
                retn
loc_not_masterboot:
                mov si, offset Dos_P_Not_Found
                call proc_printmsg
                stc
                retn

proc_load_masterboot endp

Hard_Disk:      db 80h
varBeginHead:   db 0
varBeginSector: dw 0

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;  messages
;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

RxDOS_Welcome:
                db 0Dh, 0Ah
                db 'TR-DOS Fixed Disk Boot Sector Rebuilder v1.0'
                db 0Dh, 0Ah
                db '(c) Erdogan TAN 1998-2000'
                db 0dh,0Ah
                db 0Dh,0Ah
                db 'Usage: trhdboot [drive]'
RxDOS_CRLF:
                db 0Dh, 0Ah, 0
RxDOS_PressKeyWhenReady:
                db 0Dh, 0Ah
                db 'Press Enter to write boot sector on disk '
RxDOS_Drive:
                db 'C: ', 0
RxDOS_disk_WrittenSuccesfully:
                db 0Dh, 0Ah
                db 'Boot sector updated to TR-DOS X format...'
                db 0Dh, 0Ah
                db 0
HdFailedMsg:    db 0Dh,0Ah
                db "Disk error or drive not ready !"
                db 0Dh, 0Ah, 0h
Dos_P_Not_Found:
                db 0Dh, 0Ah
                db "Primary DOS partition not found !"
                db 0Dh, 0Ah, 0h
Dos_B_Not_Found:
                db 0Dh, 0Ah
                db "DOS boot sector not found !"
                db 0Dh, 0Ah, 0h
Boot_Sector_Buffer:
                db 11 dup(0)
Boot_Sector_Buffer_Prms:
                db 435 dup (0)
Boot_Sector_Buffer_1BEh:
                db 64 dup (0)
Boot_Sector_Buffer_1FEh:
                db 2 dup(0)

                org 7C00h
;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
;±
;±		PROCEDURE proc_start
;±
;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±

proc_start      proc    near

start:
                jmp     short loc_2
                nop

; BootSector Identification (Data) Block
bsOemName       db 'TR-DOS X'          
bsBytesPerSec   dw ?
bsSecPerClust   db ?
bsResSectors    dw ?
bsFATs          db ?
bsRootDirEnts   dw ?
bsSectors       dw ?
bsMedia         db ?
bsFATsecs       dw ?
bsSecPerTrack   dw ?
bsHeads         dw ?
bsHiddenSecs    label dword
bsHidden1       dw ?
bsHidden2       dw ?
bsHugeSectors   label dword
bsHugeSec1      dw ?
bsHugeSec2      dw ?
bsDriveNumber   db ?
bsReserved1     db ?
bsbootsignature db 29h                 
bsVolumeID      dd ?
bsVolumeLabel   db 11 dup(?)
bsFileSysType   db "FAT16   "          
bsReserved2     dw ?

loc_2:          
                xor     AX,AX            
                mov     DS,AX
                mov     ES,AX
                mov     SS,AX
                mov     SP,bootstack            ; 7BFAh
                mov     BP,7C00h                ; Bootsector location on memory
                mov     DL,byte ptr bsDriveNumber ; Fixed. 18/04/2000
                int     13h                     ; BIOS Service func ( ah ) = 0
                                                ; Reset disk system
                jc      short loc_ioerr         ; To print i/o error message
loc_3:                                          ; AX = 0 for AMIBIOS (no error)
                mov     AL,Byte Ptr bsFATs      ; BP+10h = Number of FATs
                mul     Word Ptr bsFATsecs      ; BP+16h = # of FAT sectors
                add     AX,Word Ptr bsHidden1   ; BP+1Ch = Hidden sectors LSW
                adc     DX,Word Ptr bsHidden2   ; BP+1Eh = Hidden sectors MSW
                add     AX,Word Ptr bsResSectors; BP+0Eh = Reserved sectors
                adc     DX,0                    ; ADD with carry
                push    AX                      ; LSW of root directory location
                push    DX                      ; MSW of root directory location
                mov     Word Ptr [BP-4],AX      ; LSW of Data Area = 7BFCh
                mov     Word Ptr [BP-2],DX      ; MSW of Data Area = 7BFEh
                mov     AX,sizeofdirentry       ; Size of a directory entry (32)
                mov     SI,Word Ptr bsRootDirEnts ; BP+11h = Root dir entries
		mul	SI			; DX_AX = AX * data
                mov     BX,Word Ptr bsBytesPerSec ; BP+0Bh = Bytes per sector
                add     AX,BX                   ; Round up
		dec	AX			
                div	BX			; AX=AX_DX/data DX=AX_DX%data
                add     Word Ptr [BP-4],AX      ; Location of the first data cluster
                adc     Word Ptr [BP-2],0       ; 
                pop     DX                      ; DX_AX = Location of root directory
		pop	AX			
loc_4:                                 
                mov     BX,destination          ; Destination offset = 7E00h
		mov	DI,BX			
                mov     CX,1                    ; Sector count for transfer
                call    proc_read  
loc_5:          
                jc      short loc_failed        ; Transaction failed message
loc_6:          
                cmp     Byte Ptr [DI],00h       ; Is it null entry?
                je      short loc_nofile        ; Jump if zero ( = )
                mov     CX,0Bh                  ; Size of file/directory name
                push    SI                      ; SI = number of the entries
                push    DI
                mov     SI,offset stdfilename   ; Offset of the std file name
                repe    cmpsb                   ; Repeat if ZF = 1, CX > 0
						; Cmp byte at DS:SI to ES:DI
                pop     DI
                pop	SI
                je      short loc_found         ; If the file name found
                dec     SI                      
                jz      short loc_nofile        ; Jump if no next entry
                add     DI,20h                  ; To next directory entry
                cmp     DI,BX                   ; Any more on current sector?
                jb      short loc_6             ; Jump if it is no last 
                jmp     short loc_4             ; Jump for next sector
loc_nofile:          
                mov     Byte Ptr notfoundmsg,20h
                mov     SI,offset stdfilename
                jmp     short loc_9
loc_boss:       
                mov     DI,trmagicword
                jmp     short loc_9
loc_ioerr:      
                mov     SI, offset IOerrormsg       
                jmp     short loc_9      
loc_failed:
                mov     SI,offset failmessage
loc_9:          
		lodsb				; Load byte at DS:SI to AL
                and     AL,AL            
                je      short loc_reboot        ; If AL = 00h then stop
		mov	AH,0Eh			
                mov     BX,07h             
		int	10h			; BIOS Service func ( ah ) = 0Eh
						; Write char as TTY
						;AL-char BH-page BL-color
                jmp     short loc_9           
loc_reboot:     
                mov     SI,offset rebootmsg
                cmp     DI,trmagicword          ; Replace/Reboot msg control 
                jne     loc_boss                ; Print Replace/Reboot message
                xor	AX,AX
		int	16h			; BIOS Service func ( ah ) = 0
						; Read next kbd char
						;AH-scan code AL-char code
		int	19h			; Reboot
infinite_loop:  
		jmp     short infinite_loop     ; Never come here !

loc_found:
                mov     AL,Byte Ptr [DI+0Bh]    ; Move attributes byte to BL
                and     AL,notvalidfmask        ; Is it a file, really?
                ja      short loc_nofile        ; Jump if above ( > )
                mov     AX,Word Ptr [DI+1Ah]    ; First cluster of the file
                cmp     AX,02h                  ; Start cluster
                jb      short loc_failed        ; If It is invalid ( < 2 )
                mov     Word Ptr bsReserved2,AX ; Save the first cluster
                                                ; for using by presentator
                dec     AX                      ; First cluster is cluster 2
                dec     AX               
                mov     CL,Byte Ptr bsSecPerClust
                mul     CX                      ; DX_AX = AX * data
		add	AX,Word Ptr [BP-4]	
		adc	DX,Word Ptr [BP-2]	; ADD with carry
                mov     BX,destination
                push    BX
                call    proc_read               ; CX = sectors per cluster
                pop     BX
                jb      short loc_ioerr         ; If CF = 1 then I/O err msg
                jmp     BX                      ; Jump to 7E00h                

proc_start	endp

proc_read       proc    near

loop_loc_14:            
                push    CX
                push    AX                      ; PHYSICAL ADRESS CALCULATION
                push    DX                      ; DX_AX = Linear address
                div     Word Ptr bsSecPerTrack  ; BP+18h = Sectors per track
                mov     CX,DX                   ; Sector (zero based)
                inc     CL                      ; To make it 1 based
                xor     DX,DX
                div     Word Ptr bsHeads        ; Convert track to head/cylinder
                                                ; AX = cyl, DX = head, CX = sector
                mov     DH,DL
                mov     DL,Byte Ptr bsDriveNumber
		mov	CH,AL			
		ror	AH,1			; Rotate right
                ror     AH,1                   
		or	CL,AH			
                mov     AX,0201h
		int	13h			; BIOS Service func ( ah ) = 2
						; Read disk sectors
						;AL-sec num CH-track CL-sec
						; DH-head DL-drive ES:BX-buffer
						;CF-flag AH-stat AL-sec read
                pop	DX
		pop	AX			
                pop     CX
                jc      short loc_16
                add     AX,01h
                adc     DX,0h    
                jc      short loc_16
loc_15:                 
                add     BX,Word Ptr bsBytesPerSec
                loop    loop_loc_14             ; Loop if CX > 0
loc_16:         
                retn

proc_read       endp

                dw      0

IOerrormsg      db      'Disk I/O error...',0
failmessage     db      'Transaction failed...',0
rebootmsg       db      0Dh, 0Ah
                db      'Replace the disk, and press any key to reboot...'
                db      0Dh, 0Ah,0
                db      0
stdfilename     db      'PRESENTATOR'
notfoundmsg     db      0, 'not found...', 0
version         db      'TR-DOS Boot v1.0', 0
                db      0
                db      '(c) Erdogan Tan [ 1998-2000 ]'
                db      0
bossmagic       db      01h
                db      0A1h
                db      1 dup(0)

                org     start+200h-2

bootsecsign     db      55h, 0AAh

RxDOSBOOT       ends

                end     RxDOS_WRITESTUB