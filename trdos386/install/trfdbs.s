; ****************************************************************************
; TRDOS386.ASM (TRDOS 386 Kernel) - v2.0.1 - trfdbs.s - FAT12 FD BOOT SECTOR
; ----------------------------------------------------------------------------
; Last Update: 12/02/2018
; ----------------------------------------------------------------------------
; Beginning: 25/01/2016
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.11 (trfdboot.s)
; ----------------------------------------------------------------------------
; Turkish Rational DOS
; Operating System Project v2.0 by ERDOGAN TAN (Beginning: 04/01/2016)
;
; Derived from TRDOS Operating System v1.0 (8086) source code by Erdogan Tan
; TRFDBOOT.ASM (31/07/2011)
; ****************************************************************************

notvalidfmask   equ	0018h

root_dir_buff   equ	0700h
rts_segment     equ	1000h
FAT_Buffer      equ	0700h

[BITS 16]
[ORG 7C00h]

                jmp     short BS_01
                nop

; BootSector Identification (Data) Block

bsOemName:      db 'TRDOS2.0'          
bsBytesPerSec:  dw 512
bsSecPerClust:  db 1
bsResSectors:   dw 1
bsFATs:         db 2
bsRootDirEnts:  dw 224
bsSectors:      dw 2880
bsMedia:        db 0F0h
bsFATsecs:      dw 9
bsSecPerTrack:  dw 18
bsHeads:        dw 2
bsHidden1:      dw 0
bsHidden2:      dw 0
bsHugeSectors:  dd 2880
bsDriveNumber:  db 0
bsReserved1:    db 0
bsBpbSignature: db 29h                 
bsVolumeID:     dd 0
bsVolumeLabel:  db 'TRDOS      '
bsFileSysType:  db 'FAT12   '          
bsReserved2:    dw 'RT'
; TRDOS 386 v2.0 2018 Extensions
bsDataStart:    dw 33
bsRootDirStart:	dw 19
bsRootDirSects:	dw 14          

BS_01:          
                mov	ax, cs
		mov	ds, ax
		mov	es, ax
		
		cli
		mov	ss, ax
		mov     sp, 0FFFEh
		sti

		; overwrite fd drive number !
                mov	[bsDriveNumber], dl 	; drive number from INT 19h
						; (DL = 0)

                ;mov	ax, 19                  ; Root Dir Location
		mov	ax, [bsRootDirStart]
		;mov	cx, 14                  ; Root Directory Sectors
                mov	cx, [bsRootDirSects]
		mov     bx, root_dir_buff       ; Destination offset = 700h
                mov     si, bx
                call    fd_read
		jc      short BS_04		; Disk read error message

                ;mov	bx, 224                 ; Number of root dir entries
		mov	bx, [bsRootDirEnts]
BS_02:          
		cmp     byte [si], 0		; Is it null entry?
                je      short BS_03		; Jump if zero ( = )
                mov     cx, 11			; Size of file/directory name
                push    si
                mov     di, rtsfilename   
                repe    cmpsb                   ; Repeat if ZF = 1, CX > 0
						; Cmp byte DS:SI with ES:DI
                pop	si
                je      short BS_06		; If the file name found
                dec     bx                    
                jz      short BS_03		; Jump if no next entry
                add     si, 32                  ; To next directory entry
                jmp     short BS_02             ; Jump for next sector
BS_03:
                mov     si, Replace_Msg
                jmp     short BS_05
BS_04:      
                mov     si, Error_Msg
BS_05:          
                call    print_msg

                xor	ax, ax
		int	16h			; BIOS Service func ( ah ) = 0
						; Read next kbd char
						; AH-scan code AL-char code
		int	19h			; Reboot

BS_06:
                mov     al, [si+0Bh]		; Move attributes byte to BL
                and     al, notvalidfmask       ; Is it a file, really?
                jnz     short BS_03		; Jump if not
                mov     ax, [si+1Ah]		; First cluster of the file
                cmp     ax, 2                   ; Start cluster
                jb      short BS_04

                mov	[bsReserved2], ax	; Save the first cluster
    
		; Load FAT
                ;mov	ax, 1                   ; FAT Location
                mov	ax, [bsResSectors]
		;mov	cx, 9                   ; FAT Sectors
                mov	cx, [bsFATsecs]
		mov     bx, FAT_Buffer    
                call    fd_read
                jc      short BS_04

		; Load  RTS (Kernel) file
                mov     si, Loading_Msg
                call    print_msg
                mov     ax, [bsReserved2]	; The First cluster
                mov     bx, rts_segment
                mov     es, bx
                xor     bx, bx
                call    load_file
                jc      short BS_04

		; Launch RTS (Kernel)
   		mov	ax, es
		;cli
                mov	ss, ax
                ;mov	sp, 0FFFEh
                ;sti
                ;mov	dl, [bsDriveNumber]
                mov	ax, rts_segment ; 1000h
                mov	ds, ax
                mov	es, ax
             
		mov 	bp, 7C00h

		mov	ax, 417

		jmp	rts_segment:0

                ;db	0EAh
                ;dw	0
                ;dw	rts_segment

print_msg:
BS_07:
		lodsb				; Load byte at DS:SI to AL
                and     al, al            
                jz	short BS_09		; If AL = 00h then stop

                mov     ah, 0Eh
                mov     bx, 07h             
		int	10h			; BIOS Service func ( ah ) = 0Eh
						; Write char as TTY
						; AL-char BH-page BL-color
                jmp     short BS_07

BS_08:         
                dec	byte [RetryCount]
		jz	short BS_09 ; cf = 1

		; Reset disk system
		push	ax
		xor	ah, ah
                ;mov	dl, [bsDriveNumber]
		int     13h
                pop	ax
		jnc	short BS_10
BS_09:
		retn
 
fd_read:
                ; Only for FAT12 Floppy Disks
                
                mov     byte [RetryCount], 4
BS_10:
                push    cx
                push    ax                      ; PHYSICAL ADRESS CALCULATION
                ;mov	cl, 18                  ; Sectors per track
                mov	cl, [bsSecPerTrack]
		div     cl                      
                mov     cl, ah                  ; Sector (zero based)
                inc     cl                      ; To make it 1 based
                ;xor	ah, ah
                ;mov	dl, 2			; Heads 
                ;div	dl
                                                ; AL=cyl, AH=head, CL=sector
                ;mov	dh, ah
		sub	dh, dh
		shr	al, 1
		adc	dh, 0
		;mov	dl, [bsDriveNumber]	; (!DL has not been changed!)
                mov     ch, al            

                mov     ax, 0201h
		int	13h			; BIOS Service func ( ah ) = 2
						; Read disk sectors
						; AL-sec num CH-track CL-sec
						; DH-head DL-drive ES:BX-buffer
						; CF-flag AH-stat AL-sec read
		pop	ax			
                pop     cx
                jc      short BS_08
                inc     ax
	                
                ;add	bx, 512
	        add	bh, 2
		jnc	short BS_11
		push	bx
		mov	bx, es
		;add	bx, 1000h
		add	bh, 10h
		mov	es, bx
		pop	bx
BS_11:
                ;dec	cx
                dec	cl
		jnz	short fd_read
		retn

load_file:
              ; ES:BX = File Buffer
              ; AX = First Cluster Number
              ; Only for FAT12 Floppy Disks
BS_12:
                mov     [File_Cluster], ax
                dec     ax                      ; First cluster is cluster 2
                dec     ax
		               
                ;mov	cx, 1                   ; Sector count
                
		mov	cl, [bsSecPerClust]
		; ch = 0
		test	cl, 2
		jz	short BS_13
		shl	ax, 1

		;add	ax, 33                  ; Beginning sector of Data
BS_13:
                add	ax, [bsDataStart]
		call    fd_read
                jc      short BS_15
get_next_cluster:
		push	bx
                mov     ax, [File_Cluster]
                mov     bx, ax
		; Multiply by 3
		;mov	dx, 3
		;mul	dx
               	add	ax, ax
		add	ax, bx
                shr     ax, 1 ; Divide by 2
                xchg    bx, ax
                ; BX = Buffer Byte Offset
                ; AX = Current Cluster
                mov     ax, [FAT_Buffer+bx]
                jnc     short BS_16
                shr     ax, 4
BS_14:
		pop	bx
                cmp     ax, 0FF7h
		jb	short BS_12
		; EOCC (kernel file has been loaded successfully)
BS_15:
                retn
BS_16:
                and     ah, 0Fh
                jmp     short BS_14

Reserved3:	db	20h
RetryCount:     db      18h
File_Cluster:   dw	0

rtsfilename:
                db      'TRDOS386SYS'
                db      0
Error_Msg:
                db      0Dh, 0Ah
                db      'TRDOS Kernel Loading Error!'

Replace_Msg:    db      0Dh, 0Ah
                db      'Replace the disk and press any key to reboot.'
                db      0Dh, 0Ah,0

Loading_Msg:    db      0Dh, 0Ah
                db      "Loading Kernel TRDOS386.SYS ..."
                db      0Dh, 0Ah, 0

		times	510 - ($ - $$) db 0

bootsignature:  db      55h, 0AAh