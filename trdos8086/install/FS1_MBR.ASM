; SINGLIX FS1 MASTER BOOT RECORD  (c) Erdogan Tan [26/04/2009]
;
; Derivation from WINDOWS XP MASTER BOOT RECORD
;
; Modified for SINGLIX Operating System v1.0b and SINGLIX FS1 File System v1.0
; ! WINDOWs XP compatible MBR code !
;
; Original code by (c) Microsoft Corporation 2000 [ Windows XP SP2 2004 ]
; DMADMIN.EXE (4 August 2004) offset 216616 (512 byte) (XP_MASTERBOOT.BIN)
; WINDOWS XP/SP2 Turkish OS Master Boot code (masterboot.bin)
; Singlix Code Grabber v2.0 [binfilehex.exe] XP_MASTERBOOT.BIN, masterboot.bin

; Microsoft Macro Assembler 
; compatible assembly source code 
; for SINGLIX FS1 MASTER BOOT CODE (c) Erdogan Tan 2009
;
; Disassembled by Erdogan Tan [26/04/2009] as Microsoft MASM compatible code
; by using DASM.EXE [24/3/1993] and PV.EXE [22/1/1993]
;
; Some explanations from 
;"Win2k Master Boot Record (MBR) Revealed!" html page by (c) Daniel B. Sedory
; http://www.geocities.com/thestarman3/asm/mbr/Win2kmbr.htm 
; http://mirror.href.com/thestarman/asm/mbr/Win2kmbr.htm

;#############################################################################

MBR	        segment para public
		assume  CS:MBR, DS:MBR, SS:MBR, ES:MBR

;#############################################################################
;#
;#		ENTRY POINT
;#
;#############################################################################

                org   600h                     ; Final memory location 0:600h

;#############################################################################
;#
;#		PROCEDURE proc_start
;#
;#############################################################################

proc_start	proc	far

start:		
		xor	AX,AX			; db 33h, 0C0h
		mov	SS,AX			
		mov	SP,7C00h		
		sti				; Enable interrupts
		push	AX			
		pop	ES			
		push	AX			
		pop	DS			
		cld				; Clear direction flag
		mov	SI,7C1Bh		; offset 01Bh (label loc_01Bh)
		mov	DI,61Bh			
		push	AX			
		push	DI			
		mov	CX,1E5h			
		repz	movsb			; Repeat if ZF = 1, CX > 0
						; Move byte from DS:SI to ES:DI
		retf				; Return FAR

proc_start	endp

loc_01Bh:       mov    BP, 7BEh                 ; db 0BDh, 0BEh, 07h 
                mov    CL, 4                    ; db 0B1h, 04h
loc_020h:	
		cmp	Byte Ptr [BP+0],CH	
		jl	short loc_02Eh		; Jump if less ( < )
		jne	short loc_03Ah		; Jump if not equal ( != )
		add	BP,+10h			
		loop	loc_020h		; Loop if CX > 0

                ; Beginning of SINGLIX MBR code extension
		; SINGLIX MBR feature : Check Sector 1 for a valid boot record
                mov	BX,7C00h
                mov     CX,2
                mov 	DX,80h
loc_read_bootsector_location:
                mov	AX,201h			
		
		int	13h			; BIOS Service func ( ah ) = 2
						; Read disk sectors
						;IN: AL-sec num CH-track CL-sec
						; DH-head DL-drive ES:BX-buffer
						;OUT: CF-flag AH-stat AL-sec read
		jc	short loc_check_bootsector_number
                
loc_check_bootsector_sign:
                ;cmp     Word Ptr [7DFEh],0AA55h ; Check for the Magic Word/Number
                cmp     Word Ptr [data_7FEh+7600h],0AA55h
                je	short loc_094h
loc_check_bootsector_number:		
                cmp     CX,2
                jne     short loc_int18h
                ; SINGLIX MBR feature : Check Sector 63 for a valid boot record
                inc     DH      ; mov dh, 1 	                
                mov     CL, DH  ; mov cl, 1
                jmp     short loc_read_bootsector_location
		; End of SINGLIX MBR code extension

loc_int18h:                
		int	18h			; Switch to BASIC
loc_02Eh:		
		mov	SI,BP			
loc_030h:		
		add	SI,+10h			
		dec	CX			
		je	short loc_04Fh		; Jump if equal ( = )
		cmp	Byte Ptr [SI],CH	
		je	short loc_030h		
loc_03Ah:		
; SINGLIX MBR MODIFICATION
;                mov     AL,Byte Ptr [msg_ptr_ipt]

                		                ; Invalid partition table
                mov     SI, offset msg_ipt
loc_print_msg:
; SINGLIX MBR MODIFICATION
;loc_03Dh:		                        
;		mov	AH,7			
;		mov	SI,AX			
loc_041h:		
		lodsb				; Load byte at DS:SI to AL
loc_042h:		
		cmp	AL,0			
		je	short loc_042h		
		mov	BX,7			
		mov	AH,0Eh			
		int	10h			; BIOS Service func ( ah ) = 0Eh
						; Write char as TTY
						;IN: AL-char BH-page BL-color
		jmp	short loc_041h		
loc_04Fh:		
		mov	Byte Ptr [BP+10h],CL	
		call	near ptr proc_load_bootsector ; offset 09Bh		
		jnc	short loc_081h		
loc_057h:		
		inc	Byte Ptr [BP+10h]	
		cmp	Byte Ptr [BP+4],0Bh	
		je	short loc_06Bh		
		cmp	Byte Ptr [BP+4],0Ch	
		je	short loc_06Bh		
; SINGLIX MBR MODIFICATION
;                mov     AL,Byte Ptr [msg_ptr_elos]
                                                ; Error loading operating system
                mov     SI, offset msg_elos 
                jne     short loc_print_msg     ; Jump if not equal ( != )
loc_06Bh:		
		add	Byte Ptr [BP+2],6	
		add	Word Ptr [BP+8],+6	
		adc	Word Ptr [BP+0Ah],+0	; ADD with carry
		call	near ptr proc_load_bootsector		
		jnc	short loc_081h			
; SINGLIX MBR MODIFICATION
;                mov     AL,Byte Ptr [msg_ptr_elos]
                                                ; Error loading operating system
                mov     SI, offset msg_elos  
                jmp     short loc_print_msg     ; loc_03Dh       
loc_081h:		
	       ;cmp     Word Ptr [7DFEh],0AA55h ; Check for the Magic Word/Number
                cmp     Word Ptr [data_7FEh+7600h],0AA55h
                je	short loc_094h		
		cmp	Byte Ptr [BP+10h],0	
		je	short loc_057h
; SINGLIX MBR MODIFICATION		
;		 mov     AL,Byte Ptr [msg_ptr_mos]
                                                ; Missing operating system
                mov     SI, offset msg_mos
                jmp     short loc_print_msg      
loc_094h:		
                ; Beginning of SINGLIX MBR code extension
                ; SINGLIX MBR Feature/Sign 
                mov     AX, word ptr [singlix_mbr_sign] ; 417 
		; End of SINGLIX MBR code extension
                mov	DI,SP			
		push	DS			
		push	DI			
		mov	SI,BP			
		retf				; Return FAR
                                                ; jump to the Boot Sector code

;#############################################################################
;#
;#		PROCEDURE proc_load_bootsector
;#
;#############################################################################

; Original WINDOWS XP MBR Boot Sector Reading/Loading PROCEDURE
; by Microsoft Corporation

proc_load_bootsector	proc	near

loc_09Bh:
		mov	DI,5			
		mov	DL,Byte Ptr [BP+0]	
		mov	AH,8			
		int	13h			; BIOS Service func ( ah ) = 8
						; Get current drive parameters
						; OUT: DL-drvs DH-sides CL-sects
						; CH-tracks CF-flag AH-stat
		jc	short loc_0CAh		
		mov	AL,CL			
		and	AL,3Fh			
		cbw				; Convert byte to word
		mov	BL,DH			
		mov	BH,AH			
		inc	BX			
		mul	BX			; DX_AX = AX * data
		mov	DX,CX			
		xchg	DL,DH			
		mov	CL,6			
		shr	DH,CL			; Shift right ( fill zeros )
		inc	DX			
		mul	DX			; DX_AX = AX * data
		cmp	Word Ptr [BP+0Ah],DX	
		jnbe	short loc_0E6h		
		jb	short loc_0CAh		
		cmp	Word Ptr [BP+8],AX	
		jnb	short loc_0E6h		
loc_0CAh:		
		mov	AX,201h			
		mov	BX,7C00h		
		mov	CX,Word Ptr [BP+2]	
		mov	DX,Word Ptr [BP+0]	
		int	13h			; BIOS Service func ( ah ) = 2
						; Read disk sectors
						;IN: AL-sec num CH-track CL-sec
						; DH-head DL-drive ES:BX-buffer
						;OUT: CF-flag AH-stat AL-sec read
		jnc	short loc_12Bh		
		dec	DI			
		jz	short loc_12Bh		
		
                xor	AH,AH			
		mov	DL,Byte Ptr [BP+0]	
		int	13h			; BIOS Service func ( ah ) = 0
						; Reset disk system
		jmp	short loc_0CAh		
loc_check_int13h_extensions:
loc_0E6h:		
		mov	DL,Byte Ptr [BP+0]
               ;pusha                           ; db 60h
                db 60h
                mov     BX, 55AAh               ; db 0BBh, 0AAh, 55h
                mov     AH, 41h                 ; db 0B4h, 41h
                int     13h                     ; db 0CDh, 13h  
                jc      short loc_129h   
		
                cmp     BX, 0AA55h              ; Extensions present
                jne     short loc_129h
                test    CL, 1                   ; Fixed disk access subset check
                jz      short loc_129h                   
		
               ;popa                            ; db 61h
                db 61h
loc_0FFh:
               ;pusha                           ; db 60h
                db 60h

               ;push    0                       ; db 6Ah, 00h
                db 6Ah, 0
               ;push    0                       ; db 6Ah, 00h
                db 6Ah, 0
                push 	Word Ptr [BP+0Ah]       ; db 0FFh, 76h, 0Ah
                push 	Word Ptr [BP+08h]       ; db 0FFh, 76h, 08h
               ;push    0                       ; db 6Ah, 00h 
                db 6Ah, 0
               ;push    7C00h                   ; db 68h, 00h, 7Ch
                db 68h, 00h, 7Ch
               ;push    1                       ; db 6Ah, 01h
                db 6Ah, 01h
               ;push    10h                     ; db 6Ah, 10h
                db 6Ah, 10h

                mov 	AH, 42h
                mov 	SI, SP
                int 	13h

               ;popa
                db 61h
               ;popa
                db 61h

                jnc     short loc_12Bh
                
                dec     DI 

                jz      short loc_12Bh
                
                xor     AH, AH
                mov     DL, Byte Ptr [BP+0]
                int     13h
                
                jmp     short loc_0FFh                  
 
loc_129h:
               ;popa                            ; db 61h
                db 61h

                stc                             ; db 0F9h
loc_12Bh:
                retn                            ; db 0C3h
			
proc_load_bootsector	endp
                
; SINGLIX MBR MODIFICATION
singlix_mbr_sign:
                dw     01A1h	; SINGLIX MBR sign
                                ; Also, pt_pointer location
              ; org    72Ch
;; Turkish Windows XP MBR Error messages
; English Windows XP MBR Error messages
msg_ipt:       ;db      'Geáersiz bîlÅm tablosu'
		db 	'Invalid partition table'
		db	0
msg_elos:      ;db      'òületim sistemi yÅkleme hatasç'
                db	'Error loading operating system'
	        db	0
msg_mos:
               ;db      'òületim sistemi yok'
	        db 	'Missing operating system' 
          	db	0
;; End of Turkish Windows XP MBR error messages
; End of English Windows XP MBR error messages
                
                db 1                    ; SINGLIX MBR version

pt_pointer:                             ; Offset 417 (01A1h)
  		dw      07BEh		; Partition Table Location

                db 	1 dup (0)

                org     7B5h
;err_msg_pointers:
;msg_ptr_ipt:    db      2Ch ; offset msg_ipt - 700h   XP MBR
;msg_ptr_elos:   db      44h ; offset msg_elos - 700h  XP MBR
;msg_ptr_mos:    db      63h ; offset msg_mos - 700h   XP MBR
msg_ptr_ipt:    db      50h ; offset msg_ipt - 700h   SINGLIX/XP MBR
msg_ptr_elos:   db      68h ; offset msg_elos - 700h  SINGLIX/XP MBR
msg_ptr_mos:    db      87h ; offset msg_mos - 700h   SINGLIX/XP MBR
          
                org     7BEh
partition_table:
                db      64 dup(0)

                org     7FEh
               ;org     7DFEh
bs_sign:
data_7FEh:      db      55h
                db      0AAh      

MBR	ends

                end     start       
