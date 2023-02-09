;*****************************************************************************
; TRDOS2.ASM  [ Draft for Kernel TRDOS.COM ]
; (c) 2004-2011 Erdogan TAN  [ 17/01/2004 ]  Last Update: 09/11/2011
;
; 23/10/2011 device drivers (include DEV_INIT.ASM) phase 2
; 25/09/2011 end of trdos kernel v1.0 development phase 1 (2009-2011)
; 23/07/2011 ptFileSystemName -> ptFileSystemId
; 12/06/2011 (Memory init modification)
; 11/06/2011 (Boot drive checking modification)
; 21/02/2010 (Boot Drive checking modification), 03/01/2010
; CHS disk_io -> FAT/FS compatibility bugfix (DISK_IO.ASM -> proc_chs_read)
; FS logical disk structure has been changed to CHS/BPB compatible offsets 
; (Heads, SecPerTrack, BytesperStruct offsets -for FS- has been changed)
;    
; ! Under Development !

; Masterboot / Partition Table at Beginning+1BEh
ptBootable       equ 0
ptBeginHead      equ 1
ptBeginSector    equ 2
ptBeginCylinder  equ 3
ptFileSystemId   equ 4
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
Reserved2     equ 3Eh                           ; Starting cluster of P2000

; FAT32 BPB Structure
FAT32_FAT_Size equ 36
FAT32_RootFClust equ 44
FAT32_FSInfoSec equ 48
FAT32_DrvNum equ 64
FAT32_BootSig equ 66
FAT32_VolID equ 67
FAT32_VolLab equ 71
FAT32_FilSysType equ 82

; BIOS Disk Parameters
DPDiskNumber  equ 0h
DPDType       equ 1h
DPReturn      equ 2h
DPHeads       equ 3h
DPCylinders   equ 4h
DPSecPerTrack equ 6h
DPDisks       equ 7h
DPTableOff    equ 8h
DPTableSeg    equ 0Ah
DPNumOfSecs   equ 0Ch

; BIOS INT 13h Extensions (LBA extensions)
; Just After DP Data (DPDiskNumber+)
DAP_PacketSize equ 10h  ; If extensions present, this byte will be >=10h
DAP_Reserved1 equ 11h   ; Reserved Byte 
DAP_NumOfBlocks equ 12h ; Value of this byte must be 0 to 127
DAP_Reserved2 equ 13h   ; Reserved Byte
DAP_Destination equ 14h ; Address of Transfer Buffer as SEGMENT:OFFSET
DAP_LBA_Address equ 18h ; LBA=(C1*H0+H1)*S0+S1-1
                        ; C1= Selected Cylinder Number
                        ; H0= Number Of Heads (Maximum Head Number + 1)
                        ; H1= Selected Head Number
                        ; S0= Maximum Sector Number
                        ; S1= Selected Sector Number
                        ; QUAD WORD
; DAP_Flat_Destination equ 20h ; 64 bit address, if value in 4h is FFFF:FFFFh
                             ; QUAD WORD (Also, value in 0h must be 18h) 
                             ; TR-DOS will not use 64 bit Flat Address

; INT 13h Function 48h "Get Enhanced Disk Drive Parameters"
; Just After DP Data (DPDiskNumber+)
GetDParams_48h equ 20h ; Word. Data Length, must be 26 (1Ah) for short data.
GDP_48h_InfoFlag equ 22h ; Word
; Bit 1 = 1 -> The geometry returned in bytes 4-15 is valid.
GDP_48h_NumOfPCyls equ 24h ; Double Word. Number physical cylinders.
GDP_48h_NumOfPHeads equ 28h ; Double Word. Number of physical heads.
GDP_48h_NumOfPSpT equ 2Ch ; Double word. Num of physical sectors per track.
GDP_48h_LBA_Sectors equ 30h ; 8 bytes. Number of physical/LBA sectors.
GDP_48h_BytesPerSec equ 38h ; Word. Number of bytes in a sector.

; TR-DOS Standalone Program Extensions to the DiskParams Block
; Just After DP Data (DPDiskNumber+)
TRDP_CurrentSector equ 3Ah  ; DX:AX (LBA)
TRDP_SectorCount equ 3Eh    ; CX (or Counter)


; DOS Logical Disks
LD_Name equ 0
LD_DiskType equ 1
LD_PhyDrvNo equ 2
LD_FATType equ 3
LD_FSType equ 4
LD_LBAYes equ 5
LD_BPB equ 6
LD_FATBegin equ 96
LD_ROOTBegin equ 100
LD_DATABegin equ 104
LD_StartSector equ 108
LD_TotalSectors equ 112
LD_FreeSectors equ 116
LD_Clusters equ 120
LD_PartitionEntry equ 124
LD_DParamEntry equ 125
LD_MediaChanged equ 126
LD_CDirLevel equ 127
LD_CurrentDirectory equ 128

; Singlix FS Extensions to DOS Logical Disks
; 03/01/2010 (LD_BPB compatibility for CHS r/w)

LD_FS_Name equ 0
LD_FS_DiskType equ 1
LD_FS_PhyDrvNo equ 2
LD_FS_FATType equ 3
LD_FS_FSType equ 4
LD_FS_LBAYes equ 5
LD_FS_BPB equ 6
LD_FS_MediaAttrib equ 6
LD_FS_VersionMajor equ 7
LD_FS_RootDirD equ 8
LD_FS_MATLocation equ 12
LD_FS_Reserved1 equ 16 ;1 reserved byte
LD_FS_BytesPerSec equ 17 ; LD_BPB + 0Bh
LD_FS_Reserved2 equ 19 ;2 reserved byte
LD_FS_DATLocation equ 20
LD_FS_DATSectors equ 24
LD_FS_Reserved3 equ 28 ;3 reserved word
LD_FS_SecPerTrack equ 30 ; LD_BPB + 18h
LD_FS_NumHeads equ 32    ; LD_BPB + 1Ah
LD_FS_UnDelDirD equ 34
LD_FS_Reserved4 equ 38 ;4 reserved word
LD_FS_VolumeSerial equ 40
LD_FS_VolumeName equ 44
LD_FS_BeginSector equ 108
LD_FS_VolumeSize equ 112
LD_FS_FreeSectors equ 116
LD_FS_FirstFreeSector equ 120
LD_FS_PartitionEntry equ 124
LD_FS_DParamEntry equ 125
LD_FS_MediaChanged equ 126
LD_FS_CDirLevel equ 127
LD_FS_CDIR_Converted equ 128

; Valid FAT Types
FS_FAT12 equ 1
FS_FAT16_CHS equ 2
FS_FAT32_CHS equ 3
FS_FAT16_LBA equ 4
FS_FAT32_LBA equ 5

; Cursor Location
CCCpointer equ  0450h   ; BIOS data, current cursor column
; FAT Clusters EOC sign
FAT12EOC equ 0FFFh
FAT16EOC equ 0FFFFh
;FAT32EOC equ 0FFFFFFFh ; It is not direct usable for 8086 code
; BAD Cluster
FAT12BADC equ 0FF7h
FAT16BADC equ 0FFF7h
;FAT32BADC equ 0FFFFFF7h ; It is not direct usable for 8086 code
; MS-DOS FAT16 FS (Maximum Possible) Last Cluster Number= 0FFF6h 


Present segment Para 'code'

		assume CS:Present, DS:Present, ES:Present, SS:Present


;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
;±
;±              PROCEDURE proc_start
;±
;±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±

proc_start      proc    far

                org 100h
start:
                cmp ax, 417
                jne short pass_trdos_boot_sector
                mov byte ptr [BOOT_DISK], dl
pass_trdos_boot_sector:
                mov si, offset Starting_Msg
                call proc_printmsg

                call proc_memory_init ; MEM_INIT.ASM

  		mov bx, es
                sub bx, 10h
                mov es, bx  ; Move to 100h backward

                cli
                add ax, 100h
                mov ss, bx
                mov sp, ax  ; The last word of kernel/system stack
                sti

    		push es     		     ; ES:100h is beginning of the kernel
                mov si, offset retf_from_memory_init
                push si 

                mov ds, bx

                retf

retf_from_memory_init:
                call proc_drv_init  ; DRV_INIT.ASM
                ; 24/07/2011
                ;jnc short trdos_mainprog

                ;xor dl, dl
                ;push dl 
                ;call floppy_drv_init
                ;jnc short loc_set_current_floppy_drv 

  	        ;mov dl, 1
                ;call floppy_drv_init
                ;jc short trdos_drv_not_ready_error

;loc_set_current_floppy_drv:
               ;mov dl, bh
               ;jmp short trdos_mainprog_current_drive_a

trdos_mainprog:
               ; When floppy_drv_init call is disabled
               ; media changed sign is needed
               ; for proper drive initialization
               ; 12-9-2009

                mov si, offset Logical_DOSDisks
                mov al, 1 ; Initialization sign (invalid_fd_parameter)
                add si, LD_MediaChanged  ; Media Change Status = 1 (init needed)
                mov byte ptr [SI], al
                add si, 100h
                mov byte ptr [SI], al
           
               ; gecici
               ; mov si, offset CDrive_Str
               ; call proc_printmsg
                
               ; xor ah, ah
               ; int 16h

               ; mov dl, al
               ; and dl, 0DFh
                
               ; mov al, dl 
               ; sub dl, 'A'
               ; jc short trdos_mainprog_current_drive_bootdisk
                
               ; mov ah, 0Eh
               ; mov bx, 07h
               ; int 10h

               ; xor ah, ah
               ; int 16h 
                     
               ; jmp short trdos_mainprog_current_drive_a
               
               ; call proc_clear_screen

trdos_mainprog_current_drive_bootdisk:
                mov dl, byte ptr [Boot_Disk]
                ;11/06/2011 
                cmp dl, 0FFh
                je short trdos_mainprog_last_dos_diskno_check
trdos_mainprog_boot_drive_check:
               ; 11/06/2011
                cmp dl, 80h
                jb short trdos_mainprog_current_drive_a
                sub dl, 7Eh ; C = 2 , D = 3
                jmp short trdos_mainprog_current_drive_a 

trdos_mainprog_last_dos_diskno_check:
                mov dl, byte ptr [Last_Dos_DiskNo]
                cmp dl, 2
                ja short trdos_mainprog_current_drive_c
                je short trdos_mainprog_current_drive_a
                xor dl, dl ; A:
                jmp short trdos_mainprog_current_drive_a

trdos_mainprog_current_drive_c:
                ; 21/02/2010
                mov dl, 2 ; C:

trdos_mainprog_current_drive_a:
               ;call proc_clear_screen
               ;push dx
                mov si, offset msg_CRLF_temp
                call proc_printmsg
               ;pop dx
                call proc_change_current_drive
                jnc short pass_drv_not_ready_error

trdos_drv_not_ready_error: 
                mov si, msgl_drv_not_ready
                call proc_printmsg
                ;24/07/2011
	        jmp short end_of_program
                ; 2-1-2010
                ;jmp short pass_drv_not_ready_error

msgl_drv_not_ready: 
                db 07h, 0Dh, 0Ah
                db 'Drive not ready or read error !!'
                db 0Dh, 0Ah, 0

pass_drv_not_ready_error:
               ; 17/09/2011
                call mainprog_startup_configuration
               ; 
                call proc_dos_prompt
              
end_of_program:
               ;int 20h
                mov si, offset Msg_CRLF_temp
                call proc_printmsg
                mov si, Program_Version
                call proc_printmsg
                xor ah, ah
                int 16h
                int 19h

infinitiveloop: jmp short infinitiveloop

;CDrive_Str:     db "DOS Drive : " , 0
msg_CRLF_temp:
                db 7, 0Dh, 0Ah, 0

proc_start      endp

proc_printmsg   proc near

               ; mov AH, 0Fh ; Get Video Display Mode
               ; int 10h
                ;AH= Number of columns
                ;AL= Video mode
                ;BH= Current video page
              ;  mov BL, byte ptr [Program_CharColorAtr]
                 mov BX, 07h
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

Program_CharColorAtr: db 0

proc_printmsg   endp

proc_clear_screen proc near

               ;21/09/2009
               mov ax, 0600h
               mov bh, 7
               xor cx, cx
               mov dx, 184Fh
               int 10h
               mov ah, 2
               xor bh, bh
               xor dx, dx
               int 10h

               ;2004-2005
               ;mov ah, 0Fh 
               ;int 10h
               ;mov ah, 0
               ;int 10h

                retn

proc_clear_screen endp

;'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''';
; Rx_DOS  32 bit Divide                                      ;
; (Special version by Erdogan Tan)                           ;
;- - - - - - - - - - - - - - - - - - - - - - - - - -- - - - -;
;                                                            ;
; input -> DX_AX = 32 bit dividend                           ;
; input -> CX = 16 bit divisor                               ;
; output -> DX_AX = 32 bit quotient                          ;
; output -> BX = 16 bit remainder                            ;
;                                                            ;
;  This procedure divides the requested 32 bit number        ;
;  and gives the result in DX, AX and BX (remainder)         ;
;                                                            ;
; Original Procedure by Michael Podanoffsky / Real Time DOS  ;
; (c) Erdogan TAN  1999                     [ RXDOSBIO.ASM ] ;
;............................................................;

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

		retn

Rx_Dos_Div32    endp

;'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''';
; From binary (byte) to hexadecimal (character) converter    ;
;                                                            ;
; input -> AL = byte (binary number) to be converted         ;
; output -> AH = First character of hexadecimal number       ;
; output -> AL = Second character of hexadecimal number      ;
;                                                            ;
; (c) Erdogan TAN  1998 - 1999                               ;
;............................................................;

; 1998

proc_hex        proc    near

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

proc_hex        endp

;'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''';
; 32 bit Multiply                                            ;
;- - - - - - - - - - - - - - - - - - - - - - - - - -- - - - -;
;                                                            ;
; input -> DX_AX = 32 bit multiplier                         ;
; input -> BX = 16 bit number to be multiplied by DX_AX      ;
; output -> BX_DX_AX = 48 bit (16+32 bit) result number      ;
;                                                            ;
; (c) Erdogan TAN  1999                                      ;
;............................................................;

proc_mul32    proc near

	      push cx

	      mov cx, bx
	      mov bx, dx

	      mul cx

              xchg ax, bx

	      push dx

              mul cx 

              pop cx 

              add ax, cx 
	      adc dx, 0

	      xchg bx, ax
	      xchg dx, bx

	      pop cx

	      retn

proc_mul32    endp

INCLUDE MEM_INIT.ASM   ; TRDOS Memory Initialization Procedures
                       ; 04/09/2011
INCLUDE DRV_INIT.ASM   ; TRDOS Drive Initialization Procedures
                       ; 07/08/2011  
INCLUDE DISK_IO.ASM    ; TRDOS Disk Read/Write Procedures
                       ; 20/07/2011
INCLUDE DRV_FAT.ASM    ; TRDOS FAT 12-16-32 FS Procedures
                       ; 21/08/2011
INCLUDE DRV_FS.ASM     ; TRDOS FS File System Compatibility Procedures
                       ; 24/09/2011 
INCLUDE MAINPROG.ASM   ; TRDOS MAIN PROGRAM (Command Executer)
                       ; 09/11/2011
INCLUDE DIR.ASM        ; TRDOS Directory Handling Procedures, 24/09/2009
                       ; 09/10/2011
INCLUDE FILE.ASM       ; TRDOS File Handling Procedures, 04/11/2009
		       ; 09/10/2011 
INCLUDE CMD_INTR.ASM   ; TRDOS COMMAND INTERPRETER
		       ; 09/11/2011	
INCLUDE INT_21H.ASM    ; TRDOS INT 21h Functions
                       ; 08/11/2011
INCLUDE IFC.ASM        ; TRDOS Invalid Function Call Handler
		       ; 17/04/2011	
INCLUDE DEV_INIT.ASM   ; TRDOS Device Driver Procedures, 23/10/2011
                       ; 07/11/2011

Starting_Msg:
                db 0Dh, 0Ah 
                db "Starting TRDOS ..."
NextLine:       db 0Dh, 0Ah, 0

BOOT_DISK: db 0FFh

Logical_DOSDisks:  dw 3328 dup(0)

KERNEL_FILESIZE equ $-100h

Present            ends

                   end start
