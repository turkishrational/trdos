; ****************************************************************************
; TRDOS386.ASM (TRDOS 386 Kernel - v2.0.8) - INITIALIZED DATA : trdosk9.s
; ----------------------------------------------------------------------------
; Last Update: 23/06/2024 (Previous: 07/12/2023 - Kernel v2.0.7)
; ----------------------------------------------------------------------------
; Beginning: 04/01/2016
; ----------------------------------------------------------------------------
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.15 (trdos386.s)
; ----------------------------------------------------------------------------
; Derived from TRDOS Operating System v1.0 (8086) source code by Erdogan Tan
; TRDOS2.ASM (09/11/2011)
; ****************************************************************************
; DRV_INIT.ASM [26/09/2009] Last Update: 07/08/2011
; MAINPROG.ASM [17/01/2004] Last Update: 09/11/2011
; CMD_INTR.ASM [29/01/2005] Last Update: 09/11/2011
; FILE.ASM [29/10/2009] Last Update: 09/10/2011

; 12/02/2016
Last_DOS_DiskNo: 
		db 1 ; A: = 0 & B: = 1

Restore_CDIR:	
		db 0FFh ; Initial value -> any number except 0

msg_CRLF_temp:  
		db 07h, 0Dh, 0Ah, 0

Magic_Bytes:
		db 4
		db 1
mainprog_Version:
		db 7
		db "[TRDOS] Main Program v2.0.8 (23/06/2024)"
		db 0Dh, 0Ah
		db "(c) Erdogan Tan 2005-2024"
		db 0Dh, 0Ah, 0

MainProgCfgFile: ; 14/04/2016
		db "MAINPROG.CFG", 0

TRDOSPromptLabel:
		db "TRDOS"
		db 0
                times 5 db 0
		db 0

; INTERNAL COMMANDS
Command_List:
Cmd_Dir:	db "DIR", 0
Cmd_Cd:		db "CD", 0
Cmd_Drive:	db "C:", 0
Cmd_Ver:	db "VER", 0
Cmd_Exit:	db "EXIT", 0
Cmd_Prompt:	db "PROMPT", 0
Cmd_Volume:	db "VOLUME", 0
Cmd_LongName:	db "LONGNAME", 0
Cmd_Date:	db "DATE", 0
Cmd_Time:	db "TIME", 0
Cmd_Run:	db "RUN", 0
Cmd_Set:	db "SET", 0 
Cmd_Cls:	db "CLS", 0
Cmd_Show:	db "SHOW", 0
Cmd_Del:	db "DEL", 0
Cmd_Attrib:	db "ATTRIB", 0
Cmd_Rename:	db "RENAME", 0
Cmd_Rmdir:	db "RMDIR", 0
Cmd_Mkdir:	db "MKDIR", 0
Cmd_Copy:	db "COPY", 0
Cmd_Move:	db "MOVE", 0
Cmd_Path:	db "PATH", 0
Cmd_Mem:	db "MEM", 0
		db 0
Cmd_Find:	db "FIND", 0
Cmd_Echo:	db "ECHO", 0
Cmd_Remark:	db "*", 0
Cmd_Help:	db "?", 0
Cmd_Device:	db "DEVICE", 0
Cmd_DevList:	db "DEVLIST", 0
Cmd_Chdir:	db "CHDIR", 0
Cmd_Beep:	db "BEEP", 0
		
		db 0

; 15/02/2016 (FILE.ASM, 09/10/2011)
invalid_fname_chars:
		db 22h, 27h, 28h, 29h, 2Ah, 2Bh, 2Ch, 2Fh
		db 3Ah, 3Bh, 3Ch, 3Dh, 3Eh, 3Fh, 40h
		db 5Bh, 5Ch, 5Dh, 5Eh, 60h
sizeInvFnChars  equ ($ - invalid_fname_chars)                
;

Msg_Enter_Date:
                db 'Enter new date (dd-mm-yy): '
                db 0
Msg_Show_Date:
                db   'Current date is '
Day:            db   '0'
		db   '0'
                db   '/'
Month:          db   '0'
		db   '0'
                db   '/'
Century:        db   '0'
                db   '0'
Year:           db   '0'
		db   '0'
                db   0Dh, 0Ah, 0

Msg_Enter_Time:
		db 'Enter new time: '
		db 0
Msg_Show_Time:
		db   'Current time is '
Hour:           db   '0'
		db   '0'
		db   ':'
Minute:         db   '0'
		db   '0'
		db   ':'
Second:         db   '0'
		db   '0'
		db   0Dh, 0Ah, 0

;VolSize_Unit1:   dd 0
;VolSize_Unit2:   dd 0

VolSize_KiloBytes:
		db " kilobytes", 0Dh, 0Ah, 0
VolSize_Bytes:
		db " bytes", 0Dh, 0Ah, 0
Volume_in_drive:
		db 0Dh, 0Ah
Vol_FS_Name:
		db "TR FS1 "
		db "Volume in drive "
Vol_Drv_Name:   db 30h
		db ":"
		db " is "
		db 0Dh, 0Ah, 0
Dir_Drive_Str:
                db "TR-DOS Drive "
Dir_Drive_Name:
                db "0:"
                db  0Dh, 0Ah
Vol_Str_Header:
                db "Volume Name: "
Vol_Name:
		times 64 db 0
		db 0
Vol_Serial_Header:
		db 0Dh, 0Ah
		db "Volume Serial No: "
Vol_Serial1:
		db "0000"
		db "-"
Vol_Serial2:
		db "0000"
		db 0Dh, 0Ah, 0

;Vol_Tot_Sec_Str_Start:
;		dd 0
Vol_Total_Sector_Header:
		db 0Dh, 0Ah
		db "Volume Size : ", 0
;Vol_Tot_Sec_Str: 
;		db "0000000000"
;Vol_Tot_Sec_Str_End:
;		db 0
;Vol_Free_Sectors_Str_Start:
;		dd 0
Vol_Free_Sectors_Header:
		db "Free Space  : ", 0
;Vol_Free_Sectors_Str:
;		db "0000000000"
;Vol_Free_Sectors_Str_End:
;		db 0

Dir_Str_Header:
                db "Directory: "
Dir_Str_Root:   db "/"
Dir_Str:        times 64 db 0
                dd 0
                db 0

Msg_Bad_Command:
                db "Bad command or file name!"
                db 0Dh, 0Ah, 0

msgl_drv_not_ready: 
		db 07h, 0Dh, 0Ah

; CMD_INTR.ASM - 09/11/2011 - Messages

Msg_Not_Ready_Read_Err:
                db "Drive not ready or read error!"
                db 0Dh, 0Ah, 0

Msg_Not_Ready_Write_Err:
                db "Drive not ready or write error!"
                db 0Dh, 0Ah, 0

Msg_Dir_Not_Found:
                db "Directory not found!"
                db 0Dh, 0Ah, 0

Msg_File_Not_Found:
                db "File not found!"
                db 0Dh, 0Ah, 0

Msg_File_Directory_Not_Found:
                db "File or directory not found!"
                db 0Dh, 0Ah, 0

Msg_LongName_Not_Found:
                db "Long name not found!"
                db 0Dh, 0Ah, 0

beep_Insufficient_Memory: ; 20/02/2017
		db 0Dh, 0Ah
		db 07h
Msg_Insufficient_Memory:
                db "Insufficient memory!"
                db 0Dh, 0Ah, 0

Msg_Error_Code:
                db 'Command failed! Error code : '
error_code_hex: db '00h'
                db 0Ah, 0Ah, 0

align 2

; 10/02/2016
; DIR.ASM - 09/10/2011

Type_Dir:       db '<DIR>     ' ; 10 bytes

File_Name:
                times 12 db 20h
		db 20h
Dir_Or_FileSize:
                times 10 db 20h
		db 20h
File_Attribute:
		dd 20202020h
		db 20h
File_Day:
                db '0','0'
		db '/'
File_Month:
                db '0','0'
		db '/'
File_Year:
                db '0','0','0','0'
		db 20h
File_Hour:
                db '0','0'
		db ':'
File_Minute:
                db '0','0'
		db 0

Decimal_File_Count_Header:
		db 0Dh, 0Ah
Decimal_File_Count:
		times 6 db 0

str_files:	db " file(s) & "
Decimal_Dir_Count: 
		times 6 db 0
str_dirs:       
		db " directory(s) "
		db 0Dh, 0Ah, 0

str_bytes:	db " byte(s) in file(s)"
		db 0Dh, 0Ah, 0

; CMD_INTR.ASM - 09/11/2011
; 07/10/2010
Msg_invalid_name_chars:
                db "Invalid file or directory name characters!"
        	db 0Dh, 0Ah, 0
; 21/02/2016
Msg_Name_Exists: db "File or directory name exists!"
                db 0Dh, 0Ah, 0
Msg_DoYouWantMkdir:
                db "Do you want to make directory ", 0
Msg_YesNo:      db " (Y/N) ? ", 0  
Y_N_nextline:	db 0, 0Dh, 0Ah, 0 
Msg_OK:		db "OK.", 0Dh, 0Ah, 0

; 27/02/2016
Msg_DoYouWantRmDir:
                db "Do you want to delete directory ", 0
Msg_Dir_Not_Empty:
                db "Directory not empty!"
                db 0Dh, 0Ah, 0

Msg_DoYouWantDelete:
                db "Do you want to delete file ",0

Msg_Deleted:    db "Deleted...", 0Dh, 0Ah, 0

Msg_Permission_Denied:
                db 7
                db "Permission denied!", 0Dh, 0Ah, 0

; 04/03/2016
Msg_New:        db "New "
                db 0
Str_Attributes:
                db "Attributes : "
Attr_Chars:     db "NORMAL"
                db 0

; 06/03/2016
; CMD_INTR.ASM - 16/11/2010 
Msg_DoYouWantRename:
                db "Do you want to rename ", 0
Rename_File:    db "file ", 0
Rename_Directory: db "directory ", 0
Rename_OldName: times 13 db 0
Msg_File_rename_as: db " as "
Rename_NewName: times 13 db 0

; 08/03/2016
; CMD_INTR.ASM - 01/08/2010 - 23/04/2011
msg_not_same_drv:
                db "Not same drive!" 
                db 0Dh, 0Ah, 0 

Msg_DoYouWantMoveFile:
                db "Do you want to move file", 0

msg_insufficient_disk_space:
                db "Insufficient disk space!" 
                db 0Dh, 0Ah, 0

; 01/08/2010
msg_source_file: 
		db 0Dh, 0Ah, "Source file name      :   "
msg_source_file_drv: 
		db " :", 0
msg_destination_file: 
		db 0Dh, 0Ah, "Destination file name :   "
msg_destination_file_drv: 
		db " :", 0
msg_copy_nextline: 
		db 0Dh, 0Ah, 0

; 15/03/2016
; CMD_INTR.ASM

Msg_DoYouWantOverWriteFile:
                db "Do you want to overwrite file ",0
  
Msg_DoYouWantCopyFile:
                db "Do you want to copy file",0

Msg_read_file_error_before_EOF:
		db "File reading error! (before EOF)"
		db 0Ah, 0Ah, 0

; 18/03/2016
; TRDOS 386 (v2.0) mainprog copy procedure
msg_reading:
		db "Reading... ", 0
msg_writing:
		db "Writing... ", 0
percentagestr:
		db "   %", 0  ; "  0%" .. "100%"
; 11/04/2016
Msg_No_Set_Space:
                db "Insufficient environment space!"
                db 0Dh, 0Ah, 0
; 18/04/2016
isc_msg:	
		db 0Dh, 0Ah
		db "INVALID SYSTEM CALL", 0
usi_msg:
		db 0Dh, 0Ah
		db "UNDEFINED SOFTWARE INTERRUPT", 0
ifc_msg:
		db 0Dh, 0Ah
		db "INVALID FUNCTION CALL"
inv_msg_for_trdos_v2:
		db 20h
		db "for TRDOS v2!"
		db 07h
		db 0Dh, 0Ah
		db 0Dh, 0Ah
		db "INT "
int_num_str:	db "00h"
		db 0Dh, 0Ah
		db "EAX : "
eax_str:	db "00000000h", 0Dh, 0Ah
		db "EIP : "
eip_str:	db "00000000h", 0Dh, 0Ah, 0

; 17/04/2021
; ('KDEV' device parameters are disabled as temporary)

; 07/10/2016
; Device names & parameters (for kernel devices)

align 2
;KDEV_NAME:
;		db 'TTY',0,0,0,0,0 ; 1
;		db 'MEM',0,0,0,0,0 ; 2
;		db 'FD0',0,0,0,0,0 ; 3
;		db 'FD1',0,0,0,0,0 ; 4
;		db 'HD0',0,0,0,0,0 ; 5
;		db 'HD1',0,0,0,0,0 ; 6
;		db 'HD2',0,0,0,0,0 ; 7
;		db 'HD3',0,0,0,0,0 ; 8
;		db 'LPT',0,0,0,0,0 ; 9
;		db 'TTY0',0,0,0,0 ; 10
;		db 'TTY1',0,0,0,0 ; 11
;		db 'TTY2',0,0,0,0 ; 12
;		db 'TTY3',0,0,0,0 ; 13
;		db 'TTY4',0,0,0,0 ; 14
;		db 'TTY5',0,0,0,0 ; 15
;		db 'TTY6',0,0,0,0 ; 16
;		db 'TTY7',0,0,0,0 ; 17
;		db 'TTY8',0,0,0,0 ; 18
;		db 'TTY9',0,0,0,0 ; 19
;		db 'COM1',0,0,0,0 ; 18
;		db 'COM2',0,0,0,0 ; 19
;		;db 'CONSOLE',0	  ; 1
;		;db 'PRINTER',0   ; 9
;		;db 'CDROM'	  ; 20
;		;db 'CDROM0'	  ; 20
;		;db 'CDROM1'	  ; 21		
;		;db 'DVD'	  ; 22
;		;db 'DVD0'	  ; 22
;		;db 'DVD1'	  ; 23		
;		;db 'USB'	  ; 24
;		;db 'USB0'	  ; 24
;		;db 'USB1'	  ; 25
;		;db 'USB2'	  ; 26
;		;db 'USB3'        ; 27
;		;db 'KEYBOARD'	  ; 1	
;		;db 'MOUSE'	  ; 28
;		;db 'SOUND'	  ; 29
;		;db 'VGA',0,0,0,0 ; 30
;		;db 'CGA',0,0,0,0 ; 31
;		;db 'AUDIO',0,0,0 ; 29
;		;db 'VIDEO',0,0,0 ; 32
;		;db 'MUSIC',0,0,0 ; 33
;		;db 'ETHERNET'	  ; 34 		
;		;db 'SD0',0,0,0,0,0 ; 35
;		;db 'SD1',0,0,0,0,0 ; 36
;		;db 'SD2',0,0,0,0,0 ; 37
;		;db 'SD3',0,0,0,0,0 ; 38
;		;db 'SATA0'	  ; 35
;		;db 'SATA1'	  ; 36
;		;db 'SATA2'        ; 37
;		;db 'SATA3'        ; 38
;		;db 'PATA0',0,0,0  ; 5
;		;db 'PATA1',0,0,0  ; 6
;		;db 'PATA2',0,0,0  ; 7
;		;db 'PATA3',0,0,0  ; 8
;		;db 'WIRELESS'	  ; 39
;		;db 'HDMI',0,0,0,0 ; 40
;		db 'NULL',0,0,0,0 ; 0

;NumOfKernelDevNames equ ($-KDEV_NAME) / 8 ; 20 (07/10/2016)

;KDEV_NUMBER:
;		db 1,2,3,4,5,6,7,8,9
;		db 10,11,12,13,14,15,16,17,18,19
;		db 18,19,0

;NumOfKernelDevices equ $ - KDEV_NUMBER

;KDEV_OADDR:
;		dd otty ;tty  ; 1
;		dd sret ;mem  ; 2
; 		dd sret ;fd0  ; 3
; 		dd sret ;fd1  ; 4
;		dd sret ;hd0  ; 5
;		dd sret ;hd1  ; 6
;		dd sret ;hd2  ; 7
;		dd sret ;hd3  ; 8
;		dd sret ;lpt  ; 9
;		dd ocvt ;tty0 ; 10
;		dd ocvt ;tty1 ; 11
;		dd ocvt ;tty2 ; 12
;		dd ocvt ;tty3 ; 13
;		dd ocvt ;tty4 ; 14
;		dd ocvt ;tty5 ; 15
;		dd ocvt ;tty6 ; 16
;		dd ocvt ;tty7 ; 17
;		dd ocvt ;tty8 ; 18
;		dd ocvt ;tty9 ; 19
;		;dd ocvt ;com1 ; 18
;		;dd ocvt ;com2 ; 19
;		dd sret ;null ; 20  
;KDEV_CADDR:
;		dd ctty ;tty  ; 1
;		dd cret ;mem  ; 2
; 		dd cret ;fd0  ; 3
; 		dd cret ;fd1  ; 4
;		dd cret ;hd0  ; 5
;		dd cret ;hd1  ; 6
; 		dd cret ;hd2  ; 7
;		dd cret ;hd3  ; 8
; 		dd cret ;lpt  ; 9
;		dd ocvt ;tty0 ; 10
;		dd ccvt ;tty1 ; 11
;		dd ccvt ;tty2 ; 12
; 		dd ccvt ;tty3 ; 13
; 		dd ccvt ;tty4 ; 14
; 		dd ccvt ;tty5 ; 15
; 		dd ccvt ;tty6 ; 16
; 		dd ccvt ;tty7 ; 17
; 		dd ccvt ;tty8 ; 18
; 		dd ccvt ;tty9 ; 19
; 		;dd ccvt ;com1 ; 18
; 		;dd ccvt ;com2 ; 19
;		dd cret ;null ; 20
;
;KDEV_RADDR:
;		dd rtty ;tty  ; 1
;		dd rmem ;mem  ; 2
;		dd rfd  ;fd0  ; 3
; 		dd rfd  ;fd1  ; 4
; 		dd rhd  ;hd0  ; 5
; 		dd rhd  ;hd1  ; 6
; 		dd rhd  ;hd2  ; 7
; 		dd rhd  ;hd3  ; 8
; 		dd rlpt ;lpt  ; 9
; 		dd rcvt ;tty0 ; 10
;		dd rcvt ;tty1 ; 11
; 		dd rcvt ;tty2 ; 12
; 		dd rcvt ;tty3 ; 13
; 		dd rcvt ;tty4 ; 14
; 		dd rcvt ;tty5 ; 15
; 		dd rcvt ;tty6 ; 16
; 		dd rcvt ;tty7 ; 17
; 		dd rcvt ;tty8 ; 18
; 		dd rcvt ;tty9 ; 19
; 		;dd rcvt ;com1 ; 18
; 		;dd rcvt ;com2 ; 19
;		dd rnull ;null ; 20  
;KDEV_WADDR:
;		dd wtty ;tty  ; 1
;		dd wmem ;mem  ; 2
;		dd wfd  ;fd0  ; 3
; 		dd wfd  ;fd1  ; 4
; 		dd whd  ;hd0  ; 5
; 		dd whd  ;hd1  ; 6
; 		dd whd  ;hd2  ; 7
; 		dd whd  ;hd3  ; 8
; 		dd wlpt ;lpt  ; 9
; 		dd xmtt ;tty0 ; 10
;		dd xmtt ;tty1 ; 11
; 		dd xmtt ;tty2 ; 12
; 		dd xmtt ;tty3 ; 13
; 		dd xmtt ;tty4 ; 14
; 		dd xmtt ;tty5 ; 15
; 		dd xmtt ;tty6 ; 16
; 		dd xmtt ;tty7 ; 17
; 		dd xmtt ;tty8 ; 18
; 		dd xmtt ;tty9 ; 19
; 		;dd xmtt ;com1 ; 18
; 		;dd xmtt ;com2 ; 19
;		dd wnull ;null ; 20  

; DEV_ACCESS bits:
	; bit 0 = accessable by normal users
	; bit 1 = read access permission
	; bit 2 = write access permission
	; bit 3 = IOCTL permission to users
	; bit 4 = block device if it is set
	; bit 5 = 16 bit or 1024 byte data
	; bit 6 = 32 bit or 2048 byte data
	; bit 7 = installable device driver	

;KDEV_ACCESS: ; 08/10/2016
;		db  00000111b	; tty, 1
;		db  00000111b	; mem, 2	
;		db  10001111b	; fd0, 3	
;		db  10001111b	; fd1, 4
;		db  10001111b	; hd0, 5
;		db  10001111b	; hd1, 6
;		db  10001111b	; hd2, 7
;		db  10001111b	; hd3, 8
;		db  00000111b   ; lpt, 9
;		db  00000111b	; tty0, 10
;		db  00000111b	; tty1, 11
;		db  00000111b	; tty2, 12
;		db  00000111b	; tty3, 13
;		db  00000111b	; tty4, 14
;		db  00000111b	; tty5, 15
;		db  00000111b	; tty6, 16
;		db  00000111b	; tty7, 17
;		db  00000111b	; tty8, 18
;		db  00000111b	; tty9, 19
;		;db 00000111b	; com1, 18
;		;db 00000111b	; com2, 19
;		db  00000000b   ; null, 0

; 07/10/2016
;NumOfInstallableDevices equ 8
;NUMIDEV	equ NumOfInstallableDevices ; 8
;NUMOFDEVICES	equ NumOfKernelDevices + NumOfInstallableDevices

; 26/02/2017
; IRQ Callback (& Signal Response Byte) service availibity
; 'syscalbac'
; ***************************************************
; IRQ 00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15
; --- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
; --- 00 00 00 01 02 03 00 04 00 05 06 07 08 09 00 00
; ***************************************************
IRQenum:
	db  0,0,0,1,2,3,0,4,0,5,6,7,8,9,0,0

; 28/08/2017
; 20/08/2017
; DMA Registers (for 'sysdma')
; 02/07/2017 (sb16mod.s)
dma_adr:	db 0,2,4,6,0C0h,0C4h,0C8h,0CCh
dma_cnt:	db 1,3,5,7,0C2h,0C6h,0CAh,0CEh
dma_page:	db 87h,83h,81h,82h,8Fh,8Bh,89h,8Ah ; 03/08/2017
dma_mask:	db 0Ah,0Ah,0Ah,0Ah,0D4h,0D4h,0D4h,0D4h
dma_mod:	db 0Bh,0Bh,0Bh,0Bh,0D6h,0D6h,0D6h,0D6h
dma_flip:	db 0Ch,0Ch,0Ch,0Ch,0D8h,0D8h,0D8h,0D8h	