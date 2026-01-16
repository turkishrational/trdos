; ****************************************************************************
; TRDOS386.ASM (TRDOS 386 Kernel - v2.0.10) - SYSTEM DEFINITIONS : sysdefs.s
; ----------------------------------------------------------------------------
; Last Update: 08/12/2025  (Previous: 23/07/2022, v2.0.5)
; ----------------------------------------------------------------------------
; Beginning: 24/01/2016
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.15 (trdos386.s)
; ----------------------------------------------------------------------------
; Derived from 'Retro UNIX 386 Kernel - v0.2.1.0' source code by Erdogan Tan
; sysdefs.inc (14/11/2015)
; ****************************************************************************

; Retro UNIX 386 v1 Kernel - SYSDEFS.INC
; Last Modification: 14/11/2015
;
; ///////// RETRO UNIX 386 V1 SYSTEM DEFINITIONS ///////////////
; (Modified from 
;	Retro UNIX 8086 v1 system definitions in 'UNIX.ASM', 01/09/2014)
; ((UNIX.ASM (RETRO UNIX 8086 V1 Kernel), 11/03/2013 - 01/09/2014))
; 	UNIX.ASM (MASM 6.11) --> SYSDEFS.INC (NASM 2.11)
; ----------------------------------------------------------------------------
;
; Derived from UNIX Operating System (v1.0 for PDP-11)
; (Original) Source Code by Ken Thompson (1971-1972)
; <Bell Laboratories (17/3/1972)>
; <Preliminary Release of UNIX Implementation Document>
;
; ****************************************************************************

nproc 	equ	16  ; number of processes
;nfiles equ	50
ntty	equ     8   ; 8+1 -> 8 (10/05/2013)
; 17/04/2025 - TRDOS 386 v2.0.10
;nbuf	equ	114
; 05/06/2025 - temporary !
nbuf	equ	60
;nbuf	equ	4   ; 6 ;; 21/08/2015 - 'namei' buffer problem when nbuf > 4
		; NOTE: If fd0 super block buffer addres is beyond of the 1st
		; 32K, DMA r/w routine or someting else causes a jump to
		; kernel panic routine (in 'alloc' routine, in u5.s)
		; because of invalid buffer content (r/w error).
		; When all buffers are set before the end of the 1st 32k,
		; there is no problem!? (14/11/2015)

;csgmnt	equ	2000h	; 26/05/2013 (segment of process 1)
;core	equ 	0  	    ; 19/04/2013
;ecore	equ	32768 - 64  ; 04/06/2013 (24/05/2013)
	; (if total size of argument list and arguments is 128 bytes)
	; maximum executable file size = 32768-(64+40+128-6) = 32530 bytes
	; maximum stack size = 40 bytes (+6 bytes for 'IRET' at 32570)
	; initial value of user's stack pointer = 32768-64-128-2 = 32574
	; 	(sp=32768-args_space-2 at the beginning of execution)
	; argument list offset = 32768-64-128 = 32576 (if it is 128 bytes)
	; 'u' structure offset (for the '/core' dump file) = 32704
	; '/core' dump file size = 32768 bytes

; 08/03/2014
;sdsegmnt equ	6C0h  ; 256*16 bytes (swap data segment size for 16 processes)
; 19/04/2013 Retro UNIX 8086 v1 feaure only !
;;sdsegmnt equ 	740h  ; swap data segment (for user structures and registers)

; 30/08/2013
time_count equ 4 ; 10 --> 4 01/02/2014

; 05/02/2014
; process status
;SFREE 	equ 0
;SRUN	equ 1
;SWAIT	equ 2
;SZOMB	equ 3
;SSLEEP	equ 4 ; Retro UNIX 8086 V1 extension (for sleep and wakeup)

; 09/03/2015
userdata equ 80000h ; user structure data address for current user ; temporary
swap_queue equ 90000h - 2000h ; swap queue address ; temporary
swap_alloc_table equ 0D0000h  ;  swap allocation table address ; temporary

; 17/09/2015
ESPACE equ 48 ; [u.usp] (at 'sysent') - [u.sp] value for error return

; 20/08/2024
; 31/12/2017
; 19/02/2017
; 15/10/2016
; 20/05/2016
; 19/05/2016
; 18/05/2016
; 29/04/2016
; TRDOS 386 (TRDOS v2.0) system calls - temporary List
; 14/07/2013 - 21/09/2015 (Retro UNIX 8086 & 386 system calls)
_ver 	equ 0 ; Get TRDOS version (v2.0)
_exit 	equ 1
_fork 	equ 2
_read 	equ 3
_write	equ 4
_open	equ 5
_close 	equ 6
_wait 	equ 7
_creat 	equ 8
_rename	equ 9  ; TRDOS 386, Rename File (31/12/2017)
_delete	equ 10 ; TRDOS 386, Delete File (29/12/2017)
_exec	equ 11
_chdir	equ 12
_time 	equ 13 ; TRDOS 386, Get Sys Date&Time (30/12/2017)
_mkdir 	equ 14
_chmod	equ 15 ; TRDOS 386, Change Attributes (30/12/2017)
_rmdir	equ 16 ; TRDOS 386, Remove Directory (29/12/2017)
_break	equ 17
_drive	equ 18 ; TRDOS 386, Get/Set Current Drv (30/12/2017)
_seek	equ 19
_tell 	equ 20
_mem	equ 21 ; TRDOS 386, Get Total&Free Mem (31/12/2017)
_prompt	equ 22 ; TRDOS 386, Change Cmd Prompt (31/12/2017)
_path	equ 23 ; TRDOS 386, Get/Set Run Path (31/12/2017)
_env	equ 24 ; TRDOS 386, Get/Set Env Vars (31/12/2017)
_stime	equ 25 ; TRDOS 386, Set Sys Date&Time (30/12/2017)
_quit	equ 26
_intr	equ 27
_dir	equ 28 ; TRDOS 386, Get Curr Drive&Dir (30/12/2017)
_emt 	equ 29
_ldrvt 	equ 30 ; TRDOS 386, Get Logical DOS DDT (30/12/2017)
_video  equ 31 ; TRDOS 386 Video Functions (16/05/2016)
_audio	equ 32 ; TRDOS 386 Video Functions (16/05/2016)
_timer	equ 33 ; TRDOS 386 Timer Functions (18/05/2016)
_sleep	equ 34 ; Retro UNIX 8086 v1 feature only !
_msg	equ 35 ; Retro UNIX 386 v1 feature only !
_geterr	equ 36 ; Retro UNIX 386 v1 feature only !
_fpsave equ 37 ; TRDOS 386 FPU state option (28/02/2017)
_pri 	equ 38 ; change priority - TRDOS 386 (20/05/2016)
_rele	equ 39 ; TRDOS 386 (19/05/2016)
_fff	equ 40 ; Find First File - TRDOS 386 (15/10/2016)
_fnf	equ 41 ; Find Next File - TRDOS 386 (15/10/2016)
_alloc	equ 42 ; Allocate memory - TRDOS 386 (19/02/2017)
	       ; TRDOS 386 (19/02/2017) DMA buff fuctions
_dalloc equ 43 ; Deallocate mem - TRDOS 386 (19/02/2017)
_calbac equ 44 ; Set IRQ callback - TRDOS 386 (20/02/2017)
_dma	equ 45 ; DMA service - TRDOS 386 (20/08/2017)
	       ; TRDOS 386 v2.0.9
_stdio	equ 46 ; STDIN/STDOUT functions (20/08/2024)

%macro sys 1-4
    ; 29/04/2016 - TRDOS 386 (TRDOS v2.0)
    ; 03/09/2015
    ; 13/04/2015
    ; Retro UNIX 386 v1 system call.
    %if %0 >= 2   
        mov ebx, %2
        %if %0 >= 3
            mov ecx, %3
            %if %0 = 4
               mov edx, %4
            %endif
        %endif
    %endif
    mov eax, %1
    ;int 30h
    int 40h ; TRDOS 386 (TRDOS v2.0)
%endmacro

; TRDOS 386 system calls, interrupt number
; 25/12/2016
SYSCALL_INT_NUM   equ '40' ; '40h'

; 13/05/2015 - ERROR CODES
ERR_FILE_NOT_OPEN  equ 10 ; 'file not open !' error
; 13/11/2025
;ERR_FILE_ACCESS   equ 11 ; 'permission denied !' error
; 14/05/2015
;ERR_DIR_ACCESS    equ 11 ; 'permission denied !' error
ERR_FILE_NOT_FOUND equ 12 ; 'file not found !' error
ERR_TOO_MANY_FILES equ 13 ; 'too many open files !' error
ERR_DIR_EXISTS     equ 14 ; 'directory already exists !' error
; 16/05/2015
ERR_DRV_NOT_RDY    equ 15 ; 'drive not ready !' error
; 18/05/2015
ERR_DEV_NOT_RDY    equ 15 ; 'device not ready !' error
ERR_DEV_ACCESS     equ 11 ; 'permission denied !' error
ERR_DEV_NOT_OPEN   equ 10 ; 'device not open !' error
; 07/06/2015
ERR_FILE_EOF	   equ 16 ; 'end of file !' error
ERR_DEV_VOL_SIZE   equ 16 ; 'out of volume !' error
; 09/06/2015
ERR_DRV_READ	   equ 17 ; 'disk read error !'
ERR_DRV_WRITE	   equ 18 ; 'disk write error !'
; 16/06/2015
ERR_NOT_DIR	   equ 19 ; 'not a (valid) directory !' error
ERR_FILE_SIZE	   equ 20 ; 'file size error !'
; 22/06/2015
ERR_NOT_SUPERUSER  equ 11 ; 'permission denied !' error
ERR_NOT_OWNER      equ 11 ; 'permission denied !' error
ERR_NOT_FILE       equ 11 ; 'permission denied !' error
; 23/06/2015
ERR_FILE_EXISTS    equ 14 ; 'file already exists !' error
ERR_DRV_NOT_SAME   equ 21 ; 'not same drive !' error
ERR_DIR_NOT_FOUND  equ 12 ; 'directory not found !' error
ERR_NOT_EXECUTABLE equ 22 ; 'not executable file !' error
; 27/06/2015
ERR_INV_PARAMETER  equ 23 ; 'invalid parameter !' error
ERR_INV_DEV_NAME   equ 24 ; 'invalid device name !' error
; 29/06/2015
ERR_TIME_OUT	   equ 25 ; 'time out !' error
ERR_DEV_NOT_RESP   equ 25 ; 'device not responding !' error
; 10/10/2016
ERR_INV_FILE_NAME  equ 26 ; 'invalid file name !' error
ERR_INV_FLAGS	   equ 23 ; 'invalid flags !' error
; For code compatibility with previous version of TRDOS (2011)
; (Temporary error codes for current TRDOS 386 -2016- version)
ERR_NO_MORE_FILES  equ 12 ; 'no more files !' error
ERR_PATH_NOT_FOUND equ  3 ; 'path not found !' error
			  ; 'dir not found !' ; TRDOS 8086
ERR_NOT_FOUND	   equ  2 ; 'file not found !' ; TRDOS 8086
ERR_DISK_SPACE	   equ 39 ; 'out of volume !' TRDOS 8086
			  ; 'insufficient disk space !' ; 27h
ERR_DISK_WRITE	   equ 30 ; 'disk write protected !' ; 16/10/2016
ERR_ACCESS_DENIED  equ  5 ; 'access denied !' ; TRDOS 8086
; 28/02/2017
;ERR_PERM_DENIED   equ 11 ; 'permission denied !' error
; 13/11/2025 - TRDOS 386 v2.0.10
ERR_PERM_DENIED	   equ 5  ; 'permission denied !' error
ERR_FILE_ACCESS	   equ 5  ; 'permission denied !' error
ERR_DIR_ACCESS	   equ 5  ; 'permission denied !' error
; 18/05/2016
ERR_MISC	   equ 27 ; miscellaneous/other errors
; 15/10/2016
; TRDOS 8086 -> TRDOS 386 (0Bh -> 28)
ERR_INV_FORMAT	   equ 28 ; 'invalid format !' error
; TRDOS 8086 -> TRDOS 386 (0Dh -> 29)
ERR_INV_DATA	   equ 29 ; 'invalid data !' error
; TRDOS 8086 -> TRDOS 386 (0Eh -> 20)
ERR_ZERO_LENGTH	   equ 20  ; 'zero length !' error
; TRDOS 8086 -> TRDOS 386 (15h -> 17, 1Dh -> 18, 1Eh -> 17)
ERR_DRV_NR_READ	   equ 17 ; 'drive not ready or read error !'
ERR_DRV_NR_WRITE   equ 18 ; 'drive not ready or write error !'
; 15/10/2016
ERR_INV_PATH_NAME  equ 19 ; 'bad path name !' error
ERR_BAD_CMD_ARG	   equ  1 ; 'bad command argument !' ; TRDOS 8086
ERR_INV_FNUMBER	   equ  1 ; 'invalid function number !' ; TRDOS 8086
ERR_BIG_FILE	   equ  8 ; 'big file & out of memory ! ; TRDOS 8086
ERR_BIG_DATA	   equ  8 ; 'big data & out of memory ! ; TRDOS 8086
ERR_CLUSTER	   equ 35 ; 'cluster not available !' ; TRDOS 8086
ERR_OUT_OF_MEMORY  equ  4 ; 'out of memory !'
			  ; 'insufficient memory !'
ERR_P_VIOLATION	   equ	6 ; 'protection violation !'
ERR_PAGE_FAULT	   equ 224 ;'page fault !' ;0E0h
ERR_SWP_DISK_READ  	   equ 40
ERR_SWP_DISK_NOT_PRESENT   equ 41
ERR_SWP_SECTOR_NOT_PRESENT equ 42
ERR_SWP_NO_FREE_SPACE      equ 43
ERR_SWP_DISK_WRITE         equ 44
ERR_SWP_NO_PAGE_TO_SWAP    equ 45
; 10/04/2017
ERR_BUFFER	   equ 46  ; 'buffer error !'
; 28/08/2017 (20/08/2017)
ERR_DMA		   equ -1  ; DMA buffer (allocation/misc.) error!
; 03/06/2025
ERR_CHECKSUM	   equ 31  ; 'checksum error !' ; TRDOS 386 v2.0.10

; 26/08/2015
; 24/07/2015
; 24/06/2015
MAX_ARG_LEN	   equ 256 ; max. length of sys exec arguments
; 01/07/2015
MAX_MSG_LEN	   equ 255 ; max. msg length for 'sysmsg'
;
; 06/10/2016
;OPENFILES	   equ 10  ; max. number of open files (system)
; 23/07/2022
OPENFILES	   equ 32  ; max. number of open files (system)
; 07/10/2016
;NUMOFDEVICES	   equ 20  ; max. num of available devices (sys)

; 08/12/2025
open_for_read	equ 1
open_for_write	equ 2
open_for_rw	equ 3
