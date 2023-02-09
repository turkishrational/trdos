; ****************************************************************************
; TRDOS386.ASM (TRDOS 386 Kernel - v2.0.5) - UNINITIALIZED USER DATA : ubss.s
; ----------------------------------------------------------------------------
; Last Update: 07/08/2022  (Previous: 28/02/2017)
; ----------------------------------------------------------------------------
; Beginning: 24/01/2016
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.15 (trdos386.s)
; ----------------------------------------------------------------------------
; Derived from 'Retro UNIX 386 Kernel - v0.2.1.0' source code by Erdogan Tan
; ux.s (04/12/2015)
; ****************************************************************************

; Retro UNIX 386 v1 Kernel - ux.s
; Last Modification: 04/12/2015
;
; ///////// RETRO UNIX 386 V1 SYSTEM DEFINITIONS ///////////////
; (Modified from 
;	Retro UNIX 8086 v1 system definitions in 'UNIX.ASM', 01/09/2014)
; ((UNIX.ASM (RETRO UNIX 8086 V1 Kernel), 11/03/2013 - 01/09/2014))
; ----------------------------------------------------------------------------
; Derived from UNIX Operating System (v1.0 for PDP-11) 
; (Original) Source Code by Ken Thompson (1971-1972)
; <Bell Laboratories (17/3/1972)>
; <Preliminary Release of UNIX Implementation Document>
; (Section E10 (17/3/1972) - ux.s)
; ****************************************************************************
; Ref: Retro UNIX 386 v1.2 Kernel (v0.2.2.3) - ux.s - 15/07/2022

alignb 2

%if 0

inode:
	;; 11/03/2013. 
	;;Derived from UNIX v1 source code 'inode' structure (ux).
	;;i.
	;
	;i.flgs: resw 1
	;i.nlks: resb 1
	;i.uid:	 resb 1
        ;i.size: resw 1 ; size
	;i.dskp: resw 8 ; 16 bytes
	;i.ctim: resd 1
	;i.mtim: resd 1
	;i.rsvd: resw 1 ; Reserved (ZERO/Undefined word for UNIX v1)

	; 26/01/2020
	; Retro UNIX 386 v2.0 - Modified UNIX v7 inode model
	;	(15/09/2029 .. 18/12/2019)

	i.flgs:   resw 1	; /* mode and type of file */
	i.nlks:	  resw 1	; /* number of links to file */
	i.uid:	  resw 1	; /* owner's user id */  - 0 to 65535 -
	i.gid:	  resb 1	; /* owner's group id */ - o to 255 -
	i.size_h: resb 1	; /* number of bytes in file */ ; byte 5
	i.size:	  resd 1 ; size	; /* number of bytes in file */
	i.dskp:	  resd 10 ; 40 bytes ; /* disk block addresses */
	i.atim:	  resd 1	; /* time last accessed */
	i.mtim:	  resd 1	; /* time last modified */
	i.ctim:	  resd 1	; /* time created */

I_SIZE	equ $ - inode

%endif 

process:
	; 23/07/2022 - TRDOS 386 Kernel v2.0.5 
	; 27/02/2022
	; 12/01/2022 - Retro UNIX 386 v1.2 
	; 19/12/2016
	; 21/05/2016
	; 19/05/2016 - TRDOS 386 (TRDOS v2.0)
	; 06/05/2015 - Retro UNIX 386 v1
	; 11/03/2013 - 05/02/2014 (Retro UNIX 8086 v1)
	;Derived from UNIX v1 source code 'proc' structure (ux).
	;p.
	
        p.pid:   resw nproc
        p.ppid:  resw nproc
	;p.break: resw nproc ; 12/01/2022 (p.break is not used)
        p.ttyc:  resb nproc ; console tty in Retro UNIX 8086 v1.
	; 27/02/2022 (p.waitc is not used)
	;p.waitc: resb nproc ; waiting channel in Retro UNIX 8086 v1.
	p.link:	 resb nproc
	p.stat:	 resb nproc

	; 06/05/2015 (Retro UNIX 386 v1 feature only !) 
	p.upage: resd nproc ; Physical address of the process's
			    ; 'user' structure
	; 21/05/2016	
	; 19/05/2016 (TRDOS 386 feature only!)
	p.timer: resb nproc ; number of timer events of the processs

	; 19/12/2016
	p.tcb:	resd nproc ; timer callback service address (if > 0)
			  		 			 	  
P_SIZE	equ $ - process

; fsp table (original UNIX v1)
;
;Entry
;          15                                      0
;  1     |---|---------------------------------------|
;        |r/w|       i-number of open file           |
;        |---|---------------------------------------| 
;        |               device number               |
;        |-------------------------------------------|
;    (*) | offset pointer, i.e., r/w pointer to file |
;        |-------------------------------------------| 
;        |  flag that says    | number of processes  |
;        |   file deleted     | that have file open  |
;        |-------------------------------------------| 
;  2     |                                           |
;        |-------------------------------------------| 
;        |                                           |
;        |-------------------------------------------|
;        |                                           |
;        |-------------------------------------------|
;        |                                           |
;        |-------------------------------------------| 
;  3     |                                           | 
;        |                                           |  
;
; (*) Retro UNIX 386 v1 modification: 32 bit offset pointer 

; 27/03/2020 - Retro UNIX 386 v2 - FSP (OPEN FILES) TABLE 

;Entry
;         15                    7                   0
;  1     |-------------------------------------------|
;        |   	     i-number of open file           |
;        |-------------------------------------------| 
;        |        high word of 32 bit i-number       |
;        |-------------------------------------------|
;        | open mode & status  |   device number     |
;        |-------------------------------------------|
;        |    reserved byte    |     open count      |
;        |-------------------------------------------| 
;        | offset pointer, i.e., r/w pointer to file |
;        |-------------------------------------------|
;        |   64 bit file offset pointer (bit 16-31)  | 
;        |-------------------------------------------|
;        |   64 bit file offset pointer (bit 32-47)  | 
;        |-------------------------------------------|
;        |   64 bit file offset pointer (bit 48-63)  | 
;        |-------------------------------------------|
;  2     |                                           |
;        |-------------------------------------------| 
;        |                                           |
;        |-------------------------------------------|
;        |                                           |
;        |-------------------------------------------|
;        |                                           |
;        |-------------------------------------------| 
;        |                                           | 

%if 0

; (Retro UNIX 386 v1.2 - ux.s - 15/07/2022)
; 22/11/2021
; 21/07/2021 - Retro UNIX 386 v2 open file structure revision

struc file	; open files (fsp) structure ; (*)	
  .inode:  resw 1  ; inode number of open file (32 bit)
  .i32:	   resw 1  ; higher word of inode number (reserved)
  .drive:  resb 1  ; logical drive (disk) number
  .flags:  resb 1  ; open mode and status
  .count:  resb 1  ; number of processes that have file open
  ;.rsvd:  resb 1  ; reserved byte (for next versions)
  .mnt:    resb 1  ; mnttab index+1 (0 = not mounted)
  .offset: resd 1  ; file offset/pointer (64 bit) 
  .o64:	   resd 1  ; higher 32 bit of file offset
 .size:  ; = 16		
endstruc 

%endif

; 23/07/2022
;fsp:	resb nfiles * 16 ; (*)

idev:	resb 1
cdev:	resb 1	

; 15/04/2015
;fsp:	resb nfiles * 10 ; 11/05/2015 (8 -> 10)
;idev:	resw 1 ; device number is 1 byte in Retro UNIX 8086 v1 !
;cdev:	resw 1 ; device number is 1 byte in Retro UNIX 8086 v1 !

; 18/05/2015
; 26/04/2013 device/drive parameters (Retro UNIX 8086 v1 feature only!)
; 'UNIX' device numbers (as in 'cdev' and 'u.cdrv')
;	0 -> root device (which has Retro UNIX 8086 v1 file system)
; 	1 -> mounted device (which has Retro UNIX 8086 v1 file system)
; 'Retro UNIX 8086 v1' device numbers: (for disk I/O procedures)
;	0 -> fd0 (physical drive, floppy disk 1), physical drive number = 0
;	1 -> fd1 (physical drive, floppy disk 2), physical drive number = 1
;	2 -> hd0 (physical drive, hard disk 1), physical drive number = 80h
;	3 -> hd1 (physical drive, hard disk 2), physical drive number = 81h
;	4 -> hd2 (physical drive, hard disk 3), physical drive number = 82h
;	5 -> hd3 (physical drive, hard disk 4), physical drive number = 83h

rdev:	resb 1 ; root device number ; Retro UNIX 8086 v1 feature only!
	        ; as above, for physical drives numbers in following table
mdev:	resb 1 ; mounted device number ; Retro UNIX 8086 v1 feature only!

; 23/07/2022
;; 15/04/2015
;active: resb 1 
;	 resb 1 ; 09/06/2015

; 23/07/2022
;mnti:	 resw 1
; 07/08/2022
mpid:	 resw 1
;rootdir: resw 1
rootdir: resd 1	

; 21/05/2016 - TRDOS 386 (TRDOS v2.0) - priority levels, 3 run queues 
runq:
runq_event:	 resw 1 ; high priority, 'run for event'            ; 2
runq_normal:	 resw 1 ; normal/regular priority, 'run as reqular' ; 1
runq_background: resw 1 ; low priority, 'run on background'         ; 0
;
; 23/07/2022
;imod:	resb 1
;smod:	resb 1
;mmod:	resb 1
sysflg:	resb 1
	resb 1	

alignb 4

user:
	; 23/07/2022 - TRDOS 386 Kernel v2.0.5
	; 04/12/2021 - Retro UNIX 386 v1.2
	; 13/01/2017
	; 19/12/2016
	; 21/05/2016 - TRDOS 386 (TRDOS v2.0) 
	; 	       [u.pri] usage method modification
	; 04/12/2015 
	; 18/10/2015
	; 12/10/2015
	; 21/09/2015
	; 24/07/2015
	; 16/06/2015
	; 09/06/2015
	; 11/05/2015
	; 16/04/2015 (Retro UNIX 386 v1 - 32 bit modifications)
	; 10/10/2013
	; 11/03/2013. 
	;Derived from UNIX v1 source code 'user' structure (ux).
	;u.

	u.sp:	  resd 1 ; esp (kernel stack at the beginning of 'sysent')
	u.usp:	  resd 1 ; esp (kernel stack points to user's registers)
	u.r0:	  resd 1 ; eax
	u.cdir:	  resw 1
		  resw 1 ; 23/07/2022
	u.cdrv:	  resb 1 ; 23/07/2022
		  resb 1				  	
	u.fp:	  resb 10
	;u.fp:	  resb OPENFILES ; 23/07/202
	u.fsp:	  ; 23/07/2022
	u.fofp:	  resd 1
	u.dirp:	  resd 1
	u.namep:  resd 1
	u.off:	  resd 1
	;	  resd 1 ; 23/07/2022 (64 bit fptr)
	u.base:	  resd 1
	u.count:  resd 1
	u.nread:  resd 1
	u.break:  resd 1 ; break
	; 10/01/2017 (TRDOS 386, relocation and dword alignment)
	; tty number (rtty, rcvt, wtty)
	u.ttyp:	  resw 1 
	u.ttyn:	  resb 1 ; 28/07/2013 - Retro Unix 8086 v1 feature only !
	u.mode:   resb 1 ; 23/07/2022
	;u.resb:  resb 1 ; 10/01/2017 (TRDOS 386, temporary)
	u.dirbuf: resb 16 ; 04/12/2015 (10 -> 16) 
	;u.pri:	  resw 1 ; 14/02/2014
	u.quant:  resb 1 ; Retro UNIX 8086 v1 Feature only ! (uquant)
		  resb 1 ; 23/07/2022
	u.pri:	  resb 1 ; Modification: 21/05/2016 (priority levels: 0, 1, 2)
		  resb 1 ; 23/07/2022	
	u.intr:	  resw 1
	u.quit:	  resw 1
	;u.emt:	  resw 1 ; 10/10/2013
	;u.ilgins: resw 1 ; 10/01/2017
	;u.cdrv:  resw 1 ; cdev
	u.bsys:	  resb 1
	u.uno:	  resb 1
	; 23/07/2022
	;u.uid:	  resw 1 ; uid	; 27/03/2021 - Retro UNIX 386 v2
	;u.ruid:  resw 1	; 16 bit uid
	;u.gid:	  resb 1 ; gid 	; 27/03/2021 - Retro UNIX 386 v2
	;u.rgid:  resb 1
	; 23/07/2022
	;u.procp: resd 1 ; /* pointer to proc structure */
	u.uid:	  resb 1 ; uid
	u.ruid:   resb 1
        u.upage:  resd 1 ; 16/04/2015 - Retro Unix 386 v1 feature only !
	u.pgdir:  resd 1 ; 09/03/2015 (page dir addr of process)
	u.ppgdir: resd 1 ; 06/05/2015 (page dir addr of the parent process)
	u.pbase:  resd 1 ; 20/05/2015 (physical base/transfer address)
	u.pcount: resw 1 ; 20/05/2015 (byte -transfer- count for page)
	;u.pncount: resw 1 
		; 16/06/2015 (byte -transfer- count for page, 'namei', 'mkdir')
	;u.pnbase:  resd 1 
		; 16/06/2015 (physical base/transfer address, 'namei', 'mkdir')
			 ; 09/06/2015
	u.kcall:  resb 1 ; The caller is 'namei' (dskr) or 'mkdir' (dskw) sign		
	u.brwdev: resb 1 ; Block device number for direct I/O (bread & bwrite)
			 ; 24/07/2015 - 24/06/2015
	;u.args:  resd 1 ; arguments list (line) offset from start of [u.upage]
			 ; (arg list/line is from offset [u.args] to 4096 in [u.upage])
			 ; ([u.args] points to argument count -argc- address offset)
 			 ; 24/06/2015	  	
	;u.core:  resd 1 ; physical start address of user's memory space (for sys exec)
	;u.ecore: resd 1 ; physical end address of user's memory space (for sys exec)
	; last error number
	u.error:  resd 1 ; 28/07/2013 - 09/03/2015 
		         ; Retro UNIX 8086/386 v1 feature only!
			 ; 21/09/2015 (debugging - page fault analyze)
	u.pfcount: resd 1 ; page fault count for (this) process (for sys geterr)
		; 19/12/2016 (TRDOS 386)	
	u.tcb:	  resd 1 ; Timer callback address/flag which will be used by timer int
		; 13/01/2017 (TRDOS 386)
	u.t_lock: resb 1 ; Timer interrupt (callback) lock (unlocked by 'sysrele')
	u.t_mode: resb 1 ; running mode during timer interrupt (0= system, 0FFh= user)
		; 26/02/2017 (TRDOS 386)
	u.irqc:	  resb 1  ; Count of IRQ callback services (IRQs in use)
		; 28/02/2017 (TRDOS 386) 
	u.irqwait: resb 1 ; IRQ waiting for callback service flag (IRQ number, If > 0)
	u.r_lock: resb 1 ; 'IRQ callback service is in progress' flag (IRQ lock)
	u.r_mode: resb 1 ; running mode during hadware interrupt
	; 23/07/2022
	u.exit:	  resb 1 ; exit code
	; 27/02/2017 (TRDOS 386) 
	u.fpsave: resb 1 ; TRDOS 386, 'save/restore FPU registers' flag
alignb 4
	; !! wrong sizing in TRDOS 386 v2.0.4 (in 'ubss.s', 28/02/2017) !! 
	;u.fpregs: resb 94 ; 94 byte area for saving and restoring FPU registers
	; 23/07/2022
	u.fpregs: resb 108 ; 108 byte area for saving and restoring FPU registers

alignb 4

U_SIZE	equ $ - user

; 18/10/2015 - Retro UNIX 386 v1 (local variables for 'namei' and 'sysexec')
pcore:	resd 1  ; physical start address of user's memory space (for sys exec)
ecore:	resd 1  ; physical address of user's stack/last page (for sys exec)
nbase:	resd 1	; physical base address for 'namei' & 'sysexec'
; 23/07/202 - TRDOS 386 Kernel v2.0.5
;ncount: resw 1
ncount: resd 1	; remain byte count in page for 'namei' & 'sysexec'
;argc:	resw 1
argc:	resd 1	; argument count for 'sysexec'
argv:	resd 1	; argument list (recent) address for 'sysexec'

; 03/06/2015 - Retro UNIX 386 v1 Beginning
; 07/04/2013 - 31/07/2013 - Retro UNIX 8086 v1
rw:	resb 1 ;; Read/Write sign (iget)
; 23/07/2022
	resb 3	

alignb 4

; 24/04/2016
ii:	resd 1 ; first cluster of the program file
i.size:	resd 1 ; size of the program file