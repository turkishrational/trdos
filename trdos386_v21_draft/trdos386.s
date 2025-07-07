; ****************************************************************************
; TRDOS386.ASM (TRDOS 386 Kernel) - v2.0.10 (v2.1.0 pre-work)
; ----------------------------------------------------------------------------
; Last Update: 07/07/2025 (Previous: 28/01/2025)
; ----------------------------------------------------------------------------
; Beginning: 04/01/2016
; ----------------------------------------------------------------------------
; Assembler: NASM version 2.15 (trdos386.s)
; ----------------------------------------------------------------------------
; Turkish Rational DOS
; Operating System Project v2.0 by ERDOGAN TAN (Beginning: 04/01/2016)
;
; Derived from 'Retro UNIX 386 Kernel - v0.2.1.0' source code by Erdogan Tan
; unix386.s (03/01/2016)
;
; Derived from TRDOS Operating System v1.0 (8086) source code by Erdogan Tan
; TRDOS2.ASM (09/11/2011)
;
; Derived from 'IBM PC-XT-286' BIOS source code (1986)
; ****************************************************************************
; nasm trdos386.s -l trdos386.txt -o TRDOS386.SYS

KLOAD	equ 10000h ; Kernel loading address
	; NOTE: Retro UNIX 8086 v1 boot code loads kernel at 1000h:0000h 
KCODE	equ 08h	; Code segment descriptor (ring 0)
KDATA	equ 10h	; Data segment descriptor (ring 0)
; 19/03/2015
UCODE	equ 1Bh ; 18h + 3h  (ring 3)
UDATA	equ 23h ; 20h + 3h  (ring 3)
; 24/03/2015
TSS	equ 28h	; Task state segment descriptor (ring 0)
; 19/03/2015
CORE	equ 400000h  ; Start of USER's virtual/linear address space 
		     ; (at the end of the 1st 4MB)
ECORE	equ 0FFC00000h ; End of USER's virtual address space (4GB - 4MB)
		     ; ULIMIT = (ECORE/4096) - 1 = 0FFBFFh (in GDT)
;; 27/12/2013
;KEND	equ KLOAD + 65536 ; (28/12/2013) (end of kernel space)
; 04/07/2016
KEND    equ KERNELFSIZE + KLOAD

; IBM PC/AT BIOS ----- 10/06/85 (postequ.inc)
;--------- CMOS TABLE LOCATION ADDRESS'S -------------------------------------
CMOS_SECONDS	EQU	00H		; SECONDS (BCD)
CMOS_SEC_ALARM	EQU	01H		; SECONDS ALARM (BCD)
CMOS_MINUTES	EQU	02H		; MINUTES (BCD)
CMOS_MIN_ALARM	EQU	03H		; MINUTES ALARM (BCD)
CMOS_HOURS	EQU	04H		; HOURS (BCD
CMOS_HR_ALARM	EQU	05H		; HOURS ALARM   (BCD)
CMOS_DAY_WEEK	EQU	06H		; DAY OF THE WEEK  (BCD)
CMOS_DAY_MONTH	EQU	07H		; DAY OF THE MONTH (BCD)
CMOS_MONTH	EQU	08H		; MONTH (BCD)
CMOS_YEAR	EQU	09H		; YEAR (TWO DIGITS) (BCD)
CMOS_CENTURY	EQU	32H		; DATE CENTURY BYTE (BCD)
CMOS_REG_A	EQU	0AH		; STATUS REGISTER A
CMOS_REG_B	EQU	0BH		; STATUS REGISTER B  ALARM
CMOS_REG_C	EQU	0CH		; STATUS REGISTER C  FLAGS
CMOS_REG_D	EQU	0DH		; STATUS REGISTER D  BATTERY
CMOS_SHUT_DOWN	EQU	0FH		; SHUTDOWN STATUS COMMAND BYTE
;----------------------------------------
;	CMOS EQUATES FOR THIS SYSTEM	;
;-----------------------------------------------------------------------------
CMOS_PORT	EQU	070H		; I/O ADDRESS OF CMOS ADDRESS PORT
CMOS_DATA	EQU	071H		; I/O ADDRESS OF CMOS DATA PORT
NMI		EQU	10000000B	; DISABLE NMI INTERRUPTS MASK -
					; HIGH BIT OF CMOS LOCATION ADDRESS

; Memory Allocation Table Address
; 05/11/2014
; 31/10/2014
MEM_ALLOC_TBL	equ	100000h		; Memory Allocation Table at the end of
					; the 1st 1 MB memory space.
					; (This address must be aligned
					;  on 128 KB boundary, if it will be
					;  changed later.)
					; ((lower 17 bits of 32 bit M.A.T.
					;   address must be ZERO)).
					; ((((Reason: 32 bit allocation
					;     instructions, dword steps)))
					; (((byte >> 12 --> page >> 5)))
;04/11/2014
PDE_A_PRESENT	equ	1		; Present flag for PDE
PDE_A_WRITE	equ 	2		; Writable (write permission) flag
PDE_A_USER	equ	4		; User (non-system/kernel) page flag
;
PTE_A_PRESENT	equ	1		; Present flag for PTE (bit 0)
PTE_A_WRITE	equ 	2		; Writable (write permission) flag (bit 1)
PTE_A_USER	equ	4		; User (non-system/kernel) page flag (bit 2)
PTE_A_ACCESS    equ	32		; Accessed flag (bit 5) ; 09/03/2015

; 17/02/2015 (unix386.s)
; 10/12/2014 - 30/12/2014 (0B000h -> 9000h) (dsectrm2.s)
DPT_SEGM equ 09000h  ; FDPT segment (EDD v1.1, EDD v3)
;
HD0_DPT	 equ 0	    ; Disk parameter table address for hd0
HD1_DPT	 equ 32	    ; Disk parameter table address for hd1
HD2_DPT	 equ 64	    ; Disk parameter table address for hd2
HD3_DPT	 equ 96	    ; Disk parameter table address for hd3

; 15/11/2020
VBE3INFOSEG equ 97E0h ; 512 bytes before Video_Pg_Backup
; 15/12/2020
VBE3MODEINFOSEG equ 97C0h ; 512 bytes before VBE3INFOBLOCK

; 29/11/2020
VBE3INFOBLOCK equ 97E00h ; linear address (512 bytes)
VBE3MODEINFOBLOCK equ 97C00h ; linear address (256 bytes)
VBE3SAVERESTOREBLOCK equ 97600h ; linear address (2048 bytes)
VBE3CRTCINFOBLOCK equ 97D80h ; linear address (64 bytes) ; 17/01/2021
VBE3BIOSDATABLOCK equ 97000h ; linear address (1536 bytes)
VBE3STACKADDR equ 96000h ; linear address (1024 bytes)
; VBE3 32 bit Protected Mode Interface (16 bit) Selectors (in GDT)
VBE3CS equ 30h ; _vbe3_CS:
VBE3BDS equ 38h ; _vbe3_BDS:
VBE3A000 equ 40h ; _A0000Sel:
VBE3B000 equ 48h ; _B0000Sel:
VBE3B800 equ 50h ; _B8000Sel:
VBE3DS equ 58h ; _vbe3_DS:
VBE3SS equ 60h ; _vbe3_SS:
VBE3ES equ 68h ; _vbe3_ES:
KCODE16 equ 70h ; _16bit_CS:
; 14/01/2021
; 06/12/2020
VBE3VIDEOSTATE equ 95800h ; 2048 bytes
; 05/01/2021
VGAFONT16USER equ 94000h ; 8x16 pixels user font (256 chars)
			 ; (reserved/allocated font space: 4096 bytes)

VGAFONT8USER equ 95000h	; 8x8 pixels user font (256 chars)
			; (reserved/allocated font space: 2048 bytes)
; 17/01/2021
; temporary (initial) location for EDID information
VBE3EDIDINFOBLOCK equ 97D00h ; linear address (128 bytes)
 
; FDPT (Phoenix, Enhanced Disk Drive Specification v1.1, v3.0)
;      (HDPT: Programmer's Guide to the AMIBIOS, 1993)
;
FDPT_CYLS	equ 0 ; 1 word, number of cylinders
FDPT_HDS	equ 2 ; 1 byte, number of heads
FDPT_TT		equ 3 ; 1 byte, A0h = translated FDPT with logical values
		      ; otherwise it is standard FDPT with physical values
FDPT_PCMP	equ 5 ; 1 word, starting write precompensation cylinder
		      ; (obsolete for IDE/ATA drives)
FDPT_CB		equ 8 ; 1 byte, drive control byte
			; Bits 7-6 : Enable or disable retries (00h = enable)
			; Bit 5	: 1 = Defect map is located at last cyl. + 1
			; Bit 4 : Reserved. Always 0
			; Bit 3 : Set to 1 if more than 8 heads
			; Bit 2-0 : Reserved. Always 0
FDPT_LZ		equ 12 ; 1 word, landing zone (obsolete for IDE/ATA drives)
FDPT_SPT	equ 14 ; 1 byte, sectors per track

; Floppy Drive Parameters Table (Programmer's Guide to the AMIBIOS, 1993)
; (11 bytes long) will be used by diskette handler/bios
; which is derived from IBM PC-AT BIOS (DISKETTE.ASM, 21/04/1986).

; 01/02/2016
Logical_DOSDisks equ 90000h + 100h ; 26*256 = 6656 bytes
Directory_Buffer equ 80000h ; max = 64K Bytes
FAT_Buffer	 equ 91C00h ; 1536 bytes (3 sectors)
; 15/02/2016
Cluster_Buffer	 equ 70000h ; max = 64K Bytes ; buffer for file read & write
; 11/04/2016
Env_Page:	 equ 93000h ; 512 bytes (4096 bytes)
Env_Page_Size	 equ 512    ; (4096 bytes)
; 30/07/2016
Video_Pg_Backup	 equ 98000h ; Mode 3h, video page backup (32K, 8 pages)

; 29/11/2020
; Free/Reserved memory blocks (in 1st 1MB): 93200h to 96000h (available)
; 06/12/2020
; Free/Reserved memory blocks (in 1st 1MB): 93200h to 95800h (available)

; 15/12/2020
LFB_ADDR	equ LFB_Info+LFBINFO.LFB_addr
LFB_SIZE	equ LFB_Info+LFBINFO.LFB_size

; 04/12/2023 - TRDOS 386 v2.0.7
SYSTEMSTACK_ADDR  equ 97000h	; max. 3072 bytes (96400h-97000h)
VBE3BIOSCODE_ADDR equ 60000h	; Protected Mode Video Bios (64KB)
;AC97DMABUFFR_ADDR equ 40000h	; AC97 & VIA VT8233 DMA Buffer (128KB)
SB16DMABUFFR_ADDR equ 50000h	; Sound Blaster 16 DMA Buffer (64KB)
sb16_dma_buffer equ SB16DMABUFFR_ADDR

; 29/08/2023 - TRDOS 386 v2.0.6
	; 30/11/2020
	; 29/11/2020 - TRDOS 386 v2.0.3

struc PMInfo  ;  VESA VBE3 PMInfoBlock ('PMID' block)

 .Signature:	resb 4  ; db 'PMID' ; PM Info Block Signature
 .EntryPoint:	resw 1	; Offset of PM entry point within BIOS
 .PMInitialize: resw 1	; Offset of PM initialization entry point
 .BIOSDataSel:	resw 1 	; Selector to BIOS data area emulation block
 .A0000Sel:	resw 1	; Selector to access A0000h physical mem
 .B0000Sel:	resw 1  ; Selector to access B0000h physical mem
 .B8000Sel:	resw 1	; Selector to access B8000h physical mem
 .CodeSegSel:	resw 1	; Selector to access code segment as data
 .InProtectMode: resb 1 ; Set to 1 when in protected mode
 .Checksum:	resb 1	; Checksum byte for structure
 .size:

endstruc
 
[BITS 16]       ; We need 16-bit intructions for Real mode

[ORG 0]
	; 12/11/2014
	; Save boot drive number (that is default root drive)
	mov	[boot_drv], dl ; physical drv number

	; Determine installed memory
	; 31/10/2014
	;
	mov	ax, 0E801h ; Get memory size 
	int	15h	   ; for large configurations
	jnc	short chk_ms
	mov	ah, 88h    ; Get extended memory size 
	int	15h
	;	   
	;mov	al, 17h	; Extended memory (1K blocks) low byte
	;out	70h, al ; select CMOS register
	;in	al, 71h ; read data (1 byte)
	;mov	cl, al
	;mov	al, 18h ; Extended memory (1K blocks) high byte
	;out	70h, al ; select CMOS register
	;in	al, 71h ; read data (1 byte)
	;mov	ch, al
 	;
	mov	cx, ax
	xor	dx, dx
chk_ms:
	mov	[mem_1m_1k], cx
	mov	[mem_16m_64k], dx
	; 24/11/2023
	mov	[real_mem_16m_64k], dx

	; 05/11/2014
	;and	dx, dx
	;jz	short L2
        cmp     cx, 1024
	;jnb	short L0
	jnb	short V0 ; 14/11/2020
		 ; insufficient memory_error
		 ; Minimum 2 MB memory is needed... 
	; 05/11/2014
	; (real mode error printing)
	sti
	mov	si, msg_out_of_memory
	mov	bx, 7
	mov	ah, 0Eh	; write tty
oom_1:
	lodsb
	or	al, al
	jz	short oom_2
	int	10h
	jmp	short oom_1
oom_2:
        hlt
	jmp	short oom_2

; 20/02/2017
; 05/11/2014
msg_out_of_memory:
	db 	07h, 0Dh, 0Ah
        db      'Insufficient memory !'
	db	0Dh, 0Ah
_int13h_48h_buffer: ; 07/07/2016
	db	'(Minimum 2MB memory is needed.)'
 	db	0Dh, 0Ah, 0
V0:
	; 18/10/2023 - TRDOS 386 v2.0.7
	; set video mode to 03h again 
	; (to reset video bios data in ROMBIOS DATA AREA)
	mov	ax, 3
	int	10h
	; copy IVT and ROMBIOS DATA AREA to VBE3 BIOS data area
	mov	di, VBE3BIOSDATABLOCK>>4
	mov	es, di
	xor	si, si
	xor	di, di
	mov	ds, si ; 0
	mov	cx, 300h ; 600h / 2
	rep	movsw
	push	cs
	pop	ds
	
	; 24/11/2023
	; 15/12/2020
	;mov	si, [mem_16m_64k]
	;mov	[real_mem_16m_64k], si
	
	; 15/11/2020
	; 14/11/2020 (TRDOS 386 v2.0.3)
	; check VESA (VBE) VIDEO BIOS version

	mov	ax, 4F03h  ; Return current VBE mode
	int	10h
	cmp	ax, 004Fh  ; successful (vbe) function call
	;jne	short L0   ; not a VESA VBE compatible bios
	; 18/10/2023
	jne	short V1   ; restore es

	; 27/11/2023
	; 24/11/2023 - temporary
	;jmp	short V1

	;mov	ah, 3
	;;jmp	short V1
	
	; 15/11/2020
	mov	bx, VBE3INFOSEG  ; 97E0h for current version 
	mov	es, bx
	xor	di, di
	mov	dword [es:di], 'VBE2' ; request VESA VBE3 info
		; es:di = buffer address (512 bytes)
	;mov	ax, 4F00h ; Return VBE controller information
	xchg	al, ah
	int	10h

	; dx = cs
	; es = VBE3INFOSEG (97E0h)
	; di = 0
	; ss = (endofkernelfile/16)+16
	; sp = 0FFFEh

	cmp	ax, 004Fh
	jne	short V1 ; old vga bios (not VESA compatible)

	; 15/11/2020
	cmp	dword [es:di], 'VESA'
	jne	short V1

	;mov	ax, [es:di+4]
	;	; ax = vbe version in BCD format (0200h or 0300h)
	;mov	[vbe3], ah ; version number (major)

	; 15/11/2020
	mov	al, [es:di+5]
		; al = high byte of VBE version number (02h or 03h)

	mov	[vbe3], al ; version number (major)
			   ; 02h or 03h is expected
	; 17/01/2021
	; Read EDID
	mov	bl, 01h	; Read EDID
	xor	cx, cx	; Controller unit number
			; (00 = primary controller)
	xor	dx, dx	; EDID block number = 0
	mov	ax, VBE3MODEINFOSEG  ; 97C0h for current version
	mov	es, ax
	mov	di, VBE3EDIDINFOBLOCK - VBE3MODEINFOBLOCK
	; es:di = temporary address of 128 bytes EDID information
	mov	ax, 4F15h ; VBE/DDC Services 
	int	10h
	;cmp	ax, 4Fh
	;jne	short v2
	mov	[edid], al ; 4Fh > 0
;V2:
	; 17/01/2021
	xor	di, di
	; 15/12/2020
	; Get linear frame buffer info (for VESA VBE mode 118h)
	;mov	si, VBE3MODEINFOSEG  ; 97C0h for current version
	;mov	es, si
 	; di = 0
	mov	cx, 04118h  ; 1024*768, 24 bpp, LFB
	mov	ax, 4F01h ; Return VBE mode information
	int	10h
	;cmp	ax, 4Fh
	;jne	short V1
	; 19/12/2020
	;mov	si, [es:di+MODEINFO.PhysBasePtr+2]
			; hw of LFB base address
	; MODEINFO structure starts from offset -2
	mov	si, [es:di+MODEINFO.PhysBasePtr] ; hw of LFB addr
	mov	[def_LFB_addr], si ; k_LFB_size = 3145728 bytes
	sub	si, 256
	
	; 15/12/2020
	; check memory and decrease it to 3.5 GB if it is 4GB
	; (reserve upper memory for LFB)
	mov	di, [mem_16m_64k]
	; 24/11/2023
	;mov	[real_mem_16m_64k], di

	cmp	di, si
	jna	short V1

	mov	[mem_16m_64k], si

	; VESA VBE3 video hardware
	; (example: NVIDIA GEFORCE FX550, 256 MB)
	; uses upper memory from 0D0000000h to 0DFFFFFFFh
	
	;;cmp	di, 0CF00h ; 3328 MB - 16MB
	;jna	short V1  ; <= 3328 MB memory, it is not required
			  ; decrease
	;cmp	al, 3
	;jb	short V2
	; VESA VBE 3
	;mov	word [mem_16m_64k], 0CF00h ; 3328 MB - 16MB
	;jmp	short V1
;V2:
	; VESA VBE 2
	; Check Bochs/Qemu/VirtualBox Emulator
	; LFB base address: 0E0000000h
	;sub	ax, ax ; 0
	;mov	dx, 1CEh ; VBE_DISPI_IOPORT_INDEX
	;out	dx, ax ; VBE_DISPI_INDEX_ID register
	;inc	dx
	;in	ax, dx
	;and	al, 0F0h
	;cmp	ax, 0B0C0h
	;jne	short V1
	;
	; BOCHS/QEMU/VIRTUALBOX
	;mov	word [mem_16m_64k], 0DF00h ; 3584 MB - 16MB
V1:
	push	ds
	pop	es ; restore extra data segment	
L0:

%include 'diskinit.s' ; 07/03/2015

	; 10/11/2014
     	cli	; Disable interrupts (clear interrupt flag)
		; Reset Interrupt MASK Registers (Master&Slave)
	;mov	al, 0FFh	; mask off all interrupts
	;out	21h, al		; on master PIC (8259)
	;jmp 	$+2  ; (delay)
	;out	0A1h, al	; on slave PIC (8259)
	;
	; Disable NMI
	mov   	al, 80h
	out   	70h, al		; set bit 7 to 1 for disabling NMI
	;23/02/2015
	;nop			;
	;in	al, 71h		; read in 71h just after writing out to 70h
				; for preventing unknown state (!?)
	;
 	; 20/08/2014
	; Moving the kernel 64 KB back (to physical address 0)
	; DS = CS = 1000h
	; 05/11/2014
	xor	ax, ax
	mov	es, ax ; ES = 0
	;
	; 04/07/2016 - TRDOS 386 (64K - 128K kernel)
      	xor	si, si
	xor	di, di
	;mov	cx, 16384
	;rep	movsd
	; 02/12/2023
	mov	cx, 32768
	rep	movsw
	;
	push	es ; 0
	push	L17
	retf
L17:
	mov	cx, 1000h
	mov	es, cx  ; 1000h
	add	cx, cx
	mov	ds, cx  ; 2000h
	;sub	si, si
	;sub	di, di
	;mov	cx, 16384
	;rep	movsd
	; 02/12/2023
	; si = di = 0
	mov	cx, 32768
	rep	movsw

	; Turn off the floppy drive motor
        mov     dx, 3F2h
        out     dx, al ; 0 ; 31/12/2013

	; Enable access to memory above one megabyte
L18:
	in	al, 64h
	test	al, 2
        jnz     short L18
	mov	al, 0D1h	; Write output port
	out	64h, al
L19:
	in	al, 64h
	test	al, 2
        jnz     short L19
	mov	al, 0DFh	; Enable A20 line
	out	60h, al
;L20:
	;
	; Load global descriptor table register

        ;mov     ax, cs
        ;mov     ds, ax

        lgdt    [cs:gdtd]

        mov     eax, cr0
	;or 	al, 1	; 24/07/2023
	inc     ax
	mov     cr0, eax

	; Jump to 32 bit code

	db 66h 			; Prefix for 32-bit
	db 0EAh 		; Opcode for far jump
	dd StartPM 		; Offset to start, 32-bit
				; (1000h:StartPM = StartPM + 10000h)
	dw KCODE		; This is the selector for CODE32_DESCRIPTOR,
				; assuming that StartPM resides in code32

; 20/02/2017


[BITS 32] 

StartPM:
	; Kernel Base Address = 0 ; 30/12/2013
	mov ax, KDATA           ; Save data segment identifier
        mov ds, ax              ; Move a valid data segment into DS register
       	mov es, ax              ; Move data segment into ES register
       	mov fs, ax              ; Move data segment into FS register
      	mov gs, ax              ; Move data segment into GS register
        mov ss, ax              ; Move data segment into SS register
	;mov esp, 90000h        ; Move the stack pointer to 090000h
	; 04/12/2023 - TRDOS 386 v2.0.7
	;mov esp, 97000h	; 3072 bytes system stack (96400h-97000h)
	mov esp, SYSTEMSTACK_ADDR ; 97000h (max. 3072 bytes)

clear_bss: ; Clear uninitialized data area
	; 11/03/2015
	xor	eax, eax ; 0
	mov	ecx, (bss_end - bss_start)/4
	;shr	ecx, 2 ; bss section is already aligned for double words
	mov	edi, bss_start
	rep	stosd

memory_init:
	; Initialize memory allocation table and page tables
	; 04/12/2023
	; 29/11/2023
	; 27/11/2023
	; 23/11/2023 (TRDOS 386 v2.0.7)
	; 24/07/2022 (TRDOS 386 v2.0.5)
	; 18/04/2021 (TRDOS 386 v2.0.4)
	; 16/11/2014
	; 15/11/2014
	; 07/11/2014
	; 06/11/2014
	; 05/11/2014
	; 04/11/2014
	; 31/10/2014 (Retro UNIX 386 v1 - Beginning)
	;
;	xor	eax, eax
;	xor 	ecx, ecx
	mov	cl, 8
	mov	edi, MEM_ALLOC_TBL	
	rep	stosd		   ; clear Memory Allocation Table
				   ; for the first 1 MB memory
	;
	mov	cx, [mem_1m_1k]	   ; Number of contiguous KB between
				   ; 1 and 16 MB, max. 3C00h = 15 MB.
	;shr	cx, 2		   ; convert 1 KB count to 4 KB count
	; 24/07/2022
	shr	ecx, 2
	mov	[free_pages], ecx
	mov	dx, [mem_16m_64k]  ; Number of contiguous 64 KB blocks
				   ; between 16 MB and 4 GB.
	or	dx, dx
	jz	short mi_0

; 04/12/2023
%if 1
	; 02/12/2023 - temporary (2816MB limit)
	;cmp	dx, 44800 ; 0AF00h
	cmp	dx, 40704	; (2560MB limit)
	jna	short mi_x
	;mov	dx, 44800
	mov	dx, 40704
mi_x:
%endif
	; 23/11/2023 - temporary
	;and	dx, 3FFFh

	mov	[mem_16m_64k], dx

	mov	ax, dx
	shl	eax, 4		   ; 64 KB -> 4 KB (page count)
	add	[free_pages], eax
	add	eax, 4096	   ; 16 MB = 4096 pages
	jmp	short mi_1
mi_0:
	;mov	ax, cx
	; 24/07/2022
	mov	eax, ecx
	add	ax, 256		   ; add 256 pages for the first 1 MB
	;add	eax, 256
mi_1:
	mov	[memory_size], eax ; Total available memory in pages
				   ; 1 alloc. tbl. bit = 1 memory page
				   ; 32 allocation bits = 32 mem. pages
	;
	add	eax, 32767	   ; 32768 memory pages per 1 M.A.T. page
	shr	eax, 15		   ; ((32768 * x) + y) pages (y < 32768)
				   ;  --> x + 1 M.A.T. pages, if y > 0
				   ;  --> x M.A.T. pages, if y = 0
	mov	[mat_size], ax	   ; Memory Alloc. Table Size in pages
	shl	eax, 12		   ; 1 M.A.T. page = 4096 bytes
	;			   ; Max. 32 M.A.T. pages (4 GB memory)
	mov	ebx, eax	   ; M.A.T. size in bytes
	; Set/Calculate Kernel's Page Directory Address
	add	ebx, MEM_ALLOC_TBL
	mov	[k_page_dir], ebx  ; Kernel's Page Directory address
				   ; just after the last M.A.T. page
	;
	sub	eax, 4		   ; convert M.A.T. size to offset value
	mov	[last_page], eax   ; last page offset in the M.A.T.
	;			   ; (allocation status search must be
				   ; stopped after here)
	xor	eax, eax
	dec	eax		   ; FFFFFFFFh (set all bits to 1)
	;push	cx
	; 18/04/2021
	push	ecx
	; ecx = 3840 ; 27/11/2023
	; (Note: ecx < 3840 if the total memory is less than 16 MB)
	shr	ecx, 5		   ; convert 1 - 16 MB page count to
				   ; count of 32 allocation bits
	; ecx = 120 ; 27/11/2023
	rep	stosd
	;pop	cx
	; 18/04/2021
	pop	ecx
	inc	eax		   ; 0
	and	cl, 31		   ; remain bits
	jz	short mi_4
	mov	[edi], eax	   ; reset
mi_2:
	bts	[edi], eax	   ; 06/11/2014
	dec	cl
	jz	short mi_3
	inc	al
	jmp	short mi_2
mi_3:
	sub	al, al	   	   ; 0
	add	edi, 4		   ; 15/11/2014
mi_4:
	or	dx, dx		  ; check 16 MB to 4 GB memory space
	jz	short mi_6	  ; max. 16 MB memory, no more...
	;
	mov	ecx, MEM_ALLOC_TBL + 512 ; End of first 16 MB memory
	;
	sub	ecx, edi	  ; displacement (to end of 16 MB)
	jz	short mi_5	  ; jump if EDI points to
				  ;         end of first 16 MB
	shr	ecx, 1		  ; convert to dword count
	shr	ecx, 1		  ; (shift 2 bits right)
	rep 	stosd		  ; reset all bits for reserved pages
				  ; (memory hole under 16 MB)
mi_5:
	mov	cx, dx		  ; count of 64 KB memory blocks
	shr	ecx, 1		  ; 1 alloc. dword per 128 KB memory
	pushf			  ; 16/11/2014
	dec	eax		  ; FFFFFFFFh (set all bits to 1)
	rep	stosd
	inc	eax		  ; 0
	popf			  ; 16/11/2014
	jnc	short mi_6
	dec	ax		  ; eax = 0000FFFFh
	stosd
	inc	ax		  ; 0		
mi_6:
	cmp	edi, ebx	  ; check if EDI points to
	jnb	short mi_7	  ; end of memory allocation table
	;			  ; (>= MEM_ALLOC_TBL + 4906)
	mov	ecx, ebx	  ; end of memory allocation table
	sub	ecx, edi	  ; convert displacement/offset
	shr	ecx, 1		  ; to dword count
	shr	ecx, 1		  ; (shift 2 bits right)
	rep 	stosd		  ; reset all remain M.A.T. bits
mi_7:
	; Reset M.A.T. bits in M.A.T. (allocate M.A.T. pages)
	mov	edx, MEM_ALLOC_TBL
	;sub	ebx, edx	  ; Mem. Alloc. Tbl. size in bytes
	;shr	ebx, 12		  ; Mem. Alloc. Tbl. size in pages
	mov	cx, [mat_size]	  ; Mem. Alloc. Tbl. size in pages
	mov	edi, edx
	shr	edi, 15		  ; convert M.A.T. address to
				  ; byte offset in M.A.T.
				  ; (1 M.A.T. byte points to
				  ;	      32768 bytes)
				  ; Note: MEM_ALLOC_TBL address
				  ; must be aligned on 128 KB
				  ; boundary!
	add	edi, edx	  ; points to M.A.T.'s itself
	; eax = 0
	sub	[free_pages], ecx ; 07/11/2014
mi_8:
	btr	[edi], eax	  ; clear bit 0 to bit x (1 to 31)
	;dec	bl
	dec	cl
	jz	short mi_9
	inc	al
	jmp	short mi_8
mi_9:
	;
	; Reset Kernel's Page Dir. and Page Table bits in M.A.T.
	;		(allocate pages for system page tables)

	; edx = MEM_ALLOC_TBL
	mov	ecx, [memory_size] ; memory size in pages (PTEs)
	add	ecx, 1023	 ; round up (1024 PTEs per table)
	shr	ecx, 10		 ; convert memory page count to
				 ; page table count (PDE count)
	;
	push	ecx		 ; (**) PDE count (<= 1024)
	;
	inc	ecx		 ; +1 for kernel page directory
	;
	sub	[free_pages], ecx ; 07/11/2014
	;
	mov	esi, [k_page_dir] ; Kernel's Page Directory address
	shr	esi, 12		 ; convert to page number
mi_10:
	mov	eax, esi	 ; allocation bit offset
	mov	ebx, eax
	shr	ebx, 3		 ; convert to alloc. byte offset
	and	bl, 0FCh	 ; clear bit 0 and bit 1
				 ;   to align on dword boundary
	and	eax, 31		 ; set allocation bit position 
				 ;  (bit 0 to bit 31)
	;
	add	ebx, edx	 ; offset in M.A.T. + M.A.T. address
	;
	btr 	[ebx], eax	 ; reset relevant bit (0 to 31)
	;
	inc	esi		 ; next page table
	loop	mi_10		 ; allocate next kernel page table
				 ; (ecx = page table count + 1)
	;
	pop	ecx		 ; (**) PDE count (= pg. tbl. count)
	;
	; Initialize Kernel Page Directory and Kernel Page Tables
	;
	; Initialize Kernel's Page Directory
	mov	edi, [k_page_dir]
	mov	eax, edi
	or	al, PDE_A_PRESENT + PDE_A_WRITE
				; supervisor + read&write + present
	mov	edx, ecx 	; (**) PDE count (= pg. tbl. count)
mi_11:
	add	eax, 4096	; Add page size (PGSZ)
			        ; EAX points to next page table
	stosd
	loop	mi_11
	sub	eax, eax	; Empty PDE
	;mov	cx, 1024	; Entry count (PGSZ/4)
	; 29/11/2023
	mov	ch, 4 ; cx = 4*256 = 1024
	sub	ecx, edx
	jz	short mi_12
	rep	stosd 		; clear remain (empty) PDEs
	;
	; Initialization of Kernel's Page Directory is OK, here.
mi_12:
	; Initialize Kernel's Page Tables
	;
	; (EDI points to address of page table 0)
	; eax = 0
	mov	ecx, [memory_size] ; memory size in pages
	mov	edx, ecx	; (***)
	mov	al, PTE_A_PRESENT + PTE_A_WRITE
			     ; supervisor + read&write + present
mi_13:
	stosd
	add	eax, 4096
	loop	mi_13
	;and	dx, 1023	; (***)
	; 30/08/2023
	and	edx, 1023
	jz	short mi_14
	;mov	cx, 1024
	; 30/08/2023
	mov	ch, 4 ; 4*256 = 1024
	;sub	cx, dx		; from dx (<= 1023) to 1024
	; 24/07/2022
	sub	ecx, edx
	xor	eax, eax
	rep	stosd		; clear remain (empty) PTEs
				; of the last page table
mi_14:
	;  Initialization of Kernel's Page Tables is OK, here.
	;
	mov	eax, edi	; end of the last page table page
			        ; (beginging of user space pages)

	shr	eax, 15		; convert to M.A.T. byte offset
	and	al, 0FCh	; clear bit 0 and bit 1 for
				; aligning on dword boundary
	mov	[first_page], eax
	mov	[next_page], eax ; The first free page pointer
				 ; for user programs
				 ; (Offset in Mem. Alloc. Tbl.)	
	;
	; Linear/FLAT (1 to 1) memory paging for the kernel is OK, here.
	;

	; Enable paging
	;
        mov     eax, [k_page_dir]
	mov	cr3, eax
	mov	eax, cr0
	or	eax, 80000000h	; set paging bit (bit 31)
	mov	cr0, eax
        ;jmp    KCODE:StartPMP

	db 0EAh 		; Opcode for far jump
        dd StartPMP		; 32 bit offset
	dw KCODE		; kernel code segment descriptor

StartPMP:
	; 06/11//2014
	; Clear video page 0
	;
	; Temporary Code
	;
	mov	ecx, 80*25/2
	mov	edi, 0B8000h
	; 30/01/2016
	;xor	eax, eax	; black background, black fore color
	mov	eax, 07000700h  ; black background, light gray fore color
	rep	stosd

	; 19/08/2014
	; Kernel Base Address = 0
	; It is mapped to (physically) 0 in the page table.
	; So, here is exactly 'StartPMP' address.

 	; 29/01/2016 (TRDOS 386 = TRDOS v2.0)
	mov	esi, starting_msg
	;; 14/08/2015 (kernel version message will appear
	;;	       when protected mode and paging is enabled)
	mov	edi, 0B8000h ; 27/08/2014

	; 30/11/2020
	; 14/11/2020 (TRDOS 386 v2.0.3)
	;cmp	byte [vbe3], 3 ; 03h
	;jne	short pkv_1
	;;mov	ah, 0Bh ; Black background, light cyan forecolor
	;; Light red TRDOS 386 version text shows VBE3 is ready !
	;mov	ah, 0Ch ; Black background, light red forecolor
	;jmp	short pkv_2
;pkv_1:
	mov	ah, 0Ah ; Black background, light green forecolor
;pkv_2:
	; 20/08/2014
	call	printk

	; 'UNIX v7/x86' source code by Robert Nordier (1999)
	; // Set IRQ offsets
	;
	;  Linux (v0.12) source code by Linus Torvalds (1991)
	;
					;; ICW1
	mov	al, 11h			; Initialization sequence
	out	20h, al			; 	8259A-1
	; jmp 	$+2
	out	0A0h, al		; 	8259A-2
					;; ICW2
	mov	al, 20h			; Start of hardware ints (20h)
	out	21h, al			;	for 8259A-1
	; jmp 	$+2
	mov	al, 28h			; Start of hardware ints (28h)
	out	0A1h, al		; 	for 8259A-2
					;
	mov	al, 04h			;; ICW3
	out	21h, al			; 	IRQ2 of 8259A-1 (master)
	; jmp 	$+2
	mov	al, 02h			; 	is 8259A-2 (slave)
	out	0A1h, al		;
					;; ICW4
	mov	al, 01h	 		;
	out	21h, al			; 	8086 mode, normal EOI
	; jmp 	$+2
	out	0A1h, al		;	for both chips.

	;mov	al, 0FFh	; mask off all interrupts for now
	;out	21h, al
	;; jmp 	$+2
	;out	0A1h, al

	; 02/04/2015
	; 26/03/2015 System call (INT 30h) modification
	;  DPL = 3 (Interrupt service routine can be called from user mode)
	;
	;; Linux (v0.12) source code by Linus Torvalds (1991)
	;  setup_idt:
	;
        ;; 16/02/2015
	;;mov     dword [DISKETTE_INT], fdc_int ; IRQ 6 handler
	; 21/08/2014 (timer_int)
	mov	esi, ilist
	lea	edi, [idt]
	; 26/03/2015
	mov	ecx, 48		; 48 hardware interrupts (INT 0 to INT 2Fh)
	; 02/04/2015
	mov	ebx, 80000h
rp_sidt1:
	lodsd
	mov	edx, eax
	mov	dx, 8E00h
	mov	bx, ax
	mov	eax, ebx	; /* selector = 0x0008 = cs */
       			        ; /* interrupt gate - dpl=0, present */
	stosd	; selector & offset bits 0-15 	
	mov	eax, edx
	stosd	; attributes & offset bits 16-23
	loop	rp_sidt1
	; 15/04/2016
	; TRDOS 386 (TRDOS v2.0) /// 32 sofware interrupts ///
	;mov	cl, 16        ; 16 software interrupts (INT 30h to INT 3Fh)
	mov	cl, 32	      ; 32 software interrupts (INT 30h to INT 4Fh)
rp_sidt2:
	lodsd
	and	eax, eax
	jz	short rp_sidt3
	mov	edx, eax
	mov	dx, 0EE00h	; P=1b/DPL=11b/01110b
	mov	bx, ax
	mov	eax, ebx	; selector & offset bits 0-15
	stosd
	mov	eax, edx
	stosd
	loop	rp_sidt2
	jmp	short sidt_OK
rp_sidt3:
	mov	eax, ignore_int
	mov	edx, eax
	mov	dx, 0EE00h	; P=1b/DPL=11b/01110b
	mov	bx, ax
	mov	eax, ebx	; selector & offset bits 0-15
rp_sidt4:
	stosd
	xchg	eax, edx
	stosd
	xchg	edx, eax
	loop	rp_sidt4
sidt_OK: 
	lidt 	[idtd]
	;
	; TSS descriptor setup ; 24/03/2015
	mov	eax, task_state_segment
	mov	[gdt_tss0], ax
	rol	eax, 16
	mov	[gdt_tss1], al
	mov	[gdt_tss2], ah
	mov	word [tss.IOPB], tss_end - task_state_segment
		; 
		; IO Map Base address (When this address points
		; to end of the TSS, CPU does not use IO port
		; permission bit map for RING 3 IO permissions,
		; access to any IO ports in ring 3 will be forbidden.)
 		;
	;mov	[tss.esp0], esp ; TSS offset 4
	;mov	word [tss.ss0], KDATA ; TSS offset 8 (SS)
   	mov	ax, TSS  ; It is needed when an interrupt 
			 ; occurs (or a system call -software INT- is requested)
			 ; while cpu running in ring 3 (in user mode).
			 ; (Kernel stack pointer and segment will be loaded
			 ; from offset 4 and 8 of the TSS, by the CPU.)
	ltr	ax  ; Load task register
	;
esp0_set0:

; 29/11/2023 - Erdogan Tan
; ------------------------
; If we read following -disabled- stack page setting code...
; When the memory size >= 3GB, one of page tables conflicts with the stack page
; at 4MB-4096 address. So, to leave stack pointer at 90000h is better/default.
; (Problem may not appears for <= 2.5GB main memory but following code is also
; defective because kernel stack page would be seen as unallocated in the M.A.T.
; without by adding a memory allocation code.)
;
; 1st 4MB layout: 1MB kernel -base- reserved + max. 128 KB M.A.T. (at 100000h)
;                 + Kernel's page directory (4KB) -just after the M.A.T.-
;		  + Kernel's page tables (main memory size / 1024)
; Note:
; 1 or 2 additional kernel page table(s) may be needed for Linear Frame Buffer
; .. but, it/they will not have to be contiguous with other kernel page tables.
; ------------------------

; 27/11/2023 - TRDOS 386 v2.0.7
%if 0
	; 30/07/2015
	mov 	ecx, [memory_size] ; memory size in pages
	shl 	ecx, 12 ; convert page count to byte count
	cmp	ecx, CORE ; beginning of user's memory space (400000h)
			  ; (kernel mode virtual address)
	jna	short esp0_set1
	;
	; If available memory > CORE (end of the 1st 4 MB)
	; set stack pointer to CORE
	;(Because, PDE 0 is reserved for kernel space in user's page directory)
	;(PDE 0 points to page table of the 1st 4 MB virtual address space)
	mov	ecx, CORE
esp0_set1:
	mov	esp, ecx ; top of kernel stack (**tss.esp0**)
%endif

esp0_set_ok:
	; 30/07/2015 (**tss.esp0**) 
	mov	[tss.esp0], esp	; 90000h ; 29/11/2023
				; <-- 97000h ; 04/12/2023 (max. 3072 bytes)
        mov     word [tss.ss0], KDATA
	; 14/08/2015
	; 10/11/2014 (Retro UNIX 386 v1 - Erdogan Tan)
	;
	;cli	; Disable interrupts (for CPU)
	;    (CPU will not handle hardware interrupts, except NMI!)
	;
	xor	al, al		; Enable all hardware interrupts!
	out	21h, al		; (IBM PC-AT compatibility)
	jmp 	$+2		; (All conventional PC-AT hardware
	out	0A1h, al	;  interrupts will be in use.)	
				; (Even if related hardware component
				;  does not exist!)
	; Enable NMI 
	mov	al, 7Fh		; Clear bit 7 to enable NMI (again)
	out  	70h, al
	; 23/02/2015
	nop
	in	al, 71h		; read in 71h just after writing out to 70h
				; for preventing unknown state (!?)
	;
	; Only a NMI can occur here... (Before a 'STI' instruction)
	;
	; 02/09/2014
	;xor	bx, bx
	; 24/07/2022
	xor	ebx, ebx
	mov	dx, 0200h	; Row 2, column 0  ; 07/03/2015
	call	_set_cpos	; 24/01/2016

	; 14/11/2020 (TRDOS 386 v2.0.3)
	; Check VBE3 protected mode interface/feature(s)

	;cmp	byte [vbe3], 3 ; 03h
	;jne	short display_mem_info

	; 20/11/2020
	cmp	byte [vbe3], 2 ; 02h
	ja	short vbe3_pmid_chk
	;;jb	short display_mem_info
	;jb	display_mem_info ; 02/12/2020
	jb	short jmp_display_mem_info ; 24/07/2022
	jmp	check_boch_plex86_vbe

vbe3_pmid_chk:
	mov	ecx, 32768 - (20+2) ; 32766 - PMInfoBlockSize
	mov	esi, 0C0002h ; 1st word of the video bios rom is 0AA55h

chk_pmi_sign:
	;mov	eax, [esi]
	;cmp	eax, 'PMID'
	; 30/11/2020
	;cmp	al, 'P'
	;jne	short chk_pmi_sign_next
	cmp	dword [esi], 'PMID'
	;je	short display_vbios_product_name
	je	short verify_pmib_chksum ; 15/11/2020
;chk_pmi_sign_next:
	inc	esi  ; inc si
	loop	chk_pmi_sign

not_valid_pmib:
	dec	byte [vbe3] ; 2 = VBE2 compatible 
			    ; (vbe3 feature is defective in this vbios)
	;jmp	short display_mem_info
jmp_display_mem_info:	; 24/07/2022
	; 02/12/2020
	jmp	display_mem_info

verify_pmib_chksum:
	; 18/10/2023
	; (ATI RV370 video bios contains ZERO at Checksum offset.)
	xor	eax, eax
	cmp	byte [esi+19], al ; 0 ; Checksum byte
	jna	short skip_verify_pmib_chksum ; may be ATI video bios
	;cmp	dword [esi+16], 0C000h
	;	; CodeSegSel = 0C000h, InProtectMode = 0, Checksum = 0
	;je	short skip_verify_pmib_chksum ; may be ATI video bios
	; 18/10/2023
	; 15/11/2020
	;xor	eax, eax
	;;mov	ecx, eax
	;mov	cl, 20
	mov	cx, 20 ; 30/11/2020
	push	esi
pmib_sum_bytes:
	lodsb
	add	ah, al
	loop	pmib_sum_bytes
	pop	esi
	or	ah, ah
	jnz	short not_valid_pmib ; AH must be 0

	; 18/10/2023
	xor	al, al  ; eax = 0

	; 28/02/2021
	; Set default (initial) truecolor bpp value to 32
	; (for VBE3 video bios.. because vbe3 video bioses
	;  use 32bpp -for truecolor modes- instead of 24bpp)
	; (This setting may be changed via 'sysvideo' bx=0908h)

skip_verify_pmib_chksum: ; 18/10/2023

	mov	byte [truecolor], 32 ; (RGB: 00RRGGBBh)

display_vbios_product_name: ; 14/11/2020

	; ESI points to 'PMID' (0C0000h + 'PMID' offset)

	; 15/11/2020
	;mov	[pmid_addr], si	; PMInfoBlock offset
	;		; (in VGA bios, 0C0000h + offset)
	; 02/12/2020
	;push	esi ; * pmid_addr
	mov	edi, esi

	;mov	esi, [VBE3INFOBLOCK+22] ; 097E00h + 16h
	;		; OemVendorNamePtr (seg16:off16)
	mov	esi, [VBE3INFOBLOCK+6] ; 097E00h + 06h
			; OemStringPtr (seg16:off16)
	; 18/10/2023
	;xor	al, al  ; eax = 0
	xchg	ax, si	; ax = offset, si = 0
	shr	esi, 12 ; (to convert segment to base addr)
	add	si, ax  ; esi has an address < 1 MB limit
			; (OemVendorName is in VBE3INFOBLOCK)
			; Example: 
			; TRDOS 386 v2.0.3 VESA VBE3 protected mode
			; interface development reference is ...
			; NVIDIA GeForce FX5500 VGA BIOS -C000h:029Ch-
			; Version 4.34.20.54.00 -C000h:02EDh-
			; ((OemString is 'NVIDIA'))
			; ((OemVendorName is 'NVIDIA Corporation'))
			; ((OemProductName is 'NV34 Board - p162-1nz))

	;mov	ah, 0Eh ; Black background, yellow forecolor
	; 30/11/2020
	mov	ah, 0Ch ; Black background, light red forecolor

	call	print_kmsg

	;mov	ah, 07h

	mov	esi, vesa_vbe3_bios_msg
	;call	print_kmsg
	call	pkmsg_loop ; 30/11/2020

	; 02/12/2020
	;pop	edi ; * pmid_addr

; 04/12/2023 - TRDOS 386 v2.0.7
%if 0
	; 24/07/2022
	; 29/11/2020
vbe3pminit:
	; 30/11/2020
	;cmp	byte [vbe3], 3 ; is VESA VBE3 PMI ready ?
	;jne	short di4

	; Allocate 64KB contiguous (kernel) memory block
	xor	eax, eax
	mov	ecx, 65536
	call	allocate_memory_block
	;jc	short di4
	;jc	di0 ; 30/11/2020
	; 24/07/2022
	jnc	short vbe3pminit0
	jmp	di0

vbe3pminit0:
	; of course this block must be in the 1st 16MB
	; because vbe3 pmi segments will be 16 bit segments
	; (80286 type segment descriptors in GDT)

	mov	[vbe3bios_addr], eax
%endif

	; 04/12/2023 - TRDOS 386 v2.0.7	; (+!*!+)
	; fixed PM-VBIOS address (no need to add a memory block)
	; in the reserved -and already allocated- area under 1MB
	; (purpose: to prevent page faults if memory size > 2.5GB)
	; ((User's page dir contains only the 1st 4MB of system mem
	; as PDE. So, this causes to page faults during an interrupt
	; in user mode, because if memory size > 2.5GB, kernel
	; page tables overs/passes 4MB limit and PM-VBIOS
	; is located after kernel page tables.))
vbe3pminit:
	mov	eax, VBE3BIOSCODE_ADDR ; 60000h	; (+!*!+)

	; set [pmid_addr] to the new location
	mov	esi, 0C0000h

	; 30/11/2020
	sub	edi, esi ; izolate offset
	add	edi, eax ; new address
	mov	[pmid_addr], edi ; new 'PMID' location

	; Move VIDEO BIOS from 0C0000h to EAX
	mov	ecx, 65536/4
	mov	edi, eax ; 30/11/2020
	rep	movsd

	; 02/12/2020
	; 30/11/2020
	; set vbe3 segment selectors

; 04/12/2023 - TRDOS 386 v2.0.7	; (+!*!+)
%if 0
	; VBE3CS (VESA VBE3 video bios code segment)
	mov	edi, _vbe3_CS+2 ; base address bits 0..15
	stosw	; edi = _vbe3_CS+4
	ror	eax, 16
	mov	[edi], al ; base address, bits 16..23

	; VBE3DS ('CodeSegSel' in PMInfoBlock)
	mov	edi, _vbe3_DS+4 ; base addr bits 16..23
	mov	[edi], al
	rol	eax, 16
	mov	[edi-2], ax ; base address, bits 0..15
%endif
	; VBE3BDS (BIOSDataSel in PMInfoBlock)
	mov	edi, _vbe3_BDS+2 ; base addr bits 0..15
	mov	eax, VBE3BIOSDATABLOCK ; 1536 bytes
	stosw	; edi = _vbe3_BDS+4
	shr	eax, 16
	mov	[edi], al ; base address, bits 16..23

	; VBE3SS (1024 bytes)
	mov	edi, _vbe3_SS+2 ; base addr bits 0..15
	mov	eax, VBE3STACKADDR ; size = 1024 bytes
	stosw	; edi = _vbe3_SS+4
	shr	eax, 16
	mov	[edi], al ; base address, bits 16..23

	; stack pointer (esp) will be set to 1020
	; (before VBE3 PMI call)

	; VBE3ES (max: 2048 bytes)
	mov	edi, _vbe3_ES+2 ; base addr bits 0..15
	mov	eax, VBE3SAVERESTOREBLOCK
	stosw	; edi = _vbe3_ES+4
	shr	eax, 16
	mov	[edi], al ; base address, bits 16..23

	;Note: low word of _VBE3_ES base address will be
	;      set -again- by VBE3 PMI caller routine

	; 09/12/2020
	;; set pmi32 (as VBE3 PMI is ready)
	;inc	byte [pmi32] ; = 1

	; KCODE16 (set PMI far return segment)
	mov	edi, _16bit_CS+2 ; base addr bits 0..15
	mov	eax, pminit_return_addr16
	stosw	; edi = _16bit_CS+4
	shr	eax, 16
	mov	[edi], al ; base address, bits 16..23

	; 30/11/2020
	; clear mem from VBE3 BIOS data area emu block
	; to end of vbe3 buffers

	; 18/10/2023 - TRDOS v2.0.7
	; (VBE3BIOSDATABLOCK contains a copy of the 1st
	; 1536 bytes of the memory, IVT, ROMBIOS DATA etc.)
	; ((it must not be cleared here))
%if 0
	; 01/12/2020
	mov	edi, VBE3BIOSDATABLOCK ; 97000h
	;mov	cx, (VBE3INFOBLOCK-VBE3BIOSDATABLOCK)/4
	;	; ecx = 3584/4 double words
	; 21/12/2020
	mov	cx, (VBE3MODEINFOBLOCK-VBE3BIOSDATABLOCK)/4
		; ecx = 3072/4 double words
	;xor	eax, eax
	xor	al, al
	rep	stosd
%endif
	; 18/10/2023 - TRDOS v2.0.7
	; (VBE3BIOSDATABLOCK contains a copy of the 1st
	; 1536 bytes of the memory, IVT, ROMBIOS DATA etc.)
	; ((it must not be cleared here))
	mov	edi, VBE3SAVERESTOREBLOCK ; 97600h
	mov	cx, (VBE3MODEINFOBLOCK-VBE3SAVERESTOREBLOCK)/4
		 ; ecx = 1536/4 = 384 double words
	;xor	eax, eax
	xor	al, al
	rep	stosd	

	; Filling PMInfoBlock selector fields
	mov	edi, [pmid_addr]
	mov	word [edi+PMInfo.BIOSDataSel], VBE3BDS
	mov	word [edi+PMInfo.A0000Sel], VBE3A000
	mov	word [edi+PMInfo.B0000Sel], VBE3B000
	mov	word [edi+PMInfo.B8000Sel], VBE3B800
	mov	word [edi+PMInfo.CodeSegSel], VBE3DS
	mov	byte [edi+PMInfo.InProtectMode], 1

	; Calculate and write checksum byte
	mov	esi, edi
	mov	cl, PMInfo.size - 1
	;xor	ah, ah
pmid_chksum:
	lodsb
	add	ah, al
	loop	pmid_chksum
	neg	ah ; 1 -> 255, 255 -> 1
	mov	[esi], ah  ; checksum

	; far call PM initialization
	; (VBE3 video bios will return via 'retf')

	mov	ax, [edi+PMInfo.PMInitialize]
	; 30/11/2020
	shl	eax, 16 ; save entry address in hw
	; ax = 0 

	; 02/12/2020
	push	pminit_ok ; normal, near return address

	; 30/11/2020
_VBE3PMI_fcall:
	; ax = function, hw of eax = entry address
	pushf	; save 32 bit flags
	push	esi ; *
	push	ebp ; **

	mov	ebp, esp ; save 32 bit stack pointer

	mov	esi, eax

	cli

	; Disable interrupts (clear interrupt flag)
	; Reset Interrupt MASK Registers (Master&Slave)
	mov	al, 0FFh	; mask off all interrupts
	out	21h, al		; on master PIC (8259)
	jmp 	$+2  ; (delay)
	out	0A1h, al	; on slave PIC (8259)

	; 02/12/2020
	mov	ax, VBE3SS
	mov	ss, ax

	mov	esp, 1020 ; 30/11/2020

	; 01/12/2020
	;lss	esp, [stack16]

	shr	eax, 16	; now, entry address is in lw

	; 30/11/2020 - 16 bit pm selector test (OK)
	; (32 bit stack push/pop & retf with 32 bit code segment)
	; (16 bit stack push/pop with 16 bit code segment)

	; return
	;push	KCODE16
	;push	0 ; 30/11/2020 (pminit_return_addr16)

	; 30/11/2020 (16 bit stack during retf from video bios)
	mov	dword [esp], KCODE16 << 16
				; ip = 0, cs = KCODE16
	; 01/12/2020
	;mov	dword [VBE3STACKADDR+1020], KCODE16*65536

	;mov	[jumpfar16], eax

	; 02/12/2020
	; 30/11/2020 (32 bit stack during retf from kernel)
	; far jump/call via retf
	push	VBE3CS ; VBE3 video bios's code segment
	push	eax ; PMInitialize or EntryPoint

	;mov	ax, si ; restore function
	; 24/07/2022
	mov	eax, esi

	; 02/12/2020
	xor	esi, esi ; (not necessary, it is not used)

	retf 	; far return (to 16 bit code segment)

	; 01/12/2020
	;db	0EAh  ; far jump to 16 bit code segment
;jumpfar16:
	;dd	0
	;dw	VBE3CS

;stack16:
	;dd	1020
	;dw	VBE3SS

	align 2	

pminit_return_addr16:
	; 02/12/2020
	; 30/11/2020
	;;db	66h 		     ; Prefix for 32-bit
	;db	0EAh 		     ; Opcode for far jump
	;dd	pminit_return_addr32 ; 32 bit Offset
	;dw	KCODE		     ; 32 bit code segment
	; 01/12/2020
	jmp	KCODE:pminit_return_addr32

pminit_return_addr32:
	; restore 32 bit kernel selectors and 32 bit stack addr
	mov	esi, KDATA
	mov	ds, si
	mov	es, si
	mov	ss, si
	mov	esp, ebp  ; top of stack = iretd return addr

	pop	ebp ; **
	pop	esi ; *
	popf	; restore 32 bit flags

	; enable interrupts

	cli

	; 21/12/2020
	push	eax

	xor	al, al	   ; Enable all hardware interrupts!
	out	21h, al	   ; (IBM PC-AT compatibility)
	jmp 	$+2	   ; (All conventional PC-AT hardware
	out	0A1h, al   ;  interrupts will be in use.)
			   ; (Even if related hardware component
			   ;  does not exist!)
	pop	eax

	sti

	; top of stack = return address
	; ('pminit_ok' for PMinit)

	retn

pminit_ok:
	; 03/12/2020
	; (set [pmid_addr] to PMI entry point for next calls)
	add	dword [pmid_addr], PMInfo.EntryPoint ; + 4

	; 17/01/2021
	; copy EDID data from temporary location to final address
	cmp	byte [edid], 4Fh
	jne	short vbe3h_chcl
	;mov	ecx, 32 ; 128 bytes, 32 dwords
	; 24/07/2022
	xor	ecx, ecx
	mov	cl, 32
	mov	esi, VBE3EDIDINFOBLOCK ; 97D00h
	mov	edi, edid_info
	rep	movsd
	; 17/01/2021
vbe3h_chcl:
	; 16/01/2021
	;; 06/12/2020
	;; Save video mode 03h regs/dac/bios state
	;
	;mov	ax, 4F04h ; VESA VBE Function 04h
	;		  ; Save/Restore State
	;sub	dl, dl	; 0 = return buffer size
	;mov	cx, 0Fh  ; ctrl/bios/dac/regs
	;
	;call	int10h_32bit_pmi
 	;; bx = number of 64-byte blocks to hold the state buff
	;	
	;;mov	[vbe3stbufsize], bx
	;; 16/01/2021
	;or	word [vbe3stbsflags], 32768  ; set bit 15
	;mov	ax, bx
	;shl	ax, 6  ; * 64
	;mov	[vbestatebufsize+30], ax
	;
	;; 06/12/2020
	;; check 'vbe3stbufsize' (it must be <= 32)
	;
	;cmp	bx, 32
	;ja	short display_mem_info ; light red forecolor
	;
	;; 16/01/2021
	;or	byte [vbe3stbsflags], 1 ; set bit 0

	; 30/11/2020
	; Change VESA VBE3 BIOS text color in order to give
	; "VBE3 PMI initialization has been successed" meaning

	mov	esi, 0B8000h + 160*2 ; row 2
	mov	edi, esi
vbe3h_chcl_next:
	lodsw
	cmp	ah, 0Ch	; light red forecolor
	jne	short display_mem_info
	mov	ah, 0Eh ; yellow forecolor
	stosw
	jmp	short vbe3h_chcl_next

di5:
	; 18/10/2023 - TRDOS 386 v2.0.7
	; ATI RV370 Video BIOS (VESA VBE2)
	; has PMI. (as described in VESA VBE3 specification)
	;
	inc	byte [vbe3]  ; [vbe3] = 2 -> 3
			; will be decreased to 2 again if 'PMID'
			; signature will not be found. 	
	jmp	vbe3_pmid_chk ; check for ATI Video BIOS

di0:
	; 30/11/2020
	; Memory allocation error !
	mov	byte [vbe3], 0 ; disable VBE3

display_mem_info:
	; 19/12/2020
	; temporary
	; 24/11/2023
	cmp	byte [vbe3], 2
	jb	short dmi
	call	default_lfb_info
dmi:
	; 06/11/2014
	call	memory_info
	; 14/08/2015
	;call	getch ; 28/02/2015

	; 07/12/2023
	; check EDID info for LCD monitor -screen resolution-
	;	for modifying VGA mode 13h CRTC parameters
	;	(if it is needed or not)

	call	video_mode_13h_parms

drv_init:
	sti	; Enable Interrupts
	; 06/02/2015
	mov	edx, [hd0_type] ; hd0, hd1, hd2, hd3
	mov	bx, [fd0_type] ; fd0, fd1
	; 22/02/2015
	and	bx, bx
	jnz	short di1
	;
	or 	edx, edx
	jnz	short di2
	;
setup_error:
	mov 	esi, setup_error_msg
psem:	
	lodsb
	or	al, al
	;jz	short haltx ; 22/02/2015
	jz	short di3
	push	esi
	; 13/05/2016
	mov	ebx, 7	; Black background,
			; light gray forecolor
			; Video page 0 (BH=0)
	call	_write_tty
	pop	esi
	jmp	short psem

check_boch_plex86_vbe:
	; 20/10/2023
	; 18/10/2023
	; 20/11/2020
	; check Bochs/Plex86 VGABios VBE extension
	; (check if TRDOS 386 v2 is running on emulators)
	; BOCHS/QEMU/VIRTUALBOX
	;
	; ref: vbe_display_api.txt

	; bochs/plex86 VGAbios VBE source code
	;  by Jeroen Janssen (2002)
	;  and Volker Ruppert (2003-2020)

	; 20/10/2023
	; (ATI RV370 video bios has PMI support but
	; it is a VESA VBE2 bios. So, even if [vbe3] is set
	; to 3 for PMI functionality, it is better to display
	; VESA VBE number as it is declared by the video bios.)

	mov	byte [vbe_vnumber], "2" ; 20/10/2023

	sub	eax, eax ; 0
	mov	dx, 1CEh ; VBE_DISPI_IOPORT_INDEX
	out	dx, ax ; VBE_DISPI_INDEX_ID register
	;mov	ax, 0B0C0h ; VBE_DISPI_ID0
	;mov	dx, 1CFh ; VBE_DISPI_IOPORT_DATA
	inc	dx
	;out	dx, ax
	;nop
	in	ax, dx
	cmp	ah, 0B0h
	;jne	short not_boch_qemu_vbe
	; 18/10/2023
	;jne	short display_mem_info
	jne	short di5  ; ATI VESA VBE2 bios or another

	cmp	al, 0C5h ; it must be 0B0C4h or 0B0C5h ..
	;ja	short not_boch_qemu_vbe
	ja	short display_mem_info
	cmp	al, 0C0h ; 0B0C0h to 0B0C5h .. ; Qemu
	;cmp	al, 0C4h ; 0BC04h or 0B0C5h is OK ; Bochs
	;jb	short not_boch_qemu_vbe
	jb	short display_mem_info

	; save VESA VBE2 bios (bochs/qemu) signature
	; for enabling VBE2 functions in TRDOS 386 v2 kernel
	mov	[vbe2bios], al ; 0C4h or 0C5h (for BOCHS) 
			       ; (0C0h-0C5h for QEMU)
	; 20/10/2023
	;mov	byte [vbe_vnumber], "2"

	; 26/11/2020
	; "BOCHS/QEMU/VIRTUALBOX VBE2 Video BIOS ..".
	mov	esi, vbe2_bochs_vbios ; BOCH/QEMU vbios msg
	mov	ah, 0Eh  ; Yellow font
	call	print_kmsg

	; this is not necessary ! (20/11/2020)
	cmp	byte [vbe2bios], 0C4h
	;jb	display_mem_info  ; (QEMU)
	; 02/12/2023
	jb	short not_boch_qemu_vbe

	; Display kernel version message if 0E9h hack port
	; is enabled (bochs emulator feature)
	mov	dx, 0E9h ; hack port for BOCHS
	mov	esi, kernel_version_msg
kvmsg_next_char:
	lodsb
	or	al, al
	jnz	short put_kvmsg_in_hack_port
not_boch_qemu_vbe:
 	jmp	display_mem_info
put_kvmsg_in_hack_port:	
	out	dx, al
	jmp	short kvmsg_next_char

di1:
	; supress 'jmp short T6'
	;  (activate fdc motor control code)
	mov	word [T5], 9090h ; nop
	;
	;mov	ax, int_0Eh	; IRQ 6 handler
	;mov	di, 0Eh*4	; IRQ 6 vector
	;stosw
	;mov 	ax, cs
	;stosw
	;; 16/02/2015
        ;;mov     dword [DISKETTE_INT], fdc_int ; IRQ 6 handler
	;
	CALL	DSKETTE_SETUP	; Initialize Floppy Disks
	;
	or	edx, edx
        jz      short di3
di2:
	call   	DISK_SETUP	; Initialize Fixed Disks
        ;jc	setup_error
	; 24/07/2022
	jnc	short di3
	jmp	setup_error
di3:
	call	setup_rtc_int	; 22/05/2015 (dsectrpm.s)
	;
	call	display_disks ; 07/03/2015  (Temporary)
;haltx:
	; 14/08/2015
	;call	getch ; 22/02/2015
	;sti	; Enable interrupts (for CPU)
;	; 29/01/2016
;	sub	ah, ah ;  read time count
;	call	int1Ah
;	mov	edx, ecx ; 18.2 * seconds
;md_info_msg_wait1:
;	; 29/01/2016
;	mov	ah, 1
;	call	int16h
;	jz	short md_info_msg_wait2
;	xor	ah, ah ; 0
;       call    int16h
;	jmp 	short md_info_msg_ok
;md_info_msg_wait2:
;	sub	ah, ah  ; read time count
;	call	int1Ah
;	cmp	edx, ecx ; ; 18.2 * seconds
;	jna	short md_info_msg_wait3
;	xchg 	edx, ecx
;md_info_msg_wait3:
;	sub	ecx, edx
;	cmp	ecx, 127 ; 7 seconds (18.2 * 7)
;	jb	short md_info_msg_wait1
;md_info_msg_ok:

	; 15/12/2020
	; set initial values of LFB parameters

	cmp	byte [vbe3], 2
	jb	short di4

	;mov	ax, [def_LFB_addr]
	;shl	eax, 16
	;mov	[LFB_ADDR], eax
	;mov	eax, 1024*768*3
	;mov	[LFB_SIZE], eax

	mov	esi, VBE3MODEINFOBLOCK - 2
	mov	word [esi], 0118h ; default vbe mode
				  ; 1024*768, 24bpp
	call	set_lfbinfo_table

	;;;
	; 28/11/2023
	; 20/10/2023 - TRDOS 386 v2.0.7
	mov	esi, [LFB_ADDR] ; LFB base address (in bytes)
	;mov	edx, [LFB_SIZE] ; 28/11/2023
	;add	edx, 4095
	;;;
	;mov	edx, ((1024*768*4)+4095)>>12 ; 28/11/2023
	mov	edx, ((1920*1080*4)+4095)>>12 ; 28/11/2023
	; edx = LFB size in pages

	; 29/11/2023
	;mov	ebx, esi
	;;add	ebx, 4095 ; LFB start addr is always in page boundary
	;shr	ebx, 12	; convert byte address to page address
	mov	ecx, [memory_size]
	;cmp	ebx, ecx
	;jnb	short di4 ; LFB addr >= main memory size

	; (set the overlapped pages as allocated for kernel)
	;;;
	; 28/11/2023
	; (and set all of LFB pages in the kernel's page tables)

	call	allocate_lfb_pages_for_kernel
di4:
	; 08/09/2016
	mov	eax, cr0
	test	al, 10h  ; Bit 4, ET (Extension Type)
	jz	short sysinit
	; 27/02/2017
	inc	byte [fpready]
	; 80387 (FPU) is ready
	fninit ; Initialize Floating-Point Unit
sysinit:
	; 30/06/2015
	call	sys_init
	;
	;jmp 	cpu_reset ; 22/02/2015
hang:
	; 23/02/2015
	;sti			; Enable interrupts
	hlt
	;
	;nop
	;; 03/12/2014
	;; 28/08/2014
	;mov	ah, 11h
	;call	getc
	;jz      _c8
	;
	; 23/02/2015
	; 06/02/2015
	; 07/09/2014
	xor	ebx, ebx
	mov	bl, [ptty]	; active_page
	mov	esi, ebx
	;shl 	si, 1
	; 24/07/2022
	shl	esi, 1
	add	esi, ttychr
	mov	ax, [esi]
	and	ax, ax
	;jz	short _c8
	jz	short hang
	mov	word [esi], 0
	cmp	bl, 3		; Video page 3
	;jb	short _c8
	jb	short hang
	;
	; 13/05/2016
	; 07/09/2014
nxtl:
	;push	bx
	; 18/04/2021
	push	ebx
	mov	bx, 0Eh 	; Yellow character
				; on black background
				; bh = 0 (video page 0)
				; Retro UNIX 386 v1 - Video Mode 0
				; (PC/AT Video Mode 3 - 80x25 Alpha.)
	;push	ax
	; 18/04/2021
	push	eax
	call 	_write_tty
	;pop	ax
	; 18/04/2021
	pop	eax
	;pop	bx
	pop	ebx
	cmp	al, 0Dh		; carriage return (enter)
	;jne	short _c8
	jne	short hang
	mov	al, 0Ah		; next line
	jmp	short nxtl

;_c8:
;	; 25/08/2014
;	cli			; Disable interrupts
;	mov	al, [scounter + 1]
;	and	al, al
;	jnz	hang
;	call	rtc_p
;	jmp     hang

	; 27/08/2014
	; 20/08/2014
printk:
        ;mov    edi, [scr_row]
pkl:
	lodsb
	or 	al, al
	jz	short pkr
	stosw
	jmp	short pkl
pkr:
	retn

; *************************************
video_mode_13h_parms:
	; 07/12/2023 - TRDOS 386 v2.0.7
	; Check EDID information for LCD monitors
	; and change mode 13h parameters if it is required
	; (if resolution > 1280x1024, it is LCD/panel monitor
	; and Video Mode 13h parameters will be modified for 60HZ
	; because LCD monitors does/can not display 320x200 70HZ
	; standard VGA mode)

	cmp	byte [edid], 4Fh
	jne	short CRT_monitor

	mov	esi, edid_info
	add	esi, 26h ; EDID Standard Timing Identification
	mov	ecx, 8
chk_edid:
	lodsw
	cmp	al, 129	; (1280/8)-31
	ja	short LCD_monitor ; 16:9
	cmp	al, 1
	jna	short CRT_monitor
	loop	chk_edid
	;jmp	short CRT_monitor
	retn
mode13h_crtc_60hz:
	db	0Bh, 3Eh, 0B9h, 85h, 8Fh, 0B8h, 0E2h
LCD_monitor:
	; modify default (CRTC register) parameters for 60HZ
	; (320x200 letterbox type screen will appear on LCD screen)
	;
	; Note: When/While [PMI32] -protected mode video bios-
	;	feature is enabled -by user- for MODE 13h, internal
	;	(TRDOS 386) VGA -video mode setting- parameters
	;	will not be used (will be bypassed)
	;	by TRDOS 386 kernel for all standard VGA modes.
	;	((NVIDIA/ATI Video Bios PMI will be used.))
	;
	; 	ref: github, juj/60hz.cpp
	;	     https://gist.github.com/juj/
	;
	mov	cl, 7
	mov	esi, mode13h_crtc_60hz
	mov	edi, vga_mode_13h+10+6 ; CRTC Registers (index: 6)
	movsb		; Vertical Total Register
	movsb		; Overflow Register
	mov	edi, vga_mode_13h+10+16 ; CRTC Regs (index: 16)
	movsb		; Vertical Retrace Start Register
	movsb		; Vertical Retrace End Register
	movsb		; Vertical Display Enable/End Register
	inc	edi
	inc	edi
	movsb		; Vertical Blanking Start Register
	movsb		; Vertical Blanking End Register
CRT_monitor:
	retn
; *************************************

; 14/11/2020 (TRDOS 386 v2.0.3)
vbe3:	db 0  ; VESA VBE version (must be 03h)
	      ; for using video bios calls in protected mode
vbe2bios:
	db 0B0h ;
;pmid_addr:
	;dw 0  ; > 0 if 'PMID' sign is found 
	;     ;	('pmid' offset addr in VGA bios seg, 0C000h)
	;; 02/12/2020
	;dw 0	; 32 bit address in pmid_addr

; 28/02/2017
; 22/01/2017
; 15/01/2017
; 14/01/2017
; 02/01/2017
; 25/12/2016
; 19/12/2016
; 10/12/2016 (callback)
; 06/06/2016
; 23/05/2016
; 22/05/2016 - TRDOS 386 (TRDOS v2.0) Timer Event Modifications
; 25/07/2015
; 14/05/2015 (multi tasking -time sharing- 'clock', x_timer)
; 17/02/2015
; 06/02/2015 (unix386.s)
; 11/12/2014 - 22/12/2014 (dsectrm2.s) 
;
; IBM PC-XT Model 286 Source Code - BIOS2.ASM (06/10/85)
;
;-- HARDWARE INT  08 H - ( IRQ LEVEL 0 ) ---------------------------------------
;	THIS ROUTINE HANDLES THE TIMER INTERRUPT FROM FROM CHANNEL 0 OF        :
;	THE 8254 TIMER.  INPUT FREQUENCY IS 1.19318 MHZ AND THE DIVISOR        :
;	IS 65536, RESULTING IN APPROXIMATELY 18.2 INTERRUPTS EVERY SECOND.     :
;									       :
;	THE INTERRUPT HANDLER MAINTAINS A COUNT (40:6C) OF INTERRUPTS SINCE    :
;	POWER ON TIME, WHICH MAY BE USED TO ESTABLISH TIME OF DAY.	       :
;	THE INTERRUPT HANDLER ALSO DECREMENTS THE MOTOR CONTROL COUNT (40:40)  :
;	OF THE DISKETTE, AND WHEN IT EXPIRES, WILL TURN OFF THE 	       :
;	DISKETTE MOTOR(s), AND RESET THE MOTOR RUNNING FLAGS.		       :
;	THE INTERRUPT HANDLER WILL ALSO INVOKE A USER ROUTINE THROUGH	       :
;	INTERRUPT 1CH AT EVERY TIME TICK.  THE USER MUST CODE A 	       :
;	ROUTINE AND PLACE THE CORRECT ADDRESS IN THE VECTOR TABLE.	       :
;-------------------------------------------------------------------------------
;

timer_int:	; IRQ 0
;int_08h:	; Timer
	; 14/10/2015
	; Here, we are simulating system call entry (for task switch)
	; (If multitasking is enabled,
	; 'clock' procedure may jump to 'sysrelease')

	push	ds
	push	es
	push	fs
	push	gs

	pushad	; eax, ecx, edx, ebx, esp -before pushad-, ebp, esi, edi
	mov     cx, KDATA
        mov     ds, cx
        mov     es, cx
        mov     fs, cx
        mov     gs, cx

	mov	ecx, cr3
	mov	[cr3reg], ecx ; save current cr3 register value/content

	; 14/01/2017
	cmp 	ecx, [k_page_dir]
	je	short T3

	mov	ecx, [k_page_dir]
	mov	cr3, ecx
T3:
	;sti				; INTERRUPTS BACK ON
	INC	word [TIMER_LOW]	; INCREMENT TIME
	JNZ	short T4		; GO TO TEST_DAY
	INC	word [TIMER_HIGH]	; INCREMENT HIGH WORD OF TIME
T4:					; TEST_DAY
	CMP	word [TIMER_HIGH],018H	; TEST FOR COUNT EQUALING 24 HOURS
	JNZ	short T5		; GO TO DISKETTE_CTL
	CMP	word [TIMER_LOW],0B0H
	JNZ	short T5		; GO TO DISKETTE_CTL

;-----	TIMER HAS GONE 24 HOURS
	;;SUB	AX,AX
	;MOV	[TIMER_HIGH],AX
	;MOV	[TIMER_LOW],AX
	sub	eax, eax
	mov	[TIMER_LH], eax
	;
	MOV	byte [TIMER_OFL],1

;-----	TEST FOR DISKETTE TIME OUT

T5:
	; 23/12/2014
	jmp	short T6		; will be replaced with nop, nop
					; (9090h) if a floppy disk
					; is detected.
	;mov	al,[CS:MOTOR_COUNT]
	mov	al, [MOTOR_COUNT]
	dec	al
	;mov	[CS:MOTOR_COUNT], al	; DECREMENT DISKETTE MOTOR CONTROL
	mov	[MOTOR_COUNT], al
	;mov	[ORG_MOTOR_COUNT], al
	JNZ	short T6		; RETURN IF COUNT NOT OUT
	mov 	al,0F0h
	;AND	[CS:MOTOR_STATUS],al 	; TURN OFF MOTOR RUNNING BITS
	and	[MOTOR_STATUS], al
	;and	[ORG_MOTOR_STATUS], al
	MOV	AL,0CH			; bit 3 = enable IRQ & DMA,
					; bit 2 = enable controller
					;	1 = normal operation
					;	0 = reset
					; bit 0, 1 = drive select
					; bit 4-7 = motor running bits 
	MOV	DX,03F2H		; FDC CTL PORT
	OUT	DX,AL			; TURN OFF THE MOTOR
T6:
	;inc	word [CS:wait_count]	; 22/12/2014 (byte -> word)
					; TIMER TICK INTERRUPT
	;;inc	word [wait_count] ;;27/02/2015
	;INT	1CH			; TRANSFER CONTROL TO A USER ROUTINE
	;cli
	call 	u_timer			; TRANSFER CONTROL TO A USER ROUTINE
	; 23/05/2016
	call	clock			; Multi Tasking control procedure
T7:
	; 14/10/2015
	MOV	AL,EOI			; GET END OF INTERRUPT MASK
	CLI				; DISABLE INTERRUPTS TILL STACK CLEARED
	OUT	INTA00,AL		; END OF INTERRUPT TO 8259 - 1
	;
	;;;
	; 21/08/2024
	cmp	byte [p_change], 0FFh ; CTRL+BREAK signature
	je	short T9 ; current program will be forced to sysrelease
	;;;
rtc_int_2:
	; 26/12/2016
	;mov	ecx, [cr3reg]
	; 13/01/2017
	cmp	byte [u.t_lock], 0  	; T_LOCK
	ja	short timer_int_return  ; Timer Lock : 'sysrele' is needed !
	; 28/02/2017
	; We need to exit if the user's IRQ callback service is in progress!
	; (To prevent a conflict!)
	cmp	byte [u.r_lock], 0	; R_LOCK, IRQ callback service lock !
	ja	short timer_int_return  ; Timer Lock : 'sysrele' is needed !
	; 15/01/2017
	cmp	byte [priority], 2
	jnb	short T8  ; current process has a timer event (15/01/2017)
	; 22/05/2016
	cmp	byte [p_change], 0 ; in 'set_run_sequence', in 'rtc_p'
	jna	short timer_int_return ; 23/05/2016

	; 15/01/2017
	; present process must be changed with high priority process
T9:		; 21/08/2024
	;xor	al, al
	xor	eax, eax ; 26/12/2016
	mov	[p_change], al ; 0
	;mov	byte [priority], 2 ; 15/01/2017 (there is a timer event)

	cmp     byte [sysflg], 0FFh ; user or system space ?
	je	short rtc_int_3     ; user space ([sysflg]= 0FFh)

	; system space, wait for 'sysret'
	; to change running process
	; with high priority (event) process

	mov	[u.quant], al ; 0

timer_int_return: ; 23/05/2016 - jump from 'rtc_int' ('rtc_int_2')
	mov 	ecx, [cr3reg] 	; previous value/content of cr3 register
 	mov	cr3, ecx	; restore cr3 register content
	;
	popad ; edi, esi, ebp, temp (icrement esp by 4), ebx, edx, ecx, eax
	;
	pop	gs
	pop	fs
	pop	es
	pop	ds
	;
	iretd	; return from interrupt

rtc_int_3:
	inc	byte [sysflg] 	; now, we are in system space
	;
        jmp     sysrelease ; change running process immediatelly 

T8:
	; 13/01/2017 (eax -> ebx)
	; callback checking... (19/12/2016)
	xor	ebx, ebx
	xchg	ebx, [u.tcb] ; callback address (0 = normal return)
	or	ebx, ebx
	jz	short timer_int_return

	; Set user's callback routine as return address from this interrupt
	; and set normal return address as return address from callback
	; routine!!! (19/12/2016)

	; 14/01/2017
	; 13/01/2017 - Timer Lock (T_LOCK)
	inc	byte [u.t_lock]
	mov	cl, [sysflg]
	mov	[u.t_mode], cl 

	mov	ebp, [tss.esp0] ; kernel stack address (for ring 0)
	sub	ebp, 20		; eip, cs, eflags, esp, ss
 	mov	[u.sp], ebp
	mov	[u.usp], esp

	;or	word [ebp+8], 200h ; 22/01/2017, force enabling interrupts

	mov	eax, [esp+28] ; pushed eax
	mov	[u.r0], eax

	call	wswap ; save user's registers & status

	; software int is in ring 0 but timer int must return to ring 3
	; so, ring 3 return address and stack registers
	; (eip, cs, eflags, esp, ss) 
	; must be copied to timer int return
	; eip will be replaced by callback service routine address

	mov	byte [sysflg], 0FFh ; user mode

	; system mode (system call)
	;mov	ebp, [u.sp] ; EIP (u), CS (UCODE), EFLAGS (u),
			    ; ESP (u), SS (UDATA)

	mov	eax, [ebp+16]	; SS (UDATA
	mov	esi, esp
	push	eax
	push	eax
	mov	edi, esp
	mov	[u.usp], edi
	mov	ecx, ((ESPACE/4) - 4) ; except DS, ES, FS, GS
	rep	movsd
	mov	cl, 4
	rep	stosd
	mov	[u.sp], edi
	mov	esi, ebp
	mov	cl, 5 ; EIP (u), CS (UCODE), EFLAGS (u), ESP (u), SS (UDATA)
	rep	movsd

	mov	ecx, [u.pgdir]
	mov	[cr3reg], ecx

	; 13/01/207 (eax -> ebx)
	; EBX = callback routine address (virtual, not physical address!)

	; 09/01/2017
	; !!! CALLBACK ROUTINE MUST BE ENDED/RETURNED WITH 'sysrele'
	;     system call !!!
	; 25/12/2016
	; Callback Note: (19/12/2016)
	; !!! CALLBACK ROUTINE MUST BE ENDED/RETURNED WITH 'RETN' !!!
	;	pushf ; save flags
	; 	<callback service code>
	; 	popf  ; restore flags
	; 	retn ; return to normal running address
	;

	; 15/01/2017
	; 14/01/2017
	; 13/01/2017 (eax -> ebx)
	; 10/01/2017
set_callback_addr:
	; 09/01/2017 (**)
	; 02/01/2017 (*)
	; 25/12/2016 (*)
	; 19/12/2016 (TRDOS 386 feature only!)
	;
	; This routine sets return address
	; to start of user's interrupt
	; service (callback) address
	;; and sets callback 'retn' address to normal
	;; return address of user's running code! 
	;
	; INPUT:
	;	EBX = callback routine/service address
	;	      (virtual, not physical address!)
	;	[u.sp] = kernel stack, points to
	;		 user's EIP,CS,EFLAGS,ESP,SS
	;		 registers.
	; OUTPUT:
	;	EIP (user) = callback (service) address
	;	CS (user) = UCODE
	;	EFLAGS (user) = flags before callback
	;       ESP (user) = ESP-4 (user, before callback)
	;	[ESP](user) = EIP (user) before callback
	;
	; Note: If CPU was in user mode while entering
	;	the timer interrupt service routine,
	;	'IRET' will get return to callback routine
	;	immediately. If CPU was in system/kernel mode
	;	'iret' will get return to system call and
	;	then, callback routine will be return address
	;	from system call. (User's callback/service code
	;	will be able to return to normal return address
	;	via an 'retn' at the end.) 
	;
	; Note(**): User's callback service code must be ended
	;	with a 'sysrele' sytstem call ! (09/01/2017)
	;
	;	For example:
	;
	;	timer_callback:
	;	    ...
  	;	    inc	dword [time_counter]
	;	    ...
	;	    mov eax, 39 ; 'sysrele'
	;	    int 40h ; TRDOS 386 system call (interrupt)
	;
	;
	;; Note(*): User's callback service code must preserve cpu 
	;;	flags if it has any instructions which changes
	;;	flags in the service code. (25/12/2016)
	;;
	;;	For example:
	;;
	;;	timer_callback:
	;;	    pushf ; save flags
	;;	    ; this instruction changes zero flag
  	;;	    inc	dword [time_counter]
	;;	    popf ; restore flags
	;;	    retn ; return to normal user code
	;;		  (which is interrupted by the 
	;;		   timer interput)
	;;

	; 15/01/2017
	mov	ebp, [u.sp]; kernel's stack, points to EIP (user)
	mov	[ebp], ebx
	jmp	timer_int_return

	; 15/01/2017
	; 13/01/2017
	; 19/12/2016
	; 06/06/2016
	; 23/05/2016
	; 22/05/2016
	; 19/05/2016 - TRDOS 386 (TRDOS v2.0)
	; 26/02/2015
	; 07/09/2014
	; 25/08/2014
rtc_int:       ; Real Time Clock Interrupt (IRQ 8)
	; 22/05/2016
	push	ds ; ** ; 23/05/2016
	push	eax ; *
	mov	ax, KDATA
	mov	ds, ax
	;
	mov	ah, [RTC_2Hz] ;  2 Hz interrupt to 1 Hz function
	xor	ah, 1
	mov	[RTC_2Hz], ah ; 1 = 0.5 second, 0 = 1 second
	jnz	short rtc_int_return ; half second
	; 1 second
rtc_int_0:
	; 22/05/2016
	pop	eax ; *
	;
	; 14/10/2015 ('timer_int')
	; Here, we are simulating system call entry (for task switch)
	; (If multitasking is enabled, 
	; 'clock' procedure may jump to 'sysrelease')
	;push	ds ; ** ; 23/05/2016
	push	es
	push	fs
	push	gs
	pushad  ; eax, ecx, edx, ebx, esp -before pushad-, ebp, esi, edi
	mov     cx, KDATA
        ;mov    ds, cx ; 06/06/2016
        mov     es, cx
        mov     fs, cx
        mov     gs, cx
	;
	mov	ecx, cr3
	mov	[cr3reg], ecx ; save current cr3 register value/content
	;
	cmp	byte [u.t_lock], 0 ; timer lock (callback) status ?
	ja	short rtc_int_1	   ; yes

	; 15/01/2017
	cmp 	ecx, [k_page_dir]
	je	short rtc_int_1

	mov	ecx, [k_page_dir]
	mov	cr3, ecx
rtc_int_1:
	; Timer event (kernel) functions must be performed with
	; 1 second intervals - TRDOS 386 (TRDOS v2.0) feature ! -
 	;	
	; 25/08/2014
	call	rtc_p  ; 19/05/2016 - major modification 

	; 23/05/2016
	sub	ah, ah ; 0
	; 22/05/2016 - TRDOS 386 timer event modifications
rtc_int_return: ; 19/05/2016
	; 22/02/2015 - dsectpm.s
	; [ source: http://wiki.osdev.org/RTC ]
	; read status register C to complete procedure
	;(it is needed to get a next IRQ 8) 
	mov	al, 0Ch ; 
	out	70h, al ; select register C
	nop
	in	al, 71h ; just throw away contents
	; 22/02/2015
	MOV	AL,EOI		; END OF INTERRUPT
	;CLI			; DISABLE INTERRUPTS TILL STACK CLEARED
	OUT	INTB00,AL	; FOR CONTROLLER #2

	; 23/05/2016
	MOV	AL,EOI		; GET END OF INTERRUPT MASK
	CLI			; DISABLE INTERRUPTS TILL STACK CLEARED
	OUT	INTA00,AL	; END OF INTERRUPT TO 8259 - 1
	;
	; 23/05/2016
	and	ah, ah
	;jz	rtc_int_2
	; 24/07/2022
	jnz	short rtc_int_4
	jmp	rtc_int_2
rtc_int_4:
	; ah = 1 (half second)
	pop	eax ; *
	pop	ds  ; **
	iretd

; ////////////////

	; 28/08/2014
irq0:
        push 	dword 0
	jmp	short which_irq
irq1:
        push 	dword 1
	jmp	short which_irq
irq2:
        push 	dword 2
	jmp	short which_irq
irq3:
	; 20/11/2015
	; 24/10/2015
	call	dword [cs:com2_irq3]
	push 	dword 3
	jmp	short which_irq
irq4:
	; 20/11/2015
	; 24/10/2015
	call	dword [cs:com1_irq4]
        push 	dword 4
	jmp	short which_irq
irq5:
        push 	dword 5
	jmp	short which_irq
irq6:
        push 	dword 6
	jmp	short which_irq
irq7:
        push 	dword 7
	jmp	short which_irq
irq8:
        push 	dword 8
	jmp	short which_irq
irq9:
        push 	dword 9
	jmp	short which_irq
irq10:
        push 	dword 10
	jmp	short which_irq
irq11:
        push 	dword 11
	jmp	short which_irq
irq12:
        push 	dword 12
	jmp	short which_irq
irq13:
        push 	dword 13
	jmp	short which_irq
irq14:
        push 	dword 14
	jmp	short which_irq
irq15:
        push 	dword 15
	;jmp	short which_irq

	; 22/01/2017
	; 19/10/2015
	; 29/08/2014
	; 21/08/2014
which_irq:
	xchg	eax, [esp]  ; 28/08/2014
	push	ebx
	push	esi
	push	edi
	push 	ds
	push 	es
	;
	mov	bl, al
	;
	mov	eax, KDATA
	mov	ds, ax
	mov	es, ax
	; 19/10/2015
	cld
        ; 27/08/2014
        add     dword [scr_row], 0A0h
	;
	mov	ah, 17h	; blue (1) background,
			; light gray (7) forecolor
        mov     edi, [scr_row]
	mov	al, 'I'
	stosw
	mov	al, 'R'
	stosw
	mov	al, 'Q'
	stosw
	mov	al, ' '
	stosw
	mov	al, bl
	cmp	al, 10
	jb	short ii1
	mov	al, '1'
	stosw
	mov	al, bl
	sub	al, 10
ii1:
	add	al, '0'
	stosw
	mov	al, ' '
	stosw
	mov	al, '!'
	stosw
	mov	al, ' '
	stosw
	; 23/02/2015
	cmp	bl, 7 ; check for IRQ 8 to IRQ 15
	jna	ii2
	; 22/01/2017
	mov	al, 20h  ; END OF INTERRUPT COMMAND TO
	out	0A0h, al ; the 2nd 8259
ii2:
	mov	al, 20h  ; END OF INTERRUPT COMMAND TO
	out	20h, al ; the 2nd 8259
	jmp     iiret
	;
	; 22/08/2014
	;mov	al, 20h ; END OF INTERRUPT COMMAND TO 8259
	;out	20h, al	; 8259 PORT
	;
	;pop	es
	;pop	ds
	;pop	edi
	;pop	esi
	;pop	ebx
	;pop 	eax
	;iret

	; 02/04/2015
	; 25/08/2014
exc0:
        push 	dword 0
        jmp     cpu_except
exc1:
        push 	dword 1
        jmp     cpu_except
exc2:
        push 	dword 2
        jmp     cpu_except
exc3:
        push 	dword 3
        jmp     cpu_except
exc4:
        push 	dword 4
        jmp     cpu_except
exc5:
        push 	dword 5
        jmp     cpu_except
exc6:
        push 	dword 6
        jmp     cpu_except
exc7:
        push 	dword 7
        jmp     cpu_except
exc8:
	; [esp] = Error code
        push 	dword 8
        jmp     cpu_except_en
exc9:
        push 	dword 9
        jmp     cpu_except
exc10:
	; [esp] = Error code
        push 	dword 10
        jmp     cpu_except_en
exc11:
	; [esp] = Error code
        push 	dword 11
        jmp     cpu_except_en
exc12:
	; [esp] = Error code
        push 	dword 12
        jmp     cpu_except_en
exc13:
	; [esp] = Error code
        push 	dword 13
        jmp     cpu_except_en
exc14:
	; [esp] = Error code
        push 	dword 14
	jmp	short cpu_except_en
exc15:
        push 	dword 15
        jmp     cpu_except
exc16:
        push 	dword 16
        jmp     cpu_except
exc17:
	; [esp] = Error code
        push 	dword 17
	jmp	short cpu_except_en
exc18:
        push 	dword 18
	jmp	short cpu_except
exc19:
        push 	dword 19
	jmp	short cpu_except
exc20:
        push 	dword 20
	jmp	short cpu_except
exc21:
        push 	dword 21
	jmp	short cpu_except
exc22:
        push 	dword 22
	jmp	short cpu_except
exc23:
        push 	dword 23
	jmp	short cpu_except
exc24:
        push 	dword 24
	jmp	short cpu_except
exc25:
        push 	dword 25
	jmp	short cpu_except
exc26:
        push 	dword 26
	jmp	short cpu_except
exc27:
        push 	dword 27
	jmp	short cpu_except
exc28:
        push 	dword 28
	jmp	short cpu_except
exc29:
        push 	dword 29
	jmp	short cpu_except
exc30:
        push 	dword 30
	jmp	short cpu_except_en
exc31:
        push 	dword 31
        jmp     short cpu_except

	; 19/10/2015
	; 19/09/2015
	; 01/09/2015
	; 28/08/2015
	; 28/08/2014
cpu_except_en:
	xchg	eax, [esp+4] ; Error code
	mov	[ss:error_code], eax
	pop	eax  ; Exception number
	xchg	eax, [esp]
		; eax = eax before exception
		; [esp] -> exception number
		; [esp+4] -> EIP to return
	; 22/01/2017
	; 19/10/2015
	; 19/09/2015
	; 01/09/2015
	; 28/08/2015
	; 29/08/2014
	; 28/08/2014
	; 25/08/2014
	; 21/08/2014
cpu_except:	; CPU Exceptions
	cld
	xchg	eax, [esp]
		; eax = Exception number
		; [esp] = eax (before exception)
	push	ebx
	push	esi
	push	edi
	push 	ds
	push 	es
	; 28/08/2015
	mov	bx, KDATA
	mov	ds, bx
	mov	es, bx
	mov	ebx, cr3
	push	ebx ; (*) page directory
	; 19/10/2015
	cld
	; 25/03/2015
	mov	ebx, [k_page_dir]
	mov	cr3, ebx
	; 28/08/2015
	cmp	eax, 0Eh ; 14, PAGE FAULT
	jne	short cpu_except_nfp
	call	page_fault_handler
	and 	eax, eax
	;jz	iiretp ; 01/09/2015
	; 24/07/2022
	jnz	short cpu_except_pf
	jmp	iiretp
cpu_except_pf:	; 24/07/2022
	mov	al, 0Eh ; 14
cpu_except_nfp:
	; 23/08/2016
	cmp	byte [CRT_MODE], 3
	je	short cpu_except_mode_3
	push	eax
	mov	al, 3
	call	_set_mode
	pop	eax
cpu_except_mode_3:
	; 02/04/2015
	mov	ebx, hang
	xchg	ebx, [esp+28]
		; EIP (points to instruction which faults)
	  	; New EIP (hang)
	mov	[FaultOffset], ebx
	mov	dword [esp+32], KCODE ; kernel's code segment
	or	dword [esp+36], 200h ; enable interrupts (set IF)
	;
	mov	ah, al
	and	al, 0Fh
	cmp	al, 9
	jna	short h1ok
	add	al, 'A'-':'
h1ok:
	shr	ah, 4
	cmp	ah, 9
	jna	short h2ok
	add	ah, 'A'-':'
h2ok:	
	xchg 	ah, al
	add	ax, '00'
	mov	[excnstr], ax
	;
	; 29/08/2014
	mov	eax, [FaultOffset]
	push	ecx
	push	edx
	mov	ebx, esp
	; 28/08/2015
	mov	ecx, 16	; divisor value to convert binary number
			; to hexadecimal string
	;mov	ecx, 10	; divisor to convert
			; binary number to decimal string
b2d1:
	xor	edx, edx
	div	ecx
	;push	dx
	; 18/04/2021
	push	edx
	cmp	eax, ecx
	jnb	short b2d1
	mov	edi, EIPstr ; EIP value
			    ; points to instruction which faults	
	; 28/08/2015
	mov	edx, eax
b2d2:
	;add	al, '0'
	mov	al, [edx+hexchrs]
	stosb		    ; write hexadecimal digit to its place
	cmp	ebx, esp
	jna	short b2d3
	;pop	ax
	; 18/04/2021
	pop	eax
	mov	dl, al
	jmp	short b2d2
b2d3:
	mov 	al, 'h' ; 28/08/2015
	stosb
	mov	al, 20h	    ; space
	stosb
	xor	al, al	    ; to do it an ASCIIZ string	
	stosb
	;
	pop	edx
	pop	ecx
	;
	mov	ah, 4Fh	; red (4) background,
			; white (F) forecolor
	mov	esi, exc_msg ; message offset
	;
	; 20/01/2017 (!cpu exception!)
	;
        add	dword [scr_row], 0A0h
        mov	edi, [scr_row]
	;
	mov	byte [sysflg], 0  ; system mode
        sti
	;
	call 	printk
	;
	mov	ah, 10h
	call	int16h ; getc
	;
	mov	al, 3
	call	_set_mode
	;
	;mov	eax, 1
	; 30/11/2023
	xor	eax, eax
	inc	eax
	; eax = 1
	jmp	sysexit ; terminate process !!!
	
	; 22/01/2017
	; 18/04/2016
	; 28/08/2015
	; 23/02/2015
	; 20/08/2014
ignore_int:
	push	eax
	push	ebx ; 23/02/2015
	push	esi
	push	edi
	push 	ds
	push 	es
	; 18/04/2016
	mov	ax, KDATA
	mov	ds, ax
	mov	es, ax
	; 28/08/2015
	mov	eax, cr3
	push	eax ; (*) page directory
	;
	mov	ah, 67h	; brown (6) background,
			; light gray (7) forecolor
	mov	esi, int_msg ; message offset
piemsg:
        ; 27/08/2014
        add     dword [scr_row], 0A0h
        mov     edi, [scr_row]
        ;
	call 	printk
	;
	; 23/02/2015
	mov	al, 20h  ; END OF INTERRUPT COMMAND TO
	out	0A0h, al ; the 2nd 8259
	; 22/08/2014
	mov	al, 20h ; END OF INTERRUPT COMMAND TO 8259
	out	20h, al	; 8259 PORT
iiretp: 
	; 22/01/2017
	; 01/09/2015
	; 28/08/2015
	pop	eax ; (*) page directory
	mov	cr3, eax
iiret:
	pop	es
	pop	ds
	pop	edi
	pop	esi
	pop	ebx ; 29/08/2014
	pop 	eax
	iretd

	; 23/05/2016
	; 22/08/2014
	; IBM PC/AT BIOS source code ----- 10/06/85 (bios.asm)
	; (INT 1Ah)
	;; Linux (v0.12) source code (main.c) by Linus Torvalds (1991)
time_of_day:
	call	UPD_IPR			; WAIT TILL UPDATE NOT IN PROGRESS
        jc      short time_of_day_retn ; 23/05/2016
	mov	al, CMOS_SECONDS
	call	CMOS_READ
	mov	[time_seconds], al 
	mov	al, CMOS_MINUTES
	call	CMOS_READ
	mov	[time_minutes], al 
	mov	al, CMOS_HOURS
	call	CMOS_READ
        mov     [time_hours], al
	mov	al, CMOS_DAY_WEEK 
	call	CMOS_READ
	mov	[date_wday], al
 	mov	al, CMOS_DAY_MONTH
	call	CMOS_READ
	mov	[date_day], al
	mov	al, CMOS_MONTH
	call	CMOS_READ
	mov	[date_month], al
	mov	al, CMOS_YEAR
	call	CMOS_READ
	mov	[date_year], al
	mov	al, CMOS_CENTURY
	call	CMOS_READ
	mov	[date_century], al
	;
	mov	al, CMOS_SECONDS
	call 	CMOS_READ
	cmp	al, [time_seconds]
	jne	short time_of_day

time_of_day_retn:
	retn

	; 15/01/2017
	; 10/06/2016
	; 07/06/2016
	; 06/06/2016
	; 23/05/2016
rtc_p:
	mov	cl, 1 ; 15/01/2017
	jmp	short rtc_p0
u_timer: 
	; Timer Events with 18.2 Hz Timer Ticks
	; (and also timer events with RTC seconds)
	sub	cl, cl ; mov cl, 0 ; 15/01/2017
rtc_p0:
	; 19/05/2016 - TRDOS 386 (TRDOS v2.0)
	; Major Modification:
	; Check and Perform Timer Events (for RTC)
	; 25/08/2014 - 07/09/2014
	; Retro UNIX 386 v1:
 	; Print Real Time Clock content
	
	; 15/01/2017
	mov	byte [priority], cl ; 0 or 1 (not 2)
	mov	ch, [timer_events]
	and	ch, ch
	jz	short rtc_p3

	mov	esi, timer_set  ; beginning address of
				; timer events space
rtc_p1:
	mov	eax, [esi]
	and	al, al ; 0 = free, >0 = process no.
	jz	short rtc_p4
	;
	ror	eax, 16
	; ah = response value, al = interrupt type
	; 15/01/2017
	; cl = interrupt source
	;       1 = RTC, 0 = PIT
	cmp	al, cl 
	jne	short rtc_p2 ; not as requested or undefined !
	cmp	al, 1 ; 1 ; RTC interrupt ?
	je	short rtc_p5 ; yes, check for response
	; 06/06/2016 - 18.2 Hz Timer Ticks
	sub	dword [esi+8], 10 ; 1 tick = 10
	jna	short rtc_p6  ; continue for responding
rtc_p2:
	; 15/01/2017 (cl -> ch)
	; 07/06/2016
	dec	ch    ; remain count of timer events
	jnz	short rtc_p4
rtc_p3:
	retn
rtc_p4:
	;cmp	esi, timer_set + 240 ; 15*16 (last event)
	;jnb	short rtc_p3 ; end of timer event space
	add	esi, 16 ; next timer event
	jmp	short rtc_p1
rtc_p5:
	; current timer count ; 06/06/2016 (182)
	sub	dword [esi+8], 182 ; 1 second (10*18.2)
	ja	short rtc_p2  ; check for the next
rtc_p6:
	; it is the time of response! 
	mov	ebx, [esi+4] ; set (count limit) value
	mov	[esi+8], ebx ; reset count down value
			     ; to count limit
	; 19/12/2016
	; 10/12/2016 - timer callback modification
	mov	edi, [esi+12] ; response (or callback) address
	cmp	byte [esi+1], 0 ; >0 = callback
	jna	short rtc_p8

	; timer callback !
	movzx	ebx, byte [esi] ; process number (>0)
	mov	eax, ebx
	shl	bl, 2 ; *4
	mov	[ebx+p.tcb-4], edi ; user's callback service addr
	cmp	al, [u.uno]
	jne	short rtc_p9
	mov	[u.tcb], edi
rtc_p7:
	; 15/01/2017
	mov	al, 2
	mov	[priority], al ; 2
	; 10/01/2017
	;mov	byte [u.pri], 2
	mov	[u.pri], al ; 2
	jmp	short rtc_p2
rtc_p8:
	; response address is physical address of
	; the program's response (signal return) byte
	; 06/06/2016
	;mov	edi, [esi+12] ; response address
	mov	[edi], ah     ; response value
	;
	rol	eax, 16
	; 15/01/2017
	cmp	al, [u.uno] ; running process ?
	je	short rtc_p7
rtc_p9:
	; al = process number  ; 10/06/2016
	mov	dl, 2 ; priority, 2 = event (high)
	call	set_run_sequence ; 19/05/2016
	jmp	short rtc_p2 ; 10/06/2016

; Default IRQ 7 handler against spurious IRQs (from master PIC)
; 25/02/2015 (source: http://wiki.osdev.org/8259_PIC)
default_irq7:
	;push	ax
	; 18/04/2021
	push	eax
	mov	al, 0Bh  ; In-Service register
	out	20h, al
        jmp short $+2
	jmp short $+2
	in	al, 20h
	and 	al, 80h ; bit 7 (is it real IRQ 7 or fake?)
        jz      short irq7_iret ; Fake (spurious) IRQ, do not send EOI
        mov     al, 20h ; EOI
	out	20h, al 
irq7_iret:
	;pop	ax
	; 18/04/2021
	pop	eax
	iretd

bcd_to_ascii:
	; 25/08/2014
	; INPUT ->
	;	al = Packed BCD number
	; OUTPUT ->
	;	ax  = ASCII word/number
	;
	; Erdogan Tan - 1998 (proc_hex) - TRDOS.ASM (2004-2011)
	;
	db	0D4h, 10h	; Undocumented inst. AAM
				; AH = AL / 10h
				; AL = AL MOD 10h
	or	ax, '00'	; Make it ASCII based

        xchg	ah, al 

	retn

; 15/12/2020
real_mem_16m_64k: 
	dw	0	; Real size of system memory (if > 16MB)
			; as number of 64K blocks - 256
			; (This is for saving real system memory
			; because if system memory is larger than
			; 3 GB and if a VESA VBE video bios
			; is detected, 'mem_16m_64K' may be
			; decreased to reserve LFB space 
			; at the end of system memory.)
			; Upper memory space from LFB base address
			; to 4GB will not be included by M.A.T.
def_LFB_addr:
	dw	0 	; HW of default LFB addr (for mode 118h)

%include 'keyboard.s' ; 07/03/2015

%include 'video.s' ; 07/03/2015

setup_rtc_int:
; source: http://wiki.osdev.org/RTC
	cli		; disable interrupts
	; default int frequency is 1024 Hz (Lower 4 bits of register A is 0110b or 6)
	; in order to change this ...
	; frequency  = 32768 >> (rate-1) --> 32768 >> 5 = 1024
	; (rate must be above 2 and not over 15)
	; new rate = 15 --> 32768 >> (15-1) = 2 Hz
	mov	al, 8Ah 
	out	70h, al ; set index to register A, disable NMI
	nop
	in	al, 71h ; get initial value of register A
	mov 	ah, al
	and	ah, 0F0h
	mov	al, 8Ah
	out	70h, al ; reset index to register A
	mov	al, ah
	or	al, 0Fh	; new rate (0Fh -> 15)
	out	71h, al ; write only our rate to A. Note, rate is the bottom 4 bits.
	; enable RTC interrupt
	mov	al, 8Bh ;
	out	70h, al ; select register B and disable NMI
	nop
	in	al, 71h ; read the current value of register B
	mov	ah, al  ;
	mov 	al, 8Bh ;
	out	70h, al ; set the index again (a read will reset the index to register B)
	mov	al, ah  ;
	or	al, 40h ;
	out	71h, al ; write the previous value ORed with 0x40. This turns on bit 6 of register B
	sti
	retn

; Write memory information
; 29/01/2016
; 06/11/2014
; 14/08/2015
memory_info:
	mov	eax, [memory_size] ; in pages
	push	eax
	shl	eax, 12		   ; in bytes
	mov	ebx, 10
	mov	ecx, ebx	   ; 10
	mov	esi, mem_total_b_str
	call	bintdstr
	pop	eax
	mov	cl, 7
	mov	esi, mem_total_p_str
	call	bintdstr
	; 14/08/2015
	call	calc_free_mem
	; edx = calculated free pages
	; ecx = 0
	mov 	eax, [free_pages]
	cmp	eax, edx ; calculated free mem value
		; and initial free mem value are same or not?
	jne 	short pmim ; print mem info with '?' if not
	push 	edx ; free memory in pages
	;mov 	eax, edx
	shl	eax, 12 ; convert page count
			; to byte count
	mov	cl, 10
	mov	esi, free_mem_b_str
	call	bintdstr
	pop	eax
	mov	cl, 7
	mov	esi, free_mem_p_str
	call	bintdstr
pmim:
	mov	esi, msg_memory_info
	;
	mov	ah, 07h ; Black background,
			; light gray forecolor
print_kmsg: ; 29/01/2016
	mov	[ccolor], ah
pkmsg_loop:
	lodsb
	or	al, al
	jz	short pkmsg_ok
	push	esi
	; 13/05/2016
	movzx	ebx, byte [ccolor]
			; Video page 0 (bh=0)
	call	_write_tty
	pop	esi
	jmp	short pkmsg_loop
pkmsg_ok:
	retn

; 19/12/2020
; temporary
; Write default liner frame buffer address
;
default_lfb_info:
	mov	ax, [def_LFB_addr] ; high word
	; 24/11/2023 - temporary
	or	ax, ax
	jz	short pkmsg_ok
	call	wordtohex
	mov	dword [lfb_addr_str], eax
	mov	esi, msg_lfb_addr
	mov	ah, 0Fh ; Black background,
			; white forecolor
	jmp	short print_kmsg

; Convert binary number to hexadecimal string
; 10/05/2015  
; dsectpm.s (28/02/2015)
; Retro UNIX 386 v1 - Kernel v0.2.0.6  
; 01/12/2014
; 25/11/2014
;
bytetohex:
	; INPUT ->
	; 	AL = byte (binary number)
	; OUTPUT ->
	;	AX = hexadecimal string
	;
	push	ebx
	xor	ebx, ebx
	mov	bl, al
	shr	bl, 4
	mov	bl, [ebx+hexchrs]
	xchg	bl, al
	and	bl, 0Fh
	mov	ah, [ebx+hexchrs]
	pop	ebx
	retn

wordtohex:
	; INPUT ->
	; 	AX = word (binary number)
	; OUTPUT ->
	;	EAX = hexadecimal string
	;
	push	ebx
	xor	ebx, ebx
	xchg	ah, al
	push	ax ; * save ax
	mov	bl, ah
	shr	bl, 4
	mov	al, [ebx+hexchrs]
	mov	bl, ah
	and	bl, 0Fh
	mov	ah, [ebx+hexchrs]
	shl	eax, 16 ; ax -> hw of eax
	pop	ax ; * restore ax
	pop	ebx
	jmp	short bytetohex
	;mov	bl, al
	;shr	bl, 4
	;mov	bl, [ebx+hexchrs]
	;xchg	bl, al
	;and	bl, 0Fh
	;mov	ah, [ebx+hexchrs]
	;pop	ebx
	;retn

dwordtohex:
	; INPUT ->
	; 	EAX = dword (binary number)
	; OUTPUT ->
	;	EDX:EAX = hexadecimal string
	;
	push	eax
	shr	eax, 16
	call	wordtohex
	mov	edx, eax
	pop	eax
	;call	wordtohex
	;retn
	; 18/04/2021
	jmp	short wordtohex

; 10/05/2015
hex_digits:
hexchrs:
	db '0123456789ABCDEF'
; 19/01/2021 - VESA EDID ready flag (4Fh)
edid:	db 0

; Convert binary number to decimal/numeric string
; 06/11/2014
; Temporary Code
;

bintdstr:
	; EAX = binary number
	; ESI = decimal/numeric string address
	; EBX = divisor (10)
	; ECX = string length (<=10)
	add	esi, ecx
btdstr0:
	dec	esi
	xor	edx, edx
	div	ebx
	add	dl, 30h
	mov	[esi], dl
	dec	cl
	jz	short btdstr2 ; 08/09/2016
	or	eax, eax
	jnz	short btdstr0
btdstr1:
	dec	esi
        mov     byte [esi], 20h ; blank space
	dec	cl
	jnz	short btdstr1
btdstr2:
	retn

; Calculate free memory pages on M.A.T.
; 06/11/2014
; Temporary Code
;

calc_free_mem:
	xor	edx, edx
	;xor	ecx, ecx
	mov	cx, [mat_size] ; in pages
	shl	ecx, 10	; 1024 dwords per page
	mov	esi, MEM_ALLOC_TBL
cfm0:
	lodsd
	push	ecx
	mov	ecx, 32
cfm1:
	shr	eax, 1
	jnc	short cfm2
	inc	edx
cfm2:
	loop	cfm1
	pop	ecx
	loop	cfm0
	retn

%include 'diskio.s'  ; 07/03/2015
%include 'memory.s'  ; 09/03/2015
%include 'timer.s'   ; 17/01/2015

Align 16

gdt:	; Global Descriptor Table
	; 02/12/2020
	; (30/07/2015, conforming cs)
	; (26/03/2015)
	; (24/03/2015, tss)
	; (19/03/2015)
	; (29/12/2013)
	;
	dw 0, 0, 0, 0		; NULL descriptor
gdt_kcode:
	; 18/08/2014
			; 8h kernel code segment, base = 00000000h
	;dw 0FFFFh, 0, 9E00h, 00CFh	; KCODE  ; 30/12/2016
	dw 0FFFFh, 0, 9A00h, 00CFh	; KCODE
gdt_kdata:
			; 10h kernel data segment, base = 00000000h
	dw 0FFFFh, 0, 9200h, 00CFh	; KDATA
gdt_ucode:
			; 1Bh user code segment, base address = 400000h ; CORE
	;dw 0FBFFh, 0, 0FE40h, 00CFh	; UCODE  ; 30/12/2016
	dw 0FBFFh, 0, 0FA40h, 00CFh	; UCODE
gdt_udata:
			; 23h user data segment, base address = 400000h ; CORE
	dw 0FBFFh, 0, 0F240h, 00CFh	; UDATA
gdt_tss:
			; Task State Segment
	dw 0067h ; Limit = 103 ; (104-1, tss size = 104 byte,
			       ;  no IO permission in ring 3)
gdt_tss0:
	dw 0  ; TSS base address, bits 0-15
gdt_tss1:
	db 0  ; TSS base address, bits 16-23
	      		; 49h	
	db 11101001b ; 0E9h => P=1/DPL=11/0/1/0/B/1 --> B = Task is busy (1)
	db 0 ; G/0/0/AVL/LIMIT=0000 ; (Limit bits 16-19 = 0000) (G=0, 1 byte)
gdt_tss2:
	db 0  ; TSS base address, bits 24-31

	; 04/12/2023 - TRDOS v2.0.7
	; 30/11/2020
	; 29/11/2020 - TRDOS v2.0.3
	; VESA VBE3 VIDE BIOS 32 BIT PMI SEGMENTS (16 bit segments)
			; 30h ; VBE3CS
_vbe3_CS:  ; vesa vbe3 bios uses this as code seg (same addr with _vbe3_DS)
	; limit = 65536, base addr = 0, P/DPL/1/Type/C/R/A = 9Ah, 16 bit
	;dw 0FFFFh, 0, 9A00h, 0 ; Note: base addr will be initialized
	; 04/12/2023 - TRDOS 386 v2.0.7	; (+!*!+)
	dw 0FFFFh, 0, 9A06h, 0	; VBE3BIOSCODE_ADDR = 60000h
			; 38h ; VBE3BDS
_vbe3_BDS: ; vesa vbe3 bios uses this as equivalent of rombios data segment
	; limit = 1536, base addr = 0, P/DPL/1/Type/E/W/A = 92h, 16 bit
	dw 05FFh, 0, 9200h, 0 ; Note: base addr will be initialized
			; 40h ; VBE3A000
_A0000Sel: ; VGA default video memory address
	; limit = 65536, base addr = 0A0000h, 16 bit
	dw 0FFFFh, 0, 920Ah, 0
			; 48h ; VBE3B000
_B0000Sel: ; MDA (monochrome) video memory address
	; limit = 65536, base addr = 0B0000h, 16 bit
	dw 0FFFFh, 0, 920Bh, 0
			; 50h ; VBE3B800
_B8000Sel: ; CGA video memory address
	; limit = 32768, base addr = 0B8000h, 16 bit
	dw 07FFFh, 8000h, 920Bh, 0
			; 58h ; VBE3DS
_vbe3_DS: ; vesa vbe3 bios uses this as data seg (CodeSegSel in PMInfoBlock)
	; limit = 65536, base addr = 0, P/DPL/1/Type/E/W/A = 92h, 16 bit
	;dw 0FFFFh, 0, 9200h, 0 ; Note: base addr will be initialized
	; 04/12/2023 - TRDOS 386 v2.0.7	; (+!*!+)
	dw 0FFFFh, 0, 9206h, 0	; VBE3BIOSCODE_ADDR = 60000h
			; 60h ; VBE3SS
_vbe3_SS: ; kernel's stack segment but 16 bit version (same stack addr)
	; limit = 1024, base addr = 0, P/DPL/1/Type/E/W/A = 92h, 16 bit
	dw 03FFh, 0, 9200h, 0 ; Note: base addr will be initialized
			; 68h ; VBE3ES
_vbe3_ES: ; extra 16 bit segment points to buffers in kernel's mem space
	; limit = 2048, base addr = 0, P/DPL/1/Type/E/W/A = 92h, 16 bit
	dw 07FFh, 0, 9200h,0 ; Note: base addr will be initialized
			; 70h ; KODE16
_16bit_CS: ; 16 bit code segment points to kernel's far return addr
	; limit = 16M, base addr = 0, P/DPL/1/Type/E/W/A = 92h, 16 bit
	dw 0FFFFh, 0, 9A00h, 00FFh ; Note: base addr will be initialized

gdt_end:
	;; 9Eh = 1001 1110b (GDT byte 5) P=1/DPL=00/1/TYPE=1110,
					;; Type= 1 (code)/C=1/R=1/A=0
		; P= Present, DPL=0=ring 0,  1= user (0= system)
		; 1= Code C= Conforming, R= Readable, A= Accessed

	;; 9Ah = 1001 1010b (GDT byte 5) P=1/DPL=00/1/TYPE=1010,
					;; Type= 1 (code)/C=0/R=1/A=0
		; P= Present, DPL=0=ring 0,  1= user (0= system)
		; 1= Code C= non-Conforming, R= Readable, A= Accessed

	;; 92h = 1001 0010b (GDT byte 5) P=1/DPL=00/1/TYPE=1010,
					;; Type= 0 (data)/E=0/W=1/A=0
		; P= Present, DPL=0=ring 0,  1= user (0= system)
		; 0= Data E= Expansion direction (1= down, 0= up)
		; W= Writeable, A= Accessed

	;; FEh = 1111 1110b (GDT byte 5) P=1/DPL=11/1/TYPE=1110,
					;; Type= 1 (code)/C=1/R=1/A=0
		; P= Present, DPL=3=ring 3,  1= user (0= system)
		; 1= Code C= Conforming, R= Readable, A= Accessed

	;; FAh = 1111 1010b (GDT byte 5) P=1/DPL=11/1/TYPE=1010,
					;; Type= 1 (code)/C=0/R=1/A=0
		; P= Present, DPL=3=ring 3,  1= user (0= system)
		; 1= Code C= non-Conforming, R= Readable, A= Accessed

	;; F2h = 1111 0010b (GDT byte 5) P=1/DPL=11/1/TYPE=0010,
					;; Type= 0 (data)/E=0/W=1/A=0
		; P= Present, DPL=3=ring 3,  1= user (0= system)
		; 0= Data E= Expansion direction (1= down, 0= up)

	;; CFh = 1100 1111b (GDT byte 6) G=1/B=1/0/AVL=0, Limit=1111b (3)

		;; Limit = FFFFFh (=> FFFFFh+1= 100000h) // bits 0-15, 48-51 //
		;	 = 100000h * 1000h (G=1) = 4GB
		;; Limit = FFBFFh (=> FFBFFh+1= FFC00h) // bits 0-15, 48-51 //
		;	 = FFC00h * 1000h (G=1) = 4GB - 4MB
		; G= Granularity (1= 4KB), B= Big (32 bit), 
		; AVL= Available to programmers	

gdtd:
        dw gdt_end - gdt - 1    ; Limit (size)
        dd gdt			; Address of the GDT

	; 20/08/2014
idtd:
        dw idt_end - idt - 1    ; Limit (size)
        dd idt			; Address of the IDT

; 20/02/2017
;;; 11/03/2015
%include 'diskdata.s'	; DISK (BIOS) DATA (initialized)

Align 2

; 04/11/2014 (Retro UNIX 386 v1)
mem_1m_1k:   dw 0  ; Number of contiguous KB between
                     ; 1 and 16 MB, max. 3C00h = 15 MB.
mem_16m_64k: dw 0  ; Number of contiguous 64 KB blocks
		   ; between 16 MB and 4 GB.

; 12/11/2014 (Retro UNIX 386 v1)
boot_drv:    db 0 ; boot drive number (physical)
; 24/11/2014
drv:	     db 0
last_drv:    db 0 ; last hdd
hdc:         db 0  ; number of hard disk drives
		     ; (present/detected)

; 24/11/2014 (Retro UNIX 386 v1)
; Physical drive type & flags
fd0_type:    db 0  ; floppy drive type
fd1_type:    db 0  ; 4 = 1.44 Mb, 80 track, 3.5" (18 spt)
		     ; 6 = 2.88 Mb, 80 track, 3.5" (36 spt)
		     ; 3 = 720 Kb, 80 track, 3.5" (9 spt)
		     ; 2 = 1.2 Mb, 80 track, 5.25" (15 spt)
		     ; 1 = 360 Kb, 40 track, 5.25" (9 spt)
hd0_type:    db 0  ; EDD status for hd0 (bit 7 = present flag)
hd1_type:    db 0  ; EDD status for hd1 (bit 7 = present flag)
hd2_type:    db 0  ; EDD status for hd2 (bit 7 = present flag)
hd3_type:    db 0  ; EDD status for hd3 (bit 7 = present flag)
		     ; bit 0 - Fixed disk access subset supported
		     ; bit 1 - Drive locking and ejecting
		     ; bit 2 - Enhanced disk drive support
		     ; bit 3 = Reserved (64 bit EDD support)
		     ; (If bit 0 is '1' Retro UNIX 386 v1
		     ; will interpret it as 'LBA ready'!)

; 08/08/2022
; (drv.cylinders, drv.spt, drv.spt will not be used now on)
; ('diskio.inc')
; ((spt and heads and cylinder counts will be taken from DPT))

;; 11/03/2015 - 10/07/2015
;drv.cylinders: dw 0,0,0,0,0,0,0
;drv.heads:     dw 0,0,0,0,0,0,0
;drv.spt:       dw 0,0,0,0,0,0,0
; 12/07/2022 - 11/03/2015
drv.size:      dd 0,0,0,0,0,0,0
drv.status:    db 0,0,0,0,0,0,0
drv.error:     db 0,0,0,0,0,0,0
;

Align 2

;;; 11/03/2015
%include 'kybdata.s'	; KEYBOARD (BIOS) DATA
%include 'vidata.s'	; VIDEO (BIOS) DATA
;%include 'diskdata.s'	; DISK (BIOS) DATA (initialized)
;;;

Align 2

%include 'sysdefs.s' ; 24/01/2015
%include 'trdosk0.s' ; 04/01/2016
%include 'trdosk1.s' ; 04/01/2016
%include 'trdosk2.s' ; 04/01/2016
%include 'trdosk3.s' ; 06/01/2016
%include 'trdosk4.s' ; 24/01/2016
%include 'trdosk5.s' ; 24/01/2016
%include 'trdosk6.s' ; 24/01/2016
%include 'trdosk7.s' ; 24/01/2016
%include 'trdosk8.s' ; 24/01/2016
%include 'trdosk9.s' ; 04/01/2016

; 27/08/2014
scr_row:
	dd 0B8000h + 0A0h + 0A0h + 0A0h ; Row 3
scr_col:
	dd 0

Align 4
	; 15/04/2016
	; TRDOS 386 (TRDOS v2.0)

	; 21/08/2014
ilist:
	;times 	32 dd cpu_except ; INT 0 to INT 1Fh
	;
	; Exception list
	; 25/08/2014
	dd	exc0	; 0h,  Divide-by-zero Error
	dd	exc1
	dd 	exc2
	dd	exc3
	dd	exc4
	dd	exc5
	dd 	exc6	; 06h,  Invalid Opcode
	dd	exc7
	dd	exc8
	dd	exc9
	dd 	exc10
	dd	exc11
	dd	exc12
	dd	exc13	; 0Dh, General Protection Fault
	dd 	exc14	; 0Eh, Page Fault
	dd	exc15
	dd	exc16
	dd	exc17
	dd 	exc18
	dd	exc19
	dd 	exc20
	dd	exc21
	dd	exc22
	dd	exc23
	dd 	exc24
	dd	exc25
	dd	exc26
	dd	exc27
	dd 	exc28
	dd	exc29
	dd 	exc30
	dd	exc31
IRQ_list: ; 28/02/2017 ('syscalbac')
	; Interrupt list
	dd	timer_int	; INT 20h
		;dd	irq0
	dd	kb_int		; 24/01/2016
		;dd	irq1
	dd	irq2
		; COM2 int
	dd	irq3
		; COM1 int
	dd	irq4
	dd	irq5
;DISKETTE_INT: ;06/02/2015
	dd	fdc_int		; 16/02/2015, IRQ 6 handler
		;dd	irq6
; Default IRQ 7 handler against spurious IRQs (from master PIC)
; 25/02/2015 (source: http://wiki.osdev.org/8259_PIC)
	dd	default_irq7	; 25/02/2015
		;dd	irq7
; Real Time Clock Interrupt
	dd	rtc_int		; 23/02/2015, IRQ 8 handler
		;dd	irq8	; INT 28h
	dd	irq9
	dd	irq10
	dd	irq11
	dd	irq12
	dd	irq13
;HDISK_INT1:  ;06/02/2015
	dd	hdc1_int 	; 21/02/2015, IRQ 14 handler
		;dd	irq14
;HDISK_INT2:  ;06/02/2015
	dd	hdc2_int 	; 21/02/2015, IRQ 15 handler
		;dd	irq15	; INT 2Fh
		; 14/08/2015
	;dd	sysent		; INT 30h (system calls)

	; 15/04/2016
	; TRDOS 386(TRDOS v2.0) Software Interrupts

	dd	int30h		; Reserved for
				; !!! Retro UNIX (RUNIX) !!!
				; !!! SINGLIX !!! System Calls
	dd	int31h		; Video BIOS (IBM PC/AT, Int 10h)
	dd	int32h		; Keyboard Functions (IBM PC/AT, Int 16h)
	dd	int33h		; DISK I/O (IBM PC/AT, Int 13h)
	dd	int34h		; #IOCTL# (I/O port access support for ring 3)
	dd	int35h		; Time/Date Functions (IBM PC/AT, Int 1Ah)
	dd	ignore_int	; INT 36h : Timer Functions
	dd	ignore_int	; INT 37h
	dd	ignore_int	; INT 38h
	dd	ignore_int	; INT 39h
	dd	ignore_int	; INT 3Ah
	dd	ignore_int	; INT 3Bh
	dd	ignore_int	; INT 3Ch
	dd	ignore_int	; INT 3Dh
	dd	ignore_int	; INT 3Eh
	dd	ignore_int	; INT 3Fh
	dd	sysent		; INT 40h : !!! TRDOS 386 System Calls !!!
	;dd	ignore_int
	dd	0

; 20/08/2014
  ; /* This is the default interrupt "handler" :-) */
  ; Linux v0.12 (head.s)
int_msg:
	db "Unknown interrupt ! ", 0

; 15/04/2016
; TRDOS 386 (TRDOS v2.0)

; 29/04/2016
int30h:
trdos_isc_routine:
	; 02/05/2016
	; 01/05/2016
	; 29/04/2016
	; 18/04/2016
	; 15/04/2016 (TRDOS 386 = TRDOS v2.0)
	; 17/04/2011 (TRDOS v1.0, 'IFC.ASM')
	; 03/02/2011 ('trdos_ifc_routine')
	;
	mov	ebx, [esp] ; EIP (next)
	sub	ebx, 2 ; EIP (CD ##h)

	mov	ecx, eax
	mov	al, [ebx+1] ; CDh ##h

	mov	dx, KDATA
	mov	ds, dx
	mov	es, dx

	cld
	mov	edx, [k_page_dir]
	mov	cr3, edx

	call	bytetohex
	mov	[int_num_str], ax

	mov	eax, ebx ; EIP
	call	dwordtohex
	mov	[eip_str], edx
	mov	[eip_str+4], eax

	mov	eax, ecx
	call	dwordtohex
	mov	[eax_str], edx
	mov	[eax_str+4], eax

	inc	ebx
	mov	al, [ebx] ; Interrupt number

trdos_isc_handler:
	cmp	dh, 30h ; Retro UNIX, SINGLIX System calls
	jne	short trdos_usi_handler
	mov	esi, isc_msg
	jmp	short loc_write_inv_system_call_msg

trdos_usi_handler:
	mov	esi, usi_msg

loc_write_inv_system_call_msg:
	call	print_msg
	; 29/04/2016
	mov	esi, inv_msg_for_trdos_v2
	call	print_msg

loc_ifc_terminate_process:
	; u.uno = process number
	; 29/04/2016

	; 02/05/2016
	inc	byte [sysflg] ; 0FFh -> 0

	mov	eax, 1
	jmp	sysexit

; 07/03/2015
; Temporary Code
display_disks:
	cmp 	byte [fd0_type], 0
	jna 	short ddsks1
	call	pdskm
ddsks1:
	cmp	byte [fd1_type], 0
	jna	short ddsks2
	mov	byte [dskx], '1'
	call	pdskm
ddsks2:
	cmp	byte [hd0_type], 0
	jna	short ddsk6
	mov	word [dsktype], 'hd'
	mov	byte [dskx], '0'
	call	pdskm
ddsks3:
	cmp	byte [hd1_type], 0
	jna	short ddsk6
	mov	byte [dskx], '1'
	call	pdskm
ddsks4:
	cmp	byte [hd2_type], 0
	jna	short ddsk6
	mov	byte [dskx], '2'
	call	pdskm
ddsks5:
	cmp	byte [hd3_type], 0
	jna	short ddsk6
	mov	byte [dskx], '3'
	call	pdskm
ddsk6:
	mov	esi, nextline
	call	pdskml
pdskm_ok:
	retn
pdskm:
	mov	esi, dsk_ready_msg
pdskml:
	lodsb
	or	al, al
	jz	short pdskm_ok
	push	esi
	; 13/05/2016
        mov     ebx, 7  ; Black background,
			; light gray forecolor
			; Video page 0 (bh=0)
	call	_write_tty
	pop	esi
	jmp	short pdskml

Align 2
	; 21/08/2014
exc_msg:
	db "CPU exception ! "
excnstr: 		; 25/08/2014
	db "??h", "  EIP : "
EIPstr: ; 29/08/2014
	times 12 db 0

	; 23/02/2015
	; 25/08/2014
;scounter:
;	db 5
;	db 19

; 06/11/2014
; Memory Information message
; 14/08/2015
msg_memory_info:
	db	07h
	db	0Dh, 0Ah
	;db 	"MEMORY ALLOCATION INFO", 0Dh, 0Ah, 0Dh, 0Ah
	db	"Total memory : "
mem_total_b_str: ; 10 digits
	db	"0000000000 bytes", 0Dh, 0Ah
	db	"               ", 20h, 20h, 20h
mem_total_p_str: ; 7 digits
	db	"0000000 pages", 0Dh, 0Ah
	db 	0Dh, 0Ah
	db	"Free memory  : "
free_mem_b_str:  ; 10 digits
	db	"?????????? bytes", 0Dh, 0Ah
	db	"               ", 20h, 20h, 20h
free_mem_p_str:  ; 7 digits
	db	"??????? pages", 0Dh, 0Ah
	db	0Dh, 0Ah, 0

dsk_ready_msg:
	db 	0Dh, 0Ah
dsktype:
	db	'fd'
dskx:
	db	'0'
	db	20h
	db 	'is READY ...'
	db 	0

setup_error_msg:
	db	0Dh, 0Ah
	db	'Disk Setup Error !' 
	db	0Dh, 0Ah,0

next2line: ; 08/02/2016
	db	0Dh, 0Ah
nextline:
	db 	0Dh, 0Ah, 0

; temporary
; 19/12/2020
msg_lfb_addr:
	;db	0Dh, 0Ah
	db	"Linear frame buffer at "
lfb_addr_str: ; 8 (hex) digits
	db	"00000000h", 0Dh, 0Ah
	db	0Dh, 0Ah, 0

; KERNEL - SYSINIT Messages
; 24/08/2015
; 13/04/2015 - (Retro UNIX 386 v1 Beginning)
; 14/07/2013
;kernel_init_err_msg:
;	db 0Dh, 0Ah
;	db 07h
;	db 'Kernel initialization ERROR !'
;	db 0Dh, 0Ah, 0 

;welcome_msg: 
;	db 0Dh, 0Ah
;	db 07h
;	db 'Welcome to TRDOS 386 Operating System !'
;	db 0Dh, 0Ah
;	db 'by Erdogan Tan - 31/12/2017 (v2.0.0)'
;	db 0Dh, 0Ah, 0

panic_msg:
	db 0Dh, 0Ah, 07h
	db 'ERROR: Kernel Panic !'
	db 0Dh, 0Ah, 0

;msgl_drv_not_ready: 
;	db 07h, 0Dh, 0Ah
;       db 'Drive not ready or read error !'
;       db 0Dh, 0Ah, 0

starting_msg:
	;;;;;;;;db "Turkish Rational DOS v2.0 [18/04/2021] ...", 0
	;;;;;;;db "Turkish Rational DOS v2.0 [11/08/2022] ...", 0
	;;;;;;db "Turkish Rational DOS v2.0 [30/08/2023] ...", 0
	;;;;;db "Turkish Rational DOS v2.0 [07/12/2023] ...", 0
	;;;;db "Turkish Rational DOS v2.0 [23/06/2024] ...", 0
	;;;db "Turkish Rational DOS v2.0 [29/12/2024] ...", 0
	;;db "Turkish Rational DOS v2.0 [28/01/2025] ...", 0
	;db "Turkish Rational DOS v2.0 [24/04/2025] ...", 0
	db "Turkish Rational DOS v2.1 [07/07/2025] ...", 0

NextLine:
	db 0Dh, 0Ah, 0

%include 'audio.s' ; 03/04/2017

align 4

%include 'vgadata.s' ; 04/07/2016

; 20/11/2020
vbe2_bochs_vbios:
;	db "BOCHS/QEMU"
	db "BOCHS/QEMU/VIRTUALBOX"  ; 26/11/2020
;vbe_vnumber equ vbe2_bochs_vbios + 28 ; "3" or "2"
; 26/11/2020
vbe_vnumber equ vbe2_bochs_vbios + 30 ; "3" or "2"
; 15/11/2020
vesa_vbe3_bios_msg:
	;db " VESA VBE version 3 Video BIOS ..."
	db " VESA VBE3 Video BIOS ..." ; 26/11/2020
	db 0Dh, 0Ah, 0Dh, 0Ah, 0

; 28/02/2021
truecolor: db 24

align 2

; EPOCH Variables
; 13/04/2015 - Retro UNIX 386 v1 Beginning
; 09/04/2013 epoch variables
; Retro UNIX 8086 v1 Prototype: UNIXCOPY.ASM, 10/03/2013
;
year: 	dw 1970
month: 	dw 1
day: 	dw 1
hour: 	dw 0
minute: dw 0
second: dw 0

DMonth:
	dw 0
	dw 31
	dw 59
	dw 90
	dw 120
	dw 151
	dw 181
	dw 212
	dw 243
	dw 273
	dw 304
	dw 334

; 15/11/2020
db	0
kernel_version_msg: ; 17/04/2021
;;;;;;;db "TRDOS (386) Kernel v2.0.4 by Erdogan Tan"
;;;;;;db "TRDOS (386) Kernel v2.0.5 by Erdogan Tan" ; 11/08/2022
;;;;;db	"TRDOS (386) Kernel v2.0.6 by Erdogan Tan" ; 29/08/2023
;;;;db	"TRDOS (386) Kernel v2.0.7 by Erdogan Tan" ; 20/10/2023
;;;db	"TRDOS (386) Kernel v2.0.8 by Erdogan Tan" ; 16/05/2024
;;db	"TRDOS (386) Kernel v2.0.9 by Erdogan Tan" ; 20/08/2024
;db	"TRDOS (386) Kernel v2.0.10 by Erdogan Tan" ; 11/01/2025
db	"TRDOS (386) Kernel v2.1.0 by Erdogan Tan" ; 07/07/2025
db	0

; 20/02/2017
KERNELFSIZE  equ $  ; 04/07/2016

bss_start:

ABSOLUTE bss_start

alignb 8 ; 25/12/2016

	; 15/04/2016
	; TRDOS 386 (TRDOS v2.0)
	; 	80 interrupts 	
	; 11/03/2015
	; Interrupt Descriptor Table (20/08/2014)
idt:
	;resb	64*8 ; INT 0 to INT 3Fh
	; 15/04/2016
	resb	80*8 ; INT 0 to INT 4Fh

idt_end:

;alignb 4

task_state_segment:
	; 24/03/2015
tss.link:   resw 1
	    resw 1
; tss offset 4	
tss.esp0:   resd 1
tss.ss0:    resw 1
	    resw 1
tss.esp1:   resd 1
tss.ss1:    resw 1
	    resw 1
tss.esp2:   resd 1
tss.ss2:    resw 1
	    resw 1
; tss offset 28
tss.CR3:    resd 1
tss.eip:    resd 1
tss.eflags: resd 1
; tss offset 40
tss.eax:    resd 1
tss.ecx:    resd 1
tss.edx:    resd 1
tss.ebx:    resd 1
tss.esp:    resd 1
tss.ebp:    resd 1
tss.esi:    resd 1
tss.edi:    resd 1
; tss offset 72
tss.ES:     resw 1
	    resw 1
tss.CS:	    resw 1
	    resw 1
tss.SS:	    resw 1
	    resw 1
tss.DS:	    resw 1
	    resw 1
tss.FS:	    resw 1
	    resw 1
tss.GS:	    resw 1
	    resw 1
tss.LDTR:   resw 1
	    resw 1
; tss offset 100
	    resw 1
tss.IOPB:   resw 1
; tss offset 104
tss_end:

k_page_dir:  resd 1 ; Kernel's (System) Page Directory address
		    ; (Physical address = Virtual address)
memory_size: resd 1 ; memory size in pages
free_pages:  resd 1 ; number of free pages
next_page:   resd 1 ; offset value in M.A.T. for
		    ; first free page search
last_page:   resd 1 ; offset value in M.A.T. which
		    ; next free page search will be
		    ; stopped after it. (end of M.A.T.)
first_page:  resd 1 ; offset value in M.A.T. which
		    ; first free page search
		    ; will be started on it. (for user)
mat_size:    resd 1 ; Memory Allocation Table size in pages

; 20/11/2020
;vbe2bios:    resw 1 ; VBE2 video bios ID (bochs/qemu)
;		    ; (0B0C4h or 0B0C5h for bochs/plex86 vgabios)

; 02/09/2014 (Retro UNIX 386 v1)
; 04/12/2013 (Retro UNIX 8086 v1)
CRT_START:   resw 1 	  ; starting address in regen buffer
			  ; NOTE: active page only
CURSOR_POSN: resw 8 ; cursor positions for video pages
ACTIVE_PAGE:
ptty: 	     resb 1 ; current tty
; 01/07/2015 - 29/01/2016
ccolor:	     resb 1 ; current color attribute
; 26/10/2015
; 07/09/2014
ttychr:      resw ntty+2 ; Character buffer (multiscreen)

; 18/05/2015 (03/06/2013 - Retro UNIX 8086 v1 feature only!)
p_time:      resd 1     ; present time (for systime & sysmdate)

; 18/05/2015 (16/08/2013 - Retro UNIX 8086 v1 feature only !)
; (open mode locks for pseudo TTYs)
; [ major tty locks (return error in any conflicts) ]
ttyl:        resw ntty+2 ; opening locks for TTYs.

; 15/04/2015 (Retro UNIX 386 v1)
; 22/09/2013 (Retro UNIX 8086 v1)
wlist:       resb ntty+2 ; wait channel list (0 to 9 for TTYs)
; 15/04/2015 (Retro UNIX 386 v1)
;; 12/07/2014 -> sp_init set comm. parameters as 0E3h
;; 0 means serial port is not available 
;;comprm: ; 25/06/2014
com1p:       resb 1  ;;0E3h
com2p:       resb 1  ;;0E3h

; 17/11/2015
; request for response (from the terminal)
req_resp:    resw 1
; 07/11/2015
ccomport:    resb 1 ; current COM (serial) port
		    ; (0= COM1, 1= COM2)
; 09/11/2015
comqr:	     resb 1 ; 'query or response' sign (u9.s, 'sndc')
; 07/11/2015
rchar:	     resw 1 ; last received char for COM 1 and COM 2
schar:	     resw 1 ; last sent char for COM 1 and COM 2

; 22/08/2014 (RTC)
; (Packed BCD)
time_seconds: resb 1
time_minutes: resb 1
time_hours:   resb 1
date_wday:    resb 1
date_day:     resb 1
date_month:   resb 1
date_year:    resb 1
date_century: resb 1

; 24/01/2016
RTC_LH:	       resd 1
RTC_WAIT_FLAG: resb 1
USER_FLAG:     resb 1
; 19/05/2016
;RTC_second:
RTC_2Hz:       resb 1 ;  from 2Hz interrupt to 1Hz timer event function

%include 'diskbss.s'	; UNINITIALIZED DISK (BIOS) DATA

;;; Real Mode Data (10/07/2015 - BSS)

;alignb 2

; 10/01/2016
%include 'trdoskx.s'	; UNINITIALIZED KERNEL (Logical Drive & FS) DATA
; 24/01/2016
%include 'ubss.s'	; UNINITIALIZED KERNEL (USER) DATA

alignb 4

; 23/05/2016 (TRDOS 386)
; 14/10/2015 (Retro UNIX 386 v1, 'unix386.s')
cr3reg:	 resd 1  ; cr3 register content at the beginning of the timer
		 ; (or RTC) interrupt handler.

; 10/12/2016 (callback)
; 10/06/2016
; 19/05/2016
; 18/05/2016 - TRDOS 386 feature only !
timer_set: resd 16*4   ; 256 bytes memory space for 16 timer events
	; Timer Event Structure: (max. 16 timer events, 16*16 bytes)
	;       Owner:	        resb 1 ; 0 = free
	;		  	       ;>0 = process number (u.uno)
	;	Callback:	resb 1 ; 0 = response byte address (phy)
	;				 1 = callback address (virtual)
	;	Interrupt:      resb 1 ; 0 = Timer interrupt (or none)
	;		   	       ; 1 = Real Time Clock interrupt
	;	Response:       resb 1 ; 0 to 255, signal return value
	;	Count Limit:	resd 1 ; count of ticks (total/set)
	;	Current Count: 	resd 1 ; count of ticks (current)
	;	Response Addr:  resd 1 ; response byte (pointer) address
	;			       ; (or callback -user service- address)

; 17/04/2021
; (memory page swap parameters are disabled as temporary)
;
; Memory (swap) Data (11/03/2015)
; 09/03/2015
;swpq_count: resw 1 ; count of pages on the swap queue
;swp_drv:    resd 1 ; logical drive description table address of the swap drive/disk
;swpd_size:  resd 1 ; size of swap drive/disk (volume) in sectors (512 bytes).
;swpd_free:  resd 1 ; free page blocks (4096 bytes) on swap disk/drive (logical)
;swpd_next:  resd 1 ; next free page block
;swpd_last:  resd 1 ; last swap page block

alignb 4

; 10/07/2015
; 28/08/2014
error_code:	resd 1
; 29/08/2014
FaultOffset: 	resd 1
; 21/09/2015
PF_Count:	resd 1	; total page fault count
		       	; (for debugging - page fault analyze)
		 	; 'page_fault_handler' (memory.inc)
			; 'sysgeterr' (u9.s)

; 29/04/2016 (TRDOS 386 = TRDOS v2.0)
; 22/08/2015 (Retro UNIX 386 v1)
buffer: 
	resb	8
readi_buffer:
	resb 	512
	resb	8
writei_buffer:
	resb	512
; 24/10/2016
	resb	8
rw_buffer:
	resb 	2048  ; general purposed, r/w sector buffer

%if 1
; 17/01/2021
edid_info:	resb 128 ; VESA EDID (monitor capabilities) info
; 28/11/2020
pmi32:		resb 1	; (>0) use VESA VBE3 protected mode calls 
vbe_mode_x:	resb 1  ; VESA VBE3 video bios mode set options
video_mode:	resw 1	; VESA VBE3 video mode (with option flags)
; 30/11/2020
vbe3bios_addr:	resd 1	; new (writable mem) address of VBE3 bios
; 02/12/2020
pmid_addr:	resd 1	; PMInfoBlock ('PMID') linear address
; 14/01/2021
; 06/12/2020		; VESA VBE 3 video state
;vbe3stbufsize: resw 1	; video regs/dac/bios state buffer size
;			; block size in bytes
; 16/01/2021
vbe3stbsflags: resw 1	; video regs/dac/bios state buffer size
;			; pointer flags for buffer state options
%endif

%if 1
; 10/12/2020
LFB_Info:
		resb 16	; Linear Frame Buffer info block

;24/11/2020 - TRDOS 386 v2.0.3
; BOCHS/PLEX86 VESA VBE3 MODE INFO extension to TRDOS 386 v2 kernel
MODE_INFO_LIST:
		resb 68	; mode + 66 byte VESA vbe3 mode info (4F01h)
%endif

; 05/01/2021
ufont:		resb 1	; (VGA graphics) user font flags
			; bit 7 - permission flag for int 31h
			; bit 4 - 8x16 user font ready/loaded flag
			; bit 3 - 8x8 user font ready/loaded flag
			; bit 1 - select 8x16 user font (sysvideo)
			; bit 0 - select 8x8 user font (sysvideo)
		resb 1	; 19/01/2021
; 17/01/2021
srvsf:		resb 1	; 'save restore video state' permission flag
; 18/01/2021
srvso:		resb 1	; video state buffer save/restore option
VideoStateID:	resd 1	; used to verify state saved by same prog
; 29/01/2021
v_width:	resw 1	; screen (display page) width
v_ops:		resb 1	; 'sysvideo' graphics data transfer option
v_bpp:		resb 1	; bits per pixels ('sysvideo')
v_mem:		resd 1	; video memory ('sysvideo')
v_siz:		resd 1	; video page size ('sysvideo')
v_str:		resd 1	; window start adress ('sysvideo')
v_end:		resd 1	; window end (end+1) adress ('sysvideo')
; 31/01/2021
; 01/01/2021
;maskbuff:     ;resd 1	; user's bitmask buffer addr ('sysvideo')
maskcolor:	resd 1	; VGA/SVGA pixel mask color ('sysvideo')
; 27/02/2021
pixcount:	resd 1	; pixel count ('sysvideo' window ops)
; 02/02/2021
buffer8:	resd 2	; 8 bytes small buffer for 'sysvideo'

bss_end:

; 27/12/2013
_end:  ; end of kernel code