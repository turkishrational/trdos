; ****************************************************************************
; disksize.s (TRDOS 386, TRDOS v2.0 - sample binary file)
; ----------------------------------------------------------------------------
; DISKSIZE.PRG ! TEST program !
;
; 29/08/2020
;
; Derived from 'args386.s' & 'dsectpm3.s' source code for TRDOS 386 v2
;
; [ Last Modification: 29/08/2020 ]
;
; ****************************************************************************

; 14/07/2020 (lastdrv.s)
; 31/12/2017
; TRDOS 386 (v2.0) system calls
_ver 	equ 0
_exit 	equ 1
_fork 	equ 2
_read 	equ 3
_write	equ 4
_open	equ 5
_close 	equ 6
_wait 	equ 7
_create	equ 8
_rename	equ 9
_delete	equ 10
_exec	equ 11
_chdir	equ 12
_time 	equ 13
_mkdir 	equ 14
_chmod	equ 15
_rmdir	equ 16
_break	equ 17
_drive	equ 18
_seek	equ 19
_tell 	equ 20
_memory	equ 21
_prompt	equ 22
_path	equ 23
_env	equ 24
_stime	equ 25
_quit	equ 26	
_intr	equ 27
_dir	equ 28
_emt 	equ 29
_ldrvt 	equ 30
_video 	equ 31
_audio	equ 32
_timer	equ 33
_sleep	equ 34
_msg    equ 35
_geterr	equ 36
_fpstat	equ 37
_pri	equ 38
_rele	equ 39
_fff	equ 40
_fnf	equ 41
_alloc	equ 42
_dalloc equ 43
_calbac equ 44
_dma	equ 45	

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


; TRDOS 386 (and Retro UNIX 386 v1) system call format:
; sys systemcall (eax) <arg1 (ebx)>, <arg2 (ecx)>, <arg3 (edx)>

;========================================================================
; GET PYHSICAL DISK SIZE AND (VIRTUAL) CHS PARAMETERS
;========================================================================

[BITS 32] ; We need 32-bit intructions for protected mode

[ORG 0] 

START_CODE:
	;mov	esi, esp
	;lodsd
	pop	eax
	;and	eax, eax
	;jz	short terminate
	;cmp	eax, 2
	;ja	short usage
	cmp	eax, 2 ; file name (argument 0) and argument 1
	jne	short usage
	pop	eax ; pass argument 0 (file name)
getarg:
	;lodsd
	;pop	eax
	pop	esi
	cmp	word [esi], 'fd'	
	je	short floppy_disk
	cmp	word [esi], 'hd'
	jne	short invalidname
hard_disk:
	mov	word [drvname], 'hd'
	mov	al, [esi+2]
	mov	byte [drvname+2], al
	sub	al, '0'
	cmp	al, 0
	jb	short invalidname
	cmp	al, 3 
	ja	short invalidname
	add	al, 80h
	; al = physical disk number (for bios)
	call	getdiskparms
	jmp	short writediskinfo
usage:
	sys	_msg, msgUsage, 255, 0Fh
	jmp	short terminate

invalidname:
	sys	_msg, msgInvalidName, 255, 0Fh
	jmp	short terminate

floppy_disk:
	mov	word [drvname], 'fd'
	mov	al, [esi+2]
	mov	byte [drvname+2], al
	sub	al, '0'
	cmp	al, 0
	jb	short invalidname
	cmp	al, 1 
	ja	short invalidname
	; al = physical disk number (for bios)

	call	getdiskparms

writediskinfo:
	sys	_msg, msgDiskInfo, 255, 0Fh	

	sys	_msg, nexline, 2, 07h

terminate: 
	sys	_exit
halt:
	jmp	short halt

drvnotrdy:
	pop	eax ; pop return address
		    ; to the caller
		    			
	sys	_msg, msgDrvNotRdy, 255, 0Fh
	jmp	short terminate

getdiskparms:
	mov	dl, al
	mov	[drv], al

	cmp	al, 80h
	jb	short gdp_fd	

	mov	ax, 1500h ; ah = 15h, get disk size
	int	33h
	jc	short drvnotrdy
	
	mov	[disksz], dx ; low word
	mov	[disksz+2], cx ; high word

gdp_fd:
	mov	dl, [drv]
	xor	ebx, ebx ; 0, do not use DPT buffer
	mov	ah, 08h ; get disk parameters (logical)
	int	33h
	jc	short drvnotrdy

	mov	al, ch ; last cylinder (bits 0-7)
	mov	ah, cl ; 
	shr	ah, 6  ; last cylinder (bits 8-9)
	inc	ax  ; convert max. cyl number to cyl count
	inc	dh  ; convert last head to heads
	and	cx, 3Fh ; 6 bits for spt	

	push	ecx ; cl = sectors per track
	push	edx ; dh = heads
	; ax = cylinders

	cmp	byte [drv], 80h
	jnb	short gdp_hd

	mov	dl, dh
	xor	dh, dh

	; calculate floppy disk size
	; ((1.44MB, 3.5" fd))
	;	(dx = 2)
	;	(cx = 18)
	;	(ax = 80)
	;	(size = 2880 sectors)

	push	eax
	mul	dx  ; ax = dx*ax, dx = 0
	mul	cx	
	mov	[disksz], ax
	mov	[disksz+2], dx ; = 0
	pop	eax

gdp_hd:
	; Writing CHS values on screen
	
	; ax = number of cylinders
	mov	edi, cylinders
	call	write_dhex
	pop	edx	
	mov	al, dh
	; al = number of heads
	mov	edi, heads
	call	write_hex
	pop	eax
	; al = sectors per track
	mov	edi, spt
	call	write_hex
	
	mov	ax, [disksz+2]
	mov	edi, disksize
	call	write_dhex
	mov	ax, [disksz]
	mov	edi, disksize+4
	;;call	write_dhex		 
	;;retn
	;jmp	write_dhex

write_dhex:
	mov	bl, ah
        shr     bl, 4
        call    dhgd
	mov	bl, ah
	call	dhgd

write_hex:
	mov	bl, al
        shr     bl, 4
	call	dhgd
	mov	bl, al
	;call	dhgd
	;retn
dhgd:
	push	eax
	and	ebx, 0Fh
        add     ebx, hex_digits
        mov     al, [ebx]
	stosb
	pop	eax
	retn

msgUsage:
	db 0Dh, 0Ah
	db 'TRDOS 386 v2 - Physical Disk Size and (virtual) CHS Parameters'
	db 0Dh, 0Ah
	db 'test program (v1) by Erdogan Tan - 29/08/2020'
	db 0Dh, 0Ah
	db 0Dh, 0Ah
	db 'Usage: disksize <diskname>'
	db 0Dh, 0Ah
	db 0Dh, 0Ah
	db '(valid disk names: fd0, fd1, hd0, hd1, hd2, hd3)'
	db 0Dh, 0Ah, 0Dh, 0Ah, 0

msgDrvNotRdy:
	db 0Dh, 0Ah
	db 'DISK ERROR !', 0Dh, 0Ah
	db 'Physical Disk Drive not ready or not valid for TRDOS 386 v2 !'
	db 0Dh, 0Ah, 0Dh, 0Ah
	db 0

msgInvalidName:
	db 0Dh, 0Ah
	db "Invalid physical disk name !"
	db 0Dh, 0Ah, 0Dh, 0Ah
	db "Valid disk names:"
	db 0Dh, 0Ah
	db " 	'fd0' or 'fd1' (for floppy disks, A: or B:)"
	db 0dh, 0Ah
	db "	'hd0' to 'hd3' (for hard disks, 1 to 4, from C: to ..)"
	db 0Dh, 0Ah, 0Dh, 0Ah, 0

nexline:
	db 0Dh, 0Ah, 0


hex_digits:
hexchrs:
	db '0123456789ABCDEF'

	db 0

msgDiskInfo:
	db 07h
	db 0Dh, 0Ah
drvname:
	db 'XXX'
	db ' - '	
	db 'Physical disk size and CHS (logical) values'
	db 0Dh, 0Ah, 0Dh, 0Ah

	db 'Size (in sectors)   : '
disksize:
	db 'XXXXXXXXh'
	db 0Dh, 0Ah
	db 0Dh, 0Ah
	
	db 'Number of Cylinders : '
cylinders:
	db 'XXXXh'
	db 0Dh, 0Ah
	db 'Number of Heads     : '
heads:
	db 'XXh'
	db 0Dh, 0Ah
	db 'Sectors per Track   : '
spt:
	db 'XXh'
	db 0Dh, 0Ah
	db 0Dh, 0Ah
	db 0

bss_start:

ABSOLUTE bss_start

drv:	resb 1

alignb 4

disksz:	resd 1

bss_end:
	
_end: