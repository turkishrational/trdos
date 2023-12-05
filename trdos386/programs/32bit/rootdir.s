; ****************************************************************************
; rootdir.s - TRDOS 386 (TRDOS v2.0) TEST Program - display root dir address 
;	      ((Get Logical DOS Drive Description Table))
; ----------------------------------------------------------------------------
; 15/09/2020
;
; ****************************************************************************

; 14/07/2020
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
; GET LOGICAL DRIVE DESCRIPTION TABLE
;========================================================================

; Logical DOS Disks
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

;========================================================================
;------------------------------------------------------------------------

[BITS 32] ; We need 32-bit instructions for protected mode

[ORG 0] 

START_CODE:
	sys	_ldrvt, 2, ldrvtbuff  ; get logical dos drive table
	jc 	error

	xor	ebx, ebx
	mov	eax, [ldrvtbuff+LD_ROOTBegin]
	mov	ecx, 10
div_repeat:
	xor	edx, edx
	div	ecx
	add	dl, '0'
	push	edx
	inc	ebx
	or	eax, eax
	jnz	short div_repeat
	mov	edi, number
first_zero:
	pop	eax
	dec	ebx
	jz	short last_zero
	cmp	al, '0'
	jna	short first_zero
non_zero:
	stosb	
	pop	eax
	dec	ebx
	jnz	short non_zero	
last_zero:
	stosb
	;sub	al, al
	;stosb	

p_rootdir_addr:
	mov 	esi, rootdir_msg
	call	print_msg
	mov	esi, crlf
	call 	print_msg
	
	mov	esi, ldrvtbuff
	xor	eax, eax
	mov	[zeroword], ax
hexrow:
	mov	ecx, 16
	mov	edi, hextable
hexchar:
	lodsb
	call	bintohex
	stosw
	mov	ax, 'h '
	stosw
	loop	hexchar

	push	esi	
	mov	si, crlf
	call 	print_msg
	mov	si, hextable
	call 	print_msg
	pop	esi
	cmp	esi, ldrvtbuff+256
	jb	short hexrow		
	mov	esi, crlf
p_msg:
	call 	print_msg
here:	
	sys	_exit
	; hlt
	; jmp 	short here

error:
	mov 	esi, msg_err
	jmp 	short p_msg
 
print_msg:
	sys 	_msg, esi, 255, 0Fh ; message with white color 
				    ; (max. 255 chars)
	retn

;------------------------------------------------------------------------

bintohex:
	; INPUT ->
	; 	AL = byte (binary number)
	; OUTPUT ->
	;	AX = hexadecimal string
	;
	xor	ebx, ebx
	mov	bl, al
	shr	bl, 4
	mov	bl, [ebx+hexchrs] 	 	
	xchg	bl, al
	and	bl, 0Fh
	mov	ah, [ebx+hexchrs] 
	retn

hexchrs:
	db	'0123456789ABCDEF'

;------------------------------------------------------------------------
;  root directory message
;------------------------------------------------------------------------

rootdir_msg:	db 0Dh, 0Ah
		db 'Drive C: '
		db 0Dh, 0Ah
		db 'Root Directory Address : '
number:
		times 11 db 0
crlf:
		db 0Dh, 0Ah, 0

msg_err:
		db 0Dh, 0Ah 
                db 'Error ! (Drive not ready!)'
		db 0Dh, 0Ah, 0

;------------------------------------------------------------------------
;  buffer & hex table
;------------------------------------------------------------------------

bss_start:

ABSOLUTE bss_start

ldrvtbuff: resb 256
hextable:  resb 64
zeroword:  resw 1
	   		
bss_end: