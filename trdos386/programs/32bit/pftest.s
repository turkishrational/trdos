; ****************************************************************************
; pftest.s - TRDOS 386 (TRDOS v2.0) Kernel - PAGE FAULT address test
; ----------------------------------------------------------------------------
;
; 22/09/2024
;
; [ Last Modification: 22/09/2024 ]
;
; ****************************************************************************

; 20/08/2024 ; TRDOS 386 v2.0.9 (exit code)
; 20/08/2017
; 01/03/2017
; 16/10/2016
; 29/04/2016
; TRDOS 386 system calls (temporary list!)
_ver 	equ 0
_exit 	equ 1
_fork 	equ 2
_read 	equ 3
_write	equ 4
_open	equ 5
_close 	equ 6
_wait 	equ 7
_creat 	equ 8
_link 	equ 9
_unlink	equ 10 ; _delete
_exec	equ 11
_chdir	equ 12
_time 	equ 13
_mkdir 	equ 14
_chmod	equ 15
_chown	equ 16
_break	equ 17
_stat	equ 18
_seek	equ 19
_tell 	equ 20
_mount	equ 21
_umount	equ 22
_setuid	equ 23
_getuid	equ 24
_stime	equ 25
_quit	equ 26
_intr	equ 27
_fstat	equ 28
_emt 	equ 29
_mdate 	equ 30
_video 	equ 31
_audio	equ 32
_timer	equ 33
_sleep	equ 34
_msg    equ 35
_geterr	equ 36
_fpsave	equ 37
_pri	equ 38
_rele	equ 39
_fff	equ 40
_fnf	equ 41
_alloc	equ 42
_dalloc equ 43
_calbac equ 44
_dma	equ 45
_stdio  equ 46	;  TRDOS 386 v2.0.9

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
; PAGE FAULT TEST
;========================================================================

;; In TRDOS 386, user's address space starts at the end of 1st 4MB.
;; So, 0 is at 4MB and last usable virtual address is 4GB-4MB 
;; If an addr beyond 0FFBFFFFFh is used in the code, a page fault is expected.

[BITS 32] ; 32-bit intructions

[ORG 0] 
START_CODE:
	jmp	skip
	times	256-($-START_CODE) db 0FFh	 
skip:  
	mov	esp, 0FFBE0000h
	xor	edi, edi ; not necessary
	mov	ecx, 256/4
	xor	eax, eax ; not necessary
	rep	stosd
	;;;
	dec	eax
	mov	edi, 0FFBE0000h
	mov	ecx, 65536/4
	rep	stosd
	
wloop:
	mov	eax, edi
	call	calchex
	mov	[hexnum], edx
	mov	[hexnum+4], eax
	sys	_msg, hexstr, 255, 0Fh
	mov	[edi], ecx
	nop
	add	edi, 4096
	cmp	edi, 0FFC00000h
	jnb	short wloop_ok
	jmp	short wloop

wloop_ok:
	mov	eax, edi
	call	calchex
	mov	[hexnum], edx
	mov	[hexnum+4], eax
	sys	_msg, hexstr, 255, 0Ch

	mov	eax, -1
	stosd
	nop

	cmp	edi, 0FFFFFF00h
	jb	short wloop_ok

	sys	_msg, _ok, 255, 07h

	; if we are here, there was no page fault above
	sys	_exit, 0

hang:
	jmp	short hang

calchex:
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
hexchrs:
	db '0123456789ABCDEF'

hexstr:
	db 0Dh,0Ah
hexnum:
	db "00000000h",0
_ok:
	db 0Dh,0Ah,"OK.", 0Dh,0Ah,0