; ****************************************************************************
; qsort.s (TRDOS 386, TRDOS v2.0 - sample binary file, 'qsort.prg')
; ----------------------------------------------------------------------------
; QSORT.PRG ! Sorting Method TEST program !
;
; 07/04/2017
;
; [ Last Modification: 07/04/2017 ]
;
; Assembler: NASM 2.11
; ****************************************************************************

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
_unlink	equ 10
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

[BITS 32]

[ORG 0] 

;=============================================================================
;               CODE
;=============================================================================

[BITS 32] ; 80386 Protected Mode (32 bit) intructions

[ORG 0] 

start:
		sys	_msg, prg_msg, 255, 0Fh

		mov	esi, SData
		push	esi
		mov	edi, SData_End
		push	edi
		call	qsort

		mov	esi, SData
		mov	edi, SData_End 
_1:
		lodsd
		call	decimal
		sys	_msg, decimal_num_str, 255, 0Eh		
		cmp	esi, edi
		jna	short _1

		sys	_msg, _nextline, 3, 07h		
terminate:
		sys 	_exit   ; INT 40h
here:
		jmp	short here

; OMNISCENT.ASM (sncomni.s)
;***********************************************
;* descript. : recursive index quicksort       *
;* parameter : l,r:stack left and right border *
;* sideeffect: ax,bx,cx,dx,si,di               *
;* back      : none		               *
;***********************************************

; Derived from source code of 'OMNISCENT.ASM' by Dirk Küppers
;          SNC_OMNI.COM	 (MSDOS) intro file, 1997

qsort:
		pop	eax 	; get address
		pop	ecx	; get 2. param r
		pop	ebx	; get 1. param l
		push	eax	; store address

		cmp	ecx, ebx
		;jle	short QSendrek
		jna	short QSendrek
		mov	esi, ebx
		mov	edi, ecx  	
		mov	edx, dword [esi]
		jmp	short QSwhile2
QSrepeat:
QSwhile1:	cmp	dword [esi], edx
		;jle	short QSwhile2
		jna	short QSwhile2
		add	esi, 4
		jmp	short QSwhile1
QSwhile2:
		cmp	dword [edi], edx
		;jnl	short QSwhile2e
		jae	short QSwhile2e
		sub	edi, 4
		jmp	short QSwhile2
QSwhile2e:
		cmp	esi, edi
		;jg	short QSnoswap
		;jg	short _QSnoswap
		ja	short _QSnoswap
		lodsd
		xchg	eax, dword [edi]
		mov	dword [esi-4], eax
		sub	edi, 4
QSnoswap:
		cmp	esi, edi
		;jle	short QSrepeat
		jna	short QSrepeat
_QSnoswap:
		push	esi 
		push	ecx
		push	ebx
		push	edi
		call	qsort
		call	qsort
QSendrek:
		retn


decimal:
		; Input:
		; eax = binary number
		; Output:
		; decimal number str at offset _number  
		;
		mov	ebp, esp	
		mov	ecx, 10
_2:
		xor	edx, edx
		div 	ecx
		push	edx
		and	eax, eax
		jnz	short _2				
		mov	ebx, _number
_3:
		pop	eax
		add	al, '0'
		mov	[ebx], al
		inc	ebx
		cmp	esp, ebp
		jb	short _3				
		mov	byte [ebx], 0
		retn
	

;=============================================================================
;               DATA
;=============================================================================

prg_msg:
		db	'ERDOGAN TAN - TRDOS 386 Sorting Test - QSORT.PRG'
		db	0Dh, 0Ah
		db	'07/04/2017'
		db	0Dh, 0Ah
		db	0 

decimal_num_str:
		db 0Dh, 0Ah
_number:	db '0000000'
		db 0	

SData:		dd 47128
		dd 1281649
		dd 11268
		dd 15
		dd 629
		dd 2186
		dd 5839426
		dd 853116
		dd 91000
		dd 1000
		dd 100
		dd 520
SData_End:	dd 10000
_nextline:
		db 0Dh, 0Ah, 0

bss_end: