; PCI device register reader/writers.
; NASM version: Erdogan Tan (29/11/2016)

; 20/03/2017
NOT_BIT31 EQU 7FFFFFFFh
NOT_PCI32_PCI16	EQU 03FFFFFFFh ; NOT BIT31+BIT30

;===============================================================
; 8/16/32bit PCI reader
;
; Entry: EAX=PCI Bus/Device/fn/register number
;           BIT30 set if 32 bit access requested
;           BIT29 set if 16 bit access requested
;           otherwise defaults to 8bit read
;
; Exit:  DL,DX,EDX register data depending on requested read size
;
; Note: this routine is meant to be called via pciRegRead8, pciRegread16,
;	or pciRegRead32, listed below.
;
; Note2: don't attempt to read 32bits of data from a non dword aligned reg
;	 number.  Likewise, don't do 16bit reads from non word aligned reg #
; 
pciRegRead:
	push	ebx
	push	ecx
        mov     ebx, eax		; save eax, dh
        mov     cl, dh

        and     eax, NOT_PCI32_PCI16	; clear out data size request
        or      eax, BIT31		; make a PCI access request
        and     al, ~3 ; NOT 3		; force index to be dword

        mov     dx, PCI_INDEX_PORT
        out	dx, eax			; write PCI selector
	
        mov     dx, PCI_DATA_PORT
        mov     al, bl
        and     al, 3			; figure out which port to
        add     dl, al			; read to

	; 20/03/2017
	test    ebx, PCI32+PCI16
        jnz     short _pregr0
	in	al, dx			; return 8bits of data
        mov	dl, al
	mov     dh, cl			; restore dh for 8 bit read
	jmp	short _pregr2
_pregr0:	
	test    ebx, PCI32
        jnz	short _pregr1
	in	ax, dx
        mov     dx, ax			; return 16bits of data
	jmp	short _pregr2
_pregr1:
	in	eax, dx			; return 32bits of data
	mov	edx, eax
_pregr2:
	mov     eax, ebx		; restore eax
        and     eax, NOT_PCI32_PCI16	; clear out data size request
	pop	ecx
	pop	ebx
	retn

pciRegRead8:
        and     eax, NOT_PCI32_PCI16	; set up 8 bit read size
        jmp     short pciRegRead	; call generic PCI access

pciRegRead16:
        and     eax, NOT_PCI32_PCI16	; set up 16 bit read size
        or      eax, PCI16		; call generic PCI access
        jmp     short pciRegRead

pciRegRead32:
        and     eax, NOT_PCI32_PCI16	; set up 32 bit read size
        or      eax, PCI32		; call generic PCI access
        jmp     pciRegRead

;===============================================================
; 8/16/32bit PCI writer
;
; Entry: EAX=PCI Bus/Device/fn/register number
;           BIT31 set if 32 bit access requested
;           BIT30 set if 16 bit access requested
;           otherwise defaults to 8bit read
;        DL/DX/EDX data to write depending on size
;
;
; note: this routine is meant to be called via pciRegWrite8, pciRegWrite16,
; 	or pciRegWrite32 as detailed below.
;
; Note2: don't attempt to write 32bits of data from a non dword aligned reg
;	 number.  Likewise, don't do 16bit writes from non word aligned reg #
;
pciRegWrite:
	push	ebx
	push	ecx
        mov     ebx, eax		; save eax, edx
        mov     ecx, edx
	and     eax, NOT_PCI32_PCI16	; clear out data size request
        or      eax, BIT31		; make a PCI access request
        and     al, ~3 ; NOT 3		; force index to be dword

        mov     dx, PCI_INDEX_PORT
        out	dx, eax			; write PCI selector
	
        mov     dx, PCI_DATA_PORT
        mov     al, bl
        and     al, 3			; figure out which port to
        add     dl, al			; write to

	; 20/03/2017
	test    ebx, PCI32+PCI16
        jnz     short _pregw0
	mov	al, cl 			; put data into al
	out	dx, al
	jmp	short _pregw2
_pregw0:
	test    ebx, PCI32
        jnz     short _pregw1
	mov	ax, cx			; put data into ax
	out	dx, ax
	jmp	short _pregw2
_pregw1:
	mov	eax, ecx		; put data into eax 		
	out	dx, eax
_pregw2:
        mov     eax, ebx		; restore eax
        and     eax, NOT_PCI32_PCI16	; clear out data size request
        mov     edx, ecx		; restore dx
	pop	ecx
	pop	ebx
	retn

pciRegWrite8:
        and     eax, NOT_PCI32_PCI16	; set up 8 bit write size
        jmp	short pciRegWrite	; call generic PCI access

pciRegWrite16:
        and     eax, NOT_PCI32_PCI16	; set up 16 bit write size
        or      eax, PCI16		; call generic PCI access
        jmp	short pciRegWrite

pciRegWrite32:
        and     eax, NOT_PCI32_PCI16	; set up 32 bit write size
        or      eax, PCI32		; call generic PCI access
        jmp	pciRegWrite

;===============================================================
; PCIFindDevice: scan through PCI space looking for a device+vendor ID
;
; Entry: EAX=Device+Vendor ID
;
;  Exit: EAX=PCI address if device found
;	 EDX=Device+Vendor ID
;        CY clear if found, set if not found. EAX invalid if CY set.
;
; [old stackless] Destroys: ebx, esi, edi, cl
;
pciFindDevice:
	;push	cx
	push	eax
	;push	esi
	;push	edi

        mov     esi, eax                ; save off vend+device ID
        mov     edi, (80000000h - 100h) ; start with bus 0, dev 0 func 0

nextPCIdevice:
        add     edi, 100h
        cmp     edi, 80FFF800h		; scanned all devices?
        stc
        je      short PCIScanExit       ; not found

        mov     eax, edi                ; read PCI registers
        call    pciRegRead32
        cmp     edx, esi                ; found device?
        jne     short nextPCIdevice
        clc

PCIScanExit:
	pushf
	mov	eax, NOT_BIT31 	; 19/03/2017
	and	eax, edi	; return only bus/dev/fn #
	popf

	;pop	edi
	;pop	esi
	pop	edx
	;pop	ecx
	retn