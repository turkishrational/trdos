; PCI and AC97 codec functions for wav player
; Erdogan Tan (17/02/2017)
 
; ----------------------------------------------------------------------------
; PCI.ASM
; ----------------------------------------------------------------------------

; PCI device register reader/writers.
; NASM version: Erdogan Tan (29/11/2016)
; 		Last Update: 17/02/2017

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
	push	cx
        mov     ebx, eax                        ; save eax, dh
        mov     cl, dh
        and     eax, (~PCI32)+PCI16             ; clear out data size request
        or      eax, BIT31                      ; make a PCI access request
        and     al, ~3 ; NOT 3                  ; force index to be dword

        mov     dx, PCI_INDEX_PORT
        out     dx, eax                         ; write PCI selector

        mov     dx, PCI_DATA_PORT
        mov     al, bl
        and     al, 3                           ; figure out which port to
        add     dl, al                          ; read to

	in      eax, dx                         ; do 32bit read
        test    ebx, PCI32
        jz      short _pregr1

        mov     edx, eax                        ; return 32bits of data
_pregr1:
	mov     dx, ax                          ; return 16bits of data
        test    ebx, PCI32+PCI16
        jnz     short _pregr2
        mov     dh, cl                          ; restore dh for 8 bit read
_pregr2:
        mov     eax, ebx                        ; restore eax
        and     eax, (~PCI32)+PCI16             ; clear out data size request
	pop	cx
	pop	ebx
	retn

pciRegRead8:
        and     eax, (~PCI16)+PCI32             ; set up 8 bit read size
        jmp     short pciRegRead		; call generic PCI access

pciRegRead16:
        and     eax, (~PCI16)+PCI32		; set up 16 bit read size
        or      eax, PCI16			; call generic PCI access
        jmp     short pciRegRead

pciRegRead32:
        and     eax, (~PCI16)+PCI32		; set up 32 bit read size
        or      eax, PCI32			; call generic PCI access
        jmp     short pciRegRead

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
	push	cx
        mov     ebx, eax                        ; save eax, dx
        mov     cx, dx
        or      eax, BIT31                      ; make a PCI access request
        and     eax, ~PCI16 ; NOT PCI16         ; clear out data size request
        and     al, ~3 ; NOT 3                  ; force index to be dword

        mov     dx, PCI_INDEX_PORT
        out     dx, eax                         ; write PCI selector

        mov     dx, PCI_DATA_PORT
        mov     al, bl
        and     al, 3                           ; figure out which port to
        add     dl, al                          ; write to

        mov     eax, edx                        ; put data into eax
        mov     ax, cx

        out     dx, al
        test    ebx, PCI16+PCI32                ; only 8bit access? bail
        jz      short _pregw1

        out     dx, ax                          ; write 16 bit value
        test    ebx, PCI16                      ; 16bit requested?  bail
        jnz     short _pregw1

        out     dx, eax                         ; write full 32bit
_pregw1:
        mov     eax, ebx                        ; restore eax
        and     eax, (~PCI32)+PCI16             ; clear out data size request
        mov     dx, cx                          ; restore dx
	pop	cx
	pop	ebx
	ret

pciRegWrite8:
        and     eax, (~PCI16)+PCI32		; set up 8 bit write size
        jmp     short pciRegWrite		; call generic PCI access

pciRegWrite16:
        and     eax, (~PCI16)+PCI32		; set up 16 bit write size
        or      eax, PCI16			; call generic PCI access
        jmp     short pciRegWrite

pciRegWrite32:
        and     eax, (~PCI16)+PCI32		; set up 32 bit write size
        or      eax, PCI32			; call generic PCI access
        jmp     short pciRegWrite

; 17/02/2017 (Modifed by Erdogan Tan for various ICH device IDs)
;===============================================================
; PCIFindDevice: scan through PCI space looking for a device+vendor ID
;
;  ENTRY: none
;; Entry: EAX=Device+Vendor ID
;
;  Exit: EAX=PCI address if device found
;	 EDX=Device+Vendor ID
;        CY clear if found, set if not found. EAX invalid if CY set.
;
; [old stackless] Destroys: ebx, esi, edi, cl
;
pciFindDevice:
	;push	cx
	;push	eax ; *
	;push	esi
	;push	edi

 	;mov     esi, eax                ; save off vend+device ID

	; 17/02/2017
	mov	si, valid_ids	; address of Valid ICH (AC97) Device IDs
	mov	cx, valid_id_count
pfd_0:
       	mov     edi, (80000000h - 100h) ; start with bus 0, dev 0 func 0
nextPCIdevice:
        add     edi, 100h
        cmp     edi, 80FFF800h		; scanned all devices?
        ;stc
        ;je 	short PCIScanExit       ; not found
	jb	short pfd_1
	mov     edi, 80000000h
	add	si, 4 ; scan for next device ID
	loop	pfd_1	 
	stc	
	;jmp 	short PCIScanExit
	retn
pfd_1:
        mov     eax, edi                ; read PCI registers
        call    pciRegRead32
        ;cmp    edx, esi                ; found device?
        cmp	edx, dword [si]
	jne     short nextPCIdevice
        ;clc
PCIScanExit:
	;pushf
	mov	eax, BIT31
	not	eax
	and	eax, edi		; return only bus/dev/fn #
	;popf

	;pop	edi
	;pop	esi
	;pop	edx ; *
	;pop	cx
	retn

; ----------------------------------------------------------------------------
; CODEC.ASM
; ----------------------------------------------------------------------------

; codec configuration code.  Not much here really.
; NASM version: Erdogan Tan (29/11/2016)

; enable codec, unmute stuff, set output rate to 44.1
; entry: ax = desired sample rate
;

codecConfig:
	; 17/02/2017 
	; 07/11/2016 (Erdogan Tan)
	PORT_NABM_GLB_CTRL_STAT equ 60h

	;mov    dx, [NAMBAR]               	; mixer base address
	;add	dx, CODEC_EXT_AUDIO_REG	       	; 28h  	  
	;in	ax, dx
	;and	ax, 1
	;jnz	short _ssr
	;pop	ax
	;jmp	short cconf_1

_ssr:
	mov    	dx, [NAMBAR]               	
	add    	dx, CODEC_EXT_AUDIO_CTRL_REG  	; 2Ah  	  
	in     	ax, dx
	or	ax, 1
	out	dx, ax 				; Enable variable rate audio

        call    delay1_4ms
        call    delay1_4ms
        call    delay1_4ms
        call    delay1_4ms

	mov	ax, [sample_rate] ; 17/02/2017 (Erdogan Tan)

	mov    	dx, [NAMBAR]               	
	add    	dx, CODEC_PCM_FRONT_DACRATE_REG	; 2Ch  	  
	out	dx, ax 				; out sample rate
		
	;mov    dx, [NAMBAR]               	
	;add    dx, CODEC_LR_ADCRATE_REG 	; 32h  	  
	;out	dx, ax 

        call    delay1_4ms
        call    delay1_4ms
        call    delay1_4ms
        call    delay1_4ms

;cconf_1:
	mov     dx, [NAMBAR]			; mixer base address
        add     dx, CODEC_RESET_REG  		; reset register
        mov	ax, 42
	out     dx, ax                          ; reset

	mov     dx, [NABMBAR]			; bus master base address
        add     dx, PORT_NABM_GLB_CTRL_STAT
        mov	ax, 2
	out     dx, ax                         

	mov	cx, 7
_100ms:
	push	cx
        call    delay1_4ms
        call    delay1_4ms
        call    delay1_4ms
        call    delay1_4ms
	pop	cx
	loop	_100ms	
	
  	mov     dx, [NAMBAR]
  	add     dx, CODEC_MASTER_VOL_REG        ;02h 
  	xor     ax, ax ; ; volume attenuation = 0 (max. volume)
  	out     dx, ax
 
  	mov     dx, [NAMBAR]
  	add     dx, CODEC_MASTER_MONO_VOL_REG   ;06h 
  	;xor    ax, ax
  	out     dx, ax

  	mov     dx, [NAMBAR]
  	add     dx, CODEC_PCBEEP_VOL_REG        ;0Ah 
  	;xor    ax, ax
  	out     dx, ax

  	mov     dx, [NAMBAR]
  	add     dx, CODEC_PCM_OUT_REG		;18h 
  	;xor    ax, ax
  	out     dx, ax

        call    delay1_4ms
        call    delay1_4ms
        call    delay1_4ms
        call    delay1_4ms

        retn