; DOS based .WAV player using AC'97 and codec interface.
; ---------------------------------------------------------------
; NASM version: Erdogan Tan (29/11/2016)
; Last Update: 17/02/2017 (by Erdogan Tan)

; ICHWAV.ASM

; player internal variables and other equates.
BUFFERSIZE      equ     64 * 1024       ; 64k file buffer size.
ENDOFFILE       equ     BIT0            ; flag for knowing end of file

;===========================================================================
; entry: none.  File is already open and [filehandle] filled.
; exit:  not until the song is finished or the user aborts.
;
playWav:
	; clear buffer 2
        push	es
	mov     ax, [WAV_BUFFER2]
	mov	es, ax
	sub	ax, ax
	mov	di, ax ; 17/02/2017
	mov	cx, (BUFFERSIZE/2)
	rep	stosw
	pop	es	     
	
       ; load 64k into buffer 1
        mov     ax, [WAV_BUFFER1]
        call    loadFromFile

       ; and 64k into buffer 2
	mov     ax, [WAV_BUFFER2]
       	call    loadFromFile

; register reset the DMA engine.  This may cause a pop noise on the output
; lines when the device is reset.  Prolly a better idea to mute output, then
; reset.
;
        mov     dx, [NABMBAR]
        add     dx, PO_CR_REG		; set pointer to Ctrl reg
        mov	al, RR			; set reset
	out     dx, al			; self clearing bit

; write last valid index to 31 to start with.
; The Last Valid Index register tells the DMA engine when to stop playing.
; 
; As we progress through the song we change the last valid index to always be
; something other than the index we're currently playing.  
;
        ;;mov   al, 1
        ;mov	al, 31
	;call   setLastValidIndex

; create Buffer Descriptor List
;
; A buffer descriptor list is a list of pointers and control bits that the
; DMA engine uses to know where to get the .wav data and how to play it.
;
; I set it up to use only 2 buffers of .wav data, and whenever 1 buffer is
; playing, I refresh the other one with good data.
;
;
; For the control bits, you can specify that the DMA engine fire an interrupt
; after a buffer has been processed, but I poll the current index register
; to know when it's safe to update the other buffer.
;
; I set the BUP bit, which tells the DMA engine to just play 0's (silence)
; if it ever runs out of data to play.  Good for safety.
;
        push    es
        mov     ax, [BDL_BUFFER]		; get segment # for BDL
        mov     es, ax

        mov     cx, 32 / 2                      ; make 32 entries in BDL
        xor     di, di                          
_0:

; set buffer descriptor 0 to start of data file in memory
        movzx   eax, word [WAV_BUFFER1]
        shl     eax, 4                          ; convert seg:off ->0:offset
        stosd                                   ; store pointer to wavbuffer1

;
; set length to 32k samples.  1 sample is 16bits or 2bytes.
; Set control (bits 31:16) to BUP, bits 15:0=number of samples.
; 

; 17/02/2017 (Erdogan Tan)
; Intel® 82801AA (ICH) & Intel® 82801AB (ICH0) I/O Controller Hub AC ’97
; Programmer’s Reference Manual

; 2.2.1 Buffer Descriptor List  (on Page 13)
	;
	;  Generic Form of Buffer Descriptor
	;  ---------------------------------
	;  63   62    61-48    47-32   31-0
	;  ---  ---  --------  ------- -----
	;  IOC  BUP -reserved- Buffer  Buffer
	;		      Length   Pointer
	;		      [15:0]   [31:0]
	;
	;  IOC:	Interrupt On Completion. 
	;	1 = Enabled. 
	;	    When this is set, it means that the controller should
	;	    issue an interrupt upon completion of this buffer.
	;	    It should also set the IOC bit in the status register
	;	0 = Disabled	
	;
	;  BUP: Buffer Underrun Policy.
	;       0 = When this buffer is complete,
	;	    if the next buffer is not yet ready 
	;	    (i.e., the last valid buffer has been processed),
	;	    then continue to transmit the last valid sample.
	;	1 = When this buffer is complete,
	;     	    if this is the last valid buffer, transmit zeros after
	;	    this buffer has been processed completely.
	;	    This bit typically is set only if this is the last 
	;	    buffer in the current stream.
	;
	; [31:0]: Buffer pointer. This field points to the location of
	;	  the data buffer. Since samples can be as wide as one
	;	  word, the buffer must be aligned with word boundaries,
	;	  to prevent samples from straddling DWord boundaries.
	;
	; [15:0]: Buffer Length: This is the length of the data buffer,
	;	  in number of samples. The controller uses this data
	;	  to determine the length of the buffer, in bytes.
	;	  "0" indicates no sample to process.

; ICH2AC97.INC

;	IOC	equ     BIT31   ; Fire an interrupt whenever this
				; buffer is complete.

;	BUP	equ     BIT30   ; Buffer Underrun Policy.
				; if this buffer is the last buffer
				; in a playback, fill the remaining
				; samples with 0 (silence) or not.
				; It's a good idea to set this to 1
				; for the last buffer in playback,
				; otherwise you're likely to get a lot
				; of noise at the end of the sound.
;
; Bits 15:0 contain the length of the buffer, in number of samples, which
; are 16 bits each, coupled in left and right pairs, or 32bits each.
; Luckily for us, that's the same format as .wav files.
;
; A value of FFFF is 65536 samples.  Running at 44.1Khz, that's just about
; 1.5 seconds of sample time.  FFFF * 32bits is 1FFFFh bytes or 128k of data.
;
; A value of 0 in these bits means play no samples.
;

; ICHWAV.ASM

	mov	eax, BUFFERSIZE / 2 ; size of half buffer (32K)
	or	eax, IOC + BUP
	stosd

; 2nd buffer:

        movzx   eax, word [WAV_BUFFER2]
        shl     eax, 4                          ; convert seg:off ->0:offset
        stosd                                   ; store pointer to wavbuffer2

; set length to 64k (32k of two 16 bit samples)
; Set control (bits 31:16) to BUP, bits 15:0=number of samples
; 
	mov	eax, BUFFERSIZE / 2
	or	eax, IOC + BUP
	stosd

        loop    _0
        pop     es

;
; tell the DMA engine where to find our list of Buffer Descriptors.
; this 32bit value is a flat mode memory offset (ie no segment:offset)
;
; write NABMBAR+10h with offset of buffer descriptor list
;
        movzx   eax, word [BDL_BUFFER]
	shl     eax, 4                          ; convert seg:off to 0:off
        mov     dx, [NABMBAR]
        add     dx, PO_BDBAR_REG                ; set pointer to BDL
        out     dx, eax                         ; write to AC97 controller
;
;
; All set.  Let's play some music.
;
;
	; 08/12/2016
	; 07/10/2016
        ;mov    al, 1
        mov	al, 31
	call    setLastValidIndex

	mov	byte [tLoop], 1 ; 30/11/2016

	; 17/02/2017
        mov     dx, [NABMBAR]
        add     dx, PO_CR_REG                   ; PCM out Control Register
        mov	al, IOCE + RPBM	; Enable 'Interrupt On Completion' + run
				; (LVBI interrupt will not be enabled)
        out     dx, al                          ; Start bus master operation.

; while DMA engine is running, examine current index and wait until it hits 1
; as soon as it's 1, we need to refresh the data in wavbuffer1 with another
; 64k.  Likewise when it's playing buffer 2, refresh buffer 1 and repeat.
   
	; 08/12/2016
	; 28/11/2016
p_loop:
	call    check4keyboardstop      ; keyboard halt?
        jnc     short r_loop

	mov	byte [tLoop], 0
	retn
r_loop:
	; 07/12/2016 - Erdogan Tan
	nop
	nop
	nop
	; 17/02/2017
	mov	al, [tBuff]
	dec	al ; 1-32 -> 0-31 or 0 -> 0FFh
	js	short  p_loop
	mov	byte [tBuff], 0 ; reset
	and	al, 1
	jnz	short q_loop
        mov     ax, [WAV_BUFFER2] ; [tBuff]=2 (from tuneLoop)
        call    loadFromFile
	jc	short _exit_
	jmp	short r_loop
q_loop:
     	mov     ax, [WAV_BUFFER1] ; [tBuff]=1 (from tuneLoop)
        call    loadFromFile
	jc	short _exit_
	jmp	short r_loop
			
tuneLoop:
	; 08/12/2016
	; 28/11/2016 - Erdogan Tan
	
	cmp	byte [tLoop], 1
	jb	short _exit
_tlp_1:	
	; 17/02/2017
	call	getCurrentIndex
	inc	al ; 0-31 -> 1-32
	mov	[tBuff], al

	; 17/02/2017 - Buffer switch test (temporary)
	push	es 
	mov	si, 0B800h ; video display page segment
	mov	es, si
	sub	si, si ; 0
	mov	ah, 4Eh
	and	al, 1
	add	al, '1'
	mov	[es:si], ax ; show current play buffer (1, 2)
	pop	es

	; 17/02/2017

	;test	byte [irq_status], LVBCI ; last buff completion intr.
	;jz	short _tlp2 ; BCIS ; Buffer completion interrupt

	; Last Valid Buffer Completion Interrupt (LVBCI).
	;   1 = Last valid buffer has been processed. 
	;	It remains active until cleared by software. 
	;	This bit indicates the occurrence of the event 
	;	signified by the last valid buffer being processed.
	;	Thus, this is an event status bit that can be cleared
	;	by software once this event has been recognized.
	;	This event causes an interrupt if the enable bit
	;	in the Control Register is set. The interrupt is
	;	cleared when the software clears this bit.
	;	In the case of Transmits (PCM out, Modem out) this bit
	;	is set, after the last valid buffer has been
	;	fetched (not after transmitting it).
	;	While in the case of Receives, this bit is set after
	;	the data for the last buffer has been written to memory.
	;   0 = Cleared by writing a "1" to this bit position.

	; Note: We are not using LVBCI 
	;     Last Buffer Completion Interrupt !!!

	; 17/02/2017
        ;mov     dx, [NABMBAR]
        ;add     dx, PO_SR_REG	; PCM out Status register
        ;mov     al, [irq_status]
        ;out     dx, al		; Clear (LVBCI) interrupt status
	;retn

;_tlp2:
	; Buffer Completion Interrupt Status (BCIS).
	;   1 =	Set by the hardware after the last sample 
	; 	of a buffer has been processed, AND if the Interrupt
	; 	on Completion (IOC) bit is set in the command byte of 
	; 	the buffer descriptor. It remains active
	; 	until cleared by software.
	;   0 =	Cleared by writing a "1" to this bit position.

	; 17/02/2017
        mov     dx, [NABMBAR]
        add     dx, PO_SR_REG	; PCM out Status register
        mov     al, [irq_status]
        out     dx, al		; Clear (BCI) interrupt status
	retn
_exit_:
	mov	byte [tLoop], 0
_exit:
        ; finished with song, stop everything
	mov     dx, [NABMBAR]		
        add     dx, PO_CR_REG           ; PCM out Control Register
        mov     al, 0
        out     dx, al                  ; stop player
_return:
	retn

; load data from file in 32k chunks.  Would be nice to load 1 64k chunk,
; but in DOS you can only load FFFF bytes at a time.
;
; entry: ax = segment to load data to
; exit: CY set if end of file reached.
; note: last file buffer is padded with 0's to avoid pops at the end of song.
; assumes file is already open.  uses [filehandle]
;
loadFromFile:
	; 17/02/2017
        push    ax
        push    cx
        push    dx
	push	bx
	;push	es
	;push	ds
	
        test    byte [flags], ENDOFFILE		; have we already read the
        stc					; last of the file?
        jnz     endLFF
    
	mov	[fbs_seg], ax ; save buffer segment
	xor	dx, dx
lff_0:
	mov	[fbs_off], dx ; buffer offset

	mov     bx, (BUFFERSIZE / 2)              ; 32k chunk
	mov	cl, [fbs_shift]   
	and	cl, cl
	jz	short lff_1 ; stereo, 16 bit	

	; fbs_shift =
	;	2 for mono and 8 bit sample (multiplier = 4)
	;	1 for mono or 8 bit sample (multiplier = 2)
	shr	bx, cl ; 32K / multiplier

	mov	ax, cs
	mov	dx, temp_buffer ; temporary buffer for wav data
lff_1:
	; 17/02/2017 (stereo/mono, 8bit/16bit corrections)
	; load file into memory
        mov	cx, bx                         
	mov	bx, [filehandle]
	mov     ds, ax
       	mov	ah, 3fh
	int	21h

	mov	bx, cs
	mov	ds, bx

	jc	short lff_9 ; error !

	and	ax, ax
	jz	short lff_10
	
	mov	bl, [fbs_shift] ; shift count  
	or	bl, bl
	jz	short lff_7 ; 16 bit stereo samples

	push	es
	push	di
	push	si
	mov	di, [fbs_off]
	mov	si, [fbs_seg] ; buffer segment
	mov	es, si
	mov	si, temp_buffer ; temporary buffer address
	mov	cl, [bps] ; bits per sample (8 or 16)
	cmp	cl, 8
	jne	short lff_4 ; 16 bit samples
	; 8 bit samples
	mov	cx, ax ; byte count
	dec	bl  ; shift count, 1 = stereo, 2 = mono
	jz	short lff_3 ; 8 bit, stereo
lff_2:
	; mono & 8 bit
	lodsb
	shl	ax, 8 ; convert 8 bit sample to 16 bit sample
	stosw	; left channel
	stosw	; right channel
	loop	lff_2
	jmp	short lff_6	
lff_3:
	; stereo & 8 bit
	lodsb
	shl	ax, 8 ; convert 8 bit sample to 16 bit sample
	stosw
	loop	lff_3			
	jmp	short lff_6
lff_4:
	; 16 bit mono samples
	mov	cx, ax ; word count
lff_5:	
	lodsw
	stosw	; left channel
	stosw	; right channel
	loop	lff_5
lff_6:
	mov	ax, di ; save next buffer offset/position
	pop	si
	pop	di
	pop	es
	mov	cx, (BUFFERSIZE / 2)
lff_7:        
	and	ax, ax
	jz	short endLFF ; ; end of 2nd half

	cmp	ax, cx
	je	short lff_8
	jb	short lff_11
	xor	cx, cx
	sub	cx, ax
	neg	cx
	jmp	short lff_11
lff_8:
	mov	dx, ax ; buffer offset
	mov	ax, [fbs_seg] ; buffer segment
	jmp	lff_0
lff_9:  
	xor	ax, ax
lff_10:
	mov	cx, (BUFFERSIZE / 2)  
lff_11:
	call    padfill				; blank pad the remainder
        ;clc					; don't exit with CY yet.
        or	byte [flags], ENDOFFILE		; end of file flag
endLFF:
        ;pop	ds
	;pop	es
	pop	bx
	pop     dx
        pop     cx
        pop     ax
        retn

; entry ds:ax points to last byte in file
; cx=target size
; note: must do byte size fill
; destroys bx, cx
;
padfill:
	; 17/02/2017
	push	es
        push    di
	mov	di, [fbs_seg]
	mov	es, di
        sub     cx, ax
	mov	di, ax       	
	add	di, [fbs_off]
        xor     al, al
	rep	stosb
        ;mov	[fbs_off], di
	pop     di
        pop	es
	retn

; returns AL = current index value
getCurrentIndex:
	;push	dx
	mov	dx, [NABMBAR]      		
	add	dx, PO_CIV_REG
	in	al, dx
	;pop	dx
	retn
	
;input AL = index # to stop on
setLastValidIndex:
	;push	dx
	mov	dx, [NABMBAR]
	add	dx, PO_LVI_REG
        out     dx, al
	;pop	dx
	retn

; checks if either shift key has been pressed.  Exits with CY if so.
; 
check4keyboardstop:
        push    es
        push    0
        pop     es		       ; examine BDA for keyboard flags
        test    byte [es:417h], (BIT0 | BIT1)
        pop     es
        stc
        jnz     short _cksr
        clc
_cksr:
        retn