; DOS based .WAV player using AC'97 and codec interface.
; ---------------------------------------------------------------
; VIA VT8233 Modification & NASM version: Erdogan Tan (29/11/2016)
; Last Update: 18/02/2017 (by Erdogan Tan)

; player internal variables and other equates.
FILESIZE        equ     64 * 1024       ; 64k file buffer size.
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
	mov	di, ax ; 18/02/2017
	mov	cx, (FILESIZE/2)
	rep	stosw
	pop	es	     
	
       ; load 64k into buffer 1

        mov     ax, [WAV_BUFFER1]
        call    loadFromFile

       ; and 64k into buffer 2

       ;mov     ax, [WAV_BUFFER2]
       ;call    loadFromFile

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

; VIA VT8235.PDF: (Page 110) (Erdogan Tan, 29/11/2016)
	;
	; 	Audio SGD Table Format
	;	-------------------------------
	;	63   62    61-56    55-32  31-0
	;	--   --   --------  -----  ----
	;	EOL FLAG -reserved- Base   Base
	;			    Count  Address
	;			    [23:0] [31:0]
	;	EOL: End Of Link. 
	;	     1 indicates this block is the last of the link.
	;	     If the channel “Interrupt on EOL” bit is set, then
	;	     an interrupt is generated at the end of the transfer.
	;
	;	FLAG: Block Flag. If set, transfer pauses at the end of this
	;	      block. If the channel “Interrupt on FLAG” bit is set,
	;	      then an interrupt is generated at the end of this block.

	FLAG	EQU BIT30
	EOL	EQU BIT31

	; 08/12/2016 - Erdogan Tan
	mov	eax, FILESIZE
	or	eax, FLAG
	;or	eax, EOL
	stosd

; 2nd buffer:

        movzx   eax, word [WAV_BUFFER2]
        shl     eax, 4                          ; convert seg:off ->0:offset
        stosd                                   ; store pointer to wavbuffer2

; set length to 64k (32k of two 16 bit samples)
; Set control (bits 31:16) to BUP, bits 15:0=number of samples
; 
	; 08/12/2016 - Erdogan Tan
	mov	eax, FILESIZE
	or	eax, EOL
	;or	eax, FLAG
	stosd

        loop    _0
        pop     es

;
; tell the DMA engine where to find our list of Buffer Descriptors.
; this 32bit value is a flat mode memory offset (ie no segment:offset)
;
; write buffer descriptor list address
;
        movzx   eax, word [BDL_BUFFER]
	shl     eax, 4                          ; convert seg:off to 0:off
  
	; 12/11/2016 - Erdogan Tan 
	; (Ref: KolibriOS, vt823x.asm, 'create_primary_buff')
	mov     edx, VIADEV_PLAYBACK + VIA_REG_OFFSET_TABLE_PTR
        call    ctrl_io_w32

	call	codec_check_ready

  	mov     dx, VIADEV_PLAYBACK + VIA_REG_OFS_PLAYBACK_VOLUME_L
        mov     eax, 2   ;31
        call    ctrl_io_w8

	call	codec_check_ready

        mov     dx, VIADEV_PLAYBACK + VIA_REG_OFS_PLAYBACK_VOLUME_R
        mov     ax, 2   ;31
        call    ctrl_io_w8

	call	codec_check_ready
;
;
; All set.  Let's play some music.
;
;
       	;mov    dx, VIADEV_PLAYBACK + VIA_REG_OFFSET_STOP_IDX
        ;mov    ax, VIA8233_REG_TYPE_16BIT or VIA8233_REG_TYPE_STEREO or 0xfffff or 0xff000000
        ;call   ctrl_io_w32

	;call	codec_check_ready

	; 08/12/2016
	; 07/10/2016
        ;mov    al, 1
        mov	al, 31
	call    setLastValidIndex

	mov	byte [tLoop], 1 ; 30/11/2016

        mov	ax, VIA_REG_CTRL_INT
       	or	eax, VIA_REG_CTRL_START
        ;mov	ax, VIA_REG_CTRL_AUTOSTART + VIA_REG_CTRL_START
	; 28/11/2016
	;mov	ax, VIA_REG_CTRL_AUTOSTART + VIA_REG_CTRL_START + VIA_REG_CTRL_INT_FLAG
	mov     dx, VIADEV_PLAYBACK + VIA_REG_OFFSET_CONTROL
        call    ctrl_io_w8

	call	codec_check_ready

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
	xor	ax, ax
	xchg	al, [tBuff] ; AL = [tBuff], [tBuff] = 0
	cmp	al, 1
	jb	short p_loop
	je	short q_loop
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

	; 07/12/2016
	push	es 
	sub	si, si
	mov	ax, 0B800h ; video display page segment
	mov	es, ax
	mov	ah, 4Eh
	mov	al, '1'
	
	mov	byte [tBuff], 1 ; Buffer 1

	test	byte [irq_status], VIA_REG_STAT_EOL 
	jz	short _tlp1 ; FLAG
	
	; EOL
	inc	byte [tBuff] ; Buffer 2
	inc	al
_tlp1: 
	mov	[es:si], ax ; show playing buffer (1, 2)
	pop	es

	test	byte [irq_status], VIA_REG_STAT_FLAG 
	jz	short _tlp2

	mov	ax, VIA_REG_CTRL_INT
       	or	ax, VIA_REG_CTRL_START
       	mov     dx, VIADEV_PLAYBACK + VIA_REG_OFFSET_CONTROL
        call    ctrl_io_w8

	call	codec_check_ready
_tlp2:	
        mov     al, [irq_status]   ;; ack ;;
        mov     dx, VIADEV_PLAYBACK + VIA_REG_OFFSET_STATUS
        call    ctrl_io_w8

	retn

_exit_:
	mov	byte [tLoop], 0
_exit:
        ; finished with song, stop everything
	mov     ax, VIA_REG_CTRL_INT
        or      ax, VIA_REG_CTRL_TERMINATE
	mov     dx, VIADEV_PLAYBACK + VIA_REG_OFFSET_CONTROL
        call    ctrl_io_w8

        call	channel_reset
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
        push    ax
        push    cx
        push    dx
	;push	es
	push	ds
	
	;push	ds				; copy es->ds since we
	;pop	es				; mess with DS

        ;test   byte [es:flags], ENDOFFILE	; have we already read the
        test    byte [flags], ENDOFFILE
        stc					; last of the file?
        jnz     short endLFF
        
	mov     ds, ax
        xor     dx, dx                          ; load file into memory
        mov     cx, (FILESIZE / 2)              ; 32k chunk
	mov	ah, 3fh
	mov	bx, [cs:filehandle]
	int	21h
        
	clc
        cmp	ax, cx
	;je	short _lff1
	jne	short _lff1

;       or      byte [es:flags], ENDOFFILE      ; flag end of file
;       call    padfill                         ; blank pad the remainder
;       clc                                     ; don't exit with CY yet.
;       jmp	short endLFF
;_lff1:
        add     dx, ax

        mov     cx, (FILESIZE / 2)              ; 32k chunk
	mov	ah, 3fh
	mov	bx, [cs:filehandle]
	int	21h
        clc
        cmp     ax, cx
        je      short endLFF
_lff1:
        ;or     byte [es:flags], ENDOFFILE      ; flag end of file
        or	byte [cs:flags], ENDOFFILE
	call    padfill                         ; blank pad the remainder
        clc                                     ; don't exit with CY yet.
endLFF:
        pop	ds
	;pop	es
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
        push    bx
        sub     cx, ax
        mov     bx, ax
        xor     al, al
padfloop:
        mov     [bx], al
        inc     bx
        loop    padfloop
        pop     bx
        retn

;; returns AL = current index value
;getCurrentIndex:
;	; 14/11/2016 - Erdogan Tan (Ref: VIA VT8235.PDF, Page 110)
;	; 12/11/2016 - Erdogan Tan (Ref: KolibriOS, vt823x.asm)
;	;push	edx
;	mov	edx, VIADEV_PLAYBACK + VIA_REG_OFFSET_CURR_INDEX
;       call    ctrl_io_r8
;	; AL = Current index
;	;pop	edx
;	retn
	
;input AL = index # to stop on
setLastValidIndex:
	; 19/11/2016
	; 14/11/2016 - Erdogan Tan (Ref: VIA VT8235.PDF, Page 110)
	; 12/11/2016 - Erdogan Tan
	; (Ref: KolibriOS, vt823x.asm, 'create_primary_buff')
	;push	edx
	push	ax
	;push	ecx
	movzx	eax, word [sample_rate] ; Hertz
	mov	edx, 100000h ; 2^20 = 1048576
	mul	edx
	mov	ecx, 48000	
	div	ecx
	;and	eax, 0FFFFFh
	;pop	ecx
	pop	dx 
	shl	edx, 24  ; STOP Index Setting: Bit 24 to 31
	or	eax, edx
	; 19/11/2016
	cmp	byte [bps], 16
	jne	short sLVI_1
	or	eax, VIA8233_REG_TYPE_16BIT
sLVI_1:
	cmp	byte [stmo], 2
	jne	short sLVI_2
	or	eax, VIA8233_REG_TYPE_STEREO
sLVI_2:
	mov     edx, VIADEV_PLAYBACK + VIA_REG_OFFSET_STOP_IDX
        call    ctrl_io_w32
	call	codec_check_ready
	;pop	edx
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