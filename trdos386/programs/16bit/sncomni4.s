; ****************************************************************************
; sncomni.s (for MSDOS)
; ----------------------------------------------------------------------------
; SNCOMNI.COM ! VGA DEMO-TEST program !  NASM version by Erdogan TAN
;
; 04/02/2017
;
; [ Last Modification: 12/08/2017 ]
;
; Derived from source code of 'OMNISCENT.ASM' by Dirk Küppers
;          SNC_OMNI.COM	 (MSDOS) intro file, 1997
;
; Assembler: NASM 2.11
; ****************************************************************************

;		   ----====> Omniscent <====----
;
;   Omniscent was done by Pinker of SANCTION for the Mekka '97. The song
;   was written by Nyphton. It place XXX out of XXX.
;
;   Special thanks in alphabetical order :
;
;       Andreas Mautsch     (beta testing)
;       Axel Scheel Meyer   (MACM sources, nice IRC chat's)
;       Christian Cohnen    (for his help on perspective texture mapping and
;	   		designing world and script)
;       Daniel Weinand	    (song)
;       Funk                (for give me the idea trying a 4K Descent)
;       Stephanie Schepers  (moral and food support ;-) )
;
;       and all other SANCTION dudes for supporting this product !

SECTION .text

[BITS 16] ; x86 Real Mode (16 bit) intructions

[ORG 100h] 

;==============================================================================
;		   constants
;==============================================================================

GMPort		equ 0331h
CMD_NOTEON      equ 090h
CMD_NOTEOFF     equ 080h
CMD_CHANGEPARAM equ 0B0h
CMD_CHANGEPRG   equ 0C0h
MOD_ALLNOTESOFF equ 07Bh
MAXFACES	equ 400
MAXPOINTS	equ 400
SONGSPEED   	equ 29
XMAX		equ 320
YMIN		equ 21
YMAX		equ 179
SUBRANGE	equ 16
%define ASPECT_RATIO 1.2
CENTERX		equ 160
CENTERY		equ 100

;==============================================================================
;		   structures
;==============================================================================

struc channel
 .del:	resw 1
 .trk:	resb 1
 .ln:	resb 1
 .adr:	resw 1
endstruc

struc matrix
	resd 9
 .size:
endstruc

struc vector
 .x:	resd 1
 .y:	resd 1
 .z:	resd 1
 .size:
endstruc

struc point
 .x:	resw 1
 .y:	resw 1
 .z:	resw 1
 .s:	resw 1
 .size:
endstruc

struc face
	resw 5
 .size: 	
endstruc

;struc object
; .panz: resw 1
; .fanz: resw 1
; .p:	resb point.size*MAXPOINTS
; .f:	resb face.size*MAXFACES
; .size:
;endstruc

struc edges
 .x:	resd 1
 .u:	resd 1
 .v:	resd 1
 .w:	resd 1
 .s:	resd 1
 .size:	
endstruc

struc edge
 .p:	resb vector.size
 .py:	resd 1
 .e:	resb edges.size
 .size:
endstruc

struc poly
	resb edge.size*5
 .size:	
endstruc

;==============================================================================
;		      %macros
;==============================================================================
;***********************************************
;* descript. : start timer interrupt 70/s      *
;* parameter : none			       *
;* sideeffect: all		 	       *
;* back      : none			       *
;***********************************************
%macro		startTimer 0
                mov     ax, 03508h
                int     21h
                mov     [Old08Irqseg], es
                mov     [Old08Irqofs], bx
                mov     ax, 02508h
                mov     dx, irqHandler08
                int     21h
		
		mov	bx, 3409 ; 1193180/3409 = 350 ticks per second (*!)
		call	setTimer
%endmacro

;***********************************************
;* descript. : stop timer interrupt            *
;* parameter : none			       *
;* sideeffect: all		 	       *
;* back      : none			       *
;***********************************************
%macro		stopTimer 0
                push    ds
                xor     bx, bx			; set timer to default
                call    setTimer		; 18.2 ticks per second
                mov     ax, 02508h      	; restore old IRQ-vector
                lds     dx, [Old08IrqPtr]
                int     21h
                pop     ds
%endmacro

;***********************************************
;* descript. : start keyboard handler          *
;* parameter : none			       *
;* sideeffect: all		 	       *
;* back      : none			       *
;***********************************************
%macro		startKBDHandler 0
                ;mov     ax, 03509h
                ;int     21h
                ;mov     [Old09Irqseg], es
                ;mov     [Old09Irqofs], bx
                ;mov     ax, 02509h
                ;mov     dx, irqHandler09
                ;int     21h

		nop
%endmacro

;***********************************************
;* descript. : stop keyboard handler           *
;* parameter : none			       *
;* sideeffect: all		               *
;* back      : none		               *
;***********************************************
%macro		stopKBDHandler 0
                ;push    ds
                ;mov     ax,02509h		; restore old IRQ-vector
                ;lds     dx, [Old09IrqPtr]	;
                ;int     21h
                ;pop     ds

		nop
%endmacro

;***********************************************
;* descript. : zero null-initialized data      *
;* parameter : none			       *
;* sideeffect: all		               *
;* back      : none		               *
;***********************************************
%macro		nullData 0
		mov	di, nullstart
		mov	cx, nullend-nullstart
		xor	al, al
                rep	stosb
%endmacro

;***********************************************
;* descript. : set textmode		       *
;* parameter : none		               *
;* sideeffect: all		               *
;* back      : none		               *
;***********************************************
%macro		set80x25 0
		mov	ax, 3
		int	10h	; - VIDEO - SET	VIDEO MODE
				; AL = mode
%endmacro

;***********************************************
;* descript. : set gfx-mode 320x200            *
;* parameter : none		               *
;* sideeffect: all		               *
;* back      : none		               *
;***********************************************
%macro		set320x200 0
		mov	ax, 13h
		int	10h		; - VIDEO - SET	VIDEO MODE
					; AL = mode
%endmacro

;***********************************************
;* descript. : calculate a smooth colorrange   *
;* parameter : none             	       *
;* sideeffect: all              	       *
;* back	     : none                	       *
;***********************************************
%macro		makePalette 0
		push	ds
		pop	es
		mov	bx, 3
mPcolor:
		mov	si, colors
		mov	di, palette
		xor	dh, dh
		lodsb
		movzx	cx, al
mPouter:
		push	cx
		mov	cl, [si]
		mov	ah, [bx+si]
		sub	ax, dx
		push	dx
		cwd
		idiv	cx
		pop	dx
mPinner:
		add	dx, ax
		;mov	[es:di+bx-1], dh
		mov	[di+bx-1], dh
		add	di, 3
		loop	mPinner
		add	si, 4
		pop	cx
		loop	mPouter
		dec	bx
		jnz	short mPcolor
%endmacro

;***********************************************
;* descript. : reset the GM-Port and switch to *
;*             UART mode.		       *
;* parameter : none			       *
;* sideeffect: dx,al		               *
;* back      : none		               *
;***********************************************
%macro		resetGM 0
		mov	dx, GMPort
		mov	ax, 0FFh
		;mov	al, 0FFh
		out	dx, al
		;sub	cl, cl
resGMbusy1:
		;dec	cl
		dec	ah
		jz	short rGMerror	;{ timeout }
		in	al, dx		;{ read acknowledge }
		
		test	al, 80h
		jnz	short resGMbusy1
		dec	dx
		in	al, dx

		cmp	al, 0FEh
		jne	short rGMerror
		inc	dx		;{ switch into UART mode }
resGMbusy2:
		in	al, dx

		test	al, 40h
		jnz	short resGMbusy2
		mov	al, 3Fh
		out	dx, al
rGMerror:
%endmacro

;***********************************************
;* descript. : mute midi channels 0..15        *
;* parameter : none                	       *
;* sideeffect: ax,cx		               *
;* back		 : none                	       *
;***********************************************
%macro		silence 0
		mov	cx, 15 ; 0Fh
Siloop:	
		mov	ax, cx
		add	al, CMD_CHANGEPARAM ; 0B0h
		call	writeGM
		mov	al, MOD_ALLNOTESOFF ; 7Bh
		call	writeGM
		xor	al, al
		call	writeGM
		loop	Siloop
Sinosound:
%endmacro

;***********************************************
;* descript. : create the reactor wall texture *
;* parameter : none		               *
;* sideeffect: ax,bx,cx,di,es                  *
;* back      : none			       *
;* length    : 45 bytes		               *
;***********************************************
%macro		reactorWall 0
		mov	es, [tseg]
		xor	di, di
		mov	cx, 4096 ; 1000h
rsmloop:
		mov	ax, di
		test	al, 8
		jle	short rsnonot1
		not	al
rsnonot1:
		and	al, 15 ; 0Fh
		mov	bx, ax
		mov	ax, di
		shr	ax, 6
		test	al, 8
		jle	short rsnonot2
		not	al
rsnonot2:
		and	al, 15 ; 0Fh
		cmp	al, bl
		jle	short rstakeal
		mov	ax, bx
rstakeal:
		add	al, 224 ; 0E0h
		stosb
		loop	rsmloop
%endmacro

;***********************************************
;* descript. : calc background-fractal         *
;* parameter : none		               *
;* sideeffect: ax,bx,cx,si,di,es	       *
;* back      : none			       *
;***********************************************
%macro		createFrac2 0
		mov	es, [tseg+36]
		mov	al, 20	; 14h
		call	setFrac
		mov	di, 64	; 40h
		mov	cx, 4096-64
		mov	bx, 4
cF1loop:
		;mov	bx, 4
		call	rnd
		add	al, [es:di-64]
		add	al, [es:di-63]
		dec	ax
		shr	ax, 1
		stosb
		loop	cF1loop
%endmacro

;***********************************************
;* descript. : calc random noise fractal       *
;* parameter : none		               *
;* sideeffect: ax,bx,cx,si,di,es	       *
;* back      : none			       *
;***********************************************
%macro		createFrac3 0
		mov	es, [tseg+38]
		mov	al, 3
		call	setFrac
		mov	cx, 4096 ; 1000h
		;mov	bx, cx
cF3loop:
		mov	bx, 4096 ; 1000h
		call	rnd
		mov	di, ax 
		inc	byte [es:di]
		loop	cF3loop
%endmacro

;***********************************************
;* descript. : init stars (positions and state)*
;* parameter : none			       *
;* sideeffect: eax,bx,cx,edx,si,di,es          *
;* back      : none		               *
;***********************************************
%macro		initStars 0
		push	ds
		pop	es
		mov	cx, 30 ; 1Eh
		mov	di, stars
iSloop:	
		mov	bx, 256 ; 100h
		call	rnd
		stosb
		mov	bx, 4096-(64*5) ; 0EC0h
		add	bx, 128 ; 80h
		call	rnd
		add	ax, 64  ; 40h
		stosw
		loop	iSloop
%endmacro

;***********************************************
;* descript. : add effects to fractals	       *
;* parameter : none		               *
;* sideeffect: ax,bx,cx,di,es		       *
;* back      : none		               *
;***********************************************
%macro		effects 0
		mov	cx, 26	; 1Ah
		mov	si, aE
effmloop:
		push	cx
		mov	bl, [si]	; mseg+effect
		mov	dl, bl
		shr	dl, 4
		and	bl, 0Fh ; 15
		;add	bx, bx
		shl	bl, 1
		mov	es, [bx+tseg]
		movzx	cx, byte [si+2] ; y1
effyloop:
		movzx	bx, byte [si+1] ; x1
effxloop:
		mov	di, cx
		shl	di, 6
		add	di, bx
		mov	al, dl
		dec	al
		jz	short effect1
		dec	al
		jz	short effect2
		mov	ax, bx
		add	ax, cx
		and	al, 4
		;jz	short effdonot
		jz	short effect1
		mov	al, 152 ; 98h
effdonot:
		jmp	short effect1
effect2:
		mov	al, [es:di]
effect1:
		add	al, [si+5]	; value
		mov	[es:di], al
		inc	bx
		cmp	bl, [si+3]	; x2
		jle	short effxloop
		inc	cx
		cmp	cl, [si+4]	; y2
		jle	short effyloop
		add	si, 6
		pop	cx
		loop	effmloop
%endmacro

;***********************************************
;* descript. : copy fractals and add value     *
;* parameter : none		               *
;* sideeffect: al,cx,di,es,fs		       *
;* back      : none		               *
;***********************************************
%macro		addFractals 0
		mov	si, aF
		mov	dl, 4
aFloop:
		movzx	bx, dl
		mov	es, [bx+tseg]
		lodsw
		mov	bl, al
		mov	fs, [bx+tseg]		
		xor	di, di
		mov	cx, 4096 ; 1000h
aFiloop:
		mov	al, [fs:di]
		add	al, ah
		stosb
		loop	aFiloop
		add	dl, 2
		;inc	dx
		;inc	dx
		cmp	dl, 30
		jle	short aFloop
%endmacro

;***********************************************
;* descript. : calculate and initalize textures*
;* parameter : none			       *
;* sideeffect: al,cx,di,es,fs		       *
;* back      : none			       *
;***********************************************
%macro		addLava 0
		mov	es, [tseg+22]
		mov	di, 4095	; 0FFFh
		mov	cx, 20		; 14h
		mov	al, 94		; 5Eh
aLyloop:
		push	cx
		shl	cx, 3
aLxloop:
		push	di
		push	ax
		mov	bx, 64		; 40h
		call	rnd
		sub	di, ax
		pop	ax
		;stosb
		mov	[es:di], al
		pop	di
		loop	aLxloop
		pop	cx
		dec	al
		sub	di, 64 		; 40h
		loop	aLyloop
%endmacro

;***********************************************
;* descript. : calculate and initalize textures*
;* parameter : none		               *
;* sideeffect: al,cx,di,es,fs		       *
;* back      : none		               *
;***********************************************
%macro		initTextures 0
		mov	bx, 34	; 22h
		push	800	; 320h
		push	5
		call	createFrac1
		mov	bx, 32	; 20h
		push	112	; 70h
		push	15	; 0Fh
		call	createFrac1
		createFrac2
		createFrac3
		initStars
		reactorWall
		addFractals
		effects
		addLava
%endmacro

;***********************************************
;* descript. : cycle "range" colors at "base"  *
;* parameter : none		               *
;* sideeffect: ax,(bl),cx,si,di,es             *
;* back      : none			       *
;***********************************************
%macro		colorCycle 0
		base	equ 6*32
                range	equ 32
		
		push	ds
		pop	es
		mov	di, palette+(base*3)
		mov	si, di
		lodsw
		;mov	bl, [si]  ; works only with fire (no blue)
		inc	si
		mov	cx, (range*3-3)
		rep movsb
		stosw
		;mov	[di], bl  ; works only with fire (no blue)
%endmacro

;***********************************************
;* descript. : animate the stars	       *
;* parameter : none			       *
;* sideeffect: ax,bx,cx,dl,si,di,es            *
;* back      : none			       *
;***********************************************
%macro		animStars 0
		_base	equ 7*32

		mov	es, [tseg+2]
		call	clearFrac

		mov	cl, 30	; 1Eh
		mov	bx, stars
aSoloop:
		dec	byte [bx]
		mov	ah, [bx]
		and	ah, 63	; 3Fh
		cmp	ah, 31	; 1Fh
		jbe	short aSnonot
		not	ah
		and	ah, 31	; 1Fh
aSnonot:
		shr	ah, 1
		mov	di, [bx+1]
		push	cx
		push	bx

		mov	dx, 64-5 ; 3Bh
		mov	cx, _base*256+0 ; 0E000h
		call	setstarbob

		pop	bx
		pop	cx
		add	bx, 3
		loop	aSoloop
%endmacro

;***********************************************
;* descript. : calculate shading tab           *
;* parameter : none		               *
;* sideeffect: ax,bl,cx,di,es                  *
;* back      : none		               *
;***********************************************
%macro		calcShadeTab 0
		push	ds
		pop	es
		mov	di, shadetab
		xor	bl, bl
cSolp:
		inc	bl
		xor	cx, cx
cSilp1:
		mov	al, cl
		and	al, 31	; 1Fh
		mul	bl
		add	ax, ax
		mov	al, cl
		and	al, 224	; 0E0h
		add	al, ah
		stosb
		inc	cl
		cmp	cl, 192 ; 0C0h
		jne	short cSilp1
cSilp2:
		mov	al, cl
		stosb
		inc	cl
		jnz	short cSilp2
		cmp	bl, 128	; 80h
		jne	short cSolp
%endmacro

;***********************************************
;* descript. : expand a song       	       *
;* parameter : si:song base adress	       *
;* sideeffect: ax,bx,cx,dx,si,di               *
;* back      : none                	       *
;***********************************************
%macro		expandSong 0
                mov	si, credits
                mov	bx, channels
                mov	di, songdata
EPSwhile:	lodsw
                dec	al
                js      short EPSendwhile
                mov	[bx+channel.trk], al ; [bx+2]
                add	al, CMD_CHANGEPRG ; 0C0h
                call	setinstr
                mov	[bx+channel.adr], di ; [bx+4]	
               	call	expand
                xor	al, al
                stosb
                add	bx, 6
                inc	word [tracks]
                jmp	short EPSwhile
EPSendwhile:
%endmacro

;***********************************************
;* descript. : parse the script,rotate and move*
;* parameter : none                	       *
;* sideeffect: all		               *
;* back      : none		               *
;***********************************************
%macro		scriptIt 0
		mov	cx, [ticker]
		mov	word [ticker], 0
		or	cx, cx
		jz	sItickerNull
sImloop:
		push	cx
		mov	si, zspeed
		;dec	word [si+10]	      ;	scriptanz
		dec	word [scriptanz]
		jns	short sIwaitTimer
		;mov	bx, [si+8]	      ;	scriptptr
		mov	bx, [scriptptr]
		mov	al, [bx+script]
		or	al, al
		jz	_exit_
		mov	bx, ax
		and	bl, 7
		;mov	[si+12], bl           ;	scriptins
		mov	[scriptins], bl
		dec	bl
		jnz	short sInegateTurn
		; (cmd = NEG ZSTEP)  
		neg	word [zstep]
sInegateTurn:
		and	ax, 0F8h
		add	ax, ax
		;mov	[si+10], ax           ; scriptanz
		mov	word [scriptanz], ax
		;inc	word [si+8]	      ; scriptptr
		inc	word [scriptptr]
sIwaitTimer:
		;mov	al, [si+12]           ;	scriptins
		mov	al, [scriptins]
		cbw
		xor	ch, ch
		dec	ax
		jnz	short sInoturn
		; (cmd = NEG ZSTEP)  
		mov	bx, [zstep]
		;add	[si+6], bx	      ; ozw
		add	[ozw], bx
		jmp	short sI_1	
sInoturn:
		mov	cl, 3
		;mov	cx, 3
sIrotateLoop:
		dec	ax
		jnz	short sInoIncrement
		; (cmd = INC SPEED or INC XSTEP or INC YSTEP)  
		inc	word [si]	      ; zspeed, oxw, oyw
		jmp	short sI_1	   	
sInoIncrement:
		dec	ax
		jnz	short sInoDecrement
		; (cmd = DEC SPEED or DEC XSTEP or DEC YSTEP)  
		dec	word [si]	      ; zspeed, oxw, oyw
		jmp	short sI_1
sInoDecrement:
		inc	si
		inc	si
		loop	sIrotateLoop
sI_1:
		;mov	si, zspeed+2	      ; oxw 	
		mov	si, oxw	
		mov	cl, 3
sIpushloop:
		lodsw			      ; oxw, oyw, ozw	
		sar	ax, 2
		push	ax
		loop	sIpushloop

		imul	ax, word [si-4], 16   ; [oyw]*16 	
		push	ax		        	
		call	calcRotMat

		mov	cl, 3
		xor	si, si
sImoveLoop:
		fld	dword [si+owmat+24]
		fimul	word [zspeed]
		fidiv	word [CONST1792]
		fadd	dword [si+ob]
		fstp	dword [si+ob]
		add	si, 4
		loop	sImoveLoop
		pop	cx
		dec	cx
		jnz	sImloop
sItickerNull:
%endmacro

;***********************************************
;* descript. : rotate all points from o -> rp  *
;* parameter : none                	       *
;* sideeffect: all		               *
;* back      : none		               *
;***********************************************
%macro		rotation 0
		mov	cx, [object.panz]
		mov	si, object.p
		mov	di, rp
rotmlp:
		mov	bx, wmat
		mov	dx, 3
rotilp:	
		fild	word [si+0] ; point.x
		fsub	dword [ob+vector.x]
		fmul	dword [bx+0]
		fild	word [si+2] ; point.y
		fsub	dword [ob+vector.y]
		fmul	dword [bx+4]
		fild	word [si+4] ; point.z
		fsub	dword [ob+vector.z]
		fmul	dword [bx+8]
		;fadd	
		;fadd
		faddp
		faddp
		fstp	dword [di]
		add	di, 4
		add	bx, 12	; 0Ch
		dec	dx
		jnz	short rotilp
		add	si, 8 ; next point
		loop	rotmlp
%endmacro

;***********************************************
;* descript. : sort faces by z-koord	       *
;* parameter : none		               *
;* sideeffect: ax,bx,cx,dx,si,di,es            *
;* back      : none		               *
;***********************************************
%macro		sortFaces 0
		push	ds
		pop	es
		mov	di, facei
		push	di ; facei
		mov	si, object.f
		mov	ax, si ; pointer to faces
		mov	cx, [object.fanz]
sFmloop:
		fldz
		push	cx
		mov	cx, 4
sFiloop:
		imul	bx, word [si], 12 ; number of points (of face)
		inc	si
		inc	si
		fsub	dword [bx+rp+vector.z]
		loop	sFiloop
		pop	cx
		fistp	word [di] ; z distance (face total)
		inc	di
		inc	di
		stosw		; face address (in 'object.f')
		add	ax, 10
		inc	si
		inc	si
		loop	sFmloop
		push	di	; facei+4*(object.fanz-1)
		call	qsort
%endmacro

;***********************************************
;* descript. : animate the door  	       *
;* parameter : none		               *
;* sideeffect: ax,cx,si,di,es                  *
;* back      : none		               *
;***********************************************
%macro		animDoor 0
		mov	ax, [doortimer]
		or	ax, ax
		jns	short aDnounder
		xor	ax, ax
		jmp	short alreadydrawed
aDnounder:
		cmp	byte [once], 0
		jne	short alreadydrawed
		inc	byte [once]
		push	ax
		starbackground
		pop	ax
alreadydrawed:
		cmp	ax, 24
		jle	short aDnoover
		mov	ax, 24
aDnoover:
		shl	ax, 6
		push	ds
		mov	es, [tseg+26]
		mov	ds, [tseg+30]
		mov	si, ax
		xor	di, di
		mov	cx, 1024
		rep movsw
		mov	si, 2048
                sub	si, ax
		push	si
		mov	ch, 4  ; cx = 1024
		rep movsw
		pop	di
		mov	cx, ax
		xor	ax, ax
		rep stosw
		pop	ds
%endmacro

;***********************************************
;* descript. : play a tick of the song	       *
;* parameter : none                	       *
;* sideeffect: ax,bx,cx,dx,si		       *
;* back      : none                	       *
;***********************************************
%macro		playsong 0
		mov	cx, [tracks]
		mov	si, channels
PSmloop:
		dec	word [si+channel.del]
		jg	short PSdelayed
		mov	ax, [si+channel.trk]
		add	al, CMD_NOTEOFF	; 80h
		call	setnote
		mov	bx, [si+channel.adr]
		mov	ah, 127 	; 7Fh
		cmp	byte [bx], 0
		jz	short PStrackend
		mov	ah, [bx]
		mov	[si+channel.ln], ah
		mov	al, [si+channel.trk]
		add	al, CMD_NOTEON	; 90h
		call	setnote
		mov	al, [bx+1]
		mov	bl, SONGSPEED	; 1Dh
		mul	bl
PStrackend:
		mov	[si+channel.del], ax
		add	word [si+channel.adr], 2
PSdelayed:
		add	si, 6
		loop	PSmloop
PSnosound:
%endmacro

;***********************************************
;* descript. : dump palette to CRT             *
;* parameter : bl:intensity (0..255)           *
;* sideeffect: ax,es,di,cx,dx                  *
;* back      : none		               *
;***********************************************
%macro		setPalette2 0
		xor	al, al
		mov	dx, 3C8h
		out	dx, al
		inc	dx
		mov	si, palette
		mov	cx, 768 ; 300h
		rep outsb
%endmacro

;***********************************************
;* descript. : clear starbackground            *
;* parameter : none		               *
;* sideeffect: ax,es,di,cx,dx                  *
;* back      : none		               *
;***********************************************
%macro		cls 0
		push	ds
		mov	es, [bseg]
		;push	0A000h
		;pop	ds
		mov	si, 0A000h
		mov	ds, si
		mov	si, 320*5
		xor	di, di
		mov	cx, 32000 ; 7D00h
clsloop:
		lodsb
		stosb
		stosb
		loop	clsloop
		pop	ds
%endmacro

;***********************************************
;* descript. : draw starbackground             *
;* parameter : none		               *
;* sideeffect: ax,es,di,cx,dx                  *
;* back      : none		               *
;***********************************************
%macro		starbackground 0
		mov	es, [bseg]
		mov	cx, 100 ; 64h
starbackloop:
		push	cx
		mov	bx, 320*155 ; 0C1C0h
		call	rnd
		add	ax, 320*20  ; 1900h
		mov	di, ax
		and	ah, 7
		mov	dx, 320-5   ; 13Bh
		mov	cx, 2
		call	setstarbob
		pop	cx
		loop	starbackloop
%endmacro

;***********************************************
;* descript. : create identity matrix	       *
;* parameter : ds:di address                   *
;* sideeffect: cx,di		   	       *
;* back      : none			       *
;***********************************************
%macro		identityMat 0
		mov	ch, 2
iMolp:
		mov	cl, 2
iMilp:
		fldz
		cmp	ch, cl
		jne	short iMwritezero
		fstp	st0
		fld1
iMwritezero:
		fstp	dword [di]
		add	di, 4
		dec	cl
		jns	short iMilp
		dec	ch
		jns	short iMolp
%endmacro

;=============================================================================
;               entry point
;=============================================================================
;***********************************************
;* descript. : entry point		       *
;* parameter : none		               *
;* sideeffect: all		               *
;* back      : none		               *
;***********************************************

start:
		;mov	ax, cs
		;mov	ds, ax
		;mov	es, ax

		;; clear bss
		;mov	di, bss_start
		;mov	cx, (bss_end - bss_start)/4
		;xor	eax, eax
		;rep	stosd

		mov	ax, cs
		mov	di, vseg
		add	ax, 1000h
		stosw			; virtual screen address
		add	ax, 1000h
		stosw			; star background address
		add	ax, 1000h
		mov	cx, 20
segloop:				; texture addresses
		stosw
		;add	ax, 100h
		inc	ah
		loop	segloop

		;fninit

		nullData
		resetGM
		expandSong

		call	createWorld

		set320x200

		xchg	ah, al 		; ah = write string = 13h, 
					; al = write mode = 0
		mov	bx, 1Ch
		mov	bp, omniscent
		mov	cx, omniend-omniscent ; 9
		mov	dx, 106h
		int	10h		; - VIDEO - WRITE STRING (AT,XT286,PS,EGA,VGA)
					; AL = mode, BL	= attribute if AL bit 1	clear, BH = display page number
					; DH,DL	= row,column of	starting cursor	position, CX = length of string
					; ES:BP	-> start of string
		add	bp, cx
		mov	cl, sancend-sanction ; 11h
		mov	dx, 0C01h
		int	10h		; - VIDEO - WRITE STRING (AT,XT286,PS,EGA,VGA)
					; AL = mode, BL	= attribute if AL bit 1	clear, BH = display page number
					; DH,DL	= row,column of	starting cursor	position, CX = length of string
					; ES:BP	-> start of string
		makePalette
		
		initTextures

		calcShadeTab

		mov	[oldstack], sp

		startTimer
		startKBDHandler
		cls
mainloop:
		mov	ah, 1	; Check keyboard buffer
		int	16h	; Keyboard interrupt
		jnz	_exit_  ; exit

		setPalette2

		scriptIt

		animDoor

		push	ds
		mov	es, [vseg]
		mov	ds, [bseg]
		call	copyseg
		pop	ds

		rotation
		sortFaces

		mov	cx, [object.fanz]
		mov	si, facei
drawloop:
		lodsw
		lodsw
		mov	bx, ax ; face address in 'object.f'
		pusha
		call	drawclippedface
		popa
		loop	drawloop
	
		push	ds
		push	0A000h
		pop	es
		mov	ds, [vseg]
		call	copyseg
		pop	ds
		jmp	mainloop
loc_err_exit:
_exit_:
		push	cs
		pop	ds
		mov	sp, [oldstack]
		silence
		stopKBDHandler
		stopTimer
		set80x25
		ret

;==============================================================================
;		interrupt handler
;==============================================================================
;***********************************************
;* descript. : timer routine                   *
;* parameter : none		               *
;* sideeffect: none		               *
;* back      : none		               *
;***********************************************

irqHandler08:
		pusha
		
		push	ds
		push	es

		push	cs
		pop	ds

		playsong

		inc	word [ticker]
		inc	byte [twice]
		and	byte [twice], 7
		jnz	short iH08notyet

		colorCycle
		animStars
	
		inc	word [doortimer]
iH08notyet:
		mov	al, 20h
		out	20h, al
		
		pop	es
		pop	ds
		popa
		iret

;***********************************************
;* descript. : keyboard- / breakhandler	       *
;* parameter : none		               *
;* sideeffect: none		               *
;* back      : none		               *
;* length    : 12 bytes		               *
;***********************************************
;
;irqHandler09:
		;push    ax
		;mov     al, 20h
		;out     20h, al
		;in      al, 60h
		;dec     al
		;pop     ax
		;je	_exit_
		;iret
	
;==============================================================================
;		sub routines
;==============================================================================
;***********************************************
;* descript. : draw a the starbob	       *
;* parameter : ds,es: source seg,dest seg      *
;* sideeffect: ax,cx,si,di	               *
;* back      : none		               *
;***********************************************

setstarbob:
		mov	si, bob
		mov	bl, 5
yloop:
		mov	bh, 5
xloop:
		lodsb
		sub	al, ah
		jle	short aSnodraw
		add	al, ch
		shl	al, cl
		mov	[es:di], al
		;mov	[di], al
aSnodraw:
		inc	di
		dec	bh
		jnz	short xloop
		add	di, dx
		dec	bl
		jnz	short yloop
		retn

;***********************************************
;* descript. : copy 16000 longs ds --> es      *
;* parameter : ds,es: source seg,dest seg      *
;* sideeffect: ax,cx,si,di	               *
;* back      : none		               *
;***********************************************

copyseg:
		xor	di, di
		xor	si, si
		mov	cx, 16000 ; 3E80h
		rep movsd
		retn

;***********************************************
;* descript. : expand a part of the song       *
;* parameter : si:base adress		       *
;* sideeffect: si,ax		               *
;* back      : ax=si:new baseadress            *
;***********************************************

expand:
EPwhile: 	mov	cl, [si]
		or	cl, cl
		jz	short EPendwhile
		jns	short EPnote
EPcall:
		push	cx
		push	si
		inc	si
		call	expand
		pop	si
		pop	cx
		inc	cl
		jnz	short EPcall
		mov	si, ax
		jmp	short EPwhile 
EPnote:
		movsw
		jmp	short EPwhile
EPendwhile:
		inc	si
		mov	ax, si
		retn

;***********************************************
;* descript. : recursive index quicksort       *
;* parameter : l,r:stack left and right border *
;* sideeffect: ax,bx,cx,dx,si,di               *
;* back      : none		               *
;***********************************************

qsort:
		pop	ax 	; get address
		pop	cx	; get 2. param r
		pop	bx	; get 1. param l
		push	ax	; store address

		cmp	cx, bx
		jle	short QSendrek
		mov	si, bx
		mov	di, cx  	
		mov	dx, [si]
QSrepeat:
QSwhile1:	cmp	[si], dx
		jle	short QSwhile2
		add	si, 4
		jmp	short QSwhile1
QSwhile2:
		cmp	[di], dx
		jnl	short QSwhile2e
		sub	di, 4
		jmp	short QSwhile2
QSwhile2e:
		cmp	si, di
		;jg	short QSnoswap
		jg	short _QSnoswap
		lodsd
		xchg	eax, [di]
		mov	[si-4], eax
		sub	di, 4
QSnoswap:
		cmp	si, di
		jle	short QSrepeat
_QSnoswap:
		push	si 
		push	cx
		push	bx
		push	di
		call	qsort
		call	qsort
QSendrek:
		retn

;***********************************************
;* descript. : returns a pseudo random number  *
;* parameter : bx=range		               *
;* sideeffect: eax,edx,si		       *
;* back      : ax=rnd(range)                   *
;***********************************************

rnd:
		mov	si, randommul
		lodsd
		mul	dword [si] ; randomseed
		inc	eax
		mov	[si], eax  ; randomseed
		lodsw
		lodsw
		mul	bx
		xchg	ax, dx
		retn

;***********************************************
;* descript. : set timer speed to 1193180/AX   *
;*             interrupts per second           *
;* parameter : bx		               *
;* sideeffect: ax		               *
;* back      : none		               *
;***********************************************

setTimer:	; set timer speed to 1193180/BX (TRDOS 386)
		mov	al, 36h
		out	43h, al		; Timer	8253-5 (AT: 8254.2).
		mov	al, bl
		out	40h, al		; Timer	8253-5 (AT: 8254.2).
		mov	al, bh
		out	40h, al		; Timer	8253-5 (AT: 8254.2).
		retn

;***********************************************
;* descript. : send a byte to the GM-Port      *
;* parameter : al:midi command                 *
;* sideeffect: dx		               *
;* back      : none		               *
;***********************************************

writeGM:
		mov	dx, GMPort  ; 331h
		push	ax
		xor	ax, ax
busy:
		dec	ah
		jz	short timeOut
		in	al, dx
		test	al, 40h
		jnz	short busy
timeOut:
		pop	ax
		dec	dx
		out	dx, al
		retn

;***********************************************
;* descript. : send NOTEON command, volume 127 *
;* parameter : al:channel;ah:note              *
;* sideeffect: dx,al		               *
;* back      : none		               *
;***********************************************

setnote:
		call	setinstr
		mov	al, 127	; 7Fh
		call	writeGM
		retn

;***********************************************
;* descript. : send CHANGEPRG command	       *
;* parameter : al:channel;ah:instrument        *
;* sideeffect: dx,al		               *
;* back      : none		               *
;***********************************************

setinstr:
		call	writeGM
		mov	al, ah
		call	writeGM
		retn

;***********************************************
;* descript. : clear/set the fractal es: to al *
;* parameter : al,es		               *
;* sideeffect: ax,cx,di,es  	               *
;* back      : none			       *
;***********************************************

clearFrac:
		xor	ax, ax
setFrac:
		xor	di, di
		mov	cx, 4096 ; 1000h
		rep stosb
		retn

;***********************************************
;* descript. : calc shade bob fractal          *
;* parameter : es:seg;num,rad:stack            *
;* sideeffect: all			       *
;* back      : none			       *
;***********************************************

createFrac1:
		rad	equ 4 ; arg 1 in [sp+2]
		num	equ 6 ; arg 2 in [sp+4]

		push	bp
		mov	bp, sp

		mov	es, [bx+tseg]
		call	clearFrac

		mov	si, circletab
		mov	dx, [bp+rad]
		mov	al, dl
		imul	dl
		;mov	di, ax
		mov	bx, ax
		mov	cx, dx
		add	cx, cx
CCTloop:
		mov	al, dl
		imul	dl
		neg	ax
		;add	ax, di
		add	ax, bx
		mov	[si], ax
		fild	word [si]
		fsqrt
		fistp	word [si]
		inc	si
		inc	si
		dec	dx
		loop	CCTloop

		mov	cx, [bp+num]
SBloop1:
		push	cx
		mov	bx, 4096 ; 1000h
		call	rnd
		mov	di, ax
		mov	si, circletab
		mov	cx, [bp+rad]
		add	cx, cx
SBloop2:
		push	cx
		lodsw
		mov	cx, ax
		add	cx, cx
		jz	short SBskip
		push	di
		sub	di, ax
SBloop3:
		and	di, 4095  ; 0FFFh
		inc	byte [es:di]
		inc	di
		loop	SBloop3
		pop	di
SBskip:
		add	di, 64 ; 40h
		pop	cx
		loop	SBloop2
		pop	cx
		loop	SBloop1
		pop	bp
		ret	4

;***********************************************
;* descript. : draw a perpective-texturemapped *
;*	       and g.-shaded n-sided polygon   *
;* parameter : far ptr to poly, sides, textnum *
;* sideeffect: all			       *
;* back      : none			       *
;* length    : 700 bytes		       *
;***********************************************

scansubtextpoly:
		; 11/08/2017
		col	equ 4  ; arg 1, [sp+2] ; dcf.col value
		n	equ 6  ; arg 2, [sp+4] ; dcf.j value
		pol	equ 8  ; arg 3, [sp+6] ; dcf.p2 address

 		push	bp
		mov	bp, sp

		mov	ax, 32767
		mov	dx, ax
		mov	bx, ax
		neg	bx	; -32767 ; 8001h
		mov	si, bx	; -32767

		; di = dcf.p2 address
		mov	di, [bp+pol] ; [bp+8] ; dcf.p2 ; face structure

		xor	cx, cx
minmax:
		cmp	ax, [di+2+edge.e+edges.x] ; [di+12h]
		jle	short noswap1
		mov	ax, [di+2+edge.e+edges.x] ; [di+12h]
noswap1:
		cmp	bx, [di+2+edge.e+edges.x] ; [di+12h]
		jge	short noswap2
		mov	bx, [di+2+edge.e+edges.x] ; [di+12h]
noswap2:
		cmp	dx, [di+edge.py]  ; [di+0Ch]
		jle	short noswap3
		mov	dx, [di+edge.py]  ; [di+0Ch]
		mov	[sstp.l], cx
		mov	[sstp.r], cx
noswap3:
		cmp	si, [di+edge.py]  ; [di+0Ch]
		jge	short noswap4
		mov	si, [di+edge.py]  ; [di+0Ch]
noswap4:
		add	di, 36 ; 24h
		inc	cx
		cmp	cx, [bp+n] ; [bp+6]
				   ; dcf.j ; number of visible edges/points
		jne	short minmax

		cmp	bx, 0
		jl	exitDraw
		cmp	ax, XMAX  ; 140h
		jg	exitDraw

		cmp	si, YMIN  ; 15h
		jl	exitDraw
		cmp	dx, YMAX  ; 0B3h
		jg	exitDraw
		mov	[sstp.miny], dx
		mov	[sstp.maxy], si

		;{ down clipping }
		cmp	word [sstp.maxy], YMAX ; 0B3h
		jle	short noclipdown
		mov	word [sstp.maxy], YMAX ; 0B3h
noclipdown:
		mov	ax, [sstp.miny]
		cmp	ax, [sstp.maxy]
		je	exitDraw
		mov	[sstp.y], ax
		mov	bx, [bp+col]	   ; [bp+4]
					   ; dcf.col ; shade/color value
		add	bx, bx
		mov	es, [vseg]
		mov	fs, [bx+tseg]
for:
		mov	ax, [sstp.y]
		cmp	ax, [sstp.maxy]
		jg	exitDraw
		je	exitwhile2
while1:
		;mov	ax, [sstp.y]
		;cmp	ax, [sstp.maxy]
		;jge	short while2	

		mov	si, [bp+pol]	   ; [bp+8]
		imul	bx, word [sstp.l], 36
		add	si, bx
		;mov	si, bx
		;add	si, [bp+pol]	   ; [bp+8]

		cmp	ax, [si+edge.py]   ; [si+0Ch]
		jne	short while2

		mov	bx, [sstp.l]
		dec	bx
		jge	short nounder
		mov	bx, [bp+n]         ; [bp+6]
					   ; dcf.j ; visible edges/points
		dec	bx
nounder:
		mov	[sstp.l], bx
		xor	di, di
		call	calcDeltas
		mov	ax, [sstp.y]
		cmp	ax, [sstp.maxy]
		jge	short exitwhile2	
		jmp	short while1
while2:
		;mov	ax, [sstp.y]
		cmp	ax, [sstp.maxy]
		jge	short exitwhile2
		mov	si, [bp+pol]	   ; [bp+8]
		imul	bx, word [sstp.r], 36
		add	si, bx
		;
		;mov	si, bx
		;add	si, [bp+pol]	   ; [bp+8]

		cmp	ax, [si+edge.py]   ; [si+0Ch]
		jne	short exitwhile2

		mov	bx, [sstp.r]
		inc	bx
		cmp	bx, [bp+n]	   ; [bp+6]
		jl	short noover
		xor	bx, bx
noover:
		mov	[sstp.r], bx
		mov	di, 20  ; 14h
		call	calcDeltas
		;
		mov	ax, [sstp.y]
		jmp	short while2
exitwhile2:
		;{ up clipping }
		cmp	word [sstp.y], YMIN	; 15h
		jl	clipup
		xor	ebx, ebx
		xor	ecx, ecx
		mov	bx, [sstp.edg+2+edges.x]
		mov	[sstp.mx1], bx
		mov	cx, [sstp.edg+20+2+edges.x]
		mov	[sstp.mx2], cx
		sub	cx, bx
		jz	exitol
		cmp	word [sstp.mx2], 0
		jle	exitol
		cmp	word [sstp.mx1], XMAX ; 140h
		jg	exitol

		neg	bx
		jns	short myelse
		xor	bx, bx
		jmp	short myendif
_nodivbyzero:
		mov	eax, [di+sstp.del+40]
		jmp	short nodivbyzero
myelse:
		mov	word [sstp.mx1], 0
myendif:
		mov	di, 16 ; 10h
addloop1:
		;{ calculate deltas }
		mov	eax, [di+sstp.edg+20]
		sub	eax, [di+sstp.edg]
		cdq
		;jcxz	nodivbyzero
		jcxz	_nodivbyzero
		idiv	ecx
		mov	[di+sstp.del+40], eax
nodivbyzero:
		;{ left clipping }
		mov	eax, [di+sstp.del+40]
		imul	ebx
		add	eax, [di+sstp.edg]
		mov	[di+sstp.edg+40], eax
		sub	di, 4
		jnz	short addloop1

		mov	ax, [sstp.del+40+16]
		;mov	[cs:dels_pos+1], ax
		mov	[dels_pos_w], ax

		;{ right clipping }
		cmp	word  [sstp.mx2], XMAX
		jle	short norightclip
		mov	word  [sstp.mx2], XMAX  ; 140h
norightclip:
		imul	di, word [sstp.y], 320
		add	di, [sstp.mx1]
		
		mov	bx, 256 ; 100h
		mov	eax, [sstp.edg+40+edges.u]
		imul	ebx
		idiv	dword [sstp.edg+40+edges.w]
		mov	[sstp.uu2], ax
		mov	eax, [sstp.edg+40+edges.v]
		imul	ebx
		idiv	dword [sstp.edg+40+edges.w]
		mov	[sstp.vv2], ax
outloop:
		mov	cx, [sstp.mx2]
		sub	cx, [sstp.mx1]
		jle	exitol
		cmp	cx, SUBRANGE ; 10h
		jle	short lastSeg
		mov	cx, SUBRANGE ; 10h
lastSeg:
		;{ uu1:=uu2 }
		;{ vv1:=vv2 }
		mov	eax, [sstp.vv2]
		mov	[sstp.vv1], eax

		mov	eax, [sstp.del+40+edges.u]
		imul	ecx
		add	[sstp.edg+40+edges.u], eax
		mov	eax, [sstp.del+40+edges.v]
		imul	ecx
		add	[sstp.edg+40+edges.v], eax
		mov	eax, [sstp.del+40+edges.w]
		imul	ecx
		add	[sstp.edg+40+edges.w], eax

		mov	bx, 256 ; 100h
		mov	eax, [sstp.edg+40+edges.u]
		imul	ebx
		idiv	dword [sstp.edg+40+edges.w]
		mov	[sstp.uu2], ax
		mov	eax, [sstp.edg+40+edges.v]
		imul	ebx
		idiv	dword [sstp.edg+40+edges.w]
		mov	[sstp.vv2], ax

		mov	ax, [sstp.uu2]
		sub	ax, [sstp.uu1]
		cwd
		idiv	cx
		;mov	[cs:ddu_pos+2], ax
		mov	[ddu_pos_w], ax

		mov	ax, [sstp.vv2]
		sub	ax, [sstp.vv1]
		cwd
		idiv	cx
		;mov	[cs:ddv_pos+2], ax
		mov	[ddv_pos_w], ax

		mov	si, [sstp.vv1]
		mov	dx, [sstp.uu1]
		mov	ax, [sstp.edg+40+16]
innerloop:
		mov	bx, si
		xor	bl, bl
		shr	bx, 2
		add	bl, dh
		mov	bl, [fs:bx]
		or	bl, bl
		jz	short dels_pos
		mov	bh, ah
		mov	bl, [bx+shadetab]

		mov	[es:di], bl

;dels_pos:	add ax, 1111h ;{word ptr dels}
;ddu_pos:	add dx, 1111h ;{word ptr ddu }
;ddv_pos:	add si, 1111h ;{word ptr ddv }

dels_pos:	
		add	ax, [dels_pos_w]
ddu_pos:	
		add	dx, [ddu_pos_w]
ddv_pos:	
		add	si, [ddv_pos_w]

		inc	di	
		loop	innerloop

		mov	[sstp.edg+40+16], ax
		add	word [sstp.mx1], SUBRANGE
		jmp	outloop
exitol:
clipup:		mov	di, 36 ; 24h
addloop:
		mov	eax, [di+sstp.del]
		add	[di+sstp.edg], eax
		sub	di, 4
		jns	short addloop
		inc	word [sstp.y]
		jmp	for
exitDraw:
		pop	bp
	
		ret	6

  ;***********************************************
  ;* descript. : calc deltas for vertical interp.*
  ;* parameter : none			         *
  ;* sideeffect: all		 	         *
  ;***********************************************

calcDeltas:
		imul	dx, bx,	36
		mov	bx, si
		mov	si, [bp+pol]	; [bp+8]
		add	si, dx
		;
		;mov	si, dx
		;add	si, [bp+pol]	; [bp+8]	
		mov	ax, [si+12]
		sub	ax, [sstp.y]
		movsx	eax, ax
		mov	[sstp.dy], eax
		mov	cx, 5
cDloop:
		mov	edx, [bx+16]
		mov	[di+sstp.edg], edx
		mov	eax, [si+16]
		sub	eax, edx
		cdq
		cmp	word [sstp.dy], 0
		je	short cDskip
		idiv	dword [sstp.dy]
		mov	[di+sstp.del], eax
cDskip:
		add	di, 4
		add	si, 4
		add	bx, 4
		loop	cDloop
		retn


;***********************************************
;* descript. : clip a poly at the viewplane,   *
;*             project and draw it	       *
;* parameter : bx:adress of the face	       *
;* sideeffect: all		 	       *
;* back      : none			       *
;***********************************************

drawclippedface:
		; 11/08/2017

		dcf.d	equ -2	  ; word
		dcf.h	equ -4	  ; word	
		dcf.j	equ -6	  ; word
		dcf.col equ -8	  ; word
		dcf.is_in equ -10 ; byte
		dcf.p0	equ -190  ; poly (5*36 bytes)
		dcf.p2	equ -370  ; poly (5*36 bytes)
		;
		;dcf.pad equ -372  ; padding (for dword boundary)

		enter	370, 0  ; 172h
		;enter	372, 0  ; 174h ; 12/12/2016
 			; push bp ; mov bp, sp ; sub sp, 372
		mov	si, uvtab
		xor	di, di
		mov	cx, 4
dCFinitloop:
		push	bx	; face addr (in 'object.f')(29/01/2017)
		mov	bx, word [bx] ; point index into 'rp:' ? (29/01/2017)

		push	bx
		imul	bx, bx, 12 ; (point vector size = 12 bytes)
		add	bx, rp	  ; addr of rotated point vects (29/01/2017)
		mov	eax, dword [bx]
		; vector(4-cx).x
		mov	dword [bp+di+dcf.p0+0], eax	; [bp+di-0BEh]
		; vector(4-cx).y
		mov	eax, dword [bx+4]
		mov	dword [bp+di+dcf.p0+4], eax	; [bp+di-0BAh]

		fld	dword [bx+8]
		; vector(4-cx).z
		fst	dword [bp+di+dcf.p0+8]	; [bp+di-0B6h]
		fistp	word [bp+dcf.h]		; [bp-04h]	
						; dcf.h = z distance
		pop	bx

		mov	ax, word [bp+dcf.h]		; [bp-04h]
		add	ax, 512	; 200h
		js	dCFbackclip   ; not visible ? (29/01/2017) 

		cmp	ax, 511 ; 1FFh
		jle	short intensityOK
		mov	ax, 511 ; 1FFh		; maximum z distance (dept) ?
intensityOK:
		shl	bx, 3 ; point index * 8
		xor	edx, edx
		shl	ax, 7
		mul	word [bx+6+object.p]
		; edges(4-cx).s
		mov	dword [bp+di+dcf.p0+32], edx	; [bp+di-9Eh]

		xor	ah, ah
		mov	al, byte [si+4]
		shl	eax, 16	; 10h
		; edges(4-cx).v
		mov	dword [bp+di+dcf.p0+24], eax	; [bp+di-0A6h]

		lodsb
		shl	eax, 16	; 10h
		; edges(4-cx).u
		mov	dword [bp+di+dcf.p0+20], eax	; [bp+di-0AAh]

		pop	bx
		inc	bx
		inc	bx
		add	di, 36	; 24h  ; size of edge
		loop	dCFinitloop
		;dec	cx
		;jnz	dCFinitloop

		; end of p1, p2, p3, p4 point dimensioning of face
		; 
		; p1 ...... p2
		;  .	    .	
		;  . shade  .
		;  .        .
		; p3 ...... p4
		;
		; p1 (x,y,z) ... p4 (x,y,z)
		;

		mov	ax, word [bx]		; shade
		mov	word [bp+dcf.col], ax	; [bp-08h]

		xor	bx, bx
		mov	word [bp+dcf.j], bx	; [bp-06h]

		; vector(3).z
		fld	dword [bp+dcf.p0+108+8] ; [bp-4Ah]
		ftst
		;fstsw	ax
		fnstsw	ax
		fstp	st0
		sahf
		jae	short isFalse
		dec	bx
isFalse:
		mov	byte [bp+dcf.is_in], bl	; [bp-0Ah]
		xor	si, si
		xor	di, di 
forloop:
		; vector(0-3).z
		fld	dword [bp+si+dcf.p0+8]	; [bp+si-0B6h]
		ftst
		;fstsw	ax
		fnstsw	ax
		fstp	st0
		sahf
		jae	short elseFall

		cmp	byte [bp+dcf.is_in], 0FFh ; -1 ; true
		je	short inside
		call	clipsub
		; { OUT / IN }
		not	byte [bp+dcf.is_in]	; [bp-0Ah]
inside:
		push	di
		push	si
		lea	di, [bp+di+dcf.p2]	; [bp+di-172h]
		lea	si, [bp+si+dcf.p0]	; [bp+si-0BEh]

		push	ss
		pop	es
		push	ds
		push	ss
		pop	ds
		mov	cx, 18	; 12h
		rep movsw			; { IN / IN }
		pop	ds

		pop	si
		pop	di
		add	di, 36	; 24h
		inc	word [bp+dcf.j]		; [bp-06h]
		jmp	short dCFendif
elseFall:
		cmp	byte [bp+dcf.is_in], 0FFh ; [bp-0Ah]
		jne	short outside
		call	clipsub
		; { IN / OUT }
		not	byte [bp+dcf.is_in]	; [bp-0Ah]
outside:
dCFendif:	add	si, 36 ; 24h
		cmp	si, 4*36 ; 90h
		jne	short forloop

		mov	cx, word [bp+dcf.j]	; [bp-06h]
		cmp	cx, 2
		jl	dCFnodraw
		xor	di, di
dCFloop:
		fild	word [CONST13]
		fsub	dword [bp+di+dcf.p2+8]	; [bp+di-16Ah]
		fidiv	word [CONST160]
		
		fld	dword [bp+di+dcf.p2+4]	; [bp+di-16Eh]
		fdiv	st0, st1
		fiadd	word [CONST100]
		fistp	dword [bp+di+dcf.p2+12] ; [bp+di-166h]
		
		fld	dword [bp+di+dcf.p2]	; [bp+di-172h]
		fmul	dword [ASPECT_PLACE]
		fdiv	st0, st1
		fiadd	word [CONST160]
		frndint
		fimul	dword [CONST65536]
		fistp	dword [bp+di+dcf.p2+16] ; [bp+di-162h]

		fild	dword [bp+di+dcf.p2+20] ; [bp+di-15Eh]
		fdiv	st0, st1
		fistp	dword [bp+di+dcf.p2+20] ; [bp+di-15Eh]
		
		fild	dword [bp+di+dcf.p2+24] ; [bp+di-15Ah]
		fdiv	st0, st1
		fistp	dword [bp+di+dcf.p2+24] ; [bp+di-15Ah]
		
		fild	dword [CONST65536]
		fdiv	st0, st1
		fistp	dword [bp+di+dcf.p2+28] ; [bp+di-156h]
		
		fstp	st0
		add	di, 36 ; 24h
		loop	dCFloop
		;dec	cx
		;jnz	dCFloop

		lea	di, [bp+dcf.p2]		; [bp-172h]
		push	di
		push	word [bp+dcf.j]		; [bp-06h]	 
		push	word [bp+dcf.col]	; [bp-08h]
		call	scansubtextpoly
dCFnodraw:
dCFbackclip:	leave
			; mov sp, bp ; pop bp
		retn

clipsub:
		push	si
		push	di
		lea	ax, [si-36]		; [si-24h]
		cmp	ax, 0
		jge	short cSnounder
		mov	ax, 108 ; 6Ch
cSnounder:
		fld	dword [bp+si+dcf.p0+8]	; [bp+si-0B6h]
		fld	st0
		xchg	ax, di
		fsub	dword [bp+di+dcf.p0+8]	; [bp+di-0B6h]
		xchg	ax, di
		fdivp	st1, st0
		;fdiv

		mov	bx, cliptab
		mov	cx, 6
dCFclipLoop2:
		xchg	ax, di
		fld	dword [bp+di+dcf.p0]	; [bp+di-0BEh]
		xchg	ax, di
		fsub	dword [bp+si+dcf.p0]	; [bp+si-0BEh]
		fmul	st0, st1
		fadd	dword [bp+si+dcf.p0]	; [bp+si-0BEh]
		fstp	dword [bp+di+dcf.p2]	; [bp+di-172h]

		mov	dl, byte [bx]
		inc	bx
		xor	dh, dh
		add	di, dx
		add	si, dx
		add	ax, dx
		loop	dCFclipLoop2
		fstp	st0
		pop	di
		pop	si
		inc	word [bp+dcf.j] 	; [bp-6]
		add	di, 36 ; 24h
		retn

;***********************************************
;* descript. : rotate point over one axis      *
;* parameter : st(0):angle, [ds:si] ptr to     *
;*	       1.koord.,[ds:bx] ptr to 2.koord.*
;* sideeffect: empty copro-stack               *
;* back      : none			       *
;***********************************************

rotateAxis:
		fldpi				;{ PI,a}
		fmulp	st1, st0		;{ PI*a}	
		;fmul
		fidiv	dword [CONST65536]	;{ PI*a/65536}
		fsincos				;{ cos,sin}
		fld	st0			;{ cos,cos,sin}
		fmul	dword [si] 		;{ y*cos,cos,sin}
		fld	st2			;{ sin,y*cos,cos,sin}
		fmul	dword [bx]		;{ z*sin,y*cos,cos,sin}
		fsubp	st1, st0		;{ y*cos-z*sin,cos,sin}
		fxch	st2			;{ sin,cos,y*cos-z*sin}
		fmul	dword [si]		;{ y*sin,cos,y*cos-z*sin}
		fxch	st1			;{ cos,y*sin,y*cos-z*sin}
		fmul	dword [bx]		;{ z*cos,y*sin,y*cos-z*sin}
		faddp	st1, st0		;{ y*sin+z*cos,y*cos-z*sin}
		fstp	dword [bx]		;{ y*cos-z*sin}
		fstp	dword [si]		;{}
		retn

;***********************************************
;* descript. : rotate point		       *
;* parameter : a,b,c:angles,adr:address        *
;* sideeffect: all		 	       *
;* back      : none			       *
;***********************************************

xyzRotate:
		adr	equ 4	; arg 1, [sp+2]
		c	equ 6	; arg 2, [sp+4]	
		b	equ 8	; arg 3, [sp+6]
		a	equ 10	; arg 4, [sp+8]

		push	bp
		mov	bp, sp
	
		mov	di, [bp+adr]
		fild	word [bp+a]
		lea	bx, [di+8]
		lea	si, [di+4]
		call	rotateAxis
		fild	word [bp+b]
		lea	bx, [di]
		lea	si, [di+8]
		call	rotateAxis
		fild	word [bp+c]
		lea	bx, [di+4]
		lea	si, [di]
		call	rotateAxis

		pop	bp
		ret	8

;***********************************************
;* descript. : calculate world matrix	       *
;* parameter : x-,y-,z-angle,neig:stack	       *
;* sideeffect: all		 	       *
;* back      : none			       *
;***********************************************

calcRotMat:
		neig	equ 4   ; arg 1, [sp+2]
		zw	equ 6	; arg 2, [sp+4]	
		yw	equ 8	; arg 3, [sp+6]
		xw	equ 10  ; arg 4, [sp+8]

		push	bp
		mov	bp, sp

		mov	di, nwmat
		identityMat
		xor	di, di
cRMolp:
		push	di
		push	word [bp+xw]
		push	word [bp+yw]
		push	word [bp+zw]
		add	di, nwmat
		push	di
		call	xyzRotate
		pop	di
		xor	bx, bx
cRMilp:
		fld	dword [di+nwmat]
		fmul	dword [bx+owmat]
		fld	dword [di+nwmat+4]
		fmul	dword [bx+owmat+12]
		fld	dword [di+nwmat+8]
		fmul	dword [bx+owmat+24]
		fadd
		fadd
		fst	dword [bx+di+owmat]
		fstp	dword [bx+di+wmat]
		add	bx, 4
		cmp	bl, 12
		jne	short cRMilp
		add	di, 12
		cmp	di, 36
		jne	short cRMolp

		mov	si, wmat
		mov	cx, 3
cRMneigloop:
		fild	word [bp+neig]
		lea	bx, [si+12]
		call	rotateAxis
		add	si, 4
		loop	cRMneigloop

		pop	bp
		ret	8

;***********************************************
;* descript. : create the world		       *
;* parameter : none			       *
;* sideeffect: all		               *
;* back      : none			       *
;***********************************************

createWorld:
		; 11/08/2017

		mov	si, world
		mov	cx, 4
cWworldLoop:
		push	cx
		push	si
		mov	di, p
		mov	bx, pr
		mov	si, print
		mov	cl, 15 ; 0Fh
cWinitStartRoom:
		push	cx
		mov	cl, 3
cWinnerLoop:
		lodsb  ; byte data (to be converted to dword) from the 'print:'
		cbw    ; convert byte to word with sign (0E0h -> FFE0h)	
		stosw  ; store world coordinate/dimension as integer (word), to 'p:' 
		fild	word [di-2]
		fstp	dword [bx] ; store coordinate as floatpoint
		add	bx, 4 ; next dimension (x,y,z) in 'pr:'
		loop	cWinnerLoop
		pop	cx
		mov	ax, 127*256 ; 7F00h ;  4th dimension in 'p:'
		stosw
		loop	cWinitStartRoom
		pop	si
cWmainLoop:
		lodsb	; the byte from 'world:' for shade and direction
		cmp	al, 255 ; 0FFh
		je	cWexit  ; end of stage (1 to 4)

		mov	bh, al  ; High 4 bits for shading
		and	bx, 7000h
		add	bh, 15 ; 0Fh
		mov	[cw.shade], bx
		and	ax, 0Fh ; 15   ; Low 4 bits for direction
		mov	[cw.direc], ax

		xor	bx, bx
cWsideLoop:
		test	bl, 4
		jz	short cWfirstNibble
		lodsb
		and	al, 0Fh ; 15
		jmp	short cWsecondNibble
cWfirstNibble:
		mov	al, [si]
		shr	al, 4
cWsecondNibble:
		xor	ah, ah
		dec	ax
		js	short cWnoWall ; ax = 0 -> ax = 0FFFFh
		imul	di, word [object.fanz], 10 ; 0Ah
		add	di, object.f ; base address + offset
		inc	word [object.fanz] ; faces = faces + 1
		mov	word [di+8], ax ; 4th word is shading value

		push	bx
		push	si
		mov	cl, 4
cWaddPointloop:
		movsx	si, byte [bx+cube] ; cube coordinate offset
		add	si, p ; + base address (p:)

		push	di
		mov	di, object.p  ; p area of object structure
		push	cx
		mov	cx, [object.panz] ; number of points (for object.p)
		xor	ax, ax
		jcxz	cWfirstPoint
cWsearchPointLoop: ; check point coordinates are same or not
		mov	edx, [si]
		cmp	edx, [di]
		jne	short cWdifferent
		mov	dx, [si+4]
		cmp	dx, [di+4]
		je	short cWpointExists
cWdifferent:
		inc	ax ; number of different points (of object)
		add	di, 8
		loop	cWsearchPointLoop
cWfirstPoint:
		movsd
		movsd
		inc	word [object.panz] ; points = points + 1
cWpointExists:
		pop	cx
		pop	di
		stosw   ; store num of different points
			; in the Xth word of face structure (5 words)
			; (Xth word of 'object.f', X = 4-CL)
		inc	bx
		loop	cWaddPointloop
		pop	si
		pop	bx
cWnoWall:
		add	bx, 4
		cmp	bx, 24 ; 18h
		jne	short cWsideLoop

		imul	di, word [cw.direc], 12
		; max value of [cw.direc] = 0Fh
		mov	cl, 3
		mov	bx, pr+96 ; 24th dword of 'pr:'
cWaddStepLoop:
		fld	dword [bx+72] ; [bx+48h]
		fadd	dword [bx+di]
		fstp	dword [bx+72]
		add	bx, 4
		loop	cWaddStepLoop

		cmp	byte [si-4], 0
		jge	short cWsimpleRoom

		mov	bx, pr
		mov	cl, 14 ; 0Eh
cWrotloop:
		pusha
		mov	cl, 3
		mov	dl, [si]
cWpushLoop:
		xor	ax, ax
		shr	dl, 1
		jnc	short cWnoRot
		mov	ah, [si]
		;and	ax, 0F000h
		and	ah, 0F0h
		;push	ax
		;jmp	short cWendIf
cWnoRot:
		;push	0 ; arg 2 (c), arg 3 (b), arg 4 (a)
		push	ax
cWendIf:
		loop	cWpushLoop
		push	bx ; arg 1 (addr)
		call	xyzRotate
		popa
		add	bx, 12 ; 0Ch
		loop	cWrotloop
		inc	si
cWsimpleRoom:
		mov	bx, [cw.direc]
		shl	bx, 2
		mov	cl, 4
cWpointCopyLoop:
		pusha
		movsx	si, byte [bx+cube]
		mov	ax, si
		add	si, p ; + base address of 'p:'
		xor	bl, 7
		movsx	di, byte [bx+cube]
		add	di, p ; + base address of 'p:'
		push	si 
		movsd
		movsd
		pop	si

		mov	bx, ax
		shr	bx, 1
		add	bx, ax
		mov	di, pr ; + base address of 'pr:'
		mov	cl, 3
cWSround:
		fld	dword [bx+di]
		fadd	dword [di+168]  ; [di+0A8h]
		fistp	word [si] ; world coordinate/dimension
		add	di, 4
		inc	si
		inc	si
		loop	cWSround
		mov	ax, [cw.shade]
		mov	[si], ax ; 4th word
		popa
		inc	bx
		loop	cWpointCopyLoop
		jmp	cWmainLoop
cWexit:
		pop	cx
		dec	cx
		jnz	cWworldLoop

		retn

dels_pos_w:	dw 1111h
ddu_pos_w:	dw 1111h
ddv_pos_w:	dw 1111h

prg_msg:
		db	'ERDOGAN TAN - SNCOMNI.COM'
		db	0Dh, 0Ah
		db	'12/08/2017'
		db	0Dh, 0Ah
		db	0 

;=============================================================================
;               preinitialized data
;=============================================================================

ASPECT_PLACE:	dd ASPECT_RATIO		; 1.2
CONST13:	dw 13			; 000Dh			
CONST100:	dw CENTERY		; 0064h
CONST160:	dw CENTERX		; 00A0h
CONST1792:	dw 1792			; 0700h
CONST65536:	dd 65536		; 00010000h
randommul:	dd 134775813		; 08088405h
randomseed:	dd 4347			; 000010FBh
credits:
		db	 2, 80,-5
		db      -2,50,4,69,2,62,2,69,2,81,2,62,2,69,2,79,2,62,2,69,2
		db	81,2,62,2,69,2,79,2,81,2,0	; { pat 0,2,4,6,8 }
		db      -2,50,4,70,2,62,2,70,2,81,2,62,2,70,2,79,2,62,2,70,2
		db	82,2,62,2,70,2,79,2,82,2,0	; { pat 1,3,5,7,9 }
		db       0
		db      55,32,56,32			; { pat 10 }
		db      51,32,53,24,55,8                ; { pat 11 }
		db      57,32,50,32		 ; { pat 12 }
		db      60,48,58,8,57,8                 ; { pat 13 }
		db      55,64		   ; { pat 14 }
		db      -2,67,16,62,16,69,16,70,16,0	; { pat 15 }
		db      -2,70,16,69,16,62,16,65,16,0    ; { pat 16 }
		db       0
		db       4, 50,1,128,1,128              ;{ pat 0-3 }
		db      65,28,64,2,65,2,64,16,62,8,60,8 ;{ pat 4 }
		db      67,28,65,4,60,16,65,8,64,8      ;{ pat 5 }
		db      62,32,50,16,64,8,65,8           ;{ pat 6 }
		db      67,32,60,16,62,8,64,8           ;{ pat 7 }
		db      62,128		  ;{ pat 8,9 }
		db      67,128		  ;{ pat 10,11 }
		db     	67,16,64,16,66,16,64,8,66,8	;{ pat 12 }
		db     	72,48,70,16                 	;{ pat 13 }
		db     	67,32,55,32                 	;{ pat 14 }
		db     	-2,31,32,55,32,0                ;{ pat 15 }
		db      -2,26,32,50,32,0                ;{ pat 16 }
		db       0
		db       5, 89,-5,1,128,0		;{ pat 0-9 }
		db      67,8,74,24,68,8,75,24           ;{ pat 10 }
		db      63,8,70,24,60,8,69,16,67,8      ;{ pat 11 }
		db      62,8,69,16,67,8,62,8,66,24	;{ pat 12 }
		db      63,8,67,24,60,8,67,16,69,8	;{ pat 13 }
		db      55,8,67,24,43,8,50,24           ;{ pat 14 }
		db      -2,31,1,38,63,0                	;{ pat 15 }
		db      -2,26,1,33,63,0                	;{ pat 16 }
		db       0
		db	10,  0,-16,42,2,42,2,42,2,42,1,42,1,0 ;{ pat 0,1 }
		db     -16,36,2,42,2,42,2,42,1,42,1,0   ;{ pat 2,3 }
		db     -30,36,2,42,2,46,2,36,1,42,1,38,2
		db	42,2,46,2,38,1,42,1     	;{ pat 4-16 }
		db      36,2,38,2,36,2,42,1,42,1,38,2,42,2,46,2,38,1,42,1,0
		db     	 0
		db	 0

; colortable 65 bytes
colors 		db	16
                db      31,63,63,63
                db	 1, 0, 0, 0
                db      31,40,32,63
                db	 1, 0, 0, 0
                db      31,63, 0, 0
                db	 1, 6, 1, 0
                db      31,63,41,20
                db	 1, 0, 0, 0
                db      31,63,63, 8
                db	 1, 0, 0, 0
                db	31,56,56,63
                db	 1,63, 0, 0
                db      16,63,63, 0
                db      16,63, 0, 0
                db	 1,22, 5, 0
                db       7,63,56,17

; parameter for the texture effects 156 bytes
aE              db	013h,15,15,49,49,10
                db      013h,16,16,48,48,17
                db      013h,17,17,47,47,24
                db      013h,18,18,46,46,190
                db      013h,21,21,43,43,30
                db      025h,0,0,62,30,3
                db      025h,3,3,62,30,7
                db      025h,3,3,59,27,0FCh
                db      025h,0,32,62,62,3
                db      025h,3,35,62,62,7
                db      025h,3,35,59,59,0FCh
                db      025h,0,0,63,63,0FEh
                db      02Ch,0,10,63,15,0F6h
                db      02Ch,0,11,63,16,4
                db      02Ch,0,47,63,52,0F6h
                db      02Ch,0,48,63,53,4
                db      02Eh,19,24,44,39,5
                db      02Eh,20,25,44,39,0F4h
                db      02Eh,20,25,43,38,7
                db      03Eh,20,25,43,38,6
                db      02Fh,0,24,63,30,6
                db      02Fh,0,25,63,31,0FAh
                db      03Fh,0,25,63,30,6
                db      02Fh,0,32,63,38,6
                db      02Fh,0,33,63,39,0FAh
                db      03Fh,0,33,63,38,6

; parameter for fractaladd 28 bytes
aF:             db      34,192,38,1,38,1,38,0,32,0,32,32,32,64
                db	34,96,36,0,34,96,36,0,36,0,36,0,36,0

; star bob for the sparcling stars texture 25 bytes
bob:            db	0,0,3,0,0
                db	0,2,5,2,0
                db	3,5,7,5,3
                db	0,2,5,2,0
                db	0,0,3,0,0

cliptab:	db	4,4,12,4,8

uvtab:		db	0,0,63,63,63,0,0,63

cube:		db	1*8,5*8,6*8,2*8
                db      3*8,7*8,4*8,0*8
                db      2*8,6*8,7*8,3*8
                db      0*8,4*8,5*8,1*8
        	db	7*8,6*8,5*8,4*8
                db      0*8,1*8,2*8,3*8

print:		db	-32,-32,-32
                db       32,-32,-32
                db   	 32, 32,-32
                db  	-32, 32,-32
                db  	-32,-32, 32
                db       32,-32, 32
                db   	 32, 32, 32
                db  	-32, 32, 32
                db       64,  0,  0
                db      -64,  0,  0
                db        0, 64,  0
                db        0,-64,  0
                db        0,  0, 64
                db        0,  0,-64
                db	  0,  0,  0

; world contruction data 599 bytes
world:		db	071h,070h,000h,006h
                db	072h,000h,000h,006h
                db	071h,000h,070h,006h
                db	073h,007h,070h,006h
                db	073h,007h,000h,006h
                db	070h,007h,007h,006h
                db	074h,000h,007h,006h
                db	070h,000h,007h,040h
                db	072h,070h,007h,050h
                db	072h,070h,000h,050h
                db	071h,070h,070h,050h
                db	073h,000h,070h,040h
                db	071h,000h,000h,040h
                db	072h,000h,000h,050h
                db	071h,000h,070h,050h
                db	073h,007h,070h,046h
                db	073h,000h,000h,046h
                db	070h,007h,007h,046h
                db	072h,000h,007h,050h
                db	071h,000h,000h,000h
                db	071h,000h,000h,000h
                db	071h,000h,0bbh,056h
                db	070h,00Fh,0bbh,046h
                db	0FFh
                db	033h,000h,000h,000h
                db	0F0h,000h,007h,006h,0E4h
                db	0B0h,000h,0BBh,046h,0E4h
                db	090h,000h,0BBh,056h,0E4h
                db	090h,000h,0BBh,056h,0E4h
                db	030h,000h,0BBh,056h
                db	0F0h,000h,0BBh,056h,011h
                db	0F0h,000h,0BBh,046h,011h
                db	0B0h,000h,0BBh,056h,011h
                db	090h,000h,0BBh,056h,011h
                db	0B0h,000h,0BBh,056h,011h
                db	0F0h,000h,0BBh,056h,011h
                db	0F0h,000h,0BBh,046h,011h
                db	0B0h,000h,0BBh,056h,011h
                db	030h,000h,0BBh,056h
                db	010h,000h,0BBh,056h

                db	034h,000h,000h,000h
                db	032h,005h,000h,080h
                db	035h,005h,080h,080h
                db	035h,005h,080h,000h
                db	033h,005h,080h,008h
                db	033h,005h,000h,008h
                db	074h,005h,008h,008h
                db	074h,005h,000h,000h
                db	073h,005h,008h,080h
                db	075h,000h,000h,000h
                db	075h,005h,008h,088h
                db	070h,000h,000h,000h
                db	074h,064h,008h,008h
                db	074h,060h,008h,000h
                db	072h,064h,008h,080h
                db	005h,060h,000h,080h
                db	005h,060h,000h,000h
                db	002h,060h,000h,008h
                db	034h,060h,000h,008h
                db	034h,060h,000h,000h
                db	032h,060h,000h,080h
                db	035h,060h,000h,080h
                db	035h,060h,000h,000h
                db	032h,060h,000h,008h
                db	034h,035h,000h,008h
                db      014h,065h,000h,000h
                db      032h,035h,000h,080h
               	db      035h,035h,000h,080h
               	db      015h,065h,000h,000h
                db	032h,035h,000h,008h
                db	034h,035h,080h,008h
               	db      014h,065h,000h,000h
               	db      032h,035h,080h,080h
		db	075h,000h,000h,000h
		db	052h,065h,0E0h,077h
		db	032h,064h,00Eh,077h
		db	012h,065h,000h,077h
		db	000h,065h,000h,077h
		db	0FFh
		db	032h,000h,000h,000h
		db	0F0h,000h,000h,000h,014h
		db	0B0h,000h,000h,000h,014h
		db	030h,000h,000h,000h
		db	0F0h,000h,000h,000h,0F4h
		db	0F0h,000h,0DDh,056h,0F4h
		db	030h,000h,0DDh,046h
		db	030h,000h,0DDh,056h
		db	030h,000h,0DDh,056h
		db	032h,000h,000h,050h
		db	030h,00Ah,0A0h,050h
		db	033h,000h,0A0h,050h
		db	030h,000h,000h,050h
		db	032h,070h,000h,050h
		db	030h,000h,0A0h,050h
		db	033h,0A0h,0A7h,050h
		db	033h,000h,000h,000h
		db	031h,0A0h,07Ah,050h
		db	031h,000h,00Ah,050h
		db	031h,000h,00Ah,050h
		db	032h,00Ah,00Ah,050h
		db	035h,000h,000h,000h
		db	072h,00Ah,000h,000h
		db	070h,00Ah,0A0h,000h
		db	073h,000h,0A0h,000h
		db	070h,000h,000h,000h
		db	072h,000h,000h,000h
		db	070h,000h,0A0h,000h
		db	073h,0A0h,0A0h,000h
		db	073h,0A0h,000h,099h
		db	071h,0A0h,00Ah,000h
		db	071h,000h,00Ah,000h
                db	071h,000h,00Ah,000h
                db	075h,00Ah,00Ah,000h
                db	070h,00Ch,00Ch,003h
                db	070h,000h,00Ch,003h
                db	070h,000h,00Ch,003h
                db	072h,0C0h,07Ch,003h
                db	072h,000h,000h,000h
                db	071h,0C0h,0C7h,003h
                db	073h,000h,0C0h,003h
                db	071h,070h,000h,003h
                db	072h,000h,000h,003h
                db	071h,000h,0C0h,003h
                db	073h,00Ch,0C0h,003h
                db	071h,000h,000h,003h
                db	031h,00Ch,0CCh,0A3h
                db	0FFh
                db	032h,000h,000h,000h
                db	0F0h,000h,070h,006h,014h
                db	0B0h,000h,0DDh,046h,014h
                db	030h,000h,0DDh,056h
                db	0B2h,000h,00Dh,056h,0C4h
                db	032h,0DDh,000h,056h
                db	032h,0DDh,000h,056h
                db	032h,0DDh,000h,056h
                db	070h,000h,0F0h,056h
                db	070h,000h,0BBh,056h
                db	032h,0B0h,00Bh,046h
                db	072h,0BBh,000h,056h
                db	071h,0B0h,0B0h,056h
                db	071h,002h,011h,056h
                db	071h,022h,011h,056h
                db	031h,020h,011h,056h
                db	073h,00Bh,0B0h,056h
                db	073h,0BBh,000h,056h
                db	030h,00Bh,00Bh,046h
                db	030h,000h,0BBh,056h
                db	0FFh

; flying script	126 bytes
script:
;     7 6 5 4 3 2 1 0
;	  v v v v v c c c
;
;	  v = VALUE
;	  c = COMMAND
;
;	  0 = NOP
;	  1 = NEG ZSTEP
;	  2 = INC SPEED
;	  3 = DEC SPEED
;	  4 = INC XSTEP
;	  5 = DEC XSTEP
;	  6 = INC YSTEP
;	  7 = INC YSTEP

                db      0A1h,0f8h,0f8h,098h,0a1h,0A6h
                db      0F0h,0A7h,0B3h,037h,010h,036h
                db      050h,084h,085h,085h,084h,0F0h
                db      0F0h,0B2h,0A7h,0A6h,0A6h,0A7h
                db      0f3h,026h,070h,027h,080h,044h
                db      045h,077h,076h,0a6h,0b0h,061h
                db      061h,052h,0a7h,077h,030h,076h
                db      080h,0a2H,001h,041h,041h,001h
		db	0f5h,030h,0f4h,0b3h,0a4h,0f8h
		db	0a5h,0D0h,047h,046h,0F0h,0A6h
                db      080h,0A7h,070h,0A7h,010h,0A6h
                db      08eh,0f0h,050h,08fh,0b2h,0f8h
                db      0a3h,080h,08eh,0f0h,050h,08fh
                db      0b7h,010h,0b6h,0f0h,080h,0a6h
		db      0a7h,0f0h,087h,0F0h,070h,086h
		db      036h,0f0h,0f0h,090h,037h,083h
                db      02ch,0f0h,0f0h,0f0h,030h,02dh
		db	082h,030h,0b5h,0b4h,0f2h,0a6h
		db	030h,0a7h,083h,083h,0a6h,0f0h
		db	058h,0a7h,077h,076h,0f0h,0f0h
                db	0f0h,0f0h,0f8h,0f8h,000h

zstep:		dw	1			
doortimer	dw 	-4250	; 0EF66h
omniscent	db	"OMNISCENT"
omniend:
sanction        db      "(C) DIRK KšPPERS"
sancend:
ob:		dd 	-330.0	; vector.x
		dd 	   0.0	; vector.y	
		dd	  64.0	; vector.z	
owmat:
		dd 	   0.0	; vector.x
		dd 	  -1.0	; vector.y
		dd	   0.0  ; vector.z
;owmat+12:
		dd 	   0.0	; vector.x
		dd 	   0.0	; vector.y
		dd	  -1.0  ; vector.z
;owmat+24:
		dd 	   1.0	; vector.x
		dd 	   0.0	; vector.y
		dd	   0.0  ; vector.z

bss_start:

ABSOLUTE bss_start

;=============================================================================
;        	null-initialized data
;=============================================================================

nullstart:
wmat:           resb	matrix.size  ; 36
nwmat:		resb	matrix.size  ; 36
zspeed:		resw	1
oxw:		resw	1
oyw:		resw	1
ozw:		resw	1
scriptptr:	resw	1
scriptanz:	resw	1
scriptins:	resb	1
once:		resb	1
ticker:		resw	1
tracks:		resw	1
palette:        resb 	768
channels:       resb	16*6
;o:		resb	object.size ; 7204
; 11/08/2017
object.panz:	resw 1
object.fanz:	resw 1
object.p:	resb point.size*MAXPOINTS
object.f:	resb face.size*MAXFACES
nullend:

;=============================================================================
;       	uninitialized data
;=============================================================================
oldstack        resw	1
Old08IrqPtr:
Old08Irqofs:	resw	1
Old08Irqseg:	resw	1
;Old09IrqPtr:
;Old09Irqofs:	resw	1
;Old09Irqseg:	resw	1
;songdata:	resb	3605
root:   	resw	1
circletab:	resw	32
stars:		resb	90
twice:		resb	1
vseg:		resw	1
bseg:		resw    1
tseg:   	resw	20
shadetab:	resb	256*128
p:		resw	4*15
pr:		resd	3*15
po:		resb	poly.size ; 5*36
rp:		resd	3*MAXPOINTS ; 3*400
facei:		resd    MAXFACES ; 400

alignb 4

; 11/08/2017

cw.direc:	resw	1 ; word
cw.shade:	resw	1 ; word

alignb 4

sstp.dy:	resd	1 ; dword
sstp.ddv:	resw	1 ; word
sstp.ddu:	resw	1 ; word
sstp.vv2:	resw	1 ; word
sstp.uu2:	resw	1 ; word
sstp.vv1:	resw	1 ; word
sstp.uu1:	resw	1 ; word
sstp.r:		resw	1 ; word
sstp.l:		resw	1 ; word
sstp.y:		resw	1 ; word
sstp.maxy:	resw	1 ; word
sstp.miny:	resw	1 ; word
sstp.mx2:	resw	1 ; word
sstp.mx1:	resw	1 ; word
sstp.del:	resb	edges.size * 3 ; 60 bytes 
sstp.edg:	resb	edges.size * 3 ; 60 bytes

alignb 4

songdata:	resb	3605

alignb 4

bss_end: