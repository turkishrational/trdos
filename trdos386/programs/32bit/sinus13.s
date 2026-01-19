; ****************************************************************************
; sinus13.s - TRDOS 386 (TRDOS v2.0.3) Test Program - 'sysvideo' pixel tests
; ----------------------------------------------------------------------------
;
; 07/03/2021
;
; ****************************************************************************
; nasm sinus13.s -l sinus13.txt -o SINUS13.PRG -Z error.txt
; (modified from 'sinus12.s', 07/03/2021)

; Draw sinus wave/curve by using 'sysvideo' bx=0305h
; (640*480, 32 bit true color version)

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

; Retro UNIX 386 v1 system call format:
; sys systemcall (eax) <arg1 (ebx)>, <arg2 (ecx)>, <arg3 (edx)>

[BITS 32] ; We need 32-bit intructions for protected mode

[ORG 0] 

START_CODE:
	; clear bss
	mov	edi, bss_start
	mov	ecx, (bss_end - bss_start)/4
	;xor	eax, eax
	rep	stosd

	; program message
	mov	esi, program_msg
	call	print_msg

	xor	ah, ah
	;int	16h	; KEYBOARD - READ CHAR FROM BUFFER, WAIT IF EMPTY
			; Return: AH = scan code, AL = character
	int	32h	; TRDOS 386 Keyboard interrupt 

	; Get VESA VBE video bios number 
	;	(vbe2 or vbe3, emulator or not)  
	;mov	ebx, 0906h
	;sys	_video
	
	; get [truecolor] status (24bpp or 32bpp for VBE3 vbios)
	mov	ebx, 0909h
	sys	_video

	mov	ecx, 112h ; VESA VBE video mode	(640x480, 24bpp)
	
	;cmp	ah, 3
	;je	short set_vmode
	;cmp	ah, 2
	;;jne	terminate ; invalid !?
	;jne	short set_vmode
	;and	al, al
	;jz	short set_vmode ; invalid !? (AL must be >= C0h.)
	
	cmp	al, 32
	je	short set_vmode
	
	; VBE2 bios or default truecolor bpp is 24 bpp	
	; so, we are using video mode 142h 
	; (TRDOS 386 running in emulator or very old hardware!)
	
	; BOCHS/QEMU/VBOX emulator 
	mov	cl, 42h ; Bochs/Plex86 video mode 142h
			; (640*480, 32bpp) 		

	; Set Video Mode to 112h ; 640x480, 32 bit true colors
	;
	; NOTE: NVIDIA GEFORCE FX 5500 VIDEO BIOS uses 32bpp 
	;       instead of 24bpp for same VBE video mode numbers. 
	;       So, 112h is 640*480, 24bpp for BOCHS/QEMU emulator
	;	but, it is 640*480, 32 bpp for real computer 
	;	with NVIDIA graphics card and video bios. 
	; (Also it is -it must be- 32bpp for other new hardware.)	  	

	;sys	_video, 08FFh, 112h
set_vmode:
	; ecx = VESA VBE video mode
	sys	_video, 08FFh
	or	eax, eax
	;jz	short terminate
	;mov	[LFB_ADDR], edx ; pointer to LFB info table/structure
	jnz	short set_vesa_mode_112h_ok

terminate:
	call	set_text_mode
	sys	_exit
halt:
	jmp	short halt

set_vesa_mode_112h_ok:
	mov	dword [color], 0FF0000h ; initial pixel color
_0:
	call	drawsinewave
waitforkey:
	;mov	ah, 1
	;int	32h
	;jz	short getkey
	;inc	word [counter]
	;nop
	;nop
	;nop
	;jmp	short waitforkey
getkey:
	xor	ah, ah
	int	32h

	cmp	ax, 2E03h
	je	short terminate
	cmp	al, 1Bh ; ESC key
	je	short terminate	

	cmp	al, '+'
	jne	short _1
	
	add	dword [color], 20h
	jmp	short _0
_1:
	cmp	al, '-'
	jne	short _2

	sub	dword [color], 20h
	jmp	short _0
_2:
	cmp	al, 20h  ; space
	jne	short _3
	add	dword [color], 2020h	
	jmp	short _0
_3:
	cmp	ah, 4Bh
	jne	short _5
	; left arrow
_4:
	call	beep
	jmp	waitforkey
_5:
	cmp	ah, 4Dh
	jne	short _6

	; right arrow
	jmp	short _4
_6:
	cmp	ah, 50h
	jne	short _7
	; down arrow
	jmp	short _4
_7:
	cmp	ah, 48h
	jne	short _8
	; up arrow
	jmp	short _4
_8:	
	cmp	ax, 1C0Dh
	jne	short _9
	call	beep
	add	dword [color], 1010h
	jmp	short _0
_9:	
	call	beep
	jmp	waitforkey

print_msg:
	mov	ah, 0Eh
	mov	ebx, 7
	;mov	bl, 7 ; char attribute & color
p_next_chr:
	lodsb
	or	al, al
	jz	short p_retn ; retn	
	int	31h
	jmp	short p_next_chr
p_retn:
	retn

drawsinewave:
	; INPUT:
	;	sinustable
	;
	; Modified registers: esi, edi, eax, ecx, ebx, edx

	; fill _fx table by using sine wave table
	; x = 0 to 639
	; y = +200 to -200
	; +200 --> 399 -> screen row position = (400-399)+39 = 40
	; -200 --> 0 -> screen row position = (400-0)+39 = 439

	mov	esi, sinustable
	mov	edi, _fx
	;xor	eax, eax
	sub	ebx, ebx ; 0 ; x 
	xor	ebp, ebp ; pixel count

	;xor	eax, eax	
	;lodsw	; ax = 400-y value 

;	;;;; start of curve continuity code
;
;	lodsw
;	jmp	short _dsw_5
_dsw_0:
	xor	eax, eax	
	lodsw	; ax = 400-y value  ; *** 
;	;
;	cmp	ax, cx ; [prevy]
;	je	short _dsw_5
;	jb	short _dsw_3
;_dsw_1:
;	inc	cx ; previous 400-y
;	cmp	ax, cx
;	jna	short _dsw_5
;	; ebx = x
;	; eax = 400 - y
;	call	_dsw_4
;	jmp	short _dsw_1
;_dsw_2:
;	push	eax
;	call	getpixeloffset
;	stosd
;	pop	eax
;	inc	ebp ; increase pixel count
;	retn
;_dsw_3:
;	dec	cx ; previous 400-y
;	cmp	ax, cx
;	jnb	short _dsw_5
;	; ebx = x
;	; eax = 400 - y
;	call	_dsw_4
;	jmp	short _dsw_3
;_dsw_4:
;	push	ebx
;	mov	ebx, [prevx]
;	xchg	eax, ecx ; [prevy]
;	call	_dsw_2
;	xchg	ecx, eax
;	pop	ebx
;	inc	dword [prevx]
;	retn
;_dsw_5:
;	mov	[prevx], ebx ; previous x value 
;	mov	ecx, eax ; previous 400-y value
;	call	_dsw_2
;
;	;;;; end of curve continuity code

	call	getpixeloffset
	stosd
	inc	ebp ; increase pixel count	
	
	inc	ebx
	cmp	ebx, 640
	jb	short _dsw_0

	mov	esi, _fx
	;mov	edx, ebp
	;
	; edx = pixel count
	; esi = user's single color pixel buffer address
	sys	_video, 0305h, [color], ebp

	retn
	
getpixeloffset:
	; ebx = x position
	; eax = 400 - y position
	mov	edx, 439
	sub	edx, eax ; convert row position from 400-y
	; row = (400-y)+39
	mov	eax, 640*4 ; screen width
	mul	edx
	push	ebx
	shl	ebx, 2
 	add	eax, ebx ; add x to y*640
	pop	ebx
	; eax = pixel offset on display page
	retn
beep:
	; call beep function (16/64 second, 886Hz)
	sys	_audio, 16, 1331
	retn

set_text_mode:
	xor    ah, ah
	mov    al, 3                        
 	;int   10h ; al = 03h text mode, int 10 video
	int    31h ; TRDOS 386 - Video interrupt
	retn
		
program_msg:
	db "TRDOS 386 v2.0.3 - ('sysvideo') Test Program - Draw Sine Wave"
	db 0Dh, 0Ah
	db "by Erdogan Tan - 07/03/2021"
	;db 0Dh, 0Ah, 0
	db 0Dh, 0Ah, 0Dh, 0Ah

	db "Use SPACE,ENTER,'+','-' keys to change COLOR .."		
	db 0Dh, 0Ah
	db "Press ESC to exit .."
	db 0Dh, 0Ah
	db 0Dh, 0Ah
	db "Press any key to continue .."
nextline:
	db 0Dh, 0Ah, 0

sinustable: ; sine wave table (x=0 to 639, y= +200 to -200)
	; 19/02/2021
	; https://daycounter.com/Calculators/Sine-Generator-Calculator2.phtml
	; 640x400 (x= 0 to 639, y = 0 to 399)
	dw 200,201,203,205,207,209,211,213,215,217,219,221,223,225,227,229
	dw 231,233,235,236,238,240,242,244,246,248,250,252,254,256,257,259
	dw 261,263,265,267,269,270,272,274,276,278,279,281,283,285,287,288
	dw 290,292,294,295,297,299,300,302,304,305,307,309,310,312,314,315
	dw 317,318,320,321,323,325,326,328,329,331,332,333,335,336,338,339
	dw 341,342,343,345,346,347,349,350,351,352,354,355,356,357,359,360
	dw 361,362,363,364,365,366,368,369,370,371,372,373,374,375,375,376
	dw 377,378,379,380,381,381,382,383,384,385,385,386,387,387,388,389
	dw 389,390,390,391,392,392,393,393,393,394,394,395,395,396,396,396
	dw 397,397,397,397,398,398,398,398,398,399,399,399,399,399,399,399
	dw 399,399,399,399,399,399,399,399,398,398,398,398,398,397,397,397
	dw 397,396,396,396,395,395,394,394,393,393,393,392,392,391,390,390
	dw 389,389,388,387,387,386,385,385,384,383,382,381,381,380,379,378
	dw 377,376,375,375,374,373,372,371,370,369,368,366,365,364,363,362
	dw 361,360,359,357,356,355,354,352,351,350,349,347,346,345,343,342
	dw 341,339,338,336,335,333,332,331,329,328,326,325,323,321,320,318
	dw 317,315,314,312,310,309,307,305,304,302,300,299,297,295,294,292
	dw 290,288,287,285,283,281,279,278,276,274,272,270,269,267,265,263
	dw 261,259,257,256,254,252,250,248,246,244,242,240,238,236,235,233
	dw 231,229,227,225,223,221,219,217,215,213,211,209,207,205,203,201
	dw 200,198,196,194,192,190,188,186,184,182,180,178,176,174,172,170
	dw 168,166,164,163,161,159,157,155,153,151,149,147,145,143,142,140
	dw 138,136,134,132,130,129,127,125,123,121,120,118,116,114,112,111
	dw 109,107,105,104,102,100,99,97,95,94,92,90,89,87,85,84
	dw 82,81,79,78,76,74,73,71,70,68,67,66,64,63,61,60
	dw 58,57,56,54,53,52,50,49,48,47,45,44,43,42,40,39
	dw 38,37,36,35,34,33,31,30,29,28,27,26,25,24,24,23
	dw 22,21,20,19,18,18,17,16,15,14,14,13,12,12,11,10
	dw 10,9,9,8,7,7,6,6,6,5,5,4,4,3,3,3
	dw 2,2,2,2,1,1,1,1,1,0,0,0,0,0,0,0
	dw 0,0,0,0,0,0,0,0,1,1,1,1,1,2,2,2
	dw 2,3,3,3,4,4,5,5,6,6,6,7,7,8,9,9
	dw 10,10,11,12,12,13,14,14,15,16,17,18,18,19,20,21
	dw 22,23,24,24,25,26,27,28,29,30,31,33,34,35,36,37
	dw 38,39,40,42,43,44,45,47,48,49,50,52,53,54,56,57
	dw 58,60,61,63,64,66,67,68,70,71,73,74,76,78,79,81
	dw 82,84,85,87,89,90,92,94,95,97,99,100,102,104,105,107
	dw 109,111,112,114,116,118,120,121,123,125,127,129,130,132,134,136
	dw 138,140,142,143,145,147,149,151,153,155,157,159,161,163,164,166
	dw 168,170,172,174,176,178,180,182,184,186,188,190,192,194,196,198

bss:

ABSOLUTE bss

alignb 4

color:	resd 1
prevx:	resd 1
;prevy:	resd 1

bss_start:
_fx:	resd 640 ; for every X values from 0 to 639
	resd 2048-640 ; used for repetitive x values for curve continuity
bss_end: