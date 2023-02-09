; ****************************************************************************
; sinus11.s - TRDOS 386 (TRDOS v2.0.3) Test Program - 'sysvideo' pixel tests
; ----------------------------------------------------------------------------
;
; 06/03/2021
;
; ****************************************************************************
; nasm sinus11.s -l sinus11.txt -o SINUS11.PRG -Z error.txt
; (modified from 'sinus9.s', 11/03/2021)

; Draw sinus wave/curve by using 'sysvideo' bx=0305h

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

	mov	ecx, 118h ; VESA VBE video mode	(1024x768, 24bpp)
	
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
	; so, we are using video mode 144h 
	; (TRDOS 386 running in emulator or very old hardware!)
	
	; BOCHS/QEMU/VBOX emulator 
	mov	cl, 44h ; Bochs/Plex86 video mode 144h
			; (1024*768, 32bpp) 		

	; Set Video Mode to 118h ; 1024*768, 32 bit true colors
	;
	; NOTE: NVIDIA GEFORCE FX 550 VIDEO BIOS uses 32bpp 
	;       instead of 24bpp for same VBE video mode numbers. 
	;       So, 118h is 1024*768, 24bpp for BOCHS/QEMU emulator
	;	but, it is 1024*768, 32 bpp for real computer 
	;	with NVIDIA graphics card and video bios. 
	; (Also it is -it must be- 32bpp for other new hardware.)	  	

	;sys	_video, 08FFh, 118h
set_vmode:
	; ecx = VESA VBE video mode
	sys	_video, 08FFh
	or	eax, eax
	;jz	short terminate
	;mov	[LFB_ADDR], edx ; pointer to LFB info table/structure
	jnz	short set_vesa_mode_118h_ok

terminate:
	call	set_text_mode
	call	write_pixel_count
	sys	_exit
halt:
	jmp	short halt

set_vesa_mode_118h_ok:
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
	; x = 0 to 1023
	; y = +256 to -256
	; +256 --> 511 -> screen row position = (512-511)+127 = 128
	; -256 --> 0 -> screen row position = (512-0)+127 = 639

	mov	esi, sinustable
	mov	edi, _fx
	xor	eax, eax
	sub	ebx, ebx ; 0 ; x 
	xor	ebp, ebp ; pixel count
	lodsw
	jmp	short _dsw_5
_dsw_0:
	lodsw	; ax = 512-y value
	;
	cmp	ax, cx ; [prevy]
	je	short _dsw_5
	jb	short _dsw_3
_dsw_1:
	inc	cx ; previous 512-y
	cmp	ax, cx
	jna	short _dsw_5
	; ebx = x
	; eax = 512 - y
	call	_dsw_4
	jmp	short _dsw_1
_dsw_2:
	push	eax
	call	getpixeloffset
	stosd

	; temporary - 06/03/2021
	;cmp eax, [pcount] ; max. value of pixel offset
	;jna short _dsw2_0
	;mov [pcount], eax

;_dsw2_0:	
	pop	eax
	inc	ebp ; increase pixel count
	retn
_dsw_3:
	dec	cx ; previous 512-y
	cmp	ax, cx
	jnb	short _dsw_5
	; ebx = x
	; eax = 512 - y
	call	_dsw_4
	jmp	short _dsw_3
_dsw_4:
	push	ebx
	mov	ebx, [prevx]
	xchg	eax, ecx ; [prevy]
	call	_dsw_2
	xchg	ecx, eax
	pop	ebx
	inc	dword [prevx]
	retn
_dsw_5:
	mov	[prevx], ebx ; previous x value 
	mov	ecx, eax ; previous 512-y value
	call	_dsw_2
	inc	ebx
	cmp	ebx, 1024
	jb	short _dsw_0

	; pixel count to be written
	mov	[pcount], ebp ; 06/03/2021

	mov	esi, _fx
	;mov	edx, ebp
	;
	; edx = pixel count
	; esi = user's single color pixel buffer address
	sys	_video, 0305h, [color], ebp

	retn
	
getpixeloffset:
	; 32 bit true color pixel offset
	; ebx = x position
	; eax = 512-y position (<= 512)
	; Note: 06/03/2021 modifcation is for QEMU (Bochs) 32bpp
	;	bug check for vesa vbe mode 144h (1024*768, 32bpp)
	;	(display page size: 2359296 or 3145728 ?)
	mov	edx, 512+63 ; 575 ; 06/03/2021 (sinus11.s) ; 575*1024*3
	;mov	edx, 512+127 ; 639 ; 02/03/2021 (sinus9.s) ; 639*1024*4
	sub	edx, eax ; convert row position from 512-y
	; row = y+127
	mov	eax, 1024*4 ; screen width
	mul	edx
	push	ebx
	shl	ebx, 2
 	add	eax, ebx ; add x to y*1024
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

write_pixel_count:
	; 06/03/2021
	mov	eax, [pcount]
	or	eax, eax
	jz	short wpc_ok
	mov	edi, pixcstr
	mov	ecx, 10
	mov	ebp, esp
wpc_0:
	xor	edx, edx
	div	ecx
	push	edx
	and	eax, eax
	jnz	short wpc_0
wpc_1:	
	pop	eax
	add	al, '0'
	stosb
	cmp	ebp, esp
	ja	short wpc_1
	
	mov	esi, wpixels_msg
	call	print_msg
	mov	esi, nextline
	jmp	print_msg
wpc_ok:
	retn
		
program_msg:
	db "TRDOS 386 v2.0.3 - ('sysvideo') Test Program - Draw Sine Wave"
	db 0Dh, 0Ah
	db "by Erdogan Tan - 06/03/2021"
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

wpixels_msg:
	db 0Dh, 0Ah
	db "Written pixels : "
pixcstr:
	db "0"
	db 0,0,0,0,0,0
	db 0

sinustable: ; sine wave table (x=0 to 1023, y= +256 to -256)
	; 19/02/2021
	; https://daycounter.com/Calculators/Sine-Generator-Calculator2.phtml
	; 1024x512 (x= 0 to 1023, y = 0 to 511)
	dw 256,257,259,260,262,263,265,266,268,270,271,273,274,276,277,279,281,282,284,285,287,288,290,291,293,295,296,298,299,301,302,304
	dw 305,307,308,310,311,313,315,316,318,319,321,322,324,325,327,328,330,331,333,334,336,337,339,340,342,343,345,346,347,349,350,352
	dw 353,355,356,358,359,360,362,363,365,366,368,369,370,372,373,375,376,377,379,380,381,383,384,386,387,388,390,391,392,394,395,396
	dw 397,399,400,401,403,404,405,406,408,409,410,411,413,414,415,416,418,419,420,421,422,424,425,426,427,428,429,431,432,433,434,435
	dw 436,437,438,439,441,442,443,444,445,446,447,448,449,450,451,452,453,454,455,456,457,458,459,460,461,462,463,463,464,465,466,467
	dw 468,469,470,471,471,472,473,474,475,475,476,477,478,479,479,480,481,482,482,483,484,484,485,486,486,487,488,488,489,490,490,491
	dw 492,492,493,493,494,494,495,496,496,497,497,498,498,499,499,500,500,500,501,501,502,502,503,503,503,504,504,504,505,505,505,506
	dw 506,506,507,507,507,508,508,508,508,508,509,509,509,509,509,510,510,510,510,510,510,510,511,511,511,511,511,511,511,511,511,511
	dw 511,511,511,511,511,511,511,511,511,511,511,510,510,510,510,510,510,510,509,509,509,509,509,508,508,508,508,508,507,507,507,506
	dw 506,506,505,505,505,504,504,504,503,503,503,502,502,501,501,500,500,500,499,499,498,498,497,497,496,496,495,494,494,493,493,492
	dw 492,491,490,490,489,488,488,487,486,486,485,484,484,483,482,482,481,480,479,479,478,477,476,475,475,474,473,472,471,471,470,469
	dw 468,467,466,465,464,463,463,462,461,460,459,458,457,456,455,454,453,452,451,450,449,448,447,446,445,444,443,442,441,439,438,437
	dw 436,435,434,433,432,431,429,428,427,426,425,424,422,421,420,419,418,416,415,414,413,411,410,409,408,406,405,404,403,401,400,399
	dw 397,396,395,394,392,391,390,388,387,386,384,383,381,380,379,377,376,375,373,372,370,369,368,366,365,363,362,360,359,358,356,355
	dw 353,352,350,349,347,346,345,343,342,340,339,337,336,334,333,331,330,328,327,325,324,322,321,319,318,316,315,313,311,310,308,307
	dw 305,304,302,301,299,298,296,295,293,291,290,288,287,285,284,282,281,279,277,276,274,273,271,270,268,266,265,263,262,260,259,257
	dw 256,254,252,251,249,248,246,245,243,241,240,238,237,235,234,232,230,229,227,226,224,223,221,220,218,216,215,213,212,210,209,207
	dw 206,204,203,201,200,198,196,195,193,192,190,189,187,186,184,183,181,180,178,177,175,174,172,171,169,168,166,165,164,162,161,159
	dw 158,156,155,153,152,151,149,148,146,145,143,142,141,139,138,136,135,134,132,131,130,128,127,125,124,123,121,120,119,117,116,115
	dw 114,112,111,110,108,107,106,105,103,102,101,100,98,97,96,95,93,92,91,90,89,87,86,85,84,83,82,80,79,78,77,76
	dw 75,74,73,72,70,69,68,67,66,65,64,63,62,61,60,59,58,57,56,55,54,53,52,51,50,49,48,48,47,46,45,44
	dw 43,42,41,40,40,39,38,37,36,36,35,34,33,32,32,31,30,29,29,28,27,27,26,25,25,24,23,23,22,21,21,20
	dw 19,19,18,18,17,17,16,15,15,14,14,13,13,12,12,11,11,11,10,10,9,9,8,8,8,7,7,7,6,6,6,5
	dw 5,5,4,4,4,3,3,3,3,3,2,2,2,2,2,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0
	dw 0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,5
	dw 5,5,6,6,6,7,7,7,8,8,8,9,9,10,10,11,11,11,12,12,13,13,14,14,15,15,16,17,17,18,18,19
	dw 19,20,21,21,22,23,23,24,25,25,26,27,27,28,29,29,30,31,32,32,33,34,35,36,36,37,38,39,40,40,41,42
	dw 43,44,45,46,47,48,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,72,73,74
	dw 75,76,77,78,79,80,82,83,84,85,86,87,89,90,91,92,93,95,96,97,98,100,101,102,103,105,106,107,108,110,111,112
	dw 114,115,116,117,119,120,121,123,124,125,127,128,130,131,132,134,135,136,138,139,141,142,143,145,146,148,149,151,152,153,155,156
	dw 158,159,161,162,164,165,166,168,169,171,172,174,175,177,178,180,181,183,184,186,187,189,190,192,193,195,196,198,200,201,203,204
	dw 206,207,209,210,212,213,215,216,218,220,221,223,224,226,227,229,230,232,234,235,237,238,240,241,243,245,246,248,249,251,252,254

bss:

ABSOLUTE bss

alignb 4

color:	resd 1
prevx:	resd 1
;prevy:	resd 1
pcount:	resd 1 ; 06/03/2021

bss_start:
_fx:	resd 1024 ; for every X values from 0 to 1023
	resd 4096-1024 ; used for repetitive x values for continuity
bss_end: