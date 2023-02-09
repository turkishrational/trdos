; ****************************************************************************
; sinus7.s - TRDOS 386 (TRDOS v2.0.3) Test Program - 'sysvideo' pixel tests
; ----------------------------------------------------------------------------
;
; 19/02/2021
;
; ****************************************************************************
; nasm sinus7.s -l sinus7.txt -o SINUS7.PRG -Z error.txt
; (modified from 'sinus6.s', 19/02/2021)

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


	; Set Video Mode to 105h ; 1024x768, 256 colors
	sys	_video, 08FFh, 105h
	or	eax, eax
	;jz	short terminate
	;mov	[LFB_ADDR], edx ; pointer to LFB info table/structure
	jnz	short set_vesa_mode_105h_ok

terminate:
	call	set_text_mode
	sys	_exit
halt:
	jmp	short halt

set_vesa_mode_105h_ok:
	mov	byte [color], 28 ; initial pixel color
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
	
	inc	byte [color]
	jmp	short _0
_1:
	cmp	al, '-'
	jne	short _2

	dec	byte [color]
	jmp	short _0
_2:
	cmp	al, 20h  ; space
	jne	short _3
	add	byte [color], 8 	
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
	add	byte [color], 4
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
	; x = 0 to 719
	; y = +180 to -180
	; +180 --> 359 -> screen row position = (360-511)+203 = 204
	; -180 --> 0 -> screen row position = (360-0)+203 = 563

	mov	esi, sinustable
	mov	edi, _fx
	xor	eax, eax
	sub	ebx, ebx ; 0 ; x 
	xor	ebp, ebp ; pixel count
	lodsw
	jmp	short _dsw_5
_dsw_0:
	lodsw	; ax = 360-y value
	;
	cmp	ax, cx ; [prevy]
	je	short _dsw_5
	jb	short _dsw_3
_dsw_1:
	inc	cx ; previous 360-y
	cmp	ax, cx
	jna	short _dsw_5
	; ebx = x
	; eax = 360 - y
	call	_dsw_4
	jmp	short _dsw_1
_dsw_2:
	push	eax
	call	getpixeloffset
	stosd
	pop	eax
	inc	ebp ; increase pixel count
	retn
_dsw_3:
	dec	cx ; previous 360-y
	cmp	ax, cx
	jnb	short _dsw_5
	; ebx = x
	; eax = 360 - y
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
	mov	ecx, eax ; previous 360-y value
	call	_dsw_2
	inc	ebx
	cmp	ebx, 720
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
	; eax = 360 - y position
	mov	edx, 360+203 ; 563
	sub	edx, eax ; convert row position from 360-y
	; row = (360-y)+203
	mov	eax, 1024 ; screen width
	mul	edx
 	add	eax, ebx ; add x to y*1024
	add	eax, ((1024-720)/2)
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
	db "by Erdogan Tan - 19/02/2021"
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

sinustable: ; sine wave table (x=0 to 719, y= +180 to -180)
	; 19/02/2021
	; https://daycounter.com/Calculators/Sine-Generator-Calculator2.phtml
	; 720x360 (x= 0 to 719, y = 0 to 359)
	dw 180,181,183,184,186,187,189,190,192,194,195,197,198,200,201,203,204,206,208,209,211,212,214,215,217,218,220,221,223,224,226,227
	dw 229,230,232,233,235,236,238,239,241,242,244,245,247,248,250,251,253,254,255,257,258,260,261,262,264,265,267,268,269,271,272,273
	dw 275,276,277,279,280,281,282,284,285,286,288,289,290,291,292,294,295,296,297,298,300,301,302,303,304,305,306,308,309,310,311,312
	dw 313,314,315,316,317,318,319,320,321,322,323,324,325,326,327,327,328,329,330,331,332,333,333,334,335,336,336,337,338,339,339,340
	dw 341,342,342,343,343,344,345,345,346,347,347,348,348,349,349,350,350,351,351,352,352,352,353,353,354,354,354,355,355,355,356,356
	dw 356,357,357,357,357,357,358,358,358,358,358,358,359,359,359,359,359,359,359,359,359,359,359,359,359,359,359,359,359,358,358,358
	dw 358,358,358,357,357,357,357,357,356,356,356,355,355,355,354,354,354,353,353,352,352,352,351,351,350,350,349,349,348,348,347,347
	dw 346,345,345,344,343,343,342,342,341,340,339,339,338,337,336,336,335,334,333,333,332,331,330,329,328,327,327,326,325,324,323,322
	dw 321,320,319,318,317,316,315,314,313,312,311,310,309,308,306,305,304,303,302,301,300,298,297,296,295,294,292,291,290,289,288,286
	dw 285,284,282,281,280,279,277,276,275,273,272,271,269,268,267,265,264,262,261,260,258,257,255,254,253,251,250,248,247,245,244,242
	dw 241,239,238,236,235,233,232,230,229,227,226,224,223,221,220,218,217,215,214,212,211,209,208,206,204,203,201,200,198,197,195,194
	dw 192,190,189,187,186,184,183,181,180,178,176,175,173,172,170,169,167,165,164,162,161,159,158,156,155,153,151,150,148,147,145,144
	dw 142,141,139,138,136,135,133,132,130,129,127,126,124,123,121,120,118,117,115,114,112,111,109,108,106,105,104,102,101,99,98,97
	dw 95,94,92,91,90,88,87,86,84,83,82,80,79,78,77,75,74,73,71,70,69,68,67,65,64,63,62,61,59,58,57,56
	dw 55,54,53,51,50,49,48,47,46,45,44,43,42,41,40,39,38,37,36,35,34,33,32,32,31,30,29,28,27,26,26,25
	dw 24,23,23,22,21,20,20,19,18,17,17,16,16,15,14,14,13,12,12,11,11,10,10,9,9,8,8,7,7,7,6,6
	dw 5,5,5,4,4,4,3,3,3,2,2,2,2,2,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0
	dw 0,0,0,0,0,1,1,1,1,1,1,2,2,2,2,2,3,3,3,4,4,4,5,5,5,6,6,7,7,7,8,8
	dw 9,9,10,10,11,11,12,12,13,14,14,15,16,16,17,17,18,19,20,20,21,22,23,23,24,25,26,26,27,28,29,30
	dw 31,32,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,53,54,55,56,57,58,59,61,62,63
	dw 64,65,67,68,69,70,71,73,74,75,77,78,79,80,82,83,84,86,87,88,90,91,92,94,95,97,98,99,101,102,104,105
	dw 106,108,109,111,112,114,115,117,118,120,121,123,124,126,127,129,130,132,133,135,136,138,139,141,142,144,145,147,148,150,151,153
	dw 155,156,158,159,161,162,164,165,167,169,170,172,173,175,176,178,180,

bss:

ABSOLUTE bss

alignb 4

color:	resd 1
prevx:	resd 1
;prevy:	resd 1

bss_start:
_fx:	resd 720 ; for every X values from 0 to 719
	resd 2880-720 ; used for repetitive x values for continuity
bss_end: