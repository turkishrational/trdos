; ****************************************************************************
; line8.s - TRDOS 386 (TRDOS v2.0.3) Test Program - 'sysvideo' pixel tests
; ----------------------------------------------------------------------------
;
; 20/02/2021
;
; ****************************************************************************
; nasm line8.s -l line8.txt -o LINE8.PRG -Z error.txt
; (modified from 'line7.s', 20/02/2021)

; Draw line by using 'sysvideo' bx=0305h

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

	;; Set Video Mode to 13h
	;sys	_video, 0813h
	;cmp	eax, 14h 
	;je	short mode_13h_set_ok

	; set VGA mode by using int 31h
	mov	ax, 13h	; mode 13h ; 
	int	31h	; real mode: int 10h
	;jmp	short mode_13h_set_ok

mode_13h_set_ok:
	; Set inital values for angle and hypotenuse
	mov	eax, 45
	mov	byte [angle], 45
	mov	al, 142 
	;mov	[hipotenus], al ; 142
	; Set start point of line(s)   
	mov	byte [_x0], 160
	mov	byte [_y0], 150
	; Set initial color
 	mov	byte [color], 28
_new_line_1:
	mov	[hipotenus], al
_new_line_2:
	mov	al, [angle]
_new_line_3:
	; al = angle	
	call	getcosinus ; get cosine value * 16777216 for angle in AL
	; eax= cos(angle) * 16777216 
	call	getxy	
	mov	[_x1], eax ; projection of end point on x-axis
	mov	al, [angle] 
	call	getsinus ; get sine value * 16777216 for angle in AL
	; eax= sin(angle) * 16777216 
	call	getxy		
	mov	[_y1], eax ; projection of end point on y-axis
_0:
	call	drawline
waitforkey:
	mov	ah, 1
	int	32h
	jz	short getkey
	inc	byte [counter]
	nop
	nop
	nop
	jmp	short waitforkey

terminate:
	call	set_text_mode
	sys	_exit
halt:
	jmp	short halt

getkey:
	xor	ah, ah
	int	32h

	cmp	ax, 2E03h
	je	short terminate
	cmp	al, 1Bh ; ESC key
	je	short terminate	

	cmp	al, '+'
	jne	short _1
	
	mov	al, [hipotenus]

	cmp	al, 142
	jnb	short _3_  ; beep
	
	; delete line by drawing black line
	; with same length and with same x,y position
	call	blackline
	; increase length of the line
	inc	al	
	jmp	short _new_line_1 ; draw with new length
_1:
	cmp	al, '-'
	jne	short _2

	mov	al, [hipotenus]

	cmp	al, 3
	jna	short _3_ ; beep
	
	; delete line by drawing black line
	; with same length and with same x,y position
	call	blackline
	; decrease length of the line
	dec	al	
	jmp	_new_line_1 ; draw with new length
_2:
	cmp	al, 20h  ; space
	jne	short _3
	add	byte [color], 8 	
	jmp	short _0
_3:
	cmp	ah, 4Bh
	jne	short _4
	; left arrow
	cmp	byte [angle], 180
	jnb	short _3
	call	blackline ; clear current position 
	inc	byte [angle]
	jmp	_new_line_2
_3_:
	call	beep
	jmp	waitforkey
_4:
	cmp	ah, 4Dh
	jne	short _5
	; right arrow
	cmp	byte [angle], 0
	jna	short _3
	call	blackline ; clear current position 
	dec	byte [angle]
	jmp	_new_line_2
_5:
	cmp	ah, 50h
	jne	short _7
	; down arrow
	mov	al, [angle]
	sub	al, 5
	ja	short _6
	call	beep
	xor	al, al ; 0
_6:
	call	blackline ; clear current position 
	mov	[angle], al
	jmp	_new_line_3
_7:
	cmp	ah, 48h
	jne	short _8
	; up arrow
	mov	al, [angle]
	add	al, 5
	cmp	al, 180
	jb	short _6
	call	beep
	mov	al, 180
	jmp	short _6
_8:
	cmp	ah, 47h ; Home key
	jne	short _9
	sub	al, al ; 0 ; set angle to 0 degree
	jmp	short _6
_9:
	cmp	ah, 4Fh ; End key
	jne	short _10
	mov	al, 180 ; set angle to 180 degrees
	jmp	short _6
_10:	
	cmp	ax, 1C0Dh
	jne	short _11
	add	byte [color], 4
	jmp	short _13
_11:	
	cmp	ah, 53h ; INSERT
	je	short _13
	cmp	ah, 52h  ; DEL
	je	short _13

	cmp	ah, 49h  ; Page UP
	jne	short _12
	dec	byte [color]
	jmp	short _13
_12:
	cmp	ah, 51h  ; Page Down
	jne	short _14
	inc	byte [color]
_13:
	call	beep
	jmp	_0
_14:
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

drawline:
	; INPUT:
	;	[_x0]
	;	[_y0]
	;	[_x1]
	;	[_y1]
	;	[sin]
	;	[cos]
	;	[color]
	;
	; Modified registers: esi, edi, eax, ecx, ebx, edx

_draw_l_0:
	xor	ecx, ecx
	mov	[_x2], ecx ; 0  ; reset
	mov	[_y2], ecx ; 0	; reset
	mov	[pixelcount], ecx ; reset
	mov	al, [_x1]
	cmp	al, [_y1]
	jnb	short _draw_l_x0
	; base axis is y-axis
	jmp	_draw_l_y0
_draw_l_x0:
	; base axis is x-axis
	; draw line from x = 0 to [_x1]
	; set y by using x*([sin]/[cos])
	; cl = 0
	;mov	byte [prevy], cl ; previous Y value
	call	writepixel
	mov	cl, [_y2]
_draw_l_x1:
	mov	eax, [_x2]
	inc	al
	cmp	al, [_x1]
	ja	short _draw_l_x3
	mov	[_x2], al	
	mov	edx, [sin]
	mul	edx
	mov	ebx, [cos]
	div	ebx
	mov	[_y2], eax
	mov	ch, al
	inc	cl
	cmp	cl, ch ; previous Y+1 < current Y ?
	;jnb	short _draw_l_x0
	jmp	short _draw_l_x0
	dec	byte [_x2]
_draw_l_x2:
	mov	[_y2], cl
	call	writepixel
	inc	cl
	cmp	cl, ch ; loop end for [_y2]
	jb	short _draw_l_x2
	mov	[_y2], ch
	inc	byte [_x2]
	jmp	short _draw_l_x0

_draw_l_x3:
_draw_l_y3:
writeline:
	;mov	edx, [pixelcount]
	; edx = pixel count
	; esi = user's single color pixel buffer address
	mov	esi, _fx 
	sys	_video, 0305h, [color], [pixelcount]

	retn
		 	
_draw_l_y0:
	; base axis is y-axis
	; draw line from y = 0 to [_y1]
	; set x by using y*([cos]/[sin])
	; cl = 0

	;mov	byte [prevx], cl ; previous X value
	call	writepixel
	mov	cl, [_x2]	
_draw_l_y1:
	mov	eax, [_y2]
	inc	al
	cmp	al, [_y1]
	ja	short _draw_l_y3
	mov	[_y2], al	
	;mov	edx, [cos]
	;mul	edx
	mul	dword [cos]
	;mov	ebx, [sin]
	;div	ebx
	div	dword [sin]
	mov	[_x2], eax
	mov	ch, al
	inc	cl
	cmp	cl, ch ; previous X+1 < current X ?
	;jnb	short _draw_l_y0
	jmp	short _draw_l_y0
	dec	byte [_y2]
_draw_l_y2:
	mov	[_x2], cl
	call	writepixel
	inc	cl
	cmp	cl, ch ; loop end for [_y2]
	jb	short _draw_l_y2
	mov	[_x2], ch
	inc	byte [_y2]
	jmp	short _draw_l_y0

writepixel:
	; write pixel to line buffer (of user)	
	; Modified registers: eax, ebx, edx
	mov	eax, [_y0] ; 150	
	sub	eax, [_y2] ; max. 142
	mov	edx, 320
	mul	edx
	add	eax, [_x0] ; 160
			; center of the screen 
	cmp	byte [angle], 90
	jna	short wp_1
wp_0:
	sub	eax, [_x2] ; negative direction
	jmp	short wp_2 
wp_1:
	add	eax, [_x2] ; positive direction	
wp_2:
	mov	ebx, [pixelcount]	
	shl	ebx, 2 ; * 4
	add	ebx, _fx ; line buffer
	mov	[ebx], eax
	inc	dword [pixelcount]
	retn

blackline:
	push	eax
	;xor	al, al
	mov	al, [counter] ; color effect for 'line8.s'
	xchg	[color], al ; color = 0	
	mov	[pcolor], al
	inc	byte [counter] ; color effect for 'line8.s'
	call	writeline
	mov	al, [pcolor]
	mov	[color], al ; restore color
	pop	eax
	retn

beep:
	; call beep function (16/64 second, 886Hz)
	sys	_audio, 16, 1331
	retn

getsinus:
	; Input:
	;	AL = angle
	; output:
	;	EAX = sin(angle) * 16777216
	;
	; Modified registers: eax, esi
	;
	movzx	esi, al
	shl	esi, 2 ; * 4
	add	esi, sinustable
	mov	eax, [esi]
	mov	[sin], eax
	retn	

getcosinus:
	; Input:
	;	AL = angle
	; output:
	;	EAX = cos(angle) * 16777216
	;
	; Modified registers: eax, esi
	
	cmp	al, 90 ; 90 degrees
	jb	short gcos_1
	sub	al, 90	
	mov	esi, cos_90
	jmp	short gcos_2
gcos_1:	
	mov	esi, cos_0
gcos_2:
	movzx	eax, al
	shl	eax, 2 ; * 4
	add	esi, eax
	mov	eax, [esi]
	mov	[cos], eax
	retn

getxy:
	; Input:
	;	EAX = sin or cos value * 16777216
	; output:
	;	EAX = x or y projection
	;
	; Modified registers: eax, edx, (ecx)
	;
	
	;mov	edx, [hipotenus]
	;mul	edx
	mul	dword [hipotenus]
	;mov	ecx, 16777216
	;div	ecx
	shr	eax, 24
	retn

set_text_mode:
	xor    ah, ah
	mov    al, 3                        
 	;int   10h ; al = 03h text mode, int 10 video
	int    31h ; TRDOS 386 - Video interrupt
	retn

program_msg:
	db "TRDOS 386 v2.0.3 - ('sysvideo') Test Program - Draw Line (as rotated)"
	db 0Dh, 0Ah
	db "by Erdogan Tan - 20/02/2021"
	;db 0Dh, 0Ah, 0
	db 0Dh, 0Ah, 0Dh, 0Ah

	db "Use Arrow Keys, Home, End to rotate the LINE .."
	db 0Dh, 0Ah
	db "Use +,- keys to increase and decrease LINE LENGTH .."		
	db 0Dh, 0Ah
	db "Use ENTER key to draw LINE with new color .."
	db 0Dh, 0Ah
	db "Use SPACE, Pg Up, Pg Down keys to change COLOR .."
	db 0Dh, 0Ah	
	db "Press ESC to exit .."
	db 0Dh, 0Ah
	db 0Dh, 0Ah
	db "Press any key to continue .."
nextline:
	db 0Dh, 0Ah, 0

sinustable:
	; from A = 0 degree to 180 degrees (sin(A)*65536)
	;dd 0
	;dd 1144, 2287, 3430, 4572, 5712, 6850, 7987, 9121, 10252, 11380, 12505, 13626
	;dd 14742, 15855, 16962, 18064, 19161, 20252, 21336, 22415, 23486, 24550, 25607
	;dd 26656, 27697, 28729, 29753, 30767, 31772, 32768, 33754, 34729, 35693, 36647
	;dd 37590, 38521, 39441, 40348, 41243, 42126, 42995, 43852, 44695, 45525, 46341
	;dd 47143, 47930, 48703, 49461, 50203, 50931, 51643, 52339, 53020, 53684, 54332
	;dd 54963, 55578, 56175, 56756, 57319, 57865, 58393, 58903, 59396, 59870, 60326
	;dd 60764, 61183, 61584, 61966, 62328, 62672, 62997, 63303, 63589, 63856, 64104
	;dd 64332, 64540, 64729, 64898, 65048, 65177, 65287, 65376, 65446, 65496, 65526
	;dd 65536
	;dd 65526, 65496, 65446, 65376, 65287, 65177, 65048, 64898, 64729, 64540, 64332
	;dd 64104, 63856, 63589, 63303, 62997, 62672, 62328, 61966, 61584, 61183, 60764
	;dd 60326, 59870, 59396, 58903, 58393, 57865, 57319, 56756, 56175, 55578, 54963
	;dd 54332, 53684, 53020, 52339, 51643, 50931, 50203, 49461, 48703, 47930, 47143
	;dd 46341, 45525, 44695, 43852, 42995, 42126, 41243, 40348, 39441, 38521, 37590
	;dd 36647, 35693, 34729, 33754, 32768, 31772, 30767, 29753, 28729, 27697, 26656
	;dd 25607, 24550, 23486, 22415, 21336, 20252, 19161, 18064, 16962, 15855, 14742
	;dd 13626, 12505, 11380, 10252, 9121, 7987, 6850, 5712, 4572, 3430, 2287, 1144
	;dd 0
	; from A = 0 degree to 180 degrees (sin(A)*65536*256)
	; 0º
cos_90:	dd 0
	dd 292803, 585516, 878052, 1170319, 1462231, 1753697, 2044628, 2334937, 2624535, 2913333
	dd 3201244, 3488179, 3774052, 4058776, 4342263, 4624427, 4905183, 5184445, 5462127, 5738146
	dd 6012416, 6284856, 6555381, 6823909, 7090358, 7354647, 7616697, 7876426, 8133756
cos_60:	; 30º
	dd 8388608, 8640905, 8890570, 9137527, 9381700, 9623016, 9861400, 10096781, 10329086, 10558244
	dd 10784187, 11006844, 11226149, 11442034, 11654434
sin_45:	; 45º
cos_45:	dd 11863283, 12068519, 12270079, 12467901, 12661926, 12852093, 13038346, 13220627, 13398880, 13573053
	dd 13743091, 13908942, 14070557, 14227886, 14380881
cos_30:	; 60º
	dd 14529495, 14673684, 14813402, 14948609, 15079262, 15205322, 15326749, 15443509, 15555564, 15662880
	dd 15765426, 15863169, 15956081, 16044131, 16127295, 16205546, 16278861, 16347217, 16410594, 16468971
	dd 16522332, 16570661, 16613941, 16652161, 16685309, 16713374, 16736348, 16754223, 16766996, 16774661
sin_90:	; 90º
cos_0:	dd 16777216, 16774661, 16766996, 16754223, 16736348, 16713374, 16685309, 16652161, 16613941, 16570661
	dd 16522332, 16468971, 16410594, 16347217, 16278861, 16205546, 16127295, 16044131, 15956081, 15863169
	dd 15765426, 15662880, 15555564, 15443509, 15326749, 15205322, 15079262, 14948609, 14813402, 14673684
	; 120º
	dd 14529495, 14380881, 14227886, 14070557, 13908942, 13743091, 13573053, 13398880, 13220627, 13038346
	dd 12852093, 12661926, 12467901, 12270079, 12068519
	; 135º
	dd 11863283, 11654434, 11442034, 11226149, 11006844, 10784187, 10558244, 10329086, 10096781, 9861400
	dd 9623016, 9381700, 9137527, 8890570, 8640905
	; 150º
	dd 8388608, 8133756, 7876426, 7616697, 7354647, 7090358, 6823909, 6555381, 6284856, 6012416, 5738146
	dd 5462127, 5184445, 4905183, 4624427, 4342263, 4058776, 3774052, 3488179, 3201244, 2913333, 2624535
	dd 2334937, 2044628, 1753697, 1462231, 1170319, 878052, 585516, 292803
	; 180º
	dd 0

bss:

ABSOLUTE bss

alignb 4

bss_start:
counter:
	resw 1
angle:	resb 1
pcolor:	resb 1 ; previous color (used for black line drawing)
color:	resd 1
hipotenus:
	resd 1 ; current hypotenuse value (line length)
_x0:	resd 1 ; horizontal start point
_y0:	resd 1 ; vertical start point
_x1:	resd 1 ; hypotenuse * cos(angle), X-axis value of the end point
_y1:	resd 1 ; hypotenuse * sin(angle), Y-axis value of the end point
_x2:	resd 1 ; interim value of X (< _x1) = cotangent(angle)*_y1 (if _y1 > _x1)
_y2:	resd 1 ; interim value of Y (< _y1) = tangent(angle)*_x1 (if _x1 > _y1)
sin:	resd 1 ; sine(angle) * 16777216
cos:	resd 1 ; cosine(angle) * 16777216
pixelcount:
	resd 1
_fx:	resd 142 ; For every X values from 0 to 142
	resd 512-142 ; additional buffer
bss_end: