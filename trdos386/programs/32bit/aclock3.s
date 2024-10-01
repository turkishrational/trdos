; ****************************************************************************
; aclock3.s - TRDOS 386 (TRDOS v2.0) Kernel - Analog Clock Demo - Mode 13h
; ----------------------------------------------------------------------------
;
; Erdogan Tan - 30/09/2024
;
; [ Last Modification: 01/10/2024 ]
;
; ****************************************************************************
; ref: aclock.s, circle4.s, line7.s (TRDOS 386 demo programs)

; (Analog Clock display code without FPU instructions)
; ((ONly INT 40h system calls))

; 20/08/2024 ; TRDOS 386 v2.0.9 (exit code)
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
_unlink	equ 10 ; _delete
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
_dma	equ 45
_stdio  equ 46	;  TRDOS 386 v2.0.9

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

; Retro UNIX 386 v1 and TRDOS 386 v2 system call format:
; sys systemcall (eax) <arg1 (ebx)>, <arg2 (ecx)>, <arg3 (edx)>
; ----------------------------------------------------------------------------
 
	bits 32
start:
	;; clear bss
	;mov	edi, bss_start
	;mov	ecx, (bss_end - bss_start)/4
	;;xor	eax, eax
	;rep	stosd

	sys	_time, 0 ; get time in unix/epoch format
	mov	[ptime], eax ; seconds since unix epoch time

	; program message
	mov	esi, program_msg
	call	print_msg

	;;;;
	; Set squares of number from 0 to 89
	mov	edi, _squares
	mov	ecx, 90
	mov	ebx, 1
_ss_x:
	mov	eax, ebx
	mul	ebx
	stosd
	inc	ebx
	loop	_ss_x

	; x2+y2 = r2
	; Set Y values for X values from 1 to Radius - 1
	mov	edi, _fx
	mov	eax, 90
	mov	[radius], eax
	mov	ebx, eax
	mul	ebx
	mov	ebp, eax ; _r2 (square of the radius)
_yy_x:	
	dec	ebx
	jz	short start_@
	mov	eax, ebx
	mul	eax
	; eax = square of ebx
	mov	edx, ebp ; _r2
	sub	edx, eax
	call	get_squareroot
	stosd
	jmp	short _yy_x
	;;;;

	; show program message for 1 second
start_@:
	sys	_time, 0 ; get time in unix/epoch format
	nop
	cmp	eax, [ptime]
	je	short start_@ ; same second

	; 1 second has been passed

	;mov	ah, 2	; read the time
	;int	35h	; TRDOS 386 date&time interrupt
	;jnc	short start_@@
	;jmp	terminate
;start_@@:
	;mov 	ah,11h
	;int	32h
	;jz	short main
	;xor	ah, ah
	;int	32h
start_@@:
	sys	_stdio, 1 ; getchar (no wait)
	;jc	short main
	and	eax, eax
	jnz	short start_@@ 
main:
	;sys	_time, 4 ; get tick counts
	;mov	[startticks], eax

	mov	bl, -1	; signal response byte
	mov	bh, 1	; 18.2 ticks per seconds
	mov	ecx, 9	; approx. 0.5 seconds
	;mov	bh, 3	; 1 second unit(RTC)
	;mov	ecx, 1	; 1 second only
	mov	edx, srb
        sys	_timer	; start timer

	sys	_time, 0 ; get time (seconds) in UNIX format
	mov	[startticks], eax

	;sys	_time, 3 ; get time (& date) in MSDOS format
			 ; (eax = time, edx = date)
	sys	_time, 1 ; get time in MSDOS format
	mov	[second], al ; (dl in MSDOS)
	mov	[minute], ah ; (cl in MSDOS)
	shr	eax, 16	; al = hour (ch in MSDOS)
	mov	[hour], al

	; MAP VGA video buffer to user (as 1 to 1)
	;xor	ebx, ebx
 	mov	bh, 5	; Direct access/map to VGA memory
 	sys	_video
	; eax = 0A0000h
	cmp	eax, 0A0000h ; VGA memory address
	jne	terminate ; error (eax = 0)

	; set video mode to 13h
	;mov	al, 13h ; function 00h, mode 13h
	;int	31h ; TRDOS 386 - Video interrupt
	sys	_video, 0813h ; set video mode to 13h

	; draw clock circle and indicators
	call	draw_background

	;;;
	mov	byte [_x0], 159
	;mov	byte [_y0], 99
	;;;

	; set initial h/m/s parameters
	call	update_time
	mov	al, [hour]
	mov	[phour], al
	mov	al, [minute]
	mov	[pminute], al	; previous
	mov	[phminute], al  ; previous for akrep
	mov	al, [second]
	mov	[psecond], al	; previous
	mov	[pmsecond], al  ; previous for yelkovan
	jmp	short draw_hh_mh_sh
main_loop:
	call	update_time
	jc	short skip_draw
draw_hh_mh_sh:
	; draw akrep (hour hand)
	call	draw_hour_hand

	; draw yelkovan (minute hand)
	call	draw_minute_hand

	; draw second hand
	call	draw_second_hand

	; waith for 0.5 second
	;call	wait_half_second
skip_draw:
        ;mov	ah, 01h	; see if key pressed
	;int	32h 	; TRDOS 386 keyboard interrupt
        ;jz	short main_loop ; loop if no key pressed
        ;xor	ah, ah  ; key pressed so clear it
	;int	32h
	sys	_stdio, 1 ; getchar (no wait)
	or	eax, eax
	jz	short main_loop	

	;;; beep option (enabled/disabled by SPACEBAR)
	cmp	al, 20h
	jne	short exit_process
	xor	byte [nobeep], 0FFh
	jmp	short main_loop
nobeep:	db	0FFh
	;;;

	;jmp	short exit_process

exit_process:
	;mov	ax, 03h ; set video mode to 03h (default)
	;int	31h  ; TRDOS 386 video bios interrupt
	sys	_video, 0803h ; set video mode to 03h
terminate:
	sys	_timer, 0  ; stop timer
	sys	_exit, 0 ; return to TRDOS 386 MainProg

hangemhigh:
	; CPU must not come here !
	nop
	jmp	short hangemhigh

srb:	db 0
ptime: ;db 0
startticks:
	dd 0

; ------------------------------------------------------------

update_time:
;	push	esi
;	;mov	esi, -1
;	mov	esi, 10
;update_time_@:	
;	mov	ah, 2	; read the time
;	int	35h	; TRDOS 386 date&time interrupt
;	;jc	short update_time_retn
;	jnc	short update_time_@@
;	; RTC update phase
;	dec	esi
;	jnz	short update_time_@
;	pop	esi
;	;jmp	exit_process
;	stc
;	retn
;update_time_@@:
;	pop esi

	; ch = hours (bcd)
	; cl = minutes (bcd)
	; dh = seconds (bcd)

	; wait 1 second (kernel timer setup)
	cmp	byte [srb], 0FFh ; check signal response byte
	jb	short update_time_ok ; cf = 1

	mov	byte [srb], 0 ; reset for next 1 second

	; get time in UNIX/Epoch format (seconds)
	sys	_time, 0
	cmp	eax, [startticks] ; is same second ?
	jne	short updt_0	; no, 1 second passed
	; wait 0.5 second more
	stc
update_time_ok:
	retn
updt_0:
	mov	[startticks], eax

	mov	al, [second]
	inc	al
	cmp	al, 60
	jb	short updt_1
	mov	al, 0
updt_1:
	;mov	al, dh
	;call	convert_bcd_to_bin
	mov	[second], al

	or	al, al
	jnz	short update_time_ok

	mov	al, [minute]
	inc	al
	cmp	al, 60
	jb	short updt_2
	mov	al, 0
updt_2:
	;mov	al, cl
	;call	convert_bcd_to_bin
	mov	[minute], al

	and	al, al
	jnz	short update_time_ok

	mov	al, [hour]
	inc	al
	cmp	al, 24
	jb	short updt_3
	mov	al, 0
updt_3:	
	;mov	al, ch
	;call	convert_bcd_to_bin
	mov	[hour], al
;update_time_retn:
;update_time_ok:
	retn

; ------------------------------------------------------------

;wait_half_second:
;	push	ebx
;	sys	_time, 4 ; get tick counts
;	pop	ebx
;	sub	eax, [startticks]
;	cmp	eax, 9
;	jb	short wait_half_second
;	add	[startticks], eax
;
;	;nop
;	;inc	ecx
;	;nop
;	;cmp	byte [srb], -1
;	;jne	short wait_half_second
;
;	retn

; ------------------------------------------------------------

;convert_bcd_to_bin:
;	mov	bl, al
;	and	bl, 0Fh
;	shr	al, 4
;	mov	ah, 10
;	mul	ah
;	add	al, bl
;	retn

; ------------------------------------------------------------

draw_background:
	; INPUT:
	;	none
	;
	; Modified registers: esi, edi, eax, ecx, ebx, edx

	mov	dword [_x0], 160
	mov 	dword [_y0], 100
	mov	dword [radius], 90
	mov	byte [color], 0Bh ; cyan
	;mov	byte [color], 0Eh ; yellow
	;mov	byte [color], 0Fh ; white
	call	draw_circle ; writes pixels to pixel buffer 
		 ; writes all circle pixels to video buffer
	;call	write_circle 

	; draw minute indicators
	call	draw_minute_dots
	; draw hour (5 minutes) indicators
	call	draw_hour_squares
	retn

; ------------------------------------------------------------

draw_circle:
	; INPUT:
	;	[_x0]
	;	[_y0]
	;	[radius]
	;	[color]
	;
	; Modified registers: esi, edi, eax, ecx, ebx, edx, ebp

	; set pixel pointer position to start of circle buffer
	mov	eax, circlebuffer
	mov	[pixelpos], eax	
_dc_ph0:
	; quarter 1	
	; start from y = 0, x = radius
	xor	eax, eax
	mov	[_y1], eax ; 0
	mov	[phase], al ; 0
	mov	ebp, [radius]
	mov	[_x1], ebp ; y = 0, x = r
	mov	esi, _fx
_dc_ph0_n:
	dec	dword [_x1]
	lodsd
_dc_ph0_x:
	mov	edx, [_y1]
	inc	edx
	cmp	edx, eax
	jnb	short _dc_ph0_y
	push	eax
	mov	[_y1], edx
	call	get_start_offset
	call	write_pixel
	pop	eax
	jmp	short _dc_ph0_x	
_dc_ph0_y:
	mov	[_y1], eax
	call	get_start_offset
	call	write_pixel
	dec	ebp
	jnz	short _dc_ph0_n
_dc_ph1:
	; quarter 2	
	; start from y = radius, x = 0
	inc	byte [phase]
	xor	eax, eax
	mov	[_x1], eax ; 0
	mov	ebp, [radius]
	mov	[_y1], ebp ; y = r, x = 0
	mov	esi, _fx
_dc_ph1_n:
	dec 	dword [_y1]
	lodsd
_dc_ph1_x:
	mov	edx, [_x1]
	inc	edx
	cmp	edx, eax
	jnb	short _dc_ph1_y
	push	eax
	mov	[_x1], edx
	call	get_start_offset
	call	write_pixel
	pop	eax
	jmp	short _dc_ph1_x	
_dc_ph1_y:
	mov	[_x1], eax
	call	get_start_offset
	call	write_pixel
	dec	ebp
	jnz	short _dc_ph1_n
_dc_ph2:
	; quarter 3	
	; start from y = 0, x = radius
	inc	byte [phase]
	xor	eax, eax
	mov	[_y1], eax ; 0
	mov	ebp, [radius]
	mov	[_x1], ebp ; y = 0, x = r
	mov	esi, _fx
_dc_ph2_n:
	dec	dword [_x1]
	lodsd
_dc_ph2_x:
	mov	edx, [_y1]
	inc	edx
	cmp	edx, eax
	jnb	short _dc_ph2_y
	push	eax
	mov	[_y1], edx
	call	get_start_offset
	call	write_pixel
	pop	eax
	jmp	short _dc_ph2_x	
_dc_ph2_y:
	mov	[_y1], eax
	call	get_start_offset
	call	write_pixel
	dec	ebp
	jnz	short _dc_ph2_n
_dc_ph3:
	; quarter 4	
	; start from y = radius, x = 0
	inc	byte [phase]
	xor	eax, eax
	mov	[_x1], eax ; 0
	mov	ebp, [radius]
	mov	[_y1], ebp ; y = r, x = 0
	mov	esi, _fx
_dc_ph3_n:
	dec	dword [_y1]
	lodsd
_dc_ph3_x:
	mov	edx, [_x1]
	inc	edx
	cmp	edx, eax
	jnb	short _dc_ph3_y
	push	eax
	mov	[_x1], edx
	call	get_start_offset
	call	write_pixel
	pop	eax
	jmp	short _dc_ph3_x	
_dc_ph3_y:
	mov	[_x1], eax
	call	get_start_offset
	call	write_pixel
	dec	ebp
	jnz	short _dc_ph3_n
_dc_ph4:
	;retn

; ------------------------------------------------------------

write_dots:
write_line:
write_circle:
	mov	esi, circlebuffer
	mov	edi, 0A0000h ; VGA video buffer
	mov	ecx, [pixelpos]
	sub	ecx, esi
	shr	ecx, 2
	mov	bl, [color]
	;ecx = pixel count
write_circle_@:
	lodsd
	mov	[edi+eax], bl ; pixel color
	loop	write_circle_@
	retn

; ------------------------------------------------------------

write_pixel:
	; eax = (screen) pixel position
	mov	edi, [pixelpos]
	stosd
	mov	[pixelpos], edi
	retn

; ------------------------------------------------------------

get_start_offset:
	mov	eax, 320
	mov	edx, [_y0]
	cmp	byte [phase], 0
	ja	short gso_1
gso_0:
	; quarter 1
	sub	edx, [_y1] ; y = 0 -> r
	mul	edx
	add	eax, [_x0]
	add	eax, [_x1] ; x = r -> 0
	retn
gso_1:
	cmp	byte [phase], 1
	ja	short gso_2
	; quarter 2
	sub	edx, [_y1] ; y = r -> 0
	mul	edx
	add	eax, [_x0]
	sub	eax, [_x1] ; x = 0 -> -r
	retn
gso_2:
	cmp	byte [phase], 2
	ja	short gso_3
	; quarter 3
	add	edx, [_y1] ; y = 0 -> -r 
	mul	edx
	add	eax, [_x0]
	sub	eax, [_x1] ; x = -r -> 0 
	retn
gso_3:
	; quarter 4
	add	edx, [_y1] ; y = -r -> 0
	mul	edx
	add	eax, [_x0]
	add	eax, [_x1] ; x = 0 -> r 
	retn

; ------------------------------------------------------------

;black_circle:
;	xor	ah, ah
;	xchg	[color], ah ; color = 0 
;	push	eax
;	call	drawcircle
;	pop	eax
;	xchg	[color], ah ; restore color
;	retn

; ------------------------------------------------------------

beep:
	;;; beep option
	test	byte [nobeep], 0FFh
	jnz	short beep_retn
	;;;

	; call beep function (16/64 second, 886Hz)
	;sys	_audio, 16, 1331
	sys	_stdio, 3, 07h ; write beep char to STDERR
beep_retn:
	retn

; ------------------------------------------------------------

draw_minute_dots:
	; INPUT:
	;	none
	;
	; Modified registers: esi, edi, eax, ecx, ebx, edx

	;mov	dword [angle], 0
	mov	byte [radius], 85
	;mov	byte [_x0], 160
	;mov	byte [_y0], 100
	mov	dword [step], 6
	mov	byte [dcount], 60

	mov	dword [pixelpos], circlebuffer
			; reset for indicator dots

	xor	eax, eax  ; angle = 0
	call	draw_dots

	;mov	byte [angle], 0
	call	write_dots
	retn

; ------------------------------------------------------------

draw_dots:
dmd_0:
	mov	[angle], eax
	; eax = angle	
	call	getcosinus 
		; get cosine value * 16777216 for angle in AL
	; eax = cos(angle) * 16777216 
	call	getxy
	mov	[_x1], eax ; projection of end point on x-axis
	mov	eax, [angle]
	call	getsinus
		; get sine value * 16777216 for angle in AL
	; eax = sin(angle) * 16777216 
	call	getxy
	mov	[_y1], eax ; projection of end point on y-axis

	mov	byte [phase], 0	; quarter 1
	mov	eax, [angle]
	cmp	eax, 90
	jna	short dmd_1
	inc	byte [phase]	; quarter 2
	cmp	eax, 180
	jna	short dmd_1
	inc	byte [phase]	; quarter 3
	cmp	eax, 270
	jna	short dmd_1
	inc	byte [phase]	; quarter 4
dmd_1:
	; cover coordinates to video buffer offset
	call	get_start_offset
	call	write_pixel	; save it to pixel buffer
	
	dec	byte [dcount]
	jz	short dmd_2
	mov	eax, [step]
	add	eax, [angle]
	jmp	dmd_0
dmd_2:
	retn

; ------------------------------------------------------------

draw_hour_squares:
	; INPUT:
	;	none
	;
	; Modified registers: esi, edi, eax, ecx, ebx, edx

	;mov	dword [angle], 0
	mov	byte [radius], 85

	mov	dword [step],30

	mov	dword [pixelpos], circlebuffer
			; reset for indicator dots
	xor	eax, eax
dhs_@:
	mov	dword [_x0], 159
	mov	dword [_y0], 99
dhs_0:
	mov	byte [dcount], 1
	call	draw_dots
	mov	eax, [_x0]
	cmp	eax, 162
	jnb	short dhs_2
	inc	eax
dhs_1:
	mov	[_x0], eax
	mov	eax, [angle]
	jmp	short dhs_0
dhs_2:
	mov	eax, [_y0]
	cmp	eax, 102
	jnb	short dhs_3
	inc	eax
	mov	[_y0], eax
	mov	al, 159
	jmp	short dhs_1
dhs_3:
	mov	eax, [angle]
	add	eax, [step]
	cmp	eax, 360
	jb	short dhs_@

	; write all of hour indicator pixels
	; to VGA video buffer 
	;mov	dword [angle], 0
	call	write_dots
	retn

; ------------------------------------------------------------

getsinus:
	; Input:
	;	EAX = angle
	; output:
	;	EAX = sin(angle) * 16777216
	;
	; Modified registers: eax, esi
	;

	; Note: absolute (+) values are needed only.
	; (see 'get_start_offset' procedure)

	cmp	eax, 360
	jna	short nungec

	hlt
nungec:
	cmp	eax, 180
	jb	short gsin_@
	sub	eax, 180
gsin_@:
	;movzx	esi, al
	mov	esi, eax
	shl	esi, 2 ; * 4
	add	esi, sinustable
	mov	eax, [esi]
	mov	[sin], eax
	retn

; ------------------------------------------------------------

getcosinus:
	; Input:
	;	EAX = angle
	; output:
	;	EAX = cos(angle) * 16777216
	;
	; Modified registers: eax, esi

	; Note: absolute (+) values are needed only.
	; (see 'get_start_offset' procedure)



	cmp	eax, 360
	jna	short nunugec

	hlt
nunugec:

	cmp	eax, 180
	jb	short gcos_0
	sub	eax, 180
gcos_0:
	add	eax, 90
	jmp	short getsinus

	cmp	al, 90 ; 90 degrees
	jb	short gcos_1
	sub	al, 90	
	mov	esi, cos_90
	jmp	short gcos_2
gcos_1:	
	mov	esi, cos_0
gcos_2:
	;movzx	eax, al
	shl	eax, 2 ; * 4
	add	esi, eax
	mov	eax, [esi]
	mov	[cos], eax
	retn

; ------------------------------------------------------------

getxy:
	; Input:
	;	EAX = sin or cos value * 16777216
	; output:
	;	EAX = x or y projection
	;
	; Modified registers: eax, edx, (ecx)
	;

	;mov	edx, [radius]
	;mul	edx
	mul	dword [radius]  ; [hipotenus]
	;mov	ecx, 16777216
	;div	ecx
	shr	eax, 24
	retn

; ------------------------------------------------------------

get_squareroot:
	; input: edx = square of the number (y)
	; output: eax = approx. square root of ebx
	mov	esi, _squares
	push	ebx
	xor	ebx, ebx
	mov	ecx, [radius] ; max. value of radius is 89
q_sr_x:	
	lodsd
	cmp	eax, edx
	jnb	short q_sr_ok
	inc	ebx
	loop	q_sr_x
q_sr_ok:
	mov	eax, ebx
	pop	ebx
	retn

; ------------------------------------------------------------

draw_hour_hand:		; draw akrep
	; INPUT:
	;	[hour]
	;	[phour]
	;	[phminute]
	;
	; Modified registers: esi, edi, eax, ecx, ebx, edx, ebp

	movzx	eax, byte [phour] ; previous hour

	;;;
	mov	dl, [phminute]
	sub	dl, [minute] 	; is 2 minutes passed?
	jnb	short chk_m_pm
	neg	dl
chk_m_pm:
	cmp	dl, 2
	jnb	short skip_mh_chk
	;;;

	;movzx	eax, byte [phour] ; previous hour
	cmp	al, [hour]
	je	short dhh_@	; skip black/erase line stage

skip_mh_chk:
	mov	byte [color], 0	; black
	call	dhh_@@		; draw black line
				; (erase previous hour hand)
	xor	eax, eax
	mov	al, [minute]
	mov	[phminute], al	; set current minute as prev
	mov	al, [hour]
	mov	[phour], al	; set current hour as previous
dhh_@:
	mov	byte [color], 0Fh ; draw white line (hour hand)
dhh_@@:
	cmp	al, 12
	jb	short skip_24hto12h
	sub	al, 12
skip_24hto12h:
	mov	ecx, 30		; 30 degrees per hour
	mul	ecx
	mov	cl, [phminute]
	shr	cl, 1		; 60 minutes -> 30 degrees
	;adc	cl, 0		; round up if half degree
	add	eax, ecx
	mov	byte [radius], 65 ; [hipotenus], length
dmh_@@@:
dsh_@@@:
	; convert clockwise angle to counterclockwise angle
	neg	eax	; -x
	add	eax, 450 ; 450 - x ; (360+90-x)
	cmp	eax, 360
	jb	short dhh_@@@	; <= 12
	sub	eax, 360	; 2nd tour of hourhand
dhh_@@@:
	; convert clockwise angle to counterclockwise angle
	mov	[angle], eax
	
	call	draw_line

	retn

; ------------------------------------------------------------

draw_minute_hand:		; draw yelkovan
	; INPUT:
	;	[minute]
	;	[pminute]
	;	[pmsecond]
	;
	; Modified registers: esi, edi, eax, ecx, ebx, edx, ebp

	movzx	eax, byte [pminute] ; previous minute

	;;;
	mov	dl, [pmsecond]
	sub	dl, [second] 	; is 5 seconds passed?
	jnb	short chk_s_ps
	neg	dl
chk_s_ps:
	cmp	dl, 5
	jnb	short skip_sm_chk
	;;;

	;movzx	eax, byte [pminute] ; previous minute
	cmp	al, [minute]
	je	short dmh_@	; skip black/erase line stage

skip_sm_chk:
	mov	byte [color], 0	; black
	call	dmh_@@		; draw black line
				; (erase previous minute hand)
	;;;
	; redraw houndhand if blanked
	mov	al, [phour]
	mov	ah, 5
	mul	ah
	mov	ah, [pminute]
	cmp	al, ah
	jne	short skip_redraw_hh
	call	draw_hour_hand
skip_redraw_hh:
	;;;

	xor	eax, eax
	mov	al, [second]
	mov	[pmsecond], al	; set current second as prev
	mov	al, [minute]
	mov	[pminute], al	; set current minute as prev
dmh_@:
	mov	byte [color], 0Fh ; draw white line (minute hand)
dmh_@@:
	mov	ecx, 6		; 6 degrees per minute
	mul	ecx
	push	eax
	xor	eax, eax
	mov	al, [pmsecond]
	;xor	edx, edx
	mov	cl, 10
	div	ecx		; 60 seconds -> 6 degrees
	pop	edx
	add	eax, edx
	mov	byte [radius], 80 ; [hipotenus], length
	; convert clockwise angle to counterclockwise angle
	jmp	dmh_@@@
	;neg	eax	; -x
	;add	eax, 450 ; 450 - x
	;cmp	eax, 360
	;jb	short dmh_@@@	; <= 12
	;sub	eax, 360	; 2nd tour of hourhand
;dmh_@@@:
	; convert clockwise angle to counterclockwise angle 
	;sub	edx, eax
	;mov	[angle], edx
	;
	;call	draw_line
	;
	;retn

; ------------------------------------------------------------

draw_second_hand:		; saniye ibresi
	; INPUT:
	;	[second]
	;	[psecond]
	;
	; Modified registers: esi, edi, eax, ecx, ebx, edx, ebp

	movzx	eax, byte [psecond] ; previous second
	cmp	al, [second]
	je	short dsh_@	; skip black/erase line stage

	;;;	beep option
	mov	byte [pbeep], 1
	;;;

	mov	byte [color], 0	; black
	call	dsh_@@		; draw black line
				; (erase previous second hand)
	;;;
	; redraw houndhand and minutehand if blanked
	mov	al, [phour]
	mov	ah, 5
	mul	ah
	mov	ah, [psecond]
	cmp	al, ah
	jne	short skip_redraw_shh
	call	draw_hour_hand
skip_redraw_shh:
	mov	al, [pminute]
	cmp	al, [psecond]
	jne	short skip_redraw_smh
	call	draw_minute_hand
skip_redraw_smh:
	;;;

	xor	eax, eax
	mov	al, [second]
	mov	[psecond], al	; set current second as prev
dsh_@:
	;mov	byte [color], 0Fh ; draw white line (second hand)
	mov	byte [color], 0Ch ; draw red line	
dsh_@@:
	mov	ah, 6		; 6 degrees per second
	mul	ah
	mov	byte [radius], 75 ; [hipotenus], length
	;jmp	dsh_@@@
	;;;
	call	dsh_@@@
	;*** beep option (uses sysaudio system call)
	; do not beep while drawing blank/black line
	cmp	byte [color], 0
	jz	short no_beep
	; only one beep in same/one second
	dec	byte [pbeep]
	jnz	short no_beep
	call	beep
	;***
no_beep:
	retn
	;;;
	;neg	eax	; - x
	;add	eax, 450  ; 450 - x
	;cmp	eax, 360
	;jb	short dsh_@@@
	;sub	eax, 360
;dsh_@@@:
	;mov	[angle], eax
	;
	;call	draw_line
	;
	;retn
	
; ------------------------------------------------------------

draw_line:
	; INPUT:
	;	[_x0]
	;	[_y0]
	;	[radius] ; [hipotenus]
	;	[angle]
	;	[color]
	;
	; Modified registers: esi, edi, eax, ecx, ebx, edx, ebp

	mov	eax, [angle]
	; eax = angle	
	call	getcosinus ; get cosine value * 16777216 for angle in AL
	; eax = cos(angle) * 16777216
	mov	[cos], eax
	call	getxy
	mov	[_x1], eax ; projection of end point on x-axis
	mov	eax, [angle] 
	call	getsinus ; get sine value * 16777216 for angle in AL
	; eax = sin(angle) * 16777216
	mov	[sin], eax
	call	getxy
	mov	[_y1], eax ; projection of end point on y-axis

_draw_l_0:
	xor	ecx, ecx
	mov	[_x2], ecx ; 0  ; reset
	mov	[_y2], ecx ; 0	; reset
	mov	dword [pixelpos], circlebuffer ; linebuffer
	mov	eax, [_x1]
	cmp	eax, [_y1]
	jnb	short _draw_l_x0
	; base axis is y-axis
	jmp	_draw_l_y0
_draw_l_x0:
	; base axis is x-axis
	; draw line from x = 0 to [_x1]
	; set y by using x*([sin]/[cos])
	; ecx = 0
	;mov	[prevy], ecx	; previous Y value
	call	write_line_pixel
	mov	ecx, [_y2]
_draw_l_x1:
	mov	eax, [_x2]
	inc	eax
	cmp	eax, [_x1]
	ja	short _draw_l_x3
	mov	[_x2], eax
	mov	edx, [sin]
	mul	edx
	mov	ebx, [cos]
	div	ebx
	mov	[_y2], eax
	mov	ebp, eax
	inc	ecx
	cmp	ecx, ebp ; previous Y+1 < current Y ?
	jnb	short _draw_l_x0
	;jmp	short _draw_l_x0
	dec	dword [_x2]
_draw_l_x2:
	mov	[_y2], ecx
	call	write_line_pixel
	inc	ecx
	cmp	ecx, ebp ; loop end for [_y2]
	jb	short _draw_l_x2
	mov	[_y2], ebp
	inc	dword [_x2]
	jmp	short _draw_l_x0

_draw_l_x3:
_draw_l_y3:
	call	write_line  ; write line pixels to VGA buffer
	retn

_draw_l_y0:
	; base axis is y-axis
	; draw line from y = 0 to [_y1]
	; set x by using y*([cos]/[sin])
	; ecx = 0

	;mov	dword [prevx], ecx ; previous X value
	call	write_line_pixel
	mov	ecx, [_x2]
_draw_l_y1:
	mov	eax, [_y2]
	inc	eax
	cmp	eax, [_y1]
	ja	short _draw_l_y3
	mov	[_y2], eax
	;mov	edx, [cos]
	;mul	edx
	mul	dword [cos]
	;mov	ebx, [sin]
	;div	ebx
	div	dword [sin]
	mov	[_x2], eax
	mov	ebp, eax
	inc	ecx
	cmp	ecx, ebp ; previous X+1 < current X ?
	;jnb	short _draw_l_y0
	jmp	short _draw_l_y0
	dec	dword [_y2]
_draw_l_y2:
	mov	[_x2], ecx
	call	write_line_pixel
	inc	ecx
	cmp	ecx, ebp ; loop end for [_y2]
	jb	short _draw_l_y2
	mov	[_x2], ebp
	inc	dword [_y2]
	jmp	short _draw_l_y0

; ------------------------------------------------------------

write_line_pixel:
	; write pixel to line buffer
	; Modified registers: eax, ebx, edx
	mov	eax, [_y0] ; 100
	cmp	dword [angle], 180
	jna	short wlp_0
	; y = -y
	add	eax, [_y2] ; under the start point
	jmp	short wlp_1
wlp_0:
	sub	eax, [_y2]
wlp_1:
	mov	edx, 320
	mul	edx
	add	eax, [_x0] ; 160
			; center of the screen
	mov	edx, [angle]
	cmp	edx, 90
	jna	short wlp_4 ; quarter/quadrand 1
	cmp	edx, 270
	ja	short wlp_4 ; quarter/quadrand 4
wlp_3: 
	; x = -x
	sub	eax, [_x2] ; negative direction
	jmp	short wlp_5 
wlp_4:
	; x = x
	add	eax, [_x2] ; positive direction	
wlp_5:
	call	write_pixel
	retn

; ------------------------------------------------------------

;blackline:
;	push	eax
;	xor	al, al
;	xchg	[color], al ; color = 0
;	mov	[pcolor], al
;	call	write_line
;	mov	al, [pcolor]
;	mov	[color], al ; restore color
;	pop	eax
;	retn

; ------------------------------------------------------------

;set_text_mode:
;	xor    ah, ah
;	mov    al, 3
;	;int   10h ; al = 03h text mode, int 10 video
;	int    31h ; TRDOS 386 - Video interrupt
;	retn

; ------------------------------------------------------------

print_msg:
	; INPUT:
	;   esi = ASCIIZ message address	
	 
	;mov	ah, 0Eh
	;mov	ebx, 7
	;;mov	bl, 7 ; char attribute & color
;p_next_chr:
	;lodsb
	;or	al, al
	;jz	short p_retn ; retn
	;int	31h
	;jmp	short p_next_chr
	
	; max. char count = 255, cxolor = 07h
	sys	_msg, esi, 255, 07h
p_retn:
	retn

; ------------------------------------------------------------

sinustable:
; from A = 0 degree to 180 degrees (sin(A)*65536*256)
; 0º
cos_90:
dd 0
dd 292803, 585516, 878052, 1170319, 1462231, 1753697, 2044628, 2334937, 2624535, 2913333
dd 3201244, 3488179, 3774052, 4058776, 4342263, 4624427, 4905183, 5184445, 5462127, 5738146
dd 6012416, 6284856, 6555381, 6823909, 7090358, 7354647, 7616697, 7876426, 8133756
cos_60:
; 30º
dd 8388608, 8640905, 8890570, 9137527, 9381700, 9623016, 9861400, 10096781, 10329086, 10558244
dd 10784187, 11006844, 11226149, 11442034, 11654434
sin_45:
cos_45:
; 45º
dd 11863283, 12068519, 12270079, 12467901, 12661926, 12852093, 13038346, 13220627, 13398880, 13573053
dd 13743091, 13908942, 14070557, 14227886, 14380881
cos_30:	; 60º
dd 14529495, 14673684, 14813402, 14948609, 15079262, 15205322, 15326749, 15443509, 15555564, 15662880
dd 15765426, 15863169, 15956081, 16044131, 16127295, 16205546, 16278861, 16347217, 16410594, 16468971
dd 16522332, 16570661, 16613941, 16652161, 16685309, 16713374, 16736348, 16754223, 16766996, 16774661
sin_90:
cos_0:
; 90º
dd 16777216, 16774661, 16766996, 16754223, 16736348, 16713374, 16685309, 16652161, 16613941, 16570661
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

; ------------------------------------------------------------

program_msg:
	db "TRDOS 386 v2.0.9 - Analog Clock Demo Program"
	db 0Dh, 0Ah
	db "by Erdogan Tan - 01/10/2024"
nextline:
	db 0Dh, 0Ah, 0

; ------------------------------------------------------------

bss:

ABSOLUTE bss

alignb 4

bss_start:
hipotenus:
radius:	resd 1 ; Current Radius value
color:	resd 1
_x0:	resd 1
_y0:	resd 1
_x1:	resd 1
_y1:	resd 1

_x2:	resd 1
_y2:	resd 1

angle:	resd 1
step:	resd 1
dcount: resb 1
phase:	resb 1

hour:	resb 1
minute: resb 1
second: resb 1
phour:	resb 1
pminute: resb 1
psecond: resb 1
phminute: resb 1
pmsecond: resb 1

pcolor:	resb 1
pbeep:	resb 1

sin:	resd 1 ; sine(angle) * 16777216
cos:	resd 1 ; cosine(angle) * 16777216

_squares:
	resd 90 ; squares of numbers from 0 to 89
	resd 1
_fx:	resd 90 ; For every X values from 0 to 89
	resd 1

pixelpos:
	resd 1
circlebuffer:
	resd 10000 ; 100*100*4 bytes

bss_end:
