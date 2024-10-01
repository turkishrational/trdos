; analogic clock 8086/87 (using 13h graphic mode)
; written by Leonardo Ono (ono.leo@gmail.com)
; 26/09/2017
; target os: DOS (.COM file extension)
; use: nasm aclock.asm -o aclock.com -f bin
; ----------------------------------------------
; TRDOS 386 Adaptation: Erdogan Tan - 29/09/2024
; ----------------------------------------------

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

		bits 32
start:
		mov ah, 2 ; read the time
		int 35h	  ; TRDOS 386 date&time interrupt
		jnc short start_@
		jmp terminate	
start_@:
		sys _time, 4	; get tick counts
		mov [startticks], eax

		;mov bl, -1	; signal response byte
		;mov bh, 1	; 18.2 ticks per seconds
		;mov ecx, 9	; approx. 0.5 seconds
		;mov edx, srb 
                ;sys _timer	; start timer
		
		call start_graphic_mode
		;fninit	 ; FPU is initialized by TRDOS 386 kernel
		call draw_background

	.main_loop:
		call update_time
		;jc  short skip_draw

		call update_angles
		
		mov  ecx, 15	; 0Fh ; white
		call draw_pointers
		
		;sys _stdio,1

		;mov ah, 11h
		;int 32h ; if key pressed, exit
		;jnz exit_process

		call sleep_half_s ; wait 0.5 second

		sub  ecx, ecx ; 0  ; black
		call draw_pointers ; clear previous pointers
;skip_draw:
          	mov  ah,01h	; see if key pressed
	  	int  32h ; TRDOS 386 keyboard interrupt
           	jz   short .main_loop ; loop if no key pressed
           	xor  ah,ah  	; key pressed so clear it
	   	int  32h
		jmp  exit_process

sleep_half_s:
		push ebx
		sys _time, 4 ; get tick counts
		pop ebx
		sub eax, [startticks]
		cmp eax, 9
		jb  short sleep_half_s
		add [startticks], eax

		;nop
		;inc ecx
		;nop
		;cmp byte [srb], -1
		;jne short sleep_half_s
		
		retn

;srb:		db 0
startticks:	dd 0

; in:
;	edi = angle
;	esi = size
update_pointer:
		fld qword [edi]
		fcos
		fld qword [esi]
		fmul st1
		fistp word [data.x]
		ffree st0
	
		fld qword [edi]
		fsin
		fld qword [esi]
		fmul st1
		fistp word [data.y]
		ffree st0

		retn

update_angles:
		mov ebx, data.v720
		mov esi, data.hours
		mov edi, data.angle_h
		call update_angle

		mov ebx, data.v60
		mov esi, data.minutes
		mov edi, data.angle_m
		call update_angle

		mov ebx, data.v60
		mov esi, data.seconds
		mov edi, data.angle_s
		call update_angle

		retn

; in:
;	ebx = v720 or v60
;	esi = hours, minutes or seconds
;	edi = angle_h, angle_m or angle_s
update_angle:
		fld qword [data.v90deg]
		fld qword [data.pi2]
		fld qword [ebx]
		fild word [esi]
		fdiv st1
		fmul st2
		fsub st3
		fstp qword [edi]
		ffree st0
		ffree st1
		ffree st2
		retn

update_time:
		push esi
		;mov esi, -1
		mov esi, 10  	
update_time_@:	
		mov ah, 2 ; read the time
		int 35h	  ; TRDOS 386 date&time interrupt
		;jc short update_time_retn
		jnc short update_time_@@
		; RTC update phase
		dec esi
		jnz short update_time_@
		pop esi
		;jmp exit_process
		stc
		retn	
update_time_@@:
		pop esi

		; ch = hours (bcd)
		; cl = minutes (bcd)
		; dh = seconds (bcd)

		mov al, dh
		call convert_byte_bcd_to_bin
		mov byte [data.seconds], al

		mov al, cl
		call convert_byte_bcd_to_bin
		mov byte [data.minutes], al

		mov al, ch
		call convert_byte_bcd_to_bin
		;mov ah, 0
		mov bx, 60
		mul bx
		add ax, word [data.minutes]
		mov word [data.hours], ax ; in number of minutes
;update_time_retn:		
		retn

; in:
;	cx = color index
draw_pointers:
		mov edi, data.angle_h
		;mov esi, data.size50
		mov esi, data.size60
		call update_pointer
		mov edi, ecx
		call draw_pointer

		mov edi, data.angle_m
		mov esi, data.size80
		call update_pointer
		mov edi, ecx
		call draw_pointer

		mov edi, data.angle_s
		mov esi, data.size80
		call update_pointer
		mov edi, ecx
		call draw_pointer

		retn

; in:
;	di = color index
draw_pointer:
		pushad
		mov eax, 160
		mov ebx, 100
		mov ecx, 160
		mov edx, 100
		add cx, word [data.x]
		add dx, word [data.y]
		call draw_line
		popad
		retn

; in:
;	ecx = number of steps
;	ebx = angle incrementation
;   edi = angle variable
;   esi = radius
draw_circle:
	.next:
		fld qword [ebx]
		fld qword [edi]
		fadd st1
		fstp qword [edi]
		ffree st0

		mov edi, data.angle_s
		;mov esi, data.size90
		call update_pointer

		pushad
		mov al, 15
		mov ecx, 160
		add cx, word [data.x]
		mov ebx, 100
		add bx, word [data.y]
		call pset
		popad
		
		loop .next
		retn
		
draw_hours_indications:
		mov ecx, 12
	.next:
		push ecx

		fld qword [data.v30deg]
		fld qword [data.angle_h]
		fadd st1
		fstp qword [data.angle_h]
		ffree st0

		mov edi, data.angle_h
		mov esi, data.size85
		call update_pointer

	.draw_square:
		mov eax, 159
		mov edx, 99
	.next_dot:		
		call .draw_square_dot
		inc eax
		;cmp eax, 162
		cmp al, 162
		jb short .next_dot
	.dot_next_y:
		;mov eax, 159
		mov al, 159
		inc edx
		;cmp edx, 102
		cmp dl, 102
		jb short .next_dot

		pop ecx
		loop .next

		retn

	; eax = x
	; edx = y
	.draw_square_dot:
		pushad
		mov ecx, eax
		add cx, word [data.x]
		mov ebx, edx
		add bx, word [data.y]
		mov al, 15
		call pset
		popad
		retn
		
draw_background:
		; draw external circle
		mov ecx, 720
		mov ebx, data.vhalf_deg
		mov esi, data.size90
		mov edi, data.angle_s
		call draw_circle

		; draw minutes indications
		mov ecx, 60
		mov ebx, data.v6deg
		mov esi, data.size85
		mov edi, data.angle_m
		call draw_circle

		call draw_hours_indications
		retn

exit_process:
		;sys _timer, 0  ; stop timer

		mov  ax,0003h ; set video mode to 03h (default)
		int  31h  ; TRDOS 386 video bios interrupt
terminate:
		sys _exit

; in:
;	example:
;	al = 11h (bcd)
; out:
;	al = 0Bh
convert_byte_bcd_to_bin:
		;push ebx	
		mov bl, al
		and bl, 0Fh
		shr al, 4
		mov ah, 10
		mul ah
		add al, bl
		;pop ebx
		retn
		
		%include "graphic.s"

data:
		.angle_s	dq 0
		.angle_m	dq 0
		.angle_h	dq 0

		.hours		dw 0 ; in number of minutes
		.minutes	dw 0
		.seconds	dw 0

		.size90		dq 90.0
		.size85		dq 85.0
		.size80		dq 80.0
		;.size50	dq 50.0
		.size60		dq 60.0
		
		.pi2		dq 6.28318
		
		.vhalf_deg	dq 0.00872665
		.v6deg		dq 0.10472
		.v90deg		dq 1.5708
		.v30deg		dq 0.523599
		
		.v60		dq 60.0
		.v720		dq 720.0
		
		.x 		dw 0
		.y 		dw 0
		
		.tmp		dd 0
