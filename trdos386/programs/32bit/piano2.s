; ****************************************************************************
; piano2.s (TRDOS 386, TRDOS v2.0 - sample binary file, 'piano2.prg')
; ----------------------------------------------------------------------------
; PIANO2.PRG ! TEST program !  INT 34h (IOCTL functions) test !
;
; 21/06/2016
;
; [ Last Modification: 22/06/2016 ]
;
; ****************************************************************************

; 29/04/2016
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
_unlink	equ 10
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
_stty 	equ 31
_gtty	equ 32
_ilgins	equ 33
_sleep	equ 34
_msg    equ 35

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

; TRDOS 386 (and Retro UNIX 386 v1) system call format:
; sys systemcall (eax) <arg1 (ebx)>, <arg2 (ecx)>, <arg3 (edx)>

[BITS 32] ; We need 32-bit intructions for protected mode

[ORG 0] 

start: 
	call	clrscr 
	call	toneon
start1: 
	call	show_panio 
	call	sound 
	call	show_dot 
	mov	ah, 1   ; check keyboard buffer
	int	32h	; TRDOS 386 Keyboard Interrupt
	jz	short start1 
	;
	call	toneoff 
	call	clrscr
 
terminate:
	sys 	_exit			   ; INT 40h
here:
	jmp	short here
 
show_panio:
	dec	byte [delay]
	jnz	short msg1 
	mov	byte [delay], 64 
	mov	byte [fig_r], 4
	mov	byte [fig_c], 0 
	mov	ecx, 9
	mov	esi, msg 
	call	set_wp 
	call	bitmap_char

	call	video_page_update
 
        inc     byte [msgptr]
	and	byte [msgptr], 011111b 
msg1:
	retn 

show_dot: 
 	mov	ebx, [counter]
	cmp	ebx, [lastdot]
	je	short dot1 
 
	mov	ebx, [lastdot] 
	mov	al, [ebx+melody]	 ; get code for nth of 
	dec	al 
	shl	al, 3 
	mov 	[fig_c], al 
        mov     byte [fig_r], 14 
 
	xor	ecx, ecx
	inc	cl	; mov ecx, 1
	mov	esi, blnk 
	call	set_wp
	call	bitmap_char

	call	video_page_update 
 
	mov	ebx, counter 
	mov	[lastdot], ebx 
	mov	al, [ebx+melody]	; get code for nth of 
	dec	al
	shl	al, 3
        mov     [fig_c], al 
        mov     byte [fig_r], 14 
 
	mov	ecx, 1 
	mov	esi, dot 
	call	set_wp 
	call	bitmap_char 

	call	video_page_update 
dot1:
	retn 

bitmap_char: 
each_character: 
	push	ecx 
	movzx	ebx, byte [esi] ; BL is the character's ASCII code 
	inc	esi 
	shl	bx, 3	; ebx = ebx * 8, because each 
			; character's dot pattern occupys 8 bytes 
	add	ebx, pattern 
	mov	dl, 8   ; ROW_LEN ;The dot pattern is 8 bytes (row) 
each_row:
	mov	ah, [ebx] ; Get the byte which refers immediate row 
	inc	ebx	; Pointer to next byte 
	mov	cl, 8   ; COL_LEN ;each row is 8 dots 
each_dot:
	rol	ah, 1	; Bit 0 is the next bit we want 
	mov	al, ah 
	and	al, 1   ; Mask other bits 
	neg 	al	; If Bit 0 = 1 then AL = 0FFh 
	and	al, 0DBh ; If AL = 0FFh then AL = 0DBh else space 
	stosb		; Display AL = char 
	mov	al, 6 
	stosb		; Display AL = attribute 
end_e_dot:
	loop	each_dot
	push	edi 
	sub	edi, 2 
	std 
	mov	cl, 8 
shadow:
	mov	ax, 0600h 
	repe	scasw 
        jcxz    no_shadow 
	add	edi, 4
	mov	ax, 05B2h 
	stosw 
	sub	edi, 2 
	mov	ax, 06DBh 
	repe	scasw 
	jz	short no_shadow 
	inc	cl
	add	edi, 2 
	jmp	shadow 
no_shadow:
	pop	edi 
	add	edi, 144 ; Pointer to next line on screen 
	cld 
	dec	dl 
end_e_row:
	jnz	short each_row 
	sub	edi, 1264 
	pop	ecx 
end_e_char: 
	loop	each_character 
	retn 

set_wp:
	mov 	al, 80 ; number of columns * 2
	mul	byte [fig_r]
	add	al, [fig_c] 
	adc	ah, 0 
	shl	ax, 1 
	movzx	edi, ax
	add	edi, video_buffer ; video page 0 
	retn
 
sound:
	push	eax
	push	ebx
	push	ecx
	push	edx
	push	esi
	push	edi 
	mov	esi, [counter] ; initialize ptr to 
		               ; melody/beat strings 
	; check delay loop 
	mov	ebx, beat      ; get offset of beat 
		       	       ; string 
	movzx	ecx, byte [ebx+esi] ; get beat value for 
		       	       ; note number ESI 
	
	mov	ebx, [ticks]   ; get timer tick count 
	add	ebx, ecx       ; add beat count to 
		       	       ; current tick count 
	mov	ah, 0	       ; function to get 
		               ; time-of-day count 
	int	35h	       ; TRDOS 386 Date&Time Interrupt
	cmp	ecx, ebx       ; cmp count with 
		       	       ; end-of-note count 
	jbe	short finish   ; if not equal, continue 
		       	       ; sound 
 	inc	esi	       ; else, point to next 
		       	       ; note 
	inc	dword [counter] ; go get the next note 

	;mov	ah, 0	       ; function to get 
		               ; time-of-day count 
	int	35h	       ; TRDOS 386 Date&Time Interrupt

	mov	[ticks], ecx
 
 	mov	ah, 1		; out
	mov	al, 0B6h        ; initialize channel 2 
		      	 	; for mode 3 
	mov	dx, 43h		; port address/number	
	int	34h		; TRDOS 386 IOCTL Interrupt

	; Look up a note, get it's frequeny, place in channel 2 
next_note:
	mov	ebx, melody     ; get offset of melody 
		       		; string 
	movzx	eax, byte [ebx+esi] ; get code for nth of 
		       		; the string 
	cmp	al, 0FFh	; is it FF? (end of 
                                ; string marker) 
	je	short no_more   ; if so,jump to end of 
		       		; routine 
	; get the frequency 
	mov	ebx, frequency  ; get offset of the 
		                ; frequency table 
	dec 	al		; EAX-1 so that counting 
		       		; start from 0 
	shl	al, 1	        ; double AX,since word- 
		       		; length table 
	mov	edi, eax	; mov to EDI for 
		       		; addressing 
	mov	cx, [ebx+edi]   ; get the frequency 
		       		; from the table 
	; start the note playing
	mov	ah, 1		; out
	mov	al, cl		; prepare to send low 
		       		; byte of frequency
	;mov	dx, 42h 	; send to latch 
		       		; register (via I/O reg) 
	dec	dx
	int	34h		; TRDOS 386 IOCTL Interrupt

	mov	al, ch		; prepare high byte 
	;mov	dx, 42h 	; send high byte 
	int	34h		; TRDOS 386 IOCTL Interrupt
finish: 
	pop	edi
	pop	esi
	pop	edx
	pop	ecx
	pop	ebx
	pop	eax
	retn

no_more:
	mov	dword [counter], 0
	jmp	short finish

toneon:
	; Turn speaker and timer on 
	mov	ah, 0 	; in
	mov	dx, 61h	; get current status of Port B
	int	34h	; TRDOS 386 IOCTL Interrupt
	or	al, 3	; enable the speaker and timer channel2 
	;mov	ah, 1	; out
	inc	ah	; replace the byte 
	int	34h	; TRDOS 386 IOCTL Interrupt
	retn

toneoff:
	; Turn off timer 2
	mov	ah, 0 	; in
	mov	dx, 61h	; get the byte in Port_B 
	int	34h	; TRDOS 386 IOCTL Interrupt
	and	al, 0FCh ; turn off the speaker bits 
	;mov	ah, 1	; out
	inc	ah	; replace the byte in Port_B 
	int	34h	; TRDOS 386 IOCTL Interrupt
	retn

clrscr:
	mov	edi, video_buffer
	mov	ecx, 80*25
        mov     ax, 0720h ; light gray char space (blank)
	rep	stosw

	call	video_page_update

        xor     dx, dx    ; column 0, row 0

	; DX = cursor position
	mov	ah, 2		; Set cursor position
	xor	bh, bh		; for video page 0
	int	31h		; TRDOS 386 video interrupt
	retn

video_page_update:
	; copy video buffer content to video page 0
	mov	ebx, 1	; BL = 1 = user to system
	mov	dl, 0	; video page 0
	mov	ecx, video_buffer
	mov	eax, 31 ; 'sysvideo'
	int	40h	; TRDOS 386 system call	
	retn

; Data

delay:	db 1
fig_r:	db 0
fig_c:	db 0
msgptr: dd 0
msg:	db 0,1,2,0,1,2,0,1,2,0 
dot	db 3 
blnk	db 4
pattern: db	0FEh, 0FEh, 0FEh, 0FEh, 0FEh, 0FEh, 0FEh, 0FEh 
	 db	0E0h, 0E0h, 0E0h, 0E0h, 0FEh, 0FEh, 0FEh, 0FEh 
	 db	00Eh, 00Eh, 00Eh, 00Eh, 0FEh, 0FEh, 0FEh, 0FEh 
	 db	018h, 014h, 012h, 010h, 070h, 090h, 060h, 0000 
	 db	0000, 0000, 0000, 0000, 0000, 0000, 0000, 0000 
	 db	018h, 03Ch, 07Eh, 0C3h, 0C3h, 07Eh, 03Ch, 018h 

beat:	db   6,6,12	;duration of each 
	db   6,6,12 
	db   6,6,6,6,6,6,12 
	db   6,6,12 
	db   6,6,12 
	db   6,6,6,6,24,0 
			;note 
frequency:
	dw   2280,2031,1809,1709 ;table of 
	dw   1521,1355,1207,1139 ;frequencies 

melody:
	db   5,3,3	 ;,0FFH;frequency code 
	db   4,2,2 
	db   1,2,3,4,5,5,5 
	db   5,3,3 
	db   4,2,2 
	db   1,3,5,5,1,0FFh 
			 ;of each note
lastdot: dd 0
counter: dd 0 
ticks:	 dd 0

db	'Erdogan Tan - TRDOS 386 '
db	'22/06/2016'
db 	0 

bss_start:

ABSOLUTE bss_start

alignb 2

video_buffer:
	resb	4000 ; 80*25*2

;bss_end: