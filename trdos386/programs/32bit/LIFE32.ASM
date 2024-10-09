; FASM for DOS example: LIFE.ASM
; TRDOS 386 Adaptation: Erdogan Tan - October 9, 2024
; ---------------------------------------------------
; LIFE32.ASM

; LIFE - fasm example program

; Keyboard:
;   arrow keys - move cursor
;   space - switch cell
;   enter - do next generation
;   escape - exit program

	;org	100h

	use32
	;org	0x0

	;mov	di,screen_data
	mov	edi,screen_data
	xor	al,al
	;mov	cx,80*50*2
	mov	ecx,80*50*2
	rep	stosb

	mov	ax,3
	;int	10h			; set text mode
	int	31h ; TRDOS 386 VIDEO
	mov	ah,1
	mov	ch,20h
	;int	10h			; hide cursor
	int	31h
	mov	ax,1003h
	;xor	bx,bx
	xor	ebx,ebx
	;int	10h			; enable background intensity
	int	31h

	mov	ax,1100h
	;mov	bp,DCh_pattern
	mov	ebp,DCh_pattern
	mov	cx,1
	mov	dx,0DCh
	mov	bx,1000h
	;int	10h
	int	31h

	;mov	ax,0B800h
	;mov	es,ax
	;xor	di,di
	mov	bh,4	; CGA direct Video memory access
	mov	eax,31	; TRDOS 386 sysvideo system call
	int	40h
	cmp	eax,0B8000h
	jne	exit
	mov	edi,0B8000h
	
	mov	ax,0DCh
	mov	cx,80*25
	rep	stosw

	xor	esi,esi

    redraw_screen:
	mov	si,[cursor_y]
	imul	si,80
	add	si,[cursor_x]
	;and	byte [screen_data+si],8
	;or	byte [screen_data+si],2
	and	byte [screen_data+esi],8
	or	byte [screen_data+esi],2

	;mov	si,screen_data
	;xor	di,di
	;mov	cx,50
	mov	esi,screen_data
	mov	edi,0B8000h
	mov	ecx,50
      draw_screen:
	;push	cx
	;mov	cx,80
	push	ecx
	mov	cl,80
      draw_line:
	;mov	ah,[si+80]
	mov	ah,[esi+80]
	lodsb
	shl	al,4
	and	ah,0Fh
	or	al,ah
	;inc	di
	inc	edi
	stosb
	loop	draw_line
	;pop	cx
	pop	ecx
	add	si,80
	loop	draw_screen

    wait_for_key:
	xor	ah,ah
	;int	16h
	int	32h  ; TRDOS 386 KEYBOARD
	cmp	ah,1
	je	exit
	cmp	ah,1Ch
	je	next_generation
	cmp	ah,39h
	je	switch_cell
	cmp	ah,4Bh
	je	cursor_left
	cmp	ah,4Dh
	je	cursor_right
	cmp	ah,48h
	je	cursor_up
	cmp	ah,50h
	je	cursor_down
	jmp	wait_for_key

    switch_cell:
	mov	si,[cursor_y]
	imul	si,80
	add	si,[cursor_x]
	;xor	byte [screen_data+si],8
	xor	byte [screen_data+esi],8
	jmp	redraw_screen

    cursor_left:
	cmp	[cursor_x],1
	jbe	wait_for_key
	call	clear_cursor
	dec	[cursor_x]
	jmp	redraw_screen
    cursor_right:
	cmp	[cursor_x],78
	jae	wait_for_key
	call	clear_cursor
	inc	[cursor_x]
	jmp	redraw_screen
    cursor_up:
	cmp	[cursor_y],1
	jbe	wait_for_key
	call	clear_cursor
	dec	[cursor_y]
	jmp	redraw_screen
    cursor_down:
	cmp	[cursor_y],48
	jae	wait_for_key
	call	clear_cursor
	inc	[cursor_y]
	jmp	redraw_screen

    next_generation:
	call	clear_cursor
	;mov	si,screen_data+81
	;mov	di,screen_data+80*50+81
	;mov	cx,48
	mov	esi,screen_data+81
	mov	edi,screen_data+80*50+81
	mov	cl,48
      process_screen:
	;push	cx
	;mov	cx,78
	push	ecx
	mov	ecx,78
      process_line:
	xor	bl,bl
	;mov	al,[si+1]
	mov	al,[esi+1]
	and	al,1
	add	bl,al
	;mov	al,[si-1]
	mov	al,[esi-1]
	and	al,1
	add	bl,al
	;mov	al,[si+80]
	mov	al,[esi+80]
	and	al,1
	add	bl,al
	;mov	al,[si-80]
	mov	al,[esi-80]
	and	al,1
	add	bl,al
	;mov	al,[si+80+1]
	mov	al,[esi+80+1]
	and	al,1
	add	bl,al
	;mov	al,[si+80-1]
	mov	al,[esi+80+1]
	and	al,1
	add	bl,al
	;mov	al,[si-80+1]
	mov	al,[esi+80+1]
	and	al,1
	add	bl,al
	;mov	al,[si-80-1]
	mov	al,[esi+80-1]
	and	al,1
	add	bl,al
	;mov	al,byte [si]
	;mov	byte [di],al
	mov	al,byte [esi]
	mov	byte [edi],al
	cmp	bl,1
	jbe	clear_cell
	cmp	bl,4
	jae	clear_cell
	cmp	bl,2
	je	cell_ok
	;mov	byte [di],0Fh
	mov	byte [edi],0Fh
	jmp	cell_ok
      clear_cell:
	;mov	byte [di],0
	mov	byte [edi],0
      cell_ok:
	;inc	si
	;inc	di
	inc	esi
	inc	edi
	loop	process_line
	;pop	cx
	pop	ecx
	;add	si,2
	;add	di,2
	inc	esi
	inc	esi
	inc	edi
	inc	edi
	loop	process_screen
	;push	es ds
	;pop	es
	;mov	si,screen_data+80*50
	;mov	di,screen_data
	mov	esi,screen_data+80*50
	mov	edi,screen_data
	mov	cx,80*50
	rep	movsb
	;pop	es
	jmp	redraw_screen

    exit:
	mov	ax,3 ; set text mode again
	;int	10h
	int	31h ; TRDOS 386 VIDEO
	
	;int	20h
	;sys _exit
	sub	ebx,ebx ; 0
	mov	eax,1 ; sysexit
	int	40h

    clear_cursor:
	mov	si,[cursor_y]
	imul	si,80
	add	si,[cursor_x]
	;mov	al,byte [screen_data+si]
	mov	al,byte [screen_data+esi]
	cmp	al,2
	je	empty_cell
	;mov	byte [screen_data+si],0Fh
	mov	byte [screen_data+esi],0Fh
	ret
      empty_cell:
	;mov	byte [screen_data+si],0
	mov	byte [screen_data+esi],0
	ret

cursor_x dw 40
cursor_y dw 25

DCh_pattern:
 times 8 db 0
 times 8 db 0FFh

screen_data rb 80*50*2
