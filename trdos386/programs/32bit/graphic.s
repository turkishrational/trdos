; bresenham's line algorithm
; written by Leonardo Ono (ono.leo@gmail.com)
; 26/09/2017
; target os: DOS (.COM file extension)
; ----------------------------------------------
; TRDOS 386 Adaptation: Erdogan Tan - 29/09/2024
; ----------------------------------------------

start_graphic_mode:
 	;xor ebx, ebx
 	mov bh, 5 ; Direct access/map to VGA memory
 	sys _video
	; eax = 0A0000h
	cmp eax, 0A0000h ; VGA memory address
	jne short exit_process ; error (eax = 0)

	; set video mode to 13h
	mov al, 13h 	; function 00h, mode 13h 
	int 31h ; TRDOS 386 - Video interrupt

	retn	

; ax = x1
; bx = y1
; ax = x2
; dx = y2
; di = color index
draw_line:
		mov word [.x1], ax
		mov word [.y1], bx
		mov word [.x2], cx
		mov word [.y2], dx
		sub cx, ax ; cx -> dx = x2 - x1
		sub dx, bx ; dx -> dy = y2 - y1
		mov word [.dx], cx
		mov word [.dy], dx
		cmp cx, 0
		jl short .dx_less
		
	.dx_greater:
		cmp dx, 0
		jge short .dx_greater_dy_greater
		;jl short .dx_greater_dy_less
		jmp short .dx_greater_dy_less
	.dx_less:
		cmp dx, 0
		jge .dx_less_dy_greater
		;jl short .dx_less_dy_less
		jmp .dx_less_dy_less
		
	.dx_greater_dy_greater:
		mov ax, [.dx]
		mov bx, [.dy]
		mov [draw_line_quadrant.dx], ax
		mov [draw_line_quadrant.dy], bx
		mov ax, [.x1]
		mov bx, [.y1]
		mov cx, [.x2]
		mov dx, [.y2]
		mov si, 0 ; quadrant 0
		jmp .continue
	.dx_greater_dy_less:
		mov ax, [.dy]
		neg ax
		mov bx, [.dx]
		mov [draw_line_quadrant.dx], ax
		mov [draw_line_quadrant.dy], bx
		mov ax, [.y1]
		neg ax
		mov bx, [.x1]
		mov cx, [.y2]
		neg cx
		mov dx, [.x2]
		mov si, 3 ; quadrant 3
		jmp .continue
	.dx_less_dy_greater:
		mov ax, [.dy]
		mov bx, [.dx]
		neg bx
		mov [draw_line_quadrant.dx], ax
		mov [draw_line_quadrant.dy], bx
		mov ax, [.y1]
		mov bx, [.x1]
		neg bx
		mov cx, [.y2]
		mov dx, [.x2]
		neg dx
		mov si, 1 ; quadrant 1
		jmp short .continue
	.dx_less_dy_less:
		mov ax, [.dx]
		neg ax
		mov bx, [.dy]
		neg bx
		mov [draw_line_quadrant.dx], ax
		mov [draw_line_quadrant.dy], bx
		mov ax, [.x1]
		neg ax
		mov bx, [.y1]
		neg bx
		mov cx, [.x2]
		neg cx
		mov dx, [.y2]
		neg dx
		mov si, 2 ; quadrant 2
		
	.continue:
		call draw_line_quadrant
		retn

		.x1 dw 0
		.y1 dw 0
		.x2 dw 0
		.y2 dw 0
		.dx dw 0
		.dy dw 0
		
; eax = x1
; ebx = y1
; ecx = x2
; edx = y2
; edi = color index
; esi = quadrant
draw_line_quadrant:
		add esi, esi
		push ecx
		push edx
		mov cx, word [.dx] ; ecx = dx
		mov dx, word [.dy] ; edx = dy
		cmp cx, dx
		jge short .not_swap
	.swap:
		pop edx
		pop ecx
		xchg eax, ebx
		xchg ecx, edx
		inc esi
		jmp short .continue
	.not_swap:
		pop edx
		pop ecx
	.continue:
		call draw_line_octant
		retn

	.dx dw 0
	.dy dw 0
		
; eax = x1
; ebx = y1
; ecx = x2
; edx = y2
; edi = color index
; esi = octant
draw_line_octant:
		mov word [.x2], cx
		sub cx, ax
		sub dx, bx
		add dx, dx 
		mov word [.2dy], dx
		sub dx, cx ; edx = d = 2 * dy - dx
		add cx, cx
		mov word [.2dx], cx
		; ebx = y = y1
		mov cx, ax ; ecx = x
		mov ax, di
	.next_point:
		call pset_octant
		cmp dx, 0
		jle short .d_less_or_equal
	.d_greater:
		add dx, word [.2dy]
		sub dx, word [.2dx]
		inc ebx
		jmp short .continue
	.d_less_or_equal:
		add dx, word [.2dy]
	.continue:
		inc ecx
		cmp cx, word [.x2]
		jbe short .next_point
		retn

		.x2 dw 0
		.2dx dw 0
		.2dy dw 0

; al = color index
; ebx = row
; ecx = col
; esi = octant
pset_octant:
		push ebx
		push ecx
		cmp si, 0
		jz short .octant_0
		cmp si, 1
		jz short .octant_1
		cmp si, 2
		jz short .octant_2
		cmp si, 3
		jz short .octant_3
		cmp si, 4
		jz  short .octant_4
		cmp si, 5
		jz  short .octant_5
		cmp si, 6
		jz  short .octant_6
		cmp si, 7
		jz  short .octant_7
	.octant_0:
		; do nothing
		jmp short .continue
	;.octant_1:
		;xchg bx, cx
		;jmp short .continue
	.octant_2:
		neg bx
	.octant_1:
		xchg bx, cx
		jmp short .continue
	.octant_3:
		neg cx
		jmp short .continue
	.octant_4:
		neg cx
		;neg bx
		;jmp short .continue
		jmp short .octant_7
	.octant_5:
		;neg cx
		neg bx
		;xchg bx, cx
		;jmp short .continue
	.octant_6:
		;neg cx
		xchg bx, cx
		;jmp short .continue
	.octant_7:
		neg bx
	.continue:
		call pset
		pop ecx
		pop ebx
		retn
		
; al = color index
; ebx = row
; ecx = col
pset:
	;push ebx
	push edx
	push eax
	mov eax, 320
	mul bx
	add ax, cx
	mov ebx, eax
	pop eax
	mov byte [ebx+0A0000h], al
	pop edx
	;pop ebx
	retn
	