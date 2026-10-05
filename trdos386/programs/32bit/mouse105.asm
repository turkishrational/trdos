; ****************************************************************************
; mouse105.asm - TRDOS 386 Flat Binary VESA Mode 105h Direct LFB Mouse Demo
; ----------------------------------------------------------------------------
; 1024x768, 256 Renk (Mode 105h) çözünürlükte, Ring 3 Memory Mapping (Eşleme)
; yöntemi ile doğrudan LFB video belleğine yazan PS/2 fare uygulaması.
; ****************************************************************************
; Derleme Komutu: nasm mouse105.asm -o MOUSE105.PRG
; -------------------------------------------------
; Google AI - 04/10/2026
; 05/10/2026 - Google Gemini ("init_ps2_mouse")

_exit 	equ 1
_video 	equ 31

%macro sys 1-4
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
    int 40h
%endmacro

[BITS 32]
[ORG 0] 

START_CODE:
	; 1. BSS Alanını Sıfırla
	mov	edi, bss_start
	mov	ecx, (bss_end - bss_start)/4
	xor	eax, eax
	rep	stosd

	; 2. VESA Mode 105h Aktif Et
	sys	_video, 08FFh, 105h
	or	eax, eax
	jz	near terminate_error

	; 3. Ring 3 Bellek Eşlemesi (Memory Mapping)
	sys	_video, 06FFh
	or	eax, eax
	jz	near terminate_error
	mov	[LFB_ADDR], eax

	; 4. Ekranı Temizle (Siyah Zemin Yap)
	mov	edi, [LFB_ADDR]
	mov	ecx, (1024 * 768) / 4
	xor	eax, eax
	rep	stosd

	; 5. PS/2 Fare Donanımını Başlat
	call	init_ps2_mouse

	; Başlangıç Koordinatları
	mov	dword [mouse_x], 1024 / 2
	mov	dword [mouse_y], 768 / 2
	mov	dword [prev_mouse_x], 1024 / 2
	mov	dword [prev_mouse_y], 768 / 2

	call	save_background
	call	draw_mouse_arrow
	mov	byte [mouse_drawn], 1

mouse_loop:
	; === KERNEL SEVİYESİ KLAVYE / FARE ÇAKIŞMA ÇÖZÜMÜ (PORT POLLING) ===
	mov	dx, 0x64
	mov	ah, 0			; read byte
	int	34h			; AL = Status Register
	test	al, 01h			; Output buffer dolu mu? (Okunacak veri var mı?)
	jz	short mouse_loop	; Veri yoksa döngüye devam et

	; Gelen veri fareye mi yoksa klavyeye mi ait?
	test	al, 20h			; Bit 5 = 1 ise Fare (Aux Device), 0 ise Klavye
	jz	.handle_keyboard_data	; Bit 5 sıfırsa klavye verisidir, el ile işle!

	; Veri Fareye ait, Port 0x60'tan fare paketini oku
	mov	dx, 0x60
	mov	ah, 0			; read byte
	int	34h

	; 3 Byte'lık PS/2 Paketini Birleştir
	xor	ebx, ebx
	mov	bl, [packet_count]
	mov	[mouse_packet + ebx], al
	inc	byte [packet_count]
	cmp	byte [packet_count], 3
	jne	short mouse_loop

	mov	byte [packet_count], 0	; Sayacı sıfırla

	; Koordinat Güncelleme
	call	update_position

	; Fare hareket etti mi?
	mov	eax, [mouse_x]
	mov	ebx, [mouse_y]
	cmp	eax, [prev_mouse_x]
	jne	short .update_screen
	cmp	ebx, [prev_mouse_y]
	je	short mouse_loop

.update_screen:
	cmp	byte [mouse_drawn], 1
	jne	short .draw_new
	call	delete_mouse_arrow

.draw_new:
	mov	eax, [mouse_x]
	mov	ebx, [mouse_y]
	mov	[prev_mouse_x], eax
	mov	[prev_mouse_y], ebx

	call	save_background
	call	draw_mouse_arrow
	mov	byte [mouse_drawn], 1
	jmp	mouse_loop

.handle_keyboard_data:
	; Port 0x60'tan klavye scan kodunu oku ve tamponu rahatlat (EOI işlevini taklit eder)
	mov	dx, 0x60
	mov	ah, 0			; read byte
	int	34h			; AL = Scan Code (Klavye Donanım Kodu)

	cmp	al, 0x01		; 0x01 = ESC tuşu basılma (Make Code)
	je	short terminate		; ESC ise doğrudan çıkış yap

	cmp	al, 0x46		; 0x46 = Scroll Lock / CTRL+BREAK kontrolü için tarama
        je	short terminate

	; Buraya diğer kritik klavye kombinasyonlarını ekleyebilirsiniz.

	jmp	mouse_loop

terminate:
	xor    ah, ah
	mov    al, 3
	int    31h
terminate_error:
	sys	_exit

; ============================================================================
; FARE BAŞLATMA VE PROTOKOL FONKSİYONLARI (KORUNMUŞTUR)
; ============================================================================

init_ps2_mouse:
    ; 1. Fare portunu (Auxiliary Device) etkinleştir
    mov     dx, 0x64
    mov     al, 0xA8
    mov     ah, 1
    int     34h

    ; 2. 8042 Kontrolcü Konfigürasyon Baytını Oku
    mov     dx, 0x64
    mov     al, 0x20
    mov     ah, 1
    int     34h
    call    wait_read
    
    mov     dx, 0x60
    mov     ah, 0
    int     34h             ; AL = Mevcut Config Baytı
    
    ; 3. Klavye IRQ (Bit 0), Fare IRQ (Bit 1) ve Translation (Bit 6) aktif et
    or      al, 0x03        ; Klavye ve Fare kesmelerini aç
    or      al, 0x40        ; Scancode translation aktif et (Bit 6)
    push    eax

    ; 4. Yeni Konfigürasyon Baytını Geri Yaz
    mov     dx, 0x64
    mov     al, 0x60
    mov     ah, 1
    int     34h
    
    pop     eax
    mov     dx, 0x60
    mov     ah, 1
    int     34h

    ; 5. FAREYE RESET AT (0xFF) - (En Kritik Eksik Adım)
    call    write_mouse_cmd
    mov     al, 0xFF        ; Reset Mouse
    mov     ah, 1
    int     34h
    call    wait_ack        ; ACK (0xFA) beklet
    call    wait_read       ; Self-Test sonucu (0xAA) ve Device ID okuyup temizle
    mov     dx, 0x60
    mov     ah, 0
    int     34h
    
    call    wait_read       ; İkinci byte (genellikle Device ID 0x00)
    mov     dx, 0x60
    mov     ah, 0
    int     34h

    ; 6. Varsayılan Ayarları Yükle (Set Defaults - 0xF6)
    call    write_mouse_cmd
    mov     al, 0xF6
    mov     ah, 1
    int     34h
    call    wait_ack

    ; 7. Fare Veri Akışını Başlat (Enable Data Reporting - 0xF4)
    call    write_mouse_cmd
    mov     al, 0xF4
    mov     ah, 1
    int     34h
    call    wait_ack

    ; 8. Klavye Portunu ve Kesmesini Garantiye Al
    mov     dx, 0x64
    mov     al, 0xAE        ; Enable Keyboard Port
    mov     ah, 1
    int     34h

    retn

write_mouse_cmd:
    mov     dx, 0x64
    mov     al, 0xD4        ; Sonraki verinin fareye gideceğini bildir
    mov     ah, 1
    int     34h
    mov     dx, 0x60        ; Veri portu
    retn

wait_read:
    mov     dx, 0x64
.l: mov     ah, 0
    int     34h
    test    al, 01h         ; Output buffer dolu mu?
    jz      short .l
    retn

wait_ack:
    call    wait_read
    mov     dx, 0x60
    mov     ah, 0
    int     34h             ; Gelen ACK (0xFA) değerini oku
    retn

update_position:
	mov	al, [mouse_packet]
	test	al, 0x40
	jnz	.done
	test	al, 0x80
	jnz	.done

	movzx	ebx, byte [mouse_packet + 1]
	test	byte [mouse_packet], 0x10
	jz	short .x_pos
	or	ebx, 0xFFFFFF00
.x_pos:
	add	[mouse_x], ebx

	movzx	ebx, byte [mouse_packet + 2]
	test	byte [mouse_packet], 0x20
	jz	short .y_pos
	or	ebx, 0xFFFFFF00
.y_pos:
	sub	[mouse_y], ebx

	cmp	dword [mouse_x], 0
	jge	short .c1
	mov	dword [mouse_x], 0
.c1:	cmp	dword [mouse_x], 1012
	jle	short .c2
	mov	dword [mouse_x], 1012
.c2:	cmp	dword [mouse_y], 0
	jge	short .c3
	mov	dword [mouse_y], 0
.c3:	cmp	dword [mouse_y], 749
	jle	short .done
	mov	dword [mouse_y], 749
.done:
	retn

save_background:
	mov	esi, [LFB_ADDR]
	mov	edi, back_buffer
	xor	ecx, ecx
.y_loop:
	push	ecx
	mov	eax, [prev_mouse_y]
	add	eax, ecx
	shl	eax, 10
	add	eax, [prev_mouse_x]
	lea	edx, [esi + eax]
	push	esi
	mov	esi, edx
	mov	ecx, 12
	rep	movsb
	pop	esi
	pop	ecx
	inc	ecx
	cmp	ecx, 19
	jne	short .y_loop
	retn

delete_mouse_arrow:
	mov	edi, [LFB_ADDR]
	mov	esi, back_buffer
	xor	ecx, ecx
.y_loop:
	push	ecx
	mov	eax, [prev_mouse_y]
	add	eax, ecx
	shl	eax, 10
	add	eax, [prev_mouse_x]
	lea	edi, [edi + eax]
	mov	ecx, 12
	rep	movsb
	mov	edi, [LFB_ADDR]
	pop	ecx
	inc	ecx
	cmp	ecx, 19
	jne	short .y_loop
	mov	byte [mouse_drawn], 0
	retn

draw_mouse_arrow:
	mov	edi, [LFB_ADDR]
	mov	esi, mouse_arrow
	xor	ecx, ecx
.y_loop:
	push	ecx
	mov	eax, [prev_mouse_y]
	add	eax, ecx
	shl	eax, 10
	add	eax, [prev_mouse_x]
	lea	edi, [edi + eax]
	mov	ecx, 12
.x_loop:
	lodsb
	cmp	al, 0xEE
	je	short .transparent
	mov	[edi], al
.transparent:
	inc	edi
	loop	short .x_loop
	mov	edi, [LFB_ADDR]
	pop	ecx
	inc	ecx
	cmp	ecx, 19
	jne	short .y_loop
	retn

; ============================================================================
; SPRITE VE BSS ALANI
; ============================================================================
_W equ 15
_B equ 0
__ equ 0xEE

mouse_arrow:
	db _B, _B, __, __, __, __, __, __, __, __, __, __
	db _B, _W, _B, __, __, __, __, __, __, __, __, __
	db _B, _W, _W, _B, __, __, __, __, __, __, __, __
	db _B, _W, _W, _W, _B, __, __, __, __, __, __, __
	db _B, _W, _W, _W, _W, _B, __, __, __, __, __, __
	db _B, _W, _W, _W, _W, _W, _B, __, __, __, __, __
	db _B, _W, _W, _W, _W, _W, _W, _B, __, __, __, __
	db _B, _W, _W, _W, _W, _W, _W, _W, _B, __, __, __
	db _B, _W, _W, _W, _W, _W, _W, _W, _W, _B, __, __
	db _B, _W, _W, _W, _W, _W, _W, _W, _W, _W, _B, __
	db _B, _W, _W, _W, _W, _W, _W, _B, _B, _B, _B, _B
	db _B, _W, _W, _B, _W, _W, _W, _B, __, __, __, __
	db _B, _W, _B, _B, _W, _W, _W, _B, __, __, __, __
	db _B, _B, __, _B, _B, _W, _W, _W, _B, __, __, __
	db __, __, __, __, _B, _W, _W, _W, _B, __, __, __
	db __, __, __, __, _B, _W, _W, _W, _W, _B, __, __
	db __, __, __, __, __, _B, _W, _W, _B, __, __, __
	db __, __, __, __, __, _B, _W, _B, __, __, __, __
	db __, __, __, __, __, _B, _B, __, __, __, __, __

bss:
ABSOLUTE bss
alignb 4
bss_start:
LFB_ADDR:		resd 1
mouse_x:		resd 1
mouse_y:		resd 1
prev_mouse_x:		resd 1
prev_mouse_y:		resd 1
packet_count:		resb 1
mouse_packet:		resb 4
mouse_drawn:		resb 1
back_buffer:		resb (12 * 19)
bss_end:

