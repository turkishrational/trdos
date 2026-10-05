; ****************************************************************************
; wmgr_mov.asm - TRDOS 386 Ring 3 VESA Mode 105h (1024x768) Taşınabilir Pencere Demosu
; ----------------------------------------------------------------------------
; Özellikler: Font Düzeltmesi Dahil, Buton ve Başlık Çubuğundan Sürükleme Aktif.
; Derleme Komutu: nasm wmgr_mov.asm -o WMGRMOV.PRG
; ****************************************************************************
; 04/10/2026 - Google AI desteğiyle yazıldı.
; 05/10/2026 - Google Gemini ("init_ps2_mouse")

_exit 	equ 1
_video 	equ 31
_audio	equ 32

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

; Buton Durumları
BTN_NORMAL   equ 0
BTN_PRESSED  equ 1

[BITS 32]
[ORG 0] 

START_CODE:
	; BSS Alanını Sıfırla
	mov	edi, bss_start
	mov	ecx, (bss_end - bss_start)/4
	xor	eax, eax
	rep	stosd

	; 1. VESA Mode 105h Aktif Et (1024x768, 256 Renk)
	mov	ebx, 08FFh
	mov	ecx, 105h
	mov	eax, _video
	int	40h
	or	eax, eax
	jz	near terminate_error

	; 2. Ring 3 Direct LFB Bellek Eşlemesini Al
	mov	ebx, 06FFh
	mov	eax, _video
	int	40h
	or	eax, eax
	jz	near terminate_error
	mov	[LFB_ADDR], eax

	; 3. GUI Başlangıç Konumlarını Kur
	mov	dword [win_x], 250
	mov	dword [win_y], 150
	mov	dword [win_w], 500
	mov	dword [win_h], 400
	call	recalc_button_relative_pos

	; 4. PS/2 Fareyi Başlat
	call	init_ps2_mouse

	; Fare Başlangıç Konumu
	mov	dword [mouse_x], 1024 / 2
	mov	dword [mouse_y], 768 / 2
	mov	dword [prev_mouse_x], 1024 / 2
	mov	dword [prev_mouse_y], 768 / 2

	; GUI Katmanını İlk Kez Çiz
	call	render_all_gui

	; Fare imlecini koru ve ekrana bas
	call	save_background
	call	draw_mouse_arrow
	mov	byte [mouse_drawn], 1

main_gui_loop:
	; === KERNEL SEVİYESİ KLAVYE / FARE ÇAKIŞMA ÇÖZÜMÜ (PORT POLLING) ===
	mov	dx, 0x64
	mov	ah, 0			; read byte
	int	34h			; AL = Status Register
	test	al, 01h			; Output buffer dolu mu? (Okunacak veri var mı?)
	jz	short main_gui_loop	; Veri yoksa döngüye devam et

	; Gelen veri fareye mi yoksa klavyeye mi ait?
	test	al, 20h			; Bit 5 = 1 ise Fare (Aux Device), 0 ise Klavye
	jz	.handle_keyboard_data	; Bit 5 sıfırsa klavye verisidir, el ile işle!

	; Veri Fareye ait, Port 0x60'tan fare paketini oku
	mov	dx, 0x60
	mov	ah, 0			; read byte
	int	34h

	cmp	byte [packet_count], 0
	jne	short .assemble_packet
	test	al, 08h
	jz	near main_gui_loop

.assemble_packet:
	xor	ebx, ebx
	mov	bl, [packet_count]
	mov	[mouse_packet + ebx], al
	inc	byte [packet_count]
	cmp	byte [packet_count], 3
	jne	near main_gui_loop
	mov	byte [packet_count], 0

	; Delta Sürükleme Hesaplaması İçin Eski Koordinatları Yedekle
	mov	eax, [mouse_x]
	mov	ebx, [mouse_y]
	mov	[old_mouse_x], eax
	mov	[old_mouse_y], ebx

	; Fare pozisyonunu güncelle
	call	update_mouse_position

	mov	al, [mouse_packet]
	and	al, 01h
	mov	[mouse_click], al

	; Etkinlik Yönlendirici (Sürükleme ve Tıklama Analizi)
	call	process_gui_events

	; İmleç Flip Döngüsü
	mov	eax, [mouse_x]
	mov	ebx, [mouse_y]
	cmp	eax, [prev_mouse_x]
	jne	short .redraw_frame
	cmp	ebx, [prev_mouse_y]
	je	near main_gui_loop

.redraw_frame:
	cmp	byte [mouse_drawn], 1
	jne	short .skip_erase
	call	delete_mouse_arrow
.skip_erase:
	mov	eax, [mouse_x]
	mov	ebx, [mouse_y]
	mov	[prev_mouse_x], eax
	mov	[prev_mouse_y], ebx

	call	save_background
	call	draw_mouse_arrow
	mov	byte [mouse_drawn], 1
	jmp	near main_gui_loop

.handle_keyboard_data:
	; Port 0x60'tan klavye scan kodunu oku ve tamponu rahatlat (EOI işlevini taklit eder)
	mov	dx, 0x60
	mov	ah, 0			; read byte
	int	34h			; AL = Scan Code (Klavye Donanım Kodu)

	cmp	al, 0x01		; 0x01 = ESC tuşu basılma (Make Code)
	je	short terminate		; ESC ise doğrudan çıkış yap

	cmp	al, 0x46		; 0x46 = Scroll Lock / CTRL+BREAK kontrolü için tarama
	; Buraya diğer kritik klavye kombinasyonlarını ekleyebilirsiniz.

	jmp	main_gui_loop

terminate:
	xor    ah, ah
	mov    al, 3                        
	int    31h
terminate_error:
	mov	eax, _exit
	int	40h

; ============================================================================
; GUI NESNE DINAMIK BAĞLANTI METOTLARI
; ============================================================================

recalc_button_relative_pos:
	; Butonun form içindeki relatif pozisyonunu (X+180, Y+200) kilitler
	mov	eax, [win_x]
	add	eax, 180
	mov	[btn_x], eax
	mov	eax, [win_y]
	add	eax, 200
	mov	[btn_y], eax
	mov	dword [btn_w], 140
	mov	dword [btn_h], 40
	retn

render_all_gui:
	; Masaüstü Zemini (Açık Gri: 7)
	mov	edi, [LFB_ADDR]
	mov	ecx, (1024 * 768) / 4
	mov	eax, 0x07070707
	rep	stosd

	; Pencere Gövdesi (Beyaz: 15)
	mov	eax, [win_x]
	mov	ebx, [win_y]
	mov	ecx, [win_w]
	mov	edx, [win_h]
	mov	si, 15
	call	draw_rect_flat

	; Siyah Dış Hat Çerçevesi
	mov	si, 0
	call	draw_rect_outline

	; Title Bar (Mavi: 9)
	mov	eax, [win_x]
	inc	eax
	mov	ebx, [win_y]
	inc	ebx
	mov	ecx, [win_w]
	sub	ecx, 2
	mov	edx, 24
	mov	si, 9
	call	draw_rect_flat

	; Menü Barı Zemini (Sarı: 14)
	mov	eax, [win_x]
	mov	ebx, [win_y]
	add	ebx, 25
	mov	ecx, [win_w]
	mov	edx, 20
	mov	si, 14
	call	draw_rect_flat
	
	; [DÜZELTME]: Menü Alanı Alt İzolasyon Çizgisi (Koyu Gri: 8)
	mov	eax, [win_x]
	mov	ebx, [win_y]
	add	ebx, 45
	mov	ecx, [win_w]
	mov	edx, 1
	mov	si, 8			; Kontrast için koyu gri ayraç eklendi
	call	draw_rect_flat

	; --- SABİTLENMİŞ DAHİLİ KERNEL FONT YAZIMLARI ---
	mov	eax, [win_x]
	add	eax, 10
	mov	ebx, [win_y]
	add	ebx, 5
	mov	edx, title_text
	mov	ch, 15			; Beyaz renk font
	call	draw_string

	mov	eax, [win_x]
	add	eax, 10
	mov	ebx, [win_y]
	add	ebx, 28
	mov	edx, menu_text
	mov	ch, 0			; Siyah renk font
	call	draw_string

	call	draw_gui_button
	retn

draw_gui_button:
	mov	eax, [btn_x]
	mov	ebx, [btn_y]
	mov	ecx, [btn_w]
	mov	edx, [btn_h]
	
	cmp	byte [btn_state], BTN_PRESSED
	je	short .pressed

	mov	si, 7
	call	draw_rect_flat
	mov	si, 0
	call	draw_rect_outline
	jmp	short .text

.pressed:
	mov	si, 8
	call	draw_rect_flat
	mov	si, 0
	call	draw_rect_outline

.text:
	mov	eax, [btn_x]
	add	eax, 30
	mov	ebx, [btn_y]
	add	ebx, 12
	mov	edx, btn_text
	mov	ch, 0			; Siyah renk font
	call	draw_string
	retn

; ============================================================================
; SÜRÜKLEME VE TIKLAMA REAKSİYON SÜZGECİ (EVENT ROUTER)
; ============================================================================

process_gui_events:
	mov	eax, [mouse_x]
	mov	ebx, [mouse_y]

	; 1. SÜRÜKLEME AKTİF Mİ KONTROL ET
	cmp	byte [is_dragging], 1
	je	.drag_window_active

	; 2. BUTON TIKLAMA KONTROLÜ
	cmp	eax, [btn_x]
	jl	.check_title_bar
	mov	edx, [btn_x]
	add	edx, [btn_w]
	cmp	eax, edx
	jg	short .check_title_bar
	cmp	ebx, [btn_y]
	jl	short .check_title_bar
	mov	edx, [btn_y]
	add	edx, [btn_h]
	cmp	ebx, edx
	jg	short .check_title_bar

	cmp	byte [mouse_click], 1
	je	short .clicked

	cmp	byte [btn_state], BTN_PRESSED
	jne	.no_change
	mov	byte [btn_state], BTN_NORMAL
	call	refresh_button_ui
	sys	_audio, 0000h, 4, 1331
	jmp	.no_change

.clicked:
	cmp	byte [btn_state], BTN_PRESSED
	je	.no_change
	mov	byte [btn_state], BTN_PRESSED
	call	refresh_button_ui
	jmp	.no_change

.check_title_bar:
	cmp	byte [btn_state], BTN_PRESSED
	jne	short .title_drag_check
	mov	byte [btn_state], BTN_NORMAL
	call	refresh_button_ui

.title_drag_check:
	; 3. TITLE BAR SÜRÜKLENME KONTROLÜ
	cmp	byte [mouse_click], 1
	jne	.drag_stop

	mov	eax, [mouse_x]
	mov	ebx, [mouse_y]

	cmp	eax, [win_x]
	jl	.no_change
	mov	edx, [win_x]
	add	edx, [win_w]
	cmp	eax, edx
	jg	.no_change

	cmp	ebx, [win_y]
	jl	.no_change
	mov	edx, [win_y]
	add	edx, 24
	cmp	ebx, edx
	jg	.no_change

	mov	byte [is_dragging], 1
	jmp	.no_change

.drag_window_active:
	cmp	byte [mouse_click], 1
	jne	.drag_stop

	; Fare hareketi (Delta) miktarınca pencereyi X ve Y eksenlerinde kaydır
	mov	eax, [mouse_x]
	sub	eax, [old_mouse_x]
	add	[win_x], eax

	mov	ebx, [mouse_y]
	sub	ebx, [old_mouse_y]
	add	[win_y], ebx

	; Ekran Sınır Kilidi (Kırpma / Overflow Filtresi)
	cmp	dword [win_x], 0
	jge	short .clip_w1
	mov	dword [win_x], 0
.clip_w1:
	cmp	dword [win_x], 524		; 1024 - 500 (Pencere genişliği)
	jle	short .clip_w2
	mov	dword [win_x], 524
.clip_w2:
	cmp	dword [win_y], 0
	jge	short .clip_w3
	mov	dword [win_y], 0
.clip_w3:
	cmp	dword [win_y], 368		; 768 - 400 (Pencere yüksekliği)
	jle	short .do_refresh
	mov	dword [win_y], 368

.do_refresh:
	call	recalc_button_relative_pos

	; Komple grafik sahneyi yeniden oluştur
	cmp	byte [mouse_drawn], 1
	jne	short .skp_dr
	call	delete_mouse_arrow
.skp_dr:
	call	render_all_gui
	call	save_background
	call	draw_mouse_arrow
	mov	byte [mouse_drawn], 1
	jmp	short .no_change

.drag_stop:
	mov	byte [is_dragging], 0

.no_change:
	retn

refresh_button_ui:
	cmp	byte [mouse_drawn], 1
	jne	short .skp
	call	delete_mouse_arrow
.skp:
	call	draw_gui_button
	call	save_background
	call	draw_mouse_arrow
	mov	byte [mouse_drawn], 1
	retn

; ============================================================================
; ALAN VE DOĞRUDAN DAHİLİ FONT BASMA SÜRÜCÜSÜ
; ============================================================================

draw_rect_flat:
	pushad
	mov	edi, [LFB_ADDR]
.y_l:	push	ecx
	push	eax
	mov	ebp, ebx
	shl	ebp, 10
	add	ebp, eax
	lea	edi, [edi + ebp]
	mov	eax, esi
	rep	stosb
	mov	edi, [LFB_ADDR]
	pop	eax
	pop	ecx
	inc	ebx
	dec	edx
	jnz	short .y_l
	popad
	retn

draw_rect_outline:
	pushad
	push	edx
	push	ecx
	mov	edx, 1
	call	draw_rect_flat
	pop	ecx
	pop	edx
	push	ebx
	push	edx
	add	ebx, edx
	dec	ebx
	mov	edx, 1
	call	draw_rect_flat
	pop	edx
	pop	ebx
	push	edx
	push	ecx
	mov	ecx, 1
	call	draw_rect_flat
	pop	ecx
	pop	edx
	add	eax, ecx
	dec	eax
	mov	ecx, 1
	call	draw_rect_flat
	popad
	retn

draw_string:
	; INPUT: EAX=X, EBX=Y, EDX=String Ptr, CH=Renk
	mov	[wcolor], ch
	mov	[screenpos_x], eax
	mov	[screenpos_y], ebx
	mov	ebp, edx
	
	pushad

.str_loop:
	movzx	edx, byte [ebp]		; DL = ASCII
	and	dl, dl
	jz	short .exit_loop

	; Boşluk (Space) karakteri öteleme köprüsü (Dolgu/Temizleme elendi)
	cmp	dl, 20h
	je	short .skip_render_block

.sys_render:
	movzx	ecx, byte [wcolor]	
	
	mov	esi, [screenpos_y]
	shl	esi, 16
	mov	si, [screenpos_x]

	mov	ebx, 020Fh		; BH = 02h (LFB), BL = 0Fh (Write Char)
	mov	dh, 00h			; DH = 00h (Dahili ROM Font, 8x16)
	mov	eax, _video
	int	40h			; Çekirdeği tetikle!

.skip_render_block:
	inc	ebp
	add	dword [screenpos_x], 8	; Column değerini 8 piksel kaydır
	jmp	short .str_loop

.exit_loop:
	popad
	retn

; ============================================================================
; FARE DONANIM KATMANI
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

update_mouse_position:
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
; DATA SAHASI
; ============================================================================

title_text: db "TRDOS 386 - Tasinabilir Pencere Demosu v1.0", 0
menu_text:  db " Dosya   Duzen   Gorunum   Araclar   Yardim", 0
btn_text:   db "OK (Event)", 0

mouse_arrow:
	db 0, 0, 238, 238, 238, 238, 238, 238, 238, 238, 238, 238
	db 0, 15, 0, 238, 238, 238, 238, 238, 238, 238, 238, 238
	db 0, 15, 15, 0, 238, 238, 238, 238, 238, 238, 238, 238
	db 0, 15, 15, 15, 0, 238, 238, 238, 238, 238, 238, 238
	db 0, 15, 15, 15, 15, 0, 238, 238, 238, 238, 238, 238
	db 0, 15, 15, 15, 15, 15, 0, 238, 238, 238, 238, 238
	db 0, 15, 15, 15, 15, 15, 15, 0, 238, 238, 238, 238
	db 0, 15, 15, 15, 15, 15, 15, 15, 0, 238, 238, 238
	db 0, 15, 15, 15, 15, 15, 15, 15, 15, 0, 238, 238
	db 0, 15, 15, 15, 15, 15, 15, 15, 15, 15, 0, 238
	db 0, 15, 15, 15, 15, 15, 15, 0, 0, 0, 0, 0
	db 0, 15, 15, 0, 15, 15, 15, 0, 238, 238, 238, 238
	db 0, 15, 0, 0, 15, 15, 15, 0, 238, 238, 238, 238
	db 0, 0, 238, 0, 0, 15, 15, 15, 0, 238, 238, 238
	db 238, 238, 238, 238, 0, 15, 15, 15, 0, 238, 238, 238
	db 238, 238, 238, 238, 0, 15, 15, 15, 15, 0, 238, 238
	db 238, 238, 238, 238, 238, 0, 15, 15, 0, 238, 238, 238
	db 238, 238, 238, 238, 238, 0, 15, 0, 238, 238, 238, 238
	db 238, 238, 238, 238, 238, 0, 0, 238, 238, 238, 238, 238

; ============================================================================
; BSS SECTION
; ============================================================================

bss:
ABSOLUTE bss

alignb 4

bss_start:
	LFB_ADDR:	resd 1
	mouse_x:	resd 1
	mouse_y:	resd 1
	prev_mouse_x:	resd 1
	prev_mouse_y:	resd 1
	old_mouse_x:	resd 1
	old_mouse_y:	resd 1
	packet_count:	resb 1
	mouse_packet:	resb 4
	mouse_drawn:	resb 1
	mouse_click:	resb 1
	wcolor:		resb 1
	is_dragging:	resb 1
	back_buffer:	resb (12 * 19)
	
        ; Dinamik Nesne Koordinat Haritası
	win_x:		resd 1
	win_y:		resd 1
	win_w:		resd 1
	win_h:		resd 1
	btn_x:		resd 1
	btn_y:		resd 1
	btn_w:		resd 1
	btn_h:		resd 1
	btn_state:	resb 1
	screenpos_x:	resd 1
	screenpos_y:	resd 1
bss_end:



