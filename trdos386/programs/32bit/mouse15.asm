; ****************************************************************************
; mouse15.asm - TRDOS 386 Ring 3 VESA Mode 105h PS/2 Mouse Demo
; ----------------------------------------------------------------------------
; NASM ile derleme komutu:
; nasm mouse15.asm -o MOUSE15.PRG
; ****************************************************************************
; 04/10/2026 - Google AI

; TRDOS 386 (v2.0) Sistem Çağrı Sabitleri
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
    int 40h ; TRDOS 386 Sistem Kesmesi
%endmacro

[BITS 32]
[ORG 0] 

START_CODE:
	; BSS Alanını Sıfırla
	mov	edi, bss_start
	mov	ecx, (bss_end - bss_start)/4
	xor	eax, eax
	rep	stosd

	; 1. VESA Mode 105h Ayarla (1024x768, 256 Renk)
	sys	_video, 08FFh, 105h
	or	eax, eax
	jz	near terminate

	; 2. PS/2 Fare Donanımını Başlat (INT 34h IOCTL ile Port Erişimi)
	call	init_ps2_mouse

	; Fare Başlangıç Koordinatları (Ekran Ortası)
	mov	dword [mouse_x], 1024 / 2
	mov	dword [mouse_y], 768 / 2

main_loop:
	; Klavye Kontrolü (ESC tuşuna basılırsa çık)
	mov	ah, 1
	int	32h	; TRDOS 386 Klavye Durum Kesmesi
	jz	short check_mouse
	xor	ah, ah
	int	32h	; Karakteri Oku
	cmp	al, 1Bh	; ESC Key
	je	short terminate

check_mouse:
	; Fare verisi hazır mı kontrol et (Port 64h okuma)
	call	mouse_status
	test	al, 01h			; Çıkış tamponu dolu mu?
	jz	short render_mouse	; Veri yoksa çizime geç

	test	al, 02h			; Fare verisi mi (Auxiliary Device)?
	; Eğer sisteminizde klavye/fare ayrımı katıysa kontrolü aktif edebilirsiniz.
	
	; Fare paketini oku (3 Byte)
	call	read_mouse_packet
	cmp	byte [packet_count], 3
	jne	short render_mouse

	; Paket tamamlandıysa koordinatları güncelle
	call	update_mouse_position
	mov	byte [packet_count], 0	; Sayacı sıfırla

render_mouse:
	; Sadece fare hareket ettiyse ekrandaki görüntüyü güncelle
	mov	eax, [mouse_x]
	mov	ebx, [mouse_y]
	cmp	eax, [prev_mouse_x]
	jne	short redraw
	cmp	ebx, [prev_mouse_y]
	je	short delay_loop	; Hareket yoksa döngüye devam et

redraw:
	; 1. Eski imleci temizle (Siyah renkle üzerine çizerek)
	call	erase_old_mouse
	
	; 2. Yeni koordinatları kaydet
	mov	eax, [mouse_x]
	mov	ebx, [mouse_y]
	mov	[prev_mouse_x], eax
	mov	[prev_mouse_y], ebx

	; 3. Yeni imleci beyaz renkle çiz
	call	draw_new_mouse

delay_loop:
	; İşlemciyi çok yormamak için kısa bir döngü gecikmesi veya NOP
	mov	ecx, 50000
.wait:
	nop
	loop	.wait
	jmp	main_loop

terminate:
	; Metin moduna geri dön ve çık
	xor    ah, ah
	mov    al, 3                        
	int    31h ; TRDOS Video Kesmesi
	sys	_exit

; ============================================================================
; PS/2 FARE SÜRÜCÜ FONKSİYONLARI (INT 34h IOCTL KULLANIMI)
; ============================================================================

init_ps2_mouse:
	; Fareyi Etkinleştir (Command: 0xA8 to Port 64h)
	mov	dx, 0x64
	mov	al, 0xA8
	call	io_write_byte

	; Fare Komut Bloğunu Aktif Et (Command: 0x20 to Port 64h)
	mov	al, 0x20
	call	io_write_byte
	call	mouse_wait_read
	mov	dx, 0x60
	call	io_read_byte
	or	al, 0x02		; Enable IRQ12 (Mouse)
	push	eax
	
	; Güncellenmiş Komutu Geri Yaz (Command: 0x60 to Port 64h)
	mov	dx, 0x64
	mov	al, 0x60
	call	io_write_byte
	pop	eax
	mov	dx, 0x60
	call	io_write_byte

	; Varsayılan Fare Ayarlarını Yükle (0xF6 to Port 60h via 64h/A4h wrapper if needed)
	mov	al, 0xD4
	mov	dx, 0x64
	call	io_write_byte
	mov	al, 0xF6		; Set default settings
	mov	dx, 0x60
	call	io_write_byte
	call	mouse_ack

	; Veri Akışını Başlat (0xF4 to Port 60h)
	mov	al, 0xD4
	mov	dx, 0x64
	call	io_write_byte
	mov	al, 0xF4		; Enable data reporting
	mov	dx, 0x60
	call	io_write_byte
	call	mouse_ack
	retn

mouse_status:
	mov	dx, 0x64
	call	io_read_byte
	retn

mouse_wait_read:
	mov	dx, 0x64
.wait:
	call	io_read_byte
	test	al, 01h
	jz	short .wait
	retn

mouse_ack:
	call	mouse_wait_read
	mov	dx, 0x60
	call	io_read_byte	; ACK (0xFA) oku ve temizle
	retn

read_mouse_packet:
	mov	dx, 0x64
	call	io_read_byte
	test	al, 01h			; Veri var mı?
	jz	short .no_data
	
	mov	dx, 0x60
	call	io_read_byte
	
	xor	ebx, ebx
	mov	bl, [packet_count]
	mov	[mouse_raw_packet + ebx], al
	inc	byte [packet_count]
.no_data:
	retn

update_mouse_position:
	; Paket 0: Butonlar ve İşaret Bitleri
	; Paket 1: Delta X
	; Paket 2: Delta Y
	mov	al, [mouse_raw_packet]
	test	al, 0x40		; X Taşma (Overflow) kontrolü
	jnz	.skip_x
	test	al, 0x80		; Y Taşma (Overflow) kontrolü
	jnz	.skip_x

	; Delta X Güncelleme
	movzx	ebx, byte [mouse_raw_packet + 1]
	test	byte [mouse_raw_packet], 0x10 ; X Sign bit?
	jz	short .pos_x
	or	ebx, 0xFFFFFF00		; Negatif yap
.pos_x:
	add	[mouse_x], ebx

	; Delta Y Güncelleme (PS/2 yönü ile ekran yönü terstir)
	movzx	ebx, byte [mouse_raw_packet + 2]
	test	byte [mouse_raw_packet], 0x20 ; Y Sign bit?
	jz	short .pos_y
	or	ebx, 0xFFFFFF00
.pos_y:
	sub	[mouse_y], ebx		; Ekranda yukarı gitmesi için çıkarıyoruz

	; Ekran Sınır Kontrolleri (Clip to 1024x768)
.clip_bounds:
	cmp	dword [mouse_x], 0
	jge	short .check_max_x
	mov	dword [mouse_x], 0
.check_max_x:
	cmp	dword [mouse_x], 1023
	jle	short .check_min_y
	mov	dword [mouse_x], 1023

.check_min_y:
	cmp	dword [mouse_y], 0
	jge	short .check_max_y
	mov	dword [mouse_y], 0
.check_max_y:
	cmp	dword [mouse_y], 767
	jle	short .skip_x
	mov	dword [mouse_y], 767
.skip_x:
	retn

; ============================================================================
; INT 34h IOCTL PORT ERİŞİM WRAPPER FONKSİYONLARI
; ============================================================================
io_read_byte:
	; DX = Port
	; Output: AL = Data
	mov	ah, 0
	int	34h
	retn

io_write_byte:
	; DX = Port, AL = Data
	mov	ah, 1
	int	34h
	retn

; ============================================================================
; GÖRSEL RENDER FONKSİYONLARI (Gönderdiğiniz sys _video, 0305h mantığı)
; ============================================================================

draw_new_mouse:
	mov	byte [draw_color], 15	; Beyaz Renk (VGA Standart Palette)
	call	build_and_send_mouse
	retn

erase_old_mouse:
	mov	byte [draw_color], 0	; Siyah Renk (Arka Plan Rengi)
	call	build_and_send_mouse
	retn

build_and_send_mouse:
	; İmlecin çizileceği başlangıç ofsetlerini hesapla ve piksel dizisini hazırla
	mov	eax, pixel_array
	mov	[pixel_ptr], eax
	xor	ecx, ecx		; satır sayacı (Y)

.line_loop:
	push	ecx
	mov	eax, [prev_mouse_y]
	add	eax, ecx		; Mevcut ekran Y koordinatı
	cmp	eax, 768
	jnb	short .next_line	; Ekran dışındaysa satırı atla

	; Satırın bit maskesini yükle
	mov	bl, [mouse_sprite + ecx]
	xor	edx, edx		; sütun sayacı (X)

.pixel_loop:
	shl	bl, 1
	jnc	short .skip_pixel	; Bit 0 ise piksel koyma (Saydam)

	mov	ebp, [prev_mouse_x]
	add	ebp, edx		; Mevcut ekran X koordinatı
	cmp	ebp, 1024
	jnb	short .skip_pixel

	; Ekran Ofsetini Hesapla: (Y * 1024) + X
	push	edx
	push	eax
	mov	edx, 1024
	mul	edx
	add	eax, ebp
	
	; Ofseti piksel arabelleğine kaydet
	mov	edi, [pixel_ptr]
	stosd
	mov	[pixel_ptr], edi
	pop	eax
	pop	edx

.skip_pixel:
	inc	edx
	cmp	edx, 8			; Ok genişliği 8 piksel
	jne	short .pixel_loop

.next_line:
	pop	ecx
	inc	ecx
	cmp	ecx, 12			; Ok yüksekliği 12 piksel
	jne	short .line_loop

	; Hazırlanan Göreceli Piksel Array'ini Kernel'a Gönder
	mov	esi, pixel_array
	mov	edx, [pixel_ptr]
	sub	edx, esi
	shr	edx, 2			; Piksel Sayısı = Toplam Byte / 4
	jz	short .done		; Çizilecek piksel yoksa atla
	
	sys	_video, 0305h, [draw_color] ; Kernel Üzerinden Toplu Çizim Çağrısı

.done:
	retn

; ============================================================================
; SABİT VERİLER VE SPRITE TANIMLAMALARI
; ============================================================================

; Microsoft Tipi Klasik Mouse Oku Grafiği (12 Satır, 8 Bit Genişlik)
; 1 = Beyaz Piksel, 0 = Şeffaf / Siyah Arka Plan
mouse_sprite:
	db 10000000b
	db 11000000b
	db 11100000b
	db 11110000b
	db 11111000b
	db 11111100b
	db 11111110b
	db 11111000b
	db 11011000b
	db 10001100b
	db 00001100b
	db 00000000b

; ============================================================================
; BSS - DİNAMİK DEĞİŞKEN ALANI
; ============================================================================
bss:
ABSOLUTE bss
alignb 4
bss_start:

mouse_x:		resd 1
mouse_y:		resd 1
prev_mouse_x:		resd 1
prev_mouse_y:		resd 1

packet_count:		resb 1
mouse_raw_packet:	resb 4
draw_color:		resb 1
                        resb 3
pixel_ptr:		resd 1

; En fazla 12*8 = 96 piksel pozisyonu saklayacak array (Her biri dword)
pixel_array:		resd 128

bss_end:

