; ============================================================================
; TRDOS 386 Flat Binary VESA VBE Mode 105h PS/2 Mouse Demo (LFB Method)
; ----------------------------------------------------------------------------
; Siyah zemin üzerine beyaz Microsoft tipi fare oku (Çizgili/Dolgulu).
; INT 34h IOCTL arayüzü ile port polling ve doğrudan LFB bellek erişimi.
; ============================================================================
; 04/10/2026 - Google AI

[bits 32]
[org 0x0]

; TRDOS 386 Sistem Çağrı Sabitleri
_exit 	equ 1
_video 	equ 31

START_CODE:
	; BSS Alanını temizle (Flat binary'de BSS başlangıcını sıfırlıyoruz)
	mov	edi, bss_start
	mov	ecx, (bss_end - bss_start)/4
	xor	eax, eax
	rep	stosd

	; 1. VESA Mode 105h Ayarla (1024x768, 256 Renk) ve LFB Adresini Al
	; TRDOS 386 sys _video, 08FFh fonksiyonu başarılıysa EDX = LFB Info addresidir.
        mov	edx, LFB_Info
	mov	ebx, 08FFh
	mov	ecx, 105h
	mov	eax, _video
	int	40h
	or	eax, eax
	jz	near terminate
	
	; Kernel'dan dönen LFB Info Structure'ındaki LFB_Adress kullanılacak

	; Kullanıcının video bellek adresini LFB adresine eşitle (haritala)
        mov	ebx, 0A02h  ; VIDEO MEMORY MAPPING (svga, LFB)
        mov	ecx, [LFB_Address]  ; Kullanıcının sanal adresi = LFB fiziksel adresi
 	mov	edx, 1024*768*1
	mov	eax, _video
	int	40h
	or	eax, eax
	jz	near terminate

		; 2. Siyah Arka Plan Oluştur (Bütün ekranı temizle)
	mov	edi, [LFB_Address] ; LFB_Info+2
	mov	ecx, (1024 * 768) / 4
	xor	eax, eax
	rep	stosd

	; 3. PS/2 Fare Donanımını Başlat (INT 34h IOCTL Port Erişimi ile)
	call	init_ps2_mouse

	; Fare Başlangıç Koordinatları (Ekran Ortası)
	mov	dword [mouse_x], 1024 / 2
	mov	dword [mouse_y], 768 / 2
	mov	dword [prev_mouse_x], 1024 / 2
	mov	dword [prev_mouse_y], 768 / 2

	; İlk konumdaki arka planı sakla ve ilk imleci çiz
	call	save_background
	call	draw_mouse_arrow
	mov	byte [mouse_drawn], 1

mouse_loop:
	; Klavye Kontrolü (ESC tuşuna basılırsa çık)
	mov	ah, 1
	int	32h
	jz	short .poll_mouse
	xor	ah, ah
	int	32h
	cmp	al, 1Bh			; ESC tuşu mu?
	je	terminate

.poll_mouse:
	; Fare Durum Kontrolü (Port 64h okuma)
	mov	dx, 0x64
	mov	ah, 0			; read byte
	int	34h
	test	al, 01h			; Output buffer dolu mu?
	jz	short mouse_loop	; Veri yoksa beklemeye devam et

	; Fare Veri Portunu Oku (Port 60h okuma)
	mov	dx, 0x60
	mov	ah, 0			; read byte
	int	34h

	; 3 Byte'lık PS/2 paketini tamamla
	xor	ebx, ebx
	mov	bl, [packet_count]
	mov	[mouse_packet + ebx], al
	inc	byte [packet_count]
	cmp	byte [packet_count], 3
	jne	short mouse_loop	; Paket henüz bitmediyse döngüye dön

	; Paket tamamlandı, sayacı sıfırla
	mov	byte [packet_count], 0

	; Koordinatları Güncelle (Delta X ve Delta Y analizi)
	call	update_position

	; Hareket var mı kontrol et?
	mov	eax, [mouse_x]
	mov	ebx, [mouse_y]
	cmp	eax, [prev_mouse_x]
	jne	short .update_screen
	cmp	ebx, [prev_mouse_y]
	je	short mouse_loop	; Hareket yoksa çizim yapma

.update_screen:
	; Eğer ekranda çizili bir imleç varsa önce onu sil (Arka planı geri yükle)
	cmp	byte [mouse_drawn], 1
	jne	short .draw_new
	call	delete_mouse_arrow

.draw_new:
	; Yeni koordinatları "geçtiğimiz konum" olarak güncelle
	mov	eax, [mouse_x]
	mov	ebx, [mouse_y]
	mov	[prev_mouse_x], eax
	mov	[prev_mouse_y], ebx

	; Yeni konumun altındaki zemini kaydet, ardından imleci çiz
	call	save_background
	call	draw_mouse_arrow
	mov	byte [mouse_drawn], 1

	jmp	mouse_loop

terminate:
	; Çıkmadan önce ekranı metin moduna (Mode 3) geri al
	xor    ah, ah
	mov    al, 3                        
	int    31h
	
	; TRDOS 386 Program Sonlandırma Çağrısı
	mov	eax, _exit
	int	40h

; ============================================================================
; SÜRÜCÜ VE YARDIMCI FONKSİYONLAR
; ============================================================================

init_ps2_mouse:
	; Fareyi Etkinleştir (Cmd 0xA8 -> Port 64h)
	mov	dx, 0x64
	mov	al, 0xA8
	mov	ah, 1 ; write byte
	int	34h

	; Komut Byte'ını Oku (Cmd 0x20 -> Port 64h)
	mov	al, 0x20
	mov	ah, 1
	int	34h
	call	wait_read
	mov	dx, 0x60
	mov	ah, 0
	int	34h
	or	al, 0x02		; IRQ 12'yi aktif et
	push	eax

	; Komut Byte'ını Yaz (Cmd 0x60 -> Port 64h)
	mov	dx, 0x64
	mov	al, 0x60
	mov	ah, 1
	int	34h
	pop	eax
	mov	dx, 0x60
	mov	ah, 1
	int	34h

	; Varsayılan Ayarları Yükle (0xF6 -> Fareye)
	call	write_mouse_cmd
	mov	al, 0xF6
	mov	ah, 1
	int	34h
	call	wait_ack

	; Veri Akışını Başlat (0xF4 -> Fareye)
	call	write_mouse_cmd
	mov	al, 0xF4
	mov	ah, 1
	int	34h
	call	wait_ack
	retn

write_mouse_cmd:
	mov	dx, 0x64
	mov	al, 0xD4		; Fareye komut yollanacağını bildirir
	mov	ah, 1
	int	34h
	mov	dx, 0x60
	retn

wait_read:
	mov	dx, 0x64
.l:	mov	ah, 0
	int	34h
	test	al, 01h
	jz	short .l
	retn

wait_ack:
	call	wait_read
	mov	dx, 0x60
	mov	ah, 0
	int	34h			; ACK (0xFA) değerini yut
	retn

update_position:
	mov	al, [mouse_packet]
	test	al, 0x40		; X taşma kontrolü
	jnz	.done
	test	al, 0x80		; Y taşma kontrolü
	jnz	.done

	; Delta X
	movzx	ebx, byte [mouse_packet + 1]
	test	byte [mouse_packet], 0x10 ; X işaret biti?
	jz	short .x_pos
	or	ebx, 0xFFFFFF00
.x_pos:
	add	[mouse_x], ebx

	; Delta Y (PS/2 dY verisi aşağıdan yukarı doğrudur, ekran ise yukarıdan aşağıya)
	movzx	ebx, byte [mouse_packet + 2]
	test	byte [mouse_packet], 0x20 ; Y işaret biti?
	jz	short .y_pos
	or	ebx, 0xFFFFFF00
.y_pos:
	sub	[mouse_y], ebx

	; Sınır Kontrolleri (Screen Clipping 1024x768)
	cmp	dword [mouse_x], 0
	jge	short .c1
	mov	dword [mouse_x], 0
.c1:	cmp	dword [mouse_x], 1012	; İmleç genişliğini düşerek kırpıyoruz (1024 - 12)
	jle	short .c2
	mov	dword [mouse_x], 1012
.c2:	cmp	dword [mouse_y], 0
	jge	short .c3
	mov	dword [mouse_y], 0
.c3:	cmp	dword [mouse_y], 749	; İmleç yüksekliğini düşerek kırpıyoruz (768 - 19)
	jle	short .done
	mov	dword [mouse_y], 749
.done:
	retn

; ============================================================================
; LFB GRAFİK EKRAN MOTORUFONKSİYONLARI
; ============================================================================

save_background:
	; İmlecin çizileceği alanın altındaki orijinal pikselleri yedekler
	mov	esi, [LFB_Address]
	mov	edi, back_buffer
	xor	ecx, ecx		; Y sayacı
.y_loop:
	push	ecx
	mov	eax, [prev_mouse_y]
	add	eax, ecx
	shl	eax, 10			; Y * 1024
	add	eax, [prev_mouse_x]	; + X
	lea	edx, [esi + eax]	; Doğrudan LFB üzerindeki adres
	
	mov	ecx, 12			; 12 piksel genişlik kopyala
.x_loop:
	movsb
	loop	.x_loop
	
	pop	ecx
	inc	ecx
	cmp	ecx, 19			; 19 satır yüksekliğinde
	jne	short .y_loop
	retn

delete_mouse_arrow:
	; Yedeklenen orijinal arka planı ekrana geri yazar (İmleci siler)
	mov	edi, [LFB_Address]
	mov	esi, back_buffer
	xor	ecx, ecx
.y_loop:
	push	ecx
	mov	eax, [prev_mouse_y]
	add	eax, ecx
	shl	eax, 10			; Y * 1024
	add	eax, [prev_mouse_x]
	lea	edi, [edi + eax]	; Hedef LFB adresi
	
	mov	ecx, 12
	rep	movsb			; 12 pikseli geri yaz
	
	mov	edi, [LFB_Address]	; EDI'yi sıfırla/tazele
	pop	ecx
	inc	ecx
	cmp	ecx, 19
	jne	short .y_loop
	mov	byte [mouse_drawn], 0
	retn

draw_mouse_arrow:
	; Fare okunu doğrudan LFB üzerine çizer
	mov	edi, [LFB_Address]
	mov	esi, mouse_arrow
	xor	ecx, ecx		; Y satır sayacı
.y_loop:
	push	ecx
	mov	eax, [prev_mouse_y]
	add	eax, ecx
	shl	eax, 10			; Y * 1024
	add	eax, [prev_mouse_x]
	lea	edi, [edi + eax]	; LFB hedef piksel adresi
	
	mov	ecx, 12			; Sütun sayacı
.x_loop:
	lodsb				; Maske byte'ını oku
	cmp	al, 0
	je	short .transparent	; 0 ise çizme (Şeffaf arka plan)
	
	; 256 renk paletinde renk belirleme:
	; 15 = Beyaz (İmleç içi dolgu)
	; 0  = Siyah (İmleç dış siyah çizgisi)
	; Kodumuzda şablon verisinde belirtilen renk değerini doğrudan basarız.
	mov	[edi], al		

.transparent:
	inc	edi
	loop	.x_loop
	
	mov	edi, [LFB_Address]	; EDI'yi sıfırla
	pop	ecx
	inc	ecx
	cmp	ecx, 19
	jne	short .y_loop
	retn

; ============================================================================
; MOUSE ARROW SPRITE DATA (12 x 19 piksel, 256 Renkli Ekran İçin)
; ============================================================================
; Sabit Değer Tanımlamaları (Ok okunabilirliğini artırmak için siyah kenarlıklı beyaz iç dolgu)
_W equ 15	; Beyaz Dolgu/Çizgi
_B equ 0	; Siyah Dış Hat Çizgisi
__ equ 0	; Transparan / Şeffaf Alan (LFB'ye yazılmaz)

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
	db _B, _W, _B, __, _B, _W, _W, _W, _B, __, __, __
	db _B, _B, __, __, _B, _W, _W, _W, _B, __, __, __
	db __, __, __, __, __, _B, _W, _W, _B, __, __, __
	db __, __, __, __, __, _B, _W, _W, _B, __, __, __
	db __, __, __, __, __, __, _B, _W, _B, __, __, __
	db __, __, __, __, __, __, _B, _W, _B, __, __, __
	db __, __, __, __, __, __, __, _B, __, __, __, __

; ============================================================================
; BSS - DİNAMİK BELLEK ALANI (FLAT BINARY SONRASI)
; ============================================================================
bss:
ABSOLUTE bss
alignb 4
bss_start:

LFB_Info:		resw 1	; Linear Frame Buffer Info Structure
LFB_Address:		resd 1
                        resb 10
mouse_x:		resd 1	; Güncel X koordinatı
mouse_y:		resd 1	; Güncel Y koordinatı
prev_mouse_x:		resd 1	; Bir önceki X koordinatı
prev_mouse_y:		resd 1	; Bir önceki Y koordinatı

packet_count:		resb 1	; Alınan fare byte sayacı
mouse_packet:		resb 4	; PS/2 ham veri tamponu
mouse_drawn:		resb 1	; İmleç ekrandaysa 1 olur

; İmlecin altındaki zemini yedeklemek için ayrılan alan (12 genişlik * 19 yükseklik)
back_buffer:		resb (12 * 19)

bss_end:
