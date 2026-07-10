; ****************************************************************************
; gwd.s (TRDOS 386, TRDOS v2.0.11 - CWD Verification Program, 'gwd.prg')
; ----------------------------------------------------------------------------
; GWD.PRG ! 'sys_dir' (28) and 'sys_msg' (35) Test Program for TRDOS 386 !
;
; 09/07/2026 - Erdogan Tan & Google AI Collaborator
; ****************************************************************************
; Last Update: 10/07/2026
; nasm gwd.s -l gwd.txt -o GWD.PRG

; TRDOS 386 Sistem Çağrı Numaraları (System Call Vectors)
_exit 	equ 1
_dir	equ 28
_msg    equ 35

; TRDOS 386 Kesme Makrosu Tasarımı (Sırtta gereksiz taş taşımama ilkesi)
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
    int 40h ; TRDOS 386 Yerel Çekirdek Kesmesi		   
%endmacro

[BITS 32]   ; 32-bit Korumalı Mod Talimatları

[ORG 0]     ; Kod doğrudan 0. bayttan başlar, kayma riski sıfırdır!

START_CODE:
	; Program açılış mesajını ekrana mühürle
	mov	esi, msg_program
	call	print_msg

	; Tampon alanlarımızı sıfırlayarak RAM'deki çöp bayt kalıntılarını yok ediyoruz
	mov	edi, bss_start
	mov	ecx, (bss_end-bss_start)/4
	xor	eax, eax
	rep	stosd

	; sys _dir (28) çağrısı ile o anki çalışma dizinini tampona çekiyoruz
	sys	_dir, dir_buffer
	jc	short sysdir_failed

	; Sürücü harfini ve iki nokta üst üste işaretini mühürle
	add	al, 'A'             ; Harfe dönüştür
	mov	ah, ':'
	mov	edi, output_str     ; Yazılacak hedef
	stosw                       ; 'D:'

	; Kernel'ın doldurduğu tamponu çözüp output_str arkasına ekliyoruz
	mov	esi, dir_buffer     ; Okunacak tampon adresi
	
	; Dizin başlangıcına mutlaka bir '/' separator çak
	mov	al, '/'
	stosb

	cmp	byte [esi], 0
	jz	short close_output_str

parse_cwd_path:
do_parse:
	lodsb			    ; Tampondan 1 bayt oku (AL'ye al)
write_char:
	and	al, al              ; Null-terminator (\0) mü?
	jz	short close_output_str
	stosb			    ; String arkasına ekle
	jmp	short do_parse

close_output_str:
	; Dizgiyi sıfır ile bitir
	mov	byte [edi], 0
	; Toplam karakter uzunluğunu hesapla (edi - output_str)
	sub	edi, output_str

	; sys _msg (35) çağrısı ile nihai CWD yolunu ekrana bas
	sys	_msg, output_str, edi, 0000000Eh
	jmp	short terminate

sysdir_failed:
	mov	esi, msg_failed
	call	print_msg

terminate:
	mov	esi, nextline
	call	print_msg

	; _exit (1) kesmesi ile kabuğa (shell) firesiz geri dönüş
	sys 	_exit

; Ekrana TTY formatında mesaj basma alt rutini (dirlfn2 standardı)
print_msg:
	mov	ebx, 0000000Fh      ; Beyaz karakterler (bl), Video Sayfası 0 (bh)
_print_loop:
	mov	ah, 0Eh             ; Teletype output kesme fonksiyonu
	lodsb
	and	al, al
	jz	short _print_done
	int	31h                 ; Video kesmesi
	jmp	short _print_loop
_print_done:
	retn

; ----------------------------------------------------------------------------
; Statik Mesaj Tanımlamaları (DATA)
; ----------------------------------------------------------------------------
msg_program:
	db 0Dh, 0Ah
	db "GWD.PRG /// TRDOS 386 sys_dir (28) Canli Dogrulama Araci"
	db 0Dh, 0Ah, 0
msg_failed:
	db "Hata: sys_dir sistem cagrisi basarisiz oldu!", 0Dh, 0Ah, 0
nextline:
	db 0Dh, 0Ah, 0

; ----------------------------------------------------------------------------
; Dinamik Bellek Alanları (BSS)
; ----------------------------------------------------------------------------
bss:
ABSOLUTE bss

alignb 4

bss_start:
                      ; 7 seviyede 12 byte noktalı dizin adı, 6 adet '/', 1 adet sıfır
                      ; kullanılmayan 1 byte ve 'D:/' ve 0 (dword hizalama) -v2.0.11-
;dir_buffer: resb 96  ; Kernel kuralları uyarınca ayrılan 92 baytlık CWD alanı
                      ; 8 seviyede 12 byte noktalı dizin adı, 7 adet '/', 1 adet sıfır
                      ; ve 'D:/' ve 0 (dword hizalama) -v2.1.0-			
dir_buffer: resb 108  ; TRDOS 386 v2.1.0 (104 byte tampon sınırı = 8*12+7+1+4)
output_str: resb 128 ; Ekrana basılacak olan "D:/TCC\r\n\0" formatındaki nihai string
bss_end:
