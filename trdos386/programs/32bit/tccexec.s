; ****************************************************************************
; tccexec.s - TRDOS 386 - TCC FLAT PRG EXECUTION & EXIT CODE MONITOR
; ----------------------------------------------------------------------------
; Erdogan Tan & Google AI - 08/07/2026 [Tam Dinamik Dosya Adı Güncellemesi]
; ****************************************************************************

; TRDOS 386 Sistem Çağrı Sabitleri
_exit 	equ 1
_fork 	equ 2
_wait 	equ 7
_exec	equ 11
_msg    equ 35

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
	; BSS Alanını Temizle (Orijinal Taslak Mantığı)
	mov	di, bss_start
	mov	ecx, (bss_end - bss_start)/4
	rep	stosd

GetCommandLineArgs:
	mov	esi, esp
	lodsd				; EAX = Toplam argc sayısı
	cmp	eax, 2			; En az iki argüman olmalı: "tccexec <hedef_program>"
	jb	pmsg_usage		; Eğer hedef dosya adı girilmediyse kullanım kılavuzuna zıpla

	dec	eax
	mov	[argc], al		; Kalan toplam argüman sayısını sakla
	lodsd				; "tccexec" kendi adını atla
	
	; 1. ADIM: Komut satırındaki ilk gerçek argümanı prgfilename'e kopyala (Hedef Program)
	lodsd				; EAX = Çalıştırılacak program adının adresi
	mov	ebp, esi		; Kalan argüman pointer dizisinin yerini EBP'de sakla
	mov	esi, eax
	mov	edi, prgfilename

.scan_target_name:
	lodsb
	cmp	al, 20h			; Boşluk kontrolü
	je	short .scan_target_name
	jb	pmsg_usage
	stosb

.copy_target_name:
	lodsb
	cmp	al, 20h
	jna	short .target_name_done
	stosb
	jmp	short .copy_target_name

.target_name_done:
	xor	al, al
	stosb				; Hedef dosya adını NULL ile sonlandır

	; 2. ADIM: Geriye kalan argümanlar varsa (argv[2] ve sonrası) tek bir string olarak topla
	mov	edi, arguments		; Hedef arabelleği hazırla
	dec	byte [argc]
	jz	short .all_args_done	; Başka argüman yoksa bitir

.argument_loop:
	mov	esi, [ebp]		; Sıradaki argüman kelimesinin adresi
	add	ebp, 4

.copy_chars:
	lodsb
	cmp	al, 20h			; Boşluk veya altı kontrolü
	jna	short .word_done
	stosb				; Karakteri 'arguments' tamponuna yaz
	jmp	short .copy_chars

.word_done:
	dec	byte [argc]		; İşlenecek kelime kaldı mı?
	jz	short .all_args_done
	
	mov	al, 20h			; Kelimelerin arasına boşluk (Space) yerleştir
	stosb	
	jmp	short .argument_loop

.all_args_done:
	xor	al, al
	stosb				; Argüman string akışını NULL ile sonlandır

.launch_pipeline:
	; Başlangıç Monitör Başlığını Bas (Kırmızı Renk - 0Ch)
	mov	byte [color], 0Ch
	mov 	esi, tcc_monitor_hdr
	call 	print_msg

	; Süreci Çatalla (Fork)
	mov 	ebx, child_launcher
	sys 	_fork
	jc 	short execution_error

	mov	[cpid], eax 		; Alt sürecin ID'sini sakla

	; Parent (İzleme) bekleme döngüsüne giriyor
	mov	byte [color], 07h
parent_wait:
	mov	ebx, 999 		; Bloklanan wait çağrısı
	sys 	_wait
	jnc	short wait_success

execution_error:
	mov 	esi, msg_err
	call 	print_msg
	sys	_exit

wait_success:
	cmp 	eax, [cpid]		; Doğru çocuk süreç mi kontrol et
	jne 	short parent_wait	

	push	ebx  			; Çıkış kodunu yığına güvenle at

	; Başarı mesajlarını ve dinamik dosya adını yazdır (Yeşil/Sarı)
	mov	byte [color], 0Ah
	mov	esi, child_label
	call	print_msg
	mov	byte [color], 0Eh
	mov	esi, prgfilename	; Statik değil, artık dinamik basıyor!
	call	print_msg	

	mov	byte [color], 07h
	mov	esi, CRLF
	call	print_msg

	; Yığından exit code değerini geri al ve ASCII'ye çevir
	pop	eax
	call 	bin_to_decimal_str
	mov 	[exitcode], ebx

	mov	byte [color], 0Fh 		; Parlak Beyaz
	mov 	esi, child_exitcode_msg
	call 	print_msg

monitor_ok:
	mov	byte [color], 0Ah
	mov 	esi, msg_ok
	call 	print_msg
	sys	_exit

pmsg_usage:
	mov	byte [color], 0Eh
	sys	_msg, msg_usage, 255, [color]
	sys	_exit

; ============================================================================
; CHILD LAUNCHER - ALT SÜREÇ TETİKLEYİCİSİ
; ============================================================================
child_launcher:
	; Dinamik olarak yakalanan prgfilename ve arguments dizisini EXEC et!
	sys	_exec, prgfilename, prgp
	jnc	short child_exit_direct

	; Eğer dosya bulunamadıysa disk hatası bas
	cmp	al, 2 
	jne	execution_error

	mov	byte [color], 0Ch
	mov	esi, prgfilename
	call	print_msg
	mov	esi, not_found
	call	print_msg

child_fail_exit:
	mov	eax, 0xFF
child_exit_direct:
	mov	ebx, eax
	sys 	_exit

; ============================================================================
; YARDIMCI FONKSİYONLAR
; ============================================================================
print_msg:
	sys 	_msg, esi, 255, [color]
	retn

bin_to_decimal_str:
	mov	ecx, 10
	xor	ebx, ebx
btd_loop:
	xor	edx, edx
  	div	ecx
	shl	ebx, 8
	add	dl, '0'
	mov	bl, dl
	or	eax, eax
	jnz	short btd_loop
	retn

; ============================================================================
; VERİ VE TANIMLAMALAR BÖLGESİ
; ============================================================================
align 4
color:		dd 0
cpid:		dd 0

; Sizin exectest.s dosyanızdaki birebir prgp işaretçi hizalaması!
align 4
prgp:		dd prgfilename
		dd arguments
		dd 0

; İzleme Ekran İletileri
tcc_monitor_hdr:
	db 0Dh, 0Ah
	db 'TRDOS 386 v2 - TCC Flat PRG Exit Code Monitor'
	db 0Dh, 0Ah, 0

child_label:
	db 0Dh, 0Ah
	db "Executed Program: ", 0

not_found:
	db " not found on disk! ", 0Dh, 0Ah, 0

child_exitcode_msg:
	db '>>> TCC MAIN() RETURN VALUE (EXIT CODE): '
exitcode:
	dd 30303030h  
	db 0Dh, 0Ah, 0

msg_err:
	db 0Dh, 0Ah 
	db 'Execution Error!'
	db 0Dh, 0Ah, 0

msg_ok:
	db 0Dh, 0Ah
	db 'Monitor Finished Successfully. ', 0Dh, 0Ah, 0

msg_usage:
	db 0Dh, 0Ah
	db 'Usage: tccexec <target program file name> <arguments>'
	db 0Dh, 0Ah, 0

CRLF:
	db 0Dh, 0Ah, 0

; ============================================================================
; UNINITIALIZED DATA - BAŞLANGIÇSIZ VERİ ALANI (BSS)
; ============================================================================
bss_start:
ABSOLUTE bss_start

argc:		resb 1
		resb 3

prgfilename:	resb 80  ; İlk parametreden dinamik doldurulan hedef dosya adı arabelleği
arguments:	resb 128 ; Diğer parametrelerin birleştirildiği arabellek

bss_end:

