; =======================================================================
; TRDOS 386 - Native Static Library Archiver (AR.PRG)
; Date: 16/07/2026 - Developer: Erdogan Tan & Google AI
; Format: NASM (Pure Flat 32-bit Protected Mode Binary)
; Standard: UNIX/ELF Static Archive (.a) Generator
; =======================================================================

[BITS 32]
[ORG 0]

; -----------------------------------------------------------------------
; TRDOS 386 System Call Interface Constants
; -----------------------------------------------------------------------
SYS_EXIT    equ 1
SYS_OPEN    equ 5
SYS_CLOSE   equ 6
SYS_READ    equ 3
SYS_WRITE   equ 4
SYS_SEEK    equ 19
SYS_CREAT   equ 8
SYS_MSG     equ 35

; =======================================================================
; CODE SECTION
; =======================================================================
SECTION .text

global _start
_start:
    ; TRDOS 386 Stack Architecture on Program Entry:
    ; [esp]   = argc
    ; [esp+4] = argv[0] (Program Name: AR.PRG)
    ; [esp+8] = argv[1] (Output Archive Name: libc.a)
    ; [esp+12]= argv[2] (First Object Module: printf.o)
    ; [esp+16]= argv[3] (Second Object Module: string.o) ...

    ; 1. ADIM: BSS Alanını Temizle (Erdoğan Tan Standart Rutini)
    ;sub eax, eax ; 0
    mov edi, bss_start
    mov ecx, (bss_end - bss_start)/4
    rep stosd
    
    ; 2. ADIM: Komut Satırı Argümanlarını Tarama (2016 Mimariniz)
    mov esi, esp
    lodsd                   ; EAX = Toplam argc sayısı
    mov [argc], eax
    cmp eax, 3              ; En az: ar libc.a printf.o (argc >= 3)
    jge .continue_parse
    
    ; Yetersiz argüman hatası bas ve çık
    mov esi, msg_usage
    call print_string
    mov eax, SYS_EXIT
    int 40h

.continue_parse:
    lodsd                   ; 2. LODSD: EAX = argv[0] pointer ("AR.PRG" kendi adını atla)

    lodsd                   ; 3. LODSD: EAX = argv[1] pointer (Hedef kütüphane adı, örn: "libc.a")

    ; TRDOS SYS_CREAT Kuralı: Dosya adı string adresi EBX'te olmalı!
    mov ebx, eax            ; ebx = Doğrudan "libc.a" adresi (Köşeli parantez YOK)
    mov eax, SYS_CREAT
    mov ecx, 0              ; Normal dosya özniteliği
    int 40h
    jc .err_create
    mov [out_fd], eax

    ; 3. ADIM: Global Arşiv Sihirli Numarasını Yaz ("!<arch>\n")
    mov eax, SYS_WRITE
    mov ebx, [out_fd]
    mov ecx, msg_magic
    mov edx, 8              ; Sihirli numara tam 8 byte
    int 40h
    jc .err_write

    ; ESI şu an tam argv[2] (yani ilk nesne dosyası: *printf.o) adresinde bekliyor!
    mov dword [current_idx], 2

.loop_objects:
    mov eax, [argc]
    cmp [current_idx], eax  ; Tüm dosyalar bitti mi?
    jge .close_and_exit

    lodsd                   ; Döngüsel LODSD: EAX = Sıradaki .o dosyasının adresi
    mov [current_obj_name], eax

    ; TRDOS SYS_OPEN Kuralı: Dosya adı string adresi EBX'te olmalı!
    mov ebx, eax            ; ebx = "printf.o" adresi (Direkt adres!)
    mov eax, SYS_OPEN
    mov ecx, 0              ; Open for read
    int 40h
    jc .err_open_obj
    mov [in_fd], eax

    ; Girdi dosyasının boyutunu bulmak için sona seek at
    mov eax, SYS_SEEK
    mov ebx, [in_fd]
    xor ecx, ecx            ; offset = 0
    mov edx, 2              ; SEEK_END = 2
    int 40h
    mov [obj_size], eax     ; eax dosya boyutunu döner

    ; Dosya göstericisini tekrar başa al
    mov eax, SYS_SEEK
    mov ebx, [in_fd]
    xor ecx, ecx
    mov edx, 0              ; SEEK_SET = 0
    int 40h

    ; 4. ADIM: Temiz UNIX/ELF 60-byte Arşiv Başlığı Hazırla
    call prepare_ar_header

    ; Hazırlanan 60 byte başlığı arşiv dosyasına yaz
    mov eax, SYS_WRITE
    mov ebx, [out_fd]
    mov ecx, ar_header
    mov edx, 60
    int 40h
    jc .err_write

    ; 5. ADIM: Nesne Kodunun İçeriğini Arşive Kopyala
.read_write_loop:
    mov eax, SYS_READ
    mov ebx, [in_fd]
    mov ecx, io_buffer
    mov edx, 4096           ; 4KB chunk'lar halinde oku
    int 40h
    or eax, eax
    jz .object_done         ; EOF ulaştık
    jc .err_read

    mov edx, eax            ; edx = okunan byte miktarı
    mov eax, SYS_WRITE
    mov ebx, [out_fd]
    mov ecx, io_buffer
    int 40h
    jc .err_write
    jmp .read_write_loop

.object_done:
    mov eax, SYS_CLOSE
    mov ebx, [in_fd]
    int 40h

    ; UNIX Standardı: Eğer nesne boyutu tek sayı ise, araya boşluk ('\n') ekle
    mov eax, [obj_size]
    test eax, 1
    jz .next_object
    
    mov eax, SYS_WRITE
    mov ebx, [out_fd]
    mov ecx, msg_pad
    mov edx, 1
    int 40h
    jc .err_write

.next_object:
    inc dword [current_idx]
    jmp .loop_objects

.close_and_exit:
    mov eax, SYS_CLOSE
    mov ebx, [out_fd]
    int 40h

    mov esi, msg_success
    call print_string
    xor ebx, ebx            ; Exit code = 0
    mov eax, SYS_EXIT
    int 40h

; -----------------------------------------------------------------------
; Hata Bildirim Alanları
; -----------------------------------------------------------------------
.err_create:
    mov esi, err_msg_create
    jmp .die
.err_write:
    mov esi, err_msg_write
    jmp .die
.err_open_obj:
    mov esi, err_msg_open
    jmp .die
.err_read:
    mov esi, err_msg_read
.die:
    call print_string
    mov ebx, 1              ; Exit code = 1
    mov eax, SYS_EXIT
    int 40h

; -----------------------------------------------------------------------
; Alt Rutin: 60-byte UNIX Standart Arşiv Başlığı Hazırlama
; -----------------------------------------------------------------------
prepare_ar_header:
    pusha
    
    mov edi, ar_header
    mov ecx, 60
    mov al, ' '
    rep stosb

    ; 1. Dosya Adı (16 byte) - Sona '/' eklenmeli
    mov esi, [current_obj_name]
    mov edi, ar_header
    mov ecx, 15             ; Sınır
.copy_name:
    lodsb
    or al, al
    jz .name_done
    cmp al, ' '
    je .name_done
    stosb
    loop .copy_name
.name_done:
    mov byte [edi], '/'

    ; 2. Zaman Damgası (12 byte)
    mov edi, ar_header + 16
    mov esi, static_date
    mov ecx, 10
    rep movsb

    ; 3. Owner & Group ID (6 + 6 byte)
    mov byte [ar_header + 28], '0'
    mov byte [ar_header + 34], '0'

    ; 4. Dosya İzin Maskesi (8 byte)
    mov edi, ar_header + 40
    mov esi, static_mode
    mov ecx, 6
    rep movsb

    ; 5. Dosya Boyutu Metin Dönüşümü (10 byte)
    mov eax, [obj_size]
    mov edi, ar_header + 48
    add edi, 9
    mov ecx, 10
    mov ebx, 10
.num_to_ascii:
    xor edx, edx
    div ebx
    add dl, '0'
    mov [edi], dl
    dec edi
    or eax, eax
    jz .ascii_done
    loop .num_to_ascii
.ascii_done:

    ; 6. Arşiv Başlığı Kapanış İmzası (2 byte) - "`\n"
    mov byte [ar_header + 58], 0x60
    mov byte [ar_header + 59], 0x0A

    popa
    ret

; -----------------------------------------------------------------------
; Yardımcı Fonksiyon: TRDOS 386 Ekrana String Basma
; -----------------------------------------------------------------------
print_string:
    ;pusha
    mov eax, SYS_MSG
    mov	ebx, esi
    mov ecx, 255 ; string limit (max. possible sasciiz string length)
    mov edx, 7              ; Beyaz renk
    int 40h
    ;popa
    ret

; =======================================================================
; DATA & BSS SECTIONS (Erdoğan Tan Bellek Yönetimi)
; =======================================================================
SECTION .data
msg_magic         db "!<arch>"
msg_pad           db 0x0A
static_date       db "1784157100"
static_mode       db "100644"

msg_usage         db "TRDOS 386 Archiver v1.0", 0x0D, 0x0A
                  db "Usage: AR <archive.a> <file1.o> [file2.o] ...", 0x0D, 0x0A, 0

msg_success       db "[OK] Static ELF archive library created successfully.", 0x0D, 0x0A, 0
err_msg_create    db "[ERROR] Could not create destination archive file!", 0x0D, 0x0A, 0
err_msg_write     db "[ERROR] Write error occurred on target device!", 0x0D, 0x0A, 0
err_msg_open      db "[ERROR] Target object module file not found!", 0x0D, 0x0A, 0
err_msg_read      db "[ERROR] Read failure inside object code cluster!", 0x0D, 0x0A, 0

SECTION .bss
bss_start:
argc              resd 1
current_idx       resd 1
out_fd            resd 1
in_fd             resd 1
obj_size          resd 1
current_obj_name  resd 1

ar_header         resb 60
io_buffer         resb 4096
bss_end: