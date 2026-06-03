; =========================================================================
; BMP TO RAW BINARY CONVERTER (MS-DOS / RETRO DOS - 16-BIT REAL MODE)
; Amac: 256 renk, sikistirilmamis BMP dosyasinin sadece saf piksel verisini
;       okuyup basliksiz (raw) .BIN dosyasina kaydeder.
; Derleme: nasm -f bin -o bmp2raw.com bmp2raw.asm
; =========================================================================

org 100h

start:
    ; 1. Kaynak BMP Dosyasini Ac (sys_open)
    mov ah, 3Dh                 ; DOS Open File function
    mov al, 0                   ; Read-only mode
    mov dx, input_file          ; DS:DX -> Dosya adi
    int 21h
    jc file_error
    mov [input_handle], ax

    ; 2. Hedef RAW Dosyasini Olustur (sys_create / sys_open)
    mov ah, 3Ch                 ; DOS Create File function
    mov cx, 0                   ; Normal dosya özniteligi
    mov dx, output_file         ; DS:DX -> Çikis dosya adi
    int 21h
    jc file_error
    mov [output_handle], ax

    ; 3. BMP Header'i Atla (54 Byte)
    mov ah, 3Fh                 ; DOS Read File function
    mov bx, [input_handle]
    mov cx, 54                  ; 54 byte header
    mov dx, scratch_buffer      ; Geçici bellek alanina oku
    int 21h
    jc read_error

    ; 4. BMP Renk Paletini Atla (1024 Byte)
    ; Not: Palet verisini VGA kartina yuklemek isterseniz burayi saklayabilirsiniz.
    mov ah, 3Fh
    mov bx, [input_handle]
    mov cx, 1024                ; 256 renk * 4 byte (RGBA)
    mov dx, scratch_buffer
    int 21h
    jc read_error

read_write_loop:
    ; 5. Saf Piksel Verisini Bloklar Halinde Oku ve Yaz
    mov ah, 3Fh
    mov bx, [input_handle]
    mov cx, 2048                ; 2 KB'lik bloklar halinde transfer
    mov dx, scratch_buffer
    int 21h
    jc read_error
    
    and ax, ax                  ; AX = Okunan byte sayisi. 0 ise EOF (Dosya bitti)
    jz conversion_done
    
    mov [bytes_read], ax        ; Okunan miktari sakla

    ; 6. Okunan Blogu Hedef Dosyaya Yaz (sys_write)
    mov ah, 40h                 ; DOS Write File function
    mov bx, [output_handle]
    mov cx, [bytes_read]        ; Okundugu kadar yaz
    mov dx, scratch_buffer
    int 21h
    jc write_error
    jmp read_write_loop         ; Dosya bitene kadar devam et

conversion_done:
    ; 7. Dosyalari Kapat (sys_close)
    mov ah, 3Eh                 ; DOS Close File
    mov bx, [input_handle]
    int 21h

    mov ah, 3Eh
    mov bx, [output_handle]
    int 21h

    ; Ekrana basari mesaji bas
    mov ah, 09h
    mov dx, msg_success
    int 21h
    jmp exit_program

file_error:
    mov dx, msg_err_file
    jmp print_error
read_error:
    mov dx, msg_err_read
    jmp print_error
write_error:
    mov dx, msg_err_write
print_error:
    mov ah, 09h
    int 21h

exit_program:
    mov ax, 4C00h               ; DOS Terminate Process (sys_exit)
    int 21h

; =========================================================================
; DATA SEGMENT
; =========================================================================

input_file      db "CAR_R.BMP", 0      ; Dönüstürülecek 256 renk BMP görseli
output_file     db "CAR_R.BIN", 0      ; Olusacak saf raw piksel dosyasi

msg_success     db "Basariyla donusturuldu: CAR_R.BIN", 0Dh, 0Ah, "$"
msg_err_file    db "Hata: Dosya acilamadi veya olusturulamadi!", 0Dh, 0Ah, "$"
msg_err_read    db "Hata: BMP okunurken hata olustu!", 0Dh, 0Ah, "$"
msg_err_write   db "Hata: RAW yazilirken hata olustu!", 0Dh, 0Ah, "$"

; =========================================================================
; BSS / UNINITIALIZED DATA
; =========================================================================

bss:

ABSOLUTE bss

alignb 2

input_handle:   resw 1
output_handle:  resw 1
bytes_read: 	resw 1

alignb 4

scratch_buffer: resb 2048              ; Disk okuma/yazma tamponu