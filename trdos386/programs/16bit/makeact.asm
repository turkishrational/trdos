org 100h            ; COM dosyasý baþlangýç adresi

section .text

start:
    ; 1. Ekraný Mode 13h'e geçir (BIOS varsayýlan paleti RAM'e yüklesin)
    mov ax, 0013h
    int 10h

    ; 2. BIOS'tan donaným palet tablosunu oku
    ; Ýnt 10h, AX=1017h fonksiyonu 256 rengi ES:DX adresine yazar (Her renk 3 bayt, toplam 768 bayt)
    mov ax, 1017h
    xor bx, bx      ; Baþlangýç rengi = 0
    mov cx, 256     ; Okunacak renk sayýsý = 256
    mov dx, buffer  ; Verinin yazýlacaðý geçici bellek adresi
    push cs
    pop es          ; ES = CS (Segment eþitleme)
    int 10h

    ; 3. Normal metin moduna geri dön (Ekraný eski haline getirmek için)
    mov ax, 0003h
    int 10h

    ; 4. VGA 6-bit palet verisini Photoshop 8-bit ACT formatýna dönüþtür
    ; Her bir bayt deðerini 2 bit sola kaydýracaðýz ( value = value * 4 )
    mov cx, 768     ; Toplam iþlenecek bayt sayýsý (256 renk * 3 kanal)
    mov si, buffer  ; Okunacak ham 6-bit veri
    mov di, act_buf ; Yazýlacak 8-bit ACT verisi

convert_loop:
    lodsb           ; AL = [SI], SI++
    shl al, 2       ; 6-bit -> 8-bit (Sola 2 bit kaydýr)
    stosb           ; [DI] = AL, DI++
    loop convert_loop

    ; 5. MODE13H.ACT dosyasýný oluþtur
    mov ah, 3Ch     ; DOS dosya oluþturma fonksiyonu
    xor cx, cx      ; Normal dosya özniteliði (attribute)
    mov dx, filename
    int 21h
    jc error        ; Hata varsa çýkýþa git
    mov bx, ax      ; Dosya handle numarasýný BX'e aktar

    ; 6. Dönüþtürülmüþ 768 baytlýk ACT verisini dosyaya yaz
    mov ah, 40h     ; DOS dosyaya yazma fonksiyonu
    mov cx, 768     ; Yazýlacak bayt boyutu
    mov dx, act_buf ; Yazýlacak verinin adresi
    int 21h

    ; 7. Dosyayý kapat
    mov ah, 3Eh     ; DOS dosya kapatma fonksiyonu
    int 21h

error:
    ; 8. Programý sonlandýr ve DOS'a dön
    mov ax, 4C00h
    int 21h

section .data
    filename db "MODE13H.ACT", 0

section .bss
    buffer   resb 768    ; BIOS'tan gelen ham 6-bitlik palet için yer ayýr
    act_buf  resb 768    ; Dönüþtürülmüþ 8-bitlik ACT paleti için yer ayýr