; =========================================================================
; PSEUDO 3D ROAD ENGINE - TRDOS 386 INTEL PROTECTED MODE PORT (FLAT BINARY)
; Uyumluluk: TRDOS 386 v2.0.11+, 32-bit Paging, Flat Memory Model (Ring 3)
; Derleme: nasm racer386.s -l racer386.txt -o RACER386.PRG
; =========================================================================
; 31/05/2026 - Lotus Style Pure Physics & Keyboard Priority Calibration

bits 32                         ; Saf 32-bit instruction set üretimi zorunlu kýlýnýyor
org 0x0                         ; TRDOS flat binary baþlangýç adresi (0)

start:
    ; DIRECT VGA MEMORY ACCESS
    mov bh, 5                   ; Direct access/map to VGA memory (0A0000h)
    mov al, 1Fh                 ; sys _video ; TRDOS 386 Video functions
    int 40h                     ; TRDOS 386 system call

    and eax, eax
    jnz short set_mode_13h
    mov ebx, -1 
    mov al, 1                   ; sysexit
    int 40h

set_mode_13h:
    mov ax, 13h
    int 31h

    ; Sanal Arka Plan Belleði Tahsisi
    mov dword [vram_buffer_ptr], vram_sanal_bellek
    mov dword [player_x], 0     ; Baþlangýç konumu yolun ortasý
    mov dword [player_speed], 0 ; Baþlangýç hýzý sýfýr
    mov byte [scancode], 0

main_game_loop:
    ; --- 1. DONANIMSAL VSYNC KÝLÝDÝ ---
    mov dx, 03DAh
.wait_low:
    int 34h
    test al, 8
    jnz .wait_low
.wait_high:
    int 34h
    test al, 8
    jz .wait_high

    ; --- 2. KLAVYE TAMPON OKUMA (TEK SEFERDE AL REGISTER'INA ALIM) ---
    mov dx, 60h
    xor eax, eax                ; AH=0 (Port okuma modu)
    int 34h                     ; AL = Basýlan tuþun Scan Code'u
    mov [scancode], al          ; BSS'deki tampona kaydet

    ; --- 3. KLAVYE ÖNCELÝKLÝ YÖN VE DÝREKSÝYON MATRÝSÝ ---
    mov esi, 0                  ; ESI: Sprite yönü (0=DÜZ, 1=SOL, 2=SAÐ)
    mov ecx, 2                  ; Sabit, sarsmayan direksiyon dönüþ adýmý
    mov edi, 0                  ; EDI: Gaz basýlma bayraðý (0=Býrakýldý, 1=Basýlýyor)

    ; ATALET KÝLÝDÝ
    ; Kullanýcý Sol veya Sað ok tuþuna basarken Yukarý ok (Gaz) tuþunu býrakmýþ gibi 
    ; algýlanmasýný önlemek için mantýksal gaz köprülemesi kuruyoruz.
    cmp al, 4Bh                 ; Sol Ok Tuþu Scan Code
    jne .check_right
    sub dword [player_x], ecx   
    mov esi, 1                  ; SOL sprite aktif
    mov edi, 1                  ; [MANTIKSAL GAZ]: Dönüþ anýnda gazý açýk tut!
    jmp .check_throttle

.check_right:
    cmp al, 4Dh                 ; Sað Ok Tuþu Scan Code
    jne .check_throttle
    add dword [player_x], ecx   
    mov esi, 2                  ; SAÐ sprite aktif
    mov edi, 1                  ; [MANTIKSAL GAZ]: Dönüþ anýnda gazý açýk tut!
    jmp .check_throttle

    ; --- 4. GAZ / FREN / SÜRTÜNME VE ATALET MOTORU ---
.check_throttle:
    ; Eðer kullanýcý doðrudan Yukarý Ok tuþuna basýyorsa da gazý aktif et
    cmp al, 48h                 ; Yukarý Ok Tuþu (GAZ)
    je .gas_active
    
    ; Eðer Sað/Sol ok tuþlarýndan dolayý EDI=1 olmuþsa gazý yine aktif say!
    cmp edi, 1
    je .gas_active
    jmp .check_brake            ; Ýkisi de yoksa fren/sürtünme kontrolüne git

.gas_active:
    mov eax, [player_speed]
    cmp dword [on_grass], 1
    jne .asphalt_speed_limit
    
    ; Çimen hýz limiti
    cmp eax, 12                 
    jge .physics_update
    add dword [player_speed], 1 
    jmp .physics_update

.asphalt_speed_limit:
    cmp eax, 55                 ; Maksimum asfalttaki hýz sýnýrý
    jge .physics_update
    
    cmp eax, 25
    jge .low_torque
    add dword [player_speed], 2 
    jmp .physics_update
.low_torque:
    add dword [player_speed], 1
    jmp .physics_update

.check_brake:
    cmp al, 50h                 ; Aþaðý Ok Tuþu (FREN)
    jne .apply_inertia
    sub dword [player_speed], 3 
    jge .physics_update
    mov dword [player_speed], 0
    jmp .physics_update

.apply_inertia:
    ; Gerçek rölanti yavaþlamasý (Hiçbir tuþa basýlmadýðýnda)
    mov eax, [player_speed]
    or eax, eax                 
    jz .physics_update

    cmp dword [on_grass], 1
    je .grass_friction_fast     

    ; Lotus Akýþ Ataleti: Gaz tamamen býrakýldýðýnda araba çok tatlý yavaþlar (8 karede 1 birim)
    mov ebx, [time_tick]
    and ebx, 7                  ; Yavaþlamayý 8 karede bire yayarak ataleti köklüyoruz
    jnz .physics_update         
    
    cmp eax, 4
    jle .physics_update         
    dec dword [player_speed]
    jmp .physics_update

.grass_friction_fast:
    sub dword [player_speed], 2 
    jge .physics_update
    mov dword [player_speed], 0

    ; --- 5. FÝZÝK GÜNCELLEME VE AGRESÝF MERKEZLEME KÝLÝDÝ ---
.physics_update:
    ; Sýnýr korumalarý
    cmp dword [player_x], -110
    jge .limit_left_ok
    mov dword [player_x], -110   
.limit_left_ok:
    cmp dword [player_x], 110
    jle .limit_right_ok
    mov dword [player_x], 110    
.limit_right_ok:

    ; Yol ilerleme adýmý
    mov eax, [player_speed]
    add [player_z], eax         

    ; Viraj bükülme frekansý (Ufuktaki tatlý salýným ayarý)
    mov eax, [player_speed]
    shr eax, 5                  
    add [time_tick], eax        

    mov eax, [time_tick]
    shr eax, 3                  
    and eax, 63
    mov [sine_index], eax

    ; Viraj merkezkaç kuvveti
    mov edi, [sine_index]
    movsx eax, word [sine_table + edi * 2]
    sar eax, 4                  
    imul eax, [player_speed]
    sar eax, 8                  
    sub [player_x], eax         

    ; --- KESÝNTÝSÝZ MUTLAK MERKEZLEME FÝLTRESÝ ---
    ; Dönüþ tuþlarý aktif deðilse arabayý yolun ortasýndaki beyaz çizgiye zýmbalar
    cmp esi, 0
    jnz .skip_centering         
    cmp dword [player_speed], 4 
    jl .skip_centering

    mov ecx, [player_x]
    and ecx, ecx
    jz .skip_centering          
    jns .center_from_right      

    ; Araç solda: Saða (merkeze) doðru güçlü çekiþ
    add dword [player_x], 2     ; Çekiþ gücü 2 birime sabitlendi
    cmp dword [player_x], 0     
    jle .skip_centering
    mov dword [player_x], 0     
    jmp .skip_centering

.center_from_right:
    ; Araç saðda: Sola (merkeze) doðru güçlü çekiþ
    sub dword [player_x], 2     ; Çekiþ gücü 2 birime sabitlendi
    cmp dword [player_x], 0     
    jge .skip_centering
    mov dword [player_x], 0     

.skip_centering:

    ; --- 6. SANAL BUFFER TEMÝZLEME (SKY) ---
    mov edi, [vram_buffer_ptr]   
    mov ecx, 16000               
    mov eax, 34343434h           
    rep stosd                    

    ; --- 7. PERSPEKTÝF YOL ÇÝZÝM MATRÝSÝ ---
    mov ebp, 100
.line_loop:
    mov eax, ebp
    sub eax, 100                 
    mov ecx, eax                 
    
    mov ebx, 14
    imul ebx                     
    shr eax, 3
    add eax, 8
    mov [road_width], eax

    mov eax, [road_width]
    shr eax, 3
    inc eax
    mov [rumble_width], eax

    mov eax, [road_width]
    shr eax, 5
    inc eax
    mov [line_width], eax

    ; Viraj Geometrisi
    mov edi, [sine_index]
    movsx eax, word [sine_table + edi * 2]
    sar eax, 1
    mov ebx, 100
    sub ebx, ecx
    imul eax, ebx
    imul eax, ebx
    sar eax, 14
    mov ebx, eax                 

    ; --- YOL VE ARABA SARSINTI YUMUÞATMA (DAMPING) ---
    ; Oyuncunun 'player_x' hareketini perspektif çarpýmýndan sonra 2 bit daha saða kaydýrýyoruz (sar edx, 8)
    ; Böylece araba ekranda geniþ geniþ hareket ederken, yol çizgisi ve ufuk aþýrý saða sola fýrlamayacak, stabil kalacaktýr!
    mov edx, [player_x]
    imul edx, ecx                
    sar edx, 8                  ; 6 yerine 8 yapýldý (Sallantý elendi!)

    mov eax, 160
    add eax, ebx
    sub eax, edx
    mov [road_center], eax  

    ; Tekerlek Hizasý Çimen Kontrolü (EBP = 185)
    cmp ebp, 185
    jne .skip_grass_check
    
    mov edx, [player_x]
    and edx, edx
    jns .abs_x
    neg edx                                    
.abs_x:
    mov ebx, [road_width]
    add ebx, 25                 
    cmp edx, ebx
    jle .on_asphalt
    mov dword [on_grass], 1     
    jmp .skip_grass_check
.on_asphalt:
    mov dword [on_grass], 0     
.skip_grass_check:

    ; Kýrpma Sýnýrlarý (Clipping)
    mov ebx, [road_center]
    sub ebx, [road_width]
    sub ebx, [rumble_width]
    cmp ebx, 0
    jge .left_ok
    xor ebx, ebx
.left_ok:
    mov [left_bound], ebx

    mov ebx, [road_center]
    add ebx, [road_width]
    add ebx, [rumble_width]
    cmp ebx, 320
    jle .right_ok
    mov ebx, 320
.right_ok:
    mov [right_bound], ebx

    ; Çizgi Ofsetlerini Hesapla (Y * 320)
    mov eax, ebp
    shl eax, 8                   
    mov edi, eax
    mov eax, ebp
    shl eax, 6                   
    add edi, eax                 
    add edi, [vram_buffer_ptr]   

    ; Hiperbolik daralma periyotlarý
    mov eax, ebp
    sub eax, 99
    mov ebx, eax
    mov eax, 4400h
    xor edx, edx                 
    div ebx
    mov ebx, eax

    add eax, [player_z]
    and eax, 32
    mov [strip_color_flag], eax

    mov eax, ebx
    add eax, [player_z]
    and eax, 64
    mov [grass_color_flag], eax

.render_pixels:
    xor ecx, ecx                 
.pixel_loop:
    cmp ecx, [left_bound]
    jl .draw_grass_pixel
    cmp ecx, [right_bound]
    jg .draw_grass_pixel

    mov edx, [road_center]
    sub edx, [road_width]
    sub edx, [rumble_width]
    cmp ecx, edx
    jl .check_right_rumble
    mov edx, [road_center]
    sub edx, [road_width]
    cmp ecx, edx
    jl .draw_rumble_pixel
.check_right_rumble:
    mov edx, [road_center]
    add edx, [road_width]
    cmp ecx, edx
    jl .check_center_line
    mov edx, [road_center]
    add edx, [road_width]
    add edx, [rumble_width]
    cmp ecx, edx
    jl .draw_rumble_pixel
.check_center_line:
    cmp dword [strip_color_flag], 0
    jz .asphalt_logic
    mov edx, [road_center]
    sub edx, [line_width]
    cmp ecx, edx
    jl .asphalt_logic
    mov edx, [road_center]
    add edx, [line_width]
    cmp ecx, edx
    jg .asphalt_logic
    mov al, 15                  
    jmp .put_pixel
.asphalt_logic:
    cmp dword [strip_color_flag], 0
    jz .dark_asphalt
    mov al, 7                   
    jmp .put_pixel
.dark_asphalt:
    mov al, 8                   
    jmp .put_pixel
.draw_rumble_pixel:
    cmp dword [strip_color_flag], 0
    jz .red_rumble
    mov al, 15
    jmp .put_pixel
.red_rumble:
    mov al, 4
    jmp .put_pixel
.draw_grass_pixel:
    cmp dword [grass_color_flag], 0
    jz .dark_grass
    mov al, 2
    jmp .put_pixel
.dark_grass:
    mov al, 10
.put_pixel:
    mov [edi], al                
    inc edi
    inc ecx
    cmp ecx, 320
    jl .pixel_loop

    inc ebp
    cmp ebp, 200
    jl .line_loop

; =========================================================================
; NÝHAÝ ARABA ÇÝZÝM MOTORU (KLAVYE ÖNCELÝKLÝ KESÝN SÜZGEC)
; =========================================================================
draw_player_car_32:
    cld                         
    
    ; [DÜZELTME] Sprite seçimi konum yerine doðrudan basýlan tuþun kaydý (ESI) ile yapýlýyor!
    cmp esi, 1
    je .select_left
    cmp esi, 2
    je .select_right
    
    ; Kullanýcý dönüþ tuþuna basmýyorsa veya sadece yukarý basýyorsa KESÝNLÝKLE DÜZ SPRITE
    mov dword [active_bmp_ptr], car_straight_bmp
    jmp .parse_bmp

.select_left:
    mov dword [active_bmp_ptr], car_left_bmp
    jmp .parse_bmp

.select_right:
    ; Eðer kullanýcý aktif olarak Sað Ok'a basýyorsa veya viraj arabayý saða fýrlatmýþsa:
    mov dword [active_bmp_ptr], car_right_bmp

.parse_bmp:
    mov edx, [active_bmp_ptr]
    mov eax, [edx + 10]
    mov [pixel_start_ptr], eax
    
    mov ecx, [edx + 18]         ; Geniþlik (77)
    mov ebx, [edx + 22]         ; Yükseklik (38)
    mov [sprite_w], ecx
    mov [sprite_h], ebx

    mov eax, ecx
    add eax, 3
    and eax, ~3
    sub eax, ecx
    mov [padding_w], eax

    ; --- 2X BÜYÜTÜLMÜÞ ARABA MERKEZLEME DENKLEMÝ ---
    ; Araba ekranda 2X geniþletildiði için gerçek geniþliði 154 pikseldir.
    ; Tam merkez hizalama için çýkarýlmasý gereken yarýçap 38 deðil, tam 77'dir!
    mov edx, 160                ; Ekranýn matematiksel tam X merkez koordinatý
    add edx, [player_x]         ; Oyuncunun yola göre baðýl konumu (0 = Merkez)
    sub edx, 77                 ; [KRÝTÝK DÜZELTME]: 38 yerine 77 yapýldý!

    ; Ekran sýnýr korumasý (Clipping - 2X Geniþliðe göre optimize edildi)
    cmp edx, 0
    jge .clip_left
    xor edx, edx
.clip_left:
    cmp edx, 166                ; 320 - 154 = 166 (Ekran dýþýna taþma sýnýrý)
    jle .clip_right
    mov edx, 166
.clip_right:

    ; Sabit dikey konum (Y=199 tabanýna tam kilit)
    mov edi, 124
    imul edi, edi, 320
    add edi, edx
    add edi, [vram_buffer_ptr]

    ; BMP Ters Okuma Modu (Bottom-Up)
    mov esi, [active_bmp_ptr]
    add esi, [pixel_start_ptr]
    mov eax, [sprite_h]
    dec eax
    mov ebx, [sprite_w]
    add ebx, [padding_w]
    imul eax, ebx
    add esi, eax                

    mov edx, [sprite_h]         

.bmp_y_loop:
    push edi                    
    push esi                    
    
    mov ecx, [sprite_w]
.bmp_x_loop1:
    lodsb
    cmp al, 255                 
    je .skip_p1                 
    cmp al, 15                  
    je .skip_p1
    
    mov [edi], al
    mov [edi+1], al             
.skip_p1:
    add edi, 2
    dec ecx
    jnz .bmp_x_loop1

    pop esi                     
    pop edi                     
    push edi                    
    push esi                    
    
    add edi, 320                
    mov ecx, [sprite_w]
.bmp_x_loop2:
    lodsb
    cmp al, 255 
    je .skip_p2
    cmp al, 15
    je .skip_p2
    
    mov [edi], al               
    mov [edi+1], al             
.skip_p2:
    add edi, 2
    dec ecx
    jnz .bmp_x_loop2

    pop esi                     
    mov eax, [sprite_w]
    add eax, [padding_w]
    sub esi, eax                

    pop edi                     
    add edi, 640                
    
    dec edx
    jnz .bmp_y_loop
    ;;;;

    ; --- DOUBLE BUFFER FLIP (FLAT MEMORY COPY OPTIMIZATION) ---
    mov edi, 0x000A0000          ; TRDOS 386 Flat VGA bellek adresi
    mov esi, [vram_buffer_ptr]   ; Sanal arka plan bellek adresi
    mov ecx, 16000               ; 64000 byte / 4 = 16000 dword transfer adýmý
    rep movsd                    ; Blok halinde ve ultra hýzlý ekran güncelleme

    ; --- ESC TUÞU KONTROLÜ ---
    ; Belirttiðiniz gibi int 34h veya TRDOS Syscall mekanizmasý handle edebilir.
    ;;in al, 60h
    ;mov dx, 60h
    ;;mov ah, 0
    ;xor eax, eax  ; read port -byte- (ah=0)
    ;int 34h

    ;cmp al, 1
    cmp byte [scancode], 1
    jne main_game_loop

exit_program:
    mov ax, 0003h
    ;int 10h
    int 31h	; set video mode to 13h
    ; TRDOS 386 flat binary çýkýþ kesmesi veya standart çýkýþ rutini buraya gelecek
    ;mov ah, 4Ch
    ;int 21h
    mov ebx, 0 ; exit code ; not neccessary !
    mov al, 1 ; sysexit (eax=1)
    int 40h

;hang:
    ;jmp short hang

; =========================================================================
; DATA VE DEÐÝÞKENLER SAHASI (ALL FIELDS ARE 32-BIT ALIGNED DWORD)
; =========================================================================

align 4
scancode: db 0	

; 16-bit sinüs tablosu uyumluluk için word kalmýþtýr, movsx eax ile 32-bit'e geniþletilir
align 4
sine_table:
    dw 0, 15, 30, 45, 60, 74, 88, 101, 113, 123, 132, 139, 145, 149, 150, 150
    dw 148, 144, 138, 130, 120, 108, 95, 80, 65, 48, 31, 13, -4, -21, -38, -55
    dw -71, -86, -100, -112, -123, -131, -138, -143, -146, -147, -146, -142, -136, -128, -118, -106
    dw -93, -78, -63, -46, -29, -11, 6, 23, 41, 57, 73, 88, 101, 113, 123, 131

; =========================================================================

; DINAMIK BMP DEÐIÞKENLERI VE INCBIN ALANI

; =========================================================================


align 4


active_bmp_ptr:   dd 0

pixel_start_ptr:  dd 0

sprite_w:         dd 0

sprite_h:         dd 0

padding_w:        dd 0



align 4

car_straight_bmp: incbin "car.bmp"    ; Orijinal 8-bit 77x38 BMP Dosyasý

car_left_bmp:     incbin "car_l.bmp"  ; Orijinal 8-bit 77x38 BMP Dosyasý

car_right_bmp:    incbin "car_r.bmp"  ; Orijinal 8-bit 77x38 BMP Dosyasý

;current_trans_color: db 255

;-----------------------------------------------------------------
;  uninitialized data
;-----------------------------------------------------------------

bss:

ABSOLUTE bss

alignb 4

vram_buffer_ptr:  resd 1
player_x:         resd 1
player_z:         resd 1
time_tick:        resd 1
sine_index:       resd 1
road_width:       resd 1
line_width:       resd 1
rumble_width:     resd 1
road_center:      resd 1
left_bound:       resd 1
right_bound:      resd 1
strip_color_flag: resd 1
grass_color_flag: resd 1
player_speed:     resd 1        ; Aracýn anlýk dinamik hýzýný tutar
on_grass:         resd 1        ; Araç çimende mi? (0=Hayýr, 1=Evet)

; Sanal buffer alaný Flat kodun bittiði yerin arkasýndaki boþ hafýzada açýlýr
;alignb 4
vram_sanal_bellek:
                  resb 64000 ; 320x200 piksellik buffer alaný
