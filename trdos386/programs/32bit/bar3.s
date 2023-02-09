; BAR3.ASM (10/04/1993, BAR3.COM)
; ----------------------------------------------------------
; Copper bars by Patch (hamell@rigel.cs.pdx.edu)
; Uses "color each line differently and cycle colors" method
; 4-9-93
; Call Dead Man's Hand at 503.288.9264 - USR 16.8k DS
; Programming source only

; -------------------------------------------------
; TRDOS 386 & NASM version: Erdogan Tan, 30/08/2016
; ------------------------------------------------- 

; 19/05/2016
; 29/04/2016
; TRDOS 386 system calls (temporary list!)
_ver 	equ 0
_exit 	equ 1
_fork 	equ 2
_read 	equ 3
_write	equ 4
_open	equ 5
_close 	equ 6
_wait 	equ 7
_creat 	equ 8
_link 	equ 9
_unlink	equ 10
_exec	equ 11
_chdir	equ 12
_time 	equ 13
_mkdir 	equ 14
_chmod	equ 15
_chown	equ 16
_break	equ 17
_stat	equ 18
_seek	equ 19
_tell 	equ 20
_mount	equ 21
_umount	equ 22
_setuid	equ 23
_getuid	equ 24
_stime	equ 25
_quit	equ 26	
_intr	equ 27
_fstat	equ 28
_emt 	equ 29
_mdate 	equ 30
_video	equ 31
_audio	equ 32
_timer	equ 33
_sleep	equ 34
_msg    equ 35
_geterr equ 36
_rsrvd1	equ 37
_pri	equ 38
_rele 	equ 39

%macro sys 1-4
    ; 29/04/2016 - TRDOS 386 (TRDOS v2.0)	
    ; 03/09/2015	
    ; 13/04/2015
    ; Retro UNIX 386 v1 system call.	
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
    ;int 30h
    int 40h ; TRDOS 386 (TRDOS v2.0)	   
%endmacro

; TRDOS 386 (and Retro UNIX 386 v1) system call format:
; sys systemcall (eax) <arg1 (ebx)>, <arg2 (ecx)>, <arg3 (edx)>

[BITS 32] ; 80386 Protected Mode (32 bit) intructions

[ORG 0] 

start:          
		; DIRECT VGA MEMORY ACCESS
		;xor	ebx, ebx
		mov	bh, 5 ; Direct access/map to VGA memory (0A0000h)
		;mov	eax, _video ; 1Fh
		mov	al, 1Fh ; sys _video ; TRDOS 386 Video functions
		int	40h   ; TRDOS 386 system call

		; eax = 0A0000h
		and	eax, eax
		jz      terminate ; error (eax = 0)

		mov	ax, 13h
		;int	10h		; - VIDEO - SET	VIDEO MODE
					; AL = mode
		int	31h	; TRDOS 386 Video interrupt 

                mov     dx, 03C8h
                xor     al, al
                ;out     dx, al

		mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt

                inc     dx
                mov     cx, 256
spoo:           
		;out     dx, al
                ;out     dx, al
                ;out     dx, al

		;mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt
		int	34h ; TRDOS 386 - IOCTL interrupt
		int	34h ; TRDOS 386 - IOCTL interrupt

                loop    spoo

                mov     bl, 200
                mov     ax, 0101h
		mov	edi, 0A0000h
                cld
spank:          
		mov     cx, 160
                rep     stosw
                inc     al
                inc     ah
                dec     bl
                jnz     short spank

		xor	edi, edi
                mov     bx, barline
startbar:                                  		; don't poll keyboard
                mov     si, palette               	; set up pointer
                mov     bp, 1                           ; bar num = 1
palsetup:
                push    ebp
                mov     di, [ebx]
                mov     al, [edi+sintable]              ; top bar line

                mov     di, barpal
                dec     al                              ; dec for 0th index element
                xor     ah, ah                          ; clear high byte
                mov     cx, ax
                shl     ax, 1
                add     ax, cx
                add     di, ax                          ; set to proper spot in bar palette
                mov     bp, barpriority           	;
                add     bp, cx                          ; set to proper spot in priority check
                mov     cx, [barheight]
checkline:
                cmp     byte [ebp], 0                   ; is the line empty?
                jne     short nodrawline                ; if not, jump
                mov     byte [ebp], 1                   ; it's full now
                movsw
                movsb
                jmp     short nextpriority

nodrawline:     
		add     si, 3                           ; increment palette pointer
                add     di, 3                           ; increment bar palette
nextpriority:   
		inc     bp                              ; next priority line
                loop    checkline

barspot:        
		add     word [ebx], 3                   ; increment table index
                cmp     word [ebx], 359                 ; end of table?
                jle     short nextbar
resetbarspot:   
		mov     word [ebx], 0                   ; reset table index

nextbar:        
		pop     ebp
                inc     bx                              ; next bar line address
                inc     bx                              ; word size = + 2
                inc     bp                              ; increment bar counter

                cmp     bp, [barcount]                  ; bar count at max?
                jle     short palsetup                  ; if <= , go back

                mov     si, barpal
                mov     cx, 200

                mov     dx, 03DAh               ; Input Status 1 reg
VRetrace:       
		;in      al, dx

		mov	ah, 0 ; in (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt

                test    al,00001000b            ; test bit 3, vert retrace
                jnz     short VRetrace          ; if active, go back
VNoRetrace:     
		;in      al, dx

		mov	ah, 0 ; in (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt

                test    al,00001000b            ; test bit 3, vert retrace
                jz      short VNoRetrace        ; if active, go back

                mov     al, 01h                         ; start at color 1
                mov     dx, 03C8h
               ;out     dx, al

		mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt

                inc     dx
paldump:        
		;outsb                                  ; dump it out
                ;outsb
                ;outsb
		
		lodsb
		;mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt
		lodsb
		int	34h ; TRDOS 386 - IOCTL interrupt
		lodsb
		int	34h ; TRDOS 386 - IOCTL interrupt

                loop    paldump

                mov     di, barpriority
                mov     cx, 100                         ; 100 of them WORD sized
                xor     ax, ax                          ; AH = 0, AL = 0
                rep     stosw                           ; zero them all out

                mov     di, barpal
                mov     cx, 300
                xor     ax, ax
                rep     stosw

                sub     bx, [barcount]                  ; reset bar line address
                sub     bx, [barcount]                  ; twice for word size

                mov     ah, 01h	 ; Check keyboard buffer
                ;int     16h
		int	32h	 ; TRDOS 386 keyboard interrupt
                jz      startbar ; Empty
       
		mov	ax, 3
		;int	10h		; - VIDEO - SET	VIDEO MODE
					; AL = mode
		int	31h	; TRDOS 386 Video interrupt 
terminate:
		sys 	_exit   ; INT 40h
here:
		jmp	short here

sintable        db      1,1,1,1,1,1,1,1,1,2,2,2
                db      3,3,3,4,4,5,5,6,6,7,7,8
                db      8,9,10,11,11,12,13,14,14,15,16,17
                db      18,19,20,21,22,23,24,25,26,27,29,30
                db      31,32,33,35,36,37,38,40,41,42,44,45
                db      46,48,49,51,52,54,55,57,58,60,61,63
                db      64,66,67,69,70,72,73,75,77,78,80,81
                db      83,84,86,88,89,91,93,94,96,97,99,101
                db      102,104,105,107,108,110,112,113,115,116,118,119
                db      121,122,124,125,127,128,130,131,133,134,136,137
                db      138,140,141,143,144,145,147,148,149,150,152,153
                db      154,155,156,158,159,160,161,162,163,164,165,166
                db      167,168,169,170,171,171,172,173,174,174,175,176
                db      177,177,178,178,179,179,180,180,181,181,182,182
                db      182,183,183,183,184,184,184,184,184,184,184,184         ; top to bottom palette values
                db      184,184,184,184,184,184,184,184,184,183,183,183
                db      182,182,182,181,181,180,180,179,179,178,178,177
                db      177,176,175,174,174,173,172,171,171,170,169,168
                db      167,166,165,164,163,162,161,160,159,158,156,155
                db      154,153,152,150,149,148,147,145,144,143,141,140
                db      139,137,136,134,133,131,130,128,127,125,124,122
                db      121,119,118,116,115,113,112,110,108,107,105,104
                db      102,101,99,97,96,94,93,91,89,88,86,84
                db      83,81,80,78,77,75,73,72,70,69,67,66
                db      64,63,61,60,58,57,55,54,52,51,49,48
                db      47,45,44,42,41,40,38,37,36,35,33,32
                db      31,30,29,27,26,25,24,23,22,21,20,19
                db      18,17,16,15,14,14,13,12,11,11,10,9
                db      8,8,7,7,6,6,5,5,4,4,3,3
                db      3,2,2,2,1,1,1,1,1,1,1,1                                 ; bottom to top palette values

palette         db       7, 7, 7                        ; gray bar start
                db      10,10,10
                db      13,13,13
                db      16,16,16
                db      19,19,19
                db      22,22,22
                db      25,25,25
                db      28,28,28
                db      31,31,31                        ; middle
                db      28,28,28
                db      25,25,25
                db      22,22,22
                db      19,19,19
                db      16,16,16
                db      13,13,13
                db      10,10,10
                db       7, 7, 7                        ; gray bar end
                db       7, 0, 0                        ; red bar start
                db      10, 0, 0
                db      13, 0, 0
                db      16, 0, 0
                db      19, 0, 0
                db      22, 0, 0
                db      25, 0, 0
                db      28, 0, 0
                db      31, 0, 0                        ; middle
                db      28, 0, 0
                db      25, 0, 0
                db      22, 0, 0
                db      19, 0, 0
                db      16, 0, 0
                db      13, 0, 0
                db      10, 0, 0
                db       7, 0, 0                        ; red bar end
                db       0, 7, 0                        ; green bar start
                db       0,10, 0
                db       0,13, 0
                db       0,16, 0
                db       0,19, 0
                db       0,22, 0
                db       0,25, 0
                db       0,28, 0
                db       0,31, 0                        ; middle
                db       0,28, 0
                db       0,25, 0
                db       0,22, 0
                db       0,19, 0
                db       0,16, 0
                db       0,13, 0
                db       0,10, 0
                db       0, 7, 0                        ; green bar end
                db       0, 0, 7                        ; blue bar start
                db       0, 0,10
                db       0, 0,13
                db       0, 0,16
                db       0, 0,19
                db       0, 0,22
                db       0, 0,25
                db       0, 0,28
                db       0, 0,31                        ; middle
                db       0, 0,28
                db       0, 0,25
                db       0, 0,22
                db       0, 0,19
                db       0, 0,16
                db       0, 0,13
                db       0, 0,10
                db       0, 0, 7                        ; blue bar end
                db       7, 7, 0                        ; yellow bar start
                db      10,10, 0
                db      13,13, 0
                db      16,16, 0
                db      19,19, 0
                db      22,22, 0
                db      25,25, 0
                db      28,28, 0
                db      31,31, 0                        ; middle
                db      28,28, 0
                db      25,25, 0
                db      22,22, 0
                db      19,19, 0
                db      16,16, 0
                db      13,13, 0
                db      10,10, 0
                db       7, 7, 0                        ; yellow bar end
                db       7, 0, 7                        ; purple bar start
                db      10, 0,10
                db      13, 0,13
                db      16, 0,16
                db      19, 0,19
                db      22, 0,22
                db      25, 0,25
                db      28, 0,28
                db      31, 0,31                        ; middle
                db      28, 0,28
                db      25, 0,25
                db      22, 0,22
                db      19, 0,19
                db      16, 0,16
                db      13, 0,13
                db      10, 0,10
                db       7, 0, 7                        ; purple bar end
                db       0, 7, 7                        ; cyan bar start
                db       0,10,10
                db       0,13,13
                db       0,16,16
                db       0,19,19
                db       0,22,22
                db       0,25,25
                db       0,28,28
                db       0,31,31                        ; middle
                db       0,28,28
                db       0,25,25
                db       0,22,22
                db       0,19,19
                db       0,16,16
                db       0,13,13
                db       0,10,10
                db       0, 7, 7                        ; cyan bar end
                db      18, 8, 1                        ; copper bar start
                db      23,13, 5
                db      28,18, 9
                db      33,23,13
                db      38,28,17
                db      43,33,21
                db      48,38,25
                db      53,43,29
                db      58,48,33                        ; middle
                db      53,43,29
                db      48,38,25
                db      43,33,21
                db      38,28,17
                db      33,23,13
                db      28,18, 9
                db      23,13, 5
                db      18, 8, 1                        ; copper bar end

barline         dw      175, 150, 125, 100, 75, 50, 25, 0
barpriority     times 200 db 0
barheight       dw      17
barcount        dw      8
bardelay        dw      10
barpal          times 200*3 db 0

_end:
