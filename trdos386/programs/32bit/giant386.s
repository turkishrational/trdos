; ****************************************************************************
; giant386.s (TRDOS 386, TRDOS v2.0 - sample binary file, 'giant386.prg')
; ----------------------------------------------------------------------------
; GIANT386.PRG ! TEST program !  TRDOS 386 VGA Functionality test !
;
; 19/09/2016
;
; [ Last Modification: 25/09/2016 ]
;
; Derived from sourc code of 'GIANT.COM' (MSDOS) intro file
; (06/08/1994, 4096 bytes)
;
;
; GIANT.ASM by Jari Kyt”joki
; NASM version of GIANT.ASM (GIANTS.COM): Erdogan Tan, 19/09/2016 (giant.s)
;
; Assembler: NASM 2.11
;
; (Original -msdos- code has been modifed for TRDOS 386 system calls and
; other protected mode (TRDOS 386) interrupts.)
; ****************************************************************************

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

;ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
;บ Filename:    GIANT.ASM				      บ
;ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน
;บ Source Codes for GIANT. The Ultimate 4KB-Intro! [:)] v. 1.1                 บ
;บ						     บ
;บ Written by:  Sigma of Star Team Productions, (ไ, STP)                       บ
;บ              alias Jari Kyt”joki		                    บ
;บ						     บ
;บ Snail-Mail:  Jari Kyt”joki				  บ
;บ              Puolukkakatu 5				 บ
;บ              44200 Suolahti				 บ
;บ              Finland, Europe, Earth		                 บ
;บ						     บ
;บ E-Mail:      jari.kytojoki@hut.fi		                   บ
;บ						     บ
;บ Please, send me a Postcard!!!		                       บ
;บ Send me your source codes! (or at least some technical info, etc...) ;)     บ
;บ Don't hesitate to contact me!		                       บ
;บ						     บ
;บ Copyright (C) 1994, 1995 Jari Kyt”joki. All Rights Reserved.                บ
;ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ

;       After many inquiries for sources I decided to release this
;       source code. It is released as public domain. You can do it
;       whatever you want or like. But remember, if you are just
;       planning to make a new intro only by adding, removing or any
;       other way changing a few lines of this code, you are entering
;       worldwide lamers' club. But if you are reading this file just
;       because you want to learn something new, go on it! This is
;       the most biggest reason why I released this code. However,
;       I cannot promise there's anything new that you haven't known
;       before. (It's always worth studying new sources...) If you find
;       this file very helpful, please send me a postcard or at least
;       write me some E-Mail.
;ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
;       You can compile this file with TASM, I included MAKE.BAT
;       to help you. You'll notice that resulting COM-file will be
;       much larger than the released version. This is due to
;       the original COM-file has been compressed to fit in those
;       specified limits. If you are just compiling, testing and
;       debugging this source file there's no harm of it. You'll
;       have to compress the COM-file with your own compressor if
;       you want the new COM-file to be as small as the original
;       one. ;-)
;ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
;       The code was optimized mainly for size and some parts seem to
;       be coded quite weirdly. In some time critical parts I repeated
;       the source code and I trusted the compressor would do the rest,
;       so that the file would be less than 4KB. There is still some
;       "extra" dummy bytes which could easily be removed.
;         My opinion of making a small intro is not hard. I had quite a
;       many ideas to put in this intro within the limits I had set, but
;       soon all 4KB was used and I had to think all over again. Many of
;       my plans finished undone. The size limit was reached too quickly.
;         I hope you enjoy my intro and learn something new.
;
;Last words: I'm sorry there isn't too many comments in this file.
;            Just try to figure it out.
;อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

; NASM version: Erdogan Tan, 19/09/2016

[BITS 32]

section .text

        Temp0   	Equ	_10A2h
        Temp1           Equ	_10A4h
        Temp2          	Equ	_10A6h
        Temp4		Equ     0100h
        Sin2T           Equ	0D000h
        Cos2T          	Equ	0D200h
        SinT           	Equ	0E000h
        CosT           	Equ	0E200h
        Temp_PAL3       Equ 	0DF00h
        Temp_PAL2       Equ 	0DC00h
        Temp_PAL1       Equ 	0EC00h
        Row256T        	Equ	0CE00h
        CharP          	Equ	0106h
        CharColor      	Equ	010Ah
        TextSeg         Equ 	Temp0
        XMin           	Equ	010Ch
        YMax           	Equ	010Eh
        XStep          	Equ	0110h
        YStep          	Equ	0112h
        ScreenX        	Equ	0114h
        ScreenY        	Equ	0116h
        NMax           	Equ	0118h
        x              	Equ	011Ah
        y              	Equ	011Ch
        zi             	Equ	011Eh
        zr             	Equ	0120h
        cr              Equ	0122h
        ci              Equ	0124h
        d2x             Equ	zi
        d2y             Equ	zr
        i               Equ	ScreenX
        j               Equ	ScreenY
        TempPAL1        Equ	0E000h
        TempPAL2        Equ	0E400h

[ORG 0]

  Start:
		; DIRECT VGA MEMORY ACCESS
		;xor	ebx, ebx
		mov	bh, 5 ; Direct access/map to VGA memory (0A0000h)
		;mov	eax, _video ; 1Fh
		mov	al, 1Fh ; sys _video ; TRDOS 386 Video functions
		int	40h   ; TRDOS 386 system call

		; eax = 0A0000h
		and	eax, eax
		jz      terminate ; error (eax = 0)

		; clear screen (Black screen at the beginning)
		mov	al, 3 ; 80x25 16 color text (current mode)
                ;mov	ah, 0 ; Set video mode
                ;int	10h
		int	31h ; TRDOS 386 - Video interrupt

                ; Hide cursor
		mov     cx, 0FF00h
                mov	ah, 1
		;int	10h
                int	31h ; TRDOS 386 - Video interrupt
                
                mov     dx, 03CCh         ; set your VGA to
                ;in     al, dx		  ; correct mode...
		mov	ah, 0 ; in (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt

                or      al, 01h		  ; B/W -> COLOR

                mov     dx, 03C2h
                ;out    dx, al
		;mov	ah, 1 ; out (byte)
		inc	ah ; 1
		int	34h ; TRDOS 386 - IOCTL interrupt

                mov     cx, ((W_Divider-Data_Seg)/4)+2
                mov     esi, _Data_Seg
                mov     edi, Data_Seg ; 10000h
                rep     movsd
  @KeyPressed1:
                mov     ah, 1
		;int	16h
                int	32h ; TRDOS 386 Keyboard interrupt
                jz      short @KeyBufferCleared
                xor     ah, ah
                ;int	16h
		int	32h ; TRDOS 386 Keyboard interrupt
                jmp     short @KeyPressed1
  @KeyBufferCleared:
                finit		           ; Make Sin and Cos Tables
                xor     esi, esi
                mov     cx, 321
                fild    word [YY]
                fldpi
                fild    word [XX]
                fdivp   st1, st0
  @MakeSINCOS:
                mov     [XX], si
                fild    word [XX]
                fmul    st0, st1
                fsin
                fmul    st0, st2
                fistp   word [esi+SinTable]
                inc     esi
                loop    @MakeSINCOS

                mov     edx, TempPAL1
                xor     bx, bx
                mov     cx, 256
                mov     al, 17h
                mov     ah, 10h
                int	31h ; TRDOS 386 - Video interrupt

                ;M_FadeOUT TempPAL1, TempPAL2
		mov	esi, TempPAL1 ; SourcePAL
		mov	edi, TempPAL2 ; DestPAL
		call	M_FadeOUT

                wait

                ;M_KeyPressed
                mov     ah, 01h
                int	32h ; TRDOS 386 Keyboard interrupt
                jnz     @KeyDownOut

                mov     al, 13h
                mov     ah, 00h
                int	31h ; TRDOS 386 - Video interrupt

                mov     dx, TempPAL1
                xor     bx, bx
                mov     cx, 256
                mov     al, 17h
                mov     ah, 10h
                int	31h ; TRDOS 386 - Video interrupt

                call	M_ClearPAL
                mov     si, Text2String
                call	print_msg
                mov     esi, 0A0000h ; gs
                mov	edi, 20000h  ; es, fs

                mov	cl, 8
rept_8_1:
		push	cx
                mov     cx, 5*8/4
                rep	movsd  ; gs:si -> es:di
                add     si, (320-5*8)
                pop	cx
		loop	rept_8_1
                mov	cl, 8
rept_8_2:
		push	cx
                mov     cx, 12*8/4
                rep	movsd  ; gs:si -> es:di
                add     si, (320-12*8)
		pop	cx
		loop	rept_8_2
                mov	cl, 8
rept_8_3:
		push	cx
                mov     cx, 9*8/4
                rep	movsd  ; gs:si -> es:di
                add     si, (320-9*8)
		pop	cx
                loop	rept_8_3

                mov	edi, 0A0000h ; es
                mov     cx, 199*256
  @BG1:
                mov     dx, 319
  @BG2:
                mov     di, cx ; edi = 0A0000h + cx
                shr     di, 2
                add     di, cx
                add     di, dx
                mov     al, dl
                xor     al, ch
                and     al, 1Fh
                xor     al, 30h
                stosb
                dec     dx
                jge     short @BG2
                sub     ch, 1
                jnc     short @BG1
                mov     ax, 0707h

                mov	esi, 20000h
		;mov	edi, 0A0000h

                ; M_WriteText 1
		; di = (60+TextXYMove)*320+120+TextXYMove
		; bx = (92+TextXYMove)*320+64+TextXYMove
		; dx = (124+TextXYMove)*320+88+TextXYMove
		mov	di, ((60+1)*320)+120+1
		mov	bx, ((92+1)*320)+64+1
		mov	dx, ((124+1)*320)+88+1
		call	M_WriteText

                xor     ax, ax

                ;mov	esi, 20000h
		xor	si, si
		;mov	edi, 0A0000h

                ; M_WriteText -1
		; di = (60+TextXYMove)*320+120+TextXYMove
		; bx = (92+TextXYMove)*320+64+TextXYMove
		; dx = (124+TextXYMove)*320+88+TextXYMove
		mov	di, ((60-1)*320)+120-1
		mov	bx, ((92-1)*320)+64-1
		mov	dx, ((124-1)*320)+88-1
		call	M_WriteText

		;M_FadeIN TempPAL1, TempPAL2
		mov	esi, TempPAL1 ; SourcePAL
		mov	edi, TempPAL2 ; DestPAL
		call	M_FadeIN
                mov     cx, 250
  @wv1:
                call	M_WaitVRT_DX
                loop    @wv1
                ;M_FadeOUT TempPAL1, TempPAL2
		mov	si, TempPAL1 ; SourcePAL
		mov	di, TempPAL2 ; DestPAL
		call	M_FadeOUT
                ;M_KeyPressed
                mov     ah, 01h
                int	32h ; TRDOS 386 Keyboard interrupt
                jnz     @KeyDownOut

                mov     al, 12h ; 640x480 16 color graphics
                mov     ah, 00h ; Set video mode
                ;int    10h
		int	31h ; TRDOS 386 - Video interrupt

                mov     cx, 20h
  @wv2:
                call	M_WaitVRT_DX
                loop    @wv2

		; Get Current Character generator Info
		;(modified funtion for TRDOS 386)
                mov     bx, 1 ; get 8x8 VGA font data
                mov     al, 30h 
                mov     ah, 11h
		mov	edx, VGA_Fonts ; buffer address
		;sub	ecx, ecx ; all character fonts 
                ; ecx = 0
		int	31h ; TRDOS 386 - Video interrupt
	
                push    eax	
                mov     bl, 15
  @SetNextPAL:
                mov     bh, bl
                mov     al, 00h
                mov     ah, 10h
                int	31h ; TRDOS 386 - Video interrupt
                add     cx, 4
                dec     bl
                jns     short @SetNextPAL
                call	M_WaitVRT_DX
                mov     dx, 3D4h
                ;mov    ax, 6A0Ch
                ;out    dx, ax
		mov	bx, 6A0Ch 
		mov	ah, 3 ; out (word in BX)
		int	34h ; TRDOS 386 - IOCTL interrupt

                mov     dx, 03C8h
                xor     al, al
                ;out    dx, al
                mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt

                inc     dx
                mov     cx, 16*3
                xor     bl, bl
  @SetPal:
                xor     al, al
                ;out    dx, al
                ;mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt
                ;out    dx, al
 		int	34h ; TRDOS 386 - IOCTL interrupt
                mov     al, bl
                ;out    dx, al
		int	34h ; TRDOS 386 - IOCTL interrupt
                add     bl, 4
                loop    @SetPal

                mov     dx, 03CEh
                ;mov    ax, 0205h
                ;out    dx, ax
		mov	bx, 0205h
		mov	ah, 3 ; out (word in BX)
		int	34h ; TRDOS 386 - IOCTL interrupt

                finit
                xor     eax, eax
                mov     edi, 0A0000h ; es = 0A000h
                mov     cx, 65536/4
                rep     stosd
		xor	edi, edi
                xor     si, si 
  @CCPopAX:
                pop     eax
  @CC:
                mov     ebp, VGA_Fonts
                xor     ebx, ebx
                mov     bl, [esi+Text1String]
                lea     ebp, [ebp+8*ebx+7]
                inc     si
                and     si, 1Fh
                fld     dword [Const1]
                fmul    dword [CosBB]
                fadd    dword [BB]
                fcom    dword [Const3]
                fstsw   ax
                wait
                sahf
                ja      @GlobeOut
                fst     dword [BB]
                fcos
                fld1
                fdivrp  st1, st0
                fst     dword [CosBB]
                fld     dword [Const4]
                fmul    st0, st1
                fstp    dword [IIAdd]
                fmul    dword [Const2]
                fadd    dword [LL]
                fstp    dword [LL]
                fldz
                fstp    dword [JJ]
                mov     bl, 32
  @BB:
                fldz
                fstp    dword [II]
                fld     dword [JJ]		; B := ...
                fadd    dword [BB]
                fsincos
                fstp    dword [CosB]
                fstp    dword [SinB]
                test    bl, 3
                jnz     short @BPOK
                dec     ebp
  @BPOK:
                push    ebp
                mov     bp, [ebp]

		%rep	8
		call    CalcPixel
		%endrep

                pop     ebp
                fld     dword [JJ]
                fadd    dword [Const4]
                fstp    dword [JJ]
                dec     bx
                jnz     short @BB
                jmp     @CC
  @GlobeOut:
		; Read a Block of DAC Color Registers 
                ;mov     edx, TempPAL1
                mov     dx, TempPAL1
                xor     bx, bx
                mov     cx, 256
                mov     al, 17h
                mov     ah, 10h
                int	31h ; TRDOS 386 - Video interrupt
                mov     cx, 250
  @wv3:
                call	M_WaitVRT_DX
                loop    @wv3
                ;M_KeyPressed
                mov     ah, 01h
                int	32h ; TRDOS 386 Keyboard interrupt
                jnz     @KeyDownOut

                mov     dx, 03CEh
                ;mov    ax, 0FF08h
                ;out    dx, ax
		mov	bx, 0FF08h
		mov	ah, 3 ; out (word in BX)
		int	34h ; TRDOS 386 - IOCTL interrupt

                ;mov    bx, 6A00h
		mov	cx, 6A00h
  @ScrollDown:
                call	M_WaitVRT_DX
                mov     dx, 3D4h
                ;mov    al, 0Dh
                ;mov    ah, bl
		;out    dx, ax

		mov	bl, 0Dh
		mov	bh, cl
		mov	ah, 3 ; out (word in BX)
		int	34h ; TRDOS 386 - IOCTL interrupt
		;(ax = bx)
		
		;dec	ax
		;mov	ah, bh
                ;out    dx, ax

		dec     bx
                mov     bh, ch
		mov	ah, 3 ; out (word in BX)
		int	34h ; TRDOS 386 - IOCTL interrupt

                sub     cx, 80*2
                or      cx, cx
                jns     short @ScrollDown

                ;M_FadeOUT TempPAL1, TempPAL2
		mov	si, TempPAL1 ; SourcePAL
		mov	di, TempPAL2 ; DestPAL
		call	M_FadeOUT

                ;M_KeyPressed
                mov     ah, 01h
                int	32h ; TRDOS 386 Keyboard interrupt
                jnz     @KeyDownOut

        Add_Items       Equ     LastBopAdd - BopAddTab
        BopPts          Equ     Add_Items
        TimeOut         Equ     8192
        TimeOut2        Equ     150
        MaxWidth        Equ     320
        CenterX         Equ     160
        CenterY         Equ     100
        MaxBops         Equ     350
        RadiusX         Equ     144
        RadiusY         Equ     176
        PhInc1          Equ     2
        PhInc2          Equ     3

                mov     al, 13h
                mov     ah, 00h
                int	31h ; TRDOS 386 - Video interrupt

                mov     cx, 20h
  @wv4:
                call	M_WaitVRT_DX
                loop    @wv4

                mov     cx, Add_Items
                mov     si, LastBopAdd - 1
                mov     di, LastBopAdd + Add_Items - 2
                xor     ax, ax
                std
  @Expand0:
                lodsb
                stosw
                loop    @Expand0
                cld

                mov     dx, Add_Items/2
                mov     si, BopADDLengths - 1
                mov     di, LastBopAdd + Add_Items + 3*Add_Items/2
                mov     ax, 12*320
  @Expand1:
                inc     si
                sub     di, 3*Add_Items/2
                mov     bp, di
                mov     cl, [esi]
                neg     ax
                sub     ax, cx
  @Expand2:
                inc     ax
                stosw
                loop    @Expand2
                add     bp, dx
                mov     di, bp
                mov     cl, [esi]
                add     ax, cx
  @Expand3:
                stosw
                dec     ax
                loop    @Expand3
                add     bp, dx
                mov     di, bp
                mov     cl, [esi]
                add     ax, 320
                neg     ax
                sub     ax, cx
  @Expand4:
                inc     ax
                stosw
                loop    @Expand4
                add     bp, dx
                mov     di, bp
                mov     cl, [esi]
                add     ax, cx
		; es = cs
  @Expand5:
                stosw
                dec     ax
                loop    @Expand5
                jnz     short @Expand1
                
		; es = 0A000h
                ;M_ShadeBOBs TimeOut
		call	M_ShadeBOBs_1
                ;M_KeyPressed
                mov     ah, 01h
                int	32h	; TRDOS 386 Keyboard interrupt
                jnz     @KeyDownOut
                mov     cx, 30
  @FlashBOBs:
                push    cx
                ;M_ShadeBOBs TimeOut2
		call	M_ShadeBOBs_2
                pop     cx
                loop    @FlashBOBs
                jmp     @BOBs_Out
  ShadesLoop:
                mov     si, 2*2188
                mov     ax, [Phase1]
                add     ax, PhInc1
                cwd
                div     si
                mov     [Phase1], dx
                mov     ax, [Angle]
                add     ax, 20h
                mov     [Angle], ax
                mul     dx
                xor     bh, bh
                mov     bl, dl
                mov     ah, [ebx+CosTable]
                mov     al, RadiusX
                imul    ah
                sar     ax, 6
                add     ax, CenterX
		mov     di, ax
                mov     ax, [Phase2]
                add     ax, PhInc2
                cwd
                div     si
                mov     [Phase2], dx
                mov     ax, [Angle]
                mul     dx
                mov     bl, dl
                mov     ah, [ebx+SinTable]
                mov     al, RadiusY
                imul    ah
                sar     ax, 6
                add     ax, CenterY
                imul    dx, ax, MaxWidth
                call    PutBop
                dec     word [Frames]
                jne     ShadesLoop
                mov     dx, MaxBops
  @HideBOBsLoop:
                mov     di, -1
                call    HideBop
                dec     dx
                jnz     short @HideBOBsLoop
		retn
  @BOBs_Out:
                finit		           ; Make Sin and Cos Tables
                xor     si, si
                ; es = ds = cs
                ; fs = cs + 2000h
		mov     cx, 2*5*128+1
                fld     dword [Mult1]
                fild    word [Temp1]
                fldpi
                fild    word [Temp0]
                fdivp   st1, st0
  @Make2SINCOS:
                fild    word [Temp2]
                fmul    st0, st1
                fsincos
                fmul    st0, st4
                fistp   word [esi+Sin2T]
                fmul    st0, st2
                fistp   word [esi+SinT]
                inc     word [Temp2]
                inc     si
                inc     si
                loop    @Make2SINCOS
                wait
                xor     ax, ax
                mov     cx, 200
                mov     di, Row256T
  @MakeRow256:
                stosw
                add     ax, 256
		loop	@MakeRow256
               
               	;; Get Current Character generator Info
		;;(modified funtion for TRDOS 386)
                ;mov     bx, 1 ; get 8x8 VGA font data
                ;mov     al, 30h 
                ;mov     ah, 11h
		;mov	edx, VGA_Fonts ; buffer address
		;;sub	ecx, ecx ; all character fonts 
                ;; ecx = 0
		;int	31h ; TRDOS 386 - Video interrupt

                ;M_KeyPressed
                mov     ah, 01h
                int	32h ; TRDOS 386 Keyboard interrupt
                jnz     @KeyDownOut
                call	M_WaitVRT_DX
                mov     al, 13h
                mov     ah, 00h
                int	31h ; TRDOS 386 - Video interrupt
                mov     dx, Temp_PAL2
                xor     bx, bx
                mov     cx, 256
                mov     al, 17h
                mov     ah, 10h
                int	31h ; TRDOS 386 - Video interrupt

                mov     si, Temp_PAL2+3*20h
                mov     di, Temp_PAL3
                mov     cx, 3*10h
                rep     movsw
                call	M_ClearPAL

                ; es = fs = cs + 2000h
                mov	edi, 20000h
                mov     [TextSeg], edi ; es
                mov     ax, di
                mov     cx, 32*100h
                rep     stosw
                mov     word [CharColor], 0F09h
                mov     di, 4*100h+8
                mov     si, Gfx1Text
                call    GfxWrite
		; es = cs
 		mov	edi, 20000h
		mov	esi, edi
                ; es = fs = cs + 2000h
                mov     si, 0*100h
                mov     di, 4*8*100h
                mov     cx, 7*4*8*128
                rep	movsw ; es:si -> es:di (26h, movsw)
                mov     word [x], 7A00h
                mov     word [y], 2176
                xor     ebx, ebx
		xor	esi, esi
		;mov	edi, 40
                mov     di, 40 ; edi = 40
		call    RotateMap2
                ; es = cs
                ;M_FadeIN Temp_PAL2, Temp_PAL1
		mov	si, Temp_PAL2 ; SourcePAL
		mov	di, Temp_PAL1 ; DestPAL
		call	M_FadeIN
                xor     bx, bx
                mov     di, 40 ; edi = 40
  @Rotate1Map:
                push    bx
                push    di
                call    RotateMap2
                call	M_WaitVRT_DX
                pop     di
                pop     bx
                add     word [x], 16
                add     word [y], 16
                add     di, 30h
                sub     bx, 4
                cmp     di, 2*1385
                jb      short @Rotate1Map

                ; es = cs
                ;M_KeyPressed
                mov     ah, 01h
                int	32h	; TRDOS 386 Keyboard interrupt
                jnz     @KeyDownOut
                mov     word [XMin], -5500
                mov     word [YMax], 0
                mov     word [NMax], 512         ;256
                mov     word [cr], 2100
                mov     word [ci], 0
                mov     word [XStep], 10
                mov     word [YStep], 10
                ; es = fs = 2000h
		mov	ebx, 20000h ; fs = 2000h
                call    CalcFractal
                mov     word [YMax], 1280+0
		mov	ebx, 28000h
                ; es = fs = 2800h
                call    CalcFractal
                xor	ebx, ebx ; es = cs
                ;M_FadeOUT Temp_PAL2, Temp_PAL1
		mov	si, Temp_PAL2 ; SourcePAL
		mov	di, Temp_PAL1 ; DestPAL
		call	M_FadeOUT

                ;M_KeyPressed
                mov     ah, 01h
                int	32h	; TRDOS 386 Keyboard interrupt
                jnz     @KeyDownOut
                mov     si, TempPAL2
                call	M_WaitVRT_DX
                mov     dx, 03C8h
                mov     al, 00h
                ;out    dx, al
		mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt

                inc     dx
                mov     cx, 3*256
_outsb1:
                ;rep     outsb
		lodsb
		;mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt
		loop	_outsb1
                
                call    RotatePAL20
                mov     word [x], 32768
                mov     word [y], 0
                xor     bx, bx
                mov     di, 7800h
  @Rotate2Map:
                push    bx
                push    di
                call    RotateMap2
                call	M_WaitVRT_DX
                pop     di
                pop     bx
                add     bx, 8
                sub     di, 2*40h
                jnc     short @Rotate2Map
                ;M_KeyPressed
                mov     ah, 01h
                int	32h	; TRDOS 386 Keyboard interrupt
                jnz     @KeyDownOut
                mov     cx, 10
  @wv5:
                call	M_WaitVRT_DX
                loop    @wv5
                mov     cx, 984
                mov     dx, 2*50
                xor     bx, bx
                mov     di, 0
  @Rotate3Map:
                pusha
                call    RotateMap
                call    RotatePAL20
                popa
                sub     word [x], 16
                sub     word [y], 16
                add     di, dx
                cmp     di, 6000h
                jna     short @ZoomDIR
                neg     dx
  @ZoomDIR:
                sub     bx, 4
                loop    @Rotate3Map
                ;M_FadeOUT Temp_PAL2, Temp_PAL1
		mov	si, Temp_PAL2 ; SourcePAL
		mov	di, Temp_PAL1 ; DestPAL
		call	M_FadeOUT
                ;M_KeyPressed
                mov     ah, 01h
                int	32h	; TRDOS 386 Keyboard interrupt
                jnz     @KeyDownOut
		;M_WaitVRT_DX
                call	M_WaitVRT_DX
                mov     al, 13h
                mov     ah, 00h
                int	31h ; TRDOS 386 - Video interrupt
		; es = fs               
                mov	edi, 20000h
                mov     [TextSeg], edi ; es
                mov     ax, di
                mov     cx, 10000h/2
                rep     stosw
		mov     word [CharColor], 0C00h
                ;mov	edi, 80
		mov     di, 80
                mov     si, Gfx2Text
                call    GfxWrite
                mov     word [x], 8000h
                mov     word [y], 400h
                mov     bx, 2*512
                mov     di, 1200h
  @Rotate4Map:
                push    bx
                push    di
                call    RotateMap2
		;M_WaitVRT_DX
                call	M_WaitVRT_DX
                pop     di
                pop     bx
                sub     bx, 2*4
                jns     short @BXOkay
                xor     bx, bx
  @BXOkay:
                sub     di, 18h
                jnc     short @Rotate4Map
                ;M_KeyPressed
                mov     ah, 01h
                int	32h	; TRDOS 386 Keyboard interrupt
                jnz     @KeyDownOut
                ; es = fs
                mov	edi, 20000h
                mov     [TextSeg], edi ; es
                mov     ax, di
                mov     cx, 10000h/2
                rep     stosw
                mov     word [CharColor], 0A00h
                mov     di, 116
                mov     si, Gfx3Text
                call    GfxWrite
                xor     bx, bx
                mov     di, 0
  @Rotate5Map:
                push    bx
                push    di
                call    RotateMap2
		;M_WaitVRT_DX
                call	M_WaitVRT_DX
                pop     di
                pop     bx
                add     di, 18h
                cmp     di, 1200h
                jbe     short @Rotate5Map
                ;M_FadeOUT Temp_PAL2, Temp_PAL1
		mov	si, Temp_PAL2 ; SourcePAL
		mov	di, Temp_PAL1 ; DestPAL
		call	M_FadeOUT
                ;M_KeyPressed
                mov     ah, 01h
                int	32h	; TRDOS 386 Keyboard interrupt
                jnz     @KeyDownOut

        FreeSpace       Equ     W_Divider
        _TempR32A       Equ     FreeSpace+00h
        _TempR32B       Equ     FreeSpace+04h
        _TempR32C       Equ     FreeSpace+08h
        _TempReg        Equ     FreeSpace+0Ch
        MountHeight     Equ     FreeSpace+10h
        Map_X           Equ     FreeSpace+12h
        Map_Y           Equ     FreeSpace+14h
        RandomSeed      Equ     FreeSpace+16h
        _TempR16A       Equ     FreeSpace+18h
        _TempR16B       Equ     FreeSpace+1Ah
        _TempR16C       Equ     FreeSpace+1Ch
        Count_Loop      Equ     FreeSpace+1Eh
        Table_A         Equ     10200h
        Table_B         Equ     10400h
        Table_C         Equ     10600h

                mov	edi, 10000h
                stc
                sbb     eax, eax
                mov     cx, ((LastMake_-MapMakeTable)/2)+1 ; 3Dh
  @Decompress2:
                add     ax, [edi]
                inc     ax
                stosw
                loop    @Decompress2
                xor     ax, ax
                ;int    1Ah
		int	35h ; TRDOS 386 - Date&Time interrupt
		and	ecx, 7FFFh 
                mov     [RandomSeed], cx

                mov     ax, 0013h
                int	31h ; TRDOS 386 - Video interrupt
		;
                mov     dx, 03C8h
                xor     al, al
                ;out    dx, al
		mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt
                inc     dx
                mov     cx, 64
  @SetPal1:
                ;out    dx, al
		;mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt
                ;out    dx, al
		;mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt
                ;out    dx, al
		;mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt
                inc     al
                loop    @SetPal1
                mov     cl, 64
                ;xor    ah, ah
		xor	bl, bl
  @SetPal2:
                xor     al, al
                ;out    dx, al
		;mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt
                ;out    dx, al
		;mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt
                ;mov    al, ah
		mov	al, bl
                ;out    dx, al
		;mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt
                ;inc    ah
                inc	bl
		loop    @SetPal2
                mov     cl, 64
                ;xor    ax, ax
                xor     al, al
		xor	bl, bl
  @SetPal3:
                ;out    dx, al
		;mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt
                ;mov    al, ah
                ;out    dx, al
		mov	al, bl
		;mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt 
                xor     al, al
                ;mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt 
                ;inc    ah
		inc	bl
                loop    @SetPal3
                mov     cl, 64
  @SetPal4:
                ;mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt 
                ;mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt 
                ;mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt 
                loop    @SetPal4
                
                mov     edx, Temp_PAL2+Data_Seg
                xor     bx, bx
                mov     cx, 256
                mov     al, 17h
                mov     ah, 10h
                int	31h ; TRDOS 386 - Video interrupt

                call	M_ClearPAL

                call    CalcSky
                mov     word [Count_Loop], 1024
                mov     word [Map_X], 1024
                mov     word [Map_Y], 1024
                call    UpdateView
              	call    UpdateSky
                call    UpdateMap
                call	M_WaitVRT_DX
                mov	edi, VRAM_Seg ; mov es, [VRAM_Seg]
		mov     esi, Table_C
                mov     di, (320-256)/2
                mov     bl, 200
		;xor	ecx, ecx
  @Copy2VRAM:
                mov     cl, 256/4
                rep     movsd
                add     di, (320-256)
		dec	bl
                jnz     short @Copy2VRAM
  		;M_FadeIN Temp_PAL2, Temp_PAL1
		mov	esi, Temp_PAL2+Data_Seg ; SourcePAL
		mov	edi, Temp_PAL1+Data_Seg ; DestPAL ; es = [Data_Seg]
		call	M_FadeIN
                mov     cx, 250
  @wv6:
                call	M_WaitVRT_DX
                loop    @wv6
  @DrawAgain:
		call    UpdateView
                call    UpdateSky
                call    UpdateMap
                call	M_WaitVRT_DX
                mov	edi, VRAM_Seg ; mov es, [VRAM_Seg]
		mov     esi, Table_C
                mov     di, (320-256)/2
                mov     bl, 200
		;xor	ecx, ecx
  @CopyToVRAM:
                mov     cl, 256/4
                rep     movsd
                add     di, (320-256)
		dec	bl
                jnz     short @CopyToVRAM
                dec     word [Count_Loop]
                jnz     short @DrawAgain
                ;M_FadeOUT Temp_PAL2, Temp_PAL1
		mov	esi, Temp_PAL2+Data_Seg ; SourcePAL
		mov	edi, Temp_PAL1+Data_Seg ; DestPAL ; es = [Data_Seg]
		call	M_FadeOUT
                jmp     @TheEND
 
  CalcSky:	; sub_244E
                mov	edi, Sky_Seg ; mov es, [Sky_Seg]
		mov	ebp, edi

                stc
                sbb     eax, eax
                mov     cx, 4000h
                rep     stosd
                mov     si, [RandomSeed]

                call	M_MakeMUL
                mov     [RandomSeed], dx
                mov     cx, 0100h
                mov     byte [ebp+0080h], -2
                mov     byte [ebp+8000h], -2
                mov     byte [ebp+0000h],  cl 
                mov     byte [ebp+8080h],  cl ; es
               	mov	ebx, ebp ; es:bx, bx = 0
                mov	edi, ebx ; es:di, es = [Sky_Seg]
		call	CalcFrac
                mov	edi, ebp
  CalcSky1:
                mov     ah, 1
                mov     al, [edi]  ; [es:di] ; di = 0
                shr     ax, 2
                stosb	
                or	di, di
		jnz	short CalcSky1
;  CalcMap:
               	mov	edi, MapA_Seg ; mov es, [MapA_Seg]
		mov	ebp, edi

                stc
                sbb     eax, eax
                mov     cx, 4000h
		rep	stosd
                mov     si, [RandomSeed]
                call	M_MakeMUL
                mov     [RandomSeed], dx
                mov     cx, 0100h
                mov     al, 40
                ;stosb
		mov	[ebp], al
		mov	ebx, ebp ; es:bx, bx = 0
		mov	edi, ebx ; es:di, es = [MapA_Seg]
		call	CalcFrac
		;xor	ebx, ebx
                xor     di, di  ; edi -> es:di, di = 0
		mov	ebx, edi ; MapA_Seg 
  CalcMap1:
                xor     ax, ax
                mov     esi, MapCoord_
                mov     bx, [esi]
		add	bx, di
                add     al, [ebx] ; [es:bx+di]
                adc     ah, ch
                inc     si
                inc     si
                mov     bx, [esi]
		add	bx, di
                add     al, [ebx] ; [es:bx+di]
                adc     ah, ch
                inc     si
                inc     si
                mov     bx, [esi]
		add	bx, di
                add     al, [ebx] ; [es:bx+di]
                adc     ah, ch
                inc     si
                inc     si
                mov     bx, [esi]
		add	bx, di
                add     al, [ebx] ; [es:bx+di]
                adc     ah, ch
                shr     ax, 2
                stosb
                or      di, di
                jnz     short CalcMap1
		;xor	ebx, ebx
                mov	esi, ebp ; es = [MapA_Seg]
		mov	edi, esi
             	mov	ebp, MapB_Seg ; mov  fs, [MapB_Seg]
  CalcMap2:
                mov     al, [esi]    ; [es:si]
                sub     al, [esi+3]  ; [es:si+3]
                sbb     ah, ah
                add     ax, 20h
                jns     short CalcMap3
                xor     ax, ax
  CalcMap3:
                cmp     al, 3Fh
                jbe     short CalcMap4
                mov     al, 3Fh
  CalcMap4:
		mov	bp, si
		mov	[ebp], al ; mov	[fs:si],al
                inc     si
                jnz     short CalcMap2
  CalcMap5:
		mov	ebp, edi ; es = [MapA_Seg]	
                xor     ax, ax
                add     al, [ebp] ; [es:di]
                adc     ah, ch
		add	bp, 100h
                add     al, [ebp] ; [es:di+0100h]
                adc     ah, ch
                inc     di
		mov	ebp, edi
                add     al, [ebp] ; [es:di]
                adc     ah, ch
		add	bp, 100h
                add     al, [ebp] ; [es:di+0100h]
                adc     ah, ch
                dec     di
                shr     ax, 2
                stosb
                or      di, di
                jnz     short CalcMap5
		;xor	edi, edi
  OutCalcFrac:
                retn

  CalcFrac:	; sub_2552
                shr     cx, 1
                jz      short OutCalcFrac
                xor     ax, ax
                add     al, [ebx] ; [es:bx]
                adc     ah, ch
                add     bl, cl
		;mov	edi, ebx
                mov     di, bx
                add     bl, cl
                ;M_CalcSquare
		call	M_CalcSquare
                xor     ax, ax
                add     al, [ebx] ; [es:bx]		
                adc     ah, ch
                add     bh, cl
		;mov	edi, ebx
                mov     di, bx
                add     bh, cl
                ;M_CalcSquare
		call	M_CalcSquare
                xor     ax, ax
                add     al, [ebx] ; [es:bx]		
                adc     ah, ch
                sub     bl, cl
		;mov	edi, ebx
                mov     di, bx
                sub     bl, cl
		;M_CalcSquare
		call	M_CalcSquare
                xor     ax, ax
                add     al, [ebx] ; [es:bx]		
                adc     ah, ch
                sub     bh, cl
		;mov	edi, ebx
                mov     di, bx
                sub     bh, cl
                ;M_CalcSquare
		call	M_CalcSquare
                xor     ax, ax
                add     al, [ebx] ; [es:bx]	
                adc     ah, ch
                add     bl, cl
                add     bl, cl
                add     al, [ebx] ; [es:bx]		
                adc     ah, ch
                add     bh, cl
                add     bh, cl
                add     al, [ebx] ; [es:bx]		
                adc     ah, ch
                sub     bl, cl
                sub     bl, cl
                add     al, [ebx] ; [es:bx]	
                adc     ah, ch
                shr     ax, 2
                ;M_Calc2Square
		call	M_Calc2Square
                add     bl, cl
                sub     bh, cl
                mov     [ebx], al ; mov [es:bx], al
                push    bx
		push	cx
		call	CalcFrac
                pop     cx
                pop     bx
                sub     bl, cl
                push    bx
		push	cx
		call	CalcFrac
                pop     cx
                pop     bx
                sub     bh, cl
                push    bx
		push	cx
		call	CalcFrac
                pop     cx
                pop     bx
                add     bl, cl
		call	CalcFrac
		retn

  UpdateView:   ; sub_26FD
                mov     cx, [Map_X]
                mov     dx, [Map_Y]
                movzx	ebx, word [Count_Loop]
		;mov	bx, [Count_Loop]
                dec     bx
                shr     bx, 8
                shl     bx, 1
                add     cx, [ebx+AddCXTable]
                add     dx, [ebx+AddDXTable]
                mov     [Map_X], cx
                mov     [Map_Y], dx
; Calculate New Points
                mov	ebx, MapA_Seg ; mov es, [MapA_Seg]
                ror     cx, 4
                ror     dx, 4
                mov     bl, cl
                mov     bh, dl
		shr     cx, 0Ch
                shr     dx, 0Ch
                push    dx
                inc     bl
                xor     ax, ax
                add     al, [ebx] ; [es:bx]		
		dec	bl
                sub     al, [ebx] ; [es:bx]			
                sbb     ah, ah
                imul    cx
                mov     dx, ax
                xor     ax, ax
                add     al, [ebx] ; [es:bx]		
                shl     ax, 4
                add     ax, dx
                xchg    si, ax
                inc     bh
                inc     bl
                xor     ax, ax
                add     al, [ebx] ; [es:bx]		
		dec	bl
                sub     al, [ebx] ; [es:bx]			
                sbb     ah, ah
                imul    cx
                mov     dx, ax
                xor     ax, ax
                add     al, [ebx] ; [es:bx]			
                shl     ax, 4
                add     ax, dx
                pop     dx
                sub     ax, si
                imul    dx
                shl     si, 4
                add     ax, si
		xor	ebx, ebx
                add     ah, 1Ah
                jnc     short CalcNew1
                mov     ax, 0FFFFh
  CalcNew1:
                mov     [MountHeight], ax
                mov     ah, 1
                int	32h  ; TRDOS 386 Keyboard interrupt
                jz      short CalcNew2
                mov     word [Count_Loop], 1
                xor     ax, ax
                int	32h  ; TRDOS 386 Keyboard interrupt
  CalcNew2:
		retn

  UpdateSky:	; sub_2795
  		;mov	edi, Data_Seg ; mov es, [Data_Seg]
                mov     esi, MountHeight
                mov     edi, _TempR32A
                xor     eax, eax
                lodsw
		neg	ax
                shr     ax, 3
                add     ah, 40h
                shl     eax, 4
                shl     eax, 9
                stosd
                xor     eax, eax
                lodsw
                shl     eax, 9
                stosd
                xor     eax, eax
                lodsw
                shl     eax, 9
                stosd
                lea	ecx, [si-(MountHeight-Data_Seg)-6+63h]
		mov     edi, Table_C
  @UpdateSky1:
		mov	esi, _TempR32A
                mov     eax, [esi]   ; [_TempR32A]
                xor     edx, edx
                div     ecx  ; Data_Seg + 63h = 63h
                mov     ebx, [esi+4] ; [_TempR32B]
                mov     esi, [esi+8] ; [_TempR32C]
                sub     esi, eax
                add     ebx, eax
                shr     eax, 7
                mov     bp, si
                shr     esi, 10h
                and     si, 0FFh
                shr     ebx, 8
                xor     bl, bl
                or      si, bx
                mov     ebx, eax
                shr     ebx, 10h
                dec     bx

		push	cx
		mov	ecx, Sky_Seg ; gs
		mov	cx, si
		mov	esi, ecx
		mov	ecx, 255
_UpdateSky0:
                movsb	; gs:si, es:di  ; es = [Data_Seg]
                add     bp, ax
                adc     si, bx
		loop	_UpdateSky0
		pop	cx

                movsb
                dec     cx
                jnz     short @UpdateSky1
                mov     eax, 50505050h
                mov     cl, 40h
                rep     stosd
                movzx	esi, word [MountHeight]
                mov     bx, 4
  @UpdateSky2:
                mov     ax, si
                xor     dx, dx
                div     bx
                shr     ax, 7
                cmp     al, 3Fh
                jbe     short @UpdateSky3
                mov     al, 3Fh
  @UpdateSky3:
                or      al, 80h
                mov     ah, al
                mov     dx, ax
                shl     eax, 10h
                xchg    ax, dx
                mov     cl, 40h
                rep     stosd
                inc     bx
                cmp     bx, 2Ch
                jne     short @UpdateSky2
		retn

  UpdateMap:	; sub_2E43
                ;mov    fs, [MapA_Seg]
                ;mov    gs, [MapB_Seg]
		; edi = 1????h ; es = [Data_Seg]
                mov     eax, 7D007D00h
                mov     edi, Table_A
                mov     cx, 40h
                rep     stosd
                mov     cx, 40h
		rep	stosd
                xor     eax, eax
                mov     edi, Table_B
                mov     cx, 40h
                rep     stosd
                mov     cx, 40h
                rep     stosd
                mov     word [_TempR16A], 78h
  @UpdateMap1:
		mov	si, [_TempR16A]
                mov     si, [esi+MapMakeTable]
                shl     si, 4
                mov     ax, [Map_Y]
                and     ax, 0Fh
                xor     al, 0Fh
                add     si, ax
                mov     ax, [MountHeight]
                xor     dx, dx
		div	si
                add     ax, 64h
                mov     [_TempR16B], ax
		lea     eax, [si]
		shl     eax, 6
                mov     [_TempReg], eax
                cmp     word [_TempR16A], 2
                jne     short @UpdateMap2
                mov     word [_TempR16B], 7D00h
                mov     word [_TempR16C], 0000h
                jmp     short @UpdateMap3
  @UpdateMap2:
                xor     ax, ax
                mov     dx, 1
		div	si
                mov     [_TempR16C], ax
  @UpdateMap3:
                movzx	ecx, word [Map_X]
                shl     ecx, 0Ch
                mov     eax, [_TempReg]
                shl     eax, 7
                sub     ecx, eax
                mov     ax, [Map_Y]
                shl     ax, 4
                mov     ebx, ecx
                shr     ebx, 10h
                mov     bh, ah
                mov     ax, si
                shl     ax, 4
                add     bh, ah
		mov	esi, Table_C
		mov     word [_TempR32B], si
                mov     esi, 1FEh
		shr     cx, 1
  @UpdateMap4:
                shl     cx, 1
                add     cx, word [_TempReg]
                adc     bl, byte [_TempReg+2]
                shr     cx, 1

		mov	ebp, MapA_Seg ; mov fs, [MapA_Seg]
                mov	bp, bx
		
		mov     al, [ebp+1] ; [fs:bx+1]
                sub     al, [ebp]   ; [fs:bx]
                sbb     ah, ah
		imul	cx
                shrd    ax, dx, 7
                add     ah, [ebp] ; [fs:bx]
                mul     word [_TempR16C]
                mov     di, [_TempR16B]
                sub     di, dx
                jns     short @UpdateMap5
                mov     di, 0FFFFh
  @UpdateMap5:
                cmp     di, 0C8h
                jl      short @UpdateMap6
                mov     di, 0C7h
  @UpdateMap6:
                mov	ebp, MapB_Seg ; mov gs, [MapB_Seg]
                mov	bp, bx

                mov     al, [ebp+1] ; [gs:bx+1]
                sub     al, [ebp]   ; [gs:bx]
		imul	ch
                shl     ax, 1
                add     ah, [ebp]   ; [gs:bx]
                mov     dx, ax
                xchg    ax, [esi+Table_B]
                movzx   ebp, di
                xchg    bp, [esi+Table_A]
                sub     bp, di
                jns     short @UpdateMap7
                shl     di, 8
                add     di, word [_TempR32B]
                push    ax
                sub     ax, dx
		cwd
		idiv	bp
                pop     dx
                push    bx
                lea	bx, [bp+1]
		shl     bp, 2 ; * 4
                shl     bx, 8
		lea	bp, [ebp+ebp*2] ; ebp*12 -> -5x12 = -60 	
                add     bp, @UpdateMap8+0Ch  ; 12 -> 88343Bh, 6601C2h, FEC7h
		push	esi
		call    ebp
		pop	esi
                pop     bx
  @UpdateMap7:
                inc     word [_TempR32B]
                dec     si
                dec     si
                jns     @UpdateMap4
                sub     word [_TempR16A], 2
                jnz     @UpdateMap1
		xor	ecx, ecx
		retn

  @TheEND:
                ;M_KeyPressed
                mov     ah, 01h
                int	32h	; TRDOS 386 Keyboard interrupt
                jnz     @KeyDownOut
                
		call	M_WaitVRT_DX
                mov     al, 13h
                mov     ah, 00h
                int	31h ; TRDOS 386 - Video interrupt

                mov     edx, Temp_PAL2
                xor     bx, bx
                mov     cx, 256
                mov     al, 17h
                mov     ah, 10h
                int	31h ; TRDOS 386 - Video interrupt
              
                mov	edi, 20000h ; es = fs
             	mov     [TextSeg], edi ; es
                mov     ax, di ; 0
                mov     cx, 10000h/2
                rep     stosw
		;mov	edi, 20000h+44 ; es = fs

                mov     word [CharColor], 0E03h
                mov	di, 44
                mov     esi, Gfx4Text
                call    GfxWrite
                mov     word [x], 8000h
                mov     word [y], 0800h
                xor     bx, bx
                mov     di, 1008h
  @Rotate6Map: ; es = fs = 20000h
                push    bx
                push    di
                call    RotateMap2
                call	M_WaitVRT_DX
                pop     di
                pop     bx
                sub     di, 18h
                jnc     short @Rotate6Map
             
                ;M_FadeOUT Temp_PAL2, Temp_PAL1
		mov	si, Temp_PAL2 ; SourcePAL
		mov	edi, Temp_PAL1 ; DestPAL
		call	M_FadeOUT
                jmp     short @NoKeyPressed
  @KeyDownOut:
                call	M_WaitVRT_DX
                mov     al, 13h
                mov     ah, 00h
                int	31h ; TRDOS 386 - Video interrupt
                mov     cx, 20h
  @wv7:
                call	M_WaitVRT_DX
                loop    @wv7
                xor     ah, ah
                int	32h	; TRDOS 386 Keyboard interrupt
  @NoKeyPressed:
                call	M_WaitVRT_DX
                mov     al, 03h ; 80x25 16 color text
                mov     ah, 00h ; Set video mode
                int	31h ; TRDOS 386 - Video interrupt		     ; Switch Back to Char Mode

                ;mov    bx, 414Fh
		mov	bx, 4109h
                call	M_WaitVRT_DX
                call	M_WaitVRT
                mov     ah, bh
                mov     dx, 03D4h
                ;mov    al, 09h
                ;out    dx, ax
		mov	ah, 3 ; out (word in BX)
		int	34h ; TRDOS 386 - IOCTL interrupt

		; DIRECT VGA MEMORY ACCESS
		;xor	ebx, ebx
		mov	bh, 4 ; Direct access/map to CGA memory (0B8000h)
		mov	eax, _video ; 1Fh
			; sys _video ; TRDOS 386 Video functions
		int	40h   ; TRDOS 386 system call

		; eax = 0B8000h
		;and	eax, eax
		;jz     terminate ; error (eax = 0)
		;jz	exit_msg

                mov     cx, (EndPicCol-EndPic)
                mov     edi, 0B8000h
		mov     si, EndPic
  @PutPage:
                mov     ah, [esi+(EndPicCol-EndPic)]
                lodsb
                stosw
                loop    @PutPage
		
		mov     dx, 0700h
                mov     bh, 00h
                mov     ah, 02h ; set cursor position
                int	31h ; TRDOS 386 - Video interrupt

		;mov 	bx, 424Fh
                mov     bx, 4209h
  @IncCharHeight:
                call	M_WaitVRT_DX
                call	M_WaitVRT
                ;mov    ah, bh
                mov     dx, 03D4h
                ;mov    al, 09h
                ;out    dx, ax
		mov	ah, 3 ; out (word in BX)
		int	34h ; TRDOS 386 - IOCTL interrupt
                inc     bh
                ;cmp    bh, bl
		cmp	bh, 4Fh
                jbe     short @IncCharHeight
exit_msg:		
		mov	esi, prg_msg
		call	print_msg
terminate:
		sys 	_exit   ; INT 40h
here:
		jmp	short here

print_msg:
		mov	bx, 7
        	mov     ah, 0Eh
pmsg_loop:
		lodsb
		and	al, al
		jz	short pmsg_ok
		int	31h	; TRDOS 386 video interrupt
		jmp	short pmsg_loop	
pmsg_ok:
		retn

M_WriteText:
		; di = (60+TextXYMove)*320+120+TextXYMove
		; bx = (92+TextXYMove)*320+64+TextXYMove
		; dx = (124+TextXYMove)*320+88+TextXYMove

		; esi = 20000h + si ; fs: (si=0)
		; edi = 0A0000h + di

              	mov	cx, 8
M_WTxt_1:
		push	cx
                mov     cx, 5*8
_wt1_1:
		cmp     byte [esi], 0  ; fs:si
		jz      short _wt2_1
		mov     [edi], ax
		mov     [edi+320], ax
_wt2_1:
		add     di, 2
		inc     si
		loop    _wt1_1
		add     di, 240+320
                pop	cx
		loop	M_WTxt_1

              	mov     di, bx

              	mov	cl, 8
M_WTxt_2:
		push	cx
                mov     cx, 12*8
_wt1_2:
                cmp     byte [esi], 0
		jz      short _wt2_2
		mov     [edi], ax
		mov     [edi+320], ax
_wt2_2:
		add     di, 2
		inc     si
		loop    _wt1_2
		add     di, 128+320
                pop	cx
		loop	M_WTxt_2

		mov     di, dx

              	mov	cl, 8
M_WTxt_3:
		push	cx
		mov     cx, 9*8
_wt1_3:
		cmp     byte [esi], 0
		jz      short _wt2_3
		mov     [edi], ax
		mov     [edi+320], ax
_wt2_3:
		add     di, 2
		inc     si
		loop    _wt1_3
		add     di, 176+320
                pop	cx
		loop	M_WTxt_3
                retn

M_WaitVRT_DX:
		mov     dx, 03DAh
M_WaitVRT:
		mov	ah, 0 ; in (byte)
_wv1:
		;in     al, dx
		int	34h ; TRDOS 386 - IOCTL interrupt
		test    al, 8
		jz      short _wv1
_wv2:
		;in     al, dx
		;mov	ah, 0 ; in (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt
		test    al, 8
		jnz     short _wv2
                retn

M_ClearPAL:
                call	M_WaitVRT_DX
		mov     cx, 768
		mov     dx, 03C8h
		mov	ax, 0100h
		;out    dx, al
		;mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt
		inc     dx
_cpal:
		;out    dx, al
		;mov    al, 0
		;mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt
		loop    _cpal
                retn

M_FadeOUT:
		; esi = SourcePAL
		; edi = DestPAL

		; M_FadePAL_Out   SrcPAL, DstPAL
		mov    bx, 0200h
_fo1:
		push	esi
		push	edi
		mov     cx, 3*256
_fo2:
		lodsb
		sub     al, bl
		cmc
		sbb     ah, ah
		and     al, ah
		stosb
		loop    _fo2
		pop	esi	; DestPAL
		push	esi
		call	M_WaitVRT_DX
		mov     dx, 03C8h
		mov     al, 0
		;out    dx, al
		mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt

		inc     dx
		mov     cx, 3*256
		;rep    outsb
_fo3:
		lodsb
		;mov	ah, 1 ; out (byte)
		; al = data byte
		; dx = port number
		int	34h ; TRDOS 386 - IOCTL interrupt
		dec	cx
		jnz	short _fo3

		call	M_WaitVRT_DX
		pop	edi
		pop	esi
		add     bl, bh
		test    bl, 3Fh
		jnz     short _fo1

                retn

M_FadeIN:
		; esi = SourcePAL
		; edi = DestPAL

		; M_FadePAL_In    SrcPAL, DstPAL
		mov     bx, 0FE40h
_fi1:
		push	esi
		push	edi
		mov     cx, 3*256
_fi2:
		lodsb
		sub     al, bl
		cmc
		sbb     ah, ah
		and     al, ah
		stosb
		loop    _fi2
		pop	esi	; DestPAL
		push	esi
		call	M_WaitVRT_DX
		mov     dx, 03C8h
		mov     al, 00h
		;out    dx, al
		mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt
		
		inc     dx
		mov	cx, 3*256
		;rep     outsb
_fi3:
		lodsb
		;mov	ah, 1 ; out (byte)
		; al = data byte
		; dx = port number
		int	34h ; TRDOS 386 - IOCTL interrupt
		dec	cx
		jnz	short _fi3

		call	M_WaitVRT_DX
		pop	edi
		pop	esi
		add     bl, bh
		test    bl, 3Fh
		jnz     short _fi1

                retn

M_ShadeBOBs_2:
		mov	ax, TimeOut2
		jmp	short M_ShadeBOBs
M_ShadeBOBs_1:
		mov	ax, TimeOut
M_ShadeBOBs:
		push	ax
                call	M_WaitVRT_DX
		mov     dx, 03C8h
 		xor     al, al
		;out    dx, al
		mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt
		inc     dx
		mov     cx, 256
		;xor    ah, ah
		sub	bl, bl
_Set1Pal:
		;out    dx, al
		;mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt
		;mov    al, ah
		;out    dx, al
		mov	al, bl
		;mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt
		xor     al, al
		;out    dx, al
		;mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt
		;cmp    ah, 62
		cmp	bl, 62
		jae     short _Set1Brk
		;inc    ah
		inc	bl
_Set1Brk:
		loop    _Set1Pal
		xor     bp, bp
		pop	ax
		push	ax
		mov     [Frames], ax ; @Time
		mov	edi, 0A0000h ; es = 0A000h
		call    ShadesLoop
		call	M_WaitVRT_DX
		mov     dx, 03C8h
		;xor    ax, ax
		;out    dx, al
		
		xor	al, al
		mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt

		inc     dx
		mov     cx, 256
		sub	bl, bl ; 0
_Set2Pal:
		;mov    al, ah
		;out    dx, al
		mov	al, bl
		;mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt
		xor     al, al
		;out    dx, al
		;mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt
		;out    dx, al
		;mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt
		;cmp    ah, 62
		cmp	bl, 62
		jae     short _Set2Brk
		;inc    ah
		inc	bl
_Set2Brk:
		loop    _Set2Pal
		xor     bp, bp
		pop	ax
		push	ax
		mov     [Frames], ax ; @Time
		call    ShadesLoop
		call	M_WaitVRT_DX
		mov     dx, 03C8h
		;xor    ax, ax
		;out    dx, al
		
		xor	al, al
		mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt

		inc     dx
		mov     cx, 256
		xor	bl, bl ; 0
_Set3Pal:
		;out    dx, al
		;mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt
		;out    dx, al
		;mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt
		;mov    al, ah
		;out    dx, al
		mov	al, bl
		;mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt

		;cmp    ah, 62
		cmp	bl, 62
		jae     short _Set3Brk
		;inc    ah
		inc	bl
_Set3Brk:
		xor     al, al
		loop    _Set3Pal
		xor     bp, bp
		pop	ax
		mov     [Frames], ax ; @Time
		call    ShadesLoop
		xor	edi, edi ; es = cs
                retn

PutBop:		; sub_3116
                add     di, dx
                xor     esi, esi
                mov     cx, BopPts/4
@PutLoop:
                mov     ax, [esi+BopAddTab]
                mov     bx, [esi+BopTab]
                add     [ebx+edi], al ; edi = 0A????h ; es = 0A000h
                mov     ax, [esi+BopAddTab+2]
                mov     bx, [esi+BopTab+2]
                add     [ebx+edi], al ; edi = 0A????h ; es = 0A000h
                mov     ax, [esi+BopAddTab+4]
                mov     bx, [esi+BopTab+4]
                add     [ebx+edi], al ; edi = 0A????h ; es = 0A000h
                mov     ax, [esi+BopAddTab+6]
                mov     bx, [esi+BopTab+6]
                add     [ebx+edi], al ; edi = 0A????h ; es = 0A000h
                add     si, 8
                loop    @PutLoop
HideBop:	; sub_314E
                xchg    [ebp+BopQueue], di
                cmp     di, -1
                jae     short @SkipHide
                xor     si, si
                mov     cx, BopPts/4
@HideLoop:
                mov     ax, [esi+BopAddTab]
                mov     bx, [esi+BopTab]
                sub     [ebx+edi], al ; edi = 0A????h ; es = 0A000h
                mov     ax, [esi+BopAddTab+2]
                mov     bx, [esi+BopTab+2]
                sub     [ebx+edi], al ; edi = 0A????h ; es = 0A000h
                mov     ax, [esi+BopAddTab+4]
                mov     bx, [esi+BopTab+4]
                sub     [ebx+edi], al ; edi = 0A????h ; es = 0A000h
                mov     ax, [esi+BopAddTab+6]
                mov     bx, [esi+BopTab+6]
                sub     [ebx+edi], al ; edi = 0A????h ; es = 0A000h
                add     si, 8
                loop    @HideLoop
@SkipHide:
                add     bp, 2
                cmp     bp, 2*MaxBops
                sbb     bx, bx
                and     bp, bx
                retn

CalcPixel:	; sub_319A
                mov     bh, 4
@CalcPixel:
                or      bp, bp
                jns     @NoPixel
                fld     dword [II]		; L := ...
                fadd    dword [LL]
                fsincos
                fmul    dword [CosB]
                fld     dword [Cos1]
                fmul    st0, st1
                fld     dword [SinB]
                fmul    dword [Sin1]
                faddp   st1, st0
                fimul   word [Ker1]
                fistp   word [U]
                fwait
                mov     ax, [U]
                fmul    dword [Sin1]
                fld     dword [Cos1]
                fmul    dword [SinB]
                fsubrp  st1, st0
                fimul   word [Ker2]
                fistp   word [YY]
                fmul    dword [CosB]
                fimul   word [Ker2]
                fistp   word [XX]
                or      ax, ax
                js      @CCPopAX
                inc     ax
                and     al, 0Fh
                mov     cx, 320
                wait
                add     cx, [XX]
                mov     di, cx
                and     di, 7
                mov     dx, 03CEh
                mov     ah, [edi+MaskTable]
                mov     di, ax
 		;mov    al, 08h
		;out    dx, ax

		push	bx
		mov	bh, ah
		mov	bl, 08h
		mov	ah, 3 ; out (word in BX)
		int	34h ; TRDOS 386 - IOCTL interrupt
		pop	bx

                mov     ax, di
                mov     di, 240
                sub     di, [YY]
                shr     cx, 3
                shl     di, 4
                add     cx, di
                shl     di, 2
                add     di, cx
                ;add    di, 6A00h
		;add	edi, 0A0000h
                add	edi, 0A6A00h 
		mov     ah, [edi]
                stosb
		and	edi, 0FFFFh
@NoPixel:
                fld     dword [II]
                fadd    dword [IIAdd]
                fstp    dword [II]
                dec     bh
                jnz     @CalcPixel
                shl     bp, 1
                retn

PrintChar:      ; sub_3243	; si = Char Number, di = Place.
                mov	si, ax ; esi = si
		shl	si, 3
                add     esi, VGA_Fonts
                mov     dl, 08h
_MC1:
                lodsb
                mov     dh, al
                mov     ebx, [TextSeg]
		mov	bx, di
		mov	edi, ebx

                mov	cx, 8
_MC2:
		xor     ebx, ebx 
		shl     dh, 1
		adc     bx, CharColor
		mov     al, [ebx]
		stosb
                loop	_MC2

                add     di, 256-8       ;320-8
                dec     dl
                jnz     short _MC1
_MCOUT:
                sub     di, 8*256-8     ;8*320-8

                retn

CalcFractal: 	; sub_32C6
		; ebx = 20000h (1) or ebx = 28000h (2)
                mov     ax, [YMax]
                mov     [y], ax
                mov     word [ScreenY], 0
@RepeatY:
                mov     ax, [XMin]
                mov     [x], ax
                mov     word [ScreenX], 0
@RepeatX:
                xor     cx, cx
                mov     ax, [y]
                mov     [zi], ax
                mov     di, ax
                imul    di
                ;M_ShlDXAX 3
		call	M_ShlDXAX_3	
                mov     di, dx
                mov     ax, [x]
                mov     [zr], ax
                mov     si, ax
                imul    si
                ;M_ShlDXAX 3
		call	M_ShlDXAX_3	
                mov     si, dx
@RepeatCount:
                mov     ax, si
                sub     ax, di
                add     ax, [cr]
                mov     si, ax
                mov     ax, [zr]
                imul    word [zi]
                ;M_ShlDXAX 4
		call	M_ShlDXAX_4	
                add     dx, [ci]
                mov     di, dx
                mov     [zi], dx
                mov     [zr], si
                mov     ax, si
                imul    si
                ;M_ShlDXAX 3
		call	M_ShlDXAX_3	
                mov     si, dx
                mov     ax, di
                imul    di
                ;M_ShlDXAX 3
		call	M_ShlDXAX_3	
                mov     di, dx
                add     dx, si
                jc      short @CountOut
                js      short @CountOut
                inc     cx
                cmp     cx, [NMax]
                jna     short @RepeatCount
                xor     ax, ax
                jmp     short @Plot
@CountOut:
                mov     ax, cx
                mov     cl, 31
                div     cl
                mov     al, ah
                inc     al
@Plot:
                and     al, 1Fh
                add     al, 20h
                mov     si, [ScreenX]
                ;movzx  edi, word [ScreenY]
                mov	di, [ScreenY]
		mov	cx, di
		mov     di, [edi+Row256T]
		add     di, si
		mov	[ebx+edi], al ; stosb ; es=fs
		inc	di
                inc     word [ScreenX]
                mov     ax, [x]
                add     ax, [XStep]
                mov     [x], ax
                cmp     si, 288
                jb      @RepeatX
@OutM1:
                mov     dx, 3DAh
                ;in     al, dx
		mov	ah, 0 ; in (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt
                mov     dx, 3C0h
                mov     al, 11h+32
                ;out    dx, al
		mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt
                mov     al, cl
                shr     al, 1
                and     al, 0Fh
                ;out    dx, al
		;mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt
                add     word [ScreenY], 2
                mov     ax, [y]
                add     ax, [YStep]
                mov     [y], ax
                cmp     cx, 256
                jb      @RepeatY
@OutM2:
                retn

M_ShlDXAX_4:
		shl     ax, 1
		rcl     dx, 1
M_ShlDXAX_3:
		shl     ax, 1
		rcl     dx, 1
		shl     ax, 1
		rcl     dx, 1
		shl     ax, 1
		rcl     dx, 1
		retn

RotateMap:	; sub_3497
                ;M_Init_Rotate 1
		mov	ax, 1
		jmp	short M_Init_Rotate
RotateMap2:	; sub_33D6
		;M_Init_Rotate 0
		sub	ax, ax ; 0
		;jmp	short M_Init_Rotate
M_Init_Rotate:  ; sub_3497, sub_33D6
		push 	ax ; Method
		; es = 0A000h
		and     bh, 7
		mov     ax, [ebx+SinT]
		imul    di
		sar     dx, 1
		adc     dx, 0
		mov     bp, dx
		mov     ax, [ebx+CosT]
		imul    di
		sar     dx, 1
		adc     dx, 0
		mov     si, dx
		mov     ax, [ebx+Sin2T]
		imul    di
		sar     dx, 1
		adc     dx, 0
		mov     [d2y], dx
		imul    cx, dx, 100
		mov     ax, [ebx+Cos2T]
		imul    di
		sar     dx, 1
		adc     dx, 0
		mov     [d2x], dx
		imul    dx, 100
		imul    bx, si, 160
		add     bx, dx
		mov     ax, [x]
		sub     ax, bx
		imul    bx, bp, 160
		add     bx, cx
		mov     dx, [y]
		sub     dx, bx
		mov	edi, 0A0000h ; es = 0A000h
		mov	ebx, 20000h ; fs = cs + 2000h
_ir1:
		mov     [i], ax
		mov     [j], dx
		mov     cx, 320/4
_ir2:
		%rep	4
		add     ax, si
		add     dx, bp
		mov     bl, ah
		mov     bh, dh
		mov     bl, [ebx] ; [fs:bx] ; fs = cs + 2000h
		mov     [edi], bl ; es:di -> es = 0A000h
		inc     di
		%endrep
		loop    _ir2

                pop	ax ; Method
		push	ax
		or	ax, ax
		jz	short _ir3
		inc     si
		inc     bp
_ir3:
		mov     ax, [d2x]
		mov     dx, [d2y]
		add     ax, [i]
		add     dx, [j]
		cmp     di, 4*16000
		jb      _ir1
		pop	ax ; Method
		xor	edi, edi ; es = cs
		xor	ebx, ebx
		retn

RotatePAL20:	; sub_355A
                std
                mov     si, Temp_PAL3+3*1Fh-2
                mov     di, Temp_PAL3+3*20h-2
                mov     ax, [edi]
                mov     bl, [edi-1]
                mov     cx, 47
                rep     movsw
                mov     [edi+1], ax
                mov     [edi], bl
                cld
                mov     si, di
                call	M_WaitVRT_DX
                mov     dx, 03C8h
                mov     al, 20h
                ;out    dx, al
		mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt
                inc     dx
                mov     cx, 3*20h
_outsb2:
                ;rep    outsb
		lodsb
		;mov	ah, 1 ; out (byte)
		int	34h ; TRDOS 386 - IOCTL interrupt
		loop	_outsb2
                retn

  GfxWrite:	; sub_3592
                xor     ah, ah
  @WriteFPush:
                push    edi
  @WriteF:
                lodsb
                or      al, al
                jz      short @WriteFLN
                cmp     al, 0FFh
                je      short @WriteFOut
                push    esi
                call    PrintChar
                pop     esi
                jmp     short @WriteF
  @WriteFLN:
                pop     edi
                add     di, 8*256       ; 800h
                jmp     short @WriteFPush
  @WriteFOut:
                pop     edi
                and	edi, 0FFFFh ; es = cs
                retn

                %rep	199
		;89FEh,6601DEh,8836h,6601C2h,FEC7h ; 12 bytes
		mov	esi, edi
		add	si, bx
		mov     [esi], dh
		add     dx, ax
		inc     bh
                %endrep
  @UpdateMap8:
		mov	esi, edi
		add	si, bx
		mov     [esi], dh
                retn

M_MakeMUL:
		mov     ax, 0AFh
		mul     si
		add     ax, 2BC0h
		adc     dx, 0
		div     word [W_Divider]
		mov     si, dx
		retn

M_Calc2Square:
		mov     ch, al
		call	M_MakeMUL
		sub     dx, 67E8h
		xor     ax, ax
		mov     al, cl
		shl     ax, 3
		imul    dx
		xor     ax, ax
		add     dl, ch
		mov     ch, al
		adc     dh, ch
		js      short @@CS2
		jz      short @@CS1
		mov     dl, 0FEh
  @@CS1:
		mov     al, dl
  @@CS2:
        	retn

M_CalcSquare:
		cmp     byte [edi], 0FFh
		jne     short @@CS3
		add     al, [ebx]
		adc     ah, ch
		shr     ax, 1
		;M_Calc2Square
		call	M_Calc2Square	
		;stosb
		mov	[edi], al
		inc	di
  @@CS3:
                retn

EndPic:         db      '  ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป  '
                db      'ษอผ                                                                          ศอป'
                db      'บ Ü ÜÜÜ  ษออป บ  บ อหอ    ษออป ษออ    ษอหอป ษออ ษอหอป ษออป ษออป บ บ  บ   Ü ÜÜÜ บ'
                db      'บ Ü ÜÜÜ  บ  บ บ  บ  บ     บ  บ ฬอ     บ บ บ ฬอ  บ บ บ บ  บ ฬอหผ ศหผ  บ   Ü ÜÜÜ บ'
                db      'บ        ศออผ ศออผ  บ     ศออผ บ      บ บ บ ศออ บ บ บ ศออผ บ ศป  บ   þ         บ'
;               db      'บ                                 Released at Assembly 94 in Finland, (C) 1994 บ'
                db      'บ                          Released at Assembly 94 in Finland, (C) 1994 ไ, STP บ'
                db      'ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ'
                db      'ÛÛÛÛÛÛÛÛÛฑฑฑฑฑฑฑฑฐฐฐฐฐฐฐGIANT - the Ultimate 4KB-Intro!ฐฐฐฐฐฐฐฐฑฑฑฑฑฑฑฑÛÛÛÛÛÛÛÛÛ'
EndPicCol:      times	80 db 1Fh 
		times	80 db 1Fh 
		times	3 db 1Fh 
		db	11h 
		times	3 db 1Fh 
		times	66 db 1Ch 
		db	1Fh, 11h 
		times	5 db 1Fh
                times	3 db 1Fh
		db	11h, 
		times	3 db 1Fh
		times	66 db 1Eh 
		db	1Fh, 11h
		times	5 db 1Fh
                times	9 db 1Fh 
		times	62 db 1Ah 
		times	9 db 1Fh
                times	80 db 1Fh
		times	80 db 1Fh
		times	24 db 19h
		times	5 db 9Eh
		times	3 db 15h
		times	4 db 1Ah
		times	9 db 1Dh
                db	1Fh, 17h, 17h, 14h
		times	5 db 13h, 1Ch
		times	25 db 19h
Gfx1Text:       db      'ษออออออออออออออออออออออออออออป', 0
                db      'บPlease wait while Computing!บ', 0
                db      'ศออออออออออออออออออออออออออออผ', 0, -1
Gfx2Text:       db      'Is that all?', 0, -1
Gfx3Text:       db      'NO!', 0, -1
Gfx4Text:       db      'ษออออออออออออออออออออป', 0
                db      'บ That is all Folks! บ', 0
                db      'ศออออออออออออออออออออผ', 0, -1
Text1String:    db      ' Assembly 1994 *'
                db      ' GIANT 4KB-Intro'
Text2String:    db      'GIANT', 13, 10
                db      'the Ultimate', 13, 10
                db      '4KB-Intro', 0
                times   2       db 0
prg_msg:
		db 0Dh, 0Ah
		db 0Dh, 0Ah
		db "GIANT.PRG - TRDOS 386 (VGA Test) version of GIANT.COM by Erdogan Tan, 25/09/2016"
		db 0Dh, 0Ah, 0
_10A2h:
	dw      512  ; 1024 shr 1
	dw 0
_10A4h:
	dw 	8192
	dw 0
_10A6h:
	dw 	0
	dw 0

SinTable:
                times	64	db 0
CosTable:
                times   258     db 0
Frames          dw      0000h
U               dw      0
CharGen         dw      0
SinB            dd      0
CosB            dd      0
II              dd      0
JJ              dd      0
IIAdd           dd      0
LL              dd      0.0e00
BB              dd      -1.0e00
Const1          dd      2e-03
Const2          dd      0.09e00
Const3          dd      1.255e00
Const4          dd      0.0025e00
Sin1            dd      0.84147098480791e00
Cos1            dd      0.54030230586814e00
CosBB           dd      1.8508157176809e00
Mult1           dd      9830.4
Ker1            dw      14
Ker2            dw      240
XX              dw      80h
YY              dw      40h
MaskTable       db      80h, 40h, 20h, 10h, 08h, 04h, 02h, 01h
BopQueue:	times   MaxBops dw -1
Angle           dw      -1 & 0FFC0h
Phase1          dw      2*1024-PhInc1
Phase2          dw      2*1024-PhInc2
BopADDLengths   db      5,  8,  9, 11, 12, 13, 13, 14, 14, 15, 15, 15
               	times 	10 db 1
BopAddTab:
		%rep	4
		db                          1,1,1,1,1
		db                    1,1,1,2,2,2,2,2
		db                  1,1,2,2,2,3,3,3,3
		db              1,1,2,2,3,3,3,3,3,3,3
		db            1,1,2,2,3,3,3,3,4,4,4,4
		db          1,1,2,2,3,3,3,4,4,4,4,4,4
		db          1,2,2,3,3,3,4,4,4,5,5,5,5
		db        1,2,2,3,3,3,4,4,4,5,5,6,6,6
		db        1,2,2,3,3,4,4,5,5,5,6,6,6,6
		db      1,2,2,3,3,4,4,4,5,5,6,6,7,7,7
		db      1,2,2,3,3,4,4,5,5,6,6,7,7,7,7
		db      1,2,2,3,3,4,4,5,5,6,6,7,7,7,7
		%endrep
LastBopAdd:	
		times	4*144 db 0
BopTab: ; (*)
;		times   10 db 0	; in reserve for correct alignment
;		times	4 db 0
align 16

Last_Word:

Data_Seg       Equ 10000h
MapA_Seg       Equ 20000h
MapB_Seg       Equ 30000h
Sky_Seg        Equ 40000h
VRAM_Seg       Equ 0A0000h

MapMakeTable   Equ 10000h+0
LastMake_      Equ 10000h+120
AddCXTable     Equ 10000h+124
AddDXTable     Equ 10000h+132
MapCoord_      Equ 10000h+140
W_Divider      Equ 10000h+148

VGA_Fonts      EQU 16484

section .data   ;align=16

_Data_Seg:
_MapMakeTable:  times	41 dw 0
		times	10 dw 1
		times	9 dw 3
_LastMake_:     dw      0003h, 0000h
_AddCXTable:    dw       32, -16, -8, 8
_AddDXTable:    dw      -32,  -8, 16, 8
_MapCoord_:     dw      0000h, 0004h, 0202h, 0FEFFh
_W_Divider:     dw      0DB97h

_end: