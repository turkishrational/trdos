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

[BITS 16]

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

[ORG 100h]

  Start:
                sti
                mov     es, cx		  ; preparations for some nice settings
                mov     di, 0472h                       ; you'll know it for sure
                mov     ax, [ds:0002h]
                mov     dx, cs
                sub     ax, dx
                cmp     ax, 5000h                       ; This Intro needs 320K mem.
                jb      short @OutOfMemory
                cmp     sp, 0FFF8h
                jae     short @CheckFPU           ; if Enough Memory, continue
  @OutOfMemory:
                mov     al, 01h
                mov     ah, 4Ch
                int     21h
  @CheckFPU:		            ; check the existence of the Floating Point Unit
                finit
                xor     ax, ax
                stosw		           ; Use Cold Boot if necessary :)
                mov     es, dx
                mov     [Temp4], cx
                fstcw   [Temp4]
                wait
                cmp     byte [Temp4+1], 03h   ; Check for Floating Point Unit
                je      short @FPUFound
                mov     al, 03h
                mov     ah, 4Ch
                int     21h
  @FPUFound:
                pushf		           ; Check for 80286
                push    cx
                popf
                pushf
                pop     ax
                popf
                and     ax, 0F000h
                cmp     ax, 0F000h
                je      short @CPUBelow286
                mov     cx, 0F000h
                pushf		           ; Check for 80386
                push    cx
                popf
                pushf
                pop     ax
                popf
                and     ax, 0F000h
                jne     short @CPU386Plus
  @CPUBelow286:
                mov     al, 02h
                mov     ah, 4Ch
                int     21h
  @CPU386Plus:
                mov     ah, 1Ah
                mov     al, 00h
                int     10h		     ; Check for VGA
                cmp     bl, 07h
                jb      short @NoVGA
                cmp     al, 1Ah
                je      short @VGAFound
  @NoVGA:
                mov     al, 04h
                mov     ah, 4Ch
                int     21h
  @VGAFound:
                mov     ah, 30h
                int     21h		     ; Get DOS Version
                cmp     al, 03h
                jae     short @DOSOkay
                mov     al, 05h
                mov     ah, 4Ch
                int     21h
  @DOSOkay:
                mov     bl, 32h
                mov     al, 00h
                mov     ah, 12h
                int     10h
                mov     dx, 03CCh         ; set your VGA to
                in      al, dx		  ; correct mode...
                or      al, 01h		  ; B/W -> COLOR
                mov     dx, 03C2h
                out     dx, al
                mov     cx, 0FF00h
                mov     ah, 01h
                int     10h
                mov     al, 01h
                mov     ah, 35h
                int     21h		     ; Get BreakPoints Handler
                push    es
                push    bx
                mov     al, 03h
                mov     ah, 35h
                int     21h		     ; Get Single Step Handler
                push    es
                push    bx
                cli
                mov     al, 03h
                mov     ah, 25h
                mov     dx, 0F0F0h
                mov     ds, dx
                int     21h		     ; Disable Breakpoints
                mov     al, 01h
                mov     ah, 25h
                mov     dx, 0F0F0h
                mov     ds, dx
                int     21h		     ; Disable Single Step
                sti
                mov     ax, cs
                add     ax, ((Last_Word-Start)+0100h)/16
                mov     ds, ax
                mov     ax, cs
                add     ax, 1000h
                mov     es, ax
                mov     cx, ((W_Divider-Data_Seg)/4)+2
                mov     si, Data_Seg
                mov     di, si
                rep     movsd
  @KeyPressed1:
                mov     ah, 01h
                int     16h
                jz      short @KeyBufferCleared
                xor     ax, ax
                int     16h
                jmp     short @KeyPressed1
  @KeyBufferCleared:
                finit		           ; Make Sin and Cos Tables
                xor     si, si
                mov     ax, cs
                mov     es, ax
                mov     ds, ax
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
                fistp   word [si+SinTable]
                inc     si
                loop    @MakeSINCOS
                mov     dx, TempPAL1
                xor     bx, bx
                mov     cx, 256
                mov     al, 17h
                mov     ah, 10h
                int     10h
                ;M_FadeOUT TempPAL1, TempPAL2
		mov	si, TempPAL1 ; SourcePAL
		mov	di, TempPAL2 ; DestPAL
		call	M_FadeOUT
                wait
                ;M_KeyPressed
                mov     ah, 01h
                int     16h
                jnz     @KeyDownOut
                mov     al, 13h
                mov     ah, 00h
                int     10h
                mov     dx, TempPAL1
                xor     bx, bx
                mov     cx, 256
                mov     al, 17h
                mov     ah, 10h
                int     10h
                call	M_ClearPAL
                mov     dx, Text2String
                mov     ah, 9
                int     21h
                mov     ax, cs
                add     ax, 2000h
                mov     es, ax
                mov     fs, ax
                mov     ax, 0A000h
                mov     gs, ax
                xor     si, si
                xor     di, di

                mov	cl, 8
rept_8_1:
		push	cx
                mov     cx, 5*8/4
                rep
                db      65h                     ; GS segment override prefix
                movsd
                add     si, (320-5*8)
                pop	cx
		loop	rept_8_1
                mov	cl, 8
rept_8_2:
		push	cx
                mov     cx, 12*8/4
                rep
                db      65h                     ; GS segment override prefix
                movsd
                add     si, (320-12*8)
		pop	cx
		loop	rept_8_2
                mov	cl, 8
rept_8_3:
		push	cx
                mov     cx, 9*8/4
                rep
                db      65h                     ; GS segment override prefix
                movsd
                add     si, (320-9*8)
		pop	cx
                loop	rept_8_3

                mov     ax, 0A000h
                mov     es, ax
                mov     cx, 199*256
  @BG1:
                mov     dx, 319
  @BG2:
                mov     di, cx
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
                xor     si, si

                ; M_WriteText 1
		; di = (60+TextXYMove)*320+120+TextXYMove
		; bx = (92+TextXYMove)*320+64+TextXYMove
		; dx = (124+TextXYMove)*320+88+TextXYMove
		mov	di, ((60+1)*320)+120+1
		mov	bx, ((92+1)*320)+64+1
		mov	dx, ((124+1)*320)+88+1
		call	M_WriteText

                xor     ax, ax
                xor     si, si

                ; M_WriteText -1
		; di = (60+TextXYMove)*320+120+TextXYMove
		; bx = (92+TextXYMove)*320+64+TextXYMove
		; dx = (124+TextXYMove)*320+88+TextXYMove
		mov	di, ((60-1)*320)+120-1
		mov	bx, ((92-1)*320)+64-1
		mov	dx, ((124-1)*320)+88-1
		call	M_WriteText

                mov     ax, cs
                mov     ds, ax
                mov     es, ax
                ;M_FadeIN TempPAL1, TempPAL2
		mov	si, TempPAL1 ; SourcePAL
		mov	di, TempPAL2 ; DestPAL
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
                int     16h
                jnz     @KeyDownOut

                mov     al, 12h
                mov     ah, 00h
                int     10h
                mov     cx, 20h
  @wv2:
                call	M_WaitVRT_DX
                loop    @wv2
                mov     bh, 03h
                mov     al, 30h
                mov     ah, 11h
                int     10h
                mov     [CharGen], bp
                push    es
                pop     fs
                push    ax
                mov     bl, 15
  @SetNextPAL:
                mov     bh, bl
                mov     al, 00h
                mov     ah, 10h
                int     10h
                add     cx, 4
                dec     bl
                jns     short @SetNextPAL
                call	M_WaitVRT_DX
                mov     dx, 3D4h
                mov     ax, 6A0Ch
                out     dx, ax
                mov     dx, 03C8h
                xor     al, al
                out     dx, al
                inc     dx
                mov     cx, 16*3
                xor     ax, ax
  @SetPal:
                xor     al, al
                out     dx, al
                out     dx, al
                mov     al, ah
                out     dx, al
                add     ah, 4
                loop    @SetPal
                mov     dx, 03CEh
                mov     ax, 0205h
                out     dx, ax
                finit
                mov     ax, 0A000h
                mov     es, ax
                xor     si, si
                xor     ax, ax
                xor     di, di
                mov     cx, 65536/2
                rep     stosw
  @CCPopAX:
                pop     ax
  @CC:
                mov     bp, [CharGen]
                xor     ebx, ebx
                mov     bl, [si+Text1String]
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
                ja      short @GlobeOut
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
                dec     bp
  @BPOK:
                push    bp
                mov     bp, [fs:bp+0]

		%rep	8
		call    CalcPixel
		%endrep

                pop     bp
                fld     dword [JJ]
                fadd    dword [Const4]
                fstp    dword [JJ]
                dec     bx
                jnz     short @BB
                jmp     @CC
  @GlobeOut:
                push    cs
                pop     es
                mov     dx, TempPAL1
                xor     bx, bx
                mov     cx, 256
                mov     al, 17h
                mov     ah, 10h
                int     10h
                mov     cx, 250
  @wv3:
                call	M_WaitVRT_DX
                loop    @wv3
                ;M_KeyPressed
                mov     ah, 01h
                int     16h
                jnz     @KeyDownOut
                mov     dx, 03CEh
                mov     ax, 0FF08h
                out     dx, ax
                mov     bx, 6A00h
  @ScrollDown:
                call	M_WaitVRT_DX
                mov     dx, 3D4h
                mov     al, 0Dh
                mov     ah, bl
                out     dx, ax
                dec     ax
                mov     ah, bh
                out     dx, ax
                sub     bx, 80*2
                or      bx, bx
                jns     short @ScrollDown
                ;M_FadeOUT TempPAL1, TempPAL2
		mov	si, TempPAL1 ; SourcePAL
		mov	di, TempPAL2 ; DestPAL
		call	M_FadeOUT
                cli
                mov     al, 03h
                mov     ah, 25h
                mov     dx, 0F0F0h
                mov     ds, dx
                int     21h		     ; Disable Breakpoints
                mov     al, 01h
                mov     ah, 25h
                mov     dx, 0F0F0h
                mov     ds, dx
                int     21h		     ; Disable Single Step
                mov     ax, cs
                mov     ds, ax
                mov     es, ax
                sti
                ;M_KeyPressed
                mov     ah, 01h
                int     16h
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
                int     10h

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
                mov     cl, [si]
                neg     ax
                sub     ax, cx
  @Expand2:
                inc     ax
                stosw
                loop    @Expand2
                add     bp, dx
                mov     di, bp
                mov     cl, [si]
                add     ax, cx
  @Expand3:
                stosw
                dec     ax
                loop    @Expand3
                add     bp, dx
                mov     di, bp
                mov     cl, [si]
                add     ax, 320
                neg     ax
                sub     ax, cx
  @Expand4:
                inc     ax
                stosw
                loop    @Expand4
                add     bp, dx
                mov     di, bp
                mov     cl, [si]
                add     ax, cx
  @Expand5:
                stosw
                dec     ax
                loop    @Expand5
                jnz     short @Expand1
                mov     ax, 0A000h
                mov     es, ax

                ;M_ShadeBOBs TimeOut
		call	M_ShadeBOBs_1
                ;M_KeyPressed
                mov     ah, 01h
                int     16h
                jnz     @KeyDownOut
                mov     cx, 30
  @FlashBOBs:
                push    cx
                ;M_ShadeBOBs TimeOut2
		call	M_ShadeBOBs_2
                pop     cx
                loop    @FlashBOBs
                jmp     short @BOBs_Out
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
                mov     ah, [bx+CosTable]
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
                mov     ah, [bx+SinTable]
                mov     al, RadiusY
                imul    ah
                sar     ax, 6
                add     ax, CenterY
                imul    dx, ax, MaxWidth
                call    PutBop
                dec     word [Frames]
                jne     short ShadesLoop
                mov     dx, MaxBops
  @HideBOBsLoop:
                mov     di, -1
                call    HideBop
                dec     dx
                jnz     short @HideBOBsLoop
                retn
  @BOBs_Out:
                cli
                mov     al, 03h
                mov     ah, 25h
                mov     dx, 0F0F0h
                mov     ds, dx
                int     21h		     ; Disable Breakpoints
                mov     al, 01h
                mov     ah, 25h
                mov     dx, 0F0F0h
                mov     ds, dx
                int     21h		     ; Disable Single Step
                mov     ax, cs
                mov     ds, ax
                mov     es, ax
                sti
                finit		           ; Make Sin and Cos Tables
                xor     si, si
                ;mov     ax, cs
                ;mov     ds, ax
                ;mov     es, ax
                add     ax, 2000h
                mov     fs, ax
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
                fistp   word [si+Sin2T]
                fmul    st0, st2
                fistp   word [si+SinT]
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
                loop    @MakeRow256
                mov     bh, 03h
                mov     al, 30h
                mov     ah, 11h
                int     10h
                mov     [CharP+0], bp
                mov     [CharP+2], es
                mov     dx, cs
                mov     es, dx
                ;M_KeyPressed
                mov     ah, 01h
                int     16h
                jnz     @KeyDownOut
                call	M_WaitVRT_DX
                mov     al, 13h
                mov     ah, 00h
                int     10h
                mov     dx, Temp_PAL2
                xor     bx, bx
                mov     cx, 256
                mov     al, 17h
                mov     ah, 10h
                int     10h
                cli
                mov     si, Temp_PAL2+3*20h
                mov     di, Temp_PAL3
                mov     cx, 3*10h
                rep     movsw
                call	M_ClearPAL
                sti
                push    fs
                pop     es
                xor     di, di
                mov     ax, di
                mov     cx, 32*100h
                rep     stosw
                mov     [TextSeg], es
                mov     word [CharColor], 0F09h
                mov     di, 4*100h+8
                mov     si, Gfx1Text
                call    GfxWrite
                push    fs
                pop     es
                mov     si, 0*100h
                mov     di, 4*8*100h
                mov     cx, 7*4*8*128
                rep
                db      26h		     ; ES segment override prefix
                movsw
                mov     word [x], 7A00h
                mov     word [y], 2176
                xor     bx, bx
                mov     di, 40
		call    RotateMap2
                push    cs
                pop     es
                ;M_FadeIN Temp_PAL2, Temp_PAL1
		mov	si, Temp_PAL2 ; SourcePAL
		mov	di, Temp_PAL1 ; DestPAL
		call	M_FadeIN
                xor     bx, bx
                mov     di, 40
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

                push    cs
                pop     es
                ;M_KeyPressed
                mov     ah, 01h
                int     16h
                jnz     @KeyDownOut
                mov     word [XMin], -5500
                mov     word [YMax], 0
                mov     word [NMax], 512         ;256
                mov     word [cr], 2100
                mov     word [ci], 0
                mov     word [XStep], 10
                mov     word [YStep], 10
                push    fs
                pop     es
                call    CalcFractal
                mov     word [YMax], 1280+0
                mov     ax, es
                add     ah, 08h
                mov     es, ax
                call    CalcFractal
                mov     ax, cs
                mov     es, ax
                ;M_FadeOUT Temp_PAL2, Temp_PAL1
		mov	si, Temp_PAL2 ; SourcePAL
		mov	di, Temp_PAL1 ; DestPAL
		call	M_FadeOUT
                ;M_KeyPressed
                mov     ah, 01h
                int     16h
                jnz     @KeyDownOut
                mov     si, TempPAL2
                mov     cx, 3*256
                call	M_WaitVRT_DX
                mov     dx, 03C8h
                mov     al, 00h
                out     dx, al
                inc     dx
                rep     outsb
                sti
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
                int     16h
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
                int     16h
                jnz     @KeyDownOut
		;M_WaitVRT_DX
                call	M_WaitVRT_DX
                mov     al, 13h
                mov     ah, 00h
                int     10h
                push    fs
                pop     es
                xor     di, di
                mov     ax, di
                mov     cx, 10000h/2
                rep     stosw
                mov     [TextSeg], es
                mov     word [CharColor], 0C00h
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
                int     16h
                jnz     @KeyDownOut
                push    fs
                pop     es
                xor     di, di
                mov     ax, di
                mov     cx, 10000h/2
                rep     stosw
                mov     [TextSeg], es
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
                int     16h
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
        Table_A         Equ     0200h
        Table_B         Equ     0400h
        Table_C         Equ     0600h

                mov     ax, cs
                add     ax, 1000h
                mov     ds, ax
                mov     es, ax
                mov     di, Data_Seg ; 0
                stosw
                add     ax, 1000h
                stosw
                add     ax, 1000h
                stosw
                add     ax, 1000h
                stosw
                stc
                sbb     eax, eax
                mov     cx, ((LastMake_-MapMakeTable)/2)+1 ; 3Dh
  @Decompress2:
                add     ax, [di]
                inc     ax
                stosw
                loop    @Decompress2
                xor     ax, ax
                int     1Ah
                and     dh, 7Fh
;               mov     dx, 7FFFh               ; just guess why used in here...
                mov     [RandomSeed], dx
                mov     ax, 0013h
                int     10h
                cli
                mov     dx, 03C8h
                xor     al, al
                out     dx, al
                inc     dx
                mov     cl, 64
  @SetPal1:
                out     dx, al
                out     dx, al
                out     dx, al
                inc     al
                loop    @SetPal1
                mov     cl, 64
                ;xor     ah, ah
  @SetPal2:
                xor     al, al
                out     dx, al
                out     dx, al
                mov     al, ah
                out     dx, al
                inc     ah
                loop    @SetPal2
                mov     cl, 64
                xor     ax, ax
  @SetPal3:
                out     dx, al
                mov     al, ah
                out     dx, al
                xor     al, al
                out     dx, al
                inc     ah
                loop    @SetPal3
                mov     cl, 64
  @SetPal4:
                out     dx, al
                out     dx, al
                out     dx, al
                loop    @SetPal4
                sti
                mov     dx, Temp_PAL2
                xor     bx, bx
                mov     cx, 256
                mov     al, 17h
                mov     ah, 10h
                int     10h
                cli
                call	M_ClearPAL
                sti
                call    CalcSky
                mov     word [Count_Loop], 1024
                mov     word [Map_X], 1024
                mov     word [Map_Y], 1024
                call    UpdateView
                mov     es, [Data_Seg]
                call    UpdateSky
                call    UpdateMap
                call	M_WaitVRT_DX
                mov     es, [VRAM_Seg]
                mov     si, Table_C
                mov     di, (320-256)/2
                mov     bl, 200
  @Copy2VRAM:
                mov     cx, 256/4
                rep     movsd
                add     di, (320-256)
		dec	bl
                jnz     short @Copy2VRAM
                mov     es, [Data_Seg]
                ;M_FadeIN Temp_PAL2, Temp_PAL1
		mov	si, Temp_PAL2 ; SourcePAL
		mov	di, Temp_PAL1 ; DestPAL
		call	M_FadeIN
                mov     cx, 250
  @wv6:
                call	M_WaitVRT_DX
                loop    @wv6
  @DrawAgain:
                call    UpdateView
                mov     es, [Data_Seg]
                call    UpdateSky
                call    UpdateMap
                call	M_WaitVRT_DX
                mov     es, [VRAM_Seg]
                mov     si, Table_C
                mov     di, (320-256)/2
                mov     bl, 200
  @CopyToVRAM:
                mov     cx, 256/4
                rep     movsd
                add     di, (320-256)
		dec	bl
                jnz     short @CopyToVRAM
                dec     word [Count_Loop]
                jnz     short @DrawAgain
                mov     es, [Data_Seg]
                ;M_FadeOUT Temp_PAL2, Temp_PAL1
		mov	si, Temp_PAL2 ; SourcePAL
		mov	di, Temp_PAL1 ; DestPAL
		call	M_FadeOUT
                jmp     @TheEND
  CalcSky:	; sub_244E
                xor     di, di
                mov     es, [Sky_Seg]
                mov     fs, [MapB_Seg]
                stc
                sbb     eax, eax
                mov     cx, 4000h
                rep     stosd
                mov     si, [RandomSeed]

                call	M_MakeMUL
                mov     [RandomSeed], dx
                xor     bx, bx
                mov     cx, 0100h
                mov     byte [es:0080h], -2
                mov     byte [es:8000h], -2
                mov     byte [es:0000h],  cl
                mov     byte [es:8080h],  cl
		call	CalcFrac
                xor     di, di
  CalcSky1:
                mov     ah, 01h
                mov     al, [es:di]
                shr     ax, 2
                stosb	
                or      di, di
		jnz	short CalcSky1
;  CalcMap:
                mov     es, [MapA_Seg]
                mov     fs, [MapB_Seg]
                stc
                sbb     eax, eax
                mov     cx, 4000h
		rep	stosd
                mov     si, [RandomSeed]
                call	M_MakeMUL
                mov     [RandomSeed], dx
                xor     bx, bx
                mov     cx, 0100h
                mov     al, 40
                stosb
		call	CalcFrac
                xor     di, di
  CalcMap1:
                xor     ax, ax
                mov     si, MapCoord_
                mov     bx, [si]
                add     al, [es:bx+di]
                adc     ah, ch
                inc     si
                inc     si
                mov     bx, [si]
                add     al, [es:bx+di]
                adc     ah, ch
                inc     si
                inc     si
                mov     bx, [si]
                add     al, [es:bx+di]
                adc     ah, ch
                inc     si
                inc     si
                mov     bx, [si]
                add     al, [es:bx+di]
                adc     ah, ch
                shr     ax, 2
                stosb
                or      di, di
                jnz     short CalcMap1
                xor     si,si
  CalcMap2:
                mov     al, [es:si]
                sub     al, [es:si+3]
                sbb     ah, ah
                add     ax, 20h
                jns     short CalcMap3
                xor     ax, ax
  CalcMap3:
                cmp     al, 3Fh
                jbe     short CalcMap4
                mov     al, 3Fh
  CalcMap4:
		mov	[fs:si],al
                inc     si
                jnz     short CalcMap2
  CalcMap5:
                xor     ax, ax
                add     al, [es:di]
                adc     ah, ch
                add     al, [es:di+0100h]
                adc     ah, ch
                inc     di
                add     al, [es:di]
                adc     ah, ch
                add     al, [es:di+0100h]
                adc     ah, ch
                dec     di
                shr     ax, 2
                stosb
                or      di, di
                jnz     short CalcMap5
  OutCalcFrac:
                retn

  CalcFrac:	; sub_2552
                shr     cx, 1
                jz      short OutCalcFrac
                xor     ax, ax
                add     al, [es:bx]
                adc     ah, ch
                add     bl, cl
                mov     di, bx
                add     bl, cl
                ;M_CalcSquare
		call	M_CalcSquare
                xor     ax, ax
                add     al, [es:bx]		
                adc     ah, ch
                add     bh, cl
                mov     di, bx
                add     bh, cl
                ;M_CalcSquare
		call	M_CalcSquare
                xor     ax, ax
                add     al, [es:bx]		
                adc     ah, ch
                sub     bl, cl
                mov     di, bx
                sub     bl, cl
		;M_CalcSquare
		call	M_CalcSquare
                xor     ax, ax
                add     al, [es:bx]		
                adc     ah, ch
                sub     bh, cl
                mov     di, bx
                sub     bh, cl
                ;M_CalcSquare
		call	M_CalcSquare
                xor     ax, ax
                add     al, [es:bx]		
                adc     ah, ch
                add     bl, cl
                add     bl, cl
                add     al, [es:bx]		
                adc     ah, ch
                add     bh, cl
                add     bh, cl
                add     al, [es:bx]		
                adc     ah, ch
                sub     bl, cl
                sub     bl, cl
                add     al, [es:bx]
                adc     ah, ch
                shr     ax, 2
                ;M_Calc2Square
		call	M_Calc2Square
                add     bl, cl
                sub     bh, cl
                mov     [es:bx], al
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
                mov     bx, [Count_Loop]
                dec     bx
                shr     bx, 8
                shl     bx, 1
                add     cx, [bx+AddCXTable]
                add     dx, [bx+AddDXTable]
                mov     [Map_X], cx
                mov     [Map_Y], dx
; Calculate New Points
                mov     es, [MapA_Seg]
                ror     cx, 4
                ror     dx, 4
                mov     bl, cl
                mov     bh, dl
                shr     cx, 0Ch
                shr     dx, 0Ch
                push    dx
                inc     bl
                xor     ax, ax
                add     al, [es:bx]		
		dec	bl
                sub     al, [es:bx]		
                sbb     ah, ah
                imul    cx
                mov     dx, ax
                xor     ax, ax
                add     al, [es:bx]		
                shl     ax, 4
                add     ax, dx
                xchg    si, ax
                inc     bh
                inc     bl
                xor     ax, ax
                add     al, [es:bx]		
		dec	bl
                sub     al, [es:bx]		
                sbb     ah, ah
                imul    cx
                mov     dx, ax
                xor     ax, ax
                add     al, [es:bx]		
                shl     ax, 4
                add     ax, dx
                pop     dx
                sub     ax, si
                imul    dx
                shl     si, 4
                add     ax, si
                add     ah, 1Ah
                jnc     short CalcNew1
                mov     ax, 0FFFFh
  CalcNew1:
                mov     [MountHeight], ax
                mov     ah, 01h
                int     16h
                jz      short CalcNew2
                mov     word [Count_Loop], 1
                xor     ax, ax
                int     16h
  CalcNew2:
		retn
  UpdateSky:	; sub_2795
                mov     si, MountHeight
                mov     di, _TempR32A
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
                mov     gs, [Sky_Seg]
                lea     ecx, [si-(MountHeight-Data_Seg)-6+63h]
                mov     di, Table_C
  @UpdateSky1:
                mov     si, _TempR32A
                mov     eax, [si]
                xor     edx, edx
                div     ecx
                mov     ebx, [si+4]
                mov     esi, [si+8]
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
		mov	cx, 255
_UpdateSky0:
                db      65h		     ; GS segment override prefix
                movsb
                add     bp, ax
                adc     si, bx
		loop	_UpdateSky0
		pop	cx

                db      65h		     ; GS segment override prefix
                movsb
                dec     cx
                jnz     short @UpdateSky1
                mov     eax, 50505050h
                mov     cx, 40h
                rep     stosd
                mov     si, [MountHeight]
                mov     bx, 04
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
                mov     cx, 40h
                rep     stosd
                inc     bx
                cmp     bx, 2Ch
                jne     short @UpdateSky2
		retn
  UpdateMap:	; sub_2E43
                mov     fs, [MapA_Seg]
                mov     gs, [MapB_Seg]
                mov     eax, 7D007D00h
                mov     di, Table_A
                mov     cx, 40h
                rep     stosd
                mov     cx, 40h
		rep	stosd
                xor     eax, eax
                mov     di, Table_B
                mov     cx, 40h
                rep     stosd
                mov     cx, 40h
                rep     stosd
                mov     word [_TempR16A], 78h
  @UpdateMap1:
                mov     si, [_TempR16A]
                mov     si, [si+MapMakeTable]
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
                xor     ecx, ecx
                mov     cx, [Map_X]
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
                mov     si, 1FEh
                mov     word [_TempR32B], Table_C
                shr     cx, 1
  @UpdateMap4:
                shl     cx, 1
                add     cx, word [_TempReg]
                adc     bl, byte [_TempReg+2]
                shr     cx, 1
                mov     al, [fs:bx+1]
                sub     al, [fs:bx]
                sbb     ah, ah
		imul	cx
                shrd    ax, dx, 7
                add     ah, [fs:bx]
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
                mov     al, [gs:bx+1]
                sub     al, [gs:bx]
		imul	ch
                shl     ax, 1
                add     ah, [gs:bx]
                mov     dx, ax
                xchg    ax, [si+Table_B]
                mov     bp, di
                xchg    bp, [si+Table_A]
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
                lea     bx, [bp+1]
                shl     bp, 1
                shl     bx, 8
                lea     bp, [ebp+ebp*2+0]
                add     bp, @UpdateMap8+06h
                call    bp
                pop     bx
  @UpdateMap7:
                inc     word [_TempR32B]
                dec     si
                dec     si
                jns     short @UpdateMap4
                sub     word [_TempR16A], 2
                jnz     @UpdateMap1
		retn

  @TheEND:
                ;M_KeyPressed
                mov     ah, 01h
                int     16h
                jnz     short @KeyDownOut
                mov     ax, cs
                mov     ds, ax
                mov     es, ax
                add     ax, 2000h
                mov     fs, ax
                call	M_WaitVRT_DX
                mov     al, 13h
                mov     ah, 00h
                int     10h
                mov     dx, Temp_PAL2
                xor     bx, bx
                mov     cx, 256
                mov     al, 17h
                mov     ah, 10h
                int     10h
                push    fs
                pop     es
                xor     di, di
                mov     ax, di
                mov     cx, 10000h/2
                rep     stosw
                mov     [TextSeg], es
                mov     word [CharColor], 0E03h
                mov     di, 44
                mov     si, Gfx4Text
                call    GfxWrite
                mov     word [x], 8000h
                mov     word [y], 0800h
                xor     bx, bx
                mov     di, 1008h
  @Rotate6Map:
                push    bx
                push    di
                call    RotateMap2
                call	M_WaitVRT_DX
                pop     di
                pop     bx
                sub     di, 18h
                jnc     short @Rotate6Map
                mov     ax, cs
                mov     es, ax
                mov     ds, ax
                ;M_FadeOUT Temp_PAL2, Temp_PAL1
		mov	si, Temp_PAL2 ; SourcePAL
		mov	di, Temp_PAL1 ; DestPAL
		call	M_FadeOUT
                jmp     @NoKeyPressed
  @KeyDownOut:
                call	M_WaitVRT_DX
                mov     al, 13h
                mov     ah, 00h
                int     10h
                mov     cx, 20h
  @wv7:
                call	M_WaitVRT_DX
                loop    @wv7
                xor     ax, ax
                int     16h
  @NoKeyPressed:
                call	M_WaitVRT_DX
                mov     al, 03h
                mov     ah, 00h
                int     10h		     ; Switch Back to Char Mode
                cli
                mov     bx, 414Fh
                call	M_WaitVRT_DX
                call	M_WaitVRT
                mov     ah, bh
                mov     dx, 03D4h
                mov     al, 09h
                out     dx, ax
                mov     cx, (EndPicCol-EndPic)
                mov     dx, 0B800h
                mov     es, dx
                xor     di, di
                mov     si, EndPic
  @PutPage:
                mov     ah, [si+(EndPicCol-EndPic)]
                lodsb
                stosw
                loop    @PutPage
                sti
                mov     dx, 0700h
                mov     bh, 00h
                mov     ah, 02h
                int     10h
                cli
                mov     bx, 424Fh
  @IncCharHeight:
                call	M_WaitVRT_DX
                call	M_WaitVRT
                mov     ah, bh
                mov     dx, 03D4h
                mov     al, 09h
                out     dx, ax
                inc     bh
                cmp     bh, bl
                jbe     short @IncCharHeight
                sti
                mov     al, 03h
                mov     ah, 25h
                pop     dx
                pop     ds
                int     21h		     ; Enable Breakpoints
                mov     al, 01h
                mov     ah, 25h
                pop     dx
                pop     ds
                int     21h		     ; Enable Single Step
                mov     al, 00h
                mov     ah, 4Ch
                int     21h

M_WriteText:
		; di = (60+TextXYMove)*320+120+TextXYMove
		; bx = (92+TextXYMove)*320+64+TextXYMove
		; dx = (124+TextXYMove)*320+88+TextXYMove

              	mov	cx, 8
M_WTxt_1:
		push	cx
                mov     cx, 5*8
_wt1_1:
		cmp     byte [fs:si], 0
		jz      short _wt2_1
		mov     [es:di], ax
		mov     [es:di+320], ax
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
                cmp     byte [fs:si], 0
		jz      short _wt2_2
		mov     [es:di], ax
		mov     [es:di+320], ax
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
		cmp     byte [fs:si], 0
		jz      short _wt2_3
		mov     [es:di], ax
		mov     [es:di+320], ax
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
_wv1:
		in      al, dx
		test    al, 8
		jz      short _wv1
_wv2:
		in      al, dx
		test    al, 8
		jnz     short _wv2
                retn

M_ClearPAL:
                call	M_WaitVRT_DX
		mov     cx, 768
		mov     dx, 03C8h
		xor     ax, ax
		out     dx, al
		inc     dx
_cpal:
		out     dx, al
		loop    _cpal
                retn

M_FadeOUT:
		; si = SourcePAL
		; di = DestPAL
		cli
		; M_FadePAL_Out   SrcPAL, DstPAL
		mov    bx, 0200h
_fo1:
		push	si
		push	di
		mov     cx, 3*256
_fo2:
		lodsb
		sub     al, bl
		cmc
		sbb     ah, ah
		and     al, ah
		stosb
		loop    _fo2
		pop	si	; DestPAL
		push	si
		mov     cx, 3*256
		call	M_WaitVRT_DX
		mov     dx, 03C8h
		mov     al, 00h
		out     dx, al
		inc     dx
		rep     outsb
		call	M_WaitVRT_DX
		pop	di
		pop	si
		add     bl, bh
		test    bl, 3Fh
		jnz     short _fo1
		sti
                retn

M_FadeIN:
		; si = SourcePAL
		; di = DestPAL
		cli
		; M_FadePAL_In    SrcPAL, DstPAL
		mov     bx, 0FE40h
_fi1:
		push	si
		push	di
		mov     cx, 3*256
_fi2:
		lodsb
		sub     al, bl
		cmc
		sbb     ah, ah
		and     al, ah
		stosb
		loop    _fi2
		pop	si	; DestPAL
		push	si
		mov     cx, 3*256
		call	M_WaitVRT_DX
		mov     dx, 03C8h
		mov     al, 00h
		out     dx, al
		inc     dx
		rep     outsb
		call	M_WaitVRT_DX
		pop	di
		pop	si
		add     bl, bh
		test    bl, 3Fh
		jnz     short _fi1
                sti
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
		out     dx, al
		inc     dx
		mov     cx, 256
		xor     ah, ah
_Set1Pal:
		out     dx, al
		mov     al, ah
		out     dx, al
		xor     al, al
		out     dx, al
		cmp     ah, 62
		jae     short _Set1Brk
		inc     ah
_Set1Brk:
		loop    _Set1Pal
		xor     bp, bp
		pop	ax
		push	ax
		mov     [Frames], ax ; @Time
		call    ShadesLoop
		call	M_WaitVRT_DX
		mov     dx, 03C8h
		xor     ax, ax
		out     dx, al
		inc     dx
		mov     cx, 256
_Set2Pal:
		mov     al, ah
		out     dx, al
		xor     al, al
		out     dx, al
		out     dx, al
		cmp     ah, 62
		jae     short _Set2Brk
		inc     ah
_Set2Brk:
		loop    _Set2Pal
		xor     bp, bp
		pop	ax
		push	ax
		mov     [Frames], ax ; @Time
		call    ShadesLoop
		call	M_WaitVRT_DX
		mov     dx, 03C8h
		xor     ax, ax
		out     dx, al
		inc     dx
		mov     cx, 256
_Set3Pal:
		out     dx, al
		out     dx, al
		mov     al, ah
		out     dx, al
		cmp     ah, 62
		jae     short _Set3Brk
		inc     ah
_Set3Brk:
		xor     al, al
		loop    _Set3Pal
		xor     bp, bp
		pop	ax
		mov     [Frames], ax ; @Time
		call    ShadesLoop
                retn

PutBop:		; sub_3116
                add     di, dx
                xor     si, si
                mov     cx, BopPts/4
@PutLoop:
                mov     ax, [si+BopAddTab]
                mov     bx, [si+BopTab]
                add     [es:bx+di], al
                mov     ax, [si+BopAddTab+2]
                mov     bx, [si+BopTab+2]
                add     [es:bx+di], al
                mov     ax, [si+BopAddTab+4]
                mov     bx, [si+BopTab+4]
                add     [es:bx+di], al
                mov     ax, [si+BopAddTab+6]
                mov     bx, [si+BopTab+6]
                add     [es:bx+di], al
                add     si, 8
                loop    @PutLoop
HideBop:	; sub_314E
                xchg    [ds:bp+BopQueue], di
                cmp     di, -1
                jae     short @SkipHide
                xor     si, si
                mov     cx, BopPts/4
@HideLoop:
                mov     ax, [si+BopAddTab]
                mov     bx, [si+BopTab]
                sub     [es:bx+di], al
                mov     ax, [si+BopAddTab+2]
                mov     bx, [si+BopTab+2]
                sub     [es:bx+di], al
                mov     ax, [si+BopAddTab+4]
                mov     bx, [si+BopTab+4]
                sub     [es:bx+di], al
                mov     ax, [si+BopAddTab+6]
                mov     bx, [si+BopTab+6]
                sub     [es:bx+di], al
                add     si, 8
                loop    @HideLoop
@SkipHide:
                add     bp, 2
                cmp     bp, 2*MaxBops
                sbb     bx, bx
                and     bp, bx
                ret

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
                mov     ah, [di+MaskTable]
                mov     di, ax
                mov     al, 08h
                out     dx, ax
                mov     ax, di
                mov     di, 240
                sub     di, [YY]
                shr     cx, 3
                shl     di, 4
                add     cx, di
                shl     di, 2
                add     di, cx
                add     di, 6A00h
                mov     ah, [es:di]
                stosb
@NoPixel:
                fld     dword [II]
                fadd    dword [IIAdd]
                fstp    dword [II]
                dec     bh
                jnz     @CalcPixel
                shl     bp, 1
                retn

PrintChar:      ; sub_3243	; si = Char Number, di = Place.
                cli
                mov     si, ax
                shl     si, 3
                add     si, [CharP]
                mov     dl, 08h
_MC1:
                mov     es, [CharP+2]
                db      26h		     ; ES segment override prefix
                lodsb
                mov     dh, al
                mov     es, [TextSeg]

                mov	cx, 8
_MC2:
		xor     bx, bx
		shl     dh, 1
		adc     bx, CharColor
		mov     al, [bx]
		stosb
                loop	_MC2

                add     di, 256-8       ;320-8
                dec     dl
                jnz     short _MC1
_MCOUT:
                sub     di, 8*256-8     ;8*320-8
                sti
                retn

CalcFractal: 	; sub_32C6
                cli
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
                mov     di, [ScreenY]
                mov     cx, di
                mov     di, [di+Row256T]
                add     di, si
                stosb
                inc     word [ScreenX]
                mov     ax, [x]
                add     ax, [XStep]
                mov     [x], ax
                cmp     si, 288
                jb      @RepeatX
@OutM1:
                mov     dx, 3DAh
                in      al, dx
                mov     dx, 3C0h
                mov     al, 11h+32
                out     dx, al
                mov     al, cl
                shr     al, 1
                and     al, 0Fh
                out     dx, al
                add     word [ScreenY], 2
                mov     ax, [y]
                add     ax, [YStep]
                mov     [y], ax
                cmp     cx, 256
                jb      @RepeatY
@OutM2:
                sti
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
		mov     ax, 0A000h
		mov     es, ax
		and     bh, 7
		mov     ax, [bx+SinT]
		imul    di
		sar     dx, 1
		adc     dx, 0
		mov     bp, dx
		mov     ax, [bx+CosT]
		imul    di
		sar     dx, 1
		adc     dx, 0
		mov     si, dx
		mov     ax, [bx+Sin2T]
		imul    di
		sar     dx, 1
		adc     dx, 0
		mov     [d2y], dx
		imul    cx, dx, 100
		mov     ax, [bx+Cos2T]
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
		xor     di, di
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
		mov     bl, [fs:bx]
		mov     [es:di], bl
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
		jb      short _ir1
		pop	ax ; Method
		mov     ax, cs
		mov     es, ax
		retn

RotatePAL20:	; sub_355A
                cli
                std
                mov     si, Temp_PAL3+3*1Fh-2
                mov     di, Temp_PAL3+3*20h-2
                mov     ax, [di]
                mov     bl, [di-1]
                mov     cx, 47
                rep     movsw
                mov     [di+1], ax
                mov     [di], bl
                cld
                mov     si, di
                mov     cx, 3*20h
                call	M_WaitVRT_DX
                mov     dx, 03C8h
                mov     al, 20h
                out     dx, al
                inc     dx
                rep     outsb
                sti
                retn

  GfxWrite:	; sub_3592
                xor     ax, ax
  @WriteFPush:
                push    di
  @WriteF:
                lodsb
                or      al, al
                jz      short @WriteFLN
                cmp     al, 0FFh
                je      short @WriteFOut
                push    si
                call    PrintChar
                pop     si
                jmp     short @WriteF
  @WriteFLN:
                pop     di
                add     di, 8*256       ; 800h
                jmp     short @WriteFPush
  @WriteFOut:
                pop     di
                mov     dx, cs
                mov     es, dx
                retn

                %rep	199
		mov     [bx+di], dh
		add     dx, ax
		inc     bh
                %endrep
  @UpdateMap8:
                mov     [bx+di], dh
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
		cmp     byte [es:di], 0FFh
		jne     short @@CS3
		add     al, [es:bx]
		adc     ah, ch
		shr     ax, 1
		;M_Calc2Square
		call	M_Calc2Square	
		stosb
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
                db      '4KB-Intro$'
                times   2       db 0

db "ETAN 19/9/2016" ; 14 bytes sign (*)

_10A2h:
	dw      512  ; 1024 shr 1
_10A4h:
	dw 	8192
_10A6h:
	dw 	0

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

Data_Seg       Equ 0
MapA_Seg       Equ 2
MapB_Seg       Equ 4
Sky_Seg        Equ 6
MapMakeTable   Equ 8
LastMake_      Equ 128 ; 80h
AddCXTable     Equ 132 ; 84h
AddDXTable     Equ 140 ; 8Ch
MapCoord_      Equ 148 ; 94h
VRAM_Seg       Equ 156 ; 9Ch
W_Divider      Equ 158 ; 9Eh

section .data   ;align=16

_Data_Seg:      dw      0000h
_MapA_Seg:      dw      0000h
_MapB_Seg:      dw      0000h
_Sky_Seg:       dw      0000h
_MapMakeTable:  times	41 dw 0
		times	10 dw 1
		times	9 dw 3
_LastMake_:     dw      0003h, 0000h
_AddCXTable:    dw       32, -16, -8, 8
_AddDXTable:    dw      -32,  -8, 16, 8
_MapCoord_:     dw      0000h, 0004h, 0202h, 0FEFFh
_VRAM_Seg:      dw      0A000h
_W_Divider:     dw      0DB97h

_end: