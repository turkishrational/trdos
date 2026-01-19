; ****************************************************************************
; dropz.s (for TRDOS 386)
; ----------------------------------------------------------------------------
; DROPZ.PRG ! VGA DEMO program by Erdogan TAN
;
; 30/10/2017
;
; [ Last Modification: 05/12/2023 ] 
;
; Derived from source code of 'DROPZ.COM' ('dropz.asm') by baze/3SC
;				      (17/04/2000) 
; Assembler: NASM 2.15 (2.11)
; ----------------------------------------------------------------------------
;	   nasm  dropz.s -l dropz.txt -o DROPZ.PRG	
; ****************************************************************************

;-------------------------------------------------------------------------------
; dr()pz [refined], 256B intro, 1st place at Demobit 98 demoparty
; coded by -baze-> of 3SC [with help of bEETLE and loveC (many refinements)]
; e-mail: baze@decef.elf.stuba.sk
;-------------------------------------------------------------------------------
; This source code is written to be compiled by cool Netwide Assembler (NASM).
; Visit it's web site at www.web-sites.co.uk/nasm.
;-------------------------------------------------------------------------------
; And now few comments in worldwide used Slovak language :)

;------------------------------------------------------------------------------
; TRDOS 386, TRDOS v2.0
;------------------------------------------------------------------------------

; 14/07/2020
; 31/12/2017
; TRDOS 386 (v2.0) system calls
_ver 	equ 0
_exit 	equ 1
_fork 	equ 2
_read 	equ 3
_write	equ 4
_open	equ 5
_close 	equ 6
_wait 	equ 7
_create	equ 8
_rename	equ 9
_delete	equ 10
_exec	equ 11
_chdir	equ 12
_time 	equ 13
_mkdir 	equ 14
_chmod	equ 15
_rmdir	equ 16
_break	equ 17
_drive	equ 18
_seek	equ 19
_tell 	equ 20
_memory	equ 21
_prompt	equ 22
_path	equ 23
_env	equ 24
_stime	equ 25
_quit	equ 26
_intr	equ 27
_dir	equ 28
_emt 	equ 29
_ldrvt 	equ 30
_video 	equ 31
_audio	equ 32
_timer	equ 33
_sleep	equ 34
_msg    equ 35
_geterr	equ 36
_fpstat	equ 37
_pri	equ 38
_rele	equ 39
_fff	equ 40
_fnf	equ 41
_alloc	equ 42
_dalloc equ 43
_calbac equ 44
_dma	equ 45		

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

;------------------------------------------------------------------------------
; CODE
;------------------------------------------------------------------------------

[BITS 32]
[org 0]

Start:
	; clear bss
	mov	ecx, EOF
	mov	edi, bss_start
	sub	ecx, edi
	shr	ecx, 2
	xor	eax, eax
	rep	stosd

	push	esi		; naplnime niecim zasobnik pre nahodne cisla
	push	esi

	mov	al, 13h
	;int	10h		; inicializacia grafickeho modu 320x200x256
	int	31h

	; DIRECT VGA MEMORY ACCESS
	; bl = 0, bh = 5
	; Direct access/map to VGA memory (0A0000h)

	sys	_video, 0500h
	cmp	eax, 0A0000h
	;jne	ENDPROG
	je	short _x
	jmp	ENDPROG
_x:

; VYPOCET PALETY FARIEB

	mov	cl, 0		; do CL hodnota "256" (CH = ?, minus 1 byte)
PALETTE:
	mov	dx, 3C8h	; do DX port PEL ADDRESS registra
	mov	al, cl
	;out	dx, al		; odoslanie indexu farby
	mov	ah, 1	
	int	34h
	inc	edx		; do DX port PEL DATA registra
	mov	al, 0
	;out	dx, al		; odoslanie zlozky R
	int	34h
	mov	al, cl
	shr	al, 1                                             
	;out	dx, al		; odoslanie zlozky G
	int	34h
	mov	al, cl
	cmp	al, 64
	jb	short OKPAL
	;mov	ax, 4A3Fh	; do AH cislo DOS sluzby na realokaciu pamate
	mov	al, 3Fh
OKPAL:
	;out	dx, al		; odoslanie zlozky B
	;mov	ah, 1
	int	34h
	;loop	PALETTE		; opakujeme pre vsetky farby
	dec	cl
	jnz	short PALETTE

; ALOKACIA PAMATE POTREBNEJ PRE DVE 256 x 256 PIXELOVE MAPY

;	;mov	ah, 4Ah
;	mov	bh, 3*16	; do BX aspon 3*16*256 paragrafov (BL = ?, -1B)
;	int	21h		; alokujeme 3 segmenty (program + 2 mapy)
;	jc	near ENDPROG	; ak je malo pamate, ukoncime program

;	mov	ax, cs
;	add	ah, 16
;	mov	ds, ax		; do DS segmentova adresa prvej mapy
;	add	ah, 16
;	mov	es, ax		; do ES segmentova adresa druhej mapy

	mov	edx, DrawSegment ; *
	mov	ebp, EffectSegment ; *	

; VYPIS BITMAPOVEHO TEXTU

WATER:
	;push	ebp ; ***
	push	edx ; ***
	
	;push	es		; ulozime ES (budeme potrebovat pointer na mapu)
	;mov	ax, 1130h	; sluzba VGA BIOS - informacie o fonte
	mov	eax, 1130h
	;mov	bh, 3		; budeme pouzivat font 8 x 8 pixelov
	;int	10h		; v ES:BP je adresa fontu
	mov	ebx, 1	; VGA Font 8x8
	;xor	ecx, ecx 
	mov	edx, VgaFont8Buffer
	int	31h

	pop	edx ; ***
	;pop	ebp ; ***

	mov	ebx, TEXT	; ideme "virit hladinu" textom
	mov	esi, 64*256+38	; do SI umiestnenie textu v mape
	call	DRAWTEX		; zavolame vypis znakoveho retazca
	mov	esi, 100*256+100
	call	DRAWTEX		; a to celkovo 3 krat
	mov	esi, 136*256+36
	call	DRAWTEX

	;pop	es		; obnovime pointer na pixelovu mapu

; "KVAPNUTIE" NA HLADINU

RANDOM:
	pop	esi		; nahodne cisla x[n] = (5 * x[n-1] + 7) % 65536
	lea	si, [esi+4*esi+7]; takto pocita uvedeny vyraz loveC :)
	mov	byte [edx+esi], 255 ; umiestnime na nahodne miesto mapy kvapku
	push	esi		; ulozime nahodne cislo do buducej iteracie

; VYPOCET SIRENIA VLNENIA V MAPE (nemam miesto na odvodenie vztahov z fyziky)
	
WATLINE:
	mov	al, [edx+edi+1]
	add	al, [edx+edi-1]	; spocitame okolite pixely
	add	al, [edx+edi+256]
	add	al, [edx+edi-256]
	shr	al, 1		; vydelime sucet dvoma
	sub	al, [ebp+edi]	; odcitame predchadzajucu fazu
	jnc	short OK	; pripadne orezeme zaporne vlny ...
	mov	al, 0		; ... ziskame tak vacsiu amplitudu (0 - 255)
OK:
	;stosb			; ulozime pixel
	mov	[ebp+edi], al
	inc	di
	or	di, di
	jnz	short WATLINE	; opakujeme pre vsetky pixely v mape

	;push	ds		; ulozime DS (neskor POPneme do ES)
	push	edx ; *(*)
	;push	es
	;pop	ds		; do DS hodnota ES (vymena pointrov na mapy)
	;mov	edx, ebp ; **
	;push	word 0A000h
	;pop	es		; do ES segmentova adresa zaciatku videoram

; CAKANIE NA NOVY SNIMOK

FRAME:
	mov	dx, 3DAh	; do DX port INPUT STATUS registra (DH = 3, -1B)
	mov	ah, 0
	;in	al, dx
	int	34h
	and	al, 8
	jz	short FRAME	; pockame na novy snimok

; VYKRESLENIE MAPY NA OBRAZOVKU
	mov	edx, ebp ; **

	mov	edi, 32		; budeme vykreslovat od 32. stlpca (centrovanie)
	xor	esi, esi	; do SI zaciatok vykreslovanej mapy
	mov	ebp, 200	; kreslime 200 riadkov
DRAW:
	mov	cl, 128		; prenasame 256 bajtov, cize 128 slov (word)
	;rep	movsw		; prenos slov z mapy do videoram (rychlejsie)
draw_loop:
	mov	ax, [edx+esi]
	mov	[edi+0A0000h], ax
	add	si, 2
	add	di, 2
	loop	draw_loop	
	movzx	ebx, byte [edx+esi] ; vyberieme hodnotu z mapy kvoli "osciloskopu"
	or	bl, bl		; je vyska hladiny v mape nulova? ...
	jz	short NOOSCIL	; ... ak ano, nevykreslujeme pixel (estetika)
	mov	byte [ebx+edi+0A0000h-128], 80	; inak vykreslime symetricky dva pixely
	neg	ebx		; s danou intenzitou
	mov	byte [ebx+edi+0A0000h-128], 80
NOOSCIL:
	add	di, 64		; posunieme sa na dalsi riadok obrazovky
	dec	ebp
	jnz	short DRAW	; a opakujeme pre vsetky riadky

	;pop	es		; dokoncenie vymeny ES <-> DS
	pop	ebp ; *(*)

	;push	edx ; ****
	;;in	al, 60h		; test klavesy ESC
	;mov	dx, 60h
	;;mov	ah, 0
	;int	34h
	;pop	edx ; ****
	;dec	al
	;jnz	WATER		; ak nebola stlacena, skok na dalsi snimok

	mov	ah, 1
	int	32h
	;jz	WATER
	jnz	short ENDPROG
	jmp	WATER

ENDPROG:
	mov	ax, 03h
	;mov	al, 3
	;int	10h		; nastavenie textoveho rezimu
	int	31h

	pop	esi
	pop	esi		; oprava zasobnika po nahodnych cislach

	sys	_exit		; navrat do operacneho systemu

; RUTINA NA VYPIS TEXTOVEHO RETAZCA

DRAWTEX:
	movzx	edi, byte [ebx]	; do DI ASCII hodnota znaku
	inc	ebx		; zvysime ukazovatel na text
	shl	edi, 3		; DI = DI * 8, mame offset znaku vo fonte
	jz	short RETURN	; ak bol znak nulovy, koniec vypisu
	mov	cl, 8		; budeme vypisovat 8 riadkov
CHARLIN:
	mov	al, [VgaFont8Buffer+edi] ; vyberieme bajt z predlohy znaku
	inc	edi		; zvysime ukazovatel na dalsi bajt predlohy
	mov	ch, 8		; znak ma 8 stlpcov
ROTATE:
	shl	al, 1		; do CARRY pixel z predlohy
	jnc	short NOPIXEL	; ak pixel nebol nastaveny, nic nekreslime
	add	dword [edx+esi], 02040304h ; inak pripocitame "stvorpixel"
NOPIXEL:
	add	si, 4		; posunieme sa na dalsi pixel v mape
	dec	ch
	jnz	short ROTATE	; opakujeme 8-krat
	add	si, 1024-32	; posunieme sa na dalsi riadok
	loop	CHARLIN		; opakujeme 8-krat (LOOP trik, CH = 0, setrime)
	add	si, 32-8192	; posunieme sa na dalsi znak
	jmp	short DRAWTEX	; skok na vypis dalsieho znaku
RETURN:
	retn

;------------------------------------------------------------------------------
; DATA
;------------------------------------------------------------------------------

TEXT:
	db	"dr()pz", 0	; hadajte co :)
	db	"by",0
	db	"-baze>", 0

;-------------------------------------------------------------------------------
; Initially, this intro was hacked up during sleepless night before party.
; Few days after Demobit 98 we made this refined version. Although it is not
; as carefully optimized as it can be [loveC was able to make it in less than
; 230 bytes], I decided to spread this [almost original] code. It is easier
; to understand and it actually shows our poor mental state at given time :)
;-------------------------------------------------------------------------------

;db	"Erdogan Tan [01/11/2017]"
db	"Erdogan Tan [05/12/2023]", 0

;------------------------------------------------------------------------------
; UNINITIALIZED DATA
;------------------------------------------------------------------------------

bss_start:

ABSOLUTE bss_start

alignb 2 

VgaFont8Buffer:
	resb	8*256

;alignb	65536
alignb 4096

DrawSegment:
	resb	65536

EffectSegment:
	resb  	65536

EOF: