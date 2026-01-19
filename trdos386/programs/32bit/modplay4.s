; ****************************************************************************
; modplay4.s (for TRDOS 386)
; ----------------------------------------------------------------------------
; MODPLAY4.PRG ! SOUND BLASTER 16 MOD PLAYER & VGA DEMO program by Erdogan TAN
;
; 24/06/2017
;
; [ Last Modification: 08/10/2017 ]
;
; Derived from source code of 'PLAY.EXE' (TINYPLAY) by Carlos Hasan (1993)
;          PLAY.EXE: PLAY.ASM, MODLOAD.ASM, MODPLAY.ASM, SB.ASM
;
; Modified by using the source code of 'tinyply3.s' ('TINYPLY3.PRG') 
; by Erdogan Tan (07/10/2017)
;
; Modified from 'playwav3.s' (13/06/2017)
;
; Modified from 'PLAYMOD.PRG' ('playmod.s') source code by Erdogan Tan
;			                     (23/06/2017)
;
; Derived from source code of 'TINYPLAY.COM' ('TINYPLAY.ASM') by Erdogan Tan
;				      (04/03/2017) 
; Assembler: NASM 2.11
; ----------------------------------------------------------------------------
;	   nasm  modplay.s -l modplay.txt -o MODPLAY.PRG	
; ****************************************************************************
; PLAYMOD.ASM by Erdogan Tan (for MSDOS) (15/02/2017)
; TMODYPLAY.ASM by Erdogan Tan (for MSDOS) (01/10/2017)

; 01/03/2017
; 16/10/2016
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
_video 	equ 31
_audio	equ 32
_timer	equ 33
_sleep	equ 34
_msg    equ 35
_geterr	equ 36
_fpsave	equ 37
_pri	equ 38
_rele	equ 39
_fff	equ 40
_fnf	equ 41
_alloc	equ 42
_dalloc equ 43
_calbac equ 44		

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

; 19/06/2017
BUFFERSIZE equ 32768

; ----------------------------------------------------------------------------
; Tiny MOD Player v0.1b by Carlos Hasan.
;	July 14th, 1993.

;=============================================================================
;  
;=============================================================================

[BITS 32]
[org 0]

Start:
	; clear bss
	mov	ecx, EOF
	mov	edi, bss_start
	sub	ecx, edi
	shr	ecx, 1
	xor	eax, eax
	rep	stosw

	; Detect (& Enable) Sound Blaster 16 Audio Device
	call    DetectSB16
	jnc     short GetFileName

_dev_not_ready:
; couldn't find the audio device!
	sys	_msg, MsgNotFound, 255, 0Fh
        jmp     Exit

GetFileName:
	;cmp	ah, 1 ; SB16 Sound card
	;jne	_dev_not_ready	
	  
	mov	esi, esp
	lodsd
	cmp	eax, 2 ; two arguments 
		; (program file name & mod file name)
	jb	pmsg_2017 ; nothing to do

	lodsd ; program file name address 
	lodsd ; mod file name address (file to be read)
	mov	esi, eax
	mov	edi, mod_file_name
ScanName:       
	lodsb
	test	al, al
	je	pmsg_2017
	cmp	al, 20h
	je	short ScanName	; scan start of name.
	stosb
	mov	ah, 0FFh
a_0:	
	inc	ah
a_1:
	lodsb
	stosb
	cmp	al, '.'
	je	short a_0	
	and	al, al
	jnz	short a_1

	or	ah, ah		 ; if period NOT found,
	jnz	short PrintPMesg ; then add a .MOD extension.
SetExt:
	dec	edi
	mov	dword [edi], '.MOD'
	mov	byte [edi+4], 0
PrintPMesg:      
	; Prints the Credits Text.
	sys	_msg, Credits, 255, 0Fh
_1:
	; 19/06/2017
	; Allocate Audio Buffer (for user)
	sys	_audio, 0200h, BUFFERSIZE, Audio_Buffer
	jc	error_exit
_2:
	; Initialize Audio Device (bl = 1 -> Interrrupt method)
	;sys	_audio, 0301h, 0, sb16_int_handler 
	;jc	error_exit
	
	; Initialize Audio Device (bl = 0 -> SRB method)
	sys	_audio, 0300h, 1, srb 
	jc	error_exit

LoadMod:  
	mov	edi, mod_file_name
	call    LoadModule		; Load the MODule...
	; 08/10/2017
	jnc	short _3		; any error loading?

	; yes, print error and Exit.

	sys	_msg, ErrorMesg, 255, 0Fh
	jmp     Exit
_3:
	; 24/06/2017
	sys	_audio, 0E00h ; get audio controller info
	jc	error_exit

	; EAX = IRQ Number in AL
	;	Audio Device Number in AH 
	; EBX = DEV/VENDOR ID
	;       (DDDDDDDDDDDDDDDDVVVVVVVVVVVVVVVV)
	; ECX = BUS/DEV/FN 
	;       (00000000BBBBBBBBDDDDDFFF00000000)
	; EDX = Base IO Addr (DX) for SB16 & VT8233
	; EDX = NABMBAR/NAMBAR (for AC97)
	;      (Low word, DX = NAMBAR address)

	mov	[SbAddr], dx
	mov	[SbIrq], al

	; Print Sucessful message.
	;mov	dx, [SbAddr]
	;mov	al, [SbIrq]
	shr     dl, 4
	add     dl, '0'
	mov     [PortText], dl
	add     al, '0'
	mov     [IrqText], al

	sys	_msg, MsgFound, 255, 0Fh

PlayNow: 
	call    StartPlaying

        ; load 32768 bytes into audio buffer
	mov	edi, Audio_Buffer
	mov	ebx, BUFFERSIZE
	call	GetSamples
	jc	error_exit

	;mov	ecx, 128	; Make a lookup table
	mov	cl, 128
	xor     ebx, ebx	; for fastest pixel
	mov     edx, 320*(100-64)	; addressing.
MakeOfs:        
	mov     [RowOfs+ebx], dx
	mov     [RowOfs+ebx+2], dx
	add     dx, 320
	add     ebx, 4
	loop    MakeOfs

	; 23/06/2017
	; Map DMA buffer to user's memory space
	sys	_audio, 0D00h, 65536, DMA_Buffer
	;jc	error_exit

	; 24/06/2017
	; Set Master Volume Level (BL=0 or 80h)
	; 	 	for next playing (BL>=80h)
	sys	_audio, 0B80h, 1D1Dh

	;mov	word [MixSpeed], 22050	; Mixing at 22.050 kHz
	
	; Start	to play
	mov	al, [bps]
	shr	al, 4 ; 8 -> 0, 16 -> 1
	shl	al, 1 ; 16 -> 2, 8 -> 0
	mov	bl, [stmo]
	dec	bl
	or	bl, al
	mov	cx, [MixSpeed] ; [Sample_Rate] ; Hz 
	mov	bh, 4 ; start to play	
	sys	_audio
    
	;; SETUP SIGNAL RESPONSE BYTE
	;; 06/03/2017
	;mov	bl, [ac97_int_ln_reg] ; IRQ number
	;mov	bh, 1 ; Link IRQ to user for Signal Response Byte
	;mov	edx, srb  ; Signal Response/Return Byte address  
	;mov	ecx, 0FFh ; Signal Response/Return Byte value  
	;sys	_calbac
	;jc	short error_exit

	; DIRECT VGA MEMORY ACCESS
	; bl = 0, bh = 5
	; Direct access/map to VGA memory (0A0000h)

	sys	_video, 0500h
	cmp	eax, 0A0000h
	je	short _a3
error_exit:
	sys	_msg, trdos386_err_msg, 255, 0Eh
	jmp	short Exit

; Note: Normally IRQ 0 calls the ModPlay Polling at 18.2Hz thru
;       the software interrupt 1Ch. If the IRQ 0 is disabled, then
;       the INT 1Ch MUST BE CALLED at least MixSpeed/1024 times per
;       second, or the module will sound "looped".
;       Because we need better sync with the ModPlayer to draw the scope,
;       the polling is called from my routine, and then the irq 0 must be
;       disabled. The [DmaBuffer] points to the current buffer of 8-bit
;       samples played by the Sound Blaster. Note that some samples are
;       discarded in the next code, just for fun!

_a3:
	mov     ax, 0013h	; Set Mode 320x200x256
	int     31h

	; 24/06/2017
	call	PlayMod ; 13/02/2017 (ModPlay)

_s_exit:
	call	StopPlaying	; STOP!

	mov     ax, 0003h	; Set Text Mode 80x25x16
	int     31h
Exit:           
	;call    FreeModule	; Free MODule core.
	
	sys 	_exit	; Bye !
here:
	jmp	short here

pmsg_2017:
	sys	_msg, msg_2017, 255, 0Fh
	jmp	short Exit

DetectSB16:
	; 24/06/2017
	; Detect (BH=1) SB16 (BL=1) Sound Card
        sys	_audio, 0101h
	retn

;sb16_int_handler:
;	; 24/06/2017
;	mov	byte [srb], 1 ; interrupt (or signal response byte)
;
;	sys	_rele ; return from callback service 
;	; we must not come here !
;	sys	_exit

;=============================================================================
;      
;=============================================================================

PlayMod:
	; 23/06/2017   
	; 21/06/2017
	; 19/06/2017

	; 05/03/2017 (TRDOS 386)
	; 14/02/2017
	; 13/02/2017
	; 08/12/2016
	; 28/11/2016

     	jmp	short modp_gs ; 23/06/2017
p_loop:
	cmp	byte [srb], 0
	jna	short q_loop
	mov	byte [srb], 0
modp_gs:
	mov	edi, Audio_Buffer
	mov	ebx, BUFFERSIZE ; 32768 bytes ; 14/03/2017
	call	GetSamples
	jc	error_exit
q_loop:
	mov     ah, 1		; any key pressed?
	int     32h		; no, Loop.
	jz	short r_loop

	mov     ah, 0		; flush key buffer...
	int     32h
q_return:
	retn
r_loop:
	; Get Current DMA buffer Pointer 
	; 23/06/2017
	; bh = 15, get current pointer (DMA buffer offset)
	; bl = 0, for PCM OUT
	; ecx = 0
	;
	sys	_audio, 0F00h, 0
ScopeLoop:
	mov	edi, 0A0000h	; VGA display memory address
	; 23/06/2017
	mov     esi, DMA_Buffer
	add     esi, eax	; add offset value
	; 24/06/2017
	mov	ecx, DMA_Buffer + (65536 - 320) 
	cmp	esi, ecx 
	jna	short _4
	mov	esi, ecx
_4:
	xor     ecx, ecx	; to be drawed ...
	xor     edx, edx
DrawLoop:       
	mov     ebx, edx	; (save Index)
	mov     di, [Scope+ebx]	; get old SCOPE pixel address
	mov     byte [edi], 0	; erase it!
	;lodsb			; get a sample (8-bit)
	;mov	bl, al
	mov	bl, [esi]
	inc	esi
	xor     bh, bh
	shl     bx, 1
	mov     di, [RowOfs+ebx]
	add     di, cx
	mov     bx, dx		; (restore Index)
	mov     [Scope+ebx], di	; save new address...
	mov     byte [edi], 10	; and DRAW.
	add     dx, 2		; the next pixel...
	inc     ecx
	cmp     cx, 320		; 320 pixels drawed?
	jb      short DrawLoop
	jmp	p_loop

;=============================================================================
;               MODLOAD.ASM
;=============================================================================

; Amiga Module Loader v0.1b by Carlos Hasan.
;	July 10th, 1993.

; STRUCTURES

struc ModSample
.msName:	resb 22
.msLength:	resw 1
.msFinetune:	resb 1
.msVolume:	resb 1
.msRepeat:	resw 1
.msRepLen:	resw 1
.size:		; 30 bytes
endstruc

struc ModHeader
.mhName:	resb 20
.mhSamples:	resb ModSample.size*31
.mhOrderLen:	resb 1
.mhReStart:	resb 1
.mhOrder:	resb 128
.mhSign:	resw 2
.size:		; 1084 bytes
endstruc

struc ModInfoRec
.OrderLen:	resb 1
.ReStart:	resb 1
.Order:		resb 128
.Patterns:	resd 1
.SampOfs:	resw 31
.SampSeg:	resw 31
.SampLen:	resw 31
.SampRep:	resw 31
.SampRepLen:	resw 31
.SampVol:	resw 31
.size:		; 506 bytes	
endstruc

; CODE

; modplay4.s
; 07/10/2017
; tinyply3.s
; 06/10/2017
; 04/10/2017
; /* MOD FileFormat */

ID_MK	equ 2E4B2E4Dh ; "M.K."
ID_FLT4 equ 34544C46h ; "FLT4"
ID_8CHN equ 4E484338h ; "8CHN"
ID_FLT8 equ 34544C46h ; "FLT8"

; CODE

LoadModule:
	; edi = file name address

	pushad

	call    ClearModInfo
OpenFile:       
	; ebx = ASCIIZ file name address
	; ecx = open mode (0 = open for read)	
	sys	_open, edi, 0 ; open for reading
	jc	Failed
	mov     [FileHandle], eax
ReadHeader:
	; ebx = File handle
	; ecx = Buffer address
	; edx = Byte count
	sys	_read, [FileHandle], Header, ModHeader.size
	jc      CloseFile
CheckMK:  
	; 04/10/2017
	mov	eax, [Header+ModHeader.mhSign]
      
	cmp	eax, ID_MK   ; cmp eax, '.K.M'
	;je	short Is4chnMod
	je	short IsModFile
CheckFLT4:
	cmp	eax, ID_FLT4 ; cmp eax, '4TLF'
	;je	short Is4chnMod
	je	short IsModFile
Check8CHN:
	cmp	eax, ID_8CHN ; cmp eax,	'NHC8'
	je	short Is8chnMod
CheckFLT8:
	cmp	eax, ID_FLT8 ; cmp eax, '8TLF'
	; 06/10/2017
	je	short Is8chnMod
	stc
	jmp	CloseFile
Is8chnMod:
	mov	byte [numtracks], 8	; 8-CHANNEL-MOD
	mov	byte [pattern_shift], 11 ; Pattern Size = 2048 bytes
	jmp	short IsModFile
;Is4chnMod:
;	mov	byte [numtracks], 4	; 4-CHANNEL-MOD
;	mov	byte [pattern_shift], 11 ; Pattern Size = 1024 bytes

IsModFile:
	mov     al, [Header+ModHeader.mhOrderLen]
	mov     [ModInfo.OrderLen], al

	mov     al, [Header+ModHeader.mhReStart]
	cmp     al, [Header+ModHeader.mhOrderLen]
	jb      short SetReStart
	mov     al, 7Fh
SetReStart:
	mov     [ModInfo.ReStart], al

	;mov	ecx, 128
	mov	cx, 128
	xor     edx, edx
	xor     ebx, ebx
CopyOrder:
	mov     dh, [Header+ModHeader.mhOrder+ebx]
	mov     [ModInfo.Order+ebx], dh
	cmp     dh, dl
	jb      short NextOrder
	mov     dl, dh ; Max. pattern number ; 04/10/2017
NextOrder:
	inc     ebx
	loop    CopyOrder
AllocPatterns:  
	and	edx, 0FFh
	; 04/10/2017
	;inx	dx  ; 12/03/2017
	inc	dl
	; dl = number of patterns (04/07/2017)
	mov	cl, [pattern_shift] ; 10 for 4 channels, 11 for 8 channels
	shl	edx, cl ; 10 ; *1024 ; (byte count of patterns *64*4*4)
		     	     ; *2048 ; (byte count of patterns *64*8*4)
	;
	mov	ebp, edx ; offset of samples (04/07/2017)
	;mov	ecx, 10000h ; next 64K (4096*16)
	mov	ecx, file_buffer ; 12/03/2017
	;
	mov	[ModInfo.Patterns], ecx
	;
	add	ebp, ecx ; next offset for samples
ReadPatterns:  
	;mov	ebx, [FileHandle] 
	; ebx = File handle
	; ecx = Buffer address
	; edx = Byte count
	sys	_read, [FileHandle]
	jc      CloseFile

	; patterns have been loaded here... (04/07/2017)

	mov	esi, Header+ModHeader.mhSamples
	xor     edi, edi
CopySamples:
	mov     ax, [esi+ModSample.msLength]
	xchg    al, ah
	shl     ax, 1
	mov     [ModInfo.SampLen+edi], ax
	mov     al, [esi+ModSample.msVolume]
	xor     ah, ah
	mov     [ModInfo.SampVol+edi], ax
	mov     ax, [esi+ModSample.msRepeat]
	xchg    al, ah
	shl     ax, 1
	mov     [ModInfo.SampRep+edi], ax
	mov     ax, [esi+ModSample.msRepLen]
	xchg    al, ah
	shl     ax, 1
	mov     [ModInfo.SampRepLen+edi], ax
	add     esi, ModSample.size
	add     di, 2
	cmp     di, 2*31
	jb      short CopySamples

	xor     esi, esi
AllocSamples:
	movzx	edx, word [ModInfo.SampLen+esi]
	; 07/10/2017
	;shr	dx, 4 ; ***
	and	edx, edx
	jz      short NextSample
	;inc	dx  ; number of paragraphs ; ***
	;shl	dx, 4 ; ***
	mov	eax, ebp
	mov	[ModInfo.SampOfs+esi], ax
	shr	eax, 16
	mov	[ModInfo.SampSeg+esi], ax
	mov	ecx, ebp
	add	ebp, edx ; next offset for sample 
ReadSample:
	;mov	ebx, [FileHandle]
	;movzx  edx, [ModInfo.SampLen+esi]
	;mov    ecx, [ModInfo.SampOfs+esi]

	; ebx = File handle
	; ecx = Buffer address
	; edx = Byte count
	sys	_read, [FileHandle]
	jc      short CloseFile

NextSample:
	add     si, 2
	cmp     si, 2*31
	jb      short AllocSamples
CloseFile:      
	pushf
	sys	_close, [FileHandle]
	popf
Failed:       
	popad
	retn

FreeModule:
	; Erdogan Tan (13/02/2017)
	; nothing to do here for memory de-allocation
ClearModInfo:
	push	edi
	mov	edi, ModInfo
	mov     ecx, ModInfoRec.size
	;cld
	xor     al, al
	rep     stosb
	pop	edi
	retn

;=============================================================================
;               MODPLAY.ASM
;=============================================================================

; Amiga Module Loader v0.3b by Carlos Hasan.
;	July 23th, 1993.

; EQUATES

;NumTracks	equ 4 ; 07/10/2017 ([numtracks])
DefTempo        equ 6
DefBpm          equ 125
MidCRate        equ 8448
MixBufSize      equ 4096

; STRUCTURES

struc TrackInfo  ; 01/10/2017 (TMODPLAY.ASM) modif. by Erdogan Tan
.Samples:	resd 1
;.Position:	resw 1
.Position:	resd 1 ; 01/10/2017 - TRDOS 386 modification ! 
.Len:		resw 1
.Repeat:	resw 1
.RepLen:	resw 1
.Volume: 	resb 1 ; Volume
.VolDiff:	resb 1 ; 01/10/2017 ; Volume difference (Tremolo)
;.Error:	resb 1
;.Reserved:	resb 1 ; 01/10/2017
.Period:	resw 1 ; Period
.Pitch:		resw 1 
.Effect:	resw 1 ; Effect
.PortTo:	resw 1 ; Toneporta wanted period
.PortParm:	resb 1 ; Toneporta speed
.VibPos:	resb 1 ; Vibrato wave position 
.VibParm:	resb 1 ; Vibrato depth/rate
.TremPos:	resb 1 ; 01/10/2017 ; Tremolo wave position
.TremParm:	resb 1 ; 01/10/2017 ; Tremolo depth/rate
;.OldSampOfs:	resb 1 ; ******* ; 01/10/2017
.Error:		resb 1 ; 01/10/2017
.Arp:		resw 3
.ArpIndex:	resw 1
.size:		; 38 bytes ; 01/10/2017 -  TRDOS 386
endstruc

; CODE

;--------------------------------------------------------------------------
; updatechannel - update the track using the current effect
;--------------------------------------------------------------------------
; 
;--------------------------------------------------------------------------
; 	Track:  Process the next 	 in one track.
;  In:
;    ds:di -  Track info Address.
;--------------------------------------------------------------------------

; edi = Track info address

updatechannel:
BeatTrack:	; updatechannel ; 01/10/2017 (TMODPLAY.ASM)

	mov     dx, [edi+TrackInfo.Effect]

	;test   dx, dx
	;je     short None
	;cmp    dh, 00h
	;je     short Arpeggio
	;cmp    dh, 01h
	;je     short PortUp
	;cmp    dh, 02h
	;je     short PortDown
	;cmp    dh, 03h
	;je     TonePort
	;cmp    dh, 04h
	;je     Vibrato
	;cmp    dh, 05h
	;je     PortSlide
	;cmp    dh, 06h
	;je     VibSlide
	;cmp    dh, 0Ah
	;je     VolSlide
	;retn

	movzx	eax, dh
	and	al, 0Fh
	jmp	dword [4*eax+efxtable2] ; TRDOS 386 ! (32 bits)
efxnull:
None:           
	retn
efxarpeggio2:
	; 01/10/2017
	test    dl, dl
	jz      short efxnull
Arpeggio:
	movzx   ebx, word [edi+TrackInfo.ArpIndex]
	mov     ax, [edi+TrackInfo.Arp+ebx]
	mov     [edi+TrackInfo.Pitch], ax
	add     bx, 2
	cmp     bx, 6
	jb      short SetArpIndex
	xor     ebx, ebx
SetArpIndex:
	mov     [edi+TrackInfo.ArpIndex], bx
	retn
efxportaup:
PortUp:
	xor     dh, dh
	;mov	bx, [edi+TrackInfo.Period]
	movzx	ebx, word [edi+TrackInfo.Period] ; 02/10/2017
	sub     bx, dx
	;cmp	bx, 113
	cmp	bx, 28 ; 01/10/2017 
	jge     short NotSmall
	;mov	bx, 113
	mov	bx, 28 ; 01/10/2017
NotSmall:
	mov     [edi+TrackInfo.Period], bx
	add     bx, bx
	;mov	ax, [PitchTable+bx]
	mov	ax, [PitchTable+ebx]  ; 02/10/2017
	mov     [edi+TrackInfo.Pitch], ax
	retn
efxportadown:
PortDown:
	xor     dh, dh
	;mov	bx, [edi+TrackInfo.Period]
	movzx	ebx, word [edi+TrackInfo.Period] ; 02/10/2017
	add     bx, dx
	cmp	bx, 3424 ; 01/10/2017 
	;cmp	bx, 856
	jle     short NotBig
	;mov	bx, 856
	mov	bx, 3424 ; 01/10/2017
NotBig:         
	mov     [edi+TrackInfo.Period], bx
	add     bx, bx
	;mov	ax, [PitchTable+bx]
	mov	ax, [PitchTable+ebx]  ; 02/10/2017
	mov     [edi+TrackInfo.Pitch], ax
	retn
efxtoneporta2:
TonePort:
	xor     dh, dh
	mov     ax, [edi+TrackInfo.PortTo]
	;mov	bx, [edi+TrackInfo.Period]
	movzx	ebx, word [edi+TrackInfo.Period] ; 02/10/2017
	cmp     bx, ax
	je      short NoPort
	jg      short PortToUp
PortToDown:     
	add     bx, dx
	cmp     bx, ax
	jle     short SetPort
FixPort:        
	mov     bx, ax
	jmp     short SetPort
PortToUp:
	sub     bx, dx
	cmp     bx, ax
	jl      short FixPort
SetPort:        
	mov     [edi+TrackInfo.Period], bx
	add     bx, bx
	;mov	ax, [PitchTable+bx]
	mov	ax, [PitchTable+ebx]  ; 02/10/2017
	mov     [edi+TrackInfo.Pitch], ax
NoPort:         
	retn
efxvibrato2:
	; 01/10/2017
Vibrato:
	mov     dh, dl
	;and	dl, 0Fh
	;shr	dh, 4
	;shl	dh, 2
	and     dx, 0F00Fh
	shr     dh, 2
	;add	[edi+TrackInfo.VibPos], dh
	;mov	dh, [edi+TrackInfo.VibPos]
	;mov	bl, dh
	mov	bl, [edi+TrackInfo.VibPos]  ; 01/10/2017
	add	[edi+TrackInfo.VibPos], dh
	mov	dh, bl ; 01/10/2017
	shr     bl, 2
	;and	bx, 1Fh
	;mov	al, [SinTable+bx]
	and	ebx, 1Fh
	mov	al, [SinTable+ebx]
	mul     dl
	;rol	ax, 1
	;xchg	al, ah
	;and	ah, 1
	shr	ax, 7
	test    dh, dh
	jns     short VibUp
	neg     ax
VibUp:          
	add     ax, [edi+TrackInfo.Period]
	mov	bx, ax
	;movzx	ebx, ax
	cmp     bx, 113
	;cmp	bx, 113
	cmp	bx, 28  ; 01/10/2017
	jge     short NoLoVib
	;mov	bx, 113
	mov	bx, 28	; 01/10/2017
	jmp	short NoHiVib ; 01/10/2017	
NoLoVib:        
	cmp	bx, 3424 ; 01/10/2017 
	;cmp	bx, 856
	jle     short NoHiVib
	;mov	bx, 856
	mov	bx, 3424 ; 01/10/2017
NoHiVib:        
	add     bx, bx
	;mov	ax, [PitchTable+bx]
	mov	ax, [PitchTable+ebx] ; 01/10/2017
	mov     [edi+TrackInfo.Pitch], ax
	retn
efxtoneslide:
PortSlide:
	call    VolSlide
	mov     dl, [edi+TrackInfo.PortParm]  ; .tonespeed
	jmp     TonePort  ; efxtoneporta2
efxvibslide:
VibSlide:
	call    VolSlide
	mov     dl, [edi+TrackInfo.VibParm]
	jmp     short Vibrato  ; efxvibrato2
efxvolslide:
VolSlide:
	mov     dh, dl
	and     dl, 0Fh
	shr     dh, 4
	mov     al, [edi+TrackInfo.Volume]
	sub     al, dl
	jge     short NoLoVol
	xor     al, al
NoLoVol:        
	add     al, dh
	cmp     al, 64
	jbe     short NoHiVol
	mov     al, 64
NoHiVol:        
	mov     [edi+TrackInfo.Volume], al
	retn

efxtremolo2:
	; 01/10/2017 (TMODPLAY.ASM)
Tremolo:
	mov     dh, dl
	and     dx, 0F00Fh
	shr     dh, 2
	mov	bl, [edi+TrackInfo.TremPos]
	add	[edi+TrackInfo.TremPos], dh
	mov	dh, bl
	shr     bl, 2
	; 01/10/2017 - TRDOS 386
	;and	bx, 1Fh
	and	ebx, 1Fh 
	;mov	al, [SinTable+bx]
	mov     al, [SinTable+ebx]
	mul     dl
	shr	ax, 6
	test    dh, dh
	jge	short Tremolo_1 ; efxtremolof2
	neg     ax
efxtremolof2:
Tremolo_1:      
	mov	ah, [edi+TrackInfo.Volume]    
	add     al, ah
	jge     short Tremolo_2 ; efxtremolof3
	xor     al, al
efxtremolof3:
Tremolo_2:       
	cmp     al, 64 ; 40h
	jle     short Tremolo_3 ; efxtremolof4
	mov     al, 64 ; 40h
efxtremolof4:
Tremolo_3:       
	sub	al, ah  ; ****** 
	mov     [edi+TrackInfo.VolDiff], al
	retn	

;--------------------------------------------------------------------------
; readchannel - read the next note event from the pattern sheet
;--------------------------------------------------------------------------
;
;--------------------------------------------------------------------------
; GetTrack:   Get the next Note from a pattern.
;  In:
;    ds:di -  Track info Address.
;    es:si -  Pattern Note Address.
; Out:
;    es:si -  The Next Pattern Note address.
;--------------------------------------------------------------------------

; esi = Pattern note address
; edi = Track info address

readchannel:
GetTrack: 	; readchannel ; 01/10/2017 (TMODPLAY.ASM)
	lodsw
	xchg    al, ah
	mov	bl, ah
	and     ah, 0Fh
	mov     cx, ax
	lodsw
	xchg    al, ah
	mov     bh, ah
	and     ah, 0Fh
	mov     dx, ax
	mov     [edi+TrackInfo.Effect], dx
	; 01/10/2017 - TRDOS 386
	;and	bl, 0F0h
	and	ebx, 0FFF0h
	shr     bh, 4
	or      bl, bh
	jz      short SetPeriod
SetSample:
	xor	bh, bh
	;and	ebx, 0FFh
	dec     bl
	add     ebx, ebx
	mov     ax, [ModInfo.SampVol+ebx]
	mov     [edi+TrackInfo.Volume], al
	mov     ax, [ModInfo.SampOfs+ebx]
	mov     [edi+TrackInfo.Samples], ax
	mov     ax, [ModInfo.SampSeg+ebx]
	mov     [edi+TrackInfo.Samples+2], ax
	mov     ax, [ModInfo.SampLen+ebx]
	mov     [edi+TrackInfo.Len], ax
	mov     ax, [ModInfo.SampRep+ebx]
	mov     [edi+TrackInfo.Repeat], ax
	mov     ax, [ModInfo.SampRepLen+ebx]
	mov     [edi+TrackInfo.RepLen], ax
SetPeriod:      
	test    cx, cx
	jz      short SetEffect

	mov     [edi+TrackInfo.PortTo], cx ; *
	
	cmp     dh, 03h
	;je	short SetEffect
	je	short efxtoneporta ; 01/10/2017

	mov     [edi+TrackInfo.Period], cx
	;movzx	ebx, cx
	mov     bx, cx
	add     bx, bx
	;mov	ax, [PitchTable+bx]
	mov	ax, [PitchTable+ebx] ; 01/10/2017
	mov     [edi+TrackInfo.Pitch], ax
	mov     dword [edi+TrackInfo.Position], 0
SetEffect:
	;test	dx, dx
	;je	short InitNone
	;cmp	dh, 00h
	;je	InitArpeggio
	;cmp	dh, 03h
	;je	short InitTonePort
	;cmp	dh, 04h
	;je	short InitVibrato
	;cmp	dh, 09h
	;je	short SampleOfs
	;cmp	dh, 0Bh
	;je	short PosJump
	;cmp	dh, 0Ch
	;je	short SetVolume
	;cmp	dh, 0Dh
	;je	short Break
	;cmp	dh, 0Fh
	;je	SetSpeed
	;retn

	; 01/10/2017 (TMODPLAY.ASM)
	
	; dx = [di+TrackInfo.Effect]
	
	movzx	eax, dh
	and	al, 0Fh
	jmp	dword [4*eax+efxtable] ; TRDOS 386 ! (32 bits)
;efxnull:
;InitNone:
;	retn
efxtoneporta:
	; 01/10/2017
	; cx = period
	;mov	[edi+TrackInfo.PortTo], cx ; *
InitTonePort:
	test    dl, dl
	jnz     short SetPortParm
	mov     dl, [edi+TrackInfo.PortParm] ; .tonespeed
SetPortParm:    
	mov     [edi+TrackInfo.PortParm], dl
	mov     [edi+TrackInfo.Effect], dx
	retn
efxvibrato:
InitVibrato:
	mov     al, [edi+TrackInfo.VibParm]
	mov     ah, al
	;and	al, 0Fh
	;and	ah, 0F0h
	and	ax, 0F00Fh
	test    dl, 0Fh
	jne     short OkDepth
	or      dl, al
OkDepth:        
	test    dl, 0F0h
	jnz     short OkRate
	or      dl, ah
OkRate:         
	mov     [edi+TrackInfo.VibParm], dl
	mov     [edi+TrackInfo.Effect], dx
	test    cx, cx
	jz      short OkPos
	mov     byte [edi+TrackInfo.VibPos], 0
OkPos:          
	retn
efxsampoffset:
	; 01/10/2017 ; *******
SampleOfs:         
;	test    dl, dl
;	jnz     short SetSampleOfs
;	mov     dl, [edi+TrackInfo.OldSampOfs]
;SetSampleOfs:
;	mov     [edi+TrackInfo.OldSampOfs], dl
	mov     dh, dl
	and 	edx, 0FF00h ; 05/03/2017
	mov     [edi+TrackInfo.Position], edx
	retn
efxpattjump:
PosJump:
	mov     [OrderPos], dl
	mov     byte [Row], 64
	retn
efxsetvolume:
SetVolume:
	cmp     dl, 64
	jbe     short OkVol
	mov     dl, 64
OkVol:
	; 01/10/2017 (TrackInfo.VolDiff, tremolo effect)
	xor	dh, dh ; reset TrackInfo.VolDiff ; Not necessary !?
	;mov	[edi+TrackInfo.Volume], dl
	mov	[edi+TrackInfo.Volume], dx 
	retn
efxbreak:
Break:
	mov     dh, dl
	and     dl, 0Fh
	shr     dh, 4
	add     dh, dh
	add     dl, dh
	shl     dh, 2
	add     dl, dh
	mov     [BreakRow], dl
	mov     byte [Row], 64
	retn
efxsetspeed:
SetSpeed:
	test    dl,dl
	je      Skip
	cmp     dl,31
	ja      short SetBpm
SetTempo:       
	mov     [Tempo], dl
	mov     [TempoWait], dl
	retn
SetBpm:
	mov     [Bpm], dl
	mov     al, 103
	mul     dl
	mov     bl, ah
	xor     bh, bh
	mov     ax, [MixSpeed]
	xor     dx, dx
	div     bx
	mov     [BpmSamples], ax
Skip:           
	retn
efxarpeggio:
	; 01/10/2017
	test    dl, dl
	;je	efxnull
	je	short Skip
InitArpeggio:
	mov     dh, dl
	and     dl, 0Fh
	shr     dh, 4
	; 01/10/2017
	;mov	cx, 36
	mov	cx, 84 ; 84 notes/periods
	xor     ebx, ebx
	mov     ax, [edi+TrackInfo.Period]
gt_ScanPeriod:
	;cmp	ax, [PeriodTable+bx]
	cmp	ax, [PeriodTable+ebx]
	jae     short SetArp
	add     bx, 2
	loop    gt_ScanPeriod
SetArp:         
	add     dx, dx
	add     dh, bl
	add     dl, bl
	; 01/10/2017
	;mov	bx, [PeriodTable+bx]
	mov	bx, [PeriodTable+ebx]
	;add	bx, bx
	add	ebx, ebx
	;mov	ax, [PitchTable+bx]
	mov	ax, [PitchTable+ebx]
	mov     [edi+TrackInfo.Arp], ax
	mov     bl, dh
	xor     bh, bh
	mov	bx, [PeriodTable+ebx]
	;add	bx, bx
	add	ebx, ebx
	;mov	ax, [PitchTable+bx]
	mov	ax, [PitchTable+ebx]
	mov     [edi+TrackInfo.Arp+2], ax
	mov     bl, dl
	xor     bh, bh
	mov	bx, [PeriodTable+ebx]
	;add	bx, bx
	add	ebx, ebx
	;mov	ax, [PitchTable+bx]
	mov	ax, [PitchTable+ebx]
	mov     [edi+TrackInfo.Arp+4], ax
	mov     word [edi+TrackInfo.ArpIndex], 0
	retn

efxtremolo:
	; 01/10/2017 (TMODPLAY.ASM)
InitTremolo:
	mov     al, [edi+TrackInfo.TremParm]
	mov     ah, al
	and     ax, 0F00Fh
	test    dl, 0Fh
	jnz     short InitTremolo_1 ; efxtremolof0
	or      dl, al
efxtremolof0:
InitTremolo_1: 
	test    dl, 0F0h
	jnz     short InitTremolo_2 ; efxtremolof1
	or      dl, ah
efxtremolof1:
InitTremolo_2:
	mov     [edi+TrackInfo.TremParm], dl
	mov     [edi+TrackInfo.Effect], dx
	retn

;--------------------------------------------------------------------------
; pollmodule - polls the module player
;--------------------------------------------------------------------------
;--------------------------------------------------------------------------
; UpdateTracks:  Main code to process the next tick to be played.
;--------------------------------------------------------------------------

pollmodule:
UpdateTracks:	; polmodule ; 01/10/2017 (TMODPLAY.ASM)
	dec     byte [TempoWait]
	jz      short GetTracks

	;mov	ecx, NumTracks
	movzx	ecx, word [numtracks] ; 06/10/2017
	mov	edi, Tracks
BeatTracks:
	call	BeatTrack	
	add	edi, TrackInfo.size
	loop	BeatTracks
	retn
GetTracks:
	mov     al, [Tempo]
	mov     [TempoWait], al

	mov	esi, [Note]
	cmp     byte [Row], 64
	jb      short NoPattWrap

	mov	esi, [ModInfo.Patterns]
	mov     bl, [OrderPos]
	cmp     bl, [ModInfo.OrderLen]
	jb      short NoOrderWrap
	mov     bl, [ModInfo.ReStart]
	mov     [OrderPos], bl
	cmp     bl, [ModInfo.OrderLen]
	jae     short NoUpdate
NoOrderWrap:    
	;xor	bh, bh
	and	ebx, 0FFh
	mov     bl, [ModInfo.Order+ebx]
	; 05/10/2017
	;shl	ebx, 10 ; *1024
	mov	cl, [pattern_shift] ; 10 or 11
	shl	ebx, cl ; *1024 or *2048
	;
	add     esi, ebx
	mov     bl, [BreakRow]
	mov     [Row], bl
	;xor	bh, bh
	and	ebx, 0FFh
	mov     [BreakRow], bh ; 0
	shl     bx, 4
	add     esi, ebx
	mov     [Note], esi
	inc     byte [OrderPos]
NoPattWrap:     
	inc     byte [Row]

	;cld
	;mov	ecx, NumTracks
	movzx	ecx, word [numtracks] ; 06/10/2017
	mov	edi, Tracks
GetTracks_next:
	push	ecx	
	call	GetTrack ; readchannel
	pop	ecx
	add	edi, TrackInfo.size
	loop	GetTracks_next

	mov     [Note], esi
NoUpdate:
	retn

;--------------------------------------------------------------------------
; MixTrack:  Mixes one track into a CLEAN buffer.
;  In:
;   ds:si -  Track Info Address.
;   ds:di -  Buffer Address.
;    cx   -  Buffer Size.
;--------------------------------------------------------------------------

; esi = Track info address
; edi = Buffer address
; ecx = Buffer size

MixTrack:
	cmp     word [esi+TrackInfo.RepLen], 2
	ja      short MixLooped
MixNonLooped:   
	mov	edx, [esi+TrackInfo.Samples]
	mov	ebx, [esi+TrackInfo.Position]
	movzx   ebp, word [esi+TrackInfo.Len]
	push    edx
	push    esi
	add     ebx, edx
	add     ebp, edx
	mov     dx, [esi+TrackInfo.Pitch]
	; 01/10/2017
	;mov	al, [esi+TrackInfo.Volume]
	mov	ax, [esi+TrackInfo.Volume]
	; ah = [esi+TrackInfo.VolDiff]
	add	al, ah ; ****** 
	mov	byte [esi+TrackInfo.VolDiff], 0
	mov     ah, [esi+TrackInfo.Error]
	mov     esi, ebx
	xor	ebx, ebx ; 01/10/2017 ; *
	mov     bh, al
	mov     al, dl
	mov     dl, dh
	;xor	dh, dh
	and	edx, 0FFh
nlMixSamp:      
	cmp     esi, ebp
	jae     short nlMixBye
	mov     bl, [esi]
	;mov	bl, [VolTable+bx]
	mov	bl, [VolTable+ebx] ; 01/10/2017 ; *	
	add     [edi], bl
	inc     edi
	add     ah, al
	adc     esi, edx
	loop    nlMixSamp
nlMixBye:       
	mov     ebx, esi
	pop     esi
	pop     edx
	sub     ebx, edx
	mov     [esi+TrackInfo.Position], ebx
	mov     [esi+TrackInfo.Error], ah
	retn
MixLooped:
	mov	edx, [esi+TrackInfo.Samples]
	mov	ebx, [esi+TrackInfo.Position]
	movzx	ebp, word [esi+TrackInfo.RepLen]
	mov     [BufRep], ebp
	;add	ebp, [esi+TrackInfo.Repeat] ; BUG !
	add     bp, [esi+TrackInfo.Repeat] ; 07/10/2017 (BUGfix!)
	push    edx
	push    esi
	add     ebx, edx
	add     ebp, edx
	mov     dx, [esi+TrackInfo.Pitch]
	; 01/10/2017
	;mov	al, [esi+TrackInfo.Volume]
	mov	ax, [esi+TrackInfo.Volume]
	; ah = [esi+TrackInfo.VolDiff]
	add	al, ah ; ****** 
	mov	byte [esi+TrackInfo.VolDiff], 0
	mov     ah, [esi+TrackInfo.Error]
	;mov	si, bx
	mov	esi, ebx ; 04/09/2017
	xor	ebx, ebx ; 01/10/2017 ; *
	mov     bh, al
	mov     al, dl
	mov     dl, dh
	;xor	dh, dh
	and	edx, 0FFh
lpMixSamp:      
	cmp     esi, ebp
	jb      short lpMixNow
	sub     esi, [BufRep]
lpMixNow:       
	mov     bl, [esi]
	;mov	bl, [VolTable+bx]
	mov	bl, [VolTable+ebx] ; 01/10/2017 ; *
	add     [edi], bl
	inc     edi
	add     ah, al
	adc	esi, edx
	loop    lpMixSamp
lpMixBye:       
;	mov     ebx, esi
;	pop     esi
;	pop     edx
;	sub     ebx, edx
;	mov     [esi+TrackInfo.Position], ebx
;	mov     [esi+TrackInfo.Error], ah
;	retn
	jmp	short nlMixBye

;--------------------------------------------------------------------------
; mixpoll - updates the output buffer
;--------------------------------------------------------------------------
;
;--------------------------------------------------------------------------
; GetSamples:  Returns the next chunk of samples to be played.
;  In:
;    Buffer  - Buffer Address.
;    Count   - Buffer Size.
;--------------------------------------------------------------------------

mixpoll:
GetSamples:	; mixpoll ; 01/10/2017 (TMODPLAY.ASM)
	; edi = buffer address
	; ebx = count

	pushad

	;cld
NextChunk:      
	cmp     word [BufLen], 0
	jne     short CopyChunk

	push    ebx
	push    edi
MixChunk:       
	mov	edi, MixBuffer
	movzx	ecx, word [BpmSamples]
	;mov	cx, [BpmSamples]
	mov     [BufPtr], edi
	mov     [BufLen], cx

	mov     al, 80h
	rep     stosb

	;mov	cx, NumTracks
	;mov	cl, NumTracks ; 01/10/2017
	mov	cl, [numtracks] ; 06/10/2017
	mov	esi, Tracks - TrackInfo.size
GetSamples_next:
	push	ecx
	add	esi, TrackInfo.size
	mov	cx, [BufLen]
	mov	edi, [BufPtr]
	call	MixTrack
	pop	ecx
	loop	GetSamples_next	

	call    UpdateTracks

	pop     edi
	pop     ebx
CopyChunk:      
	;mov	cx, [BufLen]
	movzx	ecx, word [BufLen]
	cmp	ecx, ebx
	;cmp	cx, bx
	jbe     short MoveChunk
	;mov	cx, bx
	mov     ecx, ebx
MoveChunk:
	mov     esi, [BufPtr]
	add     [BufPtr], ecx
	sub     [BufLen], cx
	sub     ebx, ecx
	rep     movsb
	test    ebx, ebx
	jnz     short NextChunk

	popad	
	retn

;--------------------------------------------------------------------------
; StartPlaying: Initializes the Sound System.
;  In:
;   Module Information Resources.
;--------------------------------------------------------------------------

StartPlaying:
	pushad
SetModParms:    
	mov     byte [OrderPos], 0
	mov     byte [Tempo], DefTempo
	mov     byte [TempoWait], DefTempo
	mov     byte [Bpm], DefBpm
	mov     byte [Row], 64
	mov     byte [BreakRow], 0
	mov     ax, [MixSpeed]
	xor     edx, edx
	mov     bx, 24*DefBpm/60
	div     bx
	mov     [BpmSamples], ax
ClearTracks:    
	mov     edi, Tracks
	; 07/10/2017
	;mov	ecx, NumTracks*TrackInfo.size
	mov	eax, TrackInfo.size
	movzx	ecx, word [numtracks]
	mul	ecx
	mov	ecx, eax
	xor     eax, eax
	;cld
	rep     stosb

	mov     [BufPtr], eax
	mov     [BufLen], ax
MakePitch:
	mov     ax, MidCRate
	mov     bx, 428
	mul     bx
	div     word [MixSpeed]
	xor     dh, dh
	mov     dl, ah
	mov     ah, al
	xor     al, al
	;mov	cx, 857
	mov	cx, 3425  ; 01/10/2017 (TMODPLAY.ASM)
	xor     ebx, ebx
	mov     edi, PitchTable
PitchLoop:      
	push    eax
	push    edx
	cmp     dx, bx
	jae     short NoDiv
	div     bx
NoDiv:          
	stosw
	pop     edx
	pop     eax
	;inc	bx
	inc	ebx
	loop    PitchLoop
MakeVolume:     
	mov     cx, 16640
	mov     ebx, ecx
VolLoop:
	dec     bx
	mov     al, bl
	imul    bh
	;mov	[VolTable+bx], ah
	mov     [VolTable+ebx], ah
	loop    VolLoop

	popad
	retn

;--------------------------------------------------------------------------
; StopPlaying: ShutDown the Sound System.
;--------------------------------------------------------------------------

StopPlaying:
	; 19/06/2017
	; Stop Playing
	sys	_audio, 0700h
	; Cancel callback service (for user)
	sys	_audio, 0900h
	; Deallocate Audio Buffer (for user)
	sys	_audio, 0A00h
	; Disable Audio Device
	sys	_audio, 0C00h

	retn

;=============================================================================
;               preinitialized data
;=============================================================================

;=============================================================================
; Protracker effects stuff
;=============================================================================

;-----------------------------------------------------------------------------
; Effect jump tables
;-----------------------------------------------------------------------------

align 4

efxtable:
	dd      efxarpeggio	; 0 - arpeggio
	dd      efxnull		; 1 - porta up
	dd      efxnull		; 2 - porta down
	dd      efxtoneporta	; 3 - tone porta
	dd      efxvibrato	; 4 - vibrato
	dd      efxnull		; 5 - tone+slide
	dd      efxnull		; 6 - vibrato+slide
	dd      efxtremolo	; 7 - tremolo
	dd      efxnull		; 8 - unused
	dd      efxsampoffset	; 9 - sample offset
	dd      efxnull		; A - volume slide
	dd      efxpattjump	; B - pattern jump
	dd      efxsetvolume	; C - set volume
	dd      efxbreak	; D - break pattern
	dd      efxnull		; E - extra effects
	dd      efxsetspeed	; F - set speed

efxtable2:
	dd      efxarpeggio2	; 0 - arpeggio
	dd      efxportaup	; 1 - porta up
	dd      efxportadown	; 2 - porta down
	dd      efxtoneporta2	; 3 - tone porta
	dd      efxvibrato2	; 4 - vibrato
	dd      efxtoneslide	; 5 - tone+slide
	dd      efxvibslide	; 6 - vibrato+slide
	dd      efxtremolo2	; 7 - tremolo
	dd      efxnull		; 8 - unused
	dd      efxnull		; 9 - sample offset
	dd      efxvolslide	; A - volume slide
	dd      efxnull		; B - pattern jump
	dd      efxnull		; C - set volume
	dd      efxnull		; D - break pattern
	dd      efxnull		; E - extra effects
	dd      efxnull		; F - set speed

;-----------------------------------------------------------------------------
; Amiga period table
;-----------------------------------------------------------------------------

;PeriodTable0:	
;	dw	0
PeriodTable:
	dw	3424,3232,3048,2880,2712,2560,2416,2280,2152,2032,1920,1812
	dw	1712,1616,1524,1440,1356,1280,1208,1140,1076,1016,960,906
	dw	856,808,762,720,678,640,604,570,538,508,480,453
	dw	428,404,381,360,339,320,302,285,269,254,240,226
	dw	214,202,190,180,170,160,151,143,135,127,120,113
	dw	107,101,95,90,85,80,75,71,67,63,60,56
	dw	53,50,47,45,42,40,37,35,33,31,30,28

;-----------------------------------------------------------------------------
; Sinus wave table
;-----------------------------------------------------------------------------

SinTable:
	db	0,25,50,74,98,120,142,162,180,197,212,225
	db	236,244,250,254,255,254,250,244,236,225
	db	212,197,180,162,142,120,98,74,50,25

;=============================================================================
;               PLAY.ASM - DATA
;=============================================================================

msg_2017:
		db 'Tiny MOD Player for TRDOS 386 by Erdogan Tan. '
		db 'October 2017.',10,13
		db 'usage: modplay filename.mod', 10,13,0
		db '08/10/2017',10,13,0

Credits:	db 'Tiny MOD Player v0.1b by Carlos Hasan. July 1993.'
		db 10,13,0
ErrorMesg:	db 'Error loading Module file.',10,13,0
MsgNotFound:	db 'Sound Blaster not found or IRQ error.',10,13,0
MsgFound:	db 'Sound Blaster found at Address 2'
PortText:	db 'x0h, IRQ '
IrqText:	db 'x.',10,13,0

trdos386_err_msg:
		db 'TRDOS 386 System call error !', 10, 13,0

; 07/10/2017
pattern_shift:	db 10
numtracks:	dw 4

;=============================================================================
;               SB.ASM - DATA
;=============================================================================

SbAddr:		dw 220h
SbIrq:		db 7

;=============================================================================
;               PLAYER.ASM - DATA
;=============================================================================

stmo:		db 1 ; stereo (2) or mono (1)  
bps:		db 8 ; bits per sample (8 or 16)
Sample_Rate:
;MixSpeed:	dw 22050 ; Hz
MixSpeed:	dw 22222 ; Hz ; 07/10/2017

; 13/11/2016
hex_chars:	db "0123456789ABCDEF", 0
;
;; 13/11/2016 - Erdogan Tan (Ref: KolibriOS, codec.inc)
;codec_id:	   dd 0
;codec_chip_id:	   dd 0
;codec_vendor_ids: dw 0
;codec_chip_ids:   dw 0

;dword_str:	dd 30303030h, 30303030h
;	 	db 'h', 0Dh, 0Ah, 0

;=============================================================================
;        	uninitialized data
;=============================================================================

bss_start:

ABSOLUTE bss_start

alignb 4

dev_vendor:	resd 1
bus_dev_fn:	resd 1
stats_cmd:	resd 1
ac97_NamBar:	resw 1
ac97_NabmBar:	resw 1
ac97_int_ln_reg: resb 1
srb:		resb 1

; MODLOAD.ASM
FileHandle:	resd 1
Header:		resb ModHeader.size

; MODPLAY.ASM
;MixSpeed:	    resw 1

ModInfo:
ModInfo.OrderLen:   resb 1
ModInfo.ReStart:    resb 1
ModInfo.Order:	    resb 128
ModInfo.Patterns:   resd 1

ModInfo.SampOfs:    resw 31
ModInfo.SampSeg:    resw 31
ModInfo.SampLen:    resw 31
ModInfo.SampRep:    resw 31
ModInfo.SampRepLen: resw 31
ModInfo.SampVol:    resw 31

; MODPLAY.ASM
PitchTable:	;resw 857
		resw 3425 ; 01/10/2017 (TMODPLAY.ASM)
VolTable:	resb 16640
MixBuffer       resb MixBufSize

; MODPLAY.ASM
OrderPos:	resb 1
Tempo:		resb 1
TempoWait:	resb 1
Bpm:		resb 1
Row:		resb 1
BreakRow:	resb 1
BpmSamples:	resw 1
BufPtr:		resd 1
BufLen:		resw 1
BufRep:		resd 1
Note:		resd 1
;Tracks:	resb TrackInfo.size*NumTracks
; 07/10/2017
Tracks:		resb TrackInfo.size*8

alignb 16

; PLAY.ASM
Scope:		resw 320
RowOfs:		resw 256

mod_file_name:
		resb 80

alignb 4096

Audio_Buffer:
		resb BUFFERSIZE ; DMA Buffer Size / 2  (32768)
alignb 65536

DMA_Buffer:
		resb 65536	
file_buffer:
		resb 65536*6
EOF: