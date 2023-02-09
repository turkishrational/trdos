; ****************************************************************************
; loadpcx2.s (TRDOS 386, TRDOS v2.0 - sample binary file, 'loadpcx2.prg')
; ----------------------------------------------------------------------------
; LOADPCX2.PRG ! 'sysfff' and 'sysfnf', 'sysopen', 'sysread'
;		 			and VGA TEST program for TRDOS 386 !
;
; 14/10/2016
;
; [ Last Modification: 16/10/2016 ]
;
; Derived from: MICROPCX.ASM (1994, Lord Debugger. Poznan, Poland.)
;
; Note: This program displays 320x200x256 pcx files only.
; ****************************************************************************
; Previous Version: loadpcx.s (14/10/2016)

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
_rsvd1	equ 37
_pri	equ 38
_rele	equ 39
_fff	equ 40
_fnf	equ 41

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

[BITS 32] ; We need 32-bit intructions for protected mode

[ORG 0] 

START_CODE:
	mov	esi, esp
	lodsd
	cmp	eax, 2 ; two arguments (program file name & text file name)
	jb	terminate ; nothing top do
	lodsd ; program file name address 
	lodsd ; text file name address
	; EAX = arg2 ; file name address

	; EBX = EAX = file name or path address
	; CL = file attributes (archive = 20h, read only = 01h)
	;	(21h = Archived & Read Only files are included)
	; CH = 0 = basic parameters (24 bytes)
	; EDX = DTA = buffer address (24 bytes for basic parameters)
	; EAX = _fff = 'Find First File' system call for TRDOS 386

	; Find First File
	sys	_fff, eax, 0021h, DTA
	jc	_terminate ; terminate if there is 
			   ; file not found error or another error
_0:
	; check file attributes
	test	byte [DTA], 1Eh ; 10h = directory, 08h = volume label
				; 04h = system, 02h = hidden
	jz	short _2   ; atributes are proper 
			   ; but we need to check ".PCX" extension		
_1:
	; Find Next File
	sys	_fnf	; if the first file is not proper file
			; check for next file
	jc	short _show_pcx
	jmp	short _0
_2:
	mov	esi, DTA+11 ; 2nd char of file name 
			    ; (ASCIIZ, capitalized file name)
_3:
	lodsb
	cmp	al, 20h
	jna	short _1

	cmp	al, '.'
	jne	short _3

	lodsb
	cmp	al, 'P'
	jne	short _1
	lodsw
	cmp	al, 'C'
	jne	short _1
	cmp	ah, 'X'		
	jne	short _1

_4: 	; OK ! We have 1 PCX file at least, to show
	; DTA: Offset
	;	0 - file attributes byte
	;	1 - ambiguous file name wildcards are used sign
	;	2 - time stamp of file
	;	4 - date stamp off file
	;	6 - file size in bytes
	;      10 - file name (ASCIIZ) 
	;	   (in 8.3 dos filename format as capitalized)

	; Move/Copy File Name (from DTA) to Name_Buffer
	mov	edi, [Buffer_Pos]
	mov	esi, DTA+10
	mov	ecx, 13
	rep	movsb
	mov	[Buffer_Pos], edi

	inc	byte [Max_Files] 
	jnz	short _1

 _show_pcx:
	mov	ebx, Name_Buffer
	mov	[Buffer_Pos], ebx

	; DIRECT VGA MEMORY ACCESS

 	;xor    ebx, ebx
 	;mov    bh, 5 ; Direct access/map to VGA memory (0A0000h)
 	;mov    eax, _video ; 1Fh
 	;mov    al, 1Fh ; sys _video ; TRDOS 386 Video functions
 	;int    40h   ; TRDOS 386 system call
	;jc	terminate
	
	; BH = 5 -> Direct access/map to VGA memory 
	; BL = 0 -> 320 pixels width or default VGA screen width

	sys	_video, 0500h  ; sys macro formats INT 40h system calls
	jc	_terminate ; in fact, an error is not possible
			   ; if multi tasking is not enabled -default-
			   ; (VGA memory -0A0000h- will be free to use
			   ; if multi tasking is off!)
			   ; (If TRDOS 386 kernel has not got a memory
			   ; allocation error, as bug!?)
			   ; Note: As default, TRDOS 386 kernel 
			   ; does not enable multi tasking feature
			   ; (Also, it is not programmed yet, 
			   ; for October 2016).			 
set_vga_mode:
	mov    	ax, 13h    ; function 00h, mode 13h 
	;int    10h        ; bios video interrupt
	int     31h ; TRDOS 386 - Video interrupt
		    ; Note: This is not a system call -INT 40h-,
		    ; this is a direct video service like as
		    ; INT 10h ROMBIOS service in real mode.

open_file:
	mov	esi, [Buffer_Pos]
		
	sys	_open, esi, 1800h ; open for reading
	jc	open_error

	mov	[fhandle], eax ; file handle/index number

read_file:
	sys	_read, eax, IMAGE_BUFFER, 0FFFFFFFFh ; read all bytes 
 	jc	read_error  ; disk read or memory allocation error

	xor	ecx, ecx			
	mov	edi, eax ; file size (read bytes)

close_file:
	sys	_close, [fhandle] ; close file

	add	edi, IMAGE_BUFFER+3
	and	di, 0FFFCh ; dword alignment
	mov	ebp, edi
	mov	edx, ebp
	add	edx, 64001
	mov	esi, IMAGE_BUFFER + 128 ; PCX header + data
ldlp1:
	lodsb		
	mov	ah, al
	and	ah, 0C0h
	cmp	ah, 0C0h
	jne	short single
	and	al, 3Fh
	mov	cl, al
	lodsb
ldlp2:  rep stosb
	jmp	short cnt
single:
	stosb
cnt:
	cmp	edi, edx ; 64001 ; end of pixel data
	jb	short ldlp1

	mov	edx, esi	
	mov	edi, esi
	mov	cx, 768
set256:
	lodsb
	shr	al, 2
	stosb
	loop    set256
	mov	ax, 1012h 	; set palette registers
	xor	ebx, ebx
	mov	cx, 256
	;int	10h
	int	31h		; TRDOS 386 Video interrupt

; move_image_to_VGA_address:
	mov	esi, ebp
	mov	cx, 16000
	mov	edi, 0A0000h
	rep	movsd	

	xor	ah, ah
	;int	16h	; KEYBOARD - READ CHAR FROM BUFFER, WAIT IF EMPTY
			; Return: AH = scan code, AL = character
	int	32h	; TRDOS 386 Keyboard interrupt 
		    	; Note: This is not a system call -INT 40h-,
		    	; this is a direct video service like as
		    	; INT 16h ROMBIOS service in real mode.

	cmp	al, 1Bh ; ESC key
	je	short _terminate

	dec	byte [Max_Files]
	jz	short _terminate

	add	dword [Buffer_Pos], 13

	jmp	open_file ; loop (we use same code for the next file)

_terminate:
	call	set_text_mode
terminate:
	sys 	_exit			   ; INT 40h
here:
	jmp	short here

open_error:
	call	set_text_mode
	mov	esi, msg_open_error
	call	print_msg		   ; INT 31h
	jmp	short terminate	

read_error:
	sys	_close, [fhandle] ; close file
	call	set_text_mode
	mov	esi, msg_read_error
	call	print_msg		   ; INT 31h
	jmp	short terminate

print_msg:
	mov	ebx, 0Eh       ; yellow characters (bl)
		               ; video page 0 (bh)
	;mov	ah, 0Eh ; teletype output (write tty)
	mov	ah, bl
	lodsb
_p_nextchar:
	int	31h
	lodsb
	and	al, al
	jnz	short _p_nextchar
	retn

set_text_mode:
	xor    ah, ah
	mov    al, 3                        
 	;int   10h  	 ; al = 03h text mode, int 10h video
	int    31h 	 ; TRDOS 386 - Video interrupt
	retn

Max_Files:
	db 0

Buffer_Pos:
	dd Name_Buffer

;-----------------------------------------------------------------
;  messages
;-----------------------------------------------------------------

msg_program:
	db 0Dh, 0Ah
	db "LOADPCX2.PRG /// TRDOS 386 sysfff, sysfnf test program"
	db 0Dh, 0Ah
	db "by Erdogan Tan, 16/10/2016", 0Dh, 0Ah, 0
msg_open_error:
	db 0Dh, 0Ah
	db 'sysopen error !'
	db 0Dh, 0Ah, 0
msg_read_error:
	db 0Dh, 0Ah
	db 'read error !'
nextline:
	db 0Dh, 0Ah, 0

bss:

ABSOLUTE bss

alignb 4

fhandle: resd 1

DTA:	 resb 24

Name_Buffer:	

IMAGE_BUFFER equ 10000h	 