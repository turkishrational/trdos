; ****************************************************************************
; setdow.s (TRDOS 386, TRDOS v2.0.11 - sample binary file, 'setdow.prg')
; ----------------------------------------------------------------------------
; SETDOW.PRG ! "Set Day of Week" - 'sysstime' TEST program for TRDOS 386 !
;
; 17/05/2026
;
; [ Last Modification: 17/05/2026 ]
;
; ****************************************************************************
; nasm setdow.s -l setdow.txt -o SETDOW.PRG

; ref: dow4.s (07/05/2026)

; 30/04/2026
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
_stdio	equ 46

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

[BITS 32] ; 32-bit intructions

[ORG 0] 

START_CODE:
	mov	esi, esp
	lodsd
	cmp	eax, 2 ; two arguments (program file name & dow)
	jb	short g_date ; do not set
	lodsd ; program file name address
	lodsd ; day of week (1 = sunday, 7 = monday)

	mov	esi, eax
	lodsb
	
	mov	byte [flag], 1

	;cmp	al, '1'
	cmp	al, '0'
	jb	short p_usage
	;cmp	al, '7'
	cmp	al, '9'
	ja	short p_usage

	sub	al, '0'
	mov	[dow_input], al

	lodsb
	and	al, al
	jz	short g_date

p_usage:
	mov	esi, usage
	call	print_msg

	jmp	terminate

g_date:
	; Get Date&Time in MSDOS (TRDOS 386) format
	sys	_time, 3
	mov	eax, edx ; date
g_dow:
	mov	[date], eax
	; Get Day Of The Week
	sys	_time, 8
	mov	[dow], eax  ; (RTC CMOS register 6)
	; Get Date in MSDOS (TRDOS 386) format
	sys	_time, 2
	cmp	eax, [date]
	jne	short g_dow
p_date:
	mov	[day], al
	mov	[month], ah
	shr	eax, 16
	mov	[year], eax

	push	dword [day]
	push	dword [month]
	push	dword [year]

	call	DayOfWeek

	add	esp, 12

	; eax = day of the week from this program (0 = sunday)
	inc	eax
	mov	[ndow], eax
	; [dow] = day of the week from RTC CMOS register 6
	cmp	eax, [dow]
	je	short skip_dow_fix

	or	byte [flag], 80h
skip_dow_fix:
	mov	esi, header
	call	print_msg

	mov	edi, txt_date

	mov	eax, [day]
	call	bin_to_str_2

	mov	byte [edi],'/'
 	inc	edi

	mov	eax, [month]
	call	bin_to_str_2

	mov	byte [edi],'/'
 	inc	edi

	mov	eax, [year]
	call	bin_to_str_4

	mov	byte [edi],0

	mov	esi, txt_date
	call	print_msg

	mov	esi, _space
	call	print_msg

	mov	ebx, [dow]
	dec	ebx ; 1 .. 7 -> 0 .. 6
	shl	ebx, 2	; * 4
	add	ebx, day_names
	mov	esi, [ebx]
	call	print_msg

	mov	esi, newline
	call	print_msg

	cmp	byte [flag], 80h
	jb	short skip_wdow_msg

	mov	esi, wrong_dow_msg
	call	print_msg
	
skip_wdow_msg:
	test	byte [flag], 0FFh
	jz	short p_newline

	mov	esi, newline
	call	print_msg

	mov	esi, confirm
	call	print_msg

getchar:
	; TRDOS 386 keyboard input interrupt
	; (ROMBIOS INT 16h simulation)
	xor	ah, ah
	int	32h

	cmp	al, 27	; ESC (1Bh)
	je	short p_newline
	cmp	al, 13	; ENTER/CR (0Dh)
	jne	short getchar

	mov	ecx, [ndow]
	test 	byte [flag], 1 ; manual dow
	jz	short s_dow
	; use input (even if it is a wrong dow)
	mov	ecx, [dow_input]
s_dow:
	; Set Day of the Week (RTC CMOS register 6)
	sys	_stime, 11   ; bl = 11, ecx = dow
	jc	short p_newline

	; updated message
	mov	esi, updated
	call	print_msg		
	
p_newline:
	mov	esi, newline
	call	print_msg

terminate:
	sys	_exit, 0

;-----------------------------------------------------------------

hang:
	jmp	short hang

;-----------------------------------------------------------------

	; 06/05/2026
bin_to_str_4:
	mov	esi, 4
	jmp	short bin_to_str
bin_to_str_2:
	mov	esi, 2
bin_to_str:
	xor	ecx, ecx ; 0
	mov	ebp, esp
	mov	ebx, 10
bin2str_div:
	xor	edx, edx
	div	ebx
	add	dl, '0'
	push	edx
	inc	ecx
	;cmp	eax, 0
	;ja	short bin2str_div
	and	eax, eax
	jnz	short bin2str_div

	mov	ebx, esi
	add	esi, edi
	sub	ebx, ecx
	jng	short skip_zero_prefix
	; eax = 0
	add	al,'0'
zero_prefix:
	stosb
	dec	ebx
	jnz	short zero_prefix

skip_zero_prefix:
pop_next:
	pop	eax
	cmp	edi, esi
	jnb	short skip_stosb
	stosb
skip_stosb:
	cmp	esp, ebp
	jb	short pop_next

bin2str_ok:
	retn

;-----------------------------------------------------------------

print_msg:
	mov	ebx, 0Fh       ; white characters (bl)
		               ; video page 0 (bh)
print_msg_@:
	mov	ah, 0Eh ; teletype output (write tty)
	lodsb
_p_nextchar:
	int	31h
	lodsb
	and	al, al
	jnz	short _p_nextchar
	retn

;-----------------------------------------------------------------
;
;-----------------------------------------------------------------

; Work out what the day of the week is for a given date
; Speed optimized x86 assembly (PIII) by Lingo
;
; C Algorithm by Tomohiko Sakamoto
; int dayofweek(int d, int m, int y) {
;   static int t[] = { 0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4 };
;   y -= m < 3;
;   return ( y + y/4 - y/100 + y/400 + t[m-1] + d) % 7;
; }
; year  = [0, 9999+]
; month = [1, 12]
; day   = [1, 31]

;DayOfWeekL  PROTO year:DWORD, month:DWORD, day:DWORD

DayOfWeek:
  mov  ecx, [esp+8]               ; ecx=month
  mov  edx, [esp+4]               ; edx=year
  cmp  ecx, 3                     ; test if month is < 3
  sbb  edx, 0                     ; if month < 3 then dec edx->years
  mov  ecx, [monthdata + ecx*4-4] ; ecx-> monthdata[m-1]
  mov  eax, edx                   ; eax=edx=years
  imul edx, 0A3D8h                ; dividing edx=years by 100 == imul edx, 0A3D8h / shr edx, 22
  add  ecx, [esp+12]              ; add day; ecx= monthdata[m-1] + day
  add  ecx, eax                   ; ecx-> monthdata[m-1] + day + years
  shr  eax, 2                     ; eax= years/4
  add  eax, ecx                   ; eax= monthdata[m-1] + day + years + years/4
  ;mov ecx, [esp]                 ; ecx->return address
  ;mov [esp+12], ecx              ; ecx->return address to right place
  shr  edx, 22                    ; end of dividing ; edx=years/100
  sub  eax, edx                   ; eax= monthdata[m-1] +day + years + years/4 - years/100
  shr  edx, 2                     ; edx=years/400
  add  eax, edx                   ; eax= monthdata[m-1] +day + years + years/4 - years/100 + years/400
  mov  edx, 24924925h             ; dividing eax by 7 == mul edx, eax ; 24924925h=dword (1/7 * 2^35)
  mov  ecx, eax                   ; ecx= monthdata[m-1]  + day + years + years/4 - years/100 + years/400
  mul  edx                        ; dividing eax by 7 ->result in edx
  mov  eax, ecx                   ; eax= monthdata[m-1] + day + years + years/4 - years/100 + years/400
  lea  ecx, [edx+edx*2]           ; multiplying edx by 7 ->
  lea  edx, [ecx+edx*4]           ; ->result in edx
  ;add  esp, 3*4                  ; clearing the stack from 3 dword parameters
  sub  eax, edx                   ; eax= (monthdata[m-1] + day + years + years/4 - years/100 + years/400) % 7
  retn                            ; faster return then return 3*4

;-----------------------------------------------------------------
;
;-----------------------------------------------------------------

monthdata: dd 0,3,2,5,0,3,5,1,4,6,2,4

;-----------------------------------------------------------------

usage:
	db "TRDOS 386 v2 'systime' & 'sysstime' system call",0Dh,0Ah
	db  "test program by Erdogan Tan [May 2026]",0Dh,0Ah
	db  0Dh, 0Ah
	db  "Usage: setdow day_of_week",0Dh,0Ah
	db  "      (1 = sunday ... 7 = saturday)"
newline:
	db 0Dh,0Ah,0

header:	db 0Dh, 0Ah
	db "Current Date:"
_space:	db " ",0

_sunday:    db "SUNDAY",0
_monday:    db "MONDAY",0
_tuesday:   db "TUESDAY",0
_wednesday: db "WEDNESDAY",0
_thursday:  db "THURSDAY",0
_friday:    db "FRIDAY",0
_saturday:  db "SATURDAY",0

flag:	db 0	; epoch input flag

align 4

day_names:
	dd _sunday
	dd _monday
	dd _tuesday
	dd _wednesday
	dd _thursday
	dd _friday
	dd _saturday

year:	dd 0
month:	dd 0
day:	dd 0
hour:	dd 0
minute: dd 0
second: dd 0
dow_input:
	dd 0

wrong_dow_msg:
	db 0Dh,0Ah
	db "Wrong day of the week in RTC CMOS register 6 !"
	db 0Dh,0Ah,0
confirm:
	db 0Dh,0Ah
	db "Press ENTER to update day of the week",0Dh,0Ah
	db "or press ESC to cancel",0Dh,0Ah,0
updated:
	db 0Dh,0Ah
	db "OK ...",0Dh,0Ah,0

;-----------------------------------------------------------------
;  uninitialized data
;-----------------------------------------------------------------

bss:

ABSOLUTE bss

date:	resd 1
dow:	resd 1
ndow:	resd 1

txt_date:
	resb 12
