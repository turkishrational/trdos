; ****************************************************************************
; dow.s (TRDOS 386, TRDOS v2.0.11 - sample binary file, 'dow.prg')
; ----------------------------------------------------------------------------
; DOW.PRG ! "Day of Week" - 'systime' TEST program for TRDOS 386 !
;
; 06/05/2026
;
; [ Last Modification: 07/05/2026 ]
;
; ****************************************************************************
; nasm dow3.asm -l dow3.txt -o DOW3.PRG

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
	cmp	eax, 2 ; two arguments (program file name & date)
	jb	short sysdate ; use sistem date
	lodsd ; program file name address
	lodsd ; date string address

	; EAX = arg2 ; date (text)
	mov	esi, eax
	call	str_to_date
	jnc	short p_date

	mov	esi, usage
	call	print_msg

	jmp	terminate

sysdate:
	; Get Date&Time in MSDOS (TRDOS 386) format (1980->1980)
	sys	_time, 3

	; EAX = Current System Time (RTC)
	;  AL = Second (DL in MSDOS)
	;  AH = Minute (CL in MSDOS)
	;  HW of EAX = Hour (CH in MSDOS)
	; EDX = Current System Date (RTC)
	;  DL = Day (DL in MSDOS)
	;  DH = Month (DH in MSDOS)
	;  HW of EDX = Year (CX in MSDOS)

	mov 	byte [second], al
	mov 	byte [minute], ah
	shr	eax, 16
	;mov 	byte [hour], al
	mov 	word [hour], ax

	mov 	byte [day], dl
	mov 	byte [month], dh
	shr	edx, 16
	mov 	word [year], dx

p_date:
	push	dword [day]
	push	dword [month]
	push	dword [year]

	call	DayOfWeek

	add	esp, 12

	mov	[dow], eax

	cmp	byte [separator], 0
	ja	short skip_header

	mov	esi, header
	call	print_msg

skip_header:
	mov	esi, newline
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
	shl	ebx, 2	; * 4
	add	ebx, day_names
	mov	esi, [ebx]
	call	print_msg

	cmp	byte [separator], 0
	ja	short skip_time

	mov	esi, newline
	call	print_msg

	mov	edi, txt_time

	mov	eax, [hour]
	call	bin_to_str_2

	mov	byte [edi],':'
 	inc	edi

	mov	eax, [minute]
	call	bin_to_str_2

	mov	byte [edi],':'
 	inc	edi

	mov	eax, [second]
	call	bin_to_str_2

	mov	byte [edi],0

	mov	esi, txt_time
	call	print_msg

skip_time:
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

	; 06/05/2026 - Erdogan Tan
str_to_date:
	; esi = date string (dd/mm/yyyy or dd-mm-yyyy format)
	xor	eax, eax
	lodsb
	cmp	al, '0'
	jb	short _fail
	cmp	al, '9'
	ja	short _fail_stc
	mov	cl, al
	sub	al, '0'
	mov	[day], eax
	lodsb
	cmp	al, '0'
	jb	short s2d1
	cmp	al, '9'
	ja	short _fail_stc
	cmp	cl, '3'
	ja	short _fail_stc
	jb	short s2d0
	cmp	al, '1'
	;ja	short _fail_stc
	jna	short s2d0
_fail_stc:
	stc
_fail:
	retn
s2d0:
	mov	cl, al
	sub	cl, '0'
	mov	al, 10
	mul	byte [day]
	mov	[day], al
	add	[day], cl
	lodsb
s2d1:
	cmp	al, '-'
	je	short s2d2
	cmp	al, '/'
	jne	short fail
s2d2:
	cmp	byte [day], 1
	jb	short _fail

	mov	[separator], al
	;
	lodsb
	cmp	al, '0'
	jb	short fail
	cmp	al, '9'
	ja	short fail_stc
	mov	cl, al
	sub	al, '0'
	mov	[month], eax
	lodsb
	cmp	al, '0'
	jb	short s2d4
	cmp	al, '9'
	ja	short fail_stc
	cmp	cl, '1'
	ja	short fail_stc
	jb	short s2d3
	cmp	al, '2'
	ja	short fail_stc
s2d3:
	mov	cl, al
	sub	cl, '0'
	mov	al, 10
	mul	byte [month]
	mov	[month], al
	add	[month], cl
	lodsb
s2d4:
	cmp	al, [separator]
	jne	short fail_stc
	;
	cmp	byte [month], 1
	jb	short fail

	mov	edx, mdays-1
	add	edx, [month]
	mov	cl, [day]
	cmp	cl, [edx]
	jna	short s2d5

fail_stc:
	stc
fail:
	retn

s2d5:
	lodsb
	cmp	al, '0'
	jb	short fail
	cmp	al, '9'
	ja	short fail_stc
	sub	al, '0'
	mov	[year], eax
	xor	ebx, ebx
	lodsb
	cmp	al, '0'
	jb	short s2d6
	cmp	al, '9'
	ja	short fail_stc
	sub	al, '0'
	mov	ecx, eax
	mov	al, 10
	mul	byte [year]
	mov	[year], al
	add	[year], cl
	inc	ebx
	lodsb
	cmp	al, '0'
	jb	short s2d6
	cmp	al, '9'
	ja	short fail_stc
	sub	al, '0'
	mov	ecx, eax
	mov	al, 10
	mul	dword [year]
	mov	[year], eax
	add	[year], ecx
	inc	ebx
	xor 	eax, eax
	lodsb
	cmp	al, '0'
	jb	short s2d6
	cmp	al, '9'
	ja	short fail_stc
	sub	al, '0'
	mov	ecx, eax
	mov	al, 10
	mul	dword [year]
	mov	[year], eax
	add	[year], ecx
	inc	ebx
	lodsb
s2d6:
	and	al, al
	jnz	short fail_stc
	;

	; dd/mm/yy to dd/mm/20yy
	; correction
	cmp	bl, 1
	jne	short s2d7

	add	dword [year], 2000
s2d7:
	cmp	byte [month], 2
	ja	short s2d9
	jb	short s2d8

	test	byte [year], 3
	jz	short s2d8

	cmp	byte [day], 29
s2d8:
	cmc
s2d9:
	; if cf = 0
	    ; [day] = day
	    ; [month] = month
	    ; [year] = year
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

separator: db 0

usage:
	db "TRDOS 386 v2 'systime' system call test program", 0Dh, 0Ah
	db  "by Erdogan Tan [May 2026]", 0Dh, 0Ah
	db  0Dh, 0Ah
	db  "Usage: dow dd/mm/yyyy", 0Dh, 0Ah, 0 	

	 db 0
header:	 db 0Dh, 0Ah
	 db "Current Date&Time: "
newline: db 0Dh,0Ah,0
_space:	 db " ",0

_sunday:   db "SUNDAY",0
_monday:   db "MONDAY",0
_tuesday:  db "TUESDAY",0
_wednesday: db "WEDNESDAY",0
_thursday: db "THURSDAY",0
_friday:   db "FRIDAY",0
_saturday: db "SATURDAY",0

align 4

day_names:
	dd _sunday
	dd _monday
	dd _tuesday
	dd _wednesday
	dd _thursday
	dd _friday
	dd _saturday

mdays:	db 31,29,31,30,31,30,31,31,30,31,30,31

;-----------------------------------------------------------------
;  uninitialized data
;-----------------------------------------------------------------

bss:

ABSOLUTE bss

alignb 4

year:	resd 1
month:	resd 1
day:	resd 1
hour:	resd 1
minute: resd 1
second: resd 1
dow:	resd 1

txt_date:
	resb 12
txt_time:
	resb 10
