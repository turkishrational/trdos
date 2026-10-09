;BEEF MOUSE (THE COW OF MOUSE DRIVERS)
;PS/2 MOUSE DRIVER FOR ANY OS
;Tested in FreeDOS
;converted to FASM by Shawn T. Cook (Bugs fixed!)
;Version 3.0
;runs smoother now.

Org 100h

mov ah, 0  ;text screen mode
mov al, 13h
int  0x10

;I will use the NASM syntax!! CONVERTED TO FASM!
;I made this code for a Logitech M-S48a mouse. (normal PS/2 Code)
;For some reason the keyboard wil stall after program is terminated :S

;If you have problems understandig this code take a look at my mouseprog for the COM-mouse

;(Piece of disclaimer)
;(I do NOT give any guarantee or what so ever on this piece of code.)
;(I am not responsible for any damage whatsoever                    )
;(By using this code u accept the two lines above                   )
;(USAGE IS AT YOUR OWN RISK                                         )

;***************************************************
;* This is for the system to know where to jump to *
;***************************************************
; load source into DS:SI,           *
; load target into ES:DI:           *
        MOV     AX, CS             ;*
        MOV     DS, AX             ;*
        MOV     ES, AX                            ;*
;***************************************************

JMP MAINP

;***********************************************************************
;Activate mouse port (PS/2)
;***********************************************************************
PS2SET:
  mov  al, 0xa8      ; enable mouse port
  out  0x64, al      ; write to keyboardcontroller
  call CHKPRT     ; check if command is progressed (demand!)
ret

;***********************************************************************
;Check if command is accepted. (not got stuck in inputbuffer)
;***********************************************************************
CHKPRT:
  xor  cx, cx     
 again:
  in   al, 0x64      ; read from keyboardcontroller
  test al, 2      ; Check if input buffer is empty
  jz  gogo
  jmp again      ; (demand!) This may couse hanging, use only when sure.
 gogo:
ret

;***********************************************************************
;Write to mouse
;***********************************************************************
WMOUS:
  mov  al, 0xd4      ; write to mouse device instead of to keyboard
  out  0x64, al      ; write to keyboardcontroller
  call CHKPRT     ; check if command is progressed (demand!)
ret

;***********************************************************************
;mouse output buffer full
;***********************************************************************
MBUFFUL:
  xor  cx, cx
 mn:
  in   al, 0x64      ; read from keyboardcontroller
  test al, 0x20      ; check if mouse output buffer is full
  jz  mnn
  loop mn
 mnn:
ret

;***********************************************************************
;Write activate Mouse HardWare
;***********************************************************************
ACTMOUS:
  call WMOUS
  mov  al, 0xf4   ; Command to activate mouse itselve (Stream mode)
  out  0x60, al      ; write ps/2 controller output port (activate mouse)
  call CHKPRT     ; check if command is progressed (demand!)
  call CHKMOUS    ; check if a byte is available
ret

;***********************************************************************
;Check if mouse has info for us
;***********************************************************************
CHKMOUS:
  mov  bl, 0
  xor  cx, cx
 vrd:
  in   al, 0x64      ; read from keyboardcontroller
  test al, 1      ; check if controller buffer (60h) has data
  jnz yy
  loop vrd
  mov  bl, 1
 yy:
ret

;***********************************************************************
;Disable Keyboard
;***********************************************************************
DKEYB:
  mov  al, 0xad      ; Disable Keyboard
  out  0x64, al      ; write to keyboardcontroller
  call CHKPRT     ; check if command is progressed (demand!)
ret

;***********************************************************************
;Enable Keyboard
;***********************************************************************
EKEYB:
  mov  al, 0xae      ; Enable Keyboard
  out  0x64, al      ; write to keyboardcontroller
  call CHKPRT     ; check if command is progressed (demand!)
ret

;***********************************************************************
;Get Mouse Byte
;***********************************************************************
GETB:
 cagain:
  call CHKMOUS    ; check if a byte is available
  or   bl, bl
  jnz cagain
  call DKEYB      ; disable keyboard to read mouse byte
  xor  ax, ax
  in   al, 0x60      ; read ps/2 controller output port (mousebyte)
  mov  dl, al
  call EKEYB      ; enable keyboard
  mov  al, dl
ret


;***********************************************************************
;Get 1ST Mouse Byte
;***********************************************************************
GETFIRST:
  call GETB       ;Get byte1 of packet
  xor  ah, ah
  mov  bl, al
  and  bl, 1
  mov  BYTE [cs:LBUTTON], bl
  mov  bl, al
  and  bl, 2
  mov  BYTE [cs:RBUTTON], bl
  mov  bl, al
  and  bl, 4
  mov  BYTE [cs:MBUTTON], bl
  mov  bl, al
  and  bl, 8
  mov  BYTE [cs:XCOORDN], bl
  mov  bl, al
  and  bl, 16
  mov  BYTE [cs:YCOORDN], bl
  mov  bl, al
  and  bl, 32
  mov  BYTE [cs:XFLOW], bl
  mov  bl, al
  and  bl, 64
  mov  BYTE [cs:YFLOW], bl
ret

;***********************************************************************
;Get 2ND Mouse Byte
;***********************************************************************
GETSECOND:
  call GETB       ;Get byte2 of packet
  xor  ah, ah
  mov  BYTE [cs:XCOORD], al
ret


;***********************************************************************
;Get 3RD Mouse Byte
;***********************************************************************
GETTHIRD:
  call GETB       ;Get byte3 of packet
  xor  ah, ah
  mov  BYTE [cs:YCOORD], al
ret



;-----------------------------------------------------------------------
;***********************************************************************
;* MAIN PROGRAM
;***********************************************************************
;-----------------------------------------------------------------------


MAINP:
  call PS2SET
  call ACTMOUS
  call GETB    ;Get the responce byte of the mouse (like: Hey i am active)  If the bytes are mixed up, remove this line or add another of this line.

main:
  call GETFIRST
  call GETSECOND
  call GETTHIRD
   
;*NOW WE HAVE XCOORD & YCOORD* + the button status of L-butten and R-button and M-button allsow overflow + sign bits

;!!!
;! The Sign bit of X tells if the XCOORD is Negative or positive. (if 1 this means -256)
;! The XCOORD is allways positive
;!!!

;???
;? Like if:    X-Signbit = 1     Signbit
;?                |
;?             XCOORD = 11111110 ---> -256 + 254 = -2  (the mouse cursor goes left)
;?                      \      /
;?                       \    /
;?                        \Positive
;???


;?? FOR MORE INFORMATION ON THE PS/2 PROTOCOL SEE BELOW!!!!



;!!!!!!!!!!!!!
;the rest of the source... (like move cursor) (i leave this up to you m8!)
;!!!!!!!!!!!!!


;*************************************************************
;Allright, Allright i'll give an example!  |EXAMPLE CODE|
;*************************************************************
;=============================
;**Mark a position on scr**
;=============================
 
   xor   cx, cx
   xor   dx, dx
   
   mov   cl,byte [cs:XCOORD]
   mov   dl,byte [cs:YCOORD]
   
   cmp cl, 0
   je  NoxChange
   cmp cl, 0
   jg  Subx
   sub WORD [cs:xpos], 1
   jmp  NoxChange
   Subx:
   add WORD [cs:xpos], 1
   NoxChange:

   cmp dl, 0
   je  NoyChange
   cmp dl, 0
   jg  Addy
   add WORD [cs:ypos], 1
   jmp  NoyChange
   Addy:
   sub WORD [cs:ypos], 1
   NoyChange:


   mov   cx,word [cs:xpos]
   mov   dx,word [cs:ypos]

   Mov   AH,0Ch
   Mov   AL,15
   Mov   CX,CX
   Mov   DX,DX
   Int   10h
 
 mov BYTE [cs:row], 15
 mov BYTE [cs:col], 0

;=============================
;**go to start position**
;=============================
 call GOTOXY

;=============================
;**Lets display the X coord**
;=============================
 mov  si, strcdx  ; display the text for Xcoord
 call disp
 xor  ah, ah
 mov  al, BYTE [cs:XCOORD]
 call DISPDEC
 mov  si, stretr  ; goto nextline on scr
 call disp

;=============================
;**Lets display the Y coord**
;=============================
 mov  si, strcdy  ; display the text for Ycoord
 call disp
 xor  ah, ah
 mov  al, BYTE [cs:YCOORD]
 call DISPDEC
 mov  si, stretr  ; goto nextline on scr
 call disp

;=============================
;**Lets display the L button**
;=============================
 mov  si, strlbt  ; display the text for Lbutton
 call disp
 mov  al, BYTE [cs:LBUTTON]
 xor  ah, ah
 call DISPDEC
 mov  si, stretr  ; goto nextline on scr
 call disp

;=============================
;**Lets display the R button**
;=============================
 mov  si, strrbt  ; display the text for Rbutton
 call disp
 mov  al, BYTE [cs:RBUTTON]
 xor  ah, ah
 call DISPDEC
 mov  si, stretr  ; goto nextline on scr
 call disp
 
;=============================
;**Lets display the M button**
;=============================
 mov  si, strmbt  ; display the text for Mbutton
 call disp
 mov  al, BYTE [cs:MBUTTON]
 xor  ah, ah
 call DISPDEC
 mov  si, stretr  ; goto nextline on scr
 call disp
 
;=============================
;**Lets display sign of y
;=============================
 mov  si, signy  ; display the text for Mbutton
 call disp
 mov  al, BYTE [cs:YCOORDN]
 xor  ah, ah
 call DISPDEC
 mov  si, stretr  ; goto nextline on scr
 call disp

;=============================
;**stop program on keypress** |Note: sometimes it takes a while for the program stops, or keyboard stalls|
;=============================  |due to more time is spend looking at the PS/2 mouse port (keyb disabled)  |
    xor  ax, ax
    mov  ah, 0x11
    int  0x16
    jnz quitprog
;*************************************************************
 jmp main
 
quitprog:
 mov     ax, 0x0003
 int     0x10
 MOV     AH, 0x4C    ;Return to OS
 INT     0x21     
;-----------------------------------------------------------------------
;***********************************************************************
;* END OF MAIN PROGRAM
;***********************************************************************
;-----------------------------------------------------------------------


;XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
;X Dont Worry about this displaypart, its yust ripped of my os.
;X (I know it could be done nicer but this works Razz)
;XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
;XXX
;************************************************
;* Displays AX in a decimal way 
;************************************************
DISPDEC:
    mov  BYTE [cs:zerow], 0x00
    mov  WORD [cs:varbuff], ax
    xor  ax, ax
    xor  cx, cx
    xor  dx, dx
    mov  bx, 10000
    mov  WORD [cs:deel], bx
   mainl    :
    mov  bx, WORD [cs:deel]
    mov  ax, WORD [cs:varbuff]
    xor  dx, dx
    xor  cx, cx
    div  bx
    mov  WORD [cs:varbuff], dx
    jmp ydisp
   
   vdisp:
    cmp  BYTE [cs:zerow], 0x00
    je nodisp

   ydisp:
    mov  ah, 0x0E                            ; BIOS teletype
    add  al, 48                              ; lets make it a 0123456789 Very Happy
    mov  bx, 1 
    int  0x10                                ; invoke BIOS
    mov  BYTE [cs:zerow], 0x01
   jmp yydis

   nodisp:

   yydis:
    xor  dx, dx
    xor  cx, cx
    xor  bx, bx
    mov  ax, WORD [cs:deel]
    cmp  ax, 1
    je bver
    cmp  ax, 0
    je bver
    mov  bx, 10
    div  bx
    mov  WORD [cs:deel], ax
   jmp mainl

   bver:
   ret
;***************END of PROCEDURE*********************************
;****************************************************************
;* PROCEDURE disp      
;* display a string at ds:si via BIOS
;****************************************************************
disp:
 HEAD:
    lodsb                                    ; load next character
    cmp al, 0                                ; test for NUL character
    je   DONE                               ; if NUL char found then goto done
    mov  ah, 0x0E                            ; BIOS teletype
    mov  bx, 1                  ; make it a nice fluffy blue (mostly it will be grey but ok..)
    int  0x10                                ; invoke BIOS
    jmp  HEAD
 
 DONE: 
   ret
;*******************End Procedure ***********************
;*****************************
;*GOTOXY  go back to startpos
;*****************************
GOTOXY:
    mov ah, 2
    mov bh, 0                  ;0:graphic mode 0-3: in modes 2&3 0-7: in modes 0&1
    mov dl, BYTE [cs:col]
    mov dh, BYTE [cs:row]
    int 0x10
ret
;*******END********
;
;XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
;XXX
;XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


;***********************************************************************
;variables
;***********************************************************************
LBUTTON db 0x00   ;  Left   button status 1=PRESSED 0=RELEASED
RBUTTON db 0x00   ;  Right  button status 1=PRESSED 0=RELEASED
MBUTTON db 0x00   ;  Middle button status 1=PRESSED 0=RELEASED
XCOORD  db 0x00   ;  the moved distance  (horizontal)
YCOORD  db 0x00   ;  the moved distance  (vertical)
XCOORDN db 0x00 ;       Sign bit (positive/negative) of X Coord
YCOORDN db 0x00 ;       Sign bit (positive/negative) of Y Coord
XFLOW   db 0x00 ;       Overflow bit (Movement too fast) of X Coord
YFLOW   db 0x00 ;       Overflow bit (Movement too fast) of Y Coord


;************************************
;* Some var's of my display function
;************************************
deel    dw 0x0000
varbuff dw 0x0000
zerow   db 0x00
strlbt  db "Left button:   ", 0x00
strrbt  db "Right button:  ", 0x00
strmbt  db "Middle button: ", 0x00
strcdx  db "Mouse moved (X): ", 0x00
strcdy  db "Mouse moved (Y): ", 0x00
signy   db "Sign (y): " ,0x00
stretr  db 0x0D, 0x0A, 0x00
strneg  db "-", 0x00
strsp   db " ", 0x00
row     db 0x00
col     db 0x00
xpos    dw 0x0000
ypos    dw 0x0000


;***********************************************************************
; PS/2 mouse protocol (Standard PS/2 protocol)
;***********************************************************************
; ----------------------------------------------------------------------
;
; Data packet format: 
; Data packet is 3 byte packet. 
; It is send to the computer every time mouse state changes 
; (mouse moves or keys are pressed/released). 
; 
;   Bit7 Bit6 Bit5 Bit4 Bit3 Bit2 Bit1 Bit0 
; 
; 1. YO   XO   YS   XS   1    MB   RB   LB 
; 2. X7   X6   X5   X4   X3   X2   X1   X0
; 3. Y7   Y6   Y5   Y4   Y3   Y2   Y1   Y0
;
; This means:
; YO   :  Overflow bit for Y-coord (movement to fast)
; XO   :  Overflow bit for X-coord (movement to fast) 
; X0-X7:  byte of the x-coord 
; Y0-Y7:  byte of the y-coord
; LB   :  Left button pressed
; RB   :  Right button pressed
; MB   :  Middle button pressed
;
;*********************************************************************** 
;If you want to use the scroll function u might look up the protocol at 
;the company that made the mouse. 
;(the packet format will then mostly be greater then 3)
;***********************************************************************

