��  �-   �   �#   �@�	  ��   �   �#   �@�   �    �.   �@�   �    �.   �@�;	  ��   �   �#   �@�v	  ��   �   �#   �@�1
  ��   �   �#   �@�  ��   �   �#   �@�   �.   �@<tq��;r��Dw���;����L  ���  ��   �   �#   �@��  ��   �   �#   �@�   �.   �@<t�=����  ��   �   �#   �@��  �   �   �#   �@�    �   �@���t  �  R  8    �  >    �  }  �   �    �.   �@�   �.   �@��  ��   �   �#   �@�R  ��   �   �#   �@)ɵ�   �.   �@���   �.   �@��u�û   �    �.   �@�   �.   �@�%  ��   �   �#   �@�R  ��   �   �#   �@)ɵ�   �.   �@!�t����   �.   �@��u�û�  ��   �   �#   �@�  ��   �   �#   �@�F  ��   �   �#   �@��  �    �   �@s��  ��   �   �#   �@�@�  �    �   �@��  ��   �   �#   �@��  ��   �   �#   �@�   �  �.   �@�   �.   �@<tF<u��  ��   �   �#   �@�  ��   ��   ���.   �@��   �.   �@Ou��j�  ��   �   �#   �@�   �.   �@< r�   ���.   �@��   ���.   �@<t<uʰ�   ���.   �@�
�   �.   �@�  K�   �@�   �   �@�   �    �.   �@��  ��   �   �#   �@û�  ��   �   �#   �@��  ��   �   �#   �@��  ��   �#	  �(   �@r0�z vA��  �    �   �@sE��  ��   �   �#   �@û�  ��   �   �#   �@û�  ��   �   �#   �@�@�  �    �   �@�   �  �.   �@�    �.   �@r��   ���.   �@���  K�   �@�   �   �@�   �    �.   �@�  ��   �   �#   �@û�	  ��   �   �#   �@�/  ��t�   ���.   �@���I  u��  �ܻI  �   �   �#   �@�   �.   �@<t����   �.   �@��u��l����R  ��   �   �#   �@�s  ��   �   �#   �@������  ��   �   �#   �@��  ��   �   �#   �@��  �   �.   �@<tQ<t<0r�<9w�0�,0ƻI  �   �   �#   �@�   �.   �@!�t�<t���.�   �.   �@��u������G  ��   �   �#   �@��  ��   �   �#   �@��  ��   �   �#   �@1�1ҵ�    �.   �@<t1	�u�ƈ��   �.   �@��tB��uω�	   �.   �@��������
  ��   �   �#   �@��  ��   �   �#   �@�   �1�`  ��   �   �#   �@��  ��   �   �#   �@�   ���.   �@��t!�t��   P��  ��   �   �#   �@��  ��   �   �#   �@X��  ��   PP��  ��   �   �#   �@��  ��   �   �#   �@X�Ļ  �   �  ��   �   �#   �@�  ��   �   �#   �@X<t�@����������   ���.   �@ù    1�< r<t*����  �F<tF<tF<
tF<	t
F<tF<u����Ƥ  ���P0�����  �⊢�  f�X�DEL ESC CR  LF  TAB BS  BEEP0123456789ABCDEF	

TRDOS 386 v2.0.9 - SYSSTDIO Function Test
 Erdogan Tan - 19/09/2024
 ������������������������
Press one of F1-F10 keys to test or press ESC to exit.
 
F1 - read a character on stdin (wait)
F2 - read a character on stdin (no wait)
F3 - write a character onto stdout (redirection)
F4 - write a character onto stderr (no redirection)
 F5 - redirect stdin to file (if cl > 0)
F6 - redirect stdout to file (if cl > 0)
F7 - read character (ascii & scancode) on stdin (no redir, wait)
F8 - read character (ascii & scancode) on stdin (no redir, no wait)
 F9 - write character and color onto stdout (no redirection)
F10 - ungetchar (put back the ascii code in u.getc)

 
Select INPUT option to STDOUT: 
ESC - Sample Text
ENTER - Keyboad (STDIN)
 
written ...
 
Sample Text for Redirection Test:
 This is a text for TRDOS 386 v2.0.9 SYSSTDIO system call,
STDIN/STDOUT redirection test.

I run each teen me? Each team. Catch bar duck each teen?
On bar duck each team. Why high one why!
 
Press a key to continue or press ESC to exit.
 
F1 - read a character on stdin (wait)
 
F2 - read a character on stdin (no wait)
 
Keyboard is set as STDIN.
Console Screen is set as STDOUT.
Pressed keys will be sent to STDOUT.
(ENTER a character string to display.)
 
F3 - write a character onto stdout (redirection)
 
STDOUT will be redirected to STDIO.TXT file.
 
At First, the STDIO.TXT file will be opened (from scratch).
 STDIO.TXT 
SYSCREAT - File creating/open error !
 
SYSOPEN - File open error !
 STDIO.TXT File Size is zero !
 STDIO.TXT not found!
 
Pressed keys will be written into STDOUT file.
Written characters will be echoed to STDERR/screen.
Then... Press ENTER to stop and close the file.
CRLF will be added to the file instead of ENTER/CR.
 
F5 - redirect stdin to file
 
STDIN will be redirected to STDIO.TXT file.
 
This is STDERR message. String:  
F6 - redirect stdout to file
 
STDOUT will be redirected to STDIO.TXT file.
 
F9 - write character and color onto stdout
 
Select a color:
       1 = BLUE   
       2 = GREEN  
       3 = CYAN   
       4 = RED    
       5 = MAGENTA
       6 = YELLOW 
       7 = WHITE  
       8 = BROWN  
       9 = GRAY   
       0 = Default
 
ENTER a string to test ungetchar feature...
 
The 1st char will be written as 5th char without any keypress.
(a getchar just after ungetchar works as a virtual keypress)
 
F7 - read character (ascii & scancode) on stdin (no redir, wait)
 
Press any keys to continue or press ESC to cancel.
 
Character:        Ascii Code:  00h   Scan Code:  00h      
OK.  