�歃�rp�­�� �  � uc<-u_��et��dt��uP<-uL��ste��ruB��   ���a�	�   �@�t$��  ��t�k  ���   �   �#   �@��{  ��H  �   ���t�1���   �@����%����t�f=VGt6f=SVu������f=GAu��{ u��  �   �@	�u+�  �   �@���$�<Au� �u��  �   �@	�t!�  댭�ƹ	   ���   sF<htD< u&�C�{  ��   �   �#   �@��  �#   �@�{��  ��   �   �#   �@�cⰬ< u�	  �   �@<uU���s����tG��r����t9��������  �   �@	�t�s  ��   �   �#   �@�   �@���  ��   �   �#   �@��<0wt�<9v��߀�Ar ��Fw��,7�,0��  ���  ��ù   �¿g  ����$<	v7�0���V  ��   �   �#   �@�
TRDOS 386 v2.0.3 - Save/Restore Video State ('sysvideo') Test Program
by Erdogan Tan - 25/01/2021

Usage:

  srvs -s VGA    .. for saving standard VGA state to system buffer
  srvs -s SVGA   .. for saving super VGA (VBE3) state to system buffer
                    Return: EAX = Video State ID
  srvs -r <Video State ID> .. for restoring video state from system buffer

  srvs -e        .. for enabling 'save/restore video state' function
  srvs -d        .. for disabling 'save/restore video state' function

 
VideoStateID : XXXXXXXXh
 
OK.
 
Error ! (Permission denied!)
 
(Video State Save Permission must be enabled!)
((run this program as 'srvs -e'))
 
Error ! (Wrong Video State ID!)
 
Error !

(Written Video State ID is not correct or restore permission denied!)
 
SRVS function is DISABLED.
 
SRVS function is ENABLED.
     