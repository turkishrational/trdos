��f��fSf9$u��  ��   �   �#   �@��^  ��   �   �#   �@���9�u��  ��   �   �#   �@�5��9�t��  ��   �   �#   �@���  ��   �   �#   �@f[f�5�  f�<$�u��  ��   �   �#   �@���  ��   �   �#   �@���9�u�  ��   �   �#   �@�5��9�t��  ��   �   �#   �@���  ��   �   �#   �@[���  u�2  ��   �   �#   �@�6S�>  ��   �   �#   �@f[f���u�l  ��   �   �#   �@���1�
�1�   �@���
OK ! (push bx : word [esp] = bx) 
OK ! (push bx : esp = esp - 4) 
OK ! (push word [numara] : word [esp] = 1234) 
OK ! (push word [numara] : esp = esp - 4) 
OK ! (pop ebx  : ebx = 1234 = [numara])
 
  Error ! (push bx : word [esp] = bx) 
  Error ! (push bx : esp = esp - 4) : (( esp = esp - 2 )) 
  Error ! (push word [numara] : word [esp] = 1234) 
  Error ! (push word [numara] : esp = esp - 4) : (( esp = esp - 2 )) 
  Error ! (pop ebx : ebx = 1234 = [numara]) 
OK ! (pop bx : bx = 1234 = [numara])
 
  ERROR ! 