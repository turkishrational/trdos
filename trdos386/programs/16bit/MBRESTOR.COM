�� ��tI�< t�rB�"a<-u��"c<ct	<tu+�"t�< u!�< t�r<hu�<duF�<0tr	<3w�< t�b�2FP�`�< t�r��	��< v��� 	r�< wB(���	����<as �t<.u�����<zw�$ߪ��O9�s)߁�	��v!�u���v�<���	�? �N�!s�-��� �$u߾� ��	�t!�u= t= t= t���� �"� � �`��s0��Ou�b�g�> U�u���L� �t�"e��D�t
:tItC���� s���Ҁ>"et�"�"	� � =�	�!r�ù �?�"�!��>�!�s�I�� �> U�t���� ���L� �t�~�� �D�t
:tItC���� s���Ӏ>"ct)�>"et"��
�� r�a���� s�>ar�� ��� 1��<yt<Yt<nt<Nt<td<uh� �c��� �� �"<at <et<cu��
���  ���"	�"��� �"� � �`��s0��Ou�*�E ����*��&�5 � L�!��� `j j RPSjj��`0��B�sOt
0���D��aaì �t��� ������0�D�#&<t3<t/<t+<t'�#B<t<t�# �<�t<t<t��9�s�ËD�T
�"�t�r*�> U�u!�#�t0�À?)u�>�@;Du;T
t��cqr�����   
MasterBoot Record/Sector Restore Utility for TR-DOS 386
v1.0.111020 (c) Erdogan TAN 2020

Usage: 
 mbrestor [option] <hard disk name> <backup file name>

Hard disk names: 

 hd0 ..for MBR of 1st hard disk 
 hd1 ..for MBR of 2nd hard disk 
 hd2 ..for MBR of 3rd hard disk 
 hd3 ..for MBR of 4th hard disk 

Options: 
 -c : restore MBR code only 
 -t : restore partition table only 

Examples:
 mbrestor -c hd0 mbsector.bin 
 mbrestor hd1 mbsector.bin 
 
Invalid file name !
(File name must fit to 8.3 DOS format) !
 
Invalid backup file !
(MBR does not contain a known partition type) ! 
 
Do you want to overwrite masterboot sector ?  YES
 NO
 
Writing masterboot sector ...   OK.
 
Backup file not found !
 
File reading error !
 
Disk read error or drive not ready !
 
Disk write error !
 
Invalid backup file !
(Improper file attributes or wrong file size) !
 
Invalid backup file !
(Invalid MBR) !
 
MASTERBOOT SECTOR WRITING PERMISSION DENIED ! Because ... 
Start sector of present active (or primary DOS) partition 
has valid boot sector signature; but start sector of the new 
active (or primary DOS) partition has not a valid boot sector ! 
 