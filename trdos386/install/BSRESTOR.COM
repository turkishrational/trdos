�� ��t!�< t�r<hu�<duF�<0tr	<3w�< t�g��FP�e�< t�r�� ��< v���,r�< wB(��� ����<as �t<.u�����<zw�$ߪ��O9�s)߁� ��v!�u���v��� �? �N�!s���t�� �$u�� ��	�t!�u= t�f= t�l�H� �>� � �e��s0��Ou�,�%�><	U�t�v��>fr��>�>fs��5���D<rt�<t�<t��<	s����<w�t�<t�<u��ȾC�� �.�� =� �!r�Ë� �?�>�!��>�!�s�� �><	U�t��� �.���D:Eu�>	���U�t���x�E�U
;Du�;T
u̾�� 1��<yt<Yt<nt<Nt<tB<u侸�A���s ���m �E�>	�6 r�>fu ���� �>�f����E �U�������5 � L�!��� `j j RPSjj��e0��C�sOt
0���D��aaì �t��� ���  
Primary DOS Partition Boot Sector Restore Utility for TR-DOS 386
v1.0.101020 (c) Erdogan TAN 2020

Usage: bsrestor <disk drive name> <backup file name>

Disk drive names: 

 hd0 ..for primary dos partition on 1st disk 
 hd1 ..for primary dos partition on 2nd disk 
 hd2 ..for primary dos partition on 3rd disk 
 hd3 ..for primary dos partition on 4th disk 

Example: bsrestor hd0 bsbackup.bin 
 
Invalid file name !
(File name must fit to 8.3 DOS format) !
 
MBR does not contain a primary DOS partition !
 
Invalid MBR !
 
Do you want to overwrite boot sector ?  YES
 NO
 
Writing primary dos partition's boot sector ...   OK.
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
(Improper/Different/Invalid MBR) !
 
Invalid backup file !
(Invalid BS) !
 