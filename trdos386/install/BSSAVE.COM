�� ��t!�< t�r<hu�<duF�<0tr	<3w�< t	�5���pFP�3�< t�r�6��< v���Br�< wB(���6����<as �t<.u�����<zw�$ߪ��O9�s)߁�6��v!�u���v	�����6�? �N�!ra�� �$u=�� ��	�tN�S�a�6�[�p�U1��<yt(<Yt$<nt<Nt<�� <u�� ���� �~�(� �z����� �D� � �3��s0��Ou��� �>BU�t�@� ��D<rt<t<t'��Bs�����4�<w�t<t<u����r�D�T
�D�u s���_�>4u���� �D
�4�߾��C1ɺ6� <�!rA�����j �˹ �D�@�!r�>4 v� �D
�@�!r����>�!�s���8 � L�!������ `j j RPSjj��3�B�sOt
0���D��aaì �t��� ���  
Primary DOS Partition Boot Sector Backup Utility for TR-DOS 386
v1.0.101020 (c) Erdogan TAN 2020

Usage: bssave <disk drive name> <backup file name>

Disk drive names: 

 hd0 ..for primary dos partition on 1st disk 
 hd1 ..for primary dos partition on 2nd disk 
 hd2 ..for primary dos partition on 3rd disk 
 hd3 ..for primary dos partition on 4th disk 

Example: bssave hd0 bsbackup.bin 
 
Invalid file name !
(File name must fit to 8.3 DOS format) !
 
MBR does not contain a primary DOS partition ! 
 
Invalid MBR ! 
 
Do you want to overwrite ' ' file ?  YES NO 
Writing file ...   OK.
 
File creating error !
 
File writing error !
 
Disk read error or drive not ready ! 
 
Invalid backup file name !
(Improper file attributes) !
 