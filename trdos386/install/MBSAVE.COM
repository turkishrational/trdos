�� ��t!�< t�r<hu�<duF�<0tr	<3w�< t	���w�/FP���< t�r꿌��< v����r�< wB(��������<as �t<.u�����<zw�$ߪ��O9�s)߁���v!�u���v	�!�
�� ���? �N�!r]�� �$u:�� ��	�tJ���� ���� ���� 1��<yt$<Yt <nt<Nt<ty<u��s�O� ��� �e��� ��� � ��� � ����s0��Ou�%�l�>�U�t���_�X��� �D�t
:tItC�����s���޾c�5���01ɺ�� <�!r.�����, �˹ ���@�!��>�!�s����� � L�!������ì �t��� ���cqr�����  
MasterBoot Record/Sector Backup Utility for TR-DOS 386
v1.0.111020 (c) Erdogan TAN 2020

Usage: mbsave <disk drive name> <backup file name>

Disk drive names: 

 hd0 ..for MBR of 1st hard disk 
 hd1 ..for MBR of 2nd hard disk 
 hd2 ..for MBR of 3rd hard disk 
 hd3 ..for MBR of 4th hard disk 

Example: mbsave hd0 mbsector.bin 
 
Invalid file name !
(File name must fit to 8.3 DOS format) !
 
MBR does not contain a known partition type ! 
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