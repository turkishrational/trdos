*** This is the directory of "386 DOS v1.0" (and "TRDOS 386 v2.1") project files. Development stage. ***

What are being changed:

* (FAT file system) disk buffering methods are being changed to MSDOS (PCDOS 7.1, Retro DOS v5) style buffering.
    So, TRDOS v1.0 style buffering (FAT buffer -3 sectors-, Directory Buffer, Sector Buffer) will not be
    used after version 2.0.10 (version 2.0.10 is the last update of TRDOS 386 version 2.0) 
   ((Even if it is written as v2.0.10 modifications on the source code pages.. 
     there are two variants of 2.0.10, one of them is v2.1 pre-work.))
   Version number will be declared as 2.1 after adding new system calls to the kernel.
   Possible new system calls: 'syscountry', 'sysdiskio' or 'syssync'.

What will be different for 386 DOS v1.0 than TRDOS 386 v2.1:
   * 386 DOS v1.0 kernel will be used with external command interpreter (as similar to a simple command.com)
    -imagine that, it will run as a 32 bit protected mode command.com (as simplified)-
    --possible name of it will be 'main.prg'-- (Possible Alternative: unix type shell, 'shell.prg')
   * TRDOS 386 v2.1 will continue to use internal command interpreter ('mainprog' section of the kernel).
   * Both will have internationalization support (like as msdos internationalization method and content).
   * Keyboard map, character font and date-time-currency display format will be depended on configuration.
     But internationalization will be done via 'syscountry' system call before multi tasking is enabled.
     (If multi tasking is enabled via 'sysemt' system call, modification will be prohibited except process 1.)

Erdogan Tan - 29/04/2025
