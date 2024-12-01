       IDENTIFICATION DIVISION.

       PROGRAM-ID.    HELLO.
       AUTHOR.        MMELL.
       INSTALLATION.  AUBURN.
       DATE-WRITTEN.  NOV 29,2024.
       DATE-COMPILED. NOV 29,2024.
       SECURITY.      UNCLASSIFIED.

      ***************************************************************** 
      *                                                               * 
      * THIS IS THE CLASSIC HELLO WORLD APPLICATION.                  * 
      *                                                               * 
      ***************************************************************** 

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       SOURCE-COMPUTER. X86_64.
       OBJECT-COMPUTER. X86_64.

       DATA DIVISION.

       FILE SECTION.

       WORKING-STORAGE SECTION.

       PROCEDURE DIVISION.
           DISPLAY 'HELLO WORLD!'.
           STOP RUN.

