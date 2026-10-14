       IDENTIFICATION DIVISION.

       PROGRAM-ID.    ProgName [IS COMMON|INITIAL].
      *AUTHOR.        MICHAEL S. MELL.
      *INSTALLATION.  Odyssey.
      *DATE-WRITTEN.  .
      *DATE-COMPILED. .
      *SECURITY.      [UNCLAS|FOUO|CONFIDENTIAL|SECRET|TOP SECRET].

      ******************************************************************
      * Columns 1-6     Sequence number area (ppplll)                  *
      * Column 7        Indicator area ("*" = comment,- "=" cont.,     *
      *                   "/" = print stopper, "D" = debug)            *
      * Columns 8-11    Area a                                         *
      * Columns 12-72   Area b                                         *
      * Columns 73-80   reserved (system generated number)             *
      ******************************************************************
      /
      *ENVIRONMENT DIVISION.

      *CONFIGURATION SECTION.

      *SOURCE-COMPUTER. source-computer-name
      *OBJECT-COMPUTER. object-computer-name
      *SPECIAL-NAMES. special-names-entries

      *INPUT-OUTPUT SECTION.
      *FILE-CONTROL. file-control-entries
      *I-O-CONTROL. i-o-control-entries
      /
      *DATA DIVISION.
      *    FILE SECTION.
      *        FD|SD file-description-entry...
      *        data-description-entry...
      *    WORKING-STORAGE SECTION.
      *        data-description-entry...
      *    LINKAGE SECTION.
      *        data-description-entry...
      /
       PROCEDURE DIVISION.
      *section-name SECTION.
      *paragraph-name.
      *    code, code, code.
           STOP RUN
