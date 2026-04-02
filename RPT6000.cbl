       IDENTIFICATION DIVISION.                                         00010000
                                                                        00020000
       PROGRAM-ID. RPT6000.                                             00030001
                                                                        00040000
      *   Programmers.: Violet French, Hayden Schmidt                   00050002
      *   Date........: 2026.04.02                                      00060002
      *   Github URL..: https://github.com/Pirategirl9000/RPT6000       00070001
      *   Description.: This program produces a sales report based on   00080000
      *   values acquired from the CUSTMAST dataset and produces        00090000
      *   subtotals and grandtotals for the different branches and      00100000
      *   sales representatives                                         00110000
       ENVIRONMENT DIVISION.                                            00120000
                                                                        00130000
       INPUT-OUTPUT SECTION.                                            00140000
                                                                        00150000
       FILE-CONTROL.                                                    00160000
           SELECT CUSTMAST ASSIGN TO CUSTMAST.                          00170000
           SELECT ORPT6000 ASSIGN TO RPT6000.                           00180001
                                                                        00190000
       DATA DIVISION.                                                   00200000
                                                                        00210000
       FILE SECTION.                                                    00220000
                                                                        00230000
      **************************************************************    00240000
      * INPUT FILE                                                 *    00250000
      **************************************************************    00260000
       FD  CUSTMAST                                                     00270000
           RECORDING MODE IS F                                          00280000
           LABEL RECORDS ARE STANDARD                                   00290000
           RECORD CONTAINS 130 CHARACTERS                               00300000
           BLOCK CONTAINS 130 CHARACTERS.                               00310000
       01  CUSTOMER-MASTER-RECORD.                                      00320000
           05  CM-BRANCH-NUMBER        PIC 9(2).                        00330000
           05  CM-SALESREP-NUMBER      PIC 9(2).                        00340000
           05  CM-CUSTOMER-NUMBER      PIC 9(5).                        00350000
           05  CM-CUSTOMER-NAME        PIC X(20).                       00360000
           05  CM-SALES-THIS-YTD       PIC S9(5)V9(2).                  00370000
           05  CM-SALES-LAST-YTD       PIC S9(5)V9(2).                  00380000
           05  FILLER                  PIC X(87).                       00390000
                                                                        00400000
      **************************************************************    00410000
      * OUTPUT FILE                                                *    00420000
      **************************************************************    00430000
       FD  ORPT6000                                                     00440001
           RECORDING MODE IS F                                          00450000
           LABEL RECORDS ARE STANDARD                                   00460000
           RECORD CONTAINS 130 CHARACTERS                               00470000
           BLOCK CONTAINS 130 CHARACTERS.                               00480000
       01  PRINT-AREA      PIC X(130).                                  00490000
                                                                        00500000
       WORKING-STORAGE SECTION.                                         00510000
                                                                        00520000
      *------------------------------------------------------------*    00530000
      *                        WORKING FIELDS                      *    00540000
      *============================================================*    00550000
      *     THE FOLLOWING RECORDS ARE USED FOR WORKING WITH DATA   *    00560000
      *              AND ARE NOT USED FOR PROGRAM OUTPUT           *    00570000
      *------------------------------------------------------------*    00580000
       01 SALESREP-TABLE VALUE "WHATEVER12 11JSMITH   12TTHOMAS 14 BJ   00590007
      -    "ONES   18GFRANKLIN 21RWILLIAMS ".                           00600007
           05 SALESREP-GROUP OCCURS 6 TIMES                             00610007
                             INDEXED BY SRT-INDEX.                      00620007
               10 SALESREP-NUMBER   PIC 9(2).                           00630007
               10 SALESREP-MAME     PIC X(10).                          00640000
      **************************************************************    00650000
      * SWITCHES FOR END OF FILE AND FIRST RECORD                  *    00660000
      **************************************************************    00670000
       01  SWITCHES.                                                    00680000
           05  CUSTMAST-EOF-SWITCH     PIC X    VALUE "N".              00690000
               88  CUSTMAST-EOF                 VALUE "Y".              00700000
           05  FIRST-RECORD-SWITCH     PIC X    VALUE "Y".              00710000
               88  FIRST-RECORD                 VALUE "Y"               00720000
                   WHEN FALSE IS                      "N".              00730000
                                                                        00740000
      **************************************************************    00750000
      * SWITCH FOR END OF FILE                                     *    00760000
      **************************************************************    00770000
       01  CONTROL-FIELDS PACKED-DECIMAL.                               00780000
           05  OLD-BRANCH-NUMBER       PIC 99.                          00790000
           05  OLD-SALESREP-NUMBER     PIC 99.                          00800000
                                                                        00810000
      **************************************************************    00820000
      * STORES INFORMATION RELEVANT TO THE PAGE                    *    00830000
      **************************************************************    00840000
       01  PRINT-FIELDS PACKED-DECIMAL.                                 00850000
           05  PAGE-COUNT      PIC S9(3)   VALUE ZERO.                  00860000
           05  LINES-ON-PAGE   PIC S9(3)   VALUE +55.                   00870000
           05  LINE-COUNT      PIC S9(3)   VALUE +99.                   00880000
                                                                        00890000
      **************************************************************    00900000
      * STORES TOTAL FIELDS FOR CALCULATING                        *    00910000
      **************************************************************    00920000
       01  TOTAL-FIELDS PACKED-DECIMAL.                                 00930000
           05  BRANCH-TOTAL-THIS-YTD    PIC S9(6)V99   VALUE ZERO.      00940000
           05  BRANCH-TOTAL-LAST-YTD    PIC S9(6)V99   VALUE ZERO.      00950000
           05  SALESREP-TOTAL-THIS-YTD  PIC S9(6)V99   VALUE ZERO.      00960000
           05  SALESREP-TOTAL-LAST-YTD  PIC S9(6)V99   VALUE ZERO.      00970000
           05  GRAND-TOTAL-THIS-YTD     PIC S9(7)V99   VALUE ZERO.      00980000
           05  GRAND-TOTAL-LAST-YTD     PIC S9(7)V99   VALUE ZERO.      00990000
                                                                        01000000
      **************************************************************    01010000
      * USED TO PULL IN THE CURRENT-DATE-TIME VIA THE FUNCTION     *    01020000
      * CURRENT-DATE-AND-TIME WHICH WILL BE USED IN HEADER LINES   *    01030000
      **************************************************************    01040000
       01  CURRENT-DATE-AND-TIME.                                       01050000
           05  CD-YEAR         PIC 9999.                                01060000
           05  CD-MONTH        PIC 99.                                  01070000
           05  CD-DAY          PIC 99.                                  01080000
           05  CD-HOURS        PIC 99.                                  01090000
           05  CD-MINUTES      PIC 99.                                  01100000
           05  FILLER          PIC X(9).                                01110000
                                                                        01120000
      **************************************************************    01130000
      * STORES VALUES USED FOR CALCULATIONS                       *     01140000
      **************************************************************    01150000
       01  CALCULATED-FIELDS.                                           01160000
           05 CHANGE-AMOUNT    PIC S9(5)V99.                            01170000
                                                                        01180000
      *------------------------------------------------------------*    01190000
      *                       OUTPUT FIELDS                        *    01200000
      *============================================================*    01210000
      *     THE FOLLOWING RECORDS ARE USED FOR PRINTING DATA TO    *    01220000
      *                      THE OUTPUT FILE                       *    01230000
      *------------------------------------------------------------*    01240000
                                                                        01250000
      **************************************************************    01260000
      * STORES THE FIRST HEADER LINE INFORMATION                   *    01270000
      * HOLDS THE DATE, REPORT TITLE, AND PAGE NUMBER              *    01280000
      **************************************************************    01290000
       01  HEADING-LINE-1.                                              01300000
           05  FILLER          PIC X(7)    VALUE "DATE:  ".             01310000
           05  HL1-MONTH       PIC 9(2).                                01320000
           05  FILLER          PIC X(1)    VALUE "/".                   01330000
           05  HL1-DAY         PIC 9(2).                                01340000
           05  FILLER          PIC X(1)    VALUE "/".                   01350000
           05  HL1-YEAR        PIC 9(4).                                01360000
           05  FILLER          PIC X(16)   VALUE SPACE.                 01370000
           05  FILLER          PIC X(20)   VALUE "YEAR-TO-DATE SALES R".01380000
           05  FILLER          PIC X(10)   VALUE "EPORT     ".          01390000
           05  FILLER          PIC X(19)   VALUE SPACE.                 01400000
           05  FILLER          PIC X(8)    VALUE "  PAGE: ".            01410000
           05  HL1-PAGE-NUMBER PIC ZZZ9.                                01420000
           05  FILLER          PIC X(39)   VALUE SPACE.                 01430000
                                                                        01440000
      **************************************************************    01450000
      * STORES THE SECOND HEADER LINE INFORMATION                  *    01460000
      * HOLDS THE TIME AND THE PROGRAM ID                          *    01470000
      **************************************************************    01480000
       01  HEADING-LINE-2.                                              01490000
           05  FILLER          PIC X(7)    VALUE "TIME:  ".             01500000
           05  HL2-HOURS       PIC 9(2).                                01510000
           05  FILLER          PIC X(1)    VALUE ":".                   01520000
           05  HL2-MINUTES     PIC 9(2).                                01530000
           05  FILLER          PIC X(72)   VALUE SPACE.                 01540000
           05  FILLER          PIC X(10)   VALUE "RPT6000".             01550001
           05  FILLER          PIC X(39)   VALUE SPACE.                 01560000
                                                                        01570000
      **************************************************************    01580000
      * STORES THE THIRD HEADER LINE USED TO DISPLAY A LINE SPACER *    01590000
      **************************************************************    01600000
       01  HEADING-LINE-3.                                              01610000
           05 FILLER               PIC X(130)   VALUE SPACE.            01620000
                                                                        01630000
      **************************************************************    01640000
      * STORES THE FOURTH HEADER LINE INFORMATION                  *    01650000
      * HOLDS THE DIFFERENT COLUMN NAMES - SOME ARE SPLIT ACROSS   *    01660000
      * THE NEXT HEADER LINE                                       *    01670000
      **************************************************************    01680000
       01  HEADING-LINE-4.                                              01690000
           05  FILLER      PIC X(7)    VALUE "BRANCH ".                 01700000
           05  FILLER      PIC X(6)    VALUE "SALES ".                  01710000
           05  FILLER      PIC X(20)   VALUE "CUST                ".    01720000
           05  FILLER      PIC X(20)   VALUE "            SALES   ".    01730000
           05  FILLER      PIC X(20)   VALUE "      SALES         ".    01740000
           05  FILLER      PIC X(20)   VALUE "CHANGE     CHANGE   ".    01750000
           05  FILLER      PIC X(44)   VALUE SPACE.                     01760000
                                                                        01770000
      **************************************************************    01780000
      * STORES THE FIFTH HEADER LINE INFORMATION                   *    01790000
      * HOLDS SOME OF THE COLUMN NAMES AS WELL AS THE OTHER HALF   *    01800000
      * OF COLUMN NAMES THAT STARTED IN THE LAST HEADER LINE       *    01810000
      **************************************************************    01820000
       01  HEADING-LINE-5.                                              01830000
           05  FILLER      PIC X(8)    VALUE " NUM    ".                01840000
           05  FILLER      PIC X(5)    VALUE "REP  ".                   01850000
           05  FILLER      PIC X(20)   VALUE "NUM    CUSTOMER NAME".    01860000
           05  FILLER      PIC X(20)   VALUE "           THIS YTD ".    01870000
           05  FILLER      PIC X(20)   VALUE "     LAST YTD       ".    01880000
           05  FILLER      PIC X(20)   VALUE "AMOUNT    PERCENT   ".    01890000
           05  FILLER      PIC X(44)   VALUE SPACE.                     01900000
                                                                        01910000
      **************************************************************    01920000
      * STORES THE SIXTH HEADER LINE WHICH IS USED FOR SPACING     *    01930000
      **************************************************************    01940000
       01  HEADING-LINE-6.                                              01950000
           05  FILLER      PIC X(130)  VALUE SPACES.                    01960000
                                                                        01970000
      **************************************************************    01980000
      * STORES INFORMATION ABOUT CURRENT CUSTOMER                  *    01990000
      * HOLDS THE BRANCH NUMBER, SALES REP NUMBER, CUSTOMER NUMBER,*    02000000
      * CUSTOMER NAME, SALES THIS AND LAST YEAR-TO-DATE,           *    02010000
      * DIFFERENCE BETWEEN THIS YEARS SALES AND LAST, AND THE      *    02020000
      * DIFFERENCE IN PERCENT.                                     *    02030000
      **************************************************************    02040000
       01  CUSTOMER-LINE.                                               02050000
           05  FILLER              PIC X(2)     VALUE SPACE.            02060000
           05  CL-BRANCH-NUMBER    PIC X(2).                            02070000
           05  FILLER              PIC X(4)     VALUE SPACE.            02080000
           05  CL-SALESREP-NUMBER  PIC X(2).                            02090000
           05  FILLER              PIC X(3)     VALUE SPACE.            02100000
           05  CL-CUSTOMER-NUMBER  PIC 9(5).                            02110000
           05  FILLER              PIC X(2)     VALUE SPACE.            02120000
           05  CL-SALESREP-NAME    PIC X(10).                           02121008
           05  FILLER              PIC X(2).    VALUE SPACE.            02122008
           05  CL-CUSTOMER-NAME    PIC X(20).                           02130000
           05  FILLER              PIC X(3)     VALUE SPACE.            02140000
           05  CL-SALES-THIS-YTD   PIC ZZ,ZZ9.99-.                      02150000
           05  FILLER              PIC X(4)     VALUE SPACE.            02160000
           05  CL-SALES-LAST-YTD   PIC ZZ,ZZ9.99-.                      02170000
           05  FILLER              PIC X(4)     VALUE SPACE.            02180000
           05  CL-CHANGE-AMOUNT    PIC ZZ,ZZ9.99-.                      02190000
           05  FILLER              PIC X(3)     VALUE SPACE.            02200000
           05  CL-CHANGE-PERCENT   PIC ---9.9.                          02210003
           05  CL-CHANGE-PERCENT-R REDEFINES CL-CHANGE-PERCENT          02220003
                                   PIC X(6).                            02230003
           05  FILLER              PIC X(47)    VALUE SPACE.            02240000
                                                                        02250000
      **************************************************************    02260000
      * STORES THE BRANCH TOTAL LINE                               *    02270000
      * HOLDS THE TOTALS FOR THIS AND LAST YEAR-TO-DATE IN SALES   *    02280000
      * FOR THIS BRANCH AS WELL AS THE PERCENT DIFFERENCE          *    02290000
      * USED FOR OUTPUTTING                                        *    02300000
      **************************************************************    02310000
       01  BRANCH-TOTAL-LINE.                                           02320000
           05  FILLER                PIC X(28)    VALUE SPACE.          02330004
           05  FILLER                PIC X(14)    VALUE "BRANCH TOTAL". 02340004
           05  BTL-SALES-THIS-YTD    PIC $$$,$$9.99-.                   02350005
           05  FILLER                PIC X(3)     VALUE SPACE.          02360004
           05  BTL-SALES-LAST-YTD    PIC $$$,$$9.99-.                   02370005
           05  FILLER                PIC X(3)     VALUE SPACE.          02380004
           05  BTL-CHANGE-AMOUNT     PIC $$$,$$9.99-.                   02390006
           05  FILLER                PIC X(3)     VALUE SPACE.          02400004
           05  BTL-CHANGE-PERCENT    PIC +++9.9.                        02410004
           05  BTL-CHANGE-PERCENT-R  REDEFINES BTL-CHANGE-PERCENT       02420004
                                     PIC X(6).                          02430004
           05  FILLER                PIC X(48)    VALUE " **".          02440004
                                                                        02450000
      **************************************************************    02460000
      * STORES THE SALES REP TOTAL LINE                            *    02470000
      * HOLDS THE TOTALS FOR THIS AND LAST YEAR-TO-DATE IN SALES   *    02480000
      * FOR THIS REP AS WELL AS THE PERCENT DIFFERENCE             *    02490000
      * USED FOR OUTPUTTING                                        *    02500000
      **************************************************************    02510000
       01  SALESREP-TOTAL-LINE.                                         02520000
           05  FILLER               PIC X(28)    VALUE SPACE.           02530005
           05  FILLER               PIC X(14)    VALUE "SALESREP TOTAL".02540005
           05  STL-SALES-THIS-YTD   PIC $$$,$$9.99-.                    02550005
           05  FILLER               PIC X(3)     VALUE SPACE.           02560005
           05  STL-SALES-LAST-YTD   PIC $$$,$$9.99-.                    02570005
           05  FILLER               PIC X(3)     VALUE SPACE.           02580005
           05  STL-CHANGE-AMOUNT    PIC $$$,$$9.99-.                    02590006
           05  FILLER               PIC X(3)     VALUE SPACE.           02600005
           05  STL-CHANGE-PERCENT   PIC ZZ9.9-.                         02610005
           05  STL-CHANGE-PERCENT-R REDEFINES STL-CHANGE-PERCENT        02620005
                                    PIC X(6).                           02630005
           05  FILLER               PIC X(48)    VALUE " *".            02640005
      **************************************************************    02650000
      * STORES THE SECOND GRAND TOTAL LINE                         *    02660000
      * HOLDS THE TOTAL SALES FOR THIS AND LAST YEAR-TO-DATE,      *    02670000
      * THE TOTAL DIFFERENCE IN SALES MADE BETWEEN THE TWO YEARS   *    02680000
      * AND THE PERCENTAGE DIFFERENCE - FOR OUTPUTTING             *    02690000
      **************************************************************    02700000
       01  GRAND-TOTAL-LINE.                                            02710000
           05  FILLER              PIC X(28)    VALUE SPACE.            02720000
           05  FILLER              PIC X(12)    VALUE "GRAND TOTAL ".   02730000
           05  GTL-SALES-THIS-YTD  PIC $,$$$,$$9.99-.                   02740006
           05  FILLER              PIC X(1)     VALUE SPACE.            02750000
           05  GTL-SALES-LAST-YTD  PIC $,$$$,$$9.99-.                   02760006
           05  FILLER              PIC X        VALUE SPACE.            02770000
           05  GTL-CHANGE-AMOUNT   PIC $,$$$,$$9.99-.                   02780006
           05  FILLER              PIC X(3)     VALUE SPACE.            02790000
           05  GTL-CHANGE-PERCENT  PIC ZZ9.9-.                          02800000
           05  GTL-CHANGE-PERCENT-R REDEFINES GTL-CHANGE-PERCENT        02810007
                                   PIC X(6).                            02820007
           05  FILLER              PIC X(43)    VALUE " ***".           02830000
                                                                        02840000
       PROCEDURE DIVISION.                                              02850000
                                                                        02860000
      **************************************************************    02870000
      * OPENS AND CLOSES THE FILES AND DELEGATES THE WORK FOR      *    02880000
      * READING AND WRITING TO AND FROM THEM                       *    02890000
      **************************************************************    02900000
       000-PREPARE-SALES-REPORT.                                        02910000
                                                                        02920000
           OPEN INPUT  CUSTMAST                                         02930000
                OUTPUT ORPT6000.                                        02940001
                                                                        02950000
           *> GRABS THE DATE AND TIME INFORMATION FOR                   02960000
           *> THE HEADER LINES                                          02970000
           PERFORM 100-FORMAT-REPORT-HEADING.                           02980000
                                                                        02990000
           *> GRAB AND PRINT CUSTOMER SALES TO THE OUPUT FILE UNTIL     03000000
           *> THE END OF THE INPUT FILE                                 03010000
           PERFORM 200-PREPARE-SALES-LINES                              03020000
               UNTIL CUSTMAST-EOF-SWITCH = "Y".                         03030000
                                                                        03040000
           *> OUTPUT THE GRAND TOTALS TO THE OUTPUT FILE                03050000
           PERFORM 300-PRINT-GRAND-TOTALS.                              03060000
                                                                        03070000
           CLOSE CUSTMAST                                               03080000
                 ORPT6000.                                              03090001
           STOP RUN.                                                    03100000
                                                                        03110000
      **************************************************************    03120000
      * FORMATS THE REPORT HEADER BY GRABBING THE DATE TIME AND    *    03130000
      * STORING IT IN THE RELEVENT HEADER DATA ITEMS               *    03140000
      **************************************************************    03150000
       100-FORMAT-REPORT-HEADING.                                       03160000
                                                                        03170000
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.         03180000
                                                                        03190000
           *> MOVE THE RESULT OF THE DATE-TIME FUNCTION TO THE          03200000
           *> DIFFERENT HEADER LINE FIELDS ASSOCIATED WITH THEM         03210000
           *> SO WE CAN INCLUDE THE DATE IN THE OUTPUT HEADER           03220000
           MOVE CD-MONTH   TO HL1-MONTH.                                03230000
           MOVE CD-DAY     TO HL1-DAY.                                  03240000
           MOVE CD-YEAR    TO HL1-YEAR.                                 03250000
           MOVE CD-HOURS   TO HL2-HOURS.                                03260000
           MOVE CD-MINUTES TO HL2-MINUTES.                              03270000
                                                                        03280000
      **************************************************************    03290000
      * CALLS THE PARAGRAPH TO READ A LINE OF THE CUSTOMER RECORD  *    03300000
      * THEN CALLS THE PARAGRAPH TO PRINT THE LINE IF ITS NOT THE  *    03310000
      * TERMINATING LINE OF THE FILE                               *    03320000
      **************************************************************    03330000
       200-PREPARE-SALES-LINES.                                         03340000
                                                                        03350000
           *> GRAB THE NEXT LINE FROM THE CUSTOMER RECORD               03360000
           PERFORM 210-READ-CUSTOMER-RECORD.                            03370000
                                                                        03380000
           *> PERFORMS DUTIES BASED ON THE ENTRY                        03390000
           *>  * IF WE RUN OUT OF DATA PRINT THE SALES AND BRANCH TOTALS03400000
           *>  * IF IT'S THE FIRST RECORD PRINT THE CUSTOMER LINE AND   03410000
           *>    STORE THE CURRENT SALESREP AND BRANCH NUMBER TO THE OLD03420000
           *>  * IF THE BRANCH NUMBER IS GREATER THAN THE CURRENT ONE   03430000
           *>    THEN PRINT THE SALES REP LINE, BRANCH TOTAL LINE, AND  03440000
           *>    THEN THE NEW CUSTOMER'S LINE. AFTER UPDATE THE BRANCH  03450000
           *>    AND SALESREP NUMBERS                                   03460000
           *>  * IF THE SALES REP NUMBER IS GREATER THAN THE CURRENT ONE03470000
           *>    PRINT SALES LINE THEN THE CURRENT CUSTOMER LINE AFTER  03480000
           *>    UPDATE THE SALES REP NUMBER                            03490000
           *>  * IF NOTHING ELSE JUST PRINT THE CUSTOMER RECORD         03500000
           EVALUATE TRUE                                                03510000
               WHEN CUSTMAST-EOF                                        03520000
                   PERFORM 250-PRINT-SALESREP-LINE                      03530000
                   PERFORM 240-PRINT-BRANCH-LINE                        03540000
               WHEN FIRST-RECORD                                        03550000
                   PERFORM 220-PRINT-CUSTOMER-LINE                      03560000
                   SET FIRST-RECORD TO FALSE                            03570000
                 *>MOVE "N" TO FIRST-RECORD-SWITCH                      03580000
                   MOVE CM-SALESREP-NUMBER TO OLD-SALESREP-NUMBER       03590000
                   MOVE CM-BRANCH-NUMBER TO OLD-BRANCH-NUMBER           03600000
               WHEN CM-BRANCH-NUMBER > OLD-BRANCH-NUMBER                03610000
                   PERFORM 250-PRINT-SALESREP-LINE                      03620000
                   PERFORM 240-PRINT-BRANCH-LINE                        03630000
                   PERFORM 220-PRINT-CUSTOMER-LINE                      03640000
                   MOVE CM-SALESREP-NUMBER TO OLD-SALESREP-NUMBER       03650000
                   MOVE CM-BRANCH-NUMBER TO OLD-BRANCH-NUMBER           03660000
               WHEN NOT (CM-SALESREP-NUMBER = OLD-SALESREP-NUMBER)      03670000
                   PERFORM 250-PRINT-SALESREP-LINE                      03680000
                   PERFORM 220-PRINT-CUSTOMER-LINE                      03690000
                   MOVE CM-SALESREP-NUMBER TO OLD-SALESREP-NUMBER       03700000
               WHEN OTHER                                               03710000
                   PERFORM 220-PRINT-CUSTOMER-LINE                      03720000
           END-EVALUATE.                                                03730000
                                                                        03740000
      **************************************************************    03750000
      * READS A LINE OF THE INPUT FILE AND IF ITS THE LAST ONE     *    03760000
      * UPDATES THE CUSTOMER-EOF-SWITCH (END-OF-FILE)              *    03770000
      **************************************************************    03780000
       210-READ-CUSTOMER-RECORD.                                        03790000
                                                                        03800000
           READ CUSTMAST                                                03810000
               AT END                                                   03820000
                   MOVE "Y" TO CUSTMAST-EOF-SWITCH.                     03830000
                                                                        03840000
      **************************************************************    03850000
      * PRINTS THE CURRENT CUSTOMER LINE TO THE OUTPUT FILE        *    03860000
      * UPDATES THE LINE COUNTER SO IT KNOWS WHEN IT HAS TO        *    03870000
      * REPRINT THE HEADER LINES FOR A NEW PAGE                    *    03880000
      **************************************************************    03890000
       220-PRINT-CUSTOMER-LINE.                                         03900000
                                                                        03910000
           *> IF INFORMATION WE HAVE PRINTED EXCEEDS THE PAGE LIMIT     03920000
           *> WE REPRINT THE HEADERS FOR THE NEW PAGE                   03930000
           IF LINE-COUNT >= LINES-ON-PAGE                               03940000
               PERFORM 230-PRINT-HEADING-LINES.                         03950000
                                                                        03960000
           *> PERFROMS DUTIES BASED ON THE ENTRY                        03970000
           *>  * IF IT'S THE FIRST RECORD PRINT THE BRANCH NUMBER       03980000
           *>    AND THE SALESREP NUMBER                                03990000
           *>  * IF IT'S A NEW BRANCH PRINT THE BRANCH NUMBER AND       04000000
           *>    SALES REP NUMBER                                       04010000
           *>  * IF IT'S A NEW SALES REP PRINT THE SALESREP NUMBER      04020000
           *>  * OTHERWISE PRINT SPACES IN THOSE LINES FOR PADDING      04030000
           EVALUATE TRUE                                                04040000
               WHEN FIRST-RECORD                                        04050000
                   MOVE CM-BRANCH-NUMBER TO CL-BRANCH-NUMBER            04060000
                   MOVE CM-SALESREP-NUMBER TO CL-SALESREP-NUMBER        04070000
                   PERFORM 223-MOVE-SALESREP-NUMBER                     04071008
               WHEN CM-BRANCH-NUMBER > OLD-BRANCH-NUMBER                04080000
                   MOVE CM-BRANCH-NUMBER TO CL-BRANCH-NUMBER            04090000
                   MOVE CM-SALESREP-NUMBER TO CL-SALESREP-NUMBER        04100000
                   PERFORM 223-MOVE-SALESREP-NUMBER                     04101008
               WHEN NOT (CM-SALESREP-NUMBER = OLD-SALESREP-NUMBER)      04110000
                   MOVE CM-SALESREP-NUMBER TO CL-SALESREP-NUMBER        04120000
                   PERFORM 223-MOVE-SALESREP-NUMBER                     04121008
                   MOVE SPACES TO CL-BRANCH-NUMBER                      04130000
               WHEN OTHER                                               04140000
                   MOVE SPACES TO CL-BRANCH-NUMBER                      04150000
                   MOVE SPACES TO CL-SALESREP-NUMBER                    04160000
           END-EVALUATE.                                                04170000
                                                                        04180000
           *> MOVE THE DATA PULLED FROM THE INPUT FILE INTO THE         04190000
           *> CUSTOMER LINE RECORD FOR LATER OUTPUT                     04200000
           MOVE CM-CUSTOMER-NUMBER  TO CL-CUSTOMER-NUMBER.              04210000
           MOVE CM-CUSTOMER-NAME    TO CL-CUSTOMER-NAME.                04220000
           MOVE CM-SALES-THIS-YTD   TO CL-SALES-THIS-YTD.               04230000
           MOVE CM-SALES-LAST-YTD   TO CL-SALES-LAST-YTD.               04240000
                                                                        04250000
           *> CALCULATE THE DIFFERENCE BETWEEN THIS YEAR'S SALES AND    04260000
           *> AND LAST THEN SAVE THESE RESULT TO CHANGE-AMOUNT AND      04270000
           COMPUTE CHANGE-AMOUNT =                                      04280000
               CM-SALES-THIS-YTD - CM-SALES-LAST-YTD.                   04290000
           MOVE CHANGE-AMOUNT TO CL-CHANGE-AMOUNT.                      04300000
                                                                        04310000
           *> CALCULATE THE PERCENT FOR THE CHANGE IN SALES BETWEEN     04320000
           *> THIS AND LAST YTD, IF THERE WAS NO LAST YEAR SALES        04330000
           *> NUMBER WE MOVE 999.9 TO THE PERECENTAGE SINCE IT'S        04340000
           *> A DIVIDE BY ZERO ERROR OTHERWISE                          04350000
           IF CM-SALES-LAST-YTD = ZERO                                  04360000
               MOVE "  N/A " TO CL-CHANGE-PERCENT-R                     04370000
           ELSE                                                         04380000
               COMPUTE CL-CHANGE-PERCENT ROUNDED =                      04390000
                   CHANGE-AMOUNT * 100 / CM-SALES-LAST-YTD              04400000
                   ON SIZE ERROR                                        04410000
                       MOVE "OVRFLW" TO CL-CHANGE-PERCENT-R.            04420000
                                                                        04430000
           *> PRINT THIS CUSTOMERS INFORMATION TO THE OUTPUT FILE       04440000
           MOVE CUSTOMER-LINE TO PRINT-AREA.                            04450000
           PERFORM 225-WRITE-REPORT-LINE.                               04460000
                                                                        04470000
           *> ADD THIS CUSTOMERS SALES TO THE SALESREP TOTALS           04480000
           ADD CM-SALES-THIS-YTD TO SALESREP-TOTAL-THIS-YTD.            04490000
           ADD CM-SALES-LAST-YTD TO SALESREP-TOTAL-LAST-YTD.            04500000
                                                                        04501008
      **************************************************************    04501108
      * TODO                                                       *    04501208
      *                                                            *    04501308
      **************************************************************    04501408
      223-MOVE-SALESREP-NUMBER.                                         04502008
          SET SRT-INDEX TO 1.                                           04503008
          SEARCH SALESREP-GROUP                                         04510008
              AT END                                                    04511008
                  MOVE "UNKNOWN" TO CL-SALESREP-NAME                    04512008
              WHEN SALESREP-NUMBER (SRT-INDEX) = CM-SALESREP-NUMBER     04513008
                  MOVE SALESREP-NAME (SRT-INDEX) TO CL-SALESREP-NAME    04514008
          END-SEARCH.                                                   04515008
                                                                        04516008
      **************************************************************    04520000
      * PRINT ALL THE HEADER LINES TO THE OUTPUT FILE, RAN ONCE    *    04530000
      * FOR EVERY PAGE                                             *    04540000
      **************************************************************    04550000
       225-WRITE-REPORT-LINE.                                           04560000
           WRITE PRINT-AREA.                                            04570000
           ADD 1 TO LINE-COUNT.                                         04580000
                                                                        04590000
      **************************************************************    04600000
      * PRINT ALL THE HEADER LINES TO THE OUTPUT FILE, RAN ONCE    *    04610000
      * FOR EVERY PAGE                                             *    04620000
      **************************************************************    04630000
       230-PRINT-HEADING-LINES.                                         04640000
                                                                        04650000
           *> HEADERS ARE PLACED AT THE START OF EVERY PAGE             04660000
           *> SO WE INCREASE THE PAGE COUNT HERE                        04670000
           ADD 1 TO PAGE-COUNT.                                         04680000
           MOVE PAGE-COUNT     TO HL1-PAGE-NUMBER.                      04690000
                                                                        04700000
           *> PRINT EACH HEADER LINE TO THE OUTPUT FILE                 04710000
           MOVE HEADING-LINE-1 TO PRINT-AREA.                           04720000
           WRITE PRINT-AREA.                                            04730000
           MOVE HEADING-LINE-2 TO PRINT-AREA.                           04740000
           WRITE PRINT-AREA.                                            04750000
           MOVE HEADING-LINE-3 TO PRINT-AREA.                           04760000
           WRITE PRINT-AREA.                                            04770000
           MOVE HEADING-LINE-4 TO PRINT-AREA.                           04780000
           WRITE PRINT-AREA.                                            04790000
           MOVE HEADING-LINE-5 TO PRINT-AREA.                           04800000
           WRITE PRINT-AREA.                                            04810000
           MOVE HEADING-LINE-6 TO PRINT-AREA.                           04820000
           WRITE PRINT-AREA.                                            04830000
                                                                        04840000
           *> RESET THE LINE COUNTER SINCE EVERY HEADER IS THE START    04850000
           *> OF A NEW PAGE                                             04860000
           MOVE ZERO TO LINE-COUNT.                                     04870000
                                                                        04880000
      **************************************************************    04890000
      * PRINTS THE CURRENT BRANCH LINE TOTALS, RAN ONCE FOR EVERY  *    04900000
      * BRANCH. ALSO CALCULATES THE CHANGE IN THE BRANCH           *    04910000
      **************************************************************    04920000
       240-PRINT-BRANCH-LINE.                                           04930000
                                                                        04940000
           *> MOVE THE BRANCH TOTALS TO THE BRANCH TOTAL LINE           04950000
           MOVE BRANCH-TOTAL-THIS-YTD TO BTL-SALES-THIS-YTD.            04960000
           MOVE BRANCH-TOTAL-LAST-YTD TO BTL-SALES-LAST-YTD.            04970000
                                                                        04980000
           *> CALCULATE THE CHANGE BETWEEN THIS-YTD AND LAST            04990000
           *> FOR THE CURRENT BRANCH AND ADD IT TO THE TOTAL LINE       05000000
           COMPUTE CHANGE-AMOUNT =                                      05010000
               BRANCH-TOTAL-THIS-YTD - BRANCH-TOTAL-LAST-YTD.           05020000
           MOVE CHANGE-AMOUNT TO BTL-CHANGE-AMOUNT.                     05030000
                                                                        05040000
           *> CALCULATE THE CHANGE PERCENT BETWEEN YTD'S                05050000
           *> THEN MOVE TO THE BRANCH TOTAL LINE                        05060000
           IF BRANCH-TOTAL-LAST-YTD = ZERO                              05070000
               MOVE "  N/A " TO BTL-CHANGE-PERCENT-R                    05080000
           ELSE                                                         05090000
               COMPUTE BTL-CHANGE-PERCENT ROUNDED =                     05100000
                   CHANGE-AMOUNT * 100 / BRANCH-TOTAL-LAST-YTD          05110000
                   ON SIZE ERROR                                        05120000
                       MOVE "OVRFLW" TO BTL-CHANGE-PERCENT-R.           05130000
                                                                        05140000
           *> PRINT BRANCH LINE                                         05150000
           MOVE BRANCH-TOTAL-LINE TO PRINT-AREA.                        05160000
           PERFORM 225-WRITE-REPORT-LINE.                               05170000
                                                                        05180000
           *> WRITE A BLANK SPACER LINE                                 05190000
           MOVE SPACES TO PRINT-AREA.                                   05200000
           PERFORM 225-WRITE-REPORT-LINE.                               05210000
                                                                        05220000
           *> ADD THE BRANCH TOTALS TO THE GRAND TOTALS                 05230000
           ADD BRANCH-TOTAL-THIS-YTD TO GRAND-TOTAL-THIS-YTD.           05240000
           ADD BRANCH-TOTAL-LAST-YTD TO GRAND-TOTAL-LAST-YTD.           05250000
                                                                        05260000
           *> ZERO OUT THE BRANCH TOTALS                                05270000
           MOVE ZERO TO BRANCH-TOTAL-THIS-YTD.                          05280000
           MOVE ZERO TO BRANCH-TOTAL-LAST-YTD.                          05290000
                                                                        05300000
      **************************************************************    05310000
      * PRINTS THE CURRENT SALESREP'S TOTALS, RAN ONCE FOR EVERY   *    05320000
      * SALESREP. ALSO CALCULATES THE CHANGE BETWEEN YEARS         *    05330000
      **************************************************************    05340000
       250-PRINT-SALESREP-LINE.                                         05350000
                                                                        05360000
           *> MOVE THE SALESREP TOTALS TO THE SALESREP TOTAL LINE       05370000
           MOVE SALESREP-TOTAL-THIS-YTD TO STL-SALES-THIS-YTD.          05380000
           MOVE SALESREP-TOTAL-LAST-YTD TO STL-SALES-LAST-YTD.          05390000
                                                                        05400000
           *> CALCULATE THE CHANGE BETWEEN THIS-YTD AND LAST            05410000
           *> FOR THE CURRENT SALESREP AND ADD IT TO THE TOTAL LINE     05420000
           COMPUTE CHANGE-AMOUNT =                                      05430000
               SALESREP-TOTAL-THIS-YTD - SALESREP-TOTAL-LAST-YTD.       05440000
           MOVE CHANGE-AMOUNT TO STL-CHANGE-AMOUNT.                     05450000
                                                                        05460000
           *> CALCULATE THE CHANGE PERCENT BETWEEN YTD'S                05470000
           *> THEN MOVE TO THE SALESREP TOTAL LINE                      05480000
           IF SALESREP-TOTAL-LAST-YTD = ZERO                            05490000
               MOVE "  N/A " TO STL-CHANGE-PERCENT-R                    05500000
           ELSE                                                         05510000
               COMPUTE STL-CHANGE-PERCENT ROUNDED =                     05520000
                   CHANGE-AMOUNT * 100 / SALESREP-TOTAL-LAST-YTD        05530000
                   ON SIZE ERROR                                        05540000
                       MOVE "OVRFLW" TO STL-CHANGE-PERCENT-R.           05550000
                                                                        05560000
           *> PRINT SALESREP LINE                                       05570000
           MOVE SALESREP-TOTAL-LINE TO PRINT-AREA.                      05580000
           PERFORM 225-WRITE-REPORT-LINE.                               05590000
                                                                        05600000
           *> PRINT A SPACER LINE                                       05610000
           MOVE SPACES TO PRINT-AREA.                                   05620000
           PERFORM 225-WRITE-REPORT-LINE.                               05630000
                                                                        05640000
           *> ADD THE SALESREP TOTALS TO THE BRANCH TOTALS              05650000
           *> WHEN A BRANCH IS PRINTED THEN THOSE TOTALS ARE MOVED      05660000
           *> TO THE GRAND TOTALS                                       05670000
           *> CUSTOMER->SALESREP->BRANCH->GRAND-TOTAL                   05680000
           ADD SALESREP-TOTAL-THIS-YTD TO BRANCH-TOTAL-THIS-YTD.        05690000
           ADD SALESREP-TOTAL-LAST-YTD TO BRANCH-TOTAL-LAST-YTD.        05700000
                                                                        05710000
           *> ZERO OUT THE SALESREP TOTALS                              05720000
           MOVE ZERO TO SALESREP-TOTAL-THIS-YTD.                        05730000
           MOVE ZERO TO SALESREP-TOTAL-LAST-YTD.                        05740000
      **************************************************************    05750000
      * PRINTS THE GRAND TOTALS FOR ALL THE CUSTOMERS, RAN ONCE    *    05760000
      * AT THE VERY END OF THE PROGRAM WHEN ALL CUSTOMERS HAVE     *    05770000
      * BEEN PRINTED                                               *    05780000
      **************************************************************    05790000
       300-PRINT-GRAND-TOTALS.                                          05800000
                                                                        05810000
           *> MOVE THE GRAND TOTALS FOR THE SALES TO THE                05820000
           *> OUTPUT LINE FOR GRAND TOTALS                              05830000
           MOVE GRAND-TOTAL-THIS-YTD TO GTL-SALES-THIS-YTD.             05840000
           MOVE GRAND-TOTAL-LAST-YTD TO GTL-SALES-LAST-YTD.             05850000
                                                                        05860000
           *> COMPUTE THE GRAND TOTAL FOR THE CHANGE AMOUNT             05870000
           COMPUTE CHANGE-AMOUNT =                                      05880000
               GRAND-TOTAL-THIS-YTD - GRAND-TOTAL-LAST-YTD.             05890000
           MOVE CHANGE-AMOUNT TO GTL-CHANGE-AMOUNT.                     05900000
                                                                        05910000
           *> CALCULATE THE TOTAL CHANGE IN PERCENT BETWEEN             05920000
           *> THIS YTD AND LAST YTD FOR ALL CUSTOMERS                   05930000
           *> IF THERE WAS NO LAST YEAR FOR ANYONE DEFAULT TO           05940000
           *> A PERCENT OF 999.9 TO AVOID DIVIDE BY ZERO ERROR          05950000
           IF GRAND-TOTAL-LAST-YTD = ZERO                               05960000
               MOVE "  N/A " TO GTL-CHANGE-PERCENT-R                    05970000
           ELSE                                                         05980000
               COMPUTE GTL-CHANGE-PERCENT ROUNDED =                     05990000
                   CHANGE-AMOUNT * 100 / GRAND-TOTAL-LAST-YTD           06000000
                   ON SIZE ERROR                                        06010000
                       MOVE "OVRFLW" TO GTL-CHANGE-PERCENT-R.           06020000
                                                                        06030000
           *> PRINT THE GRAND-TOTAL TO THE OUTPUT FILE                  06040000
           MOVE GRAND-TOTAL-LINE TO PRINT-AREA.                         06050000
           PERFORM 225-WRITE-REPORT-LINE.                               06060000
