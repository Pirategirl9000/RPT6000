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
                                                                        00590000
      **************************************************************    00600000
      * SWITCHES FOR END OF FILE AND FIRST RECORD                  *    00610000
      **************************************************************    00620000
       01  SWITCHES.                                                    00630000
           05  CUSTMAST-EOF-SWITCH     PIC X    VALUE "N".              00640000
               88  CUSTMAST-EOF                 VALUE "Y".              00650000
           05  FIRST-RECORD-SWITCH     PIC X    VALUE "Y".              00660000
               88  FIRST-RECORD                 VALUE "Y"               00670000
                   WHEN FALSE IS                      "N".              00680000
                                                                        00690000
      **************************************************************    00700000
      * SWITCH FOR END OF FILE                                     *    00710000
      **************************************************************    00720000
       01  CONTROL-FIELDS PACKED-DECIMAL.                                              00730000
           05  OLD-BRANCH-NUMBER       PIC 99.                          00740000
           05  OLD-SALESREP-NUMBER     PIC 99.                          00750000
                                                                        00760000
      **************************************************************    00770000
      * STORES INFORMATION RELEVANT TO THE PAGE                    *    00780000
      **************************************************************    00790000
       01  PRINT-FIELDS PACKED-DECIMAL.                                                00800000
           05  PAGE-COUNT      PIC S9(3)   VALUE ZERO.                  00810000
           05  LINES-ON-PAGE   PIC S9(3)   VALUE +55.                   00820000
           05  LINE-COUNT      PIC S9(3)   VALUE +99.                   00830000
                                                                        00840000
      **************************************************************    00850000
      * STORES TOTAL FIELDS FOR CALCULATING                        *    00860000
      **************************************************************    00870000
       01  TOTAL-FIELDS PACKED-DECIMAL.                                                00880000
           05  BRANCH-TOTAL-THIS-YTD    PIC S9(6)V99   VALUE ZERO.      00890000
           05  BRANCH-TOTAL-LAST-YTD    PIC S9(6)V99   VALUE ZERO.      00900000
           05  SALESREP-TOTAL-THIS-YTD  PIC S9(6)V99   VALUE ZERO.      00910000
           05  SALESREP-TOTAL-LAST-YTD  PIC S9(6)V99   VALUE ZERO.      00920000
           05  GRAND-TOTAL-THIS-YTD     PIC S9(7)V99   VALUE ZERO.      00930000
           05  GRAND-TOTAL-LAST-YTD     PIC S9(7)V99   VALUE ZERO.      00940000
                                                                        00950000
      **************************************************************    00960000
      * USED TO PULL IN THE CURRENT-DATE-TIME VIA THE FUNCTION     *    00970000
      * CURRENT-DATE-AND-TIME WHICH WILL BE USED IN HEADER LINES   *    00980000
      **************************************************************    00990000
       01  CURRENT-DATE-AND-TIME.                                       01000000
           05  CD-YEAR         PIC 9999.                                01010000
           05  CD-MONTH        PIC 99.                                  01020000
           05  CD-DAY          PIC 99.                                  01030000
           05  CD-HOURS        PIC 99.                                  01040000
           05  CD-MINUTES      PIC 99.                                  01050000
           05  FILLER          PIC X(9).                                01060000
                                                                        01070000
      **************************************************************    01080000
      * STORES VALUES USED FOR CALCULATIONS                       *     01090000
      **************************************************************    01100000
       01  CALCULATED-FIELDS.                                           01110000
           05 CHANGE-AMOUNT    PIC S9(5)V99.                            01120000
                                                                        01130000
      *------------------------------------------------------------*    01140000
      *                       OUTPUT FIELDS                        *    01150000
      *============================================================*    01160000
      *     THE FOLLOWING RECORDS ARE USED FOR PRINTING DATA TO    *    01170000
      *                      THE OUTPUT FILE                       *    01180000
      *------------------------------------------------------------*    01190000
                                                                        01200000
      **************************************************************    01210000
      * STORES THE FIRST HEADER LINE INFORMATION                   *    01220000
      * HOLDS THE DATE, REPORT TITLE, AND PAGE NUMBER              *    01230000
      **************************************************************    01240000
       01  HEADING-LINE-1.                                              01250000
           05  FILLER          PIC X(7)    VALUE "DATE:  ".             01260000
           05  HL1-MONTH       PIC 9(2).                                01270000
           05  FILLER          PIC X(1)    VALUE "/".                   01280000
           05  HL1-DAY         PIC 9(2).                                01290000
           05  FILLER          PIC X(1)    VALUE "/".                   01300000
           05  HL1-YEAR        PIC 9(4).                                01310000
           05  FILLER          PIC X(16)   VALUE SPACE.                 01320000
           05  FILLER          PIC X(20)   VALUE "YEAR-TO-DATE SALES R".01330000
           05  FILLER          PIC X(10)   VALUE "EPORT     ".          01340000
           05  FILLER          PIC X(19)   VALUE SPACE.                 01350000
           05  FILLER          PIC X(8)    VALUE "  PAGE: ".            01360000
           05  HL1-PAGE-NUMBER PIC ZZZ9.                                01370000
           05  FILLER          PIC X(39)   VALUE SPACE.                 01380000
                                                                        01390000
      **************************************************************    01400000
      * STORES THE SECOND HEADER LINE INFORMATION                  *    01410000
      * HOLDS THE TIME AND THE PROGRAM ID                          *    01420000
      **************************************************************    01430000
       01  HEADING-LINE-2.                                              01440000
           05  FILLER          PIC X(7)    VALUE "TIME:  ".             01450000
           05  HL2-HOURS       PIC 9(2).                                01460000
           05  FILLER          PIC X(1)    VALUE ":".                   01470000
           05  HL2-MINUTES     PIC 9(2).                                01480000
           05  FILLER          PIC X(72)   VALUE SPACE.                 01490000
           05  FILLER          PIC X(10)   VALUE "RPT6000".             01500001
           05  FILLER          PIC X(39)   VALUE SPACE.                 01510000
                                                                        01520000
      **************************************************************    01530000
      * STORES THE THIRD HEADER LINE USED TO DISPLAY A LINE SPACER *    01540000
      **************************************************************    01550000
       01  HEADING-LINE-3.                                              01560000
           05 FILLER               PIC X(130)   VALUE SPACE.            01570000
                                                                        01580000
      **************************************************************    01590000
      * STORES THE FOURTH HEADER LINE INFORMATION                  *    01600000
      * HOLDS THE DIFFERENT COLUMN NAMES - SOME ARE SPLIT ACROSS   *    01610000
      * THE NEXT HEADER LINE                                       *    01620000
      **************************************************************    01630000
       01  HEADING-LINE-4.                                              01640000
           05  FILLER      PIC X(7)    VALUE "BRANCH ".                 01650000
           05  FILLER      PIC X(6)    VALUE "SALES ".                  01660000
           05  FILLER      PIC X(20)   VALUE "CUST                ".    01670000
           05  FILLER      PIC X(20)   VALUE "            SALES   ".    01680000
           05  FILLER      PIC X(20)   VALUE "      SALES         ".    01690000
           05  FILLER      PIC X(20)   VALUE "CHANGE     CHANGE   ".    01700000
           05  FILLER      PIC X(44)   VALUE SPACE.                     01710000
                                                                        01720000
      **************************************************************    01730000
      * STORES THE FIFTH HEADER LINE INFORMATION                   *    01740000
      * HOLDS SOME OF THE COLUMN NAMES AS WELL AS THE OTHER HALF   *    01750000
      * OF COLUMN NAMES THAT STARTED IN THE LAST HEADER LINE       *    01760000
      **************************************************************    01770000
       01  HEADING-LINE-5.                                              01780000
           05  FILLER      PIC X(8)    VALUE " NUM    ".                01790000
           05  FILLER      PIC X(5)    VALUE "REP  ".                   01800000
           05  FILLER      PIC X(20)   VALUE "NUM    CUSTOMER NAME".    01810000
           05  FILLER      PIC X(20)   VALUE "           THIS YTD ".    01820000
           05  FILLER      PIC X(20)   VALUE "     LAST YTD       ".    01830000
           05  FILLER      PIC X(20)   VALUE "AMOUNT    PERCENT   ".    01840000
           05  FILLER      PIC X(44)   VALUE SPACE.                     01850000
                                                                        01860000
      **************************************************************    01870000
      * STORES THE SIXTH HEADER LINE WHICH IS USED FOR SPACING     *    01880000
      **************************************************************    01890000
       01  HEADING-LINE-6.                                              01900000
           05  FILLER      PIC X(130)  VALUE SPACES.                    01910000
                                                                        01920000
      **************************************************************    01930000
      * STORES INFORMATION ABOUT CURRENT CUSTOMER                  *    01940000
      * HOLDS THE BRANCH NUMBER, SALES REP NUMBER, CUSTOMER NUMBER,*    01950000
      * CUSTOMER NAME, SALES THIS AND LAST YEAR-TO-DATE,           *    01960000
      * DIFFERENCE BETWEEN THIS YEARS SALES AND LAST, AND THE      *    01970000
      * DIFFERENCE IN PERCENT.                                     *    01980000
      **************************************************************    01990000
       01  CUSTOMER-LINE.                                               02000000
           05  FILLER              PIC X(2)     VALUE SPACE.            02010000
           05  CL-BRANCH-NUMBER    PIC X(2).                            02020000
           05  FILLER              PIC X(4)     VALUE SPACE.            02030000
           05  CL-SALESREP-NUMBER  PIC X(2).                            02040000
           05  FILLER              PIC X(3)     VALUE SPACE.            02050000
           05  CL-CUSTOMER-NUMBER  PIC 9(5).                            02060000
           05  FILLER              PIC X(2)     VALUE SPACE.            02070000
           05  CL-CUSTOMER-NAME    PIC X(20).                           02080000
           05  FILLER              PIC X(3)     VALUE SPACE.            02090000
           05  CL-SALES-THIS-YTD   PIC ZZ,ZZ9.99-.                      02100000
           05  FILLER              PIC X(4)     VALUE SPACE.            02110000
           05  CL-SALES-LAST-YTD   PIC ZZ,ZZ9.99-.                      02120000
           05  FILLER              PIC X(4)     VALUE SPACE.            02130000
           05  CL-CHANGE-AMOUNT    PIC ZZ,ZZ9.99-.                      02140000
           05  FILLER              PIC X(3)     VALUE SPACE.            02150000
           05  CL-CHANGE-PERCENT   PIC ---9.9.                          02160003
           05  CL-CHANGE-PRECENT-R REDEFINES CL-CHANGE-PERCENT          02161003
                                   PIC X(6).                            02162003
           05  FILLER              PIC X(47)    VALUE SPACE.            02170000
                                                                        02180000
      **************************************************************    02190000
      * STORES THE BRANCH TOTAL LINE                               *    02200000
      * HOLDS THE TOTALS FOR THIS AND LAST YEAR-TO-DATE IN SALES   *    02210000
      * FOR THIS BRANCH AS WELL AS THE PERCENT DIFFERENCE          *    02220000
      * USED FOR OUTPUTTING                                        *    02230000
      **************************************************************    02240000
       01  BRANCH-TOTAL-LINE.                                           02250000
           05  FILLER                PIC X(28)    VALUE SPACE.          02260004
           05  FILLER                PIC X(14)    VALUE "BRANCH TOTAL". 02270004
           05  BTL-SALES-THIS-YTD    PIC $$$,$$9.99-.                   02280005
           05  FILLER                PIC X(3)     VALUE SPACE.          02290004
           05  BTL-SALES-LAST-YTD    PIC $$$,$$9.99-.                   02300005
           05  FILLER                PIC X(3)     VALUE SPACE.          02310004
           05  BTL-CHANGE-AMOUNT     PIC $$$,$$9.99-.                   02320006
           05  FILLER                PIC X(3)     VALUE SPACE.          02330004
           05  BTL-CHANGE-PERCENT    PIC +++9.9.                        02340004
           05  BTL-CHANGE-PERCENT-R  REDEFINES BTL-CHANGE-PERCENT       02341004
                                     PIC X(6).                          02342004
           05  FILLER                PIC X(48)    VALUE " **".          02350004
                                                                        02360000
      **************************************************************    02370000
      * STORES THE SALES REP TOTAL LINE                            *    02380000
      * HOLDS THE TOTALS FOR THIS AND LAST YEAR-TO-DATE IN SALES   *    02390000
      * FOR THIS REP AS WELL AS THE PERCENT DIFFERENCE             *    02400000
      * USED FOR OUTPUTTING                                        *    02410000
      **************************************************************    02420000
       01  SALESREP-TOTAL-LINE.                                         02430000
           05  FILLER               PIC X(28)    VALUE SPACE.           02440005
           05  FILLER               PIC X(14)    VALUE "SALESREP TOTAL".02450005
           05  STL-SALES-THIS-YTD   PIC $$$,$$9.99-.                    02460005
           05  FILLER               PIC X(3)     VALUE SPACE.           02470005
           05  STL-SALES-LAST-YTD   PIC $$$,$$9.99-.                    02480005
           05  FILLER               PIC X(3)     VALUE SPACE.           02490005
           05  STL-CHANGE-AMOUNT    PIC $$$,$$9.99-.                    02500006
           05  FILLER               PIC X(3)     VALUE SPACE.           02510005
           05  STL-CHANGE-PERCENT   PIC ZZ9.9-.                         02520005
           05  STL-CHANGE-PERCENT-R REDEFINES STL-CHANGE-PERCENT        02521005
                                    PIC X(6).                           02522005
           05  FILLER               PIC X(48)    VALUE " *".            02530005
      **************************************************************    02540000
      * STORES THE SECOND GRAND TOTAL LINE                         *    02550000
      * HOLDS THE TOTAL SALES FOR THIS AND LAST YEAR-TO-DATE,      *    02560000
      * THE TOTAL DIFFERENCE IN SALES MADE BETWEEN THE TWO YEARS   *    02570000
      * AND THE PERCENTAGE DIFFERENCE - FOR OUTPUTTING             *    02580000
      **************************************************************    02590000
       01  GRAND-TOTAL-LINE.                                            02600000
           05  FILLER              PIC X(28)    VALUE SPACE.            02610000
           05  FILLER              PIC X(12)    VALUE "GRAND TOTAL ".   02620000
           05  GTL-SALES-THIS-YTD  PIC $,$$$,$$9.99-.                   02630006
           05  FILLER              PIC X(1)     VALUE SPACE.            02640000
           05  GTL-SALES-LAST-YTD  PIC $,$$$,$$9.99-.                   02650006
           05  FILLER              PIC X        VALUE SPACE.            02660000
           05  GTL-CHANGE-AMOUNT   PIC $,$$$,$$9.99-.                   02670006
           05  FILLER              PIC X(3)     VALUE SPACE.            02680000
           05  GTL-CHANGE-PERCENT  PIC ZZ9.9-.                          02690000
           05  GTL-CHANGE-PERCENT-R REDEFINES GTL-CHANGE-PERCENT
                                   PIC X(6). 
           05  FILLER              PIC X(43)    VALUE " ***".           02700000
                                                                        02710000
       PROCEDURE DIVISION.                                              02720000
                                                                        02730000
      **************************************************************    02740000
      * OPENS AND CLOSES THE FILES AND DELEGATES THE WORK FOR      *    02750000
      * READING AND WRITING TO AND FROM THEM                       *    02760000
      **************************************************************    02770000
       000-PREPARE-SALES-REPORT.                                        02780000
                                                                        02790000
           OPEN INPUT  CUSTMAST                                         02800000
                OUTPUT ORPT6000.                                        02810001
                                                                        02820000
           *> GRABS THE DATE AND TIME INFORMATION FOR                   02830000
           *> THE HEADER LINES                                          02840000
           PERFORM 100-FORMAT-REPORT-HEADING.                           02850000
                                                                        02860000
           *> GRAB AND PRINT CUSTOMER SALES TO THE OUPUT FILE UNTIL     02870000
           *> THE END OF THE INPUT FILE                                 02880000
           PERFORM 200-PREPARE-SALES-LINES                              02890000
               UNTIL CUSTMAST-EOF-SWITCH = "Y".                         02900000
                                                                        02910000
           *> OUTPUT THE GRAND TOTALS TO THE OUTPUT FILE                02920000
           PERFORM 300-PRINT-GRAND-TOTALS.                              02930000
                                                                        02940000
           CLOSE CUSTMAST                                               02950000
                 ORPT6000.                                              02960001
           STOP RUN.                                                    02970000
                                                                        02980000
      **************************************************************    02990000
      * FORMATS THE REPORT HEADER BY GRABBING THE DATE TIME AND    *    03000000
      * STORING IT IN THE RELEVENT HEADER DATA ITEMS               *    03010000
      **************************************************************    03020000
       100-FORMAT-REPORT-HEADING.                                       03030000
                                                                        03040000
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.         03050000
                                                                        03060000
           *> MOVE THE RESULT OF THE DATE-TIME FUNCTION TO THE          03070000
           *> DIFFERENT HEADER LINE FIELDS ASSOCIATED WITH THEM         03080000
           *> SO WE CAN INCLUDE THE DATE IN THE OUTPUT HEADER           03090000
           MOVE CD-MONTH   TO HL1-MONTH.                                03100000
           MOVE CD-DAY     TO HL1-DAY.                                  03110000
           MOVE CD-YEAR    TO HL1-YEAR.                                 03120000
           MOVE CD-HOURS   TO HL2-HOURS.                                03130000
           MOVE CD-MINUTES TO HL2-MINUTES.                              03140000
                                                                        03150000
      **************************************************************    03160000
      * CALLS THE PARAGRAPH TO READ A LINE OF THE CUSTOMER RECORD  *    03170000
      * THEN CALLS THE PARAGRAPH TO PRINT THE LINE IF ITS NOT THE  *    03180000
      * TERMINATING LINE OF THE FILE                               *    03190000
      **************************************************************    03200000
       200-PREPARE-SALES-LINES.                                         03210000
                                                                        03220000
           *> GRAB THE NEXT LINE FROM THE CUSTOMER RECORD               03230000
           PERFORM 210-READ-CUSTOMER-RECORD.                            03240000
                                                                        03250000
           *> PERFORMS DUTIES BASED ON THE ENTRY                        03260000
           *>  * IF WE RUN OUT OF DATA PRINT THE SALES AND BRANCH TOTALS03270000
           *>  * IF IT'S THE FIRST RECORD PRINT THE CUSTOMER LINE AND   03280000
           *>    STORE THE CURRENT SALESREP AND BRANCH NUMBER TO THE OLD03290000
           *>  * IF THE BRANCH NUMBER IS GREATER THAN THE CURRENT ONE   03300000
           *>    THEN PRINT THE SALES REP LINE, BRANCH TOTAL LINE, AND  03310000
           *>    THEN THE NEW CUSTOMER'S LINE. AFTER UPDATE THE BRANCH  03320000
           *>    AND SALESREP NUMBERS                                   03330000
           *>  * IF THE SALES REP NUMBER IS GREATER THAN THE CURRENT ONE03340000
           *>    PRINT SALES LINE THEN THE CURRENT CUSTOMER LINE AFTER  03350000
           *>    UPDATE THE SALES REP NUMBER                            03360000
           *>  * IF NOTHING ELSE JUST PRINT THE CUSTOMER RECORD         03370000
           EVALUATE TRUE                                                03380000
               WHEN CUSTMAST-EOF                                        03390000
                   PERFORM 250-PRINT-SALESREP-LINE                      03400000
                   PERFORM 240-PRINT-BRANCH-LINE                        03410000
               WHEN FIRST-RECORD                                        03420000
                   PERFORM 220-PRINT-CUSTOMER-LINE                      03430000
                   SET FIRST-RECORD TO FALSE                            03440000
                 *>MOVE "N" TO FIRST-RECORD-SWITCH                      03450000
                   MOVE CM-SALESREP-NUMBER TO OLD-SALESREP-NUMBER       03460000
                   MOVE CM-BRANCH-NUMBER TO OLD-BRANCH-NUMBER           03470000
               WHEN CM-BRANCH-NUMBER > OLD-BRANCH-NUMBER                03480000
                   PERFORM 250-PRINT-SALESREP-LINE                      03490000
                   PERFORM 240-PRINT-BRANCH-LINE                        03500000
                   PERFORM 220-PRINT-CUSTOMER-LINE                      03510000
                   MOVE CM-SALESREP-NUMBER TO OLD-SALESREP-NUMBER       03520000
                   MOVE CM-BRANCH-NUMBER TO OLD-BRANCH-NUMBER           03530000
               WHEN NOT (CM-SALESREP-NUMBER = OLD-SALESREP-NUMBER)      03540000
                   PERFORM 250-PRINT-SALESREP-LINE                      03550000
                   PERFORM 220-PRINT-CUSTOMER-LINE                      03560000
                   MOVE CM-SALESREP-NUMBER TO OLD-SALESREP-NUMBER       03570000
               WHEN OTHER                                               03580000
                   PERFORM 220-PRINT-CUSTOMER-LINE                      03590000
           END-EVALUATE.                                                03600000
                                                                        03610000
      **************************************************************    03620000
      * READS A LINE OF THE INPUT FILE AND IF ITS THE LAST ONE     *    03630000
      * UPDATES THE CUSTOMER-EOF-SWITCH (END-OF-FILE)              *    03640000
      **************************************************************    03650000
       210-READ-CUSTOMER-RECORD.                                        03660000
                                                                        03670000
           READ CUSTMAST                                                03680000
               AT END                                                   03690000
                   MOVE "Y" TO CUSTMAST-EOF-SWITCH.                     03700000
                                                                        03710000
      **************************************************************    03720000
      * PRINTS THE CURRENT CUSTOMER LINE TO THE OUTPUT FILE        *    03730000
      * UPDATES THE LINE COUNTER SO IT KNOWS WHEN IT HAS TO        *    03740000
      * REPRINT THE HEADER LINES FOR A NEW PAGE                    *    03750000
      **************************************************************    03760000
       220-PRINT-CUSTOMER-LINE.                                         03770000
                                                                        03780000
           *> IF INFORMATION WE HAVE PRINTED EXCEEDS THE PAGE LIMIT     03790000
           *> WE REPRINT THE HEADERS FOR THE NEW PAGE                   03800000
           IF LINE-COUNT >= LINES-ON-PAGE                               03810000
               PERFORM 230-PRINT-HEADING-LINES.                         03820000
                                                                        03830000
           *> PERFROMS DUTIES BASED ON THE ENTRY                        03840000
           *>  * IF IT'S THE FIRST RECORD PRINT THE BRANCH NUMBER       03850000
           *>    AND THE SALESREP NUMBER                                03860000
           *>  * IF IT'S A NEW BRANCH PRINT THE BRANCH NUMBER AND       03870000
           *>    SALES REP NUMBER                                       03880000
           *>  * IF IT'S A NEW SALES REP PRINT THE SALESREP NUMBER      03890000
           *>  * OTHERWISE PRINT SPACES IN THOSE LINES FOR PADDING      03900000
           EVALUATE TRUE                                                03910000
               WHEN FIRST-RECORD                                        03920000
                   MOVE CM-BRANCH-NUMBER TO CL-BRANCH-NUMBER            03930000
                   MOVE CM-SALESREP-NUMBER TO CL-SALESREP-NUMBER        03940000
               WHEN CM-BRANCH-NUMBER > OLD-BRANCH-NUMBER                03950000
                   MOVE CM-BRANCH-NUMBER TO CL-BRANCH-NUMBER            03960000
                   MOVE CM-SALESREP-NUMBER TO CL-SALESREP-NUMBER        03970000
               WHEN NOT (CM-SALESREP-NUMBER = OLD-SALESREP-NUMBER)      03980000
                   MOVE CM-SALESREP-NUMBER TO CL-SALESREP-NUMBER        03990000
                   MOVE SPACES TO CL-BRANCH-NUMBER                      04000000
               WHEN OTHER                                               04010000
                   MOVE SPACES TO CL-BRANCH-NUMBER                      04020000
                   MOVE SPACES TO CL-SALESREP-NUMBER                    04030000
           END-EVALUATE.                                                04040000
                                                                        04050000
           *> MOVE THE DATA PULLED FROM THE INPUT FILE INTO THE         04060000
           *> CUSTOMER LINE RECORD FOR LATER OUTPUT                     04070000
           MOVE CM-CUSTOMER-NUMBER  TO CL-CUSTOMER-NUMBER.              04080000
           MOVE CM-CUSTOMER-NAME    TO CL-CUSTOMER-NAME.                04090000
           MOVE CM-SALES-THIS-YTD   TO CL-SALES-THIS-YTD.               04100000
           MOVE CM-SALES-LAST-YTD   TO CL-SALES-LAST-YTD.               04110000
                                                                        04120000
           *> CALCULATE THE DIFFERENCE BETWEEN THIS YEAR'S SALES AND    04130000
           *> AND LAST THEN SAVE THESE RESULT TO CHANGE-AMOUNT AND      04140000
           COMPUTE CHANGE-AMOUNT =                                      04150000
               CM-SALES-THIS-YTD - CM-SALES-LAST-YTD.                   04160000
           MOVE CHANGE-AMOUNT TO CL-CHANGE-AMOUNT.                      04170000
                                                                        04180000
           *> CALCULATE THE PERCENT FOR THE CHANGE IN SALES BETWEEN     04190000
           *> THIS AND LAST YTD, IF THERE WAS NO LAST YEAR SALES        04200000
           *> NUMBER WE MOVE 999.9 TO THE PERECENTAGE SINCE IT'S        04210000
           *> A DIVIDE BY ZERO ERROR OTHERWISE                          04220000
           IF CM-SALES-LAST-YTD = ZERO                                  04230000
               MOVE "  N/A " TO CL-CHANGE-PERCENT-R                     04240000
           ELSE                                                         04250000
               COMPUTE CL-CHANGE-PERCENT ROUNDED =                      04260000
                   CHANGE-AMOUNT * 100 / CM-SALES-LAST-YTD              04270000
                   ON SIZE ERROR                                        04280000
                       MOVE "OVRFLW" TO CL-CHANGE-PERCENT-R.            04290000
                                                                        04300000
           *> PRINT THIS CUSTOMERS INFORMATION TO THE OUTPUT FILE       04310000
           MOVE CUSTOMER-LINE TO PRINT-AREA.                            04320000
           PERFORM 225-WRITE-REPORT-LINE.                               04330000
                                                                        04340000
           *> ADD THIS CUSTOMERS SALES TO THE SALESREP TOTALS           04350000
           ADD CM-SALES-THIS-YTD TO SALESREP-TOTAL-THIS-YTD.            04360000
           ADD CM-SALES-LAST-YTD TO SALESREP-TOTAL-LAST-YTD.            04370000
                                                                        04380000
      **************************************************************    04390000
      * PRINT ALL THE HEADER LINES TO THE OUTPUT FILE, RAN ONCE    *    04400000
      * FOR EVERY PAGE                                             *    04410000
      **************************************************************    04420000
       225-WRITE-REPORT-LINE.                                           04430000
           WRITE PRINT-AREA.                                            04440000
           ADD 1 TO LINE-COUNT.                                         04450000
                                                                        04460000
      **************************************************************    04470000
      * PRINT ALL THE HEADER LINES TO THE OUTPUT FILE, RAN ONCE    *    04480000
      * FOR EVERY PAGE                                             *    04490000
      **************************************************************    04500000
       230-PRINT-HEADING-LINES.                                         04510000
                                                                        04520000
           *> HEADERS ARE PLACED AT THE START OF EVERY PAGE             04530000
           *> SO WE INCREASE THE PAGE COUNT HERE                        04540000
           ADD 1 TO PAGE-COUNT.                                         04550000
           MOVE PAGE-COUNT     TO HL1-PAGE-NUMBER.                      04560000
                                                                        04570000
           *> PRINT EACH HEADER LINE TO THE OUTPUT FILE                 04580000
           MOVE HEADING-LINE-1 TO PRINT-AREA.                           04590000
           WRITE PRINT-AREA.                                            04600000
           MOVE HEADING-LINE-2 TO PRINT-AREA.                           04610000
           WRITE PRINT-AREA.                                            04620000
           MOVE HEADING-LINE-3 TO PRINT-AREA.                           04630000
           WRITE PRINT-AREA.                                            04640000
           MOVE HEADING-LINE-4 TO PRINT-AREA.                           04650000
           WRITE PRINT-AREA.                                            04660000
           MOVE HEADING-LINE-5 TO PRINT-AREA.                           04670000
           WRITE PRINT-AREA.                                            04680000
           MOVE HEADING-LINE-6 TO PRINT-AREA.                           04690000
           WRITE PRINT-AREA.                                            04700000
                                                                        04710000
           *> RESET THE LINE COUNTER SINCE EVERY HEADER IS THE START    04720000
           *> OF A NEW PAGE                                             04730000
           MOVE ZERO TO LINE-COUNT.                                     04740000
                                                                        04750000
      **************************************************************    04760000
      * PRINTS THE CURRENT BRANCH LINE TOTALS, RAN ONCE FOR EVERY  *    04770000
      * BRANCH. ALSO CALCULATES THE CHANGE IN THE BRANCH           *    04780000
      **************************************************************    04790000
       240-PRINT-BRANCH-LINE.                                           04800000
                                                                        04810000
           *> MOVE THE BRANCH TOTALS TO THE BRANCH TOTAL LINE           04820000
           MOVE BRANCH-TOTAL-THIS-YTD TO BTL-SALES-THIS-YTD.            04830000
           MOVE BRANCH-TOTAL-LAST-YTD TO BTL-SALES-LAST-YTD.            04840000
                                                                        04850000
           *> CALCULATE THE CHANGE BETWEEN THIS-YTD AND LAST            04860000
           *> FOR THE CURRENT BRANCH AND ADD IT TO THE TOTAL LINE       04870000
           COMPUTE CHANGE-AMOUNT =                                      04880000
               BRANCH-TOTAL-THIS-YTD - BRANCH-TOTAL-LAST-YTD.           04890000
           MOVE CHANGE-AMOUNT TO BTL-CHANGE-AMOUNT.                     04900000
                                                                        04910000
           *> CALCULATE THE CHANGE PERCENT BETWEEN YTD'S                04920000
           *> THEN MOVE TO THE BRANCH TOTAL LINE                        04930000
           IF BRANCH-TOTAL-LAST-YTD = ZERO                              04940000
               MOVE "  N/A " TO BTL-CHANGE-PERCENT-R                    04950000
           ELSE                                                         04960000
               COMPUTE BTL-CHANGE-PERCENT ROUNDED =                     04970000
                   CHANGE-AMOUNT * 100 / BRANCH-TOTAL-LAST-YTD          04980000
                   ON SIZE ERROR                                        04990000
                       MOVE "OVRFLW" TO BTL-CHANGE-PERCENT-R.           05000000
                                                                        05010000
           *> PRINT BRANCH LINE                                         05020000
           MOVE BRANCH-TOTAL-LINE TO PRINT-AREA.                        05030000
           PERFORM 225-WRITE-REPORT-LINE.                               05040000
                                                                        05050000
           *> WRITE A BLANK SPACER LINE                                 05060000
           MOVE SPACES TO PRINT-AREA.                                   05070000
           PERFORM 225-WRITE-REPORT-LINE.                               05080000
                                                                        05090000
           *> ADD THE BRANCH TOTALS TO THE GRAND TOTALS                 05100000
           ADD BRANCH-TOTAL-THIS-YTD TO GRAND-TOTAL-THIS-YTD.           05110000
           ADD BRANCH-TOTAL-LAST-YTD TO GRAND-TOTAL-LAST-YTD.           05120000
                                                                        05130000
           *> ZERO OUT THE BRANCH TOTALS                                05140000
           MOVE ZERO TO BRANCH-TOTAL-THIS-YTD.                          05150000
           MOVE ZERO TO BRANCH-TOTAL-LAST-YTD.                          05160000
                                                                        05170000
      **************************************************************    05180000
      * PRINTS THE CURRENT SALESREP'S TOTALS, RAN ONCE FOR EVERY   *    05190000
      * SALESREP. ALSO CALCULATES THE CHANGE BETWEEN YEARS         *    05200000
      **************************************************************    05210000
       250-PRINT-SALESREP-LINE.                                         05220000
                                                                        05230000
           *> MOVE THE SALESREP TOTALS TO THE SALESREP TOTAL LINE       05240000
           MOVE SALESREP-TOTAL-THIS-YTD TO STL-SALES-THIS-YTD.          05250000
           MOVE SALESREP-TOTAL-LAST-YTD TO STL-SALES-LAST-YTD.          05260000
                                                                        05270000
           *> CALCULATE THE CHANGE BETWEEN THIS-YTD AND LAST            05280000
           *> FOR THE CURRENT SALESREP AND ADD IT TO THE TOTAL LINE     05290000
           COMPUTE CHANGE-AMOUNT =                                      05300000
               SALESREP-TOTAL-THIS-YTD - SALESREP-TOTAL-LAST-YTD.       05310000
           MOVE CHANGE-AMOUNT TO STL-CHANGE-AMOUNT.                     05320000
                                                                        05330000
           *> CALCULATE THE CHANGE PERCENT BETWEEN YTD'S                05340000
           *> THEN MOVE TO THE SALESREP TOTAL LINE                      05350000
           IF SALESREP-TOTAL-LAST-YTD = ZERO                            05360000
               MOVE "  N/A " TO STL-CHANGE-PERCENT-R                    05370000
           ELSE                                                         05380000
               COMPUTE STL-CHANGE-PERCENT ROUNDED =                     05390000
                   CHANGE-AMOUNT * 100 / SALESREP-TOTAL-LAST-YTD        05400000
                   ON SIZE ERROR                                        05410000
                       MOVE "OVRFLW" TO STL-CHANGE-PERCENT-R.           05420000
                                                                        05430000
           *> PRINT SALESREP LINE                                       05440000
           MOVE SALESREP-TOTAL-LINE TO PRINT-AREA.                      05450000
           PERFORM 225-WRITE-REPORT-LINE.                               05460000
                                                                        05470000
           *> PRINT A SPACER LINE                                       05480000
           MOVE SPACES TO PRINT-AREA.                                   05490000
           PERFORM 225-WRITE-REPORT-LINE.                               05500000
                                                                        05510000
           *> ADD THE SALESREP TOTALS TO THE BRANCH TOTALS              05520000
           *> WHEN A BRANCH IS PRINTED THEN THOSE TOTALS ARE MOVED      05530000
           *> TO THE GRAND TOTALS                                       05540000
           *> CUSTOMER->SALESREP->BRANCH->GRAND-TOTAL                   05550000
           ADD SALESREP-TOTAL-THIS-YTD TO BRANCH-TOTAL-THIS-YTD.        05560000
           ADD SALESREP-TOTAL-LAST-YTD TO BRANCH-TOTAL-LAST-YTD.        05570000
                                                                        05580000
           *> ZERO OUT THE SALESREP TOTALS                              05590000
           MOVE ZERO TO SALESREP-TOTAL-THIS-YTD.                        05600000
           MOVE ZERO TO SALESREP-TOTAL-LAST-YTD.                        05610000
      **************************************************************    05620000
      * PRINTS THE GRAND TOTALS FOR ALL THE CUSTOMERS, RAN ONCE    *    05630000
      * AT THE VERY END OF THE PROGRAM WHEN ALL CUSTOMERS HAVE     *    05640000
      * BEEN PRINTED                                               *    05650000
      **************************************************************    05660000
       300-PRINT-GRAND-TOTALS.                                          05670000
                                                                        05680000
           *> MOVE THE GRAND TOTALS FOR THE SALES TO THE                05690000
           *> OUTPUT LINE FOR GRAND TOTALS                              05700000
           MOVE GRAND-TOTAL-THIS-YTD TO GTL-SALES-THIS-YTD.             05710000
           MOVE GRAND-TOTAL-LAST-YTD TO GTL-SALES-LAST-YTD.             05720000
                                                                        05730000
           *> COMPUTE THE GRAND TOTAL FOR THE CHANGE AMOUNT             05740000
           COMPUTE CHANGE-AMOUNT =                                      05750000
               GRAND-TOTAL-THIS-YTD - GRAND-TOTAL-LAST-YTD.             05760000
           MOVE CHANGE-AMOUNT TO GTL-CHANGE-AMOUNT.                     05770000
                                                                        05780000
           *> CALCULATE THE TOTAL CHANGE IN PERCENT BETWEEN             05790000
           *> THIS YTD AND LAST YTD FOR ALL CUSTOMERS                   05800000
           *> IF THERE WAS NO LAST YEAR FOR ANYONE DEFAULT TO           05810000
           *> A PERCENT OF 999.9 TO AVOID DIVIDE BY ZERO ERROR          05820000
           IF GRAND-TOTAL-LAST-YTD = ZERO                               05830000
               MOVE "  N/A " TO GTL-CHANGE-PERCENT-R                    05840000
           ELSE                                                         05850000
               COMPUTE GTL-CHANGE-PERCENT ROUNDED =                     05860000
                   CHANGE-AMOUNT * 100 / GRAND-TOTAL-LAST-YTD           05870000
                   ON SIZE ERROR                                        05880000
                       MOVE "OVRFLW" TO GTL-CHANGE-PERCENT-R.           05890000
                                                                        05900000
           *> PRINT THE GRAND-TOTAL TO THE OUTPUT FILE                  05910000
           MOVE GRAND-TOTAL-LINE TO PRINT-AREA.                         05920000
           PERFORM 225-WRITE-REPORT-LINE.                               05930000
