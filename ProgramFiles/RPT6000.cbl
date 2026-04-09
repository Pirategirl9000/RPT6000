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
           SELECT CUSTMAST ASSIGN TO CUSTMAST.                          00170034
           SELECT INPUT-SALESREP ASSIGN TO SALESREP.                    00180000
           SELECT ORPT6000 ASSIGN TO RPT6000.                           00190001
                                                                        00200000
       DATA DIVISION.                                                   00210000
                                                                        00220000
       FILE SECTION.                                                    00230034
      **************************************************************    00240000
      * INPUT FILES                                                *    00250000
      **************************************************************    00260034
                                                                        00270034
       FD  CUSTMAST                                                     00280000
           RECORDING MODE IS F                                          00290000
           LABEL RECORDS ARE STANDARD                                   00300000
           RECORD CONTAINS 130 CHARACTERS                               00310000
           BLOCK CONTAINS 130 CHARACTERS.                               00320000
       COPY CUSTMAST.                                                   00330037
                                                                        00340034
       FD  INPUT-SALESREP                                               00350034
           RECORDING MODE IS F                                          00360000
           LABEL RECORDS ARE STANDARD                                   00370000
           RECORD CONTAINS 130 CHARACTERS                               00380000
           BLOCK CONTAINS 130 CHARACTERS.                               00390000
        COPY SALESREP.                                                  00400037
                                                                        00410000
      **************************************************************    00420000
      * OUTPUT FILE                                                *    00430000
      **************************************************************    00440000
       FD  ORPT6000                                                     00450001
           RECORDING MODE IS F                                          00460000
           LABEL RECORDS ARE STANDARD                                   00470000
           RECORD CONTAINS 130 CHARACTERS                               00480000
           BLOCK CONTAINS 130 CHARACTERS.                               00490000
       01  PRINT-AREA      PIC X(130).                                  00500000
                                                                        00510000
       WORKING-STORAGE SECTION.                                         00520000
                                                                        00530000
      *------------------------------------------------------------*    00540000
      *                        WORKING FIELDS                      *    00550000
      *============================================================*    00560000
      *     THE FOLLOWING RECORDS ARE USED FOR WORKING WITH DATA   *    00570000
      *              AND ARE NOT USED FOR PROGRAM OUTPUT           *    00580000
      *------------------------------------------------------------*    00590000
       01 SALESREP-TABLE.                                               00600034
           05 SALESREP-GROUP OCCURS 100 TIMES                           00610007
                             INDEXED BY SRT-INDEX.                      00620007
               10 SALESREP-NUMBER   PIC 99.                             00630007
               10 SALESREP-NAME     PIC X(10).                          00640034
                                                                        00650034
      **************************************************************    00660000
      * SWITCHES FOR END OF FILE AND FIRST RECORD                  *    00670000
      **************************************************************    00680000
       01  SWITCHES.                                                    00690034
           05  SALESREP-EOF-SWITCH     PIC X    VALUE "N".              00700000
               88  SALESREP-EOF                 VALUE "Y".              00710034
           05  CUSTMAST-EOF-SWITCH     PIC X    VALUE "N".              00720000
               88  CUSTMAST-EOF                 VALUE "Y".              00730000
           05  FIRST-RECORD-SWITCH     PIC X    VALUE "Y".              00740000
               88  FIRST-RECORD                 VALUE "Y"               00750000
                   WHEN FALSE IS                      "N".              00760000
                                                                        00770000
      **************************************************************    00780000
      * SWITCH FOR END OF FILE                                     *    00790000
      **************************************************************    00800000
       01  CONTROL-FIELDS PACKED-DECIMAL.                               00810000
           05  OLD-BRANCH-NUMBER       PIC 99.                          00820000
           05  OLD-SALESREP-NUMBER     PIC 99.                          00830000
                                                                        00840000
      **************************************************************    00850000
      * STORES INFORMATION RELEVANT TO THE PAGE                    *    00860000
      **************************************************************    00870000
       01  PRINT-FIELDS PACKED-DECIMAL.                                 00880000
           05  PAGE-COUNT      PIC S9(3)   VALUE ZERO.                  00890000
           05  LINES-ON-PAGE   PIC S9(3)   VALUE +55.                   00900000
           05  LINE-COUNT      PIC S9(3)   VALUE +99.                   00910000
                                                                        00920000
      **************************************************************    00930000
      * STORES TOTAL FIELDS FOR CALCULATING                        *    00940000
      **************************************************************    00950000
       01  TOTAL-FIELDS PACKED-DECIMAL.                                 00960000
           05  BRANCH-TOTAL-THIS-YTD    PIC S9(6)V99   VALUE ZERO.      00970000
           05  BRANCH-TOTAL-LAST-YTD    PIC S9(6)V99   VALUE ZERO.      00980000
           05  SALESREP-TOTAL-THIS-YTD  PIC S9(6)V99   VALUE ZERO.      00990000
           05  SALESREP-TOTAL-LAST-YTD  PIC S9(6)V99   VALUE ZERO.      01000000
           05  GRAND-TOTAL-THIS-YTD     PIC S9(7)V99   VALUE ZERO.      01010000
           05  GRAND-TOTAL-LAST-YTD     PIC S9(7)V99   VALUE ZERO.      01020000
                                                                        01030000
      **************************************************************    01040000
      * USED TO PULL IN THE CURRENT-DATE-TIME VIA THE FUNCTION     *    01050000
      * CURRENT-DATE-AND-TIME WHICH WILL BE USED IN HEADER LINES   *    01060000
      **************************************************************    01070000
       01  CURRENT-DATE-AND-TIME.                                       01080000
           05  CD-YEAR         PIC 9999.                                01090000
           05  CD-MONTH        PIC 99.                                  01100000
           05  CD-DAY          PIC 99.                                  01110000
           05  CD-HOURS        PIC 99.                                  01120000
           05  CD-MINUTES      PIC 99.                                  01130000
           05  FILLER          PIC X(9).                                01140000
                                                                        01150000
      **************************************************************    01160000
      * STORES VALUES USED FOR CALCULATIONS                       *     01170000
      **************************************************************    01180000
       01  CALCULATED-FIELDS.                                           01190000
           05 CHANGE-AMOUNT    PIC S9(5)V99.                            01200000
                                                                        01210000
      *------------------------------------------------------------*    01220000
      *                       OUTPUT FIELDS                        *    01230000
      *============================================================*    01240000
      *     THE FOLLOWING RECORDS ARE USED FOR PRINTING DATA TO    *    01250000
      *                      THE OUTPUT FILE                       *    01260000
      *------------------------------------------------------------*    01270000
                                                                        01280000
      **************************************************************    01290000
      * STORES THE FIRST HEADER LINE INFORMATION                   *    01300000
      * HOLDS THE DATE, REPORT TITLE, AND PAGE NUMBER              *    01310000
      **************************************************************    01320000
       01  HEADING-LINE-1.                                              01330000
           05  FILLER          PIC X(7)    VALUE "DATE:  ".             01340000
           05  HL1-MONTH       PIC 9(2).                                01350000
           05  FILLER          PIC X(1)    VALUE "/".                   01360000
           05  HL1-DAY         PIC 9(2).                                01370000
           05  FILLER          PIC X(1)    VALUE "/".                   01380000
           05  HL1-YEAR        PIC 9(4).                                01390000
           05  FILLER          PIC X(26)   VALUE SPACE.                 01400000
           05  FILLER          PIC X(20)   VALUE "YEAR-TO-DATE SALES R".01410000
           05  FILLER          PIC X(31)   VALUE "EPORT".               01420000
           05  FILLER          PIC X(6)    VALUE "PAGE: ".              01430000
           05  HL1-PAGE-NUMBER PIC ZZZ9.                                01440000
           05  FILLER          PIC X(26)   VALUE SPACE.                 01450000
                                                                        01460000
      **************************************************************    01470000
      * STORES THE SECOND HEADER LINE INFORMATION                  *    01480000
      * HOLDS THE TIME AND THE PROGRAM ID                          *    01490000
      **************************************************************    01500000
       01  HEADING-LINE-2.                                              01510000
           05  FILLER          PIC X(7)    VALUE "TIME:  ".             01520000
           05  HL2-HOURS       PIC 9(2).                                01530000
           05  FILLER          PIC X(1)    VALUE ":".                   01540000
           05  HL2-MINUTES     PIC 9(2).                                01550000
           05  FILLER          PIC X(82)   VALUE SPACE.                 01560000
           05  FILLER          PIC X(7)    VALUE "RPT6000".             01570001
           05  FILLER          PIC X(29)   VALUE SPACE.                 01580000
                                                                        01590000
      **************************************************************    01600000
      * STORES THE THIRD HEADER LINE USED TO DISPLAY A LINE SPACER *    01610000
      **************************************************************    01620000
       01  HEADING-LINE-3.                                              01630000
           05 FILLER               PIC X(130)   VALUE SPACE.            01640000
                                                                        01650000
      **************************************************************    01660000
      * STORES THE FOURTH HEADER LINE INFORMATION                  *    01670000
      * HOLDS THE DIFFERENT COLUMN NAMES - SOME ARE SPLIT ACROSS   *    01680000
      * THE NEXT HEADER LINE                                       *    01690000
      **************************************************************    01700000
       01  HEADING-LINE-4.                                              01710000
           05  FILLER      PIC X(54)   VALUE SPACES.                    01720000
           05  FILLER      PIC X(19)   VALUE "SALES         SALES".     01730000
           05  FILLER      PIC X(8)    VALUE SPACES.                    01740014
           05  FILLER      PIC X(17)   VALUE "CHANGE     CHANGE".       01750014
           05  FILLER      PIC X(32)   VALUE SPACES.                    01760014
                                                                        01770000
      **************************************************************    01780000
      * STORES THE FIFTH HEADER LINE INFORMATION                   *    01790000
      * HOLDS SOME OF THE COLUMN NAMES AS WELL AS THE OTHER HALF   *    01800000
      * OF COLUMN NAMES THAT STARTED IN THE LAST HEADER LINE       *    01810000
      **************************************************************    01820000
       01  HEADING-LINE-5.                                              01830000
           05  FILLER         PIC X(17)  VALUE "BRANCH   SALESREP".     01840000
           05  FILLER         PIC X(13)  VALUE SPACES.                  01850000
           05  FILLER         PIC X(8)   VALUE "CUSTOMER".              01860000
           05  FILLER         PIC X(14)  VALUE SPACES.                  01870000
           05  FILLER         PIC X(22)  VALUE "THIS YTD      LAST YTD".01880000
           05  FILLER         PIC X(7)   VALUE SPACES.                  01890000
           05  FILLER         PIC X(18)  VALUE "AMOUNT     PERCENT".    01900000
           05  FILLER         PIC X(31)  VALUE SPACE.                   01910014
                                                                        01920000
      **************************************************************    01930000
      * STORES THE SIXTH HEADER LINE WHICH IS USED FOR SPACING     *    01940000
      **************************************************************    01950000
       01  HEADING-LINE-6.                                              01960000
           05  FILLER           PIC X(6)   VALUE ALL '-'.               01970014
           05  FILLER           PIC X(1)   VALUE SPACE.                 01980014
           05  FILLER           PIC X(13)  VALUE ALL '-'.               01990014
           05  FILLER           PIC X(1)   VALUE SPACE.                 02000014
           05  FILLER           PIC X(26)   VALUE ALL '-'.              02010014
           05  FILLER           PIC X(3)   VALUE SPACE.                 02020014
           05  FILLER           PIC X(11)  VALUE ALL '-'.               02030014
           05  FILLER           PIC X(3)   VALUE SPACE.                 02040014
           05  FILLER           PIC X(11)  VALUE ALL '-'.               02050014
           05  FILLER           PIC X(4)   VALUE SPACE.                 02060014
           05  FILLER           PIC X(11)  VALUE ALL '-'.               02070014
           05  FILLER           PIC X(2)   VALUE SPACE.                 02080014
           05  FILLER           PIC x(7)   VALUE ALL '-'.               02090014
           05  FILLER           PIC X(31)  VALUE SPACE.                 02100014
                                                                        02110000
      **************************************************************    02120000
      * STORES INFORMATION ABOUT CURRENT CUSTOMER                  *    02130000
      * HOLDS THE BRANCH NUMBER, SALES REP NUMBER, CUSTOMER NUMBER,*    02140000
      * CUSTOMER NAME, SALES THIS AND LAST YEAR-TO-DATE,           *    02150000
      * DIFFERENCE BETWEEN THIS YEARS SALES AND LAST, AND THE      *    02160000
      * DIFFERENCE IN PERCENT.                                     *    02170000
      **************************************************************    02180000
       01  CUSTOMER-LINE.                                               02190000
           05  FILLER               PIC X(2)       VALUE SPACE.         02200034
           05  CL-BRANCH-NUMBER     PIC X(2).                           02210034
           05  FILLER               PIC X(3)       VALUE SPACE.         02220034
           05  CL-SALESREP-NUMBER   PIC X(2).                           02230034
           05  FILLER               PIC X(1)       VALUE SPACE.         02240034
           05  CL-SALESREP-NAME     PIC X(10).                          02250034
           05  FILLER               PIC X(1)       VALUE SPACE.         02260034
           05  CL-CUSTOMER-NUMBER   PIC X(5).                           02270034
           05  FILLER               PIC X(1)       VALUE SPACE.         02280034
           05  CL-CUSTOMER-NAME     PIC X(20).                          02290034
           05  FILLER               PIC X(6)       VALUE SPACE.         02300034
           05  CL-SALES-THIS-YTD    PIC ZZ,ZZ9.99-.                     02310034
           05  FILLER               PIC X(4)       VALUE SPACE.         02320034
           05  CL-SALES-LAST-YTD    PIC ZZ,ZZ9.99-.                     02330034
           05  FILLER               PIC X(4)       VALUE SPACE.         02340034
           05  CL-CHANGE-AMOUNT     PIC ZZ,ZZ9.99-.                     02350034
           05  FILLER               PIC X(2)       VALUE SPACE.         02360034
           05  CL-CHANGE-PERCENT    PIC +++9.9.                         02370034
           05  CL-CHANGE-PERCENT-R  REDEFINES  CL-CHANGE-PERCENT        02380034
                                    PIC X(6).                           02390034
           05  FILLER               PIC X(31)      VALUE SPACE.         02400034
                                                                        02410000
      **************************************************************    02420000
      * STORES THE BRANCH TOTAL LINE                               *    02430000
      * HOLDS THE TOTALS FOR THIS AND LAST YEAR-TO-DATE IN SALES   *    02440000
      * FOR THIS BRANCH AS WELL AS THE PERCENT DIFFERENCE          *    02450000
      * USED FOR OUTPUTTING                                        *    02460000
      **************************************************************    02470000
       01  BRANCH-TOTAL-LINE.                                           02480016
           05  FILLER               PIC X(36)   VALUE SPACE.            02490016
           05  FILLER               PIC X(16)   VALUE "  BRANCH TOTAL". 02500016
           05  BTL-SALES-THIS-YTD   PIC $$$,$$9.99-.                    02510016
           05  FILLER               PIC X(3)    VALUE SPACE.            02520016
           05  BTL-SALES-LAST-YTD   PIC $$$,$$9.99-.                    02530016
           05  FILLER               PIC X(3)    VALUE SPACE.            02540016
           05  BTL-CHANGE-AMOUNT    PIC $$$,$$9.99-.                    02550016
           05  FILLER               PIC X(2)    VALUE SPACE.            02560016
           05  BTL-CHANGE-PERCENT   PIC +++9.9.                         02570016
           05  BTL-CHANGE-PERCENT-R REDEFINES BTL-CHANGE-PERCENT        02580016
                                    PIC X(6).                           02590016
           05  FILLER               PIC X(31)   VALUE "**".             02600016
                                                                        02610000
      **************************************************************    02620000
      * STORES THE SALES REP TOTAL LINE                            *    02630000
      * HOLDS THE TOTALS FOR THIS AND LAST YEAR-TO-DATE IN SALES   *    02640000
      * FOR THIS REP AS WELL AS THE PERCENT DIFFERENCE             *    02650000
      * USED FOR OUTPUTTING                                        *    02660000
      **************************************************************    02670000
       01  SALESREP-TOTAL-LINE.                                         02680016
           05  FILLER               PIC X(36)   VALUE SPACE.            02690016
           05  FILLER               PIC X(16)   VALUE "SALESREP TOTAL". 02700016
           05  STL-SALES-THIS-YTD   PIC $$$,$$9.99-.                    02710016
           05  FILLER               PIC X(3)    VALUE SPACE.            02720016
           05  STL-SALES-LAST-YTD   PIC $$$,$$9.99-.                    02730016
           05  FILLER               PIC X(3)    VALUE SPACE.            02740016
           05  STL-CHANGE-AMOUNT    PIC $$$,$$9.99-.                    02750016
           05  FILLER               PIC X(2)    VALUE SPACE.            02760016
           05  STL-CHANGE-PERCENT   PIC +++9.9.                         02770016
           05  STL-CHANGE-PERCENT-R REDEFINES STL-CHANGE-PERCENT        02780016
                                    PIC X(6).                           02790016
           05  FILLER               PIC X(31)   VALUE "*".              02800016
      **************************************************************    02810000
      * STORES THE SECOND GRAND TOTAL LINE                         *    02820000
      * HOLDS THE TOTAL SALES FOR THIS AND LAST YEAR-TO-DATE,      *    02830000
      * THE TOTAL DIFFERENCE IN SALES MADE BETWEEN THE TWO YEARS   *    02840000
      * AND THE PERCENTAGE DIFFERENCE - FOR OUTPUTTING             *    02850000
      **************************************************************    02860000
       01  GRAND-TOTAL-LINE.                                            02870016
           05  FILLER               PIC X(36)    VALUE SPACE.           02880016
           05  FILLER               PIC X(14)    VALUE "   GRAND TOTAL".02890016
           05  GTL-SALES-THIS-YTD   PIC $,$$$,$$9.99-.                  02900016
           05  FILLER               PIC X(1)     VALUE SPACE.           02910016
           05  GTL-SALES-LAST-YTD   PIC $,$$$,$$9.99-.                  02920016
           05  FILLER               PIC X(1)     VALUE SPACE.           02930016
           05  GTL-CHANGE-AMOUNT    PIC $,$$$,$$9.99-.                  02940016
           05  FILLER               PIC X(2)     VALUE SPACE.           02950016
           05  GTL-CHANGE-PERCENT   PIC +++9.9.                         02960016
           05  GTL-CHANGE-PERCENT-R REDEFINES GTL-CHANGE-PERCENT        02970016
                                    PIC X(6).                           02980016
           05  FILLER               PIC X(31)    VALUE "***".           02990016
                                                                        03000000
       PROCEDURE DIVISION.                                              03010000
                                                                        03020000
      **************************************************************    03030000
      * OPENS AND CLOSES THE FILES AND DELEGATES THE WORK FOR      *    03040000
      * READING AND WRITING TO AND FROM THEM                       *    03050000
      **************************************************************    03060000
       000-PREPARE-SALES-REPORT.                                        03070000
           INITIALIZE SALESREP-TABLE.                                   03080034
                                                                        03090034
           OPEN INPUT  CUSTMAST                                         03100034
                INPUT INPUT-SALESREP                                    03110034
                OUTPUT ORPT6000.                                        03120034
                                                                        03130034
           PERFORM 205-LOAD-SALESREP-TABLE.                             03140034
                                                                        03150000
           *> GRABS THE DATE AND TIME INFORMATION FOR                   03160000
           *> THE HEADER LINES                                          03170000
           PERFORM 100-FORMAT-REPORT-HEADING.                           03180034
                                                                        03190000
           *> GRAB AND PRINT CUSTOMER SALES TO THE OUPUT FILE UNTIL     03200000
           *> THE END OF THE INPUT FILE                                 03210000
           PERFORM 200-PREPARE-SALES-LINES                              03220000
               UNTIL CUSTMAST-EOF-SWITCH = "Y".                         03230000
                                                                        03240034
                                                                        03250034
                                                                        03260034
                                                                        03270034
           *> OUTPUT THE GRAND TOTALS TO THE OUTPUT FILE                03280000
           PERFORM 300-PRINT-GRAND-TOTALS.                              03290000
                                                                        03300000
           CLOSE CUSTMAST                                               03310000
                 ORPT6000.                                              03320001
           STOP RUN.                                                    03330000
                                                                        03340000
      **************************************************************    03350000
      * FORMATS THE REPORT HEADER BY GRABBING THE DATE TIME AND    *    03360000
      * STORING IT IN THE RELEVENT HEADER DATA ITEMS               *    03370000
      **************************************************************    03380000
       100-FORMAT-REPORT-HEADING.                                       03390000
                                                                        03400000
           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.         03410000
                                                                        03420000
           *> MOVE THE RESULT OF THE DATE-TIME FUNCTION TO THE          03430000
           *> DIFFERENT HEADER LINE FIELDS ASSOCIATED WITH THEM         03440000
           *> SO WE CAN INCLUDE THE DATE IN THE OUTPUT HEADER           03450000
           MOVE CD-MONTH   TO HL1-MONTH.                                03460000
           MOVE CD-DAY     TO HL1-DAY.                                  03470000
           MOVE CD-YEAR    TO HL1-YEAR.                                 03480000
           MOVE CD-HOURS   TO HL2-HOURS.                                03490000
           MOVE CD-MINUTES TO HL2-MINUTES.                              03500000
                                                                        03510000
      **************************************************************    03520034
      * READS A LINE OF THE TABLE'S INPUT FILE                     *    03530034
      **************************************************************    03540034
       110-READ-SALESREP-TABLE-RECORD.                                  03550034
           READ INPUT-SALESREP                                          03560034
                AT END                                                  03570034
                    SET SALESREP-EOF TO TRUE.                           03580034
                                                                        03590034
      **************************************************************    03600000
      * CALLS THE PARAGRAPH TO READ A LINE OF THE CUSTOMER RECORD  *    03610000
      * THEN CALLS THE PARAGRAPH TO PRINT THE LINE IF ITS NOT THE  *    03620000
      * TERMINATING LINE OF THE FILE                               *    03630000
      **************************************************************    03640000
       200-PREPARE-SALES-LINES.                                         03650000
                                                                        03660000
           *> GRAB THE NEXT LINE FROM THE CUSTOMER RECORD               03670000
           PERFORM 210-READ-CUSTOMER-RECORD.                            03680000
                                                                        03690000
           *> PERFORMS DUTIES BASED ON THE ENTRY                        03700000
           *>  * IF WE RUN OUT OF DATA PRINT THE SALES AND BRANCH TOTALS03710000
           *>  * IF IT'S THE FIRST RECORD PRINT THE CUSTOMER LINE AND   03720000
           *>    STORE THE CURRENT SALESREP AND BRANCH NUMBER TO THE OLD03730000
           *>  * IF THE BRANCH NUMBER IS GREATER THAN THE CURRENT ONE   03740000
           *>    THEN PRINT THE SALES REP LINE, BRANCH TOTAL LINE, AND  03750000
           *>    THEN THE NEW CUSTOMER'S LINE. AFTER UPDATE THE BRANCH  03760000
           *>    AND SALESREP NUMBERS                                   03770000
           *>  * IF THE SALES REP NUMBER IS GREATER THAN THE CURRENT ONE03780000
           *>    PRINT SALES LINE THEN THE CURRENT CUSTOMER LINE AFTER  03790000
           *>    UPDATE THE SALES REP NUMBER                            03800000
           *>  * IF NOTHING ELSE JUST PRINT THE CUSTOMER RECORD         03810000
           EVALUATE TRUE                                                03820000
               WHEN CUSTMAST-EOF                                        03830000
                   PERFORM 250-PRINT-SALESREP-LINE                      03840000
                   PERFORM 240-PRINT-BRANCH-LINE                        03850000
               WHEN FIRST-RECORD                                        03860000
                   PERFORM 220-PRINT-CUSTOMER-LINE                      03870000
                   SET FIRST-RECORD TO FALSE                            03880000
                 *>MOVE "N" TO FIRST-RECORD-SWITCH                      03890000
                   MOVE CM-SALESREP-NUMBER TO OLD-SALESREP-NUMBER       03900000
                   MOVE CM-BRANCH-NUMBER TO OLD-BRANCH-NUMBER           03910000
               WHEN CM-BRANCH-NUMBER > OLD-BRANCH-NUMBER                03920000
                   PERFORM 250-PRINT-SALESREP-LINE                      03930000
                   PERFORM 240-PRINT-BRANCH-LINE                        03940000
                   PERFORM 220-PRINT-CUSTOMER-LINE                      03950000
                   MOVE CM-SALESREP-NUMBER TO OLD-SALESREP-NUMBER       03960000
                   MOVE CM-BRANCH-NUMBER TO OLD-BRANCH-NUMBER           03970000
               WHEN NOT (CM-SALESREP-NUMBER = OLD-SALESREP-NUMBER)      03980000
                   PERFORM 250-PRINT-SALESREP-LINE                      03990000
                   PERFORM 220-PRINT-CUSTOMER-LINE                      04000000
                   MOVE CM-SALESREP-NUMBER TO OLD-SALESREP-NUMBER       04010000
               WHEN OTHER                                               04020000
                   PERFORM 220-PRINT-CUSTOMER-LINE                      04030000
           END-EVALUATE.                                                04040034
                                                                        04050034
      **************************************************************    04060034
      * LOADS IN THE SALESREP TABLE FROM THE INPUT FILE            *    04070034
      **************************************************************    04080034
       205-LOAD-SALESREP-TABLE.                                         04090034
           PERFORM WITH TEST AFTER                                      04100034
                VARYING SRT-INDEX FROM 1 BY 1                           04110034
                UNTIL SALESREP-EOF OR SRT-INDEX = 100                   04120034
                PERFORM 110-READ-SALESREP-TABLE-RECORD                  04130034
                IF NOT SALESREP-EOF                                     04140034
                     MOVE SM-SALESREP-NUMBER                            04150034
                          TO SALESREP-NUMBER (SRT-INDEX)                04160034
                     MOVE SM-SALESREP-NAME TO SALESREP-NAME (SRT-INDEX) 04170034
                END-IF                                                  04180034
           END-PERFORM.                                                 04190034
                                                                        04200000
      **************************************************************    04210000
      * READS A LINE OF THE INPUT FILE AND IF ITS THE LAST ONE     *    04220000
      * UPDATES THE CUSTOMER-EOF-SWITCH (END-OF-FILE)              *    04230000
      **************************************************************    04240000
       210-READ-CUSTOMER-RECORD.                                        04250000
                                                                        04260000
           READ CUSTMAST                                                04270000
               AT END                                                   04280000
                   MOVE "Y" TO CUSTMAST-EOF-SWITCH.                     04290000
                                                                        04300000
      **************************************************************    04310000
      * PRINTS THE CURRENT CUSTOMER LINE TO THE OUTPUT FILE        *    04320000
      * UPDATES THE LINE COUNTER SO IT KNOWS WHEN IT HAS TO        *    04330000
      * REPRINT THE HEADER LINES FOR A NEW PAGE                    *    04340000
      **************************************************************    04350000
       220-PRINT-CUSTOMER-LINE.                                         04360000
                                                                        04370000
           *> IF INFORMATION WE HAVE PRINTED EXCEEDS THE PAGE LIMIT     04380000
           *> WE REPRINT THE HEADERS FOR THE NEW PAGE                   04390000
           IF LINE-COUNT >= LINES-ON-PAGE                               04400000
               PERFORM 230-PRINT-HEADING-LINES.                         04410000
                                                                        04420000
           *> PERFROMS DUTIES BASED ON THE ENTRY                        04430000
           *>  * IF IT'S THE FIRST RECORD PRINT THE BRANCH NUMBER       04440000
           *>    AND THE SALESREP NUMBER                                04450000
           *>  * IF IT'S A NEW BRANCH PRINT THE BRANCH NUMBER AND       04460000
           *>    SALES REP NUMBER                                       04470000
           *>  * IF IT'S A NEW SALES REP PRINT THE SALESREP NUMBER      04480000
           *>  * OTHERWISE PRINT SPACES IN THOSE LINES FOR PADDING      04490000
           EVALUATE TRUE                                                04500000
               WHEN FIRST-RECORD                                        04510000
                   MOVE CM-BRANCH-NUMBER TO CL-BRANCH-NUMBER            04520000
                   MOVE CM-SALESREP-NUMBER TO CL-SALESREP-NUMBER        04530000
                   PERFORM 223-MOVE-SALESREP-NAME                       04540008
               WHEN CM-BRANCH-NUMBER > OLD-BRANCH-NUMBER                04550000
                   MOVE CM-BRANCH-NUMBER TO CL-BRANCH-NUMBER            04560000
                   MOVE CM-SALESREP-NUMBER TO CL-SALESREP-NUMBER        04570000
                   PERFORM 223-MOVE-SALESREP-NAME                       04580008
               WHEN NOT (CM-SALESREP-NUMBER = OLD-SALESREP-NUMBER)      04590000
                   MOVE CM-SALESREP-NUMBER TO CL-SALESREP-NUMBER        04600000
                   PERFORM 223-MOVE-SALESREP-NAME                       04610008
                   MOVE SPACES TO CL-BRANCH-NUMBER                      04620000
               WHEN OTHER                                               04630000
                   MOVE SPACES TO CL-BRANCH-NUMBER                      04640000
                   MOVE SPACES TO CL-SALESREP-NUMBER                    04650000
                   MOVE SPACES TO CL-SALESREP-NAME                      04660034
           END-EVALUATE.                                                04670000
                                                                        04680000
           *> MOVE THE DATA PULLED FROM THE INPUT FILE INTO THE         04690000
           *> CUSTOMER LINE RECORD FOR LATER OUTPUT                     04700000
           MOVE CM-CUSTOMER-NUMBER  TO CL-CUSTOMER-NUMBER.              04710000
           MOVE CM-CUSTOMER-NAME    TO CL-CUSTOMER-NAME.                04720000
           MOVE CM-SALES-THIS-YTD   TO CL-SALES-THIS-YTD.               04730000
           MOVE CM-SALES-LAST-YTD   TO CL-SALES-LAST-YTD.               04740000
                                                                        04750000
           *> CALCULATE THE DIFFERENCE BETWEEN THIS YEAR'S SALES AND    04760000
           *> AND LAST THEN SAVE THESE RESULT TO CHANGE-AMOUNT AND      04770000
           COMPUTE CHANGE-AMOUNT =                                      04780000
               CM-SALES-THIS-YTD - CM-SALES-LAST-YTD.                   04790000
           MOVE CHANGE-AMOUNT TO CL-CHANGE-AMOUNT.                      04800000
                                                                        04810000
           *> CALCULATE THE PERCENT FOR THE CHANGE IN SALES BETWEEN     04820000
           *> THIS AND LAST YTD, IF THERE WAS NO LAST YEAR SALES        04830000
           *> NUMBER WE MOVE 999.9 TO THE PERECENTAGE SINCE IT'S        04840000
           *> A DIVIDE BY ZERO ERROR OTHERWISE                          04850000
           IF CM-SALES-LAST-YTD = ZERO                                  04860000
               MOVE "  N/A " TO CL-CHANGE-PERCENT-R                     04870000
           ELSE                                                         04880000
               COMPUTE CL-CHANGE-PERCENT ROUNDED =                      04890000
                   CHANGE-AMOUNT * 100 / CM-SALES-LAST-YTD              04900000
                   ON SIZE ERROR                                        04910000
                       MOVE "OVRFLW" TO CL-CHANGE-PERCENT-R.            04920000
                                                                        04930000
           *> PRINT THIS CUSTOMERS INFORMATION TO THE OUTPUT FILE       04940000
           MOVE CUSTOMER-LINE TO PRINT-AREA.                            04950000
           PERFORM 225-WRITE-REPORT-LINE.                               04960000
                                                                        04970000
           *> ADD THIS CUSTOMERS SALES TO THE SALESREP TOTALS           04980000
           ADD CM-SALES-THIS-YTD TO SALESREP-TOTAL-THIS-YTD.            04990000
           ADD CM-SALES-LAST-YTD TO SALESREP-TOTAL-LAST-YTD.            05000000
                                                                        05010008
      **************************************************************    05020008
      * TRIES TO FIND THE SALESREP NAME THAT MATCHES THE CURRENT   *    05030034
      * SALESREP NUMBER AND MOVES IT INTO THE CUSTOMER LINE        *    05040034
      * MOVES "UNKNOWN" IF NO MATCHING SALESREP NUMBER WAS FOUND   *    05050034
      **************************************************************    05060008
       223-MOVE-SALESREP-NAME.                                          05070010
           SET SRT-INDEX TO 1.                                          05080010
           SEARCH SALESREP-GROUP                                        05090010
               AT END                                                   05100010
                   MOVE "UNKNOWN" TO CL-SALESREP-NAME                   05110010
               WHEN SALESREP-NUMBER (SRT-INDEX) = CM-SALESREP-NUMBER    05120010
                   MOVE SALESREP-NAME (SRT-INDEX) TO CL-SALESREP-NAME   05130010
           END-SEARCH.                                                  05140010
                                                                        05150008
      **************************************************************    05160000
      * PRINT ALL THE HEADER LINES TO THE OUTPUT FILE, RAN ONCE    *    05170000
      * FOR EVERY PAGE                                             *    05180000
      **************************************************************    05190000
       225-WRITE-REPORT-LINE.                                           05200000
           WRITE PRINT-AREA.                                            05210000
           ADD 1 TO LINE-COUNT.                                         05220000
                                                                        05230000
      **************************************************************    05240000
      * PRINT ALL THE HEADER LINES TO THE OUTPUT FILE, RAN ONCE    *    05250000
      * FOR EVERY PAGE                                             *    05260000
      **************************************************************    05270000
       230-PRINT-HEADING-LINES.                                         05280000
                                                                        05290000
           *> HEADERS ARE PLACED AT THE START OF EVERY PAGE             05300000
           *> SO WE INCREASE THE PAGE COUNT HERE                        05310000
           ADD 1 TO PAGE-COUNT.                                         05320000
           MOVE PAGE-COUNT     TO HL1-PAGE-NUMBER.                      05330000
                                                                        05340000
           *> PRINT EACH HEADER LINE TO THE OUTPUT FILE                 05350000
           MOVE HEADING-LINE-1 TO PRINT-AREA.                           05360000
           WRITE PRINT-AREA.                                            05370000
           MOVE HEADING-LINE-2 TO PRINT-AREA.                           05380000
           WRITE PRINT-AREA.                                            05390000
           MOVE HEADING-LINE-3 TO PRINT-AREA.                           05400000
           WRITE PRINT-AREA.                                            05410000
           MOVE HEADING-LINE-4 TO PRINT-AREA.                           05420000
           WRITE PRINT-AREA.                                            05430000
           MOVE HEADING-LINE-5 TO PRINT-AREA.                           05440000
           WRITE PRINT-AREA.                                            05450000
           MOVE HEADING-LINE-6 TO PRINT-AREA.                           05460000
           WRITE PRINT-AREA.                                            05470000
                                                                        05480000
           *> RESET THE LINE COUNTER SINCE EVERY HEADER IS THE START    05490000
           *> OF A NEW PAGE                                             05500000
           MOVE ZERO TO LINE-COUNT.                                     05510000
                                                                        05520000
      **************************************************************    05530000
      * PRINTS THE CURRENT BRANCH LINE TOTALS, RAN ONCE FOR EVERY  *    05540000
      * BRANCH. ALSO CALCULATES THE CHANGE IN THE BRANCH           *    05550000
      **************************************************************    05560000
       240-PRINT-BRANCH-LINE.                                           05570000
                                                                        05580000
           *> MOVE THE BRANCH TOTALS TO THE BRANCH TOTAL LINE           05590000
           MOVE BRANCH-TOTAL-THIS-YTD TO BTL-SALES-THIS-YTD.            05600000
           MOVE BRANCH-TOTAL-LAST-YTD TO BTL-SALES-LAST-YTD.            05610000
                                                                        05620000
           *> CALCULATE THE CHANGE BETWEEN THIS-YTD AND LAST            05630000
           *> FOR THE CURRENT BRANCH AND ADD IT TO THE TOTAL LINE       05640000
           COMPUTE CHANGE-AMOUNT =                                      05650000
               BRANCH-TOTAL-THIS-YTD - BRANCH-TOTAL-LAST-YTD.           05660000
           MOVE CHANGE-AMOUNT TO BTL-CHANGE-AMOUNT.                     05670000
                                                                        05680000
           *> CALCULATE THE CHANGE PERCENT BETWEEN YTD'S                05690000
           *> THEN MOVE TO THE BRANCH TOTAL LINE                        05700000
           IF BRANCH-TOTAL-LAST-YTD = ZERO                              05710000
               MOVE "  N/A " TO BTL-CHANGE-PERCENT-R                    05720000
           ELSE                                                         05730000
               COMPUTE BTL-CHANGE-PERCENT ROUNDED =                     05740000
                   CHANGE-AMOUNT * 100 / BRANCH-TOTAL-LAST-YTD          05750000
                   ON SIZE ERROR                                        05760000
                       MOVE "OVRFLW" TO BTL-CHANGE-PERCENT-R.           05770000
                                                                        05780000
           *> PRINT BRANCH LINE                                         05790000
           MOVE BRANCH-TOTAL-LINE TO PRINT-AREA.                        05800000
           PERFORM 225-WRITE-REPORT-LINE.                               05810000
                                                                        05820000
           *> WRITE A BLANK SPACER LINE                                 05830000
           MOVE SPACES TO PRINT-AREA.                                   05840000
           PERFORM 225-WRITE-REPORT-LINE.                               05850000
                                                                        05860000
           *> ADD THE BRANCH TOTALS TO THE GRAND TOTALS                 05870000
           ADD BRANCH-TOTAL-THIS-YTD TO GRAND-TOTAL-THIS-YTD.           05880000
           ADD BRANCH-TOTAL-LAST-YTD TO GRAND-TOTAL-LAST-YTD.           05890000
                                                                        05900000
           *> ZERO OUT THE BRANCH TOTALS                                05910000
           INITIALIZE BRANCH-TOTAL-THIS-YTD                             05920009
                      BRANCH-TOTAL-LAST-YTD.                            05930009
                                                                        05940000
      **************************************************************    05950000
      * PRINTS THE CURRENT SALESREP'S TOTALS, RAN ONCE FOR EVERY   *    05960000
      * SALESREP. ALSO CALCULATES THE CHANGE BETWEEN YEARS         *    05970000
      **************************************************************    05980000
       250-PRINT-SALESREP-LINE.                                         05990000
                                                                        06000000
           *> MOVE THE SALESREP TOTALS TO THE SALESREP TOTAL LINE       06010000
           MOVE SALESREP-TOTAL-THIS-YTD TO STL-SALES-THIS-YTD.          06020000
           MOVE SALESREP-TOTAL-LAST-YTD TO STL-SALES-LAST-YTD.          06030000
                                                                        06040000
           *> CALCULATE THE CHANGE BETWEEN THIS-YTD AND LAST            06050000
           *> FOR THE CURRENT SALESREP AND ADD IT TO THE TOTAL LINE     06060000
           COMPUTE CHANGE-AMOUNT =                                      06070000
               SALESREP-TOTAL-THIS-YTD - SALESREP-TOTAL-LAST-YTD.       06080000
           MOVE CHANGE-AMOUNT TO STL-CHANGE-AMOUNT.                     06090000
                                                                        06100000
           *> CALCULATE THE CHANGE PERCENT BETWEEN YTD'S                06110000
           *> THEN MOVE TO THE SALESREP TOTAL LINE                      06120000
           IF SALESREP-TOTAL-LAST-YTD = ZERO                            06130000
               MOVE "  N/A " TO STL-CHANGE-PERCENT-R                    06140000
           ELSE                                                         06150000
               COMPUTE STL-CHANGE-PERCENT ROUNDED =                     06160000
                   CHANGE-AMOUNT * 100 / SALESREP-TOTAL-LAST-YTD        06170000
                   ON SIZE ERROR                                        06180000
                       MOVE "OVRFLW" TO STL-CHANGE-PERCENT-R.           06190000
                                                                        06200000
           *> PRINT SALESREP LINE                                       06210000
           MOVE SALESREP-TOTAL-LINE TO PRINT-AREA.                      06220000
           PERFORM 225-WRITE-REPORT-LINE.                               06230000
                                                                        06240000
           *> PRINT A SPACER LINE                                       06250000
           MOVE SPACES TO PRINT-AREA.                                   06260000
           PERFORM 225-WRITE-REPORT-LINE.                               06270000
                                                                        06280000
           *> ADD THE SALESREP TOTALS TO THE BRANCH TOTALS              06290000
           *> WHEN A BRANCH IS PRINTED THEN THOSE TOTALS ARE MOVED      06300000
           *> TO THE GRAND TOTALS                                       06310000
           *> CUSTOMER->SALESREP->BRANCH->GRAND-TOTAL                   06320000
           ADD SALESREP-TOTAL-THIS-YTD TO BRANCH-TOTAL-THIS-YTD.        06330000
           ADD SALESREP-TOTAL-LAST-YTD TO BRANCH-TOTAL-LAST-YTD.        06340000
                                                                        06350000
           *> ZERO OUT THE SALESREP TOTALS                              06360000
           MOVE ZERO TO SALESREP-TOTAL-THIS-YTD.                        06370000
           MOVE ZERO TO SALESREP-TOTAL-LAST-YTD.                        06380000
      **************************************************************    06390000
      * PRINTS THE GRAND TOTALS FOR ALL THE CUSTOMERS, RAN ONCE    *    06400000
      * AT THE VERY END OF THE PROGRAM WHEN ALL CUSTOMERS HAVE     *    06410000
      * BEEN PRINTED                                               *    06420000
      **************************************************************    06430000
       300-PRINT-GRAND-TOTALS.                                          06440000
                                                                        06450000
           *> MOVE THE GRAND TOTALS FOR THE SALES TO THE                06460000
           *> OUTPUT LINE FOR GRAND TOTALS                              06470000
           MOVE GRAND-TOTAL-THIS-YTD TO GTL-SALES-THIS-YTD.             06480000
           MOVE GRAND-TOTAL-LAST-YTD TO GTL-SALES-LAST-YTD.             06490000
                                                                        06500000
           *> COMPUTE THE GRAND TOTAL FOR THE CHANGE AMOUNT             06510000
           COMPUTE CHANGE-AMOUNT =                                      06520000
               GRAND-TOTAL-THIS-YTD - GRAND-TOTAL-LAST-YTD.             06530000
           MOVE CHANGE-AMOUNT TO GTL-CHANGE-AMOUNT.                     06540000
                                                                        06550000
           *> CALCULATE THE TOTAL CHANGE IN PERCENT BETWEEN             06560000
           *> THIS YTD AND LAST YTD FOR ALL CUSTOMERS                   06570000
           *> IF THERE WAS NO LAST YEAR FOR ANYONE DEFAULT TO           06580000
           *> A PERCENT OF 999.9 TO AVOID DIVIDE BY ZERO ERROR          06590000
           IF GRAND-TOTAL-LAST-YTD = ZERO                               06600000
               MOVE "  N/A " TO GTL-CHANGE-PERCENT-R                    06610000
           ELSE                                                         06620000
               COMPUTE GTL-CHANGE-PERCENT ROUNDED =                     06630000
                   CHANGE-AMOUNT * 100 / GRAND-TOTAL-LAST-YTD           06640000
                   ON SIZE ERROR                                        06650000
                       MOVE "OVRFLW" TO GTL-CHANGE-PERCENT-R.           06660000
                                                                        06670000
           *> PRINT THE GRAND-TOTAL TO THE OUTPUT FILE                  06680000
           MOVE GRAND-TOTAL-LINE TO PRINT-AREA.                         06690000
           PERFORM 225-WRITE-REPORT-LINE.                               06700000
