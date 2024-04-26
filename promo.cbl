      ****************************************************************** 
      *    
      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. file.
       AUTHOR. PIERRE.

      ****************************************************************** 
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-INPUT
               ASSIGN TO 'input.dat'
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS F-INPUT-STATUS.

           SELECT F-OUTPUT
               ASSIGN TO 'output.dat'
               ACCESS MODE IS SEQUENTIAL
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS F-OUTPUT-STATUS.

      ****************************************************************** 
       DATA DIVISION.
       FILE SECTION.
       FD  F-INPUT
           RECORD CONTAINS 2 TO 1000 CHARACTERS 
           RECORDING MODE IS V.

       01  REC-F-INPUT-2         PIC X(02).

       01  REC-STUDENT.
           03 R-S-KEY            PIC 9(02).       
           03 R-S-LASTNAME       PIC X(07).       
           03 R-S-FIRSTNAME      PIC X(06).       
           03 R-S-AGE            PIC 9(02).       

       01  REC-COURSE.
           03 R-C-KEY            PIC 9(02).       
           03 R-C-LABEL          PIC X(21).       
           03 R-C-COEF           PIC X(03).       
           03 R-C-GRADE          PIC X(05).

       FD  F-OUTPUT
           RECORD CONTAINS 2000 CHARACTERS
           RECORDING MODE IS F.
       01  REC-F-OUTPUT        PIC X(2000).

      ******************************************************************
       WORKING-STORAGE SECTION.
       01  F-INPUT-STATUS      PIC X(02) VALUE SPACE.
           88 F-INPUT-STATUS-OK    VALUE '00'.        
           88 F-INPUT-STATUS-EOF   VALUE '10'.

       01  F-OUTPUT-STATUS     PIC X(02) VALUE SPACE.
           88 F-OUTPUT-STATUS-OK    VALUE '00'.        
           88 F-OUTPUT-STATUS-EOF   VALUE '10'.

       01  DATA-STUDENT.
           03 STUDENT-LGTH     PIC 9(03) VALUE 1.
           03 STUDENT  
               OCCURS 1 TO 999 TIMES
               DEPENDING ON STUDENT-LGTH
               INDEXED BY IDX-STUDENT.
                   05 S-LASTNAME   PIC X(20).
                   05 S-FIRSTNAME  PIC X(20).
                   05 S-AGE        PIC 9(02).

       01  DATA-COURSE.
           03 COURSE-LGTH     PIC 9(03) VALUE 1.
           03 COURSE
               OCCURS 1 TO 999 TIMES
               DEPENDING ON COURSE-LGTH
               INDEXED BY IDX-COURSE. 
                   05 C-COEF       PIC 9V9.
                   05 C-LABEL      PIC X(25).

       01  DATA-GRADE.
           03 GRADE-LGTH      PIC 9(03) VALUE 1.
           03 GRADE
               OCCURS 1 TO 999 TIMES
               DEPENDING ON GRADE-LGTH
               INDEXED BY IDX-GRADE. 
                   05 G-S-FULLNAME     PIC X(40).
                   05 G-C-LABEL        PIC X(25).
                   05 G-GRADE          PIC 99V99.
       01  WS-BUFFER   PIC X(03) VALUE SPACE.
           88  WS-VALUE-NOT-PRESENT VALUE 'Y'.

       01  WS-PNT.
           03 WS-PNT-NBR      PIC Z9.
           03 WS-PNT-GRADE    PIC Z9,99.
           03 WS-PNT-COEF     PIC 9,9.

       01  WS-COURSE-INFO        PIC X(50) VALUE SPACES.
       01  WS-POS                PIC 9(03) VALUE 13.
       01  WS-ALL-GRADES         PIC X(200) VALUE SPACES.
       01  WS-NOTE-COUNT         PIC 9.
       01  WS-TOTAL-GRADES      PIC 9(05)V99 VALUE ZERO.
       01  WS-GRADE-COUNT       PIC 9(03) VALUE ZERO.
       01  WS-AVERAGE           PIC 9(02)V99 VALUE ZERO.
       01  WS-TEST              PIC 9(3)V99.
       01  WS-TEST-2            PIC 9(3)V99.
       01  WS-TOTAL-COEFS       PIC 9(3)V99.
       01  WS-MOYENNEG          PIC 9(3)V99.
       01  WS-PNT-MOYENNE       PIC 99V99.
       01  WS-MOYENNE-C1         PIC 9(3)V99.
       01  WS-PNT-C1       PIC Z99,99.
       01  WS-MOYENNE-C2          PIC 9(3)V99.
       01  WS-PNT-C2       PIC 99V99.
       01  WS-MOYENNE-C3          PIC 9(3)V99.
       01  WS-PNT-C3       PIC 99V99.
       01  WS-MOYENNE-C4          PIC 9(3)V99.
       01  WS-PNT-C4       PIC 99V99.
       01  WS-MOYENNE-C5          PIC 9(3)V99.
       01  WS-PNT-C5       PIC 99V99.
       01  WS-MOYENNE-C6          PIC 9(3)V99.
       01  WS-PNT-C6       PIC 99V99.


       PROCEDURE DIVISION.
       1000-MAIN-START.
           PERFORM 7000-READ-START THRU 7000-READ-END. 

           DISPLAY G-S-FULLNAME(1).
           DISPLAY G-S-FULLNAME(10).


           PERFORM 7100-WRITE-START THRU 7100-WRITE-END.
       1000-MAIN-END.
           STOP RUN.
      ****************************************************************** 
       7000-READ-START.
           OPEN INPUT F-INPUT.          

           IF NOT F-INPUT-STATUS-OK
               DISPLAY 'ERROR INPUT FILE'
               GO TO 7000-READ-END
           END-IF.

           PERFORM UNTIL F-INPUT-STATUS-EOF
               READ F-INPUT
               IF F-INPUT-STATUS-EOF
                   GO TO 7000-READ-END
               END-IF
               EVALUATE REC-F-INPUT-2
                   WHEN '01'
                       PERFORM 8010-HANDLE-STUDENT-START 
                           THRU 8010-HANDLE-STUDENT-END
                   WHEN '02'
                       PERFORM 8020-HANDLE-COURSE-START 
                           THRU 8020-HANDLE-COURSE-END
                       PERFORM 8030-HANDLE-GRADE-START
                           THRU 8030-HANDLE-GRADE-END
           END-PERFORM.

       7000-READ-END.
           SET GRADE-LGTH COURSE-LGTH STUDENT-LGTH DOWN BY 1.
           CLOSE F-INPUT.  
      ******************************************************************
       7100-WRITE-START.
           OPEN OUTPUT F-OUTPUT.
           PERFORM 9010-HEADER-START   THRU 9010-HEADER-END.

           PERFORM 9030-BODY-START     THRU 9030-BODY-END.

           PERFORM 9020-FOOTER-START   THRU 9020-FOOTER-END.
       7100-WRITE-END.
           CLOSE F-OUTPUT.
      ******************************************************************  
       8010-HANDLE-STUDENT-START.
           MOVE R-S-FIRSTNAME  TO S-FIRSTNAME(STUDENT-LGTH).
           MOVE R-S-LASTNAME   TO S-LASTNAME(STUDENT-LGTH).
           MOVE R-S-AGE        TO S-AGE(STUDENT-LGTH).

           SET STUDENT-LGTH UP BY 1.           
       8010-HANDLE-STUDENT-END.
      *****************************************************************s* 
       8020-HANDLE-COURSE-START.
           INITIALIZE WS-BUFFER.
           SET IDX-COURSE TO 1.

           SEARCH COURSE VARYING IDX-COURSE
               AT END
                   SET WS-VALUE-NOT-PRESENT TO TRUE
               WHEN C-LABEL(IDX-COURSE) = R-C-LABEL
                   GO TO 8020-HANDLE-COURSE-END 
           END-SEARCH.

           IF WS-VALUE-NOT-PRESENT
               MOVE R-C-COEF   TO C-COEF(COURSE-LGTH)
               MOVE R-C-LABEL  TO C-LABEL(COURSE-LGTH)
               SET COURSE-LGTH UP BY 1
           END-IF.
       8020-HANDLE-COURSE-END.
      ****************************************************************** 
       8030-HANDLE-GRADE-START.
           STRING 
               S-FIRSTNAME(STUDENT-LGTH - 1) 
               S-LASTNAME(STUDENT-LGTH - 1) 
               DELIMITED BY SIZE 
           INTO G-S-FULLNAME(GRADE-LGTH).

           MOVE R-C-LABEL TO G-C-LABEL(GRADE-LGTH).
           MOVE R-C-GRADE TO G-GRADE(GRADE-LGTH).

           SET GRADE-LGTH UP BY 1.
       8030-HANDLE-GRADE-END.
      ****************************************************************** 
       9010-HEADER-START.
           INITIALIZE REC-F-OUTPUT.
           MOVE ALL '*' TO REC-F-OUTPUT(1:200).
           WRITE REC-F-OUTPUT.
 
           INITIALIZE REC-F-OUTPUT.
           MOVE '*' TO REC-F-OUTPUT(1:1).
           MOVE '*' TO REC-F-OUTPUT(200:200).
           MOVE 'BULLETIN DE NOTES' TO REC-F-OUTPUT(90:17).
           WRITE REC-F-OUTPUT.

           INITIALIZE REC-F-OUTPUT.
           MOVE ALL '*' TO REC-F-OUTPUT(1:200).
           WRITE REC-F-OUTPUT.

           INITIALIZE REC-F-OUTPUT.
           MOVE '| Eleve      |' TO REC-F-OUTPUT(1:14).
           MOVE ' MOYENNE GENERALE |' TO REC-F-OUTPUT(15:19)
           MOVE 35 TO WS-POS.
           PERFORM VARYING IDX-COURSE FROM 1 BY 1 UNTIL IDX-COURSE > 
                           COURSE-LGTH
              MOVE C-COEF(IDX-COURSE) TO WS-PNT-COEF
              INITIALIZE WS-COURSE-INFO
              STRING C-LABEL(IDX-COURSE) DELIMITED BY SIZE
                     ' Coef. ' DELIMITED BY SIZE
                     WS-PNT-COEF DELIMITED BY SIZE
                     ' | ' DELIMITED BY SIZE
              INTO WS-COURSE-INFO
              MOVE WS-COURSE-INFO TO REC-F-OUTPUT(WS-POS:)
              COMPUTE WS-POS = WS-POS + FUNCTION LENGTH(WS-COURSE-INFO) 
                      + 1

           END-PERFORM
           WRITE REC-F-OUTPUT.
 
           INITIALIZE REC-F-OUTPUT.
           MOVE ALL '_' TO REC-F-OUTPUT(1:200).
           WRITE REC-F-OUTPUT.
       9010-HEADER-END.
      ******************************************************************
       9020-FOOTER-START.
           INITIALIZE REC-F-OUTPUT.
           MOVE ALL '*' TO REC-F-OUTPUT(1:200).
           WRITE REC-F-OUTPUT.

           INITIALIZE REC-F-OUTPUT.
           MOVE '*' TO REC-F-OUTPUT(1:1).
           MOVE '*' TO REC-F-OUTPUT(200:1).
           MOVE 'NOMBRE DE' TO REC-F-OUTPUT(33:9).

           INITIALIZE REC-F-OUTPUT(43:9).
           MOVE 'ELEVES'   TO REC-F-OUTPUT(43:9).
           MOVE STUDENT-LGTH TO WS-PNT-NBR.
           MOVE FUNCTION TRIM(WS-PNT-NBR) TO REC-F-OUTPUT(50:2).
           WRITE REC-F-OUTPUT.

           INITIALIZE REC-F-OUTPUT(43:9).
           MOVE 'NOTES'    TO REC-F-OUTPUT(43:9).
           MOVE GRADE-LGTH TO WS-PNT-NBR.
           MOVE FUNCTION TRIM(WS-PNT-NBR) TO REC-F-OUTPUT(50:2).
           WRITE REC-F-OUTPUT.

           INITIALIZE REC-F-OUTPUT(43:9).
           MOVE 'COURS'     TO REC-F-OUTPUT(43:9).
           MOVE COURSE-LGTH TO WS-PNT-NBR.
           MOVE FUNCTION TRIM(WS-PNT-NBR) TO REC-F-OUTPUT(50:2).
           WRITE REC-F-OUTPUT.

           INITIALIZE REC-F-OUTPUT.
           MOVE '*' TO REC-F-OUTPUT(1:1).
           MOVE '*' TO REC-F-OUTPUT(200:1).
           MOVE 'MOYENNE DE ' TO REC-F-OUTPUT(33:10).

           INITIALIZE REC-F-OUTPUT(44:2).
           MOVE 'GN'     TO REC-F-OUTPUT(44:2).
           COMPUTE WS-PNT-MOYENNE = WS-MOYENNEG / 7
           MOVE FUNCTION TRIM(WS-PNT-MOYENNE) TO REC-F-OUTPUT(50:8).
           WRITE REC-F-OUTPUT.

           INITIALIZE REC-F-OUTPUT(44:2).
           MOVE 'C1'     TO REC-F-OUTPUT(44:2).
           COMPUTE WS-PNT-C1 = WS-MOYENNE-C1 / 7
           MOVE FUNCTION TRIM(WS-PNT-C1) TO REC-F-OUTPUT(50:2).
           WRITE REC-F-OUTPUT.

           INITIALIZE REC-F-OUTPUT(44:2).
           MOVE 'C2'     TO REC-F-OUTPUT(44:2).
           COMPUTE WS-PNT-C2 = WS-MOYENNE-C2 / 7
           MOVE FUNCTION TRIM(WS-PNT-C2) TO REC-F-OUTPUT(50:2).
           WRITE REC-F-OUTPUT.

           INITIALIZE REC-F-OUTPUT(44:2).
           MOVE 'C3'     TO REC-F-OUTPUT(44:2).
           COMPUTE WS-PNT-C3 = WS-MOYENNE-C3 / 7
           MOVE FUNCTION TRIM(WS-PNT-C3) TO REC-F-OUTPUT(50:2).
           WRITE REC-F-OUTPUT.
           
           INITIALIZE REC-F-OUTPUT(44:2).
           MOVE 'C4'     TO REC-F-OUTPUT(44:2).
           COMPUTE WS-PNT-C4 = WS-MOYENNE-C4 / 7
           MOVE FUNCTION TRIM(WS-PNT-C4) TO REC-F-OUTPUT(50:2).
           WRITE REC-F-OUTPUT.

           INITIALIZE REC-F-OUTPUT(44:2).
           MOVE 'C5'     TO REC-F-OUTPUT(44:2).
           COMPUTE WS-PNT-C1 = WS-MOYENNE-C5 / 7
           MOVE FUNCTION TRIM(WS-PNT-C5) TO REC-F-OUTPUT(50:2).
           WRITE REC-F-OUTPUT.

           INITIALIZE REC-F-OUTPUT(44:2).
           MOVE 'C6'     TO REC-F-OUTPUT(44:2).
           COMPUTE WS-PNT-C6 = WS-MOYENNE-C6 / 7
           MOVE FUNCTION TRIM(WS-PNT-C6) TO REC-F-OUTPUT(50:2).
           WRITE REC-F-OUTPUT.

           INITIALIZE REC-F-OUTPUT.
           MOVE ALL '*' TO REC-F-OUTPUT(1:200).
           WRITE REC-F-OUTPUT.
       9020-FOOTER-END.
      ******************************************************************              
       9030-BODY-START.
           INITIALIZE REC-F-OUTPUT.
           MOVE 0 TO WS-NOTE-COUNT.
           MOVE 42 TO WS-POS.
           MOVE 0 TO WS-TOTAL-GRADES.
           MOVE 0 TO WS-TOTAL-COEFS.
           PERFORM VARYING IDX-GRADE FROM 1 BY 1 UNTIL IDX-GRADE > 
               GRADE-LGTH
               MOVE G-GRADE(IDX-GRADE) TO WS-PNT-GRADE
               MOVE WS-PNT-GRADE TO WS-TEST

               EVALUATE WS-NOTE-COUNT + 1  
                   WHEN 1
                       MOVE 1,0 TO WS-TEST-2
                       COMPUTE WS-MOYENNE-C1 = WS-TEST + WS-MOYENNE-C1 
                   WHEN 2
                       MOVE 1,0 TO WS-TEST-2
                       COMPUTE WS-MOYENNE-C2 = WS-TEST + WS-MOYENNE-C2
                   WHEN 3
                       MOVE 2,0 TO WS-TEST-2 
                       COMPUTE WS-MOYENNE-C3 = WS-TEST + WS-MOYENNE-C3
                   WHEN 4
                       MOVE 2,0 TO WS-TEST-2
                       COMPUTE WS-MOYENNE-C4 = WS-TEST + WS-MOYENNE-C4
                   WHEN 5
                       MOVE 1,0 TO WS-TEST-2
                       COMPUTE WS-MOYENNE-C5 = WS-TEST + WS-MOYENNE-C5
                   WHEN 6
                       MOVE 1,5 TO WS-TEST-2  
                       COMPUTE WS-MOYENNE-C6 = WS-TEST + WS-MOYENNE-C6
                   WHEN OTHER
                       MOVE 1,0 TO WS-TEST-2
               END-EVALUATE

               COMPUTE WS-TEST = WS-TEST * WS-TEST-2
               ADD WS-TEST TO WS-TOTAL-GRADES
               ADD WS-TEST-2 TO WS-TOTAL-COEFS
               ADD 1 TO WS-GRADE-COUNT
               ADD 1 TO WS-NOTE-COUNT

               STRING G-S-FULLNAME(IDX-GRADE) DELIMITED BY SIZE 
                     ' | ' DELIMITED BY SIZE
                     INTO REC-F-OUTPUT(1:40)
               STRING WS-PNT-GRADE DELIMITED BY SIZE ' | ' 
                     DELIMITED BY SIZE
                     INTO REC-F-OUTPUT(WS-POS:)
               COMPUTE WS-POS = WS-POS + FUNCTION LENGTH(WS-PNT-GRADE)
                    + 3

               IF WS-NOTE-COUNT = 6
                   COMPUTE WS-AVERAGE = WS-TOTAL-GRADES / WS-TOTAL-COEFS
                   MOVE WS-AVERAGE TO WS-PNT-GRADE
                   COMPUTE WS-MOYENNEG = WS-AVERAGE + WS-MOYENNEG 
                   STRING WS-PNT-GRADE DELIMITED BY SIZE INTO 
                       REC-F-OUTPUT(WS-POS:)
                   WRITE REC-F-OUTPUT
                   INITIALIZE REC-F-OUTPUT
                   MOVE 0 TO WS-NOTE-COUNT
                   MOVE 42 TO WS-POS
                   MOVE 0 TO WS-TOTAL-GRADES
                   MOVE 0 TO WS-TOTAL-COEFS  
                   MOVE 0 TO WS-GRADE-COUNT
               END-IF
           END-PERFORM

           IF WS-NOTE-COUNT > 0
               COMPUTE WS-AVERAGE = WS-TOTAL-GRADES / WS-TOTAL-COEFS
               STRING WS-AVERAGE DELIMITED BY SIZE INTO
                    REC-F-OUTPUT(WS-POS:)
               WRITE REC-F-OUTPUT
           END-IF.
       9030-BODY-END.

      ******************************************************************
