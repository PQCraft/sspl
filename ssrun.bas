$CONSOLE
$SCREENHIDE
_TITLE ""
_CONSOLETITLE ""
FILE$ = COMMAND$(1)
_TITLE FILE$
DIM SHARED EC%
EC% = 0
DIM SHARED TRUE AS _BIT
DIM SHARED FALSE AS _BIT
TRUE = -1
FALSE = 0
DIM SHARED DEBUG AS _BIT
DEBUG = FALSE
DIM SHARED INDSP AS _BIT
DSP FALSE
DIM SHARED INCON AS _BIT
CON TRUE
DIM SHARED INWIN AS _BIT
INWIN = INSTR(_OS$, "WINDOWS") <> 0
DIM SHARED C$
C$ = ""
DIM SHARED A$
A$ = ""
_CONSOLETITLE FILE$
CMD$ = COMMAND$
IF (CMD$ = "--help" AND NOT INWIN) OR (CMD$ = "/?" AND INWIN) THEN
    P_HLP
END IF
IF FILE$ = "" THEN
    P_HLP
ELSE
    IF NOT _FILEEXISTS(FILE$) THEN
        P_ERR FILE$ + " does not exist."
    END IF
END IF
OPEN FILE$ FOR BINARY AS #1
DIM SHARED PPOS AS LONG
DIM SHARED PDO AS LONG
DIM SHARED PCHR AS _UNSIGNED _BYTE
DIM SHARED PLIN AS LONG
PPOS = 0
PLIN = 0
DO
    C$ = ""
    A$ = ""
    PLIN = PLIN + 1
    DO WHILE NOT EOF(1)
        PPOS = PPOS + 1
        GET #1, PPOS, PCHR
        IF PCHR = 32 OR EOF(1) THEN
            DO WHILE NOT EOF(1)
                PPOS = PPOS + 1
                GET #1, PPOS, PCHR
                IF EOF(1) THEN EXIT DO
                IF PCHR <> 13 AND PCHR <> 10 THEN
                    A$ = A$ + CHR$(PCHR)
                ELSE
                    EXIT DO
                END IF
            LOOP
        END IF
        IF PCHR <> 13 AND PCHR <> 10 THEN
            C$ = C$ + CHR$(PCHR)
        ELSE
            EXIT DO
        END IF
    LOOP
    C$ = UCASE$(_TRIM$(C$))
    IF DEBUG = -1 THEN PRINT "<"; C$; ">"; "["; A$; "]"
    IF C$ <> "" THEN
        IF ASC(C$, 1) <> 35 THEN
            EXECCMD
        END IF
    END IF
LOOP UNTIL C$ = "EXIT" OR EOF(1)
SYSTEM

'-------SUBS-------

SUB P_ERR (ERRSTR$)
    PRINT CHR$(27); "[31mE: ";
    PRINT CHR$(27); "[0m"; ERRSTR$
    SYSTEM
END SUB
SUB P_HLP
    IF INWIN <> 0 THEN
        PRINT "ssrun <ssf_file> [program arguments]"
    ELSE
        PRINT "Usage: ssrun ssf_file [program arguments]"
    END IF
    SYSTEM
END SUB
SUB CON (CONSTAT)
    INCON = CONSTAT = TRUE
    IF INCON THEN
        _CONSOLE ON
        _DEST _CONSOLE
    ELSE
        _CONSOLE OFF
    END IF
END SUB
SUB DSP (DSPSTAT)
    INDSP = DSPSTAT = TRUE
    IF INCON THEN
        _SCREENSHOW
        _DEST 0
    ELSE
        _SCREENHIDE
    END IF
END SUB
SUB EXECCMD
    EC% = 255
    ES$ = _TRIM$(C$) + " is not a command."
    SELECT CASE C$
        CASE CHR$(0): EC% = 0: ES$ = ""
        CASE "DO"
            EC% = 0
            PDO = PPOS
        CASE "LOOP"
            EC% = 0
            PPOS = PDO
        CASE "WAIT"
            EC% = 0
            _DELAY VAL(A$) / 1000
        CASE "EXIT"
            EC% = 0
            SYSTEM
        CASE "CLRS", "CLS", "CLEAR"
            EC% = 0
            CLS
        CASE "ECHO"
            EC% = 0
            PRINT A$;
        CASE "ECLN"
            EC% = 0
            PRINT A$
        CASE "PCHR"
            EC% = 0
            IF LEN(A$) > 1 OR LEN(A$) < 1 THEN
                ES$ = "Incorrect amount of data."
                EC% = 255
            ELSE
                PRINT A$;
            END IF
        CASE "PASC"
            EC% = 0
            PRINT CHR$(VAL(A$) MOD 256);
    END SELECT
    IF EC% > 0 THEN
        CES$ = "Line " + _TRIM$(STR$(PLIN)) + ": " + ES$ + " (" + _TRIM$(STR$(EC%)) + ")"
        P_ERR CES$
    END IF
END SUB
FUNCTION TEST (ARG$)

END FUNCTION
FUNCTION GETVAL$ (ARG$)

END FUNCTION

