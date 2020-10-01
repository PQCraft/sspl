$CONSOLE
$SCREENHIDE
_TITLE ""
_CONSOLETITLE ""
FILE$ = COMMAND$(1)
_TITLE FILE$
DIM SHARED EC%
NULLERR
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
DIM SHARED LAP AS _UNSIGNED _BYTE
DIM SHARED LA(0 TO 255) AS LONG
DIM SHARED VAR$(0 TO 255, 0 TO 15)
DIM SHARED VAR%(0 TO 255, 0 TO 255)
DIM SHARED VAR&(0 TO 15, 0 TO 15)
PPOS = 0
PLIN = 0
LAP = -1
_ALLOWFULLSCREEN _OFF , _OFF
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
    A$ = _TRIM$(A$)
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
    CON TRUE
    PRINT CHR$(27); "[1m"; CHR$(27); "[31mE:";
    PRINT CHR$(27); "[0m "; ERRSTR$
    SYSTEM
END SUB
SUB P_HLP
    CON TRUE
    IF INWIN <> 0 THEN
        PRINT "ssrun <ssf_file> [program arguments]"
    ELSE
        PRINT "Usage: ssrun ssf_file [program arguments]"
    END IF
    SYSTEM
END SUB
SUB CON (CONSTAT)
    INCON = CONSTAT
    INDSP = NOT CONSTAT
    IF INCON THEN
        _CONSOLE ON
        _DEST _CONSOLE
    ELSE
        _CONSOLE OFF
    END IF
END SUB
SUB DSP (DSPSTAT)
    INDSP = DSPSTAT
    INCON = NOT DSPSTAT
    IF INDSP THEN
        _SCREENSHOW
        _DEST 0
    ELSE
        _SCREENHIDE
    END IF
END SUB
SUB EXECCMD
    EC% = 255
    ES$ = _TRIM$(C$) + " is not a command."
    IF A$ = CHR$(0) THEN A$ = ""
    SELECT CASE C$
        CASE CHR$(0): NULLERR
        CASE "SET"
            NULLERR
            IF A$ = "" THEN
                ES$ = "Incorrect amount of data."
                EC% = 2
            ELSE
                VA = VAL(GETARG$(1))
                VB = VAL(GETARG$(2))
                VC = VAL(GETARG$(3))
            END IF
            IF VA > 255 THEN
                VA = VA - 256
                VAR&(VA, VB) = VC
            ELSE
                VAR%(VA, VB) = VC
            END IF
        CASE "ADD"
            NULLERR
            IF A$ = "" THEN
                ES$ = "Incorrect amount of data."
                EC% = 2
            ELSE
                VA = VAL(GETARG$(1))
                VB = VAL(GETARG$(2))
                VC = VAL(GETARG$(3))
            END IF
            IF VA > 255 THEN
                VA = VA - 256
                VAR&(VA, VB) = VAR&(VA, VB) + VC
            ELSE
                VAR%(VA, VB) = VAR%(VA, VB) + VC
            END IF
        CASE "SUBT"
            NULLERR
            IF A$ = "" THEN
                ES$ = "Incorrect amount of data."
                EC% = 2
            ELSE
                VA = VAL(GETARG$(1))
                VB = VAL(GETARG$(2))
                VC = VAL(GETARG$(3))
            END IF
            IF VA > 255 THEN
                VA = VA - 256
                VAR&(VA, VB) = VAR&(VA, VB) - VC
            ELSE
                VAR%(VA, VB) = VAR%(VA, VB) - VC
            END IF
        CASE "MULT"
            NULLERR
            IF A$ = "" THEN
                ES$ = "Incorrect amount of data."
                EC% = 2
            ELSE
                VA = VAL(GETARG$(1))
                VB = VAL(GETARG$(2))
                VC = VAL(GETARG$(3))
            END IF
            IF VA > 255 THEN
                VA = VA - 256
                VAR&(VA, VB) = VAR&(VA, VB) * VC
            ELSE
                VAR%(VA, VB) = VAR%(VA, VB) * VC
            END IF
        CASE "DV"
            NULLERR
            IF A$ = "" THEN
                ES$ = "Incorrect amount of data."
                EC% = 2
            ELSE
                VA = VAL(GETARG$(1))
                VB = VAL(GETARG$(2))
                VC = VAL(GETARG$(3))
            END IF
            IF VA > 255 THEN
                VA = VA - 256
                VAR&(VA, VB) = VAR&(VA, VB) / VC
            ELSE
                VAR%(VA, VB) = VAR%(VA, VB) / VC
            END IF
        CASE "EXP"
            NULLERR
            IF A$ = "" THEN
                ES$ = "Incorrect amount of data."
                EC% = 2
            ELSE
                VA = VAL(GETARG$(1))
                VB = VAL(GETARG$(2))
                VC = VAL(GETARG$(3))
            END IF
            IF VA > 255 THEN
                VA = VA - 256
                VAR&(VA, VB) = VAR&(VA, VB) ^ VC
            ELSE
                VAR%(VA, VB) = VAR%(VA, VB) ^ VC
            END IF
        CASE "LPIF"
            NULLERR
            IF TEST(A$) THEN
                PPOS = LA(LAP)
            ELSE
                LAP = LAP - 1
            END IF
        CASE "LOOP"
            NULLERR
            PPOS = LA(LAP)
        CASE "DO"
            NULLERR
            LAP = LAP + 1
            LA(LAP) = PPOS
        CASE "WAIT"
            NULLERR
            _DELAY VAL(A$) / 1000
        CASE "EXIT"
            NULLERR
            SYSTEM
        CASE "CLRS", "CLS", "CLEAR"
            NULLERR
            CLS
        CASE "ECHO"
            NULLERR
            PRINT A$;
        CASE "ECLN"
            NULLERR
            PRINT A$
        CASE "ECHV"
            NULLERR
            VA = VAL(GETARG$(1))
            VB = VAL(GETARG$(1))
            IF VA > 255 THEN
                VA = VA - 256
                VF& = VAR&(VA, VB)
            ELSE
                VF& = VAR%(VA, VB)
            END IF
            PRINT _TRIM$(STR$(VF&));
        CASE "PCHR"
            NULLERR
            IF LEN(A$) > 1 OR LEN(A$) < 1 THEN
                ES$ = "Incorrect amount of data."
                EC% = 2
            ELSE
                PRINT A$;
            END IF
        CASE "PASC"
            NULLERR
            PRINT CHR$(VAL(A$) MOD 256);
        CASE "CONR"
            NULLERR
            PRINT CHR$(27); "[0m";
        CASE "CONB"
            NULLERR
            PRINT CHR$(27); "[1m";
        CASE "CONU"
            NULLERR
            PRINT CHR$(27); "[4m";
        CASE "CONI"
            NULLERR
            PRINT CHR$(27); "[7m";
        CASE "CSCR", "CMODE", "CON"
            NULLERR
            DSP FALSE
            CON TRUE
        CASE "VSCR", "VMODE", "DSP"
            NULLERR
            CON FALSE
            DSP TRUE
        CASE "STTL", "TITLE"
            NULLERR
            _CONSOLETITLE A$
            _TITLE A$
        CASE "CTTL", "CTITLE", "CONTITLE", "CONTTL"
            NULLERR
            _CONSOLETITLE A$
        CASE "DTTL", "DTITLE", "DSPTITLE", "DSPTTL"
            NULLERR
            _TITLE A$
        CASE "FSCR"
            VA~%% = VAL(GETARG$(1))
            SELECT CASE (VA~%% MOD 5)
                CASE 0
                    _FULLSCREEN _OFF
                CASE 1
                    _FULLSCREEN _STRETCH
                CASE 2
                    _FULLSCREEN _STRETCH , _SMOOTH
                CASE 3
                    _FULLSCREEN _SQUAREPIXELS
                CASE 4
                    _FULLSCREEN _SQUAREPIXELS , _SMOOTH
            END SELECT
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
FUNCTION GETARG$ (ANUM)
    IF A$ = "" THEN GETARG$ = "": EXIT FUNCTION
    DIM IIS AS _BIT
    IIS = 0
    CAN = 1
    FOR I = 1 TO LEN(A$)
        CHAR = ASC(A$, I)
        IF CHAR = 34 THEN
            IIS = NOT IIS
        ELSE
            IF CHAR = 32 THEN
                IF IIS THEN
                    IF CAN = ANUM THEN GETARG$ = GETARG$ + CHR$(CHAR)
                ELSE
                    CAN = CAN + 1
                    IF CAN > ANUM THEN EXIT FUNCTION
                END IF
            ELSE
                IF CAN = ANUM THEN GETARG$ = GETARG$ + CHR$(CHAR)
            END IF
        END IF
    NEXT
END FUNCTION
SUB NULLERR
    EC% = 0
END SUB
