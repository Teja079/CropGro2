#=======================================================================
#  ERROR, Subroutine
#  Outputs error messages to screen from file ERROR.DAT
#-----------------------------------------------------------------------
#  Input :
#       ERRKEY CHARACTER*(*)
#       ERRNUM INTEGER
#       FILE   CHARACTER*(*) XFile name
#       LNUM  INTEGER
#  Output: message to screen
#  Local :
#  Ifile : MODEL.ERR
#  FN/SUB: FIND
#=======================================================================
def ERROR(ERRKEY, ERRNUM, FILE, LNUM):
    import os
    from WARNING import WARNING
    from OPHEAD import HEADER
    from ModuleDefs import ControlType, GET_CONTROL, STDPATH, RunConstants

    CONTROL = ControlType()
    CONTROL = GET_CONTROL()

    # Parameters
    MSG = [''] * 10  # array of 10 elements
    IMSG = 1
    EFILE = 'ERROR.OUT'

    try:
        if os.path.exists(EFILE):
            ELUN = open(EFILE, 'a')
        else:
            ELUN = open(EFILE, 'w')
            ELUN.write("*RUN-TIME ERRORS OUTPUT FILE\n")
    except Exception as e:
        print(f"Error opening file {EFILE}: {e}")
        return

    ELUN.close()

    HEADER(RunConstants.SEASINIT, EFILE, CONTROL.RUN)     #This subroutine writes the header
    # Write some control info to ERROR.OUT file
    ELUN = open(EFILE, 'a')

    ELUN.write(f"{CONTROL.FILEX}, Trt {CONTROL.TRTNUM:5d}\n")

    ERRORX = os.getcwd() #gets working directory
    ERRORX = os.path.join(ERRORX,'MODEL.ERR')

    # Check if the error file exists
    if not os.path.exists(ERRORX):
        SAVE_ERRORX = ERRORX
        ERRORX = os.path.join(STDPATH, 'MODEL.ERR')

    # Try to open ERRORX file for reading
    if os.path.exists(ERRORX):
        try:
            LUN = open(ERRORX, 'r')
        except Exception as e:
            print(f"Error opening file {ERRORX}: {e}")
            return

        # Search for error in the file
        FOUND = False
        KEY = 'MISC  '

        if ERRNUM > 6000 or ERRNUM < 0:
            KEY = 'MISC  '
        else:
            KEY = ERRKEY

        while True:
            LINE = LUN.readline()
            if not LINE: #End of file
                if KEY != 'GENERI':
                    KEY = 'GENERI'
                    LUN.seek(0)  # Rewinds to first line of file
                else:
                    break
            AKEY = LINE[:6]
            if AKEY == KEY:
                try:
                    ANUM = int(LINE[7:12])
                    if ANUM == ERRNUM:
                        FOUND = True
                        break
                except ValueError:
                    continue

        if FOUND:
            while True:
                LINE = LUN.readline()
                if LINE.strip() == "":
                    break
                print(LINE)
                ELUN.write(LINE+'\n')
                MSG[IMSG] = f"{LINE.strip():77s}"
                IMSG += 1
        else:
            MSG[IMSG] = f"Unknown ERROR. Error number: {ERRNUM:5d}"
            ELUN.write(f"\n{MSG[IMSG]:78}\n")
            print(f"\n{MSG[IMSG]:78}\n")
            IMSG += 1
        #40
        if FILE == ' ':
            print(f"Error key: {ERRKEY}\n")
            ELUN.write(f"Error key: {ERRKEY}\n")
            MSG[IMSG] = f"Error key: {ERRKEY}"
            IMSG += 1
        else:
            I = min(len(FILE.rstrip()),37)
            FILE_PART = FILE[:I]
            print(f"File: {FILE_PART}   Line: {LNUM:5d}   Error key: {ERRKEY}\n")
            ELUN.write(f"File: {FILE_PART}   Line: {LNUM:5d}   Error key: {ERRKEY}\n")
            MSG[IMSG] = f"File: {FILE_PART}"
            MSG[IMSG + 1] = f"   Line: {LNUM:5d}"
            MSG[IMSG + 2] = f"   Error key: {ERRKEY}"
            IMSG += 3

        LUN.close()
    else: #file at ERRORX not found
        ERRORX = SAVE_ERRORX
        print(f"Could not locate error file: {ERRORX.rstrip()}\n")
        ELUN.write(f"Could not locate error file: {ERRORX.rstrip()}\n")
        MSG[IMSG] = f"Could not locate error file: {ERRORX.rstrip():48}"
        IMSG += 1

        # Simulate generic error message
        LINE = GENERIC_MSG(ERRNUM)
        print(f"\n{LINE[:78]:78}\n")
        ELUN.write(f"\n{LINE[:78]:78}\n")
        MSG[IMSG] = f"{LINE[:78]:78}"
        IMSG += 1

        # Write additional information
        print(f"File: {FILE:12}   Line: {LNUM:5d}   Error key: {ERRKEY}")
        ELUN.write(f"File: {FILE:12}   Line: {LNUM:5d}   Error key: {ERRKEY}\n")
        MSG[IMSG] = f"File: {FILE:12}   Line: {LNUM:5d}   Error key: {ERRKEY}"
        IMSG += 1

    # Final message and file closure
    ELUN.write("Additional information may be available in WARNING.OUT file.")
    print("Additional information may be available in WARNING.OUT file.\n")
    print(' Please press < ENTER > key to continue   ')
    MSG[IMSG] = "Simulations terminated."
    ELUN.close()
    WARNING(IMSG, ERRKEY, MSG)

def GENERIC_MSG(ERRNUM):
    """
    If error messages cannot be found in MODEL.ERR file, or if MODEL.ERR
    file cannot be found, check for generic message type.

    :param ERRNUM: int
    :return MESSAGE: string containing error message and ERRNUM
    """
    if ERRNUM == 29:
        MESSAGE = f"File not found. Please check file name or create file. Error number: {ERRNUM:5d}"
    elif ERRNUM == 33:
        MESSAGE = f"End of file encountered. Error number: {ERRNUM:5d}"
    elif ERRNUM == 59:
        MESSAGE = f"Syntax error. Error number: {ERRNUM:5d}"
    elif ERRNUM == 64:
        MESSAGE = f"Invalid format in file. Error number: {ERRNUM:5d}"
    else:
        MESSAGE = f"Unknown ERROR. Error number: {ERRNUM:5d}"

    return MESSAGE