#=======================================================================
#  OPCLEAR, Subroutine
#  Delete *.OUT files
#-----------------------------------------------------------------------
#  REVISION HISTORY
#  Read previous output file names from OUTPUT.LST
#      rather than delete all *.OUT files.
#  If OUTPUT.CDE not available, save *.OUT files in temp directory.
#  If OUTPUT.CDE not available, save *.OUT files in temp directory.
#=======================================================================
def OPCLEAR():
    from WARNING import WARNING
    import os
#      EXTERNAL GETLUN, OUTFILES, WARNING
#     Can't list routine SYSTEM as external because it generates an
#       error with some compilers.
    FileData = OUTFILES()
    if FileData.NumFiles > 0:
        for fname in FileData.FileName:
            if INQUIRE(fname): #Delete if file exists
                os.remove(fname)
    else:
        # Create and run a batch file to replace
        FEXIST = INQUIRE("HEADER.OUT")
        if FEXIST:
            with open("TEMP.BAT","w") as OUTBAT:
                OUTBAT.write("ECHO OFF\n")
                OUTBAT.write("COPY /Y *.OUT *.BAK\n")
                OUTBAT.write("ERASE *.OUT")
            os.system("TEMP.BAT >TEMP.BAK")

            os.remove("TEMP.BAT")
            os.remove("TEMP.BAK")

            MSG = ["OUTPUT.CDE file not found.",
                   "Previous *.OUT files will be saved as *.BAK files."]
            WARNING(2, 'CSM', MSG)
#=======================================================================

def create_str_array(size, fill_value=""):
    """
    Creates an array (list) of strings with a specified size,
    initialized with a default value.

    Args:
        size (int): The desired size of the string array.
        fill_value (str, optional): The value to fill the array with.
                                    Defaults to an empty string.

    Returns:
        list: A list of strings with the given size,
              where each element is initialized with the fill value.
    """
    if not isinstance(size, int) or size <= 0:
        raise ValueError("Size must be a positive integer.")
    return [fill_value] * size

def create_int_array(size, fill_value=0):
    """
    Creates an array (list) of strings with a specified size,
    initialized with a default value.

    Args:
        size (int): The desired size of the string array.
        fill_value (str, optional): The value to fill the array with.
                                    Defaults to an empty string.

    Returns:
        list: A list of strings with the given size,
              where each element is initialized with the fill value.
    """
    if not isinstance(size, int) or size <= 0:
        raise ValueError("Size must be a positive integer.")
    return [fill_value] * size

def create_float_array(size, fill_value=0.0):
    """
    Creates an array (list) of strings with a specified size,
    initialized with a default value.

    Args:
        size (int): The desired size of the string array.
        fill_value (str, optional): The value to fill the array with.
                                    Defaults to an empty string.

    Returns:
        list: A list of strings with the given size,
              where each element is initialized with the fill value.
    """
    if not isinstance(size, int) or size <= 0:
        raise ValueError("Size must be a positive integer.")
    return [fill_value] * size

def line_to_vars(line:str, n_vars):
    """
    Input:
        line: String containing space or tab seperated values
        n_vars: Number of variables that need to be assigned
    Output:
        Returns list of values with correct data types
    """
    char_lst = line.split()
    values = []
    for i in range(len(char_lst)):
        if char_lst[i].isdigit():
            values.append(int(char_lst[i]))
        else:
            values.append(char_lst[i])
    return values


def date_and_time():
    """
    :return:
        date_str: String containg local date in the format YYYYMMDD
        time_str: String containg local time in the format HHMMSS.SSS
        zone_str: String containg time zone in the format +-HHMM from UTC
        date_time: List of integers containg the following values:
                   (1)  The 4-digit year
                   (2)  The month of the year
                   (3)  The day of the month
                   (4)  The time difference with respect to Coordinated Universal Time (UTC) in minutes
                   (5)  The hour of the day (range 0 to 23) - local time
                   (6)  The minutes of the hour (range 0 to 59) - local time
                   (7)  The seconds of the minute (range 0 to 59) - local time
                   (8)  The milliseconds of the second (range 0 to 999) - local time
    """
    from datetime import datetime
    local_now = datetime.now().astimezone()

    date_str = local_now.strftime("%Y%m%d")
    time_str = local_now.strftime("%H%M%S.%f")[:-3]
    zone_str = local_now.strftime("%z")

    date_time = [
        local_now.year, local_now.month, local_now.day,
        int(local_now.utcoffset().total_seconds() // 60),  # Timezone offset in minutes
        local_now.hour, local_now.minute, local_now.second,
        local_now.microsecond // 1000  # Convert microseconds to milliseconds. Operator // divides and floors
    ]

    return date_str, time_str, zone_str, date_time

# input FILE - file name or full path
# output FEXIST
def INQUIRE(FILE):
    import os
    FEXIST = False
    if os.path.exists(FILE):
        FEXIST = True

    return FEXIST

#=======================================================================
#  GET_CROPD, Subroutine
#-----------------------------------------------------------------------
#  INPUT
#       CROP - 2 letter crop code.
#  OUTPUT
#       CROPD - text description of crop
#========================================================================
def GET_CROPD(CROP) :
      # EXTERNAL READ_DETAIL, WARNING
    from WARNING import WARNING
    from READS import READ_DETAIL

    MSG = create_str_array(3)
    ERRKEY = 'CROPD'
    CROPD = ' '

    CROPD = READ_DETAIL(2, 16, CROP, "*Crop and Weed Species")

    if CROPD == ' ' :
        MSG[0] = f"Crop code {CROP} could not be found in file DETAIL.CDE"
        WARNING(1, ERRKEY, MSG)

    return CROPD

#=======================================================================
#  get_dir, Subroutine
#  Subroutine to strip file name from full path and return only the directory
#-----------------------------------------------------------------------
def get_dir(full_path):
    i = full_path.rfind('\\')
    only_dir = full_path[0:i+1]

    return only_dir
#=======================================================================

#=======================================================================
#  OUTFILES, Subroutine
#  Reads available output files from OUTPUT.CDE
#=======================================================================
def OUTFILES():

    from READS import IGNORE
    from WARNING import WARNING
    from fortranformat import FortranRecordReader
    from ModuleDefs import STDPATH, MaxFiles, OutputType, PUT_OUTPUT

    MSG = create_str_array(2)
    f55 = FortranRecordReader('A16,A2,1X,A50,1X,A10')

    # FileData = ModuleDefs.OutputType(FileName=[''])
    # FileData = OutputType(FileName=[''],OPCODE=[''],Description=[''],ALIAS=[''],LUN=[0],NumFiles=0)
    FileData = OutputType(FileName=[], OPCODE=[], Description=[], ALIAS=[], LUN=[], NumFiles=0)

    FILECDE = 'OUTPUT.CDE'
    ERRKEY = 'OUTFLE'

    # Initialize
    # FileData.FileName = []
    # FileData.OPCODE = ''
    # FileData.Description = ''
    # FileData.ALIAS = ''
    # FileData.LUN = 0
    # FileData.NumFiles = 0

    # Does file exist in data directory?
    DATAX = FILECDE
    FEXIST = INQUIRE (DATAX)

    # if not FEXIST :
    #     # File does not exist in data directory, check directory with executable.
    #     # GETARG(0,PATHX)
    #     pathx = sys.argv[1]
    #     datax = get_dir(pathx)
    #     datax = datax.strip() + FILECDE
    #     FEXIST = INQUIRE (DATAX)

    if not FEXIST :
    # Check for file in C:\DSSAT48 directory
        DATAX = STDPATH.strip() + FILECDE
        FEXIST = INQUIRE (DATAX)

    if not FEXIST :
        # get_environment_variable("DSSAT_HOME", DSSAT_HOME)
        DSSAT_HOME = "C:\\DSSAT48\\"
        if DSSAT_HOME.strip() != '':
            STDPATH = DSSAT_HOME.strip()
        DATAX = STDPATH.strip() + FILECDE
        FEXIST = INQUIRE (DATAX)

    if not FEXIST :
        MSG[1] = "OUTPUT.CDE file not found"
        MSG[2] = "Error in subroutine OUTFILES"
        WARNING(2, ERRKEY, MSG)

    if FEXIST :
        LUN = 22

        try:
            # with open(DATAX, 'r') as f:
                I = 0
                LNUM = 0
                while I <= MaxFiles :
                    LNUM, ISECT, CHARTEST = IGNORE(DATAX, LNUM)
                    if ISECT == 0 : break
                    if ISECT != 1 : continue
                    I =  I + 1

                    FileNam, OPCOD, Descript, ALIA = f55.read(CHARTEST)

                    FileData.FileName.append(FileNam)
                    FileData.OPCODE.append(OPCOD)
                    FileData.Description.append(Descript)
                    FileData.ALIAS.append(ALIA)
                    FileData.LUN.append(I + 30)
                FileData.NumFiles = I
                # f.close()
        except IOError as e:
            FEXIST = False

        PUT_OUTPUT(FileData)

    return FileData
#=======================================================================
# Input
#   num real number
# Output
#   the nearest whole number (integer)
def NINT(num):
    """
    Manual implementation simulating Fortran's NINT function.
    Rounds to the nearest integer, with halfway cases rounded away from zero.
    """
    if num >= 0:
        return int(num + 0.5)
    else:
        return int(num - 0.5)
#=======================================================================

# Use as metaclass argument to override __setattr__ function in classes to freeze class level variables.
# Prevents new class level attributes from being created outside of what is declared in class definition
class FrozenMeta(type):
    def __setattr__(cls, name, value):
        if name in cls.__dict__ or name.startswith('__'):
            super().__setattr__(name, value)
        else:
            raise AttributeError(f"Cannot add new attribute '{name}' to {cls.__name__}")


#=======================================================================
  # TABEX, Function
  # Look up utility routine
#-----------------------------------------------------------------------
def TABEX(VAL,ARG,DUMMY,K):

    # INTEGER K,J
    # REAL VAL(K),ARG(K),DUMMY

    for J in range(2,K):
        if DUMMY > ARG[J] :
            continue
        else:
            J = K
            value = (DUMMY-ARG[J-1])*(VAL[J]-VAL[J-1])/ \
                    (ARG[J]-ARG[J-1])+VAL[J-1]
    return value
#=======================================================================





# CROP = 'SR'
# CROPD=GET_CROPD(CROP)
# print(CROPD)

# FileData = OUTFILES()
# print(FileData)
#
# #Testing PUT_OUTPUT and GET_OUTPUT function
# from ModuleDefs import SAVE_data, GET_OUTPUT
#
# print(SAVE_data.OUTPUT)
# print(FileData == SAVE_data.OUTPUT)
# print(FileData == GET_OUTPUT())