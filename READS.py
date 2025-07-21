#Library of read function used in model
import numpy as np

#from FreshWt import OUTPUT
#from INPUT_SUB import INPUT_SUB

#Suppress scientific notation when printing numpy arrays
np.set_printoptions(suppress=True)

#Create dictionaries when reading tables. Parameter name as key. ecotype, etc

def find(lines:list, section: str, targ_inst:int = 1):
    """
    Searches for a specific section in an array of lines.
    Parameters:
    - lines: A list of strings, each representing a line from a file.
    - section: A string representing the section header to search for
    Returns:
    - lnum: The line number where the section is found.
    - found: A boolean indicating whether the section was found.

    Raises:
    - ValueError: If the section is not found in the lines.
    """
    found = False
    inst = 0
    for lnum, line in enumerate(lines):
        if line.strip().startswith(section):
            inst += 1
            if inst == targ_inst:
                found = True
                return lnum, found  # Return the line number and found status

    # Raise an error if the section is not found
    raise ValueError(f"Section {section} not found in the provided lines.")


def error(errkey, errcode, filename, linenum):
    error_message = (f"Error in {errkey}: Error code {errcode} occurred in file '{filename}' "
                     f"at line {linenum}.")
    raise Exception(error_message)


def ignore(lines:list, linexp:int):
    """
    Reads lines from a list and checks for blank lines, comment lines,
    or end-of-section lines.

    Parameters:
    - lines: List of strings, where each string represents a line from a file.
    - linexp: Integer, starting line number at which this routine begins to read the file.

    Returns:
    - linexp: Integer, line number last read by the routine.
    - isect: Integer indicator of completion of IGNORE routine:
        - 0: End of file encountered.
        - 1: Found a good line to read.
        - 2: End of section in file encountered, denoted by * or $ in column 1.
    """
    isect = 0

    for lnum in range(linexp, len(lines)):
        # Read the current line
        chartest = lines[lnum].strip()
        # Check for end of section (denoted by * or $ in the first column)
        if chartest.startswith('*') or chartest.startswith('$'):
            isect = 2
            return lnum, isect
        # Check for non-blank lines and lines not starting with comment characters (!, @)
        elif not chartest.startswith('!') and not chartest.startswith('@') and chartest != '':
           isect = 1
           return lnum, isect # Good line found
    #EOF
    return len(lines), isect

def IGNORE(LUN:str, LINEXP:int):
    """
    Reads lines from a list and checks for blank lines, comment lines,
    or end-of-section lines.

    Parameters:
    - LUN: file name or full path.
    - LINEXP: starting line number at which this routine begins to read the file.

    Returns:
    - LINEXP: line number last read by the routine.
    - ISECT: Integer indicator of completion of IGNORE routine:
        - 0: End of file encountered.
        - 1: Found a good line to read.
        - 2: End of section in file encountered, denoted by * or $ in column 1.
    - CHARTEST - n-character variable containing the contents of
          the last line read by the IGNORE routine
    """

    ISECT = 1

    with open(LUN) as f:
        for i in range(0, LINEXP):
            next(f)

        while True:
            CHARTEST = f.readline()
            LINEXP = LINEXP + 1

            if len(CHARTEST) > 0:
                # Check to see if all of this section has been read
                if CHARTEST.startswith('*') or CHARTEST.startswith('$'):
                    # End of section encountered
                    ISECT = 2
                    return LINEXP, ISECT, CHARTEST

                # Check for blank lines and comments (denoted by ! in column 1)
                elif CHARTEST[0] != '!' and CHARTEST[0] != '@' and CHARTEST != "\n":
                    return LINEXP, ISECT, CHARTEST

            else: #EOF reached
                ISECT = 0
                return LINEXP, ISECT, CHARTEST

def is_float(s:str):
    """
    Input: String
    Return True if str is a number. Other-wise return False.
    """
    try:
        float(s)
    except ValueError:
        return False
    return True

def strToType(s:str):
    """
        Casts string value to appropriate data type
        Input: String
        Returns int, float, boolean or string
    """
    if s.isdigit():
        return int(s)
    elif is_float(s):
        return float(s)
    elif s == 'TRUE' or s == 'FALSE':
        return s == 'TRUE'
    return s

def findlevel(lines:list, lnum:int, targetlvl:int = 1):
    # Finds desired level within section of file
    lnum+=1
    lnum,isect = ignore(lines,lnum)
    if isect!=1:
        return -1
    while int(lines[lnum][1]) != targetlvl:
        if lines[lnum][0] == '*' or lines[lnum][0] == '@' or lines[lnum][1]== '':
            # Treatment # not found
            return -1
        lnum += 1
    return lnum

# Read Experiment File _____________________________
def readX(fpath:str, xcode:str, crop:str, exp_dict:dict):
    """
    Inputs:
      fpath: Folder directory path. String
      xcode: 8 character experiment code
      crop: 2 character crop code
      exp_dict: empty dictionary containing treatment number
    Output:
      exp_dict: Updated dictionary with parameter read in from X file.
                Treatment factor levels, planting details, and cultivar code.
    """

    # Create file path to fileX. Open and split by rows into a list.
    filex = fpath+xcode+"."+crop+"X"
    try:
        with (open(filex, 'r') as filex):
            lines = filex.readlines()
    except IOError as e:
        error(1, e.errno, filex, 0)
        return

    # Read Treatment section of X file
    section = "*TREATMENT"
    lnum,found = find(lines,section)
    if not found:
        return "Treatment section not found"
    lnum += 1
    # Get columns headers for dictionary keys
    paramlist = lines[lnum][35:].split()

    # Find treatment level
    lnum = findlevel(lines,lnum,exp_dict['trtNum'])
    if lnum<0:
        return "trt level not found"
    # Read cultivar and plant level from treatment row and update dictionary
    trt_factors = lines[lnum][35:].split()
    for i in range(len(paramlist)):
        exp_dict[paramlist[i]] = int(trt_factors[i])

    if crop != 'FA':
        # Find and read in 'PLANT' section
        section = '*PLANT'
        lnum, found = find(lines, section)
        if not found:
            error(section, 42, filex, lnum)
            return
        lnum+=1
        # Get columns headers for dictionary keys
        paramlist = lines[lnum].split()
        # Find correct level/row in plant section
        lnum = findlevel(lines, lnum, exp_dict['MP'])
        if lnum < 0:
            return "trt level not found"
        # Split target row into a list of strings. Convert from string to correct data type and assign to dictionary.
        valuelist = lines[lnum].split()
        for i in range(len(paramlist)):
            exp_dict[paramlist[i]] = strToType(valuelist[i])

        # Find and read section cultivar section in the file
        section = '*CULTI'
        lnum, found = find(lines, section)
        if not found:
            error(section, 42, filex, lnum)
            return
        # Find correct level in culti section
        lnum = findlevel(lines, lnum, exp_dict['CU'])
        if lnum < 0:
            return "trt level not found"
        # Read crop code and update dictionary.
        valuelist = lines[lnum].split()
        exp_dict['cult_code'] = valuelist[2]
    return 0


# Read Cultivar File _____________________________
def readCUL(crop:str, culCode:str, modelCode:str, cul_dict:dict):
    """
    Inputs:
      crop: 2 character crop code
      culCode: 6 character cultivar code
      modelCode: 3 character code for crop model
      cul_dict: empty dictionary
    Output:
      cul_dict: Updated dictionary with parameter read in from CUL file.

    """
    # Create file path. Read and split rows into list.
    filec = 'C:/DSSAT48/Genotype/'+crop+modelCode+'048.CUL'
    try:
        with (open(filec, 'r') as filec):
            lines = filec.readlines()
    except IOError as e:
        error(1, e.errno, filec, 0)
        return
    # Find headers row and get list of keys for dictionary
    lnum, found = find(lines, '@VAR#')
    paramlist = lines[lnum][30:].split()

    # Find selected cultivar row. Update cultivar dictionary
    lnum, found = find(lines, culCode)
    if not found:
        return "Cultivar not found"
    valuelist = lines[lnum][30:].split()
    for i in range(len(paramlist)):
        cul_dict[paramlist[i]] = strToType(valuelist[i])

# Read Experiemnt File with pandas _____________________________
# def readCULdf(crop,culCode,modelCode):
#     filec = 'C:/DSSAT48/Genotype/'+crop+modelCode+'048.CUL'
#     return pd.read_fwf(filec, widths = (7,23,7,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6), skiprows=38)


# Read species file _______________________________________
def readSPE(crop:str, modelCode:str, NPHS:int, #Inputs
            spe_dict:dict,leaf_dict:dict,CT1:dict,CT2:dict,CT3:dict,
            resp_dict:dict, plant_dict): # dictionaries to update
    """
    Inputs:
      crop: 2 character crop code
      modelCode: 3 character code for crop model
      NPHS: Number of phenological stages
      spe_dict: Dictionary containing empty arrays for nprior,dltyp,ctmp,tselc,wsenp,nsenp,psenp
      CT1,CT2,CT3: Dictionary containing empty arrays for TB,TO1,TO2,TM for three main stages
    Output:
      Fill array values for spe_dict,CT1,CT2,CT3 fields.
    """
    nprior, dltyp, ctmp, tselc, wsenp, nsenp, psenp =  (spe_dict['nprior'],spe_dict['dltyp'],spe_dict['ctmp'],
                                        spe_dict['tselc'],spe_dict['wsenp'],spe_dict['nsenp'],spe_dict['psenp'])

    filecc = 'C:/DSSAT48/Genotype/' + crop + modelCode + '048.SPE'
    try:
        with open(filecc, 'r') as file:
            lines = file.readlines()
    except IOError as e:
        error(1, e.errno, filecc, 0)
        return

    # Find Leaf Growth Parameters in FILEC, .SPE file
    section = '!*LEAF'
    lnum, found = find(lines, section)
    if not found:
        error(section, 42, filecc, lnum)
        return
    # Read data under this section
    lnum += 1
    leaf_details = lines[lnum].split()
    leaf_dict['FINREF'],leaf_dict['SLAREF'],leaf_dict['SIZREF'],leaf_dict['VSSINK'],leaf_dict['EVMODC']=(
        strToType(i) for i in leaf_details[0:5])
    lnum+=1
    leaf_details = lines[lnum].split()
    leaf_dict['SLAMAX'], leaf_dict['SLAMIN'], leaf_dict['SLAPAR'], leaf_dict['TURSLA'], leaf_dict['NSLA']=(
        strToType(i) for i in leaf_details[0:5])

    # Find phenology parameters in FILEC, .SPE file
    section = '!*PHEN'
    lnum, found = find(lines, section)
    if not found:
        error(section, 42, filecc, lnum)
        return
    # Look for data under this section. Ignore comment lines
    lnum, isect = ignore(lines, lnum)
    phen_details = lines[lnum].split()
    lnum += 1
    CT1['TB'],CT1['TO1'],CT1['TO2'],CT1['TM'] = (float(phen_details[0]), float(phen_details[1]) #TODO change to CTV1,CTR1, CTR2
                                        , float(phen_details[2]), float(phen_details[3]))
    lnum, isect = ignore(lines, lnum)
    phen_details = lines[lnum].split()
    lnum += 1
    CT2['TB'],CT2['TO1'],CT2['TO2'],CT2['TM'] = (float(phen_details[0]), float(phen_details[1])
                                    , float(phen_details[2]), float(phen_details[3]))
    lnum, isect = ignore(lines, lnum)
    phen_details = lines[lnum].split()
    lnum += 1
    CT3['TB'],CT3['TO1'],CT3['TO2'],CT3['TM'] = (float(phen_details[0]), float(phen_details[1])
                                    , float(phen_details[2]), float(phen_details[3]))

    for i in range(0, NPHS):
        lnum, isect = ignore(lines, lnum)
        phen_details = lines[lnum].split()
        lnum += 1
        j = int(phen_details[0]) - 1
        nprior[j], dltyp[j], ctmp[j], tselc[j], wsenp[j], nsenp[j], psenp[j] = (
        strToType(i) for i in phen_details[1:8])

    # Find Respiration Parameters. Needed by DEMAND
    section = '!*RESP'
    lnum, found = find(lines, section)
    if not found:
        error(section, 42, filecc, lnum)
        return
    lnum += 1
    resp_details = lines[lnum].split()
    resp_dict['RES30C'], resp_dict['R30C2']=(strToType(i) for i in resp_details[0:2])
    lnum+=1
    resp_details = lines[lnum].split()
    resp_dict['RNO3C'], resp_dict['RNH4C'], resp_dict['RPRO'], resp_dict['RFIXN']=\
        (strToType(i) for i in resp_details[0:4])
    lnum += 1
    resp_details = lines[lnum].split()
    (resp_dict['RCH20'], resp_dict['RLIP'], resp_dict['RLIG'], resp_dict['ROA'],
     resp_dict['RMIN'], resp_dict['PCH2O']) = \
        (strToType(i) for i in resp_details[0:4])

    # Read Plant Composition Section. Needed by DEMAND
    section = '!*PLAN'
    lnum, found = find(lines, section)
    if not found:
        error(section, 42, filecc, lnum)
        return
    lnum += 1
    plant_details = lines[lnum].split()
    plant_dict['RES30C'], plant_dict['R30C2']=(strToType(i) for i in resp_details[0:2])

def readECO(crop:str, modelCode:str, econo:str, #Inputs
            eco_dict:dict): # Dictionaries and arrays to update
    """
    Inputs:
      crop: 2 character crop code
      modelCode: 3 character code for crop model
      eco_dict: Empty dictionary
      econo: 6 character string. Target ecotype code
    Output:
      Fill array values for eco_dict
    """
    # Read Ecotype file---------------------------
    # Set econo to DFAULT if empty string was passed in
    if econo == '':
        econo = 'DFAULT'
    filegc = 'C:/DSSAT48/Genotype/' + crop + modelCode + '048.ECO'
    errkey = 1
    lnum = 0
    try:
        with open(filegc, 'r') as file:
            lines = file.readlines()
    except IOError as e:
        error(errkey, e.errno, filegc, 0)
        return

    ecotyp: str = ''

    # Find headers row and get list of keys for dictionary
    lnum, found = find(lines, '@ECO#')
    paramlist = lines[lnum].split()

    # Find selected ecotype row. Update ecotype dictionary
    lnum, found = find(lines, econo)
    if not found:
        return "ecotype code not found"

    # Set ecotype code and econame by substring
    eco_row = lines[lnum]
    eco_dict[paramlist[0].strip('. ')], eco_dict[paramlist[1].strip('. ')] = eco_row[0:7].strip(), eco_row[7:25].strip()

    # Fill other columns
    valuelist = lines[lnum][26:].split()
    for i in range(0,len(valuelist)):
        eco_dict[paramlist[i+2]] = strToType(valuelist[i])
    #
    # while (ecotyp != econo):
    #     lnum, isect = ignore(lines, lnum)
    #     c255 = lines[lnum]
    #     lnum += 1
    #     if c255[0] != ' ' and c255[0] != '*':
    #         ecotyp, econam = c255[0:6].strip(), c255[7:26].strip()
    #         vals = c255[24:].split()
    #         ivrgrp, ivrtem = int(vals[0]), int(vals[1])
    #         thvar, phthrs[:4], pm06, pm09, phthrs[10:12], trifol, r1ppo, optbi, slobi = \
    #             (float(vals[2]),
    #             [float(i) for i in vals[3:7]],
    #             float(vals[7]),
    #             float(vals[8]),
    #             [float(i) for i in vals[10:12]],
    #              float(vals[12]),
    #             float(vals[14]),
    #             float(vals[15]),
    #             float(vals[16]))

#=======================================================================
#  HFIND, Subroutine
#  Finds appropriate HEADER in a file of logical unit number LUNUM
#  by searching for a 5-character NAME following the '@' at the
#  beginning of a header line
#-----------------------------------------------------------------------
#  INPUT  : LUNUM  file name or path
#           NAME   variable name of header section to find (5-char)
#  OUTPUT : LNUM   line number of file currently read
#           ISECT  return status of find routine
#                  0  - EOF, name not found
#                  1  - NAME found
#                  2  - End of section encountered, denoted by *
#=======================================================================
def HFIND(LUNUM,NAME,LNUM):
# Initialization, save initial line
    ISECT = 1
    NAME = NAME.upper()

# Loop to read through data file.

    with open(LUNUM) as f:
        for I in range(1, LNUM+1):
            next(f)
        for LINE in f: # Read each line as text
            # End of section
            LNUM = LNUM + 1
            if LINE[0] == '*':
                ISECT = 2
                return LNUM, ISECT

            # Header line
            if LINE[0] == '@':
                # HEADER='     '
                LINE = LINE.upper()

                for I in range (1,len(LINE)-len(NAME)):
                   # HEADER[1:len(NAME)] = LINE[I:(I+len(NAME)-1)]
                   HEADER = LINE[I:(I + len(NAME))]
                   if HEADER[0:len(NAME)] == NAME:
                        ISECT = 1
                        return LNUM, ISECT
#=======================================================================

#=======================================================================
#  IGNORE2, Subroutine
#----------------------------------------------------------------------------
#  PURPOSE: To read lines as an n-character variable and check it
#           for a blank line or for a comment line denoted by ! in col 1.
#           Also check for second tier of data as notated by @ in the first
#           column.
#----------------------------------------------------------------------------
# INPUTS:  LUN - file path
#          LINEXP - Starting line number at which this routine begins to
#                   read the file
# OUTPUTS: LINEXP - Line number last read by the routine
#          ISECT - Indicator of completion of IGNORE2 routine
#                  0 - End of file encountered
#                  1 - Found a good line to read
#                  2 - End of Section in file encountered ("*") found
#                  3 - Second tier headers found ("@" found)
#          CHARTEST - 80-character variable containing the contents of
#                     the last line read by the IGNORE2 routine
#=======================================================================
def IGNORE2(LUN,LINEXP):

    ISECT = 1

    with open(LUN) as f:
        for I in range(1, LINEXP+1):
            next(f)

        for CHARTEST in f:
            LINEXP = LINEXP + 1
        # CHECK TO SEE IF ALL OF THIS SECTION HAS BEEN READ

            if CHARTEST.startswith('*'):       # INTERMEDIATE HEADER FOUND.
                ISECT = 2
                return LINEXP, ISECT, CHARTEST

            if CHARTEST.startswith('@'):        # NEXT TIER ENCOUNTERED
                ISECT = 3
                return LINEXP, ISECT, CHARTEST

        #     CHECK FOR BLANK LINES AND COMMENTS (DENOTED BY ! IN COLUMN 1)
            if not CHARTEST.startswith('!') and not CHARTEST.startswith('@'):
                length = len(CHARTEST.strip())
                if length > 0:
        #         FOUND A GOOD LINE TO READ
                  return LINEXP, ISECT, CHARTEST
#=======================================================================

#=======================================================================
#  FIND, Subroutine
#  Finds appropriate SECTION in a file of logical unit number LUNUM by
#  searching for a 6-character NAME at beginning of each line.
#-----------------------------------------------------------------------
#  INPUT : LUNUM  - File path of the file to read
#          NAME  - 6-character variable name of section to find
#  OUTPUT: LNUM  - Line number of the file currently being read
#          FOUND - Indicator of completion of find routine
#                    0 - End-of-file encountered i.e. name not found
#                    1 - NAME was found
#  LOCAL :
#  IFILE : LUNUM
#=======================================================================
def FIND(fpath, NAME):
    #     Initialization.
    FOUND = 0
    LNUM = 1
    NAME = NAME.upper()

    #     Loop to read through data file.
    with open(fpath) as f:
        for line in f:
            line = line[0:len(NAME)].upper()
            #        String found, set FOUND to 1, and exit loop.
            if NAME == line:
                FOUND = 1
                break
            #           String not found, set FOUND to 0.
            else:
                FOUND = 0
                LNUM = LNUM + 1

    return LNUM, FOUND

#=======================================================================
#  READ_DETAIL, Subroutine
#-----------------------------------------------------------------------
#  Reads DETAIL.CDE file, searches for CODE within SECTION, returns TEXT
#   INPUT
#       LENCDE - number of letters
#       LENTXT - length of text
#       CODE - 2 letter crop code
#       SECTION - where to search section name
#   OUTPUT
#       TEXT - description
#========================================================================
def READ_DETAIL(LENCDE, LENTXT, CODE, SECTION):
    import os
    import sys
    from ModuleDefs import STDPATH
    from UTILS import get_dir
    from ERROR import ERROR

    ERRKEY = 'DETAIL'
    FILECDE = 'DETAIL.CDE'
    TEXT = ''
    LNUM = 0
    DATAX = FILECDE

    # Check if file exists in current data directory
    FEXIST = os.path.isfile(DATAX)

    if not FEXIST:
        # File does not exist in data directory, check directory with executable
        PATHX = sys.argv[0]
        PATHX = get_dir(PATHX)
        DATAX = os.path.join(PATHX.strip(), FILECDE)
        FEXIST = os.path.isfile(DATAX)

    if not FEXIST:
        # Last, check for file in STDPATH
        DATAX = os.path.join(STDPATH.strip(), FILECDE)
        FEXIST = os.path.isfile(DATAX)

    if FEXIST:
        FOUND = FIND_IN_FILE(SECTION, DATAX)
        if FOUND == 0:
            ERROR(SECTION, 42, DATAX, LNUM)

        try:
            with open(DATAX, 'r') as file:
                for line in file:
                    LNUM += 1
                    try:
                        FCODE = line[:LENCDE].strip()
                        FTEXT = line[9:LENTXT + 10].strip()
                    except Exception as ERR:
                        ERROR(ERRKEY, ERR, DATAX, LNUM)

                    if CODE == FCODE:
                        TEXT = FTEXT
                        break
        except Exception as ERR:
            ERR = -99#Temporary value to bypass comparing exception to int
            ERROR(ERRKEY, ERR, DATAX, 0)
    else:
        ERROR(ERRKEY, 29, FILECDE, 0)

    return TEXT
#=======================================================================

def FIND_IN_FILE(NEEDLE, HAYSTACK):
#  Input
#       NEEDLE - CHARACTER*(*): string
#       HAYSTACK - file unit number: file name
#  Output
#       success (1) or failure (0)
# ====================================
    find_in_file = 0
    STRLEN = len(NEEDLE.strip())

    LNUM = 0
    with open(HAYSTACK, 'r') as file:
        for line in file:
            LNUM += 1

            if line[0:STRLEN] == NEEDLE.strip():
                find_in_file = 1
                break

    return find_in_file
#=======================================================================

# Test driver for READ_DETAIL
# LENCDE = 2
# LENTXT = 16
# CODE = 'SR'
# SECTION = '*Crop and Weed Species'
# TEXT = READ_DETAIL(LENCDE, LENTXT, CODE, SECTION)
# print(TEXT)


# NEEDLE = '*Crop and Weed Species'
# HAYSTACK = ''
# FIND_IN_FILE = FIND_IN_FILE(NEEDLE, HAYSTACK)
# print(FIND_IN_FILE)

# import os
# DATAX = 'DETAIL.CDE'
# FEXIST = os.path.isfile(DATAX)
# print(FEXIST)

# Test driver for IGNORE2
# LUN = 16 - file name or path instead
# LUN = 'C:/DSSAT48/Strawberry/UFBA1701.SRX'
# LINEXP = 15
# LINEXP, ISECT, CHARTEST = IGNORE2(LUN, LINEXP)
# print(LINEXP, ISECT, CHARTEST)

# Test driver for IGNORE
# LUN = 'C:/DSSAT48/Strawberry/UFBA1701.SRX'
# LINEXP = 16
# print(IGNORE(LUN, LINEXP))

# Test driver for FIND
# fpath = 'DSSAT48.INP'
# NAME = '*SOIL'
# LNUM, FOUND = FIND(fpath, NAME)
# print(LNUM, FOUND)


# LUN = 'DSSAT48.INP'
# LINEXP = 88
# # LINEXP, ISECT, CHARTEST = IGNORE2(LUN, LINEXP)
#
# LINEXP, ISECT, CHARTEST = IGNORE(LUN, LINEXP)
#
# print (LINEXP)
# print(ISECT)
# print(CHARTEST)



