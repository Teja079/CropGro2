#Library of read function used in model
import numpy as np
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
    isect = 1
    blank_line = ''

    for lnum in range(linexp, len(lines)):
        # Read the current line
        chartest = lines[lnum].strip()

        # Check if the line contains the ASCII end-of-file character (char(26))
        if len(chartest) > 0 and ord(chartest[0]) == 26:
            isect = 0
            return lnum, isect

        # Check for end of section (denoted by * or $ in the first column)
        if chartest.startswith('*') or chartest.startswith('$'):
            isect = 2
            return lnum, isect

        # Check for non-blank lines and lines not starting with comment characters (!, @)
        if not chartest.startswith('!') and not chartest.startswith('@'):
            # If it's not a blank line, return the line
            if chartest != blank_line:
                return lnum, isect

    # End of file encountered
    isect = 0
    return lnum, isect

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

def var_assign(line:str, n_vars):
    """
    Input:
        line: String containg space or tab seperated values
        n_vars: Number of variables that need to be assigned
    Output:
        Returns list of values with correct data types
    """
    values = line.split()
    var_list = [None]*n_vars
    for i in range(n_vars):
        var_list[i] = strToType(values[i])
    return var_list

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
            resp_dict:dict, plant_dict:dict, seed_dict:dict, carb_dict:dict,
            veg_dict:dict, seedshell_dict:dict): # dictionaries to update
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

    # Read Plant Composition Section
    section = '!*PLAN'
    lnum, found = find(lines, section)
    if not found:
        error(section, 42, filecc, lnum)
        return
    lnum += 1
    plant_details = lines[lnum].split()
    (plant_dict['PROLFI'], plant_dict['PROLFG'],plant_dict['PROLFF'],
     plant_dict['PROSTI'], plant_dict['PROSTG'], plant_dict['PROSTF']) = (strToType(i) for i in plant_details[0:6])
    lnum += 1
    plant_details = lines[lnum].split()
    (plant_dict['PRORTI'], plant_dict['PRORTG'], plant_dict['PRORTF'],
     plant_dict['PROSHI'], plant_dict['PROSHG'], plant_dict['PROSHF']) = (strToType(i) for i in plant_details[0:6])
    lnum += 1
    plant_details = lines[lnum].split()
    (plant_dict['SDPROS'], plant_dict['SDPROG'], plant_dict['PRONOD'],
     plant_dict['PROMIN'], plant_dict['PROMAX'], plant_dict['THETA']) = (strToType(i) for i in plant_details[0:6])
    lnum += 1
    plant_details = lines[lnum].split()
    (plant_dict['PCARLF'], plant_dict['PCARST'], plant_dict['PCARRT'],
     plant_dict['PCARSH'], plant_dict['PCARSD'], plant_dict['PCARNO']) = (strToType(i) for i in plant_details[0:6])
    lnum += 1
    plant_details = lines[lnum].split()
    (plant_dict['PLIPLF'], plant_dict['PLIPST'],
     plant_dict['PLIPRT'], plant_dict['PLIPSH'], plant_dict['PLIPNO']) = (strToType(i) for i in plant_details[0:5])
    lnum += 1
    plant_details = lines[lnum].split()
    (plant_dict['PLIGLF'], plant_dict['PLIGST'], plant_dict['PLIGRT'],
     plant_dict['PLIGSH'], plant_dict['PLIGSD'], plant_dict['PLIGNO']) = (strToType(i) for i in plant_details[0:6])
    lnum += 1
    plant_details = lines[lnum].split()
    (plant_dict['POALF'], plant_dict['POAST'], plant_dict['POART'],
     plant_dict['POASH'], plant_dict['POASD'], plant_dict['POANO']) = (strToType(i) for i in plant_details[0:6])
    lnum += 1
    plant_details = lines[lnum].split()
    (plant_dict['PMINLF'], plant_dict['PMINST'], plant_dict['PMINRT'],
     plant_dict['PMINSH'], plant_dict['PMINSD'], plant_dict['PMINNO']) = (strToType(i) for i in plant_details[0:6])

    # Read Seed Section
    section = '!*SEED'
    lnum, found = find(lines, section)
    if not found:
        error(section, 42, filecc, lnum)
        return
    lnum += 1
    seed_details = lines[lnum].split()
    (seed_dict['LIPTB'], seed_dict['LIPOPT'], seed_dict['SLOSUM*100'],
     seed_dict['CARMIN']) = (strToType(i) for i in seed_details[0:4])

    # Read Carbon and Nitrogen Mining Section
    section = '!*CARB'
    lnum, found = find(lines, section)
    if not found:
        error(section, 42, filecc, lnum)
        return
    lnum += 1
    carb_details = lines[lnum].split()
    (carb_dict['CMOBMX'], carb_dict['CADSTF'], carb_dict['CADPR1'],
     carb_dict['NMOBMX'], carb_dict['NVSMOB'], carb_dict['NRCVR']) = (strToType(i) for i in carb_details[0:6])
    lnum += 1
    carb_details = lines[lnum].split()
    carb_dict['XPODF'], carb_dict['NSTFAC'] = (strToType(i) for i in carb_details[0:2])
    lnum += 1
    carb_details = lines[lnum].split()
    (carb_dict['ALPHL'], carb_dict['ALPHS'], carb_dict['ALPHR'],
     carb_dict['ALPHSH']) = (strToType(i) for i in carb_details[0:4])

    # Read Vegetative Partitioning Section
    section = '!*VEGE'
    lnum, found = find(lines, section)
    if not found:
        error(section, 42, filecc, lnum)
        return
    lnum += 1
    veg_details = lines[lnum].split()
    veg_dict["XLEAF"] = [strToType(i) for i in veg_details[0:8]]
    lnum += 1
    veg_details = lines[lnum].split()
    veg_dict["YLEAF"] = [strToType(i) for i in veg_details[0:8]]
    lnum += 1
    veg_details = lines[lnum].split()
    veg_dict["YSTEM"] = [strToType(i) for i in veg_details[0:8]]
    lnum += 1
    veg_details = lines[lnum].split()
    veg_dict["WTFSD"],veg_dict["PORPT"],veg_dict["FRSTMF"],veg_dict["FRLFF"],veg_dict["ATOP"],veg_dict["FRCNOD"] = (
        (strToType(i) for i in veg_details[0:6])
    )
    lnum += 1
    veg_details = lines[lnum].split()
    veg_dict["FRLFMX"] = strToType(veg_details[0])

    # Read Seed and Shell Partitioning Section
    section = '!*SEED AND SHELL'
    lnum, found = find(lines, section)
    if not found:
        error(section, 42, filecc, lnum)
        return
    lnum += 1
    seed_details = lines[lnum].split()
    seedshell_dict["SETMAX"], seedshell_dict["SRMAX"], seedshell_dict["RFLWAB"], seedshell_dict["XMPAGE"] = (
        (strToType(i) for i in seed_details[0:4])
    )
    lnum += 1
    seed_details = lines[lnum].split()
    seedshell_dict["DSWBAR"], seedshell_dict["XFRMAX"], seedshell_dict["SHLAG"] = (
        (strToType(i) for i in seed_details[0:3])
    )
    lnum += 1
    seed_details = lines[lnum].split()
    seedshell_dict["SETMAX"], seedshell_dict["SRMAX"], seedshell_dict["RFLWAB"], seedshell_dict["XMPAGE"] = (
        (strToType(i) for i in seed_details[0:4])
    )

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
        for I in range(LNUM):
            next(f)
        for LINE in f: # Read each line as text
            # End of section
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

#Example of this if statement.
#When a file is imported, any code inside will be run.
#Place the code in here to prevent that
if __name__ == '__main__':

    print('hello')