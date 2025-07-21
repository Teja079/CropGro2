#=======================================================================
# SUBROUTINE IPVAR
# Reads in genetic information for crop
# Inputs
# Outputs
#=======================================================================
def IPVAR(FILEG,NSENS,RNMODE,VARNO,PATHGE,MODEL):
    """
    Inputs:
      FILEG: Cultivar File name
      VARNO: Cultivar code (6 char string)
      PATHGE: Genotype directory path
      culCode: 6 character cultivar code
      modelCode: 3 character code for crop model
      cparams: instance of GEN03, the cultivar paramter dataclass
    Local:
      cul_dict: empty dictionary to store cultivar parameters
    Returns:
      VRNAME: cultivar name
      ECONO: ecotype code

    Updates cultivar parameters stored in GEN03 in COMGEN.py
    """
    import os
    from READS import find, strToType
    from COMGEN import GEN03

    ERRKEY = 'IPVAR'
    NLVAR = 0

    cul_dict = {'ECO#': None, 'CSDL': None, 'PPSEN': None, 'EM-FL': None, 'FL-SH': None, 'FL-SD': None, 'SD-PM': None,
                'FL-LF': None, 'LFMAX': None, 'SLAVR': None, 'SIZLF': None, 'XFRT': None, 'WTPSD': None, 'SFDUR': None,
                'SDPDV': None, 'PODUR': None, 'THRSH': None, 'SDPRO': None, 'SDLIP': None}
    # Create file path.
    filec = os.path.join(PATHGE.strip(),FILEG)

    try:
        with (open(filec, 'r') as filec):
            lines = filec.readlines() #Read and split rows into list.
    except IOError as e:
        #ERROR(ERRKEY, e.errno, filec, 0)
        return
    # Find headers row and get list of keys for dictionary
    # lnum, found = find(lines, '@VAR#')
    lnum, found = find(lines, '@VAR#')
    paramlist = lines[lnum][30:].split()
    ATLINE = lines[lnum].strip()


    # Find selected cultivar row. Update cultivar dictionary
    #lnum, found = find(lines, VARNO)
    lnum, found = find(lines, VARNO)
    if not found:
        return "Cultivar not found"

    valuelist = lines[lnum][30:].split()

    # Substring contaning cultivar name
    VARTY = lines[lnum][:6]
    VRNAME = lines[lnum][7:28]

    for i in range(len(paramlist)):
        cul_dict[paramlist[i]] = strToType(valuelist[i])

    ECONO = cul_dict["ECO#"]
    #Update variables dataclass COMGEN
    GEN03.CSDVAR    = cul_dict["CSDL"]
    GEN03.PPSEN     = cul_dict["PPSEN"]
    GEN03.PH2T5     = cul_dict["EM-FL"]
    GEN03.PHTHRS[5] = cul_dict["FL-SH"]
    GEN03.PHTHRS[7] = cul_dict["FL-SD"]
    GEN03.PHTHRS[9] = cul_dict["SD-PM"]
    GEN03.PHTHRS[12]= cul_dict["FL-LF"]
    GEN03.LFMAX     = cul_dict["LFMAX"]
    GEN03.SLAVAR    = cul_dict["SLAVR"]
    GEN03.SIZELF    = cul_dict["SIZLF"]
    GEN03.XFRUIT    = cul_dict["XFRT"]
    GEN03.WTPSD     = cul_dict["WTPSD"]
    GEN03.SFDUR     = cul_dict["SFDUR"]
    GEN03.SDPDVR    = cul_dict["SDPDV"]
    GEN03.PODUR     = cul_dict["PODUR"]
    GEN03.THRESH    = cul_dict["THRSH"]
    GEN03.SDPRO     = cul_dict["SDPRO"]
    GEN03.SDLIP     = cul_dict["SDLIP"]

    return VARTY, VRNAME, ECONO, ATLINE
# =======================================================================

#Test case
#from COMGEN import GEN03
# print(IPVAR('TMGRO048.CUL', 'HYDFAV','C:/DSSAT48/Genotype/',
#             None,None,None,None,None,None),'\n')
# for attr in vars(GEN03):
#     if not attr.startswith("__"):
#         print(f"{attr} = {getattr(GEN03, attr)}")


# Test driver for IPVAR
# FILEG = 'SRGRO048.CUL'
# NSENS = 0
# RNMODE = 'B'
# VARNO = 'SR0001'
# PATHGE = 'C:/DSSAT48/Genotype/'
# MODEL = 'CRGRO048'
# #
# # print(IPVAR(FILEG,NSENS,RNMODE,VARNO,PATHGE,MODEL))
# #
# VARTY, VRNAME, ECONO, ATLINE = IPVAR(FILEG,NSENS,RNMODE,VARNO,PATHGE,MODEL)
# print (VARTY, VRNAME, ECONO, ATLINE)

# from COMSWI import  SWITCH as sw
# from COMIBS import  IBS01 as ibs1
# sw.FILEG = 'SRGRO048.CUL'
# sw.PATHGE = 'C:\\DSSAT48\\Genotype\\'
# ibs1.MODEL = 'CRGRO048'
#
# VARTY, VRNAME, ECONO, ATLINE = IPVAR (sw.FILEG,NSENS,RNMODE,VARNO,sw.PATHGE,ibs1.MODEL)
# print (VARTY, VRNAME, ECONO, ATLINE)
