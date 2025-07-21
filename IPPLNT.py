# =======================================================================
#   IPPLNT, Subroutine, C.H. Porter
# -----------------------------------------------------------------------
#   Reads variables from crop or species specific data file
# -----------------------------------------------------------------------
#   Called:      PLANT
#   Calls:       FIND, ERROR, IGNORE
# =======================================================================
def IPPLNT(CONTROL):
    from ERROR import ERROR
    import fortranformat as ff
    from READS import find, ignore
    import os

    FILEIO = CONTROL.FILEIO
    ERRKEY = 'IPPLNT'

    f50 = ff.FortranRecordReader("15X,A12,1X,A80")
    f51 = ff.FortranRecordReader("15X,A12,1X,A80")
    LNUM = 6

    # Get file lines
    try:
        with open(FILEIO, 'r') as f:
            lines = f.readlines()
    except OSError:
        ERROR(ERRKEY,-99,FILEIO,LNUM)
        return
    # Get cultivar file path
    try:
        FILEC, PATCHCR = f50.read(lines[LNUM])
    except Exception as e:
        ERROR(ERRKEY,-99,FILEIO,LNUM)
        return
    #Get ecotype file path
    LNUM += 1
    try:
        FILEE, PATCHEC = f51.read(lines[LNUM])
    except Exception as e:
        ERROR(ERRKEY,-99,FILEIO,LNUM)
        return
    #Read crop species
    SECTION = '*CULTI'
    LNUM, found = find(lines, SECTION)
    if found:
        fmt = ff.FortranRecordReader('3X,A2')
        LNUM, ISECT = ignore(lines, LNUM + 1)
        try:
            CROP = fmt.read(lines[LNUM])[0]
        except Exception as e:
            ERROR(ERRKEY, -99, FILEIO, LNUM)
            return
    else:
        ERROR(ERRKEY, 42, FILEIO, LNUM)
        return

    if CROP == 'FA': # No Parameters to read for fallow. Should not be important for hydroponics
        return
    #-----------------------------------------------------------------------
    #    Get ecotype code ECONO
    #-----------------------------------------------------------------------
    SECTION = '*CULTI'
    LNUM, found = find(lines, SECTION, targ_inst=2)
    if found:
        fmt = ff.FortranRecordReader('24X,A6')
        try:
            LNUM, ISECT = ignore(lines, LNUM + 1)
            ECONO = fmt.read(lines[LNUM])[0]
        except Exception as e:
            ERROR(ERRKEY, -99, FILEIO, LNUM)
            return
    else:
        ERROR(ERRKEY, 42, FILEIO, LNUM)
        return

    # -----------------------------------------------------------------------
    #  READ CROP PARAMETERS FROM FILEC .SPE
    # -----------------------------------------------------------------------
    FILECC = os.path.join(PATCHCR.strip(), FILEC.strip())
    LNUM = 0
    try:
        with open(FILECC, 'r') as f:
            lines = f.readlines()
    except OSError:
        ERROR(ERRKEY, -99, FILEIO, LNUM)
        return
    #-----------------------------------------------------------------------
    # READ PHOTOSYNTHESIS PARAMETERS *******************
    #-----------------------------------------------------------------------
    SECTION = '!*PHOT'
    LNUM, found = find(lines, SECTION)
    if not found:
        ERROR(ERRKEY, 42, FILEIO, LNUM)
        return

    LNUM, ISECT = ignore(lines, LNUM+1)
    if ISECT != 1:
        ERROR(ERRKEY, -99, FILECC, LNUM)
        return
    fmt = ff.FortranRecordReader("(12X,F6.0)")
    try:
        KCAN = fmt.read(lines[LNUM])[0]
    except Exception:
        ERROR(ERRKEY,-99,FILECC,LNUM)
        return
    #KC_SLOPE is an optional input
    fmt = ff.FortranRecordReader("(18X,F6.0)")
    try:
        KC_SLOPE = fmt.read(lines[LNUM])[0]
        if KC_SLOPE < 1e-6: KC_SLOPE = 0.1
    except Exception:
        KC_SLOPE = 0.1

    # -----------------------------------------------------------------------
    # READ RESPIRATION PARAMETERS *******************
    # -----------------------------------------------------------------------
    SECTION = '!*RESP'
    LNUM, found = find(lines, SECTION)
    if not found:
        ERROR(ERRKEY, 42, FILEIO, LNUM)
        return
    try:
        LNUM, ISECT = ignore(lines,LNUM+1)
        RES30C, R30C2 = ff.FortranRecordReader("(G12.0,F6.1)").read(lines[LNUM])

        LNUM, ISECT = ignore(lines, LNUM + 1)
        RNO3C, RNH4C, RPRO, RFIXN = ff.FortranRecordReader("(4F6.0)").read(lines[LNUM])

        LNUM, ISECT = ignore(lines, LNUM + 1)
        RCH2O, RLIP, RLIG, ROA, RMIN, PCH2O = ff.FortranRecordReader("(6F6.0)").read(lines[LNUM])

    except Exception:
        ERROR(ERRKEY, -99, FILEIO, LNUM)
        return

    # -----------------------------------------------------------------------
    # READ RESPIRATION PARAMETERS *******************
    # -----------------------------------------------------------------------
    SECTION = '!*PLAN'
    LNUM, found = find(lines, SECTION)
    if not found:
        ERROR(ERRKEY, 42, FILEIO, LNUM)
        return
    try:
        LNUM, ISECT = ignore(lines, LNUM + 1)
        PROLFI, PROSTI = ff.FortranRecordReader("(F6.0,12X,F6.0)").read(lines[LNUM])

        LNUM, ISECT = ignore(lines, LNUM + 1)
        PRORTI, PROSHI = ff.FortranRecordReader("(F6.0,12X,F6.0)").read(lines[LNUM])

        LNUM, ISECT = ignore(lines, LNUM + 1)
        LNUM, ISECT = ignore(lines, LNUM + 1)
        PCARSH = ff.FortranRecordReader("(18X,F6.0)").read(lines[LNUM])[0]

        LNUM, ISECT = ignore(lines, LNUM + 1)
        PLIPSH = ff.FortranRecordReader("(18X,F6.0)").read(lines[LNUM])[0]

        LNUM, ISECT = ignore(lines, LNUM + 1)
        PLIGSH, PLIGSD = ff.FortranRecordReader("(18X,2F6.0)").read(lines[LNUM])

        LNUM, ISECT = ignore(lines, LNUM + 1)
        POASH, POASD = ff.FortranRecordReader("(18X,2F6.0)").read(lines[LNUM])

        LNUM, ISECT = ignore(lines, LNUM + 1)
        PMINSH, PMINSD = ff.FortranRecordReader("(18X,2F6.0)").read(lines[LNUM])
    except Exception:
        ERROR(ERRKEY, -99, FILEIO, LNUM)
        return

    # -----------------------------------------------------------------------
    #  READ CARBON AND NITROGEN MINING PARAMETERS
    # -----------------------------------------------------------------------
    SECTION = '!*CARB'
    LNUM, found = find(lines, SECTION)
    if not found:
        ERROR(ERRKEY, 42, FILEIO, LNUM)
        return
    try:
        LNUM, ISECT = ignore(lines, LNUM + 1)
        CMOBMX, CADPR1 = ff.FortranRecordReader("(F6.0,6X,F6.0)").read(lines[LNUM])
    except Exception:
        ERROR(ERRKEY, -99, FILEIO, LNUM)
        return

    # -----------------------------------------------------------------------
    #  READ NITROGEN FIXATION PARAMATERS
    # -----------------------------------------------------------------------
    SECTION = '!*NITR'
    LNUM, found = find(lines, SECTION)
    if not found:
        ERROR(ERRKEY, 42, FILEIO, LNUM)
        return
    try:
        LNUM, ISECT = ignore(lines, LNUM + 1)
        TTFIX = ff.FortranRecordReader("(18X,F6.0)").read(lines[LNUM])[0]
    except Exception:
        ERROR(ERRKEY, -99, FILEIO, LNUM)
        return

    # -----------------------------------------------------------------------
    #      ***** READ PARTITIONING PARAMETERS *****************
    # -----------------------------------------------------------------------
    SECTION = '!*VEGE'
    LNUM, found = find(lines, SECTION)
    if not found:
        ERROR(ERRKEY, 42, FILEIO, LNUM)
        return
    try:
        for _ in range(4): LNUM, ISECT = ignore(lines,LNUM+1)
        FRCNOD = ff.FortranRecordReader("(30X,F6.0)").read(lines[LNUM])[0]
    except Exception:
        ERROR(ERRKEY, -99, FILEIO, LNUM)
        return

    # -----------------------------------------------------------------------
    #      ***** READ SENESCENCE PARAMETERS ******************
    #        This is found in the second heading that begins with '!*LEAF'
    # -----------------------------------------------------------------------
    SECTION = '!*LEAF'
    LNUM, found = find(lines, SECTION, targ_inst = 2)
    if not found:
        ERROR(ERRKEY, 42, FILEIO, LNUM)
        return
    try:
        LNUM, ISECT = ignore(lines, LNUM + 1)
        FREEZ1, FREEZ2 = ff.FortranRecordReader("(18X,2F6.0)").read(lines[LNUM])
    except Exception:
        ERROR(ERRKEY, -99, FILEIO, LNUM)
        return

    # -----------------------------------------------------------------------
    #          Read ROOT parameters
    # -----------------------------------------------------------------------
    SECTION = '!*ROOT'
    LNUM, found = find(lines, SECTION)
    if not found:
        ERROR(ERRKEY, 42, FILEIO, LNUM)
        return
    try:
        LNUM, ISECT = ignore(lines, LNUM + 1)
        RWUEP1, RWUMX = ff.FortranRecordReader("(30X,2F6.0)").read(lines[LNUM])

        LNUM, ISECT = ignore(lines, LNUM + 1)
        LNUM, ISECT = ignore(lines, LNUM + 1)
        PORMIN = ff.FortranRecordReader("12X,F6.0").read(lines[LNUM])[0]

    except Exception:
        ERROR(ERRKEY, -99, FILEIO, LNUM)
        return

    # -----------------------------------------------------------------------
    #      ***** READ POD DETACHMENT PARAMETERS *****
    # -----------------------------------------------------------------------
    SECTION = '!*POD '
    LNUM, found = find(lines, SECTION)
    if not found:
        ERROR(ERRKEY, 42, FILEIO, LNUM)
        return
    try:
        LNUM, ISECT = ignore(lines, LNUM + 1)
        DETACH = ff.FortranRecordReader('(5X,A1)').read(lines[LNUM])[0]
        DETACH = DETACH.upper()
    except Exception:
        ERROR(ERRKEY, -99, FILEIO, LNUM)
        return

    # -----------------------------------------------------------------------
    #      ***** READ EVAPOTRANSPIRATION PARAMETERS *****
    # -----------------------------------------------------------------------
    SECTION = '!*EVAP'
    LNUM, found = find(lines, SECTION)
    if not found:
        ERROR(ERRKEY, 42, FILEIO, LNUM)
        return
    try:
        LNUM, ISECT = ignore(lines, LNUM + 1)
        KEP, EORATIO = ff.FortranRecordReader('(2F6.0)').read(lines[LNUM])
    except Exception:
        ERROR(ERRKEY, -99, FILEIO, LNUM)
        return
    # -----------------------------------------------------------------------
    #     Ecotype Parameter File Path
    # -----------------------------------------------------------------------
    FILEGC = os.path.join(PATCHEC.strip(), FILEE.strip())
    NOUTDO = 'OVERVIEW.OUT'

    return (CADPR1, CMOBMX, CROP, DETACH, ECONO,
            EORATIO, FILECC, FILEGC, FRCNOD, FREEZ1, FREEZ2,
            KCAN, KC_SLOPE, KEP, NOUTDO, PCARSH, PCH2O,
            PLIPSH, PLIGSD, PLIGSH, PMINSD, PMINSH, POASD,
            POASH, PORMIN, PROLFI, PRORTI, PROSHI, PROSTI,
            R30C2, RCH2O, RES30C, RFIXN, RLIG, RLIP, RMIN,
            RNH4C, RNO3C, ROA, RPRO, RWUEP1, RWUMX, TTFIX)

# -----------------------------------------------------------------------
#  Variable definitions
# -----------------------------------------------------------------------
#  CADPR1   Maximum fraction of stem growth after flowering that can be
#             allocated to carbohydrate storage just before a full seed load
#             is set. ( fraction)
#  CMOBMX   Maximum C pool mobilization rate (g[CH2O] / m2 / d)
#  CROP     Crop identification code
#  DETACH   Switch to determine if pod detachment will be simulated (Y or N)
#  ECONO    Ecotype code - used to match ECOTYP in .ECO file
#  ENAME    Experiment description
#  ERR      Error code for file operation
#  EXPER    Experiment code (prefix of input files)
#  FILEC    Filename for SPE file (e.g., SBGRO980.SPE)
#  FILECC   Path plus filename for species file (*.spe)
#  FILEE    Filename for ECO file (e.g., SBGRO980.ECO)
#  FILEGC   Pathname plus filename for ECO file
#  FILEIO   Filename for INP file (e.g., IBSNAT35.INP)
#  FOUND    Indicator that good data was read from file by subroutine FIND
#             (0 - End-of-file encountered, 1 - NAME was found)
#  FRCNOD   Fraction of new root dry matter allocation that is diverted to
#             nodule growth
#  FREEZ1   Temperature below which plant loses all leaves, but development
#             continues (�C)
#  FREEZ2   Temperature below which plant growth stops completely. (�C)
#  ISECT    Data record code (0 - End of file encountered, 1 - Found a good
#             line to read, 2 - End of Section in file encountered, denoted
#             by * in column 1
#  ISWDIS   Pest damage simulation switch (Y or N)
#  LUNCRP   Logical unit number for FILEC (*.spe file)
#  MODEL    Name of CROPGRO executable file
#  NL       maximum number of soil layers = 20
#  PATHCR   Pathname for SPE file or FILEE.
#  PATHEC   Pathname for FILEC
#  PATHL    Number of characters in path name (path plus filename for FILEC)
#
#  PCARSH   Proportion of shell tissue that is carbohydrate (fraction)
#  PCH2O    Respiration loss due to storage/mobilization of CH2O
#             (g[CH2O] / g[CH2O])
#  PLIGSD   Proportion of seed tissue that is lignin (fraction)
#  PLIGSH   Proportion of shell tissue that is lignin (fraction)
#  PLIPSH   Proportion of shell tissue that is lipid (fraction)
#  PMINSD   Proportion of seed tissue that is mineral (fraction)
#  PMINSH   Proportion of shell tissue that is mineral (fraction)
#  POASD    Proportion of seed tissue that is organic acid (fraction)
#  POASH    Proportion of shell tissue that is organic acid (fraction)
#  PROLFI   Maximum protein composition in leaves during growth with
#             luxurious supply of N (g[protein] / g[leaf tissue])
#  PRORTI   Maximum protein composition in roots during growth with
#             luxurious supply of N (g[protein] / g[root])
#  PROSHI   Maximum protein composition in shells during growth with
#             luxurious supply of N ( g[protein] / g[shell tissue])
#  PROSTI   Maximum protein composition in stems during growth with
#             luxurious supply of N (g[protein] / g[stem])
#  R30C2    Respiration coefficient that depends on total plant mass, value
#             at 30C (g[CH2O] used / g[CH2O] fixed / hr)
#  RCH2O    Respiration required for synthesizing CH2O structure
#             (g[CH2O] / g[tissue])
#  RES30C   Respiration coefficient that depends on gross photosynthesis,
#             value at 30C (g CH2O/g DW/hr)
#  RFIXN    CH2O required for biological N fixation (g[CH2O] / g[protein])
#  RLIG     Respiration required for synthesizing lignin structure
#             (g[CH2O] / g[lignin])
#  RLIP     Respiration required for synthesizing lipid structure
#             (g[CH2O] / g[lipid])
#  RMIN     Respiration required for synthesizing mineral structure
#             (g[CH2O] / g[mineral])
#  RNH4C    CH2O required for protein synthesis when source of N is ammonium
#             uptake (g[CH2O] / g[protein])
#  RNO3C    Respiration required for reducing NO3 to protein
#             (g[CH2O] / g[protein])
#  ROA      Respiration required for synthesizing organic acids
#             (g[CH2O] / g[product])
#  RPRO     Respiration required for re-synthesizing protein from mobilized
#             N (g[CH2O] / g[protein])
#  TTFIX    Physiological days delay in nodule initiation
#             (photo-thermal days / day)
# -----------------------------------------------------------------------
#       END SUBROUTINE IPPLNT
# =======================================================================

#Test
from ModuleDefs import ControlType, SwitchType
CONTROL = ControlType()
CONTROL.FILEIO = 'DSSAT48.INP'
print(IPPLNT(CONTROL))