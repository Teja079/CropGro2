# =======================================================================
#   IPIBS, Subroutine, G. Hoogenboom
# -----------------------------------------------------------------------
#   Reads input variables for temporary data file for transfer of
#   information from the INPUT module to CSM.
# -----------------------------------------------------------------------
# ========================================================================
def IPIBS (CONTROL, ISWITCH):
    #CROP, IDETS, MODEL)                 !Output
    from ModuleDefs import Put_CONTROL, Put_ISWITCH
    #USE ModuleData
    from ERROR import ERROR
    from READS import find
    import fortranformat as ff
    ERRKEY = 'IPIBS '

    FILEIO = CONTROL.FILEIO
    RUN = CONTROL.RUN
    RNMODE = CONTROL.RNMODE

    LNUM = -99
# -----------------------------------------------------------------------
#     Open Temporary File
# -----------------------------------------------------------------------
    try:
        with open(FILEIO,'r') as f:
            lines = f.readlines()

        # -----------------------------------------------------------------------
        #     Read FILE names and paths
        # -----------------------------------------------------------------------
        fmt = ff.FortranRecordReader('(55X,I5)')
        LNUM = 0
        ISENS = fmt.read(lines[LNUM])[0]

        fmt = ff.FortranRecordReader('(15X,A8)')
        LNUM = 2
        MODEL = fmt.read(lines[LNUM])[0]

        fmt = ff.FortranRecordReader('(15X,A12)')
        LNUM += 1
        FILEX = fmt.read(lines[LNUM])[0]

        fmt = ff.FortranRecordReader('(15X,A12,1X,A80)')
        LNUM += 1
        FILEA,PATHEX = fmt.read(lines[LNUM])

        fmt = ff.FortranRecordReader('(15X,A12,1X,A80)')
        LNUM += 2
        FILEC, PATHCR = fmt.read(lines[LNUM])
        LNUM += 2

        MEEVP = ISWITCH.MEEVP
        MEPHO = ISWITCH.MEPHO
        ISWWAT = ISWITCH.ISWWAT
        ISWNIT = ISWITCH.ISWNIT
        ISWPHO = ISWITCH.ISWPHO
        NSWI = ISWITCH.NSWI
        IDETS = ISWITCH.IDETS
        # -----------------------------------------------------------------------
        #     Read Cultivar Section
        # -----------------------------------------------------------------------
        SECTION = '*CULTI'
        LNUM, FOUND = find(lines, SECTION)
        if not FOUND:
            ERROR(SECTION, 42, FILEIO, LNUM)
            return
        # Read crop code from Cultivar Section
        LNUM += 1
        fmt = ff.FortranRecordReader('(3X,A2)')
        CROP = fmt.read(lines[LNUM])[0]
        LNUM += 1

    except Exception:
        ERROR(ERRKEY, -99, FILEIO, LNUM)
        return
# !-------------------------------------------------------------------------
# !     Can by-pass this section unless in debug model
#       IF (INDEX(RNMODE, 'D') > 0) THEN
#
# !       For sequenced runs, only read values for RUN 1
#         IF (INDEX('QF',RNMODE) .LE. 0 .OR. RUN .EQ. 1) THEN
#           READ (LUNIO,'(5(/),15X,A8)', IOSTAT=ERRNUM) FNAME
#           LNUM = LNUM + 6
#           IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)

# -----------------------------------------------------------------------
#     Read Simulation Control
# -----------------------------------------------------------------------
#           READ (LUNIO,'(/,31X,A1,41X,A5)',IOSTAT=ERRNUM) ISIMI
#           READ (LUNIO,'(14X,9(5X,A1),2I6)', IOSTAT=ERRNUM)
#      &      ISWWAT, ISWNIT, ISWSYM, ISWPHO, ISWPOT, ISWDIS, ISWCHE,
#      &      ISWTIL, ICO2
#           LNUM = LNUM + 3
#           IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
#
#           ISWWAT = UPCASE(ISWWAT)
#           ISWNIT = UPCASE(ISWNIT)
#           ISWSYM = UPCASE(ISWSYM)
#           ISWPHO = UPCASE(ISWPHO)
#           ISWPOT = UPCASE(ISWPOT)
#           ISWDIS = UPCASE(ISWDIS)
#           ISWCHE = UPCASE(ISWCHE)
#           ISWTIL = UPCASE(ISWTIL)
#           ICO2   = UPCASE(ICO2)
#
#           READ (LUNIO,200, IOSTAT=ERRNUM) MESIC, MEEVP, MEINF, MEPHO,
#      &        MEHYD, NSWI, MESOM, MESEV, MESOL, METMP, MEGHG
#   200     FORMAT(25X,A1,11X,A1,3(5X,A1),5X,I1,5(5X,A1))
#           LNUM = LNUM + 1
#           IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
#
#           READ (LUNIO,'(14X,5(5X,A1))', IOSTAT=ERRNUM)
#      &                          IPLTI, IIRRI, IFERI, IRESI, IHARI
#           LNUM = LNUM + 1
#           IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
#
#           READ (LUNIO,250,IOSTAT=ERRNUM)
#      &       IOX, IDETO, IDETS, FROP, IDETG, IDETC, IDETW,
#      &       IDETN, IDETP, IDETD, IDETL, IDETH, IDETR
#   250     FORMAT(14X,3(5X,A1),4X,I2,9(5X,A1))
#           LNUM = LNUM + 1
#           IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
#
#           IF (FROP .LE. 0) FROP = 1
#
#         ELSE        !For sequenced runs, read only selected variables
#           READ (LUNIO,'(8(/),31X,A1,17X,A1)',IOSTAT=ERRNUM)ISWSYM,ISWDIS
#           LNUM = LNUM + 9
#           IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
#
#           ISWSYM = UPCASE(ISWSYM)
#           ISWDIS = UPCASE(ISWDIS)
#
#           READ (LUNIO,'(25X,A1,23X, A1)', IOSTAT=ERRNUM) MESIC,MEPHO
#           LNUM = LNUM + 1
#           IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
#
#           READ (LUNIO,'(14X,5(5X,A1))', IOSTAT=ERRNUM)
#      &      IPLTI, IIRRI, IFERI, IRESI, IHARI
#           LNUM = LNUM + 1
#           IF (ERRNUM .NE. 0) CALL ERROR(ERRKEY,ERRNUM,FILEIO,LNUM)
#         ENDIF
#       ELSE
#     MEEVP  = ISWITCH.MEEVP
#     MEPHO  = ISWITCH.MEPHO
#     ISWWAT = ISWITCH.ISWWAT
#     ISWNIT = ISWITCH.ISWNIT
#     ISWPHO = ISWITCH.ISWPHO
#     NSWI   = ISWITCH.NSWI
#     IDETS  = ISWITCH.IDETS
#     ENDIF

#   Determine how many elements are to be simulated (i.e., N, P, K)
    if ISWWAT == 'Y':
        if ISWNIT == 'Y':
            if ISWPHO == 'Y' or ISWPHO == 'H':
            # Water, N & P will be modelled.
                N_ELEMS = 2
            else:
            # Water & N modelled, P not modelled.
                N_ELEMS = 1
        else:
        # Water modelled, but neither N nor P.
            NSWI = 0
            ISWPHO = 'N'
            N_ELEMS = 0
    else:
    #No simulation of water, N or P.
        NSWI = 0
        ISWNIT = 'N'
        ISWPHO = 'N'
        N_ELEMS = 0

    if ISWWAT == 'N' or ISWNIT == 'N':
        NSWI = 0

#-------------------------------------------------------------------
#   Values read or modified here.
    CONTROL.CROP  = CROP
    CONTROL.DAS   = 0
#    CONTROL.LUNIO = LUNIO
    #CONTROL.N_ELEMS = N_ELEMS #TODO N_ELEMS needs to be added to CONTROL

    ISWITCH.MEEVP  = MEEVP
    ISWITCH.MEPHO  = MEPHO
    ISWITCH.ISWWAT = ISWWAT
    ISWITCH.ISWNIT = ISWNIT
    ISWITCH.ISWPHO = ISWPHO
    ISWITCH.NSWI   = NSWI

# !-------------------------------------------------------------------------
# !     Can by-pass this section unless in debug model
#       IF (INDEX(RNMODE, 'D') > 0) THEN
# !       Transfer values from local variables into constructed data types.
#         CONTROL % FROP  = FROP
#         CONTROL % MESIC = MESIC
#         CONTROL % MODEL = MODEL
#
#         ISWITCH % IDETC  = IDETC
#         ISWITCH % IDETD  = IDETD
#         ISWITCH % IDETG  = IDETG
#         ISWITCH % IDETL  = IDETL
#         ISWITCH % IDETN  = IDETN
#         ISWITCH % IDETO  = IDETO
#         ISWITCH % IDETS  = IDETS
#         ISWITCH % IDETW  = IDETW
#         ISWITCH % IHARI  = IHARI
#         ISWITCH % ISIMI  = ISIMI
#
#         ISWITCH % ISWDIS = ISWDIS
#         ISWITCH % ISWSYM = ISWSYM
#         ISWITCH % ISWPOT = ISWPOT
#         ISWITCH % ISWCHE = ISWCHE
#         ISWITCH % ISWTIL = ISWTIL
#         ISWITCH % ICO2   = ICO2
#
#         ISWITCH % MEHYD  = MEHYD
#         ISWITCH % MEINF  = MEINF
#         ISWITCH % MESOL  = MESOL
#         ISWITCH % MESOM  = MESOM
#         ISWITCH % METMP  = METMP
#         ISWITCH % MESEV  = MESEV
#         ISWITCH % MEGHG  = MEGHG
#
#         ISWITCH % IPLTI  = IPLTI
#         ISWITCH % IIRRI  = IIRRI
#         ISWITCH % IFERI  = IFERI
#         ISWITCH % IRESI  = IRESI
#         ISWITCH % IDETP  = IDETP
#         ISWITCH % IDETH  = IDETH
#         ISWITCH % IDETR  = IDETR
#       ENDIF
# !-------------------------------------------------------------------------
    Put_CONTROL(CONTROL)
    Put_ISWITCH(ISWITCH)
    return CROP, IDETS, MODEL

#Test
# from ModuleDefs import ControlType, SwitchType
# CONTROL = ControlType()
# ISWITCH = SwitchType()
# CONTROL.FILEIO = 'DSSAT48.INP'
# CONTROL.RUN = 1
# CONTROL.RNMODE = 'B'
# ISWITCH.MEEVP = 'R'
# ISWITCH.MEPHO = 'L'
# ISWITCH.ISWWAT  = 'N'
# ISWITCH.ISWNIT  = 'N'
# ISWITCH.ISWPHO  = 'N'
# ISWITCH.NSWI    = 1
# ISWITCH.IDETS   = 'Y'
#
# print(IPIBS (CONTROL, ISWITCH))
