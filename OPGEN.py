#=======================================================================
#  OPGEN, Subroutine
#  Generates output for simulated data
#-----------------------------------------------------------------------
#  INPUT  : CUMDEP,TPESW,VRNAME,AINO3,AINH4,TLL,TDUL,TSAT,TSWINI,WTHADJ,
#           CO2,ECONAM,RUN,MODEL,CROP,CROPD,TITLER,ECONO,VARTY,ESW,
#           SWINIT,INO3,INH4,TSOC
#
#  OUTPUT :
#-----------------------------------------------------------------------
#  Called : INPUT
#  Calls  : CLEAR OPHEAD OPSOIL
#=======================================================================
def OPGEN(CUMDEP,TPESW,VRNAME,AINO3,AINH4,TLL,TDUL,TSAT,TSWINI,RUN,MODEL,CROP,CROPD,TITLET,ECONO,VARTY,
          ESW,SWINIT,INO3,INH4,TSOC,WTHSTR,NYRS, RNMODE,CONTROL, ISWITCH, UseSimCtr, ATLINE, PATHEX):


    from COMSOI import SOI02 as SOI2, SOI03 as SOI3
    # from COMSWI import *
    from OPHEAD import OPHEAD, OPSOIL
    from ModuleDefs import NL, RunConstants

    # CHARACTER*  1 ANS, RNMODE
    # CHARACTER*  2 CROP
    # CHARACTER*  6 VARTY,ECONO
    # CHARACTER*  8 MODEL
    # CHARACTER* 16 CROPD, VRNAME
    # CHARACTER* 25 TITLET
    # CHARACTER* 80 PATHEX
    # CHARACTER*120 WTHSTR
    # CHARACTER*1000 ATLINE
    #
    # INTEGER NYRS,RUN
    # INTEGER LUNOV,LUNOUT
    #
    # REAL    AINO3,AINH4
    # REAL    SWINIT(NL),TSWINI,INO3(NL),INH4(NL)
    # REAL    CUMDEP,TPESW,ESW(NL)
    # REAL    TLL,TDUL,TSAT,TSOC
    #
    # LOGICAL UseSimCtr
    #
    # TYPE (ControlType) CONTROL
    # TYPE (SwitchType)  ISWITCH
    #
    # PARAMETER (LUNOUT = 30,
    LUNOV = 80

#-----------------------------------------------------------------------
#     Generate header file to be used by model output routines.
#-----------------------------------------------------------------------
    OPHEAD (RunConstants.SEASINIT,LUNOV,CUMDEP,TPESW,VRNAME,AINO3,AINH4,ECONO,RUN,MODEL,TITLET,WTHSTR,RNMODE,
           CONTROL,ISWITCH,UseSimCtr,PATHEX)
#-----------------------------------------------------------------------
#     Generate a summary output for the screen
#-----------------------------------------------------------------------
    if RNMODE == 'I' and NYRS <= 1:
        pass
        # CLEAR
        # write header to console
        #   WRITE (6,50)
        # OPHEAD (SEASINIT,6,CUMDEP,TPESW,VRNAME,AINO3,AINH4,ECONO,RUN,MODEL,TITLET,WTHSTR, RNMODE,
        #        CONTROL, ISWITCH, UseSimCtr, PATHEX)

    OPSOIL (SOI3.LL,SOI3.DUL,SOI3.SAT,SOI3.DLAYR,SWINIT,SOI3.DS,SOI2.NLAYR,ESW,SOI3.SHF,SOI3.BD,SOI3.PH,INO3,INH4,SOI3.OC,
            TLL,TDUL,TSAT,TPESW,TSWINI,AINO3,AINH4,TSOC,SOI3.SWCON,SOI3.U,SOI3.SALB,SOI3.CN2,CROPD,
            VRNAME,VARTY,SOI3.SLPF,ECONO,SOI3.SLNF,CROP,RNMODE,RUN,CONTROL.MODEL,ISWITCH,ATLINE)

    if RNMODE == 'I' and NYRS <= 1:
        pass
        # WRITE (*,2900)
        # READ (5,'(1A1)') ANS

#      WRITE(LUNOV,'(//)')
#      CLOSE (LUNOV)
    return
#-----------------------------------------------------------------------
#     FORMAT Strings
#-----------------------------------------------------------------------
 #  50  FORMAT ('*SIMULATION OVERVIEW')
 # 2900 FORMAT (1X,'Please press < ENTER > key to continue ',$)
#=======================================================================

