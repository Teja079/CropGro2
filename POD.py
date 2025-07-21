# !=======================================================================
# !  PODS, Subroutine, K. J. Boote, J. W. Jones, G. Hoogenboom
# !-----------------------------------------------------------------------
# !  Computes seed and shell growth; initiation of flowers, shells, seed.
# !-----------------------------------------------------------------------
# !  Called from:  PLANT
# !  Calls:        PODCOMP
# !                ERROR, FIND, IGNORE
# !=======================================================================
import numpy as np

from ModuleDefs import *

def PODS(DYNAMIC,
         AGRSD1, AGRSH1, DLAYR, DRPP, DUL, FILECC,       #Input
         FILEGC,FILEIO, FNINL, FNINSD, FNINSH, GDMSD,    #Input
         GRRAT1, ISWWAT, LL, NAVL, NDSET, NLAYR, NRUSSH, #Input
         NSTRES, PGAVL, PHTHRS, PHTIM, PNTIM, PUNCSD,    #Input
         PUNCTR, RNITP, SDDES, SDGR, SHELWT, SW, SWFAC,  #Input
         TDUMX, TGRO, TURADD, XFRT, YRDOY, YRNR1, YRNR2, #Input
         PStres2, YRPLT,                                 #Input
         AGRSD3, LAGSD, LNGPEG, NGRSD, NGRSH, PCTMAT,    #Output
         PODNO, POTCAR, POTLIP, SDNO, SDVAR, SEEDNO,     #Output
         SHELN, SHVAR, WSDDTN, WSHDTN, WTABRT, WTSD,     #Output
         WTSHE, WTSHMT, FLWN):                           #Output

      ISWWAT, ISWFWT = " "
      CROP = " "*2
      TYPPDT = " "*3
      ERRKEY = " "*6
      ERRKEY = 'PODS  '
      ECOTYP, ECONO = (" "*6," "*6)
      SECTION = " "*6
      FILEIO = " "*30
      MESSAGE = [" "]*10
      C80 = " "*80
      FILECC, FILEGC = (" "*92," "*92)
      C255 = " "*255

      LUNECO, LUNCRP, LUNIO, ERR, LINC, LNUM, FOUND, ISECT, II = [0]*9
      NPP,NAGE,I,NLAYR = [0]*4
      DYNAMIC, TIMDIF, NR1TIM, NR2TIM = [0]*4
      YRDOY, YRNR1, YRNR2, DAS, YRPLT = [0]*5
      NDSET = 0
      TRIGGR = 0

      AGRSD1, FNINSD, GDMSD, NAVL, WSDDTN, WSHDTN, NGRSD, NGRSH = [0.0] * 8
      TEMPOD, PCTMAT, WTSHM, NSTRES, PGAVL, XFRT, LAGSD, THRESH = [0.0] * 8
      WTSHMT, TURADD, SDGR, DSWBAR, SWBAR, SWADD1, SWADD2 = [0.0] * 7
      SHVAR, LNGSH, GRRAT1, LNGPEG, AGRSH1, FNINSH, NLEFT, SHLAG = [0.0] * 8
      PROSHI, SHELWT, NRUSSH, WTABRT, TDUMX, SWFAC, SETMAX, DRPP = [0.0] * 8
      SDPDVR, RFLWAB, PMAX, SDVAR, PODUR, MNESPM, RNITP = [0.0] * 7
      PROLFF, FNINL, SEEDNO, PODNO, XMPAGE = [0.0] * 5
      WTPSD, SFDUR, PROSHF = [0.0] * 3

      NAVPOD, ADDSHL, FLWADD = [0.0] * 3
      PGLEFT, PODMAT, AFLW, FLWRDY, PODADD, SHMINE, ACCAGE, PGAVLR = [0.0] * 8
      REDPUN, WTSHMY, PAGE, REDSHL, SDMAX, RSD, PGNPOD, SHMAXG = [0.0] * 8
      SUPDAY, SDMAXX, SHRAT, WTABR, START, PNAGE, FLWFRC, FLADD = [0.0] * 8
      ACTSW, POTSW, DSW, FLAYR, CURV, TABEX = [0.0] * 6
      CUMSIG, CNSTRES, AGRSD3, ANINSD = [0.0] * 4
      RNITPD, POTLIP, POTCAR = [0.0] * 3
      CRSD, NRSD, NREQ = [0.0] * 3

#     Puncture variables, not yet functional
      PUNCSD, PUNCTR, RPRPUN = [0.0] * 3

      FNPDT = np.zeros(4)
      XSWBAR, YSWBAR, XSWFAC, YSWFAC = (np.zeros(10) for _ in range(4))
      DLAYR, SW, LL, DUL = (np.zeros(NL) for _ in range(4))
      PHTHRS = np.zeros(20)
      TGRO = np.zeros(TS)
      WTSD, WTSHE, SDNO = (np.zeros(NCOHORTS) for _ in range(3))
      SHELN, SDDES, SUPDE = (np.zeros(NCOHORTS) for _ in range(3))
      AVTEM, FLWN = (np.zeros(NCOHORTS) for _ in range(2))
      PHTIM, PNTIM = (np.zeros(NCOHORTS) for _ in range(2))

      PStres2, CPSTRES = [0.0]*2
      CONTROL = ControlType()

      if DYNAMIC == RUNINIT:
            GETLUN('FILEIO', LUNIO)
            with open(FILEIO, 'r') as file:
                  lines = file.readlines()

            LNUM = 0
            SECTION = '*CULTI'
            FIND(lines, SECTION, LINC, FOUND)
            LNUM += LINC
            if FOUND == 0:
                  ERROR(SECTION, 42, FILEIO, LNUM)
            else:
                  CROP = lines[LNUM].strip()[3:5]
                  LNUM += 1

            SECTION = '*CULTI'
            FIND(lines, SECTION, LINC, FOUND)
            LNUM += LINC
            if FOUND == 0:
                  ERROR(SECTION, 42, FILEIO, LNUM)
            else:
                  ECONO = lines[LNUM].strip()[24:30]
                  WTPSD, SFDUR, SDPDVR, PODUR, THRESH = map(float, lines[LNUM].strip()[90:].split())
                  LNUM += 1

            GETLUN('FILEC', LUNCRP)
            with open(FILECC, 'r') as file:
                  lines = file.readlines()

            LNUM = 0
            SECTION = '!*PLAN'
            FIND(lines, SECTION, LINC, FOUND)
            LNUM += LINC
            if FOUND == 0:
                  ERROR(SECTION, 42, FILECC, LNUM)
            else:
                  IGNORE(lines, LNUM, ISECT, C80)
                  PROLFF = float(C80[12:18])
                  IGNORE(lines, LNUM, ISECT, C80)
                  PROSHI, PROSHF = map(float, [C80[18:24], C80[30:36]])

            SECTION = '!*SEED'
            FIND(lines, SECTION, LINC, FOUND)
            LNUM += LINC
            if FOUND == 0:
                  ERROR(SECTION, 42, FILECC, LNUM)

            FIND(lines, SECTION, LINC, FOUND)
            LNUM += LINC
            if FOUND == 0:
                  ERROR(SECTION, 42, FILECC, LNUM)

            SECTION = '*FRESH WEIGHT'
            FIND(lines, SECTION, LNUM, FOUND)
            if FOUND != 0:
                  IGNORE(lines, LNUM, ISECT, C80)
                  if 'Y' in C80[:10]:
                        ISWFWT = 'Y'

            # !-----------------------------------------------------------------------
            # !    Read Ecotype Parameter File
            # !-----------------------------------------------------------------------
