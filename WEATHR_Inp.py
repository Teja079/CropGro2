# C=======================================================================
# C  WEATHR, Subroutine
# C
# C  Weather handling routine: input, weather modification and
# C  generation of hourly values.
# C-----------------------------------------------------------------------
# C  INPUT  : CO2ADJ,CO2FAC,DAYADJ,DAYFAC,DPTADJ,DPTFAC,PRCADJ,PRCFAC,
# C           RADADJ,RADFAC,TMADJ,TMFAC,TXADJ,TXFAC,WMODB,WMODI,WNDADJ,
# C           WNDFAC,WTHADJ,CO2
# C
# C  LOCAL  :
# C
# C  OUTPUT : Changes input arrays. Returns CO2 and WTHSTR
# C-----------------------------------------------------------------------
# C  Called : INPUT
# C
# C  Calls  : ERROR WTHMDB WTHMOD
# C=======================================================================

def WEATHR_Inp(CO2ADJ,CO2FAC,DAYADJ,DAYFAC,DPTADJ,DPTFAC,
               PRCADJ,PRCFAC,RADADJ,RADFAC,TMADJ,TMFAC,
               TXADJ,TXFAC,WMODI,WNDADJ,WNDFAC,WTHADJ,
               CO2,WTHSTR,NEV):
    import fortranformat as ff
    f75 = ff.FortranRecordReader("8(6X,A1,F6.0,2X)")
    #f80 = ff.FortranRecordWriter("5(A6,A1,F6.2,2X),1(A6,A1,F6.1,2X),2(A5,A1,F6.2,2X)")

    if (WMODI == 'Y' and WTHADJ[0][5] != 0.0) or (WMODI == 'Y' and WTHADJ[1][5] != 1.0):
        CO2 = WTHADJ[0][5] + CO2 * WTHADJ[1][5]

    WTHSTR = WTHSUM(WTHADJ, WTHSTR)

    if WMODI == 'Y':
        (DAYFAC[0], DAYADJ[0], RADFAC[0], RADADJ[0], TXFAC[0], TXADJ[0],
         TMFAC[0], TMADJ[0], PRCFAC[0], PRCADJ[0], CO2FAC[0], CO2ADJ[0],
         DPTFAC[0], DPTADJ[0], WNDFAC[0], WNDADJ[0]) = f75.read(WTHSTR[0:120])

    if NEV >= 1:
        #80  FORMAT(5(A6,A1,F6.2,2X),1(A6,A1,F6.1,2X),2(A5,A1,F6.2,2X))
        WTHSTR = f"DAYL= {DAYFAC[0]}{DAYADJ[0]:6.2f}  " \
                 f"SRAD= {RADFAC[0]}{RADADJ[0]:6.2f}  " \
                 f"TMAX= {TXFAC[0]}{TXADJ[0]:6.2f}  " \
                 f"TMIN= {TMFAC[0]}{TMADJ[0]:6.2f}  " \
                 f"RAIN= {PRCFAC[0]}{PRCADJ[0]:6.2f}  " \
                 f"CO2 = {CO2FAC[0]}{CO2ADJ[0]:6.1f}  " \
                 f"DEW = {DPTFAC[0]}{DPTADJ[0]:6.2f}  " \
                 f"WIND= {WNDFAC[0]}{WNDADJ[0]:6.2f}  "
    return CO2, WTHSTR


# C=======================================================================
# C  WTHSUM, Subroutine, N.B. Pickering
# C  Calculate WTYPE and AMOUNT of weather modification from WTHADJ for
# C  display.  Creates summary string for output of all 8 variables.
# C-----------------------------------------------------------------------
# C  Input : WTHADJ
# C  Output: WTHSTR
# C=======================================================================
def WTHSUM(WTHADJ, WTHSTR):
    WTYPE = [' '] * 8
    AMOUNT = [0.0] * 8

    # Loop over 8 elements
    for VNUM in range(8):
        if WTHADJ[0][VNUM] > 0.0 and WTHADJ[1][VNUM] == 1.0:
            WTYPE[VNUM] = 'A'
            AMOUNT[VNUM] = WTHADJ[0][VNUM]
        elif WTHADJ[0][VNUM] < 0.0 and WTHADJ[1][VNUM] == 1.0:
            WTYPE[VNUM] = 'S'
            AMOUNT[VNUM] = -WTHADJ[0][VNUM]
        elif WTHADJ[0][VNUM] == 0.0 and WTHADJ[1][VNUM] != 1.0:
            WTYPE[VNUM] = 'M'
            AMOUNT[VNUM] = WTHADJ[1][VNUM]
        elif WTHADJ[0][VNUM] != 0.0 and WTHADJ[1][VNUM] == 0.0:
            WTYPE[VNUM] = 'R'
            AMOUNT[VNUM] = WTHADJ[0][VNUM]
        else:
            WTYPE[VNUM] = ' '
            AMOUNT[VNUM] = WTHADJ[0][VNUM]

    # WTHSTR,'(8(A,A,F6.2,2X))'
    WTHSTR = (
        f"DAYL= {WTYPE[0]}{AMOUNT[0]:6.2f}  SRAD= {WTYPE[1]}{AMOUNT[1]:6.2f}  "
        f"TMAX= {WTYPE[2]}{AMOUNT[2]:6.2f}  TMIN= {WTYPE[3]}{AMOUNT[3]:6.2f}  "
        f"RAIN= {WTYPE[4]}{AMOUNT[4]:6.2f}  CO2 = {WTYPE[5]}{AMOUNT[5]:6.2f}  "
        f"DEW = {WTYPE[6]}{AMOUNT[6]:6.2f}  WIND= {WTYPE[7]}{AMOUNT[7]:6.2f}  "
    )
    return WTHSTR

# CO2ADJ = [0.0]*10
# CO2FAC = ['']*10
# DAYADJ= [0.0]*10
# DAYFAC = ['']*10
# DPTADJ= [0.0]*10
# DPTFAC = ['']*10
# PRCADJ= [0.0]*10
# PRCFAC = ['']*10
# RADADJ= [0.0]*10
# RADFAC = ['']*10
# TMADJ= [0.0]*10
# TMFAC = ['']*10
# TXADJ= [0.0]*10
# TXFAC = ['']*10
# WMODI = 'Y'
# WNDADJ = [0.0]*10
# WNDFAC = ['']*10
# WTHADJ = [[0.0]*8, [1.0]*8]
# CO2 = 0.0
# WTHSTR = ''
# NEV = 0
# #Test:
# (CO2ADJ,CO2FAC,DAYADJ,DAYFAC,DPTADJ,DPTFAC, PRCADJ,PRCFAC,RADADJ,RADFAC,TMADJ,TMFAC,
#             TXADJ,TXFAC,WMODI,WNDADJ,WNDFAC,WTHADJ,CO2, WTHSTR) = (WEATHR_Inp(CO2ADJ,CO2FAC,DAYADJ,DAYFAC,DPTADJ,DPTFAC,
#                PRCADJ,PRCFAC,RADADJ,RADFAC,TMADJ,TMFAC,
#                TXADJ,TXFAC,WMODI,WNDADJ,WNDFAC,WTHADJ,
#                CO2,WTHSTR,NEV))