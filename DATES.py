from ERROR import *
#from WARNING import *
from ModuleDefs import  *
#=======================================================================
#  4-digit Year, Subroutine
#  Converts YRDOY to YEARDOY
#-----------------------------------------------------------------------
#  Input : YRDOY
#  Output:
#=======================================================================
def Y4K_DOY(YRDOY,FILE,LINE,IERRNUM,IERRKEY):
    """
    :param YYDOY: Integer containing date in format YYDOY
    :return: Integer containing date in format YYYYDOY.
    """
    import WARNING
    CROVER = 35
    ERRKEY = 'Y4KDOY'
    MSG = [""]*5
    # !-----------------------------------------------------------------------
    # !    Convert input date (YRDOY) to 7-digit
    # !-----------------------------------------------------------------------
    if FirstWeatherDate > 0 and YRDOY > 0 and YRDOY <= 99365:

        # Convert dates
        NEWYRDOY = int(FirstWeatherDate / 100000) * 100000 + YRDOY

        if NEWYRDOY >= FirstWeatherDate:
            YRDOY = NEWYRDOY
        else:
            YRDOY = int((FirstWeatherDate + 99000) / 100000) * 100000 + YRDOY

#-----------------------------------------------------------------------
#    Check the new YRDOY converted
#-----------------------------------------------------------------------
        if YRDOY < FirstWeatherDate or YRDOY > FirstWeatherDate + 99000:
            ERROR(ERRKEY, 1, FILE, LINE)
        if YRDOY > (YRDOY + CROVER * 1000):
            MSG[1] = f"WARNING - Y4K Date - Cross-over"
            MSG[2] = f"Please check file: {FILE}"
            MSG[3] = f"Line: {LINE}"
            MSG[4] = f"Date: {YRDOY}"
            WARNING.WARNING(IERRNUM, IERRKEY, MSG)
    elif YRDOY > 0 and YRDOY <= 99365:
        YR = int(YRDOY/1000)
        DOY = int(YRDOY - YR*1000)
        if YR > CROVER:
            YRDOY = (1900 + YR)*1000 + DOY
        else:
            YRDOY = (2000 + YR)*1000 + DOY
    return YRDOY


#=======================================================================
#  YR_DOY, Subroutine
#  Converts YRDOY to YR and DOY.
#-----------------------------------------------------------------------
#  Input : YRDOY integer
#  Output: YR,DOY integer
#=======================================================================
def YR_DOY(YRDOY:int):
    """
    :param YRDOY: Integer with format YYYYDDD
    :return: integer for year YYYY and day of year DDD
    """
    YR = int(YRDOY/1000)
    DOY = YRDOY - 1000*YR
    return YR,DOY

def NAILUJ(JULD, YR):
    # Define the month names and days per month
    MonthTxt = ["JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"]

    DAYS = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

    # Function to determine if a year is a leap year
    def LEAP(YR):
        return (YR % 4 == 0 and YR % 100 != 0) or (YR % 400 == 0)

    # Adjust for leap year
    if LEAP(YR):
        DAYS[1] = 29  # Adjust February days

    NSUM = 0

    # Loop through months to determine RMON and NDAY
    for JCOUNT in range(12):
        NDIF = JULD - NSUM
        if NDIF <= DAYS[JCOUNT]:
            NDAY = NDIF
            RMON = MonthTxt[JCOUNT]
            return RMON, NDAY  # Return the month and day
        NSUM += DAYS[JCOUNT]

# =======================================================================
#   TIMDIF, Integer function, N.B. Pickering, 09/13/91
#   Calculates the time difference between two YRDOY dates (days).
# -----------------------------------------------------------------------
#   Input : YRDOY1,YRDOY2
#   Output: DAYDIF
# =======================================================================
def TIMDIF(YRDOY1,YRDOY2):
    TIMDIF = YRDOY2 - YRDOY1
    if abs(TIMDIF) > 365:
        YR1, DOY1 = YR_DOY(YRDOY1)
        YR2, DOY2 = YR_DOY(YRDOY2)
        TIMDIF = DOYC(YR2, DOY2) - DOYC(YR1, DOY1)
    return TIMDIF

def DOYC(YR, DOY):
    NLEAP = int((YR-1)/4)
    return NLEAP*366 + (YR - NLEAP - 1) * 365 + DOY

# =======================================================================
#   INCYD, Integer Function
#   Increases/decreases YRDOY by INC days. Returns incremented date in YRDOY format
# -----------------------------------------------------------------------
#   Input : YRDOY
#   Output: YRDOY (incremented)
# =======================================================================
def INCYD(YRDOY, INC):
    YR, DOY = YR_DOY(YRDOY)
    NDYR = ENDYR(YR)
    DOY = DOY + INC
    while DOY > NDYR or DOY <= 0:
        if DOY > NDYR:
            YR = YR + 1
            DOY = DOY - NDYR
            NDYR = ENDYR(YR)
        elif DOY <= 0:
            YR = YR - 1
            NDYR = ENDYR(YR)
            DOY = NDYR + DOY
    return YDOY(YR, DOY)


def ENDYR(YR):
    #Return number of days in year YR (365 or 366)
    if LEAP(YR): return 366
    return 365

def LEAP(YR):
    #Return whether YR is a leap year
    if YR%400 == 0:
        return True
    elif YR%100 == 0:
        return False
    elif YR%4 == 0:
        return True
    else:
        return False

def YDOY(YR, DOY):
    #Combines year (YR) and day of year (DOY) into a single date YRDOY
    return YR * 1000 + DOY

#TEST
# YRDOY1 = 2024161
# YRDOY2 = 2022098
# print(TIMDIF(YRDOY1, YRDOY2))
# print(INCYD(YRDOY1, 38))
# print(INCYD(YRDOY1, 730))

# YRDOY1 = 2017274
# print(INCYD(YRDOY1, -1))
#
# YRDOY1 = 2017273
# YRDOY1 = INCYD(YRDOY1, 1)
# print(YRDOY1)