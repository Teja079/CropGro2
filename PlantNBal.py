# C***********************************************************************
# C  PlantNBal, Subroutine
# C
# C  Purpose: Provides output N balance for plant growth processes
# C     (file PlantN.bal).  Based on old NBAL.for, which combined
# C     plant and soil N balances.
# C***********************************************************************

import os
from ModuleDefs import *

def PlantNBal(CONTROL, ISWITCH,
              SEEDNI, TNLEAK, WTNFX, WTNLA, WTNLF, WTNLO,
              WTNNA, WTNNO, WTNNOD, WTNRA, WTNRO, WTNRT,
              WTNSA, WTNSD, WTNSDA, WTNSDO, WTNSH, WTNSHA,
              WTNSHO, WTNSO, WTNST, WTNUP):

    IDETL, IDETN, ISWNIT = ' '*3
    PNBAL = ' '*13

    DYNAMIC, ERRNUM, YRDOY = [0]*3
    RUN, LUNPNC = [0]*2

    SEEDNI, TNLEAK = [0.0]*3
    WTNALL, WTNFX, WTNHAR, WTNLA, WTNLF, WTNLO, WTNNA = [0.0]*7
    WTNNO, WTNNOD, WTNOFF, WTNRA, WTNRO, WTNRT = [0.0]*6
    WTNSA, WTNSD, WTNSDA, WTNSDO, WTNSH, WTNSHA, WTNSHO = [0.0]*7
    WTNSO, WTNST, WTNUP = [0.0]*3

    FEXIST = False

    CONTROL = ControlType()

    ISWITCH = SwitchType()

    ISWNIT  = ISWITCH.ISWNIT
    IDETL   = ISWITCH.IDETL
    IDETN   = ISWITCH.IDETN
#---------------------------------------------------------------------------------------
    # IDETL = 'N', '0' (zero) or IDETN = 'N', suppress output
    if any(x in 'N0' for x in IDETL) or ISWNIT == 'N' or IDETN == 'N':
        return

    DYNAMIC = CONTROL.DYNAMIC
    RUN     = CONTROL.RUN
    YRDOY   = CONTROL.YRDOY

    # PNBAL file handling
    PNBAL = 'PlantNBal.OUT'
    GETLUN('PNBAL', LUNPNC)
    FEXIST = os.path.exists(PNBAL)

    if FEXIST:
        mode = 'a'  # Append mode
    else:
        mode = 'w'  # Write mode

    with open(PNBAL, mode) as file:
        if not FEXIST:
            file.write("*PLANT N BALANCE\n")

        HEADER(RUNINIT, LUNPNC, RUN)

        # Sum the N accumulated in all plant components.
        WTNALL = WTNLA + WTNSA + WTNRA + WTNSHA + WTNSDA + WTNNA

        # Sum the N in all plant components at harvest.
        WTNHAR = WTNLF + WTNST + WTNRT + WTNSH + WTNSD + WTNNOD

        # Sum N in all plant components that senesced.
        WTNOFF = WTNLO + WTNSO + WTNRO + WTNSHO + WTNSDO + WTNNO

        # Write output to PlantNBal.OUT
        file.write("\n\n   PLANT COMPONENT  HARVEST     LOST*   TOTAL  BALANCE\n")
        file.write("   ---------------  -------- kg[N]/ha -------  -------\n")

        file.write(f"   Leaf N        {WTNLF * 10.:9.2f} {WTNLO * 10.:9.2f} {WTNLA * 10.:9.2f}\n")
        file.write(f"   Stem N        {WTNST * 10.:9.2f} {WTNSO * 10.:9.2f} {WTNSA * 10.:9.2f}\n")
        file.write(f"   Shell N       {WTNSH * 10.:9.2f} {WTNSHO * 10.:9.2f} {WTNSHA * 10.:9.2f}\n")
        file.write(f"   Seed N        {WTNSD * 10.:9.2f} {WTNSDO * 10.:9.2f} {WTNSDA * 10.:9.2f}\n")
        file.write(f"   Root N        {WTNRT * 10.:9.2f} {WTNRO * 10.:9.2f} {WTNRA * 10.:9.2f}\n")
        file.write(f"   Nodule N      {WTNNOD * 10.:9.2f} {WTNNO * 10.:9.2f} {WTNNA * 10.:9.2f}\n")
        file.write(f"   Total N       {WTNHAR * 10.:9.2f} {WTNOFF * 10.:9.2f} {WTNALL * 10.:9.2f}\n")
        file.write(f"                 {'':37} {TNLEAK * 10.:9.2f}\n")
        file.write(f"   TOTAL N       {'':46} {WTNALL * 10. + TNLEAK * 10.:9.2f}\n")

        file.write("\n   N INPUTS TO SYSTEM                kg[N]/ha\n")
        file.write("   ------------------                --------\n")

        file.write(f"   Seed N At Planting{'':21} {SEEDNI * 10.:9.2f}\n")
        file.write(f"   N2 Fixed{'':30} {WTNFX * 10.:9.2f}\n")
        file.write(f"   N Uptake from Soil{'':17} {WTNUP * 10.:9.2f}\n")
        file.write(f"   TOTAL N{'':33} {WTNUP * 10. + SEEDNI * 10. + WTNFX * 10.:9.2f}\n\n")

        file.write("   * Lost due to senescence, freeze, pest or disease damage.\n")
        file.write("\n" + "*" * 79 + "\n")
