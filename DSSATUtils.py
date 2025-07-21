import math

def curv(curve_type, xb, x1, x2, xm, x):
    curv_value = 1.0

    # Return 1 if type is 'NON'
    if curve_type == 'NON' or curve_type == 'non':
        return curv_value

    # Linear interpolation (LIN)
    if curve_type == 'LIN' or curve_type == 'lin':
        curv_value = 0.0
        if xb < x < x1:
            curv_value = (x - xb) / (x1 - xb)
        elif x1 <= x <= x2:
            curv_value = 1.0
        elif x2 < x < xm:
            curv_value = 1.0 - (x - x2) / (xm - x2)
        #Set curve_value to 0 to if negative, or 1.0 if greater than 1.
        curv_value = max(min(curv_value, 1.0), 0.0)

    # Quadratic interpolation (QDR)
    elif curve_type == 'QDR' or curve_type == 'qdr':
        curv_value = 0.0
        if xb < x < x1:
            curv_value = 1.0 - ((x1 - x) / (x1 - xb)) ** 2
        elif x1 <= x <= x2:
            curv_value = 1.0
        elif x2 < x < xm:
            curv_value = 1.0 - ((x - x2) / (xm - x2)) ** 2
        curv_value = max(min(curv_value, 1.0), 0.0)

    # Inverse linear (INL)
    elif curve_type == 'INL' or curve_type == 'inl':
        curv_value = 1.0
        if x1 < x < x2:
            curv_value = 1.0 - (1.0 - xm) * (x - x1) / (x2 - x1)
        elif x >= x2:
            curv_value = xm
        curv_value = max(min(curv_value, 1.0), xm)

    # Short day plants (SHO)
    elif curve_type == 'SHO' or curve_type == 'sho':
        if x <= x1:
            curv_value = 1.0
        elif x1 < x < x2:
            curv_value = 1.0 - (1.0 - xm) * (x - x1) / (x2 - x1)
        elif x >= x2:
            curv_value = xm
        curv_value = max(min(curv_value, 1.0), xm)

    # Long day plants (LON)
    elif curve_type == 'LON' or curve_type == 'lon':
        if x < x2:
            curv_value = xm
        elif x2 <= x < x1:
            curv_value = 1.0 - (1.0 - xm) * (x1 - x) / (x1 - x2)
        else:
            curv_value = 1.0
        curv_value = max(min(curv_value, 1.0), xm)

    # Sinusoidal (SIN)
    elif curve_type == 'SIN' or curve_type == 'sin':
        curv_value = 0.0
        if xb < x < x1:
            curv_value = 0.5 * (1.0 + math.cos(2.0 * math.pi * (x - x1) / (2.0 * (x1 - xb))))
        elif x1 <= x <= x2:
            curv_value = 1.0
        elif x2 < x < xm:
            curv_value = 0.5 * (1.0 + math.cos(2.0 * math.pi * (x2 - x) / (2.0 * (xm - x2))))
        curv_value = max(min(curv_value, 1.0), 0.0)

    # Reversible process (REV)
    elif curve_type == 'REV' or curve_type == 'rev':
        curv_value = 1.0
        if xb < x < x1:
            curv_value = 1.0 - (x - xb) / (x1 - xb)
        elif x1 <= x <= x2:
            curv_value = 0.0 - (x - x1) / (x2 - x1)
        elif x > x2:
            curv_value = -1.0
        curv_value = max(min(curv_value, 1.0), -1.0)
        curv_value *= xm

    # Cold dehardening (DHD)
    elif curve_type == 'DHD' or curve_type == 'dhd':
        curv_value = 0.0
        if xb < x < x1:
            curv_value = (x - xb) / (x1 - xb)
        elif x >= x1:
            curv_value = 1.0
        curv_value = max(min(curv_value, 1.0), 0.0)
        curv_value *= xm

    # Dormancy reduction (DRD)
    elif curve_type == 'DRD' or curve_type == 'drd':
        curv_value = x2
        if xb < x < x1:
            curv_value = x2 + (xm - x2) * (x - xb) / (x1 - xb)
        elif x >= x1:
            curv_value = xm
        curv_value = max(min(curv_value, xm), x2)

    # Curvilinear dormancy (CDD)
    elif curve_type == 'CDD' or curve_type == 'cdd':
        curv_value = x2
        if xb < x < x1:
            curv_value = xm - ((xm - x2) * ((x1 - x) / (x1 - xb)) ** 2)
        elif x >= x1:
            curv_value = xm
        curv_value = max(min(curv_value, xm), x2)

    # Exponential (EXK)
    elif curve_type == 'EXK' or curve_type == 'exk':
        curv_value = xb - math.exp(x1 * (x - x2) / xm)

    # Variable Order Polynomial (VOP)
    elif curve_type == 'VOP' or curve_type == 'vop':
        curv_value = 0.0
        if xb < x < xm:
            curv_value = ((x - xb) ** x2 * (xm - x)) / (((x1 - xb) ** x2) * (xm - x1))
        curv_value = max(curv_value, 0.0)

    # Q10 function
    elif curve_type == 'Q10' or curve_type == 'q10':
        curv_value = x1 * (x2 ** ((x - xb) / 10))

    # Power function (PWR)
    elif curve_type == 'PWR' or curve_type == 'pwr':
        if x < 0:
            curv_value = x2 * xb * (0 ** x1)
        else:
            curv_value = x2 * xb * (x ** x1)

    return curv_value
