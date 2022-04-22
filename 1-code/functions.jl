"""
    half_canopy_left(width, height, h0)

Draw the half crop canopy for the plant on the left-hand side. We draw only the half canopy
because we only use this one for computations.

# Arguments

- `shape`: the plant canopy shape: `:rectangle`, `:utriangle`, `:dtriangle`.
- `width`: the plant canopy widht (total)
- `height`: the plant canopy height (total)
- `h0=0.0`: the base canopy height (default to 0 for the ground for annual crops)
- `x0=0.0`: the x coordinate of the left-hand plant

# Details

`:utriangle` is a triangle pointing up towards the sky, and `:dtriangle` pointing down
towards the ground.
"""
function half_canopy_left(shape, width, height, h0=0.0, x0=0.0)
    if !in(shape, [:dtriangle, :utriangle, :rectangle])
        error("shape should be one of `:dtriangle`, `:utriangle` or `:rectangle`")
    end

    if shape == :dtriangle
        p = poly(
            [
                Point(x0, height + h0),
                Point(x0 + width / 2.0, height + h0),
                Point(x0, h0)
            ],
            :fill,
            close=true
        )
    elseif shape == :utriangle
        p = poly(
            [
                Point(x0, height + h0),
                Point(x0, h0),
                Point(x0 + width / 2.0, h0),
            ],
            :fill,
            close=true
        )
    elseif shape == :rectangle
        p = poly(
            [
                Point(x0, height + h0),
                Point(x0, h0),
                Point(x0 + width / 2.0, h0),
                Point(x0 + width / 2.0, height + h0)
            ],
            :fill,
            close=true
        )
    end
    return p
end

"""

Computes the transmitted radiation to the plane below the plant.

# Arguments

- `width`: plant crown width
- `P_latitude`: latitude in degree
- `j`: julian day
- `ir`: interrow distance, *i.e.* distance between two plants at sowing
- `shape`: the plant canopy shape: `:rectangle`, `:utriangle`, `:dtriangle`.
- `h0=0.0`: the base canopy height (default to 0 for the ground for annual crops)
- `alpha`: row angle relative to North in radian
- `rdif`: diffuse fraction of the light
- `P_ktrou`: light extinction coefficient of the crop
- `lai`: leaf area index (m2 m-2)
- `eai`: ears area index, or the surface of any photosynthetic organs (m2 m-2)
- `height`: total plant height
"""
function transrad(rg, width, P_latitude, P_parsurrg, j, ir, shape, h0, alpha, rdif, P_ktrou, lai, eai, height)
    rdif = r_diffuse(rg, P_latitude, j)
    rombre, rsoleil = r_transmitted(width, P_latitude, j, ir, shape, h0, alpha, rdif, P_ktrou, lai, eai, height)

    # Computation of the relative surfaces of the shaded/sunlit parts of the plane below the plant:
    surfAO = width / ir

    if rombre == 0.0
        surfAO = 0.0 # RV: when largeur is very low the first point is not even shaded
    end

    surfAO = min(surfAO, 1.0)
    surfAS = 1.0 - surfAO

    if surfAS <= 0.0
        surfAS = 0.001
        surfAO = 1.0 - surfAS
    end

    # Intercepted radiation
    raint = P_parsurrg * rg * (1 - (rombre * surfAO) - (rsoleil * surfAS))

    if raint < 0.0
        raint = 0.0
    end

    return raint
end

"""
    r_diffuse(rg, P_latitude, jul)

Computes the diffuse fraction of the global radiation.

# Arguments

- `rg`: global radiation in MJ m-2 day-1
- `P_latitude`: latitude in degree
- `j`: julian day
"""
function r_diffuse(rg, P_latitude, j)

    RsRso = rg / rg_extrater(P_latitude / 180 * π, j)

    # Diffuse / global fraction  (Spitters et al 1986 AFM 38 : 217-229)
    if (RsRso < 0.07)
        rdif = 1.0
    elseif 0.07 <= RsRso < 0.35
        rdif = 1.0 - (2.3 * (RsRso - 0.07)^2)
    elseif 0.35 <= RsRso < 0.75
        rdif = 1.33 - (1.46 * RsRso)
    else # > 0.75
        rdif = 0.23
    end

    return rdif
end

"""
    rg_extrater(P_latitude, j)

Returns the extraterrestrial radiation in MJ m-2 day-1 following Saumane (1993).

# Arguments

- `P_latitude`: latitude in degree
- `j`: julian day
"""
function rg_extrater(P_latitude, j)

    z = decangle(j) # Solar declination angle
    x = sin(z)
    y = sin(P_latitude)

    solar = 1370 * 3600 * 24 / π
    a = -x * y / sqrt((1 - x^2) * (1 - y^2))
    rg_ex = 0.0 * x
    if a < 0.0
        if a + 1.0 < 0.0
            a = -1.0
        end
        u = sqrt(1 - a^2) / a
        rg_ex = x * y * (π + atan(u) - u)
    end

    if a == 0.0
        if x == 0.0
            rg_ex = sqrt(1 - y^2)
        else
            rg_ex = sqrt(1 - x^2)
        end
    end

    if a > 0.0
        u = sqrt(1 - a^2) / a
        rg_ex = x * y * (rg_ex + atan(u) - u)
    end

    rg_ex = solar * (1 + (0.033 * cos(0.0172 * j))) * rg_ex * 1e-6

    return rg_ex
end

"""

Computes the transmitted radiation to the plane below the plant.

# Arguments

- `width`: plant crown width
- `P_latitude`: latitude in degree
- `j`: julian day
- `ir`: interrow distance, *i.e.* distance between two plants at sowing
- `shape`: the plant canopy shape: `:rectangle`, `:utriangle`, `:dtriangle`.
- `h0=0.0`: the base canopy height (default to 0 for the ground for annual crops)
- `alpha`: row angle relative to North in radian
- `rdif`: diffuse fraction of the light
- `P_ktrou`: light extinction coefficient of the crop
- `lai`: leaf area index (m2 m-2)
- `eai`: ears area index, or the surface of any photosynthetic organs (m2 m-2)
- `height`: total plant height
"""
function r_transmitted(width, P_latitude, j, ir, shape, h0, alpha, rdif, P_ktrou, lai, eai, height)

    # Number of sample points along the plane:
    interval = 200
    rtransmis = zeros(Float64, interval)
    rg = 1.0
    rdirect = rg - rdif
    #: Changements d'unit�s
    lat = P_latitude / 180.0 * π

    xprec = 0.0
    ilim = interval # we initialise at the interval so if the plant is larger than the interrow
    # it is by default equal to the interrow index.

    for i in 1:interval
        x = i / interval * (ir / 2) # x is the coordinates of the point in meters
        if xprec <= width / 2 && x > width / 2
            ilim = i
        end
        xprec = x

        # Diffuse radiation:
        kgdiffus = kdif(x, h0, width, ir, height)

        # Direct radiation
        kgdirect = kdir(lat, j, width, x, ir, shape, h0, alpha, height)

        rdroit = kgdiffus * rdif + kgdirect * rdirect
        rtransmis[i] = (1.0 - rdroit) * (exp(-P_ktrou * (lai + eai)))
        rtransmis[i] = rtransmis[i] + rdroit
    end

    # Moyennes � l'ombre et au soleil
    rombre = 0.0
    rsoleil = 0.0

    for i in 1:interval
        if i < ilim
            rombre = rombre + rtransmis[i]
        else
            rsoleil = rsoleil + rtransmis[i]
        end
    end

    # Compute the average transmitted light for shaded component:
    if ilim == 1
        rombre = 0.0
    else
        rombre = rombre / (ilim - 1)
    end

    # Compute the average transmitted light for sunlit component:
    if ilim < interval
        rsoleil = rsoleil / (interval - ilim)
        if rsoleil > 1.0
            rsoleil = 1.0
        end
    end
    return (rombre, rsoleil)
end

"""
    kdif(x, h0, width, ir, e)

Fraction of diffuse radiation received by a point.

# Arguments

- `x`: the point x coordinates
- `h0`: the canopy base height
- `width`: the plant canopy widht
- `ir`: the interrow distance, *i.e.* the distance between two plants
- `e`: the effective canopy thickness
"""
function kdif(x, h0, width, ir, e)

    # Values given by Hervé Sinoquet, gives height, azimuth and fraction of diffuse light according
    # to the SOC standard for 23 directions
    htab = (5 * 9.23, 2 * 10.81, 3 * 26.57, 5 * 31.08, 3 * 47.41, 2 * 52.62, 3 * 69.16)
    aztab = (12.23, 59.77, 84.23, 131.77, 156.23, 36, 108, 0, 72, 144, 23.27, 48.73, 95.27, 120.73, 167.27, 0, 72, 144, 36, 108, 0, 72, 144)
    SOCtab = (5 * 0.0043, 2 * 0.0055, 3 * 0.0140, 5 * 0.0197, 3 * 0.0336, 2 * 0.0399, 3 * 0.0495)

    x = min(x, ir / 2)
    limite = width / 2.0
    kgdiffus = 0.0

    # For the right-hand side:
    G = (h0 + e) / (ir - x - width / 2.0)
    for i in 1:23
        hcrit = atan(G * sin(aztab[i] / 180 * π)) / π * 180
        if hcrit < htab[i]
            kgdiffus = kgdiffus + SOCtab[i]
        end
    end

    # For the left-hand side:
    # If the point is not under the plant canopy (else it is only transmitted light):
    if x > width / 2
        G = (h0 + e) / (x - width / 2)
        for i in 1:23
            hcrit = atan(G * sin(aztab(i) / 180 * π)) / π * 180
            if (hcrit < htab[i])
                kgdiffus = kgdiffus + SOCtab[i]
            end
        end
    end

    return kgdiffus
end

"""

Fraction of direct radiation received by a point.

# Arguments

- `x`: the point x coordinates
- `h0`: the canopy base height
- `width`: the plant crown widht
- `ir`: the interrow distance, *i.e.* the distance between two plants
- `e`: the effective canopy thickness
- `shape`: the plant canopy shape: `:rectangle`, `:utriangle`, `:dtriangle`.

NB: This is the `kgeom` function from STICS
"""
function kdir(lat, j, width, x, ir, shape, h0, alpha, e)

    x = min(x, ir / 2)

    θ1, θ2 = get_θ(lat, j, width, x, ir, shape, h0, alpha, e)

    kg = 0.5 * (cos(π / 2 + θ1) + cos(π / 2 + θ2))

    return max(kg, 0.0)
end
"""
    get_θ(lat, j, width, x, ir, shape, h0, alpha, e)

Compute the two angles that gives the portion of sky that is seen by a point.
"""
function get_θ(lat, j, width, x, ir, shape, h0, alpha, e)

    limite = width / 2

    if (e > 0.0)
        limite2 = width / 2 * (h0 / e + 1)
    else
        shape = :rectangle
    end

    # Rectangle shape
    if shape == :rectangle
        tgh = (h0 + e) / (ir - x - limite)
        θ1 = tetacrit(lat, j, tgh, alpha)
        if x > limite
            tgh = (h0 + e) / (x - limite)
            θ2 = tetacrit(lat, j, tgh, alpha)
        elseif x < limite
            tgh = h0 / (-x + limite)
            θ2 = -tetacrit(lat, j, tgh, alpha)
        elseif x == limite
            θ2 = 0
        end
    elseif shape == :dtriangle
        tgh = (h0 + e) / (ir - x - limite)
        θ1 = tetacrit(lat, j, tgh, alpha)
        if x > limite
            tgh = (h0 + e) / (x - limite)
            θ2 = tetacrit(lat, j, tgh, alpha)
        elseif x < limite
            tgh = (h0 + e) / (x - limite)
            θ2 = -tetacrit(lat, j, tgh, alpha)
        elseif x == limite
            θ2 = 0
        end
    elseif shape == :utriangle
        tgh = (h0 + e) / (ir - x - limite)
        θ1 = tetacrit(lat, j, tgh, alpha)
        if x < limite2
            if (x > limite)
                tgh = h0 / (x - limite)
                θ2 = tetacrit(lat, j, tgh, alpha)
            elseif x < limite
                tgh = h0 / (limite - x)
                θ2 = -tetacrit(lat, j, tgh, alpha)
            elseif x == limite
                θ2 = 0.0
            end
            if x >= limite2
                tgh = (h0 + e) / x
                θ2 = tetacrit(lat, j, tgh, alpha)
            end
        end
    end
    return (θ1, θ2)
end

"""
    tetacrit(lat, j, tgh, alpha)

Compute the cosinus of the theta angle for the apparent sun height `h` (tangent of the angle
in radian)
"""
function tetacrit(lat, j, tgh, alpha)
    # Initialisations
    acrit = 0.0
    bcrit = 0.0
    a = 0.0
    b = 0.0
    tetacriteria = 0.0
    hcritprec = 0.0
    n = 3
    teta = zeros(Float64, 180)
    dec = decangle(j)
    hprec = 0.0

    for i in 1:(18*n)
        teta[i] = 10.0 / n * (i - 1)
        teta[i] = (teta[i] - 90) / 180 * π
        # Sun position (h,azim)
        sinh = sin(lat) * sin(dec) + cos(lat) * cos(dec) * cos(teta[i])
        h = asin(sinh)
        cosazim = (-cos(lat) * sin(dec) + sin(lat) * cos(dec) * cos(teta[i])) / cos(h)

        cosazim = min(1.0, cosazim)
        if teta[i] != 0.0
            azim = acos(cosazim) * teta[i] / abs(teta[i])
        else
            azim = 0.0
        end
        if (sinh < 0.0)
            h = 0.0
        end
        # Critical height
        hcrit = atan(tgh * abs(sin(azim + alpha + 0.00001)))
        # test for h = hcrit
        if hcritprec >= hprec && hcrit <= h && i > 1
            # Linear interpolation
            acrit = (hcrit - hcritprec) / (teta[i] - teta[i-1])
            bcrit = hcrit - acrit * teta[i]
            a = (h - hprec) / (teta[i] - teta[i-1])
            b = h - a * teta[i]
            if a != acrit
                tetacriteria = (b - bcrit) / (acrit - a)
            end
            return tetacriteria
        end

        hcritprec = hcrit
        hprec = h
    end

    return tetacriteria
end

"""
    decangle(j)

Sun declination according to the julian day `j`
"""
function decangle(j)
    theta1 = 2 * π * (j - 80) / 365
    theta2 = 0.034 * (sin(2 * π * j / 365) - sin(2 * π * 80 / 365))
    return asin(0.3978 * sin(theta1 - theta2))
end
