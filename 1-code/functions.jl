"""
    half_canopy_left(width, height, h0)

Draw the half crop canopy for the plant on the left-hand side. We draw only the half canopy
because we only use this one for computations.

##### Arguments

- `shape`: the plant canopy shape: `:rectangle`, `:utriangle`, `:dtriangle`.
- `width`: the plant canopy widht (total)
- `height`: the plant canopy height (total)
- `h0=0.0`: the base canopy height (default to 0 for the ground for annual crops)
- `x0=0.0`: the x coordinate of the left-hand plant

##### Details

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
        kgdiffus, H = kdif(x, h0, width, ir, height)

        # Direct radiation
        kgdirect, θ1, θ2 = kdir(lat, j, width, x, ir, shape, h0, alpha, height)

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

Fraction of diffuse radiation coming from the sky received by a point.

# Arguments

- `x`: the point x coordinates
- `h0`: the canopy base height
- `width`: the plant canopy widht
- `ir`: the interrow distance, *i.e.* the distance between two plants
- `e`: the effective canopy thickness
"""
function kdif(x, h0, width, ir, e, shape)

    # Values given by Hervé Sinoquet, gives angle zenith (h, in degrees), azimuth (az,
    # degrees) and fraction of diffuse light according to the SOC standard for 23 directions
    h = (repeat([9.23], 5)..., 10.81, 10.81, 26.57, 26.57, 26.57, repeat([31.08], 5)..., 47.41, 47.41, 47.41, 52.62, 52.62, 69.16, 69.16, 69.16)
    az = (12.23, 59.77, 84.23, 131.77, 156.23, 36, 108, 0, 72, 144, 23.27, 48.73, 95.27, 120.73, 167.27, 0, 72, 144, 36, 108, 0, 72, 144)
    diffuse = (repeat([0.0043], 5)..., 0.0055, 0.0055, 0.0140, 0.0140, 0.0140, repeat([0.0197], 5)..., 0.0336, 0.0336, 0.0336, 0.0399, 0.0399, 0.0495, 0.0495, 0.0495)

    x = min(x, ir / 2)
    limite = width / 2.0
    if shape == :utriangle
        limite2 = width / 2 * (h0 / e + 1)
    end
    kgdiffus = 0.0
    H = [] # Container for the angle of rays that effectively receive diffuse light
    sizehint!(H, 23 * 2) # Reserve 46 values (the maximum number of rays that receive light)

    # For the right-hand side:
    G = (h0 + e) / (ir - x - limite)
    # NB: using trigonometry here, remember tan(β) = AC/AB ? Well here AC is the crop height,
    # and AB is the distance between the point and the plant on the horizontal plane.
    #! For AB, we take `ir - x - limite` to get the distance to the crop heigth at its edge. This
    #! works only for the rectangle and the up-triangle, but not for the down-triangle, in which
    #! case it should be `ir - x` when the point is far from the plant, and `ir - x- limite`
    #! when it is closer to the plant and the point sees the down corner instead of the top corner

    # So atan(G) woule be the β from above, and it represents the angle between the horizontal
    # line (AB) and BC, the line between the point and the top of the canopy.

    for i in 1:23
        hcrit = rad2deg(atan(G * sin(deg2rad(az[i]))))
        # h is the ray position in the sky, hcrit the position of the top right corner of the plant
        if hcrit < h[i]
            # We add the diffuse light from this angle only if the point views the sky at this
            # angle. i.e. if the ray is higher than the angle of the top of the plant canopy.
            push!(H, hcrit) # This ray receives light from the sky
            kgdiffus = kgdiffus + diffuse[i]
        end
    end

    # For the left-hand side:
    # If the point is not under the plant canopy, else it is only transmitted light, so 0 kgdiffus:
    if x > limite
        if shape == :utriangle
            if x < limite2

            else

            end
        else
            G = (h0 + e) / (x - limite)
        end
        for i in 1:23
            hcrit = atan(G * sin(az[i] / 180 * π)) / π * 180
            if (hcrit < h[i])
                push!(H, hcrit)
                kgdiffus = kgdiffus + diffuse[i]
            end
        end
    end
    # NB: note that the left-hand side is never computed when the point is below the plant canopy
    # x < limite because we know that it reveives only transmitted light, and no sky diffuse light

    return (kgdiffus, H)
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

    if x > ir / 2.0
        @warn "sample point position > interrow / 2. Forcing it at `interrow / 2`."
        x = min(x, ir / 2.0)
    end

    θ1, θ2 = get_θ(lat, j, width, x, ir, shape, h0, alpha, e)

    kg = 0.5 * (cos(π / 2 - θ1) + cos(π / 2 + θ2))

    return (max(kg, 0.0), θ1, θ2)
end

"""
    get_θ(lat, j, width, x, ir, shape, h0, alpha, e)

Compute the two angles that gives the portion of sky that is seen by a point.
"""
function get_θ_old(lat, j, width, x, ir, shape, h0, alpha, e)

    limite = width / 2

    if (e > 0.0)
        # If we use a triangle pointing up, we need limite2
        limite2 = width / 2 * (h0 / e + 1)
        # limite2 is the limit in the point x position above which the point starts to see
        # the top of the canopy. Below that it only sees the bottom of the canopy, which blocks
        # its view.
    else
        shape = :rectangle
    end

    # NB: using trigonometry here, remember tan(β) = AC/AB ? Well here AC is the crop height,
    # and AB is the distance between the point and the plant on the horizontal plane.

    # So atan(G) woule be the β from above, and it represents the angle between the horizontal
    # line (AB) and BC, the line between the point and the top of the canopy.

    # For reference, RHS means the right-hand side, and LHS means left-hand side.
    # θ1 = angle between the vertical plane and the line from the point to the top canopy of the RHS plant
    # θ2 = angle between the vertical plane and the line from the point to the canopy of the LHS plant

    # θ2 depends on the plant shape and on the position of the point on the plane because for the
    # rectangle and top triangle a different part of the plant blocks the light: either the top
    # of the canopy if the point is not directly below the plant, or the bottom of the canopy
    # if it is right below (x < limite)

    g1 = (h0 + e) / (ir - x - limite)
    θ1 = θcrit(lat, j, g1, alpha)

    # Rectangle shape
    if shape == :rectangle
        if x > limite
            # the point is not under plant canopy
            g2 = (h0 + e) / (x - limite)
            θ2 = θcrit(lat, j, g2, alpha)
        elseif x < limite
            # the point is under plant canopy
            g2 = h0 / (-x + limite)
            θ2 = -θcrit(lat, j, g2, alpha)
        elseif x == limite
            θ2 = 0 # In this case the end of the canopy is right above
        end
    elseif shape == :dtriangle
        # Triangle pointing down
        if x > limite
            g2 = (h0 + e) / (x - limite)
            θ2 = θcrit(lat, j, g2, alpha)
        elseif x < limite
            g2 = (h0 + e) / (x - limite)
            θ2 = -θcrit(lat, j, g2, alpha)
        elseif x == limite
            θ2 = 0
        end
    elseif shape == :utriangle
        # RV: Triangle pointing up, the most complex one because the point can see either the top
        # of the canopy if it is far enough (x >= limite2), or just the bottom if it is close
        # to it or under it.
        if x < limite2
            if (x > limite)
                g2 = h0 / (x - limite)
                θ2 = θcrit(lat, j, g2, alpha)
            elseif x < limite
                g2 = h0 / (limite - x)
                θ2 = -θcrit(lat, j, g2, alpha)
            elseif x == limite
                θ2 = 0.0
            end
        else
            g2 = (h0 + e) / x
            θ2 = θcrit(lat, j, g2, alpha)
        end
    end
    return (θ1, θ2)
end

"""
    get_G(x, shape, limite, h0, e, width)

Get the ratio between the crop height and the distance between the point x and the plant.
The crop height and distance to the plant are always computed using the top of the crop
**seen** by the point, which can be different than the top of the canopy if the point is
under the canopy, or close to the canopy and the canopy is an up-pointing triangle.

Note that the function returns G1, the ratio for the plant on the right, and G2, the
ratio for the plant on the left.
"""
function get_G(x, shape, limite, h0, e, width, ir)

    # RV: G1 is the angle with the right-hand side plant
    # G2 with the left-hand side plant.
    # We use trigonometry here, remember tan(β) = AC/AB ? Well here AC is the crop height,
    # and AB is the distance between the point and the plant on the horizontal plane.
    #! For AB, we take `ir - x - limite` to get the distance to the crop heigth at its edge. This
    #! works only for the rectangle and the up-triangle, but not for the down-triangle, in which
    #! case we have to use `ir - x` when the point is far from the plant, and `ir - x- limite`
    #! when it is closer to the plant and the point sees the down corner instead of the top corner.
    # This is the same for the right plant but a little bit more complicated.

    if shape == :rectangle
        G1 = (h0 + e) / (ir - x - limite)

        if x >= limite
            # the point is not under plant canopy
            G2 = (h0 + e) / (x - limite)
        elseif x < limite
            # the point is under plant canopy
            G2 = h0 / (-x + limite)
        elseif x == limite
            G2 = 0.0
        end
    elseif shape == :dtriangle
        G1 = (h0 + e) / (ir - x - limite)

        # Triangle pointing down
        if x > limite
            G2 = (h0 + e) / (x - limite)
        elseif x < limite
            G2 = (h0 + e) / (x - limite)
        elseif x == limite
            G2 = 0.0
        end
    elseif shape == :utriangle
        # RV: Triangle pointing up, the most complex one because the point can see either the top
        # of the canopy if it is far enough (x >= limite2), or just the bottom if it is close
        # to it or under it.

        if (e > 0.0)
            limite2 = width / 2 * (h0 / e + 1)
            # limite2 is the limit in the point x position above which the point starts to see
            # the top of the canopy. Below that it only sees the bottom of the canopy, which blocks
            # its view.
        else
            limite2 = 0.0
        end

        if x < limite2
            if (ir - x) < limite2
                # G1 is inside the limit of the right-hand-side plant,
                # It does not see the top of the plant, bu only the bottom corner
                G1 = h0 / (ir - x - limite)
            else
                # G1 sees the top of the right-hand side plant
                G1 = (h0 + e) / (ir - x)
            end

            if (x > limite)
                G2 = h0 / (x - limite)
            elseif x < limite
                G2 = h0 / (limite - x)
            elseif x == limite
                G2 = 0.0
            end
        else
            G1 = (h0 + e) / (ir - x)
            G2 = (h0 + e) / x
        end
    end

    return (G1, G2)
end

function get_θ(lat, j, width, x, ir, shape, h0, alpha, e)

    limite = width / 2

    if (e > 0.0)
        # If we use a triangle pointing up, we need limite2
        limite2 = width / 2 * (h0 / e + 1)
        # limite2 is the limit in the point x position above which the point starts to see
        # the top of the canopy. Below that it only sees the bottom of the canopy, which blocks
        # its view.
    else
        shape = :rectangle
    end

    # NB: using trigonometry here, remember tan(β) = AC/AB ? Well here AC is the crop height,
    # and AB is the distance between the point and the plant on the horizontal plane.

    # So atan(G) woule be the β from above, and it represents the angle between the horizontal
    # line (AB) and BC, the line between the point and the top of the canopy.

    # For reference, RHS means the right-hand side, and LHS means left-hand side.
    # θ1 = angle between the vertical plane and the line from the point to the top canopy of the RHS plant
    # θ2 = angle between the vertical plane and the line from the point to the canopy of the LHS plant

    # θ2 depends on the plant shape and on the position of the point on the plane because for the
    # rectangle and top triangle a different part of the plant blocks the light: either the top
    # of the canopy if the point is not directly below the plant, or the bottom of the canopy
    # if it is right below (x < limite)

    G1, G2 = get_G(x, shape, limite, h0, e, width, ir)

    θ1 = -θcrit(lat, j, G1, alpha) # θ1 is negative because it runs clockwise
    θ2 = θcrit(lat, j, G2, alpha) # θ2 is positive, it is counter-clockwise

    if x < limite
        # The point is below the canopy, its angle is negative (going towards the right-hand-side)
        θ2 = -θ2

        # In this case it may happen that θ1 < θ2, which means the point sees nothing
        # because the top of the righ-hand side plant is above the view angle of the point (i.e.
        # it is completely shaded.
        if θ1 < θ2
            θ1 = 0.0
            θ2 = 0.0
        end
    end

    return (θ1, θ2)
end


"""
    θcrit(lat, j, tgh, alpha)

Compute the cosinus of the theta angle for the apparent sun height `h` (tangent of the angle
in radian)
"""
function θcrit(lat, j, tgh, alpha)
    # Initialisations
    acrit = 0.0
    bcrit = 0.0
    a = 0.0
    b = 0.0
    θcriteria = 0.0
    hcritprec = 0.0
    n = 3
    θ = zeros(Float64, 180)
    dec = decangle(j)
    hprec = 0.0

    for i in 1:(18*n)
        θ[i] = 10.0 / n * (i - 1)
        # This gives θ between 0.0 and 176.66 by steps of 3.33 degrees
        # Then we transform into radians and move it to the vertical as reference
        θ[i] = (θ[i] - 90) / 180 * π

        # Sun position (h,azim)
        sinh = sin(lat) * sin(dec) + cos(lat) * cos(dec) * cos(θ[i])
        h = asin(sinh)
        cosazim = (-cos(lat) * sin(dec) + sin(lat) * cos(dec) * cos(θ[i])) / cos(h)

        cosazim = min(1.0, cosazim)
        if θ[i] != 0.0
            azim = acos(cosazim) * θ[i] / abs(θ[i])
        else
            azim = 0.0
        end
        if (sinh < 0.0)
            h = 0.0
        end
        # Critical height (top right corner of the plant)
        hcrit = atan(tgh * abs(sin(azim + alpha + 0.00001)))

        if hcritprec >= hprec && hcrit <= h && i > 1
            # h is the sun position in the sky, hcrit the position of the top right corner of the plant
            # Linear interpolation
            acrit = (hcrit - hcritprec) / (θ[i] - θ[i-1])
            bcrit = hcrit - acrit * θ[i]
            a = (h - hprec) / (θ[i] - θ[i-1])
            b = h - a * θ[i]
            if a != acrit
                θcriteria = (b - bcrit) / (acrit - a)
            end
            return θcriteria
        end

        hcritprec = hcrit
        hprec = h
    end

    return θcriteria
end

"""
    decangle(j)

Sun declination angle according to the julian day `j`
"""
function decangle(j)
    theta1 = 2 * π * (j - 80) / 365
    theta2 = 0.034 * (sin(2 * π * j / 365) - sin(2 * π * 80 / 365))
    return asin(0.3978 * sin(theta1 - theta2))
end


function P_from_θ(θ, sky_height, x, side=:left)
    # This is the x position of P in m relative to the sample point X
    P_x = sin(θ) * sky_height / cos(θ)

    # x - P1 to get the true position in m from the relative position:
    if side == :left
        return Point(x - P_x, 0)
    else
        return Point(x + P_x, 0)
    end
end

function P_drawing(P, orig_xmax, orig_ymax, to_xmax, to_ymax)
    # Rescale to fit the position of the point on the drawing scale
    d_P_xpos = rescale(P[1], 0, orig_xmax, to_xmax, to_ymax)

    return Point(d_P_xpos, orig_ymax)
end

"""
	draw_transmitted_light(sample_point,p,inner_box)

Draw the transmitted light using the plants dimensions.
"""
function draw_transmitted_light(sample_point, p, inner_box)
    # Transmitted light: Drawing the left triangle between θ1 and the horizontal
    corner_left = inner_box[2]
    plant_y = [i[2] for i in p]
    plant_base_points = sort(p[plant_y.==minimum(plant_y)])
    plant_top_points = p[plant_y.==maximum(plant_y)]
    plant_top_point = sort(plant_top_points)[end]

    p_outline = poly(
        [
            sample_point,
            corner_left,
            plant_base_points...,
            plant_top_point,
            sample_point
        ],
        :fill
    )

    # Transmitted light: Drawing the right triangle between θ2 and the horizontal
    # Computing the coordinates of the LHS of the right plant:
    p_outline_right = [corner_left, plant_base_points..., plant_top_point]

    polyscale!(p_outline_right, -1, 1; center=O) # Mirroring
    p_outline_right = [i - Point(-(inner_box[2][1] * 2 + (inner_box[3][1] - inner_box[1][1])), 0) for i in p_outline_right] # Translating to the right position

    p_outline_right = [sample_point, p_outline_right..., sample_point]

    poly(
        p_outline_right,
        :fill
    )
end
