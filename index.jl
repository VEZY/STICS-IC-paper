### A Pluto.jl notebook ###
# v0.19.40

using Markdown
using InteractiveUtils

# This Pluto notebook uses @bind for interactivity. When running this notebook outside of Pluto, the following 'mock version' of @bind gives bound variables a default value (instead of an error).
macro bind(def, element)
    quote
        local iv = try Base.loaded_modules[Base.PkgId(Base.UUID("6e696c72-6542-2067-7265-42206c756150"), "AbstractPlutoDingetjes")].Bonds.initial_value catch; b -> missing; end
        local el = $(esc(element))
        global $(esc(def)) = Core.applicable(Base.get, el) ? Base.get(el) : iv(el)
        el
    end
end

# ╔═╡ f5cd7710-c533-11ec-1490-a502fac92221
begin
    using Luxor
    using PlutoUI
    using HypertextLiteral
    using DataFrames
    using UUIDs # Used for the magick of the interactive parameters DataFrame
    using Thebes # Add 3D to Luxor
    using Rotations # For 3D rotations
    using Colors, ColorSchemes # For the color palette of the diffuse arrows
end

# ╔═╡ 6788dbbe-317e-4212-a0e8-d417a52301f6
md"""
# STICS radiative transfer computation

This is a notebook that helps better understand the radiative transfer computation in STICS. Most of the computations are directly translated from the FORTRAN code into Julia. The others are just inputs that don't directly depends on it such as the plant shape and the day of year.

## Parameters
"""

# ╔═╡ f92c1e63-d40f-41eb-8d58-b44b62e44ff9
md"""
### Constants
"""

# ╔═╡ 311611bd-89f9-4e34-84cb-11924e8efc2d
begin
    image_dim = (800, 500)
    display_text = true # display names and values in the diagram?
end

# ╔═╡ 4dff9014-73ff-4c32-b6ad-c936bd892588
md"""
### Dynamic
"""

# ╔═╡ dff1401d-a2e9-45c1-9e26-a46d0fa44eff
md"""
## Diagram
"""

# ╔═╡ 78c00fe4-feb0-45de-b5e1-df0fae546287
md"""
*Figure 1. Diagram of the light interception computation for each sample point below the crop. Grey arrows represent sampled angles for diffuse light coming from the sky, drawn in 2D but computed in the 3D space. Yellow triangle represents the view angle receiving direct light, whith space (triangles) receiving diffuse light. Green triangles represent the inner-part of the plant canopy, i.e. the part between the interrow.*
"""

# ╔═╡ 9db4dbb1-5f92-4ce4-bd85-5a74fae7025e
md"""
Please note that the direct angles (`kdir`) are projected in 2D, but are computed in the 3D space following row orientation and sun azimuthal and zenithal angles. So it is perfectly normal to see the angles appearing *"into"* the plant crown, because the angle is in fact projected towards or away from the viewer, thus appearing drawn on top of the plant.
"""

# ╔═╡ e261142a-c411-40a3-85e4-ae979a4d9506
md"""
## References

These are the functions used in this notebook. Some are related to the drawing of the diagram, others are functions from STICS.

### STICS functions
"""

# ╔═╡ 09a77c7f-409d-4083-8267-d52ba0346c9c
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
    # g1 = (h0 + e) / (ir - x - limite)
    # G1 is the angle with the right-hand side plant
    # G2 with the left-hand side plant.
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
            G2 = (h0 + e) / (-x + limite)
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

# ╔═╡ 6d701d6c-daa5-4bf0-9ee2-cb76dfecf510
"""
    kdif(x, h0, width, ir, e)

Fraction of diffuse radiation received by a point.

The computation uses a turtle with 46 (2x23) directions to discretize the hemisphere. Then a ray is emmitted from each direction of the turtle to check if the point sees the sky at this particular angle. If so, it cumulates the proportion of diffuse radiation in this sky sector.

This is done in two steps:

- first for the plant on the right-hand side of the interrow, for 23 directions.
- then for the plant on the left-hand side, for 23 directions again. This side is only computed if the sample point is not under the crown of left-hand side plant, because else it means it only receives transmitted light from these point of views.

##### Arguments

- `x`: the sample point x coordinates
- `h0`: the crown base height
- `width`: the crown width
- `ir`: the interrow distance, *i.e.* the distance between the two plants
- `e`: the effective canopy thickness (*i.e.* plant height - h0).
"""
function kdif(x, h0, width, ir, e, shape)

    # Values given by Hervé Sinoquet, gives height, azimuth and fraction of diffuse light according
    # to the SOC standard for 23 directions
    htab = (repeat([9.23], 5)..., 10.81, 10.81, 26.57, 26.57, 26.57, repeat([31.08], 5)..., 47.41, 47.41, 47.41, 52.62, 52.62, 69.16, 69.16, 69.16)
    aztab = (12.23, 59.77, 84.23, 131.77, 156.23, 36, 108, 0, 72, 144, 23.27, 48.73, 95.27, 120.73, 167.27, 0, 72, 144, 36, 108, 0, 72, 144)
    SOCtab = (repeat([0.0043], 5)..., 0.0055, 0.0055, 0.0140, 0.0140, 0.0140, repeat([0.0197], 5)..., 0.0336, 0.0336, 0.0336, 0.0399, 0.0399, 0.0495, 0.0495, 0.0495)

    x = min(x, ir / 2)
    limite = width / 2.0
    kgdiffus = 0.0
    Hcrit1 = [] # Container for the angle of rays that effectively receive diffuse light on the righ-hand-side
    Hcrit2 = [] # Container for the angle of rays that effectively receive diffuse light on the left-hand-side
    az1 = [] # Container for the azimuthal angle of the ray
    az2 = []

    # For the right-hand side:
    G1, G2 = get_G(x, shape, limite, h0, e, width, ir)
    for i in 1:23
        # hcrit_right is the ray that point to the top of the RHS plant according to the azimuthal angle (= points to the plant at aztab = 0, and below when turning)
        hcrit_right = atan(G1 * sin(aztab[i] / 180 * π)) / π * 180
        # hcrit_left is the same but point to the LHS plant
        hcrit_left = atan(G2 * sin(aztab[i] / 180 * π)) / π * 180

        # When the point is under the LHS plant, we also have to check which canopy is shading first, the LHS or RHS, depending on the configuration:
        if x >= limite
            hcrit_left = 180.0
        else
            # In some cases when the point is under the LHS canopy, it may happen that hcrit_right < hcrit_left, which means the point sees nothing
            # because the top of the righ-hand side plant is above the view angle of the point (i.e.
            # it is completely shaded.
            if hcrit_right > hcrit_left
                hcrit_left = 0.0
                # NB: In this case we put hcrit_left = 0.0 to say to the code below to not keep this angle (htab[i] is always > 0.0 so htab[i] < hcrit_left will be false)
            end
        end


        if hcrit_right < htab[i] < hcrit_left
            # This ray receives light from the sky
            kgdiffus = kgdiffus + SOCtab[i]
            push!(Hcrit1, deg2rad(htab[i])) # angles above the canopy (pointing to the sky)
            push!(az1, deg2rad(aztab[i])) # azimuthal angle of the ray
        end

    end

    # For the left-hand side:
    # If the point is not under the plant canopy (else it is only transmitted light):
    if x > limite
        for i in 1:23
            hcrit = atan(G2 * sin(aztab[i] / 180 * π)) / π * 180
            if (hcrit < htab[i])
                kgdiffus = kgdiffus + SOCtab[i]
                push!(Hcrit2, π / 2 - deg2rad(htab[i])) # angles above the canopy (pointing to the sky
                push!(az2, deg2rad(aztab[i]))
            end
        end
    end

    return (kgdiffus, Hcrit1, Hcrit2, az1, az2)
end

# ╔═╡ 78cc38c7-22ab-4f24-b68f-4ba0f668d253
"""
    decangle(j)

Sun declination angle according to the julian day `j`
"""
function decangle(j)
    theta1 = 2 * π * (j - 80) / 365
    theta2 = 0.034 * (sin(2 * π * j / 365) - sin(2 * π * 80 / 365))
    return asin(0.3978 * sin(theta1 - theta2))
end

# ╔═╡ 53d29bf9-dab8-4586-89d3-fcbb9d6d28bc
"""
    rg_extrater(P_latitude, j)

Returns the extraterrestrial radiation in MJ m-2 day-1 following Saumane (1993).

##### Arguments

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

# ╔═╡ 58ec9faa-cbf1-4e46-b4bb-420586ac7dba
"""
    r_diffuse(rg, P_latitude, jul)

Computes the diffuse fraction of the global radiation.

##### Arguments

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

# ╔═╡ 54cda4ec-dc89-41d4-a28d-544f556c2f34
"""
    θcrit(lat, j, tgh, alpha)

Compute the cosinus of the theta angle for the apparent sun height `h` (tangent of the angle in radian)
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
        # This gives θ between 0.0 and 176.66666666666669 by steps of 3.33 degrees
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
        # Critical height
        hcrit = atan(tgh * abs(sin(azim + alpha + 0.00001)))
        # test for h = hcrit
        if hcritprec >= hprec && hcrit <= h && i > 1
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

# ╔═╡ fbe6d054-56ff-4201-8d55-f5afcda7ec52
"""
    get_θ(lat, j, width, x, ir, shape, h0, alpha, e)

Compute the two angles that gives the portion of sky that is seen by a point.
"""
function get_θ(lat, j, width, x, ir, shape, h0, alpha, e)

    limite = width / 2

    if (e <= 0.0)
        shape = :rectangle
    end

    # NB: using trigonometry here, remember tan(β) = AC/AB ? Well here AC is the crop height,
    # and AB is the distance between the point and the plant on the horizontal plane.

    # So atan(G) would be the β from above, and it represents the angle between the horizontal
    # line (AB) and BC, the line between the point and the top of the canopy.

    # For reference, RHS means the right-hand side, and LHS means left-hand side.
    # θ1 = angle between the vertical plane and the line from the point to the top canopy of the RHS plant
    # θ2 = angle between the vertical plane and the line from the point to the canopy of the LHS plant

    # θ2 depends on the plant shape and on the position of the point on the plane because for the
    # rectangle and top triangle a different part of the plant blocks the light: either the top
    # of the canopy if the point is not directly below the plant, or the bottom of the canopy
    # if it is right below (x < limite)

    G1, G2 = get_G(x, shape, limite, h0, e, width, ir)

    θ1 = θcrit(lat, j, G1, alpha)
    θ2 = -θcrit(lat, j, G2, alpha) # This one runs anti-clockwise so it is positive (output is negative so we add a - before)

    if x < limite
        # The point is below the canopy, its angle is negative (going towards the right-hand-side)
        θ2 = -θ2

        # In this case it may happen that θ1 < θ2, which means the point sees nothing
        # because the top of the righ-hand side plant is above the view angle of the point (i.e.
        # it is completely shaded.
        if θ1 > θ2
            θ1 = 0.0
            θ2 = 0.0
        end
    end

    return (θ1, θ2)
end

# ╔═╡ 7f777012-1203-427f-86aa-78d502fefaab
"""
	kdir(lat, j, width, x, ir, shape, h0, alpha, e)

Fraction of direct radiation received by a point.

##### Arguments

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
	kg = (-θ1 + θ2) / π # proportion of sky the point receives direct light

    return (max(kg, 0.0), θ1, θ2)
end

# ╔═╡ 0385990f-397e-46f8-93d7-578c8ead2be3
"""
	r_transmitted(width, P_latitude, j, ir, shape, h0, alpha, rdif, P_ktrou, lai, eai, height)

Computes the transmitted radiation to the plane below the plant, *i.e.* the total light
intercepted by the points.

The function returns two values, `(rombre, rsoleil)`, the light transmitted to the shaded
surface (directly below the plant), and the light transmitter to the sunlit surface.

##### Arguments

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
    # Latitude in radian:
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
        kgdiffus, Hcrit1, Hcrit2, az1, az2 = kdif(x, h0, width, ir, height - h0, shape)
        # Direct radiation
        kgdirect, θ1, θ2 = kdir(lat, j, width, x, ir, shape, h0, alpha, height - h0)

        rdroit = kgdiffus * rdif + kgdirect * rdirect
        rtransmis[i] = (1.0 - rdroit) * (exp(-P_ktrou * (lai + eai)))
        rtransmis[i] = rtransmis[i] + rdroit
    end

    # Average for shaded and sunlit
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

# ╔═╡ ab594776-ea39-48f6-9218-78c5eed58916
"""
	transrad(rg, width, P_latitude, P_parsurrg, j, ir, shape, h0, alpha, rdif, P_ktrou, lai, eai, height)

Computes the transmitted radiation to the plane below the plant.

##### Arguments

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
function transrad(rg, width, P_latitude, P_parsurrg, j, ir, shape, h0, alpha, P_ktrou, lai, eai, height)

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

    return raint, rombre, rsoleil, surfAO, surfAS
end

# ╔═╡ 4ab56f65-3314-4dc4-9eb1-59f68058e435
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

    # NB: using trigonometry here, remember tan(β) = AC/AB ? Well here AC is the crop height, and AB is the distance between the point and the plant on the horizontal plane.

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

# ╔═╡ 3c421bef-6123-4554-b2de-b8ceabaf1b39
md"""
## Drawing functions
"""

# ╔═╡ b90cd9e1-30ca-48be-9e7e-dd6afbce35db
"""
	draw_radiative_transfer(
		twidth, theight, tcenter; 
		width = 0.4, 
		i_sample_point = 150, 
		latitude_r = 0.0, 
		j = 1, 
		interrow = 1.0, 
		height = 0.8, 
		diffuse_angles = true, 
		shape = :rectangle, 
		h0 = 0.4, 
		alpha = deg2rad(0),
	    rg = 20, 
	    k = 0.8,
	    lai = 2.0,
	    display_text = true,
		title_height= 0.125,
		outer_box_rel_width = 0.9,
		outer_box_rel_height = 0.60,
		ri_text_pos = (-0.15,0.1),
		text_color = "grey",
		n = ""
    )

Draw a diagram of the radiative transfer computation from the STICS soil-crop model.

##### Arguments

###### Positional

- `twidth`: Width of the drawing window
- `theight`: Height of the drawing window
- `tcenter`: Center point of the drawing window

###### Keyword arguments (named)

- `width`: Plant width
- `i_sample_point`: index of the sample point for light interception (1-200)
- `latitude_r`: latitude (radians)
- `j`: julian day of year
- `interrow`: interrow distance (m)
- `height`: plant height (m)
- `diffuse_angles`: boolean, are diffuse angles to be drawn?
- `shape`: plant shape (`:rectangle`, `:utriangle`, `:dtriangle`)
- `h0`: plant canopy base height (m), this is e.g. the trunk height
- `alpha = 0.0`: Crop row direction relative to north (radians)
- `rg = 20`: Global radiation from the atmosphere, in MJ m-2 day-1
- `k = 0.8`: Light extinction coefficient
- `lai = 2.0`: Leaf Area Index in m2[leaves] m-2[soil]
- `display_text`: Boolean, should text be displayed in the diagram?
- `text_height= 0.13`: Diagram sub-title height relative to plot height 
- `outer_box_rel_width = 0.9`: Width of the outter box relative to figure width
- `outer_box_rel_height = 0.60`: Height of the outter box relative to figure height
- `ri_text_pos = (-0.15,0.1)`: position of the Ri text relative to the inner (black) box width and height 
- `text_color = "grey"`: the color of the text
- `n = ""`: the index of the plot
"""
function draw_radiative_transfer(
	twidth, theight, tcenter; 
	width = 0.4, 
	i_sample_point = 150, 
	latitude_r = 0.0, 
	j = 1, 
	interrow = 1.0, 
	height = 0.8, 
	diffuse_angles = true, 
	shape = :rectangle, 
	h0 = 0.4, 
	alpha = deg2rad(0),
    rg = 20, 
    k = 0.8,
    lai = 2.0,
    display_text = true,
	title_height= 0.125,
	outer_box_rel_width = 0.9, # Width of the outter box relative to figure width
	outer_box_rel_height = 0.60, # Height of the outter box relative to figure height
	ri_text_pos = (-0.15,0.1),
	text_color = "grey",
	n = ""
 )

	e = height - h0
    n_sample_points = 200

	# colormap for the diffuse angles
	colormap = ColorScheme(range(colorant"black", colorant"grey", length=100))
    # colormap = colorschemes[:autumn1]
	
	# fontface("Calibri Bold")
	fontsize(11)
	sethue("black")
	center = tcenter

	# Drawing the big box inside the plot that delimits the scene boundary:
	outer_box_width = outer_box_rel_width * twidth
	outer_box_height = outer_box_rel_height * theight
	outer_box = box(center, outer_box_width, outer_box_height, :none)

	# Compute radiation:
	raint, rombre, rsoleil, surfAO, surfAS = transrad(rg, width, rad2deg(latitude_r), 0.48, j, interrow, shape, h0, alpha, k, lai, 0.0, height)

	# Writting top text:
	@layer begin
		fontsize(16)
		fontface("Calibri Bold")
		sethue("black")
		setopacity(0.8)
		scale(1, -1)
		text(
			n*"Lat. $(rad2deg(latitude_r))°N, day $j",
			Point(outer_box[1][1], -outer_box[1][2] - theight*title_height)
		)
	end

	# Rescaling the crop dimensions to match the drawing coordinates:
	d_width = width * outer_box_width / (interrow + width)
	# NB: interrow + width because the outer box include plant half-width for both plants
	d_h0 = h0 * outer_box_height / height
	d_height = outer_box_height

	sethue("black")
	setdash("solid")
	inner_box = box(
		Point(outer_box[2][1] + d_width / 2, outer_box[2][2]),
		Point(outer_box[4][1] - d_width / 2, outer_box[4][2]),
		:stroke
	)

	# Crop dimensions in box dimensions:
	x0 = inner_box[2][1]
	y0 = inner_box[2][2]
	inner_box_width = inner_box[3][1] - inner_box[1][1]
	inner_box_length = inner_box[1][2] - inner_box[3][2]

	setdash("dot")
	
	# Draw the center line
	bottom_center = midpoint(inner_box[2], inner_box[3])
	top_center = midpoint(inner_box[1], inner_box[4])
	@layer begin
		sethue("grey")
		setopacity(0.5)
		line(bottom_center, top_center, :stroke)
	end
	
	# Show base height:
	@layer begin
		sethue(text_color)
		setopacity(1)
		scale(-1, 1)
		translate(-(x0 * 2 + inner_box_width), 0)  # translate back
		dimension(inner_box[2], Point(inner_box[2][1], inner_box[2][2] + d_h0),
			offset=25,
			fromextension=[25, 5],
			toextension=[25, 5],
			textrotation=π / 2,
			textgap=20,
			format=(d) -> string("Base:", round(h0, digits=1)))
	end

	# Show plant thickness (height - h0):
	@layer begin
		sethue(text_color)
		setopacity(1)
		scale(-1, 1)
		translate(-(x0 * 2 + inner_box_width), 0)  # translate back
		dimension(Point(inner_box[2][1], inner_box[2][2] + d_h0), inner_box[1],
			offset=25,
			fromextension=[25, 5],
			toextension=[25, 5],
			textrotation=π / 2,
			textgap=40,
			format=(d) -> string("Thickness:", round(e, digits=1))
		)
	end

	# Show interrow dimension:
	@layer begin
		sethue(text_color)
		setopacity(1)
		scale(-1, 1)
		translate(-(x0 * 2 + inner_box_width), 0)  # translate back
		dimension(inner_box[1], inner_box[4],
			offset=45,
			fromextension=[45, 5],
			toextension=[45, 5],
			textrotation=π / 2,
			textgap=40,
			format=(d) -> string("Interrow:", round(interrow, digits=1))
		)
	end

	# Show crop width:
	@layer begin
		sethue(text_color)
		setopacity(1)
		setdash("dot")
		scale(-1, 1)
		translate(-(x0 * 2 + inner_box_width), 0)  # translate back
		dimension(outer_box[2], Point(x0 + d_width / 2, y0),
			offset=-25,
			fromextension=[5, d_height + 25],
			toextension=[5, 25],
			textrotation=π / 2,
			textgap=40,
			format=(d) -> string("Width:", round(width, digits=1))
		)
	end

	# Show shaded and sunlit:
	@layer begin
		sethue(text_color)
		setopacity(1)
		setdash("dot")
		scale(-1, 1)
		translate(-(x0 * 2), 0)  # translate back
		dimension(
			Point(inner_box[2][1] - d_width / 2, inner_box[2][2]), 
			inner_box[2],
			offset=-60,
			fromextension=[5, 60],
			toextension=[5, 60],
			textrotation=π / 2,
			textgap=40,
			format=(d) -> string("Shaded:", round(surfAO, digits=1))
		)

		@layer begin
			setopacity(0.5)
			sethue("lightskyblue")
			box(
				midpoint(
					Point(inner_box[2][1] - d_width / 2, inner_box[2][2]-10),
					inner_box[2]
				),
				(inner_box[2][1] - d_width / 2) -inner_box[2][1], 10, 0; action=:fill
			)
		end
		
		sethue(text_color)
		dimension(
			Point(bottom_center[1] - inner_box_width, bottom_center[2]), Point(inner_box[2][1] - d_width / 2, y0),
			offset=-60,
			fromextension=[5, 10],
			toextension=[5, 30],
			textrotation=π / 2,
			textgap=40,
			format=(d) -> string("Sunlit:", round(surfAS, digits=1))
		)

		setopacity(0.8)
		sethue("lemonchiffon")
		box(
			midpoint(
				Point(bottom_center[1] - inner_box_width, bottom_center[2]-10),
				Point(inner_box[2][1] - d_width / 2, y0)
			),
			(bottom_center[1] - inner_box_width) - (inner_box[2][1] - d_width / 2), 10, 0; action=:fill
		)
	end

	@layer begin 
		sethue(text_color)
		setopacity(1)
		setdash("dot")
		scale(1, -1)
		rsh_pos = midpoint(
			Point(inner_box[2][1] + d_width / 2, -inner_box[2][2]),
			Point(inner_box[2][1], -inner_box[2][2])
		)
		text(
			"Rsh: $(round(rombre, digits = 1))", 
			Point(rsh_pos[1], rsh_pos[2]+70), halign=:center, valign=:middle
		)

		rsu_pos = midpoint(
			Point(inner_box[2][1] + d_width / 2, -inner_box[2][2]),
			Point(bottom_center[1], -bottom_center[2]),
		)
		text(
			"Rsu: $(round(rsoleil, digits = 1))", 
			Point(rsu_pos[1], rsu_pos[2]+70), halign=:center, valign=:middle
		)
	end

	# Drawing the left-hand side crop:
	setopacity(0.4)
	sethue("green")

	# Draw right sides of the plants
	p = half_canopy_left(shape, d_width, d_height + y0, d_h0 + y0, x0)
	
	# Outter right side of the right-hand plant
	@layer begin
		setopacity(0.15)
		sethue("green")
		setdash("dot")
		translate(inner_box_width, 0)
		poly(p, :fill, close=true)
	end

	# Draw left sides of the plants
	@layer begin
		scale(-1, 1) # mirror the scene
		translate(-(x0 * 2 + inner_box_width), 0)  # translate back
		poly(p, :fill, close=true) # Inner left side of the LHS plant
		setopacity(0.15)
		sethue("green")
		setdash("dot")
		translate(inner_box_width, 0)  # translate back
		poly(p, :fill, close=true) # Outter left side of the LHS plant
	end

	if display_text
		@layer begin
			sethue(text_color)
			setopacity(1)
			scale(1, -1) # to set the y axis up
			setdash("dot")
			label(
				"Row center", :S, Point(bottom_center[1], -bottom_center[2]),
				offset=40, leader=true, leaderoffsets=[0.1, 0.95]
			)
		end
	end

	all_point_pos_m = (1:n_sample_points) ./ n_sample_points .* (interrow / 2.0)
	point_pos_m = all_point_pos_m[i_sample_point]
	all_point_pos = rescale.(all_point_pos_m, 0, interrow, x0, inner_box[4][1]) # Point position on the plane coords
	point_pos = all_point_pos[i_sample_point]
	all_sample_points = Point.(all_point_pos, y0)
	sample_point = all_sample_points[i_sample_point] # Point coordinates

	# Compute direct light:
	# Get the value of θ1 and θ2, the angles relative to the vertical plane on the sample_point
	# that give the view angle of the direct light comming from the sky:
	kgdirect, θ1, θ2 = kdir(latitude_r, j, width, point_pos_m, interrow, shape, h0, alpha, e) 

	# Compute P1 and P2, the two points on the sky that provide the direct light view angle:

	θ1_soil = π/2 + θ1
	θ2_soil = π/2 + θ2

	# Draw direct light directions (redundant with one below):
	# @layer begin
	# 	center_point = Point3D(sample_point[1], sample_point[2], 0.0)
	# 	end_line = Point3D(sample_point[1] + 800, sample_point[2], 0.0)
	# 	end_line_point1 = rotateby(end_line, center_point, RotXYZ(0, 0.0, θ1_soil))
	# 	sethue("red")
	# 	setdash("solid")
	# 	arrow(Point(center_point[1], center_point[2]), Point(end_line_point1[1], 
	# 	end_line_point1[2]))
	# 	end_line_point2 = rotateby(end_line, center_point, RotXYZ(0, 0.0, θ2_soil))
		
	# 	arrow(Point(center_point[1], center_point[2]), Point(end_line_point2[1], end_line_point2[2]))
		
	# end

	XP1 = X_from_θ(θ1_soil, height)
	XP2 = X_from_θ(θ2_soil, height)

	P1_d = Point(x0 + ((point_pos_m + XP1) / interrow) * (inner_box[4][1] - x0), y0 + d_height)
	P2_d = Point(x0 + ((point_pos_m + XP2) / interrow) * (inner_box[4][1] - x0), y0 + d_height)
		
	setopacity(0.3)
	sethue("yellow")
	poly(
		[
			P1_d,
			sample_point,
			P2_d
		],
		:fill,
		close=true
	)

	text_point = midpoint(P1_d, P2_d)

	# Add kdir text:
	if display_text && kgdirect > 6.0e-10
		@layer begin
			sethue(text_color)
			setopacity(1)
			scale(1, -1)
			dimension(Point(P1_d[1], -P1_d[2]), Point(P2_d[1], -P2_d[2]),
				offset=15,
				fromextension=[15, 5],
				toextension=[15, 5],
				textrotation=π / 2,
				textgap=40,
				format=(d) -> string("kdir: ", round(kgdirect, digits=2)))
		end
	end

	# Compute transmitted light:
	sethue("goldenrod")
	(p_trans_left, p_trans_right) = draw_transmitted_light(sample_point, p, inner_box, d_width, d_h0)

	# Theta angle :
	#   @layer begin
	#       sethue("black")
	#       setopacity(0.5)
	#       setdash("solid")
	# H1 = anglethreepoints(p_trans_left[end-1],p_trans_left[1], p_trans_left[2])
	# newpath()
	# (p_trans_left, p_trans_right)
	# arc(sample_point, 50, 0, cos(π/2 + H1), :path)
	# p_arc = pathtopoly()[1]
	# poly(p_arc, :stroke)

	# @layer begin
	#           scale(1, -1)
	# 	mid_p_arc = midpoint(p_arc[1], p_arc[end])
	# 	label(string("H2: ",round(rad2deg(H1), digits=2), "°"), :NE, Point(mid_p_arc[1], -mid_p_arc[2]),offset=10)
	# 	# offset=10, leader=false, leaderoffsets=[0.4, 0.9]
	#       end

	#   end

	# Writting Ri:
	@layer begin
		fontsize(16)
		fontface("Calibri Bold")
		scale(1, -1)
		p_Ri = Point(inner_box[3][1] + inner_box_width*ri_text_pos[1], -inner_box[3][2] - inner_box_length * ri_text_pos[2])
		setopacity(1)
		sethue("white")
		box(p_Ri, 150, 30, 5, :fill)
		setopacity(0.8)
		sethue("black")
		text("Ri: $(round(raint, digits = 1)) MJ m⁻² day⁻¹", p_Ri, halign = :center, valign = :middle)
	end

	# Compute diffuse light:
	text_point = midpoint(p[1], Point(inner_box[4][1] - d_width / 2, inner_box[4][2]))
	kgdiffus, H1, H2, az1, az2 = kdif(point_pos_m, h0, width, interrow, height - h0, shape)

	if diffuse_angles
		@layer begin
			setdash("solid")
			setopacity(1)
			setline(1)
			# RHS:
			center_point, end_line_points_right = draw_diffuse_angles_3d(sample_point, H1, π / 2 .- az1, inner_box_length * 0.3, colormap)
			# LHS:
			center_point, end_line_points_left = draw_diffuse_angles_3d(sample_point, π / 2 .- H2, -π / 2 .- az2, 100, colormap)
		end
	end

	if display_text
		# Show kdif from top:
		@layer begin
			sethue(text_color)
			setopacity(1)
			scale(-1, 1)
			translate(-(x0 * 2 + inner_box_width), 0)  # translate back
			dimension(Point(p[1][1] + d_width / 2, p[1][2]), Point(inner_box[4][1] - d_width / 2, inner_box[4][2]),
				offset=30,
				fromextension=[30, 5],
				toextension=[30, 5],
				textrotation=π / 2,
				textgap=40,
				format=(d) -> string("kdif: ", round(kgdiffus, digits=2)))
		end
	end

	@layer begin
		for (i, v) in enumerate(all_sample_points)
			sethue("black")
			setdash("solid")
			setline(0.5)
			setopacity(0.8)
			i != i_sample_point && line(v, Point(v[1], v[2] - 3), :stroke)

			if display_text && i == 1 || mod(i, 25) == 0
				@layer begin
					sethue("black")
					setopacity(0.8)
					fontsize(9)
					scale(1, -1) # to set the y axis up
					label(
						string(i), :S, Point(v[1], -v[2]),
						offset=5, leader=false
					)
				end
			end

		end
	end

	if display_text
		@layer begin
			sethue(text_color)
			setopacity(1)
			scale(1, -1) # to set the y axis up
			setdash("dot")
			label(
				"Sample point", :S, Point(sample_point[1], -sample_point[2]),
				offset=25, leader=true, leaderoffsets=[0.5, 0.9]
			)
		end
	end	
end

# ╔═╡ 4eb15ffa-5218-4d30-a9ec-4c6f6d0a4524
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
                Point(x0, height),
                Point(x0 + width / 2.0, height),
                Point(x0, h0)
            ],
            :fill,
            close=true
        )
    elseif shape == :utriangle
        p = poly(
            [
                Point(x0, height),
                Point(x0, h0),
                Point(x0 + width / 2.0, h0),
            ],
            :fill,
            close=true
        )
    elseif shape == :rectangle
        p = poly(
            [
                Point(x0, height),
                Point(x0, h0),
                Point(x0 + width / 2.0, h0),
                Point(x0 + width / 2.0, height)
            ],
            :fill,
            close=true
        )
    end
    return p
end

# ╔═╡ b777571c-91b2-4c80-a3bb-1bc65f48fbc8
"""
	draw_transmitted_light(sample_point,p,inner_box,d_width,d_h0)

Draw the transmitted light using the plants dimensions.
"""
function draw_transmitted_light(sample_point, p, inner_box, d_width, d_h0)
    # Transmitted light: Drawing the left triangle between θ1 and the horizontal
    corner_left = inner_box[2]
    plant_x = [i[1] for i in p]
    plant_y = [i[2] for i in p]
    plant_base_points = sort(p[plant_y.==minimum(plant_y)])
    plant_top_points = p[plant_y.==maximum(plant_y)]
    plant_top_point = sort(plant_top_points)[end]
	
	if length(plant_top_points) == 1
		# Case of the utriangle, the shade is casted by the top point until limite2
		limite2 = minimum(plant_x) + d_width / 2 * (d_h0 / (maximum(plant_y) - corner_left[2] - d_h0) + 1)
		if sample_point[1] > limite2
			below_limit = false
		else
			below_limit = true
		end
	elseif sample_point[1] < maximum(plant_x[plant_y.==minimum(plant_y)])
		below_limit = true
	else
		below_limit = false
	end
	
	if below_limit
	    p_outline = poly(
	        [
	            sample_point,
	            corner_left,
	            plant_base_points...,
	            sample_point
	        ],
	        :fill
	    )
	else
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
	end

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

    (p_outline, p_outline_right)
end

# ╔═╡ 172d2086-efb1-4805-b75e-7801072347f4
"""
	X_from_θ(θ, sky_height, x, side)

Get the point X position (in m) of a ray in the sky giving the angle `θ` and the sky heigth (`sky_height`).
"""
function X_from_θ(θ, sky_height)
    # This is the X position of P in m relative to the sample point X
    return sky_height / tan(θ)
end

# ╔═╡ fe676fa3-bc50-493d-b41f-55fdcba91d83
"""
	draw_diffuse_angles_3d(sample_point, H, az, colormap = :vik)

Draw the arrows that define the angles at which the sample point sees the sky. Note that the lines are drawn in 2D but computed in the 3D space.

#### Arguments

- `sample_point`: sample point position
- `H`: Plant height angle
- `az`: azimuthal angle
- `a_length`: length of the arrows (in drawing units)
- `colormap = :vik`: colormap for the arrows


#### Notes

Used in the computation of the diffuse light interception where each angle that sees sky cumulates a proportion of the sky viewed. The function is applied one side after the other (right and left).
"""
function draw_diffuse_angles_3d(sample_point, H, az, a_length=100, colormap=colorschemes[:vik])
    center_point = Point3D(sample_point[1], sample_point[2], 0)
    end_line = Point3D(sample_point[1] + a_length, sample_point[2], 0)
    end_line_points = fill(end_line, length(H))
    for i in 1:length(H)
        @layer begin
            # sethue(get(colorschemes[colormap], rescale(az[i], 0.0, π)))
            end_line_point = rotateby(end_line, center_point, RotXYZ(0, az[i], H[i]))
            sethue(get(colormap, rescale(end_line_point[3], -a_length, a_length)))
            # pin(center_point,end_line_point)
            arrow(Point(center_point[1], center_point[2]), Point(end_line_point[1], end_line_point[2]))
            end_line_points[i] = end_line_point
        end
    end
    return center_point, end_line_points
end

# ╔═╡ d02a0cb0-7e61-4d6b-a2b8-ace9ef94e4fc
# This docstring is commented because of an issue in Pluto. See https://github.com/fonsp/Pluto.jl/issues/2811 and https://github.com/fonsp/Pluto.jl/issues/2838
#"""
#	DataFrameInput(data_frame_input, combine_funct; title="")

#Make a DataFrame of binded widgets out of a Pluto `combine` array.

#Adapted from [this code](https://github.com/jeremiahpslewis/PlutoMiscellany.jl/blob/main/notebooks/DataFrameInput_Widget.jl) from the Github account **@jeremiahpslewis**.
#"""
function DataFrameInput(data_frame_input, combine_funct; title="")
    table_header = []
    table_body = []
    col_names = [@htl("<th>$(col_name)</th>") for col_name in names(data_frame_input)]

    function cell_element(row, cell)
        if isa(cell, Slider) | isa(cell, Scrubbable) | isa(cell, TextField) | isa(cell, RangeSlider) | isa(cell, Radio) | isa(cell, Select) | isa(cell, CheckBox)
            @htl("<td>$(combine_funct(row, cell))</td>")
        else
            @htl("<td>$(cell)</td>")
        end
    end

    table_header = @htl("$(col_names)")
    for df_row in eachrow(data_frame_input)
        row_output = [cell_element(string(UUIDs.uuid4()), cell) for cell in df_row]
        table_body = [table_body..., @htl("<tr>$(row_output)</tr>")]
    end

    @htl("""
    <h6 style="text-align: center;">$(title)</h6>
    <table>
    <thead>
    <tr>
    $(table_header)
    </tr>
    </thead>
    <tbody>
    $(table_body)
    </tbody>
    </table>
    """)
end

# ╔═╡ e6c55f6f-a8bf-423b-b3d7-49acf1cf74d0
begin
    params_ = let
        params_ = Any[
            Slider(param.second[1]; default=param.second[2], show_value=true) for param in [
                "latitude" => (-90:1:90, 44.0),
                "alpha" => (-90:1:90, 0.0),
				"day" => (1:365, 1),
                "width" => (0.05:0.05:1.0, 0.3),
                "interrow" => (0.05:0.05:2.0, 1),
                "height" => (0.05:0.05:1.0, 0.5),
				"base" => (0.05:0.05:1.0, 0.2),
                "sample_point" => (1:199.0, 100),
				"rg" => (1:30, 20),
				"k" => (0.1:0.1:1.0, 0.8),
				"lai" => (0.1:0.1:3.0, 2.0),
            ]
        ]

        push!(
            params_,
            Select(["dtriangle" => "🔻", "utriangle" => "🔺", "rectangle" => "🟥"]; default="dtriangle")
        )

        push!(
            params_,
            CheckBox(default=true)
        )
    end

    params_df = DataFrame(
        :Parameter => ["latitude", "alpha", "day", "width", "interrow", "height", "base", "sample_point", "rg", "k", "lai", "shape", "diffuse_angles"],
        :Units => ["degree", "degree", "julian day", "m", "m", "m", "m", "index", "MJ m⁻² d⁻¹","-","m² m⁻²", "-", "-"],
        Symbol("Value") => params_
    )


    @bind df_values PlutoUI.combine() do Child
        DataFrameInput(params_df, Child; title="Input Dataframe")
    end
end

# ╔═╡ a24703dc-9b43-4b9c-9f2e-11b042c67af2
params = Dict(zip(params_df.Parameter, [df_values[i] for i in 1:length(df_values)]));

# ╔═╡ 2030aa31-a8d6-4b44-b359-04a0eb45a748
begin
    # Beginning of the drawing:
    Drawing(image_dim[1], image_dim[2], :png)
	t = currentdrawing()
	scale(1, -1) # to set the y axis up
	translate(0.0,-t.height)
	tcenter = Point(t.width * 0.5, t.height * 0.55)

	draw_radiative_transfer(
		t.width, t.height, tcenter; 
		width = min(params["width"], params["interrow"]), 
		i_sample_point = Int(params["sample_point"]), 
		latitude_r = deg2rad(params["latitude"]), 
		j = params["day"], 
		interrow = params["interrow"], 
		height = params["height"], 
		diffuse_angles = params["diffuse_angles"], 
		shape = Symbol(params["shape"]), 
		h0 = params["base"],
		alpha = deg2rad(params["alpha"]),
	    rg = params["rg"], 
	    k = params["k"],
	    lai = params["lai"],
	    display_text = true,
		title_height= 0.125,
		outer_box_rel_width = 0.9,
		outer_box_rel_height = 0.60,
		ri_text_pos = (-0.15,0.1),
		text_color = "grey",
		n = ""
	)

    finish()
    preview()
end

# ╔═╡ 6d52ea68-1c71-4cc4-970b-8c9a947fc582
let
    str = ""
    if params["width"] > params["interrow"]
        str = str * "\nPlant width > interrow, will use interrow for the computation"
    end

    if params["diffuse_angles"]
        str = str * """\nDiffuse and direct angles are projected in 2D but are actually computed in the 3D space, so they can appear poiting below the canopy, while in fact pointing towards or afar from the viewer."""
    end

    if length(str) > 0
        @warn str
    end
end

# ╔═╡ 00000000-0000-0000-0000-000000000001
PLUTO_PROJECT_TOML_CONTENTS = """
[deps]
ColorSchemes = "35d6a980-a343-548e-a6ea-1d62b119f2f4"
Colors = "5ae59095-9a9b-59fe-a467-6f913c188581"
DataFrames = "a93c6f00-e57d-5684-b7b6-d8193f3e46c0"
HypertextLiteral = "ac1192a8-f4b3-4bfe-ba22-af5b92cd3ab2"
Luxor = "ae8d54c2-7ccd-5906-9d76-62fc9837b5bc"
PlutoUI = "7f904dfe-b85e-4ff6-b463-dae2292396a8"
Rotations = "6038ab10-8711-5258-84ad-4b1120ba62dc"
Thebes = "8b424ff8-82f5-59a4-86a6-de3761897198"
UUIDs = "cf7118a7-6976-5b1a-9a39-7adc72f591a4"

[compat]
ColorSchemes = "~3.21.0"
Colors = "~0.12.10"
DataFrames = "~1.5.0"
HypertextLiteral = "~0.9.4"
Luxor = "~3.7.0"
PlutoUI = "~0.7.51"
Rotations = "~1.5.1"
Thebes = "~0.9.0"
"""

# ╔═╡ 00000000-0000-0000-0000-000000000002
PLUTO_MANIFEST_TOML_CONTENTS = """
# This file is machine-generated - editing it directly is not advised

julia_version = "1.10.2"
manifest_format = "2.0"
project_hash = "587c8e114bdbaf6a80f7ef03b13fc52b53b3605e"

[[deps.AbstractPlutoDingetjes]]
deps = ["Pkg"]
git-tree-sha1 = "8eaf9f1b4921132a4cff3f36a1d9ba923b14a481"
uuid = "6e696c72-6542-2067-7265-42206c756150"
version = "1.1.4"

[[deps.ArgTools]]
uuid = "0dad84c5-d112-42e6-8d28-ef12dabb789f"
version = "1.1.1"

[[deps.Artifacts]]
uuid = "56f22d72-fd6d-98f1-02f0-08ddc0907c33"

[[deps.Base64]]
uuid = "2a0f44e3-6c83-55bd-87e4-b1978d98bd5f"

[[deps.Bzip2_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "19a35467a82e236ff51bc17a3a44b69ef35185a2"
uuid = "6e34b625-4abd-537c-b88f-471c36dfa7a0"
version = "1.0.8+0"

[[deps.Cairo]]
deps = ["Cairo_jll", "Colors", "Glib_jll", "Graphics", "Libdl", "Pango_jll"]
git-tree-sha1 = "d0b3f8b4ad16cb0a2988c6788646a5e6a17b6b1b"
uuid = "159f3aea-2a34-519c-b102-8c37f9878175"
version = "1.0.5"

[[deps.Cairo_jll]]
deps = ["Artifacts", "Bzip2_jll", "CompilerSupportLibraries_jll", "Fontconfig_jll", "FreeType2_jll", "Glib_jll", "JLLWrappers", "LZO_jll", "Libdl", "Pixman_jll", "Pkg", "Xorg_libXext_jll", "Xorg_libXrender_jll", "Zlib_jll", "libpng_jll"]
git-tree-sha1 = "4b859a208b2397a7a623a03449e4636bdb17bcf2"
uuid = "83423d85-b0ee-5818-9007-b63ccbeb887a"
version = "1.16.1+1"

[[deps.ColorSchemes]]
deps = ["ColorTypes", "ColorVectorSpace", "Colors", "FixedPointNumbers", "PrecompileTools", "Random"]
git-tree-sha1 = "be6ab11021cd29f0344d5c4357b163af05a48cba"
uuid = "35d6a980-a343-548e-a6ea-1d62b119f2f4"
version = "3.21.0"

[[deps.ColorTypes]]
deps = ["FixedPointNumbers", "Random"]
git-tree-sha1 = "eb7f0f8307f71fac7c606984ea5fb2817275d6e4"
uuid = "3da002f7-5984-5a60-b8a6-cbb66c0b333f"
version = "0.11.4"

[[deps.ColorVectorSpace]]
deps = ["ColorTypes", "FixedPointNumbers", "LinearAlgebra", "SpecialFunctions", "Statistics", "TensorCore"]
git-tree-sha1 = "600cc5508d66b78aae350f7accdb58763ac18589"
uuid = "c3611d14-8923-5661-9e6a-0046d554d3a4"
version = "0.9.10"

[[deps.Colors]]
deps = ["ColorTypes", "FixedPointNumbers", "Reexport"]
git-tree-sha1 = "fc08e5930ee9a4e03f84bfb5211cb54e7769758a"
uuid = "5ae59095-9a9b-59fe-a467-6f913c188581"
version = "0.12.10"

[[deps.Compat]]
deps = ["UUIDs"]
git-tree-sha1 = "7a60c856b9fa189eb34f5f8a6f6b5529b7942957"
uuid = "34da2185-b29b-5c13-b0c7-acf172513d20"
version = "4.6.1"
weakdeps = ["Dates", "LinearAlgebra"]

    [deps.Compat.extensions]
    CompatLinearAlgebraExt = "LinearAlgebra"

[[deps.CompilerSupportLibraries_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "e66e0078-7015-5450-92f7-15fbd957f2ae"
version = "1.1.0+0"

[[deps.Crayons]]
git-tree-sha1 = "249fe38abf76d48563e2f4556bebd215aa317e15"
uuid = "a8cc5b0e-0ffa-5ad4-8c14-923d3ee1735f"
version = "4.1.1"

[[deps.DataAPI]]
git-tree-sha1 = "8da84edb865b0b5b0100c0666a9bc9a0b71c553c"
uuid = "9a962f9c-6df0-11e9-0e5d-c546b8b5ee8a"
version = "1.15.0"

[[deps.DataFrames]]
deps = ["Compat", "DataAPI", "Future", "InlineStrings", "InvertedIndices", "IteratorInterfaceExtensions", "LinearAlgebra", "Markdown", "Missings", "PooledArrays", "PrettyTables", "Printf", "REPL", "Random", "Reexport", "SentinelArrays", "SnoopPrecompile", "SortingAlgorithms", "Statistics", "TableTraits", "Tables", "Unicode"]
git-tree-sha1 = "aa51303df86f8626a962fccb878430cdb0a97eee"
uuid = "a93c6f00-e57d-5684-b7b6-d8193f3e46c0"
version = "1.5.0"

[[deps.DataStructures]]
deps = ["Compat", "InteractiveUtils", "OrderedCollections"]
git-tree-sha1 = "d1fff3a548102f48987a52a2e0d114fa97d730f0"
uuid = "864edb3b-99cc-5e75-8d2d-829cb0a9cfe8"
version = "0.18.13"

[[deps.DataValueInterfaces]]
git-tree-sha1 = "bfc1187b79289637fa0ef6d4436ebdfe6905cbd6"
uuid = "e2d170a0-9d28-54be-80f0-106bbe20a464"
version = "1.0.0"

[[deps.Dates]]
deps = ["Printf"]
uuid = "ade2ca70-3891-5945-98fb-dc099432e06a"

[[deps.DocStringExtensions]]
deps = ["LibGit2"]
git-tree-sha1 = "2fb1e02f2b635d0845df5d7c167fec4dd739b00d"
uuid = "ffbed154-4ef7-542d-bbb7-c09d3a79fcae"
version = "0.9.3"

[[deps.Downloads]]
deps = ["ArgTools", "FileWatching", "LibCURL", "NetworkOptions"]
uuid = "f43a241f-c20a-4ad4-852c-f6b1247861c6"
version = "1.6.0"

[[deps.Expat_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "bad72f730e9e91c08d9427d5e8db95478a3c323d"
uuid = "2e619515-83b5-522b-bb60-26c02a35a201"
version = "2.4.8+0"

[[deps.FFMPEG]]
deps = ["FFMPEG_jll"]
git-tree-sha1 = "b57e3acbe22f8484b4b5ff66a7499717fe1a9cc8"
uuid = "c87230d0-a227-11e9-1b43-d7ebe4e7570a"
version = "0.4.1"

[[deps.FFMPEG_jll]]
deps = ["Artifacts", "Bzip2_jll", "FreeType2_jll", "FriBidi_jll", "JLLWrappers", "LAME_jll", "Libdl", "Ogg_jll", "OpenSSL_jll", "Opus_jll", "PCRE2_jll", "Pkg", "Zlib_jll", "libaom_jll", "libass_jll", "libfdk_aac_jll", "libvorbis_jll", "x264_jll", "x265_jll"]
git-tree-sha1 = "74faea50c1d007c85837327f6775bea60b5492dd"
uuid = "b22a6f82-2f65-5046-a5b2-351ab43fb4e5"
version = "4.4.2+2"

[[deps.FileIO]]
deps = ["Pkg", "Requires", "UUIDs"]
git-tree-sha1 = "299dc33549f68299137e51e6d49a13b5b1da9673"
uuid = "5789e2e9-d7fb-5bc7-8068-2c6fae9b9549"
version = "1.16.1"

[[deps.FileWatching]]
uuid = "7b1f6079-737a-58dc-b8bc-7a2ca5c1b5ee"

[[deps.FixedPointNumbers]]
deps = ["Statistics"]
git-tree-sha1 = "335bfdceacc84c5cdf16aadc768aa5ddfc5383cc"
uuid = "53c48c17-4a7d-5ca2-90c5-79b7896eea93"
version = "0.8.4"

[[deps.Fontconfig_jll]]
deps = ["Artifacts", "Bzip2_jll", "Expat_jll", "FreeType2_jll", "JLLWrappers", "Libdl", "Libuuid_jll", "Pkg", "Zlib_jll"]
git-tree-sha1 = "21efd19106a55620a188615da6d3d06cd7f6ee03"
uuid = "a3f928ae-7b40-5064-980b-68af3947d34b"
version = "2.13.93+0"

[[deps.Formatting]]
deps = ["Printf"]
git-tree-sha1 = "8339d61043228fdd3eb658d86c926cb282ae72a8"
uuid = "59287772-0a20-5a39-b81b-1366585eb4c0"
version = "0.4.2"

[[deps.FreeType2_jll]]
deps = ["Artifacts", "Bzip2_jll", "JLLWrappers", "Libdl", "Pkg", "Zlib_jll"]
git-tree-sha1 = "87eb71354d8ec1a96d4a7636bd57a7347dde3ef9"
uuid = "d7e528f0-a631-5988-bf34-fe36492bcfd7"
version = "2.10.4+0"

[[deps.FriBidi_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "aa31987c2ba8704e23c6c8ba8a4f769d5d7e4f91"
uuid = "559328eb-81f9-559d-9380-de523a88c83c"
version = "1.0.10+0"

[[deps.Future]]
deps = ["Random"]
uuid = "9fa8497b-333b-5362-9e8d-4d0656e87820"

[[deps.Gettext_jll]]
deps = ["Artifacts", "CompilerSupportLibraries_jll", "JLLWrappers", "Libdl", "Libiconv_jll", "Pkg", "XML2_jll"]
git-tree-sha1 = "9b02998aba7bf074d14de89f9d37ca24a1a0b046"
uuid = "78b55507-aeef-58d4-861c-77aaff3498b1"
version = "0.21.0+0"

[[deps.Glib_jll]]
deps = ["Artifacts", "Gettext_jll", "JLLWrappers", "Libdl", "Libffi_jll", "Libiconv_jll", "Libmount_jll", "PCRE2_jll", "Pkg", "Zlib_jll"]
git-tree-sha1 = "d3b3624125c1474292d0d8ed0f65554ac37ddb23"
uuid = "7746bdde-850d-59dc-9ae8-88ece973131d"
version = "2.74.0+2"

[[deps.Graphics]]
deps = ["Colors", "LinearAlgebra", "NaNMath"]
git-tree-sha1 = "d61890399bc535850c4bf08e4e0d3a7ad0f21cbd"
uuid = "a2bd30eb-e257-5431-a919-1863eab51364"
version = "1.1.2"

[[deps.Graphite2_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "344bf40dcab1073aca04aa0df4fb092f920e4011"
uuid = "3b182d85-2403-5c21-9c21-1e1f0cc25472"
version = "1.3.14+0"

[[deps.HarfBuzz_jll]]
deps = ["Artifacts", "Cairo_jll", "Fontconfig_jll", "FreeType2_jll", "Glib_jll", "Graphite2_jll", "JLLWrappers", "Libdl", "Libffi_jll", "Pkg"]
git-tree-sha1 = "129acf094d168394e80ee1dc4bc06ec835e510a3"
uuid = "2e76f6c2-a576-52d4-95c1-20adfe4de566"
version = "2.8.1+1"

[[deps.Hyperscript]]
deps = ["Test"]
git-tree-sha1 = "8d511d5b81240fc8e6802386302675bdf47737b9"
uuid = "47d2ed2b-36de-50cf-bf87-49c2cf4b8b91"
version = "0.0.4"

[[deps.HypertextLiteral]]
deps = ["Tricks"]
git-tree-sha1 = "c47c5fa4c5308f27ccaac35504858d8914e102f9"
uuid = "ac1192a8-f4b3-4bfe-ba22-af5b92cd3ab2"
version = "0.9.4"

[[deps.IOCapture]]
deps = ["Logging", "Random"]
git-tree-sha1 = "d75853a0bdbfb1ac815478bacd89cd27b550ace6"
uuid = "b5f81e59-6552-4d32-b1f0-c071b021bf89"
version = "0.2.3"

[[deps.InlineStrings]]
deps = ["Parsers"]
git-tree-sha1 = "9cc2baf75c6d09f9da536ddf58eb2f29dedaf461"
uuid = "842dd82b-1e85-43dc-bf29-5d0ee9dffc48"
version = "1.4.0"

[[deps.InteractiveUtils]]
deps = ["Markdown"]
uuid = "b77e0a4c-d291-57a0-90e8-8db25a27a240"

[[deps.InvertedIndices]]
git-tree-sha1 = "0dc7b50b8d436461be01300fd8cd45aa0274b038"
uuid = "41ab1584-1d38-5bbf-9106-f11c6c58b48f"
version = "1.3.0"

[[deps.IrrationalConstants]]
git-tree-sha1 = "630b497eafcc20001bba38a4651b327dcfc491d2"
uuid = "92d709cd-6900-40b7-9082-c6be49f344b6"
version = "0.2.2"

[[deps.IteratorInterfaceExtensions]]
git-tree-sha1 = "a3f24677c21f5bbe9d2a714f95dcd58337fb2856"
uuid = "82899510-4779-5014-852e-03e436cf321d"
version = "1.0.0"

[[deps.JLLWrappers]]
deps = ["Preferences"]
git-tree-sha1 = "abc9885a7ca2052a736a600f7fa66209f96506e1"
uuid = "692b3bcd-3c85-4b1f-b108-f13ce0eb3210"
version = "1.4.1"

[[deps.JSON]]
deps = ["Dates", "Mmap", "Parsers", "Unicode"]
git-tree-sha1 = "31e996f0a15c7b280ba9f76636b3ff9e2ae58c9a"
uuid = "682c06a0-de6a-54ab-a142-c8b1cf79cde6"
version = "0.21.4"

[[deps.JpegTurbo_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "6f2675ef130a300a112286de91973805fcc5ffbc"
uuid = "aacddb02-875f-59d6-b918-886e6ef4fbf8"
version = "2.1.91+0"

[[deps.Juno]]
deps = ["Base64", "Logging", "Media", "Profile"]
git-tree-sha1 = "07cb43290a840908a771552911a6274bc6c072c7"
uuid = "e5e0dc1b-0480-54bc-9374-aad01c23163d"
version = "0.8.4"

[[deps.LAME_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "f6250b16881adf048549549fba48b1161acdac8c"
uuid = "c1c5ebd0-6772-5130-a774-d5fcae4a789d"
version = "3.100.1+0"

[[deps.LERC_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "bf36f528eec6634efc60d7ec062008f171071434"
uuid = "88015f11-f218-50d7-93a8-a6af411a945d"
version = "3.0.0+1"

[[deps.LZO_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "e5b909bcf985c5e2605737d2ce278ed791b89be6"
uuid = "dd4b983a-f0e5-5f8d-a1b7-129d4a5fb1ac"
version = "2.10.1+0"

[[deps.LaTeXStrings]]
git-tree-sha1 = "f2355693d6778a178ade15952b7ac47a4ff97996"
uuid = "b964fa9f-0449-5b57-a5c2-d3ea65f4040f"
version = "1.3.0"

[[deps.LibCURL]]
deps = ["LibCURL_jll", "MozillaCACerts_jll"]
uuid = "b27032c2-a3e7-50c8-80cd-2d36dbcbfd21"
version = "0.6.4"

[[deps.LibCURL_jll]]
deps = ["Artifacts", "LibSSH2_jll", "Libdl", "MbedTLS_jll", "Zlib_jll", "nghttp2_jll"]
uuid = "deac9b47-8bc7-5906-a0fe-35ac56dc84c0"
version = "8.4.0+0"

[[deps.LibGit2]]
deps = ["Base64", "LibGit2_jll", "NetworkOptions", "Printf", "SHA"]
uuid = "76f85450-5226-5b5a-8eaa-529ad045b433"

[[deps.LibGit2_jll]]
deps = ["Artifacts", "LibSSH2_jll", "Libdl", "MbedTLS_jll"]
uuid = "e37daf67-58a4-590a-8e99-b0245dd2ffc5"
version = "1.6.4+0"

[[deps.LibSSH2_jll]]
deps = ["Artifacts", "Libdl", "MbedTLS_jll"]
uuid = "29816b5a-b9ab-546f-933c-edad1886dfa8"
version = "1.11.0+1"

[[deps.Libdl]]
uuid = "8f399da3-3557-5675-b5ff-fb832c97cbdb"

[[deps.Libffi_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "0b4a5d71f3e5200a7dff793393e09dfc2d874290"
uuid = "e9f186c6-92d2-5b65-8a66-fee21dc1b490"
version = "3.2.2+1"

[[deps.Libgcrypt_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Libgpg_error_jll", "Pkg"]
git-tree-sha1 = "64613c82a59c120435c067c2b809fc61cf5166ae"
uuid = "d4300ac3-e22c-5743-9152-c294e39db1e4"
version = "1.8.7+0"

[[deps.Libgpg_error_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "c333716e46366857753e273ce6a69ee0945a6db9"
uuid = "7add5ba3-2f88-524e-9cd5-f83b8a55f7b8"
version = "1.42.0+0"

[[deps.Libiconv_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "c7cb1f5d892775ba13767a87c7ada0b980ea0a71"
uuid = "94ce4f54-9a6c-5748-9c1c-f9c7231a4531"
version = "1.16.1+2"

[[deps.Libmount_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "9c30530bf0effd46e15e0fdcf2b8636e78cbbd73"
uuid = "4b2f31a3-9ecc-558c-b454-b3730dcb73e9"
version = "2.35.0+0"

[[deps.Librsvg_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pango_jll", "Pkg", "gdk_pixbuf_jll"]
git-tree-sha1 = "ae0923dab7324e6bc980834f709c4cd83dd797ed"
uuid = "925c91fb-5dd6-59dd-8e8c-345e74382d89"
version = "2.54.5+0"

[[deps.Libtiff_jll]]
deps = ["Artifacts", "JLLWrappers", "JpegTurbo_jll", "LERC_jll", "Libdl", "Pkg", "Zlib_jll", "Zstd_jll"]
git-tree-sha1 = "3eb79b0ca5764d4799c06699573fd8f533259713"
uuid = "89763e89-9b03-5906-acba-b20f662cd828"
version = "4.4.0+0"

[[deps.Libuuid_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "7f3efec06033682db852f8b3bc3c1d2b0a0ab066"
uuid = "38a345b3-de98-5d2b-a5d3-14cd9215e700"
version = "2.36.0+0"

[[deps.LinearAlgebra]]
deps = ["Libdl", "OpenBLAS_jll", "libblastrampoline_jll"]
uuid = "37e2e46d-f89d-539d-b4ee-838fcccc9c8e"

[[deps.LogExpFunctions]]
deps = ["DocStringExtensions", "IrrationalConstants", "LinearAlgebra"]
git-tree-sha1 = "0a1b7c2863e44523180fdb3146534e265a91870b"
uuid = "2ab3a3ac-af41-5b50-aa03-7779005ae688"
version = "0.3.23"

    [deps.LogExpFunctions.extensions]
    LogExpFunctionsChainRulesCoreExt = "ChainRulesCore"
    LogExpFunctionsChangesOfVariablesExt = "ChangesOfVariables"
    LogExpFunctionsInverseFunctionsExt = "InverseFunctions"

    [deps.LogExpFunctions.weakdeps]
    ChainRulesCore = "d360d2e6-b24c-11e9-a2a3-2a2ae2dbcce4"
    ChangesOfVariables = "9e997f8a-9a97-42d5-a9f1-ce6bfc15e2c0"
    InverseFunctions = "3587e190-3f89-42d0-90ee-14403ec27112"

[[deps.Logging]]
uuid = "56ddb016-857b-54e1-b83d-db4d58db5568"

[[deps.Luxor]]
deps = ["Base64", "Cairo", "Colors", "Dates", "FFMPEG", "FileIO", "Juno", "LaTeXStrings", "Random", "Requires", "Rsvg", "SnoopPrecompile"]
git-tree-sha1 = "909a67c53fddd216d5e986d804b26b1e3c82d66d"
uuid = "ae8d54c2-7ccd-5906-9d76-62fc9837b5bc"
version = "3.7.0"

[[deps.MIMEs]]
git-tree-sha1 = "65f28ad4b594aebe22157d6fac869786a255b7eb"
uuid = "6c6e2e6c-3030-632d-7369-2d6c69616d65"
version = "0.1.4"

[[deps.MacroTools]]
deps = ["Markdown", "Random"]
git-tree-sha1 = "42324d08725e200c23d4dfb549e0d5d89dede2d2"
uuid = "1914dd2f-81c6-5fcd-8719-6d5c9610ff09"
version = "0.5.10"

[[deps.Markdown]]
deps = ["Base64"]
uuid = "d6f4376e-aef5-505a-96c1-9c027394607a"

[[deps.MbedTLS_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "c8ffd9c3-330d-5841-b78e-0817d7145fa1"
version = "2.28.2+1"

[[deps.Media]]
deps = ["MacroTools", "Test"]
git-tree-sha1 = "75a54abd10709c01f1b86b84ec225d26e840ed58"
uuid = "e89f7d12-3494-54d1-8411-f7d8b9ae1f27"
version = "0.5.0"

[[deps.Missings]]
deps = ["DataAPI"]
git-tree-sha1 = "f66bdc5de519e8f8ae43bdc598782d35a25b1272"
uuid = "e1d29d7a-bbdc-5cf2-9ac0-f12de2c33e28"
version = "1.1.0"

[[deps.Mmap]]
uuid = "a63ad114-7e13-5084-954f-fe012c677804"

[[deps.MozillaCACerts_jll]]
uuid = "14a3606d-f60d-562e-9121-12d972cd8159"
version = "2023.1.10"

[[deps.NaNMath]]
deps = ["OpenLibm_jll"]
git-tree-sha1 = "0877504529a3e5c3343c6f8b4c0381e57e4387e4"
uuid = "77ba4419-2d1f-58cd-9bb1-8ffee604a2e3"
version = "1.0.2"

[[deps.NetworkOptions]]
uuid = "ca575930-c2e3-43a9-ace4-1e988b2c1908"
version = "1.2.0"

[[deps.Ogg_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "887579a3eb005446d514ab7aeac5d1d027658b8f"
uuid = "e7412a2a-1a6e-54c0-be00-318e2571c051"
version = "1.3.5+1"

[[deps.OpenBLAS_jll]]
deps = ["Artifacts", "CompilerSupportLibraries_jll", "Libdl"]
uuid = "4536629a-c528-5b80-bd46-f80d51c5b363"
version = "0.3.23+4"

[[deps.OpenLibm_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "05823500-19ac-5b8b-9628-191a04bc5112"
version = "0.8.1+2"

[[deps.OpenSSL_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "9ff31d101d987eb9d66bd8b176ac7c277beccd09"
uuid = "458c3c95-2e84-50aa-8efc-19380b2a3a95"
version = "1.1.20+0"

[[deps.OpenSpecFun_jll]]
deps = ["Artifacts", "CompilerSupportLibraries_jll", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "13652491f6856acfd2db29360e1bbcd4565d04f1"
uuid = "efe28fd5-8261-553b-a9e1-b2916fc3738e"
version = "0.5.5+0"

[[deps.Opus_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "51a08fb14ec28da2ec7a927c4337e4332c2a4720"
uuid = "91d4177d-7536-5919-b921-800302f37372"
version = "1.3.2+0"

[[deps.OrderedCollections]]
git-tree-sha1 = "d321bf2de576bf25ec4d3e4360faca399afca282"
uuid = "bac558e1-5e72-5ebc-8fee-abe8a469f55d"
version = "1.6.0"

[[deps.PCRE2_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "efcefdf7-47ab-520b-bdef-62a2eaa19f15"
version = "10.42.0+1"

[[deps.Pango_jll]]
deps = ["Artifacts", "Cairo_jll", "Fontconfig_jll", "FreeType2_jll", "FriBidi_jll", "Glib_jll", "HarfBuzz_jll", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "84a314e3926ba9ec66ac097e3635e270986b0f10"
uuid = "36c8627f-9965-5494-a995-c6b170f724f3"
version = "1.50.9+0"

[[deps.Parsers]]
deps = ["Dates", "PrecompileTools", "UUIDs"]
git-tree-sha1 = "a5aef8d4a6e8d81f171b2bd4be5265b01384c74c"
uuid = "69de0a69-1ddd-5017-9359-2bf0b02dc9f0"
version = "2.5.10"

[[deps.Pixman_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "b4f5d02549a10e20780a24fce72bea96b6329e29"
uuid = "30392449-352a-5448-841d-b1acce4e97dc"
version = "0.40.1+0"

[[deps.Pkg]]
deps = ["Artifacts", "Dates", "Downloads", "FileWatching", "LibGit2", "Libdl", "Logging", "Markdown", "Printf", "REPL", "Random", "SHA", "Serialization", "TOML", "Tar", "UUIDs", "p7zip_jll"]
uuid = "44cfe95a-1eb2-52ea-b672-e2afdf69b78f"
version = "1.10.0"

[[deps.PlutoUI]]
deps = ["AbstractPlutoDingetjes", "Base64", "ColorTypes", "Dates", "FixedPointNumbers", "Hyperscript", "HypertextLiteral", "IOCapture", "InteractiveUtils", "JSON", "Logging", "MIMEs", "Markdown", "Random", "Reexport", "URIs", "UUIDs"]
git-tree-sha1 = "b478a748be27bd2f2c73a7690da219d0844db305"
uuid = "7f904dfe-b85e-4ff6-b463-dae2292396a8"
version = "0.7.51"

[[deps.PooledArrays]]
deps = ["DataAPI", "Future"]
git-tree-sha1 = "a6062fe4063cdafe78f4a0a81cfffb89721b30e7"
uuid = "2dfb63ee-cc39-5dd5-95bd-886bf059d720"
version = "1.4.2"

[[deps.PrecompileTools]]
deps = ["Preferences"]
git-tree-sha1 = "259e206946c293698122f63e2b513a7c99a244e8"
uuid = "aea7be01-6a6a-4083-8856-8a6e6704d82a"
version = "1.1.1"

[[deps.Preferences]]
deps = ["TOML"]
git-tree-sha1 = "7eb1686b4f04b82f96ed7a4ea5890a4f0c7a09f1"
uuid = "21216c6a-2e73-6563-6e65-726566657250"
version = "1.4.0"

[[deps.PrettyTables]]
deps = ["Crayons", "Formatting", "LaTeXStrings", "Markdown", "Reexport", "StringManipulation", "Tables"]
git-tree-sha1 = "213579618ec1f42dea7dd637a42785a608b1ea9c"
uuid = "08abe8d2-0d0c-5749-adfa-8a2ac140af0d"
version = "2.2.4"

[[deps.Printf]]
deps = ["Unicode"]
uuid = "de0858da-6303-5e67-8744-51eddeeeb8d7"

[[deps.Profile]]
deps = ["Printf"]
uuid = "9abbd945-dff8-562f-b5e8-e1ebf5ef1b79"

[[deps.Quaternions]]
deps = ["LinearAlgebra", "Random", "RealDot"]
git-tree-sha1 = "da095158bdc8eaccb7890f9884048555ab771019"
uuid = "94ee1d12-ae83-5a48-8b1c-48b8ff168ae0"
version = "0.7.4"

[[deps.REPL]]
deps = ["InteractiveUtils", "Markdown", "Sockets", "Unicode"]
uuid = "3fa0cd96-eef1-5676-8a61-b3b8758bbffb"

[[deps.Random]]
deps = ["SHA"]
uuid = "9a3f8284-a2c9-5f02-9a11-845980a1fd5c"

[[deps.RealDot]]
deps = ["LinearAlgebra"]
git-tree-sha1 = "9f0a1b71baaf7650f4fa8a1d168c7fb6ee41f0c9"
uuid = "c1ae055f-0cd5-4b69-90a6-9a35b1a98df9"
version = "0.1.0"

[[deps.Reexport]]
git-tree-sha1 = "45e428421666073eab6f2da5c9d310d99bb12f9b"
uuid = "189a3867-3050-52da-a836-e630ba90ab69"
version = "1.2.2"

[[deps.Requires]]
deps = ["UUIDs"]
git-tree-sha1 = "838a3a4188e2ded87a4f9f184b4b0d78a1e91cb7"
uuid = "ae029012-a4dd-5104-9daa-d747884805df"
version = "1.3.0"

[[deps.Rotations]]
deps = ["LinearAlgebra", "Quaternions", "Random", "StaticArrays"]
git-tree-sha1 = "54ccb4dbab4b1f69beb255a2c0ca5f65a9c82f08"
uuid = "6038ab10-8711-5258-84ad-4b1120ba62dc"
version = "1.5.1"

[[deps.Rsvg]]
deps = ["Cairo", "Glib_jll", "Librsvg_jll"]
git-tree-sha1 = "3d3dc66eb46568fb3a5259034bfc752a0eb0c686"
uuid = "c4c386cf-5103-5370-be45-f3a111cca3b8"
version = "1.0.0"

[[deps.SHA]]
uuid = "ea8e919c-243c-51af-8825-aaa63cd721ce"
version = "0.7.0"

[[deps.SentinelArrays]]
deps = ["Dates", "Random"]
git-tree-sha1 = "77d3c4726515dca71f6d80fbb5e251088defe305"
uuid = "91c51154-3ec4-41a3-a24f-3f23e20d615c"
version = "1.3.18"

[[deps.Serialization]]
uuid = "9e88b42a-f829-5b0c-bbe9-9e923198166b"

[[deps.SnoopPrecompile]]
deps = ["Preferences"]
git-tree-sha1 = "e760a70afdcd461cf01a575947738d359234665c"
uuid = "66db9d55-30c0-4569-8b51-7e840670fc0c"
version = "1.0.3"

[[deps.Sockets]]
uuid = "6462fe0b-24de-5631-8697-dd941f90decc"

[[deps.SortingAlgorithms]]
deps = ["DataStructures"]
git-tree-sha1 = "a4ada03f999bd01b3a25dcaa30b2d929fe537e00"
uuid = "a2af1166-a08f-5f64-846c-94a0d3cef48c"
version = "1.1.0"

[[deps.SparseArrays]]
deps = ["Libdl", "LinearAlgebra", "Random", "Serialization", "SuiteSparse_jll"]
uuid = "2f01184e-e22b-5df5-ae63-d93ebab69eaf"
version = "1.10.0"

[[deps.SpecialFunctions]]
deps = ["IrrationalConstants", "LogExpFunctions", "OpenLibm_jll", "OpenSpecFun_jll"]
git-tree-sha1 = "ef28127915f4229c971eb43f3fc075dd3fe91880"
uuid = "276daf66-3868-5448-9aa4-cd146d93841b"
version = "2.2.0"

    [deps.SpecialFunctions.extensions]
    SpecialFunctionsChainRulesCoreExt = "ChainRulesCore"

    [deps.SpecialFunctions.weakdeps]
    ChainRulesCore = "d360d2e6-b24c-11e9-a2a3-2a2ae2dbcce4"

[[deps.StaticArrays]]
deps = ["LinearAlgebra", "Random", "StaticArraysCore", "Statistics"]
git-tree-sha1 = "8982b3607a212b070a5e46eea83eb62b4744ae12"
uuid = "90137ffa-7385-5640-81b9-e52037218182"
version = "1.5.25"

[[deps.StaticArraysCore]]
git-tree-sha1 = "6b7ba252635a5eff6a0b0664a41ee140a1c9e72a"
uuid = "1e83bf80-4336-4d27-bf5d-d5a4f845583c"
version = "1.4.0"

[[deps.Statistics]]
deps = ["LinearAlgebra", "SparseArrays"]
uuid = "10745b16-79ce-11e8-11f9-7d13ad32a3b2"
version = "1.10.0"

[[deps.StringManipulation]]
git-tree-sha1 = "46da2434b41f41ac3594ee9816ce5541c6096123"
uuid = "892a3eda-7b42-436c-8928-eab12a02cf0e"
version = "0.3.0"

[[deps.SuiteSparse_jll]]
deps = ["Artifacts", "Libdl", "libblastrampoline_jll"]
uuid = "bea87d4a-7f5b-5778-9afe-8cc45184846c"
version = "7.2.1+1"

[[deps.TOML]]
deps = ["Dates"]
uuid = "fa267f1f-6049-4f14-aa54-33bafae1ed76"
version = "1.0.3"

[[deps.TableTraits]]
deps = ["IteratorInterfaceExtensions"]
git-tree-sha1 = "c06b2f539df1c6efa794486abfb6ed2022561a39"
uuid = "3783bdb8-4a98-5b6b-af9a-565f29a5fe9c"
version = "1.0.1"

[[deps.Tables]]
deps = ["DataAPI", "DataValueInterfaces", "IteratorInterfaceExtensions", "LinearAlgebra", "OrderedCollections", "TableTraits", "Test"]
git-tree-sha1 = "1544b926975372da01227b382066ab70e574a3ec"
uuid = "bd369af6-aec1-5ad0-b16a-f7cc5008161c"
version = "1.10.1"

[[deps.Tar]]
deps = ["ArgTools", "SHA"]
uuid = "a4e569a6-e804-4fa4-b0f3-eef7a1d5b13e"
version = "1.10.0"

[[deps.TensorCore]]
deps = ["LinearAlgebra"]
git-tree-sha1 = "1feb45f88d133a655e001435632f019a9a1bcdb6"
uuid = "62fd8b95-f654-4bbd-a8a5-9c27f68ccd50"
version = "0.1.1"

[[deps.Test]]
deps = ["InteractiveUtils", "Logging", "Random", "Serialization"]
uuid = "8dfed614-e22c-5e08-85e1-65c5234f0b40"

[[deps.Thebes]]
deps = ["Colors", "LaTeXStrings", "Luxor", "Requires", "Rotations", "StaticArrays", "Test"]
git-tree-sha1 = "cf75860b2c79dae160b681245e9d85549e189cb4"
uuid = "8b424ff8-82f5-59a4-86a6-de3761897198"
version = "0.9.0"

[[deps.Tricks]]
git-tree-sha1 = "aadb748be58b492045b4f56166b5188aa63ce549"
uuid = "410a4b4d-49e4-4fbc-ab6d-cb71b17b3775"
version = "0.1.7"

[[deps.URIs]]
git-tree-sha1 = "074f993b0ca030848b897beff716d93aca60f06a"
uuid = "5c2747f8-b7ea-4ff2-ba2e-563bfd36b1d4"
version = "1.4.2"

[[deps.UUIDs]]
deps = ["Random", "SHA"]
uuid = "cf7118a7-6976-5b1a-9a39-7adc72f591a4"

[[deps.Unicode]]
uuid = "4ec0a83e-493e-50e2-b9ac-8f72acf5a8f5"

[[deps.XML2_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Libiconv_jll", "Pkg", "Zlib_jll"]
git-tree-sha1 = "93c41695bc1c08c46c5899f4fe06d6ead504bb73"
uuid = "02c8fc9c-b97f-50b9-bbe4-9be30ff0a78a"
version = "2.10.3+0"

[[deps.XSLT_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Libgcrypt_jll", "Libgpg_error_jll", "Libiconv_jll", "Pkg", "XML2_jll", "Zlib_jll"]
git-tree-sha1 = "91844873c4085240b95e795f692c4cec4d805f8a"
uuid = "aed1982a-8fda-507f-9586-7b0439959a61"
version = "1.1.34+0"

[[deps.Xorg_libX11_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_libxcb_jll", "Xorg_xtrans_jll"]
git-tree-sha1 = "5be649d550f3f4b95308bf0183b82e2582876527"
uuid = "4f6342f7-b3d2-589e-9d20-edeb45f2b2bc"
version = "1.6.9+4"

[[deps.Xorg_libXau_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "4e490d5c960c314f33885790ed410ff3a94ce67e"
uuid = "0c0b7dd1-d40b-584c-a123-a41640f87eec"
version = "1.0.9+4"

[[deps.Xorg_libXdmcp_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "4fe47bd2247248125c428978740e18a681372dd4"
uuid = "a3789734-cfe1-5b06-b2d0-1dd0d9d62d05"
version = "1.1.3+4"

[[deps.Xorg_libXext_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_libX11_jll"]
git-tree-sha1 = "b7c0aa8c376b31e4852b360222848637f481f8c3"
uuid = "1082639a-0dae-5f34-9b06-72781eeb8cb3"
version = "1.3.4+4"

[[deps.Xorg_libXrender_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Xorg_libX11_jll"]
git-tree-sha1 = "19560f30fd49f4d4efbe7002a1037f8c43d43b96"
uuid = "ea2f1a96-1ddc-540d-b46f-429655e07cfa"
version = "0.9.10+4"

[[deps.Xorg_libpthread_stubs_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "6783737e45d3c59a4a4c4091f5f88cdcf0908cbb"
uuid = "14d82f49-176c-5ed1-bb49-ad3f5cbd8c74"
version = "0.1.0+3"

[[deps.Xorg_libxcb_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "XSLT_jll", "Xorg_libXau_jll", "Xorg_libXdmcp_jll", "Xorg_libpthread_stubs_jll"]
git-tree-sha1 = "daf17f441228e7a3833846cd048892861cff16d6"
uuid = "c7cfdc94-dc32-55de-ac96-5a1b8d977c5b"
version = "1.13.0+3"

[[deps.Xorg_xtrans_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "79c31e7844f6ecf779705fbc12146eb190b7d845"
uuid = "c5fb5394-a638-5e4d-96e5-b29de1b5cf10"
version = "1.4.0+3"

[[deps.Zlib_jll]]
deps = ["Libdl"]
uuid = "83775a58-1f1d-513f-b197-d71354ab007a"
version = "1.2.13+1"

[[deps.Zstd_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl"]
git-tree-sha1 = "49ce682769cd5de6c72dcf1b94ed7790cd08974c"
uuid = "3161d3a3-bdf6-5164-811a-617609db77b4"
version = "1.5.5+0"

[[deps.gdk_pixbuf_jll]]
deps = ["Artifacts", "Glib_jll", "JLLWrappers", "JpegTurbo_jll", "Libdl", "Libtiff_jll", "Pkg", "Xorg_libX11_jll", "libpng_jll"]
git-tree-sha1 = "e9190f9fb03f9c3b15b9fb0c380b0d57a3c8ea39"
uuid = "da03df04-f53b-5353-a52f-6a8b0620ced0"
version = "2.42.8+0"

[[deps.libaom_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "3a2ea60308f0996d26f1e5354e10c24e9ef905d4"
uuid = "a4ae2306-e953-59d6-aa16-d00cac43593b"
version = "3.4.0+0"

[[deps.libass_jll]]
deps = ["Artifacts", "Bzip2_jll", "FreeType2_jll", "FriBidi_jll", "HarfBuzz_jll", "JLLWrappers", "Libdl", "Pkg", "Zlib_jll"]
git-tree-sha1 = "5982a94fcba20f02f42ace44b9894ee2b140fe47"
uuid = "0ac62f75-1d6f-5e53-bd7c-93b484bb37c0"
version = "0.15.1+0"

[[deps.libblastrampoline_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "8e850b90-86db-534c-a0d3-1478176c7d93"
version = "5.8.0+1"

[[deps.libfdk_aac_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "daacc84a041563f965be61859a36e17c4e4fcd55"
uuid = "f638f0a6-7fb0-5443-88ba-1cc74229b280"
version = "2.0.2+0"

[[deps.libpng_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg", "Zlib_jll"]
git-tree-sha1 = "94d180a6d2b5e55e447e2d27a29ed04fe79eb30c"
uuid = "b53b4c65-9356-5827-b1ea-8c7a1a84506f"
version = "1.6.38+0"

[[deps.libvorbis_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Ogg_jll", "Pkg"]
git-tree-sha1 = "b910cb81ef3fe6e78bf6acee440bda86fd6ae00c"
uuid = "f27f6e37-5d2b-51aa-960f-b287f2bc3b7a"
version = "1.3.7+1"

[[deps.nghttp2_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "8e850ede-7688-5339-a07c-302acd2aaf8d"
version = "1.52.0+1"

[[deps.p7zip_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "3f19e933-33d8-53b3-aaab-bd5110c3b7a0"
version = "17.4.0+2"

[[deps.x264_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "4fea590b89e6ec504593146bf8b988b2c00922b2"
uuid = "1270edf5-f2f9-52d2-97e9-ab00b5d0237a"
version = "2021.5.5+0"

[[deps.x265_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "ee567a171cce03570d77ad3a43e90218e38937a9"
uuid = "dfaa095f-4041-5dcd-9319-2fabd8486b76"
version = "3.5.0+0"
"""

# ╔═╡ Cell order:
# ╠═f5cd7710-c533-11ec-1490-a502fac92221
# ╟─6788dbbe-317e-4212-a0e8-d417a52301f6
# ╟─f92c1e63-d40f-41eb-8d58-b44b62e44ff9
# ╠═311611bd-89f9-4e34-84cb-11924e8efc2d
# ╟─4dff9014-73ff-4c32-b6ad-c936bd892588
# ╟─a24703dc-9b43-4b9c-9f2e-11b042c67af2
# ╟─e6c55f6f-a8bf-423b-b3d7-49acf1cf74d0
# ╟─dff1401d-a2e9-45c1-9e26-a46d0fa44eff
# ╟─2030aa31-a8d6-4b44-b359-04a0eb45a748
# ╟─78c00fe4-feb0-45de-b5e1-df0fae546287
# ╟─6d52ea68-1c71-4cc4-970b-8c9a947fc582
# ╟─9db4dbb1-5f92-4ce4-bd85-5a74fae7025e
# ╟─e261142a-c411-40a3-85e4-ae979a4d9506
# ╟─ab594776-ea39-48f6-9218-78c5eed58916
# ╟─53d29bf9-dab8-4586-89d3-fcbb9d6d28bc
# ╟─58ec9faa-cbf1-4e46-b4bb-420586ac7dba
# ╟─0385990f-397e-46f8-93d7-578c8ead2be3
# ╟─fbe6d054-56ff-4201-8d55-f5afcda7ec52
# ╟─09a77c7f-409d-4083-8267-d52ba0346c9c
# ╟─4ab56f65-3314-4dc4-9eb1-59f68058e435
# ╟─6d701d6c-daa5-4bf0-9ee2-cb76dfecf510
# ╟─7f777012-1203-427f-86aa-78d502fefaab
# ╟─54cda4ec-dc89-41d4-a28d-544f556c2f34
# ╟─78cc38c7-22ab-4f24-b68f-4ba0f668d253
# ╟─3c421bef-6123-4554-b2de-b8ceabaf1b39
# ╟─b90cd9e1-30ca-48be-9e7e-dd6afbce35db
# ╟─4eb15ffa-5218-4d30-a9ec-4c6f6d0a4524
# ╟─b777571c-91b2-4c80-a3bb-1bc65f48fbc8
# ╟─172d2086-efb1-4805-b75e-7801072347f4
# ╟─fe676fa3-bc50-493d-b41f-55fdcba91d83
# ╟─d02a0cb0-7e61-4d6b-a2b8-ace9ef94e4fc
# ╟─00000000-0000-0000-0000-000000000001
# ╟─00000000-0000-0000-0000-000000000002
