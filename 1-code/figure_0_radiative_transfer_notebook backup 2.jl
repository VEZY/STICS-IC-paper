### A Pluto.jl notebook ###
# v0.19.2

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
	using UUIDs
	using Thebes
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
	image_dim = (800, 400)
	h0 = 0.5
	npoints = 200
	alpha = deg2rad(10) # Crop row direction relative to north
	light_from_sky = false # if false the light stops at the inner box, else at the sky
	display_text = true # display names and values?
	n_sample_points = 200
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
*Figure 1. Diagram of the light interception computation for each sample point below the crop.*
"""

# ╔═╡ 9db4dbb1-5f92-4ce4-bd85-5a74fae7025e
md"""
Please note that the direct angles (`kdir`) are projected in 2D, but are computed in the 3D space following row orientation and sun azimuthal and zenithal angles. So it is perfectly normal to see the angles appearing *"into"* the plant crown, and that's because the angle is in fact projected towards or away from you, thus appearing drawn on top of the plant.
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


    # For the right-hand side:
	G1, G2 = get_G(x, shape, limite, h0, e, width, ir)
    for i in 1:23
		# hcrit is the ray that point to the top of the plant according to the azimuthal angle (= points to the plant at aztab = 0, and below when turning)
        # hcrit = 90 - atan(G * sin(aztab[i] - 90 / 180 * π)) / π * 180
		hcrit = atan(G1 * sin(aztab[i] / 180 * π)) / π * 180

		# push!(Hcrit1, deg2rad(hcrit)) # angles for top of the plant (RHS)
        if hcrit < htab[i]
			# This ray receives light from the sky
            kgdiffus = kgdiffus + SOCtab[i]
			push!(Hcrit1, π / 2 - deg2rad(htab[i])) # angles above the canopy (pointing to the sky)
        end
    end

    # For the left-hand side:
    # If the point is not under the plant canopy (else it is only transmitted light):
    if x > limite
        for i in 1:23
            # hcrit = atan(G2 * sin(aztab[i]/ 180 * π)) / π * 180 - 180
			hcrit = atan(G2 * sin(aztab[i] / 180 * π)) / π * 180
	
			# push!(Hcrit2, -deg2rad(hcrit)) # angles for top of the plant (LHS)
            if (hcrit < htab[i])
                kgdiffus = kgdiffus + SOCtab[i]
				push!(Hcrit2, -(π / 2 - deg2rad(htab[i]))) # angles above the canopy (pointing to the sky
            end
        end
    end

    return (kgdiffus, Hcrit1, Hcrit2)
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

        # In this case it may happen that θ1 > θ2, which means the point sees nothing
        # because the top of the righ-hand side plant is above the view angle of the point (i.e.
        # it is completely shaded.
        if θ1 < θ2
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

    kg = 0.5 * (cos(π / 2 - θ1) + cos(π / 2 + θ2))

    return (max(kg, 0.0), θ1, θ2)
end

# ╔═╡ 0385990f-397e-46f8-93d7-578c8ead2be3
"""
	r_transmitted(width, P_latitude, j, ir, shape, h0, alpha, rdif, P_ktrou, lai, eai, height)

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

# ╔═╡ b777571c-91b2-4c80-a3bb-1bc65f48fbc8
"""
	draw_transmitted_light(sample_point,p,inner_box,d_width,d_h0)

Draw the transmitted light using the plants dimensions.
"""
function draw_transmitted_light(sample_point,p,inner_box,d_width,d_h0)
	# Transmitted light: Drawing the left triangle between θ1 and the horizontal
	corner_left = inner_box[2]
	plant_x = [i[1] for i in p]
	plant_y = [i[2] for i in p]
	plant_base_points = sort(p[plant_y .== minimum(plant_y)])
	plant_top_points = p[plant_y .== maximum(plant_y)]
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

	(p_outline, p_outline_right)
end

# ╔═╡ 030d4bd1-b596-4590-b1c5-d53bdc656c7f
"""
	P_drawing(P1, orig_xmax, orig_ymax, to_xmax, to_ymax)

Rescale point P position from meters to drawing coordinates.
"""
function P_drawing(P1, orig_xmax, orig_ymax, to_xmax, to_ymax)
    # Rescale to fit the position of the point on the drawing scale
    d_P_xpos = rescale(P1[1], 0, orig_xmax, to_xmax, to_ymax)

    return Point(d_P_xpos, orig_ymax)
end

# ╔═╡ 172d2086-efb1-4805-b75e-7801072347f4
"""
	P_from_θ(θ, sky_height, x, side)

Get the point X position (in m) of a ray in the sky giving the angle `θ` (angle to the vertical), the sky heigth (`sky_height`) and the point position on the X axis.
"""
function P_from_θ(θ, sky_height, x)
    # This is the X position of P in m relative to the sample point X
    P_x = sin(θ) * sky_height / cos(θ)

    # x - P1 to get the true position in m from the relative position:
	return Point(x + P_x, 0)
end

# ╔═╡ d07ff0dc-40d5-4e03-84d3-115a891d4530
"""
	draw_diffuse_angles(sample_point, H, sky_height, sky_height_d, point_pos_m, interrow, inner_box)

Draw the lines that define the angles at which the sample point sees the sky. Note that the lines appear below the crop, but it is just a matter of perspective because they point to directions that we don't see in 2D (towards us or away from us).

Used in the computation of the diffuse light interception where each angle that sees sky cumulates a proportion of the sky viewed. The function is applied one side after the other (right and left).
"""
function draw_diffuse_angles(sample_point, H, sky_height, sky_height_d, point_pos_m, interrow, inner_box)
		@layer begin 
			P_H_m = P_from_θ.(H, sky_height, point_pos_m)
			P_H_d = P_drawing.(P_H_m, interrow, sample_point[2] + sky_height_d, inner_box[2][1], inner_box[4][1])				
	
			for i in P_H_d
				line(sample_point,i, :stroke)
			end
		end
end

# ╔═╡ d02a0cb0-7e61-4d6b-a2b8-ace9ef94e4fc
"""
	DataFrameInput(data_frame_input, combine_funct; title="")

Make a DataFrame of binded widgets out of a Pluto `combine` array.

Adapted from [this code](https://github.com/jeremiahpslewis/PlutoMiscellany.jl/blob/main/notebooks/DataFrameInput_Widget.jl) from the Github account **@jeremiahpslewis**.
"""
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
			Slider(param.second[1];default = param.second[2],show_value=true) for param in [
				"latitude" => (-90:1:90, 44),
				"day" => (1:365, 1),
				"width" => (0.05:0.05:1.0,0.2),
				"interrow" => (0.05:0.05:2.0,1),
				"height" => (0.05:0.05:1.0,0.8),
				"sample_point" => (1:199.0,100),
			]
		]

		push!(
			params_,
			Select(["dtriangle" => "🔻","utriangle" => "🔺", "rectangle" => "🟥"];default = "dtriangle")
		)

		push!(
			params_,
			CheckBox()
		)
	end
	
	params_df = DataFrame(
			:Parameter => ["latitude", "day", "width", "interrow", "height", "sample_point","shape", "diffuse_angles"],
			:Units => ["degree", "julian day", "m", "m", "m", "index","-","-"],
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
	j = params["day"]
	diffuse_angles = params["diffuse_angles"]
	latitude_r = deg2rad(params["latitude"])
	interrow = params["interrow"]
	shape = Symbol(params["shape"])
	width = min(params["width"], interrow)
	height = params["height"]
	i_sample_point = Int(params["sample_point"])
	
	# Beginning of the drawing:
	Drawing(image_dim[1], image_dim[2], :png)
	t = currentdrawing()
    background("white")
    sethue("black")
    scale(1, -1) # to set the y axis up
    translate(0, -t.height)

    center = Point(t.width * 0.5, t.height * 0.5)

    # Drawing the big box inside the plot that delimits the scene boundary:
    outer_box_rel_width = 0.8 # Width of the outter box relative to figure width
    outer_box_rel_height = 0.8 # Height of the outter box relative to figure height
    outer_box_width = outer_box_rel_width * t.width
    outer_box_height = outer_box_rel_height * t.height

    # sethue("grey")
    # setdash("dot")
    outer_box = box(center, outer_box_width, outer_box_height, :none)

    # Rescaling the crop dimensions to match the drawing coordinates:
    d_width = width * outer_box_width / (interrow + width)
    # NB: interrow + width because the outer box include plant half-width for both plants
    d_h0 = h0 * outer_box_height / (h0 + height)
    d_height = outer_box_height - d_h0

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
    inner_box_length = inner_box[3][2] - inner_box[1][2]
    # Drawing the left-hand side crop:
    setopacity(0.4)
    sethue("green")

    # Draw right sides of the plants
    p = half_canopy_left(shape, d_width, d_height, d_h0 + y0, x0)
    # NB: h0 + y0 to add the box height to the crop height

    # Outter right side of the right-hand plant
    @layer begin
        # scale(-1, 1)
        setopacity(0.1)
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

        setopacity(0.1)
        sethue("green")
        setdash("dot")
        translate(inner_box_width, 0)  # translate back
        poly(p, :fill, close=true) # Outter left side of the LHS plant
    end

    # Draw the center line
    bottom_center = midpoint(inner_box[2], inner_box[3])
    top_center = midpoint(inner_box[1], inner_box[4])
    sethue("black")
    setdash("dot")
    line(bottom_center, top_center, :stroke)

    if display_text
        @layer begin
            sethue("black")
            setopacity(1)
            scale(1, -1) # to set the y axis up
            setdash("solid")
            label(
                "Row center", :S, Point(bottom_center[1], -bottom_center[2]),
                offset=25, leader=true, leaderoffsets=[0.5, 0.9]
            )
        end
    end

    all_point_pos_m = (1:n_sample_points) ./ n_sample_points .* (interrow / 2.0)
    point_pos_m = all_point_pos_m[i_sample_point]
    all_point_pos = rescale.(all_point_pos_m, 0, interrow, x0, inner_box[4][1]) # Point position on the plane coords
    point_pos = all_point_pos[i_sample_point]
    all_sample_points = Point.(all_point_pos, y0)
    sample_point = all_sample_points[i_sample_point] # Point coordinates

    # Compute diffuse light:
    # Diffuse light: Drawing the right triangle between the vertical and θ1
    sethue("white")
    setopacity(0.2)
    poly(
        [
            Point(sample_point[1], p[2][2]),
            sample_point,
            Point(inner_box[4][1] - d_width / 2, inner_box[4][2])
        ],
        :fill,
        close=true
    )

    # Diffuse light: Drawing the left triangle between the vertical and θ2
    poly(
        [
            p[2],
            sample_point,
            Point(sample_point[1], p[2][2])
        ],
        :fill,
        close=true
    )

    # Compute direct light:
    light_ray_height = h0 + height
    d_light_ray_height = d_height + d_h0

    if light_from_sky
        light_ray_height *= rescale(t.height, sample_point[2], inner_box[4][2], 0, 1)
        d_light_ray_height = t.height - sample_point[2]
    end

    # Get the value of θ1 and θ2, the angles relative to the vertical plane on the sample_point
    # that give the view angle of the direct light comming from the sky:
    kgdirect, θ1, θ2 = kdir(latitude_r, j, width, point_pos_m, interrow, shape, h0, alpha, height)

    # Compute P1 and P2, the two points on the sky that provide the direct light view angle:
	P1 = P_from_θ(θ1, light_ray_height, point_pos_m)
	P2 = P_from_θ(θ2, light_ray_height, point_pos_m)
    P1, P2 = P_drawing.([P1, P2], interrow, sample_point[2] + d_light_ray_height, inner_box[2][1], inner_box[4][1])

    sun_pos = [P1, P2]

    setopacity(0.3)
    sethue("yellow")
    poly(
        [
            sun_pos[1],
            sample_point,
            sun_pos[2]
        ],
        :fill,
        close=true
    )

    # Recompute the points P1 and P2 but at the inner
	P1 = P_from_θ(θ1, h0 + height, point_pos_m)
	P2 = P_from_θ(θ2, h0 + height, point_pos_m)

	P1, P2 = P_drawing.([P1, P2], interrow, sample_point[2] + d_h0 + d_height, inner_box[2][1], inner_box[4][1])

    text_point = midpoint(P1, P2)
    if display_text
        @layer begin
            sethue("black")
            setopacity(1)
            scale(1, -1) # to set the y axis up
            label(
                string("kdir: ", round(kgdirect, digits=2)), :N, Point(text_point[1], -text_point[2]),
                offset=10, leader=false, leaderoffsets=[0.4, 0.9]
            )
        end
    end

    # Compute transmitted light:
    # Transmitted light: Drawing the left triangle between θ1 and the horizontal
    sethue("goldenrod")

	(p_trans_left, p_trans_right) = draw_transmitted_light(sample_point,p,inner_box,d_width,d_h0)

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
	
    text_point = midpoint(p[1], Point(inner_box[4][1] - d_width / 2, inner_box[4][2]))
	kgdiffus,H1,H2 = kdif(point_pos_m, h0, width, interrow, height, shape)
	
	if diffuse_angles
		setline(8)
		setdash("dot")
		setopacity(0.8)
		sethue("red")
		setline(1)
		# RHS: 
		draw_diffuse_angles(sample_point, H1, light_ray_height, d_light_ray_height, point_pos_m, interrow, inner_box)
		# LHS:
		sethue("green")
		draw_diffuse_angles(sample_point, H2, light_ray_height, d_light_ray_height, point_pos_m, interrow, inner_box)
	end

	if display_text
        @layer begin
            sethue("black")
            setopacity(1)
            scale(1, -1) # to set the y axis up
            label(
                string("kdif: ", round(kgdiffus, digits=2)), :N, Point(text_point[1], -text_point[2]),
                offset=10, leader=false, leaderoffsets=[0.4, 0.9]
            )
        end
    end

    @layer begin
        sethue("grey")
        setopacity(0.5)
        newpath()
        carc(sample_point, 20, π, 0, :fill)
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
                    setopacity(1)
                    fontsize(5)
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
            sethue("black")
            setopacity(1)
            scale(1, -1) # to set the y axis up
            setdash("solid")
            label(
                "Sample point", :S, Point(sample_point[1], -sample_point[2]),
                offset=25, leader=true, leaderoffsets=[0.5, 0.9]
            )
        end
    end

    finish()
    preview()
end

# ╔═╡ 6d52ea68-1c71-4cc4-970b-8c9a947fc582
let
str = ""
if params["width"]>params["interrow"] 
	str = str * "\nPlant width > interrow, will use interrow for the computation"
end

if diffuse_angles 
	str = str * """\nDiffuse angles are projected in 2D but are computed in the 3D space, so an angle that appears below the top of the plant crown in the diagram in in fact above, but either points to a direction towards of away from you."""
end

if length(str) > 0
	@warn str
end
end

# ╔═╡ Cell order:
# ╠═f5cd7710-c533-11ec-1490-a502fac92221
# ╟─6788dbbe-317e-4212-a0e8-d417a52301f6
# ╟─f92c1e63-d40f-41eb-8d58-b44b62e44ff9
# ╠═311611bd-89f9-4e34-84cb-11924e8efc2d
# ╟─4dff9014-73ff-4c32-b6ad-c936bd892588
# ╟─a24703dc-9b43-4b9c-9f2e-11b042c67af2
# ╟─e6c55f6f-a8bf-423b-b3d7-49acf1cf74d0
# ╟─6d52ea68-1c71-4cc4-970b-8c9a947fc582
# ╟─dff1401d-a2e9-45c1-9e26-a46d0fa44eff
# ╟─2030aa31-a8d6-4b44-b359-04a0eb45a748
# ╟─78c00fe4-feb0-45de-b5e1-df0fae546287
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
# ╟─4eb15ffa-5218-4d30-a9ec-4c6f6d0a4524
# ╟─b777571c-91b2-4c80-a3bb-1bc65f48fbc8
# ╟─030d4bd1-b596-4590-b1c5-d53bdc656c7f
# ╟─172d2086-efb1-4805-b75e-7801072347f4
# ╟─d07ff0dc-40d5-4e03-84d3-115a891d4530
# ╟─d02a0cb0-7e61-4d6b-a2b8-ace9ef94e4fc
