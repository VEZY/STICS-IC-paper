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
	alpha = deg2rad(0) # Crop row direction relative to north
	light_from_sky = true # if false the light stops at the inner box, else at the sky
	display_text = true # display names and values?
	n_sample_points = 200
end

# ╔═╡ 4dff9014-73ff-4c32-b6ad-c936bd892588
md"""
### Dynamic
"""

# ╔═╡ d02a0cb0-7e61-4d6b-a2b8-ace9ef94e4fc
function DataFrameInput(data_frame_input, combine_funct; title="")
	table_header = []
	table_body = []
	col_names = [@htl("<th>$(col_name)</th>") for col_name in names(data_frame_input)]

	function cell_element(row, cell)
		if isa(cell, Slider) | isa(cell, Scrubbable) | isa(cell, TextField) | isa(cell, RangeSlider) | isa(cell, Radio) | isa(cell, Select)
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
params_ = 
	params_ = let 
		params_ = Any[
			Slider(param.second[1];default = param.second[2],show_value=true) for param in [
				"latitude" => (-90:1:90, 44),
				"day" => (1:365, 1),
				"width" => (0.05:0.05:1.0,0.2),
				"interrow" => (0.05:0.05:1.0,1),
				"height" => (0.05:0.05:1.0,0.8),
				"sample_point" => (1:199.0,100),
			]
		]

		push!(
			params_,
			Select(["dtriangle" => "🔻","utriangle" => "🔺", "rectangle" => "🟥"];default = "dtriangle")
		)
	end
	
	params_df = DataFrame(
			:Parameter => ["latitude", "day", "width", "interrow", "height", "sample_point","shape"],
			:Units => ["degree", "julian day", "m", "m", "m", "index","-"],
			 Symbol("Value") => params_	
	)

		
	@bind df_values PlutoUI.combine() do Child
		DataFrameInput(params_df, Child; title="Input Dataframe")
	end
end

# ╔═╡ a24703dc-9b43-4b9c-9f2e-11b042c67af2
params = Dict(zip(params_df.Parameter, [df_values[i] for i in 1:length(df_values)]));

# ╔═╡ 6d52ea68-1c71-4cc4-970b-8c9a947fc582
if params["width"]>params["interrow"] 
	@warn "Plant width > interrow, will use interrow for the computation"
end

# ╔═╡ dff1401d-a2e9-45c1-9e26-a46d0fa44eff
md"""
## Diagram
"""

# ╔═╡ e261142a-c411-40a3-85e4-ae979a4d9506
md"""
## References

These are the functions used in this notebook. Some are related to the drawing of the diagram, others are functions from STICS.

### STICS functions
"""

# ╔═╡ 6d701d6c-daa5-4bf0-9ee2-cb76dfecf510
"""
    kdif(x, h0, width, ir, e)

Fraction of diffuse radiation received by a point.

##### Arguments

- `x`: the point x coordinates
- `h0`: the canopy base height
- `width`: the plant canopy widht
- `ir`: the interrow distance, *i.e.* the distance between two plants
- `e`: the effective canopy thickness
"""
function kdif(x, h0, width, ir, e)

    # Values given by Hervé Sinoquet, gives height, azimuth and fraction of diffuse light according
    # to the SOC standard for 23 directions
    htab = (repeat([9.23], 5)..., 10.81, 10.81, 26.57, 26.57, 26.57, repeat([31.08], 5)..., 47.41, 47.41, 47.41, 52.62, 52.62, 69.16, 69.16, 69.16)
    aztab = (12.23, 59.77, 84.23, 131.77, 156.23, 36, 108, 0, 72, 144, 23.27, 48.73, 95.27, 120.73, 167.27, 0, 72, 144, 36, 108, 0, 72, 144)
    SOCtab = (repeat([0.0043], 5)..., 0.0055, 0.0055, 0.0140, 0.0140, 0.0140, repeat([0.0197], 5)..., 0.0336, 0.0336, 0.0336, 0.0399, 0.0399, 0.0495, 0.0495, 0.0495)

    x = min(x, ir / 2)
    limite = width / 2.0
    kgdiffus = 0.0

    # For the right-hand side:
    G = (h0 + e) / (ir - x - limite)
    for i in 1:23
        hcrit = atan(G * sin(aztab[i] / 180 * π)) / π * 180
        if hcrit < htab[i]
            kgdiffus = kgdiffus + SOCtab[i]
        end
    end

    # For the left-hand side:
    # If the point is not under the plant canopy (else it is only transmitted light):
    if x > limite
        G = (h0 + e) / (x - limite)
        for i in 1:23
            hcrit = atan(G * sin(aztab[i] / 180 * π)) / π * 180
            if (hcrit < htab[i])
                kgdiffus = kgdiffus + SOCtab[i]
            end
        end
    end

    return kgdiffus
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


# ╔═╡ 4ab56f65-3314-4dc4-9eb1-59f68058e435
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
        θ1 = θcrit(lat, j, tgh, alpha)
        if x > limite
            tgh = (h0 + e) / (x - limite)
            θ2 = θcrit(lat, j, tgh, alpha)
        elseif x < limite
            tgh = h0 / (-x + limite)
            θ2 = -θcrit(lat, j, tgh, alpha)
        elseif x == limite
            θ2 = 0
        end
    elseif shape == :dtriangle
        tgh = (h0 + e) / (ir - x - limite)
        θ1 = θcrit(lat, j, tgh, alpha)
        if x > limite
            tgh = (h0 + e) / (x - limite)
            θ2 = θcrit(lat, j, tgh, alpha)
        elseif x < limite
            tgh = (h0 + e) / (x - limite)
            θ2 = -θcrit(lat, j, tgh, alpha)
        elseif x == limite
            θ2 = 0
        end
    elseif shape == :utriangle
        tgh = (h0 + e) / (ir - x - limite)
        θ1 = θcrit(lat, j, tgh, alpha)
        if x < limite2
            if (x > limite)
                tgh = h0 / (x - limite)
                θ2 = θcrit(lat, j, tgh, alpha)
            elseif x < limite
                tgh = h0 / (limite - x)
                θ2 = -θcrit(lat, j, tgh, alpha)
            elseif x == limite
                θ2 = 0.0
            end
		else
			tgh = (h0 + e) / x
			θ2 = θcrit(lat, j, tgh, alpha)
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

    kg = 0.5 * (cos(π / 2 + θ1) + cos(π / 2 + θ2))

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
        kgdiffus = kdif(x, h0, width, ir, height)

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
	P_from_θ(θ, tot_height, point_pos_m)

Get the point X position (in m) of a ray in the sky giving the angle `θ` (angle to the vertical), the sky heigth (`sky_height`) and the point position on the X axis.
"""
function P_from_θ(θ, sky_height, x)
    # This is the X position of P in m relative to the sample point X
    P = sin(θ) * sky_height / cos(θ)

    # x - P1 to get the true position in m from the relative position:
    return Point(x - P, 0) #! Check if it is - or + P
end

# ╔═╡ 2030aa31-a8d6-4b44-b359-04a0eb45a748
begin
	j = params["day"]
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
    P1, P2 = P_from_θ.([θ1, θ2], light_ray_height, point_pos_m)
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
    P1, P2 = P_from_θ.([θ1, θ2], h0 + height, point_pos_m)
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

	draw_transmitted_light(sample_point,p,inner_box,d_width,d_h0)
	
    text_point = midpoint(p[2], Point(inner_box[4][1] - d_width / 2, inner_box[4][2]))
    if display_text
        @layer begin
            kdifuse = kdif(point_pos_m, h0, width, interrow, height)
            sethue("black")
            setopacity(1)
            scale(1, -1) # to set the y axis up
            label(
                string("kdif: ", round(kdifuse, digits=2)), :N, Point(text_point[1], -text_point[2]),
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

# ╔═╡ 00000000-0000-0000-0000-000000000001
PLUTO_PROJECT_TOML_CONTENTS = """
[deps]
DataFrames = "a93c6f00-e57d-5684-b7b6-d8193f3e46c0"
HypertextLiteral = "ac1192a8-f4b3-4bfe-ba22-af5b92cd3ab2"
Luxor = "ae8d54c2-7ccd-5906-9d76-62fc9837b5bc"
PlutoUI = "7f904dfe-b85e-4ff6-b463-dae2292396a8"
UUIDs = "cf7118a7-6976-5b1a-9a39-7adc72f591a4"

[compat]
DataFrames = "~1.3.3"
HypertextLiteral = "~0.9.3"
Luxor = "~3.2.0"
PlutoUI = "~0.7.38"
"""

# ╔═╡ 00000000-0000-0000-0000-000000000002
PLUTO_MANIFEST_TOML_CONTENTS = """
# This file is machine-generated - editing it directly is not advised

julia_version = "1.7.2"
manifest_format = "2.0"

[[deps.AbstractPlutoDingetjes]]
deps = ["Pkg"]
git-tree-sha1 = "8eaf9f1b4921132a4cff3f36a1d9ba923b14a481"
uuid = "6e696c72-6542-2067-7265-42206c756150"
version = "1.1.4"

[[deps.ArgTools]]
uuid = "0dad84c5-d112-42e6-8d28-ef12dabb789f"

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
deps = ["Artifacts", "Bzip2_jll", "Fontconfig_jll", "FreeType2_jll", "Glib_jll", "JLLWrappers", "LZO_jll", "Libdl", "Pixman_jll", "Pkg", "Xorg_libXext_jll", "Xorg_libXrender_jll", "Zlib_jll", "libpng_jll"]
git-tree-sha1 = "4b859a208b2397a7a623a03449e4636bdb17bcf2"
uuid = "83423d85-b0ee-5818-9007-b63ccbeb887a"
version = "1.16.1+1"

[[deps.ColorTypes]]
deps = ["FixedPointNumbers", "Random"]
git-tree-sha1 = "024fe24d83e4a5bf5fc80501a314ce0d1aa35597"
uuid = "3da002f7-5984-5a60-b8a6-cbb66c0b333f"
version = "0.11.0"

[[deps.Colors]]
deps = ["ColorTypes", "FixedPointNumbers", "Reexport"]
git-tree-sha1 = "417b0ed7b8b838aa6ca0a87aadf1bb9eb111ce40"
uuid = "5ae59095-9a9b-59fe-a467-6f913c188581"
version = "0.12.8"

[[deps.Compat]]
deps = ["Base64", "Dates", "DelimitedFiles", "Distributed", "InteractiveUtils", "LibGit2", "Libdl", "LinearAlgebra", "Markdown", "Mmap", "Pkg", "Printf", "REPL", "Random", "SHA", "Serialization", "SharedArrays", "Sockets", "SparseArrays", "Statistics", "Test", "UUIDs", "Unicode"]
git-tree-sha1 = "b153278a25dd42c65abbf4e62344f9d22e59191b"
uuid = "34da2185-b29b-5c13-b0c7-acf172513d20"
version = "3.43.0"

[[deps.CompilerSupportLibraries_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "e66e0078-7015-5450-92f7-15fbd957f2ae"

[[deps.Crayons]]
git-tree-sha1 = "249fe38abf76d48563e2f4556bebd215aa317e15"
uuid = "a8cc5b0e-0ffa-5ad4-8c14-923d3ee1735f"
version = "4.1.1"

[[deps.DataAPI]]
git-tree-sha1 = "fb5f5316dd3fd4c5e7c30a24d50643b73e37cd40"
uuid = "9a962f9c-6df0-11e9-0e5d-c546b8b5ee8a"
version = "1.10.0"

[[deps.DataFrames]]
deps = ["Compat", "DataAPI", "Future", "InvertedIndices", "IteratorInterfaceExtensions", "LinearAlgebra", "Markdown", "Missings", "PooledArrays", "PrettyTables", "Printf", "REPL", "Reexport", "SortingAlgorithms", "Statistics", "TableTraits", "Tables", "Unicode"]
git-tree-sha1 = "6c19003824cbebd804a51211fd3bbd81bf1ecad5"
uuid = "a93c6f00-e57d-5684-b7b6-d8193f3e46c0"
version = "1.3.3"

[[deps.DataStructures]]
deps = ["Compat", "InteractiveUtils", "OrderedCollections"]
git-tree-sha1 = "3daef5523dd2e769dad2365274f760ff5f282c7d"
uuid = "864edb3b-99cc-5e75-8d2d-829cb0a9cfe8"
version = "0.18.11"

[[deps.DataValueInterfaces]]
git-tree-sha1 = "bfc1187b79289637fa0ef6d4436ebdfe6905cbd6"
uuid = "e2d170a0-9d28-54be-80f0-106bbe20a464"
version = "1.0.0"

[[deps.Dates]]
deps = ["Printf"]
uuid = "ade2ca70-3891-5945-98fb-dc099432e06a"

[[deps.DelimitedFiles]]
deps = ["Mmap"]
uuid = "8bb1440f-4735-579b-a4ab-409b98df4dab"

[[deps.Distributed]]
deps = ["Random", "Serialization", "Sockets"]
uuid = "8ba89e20-285c-5b6f-9357-94700520ee1b"

[[deps.Downloads]]
deps = ["ArgTools", "LibCURL", "NetworkOptions"]
uuid = "f43a241f-c20a-4ad4-852c-f6b1247861c6"

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
deps = ["Artifacts", "Bzip2_jll", "FreeType2_jll", "FriBidi_jll", "JLLWrappers", "LAME_jll", "Libdl", "Ogg_jll", "OpenSSL_jll", "Opus_jll", "Pkg", "Zlib_jll", "libass_jll", "libfdk_aac_jll", "libvorbis_jll", "x264_jll", "x265_jll"]
git-tree-sha1 = "d8a578692e3077ac998b50c0217dfd67f21d1e5f"
uuid = "b22a6f82-2f65-5046-a5b2-351ab43fb4e5"
version = "4.4.0+0"

[[deps.FileIO]]
deps = ["Pkg", "Requires", "UUIDs"]
git-tree-sha1 = "80ced645013a5dbdc52cf70329399c35ce007fae"
uuid = "5789e2e9-d7fb-5bc7-8068-2c6fae9b9549"
version = "1.13.0"

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
deps = ["Artifacts", "Gettext_jll", "JLLWrappers", "Libdl", "Libffi_jll", "Libiconv_jll", "Libmount_jll", "PCRE_jll", "Pkg", "Zlib_jll"]
git-tree-sha1 = "a32d672ac2c967f3deb8a81d828afc739c838a06"
uuid = "7746bdde-850d-59dc-9ae8-88ece973131d"
version = "2.68.3+2"

[[deps.Graphics]]
deps = ["Colors", "LinearAlgebra", "NaNMath"]
git-tree-sha1 = "1c5a84319923bea76fa145d49e93aa4394c73fc2"
uuid = "a2bd30eb-e257-5431-a919-1863eab51364"
version = "1.1.1"

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
git-tree-sha1 = "2b078b5a615c6c0396c77810d92ee8c6f470d238"
uuid = "ac1192a8-f4b3-4bfe-ba22-af5b92cd3ab2"
version = "0.9.3"

[[deps.IOCapture]]
deps = ["Logging", "Random"]
git-tree-sha1 = "f7be53659ab06ddc986428d3a9dcc95f6fa6705a"
uuid = "b5f81e59-6552-4d32-b1f0-c071b021bf89"
version = "0.2.2"

[[deps.InteractiveUtils]]
deps = ["Markdown"]
uuid = "b77e0a4c-d291-57a0-90e8-8db25a27a240"

[[deps.InvertedIndices]]
git-tree-sha1 = "bee5f1ef5bf65df56bdd2e40447590b272a5471f"
uuid = "41ab1584-1d38-5bbf-9106-f11c6c58b48f"
version = "1.1.0"

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
git-tree-sha1 = "3c837543ddb02250ef42f4738347454f95079d4e"
uuid = "682c06a0-de6a-54ab-a142-c8b1cf79cde6"
version = "0.21.3"

[[deps.JpegTurbo_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "b53380851c6e6664204efb2e62cd24fa5c47e4ba"
uuid = "aacddb02-875f-59d6-b918-886e6ef4fbf8"
version = "2.1.2+0"

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

[[deps.LibCURL_jll]]
deps = ["Artifacts", "LibSSH2_jll", "Libdl", "MbedTLS_jll", "Zlib_jll", "nghttp2_jll"]
uuid = "deac9b47-8bc7-5906-a0fe-35ac56dc84c0"

[[deps.LibGit2]]
deps = ["Base64", "NetworkOptions", "Printf", "SHA"]
uuid = "76f85450-5226-5b5a-8eaa-529ad045b433"

[[deps.LibSSH2_jll]]
deps = ["Artifacts", "Libdl", "MbedTLS_jll"]
uuid = "29816b5a-b9ab-546f-933c-edad1886dfa8"

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
git-tree-sha1 = "42b62845d70a619f063a7da093d995ec8e15e778"
uuid = "94ce4f54-9a6c-5748-9c1c-f9c7231a4531"
version = "1.16.1+1"

[[deps.Libmount_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "9c30530bf0effd46e15e0fdcf2b8636e78cbbd73"
uuid = "4b2f31a3-9ecc-558c-b454-b3730dcb73e9"
version = "2.35.0+0"

[[deps.Librsvg_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pango_jll", "Pkg", "gdk_pixbuf_jll"]
git-tree-sha1 = "25d5e6b4eb3558613ace1c67d6a871420bfca527"
uuid = "925c91fb-5dd6-59dd-8e8c-345e74382d89"
version = "2.52.4+0"

[[deps.Libtiff_jll]]
deps = ["Artifacts", "JLLWrappers", "JpegTurbo_jll", "LERC_jll", "Libdl", "Pkg", "Zlib_jll", "Zstd_jll"]
git-tree-sha1 = "c9551dd26e31ab17b86cbd00c2ede019c08758eb"
uuid = "89763e89-9b03-5906-acba-b20f662cd828"
version = "4.3.0+1"

[[deps.Libuuid_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "7f3efec06033682db852f8b3bc3c1d2b0a0ab066"
uuid = "38a345b3-de98-5d2b-a5d3-14cd9215e700"
version = "2.36.0+0"

[[deps.LinearAlgebra]]
deps = ["Libdl", "libblastrampoline_jll"]
uuid = "37e2e46d-f89d-539d-b4ee-838fcccc9c8e"

[[deps.Logging]]
uuid = "56ddb016-857b-54e1-b83d-db4d58db5568"

[[deps.Luxor]]
deps = ["Base64", "Cairo", "Colors", "Dates", "FFMPEG", "FileIO", "Juno", "LaTeXStrings", "Random", "Requires", "Rsvg"]
git-tree-sha1 = "156958d51d9f758dc5a00dcc6da4f61cacf579ed"
uuid = "ae8d54c2-7ccd-5906-9d76-62fc9837b5bc"
version = "3.2.0"

[[deps.MacroTools]]
deps = ["Markdown", "Random"]
git-tree-sha1 = "3d3e902b31198a27340d0bf00d6ac452866021cf"
uuid = "1914dd2f-81c6-5fcd-8719-6d5c9610ff09"
version = "0.5.9"

[[deps.Markdown]]
deps = ["Base64"]
uuid = "d6f4376e-aef5-505a-96c1-9c027394607a"

[[deps.MbedTLS_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "c8ffd9c3-330d-5841-b78e-0817d7145fa1"

[[deps.Media]]
deps = ["MacroTools", "Test"]
git-tree-sha1 = "75a54abd10709c01f1b86b84ec225d26e840ed58"
uuid = "e89f7d12-3494-54d1-8411-f7d8b9ae1f27"
version = "0.5.0"

[[deps.Missings]]
deps = ["DataAPI"]
git-tree-sha1 = "bf210ce90b6c9eed32d25dbcae1ebc565df2687f"
uuid = "e1d29d7a-bbdc-5cf2-9ac0-f12de2c33e28"
version = "1.0.2"

[[deps.Mmap]]
uuid = "a63ad114-7e13-5084-954f-fe012c677804"

[[deps.MozillaCACerts_jll]]
uuid = "14a3606d-f60d-562e-9121-12d972cd8159"

[[deps.NaNMath]]
git-tree-sha1 = "b086b7ea07f8e38cf122f5016af580881ac914fe"
uuid = "77ba4419-2d1f-58cd-9bb1-8ffee604a2e3"
version = "0.3.7"

[[deps.NetworkOptions]]
uuid = "ca575930-c2e3-43a9-ace4-1e988b2c1908"

[[deps.Ogg_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "887579a3eb005446d514ab7aeac5d1d027658b8f"
uuid = "e7412a2a-1a6e-54c0-be00-318e2571c051"
version = "1.3.5+1"

[[deps.OpenBLAS_jll]]
deps = ["Artifacts", "CompilerSupportLibraries_jll", "Libdl"]
uuid = "4536629a-c528-5b80-bd46-f80d51c5b363"

[[deps.OpenSSL_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "ab05aa4cc89736e95915b01e7279e61b1bfe33b8"
uuid = "458c3c95-2e84-50aa-8efc-19380b2a3a95"
version = "1.1.14+0"

[[deps.Opus_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "51a08fb14ec28da2ec7a927c4337e4332c2a4720"
uuid = "91d4177d-7536-5919-b921-800302f37372"
version = "1.3.2+0"

[[deps.OrderedCollections]]
git-tree-sha1 = "85f8e6578bf1f9ee0d11e7bb1b1456435479d47c"
uuid = "bac558e1-5e72-5ebc-8fee-abe8a469f55d"
version = "1.4.1"

[[deps.PCRE_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "b2a7af664e098055a7529ad1a900ded962bca488"
uuid = "2f80f16e-611a-54ab-bc61-aa92de5b98fc"
version = "8.44.0+0"

[[deps.Pango_jll]]
deps = ["Artifacts", "Cairo_jll", "Fontconfig_jll", "FreeType2_jll", "FriBidi_jll", "Glib_jll", "HarfBuzz_jll", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "3a121dfbba67c94a5bec9dde613c3d0cbcf3a12b"
uuid = "36c8627f-9965-5494-a995-c6b170f724f3"
version = "1.50.3+0"

[[deps.Parsers]]
deps = ["Dates"]
git-tree-sha1 = "3b429f37de37f1fc603cc1de4a799dc7fbe4c0b6"
uuid = "69de0a69-1ddd-5017-9359-2bf0b02dc9f0"
version = "2.3.0"

[[deps.Pixman_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "b4f5d02549a10e20780a24fce72bea96b6329e29"
uuid = "30392449-352a-5448-841d-b1acce4e97dc"
version = "0.40.1+0"

[[deps.Pkg]]
deps = ["Artifacts", "Dates", "Downloads", "LibGit2", "Libdl", "Logging", "Markdown", "Printf", "REPL", "Random", "SHA", "Serialization", "TOML", "Tar", "UUIDs", "p7zip_jll"]
uuid = "44cfe95a-1eb2-52ea-b672-e2afdf69b78f"

[[deps.PlutoUI]]
deps = ["AbstractPlutoDingetjes", "Base64", "ColorTypes", "Dates", "Hyperscript", "HypertextLiteral", "IOCapture", "InteractiveUtils", "JSON", "Logging", "Markdown", "Random", "Reexport", "UUIDs"]
git-tree-sha1 = "670e559e5c8e191ded66fa9ea89c97f10376bb4c"
uuid = "7f904dfe-b85e-4ff6-b463-dae2292396a8"
version = "0.7.38"

[[deps.PooledArrays]]
deps = ["DataAPI", "Future"]
git-tree-sha1 = "28ef6c7ce353f0b35d0df0d5930e0d072c1f5b9b"
uuid = "2dfb63ee-cc39-5dd5-95bd-886bf059d720"
version = "1.4.1"

[[deps.Preferences]]
deps = ["TOML"]
git-tree-sha1 = "47e5f437cc0e7ef2ce8406ce1e7e24d44915f88d"
uuid = "21216c6a-2e73-6563-6e65-726566657250"
version = "1.3.0"

[[deps.PrettyTables]]
deps = ["Crayons", "Formatting", "Markdown", "Reexport", "Tables"]
git-tree-sha1 = "dfb54c4e414caa595a1f2ed759b160f5a3ddcba5"
uuid = "08abe8d2-0d0c-5749-adfa-8a2ac140af0d"
version = "1.3.1"

[[deps.Printf]]
deps = ["Unicode"]
uuid = "de0858da-6303-5e67-8744-51eddeeeb8d7"

[[deps.Profile]]
deps = ["Printf"]
uuid = "9abbd945-dff8-562f-b5e8-e1ebf5ef1b79"

[[deps.REPL]]
deps = ["InteractiveUtils", "Markdown", "Sockets", "Unicode"]
uuid = "3fa0cd96-eef1-5676-8a61-b3b8758bbffb"

[[deps.Random]]
deps = ["SHA", "Serialization"]
uuid = "9a3f8284-a2c9-5f02-9a11-845980a1fd5c"

[[deps.Reexport]]
git-tree-sha1 = "45e428421666073eab6f2da5c9d310d99bb12f9b"
uuid = "189a3867-3050-52da-a836-e630ba90ab69"
version = "1.2.2"

[[deps.Requires]]
deps = ["UUIDs"]
git-tree-sha1 = "838a3a4188e2ded87a4f9f184b4b0d78a1e91cb7"
uuid = "ae029012-a4dd-5104-9daa-d747884805df"
version = "1.3.0"

[[deps.Rsvg]]
deps = ["Cairo", "Glib_jll", "Librsvg_jll"]
git-tree-sha1 = "3d3dc66eb46568fb3a5259034bfc752a0eb0c686"
uuid = "c4c386cf-5103-5370-be45-f3a111cca3b8"
version = "1.0.0"

[[deps.SHA]]
uuid = "ea8e919c-243c-51af-8825-aaa63cd721ce"

[[deps.Serialization]]
uuid = "9e88b42a-f829-5b0c-bbe9-9e923198166b"

[[deps.SharedArrays]]
deps = ["Distributed", "Mmap", "Random", "Serialization"]
uuid = "1a1011a3-84de-559e-8e89-a11a2f7dc383"

[[deps.Sockets]]
uuid = "6462fe0b-24de-5631-8697-dd941f90decc"

[[deps.SortingAlgorithms]]
deps = ["DataStructures"]
git-tree-sha1 = "b3363d7460f7d098ca0912c69b082f75625d7508"
uuid = "a2af1166-a08f-5f64-846c-94a0d3cef48c"
version = "1.0.1"

[[deps.SparseArrays]]
deps = ["LinearAlgebra", "Random"]
uuid = "2f01184e-e22b-5df5-ae63-d93ebab69eaf"

[[deps.Statistics]]
deps = ["LinearAlgebra", "SparseArrays"]
uuid = "10745b16-79ce-11e8-11f9-7d13ad32a3b2"

[[deps.TOML]]
deps = ["Dates"]
uuid = "fa267f1f-6049-4f14-aa54-33bafae1ed76"

[[deps.TableTraits]]
deps = ["IteratorInterfaceExtensions"]
git-tree-sha1 = "c06b2f539df1c6efa794486abfb6ed2022561a39"
uuid = "3783bdb8-4a98-5b6b-af9a-565f29a5fe9c"
version = "1.0.1"

[[deps.Tables]]
deps = ["DataAPI", "DataValueInterfaces", "IteratorInterfaceExtensions", "LinearAlgebra", "OrderedCollections", "TableTraits", "Test"]
git-tree-sha1 = "5ce79ce186cc678bbb5c5681ca3379d1ddae11a1"
uuid = "bd369af6-aec1-5ad0-b16a-f7cc5008161c"
version = "1.7.0"

[[deps.Tar]]
deps = ["ArgTools", "SHA"]
uuid = "a4e569a6-e804-4fa4-b0f3-eef7a1d5b13e"

[[deps.Test]]
deps = ["InteractiveUtils", "Logging", "Random", "Serialization"]
uuid = "8dfed614-e22c-5e08-85e1-65c5234f0b40"

[[deps.UUIDs]]
deps = ["Random", "SHA"]
uuid = "cf7118a7-6976-5b1a-9a39-7adc72f591a4"

[[deps.Unicode]]
uuid = "4ec0a83e-493e-50e2-b9ac-8f72acf5a8f5"

[[deps.XML2_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Libiconv_jll", "Pkg", "Zlib_jll"]
git-tree-sha1 = "1acf5bdf07aa0907e0a37d3718bb88d4b687b74a"
uuid = "02c8fc9c-b97f-50b9-bbe4-9be30ff0a78a"
version = "2.9.12+0"

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

[[deps.Zstd_jll]]
deps = ["Artifacts", "JLLWrappers", "Libdl", "Pkg"]
git-tree-sha1 = "e45044cd873ded54b6a5bac0eb5c971392cf1927"
uuid = "3161d3a3-bdf6-5164-811a-617609db77b4"
version = "1.5.2+0"

[[deps.gdk_pixbuf_jll]]
deps = ["Artifacts", "Glib_jll", "JLLWrappers", "JpegTurbo_jll", "Libdl", "Libtiff_jll", "Pkg", "Xorg_libX11_jll", "libpng_jll"]
git-tree-sha1 = "c23323cd30d60941f8c68419a70905d9bdd92808"
uuid = "da03df04-f53b-5353-a52f-6a8b0620ced0"
version = "2.42.6+1"

[[deps.libass_jll]]
deps = ["Artifacts", "Bzip2_jll", "FreeType2_jll", "FriBidi_jll", "HarfBuzz_jll", "JLLWrappers", "Libdl", "Pkg", "Zlib_jll"]
git-tree-sha1 = "5982a94fcba20f02f42ace44b9894ee2b140fe47"
uuid = "0ac62f75-1d6f-5e53-bd7c-93b484bb37c0"
version = "0.15.1+0"

[[deps.libblastrampoline_jll]]
deps = ["Artifacts", "Libdl", "OpenBLAS_jll"]
uuid = "8e850b90-86db-534c-a0d3-1478176c7d93"

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

[[deps.p7zip_jll]]
deps = ["Artifacts", "Libdl"]
uuid = "3f19e933-33d8-53b3-aaab-bd5110c3b7a0"

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
# ╟─d02a0cb0-7e61-4d6b-a2b8-ace9ef94e4fc
# ╟─a24703dc-9b43-4b9c-9f2e-11b042c67af2
# ╟─e6c55f6f-a8bf-423b-b3d7-49acf1cf74d0
# ╟─6d52ea68-1c71-4cc4-970b-8c9a947fc582
# ╟─dff1401d-a2e9-45c1-9e26-a46d0fa44eff
# ╠═2030aa31-a8d6-4b44-b359-04a0eb45a748
# ╟─e261142a-c411-40a3-85e4-ae979a4d9506
# ╟─ab594776-ea39-48f6-9218-78c5eed58916
# ╟─53d29bf9-dab8-4586-89d3-fcbb9d6d28bc
# ╟─58ec9faa-cbf1-4e46-b4bb-420586ac7dba
# ╟─0385990f-397e-46f8-93d7-578c8ead2be3
# ╟─6d701d6c-daa5-4bf0-9ee2-cb76dfecf510
# ╟─7f777012-1203-427f-86aa-78d502fefaab
# ╟─4ab56f65-3314-4dc4-9eb1-59f68058e435
# ╟─54cda4ec-dc89-41d4-a28d-544f556c2f34
# ╟─78cc38c7-22ab-4f24-b68f-4ba0f668d253
# ╟─3c421bef-6123-4554-b2de-b8ceabaf1b39
# ╟─4eb15ffa-5218-4d30-a9ec-4c6f6d0a4524
# ╟─b777571c-91b2-4c80-a3bb-1bc65f48fbc8
# ╟─030d4bd1-b596-4590-b1c5-d53bdc656c7f
# ╟─172d2086-efb1-4805-b75e-7801072347f4
# ╟─00000000-0000-0000-0000-000000000001
# ╟─00000000-0000-0000-0000-000000000002
