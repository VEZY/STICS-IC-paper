using Luxor

includet("functions.jl")

image_dim = (800, 400)
interrow = 1.00
h0 = 0.0
width = 0.2
height = 1.0
shape = :dtriangle # Possible values: :dtriangle, :utriangle, :rectangle
npoints = 200
latitude = 43.61
j = 1
alpha = 0.0 # Crop row direction relative to north
rel_sun_pos = 0.5
point_pos_m = 0.4 # position of the point to simulate light interception for


begin
    Drawing(image_dim[1], image_dim[2], :png)

    t = currentdrawing()
    background("white")
    sethue("black")
    scale(1, -1) # to set the y axis up
    translate(0, -t.height)

    # sethue("red")
    # circle(Point(0, 0), 10, :fill)
    # sethue("green")
    # circle(Point(0, t.height), 10, :fill)

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

    θ1, θ2 = get_θ(latitude, j, width, point_pos_m, interrow, shape, h0, alpha, height)
    rad2deg(cos(π / 2 + θ1))
    rad2deg(cos(π / 2 + θ2))
    #! Use these angles instead to compute direct light (relative to vertical I think)

    point_pos = rescale(point_pos_rel, 0, interrow, x0[1], center[1]) # Point position on the plane coords
    sample_point = Point(point_pos, y0) # Point coordinates

    # Compute diffuse light:
    # Direct light: Drawing the right triangle between the vertical and θ1
    sethue("khaki1")
    poly(
        [
            Point(sample_point[1], p[2][2]),
            sample_point,
            Point(inner_box[4][1] - d_width / 2, inner_box[4][2])
        ],
        :fill,
        close=true
    )

    # Direct light: Drawing the left triangle between the vertical and θ2
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
    rel_sun_pos_min = max(rel_sun_pos - 0.1, 0.0)
    rel_sun_pos_max = min(rel_sun_pos + 0.1, 1.0)
    sun_pos = rescale.([rel_sun_pos_min, rel_sun_pos_max], 0, 1, p[2], Point(inner_box[4][1] - d_width / 2, inner_box[4][2]))

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

    # Compute transmitted light:
    # Transmitted light: Drawing the left triangle between θ1 and the horizontal
    sethue("goldenrod")
    poly(
        [
            p[2],
            sample_point,
            inner_box[2]
        ],
        :fill,
        close=true
    )

    # Transmitted light: Drawing the right triangle between θ2 and the horizontal
    poly(
        [
            Point(inner_box[4][1] - d_width / 2, inner_box[4][2]),
            sample_point,
            inner_box[3]
        ],
        :fill,
        close=true
    )



    sethue("grey")
    circle(sample_point, 10, :fill)

    finish()
    preview()
end


function timestem()

end
