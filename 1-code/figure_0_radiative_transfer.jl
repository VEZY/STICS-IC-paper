using Luxor

includet("functions.jl")

image_dim = (800, 400)
interrow = 1.00
h0 = 0.5
width = 0.2
height = 1.0
shape = :dtriangle # Possible values: :dtriangle, :utriangle, :rectangle
npoints = 200
latitude = 43.61
j = 170
alpha = 0.0 # Crop row direction relative to north
rel_sun_pos = 0.5
point_pos_m = 0.4 # position of the point to simulate light interception, in meter
light_from_sky = true # if false the light stops at the inner box, else at the sky
display_text = true # display names and values?

begin
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

    point_pos = rescale(point_pos_m / interrow, 0, interrow, x0, inner_box[4][1]) # Point position on the plane coords
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
    light_ray_height = h0 + height
    d_light_ray_height = d_height + d_h0

    if light_from_sky
        light_ray_height *= rescale(t.height, sample_point[2], inner_box[4][2], 0, 1)
        d_light_ray_height = t.height - sample_point[2]
    end

    # Get the value of θ1 and θ2, the angles relative to the vertical plane on the sample_point
    # that give the view angle of the direct light comming from the sky:
    kgdirect, θ1, θ2 = kdir(latitude, j, width, point_pos_m, interrow, shape, h0, alpha, height)

    # Compute P1 and P2, the two points on the sky that provide the direct light view angle:
    P1, P2 = P_from_θ.([θ1, θ2], light_ray_height, point_pos_m)
    P1, P2 = P_drawing.([P1, P2], interrow, sample_point[2] + d_light_ray_height, inner_box[2][1], inner_box[4][1])

    sun_pos = [P1, P2]

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
            text_pos(string("kdir: ", round(kgdirect, digits=2)), text_point[1], text_point[2] + 10)
        end
    end

    # Compute transmitted light:
    # Transmitted light: Drawing the left triangle between θ1 and the horizontal
    sethue("goldenrod")
    poly(
        [
            p[2],
            sample_point,
            inner_box[2],
            p[3],
        ],
        :fill,
        close=true
    )

    # Transmitted light: Drawing the right triangle between θ2 and the horizontal
    poly(
        [
            Point(inner_box[4][1] - d_width / 2, inner_box[4][2]),
            sample_point,
            inner_box[3],
            Point(inner_box[3][1], inner_box[3][2] + d_h0),
        ],
        :fill,
        close=true
    )

    text_point = midpoint(p[2], Point(inner_box[4][1] - d_width / 2, inner_box[4][2]))
    if display_text
        @layer begin
            kdifuse = kdif(point_pos_m, h0, width, interrow, height)
            sethue("black")
            setopacity(1)
            text_pos(string("kdif: ", round(kdifuse, digits=2)), text_point[1], text_point[2] + 10)
        end
    end

    sethue("grey")
    circle(sample_point, 10, :fill)
    if display_text
        @layer begin
            sethue("black")
            setopacity(1)
            text_pos("Sample point", sample_point[1] - 30, sample_point[2] - 20)
        end
    end

    finish()
    preview()
end


function timestem()

end
