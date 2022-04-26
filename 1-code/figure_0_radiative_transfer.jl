using Luxor

includet("functions.jl")

image_dim = (800, 400)
interrow = 1.00
h0 = 0.5
width_user = 0.2
height = 1.0
shape_user = :utriangle # Possible values: :dtriangle, :utriangle, :rectangle
npoints = 200
latitude = 43.61
j = 170
alpha = deg2rad(0) # Crop row direction relative to north
light_from_sky = true # if false the light stops at the inner box, else at the sky
display_text = true # display names and values?
n_sample_points = 200
i_sample_point = 150 # Point to simulate (1:n_sample_points)

begin
    shape = Symbol(shape_user)
    width = min(width_user, interrow)
    latitude_r = deg2rad(latitude)
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
    all_point_pos = rescale.(all_point_pos_m ./ interrow, 0, interrow, x0, inner_box[4][1]) # Point position on the plane coords
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

    draw_transmitted_light(sample_point, p, inner_box, d_width, d_h0)

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
