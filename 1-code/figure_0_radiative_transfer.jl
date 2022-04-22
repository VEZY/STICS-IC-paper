using Luxor

includet("functions.jl")

image_dim = (800, 400)
interrow = 0.40
h0 = 0.1
width = 0.2
height = 0.4
shape = :dtriangle # Possible values: :dtriangle, :utriangle, :rectangle

begin
    Drawing(image_dim[1], image_dim[2], :png)
    # origin()

    t = currentdrawing()
    background("white")
    sethue("black")
    scale(1, -1) # to set the y axis up
    translate(0, -t.height)
    # t.height
    # t.width
    sethue("red")
    circle(Point(0, 0), 10, :fill)

    sethue("green")
    circle(Point(0, t.height), 10, :fill)

    center = Point(t.width * 0.5, t.height * 0.5)

    # Drawing the big box inside the plot that delimits the scene boundary:
    outer_box_rel_width = 0.8 # Width of the outter box relative to figure width
    outer_box_rel_height = 0.8 # Height of the outter box relative to figure height
    outer_box_width = outer_box_rel_width * t.width
    outer_box_height = outer_box_rel_height * t.height

    sethue("grey")
    setdash("dot")
    outer_box = box(center, outer_box_width, outer_box_height, :stroke)

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
    ) # 1 value for all corners = rectangle, several = rounded corners

    # Crop dimensions in box dimensions:
    x0 = inner_box[2][1]
    y0 = inner_box[2][2]
    inner_box_width = inner_box[3][1] - inner_box[1][1]
    # Drawing the left-hand side crop:
    setopacity(0.4)
    sethue("green")

    # Draw right sides of the plants
    p = half_canopy_left(shape, d_width, d_height, d_h0 + y0, x0)

    # Outter right side of the right-hand plant
    @layer begin
        # scale(-1, 1)
        sethue("grey")
        setdash("dot")
        translate(inner_box_width, 0)
        circle(Point(0, 0), 10, :fill)
        poly(p, :stroke, close=true)
    end

    # Draw left sides of the plants
    @layer begin
        scale(-1, 1) # mirror the scene
        translate(-(x0 * 2 + inner_box_width), 0)  # translate back
        poly(p, :fill, close=true) # Outter left side of the LHS plant

        # @layer begin
        sethue("grey")
        setdash("dot")
        translate(inner_box_width, 0)  # translate back
        poly(p, :stroke, close=true) # Outter left side of the LHS plant
    end

    # NB: h0 + box_dim[2][2] to add the box height to the crop height
    finish()
    preview()
end


# box_dim_rel =
# setdash("dot")
