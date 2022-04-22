"""
    half_canopy_left(width, height, h0)

Draw the half crop canopy for the plant on the left-hand side. We draw only the half canopy
because we only use this one for computations.

# Arguments

- `shape`: the plant canopy shape: `:rectangle`, `:utriangle`, `:dtriangle`.
- `width`: the plant canopy widht (total)
- `height`: the plant canopy height (total)
- `h0=0.0`: the base canopy height (default to 0 for the ground for annual crops)

# Details

`:utriangle` is a triangle pointing up towards the sky, and `:dtriangle` pointing down
towards the ground.
"""
function half_canopy_left(shape, width, height, h0=0.0, x0=0.0)
    if !in(shape, [:dtriangle, :utriangle, :rectangle])
        error("shape should be one of `:dtriangle`, `:utriangle` or `:rectangle`")
    end

    @layer begin
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
    end
    return p
end
