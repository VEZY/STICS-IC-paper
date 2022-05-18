using Luxor
using PlutoUI
using HypertextLiteral
using DataFrames
using UUIDs # Used for the magick of the interactive parameters DataFrame
using Thebes # Add 3D to Luxor
using Rotations # For 3D rotations
using Colors, ColorSchemes # For the color palette of the diffuse arrows

includet("figure_0_radiative_transfer_notebook.jl")

begin
    width = [0.4, 0.4, 0.4, 0.4]
    i_sample_point = [150, 150, 50, 50]
    latitude_r = deg2rad.([0, 40, 40, 40])
    j = [1, 1, 1, 122]
    interrow = [1.0, 1.0, 1.0, 1.0]
    height = [0.8, 0.8, 0.8, 0.8]
    diffuse_angles = true
    shape = [:rectangle, :dtriangle, :rectangle, :utriangle]
    h0 = [0.4, 0.4, 0.4, 0.4]
    display_text = [true, true, true, true]

    # Drawing(1000, 800, :png)
    Drawing(1000, 800, "2-outputs/plots/Fig.0_radiative_transfer.png")
    t2 = currentdrawing()
    scale(1, -1) # to set the y axis up
    translate(t2.width / 2, -t2.height / 2)

    tiles = Tiler(1000, 800, 2, 2, margin=0)

    background("white")

    index_plot = [3, 4, 1, 2] # The plot is mirrored in the y direction so we re-number the plots
    for (pos, n) in [(tiles[i][1], j) for (i, j) in enumerate(index_plot)]
        @layer begin
            draw_radiative_transfer(
                tiles.tilewidth, tiles.tileheight, pos, width[n],
                i_sample_point[n], latitude_r[n], j[n], interrow[n], height[n],
                diffuse_angles, shape[n], h0[n],
                display_text[n],
                text_height=0.18,
                outer_box_rel_width=0.95,
                outer_box_rel_height=0.55,
                text_color="grey50",
                n="$n. "
            )
        end
    end

    finish()
    preview()
end
