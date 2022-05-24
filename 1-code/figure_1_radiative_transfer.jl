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
                tiles.tilewidth, tiles.tileheight, pos;
                width=width[n],
                i_sample_point=i_sample_point[n],
                latitude_r=latitude_r[n],
                j=j[n],
                interrow=interrow[n],
                height=height[n],
                diffuse_angles=diffuse_angles,
                shape=shape[n],
                h0=h0[n],
                alpha=deg2rad(0.0),
                rg=20,
                k=0.8,
                lai=2.0,
                display_text=display_text[n],
                title_height=0.18,
                outer_box_rel_width=0.95,
                outer_box_rel_height=0.55,
                ri_text_pos=(-0.24, 0.1),
                text_color="grey50",
                n="$n. "
            )
        end
    end

    finish()
    preview()
end
