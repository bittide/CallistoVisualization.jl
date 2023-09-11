module CallistoVisualization


# independent modules
using Cairo
using PlotKit
using Callisto
const pk = PlotKit

# reeexport from PlotKit
export Anim, see

# visplot.jl
#
# A plot showing the frames moving between the
# elastic buffers, with a clock at each elastic buffer.
#
# (used to be a Visualization object, now a Frames object)
# (used to call previs, Vis.plot)
export Frames, drawframes


# timingplot.jl
#
# A plot showing time on the vertical axis and the extended (unrolled) graph
# of events. It also shows edges connecting events that can communicate
# with each other, and frames traveling along those edges.
#
# (used to be called a VerticalPlot
export Timing, drawtiming


# phaseplot.jl
#
# A plot showing time on the horizontal axis and theta on the
# vertical axis. Frames are showing in a zig-zag pattern.
# Vertical lines show the transmission and reception times.
#
# (used to be called dotplot)
export Phase, drawphase


# phase.jl
#
# PhaseHistory is a data generation object. Given the graph 
# and theta, it can return the locations of all frames
# at any time t.
#
# (used to be called PhaseSim)
export PhaseHistory, Sample



include("phase.jl")
include("timingplot.jl")
include("phaseplot.jl")
include("visplot.jl")

greet() = "Hello World!"

end
