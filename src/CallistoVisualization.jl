module CallistoVisualization


# independent modules
using PlotKit
using Callisto
const pk = PlotKit

# reeexport from PlotKit
export Anim, see

# visplot.jl
export Frames, drawframes

# timingplot.jl
export Timing, drawtiming

# phaseplot.jl
export Phase, drawphase

# phase.jl
export PhaseHistory, Sample



include("phase.jl")
include("timingplot.jl")
include("phaseplot.jl")
include("visplot.jl")

greet() = "Hello World!"

end
