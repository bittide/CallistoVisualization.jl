

module RunTests

using Random
using Test
using CallistoVisualization

const cv = CallistoVisualization
const pk = cv.PlotKit

plotpath(x) = joinpath(tempdir, x)

qsave(f, p) = cv.PlotKit.save(f, "/tmp/cvtest.png")
qsee(args...; kwargs...) = true



include("testset.jl")
end


using .RunTests
RunTests.main()
