
function example_theta()
    tmax = 40
    freq_left = cv.PiecewiseConstant([-10, 0,3,8,12,tmax],[ 0, 5/2, 1, 2, 1]./2)
    freq_right =  cv.PiecewiseConstant([-10, 0,2, 5,9,11,tmax],[ 0, 4, 2, 2/3, 3, 1]./2)
    theta = [cv.integrate(freq_left),  cv.integrate(freq_right)]
    return theta
end

##############################################################################

function random_pcp(tmin, tmax, dt)
    m = Int(round((tmax-tmin)/dt))
    t = cumsum(rand(m+1))
    t = t .- t[1]
    t = t/maximum(t)
    t = tmin .+ t*(tmax-tmin) 
    f = 0.5 .+ 1.5*rand(m)
    return cv.PiecewiseConstant(t, f)
end

    
adj_beta(src, dst, t, theta, latency) = floor(theta[src](t - latency)) - floor(theta[dst](t))

betamin(n, src, dst, theta, latency, tmax) = minimum(adj_beta(src, dst, t, theta, latency) for t in range(-1,tmax, length=2000))


function random_history(n; tmin = 0, tmax = 20, latency = 1)
    g = cv.Callisto.Graph(["line", n]; bidirectional = true)
    m = length(g.edges)
    theta = [ cv.integrate(random_pcp(-tmin - 3*latency, tmax, 2)) for i=1:n]
    for i=1:n
        theta[i] = 0.1 - theta[i](0) + theta[i]
    end
    nzero = [4 - betamin(n, g.edges[e][1], g.edges[e][2], theta, latency, tmax) for e=1:m]
    println((;nzero,))
    return g.edges, theta, nzero, latency
end


##############################################################################
function main()
    @testset "callisto visualizations" begin
        @test main1()
        @test main2()
        @test main3()
        @test main4()
    end
end

# draw a frames plot
function main1()
    num_nodes = 2
    edges = [ (1,2), (2,1) ]
    t = 3.6
    theta = example_theta()
    ph = PhaseHistory(edges, theta; nzero=[12,18])
    vis = Frames(num_nodes, edges)
    sample = Sample(ph, t)
    d = drawframes(vis, sample)
    qsave(d, "vtest1.pdf")
    return true
end

# animate the frames plot
function main2()
    num_nodes = 2
    edges = [ (1,2), (2,1) ]
    theta = example_theta()
    ph = PhaseHistory(edges, theta)
    vis = Frames(num_nodes, edges)

    f(t) = drawframes(vis, Sample(ph, t))
    anim = Anim(f)
    qsee(anim; tmax=1)
    return true
end

# create a phase plot
function main3()
    num_nodes = 2
    edges = [ (1,2), (2,1) ]
    theta = example_theta()
    ph = PhaseHistory(edges, theta)

    t = 3.6
    s = Sample(ph, t)
    dp = Phase(; xmin=-1, xmax=5, ymin=-10, ymax=20)
    d = drawphase(dp, s, t)
    qsave(d, "vtest2.pdf")
    return true
end

# timing plot
function main4()
    num_nodes = 5
    edges = [ (1,2), (2,1) ]
    edges, theta, nzero, latency = random_history(num_nodes)
    ph = PhaseHistory(edges, theta; latency)
    tg = Timing(num_nodes, edges;
                timing_straight = false,
                timing_tmax = 6,
                width = 1500,
                tmargin=10, bmargin = 30,
                height = 500, windowbackgroundcolor = (1,1,1))
    t = 3.6
    sample = Sample(ph, t)
    d = drawtiming(tg, sample, t)
    qsave(d, "vtest3.pdf")
    return true
end

# animated vis and phase plot
function main5()
    num_nodes = 2
    edges = [ (1,2), (2,1) ]
    theta = example_theta()
    ph = PhaseHistory(edges, theta; nzero=[20,10])
    vis = Frames(num_nodes, edges)
    dp = Phase(; height=vis.height, xmin=-1, xmax=30, ymin=-10, ymax=40)
    f(t) = begin
        s = Sample(ph, t)
        d = hbox(drawframes(vis, s), drawphase(dp, s, t))
        return d
    end
    anim = Anim(f)
    see(anim)
end

# animated vis and timing plot
function main6()
    n = 4
    Random.seed!(1)
    edges, theta, nzero, latency = random_history(n; latency = 4)
    ph = PhaseHistory(edges, theta; nzero, latency)
    vis = Frames(n, edges)
    tg = Timing(n, edges;
                timinggraph_straight = false,
                timinggraph_tmax = 16,
                width = 1000,
                height = vis.height,
                tmargin=60, bmargin = 60,
                windowbackgroundcolor = vis.windowbackgroundcolor)
    f(t) = begin
        s = Sample(ph, t)
        d = hbox(drawframes(vis, s), drawtiming(tg, s, t))
        return d
    end
    anim = Anim(f)
    see(anim)
end

# animated vis and timing plot
function main7()
    n = 2
    edges, theta, nzero, latency = random_history(n; tmax=100, latency=4)
    ph = PhaseHistory(edges, theta; nzero, latency)
    vis = Frames(n, edges)
    dp = Phase(; height=vis.height, xmin=-1, xmax=30, ymin=-10, ymax=40)

    tg = Timing(n, edges;
                timing_straight = false,
                timing_tmax = 40,
                width = 1500,
                tmargin=10, bmargin = 30,
                height = vis.height, windowbackgroundcolor = (1,1,1))
    f(t) = begin
        s = Sample(ph, t)
        d = hbox(hbox(drawframes(vis, s), drawtiming(tg, s, t)), drawphase(dp, s, t))
        return d
    end
    anim = Anim(f)
    see(anim)
end


# draw just the frames and the queue borders
function main8()
    num_nodes = 2
    edges = [ (1,2), (2,1) ]
    t = 3.6
    theta = example_theta()
    ph = PhaseHistory(edges, theta; nzero=[12,18])
    t = 12
    s = Sample(ph, t)
    vis = Frames(num_nodes, edges; layout_clockshift = Point[(200,0),(-200,0)])
    vis.draw_clock = (args...) -> return
    vis.windowbackgroundcolor=(1,1,1)
    sample = Sample(ph, t)
    d = drawframes(vis, sample)
    qsave(d, "vtest8.pdf")
    return true
end


# draw just the frames right-to-left and the queue borders
function main9()
    num_nodes = 2
    edges = [ (1,2), (2,1) ]
    t = 3.6
    theta = example_theta()
    ph = PhaseHistory(edges, theta; nzero=[12,18])
    
    t = 12
    s = Sample(ph, t)
    
    function f1(ctx, vis, b, ls)
        if b.edgeid == 1
            cv.draw_buffered_frame1(ctx, vis, b, ls)
        end
    end
    function f2(ctx, vis, m)
        if m.edgeid == 2
            cv.draw_moving_frame1(ctx, vis, m)
        end
    end
    
    vis = Frames(num_nodes, edges; layout_clockshift = Point[(200,0),(-200,0)],
                 layout_frameradius = 16, 
                 layout_scatterlen=0,
                 vis_draw_buffered_frame = f1,
                 vis_draw_moving_frame = f2)
    
    vis.draw_clock = (args...) -> return
    vis.windowbackgroundcolor = (1,1,1)
    d = drawframes(vis, s)
    qsave(d, "vtest9.pdf")
end
