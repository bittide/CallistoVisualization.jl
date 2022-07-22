
# general structure of plotting
#
#   x = set_some_state(params; options...)
#   drawable = plot(data, x)
#   save(drawable, fname)
#
#  should also have a one-call version
#
#   drawable = autoplot(data)
#
#

# or for animation
#
#   x = set_some_state(params; options)
#   drawable = plot(data, x)
#   anim = Anim(drawable)
#   display_move(anim)
#


rot90(a::Point) = Point(-a.y, a.x)
    

mutable struct VisLayout
    lmargin
    rmargin
    tmargin
    bmargin
    clockradius
end

mutable struct GraphLayout
    x            # list of tuples, xy of a vertices
    w            # width of each connection
    dpth         # distance from elastic buffer to node
    frameradius
    clockshift   # function mapping node index to 2-tuple
                 # specifying relative position of clock from node
    scatterlen   # length in pixels of scatter buffer when full
    scattercap   # capacity of scatter buffer
    flippededges # bool m-vector, true if edge is flipped
end

mutable struct Geometry
    width
    height
    dstheads
    srcheads
    dsttails
    flippededges
    clocks
end

mutable struct Frames
    num_nodes
    edges
    width
    height
    clockradius
    frameradius
    dstheads
    srcheads
    dsttails
    flippededges
    clockcenters
    bufcols
    linkcols
    draw_buffer
    draw_moving_frame
    draw_buffered_frame
    draw_clock
#    speed
#    tmax
    windowbackgroundcolor
    squeezebuffers
    squeezemax
    numberframes
    framefontsize 
    framefontcolor
    showclockids
    clockids
    clockthetafontsize
end


struct Buffer
    exit
    tail
    flipped
end

##############################################################################

function default_vislayout()
    return VisLayout(80, 80, 80, 80, 50)
end

function default_graphlayout(vl, n, m)
    if n == 2
        x = Point[(150,500), (1000,500)]
        w = 700
        dpth = 80
        frameradius = 13
        clockshift = Point[(80,-500), (-80,-500)]
    elseif n == 3
        x = Point[(100,200), (1000,200), (550, 1000)]
        w = 180
        dpth = 200
        frameradius = 6
        clockshift = fill( Point(0, 0), n)
    elseif n == 4
        x = Point[(100,200), (1000,200), (550, 1000),  (1450, 1000)]
        w = 80
        dpth = 100
        frameradius = 6
        clockshift = fill( Point(0, 0), n)
    end
    scatterlen = 0  
    scattercap = 4
    flippededges = fill(true, m)
    return GraphLayout(x, w, dpth, frameradius, clockshift, scatterlen, scattercap, flippededges)
end



# x is list of tuples, xy of a vertices
function Frames(n, edges; kwargs...)
    m = length(edges)
    vl = default_vislayout()
    setoptions!(vl, "", kwargs...)
  
    gl = default_graphlayout(vl, n, m)
    setoptions!(gl, "layout_", kwargs...)
    
    geom = graphlayout_to_geometry(vl, gl, edges)
    setoptions!(geom, "geom_", kwargs...)
    
    vis = Frames(n, edges, geom, vl, gl)
    setoptions!(vis, "vis_", kwargs...)
    
    return vis
end


# construct a rectangle with centerline from p1 = (x1,y1) to p2 = (x2,y2)
# starting distance d from p1 end and ending distance d from the p2 end
# and width w
function zrect(p1, p2, dpth, w)
    dir = (p2-p1)
    dir = dir / norm(dir)
    perp = -(w/2) * rot90(dir)
    forw = dpth * dir
    # returns points a,b,c,d
    # in anticlockwise order
    # a,b at p1 end
    a = p1 + perp + forw
    b = p1 - perp + forw
    c = p2 - perp - forw
    d = p2 + perp - forw
    return a,b,c,d
end

# 
function scatterpos(receive, send, l)
    eb = send - receive
    eb = eb / norm(eb)
    perp = l*rot90(eb)
    return send + perp
end

function gatherpos(receive, send, l)
    eb = send - receive
    eb = eb / norm(eb)
    return send + l*eb
end

Base.round(a::Point) = Point(Int(round(a.x)), Int(round(a.y)))


function graphlayout_to_geometry(vl, gl, edges)
    n = length(gl.x)
    m = length(edges)
    dstheads = Array{Any,1}(undef, m)
    srcheads = Array{Any,1}(undef, m)
    dsttails = Array{Any,1}(undef, m)
    clocks = Array{Any,1}(missing, n)
    for e = 1:m
        src, dst = edges[e]
        p1 = gl.x[src]
        p2 = gl.x[dst]
        a, b, c, d = zrect(p1, p2, gl.dpth, gl.w)
        dstheads[e] = round(d) # tail of buffer at dst node
        dsttails[e] = round(c) 
        srcheads[e] = round(b) # exit of buffer at src node
        # frames on edge e move from srcexit to dsttail to dstexit

        if gl.flippededges[e]
            dstheads[e] = round(c)
            dsttails[e] = round(d)
            srcheads[e] = round(a)
        end
    end




    for i=1:n
        clocks[i] = gl.x[i] + gl.clockshift[i]
    end


    # compute margins
    sr = collect(skipmissing(copy(srcheads)))
    append!(sr, collect(skipmissing(copy(dstheads))))
    append!(sr, collect(skipmissing(copy(dsttails))))

    xmin = min(minimum([a.x for a in sr]), minimum([clocks[i].x - vl.clockradius for i=1:n]))
    ymin = min(minimum([a.y for a in sr]), minimum([clocks[i].y - vl.clockradius for i=1:n]))
    xmax = max(maximum([a.x for a in sr]), maximum([clocks[i].x + vl.clockradius for i=1:n]))
    ymax = max(maximum([a.y for a in sr]), maximum([clocks[i].y + vl.clockradius for i=1:n]))

    d = Point(vl.lmargin, vl.tmargin) - Point(xmin, ymin) 

    width = xmax - xmin + vl.lmargin + vl.rmargin 
    height = ymax - ymin + vl.tmargin + vl.bmargin

    function shift!(x)
        for ind in eachindex(skipmissing(x))
            x[ind] += d
        end
    end
    
    shift!(dstheads)
    shift!(srcheads)
    shift!(dsttails)

    c = [a + d for a in clocks]
    
    return Geometry(width, height, dstheads, srcheads, dsttails, gl.flippededges, c)
end



function draw_buffer1(ctx, vis, l)
    s = l.exit
    r = l.tail
    dir = (r - s)
    dir = dir / norm(dir)
    perp = -1 * rot90(dir)
    hw = round(1.4*vis.frameradius)
    linestyle = LineStyle((0,0,0),1)
    if l.flipped
        perp = -1 * perp
    end

    line(ctx, [r - hw * perp - hw * dir, s - hw * perp + hw * dir]; linestyle)
    line(ctx, [r - hw * perp + hw * dir, r + hw * perp + hw * dir,
                   s + hw * perp - hw * dir, s - hw * perp - hw * dir]; linestyle)
        
end

# when theta=0, returns x
# then theta=1, returns y
interp(x::Point, y::Point, theta) = (1-theta)*x + theta*y

moving_frame_position(m) = interp(m.startingpoint, m.endingpoint, (m.t - m.dt)/m.latency)

#
# needs
#    (m.t - m.dt)/m.latency
#    m.edgeid
#    label
#
function draw_moving_frame1(ctx, vis, m)
    e = m.edgeid
    col =  vis.linkcols(e)
    z = interp(vis.srcheads[e], vis.dsttails[e], m.fraction_traveled)
    circle(ctx, z, vis.frameradius; fillcolor = col)
    if vis.numberframes
        text(ctx, z, vis.framefontsize,
             vis.framefontcolor, string(Int64(round(m.senders_theta)));
             horizontal = "center", vertical = "center")
    end
end

# spreads out from beginning to end
#buffered_frame_position(b) = interp(b.endingpoint, b.startingpoint, (b.theta - b.thetamin)/(b.thetamax - b.thetamin))
buffered_frame_position(src, dst, b) = interp(dst, src, (b.theta - b.thetamin)/(b.thetamax - b.thetamin))




function squeeze(z, zmin, zmax, nmax, i, n)
    # z = receivers_theta
    #
    # frames near the head of the buffer
    # have the smallest z
    #
    # we return a number from 0 to 1
    # with 1 meaning draw the frame at the head
    # and 0 meaning draw the frame at the tail
    #
    # frames in the buffer are numbered 1 to n
    # i = 1 is the frame that's nearest the head
    # i.e. about to leave
    #
    # all other frames are stacked at the tail end
    # of the buffer
    if i > 1
        return (zmax - z)/nmax
    end
    
    # the first frame we interpolate between the top
    # of the buffer and where it would have been
    y_would_have_been = (zmax - z)/nmax
    y = 1 - (z - zmin) * (1 - y_would_have_been)
    return y
end


# frame in buffer at node i, associated with link to j, with value theta
#
# needs
#    (b.theta - b.thetamin)/(b.thetamax - b.thetamin)
#    b.edgeid
#
function draw_buffered_frame1(ctx, vis, b, linkstate)
    e = b.edgeid
    col =  vis.bufcols(e)
    if vis.squeezebuffers
        pos = squeeze(b.receivers_theta, linkstate.min_receiver_theta_in_buffer,
                      linkstate.max_receiver_theta_in_buffer,
                      vis.squeezemax, b.frameidx,
                      length(linkstate.occupants))
    else
        pos = b.fraction_traveled
    end
    z = interp(vis.dsttails[e],  vis.dstheads[e], pos)
    circle(ctx, z, vis.frameradius; fillcolor = col)
    if vis.numberframes
        text(ctx, z, vis.framefontsize,
             vis.framefontcolor, string(Int64(round(b.senders_theta)));
             horizontal = "center", vertical = "center")
    end
end

function draw_clock1(ctx, vis, i, theta)
    r = vis.clockradius
    x = vis.clockcenters[i]
    circle(ctx, x, r; linestyle=LineStyle( (0,0,0), 4), fillcolor=(200,200,200) )
    ls = LineStyle((0.8,0.8,1), 4)

    dx = Point(r * sin(theta*2*pi), - r * cos(theta*2*pi))
    
    line(ctx, x, x + dx; linestyle = ls)
    circle(ctx, x, 8; linestyle=ls, fillcolor=(100,100,100) )
    fsize=14

    if vis.showclockids
        text(ctx, x, fsize, (0,0,0), vis.clockids[i]; horizontal="center", vertical="center")
    end

    text(ctx, x + Point(0, -22), vis.clockthetafontsize, (1,0,0), string(Int64(floor(theta)));
             horizontal = "center", vertical = "center")
end


function Frames(n, edges, geom, vl, gl)
    #    speed = 1/2
    #    tmax = 30
    # colors
    red = pk.colormap(345)
    green = pk.colormap(223)
    blue = pk.colormap(30)
    yellow = pk.colormap(5)
    
    if n == 2
        bufcols = e -> [blue, yellow][e]
        linkcols = e -> [red, green][e]
    else
        bufcols = e -> blue
        linkcols = e -> red
    end

    draw_buffer2 = (ctx, vis, b) -> return

    windowbackgroundcolor = hexcol(0xb0b0b0)
    numberframes = true
    framefontsize = 12
    framefontcolor = (1,1,1)
    showclockids = false
    squeezebuffers = false
    squeezemax = 40
    clockids = [string(k) for k=1:n]
    clockthetafontsize = 18
    Frames(n,
           edges,
           geom.width,
           geom.height,
           vl.clockradius,
           gl.frameradius,
           geom.dstheads,
           geom.srcheads,
           geom.dsttails,
           geom.flippededges,
           geom.clocks,
           bufcols,
           linkcols,
           draw_buffer1,
           draw_moving_frame1,
           draw_buffered_frame1,
           draw_clock1,
           windowbackgroundcolor,
           squeezebuffers,
           squeezemax,
           numberframes,
           framefontsize,
           framefontcolor,
           showclockids,
           clockids,
           clockthetafontsize
           )
end




function get_buffers_to_draw(vis)
    m = length(vis.edges)
    buffers = []
    for e=1:m
        push!(buffers, Buffer(vis.dstheads[e], vis.dsttails[e], vis.flippededges[e] ))
    end
    return buffers
end

function drawframes(ctx::PlotKit.Cairo.CairoContext, vis, visstate)
    for i=1:vis.num_nodes
        vis.draw_clock(ctx, vis, i, visstate.theta[i])
    end

    buffers = get_buffers_to_draw(vis)
    for b in buffers
        vis.draw_buffer(ctx, vis, b)
    end

    for linkstate in visstate.linkstates
        for b in linkstate.occupants
            vis.draw_buffered_frame(ctx, vis, b, linkstate)
        end
    end
    
    for linkstate in visstate.linkstates
        for m in linkstate.movers
            vis.draw_moving_frame(ctx, vis, m)
        end
    end

end

function drawframes(vis::Frames, visstate)
    d = Drawable(vis.width, vis.height) do ctx
        rect(ctx, Point(0,0), Point(vis.width, vis.height);
             fillcolor = vis.windowbackgroundcolor)
        drawframes(ctx, vis, visstate)
    end
    return d
end




