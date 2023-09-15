
const    red = pk.colormap(1)
const    green = pk.colormap(2)
const    blue = pk.colormap(3)
const    yellow = pk.colormap(4)
const    darkred = pk.colormap(5)
const    darkgreen = pk.colormap(6)
const    darkblue = pk.colormap(7)
const    darkyellow = pk.colormap(8)
const    lightred = pk.colormap(9)
const    lightgreen = pk.colormap(10)
const    lightblue = pk.colormap(11)
const    lightyellow = pk.colormap(12)


default_offsets(i) = 0

Base.@kwdef mutable struct Timing
    n
    edges
    axis = nothing
    sample = nothing
    t = nothing
    ugn = nothing  # used to create edges when there is no sample
    tmin = 0
    tmax = 10
    bufcols = default_bufcols
    linkcols = default_linkcols
    offsets = default_offsets
    straight = false
    arrowpos = 0.6
    arrowsize = 0.05
    noderadius = 10
    nodefontsize = 14
    isvisible = (edgeid, theta) -> true
    drawframes = true
    linestyle = (edgeid, theta) -> LineStyle(Color(:black),2)
    verticallinestyle = LineStyle(Color(:black),2)
    timelinestyle = LineStyle(Color(0,0,0, 0.5), 1)
    numberframes = false
    frameradius = 13
    framefontsize = 12
    framefontcolor = Color(:white)
    nodefontcolor = Color(:black)
    nodeborderlinestyle = LineStyle(Color(:black),1)
    nodefillcolor = Color(:white)
    nodelabelsincludeid = false
    scaletype = :none
    drawtime = true
    edgedict = Dict()
    ticks = []  # only used to create extgraph
    extgraph = nothing
    straightedgefn = nothing
end


    

struct Node
    nodeid
    time
    theta
end

struct ExtGraph
    nodes
    starting_thetas
end

getvar(x::Array, i) = x[i]
getvar(x,i) = x

function default_bufcols(vs::Timing, i)
    if vs.n == 2
        return (blue, yellow)[i]
    end
    return (blue, green, red, yellow)[1 + i % 4]
end

function default_linkcols(vs::Timing, i)
    if vs.n == 2
        return (red, green)[i]
    end
    return (lightblue, lightgreen, lightred, lightyellow)[1 + i % 4]
end

##############################################################################
# graph functions

#
# ticks is a list of length n
# each ticks[i] is a list of tuples  (t, theta[i](t))
# for each tick of clock i 
#
function ExtGraph(ticks)
    nodes = [[ Node(i, time, Int(theta)) for (time, theta) in ticks[i]] for i=1:length(ticks)]
    starting_thetas = Int64[nodelist[1].theta for nodelist in nodes]
    return ExtGraph(nodes, starting_thetas)
end

function allnodes(extgraph::ExtGraph)
    return [n for nodelist in extgraph.nodes for n in nodelist ]
end

function node_by_theta(extgraph::ExtGraph, i, theta)
    ind = Int(theta - extgraph.starting_thetas[i]+1)
    if ind > length(extgraph.nodes[i]) || ind < 1
        return nothing
    end
    return extgraph.nodes[i][ind]
end

function next_node(extgraph::ExtGraph, n::Node)
    return node_by_theta(extgraph, n.nodeid, n.theta+1)
end

function PlotKit.Point(n::Node)
    return Point(n.nodeid, n.time)
end

##############################################################################

function draw_link_frames(ad::AxisDrawable, vs, linkstate, t)
    for mover in linkstate.movers
        alpha = mover.fraction_traveled
        
        # from x1,y1 to x2,y2
        x1 = linkstate.src
        x2 = (linkstate.src + linkstate.dst)/2

        x = x1 .+ alpha*(x2-x1)
        circle(ad, Point(x,t), vs.frameradius;
               scaletype = vs.scaletype, 
               fillcolor = vs.linkcols(vs, linkstate.edgeid))
        if vs.numberframes
            text(ad, Point(x,t), vs.framefontsize, vs.framefontcolor,
                 string(Int64(round(mover.senders_theta)));
                 scaletype = vs.scaletype, 
                 horizontal = "center", vertical = "center")
        end
    end
end

function draw_buffer_frames(ad::AxisDrawable, vs, linkstate, t)
    for buffered_frame in linkstate.occupants
               
        # y2 is the time it was sent + latency
        # equivalently, the time it was received at the tail of the dst buffer
        y2 = buffered_frame.send_time + linkstate.latency
        x2 = (linkstate.src  + linkstate.dst)/2

        # y3 is the time at which it was pulled out of the elastic
        # buffer by the receiver
        y3 = buffered_frame.receive_time
        x3 = linkstate.dst 

        # intersect line y=t
        # with line from (x2,y2) to (x3,y3)
        alpha = (t-y2)/(y3-y2)
        x = x2 + alpha*(x3-x2)
        
        circle(ad, Point(x, t),  vs.frameradius;
               scaletype = vs.scaletype,
               fillcolor = vs.bufcols(vs, linkstate.edgeid))
        if vs.numberframes
            text(ad, Point(x,t), vs.framefontsize, vs.framefontcolor,
                 string(Int64(round(buffered_frame.senders_theta)));
                 scaletype = vs.scaletype,
                 horizontal = "center", vertical = "center")
        end
    end
end

struct StraightEdge
    edgeid
    src
    dst
    ugn
end

struct BentEdge
    edgeid
    src
    dst
    ugn
    latency
end

function draw_outgoing_edge(ad::AxisDrawable, vs, extgraph, edge::StraightEdge)
    for node in extgraph.nodes[edge.src]
        p1 = Point(node)
        next_node = node_by_theta(extgraph, edge.dst, node.theta + edge.ugn)
        if !isnothing(next_node) && vs.isvisible(edge.edgeid, node.theta)
            p3 = Point(next_node)
            if !isnothing(vs.straightedgefn)
                vs.straightedgefn(ad, p1, p3, edge.edgeid, node.theta)
            else
                b = (vs.arrowpos, TriangularArrow(; size = vs.arrowsize))
                linestyle = vs.linestyle(edge.edgeid, node.theta)
                pth = Path(; arrows = (b,), linestyle)
                pth.points = [p1, p3]
                draw(ad, pth)
            end
        end
    end
end

function draw_outgoing_edge(ad::AxisDrawable, vs, extgraph, edge::BentEdge)
    for node in extgraph.nodes[edge.src]
        p1 = Point(node)
        p2 = Point((edge.src + edge.dst)/2, node.time + edge.latency)
        next_node = node_by_theta(extgraph, edge.dst, node.theta + edge.ugn)
        if !isnothing(next_node)
            p3 = Point(next_node)
            b = (vs.arrowpos, TriangularArrow(; size = vs.arrowsize))
            linestyle = vs.linestyle(edge.edgeid, node.theta)
            pth = Path(; arrows = (b,), linestyle)
            pth.points = [p1, p2,  p3]
            draw(ad, pth)
        end
    end
end

function draw_node(ad::AxisDrawable, vs, node)
    nodetime = Int(round(node.theta  + vs.offsets(node.nodeid)))
    sl = string(nodetime)
    if vs.nodelabelsincludeid
        sl = string(node.nodeid) * ", " * string(nodetime)
    end
    p = Point(node.nodeid, node.time)
    circle(ad, p, vs.noderadius;
           scaletype = vs.scaletype,
           linestyle = vs.nodeborderlinestyle, fillcolor = vs.nodefillcolor)
    darkred = pk.colormap(185)
    text(ad, p, vs.nodefontsize, vs.nodefontcolor, sl;
         scaletype = vs.scaletype,
         horizontal = "center", vertical = "center")
end

function draw_outgoing_vertical_edge(ad::AxisDrawable, vs, extgraph, node)
    nn = next_node(extgraph, node)
    if !isnothing(nn)
        a = (vs.arrowpos, TriangularArrow(; size = 0.05))
        pth = Path(; arrows = (a,), linestyle = vs.verticallinestyle)
        pth.points = [Point(node), Point(nn)]
        draw(ad, pth)
    end
end

nodename(i) = string("ABCDEFGHIJKLMNOPQRSTUVWXYZ"[i])

##############################################################################

function makeedge(ls, straight)
    if straight
        return StraightEdge(ls.edgeid, ls.src, ls.dst, ls.nzero)
    else
        return BentEdge(ls.edgeid, ls.src, ls.dst, ls.nzero, ls.latency)
    end
end

function drawtiming(ad::AxisDrawable, vs::Timing)

    for (e, edge) in vs.edgedict 
        draw_outgoing_edge(ad, vs, vs.extgraph, edge)
    end

    for node in allnodes(vs.extgraph)
        draw_outgoing_vertical_edge(ad, vs, vs.extgraph, node)
    end

    for node in allnodes(vs.extgraph)
        draw_node(ad, vs, node)
    end
    
    # horizontal line indicating current value of t
    if vs.drawtime && !vs.straight
        line(ad, Point(-1, vs.t), Point(vs.n+1, vs.t); linestyle = vs.timelinestyle)
    end

    # draw frames
    if vs.drawframes && !vs.straight
        #for e in vs.edges
        for e in 1:length(vs.edges)
            draw_link_frames(ad, vs, vs.sample.linkstates[e], vs.t)
            draw_buffer_frames(ad, vs, vs.sample.linkstates[e], vs.t)
        end
    end
end

##############################################################################

function setup!(timing::Timing)
    nsmakeedge(e) = StraightEdge(e, timing.edges[e].src,
                               timing.edges[e].dst, timing.ugn[e])
    maketicks() = [(tick, tick) for tick = timing.tmin-10:timing.tmax+10]

    n = timing.n
    m = length(timing.edges)
    
    if isnothing(timing.sample)
        timing.edgedict = Dict( e => nsmakeedge(e) for e=1:m)
        timing.ticks = [maketicks() for i=1:n]
    else
        #timing.edgedict = Dict(e => makeedge(timing.sample.linkstates[e],
        #                                     timing.straight) for e in timing.edges)
        timing.edgedict = Dict(e => makeedge(timing.sample.linkstates[e],
                                      timing.straight) for e=1:m)
        timing.ticks = timing.sample.ticks
    end
    timing.extgraph = ExtGraph(timing.ticks)
end

function Timing(n, edges; kw...)
    timing = Timing(; n, edges, allowed_kws(Timing, kw)...)
    setup!(timing)
    axis = Axis(; axis_defaults(timing)..., kw...)
    timing.axis = axis
    return timing
end



axis_defaults(timing) = Dict(
    :windowbackgroundcolor => hexcol(0xb0b0b0),
    :axisbox_ymin => timing.tmin,
    :axisbox_ymax => timing.tmax,
    :axisbox_xmin => 0.75,
    :axisbox_xmax => timing.n + 0.25,
    :ticks_xticks => vcat(0.75, collect(1:timing.n), timing.n + 0.25),
    :ticks_yticks => [0,1],
    :ticks_xtickstrings => vcat("",  nodename.(collect(1:timing.n)),""),
    :ticks_ytickstrings => ["",""]
)


function PlotKit.draw(timing::Timing; kw...)
    axis = timing.axis
    ad = AxisDrawable(axis)
    drawaxis(ad)
    setclipbox(ad)
    drawtiming(ad, timing)
    return ad
end



