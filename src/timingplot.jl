
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
    sample = nothing
    t = nothing
    ugn = nothing
    kw = Dict()
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
    #onlytimes
end

struct BentEdge
    edgeid
    src
    dst
    ugn
    latency
    #onlytimes
end

function draw_outgoing_edge(ad::AxisDrawable, vs, extgraph, edge::StraightEdge)
    for node in extgraph.nodes[edge.src]
        p1 = Point(node)
        next_node = node_by_theta(extgraph, edge.dst, node.theta + edge.ugn)
        #if !isnothing(next_node) && (isnothing(edge.onlytimes) || node.theta in edge.onlytimes )
        if !isnothing(next_node) && vs.isvisible(edge.edgeid, node.theta) 
            p3 = Point(next_node)
            b = (vs.arrowpos, TriangularArrow(; size = vs.arrowsize))
            linestyle = vs.linestyle(edge.edgeid, node.theta)
            pth = Path(; arrows = (b,), linestyle)
            pth.points = [p1, p3]
            draw(ad, pth)
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






##############################################################################

# 
# function getpaths(; timinggraph_arrowpos = 0.5,
#                   timinggraph_linestyle = LineStyle(Color(:black),2),
#                   kwargs...)
#     linestyle = timinggraph_linestyle
#     a = (timinggraph_arrowpos, TriangularArrow(; size = 0.05))
#     b = (timinggraph_arrowpos, TriangularArrow(; size = 0.05))
#     verticalpath = Path(; arrows = (a,), linestyle)
#     straightpath = Path(; arrows = (b,), linestyle)
#     return verticalpath, straightpath
# end
# 
# 
# 
# function Timing(n, edges, bidirectional)
#     # red = pk.colormap(1)
#     # green = pk.colormap(2)
#     # blue = pk.colormap(3)
#     # yellow = pk.colormap(4)
#     # darkred = pk.colormap(5)
#     # darkgreen = pk.colormap(6)
#     # darkblue = pk.colormap(7)
#     # darkyellow = pk.colormap(8)
#     # lightred = pk.colormap(9)
#     # lightgreen = pk.colormap(10)
#     # lightblue = pk.colormap(11)
#     # lightyellow = pk.colormap(12)
#    
#     # m = length(edges)
#     # if n == 2
#     #     bufcols = [blue, yellow]
#     #     linkcols = [red, green]
#     # else
#     #     bcols = [blue, green, red, yellow]
#     #     lcols = [lightblue, lightgreen, lightred, lightyellow]
# 
#     #     function b(i)
#     #         return bcols[1 + i % 4]
#     #     end
#         
#     #     function l(i)
#     #         return lcols[1 + i % 4]
#     #     end
#         
#     #     bufcols = [b(e) for e=1:m]
#     #     linkcols = [l(e) for e=1:m]
#     # end
#     
# #    tmax = 10
# #    tmin = 0
# #    offsets = zeros(n)
# #    noderadius = 10
# #    nodefontsize = 14
# #    drawonlytimes = nothing
#                
# #    drawedges = collect(1:n-1)
# #    if bidirectional
# #        drawedges = collect(1:m)
# #    end
#     
# #    drawframes = true
# #    linestyle = LineStyle(Color(:black),2)
# 
# 
# 
# #    numberframes = false
# #    frameradius = 13
#     #framefontsize = 12
#     #framefontcolor = Color(:white)
#     #nodefontcolor = Color(:black)
# 
#    # nodeborderlinestyle = LineStyle(Color(:black),1)
#     #    nodefillcolor = Color(:white)
#     #nodelabelsincludeid = false
#     #scaletype = :none
#     ds = Timing(nothing,
#                 n,
#                 edges,
#  #               tmin,
#  #               tmax,
#  #               bufcols,
#  #               linkcols,
#  #               offsets,
# #                false,
# #                0.6,
# #                noderadius,
# #                nodefontsize,
# #                drawonlytimes,
# #                drawedges,
#                 drawframes,
#                 linestyle,
# #                verticalpath,
#                 numberframes,
#                 frameradius,
#                 framefontsize,
#                 framefontcolor,
#                 nodefontcolor,
#                 straightpath,
#                 nodeborderlinestyle,
#                 nodefillcolor,
#                 nodelabelsincludeid,
#                 scaletype
#                    )
#     return ds
# end


nodename(i) = string("ABCDEFGHIJKLMNOPQRSTUVWXYZ"[i])


##############################################################################


#
# We probably need something better than this.
#
# call this function with data not in
# the form of a sample
function drawtimingx(ad::AxisDrawable, vs::Timing, ticks, edgedict)
    extgraph = ExtGraph(ticks)

    for (e, edge) in edgedict 
        draw_outgoing_edge(ad, vs, extgraph, edge)
    end

    for node in allnodes(extgraph)
        draw_outgoing_vertical_edge(ad, vs, extgraph, node)
    end

    for node in allnodes(extgraph)
        draw_node(ad, vs, node)
    end
    
    return extgraph
end

function makeedge(ls, straight)
    if straight
        return StraightEdge(ls.edgeid, ls.src, ls.dst, ls.nzero)
    else
        return BentEdge(ls.edgeid, ls.src, ls.dst, ls.nzero, ls.latency)
    end
end

function drawtiming(ad::AxisDrawable, vs::Timing)

    makeedge(e) = StraightEdge(e, vs.edges[e].src, vs.edges[e].dst, vs.ugn[e])
    maketicks() = [(tick, tick) for tick = vs.tmin-10:vs.tmax+10]

    n = vs.n
    m = length(vs.edges)
    
    if isnothing(vs.sample)
        edgedict = Dict( e => makeedge(e) for e=1:m)
        ticks = [maketicks() for i=1:n]
    else
        edgedict = Dict(e => makeedge(vs.sample.linkstates[e], vs.straight) for e in vs.edges)
        ticks = sample.ticks
    end

    extgraph = drawtimingx(ad, vs, ticks, edgedict)

    # horizontal line indicating current value of t
    if vs.drawtime && !vs.straight
        line(ad, Point(-1, t), Point(vs.n+1, t); linestyle = vs.timelinestyle)
    end

    # draw frames
    if vs.drawframes && !vs.straight
        for e in vs.edges
            draw_link_frames(ad, vs, sample.linkstates[e], t)
            draw_buffer_frames(ad, vs, sample.linkstates[e], t)
        end
    end
end

##############################################################################

Timing(n, edges; kw...) = Timing(; n, edges, kw, allowed_kws(Timing, kw)...)

PlotKit.Axis(timing::Timing; kw...) = Axis(; axis_defaults(timing)..., timing.kw..., kw...)

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
    axis = Axis(timing; kw...)
    ad = AxisDrawable(axis)
    drawaxis(ad)
    setclipbox(ad)
    drawtiming(ad, timing)
    return ad
end



