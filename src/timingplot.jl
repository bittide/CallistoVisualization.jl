

mutable struct Timing
    axis
    n
    edges
    tmax
    bufcols
    linkcols
    offsets
    straight
    arrowpos
    noderadius
    nodefontsize
    drawonlytimes
    drawedges
    drawframes
    linestyle
    verticalpath
    numberframes
    frameradius
    framefontsize
    framefontcolor
    nodefontcolor
    straightpath
    nodeborderlinestyle
    nodefillcolor
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

##############################################################################
# graph functions

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
    if ind > length(extgraph.nodes[i])
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

function draw_link_frames(ax, ctx, vs, linkstate, t)
    for mover in linkstate.movers
        alpha = mover.fraction_traveled
             
        # from x1,y1 to x2,y2
        x1 = linkstate.src
        x2 = (linkstate.src + linkstate.dst)/2

        x = x1 .+ alpha*(x2-x1)
        circle(ax, ctx, Point(x,t), vs.frameradius;
               fillcolor = vs.linkcols[linkstate.edgeid])
        if vs.numberframes
            text(ax, ctx, Point(x,t), vs.framefontsize, vs.framefontcolor,
                 string(Int64(round(mover.senders_theta)));
                 horizontal = "center", vertical = "center")
        end
    end
end

function draw_buffer_frames(ax, ctx, vs, linkstate, t)
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
        
        circle(ax, ctx, Point(x, t),  vs.frameradius;
               fillcolor = vs.bufcols[linkstate.edgeid])
         if vs.numberframes
            text(ax, ctx, Point(x,t), vs.framefontsize, vs.framefontcolor,
                 string(Int64(round(buffered_frame.senders_theta)));
                 horizontal = "center", vertical = "center")
        end
    end
end


function draw_outgoing_edges(ax, ctx, vs, extgraph, linkstate, straight=true)
    e = linkstate.edgeid
    src = linkstate.src
    dst = linkstate.dst
    for node in extgraph.nodes[src]
        p1 = Point(node)
        p2 = Point((src+dst)/2, node.time + linkstate.latency)
        next_node = node_by_theta(extgraph, dst, node.theta + linkstate.nzero)
        if !isnothing(next_node)
            p3 = Point(next_node)
            if straight
                draw(ax, ctx, p1, p3, getvar(vs.straightpath, e))
                #line(ax, ctx, p1, p3; linestyle = vs.linestyle, arrowpos = vs.arrowpos)
            else
                line(ax, ctx, [p1, p2, p3]; linestyle=vs.linestyle)
            end
        end
    end
end


function draw_node(ax, ctx, vs, node)
    sl = string(Int(round(node.theta  + vs.offsets[node.nodeid])))
    p = Point(node.nodeid, node.time)
    circle(ax, ctx, p, vs.noderadius;
           linestyle = vs.nodeborderlinestyle, fillcolor = vs.nodefillcolor)
    darkred = pk.colormap(185)
    text(ax, ctx, p, vs.nodefontsize, vs.nodefontcolor, sl;
         horizontal = "center", vertical = "center")
end

function draw_outgoing_vertical_edge(ax, ctx, vs, extgraph, node)
    nn = next_node(extgraph, node)
    if !isnothing(nn)
        draw(ax, ctx, Point(node), Point(nn), vs.verticalpath)
    end
end





##############################################################################



function Timing(n, edges, bidirectional)
    red = pk.colormap(345)
    green = pk.colormap(223)
    blue = pk.colormap(30)
    yellow = pk.colormap(5)
    lightred = pk.colormap(49)
    lightgreen = pk.colormap(343)
    lightblue = pk.colormap(22)
    lightyellow = pk.colormap(53)
    darkred = pk.colormap(185)
    darkgreen = pk.colormap(127)
    darkblue = pk.colormap(190)
    darkyellow = pk.colormap(325)

    m = length(edges)
    if n == 2
        bufcols = [blue, yellow]
        linkcols = [red, green]
    else
        bcols = [blue, green, red, yellow]
        lcols = [lightblue, lightgreen, lightred, lightyellow]

        function b(i)
            return bcols[1 + i % 4]
        end
        
        function l(i)
            return lcols[1 + i % 4]
        end
        
        bufcols = [b(e) for e=1:m]
        linkcols = [l(e) for e=1:m]
    end
    
    tmax = 10
    offsets = zeros(n)
    noderadius = 10
    nodefontsize = 14
    drawonlytimes = nothing
  
    drawedges = collect(1:n-1)
    if bidirectional
        drawedges = edges
    end
    
    drawframes = true
    linestyle = LineStyle((0,0,0),2)

    a = (0.5, TriangularArrow())
    b = (0.5, TriangularArrow())
    verticalpath = Path(; arrows = (a,), linestyle)
    straightpath = Path(; arrows = (b,), linestyle)

    numberframes = false
    frameradius = 13
    framefontsize = 12
    framefontcolor = (1,1,1)
    nodefontcolor = (0,0,0)

    nodeborderlinestyle = LineStyle((0,0,0),1)
    nodefillcolor = (1,1,1)
    ds = Timing(nothing,
                n,
                edges,
                tmax,
                bufcols,
                linkcols,
                offsets,
                false,
                0.6,
                noderadius,
                nodefontsize,
                drawonlytimes,
                drawedges,
                drawframes,
                linestyle,
                verticalpath,
                numberframes,
                frameradius,
                framefontsize,
                framefontcolor,
                nodefontcolor,
                straightpath,
                nodeborderlinestyle,
                nodefillcolor
                   )
    return ds
end

nodename(i) = string("ABCDEFGHIJKLMNOPQRSTUVWXYZ"[i])

function Timing(n, edges; bidirectional = false, kwargs...)
    defaults = Dict(
        :windowbackgroundcolor => hexcol(0xb0b0b0),
        :axisbox_ymin => -0.5,
        :axisbox_xmin => 0.75,
        :axisbox_xmax => n + 0.25,
        :ticks_xticks => vcat(0.75, collect(1:n), n+0.25),
        :ticks_xtickstrings => vcat("",  nodename.(collect(1:n)),""),
        :ticks_ytickstrings => ["", ""]
    )

    vs = Timing(n, edges, bidirectional)
    setoptions!(vs, "timinggraph_", merge(defaults, kwargs)...)
    
    axis = Axis(; merge(defaults, kwargs)...,
                box1_ymax = vs.tmax,
                ticks_yticks = [-0.5, vs.tmax])

    vs.axis = axis

    
    return vs
end

function drawtiming(vs::Timing, sample, t)
    d = Drawable(vs.axis)
    over(d) do ctx
        drawtiming(ctx, vs, sample, t)
    end
    return d
end


function drawtiming(ctx, vs::Timing, sample, t)
    extgraph = ExtGraph(sample.ticks)
    ax = vs.axis.ax
    
    if !vs.straight
        line(ax, ctx, Point(-1, t), Point(vs.n+1, t); linestyle = LineStyle((0,0,0, 0.5), 1))
    end

    for e in vs.drawedges
        draw_outgoing_edges(ax, ctx, vs, extgraph, sample.linkstates[e], vs.straight)
    end

    for node in allnodes(extgraph)
        draw_outgoing_vertical_edge(ax, ctx, vs, extgraph, node)
    end
    
    for node in allnodes(extgraph)
        draw_node(ax, ctx, vs, node)
    end
    
    if vs.drawframes && !vs.straight
        for e in vs.drawedges
            draw_link_frames(ax, ctx, vs, sample.linkstates[e], t)
            draw_buffer_frames(ax, ctx, vs, sample.linkstates[e], t)
        end
    end
end





