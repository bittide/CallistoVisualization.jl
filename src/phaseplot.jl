


mutable struct Phase
    axis
    bufcols
    linkcols
    edge
    oppedge
    ymin
    ymax
    drawoppedge
    drawedge
end

function hline(ax, ctx, xmin, xmax, y)
    line(ax, ctx, Point(xmin, y), Point(xmax, y); linestyle = LineStyle( (0,0,0), 1))
end

function vline(ax, ctx, x, ymin, ymax)
    line(ax, ctx, Point(x, ymin), Point(x, ymax); linestyle = LineStyle( (0,0,0), 2))
end

function drawphase(ctx, dp::Phase, fl, t)
    ax = dp.axis.ax
    edgeid = dp.edge
    oppedge = dp.oppedge
    latency = maximum(ls.latency for ls in fl.linkstates)

    vline(ax, ctx, t, dp.ymin, dp.ymax)
    vline(ax, ctx, t-latency, dp.ymin, dp.ymax)

    linkstate = fl.linkstates[edgeid]
    opplinkstate = fl.linkstates[oppedge]
    
    hline(ax, ctx, t-latency, t, opplinkstate.min_receiver_theta_in_buffer - opplinkstate.nzero)
    hline(ax, ctx, t-latency, t, opplinkstate.min_receiver_theta_in_buffer + linkstate.nzero)

    src, dst = fl.edges[edgeid]
    p = [Point(a[1], a[2] + linkstate.nzero) for a in fl.theta_knots[src]]
    line(ax, ctx, p; linestyle=LineStyle((0,0,0), 2))

    p = [Point(a[1], a[2]) for a in fl.theta_knots[dst]]
    line(ax, ctx, p; linestyle=LineStyle((0,0,0), 2))

    if dp.drawedge
        for moving_frame in linkstate.movers
            p = Point(moving_frame.send_time,  moving_frame.receivers_theta)
            circle(ax, ctx, p, 6; fillcolor = dp.linkcols[edgeid])
        end
    end

    if dp.drawoppedge
        for moving_frame in opplinkstate.movers
            p = Point(moving_frame.send_time,  moving_frame.senders_theta)
            circle(ax, ctx, p, 6; fillcolor = dp.linkcols[oppedge])
        end
    end

    if dp.drawedge
        for buffered_frame in linkstate.occupants
            p = Point(t - (1-buffered_frame.fraction_traveled)*latency, buffered_frame.receivers_theta)
            circle(ax, ctx, p, 6;fillcolor = dp.bufcols[edgeid])
        end
    end

    if dp.drawoppedge
        for buffered_frame in opplinkstate.occupants
            p = Point(t - (1-buffered_frame.fraction_traveled)*latency, buffered_frame.senders_theta)
            circle(ax, ctx, p, 6;fillcolor = dp.bufcols[oppedge])
        end
    end

end

function Phase(axis, ymin, ymax)
    red = pk.colormap(345)
    green = pk.colormap(223)
    blue = pk.colormap(30)
    yellow = pk.colormap(5)
    
    bufcols = [blue, yellow]
    linkcols = [red, green]
    edge = 1
    oppedge = 2
    drawedge = true
    drawoppedge = true
    dp = Phase(axis, bufcols, linkcols, edge, oppedge, ymin, ymax, drawedge, drawoppedge)
    return dp
end

function Phase(; kwargs...)
    defaults = Dict(:windowbackgroundcolor => hexcol(0xb0b0b0), )
    axis = Axis(; merge(defaults, kwargs)...)
    dp = Phase(axis, axis.box.ymin, axis.box.ymax)
    setoptions!(dp, "dotplot_", kwargs...)
    return dp
end

function drawphase(dp::Phase, fl, t)
    d = Drawable(dp.axis)
    over(d) do ctx
        drawphase(ctx, dp::Phase, fl, t)
    end
    return d
end
