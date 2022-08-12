





# integers between t1 and t2
function integers(theta, t1, t2)
    th = after(theta, t1)
    th = before(th, t2)
    x,y = integer_crossings(th)
    return x, y
end
    

mutable struct PhaseHistory
    theta
    nzero
    latency
    gears
    edges
    num_nodes
    ticks
end

function get_ticks(theta)
    times, theta = integers(theta, theta.x[1], theta.x[end])
    return zip(times, theta)
end

#
# edges should be bidirectional
# 
function PhaseHistory(edges, theta; make_ticks = true, nzero = 12, latency = 3, gears = 1)
    n = length(theta)
    m = length(edges)
    if isa(nzero, Number)
        nzero = fill(nzero, m)
    end
    if isa(latency, Number)
        latency = fill(latency, m)
    end
    if isa(gears, Number)
        gears = fill(gears, m)
    end
    if make_ticks
        ticks = [get_ticks(th) for th in theta]
    else
        ticks = []
    end
    
    ps = PhaseHistory(theta, nzero, latency, gears, edges, n, ticks)
end

mutable struct Sample
    num_nodes
    edges
    theta_knots
    theta
    linkstates
    ticks
end

struct LinkState
    edgeid
    src
    dst
    movers
    occupants
    nzero
    latency
    min_receiver_theta_in_buffer # lower bound
    max_receiver_theta_in_buffer # upper bound
end


# Frames in the elastic buffer (occupants)
# Each edge has an Occupants object
struct Occupant
    edgeid
    senders_theta
    receivers_theta    # list of integers
    send_time          # times at which they left the sender's EB
    receive_time       # pop time
    fraction_traveled
    frameidx
end



# each edge has a list of movers
# send_time is a list of times
# senders_theta is a list of theta values
struct Mover
    edgeid
    senders_theta
    receivers_theta
    send_time
    fraction_traveled
end

#
# If zmin < the minimum value of p, then 
# there is no meaningful x at which p(x) = y
# when y < zmin.
#
# So for integers in the range (zmin, zmax]
# which are less that the minimum value of p
# we assign them an x value of -Inf.
#
# The application of this is to frames that 
# are in the buffer, but for which we do not have
# a model of the time they were sent, since theta
# at negative times is constant.
#
function adjusted_integer_crossings_in_interval(p, zmin, zmax)
    if zmin < p.y[1] || zmax > p.y[end]
        zmin_adj = max(p.y[1], zmin)
        zmax_adj = min(p.y[end], zmax)

        partial_x, partial_y  = integer_crossings_in_interval(p, zmin_adj  , zmax_adj )

        y = collect(IntegersBetween(zmin, zmax))
        x = zeros(length(y))

        if length(partial_y) > 0
            i1 = findfirst(a -> a == partial_y[1], y)
            i2 = findfirst(a -> a == partial_y[end], y)
            
            if !isnothing(i1) && !isnothing(i2)
                x[i1:i2] = partial_x
                if i1>1
                    x[1:i1-1] .= -Inf
                end
                if i2 < length(x)
                    x[i2+1:end] .= Inf
                end
            end
        end
    else
        x, y  = integer_crossings_in_interval(p, zmin  , zmax )
    end
    return x, y
end

function get_occupant_info(ps::PhaseHistory, e, t)
    src, dst = ps.edges[e]
    theta_src_del = ps.theta[src](t - ps.latency[e])
    zmin = ps.gears[e]*ps.theta[dst](t)
    zmax = ps.gears[e]*theta_src_del + ps.nzero[e]

   # Some care is needed here. Some frames were never sent,
    # they just started in the buffer.
    send_times, receivers_theta  = adjusted_integer_crossings_in_interval(ps.theta[src] + ps.nzero[e], zmin, zmax)
    receiver_times, rtheta = adjusted_integer_crossings_in_interval(ps.theta[dst], zmin, zmax)
    return zmin, zmax, receivers_theta, send_times, receiver_times
end

simple_frac(z, zmin, zmax) = 1 .- ((z .- zmin) ./ (zmax .- zmin))

function Occupants(ps::PhaseHistory, e, t)
    zmin, zmax, receivers_theta, send_times, receiver_times = get_occupant_info(ps::PhaseHistory, e, t)

    fraction_traveled = simple_frac(receivers_theta, zmin, zmax)
    
    senders_theta = receivers_theta .- ps.nzero[e]
    
    occupants = [Occupant(e,
                          senders_theta[i],
                          receivers_theta[i],
                          send_times[i],
                          receiver_times[i],
                          fraction_traveled[i],
                          i)
                 for i=1:length(receivers_theta)]
    
    return zmin, zmax, occupants
end

function get_mover_info(ps::PhaseHistory, e, t)
    src, dst = ps.edges[e]
    x, y = integers(ps.gears[e] * ps.theta[src], t - ps.latency[e], t)
    return x, y
end

function Movers(ps::PhaseHistory, e, t)
    send_times, senders_theta = get_mover_info(ps::PhaseHistory, e, t)
    fraction_traveled = (t .- send_times) ./ ps.latency[e]
    receivers_theta = senders_theta .+ ps.nzero[e]
    movers = [Mover(e,
                    senders_theta[i],
                    receivers_theta[i],
                    send_times[i],
                    fraction_traveled[i]) for i=1:length(receivers_theta)]
    return movers
end

function LinkState(ps::PhaseHistory, e, t)
    zmin, zmax, occupants = Occupants(ps, e, t)

    movers = Movers(ps, e, t)
    src,dst = ps.edges[e]
    return LinkState(e, src, dst, movers, occupants, ps.nzero[e], ps.latency[e], zmin, zmax)
end

function Sample(ps::PhaseHistory, t)
    n = ps.num_nodes
    edges = ps.edges  # bidirectional, so both (1,2) and (2,1) listed
    m = length(edges)
  
    #theta_knots = [zip(xy(ps.theta[i])...) for i=1:n]
    theta_knots = tuples.(ps.theta)
    theta = [ps.theta[i](t) for i=1:n]
    # a = @allocated println("a = ", a)

    linkstates = [LinkState(ps, e, t) for e=1:m]

    return Sample(n, edges, theta_knots, theta, linkstates, ps.ticks)
end


