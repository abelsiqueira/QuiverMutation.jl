export indexin, search, investigate_all_x

function indexin(lt :: Lattice, kr :: KR)
  I = findall([xi == kr for xi in lt.x])
  if length(I) > 0
    i, j = Tuple(I[1])
    return i, j
  else
    return 0, 0
  end
end

"""
    P, i, j = search(lattice, kr)

Search for a path of applications where `kr` is in `lattice`.
"""
function search(lt :: Lattice, kr :: KR; max_depth :: Int = 6)
  score(lt) = minimum(distance(xi, kr) for xi in lt.x)
  i, j = indexin(lt, kr)
  (i,j) != (0,0) && return (Tuple{Int,Int}[], i, j)
  # poll = [(lt,P), …]
  candidates = [(lt, Tuple{Int,Int}[], score(lt))]
  previous = [lt]

  count = 0
  depth = 0
  done = false
  while !done
    sort!(candidates, by=x->-(exp(x[3]) - 1)*length(x[2])^1.1)
    lt, P, sc = pop!(candidates)
    for i = 1:lt.rows-1
      for j = 1:lt.cols
        P⁺ = [P; (i,j)]
        #println("Testing path $P⁺ with score $sc")
        lt⁺ = activate(lt, i, j)
        if any(lti == lt⁺ for lti in previous) # Ignore previous lattices
          continue
        end

        ki, kj = indexin(lt⁺, kr)
        (ki,ki) != (0,0) && return (P⁺, ki, kj)
        count += 1
        if count % 10 == 0
          print(".")
        end
        if count % 1000 == 0
          println("")
        end

        if length(P⁺) < max_depth
          push!(candidates, (lt⁺, P⁺, score(lt⁺)))
          push!(previous, lt⁺)
        end
      end
    end
    if depth > max_depth
      done = true
    end
  end
end

"""
    all_x = investigate_all_x(lattice, max_depth)

List all x resulting from paths up to `max_depth` depth.
"""
function investigate_all_x(lt :: Lattice, max_depth :: Int)
  # poll = [(lt,P), …]
  candidates = [(lt, Tuple{Int,Int}[])]
  previous = [lt]

  all_x = lt.x[:]

  depth = 0
  done = false
  while !done
    sort!(candidates, by=x->-length(x[2]))
    lt, P = pop!(candidates)
    if depth < length(P)
      depth = length(P)
    end
    for i = 1:lt.rows-1
      for j = 1:lt.cols
        P⁺ = [P; (i,j)]
        #println("Testing path $P⁺")
        lt⁺ = activate(lt, i, j)
        if any(lti == lt⁺ for lti in previous) # Ignore previous lattices
          continue
        end

        push!(candidates, (lt⁺, P⁺))
        push!(previous, lt⁺)
        for x in lt.x
          if !(x in all_x)
            println(x)
            push!(all_x, x)
          end
        end
      end
    end
    if depth > max_depth
      done = true
    end
  end

  sort!(all_x, by=x->norm(weight(x, lt.cols)))
  return all_x
end
