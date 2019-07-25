export Lattice, print_lattice, getindex, adjacent

struct Lattice
  cols :: Int
  rows :: Int
  B :: Matrix{Int}
  x :: Matrix{KR}
end

function tolin(i, j, rows)
  return i + (j - 1) * rows
end

function toij(k, rows)
  j, i = divrem(k - 1, rows)
  return i+1, j+1
end

function Lattice(cols, rows)
  N = cols * rows

  B = zeros(Int, N, N)
  for j = 1:cols
    for i = 1:rows-1
      k = tolin(i, j, rows)
      # Above
      B[k,k+1] = 1
      j == cols && continue
      if j % 2 == 1
        B[k,k+rows] = -1
        B[k+1,k+rows] = 1
      else
        B[k,k+rows] = 1
        B[k,k+rows+1] = -1
      end
    end
    if j < cols
      k = tolin(rows, j, rows)
      B[k,k+rows] = j % 2 == 1 ? -1 : 1
    end
  end
  B = triu(B) - triu(B)'

  x = Matrix{KR}(undef, rows, cols)
  for j = 1:cols
    p = (j-1)%2
    kr = KR(j, p)
    for i = 1:rows
      x[i,j] = kr
      p += 2
      kr = kr & KR(j, p)
    end
  end

  return Lattice(cols, rows, B, x)
end

function print_lattice(lt :: Lattice)
  for j = 1:lt.cols
    for i = 1:lt.rows
      xij = lt.x[i,j]
      k = tolin(i, j, lt.rows)
      for p = findall(lt.B[k,:] .!= 0)
        σ = lt.B[k,p] > 0 ? "→" : "←"
        ki, kj = toij(p, lt.rows)
        print("$xij $σ $(lt.x[ki,kj])  ")
      end
      println("")
    end
  end
end

function getindex(lt :: Lattice, i, j)
  return lt.x[i,j]
end

"""
    [(i,j,w),…] = adjacent(lattice, i, j)

Returns all adjacent nodes to (i,j). A list of triplets is returns, with the
row, col and weight of the edge, i.e., >0 means leaving, <0 means arriving, and
|w| is the number of edges.
"""
function adjacent(lt, i, j)
  k = tolin(i, j, lt.rows)
  I = findall(lt.B[k,:] .!= 0)
  L = Tuple{Int,Int,Int}[]
  for p in I
    i, j = toij(p, lt.rows)
    push!(L, (i, j, lt.B[k,p]))
  end
  return L
end
