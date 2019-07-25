export activate

"""
    activate(lattice, i, j)

Activate node `(i,j)` of `lattice`. Notice that `lattice` is finite, and to
simulate this, no activations is allowed at the last line of it.
"""
function activate(lt :: Lattice, i :: Int, j :: Int)
  if i == lt.rows
    error("No activation is allowed in the last line of the lattice")
  elseif i < 1 || i > lt.rows || j < 1 || j > lt.cols
    error("Bad index. Check lattice dimensions")
  end

  k = i + (j - 1) * lt.rows

  local xprime
  # Variable change
  I = findall(lt.B[k,:] .> 0)
  J = findall(lt.B[k,:] .< 0)
  if length(I) == 0 && length(J) == 0
    error("No adjacent edges. Is this supposed to happen?")
  elseif length(I) == 0
    error("Fix")
  elseif length(J) == 0
    error("Fix")
  else
    kr1 = KR(0,0)
    for p = I
      kj, ki = divrem(p - 1, lt.rows)
      kr1 = kr1 & lt.x[ki+1,kj+1]^lt.B[k,p]
    end
    kr1 = kr1 - KR(0,0)
    kr2 = KR(0,0)
    for p = J
      kj, ki = divrem(p - 1, lt.rows)
      kr2 = kr2 & lt.x[ki+1,kj+1]^(-lt.B[k,p])
    end
    kr2 = kr2 - KR(0,0)
    x = lt.x[i,j]
    xprime = solve_product(x, kr1, kr2, lt.cols)
  end

  # Graph changes
  k = i + (j - 1) * lt.rows
  Bc = copy(lt.B)
  # Directly influenced
  Bc[k,:] = -lt.B[k,:]
  Bc[:,k] = -lt.B[:,k]
  I = findall(lt.B[k,:] .!= 0)
  for ii = I, jj = I
    Bc[ii,jj] = lt.B[ii,jj] + div(abs(lt.B[ii,k]) * lt.B[k,jj] + lt.B[ii,k] * abs(lt.B[k,jj]), 2)
  end

  x = copy(lt.x)
  x[i,j] = xprime

  return Lattice(lt.cols, lt.rows, Bc, x)
end
