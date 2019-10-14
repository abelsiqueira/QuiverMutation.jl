using LinearAlgebra, SparseArrays

export KR, @KR_str, glue, slice, weight, solve_product, distance

struct KR
  elements :: Array{Tuple{Int,Int},1}
end

"""
    KR(i,j)

Creates element "i_j".
"""
function KR(i::Int, j::Int)
  return KR(Tuple{Int,Int}[(i,j)])
end

"""
    KR(string)

Creates element from string in the form "i_j i_j i_j".
"""
function KR(str :: String)
  L = Tuple{Int,Int}[]
  for e in split(str)
    i, j = Meta.parse.(split(e, "_"))
    push!(L, (i,j))
  end
  return KR(sort(L))
end

"""
"""
macro KR_str(str :: String)
  return KR(str)
end

import Base.print
function print(io :: IO, kr :: KR)
  for (i,j) in kr.elements
    print(io, i)
    print(io, join('₀' .+ Meta.parse.(split(string(j),""))))
  end
end

import Base.show
show(io :: IO, kr :: KR) = print(io, kr)

"""
    glue(kr1, kr2)
    kr1 & kr2

Agluttinates `kr1` and `kr2`.
"""
function glue(kr1 :: KR, kr2 :: KR)
  kr = KR([kr1.elements; kr2.elements])
  sort!(kr.elements)
  return kr
end

import Base.&
(&)(kr1 :: KR, kr2 :: KR) = glue(kr1, kr2)

import Base.^
function (^)(kr1 :: KR, n :: Int)
  n ≥ 1 || error("Power needs to be ≥ 1")
  if n == 1
    return kr1
  elseif n == 2
    return kr1 & kr1
  else
    return kr1 & kr1^(n-1)
  end
end

"""
    slice(kr1, kr2)
    kr1 - kr2

Compute the complement of kr2 in kr1.
"""
function slice(kr1 :: KR, kr2 :: KR)
  L = copy(kr1.elements)
  for i in kr2.elements
    j = findfirst([y == i for y in L])
    j == nothing && continue
    deleteat!(L, j)
  end
  return KR(L)
end

import Base.-
(-)(kr1 :: KR, kr2 :: KR) = slice(kr1, kr2)

import Base.==
==(kr1 :: KR, kr2 :: KR) = length(slice(kr1, kr2).elements) == 0 && length(slice(kr2, kr1).elements) == 0

function distance(kr1 :: KR, kr2 :: KR)
  d = (kr1 - kr2) & (kr2 - kr1)
  return norm(weight(d, 2))
end

"""
    weight(kr, cols)

Compute the weight vector (#1,#2,#3,…,#n)
"""
function weight(kr :: KR, cols :: Int)
  return [count(x[1] .== i for x in kr.elements) for i = 1:cols]
end

"""
    solve_product(krx, krA, krB, cols)

Solves `x × krx = krA + krB`.
"""
function solve_product(krx, krA, krB, cols)
  w = weight(krA, cols) - weight(krB, cols)
  M = spdiagm(0 => 2 * ones(cols), -1 => -ones(cols-1), 1 => -ones(cols-1))
  r = round.(Int, M \ w)

  s = if all(r .≥ 0)
    krA
  elseif all(r .≤ 0)
    krB
  else
    error("Neither r ≥ 0 nor r ≤ 0")
  end
  return s - krx
end

function isless(kr1 :: KR, kr2 :: KR)
  if length(kr1.elements) < length(kr2.elements)
    return true
  elseif length(kr1.elements) > length(kr2.elements)
    return false
  end
  for (a, b) in zip(kr1.elements, kr2.elements)
    if a < b
      return true
    elseif a > b
      return false
    end
  end
  return false
end
