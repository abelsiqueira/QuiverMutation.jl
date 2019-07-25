using Plots, Printf

export plot_lattice, plot_path

function plot_lattice(lt :: Lattice;
                      distance = 15.0,
                      debug = true,
                      mark = (0, 0),
                      title = "",
                      filename = "tmp"
                     )

  ms = 0 # 6distance
  σ = 0.2distance
  p = plot(size=(300 * lt.cols, 300 * lt.rows), leg=false, axis=false, grid=false)

  ijtoxy(i, j) = (j - 1) * distance, (i - 0.5 - (j % 2) / 2) * distance

  # Plot edges
  for j = 1:lt.cols
    for i = 1:lt.rows
      L = adjacent(lt, i, j)
      for (di, dj, w) in L
        w < 0 && continue
        # TODO: Better w printing
        xsrc, ysrc = ijtoxy(i, j)
        xdest, ydest = ijtoxy(di, dj)
        if j == dj && abs(i - di) > 1 # Same column but no adjacent
          γ = j % 2 == 1 ? σ : -σ
          plot!(p, [xsrc-γ, xsrc-1.5γ, xsrc-1.5γ, xsrc-γ], [ysrc, ysrc, ydest, ydest], c=:black, l=:arrow)
        else
          d = [xdest; ydest] - [xsrc; ysrc]
          src = [xsrc; ysrc] + max(ms, σ) * d / norm(d)
          dest = [xdest; ydest] - max(ms, σ) * d / norm(d)
          plot!(p, [src[1], dest[1]], [src[2], dest[2]], c=:black, l=:arrow, lw=w)
        end
      end
    end
  end

  # Plots nodes
  for j = 1:lt.cols
    for i = 1:lt.rows
      x, y = ijtoxy(i, j)
      if mark == (i, j)
        scatter!(p, [x], [y], m=(3distance,:white,stroke(1,:red)))
      end
      scatter!(p, [x], [y], m=(ms,:white,stroke(1,:black)), ann=(x, y, lt.x[i,j]))
    end
  end
  xlims!(p, -distance / 2, distance * (lt.cols - 0.5))
  ylims!(p, -distance / 2, distance * lt.rows)
  if title != ""
    title!(p, title)
  end
  if filename != ""
    Plots.svg(filename)
  end
  return p
end

"""
    plot_path(lattice, path; prefix="quiver")

Plots all lattices that appears alone the `path` starting from `lattice`. Save files with name `prefix-%03d`, where `%03d` means 000, 001, 002, etc.
"""
function plot_path(lt, P; prefix="quiver")
  plot_lattice(lt, title="Initial", filename="$prefix-000")

  for (i,p) in enumerate(P)
    lt = activate(lt, p[1], p[2])
    fname = @sprintf("%s-%03d", prefix, i)
    plot_lattice(lt, title="Activated column $(p[2]), row $(p[1])", filename=fname, mark=(p[1],p[2]))
  end
end
