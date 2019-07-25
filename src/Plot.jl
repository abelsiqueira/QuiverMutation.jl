using Plots

export plot_lattice

function plot_lattice(lt :: Lattice;
                      distance = 15.0,
                      debug = true,
                      title = "",
                      filename = "tmp"
                     )

  ms = 0 # 6distance
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
        d = [xdest; ydest] - [xsrc; ysrc]
        src = [xsrc; ysrc] + max(ms, 0.2distance) * d / norm(d)
        dest = [xdest; ydest] - max(ms, 0.2distance) * d / norm(d)
        plot!(p, [src[1], dest[1]], [src[2], dest[2]], c=:black, l=:arrow, lw=w)
      end
    end
  end

  # Plots nodes
  for j = 1:lt.cols
    for i = 1:lt.rows
      x, y = ijtoxy(i, j)
      scatter!(p, [x], [y], m=(ms,:white,stroke(1,:black)), c=:black, ann=(x, y, lt.x[i,j]))
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
