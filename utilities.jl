using Jupyter2Pluto

function pluto2qmd(nm::AbstractString)
    ipy = pluto2jupyter(nm * ".jl")
    ipy = mv(ipy, nm * ".ipynb")
    run(`quarto convert $ipy`)
    rm(ipy)
end
